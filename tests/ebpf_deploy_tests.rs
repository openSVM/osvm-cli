use osvm::utils::ebpf_deploy::{DeployConfig, load_program_id, load_program, deploy_to_all_networks};
use solana_sdk::commitment_config::CommitmentConfig;
use std::fs::File;
use std::io::Write;
use tempfile::tempdir;

#[test]
fn test_load_program_id() {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("program_id.json");
    
    // Create a program ID file with valid pubkey
    let program_id_content = r#"{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}"#;
    let mut file = File::create(&file_path).unwrap();
    file.write_all(program_id_content.as_bytes()).unwrap();
    
    // Test loading the program ID
    let pubkey = load_program_id(file_path.to_str().unwrap()).unwrap();
    assert_eq!(
        pubkey.to_string(),
        "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"
    );
}

#[test]
fn test_load_program() {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("program.so");
    
    // Create a dummy program file
    let program_data = b"dummy eBPF program binary data";
    let mut file = File::create(&file_path).unwrap();
    file.write_all(program_data).unwrap();
    
    // Test loading the program
    let loaded_data = load_program(file_path.to_str().unwrap()).unwrap();
    assert_eq!(loaded_data, program_data);
}

#[test]
fn test_create_deploy_config() {
    // Create a deployment configuration
    let config = DeployConfig {
        binary_path: "path/to/binary.so".to_string(),
        program_id_path: "path/to/program_id.json".to_string(),
        owner_path: "path/to/owner.json".to_string(),
        fee_payer_path: "path/to/fee_payer.json".to_string(),
        publish_idl: true,
        network_filter: "devnet".to_string(),
    };
    
    // Verify config fields
    assert_eq!(config.binary_path, "path/to/binary.so");
    assert_eq!(config.program_id_path, "path/to/program_id.json");
    assert_eq!(config.owner_path, "path/to/owner.json");
    assert_eq!(config.fee_payer_path, "path/to/fee_payer.json");
    assert!(config.publish_idl);
    assert_eq!(config.network_filter, "devnet");
    
    // Clone the config and verify the clone
    let config_clone = config.clone();
    assert_eq!(config_clone.binary_path, config.binary_path);
}

#[tokio::test]
async fn test_network_filter_logic() {
    // This test only validates the network filtering logic without actually making network calls
    let dir = tempdir().unwrap();
    
    // Create test files
    let program_file = dir.path().join("program.so");
    let mut file = File::create(&program_file).unwrap();
    file.write_all(b"dummy program").unwrap();
    
    let program_id_file = dir.path().join("program_id.json");
    let mut file = File::create(&program_id_file).unwrap();
    file.write_all(br#"{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}"#).unwrap();
    
    let owner_file = dir.path().join("owner.json");
    let mut file = File::create(&owner_file).unwrap();
    // Valid Ed25519 keypair (64 bytes)
    let keypair = [
        74, 97, 123, 217, 156, 17, 153, 238, 153, 193, 31, 227, 15, 68, 138, 151,
        87, 14, 66, 179, 187, 149, 171, 224, 24, 176, 206, 49, 225, 173, 85, 69,
        35, 85, 76, 4, 41, 56, 193, 80, 141, 162, 202, 35, 90, 29, 138, 14,
        176, 85, 191, 245, 106, 196, 149, 53, 180, 252, 241, 119, 54, 11, 141, 223
    ];
    file.write_all(format!("{:?}", keypair.to_vec()).as_bytes()).unwrap();
    
    let fee_payer_file = dir.path().join("fee_payer.json");
    let mut file = File::create(&fee_payer_file).unwrap();
    file.write_all(format!("{:?}", keypair.to_vec()).as_bytes()).unwrap();
    
    // Test "all" network filter
    let config_all = DeployConfig {
        binary_path: program_file.to_string_lossy().to_string(),
        program_id_path: program_id_file.to_string_lossy().to_string(),
        owner_path: owner_file.to_string_lossy().to_string(),
        fee_payer_path: fee_payer_file.to_string_lossy().to_string(),
        publish_idl: false,
        network_filter: "all".to_string(),
    };
    
    // This would attempt network calls in a real scenario, but we're just testing the logic
    // The function should handle "all" and try to deploy to 3 networks (mainnet, testnet, devnet)
    let results = deploy_to_all_networks(config_all, CommitmentConfig::confirmed()).await;
    
    // Should return 3 results (one for each network)
    assert_eq!(results.len(), 3);
    
    // Test single network filter
    let config_single = DeployConfig {
        binary_path: program_file.to_string_lossy().to_string(),
        program_id_path: program_id_file.to_string_lossy().to_string(),
        owner_path: owner_file.to_string_lossy().to_string(),
        fee_payer_path: fee_payer_file.to_string_lossy().to_string(),
        publish_idl: false,
        network_filter: "devnet".to_string(),
    };
    
    let results = deploy_to_all_networks(config_single, CommitmentConfig::confirmed()).await;
    
    // Should return 1 result (devnet only)
    assert_eq!(results.len(), 1);
    
    // Test invalid network filter
    let config_invalid = DeployConfig {
        binary_path: program_file.to_string_lossy().to_string(),
        program_id_path: program_id_file.to_string_lossy().to_string(),
        owner_path: owner_file.to_string_lossy().to_string(),
        fee_payer_path: fee_payer_file.to_string_lossy().to_string(),
        publish_idl: false,
        network_filter: "invalid".to_string(),
    };
    
    let results = deploy_to_all_networks(config_invalid, CommitmentConfig::confirmed()).await;
    
    // Should return 1 error result
    assert_eq!(results.len(), 1);
    assert!(results[0].is_err());
}