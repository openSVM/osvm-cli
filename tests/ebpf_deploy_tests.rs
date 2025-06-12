use osvm::utils::ebpf_deploy::{
    deploy_to_all_networks, load_program, load_program_id, load_program_keypair, 
    validate_program_id_for_new_deployment, DeployConfig,
};
use solana_sdk::{commitment_config::CommitmentConfig, signature::Signer};
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
        network_selection: "devnet".to_string(),
    };

    // Verify config fields
    assert_eq!(config.binary_path, "path/to/binary.so");
    assert_eq!(config.program_id_path, "path/to/program_id.json");
    assert_eq!(config.owner_path, "path/to/owner.json");
    assert_eq!(config.fee_payer_path, "path/to/fee_payer.json");
    assert!(config.publish_idl);
    assert_eq!(config.network_selection, "devnet");

    // Clone the config and verify the clone
    let config_clone = config.clone();
    assert_eq!(config_clone.binary_path, config.binary_path);
}

#[test]
fn test_load_program_keypair() {
    let dir = tempdir().unwrap();
    
    // Test with a valid keypair file
    let keypair_path = dir.path().join("program_keypair.json");
    let keypair = solana_sdk::signature::Keypair::new();
    let keypair_bytes = keypair.to_bytes();
    let keypair_json = serde_json::to_string(&keypair_bytes.to_vec()).unwrap();
    
    let mut file = File::create(&keypair_path).unwrap();
    file.write_all(keypair_json.as_bytes()).unwrap();
    
    // Test loading the keypair
    let loaded_keypair = load_program_keypair(keypair_path.to_str().unwrap()).unwrap();
    assert_eq!(loaded_keypair.pubkey(), keypair.pubkey());
}

#[test]
fn test_load_program_keypair_fails_on_pubkey_only() {
    let dir = tempdir().unwrap();
    
    // Test with a pubkey-only file
    let pubkey_path = dir.path().join("pubkey_only.json");
    let pubkey_content = r#"{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}"#;
    
    let mut file = File::create(&pubkey_path).unwrap();
    file.write_all(pubkey_content.as_bytes()).unwrap();
    
    // This should fail because it's not a keypair
    let result = load_program_keypair(pubkey_path.to_str().unwrap());
    assert!(result.is_err());
}

#[test]
fn test_validate_program_id_for_new_deployment() {
    let dir = tempdir().unwrap();
    
    // Test with valid keypair file - should succeed
    let keypair_path = dir.path().join("valid_keypair.json");
    let keypair = solana_sdk::signature::Keypair::new();
    let keypair_bytes = keypair.to_bytes();
    let keypair_json = serde_json::to_string(&keypair_bytes.to_vec()).unwrap();
    
    let mut file = File::create(&keypair_path).unwrap();
    file.write_all(keypair_json.as_bytes()).unwrap();
    
    let result = validate_program_id_for_new_deployment(keypair_path.to_str().unwrap());
    assert!(result.is_ok());
    
    // Test with pubkey-only file - should fail
    let pubkey_path = dir.path().join("pubkey_only.json");
    let pubkey_content = r#"{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}"#;
    
    let mut file = File::create(&pubkey_path).unwrap();
    file.write_all(pubkey_content.as_bytes()).unwrap();
    
    let result = validate_program_id_for_new_deployment(pubkey_path.to_str().unwrap());
    assert!(result.is_err());
}

#[test] 
fn test_deploy_config_with_boolean_idl_flag() {
    // Test that DeployConfig properly handles boolean IDL flag
    let config = DeployConfig {
        binary_path: "path/to/binary.so".to_string(),
        program_id_path: "path/to/program_id.json".to_string(),
        owner_path: "path/to/owner.json".to_string(),
        fee_payer_path: "path/to/fee_payer.json".to_string(),
        publish_idl: true,  // Boolean flag instead of string
        network_selection: "all".to_string(),
    };
    
    assert!(config.publish_idl);
    
    let config_false = DeployConfig {
        binary_path: "path/to/binary.so".to_string(),
        program_id_path: "path/to/program_id.json".to_string(),
        owner_path: "path/to/owner.json".to_string(),
        fee_payer_path: "path/to/fee_payer.json".to_string(),
        publish_idl: false,
        network_selection: "mainnet".to_string(),
    };
    
    assert!(!config_false.publish_idl);
}

#[tokio::test]
async fn test_network_filter_logic() {
    // This test validates the network selection logic without actually making network calls
    let dir = tempdir().unwrap();

    // Create test files
    let program_file = dir.path().join("program.so");
    let mut file = File::create(&program_file).unwrap();
    file.write_all(b"dummy program").unwrap();

    let program_id_file = dir.path().join("program_id.json");
    let mut file = File::create(&program_id_file).unwrap();
    file.write_all(br#"{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}"#)
        .unwrap();

    // Create valid Solana keypairs for the test
    use solana_sdk::signature::Keypair;
    let test_keypair = Keypair::new();

    let owner_file = dir.path().join("owner.json");
    let mut file = File::create(&owner_file).unwrap();
    // Write the keypair in the format expected by Solana SDK
    file.write_all(
        serde_json::to_string(&test_keypair.to_bytes().to_vec())
            .unwrap()
            .as_bytes(),
    )
    .unwrap();

    let fee_payer_file = dir.path().join("fee_payer.json");
    let mut file = File::create(&fee_payer_file).unwrap();
    file.write_all(
        serde_json::to_string(&test_keypair.to_bytes().to_vec())
            .unwrap()
            .as_bytes(),
    )
    .unwrap();

    // Test "all" network selection
    let config_all = DeployConfig {
        binary_path: program_file.to_string_lossy().to_string(),
        program_id_path: program_id_file.to_string_lossy().to_string(),
        owner_path: owner_file.to_string_lossy().to_string(),
        fee_payer_path: fee_payer_file.to_string_lossy().to_string(),
        publish_idl: false,
        network_selection: "all".to_string(),
    };

    // This attempts network calls which will fail in test environment
    // Since the configuration validation passes but network connection fails,
    // it should return 3 error results (one for each network)
    let results = deploy_to_all_networks(config_all, CommitmentConfig::confirmed()).await;

    // Should return 3 results (one for each network: mainnet, testnet, devnet)
    assert_eq!(results.len(), 3);
    // All results should be errors due to network connectivity issues in test environment
    for result in &results {
        assert!(result.is_err());
    }

    // Test single network selection
    let config_single = DeployConfig {
        binary_path: program_file.to_string_lossy().to_string(),
        program_id_path: program_id_file.to_string_lossy().to_string(),
        owner_path: owner_file.to_string_lossy().to_string(),
        fee_payer_path: fee_payer_file.to_string_lossy().to_string(),
        publish_idl: false,
        network_selection: "devnet".to_string(),
    };

    let results = deploy_to_all_networks(config_single, CommitmentConfig::confirmed()).await;

    // Should return 1 result (devnet only)
    assert_eq!(results.len(), 1);
    // Result should be an error due to network connectivity issues in test environment
    assert!(results[0].is_err());

    // Test invalid network selection
    let config_invalid = DeployConfig {
        binary_path: program_file.to_string_lossy().to_string(),
        program_id_path: program_id_file.to_string_lossy().to_string(),
        owner_path: owner_file.to_string_lossy().to_string(),
        fee_payer_path: fee_payer_file.to_string_lossy().to_string(),
        publish_idl: false,
        network_selection: "invalid".to_string(),
    };

    let results = deploy_to_all_networks(config_invalid, CommitmentConfig::confirmed()).await;

    // Should return 1 error result
    assert_eq!(results.len(), 1);
    assert!(results[0].is_err());
}
