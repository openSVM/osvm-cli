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
        idl_file_path: None, // No custom IDL file
        network_selection: "devnet".to_string(),
        json_output: false,
        retry_attempts: 3,
        confirm_large_binaries: false,
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
        publish_idl: true, // Boolean flag instead of string
        idl_file_path: None,
        network_selection: "all".to_string(),
        json_output: false,
        retry_attempts: 3,
        confirm_large_binaries: false,
    };

    assert!(config.publish_idl);

    let config_false = DeployConfig {
        binary_path: "path/to/binary.so".to_string(),
        program_id_path: "path/to/program_id.json".to_string(),
        owner_path: "path/to/owner.json".to_string(),
        fee_payer_path: "path/to/fee_payer.json".to_string(),
        publish_idl: false,
        idl_file_path: Some("custom_idl.json".to_string()), // Test with custom IDL
        network_selection: "mainnet".to_string(),
        json_output: false,
        retry_attempts: 3,
        confirm_large_binaries: false,
    };

    assert!(!config_false.publish_idl);
}

#[test]
fn test_load_custom_idl() {
    let dir = tempdir().unwrap();

    // Create a mock Anchor IDL file
    let idl_path = dir.path().join("test_program.json");
    let anchor_idl = serde_json::json!({
        "version": "0.1.0",
        "name": "test_program",
        "instructions": [
            {
                "name": "initialize",
                "accounts": [],
                "args": []
            }
        ],
        "accounts": [],
        "types": [],
        "events": [],
        "errors": []
    });

    let mut file = File::create(&idl_path).unwrap();
    file.write_all(anchor_idl.to_string().as_bytes()).unwrap();

    // Test loading the IDL
    use osvm::utils::ebpf_deploy::load_or_create_idl;
    use solana_sdk::pubkey::Pubkey;

    let program_id = Pubkey::new_unique();
    let loaded_idl = load_or_create_idl(Some(idl_path.to_str().unwrap()), program_id).unwrap();

    // Verify it loaded the custom IDL
    assert_eq!(loaded_idl["name"], "test_program");
    assert_eq!(loaded_idl["version"], "0.1.0");
    assert!(loaded_idl["instructions"].is_array());
}

#[test]
fn test_deploy_config_with_new_fields() {
    // Test that DeployConfig properly handles new fields
    let config = DeployConfig {
        binary_path: "path/to/binary.so".to_string(),
        program_id_path: "path/to/program_id.json".to_string(),
        owner_path: "path/to/owner.json".to_string(),
        fee_payer_path: "path/to/fee_payer.json".to_string(),
        publish_idl: true,
        idl_file_path: None,
        network_selection: "all".to_string(),
        json_output: true,
        retry_attempts: 5,
        confirm_large_binaries: true,
    };

    assert!(config.json_output);
    assert_eq!(config.retry_attempts, 5);
    assert!(config.confirm_large_binaries);
}

#[test]
fn test_large_binary_validation() {
    let dir = tempdir().unwrap();

    // Create a binary that's too large (>5MB)
    let file_path = dir.path().join("large_program.so");
    let large_data = vec![0u8; 6_000_000]; // 6MB
    let mut file = File::create(&file_path).unwrap();
    file.write_all(&large_data).unwrap();

    // This should fail with an error about the file being too large
    let result = load_program(file_path.to_str().unwrap());
    assert!(result.is_err());

    if let Err(e) = result {
        assert!(e.to_string().contains("too large"));
    }
}

#[test]
fn test_large_binary_warning() {
    let dir = tempdir().unwrap();

    // Create a binary that triggers warning (1.5MB)
    let file_path = dir.path().join("warning_program.so");
    let warning_data = vec![0u8; 1_500_000]; // 1.5MB
    let mut file = File::create(&file_path).unwrap();
    file.write_all(&warning_data).unwrap();

    // This should succeed but emit a warning
    let result = load_program(file_path.to_str().unwrap());
    assert!(result.is_ok());
    assert_eq!(result.unwrap().len(), 1_500_000);
}

#[tokio::test]
async fn test_idl_publishing_edge_cases() {
    use solana_sdk::pubkey::Pubkey;

    // This test validates IDL publishing edge cases without actual network calls
    let dir = tempdir().unwrap();

    // Test with invalid IDL file (not JSON)
    let invalid_idl_path = dir.path().join("invalid.json");
    let mut file = File::create(&invalid_idl_path).unwrap();
    file.write_all(b"not valid json").unwrap();

    // Test loading invalid IDL
    use osvm::utils::ebpf_deploy::load_or_create_idl;
    let program_id = Pubkey::new_unique();
    let result = load_or_create_idl(Some(invalid_idl_path.to_str().unwrap()), program_id);
    assert!(result.is_err());

    // Test with missing IDL file
    let missing_idl_path = dir.path().join("missing.json");
    let result = load_or_create_idl(Some(missing_idl_path.to_str().unwrap()), program_id);
    assert!(result.is_err());

    // Test with valid but empty JSON object
    let empty_idl_path = dir.path().join("empty.json");
    let mut file = File::create(&empty_idl_path).unwrap();
    file.write_all(b"{}").unwrap();

    let result = load_or_create_idl(Some(empty_idl_path.to_str().unwrap()), program_id);
    assert!(result.is_ok());
}

#[test]
fn test_keypair_cloning_error_handling() {
    // Test that keypair cloning works through the public interface
    use solana_sdk::signature::Keypair;

    let original_keypair = Keypair::new();

    // Test that keypair can be cloned through bytes round-trip
    let cloned_bytes = original_keypair.to_bytes();
    let cloned = Keypair::from_bytes(&cloned_bytes);

    assert!(cloned.is_ok());
    assert_eq!(cloned.unwrap().pubkey(), original_keypair.pubkey());
}

#[test]
fn test_deployment_result_serialization() {
    // Test that DeploymentResult can be serialized/deserialized
    use osvm::utils::ebpf_deploy::DeploymentResult;
    use solana_sdk::pubkey::Pubkey;

    let result = DeploymentResult {
        network: "devnet".to_string(),
        program_id: Pubkey::new_unique(),
        success: true,
        transaction_signature: Some("test_signature".to_string()),
        error_message: None,
        retries_attempted: 2,
        duration_ms: 5000,
    };

    // Test serialization
    let json = serde_json::to_string(&result).unwrap();
    assert!(json.contains("devnet"));
    assert!(json.contains("test_signature"));
    assert!(json.contains("5000"));

    // Test deserialization
    let deserialized: DeploymentResult = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized.network, "devnet");
    assert_eq!(deserialized.retries_attempted, 2);
    assert_eq!(deserialized.duration_ms, 5000);
}

#[tokio::test]
async fn test_rpc_client_cache() {
    use osvm::utils::ebpf_deploy::RpcClientCache;

    let mut cache = RpcClientCache::new();

    // Test that same URL returns same client instance
    let client1 = cache.get_client("https://api.devnet.solana.com");
    let client2 = cache.get_client("https://api.devnet.solana.com");

    // Both should reference the same underlying client (Arc)
    assert!(std::ptr::eq(client1.as_ref(), client2.as_ref()));

    // Different URL should return different client
    let client3 = cache.get_client("https://api.testnet.solana.com");
    assert!(!std::ptr::eq(client1.as_ref(), client3.as_ref()));
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
        idl_file_path: None,
        network_selection: "all".to_string(),
        json_output: false,
        retry_attempts: 1,
        confirm_large_binaries: false,
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
        idl_file_path: None,
        network_selection: "devnet".to_string(),
        json_output: false,
        retry_attempts: 1,
        confirm_large_binaries: false,
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
        idl_file_path: None,
        network_selection: "invalid".to_string(),
        json_output: false,
        retry_attempts: 1,
        confirm_large_binaries: false,
    };

    let results = deploy_to_all_networks(config_invalid, CommitmentConfig::confirmed()).await;

    // Should return 1 error result
    assert_eq!(results.len(), 1);
    assert!(results[0].is_err());
}
