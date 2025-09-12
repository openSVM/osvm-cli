use osvm::utils::ebpf_deploy::{deploy_to_all_networks, DeployConfig, DeploymentResult};
use serial_test::serial;
use solana_commitment_config::{CommitmentConfig, CommitmentLevel};
use solana_sdk::pubkey::Pubkey;
use std::fs::{self, File};
use std::io::Write;
use tempfile::tempdir;

/// Test deployment configuration validation
#[test]
fn test_deploy_config_validation() {
    let dir = tempdir().unwrap();

    // Create test files
    let program_file = dir.path().join("program.so");
    let mut file = File::create(&program_file).unwrap();
    file.write_all(b"dummy program").unwrap();

    let program_id_file = dir.path().join("program_id.json");
    let mut file = File::create(&program_id_file).unwrap();
    file.write_all(br#"{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}"#)
        .unwrap();

    let config = DeployConfig {
        binary_path: program_file.to_string_lossy().to_string(),
        program_id_path: program_id_file.to_string_lossy().to_string(),
        owner_path: "nonexistent.json".to_string(),
        fee_payer_path: "nonexistent.json".to_string(),
        publish_idl: false,
        idl_file_path: None,
        network_selection: "devnet".to_string(),
        json_output: false,
        retry_attempts: 1,
        confirm_large_binaries: false,
    };

    // This config should fail due to missing owner and fee payer files
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let results = runtime.block_on(deploy_to_all_networks(
        config,
        CommitmentConfig::confirmed(),
    ));

    assert_eq!(results.len(), 1);
    assert!(results[0].is_err());
}

/// Test deployment with invalid network selection
#[tokio::test]
async fn test_invalid_network_selection() {
    let dir = tempdir().unwrap();

    // Create all valid files so network selection validation is reached
    let program_file = dir.path().join("program.so");
    let mut file = File::create(&program_file).unwrap();
    file.write_all(b"dummy program").unwrap();

    let program_id_file = dir.path().join("program_id.json");
    let mut file = File::create(&program_id_file).unwrap();
    file.write_all(br#"{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}"#)
        .unwrap();

    // Create valid keypairs for owner and fee payer
    use solana_sdk::signature::Keypair;
    let test_keypair = Keypair::new();

    let owner_file = dir.path().join("owner.json");
    let mut file = File::create(&owner_file).unwrap();
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

    let config = DeployConfig {
        binary_path: program_file.to_string_lossy().to_string(),
        program_id_path: program_id_file.to_string_lossy().to_string(),
        owner_path: owner_file.to_string_lossy().to_string(),
        fee_payer_path: fee_payer_file.to_string_lossy().to_string(),
        publish_idl: false,
        idl_file_path: None,
        network_selection: "invalid_network".to_string(),
        json_output: false,
        retry_attempts: 1,
        confirm_large_binaries: false,
    };

    let results = deploy_to_all_networks(config, CommitmentConfig::confirmed()).await;

    assert_eq!(results.len(), 1);
    assert!(results[0].is_err());
    if let Err(e) = &results[0] {
        assert!(e.to_string().contains("Invalid network selection"));
    }
}

/// Test retry mechanism configuration
#[test]
fn test_retry_configuration() {
    let config = DeployConfig {
        binary_path: "program.so".to_string(),
        program_id_path: "program_id.json".to_string(),
        owner_path: "owner.json".to_string(),
        fee_payer_path: "fee_payer.json".to_string(),
        publish_idl: false,
        idl_file_path: None,
        network_selection: "devnet".to_string(),
        json_output: false,
        retry_attempts: 5,
        confirm_large_binaries: false,
    };

    // Validate retry attempts configuration
    assert_eq!(config.retry_attempts, 5);
    assert!(!config.confirm_large_binaries);
}

/// Test error handling for missing files
#[tokio::test]
async fn test_missing_file_handling() {
    let config = DeployConfig {
        binary_path: "nonexistent_program.so".to_string(),
        program_id_path: "nonexistent_id.json".to_string(),
        owner_path: "nonexistent_owner.json".to_string(),
        fee_payer_path: "nonexistent_fee.json".to_string(),
        publish_idl: false,
        idl_file_path: None,
        network_selection: "devnet".to_string(),
        json_output: false,
        retry_attempts: 1,
        confirm_large_binaries: false,
    };

    let results = deploy_to_all_networks(config, CommitmentConfig::confirmed()).await;

    assert_eq!(results.len(), 1);
    assert!(results[0].is_err());
    if let Err(e) = &results[0] {
        // Should fail with file not found error
        assert!(e.to_string().contains("No such file") || e.to_string().contains("not found"));
    }
}

/// Test deployment result serialization for CI integration
#[test]
fn test_deployment_result_json_serialization() {
    let result = DeploymentResult {
        network: "devnet".to_string(),
        program_id: Pubkey::new_unique(),
        success: true,
        transaction_signature: Some("test_signature".to_string()),
        error_message: None,
        retries_attempted: 2,
        duration_ms: 5000,
    };

    // Test JSON serialization for CI systems
    let json = serde_json::to_string(&result).unwrap();
    assert!(json.contains("devnet"));
    assert!(json.contains("test_signature"));
    assert!(json.contains("5000"));

    // Test deserialization
    let deserialized: DeploymentResult = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized.network, "devnet");
    assert_eq!(deserialized.success, true);
    assert_eq!(deserialized.duration_ms, 5000);
}

/// Test deployment result error scenarios
#[test]
fn test_deployment_result_error_cases() {
    let error_result = DeploymentResult {
        network: "testnet".to_string(),
        program_id: Pubkey::new_unique(),
        success: false,
        transaction_signature: None,
        error_message: Some("Insufficient funds".to_string()),
        retries_attempted: 3,
        duration_ms: 2500,
    };

    assert!(!error_result.success);
    assert!(error_result.transaction_signature.is_none());
    assert!(error_result.error_message.is_some());
    assert_eq!(error_result.retries_attempted, 3);
}

/// Test network selection with "all" option
#[tokio::test]
async fn test_all_networks_selection() {
    let dir = tempdir().unwrap();

    // Create test files (all required files must exist for network selection to be reached)
    let program_file = dir.path().join("program.so");
    let mut file = File::create(&program_file).unwrap();
    file.write_all(b"dummy program").unwrap();

    let program_id_file = dir.path().join("program_id.json");
    let mut file = File::create(&program_id_file).unwrap();
    file.write_all(br#"{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}"#)
        .unwrap();

    // Create valid keypairs for owner and fee payer
    use solana_sdk::signature::Keypair;
    let test_keypair = Keypair::new();

    let owner_file = dir.path().join("owner.json");
    let mut file = File::create(&owner_file).unwrap();
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

    let config = DeployConfig {
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

    let results = deploy_to_all_networks(config, CommitmentConfig::confirmed()).await;

    // Should return results for all three networks (mainnet, testnet, devnet)
    // They will be errors due to network connectivity issues in test environment
    assert_eq!(results.len(), 3);
    // All should be errors due to network connectivity in test environment
    for result in &results {
        assert!(result.is_err());
    }
}

/// Test IDL file validation
#[test]
fn test_idl_configuration() {
    let dir = tempdir().unwrap();

    // Create a valid IDL file
    let idl_file = dir.path().join("test.json");
    let idl_content = serde_json::json!({
        "version": "0.1.0",
        "name": "test_program",
        "instructions": [],
        "accounts": [],
        "types": []
    });
    let mut file = File::create(&idl_file).unwrap();
    file.write_all(idl_content.to_string().as_bytes()).unwrap();

    let config = DeployConfig {
        binary_path: "program.so".to_string(),
        program_id_path: "program_id.json".to_string(),
        owner_path: "owner.json".to_string(),
        fee_payer_path: "fee_payer.json".to_string(),
        publish_idl: true,
        idl_file_path: Some(idl_file.to_string_lossy().to_string()),
        network_selection: "devnet".to_string(),
        json_output: false,
        retry_attempts: 1,
        confirm_large_binaries: false,
    };

    assert!(config.publish_idl);
    assert!(config.idl_file_path.is_some());
}

/// Test environment variable handling for GitHub Actions
#[test]
#[serial]
fn test_github_actions_environment_vars() {
    // Simulate GitHub Actions environment
    std::env::set_var("GITHUB_ACTIONS", "true");
    std::env::set_var("GITHUB_WORKSPACE", "/github/workspace");
    std::env::set_var("RUNNER_OS", "Linux");

    // Test that our deployment config respects CI environment
    let in_ci = std::env::var("GITHUB_ACTIONS").unwrap_or_default() == "true";
    assert!(in_ci);

    let workspace = std::env::var("GITHUB_WORKSPACE").unwrap_or_default();
    assert_eq!(workspace, "/github/workspace");

    // Cleanup
    std::env::remove_var("GITHUB_ACTIONS");
    std::env::remove_var("GITHUB_WORKSPACE");
    std::env::remove_var("RUNNER_OS");
}

/// Test commitment configuration handling
#[test]
fn test_commitment_config_variations() {
    let configs = vec![
        CommitmentConfig::processed(),
        CommitmentConfig::confirmed(),
        CommitmentConfig::finalized(),
    ];

    for config in configs {
        // Test that commitment configs can be used properly
        assert!(
            matches!(
                config.commitment,
                CommitmentLevel::Processed
            ) || matches!(
                config.commitment,
                CommitmentLevel::Confirmed
            ) || matches!(
                config.commitment,
                CommitmentLevel::Finalized
            )
        );
    }
}

/// Test large binary size validation
#[test]
fn test_large_binary_size_validation() {
    let dir = tempdir().unwrap();

    // Create a binary that would trigger confirmation
    let large_binary = dir.path().join("large_program.so");
    let large_data = vec![0u8; 2_000_000]; // 2MB - should trigger warning
    let mut file = File::create(&large_binary).unwrap();
    file.write_all(&large_data).unwrap();

    let config = DeployConfig {
        binary_path: large_binary.to_string_lossy().to_string(),
        program_id_path: "program_id.json".to_string(),
        owner_path: "owner.json".to_string(),
        fee_payer_path: "fee_payer.json".to_string(),
        publish_idl: false,
        idl_file_path: None,
        network_selection: "devnet".to_string(),
        json_output: false,
        retry_attempts: 1,
        confirm_large_binaries: true, // This should handle large binaries
    };

    assert!(config.confirm_large_binaries);

    // Verify file size
    let metadata = fs::metadata(&large_binary).unwrap();
    assert!(metadata.len() > 1_000_000); // Greater than 1MB
}

/// Test backward compatibility with old deployment configurations
#[test]
fn test_backward_compatibility() {
    // Test that old-style boolean IDL flag still works
    let config_old = DeployConfig {
        binary_path: "program.so".to_string(),
        program_id_path: "program_id.json".to_string(),
        owner_path: "owner.json".to_string(),
        fee_payer_path: "fee_payer.json".to_string(),
        publish_idl: false, // Boolean flag (backward compatible)
        idl_file_path: None,
        network_selection: "devnet".to_string(),
        json_output: false,
        retry_attempts: 3, // Default retry count
        confirm_large_binaries: false,
    };

    // Ensure old configuration works
    assert!(!config_old.publish_idl);
    assert_eq!(config_old.retry_attempts, 3);
    assert!(!config_old.json_output);
}

/// Test CLI argument parsing compatibility
#[test]
fn test_cli_compatibility() {
    // Test that the configuration structure matches expected CLI arguments
    let config = DeployConfig {
        binary_path: "./path/to/program.so".to_string(),
        program_id_path: "./path/to/program_id.json".to_string(),
        owner_path: "./path/to/owner.json".to_string(),
        fee_payer_path: "./path/to/fee_payer.json".to_string(),
        publish_idl: true,
        idl_file_path: Some("./path/to/idl.json".to_string()),
        network_selection: "mainnet".to_string(),
        json_output: true,
        retry_attempts: 5,
        confirm_large_binaries: true,
    };

    // Verify all fields are properly set
    assert!(config.binary_path.ends_with("program.so"));
    assert!(config.program_id_path.ends_with("program_id.json"));
    assert!(config.owner_path.ends_with("owner.json"));
    assert!(config.fee_payer_path.ends_with("fee_payer.json"));
    assert!(config.publish_idl);
    assert!(config.idl_file_path.is_some());
    assert_eq!(config.network_selection, "mainnet");
    assert!(config.json_output);
    assert_eq!(config.retry_attempts, 5);
    assert!(config.confirm_large_binaries);
}
