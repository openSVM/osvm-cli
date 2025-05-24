use osvm::utils::ebpf_deploy::{DeployConfig, load_program_id, load_keypair, load_program};
use osvm::utils::ssh_deploy::NetworkType;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
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
        network_type: NetworkType::Devnet,
    };
    
    // Verify config fields
    assert_eq!(config.binary_path, "path/to/binary.so");
    assert_eq!(config.program_id_path, "path/to/program_id.json");
    assert_eq!(config.owner_path, "path/to/owner.json");
    assert_eq!(config.fee_payer_path, "path/to/fee_payer.json");
    assert!(config.publish_idl);
    
    // Clone the config and verify the clone
    let config_clone = config.clone();
    assert_eq!(config_clone.binary_path, config.binary_path);
}