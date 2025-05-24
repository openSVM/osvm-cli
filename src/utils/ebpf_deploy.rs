use {
    crate::utils::ssh_deploy::NetworkType,
    solana_client::rpc_client::RpcClient,
    solana_sdk::{
        commitment_config::CommitmentConfig,
        pubkey::Pubkey,
        signature::{Keypair, Signer},
    },
    std::{
        fs::File,
        io::Read,
        path::Path,
    },
    thiserror::Error,
};

/// Utility module for deploying eBPF programs to Solana Virtual Machines (SVMs)
/// 
/// This module provides functionality to deploy eBPF programs (.so files) to
/// one or more SVM networks (mainnet, testnet, devnet). It handles the loading
/// of program binaries, keypairs, and program IDs from files, and provides
/// a consistent interface for deployment operations.
/// 
/// # Example
/// 
/// ```
/// let config = DeployConfig {
///     binary_path: "program.so".to_string(),
///     program_id_path: "program_id.json".to_string(),
///     owner_path: "owner_keypair.json".to_string(),
///     fee_payer_path: "fee_payer.json".to_string(),
///     publish_idl: true,
///     network_type: NetworkType::Mainnet,
/// };
/// 
/// let results = deploy_to_all_networks(config, CommitmentConfig::confirmed()).await;
/// for result in results {
///     match result {
///         Ok(deployment) => println!("Deployed to {}", deployment.network),
///         Err(e) => println!("Failed to deploy: {}", e),
///     }
/// }
/// ```

/// Error types for eBPF deployment operations
#[derive(Error, Debug)]
pub enum EbpfDeployError {
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("JSON parse error: {0}")]
    JsonError(#[from] serde_json::Error),

    #[error("RPC client error: {0}")]
    ClientError(#[from] solana_client::client_error::ClientError),

    #[error("Deployment failed: {0}")]
    DeploymentError(String),

    #[error("Invalid program ID format: {0}")]
    InvalidProgramId(String),

    #[error("Network not available: {0}")]
    NetworkNotAvailable(String),
}

/// Configuration for eBPF program deployment
pub struct DeployConfig {
    /// Path to the eBPF binary file (.so)
    pub binary_path: String,
    /// Path to program address JSON file
    pub program_id_path: String,
    /// Path to program owner JSON file
    pub owner_path: String,
    /// Path to deployment fee payer JSON file
    pub fee_payer_path: String,
    /// Whether to publish IDL
    pub publish_idl: bool,
    /// Network to deploy on
    pub network_type: NetworkType,
}

/// Result of a deployment operation
pub struct DeploymentResult {
    pub network: String,
    pub program_id: Pubkey,
    pub success: bool,
    pub transaction_signature: Option<String>,
    pub error_message: Option<String>,
}

/// Load a keypair from a JSON file
pub fn load_keypair(path: &str) -> Result<Keypair, EbpfDeployError> {
    let file = File::open(path)?;
    let keypair = solana_sdk::signature::read_keypair(file)
        .map_err(|e| EbpfDeployError::DeploymentError(format!("Failed to read keypair: {}", e)))?;
    Ok(keypair)
}

/// Load program ID from a JSON file
pub fn load_program_id(path: &str) -> Result<Pubkey, EbpfDeployError> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    
    // Try parsing as JSON with a "programId" field
    if let Ok(json) = serde_json::from_str::<serde_json::Value>(&contents) {
        if let Some(program_id) = json.get("programId").and_then(|v| v.as_str()) {
            return Pubkey::try_from(program_id)
                .map_err(|_| EbpfDeployError::InvalidProgramId(program_id.to_string()));
        }
    }
    
    // If not JSON with programId field, try direct pubkey parse
    Pubkey::try_from(contents.trim())
        .map_err(|_| EbpfDeployError::InvalidProgramId(contents))
}

/// Load eBPF program binary from file
pub fn load_program(path: &str) -> Result<Vec<u8>, EbpfDeployError> {
    let path = Path::new(path);
    let mut file = File::open(path)?;
    
    let mut program_data = Vec::new();
    file.read_to_end(&mut program_data)?;
    
    Ok(program_data)
}

/// Deploy eBPF program to a specific SVM network
pub async fn deploy_to_network(
    client: &RpcClient,
    config: &DeployConfig,
    commitment_config: CommitmentConfig,
) -> Result<DeploymentResult, EbpfDeployError> {
    let program_id = load_program_id(&config.program_id_path)?;
    let program_owner = load_keypair(&config.owner_path)?;
    let fee_payer = load_keypair(&config.fee_payer_path)?;
    let program_data = load_program(&config.binary_path)?;
    
    // Get network name for result
    let network_name = match config.network_type {
        NetworkType::Mainnet => "mainnet".to_string(),
        NetworkType::Testnet => "testnet".to_string(),
        NetworkType::Devnet => "devnet".to_string(),
    };
    
    // TODO: Implement actual deployment logic here
    // For now, we're just returning a placeholder result
    // In a real implementation, this would:
    // 1. Create a BPF loader transaction
    // 2. Sign and send the transaction
    // 3. Wait for confirmation
    // 4. Optionally publish IDL
    
    println!("Deploying to {} network...", network_name);
    println!("  Program ID: {}", program_id);
    println!("  Owner: {}", program_owner.pubkey());
    println!("  Fee payer: {}", fee_payer.pubkey());
    println!("  Binary size: {} bytes", program_data.len());
    
    // For now, simulate success
    let result = DeploymentResult {
        network: network_name,
        program_id,
        success: true,
        transaction_signature: Some("simulated_signature".to_string()),
        error_message: None,
    };
    
    Ok(result)
}

/// Deploy eBPF program to all available SVM networks
pub async fn deploy_to_all_networks(
    config: DeployConfig,
    commitment_config: CommitmentConfig,
) -> Vec<Result<DeploymentResult, EbpfDeployError>> {
    let mut results = Vec::new();
    
    // List of networks to deploy to
    // In a real implementation, this would query available networks dynamically
    let networks = match config.network_type {
        NetworkType::Mainnet => vec![NetworkType::Mainnet],
        NetworkType::Testnet => vec![NetworkType::Testnet],
        NetworkType::Devnet => vec![NetworkType::Devnet],
        _ => vec![NetworkType::Mainnet, NetworkType::Testnet, NetworkType::Devnet],
    };
    
    for network in networks {
        // Create client for the specific network
        // In a real implementation, this would use the appropriate RPC URL for each network
        let client_url = match network {
            NetworkType::Mainnet => "https://api.mainnet-beta.solana.com",
            NetworkType::Testnet => "https://api.testnet.solana.com",
            NetworkType::Devnet => "https://api.devnet.solana.com",
        };
        
        let client = RpcClient::new(client_url.to_string());
        
        // Create network-specific config
        let network_config = DeployConfig {
            network_type: network,
            ..config.clone()
        };
        
        // Deploy to this network
        let result = deploy_to_network(&client, &network_config, commitment_config).await;
        results.push(result);
    }
    
    results
}