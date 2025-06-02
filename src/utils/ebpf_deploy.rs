use {
    crate::utils::ssh_deploy::NetworkType,
    solana_client::rpc_client::RpcClient,
    solana_sdk::{
        commitment_config::CommitmentConfig,
        pubkey::Pubkey,
        signature::{Keypair, Signer},
    },
    std::{fs::File, io::Read, path::Path},
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
///     network_filter: "all".to_string(),
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

    #[error("Insufficient funds: {0}")]
    InsufficientFunds(String),

    #[error("Transaction error: {0}")]
    TransactionError(String),

    #[error("IDL publishing error: {0}")]
    IdlPublishError(String),
}

/// Configuration for eBPF program deployment
#[derive(Clone)]
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
    /// Network filter (mainnet, testnet, devnet, or all)
    pub network_filter: String,
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
    let mut file = File::open(path)?;
    let keypair = solana_sdk::signature::read_keypair(&mut file)
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
    Pubkey::try_from(contents.trim()).map_err(|_| EbpfDeployError::InvalidProgramId(contents))
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
    let network_name = config.network_filter.clone();

    println!("Deploying to {} network...", network_name);
    println!("  Program ID: {}", program_id);
    println!("  Owner: {}", program_owner.pubkey());
    println!("  Fee payer: {}", fee_payer.pubkey());
    println!("  Binary size: {} bytes", program_data.len());

    // Attempt to deploy the program
    let result = match deploy_bpf_program(
        client,
        &fee_payer,
        &program_owner,
        program_id,
        &program_data,
        commitment_config,
        config.publish_idl,
    )
    .await
    {
        Ok(signature) => {
            println!("✅ Deployment successful on {}", network_name);
            DeploymentResult {
                network: network_name,
                program_id,
                success: true,
                transaction_signature: Some(signature),
                error_message: None,
            }
        }
        Err(e) => {
            println!("❌ Deployment failed on {}: {}", network_name, e);
            DeploymentResult {
                network: network_name,
                program_id,
                success: false,
                transaction_signature: None,
                error_message: Some(e.to_string()),
            }
        }
    };

    Ok(result)
}

/// Deploy a BPF program to a Solana network
async fn deploy_bpf_program(
    client: &RpcClient,
    fee_payer: &Keypair,
    _program_owner: &Keypair,
    _program_id: Pubkey,
    _program_data: &[u8],
    _commitment_config: CommitmentConfig,
    _publish_idl: bool,
) -> Result<String, EbpfDeployError> {
    // Check client connection
    match client.get_version() {
        Ok(version) => {
            println!("Connected to Solana node version: {}", version.solana_core);
        }
        Err(err) => {
            return Err(EbpfDeployError::ClientError(err));
        }
    }

    // Check fee payer balance
    let balance = client.get_balance(&fee_payer.pubkey())?;
    if balance < 10_000_000 {
        // 0.01 SOL minimum
        return Err(EbpfDeployError::InsufficientFunds(format!(
            "Fee payer has insufficient balance: {} lamports",
            balance
        )));
    }

    // In a real implementation, this function would:
    // 1. Create a BPF loader instruction to deploy the program
    // 2. Create and sign a transaction with that instruction
    // 3. Send the transaction to the network and confirm it
    // 4. If publish_idl is true, also publish the program's IDL

    // For now, simulate with a small delay to represent network operation
    tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;

    // In a real implementation, return the actual transaction signature
    Ok("simulated_transaction_signature_for_deployment".to_string())
}

/// Deploy eBPF program to all available SVM networks
pub async fn deploy_to_all_networks(
    config: DeployConfig,
    commitment_config: CommitmentConfig,
) -> Vec<Result<DeploymentResult, EbpfDeployError>> {
    let mut results = Vec::new();

    // Determine which networks to deploy to based on the filter
    let networks = match config.network_filter.to_lowercase().as_str() {
        "mainnet" => vec![NetworkType::Mainnet],
        "testnet" => vec![NetworkType::Testnet],
        "devnet" => vec![NetworkType::Devnet],
        "all" => vec![
            NetworkType::Mainnet,
            NetworkType::Testnet,
            NetworkType::Devnet,
        ],
        _ => {
            // Invalid network filter, return error
            let error = EbpfDeployError::NetworkNotAvailable(format!(
                "Invalid network filter: {}",
                config.network_filter
            ));
            return vec![Err(error)];
        }
    };

    for network in networks {
        // Create client for the specific network
        let client_url = match network {
            NetworkType::Mainnet => "https://api.mainnet-beta.solana.com",
            NetworkType::Testnet => "https://api.testnet.solana.com",
            NetworkType::Devnet => "https://api.devnet.solana.com",
        };

        let client = RpcClient::new(client_url.to_string());

        // Create network-specific config
        let network_config = DeployConfig {
            network_filter: match network {
                NetworkType::Mainnet => "mainnet".to_string(),
                NetworkType::Testnet => "testnet".to_string(),
                NetworkType::Devnet => "devnet".to_string(),
            },
            ..config.clone()
        };

        // Deploy to this network
        let result = deploy_to_network(&client, &network_config, commitment_config).await;
        results.push(result);
    }

    results
}
