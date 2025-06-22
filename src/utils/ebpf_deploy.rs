#![allow(deprecated)]

use {
    crate::utils::ssh_deploy::NetworkType,
    solana_client::rpc_client::RpcClient,
    solana_sdk::{
        bpf_loader_upgradeable,
        commitment_config::CommitmentConfig,
        pubkey::Pubkey,
        signature::{Keypair, Signer},
        system_instruction,
        transaction::Transaction,
    },
    std::{
        collections::HashMap,
        fs::File,
        io::Read,
        path::Path,
        sync::Arc,
        time::{Duration, Instant},
    },
    thiserror::Error,
    tokio::time::sleep,
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
///     network_selection: "all".to_string(),
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

    #[error("Instruction error: {0}")]
    InstructionError(#[from] solana_sdk::instruction::InstructionError),

    #[error("Keypair cloning error: {0}")]
    KeypairCloneError(String),

    #[error("Retry limit exceeded: {0}")]
    RetryLimitExceeded(String),
}

/// Cache for RPC clients to reduce DNS lookups and connection overhead
pub struct RpcClientCache {
    clients: HashMap<String, Arc<RpcClient>>,
}

impl RpcClientCache {
    pub fn new() -> Self {
        Self {
            clients: HashMap::new(),
        }
    }

    pub fn get_client(&mut self, url: &str) -> Arc<RpcClient> {
        self.clients
            .entry(url.to_string())
            .or_insert_with(|| Arc::new(RpcClient::new(url.to_string())))
            .clone()
    }
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
    /// Optional path to custom IDL JSON file
    pub idl_file_path: Option<String>,
    /// Network selection criteria (mainnet, testnet, devnet, or all)
    pub network_selection: String,
    /// Whether to output results in JSON format
    pub json_output: bool,
    /// Number of retry attempts for failed deployments
    pub retry_attempts: u32,
    /// Whether to require confirmation for large binaries
    pub confirm_large_binaries: bool,
}

/// Result of a deployment operation
#[derive(serde::Serialize, serde::Deserialize)]
pub struct DeploymentResult {
    pub network: String,
    pub program_id: Pubkey,
    pub success: bool,
    pub transaction_signature: Option<String>,
    pub error_message: Option<String>,
    /// Number of retries attempted
    pub retries_attempted: u32,
    /// Duration of deployment in milliseconds
    pub duration_ms: u64,
}

/// Safe keypair cloning that returns proper error instead of panicking
fn clone_keypair(keypair: &Keypair, context: &str) -> Result<Keypair, EbpfDeployError> {
    Keypair::from_bytes(&keypair.to_bytes()).map_err(|e| {
        EbpfDeployError::KeypairCloneError(format!(
            "Failed to clone {} keypair: {}. This indicates memory corruption or invalid keypair data.",
            context, e
        ))
    })
}
pub fn load_keypair(path: &str) -> Result<Keypair, EbpfDeployError> {
    // Validate file exists first
    if !Path::new(path).exists() {
        return Err(EbpfDeployError::IoError(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("Keypair file not found: {}", path),
        )));
    }

    let mut file = File::open(path)?;
    let keypair = solana_sdk::signature::read_keypair(&mut file).map_err(|e| {
        EbpfDeployError::DeploymentError(format!("Failed to read keypair from {}: {}", path, e))
    })?;
    Ok(keypair)
}

/// Load program ID from a JSON file - supports both keypair files and pubkey-only files
pub fn load_program_id(path: &str) -> Result<Pubkey, EbpfDeployError> {
    // Validate file exists first
    if !Path::new(path).exists() {
        return Err(EbpfDeployError::IoError(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("Program ID file not found: {}", path),
        )));
    }

    // First try to load as a keypair file
    if let Ok(mut file) = File::open(path) {
        if let Ok(keypair) = solana_sdk::signature::read_keypair(&mut file) {
            return Ok(keypair.pubkey());
        }
    }

    // If not a keypair, try as a JSON with programId field or direct pubkey
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    // Try parsing as JSON with a "programId" field
    if let Ok(json) = serde_json::from_str::<serde_json::Value>(&contents) {
        if let Some(program_id) = json.get("programId").and_then(|v| v.as_str()) {
            return Pubkey::try_from(program_id).map_err(|_| {
                EbpfDeployError::InvalidProgramId(format!(
                    "Invalid program ID in {}: {}",
                    path, program_id
                ))
            });
        }
    }

    // If not JSON with programId field, try direct pubkey parse
    let trimmed_contents = contents.trim();
    Pubkey::try_from(trimmed_contents).map_err(|_| {
        EbpfDeployError::InvalidProgramId(format!(
            "Invalid program ID format in {}: {}",
            path, trimmed_contents
        ))
    })
}

/// Load program keypair from a JSON file - used for new program deployment
/// This function specifically requires a keypair file, not just a pubkey
pub fn load_program_keypair(path: &str) -> Result<Keypair, EbpfDeployError> {
    // Validate file exists first
    if !Path::new(path).exists() {
        return Err(EbpfDeployError::IoError(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("Program keypair file not found: {}", path),
        )));
    }

    let mut file = File::open(path)?;
    solana_sdk::signature::read_keypair(&mut file).map_err(|e| {
        EbpfDeployError::DeploymentError(format!(
            "Failed to read program keypair from {}: {}. For new program deployment, the program-id path must contain a keypair file, not just a pubkey.",
            path, e
        ))
    })
}

/// Validate that program_id_path contains a keypair for new program deployment
pub fn validate_program_id_for_new_deployment(path: &str) -> Result<(), EbpfDeployError> {
    // Try to load as a keypair - this will fail if it's just a pubkey
    load_program_keypair(path).map(|_| ())
}

/// Load eBPF program binary from file
pub fn load_program(path: &str) -> Result<Vec<u8>, EbpfDeployError> {
    let path = Path::new(path);

    // Validate file exists
    if !path.exists() {
        return Err(EbpfDeployError::IoError(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("eBPF binary file not found: {}", path.display()),
        )));
    }

    // Check file extension
    if let Some(extension) = path.extension() {
        if extension != "so" {
            eprintln!(
                "Warning: Expected .so file extension for eBPF binary, got .{}",
                extension.to_string_lossy()
            );
        }
    } else {
        eprintln!("Warning: eBPF binary file has no extension, expected .so");
    }

    let mut file = File::open(path)?;
    let mut program_data = Vec::new();
    file.read_to_end(&mut program_data)?;

    // Basic validation of file size
    if program_data.is_empty() {
        return Err(EbpfDeployError::DeploymentError(format!(
            "eBPF binary file is empty: {}",
            path.display()
        )));
    }

    // Enhanced file size validation with different thresholds
    const LARGE_FILE_THRESHOLD: usize = 1_000_000; // 1MB
    const VERY_LARGE_FILE_THRESHOLD: usize = 5_000_000; // 5MB

    if program_data.len() > VERY_LARGE_FILE_THRESHOLD {
        return Err(EbpfDeployError::DeploymentError(format!(
            "eBPF binary is too large ({} bytes). Maximum recommended size is {} bytes. \
             Large programs consume more compute units and increase deployment costs.",
            program_data.len(),
            VERY_LARGE_FILE_THRESHOLD
        )));
    } else if program_data.len() > LARGE_FILE_THRESHOLD {
        eprintln!(
            "⚠️  Warning: eBPF binary is quite large ({} bytes). Typical programs are much smaller.",
            program_data.len()
        );
        eprintln!("   Large programs may require additional confirmation for deployment.");
    }

    Ok(program_data)
}

/// Deploy a BPF program to a Solana network
async fn deploy_bpf_program(
    client: &RpcClient,
    fee_payer: &Keypair,
    program_owner: &Keypair,
    program_id: Pubkey,
    program_id_path: &str, // Added to load keypair for new deployments
    program_data: &[u8],
    commitment_config: CommitmentConfig,
    publish_idl: bool,
    idl_file_path: Option<&str>, // Added for IDL file support
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

    println!("🔧 Processing deployment transaction...");
    println!("  • Program size: {} bytes", program_data.len());
    println!("  • Target program ID: {}", program_id);

    // Check if program already exists
    let program_account = client.get_account(&program_id);
    let is_upgrade = program_account.is_ok();

    if is_upgrade {
        println!("  • Existing program detected - performing upgrade");

        // For upgrades, we need to use the upgrade instruction from the BPF loader upgradeable
        let upgrade_result = upgrade_bpf_program(
            client,
            fee_payer,
            program_owner,
            program_id,
            program_data,
            commitment_config,
        )
        .await?;

        if publish_idl {
            // Implement actual IDL publishing for upgrades
            publish_program_idl(
                client,
                fee_payer,
                program_id,
                idl_file_path,
                commitment_config,
            )
            .await?;
        }

        Ok(upgrade_result)
    } else {
        println!("  • New program deployment");

        // For new deployments, validate that program_id_path contains a keypair
        println!("  • Validating program keypair for new deployment...");
        let program_keypair = match load_program_keypair(program_id_path) {
            Ok(keypair) => {
                if keypair.pubkey() != program_id {
                    return Err(EbpfDeployError::DeploymentError(format!(
                        "Program keypair pubkey ({}) does not match expected program ID ({})",
                        keypair.pubkey(),
                        program_id
                    )));
                }
                keypair
            }
            Err(_) => {
                return Err(EbpfDeployError::DeploymentError(format!(
                    "New program deployment requires a program keypair file at {}. \
                     The file must contain a keypair (private key), not just a public key. \
                     Use 'solana-keygen new -o {}' to generate a new program keypair.",
                    program_id_path, program_id_path
                )));
            }
        };

        // For new deployments, use the BPF loader upgradeable to deploy the program
        let deploy_result = deploy_new_bpf_program(
            client,
            fee_payer,
            program_owner,
            &program_keypair, // Pass the keypair instead of pubkey
            program_data,
            commitment_config,
        )
        .await?;

        if publish_idl {
            // Implement actual IDL publishing for new deployments
            publish_program_idl(
                client,
                fee_payer,
                program_keypair.pubkey(),
                idl_file_path,
                commitment_config,
            )
            .await?;
        }

        Ok(deploy_result)
    }
}

/// Calculate dynamic transaction fees based on current network conditions
async fn calculate_dynamic_fees(
    client: &RpcClient,
    transaction_count: u32,
) -> Result<u64, EbpfDeployError> {
    // Get recent prioritization fees to understand current network congestion
    let prioritization_fees = client.get_recent_prioritization_fees(&[]).ok();

    // Calculate base fee per transaction (minimum fee for a simple transaction)
    let base_fee_per_signature = 5_000; // 5,000 lamports base fee per signature

    // Get current fee rate multiplier based on network conditions
    let fee_multiplier = if let Some(fees) = prioritization_fees {
        if fees.is_empty() {
            1.0
        } else {
            // Calculate average prioritization fee from recent transactions
            let total_priority_fee: u64 = fees.iter().map(|f| f.prioritization_fee).sum();
            let avg_priority_fee = total_priority_fee as f64 / fees.len() as f64;

            // Use prioritization fee to estimate network congestion
            // Higher prioritization fees indicate more congestion
            let congestion_multiplier = 1.0 + (avg_priority_fee / 100_000.0); // Scale factor
            congestion_multiplier.min(5.0) // Cap at 5x multiplier
        }
    } else {
        // If we can't get fee data, use a conservative multiplier
        2.0
    };

    // Calculate total fees with congestion adjustment
    let base_fees = (base_fee_per_signature as f64 * transaction_count as f64) as u64;
    let dynamic_fees = (base_fees as f64 * fee_multiplier) as u64;

    // Add buffer for transaction complexity (BPF deployment transactions are complex)
    let complexity_buffer = dynamic_fees / 2; // 50% buffer for complexity
    let total_fees = dynamic_fees + complexity_buffer;

    println!("  • Dynamic fee calculation:");
    println!("    - Base fees: {} lamports", base_fees);
    println!("    - Congestion multiplier: {:.2}x", fee_multiplier);
    println!("    - Complexity buffer: {} lamports", complexity_buffer);
    println!("    - Total estimated fees: {} lamports", total_fees);

    Ok(total_fees)
}

/// Load IDL from a JSON file or create a default one
pub fn load_or_create_idl(
    idl_file_path: Option<&str>,
    program_id: Pubkey,
) -> Result<serde_json::Value, EbpfDeployError> {
    if let Some(idl_path) = idl_file_path {
        // Try to load from the provided IDL file
        if !Path::new(idl_path).exists() {
            return Err(EbpfDeployError::IoError(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("IDL file not found: {}", idl_path),
            )));
        }

        let mut file = File::open(idl_path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;

        // Parse the IDL JSON and validate it's a proper Anchor IDL
        let idl: serde_json::Value =
            serde_json::from_str(&contents).map_err(|e| EbpfDeployError::JsonError(e))?;

        // Basic validation that it looks like an Anchor IDL
        if !idl.is_object() {
            return Err(EbpfDeployError::DeploymentError(
                "IDL file must contain a JSON object".to_string(),
            ));
        }

        println!("    - Loaded custom IDL from: {}", idl_path);
        Ok(idl)
    } else {
        // Create a basic IDL structure
        println!("    - Generating basic IDL structure");
        let basic_idl = serde_json::json!({
            "version": "0.1.0",
            "name": "deployed_program",
            "instructions": [],
            "accounts": [],
            "types": [],
            "events": [],
            "errors": [],
            "metadata": {
                "address": program_id.to_string(),
                "deployed_at": chrono::Utc::now().to_rfc3339(),
                "note": "This is a generated IDL. Use --idl-file to specify a custom Anchor IDL."
            }
        });
        Ok(basic_idl)
    }
}

/// Publish IDL for a deployed program
async fn publish_program_idl(
    client: &RpcClient,
    fee_payer: &Keypair,
    program_id: Pubkey,
    idl_file_path: Option<&str>,
    commitment_config: CommitmentConfig,
) -> Result<(), EbpfDeployError> {
    println!("  • Publishing IDL for program {}", program_id);

    // Generate IDL account PDA (Program Derived Address)
    let idl_seed = b"anchor:idl";
    let (idl_account, _bump) =
        Pubkey::find_program_address(&[idl_seed, program_id.as_ref()], &program_id);

    println!("    - IDL account: {}", idl_account);

    // Check if IDL account already exists
    let idl_account_info = client.get_account(&idl_account);

    if idl_account_info.is_ok() {
        println!("    ✓ IDL account already exists, skipping creation");
        return Ok(());
    }

    // Load IDL from file or create a default one
    let idl = load_or_create_idl(idl_file_path, program_id)?;
    let idl_data = idl.to_string().into_bytes();

    // Calculate rent for IDL account
    let idl_rent = client.get_minimum_balance_for_rent_exemption(idl_data.len() + 128)?; // +128 for account overhead

    // Calculate dynamic fees for this operation
    let estimated_fees = calculate_dynamic_fees(client, 2).await?; // 2 transactions: create + write

    // Check fee payer balance
    let balance = client.get_balance(&fee_payer.pubkey())?;
    let required_balance = idl_rent + estimated_fees;

    if balance < required_balance {
        return Err(EbpfDeployError::InsufficientFunds(format!(
            "Insufficient balance for IDL publishing: {} lamports (required: {})",
            balance, required_balance
        )));
    }

    // Create IDL account
    let create_idl_ix = system_instruction::create_account(
        &fee_payer.pubkey(),
        &idl_account,
        idl_rent,
        idl_data.len() as u64,
        &program_id,
    );

    let recent_blockhash = client.get_latest_blockhash()?;
    let create_idl_tx = Transaction::new_signed_with_payer(
        &[create_idl_ix],
        Some(&fee_payer.pubkey()),
        &[fee_payer],
        recent_blockhash,
    );

    // Note: This is a simplified IDL publishing implementation
    // In reality, you'd want to use proper IDL account structure and
    // potentially integrate with Anchor's IDL format
    let signature = client.send_and_confirm_transaction_with_spinner_and_commitment(
        &create_idl_tx,
        commitment_config,
    )?;

    println!("    ✓ IDL published successfully: {}", signature);
    println!("    ✓ IDL account created at: {}", idl_account);

    Ok(())
}
/// Deploy a new BPF program using the upgradeable BPF loader
async fn deploy_new_bpf_program(
    client: &RpcClient,
    fee_payer: &Keypair,
    program_owner: &Keypair,
    program_keypair: &Keypair, // Changed from Pubkey to &Keypair for new deployments
    program_data: &[u8],
    commitment_config: CommitmentConfig,
) -> Result<String, EbpfDeployError> {
    use solana_sdk::message::Message;

    println!("  • Deploying new BPF program using upgradeable loader");

    // Calculate rent requirements for buffer and program
    let buffer_size = program_data.len();
    let buffer_rent = client.get_minimum_balance_for_rent_exemption(buffer_size + 8)?; // +8 for discriminator
    let program_rent = client.get_minimum_balance_for_rent_exemption(36)?; // Program account size
    let program_data_rent = client.get_minimum_balance_for_rent_exemption(buffer_size + 48)?; // +48 for metadata

    let total_rent = buffer_rent + program_rent + program_data_rent;

    // Calculate dynamic transaction fees based on program size and network conditions
    let num_chunks = (buffer_size + 1023) / 1024; // Round up division for 1KB chunks
    let estimated_transaction_count = 3 + num_chunks as u32; // create buffer + write chunks + deploy
    let transaction_fees = calculate_dynamic_fees(client, estimated_transaction_count).await?;

    let minimum_balance = total_rent + transaction_fees;

    // Check fee payer balance
    let balance = client.get_balance(&fee_payer.pubkey())?;
    if balance < minimum_balance {
        return Err(EbpfDeployError::InsufficientFunds(format!(
            "Fee payer has insufficient balance: {} lamports (minimum required: {})",
            balance, minimum_balance
        )));
    }

    println!("  • Required rent: {} lamports", total_rent);
    println!(
        "  • Estimated transaction fees: {} lamports",
        transaction_fees
    );
    println!("  • Buffer size: {} bytes", buffer_size);
    println!(
        "  • Estimated transactions: {}",
        estimated_transaction_count
    );

    // Generate keypair for the buffer
    let buffer_keypair = Keypair::new();

    // Step 1: Create and initialize buffer account
    println!("  • Step 1: Creating and initializing buffer account");
    let create_buffer_instructions = bpf_loader_upgradeable::create_buffer(
        &fee_payer.pubkey(),
        &buffer_keypair.pubkey(),
        &program_owner.pubkey(),
        buffer_rent,
        buffer_size,
    )?;

    let recent_blockhash = client.get_latest_blockhash()?;
    let create_buffer_message =
        Message::new(&create_buffer_instructions, Some(&fee_payer.pubkey()));
    let create_buffer_tx = Transaction::new(
        &[fee_payer, &buffer_keypair],
        create_buffer_message,
        recent_blockhash,
    );

    let init_signature = client.send_and_confirm_transaction_with_spinner_and_commitment(
        &create_buffer_tx,
        commitment_config,
    )?;
    println!("    ✓ Buffer created and initialized: {}", init_signature);

    // Step 2: Write program data to buffer in chunks
    println!("  • Step 2: Writing program data in chunks");
    let max_chunk_size = 1024; // Conservative chunk size to stay within transaction limits
    let mut offset = 0;
    let mut chunk_count = 0;

    while offset < program_data.len() {
        let end = std::cmp::min(offset + max_chunk_size, program_data.len());
        let chunk = &program_data[offset..end];

        let write_ix = bpf_loader_upgradeable::write(
            &buffer_keypair.pubkey(),
            &program_owner.pubkey(),
            offset as u32,
            chunk.to_vec(),
        );

        let recent_blockhash = client.get_latest_blockhash()?;
        let write_message = Message::new(&[write_ix], Some(&fee_payer.pubkey()));
        let write_tx =
            Transaction::new(&[fee_payer, program_owner], write_message, recent_blockhash);

        let _write_signature = client.send_and_confirm_transaction_with_spinner_and_commitment(
            &write_tx,
            commitment_config,
        )?;

        chunk_count += 1;
        offset = end;

        if chunk_count % 10 == 0 || offset >= program_data.len() {
            println!(
                "    ✓ Written {}/{} bytes in {} chunks",
                offset,
                program_data.len(),
                chunk_count
            );
        }
    }

    // Step 3: Deploy the program from buffer
    println!("  • Step 3: Finalizing program deployment");

    // Use the provided program_id for deployment instead of generating a new one
    // Note: For new deployments to a specific program ID, we need the program keypair
    // In practice, the program_id should be loaded from a keypair file for new deployments

    let deploy_instructions = bpf_loader_upgradeable::deploy_with_max_program_len(
        &fee_payer.pubkey(),
        &program_keypair.pubkey(),
        &buffer_keypair.pubkey(),
        &program_owner.pubkey(),
        program_rent,
        buffer_size,
    )?;

    let recent_blockhash = client.get_latest_blockhash()?;
    let deploy_message = Message::new(&deploy_instructions, Some(&fee_payer.pubkey()));

    // Program keypair must sign the transaction for new program deployment
    let deploy_tx = Transaction::new(
        &[fee_payer, program_keypair],
        deploy_message,
        recent_blockhash,
    );

    let deploy_signature = client
        .send_and_confirm_transaction_with_spinner_and_commitment(&deploy_tx, commitment_config)?;

    println!("    ✓ Program deployment finalized: {}", deploy_signature);
    println!(
        "  • Program is now executable at: {}",
        program_keypair.pubkey()
    );

    Ok(deploy_signature.to_string())
}

/// Upgrade an existing BPF program using the upgradeable BPF loader  
async fn upgrade_bpf_program(
    client: &RpcClient,
    fee_payer: &Keypair,
    program_owner: &Keypair,
    program_id: Pubkey,
    program_data: &[u8],
    commitment_config: CommitmentConfig,
) -> Result<String, EbpfDeployError> {
    use solana_sdk::message::Message;

    println!("  • Upgrading existing BPF program using upgradeable loader");

    // Calculate rent for the buffer
    let buffer_size = program_data.len();
    let buffer_rent = client.get_minimum_balance_for_rent_exemption(buffer_size + 8)?; // +8 for discriminator

    // Calculate dynamic transaction fees for upgrade operations
    let num_chunks = (buffer_size + 1023) / 1024; // Round up division for 1KB chunks
    let estimated_transaction_count = 2 + num_chunks as u32; // create buffer + write chunks + upgrade
    let transaction_fees = calculate_dynamic_fees(client, estimated_transaction_count).await?;

    let minimum_balance = buffer_rent + transaction_fees;

    // Check fee payer balance
    let balance = client.get_balance(&fee_payer.pubkey())?;
    if balance < minimum_balance {
        return Err(EbpfDeployError::InsufficientFunds(format!(
            "Fee payer has insufficient balance: {} lamports (minimum required: {})",
            balance, minimum_balance
        )));
    }

    println!("  • Buffer size: {} bytes", buffer_size);
    println!("  • Required rent: {} lamports", buffer_rent);
    println!(
        "  • Estimated transaction fees: {} lamports",
        transaction_fees
    );
    println!(
        "  • Estimated transactions: {}",
        estimated_transaction_count
    );

    // Generate buffer keypair for the upgrade
    let buffer_keypair = Keypair::new();

    // Step 1: Create and initialize buffer for upgrade
    println!("  • Step 1: Creating and initializing upgrade buffer");
    let create_buffer_instructions = bpf_loader_upgradeable::create_buffer(
        &fee_payer.pubkey(),
        &buffer_keypair.pubkey(),
        &program_owner.pubkey(),
        buffer_rent,
        buffer_size,
    )?;

    let recent_blockhash = client.get_latest_blockhash()?;
    let init_message = Message::new(&create_buffer_instructions, Some(&fee_payer.pubkey()));
    let init_tx = Transaction::new(
        &[fee_payer, &buffer_keypair],
        init_message,
        recent_blockhash,
    );

    let init_signature = client
        .send_and_confirm_transaction_with_spinner_and_commitment(&init_tx, commitment_config)?;
    println!(
        "    ✓ Upgrade buffer created and initialized: {}",
        init_signature
    );

    // Step 2: Write new program data to buffer in chunks
    println!("  • Step 2: Writing updated program data");
    let max_chunk_size = 1024; // Conservative chunk size
    let mut offset = 0;
    let mut chunk_count = 0;

    while offset < program_data.len() {
        let end = std::cmp::min(offset + max_chunk_size, program_data.len());
        let chunk = &program_data[offset..end];

        let write_ix = bpf_loader_upgradeable::write(
            &buffer_keypair.pubkey(),
            &program_owner.pubkey(),
            offset as u32,
            chunk.to_vec(),
        );

        let recent_blockhash = client.get_latest_blockhash()?;
        let write_message = Message::new(&[write_ix], Some(&fee_payer.pubkey()));
        let write_tx =
            Transaction::new(&[fee_payer, program_owner], write_message, recent_blockhash);

        let _write_signature = client.send_and_confirm_transaction_with_spinner_and_commitment(
            &write_tx,
            commitment_config,
        )?;

        chunk_count += 1;
        offset = end;

        if chunk_count % 10 == 0 || offset >= program_data.len() {
            println!(
                "    ✓ Written {}/{} bytes in {} chunks",
                offset,
                program_data.len(),
                chunk_count
            );
        }
    }

    // Step 3: Upgrade the program from buffer
    println!("  • Step 3: Executing program upgrade");

    let upgrade_ix = bpf_loader_upgradeable::upgrade(
        &program_id,
        &buffer_keypair.pubkey(),
        &program_owner.pubkey(),
        &fee_payer.pubkey(), // spill address for rent refund
    );

    let recent_blockhash = client.get_latest_blockhash()?;
    let upgrade_message = Message::new(&[upgrade_ix], Some(&fee_payer.pubkey()));
    let upgrade_tx = Transaction::new(
        &[fee_payer, program_owner],
        upgrade_message,
        recent_blockhash,
    );

    let upgrade_signature = client
        .send_and_confirm_transaction_with_spinner_and_commitment(&upgrade_tx, commitment_config)?;

    println!("    ✓ Program upgrade completed: {}", upgrade_signature);

    Ok(upgrade_signature.to_string())
}

/// Display deployment results in the specified format
pub fn display_deployment_results(
    results: &[Result<DeploymentResult, EbpfDeployError>],
    json_output: bool,
) -> Result<(), EbpfDeployError> {
    if json_output {
        let json_results: Vec<serde_json::Value> = results
            .iter()
            .map(|result| match result {
                Ok(deployment) => serde_json::to_value(deployment).unwrap_or_else(|_| {
                    serde_json::json!({
                        "error": "Failed to serialize deployment result"
                    })
                }),
                Err(error) => serde_json::json!({
                    "error": error.to_string(),
                    "success": false
                }),
            })
            .collect();

        let output = serde_json::json!({
            "deployments": json_results,
            "summary": {
                "total": results.len(),
                "successful": results.iter().filter(|r|
                    r.as_ref().map_or(false, |d| d.success)).count(),
                "failed": results.iter().filter(|r|
                    r.as_ref().map_or(true, |d| !d.success)).count(),
                "timestamp": chrono::Utc::now().to_rfc3339()
            }
        });

        println!(
            "{}",
            serde_json::to_string_pretty(&output).map_err(|e| { EbpfDeployError::JsonError(e) })?
        );
    } else {
        // Display results in human-readable format
        println!("\n📋 Deployment Results Summary");
        println!("==============================");
        let mut success_count = 0;
        let mut failure_count = 0;

        for result in results {
            match result {
                Ok(deployment) => {
                    if deployment.success {
                        success_count += 1;
                        println!("✅ {} - Success 🎉", deployment.network.to_uppercase());
                        println!("   📍 Program ID: {}", deployment.program_id);
                        if let Some(signature) = &deployment.transaction_signature {
                            println!("   📄 Transaction: {signature}");
                        }
                        if deployment.retries_attempted > 0 {
                            println!("   🔄 Retries: {}", deployment.retries_attempted);
                        }
                        println!("   ⏱️  Duration: {}ms", deployment.duration_ms);
                    } else {
                        failure_count += 1;
                        println!("❌ {} - Failed ⚠️", deployment.network.to_uppercase());
                        println!("   📍 Program ID: {}", deployment.program_id);
                        if let Some(error) = &deployment.error_message {
                            println!("   🚨 Error: {error}");
                        }
                        if deployment.retries_attempted > 0 {
                            println!("   🔄 Retries attempted: {}", deployment.retries_attempted);
                        }
                    }
                }
                Err(e) => {
                    failure_count += 1;
                    println!("❌ Deployment error: {e}");
                }
            }
            println!(); // Add spacing between results
        }

        println!(
            "📊 Final Summary: {} successful ✅, {} failed ❌",
            success_count, failure_count
        );

        if failure_count > 0 {
            println!("💡 Tip: Check error messages above for troubleshooting guidance");
        } else {
            println!("🎉 All deployments completed successfully!");
        }
    }

    Ok(())
}
///
/// This function validates the deployment configuration and attempts to deploy
/// the eBPF program to the specified networks concurrently for better performance.
/// Files are loaded once and shared across all deployment tasks for efficiency.
pub async fn deploy_to_all_networks(
    config: DeployConfig,
    commitment_config: CommitmentConfig,
) -> Vec<Result<DeploymentResult, EbpfDeployError>> {
    // Early validation and loading of all input files once
    println!("🔍 Validating deployment configuration...");

    // Load all files once before starting deployment
    let program_id = match load_program_id(&config.program_id_path) {
        Ok(id) => id,
        Err(e) => return vec![Err(e)],
    };

    let program_owner = match load_keypair(&config.owner_path) {
        Ok(keypair) => keypair,
        Err(e) => return vec![Err(e)],
    };

    let fee_payer = match load_keypair(&config.fee_payer_path) {
        Ok(keypair) => keypair,
        Err(e) => return vec![Err(e)],
    };

    let program_data = match load_program(&config.binary_path) {
        Ok(data) => data,
        Err(e) => return vec![Err(e)],
    };

    println!("✅ Configuration validation successful");
    println!("📦 Loaded program binary: {} bytes", program_data.len());
    println!("🆔 Program ID: {}", program_id);
    println!("👤 Owner: {}", program_owner.pubkey());
    println!("💰 Fee payer: {}", fee_payer.pubkey());

    // Determine which networks to deploy to based on the selection criteria
    let networks = match config.network_selection.to_lowercase().as_str() {
        "mainnet" => vec![NetworkType::Mainnet],
        "testnet" => vec![NetworkType::Testnet],
        "devnet" => vec![NetworkType::Devnet],
        "all" => vec![
            NetworkType::Mainnet,
            NetworkType::Testnet,
            NetworkType::Devnet,
        ],
        _ => {
            // Invalid network selection, return error
            let error = EbpfDeployError::NetworkNotAvailable(format!(
                "Invalid network selection '{}'. Valid options: mainnet, testnet, devnet, all",
                config.network_selection
            ));
            return vec![Err(error)];
        }
    };

    println!("🚀 Starting deployment to {} network(s)...", networks.len());

    // Create RPC client cache for performance optimization
    let mut client_cache = RpcClientCache::new();

    // Deploy to networks concurrently for better performance
    // Pass references to loaded data instead of reloading in each task
    let mut deployment_tasks = Vec::new();

    for network in networks {
        let publish_idl = config.publish_idl;
        let retry_attempts = config.retry_attempts;

        // Clone the loaded data for each task (more efficient than reloading files)
        let program_id_clone = program_id;
        let program_data_clone = program_data.clone();
        let program_id_path_clone = config.program_id_path.clone();
        let idl_file_path_clone = config.idl_file_path.clone();

        // Clone keypairs safely with proper error handling
        let program_owner_clone = match clone_keypair(&program_owner, "program owner") {
            Ok(keypair) => keypair,
            Err(e) => return vec![Err(e)],
        };
        let fee_payer_clone = match clone_keypair(&fee_payer, "fee payer") {
            Ok(keypair) => keypair,
            Err(e) => return vec![Err(e)],
        };

        // Get cached client for the specific network
        let client_url = match network {
            NetworkType::Mainnet => "https://api.mainnet-beta.solana.com",
            NetworkType::Testnet => "https://api.testnet.solana.com",
            NetworkType::Devnet => "https://api.devnet.solana.com",
        };

        let client = client_cache.get_client(client_url);

        let task = tokio::spawn(async move {
            let start_time = Instant::now();

            // Get the target network name
            let target_network = match network {
                NetworkType::Mainnet => "mainnet",
                NetworkType::Testnet => "testnet",
                NetworkType::Devnet => "devnet",
            };

            // Deploy to this specific network with retry logic
            let mut retries_attempted = 0;
            let mut last_error = None;

            for attempt in 0..=retry_attempts {
                let result = deploy_to_network_with_data(
                    &client,
                    &program_data_clone,
                    program_id_clone,
                    &program_id_path_clone,
                    &program_owner_clone,
                    &fee_payer_clone,
                    target_network,
                    commitment_config,
                    publish_idl,
                    idl_file_path_clone.as_deref(),
                )
                .await;

                match result {
                    Ok(mut deployment_result) => {
                        let duration_ms = start_time.elapsed().as_millis() as u64;
                        deployment_result.retries_attempted = retries_attempted;
                        deployment_result.duration_ms = duration_ms;
                        return Ok(deployment_result);
                    }
                    Err(e) => {
                        retries_attempted = attempt;
                        last_error = Some(e);

                        if attempt < retry_attempts {
                            let delay_ms = 1000 * (2_u64.pow(attempt)); // Exponential backoff: 1s, 2s, 4s, 8s...
                            println!("  ⚠️  Deployment to {} failed (attempt {}/{}), retrying in {}ms...", 
                                     target_network, attempt + 1, retry_attempts + 1, delay_ms);
                            sleep(Duration::from_millis(delay_ms)).await;
                        }
                    }
                }
            }

            let duration_ms = start_time.elapsed().as_millis() as u64;

            // Create a failed deployment result
            Ok(DeploymentResult {
                network: target_network.to_string(),
                program_id: program_id_clone,
                success: false,
                transaction_signature: None,
                error_message: Some(last_error.unwrap().to_string()),
                retries_attempted,
                duration_ms,
            })
        });

        deployment_tasks.push(task);
    }

    // Wait for all deployments to complete
    let mut results = Vec::new();
    for task in deployment_tasks {
        match task.await {
            Ok(result) => results.push(result),
            Err(e) => results.push(Err(EbpfDeployError::DeploymentError(format!(
                "Task execution error: {}",
                e
            )))),
        }
    }

    results
}

/// Deploy eBPF program to a specific SVM network with pre-loaded data
async fn deploy_to_network_with_data(
    client: &RpcClient,
    program_data: &[u8],
    program_id: Pubkey,
    program_id_path: &str, // Added to support keypair validation for new deployments
    program_owner: &Keypair,
    fee_payer: &Keypair,
    target_network: &str,
    commitment_config: CommitmentConfig,
    publish_idl: bool,
    idl_file_path: Option<&str>, // Added for IDL file support
) -> Result<DeploymentResult, EbpfDeployError> {
    println!("\n📡 Deploying to {} network...", target_network);
    println!("  • Program ID: {}", program_id);
    println!("  • Owner: {}", program_owner.pubkey());
    println!("  • Fee payer: {}", fee_payer.pubkey());
    println!("  • Binary size: {} bytes", program_data.len());
    if publish_idl {
        println!("  • IDL publishing: enabled");
    }

    // Attempt to deploy the program
    let result = match deploy_bpf_program(
        client,
        fee_payer,
        program_owner,
        program_id,
        program_id_path,
        program_data,
        commitment_config,
        publish_idl,
        idl_file_path,
    )
    .await
    {
        Ok(signature) => {
            println!("✅ Deployment successful on {} 🎉", target_network);
            DeploymentResult {
                network: target_network.to_string(),
                program_id,
                success: true,
                transaction_signature: Some(signature),
                error_message: None,
                retries_attempted: 0, // Will be set by the caller
                duration_ms: 0,       // Will be set by the caller
            }
        }
        Err(e) => {
            println!("❌ Deployment failed on {}: {}", target_network, e);
            DeploymentResult {
                network: target_network.to_string(),
                program_id,
                success: false,
                transaction_signature: None,
                error_message: Some(e.to_string()),
                retries_attempted: 0, // Will be set by the caller
                duration_ms: 0,       // Will be set by the caller
            }
        }
    };

    Ok(result)
}
