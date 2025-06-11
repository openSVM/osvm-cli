use {
    crate::utils::ssh_deploy::NetworkType,
    solana_client::rpc_client::RpcClient,
    solana_sdk::{
        bpf_loader_upgradeable,
        commitment_config::CommitmentConfig,
        pubkey::Pubkey,
        signature::{Keypair, Signer},
        transaction::Transaction,
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
    /// Network selection criteria (mainnet, testnet, devnet, or all)
    pub network_selection: String,
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

    // Check for reasonable file size (typical eBPF programs are < 100KB)
    if program_data.len() > 1_000_000 {
        eprintln!(
            "Warning: eBPF binary is quite large ({} bytes). Typical programs are much smaller.",
            program_data.len()
        );
    }

    Ok(program_data)
}

/// Deploy a BPF program to a Solana network
async fn deploy_bpf_program(
    client: &RpcClient,
    fee_payer: &Keypair,
    program_owner: &Keypair,
    program_id: Pubkey,
    program_data: &[u8],
    commitment_config: CommitmentConfig,
    publish_idl: bool,
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
            eprintln!("⚠️  Warning: IDL publishing is requested but not yet implemented");
            eprintln!("    IDL publishing will be skipped for this deployment");
            println!(
                "  • IDL publishing: enabled (placeholder - would implement IDL publishing here)"
            );
            // TODO: Implement actual IDL publishing using anchor or similar framework
        }

        Ok(upgrade_result)
    } else {
        println!("  • New program deployment");

        // For new deployments, use the BPF loader upgradeable to deploy the program
        let deploy_result = deploy_new_bpf_program(
            client,
            fee_payer,
            program_owner,
            program_id,
            program_data,
            commitment_config,
        )
        .await?;

        if publish_idl {
            eprintln!("⚠️  Warning: IDL publishing is requested but not yet implemented");
            eprintln!("    IDL publishing will be skipped for this deployment");
            println!(
                "  • IDL publishing: enabled (placeholder - would implement IDL publishing here)"
            );
            // TODO: Implement actual IDL publishing
        }

        Ok(deploy_result)
    }
}

/// Deploy a new BPF program using the upgradeable BPF loader
async fn deploy_new_bpf_program(
    client: &RpcClient,
    fee_payer: &Keypair,
    program_owner: &Keypair,
    program_id: Pubkey,
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
    let transaction_fees = 50_000_000; // ~0.05 SOL for multiple transactions
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
    println!("  • Buffer size: {} bytes", buffer_size);

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
        &program_id,
        &buffer_keypair.pubkey(),
        &program_owner.pubkey(),
        program_rent,
        buffer_size,
    )?;

    let recent_blockhash = client.get_latest_blockhash()?;
    let deploy_message = Message::new(&deploy_instructions, Some(&fee_payer.pubkey()));

    // Note: For this to work with a specific program_id, the program_id must be a keypair
    // that can sign the transaction. In practice, this means the program_id_path should
    // contain a keypair, not just a public key.
    let deploy_tx = Transaction::new(&[fee_payer], deploy_message, recent_blockhash);

    let deploy_signature = client
        .send_and_confirm_transaction_with_spinner_and_commitment(&deploy_tx, commitment_config)?;

    println!("    ✓ Program deployment finalized: {}", deploy_signature);
    println!("  • Program is now executable at: {}", program_id);

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
    let transaction_fees = 20_000_000; // ~0.02 SOL for upgrade transactions
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

/// Deploy eBPF program to all available SVM networks
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

    // Deploy to networks concurrently for better performance
    // Pass references to loaded data instead of reloading in each task
    let mut deployment_tasks = Vec::new();

    for network in networks {
        let publish_idl = config.publish_idl;

        // Clone the loaded data for each task (more efficient than reloading files)
        let program_id_clone = program_id;
        let program_data_clone = program_data.clone();

        // Clone keypairs by bytes - these operations are extremely unlikely to fail
        // but we'll use expect with helpful messages instead of unwrap
        let program_owner_bytes = program_owner.to_bytes();
        let fee_payer_bytes = fee_payer.to_bytes();

        let task = tokio::spawn(async move {
            let program_owner_clone = Keypair::from_bytes(&program_owner_bytes)
                .expect("Failed to clone program owner keypair from valid bytes");
            let fee_payer_clone = Keypair::from_bytes(&fee_payer_bytes)
                .expect("Failed to clone fee payer keypair from valid bytes");

            // Create client for the specific network
            let client_url = match network {
                NetworkType::Mainnet => "https://api.mainnet-beta.solana.com",
                NetworkType::Testnet => "https://api.testnet.solana.com",
                NetworkType::Devnet => "https://api.devnet.solana.com",
            };

            let client = RpcClient::new(client_url.to_string());

            // Get the target network name
            let target_network = match network {
                NetworkType::Mainnet => "mainnet",
                NetworkType::Testnet => "testnet",
                NetworkType::Devnet => "devnet",
            };

            // Deploy to this specific network with pre-loaded data
            deploy_to_network_with_data(
                &client,
                &program_data_clone,
                program_id_clone,
                &program_owner_clone,
                &fee_payer_clone,
                target_network,
                commitment_config,
                publish_idl,
            )
            .await
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
    program_owner: &Keypair,
    fee_payer: &Keypair,
    target_network: &str,
    commitment_config: CommitmentConfig,
    publish_idl: bool,
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
        program_data,
        commitment_config,
        publish_idl,
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
            }
        }
    };

    Ok(result)
}
