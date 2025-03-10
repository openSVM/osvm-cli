//! Solana deployment implementation

use {
    crate::utils::ssh_deploy::{
        client::SshClient,
        errors::DeploymentError,
        types::{ServerConfig, DeploymentConfig, NetworkType},
        services::{create_systemd_service, enable_and_start_service, await_service_startup, create_binary_service_content},
    },
};

/// Deploy Solana node
///
/// # Arguments
/// * `client` - SSH client
/// * `server_config` - Server configuration
/// * `deployment_config` - Deployment configuration
/// * `progress_callback` - Callback function for progress updates
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub async fn deploy_solana(
    client: &mut SshClient,
    server_config: &ServerConfig,
    deployment_config: &DeploymentConfig,
    progress_callback: Option<&Box<dyn Fn(u8, &str) + Send>>,
) -> Result<(), DeploymentError> {
    // Install Solana CLI
    if let Some(callback) = progress_callback {
        callback(40, "Installing Solana CLI");
    }
    
    install_solana_cli(client)?;
    
    // Create Solana directory and generate keypair
    let solana_dir = format!("{}/solana", server_config.install_dir);
    client.create_directory(&solana_dir)?;
    
    if let Some(callback) = progress_callback {
        callback(50, "Generating keypair");
    }
    
    let keypair_path = generate_solana_keypair(client, &solana_dir)?;
    
    // Configure network
    if let Some(callback) = progress_callback {
        callback(60, "Configuring network");
    }
    
    configure_solana_network(client, deployment_config.network)?;
    
    // Create systemd service
    if let Some(callback) = progress_callback {
        callback(70, "Creating systemd service");
    }
    
    create_solana_service(client, deployment_config, &solana_dir, &keypair_path).await?;
    
    Ok(())
}

/// Install Solana CLI
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn install_solana_cli(
    client: &mut SshClient,
) -> Result<(), DeploymentError> {
    client.execute_command("sh -c \"$(curl -sSfL https://release.solana.com/v1.16.0/install)\"")?;
    client.execute_command("export PATH=\"/home/$(whoami)/.local/share/solana/install/active_release/bin:$PATH\"")?;
    
    Ok(())
}

/// Generate Solana keypair
///
/// # Arguments
/// * `client` - SSH client
/// * `solana_dir` - Solana directory
///
/// # Returns
/// * `Result<String, DeploymentError>` - Keypair path
fn generate_solana_keypair(
    client: &mut SshClient,
    solana_dir: &str,
) -> Result<String, DeploymentError> {
    let keypair_path = format!("{}/validator-keypair.json", solana_dir);
    if !client.file_exists(&keypair_path)? {
        client.execute_command(&format!("solana-keygen new -o {} --no-passphrase", keypair_path))?;
    }
    
    Ok(keypair_path)
}

/// Configure Solana network
///
/// # Arguments
/// * `client` - SSH client
/// * `network` - Network type
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn configure_solana_network(
    client: &mut SshClient,
    network: NetworkType,
) -> Result<(), DeploymentError> {
    let network_flag = match network {
        NetworkType::Mainnet => "--url https://api.mainnet-beta.solana.com",
        NetworkType::Testnet => "--url https://api.testnet.solana.com",
        NetworkType::Devnet => "--url https://api.devnet.solana.com",
    };
    
    client.execute_command(&format!("solana config set {}", network_flag))?;
    
    Ok(())
}

/// Create Solana service
///
/// # Arguments
/// * `client` - SSH client
/// * `deployment_config` - Deployment configuration
/// * `solana_dir` - Solana directory
/// * `keypair_path` - Keypair path
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
async fn create_solana_service(
    client: &mut SshClient,
    deployment_config: &DeploymentConfig,
    solana_dir: &str,
    keypair_path: &str,
) -> Result<(), DeploymentError> {
    let service_name = format!("solana-{}-{}", deployment_config.node_type, deployment_config.network);
    
    // Get service arguments based on node type
    let args = get_solana_service_args(
        deployment_config,
        solana_dir,
        keypair_path,
    );
    
    // Create service content
    let args_ref: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let service_content = create_binary_service_content(
        "/home/$(whoami)/.local/share/solana/install/active_release/bin/solana-validator",
        &args_ref,
        solana_dir,
        &format!("Solana {}", deployment_config.node_type.to_uppercase()),
    );
    
    // Create and start the service
    create_systemd_service(client, &service_name, &service_content)?;
    enable_and_start_service(client, &service_name)?;
    
    // Wait for the service to start
    await_service_startup(client, &service_name).await?;
    
    Ok(())
}

/// Get Solana service arguments
///
/// # Arguments
/// * `deployment_config` - Deployment configuration
/// * `solana_dir` - Solana directory
/// * `keypair_path` - Keypair path
///
/// # Returns
/// * `Vec<String>` - Service arguments
fn get_solana_service_args<'a>(
    deployment_config: &'a DeploymentConfig,
    solana_dir: &'a str,
    keypair_path: &'a str,
) -> Vec<String> {
    let mut args = vec![
        format!("--identity {}", keypair_path),
        format!("--ledger {}/ledger", solana_dir),
        "--rpc-port 8899".to_string(),
        "--dynamic-port-range 8000-8020".to_string(),
        format!("--entrypoint entrypoint.{}.solana.com:8001", deployment_config.network),
        "--expected-genesis-hash GENESIS_HASH".to_string(),
        "--wal-recovery-mode skip_any_corrupted_record".to_string(),
        "--limit-ledger-size".to_string(),
    ];
    
    if deployment_config.node_type == "rpc" {
        args.push("--private-rpc".to_string());
        args.push("--enable-rpc-transaction-history".to_string());
    }
    
    args
}
