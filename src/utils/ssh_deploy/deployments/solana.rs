//! Solana deployment implementation

use crate::utils::ssh_deploy::{
    client::SshClient,
    disk_management::{setup_disk_storage, validate_disk_requirements},
    errors::DeploymentError,
    hot_swap::{configure_hot_swap, configure_log_rotation as configure_hs_log_rotation},
    monitoring::{install_monitoring_stack, setup_monitoring},
    optimizations::{apply_system_optimizations, configure_firewall, setup_log_rotation},
    services::{
        await_service_startup, create_binary_service_content, create_systemd_service,
        enable_and_start_service,
    },
    types::{DeploymentConfig, NetworkType, ServerConfig},
};

/// Deploy Solana node with enhanced features from Validator Jumpstart
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
    progress_callback: Option<&crate::prelude::ProgressCallback>,
) -> Result<(), DeploymentError> {
    // Set up disk storage if disk configuration is provided
    if let Some(disk_config) = &deployment_config.disk_config {
        if let Some(callback) = progress_callback {
            callback(10, "Validating disk requirements");
        }

        validate_disk_requirements(client, &disk_config.ledger_disk, &disk_config.accounts_disk)?;

        if let Some(callback) = progress_callback {
            callback(20, "Setting up disk storage");
        }

        setup_disk_storage(client, &disk_config.ledger_disk, &disk_config.accounts_disk)?;
    }

    // Apply system optimizations
    if let Some(callback) = progress_callback {
        callback(30, "Applying system optimizations");
    }

    apply_system_optimizations(client)?;

    // Configure firewall
    if let Some(callback) = progress_callback {
        callback(35, "Configuring firewall");
    }

    let is_rpc = deployment_config.node_type.to_lowercase() == "rpc";
    configure_firewall(client, is_rpc)?;

    // Install Solana CLI
    if let Some(callback) = progress_callback {
        callback(40, "Installing Solana CLI");
    }

    install_solana_cli(
        client,
        deployment_config.version.as_deref(),
        deployment_config.client_type.as_deref(),
    )?;

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

    // Set up hot-swap capability if enabled
    let mut hot_swap_keypairs = None;
    if deployment_config.hot_swap_enabled {
        if let Some(callback) = progress_callback {
            callback(65, "Setting up hot-swap capability");
        }

        hot_swap_keypairs = Some(configure_hot_swap(client, &solana_dir)?);
        configure_hs_log_rotation(client)?;
    } else {
        // Set up regular log rotation
        setup_log_rotation(client)?;
    }

    // Create systemd service
    if let Some(callback) = progress_callback {
        callback(70, "Creating systemd service");
    }

    // Use hot-swap keypairs if available, otherwise use the standard keypair
    let service_keypair = if let Some((staked_keypair, _)) = &hot_swap_keypairs {
        staked_keypair
    } else {
        &keypair_path
    };

    create_solana_service(client, deployment_config, &solana_dir, service_keypair).await?;

    // Set up monitoring
    if let Some(callback) = progress_callback {
        callback(80, "Setting up monitoring");
    }

    let metrics_config = deployment_config.metrics_config.as_deref().unwrap_or("");
    setup_monitoring(
        client,
        &solana_dir,
        metrics_config,
        &deployment_config.node_type,
    )?;

    // Install monitoring stack (optional)
    if let Some(callback) = progress_callback {
        callback(90, "Preparing monitoring stack");
    }

    install_monitoring_stack(client, &solana_dir)?;

    if let Some(callback) = progress_callback {
        callback(100, "Deployment completed successfully");
    }

    Ok(())
}

/// Install Solana CLI with version selection
///
/// # Arguments
/// * `client` - SSH client
/// * `version` - Optional Solana version
/// * `client_type` - Optional client type (standard, jito, agave, firedancer, sig)
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn install_solana_cli(
    client: &mut SshClient,
    version: Option<&str>,
    client_type: Option<&str>,
) -> Result<(), DeploymentError> {
    // Default version
    let version = version.unwrap_or("v1.16.0");

    // Handle different client types
    match client_type {
        Some("jito") => {
            // Install Jito client
            let jito_version = if version.contains("jito") {
                version.to_string()
            } else {
                format!("{}-jito", version)
            };

            client.execute_command(&format!(
                "sh -c \"$(curl -sSfL https://release.solana.com/{}/install)\"",
                jito_version
            ))?;

            // Create symlink for Jito client
            client.execute_command(&format!(
                "ln -sf /home/$(whoami)/.local/share/solana/install/releases/{}/bin /home/$(whoami)/.local/share/solana/install/active_release/",
                jito_version
            ))?;
        }
        Some("agave") => {
            // Install Agave client
            let _agave_version = if version.contains("agave") {
                version.to_string()
            } else {
                format!("{}-agave", version)
            };

            client.execute_command("curl -sSf https://raw.githubusercontent.com/agave-blockchain/releases/main/install.sh | sh")?;
        }
        Some("firedancer") => {
            // Install Firedancer client
            let temp_dir = "/tmp/firedancer";
            let cleanup = || {
                let _ = client.execute_command(&format!("rm -rf {}", temp_dir));
            };
            
            // Wrap installation steps in error handling
            if let Err(e) = (|| -> Result<(), DeploymentError> {
                // First install dependencies
                client.execute_command("sudo apt-get update && sudo apt-get install -y build-essential cmake pkg-config libssl-dev")?;
                
                // Clone and build Firedancer
                client.execute_command(&format!("git clone https://github.com/firedancer-io/firedancer.git {}", temp_dir))?;
                client.execute_command(&format!("cd {} && make -j", temp_dir))?;
                
                // Install Firedancer binaries
                client.execute_command("sudo mkdir -p /opt/firedancer/bin")?;
                client.execute_command("sudo cp /tmp/firedancer/build/bin/* /opt/firedancer/bin/")?;
                
                // Create symlinks for compatibility
                client.execute_command("sudo ln -sf /opt/firedancer/bin/fdctl /usr/local/bin/solana-validator")?;
                client.execute_command("sudo ln -sf /opt/firedancer/bin/fd_keygen /usr/local/bin/solana-keygen")?;
                
                Ok(())
            })() {
                cleanup();
                return Err(e);
            }
            
            // Clean up
            cleanup();
        }
        Some("sig") => {
            // Install Sig (Solana Zig Validator)
            let result = (|| -> Result<(), DeploymentError> {
                // First install Zig compiler
                client.execute_command("curl -sSf https://ziglang.org/download/master/zig-linux-x86_64-0.14.0-dev.tar.xz | tar xJ -C /tmp")?;
                client.execute_command("sudo mv /tmp/zig-linux-x86_64-0.14.0-dev /opt/zig")?;
                client.execute_command("sudo ln -sf /opt/zig/zig /usr/local/bin/zig")?;
                
                // Clone and build Sig
                client.execute_command("git clone https://github.com/syndica/sig.git /tmp/sig")?;
                client.execute_command("cd /tmp/sig && zig build -Doptimize=ReleaseFast")?;
                
                // Install Sig binaries
                client.execute_command("sudo mkdir -p /opt/sig/bin")?;
                client.execute_command("sudo cp /tmp/sig/zig-out/bin/* /opt/sig/bin/")?;
                
                // Create symlinks for compatibility
                client.execute_command("sudo ln -sf /opt/sig/bin/sig /usr/local/bin/solana-validator")?;
                
                // For keygen, we'll use the standard Solana keygen as Sig is compatible
                client.execute_command(&format!(
                    "sh -c \"$(curl -sSfL https://release.solana.com/{}/install)\"",
                    version
                ))?;
                
                Ok(())
            })();
            
            // Ensure cleanup is performed
            client.execute_command("rm -rf /tmp/sig")?;
            
            // Propagate errors
            result?;
        }
        _ => {
            // Install standard Solana client
            client.execute_command(&format!(
                "sh -c \"$(curl -sSfL https://release.solana.com/{}/install)\"",
                version
            ))?;
        }
    }

    // Add Solana to PATH
    client.execute_command("echo 'export PATH=\"/home/$(whoami)/.local/share/solana/install/active_release/bin:$PATH\"' >> ~/.bashrc")?;
    client.execute_command(
        "export PATH=\"/home/$(whoami)/.local/share/solana/install/active_release/bin:$PATH\"",
    )?;

    // Verify installation
    let solana_version = client.execute_command("solana --version")?;
    println!("Installed Solana version: {}", solana_version);

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
        client.execute_command(&format!(
            "solana-keygen new -o {} --no-passphrase",
            keypair_path
        ))?;
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
    let service_name = format!(
        "solana-{}-{}",
        deployment_config.node_type, deployment_config.network
    );

    // Get service arguments based on node type
    let args = get_solana_service_args(deployment_config, solana_dir, keypair_path);

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
        format!(
            "--entrypoint entrypoint.{}.solana.com:8001",
            deployment_config.network
        ),
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
