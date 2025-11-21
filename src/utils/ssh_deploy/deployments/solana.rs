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
                "ln -sf ~/.local/share/solana/install/releases/{}/bin ~/.local/share/solana/install/active_release/",
                jito_version
            ))?;
        }
        Some("agave") => {
            // Install latest Agave client from Anza (official Agave releases)
            let agave_version = version;

            // Download and install Agave binaries directly
            client.execute_command(&format!(
                "curl -sSfL https://github.com/anza-xyz/agave/releases/download/{}/agave-release-x86_64-unknown-linux-gnu.tar.bz2 | tar xjf - -C /tmp",
                agave_version
            ))?;

            // Install binaries
            client.execute_command("sudo mkdir -p /opt/agave/bin")?;
            client.execute_command("sudo cp /tmp/agave-release/bin/* /opt/agave/bin/")?;

            // Create symlinks for compatibility
            client.execute_command("sudo ln -sf /opt/agave/bin/agave-validator /usr/local/bin/agave-validator")?;
            client.execute_command("sudo ln -sf /opt/agave/bin/solana-keygen /usr/local/bin/solana-keygen")?;
            client.execute_command("sudo ln -sf /opt/agave/bin/solana /usr/local/bin/solana")?;

            // Also update the Solana install location to point to Agave
            client.execute_command("mkdir -p ~/.local/share/solana/install/active_release")?;
            client.execute_command("ln -sf /opt/agave/bin ~/.local/share/solana/install/active_release/")?;

            // Cleanup
            client.execute_command("rm -rf /tmp/agave-release")?;
        }
        Some("firedancer") => {
            // Install Firedancer client
            let temp_dir = "/tmp/firedancer";

            // Wrap installation steps in error handling
            let result = (|| -> Result<(), DeploymentError> {
                // First install dependencies
                client.execute_command("sudo apt-get update && sudo apt-get install -y build-essential cmake pkg-config libssl-dev")?;

                // Clone and build Firedancer
                client.execute_command(&format!(
                    "git clone https://github.com/firedancer-io/firedancer.git {}",
                    temp_dir
                ))?;
                client.execute_command(&format!("cd {} && make -j", temp_dir))?;

                // Install Firedancer binaries
                client.execute_command("sudo mkdir -p /opt/firedancer/bin")?;
                client
                    .execute_command("sudo cp /tmp/firedancer/build/bin/* /opt/firedancer/bin/")?;

                // Create symlinks for compatibility
                client.execute_command(
                    "sudo ln -sf /opt/firedancer/bin/fdctl /usr/local/bin/solana-validator",
                )?;
                client.execute_command(
                    "sudo ln -sf /opt/firedancer/bin/fd_keygen /usr/local/bin/solana-keygen",
                )?;

                Ok(())
            })();

            // Clean up regardless of success or failure
            let _ = client.execute_command(&format!("rm -rf {}", temp_dir));

            // Return result after cleanup
            result?;
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
                client.execute_command(
                    "sudo ln -sf /opt/sig/bin/sig /usr/local/bin/solana-validator",
                )?;

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
    client.execute_command("echo 'export PATH=\"~/.local/share/solana/install/active_release/bin:$PATH\"' >> ~/.bashrc")?;
    client.execute_command(
        "export PATH=\"~/.local/share/solana/install/active_release/bin:$PATH\"",
    )?;

    // Verify installation using full path (PATH may not be updated yet)
    let solana_bin = "~/.local/share/solana/install/active_release/bin/solana";
    let solana_version = client.execute_command(&format!("{} --version", solana_bin))?;
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
        let keygen_bin = "~/.local/share/solana/install/active_release/bin/solana-keygen";
        client.execute_command(&format!(
            "{} new -o {} --no-passphrase",
            keygen_bin, keypair_path
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

    let solana_bin = "~/.local/share/solana/install/active_release/bin/solana";
    client.execute_command(&format!("{} config set {}", solana_bin, network_flag))?;

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

    // Determine home directory for the validator binary path (systemd needs absolute paths)
    let home_output = client.execute_command("echo $HOME")?;
    let home_dir = home_output.trim();
    // Modern Solana installations use agave-validator, not solana-validator
    let validator_bin = format!("{}/.local/share/solana/install/active_release/bin/agave-validator", home_dir);

    // Create service content
    let args_ref: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let service_content = create_binary_service_content(
        &validator_bin,
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
    // Get network-specific configuration
    let (entrypoint, genesis_hash) = match deployment_config.network {
        NetworkType::Mainnet => (
            "entrypoint.mainnet-beta.solana.com:8001",
            "5eykt4UsFv8P8NJdTREpY1vzqKqZKvdpKuc147dw2N9d", // Mainnet genesis
        ),
        NetworkType::Devnet => (
            "entrypoint.devnet.solana.com:8001",
            "EtWTRABZaYq6iMfeYKouRu166VU2xqa1wcaWoxPkrZBG", // Devnet genesis
        ),
        NetworkType::Testnet => (
            "entrypoint.testnet.solana.com:8001",
            "4uhcVJyU9pJkvQyS88uRDiswHXSCkY3zQawwpjk2NsNY", // Testnet genesis
        ),
    };

    let mut args = vec![
        format!("--identity {}", keypair_path),
        format!("--ledger {}/ledger", solana_dir),
        format!("--accounts {}/accounts", solana_dir),
        "--rpc-port 8899".to_string(),
        "--dynamic-port-range 8000-8020".to_string(),
        format!("--entrypoint {}", entrypoint),
        format!("--expected-genesis-hash {}", genesis_hash),
        "--wal-recovery-mode skip_any_corrupted_record".to_string(),
        "--limit-ledger-size 50000000".to_string(), // 50GB limit
        // Add known validators for snapshot downloads
        // Validator will automatically fetch snapshots from these via gossip
        "--known-validator 7Np41oeYqPefeNQEHSv1UDhYrehxin3NStELsSKCT4K2".to_string(),
        "--known-validator GdnSyH3YtwcxFvQrVVJMm1JhTS4QVX7MFsX56uJLUfiZ".to_string(),
        "--known-validator DE1bawNcRJB9rVm3buyMVfr8mBEoyyu73NBovf2oXJsJ".to_string(),
        "--known-validator CakcnaRDHka2gXyfbEd2d3xsvkJkqsLw2akB3zsN1D2S".to_string(),
        // RPC-specific flags
        "--no-port-check".to_string(), // Skip port checks
        "--no-wait-for-vote-to-start-leader".to_string(), // RPC doesn't vote
        // Increase retry attempts for snapshot download
        "--maximum-snapshot-download-abort".to_string(),
        "10".to_string(), // Retry up to 10 times
    ];

    if deployment_config.node_type == "rpc" {
        args.push("--no-voting".to_string());
        args.push("--enable-rpc-transaction-history".to_string());
        args.push("--full-rpc-api".to_string());
        args.push("--rpc-bind-address".to_string());
        args.push("0.0.0.0".to_string());
    }

    args
}
