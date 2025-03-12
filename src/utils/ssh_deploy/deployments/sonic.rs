//! Sonic deployment implementation

use crate::utils::ssh_deploy::{
    client::SshClient,
    dependencies::{install_docker_if_needed, install_nodejs_if_needed},
    errors::DeploymentError,
    services::{
        await_service_startup, create_docker_service_content, create_systemd_service,
        enable_service,
    },
    types::{DeploymentConfig, NetworkType, ServerConfig},
};

/// Deploy Sonic node
///
/// # Arguments
/// * `client` - SSH client
/// * `server_config` - Server configuration
/// * `deployment_config` - Deployment configuration
/// * `progress_callback` - Callback function for progress updates
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub async fn deploy_sonic(
    client: &mut SshClient,
    server_config: &ServerConfig,
    deployment_config: &DeploymentConfig,
    progress_callback: Option<&crate::prelude::ProgressCallback>,
) -> Result<(), DeploymentError> {
    // Clone Sonic RPC repository
    if let Some(callback) = progress_callback {
        callback(40, "Cloning Sonic RPC repository");
    }

    let sonic_dir = format!("{}/sonic-rpc", server_config.install_dir);
    clone_sonic_repository(client, &sonic_dir)?;

    // Install dependencies
    if let Some(callback) = progress_callback {
        callback(50, "Installing dependencies");
    }

    install_sonic_dependencies(client)?;

    // Run the setup script
    if let Some(callback) = progress_callback {
        callback(70, "Running setup script");
    }

    // Navigate to the repository directory and run the setup script
    client.execute_command(&format!("cd {} && bash setup.sh", sonic_dir))?;

    // Configure network settings
    if let Some(callback) = progress_callback {
        callback(80, "Configuring network settings");
    }

    configure_sonic_network(client, &sonic_dir, deployment_config.network)?;

    // Start the RPC node
    if let Some(callback) = progress_callback {
        callback(90, "Starting Sonic RPC node");
    }

    start_sonic_node(client, &sonic_dir, deployment_config).await?;

    Ok(())
}

/// Clone the Sonic repository
///
/// # Arguments
/// * `client` - SSH client
/// * `sonic_dir` - Sonic directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn clone_sonic_repository(client: &mut SshClient, sonic_dir: &str) -> Result<(), DeploymentError> {
    if !client.directory_exists(sonic_dir)? {
        client.execute_command(&format!(
            "git clone https://github.com/sonicfromnewyoke/solana-rpc.git {}",
            sonic_dir
        ))?;
    }

    Ok(())
}

/// Install Sonic dependencies
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn install_sonic_dependencies(client: &mut SshClient) -> Result<(), DeploymentError> {
    // Install required packages
    client.execute_command("sudo apt-get update")?;
    client.execute_command(
        "sudo apt-get install -y build-essential libssl-dev pkg-config curl git jq",
    )?;

    // Install Node.js if not already installed
    install_nodejs_if_needed(client)?;

    // Install Docker and Docker Compose if not already installed
    install_docker_if_needed(client)?;

    Ok(())
}

/// Configure Sonic network settings
///
/// # Arguments
/// * `client` - SSH client
/// * `sonic_dir` - Sonic directory
/// * `network` - Network type
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn configure_sonic_network(
    client: &mut SshClient,
    sonic_dir: &str,
    network: NetworkType,
) -> Result<(), DeploymentError> {
    // Set the network configuration based on the deployment config
    let network_config = match network {
        NetworkType::Mainnet => "mainnet",
        NetworkType::Testnet => "testnet",
        NetworkType::Devnet => "devnet",
    };

    // Update the configuration file with the selected network
    client.execute_command(&format!(
        "cd {} && echo 'SOLANA_NETWORK={}' > .env",
        sonic_dir, network_config
    ))?;

    Ok(())
}

/// Start the Sonic node
///
/// # Arguments
/// * `client` - SSH client
/// * `sonic_dir` - Sonic directory
/// * `deployment_config` - Deployment configuration
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
async fn start_sonic_node(
    client: &mut SshClient,
    sonic_dir: &str,
    deployment_config: &DeploymentConfig,
) -> Result<(), DeploymentError> {
    // Start the services using docker-compose
    client.execute_command(&format!("cd {} && docker-compose up -d", sonic_dir))?;

    // Create a systemd service to ensure the RPC node starts on boot
    let service_name = format!("sonic-rpc-{}", deployment_config.network);
    let service_content = create_docker_service_content(&service_name, sonic_dir, "Sonic RPC Node");

    // Create and enable the service
    create_systemd_service(client, &service_name, &service_content)?;
    enable_service(client, &service_name)?;

    // Wait for the service to start
    await_service_startup(client, &service_name).await?;

    Ok(())
}