//! Sui deployment implementation

use {
    crate::utils::ssh_deploy::{
        client::SshClient,
        errors::DeploymentError,
        types::{ServerConfig, DeploymentConfig, NetworkType},
        services::{create_systemd_service, enable_and_start_service, await_service_startup, create_binary_service_content},
    },
};

/// Deploy Sui node
///
/// # Arguments
/// * `client` - SSH client
/// * `server_config` - Server configuration
/// * `deployment_config` - Deployment configuration
/// * `progress_callback` - Callback function for progress updates
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub async fn deploy_sui(
    client: &mut SshClient,
    server_config: &ServerConfig,
    deployment_config: &DeploymentConfig,
    progress_callback: Option<&Box<dyn Fn(u8, &str) + Send>>,
) -> Result<(), DeploymentError> {
    // Clone Sui repository
    if let Some(callback) = progress_callback {
        callback(40, "Cloning Sui repository");
    }
    
    let sui_dir = format!("{}/sui", server_config.install_dir);
    clone_sui_repository(client, &sui_dir)?;
    
    // Build Sui binary
    if let Some(callback) = progress_callback {
        callback(50, "Building Sui binary");
    }
    
    build_sui_binary(client, &sui_dir)?;
    
    // Create configuration
    if let Some(callback) = progress_callback {
        callback(70, "Creating configuration");
    }
    
    create_sui_configuration(client, &sui_dir, deployment_config.network)?;
    
    // Create systemd service
    if let Some(callback) = progress_callback {
        callback(80, "Creating systemd service");
    }
    
    create_sui_service(client, deployment_config, &sui_dir).await?;
    
    Ok(())
}

/// Clone the Sui repository
///
/// # Arguments
/// * `client` - SSH client
/// * `sui_dir` - Sui directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn clone_sui_repository(
    client: &mut SshClient,
    sui_dir: &str,
) -> Result<(), DeploymentError> {
    if !client.directory_exists(sui_dir)? {
        client.execute_command(&format!("git clone https://github.com/MystenLabs/sui.git {}", sui_dir))?;
    }
    
    Ok(())
}

/// Build Sui binary
///
/// # Arguments
/// * `client` - SSH client
/// * `sui_dir` - Sui directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn build_sui_binary(
    client: &mut SshClient,
    sui_dir: &str,
) -> Result<(), DeploymentError> {
    client.execute_command(&format!("cd {} && cargo build --release --bin sui-node", sui_dir))?;
    
    Ok(())
}

/// Create Sui configuration
///
/// # Arguments
/// * `client` - SSH client
/// * `sui_dir` - Sui directory
/// * `network` - Network type
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn create_sui_configuration(
    client: &mut SshClient,
    sui_dir: &str,
    network: NetworkType,
) -> Result<(), DeploymentError> {
    let network_flag = match network {
        NetworkType::Mainnet => "mainnet",
        NetworkType::Testnet => "testnet",
        NetworkType::Devnet => "devnet",
    };
    
    client.execute_command(&format!(
        "cd {} && ./target/release/sui genesis --force --{}", 
        sui_dir,
        network_flag
    ))?;
    
    Ok(())
}

/// Create Sui service
///
/// # Arguments
/// * `client` - SSH client
/// * `deployment_config` - Deployment configuration
/// * `sui_dir` - Sui directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
async fn create_sui_service(
    client: &mut SshClient,
    deployment_config: &DeploymentConfig,
    sui_dir: &str,
) -> Result<(), DeploymentError> {
    let service_name = format!("sui-{}-{}", deployment_config.node_type, deployment_config.network);
    
    // Create service content
    let args = vec![
        &format!("--config-path {}/sui_config/fullnode.yaml", sui_dir),
    ];
    
    let service_content = create_binary_service_content(
        &format!("{}/target/release/sui-node", sui_dir),
        &args,
        sui_dir,
        "Sui Node",
    );
    
    // Create and start the service
    create_systemd_service(client, &service_name, &service_content)?;
    enable_and_start_service(client, &service_name)?;
    
    // Wait for the service to start
    await_service_startup(client, &service_name).await?;
    
    Ok(())
}