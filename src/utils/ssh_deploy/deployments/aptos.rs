//! Aptos deployment implementation

use {
    crate::utils::ssh_deploy::{
        client::SshClient,
        errors::DeploymentError,
        types::{ServerConfig, DeploymentConfig, NetworkType},
        services::{create_systemd_service, enable_and_start_service, await_service_startup, create_binary_service_content},
    },
};

/// Deploy Aptos node
///
/// # Arguments
/// * `client` - SSH client
/// * `server_config` - Server configuration
/// * `deployment_config` - Deployment configuration
/// * `progress_callback` - Callback function for progress updates
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub async fn deploy_aptos(
    client: &mut SshClient,
    server_config: &ServerConfig,
    deployment_config: &DeploymentConfig,
    progress_callback: Option<&Box<dyn Fn(u8, &str) + Send>>,
) -> Result<(), DeploymentError> {
    // Clone Aptos repository
    if let Some(callback) = progress_callback {
        callback(40, "Cloning Aptos repository");
    }
    
    let aptos_dir = format!("{}/aptos", server_config.install_dir);
    clone_aptos_repository(client, &aptos_dir)?;
    
    // Build Aptos binary
    if let Some(callback) = progress_callback {
        callback(50, "Building Aptos binary");
    }
    
    build_aptos_binary(client, &aptos_dir)?;
    
    // Create configuration
    if let Some(callback) = progress_callback {
        callback(70, "Creating configuration");
    }
    
    create_aptos_configuration(client, &aptos_dir, deployment_config.network)?;
    
    // Create systemd service
    if let Some(callback) = progress_callback {
        callback(80, "Creating systemd service");
    }
    
    create_aptos_service(client, deployment_config, &aptos_dir).await?;
    
    Ok(())
}

/// Clone the Aptos repository
///
/// # Arguments
/// * `client` - SSH client
/// * `aptos_dir` - Aptos directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn clone_aptos_repository(
    client: &mut SshClient,
    aptos_dir: &str,
) -> Result<(), DeploymentError> {
    if !client.directory_exists(aptos_dir)? {
        client.execute_command(&format!("git clone https://github.com/aptos-labs/aptos-core.git {}", aptos_dir))?;
    }
    
    Ok(())
}

/// Build Aptos binary
///
/// # Arguments
/// * `client` - SSH client
/// * `aptos_dir` - Aptos directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn build_aptos_binary(
    client: &mut SshClient,
    aptos_dir: &str,
) -> Result<(), DeploymentError> {
    client.execute_command(&format!("cd {} && cargo build --release --package aptos-node", aptos_dir))?;
    
    Ok(())
}

/// Create Aptos configuration
///
/// # Arguments
/// * `client` - SSH client
/// * `aptos_dir` - Aptos directory
/// * `network` - Network type
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn create_aptos_configuration(
    client: &mut SshClient,
    aptos_dir: &str,
    network: NetworkType,
) -> Result<(), DeploymentError> {
    let network_flag = match network {
        NetworkType::Mainnet => "mainnet",
        NetworkType::Testnet => "testnet",
        NetworkType::Devnet => "devnet",
    };
    
    let config_dir = format!("{}/config", aptos_dir);
    client.create_directory(&config_dir)?;
    
    client.execute_command(&format!(
        "cd {} && ./target/release/aptos-node -f {}",
        aptos_dir,
        network_flag
    ))?;
    
    Ok(())
}

/// Create Aptos service
///
/// # Arguments
/// * `client` - SSH client
/// * `deployment_config` - Deployment configuration
/// * `aptos_dir` - Aptos directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
async fn create_aptos_service(
    client: &mut SshClient,
    deployment_config: &DeploymentConfig,
    aptos_dir: &str,
) -> Result<(), DeploymentError> {
    let service_name = format!("aptos-{}-{}", deployment_config.node_type, deployment_config.network);
    
    // Create service content
    let args = vec![
        &format!("--config {}/config/node.yaml", aptos_dir),
    ];
    
    let service_content = create_binary_service_content(
        &format!("{}/target/release/aptos-node", aptos_dir),
        &args,
        aptos_dir,
        "Aptos Node",
    );
    
    // Create and start the service
    create_systemd_service(client, &service_name, &service_content)?;
    enable_and_start_service(client, &service_name)?;
    
    // Wait for the service to start
    await_service_startup(client, &service_name).await?;
    
    Ok(())
}