//! s00n SVM deployment implementation

use crate::utils::ssh_deploy::{
    client::SshClient,
    errors::DeploymentError,
    services::{
        await_service_startup, create_binary_service_content, create_systemd_service,
        enable_and_start_service,
    },
    types::{DeploymentConfig, NetworkType, ServerConfig},
};

/// Deploy s00n node
///
/// # Arguments
/// * `client` - SSH client
/// * `server_config` - Server configuration
/// * `deployment_config` - Deployment configuration
/// * `progress_callback` - Callback function for progress updates
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub async fn deploy_s00n(
    client: &mut SshClient,
    server_config: &ServerConfig,
    deployment_config: &DeploymentConfig,
    progress_callback: Option<&Box<dyn Fn(u8, &str) + Send>>,
) -> Result<(), DeploymentError> {
    // Clone s00n repository
    if let Some(callback) = progress_callback {
        callback(40, "Cloning s00n repository");
    }

    let s00n_dir = format!("{}/s00n", server_config.install_dir);
    clone_s00n_repository(client, &s00n_dir)?;

    // Build s00n binary
    if let Some(callback) = progress_callback {
        callback(50, "Building s00n binary");
    }

    build_s00n_binary(client, &s00n_dir)?;

    // Create configuration
    if let Some(callback) = progress_callback {
        callback(70, "Creating configuration");
    }

    create_s00n_configuration(client, &s00n_dir, deployment_config.network)?;

    // Create systemd service
    if let Some(callback) = progress_callback {
        callback(80, "Creating systemd service");
    }

    create_s00n_service(client, deployment_config, &s00n_dir).await?;

    Ok(())
}

/// Clone the s00n repository
///
/// # Arguments
/// * `client` - SSH client
/// * `s00n_dir` - s00n directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn clone_s00n_repository(client: &mut SshClient, s00n_dir: &str) -> Result<(), DeploymentError> {
    if !client.directory_exists(s00n_dir)? {
        client.execute_command(&format!(
            "git clone https://github.com/s00n-protocol/s00n-svm.git {}",
            s00n_dir
        ))?;
    }

    Ok(())
}

/// Build s00n binary
///
/// # Arguments
/// * `client` - SSH client
/// * `s00n_dir` - s00n directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn build_s00n_binary(client: &mut SshClient, s00n_dir: &str) -> Result<(), DeploymentError> {
    client.execute_command(&format!("cd {} && cargo build --release", s00n_dir))?;

    Ok(())
}

/// Create s00n configuration
///
/// # Arguments
/// * `client` - SSH client
/// * `s00n_dir` - s00n directory
/// * `network` - Network type
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn create_s00n_configuration(
    client: &mut SshClient,
    s00n_dir: &str,
    network: NetworkType,
) -> Result<(), DeploymentError> {
    let config_dir = format!("{}/config", s00n_dir);
    client.create_directory(&config_dir)?;

    let network_name = match network {
        NetworkType::Mainnet => "mainnet",
        NetworkType::Testnet => "testnet",
        NetworkType::Devnet => "devnet",
    };

    client.execute_command(&format!(
        "cd {} && ./target/release/s00n-node setup --network {}",
        s00n_dir, network_name
    ))?;

    Ok(())
}

/// Create s00n service
///
/// # Arguments
/// * `client` - SSH client
/// * `deployment_config` - Deployment configuration
/// * `s00n_dir` - s00n directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
async fn create_s00n_service(
    client: &mut SshClient,
    deployment_config: &DeploymentConfig,
    s00n_dir: &str,
) -> Result<(), DeploymentError> {
    let service_name = format!(
        "s00n-{}-{}",
        deployment_config.node_type, deployment_config.network
    );

    // Get service arguments
    let args = get_s00n_service_args(deployment_config, s00n_dir);

    // Create service content
    let args_ref: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let service_content = create_binary_service_content(
        &format!("{}/target/release/s00n-node", s00n_dir),
        &args_ref,
        s00n_dir,
        "s00n SVM Node",
    );

    // Create and start the service
    create_systemd_service(client, &service_name, &service_content)?;
    enable_and_start_service(client, &service_name)?;

    // Wait for the service to start
    await_service_startup(client, &service_name).await?;

    Ok(())
}

/// Get s00n service arguments
///
/// # Arguments
/// * `deployment_config` - Deployment configuration
/// * `s00n_dir` - s00n directory
///
/// # Returns
/// * `Vec<String>` - Service arguments
fn get_s00n_service_args(deployment_config: &DeploymentConfig, s00n_dir: &str) -> Vec<String> {
    let mut args = vec![
        "start".to_string(),
        format!("--config-path {}/config/node.yaml", s00n_dir),
        format!("--network {}", deployment_config.network),
    ];

    if deployment_config.node_type == "validator" {
        args.push("--validator".to_string());
        args.push(format!("--stake-key {}/config/stake-key.json", s00n_dir));
    } else if deployment_config.node_type == "rpc" {
        args.push("--rpc-port 8899".to_string());
        args.push("--public-rpc".to_string());
    }

    args
}
