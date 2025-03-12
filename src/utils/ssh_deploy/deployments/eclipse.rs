//! Eclipse SVM deployment implementation

use crate::utils::ssh_deploy::{
    client::SshClient,
    errors::DeploymentError,
    services::{
        await_service_startup, create_binary_service_content, create_systemd_service,
        enable_and_start_service,
    },
    types::{DeploymentConfig, NetworkType, ServerConfig},
};

/// Deploy Eclipse node
///
/// # Arguments
/// * `client` - SSH client
/// * `server_config` - Server configuration
/// * `deployment_config` - Deployment configuration
/// * `progress_callback` - Callback function for progress updates
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub async fn deploy_eclipse(
    client: &mut SshClient,
    server_config: &ServerConfig,
    deployment_config: &DeploymentConfig,
    progress_callback: Option<&Box<dyn Fn(u8, &str) + Send>>,
) -> Result<(), DeploymentError> {
    // Clone Eclipse repository
    if let Some(callback) = progress_callback {
        callback(40, "Cloning Eclipse repository");
    }

    let eclipse_dir = format!("{}/eclipse", server_config.install_dir);
    clone_eclipse_repository(client, &eclipse_dir)?;

    // Build Eclipse binary
    if let Some(callback) = progress_callback {
        callback(50, "Building Eclipse binary");
    }

    build_eclipse_binary(client, &eclipse_dir)?;

    // Create configuration
    if let Some(callback) = progress_callback {
        callback(70, "Creating configuration");
    }

    create_eclipse_configuration(client, &eclipse_dir, deployment_config.network)?;

    // Create systemd service
    if let Some(callback) = progress_callback {
        callback(80, "Creating systemd service");
    }

    create_eclipse_service(client, deployment_config, &eclipse_dir).await?;

    Ok(())
}

/// Clone the Eclipse repository
///
/// # Arguments
/// * `client` - SSH client
/// * `eclipse_dir` - Eclipse directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn clone_eclipse_repository(
    client: &mut SshClient,
    eclipse_dir: &str,
) -> Result<(), DeploymentError> {
    if !client.directory_exists(eclipse_dir)? {
        client.execute_command(&format!(
            "git clone https://github.com/Eclipse-Laboratories-Inc/eclipse.git {}",
            eclipse_dir
        ))?;
    }

    Ok(())
}

/// Build Eclipse binary
///
/// # Arguments
/// * `client` - SSH client
/// * `eclipse_dir` - Eclipse directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn build_eclipse_binary(client: &mut SshClient, eclipse_dir: &str) -> Result<(), DeploymentError> {
    client.execute_command(&format!("cd {} && cargo build --release", eclipse_dir))?;

    Ok(())
}

/// Create Eclipse configuration
///
/// # Arguments
/// * `client` - SSH client
/// * `eclipse_dir` - Eclipse directory
/// * `network` - Network type
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn create_eclipse_configuration(
    client: &mut SshClient,
    eclipse_dir: &str,
    network: NetworkType,
) -> Result<(), DeploymentError> {
    let config_dir = format!("{}/config", eclipse_dir);
    client.create_directory(&config_dir)?;

    let network_name = match network {
        NetworkType::Mainnet => "mainnet",
        NetworkType::Testnet => "testnet",
        NetworkType::Devnet => "devnet",
    };

    client.execute_command(&format!(
        "cd {} && ./target/release/eclipse-node init --network {}",
        eclipse_dir, network_name
    ))?;

    Ok(())
}

/// Create Eclipse service
///
/// # Arguments
/// * `client` - SSH client
/// * `deployment_config` - Deployment configuration
/// * `eclipse_dir` - Eclipse directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
async fn create_eclipse_service(
    client: &mut SshClient,
    deployment_config: &DeploymentConfig,
    eclipse_dir: &str,
) -> Result<(), DeploymentError> {
    let service_name = format!(
        "eclipse-{}-{}",
        deployment_config.node_type, deployment_config.network
    );

    // Get service arguments
    let args = get_eclipse_service_args(deployment_config, eclipse_dir);

    // Create service content
    let args_ref: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let service_content = create_binary_service_content(
        &format!("{}/target/release/eclipse-node", eclipse_dir),
        &args_ref,
        eclipse_dir,
        "Eclipse SVM Node",
    );

    // Create and start the service
    create_systemd_service(client, &service_name, &service_content)?;
    enable_and_start_service(client, &service_name)?;

    // Wait for the service to start
    await_service_startup(client, &service_name).await?;

    Ok(())
}

/// Get Eclipse service arguments
///
/// # Arguments
/// * `deployment_config` - Deployment configuration
/// * `eclipse_dir` - Eclipse directory
///
/// # Returns
/// * `Vec<&str>` - Service arguments
fn get_eclipse_service_args(
    deployment_config: &DeploymentConfig,
    eclipse_dir: &str,
) -> Vec<String> {
    let mut args = vec![
        "run".to_string(),
        format!("--config {}/config/node-config.yaml", eclipse_dir),
        format!("--network {}", deployment_config.network),
    ];

    if deployment_config.node_type == "validator" {
        args.push("--validator".to_string());
    } else if deployment_config.node_type == "rpc" {
        args.push("--rpc-port 8899".to_string());
    }

    args
}
