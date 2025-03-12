//! Main deployment functions for SSH deployment

use {
    crate::utils::ssh_deploy::{
        client::SshClient,
        dependencies::install_dependencies,
        errors::DeploymentError,
        types::{DeploymentConfig, NetworkType, ServerConfig},
        validators::validate_system_requirements,
    },
    std::error::Error,
};

/// Deploy a node to a remote server
///
/// # Arguments
/// * `host` - Host to deploy to
/// * `svm_name` - SVM name
/// * `node_type` - Node type (validator or RPC)
/// * `network` - Network type
///
/// # Returns
/// * `Result<String, Box<dyn Error>>` - Node ID or error
pub fn deploy_node(
    host: &str,
    svm_name: &str,
    node_type: &str,
    network: NetworkType,
) -> Result<String, Box<dyn Error>> {
    // This is a simplified implementation that returns a node ID
    let node_id = format!("{}-{}-{}-{}", svm_name, node_type, network, host);
    Ok(node_id)
}

/// Deploy a new SVM node to a remote server
///
/// # Arguments
/// * `server_config` - Server configuration
/// * `deployment_config` - Deployment configuration
/// * `progress_callback` - Callback function for progress updates
///
/// # Returns
/// * `Result<(), Box<dyn Error>>` - Success/failure
pub async fn deploy_svm_node(
    server_config: ServerConfig,
    deployment_config: DeploymentConfig,
    progress_callback: Option<crate::prelude::ProgressCallback>,
) -> Result<(), Box<dyn Error>> {
    // Create SSH client and connect
    let mut client = SshClient::new(server_config.clone())?;
    client.connect()?;

    // Send initial progress update
    if let Some(callback) = &progress_callback {
        callback(0, "Connected to server");
    }

    // Get system information
    let system_info = client.get_system_info()?;

    // Send progress update
    if let Some(callback) = &progress_callback {
        callback(5, "Retrieved system information");
    }

    // Check system requirements based on the SVM type and node type
    validate_system_requirements(&system_info, &deployment_config)?;

    // Send progress update
    if let Some(callback) = &progress_callback {
        callback(10, "Validated system requirements");
    }

    // Install dependencies
    install_dependencies(&mut client, &deployment_config)?;

    // Send progress update
    if let Some(callback) = &progress_callback {
        callback(30, "Installed dependencies");
    }

    // Create installation directory
    client.create_directory(&server_config.install_dir)?;

    // Deploy based on SVM type
    match deployment_config.svm_type.as_str() {
        "solana" => {
            crate::utils::ssh_deploy::deployments::solana::deploy_solana(
                &mut client,
                &server_config,
                &deployment_config,
                progress_callback.as_ref(),
            )
            .await?;
        }
        "sonic" => {
            crate::utils::ssh_deploy::deployments::sonic::deploy_sonic(
                &mut client,
                &server_config,
                &deployment_config,
                progress_callback.as_ref(),
            )
            .await?;
        }
        "eclipse" => {
            crate::utils::ssh_deploy::deployments::eclipse::deploy_eclipse(
                &mut client,
                &server_config,
                &deployment_config,
                progress_callback.as_ref(),
            )
            .await?;
        }
        "s00n" => {
            crate::utils::ssh_deploy::deployments::s00n::deploy_s00n(
                &mut client,
                &server_config,
                &deployment_config,
                progress_callback.as_ref(),
            )
            .await?;
        }
        _ => {
            return Err(Box::new(DeploymentError::ValidationError(format!(
                "Unsupported SVM type: {}",
                deployment_config.svm_type
            ))));
        }
    }

    // Send final progress update
    if let Some(callback) = &progress_callback {
        callback(100, "Deployment completed successfully");
    }

    // Close SSH connection
    client.close();

    Ok(())
}
