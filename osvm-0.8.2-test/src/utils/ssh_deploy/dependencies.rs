use crate::utils::ssh_deploy::{
    client::SshClient, errors::DeploymentError, types::DeploymentConfig,
};

/// Install dependencies on the remote server
///
/// # Arguments
/// * `client` - SSH client
/// * `deployment_config` - Deployment configuration
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn install_dependencies(
    client: &mut SshClient,
    deployment_config: &DeploymentConfig,
) -> Result<(), DeploymentError> {
    // Update package list
    client.execute_command("sudo apt-get update")?;

    // Install common dependencies
    install_common_dependencies(client)?;

    // Install SVM-specific dependencies
    install_svm_specific_dependencies(client, deployment_config)?;

    // Install Rust if needed
    install_rust_if_needed(client)?;

    Ok(())
}

/// Install common dependencies
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn install_common_dependencies(client: &mut SshClient) -> Result<(), DeploymentError> {
    client.execute_command(
        "sudo apt-get install -y build-essential libssl-dev pkg-config curl git",
    )?;
    Ok(())
}

/// Install SVM-specific dependencies
///
/// # Arguments
/// * `client` - SSH client
/// * `deployment_config` - Deployment configuration
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn install_svm_specific_dependencies(
    client: &mut SshClient,
    deployment_config: &DeploymentConfig,
) -> Result<(), DeploymentError> {
    match deployment_config.svm_type.as_str() {
        "solana" => {
            if !client.is_package_installed("libudev-dev")? {
                client.execute_command("sudo apt-get install -y libudev-dev")?;
            }
        }
        "sonic" => {
            if !client.is_package_installed("libclang-dev")? {
                client.execute_command("sudo apt-get install -y libclang-dev")?;
            }
        }
        "sui" => {
            if !client.is_package_installed("cmake")? {
                client.execute_command("sudo apt-get install -y cmake")?;
            }
        }
        "aptos" => {
            if !client.is_package_installed("libncursesw5")? {
                client.execute_command("sudo apt-get install -y libncursesw5")?;
            }
        }
        _ => {
            return Err(DeploymentError::ValidationError(format!(
                "Unsupported SVM type: {}",
                deployment_config.svm_type
            )));
        }
    }

    Ok(())
}

/// Install Rust if needed
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn install_rust_if_needed(client: &mut SshClient) -> Result<(), DeploymentError> {
    if !client.is_package_installed("rustc")? {
        client.execute_command(
            "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y",
        )?;
        client.execute_command("source $HOME/.cargo/env")?;
    }

    Ok(())
}

/// Install Docker and Docker Compose if needed
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn install_docker_if_needed(client: &mut SshClient) -> Result<(), DeploymentError> {
    // Install Docker if not already installed
    if !client.is_package_installed("docker-ce")? {
        client.execute_command("curl -fsSL https://get.docker.com -o get-docker.sh")?;
        client.execute_command("sudo sh get-docker.sh")?;
        client.execute_command("sudo usermod -aG docker $(whoami)")?;
    }

    // Install Docker Compose if not already installed
    if !client.is_package_installed("docker-compose")? {
        client.execute_command(
            "sudo curl -L \"https://github.com/docker/compose/releases/download/1.29.2/docker-compose-$(uname -s)-$(uname -m)\" -o /usr/local/bin/docker-compose"
        )?;
        client.execute_command("sudo chmod +x /usr/local/bin/docker-compose")?;
    }

    Ok(())
}

/// Install Node.js if needed
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn install_nodejs_if_needed(client: &mut SshClient) -> Result<(), DeploymentError> {
    if !client.is_package_installed("nodejs")? {
        client
            .execute_command("curl -fsSL https://deb.nodesource.com/setup_16.x | sudo -E bash -")?;
        client.execute_command("sudo apt-get install -y nodejs")?;
    }

    Ok(())
}
