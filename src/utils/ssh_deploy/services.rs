//! Service management utilities for SSH deployment

use {
    crate::utils::ssh_deploy::{
        errors::DeploymentError,
        client::SshClient,
    },
    std::{
        fs,
        io::Write,
        path::Path,
        time::Duration,
    },
    tokio::time,
};

/// Create and install a systemd service
///
/// # Arguments
/// * `client` - SSH client
/// * `service_name` - Service name
/// * `service_content` - Service content
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn create_systemd_service(
    client: &mut SshClient,
    service_name: &str,
    service_content: &str,
) -> Result<(), DeploymentError> {
    // Create a temporary file with service content
    let temp_path = std::env::temp_dir().join(format!("{}.service", service_name));
    let mut temp_file = fs::File::create(&temp_path)?;
    temp_file.write_all(service_content.as_bytes())?;
    
    // Upload and install the service
    client.upload_file(&temp_path, &format!("/tmp/{}.service", service_name))?;
    client.execute_command(&format!("sudo mv /tmp/{}.service /etc/systemd/system/{}.service", service_name, service_name))?;
    client.execute_command("sudo systemctl daemon-reload")?;
    
    // Delete the temporary file
    fs::remove_file(temp_path)?;
    
    Ok(())
}

/// Enable and start a systemd service
///
/// # Arguments
/// * `client` - SSH client
/// * `service_name` - Service name
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn enable_and_start_service(
    client: &mut SshClient,
    service_name: &str,
) -> Result<(), DeploymentError> {
    client.execute_command(&format!("sudo systemctl enable {}", service_name))?;
    client.execute_command(&format!("sudo systemctl start {}", service_name))?;
    
    Ok(())
}

/// Enable a systemd service without starting it
///
/// # Arguments
/// * `client` - SSH client
/// * `service_name` - Service name
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn enable_service(
    client: &mut SshClient,
    service_name: &str,
) -> Result<(), DeploymentError> {
    client.execute_command(&format!("sudo systemctl enable {}", service_name))?;
    
    Ok(())
}

/// Wait for a service to start
///
/// # Arguments
/// * `client` - SSH client
/// * `service_name` - Service name
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub async fn await_service_startup(
    client: &mut SshClient,
    service_name: &str,
) -> Result<(), DeploymentError> {
    for _ in 0..30 {
        let status = client.execute_command(&format!("systemctl is-active {}", service_name))?;
        if status.trim() == "active" {
            return Ok(());
        }
        time::sleep(Duration::from_secs(2)).await;
    }
    
    Err(DeploymentError::DeploymentError(format!(
        "Service did not start within the expected time: {}", service_name
    )))
}

/// Create a service content for a Docker-based service
///
/// # Arguments
/// * `service_name` - Service name
/// * `working_dir` - Working directory
/// * `description` - Service description
///
/// # Returns
/// * `String` - Service content
pub fn create_docker_service_content(
    service_name: &str,
    working_dir: &str,
    description: &str,
) -> String {
    format!(
        "[Unit]\n\
        Description={}\n\
        After=docker.service\n\
        Requires=docker.service\n\
        \n\
        [Service]\n\
        User=$(whoami)\n\
        WorkingDirectory={}\n\
        ExecStart=/usr/local/bin/docker-compose up\n\
        ExecStop=/usr/local/bin/docker-compose down\n\
        Restart=always\n\
        RestartSec=10\n\
        \n\
        [Install]\n\
        WantedBy=multi-user.target\n",
        description,
        working_dir
    )
}

/// Create a service content for a binary-based service
///
/// # Arguments
/// * `binary_path` - Binary path
/// * `args` - Command-line arguments
/// * `working_dir` - Working directory
/// * `description` - Service description
///
/// # Returns
/// * `String` - Service content
pub fn create_binary_service_content(
    binary_path: &str,
    args: &[&str],
    working_dir: &str,
    description: &str,
) -> String {
    let args_str = args.join(" \\\n  ");
    
    format!(
        "[Unit]\n\
        Description={}\n\
        After=network.target\n\
        \n\
        [Service]\n\
        User=$(whoami)\n\
        WorkingDirectory={}\n\
        ExecStart={} \\\n\
          {}\n\
        Restart=always\n\
        RestartSec=1\n\
        \n\
        [Install]\n\
        WantedBy=multi-user.target\n",
        description,
        working_dir,
        binary_path,
        args_str
    )
}