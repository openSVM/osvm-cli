//! End-to-end tests for SSH deployment
//! These tests use Docker to create a temporary environment for testing SSH deployment

#[cfg(test)]
mod tests {
    use {
        crate::utils::ssh_deploy::{
            client::SshClient,
            deploy::deploy_svm_node,
            types::{AuthMethod, DeploymentConfig, NetworkType, ServerConfig},
        },
        std::collections::HashMap,
        std::process::Command,
        std::time::Duration,
        tokio::time,
    };

    /// Setup a Docker container for testing
    ///
    /// # Returns
    /// * `Result<String, Box<dyn std::error::Error>>` - Container ID
    async fn setup_docker_container() -> Result<String, Box<dyn std::error::Error>> {
        // Pull Ubuntu image
        let status = Command::new("docker")
            .args(["pull", "ubuntu:20.04"])
            .status()?;

        if !status.success() {
            return Err("Failed to pull Docker image".into());
        }

        // Start container with SSH
        let output = Command::new("docker")
            .args([
                "run", 
                "-d", 
                "--privileged",
                "-p", "2222:22", 
                "ubuntu:20.04", 
                "/bin/bash", 
                "-c", 
                "apt-get update && apt-get install -y openssh-server && \
                 mkdir -p /run/sshd && \
                 echo 'root:password' | chpasswd && \
                 sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config && \
                 sed -i 's/#PasswordAuthentication yes/PasswordAuthentication yes/' /etc/ssh/sshd_config && \
                 service ssh start && \
                 tail -f /dev/null"
            ])
            .output()?;

        if !output.status.success() {
            return Err(format!(
                "Failed to start Docker container: {}",
                String::from_utf8_lossy(&output.stderr)
            )
            .into());
        }

        let container_id = String::from_utf8_lossy(&output.stdout).trim().to_string();

        // Wait for SSH to be ready
        time::sleep(Duration::from_secs(5)).await;

        Ok(container_id)
    }

    /// Cleanup Docker container
    ///
    /// # Arguments
    /// * `container_id` - Container ID
    ///
    /// # Returns
    /// * `Result<(), Box<dyn std::error::Error>>` - Success/failure
    async fn cleanup_docker_container(
        container_id: &str,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let status = Command::new("docker")
            .args(["stop", container_id])
            .status()?;

        if !status.success() {
            return Err("Failed to stop Docker container".into());
        }

        let status = Command::new("docker").args(["rm", container_id]).status()?;

        if !status.success() {
            return Err("Failed to remove Docker container".into());
        }

        Ok(())
    }

    /// Test SSH connection to Docker container
    ///
    /// # Returns
    /// * `Result<(), Box<dyn std::error::Error>>` - Success/failure
    #[tokio::test]
    #[ignore] // Ignore by default as it requires Docker
    async fn test_ssh_connection() -> Result<(), Box<dyn std::error::Error>> {
        // Setup Docker container
        let container_id = setup_docker_container().await?;

        // Create server config
        let server_config = ServerConfig {
            host: "localhost".to_string(),
            port: 2222,
            auth: AuthMethod::Password {
                username: "root".to_string(),
                password: "password".to_string(),
            },
            install_dir: "/opt/osvm".to_string(),
        };

        // Create SSH client
        let mut client = SshClient::new(server_config.clone())?;

        // Connect to server
        client.connect()?;

        // Execute command
        let output = client.execute_command("echo 'Hello, world!'")?;
        assert_eq!(output.trim(), "Hello, world!");

        // Close connection
        client.close();

        // Cleanup Docker container
        cleanup_docker_container(&container_id).await?;

        Ok(())
    }

    /// Test system information retrieval
    ///
    /// # Returns
    /// * `Result<(), Box<dyn std::error::Error>>` - Success/failure
    #[tokio::test]
    #[ignore] // Ignore by default as it requires Docker
    async fn test_system_info() -> Result<(), Box<dyn std::error::Error>> {
        // Setup Docker container
        let container_id = setup_docker_container().await?;

        // Create server config
        let server_config = ServerConfig {
            host: "localhost".to_string(),
            port: 2222,
            auth: AuthMethod::Password {
                username: "root".to_string(),
                password: "password".to_string(),
            },
            install_dir: "/opt/osvm".to_string(),
        };

        // Create SSH client
        let mut client = SshClient::new(server_config.clone())?;

        // Connect to server
        client.connect()?;

        // Get system information
        let system_info = client.get_system_info()?;

        // Verify system information
        assert!(system_info.contains_key("cpu_cores"));
        assert!(system_info.contains_key("memory_gb"));
        assert!(system_info.contains_key("disk_total"));
        assert!(system_info.contains_key("disk_available"));
        assert!(system_info.contains_key("kernel"));

        // Close connection
        client.close();

        // Cleanup Docker container
        cleanup_docker_container(&container_id).await?;

        Ok(())
    }

    /// Test file operations
    ///
    /// # Returns
    /// * `Result<(), Box<dyn std::error::Error>>` - Success/failure
    #[tokio::test]
    #[ignore] // Ignore by default as it requires Docker
    async fn test_file_operations() -> Result<(), Box<dyn std::error::Error>> {
        // Setup Docker container
        let container_id = setup_docker_container().await?;

        // Create server config
        let server_config = ServerConfig {
            host: "localhost".to_string(),
            port: 2222,
            auth: AuthMethod::Password {
                username: "root".to_string(),
                password: "password".to_string(),
            },
            install_dir: "/opt/osvm".to_string(),
        };

        // Create SSH client
        let mut client = SshClient::new(server_config.clone())?;

        // Connect to server
        client.connect()?;

        // Create directory
        client.create_directory("/tmp/test")?;
        assert!(client.directory_exists("/tmp/test")?);

        // Create temporary file
        let temp_path = std::env::temp_dir().join("test.txt");
        std::fs::write(&temp_path, "Hello, world!")?;

        // Upload file
        client.upload_file(&temp_path, "/tmp/test/test.txt")?;
        assert!(client.file_exists("/tmp/test/test.txt")?);

        // Verify file content
        let output = client.execute_command("cat /tmp/test/test.txt")?;
        assert_eq!(output.trim(), "Hello, world!");

        // Delete temporary file
        std::fs::remove_file(temp_path)?;

        // Close connection
        client.close();

        // Cleanup Docker container
        cleanup_docker_container(&container_id).await?;

        Ok(())
    }

    /// Test Sonic RPC deployment
    ///
    /// # Returns
    /// * `Result<(), Box<dyn std::error::Error>>` - Success/failure
    #[tokio::test]
    #[ignore] // Ignore by default as it requires Docker and takes a long time
    async fn test_sonic_rpc_deployment() -> Result<(), Box<dyn std::error::Error>> {
        // Setup Docker container
        let container_id = setup_docker_container().await?;

        // Create server config
        let server_config = ServerConfig {
            host: "localhost".to_string(),
            port: 2222,
            auth: AuthMethod::Password {
                username: "root".to_string(),
                password: "password".to_string(),
            },
            install_dir: "/opt/osvm".to_string(),
        };

        // Create deployment config
        let deployment_config = DeploymentConfig {
            svm_type: "sonic".to_string(),
            node_type: "rpc".to_string(),
            network: NetworkType::Devnet,
            node_name: "sonic-rpc".to_string(),
            rpc_url: None,
            additional_params: HashMap::new(),
            version: None,
            client_type: None,
            hot_swap_enabled: false,
            metrics_config: None,
            disk_config: None,
        };

        // Deploy Sonic RPC node
        let result = deploy_svm_node(
            server_config.clone(),
            deployment_config,
            Some(Box::new(|progress, message| {
                println!("Progress: {}% - {}", progress, message);
            })),
        )
        .await;

        // Verify deployment result
        assert!(result.is_ok());

        // Create SSH client
        let mut client = SshClient::new(server_config.clone())?;

        // Connect to server
        client.connect()?;

        // Verify service is running
        let status = client.execute_command("systemctl is-active sonic-rpc-devnet")?;
        assert_eq!(status.trim(), "active");

        // Close connection
        client.close();

        // Cleanup Docker container
        cleanup_docker_container(&container_id).await?;

        Ok(())
    }
}
