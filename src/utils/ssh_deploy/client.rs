//! SSH client implementation for remote server operations

use {
    crate::utils::ssh_deploy::{
        errors::DeploymentError,
        types::{AuthMethod, ServerConfig},
    },
    ssh2::Session,
    std::{
        fs,
        io::{self, Read, Write},
        path::Path,
    },
};

/// SSH client for interacting with remote servers
pub struct SshClient {
    /// SSH session
    session: Session,
    /// Server config
    config: ServerConfig,
    /// Whether the session is connected
    connected: bool,
}

impl SshClient {
    /// Create a new SSH client
    ///
    /// # Arguments
    /// * `config` - Server configuration
    ///
    /// # Returns
    /// * `Result<SshClient, DeploymentError>` - New SSH client
    pub fn new(config: ServerConfig) -> Result<Self, DeploymentError> {
        let tcp = std::net::TcpStream::connect(format!("{}:{}", config.host, config.port))
            .map_err(|e| DeploymentError::ConnectionError(format!("Failed to connect: {}", e)))?;

        let mut session = Session::new()
            .map_err(|e| DeploymentError::ConnectionError(format!("Failed to create session: {}", e)))?;

        session.set_tcp_stream(tcp);
        session.handshake()
            .map_err(|e| DeploymentError::ConnectionError(format!("SSH handshake failed: {}", e)))?;

        Ok(SshClient {
            session,
            config: config.clone(),
            connected: false,
        })
    }

    /// Connect to the server
    ///
    /// # Returns
    /// * `Result<(), DeploymentError>` - Success/failure
    pub fn connect(&mut self) -> Result<(), DeploymentError> {
        if self.connected {
            return Ok(());
        }

        match &self.config.auth {
            AuthMethod::Password { username, password } => {
                self.session.userauth_password(username, password)
                    .map_err(|e| DeploymentError::AuthError(format!("Password authentication failed: {}", e)))?;
            }
            AuthMethod::Key { username, key_path, passphrase } => {
                let mut key_file = fs::File::open(key_path)
                    .map_err(|e| DeploymentError::AuthError(format!("Failed to open key file {}: {}", key_path, e)))?;
                
                let mut key_contents = String::new();
                key_file.read_to_string(&mut key_contents)
                    .map_err(|e| DeploymentError::AuthError(format!("Failed to read key file: {}", e)))?;
                
                self.session.userauth_pubkey_memory(
                    username,
                    None,
                    &key_contents,
                    passphrase.as_deref(),
                )
                .map_err(|e| DeploymentError::AuthError(format!("Key authentication failed: {}", e)))?;
            }
        }

        self.connected = true;
        Ok(())
    }

    /// Execute a command on the remote server
    ///
    /// # Arguments
    /// * `command` - Command to execute
    ///
    /// # Returns
    /// * `Result<String, DeploymentError>` - Command output
    pub fn execute_command(&mut self, command: &str) -> Result<String, DeploymentError> {
        if !self.connected {
            self.connect()?;
        }

        let mut channel = self.session.channel_session()
            .map_err(|e| DeploymentError::CommandError(format!("Failed to create channel: {}", e)))?;

        channel.exec(command)
            .map_err(|e| DeploymentError::CommandError(format!("Failed to execute command: {}", e)))?;

        let mut output = String::new();
        channel.read_to_string(&mut output)
            .map_err(|e| DeploymentError::CommandError(format!("Failed to read command output: {}", e)))?;

        channel.wait_close()
            .map_err(|e| DeploymentError::CommandError(format!("Failed to close channel: {}", e)))?;

        let exit_status = channel.exit_status()
            .map_err(|e| DeploymentError::CommandError(format!("Failed to get exit status: {}", e)))?;

        if exit_status != 0 {
            return Err(DeploymentError::CommandError(format!(
                "Command exited with status {}: {}\nCommand: {}",
                exit_status, output, command
            )));
        }

        Ok(output)
    }

    /// Upload a file to the remote server (part 1: setup)
    ///
    /// # Arguments
    /// * `local_path` - Local file path
    /// * `remote_path` - Remote file path
    ///
    /// # Returns
    /// * `Result<(), DeploymentError>` - Success/failure
    pub fn upload_file<P: AsRef<Path>, Q: AsRef<Path>>(
        &mut self,
        local_path: P,
        remote_path: Q,
    ) -> Result<(), DeploymentError> {
        if !self.connected {
            self.connect()?;
        }

        let local_path = local_path.as_ref();
        let remote_path = remote_path.as_ref();

        let mut local_file = fs::File::open(local_path)
            .map_err(|e| DeploymentError::FileTransferError(format!(
                "Failed to open local file {}: {}", 
                local_path.display(), e
            )))?;

        let file_size = local_file.metadata()
            .map_err(|e| DeploymentError::FileTransferError(format!(
                "Failed to get file metadata: {}", e
            )))?.len();

        let mut remote_file = self.session.scp_send(
            remote_path,
            0o644,
            file_size,
            None,
        )
        .map_err(|e| DeploymentError::FileTransferError(format!(
            "Failed to initiate SCP transfer to {}: {}", 
            remote_path.display(), e
        )))?;

        self.upload_file_content(&mut local_file, &mut remote_file)
    }

    /// Upload file content (part 2: content transfer)
    ///
    /// # Arguments
    /// * `local_file` - Local file handle
    /// * `remote_file` - Remote file handle
    ///
    /// # Returns
    /// * `Result<(), DeploymentError>` - Success/failure
    fn upload_file_content(
        &mut self,
        local_file: &mut fs::File,
        remote_file: &mut ssh2::Channel,
    ) -> Result<(), DeploymentError> {
        // Create buffer and copy file contents
        let mut buffer = [0; 16384];
        loop {
            match local_file.read(&mut buffer) {
                Ok(0) => break, // EOF
                Ok(n) => {
                    remote_file.write_all(&buffer[..n])
                        .map_err(|e| DeploymentError::FileTransferError(format!(
                            "Failed to write data to remote file: {}", e
                        )))?;
                }
                Err(e) => {
                    return Err(DeploymentError::FileTransferError(format!(
                        "Failed to read from local file: {}", e
                    )));
                }
            }
        }

        remote_file.send_eof()
            .map_err(|e| DeploymentError::FileTransferError(format!(
                "Failed to signal EOF: {}", e
            )))?;

        remote_file.wait_eof()
            .map_err(|e| DeploymentError::FileTransferError(format!(
                "Failed to wait for EOF confirmation: {}", e
            )))?;

        remote_file.close()
            .map_err(|e| DeploymentError::FileTransferError(format!(
                "Failed to close remote file: {}", e
            )))?;

        remote_file.wait_close()
            .map_err(|e| DeploymentError::FileTransferError(format!(
                "Failed to wait for confirmation of file close: {}", e
            )))?;

        Ok(())
    }

    /// Create a directory on the remote server
    ///
    /// # Arguments
    /// * `path` - Directory path
    ///
    /// # Returns
    /// * `Result<(), DeploymentError>` - Success/failure
    pub fn create_directory(&mut self, path: &str) -> Result<(), DeploymentError> {
        self.execute_command(&format!("mkdir -p {}", path))?;
        Ok(())
    }

    /// Check if a file exists on the remote server
    ///
    /// # Arguments
    /// * `path` - File path
    ///
    /// # Returns
    /// * `Result<bool, DeploymentError>` - Whether the file exists
    pub fn file_exists(&mut self, path: &str) -> Result<bool, DeploymentError> {
        let output = self.execute_command(&format!("test -f {} && echo 'EXISTS' || echo 'NOT_EXISTS'", path))?;
        Ok(output.trim() == "EXISTS")
    }

    /// Check if a directory exists on the remote server
    ///
    /// # Arguments
    /// * `path` - Directory path
    ///
    /// # Returns
    /// * `Result<bool, DeploymentError>` - Whether the directory exists
    pub fn directory_exists(&mut self, path: &str) -> Result<bool, DeploymentError> {
        let output = self.execute_command(&format!("test -d {} && echo 'EXISTS' || echo 'NOT_EXISTS'", path))?;
        Ok(output.trim() == "EXISTS")
    }

    /// Check if a package is installed on the remote server
    ///
    /// # Arguments
    /// * `package` - Package name
    ///
    /// # Returns
    /// * `Result<bool, DeploymentError>` - Whether the package is installed
    pub fn is_package_installed(&mut self, package: &str) -> Result<bool, DeploymentError> {
        let cmd = format!(
            "if command -v dpkg >/dev/null 2>&1; then \
             dpkg -l | grep -q '{}' && echo 'INSTALLED' || echo 'NOT_INSTALLED'; \
             elif command -v rpm >/dev/null 2>&1; then \
             rpm -q '{}' >/dev/null 2>&1 && echo 'INSTALLED' || echo 'NOT_INSTALLED'; \
             else command -v '{}' >/dev/null 2>&1 && \
             echo 'INSTALLED' || echo 'NOT_INSTALLED'; fi",
            package, package, package
        );
        
        let output = self.execute_command(&cmd)?;
        Ok(output.trim() == "INSTALLED")
    }

    /// Get system information from the remote server
    ///
    /// # Returns
    /// * `Result<std::collections::HashMap<String, String>, DeploymentError>` - System information
    pub fn get_system_info(&mut self) -> Result<std::collections::HashMap<String, String>, DeploymentError> {
        let mut info = std::collections::HashMap::new();
        
        // Get OS information
        self.get_os_info(&mut info)?;
        
        // Get CPU information
        let cpu_info = self.execute_command("cat /proc/cpuinfo | grep -c processor")?;
        info.insert("cpu_cores".to_string(), cpu_info.trim().to_string());
        
        // Get memory information
        let mem_info = self.execute_command("free -g | grep Mem | awk '{print $2}'")?;
        info.insert("memory_gb".to_string(), mem_info.trim().to_string());
        
        // Get disk and kernel information
        self.get_disk_and_kernel_info(&mut info)?;
        
        Ok(info)
    }

    /// Get OS information
    ///
    /// # Arguments
    /// * `info` - Information map to populate
    ///
    /// # Returns
    /// * `Result<(), DeploymentError>` - Success/failure
    fn get_os_info(&mut self, info: &mut std::collections::HashMap<String, String>) -> Result<(), DeploymentError> {
        let os_info = self.execute_command("cat /etc/os-release | grep -E '^(NAME|VERSION_ID)' | sed 's/.*=//' | tr -d '\"'")?;
        let os_lines: Vec<&str> = os_info.trim().split('\n').collect();
        
        if os_lines.len() >= 2 {
            info.insert("os_name".to_string(), os_lines[0].to_string());
            info.insert("os_version".to_string(), os_lines[1].to_string());
        }
        
        Ok(())
    }

    /// Get disk and kernel information
    ///
    /// # Arguments
    /// * `info` - Information map to populate
    ///
    /// # Returns
    /// * `Result<(), DeploymentError>` - Success/failure
    fn get_disk_and_kernel_info(&mut self, info: &mut std::collections::HashMap<String, String>) -> Result<(), DeploymentError> {
        // Get disk information
        let disk_info = self.execute_command("df -h / | tail -1 | awk '{print $2,$4}'")?;
        let disk_parts: Vec<&str> = disk_info.trim().split_whitespace().collect();
        
        if disk_parts.len() >= 2 {
            info.insert("disk_total".to_string(), disk_parts[0].to_string());
            info.insert("disk_available".to_string(), disk_parts[1].to_string());
        }
        
        // Get kernel information
        let kernel_info = self.execute_command("uname -r")?;
        info.insert("kernel".to_string(), kernel_info.trim().to_string());
        
        Ok(())
    }

    /// Close the SSH connection
    pub fn close(&mut self) {
        self.session.disconnect(None, "Closing connection", None).ok();
        self.connected = false;
    }
}

impl Drop for SshClient {
    fn drop(&mut self) {
        self.close();
    }
}