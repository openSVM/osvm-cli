//! SSH deployment utilities
//! Provides functionality for deploying SVM nodes via SSH

use {
    serde::{Deserialize, Serialize},
    std::{
        error::Error,
        fmt,
        fs,
        io::{self, Read, Write},
        path::Path,
        time::Duration,
        str::FromStr,
    },
    ssh2::Session,
    tokio::time,
};

/// Network type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum NetworkType {
    /// Mainnet
    Mainnet,
    /// Testnet
    Testnet,
    /// Devnet
    Devnet,
}

impl fmt::Display for NetworkType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NetworkType::Mainnet => write!(f, "mainnet"),
            NetworkType::Testnet => write!(f, "testnet"),
            NetworkType::Devnet => write!(f, "devnet"),
        }
    }
}

impl FromStr for NetworkType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "mainnet" => Ok(NetworkType::Mainnet),
            "testnet" => Ok(NetworkType::Testnet),
            "devnet" => Ok(NetworkType::Devnet),
            _ => Err(format!("Invalid network type: {}", s)),
        }
    }
}

/// Node type (validator or RPC)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum NodeType {
    Validator,
    Rpc,
}

/// Authentication method
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AuthMethod {
    /// Password authentication
    Password {
        /// Username
        username: String,
        /// Password
        password: String,
    },
    /// SSH key authentication
    Key {
        /// Username
        username: String,
        /// Path to private key
        key_path: String,
        /// Private key passphrase (if any)
        passphrase: Option<String>,
    },
}

/// Server connection parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerConfig {
    /// Host
    pub host: String,
    /// Port
    pub port: u16,
    /// Authentication method
    pub auth: AuthMethod,
    /// Installation directory
    pub install_dir: String,
}

impl ServerConfig {
    /// Create a ServerConfig from a connection string (user@host[:port])
    ///
    /// # Arguments
    /// * `conn_str` - Connection string in the format user@host[:port]
    ///
    /// # Returns
    /// * `Result<ServerConfig, DeploymentError>` - Server config or error
    pub fn from_connection_string(conn_str: &str) -> Result<Self, DeploymentError> {
        let parts: Vec<&str> = conn_str.split('@').collect();
        if parts.len() != 2 {
            return Err(DeploymentError::InvalidConfig(format!("Invalid connection string: {}", conn_str)));
        }
        
        let username = parts[0].to_string();
        let host_parts: Vec<&str> = parts[1].split(':').collect();
        let host = host_parts[0].to_string();
        let port = if host_parts.len() > 1 { host_parts[1].parse().unwrap_or(22) } else { 22 };
        
        Ok(ServerConfig {
            host, port, auth: AuthMethod::Key { username, key_path: "~/.ssh/id_rsa".to_string(), passphrase: None }, install_dir: "/opt/osvm".to_string(),
        })
    }
}

/// Deployment configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeploymentConfig {
    /// SVM type (e.g., "solana", "sonic")
    pub svm_type: String,
    /// Node type (validator or RPC)
    pub node_type: String,
    /// Network type
    pub network: NetworkType,
    /// Node name
    pub node_name: String,
    /// RPC URL
    pub rpc_url: Option<String>,
    /// Additional config parameters
    pub additional_params: std::collections::HashMap<String, String>,
}

/// Deployment error types
#[derive(Debug)]
pub enum DeploymentError {
    /// Connection error
    ConnectionError(String),
    /// Authentication error
    AuthError(String),
    /// Command execution error
    CommandError(String),
    /// File transfer error
    FileTransferError(String),
    /// Configuration error
    InvalidConfig(String),
    /// Configuration error
    ConfigError(String),
    /// Validation error
    ValidationError(String),
    /// SSH error
    SshError(ssh2::Error),
    /// IO error
    IoError(io::Error),
    /// Deployment error
    DeploymentError(String),
    /// Other error
    Other(String),
}

impl fmt::Display for DeploymentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DeploymentError::ConnectionError(msg) => write!(f, "Connection error: {}", msg),
            DeploymentError::AuthError(msg) => write!(f, "Authentication error: {}", msg),
            DeploymentError::CommandError(msg) => write!(f, "Command execution error: {}", msg),
            DeploymentError::FileTransferError(msg) => write!(f, "File transfer error: {}", msg),
            DeploymentError::InvalidConfig(msg) => write!(f, "Invalid configuration: {}", msg),
            DeploymentError::ConfigError(msg) => write!(f, "Configuration error: {}", msg),
            DeploymentError::ValidationError(msg) => write!(f, "Validation error: {}", msg),
            DeploymentError::SshError(e) => write!(f, "SSH error: {}", e),
            DeploymentError::IoError(e) => write!(f, "IO error: {}", e),
            DeploymentError::DeploymentError(msg) => write!(f, "Deployment error: {}", msg),
            DeploymentError::Other(msg) => write!(f, "Error: {}", msg),
        }
    }
}

impl Error for DeploymentError {}

impl From<ssh2::Error> for DeploymentError {
    fn from(e: ssh2::Error) -> Self {
        DeploymentError::SshError(e)
    }
}

impl From<io::Error> for DeploymentError {
    fn from(e: io::Error) -> Self {
        DeploymentError::IoError(e)
    }
}

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

    /// Upload a file to the remote server
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
        let output = self.execute_command(&format!(
            "if command -v dpkg >/dev/null 2>&1; then dpkg -l | grep -q '{}' && echo 'INSTALLED' || echo 'NOT_INSTALLED'; \
             elif command -v rpm >/dev/null 2>&1; then rpm -q '{}' >/dev/null 2>&1 && echo 'INSTALLED' || echo 'NOT_INSTALLED'; \
             else command -v '{}' >/dev/null 2>&1 && echo 'INSTALLED' || echo 'NOT_INSTALLED'; fi",
            package, package, package
        ))?;
        
        Ok(output.trim() == "INSTALLED")
    }

    /// Get system information from the remote server
    ///
    /// # Returns
    /// * `Result<std::collections::HashMap<String, String>, DeploymentError>` - System information
    pub fn get_system_info(&mut self) -> Result<std::collections::HashMap<String, String>, DeploymentError> {
        let mut info = std::collections::HashMap::new();
        
        // Get OS information
        let os_info = self.execute_command("cat /etc/os-release | grep -E '^(NAME|VERSION_ID)' | sed 's/.*=//' | tr -d '\"'")?;
        let os_lines: Vec<&str> = os_info.trim().split('\n').collect();
        
        if os_lines.len() >= 2 {
            info.insert("os_name".to_string(), os_lines[0].to_string());
            info.insert("os_version".to_string(), os_lines[1].to_string());
        }
        
        // Get CPU information
        let cpu_info = self.execute_command("cat /proc/cpuinfo | grep -c processor")?;
        info.insert("cpu_cores".to_string(), cpu_info.trim().to_string());
        
        // Get memory information
        let mem_info = self.execute_command("free -g | grep Mem | awk '{print $2}'")?;
        info.insert("memory_gb".to_string(), mem_info.trim().to_string());
        
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
        
        Ok(info)
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
    progress_callback: Option<Box<dyn Fn(u8, &str) + Send>>,
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
            deploy_solana(
                &mut client, 
                &server_config, 
                &deployment_config, 
                progress_callback.as_ref(),
            ).await?;
        },
        "sonic" => {
            deploy_sonic(
                &mut client, 
                &server_config, 
                &deployment_config, 
                progress_callback.as_ref(),
            ).await?;
        },
        "sui" => {
            deploy_sui(
                &mut client, 
                &server_config, 
                &deployment_config, 
                progress_callback.as_ref(),
            ).await?;
        },
        "aptos" => {
            deploy_aptos(
                &mut client, 
                &server_config, 
                &deployment_config, 
                progress_callback.as_ref(),
            ).await?;
        },
        _ => {
            return Err(Box::new(DeploymentError::ValidationError(format!(
                "Unsupported SVM type: {}", deployment_config.svm_type
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

/// Validate system requirements for deployment
///
/// # Arguments
/// * `system_info` - System information
/// * `deployment_config` - Deployment configuration
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn validate_system_requirements(
    system_info: &std::collections::HashMap<String, String>,
    deployment_config: &DeploymentConfig,
) -> Result<(), DeploymentError> {
    // Get required CPU, memory, and disk based on SVM type and node type
    let (required_cpu, required_memory, required_disk) = match (deployment_config.svm_type.as_str(), deployment_config.node_type.as_str()) {
        ("solana", "validator") => (12, 128, 2048),
        ("solana", "rpc") => (16, 256, 4096),
        ("sonic", "validator") => (8, 32, 1024),
        ("sonic", "rpc") => (16, 64, 2048),
        ("sui", "validator") => (8, 32, 1024),
        ("sui", "rpc") => (16, 64, 2048),
        ("aptos", "validator") => (8, 32, 1024),
        ("aptos", "rpc") => (16, 64, 2048),
        _ => {
            return Err(DeploymentError::ValidationError(format!(
                "Unsupported SVM type or node type: {}/{}",
                deployment_config.svm_type, deployment_config.node_type
            )));
        }
    };
    
    // Check CPU cores
    let cpu_cores = system_info.get("cpu_cores")
        .ok_or_else(|| DeploymentError::ValidationError("CPU cores information not available".to_string()))?
        .parse::<u8>()
        .map_err(|e| DeploymentError::ValidationError(format!("Invalid CPU cores value: {}", e)))?;

    if cpu_cores < required_cpu {
        return Err(DeploymentError::ValidationError(format!(
            "Insufficient CPU cores: {} (required: {})",
            cpu_cores, required_cpu
        )));
    }

    // Check memory
    let memory_gb = system_info.get("memory_gb")
        .ok_or_else(|| DeploymentError::ValidationError("Memory information not available".to_string()))?
        .parse::<u16>()
        .map_err(|e| DeploymentError::ValidationError(format!("Invalid memory value: {}", e)))?;

    if memory_gb < required_memory {
        return Err(DeploymentError::ValidationError(format!(
            "Insufficient memory: {} GB (required: {} GB)",
            memory_gb, required_memory
        )));
    }

    // Check available disk space
    let available_disk = system_info.get("disk_available")
        .ok_or_else(|| DeploymentError::ValidationError("Disk space information not available".to_string()))?;

    // Parse disk space - handle different units (G, T)
    let available_disk_gb = if available_disk.ends_with('G') {
        available_disk[..available_disk.len()-1].parse::<f64>()
            .map_err(|e| DeploymentError::ValidationError(format!("Invalid disk space value: {}", e)))?
    } else if available_disk.ends_with('T') {
        available_disk[..available_disk.len()-1].parse::<f64>()
            .map_err(|e| DeploymentError::ValidationError(format!("Invalid disk space value: {}", e)))?
            * 1024.0
    } else {
        return Err(DeploymentError::ValidationError(format!(
            "Unrecognized disk space unit: {}", available_disk
        )));
    };

    if available_disk_gb as u16 < required_disk {
        return Err(DeploymentError::ValidationError(format!(
            "Insufficient disk space: {} GB (required: {} GB)",
            available_disk_gb, required_disk
        )));
    }
    
    Ok(())
}

/// Install dependencies on the remote server
///
/// # Arguments
/// * `client` - SSH client
/// * `deployment_config` - Deployment configuration
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn install_dependencies(
    client: &mut SshClient,
    deployment_config: &DeploymentConfig,
) -> Result<(), DeploymentError> {
    // Update package list
    client.execute_command("sudo apt-get update")?;
    
    // Install common dependencies
    client.execute_command("sudo apt-get install -y build-essential libssl-dev pkg-config curl git")?;
    
    // Install SVM-specific dependencies
    match deployment_config.svm_type.as_str() {
        "solana" => {
            if !client.is_package_installed("libudev-dev")? {
                client.execute_command("sudo apt-get install -y libudev-dev")?;
            }
        },
        "sonic" => {
            if !client.is_package_installed("libclang-dev")? {
                client.execute_command("sudo apt-get install -y libclang-dev")?;
            }
        },
        "sui" => {
            if !client.is_package_installed("cmake")? {
                client.execute_command("sudo apt-get install -y cmake")?;
            }
        },
        "aptos" => {
            if !client.is_package_installed("libncursesw5")? {
                client.execute_command("sudo apt-get install -y libncursesw5")?;
            }
        },
        _ => {
            return Err(DeploymentError::ValidationError(format!(
                "Unsupported SVM type: {}", deployment_config.svm_type
            )));
        }
    }
    
    // Install Rust if needed
    if !client.is_package_installed("rustc")? {
        client.execute_command("curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y")?;
        client.execute_command("source $HOME/.cargo/env")?;
    }
    
    Ok(())
}

/// Deploy Solana node
///
/// # Arguments
/// * `client` - SSH client
/// * `server_config` - Server configuration
/// * `deployment_config` - Deployment configuration
/// * `progress_callback` - Callback function for progress updates
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
async fn deploy_solana(
    client: &mut SshClient,
    server_config: &ServerConfig,
    deployment_config: &DeploymentConfig,
    progress_callback: Option<&Box<dyn Fn(u8, &str) + Send>>,
) -> Result<(), DeploymentError> {
    // Install Solana CLI
    if let Some(callback) = progress_callback {
        callback(40, "Installing Solana CLI");
    }
    
    client.execute_command("sh -c \"$(curl -sSfL https://release.solana.com/v1.16.0/install)\"")?;
    client.execute_command("export PATH=\"/home/$(whoami)/.local/share/solana/install/active_release/bin:$PATH\"")?;
    
    // Create Solana directory
    let solana_dir = format!("{}/solana", server_config.install_dir);
    client.create_directory(&solana_dir)?;
    
    // Generate Solana keypair
    if let Some(callback) = progress_callback {
        callback(50, "Generating keypair");
    }
    
    let keypair_path = format!("{}/validator-keypair.json", solana_dir);
    if !client.file_exists(&keypair_path)? {
        client.execute_command(&format!("solana-keygen new -o {} --no-passphrase", keypair_path))?;
    }
    
    // Configure network
    if let Some(callback) = progress_callback {
        callback(60, "Configuring network");
    }
    
    let network_flag = match deployment_config.network {
        NetworkType::Mainnet => "--url https://api.mainnet-beta.solana.com",
        NetworkType::Testnet => "--url https://api.testnet.solana.com",
        NetworkType::Devnet => "--url https://api.devnet.solana.com",
    };
    
    client.execute_command(&format!("solana config set {}", network_flag))?;
    
    // Create systemd service
    if let Some(callback) = progress_callback {
        callback(70, "Creating systemd service");
    }
    
    let service_name = format!("solana-{}-{}", deployment_config.node_type, deployment_config.network);
    let service_content = match deployment_config.node_type.as_str() {
        "validator" => format!(
            "[Unit]\n\
            Description=Solana Validator\n\
            After=network.target\n\
            \n\
            [Service]\n\
            User=$(whoami)\n\
            ExecStart=/home/$(whoami)/.local/share/solana/install/active_release/bin/solana-validator \\\n\
              --identity {} \\\n\
              --ledger {}/ledger \\\n\
              --rpc-port 8899 \\\n\
              --dynamic-port-range 8000-8020 \\\n\
              --entrypoint entrypoint.{}.solana.com:8001 \\\n\
              --expected-genesis-hash GENESIS_HASH \\\n\
              --wal-recovery-mode skip_any_corrupted_record \\\n\
              --limit-ledger-size\n\
            Restart=always\n\
            RestartSec=1\n\
            \n\
            [Install]\n\
            WantedBy=multi-user.target\n",
            keypair_path,
            solana_dir,
            deployment_config.network,
        ),
        "rpc" => format!(
            "[Unit]\n\
            Description=Solana RPC Node\n\
            After=network.target\n\
            \n\
            [Service]\n\
            User=$(whoami)\n\
            ExecStart=/home/$(whoami)/.local/share/solana/install/active_release/bin/solana-validator \\\n\
              --identity {} \\\n\
              --ledger {}/ledger \\\n\
              --rpc-port 8899 \\\n\
              --private-rpc \\\n\
              --dynamic-port-range 8000-8020 \\\n\
              --entrypoint entrypoint.{}.solana.com:8001 \\\n\
              --expected-genesis-hash GENESIS_HASH \\\n\
              --wal-recovery-mode skip_any_corrupted_record \\\n\
              --enable-rpc-transaction-history \\\n\
              --limit-ledger-size\n\
            Restart=always\n\
            RestartSec=1\n\
            \n\
            [Install]\n\
            WantedBy=multi-user.target\n",
            keypair_path,
            solana_dir,
            deployment_config.network,
        ),
        _ => {
            return Err(DeploymentError::ValidationError(format!(
                "Unsupported node type: {}", deployment_config.node_type
            )));
        }
    };
    
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
    
    // Start the service
    if let Some(callback) = progress_callback {
        callback(90, "Starting service");
    }
    
    client.execute_command(&format!("sudo systemctl enable {}", service_name))?;
    client.execute_command(&format!("sudo systemctl start {}", service_name))?;
    
    // Wait for the service to start
    await_service_startup(client, &service_name).await?;
    
    Ok(())
}

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
async fn deploy_sonic(
    client: &mut SshClient,
    server_config: &ServerConfig,
    deployment_config: &DeploymentConfig,
    progress_callback: Option<&Box<dyn Fn(u8, &str) + Send>>,
) -> Result<(), DeploymentError> {
    // Clone Sonic RPC repository
    if let Some(callback) = progress_callback {
        callback(40, "Cloning Sonic RPC repository");
    }
    
    let sonic_dir = format!("{}/sonic-rpc", server_config.install_dir);
    if !client.directory_exists(&sonic_dir)? {
        client.execute_command(&format!("git clone https://github.com/sonicfromnewyoke/solana-rpc.git {}", sonic_dir))?;
    }
    
    // Install dependencies
    if let Some(callback) = progress_callback {
        callback(50, "Installing dependencies");
    }
    
    // Install required packages
    client.execute_command("sudo apt-get update")?;
    client.execute_command("sudo apt-get install -y build-essential libssl-dev pkg-config curl git jq")?;
    
    // Install Node.js if not already installed
    if !client.is_package_installed("nodejs")? {
        client.execute_command("curl -fsSL https://deb.nodesource.com/setup_16.x | sudo -E bash -")?;
        client.execute_command("sudo apt-get install -y nodejs")?;
    }
    
    // Install Docker if not already installed
    if !client.is_package_installed("docker-ce")? {
        client.execute_command("curl -fsSL https://get.docker.com -o get-docker.sh")?;
        client.execute_command("sudo sh get-docker.sh")?;
        client.execute_command("sudo usermod -aG docker $(whoami)")?;
    }
    
    // Install Docker Compose if not already installed
    if !client.is_package_installed("docker-compose")? {
        client.execute_command("sudo curl -L \"https://github.com/docker/compose/releases/download/1.29.2/docker-compose-$(uname -s)-$(uname -m)\" -o /usr/local/bin/docker-compose")?;
        client.execute_command("sudo chmod +x /usr/local/bin/docker-compose")?;
    }
    
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
    
    // Set the network configuration based on the deployment config
    let network_config = match deployment_config.network {
        NetworkType::Mainnet => "mainnet",
        NetworkType::Testnet => "testnet",
        NetworkType::Devnet => "devnet",
    };
    
    // Update the configuration file with the selected network
    client.execute_command(&format!("cd {} && echo 'SOLANA_NETWORK={}' > .env", sonic_dir, network_config))?;
    
    // Start the RPC node
    if let Some(callback) = progress_callback {
        callback(90, "Starting Sonic RPC node");
    }
    
    // Start the services using docker-compose
    client.execute_command(&format!("cd {} && docker-compose up -d", sonic_dir))?;
    
    // Create a systemd service to ensure the RPC node starts on boot
    let service_name = format!("sonic-rpc-{}", deployment_config.network);
    let service_content = format!(
        "[Unit]\n\
        Description=Sonic RPC Node\n\
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
        sonic_dir
    );
    
    // Create a temporary file with service content
    let temp_service_path = std::env::temp_dir().join(format!("{}.service", service_name));
    let mut temp_service_file = fs::File::create(&temp_service_path)?;
    temp_service_file.write_all(service_content.as_bytes())?;
    
    // Upload and install the service
    client.upload_file(&temp_service_path, &format!("/tmp/{}.service", service_name))?;
    client.execute_command(&format!("sudo mv /tmp/{}.service /etc/systemd/system/{}.service", service_name, service_name))?;
    client.execute_command("sudo systemctl daemon-reload")?;
    
    // Delete the temporary file
    fs::remove_file(temp_service_path)?;
    
    // Enable the service to start on boot
    client.execute_command(&format!("sudo systemctl enable {}", service_name))?;
    
    // Wait for the service to start
    await_service_startup(client, &service_name).await?;
    
    Ok(())
}

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
async fn deploy_sui(
    client: &mut SshClient,
    server_config: &ServerConfig,
    deployment_config: &DeploymentConfig,
    progress_callback: Option<&Box<dyn Fn(u8, &str) + Send>>,
) -> Result<(), DeploymentError> {
    // Clone Sui repository
    if let Some(callback) = progress_callback {
        callback(40, "Cloning Sui repository");
    }
    
    let sui_dir = format!("{}/", server_config.install_dir);
    if !client.directory_exists(&sui_dir)? {
        client.execute_command(&format!("git clone https://github.com/MystenLabs/sui.git {}", sui_dir))?;
    }
    
    // Build Sui binary
    if let Some(callback) = progress_callback {
        callback(50, "Building Sui binary");
    }
    
    client.execute_command(&format!("cd {} && cargo build --release --bin sui-node", sui_dir))?;
    
    // Create configuration
    if let Some(callback) = progress_callback {
        callback(70, "Creating configuration");
    }
    
    let network_flag = match deployment_config.network {
        NetworkType::Mainnet => "mainnet",
        NetworkType::Testnet => "testnet",
        NetworkType::Devnet => "devnet",
    };
    
    client.execute_command(&format!(
        "cd {} && ./target/release/sui genesis --force --{}", 
        sui_dir,
        network_flag
    ))?;
    
    // Create systemd service
    if let Some(callback) = progress_callback {
        callback(80, "Creating systemd service");
    }
    
    let service_name = format!("sui-{}-{}", deployment_config.node_type, deployment_config.network);
    let service_content = format!(
        "[Unit]\n\
        Description=Sui Node\n\
        After=network.target\n\
        \n\
        [Service]\n\
        User=$(whoami)\n\
        ExecStart={}/target/release/sui-node \\\n\
          --config-path {}/sui_config/fullnode.yaml\n\
        Restart=always\n\
        RestartSec=1\n\
        \n\
        [Install]\n\
        WantedBy=multi-user.target\n",
        sui_dir,
        sui_dir,
    );
    
    // Create a temporary file with service content
    let temp_service_path = std::env::temp_dir().join(format!("{}.service", service_name));
    let mut temp_service_file = fs::File::create(&temp_service_path)?;
    temp_service_file.write_all(service_content.as_bytes())?;
    
    // Upload and install the service
    client.upload_file(&temp_service_path, &format!("/tmp/{}.service", service_name))?;
    client.execute_command(&format!("sudo mv /tmp/{}.service /etc/systemd/system/{}.service", service_name, service_name))?;
    client.execute_command("sudo systemctl daemon-reload")?;
    
    // Delete the temporary file
    fs::remove_file(temp_service_path)?;
    
    // Start the service
    if let Some(callback) = progress_callback {
        callback(90, "Starting service");
    }
    
    client.execute_command(&format!("sudo systemctl enable {}", service_name))?;
    client.execute_command(&format!("sudo systemctl start {}", service_name))?;
    
    // Wait for the service to start
    await_service_startup(client, &service_name).await?;
    
    Ok(())
}

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
async fn deploy_aptos(
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
    if !client.directory_exists(&aptos_dir)? {
        client.execute_command(&format!("git clone https://github.com/aptos-labs/aptos-core.git {}", aptos_dir))?;
    }
    
    // Build Aptos binary
    if let Some(callback) = progress_callback {
        callback(50, "Building Aptos binary");
    }
    
    client.execute_command(&format!("cd {} && cargo build --release --package aptos-node", aptos_dir))?;
    
    // Create configuration
    if let Some(callback) = progress_callback {
        callback(70, "Creating configuration");
    }
    
    let network_flag = match deployment_config.network {
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
    
    // Create systemd service
    if let Some(callback) = progress_callback {
        callback(80, "Creating systemd service");
    }
    
    let service_name = format!("aptos-{}-{}", deployment_config.node_type, deployment_config.network);
    let service_content = format!(
        "[Unit]\n\
        Description=Aptos Node\n\
        After=network.target\n\
        \n\
        [Service]\n\
        User=$(whoami)\n\
        ExecStart={}/target/release/aptos-node \\\n\
          --config {}/config/node.yaml\n\
        Restart=always\n\
        RestartSec=1\n\
        \n\
        [Install]\n\
        WantedBy=multi-user.target\n",
        aptos_dir,
        aptos_dir,
    );
    
    // Create a temporary file with service content
    let temp_service_path = std::env::temp_dir().join(format!("{}.service", service_name));
    let mut temp_service_file = fs::File::create(&temp_service_path)?;
    temp_service_file.write_all(service_content.as_bytes())?;
    
    // Upload and install the service
    client.upload_file(&temp_service_path, &format!("/tmp/{}.service", service_name))?;
    client.execute_command(&format!("sudo mv /tmp/{}.service /etc/systemd/system/{}.service", service_name, service_name))?;
    client.execute_command("sudo systemctl daemon-reload")?;
    
    // Delete the temporary file
    fs::remove_file(temp_service_path)?;
    
    // Start the service
    if let Some(callback) = progress_callback {
        callback(90, "Starting service");
    }
    
    client.execute_command(&format!("sudo systemctl enable {}", service_name))?;
    client.execute_command(&format!("sudo systemctl start {}", service_name))?;
    
    // Wait for the service to start
    await_service_startup(client, &service_name).await?;
    
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
async fn await_service_startup(
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