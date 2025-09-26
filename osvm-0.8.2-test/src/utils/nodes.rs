use {
    crate::utils::{
        ssh_deploy::{deploy_svm_node, AuthMethod, DeploymentConfig, NetworkType, ServerConfig},
        svm_info::get_svm_info,
    },
    serde::{Deserialize, Serialize},
    solana_client::rpc_client::RpcClient,
    solana_commitment_config::CommitmentConfig,
    std::{collections::HashMap, error::Error, fmt, fs, path::PathBuf},
    tokio::runtime::Runtime,
};

use colored::Colorize;
/// Node status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum NodeStatus {
    /// Running
    Running,
    /// Stopped
    Stopped,
    /// Error
    Error,
    /// Unknown
    Unknown,
}

impl fmt::Display for NodeStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeStatus::Running => write!(f, "Running"),
            NodeStatus::Stopped => write!(f, "Stopped"),
            NodeStatus::Error => write!(f, "Error"),
            NodeStatus::Unknown => write!(f, "Unknown"),
        }
    }
}

/// Node information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeInfo {
    /// Node ID
    #[serde(default = "std::string::String::new")]
    pub id: String,
    /// SVM type
    pub svm_type: String,
    /// Node type (validator or RPC)
    pub node_type: String,
    /// Network type
    pub network: NetworkType,
    /// Node name
    #[serde(default = "std::string::String::new")]
    pub name: String,
    /// Host
    pub host: String,
    /// Status
    pub status: NodeStatus,
    /// RPC URL (if applicable)
    #[serde(default)]
    pub rpc_url: Option<String>,
    /// Creation timestamp
    pub created_at: u64,
    /// System metrics (CPU, RAM, disk usage)
    #[serde(default)]
    pub system_metrics: Option<SystemMetrics>,
    /// Additional information
    #[serde(default)]
    pub additional_info: HashMap<String, String>,
}

/// System metrics for a node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemMetrics {
    /// CPU usage (percentage)
    pub cpu_usage: f32,
    /// RAM usage (percentage)
    pub ram_usage: f32,
    /// Disk usage (percentage)
    pub disk_usage: f32,
    /// Network in (kB/s)
    pub network_in: f32,
    /// Network out (kB/s)
    pub network_out: f32,
    /// Last updated timestamp
    pub last_updated: u64,
}

/// Node error types
#[derive(Debug)]
pub enum NodeError {
    /// Node not found
    NotFound(String),
    /// Invalid configuration
    InvalidConfig(String),
    /// IO error
    IoError(std::io::Error),
    /// Serialization error
    SerializationError(String),
    /// SSH error
    SshError(String),
    /// Other error
    Other(String),
}

impl fmt::Display for NodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeError::NotFound(msg) => write!(f, "Node not found: {}", msg),
            NodeError::InvalidConfig(msg) => write!(f, "Invalid configuration: {}", msg),
            NodeError::IoError(e) => write!(f, "IO error: {}", e),
            NodeError::SerializationError(msg) => write!(f, "Serialization error: {}", msg),
            NodeError::SshError(msg) => write!(f, "SSH error: {}", msg),
            NodeError::Other(msg) => write!(f, "Error: {}", msg),
        }
    }
}

impl Error for NodeError {}

impl From<std::io::Error> for NodeError {
    fn from(e: std::io::Error) -> Self {
        NodeError::IoError(e)
    }
}

impl From<serde_json::Error> for NodeError {
    fn from(e: serde_json::Error) -> Self {
        NodeError::SerializationError(e.to_string())
    }
}

/// Sort order for node listing
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortOrder {
    /// Sort by name
    Name,
    /// Sort by SVM type
    SvmType,
    /// Sort by node type
    NodeType,
    /// Sort by network
    Network,
    /// Sort by status
    Status,
    /// Sort by creation time
    CreationTime,
}

/// Node database
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeDatabase {
    /// Nodes
    pub nodes: HashMap<String, NodeInfo>,
}

impl NodeDatabase {
    /// Load the node database from disk
    ///
    /// # Returns
    /// * `Result<NodeDatabase, NodeError>` - Database or error
    pub fn load() -> Result<Self, NodeError> {
        let db_path = get_node_db_path()?;

        if !db_path.exists() {
            return Ok(NodeDatabase {
                nodes: HashMap::new(),
            });
        }

        let db_content = fs::read_to_string(&db_path)?;
        let db = serde_json::from_str(&db_content)?;

        Ok(db)
    }

    /// Save the node database to disk
    ///
    /// # Returns
    /// * `Result<(), NodeError>` - Success/failure
    pub fn save(&self) -> Result<(), NodeError> {
        let db_path = get_node_db_path()?;

        // Create parent directories if they don't exist
        if let Some(parent) = db_path.parent() {
            fs::create_dir_all(parent)?;
        }

        let db_content = serde_json::to_string_pretty(self)?;
        fs::write(&db_path, db_content)?;

        Ok(())
    }

    /// Add a node to the database
    ///
    /// # Arguments
    /// * `node` - Node information
    ///
    /// # Returns
    /// * `Result<(), NodeError>` - Success/failure
    pub fn add_node(&mut self, node: NodeInfo) -> Result<(), NodeError> {
        self.nodes.insert(node.id.clone(), node);
        self.save()?;
        Ok(())
    }

    /// Get a node from the database
    ///
    /// # Arguments
    /// * `id` - Node ID
    ///
    /// # Returns
    /// * `Result<NodeInfo, NodeError>` - Node information or error
    pub fn get_node(&self, id: &str) -> Result<NodeInfo, NodeError> {
        self.nodes
            .get(id)
            .cloned()
            .ok_or_else(|| NodeError::NotFound(format!("Node with ID '{}' not found", id)))
    }

    /// Update a node in the database
    ///
    /// # Arguments
    /// * `id` - Node ID
    /// * `node` - Updated node information
    ///
    /// # Returns
    /// * `Result<(), NodeError>` - Success/failure
    pub fn update_node(&mut self, id: &str, node: NodeInfo) -> Result<(), NodeError> {
        if !self.nodes.contains_key(id) {
            return Err(NodeError::NotFound(format!(
                "Node with ID '{}' not found",
                id
            )));
        }

        self.nodes.insert(id.to_string(), node);
        self.save()?;

        Ok(())
    }

    /// Remove a node from the database
    ///
    /// # Arguments
    /// * `id` - Node ID
    ///
    /// # Returns
    /// * `Result<(), NodeError>` - Success/failure
    pub fn remove_node(&mut self, id: &str) -> Result<(), NodeError> {
        if !self.nodes.contains_key(id) {
            return Err(NodeError::NotFound(format!(
                "Node with ID '{}' not found",
                id
            )));
        }

        self.nodes.remove(id);
        self.save()?;

        Ok(())
    }

    /// Get all nodes of a specific SVM type
    ///
    /// # Arguments
    /// * `svm_type` - SVM type
    ///
    /// # Returns
    /// * `Vec<NodeInfo>` - Nodes of the specified SVM type
    pub fn get_nodes_by_svm(&self, svm_type: &str) -> Vec<NodeInfo> {
        self.nodes
            .values()
            .filter(|node| node.svm_type == svm_type)
            .cloned()
            .collect()
    }

    /// Get all nodes
    ///
    /// # Returns
    /// * `Vec<NodeInfo>` - All nodes
    pub fn get_all_nodes(&self) -> Vec<NodeInfo> {
        self.nodes.values().cloned().collect()
    }

    /// Sort nodes by specified criteria
    ///
    /// # Arguments
    /// * `nodes` - Nodes to sort
    /// * `sort_order` - Sort criteria
    /// * `reverse` - Whether to reverse the sort order
    ///
    /// # Returns
    /// * `Vec<NodeInfo>` - Sorted nodes
    pub fn sort_nodes(nodes: Vec<NodeInfo>, sort_order: SortOrder, reverse: bool) -> Vec<NodeInfo> {
        let mut sorted_nodes = nodes;

        match sort_order {
            SortOrder::Name => {
                sorted_nodes.sort_by(|a, b| a.name.cmp(&b.name));
            }
            SortOrder::SvmType => {
                sorted_nodes.sort_by(|a, b| a.svm_type.cmp(&b.svm_type));
            }
            SortOrder::NodeType => {
                sorted_nodes.sort_by(|a, b| a.node_type.cmp(&b.node_type));
            }
            SortOrder::Network => {
                sorted_nodes.sort_by(|a, b| a.network.to_string().cmp(&b.network.to_string()));
            }
            SortOrder::Status => {
                sorted_nodes.sort_by(|a, b| {
                    // Custom sort order for status: Running > Error > Stopped > Unknown
                    let a_val = match a.status {
                        NodeStatus::Running => 0,
                        NodeStatus::Error => 1,
                        NodeStatus::Stopped => 2,
                        NodeStatus::Unknown => 3,
                    };

                    let b_val = match b.status {
                        NodeStatus::Running => 0,
                        NodeStatus::Error => 1,
                        NodeStatus::Stopped => 2,
                        NodeStatus::Unknown => 3,
                    };

                    a_val.cmp(&b_val)
                });
            }
            SortOrder::CreationTime => {
                sorted_nodes.sort_by(|a, b| a.created_at.cmp(&b.created_at));
            }
        }

        if reverse {
            sorted_nodes.reverse();
        }

        sorted_nodes
    }

    /// Search nodes by keyword
    ///
    /// # Arguments
    /// * `nodes` - Nodes to search
    /// * `keyword` - Search keyword
    ///
    /// # Returns
    /// * `Vec<NodeInfo>` - Matching nodes
    pub fn search_nodes(nodes: Vec<NodeInfo>, keyword: &str) -> Vec<NodeInfo> {
        let keyword_lower = keyword.to_lowercase();

        nodes
            .into_iter()
            .filter(|node| {
                node.id.to_lowercase().contains(&keyword_lower)
                    || node.name.to_lowercase().contains(&keyword_lower)
                    || node.svm_type.to_lowercase().contains(&keyword_lower)
                    || node.host.to_lowercase().contains(&keyword_lower)
            })
            .collect()
    }
}

/// Get the path to the node database file
///
/// # Returns
/// * `Result<PathBuf, NodeError>` - Path to the database file
fn get_node_db_path() -> Result<PathBuf, NodeError> {
    let home_dir = dirs::home_dir()
        .ok_or_else(|| NodeError::Other("Failed to get home directory".to_string()))?;

    Ok(home_dir.join(".osvm").join("nodes.json"))
}

/// List all nodes with optional filtering by network
///
/// # Arguments
/// * `client` - RPC client
/// * `network_filter` - Network to filter by (or "all")
/// * `svm_filter` - Optional SVM to filter by
/// * `node_type_filter` - Node type to filter by (or "all")
/// * `status_filter` - Node status to filter by (or "all")
/// * `commitment_config` - Commitment config
///
/// # Returns
/// * `Result<Vec<NodeInfo>, Box<dyn Error>>` - List of nodes or error
pub fn list_all_nodes(
    _client: &RpcClient,
    network_filter: &str,
    svm_filter: Option<&str>,
    node_type_filter: &str,
    status_filter: &str,
    _commitment_config: CommitmentConfig,
    verbosity: u8,
) -> Result<Vec<NodeInfo>, Box<dyn Error>> {
    let db = NodeDatabase::load()?;
    let mut nodes = db.get_all_nodes();

    // Apply SVM filter if provided
    if let Some(svm) = svm_filter {
        nodes.retain(|node| node.svm_type == svm);
    }

    // Apply network filter if not "all"
    if network_filter != "all" {
        nodes.retain(|node| node.network.to_string() == network_filter);
    }

    // Apply node type filter if not "all"
    if node_type_filter != "all" {
        nodes.retain(|node| node.node_type == node_type_filter);
    }

    // Apply status filter if not "all"
    if status_filter != "all" {
        let status = match status_filter {
            "running" => NodeStatus::Running,
            "stopped" => NodeStatus::Stopped,
            "error" => NodeStatus::Error,
            "unknown" => NodeStatus::Unknown,
            _ => NodeStatus::Running, // Default to running if invalid
        };

        nodes.retain(|node| node.status == status);
    }

    // Sort nodes by SVM type by default
    let sort_method = match verbosity {
        0 | 1 => SortOrder::SvmType,
        2 => SortOrder::Status, // For more verbose output, sort by status
        _ => SortOrder::CreationTime, // For very verbose output, sort by creation time
    };

    nodes = NodeDatabase::sort_nodes(nodes, sort_method, false);

    Ok(nodes)
}

/// Configuration for deploying a node
pub struct DeployNodeConfig {
    /// SVM type
    pub svm_type: String,
    /// Node type (validator or RPC)
    pub node_type: String,
    /// Network type
    pub network: NetworkType,
    /// Node name
    pub name: String,
    /// Host
    pub host: String,
    /// Port
    pub port: u16,
    /// Authentication method
    pub auth_method: AuthMethod,
    /// Installation directory
    pub install_dir: String,
    /// Progress callback function
    pub progress_callback: Option<crate::prelude::ProgressCallback>,
}

impl std::fmt::Debug for DeployNodeConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DeployNodeConfig")
            .field("svm_type", &self.svm_type)
            .field("node_type", &self.node_type)
            .field("network", &self.network)
            .field("name", &self.name)
            .field("host", &self.host)
            .field("port", &self.port)
            .field("auth_method", &self.auth_method)
            .field("install_dir", &self.install_dir)
            .field(
                "progress_callback",
                &if self.progress_callback.is_some() {
                    "Some(ProgressCallback)"
                } else {
                    "None"
                },
            )
            .finish()
    }
}

impl Clone for DeployNodeConfig {
    fn clone(&self) -> Self {
        Self {
            svm_type: self.svm_type.clone(),
            node_type: self.node_type.clone(),
            network: self.network,
            name: self.name.clone(),
            host: self.host.clone(),
            port: self.port,
            auth_method: self.auth_method.clone(),
            install_dir: self.install_dir.clone(),
            progress_callback: None, // Progress callback can't be cloned, so we set it to None
        }
    }
}

impl DeployNodeConfig {
    /// Create a new deploy node configuration with minimal required parameters
    pub fn new(svm_type: &str, node_type: &str, network: NetworkType) -> Self {
        Self {
            svm_type: svm_type.to_string(),
            node_type: node_type.to_string(),
            network,
            name: format!("{}-{}-{}", svm_type, node_type, network),
            host: "localhost".to_string(),
            port: 22,
            auth_method: AuthMethod::Password {
                username: "root".to_string(),
                password: "".to_string(),
            },
            install_dir: "/opt/osvm".to_string(),
            progress_callback: None,
        }
    }

    /// Set node name
    pub fn with_name(mut self, name: &str) -> Self {
        self.name = name.to_string();
        self
    }

    /// Set host
    pub fn with_host(mut self, host: &str) -> Self {
        self.host = host.to_string();
        self
    }

    /// Set port
    pub fn with_port(mut self, port: u16) -> Self {
        self.port = port;
        self
    }

    /// Set authentication method
    pub fn with_auth_method(mut self, auth_method: AuthMethod) -> Self {
        self.auth_method = auth_method;
        self
    }

    /// Set installation directory
    pub fn with_install_dir(mut self, install_dir: &str) -> Self {
        self.install_dir = install_dir.to_string();
        self
    }

    /// Set progress callback
    pub fn with_progress_callback(mut self, callback: crate::prelude::ProgressCallback) -> Self {
        self.progress_callback = Some(callback);
        self
    }
}

/// Deploy a new node
///
/// # Arguments
/// * `client` - RPC client
/// * `config` - Deployment configuration
///
/// # Returns
/// * `Result<NodeInfo, Box<dyn Error>>` - Node information or error
pub async fn deploy_node(
    client: &RpcClient,
    config: DeployNodeConfig,
) -> Result<NodeInfo, Box<dyn Error>> {
    // Get SVM information
    let svm_info = get_svm_info(client, &config.svm_type, CommitmentConfig::confirmed())?;

    // Check if the SVM supports the requested node type
    let can_install = match config.node_type.as_str() {
        "validator" => svm_info.can_install_validator,
        "rpc" => svm_info.can_install_rpc,
        _ => false,
    };

    if !can_install {
        return Err(Box::new(NodeError::InvalidConfig(format!(
            "SVM '{}' does not support installing a {} node",
            config.svm_type, config.node_type
        ))));
    }

    // Check if the network exists for this SVM
    let network_name = config.network.to_string();
    if !svm_info.networks.contains_key(&network_name) {
        return Err(Box::new(NodeError::InvalidConfig(format!(
            "Network '{}' does not exist for SVM '{}'",
            network_name, config.svm_type
        ))));
    }

    // Create server configuration
    let server_config = ServerConfig {
        host: config.host.clone(),
        port: config.port,
        auth: config.auth_method,
        install_dir: config.install_dir.clone(),
    };

    // Create deployment configuration
    let deployment_config = DeploymentConfig {
        svm_type: config.svm_type.clone(),
        node_type: config.node_type.clone(),
        network: config.network,
        node_name: config.name.clone(),
        rpc_url: None, // Will be set by the deployment process
        additional_params: HashMap::new(),
        version: None,
        client_type: None,
        hot_swap_enabled: false,
        metrics_config: None,
        disk_config: None,
    };

    // Deploy the node
    deploy_svm_node(
        server_config.clone(),
        deployment_config,
        config.progress_callback,
    )
    .await?;

    // Create node information
    let node_info = NodeInfo {
        id: format!(
            "{}-{}-{}-{}",
            config.svm_type, config.node_type, config.network, config.host
        ),
        system_metrics: None,
        svm_type: config.svm_type.clone(),
        node_type: config.node_type.clone(),
        network: config.network,
        name: config.name.clone(),
        host: config.host.clone(),
        status: NodeStatus::Running,
        rpc_url: if config.node_type == "rpc" {
            Some(format!("http://{}:8899", config.host))
        } else {
            None
        },
        created_at: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs(),
        additional_info: HashMap::new(),
    };

    // Add the node to the database
    let mut db = NodeDatabase::load()?;
    db.add_node(node_info.clone())?;

    Ok(node_info)
}

/// Stop a node
///
/// # Arguments
/// * `node_id` - Node ID
///
/// # Returns
/// * `Result<(), Box<dyn Error>>` - Success/failure
pub fn stop_node(node_id: &str) -> Result<(), Box<dyn Error>> {
    let mut db = NodeDatabase::load()?;
    let mut node = db.get_node(node_id)?;

    // Create SSH client
    let server_config = ServerConfig {
        host: node.host.clone(),
        port: 22, // Default SSH port
        auth: AuthMethod::Key {
            username: "root".to_string(),
            key_path: "~/.ssh/id_rsa".to_string(),
            passphrase: None,
        },
        install_dir: "/opt/osvm".to_string(),
    };

    let service_name = format!("{}-{}-{}", node.svm_type, node.node_type, node.network);

    // Create runtime for executing async SSH client
    let rt = Runtime::new()?;
    rt.block_on(async {
        use crate::utils::ssh_deploy::SshClient;

        let mut client = SshClient::new(server_config.clone())?;
        client.connect()?;

        client.execute_command(&format!("sudo systemctl stop {}", service_name))?;

        Ok::<_, Box<dyn Error>>(())
    })?;

    // Update node status
    node.status = NodeStatus::Stopped;
    db.update_node(node_id, node)?;

    Ok(())
}

/// Start a node
///
/// # Arguments
/// * `node_id` - Node ID
///
/// # Returns
/// * `Result<(), Box<dyn Error>>` - Success/failure
pub fn start_node(node_id: &str) -> Result<(), Box<dyn Error>> {
    let mut db = NodeDatabase::load()?;
    let mut node = db.get_node(node_id)?;

    // Create SSH client
    let server_config = ServerConfig {
        host: node.host.clone(),
        port: 22, // Default SSH port
        auth: AuthMethod::Key {
            username: "root".to_string(),
            key_path: "~/.ssh/id_rsa".to_string(),
            passphrase: None,
        },
        install_dir: "/opt/osvm".to_string(),
    };

    let service_name = format!("{}-{}-{}", node.svm_type, node.node_type, node.network);

    // Create runtime for executing async SSH client
    let rt = Runtime::new()?;
    rt.block_on(async {
        use crate::utils::ssh_deploy::SshClient;

        let mut client = SshClient::new(server_config.clone())?;
        client.connect()?;

        client.execute_command(&format!("sudo systemctl start {}", service_name))?;

        Ok::<_, Box<dyn Error>>(())
    })?;

    // Update node status
    node.status = NodeStatus::Running;
    db.update_node(node_id, node)?;

    Ok(())
}

/// Get node status
///
/// # Arguments
/// * `node_id` - Node ID
///
/// # Returns
/// * `Result<NodeStatus, Box<dyn Error>>` - Node status or error
pub fn get_node_status(node_id: &str) -> Result<NodeStatus, Box<dyn Error>> {
    let db = NodeDatabase::load()?;
    let node = db.get_node(node_id)?;

    // Create SSH client
    let server_config = ServerConfig {
        host: node.host.clone(),
        port: 22, // Default SSH port
        auth: AuthMethod::Key {
            username: "root".to_string(),
            key_path: "~/.ssh/id_rsa".to_string(),
            passphrase: None,
        },
        install_dir: "/opt/osvm".to_string(),
    };

    let service_name = format!("{}-{}-{}", node.svm_type, node.node_type, node.network);

    // Create runtime for executing async SSH client
    let rt = Runtime::new()?;
    let status = rt.block_on(async {
        use crate::utils::ssh_deploy::SshClient;

        let mut client = SshClient::new(server_config.clone())?;
        client.connect()?;

        let status_output =
            client.execute_command(&format!("systemctl is-active {}", service_name))?;

        Ok::<_, Box<dyn Error>>(match status_output.trim() {
            "active" => NodeStatus::Running,
            "inactive" => NodeStatus::Stopped,
            "failed" => NodeStatus::Error,
            _ => NodeStatus::Unknown,
        })
    })?;

    Ok(status)
}

/// Display a list of nodes
///
/// # Arguments
/// * `nodes` - List of nodes
pub fn display_node_list(nodes: &[NodeInfo], verbosity: u8) {
    println!("\n{}", "OSVM - Node Management".cyan().bold());
    println!("{}", "Managed SVM Nodes:".green().bold());
    println!("{}", "=================".green());

    if nodes.is_empty() {
        println!("\n{}", "No nodes are currently managed by OSVM.".yellow());
        println!(
            "\n{} Use '{}' to deploy a new node",
            "TIP:".yellow().bold(),
            "osvm node deploy".cyan()
        );
        return;
    }

    // Count nodes by status
    let mut running = 0;
    let mut stopped = 0;
    let mut error = 0;
    let mut unknown = 0;

    for node in nodes {
        match node.status {
            NodeStatus::Running => running += 1,
            NodeStatus::Stopped => stopped += 1,
            NodeStatus::Error => error += 1,
            NodeStatus::Unknown => unknown += 1,
        }
    }

    // Count nodes by type
    let mut validators = 0;
    let mut rpc_nodes = 0;

    for node in nodes {
        match node.node_type.as_str() {
            "validator" => validators += 1,
            "rpc" => rpc_nodes += 1,
            _ => {}
        }
    }

    // Print summary
    println!(
        "Total Nodes: {} ({} running, {} stopped, {} error, {} unknown)",
        nodes.len(),
        running.to_string().green(),
        stopped.to_string().yellow(),
        error.to_string().red(),
        unknown.to_string().bright_black()
    );
    println!(
        "Node Types: {} validators, {} RPC nodes\n",
        validators.to_string().cyan(),
        rpc_nodes.to_string().cyan()
    );

    // Print table header
    println!(
        "{:<16} {:<12} {:<10} {:<10} {:<16} {:<10}",
        "ID".blue().bold(),
        "SVM".blue().bold(),
        "TYPE".blue().bold(),
        "NETWORK".blue().bold(),
        "HOST".blue().bold(),
        "STATUS".blue().bold()
    );

    println!(
        "{:<16} {:<12} {:<10} {:<10} {:<16} {:<10}",
        "----", "---", "----", "-------", "----", "------"
    );

    println!(
        "{}",
        "---------------------------------------------------------------------------------"
            .bright_black()
    );

    for node in nodes {
        // Format the status with color
        let status_color = match node.status {
            NodeStatus::Running => node.status.to_string().green(),
            NodeStatus::Stopped => node.status.to_string().yellow(),
            NodeStatus::Error => node.status.to_string().red(),
            NodeStatus::Unknown => node.status.to_string().bright_black(),
        };

        // Format the ID to handle long IDs
        let id_display = if node.id.len() > 15 {
            format!("{}...", &node.id[0..12])
        } else {
            node.id.clone()
        };

        println!(
            "{:<16} {:<12} {:<10} {:<10} {:<16} {:<10}",
            id_display, node.svm_type, node.node_type, node.network, node.host, status_color
        );

        // Show RPC URL for RPC nodes at higher verbosity levels
        if verbosity >= 2 && node.node_type == "rpc" && node.rpc_url.is_some() {
            println!(
                "    {}: {}",
                "RPC URL".bright_black(),
                node.rpc_url.as_ref().unwrap().bright_cyan()
            );
        }
    } // End of for-loop

    // Print helpful tips at the bottom
    println!("\n{} Available commands:", "TIPS:".yellow().bold());
    println!(
        "  - {}: Get detailed information about a specific node",
        "osvm node info <id>".cyan()
    );
    println!(
        "  - {}: Filter nodes by SVM type",
        "osvm node list --svm <name>".cyan()
    );
    println!(
        "  - {}: Filter nodes by network",
        "osvm node list --network <network>".cyan()
    );
    println!(
        "  - {}: Filter nodes by type",
        "osvm node list --type <validator|rpc>".cyan()
    );
    println!(
        "  - {}: Filter nodes by status",
        "osvm node list --status <running|stopped|error|unknown>".cyan()
    );

    // Show additional tips for verbose mode
    if verbosity >= 2 {
        println!(
            "\n{} Try increasing verbosity with -v, -vv, or -vvv for more details",
            "ADVANCED:".magenta().bold()
        );
    }
}

/// Display detailed information about a node
///
/// # Arguments
/// * `node` - Node information
/// * `verbosity` - Verbosity level
pub fn display_node_info(node: &NodeInfo, verbosity: u8) {
    println!("\n{}", "OSVM - Node Management".cyan().bold());
    println!(
        "{} {}",
        "Node Information:".green().bold(),
        node.id.cyan().bold()
    );
    println!("{}", "====================".green());

    let node_type_display = match node.node_type.as_str() {
        "validator" => "Validator Node".cyan().bold(),
        "rpc" => "RPC Node".cyan().bold(),
        _ => node.node_type.cyan().bold(),
    };

    println!(
        "{} running on {} network",
        node_type_display,
        node.network.to_string().yellow().bold()
    );

    println!("\n{}", "General Information".blue().bold());
    println!("{}", "-------------------".blue());
    println!("  Name: {}", node.name.yellow());
    println!("  SVM Type: {}", node.svm_type.yellow());
    println!("  Network: {}", node.network.to_string().yellow().bold());

    let status_color = match node.status {
        NodeStatus::Running => format!("{} ●", node.status).green(),
        NodeStatus::Stopped => format!("{} ○", node.status).yellow(),
        NodeStatus::Error => format!("{} ✕", node.status).red(),
        NodeStatus::Unknown => format!("{} ?", node.status).bright_black(),
    };

    println!("  Status: {}", status_color);
    println!(
        "  Created: {}",
        chrono::DateTime::from_timestamp(node.created_at as i64, 0)
            .map(|dt| dt.format("%Y-%m-%d %H:%M:%S UTC").to_string())
            .unwrap_or_else(|| "Unknown".to_string())
    );

    println!("\n{}", "Connection Information".blue().bold());
    println!("{}", "----------------------".blue());
    println!("  Host: {}", node.host);
    println!(
        "  SSH Access: {}",
        format!("ssh user@{}", node.host).bright_white()
    );

    if let Some(rpc_url) = &node.rpc_url {
        println!("  RPC URL: {}", rpc_url.cyan().underline());
        println!("  Example: {}", format!("curl -X POST -H \"Content-Type: application/json\" -d '{{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"getHealth\"}}' {}", rpc_url).bright_black());
    }

    // Display system metrics if available
    if let Some(metrics) = &node.system_metrics {
        println!("\n{}", "System Metrics".blue().bold());
        println!("{}", "-------------".blue());
        println!("  CPU Usage: {}%", metrics.cpu_usage);
        println!("  RAM Usage: {}%", metrics.ram_usage);
        println!("  Disk Usage: {}%", metrics.disk_usage);
        println!(
            "  Network: {} kB/s in, {} kB/s out",
            metrics.network_in, metrics.network_out
        );
        println!(
            "  Last Updated: {}",
            chrono::DateTime::from_timestamp(metrics.last_updated as i64, 0)
                .map(|dt| dt.format("%Y-%m-%d %H:%M:%S").to_string())
                .unwrap_or_else(|| "Unknown".to_string())
        );
    }

    println!("\n{}", "Additional Information".blue().bold());
    println!("{}", "----------------------".blue());

    if node.additional_info.is_empty() {
        println!("  No additional information available");
    } else {
        for (key, value) in &node.additional_info {
            println!("  {}: {}", key, value);
        }
    }

    // Show more technical details at higher verbosity levels
    if verbosity >= 3 {
        println!("\n{}", "Technical Details".blue().bold());
        println!("{}", "----------------".blue());
        println!("  Created Timestamp: {}", node.created_at);
        println!(
            "  ID Format: {}-{}-{}-{}",
            node.svm_type, node.node_type, node.network, node.host
        );
    }

    println!("\n{}", "Management Commands".blue().bold());
    println!("{}", "------------------".blue());

    match node.status {
        NodeStatus::Running => {
            println!(
                "  {}: {}",
                "Stop Node".yellow(),
                format!("osvm node stop {}", node.id).cyan()
            );
            println!(
                "  {}: {}",
                "Restart Node".yellow(),
                format!("osvm node restart {}", node.id).cyan()
            );
        }
        NodeStatus::Stopped => {
            println!(
                "  {}: {}",
                "Start Node".green(),
                format!("osvm node start {}", node.id).cyan()
            );
        }
        NodeStatus::Error => {
            println!(
                "  {}: {}",
                "Start Node".green(),
                format!("osvm node start {}", node.id).cyan()
            );
            println!(
                "  {}: {}",
                "View Logs".yellow(),
                format!("osvm node logs {}", node.id).cyan()
            );
        }
        NodeStatus::Unknown => {
            println!(
                "  {}: {}",
                "Check Status".yellow(),
                format!("osvm node status {}", node.id).cyan()
            );
        }
    }

    println!(
        "  {}: {}",
        "View Logs".yellow(),
        format!("osvm node logs {}", node.id).cyan()
    );
    println!(
        "  {}: {}",
        "Remove Node".red(),
        format!("osvm node remove {}", node.id).cyan()
    );
}

/// Restart a node
///
/// # Arguments
/// * `client` - RPC client
/// * `node_id` - Node ID
///
/// # Returns
/// * `Result<(), Box<dyn Error>>` - Success/failure
pub fn restart_node(node_id: &str) -> Result<(), Box<dyn Error>> {
    // First stop the node
    stop_node(node_id)?;

    // Then start it
    start_node(node_id)?;

    Ok(())
}

/// Get node information
///
/// # Arguments
/// * `client` - RPC client
/// * `node_id` - Node ID
/// * `commitment_config` - Commitment config
///
/// # Returns
/// * `Result<NodeInfo, Box<dyn Error>>` - Node information or error
pub fn get_node_info(
    _client: &RpcClient,
    node_id: &str,
    _commitment_config: CommitmentConfig,
) -> Result<NodeInfo, Box<dyn Error>> {
    let db = NodeDatabase::load()?;
    let node = db.get_node(node_id)?;

    // Update the node status
    let current_status = get_node_status(node_id)?;

    // If status has changed, update the database
    if current_status != node.status {
        let mut node_update = node.clone();
        node_update.status = current_status;

        let mut db_update = db.clone();
        db_update.update_node(node_id, node_update.clone())?;

        Ok(node_update)
    } else {
        Ok(node)
    }
}

/// Get node logs
///
/// # Arguments
/// * `node_id` - Node ID
/// * `lines` - Number of lines to show
/// * `follow` - Whether to follow the logs
///
/// # Returns
/// * `Result<(), Box<dyn Error>>` - Success/failure
pub fn get_node_logs(node_id: &str, lines: usize, follow: bool) -> Result<(), Box<dyn Error>> {
    let db =
        NodeDatabase::load().map_err(|e| anyhow::anyhow!("Failed to load node database: {}", e))?;
    let node = db
        .get_node(node_id)
        .map_err(|e| anyhow::anyhow!("Failed to get node {}: {}", node_id, e))?;

    // Create SSH client
    let server_config = ServerConfig {
        host: node.host.clone(),
        port: 22, // Default SSH port
        auth: AuthMethod::Key {
            username: "root".to_string(),
            key_path: "~/.ssh/id_rsa".to_string(),
            passphrase: None,
        },
        install_dir: "/opt/osvm".to_string(),
    };

    let service_name = format!("{}-{}-{}", node.svm_type, node.node_type, node.network);

    // Create runtime for executing SSH client
    let rt =
        Runtime::new().map_err(|e| anyhow::anyhow!("Failed to create async runtime: {}", e))?;
    rt.block_on(async {
        use crate::utils::ssh_deploy::SshClient;

        let mut client = SshClient::new(server_config.clone())
            .map_err(|e| anyhow::anyhow!("Failed to create SSH client: {}", e))?;

        client
            .connect()
            .map_err(|e| anyhow::anyhow!("Failed to connect to {}: {}", node.host, e))?;

        let command = if follow {
            println!("Note: Log streaming is not fully supported. Displaying current logs...");
            format!("journalctl -u {} -n {}", service_name, lines)
        } else {
            format!("journalctl -u {} -n {}", service_name, lines)
        };

        if follow {
            // For follow mode, we need to stream the output
            client
                .stream_command(&command, |line| {
                    println!("{}", line);
                    true // Continue streaming
                })
                .map_err(|e| anyhow::anyhow!("Failed to stream logs: {}", e))?;
        } else {
            // For non-follow mode, just execute and print
            let logs = client
                .execute_command(&command)
                .map_err(|e| anyhow::anyhow!("Failed to execute log command: {}", e))?;
            println!("{}", logs);
        }

        Ok::<_, anyhow::Error>(())
    })?;

    Ok(())
}
/// Display status information for a node
///
/// # Arguments
/// * `node_id` - Node ID
/// * `status` - Node status
/// * `verbosity` - Verbosity level
pub fn display_node_status(node_id: &str, status: &NodeStatus, verbosity: u8) {
    println!("\n{}", "OSVM - Node Management".cyan().bold());
    println!("{} {}", "Node Status:".green().bold(), node_id.cyan());
    println!("{}", "==============".green());

    // Format the status with color and symbol
    let status_display = match status {
        NodeStatus::Running => format!("{} ●", status).green().bold(),
        NodeStatus::Stopped => format!("{} ○", status).yellow().bold(),
        NodeStatus::Error => format!("{} ✕", status).red().bold(),
        NodeStatus::Unknown => format!("{} ?", status).bright_black().bold(),
    };

    println!("\nCurrent status: {}", status_display);

    // Display available actions based on current status
    println!("\n{}", "Available Actions:".blue().bold());
    println!("{}", "----------------".blue());

    match status {
        NodeStatus::Running => {
            println!(
                "  {}: {}",
                "Stop Node".yellow(),
                format!("osvm node stop {}", node_id).cyan()
            );
            println!(
                "  {}: {}",
                "Restart Node".yellow(),
                format!("osvm node restart {}", node_id).cyan()
            );
        }
        NodeStatus::Stopped => {
            println!(
                "  {}: {}",
                "Start Node".green(),
                format!("osvm node start {}", node_id).cyan()
            );
        }
        NodeStatus::Error => {
            println!(
                "  {}: {}",
                "View Logs".red(),
                format!("osvm node logs {}", node_id).cyan()
            );
            println!(
                "  {}: {}",
                "Restart Node".yellow(),
                format!("osvm node restart {}", node_id).cyan()
            );
        }
        NodeStatus::Unknown => {}
    }

    // Show additional technical details at higher verbosity levels
    if verbosity >= 2 {
        println!("\n{}", "Technical Information:".blue().bold());
        println!("{}", "--------------------".blue());
        println!(
            "  Status check performed at: {}",
            chrono::Local::now().format("%Y-%m-%d %H:%M:%S")
        );
    }
}

/// Run the node monitoring dashboard
///
/// # Arguments
/// * `client` - RPC client
/// * `commitment_config` - Commitment config
/// * `verbosity` - Verbosity level
///
/// # Returns
/// * `Result<(), Box<dyn Error>>` - Success/failure
pub fn run_dashboard(
    client: &RpcClient,
    commitment_config: CommitmentConfig,
    verbosity: u8,
) -> Result<(), Box<dyn Error>> {
    println!("{}", "OSVM - Node Monitoring Dashboard".cyan().bold());
    println!("{}", "===============================".cyan());
    println!("\n{}", "Loading node information...".yellow());

    // Get all nodes
    let nodes = list_all_nodes(
        client,
        "all",
        None,
        "all",
        "all",
        commitment_config,
        verbosity,
    )?;

    crate::utils::dashboard::quick_dashboard(&nodes, verbosity)?;
    // In a real implementation, this would launch an interactive dashboard
    println!(
        "\n{}",
        "Interactive dashboard functionality is in development.".bright_black()
    );

    Ok(())
}

/// Extension trait for SshClient to add streaming capability
trait SshClientExt {
    /// Stream a command, processing each line as it's received
    fn stream_command<F>(&mut self, command: &str, callback: F) -> Result<(), Box<dyn Error>>
    where
        F: FnMut(&str) -> bool;
}

impl SshClientExt for crate::utils::ssh_deploy::SshClient {
    fn stream_command<F>(&mut self, command: &str, mut callback: F) -> Result<(), Box<dyn Error>>
    where
        F: FnMut(&str) -> bool,
    {
        // First try to execute the command
        let output = self.execute_command(command)?;

        // Process each line of the output
        let mut _continue_processing = true;
        for line in output.lines() {
            _continue_processing = callback(line);
            if !_continue_processing {
                break;
            }
        }

        Ok(())
    }
}
