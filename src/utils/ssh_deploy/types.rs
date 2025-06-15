//! Type definitions for SSH deployment

use {
    crate::utils::ssh_deploy::errors::DeploymentError,
    serde::{Deserialize, Serialize},
    std::{fmt, str::FromStr},
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
            return Err(DeploymentError::InvalidConfig(format!(
                "Invalid connection string: {}",
                conn_str
            )));
        }

        let username = parts[0].to_string();
        let host_parts: Vec<&str> = parts[1].split(':').collect();
        let host = host_parts[0].to_string();
        let port = if host_parts.len() > 1 {
            host_parts[1].parse().unwrap_or(22)
        } else {
            22
        };

        Ok(ServerConfig {
            host,
            port,
            auth: AuthMethod::Key {
                username,
                key_path: "~/.ssh/id_rsa".to_string(),
                passphrase: None,
            },
            install_dir: "/opt/osvm".to_string(),
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

    // New fields for enhanced Solana validator configuration
    /// Solana client version
    pub version: Option<String>,
    /// Solana client type (standard, jito, agave, firedancer, sig)
    pub client_type: Option<String>,
    /// Enable hot-swap capability
    pub hot_swap_enabled: bool,
    /// Metrics configuration
    pub metrics_config: Option<String>,
    /// Disk configuration for dedicated storage
    pub disk_config: Option<DiskConfig>,
}

/// Disk configuration for Solana validator/RPC
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiskConfig {
    /// Ledger disk device path (e.g., "/dev/nvme0n1")
    pub ledger_disk: String,
    /// Accounts disk device path (e.g., "/dev/nvme1n1")
    pub accounts_disk: String,
}
