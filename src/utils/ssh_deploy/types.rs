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
    /// Connection timeout in seconds (default: 30)
    #[serde(default = "default_connection_timeout")]
    pub connection_timeout_secs: u64,
}

/// Default connection timeout (30 seconds)
fn default_connection_timeout() -> u64 {
    30
}

impl Default for ServerConfig {
    fn default() -> Self {
        ServerConfig {
            host: "localhost".to_string(),
            port: 22,
            auth: AuthMethod::Key {
                username: "ubuntu".to_string(),
                key_path: "~/.ssh/id_rsa".to_string(),
                passphrase: None,
            },
            install_dir: "/opt/osvm".to_string(),
            connection_timeout_secs: default_connection_timeout(),
        }
    }
}

impl ServerConfig {
    /// Detect available SSH key automatically
    ///
    /// # Returns
    /// * `String` - Path to first available SSH key
    fn detect_ssh_key() -> String {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/root".to_string());

        // Try common SSH key types in order of preference (modern first)
        let key_types = vec![
            "id_ed25519", // Ed25519 (modern, recommended)
            "id_ecdsa",   // ECDSA
            "id_rsa",     // RSA (legacy but common)
            "id_dsa",     // DSA (deprecated)
        ];

        for key_type in key_types {
            let key_path = format!("{}/.ssh/{}", home, key_type);
            if std::path::Path::new(&key_path).exists() {
                return format!("~/.ssh/{}", key_type);
            }
        }

        // Default fallback to id_rsa (will fail if not present, but provides clear error)
        "~/.ssh/id_rsa".to_string()
    }

    /// Create a ServerConfig from a connection string (`user@host[:port]`)
    ///
    /// # Arguments
    /// * `conn_str` - Connection string in the format `user@host[:port]`
    ///
    /// # Returns
    /// * `Result<ServerConfig, DeploymentError>` - Server config or error
    pub fn from_connection_string(conn_str: &str) -> Result<Self, DeploymentError> {
        // Validate input is not empty
        if conn_str.trim().is_empty() {
            return Err(DeploymentError::InvalidConfig(
                "Connection string cannot be empty".to_string(),
            ));
        }

        let parts: Vec<&str> = conn_str.split('@').collect();
        if parts.len() != 2 {
            return Err(DeploymentError::InvalidConfig(format!(
                "Invalid connection string format. Expected 'user@host[:port]', got: {}",
                conn_str
            )));
        }

        // Validate and sanitize username
        let username = parts[0].trim().to_string();
        Self::validate_username(&username)?;

        let host_parts: Vec<&str> = parts[1].split(':').collect();
        let host = host_parts[0].trim().to_string();

        // Validate host before proceeding
        if host.is_empty() {
            return Err(DeploymentError::InvalidConfig(
                "Host cannot be empty in connection string".to_string(),
            ));
        }

        // Parse port with proper error handling
        let port = if host_parts.len() > 1 {
            host_parts[1].trim().parse::<u16>().map_err(|_| {
                DeploymentError::InvalidConfig(format!(
                    "Invalid port number '{}'. Port must be a number between 1 and 65535",
                    host_parts[1]
                ))
            })?
        } else {
            22
        };

        Ok(ServerConfig {
            host,
            port,
            auth: AuthMethod::Key {
                username,
                key_path: Self::detect_ssh_key(),
                passphrase: None,
            },
            install_dir: "/opt/osvm".to_string(),
            connection_timeout_secs: default_connection_timeout(),
        })
    }

    /// Validate username to prevent injection attacks
    fn validate_username(username: &str) -> Result<(), DeploymentError> {
        // Check for empty username
        if username.is_empty() {
            return Err(DeploymentError::ValidationError(
                "Username cannot be empty".to_string(),
            ));
        }

        // Check length (Unix usernames typically max 32 chars)
        if username.len() > 32 {
            return Err(DeploymentError::ValidationError(
                "Username too long (max 32 characters)".to_string(),
            ));
        }

        // Check for dangerous characters
        if username.contains(|c: char| {
            matches!(
                c,
                ';' | '&' | '|' | '$' | '`' | '\n' | '\r' | '\0' | '(' | ')' | '<' | '>'
            )
        }) {
            return Err(DeploymentError::ValidationError(
                "Username contains invalid characters".to_string(),
            ));
        }

        // Ensure username starts with letter or underscore (Unix convention)
        if let Some(first_char) = username.chars().next() {
            if !first_char.is_alphabetic() && first_char != '_' {
                return Err(DeploymentError::ValidationError(
                    "Username must start with a letter or underscore".to_string(),
                ));
            }
        }

        // Check that username only contains valid characters (alphanumeric, underscore, hyphen)
        if !username
            .chars()
            .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-'))
        {
            return Err(DeploymentError::ValidationError(
                "Username can only contain letters, numbers, underscores, and hyphens".to_string(),
            ));
        }

        Ok(())
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
