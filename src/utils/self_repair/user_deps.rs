//! User-level dependency management
//!
//! This module handles user-level dependencies such as Solana CLI configuration,
//! keypair files, and user-specific settings.

use dirs::home_dir;
use serde::{Deserialize, Serialize};
use std::error::Error as StdError;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// User dependency error
#[derive(Debug)]
pub enum UserDepsError {
    FileSystemError(String),
    CommandFailed(String),
    ConfigError(String),
    KeypairError(String),
    PermissionError(String),
}

impl fmt::Display for UserDepsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UserDepsError::FileSystemError(msg) => write!(f, "File system error: {}", msg),
            UserDepsError::CommandFailed(msg) => write!(f, "Command failed: {}", msg),
            UserDepsError::ConfigError(msg) => write!(f, "Configuration error: {}", msg),
            UserDepsError::KeypairError(msg) => write!(f, "Keypair error: {}", msg),
            UserDepsError::PermissionError(msg) => write!(f, "Permission error: {}", msg),
        }
    }
}

impl StdError for UserDepsError {}

/// Solana configuration information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SolanaConfigInfo {
    pub cli_installed: bool,
    pub cli_version: Option<String>,
    pub config_dir_exists: bool,
    pub config_file_exists: bool,
    pub keypair_exists: bool,
    pub keypair_path: Option<String>,
    pub current_network: Option<String>,
}

/// User dependency manager
#[derive(Debug)]
pub struct UserDependencyManager;

impl UserDependencyManager {
    /// Create a new user dependency manager
    pub fn new() -> Self {
        Self
    }

    /// Check all user-level dependencies
    pub async fn check_all_user_dependencies(&self) -> Result<SolanaConfigInfo, UserDepsError> {
        let cli_installed = self.is_solana_cli_installed().await?;
        let cli_version = if cli_installed {
            self.get_solana_cli_version().await?
        } else {
            None
        };

        let config_dir = self.get_solana_config_dir();
        let config_dir_exists = config_dir.exists();

        let config_file = config_dir.join("cli").join("config.yml");
        let config_file_exists = config_file.exists();

        let default_keypair_path = config_dir.join("id.json");
        let keypair_path = if config_file_exists {
            self.get_keypair_path_from_config(&config_file)
                .await
                .unwrap_or_else(|_| default_keypair_path.to_string_lossy().to_string())
        } else {
            default_keypair_path.to_string_lossy().to_string()
        };

        let keypair_exists = Path::new(&keypair_path).exists();
        let current_network = if cli_installed {
            self.get_current_network().await?
        } else {
            None
        };

        Ok(SolanaConfigInfo {
            cli_installed,
            cli_version,
            config_dir_exists,
            config_file_exists,
            keypair_exists,
            keypair_path: Some(keypair_path),
            current_network,
        })
    }

    /// Check if Solana CLI is installed
    pub async fn is_solana_cli_installed(&self) -> Result<bool, UserDepsError> {
        match Command::new("solana").arg("--version").output() {
            Ok(output) => Ok(output.status.success()),
            Err(_) => Ok(false),
        }
    }

    /// Get Solana CLI version
    async fn get_solana_cli_version(&self) -> Result<Option<String>, UserDepsError> {
        match Command::new("solana").arg("--version").output() {
            Ok(output) if output.status.success() => {
                let version_str = String::from_utf8_lossy(&output.stdout);
                if let Some(version) = version_str.split_whitespace().nth(1) {
                    Ok(Some(version.to_string()))
                } else {
                    Ok(Some("unknown".to_string()))
                }
            }
            _ => Ok(None),
        }
    }

    /// Get Solana configuration directory
    pub fn get_solana_config_dir(&self) -> PathBuf {
        if let Some(home) = home_dir() {
            home.join(".config").join("solana")
        } else {
            PathBuf::from(".config/solana")
        }
    }

    /// Get OSVM configuration directory
    pub fn get_osvm_config_dir(&self) -> PathBuf {
        if let Some(home) = home_dir() {
            home.join(".config").join("osvm")
        } else {
            PathBuf::from(".config/osvm")
        }
    }

    /// Create Solana configuration directory
    pub async fn create_solana_config_dir(&self) -> Result<String, UserDepsError> {
        let config_dir = self.get_solana_config_dir();

        if config_dir.exists() {
            return Ok("Solana config directory already exists".to_string());
        }

        fs::create_dir_all(&config_dir).map_err(|e| {
            UserDepsError::FileSystemError(format!(
                "Failed to create config directory {}: {}",
                config_dir.display(),
                e
            ))
        })?;

        // Also create the CLI subdirectory
        let cli_dir = config_dir.join("cli");
        fs::create_dir_all(&cli_dir).map_err(|e| {
            UserDepsError::FileSystemError(format!(
                "Failed to create CLI config directory {}: {}",
                cli_dir.display(),
                e
            ))
        })?;

        Ok(format!(
            "Created Solana config directory: {}",
            config_dir.display()
        ))
    }

    /// Generate a new Solana keypair
    pub async fn generate_keypair(
        &self,
        output_path: Option<&str>,
    ) -> Result<String, UserDepsError> {
        let keypair_path = if let Some(path) = output_path {
            PathBuf::from(path)
        } else {
            self.get_solana_config_dir().join("id.json")
        };

        // Ensure the parent directory exists
        if let Some(parent) = keypair_path.parent() {
            if !parent.exists() {
                fs::create_dir_all(parent).map_err(|e| {
                    UserDepsError::FileSystemError(format!(
                        "Failed to create keypair directory {}: {}",
                        parent.display(),
                        e
                    ))
                })?;
            }
        }

        // Check if keypair already exists
        if keypair_path.exists() {
            return Err(UserDepsError::KeypairError(format!(
                "Keypair already exists at {}",
                keypair_path.display()
            )));
        }

        // Generate the keypair
        let output = Command::new("solana-keygen")
            .arg("new")
            .arg("-o")
            .arg(&keypair_path)
            .arg("--no-passphrase")
            .output()
            .map_err(|e| {
                UserDepsError::CommandFailed(format!("Failed to execute solana-keygen: {}", e))
            })?;

        if output.status.success() {
            let pubkey = self
                .get_pubkey_from_keypair(&keypair_path)
                .await
                .unwrap_or_else(|_| "unknown".to_string());

            Ok(format!(
                "Generated new keypair: {} (pubkey: {})",
                keypair_path.display(),
                pubkey
            ))
        } else {
            let error_msg = String::from_utf8_lossy(&output.stderr);
            Err(UserDepsError::KeypairError(format!(
                "Failed to generate keypair: {}",
                error_msg
            )))
        }
    }

    /// Get public key from a keypair file
    async fn get_pubkey_from_keypair(&self, keypair_path: &Path) -> Result<String, UserDepsError> {
        let output = Command::new("solana-keygen")
            .arg("pubkey")
            .arg(keypair_path)
            .output()
            .map_err(|e| {
                UserDepsError::CommandFailed(format!("Failed to get pubkey from keypair: {}", e))
            })?;

        if output.status.success() {
            let pubkey = String::from_utf8_lossy(&output.stdout);
            Ok(pubkey.trim().to_string())
        } else {
            Err(UserDepsError::KeypairError(
                "Failed to extract public key from keypair".to_string(),
            ))
        }
    }

    /// Set Solana configuration
    pub async fn configure_solana_network(&self, network: &str) -> Result<String, UserDepsError> {
        let rpc_url = match network.to_lowercase().as_str() {
            "mainnet" | "mainnet-beta" => "https://api.mainnet-beta.solana.com",
            "testnet" => "https://api.testnet.solana.com",
            "devnet" => "https://api.devnet.solana.com",
            _ => {
                return Err(UserDepsError::ConfigError(format!(
                    "Unknown network: {}",
                    network
                )))
            }
        };

        let output = Command::new("solana")
            .arg("config")
            .arg("set")
            .arg("--url")
            .arg(rpc_url)
            .output()
            .map_err(|e| {
                UserDepsError::CommandFailed(format!("Failed to set Solana network: {}", e))
            })?;

        if output.status.success() {
            Ok(format!("Set Solana network to: {}", network))
        } else {
            let error_msg = String::from_utf8_lossy(&output.stderr);
            Err(UserDepsError::ConfigError(format!(
                "Failed to set network: {}",
                error_msg
            )))
        }
    }

    /// Get current Solana network configuration
    async fn get_current_network(&self) -> Result<Option<String>, UserDepsError> {
        match Command::new("solana").arg("config").arg("get").output() {
            Ok(output) if output.status.success() => {
                let config_str = String::from_utf8_lossy(&output.stdout);
                // Parse the config output to extract the RPC URL
                for line in config_str.lines() {
                    if line.contains("RPC URL:") {
                        if line.contains("mainnet-beta") {
                            return Ok(Some("mainnet".to_string()));
                        } else if line.contains("testnet") {
                            return Ok(Some("testnet".to_string()));
                        } else if line.contains("devnet") {
                            return Ok(Some("devnet".to_string()));
                        }
                    }
                }
                Ok(Some("unknown".to_string()))
            }
            _ => Ok(None),
        }
    }

    /// Get keypair path from Solana config file
    async fn get_keypair_path_from_config(
        &self,
        config_file: &Path,
    ) -> Result<String, UserDepsError> {
        let config_content = fs::read_to_string(config_file).map_err(|e| {
            UserDepsError::ConfigError(format!(
                "Failed to read config file {}: {}",
                config_file.display(),
                e
            ))
        })?;

        // Parse YAML config to extract keypair path
        if let Ok(config) = serde_yaml::from_str::<serde_yaml::Value>(&config_content) {
            if let Some(keypair_path) = config.get("keypair_path").and_then(|v| v.as_str()) {
                return Ok(keypair_path.to_string());
            }
        }

        // Fallback to default path
        Ok(self
            .get_solana_config_dir()
            .join("id.json")
            .to_string_lossy()
            .to_string())
    }

    /// Set keypair path in Solana configuration
    pub async fn set_keypair_path(&self, keypair_path: &str) -> Result<String, UserDepsError> {
        let output = Command::new("solana")
            .arg("config")
            .arg("set")
            .arg("--keypair")
            .arg(keypair_path)
            .output()
            .map_err(|e| {
                UserDepsError::CommandFailed(format!("Failed to set keypair path: {}", e))
            })?;

        if output.status.success() {
            Ok(format!("Set keypair path to: {}", keypair_path))
        } else {
            let error_msg = String::from_utf8_lossy(&output.stderr);
            Err(UserDepsError::ConfigError(format!(
                "Failed to set keypair path: {}",
                error_msg
            )))
        }
    }

    /// Install Solana CLI using the official installer
    pub async fn install_solana_cli(&self) -> Result<String, UserDepsError> {
        println!("🌐 Installing Solana CLI...");

        let install_script = "curl --proto '=https' --tlsv1.2 -sSfL https://solana-install.solana.workers.dev | bash";

        let output = Command::new("sh")
            .arg("-c")
            .arg(install_script)
            .output()
            .map_err(|e| {
                UserDepsError::CommandFailed(format!("Failed to download Solana installer: {}", e))
            })?;

        if !output.status.success() {
            let error_msg = String::from_utf8_lossy(&output.stderr);
            return Err(UserDepsError::CommandFailed(format!(
                "Solana installation failed: {}",
                error_msg
            )));
        }

        // Update PATH for current session
        let path_update = format!(
            "export PATH=\"{}/.local/share/solana/install/active_release/bin:$PATH\"",
            home_dir().unwrap_or_default().display()
        );

        let _ = Command::new("sh").arg("-c").arg(&path_update).output();

        // Verify installation
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        if self.is_solana_cli_installed().await? {
            Ok("Solana CLI installed successfully".to_string())
        } else {
            Err(UserDepsError::CommandFailed(
                "Solana CLI installation verification failed. Please restart your terminal."
                    .to_string(),
            ))
        }
    }

    /// Create a complete Solana setup (config directory + keypair + network config)
    pub async fn setup_solana_complete(
        &self,
        network: Option<&str>,
    ) -> Result<Vec<String>, UserDepsError> {
        let mut results = Vec::new();

        // 1. Create config directory
        results.push(self.create_solana_config_dir().await?);

        // 2. Generate keypair if it doesn't exist
        let config_info = self.check_all_user_dependencies().await?;
        if !config_info.keypair_exists {
            results.push(self.generate_keypair(None).await?);
        }

        // 3. Set network configuration
        let target_network = network.unwrap_or("mainnet");
        results.push(self.configure_solana_network(target_network).await?);

        // 4. Set keypair path in config
        if let Some(keypair_path) = config_info.keypair_path {
            results.push(self.set_keypair_path(&keypair_path).await?);
        }

        Ok(results)
    }

    /// Validate existing Solana configuration
    pub async fn validate_solana_config(&self) -> Result<Vec<String>, UserDepsError> {
        let mut issues = Vec::new();
        let config_info = self.check_all_user_dependencies().await?;

        if !config_info.cli_installed {
            issues.push("Solana CLI is not installed".to_string());
        }

        if !config_info.config_dir_exists {
            issues.push("Solana configuration directory does not exist".to_string());
        }

        if !config_info.keypair_exists {
            issues.push("Solana keypair file does not exist".to_string());
        }

        if config_info.current_network.is_none() {
            issues.push("Solana network is not configured".to_string());
        }

        Ok(issues)
    }
}

/// Check if config directory exists
pub async fn check_config_directory() -> Result<bool, UserDepsError> {
    let manager = UserDependencyManager::new();
    let config_dir = manager.get_solana_config_dir();
    Ok(config_dir.exists())
}

/// Get the default keypair path
pub fn get_default_keypair_path() -> Result<String, UserDepsError> {
    let manager = UserDependencyManager::new();
    let keypair_path = manager.get_solana_config_dir().join("id.json");
    Ok(keypair_path.to_string_lossy().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_user_dependency_manager_creation() {
        let manager = UserDependencyManager::new();
        let config_dir = manager.get_solana_config_dir();
        assert!(config_dir.to_string_lossy().contains("solana"));
    }

    #[tokio::test]
    async fn test_config_directory_paths() {
        let manager = UserDependencyManager::new();

        let solana_dir = manager.get_solana_config_dir();
        let osvm_dir = manager.get_osvm_config_dir();

        assert!(solana_dir.to_string_lossy().contains("solana"));
        assert!(osvm_dir.to_string_lossy().contains("osvm"));
    }

    #[test]
    fn test_default_keypair_path() {
        let path = get_default_keypair_path().expect("Failed to get default keypair path");
        assert!(path.contains("id.json"));
    }

    #[tokio::test]
    async fn test_solana_cli_check() {
        let manager = UserDependencyManager::new();
        let is_installed = manager
            .is_solana_cli_installed()
            .await
            .expect("Failed to check Solana CLI");

        // This test depends on the environment
        println!("Solana CLI installed: {}", is_installed);
    }

    #[tokio::test]
    async fn test_config_validation() {
        let manager = UserDependencyManager::new();
        let issues = manager
            .validate_solana_config()
            .await
            .expect("Failed to validate config");

        println!("Configuration issues: {:?}", issues);
    }
}
