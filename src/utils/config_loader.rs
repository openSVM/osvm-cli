//! Solana CLI config file loader
//!
//! This module replaces solana-cli-config to eliminate the clap 2.x dependency chain.
//! It provides a minimal implementation for loading ~/.config/solana/cli/config.yml
//!
//! Note: OSVM uses its own config at ~/.config/osvm/ but can fall back to Solana's config.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

lazy_static::lazy_static! {
    /// Default config file path (~/.config/solana/cli/config.yml)
    pub static ref CONFIG_FILE: Option<String> = {
        dirs::home_dir().map(|home| {
            home.join(".config/solana/cli/config.yml")
                .to_string_lossy()
                .to_string()
        })
    };

    /// OSVM-specific config file path (~/.config/osvm/config.yml)
    pub static ref OSVM_CONFIG_FILE: Option<String> = {
        dirs::home_dir().map(|home| {
            home.join(".config/osvm/config.yml")
                .to_string_lossy()
                .to_string()
        })
    };
}

/// Solana CLI configuration
///
/// Compatible with the format used by `solana config get/set`
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// RPC endpoint URL
    #[serde(default = "default_json_rpc_url")]
    pub json_rpc_url: String,

    /// WebSocket URL (optional, derived from RPC URL if empty)
    #[serde(default)]
    pub websocket_url: String,

    /// Path to the keypair file
    #[serde(default = "default_keypair_path")]
    pub keypair_path: String,

    /// Human-readable labels for addresses
    #[serde(default)]
    pub address_labels: HashMap<String, String>,

    /// Default commitment level
    #[serde(default = "default_commitment")]
    pub commitment: String,
}

fn default_json_rpc_url() -> String {
    "https://api.devnet.solana.com".to_string()
}

fn default_keypair_path() -> String {
    dirs::home_dir()
        .map(|home| {
            home.join(".config/solana/id.json")
                .to_string_lossy()
                .to_string()
        })
        .unwrap_or_else(|| "~/.config/solana/id.json".to_string())
}

fn default_commitment() -> String {
    "confirmed".to_string()
}

impl Default for Config {
    fn default() -> Self {
        Self {
            json_rpc_url: default_json_rpc_url(),
            websocket_url: String::new(),
            keypair_path: default_keypair_path(),
            address_labels: HashMap::new(),
            commitment: default_commitment(),
        }
    }
}

impl Config {
    /// Load config from a file path
    ///
    /// Returns default config if file doesn't exist or can't be parsed.
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self, std::io::Error> {
        let path = path.as_ref();

        // Expand ~ in path
        let expanded_path = if path.starts_with("~") {
            if let Some(home) = dirs::home_dir() {
                home.join(path.strip_prefix("~").unwrap_or(path))
            } else {
                path.to_path_buf()
            }
        } else {
            path.to_path_buf()
        };

        if !expanded_path.exists() {
            return Ok(Self::default());
        }

        let content = fs::read_to_string(&expanded_path)?;
        serde_yaml::from_str(&content)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))
    }

    /// Save config to a file path
    pub fn save<P: AsRef<Path>>(&self, path: P) -> Result<(), std::io::Error> {
        let path = path.as_ref();

        // Create parent directories if needed
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }

        let content = serde_yaml::to_string(self)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))?;

        fs::write(path, content)
    }

    /// Compute the websocket URL from the RPC URL if not explicitly set
    pub fn websocket_url(&self) -> String {
        if !self.websocket_url.is_empty() {
            return self.websocket_url.clone();
        }

        // Derive from RPC URL: https -> wss, http -> ws
        self.json_rpc_url
            .replace("https://", "wss://")
            .replace("http://", "ws://")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert!(config.json_rpc_url.contains("solana.com"));
        assert_eq!(config.commitment, "confirmed");
    }

    #[test]
    fn test_websocket_derivation() {
        let config = Config {
            json_rpc_url: "https://api.mainnet-beta.solana.com".to_string(),
            websocket_url: String::new(),
            ..Default::default()
        };
        assert_eq!(config.websocket_url(), "wss://api.mainnet-beta.solana.com");
    }

    #[test]
    fn test_load_nonexistent() {
        let config = Config::load("/nonexistent/path/config.yml").unwrap();
        assert_eq!(config.json_rpc_url, default_json_rpc_url());
    }

    #[test]
    fn test_serde_roundtrip() {
        let config = Config::default();
        let yaml = serde_yaml::to_string(&config).unwrap();
        let parsed: Config = serde_yaml::from_str(&yaml).unwrap();
        assert_eq!(config.json_rpc_url, parsed.json_rpc_url);
        assert_eq!(config.commitment, parsed.commitment);
    }
}
