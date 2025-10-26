//! Isolation configuration for MCP tools and unikernel execution
//!
//! This module defines the configuration structure for controlling how MCP tools
//! are executed - either directly in microVMs or in isolated unikernels.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Execution mode for MCP tools
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
#[derive(Default)]
pub enum ExecutionMode {
    /// Execute tool in ephemeral unikernel (high security, ~100ms overhead)
    Unikernel,
    /// Execute tool directly in microVM (Firecracker - preferred approach)
    #[default]
    MicroVM,
}

/// Mount configuration for filesystem access
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MountConfig {
    /// Path on the host system
    pub host_path: String,
    /// Path inside the VM/unikernel
    pub vm_path: String,
    /// Whether the mount is read-only
    #[serde(default)]
    pub readonly: bool,
}

/// Configuration for a specific MCP tool
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolConfig {
    /// How this tool should be executed
    #[serde(default)]
    pub execution_mode: ExecutionMode,
    /// Path to the unikernel image (if execution_mode is Unikernel)
    pub unikernel_image: Option<String>,
    /// Filesystem mounts available to this tool
    #[serde(default)]
    pub mounts: Vec<MountConfig>,
    /// Memory limit in MB for unikernel
    #[serde(default = "default_memory_mb")]
    pub memory_mb: u32,
    /// Number of vCPUs for unikernel
    #[serde(default = "default_vcpus")]
    pub vcpus: u32,
}

fn default_memory_mb() -> u32 {
    128
}

fn default_vcpus() -> u32 {
    1
}

impl Default for ToolConfig {
    fn default() -> Self {
        Self {
            execution_mode: ExecutionMode::MicroVM,  // Use Firecracker MicroVM by default
            unikernel_image: None,
            mounts: Vec::new(),
            memory_mb: default_memory_mb(),
            vcpus: default_vcpus(),
        }
    }
}

/// Resource configuration for MCP server microVMs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MicroVmResourceConfig {
    /// Memory allocation in MB
    #[serde(default = "default_server_memory_mb")]
    pub memory_mb: u32,
    /// Number of vCPUs
    #[serde(default = "default_server_vcpus")]
    pub vcpus: u32,
}

fn default_server_memory_mb() -> u32 {
    256
}

fn default_server_vcpus() -> u32 {
    1
}

impl Default for MicroVmResourceConfig {
    fn default() -> Self {
        Self {
            memory_mb: default_server_memory_mb(),
            vcpus: default_server_vcpus(),
        }
    }
}

/// Configuration for an MCP server
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerConfig {
    /// MicroVM ID where this server runs
    pub microvm_id: String,
    /// Whether to run this server in a dedicated microVM (Phase 3.2)
    #[serde(default)]
    pub use_microvm: bool,
    /// Resource configuration for the server's microVM (if use_microvm is true)
    #[serde(default)]
    pub microvm_config: MicroVmResourceConfig,
    /// Command to run the MCP server (used when use_microvm is true)
    #[serde(default)]
    pub server_command: Option<String>,
    /// Mounts available to the microVM (server level)
    #[serde(default)]
    pub microvm_mounts: Vec<MountConfig>,
    /// Per-tool configuration
    #[serde(default)]
    pub tools: HashMap<String, ToolConfig>,
}

/// Root isolation configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsolationConfig {
    /// Default execution mode for tools without specific config
    #[serde(default)]
    pub default_execution_mode: ExecutionMode,
    /// Directory where unikernel images are stored
    #[serde(default = "default_unikernel_dir")]
    pub unikernel_dir: String,
    /// Per-server configuration
    #[serde(default)]
    pub mcp_servers: HashMap<String, ServerConfig>,
}

fn default_unikernel_dir() -> String {
    "/var/osvm/unikernels".to_string()
}

impl Default for IsolationConfig {
    fn default() -> Self {
        Self {
            default_execution_mode: ExecutionMode::MicroVM,  // Use Firecracker MicroVM by default
            unikernel_dir: default_unikernel_dir(),
            mcp_servers: HashMap::new(),
        }
    }
}

impl IsolationConfig {
    /// Load isolation configuration from file
    pub fn load() -> Result<Self> {
        let config_path = Self::get_config_path()?;

        if !config_path.exists() {
            // Create default config if it doesn't exist
            let default_config = Self::create_default_config();
            default_config.save()?;
            return Ok(default_config);
        }

        let content = std::fs::read_to_string(&config_path)
            .context("Failed to read isolation config file")?;

        let config: IsolationConfig =
            serde_json::from_str(&content).context("Failed to parse isolation config")?;

        Ok(config)
    }

    /// Save isolation configuration to file
    pub fn save(&self) -> Result<()> {
        let config_path = Self::get_config_path()?;

        // Create parent directory if it doesn't exist
        if let Some(parent) = config_path.parent() {
            std::fs::create_dir_all(parent).context("Failed to create config directory")?;
        }

        let json =
            serde_json::to_string_pretty(self).context("Failed to serialize isolation config")?;

        std::fs::write(&config_path, json).context("Failed to write isolation config")?;

        Ok(())
    }

    /// Get the path to the isolation config file
    fn get_config_path() -> Result<PathBuf> {
        let home = std::env::var("HOME").context("HOME environment variable not set")?;

        Ok(PathBuf::from(home)
            .join(".config")
            .join("osvm")
            .join("isolation_config.json"))
    }

    /// Create a default configuration with example entries
    fn create_default_config() -> Self {
        let mut config = Self::default();

        // Example Solana MCP server configuration
        let mut solana_tools = HashMap::new();

        solana_tools.insert(
            "get_balance".to_string(),
            ToolConfig {
                execution_mode: ExecutionMode::Unikernel,
                unikernel_image: Some("solana-get_balance.img".to_string()),
                mounts: vec![MountConfig {
                    host_path: "~/.config/solana".to_string(),
                    vm_path: "/data/solana-config".to_string(),
                    readonly: true,
                }],
                memory_mb: 128,
                vcpus: 1,
            },
        );

        solana_tools.insert(
            "transfer".to_string(),
            ToolConfig {
                execution_mode: ExecutionMode::Unikernel,
                unikernel_image: Some("solana-transfer.img".to_string()),
                mounts: vec![
                    MountConfig {
                        host_path: "~/.config/solana".to_string(),
                        vm_path: "/data/solana-config".to_string(),
                        readonly: true,
                    },
                    MountConfig {
                        host_path: "~/Documents/keypairs".to_string(),
                        vm_path: "/data/keypairs".to_string(),
                        readonly: false,
                    },
                ],
                memory_mb: 128,
                vcpus: 1,
            },
        );

        config.mcp_servers.insert(
            "solana".to_string(),
            ServerConfig {
                microvm_id: "μVM-1".to_string(),
                use_microvm: true,
                microvm_config: MicroVmResourceConfig {
                    memory_mb: 512,
                    vcpus: 2,
                },
                server_command: Some("npx -y @modelcontextprotocol/server-solana".to_string()),
                microvm_mounts: vec![MountConfig {
                    host_path: "~/.config/osvm".to_string(),
                    vm_path: "/mnt/osvm-config".to_string(),
                    readonly: true,
                }],
                tools: solana_tools,
            },
        );

        // Example GitHub MCP server (less sensitive, runs in microVM)
        let mut github_tools = HashMap::new();
        github_tools.insert(
            "create_issue".to_string(),
            ToolConfig {
                execution_mode: ExecutionMode::MicroVM,
                unikernel_image: None,
                mounts: vec![],
                memory_mb: 128,
                vcpus: 1,
            },
        );

        config.mcp_servers.insert(
            "github".to_string(),
            ServerConfig {
                microvm_id: "μVM-2".to_string(),
                use_microvm: true,
                microvm_config: MicroVmResourceConfig {
                    memory_mb: 256,
                    vcpus: 1,
                },
                server_command: Some("npx -y @modelcontextprotocol/server-github".to_string()),
                microvm_mounts: vec![],
                tools: github_tools,
            },
        );

        config
    }

    /// Get configuration for a specific tool
    pub fn get_tool_config(&self, server_id: &str, tool_name: &str) -> ToolConfig {
        self.mcp_servers
            .get(server_id)
            .and_then(|server| server.tools.get(tool_name))
            .cloned()
            .unwrap_or_else(|| ToolConfig {
                execution_mode: self.default_execution_mode.clone(),
                ..Default::default()
            })
    }

    /// Get server configuration
    pub fn get_server_config(&self, server_id: &str) -> Option<&ServerConfig> {
        self.mcp_servers.get(server_id)
    }

    /// Check if a tool should use unikernel execution
    pub fn should_use_unikernel(&self, server_id: &str, tool_name: &str) -> bool {
        let tool_config = self.get_tool_config(server_id, tool_name);
        tool_config.execution_mode == ExecutionMode::Unikernel
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = IsolationConfig::create_default_config();
        assert_eq!(config.default_execution_mode, ExecutionMode::Unikernel);
        assert!(config.mcp_servers.contains_key("solana"));
        assert!(config.mcp_servers.contains_key("github"));
    }

    #[test]
    fn test_tool_config_lookup() {
        let config = IsolationConfig::create_default_config();

        let balance_config = config.get_tool_config("solana", "get_balance");
        assert_eq!(balance_config.execution_mode, ExecutionMode::Unikernel);
        assert_eq!(balance_config.mounts.len(), 1);

        let github_config = config.get_tool_config("github", "create_issue");
        assert_eq!(github_config.execution_mode, ExecutionMode::MicroVM);
    }

    #[test]
    fn test_should_use_unikernel() {
        let config = IsolationConfig::create_default_config();

        assert!(config.should_use_unikernel("solana", "get_balance"));
        assert!(!config.should_use_unikernel("github", "create_issue"));
    }
}
