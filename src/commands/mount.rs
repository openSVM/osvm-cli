//! Mount management commands for OSVM and MCP tools
//!
//! This module provides CLI commands to mount and unmount folders
//! for OSVM microVMs and individual MCP tools with automatic path detection.

use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::utils::path_security::safe_path_validation;

/// Mount information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MountInfo {
    pub host_path: String,
    pub vm_path: String,
    pub readonly: bool,
    #[serde(with = "chrono::serde::ts_seconds")]
    pub created_at: chrono::DateTime<chrono::Utc>,
}

/// Mounts configuration
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct MountsConfig {
    pub osvm_mounts: Vec<MountInfo>,
    pub mcp_tool_mounts: HashMap<String, Vec<MountInfo>>,
}

impl MountsConfig {
    /// Load configuration from file
    pub fn load() -> Result<Self> {
        let config_path = Self::get_config_path()?;

        if !config_path.exists() {
            return Ok(Self::default());
        }

        let content = std::fs::read_to_string(&config_path)
            .context("Failed to read mounts configuration")?;
        
        let config: Self = serde_json::from_str(&content)
            .context("Failed to parse mounts configuration")?;
        
        Ok(config)
    }

    /// Save configuration to file
    pub fn save(&self) -> Result<()> {
        let config_path = Self::get_config_path()?;

        // Ensure directory exists
        if let Some(parent) = config_path.parent() {
            std::fs::create_dir_all(parent)
                .context("Failed to create config directory")?;
        }

        let content = serde_json::to_string_pretty(self)
            .context("Failed to serialize mounts configuration")?;
        
        std::fs::write(&config_path, content)
            .context("Failed to write mounts configuration")?;

        Ok(())
    }

    /// Get configuration file path
    fn get_config_path() -> Result<PathBuf> {
        let home = std::env::var("HOME")
            .context("HOME environment variable not set")?;
        
        Ok(PathBuf::from(home)
            .join(".config")
            .join("osvm")
            .join("mounts.json"))
    }
}

/// Mount manager for OSVM and MCP tools
pub struct MountManager {
    config: MountsConfig,
}

impl MountManager {
    /// Create new mount manager
    pub fn new() -> Result<Self> {
        let config = MountsConfig::load()?;
        Ok(Self { config })
    }

    /// Add mount for OSVM microVM
    pub fn add_osvm_mount(&mut self, host_path: &str, readonly: bool) -> Result<String> {
        // Validate and canonicalize host path
        let host_path = self.validate_host_path(host_path)?;

        // Check if already mounted
        if self.config.osvm_mounts.iter().any(|m| m.host_path == host_path) {
            return Err(anyhow!("Path {} is already mounted", host_path));
        }

        // Auto-generate VM path
        let vm_path = self.generate_vm_path(&host_path, &self.config.osvm_mounts)?;

        // Create mount info
        let mount = MountInfo {
            host_path: host_path.clone(),
            vm_path: vm_path.clone(),
            readonly,
            created_at: chrono::Utc::now(),
        };

        self.config.osvm_mounts.push(mount);
        self.config.save()?;

        Ok(vm_path)
    }

    /// Remove mount for OSVM microVM
    pub fn remove_osvm_mount(&mut self, host_path: &str) -> Result<()> {
        let host_path = self.validate_host_path(host_path)?;

        let initial_len = self.config.osvm_mounts.len();
        self.config.osvm_mounts.retain(|m| m.host_path != host_path);

        if self.config.osvm_mounts.len() == initial_len {
            return Err(anyhow!("Mount not found: {}", host_path));
        }

        self.config.save()?;
        Ok(())
    }

    /// List OSVM mounts
    pub fn list_osvm_mounts(&self) -> &[MountInfo] {
        &self.config.osvm_mounts
    }

    /// Add mount for MCP tool
    pub fn add_mcp_mount(&mut self, tool_name: &str, host_path: &str, readonly: bool) -> Result<String> {
        // Validate and canonicalize host path
        let host_path = self.validate_host_path(host_path)?;

        // Check if already mounted and generate path before getting mutable reference
        let tool_path = {
            let existing_mounts = self.config.mcp_tool_mounts
                .get(tool_name)
                .map(|v| v.as_slice())
                .unwrap_or(&[]);
            
            // Check if already mounted
            if existing_mounts.iter().any(|m| m.host_path == host_path) {
                return Err(anyhow!("Path {} is already mounted for tool {}", host_path, tool_name));
            }
            
            // Auto-generate tool path
            self.generate_tool_path(&host_path, existing_mounts)?
        };

        // Now get mutable reference and add the mount
        let tool_mounts = self.config.mcp_tool_mounts
            .entry(tool_name.to_string())
            .or_default();

        // Create mount info
        let mount = MountInfo {
            host_path: host_path.clone(),
            vm_path: tool_path.clone(),
            readonly,
            created_at: chrono::Utc::now(),
        };

        tool_mounts.push(mount);
        self.config.save()?;

        Ok(tool_path)
    }

    /// Remove mount for MCP tool
    pub fn remove_mcp_mount(&mut self, tool_name: &str, host_path: &str) -> Result<()> {
        let host_path = self.validate_host_path(host_path)?;

        let tool_mounts = self.config.mcp_tool_mounts
            .get_mut(tool_name)
            .ok_or_else(|| anyhow!("No mounts found for tool: {}", tool_name))?;

        let initial_len = tool_mounts.len();
        tool_mounts.retain(|m| m.host_path != host_path);

        if tool_mounts.len() == initial_len {
            return Err(anyhow!("Mount not found: {}", host_path));
        }

        // Remove tool entry if no mounts left
        if tool_mounts.is_empty() {
            self.config.mcp_tool_mounts.remove(tool_name);
        }

        self.config.save()?;
        Ok(())
    }

    /// List mounts for MCP tool
    pub fn list_mcp_mounts(&self, tool_name: &str) -> Option<&[MountInfo]> {
        self.config.mcp_tool_mounts
            .get(tool_name)
            .map(|v| v.as_slice())
    }

    /// List all MCP tool mounts
    pub fn list_all_mcp_mounts(&self) -> &HashMap<String, Vec<MountInfo>> {
        &self.config.mcp_tool_mounts
    }

    /// Validate and canonicalize host path using secure validation
    fn validate_host_path(&self, path: &str) -> Result<String> {
        // Use centralized secure path validation with TOCTOU protection
        let validated_path = safe_path_validation(
            path,
            true,  // Must be a directory
            false, // Don't allow symlinks for security
        ).with_context(|| format!("Failed to validate mount path: {}", path))?;
        
        Ok(validated_path.path().display().to_string())
    }

    /// Generate VM path for OSVM mount
    fn generate_vm_path(&self, host_path: &str, existing_mounts: &[MountInfo]) -> Result<String> {
        let basename = Path::new(host_path)
            .file_name()
            .and_then(|n| n.to_str())
            .ok_or_else(|| anyhow!("Invalid path: {}", host_path))?
            .to_lowercase();

        let base_vm_path = format!("/mnt/{}", basename);

        // Check for conflicts and auto-increment if needed
        let mut vm_path = base_vm_path.clone();
        let mut counter = 1;

        while existing_mounts.iter().any(|m| m.vm_path == vm_path) {
            vm_path = format!("{}-{}", base_vm_path, counter);
            counter += 1;
        }

        Ok(vm_path)
    }

    /// Generate tool path for MCP mount
    fn generate_tool_path(&self, host_path: &str, existing_mounts: &[MountInfo]) -> Result<String> {
        let basename = Path::new(host_path)
            .file_name()
            .and_then(|n| n.to_str())
            .ok_or_else(|| anyhow!("Invalid path: {}", host_path))?
            .to_lowercase();

        let base_tool_path = format!("/data/{}", basename);

        // Check for conflicts and auto-increment if needed
        let mut tool_path = base_tool_path.clone();
        let mut counter = 1;

        while existing_mounts.iter().any(|m| m.vm_path == tool_path) {
            tool_path = format!("{}-{}", base_tool_path, counter);
            counter += 1;
        }

        Ok(tool_path)
    }
}

/// Handle 'osvm mount add' command
pub fn handle_mount_add(host_path: &str, readonly: bool) -> Result<()> {
    let mut manager = MountManager::new()?;
    let vm_path = manager.add_osvm_mount(host_path, readonly)?;

    println!("✓ Mounted {} → {}", host_path, vm_path);
    if readonly {
        println!("  (read-only)");
    }

    Ok(())
}

/// Handle 'osvm mount remove' command
pub fn handle_mount_remove(host_path: &str) -> Result<()> {
    let mut manager = MountManager::new()?;
    manager.remove_osvm_mount(host_path)?;

    println!("✓ Unmounted {}", host_path);

    Ok(())
}

/// Handle 'osvm mount list' command
pub fn handle_mount_list() -> Result<()> {
    let manager = MountManager::new()?;
    let mounts = manager.list_osvm_mounts();

    if mounts.is_empty() {
        println!("No OSVM mounts configured.");
        println!("\nDefault mount:");
        println!("  ~/.config/osvm → /config");
        return Ok(());
    }

    println!("OSVM Mounts:");
    println!("\nDefault:");
    println!("  ~/.config/osvm → /config");
    
    println!("\nConfigured:");
    for mount in mounts {
        let ro = if mount.readonly { " (read-only)" } else { "" };
        println!("  {} → {}{}", mount.host_path, mount.vm_path, ro);
    }

    Ok(())
}

/// Handle 'osvm mcp mount' command
pub fn handle_mcp_mount(tool_name: &str, host_path: &str, readonly: bool) -> Result<()> {
    // Validate that the MCP tool exists before allowing mount
    validate_mcp_tool_exists(tool_name)?;
    
    let mut manager = MountManager::new()?;
    let tool_path = manager.add_mcp_mount(tool_name, host_path, readonly)?;

    println!("✓ Mounted {} → {} (for {})", host_path, tool_path, tool_name);
    if readonly {
        println!("  (read-only)");
    }

    Ok(())
}

/// Handle 'osvm mcp unmount' command
pub fn handle_mcp_unmount(tool_name: &str, host_path: &str) -> Result<()> {
    // Provide helpful error message if tool doesn't exist
    validate_mcp_tool_exists(tool_name)?;
    
    let mut manager = MountManager::new()?;
    manager.remove_mcp_mount(tool_name, host_path)?;

    println!("✓ Unmounted {} (from {})", host_path, tool_name);

    Ok(())
}

/// Handle 'osvm mcp mounts' command
pub fn handle_mcp_mounts(tool_name: Option<&str>) -> Result<()> {
    let manager = MountManager::new()?;

    if let Some(tool) = tool_name {
        // Validate tool exists for better error messages
        validate_mcp_tool_exists(tool)?;
        
        // Show mounts for specific tool
        if let Some(mounts) = manager.list_mcp_mounts(tool) {
            println!("Mounts for {}:", tool);
            println!("\nDefault:");
            println!("  ~/.config/osvm → /config");
            
            if !mounts.is_empty() {
                println!("\nConfigured:");
                for mount in mounts {
                    let ro = if mount.readonly { " (read-only)" } else { "" };
                    println!("  {} → {}{}", mount.host_path, mount.vm_path, ro);
                }
            }
        } else {
            println!("No mounts configured for {}", tool);
            println!("\nDefault mount:");
            println!("  ~/.config/osvm → /config");
        }
    } else {
        // Show all tool mounts
        let all_mounts = manager.list_all_mcp_mounts();

        if all_mounts.is_empty() {
            println!("No MCP tool mounts configured.");
            return Ok(());
        }

        println!("MCP Tool Mounts:");
        for (tool, mounts) in all_mounts {
            println!("\n{}:", tool);
            println!("  Default: ~/.config/osvm → /config");
            for mount in mounts {
                let ro = if mount.readonly { " (read-only)" } else { "" };
                println!("  {} → {}{}", mount.host_path, mount.vm_path, ro);
            }
        }
    }

    Ok(())
}

/// Validate that an MCP tool exists in the configuration
fn validate_mcp_tool_exists(tool_name: &str) -> Result<()> {
    // Load MCP servers configuration
    let config_path = dirs::config_dir()
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from("./")))
        .join("osvm")
        .join("mcp_servers.json");
    
    if !config_path.exists() {
        return Err(anyhow!(
            "MCP tool '{}' not found. No MCP servers are configured.\n\
             \n\
             To add an MCP server:\n\
             • From GitHub: osvm mcp add-github <server-id> <github-url>\n\
             • From URL: osvm mcp add <server-id> --server-url <url>\n\
             \n\
             Then list available tools with: osvm mcp list",
            tool_name
        ));
    }
    
    // Read and parse configuration
    let content = std::fs::read_to_string(&config_path)
        .context("Failed to read MCP servers configuration")?;
    
    let servers: std::collections::HashMap<String, serde_json::Value> = 
        serde_json::from_str(&content)
            .context("Failed to parse MCP servers configuration")?;
    
    if servers.is_empty() {
        return Err(anyhow!(
            "MCP tool '{}' not found. No MCP servers are configured.\n\
             \n\
             To add an MCP server:\n\
             • From GitHub: osvm mcp add-github <server-id> <github-url>\n\
             • From URL: osvm mcp add <server-id> --server-url <url>\n\
             \n\
             Then list available tools with: osvm mcp list",
            tool_name
        ));
    }
    
    // List available servers for helpful error message
    let server_list: Vec<String> = servers.keys().cloned().collect();
    let server_names = server_list.join(", ");
    
    Err(anyhow!(
        "MCP tool '{}' not found in configured servers.\n\
         \n\
         Available MCP servers: {}\n\
         \n\
         To see tools available from each server:\n\
         • osvm mcp list\n\
         \n\
         To add a new MCP server:\n\
         • From GitHub: osvm mcp add-github <server-id> <github-url>\n\
         • From URL: osvm mcp add <server-id> --server-url <url>",
        tool_name,
        server_names
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_vm_path() {
        let manager = MountManager::new().unwrap();
        let existing = vec![];
        
        let path = manager.generate_vm_path("/home/user/Documents", &existing).unwrap();
        assert_eq!(path, "/mnt/documents");
    }

    #[test]
    fn test_generate_vm_path_with_conflict() {
        let manager = MountManager::new().unwrap();
        let existing = vec![
            MountInfo {
                host_path: "/home/user/Documents".to_string(),
                vm_path: "/mnt/documents".to_string(),
                readonly: false,
                created_at: chrono::Utc::now(),
            },
        ];
        
        let path = manager.generate_vm_path("/home/other/Documents", &existing).unwrap();
        assert_eq!(path, "/mnt/documents-1");
    }

    #[test]
    fn test_generate_tool_path() {
        let manager = MountManager::new().unwrap();
        let existing = vec![];
        
        let path = manager.generate_tool_path("/home/user/solana-data", &existing).unwrap();
        assert_eq!(path, "/data/solana-data");
    }
}
