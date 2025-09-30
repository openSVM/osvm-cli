//! Secure system operations without privilege escalation
//!
//! This module provides secure alternatives to system operations that previously
//! required sudo/root privileges, implementing defense-in-depth security practices.

use anyhow::{anyhow, Context, Result};
use std::fs;
use std::path::Path;
use std::process::Command;
use log::{warn, info, debug};

/// Secure system operations manager
pub struct SecureSystemManager {
    allow_user_operations: bool,
    dry_run_mode: bool,
}

impl SecureSystemManager {
    /// Create a new secure system manager
    pub fn new(allow_user_operations: bool, dry_run_mode: bool) -> Self {
        Self {
            allow_user_operations,
            dry_run_mode,
        }
    }

    /// Securely check system configuration without escalating privileges
    pub fn check_system_config(&self) -> Result<SystemStatus> {
        let mut status = SystemStatus::default();

        // Check disk space (no privileges needed)
        if let Ok(disk_info) = self.get_disk_info() {
            status.disk_available_gb = disk_info.available_gb;
            status.disk_total_gb = disk_info.total_gb;
        }

        // Check memory usage (no privileges needed)
        if let Ok(memory_info) = self.get_memory_info() {
            status.memory_available_gb = memory_info.available_gb;
            status.memory_total_gb = memory_info.total_gb;
        }

        // Check network connectivity (no privileges needed)
        status.network_accessible = self.check_network_connectivity();

        // Check user permissions for required directories
        status.user_can_write_config = self.check_write_permissions(&self.get_config_dir()?);
        status.user_can_write_data = self.check_write_permissions(&self.get_data_dir()?);

        Ok(status)
    }

    /// Get disk information without sudo
    fn get_disk_info(&self) -> Result<DiskInfo> {
        // Use statvfs or similar system calls instead of df with sudo
        let output = Command::new("df")
            .arg("-h")
            .arg(".")
            .output()
            .context("Failed to get disk information")?;

        if !output.status.success() {
            return Err(anyhow!("Failed to get disk information"));
        }

        let output_str = String::from_utf8_lossy(&output.stdout);
        let lines: Vec<&str> = output_str.lines().collect();

        if lines.len() < 2 {
            return Err(anyhow!("Invalid df output"));
        }

        // Parse the second line (first data line)
        let fields: Vec<&str> = lines[1].split_whitespace().collect();
        if fields.len() < 4 {
            return Err(anyhow!("Invalid df output format"));
        }

        let total_str = fields[1].trim_end_matches('G').trim_end_matches('M').trim_end_matches('K');
        let available_str = fields[3].trim_end_matches('G').trim_end_matches('M').trim_end_matches('K');

        // Simple parsing - convert to GB
        let total_gb = self.parse_size_to_gb(total_str, fields[1])?;
        let available_gb = self.parse_size_to_gb(available_str, fields[3])?;

        Ok(DiskInfo {
            total_gb,
            available_gb,
        })
    }

    /// Parse size string to GB
    fn parse_size_to_gb(&self, size_str: &str, original: &str) -> Result<f64> {
        let size: f64 = size_str.parse().context("Failed to parse size number")?;

        if original.ends_with('G') {
            Ok(size)
        } else if original.ends_with('M') {
            Ok(size / 1024.0)
        } else if original.ends_with('K') {
            Ok(size / (1024.0 * 1024.0))
        } else {
            // Assume bytes
            Ok(size / (1024.0 * 1024.0 * 1024.0))
        }
    }

    /// Get memory information without sudo
    fn get_memory_info(&self) -> Result<MemoryInfo> {
        // Read /proc/meminfo (no sudo needed)
        let meminfo_content = fs::read_to_string("/proc/meminfo")
            .context("Failed to read /proc/meminfo")?;

        let mut total_kb = 0;
        let mut available_kb = 0;

        for line in meminfo_content.lines() {
            if line.starts_with("MemTotal:") {
                total_kb = self.parse_meminfo_line(line)?;
            } else if line.starts_with("MemAvailable:") {
                available_kb = self.parse_meminfo_line(line)?;
            }
        }

        Ok(MemoryInfo {
            total_gb: total_kb as f64 / (1024.0 * 1024.0),
            available_gb: available_kb as f64 / (1024.0 * 1024.0),
        })
    }

    /// Parse a line from /proc/meminfo
    fn parse_meminfo_line(&self, line: &str) -> Result<u64> {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 2 {
            return Err(anyhow!("Invalid meminfo line format"));
        }

        parts[1].parse::<u64>()
            .context("Failed to parse memory value")
    }

    /// Check network connectivity without sudo
    fn check_network_connectivity(&self) -> bool {
        // Simple connectivity check to public DNS
        Command::new("ping")
            .arg("-c")
            .arg("1")
            .arg("-W")
            .arg("3")
            .arg("8.8.8.8")
            .output()
            .map(|output| output.status.success())
            .unwrap_or(false)
    }

    /// Check write permissions for a directory
    fn check_write_permissions(&self, path: &Path) -> bool {
        // Try to create a temporary file
        let test_file = path.join(".osvm_write_test");

        match fs::write(&test_file, "test") {
            Ok(_) => {
                let _ = fs::remove_file(&test_file);
                true
            }
            Err(_) => false,
        }
    }

    /// Get configuration directory
    fn get_config_dir(&self) -> Result<std::path::PathBuf> {
        dirs::config_dir()
            .map(|dir| dir.join("osvm"))
            .ok_or_else(|| anyhow!("Failed to determine config directory"))
    }

    /// Get data directory
    fn get_data_dir(&self) -> Result<std::path::PathBuf> {
        dirs::data_dir()
            .map(|dir| dir.join("osvm"))
            .ok_or_else(|| anyhow!("Failed to determine data directory"))
    }

    /// Attempt to create necessary directories without sudo
    pub fn setup_user_directories(&self) -> Result<()> {
        if self.dry_run_mode {
            info!("DRY RUN: Would create user directories");
            return Ok(());
        }

        let config_dir = self.get_config_dir()?;
        let data_dir = self.get_data_dir()?;

        // Create directories with user permissions only
        fs::create_dir_all(&config_dir)
            .with_context(|| format!("Failed to create config directory: {}", config_dir.display()))?;

        fs::create_dir_all(&data_dir)
            .with_context(|| format!("Failed to create data directory: {}", data_dir.display()))?;

        // Set secure permissions (user only)
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;

            let permissions = std::fs::Permissions::from_mode(0o700);
            fs::set_permissions(&config_dir, permissions.clone())
                .context("Failed to set config directory permissions")?;
            fs::set_permissions(&data_dir, permissions)
                .context("Failed to set data directory permissions")?;
        }

        info!("Successfully created user directories without privilege escalation");
        Ok(())
    }

    /// Alternative to sudo operations - provide user guidance
    pub fn suggest_manual_system_fixes(&self) -> Vec<ManualFix> {
        vec![
            ManualFix {
                issue: "System optimization".to_string(),
                suggestion: "Consider adjusting system settings through your system's settings GUI or asking your system administrator".to_string(),
                commands: vec![
                    "# If you have admin access, you can run:".to_string(),
                    "sudo sysctl -w net.core.rmem_max=134217728".to_string(),
                    "sudo sysctl -w net.core.wmem_max=134217728".to_string(),
                ],
                risk_level: RiskLevel::Low,
            },
            ManualFix {
                issue: "Process cleanup".to_string(),
                suggestion: "Use system monitoring tools to identify and manage processes".to_string(),
                commands: vec![
                    "# Use system activity monitor or:".to_string(),
                    "ps aux | grep solana".to_string(),
                    "# Then use kill <pid> for processes you own".to_string(),
                ],
                risk_level: RiskLevel::Low,
            },
            ManualFix {
                issue: "Package management".to_string(),
                suggestion: "Use your system's package manager through GUI or with admin privileges".to_string(),
                commands: vec![
                    "# System administrator can run:".to_string(),
                    "sudo apt update && sudo apt upgrade".to_string(),
                    "sudo apt autoremove".to_string(),
                ],
                risk_level: RiskLevel::Medium,
            },
        ]
    }
}

/// System status information
#[derive(Debug, Default)]
pub struct SystemStatus {
    pub disk_total_gb: f64,
    pub disk_available_gb: f64,
    pub memory_total_gb: f64,
    pub memory_available_gb: f64,
    pub network_accessible: bool,
    pub user_can_write_config: bool,
    pub user_can_write_data: bool,
}

/// Disk information
#[derive(Debug)]
struct DiskInfo {
    total_gb: f64,
    available_gb: f64,
}

/// Memory information
#[derive(Debug)]
struct MemoryInfo {
    total_gb: f64,
    available_gb: f64,
}

/// Manual fix suggestion
#[derive(Debug)]
pub struct ManualFix {
    pub issue: String,
    pub suggestion: String,
    pub commands: Vec<String>,
    pub risk_level: RiskLevel,
}

/// Risk level for manual fixes
#[derive(Debug)]
pub enum RiskLevel {
    Low,
    Medium,
    High,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_system_manager_creation() {
        let manager = SecureSystemManager::new(true, false);
        assert!(manager.allow_user_operations);
        assert!(!manager.dry_run_mode);
    }

    #[test]
    fn test_parse_size_to_gb() {
        let manager = SecureSystemManager::new(true, true);

        assert_eq!(manager.parse_size_to_gb("100", "100G").unwrap(), 100.0);
        assert_eq!(manager.parse_size_to_gb("1024", "1024M").unwrap(), 1.0);
        assert_eq!(manager.parse_size_to_gb("1048576", "1048576K").unwrap(), 1.0);
    }

    #[test]
    fn test_manual_fixes_generation() {
        let manager = SecureSystemManager::new(true, true);
        let fixes = manager.suggest_manual_system_fixes();

        assert!(!fixes.is_empty());
        assert!(fixes.iter().any(|fix| fix.issue.contains("System optimization")));
    }
}