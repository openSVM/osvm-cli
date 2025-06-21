//! Cross-platform package manager support
//!
//! This module provides unified interface for different package managers
//! across Linux, macOS, and Windows platforms.

use serde::{Deserialize, Serialize};
use std::error::Error as StdError;
use std::fmt;
use std::process::Command;

/// Supported package managers
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PackageManager {
    /// Debian/Ubuntu package manager
    Apt,
    /// RHEL/CentOS legacy package manager
    Yum,
    /// Fedora/RHEL 8+ package manager
    Dnf,
    /// Arch Linux package manager
    Pacman,
    /// macOS package manager
    Homebrew,
    /// Windows package manager
    Chocolatey,
    /// Universal Linux package manager
    Snap,
    /// Universal Linux package manager
    Flatpak,
}

/// Package manager operation error
#[derive(Debug)]
pub enum PackageManagerError {
    NotFound(String),
    ExecutionFailed(String),
    PermissionDenied(String),
    NetworkError(String),
    Unknown(String),
}

impl fmt::Display for PackageManagerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PackageManagerError::NotFound(msg) => write!(f, "Package manager not found: {}", msg),
            PackageManagerError::ExecutionFailed(msg) => write!(f, "Execution failed: {}", msg),
            PackageManagerError::PermissionDenied(msg) => write!(f, "Permission denied: {}", msg),
            PackageManagerError::NetworkError(msg) => write!(f, "Network error: {}", msg),
            PackageManagerError::Unknown(msg) => write!(f, "Unknown error: {}", msg),
        }
    }
}

impl StdError for PackageManagerError {}

/// Package manager operations
pub trait PackageManagerOps {
    /// Update package database
    fn update_packages(&self) -> Result<String, PackageManagerError>;

    /// Upgrade all packages
    fn upgrade_packages(&self) -> Result<String, PackageManagerError>;

    /// Install specific packages
    fn install_packages(&self, packages: &[&str]) -> Result<String, PackageManagerError>;

    /// Check if package is installed
    fn is_package_installed(&self, package: &str) -> Result<bool, PackageManagerError>;

    /// List available updates
    fn list_updates(&self) -> Result<Vec<String>, PackageManagerError>;

    /// Get package manager name
    fn name(&self) -> &str;

    /// Check if this package manager is available on the system
    fn is_available(&self) -> bool;
}

impl PackageManager {
    /// Detect the primary package manager on the current system
    pub fn detect() -> Result<PackageManager, PackageManagerError> {
        // Check for common package managers in order of preference
        let managers = [
            PackageManager::Apt,
            PackageManager::Dnf,
            PackageManager::Yum,
            PackageManager::Pacman,
            PackageManager::Homebrew,
            PackageManager::Chocolatey,
        ];

        for manager in &managers {
            if manager.is_available() {
                return Ok(manager.clone());
            }
        }

        Err(PackageManagerError::NotFound(
            "No supported package manager found on this system".to_string(),
        ))
    }

    /// Get all available package managers on the system
    pub fn detect_all() -> Vec<PackageManager> {
        let all_managers = [
            PackageManager::Apt,
            PackageManager::Dnf,
            PackageManager::Yum,
            PackageManager::Pacman,
            PackageManager::Homebrew,
            PackageManager::Chocolatey,
            PackageManager::Snap,
            PackageManager::Flatpak,
        ];

        all_managers
            .iter()
            .filter(|manager| manager.is_available())
            .cloned()
            .collect()
    }
}

impl PackageManagerOps for PackageManager {
    fn update_packages(&self) -> Result<String, PackageManagerError> {
        let cmd = match self {
            PackageManager::Apt => vec!["sudo", "apt", "update"],
            PackageManager::Dnf => vec!["sudo", "dnf", "check-update"],
            PackageManager::Yum => vec!["sudo", "yum", "check-update"],
            PackageManager::Pacman => vec!["sudo", "pacman", "-Sy"],
            PackageManager::Homebrew => vec!["brew", "update"],
            PackageManager::Chocolatey => vec!["choco", "upgrade", "all", "--noop"],
            PackageManager::Snap => vec!["sudo", "snap", "refresh", "--list"],
            PackageManager::Flatpak => vec!["flatpak", "update", "--appstream"],
        };

        execute_command(&cmd)
    }

    fn upgrade_packages(&self) -> Result<String, PackageManagerError> {
        let cmd = match self {
            PackageManager::Apt => vec!["sudo", "apt", "upgrade", "-y"],
            PackageManager::Dnf => vec!["sudo", "dnf", "upgrade", "-y"],
            PackageManager::Yum => vec!["sudo", "yum", "upgrade", "-y"],
            PackageManager::Pacman => vec!["sudo", "pacman", "-Syu", "--noconfirm"],
            PackageManager::Homebrew => vec!["brew", "upgrade"],
            PackageManager::Chocolatey => vec!["choco", "upgrade", "all", "-y"],
            PackageManager::Snap => vec!["sudo", "snap", "refresh"],
            PackageManager::Flatpak => vec!["flatpak", "update", "-y"],
        };

        execute_command(&cmd)
    }

    fn install_packages(&self, packages: &[&str]) -> Result<String, PackageManagerError> {
        if packages.is_empty() {
            return Ok("No packages to install".to_string());
        }

        let mut cmd = match self {
            PackageManager::Apt => vec!["sudo", "apt", "install", "-y"],
            PackageManager::Dnf => vec!["sudo", "dnf", "install", "-y"],
            PackageManager::Yum => vec!["sudo", "yum", "install", "-y"],
            PackageManager::Pacman => vec!["sudo", "pacman", "-S", "--noconfirm"],
            PackageManager::Homebrew => vec!["brew", "install"],
            PackageManager::Chocolatey => vec!["choco", "install", "-y"],
            PackageManager::Snap => vec!["sudo", "snap", "install"],
            PackageManager::Flatpak => vec!["flatpak", "install", "-y"],
        };

        cmd.extend(packages);
        execute_command(&cmd)
    }

    fn is_package_installed(&self, package: &str) -> Result<bool, PackageManagerError> {
        let cmd = match self {
            PackageManager::Apt => vec!["dpkg", "-l", package],
            PackageManager::Dnf => vec!["dnf", "list", "installed", package],
            PackageManager::Yum => vec!["yum", "list", "installed", package],
            PackageManager::Pacman => vec!["pacman", "-Q", package],
            PackageManager::Homebrew => vec!["brew", "list", package],
            PackageManager::Chocolatey => vec!["choco", "list", "--local-only", package],
            PackageManager::Snap => vec!["snap", "list", package],
            PackageManager::Flatpak => vec!["flatpak", "list", "--app", package],
        };

        match execute_command(&cmd) {
            Ok(_) => Ok(true),
            Err(PackageManagerError::ExecutionFailed(_)) => Ok(false),
            Err(e) => Err(e),
        }
    }

    fn list_updates(&self) -> Result<Vec<String>, PackageManagerError> {
        let cmd = match self {
            PackageManager::Apt => vec!["apt", "list", "--upgradable"],
            PackageManager::Dnf => vec!["dnf", "list", "--upgrades"],
            PackageManager::Yum => vec!["yum", "list", "updates"],
            PackageManager::Pacman => vec!["pacman", "-Qu"],
            PackageManager::Homebrew => vec!["brew", "outdated"],
            PackageManager::Chocolatey => vec!["choco", "outdated"],
            PackageManager::Snap => vec!["snap", "refresh", "--list"],
            PackageManager::Flatpak => vec!["flatpak", "remote-ls", "--updates"],
        };

        let output = execute_command(&cmd)?;
        let updates: Vec<String> = output
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.trim().to_string())
            .collect();

        Ok(updates)
    }

    fn name(&self) -> &str {
        match self {
            PackageManager::Apt => "apt",
            PackageManager::Dnf => "dnf",
            PackageManager::Yum => "yum",
            PackageManager::Pacman => "pacman",
            PackageManager::Homebrew => "brew",
            PackageManager::Chocolatey => "choco",
            PackageManager::Snap => "snap",
            PackageManager::Flatpak => "flatpak",
        }
    }

    fn is_available(&self) -> bool {
        let cmd = match self {
            PackageManager::Apt => "apt",
            PackageManager::Dnf => "dnf",
            PackageManager::Yum => "yum",
            PackageManager::Pacman => "pacman",
            PackageManager::Homebrew => "brew",
            PackageManager::Chocolatey => "choco",
            PackageManager::Snap => "snap",
            PackageManager::Flatpak => "flatpak",
        };

        Command::new("which")
            .arg(cmd)
            .output()
            .map(|output| output.status.success())
            .unwrap_or(false)
    }
}

/// Execute a command and return the output
fn execute_command(cmd: &[&str]) -> Result<String, PackageManagerError> {
    if cmd.is_empty() {
        return Err(PackageManagerError::ExecutionFailed(
            "Empty command".to_string(),
        ));
    }

    let mut command = Command::new(cmd[0]);
    if cmd.len() > 1 {
        command.args(&cmd[1..]);
    }

    match command.output() {
        Ok(output) => {
            if output.status.success() {
                Ok(String::from_utf8_lossy(&output.stdout).to_string())
            } else {
                let stderr = String::from_utf8_lossy(&output.stderr);
                if stderr.contains("Permission denied") || stderr.contains("permission denied") {
                    Err(PackageManagerError::PermissionDenied(stderr.to_string()))
                } else if stderr.contains("network") || stderr.contains("Network") {
                    Err(PackageManagerError::NetworkError(stderr.to_string()))
                } else {
                    Err(PackageManagerError::ExecutionFailed(stderr.to_string()))
                }
            }
        }
        Err(e) => Err(PackageManagerError::ExecutionFailed(e.to_string())),
    }
}

/// Get system-specific build dependencies
pub fn get_build_dependencies() -> Vec<&'static str> {
    if cfg!(target_os = "linux") {
        vec![
            "build-essential",
            "pkg-config",
            "libssl-dev",
            "git",
            "curl",
            "libudev-dev",
        ]
    } else if cfg!(target_os = "macos") {
        vec!["pkg-config", "openssl", "git", "curl"]
    } else if cfg!(target_os = "windows") {
        vec!["git", "curl"]
    } else {
        vec!["git", "curl"]
    }
}

/// Get platform-specific package mappings
pub fn map_package_name<'a>(package: &'a str, manager: &PackageManager) -> &'a str {
    match (package, manager) {
        ("build-essential", PackageManager::Dnf) => "groupinstall \"Development Tools\"",
        ("build-essential", PackageManager::Yum) => "groupinstall \"Development Tools\"",
        ("build-essential", PackageManager::Pacman) => "base-devel",
        ("libssl-dev", PackageManager::Dnf) => "openssl-devel",
        ("libssl-dev", PackageManager::Yum) => "openssl-devel",
        ("libssl-dev", PackageManager::Pacman) => "openssl",
        ("libssl-dev", PackageManager::Homebrew) => "openssl",
        ("libudev-dev", PackageManager::Dnf) => "systemd-devel",
        ("libudev-dev", PackageManager::Yum) => "systemd-devel",
        ("libudev-dev", PackageManager::Pacman) => "systemd",
        _ => package,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_package_manager_detection() {
        // This test will vary based on the system it's run on
        let detected = PackageManager::detect_all();
        assert!(
            !detected.is_empty(),
            "At least one package manager should be detected"
        );
    }

    #[test]
    fn test_package_name_mapping() {
        assert_eq!(
            map_package_name("libssl-dev", &PackageManager::Dnf),
            "openssl-devel"
        );
        assert_eq!(
            map_package_name("libssl-dev", &PackageManager::Homebrew),
            "openssl"
        );
        assert_eq!(
            map_package_name("unknown-package", &PackageManager::Apt),
            "unknown-package"
        );
    }

    #[test]
    fn test_build_dependencies() {
        let deps = get_build_dependencies();
        assert!(!deps.is_empty(), "Build dependencies should not be empty");
        assert!(deps.contains(&"git"), "Should include git");
        assert!(deps.contains(&"curl"), "Should include curl");
    }
}
