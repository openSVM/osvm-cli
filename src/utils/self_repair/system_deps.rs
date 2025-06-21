//! System-level dependency management
//!
//! This module handles checking and installing system-level dependencies
//! including Rust toolchain, build tools, and system packages.

use super::package_managers::{PackageManager, PackageManagerError, PackageManagerOps};
use serde::{Deserialize, Serialize};
use std::error::Error as StdError;
use std::fmt;
use std::process::Command;

/// System dependency error
#[derive(Debug)]
pub enum SystemDepsError {
    CommandFailed(String),
    NotInstalled(String),
    VersionCheckFailed(String),
    InstallationFailed(String),
    PackageManagerError(PackageManagerError),
}

impl fmt::Display for SystemDepsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SystemDepsError::CommandFailed(msg) => write!(f, "Command failed: {}", msg),
            SystemDepsError::NotInstalled(pkg) => write!(f, "Package not installed: {}", pkg),
            SystemDepsError::VersionCheckFailed(msg) => write!(f, "Version check failed: {}", msg),
            SystemDepsError::InstallationFailed(msg) => write!(f, "Installation failed: {}", msg),
            SystemDepsError::PackageManagerError(e) => write!(f, "Package manager error: {}", e),
        }
    }
}

impl StdError for SystemDepsError {}

impl From<PackageManagerError> for SystemDepsError {
    fn from(err: PackageManagerError) -> Self {
        SystemDepsError::PackageManagerError(err)
    }
}

/// Information about a system dependency
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyInfo {
    pub name: String,
    pub installed: bool,
    pub version: Option<String>,
    pub required_version: Option<String>,
    pub update_available: bool,
}

/// System dependency checker and installer
#[derive(Debug)]
pub struct SystemDependencyManager {
    package_manager: PackageManager,
}

impl SystemDependencyManager {
    /// Create a new system dependency manager
    pub fn new() -> Result<Self, SystemDepsError> {
        let package_manager =
            PackageManager::detect().map_err(SystemDepsError::PackageManagerError)?;

        Ok(Self { package_manager })
    }

    /// Check all system dependencies
    pub async fn check_all_dependencies(&self) -> Result<Vec<DependencyInfo>, SystemDepsError> {
        let mut dependencies = Vec::new();

        // Check Rust toolchain
        dependencies.push(self.check_rust_toolchain().await?);

        // Check build tools
        let build_tools = super::package_managers::get_build_dependencies();
        for tool in build_tools {
            dependencies.push(self.check_system_package(tool).await?);
        }

        // Check additional tools
        dependencies.push(self.check_system_package("git").await?);
        dependencies.push(self.check_system_package("curl").await?);

        Ok(dependencies)
    }

    /// Check Rust toolchain status
    pub async fn check_rust_toolchain(&self) -> Result<DependencyInfo, SystemDepsError> {
        let installed = self.is_rust_installed().await?;

        if !installed {
            return Ok(DependencyInfo {
                name: "rust".to_string(),
                installed: false,
                version: None,
                required_version: Some("1.70.0+".to_string()),
                update_available: false,
            });
        }

        let version = self.get_rust_version().await?;
        let update_available = self.check_rust_updates().await?;

        Ok(DependencyInfo {
            name: "rust".to_string(),
            installed: true,
            version: Some(version),
            required_version: Some("1.70.0+".to_string()),
            update_available,
        })
    }

    /// Check if Rust is installed
    pub async fn is_rust_installed(&self) -> Result<bool, SystemDepsError> {
        match Command::new("rustc").arg("--version").output() {
            Ok(output) => Ok(output.status.success()),
            Err(_) => Ok(false),
        }
    }

    /// Get current Rust version
    async fn get_rust_version(&self) -> Result<String, SystemDepsError> {
        let output = Command::new("rustc")
            .arg("--version")
            .output()
            .map_err(|e| SystemDepsError::CommandFailed(e.to_string()))?;

        if output.status.success() {
            let version_str = String::from_utf8_lossy(&output.stdout);
            // Extract version from "rustc 1.76.0 (07dca489a 2024-02-04)"
            if let Some(version) = version_str.split_whitespace().nth(1) {
                Ok(version.to_string())
            } else {
                Err(SystemDepsError::VersionCheckFailed(
                    "Could not parse Rust version".to_string(),
                ))
            }
        } else {
            Err(SystemDepsError::CommandFailed(
                "Failed to get Rust version".to_string(),
            ))
        }
    }

    /// Check for Rust updates
    async fn check_rust_updates(&self) -> Result<bool, SystemDepsError> {
        match Command::new("rustup").arg("check").output() {
            Ok(output) => {
                let output_str = String::from_utf8_lossy(&output.stdout);
                Ok(output_str.contains("Update available"))
            }
            Err(_) => Ok(false), // If rustup is not available, assume no updates
        }
    }

    /// Install or update Rust toolchain
    pub async fn install_rust_toolchain(&self) -> Result<String, SystemDepsError> {
        // Check if rustup is installed
        if !self.is_rustup_installed().await? {
            // Install rustup
            let install_script = if cfg!(unix) {
                "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y"
            } else {
                return Err(SystemDepsError::InstallationFailed(
                    "Please install Rust manually on Windows".to_string(),
                ));
            };

            let output = Command::new("sh")
                .arg("-c")
                .arg(install_script)
                .output()
                .map_err(|e| SystemDepsError::InstallationFailed(e.to_string()))?;

            if !output.status.success() {
                return Err(SystemDepsError::InstallationFailed(
                    String::from_utf8_lossy(&output.stderr).to_string(),
                ));
            }

            // Source the cargo environment
            let source_cmd = "source ~/.cargo/env";
            let _ = Command::new("sh").arg("-c").arg(source_cmd).output();

            Ok("Rust toolchain installed successfully".to_string())
        } else {
            // Update existing installation
            let output = Command::new("rustup")
                .arg("update")
                .output()
                .map_err(|e| SystemDepsError::InstallationFailed(e.to_string()))?;

            if output.status.success() {
                Ok("Rust toolchain updated successfully".to_string())
            } else {
                Err(SystemDepsError::InstallationFailed(
                    String::from_utf8_lossy(&output.stderr).to_string(),
                ))
            }
        }
    }

    /// Check if rustup is installed
    async fn is_rustup_installed(&self) -> Result<bool, SystemDepsError> {
        match Command::new("rustup").arg("--version").output() {
            Ok(output) => Ok(output.status.success()),
            Err(_) => Ok(false),
        }
    }

    /// Check system package status
    pub async fn check_system_package(
        &self,
        package: &str,
    ) -> Result<DependencyInfo, SystemDepsError> {
        let mapped_package =
            super::package_managers::map_package_name(package, &self.package_manager);
        let installed = self.package_manager.is_package_installed(mapped_package)?;

        Ok(DependencyInfo {
            name: package.to_string(),
            installed,
            version: if installed {
                Some("installed".to_string())
            } else {
                None
            },
            required_version: None,
            update_available: false, // TODO: Implement update checking
        })
    }

    /// Update system packages
    pub async fn update_system_packages(&self) -> Result<String, SystemDepsError> {
        println!("🔄 Updating package database...");
        self.package_manager.update_packages()?;

        println!("📦 Upgrading system packages...");
        let result = self.package_manager.upgrade_packages()?;

        Ok(result)
    }

    /// Install system build dependencies
    pub async fn install_build_dependencies(&self) -> Result<String, SystemDepsError> {
        let build_deps = super::package_managers::get_build_dependencies();
        let mut missing_deps = Vec::new();

        // Check which dependencies are missing
        for dep in &build_deps {
            let dep_info = self.check_system_package(dep).await?;
            if !dep_info.installed {
                missing_deps.push(*dep);
            }
        }

        if missing_deps.is_empty() {
            return Ok("All build dependencies are already installed".to_string());
        }

        println!("📦 Installing build dependencies: {:?}", missing_deps);

        // Map package names for the current package manager
        let mapped_deps: Vec<&str> = missing_deps
            .iter()
            .map(|dep| super::package_managers::map_package_name(dep, &self.package_manager))
            .collect();

        let result = self.package_manager.install_packages(&mapped_deps)?;
        Ok(result)
    }

    /// Install specific system dependencies
    pub async fn install_system_dependencies(
        &self,
        dependencies: &[String],
    ) -> Result<String, SystemDepsError> {
        if dependencies.is_empty() {
            return Ok("No dependencies to install".to_string());
        }

        println!("📦 Installing system dependencies: {:?}", dependencies);

        // Map package names for the current package manager
        let mapped_deps: Vec<&str> = dependencies
            .iter()
            .map(|dep| super::package_managers::map_package_name(dep, &self.package_manager))
            .collect();

        let result = self.package_manager.install_packages(&mapped_deps)?;
        Ok(result)
    }

    /// Get package manager information
    pub fn get_package_manager_info(&self) -> String {
        format!("{} package manager detected", self.package_manager.name())
    }

    /// Check if system has pending updates
    pub async fn check_system_updates(&self) -> Result<Vec<String>, SystemDepsError> {
        let updates = self.package_manager.list_updates()?;
        Ok(updates)
    }
}

/// Check if Solana CLI is installed
pub async fn check_solana_cli() -> Result<bool, SystemDepsError> {
    match Command::new("solana").arg("--version").output() {
        Ok(output) => Ok(output.status.success()),
        Err(_) => Ok(false),
    }
}

/// Get Solana CLI version if installed
pub async fn get_solana_version() -> Result<Option<String>, SystemDepsError> {
    match Command::new("solana").arg("--version").output() {
        Ok(output) if output.status.success() => {
            let version_str = String::from_utf8_lossy(&output.stdout);
            // Extract version from "solana-cli 1.18.23 (src:devbuild; feat:4215500972)"
            if let Some(version) = version_str.split_whitespace().nth(1) {
                Ok(Some(version.to_string()))
            } else {
                Ok(Some("unknown".to_string()))
            }
        }
        _ => Ok(None),
    }
}

/// Install Solana CLI
pub async fn install_solana_cli() -> Result<String, SystemDepsError> {
    println!("🌐 Installing Solana CLI...");

    let install_script =
        "curl --proto '=https' --tlsv1.2 -sSfL https://solana-install.solana.workers.dev | bash";

    let output = Command::new("sh")
        .arg("-c")
        .arg(install_script)
        .output()
        .map_err(|e| SystemDepsError::InstallationFailed(e.to_string()))?;

    if output.status.success() {
        // Add Solana to PATH for current session
        let path_cmd = "export PATH=\"$HOME/.local/share/solana/install/active_release/bin:$PATH\"";
        let _ = Command::new("sh").arg("-c").arg(path_cmd).output();

        Ok("Solana CLI installed successfully".to_string())
    } else {
        Err(SystemDepsError::InstallationFailed(
            String::from_utf8_lossy(&output.stderr).to_string(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_rust_installation_check() {
        let manager = SystemDependencyManager::new().expect("Failed to create manager");
        let is_installed = manager
            .is_rust_installed()
            .await
            .expect("Failed to check Rust");

        // This test depends on the environment, so we just ensure it doesn't crash
        println!("Rust installed: {}", is_installed);
    }

    #[tokio::test]
    async fn test_solana_cli_check() {
        let is_installed = check_solana_cli()
            .await
            .expect("Failed to check Solana CLI");
        println!("Solana CLI installed: {}", is_installed);
    }

    #[tokio::test]
    async fn test_dependency_info_creation() {
        let dep_info = DependencyInfo {
            name: "test-package".to_string(),
            installed: true,
            version: Some("1.0.0".to_string()),
            required_version: Some("1.0.0+".to_string()),
            update_available: false,
        };

        assert_eq!(dep_info.name, "test-package");
        assert!(dep_info.installed);
    }

    #[tokio::test]
    async fn test_system_dependency_manager_creation() {
        match SystemDependencyManager::new() {
            Ok(manager) => {
                println!("Package manager: {}", manager.get_package_manager_info());
            }
            Err(e) => {
                println!("No package manager detected: {}", e);
            }
        }
    }
}
