//! Rollback validation and health verification
//!
//! This module provides comprehensive validation after rollback operations
//! to ensure system integrity and functionality.

use super::{CheckResult, DiagnosticError};
use crate::utils::self_repair::{
    system_deps::SystemDependencyManager, user_deps::UserDependencyManager,
};
use std::time::Instant;

/// Rollback validation result
#[derive(Debug, Clone)]
pub struct RollbackValidationResult {
    pub success: bool,
    pub critical_checks: Vec<CheckResult>,
    pub warning_checks: Vec<CheckResult>,
    pub validation_time_ms: u64,
    pub issues_found: Vec<String>,
    pub recommendations: Vec<String>,
}

/// Rollback validator for comprehensive system health verification
pub struct RollbackValidator {
    system_manager: Option<SystemDependencyManager>,
    user_manager: UserDependencyManager,
}

impl RollbackValidator {
    /// Create a new rollback validator
    pub fn new() -> Self {
        let system_manager = SystemDependencyManager::new().ok();
        let user_manager = UserDependencyManager::new();

        Self {
            system_manager,
            user_manager,
        }
    }

    /// Validate that rollback was successful and system is functional
    pub async fn validate_rollback_success(
        &self,
    ) -> Result<RollbackValidationResult, DiagnosticError> {
        let start_time = Instant::now();
        let mut critical_checks = Vec::new();
        let mut warning_checks = Vec::new();
        let mut issues_found = Vec::new();
        let mut recommendations = Vec::new();

        println!("🔍 Validating rollback success...");

        // Critical system functionality checks
        critical_checks.push(self.check_package_manager_functional().await);
        critical_checks.push(self.check_file_system_integrity().await);
        critical_checks.push(self.check_system_services_running().await);
        critical_checks.push(self.check_basic_commands_working().await);

        // Warning-level checks
        warning_checks.push(self.check_rust_toolchain_functional().await);
        warning_checks.push(self.check_solana_cli_functional().await);
        warning_checks.push(self.check_network_connectivity().await);
        warning_checks.push(self.check_configuration_integrity().await);

        // Analyze results
        let critical_failures: Vec<_> = critical_checks
            .iter()
            .filter(|check| !check.passed)
            .collect();

        let warning_failures: Vec<_> = warning_checks
            .iter()
            .filter(|check| !check.passed)
            .collect();

        // Determine overall success
        let success = critical_failures.is_empty();

        // Collect issues and recommendations
        for check in &critical_failures {
            issues_found.push(format!("CRITICAL: {}", check.message));
            if let Some(details) = &check.details {
                recommendations.push(format!("Fix {}: {}", check.name, details));
            }
        }

        for check in &warning_failures {
            issues_found.push(format!("WARNING: {}", check.message));
        }

        if success {
            if warning_failures.is_empty() {
                println!("✅ Rollback validation passed - System fully functional");
            } else {
                println!(
                    "⚠️  Rollback validation passed with warnings - Core functionality restored"
                );
                recommendations.push("Some non-critical components may need attention".to_string());
            }
        } else {
            println!("❌ Rollback validation failed - Critical system issues detected");
            recommendations.push(
                "Manual intervention may be required to restore full functionality".to_string(),
            );
            recommendations
                .push("Consider restoring from an earlier snapshot if available".to_string());
        }

        let validation_time = start_time.elapsed().as_millis() as u64;

        Ok(RollbackValidationResult {
            success,
            critical_checks,
            warning_checks,
            validation_time_ms: validation_time,
            issues_found,
            recommendations,
        })
    }

    /// Check if package manager is functional
    async fn check_package_manager_functional(&self) -> CheckResult {
        let start = Instant::now();

        if let Some(ref system_manager) = self.system_manager {
            // Try to get package manager info
            let package_info = system_manager.get_package_manager_info();

            // Try to check for updates (non-destructive operation)
            match system_manager.check_system_updates().await {
                Ok(_) => CheckResult {
                    name: "Package Manager".to_string(),
                    passed: true,
                    message: format!("Package manager functional: {}", package_info),
                    details: Some("Successfully queried package database".to_string()),
                    execution_time_ms: start.elapsed().as_millis() as u64,
                },
                Err(e) => CheckResult {
                    name: "Package Manager".to_string(),
                    passed: false,
                    message: "Package manager not functional".to_string(),
                    details: Some(format!("Error: {}", e)),
                    execution_time_ms: start.elapsed().as_millis() as u64,
                },
            }
        } else {
            CheckResult {
                name: "Package Manager".to_string(),
                passed: false,
                message: "Package manager not available".to_string(),
                details: Some("Could not initialize package manager".to_string()),
                execution_time_ms: start.elapsed().as_millis() as u64,
            }
        }
    }

    /// Check file system integrity
    async fn check_file_system_integrity(&self) -> CheckResult {
        let start = Instant::now();
        let mut issues = Vec::new();

        // Check critical directories
        let critical_dirs = ["/tmp", "/var", "/etc"];

        for dir in &critical_dirs {
            if !std::path::Path::new(dir).exists() {
                issues.push(format!("Missing critical directory: {}", dir));
            }
        }

        // Check user directories
        if let Some(home) = dirs::home_dir() {
            let user_dirs = [home.join(".config"), home.join(".local")];

            for dir in &user_dirs {
                if !dir.exists() {
                    // Try to create it
                    if let Err(e) = std::fs::create_dir_all(dir) {
                        issues.push(format!(
                            "Cannot create user directory {}: {}",
                            dir.display(),
                            e
                        ));
                    }
                }
            }
        }

        CheckResult {
            name: "File System Integrity".to_string(),
            passed: issues.is_empty(),
            message: if issues.is_empty() {
                "File system integrity OK".to_string()
            } else {
                format!("File system issues detected: {}", issues.len())
            },
            details: if issues.is_empty() {
                Some("All critical directories present and accessible".to_string())
            } else {
                Some(issues.join("; "))
            },
            execution_time_ms: start.elapsed().as_millis() as u64,
        }
    }

    /// Check that system services are running
    async fn check_system_services_running(&self) -> CheckResult {
        let start = Instant::now();

        // Check if basic system commands work
        let test_commands = [
            ("ps", vec!["--version"]),
            ("ls", vec!["--version"]),
            ("cat", vec!["--version"]),
        ];

        let mut working_commands = 0;
        let total_commands = test_commands.len();

        for (cmd, args) in &test_commands {
            match tokio::process::Command::new(cmd).args(args).output().await {
                Ok(output) if output.status.success() => {
                    working_commands += 1;
                }
                _ => {
                    // Command failed
                }
            }
        }

        let passed = working_commands == total_commands;

        CheckResult {
            name: "System Services".to_string(),
            passed,
            message: if passed {
                "Essential system commands functional".to_string()
            } else {
                format!(
                    "Some system commands not working ({}/{})",
                    working_commands, total_commands
                )
            },
            details: Some(format!(
                "Tested basic commands: ps, ls, cat - {}/{} working",
                working_commands, total_commands
            )),
            execution_time_ms: start.elapsed().as_millis() as u64,
        }
    }

    /// Check that basic commands are working
    async fn check_basic_commands_working(&self) -> CheckResult {
        let start = Instant::now();

        // Test shell and basic command execution
        match tokio::process::Command::new("sh")
            .arg("-c")
            .arg("echo 'system_check' | wc -l")
            .output()
            .await
        {
            Ok(output) if output.status.success() => {
                let output_str = String::from_utf8_lossy(&output.stdout);
                if output_str.trim() == "1" {
                    CheckResult {
                        name: "Basic Commands".to_string(),
                        passed: true,
                        message: "Shell and basic commands working".to_string(),
                        details: Some("Successfully executed shell pipeline".to_string()),
                        execution_time_ms: start.elapsed().as_millis() as u64,
                    }
                } else {
                    CheckResult {
                        name: "Basic Commands".to_string(),
                        passed: false,
                        message: "Shell pipeline returned unexpected result".to_string(),
                        details: Some(format!("Expected '1', got '{}'", output_str.trim())),
                        execution_time_ms: start.elapsed().as_millis() as u64,
                    }
                }
            }
            Ok(output) => CheckResult {
                name: "Basic Commands".to_string(),
                passed: false,
                message: "Shell command failed".to_string(),
                details: Some(format!(
                    "Exit code: {}, stderr: {}",
                    output.status.code().unwrap_or(-1),
                    String::from_utf8_lossy(&output.stderr)
                )),
                execution_time_ms: start.elapsed().as_millis() as u64,
            },
            Err(e) => CheckResult {
                name: "Basic Commands".to_string(),
                passed: false,
                message: "Could not execute shell command".to_string(),
                details: Some(format!("Error: {}", e)),
                execution_time_ms: start.elapsed().as_millis() as u64,
            },
        }
    }

    /// Check Rust toolchain functionality
    async fn check_rust_toolchain_functional(&self) -> CheckResult {
        let start = Instant::now();

        if let Some(ref system_manager) = self.system_manager {
            match system_manager.is_rust_installed().await {
                Ok(true) => {
                    // Test if Rust can compile a simple program
                    match self.test_rust_compilation().await {
                        Ok(true) => CheckResult {
                            name: "Rust Toolchain".to_string(),
                            passed: true,
                            message: "Rust toolchain functional".to_string(),
                            details: Some("Successfully compiled test program".to_string()),
                            execution_time_ms: start.elapsed().as_millis() as u64,
                        },
                        Ok(false) => CheckResult {
                            name: "Rust Toolchain".to_string(),
                            passed: false,
                            message: "Rust compilation test failed".to_string(),
                            details: Some(
                                "Rust is installed but cannot compile programs".to_string(),
                            ),
                            execution_time_ms: start.elapsed().as_millis() as u64,
                        },
                        Err(e) => CheckResult {
                            name: "Rust Toolchain".to_string(),
                            passed: false,
                            message: "Could not test Rust compilation".to_string(),
                            details: Some(format!("Error: {}", e)),
                            execution_time_ms: start.elapsed().as_millis() as u64,
                        },
                    }
                }
                Ok(false) => CheckResult {
                    name: "Rust Toolchain".to_string(),
                    passed: false,
                    message: "Rust is not installed".to_string(),
                    details: Some("Rust toolchain not found".to_string()),
                    execution_time_ms: start.elapsed().as_millis() as u64,
                },
                Err(e) => CheckResult {
                    name: "Rust Toolchain".to_string(),
                    passed: false,
                    message: "Could not check Rust installation".to_string(),
                    details: Some(format!("Error: {}", e)),
                    execution_time_ms: start.elapsed().as_millis() as u64,
                },
            }
        } else {
            CheckResult {
                name: "Rust Toolchain".to_string(),
                passed: false,
                message: "System manager not available".to_string(),
                details: Some("Cannot check Rust without system manager".to_string()),
                execution_time_ms: start.elapsed().as_millis() as u64,
            }
        }
    }

    /// Test Rust compilation
    async fn test_rust_compilation(
        &self,
    ) -> Result<bool, Box<dyn std::error::Error + Send + Sync>> {
        // Create a simple test program
        let test_program = r#"fn main() { println!("test"); }"#;
        let temp_file = std::env::temp_dir().join("osvm_rust_test.rs");
        let temp_binary = std::env::temp_dir().join("osvm_rust_test");

        // Write test program
        tokio::fs::write(&temp_file, test_program).await?;

        // Try to compile it
        let output = tokio::process::Command::new("rustc")
            .arg(&temp_file)
            .arg("-o")
            .arg(&temp_binary)
            .output()
            .await?;

        // Clean up
        let _ = tokio::fs::remove_file(&temp_file).await;
        let _ = tokio::fs::remove_file(&temp_binary).await;

        Ok(output.status.success())
    }

    /// Check Solana CLI functionality
    async fn check_solana_cli_functional(&self) -> CheckResult {
        let start = Instant::now();

        match self.user_manager.is_solana_cli_installed().await {
            Ok(true) => {
                // Test if Solana CLI responds to commands
                match tokio::process::Command::new("solana")
                    .arg("--version")
                    .output()
                    .await
                {
                    Ok(output) if output.status.success() => {
                        let version = String::from_utf8_lossy(&output.stdout);
                        CheckResult {
                            name: "Solana CLI".to_string(),
                            passed: true,
                            message: "Solana CLI functional".to_string(),
                            details: Some(format!("Version: {}", version.trim())),
                            execution_time_ms: start.elapsed().as_millis() as u64,
                        }
                    }
                    Ok(output) => CheckResult {
                        name: "Solana CLI".to_string(),
                        passed: false,
                        message: "Solana CLI not responding".to_string(),
                        details: Some(format!(
                            "Error: {}",
                            String::from_utf8_lossy(&output.stderr)
                        )),
                        execution_time_ms: start.elapsed().as_millis() as u64,
                    },
                    Err(e) => CheckResult {
                        name: "Solana CLI".to_string(),
                        passed: false,
                        message: "Could not execute Solana CLI".to_string(),
                        details: Some(format!("Error: {}", e)),
                        execution_time_ms: start.elapsed().as_millis() as u64,
                    },
                }
            }
            Ok(false) => CheckResult {
                name: "Solana CLI".to_string(),
                passed: false,
                message: "Solana CLI is not installed".to_string(),
                details: Some("Solana CLI not found in PATH".to_string()),
                execution_time_ms: start.elapsed().as_millis() as u64,
            },
            Err(e) => CheckResult {
                name: "Solana CLI".to_string(),
                passed: false,
                message: "Could not check Solana CLI".to_string(),
                details: Some(format!("Error: {}", e)),
                execution_time_ms: start.elapsed().as_millis() as u64,
            },
        }
    }

    /// Check network connectivity
    async fn check_network_connectivity(&self) -> CheckResult {
        let start = Instant::now();

        // Simple connectivity test using ping
        match tokio::process::Command::new("ping")
            .arg("-c")
            .arg("1")
            .arg("-W")
            .arg("5")
            .arg("8.8.8.8")
            .output()
            .await
        {
            Ok(output) if output.status.success() => CheckResult {
                name: "Network Connectivity".to_string(),
                passed: true,
                message: "Network connectivity OK".to_string(),
                details: Some("Successfully pinged 8.8.8.8".to_string()),
                execution_time_ms: start.elapsed().as_millis() as u64,
            },
            Ok(_) => CheckResult {
                name: "Network Connectivity".to_string(),
                passed: false,
                message: "Network connectivity failed".to_string(),
                details: Some("Could not ping 8.8.8.8".to_string()),
                execution_time_ms: start.elapsed().as_millis() as u64,
            },
            Err(e) => CheckResult {
                name: "Network Connectivity".to_string(),
                passed: false,
                message: "Could not test network connectivity".to_string(),
                details: Some(format!("Error: {}", e)),
                execution_time_ms: start.elapsed().as_millis() as u64,
            },
        }
    }

    /// Check configuration integrity
    async fn check_configuration_integrity(&self) -> CheckResult {
        let start = Instant::now();

        let config_dir = self.user_manager.get_solana_config_dir();
        let osvm_config_dir = self.user_manager.get_osvm_config_dir();

        let mut issues = Vec::new();

        // Check if configuration directories are accessible
        if config_dir.exists() && !config_dir.is_dir() {
            issues.push("Solana config path exists but is not a directory".to_string());
        }

        if osvm_config_dir.exists() && !osvm_config_dir.is_dir() {
            issues.push("OSVM config path exists but is not a directory".to_string());
        }

        // Test write permissions
        let test_file = osvm_config_dir.join("write_test.tmp");
        if let Err(e) = std::fs::create_dir_all(&osvm_config_dir) {
            issues.push(format!("Cannot create OSVM config directory: {}", e));
        } else if let Err(e) = std::fs::write(&test_file, "test") {
            issues.push(format!("Cannot write to OSVM config directory: {}", e));
        } else {
            // Clean up test file
            let _ = std::fs::remove_file(&test_file);
        }

        CheckResult {
            name: "Configuration Integrity".to_string(),
            passed: issues.is_empty(),
            message: if issues.is_empty() {
                "Configuration integrity OK".to_string()
            } else {
                format!("Configuration issues detected: {}", issues.len())
            },
            details: if issues.is_empty() {
                Some("All configuration directories accessible".to_string())
            } else {
                Some(issues.join("; "))
            },
            execution_time_ms: start.elapsed().as_millis() as u64,
        }
    }
}

impl Default for RollbackValidator {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function for direct rollback validation
pub async fn validate_rollback_success() -> Result<(), DiagnosticError> {
    let validator = RollbackValidator::new();
    let result = validator.validate_rollback_success().await?;

    if result.success {
        Ok(())
    } else {
        Err(DiagnosticError::ValidationFailed(format!(
            "Rollback validation failed: {:?}",
            result.issues_found
        )))
    }
}

/// Display rollback validation results
pub fn display_rollback_results(result: &RollbackValidationResult) {
    println!("\n🔍 ROLLBACK VALIDATION RESULTS");
    println!("==============================");

    if result.success {
        println!("✅ Overall Status: PASSED");
    } else {
        println!("❌ Overall Status: FAILED");
    }

    println!("⏱️  Validation Time: {}ms", result.validation_time_ms);

    if !result.critical_checks.is_empty() {
        println!("\n🔧 CRITICAL CHECKS:");
        for check in &result.critical_checks {
            let status = if check.passed { "✅" } else { "❌" };
            println!("  {} {}: {}", status, check.name, check.message);
        }
    }

    if !result.warning_checks.is_empty() {
        println!("\n⚠️  WARNING CHECKS:");
        for check in &result.warning_checks {
            let status = if check.passed { "✅" } else { "⚠️ " };
            println!("  {} {}: {}", status, check.name, check.message);
        }
    }

    if !result.issues_found.is_empty() {
        println!("\n🚨 ISSUES FOUND:");
        for issue in &result.issues_found {
            println!("  • {}", issue);
        }
    }

    if !result.recommendations.is_empty() {
        println!("\n💡 RECOMMENDATIONS:");
        for recommendation in &result.recommendations {
            println!("  • {}", recommendation);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rollback_validator_creation() {
        let validator = RollbackValidator::new();
        // Should not panic during creation
    }

    #[tokio::test]
    async fn test_basic_commands_check() {
        let validator = RollbackValidator::new();
        let result = validator.check_basic_commands_working().await;

        // This should generally pass on most systems
        println!("Basic commands check: {}", result.message);
    }

    #[tokio::test]
    async fn test_file_system_integrity_check() {
        let validator = RollbackValidator::new();
        let result = validator.check_file_system_integrity().await;

        // This should generally pass on healthy systems
        println!("File system integrity: {}", result.message);
    }

    #[test]
    fn test_rollback_validation_result() {
        let result = RollbackValidationResult {
            success: true,
            critical_checks: Vec::new(),
            warning_checks: Vec::new(),
            validation_time_ms: 100,
            issues_found: Vec::new(),
            recommendations: Vec::new(),
        };

        assert!(result.success);
        assert_eq!(result.validation_time_ms, 100);
    }
}
