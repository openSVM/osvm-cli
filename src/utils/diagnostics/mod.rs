//! System diagnostics and health monitoring
//!
//! This module provides comprehensive system health assessment capabilities
//! to support the self-repair system and general system monitoring.

use serde::{Deserialize, Serialize};
use std::error::Error as StdError;
use std::fmt;

pub mod connectivity;
pub mod rollback_validator;
pub mod system_health;
pub mod version_checker;

use crate::utils::self_repair::system_deps::{DependencyInfo, SystemDependencyManager};
use crate::utils::self_repair::user_deps::{SolanaConfigInfo, UserDependencyManager};

/// Diagnostic error types
#[derive(Debug)]
pub enum DiagnosticError {
    SystemCheckFailed(String),
    NetworkCheckFailed(String),
    ValidationFailed(String),
    ConfigurationError(String),
    Unknown(String),
}

impl fmt::Display for DiagnosticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagnosticError::SystemCheckFailed(msg) => write!(f, "System check failed: {}", msg),
            DiagnosticError::NetworkCheckFailed(msg) => write!(f, "Network check failed: {}", msg),
            DiagnosticError::ValidationFailed(msg) => write!(f, "Validation failed: {}", msg),
            DiagnosticError::ConfigurationError(msg) => write!(f, "Configuration error: {}", msg),
            DiagnosticError::Unknown(msg) => write!(f, "Unknown diagnostic error: {}", msg),
        }
    }
}

impl StdError for DiagnosticError {}

/// Overall system health status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemHealth {
    pub overall_status: HealthStatus,
    pub system_dependencies: Vec<DependencyInfo>,
    pub user_configuration: SolanaConfigInfo,
    pub network_connectivity: NetworkHealth,
    pub issues: Vec<HealthIssue>,
    pub recommendations: Vec<String>,
}

/// Health status levels
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum HealthStatus {
    Healthy,
    Warning,
    Critical,
    Unknown,
}

/// Network connectivity health
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkHealth {
    pub internet_connected: bool,
    pub solana_mainnet_accessible: bool,
    pub solana_testnet_accessible: bool,
    pub solana_devnet_accessible: bool,
    pub response_times: std::collections::HashMap<String, u64>, // endpoint -> ms
}

/// Health issue information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthIssue {
    pub severity: IssueSeverity,
    pub category: IssueCategory,
    pub title: String,
    pub description: String,
    pub suggested_fix: Option<String>,
}

/// Issue severity levels
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IssueSeverity {
    Info,
    Warning,
    Error,
    Critical,
}

/// Issue categories
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IssueCategory {
    SystemDependencies,
    UserConfiguration,
    NetworkConnectivity,
    Security,
    Performance,
}

/// Comprehensive diagnostic results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticResults {
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub system_health: SystemHealth,
    pub detailed_checks: std::collections::HashMap<String, CheckResult>,
    pub summary: DiagnosticSummary,
}

/// Individual check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckResult {
    pub name: String,
    pub passed: bool,
    pub message: String,
    pub details: Option<String>,
    pub execution_time_ms: u64,
}

/// Diagnostic summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticSummary {
    pub total_checks: usize,
    pub passed_checks: usize,
    pub failed_checks: usize,
    pub critical_issues: usize,
    pub warnings: usize,
    pub overall_health: HealthStatus,
}

/// Main diagnostic coordinator
pub struct DiagnosticCoordinator {
    system_manager: Option<SystemDependencyManager>,
    user_manager: UserDependencyManager,
}

impl DiagnosticCoordinator {
    /// Create a new diagnostic coordinator
    pub fn new() -> Self {
        let system_manager = SystemDependencyManager::new().ok();
        let user_manager = UserDependencyManager::new();

        Self {
            system_manager,
            user_manager,
        }
    }

    /// Perform comprehensive system health check
    pub async fn check_system_health(&self) -> Result<SystemHealth, DiagnosticError> {
        let mut issues = Vec::new();
        let mut recommendations = Vec::new();

        // Check system dependencies
        let system_dependencies = if let Some(ref manager) = self.system_manager {
            match manager.check_all_dependencies().await {
                Ok(deps) => {
                    // Check for missing or outdated dependencies
                    for dep in &deps {
                        if !dep.installed {
                            issues.push(HealthIssue {
                                severity: IssueSeverity::Error,
                                category: IssueCategory::SystemDependencies,
                                title: format!("Missing dependency: {}", dep.name),
                                description: format!(
                                    "Required system dependency '{}' is not installed",
                                    dep.name
                                ),
                                suggested_fix: Some(format!(
                                    "Install {} using your package manager",
                                    dep.name
                                )),
                            });
                        } else if dep.update_available {
                            issues.push(HealthIssue {
                                severity: IssueSeverity::Warning,
                                category: IssueCategory::SystemDependencies,
                                title: format!("Update available: {}", dep.name),
                                description: format!("An update is available for {}", dep.name),
                                suggested_fix: Some("Run system updates".to_string()),
                            });
                        }
                    }
                    deps
                }
                Err(e) => {
                    issues.push(HealthIssue {
                        severity: IssueSeverity::Critical,
                        category: IssueCategory::SystemDependencies,
                        title: "System dependency check failed".to_string(),
                        description: format!("Failed to check system dependencies: {}", e),
                        suggested_fix: Some(
                            "Check system configuration and permissions".to_string(),
                        ),
                    });
                    Vec::new()
                }
            }
        } else {
            issues.push(HealthIssue {
                severity: IssueSeverity::Warning,
                category: IssueCategory::SystemDependencies,
                title: "System dependency manager unavailable".to_string(),
                description: "Could not initialize system dependency manager".to_string(),
                suggested_fix: Some(
                    "Check if a supported package manager is installed".to_string(),
                ),
            });
            Vec::new()
        };

        // Check user configuration
        let user_configuration = match self.user_manager.check_all_user_dependencies().await {
            Ok(config) => {
                if !config.cli_installed {
                    issues.push(HealthIssue {
                        severity: IssueSeverity::Error,
                        category: IssueCategory::UserConfiguration,
                        title: "Solana CLI not installed".to_string(),
                        description: "Solana CLI is required for OSVM operations".to_string(),
                        suggested_fix: Some(
                            "Install Solana CLI using the official installer".to_string(),
                        ),
                    });
                    recommendations.push(
                        "Run 'osvm doctor --fix' to automatically install Solana CLI".to_string(),
                    );
                }

                if !config.config_dir_exists {
                    issues.push(HealthIssue {
                        severity: IssueSeverity::Warning,
                        category: IssueCategory::UserConfiguration,
                        title: "Solana config directory missing".to_string(),
                        description: "Solana configuration directory does not exist".to_string(),
                        suggested_fix: Some("Create configuration directory".to_string()),
                    });
                }

                if !config.keypair_exists {
                    issues.push(HealthIssue {
                        severity: IssueSeverity::Warning,
                        category: IssueCategory::UserConfiguration,
                        title: "Solana keypair missing".to_string(),
                        description: "No Solana keypair found".to_string(),
                        suggested_fix: Some(
                            "Generate a new keypair with 'solana-keygen new'".to_string(),
                        ),
                    });
                    recommendations.push(
                        "Run 'osvm doctor --fix' to automatically generate a keypair".to_string(),
                    );
                }

                config
            }
            Err(e) => {
                issues.push(HealthIssue {
                    severity: IssueSeverity::Critical,
                    category: IssueCategory::UserConfiguration,
                    title: "User configuration check failed".to_string(),
                    description: format!("Failed to check user configuration: {}", e),
                    suggested_fix: Some(
                        "Check file permissions and configuration paths".to_string(),
                    ),
                });

                // Return default config
                SolanaConfigInfo {
                    cli_installed: false,
                    cli_version: None,
                    config_dir_exists: false,
                    config_file_exists: false,
                    keypair_exists: false,
                    keypair_path: None,
                    current_network: None,
                }
            }
        };

        // Check network connectivity
        let network_connectivity = connectivity::check_network_health()
            .await
            .unwrap_or_else(|e| {
                issues.push(HealthIssue {
                    severity: IssueSeverity::Error,
                    category: IssueCategory::NetworkConnectivity,
                    title: "Network connectivity check failed".to_string(),
                    description: format!("Failed to check network connectivity: {}", e),
                    suggested_fix: Some(
                        "Check internet connection and firewall settings".to_string(),
                    ),
                });

                NetworkHealth {
                    internet_connected: false,
                    solana_mainnet_accessible: false,
                    solana_testnet_accessible: false,
                    solana_devnet_accessible: false,
                    response_times: std::collections::HashMap::new(),
                }
            });

        // Determine overall status
        let overall_status = self.determine_overall_status(&issues);

        // Add general recommendations
        if !issues.is_empty() {
            recommendations.push(
                "Use 'osvm doctor --verbose' for detailed diagnostic information".to_string(),
            );
        }

        Ok(SystemHealth {
            overall_status,
            system_dependencies,
            user_configuration,
            network_connectivity,
            issues,
            recommendations,
        })
    }

    /// Perform detailed diagnostic checks
    pub async fn run_detailed_diagnostics(&self) -> Result<DiagnosticResults, DiagnosticError> {
        let start_time = std::time::Instant::now();
        let mut detailed_checks = std::collections::HashMap::new();

        // System health check
        let system_health = self.check_system_health().await?;

        // Run individual detailed checks
        detailed_checks.insert(
            "rust_installation".to_string(),
            self.check_rust_installation().await,
        );

        detailed_checks.insert(
            "solana_installation".to_string(),
            self.check_solana_installation().await,
        );

        detailed_checks.insert("build_tools".to_string(), self.check_build_tools().await);

        detailed_checks.insert(
            "network_endpoints".to_string(),
            connectivity::check_solana_endpoints().await,
        );

        detailed_checks.insert(
            "file_permissions".to_string(),
            self.check_file_permissions().await,
        );

        detailed_checks.insert("disk_space".to_string(), self.check_disk_space().await);

        // Calculate summary
        let total_checks = detailed_checks.len();
        let passed_checks = detailed_checks.values().filter(|r| r.passed).count();
        let failed_checks = total_checks - passed_checks;

        let critical_issues = system_health
            .issues
            .iter()
            .filter(|i| i.severity == IssueSeverity::Critical || i.severity == IssueSeverity::Error)
            .count();

        let warnings = system_health
            .issues
            .iter()
            .filter(|i| i.severity == IssueSeverity::Warning)
            .count();

        let summary = DiagnosticSummary {
            total_checks,
            passed_checks,
            failed_checks,
            critical_issues,
            warnings,
            overall_health: system_health.overall_status.clone(),
        };

        Ok(DiagnosticResults {
            timestamp: chrono::Utc::now(),
            system_health,
            detailed_checks,
            summary,
        })
    }

    /// Determine overall system status based on issues
    fn determine_overall_status(&self, issues: &[HealthIssue]) -> HealthStatus {
        if issues
            .iter()
            .any(|i| matches!(i.severity, IssueSeverity::Critical | IssueSeverity::Error))
        {
            HealthStatus::Critical
        } else if issues.iter().any(|i| i.severity == IssueSeverity::Warning) {
            HealthStatus::Warning
        } else if issues.is_empty() {
            HealthStatus::Healthy
        } else {
            HealthStatus::Unknown
        }
    }

    /// Check Rust installation
    async fn check_rust_installation(&self) -> CheckResult {
        let start = std::time::Instant::now();

        if let Some(ref manager) = self.system_manager {
            match manager.check_rust_toolchain().await {
                Ok(rust_info) => {
                    if rust_info.installed {
                        CheckResult {
                            name: "Rust Installation".to_string(),
                            passed: true,
                            message: format!(
                                "Rust {} installed",
                                rust_info.version.unwrap_or_else(|| "unknown".to_string())
                            ),
                            details: Some(format!(
                                "Update available: {}",
                                rust_info.update_available
                            )),
                            execution_time_ms: start.elapsed().as_millis() as u64,
                        }
                    } else {
                        CheckResult {
                            name: "Rust Installation".to_string(),
                            passed: false,
                            message: "Rust is not installed".to_string(),
                            details: Some(
                                "Rust toolchain is required for building Solana programs"
                                    .to_string(),
                            ),
                            execution_time_ms: start.elapsed().as_millis() as u64,
                        }
                    }
                }
                Err(e) => CheckResult {
                    name: "Rust Installation".to_string(),
                    passed: false,
                    message: format!("Rust check failed: {}", e),
                    details: None,
                    execution_time_ms: start.elapsed().as_millis() as u64,
                },
            }
        } else {
            CheckResult {
                name: "Rust Installation".to_string(),
                passed: false,
                message: "System manager not available".to_string(),
                details: None,
                execution_time_ms: start.elapsed().as_millis() as u64,
            }
        }
    }

    /// Check Solana installation
    async fn check_solana_installation(&self) -> CheckResult {
        let start = std::time::Instant::now();

        match self.user_manager.check_all_user_dependencies().await {
            Ok(config) => CheckResult {
                name: "Solana Installation".to_string(),
                passed: config.cli_installed,
                message: if config.cli_installed {
                    format!(
                        "Solana CLI {} installed",
                        config.cli_version.unwrap_or_else(|| "unknown".to_string())
                    )
                } else {
                    "Solana CLI is not installed".to_string()
                },
                details: Some(format!(
                    "Config dir: {}, Keypair: {}, Network: {}",
                    config.config_dir_exists,
                    config.keypair_exists,
                    config
                        .current_network
                        .unwrap_or_else(|| "not configured".to_string())
                )),
                execution_time_ms: start.elapsed().as_millis() as u64,
            },
            Err(e) => CheckResult {
                name: "Solana Installation".to_string(),
                passed: false,
                message: format!("Solana check failed: {}", e),
                details: None,
                execution_time_ms: start.elapsed().as_millis() as u64,
            },
        }
    }

    /// Check build tools
    async fn check_build_tools(&self) -> CheckResult {
        let start = std::time::Instant::now();

        if let Some(ref manager) = self.system_manager {
            let build_deps = crate::utils::self_repair::package_managers::get_build_dependencies();
            let mut missing_tools = Vec::new();
            let mut installed_tools = Vec::new();

            for tool in build_deps {
                match manager.check_system_package(tool).await {
                    Ok(info) => {
                        if info.installed {
                            installed_tools.push(tool);
                        } else {
                            missing_tools.push(tool);
                        }
                    }
                    Err(_) => missing_tools.push(tool),
                }
            }

            CheckResult {
                name: "Build Tools".to_string(),
                passed: missing_tools.is_empty(),
                message: if missing_tools.is_empty() {
                    "All build tools are installed".to_string()
                } else {
                    format!("Missing build tools: {:?}", missing_tools)
                },
                details: Some(format!(
                    "Installed: {:?}, Missing: {:?}",
                    installed_tools, missing_tools
                )),
                execution_time_ms: start.elapsed().as_millis() as u64,
            }
        } else {
            CheckResult {
                name: "Build Tools".to_string(),
                passed: false,
                message: "System manager not available".to_string(),
                details: None,
                execution_time_ms: start.elapsed().as_millis() as u64,
            }
        }
    }

    /// Check file permissions
    async fn check_file_permissions(&self) -> CheckResult {
        let start = std::time::Instant::now();

        let config_dir = self.user_manager.get_solana_config_dir();
        let osvm_config_dir = self.user_manager.get_osvm_config_dir();

        let mut issues = Vec::new();

        // Check if we can create directories
        if let Err(e) = std::fs::create_dir_all(&config_dir) {
            issues.push(format!("Cannot create Solana config directory: {}", e));
        }

        if let Err(e) = std::fs::create_dir_all(&osvm_config_dir) {
            issues.push(format!("Cannot create OSVM config directory: {}", e));
        }

        CheckResult {
            name: "File Permissions".to_string(),
            passed: issues.is_empty(),
            message: if issues.is_empty() {
                "File permissions are correct".to_string()
            } else {
                format!("Permission issues: {}", issues.join(", "))
            },
            details: Some(format!(
                "Solana config: {}, OSVM config: {}",
                config_dir.display(),
                osvm_config_dir.display()
            )),
            execution_time_ms: start.elapsed().as_millis() as u64,
        }
    }

    /// Check available disk space
    async fn check_disk_space(&self) -> CheckResult {
        let start = std::time::Instant::now();

        // Use `df` command to check disk space
        match std::process::Command::new("df").arg("-h").arg(".").output() {
            Ok(output) if output.status.success() => {
                let output_str = String::from_utf8_lossy(&output.stdout);
                CheckResult {
                    name: "Disk Space".to_string(),
                    passed: true,
                    message: "Disk space information available".to_string(),
                    details: Some(output_str.lines().take(2).collect::<Vec<_>>().join("\n")),
                    execution_time_ms: start.elapsed().as_millis() as u64,
                }
            }
            _ => CheckResult {
                name: "Disk Space".to_string(),
                passed: false,
                message: "Could not check disk space".to_string(),
                details: None,
                execution_time_ms: start.elapsed().as_millis() as u64,
            },
        }
    }
}

impl SystemHealth {
    /// Check if the system has package updates available
    pub fn has_package_updates(&self) -> bool {
        self.system_dependencies
            .iter()
            .any(|dep| dep.update_available)
    }

    /// Check if the system has Rust updates available
    pub fn has_rust_updates(&self) -> bool {
        self.system_dependencies
            .iter()
            .any(|dep| dep.name == "rust" && dep.update_available)
    }

    /// Get missing build tools
    pub fn missing_build_tools(&self) -> Option<Vec<String>> {
        let missing: Vec<String> = self
            .system_dependencies
            .iter()
            .filter(|dep| !dep.installed)
            .map(|dep| dep.name.clone())
            .collect();

        if missing.is_empty() {
            None
        } else {
            Some(missing)
        }
    }

    /// Check if the system is healthy overall
    pub fn is_healthy(&self) -> bool {
        matches!(self.overall_status, HealthStatus::Healthy)
    }
}

impl Default for DiagnosticCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_diagnostic_coordinator_creation() {
        let coordinator = DiagnosticCoordinator::new();
        // Should not panic during creation
    }

    #[test]
    fn test_health_status_determination() {
        let coordinator = DiagnosticCoordinator::new();

        let critical_issues = vec![HealthIssue {
            severity: IssueSeverity::Critical,
            category: IssueCategory::SystemDependencies,
            title: "Critical issue".to_string(),
            description: "Test".to_string(),
            suggested_fix: None,
        }];

        let status = coordinator.determine_overall_status(&critical_issues);
        assert_eq!(status, HealthStatus::Critical);
    }

    #[test]
    fn test_system_health_helper_methods() {
        let system_health = SystemHealth {
            overall_status: HealthStatus::Healthy,
            system_dependencies: vec![
                DependencyInfo {
                    name: "rust".to_string(),
                    installed: true,
                    version: Some("1.76.0".to_string()),
                    required_version: None,
                    update_available: true,
                },
                DependencyInfo {
                    name: "build-essential".to_string(),
                    installed: false,
                    version: None,
                    required_version: None,
                    update_available: false,
                },
            ],
            user_configuration: SolanaConfigInfo {
                cli_installed: true,
                cli_version: Some("1.18.0".to_string()),
                config_dir_exists: true,
                config_file_exists: true,
                keypair_exists: true,
                keypair_path: Some("/test/path".to_string()),
                current_network: Some("mainnet".to_string()),
            },
            network_connectivity: NetworkHealth {
                internet_connected: true,
                solana_mainnet_accessible: true,
                solana_testnet_accessible: true,
                solana_devnet_accessible: true,
                response_times: std::collections::HashMap::new(),
            },
            issues: Vec::new(),
            recommendations: Vec::new(),
        };

        assert!(system_health.has_rust_updates());
        assert_eq!(
            system_health.missing_build_tools(),
            Some(vec!["build-essential".to_string()])
        );
        assert!(system_health.is_healthy());
    }
}
