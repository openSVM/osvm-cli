//! System health assessment utilities
//!
//! This module provides detailed system health assessment capabilities
//! and health status management.

use super::{HealthIssue, HealthStatus, IssueCategory, IssueSeverity, SystemHealth};
use std::collections::HashMap;
use std::process::Command;

/// System health analyzer
pub struct SystemHealthAnalyzer;

impl SystemHealthAnalyzer {
    /// Create a new system health analyzer
    pub fn new() -> Self {
        Self
    }

    /// Analyze system health from collected data
    pub fn analyze_health(
        &self,
        system_deps: &[crate::utils::self_repair::system_deps::DependencyInfo],
        user_config: &crate::utils::self_repair::user_deps::SolanaConfigInfo,
        network_health: &super::NetworkHealth,
    ) -> SystemHealth {
        let mut issues = Vec::new();
        let mut recommendations = Vec::new();

        // Check system tuning parameters FIRST (new)
        self.check_system_tuning(&mut issues, &mut recommendations);

        // Analyze system dependencies
        for dep in system_deps {
            if !dep.installed {
                issues.push(HealthIssue {
                    severity: IssueSeverity::Error,
                    category: IssueCategory::SystemDependencies,
                    title: format!("Missing dependency: {}", dep.name),
                    description: format!(
                        "Required system dependency '{}' is not installed",
                        dep.name
                    ),
                    suggested_fix: Some(format!("Install {} using your package manager", dep.name)),
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

        // Analyze user configuration
        if !user_config.cli_installed {
            issues.push(HealthIssue {
                severity: IssueSeverity::Error,
                category: IssueCategory::UserConfiguration,
                title: "Solana CLI not installed".to_string(),
                description: "Solana CLI is required for OSVM operations".to_string(),
                suggested_fix: Some("Install Solana CLI using the official installer".to_string()),
            });
            recommendations
                .push("Run 'osvm doctor --fix' to automatically install Solana CLI".to_string());
        }

        if !user_config.config_dir_exists {
            issues.push(HealthIssue {
                severity: IssueSeverity::Warning,
                category: IssueCategory::UserConfiguration,
                title: "Solana config directory missing".to_string(),
                description: "Solana configuration directory does not exist".to_string(),
                suggested_fix: Some("Create configuration directory".to_string()),
            });
        }

        if !user_config.keypair_exists {
            issues.push(HealthIssue {
                severity: IssueSeverity::Warning,
                category: IssueCategory::UserConfiguration,
                title: "Solana keypair missing".to_string(),
                description: "No Solana keypair found".to_string(),
                suggested_fix: Some("Generate a new keypair with 'solana-keygen new'".to_string()),
            });
            recommendations
                .push("Run 'osvm doctor --fix' to automatically generate a keypair".to_string());
        }

        // Analyze network connectivity
        if !network_health.internet_connected {
            issues.push(HealthIssue {
                severity: IssueSeverity::Error,
                category: IssueCategory::NetworkConnectivity,
                title: "No internet connection".to_string(),
                description: "Internet connectivity is required for Solana operations".to_string(),
                suggested_fix: Some("Check network connection and firewall settings".to_string()),
            });
        }

        let solana_endpoints_down = !network_health.solana_mainnet_accessible
            && !network_health.solana_testnet_accessible
            && !network_health.solana_devnet_accessible;

        if solana_endpoints_down {
            issues.push(HealthIssue {
                severity: IssueSeverity::Error,
                category: IssueCategory::NetworkConnectivity,
                title: "Solana endpoints unreachable".to_string(),
                description: "All Solana network endpoints are unreachable".to_string(),
                suggested_fix: Some("Check firewall settings and network connectivity".to_string()),
            });
        } else {
            // Check individual endpoints
            if !network_health.solana_mainnet_accessible {
                issues.push(HealthIssue {
                    severity: IssueSeverity::Warning,
                    category: IssueCategory::NetworkConnectivity,
                    title: "Solana mainnet unreachable".to_string(),
                    description: "Solana mainnet endpoint is not accessible".to_string(),
                    suggested_fix: Some("Check network connectivity to mainnet".to_string()),
                });
            }

            if !network_health.solana_testnet_accessible {
                issues.push(HealthIssue {
                    severity: IssueSeverity::Warning,
                    category: IssueCategory::NetworkConnectivity,
                    title: "Solana testnet unreachable".to_string(),
                    description: "Solana testnet endpoint is not accessible".to_string(),
                    suggested_fix: Some("Check network connectivity to testnet".to_string()),
                });
            }

            if !network_health.solana_devnet_accessible {
                issues.push(HealthIssue {
                    severity: IssueSeverity::Warning,
                    category: IssueCategory::NetworkConnectivity,
                    title: "Solana devnet unreachable".to_string(),
                    description: "Solana devnet endpoint is not accessible".to_string(),
                    suggested_fix: Some("Check network connectivity to devnet".to_string()),
                });
            }
        }

        // Performance checks
        self.check_performance_issues(&network_health.response_times, &mut issues);

        // Security checks
        self.check_security_issues(user_config, &mut issues);

        // Determine overall status
        let overall_status = self.determine_overall_status(&issues);

        // Add general recommendations
        if !issues.is_empty() {
            recommendations.push(
                "Use 'osvm doctor --verbose' for detailed diagnostic information".to_string(),
            );
        }

        SystemHealth {
            overall_status,
            system_dependencies: system_deps.to_vec(),
            user_configuration: user_config.clone(),
            network_connectivity: network_health.clone(),
            issues,
            recommendations,
        }
    }

    /// Check system tuning parameters for Solana validator requirements
    fn check_system_tuning(
        &self,
        issues: &mut Vec<HealthIssue>,
        recommendations: &mut Vec<String>,
    ) {
        // Define required kernel parameters
        let required_params = vec![
            ("net.core.rmem_max", 134217728u64, "134217728"),
            ("net.core.rmem_default", 134217728u64, "134217728"),
            ("net.core.wmem_max", 134217728u64, "134217728"),
            ("net.core.wmem_default", 134217728u64, "134217728"),
            ("vm.max_map_count", 2000000u64, "2000000"),
        ];

        for (param, min_value, recommended) in required_params {
            match self.check_kernel_parameter(param) {
                Ok(current_value) => {
                    if current_value < min_value {
                        issues.push(HealthIssue {
                            severity: IssueSeverity::Error,
                            category: IssueCategory::SystemDependencies,
                            title: format!("System tuning: {} too low", param),
                            description: format!(
                                "Kernel parameter {} is set to {}, but Solana requires at least {}",
                                param, current_value, min_value
                            ),
                            suggested_fix: Some(format!(
                                "Run: sudo sysctl -w {}={}",
                                param, recommended
                            )),
                        });
                        recommendations.push(
                            "Run 'osvm doctor --fix' to automatically tune system parameters"
                                .to_string(),
                        );
                    }
                }
                Err(_) => {
                    issues.push(HealthIssue {
                        severity: IssueSeverity::Warning,
                        category: IssueCategory::SystemDependencies,
                        title: format!("Cannot check system parameter: {}", param),
                        description: format!("Unable to read kernel parameter {}", param),
                        suggested_fix: Some(
                            "Check system permissions or kernel configuration".to_string(),
                        ),
                    });
                }
            }
        }

        // Check file descriptor limits
        match self.check_file_descriptor_limit() {
            Ok(limit) => {
                if limit < 1000000 {
                    issues.push(HealthIssue {
                        severity: IssueSeverity::Warning,
                        category: IssueCategory::SystemDependencies,
                        title: "File descriptor limit too low".to_string(),
                        description: format!(
                            "Current file descriptor limit is {}, Solana recommends at least 1,000,000",
                            limit
                        ),
                        suggested_fix: Some("Update /etc/security/limits.conf or systemd limits".to_string()),
                    });
                }
            }
            Err(_) => {
                // Silently ignore if we can't check
            }
        }
    }

    /// Check a specific kernel parameter value
    fn check_kernel_parameter(&self, param: &str) -> Result<u64, String> {
        let output = Command::new("sysctl")
            .arg("-n")
            .arg(param)
            .output()
            .map_err(|e| format!("Failed to run sysctl: {}", e))?;

        if !output.status.success() {
            return Err("sysctl command failed".to_string());
        }

        let value_str = String::from_utf8_lossy(&output.stdout).trim().to_string();
        value_str
            .parse::<u64>()
            .map_err(|e| format!("Failed to parse value: {}", e))
    }

    /// Check file descriptor limits
    fn check_file_descriptor_limit(&self) -> Result<u64, String> {
        let output = Command::new("ulimit")
            .arg("-n")
            .output()
            .map_err(|e| format!("Failed to run ulimit: {}", e))?;

        if !output.status.success() {
            return Err("ulimit command failed".to_string());
        }

        let value_str = String::from_utf8_lossy(&output.stdout).trim().to_string();
        value_str
            .parse::<u64>()
            .map_err(|e| format!("Failed to parse value: {}", e))
    }

    /// Check for performance-related issues
    fn check_performance_issues(
        &self,
        response_times: &HashMap<String, u64>,
        issues: &mut Vec<HealthIssue>,
    ) {
        for (endpoint, &response_time) in response_times {
            if response_time > 5000 {
                // 5 seconds
                issues.push(HealthIssue {
                    severity: IssueSeverity::Warning,
                    category: IssueCategory::Performance,
                    title: format!("Slow network response: {}", endpoint),
                    description: format!(
                        "Network response time to {} is {} ms",
                        endpoint, response_time
                    ),
                    suggested_fix: Some(
                        "Check network connectivity and consider using a different RPC endpoint"
                            .to_string(),
                    ),
                });
            } else if response_time > 10000 {
                // 10 seconds
                issues.push(HealthIssue {
                    severity: IssueSeverity::Error,
                    category: IssueCategory::Performance,
                    title: format!("Very slow network response: {}", endpoint),
                    description: format!(
                        "Network response time to {} is {} ms",
                        endpoint, response_time
                    ),
                    suggested_fix: Some(
                        "Network performance is severely degraded - check connection".to_string(),
                    ),
                });
            }
        }
    }

    /// Check for security-related issues
    fn check_security_issues(
        &self,
        user_config: &crate::utils::self_repair::user_deps::SolanaConfigInfo,
        issues: &mut Vec<HealthIssue>,
    ) {
        // Check if keypair file has appropriate permissions
        if let Some(ref keypair_path) = user_config.keypair_path {
            if std::path::Path::new(keypair_path).exists() {
                if let Ok(metadata) = std::fs::metadata(keypair_path) {
                    // On Unix systems, check file permissions
                    #[cfg(unix)]
                    {
                        use std::os::unix::fs::PermissionsExt;
                        let mode = metadata.permissions().mode();
                        let others_readable = (mode & 0o044) != 0;
                        let group_writable = (mode & 0o020) != 0;
                        let others_writable = (mode & 0o002) != 0;

                        if others_readable || group_writable || others_writable {
                            issues.push(HealthIssue {
                                severity: IssueSeverity::Warning,
                                category: IssueCategory::Security,
                                title: "Keypair file permissions too permissive".to_string(),
                                description: format!(
                                    "Keypair file {} has overly permissive permissions",
                                    keypair_path
                                ),
                                suggested_fix: Some(format!("Run: chmod 600 {}", keypair_path)),
                            });
                        }
                    }
                }
            }
        }

        // Check if using default/development networks inappropriately
        if let Some(ref network) = user_config.current_network {
            if network == "devnet" {
                issues.push(HealthIssue {
                    severity: IssueSeverity::Info,
                    category: IssueCategory::Security,
                    title: "Using development network".to_string(),
                    description: "Currently configured to use Solana devnet".to_string(),
                    suggested_fix: Some(
                        "Consider switching to mainnet for production use".to_string(),
                    ),
                });
            }
        }
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

    /// Get health status as a color-coded emoji
    pub fn get_status_emoji(status: &HealthStatus) -> &'static str {
        match status {
            HealthStatus::Healthy => "🟢",
            HealthStatus::Warning => "🟡",
            HealthStatus::Critical => "🔴",
            HealthStatus::Unknown => "⚪",
        }
    }

    /// Get health status as a descriptive string
    pub fn get_status_description(status: &HealthStatus) -> &'static str {
        match status {
            HealthStatus::Healthy => "All systems operational",
            HealthStatus::Warning => "Minor issues detected",
            HealthStatus::Critical => "Critical issues require attention",
            HealthStatus::Unknown => "System status unknown",
        }
    }

    /// Generate a health score (0-100)
    pub fn calculate_health_score(&self, health: &SystemHealth) -> u8 {
        let mut score = 100u8;

        for issue in &health.issues {
            let deduction = match issue.severity {
                IssueSeverity::Critical => 25,
                IssueSeverity::Error => 15,
                IssueSeverity::Warning => 5,
                IssueSeverity::Info => 1,
            };
            score = score.saturating_sub(deduction);
        }

        score
    }

    /// Get recommendations for improving system health
    pub fn get_improvement_recommendations(&self, health: &SystemHealth) -> Vec<String> {
        let mut recommendations = health.recommendations.clone();

        // Add category-specific recommendations
        let has_system_issues = health
            .issues
            .iter()
            .any(|i| matches!(i.category, IssueCategory::SystemDependencies));
        let has_user_issues = health
            .issues
            .iter()
            .any(|i| matches!(i.category, IssueCategory::UserConfiguration));
        let has_network_issues = health
            .issues
            .iter()
            .any(|i| matches!(i.category, IssueCategory::NetworkConnectivity));

        if has_system_issues {
            recommendations.push("Consider updating system packages regularly".to_string());
        }

        if has_user_issues {
            recommendations.push(
                "Review user configuration and ensure all required tools are installed".to_string(),
            );
        }

        if has_network_issues {
            recommendations.push("Check firewall settings and network connectivity".to_string());
        }

        // Add proactive recommendations
        if health.overall_status == HealthStatus::Healthy {
            recommendations.push(
                "System is healthy - consider running 'osvm doctor' periodically".to_string(),
            );
            recommendations.push("Keep system dependencies up to date".to_string());
        }

        recommendations
    }
}

impl Default for SystemHealthAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::self_repair::{system_deps::DependencyInfo, user_deps::SolanaConfigInfo};

    #[test]
    fn test_system_health_analyzer_creation() {
        let analyzer = SystemHealthAnalyzer::new();
        // Should not panic
    }

    #[test]
    fn test_health_status_emoji() {
        assert_eq!(
            SystemHealthAnalyzer::get_status_emoji(&HealthStatus::Healthy),
            "🟢"
        );
        assert_eq!(
            SystemHealthAnalyzer::get_status_emoji(&HealthStatus::Warning),
            "🟡"
        );
        assert_eq!(
            SystemHealthAnalyzer::get_status_emoji(&HealthStatus::Critical),
            "🔴"
        );
        assert_eq!(
            SystemHealthAnalyzer::get_status_emoji(&HealthStatus::Unknown),
            "⚪"
        );
    }

    #[test]
    fn test_health_status_description() {
        assert_eq!(
            SystemHealthAnalyzer::get_status_description(&HealthStatus::Healthy),
            "All systems operational"
        );
        assert_eq!(
            SystemHealthAnalyzer::get_status_description(&HealthStatus::Warning),
            "Minor issues detected"
        );
        assert_eq!(
            SystemHealthAnalyzer::get_status_description(&HealthStatus::Critical),
            "Critical issues require attention"
        );
    }

    #[test]
    fn test_health_score_calculation() {
        let analyzer = SystemHealthAnalyzer::new();

        // Healthy system
        let healthy_system = SystemHealth {
            overall_status: HealthStatus::Healthy,
            system_dependencies: Vec::new(),
            user_configuration: SolanaConfigInfo {
                cli_installed: true,
                cli_version: Some("1.18.0".to_string()),
                config_dir_exists: true,
                config_file_exists: true,
                keypair_exists: true,
                keypair_path: Some("/test/path".to_string()),
                current_network: Some("mainnet".to_string()),
            },
            network_connectivity: crate::utils::diagnostics::NetworkHealth {
                internet_connected: true,
                solana_mainnet_accessible: true,
                solana_testnet_accessible: true,
                solana_devnet_accessible: true,
                response_times: std::collections::HashMap::new(),
            },
            issues: Vec::new(),
            recommendations: Vec::new(),
        };

        assert_eq!(analyzer.calculate_health_score(&healthy_system), 100);

        // System with warning
        let mut warning_system = healthy_system.clone();
        warning_system.issues.push(HealthIssue {
            severity: IssueSeverity::Warning,
            category: IssueCategory::SystemDependencies,
            title: "Test warning".to_string(),
            description: "Test".to_string(),
            suggested_fix: None,
        });

        assert_eq!(analyzer.calculate_health_score(&warning_system), 95);
    }

    #[test]
    fn test_overall_status_determination() {
        let analyzer = SystemHealthAnalyzer::new();

        // No issues
        let no_issues = Vec::new();
        assert_eq!(
            analyzer.determine_overall_status(&no_issues),
            HealthStatus::Healthy
        );

        // Warning issue
        let warning_issues = vec![HealthIssue {
            severity: IssueSeverity::Warning,
            category: IssueCategory::SystemDependencies,
            title: "Test".to_string(),
            description: "Test".to_string(),
            suggested_fix: None,
        }];
        assert_eq!(
            analyzer.determine_overall_status(&warning_issues),
            HealthStatus::Warning
        );

        // Critical issue
        let critical_issues = vec![HealthIssue {
            severity: IssueSeverity::Critical,
            category: IssueCategory::SystemDependencies,
            title: "Test".to_string(),
            description: "Test".to_string(),
            suggested_fix: None,
        }];
        assert_eq!(
            analyzer.determine_overall_status(&critical_issues),
            HealthStatus::Critical
        );
    }
}
