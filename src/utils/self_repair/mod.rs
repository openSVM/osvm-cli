//! OSVM Self-Repair System
//!
//! This module provides comprehensive self-repair functionality for OSVM CLI,
//! including automatic dependency detection, system repairs, and rollback capabilities.

use serde::{Deserialize, Serialize};
use std::error::Error as StdError;
use std::fmt;
use tokio::time::Duration;

pub mod package_managers;
pub mod repair_strategies;
// pub mod snapshots;  // REMOVED
pub mod system_deps;
pub mod user_deps;

/// Main error type for self-repair operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RepairError {
    /// System-level dependency issues
    SystemDependency(String),
    /// User-level dependency issues
    UserDependency(String),
    /// Network connectivity problems
    NetworkError(String),
    /// Permission-related errors
    PermissionError(String),
    /// Rollback operation failures
    RollbackError(String),
    /// Snapshot operation failures - REMOVED
    // SnapshotError(String),
    /// Validation failures
    ValidationError(String),
    /// Unknown or unexpected errors
    Unknown(String),
}

impl fmt::Display for RepairError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RepairError::SystemDependency(msg) => write!(f, "System dependency error: {}", msg),
            RepairError::UserDependency(msg) => write!(f, "User dependency error: {}", msg),
            RepairError::NetworkError(msg) => write!(f, "Network error: {}", msg),
            RepairError::PermissionError(msg) => write!(f, "Permission error: {}", msg),
            RepairError::RollbackError(msg) => write!(f, "Rollback error: {}", msg),
            // RepairError::SnapshotError(msg) => write!(f, "Snapshot error: {}", msg),
            RepairError::ValidationError(msg) => write!(f, "Validation error: {}", msg),
            RepairError::Unknown(msg) => write!(f, "Unknown error: {}", msg),
        }
    }
}

impl StdError for RepairError {}

/// Types of errors that can be automatically repaired
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RepairableError {
    // System-level issues
    OutdatedSystemPackages,
    MissingBuildTools,
    OutdatedRustToolchain,
    MissingSystemDependencies(Vec<String>),

    // User-level issues
    MissingSolanaCli,
    OutdatedSolanaCli,
    MissingKeypair(String),
    InvalidConfig,
    MissingConfigDirectory,

    // Network-level issues
    ConnectivityIssues,
    RpcEndpointFailure(String),

    // Permission issues
    InsufficientPermissions(String),

    // System tuning issues
    SystemTuningRequired,
}

/// Result of a repair operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RepairResult {
    Success(String),
    Partial(String, Vec<RepairableError>),
    Failed(RepairError),
    RequiresManualIntervention(String),
}

/// Configuration for repair operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepairConfig {
    /// Whether to prompt user for confirmation before repairs
    pub interactive: bool,
    /// Automatically create snapshots before repairs - REMOVED
    // pub auto_snapshot: bool,
    /// Maximum time to wait for operations
    pub timeout: Duration,
    /// Whether to perform system-level repairs
    pub allow_system_repairs: bool,
    /// Whether to perform user-level repairs
    pub allow_user_repairs: bool,
    // pub backup_dir: Option<String>,  // REMOVED: Backup directory for snapshots
}

impl Default for RepairConfig {
    fn default() -> Self {
        Self {
            interactive: true,
            // auto_snapshot: true,  // REMOVED
            timeout: Duration::from_secs(300), // 5 minutes
            allow_system_repairs: true,
            allow_user_repairs: true,
            // backup_dir: None,  // REMOVED
        }
    }
}

/// Main self-repair orchestrator
pub struct SelfRepairSystem {
    config: RepairConfig,
    diagnostics: crate::utils::diagnostics::DiagnosticCoordinator,
}

impl SelfRepairSystem {
    /// Create a new self-repair system
    pub fn new(config: RepairConfig) -> Self {
        Self {
            config,
            diagnostics: crate::utils::diagnostics::DiagnosticCoordinator::new(),
        }
    }

    /// Create a new self-repair system with default configuration
    pub fn default() -> Self {
        Self::new(RepairConfig::default())
    }

    /// Analyze an error and determine if it can be automatically repaired
    pub async fn analyze_error(&self, error: &str) -> Result<Vec<RepairableError>, RepairError> {
        let mut repairable_errors = Vec::new();

        // Check for system tuning errors
        if error.contains("net.core.rmem_max") && error.contains("too small") {
            repairable_errors.push(RepairableError::SystemTuningRequired);
        }

        // Check for keypair file errors
        if error.contains("Error reading keypair file")
            && error.contains("No such file or directory")
        {
            // Extract the keypair path from the error message
            if let Some(path) = extract_keypair_path(error) {
                repairable_errors.push(RepairableError::MissingKeypair(path));
            }

            // Check if this is due to missing Solana CLI
            if !self.is_solana_cli_installed().await? {
                repairable_errors.push(RepairableError::MissingSolanaCli);
            }

            // Check if config directory exists
            if !self.config_directory_exists().await? {
                repairable_errors.push(RepairableError::MissingConfigDirectory);
            }
        }

        // Check for other system-level issues
        let system_health = self
            .diagnostics
            .check_system_health()
            .await
            .map_err(|e| RepairError::ValidationError(e.to_string()))?;

        // Check if system tuning is needed based on health check
        if system_health.issues.iter().any(|issue| {
            issue.title.contains("System tuning") || issue.description.contains("kernel parameter")
        }) {
            repairable_errors.push(RepairableError::SystemTuningRequired);
        }

        if system_health.has_package_updates() {
            repairable_errors.push(RepairableError::OutdatedSystemPackages);
        }

        if system_health.has_rust_updates() {
            repairable_errors.push(RepairableError::OutdatedRustToolchain);
        }

        if let Some(missing_deps) = system_health.missing_build_tools() {
            if !missing_deps.is_empty() {
                repairable_errors.push(RepairableError::MissingSystemDependencies(missing_deps));
            }
        }

        Ok(repairable_errors)
    }

    /// Attempt to repair the detected issues automatically
    pub async fn repair_automatically(
        &self,
        errors: Vec<RepairableError>,
    ) -> Result<RepairResult, RepairError> {
        if errors.is_empty() {
            return Ok(RepairResult::Success("No issues detected".to_string()));
        }

        // Create snapshot if configured
        // DISABLED: Commenting out snapshot creation as requested
        // let snapshot_id = if self.config.auto_snapshot {
        //     Some(self.create_system_snapshot().await?)
        // } else {
        //     None
        // };
        let snapshot_id: Option<String> = None;

        let mut repair_transaction = repair_strategies::RepairTransaction::new()
            .map_err(|e| RepairError::Unknown(e.to_string()))?;

        // Add operations based on detected errors
        for error in &errors {
            self.add_repair_operation(&mut repair_transaction, error)
                .await?;
        }

        // Execute the repair transaction
        match repair_transaction.execute().await {
            Ok(_) => {
                // Validate the repairs
                let validation_result = self.validate_repairs(&errors).await?;
                match validation_result {
                    true => Ok(RepairResult::Success(
                        "All repairs completed successfully".to_string(),
                    )),
                    false => {
                        // Rollback if validation fails
                        // DISABLED: No rollback since no snapshot was created
                        // if let Some(snap_id) = snapshot_id {
                        //     self.rollback_to_snapshot(&snap_id).await?;
                        // }
                        Ok(RepairResult::Failed(RepairError::ValidationError(
                            "Repairs failed validation (no rollback available - snapshots disabled)".to_string()
                        )))
                    }
                }
            }
            Err(e) => {
                // Rollback on error
                // DISABLED: No rollback since no snapshot was created
                // if let Some(snap_id) = snapshot_id {
                //     self.rollback_to_snapshot(&snap_id).await?;
                // }
                Ok(RepairResult::Failed(e))
            }
        }
    }

    /// Check if Solana CLI is installed
    async fn is_solana_cli_installed(&self) -> Result<bool, RepairError> {
        system_deps::check_solana_cli()
            .await
            .map_err(|e| RepairError::SystemDependency(e.to_string()))
    }

    /// Check if config directory exists
    async fn config_directory_exists(&self) -> Result<bool, RepairError> {
        user_deps::check_config_directory()
            .await
            .map_err(|e| RepairError::UserDependency(e.to_string()))
    }

    /// Add repair operation based on error type
    async fn add_repair_operation(
        &self,
        transaction: &mut repair_strategies::RepairTransaction,
        error: &RepairableError,
    ) -> Result<(), RepairError> {
        match error {
            RepairableError::MissingSolanaCli => {
                transaction.add_operation(repair_strategies::RepairOperation::InstallSolanaCli);
            }
            RepairableError::MissingKeypair(path) => {
                transaction.add_operation(repair_strategies::RepairOperation::GenerateKeypair(
                    path.clone(),
                ));
            }
            RepairableError::MissingConfigDirectory => {
                transaction
                    .add_operation(repair_strategies::RepairOperation::CreateConfigDirectory);
            }
            RepairableError::SystemTuningRequired => {
                if self.config.allow_system_repairs {
                    transaction
                        .add_operation(repair_strategies::RepairOperation::TuneSystemParameters);
                }
            }
            RepairableError::OutdatedSystemPackages => {
                if self.config.allow_system_repairs {
                    transaction
                        .add_operation(repair_strategies::RepairOperation::UpdateSystemPackages);
                }
            }
            RepairableError::OutdatedRustToolchain => {
                transaction.add_operation(repair_strategies::RepairOperation::UpdateRustToolchain);
            }
            RepairableError::MissingSystemDependencies(deps) => {
                if self.config.allow_system_repairs {
                    transaction.add_operation(
                        repair_strategies::RepairOperation::InstallSystemDependencies(deps.clone()),
                    );
                }
            }
            _ => {
                return Err(RepairError::Unknown(format!(
                    "Unsupported repair type: {:?}",
                    error
                )));
            }
        }
        Ok(())
    }

    /// Validate that repairs were successful
    async fn validate_repairs(&self, _errors: &[RepairableError]) -> Result<bool, RepairError> {
        // Perform comprehensive health check after repairs
        let health_check = self
            .diagnostics
            .check_system_health()
            .await
            .map_err(|e| RepairError::ValidationError(e.to_string()))?;

        Ok(health_check.is_healthy())
    }
}

/// Extract keypair path from error message
fn extract_keypair_path(error: &str) -> Option<String> {
    // Look for pattern: "Error reading keypair file <path>:"
    if let Some(start) = error.find("Error reading keypair file ") {
        let start = start + "Error reading keypair file ".len();
        if let Some(end) = error[start..].find(':') {
            return Some(error[start..start + end].to_string());
        }
    }
    None
}

/// Convenience function to handle keypair reading with automatic repair
pub async fn read_keypair_with_repair(
    keypair_path: &str,
) -> Result<solana_sdk::signature::Keypair, Box<dyn StdError>> {
    // First try normal keypair reading
    match solana_sdk::signature::read_keypair_file(keypair_path) {
        Ok(keypair) => Ok(keypair),
        Err(err) => {
            // Error detected, attempt self-repair
            let repair_system = SelfRepairSystem::default();
            let error_message = format!("Error reading keypair file {}: {}", keypair_path, err);

            println!("🔧 OSVM detected a configuration issue. Attempting automatic repair...");

            match repair_system.analyze_error(&error_message).await {
                Ok(repairable_errors) => {
                    if !repairable_errors.is_empty() {
                        // Show repair plan to user
                        println!("📋 Issues detected:");
                        for error in &repairable_errors {
                            println!("  - {:?}", error);
                        }

                        println!(
                            "\n🛠️  Would you like OSVM to fix these issues automatically? (Y/n):"
                        );
                        let mut input = String::new();
                        std::io::stdin().read_line(&mut input).unwrap();

                        if input.trim().to_lowercase() != "n" {
                            match repair_system.repair_automatically(repairable_errors).await {
                                Ok(RepairResult::Success(msg)) => {
                                    println!("✅ {}", msg);
                                    println!("🔄 Retrying keypair read...");

                                    // Retry reading the keypair
                                    match solana_sdk::signature::read_keypair_file(keypair_path) {
                                        Ok(keypair) => return Ok(keypair),
                                        Err(retry_err) => {
                                            return Err(format!("Repair succeeded but keypair still not readable: {}", retry_err).into());
                                        }
                                    }
                                }
                                Ok(result) => {
                                    return Err(format!("Repair incomplete: {:?}", result).into());
                                }
                                Err(repair_err) => {
                                    return Err(format!("Repair failed: {}", repair_err).into());
                                }
                            }
                        }
                    }
                }
                Err(analysis_err) => {
                    return Err(format!("Failed to analyze error: {}", analysis_err).into());
                }
            }

            // If we get here, repair was declined or failed
            Err(err.into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_keypair_path() {
        let error = "Error reading keypair file /home/user/.config/solana/id.json: No such file or directory";
        let path = extract_keypair_path(error);
        assert_eq!(path, Some("/home/user/.config/solana/id.json".to_string()));
    }

    #[test]
    fn test_extract_keypair_path_no_match() {
        let error = "Some other error message";
        let path = extract_keypair_path(error);
        assert_eq!(path, None);
    }

    #[tokio::test]
    async fn test_repair_system_creation() {
        let repair_system = SelfRepairSystem::default();
        assert!(repair_system.config.interactive);
        // assert!(repair_system.config.auto_snapshot);  // REMOVED
    }
}
