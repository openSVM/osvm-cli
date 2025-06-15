//! Repair execution strategies and transaction management
//!
//! This module provides atomic repair operations with rollback capabilities
//! and comprehensive transaction management for system repairs.

use std::collections::HashMap;
use std::error::Error as StdError;
use std::fmt;
use serde::{Deserialize, Serialize};
use tokio::time::Duration;
use chrono::{DateTime, Utc};
use std::process::Command;

use super::{RepairError, RepairResult};
use super::system_deps::{SystemDependencyManager, SystemDepsError};
use super::user_deps::{UserDependencyManager, UserDepsError};
// use super::snapshots::{SnapshotManager, SnapshotError};  // REMOVED

/// Repair operation types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RepairOperation {
    // System-level operations
    UpdateSystemPackages,
    UpdateRustToolchain,
    InstallSystemDependencies(Vec<String>),
    TuneSystemParameters,
    
    // User-level operations
    InstallSolanaCli,
    CreateConfigDirectory,
    GenerateKeypair(String),
    ConfigureNetwork(String),
    
    // Validation operations
    ValidateSystemHealth,
    ValidateUserConfig,
}

/// Checkpoint information for rollback
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Checkpoint {
    pub id: String,
    pub timestamp: DateTime<Utc>,
    pub operation: RepairOperation,
    pub snapshot_id: Option<String>,
    pub state_data: HashMap<String, String>,
}

/// Transaction state
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TransactionState {
    NotStarted,
    InProgress,
    Committed,
    RolledBack,
    Failed,
}

/// Repair transaction with atomic operations and rollback
#[derive(Debug)]
pub struct RepairTransaction {
    pub id: String,
    pub operations: Vec<RepairOperation>,
    pub checkpoints: Vec<Checkpoint>,
    pub state: TransactionState,
    pub start_time: Option<DateTime<Utc>>,
    pub timeout: Duration,
    // snapshot_manager: SnapshotManager,  // REMOVED
    system_manager: SystemDependencyManager,
    user_manager: UserDependencyManager,
}

/// Repair strategy error
#[derive(Debug)]
pub enum RepairStrategyError {
    TransactionFailed(String),
    CheckpointFailed(String),
    RollbackFailed(String),
    ValidationFailed(String),
    TimeoutError(String),
    SystemError(SystemDepsError),
    UserError(UserDepsError),
    // SnapshotError(SnapshotError),  // REMOVED
}

impl fmt::Display for RepairStrategyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RepairStrategyError::TransactionFailed(msg) => write!(f, "Transaction failed: {}", msg),
            RepairStrategyError::CheckpointFailed(msg) => write!(f, "Checkpoint failed: {}", msg),
            RepairStrategyError::RollbackFailed(msg) => write!(f, "Rollback failed: {}", msg),
            RepairStrategyError::ValidationFailed(msg) => write!(f, "Validation failed: {}", msg),
            RepairStrategyError::TimeoutError(msg) => write!(f, "Timeout error: {}", msg),
            RepairStrategyError::SystemError(e) => write!(f, "System error: {}", e),
            RepairStrategyError::UserError(e) => write!(f, "User error: {}", e),
            // RepairStrategyError::SnapshotError(e) => write!(f, "Snapshot error: {}", e),  // REMOVED
        }
    }
}

impl StdError for RepairStrategyError {}

impl From<SystemDepsError> for RepairStrategyError {
    fn from(err: SystemDepsError) -> Self {
        RepairStrategyError::SystemError(err)
    }
}

impl From<UserDepsError> for RepairStrategyError {
    fn from(err: UserDepsError) -> Self {
        RepairStrategyError::UserError(err)
    }
}

// REMOVED: SnapshotError conversion
// impl From<SnapshotError> for RepairStrategyError {
//     fn from(err: SnapshotError) -> Self {
//         RepairStrategyError::SnapshotError(err)
//     }
// }

impl RepairTransaction {
    /// Create a new repair transaction
    pub fn new() -> Result<Self, RepairStrategyError> {
        let id = format!("repair_tx_{}", chrono::Utc::now().timestamp());
        let system_manager = SystemDependencyManager::new()
            .map_err(RepairStrategyError::SystemError)?;
        let user_manager = UserDependencyManager::new();
        // let snapshot_manager = SnapshotManager::new();  // REMOVED

        Ok(Self {
            id,
            operations: Vec::new(),
            checkpoints: Vec::new(),
            state: TransactionState::NotStarted,
            start_time: None,
            timeout: Duration::from_secs(600), // 10 minutes default
            // snapshot_manager,  // REMOVED
            system_manager,
            user_manager,
        })
    }

    /// Add a repair operation to the transaction
    pub fn add_operation(&mut self, operation: RepairOperation) {
        self.operations.push(operation);
    }

    /// Set transaction timeout
    pub fn set_timeout(&mut self, timeout: Duration) {
        self.timeout = timeout;
    }

    /// Execute the transaction with automatic rollback on failure
    pub async fn execute(&mut self) -> Result<RepairResult, RepairError> {
        if self.state != TransactionState::NotStarted {
            return Err(RepairError::ValidationError(
                "Transaction already started".to_string()
            ));
        }

        self.begin_transaction().await?;

        // Create initial snapshot
        // DISABLED: Commenting out snapshot creation as requested
        // let initial_snapshot = self.create_system_snapshot().await?;
        // println!("📸 Created system snapshot: {}", initial_snapshot);

        let mut completed_operations = 0;
        let total_operations = self.operations.len();

        for (index, operation) in self.operations.clone().iter().enumerate() {
            // Check timeout
            if let Some(start_time) = self.start_time {
                let elapsed = Utc::now() - start_time;
                if elapsed.to_std().unwrap_or(Duration::from_secs(0)) > self.timeout {
                    return self.handle_timeout().await;
                }
            }

            // Create checkpoint before operation
            let checkpoint = self.create_checkpoint(operation.clone()).await?;
            println!("🔄 [{}/{}] Executing: {:?}", index + 1, total_operations, operation);

            // Execute the operation
            match self.execute_operation(operation).await {
                Ok(result) => {
                    println!("✅ Operation completed: {}", result);
                    
                    // Validate operation success
                    if let Err(e) = self.validate_operation(operation).await {
                        println!("❌ Operation validation failed: {}", e);
                        return self.rollback_transaction().await;
                    }
                    
                    completed_operations += 1;
                }
                Err(e) => {
                    println!("❌ Operation failed: {}", e);
                    return self.rollback_transaction().await;
                }
            }
        }

        // All operations completed successfully
        self.commit_transaction().await?;
        
        Ok(RepairResult::Success(format!(
            "All {} repair operations completed successfully", 
            completed_operations
        )))
    }

    /// Begin the transaction
    async fn begin_transaction(&mut self) -> Result<(), RepairError> {
        self.state = TransactionState::InProgress;
        self.start_time = Some(Utc::now());
        println!("🚀 Starting repair transaction: {}", self.id);
        Ok(())
    }

    /// Create a checkpoint before an operation
    async fn create_checkpoint(&mut self, operation: RepairOperation) -> Result<String, RepairError> {
        let checkpoint_id = format!("checkpoint_{}_{}", self.checkpoints.len(), chrono::Utc::now().timestamp_millis());
        
        // Create snapshot for critical operations
        // DISABLED: Commenting out snapshot creation as requested
        // let snapshot_id = match operation {
        //     RepairOperation::UpdateSystemPackages |
        //     RepairOperation::UpdateRustToolchain |
        //     RepairOperation::InstallSystemDependencies(_) => {
        //         Some(self.create_system_snapshot().await?)
        //     }
        //     _ => None,
        // };
        let snapshot_id: Option<String> = None;

        let checkpoint = Checkpoint {
            id: checkpoint_id.clone(),
            timestamp: Utc::now(),
            operation: operation.clone(),
            snapshot_id,
            state_data: self.capture_state_data(&operation).await,
        };

        self.checkpoints.push(checkpoint);
        Ok(checkpoint_id)
    }

    /// Execute a single repair operation
    async fn execute_operation(&self, operation: &RepairOperation) -> Result<String, RepairStrategyError> {
        match operation {
            RepairOperation::UpdateSystemPackages => {
                Ok(self.system_manager.update_system_packages().await?)
            }
            RepairOperation::UpdateRustToolchain => {
                Ok(self.system_manager.install_rust_toolchain().await?)
            }
            RepairOperation::InstallSystemDependencies(deps) => {
                Ok(self.system_manager.install_system_dependencies(deps).await?)
            }
            RepairOperation::InstallSolanaCli => {
                Ok(self.user_manager.install_solana_cli().await?)
            }
            RepairOperation::CreateConfigDirectory => {
                Ok(self.user_manager.create_solana_config_dir().await?)
            }
            RepairOperation::GenerateKeypair(path) => {
                let keypair_path = if path.is_empty() { None } else { Some(path.as_str()) };
                Ok(self.user_manager.generate_keypair(keypair_path).await?)
            }
            RepairOperation::ConfigureNetwork(network) => {
                Ok(self.user_manager.configure_solana_network(network).await?)
            }
            RepairOperation::ValidateSystemHealth => {
                Ok(self.validate_system_health().await?)
            }
            RepairOperation::ValidateUserConfig => {
                Ok(self.validate_user_config().await?)
            }
            RepairOperation::TuneSystemParameters => {
                Ok(self.tune_system_parameters().await?)
            }
        }
    }

    /// Validate an operation was successful
    async fn validate_operation(&self, operation: &RepairOperation) -> Result<(), RepairStrategyError> {
        match operation {
            RepairOperation::InstallSolanaCli => {
                if !self.user_manager.is_solana_cli_installed().await? {
                    return Err(RepairStrategyError::ValidationFailed(
                        "Solana CLI installation validation failed".to_string()
                    ));
                }
            }
            RepairOperation::CreateConfigDirectory => {
                if !self.user_manager.get_solana_config_dir().exists() {
                    return Err(RepairStrategyError::ValidationFailed(
                        "Config directory creation validation failed".to_string()
                    ));
                }
            }
            RepairOperation::GenerateKeypair(path) => {
                let keypair_path = if path.is_empty() {
                    self.user_manager.get_solana_config_dir().join("id.json")
                } else {
                    std::path::PathBuf::from(path)
                };
                
                if !keypair_path.exists() {
                    return Err(RepairStrategyError::ValidationFailed(
                        "Keypair generation validation failed".to_string()
                    ));
                }
            }
            _ => {
                // Default validation - just ensure no critical errors
            }
        }
        Ok(())
    }

    /// Capture state data for rollback
    async fn capture_state_data(&self, operation: &RepairOperation) -> HashMap<String, String> {
        let mut state_data = HashMap::new();
        
        match operation {
            RepairOperation::UpdateRustToolchain => {
                if let Ok(rust_info) = self.system_manager.check_rust_toolchain().await {
                    if let Some(version) = rust_info.version {
                        state_data.insert("rust_version".to_string(), version);
                    }
                }
            }
            RepairOperation::ConfigureNetwork(_) => {
                if let Ok(config_info) = self.user_manager.check_all_user_dependencies().await {
                    if let Some(network) = config_info.current_network {
                        state_data.insert("current_network".to_string(), network);
                    }
                }
            }
            _ => {}
        }
        
        state_data
    }

    /// Create a system snapshot
    async fn create_system_snapshot(&self) -> Result<String, RepairError> {
        // DISABLED: Commenting out actual snapshot creation
        // self.snapshot_manager.create_snapshot(None).await
        //     .map_err(|e| RepairError::SnapshotError(e.to_string()))
        
        // Return a dummy snapshot ID
        Ok("snapshot_disabled".to_string())
    }

    /// Validate system health
    async fn validate_system_health(&self) -> Result<String, RepairStrategyError> {
        let dependencies = self.system_manager.check_all_dependencies().await?;
        let issues: Vec<_> = dependencies.iter()
            .filter(|dep| !dep.installed)
            .collect();

        if issues.is_empty() {
            Ok("System health validation passed".to_string())
        } else {
            Err(RepairStrategyError::ValidationFailed(format!(
                "System health issues found: {:?}",
                issues.iter().map(|d| &d.name).collect::<Vec<_>>()
            )))
        }
    }

    /// Validate user configuration
    async fn validate_user_config(&self) -> Result<String, RepairStrategyError> {
        let issues = self.user_manager.validate_solana_config().await?;
        
        if issues.is_empty() {
            Ok("User configuration validation passed".to_string())
        } else {
            Err(RepairStrategyError::ValidationFailed(format!(
                "User configuration issues: {:?}", issues
            )))
        }
    }

    /// Apply system tuning parameters for Solana
    async fn tune_system_parameters(&self) -> Result<String, RepairStrategyError> {
        println!("🔧 Applying system tuning parameters for Solana...");
        
        // Define required kernel parameters
        let kernel_params = vec![
            ("net.core.rmem_max", "134217728"),
            ("net.core.rmem_default", "134217728"),
            ("net.core.wmem_max", "134217728"),
            ("net.core.wmem_default", "134217728"),
            ("vm.max_map_count", "2000000"),
            ("vm.swappiness", "30"),
            ("vm.dirty_ratio", "40"),
            ("vm.dirty_background_ratio", "10"),
        ];
        
        let mut applied_count = 0;
        let mut errors = Vec::new();
        
        for (param, value) in &kernel_params {
            // Apply with sysctl
            let output = Command::new("sudo")
                .arg("sysctl")
                .arg("-w")
                .arg(&format!("{}={}", param, value))
                .output();
                
            match output {
                Ok(result) => {
                    if result.status.success() {
                        println!("✅ Set {} = {}", param, value);
                        applied_count += 1;
                    } else {
                        let error_msg = String::from_utf8_lossy(&result.stderr);
                        errors.push(format!("{}: {}", param, error_msg.trim()));
                    }
                }
                Err(e) => {
                    errors.push(format!("{}: {}", param, e));
                }
            }
        }
        
        // Try to persist settings to sysctl.conf
        if applied_count > 0 {
            let sysctl_conf_content = kernel_params.iter()
                .map(|(param, value)| format!("{}={}", param, value))
                .collect::<Vec<_>>()
                .join("\n");
                
            let persist_cmd = Command::new("sh")
                .arg("-c")
                .arg(&format!(
                    "echo '# OSVM Solana tuning parameters\n{}' | sudo tee -a /etc/sysctl.conf > /dev/null",
                    sysctl_conf_content
                ))
                .output();
                
            if let Ok(result) = persist_cmd {
                if result.status.success() {
                    println!("✅ Persisted settings to /etc/sysctl.conf");
                }
            }
        }
        
        if errors.is_empty() {
            Ok(format!("Applied {} system tuning parameters", applied_count))
        } else {
            Err(RepairStrategyError::ValidationFailed(format!(
                "Applied {} parameters, but {} failed: {:?}",
                applied_count,
                errors.len(),
                errors
            )))
        }
    }

    /// Handle transaction timeout
    async fn handle_timeout(&mut self) -> Result<RepairResult, RepairError> {
        println!("⏰ Transaction timeout reached, initiating rollback...");
        self.rollback_transaction().await
    }

    /// Rollback the transaction to the last known good state
    async fn rollback_transaction(&mut self) -> Result<RepairResult, RepairError> {
        self.state = TransactionState::RolledBack;
        println!("🔙 Rolling back transaction: {}", self.id);

        // SNAPSHOTS REMOVED: Now just try manual rollback
        // // Find the most recent checkpoint with a snapshot
        // if let Some(checkpoint) = self.checkpoints.iter()
        //     .rev()
        //     .find(|cp| cp.snapshot_id.is_some()) {
        //     
        //     if let Some(snapshot_id) = &checkpoint.snapshot_id {
        //         println!("📸 Restoring from snapshot: {}", snapshot_id);
        //         
        //         match self.snapshot_manager.restore_snapshot(snapshot_id).await {
        //             Ok(_) => {
        //                 // Validate rollback success
        //                 match self.validate_rollback_success().await {
        //                     Ok(_) => {
        //                         println!("✅ Rollback completed successfully");
        //                         Ok(RepairResult::Failed(RepairError::RollbackError(
        //                             "Transaction failed but system restored to previous state".to_string()
        //                         )))
        //                     }
        //                     Err(e) => {
        //                         println!("❌ Rollback validation failed: {}", e);
        //                         Ok(RepairResult::RequiresManualIntervention(
        //                             "Automatic rollback failed - manual intervention required".to_string()
        //                         ))
        //                     }
        //                 }
        //             }
        //             Err(e) => {
        //                 println!("❌ Snapshot restore failed: {}", e);
        //                 Ok(RepairResult::RequiresManualIntervention(
        //                     format!("Critical error: rollback failed - {}", e)
        //                 ))
        //             }
        //         }
        //     } else {
        //         // No snapshot available, try manual rollback
        //         self.manual_rollback().await
        //     }
        // } else {
        //     println!("⚠️  No snapshots available for rollback");
        //     Ok(RepairResult::Failed(RepairError::RollbackError(
        //         "No recovery point available".to_string()
        //     )))
        // }
        
        // Always do manual rollback since snapshots are removed
        self.manual_rollback().await
    }

    /// Manual rollback for operations without snapshots
    async fn manual_rollback(&mut self) -> Result<RepairResult, RepairError> {
        println!("🔧 Attempting manual rollback...");
        
        // Reverse the operations that were completed
        for checkpoint in self.checkpoints.iter().rev() {
            match self.reverse_operation(&checkpoint.operation, &checkpoint.state_data).await {
                Ok(msg) => println!("✅ Reversed: {}", msg),
                Err(e) => println!("⚠️  Could not reverse operation: {}", e),
            }
        }

        // Validate the manual rollback
        match self.validate_rollback_success().await {
            Ok(_) => Ok(RepairResult::Partial(
                "Manual rollback completed".to_string(),
                vec![] // TODO: Return remaining issues
            )),
            Err(_) => Ok(RepairResult::RequiresManualIntervention(
                "Manual rollback incomplete - please check system state".to_string()
            ))
        }
    }

    /// Reverse a specific operation
    async fn reverse_operation(
        &self, 
        operation: &RepairOperation, 
        state_data: &HashMap<String, String>
    ) -> Result<String, RepairStrategyError> {
        match operation {
            RepairOperation::ConfigureNetwork(_) => {
                if let Some(previous_network) = state_data.get("current_network") {
                    Ok(self.user_manager.configure_solana_network(previous_network).await?)
                } else {
                    Ok("Could not restore previous network configuration".to_string())
                }
            }
            RepairOperation::UpdateRustToolchain => {
                if let Some(previous_version) = state_data.get("rust_version") {
                    // Note: Downgrading Rust is complex, so we just note it
                    Ok(format!("Note: Rust was updated from version {}", previous_version))
                } else {
                    Ok("Rust toolchain update cannot be easily reversed".to_string())
                }
            }
            _ => {
                Ok(format!("Operation {:?} cannot be automatically reversed", operation))
            }
        }
    }

    /// Validate that rollback was successful
    async fn validate_rollback_success(&self) -> Result<(), RepairStrategyError> {
        // Check that critical systems are still functional
        let _system_deps = self.system_manager.check_all_dependencies().await?;
        let _user_config = self.user_manager.check_all_user_dependencies().await?;
        
        // If we get here without errors, rollback validation passed
        Ok(())
    }

    /// Commit the transaction
    async fn commit_transaction(&mut self) -> Result<(), RepairError> {
        self.state = TransactionState::Committed;
        
        // SNAPSHOTS REMOVED: No longer cleaning up snapshots
        // // Clean up old snapshots (keep the most recent one)
        // let snapshot_ids: Vec<String> = self.checkpoints.iter()
        //     .filter_map(|cp| cp.snapshot_id.as_ref())
        //     .cloned()
        //     .collect();
        // 
        // if snapshot_ids.len() > 1 {
        //     for snapshot_id in &snapshot_ids[..snapshot_ids.len()-1] {
        //         let _ = self.snapshot_manager.cleanup_snapshot(snapshot_id).await;
        //     }
        // }

        println!("✅ Transaction committed successfully: {}", self.id);
        Ok(())
    }
}

impl Default for RepairTransaction {
    fn default() -> Self {
        Self::new().expect("Failed to create default RepairTransaction")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_repair_transaction_creation() {
        let transaction = RepairTransaction::new().expect("Failed to create transaction");
        assert_eq!(transaction.state, TransactionState::NotStarted);
        assert!(transaction.operations.is_empty());
        assert!(transaction.checkpoints.is_empty());
    }

    #[test]
    fn test_repair_operation_serialization() {
        let operation = RepairOperation::InstallSolanaCli;
        let serialized = serde_json::to_string(&operation).expect("Failed to serialize");
        let deserialized: RepairOperation = serde_json::from_str(&serialized).expect("Failed to deserialize");
        
        match (operation, deserialized) {
            (RepairOperation::InstallSolanaCli, RepairOperation::InstallSolanaCli) => (),
            _ => panic!("Serialization roundtrip failed"),
        }
    }

    #[tokio::test]
    async fn test_transaction_add_operations() {
        let mut transaction = RepairTransaction::new().expect("Failed to create transaction");
        
        transaction.add_operation(RepairOperation::InstallSolanaCli);
        transaction.add_operation(RepairOperation::CreateConfigDirectory);
        
        assert_eq!(transaction.operations.len(), 2);
    }

    #[test]
    fn test_checkpoint_creation() {
        let checkpoint = Checkpoint {
            id: "test_checkpoint".to_string(),
            timestamp: Utc::now(),
            operation: RepairOperation::InstallSolanaCli,
            snapshot_id: Some("test_snapshot".to_string()),
            state_data: HashMap::new(),
        };
        
        assert_eq!(checkpoint.id, "test_checkpoint");
        assert!(checkpoint.snapshot_id.is_some());
    }
}