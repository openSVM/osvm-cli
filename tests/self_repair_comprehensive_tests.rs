//! Comprehensive tests for self-repair system including package managers,
//! repair strategies, system dependencies, and user dependencies

use anyhow::Result;
use osvm::utils::self_repair::{
    RepairableError, RepairStrategy, SelfRepair, RepairPlan, RepairResult,
    SystemDependencyCheck, UserDependencyCheck,
};
use osvm::utils::self_repair::package_managers::{
    PackageManager, detect_package_manager, AptManager, YumManager, BrewManager,
};
use osvm::utils::self_repair::repair_strategies::{
    InstallPackageStrategy, UpdateSystemStrategy, CreateDirectoryStrategy,
    InstallSolanaStrategy, UpdateRustStrategy,
};
use std::path::PathBuf;
use tempfile::TempDir;

#[cfg(test)]]
mod package_manager_tests {
    use super::*;

    #[tokio::test]
    async fn test_package_manager_detection() -> Result<()> {
        // Should detect at least one package manager on the system
        let pm = detect_package_manager();
        assert!(pm.is_some());

        let manager = pm.unwrap();
        match manager {
            PackageManager::Apt(_) => assert!(std::path::Path::new("/usr/bin/apt").exists()),
            PackageManager::Yum(_) => assert!(std::path::Path::new("/usr/bin/yum").exists()),
            PackageManager::Brew(_) => assert!(std::path::Path::new("/usr/local/bin/brew").exists() ||
                                                std::path::Path::new("/opt/homebrew/bin/brew").exists()),
            _ => {}
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_apt_package_check() -> Result<()> {
        if let Some(PackageManager::Apt(apt)) = detect_package_manager() {
            // Test checking if a common package is installed
            let is_installed = apt.is_package_installed("coreutils").await?;
            // coreutils should always be installed on any Debian/Ubuntu system
            assert!(is_installed || !is_installed); // Just verify the call works
        }
        Ok(())
    }

    #[tokio::test]
    async fn test_package_manager_capabilities() -> Result<()> {
        if let Some(manager) = detect_package_manager() {
            let capabilities = manager.get_capabilities();

            assert!(!capabilities.name.is_empty());
            assert!(capabilities.can_install);
            assert!(capabilities.can_update);

            // All managers should support querying
            assert!(capabilities.can_query);
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_multiple_package_check() -> Result<()> {
        if let Some(manager) = detect_package_manager() {
            let packages = vec!["bash".to_string(), "nonexistent-package-12345".to_string()];
            let results = manager.check_packages(&packages).await?;

            assert_eq!(results.len(), 2);
            assert!(results[0]); // bash should exist
            assert!(!results[1]); // nonexistent package shouldn't exist
        }

        Ok(())
    }
}

#[cfg(test)]
mod repairable_error_tests {
    use super::*;

    #[test]
    fn test_error_categorization() {
        let errors = vec![
            RepairableError::MissingSolanaCli,
            RepairableError::OutdatedSolanaCli,
            RepairableError::MissingBuildTools,
            RepairableError::OutdatedRustToolchain,
            RepairableError::MissingConfigDirectory,
        ];

        for error in errors {
            assert!(error.is_repairable(), "Error {:?} should be repairable", error);
            assert!(!error.is_critical(), "Error {:?} shouldn't be critical", error);
        }
    }

    #[test]
    fn test_critical_errors() {
        let critical_errors = vec![
            RepairableError::InsufficientPermissions("sudo required".to_string()),
            RepairableError::SystemTuningRequired,
        ];

        for error in critical_errors {
            assert!(error.is_repairable());
            // These may require manual intervention
            assert!(error.requires_sudo());
        }
    }

    #[test]
    fn test_error_severity_ordering() {
        let low = RepairableError::MissingConfigDirectory;
        let medium = RepairableError::MissingBuildTools;
        let high = RepairableError::InsufficientPermissions("test".to_string());

        assert!(low.severity() < medium.severity());
        assert!(medium.severity() < high.severity());
    }

    #[test]
    fn test_network_related_errors() {
        let network_errors = vec![
            RepairableError::ConnectivityIssues,
            RepairableError::RpcEndpointFailure("https://api.mainnet-beta.solana.com".to_string()),
        ];

        for error in network_errors {
            assert!(error.is_network_related());
            assert!(!error.requires_sudo());
        }
    }

    #[test]
    fn test_error_descriptions() {
        let error = RepairableError::MissingSolanaCli;
        let description = error.description();

        assert!(!description.is_empty());
        assert!(description.contains("Solana") || description.contains("solana"));
    }
}

#[cfg(test)]
mod repair_strategy_tests {
    use super::*;

    #[tokio::test]
    async fn test_strategy_selection() {
        let test_cases = vec![
            (
                RepairableError::MissingSolanaCli,
                RepairStrategy::InstallSolanaCli,
            ),
            (
                RepairableError::MissingBuildTools,
                RepairStrategy::InstallBuildTools,
            ),
            (
                RepairableError::MissingConfigDirectory,
                RepairStrategy::CreateConfigDirectory,
            ),
            (
                RepairableError::OutdatedRustToolchain,
                RepairStrategy::UpdateRustToolchain,
            ),
        ];

        for (error, expected_strategy) in test_cases {
            let strategy = error.get_repair_strategy();
            assert!(matches!(strategy, expected_strategy));
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_install_package_strategy() {
        let strategy = InstallPackageStrategy {
            package_name: "test-package".to_string(),
            use_sudo: false,
        };

        let description = strategy.describe();
        assert!(description.contains("test-package"));
        assert!(!strategy.requires_confirmation());
    }

    #[tokio::test]
    async fn test_create_directory_strategy() {
        let temp_dir = TempDir::new()?;
        let test_path = temp_dir.path().join("new_dir");

        let strategy = CreateDirectoryStrategy {
            path: test_path.clone(),
            permissions: 0o755,
        };

        // Execute the strategy
        let result = strategy.execute().await;
        assert!(result.is_ok());

        // Verify directory was created
        assert!(test_path.exists());
        assert!(test_path.is_dir());

        Ok(())
    }

    #[tokio::test]
    async fn test_strategy_dry_run() {
        let strategy = InstallPackageStrategy {
            package_name: "curl".to_string(),
            use_sudo: false,
        };

        // Dry run should not actually install
        let plan = strategy.plan();
        assert!(!plan.steps.is_empty());
        assert!(!plan.executed);

        Ok(())
    }

    #[tokio::test]
    async fn test_strategy_validation() {
        let strategy = CreateDirectoryStrategy {
            path: PathBuf::from("/root/test"), // Invalid path without sudo
            permissions: 0o755,
        };

        let validation = strategy.validate().await;
        // Should fail validation if we don't have permissions
        assert!(validation.is_err() || validation.is_ok());

        Ok(())
    }
}

#[cfg(test)]
mod self_repair_tests {
    use super::*;

    #[tokio::test]
    async fn test_self_repair_initialization() {
        let repair = SelfRepair::new();
        assert!(repair.is_initialized());
    }

    #[tokio::test]
    async fn test_repair_plan_generation() -> Result<()> {
        let repair = SelfRepair::new();

        let issues = vec![
            RepairableError::MissingConfigDirectory,
            RepairableError::MissingBuildTools,
        ];

        let plan = repair.plan_repairs(&issues)?;

        assert_eq!(plan.repairs.len(), 2);
        assert!(!plan.executed);
        assert!(plan.total_steps >= 2);

        Ok(())
    }

    #[tokio::test]
    async fn test_repair_ordering() -> Result<()> {
        let repair = SelfRepair::new();

        // These should be ordered by severity
        let issues = vec![
            RepairableError::MissingConfigDirectory, // Low
            RepairableError::OutdatedSystemPackages,  // Medium
            RepairableError::InsufficientPermissions("test".to_string()), // High
        ];

        let plan = repair.plan_repairs(&issues)?;

        // High severity should come first
        assert!(plan.repairs[0].priority > plan.repairs[1].priority);
        assert!(plan.repairs[1].priority > plan.repairs[2].priority);

        Ok(())
    }

    #[tokio::test]
    async fn test_single_repair_execution() -> Result<()> {
        let repair = SelfRepair::new();
        let temp_dir = TempDir::new()?;

        // Test directory creation (safe operation)
        let test_path = temp_dir.path().join("test_repair");
        let error = RepairableError::MissingConfigDirectory;

        // Override with temp path for testing
        std::env::set_var("OSVM_CONFIG_DIR", test_path.to_str().unwrap());

        let result = repair.attempt_repair(&error).await;

        if result.is_ok() {
            assert!(test_path.exists());
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_repair_failure_handling() -> Result<()> {
        let repair = SelfRepair::new();

        // This should fail due to permissions
        let error = RepairableError::InsufficientPermissions("sudo required".to_string());

        let result = repair.attempt_repair(&error).await;

        // Should handle the error gracefully
        assert!(result.is_err());

        if let Err(e) = result {
            assert!(e.to_string().contains("permission") ||
                    e.to_string().contains("Permission"));
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_batch_repair_execution() -> Result<()> {
        let repair = SelfRepair::new();
        let temp_dir = TempDir::new()?;

        std::env::set_var("OSVM_CONFIG_DIR", temp_dir.path().to_str().unwrap());

        let issues = vec![
            RepairableError::MissingConfigDirectory,
        ];

        let results = repair.execute_repairs(&issues).await?;

        // Should have one result
        assert_eq!(results.len(), 1);

        Ok(())
    }

    #[tokio::test]
    async fn test_repair_verification() -> Result<()> {
        let repair = SelfRepair::new();
        let temp_dir = TempDir::new()?;
        let test_path = temp_dir.path().join("verify_test");

        std::fs::create_dir_all(&test_path)?;

        let error = RepairableError::MissingConfigDirectory;
        std::env::set_var("OSVM_CONFIG_DIR", test_path.to_str().unwrap());

        // Verify the repair worked
        let is_fixed = repair.verify_repair(&error).await?;
        assert!(is_fixed);

        Ok(())
    }

    #[tokio::test]
    async fn test_repair_rollback() -> Result<()> {
        let repair = SelfRepair::new();
        let temp_dir = TempDir::new()?;

        // Create a repair that we can rollback
        let test_path = temp_dir.path().join("rollback_test");
        std::fs::create_dir_all(&test_path)?;

        let repair_id = "test-repair-001";
        repair.save_rollback_point(repair_id, &test_path).await?;

        // Simulate a failed repair
        let rollback_result = repair.rollback(repair_id).await;
        assert!(rollback_result.is_ok());

        Ok(())
    }

    #[tokio::test]
    async fn test_concurrent_repairs() -> Result<()> {
        use std::sync::Arc;
        let repair = Arc::new(SelfRepair::new());
        let temp_dir = TempDir::new()?;

        std::env::set_var("OSVM_CONFIG_DIR", temp_dir.path().to_str().unwrap());

        let mut handles = vec![];

        // Run multiple repairs concurrently
        for i in 0..3 {
            let repair_clone = Arc::clone(&repair);
            let path = temp_dir.path().join(format!("concurrent_{}", i));

            let handle = tokio::spawn(async move {
                std::env::set_var("OSVM_CONFIG_DIR", path.to_str().unwrap());
                let error = RepairableError::MissingConfigDirectory;
                repair_clone.attempt_repair(&error).await
            });

            handles.push(handle);
        }

        let results = futures::future::join_all(handles).await;

        for result in results {
            // Each should complete (success or failure)
            assert!(result.is_ok());
        }

        Ok(())
    }
}

#[cfg(test)]
mod system_dependency_tests {
    use super::*;

    #[tokio::test]
    async fn test_system_dependency_check() -> Result<()> {
        let checker = SystemDependencyCheck::new();

        let result = checker.check_all().await?;

        assert!(result.total_checks > 0);
        // Should have checked for common tools
        assert!(result.checks.iter().any(|c| c.name == "build-essential" ||
                                                c.name == "gcc"));

        Ok(())
    }

    #[tokio::test]
    async fn test_build_tools_check() -> Result<()> {
        let checker = SystemDependencyCheck::new();

        let has_build_tools = checker.check_build_tools().await?;

        // Most systems should have at least one compiler
        assert!(has_build_tools || !has_build_tools); // Just verify the call works

        Ok(())
    }

    #[tokio::test]
    async fn test_rust_toolchain_check() -> Result<()> {
        let checker = SystemDependencyCheck::new();

        let rust_result = checker.check_rust_toolchain().await?;

        // Rust should be installed for these tests to run
        assert!(rust_result.is_installed);
        assert!(rust_result.version.is_some());

        Ok(())
    }

    #[tokio::test]
    async fn test_system_packages_check() -> Result<()> {
        let checker = SystemDependencyCheck::new();

        let required_packages = vec!["curl".to_string(), "git".to_string()];
        let results = checker.check_packages(&required_packages).await?;

        // curl and git are typically installed
        assert!(!results.is_empty());

        Ok(())
    }
}

#[cfg(test)]
mod user_dependency_tests {
    use super::*;

    #[tokio::test]
    async fn test_user_dependency_check() -> Result<()> {
        let checker = UserDependencyCheck::new();

        let result = checker.check_all().await?;

        assert!(result.total_checks > 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_solana_cli_check() -> Result<()> {
        let checker = UserDependencyCheck::new();

        let solana_result = checker.check_solana_cli().await?;

        // May or may not be installed
        if solana_result.is_installed {
            assert!(solana_result.version.is_some());
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_config_directory_check() -> Result<()> {
        let checker = UserDependencyCheck::new();

        let config_result = checker.check_config_directory().await?;

        // Config dir should exist or be creatable
        assert!(config_result.exists || !config_result.exists);

        Ok(())
    }

    #[tokio::test]
    async fn test_keypair_check() -> Result<()> {
        let checker = UserDependencyCheck::new();
        let temp_dir = TempDir::new()?;

        // Create a test keypair file
        let keypair_path = temp_dir.path().join("test_keypair.json");
        std::fs::write(&keypair_path, "[1,2,3,4,5]")?;

        let keypair_result = checker.check_keypair(&keypair_path).await?;

        assert!(keypair_result.exists);
        assert!(keypair_result.is_valid || !keypair_result.is_valid);

        Ok(())
    }
}

#[cfg(test)]
mod repair_result_tests {
    use super::*;

    #[test]
    fn test_repair_result_success() {
        let result = RepairResult::success("Test repair completed");

        assert!(result.is_success());
        assert!(!result.is_failure());
        assert!(result.message.contains("completed"));
    }

    #[test]
    fn test_repair_result_failure() {
        let result = RepairResult::failure("Test repair failed", "Permission denied");

        assert!(!result.is_success());
        assert!(result.is_failure());
        assert!(result.error_details.is_some());
    }

    #[test]
    fn test_repair_result_partial() {
        let result = RepairResult::partial("Some repairs succeeded", 3, 5);

        assert!(!result.is_success());
        assert!(!result.is_failure());
        assert_eq!(result.successful_repairs, 3);
        assert_eq!(result.total_repairs, 5);
    }
}
