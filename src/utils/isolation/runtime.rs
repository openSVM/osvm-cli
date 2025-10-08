//! Runtime abstraction for different isolation technologies

use super::{Component, ComponentId, IsolationConfig, IsolationError, IsolationType};
use anyhow::Result;
use async_trait::async_trait;
use std::sync::Arc;

pub mod firecracker;
pub mod hermit;
pub mod process;

pub use firecracker::FirecrackerRuntime;
pub use hermit::HermitRuntime;
pub use process::ProcessRuntime;

/// Runtime abstraction for running components in isolated environments
#[async_trait]
pub trait Runtime: Send + Sync {
    /// Get runtime name
    fn name(&self) -> &str;

    /// Check if runtime is available on this system
    fn is_available(&self) -> bool;

    /// Start a component
    async fn start_component(&self, component: &mut Component) -> Result<()>;

    /// Stop a component
    async fn stop_component(&self, component_id: ComponentId) -> Result<()>;

    /// Restart a component
    async fn restart_component(&self, component_id: ComponentId) -> Result<()>;

    /// Get component status
    async fn get_status(&self, component_id: ComponentId) -> Result<super::ComponentStatus>;

    /// Execute command in component
    async fn exec(&self, component_id: ComponentId, command: Vec<String>) -> Result<String>;
}

/// Runtime manager that selects appropriate runtime based on configuration
pub struct RuntimeManager {
    runtimes: Vec<Arc<dyn Runtime>>,
}

impl RuntimeManager {
    /// Create a new runtime manager
    pub fn new() -> Self {
        Self { runtimes: vec![] }
    }

    /// Create a runtime manager with default runtimes
    pub fn with_defaults() -> Self {
        let mut manager = Self::new();

        // Register process runtime (always available)
        manager.register_runtime(Arc::new(ProcessRuntime::new()));

        // Register Firecracker runtime if available
        let firecracker_config = firecracker::FirecrackerConfig::default();
        if let Ok(firecracker) = FirecrackerRuntime::new(firecracker_config) {
            if firecracker.is_available() {
                manager.register_runtime(Arc::new(firecracker));
            }
        }

        // Register HermitCore runtime if available
        let hermit_config = hermit::HermitConfig::default();
        if let Ok(hermit) = HermitRuntime::new(hermit_config) {
            if hermit.is_available() {
                manager.register_runtime(Arc::new(hermit));
            }
        }

        manager
    }

    /// Register a runtime
    pub fn register_runtime(&mut self, runtime: Arc<dyn Runtime>) {
        self.runtimes.push(runtime);
    }

    /// Get runtime for isolation config
    pub fn get_runtime(&self, config: &IsolationConfig) -> Result<Arc<dyn Runtime>> {
        // Select runtime based on isolation type
        let preferred_runtime = match &config.isolation_type {
            IsolationType::None => "Process",
            IsolationType::ProcessSandbox { .. } => "Process",
            IsolationType::Container { .. } => {
                // Container runtime implementation
                // NOTE: Requires implementing a Docker/Podman runtime
                // See runtime/docker.rs for stub implementation
                log::warn!("Container isolation requested but not fully implemented - using Process runtime");
                "Docker"
            }
            IsolationType::MicroVM { .. } => {
                // MicroVM runtime (Firecracker)
                // NOTE: Firecracker runtime is partially implemented in runtime/firecracker.rs
                // Full production use requires:
                // - Firecracker binary installed
                // - KVM support enabled
                // - Proper networking configuration
                "Firecracker"
            }
            IsolationType::Unikernel { .. } => "HermitCore",
            IsolationType::TEE { .. } => {
                // TEE (Trusted Execution Environment) runtime
                // NOTE: Requires implementing SGX or AMD SEV support
                // This is complex and requires:
                // - Hardware TEE support (Intel SGX or AMD SEV)
                // - Attestation service
                // - Enclave/confidential VM runtime
                log::warn!("TEE isolation requested but not implemented - using Process runtime");
                "SGX"
            }
        };

        // Find runtime by name
        self.runtimes
            .iter()
            .find(|r| r.name() == preferred_runtime && r.is_available())
            .cloned()
            // Fallback to first available runtime
            .or_else(|| self.runtimes.iter().find(|r| r.is_available()).cloned())
            .ok_or_else(|| {
                IsolationError::RuntimeNotAvailable(format!(
                    "No runtime available for isolation type: {:?}",
                    config.isolation_type
                ))
                .into()
            })
    }

    /// List available runtimes
    pub fn list_available_runtimes(&self) -> Vec<String> {
        self.runtimes
            .iter()
            .filter(|r| r.is_available())
            .map(|r| r.name().to_string())
            .collect()
    }

    /// Check if a specific runtime is available
    pub fn is_runtime_available(&self, name: &str) -> bool {
        self.runtimes
            .iter()
            .any(|r| r.name() == name && r.is_available())
    }
}

impl Default for RuntimeManager {
    fn default() -> Self {
        Self::with_defaults()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_manager_creation() {
        let manager = RuntimeManager::new();
        assert_eq!(manager.list_available_runtimes().len(), 0);
    }

    #[test]
    fn test_runtime_manager_with_defaults() {
        let manager = RuntimeManager::with_defaults();
        let available = manager.list_available_runtimes();

        // Process runtime should always be available
        assert!(available.contains(&"Process".to_string()));

        println!("Available runtimes: {:?}", available);
    }

    #[test]
    fn test_process_runtime_always_available() {
        let manager = RuntimeManager::with_defaults();
        assert!(manager.is_runtime_available("Process"));
    }

    #[test]
    fn test_runtime_manager_get_runtime_for_isolation_types() {
        use crate::utils::isolation::config::{IsolationConfig, IsolationType};

        let manager = RuntimeManager::with_defaults();

        // Process isolation should always work
        let process_config = IsolationConfig {
            isolation_type: IsolationType::None,
            ..Default::default()
        };
        let runtime = manager.get_runtime(&process_config);
        assert!(runtime.is_ok());
        assert_eq!(runtime.unwrap().name(), "Process");

        // Process sandbox should use Process runtime
        let sandbox_config = IsolationConfig {
            isolation_type: IsolationType::ProcessSandbox {
                seccomp_profile: None,
                apparmor_profile: None,
            },
            ..Default::default()
        };
        let runtime = manager.get_runtime(&sandbox_config);
        assert!(runtime.is_ok());
    }

    #[test]
    fn test_runtime_manager_multiple_runtimes() {
        let manager = RuntimeManager::with_defaults();
        let available = manager.list_available_runtimes();

        // Should have at least Process runtime
        assert!(!available.is_empty());
        assert!(available.contains(&"Process".to_string()));
    }

    #[test]
    fn test_runtime_selection_fallback() {
        use crate::utils::isolation::config::{HypervisorType, IsolationConfig, IsolationType};

        let manager = RuntimeManager::with_defaults();

        // Request MicroVM but it might not be available
        // Should fallback to Process runtime
        let microvm_config = IsolationConfig {
            isolation_type: IsolationType::MicroVM {
                hypervisor: HypervisorType::Firecracker,
                kernel_path: None,
                rootfs_path: None,
            },
            ..Default::default()
        };

        let runtime = manager.get_runtime(&microvm_config);
        // Should get some runtime (likely Process as fallback)
        assert!(runtime.is_ok());
    }
}
