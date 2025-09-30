//! Runtime abstraction for different isolation technologies

use super::{Component, ComponentId, IsolationConfig, IsolationType, IsolationError};
use anyhow::Result;
use async_trait::async_trait;
use std::sync::Arc;

pub mod hermit;
pub mod process;
pub mod firecracker;

pub use hermit::HermitRuntime;
pub use process::ProcessRuntime;
pub use firecracker::FirecrackerRuntime;

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
        Self {
            runtimes: vec![],
        }
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
            IsolationType::Container { .. } => "Docker", // TODO: Implement
            IsolationType::MicroVM { .. } => "Firecracker", // TODO: Implement
            IsolationType::Unikernel { .. } => "HermitCore",
            IsolationType::TEE { .. } => "SGX", // TODO: Implement
        };

        // Find runtime by name
        self.runtimes
            .iter()
            .find(|r| r.name() == preferred_runtime && r.is_available())
            .cloned()
            // Fallback to first available runtime
            .or_else(|| {
                self.runtimes
                    .iter()
                    .find(|r| r.is_available())
                    .cloned()
            })
            .ok_or_else(|| IsolationError::RuntimeNotAvailable(format!(
                "No runtime available for isolation type: {:?}",
                config.isolation_type
            )).into())
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
}