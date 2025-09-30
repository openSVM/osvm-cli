//! Process-based runtime for development and testing
//!
//! This runtime runs components as regular OS processes with optional sandboxing.
//! It's useful for:
//! - Development and testing (no special hardware needed)
//! - Trusted components that don't need strong isolation
//! - Debugging (easier to attach debuggers)
//!
//! # Security Note
//!
//! This runtime provides NO hardware isolation. It should only be used for:
//! - Development/testing environments
//! - Fully trusted components
//! - Components that explicitly opt-in to lower isolation

use super::super::{Component, ComponentId, ComponentStatus, IsolationError};
use crate::utils::isolation::Runtime;
use anyhow::{anyhow, Context, Result};
use async_trait::async_trait;
use std::collections::HashMap;
use std::process::Stdio;
use std::sync::Arc;
use tokio::process::{Child, Command};
use tokio::sync::RwLock;

/// Process-based runtime (no isolation)
pub struct ProcessRuntime {
    /// Running components
    components: Arc<RwLock<HashMap<ComponentId, ProcessInstance>>>,
}

struct ProcessInstance {
    component_id: ComponentId,
    process: Child,
    command: Vec<String>,
    status: ComponentStatus,
}

impl ProcessRuntime {
    /// Create a new process runtime
    pub fn new() -> Self {
        Self {
            components: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    async fn start_process(
        &self,
        component_id: ComponentId,
        command: Vec<String>,
    ) -> Result<ProcessInstance> {
        if command.is_empty() {
            return Err(anyhow!("Empty command"));
        }

        log::info!("Starting process for component {}: {:?}", component_id, command);

        let mut cmd = Command::new(&command[0]);
        if command.len() > 1 {
            cmd.args(&command[1..]);
        }

        let process = cmd
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to spawn process")?;

        log::info!("Process started for component {} with PID {:?}", component_id, process.id());

        Ok(ProcessInstance {
            component_id,
            process,
            command,
            status: ComponentStatus::Running,
        })
    }
}

impl Default for ProcessRuntime {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl Runtime for ProcessRuntime {
    fn name(&self) -> &str {
        "Process"
    }

    fn is_available(&self) -> bool {
        // Always available
        true
    }

    async fn start_component(&self, component: &mut Component) -> Result<()> {
        log::info!("Starting component {} as process", component.id);

        // Get command from metadata
        let command_str = component
            .metadata
            .tags
            .get("command")
            .ok_or_else(|| anyhow!("command not found in component metadata"))?;

        let command: Vec<String> = serde_json::from_str(command_str)
            .context("Failed to parse command")?;

        // Start process
        let instance = self.start_process(component.id, command).await?;

        // Update component
        if let Some(pid) = instance.process.id() {
            component.runtime_handle = Some(crate::utils::isolation::component::RuntimeHandle {
                pid: Some(pid),
                socket_path: None,
                data: HashMap::new(),
            });
        }

        // Store instance
        let mut components = self.components.write().await;
        components.insert(component.id, instance);

        component.status = ComponentStatus::Running;
        Ok(())
    }

    async fn stop_component(&self, component_id: ComponentId) -> Result<()> {
        log::info!("Stopping component {}", component_id);

        let mut components = self.components.write().await;
        let mut instance = components
            .remove(&component_id)
            .ok_or_else(|| IsolationError::ComponentNotFound(component_id.to_string()))?;

        instance.process.kill().await.context("Failed to kill process")?;
        instance.process.wait().await.context("Failed to wait for process")?;

        log::info!("Component {} stopped", component_id);
        Ok(())
    }

    async fn restart_component(&self, component_id: ComponentId) -> Result<()> {
        log::info!("Restarting component {}", component_id);

        // Get command before stopping
        let command = {
            let components = self.components.read().await;
            let instance = components
                .get(&component_id)
                .ok_or_else(|| IsolationError::ComponentNotFound(component_id.to_string()))?;
            instance.command.clone()
        };

        // Stop existing
        self.stop_component(component_id).await?;

        // Start new
        let instance = self.start_process(component_id, command).await?;

        // Store new instance
        let mut components = self.components.write().await;
        components.insert(component_id, instance);

        log::info!("Component {} restarted", component_id);
        Ok(())
    }

    async fn get_status(&self, component_id: ComponentId) -> Result<ComponentStatus> {
        let components = self.components.read().await;
        let instance = components
            .get(&component_id)
            .ok_or_else(|| IsolationError::ComponentNotFound(component_id.to_string()))?;

        Ok(instance.status)
    }

    async fn exec(&self, component_id: ComponentId, command: Vec<String>) -> Result<String> {
        // In a real implementation, we'd use nsenter or similar to exec into the process namespace
        // For now, just return an error
        Err(anyhow!("exec not yet implemented for process runtime (component {})", component_id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_runtime_availability() {
        let runtime = ProcessRuntime::new();
        assert!(runtime.is_available());
        assert_eq!(runtime.name(), "Process");
    }

    #[tokio::test]
    async fn test_start_stop_process() {
        let runtime = ProcessRuntime::new();

        let mut component = Component {
            id: ComponentId::new(),
            component_type: super::super::super::ComponentType::Service {
                name: "test".to_string(),
            },
            status: ComponentStatus::Stopped,
            isolation_config: Default::default(),
            runtime_handle: None,
            metadata: {
                let mut meta = super::super::super::ComponentMetadata::default();
                meta.tags.insert(
                    "command".to_string(),
                    serde_json::to_string(&vec!["sleep", "10"]).unwrap(),
                );
                meta
            },
        };

        // Start
        runtime.start_component(&mut component).await.unwrap();
        assert_eq!(component.status, ComponentStatus::Running);
        assert!(component.runtime_handle.is_some());

        // Stop
        runtime.stop_component(component.id).await.unwrap();
    }
}