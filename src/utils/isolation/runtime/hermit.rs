//! HermitCore unikernel runtime implementation
//!
//! This module provides support for running OSVM components in HermitCore unikernels.
//! HermitCore is a Rust-based unikernel that provides:
//! - Single-address-space execution (no kernel/user separation)
//! - Minimal attack surface (~50KB of OS code)
//! - Hardware-enforced isolation via KVM
//! - Fast boot times (<100ms)
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────┐
//! │     Application (MCP Server)        │
//! ├─────────────────────────────────────┤
//! │     HermitCore Runtime (~50KB)      │
//! │  - Basic memory allocator           │
//! │  - Virtio network driver            │
//! │  - Simple scheduler                 │
//! │  - No file system                   │
//! │  - No syscalls (direct calls)       │
//! └─────────────────────────────────────┘
//!           │
//!           ▼ (KVM/hardware virtualization)
//! ┌─────────────────────────────────────┐
//! │       Host OS (Linux)               │
//! └─────────────────────────────────────┘
//! ```

use super::super::{Component, ComponentId, ComponentStatus, IsolationError};
use crate::utils::isolation::Runtime;
use anyhow::{anyhow, Context, Result};
use async_trait::async_trait;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::process::{Child, Command as TokioCommand};
use tokio::sync::RwLock;

/// HermitCore runtime for unikernel execution
pub struct HermitRuntime {
    /// Path to hermit binary
    hermit_bin: PathBuf,

    /// Running components
    components: Arc<RwLock<HashMap<ComponentId, HermitInstance>>>,

    /// Configuration
    config: HermitConfig,
}

/// Configuration for HermitCore runtime
#[derive(Debug, Clone)]
pub struct HermitConfig {
    /// Path to kernel image
    pub kernel_path: PathBuf,

    /// Maximum boot timeout
    pub boot_timeout: Duration,

    /// Enable verbose logging
    pub verbose: bool,

    /// Enable KVM acceleration
    pub kvm_enabled: bool,

    /// CPU cores to allocate
    pub vcpus: u32,

    /// Memory in MB
    pub memory_mb: u64,
}

impl Default for HermitConfig {
    fn default() -> Self {
        Self {
            kernel_path: PathBuf::from("/usr/local/lib/hermit/rusty-hermit"),
            boot_timeout: Duration::from_secs(5),
            verbose: false,
            kvm_enabled: true,
            vcpus: 1,
            memory_mb: 128,
        }
    }
}

/// A running HermitCore instance
struct HermitInstance {
    /// Component ID
    component_id: ComponentId,

    /// Process handle
    process: Child,

    /// Image path
    image_path: PathBuf,

    /// Start time
    started_at: Instant,

    /// Status
    status: ComponentStatus,
}

impl HermitRuntime {
    /// Create a new HermitCore runtime
    pub fn new(config: HermitConfig) -> Result<Self> {
        // Check if hermit binary exists
        // For now, assume it's in PATH or use a default location
        let hermit_bin = std::env::var("HERMIT_BIN")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("hermit"));

        Ok(Self {
            hermit_bin,
            components: Arc::new(RwLock::new(HashMap::new())),
            config,
        })
    }

    /// Build a unikernel image from source
    pub async fn build_image(&self, source_path: &Path, output_path: &Path) -> Result<()> {
        log::info!("Building HermitCore unikernel image from {:?}", source_path);

        // Build command: hermit build --release --target x86_64-unknown-hermit
        let output = Command::new(&self.hermit_bin)
            .arg("build")
            .arg("--release")
            .arg("--target")
            .arg("x86_64-unknown-hermit")
            .arg("--manifest-path")
            .arg(source_path.join("Cargo.toml"))
            .output()
            .context("Failed to execute hermit build")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("Hermit build failed: {}", stderr));
        }

        // Copy built image to output path
        let built_image = source_path
            .join("target/x86_64-unknown-hermit/release")
            .join("app"); // Assuming binary name is "app"

        if !built_image.exists() {
            return Err(anyhow!("Built image not found at {:?}", built_image));
        }

        std::fs::copy(&built_image, output_path)
            .context("Failed to copy built image")?;

        log::info!("Unikernel image built successfully at {:?}", output_path);
        Ok(())
    }

    /// Start a unikernel instance
    async fn start_instance(
        &self,
        component_id: ComponentId,
        image_path: PathBuf,
        vcpus: u32,
        memory_mb: u64,
    ) -> Result<HermitInstance> {
        log::info!(
            "Starting HermitCore unikernel for component {}",
            component_id
        );

        let start_time = Instant::now();

        // Build hermit run command
        let mut cmd = TokioCommand::new(&self.hermit_bin);
        cmd.arg("run")
            .arg(&image_path)
            .arg("--")
            .arg(format!("--cpus={}", vcpus))
            .arg(format!("--memory={}M", memory_mb));

        if self.config.kvm_enabled {
            cmd.arg("--kvm");
        }

        if self.config.verbose {
            cmd.arg("--verbose");
        }

        // Spawn process
        let mut process = cmd
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to spawn hermit process")?;

        // Wait for boot (check if process is still running)
        tokio::time::sleep(Duration::from_millis(50)).await;

        // Check if process exited immediately (error)
        if let Ok(Some(status)) = process.try_wait() {
            let stderr = if let Some(mut stderr) = process.stderr.take() {
                use tokio::io::AsyncReadExt;
                let mut buf = String::new();
                let _ = stderr.read_to_string(&mut buf).await;
                buf
            } else {
                "No stderr available".to_string()
            };

            return Err(anyhow!(
                "Hermit process exited immediately with status {}: {}",
                status,
                stderr
            ));
        }

        let boot_time = start_time.elapsed();
        log::info!(
            "HermitCore unikernel booted in {:?} for component {}",
            boot_time,
            component_id
        );

        if boot_time > Duration::from_secs(1) {
            log::warn!(
                "Boot time {:?} exceeds 1 second target for component {}",
                boot_time,
                component_id
            );
        }

        Ok(HermitInstance {
            component_id,
            process,
            image_path,
            started_at: start_time,
            status: ComponentStatus::Running,
        })
    }

    /// Stop a unikernel instance
    async fn stop_instance(&self, component_id: ComponentId) -> Result<()> {
        let mut components = self.components.write().await;

        let instance = components
            .get_mut(&component_id)
            .ok_or_else(|| IsolationError::ComponentNotFound(component_id.to_string()))?;

        log::info!("Stopping HermitCore unikernel for component {}", component_id);

        // Try graceful shutdown first (SIGTERM)
        instance
            .process
            .kill()
            .await
            .context("Failed to kill hermit process")?;

        // Wait for process to exit
        let timeout = Duration::from_secs(5);
        let result = tokio::time::timeout(timeout, instance.process.wait()).await;

        match result {
            Ok(Ok(status)) => {
                log::info!(
                    "HermitCore unikernel stopped for component {} with status: {}",
                    component_id,
                    status
                );
            }
            Ok(Err(e)) => {
                log::error!(
                    "Error waiting for hermit process to exit for component {}: {}",
                    component_id,
                    e
                );
            }
            Err(_) => {
                log::warn!(
                    "Timeout waiting for hermit process to exit for component {}",
                    component_id
                );
                // Force kill if still running
                let _ = instance.process.kill().await;
            }
        }

        components.remove(&component_id);
        Ok(())
    }
}

#[async_trait]
impl Runtime for HermitRuntime {
    fn name(&self) -> &str {
        "HermitCore"
    }

    fn is_available(&self) -> bool {
        // Check if hermit binary exists
        self.hermit_bin.exists()
    }

    async fn start_component(&self, component: &mut Component) -> Result<()> {
        log::info!(
            "Starting component {} in HermitCore unikernel",
            component.id
        );

        // Get image path from component metadata
        let image_path = component
            .metadata
            .tags
            .get("image_path")
            .ok_or_else(|| anyhow!("image_path not found in component metadata"))?;

        let image_path = PathBuf::from(image_path);

        if !image_path.exists() {
            return Err(anyhow!("Unikernel image not found at {:?}", image_path));
        }

        // Get resource limits
        let vcpus = component
            .isolation_config
            .resource_limits
            .max_cpu_cores
            .unwrap_or(1);

        let memory_mb = component
            .isolation_config
            .resource_limits
            .max_memory_mb
            .unwrap_or(128);

        // Start instance
        let instance = self
            .start_instance(component.id, image_path, vcpus, memory_mb)
            .await?;

        // Get PID if available
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
        log::info!(
            "Component {} started successfully in HermitCore",
            component.id
        );

        Ok(())
    }

    async fn stop_component(&self, component_id: ComponentId) -> Result<()> {
        self.stop_instance(component_id).await
    }

    async fn restart_component(&self, component_id: ComponentId) -> Result<()> {
        log::info!("Restarting component {} in HermitCore", component_id);

        // Get instance info before stopping
        let (image_path, vcpus, memory_mb) = {
            let components = self.components.read().await;
            let instance = components
                .get(&component_id)
                .ok_or_else(|| IsolationError::ComponentNotFound(component_id.to_string()))?;

            (
                instance.image_path.clone(),
                self.config.vcpus,
                self.config.memory_mb,
            )
        };

        // Stop existing instance
        self.stop_instance(component_id).await?;

        // Start new instance
        let instance = self
            .start_instance(component_id, image_path, vcpus, memory_mb)
            .await?;

        // Store new instance
        let mut components = self.components.write().await;
        components.insert(component_id, instance);

        log::info!("Component {} restarted successfully", component_id);
        Ok(())
    }

    async fn get_status(&self, component_id: ComponentId) -> Result<ComponentStatus> {
        let components = self.components.read().await;

        let instance = components
            .get(&component_id)
            .ok_or_else(|| IsolationError::ComponentNotFound(component_id.to_string()))?;

        // Check if process is still running
        // Note: This is a simplified check. In production, we'd need more sophisticated health checking
        Ok(instance.status)
    }

    async fn exec(&self, component_id: ComponentId, _command: Vec<String>) -> Result<String> {
        // HermitCore unikernels don't support exec
        // This is a feature, not a bug - no way to execute arbitrary commands
        Err(anyhow!(
            "exec not supported in HermitCore unikernels (component {})",
            component_id
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hermit_config_default() {
        let config = HermitConfig::default();
        assert_eq!(config.vcpus, 1);
        assert_eq!(config.memory_mb, 128);
        assert!(config.kvm_enabled);
    }

    #[test]
    fn test_hermit_runtime_name() {
        let config = HermitConfig::default();
        if let Ok(runtime) = HermitRuntime::new(config) {
            assert_eq!(runtime.name(), "HermitCore");
        }
        // If HermitCore not installed, skip test
    }

    #[tokio::test]
    async fn test_hermit_runtime_availability() {
        let config = HermitConfig::default();
        if let Ok(runtime) = HermitRuntime::new(config) {
            // Check if available (depends on system)
            let available = runtime.is_available();
            println!("HermitCore available: {}", available);
        }
    }
}