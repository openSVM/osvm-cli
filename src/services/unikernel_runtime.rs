//! Unikernel runtime for executing MCP tools in isolated HermitCore instances
//!
//! This module provides the infrastructure for spawning ephemeral unikernels
//! to execute MCP tool calls with strong isolation guarantees.

use anyhow::{anyhow, Context, Result};
use log::{debug, info, warn};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::Duration;
use tokio::time::timeout;

use super::isolation_config::{MountConfig, ToolConfig};
use crate::utils::path_security::{create_secure_socket_dir, generate_socket_name, safe_path_validation};

/// Configuration for spawning a unikernel
#[derive(Debug, Clone)]
pub struct UnikernelConfig {
    /// Path to the unikernel image file
    pub image_path: PathBuf,
    /// Filesystem mounts
    pub mounts: Vec<MountConfig>,
    /// Memory allocation in MB
    pub memory_mb: u32,
    /// Number of vCPUs
    pub vcpus: u32,
    /// Tool name being executed
    pub tool_name: String,
    /// Server ID for logging
    pub server_id: String,
}

/// Handle to a running unikernel instance
pub struct UnikernelHandle {
    /// Process ID of the unikernel
    pid: Option<u32>,
    /// Configuration used to spawn this unikernel
    config: UnikernelConfig,
    /// Communication socket path
    socket_path: PathBuf,
}

impl UnikernelHandle {
    /// Send a tool execution request to the unikernel
    pub async fn execute_tool(
        &self,
        tool_name: &str,
        arguments: Option<Value>,
    ) -> Result<Value> {
        // In a real implementation, this would communicate with the unikernel
        // via virtio-vsock or a Unix socket. For now, we'll simulate the response.
        
        info!(
            "Executing tool '{}' in unikernel (PID: {:?})",
            tool_name, self.pid
        );
        
        // Simulate tool execution with timeout
        let result: Result<Value> = timeout(Duration::from_secs(30), async {
            // TODO: Actual communication with unikernel via virtio-vsock
            // For now, return a placeholder response
            Ok(serde_json::json!({
                "status": "success",
                "tool": tool_name,
                "message": "Unikernel execution simulated (implementation pending)",
                "arguments": arguments
            }))
        })
        .await
        .context("Unikernel execution timed out")?;
        
        result
    }
    
    /// Terminate the unikernel
    pub fn terminate(self) {
        if let Some(pid) = self.pid {
            info!(
                "Terminating unikernel for tool '{}' (PID: {})",
                self.config.tool_name, pid
            );
            
            // Send SIGTERM to the unikernel process
            #[cfg(unix)]
            {
                use nix::sys::signal::{kill, Signal};
                use nix::unistd::Pid;
                
                if let Err(e) = kill(Pid::from_raw(pid as i32), Signal::SIGTERM) {
                    warn!("Failed to terminate unikernel PID {}: {}", pid, e);
                }
            }
        }
        
        // Clean up socket file
        if self.socket_path.exists() {
            if let Err(e) = std::fs::remove_file(&self.socket_path) {
                warn!("Failed to remove socket file: {}", e);
            }
        }
    }
}

/// Unikernel runtime manager
pub struct UnikernelRuntime {
    /// Base directory for unikernel images
    unikernel_dir: PathBuf,
    /// Temporary directory for sockets
    socket_dir: PathBuf,
}

impl UnikernelRuntime {
    /// Create a new unikernel runtime
    pub fn new(unikernel_dir: PathBuf) -> Result<Self> {
        // Use secure socket directory with proper permissions (0700)
        let socket_dir = create_secure_socket_dir()
            .context("Failed to create secure socket directory")?;
        
        Ok(Self {
            unikernel_dir,
            socket_dir,
        })
    }
    
    /// Spawn a unikernel for tool execution
    pub async fn spawn_unikernel(&self, config: UnikernelConfig) -> Result<UnikernelHandle> {
        info!(
            "Spawning unikernel for tool '{}' on server '{}'",
            config.tool_name, config.server_id
        );
        
        // Resolve image path
        let image_path = if config.image_path.is_absolute() {
            config.image_path.clone()
        } else {
            self.unikernel_dir.join(&config.image_path)
        };
        
        if !image_path.exists() {
            return Err(anyhow!(
                "Unikernel image not found: {}",
                image_path.display()
            ));
        }
        
        // Create socket path for communication
        let socket_path = self.socket_dir.join(format!(
            "{}-{}-{}.sock",
            config.server_id,
            config.tool_name,
            std::process::id()
        ));
        
        // Build Firecracker command for unikernel
        let mut cmd = self.build_firecracker_command(&config, &image_path, &socket_path)?;
        
        // Spawn the unikernel process
        debug!("Spawning unikernel: {:?}", cmd);
        let child = cmd
            .spawn()
            .context("Failed to spawn unikernel process")?;
        
        let pid = child.id();
        info!("Unikernel spawned with PID: {}", pid);
        
        // Wait for unikernel to be ready (socket file appears)
        self.wait_for_ready(&socket_path).await?;
        
        Ok(UnikernelHandle {
            pid: Some(pid),
            config,
            socket_path,
        })
    }
    
    /// Build Firecracker command for launching unikernel
    fn build_firecracker_command(
        &self,
        config: &UnikernelConfig,
        image_path: &PathBuf,
        socket_path: &PathBuf,
    ) -> Result<Command> {
        // NOTE: This is a simplified version. Real implementation would use
        // Firecracker API to configure the microVM properly.
        
        let mut cmd = Command::new("firecracker");
        
        // Basic Firecracker configuration
        cmd.arg("--api-sock").arg(socket_path);
        
        // Kernel image (unikernel)
        cmd.arg("--kernel-image-path").arg(image_path);
        
        // Memory configuration
        cmd.arg("--mem-size-mib")
            .arg(config.memory_mb.to_string());
        
        // vCPU configuration
        cmd.arg("--vcpu-count").arg(config.vcpus.to_string());
        
        // Configure 9p mounts for filesystem access with security validation
        for (idx, mount) in config.mounts.iter().enumerate() {
            let mount_tag = format!("mount{}", idx);
            
            // Securely validate and canonicalize host path
            // This prevents path traversal attacks and validates against sensitive directories
            let validated_path = safe_path_validation(
                &mount.host_path,
                true,  // Must be directory
                false, // Don't allow symlinks for security
            ).with_context(|| format!("Invalid mount path: {}", mount.host_path))?;
            
            let canonical_path = validated_path.path().display().to_string();
            
            // Log security warnings for non-readonly mounts
            if !mount.readonly {
                warn!(
                    "Mounting {} as read-write. Ensure this is intentional for security.",
                    canonical_path
                );
            }
            
            cmd.arg("--fs")
                .arg(format!(
                    "{}:{}:{}",
                    mount_tag,
                    canonical_path,
                    if mount.readonly { "ro" } else { "rw" }
                ));
        }
        
        // Redirect output for debugging
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        
        Ok(cmd)
    }
    
    /// Wait for unikernel to be ready
    async fn wait_for_ready(&self, socket_path: &PathBuf) -> Result<()> {
        let start = std::time::Instant::now();
        let timeout_duration = Duration::from_secs(5);
        
        while start.elapsed() < timeout_duration {
            if socket_path.exists() {
                debug!("Unikernel ready (socket exists)");
                return Ok(());
            }
            
            tokio::time::sleep(Duration::from_millis(50)).await;
        }
        
        Err(anyhow!("Unikernel failed to become ready within timeout"))
    }
}

/// Request sent to unikernel
#[derive(Debug, Serialize, Deserialize)]
struct UnikernelRequest {
    tool: String,
    args: Option<Value>,
}

/// Response from unikernel
#[derive(Debug, Serialize, Deserialize)]
struct UnikernelResponse {
    status: String,
    result: Option<Value>,
    error: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_unikernel_config_creation() {
        let config = UnikernelConfig {
            image_path: PathBuf::from("/var/osvm/unikernels/test.img"),
            mounts: vec![MountConfig {
                host_path: "~/.config/test".to_string(),
                vm_path: "/data/test".to_string(),
                readonly: true,
            }],
            memory_mb: 128,
            vcpus: 1,
            tool_name: "test_tool".to_string(),
            server_id: "test_server".to_string(),
        };
        
        assert_eq!(config.memory_mb, 128);
        assert_eq!(config.vcpus, 1);
        assert_eq!(config.mounts.len(), 1);
    }
    
    #[tokio::test]
    async fn test_runtime_creation() {
        let runtime = UnikernelRuntime::new(PathBuf::from("/tmp/test-unikernels"));
        assert!(runtime.is_ok());
    }
}
