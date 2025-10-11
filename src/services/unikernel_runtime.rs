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

/// Tool execution request sent to unikernel
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ToolExecutionRequest {
    jsonrpc: String,
    id: u64,
    server_id: String,
    method: String,
    params: Option<Value>,
}

/// Tool execution response from unikernel
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ToolExecutionResponse {
    jsonrpc: String,
    id: u64,
    result: Option<Value>,
    error: Option<ToolExecutionError>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ToolExecutionError {
    code: i32,
    message: String,
}

/// Unikernel launcher type
#[derive(Debug, Clone, PartialEq)]
pub enum UnikernelLauncher {
    /// Unikraft with kraft CLI (Phase 2 ephemeral unikernels)
    Unikraft,
    /// Firecracker (Phase 3 microVMs) - NOT USED for ExecutionMode::Unikernel
    #[allow(dead_code)]
    Firecracker,
}

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
    /// Which launcher to use
    pub launcher: UnikernelLauncher,
    /// kraft.yaml configuration path (for Unikraft)
    pub kraft_config: Option<PathBuf>,
    /// vsock CID to allocate (200-299 range for ephemeral)
    pub vsock_cid: Option<u32>,
}

/// Handle to a running unikernel instance
pub struct UnikernelHandle {
    /// Process ID of the unikernel
    pid: Option<u32>,
    /// Configuration used to spawn this unikernel
    config: UnikernelConfig,
    /// Vsock CID for communication
    vsock_cid: u32,
    /// Vsock port for communication (default: 5252)
    vsock_port: u32,
}

impl UnikernelHandle {
    /// Send a tool execution request to the unikernel via vsock
    pub async fn execute_tool(
        &self,
        tool_name: &str,
        arguments: Option<Value>,
        runtime: &UnikernelRuntime,
    ) -> Result<Value> {
        info!(
            "Executing tool '{}' in unikernel (CID: {}, PID: {:?})",
            tool_name, self.vsock_cid, self.pid
        );

        // Connect to unikernel via vsock
        let mut stream = runtime
            .connect_vsock(self.vsock_cid, self.vsock_port)
            .await?;

        // Build request
        let request = ToolExecutionRequest {
            jsonrpc: "2.0".to_string(),
            id: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_millis() as u64,
            server_id: self.config.server_id.clone(),
            method: tool_name.to_string(),
            params: arguments,
        };

        // Send request and get response
        let response = runtime
            .send_tool_request_vsock(&mut stream, request)
            .await?;

        // Extract result or error
        if let Some(result) = response.result {
            info!("Tool '{}' executed successfully", tool_name);
            Ok(result)
        } else if let Some(error) = response.error {
            Err(anyhow!(
                "Tool execution failed: {} (code: {})",
                error.message,
                error.code
            ))
        } else {
            Err(anyhow!("Invalid response: no result or error"))
        }
    }

    /// Terminate the unikernel
    pub fn terminate(self) {
        if let Some(pid) = self.pid {
            info!(
                "Terminating unikernel for tool '{}' (PID: {}, CID: {})",
                self.config.tool_name, pid, self.vsock_cid
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

        // Note: vsock connections are automatically cleaned up when process terminates
        // No socket file to remove
    }
}

/// Unikernel runtime manager
pub struct UnikernelRuntime {
    /// Base directory for unikernel images
    unikernel_dir: PathBuf,
    /// Path to kraft binary (default: "kraft" or with flatpak prefix)
    kraft_binary: String,
}

impl UnikernelRuntime {
    /// Create a new unikernel runtime
    pub fn new(unikernel_dir: PathBuf) -> Result<Self> {
        // Detect if running in Flatpak environment
        let kraft_binary = if std::path::Path::new("/.flatpak-info").exists() {
            info!("Detected Flatpak environment, using flatpak-spawn for kraft");
            "flatpak-spawn --host -- kraft".to_string()
        } else {
            "kraft".to_string()
        };

        Ok(Self {
            unikernel_dir,
            kraft_binary,
        })
    }

    /// Allocate an ephemeral vsock CID in the 200-299 range
    fn allocate_ephemeral_cid(&self) -> Result<u32> {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        std::process::id().hash(&mut hasher);
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
            .hash(&mut hasher);

        let hash = hasher.finish();
        let cid = 200 + (hash % 100) as u32; // 200-299 range

        debug!("Allocated ephemeral vsock CID: {}", cid);
        Ok(cid)
    }

    /// Build kraft command for launching unikernel
    fn build_kraft_command(&self, config: &UnikernelConfig, cid: u32) -> Result<Command> {
        let mut cmd = Command::new("sh");
        cmd.arg("-c");

        // Build kraft run command
        let kraft_cmd = format!(
            "{} run --rm --memory {}M {}",
            self.kraft_binary,
            config.memory_mb,
            config.image_path.display()
        );

        cmd.arg(&kraft_cmd);

        // Pass configuration via environment variables
        cmd.env("UNIKRAFT_VSOCK_CID", cid.to_string());
        cmd.env("UNIKRAFT_TOOL_SERVER", &config.server_id);

        // Redirect output for debugging
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());

        debug!("kraft command: {}", kraft_cmd);

        Ok(cmd)
    }

    /// Connect to unikernel via vsock
    async fn connect_vsock(&self, cid: u32, port: u32) -> Result<tokio_vsock::VsockStream> {
        use tokio_vsock::VsockStream;

        debug!("Connecting to vsock CID {} port {}", cid, port);

        let stream = timeout(Duration::from_secs(10), VsockStream::connect(cid, port))
            .await
            .context("Timeout connecting to unikernel vsock")?
            .context("Failed to connect to unikernel vsock")?;

        debug!("Successfully connected to unikernel vsock");
        Ok(stream)
    }

    /// Send a tool execution request via vsock and receive response
    async fn send_tool_request_vsock(
        &self,
        stream: &mut tokio_vsock::VsockStream,
        request: ToolExecutionRequest,
    ) -> Result<ToolExecutionResponse> {
        use tokio::io::{AsyncReadExt, AsyncWriteExt};

        debug!("Sending tool request {} via vsock", request.id);

        // Serialize request
        let request_bytes = serde_json::to_vec(&request).context("Failed to serialize request")?;
        let len = request_bytes.len() as u32;

        // Send length prefix (4-byte little-endian)
        stream
            .write_all(&len.to_le_bytes())
            .await
            .context("Failed to write length prefix")?;

        // Send request payload
        stream
            .write_all(&request_bytes)
            .await
            .context("Failed to write request")?;

        stream
            .flush()
            .await
            .context("Failed to flush vsock stream")?;

        debug!("Request sent, waiting for response...");

        // Read response length prefix
        let mut len_bytes = [0u8; 4];
        stream
            .read_exact(&mut len_bytes)
            .await
            .context("Failed to read response length")?;
        let response_len = u32::from_le_bytes(len_bytes) as usize;

        // Validate response size
        if response_len > 10 * 1024 * 1024 {
            return Err(anyhow!("Response too large: {} bytes", response_len));
        }

        // Read response payload
        let mut response_bytes = vec![0u8; response_len];
        stream
            .read_exact(&mut response_bytes)
            .await
            .context("Failed to read response payload")?;

        // Deserialize response
        let response: ToolExecutionResponse =
            serde_json::from_slice(&response_bytes).context("Failed to deserialize response")?;

        debug!("Received response {}", response.id);
        Ok(response)
    }

    /// Spawn a unikernel for tool execution
    pub async fn spawn_unikernel(&self, mut config: UnikernelConfig) -> Result<UnikernelHandle> {
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

        // Allocate vsock CID if not provided
        let vsock_cid = match config.vsock_cid {
            Some(cid) => cid,
            None => {
                let cid = self.allocate_ephemeral_cid()?;
                config.vsock_cid = Some(cid);
                cid
            }
        };

        // Build kraft command
        let mut cmd = self.build_kraft_command(&config, vsock_cid)?;

        // Spawn the unikernel process
        debug!("Spawning unikernel with kraft: {:?}", cmd);
        let child = cmd.spawn().context("Failed to spawn unikernel process")?;

        let pid = child.id();
        info!("Unikernel spawned with PID: {}, CID: {}", pid, vsock_cid);

        // Wait for vsock to be ready (brief delay for unikernel boot)
        tokio::time::sleep(Duration::from_millis(500)).await;

        Ok(UnikernelHandle {
            pid: Some(pid),
            config,
            vsock_cid,
            vsock_port: 5252,
        })
    }
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
            launcher: UnikernelLauncher::Unikraft,
            kraft_config: None,
            vsock_cid: None,
        };

        assert_eq!(config.memory_mb, 128);
        assert_eq!(config.vcpus, 1);
        assert_eq!(config.mounts.len(), 1);
        assert_eq!(config.launcher, UnikernelLauncher::Unikraft);
    }

    #[tokio::test]
    async fn test_runtime_creation() {
        let runtime = UnikernelRuntime::new(PathBuf::from("/tmp/test-unikernels"));
        assert!(runtime.is_ok());

        let runtime = runtime.unwrap();
        assert_eq!(runtime.unikernel_dir, PathBuf::from("/tmp/test-unikernels"));

        // kraft_binary should be set based on environment
        assert!(!runtime.kraft_binary.is_empty());
    }

    #[test]
    fn test_cid_allocation_range() {
        let runtime = UnikernelRuntime::new(PathBuf::from("/tmp/test-unikernels")).unwrap();

        // Test multiple allocations to verify range
        for _ in 0..10 {
            let cid = runtime.allocate_ephemeral_cid().unwrap();
            assert!(
                (200..300).contains(&cid),
                "CID {} not in 200-299 range",
                cid
            );
        }
    }
}
