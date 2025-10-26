//! Ephemeral MicroVM launcher for running MCP tools in isolated instances
//!
//! This module provides functionality to launch ephemeral microVMs for each tool call,
//! ensuring complete isolation and automatic cleanup after execution.

use anyhow::{anyhow, Context, Result};
use log::{debug, error, info, warn};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, Command};
use tokio::sync::{Mutex, RwLock};
use tokio::time::{sleep, timeout};
use uuid::Uuid;

/// Default timeout for tool execution in seconds
const DEFAULT_TOOL_TIMEOUT_SECS: u64 = 30;

/// Default memory allocation for ephemeral VMs (MB)
const DEFAULT_EPHEMERAL_MEMORY_MB: usize = 256;

/// Default CPU count for ephemeral VMs
const DEFAULT_EPHEMERAL_CPUS: u8 = 1;

/// Maximum concurrent ephemeral VMs
const MAX_CONCURRENT_VMS: usize = 50;

/// vsock port for tool communication (must match guest/mcp_vsock_wrapper)
const VSOCK_TOOL_PORT: u32 = 5252;

/// Next available vsock CID (starts at 3, reserves 0-2)
static NEXT_VSOCK_CID: AtomicU32 = AtomicU32::new(3);

/// Ephemeral MicroVM configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EphemeralVmConfig {
    /// Unique ID for this ephemeral VM
    pub id: String,
    /// Tool name being executed
    pub tool_name: String,
    /// Server ID this tool belongs to
    pub server_id: String,
    /// Memory allocation in MB
    pub memory_mb: usize,
    /// Number of CPUs
    pub cpus: u8,
    /// Timeout for tool execution
    pub timeout_secs: u64,
    /// Kernel image path
    pub kernel_path: PathBuf,
    /// Rootfs image path
    pub rootfs_path: PathBuf,
    /// Additional environment variables
    pub env_vars: HashMap<String, String>,
    /// vsock CID for this VM
    pub vsock_cid: u32,
    /// Whether to enable debug logging
    pub debug: bool,
}

impl Default for EphemeralVmConfig {
    fn default() -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            tool_name: String::new(),
            server_id: String::new(),
            memory_mb: DEFAULT_EPHEMERAL_MEMORY_MB,
            cpus: DEFAULT_EPHEMERAL_CPUS,
            timeout_secs: DEFAULT_TOOL_TIMEOUT_SECS,
            kernel_path: dirs::home_dir()
                .map(|h| h.join(".osvm/kernel/vmlinux.bin"))
                .unwrap_or_else(|| PathBuf::from("/usr/share/osvm/vmlinux")),
            rootfs_path: dirs::home_dir()
                .map(|h| h.join(".osvm/rootfs/osvm-runtime.ext4"))
                .unwrap_or_else(|| PathBuf::from("/usr/share/osvm/rootfs.ext4")),
            env_vars: HashMap::new(),
            vsock_cid: NEXT_VSOCK_CID.fetch_add(1, Ordering::SeqCst),
            debug: false,
        }
    }
}

/// Handle to a running ephemeral microVM
pub struct EphemeralVmHandle {
    /// VM configuration
    pub config: EphemeralVmConfig,
    /// Firecracker process
    firecracker_process: Child,
    /// Socket path for API
    api_socket: PathBuf,
    /// Temporary directory for VM files
    temp_dir: PathBuf,
    /// Start time for tracking
    start_time: SystemTime,
}

impl EphemeralVmHandle {
    /// Get the VM ID
    pub fn id(&self) -> &str {
        &self.config.id
    }

    /// Get the vsock CID
    pub fn vsock_cid(&self) -> u32 {
        self.config.vsock_cid
    }

    /// Kill the VM and cleanup resources
    pub async fn terminate(mut self) -> Result<()> {
        debug!("Terminating ephemeral VM: {}", self.config.id);

        // Kill the Firecracker process
        if let Err(e) = self.firecracker_process.kill().await {
            warn!("Failed to kill Firecracker process: {}", e);
        }

        // Cleanup temporary directory
        if self.temp_dir.exists() {
            if let Err(e) = tokio::fs::remove_dir_all(&self.temp_dir).await {
                warn!("Failed to cleanup temp directory: {}", e);
            }
        }

        let duration = SystemTime::now()
            .duration_since(self.start_time)
            .unwrap_or_default();
        debug!("Ephemeral VM {} lived for {:?}", self.config.id, duration);

        Ok(())
    }
}

/// Ephemeral MicroVM Manager
pub struct EphemeralVmManager {
    /// Active ephemeral VMs
    active_vms: Arc<RwLock<HashMap<String, Arc<Mutex<EphemeralVmHandle>>>>>,
    /// VM launch semaphore to limit concurrent VMs
    launch_semaphore: Arc<tokio::sync::Semaphore>,
    /// Debug mode flag
    debug_mode: bool,
}

impl EphemeralVmManager {
    /// Create a new ephemeral VM manager
    pub fn new(debug_mode: bool) -> Self {
        Self {
            active_vms: Arc::new(RwLock::new(HashMap::new())),
            launch_semaphore: Arc::new(tokio::sync::Semaphore::new(MAX_CONCURRENT_VMS)),
            debug_mode,
        }
    }

    /// Launch an ephemeral VM for tool execution
    pub async fn launch_tool_vm(
        &self,
        server_id: &str,
        tool_name: &str,
        tool_args: Option<serde_json::Value>,
    ) -> Result<serde_json::Value> {
        // Acquire semaphore permit to limit concurrent VMs
        let _permit = self
            .launch_semaphore
            .acquire()
            .await
            .context("Failed to acquire VM launch permit")?;

        info!(
            "Launching ephemeral VM for tool: {}/{}",
            server_id, tool_name
        );

        // Create configuration for this VM
        let config = EphemeralVmConfig {
            tool_name: tool_name.to_string(),
            server_id: server_id.to_string(),
            debug: self.debug_mode,
            ..Default::default()
        };

        // Launch the VM
        let handle = self.spawn_ephemeral_vm(&config).await?;
        let vm_id = handle.config.id.clone();
        let vsock_cid = handle.vsock_cid();

        // Store handle
        {
            let mut vms = self.active_vms.write().await;
            vms.insert(vm_id.clone(), Arc::new(Mutex::new(handle)));
        }

        // Execute the tool in the VM
        let result = self
            .execute_tool_in_vm(vsock_cid, tool_name, tool_args, config.timeout_secs)
            .await;

        // Cleanup VM regardless of result
        self.cleanup_vm(&vm_id).await?;

        result
    }

    /// Spawn an ephemeral microVM
    async fn spawn_ephemeral_vm(&self, config: &EphemeralVmConfig) -> Result<EphemeralVmHandle> {
        // Create temporary directory for this VM
        let temp_dir = PathBuf::from("/tmp/osvm-ephemeral").join(&config.id);
        tokio::fs::create_dir_all(&temp_dir)
            .await
            .context("Failed to create temp directory")?;

        // Copy rootfs CPIO to temp directory
        let rootfs_dest = temp_dir.join("rootfs.cpio.gz");
        let rootfs_source = config
            .rootfs_path
            .parent()
            .unwrap()
            .join("mcp-server.cpio.gz");

        debug!(
            "Copying rootfs from {:?} to {:?}",
            rootfs_source, rootfs_dest
        );

        let copy_result = Command::new("cp")
            .arg(&rootfs_source)
            .arg(&rootfs_dest)
            .output()
            .await
            .context("Failed to copy rootfs")?;

        if !copy_result.status.success() {
            return Err(anyhow!(
                "Failed to copy rootfs: {}",
                String::from_utf8_lossy(&copy_result.stderr)
            ));
        }

        // Prepare Firecracker configuration
        let api_socket = temp_dir.join("firecracker.sock");
        let vm_config = temp_dir.join("vm_config.json");

        // Create VM configuration file
        let vm_config_json = self.create_vm_config(config, &temp_dir)?;
        tokio::fs::write(&vm_config, serde_json::to_string_pretty(&vm_config_json)?)
            .await
            .context("Failed to write VM config")?;

        // Launch Firecracker
        let mut firecracker_cmd = Command::new("firecracker");
        firecracker_cmd
            .arg("--api-sock")
            .arg(&api_socket)
            .arg("--config-file")
            .arg(&vm_config)
            .stdout(if self.debug_mode {
                std::process::Stdio::inherit()
            } else {
                std::process::Stdio::null()
            })
            .stderr(if self.debug_mode {
                std::process::Stdio::inherit()
            } else {
                std::process::Stdio::null()
            })
            .kill_on_drop(true);

        let firecracker_process = firecracker_cmd
            .spawn()
            .context("Failed to spawn Firecracker process")?;

        // Wait for VM to be ready
        self.wait_for_vm_ready(&api_socket, config.vsock_cid)
            .await?;

        Ok(EphemeralVmHandle {
            config: config.clone(),
            firecracker_process,
            api_socket,
            temp_dir,
            start_time: SystemTime::now(),
        })
    }

    /// Create Firecracker VM configuration
    fn create_vm_config(
        &self,
        config: &EphemeralVmConfig,
        temp_dir: &Path,
    ) -> Result<serde_json::Value> {
        let initrd_path = temp_dir.join("rootfs.cpio.gz");

        Ok(serde_json::json!({
            "boot-source": {
                "kernel_image_path": config.kernel_path.to_string_lossy(),
                "initrd_path": initrd_path.to_string_lossy(),
                "boot_args": format!(
                    "console=ttyS0 reboot=k panic=1 pci=off init=/init"
                ),
            },
            "drives": [],
            "machine-config": {
                "vcpu_count": config.cpus,
                "mem_size_mib": config.memory_mb,
                "smt": false,
            },
            "network-interfaces": [],
            "vsock": {
                "guest_cid": config.vsock_cid,
                "uds_path": format!("/tmp/osvm-vsock-{}.sock", config.id),
            },
            "logger": serde_json::json!(null),
            "metrics": serde_json::json!(null),
        }))
    }

    /// Wait for VM to be ready
    async fn wait_for_vm_ready(&self, api_socket: &Path, vsock_cid: u32) -> Result<()> {
        let start = SystemTime::now();
        let timeout_duration = Duration::from_secs(30);

        while SystemTime::now().duration_since(start)? < timeout_duration {
            // Check if API socket exists
            if api_socket.exists() {
                // Try to connect via vsock
                if let Ok(stream) = timeout(
                    Duration::from_secs(1),
                    tokio_vsock::VsockStream::connect(vsock_cid, VSOCK_TOOL_PORT),
                )
                .await
                {
                    if stream.is_ok() {
                        debug!("VM {} is ready", vsock_cid);
                        return Ok(());
                    }
                }
            }
            sleep(Duration::from_millis(100)).await;
        }

        Err(anyhow!("Timeout waiting for VM to be ready"))
    }

    /// Execute tool in the VM via vsock
    async fn execute_tool_in_vm(
        &self,
        vsock_cid: u32,
        tool_name: &str,
        tool_args: Option<serde_json::Value>,
        timeout_secs: u64,
    ) -> Result<serde_json::Value> {
        debug!("Executing tool {} in VM (CID: {})", tool_name, vsock_cid);

        // Connect to VM via vsock
        let mut stream = timeout(
            Duration::from_secs(15),
            tokio_vsock::VsockStream::connect(vsock_cid, VSOCK_TOOL_PORT),
        )
        .await
        .context("Timeout connecting to ephemeral VM")?
        .context("Failed to connect to ephemeral VM")?;

        // Send tool execution request
        let request = serde_json::json!({
            "tool": tool_name,
            "args": tool_args,
        });

        let request_bytes = serde_json::to_vec(&request)?;
        stream
            .write_all(&request_bytes)
            .await
            .context("Failed to send tool request")?;
        stream.flush().await?;

        // Read response with timeout
        let response = timeout(
            Duration::from_secs(timeout_secs),
            self.read_tool_response(&mut stream),
        )
        .await
        .context("Tool execution timeout")?
        .context("Failed to read tool response")?;

        Ok(response)
    }

    /// Read tool response from vsock stream
    async fn read_tool_response(
        &self,
        stream: &mut tokio_vsock::VsockStream,
    ) -> Result<serde_json::Value> {
        let mut buffer = Vec::new();
        let mut temp_buf = [0u8; 4096];

        loop {
            match stream.read(&mut temp_buf).await {
                Ok(0) => break, // EOF
                Ok(n) => {
                    buffer.extend_from_slice(&temp_buf[..n]);
                    if buffer.len() > 10 * 1024 * 1024 {
                        return Err(anyhow!("Response too large"));
                    }
                }
                Err(e) => return Err(anyhow!("Failed to read response: {}", e)),
            }
        }

        serde_json::from_slice(&buffer).context("Failed to parse tool response")
    }

    /// Cleanup a VM
    async fn cleanup_vm(&self, vm_id: &str) -> Result<()> {
        let handle = {
            let mut vms = self.active_vms.write().await;
            vms.remove(vm_id)
        };

        if let Some(handle) = handle {
            let handle = Arc::try_unwrap(handle)
                .map_err(|_| anyhow!("VM handle still in use"))?
                .into_inner();
            handle.terminate().await?;
            info!("Cleaned up ephemeral VM: {}", vm_id);
        }

        Ok(())
    }

    /// Cleanup all active VMs (for shutdown)
    pub async fn cleanup_all(&self) -> Result<()> {
        let vms = {
            let mut vms = self.active_vms.write().await;
            std::mem::take(&mut *vms)
        };

        for (id, handle) in vms {
            if let Ok(handle) = Arc::try_unwrap(handle) {
                if let Err(e) = handle.into_inner().terminate().await {
                    error!("Failed to cleanup VM {}: {}", id, e);
                }
            }
        }

        Ok(())
    }
}

/// Main chat microVM orchestrator
pub struct ChatVmOrchestrator {
    /// Handle to the persistent chat VM
    chat_vm: Option<Arc<Mutex<EphemeralVmHandle>>>,
    /// Ephemeral VM manager for tools
    ephemeral_manager: Arc<EphemeralVmManager>,
    /// Debug mode flag
    debug_mode: bool,
}

impl ChatVmOrchestrator {
    /// Create a new chat VM orchestrator
    pub fn new(debug_mode: bool) -> Self {
        Self {
            chat_vm: None,
            ephemeral_manager: Arc::new(EphemeralVmManager::new(debug_mode)),
            debug_mode,
        }
    }

    /// Start the main chat VM
    pub async fn start_chat_vm(&mut self) -> Result<()> {
        info!("Starting main chat microVM");

        let config = EphemeralVmConfig {
            id: format!("chat-{}", Uuid::new_v4()),
            tool_name: "main-chat".to_string(),
            server_id: "osvm-chat".to_string(),
            memory_mb: 512,     // More memory for chat VM
            cpus: 2,            // More CPUs for chat VM
            timeout_secs: 3600, // 1 hour timeout for chat
            debug: self.debug_mode,
            ..Default::default()
        };

        let handle = self.ephemeral_manager.spawn_ephemeral_vm(&config).await?;
        self.chat_vm = Some(Arc::new(Mutex::new(handle)));

        Ok(())
    }

    /// Execute a tool in an ephemeral VM
    pub async fn execute_tool(
        &self,
        server_id: &str,
        tool_name: &str,
        tool_args: Option<serde_json::Value>,
    ) -> Result<serde_json::Value> {
        self.ephemeral_manager
            .launch_tool_vm(server_id, tool_name, tool_args)
            .await
    }

    /// Stop the chat VM and cleanup
    pub async fn stop(&mut self) -> Result<()> {
        // Stop chat VM
        if let Some(chat_vm) = self.chat_vm.take() {
            if let Ok(handle) = Arc::try_unwrap(chat_vm) {
                handle.into_inner().terminate().await?;
            }
        }

        // Cleanup all ephemeral VMs
        self.ephemeral_manager.cleanup_all().await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_ephemeral_vm_config() {
        let config = EphemeralVmConfig::default();
        assert_eq!(config.memory_mb, DEFAULT_EPHEMERAL_MEMORY_MB);
        assert_eq!(config.cpus, DEFAULT_EPHEMERAL_CPUS);
        assert_eq!(config.timeout_secs, DEFAULT_TOOL_TIMEOUT_SECS);
    }

    #[tokio::test]
    async fn test_vsock_cid_allocation() {
        let config1 = EphemeralVmConfig::default();
        let config2 = EphemeralVmConfig::default();
        assert_ne!(config1.vsock_cid, config2.vsock_cid);
        assert!(config1.vsock_cid >= 3);
        assert!(config2.vsock_cid >= 3);
    }
}
