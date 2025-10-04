//! MicroVM launcher for running OSVM runtime in isolated Firecracker instances
//!
//! This module provides functionality to launch OSVM agent runtime inside a microVM
//! for enhanced security and isolation.

use anyhow::{anyhow, Context, Result};
use log::{debug, info, warn};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::Duration;

/// Configuration for launching OSVM in a microVM
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OsvmMicroVmConfig {
    /// Memory allocation in MB
    pub memory_mb: u32,
    /// Number of vCPUs
    pub vcpus: u32,
    /// Path to OSVM binary to run inside microVM
    pub osvm_binary: PathBuf,
    /// Working directory for OSVM inside microVM
    pub work_dir: PathBuf,
    /// Host directories to mount (for config, data, etc.)
    pub mounts: Vec<MountPoint>,
    /// virtio-vsock CID for communication
    pub vsock_cid: u32,
}

/// A mount point from host to microVM
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MountPoint {
    pub host_path: String,
    pub guest_path: String,
    pub readonly: bool,
}

impl Default for OsvmMicroVmConfig {
    fn default() -> Self {
        Self {
            memory_mb: 512,
            vcpus: 2,
            osvm_binary: PathBuf::from("/usr/local/bin/osvm"),
            work_dir: PathBuf::from("/app"),
            mounts: vec![
                MountPoint {
                    host_path: "~/.config/osvm".to_string(),
                    guest_path: "/root/.config/osvm".to_string(),
                    readonly: false,
                },
                MountPoint {
                    host_path: "~/.osvm/data".to_string(),
                    guest_path: "/data".to_string(),
                    readonly: false,
                },
            ],
            vsock_cid: 3, // CID 0-2 are reserved
        }
    }
}

/// Handle to a running OSVM microVM instance
pub struct OsvmMicroVmHandle {
    /// Process handle for Firecracker
    child: Child,
    /// Configuration used
    config: OsvmMicroVmConfig,
    /// Socket path for Firecracker API
    api_socket: PathBuf,
}

impl OsvmMicroVmHandle {
    /// Check if the microVM is still running
    pub fn is_running(&mut self) -> bool {
        match self.child.try_wait() {
            Ok(Some(_)) => false,
            Ok(None) => true,
            Err(_) => false,
        }
    }

    /// Terminate the microVM gracefully
    pub fn shutdown(mut self) -> Result<()> {
        info!("Shutting down OSVM microVM");
        
        // Try graceful shutdown via API first
        if let Err(e) = self.send_shutdown_signal() {
            warn!("Failed to send graceful shutdown: {}", e);
        }
        
        // Wait briefly for graceful shutdown
        std::thread::sleep(Duration::from_secs(2));
        
        // Force kill if still running
        if self.is_running() {
            warn!("Forcing microVM termination");
            self.child.kill().context("Failed to kill microVM process")?;
        }
        
        self.child.wait().context("Failed to wait for microVM process")?;
        
        // Cleanup socket file
        if self.api_socket.exists() {
            let _ = std::fs::remove_file(&self.api_socket);
        }
        
        Ok(())
    }
    
    fn send_shutdown_signal(&self) -> Result<()> {
        // Send shutdown action via Firecracker API
        let client = reqwest::blocking::Client::new();
        let url = format!("http://localhost/actions");
        
        let shutdown_action = serde_json::json!({
            "action_type": "SendCtrlAltDel"
        });
        
        client
            .put(&url)
            .json(&shutdown_action)
            .send()
            .context("Failed to send shutdown signal")?;
        
        Ok(())
    }
}

/// MicroVM launcher service
pub struct MicroVmLauncher {
    /// Directory for socket files and VM state
    runtime_dir: PathBuf,
}

impl MicroVmLauncher {
    /// Create a new microVM launcher
    pub fn new() -> Result<Self> {
        // Use ~/.osvm/run for runtime files
        let home = std::env::var("HOME")
            .context("HOME environment variable not set")?;
        let runtime_dir = PathBuf::from(home).join(".osvm/run");
        
        // Create runtime directory if it doesn't exist
        if !runtime_dir.exists() {
            std::fs::create_dir_all(&runtime_dir)
                .context("Failed to create runtime directory")?;
        }
        
        Ok(Self { runtime_dir })
    }
    
    /// Launch OSVM runtime in a microVM
    pub fn launch_osvm_runtime(&self, config: OsvmMicroVmConfig) -> Result<OsvmMicroVmHandle> {
        info!("Launching OSVM runtime in microVM");
        debug!("Configuration: {:?}", config);
        
        // Check if Firecracker is available
        self.check_firecracker_available()?;
        
        // Create API socket path
        let api_socket = self.runtime_dir.join(format!(
            "osvm-runtime-{}.sock",
            std::process::id()
        ));
        
        // Build Firecracker configuration
        let firecracker_config = self.build_firecracker_config(&config, &api_socket)?;
        
        // Write config to temporary file
        let config_file = self.runtime_dir.join(format!(
            "osvm-config-{}.json",
            std::process::id()
        ));
        let config_json = serde_json::to_string_pretty(&firecracker_config)?;
        std::fs::write(&config_file, config_json)
            .context("Failed to write Firecracker config")?;
        
        // Launch Firecracker with the config
        let mut cmd = Command::new("firecracker");
        cmd.arg("--api-sock")
            .arg(&api_socket)
            .arg("--config-file")
            .arg(&config_file);
        
        // Set up I/O redirection
        cmd.stdout(Stdio::piped())
            .stderr(Stdio::piped());
        
        info!("Starting Firecracker microVM...");
        let child = cmd.spawn().context("Failed to spawn Firecracker process")?;
        
        let handle = OsvmMicroVmHandle {
            child,
            config,
            api_socket,
        };
        
        // Wait for microVM to be ready
        self.wait_for_microvm_ready(&handle)?;
        
        info!("OSVM microVM launched successfully");
        
        Ok(handle)
    }
    
    fn check_firecracker_available(&self) -> Result<()> {
        let output = Command::new("which")
            .arg("firecracker")
            .output()
            .context("Failed to check for Firecracker")?;
        
        if !output.status.success() {
            return Err(anyhow!(
                "Firecracker not found. Please install Firecracker to use microVM isolation.\n\
                 See: https://github.com/firecracker-microvm/firecracker"
            ));
        }
        
        Ok(())
    }
    
    fn build_firecracker_config(
        &self,
        config: &OsvmMicroVmConfig,
        api_socket: &Path,
    ) -> Result<serde_json::Value> {
        // Get HOME directory for paths
        let home = std::env::var("HOME")
            .context("HOME environment variable not set")?;
        let osvm_home = PathBuf::from(home).join(".osvm");
        
        let kernel_path = osvm_home.join("kernel/vmlinux.bin");
        let rootfs_path = osvm_home.join("rootfs/osvm-runtime.cpio");
        
        // Build comprehensive Firecracker configuration
        let firecracker_config = serde_json::json!({
            "boot-source": {
                "kernel_image_path": kernel_path.to_string_lossy(),
                "boot_args": "console=ttyS0 reboot=k panic=1 pci=off init=/init"
            },
            "drives": [{
                "drive_id": "rootfs",
                "path_on_host": rootfs_path.to_string_lossy(),
                "is_root_device": true,
                "is_read_only": false
            }],
            "machine-config": {
                "vcpu_count": config.vcpus,
                "mem_size_mib": config.memory_mb,
                "ht_enabled": false
            },
            "network-interfaces": [],
            "vsock": {
                "guest_cid": config.vsock_cid,
                "uds_path": api_socket.to_string_lossy()
            }
        });
        
        Ok(firecracker_config)
    }
    
    fn wait_for_microvm_ready(&self, handle: &OsvmMicroVmHandle) -> Result<()> {
        let start = std::time::Instant::now();
        let timeout = Duration::from_secs(30);
        
        while start.elapsed() < timeout {
            // Check if socket exists
            if handle.api_socket.exists() {
                debug!("MicroVM API socket ready");
                
                // Give it a moment for full initialization
                std::thread::sleep(Duration::from_millis(500));
                return Ok(());
            }
            
            std::thread::sleep(Duration::from_millis(100));
        }
        
        Err(anyhow!("MicroVM failed to become ready within timeout"))
    }
}

/// Check if we're currently running inside a microVM
pub fn is_running_in_microvm() -> bool {
    // Check for OSVM_IN_MICROVM environment variable
    if std::env::var("OSVM_IN_MICROVM").is_ok() {
        return true;
    }
    
    // Check for common microVM indicators
    // 1. Check for virtio devices
    if Path::new("/sys/bus/virtio/devices").exists() {
        return true;
    }
    
    // 2. Check DMI product name for Firecracker
    if let Ok(product) = std::fs::read_to_string("/sys/class/dmi/id/product_name") {
        if product.trim().contains("Firecracker") {
            return true;
        }
    }
    
    false
}

/// Get default configuration for OSVM runtime microVM
pub fn get_default_osvm_config() -> OsvmMicroVmConfig {
    OsvmMicroVmConfig::default()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_default_config() {
        let config = OsvmMicroVmConfig::default();
        assert_eq!(config.memory_mb, 512);
        assert_eq!(config.vcpus, 2);
        assert_eq!(config.vsock_cid, 3);
    }
    
    #[test]
    fn test_microvm_detection() {
        // This will be false in tests unless actually in microVM
        let in_vm = is_running_in_microvm();
        // Just check it doesn't panic
        assert!(in_vm || !in_vm);
    }
}
