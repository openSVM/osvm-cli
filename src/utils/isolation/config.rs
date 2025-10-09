//! Configuration for isolation runtime

use super::IsolationLevel;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Isolation configuration for a component
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsolationConfig {
    /// Type of isolation to use
    pub isolation_type: IsolationType,

    /// Resource limits
    pub resource_limits: ResourceLimits,

    /// Network configuration
    pub network: NetworkConfig,

    /// Security options
    pub security: SecurityConfig,

    /// Additional runtime-specific options
    #[serde(default)]
    pub extra: HashMap<String, serde_json::Value>,
}

impl Default for IsolationConfig {
    fn default() -> Self {
        Self {
            isolation_type: IsolationType::None,
            resource_limits: ResourceLimits::default(),
            network: NetworkConfig::default(),
            security: SecurityConfig::default(),
            extra: HashMap::new(),
        }
    }
}

/// Type of isolation
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum IsolationType {
    /// No isolation
    None,

    /// Process sandbox
    ProcessSandbox {
        #[serde(default)]
        seccomp_profile: Option<String>,
        #[serde(default)]
        apparmor_profile: Option<String>,
    },

    /// Container
    Container {
        runtime: ContainerRuntime,
        #[serde(default)]
        image: Option<String>,
    },

    /// MicroVM
    MicroVM {
        hypervisor: HypervisorType,
        #[serde(default)]
        kernel_path: Option<PathBuf>,
        #[serde(default)]
        rootfs_path: Option<PathBuf>,
    },

    /// Unikernel
    Unikernel {
        runtime: UnikernelRuntime,
        #[serde(default)]
        image_path: Option<PathBuf>,
    },

    /// Trusted Execution Environment
    TEE {
        tee_type: TEEType,
        #[serde(default)]
        enclave_path: Option<PathBuf>,
    },
}

impl IsolationType {
    /// Get the isolation level
    pub fn level(&self) -> IsolationLevel {
        match self {
            IsolationType::None => IsolationLevel::None,
            IsolationType::ProcessSandbox { .. } => IsolationLevel::ProcessSandbox,
            IsolationType::Container { .. } => IsolationLevel::Container,
            IsolationType::MicroVM { .. } => IsolationLevel::MicroVM,
            IsolationType::Unikernel { .. } => IsolationLevel::Unikernel,
            IsolationType::TEE { .. } => IsolationLevel::TEE,
        }
    }
}

/// Container runtime options
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ContainerRuntime {
    Docker,
    Podman,
    Containerd,
}

/// Hypervisor type for microVMs
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum HypervisorType {
    /// AWS Firecracker
    Firecracker,
    /// Intel Cloud Hypervisor
    CloudHypervisor,
    /// Google crosvm
    Crosvm,
    /// QEMU with KVM
    Qemu,
}

/// Unikernel runtime
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum UnikernelRuntime {
    /// HermitCore (Rust-based)
    HermitCore,
    /// Nanos (Go/C support)
    Nanos,
    /// MirageOS (OCaml)
    MirageOS,
}

/// Trusted Execution Environment type
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TEEType {
    /// Intel SGX
    IntelSGX,
    /// AMD SEV
    AMDSEV,
    /// ARM TrustZone
    ARMTrustZone,
}

/// Resource limits for a component
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceLimits {
    /// Maximum memory in MB
    #[serde(default)]
    pub max_memory_mb: Option<u64>,

    /// Maximum CPU cores
    #[serde(default)]
    pub max_cpu_cores: Option<u32>,

    /// Maximum disk space in MB
    #[serde(default)]
    pub max_disk_mb: Option<u64>,

    /// Maximum network bandwidth in Mbps
    #[serde(default)]
    pub max_network_bandwidth_mbps: Option<u32>,

    /// Maximum file descriptors
    #[serde(default)]
    pub max_file_descriptors: Option<u32>,

    /// Maximum processes
    #[serde(default)]
    pub max_processes: Option<u32>,

    /// Maximum execution time in seconds
    #[serde(default)]
    pub max_execution_time_sec: Option<u64>,
}

impl Default for ResourceLimits {
    fn default() -> Self {
        Self {
            max_memory_mb: Some(512),
            max_cpu_cores: Some(2),
            max_disk_mb: Some(1024),
            max_network_bandwidth_mbps: Some(100),
            max_file_descriptors: Some(1024),
            max_processes: Some(10),
            max_execution_time_sec: None,
        }
    }
}

/// Network configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkConfig {
    /// Network access type
    pub access: NetworkAccess,

    /// Allowed destinations (for restricted access)
    #[serde(default)]
    pub allowed_destinations: Vec<String>,

    /// Firewall rules
    #[serde(default)]
    pub firewall_rules: Vec<FirewallRule>,

    /// Enable mTLS
    #[serde(default = "default_true")]
    pub mtls_enabled: bool,

    /// Certificate validity in days
    #[serde(default = "default_cert_validity")]
    pub certificate_validity_days: u32,
}

impl Default for NetworkConfig {
    fn default() -> Self {
        Self {
            access: NetworkAccess::Internal,
            allowed_destinations: vec![],
            firewall_rules: vec![],
            mtls_enabled: true,
            certificate_validity_days: 90,
        }
    }
}

fn default_true() -> bool {
    true
}

fn default_cert_validity() -> u32 {
    90
}

/// Network access level
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum NetworkAccess {
    /// No network access
    None,
    /// Only vsock to OSVM Core
    VsockOnly,
    /// Internal network only (mTLS to other components)
    Internal,
    /// Restricted external access (whitelist only)
    Restricted,
    /// Full network access
    Full,
}

/// Firewall rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FirewallRule {
    pub action: FirewallAction,
    pub direction: TrafficDirection,
    pub protocol: NetworkProtocol,
    #[serde(default)]
    pub port_range: Option<PortRange>,
    #[serde(default)]
    pub source: Option<String>,
    #[serde(default)]
    pub destination: Option<String>,
}

/// Firewall action
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum FirewallAction {
    Allow,
    Deny,
    Log,
}

/// Traffic direction
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TrafficDirection {
    Inbound,
    Outbound,
    Bidirectional,
}

/// Network protocol
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum NetworkProtocol {
    TCP,
    UDP,
    ICMP,
}

/// Port range
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct PortRange {
    pub start: u16,
    pub end: u16,
}

/// Security configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityConfig {
    /// Enable memory encryption (if available)
    #[serde(default)]
    pub memory_encryption: bool,

    /// Enable secure boot
    #[serde(default)]
    pub secure_boot: bool,

    /// Enable attestation
    #[serde(default)]
    pub attestation: bool,

    /// Read-only root filesystem
    #[serde(default = "default_true")]
    pub read_only_root: bool,

    /// Enable ASLR
    #[serde(default = "default_true")]
    pub aslr_enabled: bool,

    /// Enable stack canaries
    #[serde(default = "default_true")]
    pub stack_canaries: bool,
}

impl Default for SecurityConfig {
    fn default() -> Self {
        Self {
            memory_encryption: false,
            secure_boot: false,
            attestation: false,
            read_only_root: true,
            aslr_enabled: true,
            stack_canaries: true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_isolation_type_level() {
        assert_eq!(IsolationType::None.level(), IsolationLevel::None);
        assert_eq!(
            IsolationType::ProcessSandbox {
                seccomp_profile: None,
                apparmor_profile: None
            }
            .level(),
            IsolationLevel::ProcessSandbox
        );
        assert_eq!(
            IsolationType::MicroVM {
                hypervisor: HypervisorType::Firecracker,
                kernel_path: None,
                rootfs_path: None
            }
            .level(),
            IsolationLevel::MicroVM
        );
    }

    #[test]
    fn test_default_config() {
        let config = IsolationConfig::default();
        assert!(matches!(config.isolation_type, IsolationType::None));
        assert_eq!(config.resource_limits.max_memory_mb, Some(512));
        assert!(config.network.mtls_enabled);
        assert!(config.security.read_only_root);
    }

    #[test]
    fn test_serde_roundtrip() {
        let config = IsolationConfig {
            isolation_type: IsolationType::Unikernel {
                runtime: UnikernelRuntime::HermitCore,
                image_path: Some(PathBuf::from("/path/to/image")),
            },
            resource_limits: ResourceLimits {
                max_memory_mb: Some(256),
                max_cpu_cores: Some(2),
                ..Default::default()
            },
            network: NetworkConfig::default(),
            security: SecurityConfig::default(),
            extra: HashMap::new(),
        };

        let json = serde_json::to_string(&config).unwrap();
        let deserialized: IsolationConfig = serde_json::from_str(&json).unwrap();

        assert!(matches!(
            deserialized.isolation_type,
            IsolationType::Unikernel { .. }
        ));
    }

    #[test]
    fn test_resource_limits_all_fields() {
        let limits = ResourceLimits {
            max_cpu_cores: Some(4),
            max_memory_mb: Some(8192),
            max_disk_mb: Some(50000),
            max_network_bandwidth_mbps: Some(10000),
            max_file_descriptors: Some(4096),
            max_processes: Some(100),
            max_execution_time_sec: Some(3600),
        };

        assert_eq!(limits.max_cpu_cores, Some(4));
        assert_eq!(limits.max_memory_mb, Some(8192));
        assert_eq!(limits.max_network_bandwidth_mbps, Some(10000));
        assert_eq!(limits.max_execution_time_sec, Some(3600));
    }

    #[test]
    fn test_resource_limits_default_values() {
        let limits = ResourceLimits::default();
        assert_eq!(limits.max_memory_mb, Some(512));
        assert_eq!(limits.max_cpu_cores, Some(2));
        assert_eq!(limits.max_disk_mb, Some(1024));
        assert!(limits.max_network_bandwidth_mbps.is_some());
    }

    #[test]
    fn test_firewall_rule_creation() {
        let rule = FirewallRule {
            action: FirewallAction::Allow,
            direction: TrafficDirection::Inbound,
            protocol: NetworkProtocol::TCP,
            port_range: Some(PortRange {
                start: 8000,
                end: 9000,
            }),
            source: Some("10.0.0.0/8".to_string()),
            destination: None,
        };

        // Verify rule was created (can't test equality without PartialEq)
        assert!(matches!(rule.action, FirewallAction::Allow));
        assert!(matches!(rule.direction, TrafficDirection::Inbound));
        assert!(matches!(rule.protocol, NetworkProtocol::TCP));
        assert_eq!(rule.source, Some("10.0.0.0/8".to_string()));
    }
}
