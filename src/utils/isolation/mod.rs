//! Isolation runtime support for OSVM components
//!
//! This module provides the foundation for running OSVM components in isolated
//! environments (unikernels, microVMs) with hardware-enforced security boundaries.
//!
//! # Architecture
//!
//! The isolation system consists of several layers:
//! - **Runtime Layer**: Abstractions for different isolation technologies (unikernels, microVMs)
//! - **Network Layer**: Zero-trust networking with mTLS authentication
//! - **Security Layer**: Certificate management, attestation, policy enforcement
//! - **Orchestration Layer**: Component lifecycle management
//!
//! # Supported Isolation Levels
//!
//! - **None**: No isolation (legacy mode)
//! - **ProcessSandbox**: OS-level sandboxing (seccomp, AppArmor)
//! - **Container**: Container isolation (shared kernel)
//! - **MicroVM**: Lightweight VM with hardware isolation (Firecracker)
//! - **Unikernel**: Single-purpose OS with minimal attack surface (HermitCore)
//! - **TEE**: Trusted Execution Environment (SGX, SEV)

use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

pub mod autoscaler;
pub mod certificate;
pub mod component;
pub mod config;
pub mod hotswap;
pub mod network;
pub mod orchestrator;
pub mod policy;
pub mod runtime;
pub mod tee;
pub mod vsock;

// Re-exports for convenience
pub use autoscaler::{AutoScaler, AutoScalerConfig, ScalingPolicy, ComponentMetrics};
pub use certificate::{CertificateAuthority, CertificateManager};
pub use component::{Component, ComponentId, ComponentRegistry, ComponentStatus, ComponentType};
pub use config::{IsolationConfig, IsolationType, ResourceLimits};
pub use hotswap::{HotSwapManager, HotSwapConfig, HotSwapResult};
pub use network::{NetworkManager, ZeroTrustNetwork};
pub use orchestrator::{Orchestrator, OrchestratorConfig, OrchestratorStats};
pub use policy::{Policy, PolicyEngine};
pub use runtime::{Runtime, RuntimeManager};
pub use tee::{TeeManager, TeeType, TeeConfig, KeyHandle, AttestationReport};
pub use vsock::{VsockManager, VsockAddr, VsockConnection, Cid, Port};

/// Isolation level for a component
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum IsolationLevel {
    /// No isolation (development/testing only)
    None,
    /// Process-level sandboxing
    ProcessSandbox,
    /// Container isolation (shared kernel)
    Container,
    /// MicroVM with hardware isolation
    MicroVM,
    /// Unikernel with minimal attack surface
    Unikernel,
    /// Trusted Execution Environment
    TEE,
}

impl IsolationLevel {
    /// Get human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            IsolationLevel::None => "None",
            IsolationLevel::ProcessSandbox => "Process Sandbox",
            IsolationLevel::Container => "Container",
            IsolationLevel::MicroVM => "MicroVM",
            IsolationLevel::Unikernel => "Unikernel",
            IsolationLevel::TEE => "TEE",
        }
    }

    /// Get security score (0-100)
    pub fn security_score(&self) -> u8 {
        match self {
            IsolationLevel::None => 0,
            IsolationLevel::ProcessSandbox => 40,
            IsolationLevel::Container => 60,
            IsolationLevel::MicroVM => 80,
            IsolationLevel::Unikernel => 95,
            IsolationLevel::TEE => 100,
        }
    }

    /// Check if this level provides hardware isolation
    pub fn has_hardware_isolation(&self) -> bool {
        matches!(
            self,
            IsolationLevel::MicroVM | IsolationLevel::Unikernel | IsolationLevel::TEE
        )
    }
}

/// Error types for isolation operations
#[derive(Debug, thiserror::Error)]
pub enum IsolationError {
    #[error("Runtime not available: {0}")]
    RuntimeNotAvailable(String),

    #[error("Insufficient privileges: {0}")]
    InsufficientPrivileges(String),

    #[error("Component not found: {0}")]
    ComponentNotFound(String),

    #[error("Certificate error: {0}")]
    CertificateError(String),

    #[error("Network error: {0}")]
    NetworkError(String),

    #[error("Policy violation: {0}")]
    PolicyViolation(String),

    #[error("Configuration error: {0}")]
    ConfigError(String),

    #[error("Operation failed: {0}")]
    OperationFailed(String),
}

// IsolationError already implements std::error::Error through thiserror
// anyhow::Error will automatically convert it

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_isolation_level_ordering() {
        assert!(IsolationLevel::None < IsolationLevel::ProcessSandbox);
        assert!(IsolationLevel::ProcessSandbox < IsolationLevel::Container);
        assert!(IsolationLevel::Container < IsolationLevel::MicroVM);
        assert!(IsolationLevel::MicroVM < IsolationLevel::Unikernel);
        assert!(IsolationLevel::Unikernel < IsolationLevel::TEE);
    }

    #[test]
    fn test_isolation_level_security_scores() {
        assert_eq!(IsolationLevel::None.security_score(), 0);
        assert_eq!(IsolationLevel::TEE.security_score(), 100);
        assert!(IsolationLevel::Unikernel.security_score() > IsolationLevel::MicroVM.security_score());
    }

    #[test]
    fn test_hardware_isolation() {
        assert!(!IsolationLevel::None.has_hardware_isolation());
        assert!(!IsolationLevel::ProcessSandbox.has_hardware_isolation());
        assert!(!IsolationLevel::Container.has_hardware_isolation());
        assert!(IsolationLevel::MicroVM.has_hardware_isolation());
        assert!(IsolationLevel::Unikernel.has_hardware_isolation());
        assert!(IsolationLevel::TEE.has_hardware_isolation());
    }
}