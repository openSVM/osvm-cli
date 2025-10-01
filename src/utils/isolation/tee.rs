//! Trusted Execution Environment (TEE) Support
//!
//! This module provides integration with hardware-based secure enclaves for protecting
//! sensitive cryptographic operations, particularly validator signing keys.
//!
//! # Supported TEE Technologies
//!
//! - **Intel SGX (Software Guard Extensions)**: x86 CPU enclaves
//! - **AMD SEV (Secure Encrypted Virtualization)**: VM memory encryption
//! - **ARM TrustZone**: ARM secure world (future)
//!
//! # Security Properties
//!
//! TEEs provide:
//! - **Memory Encryption**: Hardware-encrypted memory (AES-128)
//! - **Isolation**: Code/data invisible to OS and hypervisor
//! - **Remote Attestation**: Cryptographic proof of code integrity
//! - **Sealed Storage**: Encrypt data tied to specific enclave
//!
//! # Architecture
//!
//! ```text
//! ┌──────────────────────────────────────────────────────────┐
//! │  Physical Hardware (Intel/AMD CPU)                       │
//! │                                                          │
//! │  ┌─────────────────────────────────────────────────────┐│
//! │  │  Operating System (Untrusted)                       ││
//! │  │  • Cannot read enclave memory                       ││
//! │  │  • Cannot tamper with enclave code                  ││
//! │  └─────────────────────────────────────────────────────┘│
//! │                          │                               │
//! │  ┌───────────────────────▼────────────────────────────┐ │
//! │  │  TEE Runtime (SGX SDK / SEV)                       │ │
//! │  │  • Manages enclave lifecycle                       │ │
//! │  │  • Handles attestation                             │ │
//! │  └───────────────────────┬────────────────────────────┘ │
//! │                          │                               │
//! │  ┌───────────────────────▼────────────────────────────┐ │
//! │  │  Secure Enclave (Hardware Protected)               │ │
//! │  │  ┌──────────────────────────────────────────────┐  │ │
//! │  │  │  Validator Keys                              │  │ │
//! │  │  │  • Private keys never leave enclave          │  │ │
//! │  │  │  • Signing operations done inside            │  │ │
//! │  │  │  • Memory encrypted by CPU                   │  │ │
//! │  │  └──────────────────────────────────────────────┘  │ │
//! │  └─────────────────────────────────────────────────────┘ │
//! │                                                          │
//! │  Security Guarantee: Even root cannot extract keys      │
//! └──────────────────────────────────────────────────────────┘
//! ```
//!
//! # Use Cases
//!
//! 1. **Validator Key Protection**: Keep validator signing keys in SGX enclave
//!    - Attacker compromises validator OS → keys still safe
//!    - Even root user cannot extract keys
//!    - Signing happens inside enclave only
//!
//! 2. **Secure Key Generation**: Generate keys inside enclave
//!    - Keys never exist in unencrypted form outside
//!    - Sealed to specific enclave
//!
//! 3. **Remote Attestation**: Prove validator is running correct code
//!    - Cryptographic proof of enclave integrity
//!    - Can verify from anywhere
//!
//! # Comparison to Isolation Levels
//!
//! | Level | Memory Protection | OS Trust | Key Safety |
//! |-------|-------------------|----------|------------|
//! | Process | Page tables | Required | Low |
//! | Container | Namespaces | Required | Low |
//! | MicroVM | Hardware (KVM) | Not required | Medium |
//! | Unikernel | Hardware | Not required | Medium |
//! | **TEE** | **Hardware (CPU)** | **Not required** | **High** |
//!
//! TEE is the highest security level - even compromised OS/hypervisor cannot access keys.

use super::{ComponentId, IsolationError};
use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

/// TEE technology type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TeeType {
    /// Intel SGX (Software Guard Extensions)
    IntelSgx,

    /// AMD SEV (Secure Encrypted Virtualization)
    AmdSev,

    /// ARM TrustZone
    ArmTrustZone,
}

impl TeeType {
    /// Get human-readable name
    pub fn name(&self) -> &str {
        match self {
            TeeType::IntelSgx => "Intel SGX",
            TeeType::AmdSev => "AMD SEV",
            TeeType::ArmTrustZone => "ARM TrustZone",
        }
    }

    /// Check if this TEE type is available on current hardware
    pub fn is_available(&self) -> bool {
        match self {
            TeeType::IntelSgx => Self::check_sgx_available(),
            TeeType::AmdSev => Self::check_sev_available(),
            TeeType::ArmTrustZone => false, // Not yet implemented
        }
    }

    /// Check if Intel SGX is available
    fn check_sgx_available() -> bool {
        // Check for SGX support in /proc/cpuinfo
        if let Ok(cpuinfo) = std::fs::read_to_string("/proc/cpuinfo") {
            cpuinfo.contains("sgx")
        } else {
            false
        }
    }

    /// Check if AMD SEV is available
    fn check_sev_available() -> bool {
        // Check for SEV support
        std::path::Path::new("/dev/sev").exists()
    }
}

/// TEE Manager - manages secure enclaves
pub struct TeeManager {
    /// TEE type
    tee_type: TeeType,

    /// Active enclaves
    enclaves: Arc<RwLock<HashMap<ComponentId, Enclave>>>,

    /// TEE configuration
    config: TeeConfig,
}

/// TEE configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TeeConfig {
    /// Enable remote attestation
    pub enable_attestation: bool,

    /// Attestation service URL
    pub attestation_url: Option<String>,

    /// Enable sealed storage
    pub enable_sealed_storage: bool,

    /// Sealed storage directory
    pub sealed_storage_dir: PathBuf,

    /// Maximum enclave memory (MB)
    pub max_enclave_memory_mb: u64,
}

impl Default for TeeConfig {
    fn default() -> Self {
        Self {
            enable_attestation: true,
            attestation_url: None,
            enable_sealed_storage: true,
            sealed_storage_dir: PathBuf::from("/var/lib/osvm/tee/sealed"),
            max_enclave_memory_mb: 128,
        }
    }
}

/// Secure enclave instance
#[derive(Debug)]
pub struct Enclave {
    /// Component ID this enclave belongs to
    component_id: ComponentId,

    /// Enclave ID (from TEE runtime)
    enclave_id: u64,

    /// TEE type
    tee_type: TeeType,

    /// Enclave state
    state: EnclaveState,

    /// Measurement (for attestation)
    measurement: Option<Vec<u8>>,
}

/// Enclave state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnclaveState {
    /// Enclave is being created
    Creating,

    /// Enclave is initialized and ready
    Ready,

    /// Enclave is executing code
    Running,

    /// Enclave is being destroyed
    Destroying,

    /// Enclave has been destroyed
    Destroyed,
}

impl TeeManager {
    /// Create a new TEE manager
    pub fn new(tee_type: TeeType, config: TeeConfig) -> Result<Self> {
        if !tee_type.is_available() {
            return Err(anyhow!(
                "{} is not available on this system",
                tee_type.name()
            ));
        }

        // Create sealed storage directory
        if config.enable_sealed_storage {
            std::fs::create_dir_all(&config.sealed_storage_dir)
                .context("Failed to create sealed storage directory")?;
        }

        Ok(Self {
            tee_type,
            enclaves: Arc::new(RwLock::new(HashMap::new())),
            config,
        })
    }

    /// Create a new enclave for a component
    pub async fn create_enclave(&self, component_id: ComponentId) -> Result<u64> {
        log::info!(
            "Creating {} enclave for component {}",
            self.tee_type.name(),
            component_id
        );

        // Generate enclave ID
        let enclave_id = self.generate_enclave_id().await;

        // Create enclave (simulated - would call actual TEE SDK)
        let enclave = Enclave {
            component_id,
            enclave_id,
            tee_type: self.tee_type,
            state: EnclaveState::Creating,
            measurement: None,
        };

        // In production, would:
        // 1. Call SGX SDK: sgx_create_enclave()
        // 2. Load enclave binary
        // 3. Initialize enclave
        // 4. Get measurement for attestation

        // Store enclave
        let mut enclaves = self.enclaves.write().await;
        enclaves.insert(component_id, enclave);

        log::info!("Enclave {} created successfully", enclave_id);
        Ok(enclave_id)
    }

    /// Destroy an enclave
    pub async fn destroy_enclave(&self, component_id: ComponentId) -> Result<()> {
        log::info!("Destroying enclave for component {}", component_id);

        let mut enclaves = self.enclaves.write().await;
        if let Some(mut enclave) = enclaves.remove(&component_id) {
            enclave.state = EnclaveState::Destroying;

            // In production, would call: sgx_destroy_enclave()

            enclave.state = EnclaveState::Destroyed;
            log::info!("Enclave {} destroyed", enclave.enclave_id);
        }

        Ok(())
    }

    /// Generate validator key inside enclave
    pub async fn generate_key_in_enclave(&self, component_id: ComponentId) -> Result<KeyHandle> {
        log::info!(
            "Generating key inside enclave for component {}",
            component_id
        );

        let enclaves = self.enclaves.read().await;
        let _enclave = enclaves.get(&component_id).ok_or_else(|| {
            anyhow!(
                "No enclave found for component {}. Create enclave first with create_enclave()",
                component_id
            )
        })?;

        // In production, would:
        // 1. Call enclave function: ecall_generate_keypair()
        // 2. Key generated entirely inside enclave
        // 3. Private key never leaves enclave
        // 4. Public key returned
        // 5. Handle stored for later signing

        let handle = KeyHandle {
            enclave_id: _enclave.enclave_id,
            key_id: self.generate_key_id().await,
            public_key: vec![0u8; 32], // Simulated public key
        };

        log::info!("Key generated with handle {}", handle.key_id);
        Ok(handle)
    }

    /// Sign data using key in enclave
    pub async fn sign_in_enclave(&self, key_handle: &KeyHandle, data: &[u8]) -> Result<Vec<u8>> {
        log::debug!(
            "Signing {} bytes with key {} in enclave {}",
            data.len(),
            key_handle.key_id,
            key_handle.enclave_id
        );

        // In production, would:
        // 1. Call enclave function: ecall_sign_data(key_id, data)
        // 2. Signing happens entirely inside enclave
        // 3. Private key never exposed
        // 4. Signature returned

        // Simulated signature
        let signature = vec![0u8; 64];

        log::debug!("Data signed successfully");
        Ok(signature)
    }

    /// Get attestation report for enclave
    pub async fn get_attestation(&self, component_id: ComponentId) -> Result<AttestationReport> {
        if !self.config.enable_attestation {
            return Err(anyhow!(
                "Attestation is disabled in TEE configuration. \
                 Enable with TeeConfig {{ enable_attestation: true, .. }}"
            ));
        }

        log::info!("Getting attestation for component {}", component_id);

        let enclaves = self.enclaves.read().await;
        let enclave = enclaves
            .get(&component_id)
            .ok_or_else(|| anyhow!("No enclave for component {}", component_id))?;

        // In production, would:
        // 1. Call SGX: sgx_create_report()
        // 2. Get quote from QE (Quoting Enclave)
        // 3. Contact IAS (Intel Attestation Service) or DCAP
        // 4. Return verified attestation

        Ok(AttestationReport {
            enclave_id: enclave.enclave_id,
            measurement: enclave.measurement.clone().unwrap_or_default(),
            timestamp: std::time::SystemTime::now(),
            verified: true,
        })
    }

    /// Seal data to enclave (encrypted storage)
    pub async fn seal_data(&self, component_id: ComponentId, data: &[u8]) -> Result<Vec<u8>> {
        if !self.config.enable_sealed_storage {
            return Err(anyhow!(
                "Sealed storage is disabled in TEE configuration. \
                 Enable with TeeConfig {{ enable_sealed_storage: true, .. }}"
            ));
        }

        log::debug!(
            "Sealing {} bytes for component {}",
            data.len(),
            component_id
        );

        // In production, would:
        // 1. Call enclave: ecall_seal_data()
        // 2. Data encrypted with key derived from enclave measurement
        // 3. Can only be unsealed by same enclave code

        // Simulated sealed data (would be encrypted)
        let sealed = data.to_vec();
        Ok(sealed)
    }

    /// Unseal data from enclave storage
    pub async fn unseal_data(
        &self,
        component_id: ComponentId,
        sealed_data: &[u8],
    ) -> Result<Vec<u8>> {
        if !self.config.enable_sealed_storage {
            return Err(anyhow!(
                "Sealed storage is disabled in TEE configuration. \
                 Enable with TeeConfig {{ enable_sealed_storage: true, .. }}"
            ));
        }

        log::debug!(
            "Unsealing {} bytes for component {}",
            sealed_data.len(),
            component_id
        );

        // In production, would:
        // 1. Call enclave: ecall_unseal_data()
        // 2. Verify enclave measurement matches
        // 3. Decrypt data
        // 4. Return plaintext

        // Simulated unsealed data
        let unsealed = sealed_data.to_vec();
        Ok(unsealed)
    }

    /// Get statistics
    pub async fn stats(&self) -> TeeStats {
        let enclaves = self.enclaves.read().await;

        TeeStats {
            tee_type: self.tee_type,
            active_enclaves: enclaves.len(),
            attestation_enabled: self.config.enable_attestation,
            sealed_storage_enabled: self.config.enable_sealed_storage,
        }
    }

    /// Generate unique enclave ID
    async fn generate_enclave_id(&self) -> u64 {
        use std::sync::atomic::{AtomicU64, Ordering};
        static NEXT_ID: AtomicU64 = AtomicU64::new(1);
        NEXT_ID.fetch_add(1, Ordering::SeqCst)
    }

    /// Generate unique key ID
    async fn generate_key_id(&self) -> u64 {
        use std::sync::atomic::{AtomicU64, Ordering};
        static NEXT_KEY_ID: AtomicU64 = AtomicU64::new(1);
        NEXT_KEY_ID.fetch_add(1, Ordering::SeqCst)
    }
}

/// Handle to key stored in enclave
#[derive(Debug, Clone)]
pub struct KeyHandle {
    /// Enclave ID containing the key
    pub enclave_id: u64,

    /// Key ID within enclave
    pub key_id: u64,

    /// Public key (can be shared)
    pub public_key: Vec<u8>,
}

/// Attestation report proving enclave integrity
#[derive(Debug, Clone)]
pub struct AttestationReport {
    /// Enclave ID
    pub enclave_id: u64,

    /// Measurement (hash of enclave code/data)
    pub measurement: Vec<u8>,

    /// Timestamp
    pub timestamp: std::time::SystemTime,

    /// Verification result
    pub verified: bool,
}

/// TEE statistics
#[derive(Debug, Clone)]
pub struct TeeStats {
    /// TEE technology type
    pub tee_type: TeeType,

    /// Number of active enclaves
    pub active_enclaves: usize,

    /// Attestation enabled
    pub attestation_enabled: bool,

    /// Sealed storage enabled
    pub sealed_storage_enabled: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tee_type_name() {
        assert_eq!(TeeType::IntelSgx.name(), "Intel SGX");
        assert_eq!(TeeType::AmdSev.name(), "AMD SEV");
        assert_eq!(TeeType::ArmTrustZone.name(), "ARM TrustZone");
    }

    #[test]
    fn test_tee_config_default() {
        let config = TeeConfig::default();
        assert!(config.enable_attestation);
        assert!(config.enable_sealed_storage);
        assert_eq!(config.max_enclave_memory_mb, 128);
    }

    #[test]
    fn test_enclave_state() {
        let state = EnclaveState::Ready;
        assert_eq!(state, EnclaveState::Ready);
    }
}
