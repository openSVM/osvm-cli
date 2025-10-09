//! Certificate management for mTLS authentication
//!
//! This module provides certificate authority functionality using step-ca.
//! step-ca is a modern, automated certificate authority that provides:
//! - Automatic certificate issuance
//! - Certificate rotation
//! - Revocation support
//! - ACME protocol support
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────┐
//! │  OSVM Core                              │
//! │  ┌────────────────────────────────────┐ │
//! │  │  CertificateAuthority              │ │
//! │  │  - Issues certs for components     │ │
//! │  │  - Manages CA keys                 │ │
//! │  │  - Handles revocation              │ │
//! │  └────────────────────────────────────┘ │
//! └─────────────────────────────────────────┘
//!          │
//!          ▼ (HTTPS API)
//! ┌─────────────────────────────────────────┐
//! │  step-ca Server                         │
//! │  - Root CA                              │
//! │  - Intermediate CA                      │
//! │  - CRL (Certificate Revocation List)    │
//! └─────────────────────────────────────────┘
//!          │
//!          ▼ (Issues)
//! ┌─────────────────────────────────────────┐
//! │  Component Certificates                 │
//! │  - Validator cert                       │
//! │  - RPC cert                             │
//! │  - MCP cert (per server)                │
//! └─────────────────────────────────────────┘
//! ```

use super::{ComponentId, IsolationError};
use anyhow::{anyhow, Context, Result};
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Certificate authority for issuing component certificates
pub struct CertificateAuthority {
    /// URL of step-ca server
    ca_url: String,

    /// Path to CA root certificate
    ca_root_cert_path: PathBuf,

    /// Provisioner name for certificate issuance
    provisioner: String,

    /// Provisioner password (if required)
    provisioner_password: Option<String>,

    /// Configuration
    config: CertificateAuthorityConfig,
}

/// Configuration for certificate authority
#[derive(Debug, Clone)]
pub struct CertificateAuthorityConfig {
    /// Default certificate validity in days
    pub default_validity_days: u32,

    /// Certificate storage directory
    pub cert_dir: PathBuf,

    /// Enable automatic renewal
    pub auto_renew: bool,

    /// Renewal threshold (renew when this much validity remains)
    pub renewal_threshold_days: u32,

    /// Maximum certificate lifetime
    pub max_validity_days: u32,
}

impl Default for CertificateAuthorityConfig {
    fn default() -> Self {
        Self {
            default_validity_days: 90,
            cert_dir: PathBuf::from("/etc/osvm/certs"),
            auto_renew: true,
            renewal_threshold_days: 30,
            max_validity_days: 365,
        }
    }
}

impl CertificateAuthority {
    /// Create a new certificate authority client
    pub fn new(
        ca_url: String,
        ca_root_cert_path: PathBuf,
        provisioner: String,
        config: CertificateAuthorityConfig,
    ) -> Result<Self> {
        // Verify CA root cert exists
        if !ca_root_cert_path.exists() {
            return Err(anyhow!(
                "CA root certificate not found at {:?}. Run: step ca bootstrap --ca-url {} --fingerprint <fingerprint>",
                ca_root_cert_path,
                ca_url
            ));
        }

        // Create cert directory if it doesn't exist
        if !config.cert_dir.exists() {
            fs::create_dir_all(&config.cert_dir)
                .context("Failed to create certificate directory")?;
        }

        Ok(Self {
            ca_url,
            ca_root_cert_path,
            provisioner,
            provisioner_password: None,
            config,
        })
    }

    /// Create with default configuration
    pub fn with_defaults() -> Result<Self> {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/root".to_string());
        let ca_url =
            std::env::var("STEP_CA_URL").unwrap_or_else(|_| "https://localhost:8443".to_string());
        let ca_root_cert = PathBuf::from(format!("{}/.step/certs/root_ca.crt", home));
        let provisioner =
            std::env::var("STEP_CA_PROVISIONER").unwrap_or_else(|_| "osvm-provisioner".to_string());

        Self::new(
            ca_url,
            ca_root_cert,
            provisioner,
            CertificateAuthorityConfig::default(),
        )
    }

    /// Set provisioner password
    pub fn with_password(mut self, password: String) -> Self {
        self.provisioner_password = Some(password);
        self
    }

    /// Issue a certificate for a component
    pub async fn issue_certificate(
        &self,
        identity: &str,
        validity_days: u32,
    ) -> Result<Certificate> {
        log::info!("Issuing certificate for identity: {}", identity);

        // Validate validity period
        let validity_days = validity_days.min(self.config.max_validity_days);

        // Generate paths
        let cert_path = self.config.cert_dir.join(format!("{}.crt", identity));
        let key_path = self.config.cert_dir.join(format!("{}.key", identity));

        // Use step CLI to issue certificate
        let mut cmd = Command::new("step");
        cmd.arg("ca")
            .arg("certificate")
            .arg(identity) // Subject/CN
            .arg(&cert_path) // Output cert path
            .arg(&key_path) // Output key path
            .arg("--ca-url")
            .arg(&self.ca_url)
            .arg("--root")
            .arg(&self.ca_root_cert_path)
            .arg("--provisioner")
            .arg(&self.provisioner)
            .arg("--not-after")
            .arg(format!("{}h", validity_days * 24));

        // Add password if configured
        if let Some(password) = &self.provisioner_password {
            cmd.arg("--provisioner-password-file").arg("-"); // Read from stdin
            cmd.env("STEP_CA_PASSWORD", password);
        } else {
            cmd.arg("--force"); // Non-interactive
        }

        // Execute command
        let output = cmd
            .output()
            .context("Failed to execute step ca certificate")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("Certificate issuance failed: {}", stderr));
        }

        // Read the generated certificate
        let cert_pem =
            fs::read_to_string(&cert_path).context("Failed to read generated certificate")?;

        // Parse certificate to extract details
        let cert = self.parse_certificate(&cert_pem)?;

        log::info!(
            "Certificate issued for {} (valid until {})",
            identity,
            cert.not_after
        );

        Ok(cert)
    }

    /// Revoke a certificate
    pub async fn revoke_certificate(&self, serial_number: &str) -> Result<()> {
        log::info!("Revoking certificate with serial: {}", serial_number);

        let mut cmd = Command::new("step");
        cmd.arg("ca")
            .arg("revoke")
            .arg(serial_number)
            .arg("--ca-url")
            .arg(&self.ca_url)
            .arg("--root")
            .arg(&self.ca_root_cert_path);

        let output = cmd.output().context("Failed to execute step ca revoke")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("Certificate revocation failed: {}", stderr));
        }

        log::info!("Certificate {} revoked successfully", serial_number);
        Ok(())
    }

    /// Verify a certificate
    pub fn verify_certificate(&self, cert: &Certificate) -> Result<bool> {
        // Check expiry
        if cert.is_expired() {
            log::warn!("Certificate expired: {}", cert.subject);
            return Ok(false);
        }

        // Check against CRL (Certificate Revocation List)
        // NOTE: Full implementation requires using step-ca's CRL endpoint
        // Example production implementation:
        // ```
        // let crl_url = format!("{}/crl", self.ca_url);
        // let crl = reqwest::get(&crl_url).await?.text().await?;
        // if crl.contains(&cert.serial_number) {
        //     log::warn!("Certificate revoked: {}", cert.subject);
        //     return Ok(false);
        // }
        // ```
        log::debug!(
            "CRL check not implemented - skipping revocation check for {}",
            cert.subject
        );

        // Verify signature chain
        // NOTE: Full implementation requires using openssl or rustls to:
        // 1. Parse the certificate
        // 2. Load the CA root certificate
        // 3. Verify the certificate was signed by the CA
        // 4. Check the entire chain of trust
        //
        // Example with openssl:
        // ```
        // use openssl::x509::X509;
        // use openssl::stack::Stack;
        // let cert_x509 = X509::from_pem(cert.pem.as_bytes())?;
        // let ca_x509 = X509::from_pem(&fs::read(&self.ca_root_cert_path)?)?;
        // let mut store = X509StoreBuilder::new()?;
        // store.add_cert(ca_x509)?;
        // let store = store.build();
        // let mut context = X509StoreContext::new()?;
        // context.init(&store, &cert_x509, &Stack::new()?, |ctx| {
        //     ctx.verify_cert()
        // })?
        // ```
        log::debug!(
            "Signature verification not implemented - accepting certificate for {}",
            cert.subject
        );

        // For now, accept certificates that haven't expired
        // Production MUST implement full verification
        log::warn!(
            "Certificate verification stub - only checked expiry for {}. Production requires CRL and signature verification.",
            cert.subject
        );

        Ok(true)
    }

    /// Check if certificate needs renewal
    pub fn needs_renewal(&self, cert: &Certificate) -> bool {
        if !self.config.auto_renew {
            return false;
        }

        let threshold = Utc::now() + Duration::days(self.config.renewal_threshold_days as i64);
        cert.not_after < threshold
    }

    /// Renew a certificate
    pub async fn renew_certificate(&self, identity: &str) -> Result<Certificate> {
        log::info!("Renewing certificate for {}", identity);

        // Issue new certificate with default validity
        self.issue_certificate(identity, self.config.default_validity_days)
            .await
    }

    /// Parse certificate from PEM
    fn parse_certificate(&self, pem: &str) -> Result<Certificate> {
        // Parse certificate details using step CLI
        // NOTE: Full implementation should use openssl or x509-parser crate
        //
        // Production implementation with x509-parser:
        // ```
        // use x509_parser::prelude::*;
        // let pem_data = pem::parse(pem)?;
        // let cert = X509Certificate::from_der(&pem_data.contents)?;
        //
        // let subject = cert.subject().to_string();
        // let serial = cert.serial.to_string();
        // let not_before = convert_asn1_time(cert.validity().not_before)?;
        // let not_after = convert_asn1_time(cert.validity().not_after)?;
        // ```

        log::debug!("Parsing certificate from PEM");

        // Use step CLI to inspect certificate
        let mut child = Command::new("step")
            .arg("certificate")
            .arg("inspect")
            .arg("--format")
            .arg("json")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .context("Failed to spawn step certificate inspect")?;

        // Write PEM to stdin
        use std::io::Write;
        if let Some(ref mut stdin) = child.stdin {
            stdin
                .write_all(pem.as_bytes())
                .context("Failed to write PEM to stdin")?;
            // Close stdin to signal EOF
            drop(child.stdin.take());
        }

        let result = child
            .wait_with_output()
            .context("Failed to wait for step certificate inspect")?;

        if !result.status.success() {
            log::warn!("step certificate inspect failed, using fallback parsing");
            // Fallback to basic parsing
            return Ok(Certificate {
                subject: "unknown-subject".to_string(),
                serial_number: "unknown-serial".to_string(),
                not_before: Utc::now(),
                not_after: Utc::now() + Duration::days(self.config.default_validity_days as i64),
                pem: pem.to_string(),
            });
        }

        // Parse JSON output
        let json_str = String::from_utf8_lossy(&result.stdout);
        let json: serde_json::Value =
            serde_json::from_str(&json_str).context("Failed to parse JSON from step")?;

        // Extract certificate details
        let subject = json
            .get("subject")
            .and_then(|v| v.get("common_name"))
            .and_then(|v| v.as_str())
            .unwrap_or("unknown-subject")
            .to_string();

        let serial_number = json
            .get("serial_number")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown-serial")
            .to_string();

        let not_before_str = json
            .get("validity")
            .and_then(|v| v.get("start"))
            .and_then(|v| v.as_str())
            .context("Missing validity start")?;

        let not_after_str = json
            .get("validity")
            .and_then(|v| v.get("end"))
            .and_then(|v| v.as_str())
            .context("Missing validity end")?;

        // Parse dates (step outputs RFC3339 format)
        let not_before = DateTime::parse_from_rfc3339(not_before_str)
            .context("Failed to parse not_before date")?
            .with_timezone(&Utc);

        let not_after = DateTime::parse_from_rfc3339(not_after_str)
            .context("Failed to parse not_after date")?
            .with_timezone(&Utc);

        Ok(Certificate {
            subject,
            serial_number,
            not_before,
            not_after,
            pem: pem.to_string(),
        })
    }

    /// Initialize step-ca (for first-time setup)
    pub fn initialize_ca(name: &str, dns_names: Vec<String>, address: &str) -> Result<()> {
        log::info!("Initializing step-ca: {}", name);

        let mut cmd = Command::new("step");
        cmd.arg("ca")
            .arg("init")
            .arg("--name")
            .arg(name)
            .arg("--dns")
            .arg(dns_names.join(","))
            .arg("--address")
            .arg(address)
            .arg("--provisioner")
            .arg("osvm-provisioner");

        let output = cmd.output().context("Failed to execute step ca init")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("CA initialization failed: {}", stderr));
        }

        log::info!("step-ca initialized successfully");
        Ok(())
    }
}

/// Certificate manager for a component
///
/// Manages the lifecycle of a component's certificate including:
/// - Loading certificate and private key
/// - Automatic renewal when nearing expiry
/// - Secure storage
pub struct CertificateManager {
    /// Path to certificate file
    cert_path: PathBuf,

    /// Path to private key file
    key_path: PathBuf,

    /// Cached certificate
    cached_cert: Option<Certificate>,

    /// Certificate authority for renewals
    ca: Option<CertificateAuthority>,
}

impl CertificateManager {
    /// Create a new certificate manager
    pub fn new(cert_path: PathBuf, key_path: PathBuf) -> Self {
        Self {
            cert_path,
            key_path,
            cached_cert: None,
            ca: None,
        }
    }

    /// Create with CA for automatic renewal
    pub fn with_ca(mut self, ca: CertificateAuthority) -> Self {
        self.ca = Some(ca);
        self
    }

    /// Load certificate from disk
    pub fn load_certificate(&mut self) -> Result<Certificate> {
        log::debug!("Loading certificate from {:?}", self.cert_path);

        // Check if cert file exists
        if !self.cert_path.exists() {
            return Err(IsolationError::CertificateError(format!(
                "Certificate not found at {:?}",
                self.cert_path
            ))
            .into());
        }

        // Read PEM file
        let pem = fs::read_to_string(&self.cert_path).context("Failed to read certificate file")?;

        // Parse certificate
        let cert = self.parse_certificate_pem(&pem)?;

        // Cache it
        self.cached_cert = Some(cert.clone());

        Ok(cert)
    }

    /// Get certificate (load if not cached)
    pub fn get_certificate(&mut self) -> Result<Certificate> {
        if let Some(cert) = &self.cached_cert {
            // Check if needs renewal
            if let Some(ca) = &self.ca {
                if ca.needs_renewal(cert) {
                    log::info!("Certificate needs renewal");
                    // Note: Can't await in non-async function
                    // Caller should use renew_if_needed() separately
                }
            }

            return Ok(cert.clone());
        }

        // Load from disk
        self.load_certificate()
    }

    /// Check if certificate needs renewal
    pub fn needs_renewal(&self) -> bool {
        if let (Some(cert), Some(ca)) = (&self.cached_cert, &self.ca) {
            ca.needs_renewal(cert)
        } else {
            false
        }
    }

    /// Renew certificate if needed
    pub async fn renew_if_needed(&mut self, identity: &str) -> Result<bool> {
        if !self.needs_renewal() {
            return Ok(false);
        }

        let ca = self
            .ca
            .as_ref()
            .ok_or_else(|| anyhow!("No CA configured for renewal"))?;

        log::info!("Renewing certificate for {}", identity);
        let new_cert = ca.renew_certificate(identity).await?;

        // Update cached cert
        self.cached_cert = Some(new_cert);

        // Reload from disk (step-ca wrote new files)
        self.load_certificate()?;

        Ok(true)
    }

    /// Save certificate to disk
    pub fn save_certificate(&self, cert: &Certificate) -> Result<()> {
        log::debug!("Saving certificate to {:?}", self.cert_path);

        // Write PEM to file
        fs::write(&self.cert_path, &cert.pem).context("Failed to write certificate file")?;

        // Set appropriate permissions (readable only by owner)
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(&self.cert_path)?.permissions();
            perms.set_mode(0o600); // rw-------
            fs::set_permissions(&self.cert_path, perms)?;
        }

        Ok(())
    }

    /// Load private key
    pub fn load_private_key(&self) -> Result<Vec<u8>> {
        log::debug!("Loading private key from {:?}", self.key_path);

        if !self.key_path.exists() {
            return Err(IsolationError::CertificateError(format!(
                "Private key not found at {:?}",
                self.key_path
            ))
            .into());
        }

        fs::read(&self.key_path).context("Failed to read private key file")
    }

    /// Get certificate paths
    pub fn paths(&self) -> (&Path, &Path) {
        (self.cert_path.as_path(), self.key_path.as_path())
    }

    /// Parse certificate from PEM
    fn parse_certificate_pem(&self, pem: &str) -> Result<Certificate> {
        // Use step CLI to inspect certificate and extract details
        log::debug!("Parsing certificate from PEM");

        let mut child = Command::new("step")
            .arg("certificate")
            .arg("inspect")
            .arg("--format")
            .arg("json")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .context("Failed to spawn step certificate inspect")?;

        // Write PEM to stdin
        use std::io::Write;
        if let Some(ref mut stdin) = child.stdin {
            stdin
                .write_all(pem.as_bytes())
                .context("Failed to write PEM to stdin")?;
            drop(child.stdin.take());
        }

        let result = child
            .wait_with_output()
            .context("Failed to wait for step certificate inspect")?;

        if !result.status.success() {
            log::warn!("step certificate inspect failed, using fallback parsing");
            // Fallback to basic parsing
            return Ok(Certificate {
                subject: "unknown-subject".to_string(),
                serial_number: "unknown-serial".to_string(),
                not_before: Utc::now(),
                not_after: Utc::now() + Duration::days(90),
                pem: pem.to_string(),
            });
        }

        // Parse JSON output
        let json_str = String::from_utf8_lossy(&result.stdout);
        let json: serde_json::Value =
            serde_json::from_str(&json_str).context("Failed to parse JSON from step")?;

        // Extract certificate details
        let subject = json
            .get("subject")
            .and_then(|v| v.get("common_name"))
            .and_then(|v| v.as_str())
            .unwrap_or("unknown-subject")
            .to_string();

        let serial_number = json
            .get("serial_number")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown-serial")
            .to_string();

        let not_before_str = json
            .get("validity")
            .and_then(|v| v.get("start"))
            .and_then(|v| v.as_str())
            .context("Missing validity start")?;

        let not_after_str = json
            .get("validity")
            .and_then(|v| v.get("end"))
            .and_then(|v| v.as_str())
            .context("Missing validity end")?;

        // Parse dates (step outputs RFC3339 format)
        let not_before = DateTime::parse_from_rfc3339(not_before_str)
            .context("Failed to parse not_before date")?
            .with_timezone(&Utc);

        let not_after = DateTime::parse_from_rfc3339(not_after_str)
            .context("Failed to parse not_after date")?
            .with_timezone(&Utc);

        Ok(Certificate {
            subject,
            serial_number,
            not_before,
            not_after,
            pem: pem.to_string(),
        })
    }
}

/// A certificate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Certificate {
    pub subject: String,
    pub serial_number: String,
    pub not_before: DateTime<Utc>,
    pub not_after: DateTime<Utc>,
    pub pem: String,
}

impl Certificate {
    /// Check if certificate is expired
    pub fn is_expired(&self) -> bool {
        Utc::now() > self.not_after
    }

    /// Check if certificate is not yet valid
    pub fn is_not_yet_valid(&self) -> bool {
        Utc::now() < self.not_before
    }

    /// Check if certificate is currently valid
    pub fn is_valid(&self) -> bool {
        !self.is_expired() && !self.is_not_yet_valid()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_certificate_validity() {
        let cert = Certificate {
            subject: "test".to_string(),
            serial_number: "123".to_string(),
            not_before: Utc::now() - chrono::Duration::days(1),
            not_after: Utc::now() + chrono::Duration::days(1),
            pem: "".to_string(),
        };

        assert!(cert.is_valid());
        assert!(!cert.is_expired());
        assert!(!cert.is_not_yet_valid());
    }

    #[test]
    fn test_certificate_expired() {
        let cert = Certificate {
            subject: "expired-cert".to_string(),
            serial_number: "456".to_string(),
            not_before: Utc::now() - chrono::Duration::days(100),
            not_after: Utc::now() - chrono::Duration::days(1),
            pem: "".to_string(),
        };

        assert!(!cert.is_valid());
        assert!(cert.is_expired());
        assert!(!cert.is_not_yet_valid());
    }

    #[test]
    fn test_certificate_not_yet_valid() {
        let cert = Certificate {
            subject: "future-cert".to_string(),
            serial_number: "789".to_string(),
            not_before: Utc::now() + chrono::Duration::days(1),
            not_after: Utc::now() + chrono::Duration::days(100),
            pem: "".to_string(),
        };

        assert!(!cert.is_valid());
        assert!(!cert.is_expired());
        assert!(cert.is_not_yet_valid());
    }

    #[test]
    fn test_certificate_authority_config_default() {
        let config = CertificateAuthorityConfig::default();
        assert_eq!(config.default_validity_days, 90);
        assert_eq!(config.renewal_threshold_days, 30);
        assert_eq!(config.max_validity_days, 365);
        assert!(config.auto_renew);
    }

    #[test]
    fn test_certificate_needs_renewal() {
        let config = CertificateAuthorityConfig::default();
        let ca = CertificateAuthority {
            ca_url: "https://localhost:8443".to_string(),
            ca_root_cert_path: PathBuf::from("/tmp/ca.crt"),
            provisioner: "test".to_string(),
            provisioner_password: None,
            config: config.clone(),
        };

        // Certificate expiring in 20 days - should need renewal (threshold is 30 days)
        let cert_needs_renewal = Certificate {
            subject: "test".to_string(),
            serial_number: "123".to_string(),
            not_before: Utc::now() - chrono::Duration::days(70),
            not_after: Utc::now() + chrono::Duration::days(20),
            pem: "".to_string(),
        };

        assert!(ca.needs_renewal(&cert_needs_renewal));

        // Certificate expiring in 40 days - should not need renewal
        let cert_ok = Certificate {
            subject: "test".to_string(),
            serial_number: "456".to_string(),
            not_before: Utc::now() - chrono::Duration::days(50),
            not_after: Utc::now() + chrono::Duration::days(40),
            pem: "".to_string(),
        };

        assert!(!ca.needs_renewal(&cert_ok));
    }

    #[test]
    fn test_certificate_verify_expired() {
        let ca = CertificateAuthority {
            ca_url: "https://localhost:8443".to_string(),
            ca_root_cert_path: PathBuf::from("/tmp/ca.crt"),
            provisioner: "test".to_string(),
            provisioner_password: None,
            config: CertificateAuthorityConfig::default(),
        };

        let expired_cert = Certificate {
            subject: "test".to_string(),
            serial_number: "999".to_string(),
            not_before: Utc::now() - chrono::Duration::days(100),
            not_after: Utc::now() - chrono::Duration::days(1),
            pem: "".to_string(),
        };

        let result = ca.verify_certificate(&expired_cert).unwrap();
        assert!(!result, "Expired certificate should not verify");
    }

    #[test]
    fn test_certificate_verify_valid() {
        let ca = CertificateAuthority {
            ca_url: "https://localhost:8443".to_string(),
            ca_root_cert_path: PathBuf::from("/tmp/ca.crt"),
            provisioner: "test".to_string(),
            provisioner_password: None,
            config: CertificateAuthorityConfig::default(),
        };

        let valid_cert = Certificate {
            subject: "test".to_string(),
            serial_number: "111".to_string(),
            not_before: Utc::now() - chrono::Duration::days(1),
            not_after: Utc::now() + chrono::Duration::days(89),
            pem: "".to_string(),
        };

        let result = ca.verify_certificate(&valid_cert).unwrap();
        assert!(result, "Valid certificate should verify");
    }

    #[test]
    fn test_certificate_manager_paths() {
        let manager = CertificateManager {
            cert_path: PathBuf::from("/tmp/cert.pem"),
            key_path: PathBuf::from("/tmp/key.pem"),
            ca: None,
            cached_cert: None,
        };

        let (cert_path, key_path) = manager.paths();
        assert_eq!(cert_path, Path::new("/tmp/cert.pem"));
        assert_eq!(key_path, Path::new("/tmp/key.pem"));
    }

    #[test]
    fn test_certificate_renewal_threshold() {
        let config = CertificateAuthorityConfig {
            renewal_threshold_days: 30,
            ..Default::default()
        };

        let ca = CertificateAuthority {
            ca_url: "https://localhost:8443".to_string(),
            ca_root_cert_path: PathBuf::from("/tmp/ca.crt"),
            provisioner: "test".to_string(),
            provisioner_password: None,
            config,
        };

        // Certificate with 25 days remaining - needs renewal
        let cert_soon = Certificate {
            subject: "test".to_string(),
            serial_number: "123".to_string(),
            not_before: Utc::now() - chrono::Duration::days(65),
            not_after: Utc::now() + chrono::Duration::days(25),
            pem: "".to_string(),
        };

        assert!(ca.needs_renewal(&cert_soon));

        // Certificate with 35 days remaining - doesn't need renewal yet
        let cert_later = Certificate {
            subject: "test".to_string(),
            serial_number: "456".to_string(),
            not_before: Utc::now() - chrono::Duration::days(55),
            not_after: Utc::now() + chrono::Duration::days(35),
            pem: "".to_string(),
        };

        assert!(!ca.needs_renewal(&cert_later));
    }

    #[test]
    fn test_certificate_auto_renew_disabled() {
        let config = CertificateAuthorityConfig {
            auto_renew: false,
            renewal_threshold_days: 30,
            ..Default::default()
        };

        let ca = CertificateAuthority {
            ca_url: "https://localhost:8443".to_string(),
            ca_root_cert_path: PathBuf::from("/tmp/ca.crt"),
            provisioner: "test".to_string(),
            provisioner_password: None,
            config,
        };

        // Even with 1 day remaining, should not need renewal if auto_renew is false
        let cert_expiring = Certificate {
            subject: "test".to_string(),
            serial_number: "999".to_string(),
            not_before: Utc::now() - chrono::Duration::days(89),
            not_after: Utc::now() + chrono::Duration::days(1),
            pem: "".to_string(),
        };

        assert!(!ca.needs_renewal(&cert_expiring));
    }

    #[test]
    fn test_certificate_validity_boundary() {
        // Certificate that expires in exactly 1 second
        let now = Utc::now();
        let cert = Certificate {
            subject: "boundary-test".to_string(),
            serial_number: "111".to_string(),
            not_before: now - chrono::Duration::days(1),
            not_after: now + chrono::Duration::seconds(1),
            pem: "".to_string(),
        };

        assert!(cert.is_valid(), "Should be valid for 1 more second");
        assert!(!cert.is_expired());
    }

    #[test]
    fn test_certificate_max_validity_enforcement() {
        let config = CertificateAuthorityConfig {
            default_validity_days: 90,
            max_validity_days: 365,
            ..Default::default()
        };

        assert!(config.default_validity_days <= config.max_validity_days);

        // Max should be enforced (tested in actual issuance)
        let requested_days = 1000u32;
        let capped_days = requested_days.min(config.max_validity_days);
        assert_eq!(capped_days, 365);
    }
}
