//! Cryptographic security utilities for key validation and secure storage
//!
//! This module provides utilities for validating cryptographic keys, secure
//! key storage, and cryptographic operations with proper security practices.

use anyhow::{anyhow, Context, Result};
use solana_sdk::signature::{Keypair, Signature};
use solana_sdk::signer::Signer;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;

/// Minimum entropy requirements for key generation
const MIN_ENTROPY_BITS: usize = 256;

/// Secure key validator
pub struct KeyValidator {
    strict_mode: bool,
}

impl KeyValidator {
    /// Create a new key validator
    pub fn new(strict_mode: bool) -> Self {
        Self { strict_mode }
    }

    /// Validate a keypair file for security issues
    pub fn validate_keypair_file(&self, path: &Path) -> Result<()> {
        // Check file existence
        if !path.exists() {
            return Err(anyhow!("Keypair file does not exist: {}", path.display()));
        }

        // Check file permissions (Unix-like systems)
        #[cfg(unix)]
        {
            let metadata = fs::metadata(path)
                .with_context(|| format!("Failed to read metadata for {}", path.display()))?;

            let permissions = metadata.permissions();
            let mode = permissions.mode();

            // Check if file is readable by others (security risk)
            if mode & 0o044 != 0 {
                if self.strict_mode {
                    return Err(anyhow!(
                        "Keypair file has unsafe permissions (readable by others): {}. Use 'chmod 600 {}'",
                        path.display(),
                        path.display()
                    ));
                } else {
                    eprintln!(
                        "⚠️  Warning: Keypair file has unsafe permissions: {}",
                        path.display()
                    );
                }
            }
        }

        // Validate file content
        let content = fs::read_to_string(path)
            .with_context(|| format!("Failed to read keypair file: {}", path.display()))?;

        self.validate_keypair_content(&content)?;

        Ok(())
    }

    /// Validate keypair content for security issues
    pub fn validate_keypair_content(&self, content: &str) -> Result<()> {
        // Parse as JSON array (Solana format)
        let key_data: Vec<u8> = serde_json::from_str(content.trim())
            .context("Invalid keypair format (not a valid JSON array)")?;

        // Validate key length
        if key_data.len() != 64 {
            return Err(anyhow!(
                "Invalid keypair length: expected 64 bytes, got {}",
                key_data.len()
            ));
        }

        // Try to create a keypair to validate format
        if key_data.len() < 32 {
            return Err(anyhow!("Key data too short for keypair"));
        }
        let keypair = Keypair::new_from_array(
            key_data[0..32]
                .try_into()
                .context("Failed to extract private key bytes")?,
        );

        // Validate entropy (basic check)
        self.validate_key_entropy(&key_data)?;

        // Validate key is not from a known weak source
        self.check_known_weak_keys(&keypair)?;

        Ok(())
    }

    /// Check for adequate entropy in the key
    fn validate_key_entropy(&self, key_data: &[u8]) -> Result<()> {
        // Simple entropy check: count unique bytes
        let mut byte_counts = [0u32; 256];
        for &byte in key_data {
            byte_counts[byte as usize] += 1;
        }

        let unique_bytes = byte_counts.iter().filter(|&&count| count > 0).count();

        // Require at least 50% unique bytes for basic entropy
        if unique_bytes < key_data.len() / 2 {
            if self.strict_mode {
                return Err(anyhow!(
                    "Key appears to have low entropy (only {} unique bytes)",
                    unique_bytes
                ));
            } else {
                eprintln!("⚠️  Warning: Key may have low entropy");
            }
        }

        // Check for obvious patterns (all zeros, all ones, etc.)
        if key_data.iter().all(|&b| b == key_data[0]) {
            return Err(anyhow!(
                "Key contains repetitive pattern (potential security risk)"
            ));
        }

        Ok(())
    }

    /// Check against known weak keys (test keys, dev keys, etc.)
    fn check_known_weak_keys(&self, keypair: &Keypair) -> Result<()> {
        let pubkey = keypair.pubkey();

        // List of known test/dev keys that should never be used in production
        let known_weak_keys = [
            // Common Solana test keys
            "11111111111111111111111111111111",
            "So11111111111111111111111111111111111111112",
            // Add more known weak keys here
        ];

        let pubkey_str = pubkey.to_string();
        for weak_key in &known_weak_keys {
            if pubkey_str == *weak_key {
                return Err(anyhow!("Keypair matches known weak/test key: {}", weak_key));
            }
        }

        Ok(())
    }

    /// Secure a keypair file by setting proper permissions
    pub fn secure_keypair_file(&self, path: &Path) -> Result<()> {
        #[cfg(unix)]
        {
            let mut permissions = fs::metadata(path)?.permissions();
            permissions.set_mode(0o600); // rw-------
            fs::set_permissions(path, permissions).with_context(|| {
                format!("Failed to set secure permissions on {}", path.display())
            })?;
        }

        Ok(())
    }
}

/// Secure key storage utilities
pub struct SecureKeyStorage {
    base_path: std::path::PathBuf,
}

impl SecureKeyStorage {
    /// Create a new secure key storage
    pub fn new(base_path: std::path::PathBuf) -> Result<Self> {
        // Ensure directory exists with secure permissions
        if !base_path.exists() {
            fs::create_dir_all(&base_path).with_context(|| {
                format!(
                    "Failed to create key storage directory: {}",
                    base_path.display()
                )
            })?;
        }

        // Set secure permissions on directory
        #[cfg(unix)]
        {
            let mut permissions = fs::metadata(&base_path)?.permissions();
            permissions.set_mode(0o700); // rwx------
            fs::set_permissions(&base_path, permissions).with_context(|| {
                format!(
                    "Failed to set secure permissions on directory: {}",
                    base_path.display()
                )
            })?;
        }

        Ok(Self { base_path })
    }

    /// Store a keypair securely
    pub fn store_keypair(&self, name: &str, keypair: &Keypair) -> Result<std::path::PathBuf> {
        // Validate name to prevent path traversal
        if name.contains("..") || name.contains('/') || name.contains('\\') {
            return Err(anyhow!("Invalid keypair name: {}", name));
        }

        let file_path = self.base_path.join(format!("{}.json", name));

        // Serialize keypair
        let key_data = keypair.to_bytes();
        let key_json = serde_json::to_string_pretty(&key_data.to_vec())?;

        // Write with secure permissions
        fs::write(&file_path, key_json)
            .with_context(|| format!("Failed to write keypair to {}", file_path.display()))?;

        // Set secure file permissions
        #[cfg(unix)]
        {
            let mut permissions = fs::metadata(&file_path)?.permissions();
            permissions.set_mode(0o600); // rw-------
            fs::set_permissions(&file_path, permissions).with_context(|| {
                format!(
                    "Failed to set secure permissions on {}",
                    file_path.display()
                )
            })?;
        }

        Ok(file_path)
    }

    /// Load a keypair securely with validation
    pub fn load_keypair(&self, name: &str) -> Result<Keypair> {
        let file_path = self.base_path.join(format!("{}.json", name));

        let validator = KeyValidator::new(true);
        validator.validate_keypair_file(&file_path)?;

        let content = fs::read_to_string(&file_path)
            .with_context(|| format!("Failed to read keypair from {}", file_path.display()))?;

        let key_data: Vec<u8> = serde_json::from_str(&content).context("Invalid keypair format")?;

        if key_data.len() < 32 {
            return Err(anyhow!("Key data too short for keypair"));
        }

        Ok(Keypair::new_from_array(
            key_data[0..32]
                .try_into()
                .context("Failed to extract private key bytes")?,
        ))
    }

    /// List available keypairs
    pub fn list_keypairs(&self) -> Result<Vec<String>> {
        let mut keypairs = Vec::new();

        for entry in fs::read_dir(&self.base_path)? {
            let entry = entry?;
            let path = entry.path();

            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    keypairs.push(stem.to_string());
                }
            }
        }

        Ok(keypairs)
    }

    /// Delete a keypair securely (overwrite before deletion)
    pub fn delete_keypair(&self, name: &str) -> Result<()> {
        let file_path = self.base_path.join(format!("{}.json", name));

        if !file_path.exists() {
            return Err(anyhow!("Keypair does not exist: {}", name));
        }

        // Securely overwrite file content before deletion
        let file_size = fs::metadata(&file_path)?.len();
        let overwrite_data = vec![0u8; file_size as usize];
        fs::write(&file_path, &overwrite_data)?;

        // Delete the file
        fs::remove_file(&file_path)
            .with_context(|| format!("Failed to delete keypair: {}", name))?;

        Ok(())
    }
}

/// Generate a cryptographically secure keypair
pub fn generate_secure_keypair() -> Result<Keypair> {
    // Use OS random number generator for secure key generation
    let keypair = Keypair::new();

    // Validate the generated key
    let validator = KeyValidator::new(true);
    let key_data = keypair.to_bytes();
    validator.validate_key_entropy(&key_data)?;
    validator.check_known_weak_keys(&keypair)?;

    Ok(keypair)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_key_validator_entropy() {
        let validator = KeyValidator::new(true);

        // Test low entropy key (all zeros)
        let weak_key = vec![0u8; 64];
        assert!(validator.validate_key_entropy(&weak_key).is_err());

        // Test good entropy key
        let good_key = generate_secure_keypair().unwrap().to_bytes();
        assert!(validator.validate_key_entropy(&good_key).is_ok());
    }

    #[test]
    fn test_secure_key_storage() {
        let temp_dir = TempDir::new().unwrap();
        let storage = SecureKeyStorage::new(temp_dir.path().to_path_buf()).unwrap();

        let keypair = generate_secure_keypair().unwrap();
        let original_pubkey = keypair.pubkey();

        // Store keypair
        let stored_path = storage.store_keypair("test_key", &keypair).unwrap();
        assert!(stored_path.exists());

        // Load keypair
        let loaded_keypair = storage.load_keypair("test_key").unwrap();
        assert_eq!(original_pubkey, loaded_keypair.pubkey());

        // List keypairs
        let keypairs = storage.list_keypairs().unwrap();
        assert!(keypairs.contains(&"test_key".to_string()));

        // Delete keypair
        storage.delete_keypair("test_key").unwrap();
        assert!(!stored_path.exists());
    }

    #[test]
    fn test_generate_secure_keypair() {
        let keypair = generate_secure_keypair().unwrap();

        // Verify it's a valid keypair
        let message = b"test message";
        let signature = keypair.sign_message(message);

        // Verify signature
        assert!(signature.verify(&keypair.pubkey().to_bytes(), message));
    }
}
