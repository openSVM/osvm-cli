//! Comprehensive tests for cryptographic security utilities including
//! key management, encryption/decryption, signature verification, and secure random generation
//!
//! Note: These tests use an API that doesn't match the actual implementation.
//! The actual crypto_security module has KeyValidator and SecureKeyStorage.
//! See src/utils/crypto_security.rs for the actual implementation.
//!
//! These tests are ignored until they can be properly rewritten.

#[cfg(all(test, feature = "incomplete_tests"))]
mod key_management_tests {
    use anyhow::Result;
    use std::path::PathBuf;
    use tempfile::TempDir;

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_keypair_generation() -> Result<()> {
        let key_manager = KeyManager::new();

        let keypair = key_manager.generate_keypair()?;

        assert!(!keypair.public_key.is_empty());
        assert!(!keypair.private_key.is_empty());
        assert_ne!(keypair.public_key, keypair.private_key);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_keypair_save_and_load() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let key_path = temp_dir.path().join("test_key.json");

        let key_manager = KeyManager::new();
        let keypair = key_manager.generate_keypair()?;

        // Save keypair
        key_manager.save_keypair(&keypair, &key_path)?;

        // Verify file exists
        assert!(key_path.exists());

        // Load keypair
        let loaded_keypair = key_manager.load_keypair(&key_path)?;

        assert_eq!(keypair.public_key, loaded_keypair.public_key);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_keypair_from_seed() -> Result<()> {
        let key_manager = KeyManager::new();

        let seed = "test seed phrase for deterministic key generation";
        let keypair1 = key_manager.keypair_from_seed(seed)?;
        let keypair2 = key_manager.keypair_from_seed(seed)?;

        // Same seed should produce same keys
        assert_eq!(keypair1.public_key, keypair2.public_key);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_public_key_derivation() -> Result<()> {
        let key_manager = KeyManager::new();
        let keypair = key_manager.generate_keypair()?;

        let derived_pubkey = key_manager.derive_public_key(&keypair.private_key)?;

        assert_eq!(derived_pubkey, keypair.public_key);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_keypair_validation() -> Result<()> {
        let key_manager = KeyManager::new();
        let keypair = key_manager.generate_keypair()?;

        let is_valid = key_manager.validate_keypair(&keypair)?;
        assert!(is_valid);

        // Invalid keypair
        let invalid_keypair = KeyPair {
            public_key: vec![1, 2, 3],
            private_key: vec![4, 5, 6],
        };

        let is_invalid = key_manager.validate_keypair(&invalid_keypair)?;
        assert!(!is_invalid);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_key_rotation() -> Result<()> {
        let key_manager = KeyManager::new();
        let old_keypair = key_manager.generate_keypair()?;

        let new_keypair = key_manager.rotate_keypair(&old_keypair)?;

        assert_ne!(old_keypair.public_key, new_keypair.public_key);
        assert_ne!(old_keypair.private_key, new_keypair.private_key);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_key_derivation_path() -> Result<()> {
        let key_manager = KeyManager::new();
        let master_key = key_manager.generate_keypair()?;

        let derived1 = key_manager.derive_key(&master_key, "m/44'/501'/0'/0'")?;
        let derived2 = key_manager.derive_key(&master_key, "m/44'/501'/0'/1'")?;

        // Different paths should produce different keys
        assert_ne!(derived1.public_key, derived2.public_key);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_secure_key_storage() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let key_manager = KeyManager::with_secure_storage(temp_dir.path().to_path_buf());

        let keypair = key_manager.generate_keypair()?;
        let key_id = key_manager.store_key_securely(&keypair).await?;

        // Retrieve key
        let retrieved = key_manager.retrieve_key(&key_id).await?;
        assert_eq!(keypair.public_key, retrieved.public_key);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_key_permissions() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let key_path = temp_dir.path().join("secure_key.json");

        let key_manager = KeyManager::new();
        let keypair = key_manager.generate_keypair()?;

        key_manager.save_keypair(&keypair, &key_path)?;

        // Check permissions (should be 600 on Unix)
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let metadata = std::fs::metadata(&key_path)?;
            let permissions = metadata.permissions();
            assert_eq!(permissions.mode() & 0o777, 0o600);
        }

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_key_backup() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let key_manager = KeyManager::new();

        let keypair = key_manager.generate_keypair()?;
        let backup_path = temp_dir.path().join("backup");

        key_manager
            .backup_keys(&[keypair.clone()], &backup_path)
            .await?;

        // Verify backup exists
        assert!(backup_path.exists());

        // Restore from backup
        let restored = key_manager.restore_from_backup(&backup_path).await?;
        assert_eq!(restored.len(), 1);

        Ok(())
    }
}

#[cfg(all(test, feature = "incomplete_tests"))]
mod encryption_tests {
    use anyhow::Result;

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_symmetric_encryption() -> Result<()> {
        let encryption = EncryptionService::new();

        let plaintext = b"Hello, secure world!";
        let key = encryption.generate_symmetric_key()?;

        let encrypted = encryption.encrypt_symmetric(plaintext, &key)?;
        let decrypted = encryption.decrypt_symmetric(&encrypted, &key)?;

        assert_eq!(plaintext.to_vec(), decrypted);
        assert_ne!(plaintext.to_vec(), encrypted.ciphertext);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_asymmetric_encryption() -> Result<()> {
        let encryption = EncryptionService::new();
        let key_manager = KeyManager::new();

        let keypair = key_manager.generate_keypair()?;
        let plaintext = b"Asymmetric encryption test";

        let encrypted = encryption.encrypt_asymmetric(plaintext, &keypair.public_key)?;
        let decrypted = encryption.decrypt_asymmetric(&encrypted, &keypair.private_key)?;

        assert_eq!(plaintext.to_vec(), decrypted);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_encryption_with_aad() -> Result<()> {
        let encryption = EncryptionService::new();
        let key = encryption.generate_symmetric_key()?;

        let plaintext = b"Test data";
        let aad = b"Associated data";

        let encrypted = encryption.encrypt_with_aad(plaintext, &key, aad)?;
        let decrypted = encryption.decrypt_with_aad(&encrypted, &key, aad)?;

        assert_eq!(plaintext.to_vec(), decrypted);

        // Wrong AAD should fail
        let wrong_aad = b"Wrong associated data";
        let result = encryption.decrypt_with_aad(&encrypted, &key, wrong_aad);
        assert!(result.is_err());

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_encryption_nonce_uniqueness() -> Result<()> {
        let encryption = EncryptionService::new();
        let key = encryption.generate_symmetric_key()?;
        let plaintext = b"Test";

        let encrypted1 = encryption.encrypt_symmetric(plaintext, &key)?;
        let encrypted2 = encryption.encrypt_symmetric(plaintext, &key)?;

        // Nonces should be different
        assert_ne!(encrypted1.nonce, encrypted2.nonce);
        // But ciphertexts might differ even for same plaintext
        assert_ne!(encrypted1.ciphertext, encrypted2.ciphertext);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_stream_encryption() -> Result<()> {
        let encryption = EncryptionService::new();
        let key = encryption.generate_symmetric_key()?;

        let data = b"Large data that needs stream encryption".repeat(1000);

        let encrypted_stream = encryption.encrypt_stream(&data[..], &key)?;
        let decrypted_stream = encryption.decrypt_stream(&encrypted_stream[..], &key)?;

        assert_eq!(data, decrypted_stream);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_hybrid_encryption() -> Result<()> {
        let encryption = EncryptionService::new();
        let key_manager = KeyManager::new();

        let keypair = key_manager.generate_keypair()?;
        let large_data = b"Large data encrypted with hybrid approach".repeat(100);

        // Encrypt with hybrid (asymmetric key encryption + symmetric data encryption)
        let encrypted = encryption.hybrid_encrypt(&large_data, &keypair.public_key)?;
        let decrypted = encryption.hybrid_decrypt(&encrypted, &keypair.private_key)?;

        assert_eq!(large_data.to_vec(), decrypted);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_encryption_key_derivation() -> Result<()> {
        let encryption = EncryptionService::new();

        let password = "secure_password_123";
        let salt = encryption.generate_salt()?;

        let key1 = encryption.derive_key_from_password(password, &salt)?;
        let key2 = encryption.derive_key_from_password(password, &salt)?;

        // Same password and salt should produce same key
        assert_eq!(key1, key2);

        // Different salt should produce different key
        let different_salt = encryption.generate_salt()?;
        let key3 = encryption.derive_key_from_password(password, &different_salt)?;
        assert_ne!(key1, key3);

        Ok(())
    }
}

#[cfg(all(test, feature = "incomplete_tests"))]
mod signature_tests {
    use anyhow::Result;

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_sign_and_verify() -> Result<()> {
        let validator = SignatureValidator::new();
        let key_manager = KeyManager::new();

        let keypair = key_manager.generate_keypair()?;
        let message = b"Message to sign";

        let signature = validator.sign(message, &keypair.private_key)?;
        let is_valid = validator.verify(message, &signature, &keypair.public_key)?;

        assert!(is_valid);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_invalid_signature() -> Result<()> {
        let validator = SignatureValidator::new();
        let key_manager = KeyManager::new();

        let keypair = key_manager.generate_keypair()?;
        let message = b"Original message";

        let signature = validator.sign(message, &keypair.private_key)?;

        // Tampered message
        let tampered_message = b"Tampered message";
        let is_valid = validator.verify(tampered_message, &signature, &keypair.public_key)?;

        assert!(!is_valid);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_wrong_public_key() -> Result<()> {
        let validator = SignatureValidator::new();
        let key_manager = KeyManager::new();

        let keypair1 = key_manager.generate_keypair()?;
        let keypair2 = key_manager.generate_keypair()?;
        let message = b"Test message";

        let signature = validator.sign(message, &keypair1.private_key)?;
        let is_valid = validator.verify(message, &signature, &keypair2.public_key)?;

        assert!(!is_valid);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_batch_signature_verification() -> Result<()> {
        let validator = SignatureValidator::new();
        let key_manager = KeyManager::new();

        let keypair = key_manager.generate_keypair()?;
        let messages = vec![b"msg1".to_vec(), b"msg2".to_vec(), b"msg3".to_vec()];

        let signatures: Vec<_> = messages
            .iter()
            .map(|msg| validator.sign(msg, &keypair.private_key).unwrap())
            .collect();

        let results = validator.verify_batch(&messages, &signatures, &keypair.public_key)?;

        assert_eq!(results.len(), 3);
        assert!(results.iter().all(|&r| r));

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_signature_determinism() -> Result<()> {
        let validator = SignatureValidator::new();
        let key_manager = KeyManager::new();

        let keypair = key_manager.generate_keypair()?;
        let message = b"Deterministic test";

        let sig1 = validator.sign(message, &keypair.private_key)?;
        let sig2 = validator.sign(message, &keypair.private_key)?;

        // Signatures should be deterministic (for some schemes like Ed25519)
        assert_eq!(sig1.bytes, sig2.bytes);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_multisig_verification() -> Result<()> {
        let validator = SignatureValidator::new();
        let key_manager = KeyManager::new();

        let keypair1 = key_manager.generate_keypair()?;
        let keypair2 = key_manager.generate_keypair()?;
        let keypair3 = key_manager.generate_keypair()?;

        let message = b"Multisig message";

        let sig1 = validator.sign(message, &keypair1.private_key)?;
        let sig2 = validator.sign(message, &keypair2.private_key)?;
        let sig3 = validator.sign(message, &keypair3.private_key)?;

        let public_keys = vec![
            keypair1.public_key,
            keypair2.public_key,
            keypair3.public_key,
        ];
        let signatures = vec![sig1, sig2, sig3];

        let threshold = 2; // 2-of-3 multisig
        let is_valid = validator.verify_multisig(message, &signatures, &public_keys, threshold)?;

        assert!(is_valid);

        Ok(())
    }
}

#[cfg(all(test, feature = "incomplete_tests"))]
mod hashing_tests {
    use anyhow::Result;

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_sha256_hashing() -> Result<()> {
        let hasher = HashingService::new();

        let data = b"Data to hash";
        let hash1 = hasher.sha256(data)?;
        let hash2 = hasher.sha256(data)?;

        // Same input should produce same hash
        assert_eq!(hash1, hash2);
        assert_eq!(hash1.len(), 32); // SHA-256 is 32 bytes

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_blake3_hashing() -> Result<()> {
        let hasher = HashingService::new();

        let data = b"BLAKE3 test data";
        let hash = hasher.blake3(data)?;

        assert_eq!(hash.len(), 32); // BLAKE3 default is 32 bytes

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_hash_comparison() -> Result<()> {
        let hasher = HashingService::new();

        let data1 = b"data1";
        let data2 = b"data2";

        let hash1 = hasher.sha256(data1)?;
        let hash2 = hasher.sha256(data2)?;

        // Different data should produce different hashes
        assert_ne!(hash1, hash2);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_hmac() -> Result<()> {
        let hasher = HashingService::new();

        let key = b"secret_key";
        let message = b"Message to authenticate";

        let hmac1 = hasher.hmac_sha256(message, key)?;
        let hmac2 = hasher.hmac_sha256(message, key)?;

        // Same key and message should produce same HMAC
        assert_eq!(hmac1, hmac2);

        // Different key should produce different HMAC
        let different_key = b"different_key";
        let hmac3 = hasher.hmac_sha256(message, different_key)?;
        assert_ne!(hmac1, hmac3);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_password_hashing() -> Result<()> {
        let hasher = HashingService::new();

        let password = "user_password_123";
        let hashed = hasher.hash_password(password)?;

        // Verify password
        let is_valid = hasher.verify_password(password, &hashed)?;
        assert!(is_valid);

        // Wrong password should fail
        let wrong_password = "wrong_password";
        let is_invalid = hasher.verify_password(wrong_password, &hashed)?;
        assert!(!is_invalid);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_hash_chain() -> Result<()> {
        let hasher = HashingService::new();

        let initial_data = b"genesis block";
        let iterations = 10;

        let chain = hasher.create_hash_chain(initial_data, iterations)?;

        assert_eq!(chain.len(), iterations);

        // Verify chain integrity
        let is_valid = hasher.verify_hash_chain(&chain)?;
        assert!(is_valid);

        Ok(())
    }
}

#[cfg(all(test, feature = "incomplete_tests"))]
mod secure_random_tests {
    use anyhow::Result;

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_random_bytes_generation() -> Result<()> {
        let rng = SecureRandom::new();

        let bytes1 = rng.generate_bytes(32)?;
        let bytes2 = rng.generate_bytes(32)?;

        assert_eq!(bytes1.len(), 32);
        assert_eq!(bytes2.len(), 32);
        // Should be different
        assert_ne!(bytes1, bytes2);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_random_number_generation() -> Result<()> {
        let rng = SecureRandom::new();

        let num1 = rng.generate_u64()?;
        let num2 = rng.generate_u64()?;

        // Should be different (with very high probability)
        assert_ne!(num1, num2);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_random_range() -> Result<()> {
        let rng = SecureRandom::new();

        for _ in 0..100 {
            let num = rng.generate_range(1, 100)?;
            assert!(num >= 1 && num < 100);
        }

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_random_string_generation() -> Result<()> {
        let rng = SecureRandom::new();

        let string = rng.generate_alphanumeric_string(16)?;

        assert_eq!(string.len(), 16);
        assert!(string.chars().all(|c| c.is_alphanumeric()));

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_uuid_generation() -> Result<()> {
        let rng = SecureRandom::new();

        let uuid1 = rng.generate_uuid()?;
        let uuid2 = rng.generate_uuid()?;

        assert_eq!(uuid1.len(), 36); // UUID format: 8-4-4-4-12
        assert_ne!(uuid1, uuid2);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - crypto_security has different API"]
    fn test_cryptographic_nonce() -> Result<()> {
        let rng = SecureRandom::new();

        let nonce1 = rng.generate_nonce(12)?;
        let nonce2 = rng.generate_nonce(12)?;

        assert_eq!(nonce1.len(), 12);
        assert_ne!(nonce1, nonce2);

        Ok(())
    }
}

#[cfg(all(test, feature = "incomplete_tests"))]
mod integration_tests {
    use anyhow::Result;
    use tempfile::TempDir;

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_end_to_end_secure_communication() -> Result<()> {
        let key_manager = KeyManager::new();
        let encryption = EncryptionService::new();
        let validator = SignatureValidator::new();

        // Generate keypairs for Alice and Bob
        let alice_keypair = key_manager.generate_keypair()?;
        let bob_keypair = key_manager.generate_keypair()?;

        let message = b"Secure message from Alice to Bob";

        // Alice signs and encrypts
        let signature = validator.sign(message, &alice_keypair.private_key)?;
        let encrypted = encryption.encrypt_asymmetric(message, &bob_keypair.public_key)?;

        // Bob decrypts and verifies
        let decrypted = encryption.decrypt_asymmetric(&encrypted, &bob_keypair.private_key)?;
        let is_valid = validator.verify(&decrypted, &signature, &alice_keypair.public_key)?;

        assert_eq!(message.to_vec(), decrypted);
        assert!(is_valid);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_secure_key_exchange() -> Result<()> {
        let key_manager = KeyManager::new();

        let alice_keypair = key_manager.generate_keypair()?;
        let bob_keypair = key_manager.generate_keypair()?;

        // Derive shared secret
        let alice_shared =
            key_manager.ecdh_key_exchange(&alice_keypair.private_key, &bob_keypair.public_key)?;

        let bob_shared =
            key_manager.ecdh_key_exchange(&bob_keypair.private_key, &alice_keypair.public_key)?;

        // Both should derive the same shared secret
        assert_eq!(alice_shared, bob_shared);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - crypto_security has different API"]
    async fn test_secure_file_encryption() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let file_path = temp_dir.path().join("sensitive_data.txt");
        let encrypted_path = temp_dir.path().join("sensitive_data.enc");

        let encryption = EncryptionService::new();
        let key = encryption.generate_symmetric_key()?;

        // Write and encrypt file
        let data = b"Sensitive information that needs protection";
        std::fs::write(&file_path, data)?;

        encryption.encrypt_file(&file_path, &encrypted_path, &key)?;

        // Decrypt and verify
        let decrypted_path = temp_dir.path().join("decrypted.txt");
        encryption.decrypt_file(&encrypted_path, &decrypted_path, &key)?;

        let decrypted_data = std::fs::read(&decrypted_path)?;
        assert_eq!(data.to_vec(), decrypted_data);

        Ok(())
    }
}
