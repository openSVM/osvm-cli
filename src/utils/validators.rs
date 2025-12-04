//! Input validators for CLI arguments
//!
//! This module replaces solana-clap-utils input validators to eliminate
//! the clap 2.x dependency chain (ansi_term, atty) which has security warnings.
//!
//! Implemented based on: https://github.com/solana-labs/solana/blob/master/clap-utils/src/input_validators.rs

use std::path::Path;
use url::Url;

/// Converts network monikers to their full RPC URLs.
///
/// Supported monikers:
/// - `m` or `mainnet-beta` → mainnet
/// - `t` or `testnet` → testnet
/// - `d` or `devnet` → devnet
/// - `l` or `localhost` → localhost:8899
///
/// Any other input is returned unchanged.
pub fn normalize_to_url_if_moniker(url_or_moniker: &str) -> String {
    match url_or_moniker.to_lowercase().as_str() {
        "m" | "mainnet-beta" | "mainnet" => "https://api.mainnet-beta.solana.com".to_string(),
        "t" | "testnet" => "https://api.testnet.solana.com".to_string(),
        "d" | "devnet" => "https://api.devnet.solana.com".to_string(),
        "l" | "localhost" | "localnet" => "http://localhost:8899".to_string(),
        _ => url_or_moniker.to_string(),
    }
}

/// Validates that a string is either a valid URL or a recognized network moniker.
///
/// Returns `Ok(())` if valid, `Err(String)` with an error message otherwise.
pub fn is_url_or_moniker(s: &str) -> Result<(), String> {
    let normalized = normalize_to_url_if_moniker(s);

    let url = Url::parse(&normalized).map_err(|e| format!("Invalid URL '{}': {}", s, e))?;

    // Ensure the URL has a host
    url.host()
        .ok_or_else(|| format!("URL '{}' has no host", s))?;

    Ok(())
}

/// Validates that a string represents a valid signer source.
///
/// Accepts:
/// - Base58-encoded pubkeys (32-44 characters, alphanumeric)
/// - File paths to keypair files
/// - Signer URIs (usb://, prompt://, etc.) - basic validation only
pub fn is_valid_signer(s: &str) -> Result<(), String> {
    // Check for signer URI schemes
    if s.starts_with("usb://") || s.starts_with("prompt://") || s.starts_with("file://") {
        return Ok(());
    }

    // Check if it looks like a valid base58 pubkey
    // Solana pubkeys are 32 bytes, base58 encoded = 32-44 characters
    if is_valid_pubkey_format(s) {
        return Ok(());
    }

    // Check if it's a file path that exists
    let path = Path::new(s);
    if path.exists() {
        return Ok(());
    }

    // Check if it could be a path that will be created
    // (parent directory exists)
    if let Some(parent) = path.parent() {
        if parent.exists() || parent.as_os_str().is_empty() {
            return Ok(());
        }
    }

    Err(format!(
        "Invalid signer '{}': not a valid pubkey, existing file, or signer URI",
        s
    ))
}

/// Checks if a string has valid base58 pubkey format.
/// Does NOT verify the actual cryptographic validity.
fn is_valid_pubkey_format(s: &str) -> bool {
    // Base58 alphabet (Bitcoin variant used by Solana)
    const BASE58_CHARS: &str = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

    // Length check: 32 bytes base58 encoded = 32-44 chars
    if s.len() < 32 || s.len() > 44 {
        return false;
    }

    // Character check
    s.chars().all(|c| BASE58_CHARS.contains(c))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_mainnet() {
        assert_eq!(
            normalize_to_url_if_moniker("m"),
            "https://api.mainnet-beta.solana.com"
        );
        assert_eq!(
            normalize_to_url_if_moniker("mainnet-beta"),
            "https://api.mainnet-beta.solana.com"
        );
        assert_eq!(
            normalize_to_url_if_moniker("MAINNET"),
            "https://api.mainnet-beta.solana.com"
        );
    }

    #[test]
    fn test_normalize_devnet() {
        assert_eq!(
            normalize_to_url_if_moniker("d"),
            "https://api.devnet.solana.com"
        );
        assert_eq!(
            normalize_to_url_if_moniker("devnet"),
            "https://api.devnet.solana.com"
        );
    }

    #[test]
    fn test_normalize_testnet() {
        assert_eq!(
            normalize_to_url_if_moniker("t"),
            "https://api.testnet.solana.com"
        );
        assert_eq!(
            normalize_to_url_if_moniker("testnet"),
            "https://api.testnet.solana.com"
        );
    }

    #[test]
    fn test_normalize_localhost() {
        assert_eq!(normalize_to_url_if_moniker("l"), "http://localhost:8899");
        assert_eq!(
            normalize_to_url_if_moniker("localhost"),
            "http://localhost:8899"
        );
    }

    #[test]
    fn test_normalize_passthrough() {
        let custom = "https://my-rpc.example.com";
        assert_eq!(normalize_to_url_if_moniker(custom), custom);

        let with_port = "http://192.168.1.100:8899";
        assert_eq!(normalize_to_url_if_moniker(with_port), with_port);
    }

    #[test]
    fn test_is_url_or_moniker_valid() {
        assert!(is_url_or_moniker("m").is_ok());
        assert!(is_url_or_moniker("devnet").is_ok());
        assert!(is_url_or_moniker("https://api.mainnet-beta.solana.com").is_ok());
        assert!(is_url_or_moniker("http://localhost:8899").is_ok());
    }

    #[test]
    fn test_is_url_or_moniker_invalid() {
        assert!(is_url_or_moniker("not-a-url").is_err());
        assert!(is_url_or_moniker("").is_err());
    }

    #[test]
    fn test_is_valid_pubkey_format() {
        // Valid pubkey format (not checking actual crypto validity)
        assert!(is_valid_pubkey_format("11111111111111111111111111111111"));
        assert!(is_valid_pubkey_format(
            "So11111111111111111111111111111111111111112"
        ));

        // Invalid: too short
        assert!(!is_valid_pubkey_format("short"));

        // Invalid: contains invalid characters (0, O, I, l not in base58)
        assert!(!is_valid_pubkey_format("0OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"));
    }

    #[test]
    fn test_is_valid_signer() {
        // URI schemes
        assert!(is_valid_signer("usb://ledger").is_ok());
        assert!(is_valid_signer("prompt://").is_ok());

        // Pubkey format
        assert!(is_valid_signer("11111111111111111111111111111111").is_ok());

        // Invalid
        assert!(is_valid_signer("").is_err());
    }
}
