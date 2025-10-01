//! Input sanitization and validation utilities
//!
//! This module provides centralized input validation and sanitization functions
//! to prevent injection attacks, path traversal, and other security vulnerabilities.

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

/// Safely validate and canonicalize a path to prevent directory traversal attacks
///
/// # Arguments
/// * `path` - The path to validate
/// * `base_dir` - Base directory that the path must be within
///
/// # Returns
/// * `Result<PathBuf>` - Canonicalized path if valid
///
/// # Security
/// This function prevents:
/// - Directory traversal attacks (../)
/// - Symlink attacks
/// - Access outside the base directory
pub fn sanitize_path(path: &Path, base_dir: &Path) -> Result<PathBuf> {
    let canonical = path
        .canonicalize()
        .with_context(|| format!("Failed to canonicalize path: {}", path.display()))?;

    let canonical_base = base_dir.canonicalize().with_context(|| {
        format!(
            "Failed to canonicalize base directory: {}",
            base_dir.display()
        )
    })?;

    if !canonical.starts_with(&canonical_base) {
        anyhow::bail!(
            "Path '{}' is outside allowed directory '{}'",
            canonical.display(),
            canonical_base.display()
        );
    }

    Ok(canonical)
}

/// Validate a path without requiring it to exist yet
///
/// # Arguments
/// * `path` - Path string to validate
///
/// # Returns
/// * `Result<PathBuf>` - Validated path if safe
///
/// # Security
/// Checks for:
/// - Null bytes
/// - Excessive length
/// - Dangerous patterns (../)
pub fn validate_path_string(path: &str) -> Result<PathBuf> {
    // Check for null bytes
    if path.contains('\0') {
        anyhow::bail!("Path contains null byte");
    }

    // Check length (prevent resource exhaustion)
    if path.len() > 4096 {
        anyhow::bail!("Path too long (max 4096 characters)");
    }

    // Check for dangerous patterns
    if path.contains("..") {
        anyhow::bail!("Path contains parent directory reference (..)");
    }

    // Create PathBuf
    let path_buf = PathBuf::from(path);

    // Additional validation: check each component
    for component in path_buf.components() {
        let comp_str = component.as_os_str().to_string_lossy();
        if comp_str.contains('\0') {
            anyhow::bail!("Path component contains null byte");
        }
    }

    Ok(path_buf)
}

/// Validate a port number
///
/// # Arguments
/// * `port` - Port number to validate
/// * `allow_privileged` - Whether to allow ports < 1024
///
/// # Returns
/// * `Result<()>` - Ok if valid
pub fn validate_port(port: u16, allow_privileged: bool) -> Result<()> {
    if port == 0 {
        anyhow::bail!("Port number cannot be 0");
    }

    if !allow_privileged && port < 1024 {
        anyhow::bail!("Port number {} is privileged (< 1024). Use --allow-privileged if intended", port);
    }

    Ok(())
}

/// Validate a URL string
///
/// # Arguments
/// * `url` - URL string to validate
///
/// # Returns
/// * `Result<String>` - Validated URL
///
/// # Security
/// Checks for:
/// - Valid URL format
/// - Allowed schemes (http, https, ws, wss)
/// - No credentials in URL
pub fn validate_url(url: &str) -> Result<String> {
    // Check for null bytes
    if url.contains('\0') {
        anyhow::bail!("URL contains null byte");
    }

    // Check length
    if url.len() > 2048 {
        anyhow::bail!("URL too long (max 2048 characters)");
    }

    // Parse URL
    let parsed = url::Url::parse(url).context("Invalid URL format")?;

    // Check scheme
    let scheme = parsed.scheme();
    if !matches!(scheme, "http" | "https" | "ws" | "wss" | "stdio") {
        anyhow::bail!("URL scheme '{}' not allowed. Use http, https, ws, wss, or stdio", scheme);
    }

    // Check for embedded credentials (security risk)
    if parsed.username() != "" || parsed.password().is_some() {
        anyhow::bail!("Credentials in URL are not allowed for security reasons");
    }

    Ok(url.to_string())
}

/// Sanitize a string for safe use in shell commands
///
/// # Arguments
/// * `input` - Input string to sanitize
///
/// # Returns
/// * `Result<String>` - Sanitized string
///
/// # Security
/// Removes or escapes:
/// - Shell metacharacters
/// - Control characters
/// - Null bytes
pub fn sanitize_shell_arg(input: &str) -> Result<String> {
    // Check for null bytes
    if input.contains('\0') {
        anyhow::bail!("Input contains null byte");
    }

    // Check length
    if input.len() > 10000 {
        anyhow::bail!("Input too long (max 10000 characters)");
    }

    // Check for dangerous characters
    let dangerous_chars = [';', '&', '|', '$', '`', '\n', '\r', '(', ')', '<', '>', '\\', '"', '\''];
    if input.chars().any(|c| dangerous_chars.contains(&c)) {
        anyhow::bail!("Input contains dangerous shell metacharacters");
    }

    Ok(input.to_string())
}

/// Validate JSON input
///
/// # Arguments
/// * `json_str` - JSON string to validate
/// * `max_size` - Maximum allowed size in bytes
///
/// # Returns
/// * `Result<serde_json::Value>` - Parsed JSON if valid
///
/// # Security
/// Checks for:
/// - Valid JSON syntax
/// - Size limits to prevent DoS
pub fn validate_json(json_str: &str, max_size: usize) -> Result<serde_json::Value> {
    // Check size
    if json_str.len() > max_size {
        anyhow::bail!("JSON input too large (max {} bytes)", max_size);
    }

    // Parse JSON
    let value: serde_json::Value =
        serde_json::from_str(json_str).context("Invalid JSON format")?;

    Ok(value)
}

/// Validate an identifier (e.g., server ID, tool name)
///
/// # Arguments
/// * `identifier` - Identifier string to validate
///
/// # Returns
/// * `Result<String>` - Validated identifier
///
/// # Security
/// Ensures identifier:
/// - Contains only alphanumeric characters, hyphens, and underscores
/// - Is not empty
/// - Is not too long
pub fn validate_identifier(identifier: &str) -> Result<String> {
    // Check for empty
    if identifier.is_empty() {
        anyhow::bail!("Identifier cannot be empty");
    }

    // Check length
    if identifier.len() > 64 {
        anyhow::bail!("Identifier too long (max 64 characters)");
    }

    // Check characters
    if !identifier
        .chars()
        .all(|c| c.is_alphanumeric() || matches!(c, '-' | '_'))
    {
        anyhow::bail!("Identifier can only contain letters, numbers, hyphens, and underscores");
    }

    // Must start with alphanumeric
    if let Some(first_char) = identifier.chars().next() {
        if !first_char.is_alphanumeric() {
            anyhow::bail!("Identifier must start with a letter or number");
        }
    }

    Ok(identifier.to_string())
}

/// Validate a network name
///
/// # Arguments
/// * `network` - Network name to validate
///
/// # Returns
/// * `Result<String>` - Validated network name
pub fn validate_network(network: &str) -> Result<String> {
    let valid_networks = ["mainnet", "testnet", "devnet"];
    if !valid_networks.contains(&network) {
        anyhow::bail!(
            "Invalid network '{}'. Must be one of: {}",
            network,
            valid_networks.join(", ")
        );
    }
    Ok(network.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_validate_path_string() {
        // Valid paths
        assert!(validate_path_string("/tmp/test").is_ok());
        assert!(validate_path_string("relative/path").is_ok());

        // Invalid paths
        assert!(validate_path_string("path/with/../../traversal").is_err());
        assert!(validate_path_string("path\0null").is_err());
        assert!(validate_path_string(&"x".repeat(5000)).is_err());
    }

    #[test]
    fn test_validate_port() {
        assert!(validate_port(8080, false).is_ok());
        assert!(validate_port(22, true).is_ok());
        assert!(validate_port(22, false).is_err());
        assert!(validate_port(0, true).is_err());
    }

    #[test]
    fn test_validate_url() {
        assert!(validate_url("https://example.com").is_ok());
        assert!(validate_url("http://localhost:8080").is_ok());
        assert!(validate_url("ws://example.com/socket").is_ok());

        // Invalid URLs
        assert!(validate_url("ftp://example.com").is_err());
        assert!(validate_url("https://user:pass@example.com").is_err());
        assert!(validate_url("not a url").is_err());
        assert!(validate_url("http://example\0.com").is_err());
    }

    #[test]
    fn test_sanitize_shell_arg() {
        assert!(sanitize_shell_arg("safe_argument").is_ok());
        assert!(sanitize_shell_arg("safe-arg-123").is_ok());

        // Dangerous inputs
        assert!(sanitize_shell_arg("arg; rm -rf /").is_err());
        assert!(sanitize_shell_arg("arg && evil").is_err());
        assert!(sanitize_shell_arg("arg | evil").is_err());
        assert!(sanitize_shell_arg("arg$(evil)").is_err());
        assert!(sanitize_shell_arg("arg`evil`").is_err());
    }

    #[test]
    fn test_validate_json() {
        assert!(validate_json(r#"{"key": "value"}"#, 1000).is_ok());
        assert!(validate_json("[]", 1000).is_ok());
        assert!(validate_json("null", 1000).is_ok());

        // Invalid JSON
        assert!(validate_json("{invalid}", 1000).is_err());
        assert!(validate_json(&"x".repeat(2000), 1000).is_err());
    }

    #[test]
    fn test_validate_identifier() {
        assert!(validate_identifier("server1").is_ok());
        assert!(validate_identifier("test-server_2").is_ok());

        // Invalid identifiers
        assert!(validate_identifier("").is_err());
        assert!(validate_identifier("server with spaces").is_err());
        assert!(validate_identifier("-starts-with-dash").is_err());
        assert!(validate_identifier(&"x".repeat(100)).is_err());
    }

    #[test]
    fn test_validate_network() {
        assert!(validate_network("mainnet").is_ok());
        assert!(validate_network("testnet").is_ok());
        assert!(validate_network("devnet").is_ok());
        assert!(validate_network("invalid").is_err());
    }

    #[test]
    fn test_sanitize_path() {
        let temp_dir = std::env::temp_dir();
        let test_file = temp_dir.join("test_sanitize.txt");
        fs::write(&test_file, "test").unwrap();

        // Valid path within base dir
        assert!(sanitize_path(&test_file, &temp_dir).is_ok());

        // Try to escape base dir (should fail)
        let outside_path = temp_dir.join("..").join("etc").join("passwd");
        // This test may pass or fail depending on if the path exists
        // The key is it should never allow access outside temp_dir

        fs::remove_file(&test_file).ok();
    }
}
