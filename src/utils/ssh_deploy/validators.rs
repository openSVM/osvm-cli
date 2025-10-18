//! Input validation utilities for SSH deployment operations

use {
    crate::utils::ssh_deploy::{errors::DeploymentError, types::DeploymentConfig},
    shell_escape::unix,
    std::collections::HashMap,
};

/// Validate a remote file/directory path
///
/// # Arguments
/// * `path` - Path to validate
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn validate_remote_path(path: &str) -> Result<(), DeploymentError> {
    if path.is_empty() {
        return Err(DeploymentError::ValidationError(
            "Path cannot be empty".to_string(),
        ));
    }

    // Check for directory traversal attempts
    if path.contains("..") {
        return Err(DeploymentError::ValidationError(
            "Path contains directory traversal (..) - not allowed".to_string(),
        ));
    }

    // Check for null bytes (could break commands)
    if path.contains('\0') {
        return Err(DeploymentError::ValidationError(
            "Path contains null bytes - not allowed".to_string(),
        ));
    }

    // Check for newlines (could break multi-line commands)
    if path.contains('\n') || path.contains('\r') {
        return Err(DeploymentError::ValidationError(
            "Path contains newline characters - not allowed".to_string(),
        ));
    }

    // Check for excessive length (prevent buffer issues)
    if path.len() > 4096 {
        return Err(DeploymentError::ValidationError(
            "Path is too long (max 4096 chars)".to_string(),
        ));
    }

    Ok(())
}

/// Validate a service name
///
/// Systemd service names should follow: [a-zA-Z0-9:_\-.]+
///
/// # Arguments
/// * `name` - Service name to validate
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn validate_service_name(name: &str) -> Result<(), DeploymentError> {
    if name.is_empty() {
        return Err(DeploymentError::ValidationError(
            "Service name cannot be empty".to_string(),
        ));
    }

    if name.len() > 256 {
        return Err(DeploymentError::ValidationError(
            "Service name is too long (max 256 chars)".to_string(),
        ));
    }

    // Validate allowed characters: alphanumeric, colon, underscore, hyphen, dot
    // This is the standard systemd service name format
    for c in name.chars() {
        if !c.is_alphanumeric() && !matches!(c, ':' | '_' | '-' | '.') {
            return Err(DeploymentError::ValidationError(format!(
                "Service name contains invalid character '{}'. Allowed: [a-zA-Z0-9:_-.]",
                c
            )));
        }
    }

    Ok(())
}

/// Validate a package name (Debian/RPM package naming conventions)
///
/// # Arguments
/// * `name` - Package name to validate
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn validate_package_name(name: &str) -> Result<(), DeploymentError> {
    if name.is_empty() {
        return Err(DeploymentError::ValidationError(
            "Package name cannot be empty".to_string(),
        ));
    }

    if name.len() > 128 {
        return Err(DeploymentError::ValidationError(
            "Package name is too long (max 128 chars)".to_string(),
        ));
    }

    // Package names can contain: letters, numbers, hyphens, underscores, dots
    // but must start with letter or digit
    if !name.chars().next().unwrap().is_alphanumeric() {
        return Err(DeploymentError::ValidationError(
            "Package name must start with a letter or digit".to_string(),
        ));
    }

    for c in name.chars() {
        if !c.is_alphanumeric() && !matches!(c, '-' | '_' | '.') {
            return Err(DeploymentError::ValidationError(format!(
                "Package name contains invalid character '{}'. Allowed: [a-zA-Z0-9.-_]",
                c
            )));
        }
    }

    Ok(())
}

/// Validate a hostname/IP address
///
/// # Arguments
/// * `host` - Hostname or IP address to validate
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn validate_hostname(host: &str) -> Result<(), DeploymentError> {
    if host.is_empty() {
        return Err(DeploymentError::ValidationError(
            "Hostname cannot be empty".to_string(),
        ));
    }

    if host.len() > 253 {
        return Err(DeploymentError::ValidationError(
            "Hostname is too long (max 253 chars)".to_string(),
        ));
    }

    // Check for null bytes
    if host.contains('\0') {
        return Err(DeploymentError::ValidationError(
            "Hostname contains null bytes - not allowed".to_string(),
        ));
    }

    Ok(())
}

/// Validate a port number
///
/// # Arguments
/// * `port` - Port number to validate
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn validate_port(port: u16) -> Result<(), DeploymentError> {
    // Port 0 is reserved for dynamic allocation, but we should reject it for explicit configs
    if port == 0 {
        return Err(DeploymentError::ValidationError(
            "Port cannot be 0".to_string(),
        ));
    }

    Ok(())
}

/// Validate system requirements for deployment
///
/// # Arguments
/// * `system_info` - System information HashMap from SSH client
/// * `deployment_config` - Deployment configuration with SVM and node type
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn validate_system_requirements(
    system_info: &HashMap<String, String>,
    deployment_config: &DeploymentConfig,
) -> Result<(), DeploymentError> {
    // Basic validation: check that we have CPU and memory info
    if system_info.get("cpu_cores").is_none() {
        return Err(DeploymentError::ValidationError(
            "Could not detect CPU cores".to_string(),
        ));
    }

    if system_info.get("memory_gb").is_none() {
        return Err(DeploymentError::ValidationError(
            "Could not detect memory".to_string(),
        ));
    }

    // Validate SVM type
    match deployment_config.svm_type.as_str() {
        "solana" | "sonic" | "eclipse" | "s00n" | "sui" | "aptos" => {}
        _ => {
            return Err(DeploymentError::ValidationError(format!(
                "Unsupported SVM type: {}",
                deployment_config.svm_type
            )));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_remote_path_valid() {
        assert!(validate_remote_path("/home/user/file.txt").is_ok());
        assert!(validate_remote_path("/tmp").is_ok());
        assert!(validate_remote_path("/").is_ok());
    }

    #[test]
    fn test_validate_remote_path_directory_traversal() {
        assert!(validate_remote_path("/home/../etc/passwd").is_err());
        assert!(validate_remote_path("../../etc").is_err());
    }

    #[test]
    fn test_validate_remote_path_null_bytes() {
        assert!(validate_remote_path("/path\0to/file").is_err());
    }

    #[test]
    fn test_validate_remote_path_newlines() {
        assert!(validate_remote_path("/path\nto/file").is_err());
        assert!(validate_remote_path("/path\rto/file").is_err());
    }

    #[test]
    fn test_validate_remote_path_empty() {
        assert!(validate_remote_path("").is_err());
    }

    #[test]
    fn test_validate_service_name_valid() {
        assert!(validate_service_name("opensvm").is_ok());
        assert!(validate_service_name("my-service").is_ok());
        assert!(validate_service_name("service_v1").is_ok());
        assert!(validate_service_name("service:restart").is_ok());
        assert!(validate_service_name("service.socket").is_ok());
    }

    #[test]
    fn test_validate_service_name_invalid() {
        assert!(validate_service_name("").is_err());
        assert!(validate_service_name("service;command").is_err());
        assert!(validate_service_name("service&another").is_err());
        assert!(validate_service_name("service|cat").is_err());
    }

    #[test]
    fn test_validate_package_name_valid() {
        assert!(validate_package_name("build-essential").is_ok());
        assert!(validate_package_name("libssl-dev").is_ok());
        assert!(validate_package_name("pkg_config").is_ok());
        assert!(validate_package_name("python3").is_ok());
    }

    #[test]
    fn test_validate_package_name_invalid() {
        assert!(validate_package_name("").is_err());
        assert!(validate_package_name("-invalid").is_err());
        assert!(validate_package_name("package;rm").is_err());
        assert!(validate_package_name("package$(whoami)").is_err());
    }

    #[test]
    fn test_validate_hostname_valid() {
        assert!(validate_hostname("localhost").is_ok());
        assert!(validate_hostname("example.com").is_ok());
        assert!(validate_hostname("192.168.1.1").is_ok());
        assert!(validate_hostname("my-server-01").is_ok());
    }

    #[test]
    fn test_validate_hostname_invalid() {
        assert!(validate_hostname("").is_err());
        assert!(validate_hostname("host\0name").is_err());
    }

    #[test]
    fn test_validate_port_valid() {
        assert!(validate_port(22).is_ok());
        assert!(validate_port(80).is_ok());
        assert!(validate_port(443).is_ok());
        assert!(validate_port(8080).is_ok());
        assert!(validate_port(65535).is_ok());
    }

    #[test]
    fn test_validate_port_invalid() {
        assert!(validate_port(0).is_err());
    }
}
