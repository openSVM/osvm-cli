//! System validation utilities for SSH deployment

use {
    crate::utils::ssh_deploy::{errors::DeploymentError, types::DeploymentConfig},
    std::collections::HashMap,
};

/// Validate system requirements for deployment
///
/// # Arguments
/// * `system_info` - System information
/// * `deployment_config` - Deployment configuration
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn validate_system_requirements(
    system_info: &HashMap<String, String>,
    deployment_config: &DeploymentConfig,
) -> Result<(), DeploymentError> {
    // Get required resources based on SVM and node type
    let (required_cpu, required_memory, required_disk) =
        get_required_resources(&deployment_config.svm_type, &deployment_config.node_type)?;

    // Validate CPU cores
    validate_cpu_cores(system_info, required_cpu)?;

    // Validate memory
    validate_memory(system_info, required_memory)?;

    // Validate disk space
    validate_disk_space(system_info, required_disk)?;

    Ok(())
}

/// Get required resources based on SVM and node type
///
/// # Arguments
/// * `svm_type` - SVM type
/// * `node_type` - Node type
///
/// # Returns
/// * `Result<(u8, u16, u16), DeploymentError>` - Required CPU, memory, and disk
pub fn get_required_resources(
    svm_type: &str,
    node_type: &str,
) -> Result<(u8, u16, u16), DeploymentError> {
    match (svm_type, node_type) {
        ("solana", "validator") => Ok((12, 128, 2048)),
        ("solana", "rpc") => Ok((16, 256, 4096)),
        ("sonic", "validator") => Ok((8, 32, 1024)),
        ("sonic", "rpc") => Ok((16, 64, 2048)),
        ("sui", "validator") => Ok((8, 32, 1024)),
        ("sui", "rpc") => Ok((16, 64, 2048)),
        ("aptos", "validator") => Ok((8, 32, 1024)),
        ("aptos", "rpc") => Ok((16, 64, 2048)),
        _ => Err(DeploymentError::ValidationError(format!(
            "Unsupported SVM type or node type: {}/{}",
            svm_type, node_type
        ))),
    }
}

/// Validate CPU cores
///
/// # Arguments
/// * `system_info` - System information
/// * `required_cpu` - Required CPU cores
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn validate_cpu_cores(
    system_info: &HashMap<String, String>,
    required_cpu: u8,
) -> Result<(), DeploymentError> {
    let cpu_cores = system_info
        .get("cpu_cores")
        .and_then(|s| s.parse::<u8>().ok())
        .unwrap_or(0);

    if cpu_cores < required_cpu {
        return Err(DeploymentError::ValidationError(format!(
            "Insufficient CPU cores: {} (required: {})",
            cpu_cores, required_cpu
        )));
    }

    Ok(())
}

/// Validate memory
///
/// # Arguments
/// * `system_info` - System information
/// * `required_memory` - Required memory in GB
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn validate_memory(
    system_info: &HashMap<String, String>,
    required_memory: u16,
) -> Result<(), DeploymentError> {
    let memory_gb = system_info
        .get("memory_gb")
        .and_then(|s| s.parse::<u16>().ok())
        .unwrap_or(0);

    if memory_gb < required_memory {
        return Err(DeploymentError::ValidationError(format!(
            "Insufficient memory: {} GB (required: {} GB)",
            memory_gb, required_memory
        )));
    }

    Ok(())
}

/// Validate disk space
///
/// # Arguments
/// * `system_info` - System information
/// * `required_disk` - Required disk space in GB
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn validate_disk_space(
    system_info: &HashMap<String, String>,
    required_disk: u16,
) -> Result<(), DeploymentError> {
    let available_disk = system_info
        .get("disk_available")
        .and_then(|s| {
            // Parse disk space - handle different units (G, T)
            if s.ends_with("G") {
                s[..s.len() - 1].parse::<f64>().ok().map(|v| v as u16)
            } else if s.ends_with("T") {
                s[..s.len() - 1]
                    .parse::<f64>()
                    .ok()
                    .map(|v| (v * 1024.0) as u16)
            } else {
                None
            }
        })
        .unwrap_or(0);

    if available_disk < required_disk {
        return Err(DeploymentError::ValidationError(format!(
            "Insufficient disk space: {} GB (required: {} GB)",
            available_disk, required_disk
        )));
    }

    Ok(())
}
