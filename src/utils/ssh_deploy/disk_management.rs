//! Disk management utilities for Solana validator/RPC deployment

use crate::utils::ssh_deploy::{client::SshClient, errors::DeploymentError};

/// Set up disk storage for Solana validator/RPC
///
/// # Arguments
/// * `client` - SSH client
/// * `ledger_disk` - Ledger disk device path (e.g., "/dev/nvme0n1")
/// * `accounts_disk` - Accounts disk device path (e.g., "/dev/nvme1n1")
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn setup_disk_storage(
    client: &mut SshClient,
    ledger_disk: &str,
    accounts_disk: &str,
) -> Result<(), DeploymentError> {
    // Create mount directories
    create_mount_directories(client)?;

    // Format and mount ledger disk
    format_and_mount_disk(client, ledger_disk, "/mnt/ledger")?;

    // Format and mount accounts disk
    format_and_mount_disk(client, accounts_disk, "/mnt/extras")?;

    // Create subdirectories for accounts and snapshots
    client.execute_command("mkdir -p /mnt/extras/snapshot /mnt/extras/accounts")?;

    // Set appropriate permissions
    client.execute_command("sudo chown -R $(whoami):$(whoami) /mnt/ledger /mnt/extras")?;

    Ok(())
}

/// Create necessary mount directories
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn create_mount_directories(client: &mut SshClient) -> Result<(), DeploymentError> {
    client.execute_command("sudo mkdir -p /mnt/ledger /mnt/extras")?;

    Ok(())
}

/// Format and mount a disk
///
/// # Arguments
/// * `client` - SSH client
/// * `disk` - Disk device path
/// * `mount_point` - Mount point path
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn format_and_mount_disk(
    client: &mut SshClient,
    disk: &str,
    mount_point: &str,
) -> Result<(), DeploymentError> {
    // Check if the disk is already formatted and mounted
    let mount_check = client.execute_command(&format!("findmnt -S {} -o TARGET -n", disk))?;
    if !mount_check.trim().is_empty() {
        // Disk is already mounted somewhere
        if mount_check.trim() == mount_point {
            // Already mounted at the correct location
            return Ok(());
        }
        // Mounted at a different location, return an error
        return Err(DeploymentError::DeploymentError(format!(
            "Disk {} is already mounted at {}, cannot mount at {}",
            disk,
            mount_check.trim(),
            mount_point
        )));
    }

    // Format the disk (with confirmation to avoid data loss)
    let _confirmation =
        client.execute_command(&format!("echo \"y\" | sudo mkfs -t ext4 {}", disk))?;

    // Mount the disk
    client.execute_command(&format!("sudo mount {} {}", disk, mount_point))?;

    // Add to fstab for persistence across reboots
    let fstab_entry = format!("{} {} ext4 defaults,noatime 0 2", disk, mount_point);
    let _fstab_check = client.execute_command(&format!(
        "grep -q '{}' /etc/fstab || echo '{}' | sudo tee -a /etc/fstab",
        disk, fstab_entry
    ))?;

    Ok(())
}

/// Validate disk requirements for Solana validator/RPC
///
/// # Arguments
/// * `client` - SSH client
/// * `ledger_disk` - Ledger disk device path
/// * `accounts_disk` - Accounts disk device path
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure with detailed information about any issues
pub fn validate_disk_requirements(
    client: &mut SshClient,
    ledger_disk: &str,
    accounts_disk: &str,
) -> Result<(), DeploymentError> {
    // Check if disks exist
    if !disk_exists(client, ledger_disk)? {
        return Err(DeploymentError::DeploymentError(format!(
            "Ledger disk {} does not exist",
            ledger_disk
        )));
    }

    if !disk_exists(client, accounts_disk)? {
        return Err(DeploymentError::DeploymentError(format!(
            "Accounts disk {} does not exist",
            accounts_disk
        )));
    }

    // Check disk sizes
    let ledger_size = get_disk_size(client, ledger_disk)?;
    let accounts_size = get_disk_size(client, accounts_disk)?;

    // Minimum size requirements (in GB)
    const MIN_LEDGER_SIZE: u64 = 2000; // 2TB
    const MIN_ACCOUNTS_SIZE: u64 = 2000; // 2TB

    if ledger_size < MIN_LEDGER_SIZE {
        return Err(DeploymentError::DeploymentError(format!(
            "Ledger disk size is {}GB, minimum required is {}GB",
            ledger_size, MIN_LEDGER_SIZE
        )));
    }

    if accounts_size < MIN_ACCOUNTS_SIZE {
        return Err(DeploymentError::DeploymentError(format!(
            "Accounts disk size is {}GB, minimum required is {}GB",
            accounts_size, MIN_ACCOUNTS_SIZE
        )));
    }

    Ok(())
}

/// Check if a disk exists
///
/// # Arguments
/// * `client` - SSH client
/// * `disk` - Disk device path
///
/// # Returns
/// * `Result<bool, DeploymentError>` - Whether the disk exists
fn disk_exists(client: &mut SshClient, disk: &str) -> Result<bool, DeploymentError> {
    let result = client.execute_command(&format!("test -b {} && echo 'exists'", disk))?;
    Ok(result.trim() == "exists")
}

/// Get disk size in GB
///
/// # Arguments
/// * `client` - SSH client
/// * `disk` - Disk device path
///
/// # Returns
/// * `Result<u64, DeploymentError>` - Disk size in GB
fn get_disk_size(client: &mut SshClient, disk: &str) -> Result<u64, DeploymentError> {
    let size_bytes = client.execute_command(&format!("lsblk -b -n -o SIZE {}", disk))?;

    let size_bytes = size_bytes.trim().parse::<u64>().map_err(|e| {
        DeploymentError::DeploymentError(format!("Failed to parse disk size: {}", e))
    })?;

    // Convert bytes to GB
    Ok(size_bytes / 1_000_000_000)
}
