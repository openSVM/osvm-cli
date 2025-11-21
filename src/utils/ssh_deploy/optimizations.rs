//! System optimization utilities for Solana validator/RPC deployment

use crate::utils::ssh_deploy::{client::SshClient, errors::DeploymentError};

/// Apply recommended system optimizations for Solana validator/RPC
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn apply_system_optimizations(client: &mut SshClient) -> Result<(), DeploymentError> {
    // Apply kernel and network parameter optimizations
    apply_sysctl_settings(client)?;

    // Set CPU governor to performance mode
    set_cpu_performance(client)?;

    // Configure file descriptor limits
    configure_file_limits(client)?;

    Ok(())
}

/// Apply recommended sysctl settings for Solana
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn apply_sysctl_settings(client: &mut SshClient) -> Result<(), DeploymentError> {
    let sysctl_content = r"
# TCP Buffer Sizes (10k min, 87.38k default, 12M max)
net.ipv4.tcp_rmem=10240 87380 12582912
net.ipv4.tcp_wmem=10240 87380 12582912

# TCP Optimization
net.ipv4.tcp_congestion_control=westwood
net.ipv4.tcp_fastopen=3
net.ipv4.tcp_timestamps=0
net.ipv4.tcp_sack=1
net.ipv4.tcp_low_latency=1
net.ipv4.tcp_tw_reuse=1
net.ipv4.tcp_no_metrics_save=1
net.ipv4.tcp_moderate_rcvbuf=1

# Kernel Optimization
kernel.timer_migration=0
kernel.hung_task_timeout_secs=30
kernel.pid_max=49152

# Virtual Memory Tuning
vm.swappiness=30
vm.max_map_count=2000000
vm.stat_interval=10
vm.dirty_ratio=40
vm.dirty_background_ratio=10
vm.min_free_kbytes=3000000
vm.dirty_expire_centisecs=36000
vm.dirty_writeback_centisecs=3000
vm.dirtytime_expire_seconds=43200

# Solana Specific Tuning
net.core.rmem_max=134217728
net.core.rmem_default=134217728
net.core.wmem_max=134217728
net.core.wmem_default=134217728
";

    // Write settings to a temporary file
    let temp_path = "/tmp/solana_sysctl.conf";
    client.execute_command(&format!(
        "cat > {} << 'EOL'\n{}\nEOL",
        temp_path, sysctl_content
    ))?;

    // Append settings to sysctl.conf if they don't already exist
    client.execute_command(&format!(
        r#"
        while IFS= read -r line || [[ -n "$line" ]]; do
            if [[ "$line" =~ ^[^#] ]] && ! grep -q "^$line$" /etc/sysctl.conf; then
                echo "$line" | sudo tee -a /etc/sysctl.conf > /dev/null
            fi
        done < {}
        "#,
        temp_path
    ))?;

    // Apply the settings
    client.execute_command("sudo sysctl -p")?;

    // Clean up
    client.execute_command(&format!("rm {}", temp_path))?;

    Ok(())
}

/// Set CPU governor to performance mode
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn set_cpu_performance(client: &mut SshClient) -> Result<(), DeploymentError> {
    // Install cpufrequtils if not already installed
    client.execute_command("sudo apt-get update && sudo apt-get install -y cpufrequtils")?;

    // Set default governor to performance
    client
        .execute_command("echo 'GOVERNOR=\"performance\"' | sudo tee /etc/default/cpufrequtils")?;

    // Apply to all CPUs (handle both success and failure)
    client.execute_command("echo \"performance\" | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor || true")?;

    Ok(())
}

/// Configure file descriptor limits
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn configure_file_limits(client: &mut SshClient) -> Result<(), DeploymentError> {
    // Create systemd directory if it doesn't exist
    client.execute_command("sudo mkdir -p /etc/systemd/system.conf.d")?;

    // Add file descriptor limits to systemd
    let limits_content = "[Manager]\nDefaultLimitNOFILE=1000000\n";
    client.execute_command(&format!(
        "echo '{}' | sudo tee /etc/systemd/system.conf.d/10-solana-limits.conf",
        limits_content
    ))?;

    // Create service-specific drop-in directory
    client.execute_command("sudo mkdir -p /etc/systemd/system/solana-validator.service.d")?;

    // Also add to specific service settings
    let service_limits = "[Service]\nLimitNOFILE=1000000\n";
    client.execute_command(&format!(
        "echo '{}' | sudo tee /etc/systemd/system/solana-validator.service.d/limits.conf",
        service_limits
    ))?;

    // Reload systemd
    client.execute_command("sudo systemctl daemon-reload")?;

    // Update system limits in /etc/security/limits.conf
    let limits_entry = "* soft nofile 1000000\n* hard nofile 1000000";
    client.execute_command(&format!(
        "grep -q 'nofile 1000000' /etc/security/limits.conf || echo '{}' | sudo tee -a /etc/security/limits.conf",
        limits_entry
    ))?;

    Ok(())
}

/// Configure firewall settings for Solana validator/RPC
///
/// # Arguments
/// * `client` - SSH client
/// * `is_rpc` - Whether the node is an RPC node
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn configure_firewall(client: &mut SshClient, is_rpc: bool) -> Result<(), DeploymentError> {
    // Install UFW if not already installed
    client.execute_command("sudo apt-get update && sudo apt-get install -y ufw")?;

    // Allow SSH first to prevent lockout
    client.execute_command("sudo ufw allow OpenSSH")?;

    // Allow gossip ports
    client.execute_command("sudo ufw allow 8000:8020/tcp")?;
    client.execute_command("sudo ufw allow 8000:8020/udp")?;

    // If RPC node, allow RPC port
    if is_rpc {
        client.execute_command("sudo ufw allow 8899/tcp")?;
    }

    // Enable firewall
    client.execute_command("echo 'y' | sudo ufw enable")?;

    Ok(())
}

/// Set up log rotation for Solana validator
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn setup_log_rotation(client: &mut SshClient) -> Result<(), DeploymentError> {
    let logrotate_content = r"/home/$(whoami)/solana-validator.log {
    rotate 7
    daily
    missingok
    postrotate
        systemctl kill -s USR1 solana-validator.service
    endscript
}";

    // Create logrotate file
    client.execute_command(&format!(
        "echo '{}' | sudo tee /etc/logrotate.d/solana",
        logrotate_content
    ))?;

    // Restart logrotate service
    client.execute_command("sudo systemctl restart logrotate.service")?;

    Ok(())
}
