//! Hot-swap capability for Solana validators

use crate::utils::ssh_deploy::{client::SshClient, errors::DeploymentError};

/// Configure hot-swap capability for Solana validator
///
/// # Arguments
/// * `client` - SSH client
/// * `install_dir` - Installation directory
///
/// # Returns
/// * `Result<(String, String), DeploymentError>` - Paths to staked and unstaked keypairs
pub fn configure_hot_swap(
    client: &mut SshClient,
    install_dir: &str,
) -> Result<(String, String), DeploymentError> {
    // Create keypair directory
    let keypair_dir = format!("{}/keypairs", install_dir);
    client.execute_command(&format!("mkdir -p {}", keypair_dir))?;

    // Generate staked keypair if it doesn't exist
    let staked_keypair_path = format!("{}/staked.json", keypair_dir);
    if !client.file_exists(&staked_keypair_path)? {
        client.execute_command(&format!(
            "solana-keygen new -o {} --no-passphrase",
            staked_keypair_path
        ))?;
    }

    // Generate unstaked keypair if it doesn't exist
    let unstaked_keypair_path = format!("{}/unstaked.json", keypair_dir);
    if !client.file_exists(&unstaked_keypair_path)? {
        client.execute_command(&format!(
            "solana-keygen new -o {} --no-passphrase",
            unstaked_keypair_path
        ))?;
    }

    // Create identity transition script
    create_identity_transition_script(
        client,
        &keypair_dir,
        &staked_keypair_path,
        &unstaked_keypair_path,
    )?;

    // Set up monitoring for automatic failover
    setup_failover_monitoring(client, &keypair_dir)?;

    Ok((staked_keypair_path, unstaked_keypair_path))
}

/// Create script for identity transition between staked and unstaked identities
///
/// # Arguments
/// * `client` - SSH client
/// * `keypair_dir` - Keypair directory
/// * `staked_keypair_path` - Path to staked keypair
/// * `unstaked_keypair_path` - Path to unstaked keypair
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn create_identity_transition_script(
    client: &mut SshClient,
    keypair_dir: &str,
    staked_keypair_path: &str,
    unstaked_keypair_path: &str,
) -> Result<(), DeploymentError> {
    let script_content = format!(
        r#"#!/bin/bash
# Script for transitioning between validator identities
# Based on the Identity Transition methodology by Pumpkin

SERVICE_NAME="solana-validator.service"
IDENTITY_FILE="/home/$(whoami)/validator-identity.json"

# Function to switch to staked identity
switch_to_staked() {{
    echo "Switching to staked identity"
    cp {} "$IDENTITY_FILE"
    sudo systemctl restart $SERVICE_NAME
    echo "Transition to staked identity completed"
}}

# Function to switch to unstaked identity
switch_to_unstaked() {{
    echo "Switching to unstaked identity"
    cp {} "$IDENTITY_FILE"
    sudo systemctl restart $SERVICE_NAME
    echo "Transition to unstaked identity completed"
}}

# Main execution
case "$1" in
    staked)
        switch_to_staked
        ;;
    unstaked)
        switch_to_unstaked
        ;;
    *)
        echo "Usage: $0 {{staked|unstaked}}"
        exit 1
        ;;
esac

# Verify switch was successful
sleep 5
sudo systemctl status $SERVICE_NAME
"#,
        staked_keypair_path, unstaked_keypair_path
    );

    let script_path = format!("{}/switch-identity.sh", keypair_dir);
    client.execute_command(&format!(
        "cat > {} << 'EOL'\n{}\nEOL",
        script_path, script_content
    ))?;
    client.execute_command(&format!("chmod +x {}", script_path))?;

    Ok(())
}

/// Set up monitoring for automatic failover
///
/// # Arguments
/// * `client` - SSH client
/// * `keypair_dir` - Keypair directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn setup_failover_monitoring(
    client: &mut SshClient,
    keypair_dir: &str,
) -> Result<(), DeploymentError> {
    let monitor_script_content = r#"#!/bin/bash
# Validator health monitoring script for automatic failover

SERVICE_NAME="solana-validator.service"
SWITCH_SCRIPT="$(dirname "$0")/switch-identity.sh"
LOG_FILE="/home/$(whoami)/solana-validator.log"
CURRENT_STATE_FILE="$(dirname "$0")/current_state"

# Create state file if it doesn't exist (default to staked)
if [ ! -f "$CURRENT_STATE_FILE" ]; then
    echo "staked" > "$CURRENT_STATE_FILE"
fi

# Check if validator is running
if ! systemctl is-active --quiet "$SERVICE_NAME"; then
    echo "$(date): Validator service is not running, restarting..."
    sudo systemctl restart "$SERVICE_NAME"
    exit 1
fi

# Check if validator is catching up
if grep -q "behind by" "$LOG_FILE" | tail -n 100; then
    SLOTS_BEHIND=$(grep "behind by" "$LOG_FILE" | tail -n 1 | grep -oP "behind by \K[0-9]+")

    if [ -n "$SLOTS_BEHIND" ] && [ "$SLOTS_BEHIND" -gt 500 ]; then
        CURRENT_STATE=$(cat "$CURRENT_STATE_FILE")

        if [ "$CURRENT_STATE" = "staked" ]; then
            echo "$(date): Validator falling behind ($SLOTS_BEHIND slots), switching to unstaked identity"
            "$SWITCH_SCRIPT" unstaked
            echo "unstaked" > "$CURRENT_STATE_FILE"
        fi
    fi
fi

# Check if validator has caught up and can return to staked identity
if ! grep -q "behind by" "$LOG_FILE" | tail -n 100; then
    CURRENT_STATE=$(cat "$CURRENT_STATE_FILE")

    if [ "$CURRENT_STATE" = "unstaked" ]; then
        echo "$(date): Validator caught up, switching back to staked identity"
        "$SWITCH_SCRIPT" staked
        echo "staked" > "$CURRENT_STATE_FILE"
    fi
fi
"#;

    let monitor_script_path = format!("{}/monitor-health.sh", keypair_dir);
    client.execute_command(&format!(
        "cat > {} << 'EOL'\n{}\nEOL",
        monitor_script_path, monitor_script_content
    ))?;
    client.execute_command(&format!("chmod +x {}", monitor_script_path))?;

    // Create cron job to run monitoring every 5 minutes
    let cron_entry = format!("*/5 * * * * {}", monitor_script_path);
    client.execute_command(&format!(
        "(crontab -l 2>/dev/null | grep -v '{}' ; echo '{}') | crontab -",
        monitor_script_path, cron_entry
    ))?;

    Ok(())
}

/// Create a log rotation configuration for Solana validator
///
/// # Arguments
/// * `client` - SSH client
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn configure_log_rotation(client: &mut SshClient) -> Result<(), DeploymentError> {
    let logrotate_content = r"/home/$(whoami)/solana-validator.log {
    rotate 7
    daily
    missingok
    postrotate
        systemctl kill -s USR1 solana-validator.service
    endscript
}";

    // Write log rotation configuration
    client.execute_command(&format!(
        "cat > /tmp/solana-logrotate << 'EOL'\n{}\nEOL",
        logrotate_content
    ))?;
    client.execute_command("sudo cp /tmp/solana-logrotate /etc/logrotate.d/solana")?;
    client.execute_command("rm /tmp/solana-logrotate")?;

    // Restart logrotate service
    client.execute_command("sudo systemctl restart logrotate.service")?;

    Ok(())
}
