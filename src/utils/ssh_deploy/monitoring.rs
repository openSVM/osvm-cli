//! Monitoring and metrics utilities for Solana validator/RPC deployment

use crate::utils::ssh_deploy::{client::SshClient, errors::DeploymentError};

/// Set up monitoring for Solana validator/RPC
///
/// # Arguments
/// * `client` - SSH client
/// * `install_dir` - Installation directory
/// * `metrics_config` - Metrics configuration string
/// * `node_type` - Node type (validator or rpc)
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn setup_monitoring(
    client: &mut SshClient,
    install_dir: &str,
    metrics_config: &str,
    node_type: &str,
) -> Result<(), DeploymentError> {
    // Create directory for monitoring scripts
    let monitoring_dir = format!("{}/monitoring", install_dir);
    client.execute_command(&format!("mkdir -p {}", monitoring_dir))?;

    // Configure Solana metrics if provided
    if !metrics_config.is_empty() {
        configure_solana_metrics(client, metrics_config)?;
    }

    // Set up basic monitoring script
    setup_basic_monitoring(client, &monitoring_dir, node_type)?;

    // Set up alerts for critical issues
    setup_alert_system(client, &monitoring_dir)?;

    Ok(())
}

/// Configure Solana metrics environment
///
/// # Arguments
/// * `client` - SSH client
/// * `metrics_config` - Metrics configuration string
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn configure_solana_metrics(
    client: &mut SshClient,
    metrics_config: &str,
) -> Result<(), DeploymentError> {
    // Add metrics config to .bashrc
    let bashrc_entry = format!("export SOLANA_METRICS_CONFIG=\"{}\"", metrics_config);

    // Add to .bashrc if not already present
    client.execute_command(&format!(
        r#"grep -q "SOLANA_METRICS_CONFIG" ~/.bashrc || echo '{}' >> ~/.bashrc"#,
        bashrc_entry
    ))?;

    Ok(())
}

/// Set up basic monitoring script
///
/// # Arguments
/// * `client` - SSH client
/// * `monitoring_dir` - Monitoring directory
/// * `node_type` - Node type (validator or rpc)
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn setup_basic_monitoring(
    client: &mut SshClient,
    monitoring_dir: &str,
    node_type: &str,
) -> Result<(), DeploymentError> {
    let monitor_script_content = format!(
        r#"#!/bin/bash
# Solana {} monitoring script

SERVICE_NAME="solana-validator.service"
LOG_FILE="/home/$(whoami)/solana-validator.log"
MONITOR_LOG="{}/monitor.log"

# Check service status
SERVICE_STATUS=$(systemctl is-active "$SERVICE_NAME")
echo "$(date): Service status: $SERVICE_STATUS" >> "$MONITOR_LOG"

# Get validator stats
if [ "$SERVICE_STATUS" = "active" ]; then
    # Check validator progress and health
    CATCHUP_INFO=$(solana catchup --our-localhost | tail -n2)
    echo "$(date): $CATCHUP_INFO" >> "$MONITOR_LOG"
    
    # Get system metrics
    LOAD=$(uptime | awk -F'load average: ' '{{ print $2 }}')
    DISK=$(df -h /mnt/ledger /mnt/extras | grep -v "Filesystem")
    MEM=$(free -h | grep "Mem:" | awk '{{ print "Total: "$2", Used: "$3", Free: "$4 }}')
    
    # Log system metrics
    echo "$(date): Load average: $LOAD" >> "$MONITOR_LOG"
    echo "$(date): Disk usage:" >> "$MONITOR_LOG"
    echo "$DISK" >> "$MONITOR_LOG"
    echo "$(date): Memory usage: $MEM" >> "$MONITOR_LOG"
else
    echo "$(date): Service is not running, attempting to restart..." >> "$MONITOR_LOG"
    sudo systemctl restart "$SERVICE_NAME"
fi

# Clean up old monitor logs (keep last 7 days)
find "$(dirname "$MONITOR_LOG")" -name "*.log" -type f -mtime +7 -delete
"#,
        node_type, monitoring_dir
    );

    // Create monitoring script
    let monitor_script_path = format!("{}/monitor.sh", monitoring_dir);
    client.execute_command(&format!(
        "cat > {} << 'EOL'\n{}\nEOL",
        monitor_script_path, monitor_script_content
    ))?;
    client.execute_command(&format!("chmod +x {}", monitor_script_path))?;

    // Set up cron job to run monitoring every 15 minutes
    let cron_entry = format!("*/15 * * * * {}", monitor_script_path);
    client.execute_command(&format!(
        "(crontab -l 2>/dev/null | grep -v '{}' ; echo '{}') | crontab -",
        monitor_script_path, cron_entry
    ))?;

    Ok(())
}

/// Set up alert system for critical issues
///
/// # Arguments
/// * `client` - SSH client
/// * `monitoring_dir` - Monitoring directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
fn setup_alert_system(client: &mut SshClient, monitoring_dir: &str) -> Result<(), DeploymentError> {
    let alert_script_content = r#"#!/bin/bash
# Alert script for Solana validator critical issues

SERVICE_NAME="solana-validator.service"
LOG_FILE="/home/$(whoami)/solana-validator.log"
ALERT_LOG="$(dirname "$0")/alerts.log"
EMAIL_TO=""  # Set this to your email address to receive alerts

# Function to send alerts
send_alert() {
    local subject="$1"
    local message="$2"
    
    # Log the alert
    echo "$(date): $subject - $message" >> "$ALERT_LOG"
    
    # Send email if configured
    if [ -n "$EMAIL_TO" ] && command -v mail >/dev/null 2>&1; then
        echo "$message" | mail -s "Solana Validator Alert: $subject" "$EMAIL_TO"
    fi
}

# Check if service is running
if ! systemctl is-active --quiet "$SERVICE_NAME"; then
    send_alert "Service Down" "Validator service is not running on $(hostname)"
    exit 1
fi

# Check for excessive catching up
if grep -q "behind by" "$LOG_FILE" | tail -n 100; then
    SLOTS_BEHIND=$(grep "behind by" "$LOG_FILE" | tail -n 1 | grep -oP "behind by \K[0-9]+")
    
    if [ -n "$SLOTS_BEHIND" ] && [ "$SLOTS_BEHIND" -gt 1000 ]; then
        send_alert "Falling Behind" "Validator is $SLOTS_BEHIND slots behind"
    fi
fi

# Check disk space
LEDGER_DISK_USAGE=$(df -h /mnt/ledger | tail -n1 | awk '{print $5}' | sed 's/%//')
ACCOUNTS_DISK_USAGE=$(df -h /mnt/extras | tail -n1 | awk '{print $5}' | sed 's/%//')

if [ "$LEDGER_DISK_USAGE" -gt 90 ]; then
    send_alert "Disk Space Critical" "Ledger disk usage is at ${LEDGER_DISK_USAGE}%"
fi

if [ "$ACCOUNTS_DISK_USAGE" -gt 90 ]; then
    send_alert "Disk Space Critical" "Accounts disk usage is at ${ACCOUNTS_DISK_USAGE}%"
fi

# Check for any errors in the log
ERROR_COUNT=$(grep -i "error" "$LOG_FILE" | tail -n 1000 | wc -l)
if [ "$ERROR_COUNT" -gt 50 ]; then
    RECENT_ERRORS=$(grep -i "error" "$LOG_FILE" | tail -n 10)
    send_alert "Excessive Errors" "Found $ERROR_COUNT errors in recent logs. Latest: $RECENT_ERRORS"
fi
"#;

    // Create alert script
    let alert_script_path = format!("{}/alert.sh", monitoring_dir);
    client.execute_command(&format!(
        "cat > {} << 'EOL'\n{}\nEOL",
        alert_script_path, alert_script_content
    ))?;
    client.execute_command(&format!("chmod +x {}", alert_script_path))?;

    // Set up cron job to run alerts every 30 minutes
    let cron_entry = format!("*/30 * * * * {}", alert_script_path);
    client.execute_command(&format!(
        "(crontab -l 2>/dev/null | grep -v '{}' ; echo '{}') | crontab -",
        alert_script_path, cron_entry
    ))?;

    Ok(())
}

/// Install Grafana and InfluxDB monitoring stack (optional)
///
/// # Arguments
/// * `client` - SSH client
/// * `install_dir` - Installation directory
///
/// # Returns
/// * `Result<(), DeploymentError>` - Success/failure
pub fn install_monitoring_stack(
    client: &mut SshClient,
    install_dir: &str,
) -> Result<(), DeploymentError> {
    // Create directory for monitoring stack
    let stack_dir = format!("{}/monitoring-stack", install_dir);
    client.execute_command(&format!("mkdir -p {}", stack_dir))?;

    // Create docker-compose file for monitoring stack
    let docker_compose_content = r#"version: '3'
services:
  influxdb:
    image: influxdb:1.8
    container_name: influxdb
    ports:
      - "8086:8086"
    volumes:
      - influxdb-data:/var/lib/influxdb
    environment:
      - INFLUXDB_DB=solana
      - INFLUXDB_ADMIN_USER=admin
      - INFLUXDB_ADMIN_PASSWORD=admin

  grafana:
    image: grafana/grafana:latest
    container_name: grafana
    ports:
      - "3000:3000"
    volumes:
      - grafana-data:/var/lib/grafana
    environment:
      - GF_SECURITY_ADMIN_USER=admin
      - GF_SECURITY_ADMIN_PASSWORD=admin
    depends_on:
      - influxdb

volumes:
  influxdb-data:
  grafana-data:
"#;

    // Write docker-compose file
    let docker_compose_path = format!("{}/docker-compose.yml", stack_dir);
    client.execute_command(&format!(
        "cat > {} << 'EOL'\n{}\nEOL",
        docker_compose_path, docker_compose_content
    ))?;

    // Create script to install Docker and Docker Compose if needed
    let setup_script_content = r#"#!/bin/bash
# Setup script for Grafana + InfluxDB monitoring stack

# Install Docker if not installed
if ! command -v docker &> /dev/null; then
    echo "Installing Docker..."
    curl -fsSL https://get.docker.com -o get-docker.sh
    sudo sh get-docker.sh
    sudo usermod -aG docker $(whoami)
fi

# Install Docker Compose if not installed
if ! command -v docker-compose &> /dev/null; then
    echo "Installing Docker Compose..."
    sudo curl -L "https://github.com/docker/compose/releases/download/v2.17.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
    sudo chmod +x /usr/local/bin/docker-compose
fi

# Start monitoring stack
docker-compose up -d

# Print access instructions
echo "Monitoring stack is now running"
echo "Grafana is available at http://localhost:3000"
echo "Default login: admin/admin"
echo ""
echo "Remember to configure Grafana to use InfluxDB as a data source:"
echo "URL: http://influxdb:8086"
echo "Database: solana"
echo "User: admin"
echo "Password: admin"
"#;

    // Write setup script
    let setup_script_path = format!("{}/setup.sh", stack_dir);
    client.execute_command(&format!(
        "cat > {} << 'EOL'\n{}\nEOL",
        setup_script_path, setup_script_content
    ))?;
    client.execute_command(&format!("chmod +x {}", setup_script_path))?;

    // Don't automatically install - just provide the scripts
    client.execute_command(&format!(
        "echo 'Monitoring stack setup script created at {}. Run this script to install Grafana + InfluxDB.'",
        setup_script_path
    ))?;

    Ok(())
}
