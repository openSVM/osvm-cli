//! Devnet RPC node management
//!
//! This module provides functionality to run a legitimate RPC node that syncs
//! with Solana devnet and stores blockchain data locally.

use crate::utils::log_monitor::{monitor_logs_continuous, LogMonitorConfig};
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};
use tokio::sync::mpsc;
use tokio::task;

/// Configuration for devnet RPC node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DevnetRpcConfig {
    pub ledger_path: String,
    pub rpc_port: u16,
    pub gossip_port: u16,
    pub background: bool,
}

/// Information about a running devnet RPC node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DevnetRpcInfo {
    pub ledger_path: String,
    pub rpc_port: u16,
    pub validator_pubkey: String,
    pub pid: Option<u32>,
    pub network: String,
}

/// Status information for devnet RPC node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DevnetRpcStatus {
    pub running: bool,
    pub pid: Option<u32>,
    pub rpc_port: Option<u16>,
    pub network: String,
    pub syncing: bool,
    pub slot_height: Option<u64>,
    pub uptime: Option<String>,
}

/// Find the agave-validator binary path
fn find_agave_validator() -> Result<String> {
    // First try to find it using 'which'
    let output = Command::new("which")
        .arg("agave-validator")
        .output()
        .context("Failed to execute 'which' command")?;

    if output.status.success() && !output.stdout.is_empty() {
        return Ok(String::from_utf8_lossy(&output.stdout).trim().to_string());
    }

    // Get home directory
    let home = std::env::var("HOME").unwrap_or_else(|_| "/home/larp".to_string());

    // Check common installation paths
    let paths = vec![
        format!(
            "{}/.local/share/solana/install/active_release/bin/agave-validator",
            home
        ),
        "/usr/local/bin/agave-validator".to_string(),
        "/usr/bin/agave-validator".to_string(),
    ];

    for path in paths {
        if std::path::Path::new(&path).exists() {
            return Ok(path);
        }
    }

    anyhow::bail!("agave-validator not found. Please ensure Solana is installed.")
}

/// Find the solana-keygen binary path
fn find_solana_keygen() -> Result<String> {
    // First try to find it using 'which'
    let output = Command::new("which")
        .arg("solana-keygen")
        .output()
        .context("Failed to execute 'which' command")?;

    if output.status.success() && !output.stdout.is_empty() {
        return Ok(String::from_utf8_lossy(&output.stdout).trim().to_string());
    }

    // Get home directory
    let home = std::env::var("HOME").unwrap_or_else(|_| "/home/larp".to_string());

    // Check common installation paths
    let paths = vec![
        format!(
            "{}/.local/share/solana/install/active_release/bin/solana-keygen",
            home
        ),
        "/usr/local/bin/solana-keygen".to_string(),
        "/usr/bin/solana-keygen".to_string(),
    ];

    for path in paths {
        if std::path::Path::new(&path).exists() {
            return Ok(path);
        }
    }

    anyhow::bail!("solana-keygen not found. Please ensure Solana is installed.")
}

/// Apply system tuning parameters (legacy function for compatibility)
async fn apply_system_tuning(param: &str, recommended_value: &str) -> Result<()> {
    println!(
        "🔧 Applying system tuning: {} = {}",
        param, recommended_value
    );

    let output = Command::new("sudo")
        .arg("sysctl")
        .arg("-w")
        .arg(format!("{}={}", param, recommended_value))
        .output()
        .context("Failed to run sysctl")?;

    if output.status.success() {
        println!("✅ Successfully set {} = {}", param, recommended_value);

        // Try to persist to sysctl.conf
        let persist_cmd = Command::new("sh")
            .arg("-c")
            .arg(format!(
                "grep -q '^{}=' /etc/sysctl.conf || echo '{}={}' | sudo tee -a /etc/sysctl.conf > /dev/null",
                param, param, recommended_value
            ))
            .output();

        if let Ok(result) = persist_cmd {
            if result.status.success() {
                println!("✅ Persisted to /etc/sysctl.conf");
            }
        }
        Ok(())
    } else {
        let error_msg = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!("Failed to set {}: {}", param, error_msg.trim())
    }
}

/// Start a devnet RPC node that syncs with real Solana devnet with auto-restart
pub async fn start_devnet_rpc_with_retry(config: DevnetRpcConfig) -> Result<DevnetRpcInfo> {
    let mut attempt = 0;
    let max_attempts = 3;

    loop {
        attempt += 1;
        println!(
            "🚀 Starting devnet RPC (attempt {}/{})",
            attempt, max_attempts
        );

        match start_devnet_rpc_internal(config.clone(), attempt > 1).await {
            Ok(info) => return Ok(info),
            Err(e) => {
                if attempt >= max_attempts {
                    return Err(e);
                }

                // Check if error is due to system tuning
                let error_str = e.to_string();
                if error_str.contains("OS network limit") || error_str.contains("too small") {
                    println!(
                        "⚠️  System tuning issue detected, will retry after applying fixes..."
                    );
                    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
                } else {
                    return Err(e);
                }
            }
        }
    }
}

/// Internal function to start devnet RPC with tuning already applied flag
async fn start_devnet_rpc_internal(
    config: DevnetRpcConfig,
    skip_pre_tuning: bool,
) -> Result<DevnetRpcInfo> {
    println!("🚀 Starting legitimate Solana devnet RPC node");
    println!("📁 Ledger path: {}", config.ledger_path);
    println!("🌐 Network: devnet (will sync with real blockchain)");

    // Find the validator binary
    let validator_path = find_agave_validator()?;
    println!("🔧 Using validator: {}", validator_path);

    // Pre-apply known required system parameters (unless we're retrying)
    if !skip_pre_tuning {
        println!("🔧 Pre-applying known system tuning parameters...");
        let _ = apply_system_tuning("net.core.rmem_max", "134217728").await;
        let _ = apply_system_tuning("net.core.rmem_default", "134217728").await;
        let _ = apply_system_tuning("net.core.wmem_max", "134217728").await;
        let _ = apply_system_tuning("net.core.wmem_default", "134217728").await;
    }

    // Ensure ledger directory exists
    fs::create_dir_all(&config.ledger_path).context("Failed to create ledger directory")?;

    // Generate validator keypair if it doesn't exist
    let keypair_path = format!("{}/validator-keypair.json", config.ledger_path);
    if !std::path::Path::new(&keypair_path).exists() {
        println!("🔑 Generating validator keypair...");

        let keygen_path = find_solana_keygen()?;
        let keygen_output = Command::new(&keygen_path)
            .arg("new")
            .arg("--no-passphrase")
            .arg("--outfile")
            .arg(&keypair_path)
            .output()
            .context("Failed to generate validator keypair")?;

        if !keygen_output.status.success() {
            anyhow::bail!(
                "Failed to generate keypair: {}",
                String::from_utf8_lossy(&keygen_output.stderr)
            );
        }

        // Extract pubkey from output
        let output_str = String::from_utf8_lossy(&keygen_output.stdout);
        let pubkey = extract_pubkey_from_keygen_output(&output_str);
        println!("✅ Generated validator identity: {}", pubkey);
    }

    // Read the validator pubkey
    let keygen_path = find_solana_keygen()?;
    let pubkey_output = Command::new(&keygen_path)
        .arg("pubkey")
        .arg(&keypair_path)
        .output()
        .context("Failed to read validator pubkey")?;

    let validator_pubkey = String::from_utf8_lossy(&pubkey_output.stdout)
        .trim()
        .to_string();

    // Build the agave-validator command for devnet sync
    let mut cmd = Command::new(&validator_path);

    // Identity and basic config
    cmd.arg("--identity").arg(&keypair_path);
    cmd.arg("--ledger").arg(&config.ledger_path);
    cmd.arg("--rpc-port").arg(config.rpc_port.to_string());
    cmd.arg("--gossip-port").arg(config.gossip_port.to_string());
    cmd.arg("--dynamic-port-range").arg("8002-8020");

    // Devnet specific configuration
    cmd.arg("--entrypoint")
        .arg("entrypoint.devnet.solana.com:8001");
    cmd.arg("--entrypoint")
        .arg("entrypoint2.devnet.solana.com:8001");
    cmd.arg("--known-validator")
        .arg("dv1ZAGvdsz5hHLwWXsVnM94hWf1pjbKVau1QVkaMJ92");
    cmd.arg("--known-validator")
        .arg("dv2eQHeP4RFrJZ6UeiZWoc3XTtmtZCUKxxCApCDcRNV");
    cmd.arg("--known-validator")
        .arg("dv4ACNkpYPcE3aKmYDqZm9G5EB3J4MRoeE7WNDRBVJB");
    cmd.arg("--known-validator")
        .arg("dv3qDFk1DTF36Z62bNvrCXe9sKATA6xvVy6A798xxAS");
    cmd.arg("--expected-genesis-hash")
        .arg("EtWTRABZaYq6iMfeYKouRu166VU2xqa1wcaWoxPkrZBG");

    // RPC configuration for full functionality
    cmd.arg("--enable-rpc-transaction-history");
    cmd.arg("--full-rpc-api");
    cmd.arg("--rpc-bind-address").arg("0.0.0.0");

    // Performance and storage optimization
    cmd.arg("--limit-ledger-size").arg("50000000"); // 50GB limit
    cmd.arg("--wal-recovery-mode")
        .arg("skip_any_corrupted_record");
    cmd.arg("--allow-private-addr");

    // RPC-only mode (no voting)
    cmd.arg("--no-voting");

    // Clear any environment variables that might interfere
    cmd.env_clear();
    cmd.envs(std::env::vars().filter(|(k, _)| !k.starts_with("RUST") && !k.contains("PROXY")));

    println!("📡 Connecting to devnet entrypoints...");
    println!("🔗 This will download and sync the real blockchain data");

    if config.background {
        // Run in background
        println!("🌙 Starting in background mode...");

        // Pre-apply system tuning even in background mode
        println!("🔧 Pre-applying system tuning for background mode...");
        let _ = apply_system_tuning("net.core.rmem_max", "134217728").await;
        let _ = apply_system_tuning("net.core.rmem_default", "134217728").await;
        let _ = apply_system_tuning("net.core.wmem_max", "134217728").await;
        let _ = apply_system_tuning("net.core.wmem_default", "134217728").await;

        let mut child = cmd
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to start agave-validator")?;

        // Define monitor config for background mode
        let monitor_config = LogMonitorConfig {
            auto_fix_enabled: true,
            restart_on_critical: true,
            max_restart_attempts: 3,
            restart_cooldown_seconds: 300,
        };

        // Monitor the actual log file that validator writes to
        let log_file_pattern = format!("agave-validator-{}.log", validator_pubkey);
        let log_file_path = format!("./{}", log_file_pattern);

        // Create restart callback for background mode
        let restart_callback: Option<Box<dyn Fn() -> Result<()> + Send + Sync>> =
            Some(Box::new(|| {
                // Kill current process and let retry logic handle restart
                let _ = Command::new("pkill")
                    .arg("-f")
                    .arg("agave-validator")
                    .output();
                Ok(())
            }));

        // Wait a moment for validator to create the log file
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        // Start monitoring the log file in background
        tokio::spawn(async move {
            // Wait for log file to be created
            for _ in 0..30 {
                if std::path::Path::new(&log_file_path).exists() {
                    break;
                }
                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            }

            // Start monitoring the actual log file
            if let Err(e) = crate::utils::log_monitor::monitor_log_file(
                &log_file_path,
                monitor_config,
                restart_callback,
            )
            .await
            {
                println!("❌ Log monitoring error: {}", e);
            }
        });

        let pid = child.id();

        // Give it a moment to start
        tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;

        println!("✅ Devnet RPC node started in background");
        println!("🆔 Process ID: {}", pid);
        println!("🔗 RPC URL: http://localhost:{}", config.rpc_port);
        println!("📊 Use 'osvm rpc devnet --status' to check sync progress");

        Ok(DevnetRpcInfo {
            ledger_path: config.ledger_path,
            rpc_port: config.rpc_port,
            validator_pubkey,
            pid: Some(pid),
            network: "devnet".to_string(),
        })
    } else {
        // Run in foreground with output
        println!("📺 Starting in foreground mode (Press Ctrl+C to stop)...");

        let mut child = cmd
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to start agave-validator")?;

        let pid = child.id();
        println!("🆔 Process ID: {}", pid);
        println!("🔗 RPC URL: http://localhost:{}", config.rpc_port);

        // Monitor the actual log file that validator writes to
        let log_file_pattern = format!("agave-validator-{}.log", validator_pubkey);
        let log_file_path = format!("./{}", log_file_pattern);

        let monitor_config = LogMonitorConfig {
            auto_fix_enabled: true,
            restart_on_critical: false, // Don't auto-restart in foreground mode
            max_restart_attempts: 3,
            restart_cooldown_seconds: 300,
        };

        // Start monitoring the log file in background
        let monitor_handle = tokio::spawn(async move {
            // Wait for log file to be created
            for _ in 0..30 {
                if std::path::Path::new(&log_file_path).exists() {
                    break;
                }
                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            }

            // Start monitoring the actual log file
            if let Err(e) = crate::utils::log_monitor::monitor_log_file(
                &log_file_path,
                monitor_config,
                None, // No restart callback in foreground mode
            )
            .await
            {
                println!("❌ Log monitoring error: {}", e);
            }
        });

        // Don't wait for monitor task in foreground mode, let it run in parallel

        // Wait for process
        let status = child.wait().context("Failed to wait for agave-validator")?;

        if status.success() {
            println!("✅ Validator exited successfully");
        } else {
            println!("❌ Validator exited with error: {}", status);
        }

        Ok(DevnetRpcInfo {
            ledger_path: config.ledger_path,
            rpc_port: config.rpc_port,
            validator_pubkey,
            pid: Some(pid),
            network: "devnet".to_string(),
        })
    }
}

/// Stop the devnet RPC node
pub async fn stop_devnet_rpc() -> Result<()> {
    println!("🛑 Stopping devnet RPC node...");

    // Check if process is running
    let check_output = Command::new("pgrep")
        .arg("-f")
        .arg("agave-validator")
        .output()
        .context("Failed to execute pgrep")?;

    if !check_output.status.success() || check_output.stdout.is_empty() {
        println!("ℹ️  No devnet RPC node is currently running");
        return Ok(());
    }

    // Kill the validator process
    let output = Command::new("pkill")
        .arg("-f")
        .arg("agave-validator")
        .output()
        .context("Failed to execute pkill")?;

    if output.status.success() {
        println!("✅ Devnet RPC node stopped successfully");
        Ok(())
    } else {
        anyhow::bail!("Failed to stop agave-validator process")
    }
}

/// Check the status of devnet RPC node
pub async fn check_devnet_rpc_status() -> Result<DevnetRpcStatus> {
    // Check if agave-validator is running
    let output = Command::new("pgrep")
        .arg("-f")
        .arg("agave-validator")
        .output()
        .context("Failed to execute pgrep")?;

    if output.status.success() && !output.stdout.is_empty() {
        // Parse PID
        let pid_str = String::from_utf8_lossy(&output.stdout);
        let pid = pid_str.trim().parse::<u32>().ok();

        // Try to get RPC health and slot info
        let health_check = Command::new("curl")
            .arg("-s")
            .arg("-X")
            .arg("POST")
            .arg("-H")
            .arg("Content-Type: application/json")
            .arg("-d")
            .arg(r#"{"jsonrpc":"2.0","id":1,"method":"getHealth"}"#)
            .arg("http://127.0.0.1:8899")
            .output();

        let slot_check = Command::new("curl")
            .arg("-s")
            .arg("-X")
            .arg("POST")
            .arg("-H")
            .arg("Content-Type: application/json")
            .arg("-d")
            .arg(r#"{"jsonrpc":"2.0","id":1,"method":"getSlot"}"#)
            .arg("http://127.0.0.1:8899")
            .output();

        let rpc_healthy = health_check
            .map(|o| o.status.success() && String::from_utf8_lossy(&o.stdout).contains("ok"))
            .unwrap_or(false);

        let slot_height = slot_check.ok().and_then(|o| {
            if o.status.success() {
                let response: serde_json::Value = serde_json::from_slice(&o.stdout).ok()?;
                response["result"].as_u64()
            } else {
                None
            }
        });

        Ok(DevnetRpcStatus {
            running: true,
            pid,
            rpc_port: if rpc_healthy { Some(8899) } else { None },
            network: "devnet".to_string(),
            syncing: slot_height.is_some(),
            slot_height,
            uptime: None, // Would need to implement uptime tracking
        })
    } else {
        Ok(DevnetRpcStatus {
            running: false,
            pid: None,
            rpc_port: None,
            network: "devnet".to_string(),
            syncing: false,
            slot_height: None,
            uptime: None,
        })
    }
}

/// Extract pubkey from solana-keygen output
fn extract_pubkey_from_keygen_output(output: &str) -> String {
    for line in output.lines() {
        if line.starts_with("pubkey:") {
            return line.replace("pubkey:", "").trim().to_string();
        }
    }
    "unknown".to_string()
}

/// Start a devnet RPC node that syncs with real Solana devnet
pub async fn start_devnet_rpc(config: DevnetRpcConfig) -> Result<DevnetRpcInfo> {
    // Use the retry wrapper which includes automatic system tuning
    start_devnet_rpc_with_retry(config).await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_pubkey() {
        let output = r#"
Generating a new keypair
Wrote new keypair to test.json
==============================================================================
pubkey: 6CRfvjKzy3Px7kVtZeGmGtFyxD35KxZXTsfdV1f3VZ6b
==============================================================================
"#;
        let pubkey = extract_pubkey_from_keygen_output(output);
        assert_eq!(pubkey, "6CRfvjKzy3Px7kVtZeGmGtFyxD35KxZXTsfdV1f3VZ6b");
    }
}
