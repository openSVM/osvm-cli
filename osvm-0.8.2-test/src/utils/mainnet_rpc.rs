//! Mainnet RPC node deployment and management
//!
//! This module provides functionality to deploy and manage a mainnet RPC node
//! that syncs with the real Solana mainnet-beta blockchain.

use crate::utils::log_monitor::{monitor_log_file, LogMonitorConfig};
use crate::utils::osvm_logger::LogCategory;
use crate::{osvm_error, osvm_info};
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};

/// Configuration for mainnet RPC node deployment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MainnetRpcConfig {
    pub ledger_path: String,
    pub rpc_port: u16,
    pub gossip_port: u16,
    pub background: bool,
}

/// Information about a running mainnet RPC node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MainnetRpcInfo {
    pub ledger_path: String,
    pub rpc_port: u16,
    pub gossip_port: u16,
    pub pid: Option<u32>,
}

/// Status information for mainnet RPC node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MainnetRpcStatus {
    pub running: bool,
    pub network: String,
    pub pid: Option<u32>,
    pub rpc_port: Option<u16>,
    pub syncing: bool,
    pub slot_height: Option<u64>,
    pub behind_by: Option<u64>,
}

/// Known mainnet entrypoints
const MAINNET_ENTRYPOINTS: &[&str] = &[
    "entrypoint.mainnet-beta.solana.com:8001",
    "entrypoint2.mainnet-beta.solana.com:8001",
    "entrypoint3.mainnet-beta.solana.com:8001",
    "entrypoint4.mainnet-beta.solana.com:8001",
    "entrypoint5.mainnet-beta.solana.com:8001",
];

/// Start a mainnet RPC node
pub async fn start_mainnet_rpc(config: MainnetRpcConfig) -> Result<MainnetRpcInfo> {
    osvm_info!(
        LogCategory::Rpc,
        "start_mainnet_rpc",
        "Starting mainnet-beta RPC node"
    );
    println!("🚀 Starting Solana Mainnet-Beta RPC Node");
    println!("========================================");
    println!("⚠️  WARNING: Mainnet sync requires:");
    println!("   • 2-3TB of fast NVMe storage");
    println!("   • 256GB+ RAM (128GB minimum)");
    println!("   • High-speed internet (1Gbps+)");
    println!("   • Several days to weeks for initial sync");
    println!();

    // Create ledger directory
    fs::create_dir_all(&config.ledger_path).context("Failed to create ledger directory")?;

    // Check disk space
    let disk_space_output = Command::new("df")
        .arg("-BG")
        .arg(&config.ledger_path)
        .output()
        .context("Failed to check disk space")?;

    if disk_space_output.status.success() {
        let output_str = String::from_utf8_lossy(&disk_space_output.stdout);
        if let Some(line) = output_str.lines().nth(1) {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 4 {
                if let Ok(available_gb) = parts[3].trim_end_matches('G').parse::<u64>() {
                    if available_gb < 2000 {
                        println!("⚠️  WARNING: Only {}GB available. Mainnet requires 2000GB+ free space!", available_gb);
                        println!("💡 Consider using a dedicated disk for the ledger.");
                    } else {
                        println!("✅ Disk space: {}GB available", available_gb);
                    }
                }
            }
        }
    }

    // Build agave-validator command for mainnet RPC
    let mut cmd = Command::new("agave-validator");

    // Basic mainnet configuration
    cmd.arg("--identity")
        .arg("/home/larp/.config/solana/id.json")
        .arg("--ledger")
        .arg(&config.ledger_path)
        .arg("--rpc-port")
        .arg(config.rpc_port.to_string())
        .arg("--gossip-port")
        .arg(config.gossip_port.to_string())
        .arg("--dynamic-port-range")
        .arg("8002-8020")
        .arg("--log")
        .arg("-"); // Log to stdout

    // Add all mainnet entrypoints
    for entrypoint in MAINNET_ENTRYPOINTS {
        cmd.arg("--entrypoint").arg(entrypoint);
    }

    // Add known mainnet validators for bootstrap
    cmd.arg("--known-validator")
        .arg("7Np41oeYqPefeNQEHSv1UDhYrehxin3NStELsSKCT4K2") // Figment
        .arg("--known-validator")
        .arg("GdnSyH3YtwcxFvQrVVJMm1JhTS4QVX7MFsX56uJLUfiZ") // Figment
        .arg("--known-validator")
        .arg("DE1bawNcRJB9rVm3buyMVfr8mBEoyyu73NBovf2oXJsJ") // Bison Trails
        .arg("--known-validator")
        .arg("CakcnaRDHka2gXyfbEd2d3xsvkJkqsLw2akB3zsN1D2S") // Canonical
        .arg("--expected-genesis-hash")
        .arg("5eykt4UsFv8P8NJdTREpY1vzqKqZKvdpKuc147dw2N9d");

    // RPC node specific settings
    cmd.arg("--no-voting") // Don't vote, we're just an RPC node
        .arg("--enable-rpc-transaction-history") // Enable full history
        .arg("--enable-extended-tx-metadata-storage") // Store extended metadata
        .arg("--rpc-bind-address")
        .arg("0.0.0.0") // Bind to all interfaces
        .arg("--private-rpc") // Don't register with public RPC list
        .arg("--full-rpc-api") // Enable all RPC methods
        .arg("--account-index")
        .arg("program-id") // Index by program ID
        .arg("--account-index-exclude-key")
        .arg("TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA") // Exclude token accounts
        .arg("--account-index-exclude-key")
        .arg("TokenzQdBNbLqP5VEhdkAS6EPFLC1PHnBqCXEpPxuEb"); // Exclude token-2022 accounts

    // Performance settings
    cmd.arg("--limit-ledger-size")
        .arg("500000000") // Limit to ~250GB to start
        .arg("--accounts-db-cache-limit-mb")
        .arg("10000") // 10GB accounts cache
        .arg("--max-genesis-archive-unpacked-size")
        .arg("10737418240") // 10GB max genesis
        .arg("--no-port-check"); // Skip port reachability check for non-voting nodes

    println!("📋 Command: {:?}", cmd);
    println!();

    if config.background {
        // Run in background
        println!("🔧 Starting validator in background mode...");

        // Create log file
        let log_file_path = format!(
            "agave-validator-mainnet-{}.log",
            chrono::Utc::now().timestamp()
        );
        let log_file = fs::File::create(&log_file_path).context("Failed to create log file")?;

        let child = cmd
            .stdout(Stdio::from(log_file.try_clone()?))
            .stderr(Stdio::from(log_file))
            .spawn()
            .context("Failed to start agave-validator")?;

        let pid = child.id();

        println!("✅ Mainnet RPC node started in background");
        println!("🆔 Process ID: {}", pid);
        println!("📁 Ledger: {}", config.ledger_path);
        println!("🌐 RPC Port: {}", config.rpc_port);
        println!("📝 Log file: {}", log_file_path);
        println!();
        println!("⏳ Initial snapshot download may take several hours...");
        println!("🔧 Use 'osvm rpc-manager mainnet --status' to check sync progress");
        println!("📋 Use 'osvm rpc-manager mainnet --logs --follow' to watch logs");
        println!("🛑 Use 'osvm rpc-manager mainnet --stop' to stop the node");

        // Start log monitoring in background
        let log_monitor_config = LogMonitorConfig {
            auto_fix_enabled: true,
            restart_on_critical: false, // Don't auto-restart mainnet nodes
            max_restart_attempts: 0,
            restart_cooldown_seconds: 300,
        };

        let log_path_clone = log_file_path.clone();
        tokio::spawn(async move {
            if let Err(e) = monitor_log_file(&log_path_clone, log_monitor_config, None).await {
                osvm_error!(
                    LogCategory::Rpc,
                    "start_mainnet_rpc",
                    &format!("Log monitor error: {}", e)
                );
            }
        });

        Ok(MainnetRpcInfo {
            ledger_path: config.ledger_path,
            rpc_port: config.rpc_port,
            gossip_port: config.gossip_port,
            pid: Some(pid),
        })
    } else {
        // Run in foreground
        println!("🎯 Starting validator in foreground mode...");
        println!("ℹ️  Press Ctrl+C to stop");
        println!();

        let mut child = cmd
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to start agave-validator")?;

        // Stream stdout
        if let Some(stdout) = child.stdout.take() {
            tokio::task::spawn_blocking(move || {
                let reader = BufReader::new(stdout);
                for line in reader.lines() {
                    if let Ok(line) = line {
                        println!("{}", line);
                    }
                }
            });
        }

        // Stream stderr
        if let Some(stderr) = child.stderr.take() {
            tokio::task::spawn_blocking(move || {
                let reader = BufReader::new(stderr);
                for line in reader.lines() {
                    if let Ok(line) = line {
                        eprintln!("{}", line);
                    }
                }
            });
        }

        // Wait for process to complete
        let status = child.wait().context("Failed to wait for agave-validator")?;

        if status.success() {
            println!("✅ Mainnet RPC node exited normally");
        } else {
            eprintln!("❌ Mainnet RPC node exited with error: {}", status);
        }

        Ok(MainnetRpcInfo {
            ledger_path: config.ledger_path,
            rpc_port: config.rpc_port,
            gossip_port: config.gossip_port,
            pid: None,
        })
    }
}

/// Stop the mainnet RPC node
pub async fn stop_mainnet_rpc() -> Result<()> {
    osvm_info!(
        LogCategory::Rpc,
        "stop_mainnet_rpc",
        "Stopping mainnet RPC node"
    );
    println!("🛑 Stopping mainnet RPC node...");

    // Find agave-validator process
    let pgrep_output = Command::new("pgrep")
        .arg("-f")
        .arg("agave-validator.*mainnet")
        .output()
        .context("Failed to execute pgrep")?;

    if !pgrep_output.status.success() || pgrep_output.stdout.is_empty() {
        println!("⚠️  No mainnet RPC node process found");
        return Ok(());
    }

    // Parse PIDs and kill processes
    let pids = String::from_utf8_lossy(&pgrep_output.stdout);
    for pid_str in pids.lines() {
        if let Ok(pid) = pid_str.trim().parse::<u32>() {
            println!("🔧 Stopping process {}", pid);
            let _ = Command::new("kill")
                .arg("-TERM")
                .arg(pid.to_string())
                .output();
        }
    }

    // Wait a moment for graceful shutdown
    tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;

    // Force kill if still running
    let check_output = Command::new("pgrep")
        .arg("-f")
        .arg("agave-validator.*mainnet")
        .output()?;

    if check_output.status.success() && !check_output.stdout.is_empty() {
        println!("⚠️  Process still running, force killing...");
        let _ = Command::new("pkill")
            .arg("-9")
            .arg("-f")
            .arg("agave-validator.*mainnet")
            .output();
    }

    println!("✅ Mainnet RPC node stopped successfully");
    Ok(())
}

/// Check the status of mainnet RPC node
pub async fn check_mainnet_rpc_status() -> Result<MainnetRpcStatus> {
    // Check if agave-validator is running
    let pgrep_output = Command::new("pgrep")
        .arg("-f")
        .arg("agave-validator.*mainnet")
        .output()
        .context("Failed to execute pgrep")?;

    if !pgrep_output.status.success() || pgrep_output.stdout.is_empty() {
        return Ok(MainnetRpcStatus {
            running: false,
            network: "mainnet-beta".to_string(),
            pid: None,
            rpc_port: None,
            syncing: false,
            slot_height: None,
            behind_by: None,
        });
    }

    // Parse PID
    let pid_str = String::from_utf8_lossy(&pgrep_output.stdout);
    let pid = pid_str
        .lines()
        .next()
        .and_then(|line| line.trim().parse::<u32>().ok());

    // Try to get RPC status
    let rpc_check = Command::new("curl")
        .arg("-s")
        .arg("-X")
        .arg("POST")
        .arg("-H")
        .arg("Content-Type: application/json")
        .arg("-d")
        .arg(r#"{"jsonrpc":"2.0","id":1,"method":"getHealth"}"#)
        .arg("http://localhost:8899")
        .output();

    let rpc_healthy = rpc_check
        .map(|o| o.status.success() && String::from_utf8_lossy(&o.stdout).contains("\"ok\""))
        .unwrap_or(false);

    // Get slot height if RPC is healthy
    let mut slot_height = None;
    let mut behind_by = None;

    if rpc_healthy {
        // Get current slot
        let slot_output = Command::new("curl")
            .arg("-s")
            .arg("-X")
            .arg("POST")
            .arg("-H")
            .arg("Content-Type: application/json")
            .arg("-d")
            .arg(r#"{"jsonrpc":"2.0","id":1,"method":"getSlot"}"#)
            .arg("http://localhost:8899")
            .output();

        if let Ok(output) = slot_output {
            if output.status.success() {
                let response = String::from_utf8_lossy(&output.stdout);
                // Parse JSON to get slot number
                if let Some(start) = response.find("\"result\":") {
                    let result_part = &response[start + 9..];
                    if let Some(end) = result_part.find(|c: char| !c.is_numeric()) {
                        if let Ok(slot) = result_part[..end].parse::<u64>() {
                            slot_height = Some(slot);
                        }
                    }
                }
            }
        }

        // Check how far behind we are
        if slot_height.is_some() {
            // Get slot from public RPC
            let public_slot_output = Command::new("curl")
                .arg("-s")
                .arg("-X")
                .arg("POST")
                .arg("-H")
                .arg("Content-Type: application/json")
                .arg("-d")
                .arg(r#"{"jsonrpc":"2.0","id":1,"method":"getSlot"}"#)
                .arg("https://api.mainnet-beta.solana.com")
                .output();

            if let Ok(output) = public_slot_output {
                if output.status.success() {
                    let response = String::from_utf8_lossy(&output.stdout);
                    if let Some(start) = response.find("\"result\":") {
                        let result_part = &response[start + 9..];
                        if let Some(end) = result_part.find(|c: char| !c.is_numeric()) {
                            if let Ok(public_slot) = result_part[..end].parse::<u64>() {
                                if let Some(local_slot) = slot_height {
                                    behind_by = Some(public_slot.saturating_sub(local_slot));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(MainnetRpcStatus {
        running: true,
        network: "mainnet-beta".to_string(),
        pid,
        rpc_port: if rpc_healthy { Some(8899) } else { None },
        syncing: slot_height.is_some(),
        slot_height,
        behind_by,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mainnet_rpc_config() {
        let config = MainnetRpcConfig {
            ledger_path: "mainnet-ledger".to_string(),
            rpc_port: 8899,
            gossip_port: 8001,
            background: true,
        };

        assert_eq!(config.ledger_path, "mainnet-ledger");
        assert_eq!(config.rpc_port, 8899);
        assert_eq!(config.gossip_port, 8001);
        assert!(config.background);
    }
}
