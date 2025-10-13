//! Local RPC node deployment and management
//!
//! This module provides functionality to deploy and manage RPC nodes
//! on localhost for development and testing purposes.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};
use tokio::task;

/// Generate default OSVM ledger path for a given network and SVM
pub fn get_default_ledger_path(network: &str, svm: &str) -> String {
    let home_dir = dirs::home_dir().unwrap_or_default();
    let osvm_ledgers_dir = home_dir.join(".config/osvm/ledgers");

    // Create network-specific directory
    let ledger_path = match network.to_lowercase().as_str() {
        "mainnet" | "mainnet-beta" => osvm_ledgers_dir.join("mainnet"),
        "testnet" => osvm_ledgers_dir.join("testnet"),
        "devnet" => osvm_ledgers_dir.join("devnet"),
        "develop" => osvm_ledgers_dir.join("develop"),
        "custom" => osvm_ledgers_dir.join("custom"),
        _ => {
            // For custom SVM networks, use the SVM name
            if svm != "solana" {
                osvm_ledgers_dir.join(format!("{}_network", svm))
            } else {
                osvm_ledgers_dir.join(network)
            }
        }
    };

    ledger_path.to_string_lossy().to_string()
}

/// Configuration for local RPC node deployment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LocalRpcConfig {
    pub svm: String,
    pub network: String,
    pub port: u16,
    pub faucet_port: Option<u16>,
    pub ledger_path: String,
    pub reset: bool,
    pub background: bool,
}

/// Information about a running local RPC node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LocalRpcInfo {
    pub svm: String,
    pub port: u16,
    pub faucet_port: Option<u16>,
    pub ledger_path: String,
    pub pid: Option<u32>,
}

/// Status information for local RPC node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LocalRpcStatus {
    pub running: bool,
    pub pid: Option<u32>,
    pub port: Option<u16>,
    pub network: Option<String>,
    pub uptime: Option<String>,
}

/// Start a local RPC node that syncs with real blockchain
pub async fn start_local_rpc(config: LocalRpcConfig) -> Result<LocalRpcInfo> {
    // For now, we only support Solana
    if config.svm != "solana" {
        anyhow::bail!(
            "Currently only Solana local RPC is supported. {} support coming soon!",
            config.svm
        );
    }

    println!("🚀 Starting Local RPC Node with Real Blockchain Sync");
    println!("📋 Network: {}", config.network);
    println!("📁 Ledger: {}", config.ledger_path);
    println!("🔗 RPC Port: {}", config.port);

    // Ensure ledger directory exists
    fs::create_dir_all(&config.ledger_path).context("Failed to create ledger directory")?;

    // Get OSVM validator identity
    let identity_path = dirs::home_dir()
        .unwrap_or_default()
        .join(".config/osvm/validator.json");

    if !identity_path.exists() {
        println!("🔑 Creating OSVM validator identity...");
        let output = Command::new("solana-keygen")
            .arg("new")
            .arg("--no-passphrase")
            .arg("--outfile")
            .arg(&identity_path)
            .output()
            .context("Failed to create validator identity")?;

        if !output.status.success() {
            anyhow::bail!(
                "Failed to create validator identity: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
    }

    // Build the agave-validator command for real blockchain sync
    let mut cmd = Command::new("agave-validator");

    // Basic configuration
    cmd.arg("--identity").arg(&identity_path);
    cmd.arg("--ledger").arg(&config.ledger_path);
    cmd.arg("--rpc-port").arg(config.port.to_string());
    cmd.arg("--gossip-port").arg("8001");
    cmd.arg("--dynamic-port-range").arg("8002-8020");

    // Network-specific configuration
    match config.network.to_lowercase().as_str() {
        "devnet" => {
            cmd.arg("--entrypoint")
                .arg("entrypoint.devnet.solana.com:8001");
            cmd.arg("--entrypoint")
                .arg("entrypoint2.devnet.solana.com:8001");
            cmd.arg("--known-validator")
                .arg("dv1ZAGvdsz5hHLwWXsVnM94hWf1pjbKVau1QVkaMJ92");
            cmd.arg("--known-validator")
                .arg("dv2eQHeP4RFrJZ6UeiZWoc3XTtmtZCUKxxCApCDcRNV");
            cmd.arg("--expected-genesis-hash")
                .arg("EtWTRABZaYq6iMfeYKouRu166VU2xqa1wcaWoxPkrZBG");
        }
        "testnet" => {
            cmd.arg("--entrypoint")
                .arg("entrypoint.testnet.solana.com:8001");
            cmd.arg("--entrypoint")
                .arg("entrypoint2.testnet.solana.com:8001");
            cmd.arg("--known-validator")
                .arg("5D1fNXzvv5NjV1ysLjirC4WY92RNsVH18vjmcszZd8on");
            cmd.arg("--known-validator")
                .arg("7XSY3MrYnK8vq693VZp2hqCqKjBZlhTNvKcoYE1qpmSC");
            cmd.arg("--expected-genesis-hash")
                .arg("4uhcVJyU9pJkvQyS88uRDiswHXSCkY3zQawwpjk2NsNY");
        }
        "mainnet" | "mainnet-beta" => {
            cmd.arg("--entrypoint")
                .arg("entrypoint.mainnet-beta.solana.com:8001");
            cmd.arg("--entrypoint")
                .arg("entrypoint2.mainnet-beta.solana.com:8001");
            cmd.arg("--known-validator")
                .arg("7Np41oeYqPefeNQEHSv1UDhYrehxin3NStELsSKCT4K2");
            cmd.arg("--known-validator")
                .arg("GdnSyH3YtwcxFvQrVVJMm1JhTS4QVX7MFsX56uJLUfiZ");
            cmd.arg("--expected-genesis-hash")
                .arg("5eykt4UsFv8P8NJdTREpY1vzqKqZKvdpKuc147dw2N9d");
        }
        _ => {
            anyhow::bail!(
                "Unsupported network: {}. Use devnet, testnet, or mainnet",
                config.network
            );
        }
    }

    // RPC configuration for full functionality
    cmd.arg("--no-voting"); // RPC-only mode
    cmd.arg("--enable-rpc-transaction-history");
    cmd.arg("--full-rpc-api");
    cmd.arg("--rpc-bind-address").arg("0.0.0.0");
    cmd.arg("--limit-ledger-size").arg("50000000"); // 50GB limit
    cmd.arg("--wal-recovery-mode")
        .arg("skip_any_corrupted_record");
    cmd.arg("--allow-private-addr");
    cmd.arg("--no-port-check"); // Skip UDP port reachability checks for local development

    if config.reset {
        // For reset, we need to clear the ledger directory
        if std::path::Path::new(&config.ledger_path).exists() {
            println!("🔄 Resetting ledger directory...");
            fs::remove_dir_all(&config.ledger_path).context("Failed to reset ledger")?;
            fs::create_dir_all(&config.ledger_path)
                .context("Failed to recreate ledger directory")?;
        }
    }

    println!("📡 Connecting to {} network...", config.network);
    println!("⏳ This will download fresh snapshots and sync with real blockchain data");
    println!("💡 Initial snapshot download may take several minutes to hours depending on network");

    // Set up process handling
    if config.background {
        // Run in background
        println!("🌙 Starting in background mode...");
        let child = cmd
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .context("Failed to start agave-validator")?;

        let pid = child.id();
        println!("✅ Local RPC node started in background");
        println!("🆔 Process ID: {}", pid);
        println!("🔗 RPC URL: http://localhost:{}", config.port);
        println!("📊 Use 'osvm rpc local --status' to check sync progress");
        println!("🛑 Use 'osvm rpc local --stop' to stop the node");

        Ok(LocalRpcInfo {
            svm: config.svm,
            port: config.port,
            faucet_port: config.faucet_port,
            ledger_path: config.ledger_path,
            pid: Some(pid),
        })
    } else {
        // Run in foreground with output
        println!("📺 Starting in foreground mode (Press Ctrl+C to stop)...");
        let mut child = cmd
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to start agave-validator")?;

        // Stream output
        if let Some(stdout) = child.stdout.take() {
            let reader = BufReader::new(stdout);
            task::spawn_blocking(move || {
                for line in reader.lines() {
                    if let Ok(line) = line {
                        println!("{}", line);
                    }
                }
            });
        }

        // Wait for process
        let status = child.wait().context("Failed to wait for agave-validator")?;

        if status.success() {
            println!("✅ Local RPC node exited successfully");
        } else {
            println!("❌ Local RPC node exited with error: {}", status);
        }

        Ok(LocalRpcInfo {
            svm: config.svm,
            port: config.port,
            faucet_port: config.faucet_port,
            ledger_path: config.ledger_path,
            pid: None,
        })
    }
}

/// Stop the local RPC node
pub async fn stop_local_rpc() -> Result<()> {
    println!("🛑 Stopping local RPC node...");

    // First check if agave-validator process is running
    let check_output = Command::new("pgrep")
        .arg("-f")
        .arg("agave-validator.*osvm/validator.json")
        .output()
        .context("Failed to execute pgrep")?;

    if !check_output.status.success() || check_output.stdout.is_empty() {
        println!("ℹ️  No local RPC node is currently running");
        return Ok(());
    }

    // Find and kill agave-validator process
    let output = Command::new("pkill")
        .arg("-f")
        .arg("agave-validator.*osvm/validator.json")
        .output()
        .context("Failed to execute pkill")?;

    if output.status.success() {
        println!("✅ Local RPC node stopped successfully");
        Ok(())
    } else {
        anyhow::bail!("Failed to stop agave-validator process")
    }
}

/// Check the status of local RPC node
pub async fn check_local_rpc_status() -> Result<LocalRpcStatus> {
    // Check if agave-validator with OSVM identity is running
    let output = Command::new("pgrep")
        .arg("-f")
        .arg("agave-validator.*osvm/validator.json")
        .output()
        .context("Failed to execute pgrep")?;

    if output.status.success() && !output.stdout.is_empty() {
        // Parse PID
        let pid_str = String::from_utf8_lossy(&output.stdout);
        let pid = pid_str.trim().parse::<u32>().ok();

        // Try to get RPC health
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

        let rpc_healthy = health_check
            .map(|o| o.status.success() && String::from_utf8_lossy(&o.stdout).contains("ok"))
            .unwrap_or(false);

        // Try to get slot info to determine network sync status
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

        let network = if rpc_healthy {
            // Try to determine which network by checking slot progression
            if slot_check.is_ok() {
                Some("blockchain-sync".to_string())
            } else {
                Some("starting".to_string())
            }
        } else {
            None
        };

        Ok(LocalRpcStatus {
            running: true,
            pid,
            port: if rpc_healthy { Some(8899) } else { None },
            network,
            uptime: None, // Would need to implement uptime tracking
        })
    } else {
        Ok(LocalRpcStatus {
            running: false,
            pid: None,
            port: None,
            network: None,
            uptime: None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_local_rpc_config() {
        let config = LocalRpcConfig {
            svm: "solana".to_string(),
            network: "localnet".to_string(),
            port: 8899,
            faucet_port: Some(9900),
            ledger_path: std::env::temp_dir()
                .join("test-ledger")
                .to_str()
                .unwrap_or("./test-ledger")
                .to_string(),
            reset: true,
            background: false,
        };

        assert_eq!(config.svm, "solana");
        assert_eq!(config.port, 8899);
        assert_eq!(config.faucet_port, Some(9900));
    }
}
