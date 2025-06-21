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

/// Start a local RPC node
pub async fn start_local_rpc(config: LocalRpcConfig) -> Result<LocalRpcInfo> {
    // For now, we only support Solana
    if config.svm != "solana" {
        anyhow::bail!(
            "Currently only Solana local RPC is supported. {} support coming soon!",
            config.svm
        );
    }

    // Ensure ledger directory exists
    fs::create_dir_all(&config.ledger_path).context("Failed to create ledger directory")?;

    // Build the command
    let mut cmd = Command::new("solana-test-validator");

    if config.reset {
        cmd.arg("--reset");
    }

    cmd.arg("--ledger").arg(&config.ledger_path);
    cmd.arg("--rpc-port").arg(config.port.to_string());

    if let Some(faucet_port) = config.faucet_port {
        cmd.arg("--faucet-port").arg(faucet_port.to_string());
    }

    // Set up process handling
    if config.background {
        // Run in background
        cmd.stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .context("Failed to start solana-test-validator")?;
    } else {
        // Run in foreground with output
        let mut child = cmd
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to start solana-test-validator")?;

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
        child
            .wait()
            .context("Failed to wait for solana-test-validator")?;
    }

    Ok(LocalRpcInfo {
        svm: config.svm,
        port: config.port,
        faucet_port: config.faucet_port,
        ledger_path: config.ledger_path,
        pid: None, // Would need to implement PID tracking
    })
}

/// Stop the local RPC node
pub async fn stop_local_rpc() -> Result<()> {
    // First check if process is running
    let check_output = Command::new("pgrep")
        .arg("-f")
        .arg("solana-test-validator")
        .output()
        .context("Failed to execute pgrep")?;

    if !check_output.status.success() || check_output.stdout.is_empty() {
        // No process running, this is not an error
        return Ok(());
    }

    // Find and kill solana-test-validator process
    let output = Command::new("pkill")
        .arg("-f")
        .arg("solana-test-validator")
        .output()
        .context("Failed to execute pkill")?;

    if output.status.success() {
        Ok(())
    } else {
        anyhow::bail!("Failed to stop solana-test-validator process")
    }
}

/// Check the status of local RPC node
pub async fn check_local_rpc_status() -> Result<LocalRpcStatus> {
    // Check if solana-test-validator is running
    let output = Command::new("pgrep")
        .arg("-f")
        .arg("solana-test-validator")
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

        let running = health_check
            .map(|o| o.status.success() && String::from_utf8_lossy(&o.stdout).contains("ok"))
            .unwrap_or(false);

        Ok(LocalRpcStatus {
            running,
            pid,
            port: if running { Some(8899) } else { None },
            network: if running {
                Some("localnet".to_string())
            } else {
                None
            },
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
            ledger_path: "/tmp/test-ledger".to_string(),
            reset: true,
            background: false,
        };

        assert_eq!(config.svm, "solana");
        assert_eq!(config.port, 8899);
        assert_eq!(config.faucet_port, Some(9900));
    }
}
