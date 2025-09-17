//! Devnet RPC proxy
//!
//! This module provides a local RPC proxy that forwards requests to Solana devnet

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::process::{Command, Stdio};

/// Configuration for devnet proxy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DevnetProxyConfig {
    pub local_port: u16,
    pub background: bool,
}

/// Information about running proxy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DevnetProxyInfo {
    pub local_port: u16,
    pub pid: Option<u32>,
    pub devnet_url: String,
}

/// Start a local RPC proxy to devnet
pub async fn start_devnet_proxy(config: DevnetProxyConfig) -> Result<DevnetProxyInfo> {
    println!("🚀 Starting local devnet RPC proxy");
    println!("🌐 Proxying to: https://api.devnet.solana.com");
    println!("🔗 Local endpoint: http://localhost:{}", config.local_port);

    // Use socat to create a simple TCP proxy
    let mut cmd = Command::new("socat");
    cmd.arg(format!("TCP-LISTEN:{},fork,reuseaddr", config.local_port));
    cmd.arg("PROXY:api.devnet.solana.com:443,proxyport=443");

    if config.background {
        let child = cmd
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .context("Failed to start proxy. Make sure 'socat' is installed")?;

        let pid = child.id();

        println!("✅ Devnet proxy started in background");
        println!("🆔 Process ID: {}", pid);
        println!("🔗 Local RPC URL: http://localhost:{}", config.local_port);

        Ok(DevnetProxyInfo {
            local_port: config.local_port,
            pid: Some(pid),
            devnet_url: "https://api.devnet.solana.com".to_string(),
        })
    } else {
        println!("📺 Starting in foreground mode (Press Ctrl+C to stop)...");

        let mut child = cmd
            .spawn()
            .context("Failed to start proxy. Make sure 'socat' is installed")?;

        let pid = child.id();
        println!("🆔 Process ID: {}", pid);

        child.wait()?;

        Ok(DevnetProxyInfo {
            local_port: config.local_port,
            pid: Some(pid),
            devnet_url: "https://api.devnet.solana.com".to_string(),
        })
    }
}

/// Stop the devnet proxy
pub async fn stop_devnet_proxy() -> Result<()> {
    println!("🛑 Stopping devnet proxy...");

    let output = Command::new("pkill")
        .arg("-f")
        .arg("socat.*TCP-LISTEN")
        .output()
        .context("Failed to execute pkill")?;

    if output.status.success() {
        println!("✅ Devnet proxy stopped successfully");
        Ok(())
    } else {
        println!("ℹ️  No devnet proxy is currently running");
        Ok(())
    }
}
