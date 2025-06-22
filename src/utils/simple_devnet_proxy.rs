//! Simple Devnet RPC proxy using curl
//!
//! This module provides a very simple local proxy to Solana devnet RPC

use anyhow::{Result, Context};
use std::process::{Command, Stdio};
use serde::{Deserialize, Serialize};
use tokio::process::Command as TokioCommand;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use std::net::TcpListener;
use std::sync::Arc;

/// Start a simple HTTP proxy server for devnet RPC
pub async fn start_simple_devnet_proxy(port: u16) -> Result<()> {
    println!("🚀 Starting simple devnet RPC proxy on port {}", port);
    println!("🔗 Proxying to: https://api.devnet.solana.com");
    println!("📡 Local RPC URL: http://localhost:{}", port);
    
    // Bind to the local port
    let listener = TcpListener::bind(format!("127.0.0.1:{}", port))
        .context("Failed to bind to port")?;
    
    println!("✅ Proxy server listening on port {}", port);
    println!("🛑 Press Ctrl+C to stop");
    
    // Accept connections in a loop
    for stream in listener.incoming() {
        if let Ok(mut stream) = stream {
            tokio::spawn(async move {
                let mut buffer = [0; 4096];
                
                // Read the request
                if let Ok(n) = tokio::io::AsyncReadExt::read(&mut stream, &mut buffer).await {
                    let request = String::from_utf8_lossy(&buffer[..n]);
                    
                    // Extract the body (simple parsing for JSON-RPC)
                    if let Some(body_start) = request.find("\r\n\r\n") {
                        let body = &request[body_start + 4..];
                        
                        // Forward to devnet using curl
                        let output = TokioCommand::new("curl")
                            .arg("-s")
                            .arg("-X")
                            .arg("POST")
                            .arg("-H")
                            .arg("Content-Type: application/json")
                            .arg("-d")
                            .arg(body)
                            .arg("https://api.devnet.solana.com")
                            .output()
                            .await;
                        
                        if let Ok(output) = output {
                            if output.status.success() {
                                let response_body = String::from_utf8_lossy(&output.stdout);
                                let response = format!(
                                    "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\n\r\n{}",
                                    response_body.len(),
                                    response_body
                                );
                                
                                let _ = tokio::io::AsyncWriteExt::write_all(&mut stream, response.as_bytes()).await;
                            }
                        }
                    }
                }
            });
        }
    }
    
    Ok(())
}

/// Test if devnet RPC is accessible
pub async fn test_devnet_connection() -> Result<bool> {
    println!("🔍 Testing connection to Solana devnet...");
    
    let output = Command::new("curl")
        .arg("-s")
        .arg("-X")
        .arg("POST")
        .arg("-H")
        .arg("Content-Type: application/json")
        .arg("-d")
        .arg(r#"{"jsonrpc":"2.0","id":1,"method":"getHealth"}"#)
        .arg("https://api.devnet.solana.com")
        .output()
        .context("Failed to execute curl")?;
    
    if output.status.success() {
        let response = String::from_utf8_lossy(&output.stdout);
        if response.contains("\"result\":\"ok\"") {
            println!("✅ Successfully connected to Solana devnet");
            return Ok(true);
        }
    }
    
    println!("❌ Failed to connect to Solana devnet");
    Ok(false)
}
