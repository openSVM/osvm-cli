//! Unikraft Tool Executor - Main Entry Point
//!
//! This unikernel runs inside Unikraft and provides an isolated environment
//! for executing MCP tools. It listens on vsock port 5252 for tool execution
//! requests from the host.

mod protocol;
mod proxy;
mod types;

use anyhow::{Context, Result};
use protocol::{read_json_message, write_json_message};
use proxy::handle_tool_request;
use tokio_vsock::VsockListener;
use types::{ToolExecutionRequest, ToolExecutionResponse};

/// Vsock port for MCP communication (must match host-side)
const VSOCK_MCP_PORT: u32 = 5252;

/// Vsock CID for any (equivalent to VMADDR_CID_ANY)
const VMADDR_CID_ANY: u32 = 0xFFFFFFFF;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
        .init();

    log::info!("Unikraft Tool Executor starting");
    log::info!("Version: {}", env!("CARGO_PKG_VERSION"));

    // Get configuration from environment variables
    let vsock_cid = std::env::var("UNIKRAFT_VSOCK_CID")
        .ok()
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(0);
    
    let server_id = std::env::var("UNIKRAFT_TOOL_SERVER")
        .unwrap_or_else(|_| "unknown".to_string());

    log::info!("Configuration:");
    let cid_display = if vsock_cid == 0 { 
        "auto".to_string() 
    } else { 
        vsock_cid.to_string() 
    };
    log::info!("  Vsock CID: {}", cid_display);
    log::info!("  Server ID: {}", server_id);

    // Bind vsock listener on port 5252
    let mut listener = VsockListener::bind(VMADDR_CID_ANY, VSOCK_MCP_PORT)
        .context("Failed to bind vsock listener. Ensure virtio-vsock kernel module is loaded.")?;

    log::info!("Listening on vsock port {}", VSOCK_MCP_PORT);
    log::info!("Ready to accept connections from host");

    // Accept connections and handle requests
    loop {
        match listener.accept().await {
            Ok((stream, addr)) => {
                log::info!("Accepted connection from CID {}", addr.cid());

                // Spawn a task to handle this connection
                tokio::spawn(async move {
                    if let Err(e) = handle_connection(stream).await {
                        log::error!("Connection handler error: {:#}", e);
                    }
                });
            }
            Err(e) => {
                log::error!("Failed to accept connection: {}", e);
                // Continue accepting other connections
            }
        }
    }
}

/// Handle a single vsock connection
///
/// This function receives tool execution requests, executes them,
/// and sends back responses.
async fn handle_connection(mut stream: tokio_vsock::VsockStream) -> Result<()> {
    log::debug!("Handling new connection");

    // Process requests in a loop until the connection closes
    loop {
        // Read request from host
        let request: ToolExecutionRequest = match read_json_message(&mut stream).await {
            Ok(req) => req,
            Err(e) => {
                // Check if this is a normal connection close
                if e.to_string().contains("unexpected end of file")
                    || e.to_string().contains("connection reset")
                {
                    log::info!("Connection closed by host");
                    return Ok(());
                }
                
                log::error!("Failed to read request: {:#}", e);
                return Err(e);
            }
        };

        log::debug!(
            "Received request {} for tool '{}'",
            request.id,
            request.method
        );

        // Handle the request
        let response: ToolExecutionResponse = handle_tool_request(request).await;

        // Send response back to host
        write_json_message(&mut stream, &response)
            .await
            .context("Failed to send response")?;

        log::debug!("Sent response {}", response.id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vsock_port_constant() {
        assert_eq!(VSOCK_MCP_PORT, 5252);
    }
}
