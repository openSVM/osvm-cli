//! MCP Vsock Wrapper - Guest-side bridge for MCP servers in microVMs
//!
//! This binary runs inside Firecracker microVM guests and bridges vsock
//! communication from the host to MCP server processes via stdio.
//!
//! Architecture:
//! ```
//! Host (OSVM) ──[vsock]──> Guest Wrapper ──[stdio]──> MCP Server
//!                          (this program)           (Node.js/Python)
//! ```
//!
//! Protocol:
//! - Vsock: Length-prefixed (4-byte LE) + JSON-RPC payload
//! - Stdio: Line-delimited JSON-RPC messages

use anyhow::{Context, Result};
use log::{debug, error, info, warn};
use std::process::Stdio;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::process::Command;
use tokio_vsock::{VsockAddr, VsockListener, VsockStream, VMADDR_CID_ANY};

/// Vsock port for MCP communication (must match host-side)
const VSOCK_MCP_PORT: u32 = 5252;

/// Maximum message size (10 MB)
const MAX_MESSAGE_SIZE: usize = 10 * 1024 * 1024;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    // Get configuration from environment variables
    let server_id = std::env::var("OSVM_MCP_SERVER_ID").unwrap_or_else(|_| "default".to_string());

    let server_cmd = std::env::var("OSVM_MCP_SERVER_CMD").context(
        "OSVM_MCP_SERVER_CMD not set. This variable must contain the command to run the MCP server.",
    )?;

    info!("MCP Vsock Wrapper starting");
    info!("Server ID: {}", server_id);
    info!("Server command: {}", server_cmd);

    // Bind vsock listener on port 5252
    let addr = VsockAddr::new(VMADDR_CID_ANY, VSOCK_MCP_PORT);
    let mut listener = VsockListener::bind(addr)
        .context("Failed to bind vsock listener. Ensure virtio-vsock kernel module is loaded.")?;

    info!("Listening on vsock port {}", VSOCK_MCP_PORT);
    info!("Ready to accept connections from host");

    // Accept connections and spawn handlers
    loop {
        match listener.accept().await {
            Ok((stream, addr)) => {
                info!("Accepted connection from CID {}", addr.cid());

                let cmd = server_cmd.clone();
                let sid = server_id.clone();

                tokio::spawn(async move {
                    if let Err(e) = handle_connection(stream, cmd, sid).await {
                        error!("Connection handler error: {:#}", e);
                    }
                });
            }
            Err(e) => {
                error!("Failed to accept connection: {}", e);
                // Continue accepting other connections
            }
        }
    }
}

/// Handle a single vsock connection by spawning an MCP server and bridging I/O
async fn handle_connection(stream: VsockStream, server_cmd: String, server_id: String) -> Result<()> {
    info!("[{}] Starting MCP server process", server_id);

    // Split vsock stream for concurrent read/write
    let (vsock_reader, vsock_writer) = tokio::io::split(stream);
    let mut vsock_reader = BufReader::new(vsock_reader);
    let mut vsock_writer = vsock_writer;

    // Spawn MCP server process
    let mut child = Command::new("sh")
        .arg("-c")
        .arg(&server_cmd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true)
        .spawn()
        .context("Failed to spawn MCP server process")?;

    info!("[{}] MCP server process spawned (PID: {})", server_id, child.id().unwrap_or(0));

    // Get stdio handles
    let mut mcp_stdin = child.stdin.take().context("Failed to get MCP server stdin")?;
    let mcp_stdout = child.stdout.take().context("Failed to get MCP server stdout")?;
    let mcp_stderr = child.stderr.take().context("Failed to get MCP server stderr")?;

    let mut mcp_stdout_reader = BufReader::new(mcp_stdout);
    let mut mcp_stderr_reader = BufReader::new(mcp_stderr);

    // Spawn task to log stderr
    let sid_stderr = server_id.clone();
    tokio::spawn(async move {
        let mut line = String::new();
        loop {
            line.clear();
            match mcp_stderr_reader.read_line(&mut line).await {
                Ok(0) => break, // EOF
                Ok(_) => {
                    let trimmed = line.trim();
                    if !trimmed.is_empty() {
                        warn!("[{}] stderr: {}", sid_stderr, trimmed);
                    }
                }
                Err(e) => {
                    error!("[{}] Error reading stderr: {}", sid_stderr, e);
                    break;
                }
            }
        }
    });

    info!("[{}] Starting bidirectional forwarding loop", server_id);

    // Bidirectional forwarding loop
    let result = tokio::select! {
        // Host -> MCP Server
        res = forward_host_to_mcp(&mut vsock_reader, &mut mcp_stdin, &server_id) => res,

        // MCP Server -> Host
        res = forward_mcp_to_host(&mut mcp_stdout_reader, &mut vsock_writer, &server_id) => res,

        // Wait for process exit
        res = child.wait() => {
            match res {
                Ok(status) => {
                    if status.success() {
                        info!("[{}] MCP server process exited normally", server_id);
                        Ok(())
                    } else {
                        Err(anyhow::anyhow!("[{}] MCP server process exited with status: {}", server_id, status))
                    }
                }
                Err(e) => {
                    Err(anyhow::anyhow!("[{}] Failed to wait for MCP server process: {}", server_id, e))
                }
            }
        }
    };

    // Cleanup
    info!("[{}] Connection closing, cleaning up", server_id);

    // Try to kill the child process if it's still running
    let _ = child.kill().await;

    result
}

/// Forward messages from host (vsock) to MCP server (stdin)
async fn forward_host_to_mcp(
    vsock_reader: &mut BufReader<tokio::io::ReadHalf<VsockStream>>,
    mcp_stdin: &mut tokio::process::ChildStdin,
    server_id: &str,
) -> Result<()> {
    loop {
        // Read length-prefixed message from vsock
        let message = read_length_prefixed_message(vsock_reader, server_id).await?;

        debug!(
            "[{}] Received {} bytes from host, forwarding to MCP server",
            server_id,
            message.len()
        );

        // Forward to MCP server via stdin (line-delimited)
        mcp_stdin.write_all(&message).await.context("Failed to write to MCP server stdin")?;
        mcp_stdin.write_all(b"\n").await.context("Failed to write newline to MCP server stdin")?;
        mcp_stdin.flush().await.context("Failed to flush MCP server stdin")?;

        debug!("[{}] Message forwarded to MCP server", server_id);
    }
}

/// Forward messages from MCP server (stdout) to host (vsock)
async fn forward_mcp_to_host(
    mcp_stdout: &mut BufReader<tokio::process::ChildStdout>,
    vsock_writer: &mut tokio::io::WriteHalf<VsockStream>,
    server_id: &str,
) -> Result<()> {
    let mut line = String::new();

    loop {
        line.clear();

        // Read line from MCP server stdout
        let n = mcp_stdout
            .read_line(&mut line)
            .await
            .context("Failed to read from MCP server stdout")?;

        if n == 0 {
            // EOF - MCP server closed stdout
            info!("[{}] MCP server closed stdout", server_id);
            return Err(anyhow::anyhow!("MCP server stdout closed"));
        }

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue; // Skip empty lines
        }

        debug!(
            "[{}] Received {} bytes from MCP server, forwarding to host",
            server_id,
            trimmed.len()
        );

        // Validate JSON (optional but helpful for debugging)
        if let Err(e) = serde_json::from_str::<serde_json::Value>(trimmed) {
            warn!("[{}] Invalid JSON from MCP server: {} (error: {})", server_id, trimmed, e);
            // Continue anyway - host will handle the error
        }

        // Send to host via vsock (length-prefixed)
        write_length_prefixed_message(vsock_writer, trimmed.as_bytes(), server_id).await?;

        debug!("[{}] Message forwarded to host", server_id);
    }
}

/// Read a length-prefixed message from vsock
async fn read_length_prefixed_message(
    reader: &mut BufReader<tokio::io::ReadHalf<VsockStream>>,
    server_id: &str,
) -> Result<Vec<u8>> {
    // Read 4-byte length prefix (little-endian)
    let mut len_bytes = [0u8; 4];
    reader
        .read_exact(&mut len_bytes)
        .await
        .context("Failed to read message length prefix")?;

    let len = u32::from_le_bytes(len_bytes) as usize;

    // Validate message size
    if len > MAX_MESSAGE_SIZE {
        return Err(anyhow::anyhow!(
            "[{}] Message too large: {} bytes (max: {} bytes)",
            server_id,
            len,
            MAX_MESSAGE_SIZE
        ));
    }

    if len == 0 {
        return Err(anyhow::anyhow!("[{}] Received zero-length message", server_id));
    }

    debug!("[{}] Reading {} byte message from host", server_id, len);

    // Read message payload
    let mut buffer = vec![0u8; len];
    reader
        .read_exact(&mut buffer)
        .await
        .context("Failed to read message payload")?;

    Ok(buffer)
}

/// Write a length-prefixed message to vsock
async fn write_length_prefixed_message(
    writer: &mut tokio::io::WriteHalf<VsockStream>,
    data: &[u8],
    server_id: &str,
) -> Result<()> {
    let len = data.len() as u32;

    debug!("[{}] Writing {} byte message to host", server_id, len);

    // Write 4-byte length prefix (little-endian)
    writer
        .write_all(&len.to_le_bytes())
        .await
        .context("Failed to write message length prefix")?;

    // Write message payload
    writer.write_all(data).await.context("Failed to write message payload")?;

    writer.flush().await.context("Failed to flush vsock writer")?;

    Ok(())
}
