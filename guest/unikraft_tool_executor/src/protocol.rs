//! Length-prefixed vsock protocol implementation
//!
//! This module implements the same protocol as Phase 3's mcp_vsock_wrapper:
//! - 4-byte little-endian length prefix
//! - JSON payload
//!
//! Protocol format:
//! ```
//! [4 bytes: u32 LE length][N bytes: JSON payload]
//! ```

use anyhow::{Context, Result};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio_vsock::VsockStream;

/// Maximum message size (10 MB)
pub const MAX_MESSAGE_SIZE: usize = 10 * 1024 * 1024;

/// Read a length-prefixed message from a vsock stream
///
/// # Protocol
/// 1. Read 4-byte length prefix (little-endian u32)
/// 2. Validate length (must be > 0 and < MAX_MESSAGE_SIZE)
/// 3. Read exactly `length` bytes of payload
///
/// # Errors
/// Returns error if:
/// - Stream closed unexpectedly
/// - Length is 0 or > MAX_MESSAGE_SIZE
/// - Failed to read complete payload
pub async fn read_length_prefixed_message(stream: &mut VsockStream) -> Result<Vec<u8>> {
    // Read 4-byte length prefix
    let mut len_bytes = [0u8; 4];
    stream
        .read_exact(&mut len_bytes)
        .await
        .context("Failed to read message length prefix")?;
    
    let len = u32::from_le_bytes(len_bytes) as usize;
    
    // Validate message size
    if len == 0 {
        anyhow::bail!("Received zero-length message");
    }
    
    if len > MAX_MESSAGE_SIZE {
        anyhow::bail!(
            "Message too large: {} bytes (max: {} bytes)",
            len,
            MAX_MESSAGE_SIZE
        );
    }
    
    log::debug!("Reading {} byte message from vsock", len);
    
    // Read message payload
    let mut buffer = vec![0u8; len];
    stream
        .read_exact(&mut buffer)
        .await
        .context("Failed to read message payload")?;
    
    Ok(buffer)
}

/// Write a length-prefixed message to a vsock stream
///
/// # Protocol
/// 1. Write 4-byte length prefix (little-endian u32)
/// 2. Write message payload
/// 3. Flush the stream
///
/// # Errors
/// Returns error if:
/// - Message is too large (> MAX_MESSAGE_SIZE)
/// - Failed to write to stream
/// - Failed to flush stream
pub async fn write_length_prefixed_message(stream: &mut VsockStream, data: &[u8]) -> Result<()> {
    let len = data.len();
    
    // Validate message size
    if len > MAX_MESSAGE_SIZE {
        anyhow::bail!(
            "Message too large: {} bytes (max: {} bytes)",
            len,
            MAX_MESSAGE_SIZE
        );
    }
    
    log::debug!("Writing {} byte message to vsock", len);
    
    // Write 4-byte length prefix (little-endian)
    let len_bytes = (len as u32).to_le_bytes();
    stream
        .write_all(&len_bytes)
        .await
        .context("Failed to write message length prefix")?;
    
    // Write message payload
    stream
        .write_all(data)
        .await
        .context("Failed to write message payload")?;
    
    // Flush to ensure data is sent immediately
    stream
        .flush()
        .await
        .context("Failed to flush vsock stream")?;
    
    Ok(())
}

/// Read and deserialize a JSON message from vsock
///
/// This is a convenience function that combines `read_length_prefixed_message`
/// with JSON deserialization.
pub async fn read_json_message<T: serde::de::DeserializeOwned>(
    stream: &mut VsockStream,
) -> Result<T> {
    let bytes = read_length_prefixed_message(stream).await?;
    
    serde_json::from_slice(&bytes).context("Failed to deserialize JSON message")
}

/// Serialize and write a JSON message to vsock
///
/// This is a convenience function that combines JSON serialization
/// with `write_length_prefixed_message`.
pub async fn write_json_message<T: serde::Serialize>(
    stream: &mut VsockStream,
    value: &T,
) -> Result<()> {
    let bytes = serde_json::to_vec(value).context("Failed to serialize JSON message")?;
    
    write_length_prefixed_message(stream, &bytes).await
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_max_message_size() {
        assert_eq!(MAX_MESSAGE_SIZE, 10 * 1024 * 1024);
    }
    
    #[test]
    fn test_length_prefix_encoding() {
        let len: u32 = 1234;
        let bytes = len.to_le_bytes();
        let decoded = u32::from_le_bytes(bytes);
        assert_eq!(len, decoded);
    }
    
    // Note: Full integration tests with real vsock streams
    // are in the integration test suite
}
