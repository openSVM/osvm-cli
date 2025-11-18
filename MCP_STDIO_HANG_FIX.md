# MCP Stdio Communication Hang - Root Cause & Fix

## Problem Summary

Research agent hangs indefinitely during MCP server initialization at:
- `initialize_server()` → `initialize_stdio_server()` (line 1653)
- `list_tools()` → `list_tools_stdio()` → `read_mcp_response()` (line 2058-2060)

## Root Causes Found

### 1. ✅ FIXED: `initialize_stdio_server()` - No Timeout on Async Read
**Location**: `src/services/mcp_service.rs:1653`

**Problem**: 
```rust
while reader.read_line(&mut line).await? > 0 {
    // Process response...
}
```
- Async read loop with NO timeout
- Hangs indefinitely if MCP server doesn't respond
- No error handling for slow/unresponsive servers

**Fix Applied**:
- Wrapped read loop in `tokio::time::timeout(Duration::from_secs(10))`
- Returns clear error message after 10 seconds
- Allows fallback mechanisms to kick in

### 2. ⚠️ TODO: `read_mcp_response()` - Blocking Synchronous I/O
**Location**: `src/services/mcp_service.rs:2058-2060`

**Problem**:
```rust
let bytes_read = reader
    .read_line(&mut line)  // ← BLOCKS ENTIRE THREAD!
    .context("Failed to read line from stdio process")?;
```
- Uses **synchronous** `std::io::BufReader` (not tokio async)
- `read_line()` blocks thread indefinitely
- `MAX_ATTEMPTS` only counts lines, not time
- If server sends no data, blocks forever

**Impact**:
- Affects `list_tools_stdio()` function
- Research agent hangs when listing available tools
- Cannot be interrupted or timed out

**Recommended Fix**:
Option 1: Convert to async/tokio:
```rust
async fn read_mcp_response(
    &self,
    reader: &mut TokioBufReader<ChildStdout>,
    operation: &str,
) -> Result<String> {
    let timeout_duration = std::time::Duration::from_secs(10);
    let read_task = async {
        let mut line = String::new();
        let mut attempts = 0;
        const MAX_ATTEMPTS: usize = 50;

        while attempts < MAX_ATTEMPTS {
            attempts += 1;
            line.clear();
            
            let bytes_read = reader.read_line(&mut line).await?;
            if bytes_read == 0 {
                return Err(anyhow::anyhow!("EOF from MCP server"));
            }
            
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            
            if trimmed.starts_with('{') && trimmed.contains("jsonrpc") {
                // Process JSON response...
                return Ok(trimmed.to_string());
            }
        }
        
        Err(anyhow::anyhow!("Timeout: max attempts reached"))
    };
    
    tokio::time::timeout(timeout_duration, read_task)
        .await
        .map_err(|_| anyhow!("Timeout waiting for MCP response"))?
}
```

Option 2: Use thread + channel for sync I/O with timeout:
```rust
use std::sync::mpsc::channel;
use std::thread;

let (tx, rx) = channel();
let handle = thread::spawn(move || {
    // Synchronous read in separate thread
    let result = reader.read_line(&mut line);
    tx.send(result).ok();
});

// Wait with timeout
match rx.recv_timeout(Duration::from_secs(10)) {
    Ok(result) => result?,
    Err(_) => return Err(anyhow!("Timeout reading from MCP server")),
}
```

## Testing Done

✅ MCP Server (Node.js) verified working:
- Initialize request: ✓ responds in <1s
- Tool call request: ✓ responds in 3-15s (depending on API)
- Error handling: ✓ properly returns error responses

The hang is definitely in the Rust client, not the Node.js server.

## Impact on Research Agent

**Before Fix**:
1. Research starts
2. Calls `initialize_server("opensvm")`
3. Hangs indefinitely at stdio read
4. User must kill process

**After initialize_stdio Fix**:
1. Research starts
2. Calls `initialize_server("opensvm")` 
3. Times out after 10s with clear error
4. Fallback mechanism can activate

**Still TODO**:
- Fix `list_tools_stdio()` blocking read
- This is called BEFORE the research investigation
- Still causes hang at startup

## Files Modified

- ✅ `src/services/mcp_service.rs:1646-1695` - Added timeout to initialize_stdio_server
- ⚠️ `src/services/mcp_service.rs:2039-2088` - TODO: Fix read_mcp_response blocking

## Next Steps

1. Apply async timeout fix to `read_mcp_response()`
2. Test full research agent flow
3. Verify list_tools completes in <10s
4. Test with real blockchain queries

