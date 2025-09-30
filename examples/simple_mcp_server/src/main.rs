//! Simple MCP Server Example
//!
//! This is a minimal MCP (Model Context Protocol) server that can run in a unikernel.
//! It demonstrates:
//! - Basic MCP protocol handling
//! - Running in HermitCore unikernel
//! - Hardware isolation
//! - Minimal dependencies
//!
//! # MCP Protocol
//!
//! MCP uses JSON-RPC 2.0 for communication:
//! - Request: {"jsonrpc": "2.0", "id": 1, "method": "echo", "params": {"message": "hello"}}
//! - Response: {"jsonrpc": "2.0", "id": 1, "result": {"message": "hello"}}

use serde::{Deserialize, Serialize};
use std::io::{self, BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};

const VERSION: &str = env!("CARGO_PKG_VERSION");

/// JSON-RPC request
#[derive(Debug, Deserialize)]
struct JsonRpcRequest {
    jsonrpc: String,
    id: serde_json::Value,
    method: String,
    params: Option<serde_json::Value>,
}

/// JSON-RPC response
#[derive(Debug, Serialize)]
struct JsonRpcResponse {
    jsonrpc: String,
    id: serde_json::Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
}

/// JSON-RPC error
#[derive(Debug, Serialize)]
struct JsonRpcError {
    code: i32,
    message: String,
}

fn main() {
    println!("╔═══════════════════════════════════════════════╗");
    println!("║   Simple MCP Server v{}                    ║", VERSION);
    println!("║   Running in: {}                            ║",
        if cfg!(target_os = "hermit") { "HermitCore Unikernel 🦀" } else { "Standard OS" });
    println!("╚═══════════════════════════════════════════════╝");
    println!();

    // Bind to all interfaces on port 9000
    let listener = match TcpListener::bind("0.0.0.0:9000") {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Failed to bind to port 9000: {}", e);
            std::process::exit(1);
        }
    };

    println!("✓ Listening on 0.0.0.0:9000");
    println!("✓ MCP server ready to accept connections");
    println!();
    println!("Available tools:");
    println!("  • echo      - Echo back a message");
    println!("  • uppercase - Convert text to uppercase");
    println!("  • info      - Get server information");
    println!();

    // Accept connections
    for (i, stream) in listener.incoming().enumerate() {
        match stream {
            Ok(stream) => {
                println!("[Connection #{}] Accepted from {:?}", i + 1, stream.peer_addr());
                if let Err(e) = handle_connection(stream) {
                    eprintln!("[Connection #{}] Error: {}", i + 1, e);
                }
            }
            Err(e) => {
                eprintln!("Connection failed: {}", e);
            }
        }
    }
}

/// Handle a single client connection
fn handle_connection(mut stream: TcpStream) -> io::Result<()> {
    let mut reader = BufReader::new(stream.try_clone()?);

    loop {
        let mut line = String::new();
        let bytes_read = reader.read_line(&mut line)?;

        if bytes_read == 0 {
            // Connection closed
            println!("  Connection closed by client");
            break;
        }

        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        // Parse JSON-RPC request
        let request: JsonRpcRequest = match serde_json::from_str(line) {
            Ok(req) => req,
            Err(e) => {
                eprintln!("  Invalid JSON-RPC: {}", e);
                send_error(&mut stream, serde_json::Value::Null, -32700, "Parse error")?;
                continue;
            }
        };

        println!("  → Method: {}", request.method);

        // Process request
        let response = process_request(request);

        // Send response
        let response_json = serde_json::to_string(&response).unwrap();
        writeln!(stream, "{}", response_json)?;
        stream.flush()?;

        println!("  ← Response sent");
    }

    Ok(())
}

/// Process a JSON-RPC request
fn process_request(request: JsonRpcRequest) -> JsonRpcResponse {
    let result = match request.method.as_str() {
        "echo" => handle_echo(request.params),
        "uppercase" => handle_uppercase(request.params),
        "info" => handle_info(),
        _ => {
            return JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: None,
                error: Some(JsonRpcError {
                    code: -32601,
                    message: format!("Method not found: {}", request.method),
                }),
            };
        }
    };

    match result {
        Ok(value) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(value),
            error: None,
        },
        Err(msg) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: None,
            error: Some(JsonRpcError {
                code: -32000,
                message: msg,
            }),
        },
    }
}

/// Echo handler
fn handle_echo(params: Option<serde_json::Value>) -> Result<serde_json::Value, String> {
    let params = params.ok_or_else(|| "Missing parameters".to_string())?;
    let message = params
        .get("message")
        .and_then(|v| v.as_str())
        .ok_or_else(|| "Missing 'message' parameter".to_string())?;

    Ok(serde_json::json!({
        "message": message,
        "echo": true
    }))
}

/// Uppercase handler
fn handle_uppercase(params: Option<serde_json::Value>) -> Result<serde_json::Value, String> {
    let params = params.ok_or_else(|| "Missing parameters".to_string())?;
    let text = params
        .get("text")
        .and_then(|v| v.as_str())
        .ok_or_else(|| "Missing 'text' parameter".to_string())?;

    Ok(serde_json::json!({
        "original": text,
        "uppercase": text.to_uppercase()
    }))
}

/// Info handler
fn handle_info() -> Result<serde_json::Value, String> {
    Ok(serde_json::json!({
        "name": "Simple MCP Server",
        "version": VERSION,
        "runtime": if cfg!(target_os = "hermit") { "HermitCore" } else { "Standard" },
        "isolation": if cfg!(target_os = "hermit") { "Unikernel" } else { "Process" },
        "attack_surface": if cfg!(target_os = "hermit") { "~50KB" } else { "~30MB+" },
        "tools": ["echo", "uppercase", "info"]
    }))
}

/// Send error response
fn send_error(
    stream: &mut TcpStream,
    id: serde_json::Value,
    code: i32,
    message: &str,
) -> io::Result<()> {
    let response = JsonRpcResponse {
        jsonrpc: "2.0".to_string(),
        id,
        result: None,
        error: Some(JsonRpcError {
            code,
            message: message.to_string(),
        }),
    };

    let response_json = serde_json::to_string(&response).unwrap();
    writeln!(stream, "{}", response_json)?;
    stream.flush()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_echo_handler() {
        let params = serde_json::json!({"message": "hello"});
        let result = handle_echo(Some(params)).unwrap();
        assert_eq!(result["message"], "hello");
        assert_eq!(result["echo"], true);
    }

    #[test]
    fn test_uppercase_handler() {
        let params = serde_json::json!({"text": "hello world"});
        let result = handle_uppercase(Some(params)).unwrap();
        assert_eq!(result["uppercase"], "HELLO WORLD");
    }

    #[test]
    fn test_info_handler() {
        let result = handle_info().unwrap();
        assert_eq!(result["name"], "Simple MCP Server");
        assert!(result["tools"].is_array());
    }
}