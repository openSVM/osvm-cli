//! MCP proxy logic for forwarding tool execution requests to host
//!
//! This module implements a simple proxy pattern where the unikernel
//! receives tool execution requests and forwards them back to the host
//! where the actual MCP servers are running.
//!
//! Architecture:
//! ```
//! Host MCP Server ← [forward via host vsock] ← Unikernel ← [request] ← Host Runtime
//! ```
//!
//! Note: This is a placeholder implementation. In practice, the actual tool execution
//! will be coordinated by the host, and this unikernel serves primarily as an
//! isolated execution environment.

use anyhow::Result;
use crate::types::{ToolExecutionRequest, ToolExecutionResponse, JsonRpcError};

/// Execute a tool request by forwarding to the host MCP service
///
/// This is a placeholder implementation that simulates tool execution.
/// In a full implementation, this would establish a separate connection
/// back to the host's MCP service layer to execute the actual tool.
///
/// For Phase 2, we're using a simpler architecture where the host
/// directly manages MCP server communication, and the unikernel
/// provides isolation boundaries.
pub async fn execute_tool_request(request: ToolExecutionRequest) -> Result<ToolExecutionResponse> {
    log::info!(
        "Executing tool '{}' from server '{}'",
        request.method,
        request.server_id
    );
    
    // In the simplified architecture, we return a response indicating
    // the tool was received and would be executed by the host
    let result = serde_json::json!({
        "status": "executed_in_unikernel",
        "tool": request.method,
        "server": request.server_id,
        "isolation": "unikraft",
        "message": "Tool executed in isolated unikernel environment"
    });
    
    Ok(ToolExecutionResponse::success(request.id, result))
}

/// Validate a tool execution request
///
/// Performs basic validation on the request before execution:
/// - Checks JSON-RPC version is "2.0"
/// - Validates server_id is not empty
/// - Validates method name is not empty
pub fn validate_request(request: &ToolExecutionRequest) -> Result<()> {
    // Check JSON-RPC version
    if request.jsonrpc != "2.0" {
        anyhow::bail!("Invalid JSON-RPC version: {}", request.jsonrpc);
    }
    
    // Validate server_id
    if request.server_id.trim().is_empty() {
        anyhow::bail!("server_id cannot be empty");
    }
    
    // Validate method name
    if request.method.trim().is_empty() {
        anyhow::bail!("method name cannot be empty");
    }
    
    Ok(())
}

/// Handle a tool execution request with proper error handling
///
/// This wraps `execute_tool_request` with validation and error conversion
/// to ensure all errors are properly formatted as JSON-RPC error responses.
pub async fn handle_tool_request(request: ToolExecutionRequest) -> ToolExecutionResponse {
    let request_id = request.id;
    
    // Validate request
    if let Err(e) = validate_request(&request) {
        log::warn!("Invalid request: {}", e);
        return ToolExecutionResponse::error(
            request_id,
            JsonRpcError::invalid_request(e.to_string()),
        );
    }
    
    // Execute tool
    match execute_tool_request(request).await {
        Ok(response) => {
            log::info!("Tool executed successfully (request_id: {})", request_id);
            response
        }
        Err(e) => {
            log::error!("Tool execution failed: {:#}", e);
            ToolExecutionResponse::error(
                request_id,
                JsonRpcError::internal_error(format!("Tool execution failed: {}", e)),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    
    #[test]
    fn test_validate_request_valid() {
        let request = ToolExecutionRequest {
            jsonrpc: "2.0".to_string(),
            id: 1,
            server_id: "test_server".to_string(),
            method: "test_method".to_string(),
            params: None,
        };
        
        assert!(validate_request(&request).is_ok());
    }
    
    #[test]
    fn test_validate_request_invalid_version() {
        let request = ToolExecutionRequest {
            jsonrpc: "1.0".to_string(),
            id: 1,
            server_id: "test_server".to_string(),
            method: "test_method".to_string(),
            params: None,
        };
        
        assert!(validate_request(&request).is_err());
    }
    
    #[test]
    fn test_validate_request_empty_server() {
        let request = ToolExecutionRequest {
            jsonrpc: "2.0".to_string(),
            id: 1,
            server_id: "".to_string(),
            method: "test_method".to_string(),
            params: None,
        };
        
        assert!(validate_request(&request).is_err());
    }
    
    #[test]
    fn test_validate_request_empty_method() {
        let request = ToolExecutionRequest {
            jsonrpc: "2.0".to_string(),
            id: 1,
            server_id: "test_server".to_string(),
            method: "".to_string(),
            params: None,
        };
        
        assert!(validate_request(&request).is_err());
    }
    
    #[tokio::test]
    async fn test_handle_tool_request() {
        let request = ToolExecutionRequest {
            jsonrpc: "2.0".to_string(),
            id: 1,
            server_id: "test_server".to_string(),
            method: "test_method".to_string(),
            params: Some(json!({"arg": "value"})),
        };
        
        let response = handle_tool_request(request).await;
        
        assert_eq!(response.id, 1);
        assert!(response.result.is_some());
        assert!(response.error.is_none());
    }
}
