//! Type definitions for tool execution requests and responses
//!
//! These types define the JSON-RPC protocol between the host and the unikernel
//! for executing MCP tools in isolation.

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Tool execution request sent from host to unikernel
///
/// This follows the JSON-RPC 2.0 specification for method calls
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolExecutionRequest {
    /// JSON-RPC version (always "2.0")
    pub jsonrpc: String,
    
    /// Unique request ID
    pub id: u64,
    
    /// MCP server ID that provides this tool
    pub server_id: String,
    
    /// Tool name to execute
    pub method: String,
    
    /// Tool arguments (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<Value>,
}

impl ToolExecutionRequest {
    /// Create a new tool execution request
    pub fn new(id: u64, server_id: String, method: String, params: Option<Value>) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            server_id,
            method,
            params,
        }
    }
}

/// Tool execution response sent from unikernel to host
///
/// This follows the JSON-RPC 2.0 specification for responses
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolExecutionResponse {
    /// JSON-RPC version (always "2.0")
    pub jsonrpc: String,
    
    /// Request ID (matches the request)
    pub id: u64,
    
    /// Execution result (on success)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,
    
    /// Error details (on failure)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
}

impl ToolExecutionResponse {
    /// Create a successful response
    pub fn success(id: u64, result: Value) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(result),
            error: None,
        }
    }
    
    /// Create an error response
    pub fn error(id: u64, error: JsonRpcError) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: None,
            error: Some(error),
        }
    }
}

/// JSON-RPC error object
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcError {
    /// Error code (standard JSON-RPC error codes)
    pub code: i32,
    
    /// Error message
    pub message: String,
    
    /// Additional error data (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

impl JsonRpcError {
    /// Parse error (-32700)
    pub fn parse_error(message: impl Into<String>) -> Self {
        Self {
            code: -32700,
            message: message.into(),
            data: None,
        }
    }
    
    /// Invalid request (-32600)
    pub fn invalid_request(message: impl Into<String>) -> Self {
        Self {
            code: -32600,
            message: message.into(),
            data: None,
        }
    }
    
    /// Method not found (-32601)
    pub fn method_not_found(message: impl Into<String>) -> Self {
        Self {
            code: -32601,
            message: message.into(),
            data: None,
        }
    }
    
    /// Internal error (-32603)
    pub fn internal_error(message: impl Into<String>) -> Self {
        Self {
            code: -32603,
            message: message.into(),
            data: None,
        }
    }
    
    /// Server error (-32000 to -32099)
    pub fn server_error(code: i32, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            data: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_request_serialization() {
        let req = ToolExecutionRequest::new(
            1,
            "test_server".to_string(),
            "test_method".to_string(),
            Some(serde_json::json!({"arg": "value"})),
        );
        
        let json = serde_json::to_string(&req).unwrap();
        assert!(json.contains("\"jsonrpc\":\"2.0\""));
        assert!(json.contains("\"id\":1"));
        assert!(json.contains("\"method\":\"test_method\""));
    }
    
    #[test]
    fn test_success_response() {
        let resp = ToolExecutionResponse::success(1, serde_json::json!({"status": "ok"}));
        
        assert_eq!(resp.id, 1);
        assert!(resp.result.is_some());
        assert!(resp.error.is_none());
    }
    
    #[test]
    fn test_error_response() {
        let error = JsonRpcError::internal_error("Test error");
        let resp = ToolExecutionResponse::error(1, error);
        
        assert_eq!(resp.id, 1);
        assert!(resp.result.is_none());
        assert!(resp.error.is_some());
        assert_eq!(resp.error.unwrap().code, -32603);
    }
}
