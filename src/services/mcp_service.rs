use crate::utils::circuit_breaker::{
    AnalysisVector as CircuitAnalysisVector, EndpointId, GranularCircuitBreaker,
};
use crate::utils::debug_logger::VerbosityLevel;
use crate::{debug_error, debug_print, debug_success, debug_warn};
use anyhow::{Context, Result};
use reqwest;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;

/// Configuration for an MCP server endpoint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpServerConfig {
    /// Human-readable name for the MCP server
    pub name: String,
    /// Base URL for HTTP/WebSocket MCP server
    pub url: String,
    /// Server type (stdio, http, websocket)
    pub transport_type: McpTransportType,
    /// Authentication configuration if required
    pub auth: Option<McpAuthConfig>,
    /// Whether this server is currently enabled
    pub enabled: bool,
    /// Additional configuration specific to the server
    pub extra_config: HashMap<String, String>,
}

/// Transport types supported by MCP servers
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum McpTransportType {
    /// Standard I/O transport (local processes)
    Stdio,
    /// HTTP REST API transport
    Http,
    /// WebSocket transport for real-time data
    Websocket,
}

/// Authentication configuration for MCP servers
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpAuthConfig {
    /// Authentication method (bearer, api_key, basic)
    pub auth_type: String,
    /// Authentication token or key
    pub token: Option<String>,
    /// Username for basic auth
    pub username: Option<String>,
    /// Password for basic auth
    pub password: Option<String>,
}

/// MCP JSON-RPC request structure
#[derive(Debug, Clone, Serialize)]
struct McpRequest {
    jsonrpc: String,
    id: u64,
    method: String,
    params: Option<serde_json::Value>,
}

/// MCP JSON-RPC response structure
#[derive(Debug, Clone, Deserialize)]
struct McpResponse {
    jsonrpc: String,
    id: u64,
    #[serde(default)]
    result: Option<serde_json::Value>,
    #[serde(default)]
    error: Option<McpError>,
}

/// MCP error structure
#[derive(Debug, Clone, Deserialize)]
struct McpError {
    code: i32,
    message: String,
    #[serde(default)]
    data: Option<serde_json::Value>,
}

/// MCP initialization request
#[derive(Debug, Clone, Serialize)]
struct McpInitializeRequest {
    #[serde(rename = "protocolVersion")]
    protocol_version: String,
    capabilities: ClientCapabilities,
    #[serde(rename = "clientInfo")]
    client_info: ClientInfo,
}

/// MCP client capabilities
#[derive(Debug, Clone, Serialize)]
struct ClientCapabilities {
    #[serde(skip_serializing_if = "Option::is_none")]
    experimental: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    sampling: Option<serde_json::Value>,
}

/// MCP client information
#[derive(Debug, Clone, Serialize)]
struct ClientInfo {
    name: String,
    version: String,
}

/// Tool definition from MCP server
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct McpTool {
    pub name: String,
    pub description: Option<String>,
    #[serde(rename = "inputSchema")]
    pub input_schema: serde_json::Value,
}

/// Service for managing MCP (Model Context Protocol) server connections
pub struct McpService {
    /// Configured MCP servers
    servers: HashMap<String, McpServerConfig>,
    /// HTTP client for making requests
    client: reqwest::Client,
    /// Circuit breaker for managing failed requests
    circuit_breaker: GranularCircuitBreaker,
    /// Debug mode flag
    debug_mode: bool,
    /// Request counter for JSON-RPC ID generation
    request_counter: std::sync::atomic::AtomicU64,
}

impl McpService {
    /// Create a new MCP service instance
    pub fn new() -> Self {
        Self {
            servers: HashMap::new(),
            client: reqwest::Client::new(),
            circuit_breaker: GranularCircuitBreaker::new(),
            debug_mode: false,
            request_counter: std::sync::atomic::AtomicU64::new(1),
        }
    }

    /// Create a new MCP service with debug mode enabled
    pub fn new_with_debug(debug_mode: bool) -> Self {
        let mut service = Self::new();
        service.debug_mode = debug_mode;
        service
    }

    /// Load MCP server configurations from environment or config file
    pub fn load_config(&mut self) -> Result<()> {
        // Load from environment variables first
        self.load_from_env()?;

        // TODO: Load from config file if available
        // self.load_from_file()?;

        Ok(())
    }

    /// Load MCP server configurations from environment variables
    fn load_from_env(&mut self) -> Result<()> {
        // Check for SOLANA_MCP_SERVER_URL environment variable
        if let Ok(url) = env::var("SOLANA_MCP_SERVER_URL") {
            let config = McpServerConfig {
                name: "solana-mcp-server".to_string(),
                url,
                transport_type: McpTransportType::Http,
                auth: None,
                enabled: true,
                extra_config: HashMap::new(),
            };
            
            self.servers.insert("solana".to_string(), config);
            
            if self.debug_mode {
                debug_success!("Loaded Solana MCP server from environment");
            }
        }

        // Check for custom MCP servers
        for (key, value) in env::vars() {
            if key.starts_with("MCP_SERVER_") && key.ends_with("_URL") {
                let server_name = key
                    .strip_prefix("MCP_SERVER_")
                    .unwrap()
                    .strip_suffix("_URL")
                    .unwrap()
                    .to_lowercase();

                let config = McpServerConfig {
                    name: server_name.clone(),
                    url: value,
                    transport_type: McpTransportType::Http,
                    auth: None,
                    enabled: true,
                    extra_config: HashMap::new(),
                };

                self.servers.insert(server_name.clone(), config);

                if self.debug_mode {
                    debug_success!("Loaded {} MCP server from environment", server_name);
                }
            }
        }

        Ok(())
    }

    /// Add or update an MCP server configuration
    pub fn add_server(&mut self, server_id: String, config: McpServerConfig) {
        self.servers.insert(server_id, config);
    }

    /// Remove an MCP server configuration
    pub fn remove_server(&mut self, server_id: &str) -> Option<McpServerConfig> {
        self.servers.remove(server_id)
    }

    /// List all configured MCP servers
    pub fn list_servers(&self) -> Vec<(&String, &McpServerConfig)> {
        self.servers.iter().collect()
    }

    /// Get a specific MCP server configuration
    pub fn get_server(&self, server_id: &str) -> Option<&McpServerConfig> {
        self.servers.get(server_id)
    }

    /// Enable or disable an MCP server
    pub fn toggle_server(&mut self, server_id: &str, enabled: bool) -> Result<()> {
        let config = self.servers.get_mut(server_id)
            .ok_or_else(|| anyhow::anyhow!("Server '{}' not found", server_id))?;
        
        config.enabled = enabled;
        
        if self.debug_mode {
            debug_print!(VerbosityLevel::Basic, "Server '{}' {}", server_id, if enabled { "enabled" } else { "disabled" });
        }
        
        Ok(())
    }

    /// Initialize connection with an MCP server
    pub async fn initialize_server(&self, server_id: &str) -> Result<()> {
        let config = self.servers.get(server_id)
            .ok_or_else(|| anyhow::anyhow!("Server '{}' not found", server_id))?;

        if !config.enabled {
            return Err(anyhow::anyhow!("Server '{}' is disabled", server_id));
        }

        match config.transport_type {
            McpTransportType::Http => {
                self.initialize_http_server(config).await
            }
            McpTransportType::Websocket => {
                self.initialize_websocket_server(config).await
            }
            McpTransportType::Stdio => {
                Err(anyhow::anyhow!("Stdio transport not yet implemented"))
            }
        }
    }

    /// Initialize HTTP-based MCP server
    async fn initialize_http_server(&self, config: &McpServerConfig) -> Result<()> {
        let endpoint_id = EndpointId {
            service: "mcp".to_string(),
            endpoint: config.url.clone(),
        };

        if !self.circuit_breaker.can_execute_endpoint(&endpoint_id) {
            return Err(anyhow::anyhow!("Circuit breaker is open for MCP server '{}'", config.name));
        }

        let init_request = McpInitializeRequest {
            protocol_version: "2025-06-18".to_string(),
            capabilities: ClientCapabilities {
                experimental: None,
                sampling: None,
            },
            client_info: ClientInfo {
                name: "osvm-cli".to_string(),
                version: env!("CARGO_PKG_VERSION").to_string(),
            },
        };

        let request = McpRequest {
            jsonrpc: "2.0".to_string(),
            id: self.request_counter.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
            method: "initialize".to_string(),
            params: Some(serde_json::to_value(&init_request)?),
        };

        let mut req_builder = self.client
            .post(&format!("{}/api/mcp", config.url))
            .header("Content-Type", "application/json")
            .json(&request);

        // Add authentication if configured
        if let Some(auth) = &config.auth {
            req_builder = match auth.auth_type.as_str() {
                "bearer" => {
                    if let Some(token) = &auth.token {
                        req_builder.header("Authorization", format!("Bearer {}", token))
                    } else {
                        req_builder
                    }
                }
                "api_key" => {
                    if let Some(token) = &auth.token {
                        req_builder.header("X-API-Key", token)
                    } else {
                        req_builder
                    }
                }
                _ => req_builder,
            };
        }

        let response = req_builder.send().await
            .context("Failed to send initialize request to MCP server")?;

        if !response.status().is_success() {
            self.circuit_breaker.on_failure_endpoint(&endpoint_id);
            return Err(anyhow::anyhow!("MCP server returned error status: {}", response.status()));
        }

        let mcp_response: McpResponse = response.json().await
            .context("Failed to parse MCP initialize response")?;

        if let Some(error) = mcp_response.error {
            self.circuit_breaker.on_failure_endpoint(&endpoint_id);
            return Err(anyhow::anyhow!("MCP server initialization error: {} - {}", error.code, error.message));
        }

        self.circuit_breaker.on_success_endpoint(&endpoint_id);

        if self.debug_mode {
            debug_success!("Successfully initialized MCP server '{}'", config.name);
        }

        Ok(())
    }

    /// Initialize WebSocket-based MCP server
    async fn initialize_websocket_server(&self, _config: &McpServerConfig) -> Result<()> {
        // TODO: Implement WebSocket initialization
        Err(anyhow::anyhow!("WebSocket transport not yet implemented"))
    }

    /// List available tools from an MCP server
    pub async fn list_tools(&self, server_id: &str) -> Result<Vec<McpTool>> {
        let config = self.servers.get(server_id)
            .ok_or_else(|| anyhow::anyhow!("Server '{}' not found", server_id))?;

        if !config.enabled {
            return Err(anyhow::anyhow!("Server '{}' is disabled", server_id));
        }

        let endpoint_id = EndpointId {
            service: "mcp".to_string(),
            endpoint: config.url.clone(),
        };

        if !self.circuit_breaker.can_execute_endpoint(&endpoint_id) {
            return Err(anyhow::anyhow!("Circuit breaker is open for MCP server '{}'", config.name));
        }

        let request = McpRequest {
            jsonrpc: "2.0".to_string(),
            id: self.request_counter.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
            method: "tools/list".to_string(),
            params: None,
        };

        let mut req_builder = self.client
            .post(&format!("{}/api/mcp", config.url))
            .header("Content-Type", "application/json")
            .json(&request);

        // Add authentication if configured
        if let Some(auth) = &config.auth {
            req_builder = match auth.auth_type.as_str() {
                "bearer" => {
                    if let Some(token) = &auth.token {
                        req_builder.header("Authorization", format!("Bearer {}", token))
                    } else {
                        req_builder
                    }
                }
                "api_key" => {
                    if let Some(token) = &auth.token {
                        req_builder.header("X-API-Key", token)
                    } else {
                        req_builder
                    }
                }
                _ => req_builder,
            };
        }

        let response = req_builder.send().await
            .context("Failed to send tools/list request to MCP server")?;

        if !response.status().is_success() {
            self.circuit_breaker.on_failure_endpoint(&endpoint_id);
            return Err(anyhow::anyhow!("MCP server returned error status: {}", response.status()));
        }

        let mcp_response: McpResponse = response.json().await
            .context("Failed to parse MCP tools/list response")?;

        if let Some(error) = mcp_response.error {
            self.circuit_breaker.on_failure_endpoint(&endpoint_id);
            return Err(anyhow::anyhow!("MCP server tools/list error: {} - {}", error.code, error.message));
        }

        let result = mcp_response.result
            .ok_or_else(|| anyhow::anyhow!("MCP server returned no result for tools/list"))?;

        let tools_list: serde_json::Value = result.get("tools")
            .ok_or_else(|| anyhow::anyhow!("MCP server response missing 'tools' field"))?
            .clone();

        let tools: Vec<McpTool> = serde_json::from_value(tools_list)
            .context("Failed to parse tools from MCP server response")?;

        self.circuit_breaker.on_success_endpoint(&endpoint_id);

        if self.debug_mode {
            debug_success!("Retrieved {} tools from MCP server '{}'", tools.len(), config.name);
        }

        Ok(tools)
    }

    /// Call a tool on an MCP server
    pub async fn call_tool(
        &self, 
        server_id: &str, 
        tool_name: &str, 
        arguments: Option<serde_json::Value>
    ) -> Result<serde_json::Value> {
        let config = self.servers.get(server_id)
            .ok_or_else(|| anyhow::anyhow!("Server '{}' not found", server_id))?;

        if !config.enabled {
            return Err(anyhow::anyhow!("Server '{}' is disabled", server_id));
        }

        let endpoint_id = EndpointId {
            service: "mcp".to_string(),
            endpoint: config.url.clone(),
        };

        if !self.circuit_breaker.can_execute_endpoint(&endpoint_id) {
            return Err(anyhow::anyhow!("Circuit breaker is open for MCP server '{}'", config.name));
        }

        let tool_call = serde_json::json!({
            "name": tool_name,
            "arguments": arguments
        });

        let request = McpRequest {
            jsonrpc: "2.0".to_string(),
            id: self.request_counter.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
            method: "tools/call".to_string(),
            params: Some(tool_call),
        };

        let mut req_builder = self.client
            .post(&format!("{}/api/mcp", config.url))
            .header("Content-Type", "application/json")
            .json(&request);

        // Add authentication if configured
        if let Some(auth) = &config.auth {
            req_builder = match auth.auth_type.as_str() {
                "bearer" => {
                    if let Some(token) = &auth.token {
                        req_builder.header("Authorization", format!("Bearer {}", token))
                    } else {
                        req_builder
                    }
                }
                "api_key" => {
                    if let Some(token) = &auth.token {
                        req_builder.header("X-API-Key", token)
                    } else {
                        req_builder
                    }
                }
                _ => req_builder,
            };
        }

        let response = req_builder.send().await
            .context("Failed to send tools/call request to MCP server")?;

        if !response.status().is_success() {
            self.circuit_breaker.on_failure_endpoint(&endpoint_id);
            return Err(anyhow::anyhow!("MCP server returned error status: {}", response.status()));
        }

        let mcp_response: McpResponse = response.json().await
            .context("Failed to parse MCP tools/call response")?;

        if let Some(error) = mcp_response.error {
            self.circuit_breaker.on_failure_endpoint(&endpoint_id);
            return Err(anyhow::anyhow!("MCP server tools/call error: {} - {}", error.code, error.message));
        }

        let result = mcp_response.result
            .ok_or_else(|| anyhow::anyhow!("MCP server returned no result for tools/call"))?;

        self.circuit_breaker.on_success_endpoint(&endpoint_id);

        if self.debug_mode {
            debug_success!("Successfully called tool '{}' on MCP server '{}'", tool_name, config.name);
        }

        Ok(result)
    }

    /// Query multiple MCP servers with the same request
    pub async fn query_all_servers(&self, tool_name: &str, arguments: Option<serde_json::Value>) -> Vec<(String, Result<serde_json::Value>)> {
        let mut results = Vec::new();
        
        for (server_id, config) in &self.servers {
            if !config.enabled {
                continue;
            }
            
            let result = self.call_tool(server_id, tool_name, arguments.clone()).await;
            results.push((server_id.clone(), result));
        }
        
        results
    }

    /// Get the status information of configured MCP servers
    pub fn get_server_status(&self) -> HashMap<String, String> {
        let mut status = HashMap::new();
        
        for (server_id, config) in &self.servers {
            let status_str = if config.enabled {
                format!("Enabled - {} ({})", config.name, config.url)
            } else {
                format!("Disabled - {} ({})", config.name, config.url)
            };
            status.insert(server_id.clone(), status_str);
        }
        
        status
    }

    /// Test connectivity to an MCP server
    pub async fn test_server(&self, server_id: &str) -> Result<()> {
        // First try to initialize
        self.initialize_server(server_id).await
            .context("Failed to initialize MCP server")?;
        
        // Then try to list tools as a connectivity test
        let _tools = self.list_tools(server_id).await
            .context("Failed to list tools from MCP server")?;
        
        if self.debug_mode {
            debug_success!("MCP server '{}' connectivity test passed", server_id);
        }
        
        Ok(())
    }
}

impl Default for McpService {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mcp_service_creation() {
        let service = McpService::new();
        assert_eq!(service.servers.len(), 0);
        assert!(!service.debug_mode);
    }

    #[test]
    fn test_mcp_service_with_debug() {
        let service = McpService::new_with_debug(true);
        assert_eq!(service.servers.len(), 0);
        assert!(service.debug_mode);
    }

    #[test]
    fn test_add_server() {
        let mut service = McpService::new();
        let config = McpServerConfig {
            name: "test-server".to_string(),
            url: "http://localhost:3000".to_string(),
            transport_type: McpTransportType::Http,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
        };
        
        service.add_server("test".to_string(), config);
        assert_eq!(service.servers.len(), 1);
        assert!(service.get_server("test").is_some());
    }

    #[test]
    fn test_remove_server() {
        let mut service = McpService::new();
        let config = McpServerConfig {
            name: "test-server".to_string(),
            url: "http://localhost:3000".to_string(),
            transport_type: McpTransportType::Http,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
        };
        
        service.add_server("test".to_string(), config);
        assert_eq!(service.servers.len(), 1);
        
        let removed = service.remove_server("test");
        assert!(removed.is_some());
        assert_eq!(service.servers.len(), 0);
    }

    #[test]
    fn test_toggle_server() {
        let mut service = McpService::new();
        let config = McpServerConfig {
            name: "test-server".to_string(),
            url: "http://localhost:3000".to_string(),
            transport_type: McpTransportType::Http,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
        };
        
        service.add_server("test".to_string(), config);
        
        // Test disabling
        assert!(service.toggle_server("test", false).is_ok());
        assert!(!service.get_server("test").unwrap().enabled);
        
        // Test enabling
        assert!(service.toggle_server("test", true).is_ok());
        assert!(service.get_server("test").unwrap().enabled);
        
        // Test non-existent server
        assert!(service.toggle_server("nonexistent", true).is_err());
    }
}