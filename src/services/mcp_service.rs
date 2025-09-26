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
use std::process::{Command, Stdio};
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use tokio::process::{Child, Command as TokioCommand};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader as TokioBufReader};
use dirs;

/// Configuration for an MCP server endpoint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpServerConfig {
    /// Human-readable name for the MCP server
    pub name: String,
    /// Base URL for HTTP/WebSocket MCP server or path for stdio
    pub url: String,
    /// Server type (stdio, http, websocket)
    pub transport_type: McpTransportType,
    /// Authentication configuration if required
    pub auth: Option<McpAuthConfig>,
    /// Whether this server is currently enabled
    pub enabled: bool,
    /// Additional configuration specific to the server
    pub extra_config: HashMap<String, String>,
    /// GitHub repository URL (if cloned from GitHub)
    pub github_url: Option<String>,
    /// Local path to cloned repository (for stdio transport)
    pub local_path: Option<String>,
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

/// Stdio process manager for MCP servers
#[derive(Debug)]
pub struct StdioProcess {
    pub child: Child,
    pub server_id: String,
    pub local_path: PathBuf,
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
    /// Active stdio processes
    stdio_processes: HashMap<String, StdioProcess>,
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
            stdio_processes: HashMap::new(),
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
        
        // Load from config file if available
        self.load_from_file()?;

        Ok(())
    }

    /// Load configurations from JSON file
    fn load_from_file(&mut self) -> Result<()> {
        let config_path = dirs::config_dir()
            .unwrap_or_else(|| std::env::current_dir().unwrap())
            .join("osvm")
            .join("mcp_servers.json");

        if !config_path.exists() {
            return Ok(()); // No config file is fine
        }

        let content = std::fs::read_to_string(&config_path)
            .context("Failed to read MCP config file")?;
            
        let servers: HashMap<String, McpServerConfig> = serde_json::from_str(&content)
            .context("Failed to parse MCP config file")?;
            
        for (server_id, config) in servers {
            self.servers.insert(server_id, config);
        }
        
        if self.debug_mode {
            debug_success!("Loaded {} MCP servers from config file", self.servers.len());
        }
        
        Ok(())
    }

    /// Save configurations to JSON file
    pub fn save_config(&self) -> Result<()> {
        let config_dir = dirs::config_dir()
            .unwrap_or_else(|| std::env::current_dir().unwrap())
            .join("osvm");
            
        std::fs::create_dir_all(&config_dir)?;
        
        let config_path = config_dir.join("mcp_servers.json");
        
        let content = serde_json::to_string_pretty(&self.servers)
            .context("Failed to serialize MCP servers")?;
            
        std::fs::write(&config_path, content)
            .context("Failed to write MCP config file")?;
            
        if self.debug_mode {
            debug_success!("Saved {} MCP servers to config file", self.servers.len());
        }
        
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
                github_url: None,
                local_path: None,
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
                    github_url: None,
                    local_path: None,
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
        if let Err(e) = self.save_config() {
            if self.debug_mode {
                debug_warn!("Failed to save config: {}", e);
            }
        }
    }

    /// Add MCP server from GitHub URL
    pub async fn add_server_from_github(&mut self, server_id: String, github_url: String, name: Option<String>) -> Result<()> {
        if self.debug_mode {
            debug_print!(VerbosityLevel::Basic, "Cloning MCP server from GitHub: {}", github_url);
        }

        // Create a temp directory for cloning
        let temp_dir = std::env::temp_dir().join("osvm-mcp-servers");
        std::fs::create_dir_all(&temp_dir)?;
        
        let local_path = temp_dir.join(&server_id);
        
        // Remove existing directory if present
        if local_path.exists() {
            std::fs::remove_dir_all(&local_path)?;
        }

        // Clone the repository
        let clone_result = Command::new("git")
            .args(&["clone", &github_url, local_path.to_str().unwrap()])
            .output()
            .context("Failed to execute git clone command")?;

        if !clone_result.status.success() {
            let error_msg = String::from_utf8_lossy(&clone_result.stderr);
            return Err(anyhow::anyhow!("Git clone failed: {}", error_msg));
        }

        if self.debug_mode {
            debug_success!("Successfully cloned {} to {:?}", github_url, local_path);
        }

        // Check if it's a Rust project and build it
        let cargo_toml_path = local_path.join("Cargo.toml");
        if cargo_toml_path.exists() {
            if self.debug_mode {
                debug_print!(VerbosityLevel::Basic, "Building Rust project at {:?}", local_path);
            }

            let build_result = Command::new("cargo")
                .args(&["build", "--release"])
                .current_dir(&local_path)
                .output()
                .context("Failed to execute cargo build command")?;

            if !build_result.status.success() {
                let error_msg = String::from_utf8_lossy(&build_result.stderr);
                return Err(anyhow::anyhow!("Cargo build failed: {}", error_msg));
            }

            if self.debug_mode {
                debug_success!("Successfully built MCP server at {:?}", local_path);
            }
        }

        // Find the binary name from Cargo.toml
        let binary_name = self.get_binary_name_from_cargo_toml(&cargo_toml_path)?;
        let binary_path = local_path.join("target/release").join(&binary_name);

        if !binary_path.exists() {
            return Err(anyhow::anyhow!("Built binary not found at {:?}", binary_path));
        }

        // Create server configuration
        let config = McpServerConfig {
            name: name.unwrap_or_else(|| format!("{} (from GitHub)", server_id)),
            url: binary_path.to_str().unwrap().to_string(),
            transport_type: McpTransportType::Stdio,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
            github_url: Some(github_url),
            local_path: Some(local_path.to_str().unwrap().to_string()),
        };

        self.servers.insert(server_id, config);
        if let Err(e) = self.save_config() {
            if self.debug_mode {
                debug_warn!("Failed to save config: {}", e);
            }
        }
        Ok(())
    }

    /// Get binary name from Cargo.toml
    fn get_binary_name_from_cargo_toml(&self, cargo_toml_path: &Path) -> Result<String> {
        let content = std::fs::read_to_string(cargo_toml_path)
            .context("Failed to read Cargo.toml")?;
        
        // Simple parsing to find the package name
        for line in content.lines() {
            let line = line.trim();
            if line.starts_with("name") && line.contains("=") {
                if let Some(name_part) = line.split('=').nth(1) {
                    let name = name_part.trim().trim_matches('"').trim_matches('\'');
                    return Ok(name.to_string());
                }
            }
        }
        
        Err(anyhow::anyhow!("Could not find package name in Cargo.toml"))
    }

    /// Remove an MCP server configuration
    pub fn remove_server(&mut self, server_id: &str) -> Option<McpServerConfig> {
        let result = self.servers.remove(server_id);
        if result.is_some() {
            if let Err(e) = self.save_config() {
                if self.debug_mode {
                    debug_warn!("Failed to save config: {}", e);
                }
            }
        }
        result
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
        
        if let Err(e) = self.save_config() {
            if self.debug_mode {
                debug_warn!("Failed to save config: {}", e);
            }
        }
        
        Ok(())
    }

    /// Initialize connection with an MCP server
    pub async fn initialize_server(&mut self, server_id: &str) -> Result<()> {
        let config = self.servers.get(server_id)
            .ok_or_else(|| anyhow::anyhow!("Server '{}' not found", server_id))?
            .clone(); // Clone the config to avoid borrowing issues

        if !config.enabled {
            return Err(anyhow::anyhow!("Server '{}' is disabled", server_id));
        }

        match config.transport_type {
            McpTransportType::Http => {
                self.initialize_http_server(&config).await
            }
            McpTransportType::Websocket => {
                self.initialize_websocket_server(&config).await
            }
            McpTransportType::Stdio => {
                self.initialize_stdio_server(server_id, &config).await
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

    /// Initialize Stdio-based MCP server
    async fn initialize_stdio_server(&mut self, server_id: &str, config: &McpServerConfig) -> Result<()> {
        if self.debug_mode {
            debug_print!(VerbosityLevel::Basic, "Starting stdio MCP server: {}", config.url);
        }

        let mut cmd = TokioCommand::new(&config.url);
        cmd.args(&["stdio"]);
        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());

        let mut child = cmd.spawn()
            .context("Failed to start MCP server process")?;

        // Send initialization request
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

        let request_json = serde_json::to_string(&request)?;
        
        if let Some(stdin) = child.stdin.as_mut() {
            stdin.write_all(request_json.as_bytes()).await?;
            stdin.write_all(b"\n").await?;
            stdin.flush().await?;
        }

        // Read response
        if let Some(stdout) = child.stdout.take() {
            let mut reader = TokioBufReader::new(stdout);
            let mut line = String::new();
            
            // Skip log lines and find the JSON response
            while reader.read_line(&mut line).await? > 0 {
                if line.trim().starts_with("{") {
                    let mcp_response: Result<McpResponse, _> = serde_json::from_str(&line);
                    if let Ok(response) = mcp_response {
                        if response.id == request.id {
                            if let Some(error) = response.error {
                                return Err(anyhow::anyhow!("MCP server initialization error: {} - {}", error.code, error.message));
                            }
                            break;
                        }
                    }
                }
                line.clear();
            }
        }

        // Store the process
        if let Some(local_path) = &config.local_path {
            let stdio_process = StdioProcess {
                child,
                server_id: server_id.to_string(),
                local_path: PathBuf::from(local_path),
            };
            
            self.stdio_processes.insert(server_id.to_string(), stdio_process);
        }

        if self.debug_mode {
            debug_success!("Successfully initialized stdio MCP server '{}'", config.name);
        }

        Ok(())
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
    pub async fn test_server(&mut self, server_id: &str) -> Result<()> {
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
            github_url: None,
            local_path: None,
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
            github_url: None,
            local_path: None,
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
            github_url: None,
            local_path: None,
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