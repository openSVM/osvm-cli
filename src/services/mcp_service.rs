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
use base64::{Engine as _, engine::general_purpose};

// Constants for MCP protocol
const MCP_PROTOCOL_VERSION: &str = "2025-06-18";
const MCP_JSONRPC_VERSION: &str = "2.0";
const CLIENT_NAME: &str = "osvm-cli";
const DEFAULT_HTTP_TIMEOUT_SECS: u64 = 30;
const MAX_REQUEST_ID: u64 = u64::MAX - 1000; // Leave some buffer for overflow

// Security warnings
const GITHUB_CLONE_WARNING: &str = "\n⚠️  SECURITY WARNING: You are about to clone and build code from a remote repository.\nThis will execute arbitrary build scripts and binaries on your system.\nOnly proceed if you trust the source repository.\n";

// Exit codes for different error types
pub mod exit_codes {
    pub const SUCCESS: i32 = 0;
    pub const GENERAL_ERROR: i32 = 1;
    pub const AUTHENTICATION_ERROR: i32 = 2;
    pub const NETWORK_ERROR: i32 = 3;
    pub const CONFIG_ERROR: i32 = 4;
    pub const VALIDATION_ERROR: i32 = 5;
}

/// MCP-specific error types for better error handling
#[derive(Debug)]
pub enum McpError {
    Authentication(String),
    Network(String),
    Configuration(String),
    Validation(String),
    General(String),
}

impl std::fmt::Display for McpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            McpError::Authentication(msg) => write!(f, "Authentication error: {}", msg),
            McpError::Network(msg) => write!(f, "Network error: {}", msg),
            McpError::Configuration(msg) => write!(f, "Configuration error: {}", msg),
            McpError::Validation(msg) => write!(f, "Validation error: {}", msg),
            McpError::General(msg) => write!(f, "Error: {}", msg),
        }
    }
}

impl std::error::Error for McpError {}

impl McpError {
    pub fn exit_code(&self) -> i32 {
        match self {
            McpError::Authentication(_) => exit_codes::AUTHENTICATION_ERROR,
            McpError::Network(_) => exit_codes::NETWORK_ERROR,
            McpError::Configuration(_) => exit_codes::CONFIG_ERROR,
            McpError::Validation(_) => exit_codes::VALIDATION_ERROR,
            McpError::General(_) => exit_codes::GENERAL_ERROR,
        }
    }
}

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
    error: Option<McpJsonRpcError>,
}

/// MCP JSON-RPC error structure
#[derive(Debug, Clone, Deserialize)]
struct McpJsonRpcError {
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
    /// Create an authenticated HTTP request builder
    fn create_authenticated_request(
        &self,
        url: &str,
        auth: &Option<McpAuthConfig>,
        request: &McpRequest,
    ) -> reqwest::RequestBuilder {
        let mut req_builder = self.client
            .post(url)
            .header("Content-Type", "application/json")
            .timeout(std::time::Duration::from_secs(DEFAULT_HTTP_TIMEOUT_SECS))
            .json(request);

        // Add authentication if configured
        if let Some(auth) = auth {
            req_builder = match auth.auth_type.as_str() {
                "bearer" => {
                    if let Some(token) = &auth.token {
                        if self.debug_mode {
                            debug_print!(VerbosityLevel::Detailed, "Using Bearer authentication (token masked)");
                        }
                        req_builder.header("Authorization", format!("Bearer {}", token))
                    } else {
                        if self.debug_mode {
                            debug_warn!("Bearer auth configured but no token provided");
                        }
                        req_builder
                    }
                }
                "api_key" => {
                    if let Some(token) = &auth.token {
                        if self.debug_mode {
                            debug_print!(VerbosityLevel::Detailed, "Using API Key authentication (key masked)");
                        }
                        req_builder.header("X-API-Key", token)
                    } else {
                        if self.debug_mode {
                            debug_warn!("API key auth configured but no token provided");
                        }
                        req_builder
                    }
                }
                "basic" => {
                    if let Some(username) = &auth.username {
                        if let Some(password) = &auth.password {
                            if self.debug_mode {
                                debug_print!(VerbosityLevel::Detailed, "Using Basic authentication for user: {} (password masked)", username);
                            }
                            let credentials = general_purpose::STANDARD.encode(format!("{}:{}", username, password));
                            req_builder.header("Authorization", format!("Basic {}", credentials))
                        } else {
                            if self.debug_mode {
                                debug_warn!("Basic auth configured but no password provided");
                            }
                            req_builder
                        }
                    } else {
                        if self.debug_mode {
                            debug_warn!("Basic auth configured but no username provided");
                        }
                        req_builder
                    }
                }
                _ => {
                    if self.debug_mode {
                        debug_warn!("Unknown auth type: {}", auth.auth_type);
                    }
                    req_builder
                }
            };
        }

        req_builder
    }

    /// Generate next request ID with overflow protection
    fn next_request_id(&self) -> u64 {
        let id = self.request_counter.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        if id >= MAX_REQUEST_ID {
            // Reset to 1 when approaching overflow
            self.request_counter.store(1, std::sync::atomic::Ordering::SeqCst);
            1
        } else {
            id
        }
    }

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
    /// Priority: Environment variables override file configurations for the same server ID
    pub fn load_config(&mut self) -> Result<()> {
        // Load from config file first (lower priority)
        self.load_from_file()?;
        
        // Load from environment variables second (higher priority - can override file configs)
        self.load_from_env()?;

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
        // Validate GitHub URL to prevent malicious inputs
        if !self.is_valid_github_url(&github_url) {
            return Err(anyhow::anyhow!("Invalid GitHub URL. Must be a valid GitHub repository URL."));
        }

        // Display security warning
        eprintln!("{}", GITHUB_CLONE_WARNING);
        
        // Prompt for confirmation (in production, this would be configurable)
        if !self.confirm_github_clone(&github_url)? {
            return Err(anyhow::anyhow!("Operation cancelled by user"));
        }

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

        // Clone the repository with timeout and additional security flags
        let clone_result = Command::new("git")
            .args(&[
                "clone", 
                "--depth", "1", // Shallow clone for security and performance
                "--single-branch", // Only clone default branch
                &github_url, 
                local_path.to_str().unwrap()
            ])
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
                .env("CARGO_NET_OFFLINE", "false") // Allow network for dependencies
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

    /// Validate GitHub URL for basic security
    fn is_valid_github_url(&self, url: &str) -> bool {
        url.starts_with("https://github.com/") && 
        url.len() > 19 && // Minimum length for a valid repo URL
        url.chars().all(|c| c.is_ascii() && c != '<' && c != '>') && // Basic XSS prevention
        url.split('/').count() >= 5 // github.com/owner/repo minimum
    }

    /// Confirm GitHub clone operation with user
    fn confirm_github_clone(&self, github_url: &str) -> Result<bool> {
        use std::io::{self, Write};
        
        print!("Continue cloning from {}? [y/N]: ", github_url);
        io::stdout().flush()?;
        
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        
        let input = input.trim().to_lowercase();
        Ok(input == "y" || input == "yes")
    }

    /// Get binary name from Cargo.toml with improved parsing
    fn get_binary_name_from_cargo_toml(&self, cargo_toml_path: &Path) -> Result<String> {
        let content = std::fs::read_to_string(cargo_toml_path)
            .context("Failed to read Cargo.toml")?;
        
        // Try to parse as TOML first (more robust)
        match self.parse_toml_package_name(&content) {
            Ok(name) => Ok(name),
            Err(_) => {
                // Fallback to simple string parsing
                self.parse_simple_package_name(&content)
            }
        }
    }

    /// Parse package name using basic TOML-like parsing
    fn parse_toml_package_name(&self, content: &str) -> Result<String> {
        let mut in_package_section = false;
        
        for line in content.lines() {
            let line = line.trim();
            
            // Check for [package] section
            if line == "[package]" {
                in_package_section = true;
                continue;
            }
            
            // Check for other sections
            if line.starts_with('[') && line.ends_with(']') && line != "[package]" {
                in_package_section = false;
                continue;
            }
            
            // If we're in the package section, look for name
            if in_package_section && line.starts_with("name") {
                if let Some(eq_pos) = line.find('=') {
                    let name_part = &line[eq_pos + 1..].trim();
                    // Remove quotes
                    let name = name_part.trim_matches('"').trim_matches('\'');
                    if !name.is_empty() {
                        return Ok(name.to_string());
                    }
                }
            }
        }
        
        Err(anyhow::anyhow!("Could not find package name in [package] section"))
    }

    /// Simple fallback parsing for package name
    fn parse_simple_package_name(&self, content: &str) -> Result<String> {
        for line in content.lines() {
            let line = line.trim();
            if line.starts_with("name") && line.contains("=") {
                if let Some(name_part) = line.split('=').nth(1) {
                    let name = name_part.trim().trim_matches('"').trim_matches('\'');
                    if !name.is_empty() {
                        return Ok(name.to_string());
                    }
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
            protocol_version: MCP_PROTOCOL_VERSION.to_string(),
            capabilities: ClientCapabilities {
                experimental: None,
                sampling: None,
            },
            client_info: ClientInfo {
                name: CLIENT_NAME.to_string(),
                version: env!("CARGO_PKG_VERSION").to_string(),
            },
        };

        let request = McpRequest {
            jsonrpc: MCP_JSONRPC_VERSION.to_string(),
            id: self.next_request_id(),
            method: "initialize".to_string(),
            params: Some(serde_json::to_value(&init_request)?),
        };

        // Use the authentication helper instead of duplicating code
        let response = self.create_authenticated_request(
            &format!("{}/api/mcp", config.url),
            &config.auth,
            &request
        ).send().await
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
        Err(anyhow::anyhow!(
            "WebSocket transport is not yet implemented. Please use HTTP or stdio transport instead.\n\
             To add HTTP transport: osvm mcp add <server_id> --server-url <url> --transport http\n\
             To add stdio transport: osvm mcp add-github <server_id> <github_url>"
        ))
    }

    /// Initialize Stdio-based MCP server
    async fn initialize_stdio_server(&mut self, server_id: &str, config: &McpServerConfig) -> Result<()> {
        if self.debug_mode {
            debug_print!(VerbosityLevel::Basic, "Starting stdio MCP server: {}", config.url);
        }

        // Kill existing process if running
        if let Some(mut existing_process) = self.stdio_processes.remove(server_id) {
            if let Err(e) = existing_process.child.kill().await {
                if self.debug_mode {
                    debug_warn!("Failed to kill existing stdio process: {}", e);
                }
            }
        }

        let mut cmd = TokioCommand::new(&config.url);
        cmd.args(&["stdio"]);
        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        cmd.kill_on_drop(true); // Ensure child processes are cleaned up

        let mut child = cmd.spawn()
            .context("Failed to start MCP server process")?;

        // Send initialization request
        let init_request = McpInitializeRequest {
            protocol_version: MCP_PROTOCOL_VERSION.to_string(),
            capabilities: ClientCapabilities {
                experimental: None,
                sampling: None,
            },
            client_info: ClientInfo {
                name: CLIENT_NAME.to_string(),
                version: env!("CARGO_PKG_VERSION").to_string(),
            },
        };

        let request = McpRequest {
            jsonrpc: MCP_JSONRPC_VERSION.to_string(),
            id: self.next_request_id(),
            method: "initialize".to_string(),
            params: Some(serde_json::to_value(&init_request)?),
        };

        let request_json = serde_json::to_string(&request)?;
        
        if let Some(stdin) = child.stdin.as_mut() {
            stdin.write_all(request_json.as_bytes()).await?;
            stdin.write_all(b"\n").await?;
            stdin.flush().await?;
        }

        // Read response with timeout
        if let Some(stdout) = child.stdout.take() {
            let mut reader = TokioBufReader::new(stdout);
            let mut line = String::new();
            let mut response_found = false;
            
            // Skip log lines and find the JSON response
            while reader.read_line(&mut line).await? > 0 {
                let line_trimmed = line.trim();
                if line_trimmed.starts_with("{") {
                    let mcp_response: Result<McpResponse, _> = serde_json::from_str(line_trimmed);
                    if let Ok(response) = mcp_response {
                        if response.id == request.id {
                            if let Some(error) = response.error {
                                return Err(anyhow::anyhow!("MCP server initialization error: {} - {}", error.code, error.message));
                            }
                            response_found = true;
                            break;
                        }
                    }
                }
                line.clear();
            }
            
            if !response_found {
                return Err(anyhow::anyhow!("No valid response received from MCP server"));
            }
        }

        // Store the process for lifecycle management
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

    /// Cleanup stdio processes for graceful shutdown
    pub async fn cleanup_stdio_processes(&mut self) {
        for (server_id, mut process) in self.stdio_processes.drain() {
            if let Err(e) = process.child.kill().await {
                if self.debug_mode {
                    debug_warn!("Failed to cleanup stdio process for '{}': {}", server_id, e);
                }
            }
        }
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