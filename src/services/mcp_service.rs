use crate::services::ephemeral_microvm::{ChatVmOrchestrator, EphemeralVmManager};
use crate::services::isolation_config::{IsolationConfig, ServerConfig};
use crate::services::microvm_launcher::{
    McpServerMicroVmConfig, McpServerMicroVmHandle, MicroVmLauncher, MountPoint,
};
use crate::services::unikernel_runtime::{UnikernelConfig, UnikernelRuntime};
use crate::utils::circuit_breaker::{
    AnalysisVector as CircuitAnalysisVector, EndpointId, GranularCircuitBreaker,
};
use crate::utils::debug_logger::VerbosityLevel;
use crate::utils::input_sanitization;
use crate::utils::path_security::create_secure_socket_dir;
use crate::{debug_error, debug_print, debug_success, debug_warn};
use anyhow::{anyhow, Context, Result};
use base64::{engine::general_purpose, Engine as _};
use dirs;
use log::warn;
use reqwest;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{ChildStdout, Command, Stdio};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader as TokioBufReader};
use tokio::process::{Child, Command as TokioCommand};
use toml;
use which;

// Constants for MCP protocol
const MCP_PROTOCOL_VERSION: &str = "2025-06-18";
const MCP_JSONRPC_VERSION: &str = "2.0";
const CLIENT_NAME: &str = "osvm-cli";
const DEFAULT_HTTP_TIMEOUT_SECS: u64 = 30;
const MAX_REQUEST_ID: u64 = u64::MAX - 1000; // Leave some buffer for overflow

// MCP method names
const MCP_METHOD_INITIALIZE: &str = "initialize";
const MCP_METHOD_TOOLS_LIST: &str = "tools/list";
const MCP_METHOD_TOOLS_CALL: &str = "tools/call";

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
    /// Isolation configuration for unikernel execution
    isolation_config: IsolationConfig,
    /// Unikernel runtime for spawning isolated tool executions
    unikernel_runtime: UnikernelRuntime,
    /// MicroVM launcher for spawning isolated MCP servers
    microvm_launcher: Option<MicroVmLauncher>,
    /// Active MCP server microVMs
    mcp_server_microvms: HashMap<String, McpServerMicroVmHandle>,
    /// Ephemeral VM manager for tool execution
    ephemeral_vm_manager: EphemeralVmManager,
    /// Whether to use ephemeral VMs by default for tool execution
    use_ephemeral_vms: bool,
}

impl McpService {
    /// Centralized error printing helper
    fn print_error(&self, message: &str, error: &anyhow::Error) {
        if self.debug_mode {
            debug_error!("{}: {}", message, error);
        } else {
            eprintln!("❌ {}: {}", message, error);
        }
    }

    /// Create an authenticated HTTP request builder
    fn create_authenticated_request(
        &self,
        url: &str,
        auth: &Option<McpAuthConfig>,
        request: &McpRequest,
    ) -> reqwest::RequestBuilder {
        let mut req_builder = self
            .client
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
                            debug_print!("Using Bearer authentication (token masked)");
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
                            debug_print!("Using API Key authentication (key masked)");
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
                                debug_print!(
                                    "Using Basic authentication for user: {} (password masked)",
                                    username
                                );
                            }
                            let credentials = general_purpose::STANDARD
                                .encode(format!("{}:{}", username, password));
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
        let id = self
            .request_counter
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        if id >= MAX_REQUEST_ID {
            // Reset to 1 when approaching overflow
            self.request_counter
                .store(1, std::sync::atomic::Ordering::SeqCst);
            1
        } else {
            id
        }
    }

    /// Create a new MCP service instance
    pub fn new() -> Self {
        // Load isolation config or use default
        let isolation_config = IsolationConfig::load().unwrap_or_else(|e| {
            eprintln!(
                "Warning: Failed to load isolation config: {}. Using defaults.",
                e
            );
            IsolationConfig::default()
        });

        // Initialize unikernel runtime
        let unikernel_runtime = UnikernelRuntime::new(PathBuf::from(&isolation_config.unikernel_dir))
            .unwrap_or_else(|e| {
                eprintln!("Warning: Failed to initialize unikernel runtime: {}. Unikernel execution will be disabled.", e);
                // Create a fallback runtime with a temp directory
                UnikernelRuntime::new(std::env::temp_dir().join("osvm-unikernels"))
                    .expect("Failed to create fallback unikernel runtime")
            });

        // Initialize microVM launcher (graceful fallback if unavailable)
        let microvm_launcher = MicroVmLauncher::new().ok();
        if microvm_launcher.is_none() {
            eprintln!("Warning: MicroVM launcher unavailable. MicroVM isolation disabled.");
        }

        Self {
            servers: HashMap::new(),
            client: reqwest::Client::new(),
            circuit_breaker: GranularCircuitBreaker::new(),
            debug_mode: false,
            request_counter: std::sync::atomic::AtomicU64::new(1),
            stdio_processes: HashMap::new(),
            isolation_config,
            unikernel_runtime,
            microvm_launcher,
            mcp_server_microvms: HashMap::new(),
            ephemeral_vm_manager: EphemeralVmManager::new(false),
            use_ephemeral_vms: false, // DISABLED: Ephemeral VMs - networking issues, run directly on host
        }
    }

    /// Create a new MCP service with debug mode enabled
    pub fn new_with_debug(debug_mode: bool) -> Self {
        let mut service = Self::new();
        service.debug_mode = debug_mode;
        service.ephemeral_vm_manager = EphemeralVmManager::new(debug_mode);
        service
    }

    /// Load MCP server configurations from environment or config file
    /// Priority: Environment variables override file configurations for the same server ID
    pub fn load_config(&mut self) -> Result<()> {
        // Load from config file first (lower priority)
        self.load_from_file()?;

        // Load from environment variables second (higher priority - can override file configs)
        self.load_from_env()?;

        // Initialize default servers if none are configured (first run)
        self.initialize_default_servers();

        Ok(())
    }

    /// Initialize default MCP servers on first run
    /// This ensures osvm-mcp is available even on fresh installations
    fn initialize_default_servers(&mut self) {
        // Only add defaults if no servers are configured yet
        if !self.servers.is_empty() {
            return;
        }

        if self.debug_mode {
            debug_print!("No MCP servers configured, initializing defaults...");
        }

        // Add osvm-mcp as default MCP server
        let osvm_mcp_path = if let Some(osvm_dir) = dirs::home_dir() {
            osvm_dir.join(".osvm/mcp/osvm-mcp/build/index.js")
        } else {
            PathBuf::from("~/.osvm/mcp/osvm-mcp/build/index.js")
        };

        // Only add osvm-mcp if it exists
        if osvm_mcp_path.exists() {
            let local_path = dirs::home_dir()
                .map(|h| h.join(".osvm/mcp/osvm-mcp").to_string_lossy().to_string())
                .unwrap_or_else(|| "~/.osvm/mcp/osvm-mcp".to_string());

            let osvm_mcp_config = McpServerConfig {
                name: "OpenSVM MCP".to_string(),
                url: osvm_mcp_path.to_string_lossy().to_string(),
                transport_type: McpTransportType::Stdio,
                auth: None,
                enabled: true,
                extra_config: HashMap::new(),
                github_url: Some("https://github.com/openSVM/osvm-mcp".to_string()),
                local_path: Some(local_path),
            };

            self.servers.insert("osvm-mcp".to_string(), osvm_mcp_config);

            if self.debug_mode {
                debug_success!("Added osvm-mcp as default MCP server");
            }
        }

        // Save the default configuration
        if let Err(e) = self.save_config() {
            if self.debug_mode {
                debug_warn!("Failed to save default MCP config: {}", e);
            }
        }
    }

    /// Load configurations from JSON file
    fn load_from_file(&mut self) -> Result<()> {
        let config_dir = dirs::config_dir().unwrap_or_else(|| {
            std::env::current_dir().unwrap_or_else(|e| {
                warn!(
                    "Failed to get current directory: {}. Using current path.",
                    e
                );
                PathBuf::from("./")
            })
        });

        let config_path = config_dir.join("osvm").join("mcp_servers.json");

        if !config_path.exists() {
            return Ok(()); // No config file is fine
        }

        let content =
            std::fs::read_to_string(&config_path).context("Failed to read MCP config file")?;

        let servers: HashMap<String, McpServerConfig> =
            serde_json::from_str(&content).context("Failed to parse MCP config file")?;

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
        let base_dir = dirs::config_dir().unwrap_or_else(|| {
            std::env::current_dir().unwrap_or_else(|e| {
                warn!(
                    "Failed to get current directory: {}. Using current path.",
                    e
                );
                PathBuf::from("./")
            })
        });

        let config_dir = base_dir.join("osvm");

        std::fs::create_dir_all(&config_dir)?;

        let config_path = config_dir.join("mcp_servers.json");

        let content = serde_json::to_string_pretty(&self.servers)
            .context("Failed to serialize MCP servers")?;

        std::fs::write(&config_path, content).context("Failed to write MCP config file")?;

        if self.debug_mode {
            debug_success!("Saved {} MCP servers to config file", self.servers.len());
        }

        Ok(())
    }

    /// Securely load MCP server configurations from specific environment variables
    /// SECURITY: Only accesses predefined environment variables, no enumeration
    fn load_mcp_servers_from_env_secure(&mut self) -> Result<()> {
        // Define maximum number of MCP servers to prevent resource exhaustion
        const MAX_MCP_SERVERS: u32 = 20;

        // List of expected MCP server environment variable patterns
        let expected_env_vars = [
            "SOLANA_MCP_SERVER_URL",
            "OSVM_MCP_SERVER_URL",
            "DEVNET_MCP_SERVER_URL",
            "MAINNET_MCP_SERVER_URL",
        ];

        // Load well-known MCP server environment variables
        for env_var in &expected_env_vars {
            if let Ok(url) = env::var(env_var) {
                // Validate URL before use
                if self.validate_mcp_url(&url)? {
                    let server_name = env_var.to_lowercase().replace("_url", "").replace("_", "-");

                    let config = McpServerConfig {
                        name: server_name.clone(),
                        url,
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
        }

        // Load numbered MCP servers (MCP_SERVER_1_URL, MCP_SERVER_2_URL, etc.)
        for i in 1..=MAX_MCP_SERVERS {
            let env_var = format!("MCP_SERVER_{}_URL", i);
            if let Ok(url) = env::var(&env_var) {
                // Validate URL before use
                if self.validate_mcp_url(&url)? {
                    let server_name = format!("mcp-server-{}", i);

                    let config = McpServerConfig {
                        name: server_name.clone(),
                        url,
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
        }

        Ok(())
    }

    /// Validate MCP server URL for security
    fn validate_mcp_url(&self, url: &str) -> Result<bool> {
        // Parse URL to validate format
        let parsed_url = url::Url::parse(url).context("Invalid MCP server URL format")?;

        // Only allow HTTP and HTTPS
        if !matches!(parsed_url.scheme(), "http" | "https") {
            warn!("Invalid MCP server URL scheme: {}", parsed_url.scheme());
            return Ok(false);
        }

        // Block access to internal/private networks
        if let Some(host) = parsed_url.host() {
            match host {
                url::Host::Ipv4(ip) => {
                    if ip.is_loopback() || ip.is_private() || ip.is_link_local() {
                        warn!("MCP server URL points to internal network: {}", ip);
                        return Ok(false);
                    }
                }
                url::Host::Ipv6(ip) => {
                    if ip.is_loopback() {
                        warn!("MCP server URL points to loopback: {}", ip);
                        return Ok(false);
                    }
                }
                url::Host::Domain(domain) => {
                    let domain_lower = domain.to_lowercase();
                    if matches!(domain_lower.as_str(), "localhost" | "127.0.0.1" | "::1") {
                        warn!("MCP server URL points to localhost: {}", domain);
                        return Ok(false);
                    }
                }
            }
        }

        // Validate URL length to prevent buffer overflows
        if url.len() > 2048 {
            warn!("MCP server URL too long: {} characters", url.len());
            return Ok(false);
        }

        Ok(true)
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
        // SECURITY FIX: Use specific environment variable access instead of env::vars()
        self.load_mcp_servers_from_env_secure()?;

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
    pub async fn add_server_from_github(
        &mut self,
        server_id: String,
        github_url: String,
        name: Option<String>,
        skip_confirmation: bool,
    ) -> Result<()> {
        // Validate server_id to prevent path traversal attacks
        let server_id = input_sanitization::validate_identifier(&server_id)
            .context("Invalid server ID: must be alphanumeric with hyphens/underscores only")?;

        // Validate GitHub URL to prevent malicious inputs
        if !self.is_valid_github_url(&github_url) {
            return Err(anyhow::anyhow!(
                "Invalid GitHub URL. Must be a valid GitHub repository URL."
            ));
        }

        // Display security warning (unless automated setup)
        if !skip_confirmation {
            eprintln!("{}", GITHUB_CLONE_WARNING);

            // Prompt for confirmation
            if !self.confirm_github_clone(&github_url)? {
                return Err(anyhow::anyhow!("Operation cancelled by user"));
            }
        }

        if self.debug_mode {
            debug_print!("Cloning MCP server from GitHub: {}", github_url);
        }

        // Create directory for cloning in user's home directory
        let home_dir = dirs::home_dir()
            .ok_or_else(|| anyhow::anyhow!("Could not determine home directory"))?;
        let osvm_mcp_dir = home_dir.join(".osvm").join("mcp");
        std::fs::create_dir_all(&osvm_mcp_dir)?;

        // Canonicalize osvm_mcp_dir to get absolute path and resolve any symlinks
        let canonical_temp_dir = osvm_mcp_dir
            .canonicalize()
            .context("Failed to canonicalize MCP directory")?;

        let local_path = canonical_temp_dir.join(&server_id);

        // SECURITY: Verify the constructed path is still within our temp directory
        // This prevents path traversal via symlinks or race conditions
        let canonical_local_path = if local_path.exists() {
            local_path
                .canonicalize()
                .context("Failed to canonicalize local path")?
        } else {
            // If it doesn't exist yet, we can't canonicalize it, but we can verify the parent
            local_path.clone()
        };

        // Ensure the path is within our allowed directory
        if !canonical_local_path.starts_with(&canonical_temp_dir) {
            anyhow::bail!(
                "Security: Attempted path traversal detected. Path {:?} is outside allowed directory {:?}",
                canonical_local_path,
                canonical_temp_dir
            );
        }

        // Remove existing directory if present (now safe from TOCTOU)
        if local_path.exists() {
            // Re-check after canonicalization to prevent race condition
            if !canonical_local_path.starts_with(&canonical_temp_dir) {
                anyhow::bail!("Security: Path verification failed during removal");
            }
            std::fs::remove_dir_all(&local_path)?;
        }

        // Clone the repository with timeout and additional security flags
        let clone_result = Command::new("git")
            .args([
                "clone",
                "--depth",
                "1",               // Shallow clone for security and performance
                "--single-branch", // Only clone default branch
                &github_url,
                local_path.to_str().ok_or_else(|| {
                    anyhow!(
                        "Invalid path contains non-UTF8 characters: {}",
                        local_path.display()
                    )
                })?,
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

        // Detect project type and build accordingly
        let cargo_toml_path = local_path.join("Cargo.toml");
        let package_json_path = local_path.join("package.json");

        let execution_command = if cargo_toml_path.exists() {
            // Rust project - check for build.rs and warn user
            let build_rs_path = local_path.join("build.rs");
            if build_rs_path.exists() {
                eprintln!("\n⚠️  CRITICAL SECURITY WARNING: This project contains a build.rs script that will execute during compilation!");
                eprintln!("Build scripts can run arbitrary code with your user privileges.");
                eprintln!("Build script location: {:?}\n", build_rs_path);

                // Always require confirmation for projects with build scripts, even with --yes
                if !self.confirm_build_execution("Rust project with build.rs")? {
                    return Err(anyhow::anyhow!(
                        "Build cancelled by user for security reasons"
                    ));
                }
            }

            if self.debug_mode {
                debug_print!("Building Rust project at {:?}", local_path);
            }

            let build_result = Command::new("cargo")
                .args(["build", "--release", "--frozen"]) // --frozen prevents dependency updates
                .current_dir(&local_path)
                .env("CARGO_NET_OFFLINE", "true") // Disable network for security
                .env("CARGO_NET_GIT_FETCH_WITH_CLI", "false") // Prevent git operations
                .output()
                .context("Failed to execute cargo build command")?;

            if !build_result.status.success() {
                let error_msg = String::from_utf8_lossy(&build_result.stderr);
                return Err(anyhow::anyhow!("Cargo build failed: {}", error_msg));
            }

            if self.debug_mode {
                debug_success!("Successfully built Rust MCP server at {:?}", local_path);
            }

            // Find the binary name from Cargo.toml
            let binary_name = self.get_binary_name_from_cargo_toml(&cargo_toml_path)?;
            let binary_path = local_path.join("target/release").join(&binary_name);

            if !binary_path.exists() {
                return Err(anyhow::anyhow!(
                    "Built binary not found at {:?}",
                    binary_path
                ));
            }

            binary_path
                .to_str()
                .ok_or_else(|| {
                    anyhow!(
                        "Binary path contains non-UTF8 characters: {}",
                        binary_path.display()
                    )
                })?
                .to_string()
        } else if package_json_path.exists() {
            // Node.js project - check for postinstall scripts and warn user
            let package_json_content = std::fs::read_to_string(&package_json_path)?;
            if package_json_content.contains("\"postinstall\"")
                || package_json_content.contains("\"preinstall\"")
            {
                eprintln!("\n⚠️  CRITICAL SECURITY WARNING: This project contains npm install hooks (preinstall/postinstall)!");
                eprintln!("These scripts will execute arbitrary code during npm install.");
                eprintln!("Package.json location: {:?}\n", package_json_path);

                // Always require confirmation for projects with install hooks
                if !self.confirm_build_execution("Node.js project with install hooks")? {
                    return Err(anyhow::anyhow!(
                        "npm install cancelled by user for security reasons"
                    ));
                }
            }

            if self.debug_mode {
                debug_print!("Building Node.js project at {:?}", local_path);
            }

            // Run npm install with --ignore-scripts to prevent automatic script execution
            // Note: This may break some packages, but it's more secure
            let install_result = Command::new("npm")
                .args(["install", "--ignore-scripts"])
                .current_dir(&local_path)
                .output()
                .context("Failed to execute npm install command")?;

            if !install_result.status.success() {
                let error_msg = String::from_utf8_lossy(&install_result.stderr);
                return Err(anyhow::anyhow!("npm install failed: {}", error_msg));
            }

            if self.debug_mode {
                debug_success!(
                    "Successfully installed npm dependencies for MCP server at {:?}",
                    local_path
                );
            }

            // Run npm build for TypeScript projects (if build script exists)
            if package_json_content.contains("\"build\"") {
                println!("🔨 Building TypeScript MCP server...");
                let build_result = Command::new("npm")
                    .args(["run", "build"])
                    .current_dir(&local_path)
                    .output()
                    .context("Failed to execute npm run build command")?;

                if !build_result.status.success() {
                    let error_msg = String::from_utf8_lossy(&build_result.stderr);
                    return Err(anyhow::anyhow!("npm run build failed: {}", error_msg));
                }

                if self.debug_mode {
                    debug_success!(
                        "Successfully built TypeScript MCP server at {:?}",
                        local_path
                    );
                }
            }

            // Get package info from package.json
            let (package_name, main_script) =
                self.get_package_info_from_package_json(&package_json_path)?;

            // Determine the script to run
            let script_path = if let Some(script) = main_script {
                if let Some(stripped) = script.strip_prefix("./") {
                    local_path.join(stripped)
                } else if script.starts_with("/") {
                    PathBuf::from(script)
                } else {
                    local_path.join(script)
                }
            } else {
                return Err(anyhow::anyhow!(
                    "No main script found in package.json for package '{}'",
                    package_name
                ));
            };

            if !script_path.exists() {
                return Err(anyhow::anyhow!(
                    "Main script not found at {:?}",
                    script_path
                ));
            }

            // For Node.js MCP servers, we need to find the absolute path to node
            let node_path = which::which("node")
                .context("Failed to find node in PATH. Please ensure Node.js is installed.")?;

            format!(
                "{} {}",
                node_path.to_str().ok_or_else(|| anyhow!(
                    "Node path contains non-UTF8 characters: {}",
                    node_path.display()
                ))?,
                script_path.to_str().ok_or_else(|| anyhow!(
                    "Script path contains non-UTF8 characters: {}",
                    script_path.display()
                ))?
            )
        } else {
            return Err(anyhow::anyhow!(
                "No Cargo.toml or package.json found in the cloned repository. \
                Only Rust (Cargo) and Node.js (npm) MCP servers are currently supported."
            ));
        };

        // Create server configuration
        let config = McpServerConfig {
            name: name.unwrap_or_else(|| format!("{} (from GitHub)", server_id)),
            url: execution_command,
            transport_type: McpTransportType::Stdio,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
            github_url: Some(github_url),
            local_path: Some(
                local_path
                    .to_str()
                    .ok_or_else(|| {
                        anyhow!(
                            "Local path contains non-UTF8 characters: {}",
                            local_path.display()
                        )
                    })?
                    .to_string(),
            ),
        };

        self.servers.insert(server_id, config);
        if let Err(e) = self.save_config() {
            if self.debug_mode {
                debug_warn!("Failed to save config: {}", e);
            }
        }
        Ok(())
    }

    /// Validate GitHub URL with strict parsing to prevent command injection
    fn is_valid_github_url(&self, url: &str) -> bool {
        // Use strict URL validation from input_sanitization module
        let validated_url = match input_sanitization::validate_url(url) {
            Ok(u) => u,
            Err(_) => return false,
        };

        // Parse URL to validate structure
        let parsed = match url::Url::parse(&validated_url) {
            Ok(p) => p,
            Err(_) => return false,
        };

        // Strict validation:
        // 1. Scheme must be exactly https
        if parsed.scheme() != "https" {
            return false;
        }

        // 2. Host must be exactly github.com
        if parsed.host_str() != Some("github.com") {
            return false;
        }

        // 3. No fragments (prevents --upload-pack attacks)
        if parsed.fragment().is_some() {
            return false;
        }

        // 4. No query parameters
        if parsed.query().is_some() {
            return false;
        }

        // 5. No credentials in URL
        if !parsed.username().is_empty() || parsed.password().is_some() {
            return false;
        }

        // 6. Path must have at least owner/repo format
        let path = parsed.path();
        if path.split('/').filter(|s| !s.is_empty()).count() < 2 {
            return false;
        }

        true
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

    /// Confirm build execution for projects with potentially dangerous build scripts
    fn confirm_build_execution(&self, project_type: &str) -> Result<bool> {
        use std::io::{self, Write};

        print!(
            "⚠️  Execute build for {}? This will run build scripts on your system! [y/N]: ",
            project_type
        );
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        let input = input.trim().to_lowercase();
        Ok(input == "y" || input == "yes")
    }

    /// Validate stdio command to prevent command injection
    fn validate_stdio_command(&self, command: &str) -> Result<String> {
        use std::path::PathBuf;

        // Check for shell metacharacters in command
        if self.contains_shell_metacharacters(command) {
            anyhow::bail!(
                "Command contains forbidden characters. \
                Shell metacharacters (;|&$`<>()) are not allowed for security."
            );
        }

        // If command is not an absolute path, try to find it in PATH
        let cmd_path = if !command.starts_with('/') {
            // Try to find the command in PATH
            match which::which(command) {
                Ok(path) => path,
                Err(_) => {
                    anyhow::bail!(
                        "Command '{}' not found in PATH. \
                        Please provide an absolute path or ensure the command is in PATH.",
                        command
                    );
                }
            }
        } else {
            std::path::PathBuf::from(command)
        };

        // Canonicalize path to prevent traversal
        let canonical = cmd_path
            .canonicalize()
            .context("Invalid command path or file does not exist")?;

        // Whitelist of allowed executable directories
        let mut allowed_dirs = vec![
            PathBuf::from("/usr/bin"),
            PathBuf::from("/usr/local/bin"),
            PathBuf::from("/bin"),
        ];

        // Add common user binary directories for development environments
        if let Ok(home) = std::env::var("HOME") {
            let home_path = PathBuf::from(home);

            // Add ~/.local/bin (standard user binaries)
            allowed_dirs.push(home_path.join(".local/bin"));

            // Add ~/bin (another common location)
            allowed_dirs.push(home_path.join("bin"));

            // Add ~/.nvm/versions/node/*/bin (nvm installation)
            if let Ok(entries) = std::fs::read_dir(home_path.join(".nvm/versions/node")) {
                for entry in entries.flatten() {
                    if entry.path().is_dir() {
                        allowed_dirs.push(entry.path().join("bin"));
                    }
                }
            }

            // Add ~/.deno/bin (deno installation)
            allowed_dirs.push(home_path.join(".deno/bin"));

            // Add ~/.cargo/bin (rust binaries)
            allowed_dirs.push(home_path.join(".cargo/bin"));

            // Add ~/.osvm/mcp (osvm MCP servers)
            allowed_dirs.push(home_path.join(".osvm/mcp"));
        }

        let is_in_allowed_dir = allowed_dirs.iter().any(|dir| canonical.starts_with(dir));

        if !is_in_allowed_dir {
            anyhow::bail!(
                "Command must be in an allowed directory: /usr/bin, /usr/local/bin, /bin, \
                ~/.local/bin, ~/bin, ~/.nvm/versions/node/*/bin, ~/.deno/bin, or ~/.cargo/bin. \
                Found: {:?}",
                canonical
            );
        }

        // Check if command is in the trusted ~/.osvm/mcp directory
        let is_in_osvm_mcp = if let Ok(home) = std::env::var("HOME") {
            let osvm_mcp_dir = PathBuf::from(home).join(".osvm/mcp");
            canonical.starts_with(osvm_mcp_dir)
        } else {
            false
        };

        // Skip whitelist check for trusted ~/.osvm/mcp directory
        if !is_in_osvm_mcp {
            // Whitelist of allowed executables by filename
            let allowed_executables = ["node", "python3", "python", "deno"];
            let filename = canonical
                .file_name()
                .and_then(|n| n.to_str())
                .ok_or_else(|| anyhow::anyhow!("Invalid executable name"))?;

            if !allowed_executables.contains(&filename) {
                anyhow::bail!(
                    "Executable '{}' is not in the whitelist. \
                    Allowed executables: {}",
                    filename,
                    allowed_executables.join(", ")
                );
            }
        }

        Ok(canonical.to_string_lossy().to_string())
    }

    /// Check if string contains shell metacharacters
    fn contains_shell_metacharacters(&self, s: &str) -> bool {
        let dangerous_chars = [';', '|', '&', '$', '`', '<', '>', '(', ')', '\n', '\r'];
        s.chars().any(|c| dangerous_chars.contains(&c))
    }

    /// Get binary name from Cargo.toml with proper TOML parsing
    fn get_binary_name_from_cargo_toml(&self, cargo_toml_path: &Path) -> Result<String> {
        let content =
            std::fs::read_to_string(cargo_toml_path).context("Failed to read Cargo.toml")?;

        // Try to parse as TOML first (most robust)
        match toml::from_str::<toml::Value>(&content) {
            Ok(toml_value) => {
                if let Some(package) = toml_value.get("package") {
                    if let Some(name) = package.get("name") {
                        if let Some(name_str) = name.as_str() {
                            return Ok(name_str.to_string());
                        }
                    }
                }
                // If no package name found in TOML, fall back to manual parsing
                self.parse_simple_package_name(&content)
            }
            Err(_) => {
                // If TOML parsing fails, fall back to manual parsing
                self.parse_simple_package_name(&content)
            }
        }
    }

    /// Get package info from package.json for npm-based MCP servers
    fn get_package_info_from_package_json(
        &self,
        package_json_path: &Path,
    ) -> Result<(String, Option<String>)> {
        let content =
            std::fs::read_to_string(package_json_path).context("Failed to read package.json")?;

        let package_json: serde_json::Value =
            serde_json::from_str(&content).context("Failed to parse package.json")?;

        let name = package_json
            .get("name")
            .and_then(|n| n.as_str())
            .ok_or_else(|| anyhow::anyhow!("No 'name' field found in package.json"))?
            .to_string();

        // Check for main entry point or bin scripts
        let main_script = if let Some(bin) = package_json.get("bin") {
            // Handle both string and object bin definitions
            match bin {
                serde_json::Value::String(script) => Some(script.clone()),
                serde_json::Value::Object(bin_obj) => {
                    // Try to find the main binary or use the first one
                    bin_obj
                        .get(&name)
                        .or_else(|| bin_obj.values().next())
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string())
                }
                _ => None,
            }
        } else if let Some(main) = package_json.get("main") {
            main.as_str().map(|s| s.to_string())
        } else {
            // Default to index.js if no main specified
            Some("index.js".to_string())
        };

        Ok((name, main_script))
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
        let config = self
            .servers
            .get_mut(server_id)
            .ok_or_else(|| anyhow::anyhow!("Server '{}' not found", server_id))?;

        config.enabled = enabled;

        if self.debug_mode {
            debug_print!(
                "Server '{}' {}",
                server_id,
                if enabled { "enabled" } else { "disabled" }
            );
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
        let config = self
            .servers
            .get(server_id)
            .ok_or_else(|| anyhow::anyhow!("Server '{}' not found", server_id))?
            .clone(); // Clone the config to avoid borrowing issues

        if !config.enabled {
            return Err(anyhow::anyhow!("Server '{}' is disabled", server_id));
        }

        // Check if should use microVM (clone to avoid borrow checker issues)
        if let Some(iso_config) = self.isolation_config.get_server_config(server_id).cloned() {
            if iso_config.use_microvm {
                return self
                    .initialize_server_in_microvm(server_id, &iso_config)
                    .await;
            }
        }

        // Fallback to existing transport-based initialization
        match config.transport_type {
            McpTransportType::Http => self.initialize_http_server(&config).await,
            McpTransportType::Websocket => self.initialize_websocket_server(&config).await,
            McpTransportType::Stdio => self.initialize_stdio_server(server_id, &config).await,
        }
    }

    /// Initialize MCP server in a dedicated microVM
    async fn initialize_server_in_microvm(
        &mut self,
        server_id: &str,
        iso_config: &ServerConfig,
    ) -> Result<()> {
        if self.debug_mode {
            debug_print!(
                "Initializing MCP server '{}' in dedicated microVM",
                server_id
            );
        }

        // Check if microVM launcher is available
        let launcher = self.microvm_launcher.as_ref().ok_or_else(|| {
            anyhow!("MicroVM launcher not available. Please ensure Firecracker is installed.")
        })?;

        // Build MicroVM configuration from isolation config
        let mounts: Vec<MountPoint> = iso_config
            .microvm_mounts
            .iter()
            .map(|m| MountPoint {
                host_path: m.host_path.clone(),
                guest_path: m.vm_path.clone(),
                readonly: m.readonly,
            })
            .collect();

        let microvm_config = McpServerMicroVmConfig {
            server_id: server_id.to_string(),
            memory_mb: iso_config.microvm_config.memory_mb,
            vcpus: iso_config.microvm_config.vcpus,
            server_command: iso_config
                .server_command
                .clone()
                .unwrap_or_else(|| format!("mcp-server-{}", server_id)),
            work_dir: PathBuf::from("/app"),
            mounts,
            vsock_cid: 0, // Auto-allocate
        };

        // Launch microVM
        let handle = launcher
            .launch_mcp_server(microvm_config)
            .context("Failed to launch MCP server microVM")?;

        if self.debug_mode {
            debug_success!(
                "Launched MCP server '{}' at vsock CID {}",
                server_id,
                handle.vsock_cid()
            );
        }

        // Send initialize request
        let init_request = serde_json::json!({
            "jsonrpc": "2.0",
            "id": self.next_request_id(),
            "method": "initialize",
            "params": {
                "protocolVersion": MCP_PROTOCOL_VERSION,
                "capabilities": {},
                "clientInfo": {
                    "name": CLIENT_NAME,
                    "version": env!("CARGO_PKG_VERSION")
                }
            }
        });

        let response = handle
            .send_request(init_request)
            .await
            .context("Failed to initialize MCP server in microVM")?;

        // Validate response
        if let Some(error) = response.get("error") {
            return Err(anyhow!("MCP server initialization failed: {:?}", error));
        }

        // Store handle for future requests
        self.mcp_server_microvms
            .insert(server_id.to_string(), handle);

        if self.debug_mode {
            debug_success!(
                "Successfully initialized MCP server '{}' in microVM",
                server_id
            );
        }

        Ok(())
    }

    /// Initialize HTTP-based MCP server
    async fn initialize_http_server(&self, config: &McpServerConfig) -> Result<()> {
        let endpoint_id = EndpointId {
            service: "mcp".to_string(),
            endpoint: config.url.clone(),
        };

        if !self.circuit_breaker.can_execute_endpoint(&endpoint_id) {
            return Err(anyhow::anyhow!(
                "Circuit breaker is open for MCP server '{}'",
                config.name
            ));
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
            method: MCP_METHOD_INITIALIZE.to_string(),
            params: Some(serde_json::to_value(&init_request)?),
        };

        // Use the authentication helper instead of duplicating code
        let response = self
            .create_authenticated_request(
                &format!("{}/api/mcp", config.url),
                &config.auth,
                &request,
            )
            .send()
            .await
            .context("Failed to send initialize request to MCP server")?;

        if !response.status().is_success() {
            self.circuit_breaker.on_failure_endpoint(&endpoint_id);
            return Err(anyhow::anyhow!(
                "MCP server returned error status: {}",
                response.status()
            ));
        }

        let mcp_response: McpResponse = response
            .json()
            .await
            .context("Failed to parse MCP initialize response")?;

        if let Some(error) = mcp_response.error {
            self.circuit_breaker.on_failure_endpoint(&endpoint_id);
            return Err(anyhow::anyhow!(
                "MCP server initialization error: {} - {}",
                error.code,
                error.message
            ));
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
    async fn initialize_stdio_server(
        &mut self,
        server_id: &str,
        config: &McpServerConfig,
    ) -> Result<()> {
        if self.debug_mode {
            debug_print!("Starting stdio MCP server: {}", config.url);
        }

        // Kill existing process if running
        if let Some(mut existing_process) = self.stdio_processes.remove(server_id) {
            if let Err(e) = existing_process.child.kill().await {
                if self.debug_mode {
                    debug_warn!("Failed to kill existing stdio process: {}", e);
                }
            }
        }

        // Parse the command and arguments from the URL field, stripping the stdio:// prefix if present
        let command_str = if config.url.starts_with("stdio://") {
            &config.url[8..] // Skip "stdio://"
        } else {
            &config.url
        };

        let mut cmd_parts = command_str.split_whitespace();
        let command = cmd_parts
            .next()
            .ok_or_else(|| anyhow::anyhow!("Invalid command in server URL: {}", config.url))?;

        // SECURITY: Validate command to prevent command injection
        let validated_command = self.validate_stdio_command(command)?;

        let mut cmd = TokioCommand::new(&validated_command);

        // Add any command arguments first
        // SECURITY: Also validate arguments for shell metacharacters
        for arg in cmd_parts {
            if self.contains_shell_metacharacters(arg) {
                anyhow::bail!(
                    "Command argument contains forbidden characters: {}. \
                    Shell metacharacters are not allowed for security.",
                    arg
                );
            }
            cmd.arg(arg);
        }

        // Then add the stdio argument for MCP protocol
        cmd.arg("stdio");
        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        cmd.kill_on_drop(true); // Ensure child processes are cleaned up

        let mut child = cmd.spawn().context("Failed to start MCP server process")?;

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
            method: MCP_METHOD_INITIALIZE.to_string(),
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
                                return Err(anyhow::anyhow!(
                                    "MCP server initialization error: {} - {}",
                                    error.code,
                                    error.message
                                ));
                            }
                            response_found = true;
                            break;
                        }
                    }
                }
                line.clear();
            }

            if !response_found {
                return Err(anyhow::anyhow!(
                    "No valid response received from MCP server"
                ));
            }
        }

        // Store the process for lifecycle management
        if let Some(local_path) = &config.local_path {
            let stdio_process = StdioProcess {
                child,
                server_id: server_id.to_string(),
                local_path: PathBuf::from(local_path),
            };

            self.stdio_processes
                .insert(server_id.to_string(), stdio_process);
        }

        if self.debug_mode {
            debug_success!(
                "Successfully initialized stdio MCP server '{}'",
                config.name
            );
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

    /// Shutdown all MCP server microVMs gracefully
    pub async fn shutdown_all_mcp_microvms(&mut self) -> Result<()> {
        if self.debug_mode {
            debug_print!(
                "Shutting down {} MCP server microVMs",
                self.mcp_server_microvms.len()
            );
        }

        for (server_id, handle) in self.mcp_server_microvms.drain() {
            if let Err(e) = handle.shutdown() {
                warn!("Failed to shutdown microVM for '{}': {}", server_id, e);
                if self.debug_mode {
                    debug_warn!("Error shutting down microVM for '{}': {}", server_id, e);
                }
            } else if self.debug_mode {
                debug_success!("Successfully shut down microVM for '{}'", server_id);
            }
        }

        if self.debug_mode {
            debug_success!("All MCP server microVMs shut down");
        }

        Ok(())
    }

    /// Cleanup all resources (stdio processes and microVMs)
    pub async fn cleanup_all(&mut self) -> Result<()> {
        if self.debug_mode {
            debug_print!("Cleaning up all MCP service resources");
        }

        // Cleanup stdio processes
        self.cleanup_stdio_processes().await;

        // Cleanup microVMs
        self.shutdown_all_mcp_microvms().await?;

        if self.debug_mode {
            debug_success!("All MCP service resources cleaned up");
        }

        Ok(())
    }

    /// List available tools from an MCP server
    pub async fn list_tools(&self, server_id: &str) -> Result<Vec<McpTool>> {
        let config = self
            .servers
            .get(server_id)
            .ok_or_else(|| anyhow::anyhow!("Server '{}' not found", server_id))?;

        if !config.enabled {
            return Err(anyhow::anyhow!("Server '{}' is disabled", server_id));
        }

        // Handle different transport types
        match config.transport_type {
            McpTransportType::Http => self.list_tools_http(server_id, config).await,
            McpTransportType::Stdio => self.list_tools_stdio(server_id, config).await,
            McpTransportType::Websocket => {
                Err(anyhow::anyhow!("WebSocket transport not yet implemented"))
            }
        }
    }

    /// List tools from HTTP-based MCP server
    async fn list_tools_http(
        &self,
        _server_id: &str,
        config: &McpServerConfig,
    ) -> Result<Vec<McpTool>> {
        let endpoint_id = EndpointId {
            service: "mcp".to_string(),
            endpoint: config.url.clone(),
        };

        if !self.circuit_breaker.can_execute_endpoint(&endpoint_id) {
            return Err(anyhow::anyhow!(
                "Circuit breaker is open for MCP server '{}'",
                config.name
            ));
        }

        let request = McpRequest {
            jsonrpc: "2.0".to_string(),
            id: self
                .request_counter
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst),
            method: MCP_METHOD_TOOLS_LIST.to_string(),
            params: Some(serde_json::json!({})), // Empty object, not None
        };

        let mut req_builder = self
            .client
            .post(format!("{}/api/mcp", config.url))
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

        let response = req_builder
            .send()
            .await
            .context("Failed to send tools/list request to MCP server")?;

        if !response.status().is_success() {
            self.circuit_breaker.on_failure_endpoint(&endpoint_id);
            return Err(anyhow::anyhow!(
                "MCP server returned error status: {}",
                response.status()
            ));
        }

        let mcp_response: McpResponse = response
            .json()
            .await
            .context("Failed to parse MCP tools/list response")?;

        if let Some(error) = mcp_response.error {
            self.circuit_breaker.on_failure_endpoint(&endpoint_id);
            return Err(anyhow::anyhow!(
                "MCP server tools/list error: {} - {}",
                error.code,
                error.message
            ));
        }

        let result = mcp_response
            .result
            .ok_or_else(|| anyhow::anyhow!("MCP server returned no result for tools/list"))?;

        let tools_list: serde_json::Value = result
            .get("tools")
            .ok_or_else(|| anyhow::anyhow!("MCP server response missing 'tools' field"))?
            .clone();

        let tools: Vec<McpTool> = serde_json::from_value(tools_list)
            .context("Failed to parse tools from MCP server response")?;

        self.circuit_breaker.on_success_endpoint(&endpoint_id);

        if self.debug_mode {
            debug_success!(
                "Retrieved {} tools from MCP server '{}'",
                tools.len(),
                config.name
            );
        }

        Ok(tools)
    }

    /// List tools from stdio-based MCP server
    async fn list_tools_stdio(
        &self,
        server_id: &str,
        config: &McpServerConfig,
    ) -> Result<Vec<McpTool>> {
        // For stdio, we need to use synchronous I/O since we're not keeping the process handle
        // We'll spawn a new process for each request (not ideal but works for now)

        if self.debug_mode {
            debug_print!("Listing tools from stdio MCP server: {}", config.name);
        }

        // Parse the command from the URL field, stripping the stdio:// prefix if present
        let command_str = if config.url.starts_with("stdio://") {
            &config.url[8..] // Skip "stdio://"
        } else {
            &config.url
        };

        let mut parts = command_str.split_whitespace();
        let program = parts
            .next()
            .ok_or_else(|| anyhow::anyhow!("Invalid stdio command: {}", config.url))?;
        let args: Vec<&str> = parts.collect();

        // Create the process
        let mut child = Command::new(program)
            .args(&args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .with_context(|| {
                format!(
                    "Failed to spawn stdio MCP server process: program='{}' args={:?}",
                    program, args
                )
            })?;

        let mut stdin = child
            .stdin
            .take()
            .ok_or_else(|| anyhow::anyhow!("Failed to get stdin for stdio process"))?;
        let mut stdout = child
            .stdout
            .take()
            .ok_or_else(|| anyhow::anyhow!("Failed to get stdout for stdio process"))?;

        // Send initialize request first
        let init_request = McpRequest {
            jsonrpc: MCP_JSONRPC_VERSION.to_string(),
            id: 1,
            method: MCP_METHOD_INITIALIZE.to_string(),
            params: Some(serde_json::json!({
                "protocolVersion": MCP_PROTOCOL_VERSION,
                "capabilities": {},
                "clientInfo": {
                    "name": CLIENT_NAME,
                    "version": env!("CARGO_PKG_VERSION")
                }
            })),
        };

        let init_request_str = serde_json::to_string(&init_request)?;
        stdin.write_all(init_request_str.as_bytes())?;
        stdin.write_all(b"\n")?;
        stdin.flush()?;

        // Read initialize response
        let mut reader = BufReader::new(stdout);

        // Find the first valid MCP response (filter out log messages)
        let init_response_str = self
            .read_mcp_response(&mut reader, "initialization")
            .context("Failed to read initialization response from stdio MCP server")?;

        let _init_response: McpResponse = serde_json::from_str(&init_response_str)
            .context("Failed to parse initialize response from stdio MCP server")?;

        // Now send the tools/list request
        let tools_request = McpRequest {
            jsonrpc: MCP_JSONRPC_VERSION.to_string(),
            id: 2,
            method: MCP_METHOD_TOOLS_LIST.to_string(),
            params: Some(serde_json::json!({})), // Empty object, not None
        };

        let tools_request_str = serde_json::to_string(&tools_request)?;
        stdin.write_all(tools_request_str.as_bytes())?;
        stdin.write_all(b"\n")?;
        stdin.flush()?;

        // Read tools response with filtering
        let tools_response_str = self
            .read_mcp_response(&mut reader, "tools/list")
            .context("Failed to read tools response from stdio MCP server")?;

        let tools_response: McpResponse = serde_json::from_str(&tools_response_str)
            .context("Failed to parse tools response from stdio MCP server")?;

        // Kill the process
        let _ = child.kill();

        if let Some(error) = tools_response.error {
            return Err(anyhow::anyhow!(
                "MCP server tools/list error: {} - {}",
                error.code,
                error.message
            ));
        }

        let result = tools_response
            .result
            .ok_or_else(|| anyhow::anyhow!("MCP server returned no result for tools/list"))?;

        let tools_list: serde_json::Value = result
            .get("tools")
            .ok_or_else(|| anyhow::anyhow!("MCP server response missing 'tools' field"))?
            .clone();

        let tools: Vec<McpTool> = serde_json::from_value(tools_list)
            .context("Failed to parse tools from MCP server response")?;

        if self.debug_mode {
            debug_success!(
                "Retrieved {} tools from stdio MCP server '{}'",
                tools.len(),
                config.name
            );
        }

        Ok(tools)
    }

    /// Read and filter MCP protocol response from mixed stdout (logs + protocol)
    fn read_mcp_response(
        &self,
        reader: &mut BufReader<ChildStdout>,
        operation: &str,
    ) -> Result<String> {
        let mut line = String::new();
        let mut attempts = 0;
        const MAX_ATTEMPTS: usize = 50; // Prevent infinite loops

        loop {
            attempts += 1;
            if attempts > MAX_ATTEMPTS {
                return Err(anyhow::anyhow!(
                    "Timeout waiting for MCP response during {}",
                    operation
                ));
            }

            line.clear();
            let bytes_read = reader
                .read_line(&mut line)
                .context("Failed to read line from stdio process")?;

            if bytes_read == 0 {
                return Err(anyhow::anyhow!(
                    "Unexpected EOF from stdio MCP server during {}",
                    operation
                ));
            }

            let trimmed = line.trim();

            // Skip empty lines
            if trimmed.is_empty() {
                continue;
            }

            // Check if this looks like a JSON-RPC response
            if trimmed.starts_with('{') && trimmed.contains("jsonrpc") {
                // Try to parse as JSON to verify it's valid
                match serde_json::from_str::<serde_json::Value>(trimmed) {
                    Ok(json) => {
                        // Check if it has the expected JSON-RPC structure
                        if json.get("jsonrpc").is_some()
                            && (json.get("result").is_some() || json.get("error").is_some())
                        {
                            if self.debug_mode {
                                debug_print!("Found valid MCP response for {}", operation);
                            }
                            return Ok(trimmed.to_string());
                        }
                    }
                    Err(_) => {
                        // Not valid JSON, might be a log message that starts with '{'
                        if self.debug_mode {
                            debug_print!("Skipping invalid JSON line: {}", trimmed);
                        }
                        continue;
                    }
                }
            }

            // Skip lines that look like structured logs
            if self.is_log_message(trimmed) {
                if self.debug_mode {
                    debug_print!("Skipping log message: {}", trimmed);
                }
                continue;
            }

            // Skip other non-protocol messages
            if self.debug_mode {
                debug_print!("Skipping non-protocol line: {}", trimmed);
            }
        }
    }

    /// Check if a line looks like a structured log message rather than MCP protocol
    fn is_log_message(&self, line: &str) -> bool {
        // Common log patterns
        if line.contains("\"level\":")
            || line.contains("\"timestamp\":")
            || line.contains("\"message\":")
            || line.contains("\"time\":")
            || line.contains("\"msg\":")
        {
            return true;
        }

        // Specific patterns from solana-mcp-server
        if line.contains("Starting Solana MCP server")
            || line.contains("Loaded config:")
            || line.contains("Opened stdio transport")
            || line.contains("Starting message loop")
        {
            return true;
        }

        // JSON that doesn't look like JSON-RPC
        if line.starts_with('{') && !line.contains("jsonrpc") {
            return true;
        }

        false
    }

    /// Call a tool on an MCP server
    pub async fn call_tool(
        &self,
        server_id: &str,
        tool_name: &str,
        arguments: Option<serde_json::Value>,
    ) -> Result<serde_json::Value> {
        println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("🔧 MCP SERVICE call_tool ENTRY");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("📍 Server ID: {}", server_id);
        println!("📍 Tool Name: {}", tool_name);
        println!("📍 use_ephemeral_vms: {}", self.use_ephemeral_vms);
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        let config = self
            .servers
            .get(server_id)
            .ok_or_else(|| anyhow::anyhow!("Server '{}' not found", server_id))?;

        if !config.enabled {
            return Err(anyhow::anyhow!("Server '{}' is disabled", server_id));
        }

        // Priority 1: Use ephemeral microVM for tool execution if enabled
        if self.use_ephemeral_vms {
            println!("🔀 Taking Priority 1: Ephemeral microVM path\n");
            if self.debug_mode {
                debug_print!(
                    "Launching ephemeral microVM for tool: {}/{}",
                    server_id,
                    tool_name
                );
            }
            return self
                .ephemeral_vm_manager
                .launch_tool_vm(server_id, &config.url, tool_name, arguments)
                .await
                .context("Failed to execute tool in ephemeral VM");
        }

        // Priority 2: Check if server is running in dedicated microVM
        if let Some(handle) = self.mcp_server_microvms.get(server_id) {
            println!("🔀 Taking Priority 2: Dedicated microVM path\n");
            return self
                .call_tool_via_microvm(handle, tool_name, arguments)
                .await;
        }

        // Priority 3: Check if this tool should be executed in ephemeral unikernel
        // DISABLED: Skip unikernel isolation when use_ephemeral_vms is false
        if false
            && self
                .isolation_config
                .should_use_unikernel(server_id, tool_name)
        {
            println!("🔀 Taking Priority 3: Unikernel path\n");
            if self.debug_mode {
                debug_print!(
                    "Executing tool '{}' in ephemeral unikernel for enhanced security",
                    tool_name
                );
            }

            return self
                .call_tool_unikernel(server_id, tool_name, arguments)
                .await;
        }

        // Priority 4: Direct execution based on transport type
        println!(
            "🔀 Taking Priority 4: Direct execution via transport = {:?}\n",
            config.transport_type
        );
        match config.transport_type {
            McpTransportType::Http | McpTransportType::Websocket => {
                self.call_tool_http(server_id, tool_name, arguments, config)
                    .await
            }
            McpTransportType::Stdio => {
                self.call_tool_stdio(server_id, tool_name, arguments, config)
                    .await
            }
        }
    }

    /// Call a tool via dedicated microVM handle
    async fn call_tool_via_microvm(
        &self,
        handle: &McpServerMicroVmHandle,
        tool_name: &str,
        arguments: Option<serde_json::Value>,
    ) -> Result<serde_json::Value> {
        if self.debug_mode {
            debug_print!(
                "Calling tool '{}' via dedicated microVM '{}'",
                tool_name,
                handle.server_id()
            );
        }

        let request = serde_json::json!({
            "jsonrpc": "2.0",
            "id": self.next_request_id(),
            "method": "tools/call",
            "params": {
                "name": tool_name,
                "arguments": arguments
            }
        });

        let response = handle
            .send_request(request)
            .await
            .context("Failed to call tool via microVM")?;

        // Check for JSON-RPC error
        if let Some(error) = response.get("error") {
            return Err(anyhow!("Tool execution error: {:?}", error));
        }

        let result = response
            .get("result")
            .ok_or_else(|| anyhow!("No result in response"))?
            .clone();

        if self.debug_mode {
            debug_success!(
                "Successfully called tool '{}' via microVM '{}'",
                tool_name,
                handle.server_id()
            );
        }

        Ok(result)
    }

    /// Call a tool in an isolated unikernel
    async fn call_tool_unikernel(
        &self,
        server_id: &str,
        tool_name: &str,
        arguments: Option<serde_json::Value>,
    ) -> Result<serde_json::Value> {
        // Get tool configuration from isolation config
        let tool_config = self.isolation_config.get_tool_config(server_id, tool_name);

        // Get unikernel image path, using default if not specified
        let image_path = if let Some(ref image) = tool_config.unikernel_image {
            PathBuf::from(image)
        } else {
            // Default unikernel image path
            PathBuf::from(&self.isolation_config.unikernel_dir)
                .join(format!("{}-{}.img", server_id, tool_name))
        };

        // Build unikernel configuration
        use crate::services::unikernel_runtime::UnikernelLauncher;

        let unikernel_config = UnikernelConfig {
            image_path,
            mounts: tool_config.mounts.clone(),
            memory_mb: tool_config.memory_mb,
            vcpus: tool_config.vcpus,
            tool_name: tool_name.to_string(),
            server_id: server_id.to_string(),
            launcher: UnikernelLauncher::Unikraft,
            kraft_config: None,
            vsock_cid: None, // Will be auto-allocated
        };

        if self.debug_mode {
            debug_print!(
                "Spawning unikernel for tool '{}' with {} MB memory, {} vCPUs",
                tool_name,
                tool_config.memory_mb,
                tool_config.vcpus
            );
        }

        // Spawn the unikernel
        let handle = self
            .unikernel_runtime
            .spawn_unikernel(unikernel_config)
            .await
            .context("Failed to spawn unikernel for tool execution")?;

        if self.debug_mode {
            debug_success!("Unikernel spawned successfully for tool '{}'", tool_name);
        }

        // Execute the tool in the unikernel
        let result = handle
            .execute_tool(tool_name, arguments, &self.unikernel_runtime)
            .await
            .context("Failed to execute tool in unikernel")?;

        // Terminate the unikernel
        handle.terminate();

        if self.debug_mode {
            debug_success!(
                "Tool '{}' executed successfully in unikernel and terminated",
                tool_name
            );
        }

        Ok(result)
    }

    /// Call a tool on an HTTP/WebSocket MCP server
    async fn call_tool_http(
        &self,
        server_id: &str,
        tool_name: &str,
        arguments: Option<serde_json::Value>,
        config: &McpServerConfig,
    ) -> Result<serde_json::Value> {
        let endpoint_id = EndpointId {
            service: "mcp".to_string(),
            endpoint: config.url.clone(),
        };

        if !self.circuit_breaker.can_execute_endpoint(&endpoint_id) {
            return Err(anyhow::anyhow!(
                "Circuit breaker is open for MCP server '{}'",
                config.name
            ));
        }

        let tool_call = serde_json::json!({
            "name": tool_name,
            "arguments": arguments
        });

        let request = McpRequest {
            jsonrpc: "2.0".to_string(),
            id: self
                .request_counter
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst),
            method: MCP_METHOD_TOOLS_CALL.to_string(),
            params: Some(tool_call.clone()),
        };

        // 🔍 DETAILED DEBUG LOGGING
        println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("🔧 MCP TOOL CALL DEBUG TRACE");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("📍 Server ID: {}", server_id);
        println!("📍 Server Name: {}", config.name);
        println!("📍 Server URL: {}", config.url);
        println!("📍 Transport: HTTP");
        println!("📍 Tool Name: {}", tool_name);
        println!("\n📦 Arguments Sent:");
        if let Some(ref args) = arguments {
            println!(
                "{}",
                serde_json::to_string_pretty(args)
                    .unwrap_or_else(|_| "Failed to serialize".to_string())
            );
        } else {
            println!("  (no arguments)");
        }
        println!("\n📤 Full Request:");
        println!(
            "{}",
            serde_json::to_string_pretty(&request)
                .unwrap_or_else(|_| "Failed to serialize".to_string())
        );
        println!("\n🌐 Endpoint: {}/api/mcp", config.url);

        if let Some(auth) = &config.auth {
            println!("🔐 Auth Type: {}", auth.auth_type);
            if auth.token.is_some() {
                println!(
                    "🔐 Auth Token: <present, {} chars>",
                    auth.token.as_ref().unwrap().len()
                );
            }
        } else {
            println!("🔐 Auth: None");
        }
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        let mut req_builder = self
            .client
            .post(format!("{}/api/mcp", config.url))
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

        let start_time = std::time::Instant::now();
        let response = req_builder
            .send()
            .await
            .context("Failed to send tools/call request to MCP server")?;

        let elapsed = start_time.elapsed();
        let status = response.status();

        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("📥 MCP RESPONSE DEBUG TRACE");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("⏱️  Response Time: {:?}", elapsed);
        println!(
            "📊 HTTP Status: {} {}",
            status.as_u16(),
            status.canonical_reason().unwrap_or("")
        );
        println!("📊 Success: {}", status.is_success());

        if !response.status().is_success() {
            let error_body = response
                .text()
                .await
                .unwrap_or_else(|_| "<failed to read body>".to_string());
            println!("❌ Error Response Body:");
            println!("{}", error_body);
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

            self.circuit_breaker.on_failure_endpoint(&endpoint_id);
            return Err(anyhow::anyhow!(
                "MCP server returned error status: {} - Body: {}",
                status,
                error_body
            ));
        }

        let response_text = response
            .text()
            .await
            .context("Failed to read response body")?;

        println!("📦 Raw Response Body ({} bytes):", response_text.len());
        if response_text.len() < 2000 {
            println!("{}", response_text);
        } else {
            println!(
                "{}... (truncated, {} total bytes)",
                &response_text[..2000],
                response_text.len()
            );
        }
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        let mcp_response: McpResponse = serde_json::from_str(&response_text)
            .context("Failed to parse MCP tools/call response")?;

        if let Some(error) = mcp_response.error {
            self.circuit_breaker.on_failure_endpoint(&endpoint_id);

            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("❌ MCP PROTOCOL ERROR");
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("Error Code: {}", error.code);
            println!("Error Message: {}", error.message);
            if let Some(data) = &error.data {
                println!("Error Data:");
                println!(
                    "{}",
                    serde_json::to_string_pretty(data).unwrap_or_else(|_| format!("{:?}", data))
                );
            }
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

            return Err(anyhow::anyhow!(
                "MCP server tools/call error: {} - {}",
                error.code,
                error.message
            ));
        }

        let result = mcp_response
            .result
            .ok_or_else(|| anyhow::anyhow!("MCP server returned no result for tools/call"))?;

        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("✅ MCP SUCCESS RESULT");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!(
            "{}",
            serde_json::to_string_pretty(&result).unwrap_or_else(|_| format!("{:?}", result))
        );
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        self.circuit_breaker.on_success_endpoint(&endpoint_id);

        if self.debug_mode {
            debug_success!(
                "Successfully called tool '{}' on MCP server '{}'",
                tool_name,
                config.name
            );
        }

        Ok(result)
    }

    /// Call a tool on a stdio-based MCP server
    async fn call_tool_stdio(
        &self,
        server_id: &str,
        tool_name: &str,
        arguments: Option<serde_json::Value>,
        config: &McpServerConfig,
    ) -> Result<serde_json::Value> {
        // 🔍 DETAILED DEBUG LOGGING FOR STDIO
        println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("🔧 MCP STDIO TOOL CALL DEBUG TRACE");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("📍 Server ID: {}", server_id);
        println!("📍 Server Name: {}", config.name);
        println!("📍 Server URL/Command: {}", config.url);
        println!("📍 Transport: STDIO");
        println!("📍 Tool Name: {}", tool_name);
        println!("\n📦 Arguments Sent:");
        if let Some(ref args) = arguments {
            println!(
                "{}",
                serde_json::to_string_pretty(args)
                    .unwrap_or_else(|_| "Failed to serialize".to_string())
            );
        } else {
            println!("  (no arguments)");
        }
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        if self.debug_mode {
            debug_print!(
                "Calling tool '{}' on stdio MCP server: {}",
                tool_name,
                config.name
            );
        }

        // Parse the command from the URL field, stripping the stdio:// prefix if present
        let command_str = if config.url.starts_with("stdio://") {
            &config.url[8..] // Skip "stdio://"
        } else {
            &config.url
        };

        let mut parts = command_str.split_whitespace();
        let program = parts
            .next()
            .ok_or_else(|| anyhow::anyhow!("Invalid stdio command: {}", config.url))?;
        let args: Vec<&str> = parts.collect();

        println!("🚀 Spawning Process:");
        println!("   Program: {}", program);
        println!("   Args: {:?}", args);
        println!();

        let start_time = std::time::Instant::now();

        // Create the process
        let mut child = Command::new(program)
            .args(&args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .with_context(|| {
                format!(
                    "Failed to spawn stdio MCP server process: program='{}' args={:?}",
                    program, args
                )
            })?;

        let mut stdin = child
            .stdin
            .take()
            .ok_or_else(|| anyhow::anyhow!("Failed to get stdin for stdio process"))?;
        let mut stdout = child
            .stdout
            .take()
            .ok_or_else(|| anyhow::anyhow!("Failed to get stdout for stdio process"))?;

        // Send initialize request first
        let init_request = McpRequest {
            jsonrpc: MCP_JSONRPC_VERSION.to_string(),
            id: 1,
            method: MCP_METHOD_INITIALIZE.to_string(),
            params: Some(serde_json::json!({
                "protocolVersion": MCP_PROTOCOL_VERSION,
                "capabilities": {},
                "clientInfo": {
                    "name": CLIENT_NAME,
                    "version": env!("CARGO_PKG_VERSION")
                }
            })),
        };

        let init_request_str = serde_json::to_string(&init_request)?;

        println!("📤 Sending Initialize Request:");
        println!(
            "{}",
            serde_json::to_string_pretty(&init_request)
                .unwrap_or_else(|_| init_request_str.clone())
        );
        println!();

        stdin.write_all(init_request_str.as_bytes())?;
        stdin.write_all(b"\n")?;
        stdin.flush()?;

        // Read initialize response
        let mut reader = BufReader::new(stdout);
        let init_response_str = self
            .read_mcp_response(&mut reader, "initialization")
            .context("Failed to read initialization response from stdio MCP server")?;

        println!("📥 Initialize Response Received:");
        println!("{}", init_response_str);
        println!();

        let _init_response: McpResponse = serde_json::from_str(&init_response_str)
            .context("Failed to parse initialize response from stdio MCP server")?;

        // Now send the tools/call request
        let tool_call = serde_json::json!({
            "name": tool_name,
            "arguments": arguments
        });

        let call_request = McpRequest {
            jsonrpc: MCP_JSONRPC_VERSION.to_string(),
            id: 2,
            method: MCP_METHOD_TOOLS_CALL.to_string(),
            params: Some(tool_call.clone()),
        };

        let call_request_str = serde_json::to_string(&call_request)?;

        println!("📤 Sending Tool Call Request:");
        println!(
            "{}",
            serde_json::to_string_pretty(&call_request)
                .unwrap_or_else(|_| call_request_str.clone())
        );
        println!();

        stdin.write_all(call_request_str.as_bytes())?;
        stdin.write_all(b"\n")?;
        stdin.flush()?;

        // Read tool call response with filtering
        let call_response_str = self
            .read_mcp_response(&mut reader, "tools/call")
            .context("Failed to read tools/call response from stdio MCP server")?;

        let elapsed = start_time.elapsed();

        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("📥 MCP STDIO RESPONSE DEBUG TRACE");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("⏱️  Response Time: {:?}", elapsed);
        println!("\n📦 Raw Response ({} bytes):", call_response_str.len());
        if call_response_str.len() < 5000 {
            println!("{}", call_response_str);
        } else {
            println!(
                "{}... (truncated, {} total bytes)",
                &call_response_str[..5000],
                call_response_str.len()
            );
        }
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        let call_response: McpResponse = serde_json::from_str(&call_response_str)
            .context("Failed to parse tools/call response from stdio MCP server")?;

        // Kill the process
        let _ = child.kill();

        if let Some(error) = call_response.error {
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("❌ MCP STDIO PROTOCOL ERROR");
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("Error Code: {}", error.code);
            println!("Error Message: {}", error.message);
            if let Some(data) = &error.data {
                println!("Error Data:");
                println!(
                    "{}",
                    serde_json::to_string_pretty(data).unwrap_or_else(|_| format!("{:?}", data))
                );
            }
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

            return Err(anyhow::anyhow!(
                "MCP server tools/call error: {} - {}",
                error.code,
                error.message
            ));
        }

        let result = call_response
            .result
            .ok_or_else(|| anyhow::anyhow!("MCP server returned no result for tools/call"))?;

        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("✅ MCP STDIO SUCCESS RESULT");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        let result_str =
            serde_json::to_string_pretty(&result).unwrap_or_else(|_| format!("{:?}", result));
        if result_str.len() < 5000 {
            println!("{}", result_str);
        } else {
            println!(
                "{}... (truncated, {} total bytes)",
                &result_str[..5000],
                result_str.len()
            );
        }
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        if self.debug_mode {
            debug_success!(
                "Successfully called tool '{}' on stdio MCP server '{}'",
                tool_name,
                config.name
            );
        }

        Ok(result)
    }

    /// Query multiple MCP servers with the same request
    pub async fn query_all_servers(
        &self,
        tool_name: &str,
        arguments: Option<serde_json::Value>,
    ) -> Vec<(String, Result<serde_json::Value>)> {
        let mut results = Vec::new();

        for (server_id, config) in &self.servers {
            if !config.enabled {
                continue;
            }

            let result = self
                .call_tool(server_id, tool_name, arguments.clone())
                .await;
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
        self.initialize_server(server_id)
            .await
            .context("Failed to initialize MCP server")?;

        // Then try to list tools as a connectivity test
        let _tools = self
            .list_tools(server_id)
            .await
            .context("Failed to list tools from MCP server")?;

        if self.debug_mode {
            debug_success!("MCP server '{}' connectivity test passed", server_id);
        }

        Ok(())
    }

    /// Search for MCP servers by name, description, or features  
    pub fn search_servers(
        &self,
        query: &str,
        transport_filter: Option<&str>,
        enabled_only: bool,
    ) -> Vec<(&String, &McpServerConfig)> {
        let query_lower = query.to_lowercase();

        self.servers
            .iter()
            .filter(|(_, config)| {
                // Filter by enabled status if requested
                if enabled_only && !config.enabled {
                    return false;
                }

                // Filter by transport type if specified
                if let Some(transport) = transport_filter {
                    if transport != "any" {
                        let config_transport = match config.transport_type {
                            McpTransportType::Http => "http",
                            McpTransportType::Websocket => "websocket",
                            McpTransportType::Stdio => "stdio",
                        };
                        if config_transport != transport {
                            return false;
                        }
                    }
                }

                // Search in name, URL, and GitHub URL
                let name_matches = config.name.to_lowercase().contains(&query_lower);
                let url_matches = config.url.to_lowercase().contains(&query_lower);
                let github_matches = config
                    .github_url
                    .as_ref()
                    .map(|url| url.to_lowercase().contains(&query_lower))
                    .unwrap_or(false);

                // Also search in auth type if configured
                let auth_matches = config
                    .auth
                    .as_ref()
                    .map(|auth| auth.auth_type.to_lowercase().contains(&query_lower))
                    .unwrap_or(false);

                name_matches || url_matches || github_matches || auth_matches
            })
            .collect()
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

    #[test]
    fn test_get_package_info_from_package_json() {
        use std::io::Write;

        let service = McpService::new();

        // Test package.json with simple main field
        let temp_dir = std::env::temp_dir();
        let package_json_path = temp_dir.join("test_package.json");

        let package_content = r#"{
            "name": "test-mcp-server",
            "version": "1.0.0",
            "main": "index.js"
        }"#;

        let mut file = std::fs::File::create(&package_json_path).unwrap();
        file.write_all(package_content.as_bytes()).unwrap();

        let result = service.get_package_info_from_package_json(&package_json_path);
        assert!(result.is_ok());
        let (name, main_script) = result.unwrap();
        assert_eq!(name, "test-mcp-server");
        assert_eq!(main_script, Some("index.js".to_string()));

        // Test package.json with bin field (string)
        let bin_content = r#"{
            "name": "bin-mcp-server",
            "version": "1.0.0",
            "bin": "bin/server.js"
        }"#;

        file = std::fs::File::create(&package_json_path).unwrap();
        file.write_all(bin_content.as_bytes()).unwrap();

        let result = service.get_package_info_from_package_json(&package_json_path);
        assert!(result.is_ok());
        let (name, main_script) = result.unwrap();
        assert_eq!(name, "bin-mcp-server");
        assert_eq!(main_script, Some("bin/server.js".to_string()));

        // Test package.json with bin field (object)
        let bin_obj_content = r#"{
            "name": "obj-mcp-server",
            "version": "1.0.0",
            "bin": {
                "obj-mcp-server": "bin/server.js",
                "other-cmd": "bin/other.js"
            }
        }"#;

        file = std::fs::File::create(&package_json_path).unwrap();
        file.write_all(bin_obj_content.as_bytes()).unwrap();

        let result = service.get_package_info_from_package_json(&package_json_path);
        assert!(result.is_ok());
        let (name, main_script) = result.unwrap();
        assert_eq!(name, "obj-mcp-server");
        assert_eq!(main_script, Some("bin/server.js".to_string()));

        // Cleanup
        std::fs::remove_file(&package_json_path).ok();
    }
}
