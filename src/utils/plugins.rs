//! Advanced plugin system for extending agent chat functionality
//!
//! This module provides a comprehensive plugin architecture that allows users
//! to extend the agent chat interface with custom commands, tools, and behaviors.

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use log::{debug, error, info, warn};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use tokio::process::Command as TokioCommand;

/// Plugin metadata and configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginManifest {
    pub name: String,
    pub version: String,
    pub description: String,
    pub author: String,
    pub license: String,
    pub homepage: Option<String>,
    pub repository: Option<String>,

    // Plugin behavior
    pub plugin_type: PluginType,
    pub entry_point: String,
    pub dependencies: Vec<String>,
    pub permissions: Vec<Permission>,

    // Compatibility
    pub min_osvm_version: String,
    pub supported_platforms: Vec<String>,

    // Configuration
    pub config_schema: Option<serde_json::Value>,
    pub default_config: Option<serde_json::Value>,
}

/// Types of plugins supported
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum PluginType {
    /// Command plugins add new slash commands
    Command,
    /// Tool plugins provide new MCP-style tools
    Tool,
    /// Theme plugins provide visual customizations
    Theme,
    /// Filter plugins process input/output
    Filter,
    /// Integration plugins connect to external services
    Integration,
    /// AI plugins provide custom AI capabilities
    AI,
    /// Composite plugins combine multiple functionalities
    Composite(Vec<PluginType>),
}

/// Plugin permissions system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Permission {
    /// Read files from specified directories
    ReadFiles(Vec<String>),
    /// Write files to specified directories
    WriteFiles(Vec<String>),
    /// Execute system commands
    ExecuteCommands,
    /// Network access to specified hosts
    NetworkAccess(Vec<String>),
    /// Access to environment variables
    EnvironmentAccess,
    /// Access to MCP servers
    MCPAccess,
    /// Access to AI services
    AIAccess,
    /// Access to user configuration
    ConfigAccess,
    /// All permissions (dangerous)
    All,
}

/// Plugin execution context
#[derive(Debug, Clone, Serialize)]
pub struct PluginContext {
    pub user_input: String,
    pub session_id: String,
    pub config: HashMap<String, serde_json::Value>,
    pub environment: HashMap<String, String>,
    pub working_directory: PathBuf,
}

/// Plugin execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginResult {
    pub success: bool,
    pub output: String,
    pub error: Option<String>,
    pub data: Option<serde_json::Value>,
    pub modified_input: Option<String>,
    pub suggestions: Vec<String>,
}

/// Plugin trait for dynamic behavior
#[async_trait]
pub trait Plugin: Send + Sync {
    /// Get plugin manifest
    fn manifest(&self) -> &PluginManifest;

    /// Initialize plugin
    async fn initialize(&mut self, config: HashMap<String, serde_json::Value>) -> Result<()>;

    /// Execute plugin with given context
    async fn execute(&self, context: PluginContext) -> Result<PluginResult>;

    /// Check if plugin can handle given input
    fn can_handle(&self, input: &str) -> bool;

    /// Get plugin health status
    async fn health_check(&self) -> Result<bool>;

    /// Cleanup plugin resources
    async fn cleanup(&mut self) -> Result<()>;
}

/// Built-in example plugins

/// Echo plugin - demonstrates basic command plugin
pub struct EchoPlugin {
    manifest: PluginManifest,
}

impl EchoPlugin {
    pub fn new() -> Self {
        Self {
            manifest: PluginManifest {
                name: "echo".to_string(),
                version: "1.0.0".to_string(),
                description: "Echo input text with optional formatting".to_string(),
                author: "OSVM Team".to_string(),
                license: "MIT".to_string(),
                homepage: Some("https://github.com/opensvm/osvm-cli".to_string()),
                repository: Some("https://github.com/opensvm/osvm-cli".to_string()),
                plugin_type: PluginType::Command,
                entry_point: "builtin://echo".to_string(),
                dependencies: vec![],
                permissions: vec![],
                min_osvm_version: "0.8.0".to_string(),
                supported_platforms: vec![
                    "linux".to_string(),
                    "macos".to_string(),
                    "windows".to_string(),
                ],
                config_schema: None,
                default_config: None,
            },
        }
    }
}

#[async_trait]
impl Plugin for EchoPlugin {
    fn manifest(&self) -> &PluginManifest {
        &self.manifest
    }

    async fn initialize(&mut self, _config: HashMap<String, serde_json::Value>) -> Result<()> {
        debug!("Echo plugin initialized");
        Ok(())
    }

    async fn execute(&self, context: PluginContext) -> Result<PluginResult> {
        let input = context.user_input.trim_start_matches("/echo").trim();

        Ok(PluginResult {
            success: true,
            output: format!("Echo: {}", input),
            error: None,
            data: Some(serde_json::json!({
                "original": input,
                "timestamp": chrono::Utc::now().to_rfc3339()
            })),
            modified_input: None,
            suggestions: vec![],
        })
    }

    fn can_handle(&self, input: &str) -> bool {
        input.starts_with("/echo")
    }

    async fn health_check(&self) -> Result<bool> {
        Ok(true)
    }

    async fn cleanup(&mut self) -> Result<()> {
        debug!("Echo plugin cleaned up");
        Ok(())
    }
}

/// Time plugin - shows current time with formatting options
pub struct TimePlugin {
    manifest: PluginManifest,
}

impl TimePlugin {
    pub fn new() -> Self {
        Self {
            manifest: PluginManifest {
                name: "time".to_string(),
                version: "1.0.0".to_string(),
                description: "Display current time with various formatting options".to_string(),
                author: "OSVM Team".to_string(),
                license: "MIT".to_string(),
                homepage: Some("https://github.com/opensvm/osvm-cli".to_string()),
                repository: Some("https://github.com/opensvm/osvm-cli".to_string()),
                plugin_type: PluginType::Command,
                entry_point: "builtin://time".to_string(),
                dependencies: vec![],
                permissions: vec![],
                min_osvm_version: "0.8.0".to_string(),
                supported_platforms: vec![
                    "linux".to_string(),
                    "macos".to_string(),
                    "windows".to_string(),
                ],
                config_schema: Some(serde_json::json!({
                    "type": "object",
                    "properties": {
                        "format": {
                            "type": "string",
                            "default": "%Y-%m-%d %H:%M:%S UTC"
                        },
                        "timezone": {
                            "type": "string",
                            "default": "UTC"
                        }
                    }
                })),
                default_config: Some(serde_json::json!({
                    "format": "%Y-%m-%d %H:%M:%S UTC",
                    "timezone": "UTC"
                })),
            },
        }
    }
}

#[async_trait]
impl Plugin for TimePlugin {
    fn manifest(&self) -> &PluginManifest {
        &self.manifest
    }

    async fn initialize(&mut self, _config: HashMap<String, serde_json::Value>) -> Result<()> {
        debug!("Time plugin initialized");
        Ok(())
    }

    async fn execute(&self, context: PluginContext) -> Result<PluginResult> {
        let format = context
            .config
            .get("format")
            .and_then(|v| v.as_str())
            .unwrap_or("%Y-%m-%d %H:%M:%S UTC");

        let now = chrono::Utc::now();
        let formatted = now.format(format);

        Ok(PluginResult {
            success: true,
            output: format!("Current time: {}", formatted),
            error: None,
            data: Some(serde_json::json!({
                "timestamp": now.to_rfc3339(),
                "unix": now.timestamp(),
                "format": format
            })),
            modified_input: None,
            suggestions: vec![
                "/time --utc".to_string(),
                "/time --local".to_string(),
                "/time --unix".to_string(),
            ],
        })
    }

    fn can_handle(&self, input: &str) -> bool {
        input.starts_with("/time")
    }

    async fn health_check(&self) -> Result<bool> {
        Ok(true)
    }

    async fn cleanup(&mut self) -> Result<()> {
        debug!("Time plugin cleaned up");
        Ok(())
    }
}

/// External plugin that executes scripts/binaries
pub struct ExternalPlugin {
    manifest: PluginManifest,
    executable_path: PathBuf,
    config: HashMap<String, serde_json::Value>,
}

impl ExternalPlugin {
    pub fn from_manifest(manifest_path: &PathBuf) -> Result<Self> {
        let content = fs::read_to_string(manifest_path)
            .map_err(|e| anyhow!("Failed to read plugin manifest: {}", e))?;

        let manifest: PluginManifest = serde_json::from_str(&content)
            .map_err(|e| anyhow!("Failed to parse plugin manifest: {}", e))?;

        let plugin_dir = manifest_path
            .parent()
            .ok_or_else(|| anyhow!("Invalid manifest path"))?;

        let executable_path = plugin_dir.join(&manifest.entry_point);

        Ok(Self {
            manifest,
            executable_path,
            config: HashMap::new(),
        })
    }
}

#[async_trait]
impl Plugin for ExternalPlugin {
    fn manifest(&self) -> &PluginManifest {
        &self.manifest
    }

    async fn initialize(&mut self, config: HashMap<String, serde_json::Value>) -> Result<()> {
        self.config = config;
        debug!("External plugin '{}' initialized", self.manifest.name);
        Ok(())
    }

    async fn execute(&self, context: PluginContext) -> Result<PluginResult> {
        // Prepare execution environment
        let input_json = serde_json::to_string(&context)
            .map_err(|e| anyhow!("Failed to serialize context: {}", e))?;

        // Execute plugin
        let mut cmd = TokioCommand::new(&self.executable_path);
        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        // Add environment variables
        for (key, value) in &context.environment {
            cmd.env(key, value);
        }

        let mut child = cmd
            .spawn()
            .map_err(|e| anyhow!("Failed to spawn plugin process: {}", e))?;

        // Send input to plugin
        if let Some(stdin) = child.stdin.as_mut() {
            use tokio::io::AsyncWriteExt;
            stdin
                .write_all(input_json.as_bytes())
                .await
                .map_err(|e| anyhow!("Failed to write to plugin stdin: {}", e))?;
            stdin
                .shutdown()
                .await
                .map_err(|e| anyhow!("Failed to close plugin stdin: {}", e))?;
        }

        // Wait for completion
        let output = child
            .wait_with_output()
            .await
            .map_err(|e| anyhow!("Plugin execution failed: {}", e))?;

        // Parse result
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);

            // Try to parse as JSON first
            if let Ok(result) = serde_json::from_str::<PluginResult>(&stdout) {
                Ok(result)
            } else {
                // Fallback to plain text output
                Ok(PluginResult {
                    success: true,
                    output: stdout.to_string(),
                    error: None,
                    data: None,
                    modified_input: None,
                    suggestions: vec![],
                })
            }
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            Ok(PluginResult {
                success: false,
                output: String::new(),
                error: Some(stderr.to_string()),
                data: None,
                modified_input: None,
                suggestions: vec![],
            })
        }
    }

    fn can_handle(&self, input: &str) -> bool {
        // Check if input matches plugin's command pattern
        match &self.manifest.plugin_type {
            PluginType::Command => input.starts_with(&format!("/{}", self.manifest.name)),
            PluginType::Filter => true, // Filters can process any input
            _ => false,
        }
    }

    async fn health_check(&self) -> Result<bool> {
        Ok(self.executable_path.exists() && self.executable_path.is_file())
    }

    async fn cleanup(&mut self) -> Result<()> {
        debug!("External plugin '{}' cleaned up", self.manifest.name);
        Ok(())
    }
}

/// Plugin manager handles loading, lifecycle, and execution of plugins
pub struct PluginManager {
    plugins: HashMap<String, Box<dyn Plugin>>,
    plugin_configs: HashMap<String, HashMap<String, serde_json::Value>>,
    enabled_plugins: Vec<String>,
}

impl PluginManager {
    /// Create new plugin manager
    pub fn new() -> Self {
        Self {
            plugins: HashMap::new(),
            plugin_configs: HashMap::new(),
            enabled_plugins: Vec::new(),
        }
    }

    /// Initialize with built-in plugins
    pub async fn with_builtin_plugins() -> Result<Self> {
        let mut manager = Self::new();

        // Add built-in plugins
        manager.register_plugin(Box::new(EchoPlugin::new())).await?;
        manager.register_plugin(Box::new(TimePlugin::new())).await?;

        // Enable built-in plugins by default
        manager
            .enabled_plugins
            .extend(vec!["echo".to_string(), "time".to_string()]);

        info!(
            "Plugin manager initialized with {} built-in plugins",
            manager.plugins.len()
        );
        Ok(manager)
    }

    /// Register a plugin
    pub async fn register_plugin(&mut self, mut plugin: Box<dyn Plugin>) -> Result<()> {
        let name = plugin.manifest().name.clone();

        // Initialize plugin with default config
        if let Some(default_config) = &plugin.manifest().default_config {
            if let Ok(config) =
                serde_json::from_value::<HashMap<String, serde_json::Value>>(default_config.clone())
            {
                plugin.initialize(config.clone()).await?;
                self.plugin_configs.insert(name.clone(), config);
            }
        } else {
            plugin.initialize(HashMap::new()).await?;
        }

        self.plugins.insert(name.clone(), plugin);
        debug!("Registered plugin: {}", name);

        Ok(())
    }

    /// Load plugins from directory
    pub async fn load_plugins_from_directory(&mut self, dir: &PathBuf) -> Result<()> {
        if !dir.exists() {
            warn!("Plugin directory does not exist: {:?}", dir);
            return Ok(());
        }

        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                let manifest_path = path.join("plugin.json");
                if manifest_path.exists() {
                    match ExternalPlugin::from_manifest(&manifest_path) {
                        Ok(plugin) => {
                            info!("Loading external plugin from {:?}", path);
                            self.register_plugin(Box::new(plugin)).await?;
                        }
                        Err(e) => {
                            warn!("Failed to load plugin from {:?}: {}", path, e);
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Execute input through appropriate plugins
    pub async fn execute(&self, input: &str, context: PluginContext) -> Result<Vec<PluginResult>> {
        let mut results = Vec::new();

        for plugin_name in &self.enabled_plugins {
            if let Some(plugin) = self.plugins.get(plugin_name) {
                if plugin.can_handle(input) {
                    debug!("Executing plugin: {}", plugin_name);
                    match plugin.execute(context.clone()).await {
                        Ok(result) => {
                            results.push(result);

                            // For command plugins, stop after first successful execution
                            if matches!(plugin.manifest().plugin_type, PluginType::Command)
                                && results.last().unwrap().success
                            {
                                break;
                            }
                        }
                        Err(e) => {
                            error!("Plugin '{}' execution failed: {}", plugin_name, e);
                            results.push(PluginResult {
                                success: false,
                                output: String::new(),
                                error: Some(e.to_string()),
                                data: None,
                                modified_input: None,
                                suggestions: vec![],
                            });
                        }
                    }
                }
            }
        }

        Ok(results)
    }

    /// Get available commands from all plugins
    pub fn get_available_commands(&self) -> Vec<String> {
        let mut commands = Vec::new();

        for (name, plugin) in &self.plugins {
            if self.enabled_plugins.contains(name) {
                match plugin.manifest().plugin_type {
                    PluginType::Command => {
                        commands.push(format!("/{}", plugin.manifest().name));
                    }
                    PluginType::Composite(ref types) => {
                        if types.contains(&PluginType::Command) {
                            commands.push(format!("/{}", plugin.manifest().name));
                        }
                    }
                    _ => {}
                }
            }
        }

        commands.sort();
        commands
    }

    /// Enable plugin
    pub fn enable_plugin(&mut self, name: &str) -> Result<()> {
        if self.plugins.contains_key(name) {
            if !self.enabled_plugins.contains(&name.to_string()) {
                self.enabled_plugins.push(name.to_string());
                info!("Enabled plugin: {}", name);
            }
            Ok(())
        } else {
            Err(anyhow!("Plugin '{}' not found", name))
        }
    }

    /// Disable plugin
    pub fn disable_plugin(&mut self, name: &str) {
        self.enabled_plugins.retain(|n| n != name);
        info!("Disabled plugin: {}", name);
    }

    /// List all registered plugins
    pub fn list_plugins(&self) -> Vec<&PluginManifest> {
        self.plugins.values().map(|p| p.manifest()).collect()
    }

    /// Get plugin by name
    pub fn get_plugin(&self, name: &str) -> Option<&dyn Plugin> {
        self.plugins.get(name).map(|p| p.as_ref())
    }

    /// Health check all plugins
    pub async fn health_check_all(&self) -> HashMap<String, bool> {
        let mut results = HashMap::new();

        for (name, plugin) in &self.plugins {
            match plugin.health_check().await {
                Ok(healthy) => {
                    results.insert(name.clone(), healthy);
                }
                Err(e) => {
                    warn!("Health check failed for plugin '{}': {}", name, e);
                    results.insert(name.clone(), false);
                }
            }
        }

        results
    }

    /// Save plugin configuration
    pub async fn save_config(&self) -> Result<()> {
        let config_path = Self::config_path()?;

        if let Some(parent) = config_path.parent() {
            fs::create_dir_all(parent)?;
        }

        let config = serde_json::json!({
            "enabled_plugins": self.enabled_plugins,
            "plugin_configs": self.plugin_configs
        });

        fs::write(&config_path, serde_json::to_string_pretty(&config)?)?;
        debug!("Saved plugin configuration to {:?}", config_path);

        Ok(())
    }

    /// Load plugin configuration
    pub async fn load_config(&mut self) -> Result<()> {
        let config_path = Self::config_path()?;

        if !config_path.exists() {
            debug!("No plugin configuration found, using defaults");
            return Ok(());
        }

        let content = fs::read_to_string(&config_path)?;
        let config: serde_json::Value = serde_json::from_str(&content)?;

        if let Some(enabled) = config.get("enabled_plugins").and_then(|v| v.as_array()) {
            self.enabled_plugins = enabled
                .iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect();
        }

        if let Some(configs) = config.get("plugin_configs").and_then(|v| v.as_object()) {
            for (name, plugin_config) in configs {
                if let Ok(config_map) = serde_json::from_value::<HashMap<String, serde_json::Value>>(
                    plugin_config.clone(),
                ) {
                    self.plugin_configs.insert(name.clone(), config_map);
                }
            }
        }

        debug!("Loaded plugin configuration from {:?}", config_path);
        Ok(())
    }

    /// Get configuration file path
    fn config_path() -> Result<PathBuf> {
        let home =
            std::env::var("HOME").map_err(|_| anyhow!("HOME environment variable not set"))?;
        Ok(PathBuf::from(home).join(".osvm").join("plugins.json"))
    }

    /// Get plugins directory path
    pub fn plugins_dir() -> Result<PathBuf> {
        let home =
            std::env::var("HOME").map_err(|_| anyhow!("HOME environment variable not set"))?;
        Ok(PathBuf::from(home).join(".osvm").join("plugins"))
    }
}

/// Plugin installer for managing external plugins
pub struct PluginInstaller {
    plugins_dir: PathBuf,
}

impl PluginInstaller {
    pub fn new() -> Result<Self> {
        let plugins_dir = PluginManager::plugins_dir()?;
        fs::create_dir_all(&plugins_dir)?;

        Ok(Self { plugins_dir })
    }

    /// Install plugin from GitHub repository
    pub async fn install_from_github(&self, repo_url: &str) -> Result<String> {
        let repo_name = Self::extract_repo_name(repo_url)?;
        let plugin_dir = self.plugins_dir.join(&repo_name);

        if plugin_dir.exists() {
            return Err(anyhow!("Plugin '{}' already exists", repo_name));
        }

        // Clone repository
        let output = Command::new("git")
            .args(&["clone", repo_url, &plugin_dir.to_string_lossy()])
            .output()
            .map_err(|e| anyhow!("Failed to clone repository: {}", e))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("Git clone failed: {}", stderr));
        }

        // Verify plugin manifest exists
        let manifest_path = plugin_dir.join("plugin.json");
        if !manifest_path.exists() {
            fs::remove_dir_all(&plugin_dir)?;
            return Err(anyhow!("No plugin.json found in repository"));
        }

        // Run plugin installation script if present
        let install_script = plugin_dir.join("install.sh");
        if install_script.exists() {
            let output = Command::new("bash")
                .arg(&install_script)
                .current_dir(&plugin_dir)
                .output()
                .map_err(|e| anyhow!("Failed to run install script: {}", e))?;

            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                warn!("Install script failed: {}", stderr);
            }
        }

        info!(
            "Successfully installed plugin '{}' from {}",
            repo_name, repo_url
        );
        Ok(repo_name)
    }

    /// Uninstall plugin
    pub async fn uninstall(&self, plugin_name: &str) -> Result<()> {
        let plugin_dir = self.plugins_dir.join(plugin_name);

        if !plugin_dir.exists() {
            return Err(anyhow!("Plugin '{}' not found", plugin_name));
        }

        // Run uninstall script if present
        let uninstall_script = plugin_dir.join("uninstall.sh");
        if uninstall_script.exists() {
            let output = Command::new("bash")
                .arg(&uninstall_script)
                .current_dir(&plugin_dir)
                .output()
                .map_err(|e| anyhow!("Failed to run uninstall script: {}", e))?;

            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                warn!("Uninstall script failed: {}", stderr);
            }
        }

        // Remove plugin directory
        fs::remove_dir_all(&plugin_dir)?;

        info!("Successfully uninstalled plugin '{}'", plugin_name);
        Ok(())
    }

    /// List installed plugins
    pub fn list_installed(&self) -> Result<Vec<String>> {
        let mut plugins = Vec::new();

        if !self.plugins_dir.exists() {
            return Ok(plugins);
        }

        for entry in fs::read_dir(&self.plugins_dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() && path.join("plugin.json").exists() {
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    plugins.push(name.to_string());
                }
            }
        }

        plugins.sort();
        Ok(plugins)
    }

    /// Extract repository name from URL (public utility method)
    ///
    /// # Examples
    /// ```
    /// # use anyhow::Result;
    /// # fn main() -> Result<()> {
    /// use osvm::utils::plugins::PluginInstaller;
    ///
    /// let name = PluginInstaller::extract_repo_name("https://github.com/user/repo.git")?;
    /// assert_eq!(name, "repo");
    /// # Ok(())
    /// # }
    /// ```
    pub fn extract_repo_name(url: &str) -> Result<String> {
        let url = url.trim_end_matches('/');
        if let Some(name) = url.split('/').last() {
            let name = name.trim_end_matches(".git");
            if name.is_empty() {
                Err(anyhow!("Invalid repository URL"))
            } else {
                Ok(name.to_string())
            }
        } else {
            Err(anyhow!("Invalid repository URL"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_echo_plugin() {
        let plugin = EchoPlugin::new();

        assert!(plugin.can_handle("/echo hello"));
        assert!(!plugin.can_handle("/time"));

        let context = PluginContext {
            user_input: "/echo hello world".to_string(),
            session_id: "test".to_string(),
            config: HashMap::new(),
            environment: HashMap::new(),
            working_directory: PathBuf::from("/tmp"),
        };

        let result = plugin.execute(context).await.unwrap();
        assert!(result.success);
        assert_eq!(result.output, "Echo: hello world");
    }

    #[tokio::test]
    async fn test_plugin_manager() {
        let mut manager = PluginManager::with_builtin_plugins().await.unwrap();

        assert_eq!(manager.plugins.len(), 2);
        assert!(manager.enabled_plugins.contains(&"echo".to_string()));

        let commands = manager.get_available_commands();
        assert!(commands.contains(&"/echo".to_string()));
    }

    #[test]
    fn test_plugin_installer() {
        // Test the static extract_repo_name method
        assert_eq!(
            PluginInstaller::extract_repo_name("https://github.com/user/repo.git").unwrap(),
            "repo"
        );
        assert_eq!(
            PluginInstaller::extract_repo_name("https://github.com/user/repo").unwrap(),
            "repo"
        );

        // Test edge cases
        assert_eq!(
            PluginInstaller::extract_repo_name("https://github.com/org/my-plugin").unwrap(),
            "my-plugin"
        );
        assert_eq!(
            PluginInstaller::extract_repo_name("git@github.com:user/repo.git").unwrap(),
            "repo"
        );
        assert_eq!(
            PluginInstaller::extract_repo_name("https://github.com/user/repo/").unwrap(),
            "repo"
        );

        // Test error case - trailing slash with .git results in empty name
        assert!(PluginInstaller::extract_repo_name("https://github.com/user/.git").is_err());
    }
}
