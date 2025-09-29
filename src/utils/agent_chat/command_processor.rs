//! Command processing logic for handling user input

use super::Colors;
use crate::services::mcp_service::{McpServerConfig, McpService};
use anyhow::{anyhow, Result};
use std::collections::HashMap;

// Command handler functions
fn help_command(_args: &[&str]) -> Result<CommandResult> {
    Ok(CommandResult::Success(get_help_text()))
}

fn clear_command(_args: &[&str]) -> Result<CommandResult> {
    Ok(CommandResult::Clear)
}

fn exit_command(_args: &[&str]) -> Result<CommandResult> {
    Ok(CommandResult::Exit)
}

fn get_help_text() -> String {
    format!(
        r#"
{}╔════════════════════════════════════════════════╗
║              {}AVAILABLE COMMANDS{}                ║
╚════════════════════════════════════════════════╝{}

{}Commands:{}
  /help         - Show this help menu
  /clear        - Clear chat history
  /tools        - List available MCP tools
  /context      - Show conversation context
  /status       - Show system status
  /exit, /quit  - Exit application

{}Tool Invocation:{}
  @server/tool  - Execute specific MCP tool

{}Keyboard Shortcuts:{}
  Ctrl+T        - Toggle task navigation mode
  Ctrl+C        - Exit application
  Tab           - Auto-complete suggestion
  ↑/↓           - Navigate history or suggestions
  Escape        - Clear current input

{}Examples:{}
  @solana/get_balance
  @solana/get_recent_transactions
  /tools
  /status
"#,
        Colors::CYAN,
        Colors::BOLD,
        Colors::CYAN,
        Colors::RESET,
        Colors::GREEN,
        Colors::RESET,
        Colors::YELLOW,
        Colors::RESET,
        Colors::MAGENTA,
        Colors::RESET,
        Colors::BLUE,
        Colors::RESET,
    )
}

/// Command processor for handling various input types
pub struct CommandProcessor {
    commands: HashMap<String, CommandHandler>,
}

/// Handler function type for commands - simplified to avoid lifetime issues
type CommandHandler = fn(&[&str]) -> Result<CommandResult>;

/// Result of command execution
pub enum CommandResult {
    Success(String),
    Error(String),
    Exit,
    Clear,
    Continue,
}

impl CommandProcessor {
    /// Create new command processor with default handlers
    pub fn new() -> Self {
        let mut commands = HashMap::new();

        // Register built-in commands
        commands.insert("/help".to_string(), help_command as CommandHandler);
        commands.insert("/clear".to_string(), clear_command as CommandHandler);
        commands.insert("/exit".to_string(), exit_command as CommandHandler);
        commands.insert("/quit".to_string(), exit_command as CommandHandler);

        Self { commands }
    }

    /// Process input and determine action
    pub fn process(&self, input: &str) -> Result<CommandResult> {
        let input = input.trim();

        // Check for empty input
        if input.is_empty() {
            return Ok(CommandResult::Continue);
        }

        // Parse command and arguments
        let parts: Vec<&str> = input.split_whitespace().collect();
        let command = parts.get(0).unwrap_or(&"");

        // Check for registered commands
        if let Some(handler) = self.commands.get(*command) {
            return handler(&parts[1..]);
        }

        // Check for special command patterns
        if command.starts_with('/') {
            return Ok(CommandResult::Error(format!(
                "Unknown command: {}",
                command
            )));
        }

        if command.starts_with('@') {
            return self.process_tool_invocation(input);
        }

        // Default: process as regular message
        Ok(CommandResult::Continue)
    }

    /// Process MCP tool invocation
    fn process_tool_invocation(&self, input: &str) -> Result<CommandResult> {
        // Parse @server/tool syntax with bounds checking
        let parts: Vec<&str> = input[1..].split('/').collect();

        if parts.len() != 2 {
            return Ok(CommandResult::Error(
                "Invalid tool syntax. Use @server/tool".to_string(),
            ));
        }

        // Safe array access with bounds checking
        let server = parts.get(0).ok_or_else(|| anyhow!("Missing server name"))?;
        let tool = parts.get(1).ok_or_else(|| anyhow!("Missing tool name"))?;

        if server.is_empty() || tool.is_empty() {
            return Ok(CommandResult::Error(
                "Server and tool names cannot be empty".to_string(),
            ));
        }

        Ok(CommandResult::Success(format!(
            "{}Executing tool: {}/{}{}",
            Colors::YELLOW,
            server,
            tool,
            Colors::RESET
        )))
    }

    /// Register custom command handler
    pub fn register_command(&mut self, command: &str, handler: CommandHandler) {
        self.commands.insert(command.to_string(), handler);
    }

    /// Check if input is a command
    pub fn is_command(input: &str) -> bool {
        input.trim().starts_with('/') || input.trim().starts_with('@')
    }

    /// Parse tool invocation with safe array access
    pub fn parse_tool_invocation(input: &str) -> Option<(String, String, Vec<String>)> {
        if !input.starts_with('@') {
            return None;
        }

        let parts: Vec<&str> = input[1..].split_whitespace().collect();
        if parts.is_empty() {
            return None;
        }

        let server_tool: Vec<&str> = parts.get(0)?.split('/').collect();
        if server_tool.len() != 2 {
            return None;
        }

        // Safe array access with bounds checking
        let server = server_tool.get(0)?.to_string();
        let tool = server_tool.get(1)?.to_string();

        if server.is_empty() || tool.is_empty() {
            return None;
        }

        let args: Vec<String> = parts
            .get(1..)
            .unwrap_or(&[])
            .iter()
            .map(|s| s.to_string())
            .collect();

        Some((server, tool, args))
    }
}

/// Command context for advanced processing
pub struct CommandContext {
    pub chat_history: Vec<String>,
    pub mcp_servers: Vec<(String, bool)>, // (server_id, enabled)
    pub current_task: Option<String>,
}

impl CommandContext {
    pub fn new() -> Self {
        Self {
            chat_history: Vec::new(),
            mcp_servers: Vec::new(),
            current_task: None,
        }
    }

    /// Add message to history
    pub fn add_to_history(&mut self, message: String) {
        self.chat_history.push(message);

        // Limit history size
        if self.chat_history.len() > 1000 {
            self.chat_history.remove(0);
        }
    }

    /// Get recent history
    pub fn get_recent_history(&self, count: usize) -> Vec<String> {
        self.chat_history
            .iter()
            .rev()
            .take(count)
            .rev()
            .cloned()
            .collect()
    }

    /// Update MCP servers list
    pub fn update_servers(&mut self, servers: Vec<(String, bool)>) {
        self.mcp_servers = servers;
    }

    /// Get active servers
    pub fn get_active_servers(&self) -> Vec<String> {
        self.mcp_servers
            .iter()
            .filter(|(_, enabled)| *enabled)
            .map(|(id, _)| id.clone())
            .collect()
    }
}
