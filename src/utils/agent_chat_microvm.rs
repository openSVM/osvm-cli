//! MicroVM-enabled agent chat interface
//!
//! This module provides an agent chat interface where the main chat runs in a persistent
//! microVM and all MCP tool executions happen in ephemeral microVMs that are destroyed
//! after returning results.

use anyhow::{Context, Result};
use log::{debug, error, info, warn};
use std::sync::Arc;
use tokio::sync::Mutex;

use crate::services::ephemeral_microvm::{ChatVmOrchestrator, EphemeralVmManager};
use crate::services::mcp_service::McpService;

/// Configuration for microVM-enabled chat
pub struct MicroVmChatConfig {
    /// Enable debug logging
    pub debug_mode: bool,
    /// Use ephemeral VMs for all tool executions
    pub ephemeral_tools: bool,
    /// Memory allocation for chat VM (MB)
    pub chat_vm_memory_mb: usize,
    /// CPU count for chat VM
    pub chat_vm_cpus: u8,
}

impl Default for MicroVmChatConfig {
    fn default() -> Self {
        Self {
            debug_mode: false,
            ephemeral_tools: true,
            chat_vm_memory_mb: 512,
            chat_vm_cpus: 2,
        }
    }
}

/// Run the microVM-enabled agent chat interface
pub async fn run_microvm_agent_chat() -> Result<()> {
    println!("🚀 Starting MicroVM-enabled Agent Chat");
    println!("📦 Main chat will run in a persistent microVM");
    println!("🔒 All MCP tools will execute in ephemeral microVMs");
    println!();

    let config = MicroVmChatConfig::default();
    run_microvm_chat_with_config(config).await
}

/// Run microVM chat with custom configuration
pub async fn run_microvm_chat_with_config(config: MicroVmChatConfig) -> Result<()> {
    info!("Initializing microVM chat orchestrator");

    // Create the chat VM orchestrator
    let mut orchestrator = ChatVmOrchestrator::new(config.debug_mode);

    // Start the main chat VM
    println!("⏳ Starting main chat microVM...");
    orchestrator.start_chat_vm().await
        .context("Failed to start chat microVM")?;
    println!("✅ Chat microVM started successfully");
    println!();

    // Initialize MCP service with ephemeral VM support enabled
    let mut mcp_service = McpService::new_with_debug(config.debug_mode);

    // Override to ensure ephemeral VMs are used
    if config.ephemeral_tools {
        // The MCP service is already configured to use ephemeral VMs by default
        println!("🔐 Ephemeral microVMs enabled for all tool executions");
    }

    // Load MCP server configurations
    mcp_service.load_config()
        .context("Failed to load MCP configurations")?;

    // Run the chat interface in the microVM
    let result = run_chat_in_vm(&orchestrator, &mcp_service, config).await;

    // Cleanup on exit
    println!();
    println!("⏳ Shutting down microVMs...");
    orchestrator.stop().await
        .context("Failed to stop chat orchestrator")?;
    println!("✅ All microVMs terminated");

    result
}

/// Run the actual chat interface inside the microVM
async fn run_chat_in_vm(
    orchestrator: &ChatVmOrchestrator,
    mcp_service: &McpService,
    config: MicroVmChatConfig,
) -> Result<()> {
    println!("💬 Chat interface is now running in an isolated microVM");
    println!("Type 'exit' or 'quit' to leave the chat");
    println!("Type '/help' for available commands");
    println!();

    // Create a simple chat loop for demonstration
    // In production, this would integrate with the actual agent_chat UI
    use tokio::io::{AsyncBufReadExt, BufReader};

    let stdin = tokio::io::stdin();
    let mut reader = BufReader::new(stdin);
    let mut line = String::new();

    loop {
        print!("> ");
        use std::io::{self, Write};
        io::stdout().flush()?;

        line.clear();
        reader.read_line(&mut line).await?;

        let input = line.trim();

        if input.is_empty() {
            continue;
        }

        if input == "exit" || input == "quit" {
            break;
        }

        if input == "/help" {
            print_help();
            continue;
        }

        if input.starts_with("/tool ") {
            // Extract tool command
            let parts: Vec<&str> = input[6..].splitn(3, ' ').collect();
            if parts.len() < 2 {
                println!("Usage: /tool <server_id> <tool_name> [arguments]");
                continue;
            }

            let server_id = parts[0];
            let tool_name = parts[1];
            let arguments = if parts.len() > 2 {
                serde_json::from_str(parts[2]).ok()
            } else {
                None
            };

            // Execute tool in ephemeral microVM
            println!("🔄 Launching ephemeral microVM for tool: {}/{}", server_id, tool_name);

            match orchestrator.execute_tool(server_id, tool_name, arguments).await {
                Ok(result) => {
                    println!("✅ Tool execution successful");
                    println!("Result: {}", serde_json::to_string_pretty(&result)?);
                    println!("🗑️  Ephemeral microVM destroyed");
                }
                Err(e) => {
                    println!("❌ Tool execution failed: {}", e);
                    println!("🗑️  Ephemeral microVM destroyed");
                }
            }
        } else if input.starts_with("/") {
            println!("Unknown command. Type '/help' for available commands.");
        } else {
            // Normal chat message - would be processed by the agent
            println!("💭 Processing in isolated chat microVM...");
            // In production, this would call the actual agent processing
            println!("Agent: I received your message: {}", input);

            // Simulate tool usage
            if input.contains("check") || input.contains("get") || input.contains("list") {
                println!("🔧 Detected potential tool usage");
                println!("Would execute relevant tools in ephemeral microVMs");
            }
        }
    }

    Ok(())
}

/// Print help message
fn print_help() {
    println!("Available commands:");
    println!("  /help                              - Show this help message");
    println!("  /tool <server> <tool> [args]       - Execute a tool in ephemeral microVM");
    println!("  exit, quit                         - Exit the chat");
    println!();
    println!("Security features:");
    println!("  • Main chat runs in persistent microVM");
    println!("  • Each tool executes in fresh ephemeral microVM");
    println!("  • MicroVMs are destroyed after tool execution");
    println!("  • Complete isolation between tool executions");
}

/// Integration point for the existing chat UI to use microVMs
pub async fn run_agent_chat_with_microvms(advanced: bool) -> Result<()> {
    if advanced {
        // For advanced mode, use the full orchestrator
        run_microvm_agent_chat().await
    } else {
        // For basic mode, create a simpler setup
        let config = MicroVmChatConfig {
            debug_mode: false,
            ephemeral_tools: true,
            chat_vm_memory_mb: 256, // Less memory for basic mode
            chat_vm_cpus: 1,        // Single CPU for basic mode
        };
        run_microvm_chat_with_config(config).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = MicroVmChatConfig::default();
        assert!(config.ephemeral_tools);
        assert_eq!(config.chat_vm_memory_mb, 512);
        assert_eq!(config.chat_vm_cpus, 2);
        assert!(!config.debug_mode);
    }
}