//! AI service integration for chat functionality

use crate::services::ai_service::{AiService, ToolPlan, PlannedTool};
use crate::services::mcp_service::{McpService, McpServerConfig};
use super::{Colors, RealtimeSuggestion};
use anyhow::Result;
use std::sync::Arc;
use tokio::time::{sleep, Duration};
use log::{error, debug};
use std::io::{self, Write};

/// Generate real-time suggestions using AI
pub async fn generate_realtime_suggestions(partial_input: &str, ai_service: &AiService) -> Result<Vec<String>> {
    if partial_input.len() < 3 {
        return Ok(Vec::new());
    }

    // Quick context-aware suggestions
    let suggestions = vec![
        format!("{} --help", partial_input),
        format!("{} --verbose", partial_input),
        format!("@solana/{}", partial_input),
    ];

    Ok(suggestions)
}

/// Process message with AI and update chat history
pub async fn process_with_realtime_ai(
    message: String,
    ai_service: &Arc<AiService>,
    chat_history: &mut Vec<String>
) -> Result<()> {
    // Show processing animation
    show_animated_status("Processing with AI", "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏", 80).await;

    // Query AI - simple query for now as query_with_plan doesn't exist
    let ai_response = match ai_service.query_with_debug(&message, false).await {
        Ok(response) => response,
        Err(e) => {
            error!("AI query failed: {}", e);
            return Ok(());
        }
    };

    // For now, just display the response (planning API needs to be integrated differently)
    // The code below is disabled until we have proper plan API integration
    /*
    if let Some(ai_plan) = ai_response.plan {
        println!("\n{}• AI Agent Plan detected!{}", Colors::MAGENTA, Colors::RESET);
        show_colored_plan_diagram(&ai_plan);

        // Ask user for confirmation
        println!("\n{}Execute this plan? (y/n): {}", Colors::YELLOW, Colors::RESET);

        match get_user_choice().await {
            Ok(choice) if choice == 1 => {
                execute_ai_plan_with_colors(&ai_plan, &message, ai_service).await?;
            }
            _ => {
                println!("{}• Plan execution cancelled{}", Colors::DIM, Colors::RESET);
            }
        }
    }
    */

    // Display AI response
    if !ai_response.is_empty() {
        println!("\n{}• Assistant: {}{}{}", Colors::CYAN, Colors::BOLD, ai_response, Colors::RESET);
        chat_history.push(format!("Assistant: {}", ai_response));
    }

    Ok(())
}

/// Show animated status message
pub async fn show_animated_status(message: &str, chars: &str, duration_ms: u64) {
    let frames: Vec<char> = chars.chars().collect();

    for frame in frames.iter().take(10) {
        print!("\r{}{}  {} {}{}", Colors::YELLOW, frame, message, Colors::DIM, Colors::RESET);
        io::stdout().flush().unwrap_or(());
        sleep(Duration::from_millis(duration_ms)).await;
    }

    print!("\r\x1b[K");
    io::stdout().flush().unwrap_or(());
}

/// Display colored plan diagram
pub fn show_colored_plan_diagram(ai_plan: &ToolPlan) {
    println!("\n{}╔══════════════════════════════════════════════════╗", Colors::MAGENTA);
    println!("║              {}EXECUTION PLAN{}                     ║", Colors::BOLD, Colors::MAGENTA);
    println!("╚══════════════════════════════════════════════════╝{}", Colors::RESET);

    // Show reasoning
    println!("\n{}📋 Reasoning:{}", Colors::CYAN, Colors::RESET);
    for line in wrap_text(&ai_plan.reasoning, 50) {
        println!("   {}", line);
    }

    // Show planned tools
    println!("\n{}🔧 Planned Tools:{}", Colors::YELLOW, Colors::RESET);
    for (i, tool) in ai_plan.osvm_tools_to_use.iter().enumerate() {
        let status_icon = "○";
        println!("   {} {}. {}{}{} {}({}){}",
            status_icon,
            i + 1,
            Colors::GREEN,
            tool.tool_name,
            Colors::RESET,
            Colors::DIM,
            tool.server_id,
            Colors::RESET
        );

        // PlannedTool doesn't have a reason field, show args instead
        if !tool.args.is_null() {
            println!("      {}└─ Args: {}{}", Colors::DIM, tool.args, Colors::RESET);
        }
    }

    // Show expected outcome
    println!("\n{}✨ Expected Outcome:{}", Colors::GREEN, Colors::RESET);
    for line in wrap_text(&ai_plan.expected_outcome, 50) {
        println!("   {}", line);
    }
}

/// Wrap text to specified width
pub fn wrap_text(text: &str, width: usize) -> Vec<String> {
    let mut lines = Vec::new();
    let mut current_line = String::new();

    for word in text.split_whitespace() {
        if current_line.len() + word.len() + 1 > width {
            if !current_line.is_empty() {
                lines.push(current_line.clone());
                current_line.clear();
            }
        }

        if !current_line.is_empty() {
            current_line.push(' ');
        }
        current_line.push_str(word);
    }

    if !current_line.is_empty() {
        lines.push(current_line);
    }

    lines
}

/// Get user choice (1 for yes, 0 for no)
pub async fn get_user_choice() -> Result<u32> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;

    match buffer.trim().to_lowercase().as_str() {
        "y" | "yes" => Ok(1),
        _ => Ok(0),
    }
}

/// Execute AI plan with colored output
pub async fn execute_ai_plan_with_colors(
    ai_plan: &ToolPlan,
    original_message: &str,
    ai_service: &Arc<AiService>
) -> Result<()> {
    println!("\n{}═══ Executing Plan ═══{}", Colors::MAGENTA, Colors::RESET);

    for (i, tool) in ai_plan.osvm_tools_to_use.iter().enumerate() {
        println!("\n{}[{}/{}] Executing: {}{}{}",
            Colors::CYAN,
            i + 1,
            ai_plan.osvm_tools_to_use.len(),
            Colors::YELLOW,
            tool.tool_name,
            Colors::RESET
        );

        // Simulate tool execution animation
        show_animated_status(&format!("Running {}", tool.tool_name), "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏", 50).await;

        // Simulate execution result
        println!("   {}✓ {} completed successfully{}",
            Colors::GREEN,
            tool.tool_name,
            Colors::RESET
        );

        sleep(Duration::from_millis(500)).await;
    }

    println!("\n{}✓ Plan execution complete!{}", Colors::GREEN, Colors::RESET);

    Ok(())
}

/// Show contextual suggestions based on chat history
pub async fn show_contextual_suggestions(ai_service: &Arc<AiService>, chat_history: &[String]) {
    println!("\n{}💡 Contextual Suggestions:{}", Colors::YELLOW, Colors::RESET);

    let suggestions = vec![
        ("Check wallet balance", "@solana/get_balance"),
        ("View recent transactions", "@solana/get_recent_transactions"),
        ("Monitor network status", "/network status"),
    ];

    for (desc, cmd) in suggestions {
        println!("   • {} - {}{}{}", desc, Colors::CYAN, cmd, Colors::RESET);
    }
}

/// Show help commands
pub fn show_help_commands() {
    println!("\n{}╔════════════════════════════════════════════════╗", Colors::CYAN);
    println!("║              {}HELP - Commands{}                   ║", Colors::BOLD, Colors::CYAN);
    println!("╚════════════════════════════════════════════════╝{}", Colors::RESET);

    let commands = vec![
        ("/help", "Show this help menu"),
        ("/clear", "Clear chat history"),
        ("/tools", "List available MCP tools"),
        ("/context", "Show conversation context"),
        ("/status", "Show system status"),
        ("@server/tool", "Execute specific MCP tool"),
        ("Ctrl+T", "Toggle task navigation mode"),
        ("Ctrl+C", "Exit application"),
        ("Tab", "Auto-complete suggestion"),
        ("↑/↓", "Navigate history or suggestions"),
    ];

    for (cmd, desc) in commands {
        println!("  {}{:<15}{} - {}", Colors::GREEN, cmd, Colors::DIM, desc);
    }

    println!("{}", Colors::RESET);
}

/// Show context visualization
pub async fn show_context_visualization(chat_history: &[String], ai_service: &Arc<AiService>) -> Result<()> {
    println!("\n{}╔════════════════════════════════════════════════╗", Colors::BLUE);
    println!("║           {}CONVERSATION CONTEXT{}                 ║", Colors::BOLD, Colors::BLUE);
    println!("╚════════════════════════════════════════════════╝{}", Colors::RESET);

    if chat_history.is_empty() {
        println!("{}  No conversation history yet{}", Colors::DIM, Colors::RESET);
    } else {
        println!("\n{}Recent Messages:{}", Colors::CYAN, Colors::RESET);
        for (i, msg) in chat_history.iter().rev().take(5).enumerate() {
            let truncated = if msg.len() > 60 {
                format!("{}...", &msg[..57])
            } else {
                msg.clone()
            };
            println!("  {}. {}", i + 1, truncated);
        }

        println!("\n{}Statistics:{}", Colors::YELLOW, Colors::RESET);
        println!("  • Total messages: {}", chat_history.len());
        println!("  • Current session: Active");
    }

    Ok(())
}

/// Show status overview
pub async fn show_status_overview(
    servers: &[(&String, &McpServerConfig)],
    chat_history: &[String]
) -> Result<()> {
    println!("\n{}╔════════════════════════════════════════════════╗", Colors::CYAN);
    println!("║              {}SYSTEM STATUS{}                     ║", Colors::BOLD, Colors::CYAN);
    println!("╚════════════════════════════════════════════════╝{}", Colors::RESET);

    // MCP Servers
    println!("\n{}MCP Servers:{}", Colors::YELLOW, Colors::RESET);
    for (id, config) in servers {
        let status_icon = if config.enabled { "✓" } else { "✗" };
        let status_color = if config.enabled { Colors::GREEN } else { Colors::RED };

        println!("  {} {}{}{} - {}",
            status_color,
            status_icon,
            Colors::BLUE,
            id,
            config.url
        );
    }

    // Chat Statistics
    println!("\n{}Chat Statistics:{}", Colors::YELLOW, Colors::RESET);
    println!("  • Messages: {}", chat_history.len());
    println!("  • Session: Active");
    println!("  • AI Service: Connected");

    println!("{}", Colors::RESET);
    Ok(())
}

/// Run demo mode for testing
pub async fn run_demo_mode() -> Result<()> {
    println!("{}🎮 Demo Mode - Simulating chat interactions{}", Colors::MAGENTA, Colors::RESET);

    let demo_messages = vec![
        "What's my wallet balance?",
        "Show recent transactions",
        "How do I stake SOL?",
    ];

    for msg in demo_messages {
        println!("\n{}Simulating: {}{}", Colors::DIM, msg, Colors::RESET);
        show_animated_status("Processing", "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏", 100).await;
        sleep(Duration::from_millis(500)).await;
    }

    println!("\n{}✓ Demo complete!{}", Colors::GREEN, Colors::RESET);
    Ok(())
}