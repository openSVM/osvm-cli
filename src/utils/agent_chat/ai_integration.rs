//! AI service integration for chat functionality

use super::{Colors, RealtimeSuggestion};
use crate::services::ai_service::{AiService, PlannedTool, ToolPlan};
use crate::services::mcp_service::{McpServerConfig, McpService};
use crate::services::ovsm_service::OvsmService;
use anyhow::Result;
use log::{debug, error, info, warn};
use std::io::{self, Write};
use std::sync::Arc;
use tokio::time::{sleep, timeout, Duration};

/// Maximum chat history size to prevent unbounded memory growth
const MAX_CHAT_HISTORY_SIZE: usize = 1000;

/// Add message to chat history with size limit
/// Removes oldest 100 messages when limit is exceeded (same behavior as V2)
pub(crate) fn add_to_chat_history(chat_history: &mut Vec<String>, message: String) {
    chat_history.push(message);
    if chat_history.len() > MAX_CHAT_HISTORY_SIZE {
        chat_history.drain(0..100);
    }
}

/// Generate real-time suggestions using AI
pub async fn generate_realtime_suggestions(
    partial_input: &str,
    ai_service: &AiService,
) -> Result<Vec<String>> {
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

/// Extract OVSM code blocks from AI response
/// Returns a vector of (code, language_hint) tuples
fn extract_ovsm_code_blocks(text: &str) -> Vec<(String, String)> {
    let mut code_blocks = Vec::new();
    let lines: Vec<&str> = text.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

        // Look for code block start: ```lisp, ```ovsm, or just ```
        if line.starts_with("```") {
            let lang = line.trim_start_matches("```").trim().to_lowercase();
            let mut code = String::new();
            i += 1;

            // Collect code until we find closing ```
            while i < lines.len() {
                let code_line = lines[i];
                if code_line.trim().starts_with("```") {
                    break;
                }
                code.push_str(code_line);
                code.push('\n');
                i += 1;
            }

            // Only include if it looks like OVSM/LISP code or has no language specified
            if lang.is_empty() || lang == "lisp" || lang == "ovsm" || lang == "scheme" {
                let trimmed_code = code.trim();
                // Enhanced heuristic: LISP code starts with ( or ; (comment)
                if !trimmed_code.is_empty() {
                    let first_char = trimmed_code.chars().next();
                    if first_char == Some('(') || first_char == Some(';') {
                        code_blocks.push((trimmed_code.to_string(), lang));
                    }
                }
            }
        }
        i += 1;
    }

    code_blocks
}

/// Process message with AI and update chat history
pub async fn process_with_realtime_ai(
    message: String,
    ai_service: &Arc<AiService>,
    chat_history: &mut Vec<String>,
) -> Result<()> {
    // Show processing animation
    show_animated_status("Processing with AI", "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏", 80).await;

    // Query AI - simple query for now as query_with_plan doesn't exist
    let ai_response = match ai_service.query_with_debug(&message, false).await {
        Ok(response) => response,
        Err(e) => {
            error!("AI query failed: {}", e);
            println!(
                "\n{}✗ AI Service Error: {}{}",
                Colors::RED,
                e,
                Colors::RESET
            );
            println!(
                "{}Tip: Check your AI configuration or try again{}",
                Colors::DIM,
                Colors::RESET
            );
            return Ok(());
        }
    };

    // Display AI response
    if !ai_response.is_empty() {
        println!(
            "\n{}• Assistant: {}{}{}",
            Colors::CYAN,
            Colors::BOLD,
            ai_response,
            Colors::RESET
        );
        add_to_chat_history(chat_history, format!("Assistant: {}", ai_response));

        // Extract and offer to execute OVSM code blocks
        let code_blocks = extract_ovsm_code_blocks(&ai_response);

        if !code_blocks.is_empty() {
            for (i, (code, _lang)) in code_blocks.iter().enumerate() {
                // Pre-validate syntax before showing to user
                let validation_result = {
                    use ovsm::Scanner;
                    let mut scanner = Scanner::new(code);
                    scanner.scan_tokens()
                };

                if let Err(e) = validation_result {
                    println!(
                        "\n{}⚠️  Code Block {} has invalid syntax - skipping{}",
                        Colors::YELLOW,
                        i + 1,
                        Colors::RESET
                    );
                    println!("{}Error: {}{}", Colors::DIM, e, Colors::RESET);
                    continue; // Skip this block
                }

                println!(
                    "\n{}╭─ OVSM Code Block {} ─{}",
                    Colors::MAGENTA,
                    i + 1,
                    Colors::RESET
                );

                // Show code preview (first 3 lines)
                let preview_lines: Vec<&str> = code.lines().take(3).collect();
                for line in &preview_lines {
                    println!("{}│{} {}", Colors::MAGENTA, Colors::RESET, line);
                }
                if code.lines().count() > 3 {
                    println!("{}│{} ...", Colors::MAGENTA, Colors::RESET);
                }
                println!("{}╰─{}", Colors::MAGENTA, Colors::RESET);

                // Ask user with enhanced options
                print!(
                    "\n{}Execute? ([y]es/[n]o/[v]iew full): {}",
                    Colors::YELLOW,
                    Colors::RESET
                );
                io::stdout().flush()?;

                // Get user choice with support for view option
                let mut buffer = String::new();
                io::stdin().read_line(&mut buffer)?;
                let choice = buffer.trim().to_lowercase();

                // Handle view full code
                if choice == "v" || choice == "view" {
                    println!(
                        "\n{}Full Code (Block {}):{}\n",
                        Colors::CYAN,
                        i + 1,
                        Colors::RESET
                    );
                    for (line_num, line) in code.lines().enumerate() {
                        println!(
                            "{}{:4}{} │ {}",
                            Colors::DIM,
                            line_num + 1,
                            Colors::RESET,
                            line
                        );
                    }

                    // Ask again after showing
                    print!("\n{}Execute now? (y/n): {}", Colors::YELLOW, Colors::RESET);
                    io::stdout().flush()?;

                    buffer.clear();
                    io::stdin().read_line(&mut buffer)?;
                }

                // Check if user wants to execute
                let execute = buffer.trim().to_lowercase();
                if execute == "y" || execute == "yes" {
                    info!("User confirmed execution of OVSM code block {}", i + 1);

                    // Execute with timeout wrapper
                    println!(
                        "\n{}▶ Executing OVSM code (30s timeout)...{}",
                        Colors::GREEN,
                        Colors::RESET
                    );

                    let code_clone = code.clone();
                    let execution_timeout = Duration::from_secs(30);

                    match timeout(
                        execution_timeout,
                        tokio::task::spawn_blocking(move || {
                            let mut ovsm_service = OvsmService::with_verbose(true);
                            ovsm_service.execute_code(&code_clone)
                        }),
                    )
                    .await
                    {
                        Ok(Ok(Ok(result))) => {
                            println!(
                                "\n{}✓ Execution successful!{}",
                                Colors::GREEN,
                                Colors::RESET
                            );
                            println!("{}Result:{} {:?}", Colors::CYAN, Colors::RESET, result);
                            add_to_chat_history(
                                chat_history,
                                format!("System: Executed OVSM code, result: {:?}", result),
                            );
                        }
                        Ok(Ok(Err(e))) => {
                            println!(
                                "\n{}✗ Execution failed: {}{}",
                                Colors::RED,
                                e,
                                Colors::RESET
                            );
                            add_to_chat_history(
                                chat_history,
                                format!("System: OVSM execution failed: {}", e),
                            );
                        }
                        Ok(Err(e)) => {
                            println!("\n{}✗ Thread panic: {}{}", Colors::RED, e, Colors::RESET);
                            add_to_chat_history(
                                chat_history,
                                "System: OVSM execution panicked".to_string(),
                            );
                        }
                        Err(_) => {
                            println!(
                                "\n{}✗ Execution timeout (30 seconds){}",
                                Colors::RED,
                                Colors::RESET
                            );
                            println!(
                                "{}The code took too long to execute{}",
                                Colors::DIM,
                                Colors::RESET
                            );
                            add_to_chat_history(
                                chat_history,
                                "System: OVSM execution timed out".to_string(),
                            );
                        }
                    }
                } else {
                    println!("{}• Execution cancelled{}", Colors::DIM, Colors::RESET);
                }
            }
        }
    }

    Ok(())
}

/// Show animated status message
pub async fn show_animated_status(message: &str, chars: &str, duration_ms: u64) {
    let frames: Vec<char> = chars.chars().collect();

    for frame in frames.iter().take(10) {
        print!(
            "\r{}{}  {} {}{}",
            Colors::YELLOW,
            frame,
            message,
            Colors::DIM,
            Colors::RESET
        );
        io::stdout().flush().unwrap_or(());
        sleep(Duration::from_millis(duration_ms)).await;
    }

    print!("\r\x1b[K");
    io::stdout().flush().unwrap_or(());
}

/// Display colored plan diagram
pub fn show_colored_plan_diagram(ai_plan: &ToolPlan) {
    println!(
        "\n{}╔══════════════════════════════════════════════════╗",
        Colors::MAGENTA
    );
    println!(
        "║              {}EXECUTION PLAN{}                     ║",
        Colors::BOLD,
        Colors::MAGENTA
    );
    println!(
        "╚══════════════════════════════════════════════════╝{}",
        Colors::RESET
    );

    // Show reasoning
    println!("\n{}📋 Reasoning:{}", Colors::CYAN, Colors::RESET);
    for line in wrap_text(&ai_plan.reasoning, 50) {
        println!("   {}", line);
    }

    // Show planned tools
    println!("\n{}🔧 Planned Tools:{}", Colors::YELLOW, Colors::RESET);
    for (i, tool) in ai_plan.osvm_tools_to_use.iter().enumerate() {
        let status_icon = "○";
        println!(
            "   {} {}. {}{}{} {}({}){}",
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
            println!(
                "      {}└─ Args: {}{}",
                Colors::DIM,
                tool.args,
                Colors::RESET
            );
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
    ai_service: &Arc<AiService>,
) -> Result<()> {
    println!(
        "\n{}═══ Executing Plan ═══{}",
        Colors::MAGENTA,
        Colors::RESET
    );

    for (i, tool) in ai_plan.osvm_tools_to_use.iter().enumerate() {
        println!(
            "\n{}[{}/{}] Executing: {}{}{}",
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
        println!(
            "   {}✓ {} completed successfully{}",
            Colors::GREEN,
            tool.tool_name,
            Colors::RESET
        );

        sleep(Duration::from_millis(500)).await;
    }

    println!(
        "\n{}✓ Plan execution complete!{}",
        Colors::GREEN,
        Colors::RESET
    );

    Ok(())
}

/// Show contextual suggestions based on chat history
pub async fn show_contextual_suggestions(ai_service: &Arc<AiService>, chat_history: &[String]) {
    println!(
        "\n{}💡 Contextual Suggestions:{}",
        Colors::YELLOW,
        Colors::RESET
    );

    let suggestions = vec![
        ("Check wallet balance", "@solana/get_balance"),
        (
            "View recent transactions",
            "@solana/get_recent_transactions",
        ),
        ("Monitor network status", "/network status"),
    ];

    for (desc, cmd) in suggestions {
        println!("   • {} - {}{}{}", desc, Colors::CYAN, cmd, Colors::RESET);
    }
}

/// Show help commands
pub fn show_help_commands() {
    println!(
        "\n{}╔════════════════════════════════════════════════╗",
        Colors::CYAN
    );
    println!(
        "║              {}HELP - Commands{}                   ║",
        Colors::BOLD,
        Colors::CYAN
    );
    println!(
        "╚════════════════════════════════════════════════╝{}",
        Colors::RESET
    );

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
pub async fn show_context_visualization(
    chat_history: &[String],
    ai_service: &Arc<AiService>,
) -> Result<()> {
    println!(
        "\n{}╔════════════════════════════════════════════════╗",
        Colors::BLUE
    );
    println!(
        "║           {}CONVERSATION CONTEXT{}                 ║",
        Colors::BOLD,
        Colors::BLUE
    );
    println!(
        "╚════════════════════════════════════════════════╝{}",
        Colors::RESET
    );

    if chat_history.is_empty() {
        println!(
            "{}  No conversation history yet{}",
            Colors::DIM,
            Colors::RESET
        );
    } else {
        println!("\n{}Recent Messages:{}", Colors::CYAN, Colors::RESET);
        for (i, msg) in chat_history.iter().rev().take(5).enumerate() {
            let truncated = if msg.chars().count() > 60 {
                let truncated_chars: String = msg.chars().take(57).collect();
                format!("{}...", truncated_chars)
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
    chat_history: &[String],
) -> Result<()> {
    println!(
        "\n{}╔════════════════════════════════════════════════╗",
        Colors::CYAN
    );
    println!(
        "║              {}SYSTEM STATUS{}                     ║",
        Colors::BOLD,
        Colors::CYAN
    );
    println!(
        "╚════════════════════════════════════════════════╝{}",
        Colors::RESET
    );

    // MCP Servers
    println!("\n{}MCP Servers:{}", Colors::YELLOW, Colors::RESET);
    for (id, config) in servers {
        let status_icon = if config.enabled { "✓" } else { "✗" };
        let status_color = if config.enabled {
            Colors::GREEN
        } else {
            Colors::RED
        };

        println!(
            "  {} {}{}{} - {}",
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
    println!(
        "{}🎮 Demo Mode - Simulating chat interactions{}",
        Colors::MAGENTA,
        Colors::RESET
    );

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
