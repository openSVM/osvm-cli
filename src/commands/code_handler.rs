//! Handler for the `osvm code` command - AI-powered coding assistant

use anyhow::Result;
use clap::ArgMatches;
use std::path::PathBuf;

/// Handle the code command
pub async fn handle_code_command(matches: &ArgMatches) -> Result<()> {
    // Get command arguments
    let directory = matches
        .get_one::<String>("directory")
        .map(PathBuf::from)
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

    let initial_prompt = matches.get_one::<String>("prompt").cloned();
    let _model = matches.get_one::<String>("model").cloned();
    let yolo_mode = matches.get_flag("yolo");
    let no_tools = matches.get_flag("no-tools");
    let debug = matches.get_flag("debug");

    // Resolve directory to absolute path
    let project_root = directory.canonicalize().unwrap_or(directory);

    // Print startup banner
    println!();
    println!("╭─────────────────────────────────────────────────────────╮");
    println!("│           🤖 OSVM Code - AI Coding Assistant            │");
    println!("╰─────────────────────────────────────────────────────────╯");
    println!();
    println!("  Project: {}", project_root.display());

    if yolo_mode {
        println!("  ⚠️  YOLO Mode: Auto-approving all operations!");
    }
    if no_tools {
        println!("  📝 Chat-only mode (tools disabled)");
    }
    if debug {
        println!("  🐛 Debug mode enabled");
    }

    println!();
    println!("  Commands:");
    println!("    /help     - Show available commands");
    println!("    /clear    - Clear conversation");
    println!("    /quit     - Exit");
    println!();

    run_code_chat_loop(project_root, initial_prompt, yolo_mode, no_tools, debug).await
}

/// Maximum tool execution iterations to prevent infinite loops
const MAX_TOOL_ITERATIONS: usize = 20;

/// Run the interactive code chat loop with tool execution
async fn run_code_chat_loop(
    project_root: PathBuf,
    initial_prompt: Option<String>,
    yolo_mode: bool,
    no_tools: bool,
    debug: bool,
) -> Result<()> {
    use crate::services::ai_service::AiService;
    use crate::utils::tui::code::tools::{Tool, ToolContext, ToolRegistry};
    use crate::utils::tui::code::prompt::{build_system_prompt, extract_text_content, parse_tool_calls};
    use crate::utils::tui::code::permissions::PermissionManager;
    use std::io::{self, Write};

    let ai_service = AiService::new();
    let registry = ToolRegistry::new();
    let context = ToolContext::new(project_root);
    let mut permissions = PermissionManager::new();

    // Build system prompt
    let system_prompt = build_system_prompt(&context, &registry);

    // Track conversation history for context
    let mut conversation: Vec<String> = Vec::new();

    if debug {
        println!("📋 System prompt length: {} chars", system_prompt.len());
        println!("🔧 Available tools: {:?}", registry.list_tools());
        println!();
    }

    // Process initial prompt if provided
    if let Some(prompt) = initial_prompt {
        process_user_input(
            &prompt,
            &ai_service,
            &registry,
            &context,
            &mut permissions,
            &system_prompt,
            &mut conversation,
            yolo_mode,
            no_tools,
            debug,
        ).await?;
    }

    // Interactive loop
    loop {
        print!("\n\x1b[1;36mYou:\x1b[0m ");
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let input = input.trim();

        if input.is_empty() {
            continue;
        }

        // Handle commands
        if input.starts_with('/') {
            match input {
                "/quit" | "/exit" | "/q" => {
                    println!("👋 Goodbye!");
                    break;
                }
                "/clear" => {
                    print!("\x1B[2J\x1B[1;1H"); // Clear screen
                    conversation.clear();
                    permissions.reset_session();
                    println!("💬 Conversation and session cleared");
                    continue;
                }
                "/help" => {
                    print_help();
                    continue;
                }
                "/tools" => {
                    println!("\n🔧 Available tools:");
                    for tool_name in registry.list_tools() {
                        if let Some(tool) = registry.get(&tool_name) {
                            println!("  \x1b[1;33m{}\x1b[0m - {}", tool_name, tool.description());
                        }
                    }
                    continue;
                }
                "/context" => {
                    println!("\n📁 Project: {}", context.project_name);
                    println!("   Root: {}", context.project_root.display());
                    println!("   History: {} messages", conversation.len());
                    continue;
                }
                _ => {
                    println!("❓ Unknown command: {}", input);
                    println!("   Type /help for available commands");
                    continue;
                }
            }
        }

        // Process the user input with tool execution
        process_user_input(
            input,
            &ai_service,
            &registry,
            &context,
            &mut permissions,
            &system_prompt,
            &mut conversation,
            yolo_mode,
            no_tools,
            debug,
        ).await?;
    }

    Ok(())
}

/// Process user input, execute tools, and display results
async fn process_user_input(
    input: &str,
    ai_service: &crate::services::ai_service::AiService,
    registry: &crate::utils::tui::code::tools::ToolRegistry,
    context: &crate::utils::tui::code::tools::ToolContext,
    permissions: &mut crate::utils::tui::code::permissions::PermissionManager,
    system_prompt: &str,
    conversation: &mut Vec<String>,
    yolo_mode: bool,
    no_tools: bool,
    debug: bool,
) -> Result<()> {
    use crate::utils::tui::code::prompt::{extract_text_content, parse_tool_calls};
    use crate::utils::tui::code::tools::Tool;
    use std::io::{self, Write};

    // Add user message to history
    conversation.push(format!("USER: {}", input));

    println!("\n\x1b[2m🤔 Thinking...\x1b[0m");

    // Build full prompt with conversation context
    let full_prompt = build_conversation_prompt(system_prompt, conversation);

    if debug {
        println!("📝 Full prompt: {} chars", full_prompt.len());
    }

    // Query AI
    let mut current_response = match ai_service.query_with_debug(&full_prompt, debug).await {
        Ok(r) => r,
        Err(e) => {
            println!("\n\x1b[1;31m❌ Error:\x1b[0m {}", e);
            return Ok(());
        }
    };

    // Tool execution loop
    let mut iteration = 0;
    loop {
        iteration += 1;
        if iteration > MAX_TOOL_ITERATIONS {
            println!("\n\x1b[1;33m⚠️  Max tool iterations reached. Stopping.\x1b[0m");
            break;
        }

        // Parse tool calls from response
        let tool_calls = parse_tool_calls(&current_response);

        // Extract and display text content
        let text_content = extract_text_content(&current_response);
        if !text_content.is_empty() {
            println!("\n\x1b[1;32m🤖 Assistant:\x1b[0m");
            println!("{}", text_content);
        }

        // If no tools or tools disabled, we're done
        if tool_calls.is_empty() || no_tools {
            conversation.push(format!("ASSISTANT: {}", current_response));
            break;
        }

        // Execute each tool
        let mut tool_results = Vec::new();
        for call in &tool_calls {
            // Get the tool
            let tool = match registry.get(&call.name) {
                Some(t) => t,
                None => {
                    println!("\n\x1b[1;31m❌ Unknown tool: {}\x1b[0m", call.name);
                    tool_results.push(format!(
                        "ERROR: Unknown tool '{}'. Available tools: {:?}",
                        call.name,
                        registry.list_tools()
                    ));
                    continue;
                }
            };

            // Check if approval needed
            let needs_approval = !yolo_mode && tool.requires_approval(&call.params);

            if needs_approval {
                // Show preview and ask for approval
                println!("\n\x1b[1;33m⚡ Tool: {}\x1b[0m", call.name);
                if let Some(preview) = tool.generate_preview(&call.params, context) {
                    println!("{}", preview);
                } else {
                    println!("Params: {}", serde_json::to_string_pretty(&call.params).unwrap_or_default());
                }

                // Ask for approval
                print!("\n\x1b[1;36mApprove? [y/N/always]: \x1b[0m");
                io::stdout().flush()?;

                let mut approval = String::new();
                io::stdin().read_line(&mut approval)?;
                let approval = approval.trim().to_lowercase();

                match approval.as_str() {
                    "y" | "yes" => {
                        // Continue to execute
                    }
                    "always" | "a" => {
                        // Approve for session
                        permissions.approve_tool_for_session(&call.name);
                        println!("\x1b[2m✓ Auto-approving '{}' for this session\x1b[0m", call.name);
                    }
                    _ => {
                        println!("\x1b[2m✗ Skipped\x1b[0m");
                        tool_results.push(format!("SKIPPED: User declined to execute {}", call.name));
                        continue;
                    }
                }
            }

            // Execute the tool
            println!("\n\x1b[2m⏳ Executing {}...\x1b[0m", call.name);

            match tool.execute(call.params.clone(), context).await {
                Ok(output) => {
                    // Truncate long outputs for display
                    let display_output = if output.text.len() > 2000 {
                        format!("{}...\n[truncated, {} total chars]", &output.text[..2000], output.text.len())
                    } else {
                        output.text.clone()
                    };

                    println!("\x1b[2m✓ Result:\x1b[0m");
                    println!("{}", display_output);

                    tool_results.push(format!(
                        "TOOL RESULT [{}]: {}",
                        call.name,
                        output.text
                    ));
                }
                Err(e) => {
                    println!("\x1b[1;31m✗ Error: {}\x1b[0m", e);
                    tool_results.push(format!(
                        "TOOL ERROR [{}]: {}",
                        call.name,
                        e
                    ));
                }
            }
        }

        // If we executed tools, send results back to AI
        if !tool_results.is_empty() {
            // Add current response and results to conversation
            conversation.push(format!("ASSISTANT: {}", current_response));
            conversation.push(tool_results.join("\n\n"));

            println!("\n\x1b[2m🤔 Processing results...\x1b[0m");

            // Build new prompt with tool results
            let followup_prompt = build_conversation_prompt(system_prompt, conversation);

            // Query AI again
            current_response = match ai_service.query_with_debug(&followup_prompt, debug).await {
                Ok(r) => r,
                Err(e) => {
                    println!("\n\x1b[1;31m❌ Error:\x1b[0m {}", e);
                    break;
                }
            };
        } else {
            // No tool results, we're done
            conversation.push(format!("ASSISTANT: {}", current_response));
            break;
        }
    }

    Ok(())
}

/// Build conversation prompt with history
fn build_conversation_prompt(system_prompt: &str, conversation: &[String]) -> String {
    let mut prompt = format!("{}\n\n", system_prompt);

    // Add last N messages of conversation (keep context manageable)
    let history_start = conversation.len().saturating_sub(10);
    for msg in &conversation[history_start..] {
        prompt.push_str(msg);
        prompt.push_str("\n\n");
    }

    prompt.push_str("ASSISTANT:");
    prompt
}

fn print_help() {
    println!();
    println!("╭─────────────────────────────────────────────────────────╮");
    println!("│              📚 OSVM Code - Commands                    │");
    println!("╰─────────────────────────────────────────────────────────╯");
    println!();
    println!("  \x1b[1;33m/help\x1b[0m      - Show this help message");
    println!("  \x1b[1;33m/tools\x1b[0m     - List available tools with descriptions");
    println!("  \x1b[1;33m/context\x1b[0m   - Show project context and history");
    println!("  \x1b[1;33m/clear\x1b[0m     - Clear conversation and session approvals");
    println!("  \x1b[1;33m/quit\x1b[0m      - Exit the application");
    println!();
    println!("  \x1b[1;36m💡 Tips:\x1b[0m");
    println!("  • Ask the AI to read files before editing them");
    println!("  • Be specific about what changes you want");
    println!("  • Use 'always' at approval prompts to auto-approve a tool");
    println!("  • Use --yolo flag to auto-approve everything (dangerous!)");
    println!();
}
