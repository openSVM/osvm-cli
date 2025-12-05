//! Handler for the `osvm code` command - AI-powered coding assistant TUI
//!
//! This provides a Claude Code-style terminal interface using ratatui for
//! AI-assisted coding with file operations, command execution, and approval flows.

use anyhow::Result;
use clap::ArgMatches;
use crossterm::{
    event::{self, Event},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{backend::CrosstermBackend, Terminal};
use std::io;
use std::path::PathBuf;
use std::time::Duration;

use crate::services::ai_service::AiService;
use crate::utils::tui::code::{
    AppStatus, CodeApp, InputResult, Message, MessageRole, ToolCallDisplay, ToolCallStatus,
};
use crate::utils::tui::code::permissions::ApprovalResponse;
use crate::utils::tui::code::prompt::{build_system_prompt, extract_text_content, parse_tool_calls};
use crate::utils::tui::code::tools::Tool;

/// Maximum tool execution iterations to prevent infinite loops
const MAX_TOOL_ITERATIONS: usize = 20;

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

    // Run the TUI
    run_code_tui(project_root, initial_prompt, yolo_mode, no_tools, debug).await
}

/// Run the code assistant TUI
async fn run_code_tui(
    project_root: PathBuf,
    initial_prompt: Option<String>,
    yolo_mode: bool,
    no_tools: bool,
    debug: bool,
) -> Result<()> {
    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create app state
    let mut app = CodeApp::new(project_root, yolo_mode, no_tools, debug);

    // Process initial prompt if provided
    if let Some(prompt) = initial_prompt {
        app.add_user_message(prompt.clone());
        app.conversation_context.push(format!("USER: {}", prompt));
        app.status = AppStatus::Thinking;

        // Draw initial state
        terminal.draw(|f| app.render(f))?;

        // Process the prompt
        process_ai_response(&mut app).await;
    }

    // Main event loop
    let result = run_event_loop(&mut terminal, &mut app).await;

    // Restore terminal
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;

    result
}

/// Main event loop
async fn run_event_loop(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut CodeApp,
) -> Result<()> {
    loop {
        // Draw the UI
        terminal.draw(|f| app.render(f))?;

        // Poll for events with a timeout to allow async processing
        if event::poll(Duration::from_millis(100))? {
            let event = event::read()?;

            match app.handle_event(event) {
                InputResult::Quit => {
                    break;
                }
                InputResult::Submit(input) => {
                    app.add_user_message(input.clone());
                    app.conversation_context.push(format!("USER: {}", input));
                    app.status = AppStatus::Thinking;
                    app.scroll_to_bottom();

                    // Draw thinking state
                    terminal.draw(|f| app.render(f))?;

                    // Process AI response
                    process_ai_response(app).await;
                }
                InputResult::Command(cmd) => {
                    handle_command(app, &cmd);
                }
                InputResult::Approval(response) => {
                    if let Some((tool_call, approved)) = app.handle_approval(response) {
                        if approved {
                            // Execute the approved tool
                            execute_tool_call(app, &tool_call).await;
                        } else {
                            // Tool was rejected
                            app.add_tool_result(&tool_call.name, "Skipped by user", false);
                        }
                    }
                }
                InputResult::Refresh => {
                    // Just redraw
                }
                InputResult::Continue => {
                    // Normal continuation
                }
            }
        }

        // Check if we should quit
        if app.should_quit {
            break;
        }
    }

    Ok(())
}

/// Process AI response and handle tool calls
async fn process_ai_response(app: &mut CodeApp) {
    let full_prompt = app.build_full_prompt();

    // Query AI
    let response = match app.ai_service.query(&full_prompt).await {
        Ok(r) => r,
        Err(e) => {
            app.status = AppStatus::Error(e.to_string());
            app.add_assistant_message(format!("Error: {}", e), vec![]);
            return;
        }
    };

    // Tool execution loop
    let mut current_response = response;
    let mut iteration = 0;

    loop {
        iteration += 1;
        if iteration > MAX_TOOL_ITERATIONS {
            app.add_assistant_message(
                "⚠️ Max tool iterations reached. Stopping.".to_string(),
                vec![],
            );
            break;
        }

        // Parse tool calls
        let tool_calls = parse_tool_calls(&current_response);

        // Extract text content
        let text_content = extract_text_content(&current_response);

        // Create tool call displays
        let tool_displays: Vec<ToolCallDisplay> = tool_calls
            .iter()
            .map(|tc| ToolCallDisplay {
                name: tc.name.clone(),
                status: ToolCallStatus::Pending,
                output: None,
            })
            .collect();

        // Add assistant message
        if !text_content.is_empty() || !tool_displays.is_empty() {
            app.add_assistant_message(text_content, tool_displays);
        }

        // If no tools or tools disabled, we're done
        if tool_calls.is_empty() || app.no_tools {
            app.conversation_context
                .push(format!("ASSISTANT: {}", current_response));
            app.status = AppStatus::Ready;
            break;
        }

        // Execute tools
        let mut tool_results = Vec::new();
        for call in tool_calls {
            let tool = match app.registry.get(&call.name) {
                Some(t) => t,
                None => {
                    tool_results.push(format!(
                        "ERROR: Unknown tool '{}'. Available: {:?}",
                        call.name,
                        app.registry.list_tools()
                    ));
                    continue;
                }
            };

            // Check if approval needed
            if app.needs_approval(tool.as_ref(), &call.params) {
                app.show_approval_modal(call);
                // Wait for user decision - this will be handled in event loop
                return;
            }

            // Execute tool
            app.status = AppStatus::ExecutingTool(call.name.clone());

            match tool.execute(call.params.clone(), &app.context).await {
                Ok(output) => {
                    tool_results.push(format!("TOOL RESULT [{}]: {}", call.name, output.text));
                    app.add_tool_result(&call.name, &output.text, true);
                }
                Err(e) => {
                    tool_results.push(format!("TOOL ERROR [{}]: {}", call.name, e));
                    app.add_tool_result(&call.name, &e.to_string(), false);
                }
            }
        }

        // If we executed tools, send results back to AI
        if !tool_results.is_empty() {
            app.conversation_context
                .push(format!("ASSISTANT: {}", current_response));
            app.conversation_context.push(tool_results.join("\n\n"));

            app.status = AppStatus::Thinking;

            // Query AI again
            let followup_prompt = app.build_full_prompt();
            current_response = match app.ai_service.query(&followup_prompt).await {
                Ok(r) => r,
                Err(e) => {
                    app.status = AppStatus::Error(e.to_string());
                    break;
                }
            };
        } else {
            app.conversation_context
                .push(format!("ASSISTANT: {}", current_response));
            app.status = AppStatus::Ready;
            break;
        }
    }
}

/// Execute a single tool call
async fn execute_tool_call(
    app: &mut CodeApp,
    tool_call: &crate::utils::tui::code::prompt::ParsedToolCall,
) {
    let tool = match app.registry.get(&tool_call.name) {
        Some(t) => t,
        None => {
            app.add_tool_result(&tool_call.name, "Unknown tool", false);
            return;
        }
    };

    app.status = AppStatus::ExecutingTool(tool_call.name.clone());

    match tool.execute(tool_call.params.clone(), &app.context).await {
        Ok(output) => {
            app.add_tool_result(&tool_call.name, &output.text, true);
            app.conversation_context
                .push(format!("TOOL RESULT [{}]: {}", tool_call.name, output.text));
        }
        Err(e) => {
            app.add_tool_result(&tool_call.name, &e.to_string(), false);
            app.conversation_context
                .push(format!("TOOL ERROR [{}]: {}", tool_call.name, e));
        }
    }

    app.status = AppStatus::Ready;

    // Continue processing if needed
    process_ai_response(app).await;
}

/// Handle slash commands
fn handle_command(app: &mut CodeApp, cmd: &str) {
    match cmd.trim() {
        "/quit" | "/exit" | "/q" => {
            app.should_quit = true;
        }
        "/clear" => {
            app.clear_conversation();
            app.add_assistant_message(
                "💬 Conversation cleared".to_string(),
                vec![],
            );
        }
        "/help" => {
            app.add_assistant_message(
                r#"📚 Available Commands:
  /help     - Show this help
  /tools    - List available tools
  /context  - Show project context
  /clear    - Clear conversation
  /quit     - Exit

💡 Tips:
  • Ask the AI to read files before editing
  • Use Tab to switch focus
  • Use ↑↓ or j/k to scroll messages
  • Press y/n/a at approval prompts"#
                    .to_string(),
                vec![],
            );
        }
        "/tools" => {
            let tools: Vec<String> = app
                .registry
                .list_tools()
                .iter()
                .filter_map(|name| {
                    app.registry
                        .get(name)
                        .map(|t| format!("  {} - {}", name, t.description()))
                })
                .collect();

            app.add_assistant_message(
                format!("🔧 Available tools:\n{}", tools.join("\n")),
                vec![],
            );
        }
        "/context" => {
            app.add_assistant_message(
                format!(
                    "📁 Project: {}\n   Root: {}\n   History: {} messages",
                    app.project_name,
                    app.project_root.display(),
                    app.conversation_context.len()
                ),
                vec![],
            );
        }
        _ => {
            app.add_assistant_message(
                format!("❓ Unknown command: {}\n   Type /help for available commands", cmd),
                vec![],
            );
        }
    }
}
