//! Advanced Agent Chat UI with FAR-style design using cursive-multiplex
//!
//! This module provides a comprehensive chat interface with multiple chat sessions,
//! AI-powered tool planning, background agent execution, and session recording.

use anyhow::Result;
use cursive::{Cursive, CursiveExt};

// Public module exports
pub mod types;
pub mod session;
pub mod state;
pub mod agent;
pub mod ui;
pub mod utils;

// Re-export commonly used types
pub use types::{ChatMessage, AgentState};
pub use session::ChatSession;
pub use state::AdvancedChatState;
pub use agent::AgentCommand;
pub use ui::{AdvancedChatUI, update_ui_displays};

/// Main entry point for the advanced agent chat UI
pub async fn run_advanced_agent_chat() -> Result<()> {
    println!("Starting OSVM Advanced Agent Chat Interface...");

    // Check if we're in a terminal environment
    if std::env::var("TERM").is_err() || std::env::var("CI").is_ok() {
        return run_advanced_demo_mode().await;
    }

    // Initialize the state (skip problematic initialization that can hang)
    let state = AdvancedChatState::new()?;

    // Skip initialize() and start_agent_worker() as they can hang on MCP connections
    // The UI will work without these background services for basic functionality
    // TODO: Add async initialization option in the UI for optional MCP features

    // Create default session
    let _default_session_id = state.create_session("Main Chat".to_string())?;

    // Run the UI in a blocking task to avoid runtime conflicts
    let result = tokio::task::spawn_blocking(move || {
        run_advanced_ui_sync(state)
    }).await;

    match result {
        Ok(Ok(())) => Ok(()),
        Ok(Err(e)) => {
            eprintln!("UI Error: {}", e);
            run_advanced_demo_mode().await
        }
        Err(e) => {
            eprintln!("Failed to run UI: {}", e);
            run_advanced_demo_mode().await
        }
    }
}

/// Synchronous UI runner to avoid async runtime conflicts
fn run_advanced_ui_sync(state: AdvancedChatState) -> Result<()> {
    // Create Cursive - it will panic if no terminal is available
    // This is handled by the caller which falls back to demo mode
    let mut siv = Cursive::default();

    // Enable mouse support for better interaction
    siv.set_window_title("OSVM Advanced Agent Chat");

    // Configure better key handling
    siv.set_autorefresh(true);

    // Set the state as user data for the UI
    siv.set_user_data(state.clone());

    // Create UI wrapper to access methods
    let ui = AdvancedChatUI { state: state.clone() };

    // Start spinner animation after Cursive is initialized
    state.start_spinner_animation(siv.cb_sink().clone());

    // Set up the FAR-style UI layout
    ui.setup_far_ui(&mut siv);

    // Add global hotkeys for suggestions and actions
    ui.setup_suggestion_hotkeys(&mut siv);
    ui.setup_action_hotkeys(&mut siv);

    // Update displays
    update_ui_displays(&mut siv);

    // Run the TUI
    siv.run();

    Ok(())
}

/// Demo mode for advanced chat interface
async fn run_advanced_demo_mode() -> Result<()> {
    println!("Running Advanced Agent Chat in demo mode");
    println!();

    // Initialize services to show real MCP status
    let state = AdvancedChatState::new()?;
    state.initialize().await?;

    println!("Advanced Features:");
    println!("   • AI-powered input parsing and tool planning");
    println!("   • Multiple chat sessions with background agent execution");
    println!("   • Session recording and agent control (run/pause/stop)");
    println!("   • Comprehensive MCP tool integration");
    println!();

    // Show available MCP tools
    println!("Available MCP Tools:");
    let available_tools = state.available_tools.read()
        .map_err(|e| anyhow::anyhow!("Failed to read tools: {}", e))?;

    if available_tools.is_empty() {
        println!("   ⚠️  No MCP servers configured");
        println!("   Run 'osvm mcp setup' to configure Solana tools");
    } else {
        for (server_id, tools) in available_tools.iter() {
            println!("   📦 Server: {}", server_id);
            if tools.is_empty() {
                println!("      Loading tools...");
            } else {
                for (i, tool) in tools.iter().enumerate() {
                    if i < 5 {
                        println!("      • {}: {}",
                            tool.name,
                            tool.description.as_ref().unwrap_or(&"No description".to_string()));
                    }
                }
                if tools.len() > 5 {
                    println!("      ... and {} more tools", tools.len() - 5);
                }
            }
        }
    }
    println!();
    println!("Advanced FAR-Style Layout:");
    println!("   ┌─ Chat Sessions ─┬─ Active Chat History ─────────────────┐");
    println!("   | Main Chat       | You: What's my balance?                |");
    println!("   | Analysis        | Agent: I'll analyze your request...   |");
    println!("   | Paused Chat     | Plan: Using get_balance tool          |");
    println!("   |                 | Executing: get_balance                |");
    println!("   | + New Chat      | Result: 2.5 SOL                       |");
    println!("   |                 | Agent: Your balance is 2.5 SOL        |");
    println!("   | Run             |                                        |");
    println!("   | Pause           | Agent Status: Idle                     |");
    println!("   | Stop            |                                        |");
    println!("   │                 │ You: [Input field...] [Send]          │");
    println!("   | Record          | [Clear] [Export] [Settings] [Help]    |");
    println!("   | Stop Rec        |                                        |");
    println!("   └─────────────────┴────────────────────────────────────────┘");
    println!();
    println!("Key Improvements:");
    println!("   • AI service integration for intelligent tool selection");
    println!("   • Background agent processing with state management");
    println!("   • Professional FAR-style interface design");
    println!("   • Session recording and replay capabilities");
    println!("   • Multi-chat support with independent agent states");
    println!();
    println!("Reply Suggestions (press 1-5 to insert, ESC to hide):");
    println!();
    // Show suggestions with blue background simulation (in terminal would be colored)
    println!("\x1b[44;37m 1. Show recent transactions                         \x1b[0m");
    println!("\x1b[44;37m 2. What's the current SOL price?                    \x1b[0m");
    println!("\x1b[44;37m 3. How do I stake my SOL?                           \x1b[0m");
    println!("\x1b[44;37m 4. Send 0.5 SOL to address                          \x1b[0m");
    println!("\x1b[44;37m 5. Check my staking rewards                         \x1b[0m");
    println!();
    println!("   Suggestions Features:");
    println!("   • Generated by AI using chat context");
    println!("   • Blue background with white text for visibility");
    println!("   • Press number key (1-5) to insert at cursor");
    println!("   • ESC hides suggestions");
    println!();

    // Run in demo mode if terminal not available
    println!("Note: Full TUI interface requires a proper terminal.");
    println!("   Run 'osvm chat --advanced' in a terminal to see the full UI.");

    Ok(())
}