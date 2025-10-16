//! Advanced Agent Chat UI with FAR-style design using cursive-multiplex
//!
//! This module provides a comprehensive chat interface with multiple chat sessions,
//! AI-powered tool planning, background agent execution, and session recording.

use anyhow::Result;
use cursive::{Cursive, CursiveExt};
use log::{error, info, warn};

// Public module exports
pub mod agent;
pub mod persistence;
pub mod session;
pub mod state;
pub mod types;
pub mod ui;
pub mod utils;

// Re-export commonly used types
pub use agent::AgentCommand;
pub use session::ChatSession;
pub use state::AdvancedChatState;
pub use types::{AgentState, ChatMessage};
pub use ui::{update_ui_displays, AdvancedChatUI};

/// Main entry point for the advanced agent chat UI
pub async fn run_advanced_agent_chat() -> Result<()> {
    // Set environment variable to indicate OSVM agent is running
    // This makes the status bar count the agent as a microVM
    std::env::set_var("OSVM_AGENT_MODE", "1");

    println!("Starting OSVM Advanced Agent Chat Interface...");

    // Check if we're in a terminal environment - only fail if clearly no terminal support
    let term_var = std::env::var("TERM").unwrap_or_default();
    if term_var.is_empty() || term_var == "dumb" {
        println!(
            "No suitable terminal detected (TERM={}), falling back to demo mode",
            term_var
        );
        return run_advanced_demo_mode().await;
    }

    println!("Terminal detected: {}", term_var);

    // Initialize the state and MCP services (safe now that problematic servers are disabled)
    let state = AdvancedChatState::new()?;

    // Auto-clone, build, and start default OSVM MCP server
    {
        let mut mcp_service = state.mcp_service.lock().await;

        // Load existing config
        let _ = mcp_service.load_config();

        // Check if osvm-mcp server already exists
        let server_exists = mcp_service.get_server("osvm-mcp").is_some();

        if !server_exists {
            println!("🔧 Auto-cloning OSVM MCP server from GitHub...");
            println!("   Repository: https://github.com/openSVM/osvm-mcp");

            // Use add_server_from_github to clone and configure
            match mcp_service
                .add_server_from_github(
                    "osvm-mcp".to_string(),
                    "https://github.com/openSVM/osvm-mcp".to_string(),
                    Some("OSVM MCP Server".to_string()),
                    true, // skip_confirmation for automated setup
                )
                .await
            {
                Ok(_) => {
                    println!("✅ OSVM MCP server cloned and built");
                    println!("   Using stdio transport (Node.js MCP server)");
                }
                Err(e) => {
                    warn!("⚠️  Failed to auto-clone osvm-mcp: {}", e);
                    println!("   💡 You can manually run: osvm mcp add-github osvm-mcp https://github.com/openSVM/osvm-mcp");
                }
            }
        }

        // osvm-mcp uses stdio transport - it's started on-demand when tools are called
        // No need to keep a persistent server process running
    }

    // Initialize MCP tools with timeout to prevent hanging
    let init_timeout = tokio::time::Duration::from_secs(5);
    match tokio::time::timeout(init_timeout, state.initialize()).await {
        Ok(Ok(())) => {
            info!("✅ MCP tools initialized successfully");
        }
        Ok(Err(e)) => {
            warn!(
                "⚠️  MCP initialization failed: {}, continuing without MCP tools",
                e
            );
        }
        Err(_) => {
            warn!("⏱️  MCP initialization timed out after 5s, continuing without MCP tools");
        }
    }

    // Start agent worker for background processing
    if let Err(e) = state.start_agent_worker().await {
        warn!(
            "Failed to start agent worker: {}, chat will work without background processing",
            e
        );
    } else {
        info!("✅ Agent worker started successfully");
    }

    // BUG-2002 fix: Do NOT create duplicate "Main Chat" session here!
    // AdvancedChatState::new() already creates it during initialization
    // Creating it again would overwrite the first session and cause data loss

    // Run the UI in a blocking task to avoid runtime conflicts
    let result = tokio::task::spawn_blocking(move || run_advanced_ui_sync(state)).await;

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
    // Create Cursive with panic protection - fallback to demo mode if no TTY
    let mut siv = match std::panic::catch_unwind(|| Cursive::default()) {
        Ok(siv) => siv,
        Err(e) => {
            eprintln!("Failed to initialize terminal UI: {:?}", e);
            eprintln!("This usually means no interactive terminal is available.");
            eprintln!("Falling back to demo mode...");
            return Err(anyhow::anyhow!("Terminal initialization failed"));
        }
    };

    // Check terminal size - but allow 0x0 for automated environments
    let term_size = siv.screen_size();
    println!("Terminal size: {}x{}", term_size.x, term_size.y);

    // Only fail on very specific size constraints (not 0x0 which means "unknown")
    if (term_size.x > 0 && term_size.x < 60) || (term_size.y > 0 && term_size.y < 15) {
        eprintln!(
            "Terminal too small: {}x{} (minimum: 60x15)",
            term_size.x, term_size.y
        );
        eprintln!("Please resize your terminal and try again.");
        return Err(anyhow::anyhow!(
            "Terminal size too small for advanced chat interface"
        ));
    }

    if term_size.x == 0 || term_size.y == 0 {
        println!("Unknown terminal size - likely running in non-interactive environment");
        println!("Will attempt to run UI - if terminal is unavailable, will fallback to demo mode");
    }

    // Enable mouse support for better interaction
    siv.set_window_title("OSVM Advanced Agent Chat");
    // Note: Mouse support enabled by default in recent cursive versions

    // Configure better key handling
    siv.set_autorefresh(true);

    // Add resize handling with error protection
    siv.add_global_callback(cursive::event::Event::WindowResize, |siv| {
        // Wrap resize handling in error protection to prevent crashes
        if let Err(e) = handle_window_resize(siv) {
            eprintln!("⚠️ Resize handling error: {}", e);
            // Continue running even if resize handling fails
        }
    });

    // Set the state as user data for the UI
    siv.set_user_data(state.clone());

    // Create UI wrapper to access methods
    let ui = AdvancedChatUI {
        state: state.clone(),
    };

    // Start spinner animation and periodic updates after Cursive is initialized
    state.start_spinner_animation(siv.cb_sink().clone());
    start_periodic_updates(&mut siv, state.clone());

    // Set up the FAR-style UI layout
    ui.setup_far_ui(&mut siv);

    // Set up auto-suggestions as user types
    ui::handlers::setup_input_suggestions(&mut siv, state.clone());

    // Add global hotkeys for suggestions and actions
    ui.setup_suggestion_hotkeys(&mut siv);
    ui.setup_action_hotkeys(&mut siv);

    // Update displays
    update_ui_displays(&mut siv);

    // Show welcome dialog for first-time users
    ui::onboarding::show_welcome_dialog_if_first_time(&mut siv);

    // Run the TUI with panic protection for internal cursive operations
    // This catches panics in cursive's event loop and worker threads
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        siv.run();
    })) {
        Ok(_) => {
            println!("✅ UI closed gracefully");
            Ok(())
        }
        Err(e) => {
            eprintln!("⚠️  UI event loop panicked: {:?}", e);
            eprintln!("This can happen when terminal device is unavailable.");
            eprintln!("The UI will fall back to demo mode.");
            Err(anyhow::anyhow!("UI event loop panic - terminal device unavailable"))
        }
    }
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
    let available_tools = state
        .available_tools
        .read()
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
                        println!(
                            "      • {}: {}",
                            tool.name,
                            tool.description
                                .as_ref()
                                .unwrap_or(&"No description".to_string())
                        );
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

/// Handle window resize events safely
fn handle_window_resize(siv: &mut Cursive) -> anyhow::Result<()> {
    println!("\n🔄 Window resize detected, refreshing layout...");

    // Get new terminal size safely
    let new_size = siv.screen_size();
    println!("📐 New terminal size: {}x{}", new_size.x, new_size.y);

    // Check if terminal is still usable
    if (new_size.x > 0 && new_size.x < 60) || (new_size.y > 0 && new_size.y < 15) {
        println!(
            "⚠️ Terminal too small after resize: {}x{}",
            new_size.x, new_size.y
        );

        // Try to show warning dialog, but don't crash if it fails
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            siv.add_layer(
                cursive::views::Dialog::info(format!(
                    "Terminal too small: {}x{} (minimum: 60x15)\nPlease resize your terminal.",
                    new_size.x, new_size.y
                ))
                .title("Resize Required")
                .button("OK", |s| {
                    s.pop_layer();
                }),
            );
        }));

        if result.is_err() {
            println!("⚠️ Could not show resize warning dialog");
        }

        return Ok(());
    }

    // Safely refresh all UI displays to adapt to new size
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        update_ui_displays(siv);
    }));

    match result {
        Ok(_) => {
            println!(
                "✅ Layout refreshed successfully for size {}x{}",
                new_size.x, new_size.y
            );
            Ok(())
        }
        Err(_) => {
            println!("⚠️ Layout refresh failed, but continuing...");
            Err(anyhow::anyhow!("Layout refresh failed during resize"))
        }
    }
}

/// Set up periodic UI updates every 30 seconds as requested
fn start_periodic_updates(siv: &mut Cursive, state: AdvancedChatState) {
    use std::thread;
    use std::time::Duration;

    let cb_sink = siv.cb_sink().clone();

    // Start background thread for periodic updates
    thread::spawn(move || {
        loop {
            thread::sleep(Duration::from_secs(30)); // Update every 30 seconds as requested

            // Request UI update
            if cb_sink
                .send(Box::new(move |siv| {
                    update_ui_displays(siv);
                }))
                .is_err()
            {
                break; // Exit if UI is closed
            }
        }
    });
}
