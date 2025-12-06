use clap::{Arg, ArgAction, Command};

/// Build the chat command
pub fn build_chat_command() -> Command {
    Command::new("chat")
        .about("Interactive AI-powered agent chat with MCP tools and intelligent planning")
        .long_about(
            "Launch a comprehensive chat interface with AI-powered tool planning and execution.\n\
                           \n\
                           Basic Mode:\n\
                           • Simple chat interface with MCP tool integration\n\
                           • Single chat session\n\
                           • Direct tool calling\n\
                           \n\
                           Advanced Mode (--advanced, default when no args):\n\
                           • FAR-style/Borland TUI design with dual panels\n\
                           • AI-powered input parsing and intelligent tool planning\n\
                           • Multi-session management with background agent execution\n\
                           • Session recording and agent control (run/pause/stop)\n\
                           • Professional keyboard shortcuts and vim-like navigation",
        )
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm chat
     Launch basic chat interface for quick interactions.
     💡 Type naturally - AI understands context and uses tools automatically.

  2. osvm chat --advanced
     Launch the full FAR-style TUI with dual panels.
     💡 Left panel: sessions/tools. Right panel: chat. F1-F12 for actions.

  3. osvm chat --microvm
     Run chat in isolated microVM for maximum security.
     💡 All AI tool executions happen in ephemeral sandboxed VMs.

  4. osvm chat --debug
     Enable debug mode to see AI reasoning and tool calls.
     💡 Great for understanding how the agent plans and executes.

  5. osvm chat --test
     Run comprehensive UI tests with screenshots.
     💡 Validates that the TUI renders correctly in your terminal.

  6. osvm chat --test-mode
     Enable programmatic input mode (line-buffered).
     💡 Useful for scripting and automated testing of chat.

  7. OPENAI_KEY=sk-... osvm chat
     Use a specific API key for the chat session.
     💡 Supports OpenAI, Anthropic, and local models via OPENAI_URL.

  8. RUST_LOG=debug osvm chat
     Enable debug logging to see tool calls and reasoning.
     💡 Helps understand AI decision-making process.

  9. Ctrl+C in chat
     Gracefully exit chat, saving session history.
     💡 Sessions are persisted in ~/.osvm/chat_sessions/

 10. osvm chat (then type: /export chat.md)
     Export chat history from within the session.
     💡 Use slash commands for session management.

💡 PRO TIPS:
  • Type "/help" in chat for available slash commands
  • Use Tab for autocompletion of tool names and parameters
  • Press Ctrl+L to clear the screen, Ctrl+E for multiline input
  • The AI can call multiple MCP tools in sequence automatically
  • Try: "Show my wallet balance and recent transactions"

KEYBOARD SHORTCUTS (Advanced Mode):
  F1: Help    F2: New Session    F3: Load    F4: Save
  F5: Run     F6: Pause          F7: Stop    F10: Quit
  Tab: Switch panels    ↑↓: Navigate    Enter: Send
"#)
        .arg(
            Arg::new("debug")
                .long("debug")
                .action(ArgAction::SetTrue)
                .help("Enable debug mode for chat interface"),
        )
        .arg(
            Arg::new("test")
                .long("test")
                .action(ArgAction::SetTrue)
                .help("Run comprehensive UI tests and show screenshots"),
        )
        .arg(
            Arg::new("advanced")
                .long("advanced")
                .action(ArgAction::SetTrue)
                .help("Launch advanced FAR-style chat interface with AI planning and multi-session support"),
        )
        .arg(
            Arg::new("microvm")
                .long("microvm")
                .action(ArgAction::SetTrue)
                .help("Run chat in isolated microVM with ephemeral VMs for all tool executions"),
        )
        .arg(
            Arg::new("test_mode")
                .long("test-mode")
                .action(ArgAction::SetTrue)
                .help("Enable test mode with line-buffered input for programmatic access (disables raw mode)"),
        )
}
