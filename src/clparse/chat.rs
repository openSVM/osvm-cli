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
