use clap::{Arg, ArgAction, Command};

/// Build the code command for AI-powered coding assistant
pub fn build_code_command() -> Command {
    Command::new("code")
        .about("AI-powered coding assistant (Claude Code style)")
        .long_about(
            "Launch an AI-powered coding assistant that can read, write, and edit files,\n\
             run commands, and help you with software development tasks.\n\
             \n\
             Features:\n\
             • File operations: read, write, edit with diff preview\n\
             • Command execution with smart approval (safe commands auto-approved)\n\
             • Search: glob patterns and grep for code search\n\
             • Permission system: approve changes before they're applied\n\
             • Streaming responses with thinking blocks\n\
             \n\
             Examples:\n\
               osvm code                       # Interactive mode in current directory\n\
               osvm code -d ~/project          # Open specific project\n\
               osvm code \"add unit tests\"      # Start with an initial prompt\n\
               osvm code --yolo \"fix all bugs\" # Auto-approve everything (dangerous!)",
        )
        .arg(
            Arg::new("prompt")
                .help("Initial prompt to send to the AI")
                .index(1),
        )
        .arg(
            Arg::new("directory")
                .short('d')
                .long("directory")
                .value_name("PATH")
                .default_value(".")
                .help("Project directory to work in"),
        )
        .arg(
            Arg::new("model")
                .short('m')
                .long("model")
                .value_name("MODEL")
                .help("AI model to use (e.g., claude-sonnet-4-20250514)"),
        )
        .arg(
            Arg::new("yolo")
                .long("yolo")
                .action(ArgAction::SetTrue)
                .help("Auto-approve all tool executions (dangerous!)"),
        )
        .arg(
            Arg::new("no-tools")
                .long("no-tools")
                .action(ArgAction::SetTrue)
                .help("Disable tool usage (chat-only mode)"),
        )
        .arg(
            Arg::new("debug")
                .long("debug")
                .action(ArgAction::SetTrue)
                .help("Enable debug mode"),
        )
}
