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
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm code
     Start interactive coding assistant in current directory.
     💡 AI reads files, understands codebase, helps you code.

  2. osvm code "fix the bug in main.rs"
     Start with an initial task.
     💡 AI immediately begins working on your request.

  3. osvm code -d ~/my-project
     Open a specific project directory.
     💡 AI has context of entire project structure.

  4. osvm code "add unit tests for auth module"
     AI writes tests based on your code.
     💡 Understands code patterns and suggests test cases.

  5. osvm code "refactor to use async/await"
     Large-scale refactoring with AI assistance.
     💡 Shows diffs before applying changes.

  6. osvm code --yolo "format all files"
     Auto-approve all changes (dangerous!).
     💡 Only use for trusted operations like formatting.

  7. osvm code "explain this function"
     Get explanations of complex code.
     💡 AI reads context and explains in plain English.

  8. osvm code -m claude-sonnet-4-20250514
     Use specific AI model.
     💡 Different models have different capabilities.

  9. osvm code --no-tools
     Chat-only mode without file operations.
     💡 Safe mode for getting advice without changes.

 10. osvm code "review this PR for security issues"
     Security-focused code review.
     💡 AI analyzes code for vulnerabilities.

💡 AI CAPABILITIES:
  File Operations:
  • Read any file in the project
  • Write new files with preview
  • Edit existing files (shows diff)
  • Search with glob and grep

  Command Execution:
  • Run tests, builds, linters
  • Safe commands auto-approved
  • Dangerous commands need confirmation

SAFETY FEATURES:
  • All file changes shown as diffs
  • Commands require approval (unless --yolo)
  • Can undo recent changes
  • Sandboxed execution

PRO TIPS:
  • Use Ctrl+C to interrupt long operations
  • Type "undo" to revert last change
  • Use specific file paths for targeted edits
  • The AI remembers conversation context
"#)
}
