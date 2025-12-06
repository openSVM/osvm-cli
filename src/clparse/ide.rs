//! CLI definition for the `osvm ide` command
//!
//! Provides an IDE for OVSM LISP scripting.

use clap::{Arg, Command};

/// Build the ide subcommand
pub fn build_ide_command() -> Command {
    Command::new("ide")
        .about("Integrated development environment for OVSM LISP")
        .long_about(
            "Launch an interactive IDE for writing and running OVSM LISP scripts.\n\n\
             Features:\n\
             • Syntax highlighting for OVSM LISP\n\
             • Interactive REPL for testing expressions\n\
             • File browser for .ovsm files\n\
             • Run scripts with F5\n\
             • Multi-file editing with tabs\n\
             • Output panel for results\n\n\
             Keyboard Shortcuts:\n\
             • F5 / Ctrl+R    - Run current file\n\
             • Ctrl+S         - Save file\n\
             • Ctrl+N         - New file\n\
             • Ctrl+B         - Toggle file tree\n\
             • Tab            - Switch focus\n\
             • Ctrl+Q         - Quit\n\n\
             Examples:\n\
               osvm ide                        # Open IDE in current directory\n\
               osvm ide /path/to/project       # Open IDE in specific directory\n\
               osvm ide script.ovsm            # Open IDE with specific file"
        )
        // Path to working directory or file
        .arg(
            Arg::new("path")
                .help("Working directory or file to open")
                .value_name("PATH")
                .index(1)
        )
        // Explicit file argument
        .arg(
            Arg::new("file")
                .long("file")
                .short('f')
                .help("Specific file to open")
                .value_name("FILE")
        )
        // Theme
        .arg(
            Arg::new("theme")
                .long("theme")
                .short('t')
                .help("Color theme")
                .value_name("THEME")
                .value_parser(["dark", "light", "monokai", "solarized"])
                .default_value("dark")
        )
        // Font size (for future GUI support)
        .arg(
            Arg::new("font-size")
                .long("font-size")
                .help("Font size (for future GUI)")
                .value_name("SIZE")
                .default_value("12")
        )
        // Show line numbers
        .arg(
            Arg::new("line-numbers")
                .long("line-numbers")
                .help("Show line numbers (default: true)")
                .value_name("BOOL")
                .default_value("true")
        )
        // Auto-save interval
        .arg(
            Arg::new("auto-save")
                .long("auto-save")
                .help("Auto-save interval in seconds (0 to disable)")
                .value_name("SECONDS")
                .default_value("0")
        )
}
