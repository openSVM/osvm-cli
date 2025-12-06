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
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm ide
     Launch IDE in current directory.
     💡 Opens file browser, ready to create/edit .ovsm files.

  2. osvm ide ~/my-scripts
     Open IDE in specific project folder.
     💡 File tree shows all .ovsm files in directory.

  3. osvm ide script.ovsm
     Open IDE with specific file loaded.
     💡 Jump straight into editing.

  4. osvm ide --theme monokai
     Use Monokai color scheme.
     💡 Available: dark, light, monokai, solarized

  5. F5 in IDE
     Run current script and see output.
     💡 Results appear in output panel below.

  6. Ctrl+S in IDE
     Save current file.
     💡 Also triggers syntax check.

  7. Tab in IDE
     Switch focus between panels.
     💡 File tree → Editor → Output → File tree

  8. osvm ide --auto-save 30
     Auto-save every 30 seconds.
     💡 Never lose work to crashes.

  9. Ctrl+N in IDE
     Create new untitled file.
     💡 Prompts for filename on save.

 10. Ctrl+B in IDE
     Toggle file tree visibility.
     💡 More screen space for editing.

💡 OVSM LISP QUICK START:
  ;; Hello World
  (log "Hello from OVSM!")

  ;; Variables
  (define x 10)
  (set! x (+ x 5))

  ;; Control flow
  (if (> x 10) "big" "small")

  ;; Loops
  (for (i (range 1 5))
    (log "i =" i))

KEYBOARD SHORTCUTS:
  F5 / Ctrl+R    Run current file
  Ctrl+S         Save file
  Ctrl+N         New file
  Ctrl+B         Toggle file tree
  Tab            Switch focus
  Ctrl+Q         Quit

FILE TYPES:
  .ovsm    OVSM LISP script (executable)
  .ovsmx   OVSM compiled bytecode
  .lisp    Generic LISP (syntax supported)
"#)
}
