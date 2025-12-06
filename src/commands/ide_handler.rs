//! Handler for the `osvm ide` command
//!
//! Runs the OVSM IDE TUI for LISP scripting.

use anyhow::Result;
use clap::ArgMatches;
use crossterm::{
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{backend::CrosstermBackend, Terminal};
use std::io;
use std::path::PathBuf;

use crate::utils::tui::ide::IdeApp;

/// Handle the ide command
pub async fn handle_ide_command(matches: &ArgMatches) -> Result<()> {
    // Get working directory
    let working_dir = if let Some(path) = matches.get_one::<String>("path") {
        PathBuf::from(path)
    } else {
        std::env::current_dir()?
    };

    // Check for file argument
    let file_to_open = matches.get_one::<String>("file").map(PathBuf::from);

    // Print banner
    print_banner();

    // Run TUI
    run_tui_mode(working_dir, file_to_open).await
}

/// Print startup banner
fn print_banner() {
    println!();
    println!("╔══════════════════════════════════════════════════════════════════╗");
    println!("║                                                                  ║");
    println!("║    📝 OSVM IDE - Integrated Development Environment              ║");
    println!("║                                                                  ║");
    println!("║    Features:                                                     ║");
    println!("║      • OVSM LISP syntax highlighting                             ║");
    println!("║      • Interactive REPL                                          ║");
    println!("║      • File management                                           ║");
    println!("║      • Run scripts with F5                                       ║");
    println!("║                                                                  ║");
    println!("╚══════════════════════════════════════════════════════════════════╝");
    println!();
}

/// Run with TUI
async fn run_tui_mode(working_dir: PathBuf, file_to_open: Option<PathBuf>) -> Result<()> {
    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create app
    let mut app = IdeApp::new(working_dir);

    // Initialize
    app.initialize().await?;

    // Open file if specified
    if let Some(path) = file_to_open {
        if path.exists() {
            let _ = app.open_file(&path);
        }
    }

    // Run main loop
    let result = app.run(&mut terminal).await;

    // Restore terminal
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    result
}
