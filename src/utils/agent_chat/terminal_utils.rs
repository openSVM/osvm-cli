//! Terminal utility functions for display and input management

use super::{Colors, RealtimeSuggestion};
use std::io::{self, Write};
use anyhow::Result;

/// Show input border for visual separation
pub fn show_input_border() {
    println!("{}┌──────────────────────────────────────────────────────┐{}", Colors::GREEN, Colors::RESET);
}

/// Print input prompt
pub fn print_input_prompt() {
    print!("{}│ > {}", Colors::GREEN, Colors::RESET);
    io::stdout().flush().unwrap_or(());
}

/// Close input border
pub fn close_input_border() {
    println!("{}└──────────────────────────────────────────────────────┘{}", Colors::GREEN, Colors::RESET);
}

/// Clear current line
pub fn clear_current_line() {
    print!("\r\x1b[K");
    io::stdout().flush().unwrap_or(());
}

/// Clear suggestions display area - now handled by responsive layout system
#[deprecated(note = "Suggestions are now cleared automatically by the responsive layout system")]
pub fn clear_suggestions_display() {
    // This function is now obsolete - suggestions are cleared automatically by TerminalRenderer
}

/// Clear inline suggestion (ghost text)
pub fn clear_inline_suggestion(current_input: &str) {
    print!("\r\x1b[K{}│ > {}{}", Colors::GREEN, Colors::RESET, current_input);
    io::stdout().flush().unwrap_or(());
}

/// Redraw input line with current text
pub fn redraw_input_line(input: &str) -> Result<()> {
    print!("\r{}│ > {}{}", Colors::GREEN, Colors::RESET, input);
    io::stdout().flush()?;
    Ok(())
}

/// Legacy function - suggestions now handled by InputState::update_suggestions_in_place
#[deprecated(note = "Use InputState::update_suggestions_in_place instead")]
pub fn show_navigable_suggestions(_suggestions: &[RealtimeSuggestion], _selected_index: usize) {
    // This function is now obsolete - suggestions are handled in-place by the new system
}

/// Legacy function - real-time suggestions now handled by new responsive system
#[deprecated(note = "Use InputState::update_suggestions_in_place instead")]
pub fn show_realtime_suggestions(_suggestions: &[RealtimeSuggestion], _current_input: &str) {
    // This function is now obsolete - suggestions are handled in-place by the new system
}

/// Legacy function - real-time suggestions now handled by new responsive system
#[deprecated(note = "Use InputState::update_suggestions_in_place instead")]
pub fn show_realtime_suggestions_fixed(_suggestions: &[RealtimeSuggestion], _current_input: &str) {
    // This function is now obsolete - suggestions are handled in-place by the new system
}

/// Show available tools from MCP servers
pub fn show_available_tools(servers: &[(String, Vec<String>)]) {
    println!("\n{}╭─ Available MCP Tools ──────────────────────────╮{}", Colors::CYAN, Colors::RESET);

    for (server_id, tools) in servers {
        println!("{}│ {}{}{} ({} tools){}",
            Colors::CYAN,
            Colors::BLUE,
            server_id,
            Colors::DIM,
            tools.len(),
            Colors::RESET
        );

        for tool in tools.iter().take(3) {
            println!("{}│   • {}{}{}",
                Colors::CYAN,
                Colors::YELLOW,
                tool,
                Colors::RESET
            );
        }

        if tools.len() > 3 {
            println!("{}│   ... and {} more{}",
                Colors::CYAN,
                tools.len() - 3,
                Colors::RESET
            );
        }
    }

    println!("{}╰────────────────────────────────────────────────╯{}\n", Colors::CYAN, Colors::RESET);
}

/// Clear screen and reset cursor
pub fn clear_screen() {
    print!("\x1b[2J\x1b[H");
    io::stdout().flush().unwrap_or(());
}

/// Move cursor to position
pub fn move_cursor(x: u16, y: u16) {
    print!("\x1b[{};{}H", y + 1, x + 1);
    io::stdout().flush().unwrap_or(());
}

/// Save cursor position
pub fn save_cursor() {
    print!("\x1b[s");
    io::stdout().flush().unwrap_or(());
}

/// Restore cursor position
pub fn restore_cursor() {
    print!("\x1b[u");
    io::stdout().flush().unwrap_or(());
}
