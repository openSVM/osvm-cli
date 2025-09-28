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

/// Clear suggestions display area
pub fn clear_suggestions_display() {
    // Clear up to 10 lines for suggestions
    for _ in 0..10 {
        println!("\x1b[K");
    }
    // Move cursor back up
    print!("\x1b[10A");
    io::stdout().flush().unwrap_or(());
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

/// Show navigable suggestions with selection indicator
pub fn show_navigable_suggestions(suggestions: &[RealtimeSuggestion], selected_index: usize) {
    if suggestions.is_empty() {
        return;
    }

    println!("\n{}╭─ Suggestions (↑/↓ to navigate, Tab to select) ─╮{}", Colors::DIM, Colors::RESET);

    for (i, suggestion) in suggestions.iter().enumerate().take(5) {
        let selector = if i == selected_index { "▶" } else { " " };
        let highlight_color = if i == selected_index {
            Colors::YELLOW
        } else {
            Colors::GRAY
        };

        // Highlight matched characters
        let highlighted_text = highlight_fuzzy_match(&suggestion.text, &suggestion.matched_indices, highlight_color);

        println!("{}│{} {} {} - {}{}{}",
            Colors::DIM,
            selector,
            highlight_color,
            highlighted_text,
            suggestion.description,
            Colors::DIM,
            Colors::RESET
        );
    }

    println!("{}╰──────────────────────────────────────────────────╯{}", Colors::DIM, Colors::RESET);
}

/// Show real-time suggestions with fuzzy matching highlights
pub fn show_realtime_suggestions(suggestions: &[RealtimeSuggestion], current_input: &str) {
    show_realtime_suggestions_fixed(suggestions, current_input);
}

/// Fixed version of show real-time suggestions
pub fn show_realtime_suggestions_fixed(suggestions: &[RealtimeSuggestion], current_input: &str) {
    if suggestions.is_empty() {
        return;
    }

    // Use a mutex to ensure atomic terminal operations
    use std::sync::Mutex;
    static TERMINAL_LOCK: Mutex<()> = Mutex::new(());

    let _guard = TERMINAL_LOCK.lock().unwrap_or_else(|e| e.into_inner());

    // Build the entire suggestion block in memory first
    let mut output = String::new();

    // Save cursor position
    output.push_str("\x1b[s");

    // Move down from current line and clear suggestion area
    output.push_str("\r\n");

    // Clear exactly 6 lines for suggestions
    for i in 0..6 {
        output.push_str("\x1b[K");
        if i < 5 {
            output.push_str("\n");
        }
    }

    // Move back up to start of suggestion area
    output.push_str("\x1b[5A");

    // Add suggestions header
    output.push_str(&format!("{}  ↓ Suggestions (Tab to complete):{}\n", Colors::DIM, Colors::RESET));

    // Show up to 5 suggestions
    for suggestion in suggestions.iter().take(5) {
        let category_icon = match suggestion.category.as_str() {
            "command" => "⌘",
            "tool" => "⚙",
            "history" => "↺",
            "context" => "◉",
            _ => "•",
        };

        // Highlight matched characters
        let highlighted_text = highlight_fuzzy_match(&suggestion.text, &suggestion.matched_indices, Colors::CYAN);

        output.push_str(&format!("  {} {} {} - {}{}{}\n",
            Colors::GRAY,
            category_icon,
            highlighted_text,
            Colors::DIM,
            suggestion.description,
            Colors::RESET
        ));
    }

    // Restore cursor position
    output.push_str("\x1b[u");

    // Write everything at once for atomic update
    print!("{}", output);
    io::stdout().flush().unwrap_or(());
}

/// Highlight matched characters in fuzzy search results
fn highlight_fuzzy_match(text: &str, matched_indices: &[usize], base_color: &str) -> String {
    if matched_indices.is_empty() {
        return format!("{}{}{}", base_color, text, Colors::RESET);
    }

    let mut result = String::new();
    let chars: Vec<char> = text.chars().collect();

    for (i, ch) in chars.iter().enumerate() {
        if matched_indices.contains(&i) {
            result.push_str(&format!("{}{}{}", Colors::BOLD, ch, base_color));
        } else {
            result.push(*ch);
        }
    }

    result
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