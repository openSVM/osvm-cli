//! Input handling and state management for the chat interface

use super::responsive_layout::{
    format_colored_text, wrap_text_smart, ComponentArea, TerminalRenderer,
};
use super::{Colors, FuzzyMatcher, RealtimeSuggestion};
use anyhow::{anyhow, Result};
use crossterm::{cursor::MoveTo, execute, style::Color};
use log::debug;
use serde::{Deserialize, Serialize};
use std::io::{self, Read, Write};
use tokio::sync::mpsc;

/// Configuration for input handling behavior
#[derive(Debug, Clone)]
pub struct InputConfig {
    pub max_history_size: usize,
    pub debounce_ms: u64,
    pub max_suggestions: usize,
    pub fuzzy_threshold: f32,
    pub enable_fuzzy_matching: bool,
}

impl Default for InputConfig {
    fn default() -> Self {
        Self {
            max_history_size: 100,
            debounce_ms: 150,
            max_suggestions: 8,
            fuzzy_threshold: 0.4,
            enable_fuzzy_matching: true,
        }
    }
}

/// Input state management
#[derive(Debug)]
pub struct InputState {
    pub input: String,
    pub cursor_pos: usize,
    pub selected_suggestion: usize,
    pub command_history: Vec<String>,
    pub history_index: usize,
    pub suggestions: Vec<RealtimeSuggestion>,
    pub last_suggestion_time: std::time::Instant,
    pub config: InputConfig,
    pub original_before_sug: Option<String>,
    pub suggestions_suppressed: bool,
    pub sug_win_start: usize,
    pub win_height: usize,
    // Multiline support
    pub multiline_mode: bool,
    pub lines: Vec<String>,
    pub current_line: usize,
    pub line_cursor: usize,
    // Dynamic MCP tools cache (server_id, tool_name, description)
    pub mcp_tools_cache: Vec<(String, String, Option<String>)>,
}

impl InputState {
    /// Create new input state with default configuration
    pub fn new() -> Self {
        Self {
            input: String::new(),
            cursor_pos: 0,
            selected_suggestion: 0,
            command_history: Self::default_command_history(),
            history_index: 0,
            suggestions: Vec::new(),
            last_suggestion_time: std::time::Instant::now(),
            config: InputConfig::default(),
            original_before_sug: None,
            suggestions_suppressed: false,
            sug_win_start: 0,
            win_height: 6,
            // Multiline support
            multiline_mode: false,
            lines: vec![String::new()],
            current_line: 0,
            line_cursor: 0,
            // Dynamic MCP tools cache
            mcp_tools_cache: Vec::new(),
        }
    }

    /// Update the MCP tools cache from available tools
    /// Call this when MCP servers are initialized or tools change
    pub fn update_mcp_tools_cache(&mut self, tools: Vec<(String, String, Option<String>)>) {
        self.mcp_tools_cache = tools;
    }

    /// Default command history for demonstration
    fn default_command_history() -> Vec<String> {
        vec![
            "/balance".to_string(),
            "/transactions".to_string(),
            "/stake".to_string(),
            "/price".to_string(),
            "/network".to_string(),
        ]
    }

    /// Add command to history with size limit
    pub fn add_to_history(&mut self, command: String) {
        if !command.trim().is_empty() {
            self.command_history.push(command);
            if self.command_history.len() > self.config.max_history_size {
                self.command_history.remove(0);
            }
            self.history_index = self.command_history.len();
        }
    }

    /// Check if suggestions should be debounced
    pub fn should_update_suggestions(&mut self) -> bool {
        let elapsed = self.last_suggestion_time.elapsed();
        if elapsed.as_millis() > self.config.debounce_ms as u128 {
            self.last_suggestion_time = std::time::Instant::now();
            true
        } else {
            false
        }
    }

    /// Reset selection when input changes
    pub fn reset_selection(&mut self) {
        self.selected_suggestion = 0;
    }

    /// Insert character at cursor position (handles UTF-8 properly)
    pub fn insert_char(&mut self, ch: char) {
        // Convert cursor position from char index to byte index
        let byte_pos = self
            .input
            .char_indices()
            .nth(self.cursor_pos)
            .map(|(i, _)| i)
            .unwrap_or(self.input.len());

        self.input.insert(byte_pos, ch);
        self.cursor_pos += 1;
        self.reset_selection();
    }

    /// Delete character before cursor (handles UTF-8 properly)
    pub fn delete_before_cursor(&mut self) -> bool {
        if self.cursor_pos > 0 {
            // Find the byte position of the character to delete
            let mut char_indices: Vec<(usize, char)> = self.input.char_indices().collect();

            if self.cursor_pos <= char_indices.len() {
                let byte_pos = if self.cursor_pos == char_indices.len() {
                    // Deleting the last character
                    char_indices.get(self.cursor_pos - 1).map(|(i, _)| *i)
                } else {
                    // Deleting a character in the middle
                    char_indices.get(self.cursor_pos - 1).map(|(i, _)| *i)
                };

                if let Some(pos) = byte_pos {
                    // Find the char to remove
                    if let Some((_, ch)) = char_indices.get(self.cursor_pos - 1) {
                        let char_len = ch.len_utf8();
                        for _ in 0..char_len {
                            if pos < self.input.len() {
                                self.input.remove(pos);
                            }
                        }
                        self.cursor_pos -= 1;
                        self.reset_selection();
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Move cursor left
    pub fn move_cursor_left(&mut self) {
        if self.cursor_pos > 0 {
            self.cursor_pos -= 1;
        }
    }

    /// Move cursor right
    pub fn move_cursor_right(&mut self) {
        let char_count = self.input.chars().count();
        if self.cursor_pos < char_count {
            self.cursor_pos += 1;
        }
    }

    /// Move to start of line
    pub fn move_cursor_home(&mut self) {
        self.cursor_pos = 0;
    }

    /// Move to end of line
    pub fn move_cursor_end(&mut self) {
        self.cursor_pos = self.input.chars().count(); // Fix: use char count, not byte count
    }

    /// Clear current input
    pub fn clear(&mut self) {
        self.input.clear();
        self.cursor_pos = 0;
        self.suggestions.clear();
        self.selected_suggestion = 0;
        self.original_before_sug = None;
        self.suggestions_suppressed = false;
        self.sug_win_start = 0;
        // Clear multiline state
        self.multiline_mode = false;
        self.lines = vec![String::new()];
        self.current_line = 0;
        self.line_cursor = 0;
    }

    // =====================
    // Multiline Operations
    // =====================

    /// Toggle multiline mode
    pub fn toggle_multiline(&mut self) {
        if self.multiline_mode {
            // Exit multiline mode: join all lines with newlines
            self.input = self.lines.join("\n");
            self.cursor_pos = self.input.chars().count();
            self.multiline_mode = false;
        } else {
            // Enter multiline mode: split current input by newlines
            self.lines = if self.input.is_empty() {
                vec![String::new()]
            } else {
                self.input.lines().map(|s| s.to_string()).collect()
            };
            if self.lines.is_empty() {
                self.lines.push(String::new());
            }
            self.current_line = self.lines.len() - 1;
            self.line_cursor = self.lines[self.current_line].chars().count();
            self.multiline_mode = true;
        }
    }

    /// Insert a new line in multiline mode
    pub fn insert_newline(&mut self) {
        if !self.multiline_mode {
            self.toggle_multiline();
        }

        // Split current line at cursor
        let current = &self.lines[self.current_line];
        let byte_pos = current
            .char_indices()
            .nth(self.line_cursor)
            .map(|(i, _)| i)
            .unwrap_or(current.len());

        let (before, after) = current.split_at(byte_pos);
        let before_str = before.to_string();
        let after_str = after.to_string();

        self.lines[self.current_line] = before_str;
        self.lines.insert(self.current_line + 1, after_str);
        self.current_line += 1;
        self.line_cursor = 0;

        // Sync to main input
        self.sync_from_multiline();
    }

    /// Move cursor up in multiline mode
    pub fn multiline_up(&mut self) {
        if self.current_line > 0 {
            self.current_line -= 1;
            // Keep cursor position, but clamp to line length
            let line_len = self.lines[self.current_line].chars().count();
            self.line_cursor = self.line_cursor.min(line_len);
        }
    }

    /// Move cursor down in multiline mode
    pub fn multiline_down(&mut self) {
        if self.current_line < self.lines.len() - 1 {
            self.current_line += 1;
            let line_len = self.lines[self.current_line].chars().count();
            self.line_cursor = self.line_cursor.min(line_len);
        }
    }

    /// Insert character in multiline mode
    pub fn multiline_insert_char(&mut self, ch: char) {
        if self.lines.is_empty() {
            self.lines.push(String::new());
        }

        let line = &mut self.lines[self.current_line];
        let byte_pos = line
            .char_indices()
            .nth(self.line_cursor)
            .map(|(i, _)| i)
            .unwrap_or(line.len());

        line.insert(byte_pos, ch);
        self.line_cursor += 1;

        // Sync to main input
        self.sync_from_multiline();
    }

    /// Delete character before cursor in multiline mode
    pub fn multiline_backspace(&mut self) -> bool {
        if self.line_cursor > 0 {
            let line = &mut self.lines[self.current_line];
            let char_indices: Vec<(usize, char)> = line.char_indices().collect();

            if let Some((byte_pos, ch)) = char_indices.get(self.line_cursor - 1) {
                let char_len = ch.len_utf8();
                for _ in 0..char_len {
                    if *byte_pos < line.len() {
                        line.remove(*byte_pos);
                    }
                }
                self.line_cursor -= 1;
                self.sync_from_multiline();
                return true;
            }
        } else if self.current_line > 0 {
            // Merge with previous line
            let current_content = self.lines.remove(self.current_line);
            self.current_line -= 1;
            self.line_cursor = self.lines[self.current_line].chars().count();
            self.lines[self.current_line].push_str(&current_content);
            self.sync_from_multiline();
            return true;
        }
        false
    }

    /// Sync multiline content to main input field
    pub fn sync_from_multiline(&mut self) {
        self.input = self.lines.join("\n");
        // Calculate cursor position in the flat string
        let mut pos = 0;
        for (i, line) in self.lines.iter().enumerate() {
            if i < self.current_line {
                pos += line.chars().count() + 1; // +1 for newline
            } else if i == self.current_line {
                pos += self.line_cursor;
            }
        }
        self.cursor_pos = pos;
    }

    /// Get current content as final string (joining all lines)
    pub fn get_multiline_content(&self) -> String {
        if self.multiline_mode {
            self.lines.join("\n")
        } else {
            self.input.clone()
        }
    }

    /// Get line count
    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    /// Format multiline input for display
    pub fn format_multiline_display(&self) -> String {
        if !self.multiline_mode || self.lines.len() <= 1 {
            return self.input.clone();
        }

        self.lines
            .iter()
            .enumerate()
            .map(|(i, line)| {
                let indicator = if i == self.current_line { "▶" } else { " " };
                format!("{}{:2}│ {}", indicator, i + 1, line)
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Navigate history up
    pub fn history_up(&mut self) {
        if self.command_history.is_empty() {
            return;
        }

        if self.history_index == self.command_history.len() {
            // Save current input temporarily (not to permanent history)
            // We'll implement a separate temporary storage to avoid memory leak
            // For now, just clear to avoid the memory leak issue
        }

        if self.history_index > 0 {
            self.history_index -= 1;
            self.input = self.command_history[self.history_index].clone();
            self.cursor_pos = self.input.chars().count(); // Fix: use char count, not byte count
        }
    }

    /// Navigate history down
    pub fn history_down(&mut self) {
        if self.history_index < self.command_history.len() - 1 {
            self.history_index += 1;
            self.input = self.command_history[self.history_index].clone();
            self.cursor_pos = self.input.chars().count(); // Fix: use char count, not byte count
        } else if self.history_index == self.command_history.len() - 1 {
            self.history_index = self.command_history.len();
            self.clear();
        }
    }

    /// Apply selected suggestion
    pub fn apply_suggestion(&mut self) -> bool {
        if self.selected_suggestion < self.suggestions.len() {
            self.input = self.suggestions[self.selected_suggestion].text.clone();
            self.cursor_pos = self.input.chars().count(); // Fix: use char count, not byte count
            self.suggestions.clear();
            self.selected_suggestion = 0;
            true
        } else {
            false
        }
    }

    /// Navigate suggestions up
    pub fn suggestion_up(&mut self) {
        if !self.suggestions.is_empty() && self.selected_suggestion > 0 {
            self.selected_suggestion -= 1;
        }
    }

    /// Navigate suggestions down
    pub fn suggestion_down(&mut self) {
        if !self.suggestions.is_empty() && self.selected_suggestion < self.suggestions.len() - 1 {
            self.selected_suggestion += 1;
        }
    }

    /// Generate smart auto-suggestions based on current input and context
    pub fn generate_auto_suggestions(
        &mut self,
        context_history: &[String],
    ) -> Vec<RealtimeSuggestion> {
        if self.input.len() < 2 {
            return Vec::new();
        }

        let mut suggestions = Vec::new();
        let input_lower = self.input.to_lowercase();

        // Command suggestions
        let commands = vec![
            ("/help", "Show available commands", "command"),
            ("/clear", "Clear chat history", "command"),
            ("/status", "Show system status", "command"),
            ("/tools", "List available MCP tools", "command"),
            ("/context", "Show conversation context", "command"),
            ("/balance", "Check wallet balance", "crypto"),
            ("/transactions", "Show recent transactions", "crypto"),
            ("/stake", "Stake management", "crypto"),
            ("/price", "Check token prices", "crypto"),
            ("/network", "Network status", "system"),
        ];

        // Add command suggestions
        for (cmd, desc, category) in commands {
            if cmd.to_lowercase().contains(&input_lower) {
                suggestions.push(RealtimeSuggestion::new(
                    cmd.to_string(),
                    desc.to_string(),
                    category.to_string(),
                ));
            }
        }

        // MCP tool suggestions - use dynamic cache if available, fallback to defaults
        if !self.mcp_tools_cache.is_empty() {
            // Use dynamically loaded MCP tools
            for (server_id, tool_name, desc) in &self.mcp_tools_cache {
                let tool_path = format!("@{}/{}", server_id, tool_name);
                if tool_path.to_lowercase().contains(&input_lower)
                    || tool_name.to_lowercase().contains(&input_lower)
                    || input_lower.contains("@")
                {
                    suggestions.push(RealtimeSuggestion::new(
                        tool_path,
                        desc.clone().unwrap_or_else(|| format!("MCP tool from {}", server_id)),
                        "mcp".to_string(),
                    ));
                }
            }
        } else {
            // Fallback to hardcoded defaults when no MCP tools are cached
            let default_mcp_tools = vec![
                ("@solana/get_balance", "Get wallet balance via Solana MCP"),
                ("@solana/get_transactions", "Get transaction history"),
                ("@solana/get_network_status", "Check Solana network status"),
                ("@solana/stake_account", "Manage stake accounts"),
            ];

            for (tool, desc) in default_mcp_tools {
                if tool.to_lowercase().contains(&input_lower) || input_lower.contains("@") {
                    suggestions.push(RealtimeSuggestion::new(
                        tool.to_string(),
                        desc.to_string(),
                        "mcp".to_string(),
                    ));
                }
            }
        }

        // History-based suggestions
        let fuzzy_matcher = FuzzyMatcher::new(0.3);
        for cmd in &self.command_history {
            if cmd.to_lowercase().contains(&input_lower) && cmd != &self.input {
                suggestions.push(RealtimeSuggestion::new(
                    cmd.clone(),
                    "From history".to_string(),
                    "history".to_string(),
                ));
            }
        }

        // Context-aware suggestions based on recent conversation
        if let Some(last_message) = context_history.last() {
            if last_message.to_lowercase().contains("balance") && !input_lower.contains("balance") {
                suggestions.push(RealtimeSuggestion::new(
                    "/balance".to_string(),
                    "Check your wallet balance".to_string(),
                    "contextual".to_string(),
                ));
            }
            if last_message.to_lowercase().contains("transaction") && !input_lower.contains("trans")
            {
                suggestions.push(RealtimeSuggestion::new(
                    "/transactions".to_string(),
                    "View recent transactions".to_string(),
                    "contextual".to_string(),
                ));
            }
        }

        // Smart completions for partial commands
        if input_lower.starts_with("/") && input_lower.len() > 1 {
            let partial = &input_lower[1..];
            if "balance".starts_with(partial) {
                suggestions.push(RealtimeSuggestion::new(
                    "/balance".to_string(),
                    "Check wallet balance".to_string(),
                    "completion".to_string(),
                ));
            }
            if "transactions".starts_with(partial) {
                suggestions.push(RealtimeSuggestion::new(
                    "/transactions".to_string(),
                    "Show transaction history".to_string(),
                    "completion".to_string(),
                ));
            }
        }

        // Sort by relevance and limit
        suggestions.sort_by(|a, b| {
            // Prioritize exact matches and shorter suggestions
            let a_exact = a.text.to_lowercase() == input_lower;
            let b_exact = b.text.to_lowercase() == input_lower;

            if a_exact && !b_exact {
                std::cmp::Ordering::Less
            } else if !a_exact && b_exact {
                std::cmp::Ordering::Greater
            } else {
                a.text.len().cmp(&b.text.len())
            }
        });

        suggestions.truncate(self.config.max_suggestions);
        self.suggestions = suggestions.clone();
        suggestions
    }

    /// Update suggestions without disturbing terminal state
    pub fn update_suggestions_in_place(
        &mut self,
        renderer: &mut TerminalRenderer,
        context_history: &[String],
    ) -> Result<()> {
        let suggestions = self.generate_auto_suggestions(context_history);

        if suggestions.is_empty() {
            // Clear suggestions area
            let suggestion_area = renderer.areas().suggestions.clone();
            renderer.render_component_in_place(&suggestion_area, |stdout, area| {
                // Just clear the area
                Ok(())
            })?;
            return Ok(());
        }

        let suggestion_area = renderer.areas().suggestions.clone();

        renderer.render_component_in_place(&suggestion_area, |stdout, area| {
            if area.height < 2 {
                return Ok(()); // Not enough space
            }

            // Header
            write!(
                stdout,
                "{}╭─ Suggestions (↑/↓ to navigate, Tab to select) ─╮{}",
                Colors::DIM,
                Colors::RESET
            )?;

            let max_suggestions = (area.height.saturating_sub(2) as usize).min(suggestions.len());

            for (i, suggestion) in suggestions.iter().enumerate().take(max_suggestions) {
                if i + 1 >= area.height as usize {
                    break;
                }

                execute!(stdout, MoveTo(area.x, area.y + i as u16 + 1))?;

                let selector = if i == self.selected_suggestion {
                    "▶"
                } else {
                    " "
                };
                let icon = match suggestion.category.as_str() {
                    "command" => "⌘",
                    "mcp" => "⚙",
                    "crypto" => "₿",
                    "history" => "↺",
                    "contextual" => "◉",
                    "completion" => "→",
                    _ => "•",
                };

                let color = if i == self.selected_suggestion {
                    Color::Yellow
                } else {
                    Color::Grey
                };

                // Truncate text to fit terminal width - UTF-8 safe
                let available_width = area.width.saturating_sub(8); // Account for prefix and margins
                let display_text = if suggestion.text.chars().count() > available_width as usize {
                    let truncate_at = available_width.saturating_sub(3) as usize;
                    let truncated: String = suggestion.text.chars().take(truncate_at).collect();
                    format!("{}...", truncated)
                } else {
                    suggestion.text.clone()
                };

                let display_desc = if suggestion.description.chars().count() > 30 {
                    let truncated: String = suggestion.description.chars().take(27).collect();
                    format!("{}...", truncated)
                } else {
                    suggestion.description.clone()
                };

                write!(
                    stdout,
                    "{}│{} {} {} - {}{}",
                    Colors::DIM,
                    selector,
                    icon,
                    format_colored_text(&display_text, color),
                    format_colored_text(&display_desc, Color::White),
                    Colors::RESET
                )?;
            }

            // Footer
            if max_suggestions < suggestions.len() {
                let remaining = suggestions.len() - max_suggestions;
                execute!(stdout, MoveTo(area.x, area.y + max_suggestions as u16 + 1))?;
                write!(
                    stdout,
                    "{}│ ... {} more suggestions{}",
                    Colors::DIM,
                    remaining,
                    Colors::RESET
                )?;
            }

            // Bottom border (if space)
            if area.height > max_suggestions as u16 + 2 {
                execute!(stdout, MoveTo(area.x, area.y + area.height - 1))?;
                write!(
                    stdout,
                    "{}╰{}─╯{}",
                    Colors::DIM,
                    "─".repeat(area.width.saturating_sub(3) as usize),
                    Colors::RESET
                )?;
            }

            Ok(())
        })?;

        Ok(())
    }
}

/// Input character classification
#[derive(Debug)]
pub enum InputChar {
    Enter,
    Backspace,
    CtrlC,
    CtrlT,
    CtrlL,
    CtrlM,    // Toggle multiline mode
    CtrlEnter, // Insert newline in multiline mode
    Tab,
    Escape,
    Arrow(ArrowKey),
    Regular(char),
    Mouse,  // Mouse event to be ignored
    Resize(u16, u16), // Terminal resize event (new_cols, new_rows)
    Unknown,
}

/// Arrow key directions
#[derive(Debug)]
pub enum ArrowKey {
    Up,
    Down,
    Left,
    Right,
}

/// Input handler for processing keyboard input
pub struct InputHandler;

impl InputHandler {
    /// Read and classify a single character from input
    pub fn read_character() -> Result<InputChar> {
        let mut buffer = [0; 1];
        if std::io::stdin().read(&mut buffer)? != 1 {
            return Ok(InputChar::Unknown);
        }

        match buffer[0] {
            b'\n' | b'\r' => Ok(InputChar::Enter),
            0x7f | 0x08 => Ok(InputChar::Backspace),
            0x03 => Ok(InputChar::CtrlC),
            0x14 => Ok(InputChar::CtrlT), // Ctrl+T
            0x0c => Ok(InputChar::CtrlL), // Ctrl+L
            0x0e => Ok(InputChar::CtrlEnter), // Ctrl+N for newline insert in multiline
            0x15 => Ok(InputChar::CtrlM), // Ctrl+U for multiline toggle
            b'\t' => Ok(InputChar::Tab),
            0x1b => Self::read_escape_sequence(),
            ch if ch >= 0x20 && ch < 0x7f => Ok(InputChar::Regular(ch as char)),
            // Filter out common mouse event byte sequences
            0x00..=0x1f => Ok(InputChar::Unknown), // Control characters
            0x80..=0xff => Ok(InputChar::Unknown), // Extended ASCII often used in mouse events
            _ => Ok(InputChar::Unknown),
        }
    }

    /// Read escape sequences (arrow keys, etc.)
    fn read_escape_sequence() -> Result<InputChar> {
        use std::io::{ErrorKind, Read};
        use std::time::Duration;

        let mut buffer = [0; 1];

        // Try to read the next character with a timeout
        // If it's not available quickly, it's just an ESC key
        let mut stdin = std::io::stdin();

        // First, check if there's a '[' following the ESC
        match stdin.read_exact(&mut buffer) {
            Ok(_) if buffer[0] == b'[' => {
                // Now read the direction character
                match stdin.read_exact(&mut buffer) {
                    Ok(_) => match buffer[0] {
                        b'A' => Ok(InputChar::Arrow(ArrowKey::Up)),
                        b'B' => Ok(InputChar::Arrow(ArrowKey::Down)),
                        b'C' => Ok(InputChar::Arrow(ArrowKey::Right)),
                        b'D' => Ok(InputChar::Arrow(ArrowKey::Left)),
                        b'M' | b'<' => {
                            // Mouse event sequence detected, consume and ignore it
                            // Read remaining mouse event bytes (typically 3 more bytes)
                            let mut discard = [0; 10];
                            let _ = stdin.read(&mut discard);
                            Ok(InputChar::Mouse)
                        }
                        b'0'..=b'9' => {
                            // Possible extended sequence (like mouse events)
                            // Consume the rest of the sequence
                            let mut discard = [0; 10];
                            let _ = stdin.read(&mut discard);
                            Ok(InputChar::Unknown)
                        }
                        _ => Ok(InputChar::Escape),
                    },
                    Err(_) => Ok(InputChar::Escape), // Incomplete sequence
                }
            }
            Ok(_) => {
                // We read a character that wasn't '[', put it back conceptually
                // In practice, we'll just treat this as ESC + that character
                Ok(InputChar::Escape)
            }
            Err(_) => Ok(InputChar::Escape), // Just ESC key
        }
    }

    /// Process input character and update state with suggestion updates
    pub async fn process_input_with_suggestions(
        state: &mut InputState,
        ch: InputChar,
        renderer: &mut TerminalRenderer,
        context_history: &[String],
    ) -> Result<Option<String>> {
        let mut should_update_suggestions = false;

        let result = match ch {
            InputChar::Enter => {
                // If a suggestion is selected, apply it instead of submitting
                if !state.suggestions.is_empty()
                    && state.selected_suggestion < state.suggestions.len()
                {
                    state.apply_suggestion();
                    should_update_suggestions = true;
                    Ok(None)
                } else {
                    // Get content (handles multiline mode properly)
                    let result = state.get_multiline_content();
                    state.add_to_history(result.clone());
                    state.clear();
                    should_update_suggestions = true;
                    Ok(Some(result))
                }
            }

            InputChar::Backspace => {
                if state.multiline_mode {
                    if state.multiline_backspace() {
                        should_update_suggestions = true;
                    }
                } else if state.delete_before_cursor() {
                    should_update_suggestions = true;
                }
                Ok(None)
            }

            InputChar::Tab => {
                if state.apply_suggestion() {
                    should_update_suggestions = true;
                }
                Ok(None)
            }

            InputChar::Arrow(ArrowKey::Up) => {
                if state.multiline_mode && state.current_line > 0 {
                    // Navigate up in multiline mode
                    state.multiline_up();
                } else if state.suggestions.is_empty() {
                    state.history_up();
                    should_update_suggestions = true;
                } else {
                    state.suggestion_up();
                    // Only update the suggestion display, not regenerate
                    state.update_suggestions_in_place(renderer, context_history)?;
                }
                Ok(None)
            }

            InputChar::Arrow(ArrowKey::Down) => {
                if state.multiline_mode && state.current_line < state.lines.len() - 1 {
                    // Navigate down in multiline mode
                    state.multiline_down();
                } else if state.suggestions.is_empty() {
                    state.history_down();
                    should_update_suggestions = true;
                } else {
                    state.suggestion_down();
                    // Only update the suggestion display, not regenerate
                    state.update_suggestions_in_place(renderer, context_history)?;
                }
                Ok(None)
            }

            InputChar::Arrow(ArrowKey::Left) => {
                if state.multiline_mode {
                    if state.line_cursor > 0 {
                        state.line_cursor -= 1;
                        state.sync_from_multiline();
                    }
                } else {
                    state.move_cursor_left();
                }
                Ok(None)
            }

            InputChar::Arrow(ArrowKey::Right) => {
                if state.multiline_mode {
                    let line_len = state.lines[state.current_line].chars().count();
                    if state.line_cursor < line_len {
                        state.line_cursor += 1;
                        state.sync_from_multiline();
                    }
                } else {
                    state.move_cursor_right();
                }
                Ok(None)
            }

            InputChar::Regular(c) => {
                if state.multiline_mode {
                    state.multiline_insert_char(c);
                } else {
                    state.insert_char(c);
                }
                should_update_suggestions = state.should_update_suggestions();
                Ok(None)
            }

            InputChar::Escape => {
                state.clear();
                should_update_suggestions = true;
                Ok(None)
            }

            InputChar::CtrlC => Err(anyhow!("User interrupted")),

            InputChar::CtrlM => {
                // Toggle multiline mode with Ctrl+U
                state.toggle_multiline();
                should_update_suggestions = true;
                Ok(None)
            }

            InputChar::CtrlEnter => {
                // Insert newline in multiline mode with Ctrl+N
                if state.multiline_mode {
                    state.insert_newline();
                    should_update_suggestions = true;
                } else {
                    // If not in multiline mode, enter it and add a newline
                    state.toggle_multiline();
                    state.insert_newline();
                    should_update_suggestions = true;
                }
                Ok(None)
            }

            InputChar::Mouse => {
                // Silently ignore mouse events to preserve right-click functionality
                Ok(None)
            }

            InputChar::Resize(_cols, _rows) => {
                // Terminal resize event - caller should handle redraw
                // We mark suggestions for update to ensure proper redraw
                should_update_suggestions = true;
                Ok(None)
            }

            _ => Ok(None),
        };

        // Update suggestions if needed
        if should_update_suggestions {
            state.update_suggestions_in_place(renderer, context_history)?;
        }

        result
    }

    /// Legacy process input method for backward compatibility
    pub async fn process_input(
        state: &mut InputState,
        ch: InputChar,
        suggestion_tx: &mpsc::UnboundedSender<String>,
    ) -> Result<Option<String>> {
        match ch {
            InputChar::Enter => {
                let result = state.input.clone();
                state.add_to_history(result.clone());
                state.clear();
                Ok(Some(result))
            }

            InputChar::Backspace => {
                if state.delete_before_cursor() {
                    let _ = suggestion_tx.send(state.input.clone());
                }
                Ok(None)
            }

            InputChar::Tab => {
                state.apply_suggestion();
                Ok(None)
            }

            InputChar::Arrow(ArrowKey::Up) => {
                if state.suggestions.is_empty() {
                    state.history_up();
                } else {
                    state.suggestion_up();
                }
                Ok(None)
            }

            InputChar::Arrow(ArrowKey::Down) => {
                if state.suggestions.is_empty() {
                    state.history_down();
                } else {
                    state.suggestion_down();
                }
                Ok(None)
            }

            InputChar::Arrow(ArrowKey::Left) => {
                state.move_cursor_left();
                Ok(None)
            }

            InputChar::Arrow(ArrowKey::Right) => {
                state.move_cursor_right();
                Ok(None)
            }

            InputChar::Regular(c) => {
                state.insert_char(c);
                if state.should_update_suggestions() {
                    let _ = suggestion_tx.send(state.input.clone());
                }
                Ok(None)
            }

            InputChar::Escape => {
                state.clear();
                Ok(None)
            }

            InputChar::CtrlC => Err(anyhow!("User interrupted")),

            InputChar::Mouse => {
                // Silently ignore mouse events
                Ok(None)
            }

            InputChar::Resize(_cols, _rows) => {
                // Terminal resize event - legacy handler doesn't do layout updates
                // Caller should handle redraw if needed
                Ok(None)
            }

            _ => Ok(None),
        }
    }
}

/// Enable raw mode for terminal input
pub fn enable_raw_mode() -> Result<()> {
    use crossterm::terminal;
    terminal::enable_raw_mode()?;
    Ok(())
}

/// Disable raw mode for terminal input
pub fn disable_raw_mode() -> Result<()> {
    use crossterm::terminal;
    terminal::disable_raw_mode()?;
    Ok(())
}
