//! Input handling and state management for the chat interface

use super::{Colors, RealtimeSuggestion, FuzzyMatcher};
use anyhow::{Result, anyhow};
use std::io::{self, Write, Read};
use tokio::sync::mpsc;
use log::debug;
use serde::{Serialize, Deserialize};

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
        }
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
        let byte_pos = self.input
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
        if self.cursor_pos < self.input.len() {
            self.cursor_pos += 1;
        }
    }

    /// Move to start of line
    pub fn move_cursor_home(&mut self) {
        self.cursor_pos = 0;
    }

    /// Move to end of line
    pub fn move_cursor_end(&mut self) {
        self.cursor_pos = self.input.len();
    }

    /// Clear current input
    pub fn clear(&mut self) {
        self.input.clear();
        self.cursor_pos = 0;
        self.suggestions.clear();
        self.selected_suggestion = 0;
    }

    /// Navigate history up
    pub fn history_up(&mut self) {
        if self.command_history.is_empty() {
            return;
        }

        if self.history_index == self.command_history.len() {
            // Save current input before going into history
            // Store it temporarily so we can return to it
            // We'll use a special marker to track this
            if !self.input.is_empty() {
                // Don't add to permanent history, just remember it
                self.command_history.push(self.input.clone());
            }
        }

        if self.history_index > 0 {
            self.history_index -= 1;
            self.input = self.command_history[self.history_index].clone();
            self.cursor_pos = self.input.len();
        }
    }

    /// Navigate history down
    pub fn history_down(&mut self) {
        if self.history_index < self.command_history.len() - 1 {
            self.history_index += 1;
            self.input = self.command_history[self.history_index].clone();
            self.cursor_pos = self.input.len();
        } else if self.history_index == self.command_history.len() - 1 {
            self.history_index = self.command_history.len();
            self.clear();
        }
    }

    /// Apply selected suggestion
    pub fn apply_suggestion(&mut self) -> bool {
        if self.selected_suggestion < self.suggestions.len() {
            self.input = self.suggestions[self.selected_suggestion].text.clone();
            self.cursor_pos = self.input.len();
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
}

/// Input character classification
#[derive(Debug)]
pub enum InputChar {
    Enter,
    Backspace,
    CtrlC,
    CtrlT,
    CtrlL,
    Tab,
    Escape,
    Arrow(ArrowKey),
    Regular(char),
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
            0x14 => Ok(InputChar::CtrlT),
            0x0c => Ok(InputChar::CtrlL),
            b'\t' => Ok(InputChar::Tab),
            0x1b => Self::read_escape_sequence(),
            ch if ch >= 0x20 && ch < 0x7f => Ok(InputChar::Regular(ch as char)),
            _ => Ok(InputChar::Unknown),
        }
    }

    /// Read escape sequences (arrow keys, etc.)
    fn read_escape_sequence() -> Result<InputChar> {
        use std::time::Duration;
        use std::io::ErrorKind;

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

    /// Process input character and update state
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

            InputChar::CtrlC => {
                Err(anyhow!("User interrupted"))
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