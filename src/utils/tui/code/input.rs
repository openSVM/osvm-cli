//! Input handling for the code assistant TUI
//!
//! Handles keyboard events and dispatches them to the appropriate handlers.

use crate::utils::tui::code::app::{AppStatus, CodeApp, CodeFocus};
use crate::utils::tui::code::permissions::ApprovalResponse;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};

/// Result of handling input
pub enum InputResult {
    /// Continue running
    Continue,
    /// Quit the application
    Quit,
    /// Submit user input for processing
    Submit(String),
    /// Approval decision made
    Approval(ApprovalResponse),
    /// Execute a slash command
    Command(String),
    /// Refresh/redraw
    Refresh,
}

impl CodeApp {
    /// Handle a terminal event
    pub fn handle_event(&mut self, event: Event) -> InputResult {
        match event {
            Event::Key(key) => self.handle_key(key),
            Event::Resize(_, _) => InputResult::Refresh,
            _ => InputResult::Continue,
        }
    }

    /// Handle a key event
    fn handle_key(&mut self, key: KeyEvent) -> InputResult {
        // Handle approval modal first
        if self.approval_modal.is_some() {
            return self.handle_approval_key(key);
        }

        // Handle global shortcuts
        match (key.modifiers, key.code) {
            // Ctrl+C - Quit
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => return InputResult::Quit,

            // Ctrl+L - Clear screen
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => {
                self.clear_conversation();
                return InputResult::Refresh;
            }

            // Tab - Toggle focus
            (KeyModifiers::NONE, KeyCode::Tab) | (KeyModifiers::SHIFT, KeyCode::BackTab) => {
                self.toggle_focus();
                return InputResult::Continue;
            }

            // Escape - Focus input or clear input
            (_, KeyCode::Esc) => {
                if self.focus == CodeFocus::Messages {
                    self.focus = CodeFocus::Input;
                } else if !self.input.is_empty() {
                    self.input.clear();
                    self.input_cursor = 0;
                }
                return InputResult::Continue;
            }

            _ => {}
        }

        // Handle focus-specific keys
        match self.focus {
            CodeFocus::Input => self.handle_input_key(key),
            CodeFocus::Messages => self.handle_messages_key(key),
        }
    }

    /// Handle key when approval modal is active
    fn handle_approval_key(&mut self, key: KeyEvent) -> InputResult {
        if let Some(modal) = &mut self.approval_modal {
            if let Some(response) = modal.handle_key(key.code) {
                return InputResult::Approval(response);
            }
        }
        InputResult::Continue
    }

    /// Handle key in input mode
    fn handle_input_key(&mut self, key: KeyEvent) -> InputResult {
        match (key.modifiers, key.code) {
            // Enter - Submit input
            (KeyModifiers::NONE, KeyCode::Enter) | (KeyModifiers::SHIFT, KeyCode::Enter) => {
                if let Some(input) = self.submit_input() {
                    // Check for commands
                    if input.starts_with('/') {
                        return InputResult::Command(input);
                    }
                    return InputResult::Submit(input);
                }
                InputResult::Continue
            }

            // Character input
            (KeyModifiers::NONE | KeyModifiers::SHIFT, KeyCode::Char(c)) => {
                self.handle_char(c);
                InputResult::Continue
            }

            // Backspace
            (_, KeyCode::Backspace) => {
                self.handle_backspace();
                InputResult::Continue
            }

            // Delete
            (_, KeyCode::Delete) => {
                self.handle_delete();
                InputResult::Continue
            }

            // Cursor movement
            (KeyModifiers::NONE, KeyCode::Left) => {
                self.cursor_left();
                InputResult::Continue
            }
            (KeyModifiers::NONE, KeyCode::Right) => {
                self.cursor_right();
                InputResult::Continue
            }
            (KeyModifiers::NONE, KeyCode::Home) | (KeyModifiers::CONTROL, KeyCode::Char('a')) => {
                self.cursor_home();
                InputResult::Continue
            }
            (KeyModifiers::NONE, KeyCode::End) | (KeyModifiers::CONTROL, KeyCode::Char('e')) => {
                self.cursor_end();
                InputResult::Continue
            }

            // Word-based movement
            (KeyModifiers::CONTROL, KeyCode::Left) => {
                self.cursor_word_left();
                InputResult::Continue
            }
            (KeyModifiers::CONTROL, KeyCode::Right) => {
                self.cursor_word_right();
                InputResult::Continue
            }

            // Delete word
            (KeyModifiers::CONTROL, KeyCode::Backspace) | (KeyModifiers::CONTROL, KeyCode::Char('w')) => {
                self.delete_word_back();
                InputResult::Continue
            }

            // Clear line
            (KeyModifiers::CONTROL, KeyCode::Char('u')) => {
                self.input.clear();
                self.input_cursor = 0;
                InputResult::Continue
            }

            // Up/Down - scroll messages when input is empty
            (KeyModifiers::NONE, KeyCode::Up) if self.input.is_empty() => {
                self.focus = CodeFocus::Messages;
                self.scroll_up();
                InputResult::Continue
            }
            (KeyModifiers::NONE, KeyCode::Down) if self.input.is_empty() => {
                self.focus = CodeFocus::Messages;
                self.scroll_down();
                InputResult::Continue
            }

            _ => InputResult::Continue,
        }
    }

    /// Handle key in messages mode
    fn handle_messages_key(&mut self, key: KeyEvent) -> InputResult {
        match key.code {
            KeyCode::Up | KeyCode::Char('k') => {
                self.scroll_up();
                InputResult::Continue
            }
            KeyCode::Down | KeyCode::Char('j') => {
                self.scroll_down();
                InputResult::Continue
            }
            KeyCode::PageUp => {
                for _ in 0..10 {
                    self.scroll_up();
                }
                InputResult::Continue
            }
            KeyCode::PageDown => {
                for _ in 0..10 {
                    self.scroll_down();
                }
                InputResult::Continue
            }
            KeyCode::Home | KeyCode::Char('g') => {
                self.scroll_offset = 0;
                InputResult::Continue
            }
            KeyCode::End | KeyCode::Char('G') => {
                self.scroll_to_bottom();
                InputResult::Continue
            }
            // Any character input switches to input mode
            KeyCode::Char(c) if c.is_alphanumeric() || c == '/' => {
                self.focus = CodeFocus::Input;
                self.handle_char(c);
                InputResult::Continue
            }
            KeyCode::Enter => {
                self.focus = CodeFocus::Input;
                InputResult::Continue
            }
            _ => InputResult::Continue,
        }
    }

    /// Move cursor one word left
    fn cursor_word_left(&mut self) {
        if self.input_cursor == 0 {
            return;
        }

        // Skip whitespace
        while self.input_cursor > 0 {
            let c = self.input.chars().nth(self.input_cursor - 1);
            if c.map(|c| !c.is_whitespace()).unwrap_or(false) {
                break;
            }
            self.input_cursor -= 1;
        }

        // Move to start of word
        while self.input_cursor > 0 {
            let c = self.input.chars().nth(self.input_cursor - 1);
            if c.map(|c| c.is_whitespace()).unwrap_or(true) {
                break;
            }
            self.input_cursor -= 1;
        }
    }

    /// Move cursor one word right
    fn cursor_word_right(&mut self) {
        let len = self.input.len();
        if self.input_cursor >= len {
            return;
        }

        // Skip current word
        while self.input_cursor < len {
            let c = self.input.chars().nth(self.input_cursor);
            if c.map(|c| c.is_whitespace()).unwrap_or(true) {
                break;
            }
            self.input_cursor += 1;
        }

        // Skip whitespace
        while self.input_cursor < len {
            let c = self.input.chars().nth(self.input_cursor);
            if c.map(|c| !c.is_whitespace()).unwrap_or(false) {
                break;
            }
            self.input_cursor += 1;
        }
    }

    /// Delete word backwards
    fn delete_word_back(&mut self) {
        if self.input_cursor == 0 {
            return;
        }

        let end = self.input_cursor;

        // Skip whitespace
        while self.input_cursor > 0 {
            let c = self.input.chars().nth(self.input_cursor - 1);
            if c.map(|c| !c.is_whitespace()).unwrap_or(false) {
                break;
            }
            self.input_cursor -= 1;
        }

        // Delete word
        while self.input_cursor > 0 {
            let c = self.input.chars().nth(self.input_cursor - 1);
            if c.map(|c| c.is_whitespace()).unwrap_or(true) {
                break;
            }
            self.input_cursor -= 1;
        }

        // Remove the characters
        self.input.drain(self.input_cursor..end);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_word_navigation() {
        let mut app = CodeApp::new(PathBuf::from("/tmp"), false, false, false);
        app.input = "hello world test".to_string();
        app.input_cursor = 16; // End

        app.cursor_word_left();
        assert_eq!(app.input_cursor, 12); // Start of "test"

        app.cursor_word_left();
        assert_eq!(app.input_cursor, 6); // Start of "world"

        app.cursor_word_right();
        assert_eq!(app.input_cursor, 12); // Start of "test"
    }

    #[test]
    fn test_delete_word() {
        let mut app = CodeApp::new(PathBuf::from("/tmp"), false, false, false);
        app.input = "hello world".to_string();
        app.input_cursor = 11; // End

        app.delete_word_back();
        assert_eq!(app.input, "hello ");
        assert_eq!(app.input_cursor, 6);
    }

    #[test]
    fn test_focus_toggle() {
        let mut app = CodeApp::new(PathBuf::from("/tmp"), false, false, false);
        assert_eq!(app.focus, CodeFocus::Input);

        app.toggle_focus();
        assert_eq!(app.focus, CodeFocus::Messages);

        app.toggle_focus();
        assert_eq!(app.focus, CodeFocus::Input);
    }
}
