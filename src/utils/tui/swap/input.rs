//! Input handling for swap TUI
//!
//! Handles keyboard events for the swap interface.

use crate::utils::tui::swap::app::{SwapApp, SwapFocus, SwapStatus};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

impl SwapApp {
    /// Handle a keyboard event
    pub fn handle_key(&mut self, key: KeyEvent) -> InputResult {
        // Handle Ctrl+C globally
        if key.modifiers.contains(KeyModifiers::CONTROL) && key.code == KeyCode::Char('c') {
            self.should_quit = true;
            return InputResult::Quit;
        }

        // Handle modal states first
        if self.search_modal_open {
            return self.handle_search_key(key);
        }

        if matches!(self.swap_status, Some(SwapStatus::Confirming)) {
            return self.handle_confirmation_key(key);
        }

        // Handle swap status states
        if let Some(status) = &self.swap_status {
            match status {
                SwapStatus::Success(_) | SwapStatus::Failed(_) => {
                    // Any key clears the status
                    self.swap_status = None;
                    return InputResult::Continue;
                }
                _ => {
                    // Can't interact during swap
                    return InputResult::Continue;
                }
            }
        }

        // Normal mode key handling
        match key.code {
            KeyCode::Char('q') | KeyCode::Esc => {
                self.should_quit = true;
                InputResult::Quit
            }
            KeyCode::Tab => {
                self.next_focus();
                InputResult::Continue
            }
            KeyCode::BackTab => {
                self.prev_focus();
                InputResult::Continue
            }
            KeyCode::Char('s') => {
                self.switch_tokens();
                InputResult::NeedsQuote
            }
            KeyCode::Char('r') => InputResult::NeedsQuote,
            KeyCode::Char('m') => {
                self.set_max_amount();
                InputResult::NeedsQuote
            }
            KeyCode::Enter => {
                match self.focus {
                    SwapFocus::FromToken | SwapFocus::ToToken => {
                        self.open_search();
                        InputResult::Continue
                    }
                    SwapFocus::SwapButton => {
                        if self.can_swap() {
                            self.swap_status = Some(SwapStatus::Confirming);
                        }
                        InputResult::Continue
                    }
                    _ => InputResult::Continue,
                }
            }
            KeyCode::Char(c) if self.focus == SwapFocus::Amount => {
                if c.is_ascii_digit() || (c == '.' && !self.amount_input.contains('.')) {
                    self.amount_input.push(c);
                    self.parse_amount();
                    self.invalidate_quote();
                    InputResult::NeedsQuote
                } else {
                    InputResult::Continue
                }
            }
            KeyCode::Backspace if self.focus == SwapFocus::Amount => {
                self.amount_input.pop();
                self.parse_amount();
                self.invalidate_quote();
                if self.amount_parsed.is_some() {
                    InputResult::NeedsQuote
                } else {
                    InputResult::Continue
                }
            }
            KeyCode::Up => {
                self.prev_focus();
                InputResult::Continue
            }
            KeyCode::Down => {
                self.next_focus();
                InputResult::Continue
            }
            KeyCode::Left | KeyCode::Right => {
                // Navigate within focused element
                if matches!(self.focus, SwapFocus::FromToken | SwapFocus::ToToken) {
                    self.open_search();
                }
                InputResult::Continue
            }
            _ => InputResult::Continue,
        }
    }

    /// Handle keyboard events in search modal
    fn handle_search_key(&mut self, key: KeyEvent) -> InputResult {
        match key.code {
            KeyCode::Esc => {
                self.close_search();
                InputResult::Continue
            }
            KeyCode::Enter => {
                self.select_search_result();
                InputResult::NeedsQuote
            }
            KeyCode::Up => {
                if self.selected_search_idx > 0 {
                    self.selected_search_idx -= 1;
                }
                InputResult::Continue
            }
            KeyCode::Down => {
                if self.selected_search_idx < self.token_search_results.len().saturating_sub(1) {
                    self.selected_search_idx += 1;
                }
                InputResult::Continue
            }
            KeyCode::Char(c) => {
                self.token_search.push(c);
                self.update_search();
                InputResult::Continue
            }
            KeyCode::Backspace => {
                self.token_search.pop();
                self.update_search();
                InputResult::Continue
            }
            _ => InputResult::Continue,
        }
    }

    /// Handle keyboard events in confirmation modal
    fn handle_confirmation_key(&mut self, key: KeyEvent) -> InputResult {
        match key.code {
            KeyCode::Esc | KeyCode::Char('n') | KeyCode::Char('N') => {
                self.swap_status = None;
                InputResult::Continue
            }
            KeyCode::Enter | KeyCode::Char('y') | KeyCode::Char('Y') => {
                InputResult::ExecuteSwap
            }
            _ => InputResult::Continue,
        }
    }
}

/// Result of handling an input event
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputResult {
    /// Continue normally
    Continue,
    /// Need to fetch a new quote
    NeedsQuote,
    /// Execute the swap
    ExecuteSwap,
    /// Quit the application
    Quit,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quit_handling() {
        let mut app = SwapApp::new("https://api.mainnet-beta.solana.com");

        let result = app.handle_key(KeyEvent::from(KeyCode::Char('q')));
        assert_eq!(result, InputResult::Quit);
        assert!(app.should_quit);
    }

    #[test]
    fn test_amount_input() {
        let mut app = SwapApp::new("https://api.mainnet-beta.solana.com");
        app.focus = SwapFocus::Amount;

        // Type "1.5"
        app.handle_key(KeyEvent::from(KeyCode::Char('1')));
        app.handle_key(KeyEvent::from(KeyCode::Char('.')));
        app.handle_key(KeyEvent::from(KeyCode::Char('5')));

        assert_eq!(app.amount_input, "1.5");
        assert_eq!(app.amount_parsed, Some(1.5));
    }

    #[test]
    fn test_only_one_decimal() {
        let mut app = SwapApp::new("https://api.mainnet-beta.solana.com");
        app.focus = SwapFocus::Amount;

        app.handle_key(KeyEvent::from(KeyCode::Char('1')));
        app.handle_key(KeyEvent::from(KeyCode::Char('.')));
        app.handle_key(KeyEvent::from(KeyCode::Char('5')));
        app.handle_key(KeyEvent::from(KeyCode::Char('.'))); // Should be ignored

        assert_eq!(app.amount_input, "1.5");
    }

    #[test]
    fn test_tab_navigation() {
        let mut app = SwapApp::new("https://api.mainnet-beta.solana.com");
        assert_eq!(app.focus, SwapFocus::Amount);

        app.handle_key(KeyEvent::from(KeyCode::Tab));
        assert_eq!(app.focus, SwapFocus::ToToken);

        app.handle_key(KeyEvent::from(KeyCode::Tab));
        assert_eq!(app.focus, SwapFocus::SwapButton);
    }
}
