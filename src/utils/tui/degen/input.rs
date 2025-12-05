//! Input handling for degen TUI
//!
//! Handles keyboard input and UI interactions.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

/// Result of input handling
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputResult {
    /// Continue normal operation
    Continue,
    /// User wants to quit
    Quit,
    /// Start the agent
    StartAgent,
    /// Stop the agent
    StopAgent,
    /// Toggle pause
    TogglePause,
    /// Switch to a specific tab
    SwitchTab(usize),
    /// Scroll up
    ScrollUp,
    /// Scroll down
    ScrollDown,
    /// Show help
    ShowHelp,
    /// Hide help
    HideHelp,
}

/// Parse a key event into an input result
pub fn handle_key(key: KeyEvent, state: &InputState) -> InputResult {
    // Check for Ctrl+C
    if key.modifiers.contains(KeyModifiers::CONTROL) && key.code == KeyCode::Char('c') {
        return InputResult::Quit;
    }

    // Help overlay active
    if state.show_help {
        return match key.code {
            KeyCode::Esc | KeyCode::Char('?') => InputResult::HideHelp,
            _ => InputResult::Continue,
        };
    }

    // Normal mode
    match key.code {
        KeyCode::Char('q') | KeyCode::Esc => InputResult::Quit,
        KeyCode::Char('?') => InputResult::ShowHelp,
        KeyCode::Char('s') | KeyCode::Char('S') => InputResult::StartAgent,
        KeyCode::Char('x') | KeyCode::Char('X') => InputResult::StopAgent,
        KeyCode::Char('p') | KeyCode::Char('P') => InputResult::TogglePause,

        // Tab navigation
        KeyCode::Char('1') => InputResult::SwitchTab(0),
        KeyCode::Char('2') => InputResult::SwitchTab(1),
        KeyCode::Char('3') => InputResult::SwitchTab(2),
        KeyCode::Char('4') => InputResult::SwitchTab(3),
        KeyCode::Tab | KeyCode::Right => InputResult::SwitchTab(state.next_tab()),
        KeyCode::BackTab | KeyCode::Left => InputResult::SwitchTab(state.prev_tab()),

        // Scrolling
        KeyCode::Up | KeyCode::Char('k') => InputResult::ScrollUp,
        KeyCode::Down | KeyCode::Char('j') => InputResult::ScrollDown,

        _ => InputResult::Continue,
    }
}

/// Current input state for context-aware handling
#[derive(Debug, Clone)]
pub struct InputState {
    pub active_tab: usize,
    pub tab_count: usize,
    pub show_help: bool,
    pub is_running: bool,
    pub is_paused: bool,
}

impl InputState {
    pub fn new() -> Self {
        Self {
            active_tab: 0,
            tab_count: 4,
            show_help: false,
            is_running: false,
            is_paused: false,
        }
    }

    pub fn next_tab(&self) -> usize {
        (self.active_tab + 1) % self.tab_count
    }

    pub fn prev_tab(&self) -> usize {
        if self.active_tab == 0 {
            self.tab_count - 1
        } else {
            self.active_tab - 1
        }
    }
}

impl Default for InputState {
    fn default() -> Self {
        Self::new()
    }
}
