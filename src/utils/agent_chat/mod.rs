//! Dynamic embedded terminal chat interface with real-time suggestions
//!
//! This module provides a Claude Code-style chat interface that runs embedded
//! in the terminal session with real-time auto-complete and suggestions.

mod ai_integration;
mod chat_application;
mod colors;
mod command_processor;
mod fuzzy_matcher;
mod input_handler;
mod responsive_layout;
mod suggestions;
mod system_status_bar;
mod task_state;
mod terminal_utils;
mod ui_components;

// Re-export public API
pub use chat_application::App;
pub use chat_application::{
    get_instant_suggestions, handle_regular_character, run_agent_chat_ui, run_chat_ui_tests,
};
pub use input_handler::{disable_raw_mode, enable_raw_mode, InputState};
pub use task_state::TaskState;

// Internal re-exports for module communication
pub(crate) use colors::Colors;
pub(crate) use fuzzy_matcher::FuzzyMatcher;
pub(crate) use input_handler::{ArrowKey, InputChar, InputConfig};
pub(crate) use suggestions::RealtimeSuggestion;
pub(crate) use task_state::{InputMode, TodoItem, TodoPriority, SPINNER_FRAMES};
