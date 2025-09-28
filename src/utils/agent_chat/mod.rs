//! Dynamic embedded terminal chat interface with real-time suggestions
//!
//! This module provides a Claude Code-style chat interface that runs embedded
//! in the terminal session with real-time auto-complete and suggestions.

mod colors;
mod fuzzy_matcher;
mod suggestions;
mod task_state;
mod input_handler;
mod ui_components;
mod terminal_utils;
mod ai_integration;
mod command_processor;
mod chat_application;

// Re-export public API
pub use chat_application::{run_agent_chat_ui, run_chat_ui_tests};
pub use task_state::TaskState;
pub use input_handler::InputState;
pub use chat_application::App;

// Internal re-exports for module communication
pub(crate) use colors::Colors;
pub(crate) use fuzzy_matcher::FuzzyMatcher;
pub(crate) use suggestions::RealtimeSuggestion;
pub(crate) use task_state::{TodoItem, TodoPriority, InputMode, SPINNER_FRAMES};
pub(crate) use input_handler::{InputConfig, InputChar, ArrowKey};