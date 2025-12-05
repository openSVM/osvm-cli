//! osvm code - AI-powered coding assistant TUI
//!
//! This module provides a Claude Code-style terminal interface for AI-assisted
//! coding with file operations, command execution, and intelligent approval flows.

pub mod app;
pub mod input;
pub mod tools;
pub mod permissions;
pub mod diff;
pub mod prompt;
pub mod agent;
pub mod views;

pub use app::{CodeApp, CodeFocus, AppStatus, Message, MessageRole, ToolCallDisplay, ToolCallStatus};
pub use input::InputResult;
pub use tools::{Tool, ToolRegistry, ToolContext, ToolOutput, RiskLevel};
pub use permissions::PermissionManager;
pub use agent::CodeAgent;
