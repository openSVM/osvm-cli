//! osvm code - AI-powered coding assistant TUI
//!
//! This module provides a Claude Code-style terminal interface for AI-assisted
//! coding with file operations, command execution, and intelligent approval flows.

pub mod tools;
pub mod permissions;
pub mod diff;
pub mod prompt;
pub mod agent;
pub mod views;

pub use tools::{Tool, ToolRegistry, ToolContext, ToolOutput, RiskLevel};
pub use permissions::PermissionManager;
pub use agent::CodeAgent;
