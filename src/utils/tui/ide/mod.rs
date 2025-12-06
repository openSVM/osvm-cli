//! OVSM IDE TUI
//!
//! Provides an integrated development environment for OVSM LISP scripting
//! with syntax highlighting, REPL, and file management.

pub mod app;

pub use app::{IdeApp, IdeFocus, SourceFile};
