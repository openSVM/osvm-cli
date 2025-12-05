//! Degen Trading Agent TUI
//!
//! Provides a beautiful terminal dashboard for the autonomous trading agent.

pub mod app;
pub mod input;
pub mod views;

pub use app::DegenApp;
pub use crate::services::degen_agent::DegenConfig;
