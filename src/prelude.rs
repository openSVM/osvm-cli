//! Exports key capabilities in a single module for convenient use
//!
//! This module re-exports the key modules from the utils directory
//! to make them easily accessible throughout the codebase.

// SVM information and management utilities
pub use crate::utils::svm_info;
// SSH deployment utilities
pub use crate::utils::ssh_deploy;
// Node management utilities
pub use crate::utils::nodes;
// Dashboard utilities
pub use crate::utils::dashboard;
// Example command utilities
pub use crate::utils::examples;
// Color formatting utilities
pub use crate::utils::color;

/// Type alias for progress callback functions
///
/// This type is used throughout the codebase for functions that report progress
/// with a percentage (0-100) and a status message.
pub type ProgressCallback = Box<dyn Fn(u8, &str) + Send>;
