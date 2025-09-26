//! Debug logging utilities with configurable verbosity levels
//!
//! This module provides standardized debug output across the application
//! with fine-grained control over verbosity levels.

use std::sync::atomic::{AtomicU8, Ordering};

/// Global verbosity level
static VERBOSITY_LEVEL: AtomicU8 = AtomicU8::new(0);

/// Debug verbosity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum VerbosityLevel {
    /// No debug output
    Silent = 0,
    /// Basic operations and errors
    Basic = 1,
    /// Detailed operations
    Detailed = 2,
    /// Everything including internal state
    Verbose = 3,
}

impl From<u8> for VerbosityLevel {
    fn from(level: u8) -> Self {
        match level {
            0 => VerbosityLevel::Silent,
            1 => VerbosityLevel::Basic,
            2 => VerbosityLevel::Detailed,
            _ => VerbosityLevel::Verbose,
        }
    }
}

/// Set global verbosity level
pub fn set_verbosity(level: VerbosityLevel) {
    VERBOSITY_LEVEL.store(level as u8, Ordering::Relaxed);
}

/// Get current verbosity level
pub fn get_verbosity() -> VerbosityLevel {
    VerbosityLevel::from(VERBOSITY_LEVEL.load(Ordering::Relaxed))
}

/// Check if a verbosity level should be logged
pub fn should_log(level: VerbosityLevel) -> bool {
    level <= get_verbosity()
}

/// Debug print macro with verbosity control
#[macro_export]
macro_rules! debug_print {
    ($level:expr, $($arg:tt)*) => {
        if $crate::utils::debug_logger::should_log($level) {
            match $level {
                $crate::utils::debug_logger::VerbosityLevel::Basic => {
                    println!("🔍 {}", format!($($arg)*));
                }
                $crate::utils::debug_logger::VerbosityLevel::Detailed => {
                    println!("📋 {}", format!($($arg)*));
                }
                $crate::utils::debug_logger::VerbosityLevel::Verbose => {
                    println!("🔬 {}", format!($($arg)*));
                }
                _ => {}
            }
        }
    };
}

/// Warning print macro
#[macro_export]
macro_rules! debug_warn {
    ($($arg:tt)*) => {
        if $crate::utils::debug_logger::should_log($crate::utils::debug_logger::VerbosityLevel::Basic) {
            println!("⚠️  {}", format!($($arg)*));
        }
    };
}

/// Success print macro
#[macro_export]
macro_rules! debug_success {
    ($($arg:tt)*) => {
        if $crate::utils::debug_logger::should_log($crate::utils::debug_logger::VerbosityLevel::Basic) {
            println!("✅ {}", format!($($arg)*));
        }
    };
}

/// Error print macro
#[macro_export]
macro_rules! debug_error {
    ($($arg:tt)*) => {
        if $crate::utils::debug_logger::should_log($crate::utils::debug_logger::VerbosityLevel::Basic) {
            eprintln!("❌ {}", format!($($arg)*));
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_verbosity_control() {
        set_verbosity(VerbosityLevel::Detailed);
        assert!(should_log(VerbosityLevel::Basic));
        assert!(should_log(VerbosityLevel::Detailed));
        assert!(!should_log(VerbosityLevel::Verbose));

        set_verbosity(VerbosityLevel::Silent);
        assert!(!should_log(VerbosityLevel::Basic));
    }
}
