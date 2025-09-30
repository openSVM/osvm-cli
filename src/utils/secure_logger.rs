//! Secure logging utilities to prevent information disclosure
//!
//! This module provides secure logging functions that automatically sanitize
//! sensitive information before outputting to logs or console.

use regex::Regex;
use std::collections::HashMap;

/// Patterns that should be redacted from logs
static SENSITIVE_PATTERNS: &[(&str, &str)] = &[
    // API Keys and tokens
    (r"(?i)(api[_-]?key|token|secret|password)\s*[:=]\s*[A-Za-z0-9+/=]{8,}", "$1: [REDACTED]"),
    // Private keys (base58/hex)
    (r"[A-Za-z0-9]{44,88}", "[PRIVATE_KEY_REDACTED]"),
    // Solana addresses (but preserve format for debugging)
    (r"\b[1-9A-HJ-NP-Za-km-z]{32,44}\b", "[ADDRESS_REDACTED]"),
    // URLs with credentials
    (r"(https?://)([^:]+):([^@]+)@", "$1[USER]:[PASS]@"),
    // File paths that might contain usernames
    (r"/home/([^/\s]+)", "/home/[USER]"),
    (r"C:\\Users\\([^\\]+)", r"C:\Users\[USER]"),
];

/// Secure logger that automatically redacts sensitive information
pub struct SecureLogger {
    patterns: Vec<(Regex, String)>,
    debug_mode: bool,
}

impl SecureLogger {
    /// Create a new secure logger
    pub fn new(debug_mode: bool) -> Self {
        let patterns = SENSITIVE_PATTERNS
            .iter()
            .filter_map(|(pattern, replacement)| {
                Regex::new(pattern)
                    .ok()
                    .map(|regex| (regex, replacement.to_string()))
            })
            .collect();

        Self {
            patterns,
            debug_mode,
        }
    }

    /// Sanitize a string to remove sensitive information
    pub fn sanitize(&self, input: &str) -> String {
        let mut sanitized = input.to_string();

        for (regex, replacement) in &self.patterns {
            sanitized = regex.replace_all(&sanitized, replacement.as_str()).to_string();
        }

        sanitized
    }

    /// Log an info message with automatic sanitization
    pub fn info(&self, message: &str) {
        let sanitized = self.sanitize(message);
        println!("ℹ️  {}", sanitized);
    }

    /// Log a success message with automatic sanitization
    pub fn success(&self, message: &str) {
        let sanitized = self.sanitize(message);
        println!("✅ {}", sanitized);
    }

    /// Log a warning message with automatic sanitization
    pub fn warn(&self, message: &str) {
        let sanitized = self.sanitize(message);
        eprintln!("⚠️  {}", sanitized);
    }

    /// Log an error message with automatic sanitization
    pub fn error(&self, message: &str) {
        let sanitized = self.sanitize(message);
        eprintln!("❌ {}", sanitized);
    }

    /// Log a debug message (only in debug mode) with sanitization
    pub fn debug(&self, message: &str) {
        if self.debug_mode {
            let sanitized = self.sanitize(message);
            eprintln!("🔍 DEBUG: {}", sanitized);
        }
    }

    /// Log raw data with extra sanitization for debugging
    pub fn debug_data(&self, label: &str, data: &str) {
        if self.debug_mode {
            let sanitized = self.sanitize(data);
            // Extra protection: truncate very long debug output
            let truncated = if sanitized.len() > 500 {
                format!("{}... [TRUNCATED]", &sanitized[..500])
            } else {
                sanitized
            };
            eprintln!("🔍 DEBUG {}: {}", label, truncated);
        }
    }
}

/// Global secure logger instance
lazy_static::lazy_static! {
    static ref GLOBAL_LOGGER: std::sync::Mutex<Option<SecureLogger>> = std::sync::Mutex::new(None);
}

/// Initialize the global secure logger
pub fn init_secure_logger(debug_mode: bool) {
    let mut logger = GLOBAL_LOGGER.lock().unwrap();
    *logger = Some(SecureLogger::new(debug_mode));
}

/// Get a reference to the global secure logger
fn with_logger<F, R>(f: F) -> R
where
    F: FnOnce(&SecureLogger) -> R,
{
    let logger_guard = GLOBAL_LOGGER.lock().unwrap();
    if let Some(logger) = logger_guard.as_ref() {
        f(logger)
    } else {
        // Fallback logger if not initialized
        let fallback = SecureLogger::new(false);
        f(&fallback)
    }
}

/// Secure logging macros
#[macro_export]
macro_rules! secure_info {
    ($($arg:tt)*) => {
        $crate::utils::secure_logger::with_logger(|logger| {
            logger.info(&format!($($arg)*));
        })
    };
}

#[macro_export]
macro_rules! secure_success {
    ($($arg:tt)*) => {
        $crate::utils::secure_logger::with_logger(|logger| {
            logger.success(&format!($($arg)*));
        })
    };
}

#[macro_export]
macro_rules! secure_warn {
    ($($arg:tt)*) => {
        $crate::utils::secure_logger::with_logger(|logger| {
            logger.warn(&format!($($arg)*));
        })
    };
}

#[macro_export]
macro_rules! secure_error {
    ($($arg:tt)*) => {
        $crate::utils::secure_logger::with_logger(|logger| {
            logger.error(&format!($($arg)*));
        })
    };
}

#[macro_export]
macro_rules! secure_debug {
    ($($arg:tt)*) => {
        $crate::utils::secure_logger::with_logger(|logger| {
            logger.debug(&format!($($arg)*));
        })
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_api_key_redaction() {
        let logger = SecureLogger::new(false);
        let input = "API_KEY=sk-1234567890abcdef";
        let sanitized = logger.sanitize(input);
        assert!(!sanitized.contains("sk-1234567890abcdef"));
        assert!(sanitized.contains("[REDACTED]"));
    }

    #[test]
    fn test_private_key_redaction() {
        let logger = SecureLogger::new(false);
        let input = "Private key: 5KJvsngHeMpm884wtkJNzQGaCErckhHJBGFsvd3VyK5qMZXj3hS";
        let sanitized = logger.sanitize(input);
        assert!(!sanitized.contains("5KJvsngHeMpm884wtkJNzQGaCErckhHJBGFsvd3VyK5qMZXj3hS"));
        assert!(sanitized.contains("[PRIVATE_KEY_REDACTED]"));
    }

    #[test]
    fn test_path_redaction() {
        let logger = SecureLogger::new(false);
        let input = "/home/alice/.config/osvm/keys";
        let sanitized = logger.sanitize(input);
        assert!(!sanitized.contains("alice"));
        assert!(sanitized.contains("/home/[USER]/"));
    }
}