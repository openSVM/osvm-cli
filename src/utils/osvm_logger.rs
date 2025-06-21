use chrono::{DateTime, Utc};
use colored::*;
use serde::{Deserialize, Serialize};
use std::fs::OpenOptions;
use std::io::Write;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LogLevel {
    Debug,
    Info,
    Warn,
    Error,
    Critical,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LogCategory {
    System,
    AutoRepair,
    Validator,
    Network,
    Ngrok,
    Config,
    CLI,
    Performance,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogEntry {
    pub timestamp: DateTime<Utc>,
    pub level: LogLevel,
    pub category: LogCategory,
    pub message: String,
    pub context: std::collections::HashMap<String, String>,
    pub component: String,
}

pub struct OsvmLogger {
    log_file: Arc<Mutex<std::fs::File>>,
    min_level: LogLevel,
    console_enabled: bool,
    file_enabled: bool,
}

impl OsvmLogger {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let log_file = OpenOptions::new()
            .create(true)
            .append(true)
            .open("osvm.log")?;

        Ok(Self {
            log_file: Arc::new(Mutex::new(log_file)),
            min_level: LogLevel::Info,
            console_enabled: true,
            file_enabled: true,
        })
    }

    pub fn with_level(mut self, level: LogLevel) -> Self {
        self.min_level = level;
        self
    }

    pub fn with_console(mut self, enabled: bool) -> Self {
        self.console_enabled = enabled;
        self
    }

    pub fn with_file(mut self, enabled: bool) -> Self {
        self.file_enabled = enabled;
        self
    }

    pub fn log(
        &self,
        level: LogLevel,
        category: LogCategory,
        component: &str,
        message: &str,
        context: std::collections::HashMap<String, String>,
    ) {
        if !self.should_log(&level) {
            return;
        }

        let entry = LogEntry {
            timestamp: Utc::now(),
            level: level.clone(),
            category: category.clone(),
            message: message.to_string(),
            context,
            component: component.to_string(),
        };

        if self.console_enabled {
            self.log_to_console(&entry);
        }

        if self.file_enabled {
            self.log_to_file(&entry);
        }
    }

    fn should_log(&self, level: &LogLevel) -> bool {
        let level_priority = match level {
            LogLevel::Debug => 0,
            LogLevel::Info => 1,
            LogLevel::Warn => 2,
            LogLevel::Error => 3,
            LogLevel::Critical => 4,
        };

        let min_priority = match self.min_level {
            LogLevel::Debug => 0,
            LogLevel::Info => 1,
            LogLevel::Warn => 2,
            LogLevel::Error => 3,
            LogLevel::Critical => 4,
        };

        level_priority >= min_priority
    }

    fn log_to_console(&self, entry: &LogEntry) {
        let level_str = match entry.level {
            LogLevel::Debug => "🐛 DEBUG".bright_blue(),
            LogLevel::Info => "ℹ️  INFO".bright_green(),
            LogLevel::Warn => "⚠️  WARN".bright_yellow(),
            LogLevel::Error => "❌ ERROR".bright_red(),
            LogLevel::Critical => "🚨 CRITICAL".bright_magenta(),
        };

        let category_str = match entry.category {
            LogCategory::System => "SYS",
            LogCategory::AutoRepair => "REPAIR",
            LogCategory::Validator => "VALIDATOR",
            LogCategory::Network => "NET",
            LogCategory::Ngrok => "NGROK",
            LogCategory::Config => "CFG",
            LogCategory::CLI => "CLI",
            LogCategory::Performance => "PERF",
        };

        let time = entry.timestamp.format("%H:%M:%S%.3f");

        println!(
            "{} [{}] [{}] [{}] {}",
            level_str,
            time,
            category_str.bright_cyan(),
            entry.component.bright_white(),
            entry.message
        );

        // Print context if not empty
        if !entry.context.is_empty() {
            for (key, value) in &entry.context {
                println!("    {} = {}", key.bright_black(), value.bright_black());
            }
        }
    }

    fn log_to_file(&self, entry: &LogEntry) {
        if let Ok(mut file) = self.log_file.lock() {
            let json_line = serde_json::to_string(entry).unwrap_or_else(|_| {
                format!(r#"{{"timestamp":"{}","level":"{:?}","category":"{:?}","component":"{}","message":"{}","context":{{}}}}"#,
                    entry.timestamp.to_rfc3339(),
                    entry.level,
                    entry.category,
                    entry.component,
                    entry.message.replace('"', "\\\"")
                )
            });

            let _ = writeln!(file, "{}", json_line);
            let _ = file.flush();
        }
    }

    // Convenience methods
    pub fn debug(&self, category: LogCategory, component: &str, message: &str) {
        self.log(
            LogLevel::Debug,
            category,
            component,
            message,
            std::collections::HashMap::new(),
        );
    }

    pub fn info(&self, category: LogCategory, component: &str, message: &str) {
        self.log(
            LogLevel::Info,
            category,
            component,
            message,
            std::collections::HashMap::new(),
        );
    }

    pub fn warn(&self, category: LogCategory, component: &str, message: &str) {
        self.log(
            LogLevel::Warn,
            category,
            component,
            message,
            std::collections::HashMap::new(),
        );
    }

    pub fn error(&self, category: LogCategory, component: &str, message: &str) {
        self.log(
            LogLevel::Error,
            category,
            component,
            message,
            std::collections::HashMap::new(),
        );
    }

    pub fn critical(&self, category: LogCategory, component: &str, message: &str) {
        self.log(
            LogLevel::Critical,
            category,
            component,
            message,
            std::collections::HashMap::new(),
        );
    }

    // Methods with context
    pub fn info_with_context(
        &self,
        category: LogCategory,
        component: &str,
        message: &str,
        context: std::collections::HashMap<String, String>,
    ) {
        self.log(LogLevel::Info, category, component, message, context);
    }

    pub fn error_with_context(
        &self,
        category: LogCategory,
        component: &str,
        message: &str,
        context: std::collections::HashMap<String, String>,
    ) {
        self.log(LogLevel::Error, category, component, message, context);
    }

    pub fn debug_with_context(
        &self,
        category: LogCategory,
        component: &str,
        message: &str,
        context: std::collections::HashMap<String, String>,
    ) {
        self.log(LogLevel::Debug, category, component, message, context);
    }
}

// Global logger instance
lazy_static::lazy_static! {
    pub static ref OSVM_LOGGER: OsvmLogger = OsvmLogger::new().unwrap_or_else(|e| {
        eprintln!("Failed to initialize OSVM logger: {}", e);
        OsvmLogger::new().unwrap()
    });
}

// Convenience macros
#[macro_export]
macro_rules! osvm_info {
    ($category:expr, $component:expr, $message:expr) => {
        $crate::utils::osvm_logger::OSVM_LOGGER.info($category, $component, $message);
    };
    ($category:expr, $component:expr, $message:expr, $context:expr) => {
        $crate::utils::osvm_logger::OSVM_LOGGER
            .info_with_context($category, $component, $message, $context);
    };
}

#[macro_export]
macro_rules! osvm_error {
    ($category:expr, $component:expr, $message:expr) => {
        $crate::utils::osvm_logger::OSVM_LOGGER.error($category, $component, $message);
    };
    ($category:expr, $component:expr, $message:expr, $context:expr) => {
        $crate::utils::osvm_logger::OSVM_LOGGER
            .error_with_context($category, $component, $message, $context);
    };
}

#[macro_export]
macro_rules! osvm_warn {
    ($category:expr, $component:expr, $message:expr) => {
        $crate::utils::osvm_logger::OSVM_LOGGER.warn($category, $component, $message);
    };
}

#[macro_export]
macro_rules! osvm_debug {
    ($category:expr, $component:expr, $message:expr) => {
        $crate::utils::osvm_logger::OSVM_LOGGER.debug($category, $component, $message);
    };
    ($category:expr, $component:expr, $message:expr, $context:expr) => {
        $crate::utils::osvm_logger::OSVM_LOGGER
            .debug_with_context($category, $component, $message, $context);
    };
}

#[macro_export]
macro_rules! osvm_critical {
    ($category:expr, $component:expr, $message:expr) => {
        $crate::utils::osvm_logger::OSVM_LOGGER.critical($category, $component, $message);
    };
}
