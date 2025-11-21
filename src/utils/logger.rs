use anyhow::Result;
use std::fs;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};
use tracing_subscriber::{fmt, layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

/// Initialize logging to ~/.osvm/logs/log-{timestamp}.log
///
/// Creates the logs directory if it doesn't exist and sets up file logging
/// for all OSVM operations.
pub fn init_logging() -> Result<PathBuf> {
    // Get home directory
    let home = dirs::home_dir().ok_or_else(|| anyhow::anyhow!("Cannot determine home directory"))?;

    // Create ~/.osvm/logs/ directory
    let logs_dir = home.join(".osvm").join("logs");
    fs::create_dir_all(&logs_dir)?;

    // Generate timestamp-based log filename
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|e| anyhow::anyhow!("System time error: {}", e))?
        .as_secs();

    let log_filename = format!("log-{}.log", timestamp);
    let log_path = logs_dir.join(&log_filename);

    // Set up file appender
    let file_appender = tracing_appender::rolling::never(&logs_dir, &log_filename);

    // Create fmt layer for file output
    let file_layer = fmt::layer()
        .with_writer(file_appender)
        .with_ansi(false)  // No ANSI codes in log files
        .with_target(true)
        .with_thread_ids(true)
        .with_line_number(true);

    // Create console layer for stdout (optional, can be disabled)
    let console_layer = fmt::layer()
        .with_writer(std::io::stdout)
        .with_target(false);

    // Set up env filter (default to info level)
    let filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new("info"));

    // Initialize subscriber with both file and console output
    tracing_subscriber::registry()
        .with(filter)
        .with(file_layer)
        .with(console_layer)
        .init();

    // Log initialization
    tracing::info!("OSVM logging initialized");
    tracing::info!("Log file: {}", log_path.display());

    Ok(log_path)
}

/// Initialize logging to file only (no console output) for TUI mode
///
/// This suppresses all stdout/stderr logging to keep the TUI clean.
pub fn init_logging_quiet() -> Result<PathBuf> {
    // Get home directory
    let home = dirs::home_dir().ok_or_else(|| anyhow::anyhow!("Cannot determine home directory"))?;

    // Create ~/.osvm/logs/ directory
    let logs_dir = home.join(".osvm").join("logs");
    fs::create_dir_all(&logs_dir)?;

    // Generate timestamp-based log filename
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|e| anyhow::anyhow!("System time error: {}", e))?
        .as_secs();

    let log_filename = format!("log-{}.log", timestamp);
    let log_path = logs_dir.join(&log_filename);

    // Set up file appender
    let file_appender = tracing_appender::rolling::never(&logs_dir, &log_filename);

    // Create fmt layer for file output ONLY (no console)
    let file_layer = fmt::layer()
        .with_writer(file_appender)
        .with_ansi(false)
        .with_target(true)
        .with_thread_ids(true)
        .with_line_number(true);

    // Set up env filter (default to info level)
    let filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new("info"));

    // Initialize subscriber with file output ONLY (no console layer)
    tracing_subscriber::registry()
        .with(filter)
        .with(file_layer)
        .init();

    // Log initialization (goes to file only)
    tracing::info!("OSVM logging initialized (quiet mode for TUI)");
    tracing::info!("Log file: {}", log_path.display());

    Ok(log_path)
}

/// Get the logs directory path
pub fn get_logs_dir() -> Result<PathBuf> {
    let home = dirs::home_dir().ok_or_else(|| anyhow::anyhow!("Cannot determine home directory"))?;
    Ok(home.join(".osvm").join("logs"))
}

/// List all log files in the logs directory
pub fn list_log_files() -> Result<Vec<PathBuf>> {
    let logs_dir = get_logs_dir()?;

    if !logs_dir.exists() {
        return Ok(Vec::new());
    }

    let mut log_files: Vec<PathBuf> = fs::read_dir(&logs_dir)?
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| {
            path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("log")
        })
        .collect();

    // Sort by modification time (newest first)
    log_files.sort_by(|a, b| {
        let a_time = fs::metadata(a).and_then(|m| m.modified()).ok();
        let b_time = fs::metadata(b).and_then(|m| m.modified()).ok();
        b_time.cmp(&a_time)
    });

    Ok(log_files)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_logs_dir() {
        let logs_dir = get_logs_dir().unwrap();
        assert!(logs_dir.ends_with(".osvm/logs"));
    }

    #[test]
    fn test_init_logging() {
        // This test just ensures the function doesn't panic
        // Actual logging initialization would conflict with other tests
        let _ = get_logs_dir();
    }
}
