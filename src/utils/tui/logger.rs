// File-based logger for TUI mode - prevents logs from corrupting the UI
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::sync::Mutex;
use once_cell::sync::Lazy;

static LOG_FILE: Lazy<Mutex<Option<File>>> = Lazy::new(|| Mutex::new(None));

/// Initialize the file logger. Call this before starting TUI.
pub fn init_file_logger() -> std::io::Result<String> {
    let log_path = format!("/tmp/osvm_research_{}.log", std::process::id());
    let file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&log_path)?;

    *LOG_FILE.lock().unwrap() = Some(file);
    Ok(log_path)
}

/// Log a message to the file (no-op if logger not initialized)
pub fn log(msg: &str) {
    if let Ok(mut guard) = LOG_FILE.lock() {
        if let Some(ref mut file) = *guard {
            let _ = writeln!(file, "{}", msg);
            let _ = file.flush();
        }
    }
}

/// Log with format
#[macro_export]
macro_rules! tui_log {
    ($($arg:tt)*) => {
        $crate::utils::tui::logger::log(&format!($($arg)*))
    };
}
