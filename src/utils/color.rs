//! Color utility functions for consistent CLI output formatting
//!
//! This module provides standardized colors and formatting for different types of CLI output
//! such as success messages, warnings, errors, and information. It centralizes color schemes
//! to ensure consistent user experience across all commands.

use colored::{Color, Colorize};

/// Format text as a success message (green)
pub fn success(text: &str) -> colored::ColoredString {
    text.green()
}

/// Format text as a warning message (yellow)
pub fn warning(text: &str) -> colored::ColoredString {
    text.yellow()
}

/// Format text as an error message (red)
pub fn error(text: &str) -> colored::ColoredString {
    text.red()
}

/// Format text as a heading (cyan bold)
pub fn heading(text: &str) -> colored::ColoredString {
    text.cyan().bold()
}

/// Format text as a subheading (blue bold)
pub fn subheading(text: &str) -> colored::ColoredString {
    text.blue().bold()
}

/// Format text as important information (bright white)
pub fn important(text: &str) -> colored::ColoredString {
    text.bright_white()
}

/// Format text as secondary information (bright black/gray)
pub fn secondary(text: &str) -> colored::ColoredString {
    text.bright_black()
}

/// Format text as a command example (cyan)
pub fn command(text: &str) -> colored::ColoredString {
    text.cyan()
}

/// Format text as a URL or path (cyan underlined)
pub fn url(text: &str) -> colored::ColoredString {
    text.cyan().underline()
}

/// Format text as a key in key-value pair (yellow)
pub fn key(text: &str) -> colored::ColoredString {
    text.yellow()
}

/// Format text as a value in key-value pair (bright white)
pub fn value(text: &str) -> colored::ColoredString {
    text.bright_white()
}

/// Format node status with appropriate color
pub fn node_status(status: &str) -> colored::ColoredString {
    match status.to_lowercase().as_str() {
        "running" => format!("{} ●", status).green(),
        "stopped" => format!("{} ○", status).yellow(),
        "error" => format!("{} ✕", status).red(),
        _ => format!("{} ?", status).bright_black(),
    }
}

/// Format numeric value with appropriate color based on threshold
pub fn numeric_value(
    value: f64,
    warning_threshold: f64,
    error_threshold: f64,
) -> colored::ColoredString {
    if value >= error_threshold {
        format!("{:.1}", value).red()
    } else if value >= warning_threshold {
        format!("{:.1}", value).yellow()
    } else {
        format!("{:.1}", value).green()
    }
}

/// Format table header text (blue bold)
pub fn table_header(text: &str) -> colored::ColoredString {
    text.blue().bold()
}

/// Format separator line (bright black)
pub fn separator() -> colored::ColoredString {
    "---------------------------------------------------------------------------------"
        .bright_black()
}

/// Format text with custom color
pub fn custom(text: &str, color: Color) -> colored::ColoredString {
    text.color(color)
}

/// Format text in bold
pub fn bold(text: &str) -> colored::ColoredString {
    text.bold()
}

/// Format text in italic
pub fn italic(text: &str) -> colored::ColoredString {
    text.italic()
}

/// Format text as code/monospace
pub fn code(text: &str) -> colored::ColoredString {
    text.white().on_bright_black()
}
