//! Color utility functions for consistent CLI output formatting
//!
//! This module provides standardized colors and formatting for different types of CLI output
//! such as success messages, warnings, errors, and information. It centralizes color schemes
//! to ensure consistent user experience across all commands.

use colored::{Color, Colorize};

/// Format text as a success message (green)
#[must_use]
pub fn success(text: &str) -> colored::ColoredString {
    text.green()
}

/// Format text as a warning message (yellow)
#[must_use]
pub fn warning(text: &str) -> colored::ColoredString {
    text.yellow()
}

/// Format text as an error message (red)
#[must_use]
pub fn error(text: &str) -> colored::ColoredString {
    text.red()
}

/// Format text as a heading (cyan bold)
#[must_use]
pub fn heading(text: &str) -> colored::ColoredString {
    text.cyan().bold()
}

/// Format text as a subheading (blue bold)
#[must_use]
pub fn subheading(text: &str) -> colored::ColoredString {
    text.blue().bold()
}

/// Format text as important information (bright white)
#[must_use]
pub fn important(text: &str) -> colored::ColoredString {
    text.bright_white()
}

/// Format text as secondary information (bright black/gray)
#[must_use]
pub fn secondary(text: &str) -> colored::ColoredString {
    text.bright_black()
}

/// Format text as a command example (cyan)
#[must_use]
pub fn command(text: &str) -> colored::ColoredString {
    text.cyan()
}

/// Format text as a URL or path (cyan underlined)
#[must_use]
pub fn url(text: &str) -> colored::ColoredString {
    text.cyan().underline()
}

/// Format text as a key in key-value pair (yellow)
#[must_use]
pub fn key(text: &str) -> colored::ColoredString {
    text.yellow()
}

/// Format text as a value in key-value pair (bright white)
#[must_use]
pub fn value(text: &str) -> colored::ColoredString {
    text.bright_white()
}

/// Format node status with appropriate color
#[must_use]
pub fn node_status(status: &str) -> colored::ColoredString {
    match status.to_lowercase().as_str() {
        "running" => format!("{} ●", status).green(),
        "stopped" => format!("{} ○", status).yellow(),
        "error" => format!("{} ✕", status).red(),
        _ => format!("{} ?", status).bright_black(),
    }
}

/// Format numeric value with appropriate color based on threshold
#[must_use]
pub fn numeric_value(
    value: f64,
    warning_threshold: f64,
    error_threshold: f64,
) -> colored::ColoredString {
    if value >= error_threshold {
        format!("{value:.1}").red()
    } else if value >= warning_threshold {
        format!("{value:.1}").yellow()
    } else {
        format!("{value:.1}").green()
    }
}

/// Format table header text (blue bold)
#[must_use]
pub fn table_header(text: &str) -> colored::ColoredString {
    text.blue().bold()
}

/// Format separator line (bright black)
#[must_use]
pub fn separator() -> colored::ColoredString {
    "---------------------------------------------------------------------------------"
        .bright_black()
}

/// Format text with custom color
#[must_use]
pub fn custom(text: &str, color: Color) -> colored::ColoredString {
    text.color(color)
}

/// Format text in bold
#[must_use]
pub fn bold(text: &str) -> colored::ColoredString {
    text.bold()
}

/// Format text in italic
#[must_use]
pub fn italic(text: &str) -> colored::ColoredString {
    text.italic()
}

/// Format text as code/monospace
#[must_use]
pub fn code(text: &str) -> colored::ColoredString {
    text.white().on_bright_black()
}
