//! Text sanitization and formatting utilities

use regex::Regex;
use serde_json::Value;

/// Sanitize text for safe display in TUI
pub fn sanitize_text_for_ui(text: &str) -> String {
    let mut sanitized = text.to_string();

    // Remove common ANSI escape sequences (colors, cursor movement) which break TUI layout
    if let Ok(ansi_re) = Regex::new(r"\x1B\[[0-9;?]*[ -/]*[@-~]") {
        sanitized = ansi_re.replace_all(&sanitized, "").to_string();
    }

    // Convert tabs to 4 spaces to keep columns aligned
    sanitized = sanitized.replace('\t', "    ");

    // Normalize line endings
    sanitized = sanitized.replace("\r\n", "\n").replace('\r', "\n");

    // Remove other control characters (except newline) that can corrupt the TUI
    sanitized = sanitized.chars()
        .filter(|c| *c == '\n' || !c.is_control())
        .collect();

    // Redact potential private keys (base58-like patterns)
    if let Ok(key_pattern) = Regex::new(r"\b[1-9A-HJ-NP-Za-km-z]{32,44}\b") {
        sanitized = key_pattern.replace_all(&sanitized, "[REDACTED_KEY]").to_string();
    }

    // Collapse long runs of whitespace/newlines to avoid blowups
    if let Ok(multi_nl) = Regex::new(r"\n{3,}") {
        sanitized = multi_nl.replace_all(&sanitized, "\n\n").to_string();
    }

    // Limit length to prevent display overflow
    if sanitized.len() > 2000 {
        sanitized.truncate(1997);
        sanitized.push_str("...");
    }

    sanitized
}

/// Sanitize JSON for safe display in TUI
pub fn sanitize_json_for_ui(value: &Value) -> String {
    match serde_json::to_string_pretty(value) {
        Ok(json_str) => {
            let sanitized = sanitize_text_for_ui(&json_str);
            if sanitized.len() > 1000 {
                let take = 997.min(sanitized.len());
                format!("{}...", &sanitized[..take])
            } else {
                sanitized
            }
        }
        Err(_) => "[Invalid JSON]".to_string()
    }
}

/// Enhanced text sanitization with emoji removal (alias for backward compatibility)
pub fn sanitize_text(text: &str) -> String {
    sanitize_text_for_ui(text)
}

/// Enhanced JSON sanitization (alias for backward compatibility)
pub fn sanitize_json(value: &Value) -> String {
    sanitize_json_for_ui(value)
}