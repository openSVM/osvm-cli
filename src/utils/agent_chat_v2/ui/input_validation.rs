//! Input validation and sanitization

/// Input validation result
pub enum ValidationResult {
    Valid(String),
    Empty,
    TooLong { text: String, max_length: usize },
    OnlyWhitespace,
    ContainsBinaryData,
    TooManyNewlines { text: String, max_lines: usize },
}

/// Validate user input before processing
pub fn validate_input(input: &str) -> ValidationResult {
    // Check for empty input
    if input.is_empty() {
        return ValidationResult::Empty;
    }

    // Check for only whitespace
    if input.trim().is_empty() {
        return ValidationResult::OnlyWhitespace;
    }

    // Check length (reasonable limit for chat messages)
    const MAX_LENGTH: usize = 10_000;
    if input.len() > MAX_LENGTH {
        // BUG-2001 fix: Use char-based truncation to avoid UTF-8 boundary panic
        let truncated: String = input.chars().take(MAX_LENGTH).collect();
        return ValidationResult::TooLong {
            text: truncated,
            max_length: MAX_LENGTH,
        };
    }

    // Check for excessive newlines (prevents UI breaking)
    const MAX_NEWLINES: usize = 50;
    let newline_count = input.chars().filter(|&c| c == '\n').count();
    if newline_count > MAX_NEWLINES {
        return ValidationResult::TooManyNewlines {
            text: input.to_string(),
            max_lines: MAX_NEWLINES,
        };
    }

    // Check for binary data or control characters (except common ones)
    let has_binary = input
        .chars()
        .any(|c| c.is_control() && c != '\n' && c != '\t' && c != '\r');

    if has_binary {
        return ValidationResult::ContainsBinaryData;
    }

    // Input is valid
    ValidationResult::Valid(input.to_string())
}

/// Sanitize input for safe display and processing
pub fn sanitize_input(input: &str) -> String {
    input
        .chars()
        .filter(|&c| {
            // Keep printable characters, newlines, and tabs
            !c.is_control() || c == '\n' || c == '\t' || c == '\r'
        })
        .collect()
}

/// Truncate message for display with ellipsis
pub fn truncate_for_display(text: &str, max_length: usize) -> String {
    if text.len() <= max_length {
        text.to_string()
    } else {
        // BUG-2001 fix: Use char-based truncation to avoid UTF-8 boundary panic
        // especially for emoji and multi-byte characters
        let chars_to_take = max_length.saturating_sub(3);
        let truncated: String = text.chars().take(chars_to_take).collect();
        format!("{}...", truncated)
    }
}

/// Check if input looks like a command
pub fn is_command(input: &str) -> bool {
    input.trim().starts_with('/')
}

/// Parse command from input
pub fn parse_command(input: &str) -> Option<(String, Vec<String>)> {
    let trimmed = input.trim();
    if !trimmed.starts_with('/') {
        return None;
    }

    // Skip the '/' character safely using chars().skip() instead of unchecked slicing
    let remaining: String = trimmed.chars().skip(1).collect();
    let parts: Vec<&str> = remaining.split_whitespace().collect();
    if parts.is_empty() {
        return None;
    }

    let command = parts[0].to_string();
    let args = parts.iter().skip(1).map(|s| s.to_string()).collect();

    Some((command, args))
}

/// Check if input contains potentially sensitive data
pub fn contains_sensitive_pattern(input: &str) -> bool {
    let input_lower = input.to_lowercase();

    // Common patterns for private keys, seeds, passwords
    let sensitive_patterns = [
        "private key",
        "seed phrase",
        "mnemonic",
        "password:",
        "secret:",
        "api_key:",
        "token:",
    ];

    sensitive_patterns
        .iter()
        .any(|pattern| input_lower.contains(pattern))
}

/// Warning message for potentially sensitive input
pub fn get_sensitive_warning() -> &'static str {
    "⚠️  WARNING: Your message may contain sensitive information!\n\n\
    NEVER share:\n\
    • Private keys or seed phrases\n\
    • Passwords or API keys\n\
    • Secret tokens or credentials\n\n\
    Are you sure you want to send this message?"
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_empty() {
        match validate_input("") {
            ValidationResult::Empty => (),
            _ => panic!("Expected Empty"),
        }
    }

    #[test]
    fn test_validate_whitespace_only() {
        match validate_input("   \n  \t  ") {
            ValidationResult::OnlyWhitespace => (),
            _ => panic!("Expected OnlyWhitespace"),
        }
    }

    #[test]
    fn test_validate_normal() {
        match validate_input("Hello, world!") {
            ValidationResult::Valid(s) => assert_eq!(s, "Hello, world!"),
            _ => panic!("Expected Valid"),
        }
    }

    #[test]
    fn test_sanitize_removes_control_chars() {
        let input = "Hello\x00World\x01!";
        let output = sanitize_input(input);
        assert_eq!(output, "HelloWorld!");
    }

    #[test]
    fn test_truncate() {
        let text = "This is a long message";
        let truncated = truncate_for_display(text, 10);
        assert_eq!(truncated, "This is...");
    }

    #[test]
    fn test_is_command() {
        assert!(is_command("/help"));
        assert!(is_command("  /theme  "));
        assert!(!is_command("regular message"));
    }

    #[test]
    fn test_parse_command() {
        let (cmd, args) = parse_command("/theme switch dark").unwrap();
        assert_eq!(cmd, "theme");
        assert_eq!(args, vec!["switch", "dark"]);
    }

    #[test]
    fn test_sensitive_detection() {
        assert!(contains_sensitive_pattern("My private key is xyz"));
        assert!(contains_sensitive_pattern("seed phrase: word1 word2"));
        assert!(!contains_sensitive_pattern("Just a normal message"));
    }
}
