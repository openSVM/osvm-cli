//! Markdown rendering utilities for text display

use regex::Regex;

/// Render markdown text for display in TUI
pub fn render_markdown(text: &str) -> String {
    let mut rendered = text.to_string();

    // Convert markdown headers
    rendered = rendered.replace("### ", "═══ ");
    rendered = rendered.replace("## ", "══ ");
    rendered = rendered.replace("# ", "═ ");

    // Convert bold text
    if let Ok(bold_re) = Regex::new(r"\*\*([^*]+)\*\*") {
        rendered = bold_re.replace_all(&rendered, "\x1b[1m$1\x1b[0m").to_string();
    }

    // Convert italic text
    if let Ok(italic_re) = Regex::new(r"\*([^*]+)\*") {
        rendered = italic_re.replace_all(&rendered, "\x1b[3m$1\x1b[0m").to_string();
    }

    // Convert code blocks
    if let Ok(code_block_re) = Regex::new(r"```([^`]*)```") {
        rendered = code_block_re.replace_all(&rendered, "\x1b[48;5;236m$1\x1b[0m").to_string();
    }

    // Convert inline code
    if let Ok(inline_code_re) = Regex::new(r"`([^`]+)`") {
        rendered = inline_code_re.replace_all(&rendered, "\x1b[7m$1\x1b[0m").to_string();
    }

    // Convert bullet points
    rendered = rendered.replace("\n- ", "\n  - ");
    rendered = rendered.replace("\n* ", "\n  * ");
    rendered = rendered.replace("\n+ ", "\n  + ");

    // Convert numbered lists
    for i in 1..=20 {
        rendered = rendered.replace(&format!("\n{}. ", i), &format!("\n  {}. ", i));
    }

    // Convert blockquotes
    if let Ok(quote_re) = Regex::new(r"(?m)^> (.*)$") {
        rendered = quote_re.replace_all(&rendered, "  ┃ $1").to_string();
    }

    // Convert horizontal rules
    rendered = rendered.replace("\n---\n", "\n───────────────────────\n");
    rendered = rendered.replace("\n***\n", "\n═══════════════════════\n");

    rendered
}