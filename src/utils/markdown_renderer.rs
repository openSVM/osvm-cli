use termimad::MadSkin;

/// A markdown renderer for CLI output
#[derive(Debug, Clone)]
pub struct MarkdownRenderer {
    skin: MadSkin,
}

impl MarkdownRenderer {
    /// Create a new markdown renderer with default styling
    pub fn new() -> Self {
        let skin = MadSkin::default();
        Self { skin }
    }

    /// Create a new markdown renderer with custom theme configuration
    /// Optimized for blockchain analysis reports
    pub fn with_theme() -> Self {
        let mut skin = MadSkin::default();

        // Enhance colors for better readability in blockchain reports
        // Headers stand out for sections like "Wallet Activity", "Risk Assessment"
        skin.bold.set_fg(termimad::crossterm::style::Color::Cyan);
        skin.italic
            .set_fg(termimad::crossterm::style::Color::Yellow);

        // Code blocks for wallet addresses, transaction IDs
        skin.code_block
            .set_fg(termimad::crossterm::style::Color::Green);
        skin.inline_code
            .set_fg(termimad::crossterm::style::Color::Green);

        Self { skin }
    }

    /// Render markdown text to the terminal
    /// Note: termimad's print_text never actually fails, so we return unit type
    pub fn render(&self, markdown: &str) {
        self.skin.print_text(markdown);
    }

    /// Render markdown text and return as string (for testing)
    pub fn render_to_string(&self, markdown: &str) -> String {
        let fmt_text = self.skin.text(markdown, None);
        fmt_text.to_string()
    }
}

impl Default for MarkdownRenderer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_markdown_renderer_creation() {
        let renderer = MarkdownRenderer::new();
        let result = renderer.render_to_string("# Test");
        assert!(result.contains("Test"));
    }

    #[test]
    fn test_basic_markdown_rendering() {
        let renderer = MarkdownRenderer::new();
        let markdown = "# Header\nSome **bold** text\n```rust\nlet x = 5;\n```";
        let output = renderer.render_to_string(markdown);
        assert!(output.contains("Header"));
    }

    #[test]
    fn test_themed_renderer() {
        let renderer = MarkdownRenderer::with_theme();
        let result = renderer.render_to_string("# Themed Test");
        assert!(result.contains("Themed Test"));
    }
}
