use termimad::{MadSkin};

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
    pub fn with_theme() -> Self {
        // For now, use default theme since termimad color API is complex
        // TODO: Implement proper color configuration once termimad usage is clarified
        let skin = MadSkin::default();
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