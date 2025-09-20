use termimad::{MadSkin};

/// A markdown renderer for CLI output
pub struct MarkdownRenderer {
    skin: MadSkin,
}

impl MarkdownRenderer {
    /// Create a new markdown renderer with default styling
    pub fn new() -> Self {
        let skin = MadSkin::default();
        Self { skin }
    }

    /// Render markdown text to the terminal
    pub fn render(&self, markdown: &str) -> Result<(), Box<dyn std::error::Error>> {
        self.skin.print_text(markdown);
        Ok(())
    }

    /// Render markdown text and return as string (for testing)
    pub fn render_to_string(&self, markdown: &str) -> Result<String, Box<dyn std::error::Error>> {
        let fmt_text = self.skin.text(markdown, None);
        Ok(fmt_text.to_string())
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
        assert!(renderer.render_to_string("# Test").is_ok());
    }

    #[test]
    fn test_basic_markdown_rendering() {
        let renderer = MarkdownRenderer::new();
        let markdown = "# Header\nSome **bold** text\n```rust\nlet x = 5;\n```";
        let result = renderer.render_to_string(markdown);
        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.contains("Header"));
    }
}