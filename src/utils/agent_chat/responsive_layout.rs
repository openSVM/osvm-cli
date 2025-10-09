//! Simple sequential layout system for agent_chat with clean terminal rendering
//!
//! This module provides utilities for rendering TUI components sequentially in the terminal
//! using simple println! statements that preserve text selection and normal terminal behavior.

use anyhow::Result;
use std::io::{stdout, Write};

/// Screen areas for different components with absolute positioning
#[derive(Debug, Clone)]
pub struct ComponentAreas {
    pub terminal_size: (u16, u16),
    pub status_bar: ComponentArea,
    pub task_panel: ComponentArea,
    pub chat_area: ComponentArea,
    pub input_field: ComponentArea,
    pub suggestions: ComponentArea,
}

/// Individual component area with position and size
#[derive(Debug, Clone)]
pub struct ComponentArea {
    pub x: u16,
    pub y: u16,
    pub width: u16,
    pub height: u16,
    pub visible: bool,
}

impl ComponentArea {
    pub fn new(x: u16, y: u16, width: u16, height: u16) -> Self {
        Self {
            x,
            y,
            width,
            height,
            visible: true,
        }
    }

    /// Check if area is valid for current terminal size
    pub fn is_valid(&self, terminal_size: (u16, u16)) -> bool {
        self.x + self.width <= terminal_size.0 && self.y + self.height <= terminal_size.1
    }
}

impl ComponentAreas {
    /// Calculate responsive component areas based on terminal size
    pub fn calculate(terminal_size: (u16, u16)) -> Self {
        let (cols, rows) = terminal_size;

        // Don't use complex overlapping layout - use simple sequential layout
        let mut current_y = 0u16;

        // Status bar at top (1 row with 1 row spacing)
        let status_bar = ComponentArea::new(0, current_y, cols, 1);
        current_y += 2; // Status bar + spacing

        // Task panel (fixed height, not overlapping)
        let task_panel_height = if rows >= 20 {
            6
        } else if rows >= 15 {
            4
        } else {
            0
        };
        let task_panel = if task_panel_height > 0 {
            let area = ComponentArea::new(0, current_y, cols, task_panel_height);
            current_y += task_panel_height + 1; // Task panel + spacing
            area
        } else {
            ComponentArea::new(0, 0, 0, 0) // Hidden
        };

        // Chat area takes most remaining space
        let remaining_rows = rows.saturating_sub(current_y).saturating_sub(4); // Reserve 4 for input/suggestions
        let chat_height = remaining_rows.max(2); // At least 2 rows
        let chat_area = ComponentArea::new(0, current_y, cols, chat_height);
        current_y += chat_height;

        // Suggestions (2 rows)
        let suggestions_height = 2;
        let suggestions = ComponentArea::new(0, current_y, cols, suggestions_height);
        current_y += suggestions_height;

        // Input field at bottom (1 row)
        let input_field = ComponentArea::new(0, current_y, cols, 1);

        Self {
            terminal_size,
            status_bar,
            task_panel: ComponentArea {
                visible: task_panel_height > 0,
                ..task_panel
            },
            chat_area,
            input_field,
            suggestions: ComponentArea {
                visible: false,
                ..suggestions
            }, // Hidden by default
        }
    }

    /// Minimal layout for very small terminals
    fn minimal_layout(terminal_size: (u16, u16)) -> Self {
        let (cols, rows) = terminal_size;

        // Only status bar and input field for tiny terminals
        let status_bar = ComponentArea::new(0, 0, cols, 1);
        let input_field = ComponentArea::new(0, rows.saturating_sub(2), cols, 2);

        // Hide other components
        let task_panel = ComponentArea::new(0, 0, 0, 0);
        let chat_area = ComponentArea::new(0, 1, cols, rows.saturating_sub(3));
        let suggestions = ComponentArea::new(0, 0, 0, 0);

        Self {
            terminal_size,
            status_bar,
            task_panel: ComponentArea {
                visible: false,
                ..task_panel
            },
            chat_area,
            input_field,
            suggestions: ComponentArea {
                visible: false,
                ..suggestions
            },
        }
    }

    /// Update layout for new terminal size
    pub fn update_for_size(&mut self, new_size: (u16, u16)) {
        *self = Self::calculate(new_size);
    }
}

/// Simple sequential terminal renderer that preserves text selection
pub struct TerminalRenderer {
    areas: ComponentAreas,
}

impl TerminalRenderer {
    /// Create new renderer - no complex positioning needed
    pub fn new() -> Result<Self> {
        Ok(Self {
            areas: ComponentAreas::calculate((80, 24)), // Default size
        })
    }

    /// Simple sequential rendering - just use println!
    pub fn render_component_in_place<F>(
        &mut self,
        _area: &ComponentArea,
        render_fn: F,
    ) -> Result<()>
    where
        F: FnOnce(&mut std::io::Stdout, &ComponentArea) -> Result<()>,
    {
        // No positioning needed - just render sequentially
        let dummy_area = ComponentArea::new(0, 0, 80, 10);
        render_fn(&mut stdout(), &dummy_area)?;
        stdout().flush()?;
        Ok(())
    }

    /// Get areas (simplified)
    pub fn areas(&self) -> &ComponentAreas {
        &self.areas
    }

    /// Handle resize - not needed for sequential rendering
    pub fn handle_resize(&mut self) -> Result<bool> {
        Ok(false)
    }
}

/// Text formatting utilities for responsive display
pub fn wrap_text_smart(text: &str, width: usize) -> Vec<String> {
    if width < 10 {
        // Very narrow - just split by characters
        return text
            .chars()
            .collect::<Vec<char>>()
            .chunks(width)
            .map(|chunk| chunk.iter().collect())
            .collect();
    }

    let mut lines = Vec::new();
    let mut current_line = String::new();

    for word in text.split_whitespace() {
        // If word itself is too long, split it
        if word.len() > width {
            if !current_line.is_empty() {
                lines.push(current_line.clone());
                current_line.clear();
            }

            // Split long word
            let chars: Vec<char> = word.chars().collect();
            for chunk in chars.chunks(width) {
                lines.push(chunk.iter().collect());
            }
            continue;
        }

        // Check if adding this word would exceed width
        let space_needed = if current_line.is_empty() { 0 } else { 1 }; // Space before word
        if current_line.len() + space_needed + word.len() > width {
            if !current_line.is_empty() {
                lines.push(current_line.clone());
                current_line.clear();
            }
        }

        if !current_line.is_empty() {
            current_line.push(' ');
        }
        current_line.push_str(word);
    }

    if !current_line.is_empty() {
        lines.push(current_line);
    }

    // Ensure we return at least one line (empty if needed)
    if lines.is_empty() {
        lines.push(String::new());
    }

    lines
}

/// Truncate text with ellipsis for narrow displays
pub fn truncate_with_ellipsis(text: &str, max_width: usize) -> String {
    if text.len() <= max_width {
        return text.to_string();
    }

    if max_width <= 3 {
        return "...".chars().take(max_width).collect();
    }

    let mut result = text.chars().take(max_width - 3).collect::<String>();
    result.push_str("...");
    result
}

/// Format text with color preservation for terminal compatibility
pub fn format_colored_text(text: &str, color: crossterm::style::Color) -> String {
    let color_code = match color {
        crossterm::style::Color::Red => "31",
        crossterm::style::Color::Green => "32",
        crossterm::style::Color::Yellow => "33",
        crossterm::style::Color::Blue => "34",
        crossterm::style::Color::Magenta => "35",
        crossterm::style::Color::Cyan => "36",
        crossterm::style::Color::White => "37",
        crossterm::style::Color::Grey => "90",
        _ => "37", // Default to white
    };
    format!("\x1b[{}m{}\x1b[0m", color_code, text)
}

/// Simple rendering that respects terminal scroll buffer
pub fn render_without_scroll<F>(render_fn: F) -> Result<()>
where
    F: FnOnce() -> Result<()>,
{
    // Just call the render function - no positioning needed
    render_fn()?;
    stdout().flush()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_component_areas_calculation() {
        let areas = ComponentAreas::calculate((100, 30));

        assert_eq!(areas.terminal_size, (100, 30));
        assert_eq!(areas.status_bar.height, 1);
        assert!(areas.task_panel.height > 0);
        assert_eq!(areas.input_field.height, 1); // Input field is always 1 row in the current implementation
        assert!(areas.chat_area.height > 0);
    }

    #[test]
    fn test_minimal_layout() {
        let areas = ComponentAreas::calculate((50, 8));

        // Should work even with small terminals
        assert!(!areas.task_panel.visible);
        assert!(!areas.suggestions.visible);
        assert!(areas.status_bar.visible);
        assert!(areas.input_field.visible);
    }

    #[test]
    fn test_text_wrapping() {
        let wrapped = wrap_text_smart("This is a long line of text that should be wrapped", 20);

        assert!(wrapped.len() > 1);
        for line in &wrapped {
            assert!(line.len() <= 20);
        }
    }

    #[test]
    fn test_text_truncation() {
        let truncated = truncate_with_ellipsis("This is a very long string", 15);
        assert_eq!(truncated.len(), 15);
        assert!(truncated.ends_with("..."));
    }
}
