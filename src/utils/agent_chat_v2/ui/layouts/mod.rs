//! Advanced layout system with golden ratio, adaptive sizing, and smooth transitions

use cursive::views::{LinearLayout, ResizedView};
use cursive::View;
use cursive::{Cursive, Vec2};

pub mod adaptive;
pub mod golden_ratio;

/// Layout configuration based on terminal size
#[derive(Debug, Clone)]
pub struct LayoutConfig {
    pub sidebar_width: usize,
    pub input_height: usize,
    pub status_bar_height: usize,
    pub padding: usize,
    pub layout_type: LayoutType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LayoutType {
    Compact,     // < 80 cols: Single column, collapsible sections
    Standard,    // 80-120 cols: Two column
    Wide,        // > 120 cols: Three column with tools panel
    UltraWide,   // > 160 cols: Four column with multiple panels
    GoldenRatio, // Golden ratio proportions
    Fibonacci,   // Fibonacci sequence proportions
    Custom,      // User-defined proportions
}

/// Main layout manager that handles responsive design
pub struct LayoutManager {
    current_config: LayoutConfig,
    terminal_size: Vec2,
    animations_enabled: bool,
}

impl LayoutManager {
    pub fn new(terminal_size: Vec2) -> Self {
        let config = Self::calculate_config(terminal_size);
        Self {
            current_config: config,
            terminal_size,
            animations_enabled: true,
        }
    }

    /// Calculate optimal layout based on terminal size
    pub fn calculate_config(size: Vec2) -> LayoutConfig {
        let (width, height) = (size.x, size.y);

        // Determine layout type
        let layout_type = match (width, height) {
            (w, h) if w < 60 || h < 20 => LayoutType::Compact,
            (w, _) if w < 80 => LayoutType::Standard,
            (w, _) if w < 120 => LayoutType::Wide,
            (w, h) if w >= 160 && h >= 40 => LayoutType::UltraWide,
            (w, h) if w >= 100 && h >= 30 => LayoutType::GoldenRatio,
            _ => LayoutType::Standard,
        };

        // Calculate dimensions based on layout type
        let (sidebar_width, input_height, status_bar_height, padding) = match layout_type {
            LayoutType::Compact => {
                (0, 3, 0, 0) // No sidebar, minimal input, no status bar
            }
            LayoutType::Standard => {
                (25, 5, 1, 1) // Narrow sidebar, standard input
            }
            LayoutType::Wide => {
                (30, 6, 1, 2) // Normal sidebar, comfortable input
            }
            LayoutType::UltraWide => {
                (35, 8, 2, 3) // Wide sidebar, spacious input, double status
            }
            LayoutType::GoldenRatio => {
                // Use golden ratio for proportions
                let sidebar = (width as f64 * 0.382) as usize; // 38.2%
                let input = (height as f64 * 0.236) as usize; // 23.6%
                (sidebar, input, 1, 2)
            }
            LayoutType::Fibonacci => {
                // Use Fibonacci sequence for nested proportions
                let sidebar = Self::fibonacci_width(width, 5); // 5th number
                let input = Self::fibonacci_height(height, 3); // 3rd number
                (sidebar, input, 1, 1)
            }
            LayoutType::Custom => {
                // Default custom layout
                (30, 5, 1, 2)
            }
        };

        LayoutConfig {
            sidebar_width,
            input_height,
            status_bar_height,
            padding,
            layout_type,
        }
    }

    /// Calculate width using Fibonacci sequence
    fn fibonacci_width(total: usize, n: usize) -> usize {
        let fib_sum: usize = 1 + 1 + 2 + 3 + 5 + 8 + 13 + 21; // Sum of first 8
        let fib_n = Self::fibonacci(n);
        (total as f64 * (fib_n as f64 / fib_sum as f64)) as usize
    }

    /// Calculate height using Fibonacci sequence
    fn fibonacci_height(total: usize, n: usize) -> usize {
        let fib_n = Self::fibonacci(n);
        (total as f64 * (fib_n as f64 / 21.0)) as usize // 21 is reasonable max
    }

    /// Get nth Fibonacci number
    fn fibonacci(n: usize) -> usize {
        match n {
            0 => 0,
            1 => 1,
            2 => 1,
            3 => 2,
            4 => 3,
            5 => 5,
            6 => 8,
            7 => 13,
            8 => 21,
            _ => 21, // Cap at 21 for practical use
        }
    }

    /// Update layout when terminal is resized
    pub fn on_resize(&mut self, new_size: Vec2) -> bool {
        let new_config = Self::calculate_config(new_size);

        if new_config.layout_type != self.current_config.layout_type {
            // Layout type changed, need full rebuild
            self.terminal_size = new_size;
            self.current_config = new_config;
            true // Signal that UI needs rebuild
        } else {
            // Just dimension changes, can animate
            self.animate_transition(new_config);
            false
        }
    }

    /// Animate transition between layouts
    fn animate_transition(&mut self, new_config: LayoutConfig) {
        if !self.animations_enabled {
            self.current_config = new_config;
            return;
        }

        // FUTURE: Implement smooth animation (requires animation framework integration)
        // For now, just update directly
        self.current_config = new_config;
    }

    /// Get current layout configuration
    pub fn config(&self) -> &LayoutConfig {
        &self.current_config
    }

    /// Check if animations are enabled
    pub fn animations_enabled(&self) -> bool {
        self.animations_enabled
    }

    /// Toggle animations
    pub fn set_animations(&mut self, enabled: bool) {
        self.animations_enabled = enabled;
    }
}

/// Helper functions for creating responsive views
pub mod responsive {
    use super::*;
    use cursive::views::{Panel, ScrollView, TextView};

    /// Create a responsive sidebar that adapts to width
    pub fn sidebar(width: usize, content: impl View + 'static) -> Box<dyn View> {
        if width == 0 {
            // Hidden sidebar
            Box::new(TextView::new(""))
        } else if width < 20 {
            // Ultra-narrow: Icons only
            Box::new(ResizedView::with_fixed_width(
                width,
                ScrollView::new(content),
            ))
        } else if width < 30 {
            // Narrow: Abbreviated content
            Box::new(ResizedView::with_fixed_width(
                width,
                Panel::new(ScrollView::new(content)).title("◂"), // Collapsed indicator
            ))
        } else {
            // Normal: Full content
            Box::new(ResizedView::with_fixed_width(
                width,
                Panel::new(ScrollView::new(content)).title("Sessions"),
            ))
        }
    }

    /// Create responsive input area that adapts to height
    pub fn input_area(height: usize, content: impl View + 'static) -> Box<dyn View> {
        if height <= 3 {
            // Single line input
            Box::new(ResizedView::with_fixed_height(height, content))
        } else if height <= 5 {
            // Multi-line without scroll
            Box::new(ResizedView::with_fixed_height(height, content))
        } else {
            // Multi-line with scroll
            Box::new(ResizedView::with_fixed_height(
                height,
                ScrollView::new(content),
            ))
        }
    }

    /// Create adaptive padding based on available space
    pub fn padding(size: usize) -> Box<dyn View> {
        use cursive::views::DummyView;

        match size {
            0 => Box::new(DummyView),
            _ => Box::new(ResizedView::with_fixed_width(size, DummyView)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_layout_detection() {
        // Compact layout
        let config = LayoutManager::calculate_config(Vec2::new(50, 20));
        assert_eq!(config.layout_type, LayoutType::Compact);

        // Standard layout
        let config = LayoutManager::calculate_config(Vec2::new(80, 30));
        assert_eq!(config.layout_type, LayoutType::Standard);

        // Golden ratio layout
        let config = LayoutManager::calculate_config(Vec2::new(120, 40));
        assert_eq!(config.layout_type, LayoutType::GoldenRatio);
    }

    #[test]
    fn test_golden_ratio_calculation() {
        let config = LayoutManager::calculate_config(Vec2::new(100, 50));
        if config.layout_type == LayoutType::GoldenRatio {
            // Sidebar should be ~38.2% of width
            assert!((config.sidebar_width as f64 - 38.2).abs() < 2.0);
            // Input should be ~23.6% of height
            assert!((config.input_height as f64 - 11.8).abs() < 2.0);
        }
    }
}
