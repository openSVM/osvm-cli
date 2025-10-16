//! Golden ratio layout implementation for harmonious proportions

use cursive::direction::Orientation;
use cursive::views::{LinearLayout, Panel, ResizedView};
use cursive::View;
use cursive::Vec2;

/// Golden ratio constant (phi)
pub const GOLDEN_RATIO: f64 = 1.618033988749895;

/// Calculate golden ratio proportions for a given dimension
pub struct GoldenRatioCalculator;

impl GoldenRatioCalculator {
    /// Split a dimension into golden ratio proportions
    /// Returns (smaller, larger) where larger/smaller ≈ 1.618
    pub fn split(total: usize) -> (usize, usize) {
        let smaller = (total as f64 / GOLDEN_RATIO) as usize;
        let larger = total - smaller;
        (smaller, larger)
    }

    /// Calculate multiple golden ratio segments
    pub fn split_multiple(total: usize, segments: usize) -> Vec<usize> {
        if segments <= 1 {
            return vec![total];
        }

        let mut sizes = Vec::new();
        let mut remaining = total;

        for i in 0..segments - 1 {
            let (smaller, larger) = Self::split(remaining);
            if i % 2 == 0 {
                sizes.push(smaller);
                remaining = larger;
            } else {
                sizes.push(larger);
                remaining = smaller;
            }
        }
        sizes.push(remaining);

        sizes
    }

    /// Calculate golden rectangle dimensions
    pub fn golden_rectangle(width: usize) -> usize {
        (width as f64 / GOLDEN_RATIO) as usize
    }

    /// Create a golden spiral layout
    pub fn spiral_layout(size: Vec2) -> Vec<(usize, usize, usize, usize)> {
        let mut rectangles = Vec::new();
        let mut x = 0;
        let mut y = 0;
        let mut width = size.x;
        let mut height = size.y;

        for i in 0..5 {
            // Create 5 nested golden rectangles
            rectangles.push((x, y, width, height));

            let (smaller, larger) = Self::split(width.min(height));

            match i % 4 {
                0 => {
                    // Split horizontally, take right
                    x += smaller;
                    width = larger;
                }
                1 => {
                    // Split vertically, take bottom
                    y += smaller;
                    height = larger;
                }
                2 => {
                    // Split horizontally, take left
                    width = smaller;
                }
                3 => {
                    // Split vertically, take top
                    height = smaller;
                }
                _ => {}
            }

            if width < 10 || height < 10 {
                break; // Too small to continue
            }
        }

        rectangles
    }
}

/// Create a golden ratio based layout
pub fn create_golden_layout(
    size: Vec2,
    sidebar_content: impl View + 'static,
    main_content: impl View + 'static,
    input_content: impl View + 'static,
) -> LinearLayout {
    let (sidebar_width, main_width) = GoldenRatioCalculator::split(size.x);
    let (main_height, input_height) = GoldenRatioCalculator::split(size.y);

    let mut root = LinearLayout::horizontal();

    // Sidebar (smaller golden ratio part)
    root.add_child(
        ResizedView::with_fixed_width(
            sidebar_width,
            Panel::new(sidebar_content).title("φ Sessions"),
        ),
    );

    // Main area (larger golden ratio part)
    let mut main_layout = LinearLayout::vertical();

    // Chat area (larger part of vertical split)
    main_layout.add_child(
        ResizedView::with_fixed_height(main_height, Panel::new(main_content).title("φ Chat")),
    );

    // Input area (smaller part of vertical split)
    main_layout.add_child(
        ResizedView::with_fixed_height(input_height, Panel::new(input_content).title("φ Input")),
    );

    root.add_child(ResizedView::with_fixed_width(main_width, main_layout));

    root
}

/// Create a Fibonacci spiral layout for artistic presentation
pub fn create_spiral_layout(size: Vec2) -> LinearLayout {
    let mut layout = LinearLayout::vertical();

    let rectangles = GoldenRatioCalculator::spiral_layout(size);

    // Create nested panels following the spiral
    for (i, (x, y, w, h)) in rectangles.iter().enumerate() {
        let content = format!("Section {} ({}, {}) {}x{}", i + 1, x, y, w, h);
        layout.add_child(Panel::new(
            ResizedView::with_fixed_size((*w, *h), cursive::views::TextView::new(content)),
        ));
    }

    layout
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_golden_split() {
        let (smaller, larger) = GoldenRatioCalculator::split(100);
        let ratio = larger as f64 / smaller as f64;
        assert!((ratio - GOLDEN_RATIO).abs() < 0.1);
    }

    #[test]
    fn test_multiple_splits() {
        let segments = GoldenRatioCalculator::split_multiple(100, 3);
        assert_eq!(segments.len(), 3);
        assert_eq!(segments.iter().sum::<usize>(), 100);
    }
}