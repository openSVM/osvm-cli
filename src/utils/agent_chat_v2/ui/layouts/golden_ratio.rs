//! Golden ratio layout implementation for harmonious proportions

use cursive::direction::Orientation;
use cursive::views::{LinearLayout, Panel, ResizedView};
use cursive::Vec2;
use cursive::View;

/// Golden ratio constant (phi)
pub const GOLDEN_RATIO: f64 = 1.618033988749895;

/// Calculate golden ratio proportions for a given dimension
pub struct GoldenRatioCalculator;

impl GoldenRatioCalculator {
    /// Split a dimension into golden ratio proportions
    /// Returns (smaller, larger) where larger/smaller ≈ 1.618
    pub fn split(total: usize) -> (usize, usize) {
        // For golden ratio: total = smaller + larger, and larger/smaller = phi
        // Therefore: total = smaller * (1 + phi), so smaller = total / (1 + phi)
        let smaller = (total as f64 / (GOLDEN_RATIO + 1.0)) as usize;
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
    root.add_child(ResizedView::with_fixed_width(
        sidebar_width,
        Panel::new(sidebar_content).title("φ Sessions"),
    ));

    // Main area (larger golden ratio part)
    let mut main_layout = LinearLayout::vertical();

    // Chat area (larger part of vertical split)
    main_layout.add_child(ResizedView::with_fixed_height(
        main_height,
        Panel::new(main_content).title("φ Chat"),
    ));

    // Input area (smaller part of vertical split)
    main_layout.add_child(ResizedView::with_fixed_height(
        input_height,
        Panel::new(input_content).title("φ Input"),
    ));

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
        layout.add_child(Panel::new(ResizedView::with_fixed_size(
            (*w, *h),
            cursive::views::TextView::new(content),
        )));
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

    // Property-based test: Golden ratio formula holds for any valid input
    #[cfg(test)]
    mod property_tests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            /// Property: For any total > 0, smaller + larger = total
            #[test]
            fn prop_golden_split_sums_to_total(total in 1usize..10000) {
                let (smaller, larger) = GoldenRatioCalculator::split(total);
                prop_assert_eq!(smaller + larger, total,
                    "smaller ({}) + larger ({}) must equal total ({})",
                    smaller, larger, total);
            }

            /// Property: For any total > 50, larger/smaller ≈ φ (within 10% tolerance)
            ///
            /// NOTE: Integer quantization causes cyclical error patterns. Empirical analysis:
            /// - total=50: 0.84% error (sweet spot)
            /// - total=51: 4.09% error
            /// - total=52: 7.34% error (WORST CASE in 50-100 range)
            /// - total=57: 5.95% error (proptest found this)
            /// - total=100,150,200: 0.84% error (sweet spots repeat)
            /// - total=1000: 0.41% error (converges as numbers grow)
            ///
            /// For UI layout dimensions (50-1000 pixels), 10% tolerance accommodates the
            /// worst-case quantization while still catching major formula bugs.
            #[test]
            fn prop_golden_ratio_is_accurate(total in 50usize..10000) {
                let (smaller, larger) = GoldenRatioCalculator::split(total);
                let ratio = larger as f64 / smaller as f64;
                let error = (ratio - GOLDEN_RATIO).abs() / GOLDEN_RATIO;
                prop_assert!(error < 0.10,
                    "Ratio {} should be within 10% of golden ratio {}, got {}% error\n\
                     (Note: Integer quantization causes cyclical error patterns up to ~7% in range 50-100)",
                    ratio, GOLDEN_RATIO, error * 100.0);
            }

            /// Property: Split is monotonic (larger total → larger parts)
            #[test]
            fn prop_split_is_monotonic(total1 in 10usize..5000, delta in 1usize..5000) {
                let total2 = total1 + delta;
                let (small1, large1) = GoldenRatioCalculator::split(total1);
                let (small2, large2) = GoldenRatioCalculator::split(total2);
                prop_assert!(small2 >= small1);
                prop_assert!(large2 >= large1);
            }
        }
    }
}
