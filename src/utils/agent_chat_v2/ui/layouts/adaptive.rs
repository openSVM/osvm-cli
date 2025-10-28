//! Adaptive layout that responds to terminal size changes

use cursive::direction::Orientation;
use cursive::views::{
    BoxedView, Dialog, DummyView, LinearLayout, Panel, ResizedView, ScrollView, TextView,
};
use cursive::Vec2;
use cursive::View;

/// Breakpoints for responsive design
pub struct Breakpoints;

impl Breakpoints {
    pub const MOBILE: usize = 60; // < 60 cols
    pub const TABLET: usize = 80; // 60-80 cols
    pub const DESKTOP: usize = 120; // 80-120 cols
    pub const WIDE: usize = 160; // 120-160 cols
    pub const ULTRAWIDE: usize = 200; // > 160 cols
}

/// Adaptive container that changes based on available space
pub struct AdaptiveContainer {
    content: Vec<Box<dyn View>>,
    current_layout: AdaptiveLayout,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum AdaptiveLayout {
    Stacked,   // All elements vertical
    TwoColumn, // Side by side
    ThreeColumn,
    Grid,    // 2x2 or 3x3 grid
    Masonry, // Pinterest-style
}

impl AdaptiveContainer {
    pub fn new() -> Self {
        Self {
            content: Vec::new(),
            current_layout: AdaptiveLayout::Stacked,
        }
    }

    pub fn add_child(&mut self, view: impl View + 'static) {
        self.content.push(Box::new(view));
    }

    pub fn build_for_size(&self, size: Vec2) -> Box<dyn View> {
        let layout = Self::determine_layout(size, self.content.len());

        match layout {
            AdaptiveLayout::Stacked => self.build_stacked(),
            AdaptiveLayout::TwoColumn => self.build_two_column(),
            AdaptiveLayout::ThreeColumn => self.build_three_column(),
            AdaptiveLayout::Grid => self.build_grid(),
            AdaptiveLayout::Masonry => self.build_masonry(),
        }
    }

    fn determine_layout(size: Vec2, num_items: usize) -> AdaptiveLayout {
        match size.x {
            w if w < Breakpoints::MOBILE => AdaptiveLayout::Stacked,
            w if w < Breakpoints::TABLET => {
                if num_items <= 2 {
                    AdaptiveLayout::Stacked
                } else {
                    AdaptiveLayout::TwoColumn
                }
            }
            w if w < Breakpoints::DESKTOP => AdaptiveLayout::TwoColumn,
            w if w < Breakpoints::WIDE => {
                if num_items >= 6 {
                    AdaptiveLayout::Grid
                } else {
                    AdaptiveLayout::ThreeColumn
                }
            }
            _ => {
                if num_items >= 9 {
                    AdaptiveLayout::Masonry
                } else {
                    AdaptiveLayout::ThreeColumn
                }
            }
        }
    }

    fn build_stacked(&self) -> Box<dyn View> {
        let mut layout = LinearLayout::vertical();
        for child in &self.content {
            layout.add_child(DummyView); // Placeholder for actual view cloning
        }
        Box::new(layout)
    }

    fn build_two_column(&self) -> Box<dyn View> {
        let mut layout = LinearLayout::horizontal();
        let mut left = LinearLayout::vertical();
        let mut right = LinearLayout::vertical();

        for (i, _child) in self.content.iter().enumerate() {
            if i % 2 == 0 {
                left.add_child(DummyView);
            } else {
                right.add_child(DummyView);
            }
        }

        layout.add_child(ResizedView::with_full_screen(left));
        layout.add_child(ResizedView::with_full_screen(right));
        Box::new(layout)
    }

    fn build_three_column(&self) -> Box<dyn View> {
        let mut layout = LinearLayout::horizontal();
        let mut columns = vec![
            LinearLayout::vertical(),
            LinearLayout::vertical(),
            LinearLayout::vertical(),
        ];

        for (i, _child) in self.content.iter().enumerate() {
            columns[i % 3].add_child(DummyView);
        }

        for col in columns {
            layout.add_child(ResizedView::with_full_screen(col));
        }
        Box::new(layout)
    }

    fn build_grid(&self) -> Box<dyn View> {
        // Build a 3x3 grid
        let mut rows = LinearLayout::vertical();
        let mut current_row = LinearLayout::horizontal();
        let items_per_row = 3;

        for (i, _child) in self.content.iter().enumerate() {
            current_row.add_child(DummyView);

            if (i + 1) % items_per_row == 0 {
                rows.add_child(current_row);
                current_row = LinearLayout::horizontal();
            }
        }

        // BUG-1009 fix: Use idiomatic empty check
        if !current_row.is_empty() {
            rows.add_child(current_row);
        }

        Box::new(rows)
    }

    fn build_masonry(&self) -> Box<dyn View> {
        // Pinterest-style layout with varying heights
        let mut layout = LinearLayout::horizontal();
        let num_columns = 4;
        let mut columns: Vec<LinearLayout> =
            (0..num_columns).map(|_| LinearLayout::vertical()).collect();

        // Distribute items to columns with least height (simplified)
        for (i, _child) in self.content.iter().enumerate() {
            columns[i % num_columns].add_child(DummyView);
        }

        for col in columns {
            layout.add_child(ResizedView::with_full_width(col));
        }

        Box::new(layout)
    }
}

/// Fluid container that scales content proportionally
pub struct FluidContainer {
    base_size: Vec2,
    min_size: Vec2,
    max_size: Vec2,
}

impl FluidContainer {
    pub fn new(base: Vec2) -> Self {
        Self {
            base_size: base,
            min_size: Vec2::new(base.x / 2, base.y / 2),
            max_size: Vec2::new(base.x * 2, base.y * 2),
        }
    }

    pub fn scale_for_terminal(&self, terminal_size: Vec2) -> Vec2 {
        let scale_x = terminal_size.x as f32 / self.base_size.x as f32;
        let scale_y = terminal_size.y as f32 / self.base_size.y as f32;
        let scale = scale_x.min(scale_y); // Maintain aspect ratio

        let new_width = (self.base_size.x as f32 * scale) as usize;
        let new_height = (self.base_size.y as f32 * scale) as usize;

        Vec2::new(
            new_width.max(self.min_size.x).min(self.max_size.x),
            new_height.max(self.min_size.y).min(self.max_size.y),
        )
    }
}

/// Collapsible sidebar that can be toggled
pub struct CollapsibleSidebar {
    expanded: bool,
    expanded_width: usize,
    collapsed_width: usize,
}

impl CollapsibleSidebar {
    pub fn new(expanded_width: usize) -> Self {
        Self {
            expanded: true,
            expanded_width,
            collapsed_width: 5, // Just show icons
        }
    }

    pub fn toggle(&mut self) {
        self.expanded = !self.expanded;
    }

    pub fn current_width(&self) -> usize {
        if self.expanded {
            self.expanded_width
        } else {
            self.collapsed_width
        }
    }

    pub fn build(&self, content: impl View + 'static) -> Box<dyn View> {
        let width = self.current_width();
        let title = if self.expanded { "◂ Sessions" } else { "▸" };

        Box::new(ResizedView::with_fixed_width(
            width,
            Panel::new(content).title(title),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_adaptive_layout_selection() {
        let small = Vec2::new(50, 30);
        let layout = AdaptiveContainer::determine_layout(small, 5);
        assert_eq!(layout, AdaptiveLayout::Stacked);

        let medium = Vec2::new(100, 40);
        let layout = AdaptiveContainer::determine_layout(medium, 5);
        assert_eq!(layout, AdaptiveLayout::TwoColumn);

        let large = Vec2::new(180, 50);
        let layout = AdaptiveContainer::determine_layout(large, 10);
        assert_eq!(layout, AdaptiveLayout::Masonry);
    }

    #[test]
    fn test_fluid_scaling() {
        let fluid = FluidContainer::new(Vec2::new(100, 50));

        let scaled = fluid.scale_for_terminal(Vec2::new(200, 100));
        assert_eq!(scaled, Vec2::new(200, 100));

        let scaled_small = fluid.scale_for_terminal(Vec2::new(50, 25));
        assert_eq!(scaled_small, Vec2::new(50, 25));
    }
}
