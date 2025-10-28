//! Micro-interactions that make the UI feel alive and responsive

use super::{EffectIntensity, VisualEffect};
use std::time::Instant;

/// Button press effect with visual feedback
pub struct ButtonPressEffect {
    is_pressed: bool,
    press_depth: f32,
    release_spring: f32,
}

impl ButtonPressEffect {
    pub fn new() -> Self {
        Self {
            is_pressed: false,
            press_depth: 0.0,
            release_spring: 10.0,
        }
    }

    pub fn press(&mut self) {
        self.is_pressed = true;
        self.press_depth = 1.0;
    }

    pub fn release(&mut self) {
        self.is_pressed = false;
    }

    pub fn get_offset(&self) -> (i32, i32) {
        let x_offset = (self.press_depth * 2.0) as i32;
        let y_offset = (self.press_depth) as i32;
        (x_offset, y_offset)
    }

    pub fn get_shadow_char(&self) -> char {
        if self.press_depth > 0.7 {
            '▓'
        } else if self.press_depth > 0.3 {
            '▒'
        } else if self.press_depth > 0.0 {
            '░'
        } else {
            ' '
        }
    }
}

impl VisualEffect for ButtonPressEffect {
    fn update(&mut self, delta: f32) {
        if self.is_pressed {
            // Quick press
            self.press_depth = (self.press_depth + delta * 5.0).min(1.0);
        } else {
            // Spring back
            self.press_depth = (self.press_depth - delta * self.release_spring).max(0.0);
        }
    }

    fn render(&self) -> String {
        let shadow = self.get_shadow_char();
        let (x, y) = self.get_offset();
        format!(
            "{}{}[{}]",
            " ".repeat(x as usize),
            shadow,
            if self.is_pressed { "▼" } else { "▶" }
        )
    }

    fn is_active(&self) -> bool {
        self.is_pressed || self.press_depth > 0.0
    }

    fn reset(&mut self) {
        self.is_pressed = false;
        self.press_depth = 0.0;
    }
}

/// Hover effect for interactive elements
pub struct HoverEffect {
    is_hovering: bool,
    glow: f32,
    scale: f32,
}

impl HoverEffect {
    pub fn new() -> Self {
        Self {
            is_hovering: false,
            glow: 0.0,
            scale: 1.0,
        }
    }

    pub fn set_hover(&mut self, hovering: bool) {
        self.is_hovering = hovering;
    }

    pub fn get_decoration(&self) -> (String, String) {
        if self.glow > 0.5 {
            ("▶ ".to_string(), " ◀".to_string())
        } else if self.glow > 0.0 {
            ("> ".to_string(), " <".to_string())
        } else {
            ("  ".to_string(), "  ".to_string())
        }
    }

    pub fn get_highlight_level(&self) -> u8 {
        (self.glow * 100.0) as u8
    }
}

impl VisualEffect for HoverEffect {
    fn update(&mut self, delta: f32) {
        if self.is_hovering {
            self.glow = (self.glow + delta * 3.0).min(1.0);
            self.scale = (self.scale + delta * 0.5).min(1.1);
        } else {
            self.glow = (self.glow - delta * 5.0).max(0.0);
            self.scale = (self.scale - delta).max(1.0);
        }
    }

    fn render(&self) -> String {
        let (left, right) = self.get_decoration();
        format!(
            "{}{}{}",
            left,
            "═".repeat((self.scale * 10.0) as usize),
            right
        )
    }

    fn is_active(&self) -> bool {
        self.is_hovering || self.glow > 0.0
    }

    fn reset(&mut self) {
        self.is_hovering = false;
        self.glow = 0.0;
        self.scale = 1.0;
    }
}

/// Typing feedback for input fields
pub struct TypingFeedback {
    last_keystroke: Instant,
    cursor_phase: f32,
    typing_speed: f32,
    ripples: Vec<KeyRipple>,
}

struct KeyRipple {
    age: f32,
    char: char,
}

impl TypingFeedback {
    pub fn new() -> Self {
        Self {
            last_keystroke: Instant::now(),
            cursor_phase: 0.0,
            typing_speed: 0.0,
            ripples: Vec::new(),
        }
    }

    pub fn on_keystroke(&mut self, ch: char) {
        self.last_keystroke = Instant::now();
        self.typing_speed = 1.0;

        // Add ripple effect
        self.ripples.push(KeyRipple { age: 0.0, char: ch });

        // Keep only recent ripples
        if self.ripples.len() > 3 {
            self.ripples.remove(0);
        }
    }

    pub fn get_cursor(&self) -> char {
        let blink_speed = 1.0 + self.typing_speed * 2.0;
        let blink = (self.cursor_phase * blink_speed).sin() > 0.0;

        if self.typing_speed > 0.5 {
            if blink {
                '▌'
            } else {
                '▐'
            } // Fast blinking when typing
        } else {
            if blink {
                '█'
            } else {
                '▁'
            } // Slow pulse when idle
        }
    }

    pub fn get_ripple_chars(&self) -> Vec<String> {
        self.ripples
            .iter()
            .map(|r| {
                let opacity = 1.0 - r.age;
                if opacity > 0.7 {
                    format!("({})", r.char)
                } else if opacity > 0.3 {
                    format!("[{}]", r.char)
                } else {
                    format!(" {} ", r.char)
                }
            })
            .collect()
    }
}

impl VisualEffect for TypingFeedback {
    fn update(&mut self, delta: f32) {
        // Update cursor blink
        self.cursor_phase += delta;
        if self.cursor_phase > std::f32::consts::PI * 2.0 {
            self.cursor_phase -= std::f32::consts::PI * 2.0;
        }

        // Decay typing speed
        self.typing_speed = (self.typing_speed - delta * 0.5).max(0.0);

        // Age ripples
        self.ripples.retain_mut(|r| {
            r.age += delta * 2.0;
            r.age < 1.0
        });
    }

    fn render(&self) -> String {
        format!("{} {}", self.get_cursor(), self.get_ripple_chars().join(""))
    }

    fn is_active(&self) -> bool {
        true // Always show cursor
    }

    fn reset(&mut self) {
        self.typing_speed = 0.0;
        self.ripples.clear();
    }
}

/// Scroll momentum for smooth scrolling
pub struct ScrollMomentum {
    velocity: f32,
    position: f32,
    target_position: f32,
    friction: f32,
}

impl ScrollMomentum {
    pub fn new() -> Self {
        Self {
            velocity: 0.0,
            position: 0.0,
            target_position: 0.0,
            friction: 0.9,
        }
    }

    pub fn scroll_to(&mut self, target: f32) {
        self.target_position = target;
        let distance = target - self.position;
        self.velocity = distance * 0.2; // Initial velocity based on distance
    }

    pub fn flick(&mut self, velocity: f32) {
        self.velocity = velocity;
    }

    pub fn get_position(&self) -> f32 {
        self.position
    }

    pub fn get_scroll_indicator(&self) -> char {
        if self.velocity.abs() > 0.5 {
            '⣿' // Fast scrolling
        } else if self.velocity.abs() > 0.1 {
            '⡿' // Medium scrolling
        } else {
            '⠿' // Slow/stopped
        }
    }
}

impl VisualEffect for ScrollMomentum {
    fn update(&mut self, delta: f32) {
        // Apply velocity
        self.position += self.velocity * delta * 60.0; // 60 FPS base

        // Apply friction
        self.velocity *= self.friction;

        // Snap to target when close
        let distance = self.target_position - self.position;
        if distance.abs() > 0.1 {
            self.velocity += distance * 0.1; // Spring force
        } else if self.velocity.abs() < 0.01 {
            self.position = self.target_position;
            self.velocity = 0.0;
        }
    }

    fn render(&self) -> String {
        let indicator = self.get_scroll_indicator();
        format!("{} {:.1}", indicator, self.position)
    }

    fn is_active(&self) -> bool {
        self.velocity.abs() > 0.001
    }

    fn reset(&mut self) {
        self.velocity = 0.0;
        self.position = 0.0;
        self.target_position = 0.0;
    }
}

/// Selection feedback with visual highlighting
pub struct SelectionFeedback {
    selected_items: Vec<String>,
    selection_glow: f32,
    multi_select: bool,
}

impl SelectionFeedback {
    pub fn new() -> Self {
        Self {
            selected_items: Vec::new(),
            selection_glow: 0.0,
            multi_select: false,
        }
    }

    pub fn select(&mut self, item: String) {
        if self.multi_select {
            if !self.selected_items.contains(&item) {
                self.selected_items.push(item);
            }
        } else {
            self.selected_items.clear();
            self.selected_items.push(item);
        }
        self.selection_glow = 1.0;
    }

    pub fn deselect(&mut self, item: &str) {
        self.selected_items.retain(|i| i != item);
    }

    pub fn is_selected(&self, item: &str) -> bool {
        self.selected_items.contains(&item.to_string())
    }

    pub fn get_selection_decoration(&self, item: &str) -> String {
        if self.is_selected(item) {
            let glow_char = if self.selection_glow > 0.7 {
                "◉"
            } else if self.selection_glow > 0.3 {
                "◎"
            } else {
                "○"
            };
            format!("{} ", glow_char)
        } else {
            "  ".to_string()
        }
    }
}

impl VisualEffect for SelectionFeedback {
    fn update(&mut self, delta: f32) {
        // Fade glow
        self.selection_glow = (self.selection_glow - delta * 0.5).max(0.0);
    }

    fn render(&self) -> String {
        let count = self.selected_items.len();
        if count > 0 {
            format!("[{} selected]", count)
        } else {
            String::new()
        }
    }

    fn is_active(&self) -> bool {
        !self.selected_items.is_empty()
    }

    fn reset(&mut self) {
        self.selected_items.clear();
        self.selection_glow = 0.0;
    }
}

/// Drag feedback for draggable elements
pub struct DragFeedback {
    is_dragging: bool,
    drag_offset: (f32, f32),
    ghost_opacity: f32,
}

impl DragFeedback {
    pub fn new() -> Self {
        Self {
            is_dragging: false,
            drag_offset: (0.0, 0.0),
            ghost_opacity: 0.0,
        }
    }

    pub fn start_drag(&mut self, x: f32, y: f32) {
        self.is_dragging = true;
        self.drag_offset = (x, y);
        self.ghost_opacity = 0.5;
    }

    pub fn update_drag(&mut self, x: f32, y: f32) {
        self.drag_offset = (x, y);
    }

    pub fn end_drag(&mut self) {
        self.is_dragging = false;
    }

    pub fn get_ghost_char(&self) -> char {
        if self.ghost_opacity > 0.7 {
            '█'
        } else if self.ghost_opacity > 0.4 {
            '▓'
        } else if self.ghost_opacity > 0.1 {
            '▒'
        } else {
            '░'
        }
    }
}

impl VisualEffect for DragFeedback {
    fn update(&mut self, delta: f32) {
        if self.is_dragging {
            self.ghost_opacity = (self.ghost_opacity + delta).min(0.8);
        } else {
            self.ghost_opacity = (self.ghost_opacity - delta * 2.0).max(0.0);
        }
    }

    fn render(&self) -> String {
        if self.is_dragging {
            format!(
                "↔ ({:.0}, {:.0}) {}",
                self.drag_offset.0,
                self.drag_offset.1,
                self.get_ghost_char()
            )
        } else {
            String::new()
        }
    }

    fn is_active(&self) -> bool {
        self.is_dragging || self.ghost_opacity > 0.0
    }

    fn reset(&mut self) {
        self.is_dragging = false;
        self.drag_offset = (0.0, 0.0);
        self.ghost_opacity = 0.0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_button_press() {
        let mut button = ButtonPressEffect::new();

        button.press();
        assert!(button.is_active());

        button.update(0.1);
        assert!(button.press_depth > 0.0);

        button.release();
        button.update(0.1);
        assert!(button.press_depth < 1.0);
    }

    #[test]
    fn test_typing_feedback() {
        let mut typing = TypingFeedback::new();

        typing.on_keystroke('H');
        typing.on_keystroke('i');

        assert_eq!(typing.ripples.len(), 2);

        // Age ripples
        for _ in 0..10 {
            typing.update(0.1);
        }

        assert!(typing.ripples.len() < 2); // Some ripples should have aged out
    }
}
