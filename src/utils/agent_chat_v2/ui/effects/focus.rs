//! Focus management and visual indicators for active elements

use cursive::theme::{Color, ColorStyle};
use super::{EffectIntensity, VisualEffect};

/// Focus indicator with animated borders
pub struct FocusIndicator {
    is_focused: bool,
    animation_phase: f32,
    border_style: BorderStyle,
    glow_intensity: f32,
}

#[derive(Clone, Copy)]
pub enum BorderStyle {
    Single,    // ┌─┐
    Double,    // ╔═╗
    Rounded,   // ╭─╮
    Bold,      // ┏━┓
    Dashed,    // ┌╌┐
    Glowing,   // With animation
}

impl FocusIndicator {
    pub fn new() -> Self {
        Self {
            is_focused: false,
            animation_phase: 0.0,
            border_style: BorderStyle::Single,
            glow_intensity: 0.0,
        }
    }

    pub fn set_focused(&mut self, focused: bool) {
        self.is_focused = focused;
        if focused {
            self.border_style = BorderStyle::Double;
            self.glow_intensity = 1.0;
        } else {
            self.border_style = BorderStyle::Single;
            self.glow_intensity = 0.0;
        }
    }

    pub fn get_border_chars(&self) -> BorderChars {
        match (self.border_style, self.is_focused) {
            (_, true) if self.animation_phase < 0.5 => BorderChars {
                top_left: '╔',
                top: '═',
                top_right: '╗',
                left: '║',
                right: '║',
                bottom_left: '╚',
                bottom: '═',
                bottom_right: '╝',
            },
            (BorderStyle::Rounded, _) => BorderChars {
                top_left: '╭',
                top: '─',
                top_right: '╮',
                left: '│',
                right: '│',
                bottom_left: '╰',
                bottom: '─',
                bottom_right: '╯',
            },
            (BorderStyle::Bold, _) => BorderChars {
                top_left: '┏',
                top: '━',
                top_right: '┓',
                left: '┃',
                right: '┃',
                bottom_left: '┗',
                bottom: '━',
                bottom_right: '┛',
            },
            _ => BorderChars {
                top_left: '┌',
                top: '─',
                top_right: '┐',
                left: '│',
                right: '│',
                bottom_left: '└',
                bottom: '─',
                bottom_right: '┘',
            },
        }
    }

    pub fn get_focus_color(&self) -> ColorStyle {
        if self.is_focused {
            // Pulsing bright color
            let intensity = (self.animation_phase.sin() * 0.3 + 0.7) * 255.0;
            ColorStyle::new(
                Color::Rgb(0, intensity as u8, 255),
                Color::Rgb(30, 30, 30),
            )
        } else {
            // Dim unfocused color
            ColorStyle::new(
                Color::Rgb(100, 100, 100),
                Color::Rgb(30, 30, 30),
            )
        }
    }
}

pub struct BorderChars {
    pub top_left: char,
    pub top: char,
    pub top_right: char,
    pub left: char,
    pub right: char,
    pub bottom_left: char,
    pub bottom: char,
    pub bottom_right: char,
}

impl VisualEffect for FocusIndicator {
    fn update(&mut self, delta: f32) {
        if self.is_focused {
            self.animation_phase += delta * 2.0;
            if self.animation_phase > std::f32::consts::PI * 2.0 {
                self.animation_phase -= std::f32::consts::PI * 2.0;
            }

            // Fade glow intensity
            self.glow_intensity = (self.glow_intensity + delta).min(1.0);
        } else {
            // Fade out when not focused
            self.glow_intensity = (self.glow_intensity - delta * 2.0).max(0.0);
        }
    }

    fn render(&self) -> String {
        let chars = self.get_border_chars();
        format!(
            "{}{}{}",
            chars.top_left,
            chars.top,
            chars.top_right
        )
    }

    fn is_active(&self) -> bool {
        true // Always active to show borders
    }

    fn reset(&mut self) {
        self.is_focused = false;
        self.animation_phase = 0.0;
        self.glow_intensity = 0.0;
    }
}

/// Tab order manager for keyboard navigation
pub struct FocusManager {
    focusable_elements: Vec<String>,
    current_index: usize,
    wrap_around: bool,
}

impl FocusManager {
    pub fn new() -> Self {
        Self {
            focusable_elements: Vec::new(),
            current_index: 0,
            wrap_around: true,
        }
    }

    pub fn register_element(&mut self, id: String) {
        if !self.focusable_elements.contains(&id) {
            self.focusable_elements.push(id);
        }
    }

    pub fn focus_next(&mut self) -> Option<&String> {
        if self.focusable_elements.is_empty() {
            return None;
        }

        self.current_index += 1;

        if self.current_index >= self.focusable_elements.len() {
            if self.wrap_around {
                self.current_index = 0;
            } else {
                self.current_index = self.focusable_elements.len() - 1;
            }
        }

        self.focusable_elements.get(self.current_index)
    }

    pub fn focus_previous(&mut self) -> Option<&String> {
        if self.focusable_elements.is_empty() {
            return None;
        }

        if self.current_index == 0 {
            if self.wrap_around {
                self.current_index = self.focusable_elements.len() - 1;
            }
        } else {
            self.current_index -= 1;
        }

        self.focusable_elements.get(self.current_index)
    }

    pub fn focus_element(&mut self, id: &str) -> bool {
        if let Some(index) = self.focusable_elements.iter().position(|e| e == id) {
            self.current_index = index;
            true
        } else {
            false
        }
    }

    pub fn current_focus(&self) -> Option<&String> {
        self.focusable_elements.get(self.current_index)
    }

    pub fn clear_focus(&mut self) {
        self.current_index = 0;
    }
}

/// Visual focus trail for smooth transitions
pub struct FocusTrail {
    trail_points: Vec<(f32, f32)>,
    max_points: usize,
    fade_speed: f32,
}

impl FocusTrail {
    pub fn new() -> Self {
        Self {
            trail_points: Vec::new(),
            max_points: 5,
            fade_speed: 2.0,
        }
    }

    pub fn add_point(&mut self, x: f32, y: f32) {
        self.trail_points.push((x, y));

        if self.trail_points.len() > self.max_points {
            self.trail_points.remove(0);
        }
    }

    pub fn render_trail(&self) -> Vec<(f32, f32, char)> {
        let trail_chars = ['·', '∘', '○', '◉', '●'];

        self.trail_points
            .iter()
            .enumerate()
            .map(|(i, &(x, y))| {
                let intensity = i as f32 / self.max_points as f32;
                let char_idx = (intensity * trail_chars.len() as f32) as usize;
                (x, y, trail_chars[char_idx.min(trail_chars.len() - 1)])
            })
            .collect()
    }
}

impl VisualEffect for FocusTrail {
    fn update(&mut self, delta: f32) {
        // Fade out old trail points
        if self.trail_points.len() > 1 {
            self.trail_points.remove(0);
        }
    }

    fn render(&self) -> String {
        let trail = self.render_trail();
        trail.iter()
            .map(|(_, _, ch)| ch.to_string())
            .collect::<Vec<_>>()
            .join("")
    }

    fn is_active(&self) -> bool {
        !self.trail_points.is_empty()
    }

    fn reset(&mut self) {
        self.trail_points.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_focus_indicator() {
        let mut indicator = FocusIndicator::new();

        indicator.set_focused(true);
        assert_eq!(indicator.is_focused, true);

        let chars = indicator.get_border_chars();
        assert_eq!(chars.top_left, '╔');
    }

    #[test]
    fn test_focus_manager() {
        let mut manager = FocusManager::new();

        manager.register_element("input".to_string());
        manager.register_element("button1".to_string());
        manager.register_element("button2".to_string());

        assert_eq!(manager.current_focus(), Some(&"input".to_string()));

        manager.focus_next();
        assert_eq!(manager.current_focus(), Some(&"button1".to_string()));
    }
}