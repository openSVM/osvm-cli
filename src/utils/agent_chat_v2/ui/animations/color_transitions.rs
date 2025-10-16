//! Smooth color transitions for visual feedback

use super::{easing, lerp_color, Animation, QualityLevel};
use cursive::theme::Color;

/// Smooth color fade between two colors
pub struct ColorFade {
    start_color: Color,
    end_color: Color,
    current_color: Color,
    progress: f32,
    duration: f32,
    easing_fn: EasingFunction,
}

#[derive(Clone, Copy)]
pub enum EasingFunction {
    Linear,
    EaseIn,
    EaseOut,
    EaseInOut,
    Bounce,
    Elastic,
}

impl ColorFade {
    pub fn new(start: Color, end: Color, duration: f32) -> Self {
        Self {
            start_color: start,
            end_color: end,
            current_color: start,
            progress: 0.0,
            duration,
            easing_fn: EasingFunction::EaseInOut,
        }
    }

    pub fn set_easing(&mut self, easing: EasingFunction) {
        self.easing_fn = easing;
    }

    pub fn get_color(&self) -> Color {
        self.current_color
    }

    pub fn reverse(&mut self) {
        std::mem::swap(&mut self.start_color, &mut self.end_color);
        self.progress = 1.0 - self.progress;
    }

    fn apply_easing(&self, t: f32) -> f32 {
        match self.easing_fn {
            EasingFunction::Linear => t,
            EasingFunction::EaseIn => easing::ease_in_quad(t),
            EasingFunction::EaseOut => easing::ease_out_quad(t),
            EasingFunction::EaseInOut => easing::ease_in_out_cubic(t),
            EasingFunction::Bounce => easing::ease_out_bounce(t),
            EasingFunction::Elastic => easing::ease_out_elastic(t),
        }
    }
}

impl Animation for ColorFade {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        self.progress = (self.progress + delta / self.duration).min(1.0);

        let eased_progress = self.apply_easing(self.progress);
        self.current_color = lerp_color(self.start_color, self.end_color, eased_progress);
    }

    fn is_complete(&self) -> bool {
        self.progress >= 1.0
    }

    fn render(&self) -> String {
        // Return color as hex string for display
        match self.current_color {
            Color::Rgb(r, g, b) => format!("#{:02x}{:02x}{:02x}", r, g, b),
            _ => String::from("#ffffff"),
        }
    }
}

/// Rainbow color cycle for special effects
pub struct RainbowCycle {
    hue: f32,
    speed: f32,
    saturation: f32,
    lightness: f32,
}

impl RainbowCycle {
    pub fn new() -> Self {
        Self {
            hue: 0.0,
            speed: 1.0,
            saturation: 1.0,
            lightness: 0.5,
        }
    }

    pub fn set_speed(&mut self, speed: f32) {
        self.speed = speed;
    }

    pub fn get_color(&self) -> Color {
        // Convert HSL to RGB
        let (r, g, b) = Self::hsl_to_rgb(self.hue, self.saturation, self.lightness);
        Color::Rgb(r, g, b)
    }

    fn hsl_to_rgb(h: f32, s: f32, l: f32) -> (u8, u8, u8) {
        let c = (1.0 - (2.0 * l - 1.0).abs()) * s;
        let x = c * (1.0 - ((h * 6.0) % 2.0 - 1.0).abs());
        let m = l - c / 2.0;

        let (r, g, b) = match (h * 6.0) as usize {
            0 => (c, x, 0.0),
            1 => (x, c, 0.0),
            2 => (0.0, c, x),
            3 => (0.0, x, c),
            4 => (x, 0.0, c),
            _ => (c, 0.0, x),
        };

        (
            ((r + m) * 255.0) as u8,
            ((g + m) * 255.0) as u8,
            ((b + m) * 255.0) as u8,
        )
    }
}

impl Animation for RainbowCycle {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        self.hue += delta * self.speed * 0.1; // Full cycle every 10 seconds at speed 1.0
        if self.hue > 1.0 {
            self.hue -= 1.0;
        }
    }

    fn is_complete(&self) -> bool {
        false // Runs forever
    }

    fn render(&self) -> String {
        let color = self.get_color();
        match color {
            Color::Rgb(r, g, b) => format!("#{:02x}{:02x}{:02x}", r, g, b),
            _ => String::from("#ffffff"),
        }
    }
}

/// Breathing/pulsing color effect
pub struct BreathingColor {
    base_color: Color,
    highlight_color: Color,
    phase: f32,
    speed: f32,
}

impl BreathingColor {
    pub fn new(base: Color, highlight: Color) -> Self {
        Self {
            base_color: base,
            highlight_color: highlight,
            phase: 0.0,
            speed: 1.0,
        }
    }

    pub fn get_color(&self) -> Color {
        let intensity = (self.phase * std::f32::consts::PI * 2.0).sin() * 0.5 + 0.5;
        lerp_color(self.base_color, self.highlight_color, intensity)
    }
}

impl Animation for BreathingColor {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        self.phase += delta * self.speed;
        if self.phase > 1.0 {
            self.phase -= 1.0;
        }
    }

    fn is_complete(&self) -> bool {
        false // Runs forever
    }

    fn render(&self) -> String {
        let color = self.get_color();
        match color {
            Color::Rgb(r, g, b) => format!("#{:02x}{:02x}{:02x}", r, g, b),
            _ => String::from("#ffffff"),
        }
    }
}

/// Flash effect for notifications
pub struct FlashEffect {
    base_color: Color,
    flash_color: Color,
    intensity: f32,
    decay_rate: f32,
}

impl FlashEffect {
    pub fn new(base: Color, flash: Color) -> Self {
        Self {
            base_color: base,
            flash_color: flash,
            intensity: 1.0,
            decay_rate: 3.0, // Decay in 1/3 second
        }
    }

    pub fn trigger(&mut self) {
        self.intensity = 1.0;
    }

    pub fn get_color(&self) -> Color {
        lerp_color(self.base_color, self.flash_color, self.intensity)
    }
}

impl Animation for FlashEffect {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        self.intensity = (self.intensity - delta * self.decay_rate).max(0.0);
    }

    fn is_complete(&self) -> bool {
        self.intensity <= 0.0
    }

    fn render(&self) -> String {
        let color = self.get_color();
        match color {
            Color::Rgb(r, g, b) => format!("#{:02x}{:02x}{:02x}", r, g, b),
            _ => String::from("#ffffff"),
        }
    }
}

/// Multi-color gradient animation
pub struct GradientAnimation {
    colors: Vec<Color>,
    position: f32,
    speed: f32,
}

impl GradientAnimation {
    pub fn new(colors: Vec<Color>) -> Self {
        Self {
            colors,
            position: 0.0,
            speed: 1.0,
        }
    }

    pub fn get_color_at(&self, normalized_position: f32) -> Color {
        if self.colors.is_empty() {
            return Color::Rgb(255, 255, 255);
        }

        if self.colors.len() == 1 {
            return self.colors[0];
        }

        // Wrap position with animation offset
        let pos = (normalized_position + self.position) % 1.0;
        let segment_size = 1.0 / (self.colors.len() - 1) as f32;
        let segment = (pos / segment_size) as usize;
        let segment_progress = (pos % segment_size) / segment_size;

        if segment >= self.colors.len() - 1 {
            return self.colors[self.colors.len() - 1];
        }

        lerp_color(self.colors[segment], self.colors[segment + 1], segment_progress)
    }

    pub fn render_gradient_bar(&self, width: usize) -> String {
        let mut bar = String::new();

        for i in 0..width {
            let pos = i as f32 / width as f32;
            let color = self.get_color_at(pos);

            // Convert to grayscale intensity for ASCII representation
            let intensity = match color {
                Color::Rgb(r, g, b) => {
                    (r as f32 * 0.299 + g as f32 * 0.587 + b as f32 * 0.114) / 255.0
                }
                _ => 0.5,
            };

            let ch = match (intensity * 8.0) as usize {
                0 => ' ',
                1 => '░',
                2 => '▒',
                3 => '▓',
                4 => '█',
                5 => '▓',
                6 => '▒',
                7 => '░',
                _ => ' ',
            };

            bar.push(ch);
        }

        bar
    }
}

impl Animation for GradientAnimation {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        self.position += delta * self.speed * 0.1;
        if self.position > 1.0 {
            self.position -= 1.0;
        }
    }

    fn is_complete(&self) -> bool {
        false // Runs forever
    }

    fn render(&self) -> String {
        self.render_gradient_bar(20)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_color_fade() {
        let mut fade = ColorFade::new(
            Color::Rgb(0, 0, 0),
            Color::Rgb(255, 255, 255),
            1.0,
        );

        // Simulate 1 second of updates
        for _ in 0..10 {
            fade.update(0.1, QualityLevel::High);
        }

        assert!(fade.is_complete());
    }

    #[test]
    fn test_rainbow_cycle() {
        let mut rainbow = RainbowCycle::new();

        for _ in 0..10 {
            rainbow.update(0.1, QualityLevel::High);
            let _ = rainbow.get_color();
        }

        assert!(!rainbow.is_complete()); // Should never complete
    }

    #[test]
    fn test_flash_effect() {
        let mut flash = FlashEffect::new(
            Color::Rgb(0, 0, 0),
            Color::Rgb(255, 255, 255),
        );

        flash.trigger();
        assert_eq!(flash.intensity, 1.0);

        // Simulate decay
        for _ in 0..10 {
            flash.update(0.1, QualityLevel::High);
        }

        assert!(flash.intensity < 0.1);
    }
}