//! Visual effects for focus, attention, and user feedback

use std::time::{Duration, Instant};

pub mod focus;
pub mod micro_interactions;
pub mod notifications;

/// Effect intensity levels
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EffectIntensity {
    Subtle,   // Barely noticeable
    Normal,   // Default level
    Strong,   // Very noticeable
    Dramatic, // Maximum impact
}

/// Base trait for all visual effects
pub trait VisualEffect: Send + Sync {
    fn update(&mut self, delta: f32);
    fn render(&self) -> String;
    fn is_active(&self) -> bool;
    fn reset(&mut self);
}

/// Shake effect for errors or attention
pub struct ShakeEffect {
    intensity: f32,
    duration: Duration,
    start_time: Instant,
    frequency: f32,
    is_active: bool,
}

impl ShakeEffect {
    pub fn new(intensity: EffectIntensity) -> Self {
        let (amplitude, freq) = match intensity {
            EffectIntensity::Subtle => (1.0, 10.0),
            EffectIntensity::Normal => (2.0, 15.0),
            EffectIntensity::Strong => (3.0, 20.0),
            EffectIntensity::Dramatic => (5.0, 30.0),
        };

        Self {
            intensity: amplitude,
            duration: Duration::from_millis(300),
            start_time: Instant::now(),
            frequency: freq,
            is_active: false,
        }
    }

    pub fn trigger(&mut self) {
        self.start_time = Instant::now();
        self.is_active = true;
    }

    pub fn get_offset(&self) -> i32 {
        if !self.is_active {
            return 0;
        }

        let elapsed = self.start_time.elapsed().as_secs_f32();
        let progress = elapsed / self.duration.as_secs_f32();

        if progress >= 1.0 {
            // Don't modify state in a getter - let update() handle it
            return 0;
        }

        // Dampen over time
        let damping = 1.0 - progress;
        let offset = (elapsed * self.frequency * std::f32::consts::PI * 2.0).sin()
            * self.intensity
            * damping;

        offset as i32
    }
}

impl VisualEffect for ShakeEffect {
    fn update(&mut self, _delta: f32) {
        if self.is_active && self.start_time.elapsed() >= self.duration {
            self.is_active = false;
        }
    }

    fn render(&self) -> String {
        let offset = self.get_offset();
        if offset > 0 {
            " ".repeat(offset.abs() as usize)
        } else {
            String::new()
        }
    }

    fn is_active(&self) -> bool {
        self.is_active
    }

    fn reset(&mut self) {
        self.is_active = false;
    }
}

/// Glow/highlight effect for important elements
pub struct GlowEffect {
    intensity: f32,
    phase: f32,
    speed: f32,
    is_pulsing: bool,
}

impl GlowEffect {
    pub fn new(intensity: EffectIntensity) -> Self {
        let strength = match intensity {
            EffectIntensity::Subtle => 0.3,
            EffectIntensity::Normal => 0.6,
            EffectIntensity::Strong => 0.85,
            EffectIntensity::Dramatic => 1.0,
        };

        Self {
            intensity: strength,
            phase: 0.0,
            speed: 2.0,
            is_pulsing: true,
        }
    }

    pub fn set_pulsing(&mut self, enabled: bool) {
        self.is_pulsing = enabled;
    }

    pub fn get_border_chars(&self) -> (&str, &str, &str, &str) {
        let glow_level = if self.is_pulsing {
            (self.phase.sin() * 0.5 + 0.5) * self.intensity
        } else {
            self.intensity
        };

        match (glow_level * 4.0) as usize {
            0 => ("┌", "┐", "└", "┘"),
            1 => ("╭", "╮", "╰", "╯"),
            2 => ("╔", "╗", "╚", "╝"),
            3 => ("◆", "◆", "◆", "◆"),
            _ => ("✦", "✦", "✦", "✦"),
        }
    }

    pub fn get_glow_color_offset(&self) -> u8 {
        let glow = if self.is_pulsing {
            (self.phase.sin() * 0.5 + 0.5) * self.intensity
        } else {
            self.intensity
        };

        (glow * 50.0) as u8 // Brightness offset for RGB
    }
}

impl VisualEffect for GlowEffect {
    fn update(&mut self, delta: f32) {
        if self.is_pulsing {
            self.phase += delta * self.speed;
            if self.phase > std::f32::consts::PI * 2.0 {
                self.phase -= std::f32::consts::PI * 2.0;
            }
        }
    }

    fn render(&self) -> String {
        let (tl, tr, bl, br) = self.get_border_chars();
        format!("{}{}{}{}", tl, tr, bl, br)
    }

    fn is_active(&self) -> bool {
        true // Always active when created
    }

    fn reset(&mut self) {
        self.phase = 0.0;
    }
}

/// Ripple effect for touch/click feedback
pub struct RippleEffect {
    center_x: f32,
    center_y: f32,
    radius: f32,
    max_radius: f32,
    speed: f32,
    is_active: bool,
}

impl RippleEffect {
    pub fn new(x: f32, y: f32, max_radius: f32) -> Self {
        Self {
            center_x: x,
            center_y: y,
            radius: 0.0,
            max_radius,
            speed: 50.0,
            is_active: false,
        }
    }

    pub fn trigger(&mut self, x: f32, y: f32) {
        self.center_x = x;
        self.center_y = y;
        self.radius = 0.0;
        self.is_active = true;
    }

    pub fn get_char_at(&self, x: f32, y: f32) -> Option<char> {
        if !self.is_active {
            return None;
        }

        let distance = ((x - self.center_x).powi(2) + (y - self.center_y).powi(2)).sqrt();
        let ripple_width = 2.0;

        if (distance - self.radius).abs() < ripple_width {
            let intensity = 1.0 - (distance - self.radius).abs() / ripple_width;

            Some(match (intensity * 4.0) as usize {
                0 => '·',
                1 => '○',
                2 => '◉',
                3 => '●',
                _ => '◉',
            })
        } else {
            None
        }
    }
}

impl VisualEffect for RippleEffect {
    fn update(&mut self, delta: f32) {
        if self.is_active {
            self.radius += self.speed * delta;
            if self.radius > self.max_radius {
                self.is_active = false;
                self.radius = 0.0;
            }
        }
    }

    fn render(&self) -> String {
        // Simple ASCII representation
        if self.is_active {
            let progress = self.radius / self.max_radius;
            let rings = ["·", "○", "◎", "◉", "●"];
            // BUG-1003 fix: Properly normalize progress before indexing
            let normalized_progress = progress.clamp(0.0, 1.0);
            let index = (normalized_progress * (rings.len() - 1) as f32) as usize;
            rings[index].to_string()
        } else {
            String::new()
        }
    }

    fn is_active(&self) -> bool {
        self.is_active
    }

    fn reset(&mut self) {
        self.is_active = false;
        self.radius = 0.0;
    }
}

/// Sparkle effect for success/achievement
pub struct SparkleEffect {
    particles: Vec<Sparkle>,
    spawn_rate: f32,
    spawn_timer: f32,
    is_active: bool,
}

impl SparkleEffect {
    /// Maximum particles allowed to prevent memory bloat
    const MAX_PARTICLES: usize = 30;
}

struct Sparkle {
    x: f32,
    y: f32,
    life: f32,
    char: char,
}

impl SparkleEffect {
    pub fn new() -> Self {
        Self {
            particles: Vec::new(),
            spawn_rate: 10.0, // Sparkles per second
            spawn_timer: 0.0,
            is_active: false,
        }
    }

    pub fn trigger(&mut self) {
        self.is_active = true;
        self.spawn_timer = 0.0;

        // Spawn initial burst
        for _ in 0..5 {
            self.spawn_sparkle();
        }
    }

    fn spawn_sparkle(&mut self) {
        // Don't spawn if at capacity (BUG-1002 fix)
        if self.particles.len() >= Self::MAX_PARTICLES {
            return;
        }

        use rand::Rng;
        let mut rng = rand::thread_rng();

        let chars = ['✨', '⭐', '✦', '✧', '⋆', '･', '◦'];

        self.particles.push(Sparkle {
            x: rng.gen_range(-5.0..5.0),
            y: rng.gen_range(-3.0..3.0),
            life: 1.0,
            char: chars[rng.gen_range(0..chars.len())],
        });
    }
}

impl VisualEffect for SparkleEffect {
    fn update(&mut self, delta: f32) {
        if self.is_active {
            // Update existing particles
            self.particles.retain_mut(|sparkle| {
                sparkle.life -= delta * 2.0; // Fade out in 0.5 seconds
                sparkle.y -= delta * 3.0;     // Float upward
                sparkle.life > 0.0
            });

            // Spawn new particles
            self.spawn_timer += delta;
            if self.spawn_timer > 1.0 / self.spawn_rate {
                self.spawn_sparkle();
                self.spawn_timer = 0.0;
            }

            // Stop spawning after a while
            if self.particles.is_empty() && self.spawn_timer > 1.0 {
                self.is_active = false;
            }
        }
    }

    fn render(&self) -> String {
        if self.particles.is_empty() {
            return String::new();
        }

        // Create simple representation
        self.particles
            .iter()
            .map(|s| s.char.to_string())
            .collect::<Vec<_>>()
            .join("")
    }

    fn is_active(&self) -> bool {
        self.is_active || !self.particles.is_empty()
    }

    fn reset(&mut self) {
        self.is_active = false;
        self.particles.clear();
    }
}

/// Attention director - guides user focus
pub struct AttentionDirector {
    effects: Vec<Box<dyn VisualEffect>>,
}

impl AttentionDirector {
    pub fn new() -> Self {
        Self {
            effects: Vec::new(),
        }
    }

    pub fn add_effect(&mut self, effect: Box<dyn VisualEffect>) {
        self.effects.push(effect);
    }

    pub fn update(&mut self, delta: f32) {
        self.effects.retain_mut(|effect| {
            effect.update(delta);
            effect.is_active()
        });
    }

    pub fn clear(&mut self) {
        self.effects.clear();
    }

    pub fn render_all(&self) -> Vec<String> {
        self.effects.iter().map(|e| e.render()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shake_effect() {
        let mut shake = ShakeEffect::new(EffectIntensity::Normal);
        shake.trigger();
        assert!(shake.is_active());

        // Update for duration
        for _ in 0..10 {
            shake.update(0.05);
        }

        assert!(!shake.is_active()); // Should be done after ~500ms
    }

    #[test]
    fn test_glow_effect() {
        let mut glow = GlowEffect::new(EffectIntensity::Strong);

        for _ in 0..10 {
            glow.update(0.1);
        }

        let _ = glow.get_border_chars();
        assert!(glow.is_active());
    }

    #[test]
    fn test_ripple_effect() {
        let mut ripple = RippleEffect::new(0.0, 0.0, 10.0);
        ripple.trigger(5.0, 5.0);

        for _ in 0..20 {
            ripple.update(0.05);
        }

        assert!(!ripple.is_active()); // Should complete after radius exceeds max
    }
}