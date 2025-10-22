//! Smooth animations and transitions for a delightful TUI experience

use cursive::theme::Color;
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant};

pub mod color_transitions;
pub mod progress;
pub mod spinners;
pub mod typewriter;

/// Animation state that can be shared across threads
#[derive(Clone)]
pub struct AnimationState {
    frame: Arc<RwLock<usize>>,
    last_update: Arc<RwLock<Instant>>,
    is_running: Arc<RwLock<bool>>,
}

impl AnimationState {
    pub fn new() -> Self {
        Self {
            frame: Arc::new(RwLock::new(0)),
            last_update: Arc::new(RwLock::new(Instant::now())),
            is_running: Arc::new(RwLock::new(false)),
        }
    }

    pub fn start(&self) {
        *self.is_running.write().unwrap() = true;
        *self.last_update.write().unwrap() = Instant::now();
    }

    pub fn stop(&self) {
        *self.is_running.write().unwrap() = false;
    }

    pub fn is_running(&self) -> bool {
        *self.is_running.read().unwrap()
    }

    pub fn advance_frame(&self) -> usize {
        // BUG-1007 fix: Acquire both locks together to prevent deadlock
        let mut frame = self.frame.write().unwrap();
        let mut last_update = self.last_update.write().unwrap();

        *frame += 1;
        *last_update = Instant::now();

        *frame
    }

    pub fn current_frame(&self) -> usize {
        *self.frame.read().unwrap()
    }

    pub fn elapsed(&self) -> Duration {
        self.last_update.read().unwrap().elapsed()
    }
}

/// Easing functions for smooth animations
pub mod easing {
    /// Linear interpolation (no easing)
    pub fn linear(t: f32) -> f32 {
        t
    }

    /// Quadratic ease in (accelerating)
    pub fn ease_in_quad(t: f32) -> f32 {
        t * t
    }

    /// Quadratic ease out (decelerating)
    pub fn ease_out_quad(t: f32) -> f32 {
        t * (2.0 - t)
    }

    /// Quadratic ease in-out (accelerate then decelerate)
    pub fn ease_in_out_quad(t: f32) -> f32 {
        if t < 0.5 {
            2.0 * t * t
        } else {
            -1.0 + (4.0 - 2.0 * t) * t
        }
    }

    /// Cubic ease in-out (smoother than quadratic)
    pub fn ease_in_out_cubic(t: f32) -> f32 {
        if t < 0.5 {
            4.0 * t * t * t
        } else {
            (t - 1.0) * (2.0 * t - 2.0) * (2.0 * t - 2.0) + 1.0
        }
    }

    /// Elastic ease out (bouncy)
    pub fn ease_out_elastic(t: f32) -> f32 {
        const C4: f32 = 2.0 * std::f32::consts::PI / 3.0;

        if t == 0.0 {
            0.0
        } else if t == 1.0 {
            1.0
        } else {
            2.0_f32.powf(-10.0 * t) * ((t * 10.0 - 0.75) * C4).sin() + 1.0
        }
    }

    /// Back ease out (overshoot then settle)
    pub fn ease_out_back(t: f32) -> f32 {
        const C1: f32 = 1.70158;
        const C3: f32 = C1 + 1.0;

        1.0 + C3 * (t - 1.0).powi(3) + C1 * (t - 1.0).powi(2)
    }

    /// Bounce ease out
    pub fn ease_out_bounce(mut t: f32) -> f32 {
        const N1: f32 = 7.5625;
        const D1: f32 = 2.75;

        if t < 1.0 / D1 {
            N1 * t * t
        } else if t < 2.0 / D1 {
            t -= 1.5 / D1;
            N1 * t * t + 0.75
        } else if t < 2.5 / D1 {
            t -= 2.25 / D1;
            N1 * t * t + 0.9375
        } else {
            t -= 2.625 / D1;
            N1 * t * t + 0.984375
        }
    }
}

/// Frame rate controller for smooth animations
pub struct FrameRateController {
    target_fps: u32,
    last_frame: Instant,
    frame_times: Vec<Duration>,
    max_history: usize,
}

impl FrameRateController {
    pub fn new(target_fps: u32) -> Self {
        Self {
            target_fps,
            last_frame: Instant::now(),
            frame_times: Vec::with_capacity(60),
            max_history: 60,
        }
    }

    /// Check if enough time has passed for next frame
    pub fn should_update(&mut self) -> bool {
        let frame_duration = Duration::from_millis(1000 / self.target_fps as u64);
        let elapsed = self.last_frame.elapsed();

        if elapsed >= frame_duration {
            // Track frame time for stats
            if self.frame_times.len() >= self.max_history {
                self.frame_times.remove(0);
            }
            self.frame_times.push(elapsed);

            self.last_frame = Instant::now();
            true
        } else {
            false
        }
    }

    /// Get current actual FPS
    pub fn current_fps(&self) -> f32 {
        if self.frame_times.is_empty() {
            return self.target_fps as f32;
        }

        let avg_frame_time: Duration =
            self.frame_times.iter().sum::<Duration>() / self.frame_times.len() as u32;

        if avg_frame_time.as_millis() > 0 {
            1000.0 / avg_frame_time.as_millis() as f32
        } else {
            self.target_fps as f32
        }
    }

    /// Adaptive quality - reduce quality if FPS drops
    pub fn quality_level(&self) -> QualityLevel {
        let fps = self.current_fps();
        let target = self.target_fps as f32;

        if fps >= target * 0.9 {
            QualityLevel::High
        } else if fps >= target * 0.7 {
            QualityLevel::Medium
        } else {
            QualityLevel::Low
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum QualityLevel {
    Low,    // Skip animations, basic updates only
    Medium, // Simple animations
    High,   // All animations and effects
}

/// Interpolate between two values
pub fn lerp<T>(start: T, end: T, t: f32) -> T
where
    T: std::ops::Sub<Output = T>
        + std::ops::Add<Output = T>
        + std::ops::Mul<f32, Output = T>
        + Copy,
{
    start + (end - start) * t.clamp(0.0, 1.0)
}

/// Interpolate colors smoothly
pub fn lerp_color(start: Color, end: Color, t: f32) -> Color {
    match (start, end) {
        (Color::Rgb(r1, g1, b1), Color::Rgb(r2, g2, b2)) => {
            Color::Rgb(
                lerp(r1 as f32, r2 as f32, t) as u8,
                lerp(g1 as f32, g2 as f32, t) as u8,
                lerp(b1 as f32, b2 as f32, t) as u8,
            )
        }
        _ => end, // Fallback to end color if types don't match
    }
}

/// Spring physics for bouncy animations
pub struct SpringAnimation {
    position: f32,
    velocity: f32,
    target: f32,
    stiffness: f32,
    damping: f32,
}

impl SpringAnimation {
    pub fn new(initial: f32, stiffness: f32, damping: f32) -> Self {
        Self {
            position: initial,
            velocity: 0.0,
            target: initial,
            stiffness,
            damping,
        }
    }

    pub fn set_target(&mut self, target: f32) {
        self.target = target;
    }

    pub fn update(&mut self, delta_time: f32) -> f32 {
        let force = (self.target - self.position) * self.stiffness;
        let damping_force = self.velocity * self.damping;
        let acceleration = force - damping_force;

        self.velocity += acceleration * delta_time;
        self.position += self.velocity * delta_time;

        self.position
    }

    pub fn is_settled(&self) -> bool {
        (self.position - self.target).abs() < 0.001 && self.velocity.abs() < 0.001
    }
}

/// Particle system for special effects
pub struct Particle {
    pub x: f32,
    pub y: f32,
    pub vx: f32,
    pub vy: f32,
    pub life: f32,
    pub char: char,
}

impl Particle {
    pub fn new(x: f32, y: f32) -> Self {
        use rand::Rng;
        let mut rng = rand::rng();

        Self {
            x,
            y,
            vx: rng.random_range(-1.0..1.0),
            vy: rng.random_range(-2.0..-0.5),
            life: 1.0,
            char: ['✨', '⭐', '💫', '✦', '•', '◦'][rng.random_range(0..6)],
        }
    }

    pub fn update(&mut self, delta: f32) {
        self.x += self.vx * delta;
        self.y += self.vy * delta;
        self.vy += 2.0 * delta; // Gravity
        self.life -= delta * 0.5;
    }

    pub fn is_alive(&self) -> bool {
        self.life > 0.0
    }
}

/// Animation manager to coordinate all animations
pub struct AnimationManager {
    animations: Vec<Box<dyn Animation>>,
    frame_controller: FrameRateController,
}

impl AnimationManager {
    pub fn new(target_fps: u32) -> Self {
        Self {
            animations: Vec::new(),
            frame_controller: FrameRateController::new(target_fps),
        }
    }

    pub fn add_animation(&mut self, anim: Box<dyn Animation>) {
        self.animations.push(anim);
    }

    pub fn update(&mut self) -> bool {
        if !self.frame_controller.should_update() {
            return false;
        }

        let quality = self.frame_controller.quality_level();
        let delta = 1.0 / self.frame_controller.target_fps as f32;

        // Update all animations
        self.animations.retain_mut(|anim| {
            anim.update(delta, quality);
            !anim.is_complete()
        });

        !self.animations.is_empty()
    }
}

/// Trait for all animations
pub trait Animation: Send + Sync {
    fn update(&mut self, delta: f32, quality: QualityLevel);
    fn is_complete(&self) -> bool;
    fn render(&self) -> String;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_easing_functions() {
        assert_eq!(easing::linear(0.0), 0.0);
        assert_eq!(easing::linear(0.5), 0.5);
        assert_eq!(easing::linear(1.0), 1.0);

        assert!(easing::ease_in_quad(0.5) < 0.5);
        assert!(easing::ease_out_quad(0.5) > 0.5);
    }

    #[test]
    fn test_spring_animation() {
        let mut spring = SpringAnimation::new(0.0, 10.0, 0.5);
        spring.set_target(100.0);

        for _ in 0..300 {
            spring.update(0.016); // 60 FPS
        }

        // Spring animations with low stiffness take time to settle
        assert!((spring.position - 100.0).abs() < 5.0);
    }
}