//! Modern color system with RGB support and gradients
//! Enhanced color palette for terminal output with theme integration

use crate::utils::theme::{Gradient, Theme};

/// Modern color system with rich RGB colors
pub struct Colors;

impl Colors {
    // Use theme colors for consistency
    pub const RESET: &'static str = Theme::RESET;
    pub const BOLD: &'static str = Theme::BOLD;
    pub const DIM: &'static str = Theme::DIM;
    pub const ITALIC: &'static str = Theme::ITALIC;
    pub const UNDERLINE: &'static str = Theme::UNDERLINE;

    // Primary colors with modern RGB values
    pub const PRIMARY: &'static str = Theme::PRIMARY; // Main brand color
    pub const PRIMARY_LIGHT: &'static str = Theme::PRIMARY_LIGHT;
    pub const ACCENT: &'static str = Theme::ACCENT; // Accent blue
    pub const ACCENT_BRIGHT: &'static str = Theme::ACCENT_BRIGHT;

    // Semantic colors for different message types
    pub const SUCCESS: &'static str = Theme::SUCCESS; // Success green
    pub const WARNING: &'static str = Theme::WARNING; // Warning orange
    pub const ERROR: &'static str = Theme::ERROR; // Error red
    pub const INFO: &'static str = Theme::ACCENT; // Info blue

    // Legacy mappings for backward compatibility
    pub const CYAN: &'static str = Theme::CYAN_NEON; // System messages
    pub const GREEN: &'static str = Theme::SUCCESS; // User input, success
    pub const YELLOW: &'static str = Theme::WARNING; // Tool names
    pub const RED: &'static str = Theme::ERROR; // Errors
    pub const BLUE: &'static str = Theme::ACCENT; // Server names
    pub const MAGENTA: &'static str = Theme::PINK_NEON; // Special actions
    pub const GRAY: &'static str = Theme::TEXT_DIM; // Auto-complete suggestions

    // Text colors
    pub const TEXT: &'static str = Theme::TEXT; // Primary text
    pub const TEXT_DIM: &'static str = Theme::TEXT_DIM; // Secondary text
    pub const TEXT_MUTED: &'static str = Theme::TEXT_MUTED; // Disabled text

    // Special effects
    pub const GOLD: &'static str = Theme::GOLD; // Premium features
    pub const NEON_CYAN: &'static str = Theme::CYAN_NEON; // Highlights
    pub const NEON_PINK: &'static str = Theme::PINK_NEON; // Alerts

    /// Apply gradient to text
    pub fn gradient(text: &str, gradient_type: GradientType) -> String {
        let colors = match gradient_type {
            GradientType::Purple => Gradient::purple_gradient(),
            GradientType::Fire => Gradient::fire_gradient(),
            GradientType::Ocean => Gradient::ocean_gradient(),
            GradientType::Sunset => Gradient::sunset_gradient(),
            GradientType::Matrix => Gradient::matrix_gradient(),
        };
        Gradient::apply_to_text(text, &colors)
    }

    /// Create glowing text effect
    pub fn glow(text: &str, base_color: &str) -> String {
        format!(
            "{}{}{}{}{}",
            base_color,
            Theme::BOLD,
            text,
            Theme::RESET,
            "\x1b[0m" // Extra reset for safety
        )
    }

    /// Create pulsing text (for animations)
    pub fn pulse_text(text: &str, intensity: f32) -> String {
        let alpha = (intensity * 255.0) as u8;
        format!(
            "\x1b[38;2;{};{};{}m{}{}",
            alpha,
            alpha,
            alpha,
            text,
            Theme::RESET
        )
    }
}

/// Gradient types for text effects
pub enum GradientType {
    Purple,
    Fire,
    Ocean,
    Sunset,
    Matrix,
}

/// Helper functions for color manipulation
impl Colors {
    /// Mix two RGB colors
    pub fn mix_rgb(color1: (u8, u8, u8), color2: (u8, u8, u8), ratio: f32) -> String {
        let r = (color1.0 as f32 * (1.0 - ratio) + color2.0 as f32 * ratio) as u8;
        let g = (color1.1 as f32 * (1.0 - ratio) + color2.1 as f32 * ratio) as u8;
        let b = (color1.2 as f32 * (1.0 - ratio) + color2.2 as f32 * ratio) as u8;
        format!("\x1b[38;2;{};{};{}m", r, g, b)
    }

    /// Create color with transparency simulation (using dimming)
    pub fn with_alpha(base_rgb: (u8, u8, u8), alpha: f32) -> String {
        let r = (base_rgb.0 as f32 * alpha) as u8;
        let g = (base_rgb.1 as f32 * alpha) as u8;
        let b = (base_rgb.2 as f32 * alpha) as u8;
        format!("\x1b[38;2;{};{};{}m", r, g, b)
    }
}
