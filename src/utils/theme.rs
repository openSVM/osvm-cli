//! Modern theme system for OSVM CLI
//! Provides centralized styling with gradient support, animations, and modern UI elements

use std::fmt;

/// Modern color palette with RGB and gradient support
pub struct Theme;

impl Theme {
    // ═══════════════════════════════════════════════════════════════════
    // Primary Color Palette - Modern, vibrant colors
    // ═══════════════════════════════════════════════════════════════════
    
    // Primary brand colors
    pub const PRIMARY: &'static str = "\x1b[38;2;139;92;246m";      // Purple #8B5CF6
    pub const PRIMARY_LIGHT: &'static str = "\x1b[38;2;167;139;250m"; // Light Purple #A78BFA
    pub const PRIMARY_DARK: &'static str = "\x1b[38;2;109;40;217m";  // Dark Purple #6D28D9
    
    // Accent colors for highlights
    pub const ACCENT: &'static str = "\x1b[38;2;59;130;246m";       // Blue #3B82F6
    pub const ACCENT_BRIGHT: &'static str = "\x1b[38;2;96;165;250m"; // Bright Blue #60A5FA
    pub const ACCENT_GLOW: &'static str = "\x1b[38;2;147;197;253m"; // Glow Blue #93C5FD
    
    // Semantic colors
    pub const SUCCESS: &'static str = "\x1b[38;2;34;197;94m";       // Green #22C55E
    pub const SUCCESS_LIGHT: &'static str = "\x1b[38;2;74;222;128m"; // Light Green #4ADE80
    pub const WARNING: &'static str = "\x1b[38;2;251;146;60m";      // Orange #FB923C
    pub const WARNING_LIGHT: &'static str = "\x1b[38;2;252;211;77m"; // Yellow #FDD34D
    pub const ERROR: &'static str = "\x1b[38;2;239;68;68m";         // Red #EF4444
    pub const ERROR_LIGHT: &'static str = "\x1b[38;2;248;113;113m";  // Light Red #F87171
    
    // Neutral colors
    pub const TEXT: &'static str = "\x1b[38;2;243;244;246m";        // Light Gray #F3F4F6
    pub const TEXT_DIM: &'static str = "\x1b[38;2;156;163;175m";    // Medium Gray #9CA3AF
    pub const TEXT_MUTED: &'static str = "\x1b[38;2;107;114;128m";  // Dark Gray #6B7280
    pub const SURFACE: &'static str = "\x1b[38;2;31;41;55m";        // Dark Surface #1F2937
    pub const BACKGROUND: &'static str = "\x1b[48;2;17;24;39m";     // Background #111827
    
    // Special effects
    pub const GOLD: &'static str = "\x1b[38;2;251;191;36m";         // Gold #FBBF24
    pub const CYAN_NEON: &'static str = "\x1b[38;2;6;182;212m";     // Neon Cyan #06B6D4
    pub const PINK_NEON: &'static str = "\x1b[38;2;236;72;153m";    // Neon Pink #EC4899
    
    // Terminal control
    pub const RESET: &'static str = "\x1b[0m";
    pub const BOLD: &'static str = "\x1b[1m";
    pub const DIM: &'static str = "\x1b[2m";
    pub const ITALIC: &'static str = "\x1b[3m";
    pub const UNDERLINE: &'static str = "\x1b[4m";
    pub const BLINK: &'static str = "\x1b[5m";
    pub const REVERSE: &'static str = "\x1b[7m";
    
    // ═══════════════════════════════════════════════════════════════════
    // Unicode Box Drawing Characters - Modern rounded corners
    // ═══════════════════════════════════════════════════════════════════
    
    // Rounded corners
    pub const BOX_TOP_LEFT: char = '╭';
    pub const BOX_TOP_RIGHT: char = '╮';
    pub const BOX_BOTTOM_LEFT: char = '╰';
    pub const BOX_BOTTOM_RIGHT: char = '╯';
    
    // Lines
    pub const BOX_HORIZONTAL: char = '─';
    pub const BOX_VERTICAL: char = '│';
    pub const BOX_HORIZONTAL_THICK: char = '━';
    pub const BOX_VERTICAL_THICK: char = '┃';
    
    // Double lines for emphasis
    pub const BOX_DOUBLE_HORIZONTAL: char = '═';
    pub const BOX_DOUBLE_VERTICAL: char = '║';
    
    // Connectors
    pub const BOX_T_DOWN: char = '┬';
    pub const BOX_T_UP: char = '┴';
    pub const BOX_T_RIGHT: char = '├';
    pub const BOX_T_LEFT: char = '┤';
    pub const BOX_CROSS: char = '┼';
    
    // ═══════════════════════════════════════════════════════════════════
    // Icons and Symbols
    // ═══════════════════════════════════════════════════════════════════
    
    pub const ICON_SUCCESS: &'static str = "✓";
    pub const ICON_ERROR: &'static str = "✗";
    pub const ICON_WARNING: &'static str = "⚠";
    pub const ICON_INFO: &'static str = "ℹ";
    pub const ICON_ARROW_RIGHT: &'static str = "→";
    pub const ICON_ARROW_LEFT: &'static str = "←";
    pub const ICON_ARROW_UP: &'static str = "↑";
    pub const ICON_ARROW_DOWN: &'static str = "↓";
    pub const ICON_CHEVRON_RIGHT: &'static str = "›";
    pub const ICON_CHEVRON_DOWN: &'static str = "⌄";
    pub const ICON_BULLET: &'static str = "•";
    pub const ICON_STAR: &'static str = "★";
    pub const ICON_HEART: &'static str = "♥";
    pub const ICON_LIGHTNING: &'static str = "⚡";
    pub const ICON_FIRE: &'static str = "🔥";
    pub const ICON_ROCKET: &'static str = "🚀";
    pub const ICON_SPARKLES: &'static str = "✨";
    pub const ICON_GEM: &'static str = "💎";
    pub const ICON_SHIELD: &'static str = "🛡";
    pub const ICON_LOCK: &'static str = "🔒";
    pub const ICON_KEY: &'static str = "🔑";
    pub const ICON_GEAR: &'static str = "⚙";
    pub const ICON_CLOCK: &'static str = "🕐";
    pub const ICON_HOURGLASS: &'static str = "⏳";
    
    // Progress indicators
    pub const PROGRESS_EMPTY: &'static str = "░";
    pub const PROGRESS_QUARTER: &'static str = "▒";
    pub const PROGRESS_HALF: &'static str = "▓";
    pub const PROGRESS_FULL: &'static str = "█";
    
    // Spinner frames with smooth animation
    pub const SPINNER_FRAMES: &'static [&'static str] = &[
        "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"
    ];
    
    pub const SPINNER_DOTS: &'static [&'static str] = &[
        "⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"
    ];
    
    pub const SPINNER_MODERN: &'static [&'static str] = &[
        "◐", "◓", "◑", "◒"
    ];
}

/// Gradient builder for smooth color transitions
pub struct Gradient;

impl Gradient {
    /// Create a gradient between two RGB colors
    pub fn linear(start_rgb: (u8, u8, u8), end_rgb: (u8, u8, u8), steps: usize) -> Vec<String> {
        let mut colors = Vec::with_capacity(steps);
        
        for i in 0..steps {
            let t = i as f32 / (steps - 1) as f32;
            let r = Self::lerp(start_rgb.0, end_rgb.0, t);
            let g = Self::lerp(start_rgb.1, end_rgb.1, t);
            let b = Self::lerp(start_rgb.2, end_rgb.2, t);
            
            colors.push(format!("\x1b[38;2;{};{};{}m", r, g, b));
        }
        
        colors
    }
    
    /// Apply gradient to text
    pub fn apply_to_text(text: &str, colors: &[String]) -> String {
        let chars: Vec<char> = text.chars().collect();
        let mut result = String::new();
        
        for (i, ch) in chars.iter().enumerate() {
            let color_idx = (i * colors.len()) / chars.len();
            result.push_str(&colors[color_idx.min(colors.len() - 1)]);
            result.push(*ch);
        }
        
        result.push_str(Theme::RESET);
        result
    }
    
    /// Linear interpolation helper
    fn lerp(start: u8, end: u8, t: f32) -> u8 {
        (start as f32 + (end as f32 - start as f32) * t) as u8
    }
    
    /// Preset gradients
    pub fn purple_gradient() -> Vec<String> {
        Self::linear((139, 92, 246), (59, 130, 246), 10)
    }
    
    pub fn fire_gradient() -> Vec<String> {
        Self::linear((239, 68, 68), (251, 191, 36), 10)
    }
    
    pub fn ocean_gradient() -> Vec<String> {
        Self::linear((6, 182, 212), (59, 130, 246), 10)
    }
    
    pub fn sunset_gradient() -> Vec<String> {
        Self::linear((251, 146, 60), (236, 72, 153), 10)
    }
    
    pub fn matrix_gradient() -> Vec<String> {
        Self::linear((34, 197, 94), (6, 182, 212), 10)
    }
}

/// Styled box builder for modern UI components
pub struct StyledBox;

impl StyledBox {
    /// Create a box with rounded corners
    pub fn rounded(title: &str, content: &str, width: usize, color: &str) -> String {
        let mut result = String::new();
        
        // Calculate padding
        let title_with_spaces = format!(" {} ", title);
        let title_len = title_with_spaces.chars().count();
        let padding = width.saturating_sub(title_len + 2);
        let left_pad = padding / 2;
        let right_pad = padding - left_pad;
        
        // Top border with title
        result.push_str(color);
        result.push(Theme::BOX_TOP_LEFT);
        result.push_str(&Theme::BOX_HORIZONTAL.to_string().repeat(left_pad));
        result.push_str(&title_with_spaces);
        result.push_str(&Theme::BOX_HORIZONTAL.to_string().repeat(right_pad));
        result.push(Theme::BOX_TOP_RIGHT);
        result.push_str(Theme::RESET);
        result.push('\n');
        
        // Content lines
        for line in content.lines() {
            result.push_str(color);
            result.push(Theme::BOX_VERTICAL);
            result.push_str(Theme::RESET);
            result.push(' ');
            result.push_str(line);
            
            // Pad to width
            let line_len = line.chars().count();
            if line_len < width - 2 {
                result.push_str(&" ".repeat(width - 2 - line_len));
            }
            result.push(' ');
            result.push_str(color);
            result.push(Theme::BOX_VERTICAL);
            result.push_str(Theme::RESET);
            result.push('\n');
        }
        
        // Bottom border
        result.push_str(color);
        result.push(Theme::BOX_BOTTOM_LEFT);
        result.push_str(&Theme::BOX_HORIZONTAL.to_string().repeat(width));
        result.push(Theme::BOX_BOTTOM_RIGHT);
        result.push_str(Theme::RESET);
        
        result
    }
    
    /// Create a box with shadow effect (using dimmed duplicate)
    pub fn with_shadow(title: &str, content: &str, width: usize, color: &str) -> String {
        let box_content = Self::rounded(title, content, width, color);
        let mut result = String::new();
        
        // Add shadow by offsetting dim version
        for line in box_content.lines() {
            result.push_str(line);
            result.push_str(&format!("{}  {}", Theme::DIM, Theme::RESET));
            result.push('\n');
        }
        
        result
    }
}

/// Progress bar builder
pub struct ProgressBar;

impl ProgressBar {
    /// Create a modern progress bar
    pub fn render(progress: f32, width: usize, show_percentage: bool) -> String {
        let filled = (progress * width as f32) as usize;
        let empty = width.saturating_sub(filled);
        
        let mut bar = String::new();
        
        // Start bracket
        bar.push_str(Theme::TEXT_DIM);
        bar.push('[');
        
        // Filled portion with gradient
        let gradient = Gradient::purple_gradient();
        for i in 0..filled {
            let color_idx = (i * gradient.len()) / width;
            bar.push_str(&gradient[color_idx.min(gradient.len() - 1)]);
            bar.push_str(Theme::PROGRESS_FULL);
        }
        
        // Empty portion
        bar.push_str(Theme::TEXT_MUTED);
        bar.push_str(&Theme::PROGRESS_EMPTY.repeat(empty));
        
        // End bracket
        bar.push_str(Theme::TEXT_DIM);
        bar.push(']');
        
        // Percentage
        if show_percentage {
            bar.push_str(&format!(" {}{}%{}", 
                Theme::ACCENT_BRIGHT, 
                (progress * 100.0) as u32,
                Theme::RESET
            ));
        }
        
        bar.push_str(Theme::RESET);
        bar
    }
}

/// Animation effects
pub struct Effects;

impl Effects {
    /// Typewriter effect for text
    pub fn typewriter(text: &str, delay_ms: u64) {
        use std::io::{stdout, Write};
        use std::thread;
        use std::time::Duration;
        
        for ch in text.chars() {
            print!("{}", ch);
            stdout().flush().unwrap();
            thread::sleep(Duration::from_millis(delay_ms));
        }
        println!();
    }
    
    /// Fade in effect using color intensity
    pub fn fade_in(text: &str, steps: usize) {
        use std::io::{stdout, Write};
        use std::thread;
        use std::time::Duration;
        
        for i in 0..=steps {
            let intensity = (i as f32 / steps as f32 * 255.0) as u8;
            print!("\x1b[2J\x1b[H"); // Clear screen and move to top
            print!("\x1b[38;2;{};{};{}m{}\x1b[0m", intensity, intensity, intensity, text);
            stdout().flush().unwrap();
            thread::sleep(Duration::from_millis(50));
        }
    }
    
    /// Pulse effect for highlighting
    pub fn pulse(text: &str, color_rgb: (u8, u8, u8), pulses: usize) {
        use std::io::{stdout, Write};
        use std::thread;
        use std::time::Duration;
        
        for _ in 0..pulses {
            // Bright
            print!("\r\x1b[38;2;{};{};{}m{}\x1b[0m", 
                color_rgb.0, color_rgb.1, color_rgb.2, text);
            stdout().flush().unwrap();
            thread::sleep(Duration::from_millis(200));
            
            // Dim
            print!("\r\x1b[38;2;{};{};{}m{}\x1b[0m", 
                color_rgb.0 / 2, color_rgb.1 / 2, color_rgb.2 / 2, text);
            stdout().flush().unwrap();
            thread::sleep(Duration::from_millis(200));
        }
        
        // Final bright state
        print!("\r\x1b[38;2;{};{};{}m{}\x1b[0m", 
            color_rgb.0, color_rgb.1, color_rgb.2, text);
        stdout().flush().unwrap();
    }
}

/// Status indicator with icon and color
pub struct StatusIndicator;

impl StatusIndicator {
    pub fn success(message: &str) -> String {
        format!("{}{} {}{} {}{}", 
            Theme::SUCCESS, Theme::ICON_SUCCESS, 
            Theme::BOLD, Theme::TEXT, message, Theme::RESET)
    }
    
    pub fn error(message: &str) -> String {
        format!("{}{} {}{} {}{}", 
            Theme::ERROR, Theme::ICON_ERROR,
            Theme::BOLD, Theme::TEXT, message, Theme::RESET)
    }
    
    pub fn warning(message: &str) -> String {
        format!("{}{} {}{} {}{}", 
            Theme::WARNING, Theme::ICON_WARNING,
            Theme::BOLD, Theme::TEXT, message, Theme::RESET)
    }
    
    pub fn info(message: &str) -> String {
        format!("{}{} {}{} {}{}", 
            Theme::ACCENT, Theme::ICON_INFO,
            Theme::BOLD, Theme::TEXT, message, Theme::RESET)
    }
    
    pub fn loading(message: &str, frame_idx: usize) -> String {
        let spinner = Theme::SPINNER_FRAMES[frame_idx % Theme::SPINNER_FRAMES.len()];
        format!("{}{} {}{} {}{}", 
            Theme::ACCENT_BRIGHT, spinner,
            Theme::BOLD, Theme::TEXT, message, Theme::RESET)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_gradient_generation() {
        let gradient = Gradient::linear((0, 0, 0), (255, 255, 255), 5);
        assert_eq!(gradient.len(), 5);
    }
    
    #[test]
    fn test_progress_bar() {
        let bar = ProgressBar::render(0.5, 20, true);
        assert!(bar.contains("50%"));
    }
    
    #[test]
    fn test_styled_box() {
        let box_content = StyledBox::rounded("Test", "Content", 20, Theme::ACCENT);
        assert!(box_content.contains('╭'));
        assert!(box_content.contains('╯'));
    }
}