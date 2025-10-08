//! Modern theme system for agent_chat_v2 UI
//! Provides enhanced visual styling for the cursive-based TUI

use cursive::theme::{BaseColor, BorderStyle, Color, ColorStyle, Palette, PaletteColor, Theme};
use cursive::utils::markup::StyledString;

/// Modern color palette for the advanced chat UI
pub struct ModernTheme;

impl ModernTheme {
    /// Create a modern dark theme with vibrant accents
    pub fn dark() -> Theme {
        let mut theme = Theme::default();

        // Modern color palette
        let mut palette = Palette::default();

        // Background colors - dark with subtle gradients
        palette[PaletteColor::Background] = Color::Rgb(15, 15, 23); // Deep dark blue-black
        palette[PaletteColor::View] = Color::Rgb(24, 24, 36); // Slightly lighter surface
        palette[PaletteColor::Primary] = Color::Rgb(139, 92, 246); // Purple accent
        palette[PaletteColor::Secondary] = Color::Rgb(59, 130, 246); // Blue accent
        palette[PaletteColor::Tertiary] = Color::Rgb(34, 197, 94); // Success green
        palette[PaletteColor::TitlePrimary] = Color::Rgb(167, 139, 250); // Light purple
        palette[PaletteColor::TitleSecondary] = Color::Rgb(96, 165, 250); // Light blue
        palette[PaletteColor::Highlight] = Color::Rgb(236, 72, 153); // Pink highlight
        palette[PaletteColor::HighlightInactive] = Color::Rgb(75, 85, 99); // Muted gray
        palette[PaletteColor::HighlightText] = Color::Rgb(243, 244, 246); // Bright text

        theme.palette = palette;

        // Use simple borders (cursive doesn't have Rounded in older versions)
        theme.borders = BorderStyle::Simple;

        // Enable shadows for depth
        theme.shadow = true;

        theme
    }

    /// Create a modern light theme
    pub fn light() -> Theme {
        let mut theme = Theme::default();

        let mut palette = Palette::default();

        // Light theme colors
        palette[PaletteColor::Background] = Color::Rgb(248, 250, 252); // Off-white
        palette[PaletteColor::View] = Color::Rgb(255, 255, 255); // Pure white
        palette[PaletteColor::Primary] = Color::Rgb(109, 40, 217); // Dark purple
        palette[PaletteColor::Secondary] = Color::Rgb(37, 99, 235); // Dark blue
        palette[PaletteColor::Tertiary] = Color::Rgb(22, 163, 74); // Dark green
        palette[PaletteColor::TitlePrimary] = Color::Rgb(109, 40, 217); // Purple
        palette[PaletteColor::TitleSecondary] = Color::Rgb(37, 99, 235); // Blue
        palette[PaletteColor::Highlight] = Color::Rgb(219, 39, 119); // Pink
        palette[PaletteColor::HighlightInactive] = Color::Rgb(156, 163, 175);
        palette[PaletteColor::HighlightText] = Color::Rgb(17, 24, 39); // Dark text

        theme.palette = palette;
        theme.borders = BorderStyle::Simple;
        theme.shadow = true;

        theme
    }

    /// Create a high-contrast theme for accessibility
    pub fn high_contrast() -> Theme {
        let mut theme = Theme::default();

        let mut palette = Palette::default();

        // High contrast colors
        palette[PaletteColor::Background] = Color::Rgb(0, 0, 0); // Pure black
        palette[PaletteColor::View] = Color::Rgb(10, 10, 10); // Near black
        palette[PaletteColor::Primary] = Color::Rgb(255, 255, 0); // Yellow
        palette[PaletteColor::Secondary] = Color::Rgb(0, 255, 255); // Cyan
        palette[PaletteColor::Tertiary] = Color::Rgb(0, 255, 0); // Green
        palette[PaletteColor::TitlePrimary] = Color::Rgb(255, 255, 255); // White
        palette[PaletteColor::TitleSecondary] = Color::Rgb(255, 255, 0); // Yellow
        palette[PaletteColor::Highlight] = Color::Rgb(255, 0, 255); // Magenta
        palette[PaletteColor::HighlightInactive] = Color::Rgb(128, 128, 128);
        palette[PaletteColor::HighlightText] = Color::Rgb(0, 0, 0); // Black on highlight

        theme.palette = palette;
        theme.borders = BorderStyle::Simple; // Simple for clarity
        theme.shadow = false; // No shadows for clarity

        theme
    }
}

/// UI status icons
pub struct Icons;

impl Icons {
    pub const IDLE: &'static str = "◉";
    pub const THINKING: &'static str = "◐";
    pub const PLANNING: &'static str = "◑";
    pub const EXECUTING: &'static str = "▶";
    pub const WAITING: &'static str = "◯";
    pub const PAUSED: &'static str = "⏸";
    pub const ERROR: &'static str = "⚠";
    pub const SUCCESS: &'static str = "✓";
    pub const CHAT: &'static str = "💬";
    pub const NEW: &'static str = "✨";
    pub const RECORD: &'static str = "⏺";
    pub const STOP: &'static str = "⏹";
    pub const TOOL: &'static str = "🔧";
    pub const FOLDER: &'static str = "📁";
    pub const FILE: &'static str = "📄";
    pub const ROCKET: &'static str = "🚀";
    pub const SPARKLES: &'static str = "✨";
    pub const LIGHTNING: &'static str = "⚡";
    pub const FIRE: &'static str = "🔥";
    pub const STAR: &'static str = "⭐";
    pub const HEART: &'static str = "❤";
    pub const INFO: &'static str = "ℹ";
    pub const ARROW_RIGHT: &'static str = "→";
    pub const ARROW_LEFT: &'static str = "←";
    pub const CHECK_HEAVY: &'static str = "✔";
    pub const CROSS: &'static str = "✖";
}

/// Styled text builders for consistent formatting
pub struct StyledText;

impl StyledText {
    /// Create gradient-like text (simulated with alternating colors)
    pub fn gradient(text: &str) -> StyledString {
        let mut styled = StyledString::new();
        let colors = vec![
            Color::Rgb(139, 92, 246), // Purple
            Color::Rgb(96, 130, 246), // Blue-purple
            Color::Rgb(59, 130, 246), // Blue
        ];

        for (i, ch) in text.chars().enumerate() {
            let color_idx = i % colors.len();
            styled.append_styled(
                ch.to_string(),
                ColorStyle::new(colors[color_idx], Color::Rgb(24, 24, 36)),
            );
        }

        styled
    }

    /// Create success text (green)
    pub fn success(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(34, 197, 94), Color::Rgb(24, 24, 36)),
        )
    }

    /// Create error text (red)
    pub fn error(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(239, 68, 68), Color::Rgb(24, 24, 36)),
        )
    }

    /// Create warning text (yellow)
    pub fn warning(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(251, 146, 60), Color::Rgb(24, 24, 36)),
        )
    }

    /// Create info text (blue)
    pub fn info(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(59, 130, 246), Color::Rgb(24, 24, 36)),
        )
    }

    /// Create accent text (purple)
    pub fn accent(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(139, 92, 246), Color::Rgb(24, 24, 36)),
        )
    }

    /// Create highlighted text
    pub fn highlight(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(236, 72, 153), Color::Rgb(24, 24, 36)),
        )
    }
}

/// Progress bar builder
pub struct ProgressBar;

impl ProgressBar {
    /// Create a styled progress bar string
    pub fn render(progress: f32, width: usize) -> String {
        let filled = (progress * width as f32) as usize;
        let empty = width.saturating_sub(filled);

        let filled_char = '█';
        let empty_char = '░';

        format!(
            "{}{}{}",
            filled_char.to_string().repeat(filled),
            empty_char.to_string().repeat(empty),
            format!(" {:.0}%", progress * 100.0)
        )
    }

    /// Create a segmented progress bar
    pub fn render_segmented(current: usize, total: usize, width: usize) -> String {
        if total == 0 {
            return "░".repeat(width);
        }

        let segment_width = width / total.max(1);
        let mut bar = String::new();

        for i in 0..total {
            if i < current {
                bar.push_str(&"█".repeat(segment_width));
            } else {
                bar.push_str(&"░".repeat(segment_width));
            }
        }

        format!("{} [{}/{}]", bar, current, total)
    }
}

/// Animation frames for spinners
pub struct Spinners;

impl Spinners {
    pub const DOTS: &'static [&'static str] = &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
    pub const CIRCLE: &'static [&'static str] = &["◐", "◓", "◑", "◒"];
    pub const BLOCKS: &'static [&'static str] = &["▖", "▘", "▝", "▗"];
    pub const ARROWS: &'static [&'static str] = &["←", "↖", "↑", "↗", "→", "↘", "↓", "↙"];
    pub const DOTS_WAVE: &'static [&'static str] = &["⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"];
    pub const PULSE: &'static [&'static str] = &["○", "◔", "◑", "◕", "●", "◕", "◑", "◔"];
}

/// Decorative borders and separators
pub struct Decorations;

impl Decorations {
    pub const SEPARATOR_THIN: &'static str = "─────────────────────────────────";
    pub const SEPARATOR_THICK: &'static str = "═════════════════════════════════";
    pub const SEPARATOR_DOTTED: &'static str = "·····································";
    pub const SEPARATOR_WAVE: &'static str = "∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿";

    /// Create a fancy header with decorations
    pub fn fancy_header(title: &str) -> String {
        format!("╔═══════╣ {} ╠═══════╗", title)
    }

    /// Create a section divider
    pub fn section_divider(label: &str) -> String {
        let padding = (30 - label.len()) / 2;
        format!(
            "━━━{}━ {} ━{}━━━",
            "━".repeat(padding),
            label,
            "━".repeat(padding)
        )
    }
}
