//! Modern theme system for agent_chat_v2 UI
//! Provides enhanced visual styling for the cursive-based TUI

use cursive::theme::{BaseColor, BorderStyle, Color, ColorStyle, Palette, PaletteColor, Theme};
use cursive::utils::markup::StyledString;

/// Modern color palette for the advanced chat UI
pub struct ModernTheme;

impl ModernTheme {
    /// Get list of available theme names
    pub fn available_themes() -> Vec<&'static str> {
        vec!["dark", "light", "high_contrast"]
    }

    /// Get theme by name
    pub fn by_name(name: &str) -> Theme {
        match name {
            "light" => Self::light(),
            "high_contrast" => Self::high_contrast(),
            _ => Self::dark(), // Default to dark
        }
    }

    /// VS Code inspired dark theme with professional colors
    pub fn dark() -> Theme {
        let mut theme = Theme::default();

        // VS Code color palette
        let mut palette = Palette::default();

        // Background colors - VS Code dark theme
        palette[PaletteColor::Background] = Color::Rgb(30, 30, 30); // #1e1e1e - VS Code background
        palette[PaletteColor::View] = Color::Rgb(37, 37, 38); // #252526 - VS Code sidebar
        palette[PaletteColor::Primary] = Color::Rgb(0, 122, 204); // #007acc - VS Code blue
        palette[PaletteColor::Secondary] = Color::Rgb(86, 156, 214); // #569cd6 - VS Code keyword blue
        palette[PaletteColor::Tertiary] = Color::Rgb(106, 153, 85); // #6a9955 - VS Code green
        palette[PaletteColor::TitlePrimary] = Color::Rgb(212, 212, 212); // #d4d4d4 - VS Code text
        palette[PaletteColor::TitleSecondary] = Color::Rgb(86, 156, 214); // #569cd6 - VS Code blue
        palette[PaletteColor::Highlight] = Color::Rgb(38, 79, 120); // #264f78 - VS Code selection
        palette[PaletteColor::HighlightInactive] = Color::Rgb(42, 45, 46); // #2a2d2e - VS Code line highlight
        palette[PaletteColor::HighlightText] = Color::Rgb(212, 212, 212); // #d4d4d4 - VS Code text

        theme.palette = palette;

        // Use simple borders for clean VS Code look
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

/// UI status icons (text-based, no emojis)
pub struct Icons;

impl Icons {
    pub const IDLE: &'static str = "[IDLE]";
    pub const THINKING: &'static str = "[...]";
    pub const PLANNING: &'static str = "[PLAN]";
    pub const EXECUTING: &'static str = "[EXEC]";
    pub const WAITING: &'static str = "[WAIT]";
    pub const PAUSED: &'static str = "[PAUSE]";
    pub const ERROR: &'static str = "[ERROR]";
    pub const SUCCESS: &'static str = "[OK]";
    pub const CHAT: &'static str = "";
    pub const NEW: &'static str = "";
    pub const RECORD: &'static str = "[REC]";
    pub const STOP: &'static str = "[STOP]";
    pub const TOOL: &'static str = "";
    pub const FOLDER: &'static str = "";
    pub const FILE: &'static str = "";
    pub const ROCKET: &'static str = "";
    pub const SPARKLES: &'static str = "";
    pub const LIGHTNING: &'static str = "";
    pub const FIRE: &'static str = "";
    pub const STAR: &'static str = "";
    pub const HEART: &'static str = "";
    pub const INFO: &'static str = "[INFO]";
    pub const ARROW_RIGHT: &'static str = ">";
    pub const ARROW_LEFT: &'static str = "<";
    pub const CHECK_HEAVY: &'static str = "[OK]";
    pub const CROSS: &'static str = "[X]";
    pub const SEPARATOR: &'static str = "│";
    pub const KEYBOARD: &'static str = "⌨";
}

/// Styled text builders for consistent formatting (VS Code colors)
pub struct StyledText;

impl StyledText {
    /// Create gradient-like text (VS Code style)
    pub fn gradient(text: &str) -> StyledString {
        let mut styled = StyledString::new();
        let colors = vec![
            Color::Rgb(86, 156, 214),  // #569cd6 - VS Code keyword blue
            Color::Rgb(0, 122, 204),   // #007acc - VS Code bright blue
            Color::Rgb(78, 201, 176),  // #4ec9b0 - VS Code cyan
        ];

        for (i, ch) in text.chars().enumerate() {
            let color_idx = i % colors.len();
            styled.append_styled(
                ch.to_string(),
                ColorStyle::new(colors[color_idx], Color::Rgb(30, 30, 30)),
            );
        }

        styled
    }

    /// Create success text (VS Code green)
    pub fn success(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(106, 153, 85), Color::Rgb(30, 30, 30)), // #6a9955
        )
    }

    /// Create error text (VS Code red)
    pub fn error(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(244, 71, 71), Color::Rgb(30, 30, 30)), // #f44747
        )
    }

    /// Create warning text (VS Code orange)
    pub fn warning(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(206, 145, 120), Color::Rgb(30, 30, 30)), // #ce9178
        )
    }

    /// Create info text (VS Code blue)
    pub fn info(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(86, 156, 214), Color::Rgb(30, 30, 30)), // #569cd6
        )
    }

    /// Create accent text (VS Code bright blue)
    pub fn accent(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(0, 122, 204), Color::Rgb(30, 30, 30)), // #007acc
        )
    }

    /// Create highlighted text (VS Code selection)
    pub fn highlight(text: &str) -> StyledString {
        StyledString::styled(
            text,
            ColorStyle::new(Color::Rgb(212, 212, 212), Color::Rgb(38, 79, 120)), // #d4d4d4 on #264f78
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
