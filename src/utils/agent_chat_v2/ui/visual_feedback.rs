//! Visual feedback system for state changes and user actions

use cursive::theme::{BaseColor, Color, ColorStyle, Effect, Style};
use cursive::utils::markup::StyledString;

/// Visual feedback levels
pub enum FeedbackLevel {
    Success,
    Info,
    Warning,
    Error,
    Processing,
    Neutral,
}

impl FeedbackLevel {
    /// Get color for feedback level
    pub fn color(&self) -> Color {
        match self {
            FeedbackLevel::Success => Color::Rgb(34, 197, 94),    // Green
            FeedbackLevel::Info => Color::Rgb(59, 130, 246),       // Blue
            FeedbackLevel::Warning => Color::Rgb(251, 146, 60),    // Orange
            FeedbackLevel::Error => Color::Rgb(239, 68, 68),       // Red
            FeedbackLevel::Processing => Color::Rgb(139, 92, 246), // Purple
            FeedbackLevel::Neutral => Color::Rgb(156, 163, 175),   // Gray
        }
    }

    /// Get emoji icon for feedback level
    pub fn icon(&self) -> &'static str {
        match self {
            FeedbackLevel::Success => "✓",
            FeedbackLevel::Info => "ℹ",
            FeedbackLevel::Warning => "⚠",
            FeedbackLevel::Error => "✖",
            FeedbackLevel::Processing => "⟳",
            FeedbackLevel::Neutral => "•",
        }
    }

    /// Get background color for feedback level
    pub fn background(&self) -> Color {
        match self {
            FeedbackLevel::Success => Color::Rgb(20, 83, 45),    // Dark green
            FeedbackLevel::Info => Color::Rgb(30, 58, 138),       // Dark blue
            FeedbackLevel::Warning => Color::Rgb(120, 53, 15),    // Dark orange
            FeedbackLevel::Error => Color::Rgb(127, 29, 29),      // Dark red
            FeedbackLevel::Processing => Color::Rgb(67, 56, 202), // Dark purple
            FeedbackLevel::Neutral => Color::Rgb(55, 65, 81),     // Dark gray
        }
    }
}

/// Create styled text with feedback level
pub fn styled_feedback(text: &str, level: FeedbackLevel) -> StyledString {
    let mut styled = StyledString::new();
    let style = ColorStyle::new(level.color(), Color::Rgb(24, 24, 36));

    // Add icon and text
    styled.append_styled(format!("{} {}", level.icon(), text), style);

    styled
}

/// Create styled text with background
pub fn styled_badge(text: &str, level: FeedbackLevel) -> StyledString {
    let mut styled = StyledString::new();
    let style = ColorStyle::new(Color::Rgb(255, 255, 255), level.background());

    // Add padding and text
    styled.append_styled(format!(" {} {} ", level.icon(), text), style);

    styled
}

/// Visual state indicators for different components
pub struct VisualStates;

impl VisualStates {
    /// Agent state with full visual treatment
    pub fn agent_state_badge(state_name: &str, is_active: bool) -> StyledString {
        let level = if is_active {
            FeedbackLevel::Processing
        } else {
            FeedbackLevel::Neutral
        };

        styled_badge(state_name, level)
    }

    /// Message state indicator
    pub fn message_state(state: MessageState) -> StyledString {
        match state {
            MessageState::Sent => styled_feedback("Sent", FeedbackLevel::Success),
            MessageState::Sending => styled_feedback("Sending...", FeedbackLevel::Processing),
            MessageState::Failed => styled_feedback("Failed", FeedbackLevel::Error),
            MessageState::Queued => styled_feedback("Queued", FeedbackLevel::Info),
        }
    }

    /// Connection state indicator
    pub fn connection_state(connected: bool) -> StyledString {
        if connected {
            styled_badge("Connected", FeedbackLevel::Success)
        } else {
            styled_badge("Disconnected", FeedbackLevel::Error)
        }
    }

    /// Operation progress indicator
    pub fn operation_progress(current: usize, total: usize) -> StyledString {
        let percentage = if total > 0 {
            (current as f64 / total as f64 * 100.0) as usize
        } else {
            0
        };

        styled_feedback(
            &format!("{}% ({}/{})", percentage, current, total),
            FeedbackLevel::Processing,
        )
    }

    /// History navigation indicator
    pub fn history_indicator(position: usize, total: usize) -> StyledString {
        styled_badge(
            &format!("History {}/{}", position + 1, total),
            FeedbackLevel::Info,
        )
    }

    /// Recording indicator (animated)
    pub fn recording_indicator(is_recording: bool) -> StyledString {
        if is_recording {
            styled_badge("⏺ REC", FeedbackLevel::Error)
        } else {
            styled_badge("○ Ready", FeedbackLevel::Neutral)
        }
    }

    /// Validation state indicator
    pub fn validation_state(is_valid: bool, message: &str) -> StyledString {
        if is_valid {
            styled_feedback(&format!("✓ {}", message), FeedbackLevel::Success)
        } else {
            styled_feedback(&format!("✖ {}", message), FeedbackLevel::Error)
        }
    }

    /// MCP server status
    pub fn mcp_server_status(status: ServerStatus) -> StyledString {
        match status {
            ServerStatus::Online => styled_badge("Online", FeedbackLevel::Success),
            ServerStatus::Offline => styled_badge("Offline", FeedbackLevel::Error),
            ServerStatus::Loading => styled_badge("Loading...", FeedbackLevel::Processing),
            ServerStatus::Error => styled_badge("Error", FeedbackLevel::Error),
        }
    }

    /// Tool execution status
    pub fn tool_status(status: ToolStatus) -> StyledString {
        match status {
            ToolStatus::Ready => styled_badge("Ready", FeedbackLevel::Success),
            ToolStatus::Running => styled_badge("Running", FeedbackLevel::Processing),
            ToolStatus::Completed => styled_badge("Completed", FeedbackLevel::Success),
            ToolStatus::Failed => styled_badge("Failed", FeedbackLevel::Error),
        }
    }
}

/// Message states
#[derive(Clone, Copy)]
pub enum MessageState {
    Sent,
    Sending,
    Failed,
    Queued,
}

/// Server status states
#[derive(Clone, Copy)]
pub enum ServerStatus {
    Online,
    Offline,
    Loading,
    Error,
}

/// Tool status states
#[derive(Clone, Copy)]
pub enum ToolStatus {
    Ready,
    Running,
    Completed,
    Failed,
}

/// Animated indicators
pub struct AnimatedIndicators;

impl AnimatedIndicators {
    /// Spinner frames for different states
    pub const SPINNER_DOTS: &'static [&'static str] = &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
    pub const SPINNER_CIRCLE: &'static [&'static str] = &["◐", "◓", "◑", "◒"];
    pub const SPINNER_PULSE: &'static [&'static str] = &["○", "◔", "◑", "◕", "●", "◕", "◑", "◔"];
    pub const SPINNER_BOUNCE: &'static [&'static str] = &["⠁", "⠂", "⠄", "⡀", "⢀", "⠠", "⠐", "⠈"];

    /// Get spinner frame
    pub fn spinner_frame(frame_index: usize, style: SpinnerStyle) -> &'static str {
        let frames = match style {
            SpinnerStyle::Dots => Self::SPINNER_DOTS,
            SpinnerStyle::Circle => Self::SPINNER_CIRCLE,
            SpinnerStyle::Pulse => Self::SPINNER_PULSE,
            SpinnerStyle::Bounce => Self::SPINNER_BOUNCE,
        };
        frames[frame_index % frames.len()]
    }

    /// Create animated spinner with message
    pub fn spinner_with_message(frame_index: usize, message: &str, style: SpinnerStyle) -> StyledString {
        let frame = Self::spinner_frame(frame_index, style);
        styled_feedback(&format!("{} {}", frame, message), FeedbackLevel::Processing)
    }
}

/// Spinner animation styles
#[derive(Clone, Copy)]
pub enum SpinnerStyle {
    Dots,
    Circle,
    Pulse,
    Bounce,
}

/// Accessibility helpers
pub struct AccessibilityHelpers;

impl AccessibilityHelpers {
    /// Create high-contrast styled text
    pub fn high_contrast_text(text: &str, level: FeedbackLevel) -> StyledString {
        let mut styled = StyledString::new();
        let style = match level {
            FeedbackLevel::Success => ColorStyle::new(Color::Rgb(0, 255, 0), Color::Rgb(0, 0, 0)),
            FeedbackLevel::Error => ColorStyle::new(Color::Rgb(255, 0, 0), Color::Rgb(0, 0, 0)),
            FeedbackLevel::Warning => ColorStyle::new(Color::Rgb(255, 255, 0), Color::Rgb(0, 0, 0)),
            FeedbackLevel::Info => ColorStyle::new(Color::Rgb(0, 255, 255), Color::Rgb(0, 0, 0)),
            _ => ColorStyle::new(Color::Rgb(255, 255, 255), Color::Rgb(0, 0, 0)),
        };

        styled.append_styled(format!("{} {}", level.icon(), text), style);
        styled
    }

    /// Add semantic prefix for screen readers
    pub fn screen_reader_text(text: &str, level: FeedbackLevel) -> String {
        let prefix = match level {
            FeedbackLevel::Success => "[SUCCESS]",
            FeedbackLevel::Error => "[ERROR]",
            FeedbackLevel::Warning => "[WARNING]",
            FeedbackLevel::Info => "[INFO]",
            FeedbackLevel::Processing => "[PROCESSING]",
            FeedbackLevel::Neutral => "",
        };

        if prefix.is_empty() {
            text.to_string()
        } else {
            format!("{} {}", prefix, text)
        }
    }

    /// Check if high contrast mode is needed (from environment)
    pub fn is_high_contrast_mode() -> bool {
        std::env::var("HIGH_CONTRAST").unwrap_or_default() == "1"
            || std::env::var("OSVM_HIGH_CONTRAST").unwrap_or_default() == "1"
    }
}

/// Create visual dividers and separators
pub struct Dividers;

impl Dividers {
    /// Horizontal divider
    pub fn horizontal(width: usize) -> String {
        "─".repeat(width)
    }

    /// Thick horizontal divider
    pub fn horizontal_thick(width: usize) -> String {
        "═".repeat(width)
    }

    /// Section divider with label
    pub fn section(label: &str, width: usize) -> String {
        let label_len = label.len() + 2; // Include spaces
        let side_len = (width.saturating_sub(label_len)) / 2;
        format!(
            "{}─ {} ─{}",
            "─".repeat(side_len),
            label,
            "─".repeat(width.saturating_sub(side_len + label_len))
        )
    }

    /// Box drawing characters
    pub const BOX_TOP_LEFT: &'static str = "┌";
    pub const BOX_TOP_RIGHT: &'static str = "┐";
    pub const BOX_BOTTOM_LEFT: &'static str = "└";
    pub const BOX_BOTTOM_RIGHT: &'static str = "┘";
    pub const BOX_HORIZONTAL: &'static str = "─";
    pub const BOX_VERTICAL: &'static str = "│";
    pub const BOX_T_DOWN: &'static str = "┬";
    pub const BOX_T_UP: &'static str = "┴";
    pub const BOX_T_RIGHT: &'static str = "├";
    pub const BOX_T_LEFT: &'static str = "┤";
    pub const BOX_CROSS: &'static str = "┼";

    /// Create a simple box around text
    pub fn box_around(text: &str, width: usize) -> Vec<String> {
        let mut lines = Vec::new();

        // Top border
        lines.push(format!(
            "{}{}{}",
            Self::BOX_TOP_LEFT,
            Self::BOX_HORIZONTAL.repeat(width - 2),
            Self::BOX_TOP_RIGHT
        ));

        // Content
        for line in text.lines() {
            let padding = width.saturating_sub(line.len() + 2);
            lines.push(format!(
                "{} {}{}{}",
                Self::BOX_VERTICAL,
                line,
                " ".repeat(padding),
                Self::BOX_VERTICAL
            ));
        }

        // Bottom border
        lines.push(format!(
            "{}{}{}",
            Self::BOX_BOTTOM_LEFT,
            Self::BOX_HORIZONTAL.repeat(width - 2),
            Self::BOX_BOTTOM_RIGHT
        ));

        lines
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_feedback_levels() {
        let success = FeedbackLevel::Success;
        assert_eq!(success.icon(), "✓");

        let error = FeedbackLevel::Error;
        assert_eq!(error.icon(), "✖");
    }

    #[test]
    fn test_dividers() {
        let divider = Dividers::horizontal(10);
        // Count characters, not bytes (─ is 3 bytes in UTF-8)
        assert_eq!(divider.chars().count(), 10);

        let section = Dividers::section("Test", 20);
        assert!(section.contains("Test"));
        assert!(section.contains("─"));
    }

    #[test]
    fn test_screen_reader_text() {
        let text = AccessibilityHelpers::screen_reader_text("Message", FeedbackLevel::Error);
        assert!(text.starts_with("[ERROR]"));
    }
}
