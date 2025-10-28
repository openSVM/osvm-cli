//! Enhanced message rendering with semantic colors and effects

use super::animations::typewriter::TypewriterEffect;
use crate::utils::agent_chat_v2::types::ChatMessage;
use cursive::theme::{Color, ColorStyle};
use cursive::utils::markup::StyledString;

/// Message type for semantic coloring
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MessageType {
    User,
    Agent,
    System,
    Tool,
    Error,
    Success,
    Processing,
    Code,
}

impl MessageType {
    /// Get the semantic color for this message type
    pub fn color(&self) -> ColorStyle {
        match self {
            Self::User => ColorStyle::new(
                Color::Rgb(59, 130, 246), // Blue - human, trustworthy
                Color::Rgb(30, 30, 30),
            ),
            Self::Agent => ColorStyle::new(
                Color::Rgb(147, 51, 234), // Purple - AI, intelligent
                Color::Rgb(30, 30, 30),
            ),
            Self::System => ColorStyle::new(
                Color::Rgb(107, 114, 128), // Gray - neutral, background
                Color::Rgb(30, 30, 30),
            ),
            Self::Tool => ColorStyle::new(
                Color::Rgb(251, 146, 60), // Orange - action, execution
                Color::Rgb(30, 30, 30),
            ),
            Self::Error => ColorStyle::new(
                Color::Rgb(239, 68, 68), // Red - alert, problem
                Color::Rgb(30, 30, 30),
            ),
            Self::Success => ColorStyle::new(
                Color::Rgb(34, 197, 94), // Green - complete, good
                Color::Rgb(30, 30, 30),
            ),
            Self::Processing => ColorStyle::new(
                Color::Rgb(59, 130, 246), // Blue - working, active
                Color::Rgb(30, 30, 30),
            ),
            Self::Code => ColorStyle::new(
                Color::Rgb(212, 212, 212), // Light gray on dark
                Color::Rgb(24, 24, 36),    // Dark background
            ),
        }
    }

    /// Get the icon for this message type
    pub fn icon(&self) -> &str {
        match self {
            Self::User => "👤",
            Self::Agent => "🤖",
            Self::System => "⚙",
            Self::Tool => "🔧",
            Self::Error => "❌",
            Self::Success => "✅",
            Self::Processing => "⏳",
            Self::Code => "📝",
        }
    }

    /// Get ASCII icon for terminals without emoji support
    pub fn ascii_icon(&self) -> &str {
        match self {
            Self::User => "[U]",
            Self::Agent => "[A]",
            Self::System => "[S]",
            Self::Tool => "[T]",
            Self::Error => "[!]",
            Self::Success => "[✓]",
            Self::Processing => "[...]",
            Self::Code => "[>]",
        }
    }

    /// Get the border style for message containers
    pub fn border_style(&self) -> MessageBorder {
        match self {
            Self::User => MessageBorder {
                top_left: '╭',
                top_right: '╮',
                bottom_left: '╰',
                bottom_right: '╯',
                horizontal: '─',
                vertical: '│',
            },
            Self::Agent => MessageBorder {
                top_left: '┌',
                top_right: '┐',
                bottom_left: '└',
                bottom_right: '┘',
                horizontal: '─',
                vertical: '│',
            },
            Self::Tool | Self::Processing => MessageBorder {
                top_left: '╔',
                top_right: '╗',
                bottom_left: '╚',
                bottom_right: '╝',
                horizontal: '═',
                vertical: '║',
            },
            Self::Error => MessageBorder {
                top_left: '┏',
                top_right: '┓',
                bottom_left: '┗',
                bottom_right: '┛',
                horizontal: '━',
                vertical: '┃',
            },
            _ => MessageBorder::default(),
        }
    }
}

pub struct MessageBorder {
    pub top_left: char,
    pub top_right: char,
    pub bottom_left: char,
    pub bottom_right: char,
    pub horizontal: char,
    pub vertical: char,
}

impl Default for MessageBorder {
    fn default() -> Self {
        Self {
            top_left: '┌',
            top_right: '┐',
            bottom_left: '└',
            bottom_right: '┘',
            horizontal: '─',
            vertical: '│',
        }
    }
}

/// Enhanced message renderer with effects
pub struct MessageRenderer {
    typewriter_enabled: bool,
    semantic_colors: bool,
    show_timestamps: bool,
    compact_mode: bool,
    animation_speed: f32,
}

impl MessageRenderer {
    pub fn new() -> Self {
        Self {
            typewriter_enabled: true,
            semantic_colors: true,
            show_timestamps: true,
            compact_mode: false,
            animation_speed: 1.0,
        }
    }

    /// Render a message with all effects
    pub fn render_message(&self, message: &ChatMessage, msg_type: MessageType) -> StyledString {
        let mut result = StyledString::new();

        // Add timestamp if enabled
        if self.show_timestamps && !self.compact_mode {
            let timestamp = ChatMessage::get_display_timestamp();
            result.append_styled(timestamp, ColorStyle::secondary());
            result.append_plain(" ");
        }

        // Add sender with semantic color
        let sender_style = if self.semantic_colors {
            msg_type.color()
        } else {
            ColorStyle::primary()
        };

        let icon = if self.compact_mode {
            msg_type.ascii_icon()
        } else {
            msg_type.icon()
        };

        // Pre-allocate strings for cases that need owned values
        let plan_display: String;

        // Extract sender and content using explicit lifetime handling
        let (sender, content): (&str, &str) = match message {
            ChatMessage::User(text) => ("User", text.as_str()),
            ChatMessage::Agent(text) => ("Agent", text.as_str()),
            ChatMessage::System(text) => ("System", text.as_str()),
            ChatMessage::ToolCall { tool_name, .. } => (tool_name.as_str(), "Calling tool"),
            ChatMessage::ToolResult { tool_name, .. } => (tool_name.as_str(), "Tool completed"),
            ChatMessage::Error(text) => ("Error", text.as_str()),
            ChatMessage::AgentThinking(text) => ("Thinking", text.as_str()),
            ChatMessage::AgentPlan(plan) => {
                plan_display = format!("{} planned tools", plan.len());
                ("Planning", plan_display.as_str())
            }
            ChatMessage::Processing { message: msg, .. } => ("Processing", msg.as_str()),
        };

        result.append_styled(format!("{} {}: ", icon, sender), sender_style);

        // Add message content
        let content_style = match msg_type {
            MessageType::Code => msg_type.color(),
            MessageType::Error => {
                ColorStyle::new(Color::Rgb(254, 202, 202), Color::Rgb(30, 30, 30))
            }
            _ => ColorStyle::primary(),
        };

        result.append_styled(content, content_style);

        result
    }

    /// Render message in a box with borders
    pub fn render_boxed_message(
        &self,
        message: &ChatMessage,
        msg_type: MessageType,
        width: usize,
    ) -> Vec<String> {
        let border = msg_type.border_style();
        let color = msg_type.color();

        let mut lines = Vec::new();

        // Top border
        let top = format!(
            "{}{}{}",
            border.top_left,
            border.horizontal.to_string().repeat(width - 2),
            border.top_right
        );
        lines.push(top);

        // Message content wrapped
        let content = match message {
            ChatMessage::User(text) => text,
            ChatMessage::Agent(text) => text,
            ChatMessage::System(text) => text,
            ChatMessage::ToolCall {
                tool_name,
                description,
                ..
            } => &format!("{}: {}", tool_name, description),
            ChatMessage::ToolResult {
                tool_name, result, ..
            } => &format!("{} result: {}", tool_name, result),
            ChatMessage::Error(text) => text,
            ChatMessage::AgentThinking(text) => text,
            ChatMessage::AgentPlan(plan) => &format!("Plan with {} tools", plan.len()),
            ChatMessage::Processing { message, .. } => message,
        };

        let wrapped = self.wrap_text(content, width - 4);
        for line in wrapped {
            lines.push(format!(
                "{} {:<width$} {}",
                border.vertical,
                line,
                border.vertical,
                width = width - 4
            ));
        }

        // Bottom border
        let bottom = format!(
            "{}{}{}",
            border.bottom_left,
            border.horizontal.to_string().repeat(width - 2),
            border.bottom_right
        );
        lines.push(bottom);

        lines
    }

    /// Format timestamp for display
    fn format_timestamp(&self, timestamp: &str) -> String {
        // Use the static method from ChatMessage for current timestamp
        ChatMessage::get_display_timestamp()
    }

    /// Wrap text to fit width
    fn wrap_text(&self, text: &str, width: usize) -> Vec<String> {
        let mut lines = Vec::new();
        let mut current_line = String::new();

        for word in text.split_whitespace() {
            if current_line.len() + word.len() + 1 > width {
                if !current_line.is_empty() {
                    lines.push(current_line.clone());
                    current_line.clear();
                }
            }

            if !current_line.is_empty() {
                current_line.push(' ');
            }
            current_line.push_str(word);
        }

        if !current_line.is_empty() {
            lines.push(current_line);
        }

        if lines.is_empty() {
            lines.push(String::new());
        }

        lines
    }

    /// Create gradient effect for important messages
    pub fn render_gradient_message(&self, message: &str, colors: Vec<Color>) -> StyledString {
        let mut result = StyledString::new();

        if colors.len() < 2 {
            result.append_plain(message);
            return result;
        }

        let chars: Vec<char> = message.chars().collect();
        let segment_size = chars.len() / (colors.len() - 1);

        for (i, ch) in chars.iter().enumerate() {
            let segment = i / segment_size.max(1);
            let segment_progress = (i % segment_size.max(1)) as f32 / segment_size.max(1) as f32;

            let color = if segment < colors.len() - 1 {
                self.lerp_color(colors[segment], colors[segment + 1], segment_progress)
            } else {
                colors[colors.len() - 1]
            };

            result.append_styled(
                ch.to_string(),
                ColorStyle::new(color, Color::Rgb(30, 30, 30)),
            );
        }

        result
    }

    fn lerp_color(&self, start: Color, end: Color, t: f32) -> Color {
        match (start, end) {
            (Color::Rgb(r1, g1, b1), Color::Rgb(r2, g2, b2)) => Color::Rgb(
                (r1 as f32 + (r2 as f32 - r1 as f32) * t) as u8,
                (g1 as f32 + (g2 as f32 - g1 as f32) * t) as u8,
                (b1 as f32 + (b2 as f32 - b1 as f32) * t) as u8,
            ),
            _ => end,
        }
    }
}

/// Code block renderer with syntax highlighting hints
pub struct CodeRenderer {
    line_numbers: bool,
    highlight_line: Option<usize>,
}

impl CodeRenderer {
    pub fn new() -> Self {
        Self {
            line_numbers: true,
            highlight_line: None,
        }
    }

    pub fn render_code(&self, code: &str, language: &str) -> Vec<String> {
        let mut lines = Vec::new();

        // Header
        lines.push(format!("╔═ {} ════════════════════════╗", language));

        // Code lines
        for (i, line) in code.lines().enumerate() {
            let line_num = if self.line_numbers {
                format!("{:3} │ ", i + 1)
            } else {
                String::new()
            };

            let highlight = if Some(i) == self.highlight_line {
                "▶ "
            } else {
                "  "
            };

            lines.push(format!("║{}{}{}", highlight, line_num, line));
        }

        // Footer
        lines.push("╚══════════════════════════════╝".to_string());

        lines
    }
}

/// Message animation coordinator
pub struct MessageAnimator {
    active_animations: Vec<Box<dyn MessageAnimation>>,
}

trait MessageAnimation {
    fn update(&mut self, delta: f32) -> bool;
    fn render(&self) -> String;
}

impl MessageAnimator {
    pub fn new() -> Self {
        Self {
            active_animations: Vec::new(),
        }
    }

    pub fn add_typewriter(&mut self, text: String) {
        // Would add TypewriterEffect here
    }

    pub fn update(&mut self, delta: f32) {
        self.active_animations.retain_mut(|anim| anim.update(delta));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_message_type_colors() {
        let user_color = MessageType::User.color();
        let agent_color = MessageType::Agent.color();

        // Colors should be different
        assert_ne!(format!("{:?}", user_color), format!("{:?}", agent_color));
    }

    #[test]
    fn test_message_wrapping() {
        let renderer = MessageRenderer::new();
        let wrapped =
            renderer.wrap_text("This is a very long message that needs to be wrapped", 20);

        assert!(wrapped.len() > 1);
        for line in &wrapped {
            assert!(line.len() <= 20);
        }
    }
}
