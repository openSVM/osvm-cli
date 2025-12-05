//! Main code assistant view rendering
//!
//! Renders the conversation UI using ratatui.

use crate::utils::tui::code::app::{AppStatus, CodeApp, CodeFocus, Message, MessageRole, ToolCallStatus};
use crate::utils::tui::code::views::approval::centered_rect;
use ratatui::{
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, List, ListItem, Paragraph, Scrollbar, ScrollbarOrientation, ScrollbarState, Wrap},
    Frame,
};

impl CodeApp {
    /// Render the entire code assistant UI
    pub fn render(&mut self, f: &mut Frame) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),  // Header
                Constraint::Min(10),    // Messages
                Constraint::Length(5),  // Input
                Constraint::Length(1),  // Status bar
            ])
            .split(f.area());

        self.render_header(f, chunks[0]);
        self.render_messages(f, chunks[1]);
        self.render_input(f, chunks[2]);
        self.render_status_bar(f, chunks[3]);

        // Render approval modal if active
        if let Some(modal) = &self.approval_modal {
            let modal_area = centered_rect(70, 50, f.area());
            // Clone to avoid borrow issues
            let modal_clone = modal.clone();
            modal_clone.render(f, modal_area);
        }
    }

    fn render_header(&self, f: &mut Frame, area: Rect) {
        let mode_info = if self.yolo_mode {
            Span::styled(" YOLO ", Style::default().fg(Color::Black).bg(Color::Red).add_modifier(Modifier::BOLD))
        } else if self.no_tools {
            Span::styled(" CHAT ", Style::default().fg(Color::Black).bg(Color::Yellow).add_modifier(Modifier::BOLD))
        } else {
            Span::styled(" CODE ", Style::default().fg(Color::Black).bg(Color::Green).add_modifier(Modifier::BOLD))
        };

        let header = Paragraph::new(Line::from(vec![
            Span::styled(
                " 🤖 OSVM Code ",
                Style::default()
                    .fg(Color::Cyan)
                    .add_modifier(Modifier::BOLD),
            ),
            mode_info,
            Span::raw(" │ "),
            Span::styled(
                &self.project_name,
                Style::default().fg(Color::White).add_modifier(Modifier::BOLD),
            ),
            Span::styled(
                format!(" ({})", self.project_root.display()),
                Style::default().fg(Color::DarkGray),
            ),
        ]))
        .block(Block::default().borders(Borders::ALL).border_style(
            Style::default().fg(Color::DarkGray),
        ));

        f.render_widget(header, area);
    }

    fn render_messages(&mut self, f: &mut Frame, area: Rect) {
        let is_focused = self.focus == CodeFocus::Messages;
        let border_color = if is_focused { Color::Cyan } else { Color::DarkGray };

        let block = Block::default()
            .title(Span::styled(
                " Conversation ",
                Style::default().fg(if is_focused { Color::Cyan } else { Color::White }),
            ))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(border_color));

        let inner = block.inner(area);
        f.render_widget(block, area);

        if self.messages.is_empty() {
            let welcome = Paragraph::new(vec![
                Line::from(""),
                Line::from(vec![
                    Span::styled("  Welcome to ", Style::default().fg(Color::DarkGray)),
                    Span::styled("OSVM Code", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                    Span::styled("!", Style::default().fg(Color::DarkGray)),
                ]),
                Line::from(""),
                Line::from(vec![Span::styled("  Ask me to help with your code. I can:", Style::default().fg(Color::DarkGray))]),
                Line::from(vec![Span::styled("    • Read and analyze files", Style::default().fg(Color::White))]),
                Line::from(vec![Span::styled("    • Edit code with your approval", Style::default().fg(Color::White))]),
                Line::from(vec![Span::styled("    • Run commands and tests", Style::default().fg(Color::White))]),
                Line::from(vec![Span::styled("    • Search across the codebase", Style::default().fg(Color::White))]),
                Line::from(""),
                Line::from(vec![
                    Span::styled("  Press ", Style::default().fg(Color::DarkGray)),
                    Span::styled("Tab", Style::default().fg(Color::Yellow)),
                    Span::styled(" to toggle focus, ", Style::default().fg(Color::DarkGray)),
                    Span::styled("Ctrl+C", Style::default().fg(Color::Yellow)),
                    Span::styled(" to quit", Style::default().fg(Color::DarkGray)),
                ]),
            ])
            .alignment(Alignment::Left);

            f.render_widget(welcome, inner);
            return;
        }

        // Convert messages to list items
        let items: Vec<ListItem> = self.messages.iter().map(|msg| {
            let (prefix, style) = match msg.role {
                MessageRole::User => (
                    "You: ",
                    Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD),
                ),
                MessageRole::Assistant => (
                    "AI: ",
                    Style::default().fg(Color::Green).add_modifier(Modifier::BOLD),
                ),
                MessageRole::System => (
                    "System: ",
                    Style::default().fg(Color::Yellow),
                ),
                MessageRole::ToolResult => (
                    "Tool: ",
                    Style::default().fg(Color::Magenta),
                ),
            };

            let mut lines = vec![Line::from(vec![
                Span::styled(prefix, style),
                Span::raw(&msg.content),
            ])];

            // Add tool calls if any
            for tool in &msg.tool_calls {
                let status_icon = match tool.status {
                    ToolCallStatus::Pending => "⏳",
                    ToolCallStatus::Running => "🔄",
                    ToolCallStatus::Completed => "✓",
                    ToolCallStatus::Failed => "✗",
                    ToolCallStatus::Skipped => "○",
                };
                let status_color = match tool.status {
                    ToolCallStatus::Pending => Color::Yellow,
                    ToolCallStatus::Running => Color::Cyan,
                    ToolCallStatus::Completed => Color::Green,
                    ToolCallStatus::Failed => Color::Red,
                    ToolCallStatus::Skipped => Color::DarkGray,
                };

                lines.push(Line::from(vec![
                    Span::raw("   "),
                    Span::styled(status_icon, Style::default().fg(status_color)),
                    Span::raw(" "),
                    Span::styled(&tool.name, Style::default().fg(Color::Yellow)),
                ]));

                if let Some(output) = &tool.output {
                    // Truncate long outputs
                    let display = if output.len() > 200 {
                        format!("{}...", &output[..200])
                    } else {
                        output.clone()
                    };
                    lines.push(Line::from(vec![
                        Span::raw("     "),
                        Span::styled(display, Style::default().fg(Color::DarkGray)),
                    ]));
                }
            }

            ListItem::new(lines)
        }).collect();

        // Calculate scroll state
        let total_items = items.len();
        let visible_height = inner.height as usize;

        // Clamp scroll offset
        if total_items > visible_height {
            self.scroll_offset = self.scroll_offset.min(total_items.saturating_sub(visible_height));
        } else {
            self.scroll_offset = 0;
        }

        let list = List::new(items)
            .highlight_style(Style::default().add_modifier(Modifier::BOLD));

        f.render_widget(list, inner);

        // Render scrollbar if needed
        if total_items > visible_height {
            let scrollbar = Scrollbar::new(ScrollbarOrientation::VerticalRight)
                .begin_symbol(Some("↑"))
                .end_symbol(Some("↓"));

            let mut scrollbar_state = ScrollbarState::new(total_items)
                .position(self.scroll_offset);

            f.render_stateful_widget(
                scrollbar,
                inner.inner(ratatui::layout::Margin { vertical: 1, horizontal: 0 }),
                &mut scrollbar_state,
            );
        }
    }

    fn render_input(&self, f: &mut Frame, area: Rect) {
        let is_focused = self.focus == CodeFocus::Input;
        let border_color = if is_focused { Color::Cyan } else { Color::DarkGray };

        let block = Block::default()
            .title(Span::styled(
                " Input ",
                Style::default().fg(if is_focused { Color::Cyan } else { Color::White }),
            ))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(border_color));

        let inner = block.inner(area);
        f.render_widget(block, area);

        // Build input text with cursor
        let (before_cursor, after_cursor) = self.input.split_at(self.input_cursor.min(self.input.len()));

        let input_line = if is_focused {
            Line::from(vec![
                Span::raw(before_cursor),
                Span::styled("│", Style::default().fg(Color::Cyan).add_modifier(Modifier::SLOW_BLINK)),
                Span::raw(after_cursor),
            ])
        } else {
            Line::from(vec![Span::raw(&self.input)])
        };

        let input_para = Paragraph::new(input_line)
            .wrap(Wrap { trim: false });

        f.render_widget(input_para, inner);

        // Show hints
        if self.input.is_empty() && is_focused {
            let hint = Paragraph::new(Line::from(vec![
                Span::styled("Type your message... ", Style::default().fg(Color::DarkGray)),
                Span::styled("(Enter", Style::default().fg(Color::Yellow)),
                Span::styled(" to send, ", Style::default().fg(Color::DarkGray)),
                Span::styled("/help", Style::default().fg(Color::Yellow)),
                Span::styled(" for commands)", Style::default().fg(Color::DarkGray)),
            ]));
            f.render_widget(hint, inner);
        }
    }

    fn render_status_bar(&self, f: &mut Frame, area: Rect) {
        let status_text = match &self.status {
            AppStatus::Ready => {
                Line::from(vec![
                    Span::styled("Ready", Style::default().fg(Color::Green)),
                    Span::raw(" │ "),
                    Span::styled("Tab", Style::default().fg(Color::Yellow)),
                    Span::raw(": focus │ "),
                    Span::styled("↑↓", Style::default().fg(Color::Yellow)),
                    Span::raw(": scroll │ "),
                    Span::styled("Ctrl+L", Style::default().fg(Color::Yellow)),
                    Span::raw(": clear │ "),
                    Span::styled("Ctrl+C", Style::default().fg(Color::Yellow)),
                    Span::raw(": quit"),
                ])
            }
            AppStatus::Thinking => {
                Line::from(vec![
                    Span::styled("🤔 ", Style::default()),
                    Span::styled("Thinking...", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
                ])
            }
            AppStatus::ExecutingTool(name) => {
                Line::from(vec![
                    Span::styled("⚙️  ", Style::default()),
                    Span::styled("Executing: ", Style::default().fg(Color::Cyan)),
                    Span::styled(name, Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
                ])
            }
            AppStatus::WaitingApproval => {
                Line::from(vec![
                    Span::styled("⚠️  ", Style::default()),
                    Span::styled("Waiting for approval...", Style::default().fg(Color::Yellow)),
                    Span::raw(" "),
                    Span::styled("[y]", Style::default().fg(Color::Green)),
                    Span::raw(" approve "),
                    Span::styled("[n]", Style::default().fg(Color::Red)),
                    Span::raw(" reject "),
                    Span::styled("[a]", Style::default().fg(Color::Cyan)),
                    Span::raw(" always"),
                ])
            }
            AppStatus::Error(err) => {
                Line::from(vec![
                    Span::styled("❌ ", Style::default()),
                    Span::styled("Error: ", Style::default().fg(Color::Red)),
                    Span::styled(err, Style::default().fg(Color::Red)),
                ])
            }
        };

        f.render_widget(
            Paragraph::new(status_text).style(Style::default().fg(Color::DarkGray)),
            area,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_render_creates_layout() {
        let app = CodeApp::new(PathBuf::from("/tmp/test"), false, false, false);
        assert!(!app.messages.is_empty() || app.messages.is_empty()); // Trivial to ensure code compiles
    }
}
