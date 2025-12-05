//! Approval modal for tool execution

use crate::utils::tui::code::permissions::{ApprovalRequest, ApprovalResponse};
use crate::utils::tui::code::tools::RiskLevel;
use ratatui::{
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, Paragraph, Wrap},
    Frame,
};

/// Modal for approving tool executions
pub struct ApprovalModal {
    pub request: ApprovalRequest,
    pub selected: ApprovalResponse,
}

impl ApprovalModal {
    pub fn new(request: ApprovalRequest) -> Self {
        Self {
            request,
            selected: ApprovalResponse::Approve,
        }
    }

    /// Move selection left
    pub fn select_prev(&mut self) {
        self.selected = match self.selected {
            ApprovalResponse::Approve => ApprovalResponse::Edit,
            ApprovalResponse::Reject => ApprovalResponse::Approve,
            ApprovalResponse::ApproveAlways => ApprovalResponse::Reject,
            ApprovalResponse::Edit => ApprovalResponse::ApproveAlways,
        };
    }

    /// Move selection right
    pub fn select_next(&mut self) {
        self.selected = match self.selected {
            ApprovalResponse::Approve => ApprovalResponse::Reject,
            ApprovalResponse::Reject => ApprovalResponse::ApproveAlways,
            ApprovalResponse::ApproveAlways => ApprovalResponse::Edit,
            ApprovalResponse::Edit => ApprovalResponse::Approve,
        };
    }

    /// Render the modal
    pub fn render(&self, f: &mut Frame, area: Rect) {
        // Clear the area
        f.render_widget(Clear, area);

        // Border color based on risk
        let border_color = match self.request.risk_level {
            RiskLevel::Safe => Color::Green,
            RiskLevel::Low => Color::Yellow,
            RiskLevel::Medium => Color::LightYellow,
            RiskLevel::High => Color::Red,
        };

        let block = Block::default()
            .title(" Approve Action? ")
            .title_style(Style::default().fg(Color::White).add_modifier(Modifier::BOLD))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(border_color));

        let inner = block.inner(area);
        f.render_widget(block, area);

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .margin(1)
            .constraints([
                Constraint::Length(2),  // Tool info
                Constraint::Min(5),     // Preview
                Constraint::Length(3),  // Actions
            ])
            .split(inner);

        // Tool info
        let risk_str = match self.request.risk_level {
            RiskLevel::Safe => ("Safe", Color::Green),
            RiskLevel::Low => ("Low", Color::Yellow),
            RiskLevel::Medium => ("Medium", Color::LightYellow),
            RiskLevel::High => ("High", Color::Red),
        };

        let info = Line::from(vec![
            Span::raw("Tool: "),
            Span::styled(&self.request.tool_name, Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::raw("  Risk: "),
            Span::styled(risk_str.0, Style::default().fg(risk_str.1)),
        ]);
        f.render_widget(Paragraph::new(info), chunks[0]);

        // Preview - use owned String to avoid lifetime issues
        let preview_content: String = self.request.preview.clone().unwrap_or_else(|| {
            // Default preview from params
            serde_json::to_string_pretty(&self.request.params).unwrap_or_default()
        });

        let preview = Paragraph::new(preview_content.as_str())
            .block(Block::default().borders(Borders::ALL).title(" Preview "))
            .wrap(Wrap { trim: false })
            .style(Style::default().fg(Color::Gray));
        f.render_widget(preview, chunks[1]);

        // Actions
        let actions = Line::from(vec![
            self.action_span("y", "Approve", ApprovalResponse::Approve),
            Span::raw("  "),
            self.action_span("n", "Reject", ApprovalResponse::Reject),
            Span::raw("  "),
            self.action_span("a", "Always", ApprovalResponse::ApproveAlways),
            Span::raw("  "),
            self.action_span("e", "Edit", ApprovalResponse::Edit),
        ]);

        f.render_widget(
            Paragraph::new(actions).alignment(Alignment::Center),
            chunks[2],
        );
    }

    fn action_span(&self, key: &str, label: &str, response: ApprovalResponse) -> Span<'static> {
        let is_selected = self.selected == response;
        let style = if is_selected {
            Style::default()
                .fg(Color::Black)
                .bg(Color::White)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(Color::White)
        };

        Span::styled(format!("[{}] {} ", key, label), style)
    }

    /// Handle key input, returns Some(response) if user made a choice
    pub fn handle_key(&mut self, key: crossterm::event::KeyCode) -> Option<ApprovalResponse> {
        use crossterm::event::KeyCode;

        match key {
            KeyCode::Char('y') | KeyCode::Enter if self.selected == ApprovalResponse::Approve => {
                Some(ApprovalResponse::Approve)
            }
            KeyCode::Char('n') | KeyCode::Esc => Some(ApprovalResponse::Reject),
            KeyCode::Char('a') => Some(ApprovalResponse::ApproveAlways),
            KeyCode::Char('e') => Some(ApprovalResponse::Edit),
            KeyCode::Left | KeyCode::Char('h') => {
                self.select_prev();
                None
            }
            KeyCode::Right | KeyCode::Char('l') => {
                self.select_next();
                None
            }
            KeyCode::Enter => Some(self.selected),
            _ => None,
        }
    }
}

/// Calculate a centered rectangle for the modal
pub fn centered_rect(percent_x: u16, percent_y: u16, area: Rect) -> Rect {
    let popup_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage((100 - percent_y) / 2),
            Constraint::Percentage(percent_y),
            Constraint::Percentage((100 - percent_y) / 2),
        ])
        .split(area);

    Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage((100 - percent_x) / 2),
            Constraint::Percentage(percent_x),
            Constraint::Percentage((100 - percent_x) / 2),
        ])
        .split(popup_layout[1])[1]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_modal_navigation() {
        let request = ApprovalRequest::new("test", serde_json::json!({}), RiskLevel::Low, None);
        let mut modal = ApprovalModal::new(request);

        assert_eq!(modal.selected, ApprovalResponse::Approve);

        modal.select_next();
        assert_eq!(modal.selected, ApprovalResponse::Reject);

        modal.select_next();
        assert_eq!(modal.selected, ApprovalResponse::ApproveAlways);

        modal.select_prev();
        assert_eq!(modal.selected, ApprovalResponse::Reject);
    }
}
