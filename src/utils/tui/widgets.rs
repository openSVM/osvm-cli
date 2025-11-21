// Custom widgets for OSVM TUI
use ratatui::{
    layout::Rect,
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
    Frame,
};

pub struct LoadingSpinner {
    frame: usize,
    message: String,
}

impl LoadingSpinner {
    pub fn new(message: String) -> Self {
        Self { frame: 0, message }
    }

    pub fn tick(&mut self) {
        self.frame = (self.frame + 1) % 8;
    }

    pub fn render(&self, f: &mut Frame, area: Rect) {
        let spinner_chars = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧"];
        let spinner = spinner_chars[self.frame];

        let text = Line::from(vec![
            Span::styled(
                format!("{} ", spinner),
                Style::default().fg(Color::Cyan),
            ),
            Span::styled(&self.message, Style::default().fg(Color::White)),
        ]);

        let widget = Paragraph::new(text)
            .block(Block::default().borders(Borders::ALL).title("Status"));

        f.render_widget(widget, area);
    }
}

pub struct ProgressBar {
    current: usize,
    total: usize,
    label: String,
}

impl ProgressBar {
    pub fn new(label: String, total: usize) -> Self {
        Self {
            current: 0,
            total,
            label,
        }
    }

    pub fn update(&mut self, current: usize) {
        self.current = current;
    }

    pub fn render(&self, f: &mut Frame, area: Rect) {
        let percentage = if self.total > 0 {
            (self.current as f64 / self.total as f64 * 100.0) as usize
        } else {
            0
        };

        let filled = (area.width as f64 * self.current as f64 / self.total as f64) as usize;
        let bar = "█".repeat(filled) + &"░".repeat(area.width as usize - filled);

        let text = vec![
            Line::from(self.label.clone()),
            Line::from(format!("[{}] {}%", bar, percentage)),
            Line::from(format!("{}/{}", self.current, self.total)),
        ];

        let widget = Paragraph::new(text)
            .block(Block::default().borders(Borders::ALL).title("Progress"));

        f.render_widget(widget, area);
    }
}
