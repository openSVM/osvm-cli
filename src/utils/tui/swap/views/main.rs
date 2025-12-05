//! Main swap view rendering
//!
//! Renders the swap UI using ratatui.

use crate::utils::tui::swap::app::{SwapApp, SwapFocus, SwapStatus};
use crate::utils::tui::swap::wallet::format_balance;
use ratatui::{
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, List, ListItem, Paragraph},
    Frame,
};

impl SwapApp {
    /// Render the entire swap UI
    pub fn render(&mut self, f: &mut Frame) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3), // Header
                Constraint::Min(15),   // Main content
                Constraint::Length(3), // Action bar
                Constraint::Length(1), // Status bar
            ])
            .split(f.area());

        self.render_header(f, chunks[0]);
        self.render_main(f, chunks[1]);
        self.render_action_bar(f, chunks[2]);
        self.render_status_bar(f, chunks[3]);

        // Render token search modal if open
        if self.search_modal_open {
            self.render_search_modal(f);
        }

        // Render swap confirmation if active
        if let Some(SwapStatus::Confirming) = &self.swap_status {
            self.render_confirmation_modal(f);
        }
    }

    fn render_header(&self, f: &mut Frame, area: Rect) {
        let wallet_info = if let Some(w) = &self.wallet {
            let short_addr = format!(
                "{}...{}",
                &w.pubkey.to_string()[..4],
                &w.pubkey.to_string()[40..]
            );
            format!(
                " {} │ {:.4} SOL ",
                short_addr,
                w.sol_balance as f64 / 1e9
            )
        } else {
            " No wallet connected ".to_string()
        };

        let header = Paragraph::new(Line::from(vec![
            Span::styled(
                " ◎ OSVM Swap ",
                Style::default()
                    .fg(Color::Cyan)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::raw("│"),
            Span::styled(wallet_info, Style::default().fg(Color::Green)),
            Span::raw("│"),
            Span::styled(
                format!(" Slippage: {}% ", self.slippage_bps as f64 / 100.0),
                Style::default().fg(Color::Yellow),
            ),
        ]))
        .block(Block::default().borders(Borders::ALL).border_style(
            Style::default().fg(Color::DarkGray),
        ));

        f.render_widget(header, area);
    }

    fn render_main(&self, f: &mut Frame, area: Rect) {
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray))
            .title(" Swap ");

        let inner = block.inner(area);
        f.render_widget(block, area);

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .margin(1)
            .constraints([
                Constraint::Length(4), // From token
                Constraint::Length(1), // Swap arrow
                Constraint::Length(4), // To token
                Constraint::Length(1), // Spacer
                Constraint::Min(5),    // Quote info
            ])
            .split(inner);

        self.render_from_token(f, chunks[0]);
        self.render_swap_arrow(f, chunks[1]);
        self.render_to_token(f, chunks[2]);
        self.render_quote(f, chunks[4]);
    }

    fn render_from_token(&self, f: &mut Frame, area: Rect) {
        let is_focused = self.focus == SwapFocus::FromToken;
        let border_color = if is_focused { Color::Cyan } else { Color::DarkGray };

        let token_symbol = self
            .from_token
            .as_ref()
            .map(|t| t.symbol.as_str())
            .unwrap_or("Select ▼");

        let balance = self.get_from_balance();

        let block = Block::default()
            .title(Span::styled(
                " From ",
                Style::default().fg(if is_focused { Color::Cyan } else { Color::White }),
            ))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(border_color));

        let inner = block.inner(area);
        f.render_widget(block, area);

        // Layout: [Token] [Amount Input] [Balance]
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(10), // Token
                Constraint::Min(15),    // Amount
                Constraint::Length(15), // Balance
            ])
            .split(inner);

        // Token selector
        let token_style = Style::default()
            .fg(Color::White)
            .add_modifier(Modifier::BOLD);
        f.render_widget(
            Paragraph::new(token_symbol).style(token_style),
            chunks[0],
        );

        // Amount input
        let amount_focused = self.focus == SwapFocus::Amount;
        let amount_style = if amount_focused {
            Style::default().fg(Color::Cyan)
        } else {
            Style::default().fg(Color::White)
        };

        let amount_display = if self.amount_input.is_empty() {
            "0.0".to_string()
        } else {
            self.amount_input.clone()
        };

        let cursor = if amount_focused { "│" } else { "" };
        f.render_widget(
            Paragraph::new(format!("{}{}", amount_display, cursor))
                .style(amount_style)
                .alignment(Alignment::Right),
            chunks[1],
        );

        // Balance
        f.render_widget(
            Paragraph::new(format!("Bal: {}", format_balance(balance, 9)))
                .style(Style::default().fg(Color::DarkGray))
                .alignment(Alignment::Right),
            chunks[2],
        );
    }

    fn render_swap_arrow(&self, f: &mut Frame, area: Rect) {
        f.render_widget(
            Paragraph::new("        ↕ [s] switch")
                .style(Style::default().fg(Color::DarkGray))
                .alignment(Alignment::Center),
            area,
        );
    }

    fn render_to_token(&self, f: &mut Frame, area: Rect) {
        let is_focused = self.focus == SwapFocus::ToToken;
        let border_color = if is_focused { Color::Cyan } else { Color::DarkGray };

        let token_symbol = self
            .to_token
            .as_ref()
            .map(|t| t.symbol.as_str())
            .unwrap_or("Select ▼");

        let output = self.get_output_amount().unwrap_or(0.0);

        let block = Block::default()
            .title(Span::styled(
                " To ",
                Style::default().fg(if is_focused { Color::Cyan } else { Color::White }),
            ))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(border_color));

        let inner = block.inner(area);
        f.render_widget(block, area);

        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(10),
                Constraint::Min(15),
            ])
            .split(inner);

        // Token selector
        f.render_widget(
            Paragraph::new(token_symbol).style(
                Style::default()
                    .fg(Color::White)
                    .add_modifier(Modifier::BOLD),
            ),
            chunks[0],
        );

        // Estimated output
        let output_text = if self.quote_loading {
            "Loading...".to_string()
        } else if output > 0.0 {
            format!("≈ {}", format_balance(output, 6))
        } else {
            "0.0".to_string()
        };

        f.render_widget(
            Paragraph::new(output_text)
                .style(Style::default().fg(Color::Green))
                .alignment(Alignment::Right),
            chunks[1],
        );
    }

    fn render_quote(&self, f: &mut Frame, area: Rect) {
        let block = Block::default()
            .title(" Quote ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray));

        let inner = block.inner(area);
        f.render_widget(block, area);

        if self.quote_loading {
            f.render_widget(
                Paragraph::new("⏳ Fetching best route...")
                    .style(Style::default().fg(Color::Yellow)),
                inner,
            );
            return;
        }

        if let Some(error) = &self.quote_error {
            f.render_widget(
                Paragraph::new(format!("❌ {}", error)).style(Style::default().fg(Color::Red)),
                inner,
            );
            return;
        }

        if self.quote.is_some() {
            let impact = self.get_price_impact().unwrap_or(0.0);
            let impact_color = if impact < 0.1 {
                Color::Green
            } else if impact < 1.0 {
                Color::Yellow
            } else {
                Color::Red
            };

            let route = self.get_route_description().unwrap_or_else(|| "Direct".to_string());
            let min_received = self.get_min_received().unwrap_or(0.0);

            let to_symbol = self
                .to_token
                .as_ref()
                .map(|t| t.symbol.as_str())
                .unwrap_or("");

            let lines = vec![
                Line::from(vec![
                    Span::styled("Route: ", Style::default().fg(Color::DarkGray)),
                    Span::styled(route, Style::default().fg(Color::Cyan)),
                ]),
                Line::from(vec![
                    Span::styled("Price Impact: ", Style::default().fg(Color::DarkGray)),
                    Span::styled(format!("{:.4}%", impact), Style::default().fg(impact_color)),
                ]),
                Line::from(vec![
                    Span::styled("Min Received: ", Style::default().fg(Color::DarkGray)),
                    Span::styled(
                        format!("{} {}", format_balance(min_received, 6), to_symbol),
                        Style::default().fg(Color::White),
                    ),
                ]),
            ];

            f.render_widget(Paragraph::new(lines), inner);
        } else {
            f.render_widget(
                Paragraph::new("Enter amount to get quote")
                    .style(Style::default().fg(Color::DarkGray)),
                inner,
            );
        }
    }

    fn render_action_bar(&self, f: &mut Frame, area: Rect) {
        let can_swap = self.can_swap();

        let swap_style = if can_swap {
            Style::default()
                .fg(Color::Black)
                .bg(Color::Green)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(Color::DarkGray).bg(Color::Black)
        };

        let actions = Line::from(vec![
            Span::styled(" [Enter] Swap ", swap_style),
            Span::raw(" │ "),
            Span::styled("[r]", Style::default().fg(Color::Cyan)),
            Span::raw(" Refresh "),
            Span::raw("│ "),
            Span::styled("[s]", Style::default().fg(Color::Cyan)),
            Span::raw(" Switch "),
            Span::raw("│ "),
            Span::styled("[m]", Style::default().fg(Color::Cyan)),
            Span::raw(" Max "),
            Span::raw("│ "),
            Span::styled("[q]", Style::default().fg(Color::Red)),
            Span::raw(" Quit"),
        ]);

        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray));

        f.render_widget(
            Paragraph::new(actions)
                .alignment(Alignment::Center)
                .block(block),
            area,
        );
    }

    fn render_status_bar(&self, f: &mut Frame, area: Rect) {
        let status = if let Some(swap_status) = &self.swap_status {
            match swap_status {
                SwapStatus::Confirming => "Review and confirm swap...".to_string(),
                SwapStatus::Building => "🔨 Building transaction...".to_string(),
                SwapStatus::Signing => "✍️  Waiting for signature...".to_string(),
                SwapStatus::Submitting => "📤 Submitting to network...".to_string(),
                SwapStatus::WaitingConfirmation(sig) => format!("⏳ Confirming {}...", &sig[..8]),
                SwapStatus::Success(sig) => format!("✅ Success: {}", &sig[..16]),
                SwapStatus::Failed(err) => format!("❌ Failed: {}", err),
            }
        } else if let Some(msg) = &self.status_message {
            msg.clone()
        } else if let Some(time) = self.last_quote_time {
            format!("Quote updated {}s ago", time.elapsed().as_secs())
        } else {
            "Ready".to_string()
        };

        f.render_widget(
            Paragraph::new(status).style(Style::default().fg(Color::DarkGray)),
            area,
        );
    }

    fn render_search_modal(&self, f: &mut Frame) {
        let area = centered_rect(50, 60, f.area());

        // Clear the area first
        f.render_widget(Clear, area);

        let block = Block::default()
            .title(format!(
                " Select {} Token ",
                if self.focus == SwapFocus::FromToken {
                    "From"
                } else {
                    "To"
                }
            ))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Cyan));

        let inner = block.inner(area);
        f.render_widget(block, area);

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .margin(1)
            .constraints([
                Constraint::Length(3), // Search input
                Constraint::Min(5),    // Results
            ])
            .split(inner);

        // Search input
        let search_block = Block::default()
            .title(" Search ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Yellow));

        f.render_widget(
            Paragraph::new(format!("{}│", self.token_search))
                .style(Style::default().fg(Color::White))
                .block(search_block),
            chunks[0],
        );

        // Results
        let items: Vec<ListItem> = self
            .token_search_results
            .iter()
            .enumerate()
            .map(|(i, token)| {
                let style = if i == self.selected_search_idx {
                    Style::default()
                        .fg(Color::Black)
                        .bg(Color::Cyan)
                        .add_modifier(Modifier::BOLD)
                } else {
                    Style::default().fg(Color::White)
                };

                ListItem::new(Line::from(vec![
                    Span::styled(format!("{:6}", token.symbol), style.add_modifier(Modifier::BOLD)),
                    Span::styled(format!("  {}", token.name), style),
                ]))
            })
            .collect();

        let list = List::new(items).block(
            Block::default()
                .title(" Results ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::DarkGray)),
        );

        f.render_widget(list, chunks[1]);
    }

    fn render_confirmation_modal(&self, f: &mut Frame) {
        let area = centered_rect(60, 50, f.area());

        f.render_widget(Clear, area);

        let block = Block::default()
            .title(" Confirm Swap ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Yellow));

        let inner = block.inner(area);
        f.render_widget(block, area);

        let from_symbol = self
            .from_token
            .as_ref()
            .map(|t| t.symbol.as_str())
            .unwrap_or("?");
        let to_symbol = self
            .to_token
            .as_ref()
            .map(|t| t.symbol.as_str())
            .unwrap_or("?");
        let amount = self.amount_parsed.unwrap_or(0.0);
        let output = self.get_output_amount().unwrap_or(0.0);
        let min_received = self.get_min_received().unwrap_or(0.0);
        let impact = self.get_price_impact().unwrap_or(0.0);

        let lines = vec![
            Line::from(""),
            Line::from(vec![
                Span::styled("  Swapping: ", Style::default().fg(Color::DarkGray)),
                Span::styled(
                    format!("{} {}", format_balance(amount, 9), from_symbol),
                    Style::default().fg(Color::White).add_modifier(Modifier::BOLD),
                ),
            ]),
            Line::from(vec![
                Span::styled("  For:      ", Style::default().fg(Color::DarkGray)),
                Span::styled(
                    format!("≈ {} {}", format_balance(output, 6), to_symbol),
                    Style::default().fg(Color::Green).add_modifier(Modifier::BOLD),
                ),
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled("  Min received: ", Style::default().fg(Color::DarkGray)),
                Span::styled(
                    format!("{} {}", format_balance(min_received, 6), to_symbol),
                    Style::default().fg(Color::White),
                ),
            ]),
            Line::from(vec![
                Span::styled("  Price impact: ", Style::default().fg(Color::DarkGray)),
                Span::styled(
                    format!("{:.4}%", impact),
                    Style::default().fg(if impact < 1.0 { Color::Green } else { Color::Red }),
                ),
            ]),
            Line::from(""),
            Line::from(""),
            Line::from(vec![
                Span::styled(
                    "  [Enter] Confirm  ",
                    Style::default()
                        .fg(Color::Black)
                        .bg(Color::Green)
                        .add_modifier(Modifier::BOLD),
                ),
                Span::raw("  "),
                Span::styled(
                    "  [Esc] Cancel  ",
                    Style::default().fg(Color::White).bg(Color::DarkGray),
                ),
            ]),
        ];

        f.render_widget(Paragraph::new(lines), inner);
    }
}

/// Create a centered rect of given percentage size
fn centered_rect(percent_x: u16, percent_y: u16, r: Rect) -> Rect {
    let popup_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage((100 - percent_y) / 2),
            Constraint::Percentage(percent_y),
            Constraint::Percentage((100 - percent_y) / 2),
        ])
        .split(r);

    Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage((100 - percent_x) / 2),
            Constraint::Percentage(percent_x),
            Constraint::Percentage((100 - percent_x) / 2),
        ])
        .split(popup_layout[1])[1]
}
