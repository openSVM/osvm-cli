//! Degen TUI Application State
//!
//! Main state struct and event loop for the degen trading dashboard.

use super::super::common::{centered_rect, format_duration};
use anyhow::Result;
use chrono::Utc;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    symbols,
    text::{Line, Span},
    widgets::{
        Axis, Block, Borders, Cell, Chart, Dataset, Gauge, List, ListItem, Paragraph, Row,
        Sparkline, Table, Tabs, Wrap,
    },
    Frame, Terminal,
};
use std::io;
use std::time::{Duration, Instant};

use crate::services::degen_agent::{
    AgentHandle, AgentState, DegenAgent, DegenConfig, GainerLoser, GainerTimeframe, NewTokenLaunch,
    Position, PositionStatus, SmartMoneyData, SmartMoneySignal, SmartMoneySignalType,
    TokenSafety, Trade, TradeSignal, TradeSide, TrenchesData, TrendingToken,
    TrackedWallet, WalletAction, WalletActivity, WalletType,
};
use crate::services::pumpfun_client::{PumpFunClient, PumpFunConfig};

/// Main degen application
pub struct DegenApp {
    // Configuration
    pub config: DegenConfig,

    // Agent
    pub agent: Option<DegenAgent>,
    pub agent_handle: Option<AgentHandle>,

    // Data client for pump.fun/OpenSVM
    pub pumpfun_client: PumpFunClient,
    pub last_data_refresh: Instant,

    // State cache (updated from agent)
    pub state: AgentState,
    pub positions: Vec<Position>,
    pub trades: Vec<Trade>,
    pub signals: Vec<TradeSignal>,

    // Trenches data (like GMGN.ai)
    pub trenches: TrenchesData,
    pub smart_money: SmartMoneyData,

    // UI State
    pub active_tab: usize,
    pub trenches_sub_tab: usize,  // 0=New, 1=Trending, 2=Gainers, 3=Losers
    pub scroll_offset: usize,
    pub should_quit: bool,
    pub show_help: bool,
    pub show_confirm_start: bool,
    pub show_confirm_stop: bool,

    // PnL history for chart
    pub pnl_history: Vec<f64>,

    // Status messages
    pub status_message: Option<String>,
    pub status_time: Option<Instant>,

    // Last update time
    pub last_update: Instant,
}

impl DegenApp {
    /// Create a new degen app
    pub fn new(config: DegenConfig) -> Self {
        let auto_start = config.auto_start;
        Self {
            config,
            agent: None,
            agent_handle: None,
            pumpfun_client: PumpFunClient::new(PumpFunConfig::default()),
            last_data_refresh: Instant::now() - Duration::from_secs(60), // Force initial refresh
            state: AgentState::default(),
            positions: Vec::new(),
            trades: Vec::new(),
            signals: Vec::new(),
            trenches: TrenchesData::default(),
            smart_money: SmartMoneyData::default(),
            active_tab: 0,
            trenches_sub_tab: 0,
            scroll_offset: 0,
            should_quit: false,
            show_help: false,
            show_confirm_start: !auto_start,
            show_confirm_stop: false,
            pnl_history: vec![0.0; 60], // Last 60 data points
            status_message: None,
            status_time: None,
            last_update: Instant::now(),
        }
    }

    /// Initialize the app
    pub async fn initialize(&mut self) -> Result<()> {
        // Create agent
        self.agent = Some(DegenAgent::new(self.config.clone()).await?);

        // Auto-start if configured
        if self.config.auto_start {
            self.start_agent().await?;
        }

        // Initial data refresh
        self.refresh_market_data().await;

        self.set_status("Initialized");
        Ok(())
    }

    /// Refresh market data from PumpFunClient (trenches, smart money)
    async fn refresh_market_data(&mut self) {
        // Refresh every 10 seconds
        if self.last_data_refresh.elapsed() < Duration::from_secs(10) {
            return;
        }
        self.last_data_refresh = Instant::now();

        // Fetch trending tokens for trenches
        match self.pumpfun_client.get_trending_tokens().await {
            Ok(trending) => {
                // Convert to our TrendingToken format
                self.trenches.trending = trending
                    .iter()
                    .take(20)
                    .map(|t| TrendingToken {
                        mint: t.mint.clone(),
                        symbol: t.symbol.clone(),
                        name: t.name.clone(),
                        price_sol: t.price_sol,
                        market_cap_sol: t.market_cap_sol,
                        volume_1h_sol: t.volume_24h_sol / 24.0, // Estimate
                        volume_24h_sol: t.volume_24h_sol,
                        holders: t.holder_count,
                        holder_change_1h: 0, // Not available from API
                        price_change_5m: t.price_change_5m,
                        price_change_1h: t.price_change_1h,
                        price_change_24h: t.price_change_24h,
                        buy_pressure: if t.buy_count_24h + t.sell_count_24h > 0 {
                            t.buy_count_24h as f64 / (t.buy_count_24h + t.sell_count_24h) as f64
                        } else {
                            0.5
                        },
                        smart_money_buys: 0, // Not available
                        trend_score: 50.0, // Default
                        safety: TokenSafety::default(),
                    })
                    .collect();

                // Sort for gainers/losers
                let mut sorted_by_change = self.trenches.trending.clone();
                sorted_by_change.sort_by(|a, b| b.price_change_24h.partial_cmp(&a.price_change_24h).unwrap_or(std::cmp::Ordering::Equal));

                // Top gainers (5m timeframe as placeholder)
                self.trenches.top_gainers_5m = sorted_by_change
                    .iter()
                    .filter(|t| t.price_change_24h > 0.0)
                    .take(10)
                    .map(|t| GainerLoser {
                        mint: t.mint.clone(),
                        symbol: t.symbol.clone(),
                        name: t.name.clone(),
                        price_sol: t.price_sol,
                        market_cap_sol: t.market_cap_sol,
                        price_change_pct: t.price_change_24h,
                        volume_sol: t.volume_24h_sol,
                        timeframe: GainerTimeframe::FiveMin,
                    })
                    .collect();

                // Top gainers (1h timeframe)
                self.trenches.top_gainers_1h = sorted_by_change
                    .iter()
                    .filter(|t| t.price_change_24h > 0.0)
                    .take(10)
                    .map(|t| GainerLoser {
                        mint: t.mint.clone(),
                        symbol: t.symbol.clone(),
                        name: t.name.clone(),
                        price_sol: t.price_sol,
                        market_cap_sol: t.market_cap_sol,
                        price_change_pct: t.price_change_24h,
                        volume_sol: t.volume_24h_sol,
                        timeframe: GainerTimeframe::OneHour,
                    })
                    .collect();

                // Top losers (5m timeframe)
                self.trenches.top_losers_5m = sorted_by_change
                    .iter()
                    .rev()
                    .filter(|t| t.price_change_24h < 0.0)
                    .take(10)
                    .map(|t| GainerLoser {
                        mint: t.mint.clone(),
                        symbol: t.symbol.clone(),
                        name: t.name.clone(),
                        price_sol: t.price_sol,
                        market_cap_sol: t.market_cap_sol,
                        price_change_pct: t.price_change_24h,
                        volume_sol: t.volume_24h_sol,
                        timeframe: GainerTimeframe::FiveMin,
                    })
                    .collect();

                // Top losers (1h timeframe)
                self.trenches.top_losers_1h = sorted_by_change
                    .iter()
                    .rev()
                    .filter(|t| t.price_change_24h < 0.0)
                    .take(10)
                    .map(|t| GainerLoser {
                        mint: t.mint.clone(),
                        symbol: t.symbol.clone(),
                        name: t.name.clone(),
                        price_sol: t.price_sol,
                        market_cap_sol: t.market_cap_sol,
                        price_change_pct: t.price_change_24h,
                        volume_sol: t.volume_24h_sol,
                        timeframe: GainerTimeframe::OneHour,
                    })
                    .collect();

                self.trenches.last_updated = Some(Utc::now());
            }
            Err(e) => {
                log::warn!("Failed to fetch trending tokens: {}", e);
            }
        }
    }

    /// Start the agent
    pub async fn start_agent(&mut self) -> Result<()> {
        if let Some(agent) = self.agent.take() {
            self.agent_handle = Some(agent.start().await?);
            self.set_status("Agent started");
        }
        Ok(())
    }

    /// Stop the agent
    pub async fn stop_agent(&mut self) -> Result<()> {
        if let Some(handle) = &self.agent_handle {
            handle.stop().await?;
            self.set_status("Agent stopped");
        }
        self.agent_handle = None;
        Ok(())
    }

    /// Toggle pause/resume
    pub async fn toggle_pause(&mut self) {
        if let Some(handle) = &self.agent_handle {
            if self.state.is_paused {
                handle.resume().await;
                self.set_status("Trading resumed");
            } else {
                handle.pause().await;
                self.set_status("Trading paused");
            }
        }
    }

    /// Set status message
    fn set_status(&mut self, msg: &str) {
        self.status_message = Some(msg.to_string());
        self.status_time = Some(Instant::now());
    }

    /// Update state from agent
    async fn update_from_agent(&mut self) {
        if let Some(handle) = &self.agent_handle {
            self.state = handle.state().await;
            self.positions = handle.positions().await;
            self.trades = handle.trades().await;
            self.signals = handle.signals().await;
        }

        // Update PnL history
        self.pnl_history.push(self.state.daily_pnl_sol);
        if self.pnl_history.len() > 60 {
            self.pnl_history.remove(0);
        }
    }

    /// Run the main event loop
    pub async fn run(&mut self, terminal: &mut Terminal<CrosstermBackend<io::Stdout>>) -> Result<()> {
        loop {
            // Update from agent periodically
            if self.last_update.elapsed() >= Duration::from_millis(500) {
                self.update_from_agent().await;
                self.last_update = Instant::now();
            }

            // Refresh market data (trenches, smart money) every 10 seconds
            self.refresh_market_data().await;

            // Clear old status messages
            if let Some(time) = self.status_time {
                if time.elapsed() > Duration::from_secs(5) {
                    self.status_message = None;
                    self.status_time = None;
                }
            }

            // Render
            terminal.draw(|f| self.render(f))?;

            // Handle events
            if event::poll(Duration::from_millis(100))? {
                if let Event::Key(key) = event::read()? {
                    self.handle_key(key).await?;
                }
            }

            if self.should_quit {
                // Clean shutdown
                if self.agent_handle.is_some() {
                    self.stop_agent().await?;
                }
                break;
            }
        }

        Ok(())
    }

    /// Handle a key event
    async fn handle_key(&mut self, key: KeyEvent) -> Result<()> {
        // Help overlay
        if self.show_help {
            if key.code == KeyCode::Esc || key.code == KeyCode::Char('?') {
                self.show_help = false;
            }
            return Ok(());
        }

        // Start confirmation
        if self.show_confirm_start {
            match key.code {
                KeyCode::Char('y') | KeyCode::Char('Y') => {
                    self.show_confirm_start = false;
                    self.start_agent().await?;
                }
                KeyCode::Char('n') | KeyCode::Char('N') | KeyCode::Esc => {
                    self.show_confirm_start = false;
                }
                _ => {}
            }
            return Ok(());
        }

        // Stop confirmation
        if self.show_confirm_stop {
            match key.code {
                KeyCode::Char('y') | KeyCode::Char('Y') => {
                    self.show_confirm_stop = false;
                    self.stop_agent().await?;
                }
                KeyCode::Char('n') | KeyCode::Char('N') | KeyCode::Esc => {
                    self.show_confirm_stop = false;
                }
                _ => {}
            }
            return Ok(());
        }

        // Main controls
        match key.code {
            KeyCode::Char('q') | KeyCode::Esc => {
                if self.state.is_running {
                    self.show_confirm_stop = true;
                } else {
                    self.should_quit = true;
                }
            }
            KeyCode::Char('Q') => {
                self.should_quit = true;
            }
            KeyCode::Char('?') => {
                self.show_help = true;
            }
            KeyCode::Char('s') | KeyCode::Char('S') => {
                if !self.state.is_running && self.agent_handle.is_none() {
                    self.show_confirm_start = true;
                }
            }
            KeyCode::Char('p') | KeyCode::Char('P') => {
                self.toggle_pause().await;
            }
            KeyCode::Char('x') | KeyCode::Char('X') => {
                if self.state.is_running {
                    self.show_confirm_stop = true;
                }
            }
            // Tab navigation
            KeyCode::Tab => {
                self.active_tab = (self.active_tab + 1) % 6;
            }
            KeyCode::BackTab => {
                self.active_tab = if self.active_tab == 0 { 5 } else { self.active_tab - 1 };
            }
            // Left/Right for sub-tabs in Trenches
            KeyCode::Right => {
                if self.active_tab == 1 {
                    self.trenches_sub_tab = (self.trenches_sub_tab + 1) % 4;
                } else {
                    self.active_tab = (self.active_tab + 1) % 6;
                }
            }
            KeyCode::Left => {
                if self.active_tab == 1 {
                    self.trenches_sub_tab = if self.trenches_sub_tab == 0 { 3 } else { self.trenches_sub_tab - 1 };
                } else {
                    self.active_tab = if self.active_tab == 0 { 5 } else { self.active_tab - 1 };
                }
            }
            KeyCode::Char('1') => self.active_tab = 0,
            KeyCode::Char('2') => self.active_tab = 1,
            KeyCode::Char('3') => self.active_tab = 2,
            KeyCode::Char('4') => self.active_tab = 3,
            KeyCode::Char('5') => self.active_tab = 4,
            KeyCode::Char('6') => self.active_tab = 5,
            // Scrolling
            KeyCode::Up | KeyCode::Char('k') => {
                if self.scroll_offset > 0 {
                    self.scroll_offset -= 1;
                }
            }
            KeyCode::Down | KeyCode::Char('j') => {
                self.scroll_offset += 1;
            }
            KeyCode::Home => self.scroll_offset = 0,
            _ => {}
        }

        Ok(())
    }

    /// Render the UI
    fn render(&self, f: &mut Frame) {
        let size = f.area();

        // Main layout
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),  // Header
                Constraint::Length(3),  // Tabs
                Constraint::Min(10),    // Main content
                Constraint::Length(3),  // Status bar
            ])
            .split(size);

        self.render_header(f, chunks[0]);
        self.render_tabs(f, chunks[1]);
        self.render_content(f, chunks[2]);
        self.render_status_bar(f, chunks[3]);

        // Overlays
        if self.show_help {
            self.render_help_overlay(f, size);
        }
        if self.show_confirm_start {
            self.render_confirm_dialog(f, size, "Start Trading Agent?",
                "This will begin autonomous trading with your configuration.");
        }
        if self.show_confirm_stop {
            self.render_confirm_dialog(f, size, "Stop Trading Agent?",
                "All open positions will remain. Stop monitoring?");
        }
    }

    fn render_header(&self, f: &mut Frame, area: Rect) {
        let mode = if self.config.dry_run { "PAPER" } else { "LIVE" };
        let mode_color = if self.config.dry_run { Color::Yellow } else { Color::Red };

        let status = if self.state.is_running {
            if self.state.is_paused { "⏸ PAUSED" } else { "🟢 RUNNING" }
        } else {
            "⏹ STOPPED"
        };

        let uptime = format_duration(self.state.uptime);

        let header_text = vec![
            Line::from(vec![
                Span::styled("🎰 DEGEN MODE ", Style::default().fg(Color::Magenta).add_modifier(Modifier::BOLD)),
                Span::styled(format!("[{}] ", mode), Style::default().fg(mode_color).add_modifier(Modifier::BOLD)),
                Span::styled(status, Style::default().fg(Color::Green)),
                Span::raw("  │  "),
                Span::styled(format!("Strategy: {}", self.config.strategy), Style::default().fg(Color::Cyan)),
                Span::raw("  │  "),
                Span::styled(format!("Uptime: {}", uptime), Style::default().fg(Color::White)),
            ]),
        ];

        let header = Paragraph::new(header_text)
            .block(Block::default().borders(Borders::BOTTOM).border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(header, area);
    }

    fn render_tabs(&self, f: &mut Frame, area: Rect) {
        let titles = vec![
            "📊 Dashboard",
            "🔥 Trenches",
            "🐋 Smart Money",
            "💼 Positions",
            "📜 Trades",
            "⚙️ Config",
        ];
        let tabs = Tabs::new(titles)
            .select(self.active_tab)
            .style(Style::default().fg(Color::White))
            .highlight_style(Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
            .divider(Span::raw(" │ "));

        f.render_widget(tabs, area);
    }

    fn render_content(&self, f: &mut Frame, area: Rect) {
        match self.active_tab {
            0 => self.render_dashboard(f, area),
            1 => self.render_trenches(f, area),
            2 => self.render_smart_money(f, area),
            3 => self.render_positions(f, area),
            4 => self.render_trades(f, area),
            5 => self.render_config(f, area),
            _ => {}
        }
    }

    fn render_dashboard(&self, f: &mut Frame, area: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(40), Constraint::Percentage(60)])
            .split(area);

        // Left side - Stats
        let left_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(8),  // PnL Stats
                Constraint::Length(6),  // Trade Stats
                Constraint::Min(5),     // Recent Signals
            ])
            .split(chunks[0]);

        self.render_pnl_stats(f, left_chunks[0]);
        self.render_trade_stats(f, left_chunks[1]);
        self.render_recent_signals(f, left_chunks[2]);

        // Right side - Chart and positions summary
        let right_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(chunks[1]);

        self.render_pnl_chart(f, right_chunks[0]);
        self.render_positions_summary(f, right_chunks[1]);
    }

    fn render_pnl_stats(&self, f: &mut Frame, area: Rect) {
        let daily_color = if self.state.daily_pnl_sol >= 0.0 { Color::Green } else { Color::Red };
        let total_color = if self.state.total_pnl_sol >= 0.0 { Color::Green } else { Color::Red };

        let text = vec![
            Line::from(vec![
                Span::styled("Daily P&L:  ", Style::default().fg(Color::Gray)),
                Span::styled(
                    format!("{:+.4} SOL", self.state.daily_pnl_sol),
                    Style::default().fg(daily_color).add_modifier(Modifier::BOLD),
                ),
            ]),
            Line::from(vec![
                Span::styled("Total P&L:  ", Style::default().fg(Color::Gray)),
                Span::styled(
                    format!("{:+.4} SOL", self.state.total_pnl_sol),
                    Style::default().fg(total_color).add_modifier(Modifier::BOLD),
                ),
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled("Balance:    ", Style::default().fg(Color::Gray)),
                Span::styled(
                    format!("{:.4} SOL", self.state.current_balance_sol),
                    Style::default().fg(Color::White),
                ),
            ]),
            Line::from(vec![
                Span::styled("Max Loss:   ", Style::default().fg(Color::Gray)),
                Span::styled(
                    format!("{:.2} SOL", self.config.max_daily_loss_sol),
                    Style::default().fg(Color::Yellow),
                ),
            ]),
        ];

        let block = Block::default()
            .title(" 💰 P&L ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray));

        let paragraph = Paragraph::new(text).block(block);
        f.render_widget(paragraph, area);
    }

    fn render_trade_stats(&self, f: &mut Frame, area: Rect) {
        let win_rate = if self.state.total_trades > 0 {
            (self.state.winning_trades as f64 / self.state.total_trades as f64) * 100.0
        } else {
            0.0
        };

        let text = vec![
            Line::from(vec![
                Span::styled("Total:  ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{}", self.state.total_trades), Style::default().fg(Color::White)),
            ]),
            Line::from(vec![
                Span::styled("Wins:   ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{}", self.state.winning_trades), Style::default().fg(Color::Green)),
                Span::raw("  "),
                Span::styled("Losses: ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{}", self.state.losing_trades), Style::default().fg(Color::Red)),
            ]),
            Line::from(vec![
                Span::styled("Win Rate: ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{:.1}%", win_rate), Style::default().fg(Color::Cyan)),
            ]),
        ];

        let block = Block::default()
            .title(" 📈 Trades ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray));

        let paragraph = Paragraph::new(text).block(block);
        f.render_widget(paragraph, area);
    }

    fn render_recent_signals(&self, f: &mut Frame, area: Rect) {
        let items: Vec<ListItem> = self.signals
            .iter()
            .rev()
            .take(10)
            .map(|s| {
                let icon = match s.signal_type {
                    crate::services::degen_agent::SignalType::Buy => "🟢",
                    crate::services::degen_agent::SignalType::Sell => "🔴",
                    crate::services::degen_agent::SignalType::Hold => "🟡",
                };
                ListItem::new(Line::from(vec![
                    Span::raw(format!("{} ", icon)),
                    Span::styled(&s.symbol, Style::default().fg(Color::Cyan)),
                    Span::raw(format!(" ({:.0}%) ", s.confidence * 100.0)),
                ]))
            })
            .collect();

        let list = List::new(items)
            .block(Block::default()
                .title(" 📡 Signals ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(list, area);
    }

    fn render_pnl_chart(&self, f: &mut Frame, area: Rect) {
        // Convert to u64 for sparkline (offset to handle negatives)
        let min_val = self.pnl_history.iter().cloned().fold(f64::INFINITY, f64::min);
        let offset = if min_val < 0.0 { -min_val } else { 0.0 };
        let max_val = self.pnl_history.iter().cloned().fold(f64::NEG_INFINITY, f64::max) + offset;

        let data: Vec<u64> = self.pnl_history
            .iter()
            .map(|v| {
                let normalized = (v + offset) / max_val.max(1.0);
                (normalized * 100.0).max(0.0) as u64
            })
            .collect();

        let sparkline = Sparkline::default()
            .block(Block::default()
                .title(" 📊 P&L Trend ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::DarkGray)))
            .data(&data)
            .style(Style::default().fg(if self.state.daily_pnl_sol >= 0.0 { Color::Green } else { Color::Red }));

        f.render_widget(sparkline, area);
    }

    fn render_positions_summary(&self, f: &mut Frame, area: Rect) {
        let open_positions: Vec<&Position> = self.positions
            .iter()
            .filter(|p| p.status == PositionStatus::Open)
            .collect();

        if open_positions.is_empty() {
            let text = Paragraph::new("No open positions")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default()
                    .title(" 💼 Open Positions ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray)));
            f.render_widget(text, area);
            return;
        }

        let rows: Vec<Row> = open_positions
            .iter()
            .map(|p| {
                let pnl_color = if p.unrealized_pnl_sol >= 0.0 { Color::Green } else { Color::Red };
                Row::new(vec![
                    Cell::from(p.symbol.clone()),
                    Cell::from(format!("{:.4}", p.cost_basis_sol)),
                    Cell::from(format!("{:+.4}", p.unrealized_pnl_sol)).style(Style::default().fg(pnl_color)),
                    Cell::from(format!("{:+.1}%", p.unrealized_pnl_pct)).style(Style::default().fg(pnl_color)),
                ])
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Percentage(30),
                Constraint::Percentage(25),
                Constraint::Percentage(25),
                Constraint::Percentage(20),
            ],
        )
        .header(Row::new(vec!["Token", "Size", "P&L", "%"]).style(Style::default().fg(Color::Yellow)))
        .block(Block::default()
            .title(format!(" 💼 Open Positions ({}) ", open_positions.len()))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, area);
    }

    fn render_positions(&self, f: &mut Frame, area: Rect) {
        if self.positions.is_empty() {
            let text = Paragraph::new("No positions yet")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default()
                    .title(" 💼 All Positions ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray)));
            f.render_widget(text, area);
            return;
        }

        let rows: Vec<Row> = self.positions
            .iter()
            .skip(self.scroll_offset)
            .map(|p| {
                let status_icon = match p.status {
                    PositionStatus::Open => "🟢",
                    PositionStatus::ClosedProfit => "💰",
                    PositionStatus::ClosedLoss => "📉",
                    PositionStatus::ClosedStopLoss => "🛑",
                    PositionStatus::ClosedTakeProfit => "🎯",
                };
                let pnl_color = if p.unrealized_pnl_sol >= 0.0 { Color::Green } else { Color::Red };

                Row::new(vec![
                    Cell::from(status_icon),
                    Cell::from(p.symbol.clone()),
                    Cell::from(format!("{:.6}", p.entry_price_sol)),
                    Cell::from(format!("{:.6}", p.current_price_sol)),
                    Cell::from(format!("{:.4}", p.cost_basis_sol)),
                    Cell::from(format!("{:+.4}", p.unrealized_pnl_sol)).style(Style::default().fg(pnl_color)),
                    Cell::from(format!("{:+.1}%", p.unrealized_pnl_pct)).style(Style::default().fg(pnl_color)),
                ])
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(3),
                Constraint::Percentage(15),
                Constraint::Percentage(15),
                Constraint::Percentage(15),
                Constraint::Percentage(15),
                Constraint::Percentage(15),
                Constraint::Percentage(10),
            ],
        )
        .header(Row::new(vec!["", "Token", "Entry", "Current", "Size", "P&L", "%"])
            .style(Style::default().fg(Color::Yellow)))
        .block(Block::default()
            .title(" 💼 All Positions ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, area);
    }

    fn render_trades(&self, f: &mut Frame, area: Rect) {
        if self.trades.is_empty() {
            let text = Paragraph::new("No trades yet")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default()
                    .title(" 📜 Trade History ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray)));
            f.render_widget(text, area);
            return;
        }

        let rows: Vec<Row> = self.trades
            .iter()
            .rev()
            .skip(self.scroll_offset)
            .map(|t| {
                let side_color = match t.side {
                    TradeSide::Buy => Color::Green,
                    TradeSide::Sell => Color::Red,
                };
                let side_text = match t.side {
                    TradeSide::Buy => "BUY",
                    TradeSide::Sell => "SELL",
                };

                Row::new(vec![
                    Cell::from(t.timestamp.format("%H:%M:%S").to_string()),
                    Cell::from(side_text).style(Style::default().fg(side_color)),
                    Cell::from(t.symbol.clone()),
                    Cell::from(format!("{:.6}", t.price_sol)),
                    Cell::from(format!("{:.4}", t.value_sol)),
                    Cell::from(t.strategy.clone()),
                ])
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(10),
                Constraint::Length(6),
                Constraint::Percentage(20),
                Constraint::Percentage(20),
                Constraint::Percentage(15),
                Constraint::Percentage(20),
            ],
        )
        .header(Row::new(vec!["Time", "Side", "Token", "Price", "Value", "Strategy"])
            .style(Style::default().fg(Color::Yellow)))
        .block(Block::default()
            .title(" 📜 Trade History ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, area);
    }

    fn render_config(&self, f: &mut Frame, area: Rect) {
        let text = vec![
            Line::from(""),
            Line::from(vec![
                Span::styled("Strategy:          ", Style::default().fg(Color::Gray)),
                Span::styled(&self.config.strategy, Style::default().fg(Color::Cyan)),
            ]),
            Line::from(vec![
                Span::styled("Max Position:      ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{} SOL", self.config.max_position_sol), Style::default().fg(Color::White)),
            ]),
            Line::from(vec![
                Span::styled("Max Positions:     ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{}", self.config.max_positions), Style::default().fg(Color::White)),
            ]),
            Line::from(vec![
                Span::styled("Min Liquidity:     ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{} SOL", self.config.min_liquidity_sol), Style::default().fg(Color::White)),
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled("Stop Loss:         ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{}%", self.config.stop_loss_pct), Style::default().fg(Color::Red)),
            ]),
            Line::from(vec![
                Span::styled("Take Profit:       ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{}%", self.config.take_profit_pct), Style::default().fg(Color::Green)),
            ]),
            Line::from(vec![
                Span::styled("Trailing Stop:     ", Style::default().fg(Color::Gray)),
                Span::styled(
                    if self.config.trailing_stop { "Enabled" } else { "Disabled" },
                    Style::default().fg(if self.config.trailing_stop { Color::Green } else { Color::DarkGray }),
                ),
            ]),
            Line::from(vec![
                Span::styled("Max Daily Loss:    ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{} SOL", self.config.max_daily_loss_sol), Style::default().fg(Color::Yellow)),
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled("Slippage:          ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{} bps ({}%)", self.config.slippage_bps, self.config.slippage_bps as f64 / 100.0), Style::default().fg(Color::White)),
            ]),
            Line::from(vec![
                Span::styled("Cooldown:          ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{} seconds", self.config.cooldown_secs), Style::default().fg(Color::White)),
            ]),
            Line::from(vec![
                Span::styled("Mode:              ", Style::default().fg(Color::Gray)),
                Span::styled(
                    if self.config.dry_run { "Paper Trading" } else { "Live Trading" },
                    Style::default().fg(if self.config.dry_run { Color::Yellow } else { Color::Red }),
                ),
            ]),
        ];

        let block = Block::default()
            .title(" ⚙️ Configuration ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray));

        let paragraph = Paragraph::new(text).block(block);
        f.render_widget(paragraph, area);
    }

    // ========================================================================
    // Trenches Tab - New Tokens, Trending, Gainers/Losers
    // ========================================================================

    fn render_trenches(&self, f: &mut Frame, area: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),  // Sub-tabs
                Constraint::Min(10),    // Content
            ])
            .split(area);

        // Sub-tabs for trenches
        let sub_titles = vec!["🆕 New", "📈 Trending", "🚀 Gainers", "📉 Losers"];
        let sub_tabs = Tabs::new(sub_titles)
            .select(self.trenches_sub_tab)
            .style(Style::default().fg(Color::DarkGray))
            .highlight_style(Style::default().fg(Color::Magenta).add_modifier(Modifier::BOLD))
            .divider(Span::raw(" | "));
        f.render_widget(sub_tabs, chunks[0]);

        // Render sub-tab content
        match self.trenches_sub_tab {
            0 => self.render_new_tokens(f, chunks[1]),
            1 => self.render_trending_tokens(f, chunks[1]),
            2 => self.render_gainers(f, chunks[1]),
            3 => self.render_losers(f, chunks[1]),
            _ => {}
        }
    }

    fn render_new_tokens(&self, f: &mut Frame, area: Rect) {
        if self.trenches.new_tokens.is_empty() {
            let text = Paragraph::new("Fetching new token launches from pump.fun...\n\n💡 New tokens will appear here in real-time")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default()
                    .title(" 🆕 New Token Launches ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray)));
            f.render_widget(text, area);
            return;
        }

        let rows: Vec<Row> = self.trenches.new_tokens
            .iter()
            .take(20)
            .map(|t| {
                let age_str = format!("{}m", t.age_minutes);
                let safety_icon = if t.safety.is_safe() { "✅" } else { "⚠️" };
                let change_color = if t.price_change_since_launch >= 0.0 { Color::Green } else { Color::Red };

                Row::new(vec![
                    Cell::from(age_str),
                    Cell::from(t.symbol.clone()).style(Style::default().fg(Color::Cyan)),
                    Cell::from(format!("{:.2}", t.market_cap_sol)),
                    Cell::from(format!("{:.1}", t.current_liquidity_sol)),
                    Cell::from(format!("{}", t.holders)),
                    Cell::from(format!("{:+.0}%", t.price_change_since_launch)).style(Style::default().fg(change_color)),
                    Cell::from(format!("{:.0}%", t.dev_holdings_pct)),
                    Cell::from(safety_icon),
                ])
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(5),
                Constraint::Percentage(15),
                Constraint::Percentage(12),
                Constraint::Percentage(12),
                Constraint::Percentage(10),
                Constraint::Percentage(12),
                Constraint::Percentage(10),
                Constraint::Length(3),
            ],
        )
        .header(Row::new(vec!["Age", "Token", "MCap", "Liq", "Hold", "Chg%", "Dev%", ""])
            .style(Style::default().fg(Color::Yellow)))
        .block(Block::default()
            .title(" 🆕 New Token Launches (pump.fun) ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, area);
    }

    fn render_trending_tokens(&self, f: &mut Frame, area: Rect) {
        if self.trenches.trending.is_empty() {
            let text = Paragraph::new("Analyzing trending tokens...\n\n💡 Tokens ranked by volume, momentum, and smart money activity")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default()
                    .title(" 📈 Trending Tokens ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray)));
            f.render_widget(text, area);
            return;
        }

        let rows: Vec<Row> = self.trenches.trending
            .iter()
            .enumerate()
            .take(20)
            .map(|(i, t)| {
                let rank = format!("#{}", i + 1);
                let change_5m_color = if t.price_change_5m >= 0.0 { Color::Green } else { Color::Red };
                let change_1h_color = if t.price_change_1h >= 0.0 { Color::Green } else { Color::Red };

                Row::new(vec![
                    Cell::from(rank).style(Style::default().fg(Color::Yellow)),
                    Cell::from(t.symbol.clone()).style(Style::default().fg(Color::Cyan)),
                    Cell::from(format!("{:.2}", t.market_cap_sol)),
                    Cell::from(format!("{:.1}", t.volume_1h_sol)),
                    Cell::from(format!("{:+.1}%", t.price_change_5m)).style(Style::default().fg(change_5m_color)),
                    Cell::from(format!("{:+.1}%", t.price_change_1h)).style(Style::default().fg(change_1h_color)),
                    Cell::from(format!("{:.1}", t.buy_pressure)),
                    Cell::from(format!("{}", t.smart_money_buys)).style(Style::default().fg(Color::Magenta)),
                ])
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(4),
                Constraint::Percentage(15),
                Constraint::Percentage(12),
                Constraint::Percentage(12),
                Constraint::Percentage(12),
                Constraint::Percentage(12),
                Constraint::Percentage(10),
                Constraint::Percentage(8),
            ],
        )
        .header(Row::new(vec!["#", "Token", "MCap", "Vol1h", "5m", "1h", "Buy%", "🐋"])
            .style(Style::default().fg(Color::Yellow)))
        .block(Block::default()
            .title(" 📈 Trending Tokens ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, area);
    }

    fn render_gainers(&self, f: &mut Frame, area: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(area);

        // 5-minute gainers
        self.render_gainer_list(f, chunks[0], "🚀 5m Gainers", &self.trenches.top_gainers_5m);

        // 1-hour gainers
        self.render_gainer_list(f, chunks[1], "🚀 1h Gainers", &self.trenches.top_gainers_1h);
    }

    fn render_losers(&self, f: &mut Frame, area: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(area);

        // 5-minute losers
        self.render_gainer_list(f, chunks[0], "📉 5m Losers", &self.trenches.top_losers_5m);

        // 1-hour losers
        self.render_gainer_list(f, chunks[1], "📉 1h Losers", &self.trenches.top_losers_1h);
    }

    fn render_gainer_list(&self, f: &mut Frame, area: Rect, title: &str, tokens: &[GainerLoser]) {
        if tokens.is_empty() {
            let text = Paragraph::new("Loading...")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default()
                    .title(format!(" {} ", title))
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray)));
            f.render_widget(text, area);
            return;
        }

        let rows: Vec<Row> = tokens
            .iter()
            .enumerate()
            .take(10)
            .map(|(i, t)| {
                let change_color = if t.price_change_pct >= 0.0 { Color::Green } else { Color::Red };

                Row::new(vec![
                    Cell::from(format!("{}", i + 1)),
                    Cell::from(t.symbol.clone()).style(Style::default().fg(Color::Cyan)),
                    Cell::from(format!("{:.2}", t.market_cap_sol)),
                    Cell::from(format!("{:+.1}%", t.price_change_pct)).style(Style::default().fg(change_color).add_modifier(Modifier::BOLD)),
                ])
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(3),
                Constraint::Percentage(40),
                Constraint::Percentage(25),
                Constraint::Percentage(25),
            ],
        )
        .header(Row::new(vec!["#", "Token", "MCap", "Change"])
            .style(Style::default().fg(Color::Yellow)))
        .block(Block::default()
            .title(format!(" {} ", title))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, area);
    }

    // ========================================================================
    // Smart Money Tab - Whale Tracking & Copy Trading
    // ========================================================================

    fn render_smart_money(&self, f: &mut Frame, area: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Percentage(40),  // Tracked wallets
                Constraint::Percentage(35),  // Recent activity
                Constraint::Percentage(25),  // Signals
            ])
            .split(area);

        self.render_tracked_wallets(f, chunks[0]);
        self.render_wallet_activity(f, chunks[1]);
        self.render_smart_signals(f, chunks[2]);
    }

    fn render_tracked_wallets(&self, f: &mut Frame, area: Rect) {
        if self.smart_money.tracked_wallets.is_empty() {
            let text = Paragraph::new("No wallets tracked yet\n\n💡 Add whale wallets to track their activity and copy trade")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default()
                    .title(" 🐋 Tracked Wallets ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray)));
            f.render_widget(text, area);
            return;
        }

        let rows: Vec<Row> = self.smart_money.tracked_wallets
            .iter()
            .map(|w| {
                let type_icon = match w.wallet_type {
                    WalletType::Whale => "🐋",
                    WalletType::SmartMoney => "🧠",
                    WalletType::Kol => "🎤",
                    WalletType::Insider => "👀",
                    WalletType::Sniper => "🎯",
                    WalletType::Custom => "📌",
                };
                let pnl_color = if w.total_pnl_sol >= 0.0 { Color::Green } else { Color::Red };
                let copy_status = if w.is_copying { "✅" } else { "—" };

                Row::new(vec![
                    Cell::from(type_icon),
                    Cell::from(w.label.clone().unwrap_or_else(|| format!("{}...", &w.address[..8]))),
                    Cell::from(format!("{:+.2}", w.total_pnl_sol)).style(Style::default().fg(pnl_color)),
                    Cell::from(format!("{:.0}%", w.win_rate * 100.0)),
                    Cell::from(format!("{}", w.total_trades)),
                    Cell::from(copy_status),
                ])
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(3),
                Constraint::Percentage(30),
                Constraint::Percentage(20),
                Constraint::Percentage(15),
                Constraint::Percentage(15),
                Constraint::Length(4),
            ],
        )
        .header(Row::new(vec!["", "Wallet", "PnL", "Win%", "Trades", "Copy"])
            .style(Style::default().fg(Color::Yellow)))
        .block(Block::default()
            .title(" 🐋 Tracked Wallets ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, area);
    }

    fn render_wallet_activity(&self, f: &mut Frame, area: Rect) {
        if self.smart_money.recent_activity.is_empty() {
            let text = Paragraph::new("No recent activity")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default()
                    .title(" 📡 Recent Whale Activity ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray)));
            f.render_widget(text, area);
            return;
        }

        let rows: Vec<Row> = self.smart_money.recent_activity
            .iter()
            .rev()
            .take(10)
            .map(|a| {
                let action_icon = match a.action {
                    WalletAction::Buy => ("🟢", Color::Green),
                    WalletAction::Sell => ("🔴", Color::Red),
                    WalletAction::AddLiquidity => ("💧", Color::Blue),
                    WalletAction::RemoveLiquidity => ("🔥", Color::Yellow),
                    WalletAction::Transfer => ("➡️", Color::Gray),
                };

                Row::new(vec![
                    Cell::from(a.timestamp.format("%H:%M:%S").to_string()),
                    Cell::from(action_icon.0).style(Style::default().fg(action_icon.1)),
                    Cell::from(a.wallet_label.clone().unwrap_or_else(|| format!("{}...", &a.wallet[..6]))),
                    Cell::from(a.symbol.clone()).style(Style::default().fg(Color::Cyan)),
                    Cell::from(format!("{:.3}", a.amount_sol)),
                ])
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(10),
                Constraint::Length(3),
                Constraint::Percentage(25),
                Constraint::Percentage(25),
                Constraint::Percentage(20),
            ],
        )
        .header(Row::new(vec!["Time", "", "Wallet", "Token", "SOL"])
            .style(Style::default().fg(Color::Yellow)))
        .block(Block::default()
            .title(" 📡 Recent Whale Activity ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, area);
    }

    fn render_smart_signals(&self, f: &mut Frame, area: Rect) {
        if self.smart_money.signals.is_empty() {
            let text = Paragraph::new("Analyzing smart money flow...")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default()
                    .title(" 🧠 Smart Money Signals ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray)));
            f.render_widget(text, area);
            return;
        }

        let items: Vec<ListItem> = self.smart_money.signals
            .iter()
            .take(5)
            .map(|s| {
                let (icon, color) = match s.signal_type {
                    SmartMoneySignalType::Accumulation => ("🟢 ACCUMULATION", Color::Green),
                    SmartMoneySignalType::Distribution => ("🔴 DISTRIBUTION", Color::Red),
                    SmartMoneySignalType::Neutral => ("🟡 NEUTRAL", Color::Yellow),
                };

                ListItem::new(Line::from(vec![
                    Span::styled(icon, Style::default().fg(color)),
                    Span::raw(" "),
                    Span::styled(&s.symbol, Style::default().fg(Color::Cyan)),
                    Span::raw(format!(" ({} whales, {:+.2} SOL flow)", s.unique_whales, s.net_flow_sol)),
                ]))
            })
            .collect();

        let list = List::new(items)
            .block(Block::default()
                .title(" 🧠 Smart Money Signals ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(list, area);
    }

    fn render_status_bar(&self, f: &mut Frame, area: Rect) {
        let help_hint = " [?] Help  [S] Start  [P] Pause  [X] Stop  [Q] Quit  [1-6] Tabs  [←→] Sub-tabs ";

        let status = self.status_message.as_deref().unwrap_or("");

        let text = Line::from(vec![
            Span::styled(help_hint, Style::default().fg(Color::DarkGray)),
            Span::raw("  "),
            Span::styled(status, Style::default().fg(Color::Yellow)),
        ]);

        let paragraph = Paragraph::new(text)
            .block(Block::default().borders(Borders::TOP).border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(paragraph, area);
    }

    fn render_help_overlay(&self, f: &mut Frame, area: Rect) {
        let help_text = vec![
            Line::from(""),
            Line::from(vec![Span::styled("  KEYBOARD SHORTCUTS", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD))]),
            Line::from(""),
            Line::from("  [S]       Start trading agent"),
            Line::from("  [P]       Pause/Resume trading"),
            Line::from("  [X]       Stop trading agent"),
            Line::from("  [Q/Esc]   Quit application"),
            Line::from(""),
            Line::from(vec![Span::styled("  NAVIGATION", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD))]),
            Line::from(""),
            Line::from("  [1-6]     Switch tabs: Dashboard/Trenches/Smart/Pos/Trades/Config"),
            Line::from("  [Tab]     Next tab"),
            Line::from("  [←→]      Sub-tabs (in Trenches) or prev/next tab"),
            Line::from("  [↑↓/jk]   Scroll within tab"),
            Line::from(""),
            Line::from("  [?]       Toggle this help"),
            Line::from(""),
            Line::from(vec![Span::styled("  Press any key to close", Style::default().fg(Color::DarkGray))]),
        ];

        let block = Block::default()
            .title(" Help ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Cyan));

        let paragraph = Paragraph::new(help_text).block(block);

        let popup_area = centered_rect(50, 60, area);
        f.render_widget(ratatui::widgets::Clear, popup_area);
        f.render_widget(paragraph, popup_area);
    }

    fn render_confirm_dialog(&self, f: &mut Frame, area: Rect, title: &str, message: &str) {
        let text = vec![
            Line::from(""),
            Line::from(vec![Span::styled(message, Style::default().fg(Color::White))]),
            Line::from(""),
            Line::from(vec![
                Span::styled("[Y] Yes", Style::default().fg(Color::Green)),
                Span::raw("    "),
                Span::styled("[N] No", Style::default().fg(Color::Red)),
            ]),
            Line::from(""),
        ];

        let block = Block::default()
            .title(format!(" {} ", title))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Yellow));

        let paragraph = Paragraph::new(text)
            .block(block)
            .wrap(Wrap { trim: true });

        let popup_area = centered_rect(50, 30, area);
        f.render_widget(ratatui::widgets::Clear, popup_area);
        f.render_widget(paragraph, popup_area);
    }
}
