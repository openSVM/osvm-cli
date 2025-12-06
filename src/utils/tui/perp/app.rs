//! Perpetual Futures Trading TUI Application
//!
//! Main state struct for trading perpetual futures on Solana DEXs.

use super::super::common::{centered_rect, format_price, format_usd, load_wallet_address};

#[allow(unused_imports)]
use solana_sdk::signer::Signer;

use anyhow::Result;
use chrono::{DateTime, Utc};
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    symbols,
    text::{Line, Span},
    widgets::{
        Block, Borders, Cell, Clear, Gauge, List, ListItem, Paragraph, Row, Sparkline, Table,
        Tabs, Wrap,
    },
    Frame, Terminal,
};
use std::collections::VecDeque;
use std::io;
use std::time::{Duration, Instant};

// ============================================================================
// Data Types
// ============================================================================

/// Supported perpetual protocols
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PerpProtocol {
    Drift,
    Zeta,
    MangoV4,
    Jupiter,
}

impl PerpProtocol {
    pub fn name(&self) -> &str {
        match self {
            PerpProtocol::Drift => "Drift",
            PerpProtocol::Zeta => "Zeta",
            PerpProtocol::MangoV4 => "Mango V4",
            PerpProtocol::Jupiter => "Jupiter",
        }
    }

    pub fn color(&self) -> Color {
        match self {
            PerpProtocol::Drift => Color::Magenta,
            PerpProtocol::Zeta => Color::Cyan,
            PerpProtocol::MangoV4 => Color::Yellow,
            PerpProtocol::Jupiter => Color::Green,
        }
    }
}

/// Position side
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PositionSide {
    Long,
    Short,
}

impl PositionSide {
    pub fn name(&self) -> &str {
        match self {
            PositionSide::Long => "LONG",
            PositionSide::Short => "SHORT",
        }
    }

    pub fn color(&self) -> Color {
        match self {
            PositionSide::Long => Color::Green,
            PositionSide::Short => Color::Red,
        }
    }
}

/// Order type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OrderType {
    Market,
    Limit,
    StopLoss,
    TakeProfit,
}

impl OrderType {
    pub fn name(&self) -> &str {
        match self {
            OrderType::Market => "Market",
            OrderType::Limit => "Limit",
            OrderType::StopLoss => "Stop Loss",
            OrderType::TakeProfit => "Take Profit",
        }
    }
}

/// A perpetual market
#[derive(Debug, Clone)]
pub struct PerpMarket {
    pub symbol: String,
    pub base_asset: String,
    pub protocol: PerpProtocol,
    pub mark_price: f64,
    pub index_price: f64,
    pub funding_rate: f64,      // Current funding rate (hourly %)
    pub funding_rate_24h: f64,  // 24h average funding rate
    pub open_interest: f64,     // In USD
    pub volume_24h: f64,
    pub price_change_24h: f64,  // Percentage
    pub high_24h: f64,
    pub low_24h: f64,
    pub max_leverage: u32,
}

/// A user's perpetual position
#[derive(Debug, Clone)]
pub struct PerpPosition {
    pub id: String,
    pub market: PerpMarket,
    pub side: PositionSide,
    pub size: f64,              // Base asset amount
    pub entry_price: f64,
    pub mark_price: f64,
    pub leverage: f64,
    pub margin: f64,            // Collateral in USD
    pub unrealized_pnl: f64,
    pub realized_pnl: f64,
    pub liquidation_price: f64,
    pub opened_at: DateTime<Utc>,
}

/// An open order
#[derive(Debug, Clone)]
pub struct PerpOrder {
    pub id: String,
    pub market_symbol: String,
    pub side: PositionSide,
    pub order_type: OrderType,
    pub size: f64,
    pub price: Option<f64>,     // None for market orders
    pub trigger_price: Option<f64>,
    pub filled: f64,
    pub created_at: DateTime<Utc>,
}

/// Trade history entry
#[derive(Debug, Clone)]
pub struct TradeHistory {
    pub id: String,
    pub market_symbol: String,
    pub side: PositionSide,
    pub size: f64,
    pub price: f64,
    pub fee: f64,
    pub pnl: f64,
    pub executed_at: DateTime<Utc>,
}

// ============================================================================
// Application State
// ============================================================================

/// Main Perp TUI application
pub struct PerpApp {
    // Wallet
    pub wallet_address: Option<String>,
    pub sol_balance: f64,
    pub usdc_balance: f64,

    // Data
    pub markets: Vec<PerpMarket>,
    pub positions: Vec<PerpPosition>,
    pub orders: Vec<PerpOrder>,
    pub trades: Vec<TradeHistory>,
    pub price_history: VecDeque<f64>,  // Recent prices for sparkline

    // UI State
    pub active_tab: usize,  // 0=Markets, 1=Positions, 2=Orders, 3=History
    pub selected_market: usize,
    pub selected_position: usize,
    pub selected_order: usize,
    pub scroll_offset: usize,
    pub should_quit: bool,

    // Trading panel
    pub show_trade_panel: bool,
    pub trade_side: PositionSide,
    pub trade_leverage: f64,
    pub trade_size: String,
    pub trade_price: String,
    pub trade_order_type: OrderType,
    pub trade_focus: usize,  // 0=size, 1=price, 2=leverage

    // Filters
    pub filter_protocol: Option<PerpProtocol>,
    pub show_only_with_positions: bool,

    // Status
    pub status_message: Option<String>,
    pub status_time: Option<Instant>,
    pub last_refresh: Instant,

    // Stats
    pub total_unrealized_pnl: f64,
    pub total_margin_used: f64,
    pub total_position_value: f64,
}

impl PerpApp {
    /// Create a new Perp app
    pub fn new() -> Self {
        Self {
            wallet_address: None,
            sol_balance: 0.0,
            usdc_balance: 0.0,
            markets: Vec::new(),
            positions: Vec::new(),
            orders: Vec::new(),
            trades: Vec::new(),
            price_history: VecDeque::with_capacity(100),
            active_tab: 0,
            selected_market: 0,
            selected_position: 0,
            selected_order: 0,
            scroll_offset: 0,
            should_quit: false,
            show_trade_panel: false,
            trade_side: PositionSide::Long,
            trade_leverage: 5.0,
            trade_size: String::new(),
            trade_price: String::new(),
            trade_order_type: OrderType::Market,
            trade_focus: 0,
            filter_protocol: None,
            show_only_with_positions: false,
            status_message: None,
            status_time: None,
            last_refresh: Instant::now(),
            total_unrealized_pnl: 0.0,
            total_margin_used: 0.0,
            total_position_value: 0.0,
        }
    }

    /// Initialize with mock data
    pub async fn initialize(&mut self) -> Result<()> {
        // Load wallet if available (using shared utility)
        self.wallet_address = load_wallet_address();
        self.usdc_balance = 5000.0; // Mock balance

        // Load mock markets
        self.markets = self.fetch_mock_markets();

        // Load mock positions
        self.positions = self.fetch_mock_positions();

        // Load mock orders
        self.orders = self.fetch_mock_orders();

        // Initialize price history
        for i in 0..50 {
            let base = 95000.0;
            let variation = (i as f64 * 0.1).sin() * 500.0 + (i as f64 * 0.3).cos() * 300.0;
            self.price_history.push_back(base + variation);
        }

        self.calculate_totals();
        self.set_status("Connected to perpetual markets");
        Ok(())
    }

    fn fetch_mock_markets(&self) -> Vec<PerpMarket> {
        vec![
            PerpMarket {
                symbol: "SOL-PERP".to_string(),
                base_asset: "SOL".to_string(),
                protocol: PerpProtocol::Drift,
                mark_price: 180.25,
                index_price: 180.20,
                funding_rate: 0.01,
                funding_rate_24h: 0.008,
                open_interest: 45_000_000.0,
                volume_24h: 125_000_000.0,
                price_change_24h: 3.5,
                high_24h: 185.00,
                low_24h: 175.50,
                max_leverage: 20,
            },
            PerpMarket {
                symbol: "BTC-PERP".to_string(),
                base_asset: "BTC".to_string(),
                protocol: PerpProtocol::Drift,
                mark_price: 95_250.0,
                index_price: 95_200.0,
                funding_rate: 0.005,
                funding_rate_24h: 0.004,
                open_interest: 250_000_000.0,
                volume_24h: 500_000_000.0,
                price_change_24h: 1.2,
                high_24h: 96_000.0,
                low_24h: 93_500.0,
                max_leverage: 50,
            },
            PerpMarket {
                symbol: "ETH-PERP".to_string(),
                base_asset: "ETH".to_string(),
                protocol: PerpProtocol::Zeta,
                mark_price: 3_450.0,
                index_price: 3_448.0,
                funding_rate: -0.002,
                funding_rate_24h: -0.001,
                open_interest: 85_000_000.0,
                volume_24h: 180_000_000.0,
                price_change_24h: -0.8,
                high_24h: 3_520.0,
                low_24h: 3_400.0,
                max_leverage: 30,
            },
            PerpMarket {
                symbol: "JUP-PERP".to_string(),
                base_asset: "JUP".to_string(),
                protocol: PerpProtocol::Jupiter,
                mark_price: 1.25,
                index_price: 1.24,
                funding_rate: 0.02,
                funding_rate_24h: 0.018,
                open_interest: 12_000_000.0,
                volume_24h: 35_000_000.0,
                price_change_24h: 5.2,
                high_24h: 1.30,
                low_24h: 1.18,
                max_leverage: 10,
            },
            PerpMarket {
                symbol: "BONK-PERP".to_string(),
                base_asset: "BONK".to_string(),
                protocol: PerpProtocol::MangoV4,
                mark_price: 0.0000325,
                index_price: 0.0000324,
                funding_rate: 0.05,
                funding_rate_24h: 0.04,
                open_interest: 8_000_000.0,
                volume_24h: 25_000_000.0,
                price_change_24h: 12.5,
                high_24h: 0.000035,
                low_24h: 0.000028,
                max_leverage: 5,
            },
        ]
    }

    fn fetch_mock_positions(&self) -> Vec<PerpPosition> {
        if self.markets.is_empty() {
            return Vec::new();
        }

        vec![
            PerpPosition {
                id: "pos_001".to_string(),
                market: self.markets[0].clone(),
                side: PositionSide::Long,
                size: 10.0,
                entry_price: 175.00,
                mark_price: 180.25,
                leverage: 5.0,
                margin: 350.0,
                unrealized_pnl: 52.50,
                realized_pnl: 0.0,
                liquidation_price: 140.00,
                opened_at: Utc::now() - chrono::Duration::hours(6),
            },
            PerpPosition {
                id: "pos_002".to_string(),
                market: self.markets[2].clone(),
                side: PositionSide::Short,
                size: 1.0,
                entry_price: 3_500.0,
                mark_price: 3_450.0,
                leverage: 10.0,
                margin: 350.0,
                unrealized_pnl: 50.0,
                realized_pnl: 25.0,
                liquidation_price: 3_850.0,
                opened_at: Utc::now() - chrono::Duration::days(1),
            },
        ]
    }

    fn fetch_mock_orders(&self) -> Vec<PerpOrder> {
        vec![
            PerpOrder {
                id: "ord_001".to_string(),
                market_symbol: "SOL-PERP".to_string(),
                side: PositionSide::Long,
                order_type: OrderType::Limit,
                size: 5.0,
                price: Some(170.00),
                trigger_price: None,
                filled: 0.0,
                created_at: Utc::now() - chrono::Duration::minutes(30),
            },
            PerpOrder {
                id: "ord_002".to_string(),
                market_symbol: "BTC-PERP".to_string(),
                side: PositionSide::Short,
                order_type: OrderType::StopLoss,
                size: 0.1,
                price: None,
                trigger_price: Some(90_000.0),
                filled: 0.0,
                created_at: Utc::now() - chrono::Duration::hours(2),
            },
        ]
    }

    fn calculate_totals(&mut self) {
        self.total_unrealized_pnl = self.positions.iter().map(|p| p.unrealized_pnl).sum();
        self.total_margin_used = self.positions.iter().map(|p| p.margin).sum();
        self.total_position_value = self.positions
            .iter()
            .map(|p| p.size * p.mark_price)
            .sum();
    }

    fn set_status(&mut self, msg: &str) {
        self.status_message = Some(msg.to_string());
        self.status_time = Some(Instant::now());
    }

    /// Run the main event loop
    pub async fn run(&mut self, terminal: &mut Terminal<CrosstermBackend<io::Stdout>>) -> Result<()> {
        loop {
            // Clear old status
            if let Some(time) = self.status_time {
                if time.elapsed() > Duration::from_secs(5) {
                    self.status_message = None;
                    self.status_time = None;
                }
            }

            // Simulate price updates
            if self.last_refresh.elapsed() > Duration::from_millis(500) {
                self.update_prices();
                self.last_refresh = Instant::now();
            }

            // Render
            terminal.draw(|f| self.render(f))?;

            // Handle events
            if crossterm::event::poll(Duration::from_millis(100))? {
                if let Event::Key(key) = crossterm::event::read()? {
                    self.handle_key(key).await?;
                }
            }

            if self.should_quit {
                break;
            }
        }

        Ok(())
    }

    fn update_prices(&mut self) {
        // Simulate price movement using updated rand API
        use rand::Rng;
        let mut rng = rand::rng();

        for market in &mut self.markets {
            let change = rng.random_range(-0.5..0.5);
            market.mark_price *= 1.0 + (change / 100.0);
            market.index_price = market.mark_price * rng.random_range(0.999..1.001);
        }

        // Update price history
        if !self.markets.is_empty() {
            self.price_history.push_back(self.markets[0].mark_price);
            if self.price_history.len() > 100 {
                self.price_history.pop_front();
            }
        }

        // Update position PnLs
        for pos in &mut self.positions {
            if let Some(market) = self.markets.iter().find(|m| m.symbol == pos.market.symbol) {
                pos.mark_price = market.mark_price;
                let price_diff = match pos.side {
                    PositionSide::Long => pos.mark_price - pos.entry_price,
                    PositionSide::Short => pos.entry_price - pos.mark_price,
                };
                pos.unrealized_pnl = price_diff * pos.size;
            }
        }

        self.calculate_totals();
    }

    /// Handle key input
    async fn handle_key(&mut self, key: KeyEvent) -> Result<()> {
        // Trade panel
        if self.show_trade_panel {
            return self.handle_trade_panel_key(key);
        }

        // Main controls
        match key.code {
            KeyCode::Char('q') | KeyCode::Esc => {
                self.should_quit = true;
            }
            KeyCode::Char('?') => {
                self.set_status("Help: Tab=switch, j/k=navigate, t=trade, c=close, x=cancel");
            }
            // Tab navigation
            KeyCode::Tab => {
                self.active_tab = (self.active_tab + 1) % 4;
            }
            KeyCode::BackTab => {
                self.active_tab = if self.active_tab == 0 { 3 } else { self.active_tab - 1 };
            }
            KeyCode::Char('1') => self.active_tab = 0,
            KeyCode::Char('2') => self.active_tab = 1,
            KeyCode::Char('3') => self.active_tab = 2,
            KeyCode::Char('4') => self.active_tab = 3,
            // Selection
            KeyCode::Up | KeyCode::Char('k') => {
                match self.active_tab {
                    0 => {
                        if self.selected_market > 0 {
                            self.selected_market -= 1;
                        }
                    }
                    1 => {
                        if self.selected_position > 0 {
                            self.selected_position -= 1;
                        }
                    }
                    2 => {
                        if self.selected_order > 0 {
                            self.selected_order -= 1;
                        }
                    }
                    _ => {}
                }
            }
            KeyCode::Down | KeyCode::Char('j') => {
                match self.active_tab {
                    0 => {
                        if self.selected_market < self.markets.len().saturating_sub(1) {
                            self.selected_market += 1;
                        }
                    }
                    1 => {
                        if self.selected_position < self.positions.len().saturating_sub(1) {
                            self.selected_position += 1;
                        }
                    }
                    2 => {
                        if self.selected_order < self.orders.len().saturating_sub(1) {
                            self.selected_order += 1;
                        }
                    }
                    _ => {}
                }
            }
            // Actions
            KeyCode::Enter | KeyCode::Char('t') => {
                if self.active_tab == 0 && !self.markets.is_empty() {
                    self.show_trade_panel = true;
                    self.trade_size.clear();
                    self.trade_price.clear();
                    self.trade_focus = 0;
                }
            }
            KeyCode::Char('c') => {
                if self.active_tab == 1 && !self.positions.is_empty() {
                    self.set_status("Closing position... (simulated)");
                }
            }
            KeyCode::Char('x') => {
                if self.active_tab == 2 && !self.orders.is_empty() {
                    self.set_status("Cancelling order... (simulated)");
                }
            }
            // Toggle side (Long/Short)
            KeyCode::Char('l') => {
                self.trade_side = PositionSide::Long;
                self.set_status("Trade side: LONG");
            }
            KeyCode::Char('s') => {
                self.trade_side = PositionSide::Short;
                self.set_status("Trade side: SHORT");
            }
            // Protocol filters
            KeyCode::Char('D') => {
                self.filter_protocol = if self.filter_protocol == Some(PerpProtocol::Drift) {
                    None
                } else {
                    Some(PerpProtocol::Drift)
                };
            }
            KeyCode::Char('Z') => {
                self.filter_protocol = if self.filter_protocol == Some(PerpProtocol::Zeta) {
                    None
                } else {
                    Some(PerpProtocol::Zeta)
                };
            }
            _ => {}
        }

        Ok(())
    }

    fn handle_trade_panel_key(&mut self, key: KeyEvent) -> Result<()> {
        match key.code {
            KeyCode::Esc => {
                self.show_trade_panel = false;
            }
            KeyCode::Tab => {
                self.trade_focus = (self.trade_focus + 1) % 3;
            }
            KeyCode::Enter => {
                self.show_trade_panel = false;
                let side = self.trade_side.name();
                self.set_status(&format!("Placing {} order... (simulated)", side));
            }
            KeyCode::Char(c) if c.is_ascii_digit() || c == '.' => {
                match self.trade_focus {
                    0 => self.trade_size.push(c),
                    1 => self.trade_price.push(c),
                    2 => {
                        if let Some(digit) = c.to_digit(10) {
                            let new_lev = self.trade_leverage * 10.0 + digit as f64;
                            if let Some(market) = self.markets.get(self.selected_market) {
                                if new_lev <= market.max_leverage as f64 {
                                    self.trade_leverage = new_lev;
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
            KeyCode::Backspace => {
                match self.trade_focus {
                    0 => { self.trade_size.pop(); }
                    1 => { self.trade_price.pop(); }
                    2 => { self.trade_leverage = (self.trade_leverage / 10.0).floor(); }
                    _ => {}
                }
            }
            KeyCode::Left | KeyCode::Right => {
                // Toggle Long/Short
                self.trade_side = match self.trade_side {
                    PositionSide::Long => PositionSide::Short,
                    PositionSide::Short => PositionSide::Long,
                };
            }
            KeyCode::Char('m') => self.trade_order_type = OrderType::Market,
            KeyCode::Char('L') => self.trade_order_type = OrderType::Limit,
            _ => {}
        }
        Ok(())
    }

    // ========================================================================
    // Rendering
    // ========================================================================

    fn render(&self, f: &mut Frame) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),  // Header
                Constraint::Length(3),  // Tabs
                Constraint::Min(10),    // Content
                Constraint::Length(1),  // Status
            ])
            .split(f.area());

        self.render_header(f, chunks[0]);
        self.render_tabs(f, chunks[1]);
        self.render_content(f, chunks[2]);
        self.render_status(f, chunks[3]);

        // Trade panel overlay
        if self.show_trade_panel {
            self.render_trade_panel(f);
        }
    }

    fn render_header(&self, f: &mut Frame, area: Rect) {
        let wallet_info = self.wallet_address
            .as_ref()
            .map(|w| format!("{}...{}", &w[..4], &w[40..]))
            .unwrap_or_else(|| "No wallet".to_string());

        let pnl_color = if self.total_unrealized_pnl >= 0.0 { Color::Green } else { Color::Red };
        let pnl_str = if self.total_unrealized_pnl >= 0.0 {
            format!("+${:.2}", self.total_unrealized_pnl)
        } else {
            format!("-${:.2}", self.total_unrealized_pnl.abs())
        };

        let header = Paragraph::new(Line::from(vec![
            Span::styled(" 📈 OSVM Perp ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::raw("│ "),
            Span::styled(&wallet_info, Style::default().fg(Color::Green)),
            Span::raw(" │ "),
            Span::styled(format!("USDC: ${:.2}", self.usdc_balance), Style::default().fg(Color::White)),
            Span::raw(" │ "),
            Span::styled("Unrealized: ", Style::default().fg(Color::Gray)),
            Span::styled(pnl_str, Style::default().fg(pnl_color)),
            Span::raw(" │ "),
            Span::styled(format!("Margin: ${:.2}", self.total_margin_used), Style::default().fg(Color::Yellow)),
        ]))
        .block(Block::default().borders(Borders::BOTTOM).border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(header, area);
    }

    fn render_tabs(&self, f: &mut Frame, area: Rect) {
        let titles = vec![
            format!("📊 Markets ({})", self.markets.len()),
            format!("💼 Positions ({})", self.positions.len()),
            format!("📋 Orders ({})", self.orders.len()),
            "📜 History".to_string(),
        ];

        let tabs = Tabs::new(titles)
            .select(self.active_tab)
            .style(Style::default().fg(Color::White))
            .highlight_style(Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
            .divider(" │ ");

        f.render_widget(tabs, area);
    }

    fn render_content(&self, f: &mut Frame, area: Rect) {
        match self.active_tab {
            0 => self.render_markets(f, area),
            1 => self.render_positions(f, area),
            2 => self.render_orders(f, area),
            3 => self.render_history(f, area),
            _ => {}
        }
    }

    fn render_markets(&self, f: &mut Frame, area: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(5),  // Price chart
                Constraint::Min(5),     // Market list
            ])
            .split(area);

        // Sparkline for selected market
        if !self.price_history.is_empty() {
            let prices: Vec<u64> = self.price_history
                .iter()
                .map(|&p| (p * 10.0) as u64)
                .collect();

            let min_price = prices.iter().min().copied().unwrap_or(0);
            let max_price = prices.iter().max().copied().unwrap_or(1);

            let market_name = self.markets.get(self.selected_market)
                .map(|m| m.symbol.as_str())
                .unwrap_or("SOL-PERP");

            let sparkline = Sparkline::default()
                .block(Block::default()
                    .title(format!(" {} Price ", market_name))
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray)))
                .data(&prices)
                .style(Style::default().fg(Color::Cyan))
                .max(max_price);

            f.render_widget(sparkline, chunks[0]);
        }

        // Market table
        let rows: Vec<Row> = self.markets
            .iter()
            .enumerate()
            .map(|(i, market)| {
                let is_selected = i == self.selected_market;
                let style = if is_selected {
                    Style::default().bg(Color::DarkGray)
                } else {
                    Style::default()
                };

                let change_color = if market.price_change_24h >= 0.0 { Color::Green } else { Color::Red };
                let funding_color = if market.funding_rate >= 0.0 { Color::Green } else { Color::Red };

                Row::new(vec![
                    Cell::from(market.protocol.name()).style(Style::default().fg(market.protocol.color())),
                    Cell::from(market.symbol.as_str()).style(Style::default().add_modifier(Modifier::BOLD)),
                    Cell::from(format_price(market.mark_price)),
                    Cell::from(format!("{:+.2}%", market.price_change_24h)).style(Style::default().fg(change_color)),
                    Cell::from(format!("{:.4}%", market.funding_rate)).style(Style::default().fg(funding_color)),
                    Cell::from(format_usd(market.open_interest)),
                    Cell::from(format_usd(market.volume_24h)),
                    Cell::from(format!("{}x", market.max_leverage)),
                ])
                .style(style)
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(10),
                Constraint::Length(12),
                Constraint::Length(14),
                Constraint::Length(10),
                Constraint::Length(10),
                Constraint::Length(12),
                Constraint::Length(12),
                Constraint::Length(6),
            ],
        )
        .header(
            Row::new(vec!["Protocol", "Market", "Price", "24h %", "Funding", "OI", "Vol 24h", "Lev"])
                .style(Style::default().fg(Color::Yellow))
        )
        .block(Block::default()
            .title(" Markets │ [t] Trade │ [l] Long │ [s] Short ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, chunks[1]);
    }

    fn render_positions(&self, f: &mut Frame, area: Rect) {
        if self.positions.is_empty() {
            let empty = Paragraph::new("No open positions\n\nGo to Markets tab and press [t] to trade")
                .style(Style::default().fg(Color::DarkGray))
                .alignment(Alignment::Center)
                .block(Block::default().title(" Your Positions ").borders(Borders::ALL));
            f.render_widget(empty, area);
            return;
        }

        let rows: Vec<Row> = self.positions
            .iter()
            .enumerate()
            .map(|(i, pos)| {
                let is_selected = i == self.selected_position;
                let style = if is_selected {
                    Style::default().bg(Color::DarkGray)
                } else {
                    Style::default()
                };

                let pnl_color = if pos.unrealized_pnl >= 0.0 { Color::Green } else { Color::Red };
                let pnl_str = if pos.unrealized_pnl >= 0.0 {
                    format!("+${:.2}", pos.unrealized_pnl)
                } else {
                    format!("-${:.2}", pos.unrealized_pnl.abs())
                };

                Row::new(vec![
                    Cell::from(pos.market.symbol.as_str()),
                    Cell::from(pos.side.name()).style(Style::default().fg(pos.side.color())),
                    Cell::from(format!("{:.4}", pos.size)),
                    Cell::from(format_price(pos.entry_price)),
                    Cell::from(format_price(pos.mark_price)),
                    Cell::from(format!("{:.1}x", pos.leverage)),
                    Cell::from(pnl_str).style(Style::default().fg(pnl_color)),
                    Cell::from(format_price(pos.liquidation_price)).style(Style::default().fg(Color::Red)),
                ])
                .style(style)
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(12),
                Constraint::Length(8),
                Constraint::Length(10),
                Constraint::Length(12),
                Constraint::Length(12),
                Constraint::Length(8),
                Constraint::Length(12),
                Constraint::Length(12),
            ],
        )
        .header(
            Row::new(vec!["Market", "Side", "Size", "Entry", "Mark", "Lev", "uPnL", "Liq Price"])
                .style(Style::default().fg(Color::Yellow))
        )
        .block(Block::default()
            .title(" Positions │ [c] Close Position ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, area);
    }

    fn render_orders(&self, f: &mut Frame, area: Rect) {
        if self.orders.is_empty() {
            let empty = Paragraph::new("No open orders")
                .style(Style::default().fg(Color::DarkGray))
                .alignment(Alignment::Center)
                .block(Block::default().title(" Open Orders ").borders(Borders::ALL));
            f.render_widget(empty, area);
            return;
        }

        let rows: Vec<Row> = self.orders
            .iter()
            .enumerate()
            .map(|(i, order)| {
                let is_selected = i == self.selected_order;
                let style = if is_selected {
                    Style::default().bg(Color::DarkGray)
                } else {
                    Style::default()
                };

                let price_str = order.price
                    .map(|p| format_price(p))
                    .unwrap_or_else(|| "Market".to_string());

                let trigger_str = order.trigger_price
                    .map(|p| format_price(p))
                    .unwrap_or_else(|| "-".to_string());

                Row::new(vec![
                    Cell::from(order.market_symbol.as_str()),
                    Cell::from(order.side.name()).style(Style::default().fg(order.side.color())),
                    Cell::from(order.order_type.name()),
                    Cell::from(format!("{:.4}", order.size)),
                    Cell::from(price_str),
                    Cell::from(trigger_str),
                    Cell::from(format!("{:.4}/{:.4}", order.filled, order.size)),
                ])
                .style(style)
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(12),
                Constraint::Length(8),
                Constraint::Length(12),
                Constraint::Length(10),
                Constraint::Length(12),
                Constraint::Length(12),
                Constraint::Length(14),
            ],
        )
        .header(
            Row::new(vec!["Market", "Side", "Type", "Size", "Price", "Trigger", "Filled"])
                .style(Style::default().fg(Color::Yellow))
        )
        .block(Block::default()
            .title(" Open Orders │ [x] Cancel Order ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, area);
    }

    fn render_history(&self, f: &mut Frame, area: Rect) {
        let items: Vec<ListItem> = self.trades
            .iter()
            .map(|trade| {
                let pnl_color = if trade.pnl >= 0.0 { Color::Green } else { Color::Red };
                let pnl_str = if trade.pnl >= 0.0 {
                    format!("+${:.2}", trade.pnl)
                } else {
                    format!("-${:.2}", trade.pnl.abs())
                };

                ListItem::new(Line::from(vec![
                    Span::styled(&trade.market_symbol, Style::default().fg(Color::White)),
                    Span::raw(" "),
                    Span::styled(trade.side.name(), Style::default().fg(trade.side.color())),
                    Span::raw(format!(" {} @ {} ", trade.size, format_price(trade.price))),
                    Span::styled(pnl_str, Style::default().fg(pnl_color)),
                ]))
            })
            .collect();

        if items.is_empty() {
            let empty = Paragraph::new("No trade history")
                .style(Style::default().fg(Color::DarkGray))
                .alignment(Alignment::Center)
                .block(Block::default().title(" Trade History ").borders(Borders::ALL));
            f.render_widget(empty, area);
            return;
        }

        let list = List::new(items)
            .block(Block::default()
                .title(" Trade History ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(list, area);
    }

    fn render_status(&self, f: &mut Frame, area: Rect) {
        let status = self.status_message.as_deref().unwrap_or("");
        let help = " [?] Help  [Tab] Switch  [↑↓] Navigate  [t] Trade  [q] Quit ";

        let line = Line::from(vec![
            Span::styled(help, Style::default().fg(Color::DarkGray)),
            Span::raw("  "),
            Span::styled(status, Style::default().fg(Color::Yellow)),
        ]);

        f.render_widget(Paragraph::new(line), area);
    }

    fn render_trade_panel(&self, f: &mut Frame) {
        let market = match self.markets.get(self.selected_market) {
            Some(m) => m,
            None => return,
        };

        let side_style = Style::default().fg(self.trade_side.color()).add_modifier(Modifier::BOLD);

        let lines = vec![
            Line::from(""),
            Line::from(vec![
                Span::styled("  Market: ", Style::default().fg(Color::Gray)),
                Span::styled(&market.symbol, Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::raw(format!(" @ {}", format_price(market.mark_price))),
            ]),
            Line::from(vec![
                Span::styled("  Max Leverage: ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{}x", market.max_leverage), Style::default().fg(Color::Yellow)),
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled("  Side: ", Style::default().fg(Color::Gray)),
                Span::styled(self.trade_side.name(), side_style),
                Span::styled("  (←→ to toggle)", Style::default().fg(Color::DarkGray)),
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled("  Size: ", Style::default().fg(if self.trade_focus == 0 { Color::Cyan } else { Color::Gray })),
                Span::styled(if self.trade_size.is_empty() { "0.0" } else { &self.trade_size }, Style::default().fg(Color::White)),
                Span::raw(format!(" {}", market.base_asset)),
                if self.trade_focus == 0 { Span::styled("│", Style::default().fg(Color::Cyan)) } else { Span::raw("") },
            ]),
            Line::from(vec![
                Span::styled("  Price: ", Style::default().fg(if self.trade_focus == 1 { Color::Cyan } else { Color::Gray })),
                Span::styled(
                    if self.trade_order_type == OrderType::Market {
                        "Market"
                    } else if self.trade_price.is_empty() {
                        "0.0"
                    } else {
                        &self.trade_price
                    },
                    Style::default().fg(Color::White)
                ),
                if self.trade_focus == 1 && self.trade_order_type != OrderType::Market {
                    Span::styled("│", Style::default().fg(Color::Cyan))
                } else {
                    Span::raw("")
                },
            ]),
            Line::from(vec![
                Span::styled("  Leverage: ", Style::default().fg(if self.trade_focus == 2 { Color::Cyan } else { Color::Gray })),
                Span::styled(format!("{:.0}x", self.trade_leverage), Style::default().fg(Color::Yellow)),
                if self.trade_focus == 2 { Span::styled("│", Style::default().fg(Color::Cyan)) } else { Span::raw("") },
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled("  [m] Market  [L] Limit │ Order: ", Style::default().fg(Color::DarkGray)),
                Span::styled(self.trade_order_type.name(), Style::default().fg(Color::White)),
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled("  [Tab] ", Style::default().fg(Color::Yellow)),
                Span::raw("Switch field  "),
                Span::styled("[Enter] ", Style::default().fg(Color::Green)),
                Span::raw("Submit  "),
                Span::styled("[Esc] ", Style::default().fg(Color::Red)),
                Span::raw("Cancel"),
            ]),
        ];

        let popup = Paragraph::new(lines)
            .block(Block::default()
                .title(format!(" {} {} ", self.trade_side.name(), market.symbol))
                .borders(Borders::ALL)
                .border_style(Style::default().fg(self.trade_side.color())));

        let area = centered_rect(50, 55, f.area());
        f.render_widget(Clear, area);
        f.render_widget(popup, area);
    }
}

// Helper functions are now in super::super::common module

impl Default for PerpApp {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_app_creation() {
        let app = PerpApp::new();
        assert!(app.markets.is_empty());
        assert_eq!(app.active_tab, 0);
    }

    #[test]
    fn test_format_price() {
        assert_eq!(format_price(95000.0), "$95000.00");
        assert_eq!(format_price(180.25), "$180.2500");
        assert_eq!(format_price(0.0000325), "$0.00003250");
    }

    #[test]
    fn test_format_usd() {
        assert_eq!(format_usd(1_500_000_000.0), "$1.5B");
        assert_eq!(format_usd(45_000_000.0), "$45.0M");
        assert_eq!(format_usd(125_000.0), "$125.0K");
    }

    #[test]
    fn test_position_side() {
        assert_eq!(PositionSide::Long.name(), "LONG");
        assert_eq!(PositionSide::Short.name(), "SHORT");
    }
}
