//! AMM Liquidity Management TUI Application
//!
//! Main state struct for managing liquidity positions on Solana DEXs.

use super::super::common::{centered_rect, format_usd, load_wallet_address};

#[allow(unused_imports)]
use solana_sdk::signer::Signer;

use anyhow::Result;
use chrono::{DateTime, Utc};
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{
        Block, Borders, Cell, Clear, Gauge, List, ListItem, Paragraph, Row, Table, Tabs, Wrap,
    },
    Frame, Terminal,
};
use std::io;
use std::time::{Duration, Instant};

// ============================================================================
// Data Types
// ============================================================================

/// Supported AMM protocols
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AmmProtocol {
    Raydium,
    Orca,
    Meteora,
    Lifinity,
}

impl AmmProtocol {
    pub fn name(&self) -> &str {
        match self {
            AmmProtocol::Raydium => "Raydium",
            AmmProtocol::Orca => "Orca",
            AmmProtocol::Meteora => "Meteora",
            AmmProtocol::Lifinity => "Lifinity",
        }
    }

    pub fn color(&self) -> Color {
        match self {
            AmmProtocol::Raydium => Color::Magenta,
            AmmProtocol::Orca => Color::Cyan,
            AmmProtocol::Meteora => Color::Yellow,
            AmmProtocol::Lifinity => Color::Green,
        }
    }
}

/// A liquidity pool
#[derive(Debug, Clone)]
pub struct LiquidityPool {
    pub address: String,
    pub protocol: AmmProtocol,
    pub token_a: TokenInfo,
    pub token_b: TokenInfo,
    pub tvl_usd: f64,
    pub volume_24h_usd: f64,
    pub fee_tier: f64,      // e.g., 0.3 for 0.3%
    pub apr_24h: f64,       // Estimated APR from fees
    pub apr_7d: f64,
    pub price: f64,         // token_b per token_a
    pub reserve_a: f64,
    pub reserve_b: f64,
}

/// Token information
#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub mint: String,
    pub symbol: String,
    pub name: String,
    pub decimals: u8,
    pub price_usd: f64,
}

/// A user's liquidity position
#[derive(Debug, Clone)]
pub struct LiquidityPosition {
    pub id: String,
    pub pool: LiquidityPool,
    pub lp_tokens: f64,
    pub share_pct: f64,         // Share of pool
    pub value_usd: f64,
    pub token_a_amount: f64,
    pub token_b_amount: f64,
    pub initial_value_usd: f64,
    pub pnl_usd: f64,
    pub pnl_pct: f64,
    pub fees_earned_usd: f64,
    pub impermanent_loss_pct: f64,
    pub opened_at: DateTime<Utc>,
}

/// Pending transaction
#[derive(Debug, Clone)]
pub struct PendingTx {
    pub tx_type: TxType,
    pub pool_name: String,
    pub amount_a: f64,
    pub amount_b: f64,
    pub status: TxStatus,
    pub started_at: Instant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TxType {
    AddLiquidity,
    RemoveLiquidity,
    Harvest,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TxStatus {
    Building,
    Signing,
    Submitting,
    Confirming,
    Success,
    Failed,
}

// ============================================================================
// Application State
// ============================================================================

/// Main AMM application
pub struct AmmApp {
    // Wallet
    pub wallet_address: Option<String>,
    pub sol_balance: f64,

    // Data
    pub pools: Vec<LiquidityPool>,
    pub positions: Vec<LiquidityPosition>,
    pub filtered_pools: Vec<usize>,  // Indices into pools

    // UI State
    pub active_tab: usize,
    pub selected_pool: usize,
    pub selected_position: usize,
    pub scroll_offset: usize,
    pub should_quit: bool,

    // Modals
    pub show_add_liquidity: bool,
    pub show_remove_liquidity: bool,
    pub show_help: bool,

    // Add liquidity form
    pub add_amount_a: String,
    pub add_amount_b: String,
    pub add_focus: usize,  // 0 = amount_a, 1 = amount_b

    // Remove liquidity form
    pub remove_pct: f64,

    // Filters
    pub filter_protocol: Option<AmmProtocol>,
    pub filter_search: String,
    pub min_tvl: f64,
    pub min_apr: f64,

    // Transaction state
    pub pending_tx: Option<PendingTx>,

    // Status
    pub status_message: Option<String>,
    pub status_time: Option<Instant>,
    pub last_refresh: Instant,

    // Stats
    pub total_value_usd: f64,
    pub total_fees_earned: f64,
    pub avg_apr: f64,
}

impl AmmApp {
    /// Create a new AMM app
    pub fn new() -> Self {
        Self {
            wallet_address: None,
            sol_balance: 0.0,
            pools: Vec::new(),
            positions: Vec::new(),
            filtered_pools: Vec::new(),
            active_tab: 0,
            selected_pool: 0,
            selected_position: 0,
            scroll_offset: 0,
            should_quit: false,
            show_add_liquidity: false,
            show_remove_liquidity: false,
            show_help: false,
            add_amount_a: String::new(),
            add_amount_b: String::new(),
            add_focus: 0,
            remove_pct: 100.0,
            filter_protocol: None,
            filter_search: String::new(),
            min_tvl: 0.0,
            min_apr: 0.0,
            pending_tx: None,
            status_message: None,
            status_time: None,
            last_refresh: Instant::now(),
            total_value_usd: 0.0,
            total_fees_earned: 0.0,
            avg_apr: 0.0,
        }
    }

    /// Initialize with mock data for demonstration
    pub async fn initialize(&mut self) -> Result<()> {
        // Load wallet if available (using shared utility)
        self.wallet_address = load_wallet_address();

        // Load mock pools (would fetch from APIs in production)
        self.pools = self.fetch_mock_pools();
        self.apply_filters();

        // Load mock positions
        self.positions = self.fetch_mock_positions();
        self.calculate_totals();

        self.set_status("Loaded pools and positions");
        Ok(())
    }

    fn fetch_mock_pools(&self) -> Vec<LiquidityPool> {
        vec![
            LiquidityPool {
                address: "58oQChx4yWmvKdwLLZzBi4ChoCc2fqCUWBkwMihLYQo2".to_string(),
                protocol: AmmProtocol::Raydium,
                token_a: TokenInfo {
                    mint: "So11111111111111111111111111111111111111112".to_string(),
                    symbol: "SOL".to_string(),
                    name: "Solana".to_string(),
                    decimals: 9,
                    price_usd: 180.0,
                },
                token_b: TokenInfo {
                    mint: "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v".to_string(),
                    symbol: "USDC".to_string(),
                    name: "USD Coin".to_string(),
                    decimals: 6,
                    price_usd: 1.0,
                },
                tvl_usd: 45_000_000.0,
                volume_24h_usd: 12_500_000.0,
                fee_tier: 0.25,
                apr_24h: 32.5,
                apr_7d: 28.7,
                price: 180.0,
                reserve_a: 125_000.0,
                reserve_b: 22_500_000.0,
            },
            LiquidityPool {
                address: "7XawhbbxtsRcQA8KTkHT9f9nc6d69UwqCDh6U5EEbEmX".to_string(),
                protocol: AmmProtocol::Orca,
                token_a: TokenInfo {
                    mint: "So11111111111111111111111111111111111111112".to_string(),
                    symbol: "SOL".to_string(),
                    name: "Solana".to_string(),
                    decimals: 9,
                    price_usd: 180.0,
                },
                token_b: TokenInfo {
                    mint: "mSoLzYCxHdYgdzU16g5QSh3i5K3z3KZK7ytfqcJm7So".to_string(),
                    symbol: "mSOL".to_string(),
                    name: "Marinade SOL".to_string(),
                    decimals: 9,
                    price_usd: 195.0,
                },
                tvl_usd: 28_000_000.0,
                volume_24h_usd: 3_200_000.0,
                fee_tier: 0.01,
                apr_24h: 8.2,
                apr_7d: 7.5,
                price: 0.923,
                reserve_a: 78_000.0,
                reserve_b: 72_000.0,
            },
            LiquidityPool {
                address: "HJPjoWUrhoZzkNfRpHuieeFk9WcZWjwy6PBjZ81ngndJ".to_string(),
                protocol: AmmProtocol::Meteora,
                token_a: TokenInfo {
                    mint: "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN".to_string(),
                    symbol: "JUP".to_string(),
                    name: "Jupiter".to_string(),
                    decimals: 6,
                    price_usd: 1.25,
                },
                token_b: TokenInfo {
                    mint: "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v".to_string(),
                    symbol: "USDC".to_string(),
                    name: "USD Coin".to_string(),
                    decimals: 6,
                    price_usd: 1.0,
                },
                tvl_usd: 15_000_000.0,
                volume_24h_usd: 8_500_000.0,
                fee_tier: 0.3,
                apr_24h: 45.2,
                apr_7d: 38.1,
                price: 1.25,
                reserve_a: 6_000_000.0,
                reserve_b: 7_500_000.0,
            },
            LiquidityPool {
                address: "CuEDMUqgkGTVcAaqEDHuVR848XN38MPsD11JrkxcDjYT".to_string(),
                protocol: AmmProtocol::Raydium,
                token_a: TokenInfo {
                    mint: "RasWtVMRJETJPCz8ynEBkYoixEBm3HvKfvQ2XCQYhSN".to_string(),
                    symbol: "RAY".to_string(),
                    name: "Raydium".to_string(),
                    decimals: 6,
                    price_usd: 2.80,
                },
                token_b: TokenInfo {
                    mint: "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v".to_string(),
                    symbol: "USDC".to_string(),
                    name: "USD Coin".to_string(),
                    decimals: 6,
                    price_usd: 1.0,
                },
                tvl_usd: 8_500_000.0,
                volume_24h_usd: 2_100_000.0,
                fee_tier: 0.25,
                apr_24h: 22.8,
                apr_7d: 19.5,
                price: 2.80,
                reserve_a: 1_500_000.0,
                reserve_b: 4_200_000.0,
            },
        ]
    }

    fn fetch_mock_positions(&self) -> Vec<LiquidityPosition> {
        if self.pools.is_empty() {
            return Vec::new();
        }

        vec![
            LiquidityPosition {
                id: "pos_001".to_string(),
                pool: self.pools[0].clone(),
                lp_tokens: 125.5,
                share_pct: 0.0028,
                value_usd: 1_260.0,
                token_a_amount: 3.5,
                token_b_amount: 630.0,
                initial_value_usd: 1_100.0,
                pnl_usd: 160.0,
                pnl_pct: 14.5,
                fees_earned_usd: 45.20,
                impermanent_loss_pct: -1.2,
                opened_at: Utc::now() - chrono::Duration::days(14),
            },
            LiquidityPosition {
                id: "pos_002".to_string(),
                pool: self.pools[2].clone(),
                lp_tokens: 850.0,
                share_pct: 0.0057,
                value_usd: 855.0,
                token_a_amount: 342.0,
                token_b_amount: 427.5,
                initial_value_usd: 800.0,
                pnl_usd: 55.0,
                pnl_pct: 6.9,
                fees_earned_usd: 28.75,
                impermanent_loss_pct: -0.5,
                opened_at: Utc::now() - chrono::Duration::days(7),
            },
        ]
    }

    fn calculate_totals(&mut self) {
        self.total_value_usd = self.positions.iter().map(|p| p.value_usd).sum();
        self.total_fees_earned = self.positions.iter().map(|p| p.fees_earned_usd).sum();

        if !self.positions.is_empty() {
            let total_weighted_apr: f64 = self.positions
                .iter()
                .map(|p| p.pool.apr_7d * p.value_usd)
                .sum();
            self.avg_apr = total_weighted_apr / self.total_value_usd;
        }
    }

    fn apply_filters(&mut self) {
        self.filtered_pools = self.pools
            .iter()
            .enumerate()
            .filter(|(_, pool)| {
                // Protocol filter
                if let Some(protocol) = self.filter_protocol {
                    if pool.protocol != protocol {
                        return false;
                    }
                }

                // Search filter
                if !self.filter_search.is_empty() {
                    let search = self.filter_search.to_lowercase();
                    let matches = pool.token_a.symbol.to_lowercase().contains(&search)
                        || pool.token_b.symbol.to_lowercase().contains(&search)
                        || pool.token_a.name.to_lowercase().contains(&search)
                        || pool.token_b.name.to_lowercase().contains(&search);
                    if !matches {
                        return false;
                    }
                }

                // TVL filter
                if pool.tvl_usd < self.min_tvl {
                    return false;
                }

                // APR filter
                if pool.apr_7d < self.min_apr {
                    return false;
                }

                true
            })
            .map(|(i, _)| i)
            .collect();

        // Reset selection if out of bounds
        if self.selected_pool >= self.filtered_pools.len() {
            self.selected_pool = 0;
        }
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

    /// Handle key input
    async fn handle_key(&mut self, key: KeyEvent) -> Result<()> {
        // Help overlay
        if self.show_help {
            if matches!(key.code, KeyCode::Esc | KeyCode::Char('?')) {
                self.show_help = false;
            }
            return Ok(());
        }

        // Add liquidity modal
        if self.show_add_liquidity {
            return self.handle_add_liquidity_key(key);
        }

        // Remove liquidity modal
        if self.show_remove_liquidity {
            return self.handle_remove_liquidity_key(key);
        }

        // Main controls
        match key.code {
            KeyCode::Char('q') | KeyCode::Esc => {
                self.should_quit = true;
            }
            KeyCode::Char('?') => {
                self.show_help = true;
            }
            // Tab navigation
            KeyCode::Tab => {
                self.active_tab = (self.active_tab + 1) % 3;
            }
            KeyCode::BackTab => {
                self.active_tab = if self.active_tab == 0 { 2 } else { self.active_tab - 1 };
            }
            KeyCode::Char('1') => self.active_tab = 0,
            KeyCode::Char('2') => self.active_tab = 1,
            KeyCode::Char('3') => self.active_tab = 2,
            // Selection
            KeyCode::Up | KeyCode::Char('k') => {
                match self.active_tab {
                    0 => {
                        if self.selected_pool > 0 {
                            self.selected_pool -= 1;
                        }
                    }
                    1 => {
                        if self.selected_position > 0 {
                            self.selected_position -= 1;
                        }
                    }
                    _ => {}
                }
            }
            KeyCode::Down | KeyCode::Char('j') => {
                match self.active_tab {
                    0 => {
                        if self.selected_pool < self.filtered_pools.len().saturating_sub(1) {
                            self.selected_pool += 1;
                        }
                    }
                    1 => {
                        if self.selected_position < self.positions.len().saturating_sub(1) {
                            self.selected_position += 1;
                        }
                    }
                    _ => {}
                }
            }
            // Actions
            KeyCode::Enter | KeyCode::Char('a') => {
                if self.active_tab == 0 && !self.filtered_pools.is_empty() {
                    self.show_add_liquidity = true;
                    self.add_amount_a.clear();
                    self.add_amount_b.clear();
                    self.add_focus = 0;
                }
            }
            KeyCode::Char('r') => {
                if self.active_tab == 1 && !self.positions.is_empty() {
                    self.show_remove_liquidity = true;
                    self.remove_pct = 100.0;
                }
            }
            KeyCode::Char('h') => {
                if self.active_tab == 1 && !self.positions.is_empty() {
                    self.set_status("Harvesting fees... (simulated)");
                }
            }
            // Protocol filters
            KeyCode::Char('R') => {
                self.filter_protocol = if self.filter_protocol == Some(AmmProtocol::Raydium) {
                    None
                } else {
                    Some(AmmProtocol::Raydium)
                };
                self.apply_filters();
            }
            KeyCode::Char('O') => {
                self.filter_protocol = if self.filter_protocol == Some(AmmProtocol::Orca) {
                    None
                } else {
                    Some(AmmProtocol::Orca)
                };
                self.apply_filters();
            }
            KeyCode::Char('M') => {
                self.filter_protocol = if self.filter_protocol == Some(AmmProtocol::Meteora) {
                    None
                } else {
                    Some(AmmProtocol::Meteora)
                };
                self.apply_filters();
            }
            // Search
            KeyCode::Char('/') => {
                // Would enter search mode
                self.set_status("Search: type to filter pools");
            }
            KeyCode::Char('c') => {
                // Clear filters
                self.filter_protocol = None;
                self.filter_search.clear();
                self.apply_filters();
                self.set_status("Filters cleared");
            }
            _ => {}
        }

        Ok(())
    }

    fn handle_add_liquidity_key(&mut self, key: KeyEvent) -> Result<()> {
        match key.code {
            KeyCode::Esc => {
                self.show_add_liquidity = false;
            }
            KeyCode::Tab => {
                self.add_focus = (self.add_focus + 1) % 2;
            }
            KeyCode::Enter => {
                // Would execute add liquidity
                self.show_add_liquidity = false;
                self.set_status("Adding liquidity... (simulated)");
            }
            KeyCode::Char(c) if c.is_ascii_digit() || c == '.' => {
                if self.add_focus == 0 {
                    self.add_amount_a.push(c);
                } else {
                    self.add_amount_b.push(c);
                }
            }
            KeyCode::Backspace => {
                if self.add_focus == 0 {
                    self.add_amount_a.pop();
                } else {
                    self.add_amount_b.pop();
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn handle_remove_liquidity_key(&mut self, key: KeyEvent) -> Result<()> {
        match key.code {
            KeyCode::Esc => {
                self.show_remove_liquidity = false;
            }
            KeyCode::Enter => {
                self.show_remove_liquidity = false;
                self.set_status(&format!("Removing {}% liquidity... (simulated)", self.remove_pct));
            }
            KeyCode::Left => {
                self.remove_pct = (self.remove_pct - 25.0).max(25.0);
            }
            KeyCode::Right => {
                self.remove_pct = (self.remove_pct + 25.0).min(100.0);
            }
            KeyCode::Char('1') => self.remove_pct = 25.0,
            KeyCode::Char('2') => self.remove_pct = 50.0,
            KeyCode::Char('3') => self.remove_pct = 75.0,
            KeyCode::Char('4') => self.remove_pct = 100.0,
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

        // Modals
        if self.show_help {
            self.render_help(f);
        }
        if self.show_add_liquidity {
            self.render_add_liquidity_modal(f);
        }
        if self.show_remove_liquidity {
            self.render_remove_liquidity_modal(f);
        }
    }

    fn render_header(&self, f: &mut Frame, area: Rect) {
        let wallet_info = self.wallet_address
            .as_ref()
            .map(|w| format!("{}...{}", &w[..4], &w[40..]))
            .unwrap_or_else(|| "No wallet".to_string());

        let header = Paragraph::new(Line::from(vec![
            Span::styled(" 💧 OSVM AMM ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::raw("│ "),
            Span::styled(&wallet_info, Style::default().fg(Color::Green)),
            Span::raw(" │ "),
            Span::styled(format!("Portfolio: ${:.2}", self.total_value_usd), Style::default().fg(Color::White)),
            Span::raw(" │ "),
            Span::styled(format!("Fees: ${:.2}", self.total_fees_earned), Style::default().fg(Color::Yellow)),
            Span::raw(" │ "),
            Span::styled(format!("Avg APR: {:.1}%", self.avg_apr), Style::default().fg(Color::Magenta)),
        ]))
        .block(Block::default().borders(Borders::BOTTOM).border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(header, area);
    }

    fn render_tabs(&self, f: &mut Frame, area: Rect) {
        let titles = vec![
            format!("📊 Pools ({})", self.filtered_pools.len()),
            format!("💼 Positions ({})", self.positions.len()),
            "📈 Analytics".to_string(),
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
            0 => self.render_pools(f, area),
            1 => self.render_positions(f, area),
            2 => self.render_analytics(f, area),
            _ => {}
        }
    }

    fn render_pools(&self, f: &mut Frame, area: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(2),  // Filter bar
                Constraint::Min(5),     // Pool list
            ])
            .split(area);

        // Filter bar
        let filter_text = if let Some(protocol) = self.filter_protocol {
            format!("Filter: {} │ ", protocol.name())
        } else {
            "Filter: All │ ".to_string()
        };

        let filter_line = Line::from(vec![
            Span::styled(&filter_text, Style::default().fg(Color::Yellow)),
            Span::styled("[R]", Style::default().fg(AmmProtocol::Raydium.color())),
            Span::raw("aydium "),
            Span::styled("[O]", Style::default().fg(AmmProtocol::Orca.color())),
            Span::raw("rca "),
            Span::styled("[M]", Style::default().fg(AmmProtocol::Meteora.color())),
            Span::raw("eteora "),
            Span::styled("[c]", Style::default().fg(Color::DarkGray)),
            Span::raw("lear"),
        ]);
        f.render_widget(Paragraph::new(filter_line), chunks[0]);

        // Pool table
        if self.filtered_pools.is_empty() {
            let empty = Paragraph::new("No pools match filters")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default().title(" Pools ").borders(Borders::ALL));
            f.render_widget(empty, chunks[1]);
            return;
        }

        let rows: Vec<Row> = self.filtered_pools
            .iter()
            .enumerate()
            .map(|(i, &pool_idx)| {
                let pool = &self.pools[pool_idx];
                let is_selected = i == self.selected_pool;
                let style = if is_selected {
                    Style::default().bg(Color::DarkGray)
                } else {
                    Style::default()
                };

                Row::new(vec![
                    Cell::from(pool.protocol.name()).style(Style::default().fg(pool.protocol.color())),
                    Cell::from(format!("{}/{}", pool.token_a.symbol, pool.token_b.symbol)),
                    Cell::from(format_usd(pool.tvl_usd)),
                    Cell::from(format_usd(pool.volume_24h_usd)),
                    Cell::from(format!("{:.2}%", pool.fee_tier)),
                    Cell::from(format!("{:.1}%", pool.apr_24h)).style(Style::default().fg(Color::Green)),
                    Cell::from(format!("{:.1}%", pool.apr_7d)).style(Style::default().fg(Color::Cyan)),
                ])
                .style(style)
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(10),
                Constraint::Length(12),
                Constraint::Length(12),
                Constraint::Length(12),
                Constraint::Length(8),
                Constraint::Length(10),
                Constraint::Length(10),
            ],
        )
        .header(
            Row::new(vec!["Protocol", "Pair", "TVL", "Vol 24h", "Fee", "APR 24h", "APR 7d"])
                .style(Style::default().fg(Color::Yellow))
        )
        .block(Block::default()
            .title(" Pools │ [a] Add Liquidity │ [Enter] Select ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, chunks[1]);
    }

    fn render_positions(&self, f: &mut Frame, area: Rect) {
        if self.positions.is_empty() {
            let empty = Paragraph::new("No liquidity positions\n\nGo to Pools tab and press [a] to add liquidity")
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

                let pnl_color = if pos.pnl_usd >= 0.0 { Color::Green } else { Color::Red };
                let il_color = if pos.impermanent_loss_pct.abs() < 1.0 { Color::Yellow } else { Color::Red };

                Row::new(vec![
                    Cell::from(format!("{}/{}", pos.pool.token_a.symbol, pos.pool.token_b.symbol)),
                    Cell::from(pos.pool.protocol.name()).style(Style::default().fg(pos.pool.protocol.color())),
                    Cell::from(format!("${:.2}", pos.value_usd)),
                    Cell::from(format!("{:.4}%", pos.share_pct)),
                    Cell::from(format!("{:+.2}", pos.pnl_usd)).style(Style::default().fg(pnl_color)),
                    Cell::from(format!("{:+.1}%", pos.pnl_pct)).style(Style::default().fg(pnl_color)),
                    Cell::from(format!("${:.2}", pos.fees_earned_usd)).style(Style::default().fg(Color::Yellow)),
                    Cell::from(format!("{:.2}%", pos.impermanent_loss_pct)).style(Style::default().fg(il_color)),
                ])
                .style(style)
            })
            .collect();

        let table = Table::new(
            rows,
            [
                Constraint::Length(12),
                Constraint::Length(10),
                Constraint::Length(12),
                Constraint::Length(10),
                Constraint::Length(10),
                Constraint::Length(8),
                Constraint::Length(10),
                Constraint::Length(8),
            ],
        )
        .header(
            Row::new(vec!["Pair", "Protocol", "Value", "Share", "PnL $", "PnL %", "Fees", "IL"])
                .style(Style::default().fg(Color::Yellow))
        )
        .block(Block::default()
            .title(" Your Positions │ [r] Remove │ [h] Harvest Fees ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)));

        f.render_widget(table, area);
    }

    fn render_analytics(&self, f: &mut Frame, area: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(area);

        // Left: Summary stats
        let stats = vec![
            Line::from(""),
            Line::from(vec![
                Span::styled("  Total Value:      ", Style::default().fg(Color::Gray)),
                Span::styled(format!("${:.2}", self.total_value_usd), Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
            ]),
            Line::from(vec![
                Span::styled("  Total Fees:       ", Style::default().fg(Color::Gray)),
                Span::styled(format!("${:.2}", self.total_fees_earned), Style::default().fg(Color::Yellow)),
            ]),
            Line::from(vec![
                Span::styled("  Avg APR:          ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{:.1}%", self.avg_apr), Style::default().fg(Color::Green)),
            ]),
            Line::from(vec![
                Span::styled("  Positions:        ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{}", self.positions.len()), Style::default().fg(Color::Cyan)),
            ]),
            Line::from(""),
            Line::from(vec![Span::styled("  Protocol Distribution:", Style::default().fg(Color::Yellow))]),
        ];

        // Count positions by protocol
        let mut by_protocol: std::collections::HashMap<AmmProtocol, (usize, f64)> = std::collections::HashMap::new();
        for pos in &self.positions {
            let entry = by_protocol.entry(pos.pool.protocol).or_insert((0, 0.0));
            entry.0 += 1;
            entry.1 += pos.value_usd;
        }

        let mut stats = stats;
        for (protocol, (count, value)) in by_protocol {
            let name = protocol.name().to_string();
            let color = protocol.color();
            stats.push(Line::from(vec![
                Span::raw("    "),
                Span::styled(name, Style::default().fg(color)),
                Span::raw(format!(": {} pos, ${:.2}", count, value)),
            ]));
        }

        let stats_block = Paragraph::new(stats)
            .block(Block::default().title(" Portfolio Summary ").borders(Borders::ALL).border_style(Style::default().fg(Color::DarkGray)));
        f.render_widget(stats_block, chunks[0]);

        // Right: Top pools by APR
        let mut sorted_pools: Vec<_> = self.pools.iter().collect();
        sorted_pools.sort_by(|a, b| b.apr_7d.partial_cmp(&a.apr_7d).unwrap_or(std::cmp::Ordering::Equal));

        let top_pools: Vec<ListItem> = sorted_pools
            .iter()
            .take(8)
            .map(|pool| {
                ListItem::new(Line::from(vec![
                    Span::styled(format!("{:.1}%", pool.apr_7d), Style::default().fg(Color::Green).add_modifier(Modifier::BOLD)),
                    Span::raw("  "),
                    Span::styled(format!("{}/{}", pool.token_a.symbol, pool.token_b.symbol), Style::default().fg(Color::White)),
                    Span::raw("  "),
                    Span::styled(pool.protocol.name(), Style::default().fg(pool.protocol.color())),
                ]))
            })
            .collect();

        let top_list = List::new(top_pools)
            .block(Block::default().title(" Top Pools by APR ").borders(Borders::ALL).border_style(Style::default().fg(Color::DarkGray)));
        f.render_widget(top_list, chunks[1]);
    }

    fn render_status(&self, f: &mut Frame, area: Rect) {
        let status = self.status_message.as_deref().unwrap_or("");
        let help = " [?] Help  [Tab] Switch  [↑↓] Navigate  [q] Quit ";

        let line = Line::from(vec![
            Span::styled(help, Style::default().fg(Color::DarkGray)),
            Span::raw("  "),
            Span::styled(status, Style::default().fg(Color::Yellow)),
        ]);

        f.render_widget(Paragraph::new(line), area);
    }

    fn render_help(&self, f: &mut Frame) {
        let help_text = vec![
            Line::from(""),
            Line::from(vec![Span::styled("  KEYBOARD SHORTCUTS", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD))]),
            Line::from(""),
            Line::from("  [1-3]     Switch tabs"),
            Line::from("  [Tab]     Next tab"),
            Line::from("  [↑↓/jk]   Navigate list"),
            Line::from(""),
            Line::from(vec![Span::styled("  POOLS TAB", Style::default().fg(Color::Cyan))]),
            Line::from("  [a]       Add liquidity to selected pool"),
            Line::from("  [R/O/M]   Filter by protocol"),
            Line::from("  [c]       Clear filters"),
            Line::from(""),
            Line::from(vec![Span::styled("  POSITIONS TAB", Style::default().fg(Color::Cyan))]),
            Line::from("  [r]       Remove liquidity"),
            Line::from("  [h]       Harvest fees"),
            Line::from(""),
            Line::from("  [q/Esc]   Quit"),
            Line::from(""),
        ];

        let popup = Paragraph::new(help_text)
            .block(Block::default()
                .title(" Help ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Cyan)));

        let area = centered_rect(50, 60, f.area());
        f.render_widget(Clear, area);
        f.render_widget(popup, area);
    }

    fn render_add_liquidity_modal(&self, f: &mut Frame) {
        let pool_idx = self.filtered_pools.get(self.selected_pool).copied().unwrap_or(0);
        let pool = &self.pools[pool_idx];

        let lines = vec![
            Line::from(""),
            Line::from(vec![
                Span::styled("  Pool: ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{}/{}", pool.token_a.symbol, pool.token_b.symbol), Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::raw(" on "),
                Span::styled(pool.protocol.name(), Style::default().fg(pool.protocol.color())),
            ]),
            Line::from(vec![
                Span::styled("  APR: ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{:.1}%", pool.apr_7d), Style::default().fg(Color::Green)),
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled(format!("  {} Amount: ", pool.token_a.symbol), Style::default().fg(if self.add_focus == 0 { Color::Cyan } else { Color::Gray })),
                Span::styled(if self.add_amount_a.is_empty() { "0.0" } else { &self.add_amount_a }, Style::default().fg(Color::White)),
                if self.add_focus == 0 { Span::styled("│", Style::default().fg(Color::Cyan)) } else { Span::raw("") },
            ]),
            Line::from(vec![
                Span::styled(format!("  {} Amount: ", pool.token_b.symbol), Style::default().fg(if self.add_focus == 1 { Color::Cyan } else { Color::Gray })),
                Span::styled(if self.add_amount_b.is_empty() { "0.0" } else { &self.add_amount_b }, Style::default().fg(Color::White)),
                if self.add_focus == 1 { Span::styled("│", Style::default().fg(Color::Cyan)) } else { Span::raw("") },
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled("  [Tab] ", Style::default().fg(Color::Yellow)),
                Span::raw("Switch field  "),
                Span::styled("[Enter] ", Style::default().fg(Color::Green)),
                Span::raw("Confirm  "),
                Span::styled("[Esc] ", Style::default().fg(Color::Red)),
                Span::raw("Cancel"),
            ]),
        ];

        let popup = Paragraph::new(lines)
            .block(Block::default()
                .title(" Add Liquidity ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Yellow)));

        let area = centered_rect(50, 40, f.area());
        f.render_widget(Clear, area);
        f.render_widget(popup, area);
    }

    fn render_remove_liquidity_modal(&self, f: &mut Frame) {
        let pos = &self.positions[self.selected_position];

        let lines = vec![
            Line::from(""),
            Line::from(vec![
                Span::styled("  Position: ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{}/{}", pos.pool.token_a.symbol, pos.pool.token_b.symbol), Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            ]),
            Line::from(vec![
                Span::styled("  Value: ", Style::default().fg(Color::Gray)),
                Span::styled(format!("${:.2}", pos.value_usd), Style::default().fg(Color::White)),
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled("  Remove: ", Style::default().fg(Color::Gray)),
                Span::styled(format!("{}%", self.remove_pct), Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
            ]),
            Line::from(""),
            // Percentage bar
            Line::from(vec![
                Span::raw("  "),
                Span::styled(
                    "█".repeat((self.remove_pct / 5.0) as usize),
                    Style::default().fg(Color::Yellow)
                ),
                Span::styled(
                    "░".repeat(20 - (self.remove_pct / 5.0) as usize),
                    Style::default().fg(Color::DarkGray)
                ),
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled("  [1] 25%  [2] 50%  [3] 75%  [4] 100%", Style::default().fg(Color::DarkGray)),
            ]),
            Line::from(vec![
                Span::styled("  [←→] Adjust  [Enter] Confirm  [Esc] Cancel", Style::default().fg(Color::DarkGray)),
            ]),
        ];

        let popup = Paragraph::new(lines)
            .block(Block::default()
                .title(" Remove Liquidity ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Red)));

        let area = centered_rect(50, 45, f.area());
        f.render_widget(Clear, area);
        f.render_widget(popup, area);
    }
}

// Helper functions are now in super::super::common module

impl Default for AmmApp {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_app_creation() {
        let app = AmmApp::new();
        assert!(app.pools.is_empty());
        assert_eq!(app.active_tab, 0);
    }

    #[test]
    fn test_format_usd() {
        assert_eq!(format_usd(1_500_000.0), "$1.5M");
        assert_eq!(format_usd(45_000.0), "$45.0K");
        assert_eq!(format_usd(500.0), "$500.00");
    }
}
