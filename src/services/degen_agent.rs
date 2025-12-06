//! Autonomous Trading Agent for pump.fun markets
//!
//! This service provides 24/7 automated trading with configurable strategies,
//! position management, and risk controls.

use anyhow::{anyhow, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::{
    pubkey::Pubkey,
    signature::Signature,
    signer::{keypair::Keypair, Signer},
};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{mpsc, Mutex, RwLock};

use crate::services::pumpfun_client::{PumpFunClient, PumpFunConfig};
use crate::utils::tui::swap::{JupiterClient, QuoteParams, TokenInfo, TokenRegistry};

// ============================================================================
// Configuration
// ============================================================================

/// Configuration for the degen agent
#[derive(Debug, Clone)]
pub struct DegenConfig {
    // Strategy
    pub strategy: String,

    // Position sizing
    pub max_position_sol: f64,
    pub max_positions: usize,
    pub min_liquidity_sol: f64,

    // Risk management
    pub stop_loss_pct: f64,
    pub take_profit_pct: f64,
    pub trailing_stop: bool,
    pub max_daily_loss_sol: f64,

    // Operation modes
    pub dry_run: bool,
    pub headless: bool,
    pub auto_start: bool,

    // Filtering
    pub min_holders: u32,
    pub min_volume_sol: f64,
    pub max_age_hours: u32,
    pub blacklist_path: Option<String>,

    // Execution
    pub slippage_bps: u16,
    pub priority_fee: String,

    // Logging
    pub log_trades_path: Option<String>,
    pub webhook_url: Option<String>,

    // Strategy parameters
    pub momentum_threshold_pct: f64,
    pub cooldown_secs: u64,

    // Custom strategy
    pub strategy_file: Option<String>,
}

impl Default for DegenConfig {
    fn default() -> Self {
        Self {
            strategy: "momentum".to_string(),
            max_position_sol: 0.1,
            max_positions: 5,
            min_liquidity_sol: 10.0,
            stop_loss_pct: 15.0,
            take_profit_pct: 100.0,
            trailing_stop: false,
            max_daily_loss_sol: 1.0,
            dry_run: true,
            headless: false,
            auto_start: false,
            min_holders: 100,
            min_volume_sol: 50.0,
            max_age_hours: 24,
            blacklist_path: None,
            slippage_bps: 100,
            priority_fee: "auto".to_string(),
            log_trades_path: None,
            webhook_url: None,
            momentum_threshold_pct: 5.0,
            cooldown_secs: 300,
            strategy_file: None,
        }
    }
}

// ============================================================================
// Market Data Types
// ============================================================================

/// Real-time token data from pump.fun
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TokenMarketData {
    pub mint: String,
    pub symbol: String,
    pub name: String,
    pub price_sol: f64,
    pub price_usd: f64,
    pub market_cap_sol: f64,
    pub liquidity_sol: f64,
    pub volume_24h_sol: f64,
    pub holders: u32,
    pub created_at: DateTime<Utc>,
    pub price_change_1m: f64,
    pub price_change_5m: f64,
    pub price_change_1h: f64,
    pub price_change_24h: f64,
    pub last_updated: DateTime<Utc>,
}

// ============================================================================
// Trenches Data Types (like GMGN.ai)
// ============================================================================

/// New token launch from pump.fun
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NewTokenLaunch {
    pub mint: String,
    pub symbol: String,
    pub name: String,
    pub creator: String,
    pub created_at: DateTime<Utc>,
    pub initial_liquidity_sol: f64,
    pub current_liquidity_sol: f64,
    pub current_price_sol: f64,
    pub market_cap_sol: f64,
    pub holders: u32,
    pub buy_count: u32,
    pub sell_count: u32,
    pub volume_sol: f64,
    pub price_change_since_launch: f64,
    pub age_minutes: i64,
    pub safety: TokenSafety,
    pub dev_holdings_pct: f64,
    pub top10_holdings_pct: f64,
    pub is_graduated: bool,  // Graduated from pump.fun to Raydium
}

/// Trending token with momentum data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrendingToken {
    pub mint: String,
    pub symbol: String,
    pub name: String,
    pub price_sol: f64,
    pub market_cap_sol: f64,
    pub volume_1h_sol: f64,
    pub volume_24h_sol: f64,
    pub holders: u32,
    pub holder_change_1h: i32,
    pub price_change_5m: f64,
    pub price_change_1h: f64,
    pub price_change_24h: f64,
    pub buy_pressure: f64,  // Ratio of buys vs sells
    pub smart_money_buys: u32,  // Buys from tracked wallets
    pub trend_score: f64,   // 0-100 composite score
    pub safety: TokenSafety,
}

/// Top gainer/loser entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GainerLoser {
    pub mint: String,
    pub symbol: String,
    pub name: String,
    pub price_sol: f64,
    pub market_cap_sol: f64,
    pub price_change_pct: f64,
    pub volume_sol: f64,
    pub timeframe: GainerTimeframe,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GainerTimeframe {
    FiveMin,
    OneHour,
    SixHour,
    TwentyFourHour,
}

/// Token safety analysis
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TokenSafety {
    pub is_honeypot: bool,
    pub is_mintable: bool,
    pub is_freezable: bool,
    pub has_blacklist: bool,
    pub buy_tax_pct: f64,
    pub sell_tax_pct: f64,
    pub liquidity_locked: bool,
    pub lp_burned_pct: f64,
    pub top_holder_pct: f64,
    pub risk_score: u8,  // 0-100 (0 = safe, 100 = dangerous)
    pub risk_flags: Vec<String>,
}

impl TokenSafety {
    pub fn is_safe(&self) -> bool {
        !self.is_honeypot
            && !self.is_mintable
            && self.buy_tax_pct < 10.0
            && self.sell_tax_pct < 10.0
            && self.risk_score < 50
    }
}

// ============================================================================
// Smart Money Tracking
// ============================================================================

/// Tracked whale/smart money wallet
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackedWallet {
    pub address: String,
    pub label: Option<String>,  // "Whale #1", "KOL xyz", etc.
    pub wallet_type: WalletType,
    pub total_pnl_sol: f64,
    pub win_rate: f64,
    pub total_trades: u32,
    pub avg_hold_time_mins: u32,
    pub last_active: DateTime<Utc>,
    pub is_copying: bool,  // Are we copy trading this wallet?
    pub copy_amount_sol: f64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum WalletType {
    Whale,       // High-value trader
    SmartMoney,  // Consistently profitable
    Kol,         // Key Opinion Leader
    Insider,     // Early/dev access patterns
    Sniper,      // Fast entry specialist
    Custom,      // User-added wallet
}

/// Recent activity from a tracked wallet
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WalletActivity {
    pub wallet: String,
    pub wallet_label: Option<String>,
    pub action: WalletAction,
    pub mint: String,
    pub symbol: String,
    pub amount_tokens: f64,
    pub amount_sol: f64,
    pub price_sol: f64,
    pub timestamp: DateTime<Utc>,
    pub tx_signature: String,
    pub pnl_sol: Option<f64>,  // For sells, the realized PnL
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum WalletAction {
    Buy,
    Sell,
    AddLiquidity,
    RemoveLiquidity,
    Transfer,
}

/// Copy trade configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CopyTradeConfig {
    pub wallet: String,
    pub enabled: bool,
    pub copy_buys: bool,
    pub copy_sells: bool,
    pub amount_sol: f64,
    pub max_slippage_bps: u16,
    pub min_liquidity_sol: f64,
    pub min_market_cap_sol: f64,
    pub only_new_positions: bool,  // Only copy if wallet is opening new position
    pub stop_loss_pct: Option<f64>,
    pub take_profit_pct: Option<f64>,
}

/// Smart money aggregated signal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SmartMoneySignal {
    pub mint: String,
    pub symbol: String,
    pub signal_type: SmartMoneySignalType,
    pub whale_buys: u32,
    pub whale_sells: u32,
    pub net_flow_sol: f64,  // Positive = inflow, negative = outflow
    pub unique_whales: u32,
    pub confidence: f64,
    pub timestamp: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SmartMoneySignalType {
    Accumulation,  // Whales buying
    Distribution,  // Whales selling
    Neutral,
}

// ============================================================================
// Trenches View State
// ============================================================================

/// Complete trenches data for the TUI
#[derive(Debug, Clone, Default)]
pub struct TrenchesData {
    pub new_tokens: Vec<NewTokenLaunch>,
    pub trending: Vec<TrendingToken>,
    pub top_gainers_5m: Vec<GainerLoser>,
    pub top_gainers_1h: Vec<GainerLoser>,
    pub top_losers_5m: Vec<GainerLoser>,
    pub top_losers_1h: Vec<GainerLoser>,
    pub last_updated: Option<DateTime<Utc>>,
}

/// Smart money view state
#[derive(Debug, Clone, Default)]
pub struct SmartMoneyData {
    pub tracked_wallets: Vec<TrackedWallet>,
    pub recent_activity: Vec<WalletActivity>,
    pub signals: Vec<SmartMoneySignal>,
    pub copy_configs: Vec<CopyTradeConfig>,
    pub last_updated: Option<DateTime<Utc>>,
}

/// A price tick from the market
#[derive(Debug, Clone)]
pub struct PriceTick {
    pub mint: String,
    pub price_sol: f64,
    pub timestamp: DateTime<Utc>,
}

// ============================================================================
// Position Types
// ============================================================================

/// A trading position
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub id: String,
    pub mint: String,
    pub symbol: String,
    pub entry_price_sol: f64,
    pub current_price_sol: f64,
    pub amount: f64,           // Token amount
    pub cost_basis_sol: f64,   // How much SOL spent
    pub current_value_sol: f64,
    pub unrealized_pnl_sol: f64,
    pub unrealized_pnl_pct: f64,
    pub entry_time: DateTime<Utc>,
    pub stop_loss_price: f64,
    pub take_profit_price: f64,
    pub trailing_stop_high: Option<f64>,
    pub status: PositionStatus,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum PositionStatus {
    Open,
    ClosedProfit,
    ClosedLoss,
    ClosedStopLoss,
    ClosedTakeProfit,
}

/// A completed trade
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Trade {
    pub id: String,
    pub position_id: String,
    pub side: TradeSide,
    pub mint: String,
    pub symbol: String,
    pub amount: f64,
    pub price_sol: f64,
    pub value_sol: f64,
    pub signature: Option<String>,
    pub timestamp: DateTime<Utc>,
    pub strategy: String,
    pub reason: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TradeSide {
    Buy,
    Sell,
}

// ============================================================================
// Strategy Types
// ============================================================================

/// A trading signal from a strategy
#[derive(Debug, Clone)]
pub struct TradeSignal {
    pub signal_type: SignalType,
    pub mint: String,
    pub symbol: String,
    pub confidence: f64,       // 0.0 to 1.0
    pub suggested_size_sol: f64,
    pub reason: String,
    pub strategy: String,
    pub timestamp: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignalType {
    Buy,
    Sell,
    Hold,
}

/// Strategy trait for implementing trading strategies
pub trait TradingStrategy: Send + Sync {
    /// Analyze market data and generate signals
    fn analyze(&self, market_data: &[TokenMarketData], positions: &[Position]) -> Vec<TradeSignal>;

    /// Get strategy name
    fn name(&self) -> &str;

    /// Get strategy description
    fn description(&self) -> &str;
}

// ============================================================================
// Built-in Strategies
// ============================================================================

/// Momentum strategy - buy tokens with strong upward price movement
pub struct MomentumStrategy {
    threshold_pct: f64,
}

impl MomentumStrategy {
    pub fn new(threshold_pct: f64) -> Self {
        Self { threshold_pct }
    }
}

impl TradingStrategy for MomentumStrategy {
    fn analyze(&self, market_data: &[TokenMarketData], _positions: &[Position]) -> Vec<TradeSignal> {
        let mut signals = Vec::new();

        for token in market_data {
            // Look for strong 5-minute momentum
            if token.price_change_5m >= self.threshold_pct {
                let confidence = (token.price_change_5m / self.threshold_pct).min(1.0);

                signals.push(TradeSignal {
                    signal_type: SignalType::Buy,
                    mint: token.mint.clone(),
                    symbol: token.symbol.clone(),
                    confidence,
                    suggested_size_sol: 0.1 * confidence,
                    reason: format!(
                        "Strong momentum: +{:.1}% in 5m (threshold: {:.1}%)",
                        token.price_change_5m, self.threshold_pct
                    ),
                    strategy: "momentum".to_string(),
                    timestamp: Utc::now(),
                });
            }
        }

        signals
    }

    fn name(&self) -> &str {
        "Momentum"
    }

    fn description(&self) -> &str {
        "Buys tokens with strong upward price movement"
    }
}

/// Mean reversion strategy - buy dips, sell rallies
pub struct MeanReversionStrategy {
    deviation_threshold: f64,
}

impl MeanReversionStrategy {
    pub fn new(deviation_threshold: f64) -> Self {
        Self { deviation_threshold }
    }
}

impl TradingStrategy for MeanReversionStrategy {
    fn analyze(&self, market_data: &[TokenMarketData], positions: &[Position]) -> Vec<TradeSignal> {
        let mut signals = Vec::new();

        for token in market_data {
            // Look for oversold conditions (price dropped significantly)
            if token.price_change_1h <= -self.deviation_threshold {
                let confidence = (-token.price_change_1h / self.deviation_threshold).min(1.0);

                signals.push(TradeSignal {
                    signal_type: SignalType::Buy,
                    mint: token.mint.clone(),
                    symbol: token.symbol.clone(),
                    confidence,
                    suggested_size_sol: 0.1 * confidence,
                    reason: format!(
                        "Oversold: {:.1}% drop in 1h (threshold: -{:.1}%)",
                        token.price_change_1h, self.deviation_threshold
                    ),
                    strategy: "mean-revert".to_string(),
                    timestamp: Utc::now(),
                });
            }
        }

        // Check existing positions for sell signals
        for position in positions {
            if let Some(token) = market_data.iter().find(|t| t.mint == position.mint) {
                // Sell if price has rallied significantly from entry
                if position.unrealized_pnl_pct >= self.deviation_threshold * 2.0 {
                    signals.push(TradeSignal {
                        signal_type: SignalType::Sell,
                        mint: position.mint.clone(),
                        symbol: position.symbol.clone(),
                        confidence: 0.8,
                        suggested_size_sol: position.current_value_sol,
                        reason: format!(
                            "Mean reversion target: +{:.1}% gain",
                            position.unrealized_pnl_pct
                        ),
                        strategy: "mean-revert".to_string(),
                        timestamp: Utc::now(),
                    });
                }
            }
        }

        signals
    }

    fn name(&self) -> &str {
        "Mean Reversion"
    }

    fn description(&self) -> &str {
        "Buys dips and sells rallies based on price deviation"
    }
}

/// Trend following strategy
pub struct TrendStrategy {
    trend_strength_threshold: f64,
}

impl TrendStrategy {
    pub fn new(trend_strength_threshold: f64) -> Self {
        Self { trend_strength_threshold }
    }
}

impl TradingStrategy for TrendStrategy {
    fn analyze(&self, market_data: &[TokenMarketData], _positions: &[Position]) -> Vec<TradeSignal> {
        let mut signals = Vec::new();

        for token in market_data {
            // Look for consistent uptrend (all timeframes positive)
            let all_positive = token.price_change_1m > 0.0
                && token.price_change_5m > 0.0
                && token.price_change_1h > 0.0;

            if all_positive && token.price_change_1h >= self.trend_strength_threshold {
                let trend_strength = (token.price_change_1m + token.price_change_5m + token.price_change_1h) / 3.0;
                let confidence = (trend_strength / self.trend_strength_threshold).min(1.0);

                signals.push(TradeSignal {
                    signal_type: SignalType::Buy,
                    mint: token.mint.clone(),
                    symbol: token.symbol.clone(),
                    confidence,
                    suggested_size_sol: 0.1 * confidence,
                    reason: format!(
                        "Strong uptrend: 1m={:+.1}%, 5m={:+.1}%, 1h={:+.1}%",
                        token.price_change_1m, token.price_change_5m, token.price_change_1h
                    ),
                    strategy: "trend".to_string(),
                    timestamp: Utc::now(),
                });
            }
        }

        signals
    }

    fn name(&self) -> &str {
        "Trend Following"
    }

    fn description(&self) -> &str {
        "Follows established price trends across multiple timeframes"
    }
}

/// Sniper strategy - fast entry on new token launches
pub struct SniperStrategy {
    max_age_minutes: u32,
}

impl SniperStrategy {
    pub fn new(max_age_minutes: u32) -> Self {
        Self { max_age_minutes }
    }
}

impl TradingStrategy for SniperStrategy {
    fn analyze(&self, market_data: &[TokenMarketData], _positions: &[Position]) -> Vec<TradeSignal> {
        let mut signals = Vec::new();
        let now = Utc::now();

        for token in market_data {
            let age_minutes = (now - token.created_at).num_minutes();

            if age_minutes <= self.max_age_minutes as i64 {
                // New token - check for early strength
                if token.price_change_1m > 10.0 && token.liquidity_sol >= 5.0 {
                    let freshness = 1.0 - (age_minutes as f64 / self.max_age_minutes as f64);
                    let confidence = freshness * 0.7 + 0.3;

                    signals.push(TradeSignal {
                        signal_type: SignalType::Buy,
                        mint: token.mint.clone(),
                        symbol: token.symbol.clone(),
                        confidence,
                        suggested_size_sol: 0.05, // Small size for sniping
                        reason: format!(
                            "New token ({} min old): +{:.1}% pump, {:.1} SOL liquidity",
                            age_minutes, token.price_change_1m, token.liquidity_sol
                        ),
                        strategy: "sniper".to_string(),
                        timestamp: Utc::now(),
                    });
                }
            }
        }

        signals
    }

    fn name(&self) -> &str {
        "Sniper"
    }

    fn description(&self) -> &str {
        "Fast entry on new token launches with early strength"
    }
}

// ============================================================================
// Risk Manager
// ============================================================================

/// Risk manager with veto power over trades
pub struct RiskManager {
    config: DegenConfig,
    daily_pnl_sol: f64,
    trade_cooldowns: HashMap<String, Instant>,
}

impl RiskManager {
    pub fn new(config: DegenConfig) -> Self {
        Self {
            config,
            daily_pnl_sol: 0.0,
            trade_cooldowns: HashMap::new(),
        }
    }

    /// Check if a trade is allowed
    pub fn check_trade(&mut self, signal: &TradeSignal, positions: &[Position]) -> Result<()> {
        // Check daily loss limit
        if self.daily_pnl_sol <= -self.config.max_daily_loss_sol {
            return Err(anyhow!(
                "Daily loss limit reached ({:.3} SOL)",
                self.config.max_daily_loss_sol
            ));
        }

        // Check max positions
        if signal.signal_type == SignalType::Buy {
            let open_positions = positions.iter().filter(|p| p.status == PositionStatus::Open).count();
            if open_positions >= self.config.max_positions {
                return Err(anyhow!(
                    "Max positions reached ({}/{})",
                    open_positions,
                    self.config.max_positions
                ));
            }

            // Check position size
            if signal.suggested_size_sol > self.config.max_position_sol {
                return Err(anyhow!(
                    "Position size too large ({:.3} > {:.3} SOL)",
                    signal.suggested_size_sol,
                    self.config.max_position_sol
                ));
            }

            // Check cooldown
            if let Some(last_trade) = self.trade_cooldowns.get(&signal.mint) {
                if last_trade.elapsed() < Duration::from_secs(self.config.cooldown_secs) {
                    return Err(anyhow!(
                        "Cooldown active for {} ({} secs remaining)",
                        signal.symbol,
                        self.config.cooldown_secs - last_trade.elapsed().as_secs()
                    ));
                }
            }
        }

        Ok(())
    }

    /// Record a completed trade
    pub fn record_trade(&mut self, trade: &Trade) {
        if trade.side == TradeSide::Sell {
            // Would need the original position to calculate PnL
            // For now just track that we traded this token
        }

        self.trade_cooldowns.insert(trade.mint.clone(), Instant::now());
    }

    /// Update daily PnL
    pub fn update_pnl(&mut self, pnl: f64) {
        self.daily_pnl_sol += pnl;
    }

    /// Check if a position should be stopped out
    pub fn check_stop_loss(&self, position: &Position) -> bool {
        if self.config.trailing_stop {
            if let Some(high) = position.trailing_stop_high {
                let trailing_stop = high * (1.0 - self.config.stop_loss_pct / 100.0);
                return position.current_price_sol <= trailing_stop;
            }
        }

        position.current_price_sol <= position.stop_loss_price
    }

    /// Check if a position should take profit
    pub fn check_take_profit(&self, position: &Position) -> bool {
        position.current_price_sol >= position.take_profit_price
    }
}

// ============================================================================
// Agent State
// ============================================================================

/// Current state of the agent
#[derive(Debug, Clone)]
pub struct AgentState {
    pub is_running: bool,
    pub is_paused: bool,
    pub total_trades: u32,
    pub winning_trades: u32,
    pub losing_trades: u32,
    pub total_pnl_sol: f64,
    pub daily_pnl_sol: f64,
    pub current_balance_sol: f64,
    pub last_signal: Option<TradeSignal>,
    pub last_trade: Option<Trade>,
    pub uptime: Duration,
    pub errors: Vec<String>,
}

impl Default for AgentState {
    fn default() -> Self {
        Self {
            is_running: false,
            is_paused: false,
            total_trades: 0,
            winning_trades: 0,
            losing_trades: 0,
            total_pnl_sol: 0.0,
            daily_pnl_sol: 0.0,
            current_balance_sol: 0.0,
            last_signal: None,
            last_trade: None,
            uptime: Duration::ZERO,
            errors: Vec::new(),
        }
    }
}

// ============================================================================
// Agent Handle
// ============================================================================

/// Handle to control a running agent
pub struct AgentHandle {
    stop_tx: mpsc::Sender<()>,
    state: Arc<RwLock<AgentState>>,
    positions: Arc<RwLock<Vec<Position>>>,
    trades: Arc<RwLock<Vec<Trade>>>,
    signals: Arc<RwLock<Vec<TradeSignal>>>,
}

impl AgentHandle {
    /// Stop the agent
    pub async fn stop(&self) -> Result<()> {
        self.stop_tx.send(()).await.map_err(|_| anyhow!("Failed to send stop signal"))
    }

    /// Get current state
    pub async fn state(&self) -> AgentState {
        self.state.read().await.clone()
    }

    /// Get current positions
    pub async fn positions(&self) -> Vec<Position> {
        self.positions.read().await.clone()
    }

    /// Get trade history
    pub async fn trades(&self) -> Vec<Trade> {
        self.trades.read().await.clone()
    }

    /// Get recent signals
    pub async fn signals(&self) -> Vec<TradeSignal> {
        self.signals.read().await.clone()
    }

    /// Pause trading
    pub async fn pause(&self) {
        self.state.write().await.is_paused = true;
    }

    /// Resume trading
    pub async fn resume(&self) {
        self.state.write().await.is_paused = false;
    }
}

// ============================================================================
// Main Agent
// ============================================================================

/// The autonomous trading agent
pub struct DegenAgent {
    config: DegenConfig,
    keypair: Option<Keypair>,
    rpc: Arc<RpcClient>,
    jupiter: JupiterClient,
    pumpfun: PumpFunClient,
    tokens: Option<TokenRegistry>, // Lazy-loaded for non-pumpfun swaps
    strategy: Box<dyn TradingStrategy>,
    risk_manager: Arc<Mutex<RiskManager>>,
    positions: Arc<RwLock<Vec<Position>>>,
    trades: Arc<RwLock<Vec<Trade>>>,
    signals: Arc<RwLock<Vec<TradeSignal>>>,
    state: Arc<RwLock<AgentState>>,
}

impl DegenAgent {
    /// Create a new degen agent
    pub async fn new(config: DegenConfig) -> Result<Self> {
        // Load RPC URL
        let rpc_url = std::env::var("SOLANA_RPC_URL")
            .unwrap_or_else(|_| "https://api.mainnet-beta.solana.com".to_string());

        // Load wallet if not dry run
        let keypair = if !config.dry_run {
            Self::load_keypair()
        } else {
            None
        };

        // Create strategy
        let strategy: Box<dyn TradingStrategy> = match config.strategy.as_str() {
            "momentum" => Box::new(MomentumStrategy::new(config.momentum_threshold_pct)),
            "mean-revert" => Box::new(MeanReversionStrategy::new(10.0)),
            "trend" => Box::new(TrendStrategy::new(5.0)),
            "sniper" => Box::new(SniperStrategy::new(10)),
            _ => Box::new(MomentumStrategy::new(config.momentum_threshold_pct)),
        };

        // Create pump.fun client for market data (primary data source)
        let pumpfun = PumpFunClient::new(PumpFunConfig::default());

        // TokenRegistry is lazy-loaded only when needed for non-pumpfun swaps
        // This avoids blocking startup with Jupiter API calls
        let tokens = None;

        Ok(Self {
            risk_manager: Arc::new(Mutex::new(RiskManager::new(config.clone()))),
            config,
            keypair,
            rpc: Arc::new(RpcClient::new(rpc_url)),
            jupiter: JupiterClient::new(),
            pumpfun,
            tokens,
            strategy,
            positions: Arc::new(RwLock::new(Vec::new())),
            trades: Arc::new(RwLock::new(Vec::new())),
            signals: Arc::new(RwLock::new(Vec::new())),
            state: Arc::new(RwLock::new(AgentState::default())),
        })
    }

    /// Load keypair from file
    fn load_keypair() -> Option<Keypair> {
        let keypair_path = std::env::var("SOLANA_KEYPAIR")
            .ok()
            .or_else(|| {
                dirs::home_dir().map(|h| {
                    h.join(".config/solana/id.json")
                        .to_string_lossy()
                        .to_string()
                })
            })?;

        let keypair_bytes = std::fs::read_to_string(&keypair_path).ok()?;
        let keypair_vec: Vec<u8> = serde_json::from_str(&keypair_bytes).ok()?;

        if keypair_vec.len() != 64 {
            return None;
        }

        let mut secret_key = [0u8; 32];
        secret_key.copy_from_slice(&keypair_vec[..32]);

        Some(Keypair::new_from_array(secret_key))
    }

    /// Start the agent
    pub async fn start(self) -> Result<AgentHandle> {
        let (stop_tx, mut stop_rx) = mpsc::channel::<()>(1);
        let state = self.state.clone();
        let positions = self.positions.clone();
        let trades = self.trades.clone();
        let signals = self.signals.clone();

        // Mark as running
        {
            let mut state = state.write().await;
            state.is_running = true;
        }

        let agent = Arc::new(self);

        // Spawn the main loop
        let agent_clone = agent.clone();
        let state_clone = state.clone();
        tokio::spawn(async move {
            let start_time = Instant::now();

            loop {
                tokio::select! {
                    _ = stop_rx.recv() => {
                        break;
                    }
                    _ = tokio::time::sleep(Duration::from_secs(5)) => {
                        // Update uptime
                        {
                            let mut state = state_clone.write().await;
                            state.uptime = start_time.elapsed();
                        }

                        // Run trading cycle if not paused
                        let is_paused = state_clone.read().await.is_paused;
                        if !is_paused {
                            if let Err(e) = agent_clone.trading_cycle().await {
                                let mut state = state_clone.write().await;
                                state.errors.push(format!("{}: {}", Utc::now(), e));
                                if state.errors.len() > 100 {
                                    state.errors.remove(0);
                                }
                            }
                        }
                    }
                }
            }

            // Mark as stopped
            {
                let mut state = state_clone.write().await;
                state.is_running = false;
            }
        });

        Ok(AgentHandle { stop_tx, state, positions, trades, signals })
    }

    /// Execute one trading cycle
    async fn trading_cycle(&self) -> Result<()> {
        // Fetch market data
        let market_data = self.fetch_market_data().await?;

        // Update existing positions
        self.update_positions(&market_data).await?;

        // Check stop losses and take profits
        self.check_exits().await?;

        // Generate signals
        let positions = self.positions.read().await;
        let new_signals = self.strategy.analyze(&market_data, &positions);
        drop(positions);

        // Store signals for TUI display (keep last 50)
        {
            let mut signals = self.signals.write().await;
            signals.extend(new_signals.clone());
            if signals.len() > 50 {
                let excess = signals.len() - 50;
                signals.drain(0..excess);
            }
        }

        // Process signals
        for signal in new_signals {
            if let Err(e) = self.process_signal(signal).await {
                log::debug!("Signal rejected: {}", e);
            }
        }

        Ok(())
    }

    /// Fetch market data from PumpFunClient
    async fn fetch_market_data(&self) -> Result<Vec<TokenMarketData>> {
        // Fetch trending tokens from OpenSVM API via PumpFunClient
        match self.pumpfun.get_trending_tokens().await {
            Ok(tokens) => {
                // Convert PumpToken to TokenMarketData
                let market_data: Vec<TokenMarketData> = tokens
                    .into_iter()
                    .map(|t| TokenMarketData {
                        mint: t.mint,
                        symbol: t.symbol,
                        name: t.name,
                        price_sol: t.price_sol,
                        price_usd: t.price_usd,
                        market_cap_sol: t.market_cap_sol,
                        liquidity_sol: t.liquidity_sol,
                        volume_24h_sol: t.volume_24h_sol,
                        holders: t.holder_count,
                        price_change_1m: t.price_change_1m,
                        price_change_5m: t.price_change_5m,
                        price_change_1h: t.price_change_1h,
                        price_change_24h: t.price_change_24h,
                        created_at: t.created_at,
                        last_updated: Utc::now(),
                    })
                    .collect();

                log::debug!("Fetched {} tokens from market data", market_data.len());
                Ok(market_data)
            }
            Err(e) => {
                log::warn!("Failed to fetch market data: {}", e);
                Ok(Vec::new()) // Return empty on error to avoid breaking the trading cycle
            }
        }
    }

    /// Update position prices
    async fn update_positions(&self, market_data: &[TokenMarketData]) -> Result<()> {
        let mut positions = self.positions.write().await;

        for position in positions.iter_mut() {
            if position.status != PositionStatus::Open {
                continue;
            }

            if let Some(data) = market_data.iter().find(|t| t.mint == position.mint) {
                position.current_price_sol = data.price_sol;
                position.current_value_sol = position.amount * data.price_sol;
                position.unrealized_pnl_sol = position.current_value_sol - position.cost_basis_sol;
                // Guard against division by zero
                position.unrealized_pnl_pct = if position.cost_basis_sol > 0.0 {
                    (position.unrealized_pnl_sol / position.cost_basis_sol) * 100.0
                } else {
                    0.0
                };

                // Update trailing stop
                if self.config.trailing_stop {
                    if let Some(ref mut high) = position.trailing_stop_high {
                        if position.current_price_sol > *high {
                            *high = position.current_price_sol;
                        }
                    } else if position.current_price_sol > position.entry_price_sol {
                        position.trailing_stop_high = Some(position.current_price_sol);
                    }
                }
            }
        }

        Ok(())
    }

    /// Check for stop loss and take profit exits
    async fn check_exits(&self) -> Result<()> {
        let positions = self.positions.read().await.clone();
        let risk_manager = self.risk_manager.lock().await;

        // Collect all positions that need to exit
        let mut exits: Vec<(String, &str)> = Vec::new();

        for position in positions.iter() {
            if position.status != PositionStatus::Open {
                continue;
            }

            if risk_manager.check_stop_loss(position) {
                exits.push((position.id.clone(), "stop_loss"));
            } else if risk_manager.check_take_profit(position) {
                exits.push((position.id.clone(), "take_profit"));
            }
        }

        // Drop lock before closing positions
        drop(risk_manager);

        // Close all positions that need to exit
        for (position_id, reason) in exits {
            if let Err(e) = self.close_position(&position_id, reason).await {
                log::warn!("Failed to close position {}: {}", position_id, e);
            }
        }

        Ok(())
    }

    /// Process a trading signal
    async fn process_signal(&self, signal: TradeSignal) -> Result<()> {
        // Check with risk manager
        let positions = self.positions.read().await;
        let mut risk_manager = self.risk_manager.lock().await;
        risk_manager.check_trade(&signal, &positions)?;
        drop(positions);
        drop(risk_manager);

        // Update state with signal
        {
            let mut state = self.state.write().await;
            state.last_signal = Some(signal.clone());
        }

        match signal.signal_type {
            SignalType::Buy => self.execute_buy(signal).await,
            SignalType::Sell => self.execute_sell(signal).await,
            SignalType::Hold => Ok(()),
        }
    }

    /// Execute a buy order
    async fn execute_buy(&self, signal: TradeSignal) -> Result<()> {
        if self.config.dry_run {
            // Paper trade
            self.paper_buy(signal).await
        } else {
            // Real trade via Jupiter
            self.real_buy(signal).await
        }
    }

    /// Execute a sell order
    async fn execute_sell(&self, signal: TradeSignal) -> Result<()> {
        if self.config.dry_run {
            self.paper_sell(signal).await
        } else {
            self.real_sell(signal).await
        }
    }

    /// Paper trade buy
    async fn paper_buy(&self, signal: TradeSignal) -> Result<()> {
        // Fetch current price from API
        let entry_price_sol = match self.pumpfun.get_token_info_opensvm(&signal.mint).await {
            Ok(token) => token.price_sol.max(0.0000001), // Prevent division by zero
            Err(_) => {
                // Fallback: calculate from signal if price lookup fails
                log::warn!("Could not fetch price for {}, using estimate", signal.symbol);
                0.0001
            }
        };

        let amount = signal.suggested_size_sol / entry_price_sol;

        let position = Position {
            id: uuid::Uuid::new_v4().to_string(),
            mint: signal.mint.clone(),
            symbol: signal.symbol.clone(),
            entry_price_sol,
            current_price_sol: entry_price_sol,
            amount,
            cost_basis_sol: signal.suggested_size_sol,
            current_value_sol: signal.suggested_size_sol,
            unrealized_pnl_sol: 0.0,
            unrealized_pnl_pct: 0.0,
            entry_time: Utc::now(),
            stop_loss_price: entry_price_sol * (1.0 - self.config.stop_loss_pct / 100.0),
            take_profit_price: entry_price_sol * (1.0 + self.config.take_profit_pct / 100.0),
            trailing_stop_high: None,
            status: PositionStatus::Open,
        };

        let trade = Trade {
            id: uuid::Uuid::new_v4().to_string(),
            position_id: position.id.clone(),
            side: TradeSide::Buy,
            mint: signal.mint,
            symbol: signal.symbol,
            amount: position.amount,
            price_sol: position.entry_price_sol,
            value_sol: signal.suggested_size_sol,
            signature: None,
            timestamp: Utc::now(),
            strategy: signal.strategy,
            reason: signal.reason,
        };

        // Record
        self.positions.write().await.push(position);
        self.trades.write().await.push(trade.clone());
        self.risk_manager.lock().await.record_trade(&trade);

        // Update state
        {
            let mut state = self.state.write().await;
            state.total_trades += 1;
            state.last_trade = Some(trade);
        }

        Ok(())
    }

    /// Paper trade sell
    async fn paper_sell(&self, signal: TradeSignal) -> Result<()> {
        // Find position
        let mut positions = self.positions.write().await;
        if let Some(position) = positions.iter_mut().find(|p| p.mint == signal.mint && p.status == PositionStatus::Open) {
            let pnl = position.current_value_sol - position.cost_basis_sol;

            let trade = Trade {
                id: uuid::Uuid::new_v4().to_string(),
                position_id: position.id.clone(),
                side: TradeSide::Sell,
                mint: signal.mint,
                symbol: signal.symbol,
                amount: position.amount,
                price_sol: position.current_price_sol,
                value_sol: position.current_value_sol,
                signature: None,
                timestamp: Utc::now(),
                strategy: signal.strategy,
                reason: signal.reason,
            };

            // Update position status
            position.status = if pnl >= 0.0 {
                PositionStatus::ClosedProfit
            } else {
                PositionStatus::ClosedLoss
            };

            // Record
            self.trades.write().await.push(trade.clone());
            self.risk_manager.lock().await.record_trade(&trade);
            self.risk_manager.lock().await.update_pnl(pnl);

            // Update state
            {
                let mut state = self.state.write().await;
                state.total_trades += 1;
                state.total_pnl_sol += pnl;
                state.daily_pnl_sol += pnl;
                if pnl >= 0.0 {
                    state.winning_trades += 1;
                } else {
                    state.losing_trades += 1;
                }
                state.last_trade = Some(trade);
            }
        }

        Ok(())
    }

    /// Real buy via Jupiter
    async fn real_buy(&self, signal: TradeSignal) -> Result<()> {
        let keypair = self.keypair.as_ref().ok_or_else(|| anyhow!("No keypair loaded"))?;

        // Get quote from Jupiter
        let quote = self.jupiter.get_quote(&QuoteParams {
            input_mint: "So11111111111111111111111111111111111111112".to_string(), // SOL
            output_mint: signal.mint.clone(),
            amount: (signal.suggested_size_sol * 1_000_000_000.0) as u64, // Convert to lamports
            slippage_bps: self.config.slippage_bps,
        }).await?;

        // Get swap transaction
        let swap_tx = self.jupiter.get_swap_transaction(&quote, &keypair.pubkey().to_string()).await?;

        // TODO: Implement full transaction signing and submission
        // The swap_transaction is base64-encoded and needs to be decoded, signed, and sent
        // This requires proper blockhash handling and transaction building

        // For now, just log that we would execute
        log::info!(
            "Would execute real buy for {} ({} SOL) - tx_len: {} bytes",
            signal.symbol,
            signal.suggested_size_sol,
            swap_tx.swap_transaction.len()
        );

        Ok(())
    }

    /// Real sell via Jupiter
    async fn real_sell(&self, signal: TradeSignal) -> Result<()> {
        // Similar to real_buy but reversed
        log::info!("Would execute real sell for {}", signal.symbol);
        Ok(())
    }

    /// Close a position
    async fn close_position(&self, position_id: &str, reason: &str) -> Result<()> {
        let positions = self.positions.read().await;
        if let Some(position) = positions.iter().find(|p| p.id == position_id) {
            let signal = TradeSignal {
                signal_type: SignalType::Sell,
                mint: position.mint.clone(),
                symbol: position.symbol.clone(),
                confidence: 1.0,
                suggested_size_sol: position.current_value_sol,
                reason: reason.to_string(),
                strategy: "risk_manager".to_string(),
                timestamp: Utc::now(),
            };

            drop(positions);
            self.execute_sell(signal).await
        } else {
            Err(anyhow!("Position not found"))
        }
    }

    /// Get current state
    pub async fn state(&self) -> AgentState {
        self.state.read().await.clone()
    }

    /// Get all positions
    pub async fn positions(&self) -> Vec<Position> {
        self.positions.read().await.clone()
    }

    /// Get all trades
    pub async fn trades(&self) -> Vec<Trade> {
        self.trades.read().await.clone()
    }
}
