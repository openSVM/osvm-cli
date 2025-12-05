# OSVM Perp TUI - Steering Document

**Command:** `osvm perp`
**Purpose:** Terminal UI for trading on-chain perpetual futures on Solana
**Priority:** High - Advanced DeFi functionality

---

## Executive Summary

The Perp TUI provides a professional-grade terminal trading interface for Solana's on-chain perpetual futures protocols. Designed for serious traders who need fast execution, real-time data, and keyboard-driven workflows without leaving the terminal.

---

## Supported Protocols

### Tier 1 (Launch Priority)

| Protocol | Features | Status |
|----------|----------|--------|
| **Drift Protocol** | CLOB + vAMM hybrid, cross-margin, spot | Largest Solana perps |
| **Jupiter Perps** | LP-to-trader model, simple UX | Growing rapidly |
| **Flash Trade** | Multi-collateral, isolated margin | Active development |

### Tier 2 (Post-Launch)

| Protocol | Features | Status |
|----------|----------|--------|
| **Zeta Markets** | Options + perps, portfolio margin | Established |
| **Mango Markets** | Cross-margin, governance | V4 active |
| **Parcl** | Real estate perps | Niche |

---

## User Stories

### Primary Users
1. **Active Traders** - Need fast order entry and position management
2. **Scalpers** - Sub-second execution requirements
3. **Swing Traders** - Multi-day position management
4. **Market Makers** - Two-sided liquidity provision
5. **Risk Managers** - Portfolio monitoring and hedging

### Key User Stories

| ID | Story | Priority |
|----|-------|----------|
| PERP-1 | As a trader, I want to open long/short positions with one keystroke | P0 |
| PERP-2 | As a trader, I want to see real-time P&L for all positions | P0 |
| PERP-3 | As a trader, I want to set stop-loss and take-profit orders | P0 |
| PERP-4 | As a trader, I want to see the order book depth | P0 |
| PERP-5 | As a trader, I want to see funding rates before opening positions | P0 |
| PERP-6 | As a trader, I want to manage collateral and margin | P0 |
| PERP-7 | As a trader, I want to see liquidation prices clearly | P0 |
| PERP-8 | As a scalper, I want hotkeys for quick market orders | P1 |
| PERP-9 | As a swing trader, I want price alerts on positions | P1 |
| PERP-10 | As a risk manager, I want portfolio-level risk metrics | P1 |
| PERP-11 | As a trader, I want to view historical trades and P&L | P2 |
| PERP-12 | As a market maker, I want to place grid orders | P2 |

---

## Architecture

### Component Hierarchy

```
PerpApp
├── HeaderBar
│   ├── AccountInfo
│   │   ├── AccountValue
│   │   ├── AvailableMargin
│   │   ├── UnrealizedPnL
│   │   └── LeverageUsed
│   ├── MarketSelector
│   ├── ProtocolSelector (Drift/Jupiter/Flash)
│   └── ConnectionStatus
├── LeftPanel (Market Data)
│   ├── OrderBook
│   │   ├── AskLevels
│   │   ├── SpreadIndicator
│   │   └── BidLevels
│   ├── RecentTrades
│   └── MarketStats
│       ├── MarkPrice
│       ├── IndexPrice
│       ├── FundingRate
│       ├── OpenInterest
│       └── Volume24h
├── CenterPanel (Chart + Orders)
│   ├── PriceChart
│   │   ├── CandleChart
│   │   ├── PositionLines
│   │   └── OrderLines
│   ├── OrderEntry
│   │   ├── TypeSelector (Market/Limit/Stop)
│   │   ├── SideButtons (Long/Short)
│   │   ├── SizeInput
│   │   ├── PriceInput (for limit)
│   │   ├── LeverageSlider
│   │   ├── StopLoss/TakeProfit
│   │   └── Preview
│   └── OpenOrders
│       ├── OrderList
│       └── CancelControls
├── RightPanel (Positions)
│   ├── PositionsList
│   │   ├── PositionCard (per position)
│   │   │   ├── MarketSymbol
│   │   │   ├── Side/Size
│   │   │   ├── EntryPrice
│   │   │   ├── MarkPrice
│   │   │   ├── UnrealizedPnL
│   │   │   ├── LiquidationPrice
│   │   │   └── QuickActions
│   │   └── TotalPnL
│   └── CollateralPanel
│       ├── Deposits
│       ├── Borrows
│       └── HealthRatio
├── BottomPanel
│   ├── TradeHistory
│   ├── FundingHistory
│   └── AlertsPanel
└── StatusBar
    ├── LatencyIndicator
    ├── RPC Status
    ├── WebSocket Status
    └── PendingOrders
```

### Real-Time Data Flow

```
                          ┌─────────────────┐
                          │   Price Oracle  │
                          │   (Pyth/Switch) │
                          └────────┬────────┘
                                   │
┌─────────────┐           ┌────────▼────────┐           ┌─────────────┐
│  WebSocket  │──────────▶│    PerpApp      │◀──────────│    RPC      │
│  (orderbook,│           │    State        │           │  (accounts, │
│   trades)   │           └────────┬────────┘           │   txs)      │
└─────────────┘                    │                    └─────────────┘
                                   │
                    ┌──────────────┼──────────────┐
                    │              │              │
               ┌────▼────┐   ┌────▼────┐   ┌────▼────┐
               │ Order   │   │Position │   │ Market  │
               │ Book    │   │ Manager │   │ Stats   │
               │ View    │   │ View    │   │ View    │
               └─────────┘   └─────────┘   └─────────┘
```

---

## State Management

### Core State Structure

```rust
pub struct PerpState {
    // Protocol selection
    pub active_protocol: PerpProtocol,
    pub protocols: HashMap<PerpProtocol, Arc<dyn PerpClient>>,

    // Market selection
    pub active_market: MarketId,
    pub markets: Vec<MarketInfo>,

    // Account state
    pub account: Option<PerpAccount>,
    pub positions: Vec<Position>,
    pub open_orders: Vec<Order>,
    pub collateral: CollateralState,

    // Market data (real-time)
    pub order_book: OrderBook,
    pub recent_trades: VecDeque<Trade>,
    pub market_stats: MarketStats,
    pub price_history: Vec<Candle>,

    // Order entry form
    pub order_form: OrderForm,

    // UI state
    pub focus: PerpFocus,
    pub alerts: Vec<Alert>,
    pub trade_history: Vec<TradeRecord>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum PerpProtocol {
    Drift,
    JupiterPerps,
    FlashTrade,
    ZetaMarkets,
    MangoMarkets,
}

#[derive(Clone)]
pub struct Position {
    pub market: MarketId,
    pub side: PositionSide,
    pub size: f64,              // Base asset amount
    pub notional: f64,         // USD value
    pub entry_price: f64,
    pub mark_price: f64,
    pub unrealized_pnl: f64,
    pub unrealized_pnl_pct: f64,
    pub liquidation_price: Option<f64>,
    pub margin_used: f64,
    pub leverage: f64,
    // Protocol-specific
    pub protocol: PerpProtocol,
    pub position_id: String,
}

#[derive(Clone, Copy)]
pub enum PositionSide {
    Long,
    Short,
}

#[derive(Clone)]
pub struct OrderBook {
    pub bids: Vec<PriceLevel>,
    pub asks: Vec<PriceLevel>,
    pub last_update: Instant,
    pub spread: f64,
    pub spread_bps: f64,
}

#[derive(Clone)]
pub struct PriceLevel {
    pub price: f64,
    pub size: f64,
    pub total: f64,  // Cumulative size
    pub orders: u32, // Number of orders
}

#[derive(Clone)]
pub struct MarketStats {
    pub mark_price: f64,
    pub index_price: f64,
    pub funding_rate: f64,         // Current rate
    pub funding_rate_annual: f64,  // Annualized
    pub next_funding_time: DateTime<Utc>,
    pub open_interest: f64,
    pub volume_24h: f64,
    pub high_24h: f64,
    pub low_24h: f64,
    pub change_24h_pct: f64,
}

pub struct OrderForm {
    pub order_type: OrderType,
    pub side: PositionSide,
    pub size: String,
    pub size_usd: Option<f64>,
    pub price: String,           // For limit orders
    pub leverage: f64,
    pub reduce_only: bool,
    pub post_only: bool,         // For limit orders
    pub stop_loss: Option<String>,
    pub take_profit: Option<String>,
    // Preview
    pub preview: Option<OrderPreview>,
}

#[derive(Clone, Copy)]
pub enum OrderType {
    Market,
    Limit,
    StopMarket,
    StopLimit,
    TakeProfit,
    TrailingStop,
}

pub struct OrderPreview {
    pub entry_price: f64,
    pub size_base: f64,
    pub size_usd: f64,
    pub margin_required: f64,
    pub liquidation_price: f64,
    pub fees: f64,
    pub slippage_estimate: f64,
}
```

---

## Protocol Integrations

### Drift Protocol

```rust
pub struct DriftClient {
    rpc: Arc<RpcClient>,
    ws: Option<WebSocketClient>,
    program_id: Pubkey,
    user_account: Option<Pubkey>,
}

impl DriftClient {
    const PROGRAM_ID: &'static str = "dRiftyHA39MWEi3m9aunc5MzRF1JYuBsbn6VPcn33UH";

    // Account queries
    pub async fn get_user_account(&self, authority: &Pubkey) -> Result<UserAccount>;
    pub async fn get_perp_positions(&self, user: &Pubkey) -> Result<Vec<PerpPosition>>;
    pub async fn get_open_orders(&self, user: &Pubkey) -> Result<Vec<Order>>;

    // Market data
    pub async fn get_perp_markets(&self) -> Result<Vec<PerpMarket>>;
    pub async fn subscribe_orderbook(&mut self, market: u16) -> Result<OrderBookStream>;
    pub async fn subscribe_trades(&mut self, market: u16) -> Result<TradeStream>;

    // Order execution
    pub fn build_place_perp_order(&self, params: PlaceOrderParams) -> Result<Transaction>;
    pub fn build_cancel_order(&self, order_id: u32) -> Result<Transaction>;
    pub fn build_close_position(&self, market: u16) -> Result<Transaction>;

    // Collateral management
    pub fn build_deposit(&self, amount: u64, market_index: u16) -> Result<Transaction>;
    pub fn build_withdraw(&self, amount: u64, market_index: u16) -> Result<Transaction>;
}

#[derive(Clone)]
pub struct PlaceOrderParams {
    pub market_index: u16,
    pub direction: PositionDirection,
    pub base_asset_amount: u64,
    pub price: Option<u64>,
    pub order_type: DriftOrderType,
    pub reduce_only: bool,
    pub post_only: bool,
    pub trigger_price: Option<u64>,
    pub trigger_condition: Option<TriggerCondition>,
}
```

### Jupiter Perps

```rust
pub struct JupiterPerpsClient {
    rpc: Arc<RpcClient>,
    api_base: String,
}

impl JupiterPerpsClient {
    const API_BASE: &'static str = "https://perps-api.jup.ag";

    pub async fn get_positions(&self, owner: &Pubkey) -> Result<Vec<JupPosition>>;
    pub async fn get_markets(&self) -> Result<Vec<JupMarket>>;

    pub fn build_open_position(&self, params: OpenPositionParams) -> Result<Transaction>;
    pub fn build_close_position(&self, position: &Pubkey) -> Result<Transaction>;
    pub fn build_add_collateral(&self, position: &Pubkey, amount: u64) -> Result<Transaction>;
}
```

### Flash Trade

```rust
pub struct FlashTradeClient {
    rpc: Arc<RpcClient>,
}

impl FlashTradeClient {
    pub async fn get_positions(&self, owner: &Pubkey) -> Result<Vec<FlashPosition>>;
    pub async fn get_markets(&self) -> Result<Vec<FlashMarket>>;

    pub fn build_open_position(&self, params: OpenPositionParams) -> Result<Transaction>;
    pub fn build_close_position(&self, position: &Pubkey) -> Result<Transaction>;
}
```

---

## Keyboard Shortcuts

### Global

| Key | Action |
|-----|--------|
| `q` / `Esc` | Quit / Cancel |
| `Tab` | Next panel |
| `Shift+Tab` | Previous panel |
| `m` | Market selector |
| `p` | Protocol selector |
| `r` | Refresh all |
| `?` | Help overlay |

### Order Entry (when focused)

| Key | Action |
|-----|--------|
| `l` | Long (buy) |
| `s` | Short (sell) |
| `1-9` | Quick size presets |
| `t` | Toggle order type |
| `Enter` | Submit order |
| `Tab` | Next field |

### Quick Trading (Global Hotkeys)

| Key | Action |
|-----|--------|
| `Shift+L` | Quick long (market) |
| `Shift+S` | Quick short (market) |
| `Shift+C` | Close all positions |
| `Shift+X` | Cancel all orders |
| `Shift+F` | Flatten (close + cancel) |

### Position Management

| Key | Action |
|-----|--------|
| `↑/↓` | Navigate positions |
| `Enter` | Position details |
| `c` | Close selected position |
| `+/-` | Add/reduce position |
| `t` | Set TP/SL |

### Order Book

| Key | Action |
|-----|--------|
| `↑/↓` | Navigate price levels |
| `Enter` | Place limit at price |
| `d` | Toggle depth view |
| `g` | Group price levels |

---

## UI Mockups

### Main Trading View

```
┌─ OSVM Perp ─────────────────────────────────────────────────────────────────┐
│ Account: $12,456.78 │ Margin: $8,234.12 (66%) │ uPnL: +$234.56 │ Drift     │
├───────────────────────────────────────────────────────────────────────┬─────┤
│ SOL-PERP │ $234.56 │ +2.34% │ Funding: +0.0012% (1h) │ OI: $45.2M    │ [m] │
├─────────────────┬───────────────────────────────────┬───────────────────────┤
│   ORDER BOOK    │         PRICE CHART               │      POSITIONS        │
├─────────────────┼───────────────────────────────────┼───────────────────────┤
│                 │                                   │                       │
│ Price     Size  │  240│                     ╭──     │ SOL-PERP LONG 2.5x   │
│ ───────────────│     │                ╭───╯        │ Size: +5.0 SOL        │
│ 235.50   12.3  │     │           ╭───╯             │ Entry: 230.00         │
│ 235.25   45.6  │     │      ╭───╯                  │ Mark: 234.56          │
│ 235.00   89.2  │  235│ ╭───╯                       │ PnL: +$22.80 (+1.98%) │
│ 234.80  123.4  │     │╯                            │ Liq: 185.00           │
│ ═══════════════│     │                             │ ─────────────────────│
│ Spread: 0.02%  │  230│──────────────────────────── │                       │
│ ═══════════════│     │  [1h] [4h] [1d] [1w]        │ ETH-PERP SHORT 3.0x  │
│ 234.55  156.7  │     │                             │ Size: -0.5 ETH        │
│ 234.30   78.9  │  225│        Entry ──────         │ Entry: 2450.00        │
│ 234.00   34.5  │     │        TP ─ ─ ─ ─ ─         │ Mark: 2380.00         │
│ 233.75   12.1  │     │        SL ─ ─ ─ ─ ─         │ PnL: +$35.00 (+2.86%) │
│ 233.50    8.9  │  220│                             │ Liq: 2650.00          │
│                 │                                   │                       │
├─────────────────┼───────────────────────────────────┼───────────────────────┤
│ Recent Trades   │         ORDER ENTRY               │ Total uPnL: +$57.80  │
│ ───────────────│                                   │ ─────────────────────│
│ 234.56  1.2  ▲ │ ┌─────────────┬─────────────────┐ │                       │
│ 234.54  0.5  ▼ │ │  [L] LONG   │  [S] SHORT      │ │ Collateral            │
│ 234.56  2.3  ▲ │ └─────────────┴─────────────────┘ │ USDC: $10,000.00     │
│ 234.52  0.8  ▼ │                                   │ SOL:  5.0 ($1,172)   │
│ 234.55  1.1  ▲ │ Type: [Market] Limit  Stop       │ ─────────────────────│
│ 234.50  3.4  ▼ │                                   │ Total: $11,172.78    │
│ 234.53  0.9  ▲ │ Size: [____1.0____] SOL          │ Health: 85% ████████░│
│                 │       ≈ $234.56                   │                       │
│                 │ Leverage: [═══════●══] 5.0x      │                       │
│                 │                                   │                       │
│                 │ Est. Entry: 234.56               │                       │
│                 │ Liq. Price: 187.65               │                       │
│                 │ Margin Req: $46.91               │                       │
│                 │                                   │                       │
│                 │ [  TP: _______ ] [ SL: _______ ] │                       │
│                 │                                   │                       │
│                 │ ┌─────────────────────────────┐  │                       │
│                 │ │      [Enter] OPEN LONG      │  │                       │
│                 │ └─────────────────────────────┘  │                       │
├─────────────────┴───────────────────────────────────┴───────────────────────┤
│ Open Orders (2)                                                             │
│ ──────────────────────────────────────────────────────────────────────────│
│ SOL-PERP  LIMIT LONG   1.0 SOL @ 230.00  │ Filled: 0%  │ [c] Cancel        │
│ ETH-PERP  STOP SHORT   0.2 ETH @ 2500.00 │ Triggered   │ [c] Cancel        │
├─────────────────────────────────────────────────────────────────────────────┤
│ [l/s] Long/Short │ [c] Close │ [t] TP/SL │ [Shift+F] Flatten │ [?] Help    │
├─────────────────────────────────────────────────────────────────────────────┤
│ Latency: 45ms │ WS: ● │ RPC: ● │ 2 pending │ Last: 234.56 @ 14:32:45      │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Position Details Modal

```
┌─ Position Details ──────────────────────────────────────────────────────────┐
│                                                                             │
│  SOL-PERP │ LONG │ Drift Protocol                                          │
│                                                                             │
│  ┌─ Position Info ────────────────────────────────────────────────────────┐ │
│  │                                                                        │ │
│  │  Size               +5.0 SOL ($1,172.80)                              │ │
│  │  Entry Price        $230.00                                            │ │
│  │  Mark Price         $234.56                                            │ │
│  │  Leverage           2.5x                                               │ │
│  │  Margin Used        $468.00                                            │ │
│  │                                                                        │ │
│  │  ─────────────────────────────────────────                             │ │
│  │                                                                        │ │
│  │  Unrealized PnL     +$22.80 (+1.98%)                                  │ │
│  │  Realized PnL       +$45.00 (from partial closes)                     │ │
│  │  Funding Paid       -$3.45                                             │ │
│  │  ─────────────────────────────────────────                             │ │
│  │  Net PnL            +$64.35 (+2.79%)                                  │ │
│  │                                                                        │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  ┌─ Liquidation Analysis ─────────────────────────────────────────────────┐ │
│  │                                                                        │ │
│  │  Liquidation Price: $185.00 (-21.1% from current)                     │ │
│  │                                                                        │ │
│  │  185 ░░░░░░░░░░░░░░░░░░░░░░░████████████████████████ 234.56           │ │
│  │      Liq                    ▲Entry              Current               │ │
│  │                                                                        │ │
│  │  Buffer to Liquidation: $49.56 (21.1%)                                │ │
│  │  Maintenance Margin: $23.40 (5% of position)                          │ │
│  │                                                                        │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  ┌─ Risk Controls ────────────────────────────────────────────────────────┐ │
│  │                                                                        │ │
│  │  Stop Loss:     Not Set   [ Set SL ]                                  │ │
│  │  Take Profit:   Not Set   [ Set TP ]                                  │ │
│  │  Trailing Stop: Not Set   [ Set TS ]                                  │ │
│  │                                                                        │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  ┌─ Quick Actions ────────────────────────────────────────────────────────┐ │
│  │                                                                        │ │
│  │  [ Close 100% ]  [ Close 50% ]  [ Close 25% ]  [ Add Margin ]         │ │
│  │                                                                        │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│ [1-3] Close % │ [t] Set TP/SL │ [+] Add Margin │ [Esc] Close              │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Market Selector

```
┌─ Select Market ─────────────────────────────────────────────────────────────┐
│ Search: _______________                                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│ Market          Price         24h%      Funding    OI           Volume     │
│ ─────────────────────────────────────────────────────────────────────────── │
│ ▶ SOL-PERP     $234.56       +2.34%    +0.0012%   $45.2M       $234.5M    │
│   BTC-PERP     $67,890.00    +1.23%    +0.0008%   $890.5M      $1.2B      │
│   ETH-PERP     $2,380.00     -0.45%    -0.0003%   $234.5M      $567.8M    │
│   JUP-PERP     $1.23         +5.67%    +0.0045%   $12.3M       $45.6M     │
│   WIF-PERP     $2.45         +12.34%   +0.0089%   $8.9M        $34.5M     │
│   BONK-PERP    $0.000023     -3.21%    -0.0012%   $5.6M        $23.4M     │
│   RENDER-PERP  $8.90         +4.56%    +0.0034%   $15.6M       $56.7M     │
│   PYTH-PERP    $0.45         +2.12%    +0.0021%   $7.8M        $12.3M     │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│ [↑↓] Navigate │ [Enter] Select │ [/] Search │ [Esc] Cancel                 │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Implementation Plan

### Phase 1: Core Infrastructure (Week 1-2)

| Task | Description | Files |
|------|-------------|-------|
| 1.1 | PerpClient trait definition | `src/utils/tui/perp/protocol.rs` |
| 1.2 | Drift Protocol client | `src/utils/tui/perp/providers/drift.rs` |
| 1.3 | Core state management | `src/utils/tui/perp/app.rs` |
| 1.4 | Basic position display | `src/utils/tui/perp/views/positions.rs` |
| 1.5 | Market data models | `src/utils/tui/perp/models.rs` |

### Phase 2: Real-Time Data (Week 3-4)

| Task | Description | Files |
|------|-------------|-------|
| 2.1 | WebSocket integration | `src/utils/tui/perp/ws.rs` |
| 2.2 | Order book rendering | `src/utils/tui/perp/views/orderbook.rs` |
| 2.3 | Recent trades feed | `src/utils/tui/perp/views/trades.rs` |
| 2.4 | Price chart (ASCII) | `src/utils/tui/perp/views/chart.rs` |
| 2.5 | Market stats panel | `src/utils/tui/perp/views/stats.rs` |

### Phase 3: Order Execution (Week 5-6)

| Task | Description | Files |
|------|-------------|-------|
| 3.1 | Order entry form | `src/utils/tui/perp/views/order_entry.rs` |
| 3.2 | Order validation | `src/utils/tui/perp/validation.rs` |
| 3.3 | Transaction building | `src/utils/tui/perp/tx/` |
| 3.4 | Order management | `src/utils/tui/perp/views/orders.rs` |
| 3.5 | Position close/modify | `src/utils/tui/perp/views/position_manage.rs` |

### Phase 4: Additional Protocols (Week 7-8)

| Task | Description | Files |
|------|-------------|-------|
| 4.1 | Jupiter Perps client | `src/utils/tui/perp/providers/jupiter.rs` |
| 4.2 | Flash Trade client | `src/utils/tui/perp/providers/flash.rs` |
| 4.3 | Protocol switcher UI | Integration |
| 4.4 | History and analytics | `src/utils/tui/perp/views/history.rs` |
| 4.5 | Testing suite | `tests/tui_perp_tests.rs` |

---

## File Structure

```
src/utils/tui/perp/
├── mod.rs                  # Module exports
├── app.rs                  # Main PerpApp state machine
├── models.rs               # Data types
├── protocol.rs             # PerpClient trait
├── validation.rs           # Order validation
├── ws.rs                   # WebSocket management
├── providers/
│   ├── mod.rs
│   ├── drift.rs            # Drift Protocol
│   ├── jupiter.rs          # Jupiter Perps
│   ├── flash.rs            # Flash Trade
│   └── zeta.rs             # Zeta Markets (future)
├── views/
│   ├── mod.rs
│   ├── positions.rs        # Position list
│   ├── orderbook.rs        # Order book display
│   ├── trades.rs           # Recent trades
│   ├── chart.rs            # ASCII price chart
│   ├── stats.rs            # Market statistics
│   ├── order_entry.rs      # Order form
│   ├── orders.rs           # Open orders
│   ├── position_manage.rs  # Position details modal
│   └── history.rs          # Trade history
├── widgets/
│   ├── mod.rs
│   ├── price_ladder.rs     # Order book ladder
│   ├── pnl_display.rs      # PnL formatting
│   ├── leverage_slider.rs  # Leverage control
│   ├── candle_chart.rs     # Candlestick rendering
│   └── funding_bar.rs      # Funding rate display
├── tx/
│   ├── mod.rs
│   ├── place_order.rs
│   ├── cancel_order.rs
│   ├── close_position.rs
│   └── collateral.rs
└── tests.rs
```

---

## Risk Management Features

### Liquidation Protection

```rust
pub struct LiquidationMonitor {
    positions: Arc<RwLock<Vec<Position>>>,
    alert_thresholds: Vec<AlertThreshold>,
}

impl LiquidationMonitor {
    pub fn check_positions(&self) -> Vec<LiquidationAlert> {
        let positions = self.positions.read();
        positions.iter()
            .filter_map(|p| self.check_position(p))
            .collect()
    }

    fn check_position(&self, position: &Position) -> Option<LiquidationAlert> {
        let distance_pct = position.distance_to_liquidation_pct();

        for threshold in &self.alert_thresholds {
            if distance_pct <= threshold.pct {
                return Some(LiquidationAlert {
                    position: position.clone(),
                    threshold: threshold.clone(),
                    distance_pct,
                });
            }
        }
        None
    }
}

// Default thresholds
pub const LIQUIDATION_ALERTS: [f64; 3] = [
    25.0,  // Warning: 25% to liquidation
    15.0,  // Danger: 15% to liquidation
    10.0,  // Critical: 10% to liquidation
];
```

### Position Sizing

```rust
pub fn calculate_safe_position_size(
    account: &PerpAccount,
    market: &MarketInfo,
    leverage: f64,
    risk_pct: f64,  // % of account to risk
) -> SafePositionSize {
    let available_margin = account.free_collateral;
    let max_position_value = available_margin * leverage;

    // Risk-adjusted sizing
    let stop_distance = market.atr_14 * 2.0;  // 2 ATR stop
    let risk_amount = account.total_value * (risk_pct / 100.0);
    let risk_based_size = risk_amount / stop_distance;

    SafePositionSize {
        max_size: max_position_value / market.mark_price,
        recommended_size: risk_based_size.min(max_position_value / market.mark_price),
        risk_amount,
        suggested_stop: market.mark_price - stop_distance,
    }
}
```

---

## Performance Requirements

| Metric | Target |
|--------|--------|
| Order book update | < 50ms latency |
| Order submission | < 500ms to confirm |
| Position refresh | < 1s |
| UI responsiveness | < 16ms frame |
| WebSocket reconnect | < 3s |
| Memory usage | < 200MB |

---

## Security Considerations

### Order Safety

1. **Confirmation Dialogs** - Large orders require confirmation
2. **Max Position Limits** - Configurable position size limits
3. **Price Deviation Checks** - Alert on orders far from mark
4. **Double-Spend Prevention** - Deduplicate rapid clicks

### Session Security

1. **Wallet Connection** - Use existing secure wallet loading
2. **Session Timeout** - Clear sensitive state after inactivity
3. **Order Audit Log** - Local log of all submitted orders

---

## CLI Integration

```rust
// src/clparse.rs addition
#[derive(Subcommand)]
pub enum Commands {
    /// Open perpetual futures trading TUI
    Perp {
        /// Initial protocol (drift, jupiter, flash)
        #[arg(long, default_value = "drift")]
        protocol: String,

        /// Initial market (e.g., "SOL-PERP")
        #[arg(long)]
        market: Option<String>,

        /// Demo mode (paper trading)
        #[arg(long)]
        demo: bool,
    },
}
```

---

## Open Questions

1. **Paper Trading** - Implement demo mode with simulated execution?
2. **Multi-Account** - Support for sub-accounts (Drift)?
3. **Alerts** - System notifications for price/liquidation alerts?
4. **Auto-TP/SL** - Automatic stop-loss on position open?
5. **Order History** - How much history to store locally?
6. **Advanced Orders** - TWAP, iceberg orders?
