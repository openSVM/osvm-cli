# OSVM AMM TUI - Steering Document

**Command:** `osvm amm`
**Purpose:** Terminal UI for AMM LP management (Aldrin, Raydium, Meteora)
**Priority:** High - Core DeFi functionality for liquidity providers

---

## Executive Summary

The AMM TUI provides a unified terminal interface for managing liquidity positions across Solana's major AMM protocols. Users can view positions, add/remove liquidity, claim rewards, and analyze pool performance—all through a keyboard-driven terminal interface.

---

## Supported Protocols

### Tier 1 (Launch Priority)

| Protocol | Pool Types | Key Features |
|----------|-----------|--------------|
| **Raydium** | CLMM, AMM V4, Concentrated | Largest TVL, OpenBook integration |
| **Meteora** | DLMM, Dynamic Pools, Multi-token | Dynamic fees, advanced bin strategies |
| **Orca** | Whirlpools (CLMM) | Best UX, concentrated liquidity |

### Tier 2 (Post-Launch)

| Protocol | Pool Types | Key Features |
|----------|-----------|--------------|
| **Aldrin** | Classic AMM | Token launchpad integration |
| **Lifinity** | Proactive MM | MEV-resistant, oracle-based |
| **Phoenix** | CLOB + AMM hybrid | Order book integration |

---

## User Stories

### Primary Users
1. **Liquidity Providers** - Manage positions across protocols
2. **Yield Farmers** - Track and harvest rewards
3. **Pool Analysts** - Analyze APY, fees, impermanent loss
4. **DeFi Power Users** - Execute complex LP strategies

### Key User Stories

| ID | Story | Priority |
|----|-------|----------|
| AMM-1 | As an LP, I want to see all my positions across protocols in one view | P0 |
| AMM-2 | As an LP, I want to add liquidity to a pool with proper price range selection | P0 |
| AMM-3 | As an LP, I want to remove liquidity and claim fees in one action | P0 |
| AMM-4 | As an LP, I want to see my earned fees and pending rewards | P0 |
| AMM-5 | As an LP, I want to analyze a pool's historical APY before depositing | P0 |
| AMM-6 | As an LP, I want to rebalance my position when price moves | P1 |
| AMM-7 | As an LP, I want to track impermanent loss in real-time | P1 |
| AMM-8 | As a power user, I want to compare similar pools across protocols | P1 |
| AMM-9 | As a yield farmer, I want to auto-compound rewards | P2 |
| AMM-10 | As an analyst, I want to export pool data to CSV | P2 |

---

## Architecture

### Component Hierarchy

```
AmmApp
├── HeaderBar
│   ├── WalletStatus
│   ├── TotalValueLocked (user's TVL)
│   ├── TotalEarnedFees
│   └── NetworkIndicator
├── SidePanel (Protocol Selector)
│   ├── AllProtocols (aggregated view)
│   ├── RaydiumSection
│   ├── MeteoraSection
│   ├── OrcaSection
│   └── MoreProtocols...
├── MainPanel (TabGroup)
│   ├── PositionsTab
│   │   ├── PositionList
│   │   │   ├── PositionCard (per position)
│   │   │   │   ├── PoolInfo
│   │   │   │   ├── LiquidityAmount
│   │   │   │   ├── PriceRange (for CLMM)
│   │   │   │   ├── UnclaimedFees
│   │   │   │   └── APYIndicator
│   │   │   └── SortControls
│   │   └── PositionDetails (expanded)
│   ├── PoolsTab (Discovery)
│   │   ├── PoolSearch
│   │   ├── PoolFilters
│   │   │   ├── ByTVL
│   │   │   ├── ByAPY
│   │   │   ├── ByVolume
│   │   │   └── ByProtocol
│   │   ├── PoolList
│   │   └── PoolDetails
│   ├── AddLiquidityTab
│   │   ├── PoolSelector
│   │   ├── AmountInputs (Token A/B)
│   │   ├── PriceRangeSelector (CLMM)
│   │   │   ├── RangePresets
│   │   │   ├── CustomRange
│   │   │   └── PriceChart
│   │   └── Preview
│   ├── AnalyticsTab
│   │   ├── PerformanceChart
│   │   ├── FeeEarnings
│   │   ├── ImpermanentLoss
│   │   └── APYBreakdown
│   └── HistoryTab
│       ├── LPTransactions
│       ├── RewardsClaimed
│       └── ExportOptions
├── ActionBar
│   ├── AddLiquidity
│   ├── RemoveLiquidity
│   ├── ClaimRewards
│   └── Rebalance
└── StatusBar
    ├── ProtocolStatus
    ├── PendingActions
    └── LastRefresh
```

### Protocol Abstraction Layer

```rust
/// Unified trait for all AMM protocols
pub trait AmmProtocol: Send + Sync {
    fn name(&self) -> &str;
    fn protocol_id(&self) -> ProtocolId;

    // Position management
    async fn get_positions(&self, owner: &Pubkey) -> Result<Vec<Position>>;
    async fn get_pool_info(&self, pool: &Pubkey) -> Result<PoolInfo>;

    // Liquidity operations
    async fn build_add_liquidity_tx(&self, params: AddLiquidityParams) -> Result<Transaction>;
    async fn build_remove_liquidity_tx(&self, params: RemoveLiquidityParams) -> Result<Transaction>;
    async fn build_claim_rewards_tx(&self, position: &Position) -> Result<Transaction>;

    // Analytics
    async fn get_pool_stats(&self, pool: &Pubkey, timeframe: Timeframe) -> Result<PoolStats>;
    async fn estimate_apy(&self, pool: &Pubkey) -> Result<ApyEstimate>;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ProtocolId {
    Raydium,
    Meteora,
    Orca,
    Aldrin,
    Lifinity,
    Phoenix,
}
```

---

## State Management

### Core State Structure

```rust
pub struct AmmState {
    // Protocol registry
    pub protocols: HashMap<ProtocolId, Arc<dyn AmmProtocol>>,
    pub active_protocol: Option<ProtocolId>,

    // Position data
    pub positions: Vec<Position>,
    pub positions_loading: bool,
    pub selected_position: Option<usize>,

    // Pool discovery
    pub pools: Vec<PoolInfo>,
    pub pool_filters: PoolFilters,
    pub pool_search: String,
    pub selected_pool: Option<Pubkey>,

    // Add liquidity form
    pub add_liq_form: AddLiquidityForm,

    // Analytics
    pub analytics_data: Option<AnalyticsData>,
    pub analytics_timeframe: Timeframe,

    // Wallet
    pub wallet_pubkey: Option<Pubkey>,
    pub token_balances: HashMap<String, TokenBalance>,

    // UI state
    pub active_tab: AmmTab,
    pub active_panel: AmmPanel,
    pub modals: ModalStack,
}

#[derive(Clone)]
pub struct Position {
    pub protocol: ProtocolId,
    pub pool_address: Pubkey,
    pub position_address: Pubkey,
    pub token_a: TokenInfo,
    pub token_b: TokenInfo,
    pub liquidity: u128,
    pub token_a_amount: u64,
    pub token_b_amount: u64,
    pub unclaimed_fees_a: u64,
    pub unclaimed_fees_b: u64,
    pub unclaimed_rewards: Vec<RewardInfo>,
    // CLMM-specific
    pub price_lower: Option<f64>,
    pub price_upper: Option<f64>,
    pub in_range: Option<bool>,
    // Metadata
    pub created_at: DateTime<Utc>,
    pub last_updated: DateTime<Utc>,
}

#[derive(Clone)]
pub struct PoolInfo {
    pub protocol: ProtocolId,
    pub address: Pubkey,
    pub token_a: TokenInfo,
    pub token_b: TokenInfo,
    pub pool_type: PoolType,
    pub tvl_usd: f64,
    pub volume_24h_usd: f64,
    pub fees_24h_usd: f64,
    pub apy_estimate: f64,
    pub fee_rate_bps: u16,
    pub current_price: f64,
}

#[derive(Clone, Copy)]
pub enum PoolType {
    ConstantProduct,    // x * y = k
    Concentrated,       // Uniswap V3 style
    DynamicAmm,         // Meteora DLMM
    StableSwap,         // Curve-style
    Hybrid,             // Mixed mechanisms
}

pub struct AddLiquidityForm {
    pub pool: Option<Pubkey>,
    pub amount_a: String,
    pub amount_b: String,
    pub amount_mode: AmountMode,
    // CLMM range
    pub price_lower: String,
    pub price_upper: String,
    pub range_preset: RangePreset,
    // Preview
    pub preview: Option<AddLiquidityPreview>,
}

#[derive(Clone, Copy)]
pub enum RangePreset {
    Full,           // Full range (like V2)
    Narrow,         // ±2% from current
    Medium,         // ±10% from current
    Wide,           // ±50% from current
    Custom,         // User-defined
}
```

---

## Protocol Integrations

### Raydium Integration

```rust
pub struct RaydiumClient {
    rpc: Arc<RpcClient>,
    api_base: String,
}

impl RaydiumClient {
    const API_BASE: &'static str = "https://api-v3.raydium.io";

    pub async fn get_clmm_positions(&self, owner: &Pubkey) -> Result<Vec<ClmmPosition>>;
    pub async fn get_amm_positions(&self, owner: &Pubkey) -> Result<Vec<AmmPosition>>;
    pub async fn get_pools(&self, filters: &PoolFilters) -> Result<Vec<RaydiumPool>>;

    // Transaction builders
    pub fn build_open_position(&self, params: OpenPositionParams) -> Result<Transaction>;
    pub fn build_increase_liquidity(&self, params: IncreaseLiqParams) -> Result<Transaction>;
    pub fn build_decrease_liquidity(&self, params: DecreaseLiqParams) -> Result<Transaction>;
    pub fn build_collect_fees(&self, position: &Pubkey) -> Result<Transaction>;
}

// On-chain account parsing
#[derive(BorshDeserialize)]
pub struct RaydiumClmmPosition {
    pub pool_id: Pubkey,
    pub nft_mint: Pubkey,
    pub tick_lower: i32,
    pub tick_upper: i32,
    pub liquidity: u128,
    pub fee_growth_inside_a: u128,
    pub fee_growth_inside_b: u128,
    pub token_fees_owed_a: u64,
    pub token_fees_owed_b: u64,
    pub reward_infos: [PositionRewardInfo; 3],
}
```

### Meteora Integration

```rust
pub struct MeteoraClient {
    rpc: Arc<RpcClient>,
    api_base: String,
}

impl MeteoraClient {
    const API_BASE: &'static str = "https://dlmm-api.meteora.ag";

    pub async fn get_dlmm_positions(&self, owner: &Pubkey) -> Result<Vec<DlmmPosition>>;
    pub async fn get_pools(&self, filters: &PoolFilters) -> Result<Vec<MeteoraPool>>;
    pub async fn get_bin_arrays(&self, pool: &Pubkey) -> Result<Vec<BinArray>>;

    // DLMM-specific operations
    pub fn build_add_liquidity_by_strategy(
        &self,
        pool: &Pubkey,
        strategy: LiquidityStrategy,
        amount: u64,
    ) -> Result<Transaction>;
}

#[derive(Clone, Copy)]
pub enum LiquidityStrategy {
    Spot,           // Concentrated around current price
    Curve,          // Bell curve distribution
    BidAsk,         // Split on both sides
    Uniform,        // Even distribution
}
```

### Orca Integration

```rust
pub struct OrcaClient {
    rpc: Arc<RpcClient>,
    api_base: String,
}

impl OrcaClient {
    const API_BASE: &'static str = "https://api.orca.so";

    pub async fn get_whirlpool_positions(&self, owner: &Pubkey) -> Result<Vec<WhirlpoolPosition>>;
    pub async fn get_whirlpools(&self, filters: &PoolFilters) -> Result<Vec<Whirlpool>>;

    pub fn build_open_position_with_metadata(
        &self,
        params: OpenWhirlpoolPositionParams,
    ) -> Result<Transaction>;
}
```

---

## Keyboard Shortcuts

### Global

| Key | Action |
|-----|--------|
| `q` / `Esc` | Quit / Back |
| `Tab` | Next panel |
| `Shift+Tab` | Previous panel |
| `1-5` | Jump to tab |
| `r` | Refresh data |
| `?` | Help overlay |
| `p` | Protocol selector |

### Positions Tab

| Key | Action |
|-----|--------|
| `↑/↓` | Navigate positions |
| `Enter` | View position details |
| `a` | Add liquidity to selected |
| `w` | Withdraw liquidity |
| `c` | Claim fees/rewards |
| `b` | Rebalance position |
| `s` | Sort positions |
| `f` | Filter positions |

### Pools Tab

| Key | Action |
|-----|--------|
| `↑/↓` | Navigate pools |
| `Enter` | View pool details |
| `/` | Search pools |
| `n` | New position in pool |
| `t` | Toggle TVL/APY/Volume sort |

### Add Liquidity

| Key | Action |
|-----|--------|
| `Tab` | Next input field |
| `1-4` | Select range preset |
| `[/]` | Adjust lower bound |
| `{/}` | Adjust upper bound |
| `m` | Max balance |
| `Enter` | Preview → Confirm |

---

## UI Mockups

### Positions Overview

```
┌─ OSVM AMM ─────────────────────────────────────────────────────────────────┐
│ Wallet: 7xKX...9fP2 │ Total TVL: $12,345.67 │ Unclaimed: $234.56 │ Mainnet │
├────────────┬────────────────────────────────────────────────────────────────┤
│ Protocols  │ [Positions] [Pools] [Add Liquidity] [Analytics] [History]     │
├────────────┼────────────────────────────────────────────────────────────────┤
│            │                                                                │
│ ▶ All (5)  │  ┌─ Active Positions ────────────────────────────────────────┐ │
│   Raydium  │  │                                                           │ │
│     CLMM 2 │  │ ▶ SOL/USDC (Raydium CLMM)                                │ │
│     AMM  1 │  │   $5,234.00 │ APY: 45.2% │ Fees: $12.34 │ ● In Range     │ │
│   Meteora  │  │   Range: 180.50 - 250.00 │ Current: 234.56              │ │
│     DLMM 1 │  │   ████████████████░░░░░░░░░ 68% utilization              │ │
│   Orca     │  │                                                           │ │
│     WP   1 │  │   SOL/USDC (Meteora DLMM)                                │ │
│            │  │   $3,456.00 │ APY: 52.1% │ Fees: $8.90 │ ● In Range      │ │
│            │  │   Bins: 234-256 │ Current Bin: 245                        │ │
│            │  │   ██████████████████░░░░░░ 72% utilization                │ │
│            │  │                                                           │ │
│            │  │   JUP/USDC (Orca Whirlpool)                              │ │
│            │  │   $2,100.00 │ APY: 38.5% │ Fees: $5.67 │ ⚠ Out of Range │ │
│            │  │   Range: 0.80 - 1.20 │ Current: 1.45                     │ │
│            │  │   ░░░░░░░░░░░░░░░░░░░░░░░░ 0% earning                    │ │
│            │  │                                                           │ │
│            │  └───────────────────────────────────────────────────────────┘ │
│            │                                                                │
│            │  ┌─ Summary ─────────────────────────────────────────────────┐ │
│            │  │ Total Value: $12,345.67 │ 24h Fees: $45.23 │ 7d: $312.45 │ │
│            │  │ In Range: 4/5 (80%) │ Est. Daily: ~$6.46 │ APY: ~19.1%   │ │
│            │  └───────────────────────────────────────────────────────────┘ │
│            │                                                                │
├────────────┴────────────────────────────────────────────────────────────────┤
│ [a] Add │ [w] Withdraw │ [c] Claim $234.56 │ [b] Rebalance │ [?] Help      │
├─────────────────────────────────────────────────────────────────────────────┤
│ Last updated: 5s ago │ Raydium: ● │ Meteora: ● │ Orca: ● │ 2 pending      │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Add Liquidity (CLMM)

```
┌─ Add Liquidity ─────────────────────────────────────────────────────────────┐
│                                                                             │
│  Pool: SOL/USDC (Raydium CLMM)                                             │
│  Current Price: 234.56 USDC per SOL                                        │
│  24h Volume: $1.2M │ TVL: $5.4M │ Fee: 0.25%                               │
│                                                                             │
│  ┌─ Price Range ──────────────────────────────────────────────────────────┐ │
│  │                                                                        │ │
│  │  Presets: [1] Full  [2] ±10%  [3] ±25%  [4] ±50%  [5] Custom          │ │
│  │                                                                        │ │
│  │         ▼ Lower                          ▼ Upper                       │ │
│  │  ┌──────────────────┐            ┌──────────────────┐                  │ │
│  │  │     210.00       │            │     260.00       │                  │ │
│  │  └──────────────────┘            └──────────────────┘                  │ │
│  │                                                                        │ │
│  │                     Price Distribution                                 │ │
│  │     180    200    220    240    260    280    300                     │ │
│  │      │      │      │      │      │      │      │                      │ │
│  │              ┌─────┬──────┬─────┐                                     │ │
│  │              │░░░░░│██████│░░░░░│                                     │ │
│  │              │░░░░░│██████│░░░░░│                                     │ │
│  │              └─────┴──────┴─────┘                                     │ │
│  │                    ▲ 234.56 (current)                                 │ │
│  │                                                                        │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  ┌─ Deposit Amounts ──────────────────────────────────────────────────────┐ │
│  │                                                                        │ │
│  │  SOL                                          Balance: 12.5432        │ │
│  │  ┌────────────────────────────────────────────────────────┐           │ │
│  │  │ 2.0                                          [Max] [½] │           │ │
│  │  └────────────────────────────────────────────────────────┘           │ │
│  │  ≈ $469.12                                                            │ │
│  │                                                                        │ │
│  │  USDC                                         Balance: 1,234.56       │ │
│  │  ┌────────────────────────────────────────────────────────┐           │ │
│  │  │ 469.12                                       [Max] [½] │           │ │
│  │  └────────────────────────────────────────────────────────┘           │ │
│  │                                                                        │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  ┌─ Preview ──────────────────────────────────────────────────────────────┐ │
│  │  Total Value: $938.24                                                  │ │
│  │  Estimated APY: 45.2% (based on 7d volume)                            │ │
│  │  Fee Tier: 0.25% │ In Range: Yes                                       │ │
│  │  Network Fee: ~0.00001 SOL                                             │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│ [Enter] Confirm │ [Tab] Next Field │ [±] Adjust Range │ [Esc] Cancel       │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Position Details Modal

```
┌─ Position Details ──────────────────────────────────────────────────────────┐
│                                                                             │
│  SOL/USDC │ Raydium CLMM │ Position #1234                                  │
│                                                                             │
│  ┌─ Value Breakdown ──────────────────────────────────────────────────────┐ │
│  │                                                                        │ │
│  │  Token           Amount              Value                             │ │
│  │  ─────────────────────────────────────────────                         │ │
│  │  SOL             12.3456             $2,895.12                         │ │
│  │  USDC            2,338.88            $2,338.88                         │ │
│  │  ─────────────────────────────────────────────                         │ │
│  │  Total Position                      $5,234.00                         │ │
│  │                                                                        │ │
│  │  Unclaimed Fees                                                        │ │
│  │  SOL             0.0234              $5.49                             │ │
│  │  USDC            6.85                $6.85                             │ │
│  │  ─────────────────────────────────────────────                         │ │
│  │  Total Fees                          $12.34                            │ │
│  │                                                                        │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  ┌─ Price Range ──────────────────────────────────────────────────────────┐ │
│  │                                                                        │ │
│  │  Lower Tick: 180.50    Current: 234.56    Upper Tick: 250.00          │ │
│  │  │░░░░░░░░░░░░░░░░░░░░░░████░░░░░░│                                   │ │
│  │  Status: ● IN RANGE (earning fees)                                    │ │
│  │  Capital Efficiency: 4.2x vs full range                               │ │
│  │                                                                        │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  ┌─ Performance ──────────────────────────────────────────────────────────┐ │
│  │                                                                        │ │
│  │  Created: 2024-01-15 │ Age: 45 days                                    │ │
│  │  Initial Value: $4,800.00 │ Current: $5,234.00 │ Change: +$434 (+9%)  │ │
│  │  Fees Earned (All Time): $312.45                                       │ │
│  │  Impermanent Loss: -$78.45 (-1.6%)                                    │ │
│  │  Net P&L: +$234.00 (+4.9%)                                            │ │
│  │  Realized APY: 39.7%                                                   │ │
│  │                                                                        │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│ [a] Add More │ [w] Withdraw │ [c] Claim Fees │ [b] Rebalance │ [Esc] Close │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Implementation Plan

### Phase 1: Core Infrastructure (Week 1-2)

| Task | Description | Files |
|------|-------------|-------|
| 1.1 | Protocol abstraction trait | `src/utils/tui/amm/protocol.rs` |
| 1.2 | Raydium client implementation | `src/utils/tui/amm/providers/raydium.rs` |
| 1.3 | Position data models | `src/utils/tui/amm/models.rs` |
| 1.4 | Main AmmApp state machine | `src/utils/tui/amm/app.rs` |
| 1.5 | Basic positions view | `src/utils/tui/amm/views/positions.rs` |

### Phase 2: Protocol Implementations (Week 3-4)

| Task | Description | Files |
|------|-------------|-------|
| 2.1 | Meteora DLMM client | `src/utils/tui/amm/providers/meteora.rs` |
| 2.2 | Orca Whirlpool client | `src/utils/tui/amm/providers/orca.rs` |
| 2.3 | Position aggregation service | `src/utils/tui/amm/aggregator.rs` |
| 2.4 | Pool discovery view | `src/utils/tui/amm/views/pools.rs` |
| 2.5 | Protocol-specific account parsing | `src/utils/tui/amm/parsers/` |

### Phase 3: Liquidity Operations (Week 5-6)

| Task | Description | Files |
|------|-------------|-------|
| 3.1 | Add liquidity form + validation | `src/utils/tui/amm/views/add_liquidity.rs` |
| 3.2 | Price range selector widget | `src/utils/tui/amm/widgets/range_selector.rs` |
| 3.3 | Remove liquidity flow | `src/utils/tui/amm/views/remove_liquidity.rs` |
| 3.4 | Claim rewards transaction builder | `src/utils/tui/amm/tx/claim.rs` |
| 3.5 | Transaction submission + confirmation | `src/utils/tui/amm/tx/submit.rs` |

### Phase 4: Analytics & Polish (Week 7-8)

| Task | Description | Files |
|------|-------------|-------|
| 4.1 | Position analytics view | `src/utils/tui/amm/views/analytics.rs` |
| 4.2 | Impermanent loss calculator | `src/utils/tui/amm/analytics/il.rs` |
| 4.3 | APY estimation engine | `src/utils/tui/amm/analytics/apy.rs` |
| 4.4 | History and export | `src/utils/tui/amm/views/history.rs` |
| 4.5 | Testing and documentation | `tests/tui_amm_tests.rs` |

---

## File Structure

```
src/utils/tui/amm/
├── mod.rs                  # Module exports
├── app.rs                  # Main AmmApp state machine
├── models.rs               # Data types (Position, Pool, etc.)
├── protocol.rs             # AmmProtocol trait definition
├── aggregator.rs           # Multi-protocol position aggregation
├── providers/
│   ├── mod.rs
│   ├── raydium.rs          # Raydium CLMM + AMM
│   ├── meteora.rs          # Meteora DLMM + Dynamic
│   ├── orca.rs             # Orca Whirlpools
│   └── aldrin.rs           # Aldrin (future)
├── parsers/
│   ├── mod.rs
│   ├── raydium_accounts.rs # On-chain account parsing
│   ├── meteora_accounts.rs
│   └── orca_accounts.rs
├── views/
│   ├── mod.rs
│   ├── positions.rs        # Position list view
│   ├── pools.rs            # Pool discovery
│   ├── add_liquidity.rs    # Add liquidity form
│   ├── remove_liquidity.rs # Remove liquidity form
│   ├── analytics.rs        # Performance analytics
│   └── history.rs          # Transaction history
├── widgets/
│   ├── mod.rs
│   ├── position_card.rs    # Position summary card
│   ├── pool_card.rs        # Pool info card
│   ├── range_selector.rs   # Price range picker
│   ├── price_chart.rs      # Mini price chart
│   └── utilization_bar.rs  # Range utilization
├── analytics/
│   ├── mod.rs
│   ├── il.rs               # Impermanent loss calc
│   ├── apy.rs              # APY estimation
│   └── fees.rs             # Fee tracking
├── tx/
│   ├── mod.rs
│   ├── add.rs              # Add liquidity tx builders
│   ├── remove.rs           # Remove liquidity tx builders
│   ├── claim.rs            # Claim fees/rewards
│   └── submit.rs           # Transaction submission
└── tests.rs
```

---

## Security Considerations

### Position Safety

1. **Slippage Protection** - Enforce min amounts for withdrawals
2. **Price Impact Warnings** - Alert on large positions
3. **Range Validation** - Ensure valid tick ranges
4. **Balance Verification** - Confirm sufficient balances before TX

### Protocol-Specific Risks

| Risk | Mitigation |
|------|------------|
| Smart contract bugs | Display protocol audit status |
| Impermanent loss | Show real-time IL calculations |
| Out-of-range positions | Visual alerts + rebalance suggestions |
| MEV/sandwich attacks | Priority fee recommendations |

---

## Performance Requirements

| Metric | Target |
|--------|--------|
| Position fetch (all protocols) | < 5s |
| Pool list fetch | < 3s |
| UI responsiveness | < 16ms |
| Transaction build | < 2s |
| Memory usage | < 150MB |

---

## Dependencies

### Existing
- `ratatui`, `crossterm` (TUI)
- `solana-sdk`, `solana-client` (blockchain)
- `tokio`, `reqwest` (async, HTTP)
- `borsh` (account parsing)

### Protocol SDKs (optional)
- Consider integrating official Raydium/Orca SDKs if complexity warrants

---

## CLI Integration

```rust
// src/clparse.rs addition
#[derive(Subcommand)]
pub enum Commands {
    /// Open AMM LP management TUI
    Amm {
        /// Filter by protocol (raydium, meteora, orca, all)
        #[arg(long, default_value = "all")]
        protocol: String,

        /// Show only positions (skip pool discovery)
        #[arg(long)]
        positions_only: bool,

        /// Start with specific pool address
        #[arg(long)]
        pool: Option<String>,
    },
}
```

---

## Open Questions

1. **Position NFTs** - How to handle Raydium/Orca position NFTs in UI?
2. **Multi-wallet** - Support for viewing multiple wallets?
3. **Auto-compound** - Build auto-compounding feature or defer?
4. **Notifications** - Alert when position goes out of range?
5. **CSV Export** - Full transaction history or summary only?
