# OSVM Swap TUI - Steering Document

**Command:** `osvm swap`
**Purpose:** Terminal UI for cross-market token swaps on Solana
**Priority:** High - Core DeFi functionality

---

## Executive Summary

The Swap TUI provides a unified terminal interface for executing token swaps across multiple Solana DEX aggregators and markets. Users can compare prices, execute swaps, and monitor pending transactions—all from a keyboard-driven terminal interface.

---

## User Stories

### Primary Users
1. **Power Traders** - Need fast execution with keyboard shortcuts
2. **Arbitrageurs** - Compare prices across markets instantly
3. **CLI Enthusiasts** - Prefer terminal over web interfaces
4. **Bot Operators** - Integration point for automated trading

### Key User Stories

| ID | Story | Priority |
|----|-------|----------|
| SW-1 | As a trader, I want to see real-time quotes from multiple DEXs so I can find the best price | P0 |
| SW-2 | As a trader, I want to execute swaps with a single keystroke after selecting a quote | P0 |
| SW-3 | As a user, I want to see my token balances before swapping | P0 |
| SW-4 | As a trader, I want to set slippage tolerance to protect against price movement | P0 |
| SW-5 | As a user, I want to see transaction status and confirmation | P0 |
| SW-6 | As a power user, I want to save favorite token pairs for quick access | P1 |
| SW-7 | As a user, I want to see historical swap transactions | P1 |
| SW-8 | As a trader, I want price alerts when a token reaches a target | P2 |

---

## Architecture

### Component Hierarchy

```
SwapApp
├── HeaderBar
│   ├── WalletStatus (connected address, SOL balance)
│   ├── NetworkIndicator (mainnet/devnet/testnet)
│   └── GasEstimate (priority fee indicator)
├── MainPanel (TabGroup)
│   ├── SwapTab
│   │   ├── TokenSelector (From)
│   │   │   ├── SearchInput
│   │   │   ├── TokenList (filterable)
│   │   │   └── BalanceDisplay
│   │   ├── AmountInput
│   │   ├── SwapDirectionToggle (↕)
│   │   ├── TokenSelector (To)
│   │   └── QuotePanel
│   │       ├── RouteVisualization
│   │       ├── PriceImpact
│   │       ├── MinReceived
│   │       └── DexComparison
│   ├── QuotesTab
│   │   ├── DexList (Jupiter, Raydium, Orca, etc.)
│   │   ├── QuoteComparison
│   │   └── RouteDetails
│   ├── HistoryTab
│   │   ├── RecentSwaps
│   │   ├── PendingTransactions
│   │   └── TransactionDetails
│   └── SettingsTab
│       ├── SlippageConfig
│       ├── PriorityFeeConfig
│       ├── FavoriteTokens
│       └── RPCEndpoint
├── ActionBar
│   ├── SwapButton
│   ├── RefreshQuotes
│   └── KeyboardHints
└── StatusBar
    ├── LastUpdate
    ├── RPC Status
    └── PendingTxCount
```

### Data Flow

```
┌─────────────┐     ┌──────────────┐     ┌─────────────────┐
│   User      │────▶│   SwapApp    │────▶│ Quote Providers │
│   Input     │     │   State      │     │ (Jupiter API)   │
└─────────────┘     └──────────────┘     └─────────────────┘
                           │                      │
                           ▼                      ▼
                    ┌──────────────┐     ┌─────────────────┐
                    │   Renderer   │     │  Quote Cache    │
                    │   (ratatui)  │     │  (30s TTL)      │
                    └──────────────┘     └─────────────────┘
                           │
                           ▼
                    ┌──────────────┐
                    │   Terminal   │
                    │   Output     │
                    └──────────────┘
```

---

## State Management

### Core State Structure

```rust
pub struct SwapState {
    // Token selection
    pub from_token: Option<TokenInfo>,
    pub to_token: Option<TokenInfo>,
    pub amount: String, // User input as string for editing
    pub amount_parsed: Option<f64>,

    // Quote data
    pub quotes: Vec<DexQuote>,
    pub selected_quote: Option<usize>,
    pub quote_loading: bool,
    pub last_quote_time: Option<Instant>,

    // Wallet data
    pub wallet_pubkey: Option<Pubkey>,
    pub token_balances: HashMap<String, TokenBalance>,
    pub sol_balance: u64,

    // Settings
    pub slippage_bps: u16, // Basis points (50 = 0.5%)
    pub priority_fee: PriorityFee,
    pub favorite_tokens: Vec<String>,

    // UI state
    pub active_panel: SwapPanel,
    pub token_search: String,
    pub history: Vec<SwapTransaction>,
    pub pending_tx: Vec<PendingTransaction>,
}

#[derive(Clone)]
pub struct DexQuote {
    pub dex_name: String,
    pub input_mint: String,
    pub output_mint: String,
    pub in_amount: u64,
    pub out_amount: u64,
    pub price_impact_pct: f64,
    pub route: Vec<RouteStep>,
    pub fees: QuoteFees,
    pub expires_at: Instant,
}

pub enum SwapPanel {
    FromToken,
    ToToken,
    Amount,
    Quotes,
    History,
    Settings,
}
```

---

## API Integrations

### Primary: Jupiter Aggregator

```rust
// Jupiter V6 API endpoints
const JUPITER_QUOTE: &str = "https://quote-api.jup.ag/v6/quote";
const JUPITER_SWAP: &str = "https://quote-api.jup.ag/v6/swap";
const JUPITER_TOKENS: &str = "https://token.jup.ag/all";

pub struct JupiterClient {
    client: reqwest::Client,
    base_url: String,
}

impl JupiterClient {
    pub async fn get_quote(&self, params: QuoteParams) -> Result<QuoteResponse>;
    pub async fn get_swap_tx(&self, quote: &QuoteResponse, user: Pubkey) -> Result<VersionedTransaction>;
    pub async fn get_token_list(&self) -> Result<Vec<TokenInfo>>;
}
```

### Secondary Sources (for comparison)

| Provider | Purpose | API |
|----------|---------|-----|
| Jupiter | Primary aggregator | quote-api.jup.ag |
| Raydium | Direct AMM quotes | api.raydium.io |
| Orca | Whirlpool quotes | api.orca.so |
| Birdeye | Price/token data | public-api.birdeye.so |

---

## Keyboard Shortcuts

### Global

| Key | Action |
|-----|--------|
| `q` / `Esc` | Quit / Back |
| `Tab` | Next panel |
| `Shift+Tab` | Previous panel |
| `1-4` | Jump to tab |
| `r` | Refresh quotes |
| `?` | Help overlay |

### Swap Panel

| Key | Action |
|-----|--------|
| `f` | Focus from-token selector |
| `t` | Focus to-token selector |
| `a` | Focus amount input |
| `s` | Switch from/to tokens |
| `Enter` | Execute selected swap |
| `↑/↓` | Navigate quotes |
| `/` | Search tokens |

### Amount Input

| Key | Action |
|-----|--------|
| `0-9` | Enter digits |
| `.` | Decimal point |
| `Backspace` | Delete |
| `m` | Max balance |
| `h` | Half balance |
| `%` | Percentage mode |

---

## UI Mockups

### Main Swap View

```
┌─ OSVM Swap ─────────────────────────────────────────────────────────────────┐
│ Wallet: 7xKX...9fP2 │ SOL: 12.5432 │ Mainnet │ Priority: Medium (5000)     │
├─────────────────────────────────────────────────────────────────────────────┤
│ [Swap] [Quotes] [History] [Settings]                                        │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─ From ─────────────────────────────────────────────────────────────────┐ │
│  │  SOL                                                Balance: 12.5432   │ │
│  │  ┌────────────────────────────────────────────────────────────────────┐│ │
│  │  │ 1.0                                                     [Max] [½]  ││ │
│  │  └────────────────────────────────────────────────────────────────────┘│ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│                              ↕ [s] Switch                                   │
│                                                                             │
│  ┌─ To ───────────────────────────────────────────────────────────────────┐ │
│  │  USDC                                                Balance: 0.00     │ │
│  │  ┌────────────────────────────────────────────────────────────────────┐│ │
│  │  │ ≈ 234.56                                                           ││ │
│  │  └────────────────────────────────────────────────────────────────────┘│ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  ┌─ Best Quote ───────────────────────────────────────────────────────────┐ │
│  │  Jupiter via Raydium → Orca                                            │ │
│  │  Rate: 1 SOL = 234.56 USDC                                             │ │
│  │  Price Impact: 0.01%  │  Min Received: 233.39 USDC                     │ │
│  │  Network Fee: ~0.000005 SOL  │  Route: SOL → USDC                      │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  ┌─ Compare Quotes ───────────────────────────────────────────────────────┐ │
│  │  ▶ Jupiter    234.56 USDC   0.01%  ████████████████████████████████ ✓  │ │
│  │    Raydium    234.12 USDC   0.02%  ███████████████████████████████     │ │
│  │    Orca       233.89 USDC   0.03%  ██████████████████████████████      │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│ [Enter] Swap │ [r] Refresh │ [s] Switch │ [/] Search │ [?] Help            │
├─────────────────────────────────────────────────────────────────────────────┤
│ Quotes updated 3s ago │ RPC: ● Connected │ 0 pending                       │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Token Selector Modal

```
┌─ Select Token ─────────────────────────────────────────────────────────────┐
│ Search: sol                                                                │
├────────────────────────────────────────────────────────────────────────────┤
│ ★ Favorites                                                                │
│   SOL      Solana          Balance: 12.5432                               │
│   USDC     USD Coin        Balance: 1,234.56                              │
│   BONK     Bonk            Balance: 10,000,000                            │
├────────────────────────────────────────────────────────────────────────────┤
│ 🔍 Search Results                                                          │
│ ▶ SOL      Solana                     So11111111111111111111111111111112  │
│   mSOL     Marinade Staked SOL        mSoLzYCxHdYgdzU16g5QSh3i5K3z3KZK7yt │
│   stSOL    Lido Staked SOL            7dHbWXmci3dT8UFYWYZweBLXgycu7Y3iL6t │
│   JitoSOL  Jito Staked SOL            J1toso1uCk3RLmjorhTtrVwY9HJ7X8V9yY │
├────────────────────────────────────────────────────────────────────────────┤
│ [↑↓] Navigate │ [Enter] Select │ [★] Favorite │ [Esc] Cancel              │
└────────────────────────────────────────────────────────────────────────────┘
```

---

## Implementation Plan

### Phase 1: Core Infrastructure (Week 1)

| Task | Description | Files |
|------|-------------|-------|
| 1.1 | Create `SwapApp` struct with state management | `src/utils/tui/swap/app.rs` |
| 1.2 | Implement token list fetching from Jupiter | `src/utils/tui/swap/api.rs` |
| 1.3 | Build basic two-panel swap view | `src/utils/tui/swap/views/swap.rs` |
| 1.4 | Add keyboard input handling | `src/utils/tui/swap/input.rs` |
| 1.5 | Integrate with existing wallet loading | `src/utils/tui/swap/wallet.rs` |

### Phase 2: Quote Engine (Week 2)

| Task | Description | Files |
|------|-------------|-------|
| 2.1 | Jupiter quote API integration | `src/utils/tui/swap/providers/jupiter.rs` |
| 2.2 | Quote comparison view | `src/utils/tui/swap/views/quotes.rs` |
| 2.3 | Route visualization | `src/utils/tui/swap/widgets/route.rs` |
| 2.4 | Price impact warnings | `src/utils/tui/swap/widgets/warnings.rs` |
| 2.5 | Quote auto-refresh (every 30s) | Background task |

### Phase 3: Swap Execution (Week 3)

| Task | Description | Files |
|------|-------------|-------|
| 3.1 | Transaction building from quote | `src/utils/tui/swap/tx.rs` |
| 3.2 | Wallet signing integration | Existing wallet utils |
| 3.3 | Transaction submission + confirmation | `src/utils/tui/swap/submit.rs` |
| 3.4 | Pending transaction tracking | `src/utils/tui/swap/pending.rs` |
| 3.5 | Error handling and retry logic | Across modules |

### Phase 4: Polish & Features (Week 4)

| Task | Description | Files |
|------|-------------|-------|
| 4.1 | History tab with past swaps | `src/utils/tui/swap/views/history.rs` |
| 4.2 | Settings persistence | `src/utils/tui/swap/config.rs` |
| 4.3 | Favorite tokens | Settings integration |
| 4.4 | Help overlay | `src/utils/tui/swap/views/help.rs` |
| 4.5 | Testing and edge cases | `tests/tui_swap_tests.rs` |

---

## File Structure

```
src/utils/tui/swap/
├── mod.rs              # Module exports
├── app.rs              # Main SwapApp state machine
├── state.rs            # State types and transitions
├── input.rs            # Keyboard input handling
├── config.rs           # Settings persistence
├── providers/
│   ├── mod.rs
│   ├── jupiter.rs      # Jupiter aggregator
│   ├── raydium.rs      # Raydium direct
│   └── orca.rs         # Orca whirlpools
├── views/
│   ├── mod.rs
│   ├── swap.rs         # Main swap panel
│   ├── quotes.rs       # Quote comparison
│   ├── history.rs      # Transaction history
│   ├── settings.rs     # Configuration
│   └── help.rs         # Help overlay
├── widgets/
│   ├── mod.rs
│   ├── token_selector.rs
│   ├── amount_input.rs
│   ├── quote_card.rs
│   ├── route_viz.rs
│   └── tx_status.rs
└── tests.rs            # Unit tests
```

---

## Error Handling

### User-Facing Errors

| Error | Message | Action |
|-------|---------|--------|
| Insufficient balance | "Insufficient SOL balance (have: X, need: Y)" | Highlight balance, disable swap |
| High slippage | "Price impact is high (X%). Proceed?" | Confirmation dialog |
| Quote expired | "Quote expired. Refresh for new price." | Auto-refresh or manual |
| Network error | "Failed to fetch quotes. Check connection." | Retry button |
| Tx failed | "Transaction failed: {reason}" | Show details, retry option |

### Recovery Strategies

```rust
pub enum QuoteError {
    NetworkError(reqwest::Error),
    InvalidToken(String),
    InsufficientLiquidity,
    RateLimited,
    Timeout,
}

impl SwapApp {
    async fn handle_quote_error(&mut self, err: QuoteError) {
        match err {
            QuoteError::RateLimited => {
                self.status_message = "Rate limited. Retrying in 5s...".into();
                self.schedule_retry(Duration::from_secs(5));
            }
            QuoteError::NetworkError(_) => {
                self.status_message = "Network error. Press [r] to retry.".into();
            }
            // ... other cases
        }
    }
}
```

---

## Testing Strategy

### Unit Tests

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_amount_parsing() {
        assert_eq!(parse_amount("1.5", 9), Some(1_500_000_000u64));
        assert_eq!(parse_amount("0.000000001", 9), Some(1u64));
        assert_eq!(parse_amount("invalid", 9), None);
    }

    #[test]
    fn test_quote_sorting() {
        let quotes = vec![/* mock quotes */];
        let sorted = sort_quotes_by_output(quotes);
        assert!(sorted[0].out_amount >= sorted[1].out_amount);
    }

    #[test]
    fn test_slippage_calculation() {
        let min_received = calculate_min_received(1000, 50); // 0.5% slippage
        assert_eq!(min_received, 995);
    }
}
```

### Integration Tests

```rust
#[tokio::test]
async fn test_jupiter_quote_integration() {
    let client = JupiterClient::new();
    let quote = client.get_quote(QuoteParams {
        input_mint: SOL_MINT.to_string(),
        output_mint: USDC_MINT.to_string(),
        amount: 1_000_000_000, // 1 SOL
        slippage_bps: 50,
    }).await.unwrap();

    assert!(quote.out_amount > 0);
    assert!(quote.price_impact_pct < 1.0);
}
```

---

## Security Considerations

### Transaction Safety

1. **Slippage Protection** - Always enforce minimum output amount
2. **Quote Freshness** - Reject quotes older than 60 seconds
3. **Simulation First** - Simulate transaction before signing
4. **Priority Fee Caps** - Warn on excessive priority fees
5. **Token Verification** - Validate token mints against known lists

### Wallet Security

1. **No Private Key Storage** - Use existing keypair loading
2. **Confirmation Prompts** - Require confirmation for large swaps
3. **Session Timeouts** - Clear sensitive state after inactivity

---

## Performance Requirements

| Metric | Target |
|--------|--------|
| Quote fetch | < 2s |
| UI responsiveness | < 16ms frame time |
| Token list load | < 3s |
| Transaction submission | < 5s |
| Memory usage | < 100MB |

---

## Dependencies

### Existing (from Cargo.toml)
- `ratatui = "0.29.0"`
- `crossterm = "0.29.0"`
- `tokio` (async runtime)
- `reqwest` (HTTP client)
- `solana-sdk` (transaction building)
- `solana-client` (RPC)

### New (if needed)
- None required - all dependencies available

---

## CLI Integration

```rust
// src/clparse.rs addition
#[derive(Subcommand)]
pub enum Commands {
    // ... existing

    /// Open swap TUI for cross-market token swaps
    Swap {
        /// Initial from-token mint address
        #[arg(long)]
        from: Option<String>,

        /// Initial to-token mint address
        #[arg(long)]
        to: Option<String>,

        /// Initial amount to swap
        #[arg(long)]
        amount: Option<f64>,

        /// Slippage tolerance in basis points (default: 50 = 0.5%)
        #[arg(long, default_value = "50")]
        slippage: u16,
    },
}
```

---

## Success Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| Time to first swap | < 30 seconds | User testing |
| Quote accuracy | Within 0.1% of executed | Compare quote vs result |
| Crash rate | < 0.1% | Error tracking |
| User satisfaction | > 4/5 | Feedback survey |

---

## Open Questions

1. **Multi-hop routing visualization** - How detailed should route display be?
2. **Token list curation** - Full Jupiter list or curated subset?
3. **Ledger support** - Priority for hardware wallet integration?
4. **Price alerts** - Persist to disk or session-only?

---

## Appendix: Token Mints

```rust
// Common token mints for testing
pub const SOL_MINT: &str = "So11111111111111111111111111111111111111112";
pub const USDC_MINT: &str = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v";
pub const USDT_MINT: &str = "Es9vMFrzaCERmJfrF4H2FYD4KCoNkY11McCe8BenwNYB";
pub const BONK_MINT: &str = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263";
pub const JUP_MINT: &str = "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN";
```
