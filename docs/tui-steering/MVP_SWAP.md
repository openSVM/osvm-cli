# osvm swap - MVP Specification

**Timeline:** 4-5 weeks
**New Code:** ~1,800 LOC
**Leverage:** 40% existing infrastructure

---

## Scope: What We're Building

A terminal UI for token swaps using Jupiter aggregator:
1. 🔨 Token pair selection (from/to)
2. 🔨 Amount input with balance display
3. 🔨 Quote fetching from Jupiter
4. 🔨 Swap execution with confirmation
5. 🔨 Transaction status tracking

**NOT in MVP:**
- Multiple DEX comparison (Jupiter aggregates anyway)
- Price alerts
- Historical swap data
- Favorite tokens
- Hardware wallet support

---

## Week 1: Jupiter API Integration

### Day 1-2: API Client

```rust
// src/utils/tui/swap/jupiter.rs

use reqwest::Client;
use serde::{Deserialize, Serialize};

const JUPITER_QUOTE_API: &str = "https://quote-api.jup.ag/v6/quote";
const JUPITER_SWAP_API: &str = "https://quote-api.jup.ag/v6/swap";
const JUPITER_TOKENS_API: &str = "https://token.jup.ag/all";

pub struct JupiterClient {
    client: Client,
}

#[derive(Debug, Serialize)]
pub struct QuoteParams {
    #[serde(rename = "inputMint")]
    pub input_mint: String,
    #[serde(rename = "outputMint")]
    pub output_mint: String,
    pub amount: u64,  // In smallest units (lamports for SOL)
    #[serde(rename = "slippageBps")]
    pub slippage_bps: u16,
}

#[derive(Debug, Deserialize)]
pub struct QuoteResponse {
    #[serde(rename = "inputMint")]
    pub input_mint: String,
    #[serde(rename = "outputMint")]
    pub output_mint: String,
    #[serde(rename = "inAmount")]
    pub in_amount: String,
    #[serde(rename = "outAmount")]
    pub out_amount: String,
    #[serde(rename = "priceImpactPct")]
    pub price_impact_pct: String,
    #[serde(rename = "routePlan")]
    pub route_plan: Vec<RoutePlanStep>,
    #[serde(rename = "swapMode")]
    pub swap_mode: String,
}

#[derive(Debug, Deserialize)]
pub struct RoutePlanStep {
    #[serde(rename = "swapInfo")]
    pub swap_info: SwapInfo,
    pub percent: u8,
}

#[derive(Debug, Deserialize)]
pub struct SwapInfo {
    #[serde(rename = "ammKey")]
    pub amm_key: String,
    pub label: Option<String>,
    #[serde(rename = "inputMint")]
    pub input_mint: String,
    #[serde(rename = "outputMint")]
    pub output_mint: String,
    #[serde(rename = "inAmount")]
    pub in_amount: String,
    #[serde(rename = "outAmount")]
    pub out_amount: String,
    #[serde(rename = "feeAmount")]
    pub fee_amount: String,
}

impl JupiterClient {
    pub fn new() -> Self {
        Self {
            client: Client::builder()
                .timeout(std::time::Duration::from_secs(30))
                .build()
                .unwrap(),
        }
    }

    pub async fn get_quote(&self, params: QuoteParams) -> Result<QuoteResponse> {
        let url = format!(
            "{}?inputMint={}&outputMint={}&amount={}&slippageBps={}",
            JUPITER_QUOTE_API,
            params.input_mint,
            params.output_mint,
            params.amount,
            params.slippage_bps
        );

        let response = self.client.get(&url).send().await?;

        if !response.status().is_success() {
            let error_text = response.text().await?;
            return Err(anyhow!("Jupiter API error: {}", error_text));
        }

        Ok(response.json().await?)
    }

    pub async fn get_swap_transaction(
        &self,
        quote: &QuoteResponse,
        user_pubkey: &str,
    ) -> Result<SwapTransaction> {
        let body = serde_json::json!({
            "quoteResponse": quote,
            "userPublicKey": user_pubkey,
            "wrapAndUnwrapSol": true,
            "dynamicComputeUnitLimit": true,
            "prioritizationFeeLamports": "auto"
        });

        let response = self.client
            .post(JUPITER_SWAP_API)
            .json(&body)
            .send()
            .await?;

        if !response.status().is_success() {
            let error_text = response.text().await?;
            return Err(anyhow!("Jupiter swap error: {}", error_text));
        }

        Ok(response.json().await?)
    }
}

#[derive(Debug, Deserialize)]
pub struct SwapTransaction {
    #[serde(rename = "swapTransaction")]
    pub swap_transaction: String,  // Base64 encoded
    #[serde(rename = "lastValidBlockHeight")]
    pub last_valid_block_height: u64,
}
```

### Day 3-4: Token List + Caching

```rust
// src/utils/tui/swap/tokens.rs

use std::collections::HashMap;

#[derive(Debug, Clone, Deserialize)]
pub struct TokenInfo {
    pub address: String,
    pub symbol: String,
    pub name: String,
    pub decimals: u8,
    #[serde(rename = "logoURI")]
    pub logo_uri: Option<String>,
    pub tags: Option<Vec<String>>,
}

pub struct TokenRegistry {
    tokens: HashMap<String, TokenInfo>,  // address -> info
    by_symbol: HashMap<String, String>,   // symbol -> address
    popular: Vec<String>,                  // Popular token addresses
}

impl TokenRegistry {
    pub async fn load() -> Result<Self> {
        // Load from Jupiter token list
        let client = reqwest::Client::new();
        let tokens: Vec<TokenInfo> = client
            .get("https://token.jup.ag/all")
            .send()
            .await?
            .json()
            .await?;

        let mut registry = Self {
            tokens: HashMap::new(),
            by_symbol: HashMap::new(),
            popular: vec![
                "So11111111111111111111111111111111111111112".into(), // SOL
                "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v".into(), // USDC
                "Es9vMFrzaCERmJfrF4H2FYD4KCoNkY11McCe8BenwNYB".into(), // USDT
                "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN".into(),  // JUP
                "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263".into(), // BONK
            ],
        };

        for token in tokens {
            registry.by_symbol.insert(token.symbol.to_uppercase(), token.address.clone());
            registry.tokens.insert(token.address.clone(), token);
        }

        Ok(registry)
    }

    pub fn search(&self, query: &str) -> Vec<&TokenInfo> {
        let query_upper = query.to_uppercase();
        let mut results: Vec<_> = self.tokens.values()
            .filter(|t| {
                t.symbol.to_uppercase().contains(&query_upper) ||
                t.name.to_uppercase().contains(&query_upper) ||
                t.address.starts_with(query)
            })
            .collect();

        // Sort by relevance (exact symbol match first, then popular, then alphabetical)
        results.sort_by(|a, b| {
            let a_exact = a.symbol.to_uppercase() == query_upper;
            let b_exact = b.symbol.to_uppercase() == query_upper;
            if a_exact != b_exact {
                return b_exact.cmp(&a_exact);
            }

            let a_pop = self.popular.contains(&a.address);
            let b_pop = self.popular.contains(&b.address);
            if a_pop != b_pop {
                return b_pop.cmp(&a_pop);
            }

            a.symbol.cmp(&b.symbol)
        });

        results.into_iter().take(10).collect()
    }

    pub fn get(&self, address: &str) -> Option<&TokenInfo> {
        self.tokens.get(address)
    }

    pub fn get_by_symbol(&self, symbol: &str) -> Option<&TokenInfo> {
        self.by_symbol.get(&symbol.to_uppercase())
            .and_then(|addr| self.tokens.get(addr))
    }
}
```

### Day 5: Wallet Balance Fetching

```rust
// src/utils/tui/swap/wallet.rs

use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;

pub struct WalletState {
    pub pubkey: Pubkey,
    pub sol_balance: u64,
    pub token_balances: HashMap<String, TokenBalance>,
}

pub struct TokenBalance {
    pub mint: String,
    pub amount: u64,
    pub decimals: u8,
    pub ui_amount: f64,
}

pub async fn fetch_wallet_balances(
    rpc: &RpcClient,
    pubkey: &Pubkey,
) -> Result<WalletState> {
    // Get SOL balance
    let sol_balance = rpc.get_balance(pubkey).await?;

    // Get token accounts
    let token_accounts = rpc.get_token_accounts_by_owner(
        pubkey,
        solana_client::rpc_request::TokenAccountsFilter::ProgramId(
            spl_token::id()
        ),
    ).await?;

    let mut token_balances = HashMap::new();

    for account in token_accounts.value {
        if let Some(data) = account.account.data.decode() {
            let parsed: serde_json::Value = serde_json::from_slice(&data)?;
            if let Some(info) = parsed.get("parsed").and_then(|p| p.get("info")) {
                let mint = info["mint"].as_str().unwrap_or_default().to_string();
                let amount = info["tokenAmount"]["amount"].as_str()
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                let decimals = info["tokenAmount"]["decimals"].as_u64().unwrap_or(0) as u8;
                let ui_amount = info["tokenAmount"]["uiAmount"].as_f64().unwrap_or(0.0);

                token_balances.insert(mint.clone(), TokenBalance {
                    mint,
                    amount,
                    decimals,
                    ui_amount,
                });
            }
        }
    }

    Ok(WalletState {
        pubkey: *pubkey,
        sol_balance,
        token_balances,
    })
}
```

---

## Week 2: TUI State + Basic View

### Day 1-2: SwapApp State

```rust
// src/utils/tui/swap/app.rs

pub struct SwapApp {
    // Token selection
    pub from_token: Option<TokenInfo>,
    pub to_token: Option<TokenInfo>,

    // Amount input
    pub amount_input: String,
    pub amount_parsed: Option<f64>,

    // Quote
    pub quote: Option<QuoteResponse>,
    pub quote_loading: bool,
    pub quote_error: Option<String>,
    pub last_quote_time: Option<Instant>,

    // Wallet
    pub wallet: Option<WalletState>,
    pub wallet_loading: bool,

    // Services
    pub jupiter: JupiterClient,
    pub tokens: Option<TokenRegistry>,
    pub rpc: RpcClient,

    // UI state
    pub focus: SwapFocus,
    pub token_search: String,
    pub token_search_results: Vec<TokenInfo>,
    pub selected_search_result: usize,

    // Swap execution
    pub swap_status: Option<SwapStatus>,

    // Settings
    pub slippage_bps: u16,  // Default 50 = 0.5%

    // General
    pub should_quit: bool,
}

#[derive(Clone, Copy, PartialEq)]
pub enum SwapFocus {
    FromToken,
    ToToken,
    Amount,
    SwapButton,
    Settings,
}

#[derive(Clone)]
pub enum SwapStatus {
    Confirming,           // User confirming swap
    Building,             // Building transaction
    Signing,              // Waiting for signature
    Submitting,           // Submitting to network
    Confirming(String),   // Waiting for confirmation, sig
    Success(String),      // Success with signature
    Failed(String),       // Failed with error
}

impl SwapApp {
    pub fn new(rpc_url: &str) -> Self {
        Self {
            from_token: None,
            to_token: None,
            amount_input: String::new(),
            amount_parsed: None,
            quote: None,
            quote_loading: false,
            quote_error: None,
            last_quote_time: None,
            wallet: None,
            wallet_loading: false,
            jupiter: JupiterClient::new(),
            tokens: None,
            rpc: RpcClient::new(rpc_url.to_string()),
            focus: SwapFocus::FromToken,
            token_search: String::new(),
            token_search_results: Vec::new(),
            selected_search_result: 0,
            swap_status: None,
            slippage_bps: 50,
            should_quit: false,
        }
    }

    pub async fn initialize(&mut self, wallet_pubkey: &Pubkey) -> Result<()> {
        // Load token registry
        self.tokens = Some(TokenRegistry::load().await?);

        // Set defaults
        if let Some(tokens) = &self.tokens {
            self.from_token = tokens.get_by_symbol("SOL").cloned();
            self.to_token = tokens.get_by_symbol("USDC").cloned();
        }

        // Load wallet balances
        self.wallet = Some(fetch_wallet_balances(&self.rpc, wallet_pubkey).await?);

        Ok(())
    }
}
```

### Day 3-5: Main View Rendering

```rust
// src/utils/tui/swap/views/main.rs

impl SwapApp {
    pub fn render(&mut self, f: &mut Frame) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),   // Header
                Constraint::Min(20),     // Main content
                Constraint::Length(3),   // Action bar
                Constraint::Length(1),   // Status bar
            ])
            .split(f.area());

        self.render_header(f, chunks[0]);
        self.render_main(f, chunks[1]);
        self.render_action_bar(f, chunks[2]);
        self.render_status_bar(f, chunks[3]);

        // Render token selector modal if active
        if matches!(self.focus, SwapFocus::FromToken | SwapFocus::ToToken)
            && !self.token_search.is_empty()
        {
            self.render_token_selector(f);
        }
    }

    fn render_header(&self, f: &mut Frame, area: Rect) {
        let wallet_info = if let Some(w) = &self.wallet {
            format!(
                " {}...{} │ {} SOL ",
                &w.pubkey.to_string()[..4],
                &w.pubkey.to_string()[40..],
                w.sol_balance as f64 / 1e9
            )
        } else {
            " No wallet ".to_string()
        };

        let header = Paragraph::new(Line::from(vec![
            Span::styled(" OSVM Swap ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::raw("│"),
            Span::styled(wallet_info, Style::default().fg(Color::Green)),
            Span::raw("│"),
            Span::styled(format!(" Slippage: {}% ", self.slippage_bps as f64 / 100.0), Style::default().fg(Color::Yellow)),
        ]))
        .block(Block::default().borders(Borders::ALL));

        f.render_widget(header, area);
    }

    fn render_main(&mut self, f: &mut Frame, area: Rect) {
        let block = Block::default()
            .borders(Borders::ALL)
            .title(" Swap ");

        let inner = block.inner(area);
        f.render_widget(block, area);

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .margin(1)
            .constraints([
                Constraint::Length(5),   // From token
                Constraint::Length(1),   // Swap arrow
                Constraint::Length(5),   // To token
                Constraint::Length(1),   // Spacer
                Constraint::Min(6),      // Quote info
            ])
            .split(inner);

        self.render_from_token(f, chunks[0]);
        self.render_swap_arrow(f, chunks[1]);
        self.render_to_token(f, chunks[2]);
        self.render_quote(f, chunks[4]);
    }

    fn render_from_token(&self, f: &mut Frame, area: Rect) {
        let is_focused = self.focus == SwapFocus::FromToken;
        let border_color = if is_focused { Color::Cyan } else { Color::Gray };

        let token_name = self.from_token.as_ref()
            .map(|t| t.symbol.as_str())
            .unwrap_or("Select token");

        let balance = self.from_token.as_ref()
            .and_then(|t| {
                self.wallet.as_ref().and_then(|w| {
                    if t.address == "So11111111111111111111111111111111111111112" {
                        Some(w.sol_balance as f64 / 1e9)
                    } else {
                        w.token_balances.get(&t.address).map(|b| b.ui_amount)
                    }
                })
            })
            .unwrap_or(0.0);

        let block = Block::default()
            .title(" From ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(border_color));

        let inner = block.inner(area);
        f.render_widget(block, area);

        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(12),  // Token selector
                Constraint::Min(10),     // Amount input
                Constraint::Length(15),  // Balance
            ])
            .split(inner);

        // Token selector
        f.render_widget(
            Paragraph::new(token_name)
                .style(Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
            chunks[0]
        );

        // Amount input
        let amount_style = if self.focus == SwapFocus::Amount {
            Style::default().fg(Color::Cyan)
        } else {
            Style::default().fg(Color::White)
        };

        let amount_display = if self.amount_input.is_empty() {
            "0.0".to_string()
        } else {
            self.amount_input.clone()
        };

        f.render_widget(
            Paragraph::new(amount_display).style(amount_style),
            chunks[1]
        );

        // Balance
        f.render_widget(
            Paragraph::new(format!("Bal: {:.4}", balance))
                .style(Style::default().fg(Color::DarkGray))
                .alignment(Alignment::Right),
            chunks[2]
        );
    }

    fn render_swap_arrow(&self, f: &mut Frame, area: Rect) {
        f.render_widget(
            Paragraph::new("     ↕ [s] switch")
                .style(Style::default().fg(Color::DarkGray))
                .alignment(Alignment::Center),
            area
        );
    }

    fn render_to_token(&self, f: &mut Frame, area: Rect) {
        let is_focused = self.focus == SwapFocus::ToToken;
        let border_color = if is_focused { Color::Cyan } else { Color::Gray };

        let token_name = self.to_token.as_ref()
            .map(|t| t.symbol.as_str())
            .unwrap_or("Select token");

        let estimated = self.quote.as_ref()
            .map(|q| {
                let out = q.out_amount.parse::<u64>().unwrap_or(0);
                let decimals = self.to_token.as_ref().map(|t| t.decimals).unwrap_or(6);
                out as f64 / 10f64.powi(decimals as i32)
            })
            .unwrap_or(0.0);

        let block = Block::default()
            .title(" To ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(border_color));

        let inner = block.inner(area);
        f.render_widget(block, area);

        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(12),
                Constraint::Min(10),
            ])
            .split(inner);

        f.render_widget(
            Paragraph::new(token_name)
                .style(Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
            chunks[0]
        );

        f.render_widget(
            Paragraph::new(format!("≈ {:.4}", estimated))
                .style(Style::default().fg(Color::Green)),
            chunks[1]
        );
    }

    fn render_quote(&self, f: &mut Frame, area: Rect) {
        let block = Block::default()
            .title(" Quote ")
            .borders(Borders::ALL);

        let inner = block.inner(area);
        f.render_widget(block, area);

        if self.quote_loading {
            f.render_widget(
                Paragraph::new("⏳ Fetching quote...")
                    .style(Style::default().fg(Color::Yellow)),
                inner
            );
            return;
        }

        if let Some(error) = &self.quote_error {
            f.render_widget(
                Paragraph::new(format!("❌ {}", error))
                    .style(Style::default().fg(Color::Red)),
                inner
            );
            return;
        }

        if let Some(quote) = &self.quote {
            let impact: f64 = quote.price_impact_pct.parse().unwrap_or(0.0);
            let impact_color = if impact < 0.1 {
                Color::Green
            } else if impact < 1.0 {
                Color::Yellow
            } else {
                Color::Red
            };

            let route = quote.route_plan.iter()
                .filter_map(|step| step.swap_info.label.clone())
                .collect::<Vec<_>>()
                .join(" → ");

            let lines = vec![
                Line::from(vec![
                    Span::raw("Route: "),
                    Span::styled(&route, Style::default().fg(Color::Cyan)),
                ]),
                Line::from(vec![
                    Span::raw("Price Impact: "),
                    Span::styled(format!("{:.4}%", impact), Style::default().fg(impact_color)),
                ]),
                Line::from(vec![
                    Span::raw("Min Received: "),
                    Span::styled(
                        format!("{:.4}", self.calculate_min_received()),
                        Style::default().fg(Color::White)
                    ),
                ]),
            ];

            f.render_widget(Paragraph::new(lines), inner);
        } else {
            f.render_widget(
                Paragraph::new("Enter amount to get quote")
                    .style(Style::default().fg(Color::DarkGray)),
                inner
            );
        }
    }

    fn calculate_min_received(&self) -> f64 {
        self.quote.as_ref()
            .map(|q| {
                let out = q.out_amount.parse::<u64>().unwrap_or(0);
                let decimals = self.to_token.as_ref().map(|t| t.decimals).unwrap_or(6);
                let amount = out as f64 / 10f64.powi(decimals as i32);
                amount * (1.0 - self.slippage_bps as f64 / 10000.0)
            })
            .unwrap_or(0.0)
    }

    fn render_action_bar(&self, f: &mut Frame, area: Rect) {
        let can_swap = self.quote.is_some()
            && self.from_token.is_some()
            && self.to_token.is_some()
            && self.amount_parsed.is_some();

        let swap_style = if can_swap {
            Style::default().fg(Color::Black).bg(Color::Green)
        } else {
            Style::default().fg(Color::DarkGray).bg(Color::Gray)
        };

        let actions = Line::from(vec![
            Span::styled(" [Enter] Swap ", swap_style),
            Span::raw(" │ "),
            Span::styled("[r] Refresh", Style::default().fg(Color::Cyan)),
            Span::raw(" │ "),
            Span::styled("[s] Switch", Style::default().fg(Color::Cyan)),
            Span::raw(" │ "),
            Span::styled("[m] Max", Style::default().fg(Color::Cyan)),
            Span::raw(" │ "),
            Span::styled("[q] Quit", Style::default().fg(Color::Red)),
        ]);

        f.render_widget(
            Paragraph::new(actions).alignment(Alignment::Center),
            area
        );
    }

    fn render_status_bar(&self, f: &mut Frame, area: Rect) {
        let status = if let Some(swap_status) = &self.swap_status {
            match swap_status {
                SwapStatus::Building => "Building transaction...".to_string(),
                SwapStatus::Signing => "Waiting for signature...".to_string(),
                SwapStatus::Submitting => "Submitting to network...".to_string(),
                SwapStatus::Confirming(sig) => format!("Confirming {}...", &sig[..8]),
                SwapStatus::Success(sig) => format!("✅ Success: {}", &sig[..16]),
                SwapStatus::Failed(err) => format!("❌ Failed: {}", err),
                _ => String::new(),
            }
        } else if let Some(time) = self.last_quote_time {
            format!("Quote updated {}s ago", time.elapsed().as_secs())
        } else {
            "Ready".to_string()
        };

        f.render_widget(
            Paragraph::new(status).style(Style::default().fg(Color::DarkGray)),
            area
        );
    }
}
```

---

## Week 3: Input Handling + Quote Refresh

### Day 1-2: Keyboard Input

```rust
// src/utils/tui/swap/input.rs

impl SwapApp {
    pub fn handle_key(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Char('q') | KeyCode::Esc => {
                if matches!(self.focus, SwapFocus::FromToken | SwapFocus::ToToken)
                    && !self.token_search.is_empty()
                {
                    self.token_search.clear();
                    self.token_search_results.clear();
                } else {
                    self.should_quit = true;
                }
            }
            KeyCode::Tab => {
                self.focus = match self.focus {
                    SwapFocus::FromToken => SwapFocus::Amount,
                    SwapFocus::Amount => SwapFocus::ToToken,
                    SwapFocus::ToToken => SwapFocus::SwapButton,
                    SwapFocus::SwapButton => SwapFocus::FromToken,
                    _ => self.focus,
                };
            }
            KeyCode::Char('s') => {
                // Swap from/to tokens
                std::mem::swap(&mut self.from_token, &mut self.to_token);
                self.quote = None;
                self.trigger_quote_refresh();
            }
            KeyCode::Char('r') => {
                self.trigger_quote_refresh();
            }
            KeyCode::Char('m') => {
                // Max balance
                if let (Some(token), Some(wallet)) = (&self.from_token, &self.wallet) {
                    let balance = if token.address == "So11111111111111111111111111111111111111112" {
                        // Leave some SOL for fees
                        (wallet.sol_balance.saturating_sub(10_000_000)) as f64 / 1e9
                    } else {
                        wallet.token_balances.get(&token.address)
                            .map(|b| b.ui_amount)
                            .unwrap_or(0.0)
                    };
                    self.amount_input = format!("{:.6}", balance);
                    self.amount_parsed = Some(balance);
                    self.trigger_quote_refresh();
                }
            }
            KeyCode::Enter => {
                if self.focus == SwapFocus::SwapButton && self.quote.is_some() {
                    self.execute_swap();
                }
            }
            KeyCode::Char(c) if self.focus == SwapFocus::Amount => {
                if c.is_ascii_digit() || c == '.' {
                    self.amount_input.push(c);
                    self.amount_parsed = self.amount_input.parse().ok();
                    self.trigger_quote_refresh();
                }
            }
            KeyCode::Backspace if self.focus == SwapFocus::Amount => {
                self.amount_input.pop();
                self.amount_parsed = self.amount_input.parse().ok();
                self.trigger_quote_refresh();
            }
            KeyCode::Char(c) if matches!(self.focus, SwapFocus::FromToken | SwapFocus::ToToken) => {
                self.token_search.push(c);
                self.update_token_search();
            }
            KeyCode::Backspace if matches!(self.focus, SwapFocus::FromToken | SwapFocus::ToToken) => {
                self.token_search.pop();
                self.update_token_search();
            }
            KeyCode::Up => {
                if self.selected_search_result > 0 {
                    self.selected_search_result -= 1;
                }
            }
            KeyCode::Down => {
                if self.selected_search_result < self.token_search_results.len().saturating_sub(1) {
                    self.selected_search_result += 1;
                }
            }
            KeyCode::Enter if !self.token_search_results.is_empty() => {
                let selected = self.token_search_results[self.selected_search_result].clone();
                if self.focus == SwapFocus::FromToken {
                    self.from_token = Some(selected);
                } else {
                    self.to_token = Some(selected);
                }
                self.token_search.clear();
                self.token_search_results.clear();
                self.trigger_quote_refresh();
            }
            _ => {}
        }
    }

    fn update_token_search(&mut self) {
        if let Some(tokens) = &self.tokens {
            self.token_search_results = tokens.search(&self.token_search)
                .into_iter()
                .cloned()
                .collect();
            self.selected_search_result = 0;
        }
    }

    fn trigger_quote_refresh(&mut self) {
        // Debounce - only fetch if we have valid input
        if self.amount_parsed.is_some()
            && self.from_token.is_some()
            && self.to_token.is_some()
        {
            self.quote_loading = true;
            // Actual fetch happens in async task
        }
    }
}
```

### Day 3-5: Async Quote Fetching

```rust
// src/utils/tui/swap/quote.rs

impl SwapApp {
    pub async fn fetch_quote(&mut self) -> Result<()> {
        let from = self.from_token.as_ref().ok_or(anyhow!("No from token"))?;
        let to = self.to_token.as_ref().ok_or(anyhow!("No to token"))?;
        let amount = self.amount_parsed.ok_or(anyhow!("No amount"))?;

        // Convert to smallest units
        let amount_units = (amount * 10f64.powi(from.decimals as i32)) as u64;

        let params = QuoteParams {
            input_mint: from.address.clone(),
            output_mint: to.address.clone(),
            amount: amount_units,
            slippage_bps: self.slippage_bps,
        };

        self.quote_loading = true;
        self.quote_error = None;

        match self.jupiter.get_quote(params).await {
            Ok(quote) => {
                self.quote = Some(quote);
                self.last_quote_time = Some(Instant::now());
            }
            Err(e) => {
                self.quote_error = Some(e.to_string());
            }
        }

        self.quote_loading = false;
        Ok(())
    }
}
```

---

## Week 4: Swap Execution + Polish

### Day 1-3: Transaction Execution

```rust
// src/utils/tui/swap/execute.rs

use solana_sdk::signature::Keypair;
use solana_sdk::transaction::VersionedTransaction;

impl SwapApp {
    pub async fn execute_swap(&mut self, keypair: &Keypair) -> Result<String> {
        let quote = self.quote.as_ref().ok_or(anyhow!("No quote"))?;

        self.swap_status = Some(SwapStatus::Building);

        // Get swap transaction from Jupiter
        let swap_tx = self.jupiter.get_swap_transaction(
            quote,
            &keypair.pubkey().to_string(),
        ).await?;

        self.swap_status = Some(SwapStatus::Signing);

        // Decode and sign
        let tx_bytes = base64::decode(&swap_tx.swap_transaction)?;
        let mut tx: VersionedTransaction = bincode::deserialize(&tx_bytes)?;

        tx.try_sign(&[keypair], tx.message.recent_blockhash())?;

        self.swap_status = Some(SwapStatus::Submitting);

        // Submit
        let sig = self.rpc.send_transaction(&tx).await?;
        let sig_str = sig.to_string();

        self.swap_status = Some(SwapStatus::Confirming(sig_str.clone()));

        // Wait for confirmation
        let result = self.rpc.confirm_transaction_with_spinner(
            &sig,
            swap_tx.last_valid_block_height,
            solana_sdk::commitment_config::CommitmentConfig::confirmed(),
        ).await;

        match result {
            Ok(_) => {
                self.swap_status = Some(SwapStatus::Success(sig_str.clone()));
                // Refresh wallet balances
                if let Some(wallet) = &self.wallet {
                    self.wallet = Some(fetch_wallet_balances(&self.rpc, &wallet.pubkey).await?);
                }
                Ok(sig_str)
            }
            Err(e) => {
                self.swap_status = Some(SwapStatus::Failed(e.to_string()));
                Err(e.into())
            }
        }
    }
}
```

### Day 4-5: Polish + Testing

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_token_search() {
        // ...
    }

    #[test]
    fn test_amount_parsing() {
        // ...
    }

    #[test]
    fn test_min_received_calculation() {
        // ...
    }

    #[tokio::test]
    async fn test_jupiter_quote() {
        // Mock test
    }
}
```

---

## File Structure

```
src/utils/tui/swap/
├── mod.rs           # Module exports
├── app.rs           # SwapApp state
├── jupiter.rs       # Jupiter API client
├── tokens.rs        # Token registry
├── wallet.rs        # Wallet balance fetching
├── views/
│   └── main.rs      # Main view rendering
├── input.rs         # Keyboard handling
├── quote.rs         # Quote fetching logic
├── execute.rs       # Swap execution
└── tests.rs
```

---

## CLI Entry Point

```rust
// Add to src/clparse.rs
Swap {
    /// Initial from token (symbol or mint)
    #[arg(long)]
    from: Option<String>,

    /// Initial to token (symbol or mint)
    #[arg(long)]
    to: Option<String>,

    /// Slippage in basis points (default: 50 = 0.5%)
    #[arg(long, default_value = "50")]
    slippage: u16,
},
```

---

## Success Criteria

MVP is complete when:
1. ✅ Can select from/to tokens by searching
2. ✅ Can enter swap amount
3. ✅ Quote refreshes automatically
4. ✅ Shows route and price impact
5. ✅ Can execute swap with keypair
6. ✅ Shows transaction status
7. ✅ Updates balances after swap
8. ✅ All tests pass
