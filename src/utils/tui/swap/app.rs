//! Swap application state
//!
//! Main state struct for the swap TUI.

use super::jupiter::{JupiterClient, QuoteParams, QuoteResponse, TokenInfo};
use super::tokens::TokenRegistry;
use super::wallet::{fetch_wallet_balances, WalletState};
use anyhow::Result;
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::sync::Arc;
use std::time::Instant;

/// Main swap application state
pub struct SwapApp {
    // === Token Selection ===
    /// From token (input)
    pub from_token: Option<TokenInfo>,
    /// To token (output)
    pub to_token: Option<TokenInfo>,

    // === Amount Input ===
    /// Raw input string
    pub amount_input: String,
    /// Parsed amount (if valid)
    pub amount_parsed: Option<f64>,

    // === Quote State ===
    /// Current quote from Jupiter
    pub quote: Option<QuoteResponse>,
    /// Whether a quote is currently being fetched
    pub quote_loading: bool,
    /// Quote error message
    pub quote_error: Option<String>,
    /// Time when last quote was fetched
    pub last_quote_time: Option<Instant>,

    // === Wallet State ===
    /// Current wallet state with balances
    pub wallet: Option<WalletState>,
    /// Whether wallet is being loaded
    pub wallet_loading: bool,

    // === Services ===
    /// Jupiter API client
    pub jupiter: JupiterClient,
    /// Token registry
    pub tokens: Option<TokenRegistry>,
    /// Solana RPC client
    pub rpc: Arc<RpcClient>,

    // === UI State ===
    /// Current focus
    pub focus: SwapFocus,
    /// Token search query
    pub token_search: String,
    /// Token search results
    pub token_search_results: Vec<TokenInfo>,
    /// Selected search result index
    pub selected_search_idx: usize,
    /// Whether token search modal is open
    pub search_modal_open: bool,

    // === Swap Execution ===
    /// Current swap status
    pub swap_status: Option<SwapStatus>,

    // === Settings ===
    /// Slippage tolerance in basis points (50 = 0.5%)
    pub slippage_bps: u16,

    // === General ===
    /// Whether app should quit
    pub should_quit: bool,
    /// Status message for status bar
    pub status_message: Option<String>,
}

/// Which UI element has focus
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwapFocus {
    /// From token selector
    FromToken,
    /// To token selector
    ToToken,
    /// Amount input field
    Amount,
    /// Swap button
    SwapButton,
    /// Settings
    Settings,
}

/// Swap execution status
#[derive(Debug, Clone)]
pub enum SwapStatus {
    /// Waiting for user confirmation
    Confirming,
    /// Building transaction
    Building,
    /// Waiting for signature
    Signing,
    /// Submitting to network
    Submitting,
    /// Waiting for on-chain confirmation
    WaitingConfirmation(String),
    /// Swap succeeded
    Success(String),
    /// Swap failed
    Failed(String),
}

impl SwapApp {
    /// Create a new swap application
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
            rpc: Arc::new(RpcClient::new(rpc_url.to_string())),
            focus: SwapFocus::Amount,
            token_search: String::new(),
            token_search_results: Vec::new(),
            selected_search_idx: 0,
            search_modal_open: false,
            swap_status: None,
            slippage_bps: 50, // 0.5% default
            should_quit: false,
            status_message: None,
        }
    }

    /// Initialize the app (load tokens, wallet)
    pub async fn initialize(&mut self, wallet_pubkey: Option<&Pubkey>) -> Result<()> {
        self.status_message = Some("Loading tokens...".to_string());

        // Load token registry
        match TokenRegistry::load().await {
            Ok(registry) => {
                // Set default tokens
                self.from_token = registry.get_by_symbol("SOL").cloned();
                self.to_token = registry.get_by_symbol("USDC").cloned();
                self.tokens = Some(registry);
            }
            Err(e) => {
                self.status_message = Some(format!("Failed to load tokens: {}", e));
                return Err(e);
            }
        }

        // Load wallet if provided
        if let Some(pubkey) = wallet_pubkey {
            self.wallet_loading = true;
            self.status_message = Some("Loading wallet...".to_string());

            match fetch_wallet_balances(&self.rpc, pubkey).await {
                Ok(wallet) => {
                    self.wallet = Some(wallet);
                }
                Err(e) => {
                    self.status_message = Some(format!("Failed to load wallet: {}", e));
                }
            }
            self.wallet_loading = false;
        }

        self.status_message = Some("Ready".to_string());
        Ok(())
    }

    /// Update token search results
    pub fn update_search(&mut self) {
        if let Some(tokens) = &self.tokens {
            self.token_search_results = tokens
                .search(&self.token_search)
                .into_iter()
                .cloned()
                .collect();
            self.selected_search_idx = 0;
        }
    }

    /// Select a token from search results
    pub fn select_search_result(&mut self) {
        if self.selected_search_idx < self.token_search_results.len() {
            let selected = self.token_search_results[self.selected_search_idx].clone();
            match self.focus {
                SwapFocus::FromToken => self.from_token = Some(selected),
                SwapFocus::ToToken => self.to_token = Some(selected),
                _ => {}
            }
            self.close_search();
            self.invalidate_quote();
        }
    }

    /// Open token search modal
    pub fn open_search(&mut self) {
        self.search_modal_open = true;
        self.token_search.clear();
        self.update_search();
    }

    /// Close token search modal
    pub fn close_search(&mut self) {
        self.search_modal_open = false;
        self.token_search.clear();
        self.token_search_results.clear();
    }

    /// Switch from/to tokens
    pub fn switch_tokens(&mut self) {
        std::mem::swap(&mut self.from_token, &mut self.to_token);
        self.invalidate_quote();
    }

    /// Set amount to max balance
    pub fn set_max_amount(&mut self) {
        if let (Some(token), Some(wallet)) = (&self.from_token, &self.wallet) {
            let mut balance = wallet.get_balance(&token.address);

            // Leave some SOL for fees if swapping SOL
            if token.address == super::tokens::well_known::SOL {
                balance = (balance - 0.01).max(0.0);
            }

            self.amount_input = format!("{:.6}", balance);
            self.amount_parsed = Some(balance);
            self.invalidate_quote();
        }
    }

    /// Parse and validate the current amount input
    pub fn parse_amount(&mut self) {
        self.amount_parsed = self.amount_input.parse::<f64>().ok().filter(|&v| v > 0.0);
    }

    /// Invalidate current quote (needs refresh)
    pub fn invalidate_quote(&mut self) {
        self.quote = None;
        self.quote_error = None;
    }

    /// Check if we can request a quote
    pub fn can_quote(&self) -> bool {
        self.from_token.is_some()
            && self.to_token.is_some()
            && self.amount_parsed.is_some()
            && self.amount_parsed.unwrap() > 0.0
    }

    /// Check if we can execute a swap
    pub fn can_swap(&self) -> bool {
        self.quote.is_some()
            && self.from_token.is_some()
            && self.to_token.is_some()
            && self.amount_parsed.is_some()
            && self.wallet.is_some()
            && self.swap_status.is_none()
    }

    /// Get quote parameters
    pub fn get_quote_params(&self) -> Option<QuoteParams> {
        let from = self.from_token.as_ref()?;
        let to = self.to_token.as_ref()?;
        let amount = self.amount_parsed?;

        // Convert to smallest units
        let amount_units = (amount * 10f64.powi(from.decimals as i32)) as u64;

        Some(QuoteParams {
            input_mint: from.address.clone(),
            output_mint: to.address.clone(),
            amount: amount_units,
            slippage_bps: self.slippage_bps,
        })
    }

    /// Get output amount from current quote
    pub fn get_output_amount(&self) -> Option<f64> {
        let quote = self.quote.as_ref()?;
        let to = self.to_token.as_ref()?;

        let out_amount: u64 = quote.out_amount.parse().ok()?;
        Some(out_amount as f64 / 10f64.powi(to.decimals as i32))
    }

    /// Get minimum received after slippage
    pub fn get_min_received(&self) -> Option<f64> {
        let output = self.get_output_amount()?;
        Some(output * (1.0 - self.slippage_bps as f64 / 10000.0))
    }

    /// Get price impact from quote
    pub fn get_price_impact(&self) -> Option<f64> {
        self.quote
            .as_ref()
            .and_then(|q| q.price_impact_pct.parse().ok())
    }

    /// Get route description from quote
    pub fn get_route_description(&self) -> Option<String> {
        self.quote.as_ref().map(|q| {
            q.route_plan
                .iter()
                .filter_map(|step| step.swap_info.label.clone())
                .collect::<Vec<_>>()
                .join(" → ")
        })
    }

    /// Get from token balance
    pub fn get_from_balance(&self) -> f64 {
        self.from_token
            .as_ref()
            .and_then(|t| self.wallet.as_ref().map(|w| w.get_balance(&t.address)))
            .unwrap_or(0.0)
    }

    /// Cycle focus to next element
    pub fn next_focus(&mut self) {
        self.focus = match self.focus {
            SwapFocus::FromToken => SwapFocus::Amount,
            SwapFocus::Amount => SwapFocus::ToToken,
            SwapFocus::ToToken => SwapFocus::SwapButton,
            SwapFocus::SwapButton => SwapFocus::Settings,
            SwapFocus::Settings => SwapFocus::FromToken,
        };
    }

    /// Cycle focus to previous element
    pub fn prev_focus(&mut self) {
        self.focus = match self.focus {
            SwapFocus::FromToken => SwapFocus::Settings,
            SwapFocus::Amount => SwapFocus::FromToken,
            SwapFocus::ToToken => SwapFocus::Amount,
            SwapFocus::SwapButton => SwapFocus::ToToken,
            SwapFocus::Settings => SwapFocus::SwapButton,
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_swap_app_creation() {
        let app = SwapApp::new("https://api.mainnet-beta.solana.com");
        assert!(app.from_token.is_none());
        assert!(app.to_token.is_none());
        assert_eq!(app.slippage_bps, 50);
        assert!(!app.should_quit);
    }

    #[test]
    fn test_focus_cycling() {
        let mut app = SwapApp::new("https://api.mainnet-beta.solana.com");
        assert_eq!(app.focus, SwapFocus::Amount);

        app.next_focus();
        assert_eq!(app.focus, SwapFocus::ToToken);

        app.next_focus();
        assert_eq!(app.focus, SwapFocus::SwapButton);

        app.prev_focus();
        assert_eq!(app.focus, SwapFocus::ToToken);
    }

    #[test]
    fn test_amount_parsing() {
        let mut app = SwapApp::new("https://api.mainnet-beta.solana.com");

        app.amount_input = "1.5".to_string();
        app.parse_amount();
        assert_eq!(app.amount_parsed, Some(1.5));

        app.amount_input = "invalid".to_string();
        app.parse_amount();
        assert_eq!(app.amount_parsed, None);

        app.amount_input = "0".to_string();
        app.parse_amount();
        assert_eq!(app.amount_parsed, None); // 0 is not valid
    }
}
