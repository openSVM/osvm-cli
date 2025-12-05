//! Swap TUI module
//!
//! Provides a terminal UI for token swaps using Jupiter aggregator.

pub mod app;
pub mod input;
pub mod jupiter;
pub mod tokens;
pub mod views;
pub mod wallet;

pub use app::{SwapApp, SwapFocus, SwapStatus};
pub use input::InputResult;
pub use jupiter::{JupiterClient, QuoteParams, QuoteResponse, TokenInfo};
pub use tokens::TokenRegistry;
pub use wallet::{WalletState, fetch_wallet_balances};
