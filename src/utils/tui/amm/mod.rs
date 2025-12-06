//! AMM Liquidity Management TUI
//!
//! Provides a beautiful terminal dashboard for managing liquidity positions
//! across Solana DEXs including Raydium, Orca, Meteora, and Lifinity.

pub mod app;

pub use app::{AmmApp, AmmProtocol, LiquidityPool, LiquidityPosition};
