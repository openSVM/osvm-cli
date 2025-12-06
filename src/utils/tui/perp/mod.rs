//! Perpetual Futures Trading TUI
//!
//! Provides a beautiful terminal dashboard for trading perpetual futures
//! on Solana DEXs including Drift, Zeta, Mango V4, and Jupiter.

pub mod app;

pub use app::{PerpApp, PerpMarket, PerpPosition, PerpProtocol, PositionSide};
