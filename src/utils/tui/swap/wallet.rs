//! Wallet state and balance fetching
//!
//! Handles fetching SOL and SPL token balances for the swap UI.

use anyhow::Result;
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::collections::HashMap;

/// Wallet state with balances
#[derive(Debug, Clone)]
pub struct WalletState {
    /// Wallet public key
    pub pubkey: Pubkey,
    /// Native SOL balance in lamports
    pub sol_balance: u64,
    /// SPL token balances by mint address
    pub token_balances: HashMap<String, TokenBalance>,
}

/// SPL token balance
#[derive(Debug, Clone)]
pub struct TokenBalance {
    /// Token mint address
    pub mint: String,
    /// Raw amount in smallest units
    pub amount: u64,
    /// Token decimals
    pub decimals: u8,
    /// UI-friendly amount (amount / 10^decimals)
    pub ui_amount: f64,
}

impl WalletState {
    /// Get balance for a specific token
    pub fn get_balance(&self, mint: &str) -> f64 {
        // Handle native SOL
        if mint == super::tokens::well_known::SOL {
            return self.sol_balance as f64 / 1e9;
        }

        self.token_balances
            .get(mint)
            .map(|b| b.ui_amount)
            .unwrap_or(0.0)
    }

    /// Get raw balance in smallest units
    pub fn get_raw_balance(&self, mint: &str, decimals: u8) -> u64 {
        // Handle native SOL
        if mint == super::tokens::well_known::SOL {
            return self.sol_balance;
        }

        self.token_balances
            .get(mint)
            .map(|b| b.amount)
            .unwrap_or(0)
    }

    /// Check if wallet has enough balance for a swap
    pub fn has_sufficient_balance(&self, mint: &str, amount: f64) -> bool {
        self.get_balance(mint) >= amount
    }
}

/// Fetch wallet balances from RPC
pub async fn fetch_wallet_balances(rpc: &RpcClient, pubkey: &Pubkey) -> Result<WalletState> {
    // Get SOL balance
    let sol_balance = rpc.get_balance(pubkey).await?;

    // For now, we'll use a simplified approach that just gets SOL balance
    // Full SPL token balance fetching requires more complex parsing
    // and isn't critical for MVP (quotes work without wallet balance)
    let token_balances = HashMap::new();

    // Note: Full implementation would use:
    // rpc.get_token_accounts_by_owner() with proper parsing
    // This is simplified for MVP to avoid SDK version compatibility issues

    Ok(WalletState {
        pubkey: *pubkey,
        sol_balance,
        token_balances,
    })
}

/// Fetch SPL token balances (simplified version)
/// Returns empty map if RPC call fails - swap will still work
pub async fn fetch_token_balances(
    _rpc: &RpcClient,
    _pubkey: &Pubkey,
) -> HashMap<String, TokenBalance> {
    // Simplified for MVP - returns empty
    // Full implementation would parse token account data
    HashMap::new()
}

/// Format a balance for display
pub fn format_balance(amount: f64, decimals: u8) -> String {
    if amount == 0.0 {
        return "0".to_string();
    }

    // Determine appropriate decimal places based on value
    let display_decimals = if amount >= 1000.0 {
        2
    } else if amount >= 1.0 {
        4
    } else if amount >= 0.0001 {
        6
    } else {
        decimals.min(8) as usize
    };

    format!("{:.prec$}", amount, prec = display_decimals)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_balance() {
        assert_eq!(format_balance(0.0, 9), "0");
        assert_eq!(format_balance(1234.5678, 9), "1234.57");
        assert_eq!(format_balance(1.23456789, 9), "1.2346");
        assert_eq!(format_balance(0.00012345, 9), "0.000123");
    }

    #[test]
    fn test_wallet_get_balance() {
        let mut balances = HashMap::new();
        balances.insert(
            "test_mint".to_string(),
            TokenBalance {
                mint: "test_mint".to_string(),
                amount: 1_000_000,
                decimals: 6,
                ui_amount: 1.0,
            },
        );

        let wallet = WalletState {
            pubkey: Pubkey::new_unique(),
            sol_balance: 5_000_000_000, // 5 SOL
            token_balances: balances,
        };

        assert_eq!(wallet.get_balance(super::super::tokens::well_known::SOL), 5.0);
        assert_eq!(wallet.get_balance("test_mint"), 1.0);
        assert_eq!(wallet.get_balance("unknown_mint"), 0.0);
    }
}
