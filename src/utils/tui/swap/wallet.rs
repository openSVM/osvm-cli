//! Wallet state and balance fetching
//!
//! Handles fetching SOL and SPL token balances for the swap UI.

use anyhow::Result;
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_client::rpc_config::RpcProgramAccountsConfig;
use solana_client::rpc_filter::{Memcmp, MemcmpEncodedBytes, RpcFilterType};
use solana_sdk::pubkey::Pubkey;
use std::collections::HashMap;
use std::str::FromStr;

/// SPL Token Program ID
const TOKEN_PROGRAM_ID: &str = "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA";

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

/// Fetch wallet balances from RPC (SOL + SPL tokens)
pub async fn fetch_wallet_balances(rpc: &RpcClient, pubkey: &Pubkey) -> Result<WalletState> {
    // Get SOL balance
    let sol_balance = rpc.get_balance(pubkey).await?;

    // Fetch SPL token balances
    let token_balances = fetch_spl_token_balances(rpc, pubkey).await;

    Ok(WalletState {
        pubkey: *pubkey,
        sol_balance,
        token_balances,
    })
}

/// Fetch SPL token balances for a wallet
///
/// Uses getProgramAccounts with filters to find all token accounts owned by the wallet.
/// Returns empty map if RPC call fails - swap will still work.
async fn fetch_spl_token_balances(
    rpc: &RpcClient,
    owner: &Pubkey,
) -> HashMap<String, TokenBalance> {
    let mut balances = HashMap::new();

    // Token account structure: 32 bytes mint + 32 bytes owner + 8 bytes amount + ...
    // We filter by owner at offset 32
    let token_program = match Pubkey::from_str(TOKEN_PROGRAM_ID) {
        Ok(p) => p,
        Err(_) => return balances,
    };

    let filters = vec![
        // Filter by owner (offset 32 in token account data)
        RpcFilterType::Memcmp(Memcmp::new(
            32, // Owner starts at byte 32
            MemcmpEncodedBytes::Base58(owner.to_string()),
        )),
        // Filter by data size (165 bytes for token account)
        RpcFilterType::DataSize(165),
    ];

    let config = RpcProgramAccountsConfig {
        filters: Some(filters),
        ..Default::default()
    };

    // Fetch token accounts
    #[allow(deprecated)]
    let accounts = match rpc
        .get_program_accounts_with_config(&token_program, config)
        .await
    {
        Ok(accounts) => accounts,
        Err(e) => {
            log::warn!("Failed to fetch token accounts: {}", e);
            return balances;
        }
    };

    // Parse each token account
    for (_, account) in accounts {
        if let Some(balance) = parse_token_account(&account.data) {
            if balance.amount > 0 {
                balances.insert(balance.mint.clone(), balance);
            }
        }
    }

    log::info!("Found {} token balances", balances.len());
    balances
}

/// Parse a token account's binary data into a TokenBalance
///
/// Token account structure (165 bytes):
/// - 0..32: mint pubkey
/// - 32..64: owner pubkey
/// - 64..72: amount (u64 little-endian)
/// - 72..73: delegate option
/// - 73..105: delegate pubkey (if present)
/// - 105..113: state
/// - 113..114: is_native option
/// - 114..122: is_native value
/// - 122..130: delegated_amount
/// - 130..131: close_authority option
/// - 131..163: close_authority pubkey
fn parse_token_account(data: &[u8]) -> Option<TokenBalance> {
    if data.len() < 72 {
        return None;
    }

    // Extract mint (first 32 bytes)
    let mint = bs58::encode(&data[0..32]).into_string();

    // Extract amount (bytes 64-72, little-endian u64)
    let amount = u64::from_le_bytes(data[64..72].try_into().ok()?);

    // We don't know decimals from raw data, default to 6 (USDC-like)
    // The actual decimals will be looked up from token registry when displaying
    let decimals = 6u8;
    let ui_amount = amount as f64 / 10f64.powi(decimals as i32);

    Some(TokenBalance {
        mint,
        amount,
        decimals,
        ui_amount,
    })
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
    fn test_parse_token_account() {
        // Create a mock token account data (165 bytes)
        let mut data = vec![0u8; 165];

        // Set mint (first 32 bytes) - this will encode to some base58 string
        for i in 0..32 {
            data[i] = i as u8;
        }

        // Set owner (bytes 32-64) - not used in parsing but included for completeness
        for i in 32..64 {
            data[i] = (i - 32) as u8;
        }

        // Set amount (bytes 64-72) - 1,000,000 in little-endian
        let amount: u64 = 1_000_000;
        data[64..72].copy_from_slice(&amount.to_le_bytes());

        let result = parse_token_account(&data);
        assert!(result.is_some());

        let balance = result.unwrap();
        assert_eq!(balance.amount, 1_000_000);
        assert!(!balance.mint.is_empty());
    }

    #[test]
    fn test_parse_token_account_too_short() {
        let data = vec![0u8; 50]; // Too short
        let result = parse_token_account(&data);
        assert!(result.is_none());
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
