//! Token registry for swap UI
//!
//! Caches token list from Jupiter and provides fast search.

use super::jupiter::{JupiterClient, TokenInfo};
use anyhow::Result;
use std::collections::HashMap;

/// Well-known token mint addresses
pub mod well_known {
    /// Native SOL (wrapped)
    pub const SOL: &str = "So11111111111111111111111111111111111111112";
    /// USDC
    pub const USDC: &str = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v";
    /// USDT
    pub const USDT: &str = "Es9vMFrzaCERmJfrF4H2FYD4KCoNkY11McCe8BenwNYB";
    /// JUP
    pub const JUP: &str = "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN";
    /// BONK
    pub const BONK: &str = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263";
    /// RAY
    pub const RAY: &str = "4k3Dyjzvzp8eMZWUXbBCjEvwSkkk59S5iCNLY3QrkX6R";
    /// ORCA
    pub const ORCA: &str = "orcaEKTdK7LKz57vaAYr9QeNsVEPfiu6QeMU1kektZE";
    /// PYTH
    pub const PYTH: &str = "HZ1JovNiVvGrGNiiYvEozEVgZ58xaU3RKwX8eACQBCt3";
}

/// Token registry with search capabilities
pub struct TokenRegistry {
    /// All tokens by mint address
    tokens: HashMap<String, TokenInfo>,
    /// Symbol to address mapping (uppercase)
    by_symbol: HashMap<String, String>,
    /// Popular/common tokens for quick access
    popular: Vec<String>,
}

impl TokenRegistry {
    /// Create an empty registry
    pub fn new() -> Self {
        Self {
            tokens: HashMap::new(),
            by_symbol: HashMap::new(),
            popular: vec![
                well_known::SOL.to_string(),
                well_known::USDC.to_string(),
                well_known::USDT.to_string(),
                well_known::JUP.to_string(),
                well_known::BONK.to_string(),
                well_known::RAY.to_string(),
                well_known::ORCA.to_string(),
            ],
        }
    }

    /// Load token list from Jupiter API
    pub async fn load() -> Result<Self> {
        let client = JupiterClient::new();
        let tokens = client.get_token_list().await?;

        let mut registry = Self::new();
        registry.populate(tokens);
        Ok(registry)
    }

    /// Populate registry from a list of tokens
    pub fn populate(&mut self, tokens: Vec<TokenInfo>) {
        for token in tokens {
            self.by_symbol
                .insert(token.symbol.to_uppercase(), token.address.clone());
            self.tokens.insert(token.address.clone(), token);
        }
    }

    /// Search for tokens by symbol, name, or address prefix
    pub fn search(&self, query: &str) -> Vec<&TokenInfo> {
        if query.is_empty() {
            // Return popular tokens if no query
            return self
                .popular
                .iter()
                .filter_map(|addr| self.tokens.get(addr))
                .collect();
        }

        let query_upper = query.to_uppercase();
        let query_lower = query.to_lowercase();

        let mut results: Vec<(&TokenInfo, u32)> = self
            .tokens
            .values()
            .filter_map(|t| {
                let symbol_upper = t.symbol.to_uppercase();
                let name_lower = t.name.to_lowercase();

                // Exact symbol match - highest priority
                if symbol_upper == query_upper {
                    return Some((t, 0));
                }

                // Symbol starts with query
                if symbol_upper.starts_with(&query_upper) {
                    return Some((t, 1));
                }

                // Symbol contains query
                if symbol_upper.contains(&query_upper) {
                    return Some((t, 2));
                }

                // Name contains query
                if name_lower.contains(&query_lower) {
                    return Some((t, 3));
                }

                // Address starts with query
                if t.address.starts_with(query) {
                    return Some((t, 4));
                }

                None
            })
            .collect();

        // Sort by priority, then by whether it's a popular token, then alphabetically
        results.sort_by(|(a, a_prio), (b, b_prio)| {
            // First by match priority
            if a_prio != b_prio {
                return a_prio.cmp(b_prio);
            }

            // Then by popularity
            let a_pop = self.popular.contains(&a.address);
            let b_pop = self.popular.contains(&b.address);
            if a_pop != b_pop {
                return b_pop.cmp(&a_pop);
            }

            // Then alphabetically
            a.symbol.cmp(&b.symbol)
        });

        results.into_iter().map(|(t, _)| t).take(10).collect()
    }

    /// Get token by mint address
    pub fn get(&self, address: &str) -> Option<&TokenInfo> {
        self.tokens.get(address)
    }

    /// Get token by symbol (case-insensitive)
    pub fn get_by_symbol(&self, symbol: &str) -> Option<&TokenInfo> {
        self.by_symbol
            .get(&symbol.to_uppercase())
            .and_then(|addr| self.tokens.get(addr))
    }

    /// Get popular tokens
    pub fn popular(&self) -> Vec<&TokenInfo> {
        self.popular
            .iter()
            .filter_map(|addr| self.tokens.get(addr))
            .collect()
    }

    /// Total number of tokens in registry
    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    /// Check if registry is empty
    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }
}

impl Default for TokenRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_registry() -> TokenRegistry {
        let mut registry = TokenRegistry::new();
        registry.populate(vec![
            TokenInfo {
                address: well_known::SOL.to_string(),
                symbol: "SOL".to_string(),
                name: "Wrapped SOL".to_string(),
                decimals: 9,
                logo_uri: None,
                tags: None,
                daily_volume: None,
            },
            TokenInfo {
                address: well_known::USDC.to_string(),
                symbol: "USDC".to_string(),
                name: "USD Coin".to_string(),
                decimals: 6,
                logo_uri: None,
                tags: None,
                daily_volume: None,
            },
            TokenInfo {
                address: well_known::BONK.to_string(),
                symbol: "BONK".to_string(),
                name: "Bonk".to_string(),
                decimals: 5,
                logo_uri: None,
                tags: None,
                daily_volume: None,
            },
        ]);
        registry
    }

    #[test]
    fn test_search_exact_symbol() {
        let registry = create_test_registry();
        let results = registry.search("SOL");
        assert!(!results.is_empty());
        assert_eq!(results[0].symbol, "SOL");
    }

    #[test]
    fn test_search_partial_symbol() {
        let registry = create_test_registry();
        let results = registry.search("BO");
        assert!(!results.is_empty());
        assert_eq!(results[0].symbol, "BONK");
    }

    #[test]
    fn test_search_case_insensitive() {
        let registry = create_test_registry();
        let results = registry.search("usdc");
        assert!(!results.is_empty());
        assert_eq!(results[0].symbol, "USDC");
    }

    #[test]
    fn test_get_by_symbol() {
        let registry = create_test_registry();
        let token = registry.get_by_symbol("sol");
        assert!(token.is_some());
        assert_eq!(token.unwrap().address, well_known::SOL);
    }

    #[test]
    fn test_empty_search_returns_popular() {
        let registry = create_test_registry();
        let results = registry.search("");
        assert!(!results.is_empty());
    }
}
