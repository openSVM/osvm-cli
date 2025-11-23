/// Program aliases for popular Solana programs
/// Allows users to use friendly names instead of program IDs
use std::collections::HashMap;

lazy_static::lazy_static! {
    /// Map of program aliases to their public keys
    pub static ref PROGRAM_ALIASES: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();

        // DEXes
        m.insert("raydium", "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8");
        m.insert("raydium-v4", "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8");
        m.insert("raydium-amm", "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8");
        m.insert("orca", "9W959DqEETiGZocYWCQPaJ6sBmUzgfxXfqGeTEdp3aQP");
        m.insert("orca-whirlpool", "whirLbMiicVdio4qvUfM5KAg6Ct8VwpYzGff3uctyCc");
        m.insert("jupiter", "JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4");
        m.insert("jupiter-v6", "JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4");
        m.insert("jupiter-v4", "JUP4Fb2cqiRUcaTHdrPC8h2gNsA2ETXiPDD33WcGuJB");
        m.insert("meteora", "LBUZKhRxPF3XUpBCjp4YzTKgLccjZhTSDM9YuVaPwxo");
        m.insert("phoenix", "PhoeNiXZ8ByJGLkxNfZRnkUfjvmuYqLR89jjFHGqdXY");
        m.insert("openbook", "srmqPvymJeFKQ4zGQed1GFppgkRHL9kaELCbyksJtPX");
        m.insert("serum", "9xQeWvG816bUx9EPjHmaT23yvVM2ZWbrrpZb9PusVFin");

        // Meme/Launch Platforms
        m.insert("pumpfun", "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P");
        m.insert("pump", "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P");
        m.insert("moonshot", "MoonCVVNZFSYkqNXP6bxHLPL6QQJiMagDL3qcqUQTrG");

        // Lending
        m.insert("marinade", "MarBmsSgKXdrN1egZf5sqe1TMai9K1rChYNDJgjq7aD");
        m.insert("solend", "So1endDq2YkqhipRh3WViPa8hdiSpxWy6z3Z6tMCpAo");
        m.insert("marginfi", "MFv2hWf31Z9kbCa1snEPYctwafyhdvnV7FZnsebVacA");
        m.insert("kamino", "KLend2g3cP87fffoy8q1mQqGKjrxjC8boSyAYavgmjD");

        // NFT Marketplaces
        m.insert("magiceden", "M2mx93ekt1fmXSVkTrUL9xVFHkmME8HTUi5Cyc5aF7K");
        m.insert("tensor", "TSWAPaqyCSx2KABk68Shruf4rp7CxcNi8hAsbdwmHbN");
        m.insert("metaplex", "metaqbxxUerdq28cj1RbAWkYQm3ybzjb6a8bt518x1s");

        // Staking
        m.insert("jito", "Jito4APyf642JPZPx3hGc6WWJ8zPKtRbRs4P815Awbb");
        m.insert("sanctum", "SP12tWFxD9oJsVWNavTTBZvMbA6gkAmxtVgxdqvyvhY");
        m.insert("lido", "CrX7kMhLC3cSsXJdT7JDgqrRVWGnUpX3gfEfxxU2NVLi");

        // Token Programs
        m.insert("token", "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA");
        m.insert("spl-token", "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA");
        m.insert("token-2022", "TokenzQdBNbLqP5VEhdkAS6EPFLC1PHnBqCXEpPxuEb");
        m.insert("spl-token-2022", "TokenzQdBNbLqP5VEhdkAS6EPFLC1PHnBqCXEpPxuEb");

        // System Programs
        m.insert("system", "11111111111111111111111111111111");
        m.insert("compute-budget", "ComputeBudget111111111111111111111111111111");

        // Perpetuals
        m.insert("drift", "dRiftyHA39MWEi3m9aunc5MzRF1JYuBsbn6VPcn33UH");
        m.insert("mango", "mv3ekLzLbnVPNxjSKvqBpU3ZeZXPQdEC3bp5MDEBG68");
        m.insert("zo", "Zo1ggzTUKMY5bYnDvT5mtVeZxzf2FaLTbKkmvGUhUQk");

        m
    };

    /// Map of popular token symbols to their mint addresses
    pub static ref TOKEN_SYMBOLS: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();

        // Major tokens
        m.insert("SOL", "So11111111111111111111111111111111111111112"); // Wrapped SOL
        m.insert("USDC", "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v");
        m.insert("USDT", "Es9vMFrzaCERmJfrF4H2FYD4KCoNkY11McCe8BenwNYB");
        m.insert("wSOL", "So11111111111111111111111111111111111111112");

        // Stablecoins
        m.insert("USDS", "USDSwr9ApdHk5bvJKMjzff41FfuX8bSxdKcR81vTwcA");
        m.insert("UXD", "7kbnvuGBxxj8AG9qp8Scn56muWGaRaFqxg1FsRp3PaFT");
        m.insert("USDH", "USDH1SM1ojwWUga67PGrgFWUHibbjqMvuMaDkRJTgkX");

        // DEX Tokens
        m.insert("RAY", "4k3Dyjzvzp8eMZWUXbBCjEvwSkkk59S5iCNLY3QrkX6R");
        m.insert("ORCA", "orcaEKTdK7LKz57vaAYr9QeNsVEPfiu6QeMU1kektZE");
        m.insert("JUP", "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN");

        // Meme Tokens
        m.insert("BONK", "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263");
        m.insert("WIF", "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm");
        m.insert("POPCAT", "7GCihgDB8fe6KNjn2MYtkzZcRjQy3t9GHdC8uHYmW2hr");
        m.insert("MEW", "MEW1gQWJ3nEXg2qgERiKu7FAFj79PHvQVREQUzScPP5");

        // Liquid Staking
        m.insert("mSOL", "mSoLzYCxHdYgdzU16g5QSh3i5K3z3KZK7ytfqcJm7So");
        m.insert("stSOL", "7dHbWXmci3dT8UFYWYZweBLXgycu7Y3iL6trKn1Y7ARj");
        m.insert("jitoSOL", "J1toso1uCk3RLmjorhTtrVwY9HJ7X8V9yYac6Y7kGCPn");

        // Pyth
        m.insert("PYTH", "HZ1JovNiVvGrGNiiYvEozEVgZ58xaU3RKwX8eACQBCt3");

        m
    };
}

/// Resolve a program alias or ID to a program ID
pub fn resolve_program(input: &str) -> String {
    // Try alias first (case-insensitive)
    let lowercase = input.to_lowercase();
    if let Some(program_id) = PROGRAM_ALIASES.get(lowercase.as_str()) {
        return program_id.to_string();
    }

    // Otherwise return as-is (assume it's a program ID)
    input.to_string()
}

/// Resolve a token symbol to mint address
pub fn resolve_token(input: &str) -> Option<String> {
    // Try symbol (case-insensitive)
    let uppercase = input.to_uppercase();
    TOKEN_SYMBOLS.get(uppercase.as_str()).map(|s| s.to_string())
}

/// Resolve multiple comma-separated programs
pub fn resolve_programs(input: &str) -> String {
    input
        .split(',')
        .map(|s| resolve_program(s.trim()))
        .collect::<Vec<_>>()
        .join(",")
}

/// List all available program aliases
pub fn list_program_aliases() -> Vec<(&'static str, &'static str)> {
    let mut aliases: Vec<_> = PROGRAM_ALIASES.iter().map(|(k, v)| (*k, *v)).collect();
    aliases.sort_by_key(|(k, _)| *k);
    aliases
}

/// List all available token symbols
pub fn list_token_symbols() -> Vec<(&'static str, &'static str)> {
    let mut symbols: Vec<_> = TOKEN_SYMBOLS.iter().map(|(k, v)| (*k, *v)).collect();
    symbols.sort_by_key(|(k, _)| *k);
    symbols
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_program() {
        assert_eq!(
            resolve_program("pumpfun"),
            "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P"
        );
        assert_eq!(
            resolve_program("raydium"),
            "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8"
        );
        // Unknown returns as-is
        assert_eq!(resolve_program("unknown123"), "unknown123");
    }

    #[test]
    fn test_resolve_token() {
        assert_eq!(
            resolve_token("USDC"),
            Some("EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v".to_string())
        );
        assert_eq!(resolve_token("unknown"), None);
    }

    #[test]
    fn test_resolve_programs() {
        let result = resolve_programs("pumpfun,raydium,unknown");
        assert!(result.contains("6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P"));
        assert!(result.contains("675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8"));
    }
}
