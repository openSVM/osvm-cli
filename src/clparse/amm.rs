//! CLI definition for the `osvm amm` command
//!
//! Provides liquidity management for Solana AMMs.

use clap::{Arg, ArgAction, Command};

/// Build the amm subcommand
pub fn build_amm_command() -> Command {
    Command::new("amm")
        .about("Manage liquidity positions on Solana AMMs")
        .long_about(
            "Launch an interactive TUI for managing liquidity positions across Solana DEXs.\n\n\
             Features:\n\
             • View pools across Raydium, Orca, Meteora, and Lifinity\n\
             • Add and remove liquidity with real-time price impact\n\
             • Track positions, fees earned, and impermanent loss\n\
             • Filter pools by protocol, TVL, and APR\n\
             • Beautiful TUI dashboard for monitoring\n\n\
             Supported Protocols:\n\
             • Raydium    - Leading Solana AMM with concentrated liquidity\n\
             • Orca       - User-friendly DEX with Whirlpools\n\
             • Meteora    - Dynamic AMM with dynamic fees\n\
             • Lifinity   - Oracle-based AMM with reduced IL\n\n\
             Examples:\n\
               osvm amm                          # Interactive TUI dashboard\n\
               osvm amm --protocol raydium       # Filter by Raydium pools\n\
               osvm amm --list                   # List positions (non-interactive)\n\
               osvm amm --min-tvl 1000000        # Only pools with >$1M TVL"
        )
        // Protocol filter
        .arg(
            Arg::new("protocol")
                .long("protocol")
                .short('p')
                .help("Filter by AMM protocol")
                .value_name("PROTOCOL")
                .value_parser(["raydium", "orca", "meteora", "lifinity"])
        )
        // List mode (non-interactive)
        .arg(
            Arg::new("list")
                .long("list")
                .short('l')
                .help("List positions without TUI (non-interactive)")
                .action(ArgAction::SetTrue)
        )
        // Filters
        .arg(
            Arg::new("min-tvl")
                .long("min-tvl")
                .help("Minimum TVL in USD to show pool")
                .value_name("USD")
                .default_value("0")
        )
        .arg(
            Arg::new("min-apr")
                .long("min-apr")
                .help("Minimum APR percentage to show pool")
                .value_name("PERCENT")
                .default_value("0")
        )
        .arg(
            Arg::new("search")
                .long("search")
                .short('s')
                .help("Search for pools by token symbol")
                .value_name("TOKEN")
        )
        // Pool address for direct access
        .arg(
            Arg::new("pool")
                .long("pool")
                .help("Direct pool address to view/manage")
                .value_name("ADDRESS")
        )
        // Wallet override
        .arg(
            Arg::new("keypair")
                .long("keypair")
                .short('k')
                .help("Path to keypair file")
                .value_name("PATH")
        )
        // RPC endpoint
        .arg(
            Arg::new("rpc")
                .long("rpc")
                .help("RPC endpoint URL")
                .value_name("URL")
        )
        // Output format for list mode
        .arg(
            Arg::new("format")
                .long("format")
                .help("Output format for list mode")
                .value_name("FORMAT")
                .value_parser(["table", "json", "csv"])
                .default_value("table")
        )
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm amm
     Launch interactive AMM dashboard.
     💡 View pools, positions, and manage liquidity.

  2. osvm amm --protocol raydium
     Filter to only Raydium pools.
     💡 Raydium is the leading Solana AMM.

  3. osvm amm --search SOL
     Search pools containing SOL.
     💡 Find SOL/USDC, SOL/BONK, etc.

  4. osvm amm --min-tvl 1000000
     Only show pools with >$1M TVL.
     💡 Filter out low-liquidity pools.

  5. osvm amm --min-apr 50
     Only show pools with >50% APR.
     💡 Higher APR = higher risk usually.

  6. osvm amm --list --format json
     Export pool data as JSON.
     💡 Useful for analytics and scripting.

  7. osvm amm --pool <ADDRESS>
     View specific pool by address.
     💡 Direct access to pool details.

  8. osvm amm --list --format csv > pools.csv
     Export pools to CSV spreadsheet.
     💡 Analyze in Excel or Google Sheets.

  9. osvm amm --protocol orca --min-tvl 100000
     Orca pools with decent liquidity.
     💡 Combine filters for precision.

 10. osvm amm --keypair ~/.my-lp-wallet.json
     Use specific wallet for LP positions.
     💡 Keep LP funds in dedicated wallet.

💡 WHAT IS AN AMM?
  Automated Market Maker - a decentralized exchange where
  liquidity providers deposit tokens into pools. Traders
  swap against these pools, paying fees to LPs.

KEY CONCEPTS:
  • TVL: Total Value Locked (liquidity in pool)
  • APR: Annual Percentage Rate (estimated returns)
  • IL:  Impermanent Loss (risk of price divergence)
  • LP:  Liquidity Provider (you!)

SUPPORTED PROTOCOLS:
  Raydium  - Leading Solana AMM, concentrated liquidity
  Orca     - User-friendly with Whirlpools (CLMM)
  Meteora  - Dynamic fees that adjust to volatility
  Lifinity - Oracle-based, reduced impermanent loss

⚠️  RISKS:
  • Impermanent loss if prices diverge significantly
  • Smart contract risk
  • Pool can be drained in exploits
  • APR is variable, not guaranteed
"#)
}
