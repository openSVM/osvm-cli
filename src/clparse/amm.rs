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
}
