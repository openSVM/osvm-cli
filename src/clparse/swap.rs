//! CLI definition for the `osvm swap` command
//!
//! Provides a terminal UI for token swaps via Jupiter aggregator.

use clap::{Arg, ArgAction, Command};

/// Build the swap subcommand
pub fn build_swap_command() -> Command {
    Command::new("swap")
        .about("Swap tokens via Jupiter aggregator")
        .long_about(
            "Launch a terminal UI for swapping tokens on Solana using Jupiter aggregator.\n\n\
             Features:\n\
             • Search and select any token\n\
             • Real-time quotes with route visualization\n\
             • Price impact warnings\n\
             • Slippage protection\n\n\
             Examples:\n\
               osvm swap                      # Interactive swap UI\n\
               osvm swap --from SOL --to USDC # Pre-select tokens\n\
               osvm swap --amount 1.5         # Start with amount"
        )
        .arg(
            Arg::new("from")
                .long("from")
                .short('f')
                .help("Input token symbol or mint address")
                .value_name("TOKEN")
        )
        .arg(
            Arg::new("to")
                .long("to")
                .short('t')
                .help("Output token symbol or mint address")
                .value_name("TOKEN")
        )
        .arg(
            Arg::new("amount")
                .long("amount")
                .help("Amount to swap")
                .value_name("AMOUNT")
        )
        .arg(
            Arg::new("slippage")
                .long("slippage")
                .short('s')
                .help("Slippage tolerance in basis points (default: 50 = 0.5%)")
                .value_name("BPS")
                .default_value("50")
        )
        .arg(
            Arg::new("dry-run")
                .long("dry-run")
                .help("Get quote without executing swap")
                .action(ArgAction::SetTrue)
        )
}
