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
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm swap
     Launch interactive swap UI with token search.
     💡 Search any token by name, symbol, or mint address.

  2. osvm swap --from SOL --to USDC
     Pre-select tokens for faster swapping.
     💡 Common pairs: SOL/USDC, SOL/BONK, USDC/RAY

  3. osvm swap --from SOL --to USDC --amount 1.5
     Specify exact amount to swap.
     💡 Amount is in input token units (1.5 SOL in this case).

  4. osvm swap --slippage 100
     Set 1% slippage tolerance (100 basis points).
     💡 Default is 50 bps (0.5%). Increase for volatile tokens.

  5. osvm swap --dry-run --from SOL --to BONK --amount 1
     Get quote without executing (preview mode).
     💡 See exact output amount, price impact, and route.

  6. osvm swap --from EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v --to SOL
     Use mint addresses for precise token selection.
     💡 Required for tokens with duplicate symbols.

  7. osvm swap --slippage 200 --from SOL --to <new_token>
     Higher slippage for low-liquidity tokens.
     💡 New memecoins often need 2-5% slippage.

  8. osvm swap --from USDC --to SOL --amount 100
     Swap stablecoin back to SOL.
     💡 USDC has 6 decimals, so 100 = 100 USDC.

  9. osvm swap && osvm balance
     Swap and immediately check new balance.
     💡 Chain commands for quick workflows.

 10. RUST_LOG=debug osvm swap
     Enable debug logging for troubleshooting.
     💡 See full Jupiter API requests/responses.

💡 PRO TIPS:
  • Jupiter aggregates 20+ DEXs for best pricing
  • Price impact shows how much your trade moves the market
  • "Route" shows which DEXs/pools are used
  • Always verify the output amount before confirming
  • Use --dry-run first for large trades to check impact

UNDERSTANDING SLIPPAGE:
  • 50 bps  = 0.5% = Safe for liquid pairs (SOL/USDC)
  • 100 bps = 1%   = Normal for medium liquidity
  • 300 bps = 3%   = High, use for memecoins
  • 500 bps = 5%   = Very high, only for new tokens
"#)
}
