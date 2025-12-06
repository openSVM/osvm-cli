//! CLI definition for the `osvm perp` command
//!
//! Provides perpetual futures trading for Solana DEXs.

use clap::{Arg, ArgAction, Command};

/// Build the perp subcommand
pub fn build_perp_command() -> Command {
    Command::new("perp")
        .about("Trade perpetual futures on Solana DEXs")
        .long_about(
            "Launch an interactive TUI for trading perpetual futures on Solana.\n\n\
             ⚠️  WARNING: Leveraged trading carries significant risk.\n\
             You can lose more than your initial margin. Trade responsibly.\n\n\
             Features:\n\
             • Real-time market data with price charts\n\
             • Long and short positions with leverage\n\
             • Stop-loss and take-profit orders\n\
             • Position management and PnL tracking\n\
             • Multi-protocol support\n\n\
             Supported Protocols:\n\
             • Drift     - Leading Solana perps DEX\n\
             • Zeta      - Options and perps protocol\n\
             • Mango V4  - Margin trading and perps\n\
             • Jupiter   - Perps via Jupiter aggregator\n\n\
             Examples:\n\
               osvm perp                        # Interactive TUI dashboard\n\
               osvm perp --protocol drift       # Filter by Drift markets\n\
               osvm perp --list                 # List positions (non-interactive)\n\
               osvm perp --market SOL-PERP      # Direct to SOL perp market"
        )
        // Protocol filter
        .arg(
            Arg::new("protocol")
                .long("protocol")
                .short('p')
                .help("Filter by perp protocol")
                .value_name("PROTOCOL")
                .value_parser(["drift", "zeta", "mango", "jupiter"])
        )
        // Market selection
        .arg(
            Arg::new("market")
                .long("market")
                .short('m')
                .help("Direct to specific market")
                .value_name("MARKET")
        )
        // List mode (non-interactive)
        .arg(
            Arg::new("list")
                .long("list")
                .short('l')
                .help("List positions without TUI (non-interactive)")
                .action(ArgAction::SetTrue)
        )
        // Trade parameters (for CLI trading)
        .arg(
            Arg::new("side")
                .long("side")
                .help("Position side for CLI trading")
                .value_name("SIDE")
                .value_parser(["long", "short"])
        )
        .arg(
            Arg::new("size")
                .long("size")
                .help("Position size in base asset")
                .value_name("SIZE")
        )
        .arg(
            Arg::new("leverage")
                .long("leverage")
                .help("Leverage multiplier")
                .value_name("LEVERAGE")
                .default_value("5")
        )
        // Risk management
        .arg(
            Arg::new("stop-loss")
                .long("stop-loss")
                .help("Stop-loss price")
                .value_name("PRICE")
        )
        .arg(
            Arg::new("take-profit")
                .long("take-profit")
                .help("Take-profit price")
                .value_name("PRICE")
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

  1. osvm perp
     Launch interactive perpetual futures dashboard.
     💡 Real-time charts, positions, and order entry.

  2. osvm perp --protocol drift
     Filter to Drift markets only.
     💡 Drift is the leading Solana perps protocol.

  3. osvm perp --market SOL-PERP
     Go directly to SOL perpetual market.
     💡 Skip market selection, trade immediately.

  4. osvm perp --list
     List current positions (non-interactive).
     💡 Quick check without launching TUI.

  5. osvm perp --list --format json
     Export positions as JSON.
     💡 Integrate with trading bots.

  6. osvm perp --side long --size 1 --leverage 10 --market SOL-PERP
     CLI trading: Open 1 SOL long at 10x.
     💡 Dangerous! Only for advanced users.

  7. osvm perp --stop-loss 180 --take-profit 220 --market SOL-PERP
     Set risk parameters before trading.
     💡 Always use stop-losses with leverage!

  8. osvm perp --protocol zeta --market ETH-PERP
     Trade ETH on Zeta Markets.
     💡 Zeta offers options and perps.

  9. osvm perp --keypair ~/.trading-wallet.json
     Use dedicated trading wallet.
     💡 NEVER trade with your main wallet!

 10. osvm perp --list && osvm balance
     Check positions and available balance.
     💡 Always monitor margin health.

⚠️  EXTREME RISK WARNING:
  Perpetual futures are leveraged derivative products.
  • You can lose MORE than your initial deposit
  • Liquidation can happen suddenly in volatile markets
  • Funding rates can erode profits over time
  • NEVER trade with money you can't afford to lose

KEY CONCEPTS:
  • Long:       Profit when price goes up
  • Short:      Profit when price goes down
  • Leverage:   Amplifies gains AND losses
  • Margin:     Collateral for your position
  • Liquidation: Forced closure at max loss
  • Funding:    Periodic payment between longs/shorts

SUPPORTED PROTOCOLS:
  Drift   - Largest perps, cross-margin, insurance
  Zeta    - Options + perps, European style
  Mango   - V4 margin trading, isolated margin
  Jupiter - Aggregated perps routing
"#)
}
