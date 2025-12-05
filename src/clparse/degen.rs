//! CLI definition for the `osvm degen` command
//!
//! Provides an autonomous trading agent for pump.fun markets.

use clap::{Arg, ArgAction, Command};

/// Build the degen subcommand
pub fn build_degen_command() -> Command {
    Command::new("degen")
        .about("Autonomous trading agent for pump.fun markets")
        .long_about(
            "Launch an autonomous trading agent that monitors markets, runs strategies, and adjusts positions.\n\n\
             ⚠️  WARNING: This is an experimental autonomous trading agent.\n\
             Real funds are at risk. Use at your own discretion.\n\n\
             Features:\n\
             • Real-time pump.fun market monitoring\n\
             • Configurable trading strategies (momentum, mean reversion, trend)\n\
             • Position sizing and risk management\n\
             • Stop-loss and take-profit automation\n\
             • Beautiful TUI dashboard for monitoring\n\n\
             Strategies:\n\
             • momentum    - Buy tokens with strong upward price movement\n\
             • mean-revert - Buy dips, sell rallies based on price deviation\n\
             • trend       - Follow established price trends\n\
             • sniper      - Fast entry on new token launches\n\n\
             Examples:\n\
               osvm degen                         # Interactive TUI dashboard\n\
               osvm degen --strategy momentum     # Run momentum strategy\n\
               osvm degen --max-position 0.1      # Max 0.1 SOL per position\n\
               osvm degen --dry-run               # Paper trading mode\n\
               osvm degen --stop-loss 10          # 10% stop-loss"
        )
        // Strategy selection
        .arg(
            Arg::new("strategy")
                .long("strategy")
                .short('s')
                .help("Trading strategy to use")
                .value_name("STRATEGY")
                .value_parser(["momentum", "mean-revert", "trend", "sniper", "custom"])
                .default_value("momentum")
        )
        // Position sizing
        .arg(
            Arg::new("max-position")
                .long("max-position")
                .help("Maximum position size in SOL")
                .value_name("SOL")
                .default_value("0.1")
        )
        .arg(
            Arg::new("max-positions")
                .long("max-positions")
                .help("Maximum number of concurrent positions")
                .value_name("COUNT")
                .default_value("5")
        )
        .arg(
            Arg::new("min-liquidity")
                .long("min-liquidity")
                .help("Minimum liquidity in SOL to consider a token")
                .value_name("SOL")
                .default_value("10")
        )
        // Risk management
        .arg(
            Arg::new("stop-loss")
                .long("stop-loss")
                .help("Stop-loss percentage (e.g., 10 for 10%)")
                .value_name("PERCENT")
                .default_value("15")
        )
        .arg(
            Arg::new("take-profit")
                .long("take-profit")
                .help("Take-profit percentage (e.g., 50 for 50%)")
                .value_name("PERCENT")
                .default_value("100")
        )
        .arg(
            Arg::new("trailing-stop")
                .long("trailing-stop")
                .help("Enable trailing stop-loss")
                .action(ArgAction::SetTrue)
        )
        .arg(
            Arg::new("max-daily-loss")
                .long("max-daily-loss")
                .help("Maximum daily loss in SOL before pausing")
                .value_name("SOL")
                .default_value("1.0")
        )
        // Operation modes
        .arg(
            Arg::new("dry-run")
                .long("dry-run")
                .help("Paper trading mode (no real trades)")
                .action(ArgAction::SetTrue)
        )
        .arg(
            Arg::new("headless")
                .long("headless")
                .help("Run without TUI (daemon mode)")
                .action(ArgAction::SetTrue)
        )
        .arg(
            Arg::new("auto-start")
                .long("auto-start")
                .help("Start trading immediately (skip confirmation)")
                .action(ArgAction::SetTrue)
        )
        // Filtering
        .arg(
            Arg::new("min-holders")
                .long("min-holders")
                .help("Minimum number of token holders")
                .value_name("COUNT")
                .default_value("100")
        )
        .arg(
            Arg::new("min-volume")
                .long("min-volume")
                .help("Minimum 24h volume in SOL")
                .value_name("SOL")
                .default_value("50")
        )
        .arg(
            Arg::new("max-age")
                .long("max-age")
                .help("Maximum token age in hours (0 = no limit)")
                .value_name("HOURS")
                .default_value("24")
        )
        .arg(
            Arg::new("blacklist")
                .long("blacklist")
                .help("Path to token blacklist file")
                .value_name("PATH")
        )
        // Slippage and execution
        .arg(
            Arg::new("slippage")
                .long("slippage")
                .help("Slippage tolerance in basis points (100 = 1%)")
                .value_name("BPS")
                .default_value("100")
        )
        .arg(
            Arg::new("priority-fee")
                .long("priority-fee")
                .help("Priority fee in lamports (or 'auto')")
                .value_name("LAMPORTS")
                .default_value("auto")
        )
        // Logging
        .arg(
            Arg::new("log-trades")
                .long("log-trades")
                .help("Log all trades to file")
                .value_name("PATH")
        )
        .arg(
            Arg::new("webhook")
                .long("webhook")
                .help("Discord/Telegram webhook URL for trade notifications")
                .value_name("URL")
        )
        // Strategy parameters
        .arg(
            Arg::new("momentum-threshold")
                .long("momentum-threshold")
                .help("Momentum strategy: price change % to trigger buy")
                .value_name("PERCENT")
                .default_value("5")
        )
        .arg(
            Arg::new("cooldown")
                .long("cooldown")
                .help("Cooldown between trades on same token (seconds)")
                .value_name("SECONDS")
                .default_value("300")
        )
        // Custom strategy file
        .arg(
            Arg::new("strategy-file")
                .long("strategy-file")
                .help("OVSM script file for custom strategy")
                .value_name("PATH")
        )
}
