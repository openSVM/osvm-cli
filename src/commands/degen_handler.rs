//! Handler for the `osvm degen` command
//!
//! Runs the autonomous trading agent for pump.fun markets.

use anyhow::Result;
use clap::ArgMatches;
use crossterm::{
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{backend::CrosstermBackend, Terminal};
use std::io;

use crate::utils::tui::degen::{DegenApp, DegenConfig};

/// Handle the degen command
pub async fn handle_degen_command(matches: &ArgMatches) -> Result<()> {
    // Parse configuration from arguments
    let config = parse_config(matches)?;

    // Check for required wallet
    if !config.dry_run {
        if load_wallet_keypair().is_none() {
            eprintln!("❌ No wallet keypair found!");
            eprintln!();
            eprintln!("To use the degen agent, you need a Solana wallet:");
            eprintln!("  1. Set SOLANA_KEYPAIR environment variable, or");
            eprintln!("  2. Have a keypair at ~/.config/solana/id.json");
            eprintln!();
            eprintln!("💡 Use --dry-run for paper trading without a wallet");
            return Ok(());
        }
    }

    // Print startup banner
    print_banner(&config);

    // Run in headless mode or TUI mode
    if config.headless {
        run_headless_mode(config).await
    } else {
        run_tui_mode(config).await
    }
}

/// Parse configuration from command line arguments
fn parse_config(matches: &ArgMatches) -> Result<DegenConfig> {
    Ok(DegenConfig {
        // Strategy
        strategy: matches
            .get_one::<String>("strategy")
            .map(|s| s.to_string())
            .unwrap_or_else(|| "momentum".to_string()),

        // Position sizing
        max_position_sol: matches
            .get_one::<String>("max-position")
            .and_then(|s| s.parse().ok())
            .unwrap_or(0.1),
        max_positions: matches
            .get_one::<String>("max-positions")
            .and_then(|s| s.parse().ok())
            .unwrap_or(5),
        min_liquidity_sol: matches
            .get_one::<String>("min-liquidity")
            .and_then(|s| s.parse().ok())
            .unwrap_or(10.0),

        // Risk management
        stop_loss_pct: matches
            .get_one::<String>("stop-loss")
            .and_then(|s| s.parse().ok())
            .unwrap_or(15.0),
        take_profit_pct: matches
            .get_one::<String>("take-profit")
            .and_then(|s| s.parse().ok())
            .unwrap_or(100.0),
        trailing_stop: matches.get_flag("trailing-stop"),
        max_daily_loss_sol: matches
            .get_one::<String>("max-daily-loss")
            .and_then(|s| s.parse().ok())
            .unwrap_or(1.0),

        // Operation modes
        dry_run: matches.get_flag("dry-run"),
        headless: matches.get_flag("headless"),
        auto_start: matches.get_flag("auto-start"),

        // Filtering
        min_holders: matches
            .get_one::<String>("min-holders")
            .and_then(|s| s.parse().ok())
            .unwrap_or(100),
        min_volume_sol: matches
            .get_one::<String>("min-volume")
            .and_then(|s| s.parse().ok())
            .unwrap_or(50.0),
        max_age_hours: matches
            .get_one::<String>("max-age")
            .and_then(|s| s.parse().ok())
            .unwrap_or(24),
        blacklist_path: matches.get_one::<String>("blacklist").cloned(),

        // Execution
        slippage_bps: matches
            .get_one::<String>("slippage")
            .and_then(|s| s.parse().ok())
            .unwrap_or(100),
        priority_fee: matches
            .get_one::<String>("priority-fee")
            .map(|s| s.to_string())
            .unwrap_or_else(|| "auto".to_string()),

        // Logging
        log_trades_path: matches.get_one::<String>("log-trades").cloned(),
        webhook_url: matches.get_one::<String>("webhook").cloned(),

        // Strategy parameters
        momentum_threshold_pct: matches
            .get_one::<String>("momentum-threshold")
            .and_then(|s| s.parse().ok())
            .unwrap_or(5.0),
        cooldown_secs: matches
            .get_one::<String>("cooldown")
            .and_then(|s| s.parse().ok())
            .unwrap_or(300),

        // Custom strategy
        strategy_file: matches.get_one::<String>("strategy-file").cloned(),
    })
}

/// Load wallet keypair from default locations
fn load_wallet_keypair() -> Option<solana_sdk::signer::keypair::Keypair> {
    let keypair_path = std::env::var("SOLANA_KEYPAIR")
        .ok()
        .or_else(|| {
            dirs::home_dir().map(|h| {
                h.join(".config/solana/id.json")
                    .to_string_lossy()
                    .to_string()
            })
        })?;

    let keypair_bytes = std::fs::read_to_string(&keypair_path).ok()?;
    let keypair_vec: Vec<u8> = serde_json::from_str(&keypair_bytes).ok()?;

    if keypair_vec.len() != 64 {
        return None;
    }

    let mut secret_key = [0u8; 32];
    secret_key.copy_from_slice(&keypair_vec[..32]);

    Some(solana_sdk::signer::keypair::Keypair::new_from_array(secret_key))
}

/// Print startup banner
fn print_banner(config: &DegenConfig) {
    println!();
    println!("╔══════════════════════════════════════════════════════════════════╗");
    println!("║                                                                  ║");
    println!("║    🎰 DEGEN MODE - Autonomous Trading Agent                      ║");
    println!("║                                                                  ║");
    println!("╚══════════════════════════════════════════════════════════════════╝");
    println!();

    if config.dry_run {
        println!("📝 Mode: PAPER TRADING (no real funds at risk)");
    } else {
        println!("⚠️  Mode: LIVE TRADING (real funds at risk!)");
    }

    println!("📈 Strategy: {}", config.strategy);
    println!("💰 Max Position: {} SOL", config.max_position_sol);
    println!("📊 Max Positions: {}", config.max_positions);
    println!("🛑 Stop Loss: {}%", config.stop_loss_pct);
    println!("🎯 Take Profit: {}%", config.take_profit_pct);

    if config.trailing_stop {
        println!("📉 Trailing Stop: Enabled");
    }

    println!("💸 Max Daily Loss: {} SOL", config.max_daily_loss_sol);
    println!();
}

/// Run in headless/daemon mode
async fn run_headless_mode(config: DegenConfig) -> Result<()> {
    use crate::services::degen_agent::DegenAgent;
    use tokio::signal;

    println!("🚀 Starting headless degen agent...");
    println!("   Press Ctrl+C to stop");
    println!();

    // Create and start the agent
    let agent = DegenAgent::new(config).await?;
    let agent_handle = agent.start().await?;

    // Wait for shutdown signal
    signal::ctrl_c().await?;

    println!("\n🛑 Shutting down...");
    agent_handle.stop().await?;

    println!("✅ Agent stopped gracefully");
    Ok(())
}

/// Run with TUI dashboard
async fn run_tui_mode(config: DegenConfig) -> Result<()> {
    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create app
    let mut app = DegenApp::new(config);

    // Initialize
    app.initialize().await?;

    // Run main loop
    let result = app.run(&mut terminal).await;

    // Restore terminal
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    result
}
