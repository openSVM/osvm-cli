//! Handler for the `osvm amm` command
//!
//! Runs the AMM liquidity management TUI for Solana DEXs.

use anyhow::Result;
use clap::ArgMatches;
use crossterm::{
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{backend::CrosstermBackend, Terminal};
use solana_sdk::signer::Signer;
use std::io;

use crate::utils::tui::amm::AmmApp;

/// Handle the amm command
pub async fn handle_amm_command(matches: &ArgMatches) -> Result<()> {
    // Check for specific subcommands first
    if let Some(protocol) = matches.get_one::<String>("protocol") {
        println!("Filtering by protocol: {}", protocol);
    }

    // Check for list-only mode
    if matches.get_flag("list") {
        return list_positions_mode().await;
    }

    // Check for wallet
    if load_wallet_address().is_none() {
        println!();
        println!("╔══════════════════════════════════════════════════════════════════╗");
        println!("║  💧 OSVM AMM - Liquidity Management                              ║");
        println!("╚══════════════════════════════════════════════════════════════════╝");
        println!();
        println!("⚠️  No wallet keypair found!");
        println!();
        println!("To manage liquidity positions, you need a Solana wallet:");
        println!("  1. Set SOLANA_KEYPAIR environment variable, or");
        println!("  2. Have a keypair at ~/.config/solana/id.json");
        println!();
        println!("You can still view pool information in browse-only mode.");
        println!();
    }

    // Print banner
    print_banner();

    // Run TUI
    run_tui_mode().await
}

/// Load wallet address from default locations
fn load_wallet_address() -> Option<String> {
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

    // Derive pubkey from secret key
    use solana_sdk::signer::keypair::Keypair;
    let mut secret_key = [0u8; 32];
    secret_key.copy_from_slice(&keypair_vec[..32]);
    let keypair = Keypair::new_from_array(secret_key);

    Some(keypair.pubkey().to_string())
}

/// Print startup banner
fn print_banner() {
    println!();
    println!("╔══════════════════════════════════════════════════════════════════╗");
    println!("║                                                                  ║");
    println!("║    💧 OSVM AMM - Liquidity Management Dashboard                  ║");
    println!("║                                                                  ║");
    println!("║    Supported Protocols:                                          ║");
    println!("║      • Raydium     • Orca                                        ║");
    println!("║      • Meteora     • Lifinity                                    ║");
    println!("║                                                                  ║");
    println!("╚══════════════════════════════════════════════════════════════════╝");
    println!();
}

/// List positions in non-interactive mode
async fn list_positions_mode() -> Result<()> {
    println!();
    println!("📊 Fetching liquidity positions...");
    println!();

    // Create app just to fetch data
    let mut app = AmmApp::new();
    app.initialize().await?;

    if app.positions.is_empty() {
        println!("No liquidity positions found.");
        println!();
        println!("Use 'osvm amm' to explore pools and add liquidity.");
        return Ok(());
    }

    println!("┌─────────────────────────────────────────────────────────────────────────┐");
    println!("│ Pool          │ Protocol │ Value     │ Share   │ PnL      │ Fees      │");
    println!("├─────────────────────────────────────────────────────────────────────────┤");

    for pos in &app.positions {
        let pair = format!("{}/{}", pos.pool.token_a.symbol, pos.pool.token_b.symbol);
        let pnl_str = if pos.pnl_usd >= 0.0 {
            format!("+${:.2}", pos.pnl_usd)
        } else {
            format!("-${:.2}", pos.pnl_usd.abs())
        };

        println!(
            "│ {:13} │ {:8} │ ${:8.2} │ {:6.4}% │ {:8} │ ${:7.2} │",
            pair,
            pos.pool.protocol.name(),
            pos.value_usd,
            pos.share_pct,
            pnl_str,
            pos.fees_earned_usd
        );
    }

    println!("└─────────────────────────────────────────────────────────────────────────┘");
    println!();
    println!(
        "Total Value: ${:.2} │ Total Fees: ${:.2} │ Avg APR: {:.1}%",
        app.total_value_usd, app.total_fees_earned, app.avg_apr
    );
    println!();

    Ok(())
}

/// Run with TUI dashboard
async fn run_tui_mode() -> Result<()> {
    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create app
    let mut app = AmmApp::new();

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
