//! Handler for the `osvm perp` command
//!
//! Runs the perpetual futures trading TUI for Solana DEXs.

use anyhow::Result;
use clap::ArgMatches;
use crossterm::{
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{backend::CrosstermBackend, Terminal};
use solana_sdk::signer::Signer;
use std::io;

use crate::utils::tui::perp::PerpApp;

/// Handle the perp command
pub async fn handle_perp_command(matches: &ArgMatches) -> Result<()> {
    // Check for protocol filter
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
        println!("║  📈 OSVM Perp - Perpetual Futures Trading                        ║");
        println!("╚══════════════════════════════════════════════════════════════════╝");
        println!();
        println!("⚠️  No wallet keypair found!");
        println!();
        println!("To trade perpetual futures, you need a Solana wallet:");
        println!("  1. Set SOLANA_KEYPAIR environment variable, or");
        println!("  2. Have a keypair at ~/.config/solana/id.json");
        println!();
        println!("You can still view market information in browse-only mode.");
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
    println!("║    📈 OSVM Perp - Perpetual Futures Trading                      ║");
    println!("║                                                                  ║");
    println!("║    Supported Protocols:                                          ║");
    println!("║      • Drift        • Zeta                                       ║");
    println!("║      • Mango V4     • Jupiter                                    ║");
    println!("║                                                                  ║");
    println!("╚══════════════════════════════════════════════════════════════════╝");
    println!();
}

/// List positions in non-interactive mode
async fn list_positions_mode() -> Result<()> {
    println!();
    println!("📊 Fetching perpetual positions...");
    println!();

    let mut app = PerpApp::new();
    app.initialize().await?;

    if app.positions.is_empty() {
        println!("No open positions found.");
        println!();
        println!("Use 'osvm perp' to explore markets and open positions.");
        return Ok(());
    }

    println!("┌──────────────────────────────────────────────────────────────────────────────┐");
    println!("│ Market       │ Side   │ Size     │ Entry      │ Mark       │ uPnL         │");
    println!("├──────────────────────────────────────────────────────────────────────────────┤");

    for pos in &app.positions {
        let pnl_str = if pos.unrealized_pnl >= 0.0 {
            format!("+${:.2}", pos.unrealized_pnl)
        } else {
            format!("-${:.2}", pos.unrealized_pnl.abs())
        };

        println!(
            "│ {:12} │ {:6} │ {:8.4} │ ${:9.2} │ ${:9.2} │ {:12} │",
            pos.market.symbol,
            pos.side.name(),
            pos.size,
            pos.entry_price,
            pos.mark_price,
            pnl_str
        );
    }

    println!("└──────────────────────────────────────────────────────────────────────────────┘");
    println!();
    println!(
        "Total Unrealized PnL: ${:.2} │ Total Margin: ${:.2}",
        app.total_unrealized_pnl, app.total_margin_used
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
    let mut app = PerpApp::new();

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
