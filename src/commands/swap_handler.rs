//! Handler for the `osvm swap` command
//!
//! Runs the swap TUI using Jupiter aggregator.

use anyhow::Result;
use clap::ArgMatches;
use crossterm::{
    event::{self, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{backend::CrosstermBackend, Terminal};
use std::io;
use std::time::Duration;

use crate::utils::tui::swap::{InputResult, SwapApp};

/// Handle the swap command
pub async fn handle_swap_command(matches: &ArgMatches) -> Result<()> {
    // Get arguments
    let from_token = matches.get_one::<String>("from").cloned();
    let to_token = matches.get_one::<String>("to").cloned();
    let amount = matches.get_one::<String>("amount").cloned();
    let slippage_bps: u16 = matches
        .get_one::<String>("slippage")
        .and_then(|s| s.parse().ok())
        .unwrap_or(50);
    let dry_run = matches.get_flag("dry-run");

    // Get RPC URL from config
    let rpc_url = std::env::var("SOLANA_RPC_URL")
        .unwrap_or_else(|_| "https://api.mainnet-beta.solana.com".to_string());

    // Try to load wallet keypair
    let wallet_pubkey = load_wallet_pubkey();

    if dry_run {
        // Non-interactive quote mode
        return run_dry_run(&rpc_url, from_token, to_token, amount, slippage_bps).await;
    }

    // Run interactive TUI
    run_swap_tui(&rpc_url, wallet_pubkey, from_token, to_token, amount, slippage_bps).await
}

/// Load wallet public key from default keypair location
fn load_wallet_pubkey() -> Option<solana_sdk::pubkey::Pubkey> {
    use solana_sdk::signer::Signer;
    use std::str::FromStr;

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

    // The JSON file contains a 64-byte array: [secret_key(32) | public_key(32)]
    // Extract just the secret key (first 32 bytes)
    if keypair_vec.len() != 64 {
        return None;
    }
    let mut secret_key = [0u8; 32];
    secret_key.copy_from_slice(&keypair_vec[..32]);

    let keypair = solana_sdk::signer::keypair::Keypair::new_from_array(secret_key);
    Some(keypair.pubkey())
}

/// Run a dry-run quote without TUI
async fn run_dry_run(
    rpc_url: &str,
    from: Option<String>,
    to: Option<String>,
    amount: Option<String>,
    slippage_bps: u16,
) -> Result<()> {
    use crate::utils::tui::swap::{JupiterClient, QuoteParams, TokenRegistry};

    println!("🔍 Fetching quote...\n");

    // Load token registry (with fallback for offline mode)
    let tokens = TokenRegistry::load_with_fallback().await;

    // Resolve tokens
    let from_token = from
        .as_deref()
        .and_then(|s| tokens.get_by_symbol(s).or_else(|| tokens.get(s)))
        .or_else(|| tokens.get_by_symbol("SOL"))
        .ok_or_else(|| anyhow::anyhow!("Unknown from token"))?;

    let to_token = to
        .as_deref()
        .and_then(|s| tokens.get_by_symbol(s).or_else(|| tokens.get(s)))
        .or_else(|| tokens.get_by_symbol("USDC"))
        .ok_or_else(|| anyhow::anyhow!("Unknown to token"))?;

    let amount_f64: f64 = amount
        .as_deref()
        .and_then(|s| s.parse().ok())
        .unwrap_or(1.0);

    let amount_units = (amount_f64 * 10f64.powi(from_token.decimals as i32)) as u64;

    // Fetch quote
    let client = JupiterClient::new();
    let quote = client
        .get_quote(&QuoteParams {
            input_mint: from_token.address.clone(),
            output_mint: to_token.address.clone(),
            amount: amount_units,
            slippage_bps,
        })
        .await?;

    // Display results
    let out_amount: u64 = quote.out_amount.parse().unwrap_or(0);
    let out_f64 = out_amount as f64 / 10f64.powi(to_token.decimals as i32);
    let impact: f64 = quote.price_impact_pct.parse().unwrap_or(0.0);

    println!("  Swap: {} {} → {} {}", amount_f64, from_token.symbol, out_f64, to_token.symbol);
    println!();
    println!("  Route: {}",
        quote.route_plan.iter()
            .filter_map(|s| s.swap_info.label.clone())
            .collect::<Vec<_>>()
            .join(" → ")
    );
    println!("  Price Impact: {:.4}%", impact);
    println!("  Slippage: {}%", slippage_bps as f64 / 100.0);
    println!();

    if impact > 1.0 {
        println!("⚠️  Warning: High price impact!");
    }

    Ok(())
}

/// Run the interactive swap TUI
async fn run_swap_tui(
    rpc_url: &str,
    wallet_pubkey: Option<solana_sdk::pubkey::Pubkey>,
    from: Option<String>,
    to: Option<String>,
    amount: Option<String>,
    slippage_bps: u16,
) -> Result<()> {
    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create app
    let mut app = SwapApp::new(rpc_url);
    app.slippage_bps = slippage_bps;

    // Initialize
    app.initialize(wallet_pubkey.as_ref()).await?;

    // Set initial values from args
    if let Some(from_sym) = from {
        if let Some(tokens) = &app.tokens {
            if let Some(token) = tokens.get_by_symbol(&from_sym).or_else(|| tokens.get(&from_sym)) {
                app.from_token = Some(token.clone());
            }
        }
    }

    if let Some(to_sym) = to {
        if let Some(tokens) = &app.tokens {
            if let Some(token) = tokens.get_by_symbol(&to_sym).or_else(|| tokens.get(&to_sym)) {
                app.to_token = Some(token.clone());
            }
        }
    }

    if let Some(amt) = amount {
        app.amount_input = amt;
        app.parse_amount();
    }

    // Main loop
    let result = run_event_loop(&mut terminal, &mut app).await;

    // Restore terminal
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    result
}

/// Run the main event loop
async fn run_event_loop(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut SwapApp,
) -> Result<()> {
    // Initial quote if we have tokens and amount
    if app.can_quote() {
        fetch_quote(app).await;
    }

    loop {
        // Render
        terminal.draw(|f| app.render(f))?;

        // Poll for events
        if event::poll(Duration::from_millis(100))? {
            if let Event::Key(key) = event::read()? {
                let result = app.handle_key(key);

                match result {
                    InputResult::Quit => break,
                    InputResult::NeedsQuote => {
                        if app.can_quote() {
                            fetch_quote(app).await;
                        }
                    }
                    InputResult::ExecuteSwap => {
                        // For now, just show a message since we'd need the full keypair
                        app.status_message = Some("Swap execution requires keypair access".to_string());
                        app.swap_status = None;
                    }
                    InputResult::Continue => {}
                }
            }
        }

        // Check if we should quit
        if app.should_quit {
            break;
        }
    }

    Ok(())
}

/// Fetch a quote from Jupiter
async fn fetch_quote(app: &mut SwapApp) {
    if let Some(params) = app.get_quote_params() {
        app.quote_loading = true;
        app.quote_error = None;

        match app.jupiter.get_quote(&params).await {
            Ok(quote) => {
                app.quote = Some(quote);
                app.last_quote_time = Some(std::time::Instant::now());
            }
            Err(e) => {
                app.quote_error = Some(e.to_string());
            }
        }

        app.quote_loading = false;
    }
}
