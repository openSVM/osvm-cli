//! Demo of OSVM TUI with ratatui and tui-nodes
//!
//! Run with: cargo run --example tui_demo

use osvm::utils::tui::graph::TransferData;
use osvm::utils::tui::OsvmApp;

fn main() -> anyhow::Result<()> {
    // Create TUI app with test wallet
    let test_wallet = "REVXui3vBCcsDHd7oUaiTNc885YiXT773yoD8DuFuck".to_string();
    let mut app = OsvmApp::new(test_wallet.clone());

    // Add some mock agent output (need to lock the Arc<Mutex<...>>)
    {
        let mut agent_output = app.agent_output.lock().unwrap();
        agent_output.push("🚀 Starting Intelligent Wallet Research...".to_string());
        agent_output.push("✅ MCP tools initialized (84 tools loaded)".to_string());
        agent_output.push("🔍 DEBUG: Iteration #1 - get_account_stats".to_string());
        agent_output.push("🔍 DEBUG: Iteration #2 - get_account_transfers".to_string());
        agent_output.push("✅ Transfer data fetched successfully".to_string());
    }

    // Add some mock logs
    {
        let mut logs = app.logs.lock().unwrap();
        logs.push("MCP server 'osvm-mcp' connected".to_string());
        logs.push("Brotli compression enabled".to_string());
        logs.push("Executing OVSM script for get_account_transfers".to_string());
    }

    // Set investigation stats
    app.iteration = 3;
    app.findings_count = 15;

    // Build transfer graph with mock data
    let transfers = vec![
        TransferData {
            from: test_wallet.clone(),
            to: "66SLv5SAj1NyiYiovf6mnVSh91VTdguvqRR9i57xjroc".to_string(),
            amount: 880084.64,
            token: "SLONANA".to_string(),
            is_defi: false,
            timestamp: Some("2025-11-15T03:44:07.000Z".to_string()),
            signature: Some("5abc123...".to_string()),
        },
        TransferData {
            from: "5Q544fKrFoe6tsEbD7S8EmxGTJYAKtTVhAW5Q5pge4j1".to_string(),
            to: test_wallet.clone(),
            amount: 20580360.009,
            token: "SVMAI".to_string(),
            is_defi: true,
            timestamp: Some("2025-11-02T18:37:12.000Z".to_string()),
            signature: Some("6def456...".to_string()),
        },
        TransferData {
            from: "42h8enKo2t2PJ9sjp3juGX1yP5C5TaCdAEYu2q2Wf8rg".to_string(),
            to: test_wallet.clone(),
            amount: 2000000.0,
            token: "SLON".to_string(),
            is_defi: false,
            timestamp: Some("2025-11-02T02:31:05.000Z".to_string()),
            signature: Some("7ghi789...".to_string()),
        },
    ];

    // Build the graph (need to lock the mutex)
    {
        let mut wallet_graph = app.wallet_graph.lock().unwrap();
        wallet_graph.build_from_transfers(&transfers);
    }

    // Run the TUI
    println!("🚀 OSVM TUI Demo");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Navigation:");
    println!("  Tab / Shift+Tab  - Cycle between tabs");
    println!("  1-4             - Jump to specific tab");
    println!("  q / Esc         - Quit");
    println!();
    println!("This demo shows mock blockchain investigation data.");
    println!("For real wallet analysis, use: osvm research --agent --tui <WALLET>");
    println!();
    println!("Press Enter to launch TUI...");

    let mut input = String::new();
    if let Err(e) = std::io::stdin().read_line(&mut input) {
        eprintln!("Error reading input: {}", e);
        eprintln!("Running TUI without confirmation...");
        std::thread::sleep(std::time::Duration::from_secs(2));
    }

    // Run TUI and handle errors gracefully
    match app.run() {
        Ok(_) => {
            println!("\n✅ TUI demo completed successfully!");
            Ok(())
        }
        Err(e) => {
            eprintln!("\n❌ TUI error: {}", e);
            eprintln!("\nTroubleshooting:");
            eprintln!("  - Make sure you're running in an interactive terminal");
            eprintln!("  - Try: TERM=xterm-256color cargo run --example tui_demo");
            eprintln!("  - Avoid pipes or redirects");
            Err(e)
        }
    }
}
