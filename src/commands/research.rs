use anyhow::{Context, Result};
use clap::ArgMatches;
use std::sync::Arc;
use tokio::sync::Mutex;
use crate::services::{
    ai_service::AiService,
    mcp_service::McpService,
    ovsm_service::OvsmService,
    research_agent::ResearchAgent,
};
use crate::utils::mcp_bridge::McpBridgeTool;

/// Handle the research command for intelligent wallet investigation
pub async fn handle_research_command(matches: &ArgMatches) -> Result<()> {
    // Get the wallet address from command arguments
    let wallet = matches
        .get_one::<String>("wallet")
        .context("Wallet address is required")?;

    // Check if user wants simple OVSM analysis (default) or complex agent-based research
    let use_agent = matches.get_flag("agent");

    if use_agent {
        // Use the complex multi-iteration research agent
        return handle_agent_research(matches, wallet).await;
    }

    // Simple OVSM-based analysis with AI formatting
    crate::tui_log!("🔍 Analyzing wallet: {}", wallet);
    crate::tui_log!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    // Initialize services
    let ai_service = Arc::new(Mutex::new(AiService::new()));

    // Initialize OVSM with MCP tools for blockchain data access
    crate::tui_log!("🔧 Initializing OVSM with MCP tools...");
    let mut registry = {
        use crate::utils::rpc_bridge::create_rpc_registry;
        create_rpc_registry()
    };

    // Load MCP service and register tools
    let mut mcp_service = McpService::new_with_debug(false);
    let _ = mcp_service.load_config();
    let mcp_arc = Arc::new(tokio::sync::Mutex::new(mcp_service));

    // Discover and register MCP tools
    {
        let mut svc = mcp_arc.lock().await;
        let servers: Vec<String> = svc.list_servers().iter().map(|(id, _)| (*id).clone()).collect();

        for server_id in servers {
            if svc.initialize_server(&server_id).await.is_err() {
                continue;
            }

            if let Ok(tools) = svc.list_tools(&server_id).await {
                drop(svc);
                for tool in tools {
                    registry.register(McpBridgeTool::new(&tool.name, Arc::clone(&mcp_arc)));
                }
                svc = mcp_arc.lock().await;
            }
        }
    }

    let mut ovsm_service = OvsmService::with_registry(registry, false, false);
    crate::tui_log!("✅ OVSM initialized with blockchain tools");

    // Generate OVSM script for wallet analysis
    crate::tui_log!("📊 Generating OVSM analysis script...");
    let ovsm_script = generate_wallet_analysis_script(wallet);

    // Execute OVSM script
    crate::tui_log!("⚙️  Executing on-chain data analysis...");
    let raw_result = ovsm_service.execute_code(&ovsm_script)
        .context("Failed to execute OVSM analysis")?;

    // Extract ONLY the aggregated summary (not raw transfers!)
    let summary_json = extract_summary_json(&raw_result)?;

    // Format with AI service (with fallback to raw output)
    crate::tui_log!("🤖 Formatting results with AI...");
    let formatted_report = {
        let mut ai = ai_service.lock().await;
        match format_wallet_analysis(&mut ai, wallet, &summary_json).await {
            Ok(report) => report,
            Err(e) => {
                crate::tui_log!("⚠️  AI formatting failed: {}", e);
                crate::tui_log!("⚠️  AI formatting failed, showing aggregated summary");
                summary_json.clone()
            }
        }
    };

    // Display formatted report with markdown rendering (enhanced colors for blockchain data)
    use crate::utils::markdown_renderer::MarkdownRenderer;
    crate::tui_log!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    let renderer = MarkdownRenderer::with_theme();
    renderer.render(&formatted_report);
    crate::tui_log!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    crate::tui_log!("\n✅ Research completed successfully!");

    // Optionally save report to file
    if matches.get_flag("save") {
        let filename = format!("wallet_research_{}.md", wallet);
        std::fs::write(&filename, &formatted_report)?;
        crate::tui_log!("📄 Report saved to: {}", filename);
    }

    Ok(())
}

/// Handle agent-based research (complex multi-iteration)
async fn handle_agent_research(matches: &ArgMatches, wallet: &str) -> Result<()> {
    // Check if TUI mode is requested
    let use_tui = matches.get_flag("tui");

    if use_tui {
        return handle_tui_research(matches, wallet).await;
    }

    crate::tui_log!("🚀 Starting Intelligent Wallet Research for: {}", wallet);
    crate::tui_log!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    // Initialize services
    let ai_service = Arc::new(Mutex::new(AiService::new()));

    // Initialize OVSM with MCP tools
    crate::tui_log!("🔧 Initializing OVSM with MCP tools...");
    let mut registry = {
        use crate::utils::rpc_bridge::create_rpc_registry;
        create_rpc_registry()
    };

    // Load MCP service and register tools
    let mut mcp_service = McpService::new_with_debug(false);
    let _ = mcp_service.load_config();
    let mcp_arc = Arc::new(tokio::sync::Mutex::new(mcp_service));

    // Discover and register MCP tools
    {
        crate::tui_log!("🔍 DEBUG: Acquiring MCP lock...");
        let mut svc = mcp_arc.lock().await;
        crate::tui_log!("🔍 DEBUG: Lock acquired, listing servers...");
        let servers: Vec<String> = svc.list_servers().iter().map(|(id, _)| (*id).clone()).collect();
        crate::tui_log!("🔍 DEBUG: Found {} servers: {:?}", servers.len(), servers);

        for server_id in servers {
            crate::tui_log!("🔍 DEBUG: Initializing server '{}'...", server_id);
            if svc.initialize_server(&server_id).await.is_err() {
                crate::tui_log!("⚠️  DEBUG: Failed to initialize server '{}'", server_id);
                continue;
            }
            crate::tui_log!("✅ DEBUG: Server '{}' initialized", server_id);

            crate::tui_log!("🔍 DEBUG: Listing tools for server '{}'...", server_id);
            if let Ok(tools) = svc.list_tools(&server_id).await {
                crate::tui_log!("✅ DEBUG: Found {} tools for server '{}'", tools.len(), server_id);
                drop(svc);
                for tool in tools {
                    crate::tui_log!("  📋 Registering tool: {}", tool.name);
                    registry.register(McpBridgeTool::new(&tool.name, Arc::clone(&mcp_arc)));
                }
                svc = mcp_arc.lock().await;
            } else {
                crate::tui_log!("⚠️  DEBUG: Failed to list tools for server '{}'", server_id);
            }
        }
        crate::tui_log!("🔍 DEBUG: MCP initialization loop complete");
    }

    let ovsm_service = Arc::new(Mutex::new(OvsmService::with_registry(registry, false, false)));
    crate::tui_log!("✅ OVSM initialized with blockchain tools\n");

    // Create research agent with MCP service
    let agent = ResearchAgent::new(ai_service, ovsm_service, Arc::clone(&mcp_arc), wallet.to_string());

    // Check if real-time streaming mode is enabled
    // Note: --realtime is an alias for --stream, so we only check "stream"
    let use_streaming = matches.get_flag("stream");

    // Run investigation with self-evaluation
    if use_streaming {
        crate::tui_log!("\n🌊 Starting REAL-TIME streaming visualization...");
        crate::tui_log!("Graph will update progressively as data arrives.\n");
        match agent.investigate_with_streaming().await {
            Ok(_) => {
                crate::tui_log!("\n✅ Streaming investigation completed!");
                return Ok(());
            }
            Err(e) => {
                crate::tui_log!("❌ Streaming failed: {}", e);
                return Err(e.into());
            }
        }
    } else {
        crate::tui_log!("\n🔬 Initiating multi-phase investigation with AI self-evaluation...\n");
    }

    match agent.investigate().await {
        Ok(report) => {
            crate::tui_log!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            crate::tui_log!("{}", report);
            crate::tui_log!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            crate::tui_log!("\n✅ Research completed successfully!");

            // Optionally save report to file
            if matches.get_flag("save") {
                let filename = format!("wallet_research_{}.md", wallet);
                std::fs::write(&filename, &report)?;
                crate::tui_log!("📄 Report saved to: {}", filename);
            }
        }
        Err(e) => {
            crate::tui_log!("❌ Research failed: {}", e);
            return Err(e.into());
        }
    }

    Ok(())
}

/// Generate the OVSM script for wallet analysis
/// Fetches ALL transfers with pagination, then aggregates by token
fn generate_wallet_analysis_script(wallet: &str) -> String {
    format!(r#";; Wallet analysis - fetch ALL transfers and aggregate by token
(do
  (define target "{}")

  ;; Fetch ALL transfers with cursor-based pagination
  (define all_transfers [])
  (define next_cursor null)
  (define is_first_page true)
  (define prev_cursor null)
  (define max_iterations 100)
  (define iteration_count 0)

  (while (and (or is_first_page next_cursor) (< iteration_count max_iterations))
    (do
      (set! iteration_count (+ iteration_count 1))

      ;; Build request params based on whether we have a cursor
      (define params
        (if is_first_page
            {{:address target :limit 1000}}
            {{:address target :limit 1000 :beforeSignature next_cursor}}))

      ;; Fetch batch
      (define resp (get_account_transfers params))
      (define batch (get resp "data"))
      (define new_cursor (get resp "nextPageSignature"))

      ;; Append data if we got any
      (if (> (length batch) 0)
          (set! all_transfers (append all_transfers batch))
          null)

      ;; Stop if cursor hasn't changed (prevents infinite loop with same cursor)
      (if (and (not is_first_page) (= new_cursor prev_cursor))
          (set! next_cursor null)
          (do
            (set! prev_cursor next_cursor)
            (set! next_cursor new_cursor)))

      (set! is_first_page false)))

  ;; Deduplicate transfers by transaction ID + from + to (same tx can have multiple token transfers)
  (define dedup_map
    (reduce
      all_transfers
      {{}}
      (lambda (acc tx)
        (do
          (define key (+ (get tx "txId") "_" (get tx "from") "_" (get tx "to")))
          (put acc key tx)))))

  (define unique_transfers
    (map
      (entries dedup_map)
      (lambda (entry) (get entry 1))))

  ;; Split all transfers by direction
  (define all_inflows (filter unique_transfers (lambda (t) (= (get t "transferType") "IN"))))
  (define all_outflows (filter unique_transfers (lambda (t) (= (get t "transferType") "OUT"))))

  ;; Count unique tokens
  (define unique_tokens (group-by unique_transfers (lambda (tx) (get tx "mint"))))
  (define num_tokens (length (entries unique_tokens)))

  ;; Aggregate ALL inflows by sender (with token details)
  (define global_senders
    (reduce
      all_inflows
      {{}}
      (lambda (acc tx)
        (do
          (define from (get tx "from"))
          (define symbol (get tx "tokenSymbol"))
          (define amt (float (get tx "tokenAmount")))

          ;; Get or create sender record
          (define existing (get acc from))
          (define sender_record (if existing existing {{}}))

          ;; Add token amount to sender's token list
          (define token_existing (get sender_record symbol))
          (define token_current (if token_existing token_existing 0))
          (define updated_record (put sender_record symbol (+ token_current amt)))

          (put acc from updated_record)))))

  ;; Aggregate ALL outflows by receiver (with token details)
  (define global_receivers
    (reduce
      all_outflows
      {{}}
      (lambda (acc tx)
        (do
          (define to (get tx "to"))
          (define symbol (get tx "tokenSymbol"))
          (define amt (float (get tx "tokenAmount")))

          ;; Get or create receiver record
          (define existing (get acc to))
          (define receiver_record (if existing existing {{}}))

          ;; Add token amount to receiver's token list
          (define token_existing (get receiver_record symbol))
          (define token_current (if token_existing token_existing 0))
          (define updated_record (put receiver_record symbol (+ token_current amt)))

          (put acc to updated_record)))))

  ;; Convert to sorted arrays and take top 3
  (define top_senders
    (take 3
      (sort
        (entries global_senders)
        (lambda (a b) (> (length (entries (get a 1))) (length (entries (get b 1))))))))

  (define top_receivers
    (take 3
      (sort
        (entries global_receivers)
        (lambda (a b) (> (length (entries (get a 1))) (length (entries (get b 1))))))))

  ;; Return MINIMAL summary (no per-token details!)
  {{:wallet target
   :total_transfers_raw (length all_transfers)
   :total_transfers_unique (length unique_transfers)
   :num_tokens num_tokens
   :inflow_count (length all_inflows)
   :outflow_count (length all_outflows)
   :top_senders top_senders
   :top_receivers top_receivers}})
"#, wallet)
}

/// Extract compact summary JSON from OVSM result (no raw transfers!)
fn extract_summary_json(result: &ovsm::Value) -> Result<String> {
    use serde_json::json;

    let obj = result.as_object()?;

    let wallet = obj.get("wallet")
        .and_then(|v| v.as_string().ok())
        .unwrap_or("unknown");

    let total_raw = obj.get("total_transfers_raw")
        .and_then(|v| v.as_int().ok())
        .unwrap_or(0);

    let total_unique = obj.get("total_transfers_unique")
        .and_then(|v| v.as_int().ok())
        .unwrap_or(0);

    let num_tokens = obj.get("num_tokens")
        .and_then(|v| v.as_int().ok())
        .unwrap_or(0);

    let inflow_count = obj.get("inflow_count")
        .and_then(|v| v.as_int().ok())
        .unwrap_or(0);

    let outflow_count = obj.get("outflow_count")
        .and_then(|v| v.as_int().ok())
        .unwrap_or(0);

    // Extract top senders: [[address, {symbol: amount, ...}], ...]
    let top_senders_array = obj.get("top_senders")
        .ok_or_else(|| anyhow::anyhow!("Expected top_senders key"))?
        .as_array()?;

    let mut top_senders = Vec::new();
    for sender_pair in top_senders_array {
        let pair = sender_pair.as_array()?;
        if pair.len() == 2 {
            let address = pair[0].as_string().ok().unwrap_or("unknown");
            let tokens_obj = pair[1].as_object()?;

            let mut tokens = Vec::new();
            for (symbol, amount) in tokens_obj.iter() {
                if let Ok(amt) = amount.as_float() {
                    tokens.push(json!({
                        "symbol": symbol,
                        "amount": amt
                    }));
                }
            }

            top_senders.push(json!({
                "address": address,
                "tokens": tokens
            }));
        }
    }

    // Extract top receivers: [[address, {symbol: amount, ...}], ...]
    let top_receivers_array = obj.get("top_receivers")
        .ok_or_else(|| anyhow::anyhow!("Expected top_receivers key"))?
        .as_array()?;

    let mut top_receivers = Vec::new();
    for receiver_pair in top_receivers_array {
        let pair = receiver_pair.as_array()?;
        if pair.len() == 2 {
            let address = pair[0].as_string().ok().unwrap_or("unknown");
            let tokens_obj = pair[1].as_object()?;

            let mut tokens = Vec::new();
            for (symbol, amount) in tokens_obj.iter() {
                if let Ok(amt) = amount.as_float() {
                    tokens.push(json!({
                        "symbol": symbol,
                        "amount": amt
                    }));
                }
            }

            top_receivers.push(json!({
                "address": address,
                "tokens": tokens
            }));
        }
    }

    let summary = json!({
        "wallet": wallet,
        "total_transfers_raw": total_raw,
        "total_transfers_unique": total_unique,
        "num_tokens": num_tokens,
        "inflow_count": inflow_count,
        "outflow_count": outflow_count,
        "top_senders": top_senders,
        "top_receivers": top_receivers
    });

    Ok(serde_json::to_string_pretty(&summary)?)
}

/// Format the wallet analysis summary using AI
async fn format_wallet_analysis(ai_service: &mut AiService, wallet: &str, summary_json: &str) -> Result<String> {
    // System prompt for custom formatting (bypass planning mode)
    let system_prompt = r#"You are a blockchain detective presenting findings. Format the JSON data as an engaging terminal report.

CRITICAL RULES:
1. NEVER use <br> or HTML tags - use NEWLINES only
2. Keep addresses SHORT (first 35 chars + "...")
3. Be entertaining but concise

FORMAT:
🔍 WALLET INTEL: [wallet]
═══════════════════════════════════════════════════

📊 Activity Snapshot
• Total moves: X unique transfers (Y in, Z out)
• Token variety: N different tokens

💰 Top 3 Senders (money flowing IN)
┌───────────────────────────────────┬────────────────────────────────────────┐
│              Address              │              Tokens Sent               │
├───────────────────────────────────┼────────────────────────────────────────┤
│ [35 chars...]                     │ TOKEN1: 1,234.56                       │
│                                   │ TOKEN2: 789.01                         │
└───────────────────────────────────┴────────────────────────────────────────┘
(Each token on its own line, NO <br> tags!)

📤 Top 3 Receivers (money flowing OUT)
[Same table format]

Add a witty one-liner observation at the end based on the flow pattern."#.to_string();

    // Question contains the actual data
    let question = format!(r#"Wallet Address: {}

Aggregated Transfer Summary (JSON):
{}"#, wallet, summary_json);

    // Use ownPlan=true to bypass planning and use our custom system prompt directly
    ai_service.query_osvm_ai_with_options(&question, Some(system_prompt), Some(true), false).await
        .context("Failed to format analysis with AI")
}

/// Handle TUI-based research with real-time visualization
async fn handle_tui_research(matches: &ArgMatches, wallet: &str) -> Result<()> {
    use crate::utils::tui::OsvmApp;
    use crate::utils::tui::graph::TransferData;

    // Set OSVM_QUIET to suppress all console output during TUI mode
    std::env::set_var("OSVM_QUIET", "1");

    // Initialize file logger BEFORE any log output - redirects all logs to file
    let log_path = crate::utils::tui::init_file_logger()
        .unwrap_or_else(|_| "/tmp/osvm_research.log".to_string());

    // These now go to file, not stdout
    crate::tui_log!("🎨 Launching TUI for wallet: {}", wallet);
    crate::tui_log!("Log file: {}", log_path);

    // Initialize services (same as regular agent mode)
    // Use debug_mode=false to suppress HTTP request/response logging in TUI
    let ai_service = Arc::new(Mutex::new(AiService::new_with_debug(false)));

    crate::tui_log!("🔧 Initializing OVSM with MCP tools...");
    let mut registry = {
        use crate::utils::rpc_bridge::create_rpc_registry;
        create_rpc_registry()
    };

    // Load MCP service and register tools
    let mut mcp_service = McpService::new_with_debug(false);
    let _ = mcp_service.load_config();
    let mcp_arc = Arc::new(tokio::sync::Mutex::new(mcp_service));

    // Discover and register MCP tools
    {
        let mut svc = mcp_arc.lock().await;
        let servers: Vec<String> = svc.list_servers().iter().map(|(id, _)| (*id).clone()).collect();

        for server_id in servers {
            if svc.initialize_server(&server_id).await.is_err() {
                continue;
            }

            if let Ok(tools) = svc.list_tools(&server_id).await {
                drop(svc);
                for tool in tools {
                    registry.register(McpBridgeTool::new(&tool.name, Arc::clone(&mcp_arc)));
                }
                svc = mcp_arc.lock().await;
            }
        }
    }

    let ovsm_service = Arc::new(Mutex::new(OvsmService::with_registry(registry, false, false)));

    // Create TUI app
    let mut app = OsvmApp::new(wallet.to_string());

    // Set RPC URL for live blockchain queries (needed for transaction search)
    // Default to public Solana mainnet (or use SOLANA_RPC_URL env var to override)
    let rpc_url = std::env::var("SOLANA_RPC_URL")
        .unwrap_or_else(|_| "https://api.mainnet-beta.solana.com".to_string());
    app.rpc_url = Some(rpc_url);

    // (No network stats polling - we show wallet-specific analytics instead)

    // Clone Arc references for background thread
    let agent_output = Arc::clone(&app.agent_output);
    let logs = Arc::clone(&app.logs);
    let status = Arc::clone(&app.status);
    let phase = Arc::clone(&app.phase);
    let wallet_graph = app.get_graph_handle();
    let (token_volumes, transfer_events) = app.get_analytics_handles();
    let wallet_clone = wallet.to_string();
    let target_wallet_for_callback = wallet.to_string();

    // Get Tokio runtime handle for spawning async tasks
    let runtime_handle = tokio::runtime::Handle::current();

    // Spawn real research agent in background thread
    let agent_handle = std::thread::spawn(move || {
        // Block on async agent execution
        runtime_handle.block_on(async {
            // Update status
            *phase.lock().unwrap() = "INIT".to_string();
            *status.lock().unwrap() = "Starting wallet research...".to_string();

            // Add startup messages
            {
                let mut output = agent_output.lock().unwrap();
                output.push("🚀 Starting Intelligent Wallet Research...".to_string());
            }

            {
                let mut log = logs.lock().unwrap();
                log.push(format!("Investigation started for wallet: {}", wallet_clone));
                log.push("Initializing MCP service...".to_string());
            }

            *status.lock().unwrap() = "Initializing MCP tools...".to_string();

            // Create callbacks for TUI updates
            let agent_output_clone = Arc::clone(&agent_output);
            let output_callback: crate::services::research_agent::TuiCallback = Arc::new(move |msg: &str| {
                if let Ok(mut output) = agent_output_clone.lock() {
                    output.push(msg.to_string());
                    if output.len() > 500 {
                        output.drain(0..250);
                    }
                }
            });

            let logs_clone = Arc::clone(&logs);
            let logs_callback: crate::services::research_agent::TuiCallback = Arc::new(move |msg: &str| {
                if let Ok(mut log) = logs_clone.lock() {
                    log.push(format!("[{}] {}", chrono::Local::now().format("%H:%M:%S"), msg));
                    if log.len() > 1000 {
                        log.drain(0..500);
                    }
                }
            });

            // Create graph callback to update TUI wallet graph AND analytics
            let target_w = target_wallet_for_callback.clone();
            let graph_callback: crate::services::research_agent::GraphCallback = Arc::new(move |from: &str, to: &str, amount: f64, token: &str, timestamp: &str| {
                // Update graph
                if let Ok(mut graph) = wallet_graph.lock() {
                    use crate::utils::tui::graph::WalletNodeType;
                    graph.add_transfer(
                        from.to_string(),
                        to.to_string(),
                        amount,
                        token.to_string(),
                        WalletNodeType::Funding,
                        WalletNodeType::Recipient,
                        Some(timestamp.to_string()),
                        None,  // No signature available in this context
                    );
                }

                // Update token volumes
                if let Ok(mut vols) = token_volumes.lock() {
                    if let Some(existing) = vols.iter_mut().find(|v| v.symbol == token) {
                        existing.amount += amount;
                    } else {
                        vols.push(crate::utils::tui::app::TokenVolume {
                            symbol: token.to_string(),
                            amount,
                        });
                    }
                    // Sort by volume descending
                    vols.sort_by(|a, b| b.amount.partial_cmp(&a.amount).unwrap_or(std::cmp::Ordering::Equal));
                }

                // Update transfer events with HISTORICAL timestamp
                if let Ok(mut evts) = transfer_events.lock() {
                    let direction = if to == target_w { "IN" } else { "OUT" };
                    // Use historical timestamp if available, else current time
                    let ts = if !timestamp.is_empty() && timestamp.len() >= 10 {
                        timestamp[..10].to_string() // Just the date
                    } else {
                        chrono::Local::now().format("%Y-%m-%d").to_string()
                    };
                    evts.push(crate::utils::tui::app::TransferEvent {
                        timestamp: ts,
                        amount,
                        token: token.to_string(),
                        direction: direction.to_string(),
                    });
                    // Sort by timestamp descending (newest first) and keep last 50
                    evts.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));
                    evts.truncate(50);
                }
            });

            let tui_callbacks = crate::services::research_agent::TuiCallbacks {
                output: Some(output_callback),
                logs: Some(logs_callback),
                graph: Some(graph_callback),
            };

            // Create research agent with TUI callbacks
            let agent = ResearchAgent::new_with_callbacks(
                ai_service,
                ovsm_service,
                Arc::clone(&mcp_arc),
                wallet_clone.clone(),
                tui_callbacks
            );

            // Add initialization complete message
            {
                let mut output = agent_output.lock().unwrap();
                output.push("✅ MCP tools initialized".to_string());
            }

            *phase.lock().unwrap() = "PLANNING".to_string();
            *status.lock().unwrap() = "AI generating investigation plan...".to_string();

            // Run investigation in AUTO mode (deterministic, no AI prompts)
            *phase.lock().unwrap() = "INVESTIGATING".to_string();
            *status.lock().unwrap() = "BFS graph expansion starting...".to_string();

            // Use investigate_auto for TUI mode - no AI, just deterministic graph building
            match agent.investigate_auto().await {
                Ok(report) => {
                    *phase.lock().unwrap() = "COMPLETE".to_string();
                    *status.lock().unwrap() = "Investigation complete!".to_string();

                    let mut output = agent_output.lock().unwrap();
                    output.push("✅ Investigation complete!".to_string());
                    output.push(format!("📊 Final report generated ({} chars)", report.len()));

                    let mut log = logs.lock().unwrap();
                    log.push("Investigation finished successfully".to_string());
                }
                Err(e) => {
                    *phase.lock().unwrap() = "ERROR".to_string();
                    *status.lock().unwrap() = format!("Failed: {}", e);

                    let mut output = agent_output.lock().unwrap();
                    output.push(format!("❌ Investigation failed: {}", e));

                    let mut log = logs.lock().unwrap();
                    log.push(format!("ERROR: {}", e));
                }
            }
        });
    });

    // Run the TUI in the main thread (blocking)
    app.run()?;

    // Wait for agent to finish
    let _ = agent_handle.join();

    crate::tui_log!("\n✅ TUI session ended");
    Ok(())
}

/// Run a quick test of the research agent
pub async fn run_research_demo() -> Result<()> {
    crate::tui_log!("🧪 Running Research Agent Demo");
    crate::tui_log!("================================\n");

    // Use a well-known wallet for demo
    let demo_wallet = "11111111111111111111111111111111"; // System program

    let ai_service = Arc::new(Mutex::new(AiService::new()));
    let ovsm_service = Arc::new(Mutex::new(OvsmService::new()));

    // Initialize MCP service for demo
    let mut mcp_service = McpService::new_with_debug(false);
    let _ = mcp_service.load_config();
    let mcp_arc = Arc::new(tokio::sync::Mutex::new(mcp_service));

    let agent = ResearchAgent::new(ai_service, ovsm_service, Arc::clone(&mcp_arc), demo_wallet.to_string());

    crate::tui_log!("📊 Demonstrating iterative investigation with self-evaluation:\n");

    // Run abbreviated investigation
    let report = agent.investigate().await?;

    crate::tui_log!("\n📝 Demo Report Preview:");
    crate::tui_log!("{}", &report[..report.len().min(1000)]);

    Ok(())
}