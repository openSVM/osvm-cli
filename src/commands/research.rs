use anyhow::{Context, Result};
use clap::ArgMatches;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::Mutex;
use crate::services::{
    ai_service::AiService,
    mcp_pool::{McpPool, McpPoolConfig},
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
    let use_tui = matches.get_flag("tui");
    let use_auto = matches.get_flag("auto");

    // --auto implies agent mode (shortcut so users don't need --agent --auto)
    if use_agent || use_tui || use_auto {
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
    let use_auto = matches.get_flag("auto");

    if use_tui {
        return handle_tui_research(matches, wallet).await;
    }

    if use_auto {
        return handle_auto_research(matches, wallet).await;
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
    // Use OSVM proxy RPC endpoint (faster and more reliable than public mainnet)
    let rpc_url = std::env::var("SOLANA_RPC_URL")
        .unwrap_or_else(|_| "https://osvm.ai/api/proxy/rpc".to_string());
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

    // Initialize chat AI integration with the runtime handle
    app.set_runtime_handle(runtime_handle.clone());

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

/// Handle autonomous CLI research (no TUI, no user input)
async fn handle_auto_research(matches: &ArgMatches, wallet: &str) -> Result<()> {
    use colored::Colorize;
    use std::collections::{HashSet, VecDeque};
    use std::io::Write;
    use crate::services::opensvm_api::OpenSvmApi;

    // Parse options
    let depth: usize = matches.get_one::<String>("depth")
        .map(|s| s.parse().unwrap_or(5))
        .unwrap_or(5);
    let max_wallets: usize = matches.get_one::<String>("max-wallets")
        .map(|s| s.parse().unwrap_or(50))
        .unwrap_or(50);
    let query = matches.get_one::<String>("query").map(|s| s.as_str());
    let output_path = matches.get_one::<String>("output").cloned();
    let save_report = matches.get_flag("save") || output_path.is_some();
    let token_filter = matches.get_one::<String>("token").cloned();

    // Print banner
    println!();
    println!("{}", "╔══════════════════════════════════════════════════════════════════════════════╗".cyan());
    println!("{} {} {}",
        "║".cyan(),
        "🔍 OSVM AUTONOMOUS INVESTIGATION".bold().white(),
        "                                      ║".cyan()
    );
    println!("{}", "╚══════════════════════════════════════════════════════════════════════════════╝".cyan());
    println!();

    // Print configuration
    println!("{} {}", "Target Wallet:".bright_yellow(), wallet.white());
    println!("{} {}", "Max Depth:".bright_yellow(), depth.to_string().white());
    println!("{} {}", "Max Wallets:".bright_yellow(), max_wallets.to_string().white());
    if let Some(q) = query {
        println!("{} {}", "Query/Hypothesis:".bright_yellow(), q.bright_magenta());
    }
    if let Some(ref token) = token_filter {
        println!("{} {} {}", "Token Filter:".bright_yellow(), token.bright_cyan(), "(tracing specific token flow)".dimmed());
    }
    println!();

    // Initialize MCP pool with dynamic scaling
    print!("{}", "⏳ Initializing MCP pool (dynamic scaling)...".dimmed());
    std::io::stdout().flush()?;

    let pool_config = McpPoolConfig {
        min_instances: 5, // Pre-warm 5 instances to match BATCH_SIZE (avoids slow dynamic scaling)
        max_instances: 6, // Scale up to 6 parallel MCP instances
        idle_timeout: std::time::Duration::from_secs(30),
        debug: false,
    };

    let mcp_pool = match McpPool::with_config(pool_config).await {
        Ok(pool) => Arc::new(pool),
        Err(e) => {
            println!("\r{} {}", "❌ MCP pool failed:".red(), e);
            return Err(e);
        }
    };

    // Get tool count from first instance for display
    let tool_count = {
        let guard = mcp_pool.acquire().await?;
        let svc = guard.service().await;
        let servers: Vec<String> = svc.list_servers().iter().map(|(id, _)| (*id).clone()).collect();
        let mut count = 0;
        for server_id in &servers {
            if let Ok(tools) = svc.list_tools(server_id).await {
                count += tools.len();
            }
        }
        count
    };
    println!("\r{} {} tools, pool ready (1-6 instances)", "✅ MCP pool:".green(), tool_count);

    // Initialize OpenSVM API for address labels
    let opensvm_api = OpenSvmApi::new();

    // BFS exploration state
    let mut discovered: HashSet<String> = HashSet::new(); // All wallets we've seen
    let mut fetched_count: usize = 0; // Wallets we've actually fetched data for
    let mut queue: VecDeque<(String, usize)> = VecDeque::new(); // (wallet, depth)
    let mut all_transfers: Vec<TransferRecord> = Vec::new();
    let mut findings: Vec<Finding> = Vec::new();

    queue.push_back((wallet.to_string(), 0));
    discovered.insert(wallet.to_string());

    println!();
    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!("{}", "📡 PHASE 1: BFS GRAPH EXPLORATION".bold().cyan());
    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!();

    let start_time = std::time::Instant::now();
    const BATCH_SIZE: usize = 5; // Fetch 5 wallets in parallel via pool

    // Create progress bar
    use indicatif::{ProgressBar, ProgressStyle};
    let progress = ProgressBar::new(max_wallets as u64);
    progress.set_style(ProgressStyle::default_bar()
        .template("{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} wallets ({eta})")
        .unwrap()
        .progress_chars("█▓░"));
    progress.set_message("BFS exploring...");

    // BFS exploration loop with parallel batching via MCP pool
    while !queue.is_empty() && fetched_count < max_wallets {
        // Pop up to BATCH_SIZE wallets from queue
        let mut batch: Vec<(String, usize)> = Vec::with_capacity(BATCH_SIZE);
        while batch.len() < BATCH_SIZE && !queue.is_empty() {
            if let Some((w, d)) = queue.pop_front() {
                if d < depth {
                    batch.push((w, d));
                }
            }
        }

        if batch.is_empty() {
            break;
        }

        // Show pool scaling status
        let stats = mcp_pool.stats().await;
        eprintln!("  {} batch of {}: {} [pool: {}]",
            "⚡ Fetching".bright_blue(),
            batch.len(),
            batch.iter().map(|(w, _)| truncate_address(w)).collect::<Vec<_>>().join(", "),
            stats
        );

        // Fetch wallets in parallel via MCP pool (auto-scales as needed!)
        let fetch_futures: Vec<_> = batch.iter()
            .map(|(wallet, _)| {
                let pool = Arc::clone(&mcp_pool);
                let w = wallet.clone();
                async move {
                    let result = fetch_wallet_via_pool(&pool, &w).await;
                    (w, result)
                }
            })
            .collect();

        let results = futures::future::join_all(fetch_futures).await;

        // Show pool stats after batch (to see scaling)
        let post_stats = mcp_pool.stats().await;
        eprintln!("  {} [pool: {}/{} instances active, {}/{} busy after batch]",
            "📈".bright_green(),
            post_stats.total_instances,
            post_stats.max_instances,
            post_stats.busy_instances,
            post_stats.total_instances
        );

        // Process results
        for ((wallet, current_depth), (_, result)) in batch.into_iter().zip(results.into_iter()) {
            fetched_count += 1;
            progress.set_position(fetched_count as u64);

            // Get annotation (quick lookup, cached)
            let annotation = opensvm_api.get_annotation(&wallet).await.ok().flatten();
            let label = annotation.as_ref().map(|a| a.label.as_str()).unwrap_or("");
            let risk = annotation.as_ref().and_then(|a| a.risk.as_deref());

            let wallet_display = if label.is_empty() {
                truncate_address(&wallet)
            } else {
                format!("{} ({})", label, truncate_address(&wallet))
            };

            let risk_indicator = match risk {
                Some("malicious") => "🚨",
                Some("suspicious") => "⚠️",
                Some("safe") => "✅",
                _ => "❓",
            };

            match result {
                Ok(txs) => {
                    let new_wallets = txs.iter()
                        .filter(|tx| {
                            let connected = if tx.direction == "IN" { &tx.from } else { &tx.to };
                            !discovered.contains(connected) && current_depth + 1 < depth
                        })
                        .count();

                    progress.println(format!("  {} [{}/{}] {} {} d={} → {} txs, +{} wallets",
                        "→".bright_blue(),
                        fetched_count,
                        max_wallets,
                        wallet_display.green(),
                        risk_indicator,
                        current_depth,
                        txs.len().to_string().bright_white(),
                        new_wallets.to_string().cyan()
                    ));

                    // Process transfers
                    for tx in &txs {
                        all_transfers.push(tx.clone());

                        let connected = if tx.direction == "IN" { &tx.from } else { &tx.to };
                        if !discovered.contains(connected) && current_depth + 1 < depth {
                            discovered.insert(connected.clone());
                            queue.push_back((connected.clone(), current_depth + 1));
                        }

                        if tx.amount > 10000.0 {
                            findings.push(Finding {
                                category: "Large Transfer".to_string(),
                                severity: "High".to_string(),
                                description: format!("{:.2} {} {} → {}",
                                    tx.amount, tx.token,
                                    truncate_address(&tx.from),
                                    truncate_address(&tx.to)
                                ),
                            });
                        }
                    }
                }
                Err(e) => {
                    progress.println(format!("  {} [{}/{}] {} {} d={} → {}",
                        "→".bright_blue(),
                        fetched_count,
                        max_wallets,
                        wallet_display,
                        risk_indicator,
                        current_depth,
                        format!("error: {}", e).red()
                    ));
                }
            }

            if fetched_count >= max_wallets {
                break;
            }
        }
    }

    progress.finish_with_message("BFS complete!");

    if !queue.is_empty() {
        println!("{}", format!("⚠️  Reached max wallets limit ({}) - {} in queue remaining",
            max_wallets, queue.len()).yellow());
    }

    let exploration_time = start_time.elapsed();

    println!();
    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!("{}", "📊 PHASE 2: ANALYSIS".bold().cyan());
    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!();

    // Calculate statistics
    let total_inflows: f64 = all_transfers.iter()
        .filter(|t| t.direction == "IN")
        .map(|t| t.amount)
        .sum();
    let total_outflows: f64 = all_transfers.iter()
        .filter(|t| t.direction == "OUT")
        .map(|t| t.amount)
        .sum();

    let unique_tokens: HashSet<_> = all_transfers.iter().map(|t| &t.token).collect();
    let unique_counterparties: HashSet<_> = all_transfers.iter()
        .flat_map(|t| vec![&t.from, &t.to])
        .collect();

    println!("📈 {}", "Statistics:".bold());
    println!("   • Wallets fetched: {}", fetched_count.to_string().bright_white());
    println!("   • Wallets discovered: {}", discovered.len().to_string().bright_white());
    println!("   • Total transfers: {}", all_transfers.len().to_string().bright_white());
    println!("   • Unique tokens: {}", unique_tokens.len().to_string().bright_white());
    println!("   • Unique counterparties: {}", unique_counterparties.len().to_string().bright_white());
    println!("   • Total inflows: {:.2}", format!("{:.2}", total_inflows).green());
    println!("   • Total outflows: {:.2}", format!("{:.2}", total_outflows).red());
    println!("   • Exploration time: {:.2}s", exploration_time.as_secs_f64());
    println!();

    // Render ASCII graph visualization (or token flow if filtering)
    if let Some(ref token) = token_filter {
        render_token_flow(wallet, &all_transfers, token);
    } else {
        // Show token summary before graph (helps users discover which tokens to trace)
        render_token_summary(&all_transfers);
        render_ascii_graph(wallet, &all_transfers);
    }

    // Print findings
    if !findings.is_empty() {
        println!("🚨 {}", "Notable Findings:".bold().yellow());
        for finding in &findings {
            let severity_color = match finding.severity.as_str() {
                "Critical" => finding.severity.bright_red(),
                "High" => finding.severity.red(),
                "Medium" => finding.severity.yellow(),
                _ => finding.severity.white(),
            };
            println!("   [{:^8}] {} - {}",
                severity_color,
                finding.category.bright_cyan(),
                finding.description
            );
        }
        println!();
    }

    // Entity clusters (related wallets likely controlled by same entity)
    let entity_clusters = detect_wallet_entities(&all_transfers);
    if !entity_clusters.is_empty() {
        println!();
        println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
        println!("🔗 {} {}", "ENTITY CLUSTERS".bold().cyan(),
            format!("({} groups of related wallets)", entity_clusters.len()).dimmed());
        println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
        println!();

        for cluster in entity_clusters.iter().take(5) {
            let confidence_bar = "█".repeat((cluster.confidence * 10.0) as usize);
            let confidence_empty = "░".repeat(10 - (cluster.confidence * 10.0) as usize);
            let conf_color = if cluster.confidence > 0.7 {
                format!("{}{}", confidence_bar.green(), confidence_empty.dimmed())
            } else if cluster.confidence > 0.4 {
                format!("{}{}", confidence_bar.yellow(), confidence_empty.dimmed())
            } else {
                format!("{}{}", confidence_bar.red(), confidence_empty.dimmed())
            };

            let type_icon = match cluster.entity_type.as_str() {
                "Bot Swarm" => "🤖",
                "Same Owner" => "👤",
                "Coordinated" => "🎯",
                "Layered Transfer" => "🧅",
                _ => "🔗",
            };

            println!("  {} {} {} [{}] {:.0}%",
                type_icon,
                cluster.entity_type.bold().cyan(),
                format!("(Entity #{})", cluster.id).dimmed(),
                conf_color,
                cluster.confidence * 100.0
            );

            // Show member wallets
            println!("    {} {}", "Wallets:".dimmed(),
                format!("{} linked", cluster.wallets.len()).white());
            for (i, wallet) in cluster.wallets.iter().take(4).enumerate() {
                let wallet_label = classify_wallet(wallet)
                    .map(|t| format!(" ({})", t).bright_blue().to_string())
                    .unwrap_or_default();
                let prefix = if i == cluster.wallets.len().min(4) - 1 { "└─" } else { "├─" };
                println!("      {} {}{}",
                    prefix.dimmed(),
                    truncate_address(wallet).white(),
                    wallet_label
                );
            }
            if cluster.wallets.len() > 4 {
                println!("      {} {} more", "...".dimmed(),
                    cluster.wallets.len() - 4);
            }

            // Show evidence
            if !cluster.evidence.is_empty() {
                println!("    {} {}", "Evidence:".dimmed(),
                    cluster.evidence.first().unwrap_or(&String::new()).dimmed());
                for ev in cluster.evidence.iter().skip(1).take(2) {
                    println!("              {}", ev.dimmed());
                }
            }
            println!();
        }

        if entity_clusters.len() > 5 {
            println!("  {} {} more clusters...", "...".dimmed(), entity_clusters.len() - 5);
            println!();
        }
    }

    // Query/hypothesis evaluation
    if let Some(q) = query {
        println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
        println!("{}", "🧠 PHASE 3: HYPOTHESIS EVALUATION".bold().cyan());
        println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
        println!();
        println!("📝 {}: {}", "Query".bright_yellow(), q.bright_white());
        println!();

        // Use AI to evaluate hypothesis
        let ai_service = Arc::new(Mutex::new(AiService::new_with_debug(false)));
        let evaluation_prompt = format!(
            "Based on this wallet investigation data, evaluate the hypothesis/query: '{}'\n\n\
            Statistics:\n\
            - Wallets explored: {}\n\
            - Total transfers: {}\n\
            - Unique tokens: {}\n\
            - Total inflows: {:.2}\n\
            - Total outflows: {:.2}\n\
            - Notable findings: {}\n\n\
            Provide a concise evaluation (3-5 sentences) with:\n\
            1. Whether the hypothesis is supported or refuted\n\
            2. Key evidence for/against\n\
            3. Confidence level (low/medium/high)",
            q,
            fetched_count,
            all_transfers.len(),
            unique_tokens.len(),
            total_inflows,
            total_outflows,
            findings.len()
        );

        print!("{}", "⏳ AI evaluating hypothesis...".dimmed());
        std::io::stdout().flush()?;

        let mut ai = ai_service.lock().await;
        match ai.query_osvm_ai_with_options(&evaluation_prompt, None, Some(true), false).await {
            Ok(evaluation) => {
                println!("\r{}", "                              ".dimmed()); // Clear line
                println!("{}", evaluation.bright_white());
            }
            Err(e) => {
                println!("\r{}", format!("AI evaluation failed: {}", e).red());
            }
        }
        println!();
    }

    // Generate report
    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!("{}", "📋 FINAL REPORT".bold().cyan());
    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!();

    let report = generate_auto_report(
        wallet,
        fetched_count,
        &discovered,
        &all_transfers,
        &findings,
        query,
        exploration_time,
    );

    // Save report if requested
    if save_report {
        let report_path = output_path.unwrap_or_else(|| {
            let home = dirs::home_dir().unwrap_or_else(|| std::path::PathBuf::from("."));
            let reports_dir = home.join(".osvm").join("reports");
            std::fs::create_dir_all(&reports_dir).ok();
            let timestamp = chrono::Local::now().format("%Y%m%d_%H%M%S");
            reports_dir.join(format!("investigation_{}_{}.md", &wallet[..8], timestamp))
                .to_string_lossy()
                .to_string()
        });

        std::fs::write(&report_path, &report)?;
        println!("{} {}", "📄 Report saved to:".green(), report_path.bright_white());
    }

    println!();
    println!("{}", "✅ Investigation complete!".bold().green());
    println!();

    Ok(())
}

/// Transfer record for CLI auto mode
#[derive(Clone, Debug)]
struct TransferRecord {
    from: String,
    to: String,
    amount: f64,
    token: String,
    direction: String,
    timestamp: Option<String>,
}

/// Finding record for CLI auto mode
#[derive(Clone, Debug)]
struct Finding {
    category: String,
    severity: String,
    description: String,
}

/// Fetch transfers for a wallet via MCP pool (parallel-safe!)
async fn fetch_wallet_via_pool(
    pool: &Arc<McpPool>,
    wallet: &str,
) -> Result<Vec<TransferRecord>> {
    // Acquire instance from pool (will scale up if all busy)
    let guard = pool.acquire().await?;

    let params = serde_json::json!({
        "address": wallet,
        "limit": 100,
        "compress": true
    });

    // Call via the pooled instance
    let result = guard.call_tool("get_account_transfers", Some(params)).await?;

    // Parse the result
    if let Some(data) = result.get("data").and_then(|d| d.as_array()) {
        let transfers: Vec<TransferRecord> = data.iter()
            .filter_map(|tx| {
                Some(TransferRecord {
                    from: tx.get("from")?.as_str()?.to_string(),
                    to: tx.get("to")?.as_str()?.to_string(),
                    amount: tx.get("tokenAmount")
                        .and_then(|v| v.as_str())
                        .and_then(|s| s.parse().ok())
                        .unwrap_or(0.0),
                    token: tx.get("tokenSymbol")
                        .and_then(|v| v.as_str())
                        .unwrap_or("SOL")
                        .to_string(),
                    direction: tx.get("transferType")
                        .and_then(|v| v.as_str())
                        .unwrap_or("OUT")
                        .to_string(),
                    timestamp: {
                        // Try multiple possible timestamp field names
                        let ts_value = tx.get("timestamp")
                            .or_else(|| tx.get("blockTime"))
                            .or_else(|| tx.get("date"));

                        ts_value.and_then(|v| {
                            // Handle multiple timestamp formats
                            if let Some(s) = v.as_str() {
                                // Try parsing as ISO 8601 date string (e.g., "2025-11-22T17:47:50.000Z")
                                if let Ok(dt) = chrono::DateTime::parse_from_rfc3339(s) {
                                    return Some(dt.timestamp().to_string());
                                }
                                // Try parsing as Unix timestamp string
                                if s.parse::<i64>().is_ok() {
                                    return Some(s.to_string());
                                }
                                // Try other common date formats
                                if let Ok(dt) = chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M:%S") {
                                    return Some(dt.and_utc().timestamp().to_string());
                                }
                                None
                            } else if let Some(n) = v.as_i64() {
                                Some(n.to_string())
                            } else if let Some(n) = v.as_f64() {
                                Some((n as i64).to_string())
                            } else {
                                None
                            }
                        })
                    },
                })
            })
            .collect();

        return Ok(transfers);
    }

    Ok(Vec::new())
}

/// Fetch transfers for a wallet via MCP (legacy, holds lock)
#[allow(dead_code)]
async fn fetch_wallet_transfers(
    mcp: &Arc<tokio::sync::Mutex<McpService>>,
    wallet: &str,
) -> Result<Vec<TransferRecord>> {
    let mut svc = mcp.lock().await;

    // Try to find a server with get_account_transfers tool
    let servers: Vec<String> = svc.list_servers().iter().map(|(id, _)| (*id).clone()).collect();

    for server_id in servers {
        if svc.initialize_server(&server_id).await.is_err() {
            continue;
        }

        // Build request
        let params = serde_json::json!({
            "address": wallet,
            "limit": 100,
            "compress": true
        });

        // Try to call get_account_transfers
        if let Ok(result) = svc.call_tool(&server_id, "get_account_transfers", Some(params.clone())).await {
            // Parse the result
            if let Some(data) = result.get("data").and_then(|d| d.as_array()) {
                let transfers: Vec<TransferRecord> = data.iter()
                    .filter_map(|tx| {
                        Some(TransferRecord {
                            from: tx.get("from")?.as_str()?.to_string(),
                            to: tx.get("to")?.as_str()?.to_string(),
                            amount: tx.get("tokenAmount")
                                .and_then(|v| v.as_str())
                                .and_then(|s| s.parse().ok())
                                .unwrap_or(0.0),
                            token: tx.get("tokenSymbol")
                                .and_then(|v| v.as_str())
                                .unwrap_or("SOL")
                                .to_string(),
                            direction: tx.get("transferType")
                                .and_then(|v| v.as_str())
                                .unwrap_or("OUT")
                                .to_string(),
                            timestamp: tx.get("timestamp")
                                .and_then(|v| v.as_str())
                                .map(|s| s.to_string()),
                        })
                    })
                    .collect();

                return Ok(transfers);
            }
        }
    }

    Ok(Vec::new()) // Return empty if no transfers found
}

/// Truncate address for display
fn truncate_address(addr: &str) -> String {
    if addr.len() > 12 {
        format!("{}...{}", &addr[..6], &addr[addr.len()-4..])
    } else {
        addr.to_string()
    }
}

/// Format large numbers with K/M/B suffixes
fn format_amount(amount: f64) -> String {
    if amount >= 1_000_000_000.0 {
        format!("{:.1}B", amount / 1_000_000_000.0)
    } else if amount >= 1_000_000.0 {
        format!("{:.1}M", amount / 1_000_000.0)
    } else if amount >= 1_000.0 {
        format!("{:.1}K", amount / 1_000.0)
    } else if amount >= 1.0 {
        format!("{:.1}", amount)
    } else {
        format!("{:.4}", amount)
    }
}

/// Get arrow style based on amount (bigger = thicker)
fn get_arrow_style(amount: f64, max_amount: f64) -> &'static str {
    let ratio = amount / max_amount;
    if ratio > 0.5 {
        "━━━▶"  // Thick arrow for big flows
    } else if ratio > 0.2 {
        "──▶"   // Medium arrow
    } else {
        "─▶"    // Thin arrow for small flows
    }
}

/// Render ASCII graph visualization of wallet flows
fn render_ascii_graph(target: &str, transfers: &[TransferRecord]) {
    use colored::Colorize;
    use std::collections::HashMap;

    // Aggregate flows by counterparty with token breakdown
    let mut inflows: HashMap<String, (f64, HashMap<String, f64>)> = HashMap::new();
    let mut outflows: HashMap<String, (f64, HashMap<String, f64>)> = HashMap::new();

    for tx in transfers {
        if tx.direction == "IN" {
            let entry = inflows.entry(tx.from.clone()).or_insert((0.0, HashMap::new()));
            entry.0 += tx.amount;
            *entry.1.entry(tx.token.clone()).or_insert(0.0) += tx.amount;
        } else {
            let entry = outflows.entry(tx.to.clone()).or_insert((0.0, HashMap::new()));
            entry.0 += tx.amount;
            *entry.1.entry(tx.token.clone()).or_insert(0.0) += tx.amount;
        }
    }

    // Sort by total amount and take top 5
    let mut top_inflows: Vec<_> = inflows.into_iter().collect();
    top_inflows.sort_by(|a, b| b.1.0.partial_cmp(&a.1.0).unwrap_or(std::cmp::Ordering::Equal));
    top_inflows.truncate(5);

    let mut top_outflows: Vec<_> = outflows.into_iter().collect();
    top_outflows.sort_by(|a, b| b.1.0.partial_cmp(&a.1.0).unwrap_or(std::cmp::Ordering::Equal));
    top_outflows.truncate(5);

    // Calculate max amounts for arrow scaling
    let max_inflow = top_inflows.first().map(|(_, (a, _))| *a).unwrap_or(1.0);
    let max_outflow = top_outflows.first().map(|(_, (a, _))| *a).unwrap_or(1.0);

    // Calculate totals
    let total_inflow: f64 = top_inflows.iter().map(|(_, (a, _))| a).sum();
    let total_outflow: f64 = top_outflows.iter().map(|(_, (a, _))| a).sum();

    let target_short = truncate_address(target);

    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!("{}", "🕸️  WALLET FLOW GRAPH".bold().cyan());
    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!();

    // Header with legend
    println!("    {}                                              {}",
        "◀── INFLOWS".bright_green().bold(),
        "OUTFLOWS ──▶".bright_red().bold()
    );
    println!();

    // Print each flow pair
    let max_lines = top_inflows.len().max(top_outflows.len()).max(1);

    for i in 0..max_lines {
        // Build left side (inflows)
        let left_line = if i < top_inflows.len() {
            let (addr, (amount, tokens)) = &top_inflows[i];
            let addr_short = truncate_address(addr);

            // Get main token (highest amount)
            let main_token = tokens.iter()
                .max_by(|a, b| a.1.partial_cmp(b.1).unwrap_or(std::cmp::Ordering::Equal))
                .map(|(t, _)| t.as_str())
                .unwrap_or("?");

            // Format token name (show full if short, truncate if long)
            let token_display = if main_token.len() <= 6 {
                main_token.to_string()
            } else if main_token == "SOL" || main_token == "USDC" || main_token == "USDT" {
                main_token.to_string()
            } else {
                format!("{}…", &main_token[..4])
            };

            let extra = if tokens.len() > 1 { format!("+{}", tokens.len() - 1) } else { String::new() };
            let arrow = get_arrow_style(*amount, max_inflow);
            let amount_str = format_amount(*amount);

            format!("{} {} {}",
                addr_short.bright_green(),
                format!("[{}{} {}]", token_display, extra, amount_str).dimmed(),
                arrow.green()
            )
        } else {
            " ".repeat(45)
        };

        // Build right side (outflows)
        let right_line = if i < top_outflows.len() {
            let (addr, (amount, tokens)) = &top_outflows[i];
            let addr_short = truncate_address(addr);

            // Get main token
            let main_token = tokens.iter()
                .max_by(|a, b| a.1.partial_cmp(b.1).unwrap_or(std::cmp::Ordering::Equal))
                .map(|(t, _)| t.as_str())
                .unwrap_or("?");

            let token_display = if main_token.len() <= 6 {
                main_token.to_string()
            } else if main_token == "SOL" || main_token == "USDC" || main_token == "USDT" {
                main_token.to_string()
            } else {
                format!("{}…", &main_token[..4])
            };

            let extra = if tokens.len() > 1 { format!("+{}", tokens.len() - 1) } else { String::new() };
            let arrow = get_arrow_style(*amount, max_outflow);
            let amount_str = format_amount(*amount);

            // Reverse arrow for outflows
            let arrow_rev = match arrow {
                "━━━▶" => "▶━━━",
                "──▶" => "▶──",
                _ => "▶─",
            };

            format!("{} {} {}",
                arrow_rev.red(),
                format!("[{}{} {}]", token_display, extra, amount_str).dimmed(),
                addr_short.bright_red()
            )
        } else {
            String::new()
        };

        // Center box (only on middle rows)
        let center = if max_lines <= 3 {
            if i == 0 {
                format!("┌{}┐", "─".repeat(14))
            } else if i == max_lines / 2 || (max_lines == 1 && i == 0) {
                if max_lines == 1 {
                    format!("│ {} │", target_short)
                } else {
                    format!("│ {} │", target_short)
                }
            } else if i == max_lines - 1 || (max_lines == 2 && i == 1) {
                format!("└{}┘", "─".repeat(14))
            } else {
                format!("│{}│", " ".repeat(14))
            }
        } else {
            let mid = max_lines / 2;
            if i == mid - 1 {
                format!("┌{}┐", "─".repeat(14))
            } else if i == mid {
                format!("│ {} │", target_short)
            } else if i == mid + 1 {
                format!("└{}┘", "─".repeat(14))
            } else {
                format!("│{}│", " ".repeat(14))
            }
        };

        println!("  {}  {}  {}", left_line, center.bright_yellow(), right_line);
    }

    println!();

    // Summary with totals
    println!("  {}",
        format!("  {} sources ({} total)    ═══════════════    {} destinations ({} total)",
            top_inflows.len(),
            format_amount(total_inflow),
            top_outflows.len(),
            format_amount(total_outflow)
        ).dimmed()
    );
    println!();
}

/// Render token summary - shows breakdown of all tokens found
fn render_token_summary(transfers: &[TransferRecord]) {
    use colored::Colorize;
    use std::collections::HashMap;

    if transfers.is_empty() {
        return;
    }

    // Aggregate by token
    let mut token_stats: HashMap<&str, (usize, f64)> = HashMap::new(); // (count, total_amount)
    for tx in transfers {
        let entry = token_stats.entry(&tx.token).or_insert((0, 0.0));
        entry.0 += 1;
        entry.1 += tx.amount;
    }

    // Sort by transfer count descending
    let mut sorted: Vec<_> = token_stats.into_iter().collect();
    sorted.sort_by(|a, b| b.1.0.cmp(&a.1.0));

    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!("{} {}", "🪙 TOKEN BREAKDOWN".bold().cyan(),
        format!("({} unique tokens)", sorted.len()).dimmed());
    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!();

    // Show top 10 tokens
    let show_count = sorted.len().min(10);
    for (i, (token, (count, total))) in sorted.iter().take(show_count).enumerate() {
        // Display token name (truncate if too long, highlight if known stablecoin)
        let token_display = if token.len() > 12 {
            format!("{}…", &token[..11])
        } else {
            (*token).to_string()
        };

        let is_stablecoin = matches!(*token, "USDC" | "USDT" | "BUSD" | "DAI" | "TUSD");
        let token_colored = if is_stablecoin {
            token_display.bright_green().bold()
        } else if *token == "SOL" {
            token_display.bright_magenta().bold()
        } else {
            token_display.bright_cyan()
        };

        // Bar visualization
        let max_bar = 20;
        let bar_len = (((*count as f64) / (sorted[0].1.0 as f64)) * max_bar as f64) as usize;
        let bar = "█".repeat(bar_len);

        println!("  {:>2}. {:<12}  {:>5} transfers  {}  {}",
            (i + 1).to_string().dimmed(),
            token_colored,
            count.to_string().white(),
            format_amount(*total).dimmed(),
            bar.bright_blue()
        );
    }

    if sorted.len() > 10 {
        println!("  {} {}", "...".dimmed(),
            format!("and {} more tokens", sorted.len() - 10).dimmed());
    }

    println!();
    println!("  {} {}",
        "💡 Tip:".bright_yellow(),
        "Use --token <NAME> to trace specific token flow".dimmed()
    );
    println!();
}

/// Render token flow visualization as a tree structure
/// Shows how a specific token moves between wallets in a hierarchical tree format
fn render_token_flow(target: &str, transfers: &[TransferRecord], token_filter: &str) {
    use colored::Colorize;
    use std::collections::{HashMap, HashSet, VecDeque};

    // Filter transfers for the specific token
    let token_lower = token_filter.to_lowercase();
    let filtered: Vec<&TransferRecord> = transfers.iter()
        .filter(|tx| {
            let tx_token_lower = tx.token.to_lowercase();
            tx_token_lower == token_lower ||
            tx_token_lower.contains(&token_lower) ||
            tx.token == token_filter
        })
        .collect();

    if filtered.is_empty() {
        println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
        println!("{} {}", "🌳 TOKEN FLOW TREE:".bold().cyan(), token_filter.bright_cyan());
        println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
        println!();
        println!("  {} No transfers found for token '{}' in the explored wallets.",
            "⚠️".yellow(), token_filter.bright_cyan());
        println!();
        return;
    }

    // Build adjacency and stats
    let mut adjacency: HashMap<&str, Vec<(&str, f64, Option<i64>)>> = HashMap::new(); // (dest, amount, timestamp)
    let mut all_wallets: HashSet<&str> = HashSet::new();
    let mut wallet_inflow: HashMap<&str, f64> = HashMap::new();
    let mut wallet_outflow: HashMap<&str, f64> = HashMap::new();
    let mut edge_count: HashMap<(&str, &str), usize> = HashMap::new();

    let token_name = filtered.first().map(|t| t.token.as_str()).unwrap_or(token_filter);

    // First pass: aggregate edges and build stats
    let mut edge_totals: HashMap<(&str, &str), f64> = HashMap::new();
    for tx in &filtered {
        all_wallets.insert(&tx.from);
        all_wallets.insert(&tx.to);
        *wallet_outflow.entry(&tx.from).or_insert(0.0) += tx.amount;
        *wallet_inflow.entry(&tx.to).or_insert(0.0) += tx.amount;

        let key = (tx.from.as_str(), tx.to.as_str());
        *edge_totals.entry(key).or_insert(0.0) += tx.amount;
        *edge_count.entry(key).or_insert(0) += 1;
    }

    // Build adjacency with aggregated amounts
    for ((from, to), total_amount) in &edge_totals {
        // Get earliest timestamp for this edge (parse from string to i64)
        let timestamp: Option<i64> = filtered.iter()
            .filter(|tx| tx.from.as_str() == *from && tx.to.as_str() == *to)
            .filter_map(|tx| tx.timestamp.as_ref())
            .filter_map(|ts| ts.parse::<i64>().ok())
            .min();
        adjacency.entry(*from).or_default().push((*to, *total_amount, timestamp));
    }

    // Sort adjacency lists by amount descending
    for (_, neighbors) in adjacency.iter_mut() {
        neighbors.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    }

    let total_flow: f64 = edge_totals.values().sum();
    let total_transfers: usize = edge_count.values().sum();
    let unique_pairs = edge_totals.len();

    // Header
    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!("{} {} {}", "🌳 TOKEN FLOW TREE:".bold().cyan(), token_name.bright_cyan().bold(),
        format!("({} transfers, {} pairs, {} total)", total_transfers, unique_pairs, format_amount(total_flow)).dimmed());
    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!();

    // === TREE VISUALIZATION ===
    // Render tree starting from target wallet (the origin)
    fn render_tree_node(
        node: &str,
        adjacency: &HashMap<&str, Vec<(&str, f64, Option<i64>)>>,
        edge_count: &HashMap<(&str, &str), usize>,
        target: &str,
        token_name: &str,
        prefix: &str,
        is_last: bool,
        visited: &mut HashSet<String>,
        depth: usize,
        max_depth: usize,
    ) {
        use colored::Colorize;

        if depth > max_depth {
            return;
        }

        // Get wallet type label
        let wallet_label = classify_wallet(node)
            .map(|t| format!(" ({})", t).bright_blue().to_string())
            .unwrap_or_default();

        // Render this node
        let node_display = if node == target {
            format!("{}{}", node.bright_white().bold(), wallet_label)
        } else if node.len() > 12 {
            format!("{}...{}{}", &node[..4], &node[node.len()-4..], wallet_label)
        } else {
            format!("{}{}", node, wallet_label)
        };

        if depth == 0 {
            // Root node
            println!("  🏦 {} {}", "ORIGIN".bold().bright_yellow(), "(investigation target)".dimmed());
            println!("     {}", node_display);
        }

        visited.insert(node.to_string());

        // Get children (outflows from this node)
        if let Some(children) = adjacency.get(node) {
            let filtered_children: Vec<_> = children.iter()
                .filter(|(dest, _, _)| !visited.contains(*dest))
                .take(5) // Limit branches per node
                .collect();

            let child_count = filtered_children.len();
            let omitted = children.len().saturating_sub(child_count);

            for (i, (dest, amount, timestamp)) in filtered_children.iter().enumerate() {
                let is_last_child = i == child_count - 1 && omitted == 0;

                // Build the connector
                let connector = if is_last_child { "└─→" } else { "├─→" };

                // Format amount
                let amount_str = format_amount(*amount);
                let tx_count = edge_count.get(&(node, *dest)).copied().unwrap_or(1);
                let count_suffix = if tx_count > 1 { format!(" ({}x)", tx_count) } else { String::new() };

                // Format timestamp if available
                let time_str = timestamp.map(|ts| {
                    let dt = chrono::DateTime::from_timestamp(ts, 0)
                        .map(|d| d.format("%Y-%m-%d %H:%M").to_string())
                        .unwrap_or_default();
                    format!(" {}", dt.dimmed())
                }).unwrap_or_default();

                // Dest wallet display
                let dest_label = classify_wallet(dest)
                    .map(|t| format!(" ({})", t).bright_blue().to_string())
                    .unwrap_or_default();
                let dest_display = if dest.len() > 12 {
                    format!("{}...{}{}", &dest[..4], &dest[dest.len()-4..], dest_label)
                } else {
                    format!("{}{}", dest, dest_label)
                };

                // Print the edge
                let new_prefix = format!("{}     ", prefix);
                let edge_prefix = if depth == 0 { "     " } else { prefix };

                println!("{}│", edge_prefix);
                println!("{}{} [{}{}{}] ──→ {}{}",
                    edge_prefix,
                    connector.cyan(),
                    amount_str.bright_yellow(),
                    format!(" {}", token_name).dimmed(),
                    count_suffix.dimmed(),
                    dest_display,
                    time_str
                );

                // Recursively render children
                let child_prefix = if is_last_child {
                    format!("{}      ", edge_prefix)
                } else {
                    format!("{}│     ", edge_prefix)
                };

                render_tree_node(
                    dest,
                    adjacency,
                    edge_count,
                    target,
                    token_name,
                    &child_prefix,
                    is_last_child,
                    visited,
                    depth + 1,
                    max_depth,
                );
            }

            // Show omitted count
            if omitted > 0 {
                let edge_prefix = if depth == 0 { "     " } else { prefix };
                println!("{}│", edge_prefix);
                println!("{}└── {} {} more destinations", edge_prefix, "...".dimmed(), omitted);
            }
        }
    }

    let mut visited: HashSet<String> = HashSet::new();
    render_tree_node(
        target,
        &adjacency,
        &edge_count,
        target,
        token_name,
        "",
        true,
        &mut visited,
        0,
        4, // Max depth
    );
    println!();

    // === REVERSE TREE (Inflows) ===
    // Also show who sent TO the target
    let reverse_adj: HashMap<&str, Vec<(&str, f64)>> = {
        let mut rev: HashMap<&str, Vec<(&str, f64)>> = HashMap::new();
        for ((from, to), amount) in &edge_totals {
            rev.entry(*to).or_default().push((*from, *amount));
        }
        for (_, v) in rev.iter_mut() {
            v.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        }
        rev
    };

    if let Some(inflows) = reverse_adj.get(target) {
        if !inflows.is_empty() {
            println!("  📥 {} {}", "INFLOWS TO ORIGIN".bold().green(), "(who sent to target)".dimmed());
            println!();

            for (i, (from, amount)) in inflows.iter().take(5).enumerate() {
                let is_last = i == inflows.len().min(5) - 1;
                let connector = if is_last { "└─→" } else { "├─→" };
                let from_label = classify_wallet(from)
                    .map(|t| format!(" ({})", t).bright_blue().to_string())
                    .unwrap_or_default();
                let from_display = if from.len() > 12 {
                    format!("{}...{}{}", &from[..4], &from[from.len()-4..], from_label)
                } else {
                    format!("{}{}", from, from_label)
                };

                println!("     {} [{}{}] ──→ {}",
                    from_display.bright_green(),
                    format_amount(*amount).bright_yellow(),
                    format!(" {}", token_name).dimmed(),
                    connector.cyan()
                );
            }

            if inflows.len() > 5 {
                println!("     {} and {} more sources", "...".dimmed(), inflows.len() - 5);
            }
            println!();
        }
    }

    // === SUMMARY BOX ===
    println!("  ┌─────────────────────────────────────────────────────────────────┐");
    println!("  │ {} FLOW SUMMARY                                              │", "📊".to_string());
    println!("  ├─────────────────────────────────────────────────────────────────┤");
    println!("  │ Token: {:<55} │", token_name);
    println!("  │ Total Flow: {:<50} │", format_amount(total_flow));
    println!("  │ Unique Wallets: {:<46} │", all_wallets.len());
    println!("  │ Transfer Pairs: {:<46} │", unique_pairs);
    println!("  │ Total Transfers: {:<45} │", total_transfers);
    println!("  └─────────────────────────────────────────────────────────────────┘");
    println!();

    // === PATTERN DETECTION ===
    // Detect circular flows
    let mut adj_map: HashMap<&str, Vec<&str>> = HashMap::new();
    for ((from, to), _) in &edge_totals {
        adj_map.entry(*from).or_default().push(*to);
    }

    let mut cycles: Vec<(Vec<&str>, f64)> = Vec::new();
    for (start, neighbors) in &adj_map {
        for first_hop in neighbors.iter().take(3) {
            let mut path = vec![*start, *first_hop];
            let mut current = *first_hop;

            for _ in 0..3 {
                if let Some(next_neighbors) = adj_map.get(current) {
                    if next_neighbors.contains(start) {
                        path.push(*start);
                        let mut min_flow = f64::MAX;
                        for window in path.windows(2) {
                            if let Some(amt) = edge_totals.get(&(window[0], window[1])) {
                                min_flow = min_flow.min(*amt);
                            }
                        }
                        cycles.push((path.clone(), min_flow));
                        break;
                    }
                    if let Some(next) = next_neighbors.first() {
                        if !path.contains(next) {
                            path.push(*next);
                            current = *next;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
    }

    // Deduplicate cycles
    let mut unique_cycles: Vec<(Vec<&str>, f64)> = Vec::new();
    for (cycle, flow) in cycles {
        let mut sorted = cycle.clone();
        sorted.pop();
        sorted.sort();
        let is_dup = unique_cycles.iter().any(|(c, _)| {
            let mut s = c.clone();
            s.pop();
            s.sort();
            s == sorted
        });
        if !is_dup && cycle.len() > 2 {
            unique_cycles.push((cycle, flow));
        }
    }

    if !unique_cycles.is_empty() {
        println!("  ⚠️  {} {}", "CIRCULAR FLOWS DETECTED".bold().yellow(),
            format!("({} cycles - potential wash trading)", unique_cycles.len()).dimmed());
        for (i, (cycle, flow)) in unique_cycles.iter().take(3).enumerate() {
            let cycle_str: Vec<String> = cycle.iter()
                .map(|w| if w.len() > 8 { format!("{}..{}", &w[..4], &w[w.len()-4..]) } else { w.to_string() })
                .collect();
            println!("     {}. {} {}", i + 1, cycle_str.join(" → ").yellow(), format!("[{}]", format_amount(*flow)).dimmed());
        }
        println!();
    }

    // Detect hub wallets
    let mut hub_wallets: Vec<(&str, usize, f64)> = Vec::new();
    for (wallet, inflow) in &wallet_inflow {
        let senders: usize = edge_totals.iter().filter(|((_, to), _)| to == wallet).count();
        if senders >= 3 && *inflow > total_flow * 0.1 {
            hub_wallets.push((*wallet, senders, *inflow));
        }
    }
    hub_wallets.sort_by(|a, b| b.1.cmp(&a.1));

    if !hub_wallets.is_empty() {
        println!("  🎯 {} {}", "HUB WALLETS".bold().magenta(), "(3+ sources)".dimmed());
        for (wallet, senders, amount) in hub_wallets.iter().take(3) {
            let wallet_type = classify_wallet(wallet)
                .unwrap_or(if *senders > 10 { "likely exchange" } else { "aggregator" });
            let wallet_short = if wallet.len() > 12 {
                format!("{}...{}", &wallet[..4], &wallet[wallet.len()-4..])
            } else {
                wallet.to_string()
            };
            println!("     {} ← {} senders, {} ({})",
                wallet_short.magenta(), senders, format_amount(*amount), wallet_type.bright_blue());
        }
        println!();
    }

    // Detect flow patterns
    let patterns = detect_flow_patterns(transfers);
    if !patterns.is_empty() {
        println!("  🔍 {} {}", "FLOW PATTERNS".bold().yellow(),
            format!("({} detected)", patterns.len()).dimmed());
        for (pattern_name, description, _) in patterns.iter().take(3) {
            let icon = match pattern_name.as_str() {
                "Peel Chain" => "🔗",
                "Scatter-Gather" => "🌐",
                "Round-Trip" => "↔️",
                "Structuring" => "📦",
                "Concentration" => "🎯",
                "Pass-Through" => "⏭️",
                _ => "📊",
            };
            println!("     {} {} - {}", icon, pattern_name.yellow(), description.dimmed());
        }
        println!();
    }

    // Detect temporal patterns (bot detection, bursts, timing)
    let temporal_patterns = detect_temporal_patterns(transfers);
    if !temporal_patterns.is_empty() {
        println!("  ⏱️  {} {}", "TEMPORAL PATTERNS".bold().magenta(),
            format!("({} timing anomalies)", temporal_patterns.len()).dimmed());
        for (pattern_type, description, _, severity) in temporal_patterns.iter().take(5) {
            let icon = match pattern_type.as_str() {
                "Rapid Burst" => "⚡",
                "Regular Intervals" => "🤖",
                "Time Clustering" => "🕐",
                "Dormancy Burst" => "💤",
                "Weekday Only" => "📅",
                "Weekend Only" => "🌙",
                _ => "⏱️",
            };
            let sev_color = match severity.as_str() {
                "HIGH" => format!("[{}]", severity).red().bold().to_string(),
                "MEDIUM" => format!("[{}]", severity).yellow().to_string(),
                _ => format!("[{}]", severity).dimmed().to_string(),
            };
            println!("     {} {} {} - {}", icon, sev_color, pattern_type.magenta(), description.dimmed());
        }
        println!();
    }

    // Detect entity clusters (wallets likely controlled by same entity)
    let entity_clusters = detect_wallet_entities(transfers);
    if !entity_clusters.is_empty() {
        println!("  🔗 {} {}", "ENTITY CLUSTERS".bold().cyan(),
            format!("({} groups of related wallets)", entity_clusters.len()).dimmed());
        println!();

        for cluster in entity_clusters.iter().take(5) {
            let confidence_bar = "█".repeat((cluster.confidence * 10.0) as usize);
            let confidence_empty = "░".repeat(10 - (cluster.confidence * 10.0) as usize);
            let conf_color = if cluster.confidence > 0.7 {
                format!("{}{}", confidence_bar.green(), confidence_empty.dimmed())
            } else if cluster.confidence > 0.4 {
                format!("{}{}", confidence_bar.yellow(), confidence_empty.dimmed())
            } else {
                format!("{}{}", confidence_bar.red(), confidence_empty.dimmed())
            };

            let type_icon = match cluster.entity_type.as_str() {
                "Bot Swarm" => "🤖",
                "Same Owner" => "👤",
                "Coordinated" => "🎯",
                "Layered Transfer" => "🧅",
                _ => "🔗",
            };

            println!("     {} {} {} [{}] {:.0}%",
                type_icon,
                cluster.entity_type.bold().cyan(),
                format!("(Entity #{})", cluster.id).dimmed(),
                conf_color,
                cluster.confidence * 100.0
            );

            // Show member wallets
            println!("       {} {}", "Wallets:".dimmed(),
                format!("{} linked", cluster.wallets.len()).white());
            for (i, wallet) in cluster.wallets.iter().take(4).enumerate() {
                let wallet_label = classify_wallet(wallet)
                    .map(|t| format!(" ({})", t).bright_blue().to_string())
                    .unwrap_or_default();
                let prefix = if i == cluster.wallets.len().min(4) - 1 { "└─" } else { "├─" };
                println!("         {} {}{}",
                    prefix.dimmed(),
                    truncate_address(wallet).white(),
                    wallet_label
                );
            }
            if cluster.wallets.len() > 4 {
                println!("         {} {} more", "...".dimmed(),
                    cluster.wallets.len() - 4);
            }

            // Show evidence
            if !cluster.evidence.is_empty() {
                println!("       {} {}", "Evidence:".dimmed(),
                    cluster.evidence.first().unwrap_or(&String::new()).dimmed());
                for ev in cluster.evidence.iter().skip(1).take(2) {
                    println!("                 {}", ev.dimmed());
                }
            }
            println!();
        }

        if entity_clusters.len() > 5 {
            println!("     {} {} more clusters...", "...".dimmed(), entity_clusters.len() - 5);
            println!();
        }
    }
}

/// Generate plain-text ASCII graph for markdown report (no colors)
fn generate_ascii_graph_text(target: &str, transfers: &[TransferRecord]) -> String {
    use std::collections::HashMap;

    let mut output = String::new();

    // Aggregate flows by counterparty
    let mut inflows: HashMap<String, (f64, Vec<String>)> = HashMap::new();
    let mut outflows: HashMap<String, (f64, Vec<String>)> = HashMap::new();

    for tx in transfers {
        if tx.direction == "IN" {
            let entry = inflows.entry(tx.from.clone()).or_insert((0.0, Vec::new()));
            entry.0 += tx.amount;
            if !entry.1.contains(&tx.token) {
                entry.1.push(tx.token.clone());
            }
        } else {
            let entry = outflows.entry(tx.to.clone()).or_insert((0.0, Vec::new()));
            entry.0 += tx.amount;
            if !entry.1.contains(&tx.token) {
                entry.1.push(tx.token.clone());
            }
        }
    }

    // Sort and take top 5
    let mut top_inflows: Vec<_> = inflows.into_iter().collect();
    top_inflows.sort_by(|a, b| b.1.0.partial_cmp(&a.1.0).unwrap_or(std::cmp::Ordering::Equal));
    top_inflows.truncate(5);

    let mut top_outflows: Vec<_> = outflows.into_iter().collect();
    top_outflows.sort_by(|a, b| b.1.0.partial_cmp(&a.1.0).unwrap_or(std::cmp::Ordering::Equal));
    top_outflows.truncate(5);

    let max_lines = top_inflows.len().max(top_outflows.len()).max(3);
    let target_short = truncate_address(target);
    let mid_row = max_lines / 2;

    output.push_str("        INFLOWS                              OUTFLOWS\n");
    output.push_str("       (sources)                           (destinations)\n\n");

    for i in 0..max_lines {
        // Left side
        let left = if i < top_inflows.len() {
            let (addr, (_, tokens)) = &top_inflows[i];
            let addr_short = truncate_address(addr);
            let tokens_short: Vec<String> = tokens.iter()
                .take(2)
                .map(|t| if t.len() > 6 { format!("{}…", &t[..5]) } else { t.clone() })
                .collect();
            let tokens_str = if tokens.len() > 2 {
                format!("{},+{}", tokens_short[0], tokens.len() - 1)
            } else {
                tokens_short.join(",")
            };
            format!("{:>12} [{:>8}] ───▶", addr_short, tokens_str)
        } else {
            " ".repeat(28)
        };

        // Right side
        let right = if i < top_outflows.len() {
            let (addr, (_, tokens)) = &top_outflows[i];
            let addr_short = truncate_address(addr);
            let tokens_short: Vec<String> = tokens.iter()
                .take(2)
                .map(|t| if t.len() > 6 { format!("{}…", &t[..5]) } else { t.clone() })
                .collect();
            let tokens_str = if tokens.len() > 2 {
                format!("{},+{}", tokens_short[0], tokens.len() - 1)
            } else {
                tokens_short.join(",")
            };
            format!("▶─── [{:<8}] {}", tokens_str, addr_short)
        } else {
            String::new()
        };

        // Center
        let center = if i == mid_row - 1 {
            format!("┌{}┐", "─".repeat(14))
        } else if i == mid_row {
            format!("│ {:^12} │", target_short)
        } else if i == mid_row + 1 {
            format!("└{}┘", "─".repeat(14))
        } else {
            format!("{:^16}", "│")
        };

        output.push_str(&format!("{:<28}  {}  {}\n", left, center, right));
    }

    output.push_str(&format!("\n  ◀── {} sources    |    {} destinations ──▶\n",
        top_inflows.len(), top_outflows.len()));

    output
}

/// Known wallet/program addresses for classification
fn classify_wallet(address: &str) -> Option<&'static str> {
    // System programs
    if address == "11111111111111111111111111111111" { return Some("System Program"); }
    if address == "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA" { return Some("Token Program"); }
    if address == "ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL" { return Some("Associated Token"); }
    if address == "ComputeBudget111111111111111111111111111111" { return Some("Compute Budget"); }

    // DEX Programs
    if address.starts_with("JUP") { return Some("Jupiter Aggregator"); }
    if address == "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8" { return Some("Raydium AMM"); }
    if address == "whirLbMiicVdio4qvUfM5KAg6Ct8VwpYzGff3uctyCc" { return Some("Orca Whirlpool"); }
    if address == "9W959DqEETiGZocYWCQPaJ6sBmUzgfxXfqGeTEdp3aQP" { return Some("Orca Legacy"); }
    if address == "CAMMCzo5YL8w4VFF8KVHrK22GGUsp5VTaW7grrKgrWqK" { return Some("Raydium CLMM"); }
    if address == "srmqPvymJeFKQ4zGQed1GFppgkRHL9kaELCbyksJtPX" { return Some("Serum DEX"); }
    if address == "PhoeNiXZ8ByJGLkxNfZRnkUfjvmuYqLR89jjFHGqdXY" { return Some("Phoenix DEX"); }
    if address == "LBUZKhRxPF3XUpBCjp4YzTKgLccjZhTSDM9YuVaPwxo" { return Some("Meteora DLMM"); }

    // Staking/Lending
    if address == "MarBmsSgKXdrN1egZf5sqe1TMai9K1rChYNDJgjq7aD" { return Some("Marinade Staking"); }
    if address.starts_with("So1") { return Some("Solend"); }
    if address == "MFv2hWf31Z9kbCa1snEPYctwafyhdvnV7FZnsebVacA" { return Some("Marginfi"); }

    // NFT Marketplaces
    if address == "M2mx93ekt1fmXSVkTrUL9xVFHkmME8HTUi5Cyc5aF7K" { return Some("Magic Eden v2"); }
    if address.starts_with("TSW") { return Some("Tensor"); }

    // Bridges
    if address == "wormDTUJ6AWPNvk59vGQbDvGJmqbDTdgWgAqcLBCgUb" { return Some("Wormhole"); }

    None
}

/// Detect money laundering patterns in transfer graph
/// Returns: (pattern_name, description, involved_wallets, risk_score, recommendation)
fn detect_flow_patterns(transfers: &[TransferRecord]) -> Vec<(String, String, Vec<String>)> {
    use std::collections::{HashMap, HashSet};

    let mut patterns: Vec<(String, String, Vec<String>)> = Vec::new();

    if transfers.is_empty() {
        return patterns;
    }

    // Build comprehensive flow data
    let mut outflows_by_sender: HashMap<&str, Vec<(&str, f64, Option<&str>)>> = HashMap::new(); // (dest, amount, token)
    let mut inflows_by_receiver: HashMap<&str, Vec<(&str, f64, Option<&str>)>> = HashMap::new();
    let mut total_by_sender: HashMap<&str, f64> = HashMap::new();
    let mut total_by_receiver: HashMap<&str, f64> = HashMap::new();
    let mut token_counts: HashMap<&str, usize> = HashMap::new();

    for tx in transfers {
        outflows_by_sender.entry(&tx.from).or_default().push((&tx.to, tx.amount, Some(tx.token.as_str())));
        inflows_by_receiver.entry(&tx.to).or_default().push((&tx.from, tx.amount, Some(tx.token.as_str())));
        *total_by_sender.entry(&tx.from).or_insert(0.0) += tx.amount;
        *total_by_receiver.entry(&tx.to).or_insert(0.0) += tx.amount;
        *token_counts.entry(&tx.token).or_insert(0) += 1;
    }

    let global_total: f64 = total_by_sender.values().sum();

    // Pattern 1: PEEL CHAIN - One source distributes to many recipients in decreasing amounts
    // Classic money laundering technique: peel off small amounts to obscure trail
    for (sender, recipients) in &outflows_by_sender {
        if recipients.len() >= 5 {
            let mut amounts: Vec<f64> = recipients.iter().map(|(_, a, _)| *a).collect();
            amounts.sort_by(|a, b| b.partial_cmp(a).unwrap_or(std::cmp::Ordering::Equal));
            let total_sent = amounts.iter().sum::<f64>();

            // Check for characteristic peel pattern: few large, many small
            let top_3_pct = amounts.iter().take(3).sum::<f64>() / total_sent * 100.0;
            let small_tx_count = amounts.iter().filter(|a| **a < total_sent * 0.05).count();

            if top_3_pct > 60.0 && small_tx_count >= 3 {
                let largest = amounts.first().copied().unwrap_or(0.0);
                let smallest = amounts.last().copied().unwrap_or(0.0);
                let ratio = if smallest > 0.0 { largest / smallest } else { f64::MAX };

                patterns.push((
                    "Peel Chain".to_string(),
                    format!("{}→{} dests: top 3 = {:.0}% of {:.2} total, ratio {:.0}:1 (largest:{:.2}, smallest:{:.4})",
                        truncate_address(sender), recipients.len(),
                        top_3_pct, total_sent, ratio.min(9999.0), largest, smallest
                    ),
                    recipients.iter().take(5).map(|(r, _, _)| truncate_address(r)).collect(),
                ));
            }
        }
    }

    // Pattern 2: SCATTER-GATHER (Fan-out/Fan-in) - Layering technique
    // Source → multiple intermediaries → single destination
    for (sender, recipients) in &outflows_by_sender {
        if recipients.len() >= 3 {
            let recipient_addrs: HashSet<&str> = recipients.iter().map(|(r, _, _)| *r).collect();
            let scatter_total: f64 = recipients.iter().map(|(_, a, _)| a).sum();

            for (potential_gather, gatherers) in &inflows_by_receiver {
                if *potential_gather == *sender { continue; }

                let senders_to_gather: HashSet<&str> = gatherers.iter().map(|(s, _, _)| *s).collect();
                let overlap: Vec<&&str> = recipient_addrs.intersection(&senders_to_gather).collect();

                if overlap.len() >= 2 {
                    let gather_total: f64 = gatherers.iter()
                        .filter(|(s, _, _)| overlap.contains(&s))
                        .map(|(_, a, _)| a)
                        .sum();

                    // Calculate how much was "lost" in transit (fees, skimmed, etc)
                    let loss_pct = if scatter_total > 0.0 {
                        ((scatter_total - gather_total) / scatter_total * 100.0).abs()
                    } else { 0.0 };

                    patterns.push((
                        "Scatter-Gather".to_string(),
                        format!("{}→{} hops→{}: {:.2} scattered, {:.2} gathered ({:.1}% transit loss)",
                            truncate_address(sender), overlap.len(), truncate_address(potential_gather),
                            scatter_total, gather_total, loss_pct
                        ),
                        overlap.iter().map(|w| truncate_address(w)).collect(),
                    ));
                    break;
                }
            }
        }
    }

    // Pattern 3: ROUND-TRIP (Ping-Pong) - Potential wash trading or loan repayment
    for (sender, recipients) in &outflows_by_sender {
        for (recipient, out_amount, out_token) in recipients {
            if let Some(return_flows) = outflows_by_sender.get(recipient) {
                for (return_dest, return_amount, return_token) in return_flows {
                    if *return_dest == *sender && *return_amount > 0.0 {
                        let diff_pct = ((*out_amount - *return_amount) / out_amount.max(0.001) * 100.0).abs();
                        let same_token = out_token == return_token;
                        let suspicion = if same_token && diff_pct < 5.0 { "⚠️ HIGHLY SUSPICIOUS" }
                            else if same_token { "suspicious" }
                            else { "cross-token (possible swap)" };

                        patterns.push((
                            "Round-Trip".to_string(),
                            format!("{}↔{}: {:.2}→{:.2} ({:.1}% diff) {} - {}",
                                truncate_address(sender), truncate_address(recipient),
                                out_amount, return_amount, diff_pct,
                                out_token.unwrap_or("?"), suspicion
                            ),
                            vec![sender.to_string(), recipient.to_string()],
                        ));
                    }
                }
            }
        }
    }

    // Pattern 4: STRUCTURING (Smurfing) - Breaking up amounts to avoid detection thresholds
    // Look for many similar-sized transactions
    for (sender, recipients) in &outflows_by_sender {
        if recipients.len() >= 4 {
            let amounts: Vec<f64> = recipients.iter().map(|(_, a, _)| *a).collect();
            let avg = amounts.iter().sum::<f64>() / amounts.len() as f64;

            // Count how many are within 20% of average (suspiciously uniform)
            let uniform_count = amounts.iter().filter(|a| {
                let diff = ((**a - avg) / avg.max(0.001)).abs();
                diff < 0.20
            }).count();

            let uniform_pct = uniform_count as f64 / amounts.len() as f64 * 100.0;

            if uniform_pct > 60.0 && recipients.len() >= 5 {
                patterns.push((
                    "Structuring".to_string(),
                    format!("{}: {}/{} txs within 20% of avg ({:.2}) = {:.0}% uniform (possible smurfing)",
                        truncate_address(sender), uniform_count, recipients.len(), avg, uniform_pct
                    ),
                    recipients.iter().take(5).map(|(r, _, _)| truncate_address(r)).collect(),
                ));
            }
        }
    }

    // Pattern 5: CONCENTRATION - Many sources to one destination (potential mixer/tumbler output)
    for (receiver, senders) in &inflows_by_receiver {
        if senders.len() >= 5 {
            let total_received: f64 = senders.iter().map(|(_, a, _)| a).sum();
            let pct_of_global = total_received / global_total.max(0.001) * 100.0;

            // Check if this is a known program
            let wallet_type = classify_wallet(receiver);
            if wallet_type.is_none() && pct_of_global > 20.0 {
                let unique_tokens: HashSet<_> = senders.iter().filter_map(|(_, _, t)| *t).collect();

                patterns.push((
                    "Concentration".to_string(),
                    format!("{}←{} sources: {:.2} ({:.0}% of graph) in {} tokens - investigate this sink",
                        truncate_address(receiver), senders.len(), total_received, pct_of_global, unique_tokens.len()
                    ),
                    senders.iter().take(5).map(|(s, _, _)| truncate_address(s)).collect(),
                ));
            }
        }
    }

    // Pattern 6: RAPID CYCLING - Wallet receives and immediately sends (pass-through)
    for (wallet, inflows) in &inflows_by_receiver {
        if let Some(outflows) = outflows_by_sender.get(wallet) {
            let in_total: f64 = inflows.iter().map(|(_, a, _)| a).sum();
            let out_total: f64 = outflows.iter().map(|(_, a, _)| a).sum();

            // Check if >80% of inflows were sent out (pass-through behavior)
            let passthrough_pct = out_total / in_total.max(0.001) * 100.0;

            if passthrough_pct > 80.0 && inflows.len() >= 2 && outflows.len() >= 2 {
                let retention = in_total - out_total;

                patterns.push((
                    "Pass-Through".to_string(),
                    format!("{}: {:.2} in→{:.2} out ({:.0}% pass-through, {:.4} retained)",
                        truncate_address(wallet), in_total, out_total, passthrough_pct, retention.max(0.0)
                    ),
                    vec![wallet.to_string()],
                ));
            }
        }
    }

    // Deduplicate and sort by pattern type
    let mut seen: HashSet<String> = HashSet::new();
    patterns.retain(|(name, desc, _)| {
        let key = format!("{}:{}", name, &desc[..desc.len().min(50)]);
        if seen.contains(&key) { false } else { seen.insert(key); true }
    });

    // Sort: Round-Trip first (most suspicious), then others
    patterns.sort_by(|(a, _, _), (b, _, _)| {
        let priority = |name: &str| match name {
            "Round-Trip" => 0,
            "Scatter-Gather" => 1,
            "Structuring" => 2,
            "Concentration" => 3,
            "Peel Chain" => 4,
            "Pass-Through" => 5,
            _ => 6,
        };
        priority(a).cmp(&priority(b))
    });

    patterns
}

/// Temporal pattern analysis for detecting bot behavior, bursts, and scheduling
/// Returns: (pattern_type, description, wallets_involved, severity)
fn detect_temporal_patterns(transfers: &[TransferRecord]) -> Vec<(String, String, Vec<String>, String)> {
    use std::collections::HashMap;

    let mut patterns: Vec<(String, String, Vec<String>, String)> = Vec::new();

    if transfers.is_empty() {
        return patterns;
    }

    // Parse timestamps and group by wallet
    let mut wallet_timestamps: HashMap<&str, Vec<i64>> = HashMap::new();

    for tx in transfers {
        if let Some(ts_str) = &tx.timestamp {
            if let Ok(ts) = ts_str.parse::<i64>() {
                wallet_timestamps.entry(&tx.from).or_default().push(ts);
            }
        }
    }

    // Sort timestamps for each wallet
    for (_, timestamps) in wallet_timestamps.iter_mut() {
        timestamps.sort();
    }

    // Check if we have any timestamps at all
    let total_timestamps: usize = wallet_timestamps.values().map(|v| v.len()).sum();
    if total_timestamps == 0 {
        // No timestamps available, skip temporal analysis
        return patterns;
    }

    // === Pattern 1: RAPID BURSTS ===
    // Many transactions in a short time window indicates automation or panic selling
    for (wallet, timestamps) in &wallet_timestamps {
        if timestamps.len() < 3 { continue; } // Lowered threshold

        // Sliding window: count txs within 60-second windows
        let mut max_burst = 0;
        let mut burst_start = 0;
        for i in 0..timestamps.len() {
            let window_end = timestamps[i] + 60; // 60-second window
            let count = timestamps.iter().skip(i).take_while(|&&t| t <= window_end).count();
            if count > max_burst {
                max_burst = count;
                burst_start = timestamps[i];
            }
        }

        if max_burst >= 3 {
            let severity = if max_burst >= 8 { "HIGH" } else if max_burst >= 5 { "MEDIUM" } else { "LOW" };
            let time_str = chrono::DateTime::from_timestamp(burst_start, 0)
                .map(|d| d.format("%Y-%m-%d %H:%M:%S UTC").to_string())
                .unwrap_or_else(|| burst_start.to_string());

            patterns.push((
                "Rapid Burst".to_string(),
                format!("{}: {} txs within 60s at {} - likely bot/script",
                    truncate_address(wallet), max_burst, time_str),
                vec![wallet.to_string()],
                severity.to_string(),
            ));
        }
    }

    // === Pattern 2: REGULAR INTERVALS (Bot Behavior) ===
    // Consistent timing between transactions suggests automated scheduling
    for (wallet, timestamps) in &wallet_timestamps {
        if timestamps.len() < 4 { continue; }

        // Calculate intervals between consecutive transactions
        let intervals: Vec<i64> = timestamps.windows(2)
            .map(|w| w[1] - w[0])
            .filter(|&i| i > 0 && i < 86400) // Filter out outliers (>1 day)
            .collect();

        if intervals.len() < 3 { continue; }

        // Calculate mean and standard deviation
        let mean: f64 = intervals.iter().sum::<i64>() as f64 / intervals.len() as f64;
        let variance: f64 = intervals.iter()
            .map(|&i| (i as f64 - mean).powi(2))
            .sum::<f64>() / intervals.len() as f64;
        let std_dev = variance.sqrt();

        // Low coefficient of variation (std/mean < 0.3) suggests regularity
        let cv = std_dev / mean.max(1.0);

        if cv < 0.3 && mean > 10.0 {
            let avg_interval_str = if mean < 60.0 {
                format!("{:.0}s", mean)
            } else if mean < 3600.0 {
                format!("{:.1}m", mean / 60.0)
            } else {
                format!("{:.1}h", mean / 3600.0)
            };

            let severity = if cv < 0.1 { "HIGH" } else if cv < 0.2 { "MEDIUM" } else { "LOW" };

            patterns.push((
                "Regular Intervals".to_string(),
                format!("{}: avg {}, CV={:.2} ({}x) - 🤖 BOT DETECTED",
                    truncate_address(wallet), avg_interval_str, cv, intervals.len()),
                vec![wallet.to_string()],
                severity.to_string(),
            ));
        }
    }

    // === Pattern 3: TIME-OF-DAY CLUSTERING ===
    // Transactions clustered at specific hours (e.g., always at midnight UTC)
    for (wallet, timestamps) in &wallet_timestamps {
        if timestamps.len() < 5 { continue; }

        // Extract hours (0-23) from each timestamp
        let hours: Vec<u32> = timestamps.iter()
            .filter_map(|&ts| chrono::DateTime::from_timestamp(ts, 0))
            .map(|dt| dt.format("%H").to_string().parse::<u32>().unwrap_or(0))
            .collect();

        if hours.is_empty() { continue; }

        // Count occurrences per hour
        let mut hour_counts: HashMap<u32, usize> = HashMap::new();
        for h in &hours {
            *hour_counts.entry(*h).or_insert(0) += 1;
        }

        // Find the most popular hour
        if let Some((&peak_hour, &peak_count)) = hour_counts.iter().max_by_key(|(_, &c)| c) {
            let pct = peak_count as f64 / hours.len() as f64 * 100.0;

            // If >40% of transactions are in a single hour, flag it
            if pct > 40.0 && peak_count >= 3 {
                patterns.push((
                    "Time Clustering".to_string(),
                    format!("{}: {:.0}% of txs at {:02}:00 UTC ({}/{}) - scheduled activity",
                        truncate_address(wallet), pct, peak_hour, peak_count, hours.len()),
                    vec![wallet.to_string()],
                    if pct > 70.0 { "HIGH".to_string() } else { "MEDIUM".to_string() },
                ));
            }
        }
    }

    // === Pattern 4: DORMANCY FOLLOWED BY BURST ===
    // Long period of inactivity then sudden activity
    for (wallet, timestamps) in &wallet_timestamps {
        if timestamps.len() < 3 { continue; }

        // Find the longest gap between transactions
        let gaps: Vec<i64> = timestamps.windows(2)
            .map(|w| w[1] - w[0])
            .collect();

        if let Some(&max_gap) = gaps.iter().max() {
            let max_gap_days = max_gap as f64 / 86400.0;

            // If there's a gap > 7 days, check activity before and after
            if max_gap_days > 7.0 {
                // Find where the gap occurred
                if let Some(gap_idx) = gaps.iter().position(|&g| g == max_gap) {
                    let txs_after_gap = timestamps.len() - gap_idx - 1;

                    if txs_after_gap >= 3 {
                        // Calculate burst rate after dormancy
                        let post_gap_timestamps = &timestamps[gap_idx + 1..];
                        let post_duration = post_gap_timestamps.last().unwrap_or(&0) - post_gap_timestamps.first().unwrap_or(&0);
                        let rate = if post_duration > 0 {
                            post_gap_timestamps.len() as f64 / (post_duration as f64 / 3600.0)
                        } else {
                            0.0
                        };

                        patterns.push((
                            "Dormancy Burst".to_string(),
                            format!("{}: {:.0} days dormant → {} txs burst ({:.1}/hr) - reactivation",
                                truncate_address(wallet), max_gap_days, txs_after_gap, rate),
                            vec![wallet.to_string()],
                            if max_gap_days > 30.0 { "HIGH".to_string() } else { "MEDIUM".to_string() },
                        ));
                    }
                }
            }
        }
    }

    // === Pattern 5: WEEKEND/WEEKDAY CONCENTRATION ===
    // Transactions only during business hours or only weekends
    for (wallet, timestamps) in &wallet_timestamps {
        if timestamps.len() < 5 { continue; }

        // Extract day-of-week (0=Sunday, 6=Saturday)
        let weekdays: Vec<u32> = timestamps.iter()
            .filter_map(|&ts| chrono::DateTime::from_timestamp(ts, 0))
            .map(|dt| dt.format("%w").to_string().parse::<u32>().unwrap_or(0))
            .collect();

        if weekdays.is_empty() { continue; }

        let weekend_count = weekdays.iter().filter(|&&d| d == 0 || d == 6).count();
        let weekday_count = weekdays.len() - weekend_count;

        let weekend_pct = weekend_count as f64 / weekdays.len() as f64 * 100.0;

        // Flag if >80% weekday-only or >80% weekend-only
        if weekday_count >= 4 && weekend_pct < 10.0 {
            patterns.push((
                "Weekday Only".to_string(),
                format!("{}: {:.0}% weekday txs - business hours activity",
                    truncate_address(wallet), 100.0 - weekend_pct),
                vec![wallet.to_string()],
                "LOW".to_string(),
            ));
        } else if weekend_count >= 3 && weekend_pct > 80.0 {
            patterns.push((
                "Weekend Only".to_string(),
                format!("{}: {:.0}% weekend txs - unusual timing",
                    truncate_address(wallet), weekend_pct),
                vec![wallet.to_string()],
                "MEDIUM".to_string(),
            ));
        }
    }

    // Sort by severity (HIGH first)
    patterns.sort_by(|(_, _, _, sev_a), (_, _, _, sev_b)| {
        let priority = |s: &str| match s { "HIGH" => 0, "MEDIUM" => 1, "LOW" => 2, _ => 3 };
        priority(sev_a).cmp(&priority(sev_b))
    });

    // Deduplicate
    let mut seen = std::collections::HashSet::new();
    patterns.retain(|(name, desc, _, _)| {
        let key = format!("{}:{}", name, &desc[..desc.len().min(40)]);
        if seen.contains(&key) { false } else { seen.insert(key); true }
    });

    patterns
}

/// Entity cluster representing a group of wallets likely controlled by same entity
#[derive(Debug, Clone)]
struct EntityCluster {
    id: usize,
    wallets: Vec<String>,
    confidence: f64,  // 0.0 - 1.0
    evidence: Vec<String>,
    entity_type: String,  // "Bot Swarm", "Same Owner", "Coordinated", "Unknown"
}

/// Detect wallet entities - groups of wallets likely controlled by same entity
/// Uses Union-Find with multiple linkage signals
fn detect_wallet_entities(transfers: &[TransferRecord]) -> Vec<EntityCluster> {
    use std::collections::{HashMap, HashSet};

    // Collect all unique wallets
    let mut all_wallets: HashSet<String> = HashSet::new();
    for tx in transfers {
        all_wallets.insert(tx.from.clone());
        all_wallets.insert(tx.to.clone());
    }

    let wallets: Vec<String> = all_wallets.into_iter().collect();
    if wallets.len() < 2 {
        return Vec::new();
    }

    let wallet_idx: HashMap<&str, usize> = wallets.iter()
        .enumerate()
        .map(|(i, w)| (w.as_str(), i))
        .collect();

    // Union-Find data structure
    let mut parent: Vec<usize> = (0..wallets.len()).collect();
    let mut rank: Vec<usize> = vec![0; wallets.len()];

    fn find(parent: &mut Vec<usize>, i: usize) -> usize {
        if parent[i] != i {
            parent[i] = find(parent, parent[i]);
        }
        parent[i]
    }

    fn union(parent: &mut Vec<usize>, rank: &mut Vec<usize>, x: usize, y: usize) {
        let root_x = find(parent, x);
        let root_y = find(parent, y);
        if root_x != root_y {
            if rank[root_x] < rank[root_y] {
                parent[root_x] = root_y;
            } else if rank[root_x] > rank[root_y] {
                parent[root_y] = root_x;
            } else {
                parent[root_y] = root_x;
                rank[root_x] += 1;
            }
        }
    }

    // Track linkage evidence
    let mut linkage_evidence: HashMap<(usize, usize), Vec<String>> = HashMap::new();

    // === Signal 1: Common Funding Source ===
    // Wallets that received first inflow from same source are likely related
    let mut first_funder: HashMap<&str, &str> = HashMap::new();
    let mut funding_amounts: HashMap<(&str, &str), f64> = HashMap::new();

    for tx in transfers {
        if !first_funder.contains_key(tx.to.as_str()) {
            first_funder.insert(&tx.to, &tx.from);
            funding_amounts.insert((&tx.to, &tx.from), tx.amount);
        }
    }

    // Group by common funder
    let mut funder_groups: HashMap<&str, Vec<&str>> = HashMap::new();
    for (wallet, funder) in &first_funder {
        funder_groups.entry(*funder).or_default().push(*wallet);
    }

    for (funder, funded_wallets) in &funder_groups {
        if funded_wallets.len() >= 2 {
            // Link all wallets funded by same source
            for i in 0..funded_wallets.len() {
                for j in (i+1)..funded_wallets.len() {
                    if let (Some(&idx_i), Some(&idx_j)) = (
                        wallet_idx.get(funded_wallets[i]),
                        wallet_idx.get(funded_wallets[j])
                    ) {
                        union(&mut parent, &mut rank, idx_i, idx_j);
                        let key = if idx_i < idx_j { (idx_i, idx_j) } else { (idx_j, idx_i) };
                        linkage_evidence.entry(key).or_default().push(
                            format!("Common funder: {}", truncate_address(funder))
                        );
                    }
                }
            }
        }
    }

    // === Signal 2: Synchronized Timing ===
    // Wallets that transact within 60 seconds of each other repeatedly
    let mut wallet_timestamps: HashMap<&str, Vec<i64>> = HashMap::new();
    for tx in transfers {
        if let Some(ts_str) = &tx.timestamp {
            if let Ok(ts) = ts_str.parse::<i64>() {
                wallet_timestamps.entry(&tx.from).or_default().push(ts);
                wallet_timestamps.entry(&tx.to).or_default().push(ts);
            }
        }
    }

    // Sort timestamps for each wallet
    for (_, timestamps) in wallet_timestamps.iter_mut() {
        timestamps.sort();
    }

    // Find synchronized pairs (multiple txs within 60s window)
    let wallet_keys: Vec<&str> = wallet_timestamps.keys().copied().collect();
    for i in 0..wallet_keys.len() {
        for j in (i+1)..wallet_keys.len() {
            let w1 = wallet_keys[i];
            let w2 = wallet_keys[j];

            let ts1 = &wallet_timestamps[w1];
            let ts2 = &wallet_timestamps[w2];

            // Count synchronized timestamps (within 60s)
            let mut sync_count = 0;
            let mut idx1 = 0;
            let mut idx2 = 0;

            while idx1 < ts1.len() && idx2 < ts2.len() {
                let diff = (ts1[idx1] - ts2[idx2]).abs();
                if diff <= 60 {
                    sync_count += 1;
                    idx1 += 1;
                    idx2 += 1;
                } else if ts1[idx1] < ts2[idx2] {
                    idx1 += 1;
                } else {
                    idx2 += 1;
                }
            }

            // If >30% of transactions are synchronized, link them
            let min_txs = ts1.len().min(ts2.len());
            if min_txs >= 3 && sync_count as f64 / min_txs as f64 > 0.3 {
                if let (Some(&idx_i), Some(&idx_j)) = (wallet_idx.get(w1), wallet_idx.get(w2)) {
                    union(&mut parent, &mut rank, idx_i, idx_j);
                    let key = if idx_i < idx_j { (idx_i, idx_j) } else { (idx_j, idx_i) };
                    linkage_evidence.entry(key).or_default().push(
                        format!("Sync timing: {}/{} txs within 60s", sync_count, min_txs)
                    );
                }
            }
        }
    }

    // === Signal 3: Same Counterparties ===
    // Wallets that interact with the same set of counterparties
    let mut wallet_counterparties: HashMap<&str, HashSet<&str>> = HashMap::new();
    for tx in transfers {
        wallet_counterparties.entry(&tx.from).or_default().insert(&tx.to);
        wallet_counterparties.entry(&tx.to).or_default().insert(&tx.from);
    }

    for i in 0..wallet_keys.len() {
        for j in (i+1)..wallet_keys.len() {
            let w1 = wallet_keys[i];
            let w2 = wallet_keys[j];

            // Skip if w1 and w2 are counterparties of each other
            if wallet_counterparties.get(w1).map(|s| s.contains(w2)).unwrap_or(false) {
                continue;
            }

            let cp1 = wallet_counterparties.get(w1);
            let cp2 = wallet_counterparties.get(w2);

            if let (Some(cp1), Some(cp2)) = (cp1, cp2) {
                // Jaccard similarity of counterparty sets
                let intersection = cp1.intersection(cp2).count();
                let union_size = cp1.union(cp2).count();

                if union_size >= 3 && intersection as f64 / union_size as f64 > 0.5 {
                    if let (Some(&idx_i), Some(&idx_j)) = (wallet_idx.get(w1), wallet_idx.get(w2)) {
                        union(&mut parent, &mut rank, idx_i, idx_j);
                        let key = if idx_i < idx_j { (idx_i, idx_j) } else { (idx_j, idx_i) };
                        linkage_evidence.entry(key).or_default().push(
                            format!("Shared counterparties: {}/{} overlap", intersection, union_size)
                        );
                    }
                }
            }
        }
    }

    // === Signal 4: Similar Transaction Amounts ===
    // Wallets using identical/similar amounts repeatedly
    let mut wallet_amounts: HashMap<&str, Vec<f64>> = HashMap::new();
    for tx in transfers {
        wallet_amounts.entry(&tx.from).or_default().push(tx.amount);
    }

    for i in 0..wallet_keys.len() {
        for j in (i+1)..wallet_keys.len() {
            let w1 = wallet_keys[i];
            let w2 = wallet_keys[j];

            let amounts1 = wallet_amounts.get(w1);
            let amounts2 = wallet_amounts.get(w2);

            if let (Some(a1), Some(a2)) = (amounts1, amounts2) {
                if a1.len() >= 3 && a2.len() >= 3 {
                    // Check for similar amount patterns (within 5%)
                    let mut similar_count = 0;
                    for amt1 in a1 {
                        for amt2 in a2 {
                            if *amt1 > 0.0 && *amt2 > 0.0 {
                                let ratio = amt1 / amt2;
                                if ratio > 0.95 && ratio < 1.05 {
                                    similar_count += 1;
                                }
                            }
                        }
                    }

                    let total_pairs = a1.len() * a2.len();
                    if similar_count as f64 / total_pairs as f64 > 0.3 {
                        if let (Some(&idx_i), Some(&idx_j)) = (wallet_idx.get(w1), wallet_idx.get(w2)) {
                            union(&mut parent, &mut rank, idx_i, idx_j);
                            let key = if idx_i < idx_j { (idx_i, idx_j) } else { (idx_j, idx_i) };
                            linkage_evidence.entry(key).or_default().push(
                                format!("Similar amounts: {}/{} matches", similar_count, total_pairs.min(20))
                            );
                        }
                    }
                }
            }
        }
    }

    // === Signal 5: Transfer Chains ===
    // A→B→C pattern where B is just passing through
    let mut passthrough: HashMap<&str, (f64, f64)> = HashMap::new(); // (inflow, outflow)
    for tx in transfers {
        passthrough.entry(&tx.to).or_insert((0.0, 0.0)).0 += tx.amount;
        passthrough.entry(&tx.from).or_insert((0.0, 0.0)).1 += tx.amount;
    }

    // Find wallets that are >90% pass-through
    let pass_wallets: Vec<&str> = passthrough.iter()
        .filter(|(_, (in_amt, out_amt))| {
            *in_amt > 0.0 && *out_amt > 0.0 && (*out_amt / *in_amt) > 0.9 && (*out_amt / *in_amt) < 1.1
        })
        .map(|(w, _)| *w)
        .collect();

    // Link pass-through wallets to their sources and destinations
    for pass_wallet in &pass_wallets {
        let sources: Vec<&str> = transfers.iter()
            .filter(|tx| tx.to == *pass_wallet)
            .map(|tx| tx.from.as_str())
            .collect();
        let dests: Vec<&str> = transfers.iter()
            .filter(|tx| tx.from == *pass_wallet)
            .map(|tx| tx.to.as_str())
            .collect();

        // Link sources and dests through the pass-through wallet
        for src in &sources {
            for dst in &dests {
                if let (Some(&idx_s), Some(&idx_d)) = (wallet_idx.get(*src), wallet_idx.get(*dst)) {
                    if idx_s != idx_d {
                        union(&mut parent, &mut rank, idx_s, idx_d);
                        let key = if idx_s < idx_d { (idx_s, idx_d) } else { (idx_d, idx_s) };
                        linkage_evidence.entry(key).or_default().push(
                            format!("Pass-through: {} via {}", truncate_address(pass_wallet), truncate_address(pass_wallet))
                        );
                    }
                }
            }
        }
    }

    // === Build clusters from Union-Find ===
    let mut cluster_members: HashMap<usize, Vec<usize>> = HashMap::new();
    for i in 0..wallets.len() {
        let root = find(&mut parent, i);
        cluster_members.entry(root).or_default().push(i);
    }

    // Filter to clusters with 2+ members and build EntityCluster structs
    let mut clusters: Vec<EntityCluster> = Vec::new();
    let mut cluster_id = 0;

    for (_, members) in cluster_members {
        if members.len() < 2 {
            continue;
        }

        let cluster_wallets: Vec<String> = members.iter()
            .map(|&idx| wallets[idx].clone())
            .collect();

        // Collect all evidence for this cluster
        let mut evidence: HashSet<String> = HashSet::new();
        for i in 0..members.len() {
            for j in (i+1)..members.len() {
                let key = if members[i] < members[j] {
                    (members[i], members[j])
                } else {
                    (members[j], members[i])
                };
                if let Some(ev) = linkage_evidence.get(&key) {
                    for e in ev {
                        evidence.insert(e.clone());
                    }
                }
            }
        }

        // Calculate confidence based on evidence count and types
        let evidence_count = evidence.len();
        let has_funding = evidence.iter().any(|e| e.contains("funder"));
        let has_timing = evidence.iter().any(|e| e.contains("Sync"));
        let has_counterparty = evidence.iter().any(|e| e.contains("counterpart"));
        let has_amounts = evidence.iter().any(|e| e.contains("amount"));

        let confidence = ((evidence_count as f64 * 0.1).min(0.4)
            + if has_funding { 0.2 } else { 0.0 }
            + if has_timing { 0.2 } else { 0.0 }
            + if has_counterparty { 0.1 } else { 0.0 }
            + if has_amounts { 0.1 } else { 0.0 }).min(1.0);

        // Determine entity type
        let entity_type = if has_timing && evidence.iter().any(|e| e.contains("60s")) {
            "Bot Swarm"
        } else if has_funding && has_counterparty {
            "Same Owner"
        } else if has_timing && has_counterparty {
            "Coordinated"
        } else if evidence.iter().any(|e| e.contains("Pass-through")) {
            "Layered Transfer"
        } else {
            "Related"
        };

        clusters.push(EntityCluster {
            id: cluster_id,
            wallets: cluster_wallets,
            confidence,
            evidence: evidence.into_iter().take(5).collect(),
            entity_type: entity_type.to_string(),
        });

        cluster_id += 1;
    }

    // Sort by confidence descending
    clusters.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap_or(std::cmp::Ordering::Equal));

    clusters
}

/// Generate token flow tree visualization as markdown-compatible text
fn generate_token_flow_tree_markdown(target: &str, transfers: &[TransferRecord], token_filter: &str) -> String {
    use std::collections::{HashMap, HashSet};

    let mut output = String::new();

    // Filter transfers for the specific token
    let token_lower = token_filter.to_lowercase();
    let filtered: Vec<&TransferRecord> = transfers.iter()
        .filter(|tx| {
            let tx_token_lower = tx.token.to_lowercase();
            tx_token_lower == token_lower ||
            tx_token_lower.contains(&token_lower) ||
            tx.token == token_filter
        })
        .collect();

    if filtered.is_empty() {
        output.push_str(&format!("No transfers found for token '{}'\n", token_filter));
        return output;
    }

    // Build adjacency and stats
    let mut adjacency: HashMap<&str, Vec<(&str, f64, Option<i64>)>> = HashMap::new();
    let mut edge_totals: HashMap<(&str, &str), f64> = HashMap::new();
    let mut edge_count: HashMap<(&str, &str), usize> = HashMap::new();

    let token_name = filtered.first().map(|t| t.token.as_str()).unwrap_or(token_filter);

    // First pass: aggregate edges
    for tx in &filtered {
        let key = (tx.from.as_str(), tx.to.as_str());
        *edge_totals.entry(key).or_insert(0.0) += tx.amount;
        *edge_count.entry(key).or_insert(0) += 1;
    }

    // Build adjacency with aggregated amounts
    for ((from, to), total_amount) in &edge_totals {
        let timestamp: Option<i64> = filtered.iter()
            .filter(|tx| tx.from.as_str() == *from && tx.to.as_str() == *to)
            .filter_map(|tx| tx.timestamp.as_ref())
            .filter_map(|ts| ts.parse::<i64>().ok())
            .min();
        adjacency.entry(*from).or_default().push((*to, *total_amount, timestamp));
    }

    // Sort adjacency lists by amount descending
    for (_, neighbors) in adjacency.iter_mut() {
        neighbors.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    }

    let total_flow: f64 = edge_totals.values().sum();
    let total_transfers: usize = edge_count.values().sum();
    let unique_pairs = edge_totals.len();

    // Header
    output.push_str(&format!("Token: {} | {} transfers | {} unique pairs | {} total\n\n",
        token_name, total_transfers, unique_pairs, format_amount(total_flow)));

    // Tree rendering helper
    fn render_tree_node_md(
        node: &str,
        adjacency: &HashMap<&str, Vec<(&str, f64, Option<i64>)>>,
        edge_count: &HashMap<(&str, &str), usize>,
        target: &str,
        token_name: &str,
        prefix: &str,
        visited: &mut HashSet<String>,
        depth: usize,
        max_depth: usize,
        output: &mut String,
    ) {
        if depth > max_depth { return; }

        // Get wallet type label
        let wallet_label = classify_wallet(node)
            .map(|t| format!(" ({})", t))
            .unwrap_or_default();

        // Render this node
        let node_display = if node == target {
            format!("`{}`{}", node, wallet_label)
        } else if node.len() > 12 {
            format!("`{}...{}`{}", &node[..4], &node[node.len()-4..], wallet_label)
        } else {
            format!("`{}`{}", node, wallet_label)
        };

        if depth == 0 {
            output.push_str("🏦 **ORIGIN** (investigation target)\n");
            output.push_str(&format!("   {}\n", node_display));
        }

        visited.insert(node.to_string());

        // Get children (outflows from this node)
        if let Some(children) = adjacency.get(node) {
            let filtered_children: Vec<_> = children.iter()
                .filter(|(dest, _, _)| !visited.contains(*dest))
                .take(5)
                .collect();

            let child_count = filtered_children.len();
            let omitted = children.len().saturating_sub(child_count);

            for (i, (dest, amount, timestamp)) in filtered_children.iter().enumerate() {
                let is_last_child = i == child_count - 1 && omitted == 0;
                let connector = if is_last_child { "└─→" } else { "├─→" };

                let amount_str = format_amount(*amount);
                let tx_count = edge_count.get(&(node, *dest)).copied().unwrap_or(1);
                let count_suffix = if tx_count > 1 { format!(" ({}x)", tx_count) } else { String::new() };

                let time_str = timestamp.map(|ts| {
                    chrono::DateTime::from_timestamp(ts, 0)
                        .map(|d| format!(" @ {}", d.format("%Y-%m-%d %H:%M")))
                        .unwrap_or_default()
                }).unwrap_or_default();

                let dest_label = classify_wallet(dest)
                    .map(|t| format!(" ({})", t))
                    .unwrap_or_default();
                let dest_display = if dest.len() > 12 {
                    format!("`{}...{}`{}", &dest[..4], &dest[dest.len()-4..], dest_label)
                } else {
                    format!("`{}`{}", dest, dest_label)
                };

                let edge_prefix = if depth == 0 { "   " } else { prefix };
                output.push_str(&format!("{}│\n", edge_prefix));
                output.push_str(&format!("{}{} [**{}** {}{}] ──→ {}{}\n",
                    edge_prefix, connector, amount_str, token_name, count_suffix, dest_display, time_str));

                let child_prefix = if is_last_child {
                    format!("{}      ", edge_prefix)
                } else {
                    format!("{}│     ", edge_prefix)
                };

                render_tree_node_md(
                    dest, adjacency, edge_count, target, token_name, &child_prefix,
                    visited, depth + 1, max_depth, output,
                );
            }

            if omitted > 0 {
                let edge_prefix = if depth == 0 { "   " } else { prefix };
                output.push_str(&format!("{}│\n", edge_prefix));
                output.push_str(&format!("{}└── *...and {} more destinations*\n", edge_prefix, omitted));
            }
        }
    }

    let mut visited: HashSet<String> = HashSet::new();
    render_tree_node_md(target, &adjacency, &edge_count, target, token_name, "", &mut visited, 0, 4, &mut output);

    // Reverse tree (inflows)
    let reverse_adj: HashMap<&str, Vec<(&str, f64)>> = {
        let mut rev: HashMap<&str, Vec<(&str, f64)>> = HashMap::new();
        for ((from, to), amount) in &edge_totals {
            rev.entry(*to).or_default().push((*from, *amount));
        }
        for (_, v) in rev.iter_mut() {
            v.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        }
        rev
    };

    if let Some(inflows) = reverse_adj.get(target) {
        if !inflows.is_empty() {
            output.push_str("\n📥 **INFLOWS TO ORIGIN** (who sent to target)\n\n");
            for (i, (from, amount)) in inflows.iter().take(5).enumerate() {
                let is_last = i == inflows.len().min(5) - 1;
                let connector = if is_last { "└─→" } else { "├─→" };
                let from_label = classify_wallet(from)
                    .map(|t| format!(" ({})", t))
                    .unwrap_or_default();
                let from_display = if from.len() > 12 {
                    format!("`{}...{}`{}", &from[..4], &from[from.len()-4..], from_label)
                } else {
                    format!("`{}`{}", from, from_label)
                };
                output.push_str(&format!("   {} [**{}** {}] {} TARGET\n",
                    from_display, format_amount(*amount), token_name, connector));
            }
            if inflows.len() > 5 {
                output.push_str(&format!("\n   *...and {} more sources*\n", inflows.len() - 5));
            }
        }
    }

    output
}

/// Generate markdown report for auto mode
fn generate_auto_report(
    wallet: &str,
    fetched_count: usize,
    discovered: &HashSet<String>,
    transfers: &[TransferRecord],
    findings: &[Finding],
    query: Option<&str>,
    duration: std::time::Duration,
) -> String {
    let mut report = String::new();

    report.push_str(&format!("# Wallet Investigation Report\n\n"));
    report.push_str(&format!("**Target:** `{}`\n\n", wallet));
    report.push_str(&format!("**Generated:** {}\n\n", chrono::Local::now().format("%Y-%m-%d %H:%M:%S")));

    if let Some(q) = query {
        report.push_str(&format!("**Query/Hypothesis:** {}\n\n", q));
    }

    report.push_str("---\n\n");
    report.push_str("## Summary\n\n");
    report.push_str(&format!("| Metric | Value |\n"));
    report.push_str(&format!("|--------|-------|\n"));
    report.push_str(&format!("| Wallets Fetched | {} |\n", fetched_count));
    report.push_str(&format!("| Wallets Discovered | {} |\n", discovered.len()));
    report.push_str(&format!("| Total Transfers | {} |\n", transfers.len()));
    report.push_str(&format!("| Exploration Time | {:.2}s |\n\n", duration.as_secs_f64()));

    // Token breakdown
    let mut token_volumes: std::collections::HashMap<String, (f64, f64)> = std::collections::HashMap::new();
    for tx in transfers {
        let entry = token_volumes.entry(tx.token.clone()).or_insert((0.0, 0.0));
        if tx.direction == "IN" {
            entry.0 += tx.amount;
        } else {
            entry.1 += tx.amount;
        }
    }

    report.push_str("## Token Flow Summary\n\n");
    report.push_str("| Token | Inflows | Outflows |\n");
    report.push_str("|-------|---------|----------|\n");
    for (token, (inflow, outflow)) in &token_volumes {
        let token_display = if token.len() > 10 { format!("{}…", &token[..9]) } else { token.clone() };
        report.push_str(&format!("| {} | {:.4} | {:.4} |\n", token_display, inflow, outflow));
    }
    report.push_str("\n");

    // Token flow tree visualizations for top 3 tokens
    let mut sorted_tokens: Vec<_> = token_volumes.iter().collect();
    sorted_tokens.sort_by(|a, b| (b.1.0 + b.1.1).partial_cmp(&(a.1.0 + a.1.1)).unwrap_or(std::cmp::Ordering::Equal));

    for (i, (token, _)) in sorted_tokens.iter().take(3).enumerate() {
        report.push_str(&format!("### {} Flow Tree (Token #{})\n\n", token, i + 1));
        report.push_str("```\n");
        report.push_str(&generate_token_flow_tree_markdown(wallet, transfers, token));
        report.push_str("```\n\n");
    }

    // ASCII Graph for report
    report.push_str("## Wallet Flow Graph\n\n");
    report.push_str("```\n");
    report.push_str(&generate_ascii_graph_text(wallet, transfers));
    report.push_str("```\n\n");

    // Hub wallet detection for report
    let mut hub_data: Vec<(&str, usize, f64)> = Vec::new();
    let mut inflow_counts: HashMap<&str, (usize, f64)> = HashMap::new();
    for tx in transfers {
        let entry = inflow_counts.entry(&tx.to).or_insert((0, 0.0));
        entry.0 += 1;
        entry.1 += tx.amount;
    }
    for (addr, (count, amount)) in &inflow_counts {
        if *count >= 3 {
            hub_data.push((*addr, *count, *amount));
        }
    }
    hub_data.sort_by(|a, b| b.1.cmp(&a.1));

    if !hub_data.is_empty() {
        report.push_str("## Hub Wallets\n\n");
        report.push_str("| Wallet | Senders | Amount | Type |\n");
        report.push_str("|--------|---------|--------|------|\n");
        for (addr, senders, amount) in hub_data.iter().take(10) {
            let wallet_type = classify_wallet(addr)
                .unwrap_or(if *senders > 10 { "Likely Exchange/Pool" } else { "Aggregator" });
            report.push_str(&format!("| `{}` | {} | {:.2} | {} |\n",
                truncate_address(addr), senders, amount, wallet_type));
        }
        report.push_str("\n");
    }

    // Flow patterns detection for report
    let patterns = detect_flow_patterns(transfers);
    if !patterns.is_empty() {
        report.push_str("## Detected Flow Patterns\n\n");
        report.push_str("⚠️ These patterns may indicate suspicious activity but require further investigation.\n\n");
        for (pattern_name, description, wallets) in &patterns {
            report.push_str(&format!("### {}\n\n", pattern_name));
            report.push_str(&format!("{}\n\n", description));
            if !wallets.is_empty() {
                report.push_str("**Involved wallets:** ");
                report.push_str(&wallets.join(", "));
                report.push_str("\n\n");
            }
        }
    }

    // Temporal patterns detection for report
    let temporal_patterns = detect_temporal_patterns(transfers);
    if !temporal_patterns.is_empty() {
        report.push_str("## Temporal Patterns (Timing Analysis)\n\n");
        report.push_str("⏱️ Analysis of transaction timing reveals potential automated or coordinated activity.\n\n");
        report.push_str("| Type | Severity | Description |\n");
        report.push_str("|------|----------|-------------|\n");
        for (pattern_type, description, wallets, severity) in temporal_patterns.iter().take(15) {
            let icon = match pattern_type.as_str() {
                "Rapid Burst" => "⚡",
                "Regular Intervals" => "🤖",
                "Time Clustering" => "🕐",
                "Dormancy Burst" => "💤",
                "Weekday Only" => "📅",
                "Weekend Only" => "🌙",
                _ => "⏱️",
            };
            let severity_badge = match severity.as_str() {
                "HIGH" => "**HIGH**",
                "MEDIUM" => "*MEDIUM*",
                _ => "LOW",
            };
            report.push_str(&format!("| {} {} | {} | {} |\n",
                icon, pattern_type, severity_badge, description));
        }
        if temporal_patterns.len() > 15 {
            report.push_str(&format!("\n*...and {} more timing anomalies*\n", temporal_patterns.len() - 15));
        }
        report.push_str("\n");

        // Add details for high severity patterns
        let high_severity: Vec<_> = temporal_patterns.iter()
            .filter(|(_, _, _, sev)| sev == "HIGH")
            .take(5)
            .collect();
        if !high_severity.is_empty() {
            report.push_str("### High-Severity Timing Alerts\n\n");
            for (pattern_type, description, wallets, _) in high_severity {
                report.push_str(&format!("**{}**: {}\n", pattern_type, description));
                if !wallets.is_empty() {
                    report.push_str(&format!("- Involved: `{}`\n", wallets.join("`, `")));
                }
                report.push_str("\n");
            }
        }
    }

    // Entity clusters for report
    let entity_clusters = detect_wallet_entities(transfers);
    if !entity_clusters.is_empty() {
        report.push_str("## Entity Clusters (Related Wallets)\n\n");
        report.push_str("🔗 Wallets grouped by behavioral and transactional similarity.\n\n");

        for cluster in entity_clusters.iter().take(10) {
            let type_icon = match cluster.entity_type.as_str() {
                "Bot Swarm" => "🤖",
                "Same Owner" => "👤",
                "Coordinated" => "🎯",
                "Layered Transfer" => "🧅",
                _ => "🔗",
            };

            report.push_str(&format!("### {} {} (Confidence: {:.0}%)\n\n",
                type_icon, cluster.entity_type, cluster.confidence * 100.0));

            report.push_str("**Member Wallets:**\n");
            for wallet in cluster.wallets.iter().take(10) {
                let wallet_label = classify_wallet(wallet)
                    .map(|t| format!(" *({})*", t))
                    .unwrap_or_default();
                report.push_str(&format!("- `{}`{}\n", wallet, wallet_label));
            }
            if cluster.wallets.len() > 10 {
                report.push_str(&format!("- *...and {} more wallets*\n", cluster.wallets.len() - 10));
            }

            if !cluster.evidence.is_empty() {
                report.push_str("\n**Linkage Evidence:**\n");
                for evidence in &cluster.evidence {
                    report.push_str(&format!("- {}\n", evidence));
                }
            }
            report.push_str("\n");
        }

        if entity_clusters.len() > 10 {
            report.push_str(&format!("*...and {} more entity clusters*\n\n", entity_clusters.len() - 10));
        }
    }

    // Cross-token correlation (basic swap detection)
    let mut token_timing: HashMap<&str, Vec<(&str, f64, &str)>> = HashMap::new(); // token -> [(wallet, amount, direction)]
    for tx in transfers {
        token_timing.entry(&tx.token).or_default().push((&tx.from, tx.amount, &tx.direction));
    }

    // Find potential swaps (SOL out, other token in at similar amounts)
    let mut potential_swaps: Vec<(String, String, f64, f64)> = Vec::new();
    if let Some(sol_txs) = token_timing.get("SOL") {
        for (other_token, other_txs) in &token_timing {
            if *other_token == "SOL" { continue; }
            for (sol_wallet, sol_amount, sol_dir) in sol_txs {
                if *sol_dir != "OUT" { continue; }
                for (other_wallet, other_amount, other_dir) in other_txs {
                    if *other_dir == "IN" && sol_wallet == other_wallet {
                        potential_swaps.push((
                            truncate_address(sol_wallet),
                            other_token.to_string(),
                            *sol_amount,
                            *other_amount,
                        ));
                    }
                }
            }
        }
    }

    if !potential_swaps.is_empty() {
        report.push_str("## Potential DEX Swaps\n\n");
        report.push_str("| Wallet | SOL Out | Token In | Amount |\n");
        report.push_str("|--------|---------|----------|--------|\n");
        for (wallet_addr, token, sol_out, token_in) in potential_swaps.iter().take(10) {
            let token_display = if token.len() > 10 { format!("{}…", &token[..9]) } else { token.clone() };
            report.push_str(&format!("| {} | {:.4} | {} | {:.2} |\n",
                wallet_addr, sol_out, token_display, token_in));
        }
        report.push_str("\n");
    }

    // Findings
    if !findings.is_empty() {
        report.push_str("## Notable Findings\n\n");
        for finding in findings {
            report.push_str(&format!("- **[{}]** {} - {}\n",
                finding.severity, finding.category, finding.description));
        }
        report.push_str("\n");
    }

    // Classified wallets
    let mut classified: Vec<(&str, &str)> = Vec::new();
    for w in discovered.iter() {
        if let Some(classification) = classify_wallet(w) {
            classified.push((w.as_str(), classification));
        }
    }

    if !classified.is_empty() {
        report.push_str("## Identified Programs/Contracts\n\n");
        for (addr, name) in &classified {
            report.push_str(&format!("- `{}` - **{}**\n", truncate_address(addr), name));
        }
        report.push_str("\n");
    }

    // Discovered wallets
    report.push_str("## Discovered Wallets\n\n");
    for (i, w) in discovered.iter().take(20).enumerate() {
        report.push_str(&format!("{}. `{}`\n", i + 1, w));
    }
    if discovered.len() > 20 {
        report.push_str(&format!("\n*...and {} more wallets*\n", discovered.len() - 20));
    }

    report.push_str("\n---\n");
    report.push_str("*Generated by OSVM Autonomous Investigation*\n");

    report
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