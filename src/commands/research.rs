use anyhow::{Context, Result};
use clap::ArgMatches;
use std::collections::HashSet;
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
    println!();

    // Initialize MCP pool with dynamic scaling
    print!("{}", "⏳ Initializing MCP pool (dynamic scaling)...".dimmed());
    std::io::stdout().flush()?;

    let pool_config = McpPoolConfig {
        min_instances: 1,
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
        progress.println(format!("  {} batch of {}: {} [pool: {}]",
            "⚡ Fetching".bright_blue(),
            batch.len(),
            batch.iter().map(|(w, _)| truncate_address(w)).collect::<Vec<_>>().join(", "),
            stats
        ));

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

    // Render ASCII graph visualization
    render_ascii_graph(wallet, &all_transfers);

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
                    timestamp: tx.get("timestamp")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string()),
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

/// Render ASCII graph visualization of wallet flows
fn render_ascii_graph(target: &str, transfers: &[TransferRecord]) {
    use colored::Colorize;
    use std::collections::HashMap;

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

    // Sort by amount and take top 5
    let mut top_inflows: Vec<_> = inflows.into_iter().collect();
    top_inflows.sort_by(|a, b| b.1.0.partial_cmp(&a.1.0).unwrap_or(std::cmp::Ordering::Equal));
    top_inflows.truncate(5);

    let mut top_outflows: Vec<_> = outflows.into_iter().collect();
    top_outflows.sort_by(|a, b| b.1.0.partial_cmp(&a.1.0).unwrap_or(std::cmp::Ordering::Equal));
    top_outflows.truncate(5);

    let max_lines = top_inflows.len().max(top_outflows.len()).max(3);
    let target_short = truncate_address(target);

    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!("{}", "🕸️  WALLET FLOW GRAPH".bold().cyan());
    println!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
    println!();

    // Print the graph with cleaner layout
    let mid_row = max_lines / 2;

    for i in 0..max_lines {
        // Build left side (inflows)
        let left = if i < top_inflows.len() {
            let (addr, (_, tokens)) = &top_inflows[i];
            let addr_short = truncate_address(addr);
            // Truncate token names to 6 chars max
            let tokens_short: Vec<String> = tokens.iter()
                .take(2)
                .map(|t| if t.len() > 6 { format!("{}…", &t[..5]) } else { t.clone() })
                .collect();
            let tokens_str = if tokens.len() > 2 {
                format!("{},+{}", tokens_short[0], tokens.len() - 1)
            } else {
                tokens_short.join(",")
            };
            format!("{:>12} {:>10} ───▶", addr_short.green(), format!("[{}]", tokens_str).dimmed())
        } else {
            " ".repeat(28)
        };

        // Build right side (outflows)
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
            format!("▶─── {:<10} {}", format!("[{}]", tokens_str).dimmed(), addr_short.red())
        } else {
            String::new()
        };

        // Build center with target box
        let center = if i == mid_row - 1 {
            format!("┌{}┐", "─".repeat(14)).bright_yellow().to_string()
        } else if i == mid_row {
            format!("│ {} │", target_short.bright_white().bold()).to_string()
        } else if i == mid_row + 1 {
            format!("└{}┘", "─".repeat(14)).bright_yellow().to_string()
        } else {
            format!("{:^16}", "│")
        };

        println!("{:<28}  {}  {}", left, center, right);
    }

    println!();

    // Summary with counts
    println!("  {}  {} sources sending IN    |    {} destinations receiving OUT  {}",
        "◀──".green(),
        format!("{}", top_inflows.len()).bright_green(),
        format!("{}", top_outflows.len()).bright_red(),
        "──▶".red()
    );
    println!();
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

    report.push_str("## Token Flow\n\n");
    report.push_str("| Token | Inflows | Outflows |\n");
    report.push_str("|-------|---------|----------|\n");
    for (token, (inflow, outflow)) in &token_volumes {
        let token_display = if token.len() > 10 { format!("{}…", &token[..9]) } else { token.clone() };
        report.push_str(&format!("| {} | {:.4} | {:.4} |\n", token_display, inflow, outflow));
    }
    report.push_str("\n");

    // ASCII Graph for report
    report.push_str("## Wallet Flow Graph\n\n");
    report.push_str("```\n");
    report.push_str(&generate_ascii_graph_text(wallet, transfers));
    report.push_str("```\n\n");

    // Findings
    if !findings.is_empty() {
        report.push_str("## Notable Findings\n\n");
        for finding in findings {
            report.push_str(&format!("- **[{}]** {} - {}\n",
                finding.severity, finding.category, finding.description));
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