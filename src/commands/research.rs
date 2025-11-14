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
    println!("🔍 Analyzing wallet: {}", wallet);
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    // Initialize services
    let ai_service = Arc::new(Mutex::new(AiService::new()));

    // Initialize OVSM with MCP tools for blockchain data access
    println!("\n🔧 Initializing OVSM with MCP tools...");
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
    println!("✅ OVSM initialized with blockchain tools");

    // Generate OVSM script for wallet analysis
    println!("\n📊 Generating OVSM analysis script...");
    let ovsm_script = generate_wallet_analysis_script(wallet);

    // Execute OVSM script
    println!("⚙️  Executing on-chain data analysis...\n");
    let raw_result = ovsm_service.execute_code(&ovsm_script)
        .context("Failed to execute OVSM analysis")?;

    // Extract ONLY the aggregated summary (not raw transfers!)
    let summary_json = extract_summary_json(&raw_result)?;

    // Format with AI service (with fallback to raw output)
    println!("🤖 Formatting results with AI...\n");
    let formatted_report = {
        let mut ai = ai_service.lock().await;
        match format_wallet_analysis(&mut ai, wallet, &summary_json).await {
            Ok(report) => report,
            Err(e) => {
                eprintln!("⚠️  AI formatting failed: {}", e);
                eprintln!("📋 Showing aggregated summary instead:\n");
                summary_json.clone()
            }
        }
    };

    // Display formatted report
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("{}", formatted_report);
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("\n✅ Research completed successfully!");

    // Optionally save report to file
    if matches.get_flag("save") {
        let filename = format!("wallet_research_{}.md", wallet);
        std::fs::write(&filename, &formatted_report)?;
        println!("📄 Report saved to: {}", filename);
    }

    Ok(())
}

/// Handle agent-based research (complex multi-iteration)
async fn handle_agent_research(matches: &ArgMatches, wallet: &str) -> Result<()> {
    println!("🚀 Starting Intelligent Wallet Research for: {}", wallet);
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    // Initialize services
    let ai_service = Arc::new(Mutex::new(AiService::new()));
    let ovsm_service = Arc::new(Mutex::new(OvsmService::new()));

    // Create research agent
    let agent = ResearchAgent::new(ai_service, ovsm_service, wallet.to_string());

    // Run investigation with self-evaluation
    println!("\n🔬 Initiating multi-phase investigation with AI self-evaluation...\n");

    match agent.investigate().await {
        Ok(report) => {
            println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("{}", report);
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("\n✅ Research completed successfully!");

            // Optionally save report to file
            if matches.get_flag("save") {
                let filename = format!("wallet_research_{}.md", wallet);
                std::fs::write(&filename, &report)?;
                println!("📄 Report saved to: {}", filename);
            }
        }
        Err(e) => {
            eprintln!("❌ Research failed: {}", e);
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
    let system_prompt = r#"You are a markdown formatter. The blockchain analysis is ALREADY COMPLETE.

Your ONLY job: Convert the provided JSON into clean markdown.

DO NOT analyze, interpret, or add commentary. ONLY format what's given:

**Wallet Summary**
- Total: {total_transfers_unique} unique transfers ({inflow_count} in, {outflow_count} out)
- Tokens: {num_tokens} different tokens

**Top 3 Senders** (addresses sending TO this wallet)
| Address | Tokens Sent |
|---------|-------------|
[For each sender in top_senders array, list address and all tokens with amounts]

**Top 3 Receivers** (addresses receiving FROM this wallet)
| Address | Tokens Received |
|---------|-----------------|
[For each receiver in top_receivers array, list address and all tokens with amounts]

NO analysis. NO interpretation. ONLY formatting the JSON into tables."#.to_string();

    // Question contains the actual data
    let question = format!(r#"Wallet Address: {}

Aggregated Transfer Summary (JSON):
{}"#, wallet, summary_json);

    // Use ownPlan=true to bypass planning and use our custom system prompt directly
    ai_service.query_osvm_ai_with_options(&question, Some(system_prompt), Some(true), false).await
        .context("Failed to format analysis with AI")
}

/// Run a quick test of the research agent
pub async fn run_research_demo() -> Result<()> {
    println!("🧪 Running Research Agent Demo");
    println!("================================\n");

    // Use a well-known wallet for demo
    let demo_wallet = "11111111111111111111111111111111"; // System program

    let ai_service = Arc::new(Mutex::new(AiService::new()));
    let ovsm_service = Arc::new(Mutex::new(OvsmService::new()));
    let agent = ResearchAgent::new(ai_service, ovsm_service, demo_wallet.to_string());

    println!("📊 Demonstrating iterative investigation with self-evaluation:\n");

    // Run abbreviated investigation
    let report = agent.investigate().await?;

    println!("\n📝 Demo Report Preview:");
    println!("{}", &report[..report.len().min(1000)]);

    Ok(())
}