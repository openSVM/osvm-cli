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

    // Convert OVSM result to JSON string
    let raw_output = format!("{:?}", raw_result);

    // Format with AI service (with fallback to raw output)
    println!("🤖 Formatting results with AI...\n");
    let formatted_report = {
        let mut ai = ai_service.lock().await;
        match format_wallet_analysis(&mut ai, wallet, &raw_output).await {
            Ok(report) => report,
            Err(e) => {
                eprintln!("⚠️  AI formatting failed: {}", e);
                eprintln!("📋 Showing raw OVSM output instead:\n");
                raw_output.clone()
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

  ;; Fetch ALL transfers with pagination (1000 at a time)
  (define all_transfers [])
  (define keep_fetching true)
  (define batch_num 0)

  (while keep_fetching
    (do
      (define resp (get_account_transfers {{:address target :limit 1000 :offset (* batch_num 1000)}}))
      (define batch (get resp "data"))
      (define batch_size (length batch))

      ;; Append this batch to all_transfers
      (set! all_transfers (append all_transfers batch))

      ;; Stop if we got less than 1000 (last batch)
      (if (< batch_size 1000)
          (set! keep_fetching false)
          (set! batch_num (+ batch_num 1)))))

  ;; Now aggregate by token mint
  (define by_mint (group-by all_transfers (lambda (tx) (get tx "mint"))))

  ;; For each token, aggregate senders/receivers
  (define token_summaries
    (map
      (entries by_mint)
      (lambda (mint_pair)
        (do
          (define mint (get mint_pair 0))
          (define txs (get mint_pair 1))

          ;; Get token symbol from first tx
          (define symbol (if (> (length txs) 0)
                            (get (get txs 0) "tokenSymbol")
                            mint))

          ;; Split by direction
          (define inflows (filter txs (lambda (t) (= (get t "transferType") "IN"))))
          (define outflows (filter txs (lambda (t) (= (get t "transferType") "OUT"))))

          ;; Aggregate inflows by sender
          (define inflow_agg
            (reduce
              inflows
              {{}}
              (lambda (acc tx)
                (do
                  (define from (get tx "from"))
                  (define amt (float (get tx "tokenAmount")))
                  (define existing (get acc from))
                  (define current (if existing existing 0))
                  (put acc from (+ current amt))))))

          ;; Aggregate outflows by receiver
          (define outflow_agg
            (reduce
              outflows
              {{}}
              (lambda (acc tx)
                (do
                  (define to (get tx "to"))
                  (define amt (float (get tx "tokenAmount")))
                  (define existing (get acc to))
                  (define current (if existing existing 0))
                  (put acc to (+ current amt))))))

          ;; Sort and take top 5
          (define top_senders
            (take 5
              (sort
                (entries inflow_agg)
                (lambda (a b) (> (get a 1) (get b 1))))))

          (define top_receivers
            (take 5
              (sort
                (entries outflow_agg)
                (lambda (a b) (> (get a 1) (get b 1))))))

          {{:mint mint
           :symbol symbol
           :transfer_count (length txs)
           :inflow_count (length inflows)
           :outflow_count (length outflows)
           :top_senders top_senders
           :top_receivers top_receivers}})))

  ;; Return AGGREGATED summaries (NOT raw transfers!)
  {{:wallet target
   :total_transfers (length all_transfers)
   :num_tokens (length token_summaries)
   :tokens token_summaries}})
"#, wallet)
}

/// Format the raw OVSM output using AI
async fn format_wallet_analysis(ai_service: &mut AiService, wallet: &str, raw_output: &str) -> Result<String> {
    let prompt = format!(r#"You are a blockchain analyst. Analyze the following Solana wallet transfer data and create a comprehensive token flow report.

Wallet Address: {}

Raw Transfer Data:
{}

CRITICAL: Group ALL transfers by TOKEN (mint address), then for EACH token show:

1. **Token Summary**:
   - Token symbol and mint address
   - Total transfers for this token

2. **Inflow Analysis** (transfers where transferType="IN"):
   - List TOP 5 unique addresses that sent this token TO the wallet
   - Show total amount received from each address
   - Exclude the target wallet itself

3. **Outflow Analysis** (transfers where transferType="OUT"):
   - List TOP 5 unique addresses that received this token FROM the wallet
   - Show total amount sent to each address
   - Exclude the target wallet itself

Format as markdown with:
- Clear section for each token
- Tables showing top inflow/outflow addresses with amounts
- Identify if addresses are DEX programs (Raydium, Orca, Jupiter, etc.)
- Highlight individual wallet addresses vs protocol addresses
- Note: SOL token mint is "SOL" (native token)

Be concise but comprehensive. Focus on INDIVIDUAL WALLET counterparties, not protocol addresses."#, wallet, raw_output);

    ai_service.query(&prompt).await
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