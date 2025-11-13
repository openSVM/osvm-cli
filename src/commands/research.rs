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

    // Format with AI service
    println!("🤖 Formatting results with AI...\n");
    let formatted_report = {
        let mut ai = ai_service.lock().await;
        format_wallet_analysis(&mut ai, wallet, &raw_output).await?
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
fn generate_wallet_analysis_script(wallet: &str) -> String {
    format!(r#";; Wallet analysis script
(do
  (define target "{}")
  ;; Fetch transactions with pagination to get up to 10,000 txs
  (define all_txs [])
  (define batch_size 100)
  (define max_txs 10000)
  (define before null)
  (define keep_fetching true)

  (while (and keep_fetching (< (count all_txs) max_txs))
    (define params
      (if (null? before)
        {{:address target :limit batch_size}}
        {{:address target :limit batch_size :before before}}))

    (define resp (get_account_transactions params))
    (define batch (get resp "transactions"))

    (when (array? batch)
      (set! all_txs (concat all_txs batch))

      ;; Get last signature for pagination
      (when (> (count batch) 0)
        (define last_tx (get batch (- (count batch) 1)))
        (set! before (get last_tx "signature")))

      ;; Stop if we got fewer than batch_size (no more txs)
      (when (< (count batch) batch_size)
        (set! keep_fetching false))))

  (define txs all_txs)

  ;; Track all token flows (not just SOL)
  (define token_flows {{}})

  (for (tx txs)
    (define xfers (get tx "transfers"))
    (when (array? xfers)
      (for (xf xfers)
        (define addr (get xf "account"))
        (define chg (get xf "change"))
        (define mnt (get xf "mint"))

        (when (and addr mnt (!= addr target))
          ;; Initialize token if not exists
          (when (null? (get token_flows mnt))
            (set! token_flows (set token_flows mnt {{:inflow {{}} :outflow {{}}}})))

          (define tok_data (get token_flows mnt))

          (if (< chg 0)
            ;; Inflow (negative change = sent TO target)
            (do
              (define inf_map (get tok_data "inflow"))
              (define prev (get inf_map addr))
              (set! inf_map (set inf_map addr (+ (if (null? prev) 0 prev) (- 0 chg))))
              (set! tok_data (set tok_data "inflow" inf_map))
              (set! token_flows (set token_flows mnt tok_data)))
            ;; Outflow (positive change = received FROM target)
            (do
              (define out_map (get tok_data "outflow"))
              (define prev (get out_map addr))
              (set! out_map (set out_map addr (+ (if (null? prev) 0 prev) chg)))
              (set! tok_data (set tok_data "outflow" out_map))
              (set! token_flows (set token_flows mnt tok_data))))))))

  ;; Format results per token
  (define token_results [])
  (define mints (keys token_flows))

  (for (mint mints)
    (define tok_data (get token_flows mint))
    (define inf_map (get tok_data "inflow"))
    (define out_map (get tok_data "outflow"))
    (define inf_list (entries inf_map))
    (define out_list (entries out_map))

    (set! token_results (append token_results {{
      :token mint
      :top_5_inflow (take 5 inf_list)
      :top_5_outflow (take 5 out_list)
      :total_inflow_addresses (count inf_list)
      :total_outflow_addresses (count out_list)
    }})))

  {{:wallet target
   :total_txs (count txs)
   :tokens_analyzed (count mints)
   :token_analysis token_results}})
"#, wallet)
}

/// Format the raw OVSM output using AI
async fn format_wallet_analysis(ai_service: &mut AiService, wallet: &str, raw_output: &str) -> Result<String> {
    let prompt = format!(r#"You are a blockchain analyst. Format the following Solana wallet analysis data into a clear, human-readable report.

Wallet Address: {}

Raw Analysis Data:
{}

The data includes per-token analysis with inflow/outflow addresses and amounts.

Create a comprehensive markdown report with:
1. **Executive Summary** - brief overview of wallet activity and key findings
2. **Token Analysis** (for each token found):
   - Token address/mint
   - Top 5 addresses that sent this token TO the wallet (inflow) with amounts
   - Top 5 addresses that received this token FROM the wallet (outflow) with amounts
   - Total unique inflow/outflow addresses
3. **Overall Statistics** - total transactions analyzed, number of tokens, unique counterparties
4. **Key Observations** - notable patterns, suspicious activity, concentration risks

Notes:
- Convert lamports to human-readable amounts (1 SOL = 1,000,000,000 lamports)
- Identify SOL token as "So11111111111111111111111111111111111111112"
- Format addresses as monospace code blocks
- Use tables where appropriate
- Highlight unusual patterns (e.g., mirror transactions, dust, concentration)
- Keep it professional but insightful"#, wallet, raw_output);

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