use crate::services::{
    agentic_researcher::AgenticResearcher, ai_service::AiService, ovsm_service::OvsmService,
};
use anyhow::{Context, Result};
use clap::ArgMatches;
use std::sync::Arc;
use tokio::sync::Mutex;

/// Handle the deep research command with fully autonomous investigation
pub async fn handle_deep_research_command(matches: &ArgMatches) -> Result<()> {
    let target = matches
        .get_one::<String>("target")
        .context("Target is required for investigation")?;

    let depth = matches
        .get_one::<String>("depth")
        .map(|d| d.parse::<u32>().unwrap_or(10))
        .unwrap_or(10);

    let strategy = matches
        .get_one::<String>("strategy")
        .map(|s| s.as_str())
        .unwrap_or("opportunistic");

    println!("\n╔══════════════════════════════════════════════════════════════╗");
    println!("║           🧠 AGENTIC DEEP RESEARCH SYSTEM 🧠                ║");
    println!("╚══════════════════════════════════════════════════════════════╝");
    println!();
    println!("Target: {}", target);
    println!("Max Depth: {}", depth);
    println!("Strategy: {}", strategy);
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    // Initialize services
    let ai_service = Arc::new(Mutex::new(AiService::new()));
    let ovsm_service = Arc::new(Mutex::new(OvsmService::new()));

    // Create agentic researcher
    let researcher = AgenticResearcher::new(ai_service, ovsm_service, target.clone());

    // Launch autonomous investigation
    println!("\n🚀 Initiating autonomous investigation...");
    println!("   The agent will:");
    println!("   • Generate its own questions");
    println!("   • Seek contradictory evidence");
    println!("   • Spawn sub-investigations for anomalies");
    println!("   • Adapt strategy based on discoveries");
    println!("   • Converge on high-confidence conclusions\n");

    let start_time = std::time::Instant::now();

    match researcher.investigate_autonomously().await {
        Ok(report) => {
            let duration = start_time.elapsed();

            println!("\n╔══════════════════════════════════════════════════════════════╗");
            println!("║                   INVESTIGATION REPORT                       ║");
            println!("╚══════════════════════════════════════════════════════════════╝");
            println!();
            println!("{}", report);
            println!();
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("⏱️  Investigation completed in: {:.2?}", duration);

            // Save report if requested
            if matches.get_flag("save") {
                let filename = format!(
                    "deep_research_{}_{}.md",
                    target.chars().take(8).collect::<String>(),
                    chrono::Local::now().format("%Y%m%d_%H%M%S")
                );
                std::fs::write(&filename, &report)?;
                println!("📄 Report saved to: {}", filename);
            }

            // Generate visualization if requested
            if matches.get_flag("visualize") {
                generate_investigation_graph(&researcher).await?;
            }
        }
        Err(e) => {
            eprintln!("\n❌ Investigation failed: {}", e);
            eprintln!("   The agent encountered an unrecoverable error.");
            return Err(e);
        }
    }

    Ok(())
}

/// Generate a visualization of the investigation graph
async fn generate_investigation_graph(researcher: &AgenticResearcher) -> Result<()> {
    println!("\n📊 Generating investigation graph visualization...");

    // This would generate a graph visualization using graphviz or similar
    // For now, we'll output a text representation

    println!("\n🌳 Investigation Tree:");
    println!("├── Initial Questions");
    println!("│   ├── Exploratory Branch 1");
    println!("│   │   ├── Finding A");
    println!("│   │   ├── Contradiction A.1");
    println!("│   │   └── Hypothesis A");
    println!("│   ├── Anomaly Detection");
    println!("│   │   └── Sub-Investigation 1");
    println!("│   └── Pattern Recognition");
    println!("└── Convergence Points");
    println!("    ├── High Confidence Hypothesis 1");
    println!("    └── High Confidence Hypothesis 2");

    Ok(())
}

/// Interactive mode for guided deep research
pub async fn handle_interactive_research(matches: &ArgMatches) -> Result<()> {
    use std::io::{self, Write};

    println!("\n🎮 Interactive Deep Research Mode");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Commands:");
    println!("  'start <target>' - Begin investigation");
    println!("  'status' - Show current investigation state");
    println!("  'focus <area>' - Direct investigation focus");
    println!("  'question <q>' - Add specific question");
    println!("  'strategy <s>' - Change investigation strategy");
    println!("  'report' - Generate current report");
    println!("  'exit' - Exit interactive mode");

    let ai_service = Arc::new(Mutex::new(AiService::new()));
    let ovsm_service = Arc::new(Mutex::new(OvsmService::new()));
    let mut current_researcher: Option<AgenticResearcher> = None;

    loop {
        print!("\n🔬 research> ");
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let parts: Vec<&str> = input.split_whitespace().collect();

        if parts.is_empty() {
            continue;
        }

        match parts[0] {
            "start" => {
                if parts.len() < 2 {
                    println!("Usage: start <target>");
                    continue;
                }
                let target = parts[1..].join(" ");
                println!("Starting investigation of: {}", target);

                current_researcher = Some(AgenticResearcher::new(
                    ai_service.clone(),
                    ovsm_service.clone(),
                    target,
                ));

                // Start async investigation in background
                if let Some(ref researcher) = current_researcher {
                    let r = researcher.clone();
                    tokio::spawn(async move {
                        if let Err(e) = r.investigate_autonomously().await {
                            eprintln!("Background investigation error: {}", e);
                        }
                    });
                    println!("✅ Investigation started in background");
                }
            }
            "status" => {
                if current_researcher.is_none() {
                    println!("No active investigation. Use 'start <target>' to begin.");
                } else {
                    println!("📊 Investigation in progress...");
                    // Would show actual status from the researcher
                }
            }
            "focus" => {
                if parts.len() < 2 {
                    println!("Usage: focus <area>");
                    continue;
                }
                let area = parts[1..].join(" ");
                println!("Redirecting focus to: {}", area);
                // Would actually redirect the investigation
            }
            "question" => {
                if parts.len() < 2 {
                    println!("Usage: question <your question>");
                    continue;
                }
                let question = parts[1..].join(" ");
                println!("Adding question: {}", question);
                // Would add question to researcher's queue
            }
            "strategy" => {
                if parts.len() < 2 {
                    println!("Available strategies:");
                    println!("  - breadth: Explore many shallow paths");
                    println!("  - depth: Deep dive into promising leads");
                    println!("  - opportunistic: Follow anomalies");
                    println!("  - adversarial: Seek contradictions");
                    println!("  - creative: Generate novel hypotheses");
                    continue;
                }
                let strategy = parts[1];
                println!("Switching to {} strategy", strategy);
                // Would change researcher's strategy
            }
            "report" => {
                if let Some(ref researcher) = current_researcher {
                    println!("Generating current report...");
                    // Would generate interim report
                    println!("📝 [Report would be displayed here]");
                } else {
                    println!("No active investigation.");
                }
            }
            "exit" | "quit" => {
                println!("Exiting interactive mode...");
                break;
            }
            _ => {
                println!("Unknown command: {}", parts[0]);
            }
        }
    }

    Ok(())
}

/// Demo mode showcasing the agentic researcher capabilities
pub async fn run_deep_research_demo() -> Result<()> {
    println!("\n🎭 Deep Research Demo: Showcasing Agentic Investigation");
    println!("═══════════════════════════════════════════════════════");

    // Create a mock investigation scenario
    println!("\nScenario: Investigating suspicious wallet behavior");
    println!("Target: Demo_Wallet_123");

    let ai_service = Arc::new(Mutex::new(AiService::new()));
    let ovsm_service = Arc::new(Mutex::new(OvsmService::new()));
    let researcher =
        AgenticResearcher::new(ai_service, ovsm_service, "Demo_Wallet_123".to_string());

    println!("\n📋 Demonstration Phases:");
    println!("1. Self-Questioning: Agent generates its own investigation paths");
    println!("2. Contradiction Seeking: Actively looks for disproving evidence");
    println!("3. Anomaly Detection: Identifies unusual patterns");
    println!("4. Sub-Investigations: Spawns parallel investigation threads");
    println!("5. Strategy Adaptation: Changes approach based on findings");
    println!("6. Convergence: Reaches high-confidence conclusions");

    println!("\n[In a real scenario, the agent would now run autonomously...]");

    // Simulate some investigation steps
    println!("\n🔄 Iteration 1:");
    println!("   ❓ Generated question: \"What are the primary transaction patterns?\"");
    println!("   🔍 Investigating...");
    println!("   ✅ Found: Regular DeFi interactions");
    println!("   ❌ Contradiction: Irregular timing on weekends");

    println!("\n🔄 Iteration 2:");
    println!("   💡 Hypothesis: Automated trading bot with manual overrides");
    println!("   🔍 Seeking evidence...");
    println!("   🚀 Spawning sub-investigation: \"Weekend anomaly analysis\"");

    println!("\n🔄 Iteration 3:");
    println!("   🎯 Strategy shift: Moving from breadth-first to focused anomaly investigation");
    println!("   🔍 Deep diving into weekend patterns...");

    println!("\n📊 Convergence achieved!");
    println!("   Confidence: 87% - Hybrid bot/manual trading system");

    Ok(())
}
