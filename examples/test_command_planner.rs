//! Test the OSVM Command Planner
//!
//! Run with: cargo run --example test_command_planner

use osvm::utils::osvm_command_planner::OsvmCommandPlanner;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🤖 OSVM Command Planner Test\n");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

    let planner = OsvmCommandPlanner::new(true);

    // Test different types of queries
    let test_queries = [
        "show me all svms",
        "check my balance",
        "what's the status of the network?",
        "list all nodes",
        "show me examples of how to use osvm",
    ];

    for (i, query) in test_queries.iter().enumerate() {
        println!("📝 Test {}: {}\n", i + 1, query);

        match planner.create_plan(query).await {
            Ok(plan) => {
                println!("✅ Plan created successfully!");
                println!("   Reasoning: {}", plan.reasoning);
                println!("   Confidence: {:.1}%", plan.confidence * 100.0);
                println!("   Steps: {}", plan.steps.len());
                println!("   Auto-executable: {}", plan.auto_executable);

                for (j, step) in plan.steps.iter().enumerate() {
                    println!("\n   Step {}:", j + 1);
                    println!("      Command: {}", step.full_command);
                    println!("      Explanation: {}", step.explanation);
                    println!(
                        "      Requires confirmation: {}",
                        step.requires_confirmation
                    );
                }

                println!("\n   Expected outcome: {}", plan.expected_outcome);
                println!();
            }
            Err(e) => {
                println!("❌ Failed to create plan: {}\n", e);
            }
        }

        println!("{}\n", "─".repeat(50));
    }

    Ok(())
}
