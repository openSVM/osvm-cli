use osvm::services::mcp_service::{McpService, McpTool};
use osvm::services::ai_service::{AiService, PlannedTool};
use std::collections::HashMap;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("Testing OSVM Planning System");
    println!("============================\n");

    // Initialize services
    let mut mcp_service = McpService::new_with_debug(true);
    mcp_service.load_config()?;

    let ai_service = AiService::new_with_debug(true);

    // List available MCP servers
    let servers = mcp_service.list_servers(None, None)?;
    println!("Available MCP servers:");
    for (id, config) in servers {
        println!("  • {} ({})", id, config.name);
    }
    println!();

    // Get tools from osvm-mcp
    if servers.contains_key("osvm-mcp") {
        println!("Fetching tools from osvm-mcp...");
        match mcp_service.list_tools("osvm-mcp").await {
            Ok(tools) => {
                println!("Found {} tools:", tools.len());
                for (i, tool) in tools.iter().enumerate() {
                    if i < 5 {
                        println!("  - {}: {}",
                            tool.name,
                            tool.description.as_ref().unwrap_or(&"No description".to_string()));
                    }
                }
                if tools.len() > 5 {
                    println!("  ... and {} more", tools.len() - 5);
                }
            }
            Err(e) => {
                println!("Failed to fetch tools: {}", e);
            }
        }
    }
    println!();

    // Test planning for different queries
    let test_queries = vec![
        "What is my wallet balance?",
        "Show recent transactions",
        "Get transaction abc123",
    ];

    let mut available_tools = HashMap::new();

    // Populate with osvm-mcp tools if available
    if servers.contains_key("osvm-mcp") {
        if let Ok(tools) = mcp_service.list_tools("osvm-mcp").await {
            available_tools.insert("osvm-mcp".to_string(), tools);
        }
    }

    for query in test_queries {
        println!("Query: \"{}\"", query);

        // Try AI planning (will fail without AI configured)
        match ai_service.create_tool_plan(query, &available_tools).await {
            Ok(plan) => {
                println!("  AI Plan: {}", plan.reasoning);
                for tool in &plan.osvm_tools_to_use {
                    println!("    - Use {}: {}", tool.tool_name, tool.reason);
                }
            }
            Err(_) => {
                println!("  AI planning not available (expected without AI service)");

                // Show fallback heuristic
                let lower = query.to_lowercase();
                if lower.contains("balance") {
                    println!("  Fallback: Would use get_balance or similar tool");
                } else if lower.contains("transaction") {
                    if lower.contains("abc123") {
                        println!("  Fallback: Would use get_transaction with signature");
                    } else {
                        println!("  Fallback: Would use list_transactions or similar");
                    }
                }
            }
        }
        println!();
    }

    Ok(())
}