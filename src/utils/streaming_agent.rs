//! Streaming agent CLI - Real-time terminal output mode
//!
//! This module provides streaming output to the terminal as the agent executes,
//! without launching the chat UI. It's ideal for one-off queries where you want
//! to see real-time progress and results directly in your terminal.
//!
//! Usage: `osvm query text here` - runs as a streaming agent query

use anyhow::{Context, Result};
use serde_json::Value;
use std::collections::HashMap;
use std::io::Write;

use crate::services::{
    ai_service::{AiService, PlannedTool, ToolPlan},
    mcp_service::{McpService, McpTool},
};

/// Execute agent command with real-time streaming output to terminal
pub async fn execute_streaming_agent(query: &str, verbose: u8) -> Result<()> {
    let start_time = std::time::Instant::now();

    // Print header
    println!("\n🤖 OSVM Agent - Streaming Mode");
    println!("{}", "━".repeat(60));
    println!("📝 Query: {}", query);
    println!("{}", "━".repeat(60));
    println!();

    // Initialize services
    let ai_service = AiService::new_with_debug(verbose > 1);
    let mut mcp_service = McpService::new_with_debug(verbose > 1);

    // Load MCP configurations
    if let Err(e) = mcp_service.load_config() {
        if verbose > 0 {
            eprintln!("⚠️  Warning: Failed to load MCP configurations: {}", e);
        }
    }

    // Get available tools from MCP service
    let mut available_tools: HashMap<String, Vec<McpTool>> = HashMap::new();

    // In a real implementation, would get tools from active MCP servers
    let mock_tools = vec![
        McpTool {
            name: "get_balance".to_string(),
            description: Some("Get wallet balance from blockchain".to_string()),
            input_schema: serde_json::json!({"type": "object", "properties": {}}),
        },
        McpTool {
            name: "get_transactions".to_string(),
            description: Some("Get recent transactions from wallet".to_string()),
            input_schema: serde_json::json!({"type": "object", "properties": {"limit": {"type": "number", "default": 10}}}),
        },
        McpTool {
            name: "deploy_validator".to_string(),
            description: Some("Deploy a new validator node".to_string()),
            input_schema: serde_json::json!({"type": "object", "properties": {"network": {"type": "string"}}}),
        },
        McpTool {
            name: "get_network_status".to_string(),
            description: Some("Get current network status and statistics".to_string()),
            input_schema: serde_json::json!({"type": "object", "properties": {}}),
        },
    ];

    if !mock_tools.is_empty() {
        available_tools.insert("blockchain_tools".to_string(), mock_tools);
    }

    // Step 1: AI Planning
    print!("🧠 AI Planning");
    std::io::stdout().flush()?;

    let tool_plan = match ai_service.create_tool_plan(query, &available_tools).await {
        Ok(plan) => {
            println!(" ✅");
            plan
        }
        Err(e) => {
            println!(" ❌");
            eprintln!("❌ Failed to create AI plan: {}", e);
            return Err(e);
        }
    };

    // Display plan details
    println!("\n📋 Plan Details:");
    println!("   Reasoning: {}", tool_plan.reasoning);
    println!("   Expected Outcome: {}", tool_plan.expected_outcome);

    if !tool_plan.osvm_tools_to_use.is_empty() {
        println!("\n🔧 Tools to Execute:");
        for (i, tool) in tool_plan.osvm_tools_to_use.iter().enumerate() {
            println!("   {}. {} (from {})", i + 1, tool.tool_name, tool.server_id);
            if verbose > 0 {
                println!("      Reason: {}", tool.reason);
            }
        }
    } else {
        println!("\n📌 No specific tools needed - will provide direct response");
    }

    println!();

    // Step 2: Execute Tools
    let mut tool_results = Vec::new();

    for (i, planned_tool) in tool_plan.osvm_tools_to_use.iter().enumerate() {
        print!("⚙️  Executing tool [{}/{}]: {}",
               i + 1,
               tool_plan.osvm_tools_to_use.len(),
               planned_tool.tool_name);
        std::io::stdout().flush()?;

        let tool_start = std::time::Instant::now();

        // Execute tool (mock for now)
        let (result, error) = execute_tool(&planned_tool).await;
        let elapsed = tool_start.elapsed().as_millis();

        if error.is_some() {
            println!(" ❌ ({}ms)", elapsed);
            println!("   Error: {}", error.as_ref().unwrap());
        } else {
            println!(" ✅ ({}ms)", elapsed);
            if let Some(ref result_value) = result {
                // Show key results
                print_tool_result(&planned_tool.tool_name, result_value);
            }
        }

        tool_results.push((planned_tool.clone(), result, error));
    }

    println!();

    // Step 3: Generate Response
    print!("💬 Generating Final Response");
    std::io::stdout().flush()?;

    let tool_results_for_ai: Vec<(String, Value)> = tool_results
        .iter()
        .filter_map(|(tool, result, _error)| {
            result
                .as_ref()
                .map(|r| (tool.tool_name.clone(), r.clone()))
        })
        .collect();

    let final_response = if tool_results_for_ai.is_empty() && tool_plan.osvm_tools_to_use.is_empty() {
        // No tools - direct response
        match ai_service.query_with_debug(query, verbose > 1).await {
            Ok(resp) => {
                println!(" ✅");
                resp
            }
            Err(e) => {
                println!(" ❌");
                eprintln!("❌ Failed to generate response: {}", e);
                format!("Error: {}", e)
            }
        }
    } else {
        // Generate contextual response with tool results
        match ai_service
            .generate_contextual_response(query, &tool_results_for_ai, &tool_plan.expected_outcome)
            .await
        {
            Ok(resp) => {
                println!(" ✅");
                resp
            }
            Err(e) => {
                println!(" ❌");
                eprintln!("❌ Failed to generate response: {}", e);
                format!("Error generating response: {}", e)
            }
        }
    };

    println!();

    // Step 4: Display Results
    println!("{}", "━".repeat(60));
    println!("📊 Result:");
    println!("{}", "━".repeat(60));
    println!();
    println!("{}", final_response);
    println!();

    // Display execution statistics
    let elapsed_ms = start_time.elapsed().as_millis();
    println!("{}", "━".repeat(60));
    println!("⏱️  Execution Time: {}ms", elapsed_ms);
    println!("✨ Done!");
    println!();

    Ok(())
}

/// Execute a tool with streaming output
async fn execute_tool(tool: &PlannedTool) -> (Option<Value>, Option<String>) {
    // Simulate tool execution with small delay
    tokio::time::sleep(std::time::Duration::from_millis(100)).await;

    match tool.tool_name.as_str() {
        "get_balance" => (
            Some(serde_json::json!({
                "balance": "2.5 SOL",
                "usd_value": 375.50,
                "address": "7nYzPUfXgvNgSPe5kEqKvGvSZg4rPKnrJdK5eTbDwRmQ",
                "last_activity": "2024-01-20T15:30:00Z"
            })),
            None,
        ),
        "get_transactions" => (
            Some(serde_json::json!({
                "transactions": [
                    {
                        "signature": "3n8wF9tK2pL...",
                        "amount": "0.1 SOL",
                        "type": "transfer"
                    },
                    {
                        "signature": "5kP2xRvN9dW...",
                        "amount": "1.0 SOL",
                        "type": "receive"
                    }
                ],
                "total": 2
            })),
            None,
        ),
        "deploy_validator" => (
            Some(serde_json::json!({
                "status": "initiated",
                "validator_id": "val_abc12345",
                "network": tool.args.get("network").and_then(|v| v.as_str()).unwrap_or("mainnet"),
                "estimated_time": "5-10 minutes"
            })),
            None,
        ),
        "get_network_status" => (
            Some(serde_json::json!({
                "network": "mainnet-beta",
                "slot": 250000000,
                "epoch": 580,
                "tps": 3000,
                "validators": {"active": 1800, "delinquent": 12},
                "status": "healthy"
            })),
            None,
        ),
        _ => (
            None,
            Some(format!(
                "Tool '{}' is not available in the current context",
                tool.tool_name
            )),
        ),
    }
}

/// Print tool result with formatting
fn print_tool_result(tool_name: &str, result: &Value) {
    match tool_name {
        "get_balance" => {
            if let Some(balance) = result.get("balance") {
                println!("   Balance: {}", balance);
            }
        }
        "get_transactions" => {
            if let Some(txs) = result.get("transactions").and_then(|v| v.as_array()) {
                println!("   Retrieved {} transactions", txs.len());
                for (i, tx) in txs.iter().take(3).enumerate() {
                    if let Some(sig) = tx.get("signature") {
                        println!("     {}: {}", i + 1, sig);
                    }
                }
                if txs.len() > 3 {
                    println!("     ... and {} more", txs.len() - 3);
                }
            }
        }
        "deploy_validator" => {
            if let Some(status) = result.get("status") {
                println!("   Status: {}", status);
            }
            if let Some(id) = result.get("validator_id") {
                println!("   ID: {}", id);
            }
        }
        "get_network_status" => {
            if let Some(slot) = result.get("slot") {
                println!("   Current Slot: {}", slot);
            }
            if let Some(status) = result.get("status") {
                println!("   Network Status: {}", status);
            }
        }
        _ => {
            println!("   Result: {}", serde_json::to_string(result).unwrap_or_default());
        }
    }
}
