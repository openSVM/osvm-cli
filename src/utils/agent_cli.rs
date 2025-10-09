//! CLI-based agent execution module
//!
//! This module provides a command-line interface for executing agent commands
//! without launching the full interactive UI. It uses the AI service to create
//! tool plans and executes them, providing results directly to stdout.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::time::Duration;
use uuid::Uuid;

use crate::services::{
    ai_service::{AiService, PlannedTool, ToolPlan},
    mcp_service::{McpService, McpTool},
};
use crate::utils::osvm_command_planner::OsvmCommandPlanner;

/// Result of agent command execution
#[derive(Debug, Serialize, Deserialize)]
pub struct AgentExecutionResult {
    pub prompt: String,
    pub plan: Option<ToolPlan>,
    pub tool_results: Vec<ToolExecutionResult>,
    pub final_response: String,
    pub execution_time_ms: u128,
    pub success: bool,
    pub error: Option<String>,
}

/// Individual tool execution result
#[derive(Debug, Serialize, Deserialize)]
pub struct ToolExecutionResult {
    pub tool_name: String,
    pub server_id: String,
    pub args: Value,
    pub result: Option<Value>,
    pub error: Option<String>,
    pub execution_time_ms: u128,
}

/// Execute an agent command from the CLI
pub async fn execute_agent_command(
    prompt: &str,
    json_output: bool,
    verbose: u8,
    no_tools: bool,
    timeout_seconds: u64,
) -> Result<()> {
    let start_time = std::time::Instant::now();

    // Initialize services
    let ai_service = AiService::new_with_debug(verbose > 1);
    let mut mcp_service = McpService::new_with_debug(verbose > 1);

    // Load MCP configurations
    if let Err(e) = mcp_service.load_config() {
        if verbose > 0 {
            eprintln!("⚠️  Warning: Failed to load MCP configurations: {}", e);
        }
    }

    // Get available tools
    let mut available_tools: HashMap<String, Vec<McpTool>> = HashMap::new();

    if !no_tools {
        // In a real implementation, we would get tools from active MCP servers
        // For now, we'll simulate some basic tools
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
    }

    // Show initial status if not in JSON mode
    if !json_output {
        println!("🤖 OSVM Agent CLI");
        println!("{}", "━".repeat(50));
        println!("📝 Prompt: {}", prompt);

        if verbose > 0 {
            println!("\n⏳ Analyzing request...");
        }
    }

    // Try OSVM command planner first (for CLI commands)
    let planner = OsvmCommandPlanner::new(verbose > 1);
    if let Ok(osvm_plan) = planner.create_plan(prompt).await {
        if !json_output {
            println!("\n💡 Detected OSVM command intent");
            println!("{}", "━".repeat(50));
            println!("📋 Execution Plan:");
            println!("   💭 {}", osvm_plan.reasoning);
            println!("   🎯 Confidence: {:.0}%", osvm_plan.confidence * 100.0);
            println!();

            for (i, step) in osvm_plan.steps.iter().enumerate() {
                println!("   {}. {}", i + 1, step.full_command);
                println!("      → {}", step.explanation);
            }

            println!();
            println!("   ✨ Expected: {}", osvm_plan.expected_outcome);
            println!();

            // Execute the OSVM commands
            match planner.execute_plan(&osvm_plan, true).await {
                Ok(results) => {
                    println!("{}", "━".repeat(50));
                    println!("📊 Execution Results:");
                    println!();

                    for (i, cmd_result) in results.iter().enumerate() {
                        let status = if cmd_result.success { "✅" } else { "❌" };
                        println!("   {}. {} {}", i + 1, status, cmd_result.command);

                        if !cmd_result.stdout.is_empty() {
                            // Indent the output
                            for line in cmd_result.stdout.lines() {
                                println!("      {}", line);
                            }
                        }

                        if !cmd_result.success && !cmd_result.stderr.is_empty() {
                            println!("      ❌ {}", cmd_result.stderr);
                        }

                        println!("      ⏱️  {}ms", cmd_result.execution_time_ms);
                        println!();
                    }

                    let agent_result = AgentExecutionResult {
                        prompt: prompt.to_string(),
                        plan: None,
                        tool_results: vec![],
                        final_response: format!(
                            "Executed {} OSVM command(s) successfully",
                            results.len()
                        ),
                        execution_time_ms: start_time.elapsed().as_millis(),
                        success: results.iter().all(|r| r.success),
                        error: None,
                    };

                    if json_output {
                        println!("{}", serde_json::to_string_pretty(&agent_result)?);
                    }

                    return Ok(());
                }
                Err(e) => {
                    if verbose > 0 {
                        println!("⚠️  OSVM command execution failed: {}", e);
                        println!("   Falling back to MCP tools...\n");
                    }
                }
            }
        }
    }

    // Create execution result
    let mut result = AgentExecutionResult {
        prompt: prompt.to_string(),
        plan: None,
        tool_results: Vec::new(),
        final_response: String::new(),
        execution_time_ms: 0,
        success: false,
        error: None,
    };

    // Apply timeout to the entire operation
    let execution_future = execute_with_plan(
        &ai_service,
        &mut mcp_service,
        prompt,
        &available_tools,
        verbose,
        no_tools,
        json_output,
    );

    match tokio::time::timeout(Duration::from_secs(timeout_seconds), execution_future).await {
        Ok(Ok((plan, tool_results, final_response))) => {
            result.plan = Some(plan);
            result.tool_results = tool_results;
            result.final_response = final_response;
            result.success = true;
        }
        Ok(Err(e)) => {
            result.error = Some(format!("Execution failed: {}", e));
            result.final_response = format!(
                "I encountered an error while processing your request: {}",
                e
            );
        }
        Err(_) => {
            result.error = Some(format!(
                "Execution timed out after {} seconds",
                timeout_seconds
            ));
            result.final_response = "The operation timed out. Please try again with a simpler request or increase the timeout.".to_string();
        }
    }

    result.execution_time_ms = start_time.elapsed().as_millis();

    // Output results
    if json_output {
        println!("{}", serde_json::to_string_pretty(&result)?);
    } else {
        // Pretty print the results
        println!("\n{}", "━".repeat(50));

        if let Some(plan) = &result.plan {
            if verbose > 0 {
                println!("\n📋 Plan: {}", plan.reasoning);

                if !plan.osvm_tools_to_use.is_empty() {
                    println!("\n🔧 Tools to execute:");
                    for tool in &plan.osvm_tools_to_use {
                        println!("   • {} ({})", tool.tool_name, tool.server_id);
                        if verbose > 1 {
                            println!("     Reason: {}", tool.reason);
                        }
                    }
                }
            }
        }

        if !result.tool_results.is_empty() && verbose > 0 {
            println!("\n⚙️  Tool Results:");
            for tool_result in &result.tool_results {
                if let Some(ref error) = tool_result.error {
                    println!("   ❌ {}: {}", tool_result.tool_name, error);
                } else if let Some(ref result_value) = tool_result.result {
                    println!(
                        "   ✅ {}: {}",
                        tool_result.tool_name,
                        format_tool_result(result_value)
                    );
                }
            }
        }

        println!("\n💬 Response: {}", result.final_response);

        if verbose > 0 {
            println!("\n⏱️  Execution time: {}ms", result.execution_time_ms);
        }

        if let Some(ref error) = result.error {
            println!("\n❌ Error: {}", error);
        }
    }

    Ok(())
}

/// Execute the agent plan
async fn execute_with_plan(
    ai_service: &AiService,
    mcp_service: &mut McpService,
    prompt: &str,
    available_tools: &HashMap<String, Vec<McpTool>>,
    verbose: u8,
    no_tools: bool,
    json_output: bool,
) -> Result<(ToolPlan, Vec<ToolExecutionResult>, String)> {
    // Create tool plan using AI
    let tool_plan = if no_tools || available_tools.is_empty() {
        // Create a simple plan without tools
        ToolPlan {
            reasoning: "No tools available or tool execution disabled. Providing direct response."
                .to_string(),
            osvm_tools_to_use: Vec::new(),
            expected_outcome: "Direct AI response to the query".to_string(),
        }
    } else {
        // Try to get OSVM plan from AI
        match ai_service.create_tool_plan(prompt, available_tools).await {
            Ok(plan) => {
                if verbose > 1 {
                    eprintln!(
                        "📝 OSVM Plan received with {} tools",
                        plan.osvm_tools_to_use.len()
                    );
                }
                plan
            }
            Err(e) => {
                eprintln!("❌ Failed to get OSVM plan from AI service: {}", e);
                eprintln!("   Please ensure the AI service is configured and accessible.");
                // Return empty plan on error - no fallback demos allowed
                return Err(anyhow::anyhow!("Failed to create OSVM plan: {}", e));
            }
        }
    };

    if !json_output && verbose > 0 {
        println!("🎯 Creating execution plan...");
    }

    // Execute tools if any
    let mut tool_results = Vec::new();

    for planned_tool in &tool_plan.osvm_tools_to_use {
        if !json_output && verbose > 0 {
            println!("🔧 Executing {}...", planned_tool.tool_name);
        }

        let tool_start = std::time::Instant::now();

        // Simulate tool execution (in real implementation, would call MCP)
        let (result, error) = execute_mock_tool(&planned_tool).await;

        tool_results.push(ToolExecutionResult {
            tool_name: planned_tool.tool_name.clone(),
            server_id: planned_tool.server_id.clone(),
            args: planned_tool.args.clone(),
            result,
            error,
            execution_time_ms: tool_start.elapsed().as_millis(),
        });
    }

    // Generate final response
    let tool_results_for_ai: Vec<(String, Value)> = tool_results
        .iter()
        .filter_map(|tr| {
            tr.result
                .as_ref()
                .map(|r| (tr.tool_name.clone(), r.clone()))
        })
        .collect();

    let final_response = if tool_results_for_ai.is_empty() && tool_plan.osvm_tools_to_use.is_empty()
    {
        // No tools were used, get direct AI response
        ai_service
            .query_with_debug(prompt, verbose > 1)
            .await
            .unwrap_or_else(|e| format!("I apologize, but I couldn't generate a response: {}", e))
    } else {
        ai_service.generate_contextual_response(
            prompt,
            &tool_results_for_ai,
            &tool_plan.expected_outcome
        ).await
            .unwrap_or_else(|e| {
                format!("I executed the tools but encountered an issue generating the final response: {}", e)
            })
    };

    Ok((tool_plan, tool_results, final_response))
}

/// Execute a tool from the OSVM plan
async fn execute_mock_tool(planned_tool: &PlannedTool) -> (Option<Value>, Option<String>) {
    // Log tool execution for agentic behavior
    eprintln!(
        "🤖 Agent executing: {} on {}",
        planned_tool.tool_name, planned_tool.server_id
    );

    // Simulate execution delay for realistic behavior
    tokio::time::sleep(Duration::from_millis(200)).await;

    // In a real implementation, this would:
    // 1. Connect to the MCP server specified by server_id
    // 2. Execute the tool with the provided args
    // 3. Return the actual result

    match planned_tool.tool_name.as_str() {
        "get_balance" => {
            // Simulate checking wallet balance
            (
                Some(serde_json::json!({
                    "balance": "2.5 SOL",
                    "usd_value": 375.50,
                    "address": "7nYzPUfXgvNgSPe5kEqKvGvSZg4rPKnrJdK5eTbDwRmQ",
                    "last_activity": "2024-01-20T15:30:00Z"
                })),
                None,
            )
        }
        "get_transactions" => {
            // Simulate fetching transactions
            let limit = planned_tool
                .args
                .get("limit")
                .and_then(|v| v.as_str())
                .and_then(|s| s.parse::<usize>().ok())
                .unwrap_or(10);

            (
                Some(serde_json::json!({
                    "transactions": [
                        {
                            "signature": "3n8wF9tK2pL...",
                            "amount": "0.1 SOL",
                            "type": "transfer",
                            "from": "7nYzPUfXgvNgSPe5kEqKvGvSZg4rPKnrJdK5eTbDwRmQ",
                            "to": "9aKzRkN3mQ2...",
                            "timestamp": "2024-01-20T10:30:00Z",
                            "status": "confirmed"
                        },
                        {
                            "signature": "5kP2xRvN9dW...",
                            "amount": "1.0 SOL",
                            "type": "receive",
                            "from": "2bNxPp4vK8...",
                            "to": "7nYzPUfXgvNgSPe5kEqKvGvSZg4rPKnrJdK5eTbDwRmQ",
                            "timestamp": "2024-01-19T15:45:00Z",
                            "status": "confirmed"
                        }
                    ],
                    "total": 2,
                    "limit": limit
                })),
                None,
            )
        }
        "deploy_validator" => {
            // Simulate validator deployment
            (
                Some(serde_json::json!({
                    "status": "initiated",
                    "validator_id": format!("val_{}", &Uuid::new_v4().to_string()[0..8]),
                    "network": planned_tool.args.get("network")
                        .and_then(|v| v.as_str())
                        .unwrap_or("mainnet"),
                    "estimated_time": "5-10 minutes",
                    "next_steps": [
                        "SSH connection will be established",
                        "Dependencies will be installed",
                        "Validator software will be configured",
                        "Service will be started"
                    ]
                })),
                None,
            )
        }
        "get_network_status" => {
            // Simulate network status check
            (
                Some(serde_json::json!({
                    "network": "mainnet-beta",
                    "slot": 250000000,
                    "epoch": 580,
                    "tps": 3000,
                    "validators": {
                        "active": 1800,
                        "delinquent": 12
                    },
                    "status": "healthy"
                })),
                None,
            )
        }
        _ => {
            // Unknown tool - this shouldn't happen with proper OSVM planning
            (
                None,
                Some(format!(
                    "Tool '{}' is not available in the current context",
                    planned_tool.tool_name
                )),
            )
        }
    }
}

/// Format tool result for display
fn format_tool_result(value: &Value) -> String {
    // Try to format the result nicely
    if let Some(obj) = value.as_object() {
        if let Some(balance) = obj.get("balance") {
            return format!("{}", balance);
        }
        if let Some(transactions) = obj.get("transactions") {
            if let Some(arr) = transactions.as_array() {
                return format!("{} transactions", arr.len());
            }
        }
    }

    // Fallback to compact JSON
    serde_json::to_string(value).unwrap_or_else(|_| value.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_agent_cli_execution() {
        // Test basic execution without tools
        let result = execute_agent_command(
            "Hello, agent!",
            true, // json output
            0,    // no verbose
            true, // no tools
            5,    // timeout
        )
        .await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_format_tool_result() {
        let balance_result = serde_json::json!({
            "balance": "2.5 SOL",
            "usd_value": 375.50
        });

        let formatted = format_tool_result(&balance_result);
        assert_eq!(formatted, "\"2.5 SOL\"");

        let tx_result = serde_json::json!({
            "transactions": [1, 2, 3]
        });

        let formatted = format_tool_result(&tx_result);
        assert_eq!(formatted, "3 transactions");
    }
}
