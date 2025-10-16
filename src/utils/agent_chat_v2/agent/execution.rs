//! Agent execution logic for AI processing and tool execution

use anyhow::{anyhow, Context, Result};
use log::{debug, error, info, warn};
use serde_json::Value;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::Ordering;
use std::time::Duration;
use uuid::Uuid;

use crate::services::{
    ai_service::PlannedTool,
    isolation_config::IsolationConfig,
    mcp_service::McpTool,
    unikernel_runtime::{UnikernelConfig, UnikernelRuntime},
};

use super::super::state::AdvancedChatState;
use super::super::types::{AgentState, ChatMessage};

impl AdvancedChatState {
    pub async fn process_input_async(&self, session_id: Uuid, input: String) -> Result<()> {
        // Set agent state to thinking
        self.set_agent_state(session_id, AgentState::Thinking);
        let _ = self.add_message_to_session(session_id, ChatMessage::User(input.clone()));
        let _ = self.add_message_to_session(
            session_id,
            ChatMessage::Processing {
                message: "Analyzing your request...".to_string(),
                spinner_index: self.spinner_state.load(Ordering::Relaxed),
            },
        );

        // First, refresh available tools to ensure we have the latest
        if let Err(e) = self.refresh_tools_from_mcp().await {
            warn!("Failed to refresh tools: {}", e);
        }

        self.set_agent_state(session_id, AgentState::Planning);
        let _ = self.add_message_to_session(
            session_id,
            ChatMessage::Processing {
                message: "Creating execution plan...".to_string(),
                spinner_index: self.spinner_state.load(Ordering::Relaxed),
            },
        );

        // Get available tools
        let available_tools = match self.available_tools.read() {
            Ok(tools) => tools.clone(),
            Err(e) => {
                error!("Failed to read available tools: {}", e);
                HashMap::new() // Use empty map as fallback
            }
        };

        // Use the proper AI service method to create tool plan
        let tool_plan_result = tokio::time::timeout(
            Duration::from_secs(120), // Extended timeout for comprehensive AI responses
            self.ai_service.create_tool_plan(&input, &available_tools),
        )
        .await;

        // Helper closure to build heuristic tools based on input
        let build_heuristic_plan = |text: &str| -> Vec<PlannedTool> {
            let mut tools = Vec::new();
            let lc = text.to_lowercase();

            // Batch validator analysis
            if (lc.contains("validator") || lc.contains("validators"))
                && (lc.contains("100") || lc.contains("top") || lc.contains("analyze")) {
                tools.push(PlannedTool {
                    server_id: "local_sim".into(),
                    tool_name: "analyze_batch_validators".into(),
                    args: serde_json::json!({
                        "count": 100,
                        "metrics": ["stake", "commission", "uptime", "vote_credits", "delinquency"]
                    }),
                    reason: "Batch analyze validators with simulated data".into(),
                });
            }

            // Batch token analysis
            if (lc.contains("token") || lc.contains("tokens"))
                && (lc.contains("50") || lc.contains("top") || lc.contains("portfolio")) {
                tools.push(PlannedTool {
                    server_id: "local_sim".into(),
                    tool_name: "analyze_batch_tokens".into(),
                    args: serde_json::json!({
                        "count": 50,
                        "timeframe": "30d",
                        "metrics": ["price", "volume", "market_cap"]
                    }),
                    reason: "Batch analyze tokens with market data".into(),
                });
            }

            // Batch account analysis
            if (lc.contains("account") || lc.contains("accounts"))
                && (lc.contains("transaction") || lc.contains("pattern")) {
                tools.push(PlannedTool {
                    server_id: "local_sim".into(),
                    tool_name: "analyze_batch_accounts".into(),
                    args: serde_json::json!({
                        "account_count": 50,
                        "transaction_limit": 100
                    }),
                    reason: "Batch analyze account transaction patterns".into(),
                });
            }

            // Single balance query
            if lc.contains("balance") && !lc.contains("validator") && !lc.contains("token") {
                tools.push(PlannedTool {
                    server_id: "local_sim".into(),
                    tool_name: "get_balance".into(),
                    args: serde_json::json!({}),
                    reason: "Heuristic: user asked about balance".into(),
                });
            }

            // Single transaction query
            if (lc.contains("transaction") || lc.contains("transactions") || lc.contains("tx"))
                && !lc.contains("account") && !lc.contains("validator") {
                tools.push(PlannedTool {
                    server_id: "local_sim".into(),
                    tool_name: "get_transactions".into(),
                    args: serde_json::json!({}),
                    reason: "Heuristic: user asked about transactions".into(),
                });
            }

            tools
        };

        // Determine if any MCP servers/tools are configured
        let no_configured_tools = available_tools.is_empty();

        match tool_plan_result {
            Err(_) => {
                warn!("AI planning timed out");
                let _ = self.add_message_to_session(
                    session_id,
                    ChatMessage::Error(
                        "AI planning timed out. Using heuristic fallback.".to_string(),
                    ),
                );
                if no_configured_tools {
                    let _ = self
                        .run_heuristic_fallback(session_id, &input, build_heuristic_plan(&input))
                        .await;
                } else {
                    let _ = self.simple_response(session_id, &input).await;
                }
            }
            Ok(Err(e)) => {
                error!("AI service failed: {}", e);
                let _ = self.add_message_to_session(
                    session_id,
                    ChatMessage::Error(
                        "AI planning service failed. Using heuristic fallback.".to_string(),
                    ),
                );
                if no_configured_tools {
                    let _ = self
                        .run_heuristic_fallback(session_id, &input, build_heuristic_plan(&input))
                        .await;
                } else {
                    let _ = self.simple_response(session_id, &input).await;
                }
            }
            Ok(Ok(tool_plan)) => {
                // Successfully got tool plan from AI service
                let _ = self.add_message_to_session(
                    session_id,
                    ChatMessage::AgentPlan(tool_plan.osvm_tools_to_use.clone()),
                );

                if tool_plan.osvm_tools_to_use.is_empty() {
                    // No tools needed according to AI
                    if no_configured_tools {
                        // Try heuristic plan instead
                        let heur = build_heuristic_plan(&input);
                        if !heur.is_empty() {
                            let _ = self.add_message_to_session(
                                session_id,
                                ChatMessage::AgentPlan(heur.clone()),
                            );
                            let _ = self.run_heuristic_fallback(session_id, &input, heur).await;
                        } else {
                            let _ = self.simple_response(session_id, &input).await;
                        }
                    } else {
                        let _ = self.simple_response(session_id, &input).await;
                    }
                } else {
                    // Phase 2: Check if we have raw OVSM plan for execution engine
                    if let Some(ref raw_ovsm) = tool_plan.raw_ovsm_plan {
                        // Execute using OVSM engine with runtime branching
                        debug!("Phase 2: Executing plan using OVSM execution engine");
                        let _ = self.execute_ovsm_plan(session_id, raw_ovsm, &input).await;
                    } else {
                        // Fallback: Execute tools iteratively (Phase 1 behavior)
                        debug!("No raw OVSM plan available, using iterative execution");
                        let _ = self.execute_tools_iteratively(session_id, tool_plan.osvm_tools_to_use, &input, &available_tools).await;
                    }

                    // Generate final response with all executed tools
                    let _ = self
                        .generate_final_response(session_id, &input, &tool_plan.expected_outcome)
                        .await;
                }
            }
        }

        // ALWAYS cleanup processing messages, even if errors occurred above
        // This ensures the spinner stops regardless of success/failure
        self.cleanup_processing_messages(session_id);

        self.set_agent_state(session_id, AgentState::Idle);

        // Generate suggestions after agent completes
        let _ = self.generate_reply_suggestions(session_id).await;

        Ok(())
    }

    /// Helper to ensure cleanup always happens
    fn cleanup_processing_messages(&self, session_id: Uuid) {
        // Remove ALL processing messages from the session
        loop {
            match self.remove_last_processing_message(session_id) {
                Ok(()) => {
                    // Check if there are more processing messages
                    if let Some(session) = self.get_session_by_id(session_id) {
                        let has_more_processing = session
                            .messages
                            .iter()
                            .any(|msg| matches!(msg, ChatMessage::Processing { .. }));
                        if !has_more_processing {
                            break; // No more processing messages
                        }
                    } else {
                        break; // Session not found
                    }
                }
                Err(_) => break, // Error or no processing message found
            }
        }
    }

    // Heuristic fallback execution simulating basic tools so user sees end-to-end flow
    async fn run_heuristic_fallback(
        &self,
        session_id: Uuid,
        original_input: &str,
        tools: Vec<PlannedTool>,
    ) -> Result<()> {
        if tools.is_empty() {
            // Nothing heuristic matched; fall back to simple
            return self.simple_response(session_id, original_input).await;
        }
        self.add_message_to_session(session_id, ChatMessage::AgentPlan(tools.clone()))?;
        for planned_tool in tools {
            self.execute_planned_tool(session_id, planned_tool).await?;
        }
        self.generate_final_response(session_id, original_input, "Heuristic simulated execution")
            .await?;
        Ok(())
    }

    async fn execute_planned_tool(
        &self,
        session_id: Uuid,
        planned_tool: PlannedTool,
    ) -> Result<()> {
        let execution_id = Uuid::new_v4().to_string();

        self.set_agent_state(
            session_id,
            AgentState::ExecutingTool(planned_tool.tool_name.clone()),
        );
        self.add_message_to_session(
            session_id,
            ChatMessage::ToolCall {
                tool_name: planned_tool.tool_name.clone(),
                description: planned_tool.reason.clone(),
                args: Some(planned_tool.args.clone()),
                execution_id: execution_id.clone(),
            },
        )?;

        // Execute the tool using MCP service
        match self.call_mcp_tool(&planned_tool).await {
            Ok(result) => {
                self.add_message_to_session(
                    session_id,
                    ChatMessage::ToolResult {
                        tool_name: planned_tool.tool_name,
                        result,
                        execution_id,
                    },
                )?;
            }
            Err(e) => {
                error!("Tool execution failed: {}", e);
                self.add_message_to_session(
                    session_id,
                    ChatMessage::Error(format!("Tool {} failed: {}", planned_tool.tool_name, e)),
                )?;
            }
        }

        Ok(())
    }

    async fn call_mcp_tool(&self, planned_tool: &PlannedTool) -> Result<Value> {
        // Load isolation configuration
        let isolation_config = match IsolationConfig::load() {
            Ok(config) => config,
            Err(e) => {
                warn!(
                    "Failed to load isolation config: {}, using direct MCP execution",
                    e
                );
                return self.call_mcp_tool_direct(planned_tool).await;
            }
        };

        // Check if this tool should be executed in a unikernel
        if isolation_config.should_use_unikernel(&planned_tool.server_id, &planned_tool.tool_name) {
            // Attempt unikernel execution with fallback
            match self
                .execute_tool_in_unikernel(planned_tool, &isolation_config)
                .await
            {
                Ok(result) => return Ok(result),
                Err(e) => {
                    warn!(
                        "Unikernel execution failed: {}, falling back to direct MCP",
                        e
                    );
                    return self.call_mcp_tool_direct(planned_tool).await;
                }
            }
        }

        // Use direct MCP execution (MicroVM mode or no isolation config)
        self.call_mcp_tool_direct(planned_tool).await
    }

    /// Execute tool in ephemeral unikernel for maximum isolation
    async fn execute_tool_in_unikernel(
        &self,
        planned_tool: &PlannedTool,
        isolation_config: &IsolationConfig,
    ) -> Result<Value> {
        info!(
            "Executing tool '{}' in ephemeral unikernel for server '{}'",
            planned_tool.tool_name, planned_tool.server_id
        );

        // Get tool-specific configuration
        let tool_config =
            isolation_config.get_tool_config(&planned_tool.server_id, &planned_tool.tool_name);

        // Validate unikernel image is configured
        let unikernel_image = match &tool_config.unikernel_image {
            Some(img) => img.clone(),
            None => {
                warn!(
                    "No unikernel image configured for tool '{}' on server '{}', falling back",
                    planned_tool.tool_name, planned_tool.server_id
                );
                return Err(anyhow!("No unikernel image configured"));
            }
        };

        // Create unikernel runtime
        let unikernel_runtime =
            UnikernelRuntime::new(PathBuf::from(&isolation_config.unikernel_dir))
                .context("Failed to create unikernel runtime")?;

        // Build unikernel configuration
        use crate::services::unikernel_runtime::UnikernelLauncher;

        let unikernel_config = UnikernelConfig {
            image_path: PathBuf::from(unikernel_image),
            mounts: tool_config.mounts.clone(),
            memory_mb: tool_config.memory_mb,
            vcpus: tool_config.vcpus,
            tool_name: planned_tool.tool_name.clone(),
            server_id: planned_tool.server_id.clone(),
            launcher: UnikernelLauncher::Unikraft,
            kraft_config: None,
            vsock_cid: None, // Will be auto-allocated
        };

        // Spawn the unikernel (~100ms overhead)
        let handle = unikernel_runtime
            .spawn_unikernel(unikernel_config)
            .await
            .context("Failed to spawn unikernel")?;

        // Execute the tool in the isolated unikernel (pass runtime for vsock communication)
        let result = handle
            .execute_tool(
                &planned_tool.tool_name,
                Some(planned_tool.args.clone()),
                &unikernel_runtime,
            )
            .await;

        // Always terminate the ephemeral unikernel
        handle.terminate();

        match result {
            Ok(value) => {
                info!(
                    "Successfully executed tool '{}' in unikernel",
                    planned_tool.tool_name
                );
                Ok(value)
            }
            Err(e) => {
                error!("Unikernel tool execution failed: {}", e);
                Err(e)
            }
        }
    }

    /// Direct MCP tool execution without unikernel isolation
    async fn call_mcp_tool_direct(&self, planned_tool: &PlannedTool) -> Result<Value> {
        // Handle local_sim batch tools with mock data
        if planned_tool.server_id == "local_sim" {
            return match planned_tool.tool_name.as_str() {
                "analyze_batch_validators" => {
                    let count = planned_tool.args["count"].as_u64().unwrap_or(10);
                    info!("Simulating batch analysis of {} validators", count);

                    // Simulate processing time
                    tokio::time::sleep(Duration::from_millis(800)).await;

                    Ok(serde_json::json!({
                        "status": "success",
                        "validators_analyzed": count,
                        "execution_time_ms": 1500,
                        "data": {
                            "top_performers": [
                                {"name": "Validator Alpha", "stake_sol": 2_500_000, "commission": 5.0, "uptime": 99.8},
                                {"name": "Validator Beta", "stake_sol": 2_200_000, "commission": 4.5, "uptime": 99.6},
                                {"name": "Validator Gamma", "stake_sol": 1_900_000, "commission": 5.5, "uptime": 99.4},
                                {"name": "Validator Delta", "stake_sol": 1_700_000, "commission": 6.0, "uptime": 99.2},
                                {"name": "Validator Epsilon", "stake_sol": 1_500_000, "commission": 5.0, "uptime": 99.0}
                            ],
                            "average_metrics": {
                                "stake_sol": 1_250_000,
                                "commission_percent": 5.2,
                                "uptime_percent": 96.4,
                                "vote_credits_avg": 458_230
                            },
                            "total_stake": count * 1_250_000,
                            "anomalies_detected": 3,
                            "anomalies": [
                                {"validator": "Validator-42", "issue": "High commission (12%)", "severity": "medium"},
                                {"validator": "Validator-78", "issue": "Low uptime (87%)", "severity": "high"},
                                {"validator": "Validator-91", "issue": "Recent delinquency", "severity": "medium"}
                            ]
                        }
                    }))
                },
                "analyze_batch_tokens" => {
                    let count = planned_tool.args["count"].as_u64().unwrap_or(10);
                    info!("Simulating batch analysis of {} tokens", count);

                    tokio::time::sleep(Duration::from_millis(600)).await;

                    Ok(serde_json::json!({
                        "status": "success",
                        "tokens_analyzed": count,
                        "execution_time_ms": 1200,
                        "data": {
                            "top_gainers": [
                                {"symbol": "BONK", "price_change_30d": 45.2, "volume_24h": 15_000_000},
                                {"symbol": "JUP", "price_change_30d": 32.8, "volume_24h": 22_000_000},
                                {"symbol": "WIF", "price_change_30d": 28.5, "volume_24h": 8_500_000}
                            ],
                            "average_metrics": {
                                "price_change_30d": 5.4,
                                "volume_24h_usd": 2_500_000,
                                "market_cap_avg": 125_000_000
                            },
                            "volatility_index": 3.2,
                            "correlation_matrix": "Available in detailed view"
                        }
                    }))
                },
                "analyze_batch_accounts" => {
                    let account_count = planned_tool.args["account_count"].as_u64().unwrap_or(10);
                    let tx_limit = planned_tool.args["transaction_limit"].as_u64().unwrap_or(100);
                    info!("Simulating batch analysis of {} accounts ({} tx each)", account_count, tx_limit);

                    tokio::time::sleep(Duration::from_millis(1000)).await;

                    Ok(serde_json::json!({
                        "status": "success",
                        "accounts_analyzed": account_count,
                        "total_transactions": account_count * tx_limit,
                        "execution_time_ms": 2500,
                        "data": {
                            "transaction_patterns": {
                                "high_frequency_traders": 12,
                                "holders": 25,
                                "mixed_activity": 13
                            },
                            "anomalies": [
                                {"account": "7x...abc", "pattern": "Wash trading suspected", "confidence": 0.78},
                                {"account": "9w...def", "pattern": "Bot activity detected", "confidence": 0.91}
                            ],
                            "network_analysis": {
                                "clusters_detected": 5,
                                "interconnected_accounts": 18
                            },
                            "average_tx_per_account": tx_limit,
                            "total_volume_sol": account_count * tx_limit * 10
                        }
                    }))
                },
                "get_balance" => {
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    Ok(serde_json::json!({
                        "balance_sol": 42.5,
                        "balance_lamports": 42_500_000_000u64,
                        "status": "success"
                    }))
                },
                "get_transactions" => {
                    tokio::time::sleep(Duration::from_millis(200)).await;
                    Ok(serde_json::json!({
                        "transactions": [
                            {"signature": "3x...abc", "type": "transfer", "amount": 1.5},
                            {"signature": "5y...def", "type": "swap", "amount": 10.0}
                        ],
                        "count": 2,
                        "status": "success"
                    }))
                },
                _ => Ok(serde_json::json!({
                    "error": format!("Unknown local_sim tool: {}", planned_tool.tool_name),
                    "status": "unknown_tool"
                }))
            };
        }

        // Try to call real MCP tool first
        let mut mcp_service = self.mcp_service.lock().await;

        // Check if the server is initialized
        if let Some(server_config) = mcp_service.get_server(&planned_tool.server_id) {
            if server_config.enabled {
                // Initialize the server if needed
                if let Err(e) = mcp_service.initialize_server(&planned_tool.server_id).await {
                    error!(
                        "Failed to initialize MCP server '{}': {}",
                        planned_tool.server_id, e
                    );
                    // Return error info instead of falling back silently
                    return Ok(serde_json::json!({
                        "error": format!("Failed to initialize MCP server '{}': {}", planned_tool.server_id, e),
                        "status": "initialization_failed",
                        "server_id": planned_tool.server_id,
                        "tool_name": planned_tool.tool_name
                    }));
                }

                // Try to call the actual tool
                match mcp_service
                    .call_tool(
                        &planned_tool.server_id,
                        &planned_tool.tool_name,
                        Some(planned_tool.args.clone()),
                    )
                    .await
                {
                    Ok(result) => {
                        info!(
                            "Successfully executed MCP tool '{}' on server '{}'",
                            planned_tool.tool_name, planned_tool.server_id
                        );
                        return Ok(result);
                    }
                    Err(e) => {
                        error!(
                            "MCP tool call failed for '{}' on server '{}': {}",
                            planned_tool.tool_name, planned_tool.server_id, e
                        );
                        // Return detailed error instead of falling back silently
                        return Ok(serde_json::json!({
                            "error": format!("MCP tool execution failed: {}", e),
                            "status": "execution_failed",
                            "server_id": planned_tool.server_id,
                            "tool_name": planned_tool.tool_name,
                            "hint": "Check if the MCP server is properly configured and running. Use 'osvm mcp list' to see configured servers and 'osvm mcp test <server_id>' to test connectivity."
                        }));
                    }
                }
            } else {
                warn!(
                    "MCP server '{}' is disabled. Enable it with 'osvm mcp enable {}'",
                    planned_tool.server_id, planned_tool.server_id
                );
                return Ok(serde_json::json!({
                    "error": format!("MCP server '{}' is disabled", planned_tool.server_id),
                    "status": "server_disabled",
                    "server_id": planned_tool.server_id,
                    "hint": format!("Enable the server with: osvm mcp enable {}", planned_tool.server_id)
                }));
            }
        } else {
            error!(
                "MCP server '{}' not found in configuration",
                planned_tool.server_id
            );
            return Ok(serde_json::json!({
                "error": format!("MCP server '{}' not found", planned_tool.server_id),
                "status": "server_not_found",
                "server_id": planned_tool.server_id,
                "available_servers": mcp_service.list_servers()
                    .iter()
                    .map(|(id, _)| id.as_str())
                    .collect::<Vec<_>>(),
                "hint": "Add the server with 'osvm mcp add-github <server_id> <github_url>' or check available servers with 'osvm mcp list'"
            }));
        }
    }

    async fn generate_final_response(
        &self,
        session_id: Uuid,
        original_input: &str,
        expected_outcome: &str,
    ) -> Result<()> {
        // Get the recent tool results to inform the response
        let session = self
            .get_session_by_id(session_id)
            .ok_or_else(|| anyhow!("Session not found"))?;
        let tool_results: Vec<(String, Value)> = session
            .messages
            .iter()
            .rev()
            .filter_map(|msg| match msg {
                ChatMessage::ToolResult {
                    tool_name, result, ..
                } => Some((tool_name.clone(), result.clone())),
                _ => None,
            })
            .collect();

        // Use the AI service's contextual response generation
        match self
            .ai_service
            .generate_contextual_response(original_input, &tool_results, expected_outcome)
            .await
        {
            Ok(response) => {
                self.add_message_to_session(session_id, ChatMessage::Agent(response))?;
            }
            Err(e) => {
                error!("Failed to generate final response: {}", e);
                self.add_message_to_session(session_id, ChatMessage::Agent(
                    "I've completed the requested operations. Please check the tool results above.".to_string()
                ))?;
            }
        }

        Ok(())
    }

    async fn simple_response(&self, session_id: Uuid, input: &str) -> Result<()> {
        // Try to get a direct AI response without tools
        match self.ai_service.query_with_debug(input, false).await {
            Ok(response) => {
                self.add_message_to_session(session_id, ChatMessage::Agent(response))?;
            }
            Err(e) => {
                warn!("AI service failed for simple response: {}", e);
                // Fallback response logic
                let response = if input.to_lowercase().contains("balance") {
                    "I can help you check your wallet balance. However, I need MCP tools to be properly configured to fetch real data."
                } else if input.to_lowercase().contains("transaction") {
                    "I can help you with transaction history. Please make sure MCP servers are configured for blockchain operations."
                } else {
                    "I understand you're asking about blockchain operations. Please ensure MCP servers are configured so I can assist you with real data."
                };

                self.add_message_to_session(session_id, ChatMessage::Agent(response.to_string()))?;
            }
        }
        Ok(())
    }

    // New method to check for follow-up actions based on tool results
    async fn check_for_follow_up_actions(
        &self,
        original_input: &str,
        tool_results: &[(String, Value)],
        available_tools: &HashMap<String, Vec<McpTool>>,
    ) -> Result<Vec<PlannedTool>> {
        // Create a context string with the results
        let results_context = tool_results
            .iter()
            .map(|(name, result)| {
                format!(
                    "{}: {}",
                    name,
                    serde_json::to_string(result).unwrap_or_default()
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        let follow_up_prompt = format!(
            "Based on the user's request: '{}'\n\n\
            And these tool execution results:\n{}\n\n\
            Do we need any follow-up tools? If yes, create an OSVM plan for the next steps.",
            original_input, results_context
        );

        // Use a shorter timeout for follow-up checks
        match tokio::time::timeout(
            Duration::from_secs(10),
            self.ai_service
                .create_tool_plan(&follow_up_prompt, available_tools),
        )
        .await
        {
            Ok(Ok(plan)) if !plan.osvm_tools_to_use.is_empty() => Ok(plan.osvm_tools_to_use),
            _ => Ok(Vec::new()), // No follow-up needed or error occurred
        }
    }

    /// Phase 2: Execute OVSM plan using the execution engine with runtime branching
    async fn execute_ovsm_plan(
        &self,
        session_id: Uuid,
        raw_ovsm_plan: &str,
        _original_input: &str,
    ) -> Result<()> {
        info!("Phase 2: Executing OVSM plan with execution engine");

        // Get all available tools to register them with the executor
        let available_tools = match self.available_tools.read() {
            Ok(tools) => tools.clone(),
            Err(e) => {
                error!("Failed to read available tools: {}", e);
                return Err(anyhow!("Failed to read available tools: {}", e));
            }
        };

        // Register all available tools with OVSM executor
        let executor = self.ovsm_executor.lock().await;
        for (server_id, tool_list) in available_tools.iter() {
            for tool in tool_list {
                let tool_wrapper = McpToolWrapper {
                    state: self.clone(),
                    session_id,
                    server_id: server_id.clone(),
                    tool_name: tool.name.clone(),
                };
                executor
                    .register_tool(tool.name.clone(), Box::new(tool_wrapper))
                    .await?;
            }
        }

        // Execute the plan
        match executor.execute_plan(raw_ovsm_plan).await {
            Ok(result) => {
                info!(
                    "Phase 2: OVSM execution completed successfully (confidence: {:.2})",
                    result.confidence
                );
                debug!(
                    "OVSM execution metadata: {} tools called, {} branches taken, {} warnings, execution time: {}ms",
                    result.tools_called.len(),
                    result.branches_taken.len(),
                    result.warnings.len(),
                    result.execution_time_ms
                );

                // Log execution details
                if !result.branches_taken.is_empty() {
                    debug!("Branch decisions: {:?}", result.branches_taken);
                }
                if !result.warnings.is_empty() {
                    warn!("OVSM execution warnings: {:?}", result.warnings);
                }

                // Add execution metadata as a chat message (optional - for debugging)
                if log::log_enabled!(log::Level::Debug) {
                    let metadata_msg = format!(
                        "📊 Execution Stats: {} tools, {} branches, {}ms",
                        result.tools_called.len(),
                        result.branches_taken.len(),
                        result.execution_time_ms
                    );
                    let _ = self.add_message_to_session(
                        session_id,
                        ChatMessage::AgentThinking(metadata_msg),
                    );
                }

                Ok(())
            }
            Err(e) => {
                error!("Phase 2: OVSM execution failed: {}", e);
                let _ = self.add_message_to_session(
                    session_id,
                    ChatMessage::Error(format!("Plan execution failed: {}", e)),
                );
                Err(e)
            }
        }
    }

    /// Phase 1 fallback: Execute tools iteratively without OVSM engine
    async fn execute_tools_iteratively(
        &self,
        session_id: Uuid,
        tools: Vec<PlannedTool>,
        input: &str,
        available_tools: &HashMap<String, Vec<McpTool>>,
    ) -> Result<()> {
        let mut executed_tools = Vec::new();
        let mut iteration_count = 0;
        let max_iterations = 3; // Reduced to prevent excessive loops

        let mut current_tools = tools;
        let mut executed_tool_signatures = std::collections::HashSet::new();

        while !current_tools.is_empty() && iteration_count < max_iterations {
            iteration_count += 1;

            // Filter out duplicate tools to prevent infinite loops
            current_tools.retain(|tool| {
                let signature = format!("{}:{:?}", tool.tool_name, tool.args);
                !executed_tool_signatures.contains(&signature)
            });

            if current_tools.is_empty() {
                warn!("All remaining tools are duplicates, breaking execution loop");
                break;
            }

            // Execute current batch of tools
            for planned_tool in &current_tools {
                let signature = format!("{}:{:?}", planned_tool.tool_name, planned_tool.args);
                executed_tool_signatures.insert(signature);

                let _ = self
                    .execute_planned_tool(session_id, planned_tool.clone())
                    .await;
                executed_tools.push(planned_tool.clone());
            }

            // Check if we need follow-up actions based on results
            let session = match self.get_session_by_id(session_id) {
                Some(s) => s,
                None => break, // Session not found, exit loop
            };

            let recent_results: Vec<(String, Value)> = session
                .messages
                .iter()
                .rev()
                .filter_map(|msg| match msg {
                    ChatMessage::ToolResult {
                        tool_name, result, ..
                    } => Some((tool_name.clone(), result.clone())),
                    _ => None,
                })
                .take(current_tools.len()) // Get results for current batch of tools
                .collect();

            // Ask AI if we need follow-up tools based on results
            if iteration_count < max_iterations && !recent_results.is_empty() {
                match self
                    .check_for_follow_up_actions(input, &recent_results, available_tools)
                    .await
                {
                    Ok(mut follow_up_tools) if !follow_up_tools.is_empty() => {
                        // Filter follow-up tools to remove duplicates
                        follow_up_tools.retain(|tool| {
                            let signature = format!("{}:{:?}", tool.tool_name, tool.args);
                            !executed_tool_signatures.contains(&signature)
                        });

                        if !follow_up_tools.is_empty() {
                            let _ = self.add_message_to_session(
                                session_id,
                                ChatMessage::AgentThinking(format!(
                                    "Executing {} follow-up actions...",
                                    follow_up_tools.len()
                                )),
                            );
                            current_tools = follow_up_tools;
                        } else {
                            warn!("All follow-up tools are duplicates, breaking execution loop");
                            break;
                        }
                    }
                    _ => break, // No more tools needed
                }
            } else {
                break;
            }
        }

        Ok(())
    }
}

/// Phase 2: Wrapper that implements McpToolExecutor for OVSM executor
struct McpToolWrapper {
    state: AdvancedChatState,
    session_id: Uuid,
    server_id: String,
    tool_name: String,
}

impl crate::services::ovsm_executor::McpToolExecutor for McpToolWrapper {
    fn execute(&self, args: &serde_json::Value) -> Result<serde_json::Value> {
        // We need to run async code in a sync context
        // Use tokio's block_on from a new runtime
        let state = self.state.clone();
        let session_id = self.session_id;
        let server_id = self.server_id.clone();
        let tool_name = self.tool_name.clone();
        let args = args.clone();

        // Create PlannedTool from OVSM execution context
        let planned_tool = PlannedTool {
            server_id: server_id.clone(),
            tool_name: tool_name.clone(),
            args: args.clone(),
            reason: format!("OVSM plan execution: {}", tool_name),
        };

        // We need to use a runtime to execute async code
        // Try to get the current runtime, otherwise create one
        let result = match tokio::runtime::Handle::try_current() {
            Ok(handle) => {
                // We're already in a tokio runtime
                handle.block_on(async {
                    execute_tool_async(&state, session_id, &planned_tool, &tool_name).await
                })
            }
            Err(_) => {
                // No runtime, create a new one
                tokio::runtime::Runtime::new()
                    .context("Failed to create Tokio runtime")?
                    .block_on(async {
                        execute_tool_async(&state, session_id, &planned_tool, &tool_name).await
                    })
            }
        };

        result
    }
}

// Helper function to keep async logic separate
async fn execute_tool_async(
    state: &AdvancedChatState,
    session_id: Uuid,
    planned_tool: &PlannedTool,
    tool_name: &str,
) -> Result<serde_json::Value> {
    // Add tool call message to chat
    let execution_id = Uuid::new_v4().to_string();
    state.set_agent_state(
        session_id,
        AgentState::ExecutingTool(tool_name.to_string()),
    );
    let _ = state.add_message_to_session(
        session_id,
        ChatMessage::ToolCall {
            tool_name: tool_name.to_string(),
            description: format!("Executing: {}", tool_name),
            args: Some(planned_tool.args.clone()),
            execution_id: execution_id.clone(),
        },
    );

    // Execute the tool
    match state.call_mcp_tool(planned_tool).await {
        Ok(result) => {
            // Add result message to chat
            let _ = state.add_message_to_session(
                session_id,
                ChatMessage::ToolResult {
                    tool_name: tool_name.to_string(),
                    result: result.clone(),
                    execution_id,
                },
            );
            Ok(result)
        }
        Err(e) => {
            error!("MCP tool execution failed: {}", e);
            let _ = state.add_message_to_session(
                session_id,
                ChatMessage::Error(format!("Tool {} failed: {}", tool_name, e)),
            );
            Err(e)
        }
    }
}
