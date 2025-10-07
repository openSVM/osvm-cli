//! End-to-end integration tests for complete workflows

use anyhow::Result;
use osvm::{
    services::{
        ai_service::AiService,
        mcp_service::{McpService, McpServerConfig, McpTransportType, McpAuthConfig},
    },
    utils::{
        isolation::{IsolationManager, IsolationConfig, RuntimeConfig, RuntimeType, ResourceLimits},
        agent_chat_v2::{ChatState, ChatMessage, AgentState},
    },
};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;
use tempfile::TempDir;
use mockito::Server;
use serde_json::json;
use tokio::time::sleep;

/// Complete workflow: User request -> AI Planning -> MCP Execution -> Result
#[cfg(test)]
mod complete_workflow_tests {
    use super::*;

    #[tokio::test]
    async fn test_balance_check_workflow() -> Result<()> {
        // Setup mock servers
        let mut ai_server = Server::new_async().await;
        let mut mcp_server = Server::new_async().await;

        // Mock AI planning response
        let ai_mock = ai_server.mock("POST", "/api/getAnswer")
            .with_status(200)
            .with_body(json!({
                "reasoning": "User wants to check balance, using get_balance tool",
                "osvm_tools_to_use": [{
                    "server_id": "solana-mcp",
                    "tool_name": "get_balance",
                    "reason": "Fetch wallet balance",
                    "args": {
                        "address": "UserWallet123"
                    }
                }],
                "expected_outcome": "Balance displayed to user"
            }).to_string())
            .create_async()
            .await;

        // Mock MCP tool discovery
        let mcp_tools_mock = mcp_server.mock("POST", "/tools/list")
            .with_status(200)
            .with_body(json!({
                "tools": [{
                    "name": "get_balance",
                    "description": "Get SOL balance",
                    "parameters": [{
                        "name": "address",
                        "type": "string",
                        "required": true
                    }]
                }]
            }).to_string())
            .create_async()
            .await;

        // Mock MCP tool execution
        let mcp_exec_mock = mcp_server.mock("POST", "/tools/execute")
            .with_status(200)
            .with_body(json!({
                "success": true,
                "result": {
                    "balance": 42.5,
                    "unit": "SOL"
                }
            }).to_string())
            .create_async()
            .await;

        // Initialize services
        let ai_service = AiService::new().with_api_url(&ai_server.url());
        let mcp_service = McpService::new()?;

        // Configure MCP server
        let mcp_config = McpServerConfig {
            name: "solana-mcp".to_string(),
            url: mcp_server.url(),
            transport_type: McpTransportType::Http,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
            github_url: None,
            local_path: None,
        };
        mcp_service.add_server(mcp_config).await?;

        // Get available tools
        let mut tools = HashMap::new();
        let mcp_tools = mcp_service.list_tools("solana-mcp").await?;
        tools.insert("solana-mcp".to_string(), mcp_tools);

        // Step 1: AI Planning
        let plan = ai_service.create_tool_plan(
            "What is my wallet balance?",
            &tools
        ).await?;

        assert_eq!(plan.tools_to_use.len(), 1);
        assert_eq!(plan.tools_to_use[0].tool_name, "get_balance");

        // Step 2: Execute planned tools
        for tool_call in plan.tools_to_use {
            let result = mcp_service.execute_tool(
                &tool_call.server_id,
                &tool_call.tool_name,
                tool_call.args
            ).await?;

            assert!(result.success);
            assert_eq!(result.result["balance"], 42.5);
        }

        // Verify all mocks were called
        ai_mock.assert_async().await;
        mcp_tools_mock.assert_async().await;
        mcp_exec_mock.assert_async().await;

        Ok(())
    }

    #[tokio::test]
    async fn test_program_deployment_workflow() -> Result<()> {
        let mut ai_server = Server::new_async().await;
        let mut mcp_server = Server::new_async().await;

        // Mock AI planning for deployment
        let ai_mock = ai_server.mock("POST", "/api/getAnswer")
            .with_status(200)
            .with_body(json!({
                "reasoning": "Deploy program then verify",
                "osvm_tools_to_use": [
                    {
                        "server_id": "osvm-mcp",
                        "tool_name": "deploy_program",
                        "reason": "Deploy to devnet",
                        "args": {
                            "program_path": "./program.so",
                            "network": "devnet"
                        }
                    },
                    {
                        "server_id": "osvm-mcp",
                        "tool_name": "verify_deployment",
                        "reason": "Verify deployment success",
                        "args": {
                            "program_id": "PENDING"
                        }
                    }
                ],
                "expected_outcome": "Program deployed and verified"
            }).to_string())
            .create_async()
            .await;

        // Mock deployment execution
        let deploy_mock = mcp_server.mock("POST", "/tools/execute")
            .match_body(mockito::Matcher::PartialJson(json!({
                "tool": "deploy_program"
            })))
            .with_status(200)
            .with_body(json!({
                "success": true,
                "result": {
                    "program_id": "ProgramID123",
                    "transaction": "TxHash456"
                }
            }).to_string())
            .create_async()
            .await;

        // Mock verification
        let verify_mock = mcp_server.mock("POST", "/tools/execute")
            .match_body(mockito::Matcher::PartialJson(json!({
                "tool": "verify_deployment"
            })))
            .with_status(200)
            .with_body(json!({
                "success": true,
                "result": {
                    "verified": true,
                    "executable": true
                }
            }).to_string())
            .create_async()
            .await;

        let ai_service = AiService::new().with_api_url(&ai_server.url());
        let mcp_service = McpService::new()?;

        let mcp_config = McpServerConfig {
            name: "osvm-mcp".to_string(),
            url: mcp_server.url(),
            transport_type: McpTransportType::Http,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
            github_url: None,
            local_path: None,
        };
        mcp_service.add_server(mcp_config).await?;

        // Execute workflow
        let mut tools = HashMap::new();
        tools.insert("osvm-mcp".to_string(), vec![]);

        let plan = ai_service.create_tool_plan(
            "Deploy my program to devnet",
            &tools
        ).await?;

        let mut program_id = None;
        for mut tool_call in plan.tools_to_use {
            // Update verify args with actual program ID
            if tool_call.tool_name == "verify_deployment" && program_id.is_some() {
                tool_call.args.insert(
                    "program_id".to_string(),
                    json!(program_id.as_ref().unwrap())
                );
            }

            let result = mcp_service.execute_tool(
                &tool_call.server_id,
                &tool_call.tool_name,
                tool_call.args
            ).await?;

            assert!(result.success);

            // Capture program ID from deployment
            if tool_call.tool_name == "deploy_program" {
                program_id = result.result["program_id"].as_str().map(String::from);
            }
        }

        ai_mock.assert_async().await;
        deploy_mock.assert_async().await;
        verify_mock.assert_async().await;

        Ok(())
    }
}

#[cfg(test)]
mod isolated_execution_tests {
    use super::*;

    #[tokio::test]
    async fn test_mcp_in_microvm_workflow() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let mut mcp_server = Server::new_async().await;

        // Setup isolation manager
        let isolation_config = IsolationConfig {
            runtime_type: RuntimeType::Process,
            resource_limits: ResourceLimits {
                cpu_cores: 2,
                memory_mb: 512,
                disk_gb: 1,
                network_bandwidth_mbps: Some(100),
                max_processes: Some(50),
                max_open_files: Some(1000),
            },
            network_isolation: true,
            filesystem_isolation: true,
            process_isolation: true,
            rootfs_path: Some(temp_dir.path().to_path_buf()),
            kernel_path: None,
            additional_drives: vec![],
            network_interfaces: vec![],
            enable_monitoring: true,
            monitoring_interval_ms: 1000,
        };

        let isolation_manager = IsolationManager::new(isolation_config).await?;

        // Create isolated runtime for MCP server
        let runtime_config = RuntimeConfig {
            name: "mcp-isolated".to_string(),
            runtime_type: RuntimeType::Process,
            resource_limits: ResourceLimits {
                cpu_cores: 1,
                memory_mb: 256,
                disk_gb: 1,
                network_bandwidth_mbps: Some(50),
                max_processes: Some(10),
                max_open_files: Some(100),
            },
            rootfs: Some(temp_dir.path().to_path_buf()),
            command: vec!["echo".to_string(), "MCP Server".to_string()],
            env_vars: vec![
                ("MCP_PORT".to_string(), "9090".to_string()),
            ],
            mounts: vec![],
            network_enabled: true,
        };

        let runtime_id = isolation_manager.create_runtime(runtime_config).await?;
        isolation_manager.start_runtime(runtime_id).await?;

        // Mock MCP execution
        let exec_mock = mcp_server.mock("POST", "/isolated/execute")
            .with_status(200)
            .with_body(json!({
                "success": true,
                "result": {
                    "executed_in": "microvm",
                    "isolation_level": "full"
                }
            }).to_string())
            .create_async()
            .await;

        // Configure MCP to connect to isolated runtime
        let mcp_service = McpService::new()?;
        let mcp_config = McpServerConfig {
            name: "isolated-mcp".to_string(),
            url: mcp_server.url() + "/isolated",
            transport_type: McpTransportType::Http,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
            github_url: None,
            local_path: None,
        };
        mcp_service.add_server(mcp_config).await?;

        // Execute tool in isolated environment
        let result = mcp_service.execute_tool(
            "isolated-mcp",
            "execute",
            HashMap::new()
        ).await?;

        assert!(result.success);
        assert_eq!(result.result["executed_in"], "microvm");

        // Cleanup
        isolation_manager.stop_runtime(runtime_id).await?;
        exec_mock.assert_async().await;

        Ok(())
    }

    #[tokio::test]
    async fn test_resource_limited_execution() -> Result<()> {
        let temp_dir = TempDir::new()?;

        // Create resource-constrained environment
        let isolation_config = IsolationConfig {
            runtime_type: RuntimeType::Process,
            resource_limits: ResourceLimits {
                cpu_cores: 1,
                memory_mb: 128, // Very limited memory
                disk_gb: 1,
                network_bandwidth_mbps: Some(10), // Limited bandwidth
                max_processes: Some(5),
                max_open_files: Some(50),
            },
            network_isolation: true,
            filesystem_isolation: true,
            process_isolation: true,
            rootfs_path: Some(temp_dir.path().to_path_buf()),
            kernel_path: None,
            additional_drives: vec![],
            network_interfaces: vec![],
            enable_monitoring: true,
            monitoring_interval_ms: 100,
        };

        let isolation_manager = IsolationManager::new(isolation_config).await?;

        let runtime_config = RuntimeConfig {
            name: "limited-runtime".to_string(),
            runtime_type: RuntimeType::Process,
            resource_limits: ResourceLimits {
                cpu_cores: 1,
                memory_mb: 64,
                disk_gb: 1,
                network_bandwidth_mbps: Some(5),
                max_processes: Some(3),
                max_open_files: Some(25),
            },
            rootfs: Some(temp_dir.path().to_path_buf()),
            command: vec!["sleep".to_string(), "0.5".to_string()],
            env_vars: vec![],
            mounts: vec![],
            network_enabled: false,
        };

        let runtime_id = isolation_manager.create_runtime(runtime_config).await?;
        isolation_manager.start_runtime(runtime_id).await?;

        // Monitor resource usage
        sleep(Duration::from_millis(200)).await;
        let metrics = isolation_manager.get_runtime_metrics(runtime_id).await?;

        // Verify resource limits are respected
        assert!(metrics.memory_usage_mb <= 64);
        assert!(metrics.cpu_usage_percent <= 100.0);

        isolation_manager.stop_runtime(runtime_id).await?;

        Ok(())
    }
}

#[cfg(test)]
mod chat_integration_tests {
    use super::*;

    #[tokio::test]
    async fn test_chat_with_tool_execution() -> Result<()> {
        let mut ai_server = Server::new_async().await;
        let mut mcp_server = Server::new_async().await;

        // Mock AI response for chat
        let ai_mock = ai_server.mock("POST", "/chat")
            .with_status(200)
            .with_body(json!({
                "response": "I'll check your balance for you.",
                "tool_plan": {
                    "reasoning": "Need to check balance",
                    "osvm_tools_to_use": [{
                        "server_id": "solana-mcp",
                        "tool_name": "get_balance",
                        "reason": "Get balance",
                        "args": {"address": "wallet123"}
                    }],
                    "expected_outcome": "Balance retrieved"
                }
            }).to_string())
            .create_async()
            .await;

        // Mock tool execution
        let tool_mock = mcp_server.mock("POST", "/tools/execute")
            .with_status(200)
            .with_body(json!({
                "success": true,
                "result": {"balance": 100.0}
            }).to_string())
            .create_async()
            .await;

        // Initialize chat state
        let mut chat_state = ChatState::new()?;
        let session_id = chat_state.create_session("Test Session".to_string())?;

        // Add user message
        chat_state.add_message(
            session_id,
            ChatMessage {
                role: "user".to_string(),
                content: "What's my balance?".to_string(),
                timestamp: chrono::Utc::now(),
                tool_calls: None,
            }
        )?;

        // Update agent state
        chat_state.set_agent_state(session_id, AgentState::Planning)?;

        // Simulate AI planning
        let ai_service = AiService::new().with_api_url(&ai_server.url());
        let mcp_service = McpService::new()?;

        let mcp_config = McpServerConfig {
            name: "solana-mcp".to_string(),
            url: mcp_server.url(),
            transport_type: McpTransportType::Http,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
            github_url: None,
            local_path: None,
        };
        mcp_service.add_server(mcp_config).await?;

        // Execute tool
        chat_state.set_agent_state(session_id, AgentState::ExecutingTool("get_balance".to_string()))?;

        let result = mcp_service.execute_tool(
            "solana-mcp",
            "get_balance",
            [("address".to_string(), json!("wallet123"))].into()
        ).await?;

        assert!(result.success);

        // Add response to chat
        chat_state.add_message(
            session_id,
            ChatMessage {
                role: "assistant".to_string(),
                content: format!("Your balance is {} SOL", result.result["balance"]),
                timestamp: chrono::Utc::now(),
                tool_calls: Some(vec!["get_balance".to_string()]),
            }
        )?;

        chat_state.set_agent_state(session_id, AgentState::Idle)?;

        // Verify chat history
        let messages = chat_state.get_messages(session_id)?;
        assert_eq!(messages.len(), 2);
        assert!(messages[1].content.contains("100"));

        ai_mock.assert_async().await;
        tool_mock.assert_async().await;

        Ok(())
    }

    #[tokio::test]
    async fn test_multi_turn_conversation() -> Result<()> {
        let mut chat_state = ChatState::new()?;
        let session_id = chat_state.create_session("Multi-turn Test".to_string())?;

        // Simulate multi-turn conversation
        let turns = vec![
            ("user", "Hello, can you help me?"),
            ("assistant", "Of course! I'm here to help. What do you need?"),
            ("user", "I want to check my balance"),
            ("assistant", "I'll check your balance for you."),
            ("user", "Also, show me recent transactions"),
            ("assistant", "Here are your recent transactions..."),
        ];

        for (role, content) in turns {
            chat_state.add_message(
                session_id,
                ChatMessage {
                    role: role.to_string(),
                    content: content.to_string(),
                    timestamp: chrono::Utc::now(),
                    tool_calls: None,
                }
            )?;
        }

        let messages = chat_state.get_messages(session_id)?;
        assert_eq!(messages.len(), 6);

        // Test session management
        let sessions = chat_state.list_sessions()?;
        assert_eq!(sessions.len(), 1);

        Ok(())
    }
}

#[cfg(test)]
mod stress_tests {
    use super::*;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_concurrent_workflow_execution() -> Result<()> {
        let mut server = Server::new_async().await;

        // Mock that handles multiple concurrent requests
        let mock = server.mock("POST", "/concurrent")
            .with_status(200)
            .with_body(json!({"success": true}).to_string())
            .expect(10)
            .create_async()
            .await;

        let mcp_service = Arc::new(McpService::new()?);
        let mcp_config = McpServerConfig {
            name: "concurrent-test".to_string(),
            url: server.url() + "/concurrent",
            transport_type: McpTransportType::Http,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
            github_url: None,
            local_path: None,
        };
        mcp_service.add_server(mcp_config).await?;

        // Launch 10 concurrent workflows
        let mut handles = vec![];
        for i in 0..10 {
            let service = Arc::clone(&mcp_service);
            let handle = tokio::spawn(async move {
                service.execute_tool(
                    "concurrent-test",
                    "test",
                    [("id".to_string(), json!(i))].into()
                ).await
            });
            handles.push(handle);
        }

        let results: Vec<_> = futures::future::join_all(handles).await;
        assert_eq!(results.len(), 10);

        for result in results {
            assert!(result?.is_ok());
        }

        mock.assert_async().await;
        Ok(())
    }
}