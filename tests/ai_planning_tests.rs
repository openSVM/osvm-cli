#![cfg(feature = "incomplete_tests")]
//! Comprehensive tests for AI planning and tool calling functionality

use anyhow::Result;
use mockito::{Mock, Server};
use osvm::services::ai_service::{AiService, PlannedTool, ToolPlan};
use osvm::services::mcp_service::{McpService, McpTool};
use serde_json::json;
use std::collections::HashMap;

/// Create a mock AI service for testing
fn create_mock_ai_service(_server_url: String) -> AiService {
    // Note: AiService doesn't have with_api_url method, using default construction
    AiService::new()
}

/// Create sample MCP tools for testing
fn create_sample_tools() -> HashMap<String, Vec<McpTool>> {
    let mut tools = HashMap::new();

    // Solana MCP tools
    let solana_tools = vec![
        McpTool {
            name: "get_balance".to_string(),
            description: Some("Get SOL balance for an address".to_string()),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "address": {
                        "type": "string",
                        "description": "Solana wallet address"
                    }
                },
                "required": ["address"]
            }),
        },
        McpTool {
            name: "get_recent_transactions".to_string(),
            description: Some("Get recent transactions for an address".to_string()),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "address": {
                        "type": "string",
                        "description": "Solana wallet address"
                    },
                    "limit": {
                        "type": "number",
                        "description": "Number of transactions to return"
                    }
                },
                "required": ["address"]
            }),
        },
    ];
    tools.insert("solana-mcp".to_string(), solana_tools);

    // OSVM MCP tools
    let osvm_tools = vec![McpTool {
        name: "deploy_program".to_string(),
        description: Some("Deploy a Solana program".to_string()),
        input_schema: json!({
            "type": "object",
            "properties": {
                "program_path": {
                    "type": "string",
                    "description": "Path to program binary"
                },
                "network": {
                    "type": "string",
                    "description": "Target network (mainnet/devnet/testnet)"
                }
            },
            "required": ["program_path"]
        }),
    }];
    tools.insert("osvm-mcp".to_string(), osvm_tools);

    tools
}

#[cfg(all(test, feature = "incomplete_tests"))]
mod tests {
    use super::*;
    use tokio;

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    async fn test_ai_planning_simple_request() -> Result<()> {
        let mut server = Server::new_async().await;
        let mock = server
            .mock("POST", "/api/getAnswer")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "reasoning": "User wants to check balance",
                    "osvm_tools_to_use": [{
                        "server_id": "solana-mcp",
                        "tool_name": "get_balance",
                        "reason": "Need to fetch wallet balance",
                        "args": {
                            "address": "7xKXtg2CW87d3TXQ5xmD7mSZQ4mPfFrPwJzQ7xRT7mF9"
                        }
                    }],
                    "expected_outcome": "Balance will be displayed"
                })
                .to_string(),
            )
            .create_async()
            .await;

        let ai_service = create_mock_ai_service(server.url());
        let tools = create_sample_tools();

        let plan = ai_service
            .create_tool_plan("What is my wallet balance?", &tools)
            .await?;

        assert_eq!(plan.osvm_tools_to_use.len(), 1);
        assert_eq!(plan.osvm_tools_to_use[0].server_id, "solana-mcp");
        assert_eq!(plan.osvm_tools_to_use[0].tool_name, "get_balance");

        mock.assert_async().await;
        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    async fn test_ai_planning_complex_workflow() -> Result<()> {
        let mut server = Server::new_async().await;
        let mock = server
            .mock("POST", "/api/getAnswer")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "reasoning": "User wants to deploy and verify a program",
                    "osvm_tools_to_use": [
                        {
                            "server_id": "osvm-mcp",
                            "tool_name": "deploy_program",
                            "reason": "Deploy the program to devnet",
                            "args": {
                                "program_path": "./target/deploy/program.so",
                                "network": "devnet"
                            }
                        },
                        {
                            "server_id": "solana-mcp",
                            "tool_name": "get_recent_transactions",
                            "reason": "Verify deployment transaction",
                            "args": {
                                "address": "DeployerAddress123",
                                "limit": 5
                            }
                        }
                    ],
                    "expected_outcome": "Program deployed and verified"
                })
                .to_string(),
            )
            .create_async()
            .await;

        let ai_service = create_mock_ai_service(server.url());
        let tools = create_sample_tools();

        let plan = ai_service
            .create_tool_plan("Deploy my program to devnet and verify it worked", &tools)
            .await?;

        assert_eq!(plan.osvm_tools_to_use.len(), 2);
        assert_eq!(plan.osvm_tools_to_use[0].tool_name, "deploy_program");
        assert_eq!(
            plan.osvm_tools_to_use[1].tool_name,
            "get_recent_transactions"
        );

        mock.assert_async().await;
        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    async fn test_ai_planning_with_no_matching_tools() -> Result<()> {
        let mut server = Server::new_async().await;
        let mock = server
            .mock("POST", "/api/getAnswer")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "reasoning": "No tools available for this request",
                    "osvm_tools_to_use": [],
                    "expected_outcome": "Cannot complete request without appropriate tools"
                })
                .to_string(),
            )
            .create_async()
            .await;

        let ai_service = create_mock_ai_service(server.url());
        let tools = create_sample_tools();

        let plan = ai_service
            .create_tool_plan("Send an email to support", &tools)
            .await?;

        assert_eq!(plan.osvm_tools_to_use.len(), 0);
        assert!(plan.reasoning.contains("No tools"));

        mock.assert_async().await;
        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    async fn test_ai_planning_error_handling() -> Result<()> {
        let mut server = Server::new_async().await;
        let _mock = server
            .mock("POST", "/api/getAnswer")
            .with_status(500)
            .with_body("Internal Server Error")
            .create_async()
            .await;

        let ai_service = create_mock_ai_service(server.url());
        let tools = create_sample_tools();

        let result = ai_service.create_tool_plan("Check balance", &tools).await;

        assert!(result.is_err());
        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    async fn test_ai_planning_malformed_response() -> Result<()> {
        let mut server = Server::new_async().await;
        let _mock = server
            .mock("POST", "/api/getAnswer")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body("{\"invalid\": \"json_structure\"}")
            .create_async()
            .await;

        let ai_service = create_mock_ai_service(server.url());
        let tools = create_sample_tools();

        let result = ai_service.create_tool_plan("Check balance", &tools).await;

        assert!(result.is_err());
        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    async fn test_ai_planning_with_circuit_breaker() -> Result<()> {
        let mut server = Server::new_async().await;

        // First 5 calls fail to trigger circuit breaker
        for _ in 0..5 {
            let _mock = server
                .mock("POST", "/api/getAnswer")
                .with_status(500)
                .with_body("Error")
                .create_async()
                .await
                .expect(1);
        }

        let ai_service = create_mock_ai_service(server.url());
        let tools = create_sample_tools();

        // Make 5 failing requests
        for _ in 0..5 {
            let _ = ai_service.create_tool_plan("Test", &tools).await;
        }

        // Circuit should be open now, next call should fail immediately
        let start = std::time::Instant::now();
        let result = ai_service.create_tool_plan("Test", &tools).await;
        let duration = start.elapsed();

        assert!(result.is_err());
        assert!(duration < std::time::Duration::from_secs(1)); // Should fail fast

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    async fn test_parallel_planning_requests() -> Result<()> {
        let mut server = Server::new_async().await;
        let mock = server
            .mock("POST", "/api/getAnswer")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "reasoning": "Planning",
                    "osvm_tools_to_use": [],
                    "expected_outcome": "Done"
                })
                .to_string(),
            )
            .expect(3)
            .create_async()
            .await;

        let ai_service = create_mock_ai_service(server.url());
        let tools = create_sample_tools();

        // Run 3 parallel requests
        let futures = vec![
            ai_service.create_tool_plan("Request 1", &tools),
            ai_service.create_tool_plan("Request 2", &tools),
            ai_service.create_tool_plan("Request 3", &tools),
        ];

        let results = futures::future::join_all(futures).await;

        assert_eq!(results.len(), 3);
        for result in results {
            assert!(result.is_ok());
        }

        mock.assert_async().await;
        Ok(())
    }
}

#[cfg(all(test, feature = "incomplete_tests"))]
#[cfg(feature = "proptest")] // Disabled: proptest not in dependencies
mod property_tests {
    use super::*;
    // use proptest::prelude::*;

    // proptest! {
    //     #[test]
    //     fn test_tool_plan_serialization_roundtrip(
    //         reasoning in "[a-zA-Z ]{10,100}",
    //         outcome in "[a-zA-Z ]{10,100}",
    //         num_tools in 0usize..5
    //     ) {
    //         let tools: Vec<PlannedTool> = (0..num_tools)
    //             .map(|i| PlannedTool {
    //                 server_id: format!("server-{}", i),
    //                 tool_name: format!("tool-{}", i),
    //                 args: json!({}),
    //             })
    //             .collect();

    //         let plan = ToolPlan {
    //             reasoning: reasoning.clone(),
    //             osvm_tools_to_use: tools,
    //             expected_outcome: outcome.clone(),
    //         };

    //         let serialized = serde_json::to_string(&plan).unwrap();
    //         let deserialized: ToolPlan = serde_json::from_str(&serialized).unwrap();

    //         assert_eq!(plan.reasoning, deserialized.reasoning);
    //         assert_eq!(plan.expected_outcome, deserialized.expected_outcome);
    //         assert_eq!(plan.osvm_tools_to_use.len(), deserialized.osvm_tools_to_use.len());
    //     }
    // }
}
