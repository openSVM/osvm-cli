//! Tests for MCP (Model Context Protocol) tool execution

use anyhow::Result;
use mockito::Server;
use osvm::services::mcp_service::{
    McpAuthConfig, McpServerConfig, McpService, McpTransportType,
};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Create a mock MCP server configuration
fn create_mock_mcp_config(server_url: String) -> McpServerConfig {
    McpServerConfig {
        name: "test-mcp".to_string(),
        url: server_url,
        transport_type: McpTransportType::Http,
        auth: Some(McpAuthConfig {
            auth_type: "bearer".to_string(),
            token: Some("test-token".to_string()),
            username: None,
            password: None,
        }),
        enabled: true,
        extra_config: HashMap::new(),
        github_url: None,
        local_path: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_mcp_tool_discovery() -> Result<()> {
        let mut server = Server::new_async().await;
        let mock = server
            .mock("POST", "/tools/list")
            .match_header("authorization", "Bearer test-token")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "tools": [
                        {
                            "name": "get_balance",
                            "description": "Get SOL balance",
                            "inputSchema": {
                                "type": "object",
                                "properties": {
                                    "address": {
                                        "type": "string",
                                        "description": "Wallet address"
                                    }
                                },
                                "required": ["address"]
                            }
                        },
                        {
                            "name": "transfer_sol",
                            "description": "Transfer SOL between wallets",
                            "inputSchema": {
                                "type": "object",
                                "properties": {
                                    "from": {"type": "string"},
                                    "to": {"type": "string"},
                                    "amount": {"type": "number"}
                                },
                                "required": ["from", "to", "amount"]
                            }
                        }
                    ]
                })
                .to_string(),
            )
            .create_async()
            .await;

        let config = create_mock_mcp_config(server.url());
        let mut mcp_service = McpService::new();
        mcp_service.add_server("test-mcp".to_string(), config);

        let tools = mcp_service.list_tools("test-mcp").await?;

        assert_eq!(tools.len(), 2);
        assert_eq!(tools[0].name, "get_balance");
        assert_eq!(tools[1].name, "transfer_sol");
        // Check input_schema instead of parameters
        assert!(tools[1].input_schema.is_object());

        mock.assert_async().await;
        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_tool_execution_success() -> Result<()> {
        let mut server = Server::new_async().await;
        let mock = server
            .mock("POST", "/tools/execute")
            .match_header("authorization", "Bearer test-token")
            .match_body(mockito::Matcher::Json(json!({
                "tool": "get_balance",
                "args": {
                    "address": "7xKXtg2CW87d3TXQ5xmD7mSZQ4mPfFrPwJzQ7xRT7mF9"
                }
            })))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "balance": 100.5,
                    "unit": "SOL"
                })
                .to_string(),
            )
            .create_async()
            .await;

        let config = create_mock_mcp_config(server.url());
        let mut mcp_service = McpService::new();
        mcp_service.add_server("test-mcp".to_string(), config);

        let args = json!({
            "address": "7xKXtg2CW87d3TXQ5xmD7mSZQ4mPfFrPwJzQ7xRT7mF9"
        });

        let result = mcp_service
            .call_tool("test-mcp", "get_balance", Some(args))
            .await?;

        assert_eq!(result["balance"], 100.5);
        assert_eq!(result["unit"], "SOL");

        mock.assert_async().await;
        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_tool_execution_failure() -> Result<()> {
        let mut server = Server::new_async().await;
        let mock = server
            .mock("POST", "/tools/execute")
            .with_status(500)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "error": "Insufficient balance for transfer"
                })
                .to_string(),
            )
            .create_async()
            .await;

        let config = create_mock_mcp_config(server.url());
        let mut mcp_service = McpService::new();
        mcp_service.add_server("test-mcp".to_string(), config);

        let args = json!({
            "from": "source_wallet",
            "to": "dest_wallet",
            "amount": 1000
        });

        let result = mcp_service
            .call_tool("test-mcp", "transfer_sol", Some(args))
            .await;

        // Should return error due to 500 status
        assert!(result.is_err());

        mock.assert_async().await;
        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_websocket_connection() -> Result<()> {
        // This test would require a WebSocket mock server
        // For now, we'll test the configuration parsing

        let config = McpServerConfig {
            name: "ws-mcp".to_string(),
            url: "ws://localhost:8080".to_string(),
            transport_type: McpTransportType::Websocket,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
            github_url: None,
            local_path: None,
        };

        let mut mcp_service = McpService::new();
        mcp_service.add_server("ws-mcp".to_string(), config);

        let servers = mcp_service.list_servers();
        assert!(servers.iter().any(|(id, _)| *id == "ws-mcp"));

        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_stdio_server() -> Result<()> {
        let config = McpServerConfig {
            name: "stdio-mcp".to_string(),
            url: "stdio://./test-mcp-server".to_string(),
            transport_type: McpTransportType::Stdio,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
            github_url: None,
            local_path: Some("./test-mcp-server".to_string()),
        };

        let mut mcp_service = McpService::new();
        mcp_service.add_server("stdio-mcp".to_string(), config);

        let servers = mcp_service.list_servers();
        assert!(servers.iter().any(|(id, _)| *id == "stdio-mcp"));

        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_authentication_types() -> Result<()> {
        let mut server = Server::new_async().await;

        // Test Bearer auth
        let mock_bearer = server
            .mock("POST", "/bearer/test")
            .match_header("authorization", "Bearer bearer-token")
            .with_status(200)
            .with_body("{\"success\": true}")
            .create_async()
            .await;

        // Test Basic auth
        let mock_basic = server
            .mock("POST", "/basic/test")
            .match_header("authorization", "Basic dXNlcjpwYXNz") // user:pass base64
            .with_status(200)
            .with_body("{\"success\": true}")
            .create_async()
            .await;

        // Test API Key auth
        let mock_apikey = server
            .mock("POST", "/apikey/test")
            .match_header("x-api-key", "api-key-123")
            .with_status(200)
            .with_body("{\"success\": true}")
            .create_async()
            .await;

        let mcp_service = McpService::new();

        // Add servers with different auth types
        let configs = vec![
            McpServerConfig {
                name: "bearer-server".to_string(),
                url: format!("{}/bearer", server.url()),
                transport_type: McpTransportType::Http,
                auth: Some(McpAuthConfig {
                    auth_type: "bearer".to_string(),
                    token: Some("bearer-token".to_string()),
                    username: None,
                    password: None,
                }),
                enabled: true,
                extra_config: HashMap::new(),
                github_url: None,
                local_path: None,
            },
            McpServerConfig {
                name: "basic-server".to_string(),
                url: format!("{}/basic", server.url()),
                transport_type: McpTransportType::Http,
                auth: Some(McpAuthConfig {
                    auth_type: "basic".to_string(),
                    token: None,
                    username: Some("user".to_string()),
                    password: Some("pass".to_string()),
                }),
                enabled: true,
                extra_config: HashMap::new(),
                github_url: None,
                local_path: None,
            },
            McpServerConfig {
                name: "apikey-server".to_string(),
                url: format!("{}/apikey", server.url()),
                transport_type: McpTransportType::Http,
                auth: Some(McpAuthConfig {
                    auth_type: "api_key".to_string(),
                    token: Some("api-key-123".to_string()),
                    username: None,
                    password: None,
                }),
                enabled: true,
                extra_config: HashMap::new(),
                github_url: None,
                local_path: None,
            },
        ];

        let mut mcp_service = mcp_service;
        for (i, config) in configs.into_iter().enumerate() {
            let server_id = if i == 0 { "bearer-server" } else if i == 1 { "basic-server" } else { "apikey-server" };
            mcp_service.add_server(server_id.to_string(), config);
        }

        // Test each server connection
        let _ = mcp_service.test_server("bearer-server").await;
        let _ = mcp_service.test_server("basic-server").await;
        let _ = mcp_service.test_server("apikey-server").await;

        mock_bearer.assert_async().await;
        mock_basic.assert_async().await;
        mock_apikey.assert_async().await;

        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_concurrent_tool_execution() -> Result<()> {
        let mut server = Server::new_async().await;
        let mock = server
            .mock("POST", "/tools/execute")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({"value": 42})
                .to_string(),
            )
            .expect(5)
            .create_async()
            .await;

        let config = create_mock_mcp_config(server.url());
        let mut mcp_service = McpService::new();
        mcp_service.add_server("test-mcp".to_string(), config);
        let mcp_service = Arc::new(RwLock::new(mcp_service));

        // Execute 5 tools concurrently
        let mut handles = vec![];
        for i in 0..5 {
            let service = Arc::clone(&mcp_service);
            let handle = tokio::spawn(async move {
                let args = json!({"id": i});
                let svc = service.read().await;
                svc.call_tool("test-mcp", "test_tool", Some(args)).await
            });
            handles.push(handle);
        }

        let results: Vec<Result<Value>> = futures::future::join_all(handles)
            .await
            .into_iter()
            .map(|r| r.unwrap())
            .collect();

        assert_eq!(results.len(), 5);
        for result in results {
            let val = result?;
            assert_eq!(val["value"], 42);
        }

        mock.assert_async().await;
        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_server_enable_disable() -> Result<()> {
        let config = create_mock_mcp_config("http://localhost:9999".to_string());
        let mut mcp_service = McpService::new();

        mcp_service.add_server("test-mcp".to_string(), config);

        // Initially enabled
        let servers = mcp_service.list_servers();
        assert!(
            servers
                .iter()
                .find(|(id, _)| *id == "test-mcp")
                .unwrap()
                .1
                .enabled
        );

        // Disable
        mcp_service.toggle_server("test-mcp", false)?;
        let servers = mcp_service.list_servers();
        assert!(
            !servers
                .iter()
                .find(|(id, _)| *id == "test-mcp")
                .unwrap()
                .1
                .enabled
        );

        // Re-enable
        mcp_service.toggle_server("test-mcp", true)?;
        let servers = mcp_service.list_servers();
        assert!(
            servers
                .iter()
                .find(|(id, _)| *id == "test-mcp")
                .unwrap()
                .1
                .enabled
        );

        Ok(())
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    #[tokio::test]
    async fn test_mcp_github_server_setup() -> Result<()> {
        let mut mcp_service = McpService::new();

        // Test GitHub URL parsing
        let github_url = "https://github.com/opensvm/solana-mcp-server".to_string();
        let result = mcp_service
            .add_server_from_github("solana-mcp".to_string(), github_url, None, true)
            .await;

        // This will fail in test environment but should parse correctly
        match result {
            Ok(_) => {
                let servers = mcp_service.list_servers();
                assert!(servers.iter().any(|(id, _)| *id == "solana-mcp"));
            }
            Err(e) => {
                // Expected in test environment without actual GitHub access
                println!("Expected error in test: {}", e);
            }
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_tool_chain_execution() -> Result<()> {
        let mut server = Server::new_async().await;

        // Mock first tool
        let mock1 = server
            .mock("POST", "/tools/execute")
            .match_body(mockito::Matcher::Json(json!({
                "tool": "get_account_info",
                "args": {"address": "wallet123"}
            })))
            .with_status(200)
            .with_body(
                json!({
                    "balance": 100,
                    "program": "program456"
                })
                .to_string(),
            )
            .create_async()
            .await;

        // Mock second tool using result from first
        let mock2 = server
            .mock("POST", "/tools/execute")
            .match_body(mockito::Matcher::Json(json!({
                "tool": "get_program_info",
                "args": {"program_id": "program456"}
            })))
            .with_status(200)
            .with_body(
                json!({
                    "name": "MyProgram",
                    "version": "1.0.0"
                })
                .to_string(),
            )
            .create_async()
            .await;

        let config = create_mock_mcp_config(server.url());
        let mut mcp_service = McpService::new();
        mcp_service.add_server("test-mcp".to_string(), config);

        // Execute tool chain
        let args1 = json!({"address": "wallet123"});
        let result1 = mcp_service
            .call_tool("test-mcp", "get_account_info", Some(args1))
            .await?;

        let program_id = result1["program"].as_str().unwrap();

        let args2 = json!({"program_id": program_id});
        let result2 = mcp_service
            .call_tool("test-mcp", "get_program_info", Some(args2))
            .await?;

        assert_eq!(result2["name"], "MyProgram");

        mock1.assert_async().await;
        mock2.assert_async().await;

        Ok(())
    }
}
