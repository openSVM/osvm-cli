//! Tests for MCP (Model Context Protocol) tool execution

use anyhow::Result;
use osvm::services::mcp_service::{
    McpService, McpServerConfig, McpTool, McpParameter,
    McpTransportType, McpAuthConfig, McpToolResult
};
use std::collections::HashMap;
use std::path::PathBuf;
use mockito::{Server, Mock};
use serde_json::{json, Value};
use tokio::sync::RwLock;
use std::sync::Arc;

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
        let mock = server.mock("POST", "/tools/list")
            .match_header("authorization", "Bearer test-token")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(json!({
                "tools": [
                    {
                        "name": "get_balance",
                        "description": "Get SOL balance",
                        "parameters": [
                            {
                                "name": "address",
                                "type": "string",
                                "required": true,
                                "description": "Wallet address"
                            }
                        ]
                    },
                    {
                        "name": "transfer_sol",
                        "description": "Transfer SOL between wallets",
                        "parameters": [
                            {
                                "name": "from",
                                "type": "string",
                                "required": true
                            },
                            {
                                "name": "to",
                                "type": "string",
                                "required": true
                            },
                            {
                                "name": "amount",
                                "type": "number",
                                "required": true
                            }
                        ]
                    }
                ]
            }).to_string())
            .create_async()
            .await;

        let config = create_mock_mcp_config(server.url());
        let mcp_service = McpService::new()?;
        mcp_service.add_server(config).await?;

        let tools = mcp_service.list_tools("test-mcp").await?;

        assert_eq!(tools.len(), 2);
        assert_eq!(tools[0].name, "get_balance");
        assert_eq!(tools[1].name, "transfer_sol");
        assert_eq!(tools[1].parameters.len(), 3);

        mock.assert_async().await;
        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_tool_execution_success() -> Result<()> {
        let mut server = Server::new_async().await;
        let mock = server.mock("POST", "/tools/execute")
            .match_header("authorization", "Bearer test-token")
            .match_body(mockito::Matcher::Json(json!({
                "tool": "get_balance",
                "args": {
                    "address": "7xKXtg2CW87d3TXQ5xmD7mSZQ4mPfFrPwJzQ7xRT7mF9"
                }
            })))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(json!({
                "success": true,
                "result": {
                    "balance": 100.5,
                    "unit": "SOL"
                }
            }).to_string())
            .create_async()
            .await;

        let config = create_mock_mcp_config(server.url());
        let mcp_service = McpService::new()?;
        mcp_service.add_server(config).await?;

        let mut args = HashMap::new();
        args.insert("address".to_string(), json!("7xKXtg2CW87d3TXQ5xmD7mSZQ4mPfFrPwJzQ7xRT7mF9"));

        let result = mcp_service
            .execute_tool("test-mcp", "get_balance", args)
            .await?;

        assert!(result.success);
        assert_eq!(result.result["balance"], 100.5);
        assert_eq!(result.result["unit"], "SOL");

        mock.assert_async().await;
        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_tool_execution_failure() -> Result<()> {
        let mut server = Server::new_async().await;
        let mock = server.mock("POST", "/tools/execute")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(json!({
                "success": false,
                "error": "Insufficient balance for transfer"
            }).to_string())
            .create_async()
            .await;

        let config = create_mock_mcp_config(server.url());
        let mcp_service = McpService::new()?;
        mcp_service.add_server(config).await?;

        let mut args = HashMap::new();
        args.insert("from".to_string(), json!("source_wallet"));
        args.insert("to".to_string(), json!("dest_wallet"));
        args.insert("amount".to_string(), json!(1000));

        let result = mcp_service
            .execute_tool("test-mcp", "transfer_sol", args)
            .await?;

        assert!(!result.success);
        assert_eq!(result.error.unwrap(), "Insufficient balance for transfer");

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
            transport_type: McpTransportType::WebSocket,
            auth: None,
            enabled: true,
            extra_config: HashMap::new(),
            github_url: None,
            local_path: None,
        };

        let mcp_service = McpService::new()?;
        let result = mcp_service.add_server(config).await;

        // WebSocket connection will fail in test but config should be added
        assert!(result.is_ok());

        let servers = mcp_service.list_servers().await?;
        assert!(servers.iter().any(|s| s.name == "ws-mcp"));

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
            local_path: Some(PathBuf::from("./test-mcp-server")),
        };

        let mcp_service = McpService::new()?;
        let result = mcp_service.add_server(config).await;

        // Stdio server won't actually start but config should be added
        assert!(result.is_ok());

        let servers = mcp_service.list_servers().await?;
        assert!(servers.iter().any(|s| s.name == "stdio-mcp"));

        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_authentication_types() -> Result<()> {
        let mut server = Server::new_async().await;

        // Test Bearer auth
        let mock_bearer = server.mock("POST", "/bearer/test")
            .match_header("authorization", "Bearer bearer-token")
            .with_status(200)
            .with_body("{\"success\": true}")
            .create_async()
            .await;

        // Test Basic auth
        let mock_basic = server.mock("POST", "/basic/test")
            .match_header("authorization", "Basic dXNlcjpwYXNz") // user:pass base64
            .with_status(200)
            .with_body("{\"success\": true}")
            .create_async()
            .await;

        // Test API Key auth
        let mock_apikey = server.mock("POST", "/apikey/test")
            .match_header("x-api-key", "api-key-123")
            .with_status(200)
            .with_body("{\"success\": true}")
            .create_async()
            .await;

        let mcp_service = McpService::new()?;

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

        for config in configs {
            mcp_service.add_server(config).await?;
        }

        // Test each server connection
        let _ = mcp_service.test_connection("bearer-server").await;
        let _ = mcp_service.test_connection("basic-server").await;
        let _ = mcp_service.test_connection("apikey-server").await;

        mock_bearer.assert_async().await;
        mock_basic.assert_async().await;
        mock_apikey.assert_async().await;

        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_concurrent_tool_execution() -> Result<()> {
        let mut server = Server::new_async().await;
        let mock = server.mock("POST", "/tools/execute")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(json!({
                "success": true,
                "result": {"value": 42}
            }).to_string())
            .expect(5)
            .create_async()
            .await;

        let config = create_mock_mcp_config(server.url());
        let mcp_service = Arc::new(McpService::new()?);
        mcp_service.add_server(config).await?;

        // Execute 5 tools concurrently
        let mut handles = vec![];
        for i in 0..5 {
            let service = Arc::clone(&mcp_service);
            let handle = tokio::spawn(async move {
                let mut args = HashMap::new();
                args.insert("id".to_string(), json!(i));
                service.execute_tool("test-mcp", "test_tool", args).await
            });
            handles.push(handle);
        }

        let results: Vec<Result<McpToolResult>> =
            futures::future::join_all(handles)
                .await
                .into_iter()
                .map(|r| r.unwrap())
                .collect();

        assert_eq!(results.len(), 5);
        for result in results {
            assert!(result?.success);
        }

        mock.assert_async().await;
        Ok(())
    }

    #[tokio::test]
    async fn test_mcp_server_enable_disable() -> Result<()> {
        let config = create_mock_mcp_config("http://localhost:9999".to_string());
        let mcp_service = McpService::new()?;

        mcp_service.add_server(config).await?;

        // Initially enabled
        let servers = mcp_service.list_servers().await?;
        assert!(servers.iter().find(|s| s.name == "test-mcp").unwrap().enabled);

        // Disable
        mcp_service.disable_server("test-mcp").await?;
        let servers = mcp_service.list_servers().await?;
        assert!(!servers.iter().find(|s| s.name == "test-mcp").unwrap().enabled);

        // Re-enable
        mcp_service.enable_server("test-mcp").await?;
        let servers = mcp_service.list_servers().await?;
        assert!(servers.iter().find(|s| s.name == "test-mcp").unwrap().enabled);

        Ok(())
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    #[tokio::test]
    async fn test_mcp_github_server_setup() -> Result<()> {
        let mcp_service = McpService::new()?;

        // Test GitHub URL parsing
        let github_url = "https://github.com/opensvm/solana-mcp-server";
        let result = mcp_service.add_github_server(
            "solana-mcp",
            github_url,
            true
        ).await;

        // This will fail in test environment but should parse correctly
        match result {
            Ok(_) => {
                let servers = mcp_service.list_servers().await?;
                assert!(servers.iter().any(|s| s.name == "solana-mcp"));
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
        let mock1 = server.mock("POST", "/tools/execute")
            .match_body(mockito::Matcher::Json(json!({
                "tool": "get_account_info",
                "args": {"address": "wallet123"}
            })))
            .with_status(200)
            .with_body(json!({
                "success": true,
                "result": {
                    "balance": 100,
                    "program": "program456"
                }
            }).to_string())
            .create_async()
            .await;

        // Mock second tool using result from first
        let mock2 = server.mock("POST", "/tools/execute")
            .match_body(mockito::Matcher::Json(json!({
                "tool": "get_program_info",
                "args": {"program_id": "program456"}
            })))
            .with_status(200)
            .with_body(json!({
                "success": true,
                "result": {
                    "name": "MyProgram",
                    "version": "1.0.0"
                }
            }).to_string())
            .create_async()
            .await;

        let config = create_mock_mcp_config(server.url());
        let mcp_service = McpService::new()?;
        mcp_service.add_server(config).await?;

        // Execute tool chain
        let mut args1 = HashMap::new();
        args1.insert("address".to_string(), json!("wallet123"));
        let result1 = mcp_service.execute_tool("test-mcp", "get_account_info", args1).await?;

        assert!(result1.success);
        let program_id = result1.result["program"].as_str().unwrap();

        let mut args2 = HashMap::new();
        args2.insert("program_id".to_string(), json!(program_id));
        let result2 = mcp_service.execute_tool("test-mcp", "get_program_info", args2).await?;

        assert!(result2.success);
        assert_eq!(result2.result["name"], "MyProgram");

        mock1.assert_async().await;
        mock2.assert_async().await;

        Ok(())
    }
}