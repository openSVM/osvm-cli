/// Helper functions for calling real osvm.ai APIs
/// This module provides utilities for integration testing against actual osvm.ai endpoints

use serde_json::json;

// Real osvm.ai API endpoints
pub const OSVM_AI_BASE_URL: &str = "https://osvm.ai/api";
pub const RPC_PROXY_URL: &str = "https://osvm.ai/api/proxy/rpc";

/// Response from osvm.ai API call
#[derive(Debug, Clone)]
pub struct ApiResponse {
    pub status: u16,
    pub body: String,
}

/// Call osvm.ai API for AI planning (getAnswer endpoint)
///
/// # Arguments
/// * `query` - The natural language query to send to Claude AI
///
/// # Returns
/// * `Ok(ApiResponse)` - Contains HTTP status and response body
/// * `Err(String)` - Error message if the call fails
pub async fn call_osvm_ai_api(query: &str) -> Result<ApiResponse, String> {
    let client = reqwest::Client::new();

    println!("  📡 POST {} with query: '{}'", OSVM_AI_BASE_URL, query);

    match client
        .post(&format!("{}/getAnswer", OSVM_AI_BASE_URL))
        .json(&json!({
            "query": query
        }))
        .send()
        .await
    {
        Ok(response) => {
            let status = response.status().as_u16();
            match response.text().await {
                Ok(body) => {
                    println!("  ✓ Response: {} (size: {} bytes)", status, body.len());
                    Ok(ApiResponse { status, body })
                }
                Err(e) => Err(format!("Failed to read response body: {}", e)),
            }
        }
        Err(e) => Err(format!("API request failed: {}", e)),
    }
}

/// Call osvm.ai RPC proxy for Solana RPC calls
///
/// # Arguments
/// * `method` - JSON-RPC method name (e.g., "getBalance", "getHealth", "getSlot")
/// * `params` - Vector of JSON-RPC parameters
///
/// # Returns
/// * `Ok(ApiResponse)` - Contains HTTP status and response body (JSON-RPC response)
/// * `Err(String)` - Error message if the call fails
pub async fn call_rpc_proxy(
    method: &str,
    params: Vec<serde_json::Value>,
) -> Result<ApiResponse, String> {
    let client = reqwest::Client::new();

    println!("  📡 POST {} with method: '{}'", RPC_PROXY_URL, method);

    let request_body = json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": method,
        "params": params
    });

    match client
        .post(RPC_PROXY_URL)
        .json(&request_body)
        .send()
        .await
    {
        Ok(response) => {
            let status = response.status().as_u16();
            match response.text().await {
                Ok(body) => {
                    println!("  ✓ Response: {} (size: {} bytes)", status, body.len());
                    Ok(ApiResponse { status, body })
                }
                Err(e) => Err(format!("Failed to read response body: {}", e)),
            }
        }
        Err(e) => Err(format!("RPC proxy request failed: {}", e)),
    }
}

/// Helper to check if osvm.ai APIs are reachable
/// Useful for conditional test execution when APIs might be down
#[allow(dead_code)]
pub async fn check_apis_available() -> bool {
    let client = reqwest::Client::new();

    // Try a simple health check on the AI API
    match client
        .post(&format!("{}/getAnswer", OSVM_AI_BASE_URL))
        .json(&json!({ "query": "test" }))
        .send()
        .await
    {
        Ok(response) => response.status().as_u16() != 0,
        Err(_) => false,
    }
}
