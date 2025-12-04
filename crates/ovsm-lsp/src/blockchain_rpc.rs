//! Live Blockchain RPC Integration
//!
//! Connects the LSP REPL to actual Solana RPC endpoints, enabling
//! live blockchain queries directly from the editor.
//!
//! # Supported Operations
//!
//! - `getBalance(address)` - Get SOL balance for an address
//! - `getAccountInfo(address)` - Get full account information
//! - `getSignaturesForAddress(address, limit?)` - Get transaction signatures
//! - `getTransaction(signature)` - Get transaction details
//! - `getSlot()` - Get current slot
//! - `getBlockTime(slot)` - Get block timestamp
//!
//! # Configuration
//!
//! The RPC endpoint can be configured via:
//! - Environment variable: `SOLANA_RPC_URL`
//! - Default: `https://api.mainnet-beta.solana.com`

use reqwest::Client;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;

/// RPC endpoint configuration
#[derive(Debug, Clone)]
pub struct RpcConfig {
    /// RPC endpoint URL
    pub url: String,
    /// Request timeout in seconds
    pub timeout_secs: u64,
    /// Commitment level (processed, confirmed, finalized)
    pub commitment: String,
}

impl Default for RpcConfig {
    fn default() -> Self {
        Self {
            url: std::env::var("SOLANA_RPC_URL")
                .unwrap_or_else(|_| "https://api.mainnet-beta.solana.com".to_string()),
            timeout_secs: 30,
            commitment: "confirmed".to_string(),
        }
    }
}

/// Live blockchain RPC client
#[derive(Debug)]
pub struct BlockchainRpc {
    config: RpcConfig,
    client: Client,
    request_id: Arc<RwLock<u64>>,
}

impl BlockchainRpc {
    /// Create a new RPC client with default config
    pub fn new() -> Self {
        Self::with_config(RpcConfig::default())
    }

    /// Create with custom config
    pub fn with_config(config: RpcConfig) -> Self {
        let client = Client::builder()
            .timeout(Duration::from_secs(config.timeout_secs))
            .build()
            .expect("Failed to create HTTP client");

        Self {
            config,
            client,
            request_id: Arc::new(RwLock::new(0)),
        }
    }

    /// Get next request ID
    async fn next_id(&self) -> u64 {
        let mut id = self.request_id.write().await;
        *id += 1;
        *id
    }

    /// Make a JSON-RPC request
    async fn rpc_request(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        let id = self.next_id().await;

        let request = json!({
            "jsonrpc": "2.0",
            "id": id,
            "method": method,
            "params": params
        });

        let response = self
            .client
            .post(&self.config.url)
            .header("Content-Type", "application/json")
            .json(&request)
            .send()
            .await
            .map_err(|e| RpcError::Network(e.to_string()))?;

        let json: JsonRpcResponse = response
            .json()
            .await
            .map_err(|e| RpcError::Parse(e.to_string()))?;

        if let Some(error) = json.error {
            return Err(RpcError::Rpc {
                code: error.code,
                message: error.message,
            });
        }

        json.result.ok_or(RpcError::NoResult)
    }

    /// Get SOL balance for an address
    pub async fn get_balance(&self, address: &str) -> Result<u64, RpcError> {
        let params = json!([
            address,
            {"commitment": self.config.commitment}
        ]);

        let result = self.rpc_request("getBalance", params).await?;
        result["value"]
            .as_u64()
            .ok_or(RpcError::Parse("Invalid balance value".to_string()))
    }

    /// Get SOL balance as human-readable string
    pub async fn get_balance_sol(&self, address: &str) -> Result<f64, RpcError> {
        let lamports = self.get_balance(address).await?;
        Ok(lamports as f64 / 1_000_000_000.0)
    }

    /// Get account info
    pub async fn get_account_info(&self, address: &str) -> Result<AccountInfo, RpcError> {
        let params = json!([
            address,
            {
                "encoding": "jsonParsed",
                "commitment": self.config.commitment
            }
        ]);

        let result = self.rpc_request("getAccountInfo", params).await?;

        if result["value"].is_null() {
            return Err(RpcError::AccountNotFound(address.to_string()));
        }

        let value = &result["value"];
        Ok(AccountInfo {
            lamports: value["lamports"].as_u64().unwrap_or(0),
            owner: value["owner"].as_str().unwrap_or("").to_string(),
            executable: value["executable"].as_bool().unwrap_or(false),
            rent_epoch: value["rentEpoch"].as_u64().unwrap_or(0),
            data: value["data"].clone(),
        })
    }

    /// Get transaction signatures for an address
    pub async fn get_signatures(
        &self,
        address: &str,
        limit: Option<usize>,
    ) -> Result<Vec<SignatureInfo>, RpcError> {
        let limit = limit.unwrap_or(10);
        let params = json!([
            address,
            {
                "limit": limit,
                "commitment": self.config.commitment
            }
        ]);

        let result = self.rpc_request("getSignaturesForAddress", params).await?;

        let signatures: Vec<SignatureInfo> = result
            .as_array()
            .ok_or(RpcError::Parse("Expected array".to_string()))?
            .iter()
            .filter_map(|v| {
                Some(SignatureInfo {
                    signature: v["signature"].as_str()?.to_string(),
                    slot: v["slot"].as_u64()?,
                    err: v["err"].clone(),
                    memo: v["memo"].as_str().map(|s| s.to_string()),
                    block_time: v["blockTime"].as_i64(),
                })
            })
            .collect();

        Ok(signatures)
    }

    /// Get transaction details
    pub async fn get_transaction(&self, signature: &str) -> Result<TransactionInfo, RpcError> {
        let params = json!([
            signature,
            {
                "encoding": "jsonParsed",
                "commitment": self.config.commitment,
                "maxSupportedTransactionVersion": 0
            }
        ]);

        let result = self.rpc_request("getTransaction", params).await?;

        if result.is_null() {
            return Err(RpcError::TransactionNotFound(signature.to_string()));
        }

        Ok(TransactionInfo {
            slot: result["slot"].as_u64().unwrap_or(0),
            block_time: result["blockTime"].as_i64(),
            meta: result["meta"].clone(),
            transaction: result["transaction"].clone(),
        })
    }

    /// Get current slot
    pub async fn get_slot(&self) -> Result<u64, RpcError> {
        let params = json!([{"commitment": self.config.commitment}]);
        let result = self.rpc_request("getSlot", params).await?;
        result
            .as_u64()
            .ok_or(RpcError::Parse("Invalid slot".to_string()))
    }

    /// Get block time for a slot
    pub async fn get_block_time(&self, slot: u64) -> Result<i64, RpcError> {
        let params = json!([slot]);
        let result = self.rpc_request("getBlockTime", params).await?;
        result
            .as_i64()
            .ok_or(RpcError::Parse("Invalid block time".to_string()))
    }

    /// Get token accounts by owner
    pub async fn get_token_accounts_by_owner(
        &self,
        owner: &str,
    ) -> Result<Vec<TokenAccount>, RpcError> {
        let params = json!([
            owner,
            {"programId": "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"},
            {
                "encoding": "jsonParsed",
                "commitment": self.config.commitment
            }
        ]);

        let result = self.rpc_request("getTokenAccountsByOwner", params).await?;

        let accounts: Vec<TokenAccount> = result["value"]
            .as_array()
            .ok_or(RpcError::Parse("Expected array".to_string()))?
            .iter()
            .filter_map(|v| {
                let info = &v["account"]["data"]["parsed"]["info"];
                Some(TokenAccount {
                    pubkey: v["pubkey"].as_str()?.to_string(),
                    mint: info["mint"].as_str()?.to_string(),
                    owner: info["owner"].as_str()?.to_string(),
                    amount: info["tokenAmount"]["uiAmount"].as_f64()?,
                    decimals: info["tokenAmount"]["decimals"].as_u64()? as u8,
                })
            })
            .collect();

        Ok(accounts)
    }

    /// Get recent blockhash
    pub async fn get_recent_blockhash(&self) -> Result<String, RpcError> {
        let params = json!([{"commitment": self.config.commitment}]);
        let result = self.rpc_request("getLatestBlockhash", params).await?;
        result["value"]["blockhash"]
            .as_str()
            .map(|s| s.to_string())
            .ok_or(RpcError::Parse("Invalid blockhash".to_string()))
    }

    /// Get cluster nodes
    pub async fn get_cluster_nodes(&self) -> Result<Vec<ClusterNode>, RpcError> {
        let result = self.rpc_request("getClusterNodes", json!([])).await?;

        let nodes: Vec<ClusterNode> = result
            .as_array()
            .ok_or(RpcError::Parse("Expected array".to_string()))?
            .iter()
            .filter_map(|v| {
                Some(ClusterNode {
                    pubkey: v["pubkey"].as_str()?.to_string(),
                    gossip: v["gossip"].as_str().map(|s| s.to_string()),
                    tpu: v["tpu"].as_str().map(|s| s.to_string()),
                    rpc: v["rpc"].as_str().map(|s| s.to_string()),
                    version: v["version"].as_str().map(|s| s.to_string()),
                })
            })
            .collect();

        Ok(nodes)
    }
}

impl Default for BlockchainRpc {
    fn default() -> Self {
        Self::new()
    }
}

/// RPC error types
#[derive(Debug, Clone)]
pub enum RpcError {
    Network(String),
    Parse(String),
    Rpc { code: i64, message: String },
    NoResult,
    AccountNotFound(String),
    TransactionNotFound(String),
}

impl std::fmt::Display for RpcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RpcError::Network(e) => write!(f, "Network error: {}", e),
            RpcError::Parse(e) => write!(f, "Parse error: {}", e),
            RpcError::Rpc { code, message } => write!(f, "RPC error {}: {}", code, message),
            RpcError::NoResult => write!(f, "No result returned"),
            RpcError::AccountNotFound(addr) => write!(f, "Account not found: {}", addr),
            RpcError::TransactionNotFound(sig) => write!(f, "Transaction not found: {}", sig),
        }
    }
}

impl std::error::Error for RpcError {}

/// JSON-RPC response structure
#[derive(Debug, Deserialize)]
struct JsonRpcResponse {
    result: Option<Value>,
    error: Option<JsonRpcError>,
}

#[derive(Debug, Deserialize)]
struct JsonRpcError {
    code: i64,
    message: String,
}

/// Account information
#[derive(Debug, Clone, Serialize)]
pub struct AccountInfo {
    pub lamports: u64,
    pub owner: String,
    pub executable: bool,
    pub rent_epoch: u64,
    pub data: Value,
}

/// Signature information
#[derive(Debug, Clone, Serialize)]
pub struct SignatureInfo {
    pub signature: String,
    pub slot: u64,
    pub err: Value,
    pub memo: Option<String>,
    pub block_time: Option<i64>,
}

/// Transaction information
#[derive(Debug, Clone, Serialize)]
pub struct TransactionInfo {
    pub slot: u64,
    pub block_time: Option<i64>,
    pub meta: Value,
    pub transaction: Value,
}

/// Token account information
#[derive(Debug, Clone, Serialize)]
pub struct TokenAccount {
    pub pubkey: String,
    pub mint: String,
    pub owner: String,
    pub amount: f64,
    pub decimals: u8,
}

/// Cluster node information
#[derive(Debug, Clone, Serialize)]
pub struct ClusterNode {
    pub pubkey: String,
    pub gossip: Option<String>,
    pub tpu: Option<String>,
    pub rpc: Option<String>,
    pub version: Option<String>,
}

/// Convert RPC results to OVSM-compatible JSON values
pub fn to_ovsm_value(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Number(n) => n.to_string(),
        Value::String(s) => format!("\"{}\"", s),
        Value::Array(arr) => {
            let items: Vec<String> = arr.iter().map(to_ovsm_value).collect();
            format!("[{}]", items.join(" "))
        }
        Value::Object(obj) => {
            let items: Vec<String> = obj
                .iter()
                .map(|(k, v)| format!(":{} {}", k, to_ovsm_value(v)))
                .collect();
            format!("{{{}}}", items.join(" "))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rpc_config_default() {
        let config = RpcConfig::default();
        assert!(config.url.contains("solana"));
        assert_eq!(config.commitment, "confirmed");
    }

    #[test]
    fn test_to_ovsm_value() {
        assert_eq!(to_ovsm_value(&json!(null)), "null");
        assert_eq!(to_ovsm_value(&json!(true)), "true");
        assert_eq!(to_ovsm_value(&json!(42)), "42");
        assert_eq!(to_ovsm_value(&json!("hello")), "\"hello\"");
        assert_eq!(to_ovsm_value(&json!([1, 2, 3])), "[1 2 3]");
        assert_eq!(to_ovsm_value(&json!({"name": "test"})), "{:name \"test\"}");
    }

    #[tokio::test]
    async fn test_rpc_client_creation() {
        let rpc = BlockchainRpc::new();
        assert_eq!(rpc.config.commitment, "confirmed");
    }

    // Integration tests require network access
    // Run with: cargo test --features integration -- --ignored

    #[tokio::test]
    #[ignore]
    async fn test_get_slot_live() {
        let rpc = BlockchainRpc::new();
        let slot = rpc.get_slot().await;
        assert!(slot.is_ok());
        assert!(slot.unwrap() > 0);
    }

    #[tokio::test]
    #[ignore]
    async fn test_get_balance_live() {
        let rpc = BlockchainRpc::new();
        // Solana Foundation's address
        let balance = rpc
            .get_balance("HN7cABqLq46Es1jh92dQQisAq662SmxELLLsHHe4YWrH")
            .await;
        assert!(balance.is_ok());
    }
}
