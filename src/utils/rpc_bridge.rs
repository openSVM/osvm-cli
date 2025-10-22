//! RPC Bridge for OVSM - allows OVSM scripts to call Solana RPC methods

use ovsm::runtime::Value as OvsmValue;
use ovsm::tools::{Tool, ToolRegistry};
use ovsm::error::Result as OvsmResult;
use serde_json::{json, Value};
use std::sync::Arc;

// Real Solana RPC endpoint
const SOLANA_RPC_URL: &str = "https://osvm.ai/api/proxy/rpc";

/// Generic RPC Bridge Tool - Dynamically calls ANY Solana RPC method
pub struct RpcBridgeTool {
    name: String,
}

impl RpcBridgeTool {
    pub fn new(name: &str) -> Self {
        RpcBridgeTool {
            name: name.to_string(),
        }
    }
}

impl Tool for RpcBridgeTool {
    fn name(&self) -> &str {
        &self.name
    }

    fn description(&self) -> &str {
        "Dynamic RPC bridge - calls Solana RPC methods"
    }

    fn execute(&self, args: &[OvsmValue]) -> OvsmResult<OvsmValue> {
        let mut rpc_params = Vec::new();

        // OVSM evaluator passes named parameters as positional arguments
        // For RPC calls like getSignaturesForAddress(address: "...", limit: 5)
        // We receive: [String("..."), Int(5)]
        // We need to convert to: ["...", {"limit": 5}]

        if args.is_empty() {
            // No arguments - just call the method
        } else if args.len() == 1 {
            // Single argument - could be just primary param or an object
            if let OvsmValue::Object(obj) = &args[0] {
                // If it's an object, check if it's a config or has a primary param
                let primary_param = obj.get("address")
                    .or_else(|| obj.get("signature"))
                    .or_else(|| obj.get("slot"))
                    .or_else(|| obj.get("pubkey"));

                if let Some(primary) = primary_param {
                    rpc_params.push(ovsm_value_to_json(primary));

                    // Build config object from remaining named parameters
                    let mut config = serde_json::Map::new();
                    for (key, val) in obj.iter() {
                        if !matches!(key.as_str(), "address" | "signature" | "slot" | "pubkey") {
                            config.insert(key.clone(), ovsm_value_to_json(val));
                        }
                    }

                    // Only add config object if it has fields
                    if !config.is_empty() {
                        rpc_params.push(Value::Object(config));
                    }
                } else {
                    // No primary param found, use the whole object as config
                    rpc_params.push(ovsm_value_to_json(&args[0]));
                }
            } else {
                // Single non-object argument (primary param only)
                rpc_params.push(ovsm_value_to_json(&args[0]));
            }
        } else {
            // Multiple arguments - first is primary param, rest form the config object
            // This handles cases like: getSignaturesForAddress(address: "...", limit: 5, before: "...")
            rpc_params.push(ovsm_value_to_json(&args[0]));

            // Build config object from remaining arguments
            // Check if second argument is already an object (named params)
            if args.len() == 2 && matches!(&args[1], OvsmValue::Object(_)) {
                // Second arg is a config object, use it directly
                rpc_params.push(ovsm_value_to_json(&args[1]));
            } else {
                // Build config from positional args
                let mut config = serde_json::Map::new();

                // For getSignaturesForAddress specifically
                if matches!(self.name.as_str(), "getSignaturesForAddress") {
                    if args.len() >= 2 {
                        config.insert("limit".to_string(), ovsm_value_to_json(&args[1]));
                    }
                    if args.len() >= 3 {
                        config.insert("before".to_string(), ovsm_value_to_json(&args[2]));
                    }
                } else {
                    // For other methods, try to infer config structure
                    for (i, arg) in args.iter().enumerate().skip(1) {
                        if let OvsmValue::Object(obj) = arg {
                            for (k, v) in obj.iter() {
                                config.insert(k.clone(), ovsm_value_to_json(v));
                            }
                        } else {
                            config.insert(format!("param{}", i), ovsm_value_to_json(arg));
                        }
                    }
                }

                if !config.is_empty() {
                    rpc_params.push(Value::Object(config));
                }
            }
        }

        // For getTransaction and getParsedTransaction, ensure we always include maxSupportedTransactionVersion
        if matches!(self.name.as_str(), "getTransaction" | "getParsedTransaction") {
            // If we have no config yet, add one
            if rpc_params.len() == 1 {
                let mut config = serde_json::Map::new();
                config.insert("maxSupportedTransactionVersion".to_string(), json!(0));

                // getParsedTransaction needs "jsonParsed" encoding
                if self.name == "getParsedTransaction" {
                    config.insert("encoding".to_string(), json!("jsonParsed"));
                } else {
                    config.insert("encoding".to_string(), json!("json"));
                }

                rpc_params.push(Value::Object(config));
            } else if rpc_params.len() >= 2 {
                // If we have a config, ensure it has maxSupportedTransactionVersion
                if let Some(Value::Object(config)) = rpc_params.get_mut(1) {
                    if !config.contains_key("maxSupportedTransactionVersion") {
                        config.insert("maxSupportedTransactionVersion".to_string(), json!(0));
                    }
                    if !config.contains_key("encoding") {
                        // getParsedTransaction needs "jsonParsed" encoding
                        if self.name == "getParsedTransaction" {
                            config.insert("encoding".to_string(), json!("jsonParsed"));
                        } else {
                            config.insert("encoding".to_string(), json!("json"));
                        }
                    }
                }
            }
        }

        let result = tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                call_solana_rpc(&self.name, rpc_params).await
            })
        });

        match result {
            Ok(json_val) => Ok(json_to_ovsm_value(&json_val)),
            Err(e) => Err(ovsm::error::Error::RpcError {
                message: format!("{} failed: {}", self.name, e),
            }),
        }
    }
}

async fn call_solana_rpc(method: &str, params: Vec<Value>) -> anyhow::Result<Value> {
    // Configure client with proper timeouts to prevent hanging
    let client = reqwest::Client::builder()
        .timeout(std::time::Duration::from_secs(30))       // Total request timeout
        .connect_timeout(std::time::Duration::from_secs(10)) // Connection timeout
        .build()?;

    let request_body = json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": method,
        "params": params
    });


    let response = client
        .post(SOLANA_RPC_URL)
        .json(&request_body)
        .send()
        .await?;

    let response_text = response.text().await?;
    let response_json: Value = serde_json::from_str(&response_text)?;

    if let Some(result) = response_json.get("result") {
        Ok(result.clone())
    } else if let Some(error) = response_json.get("error") {
        Err(anyhow::anyhow!("RPC error: {}", error))
    } else {
        Err(anyhow::anyhow!("Invalid RPC response: {}", response_text))
    }
}

fn ovsm_value_to_json(val: &OvsmValue) -> Value {
    match val {
        OvsmValue::Int(n) => json!(n),
        OvsmValue::Float(f) => json!(f),
        OvsmValue::String(s) => json!(s),
        OvsmValue::Bool(b) => json!(b),
        OvsmValue::Null => Value::Null,
        OvsmValue::Array(arr) => {
            let items: Vec<Value> = arr.iter().map(ovsm_value_to_json).collect();
            json!(items)
        }
        OvsmValue::Object(obj) => {
            let mut map = serde_json::Map::new();
            for (k, v) in obj.iter() {
                map.insert(k.clone(), ovsm_value_to_json(v));
            }
            Value::Object(map)
        }
        OvsmValue::Function { .. } => {
            // Functions cannot be serialized to JSON for RPC calls
            // This is an error - lambdas should not be passed to RPC
            json!({"error": "Cannot serialize function to JSON for RPC call"})
        }
        OvsmValue::Range { start, end } => {
            json!({"start": start, "end": end, "type": "range"})
        }
        OvsmValue::Multiple(vals) => {
            // Serialize multiple values as array
            let items: Vec<Value> = vals.iter().map(ovsm_value_to_json).collect();
            json!({"type": "multiple-values", "values": items})
        }
    }
}

fn json_to_ovsm_value(val: &Value) -> OvsmValue {
    match val {
        Value::Null => OvsmValue::Null,
        Value::Bool(b) => OvsmValue::Bool(*b),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                OvsmValue::Int(i)
            } else if let Some(f) = n.as_f64() {
                OvsmValue::Float(f)
            } else {
                OvsmValue::Null
            }
        }
        Value::String(s) => OvsmValue::String(s.clone()),
        Value::Array(arr) => {
            let items: Vec<OvsmValue> = arr.iter().map(json_to_ovsm_value).collect();
            OvsmValue::Array(Arc::new(items))
        }
        Value::Object(obj) => {
            let mut map = std::collections::HashMap::new();
            for (k, v) in obj.iter() {
                map.insert(k.clone(), json_to_ovsm_value(v));
            }
            OvsmValue::Object(Arc::new(map))
        }
    }
}

/// Create a tool registry with RPC bridge tools
pub fn create_rpc_registry() -> ToolRegistry {
    let mut registry = ToolRegistry::new();

    // Register common RPC methods
    let methods = vec![
        "getSignaturesForAddress",
        "getParsedTransaction",
        "getSlot",
        "getBlock",
        "getTransaction",
        "getAccountInfo",
        "getBalance",
        "getBlockTime",
        "getHealth",
        "getVersion",
    ];

    for method in methods {
        registry.register(RpcBridgeTool::new(method));
    }

    registry
}
