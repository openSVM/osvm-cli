//! MCP Bridge Tool - dynamically calls any configured MCP tool
use crate::services::mcp_service::{McpService, McpTool};
use crate::utils::debug_logger::{log_ovsm_value, get_verbosity, VerbosityLevel};
use ovsm::error::Result as OvsmResult;
use ovsm::runtime::Value as OvsmValue;
use ovsm::tools::Tool;
use serde_json::Value as JsonValue;
use std::sync::Arc;

pub struct McpBridgeTool {
    name: String,
    mcp_service: Arc<tokio::sync::Mutex<McpService>>,
}

impl McpBridgeTool {
    pub fn new(name: &str, mcp_service: Arc<tokio::sync::Mutex<McpService>>) -> Self {
        McpBridgeTool {
            name: name.to_string(),
            mcp_service,
        }
    }
}

impl Tool for McpBridgeTool {
    fn name(&self) -> &str {
        &self.name
    }

    fn description(&self) -> &str {
        "Dynamic MCP bridge - calls configured MCP tools"
    }

    fn execute(&self, args: &[OvsmValue]) -> OvsmResult<OvsmValue> {
        // 🔍 DETAILED DEBUG LOGGING AT BRIDGE LEVEL (only at Verbose level)
        if get_verbosity() >= VerbosityLevel::Verbose {
            println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("🌉 MCP BRIDGE TOOL CALL");
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("📍 Tool Name: {}", self.name);
            println!("📍 Args Count: {}", args.len());
            println!("\n📦 Arguments Received (OVSM Values):");
            for (i, arg) in args.iter().enumerate() {
                println!("  [{}]: {} = {}", i, arg.type_name(), arg);
            }
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");
        }

        // Lock the MCP service
        let service = self.mcp_service.clone();
        let mut svc = futures::executor::block_on(service.lock());

        // Initialize the MCP server if not already initialized
        const SERVER_ID: &str = "osvm-mcp";
        if let Err(e) = futures::executor::block_on(svc.initialize_server(SERVER_ID)) {
            return Err(ovsm::error::Error::RpcError {
                message: format!("Failed to initialize MCP server '{}': {}", SERVER_ID, e),
            });
        }

        // List configured tools from the correct server
        let tools = futures::executor::block_on(svc.list_tools(SERVER_ID)).unwrap_or_default();

        // Find matching tool metadata
        let _meta = tools
            .into_iter()
            .find(|t| t.name == self.name)
            .ok_or_else(|| ovsm::error::Error::RpcError {
                message: format!(
                    "MCP tool '{}' not found in server '{}'",
                    self.name, SERVER_ID
                ),
            })?;

        // Convert OvsmValue args to JSON properly
        fn ovsm_to_json(val: &OvsmValue) -> JsonValue {
            match val {
                OvsmValue::Null => JsonValue::Null,
                OvsmValue::Bool(b) => JsonValue::Bool(*b),
                OvsmValue::Int(i) => JsonValue::Number((*i).into()),
                OvsmValue::Float(f) => serde_json::Number::from_f64(*f)
                    .map(JsonValue::Number)
                    .unwrap_or(JsonValue::Null),
                OvsmValue::String(s) => JsonValue::String(s.clone()),
                OvsmValue::Array(arr) => JsonValue::Array(arr.iter().map(ovsm_to_json).collect()),
                OvsmValue::Object(obj) => {
                    let mut map = serde_json::Map::new();
                    for (k, v) in obj.iter() {
                        map.insert(k.clone(), ovsm_to_json(v));
                    }
                    JsonValue::Object(map)
                }
                _ => {
                    // Fallback for other types (Range, Function, etc.)
                    serde_json::from_str(&val.to_string()).unwrap_or(JsonValue::Null)
                }
            }
        }

        let params: Vec<JsonValue> = args.iter().map(ovsm_to_json).collect();

        // MCP protocol expects arguments as an object, not an array
        // If we have a single argument that's already an object, use it directly
        // Otherwise, wrap multiple args in an array
        let arguments = if params.len() == 1 && params[0].is_object() {
            Some(params[0].clone())
        } else if params.is_empty() {
            None
        } else {
            Some(JsonValue::Array(params.clone()))
        };

        if get_verbosity() >= VerbosityLevel::Verbose {
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("🔄 CONVERTED TO JSON-RPC");
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("📤 JSON Params ({} items):", params.len());
            for (i, param) in params.iter().enumerate() {
                println!(
                    "  [{}]: {}",
                    i,
                    serde_json::to_string_pretty(param).unwrap_or_else(|_| "<invalid>".to_string())
                );
            }
            println!("\n📤 Final Arguments for MCP:");
            if let Some(ref args) = arguments {
                println!(
                    "{}",
                    serde_json::to_string_pretty(args).unwrap_or_else(|_| "<invalid>".to_string())
                );
            } else {
                println!("  (none)");
            }
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");
        }

        // Execute the tool with correct server ID
        let result_json = futures::executor::block_on(
            svc.call_tool(SERVER_ID, &self.name, arguments),
        )
        .map_err(|e| ovsm::error::Error::RpcError {
            message: format!("MCP call_tool failed: {}", e),
        })?;

        // Convert JSON result back to OvsmValue
        fn json_to_ovsm(json: &JsonValue) -> OvsmValue {
            use std::collections::HashMap;
            use std::sync::Arc;

            match json {
                JsonValue::Null => OvsmValue::Null,
                JsonValue::Bool(b) => OvsmValue::Bool(*b),
                JsonValue::Number(n) => {
                    if let Some(i) = n.as_i64() {
                        OvsmValue::Int(i)
                    } else if let Some(f) = n.as_f64() {
                        OvsmValue::Float(f)
                    } else {
                        OvsmValue::Null
                    }
                }
                JsonValue::String(s) => OvsmValue::String(s.clone()),
                JsonValue::Array(arr) => {
                    let ovsm_arr: Vec<OvsmValue> = arr.iter().map(json_to_ovsm).collect();
                    OvsmValue::Array(Arc::new(ovsm_arr))
                }
                JsonValue::Object(obj) => {
                    // Check if this is an error object from the API
                    if let Some(error_msg) = obj.get("error") {
                        // Return null for errors - this allows the AI to check for null
                        // and handle appropriately without breaking type expectations
                        eprintln!("⚠️  API returned error: {}", error_msg);
                        return OvsmValue::Null;
                    }

                    let mut ovsm_obj = HashMap::new();
                    for (k, v) in obj.iter() {
                        ovsm_obj.insert(k.clone(), json_to_ovsm(v));
                    }
                    OvsmValue::Object(Arc::new(ovsm_obj))
                }
            }
        }

        // Convert the result and log its type
        let ovsm_result = json_to_ovsm(&result_json);

        // Debug logging to help AI understand response structure
        log_ovsm_value(&self.name, &ovsm_result);

        Ok(ovsm_result)
    }
}
