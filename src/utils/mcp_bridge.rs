//! MCP Bridge Tool - dynamically calls any configured MCP tool
use ovsm::runtime::Value as OvsmValue;
use ovsm::tools::Tool;
use ovsm::error::Result as OvsmResult;
use crate::services::mcp_service::{McpService, McpTool};
use std::sync::Arc;
use serde_json::Value as JsonValue;

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
        // Lock the MCP service
        let service = self.mcp_service.clone();
        let mut svc = futures::executor::block_on(service.lock());
        // List configured tools
        let tools = futures::executor::block_on(svc.list_tools("default_server")).unwrap_or_default();
        // Find matching tool metadata
        let meta = tools.into_iter()
            .find(|t| t.name == self.name)
            .ok_or_else(|| ovsm::error::Error::RpcError { message: format!("MCP tool '{}' not found", self.name) })?;
        // Convert OvsmValue args to JSON
        let params: Vec<JsonValue> = args.iter()
            .map(|v| serde_json::from_str(&v.to_string()).unwrap_or(JsonValue::Null))
            .collect();
        // Execute the tool
        let result_json = futures::executor::block_on(svc.call_tool("", &self.name, Some(JsonValue::Array(params))))
            .map_err(|e| ovsm::error::Error::RpcError { message: e.to_string() })?;
        // Convert JSON back to OvsmValue (simplest case)
        let ovsm_val = OvsmValue::String(result_json.to_string());
        Ok(ovsm_val)
    }
}
