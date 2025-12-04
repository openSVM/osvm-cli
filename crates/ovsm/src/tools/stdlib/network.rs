//! Network operations for OVSM
//!
//! Provides HTTP, WebSocket, and JSON-RPC functionality for blockchain integration.

use crate::error::{Error, Result};
use crate::runtime::Value;
use serde_json::json;
use std::collections::HashMap;
use std::sync::Arc;

/// HTTP GET request
pub async fn http_get(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::InvalidArguments {
            tool: "http-get".to_string(),
            reason: "Expected url and optional headers".to_string(),
        });
    }

    let url = match &args[0] {
        Value::String(s) => s.as_str(),
        _ => {
            return Err(Error::InvalidArguments {
                tool: "http-get".to_string(),
                reason: format!("Expected string url, got {}", args[0].type_name()),
            })
        }
    };

    let client = reqwest::Client::new();
    let mut request = client.get(url);

    // Add headers if provided
    if args.len() > 1 {
        if let Value::Object(headers) = &args[1] {
            for (key, value) in headers.iter() {
                if let Value::String(val) = value {
                    request = request.header(key.as_str(), val.as_str());
                }
            }
        }
    }

    let response = request
        .send()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "http-get".to_string(),
            reason: format!("HTTP request failed: {}", e),
        })?;

    let status = response.status().as_u16() as i64;
    let body = response
        .text()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "http-get".to_string(),
            reason: format!("Failed to read response: {}", e),
        })?;

    Ok(Value::Object(Arc::new(
        [
            ("status".to_string(), Value::Int(status)),
            ("body".to_string(), Value::String(body)),
        ]
        .iter()
        .cloned()
        .collect(),
    )))
}

/// HTTP POST request
pub async fn http_post(args: &[Value]) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::InvalidArguments {
            tool: "http-post".to_string(),
            reason: format!("Expected url and body, got {} arguments", args.len()),
        });
    }

    let url = match &args[0] {
        Value::String(s) => s.as_str(),
        _ => {
            return Err(Error::InvalidArguments {
                tool: "http-post".to_string(),
                reason: format!("Expected string url, got {}", args[0].type_name()),
            })
        }
    };

    let client = reqwest::Client::new();
    let mut request = client.post(url);

    // Handle body
    match &args[1] {
        Value::String(s) => {
            request = request.body(s.to_string());
        }
        Value::Object(_) => {
            let json_body = value_to_json(&args[1])?;
            request = request.json(&json_body);
        }
        _ => {
            return Err(Error::InvalidArguments {
                tool: "http-post".to_string(),
                reason: format!(
                    "Expected string or object body, got {}",
                    args[1].type_name()
                ),
            })
        }
    }

    // Add headers if provided
    if args.len() > 2 {
        if let Value::Object(headers) = &args[2] {
            for (key, value) in headers.iter() {
                if let Value::String(val) = value {
                    request = request.header(key.as_str(), val.as_str());
                }
            }
        }
    }

    let response = request
        .send()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "http-post".to_string(),
            reason: format!("HTTP POST failed: {}", e),
        })?;

    let status = response.status().as_u16() as i64;
    let body = response
        .text()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "http-post".to_string(),
            reason: format!("Failed to read response: {}", e),
        })?;

    Ok(Value::Object(Arc::new(
        [
            ("status".to_string(), Value::Int(status)),
            ("body".to_string(), Value::String(body)),
        ]
        .iter()
        .cloned()
        .collect(),
    )))
}

/// JSON-RPC call
pub async fn json_rpc(args: &[Value]) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::InvalidArguments {
            tool: "json-rpc".to_string(),
            reason: format!("Expected url and method, got {} arguments", args.len()),
        });
    }

    let url = match &args[0] {
        Value::String(s) => s.as_str(),
        _ => {
            return Err(Error::InvalidArguments {
                tool: "json-rpc".to_string(),
                reason: format!("Expected string url, got {}", args[0].type_name()),
            })
        }
    };

    let method = match &args[1] {
        Value::String(s) => s.as_str(),
        _ => {
            return Err(Error::InvalidArguments {
                tool: "json-rpc".to_string(),
                reason: format!("Expected string method, got {}", args[1].type_name()),
            })
        }
    };

    // Build params array
    let params = if args.len() > 2 {
        match &args[2] {
            Value::Array(arr) => {
                let mut json_params = Vec::new();
                for item in arr.iter() {
                    json_params.push(value_to_json(item)?);
                }
                json_params
            }
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "json-rpc".to_string(),
                    reason: format!("Expected array params, got {}", args[2].type_name()),
                })
            }
        }
    } else {
        vec![]
    };

    // Build JSON-RPC request
    let request_body = json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": method,
        "params": params
    });

    // Make request
    let client = reqwest::Client::new();
    let response = client
        .post(url)
        .json(&request_body)
        .send()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "json-rpc".to_string(),
            reason: format!("JSON-RPC request failed: {}", e),
        })?;

    let body_text = response
        .text()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "json-rpc".to_string(),
            reason: format!("Failed to read response: {}", e),
        })?;

    // Parse JSON response
    let json_response: serde_json::Value =
        serde_json::from_str(&body_text).map_err(|e| Error::ToolExecutionError {
            tool: "json-rpc".to_string(),
            reason: format!("Failed to parse JSON response: {}", e),
        })?;

    // Check for RPC error
    if let Some(error) = json_response.get("error") {
        return Err(Error::ToolExecutionError {
            tool: "json-rpc".to_string(),
            reason: format!("JSON-RPC error: {}", error),
        });
    }

    // Extract result
    let result = json_response
        .get("result")
        .ok_or_else(|| Error::ToolExecutionError {
            tool: "json-rpc".to_string(),
            reason: "JSON-RPC response missing 'result' field".to_string(),
        })?;

    // Convert to Value
    json_to_value(result)
}

/// Convert OVSM Value to serde_json::Value
fn value_to_json(value: &Value) -> Result<serde_json::Value> {
    match value {
        Value::Null => Ok(serde_json::Value::Null),
        Value::Bool(b) => Ok(serde_json::Value::Bool(*b)),
        Value::Int(i) => Ok(serde_json::Value::Number((*i).into())),
        Value::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .ok_or_else(|| Error::ToolExecutionError {
                tool: "json-conversion".to_string(),
                reason: "Invalid float value for JSON".to_string(),
            }),
        Value::String(s) => Ok(serde_json::Value::String(s.to_string())),
        Value::Array(arr) => {
            let mut json_arr = Vec::new();
            for item in arr.iter() {
                json_arr.push(value_to_json(item)?);
            }
            Ok(serde_json::Value::Array(json_arr))
        }
        Value::Object(obj) => {
            let mut json_obj = serde_json::Map::new();
            for (key, val) in obj.iter() {
                json_obj.insert(key.clone(), value_to_json(val)?);
            }
            Ok(serde_json::Value::Object(json_obj))
        }
        _ => Err(Error::ToolExecutionError {
            tool: "json-conversion".to_string(),
            reason: format!("Cannot convert {} to JSON", value.type_name()),
        }),
    }
}

/// Convert serde_json::Value to OVSM Value
fn json_to_value(json: &serde_json::Value) -> Result<Value> {
    match json {
        serde_json::Value::Null => Ok(Value::Null),
        serde_json::Value::Bool(b) => Ok(Value::Bool(*b)),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(Value::Int(i))
            } else if let Some(f) = n.as_f64() {
                Ok(Value::Float(f))
            } else {
                Err(Error::ToolExecutionError {
                    tool: "json-conversion".to_string(),
                    reason: "Invalid JSON number".to_string(),
                })
            }
        }
        serde_json::Value::String(s) => Ok(Value::String(s.clone())),
        serde_json::Value::Array(arr) => {
            let mut values = Vec::new();
            for item in arr {
                values.push(json_to_value(item)?);
            }
            Ok(Value::Array(Arc::new(values)))
        }
        serde_json::Value::Object(obj) => {
            let mut map = HashMap::new();
            for (key, val) in obj {
                map.insert(key.clone(), json_to_value(val)?);
            }
            Ok(Value::Object(Arc::new(map)))
        }
    }
}
