//! Parsing tools for OVSM - JSON, Base58, Base64, URLs, and more

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::collections::HashMap;

// ============================================================================
// JSON PARSING
// ============================================================================

/// Parse JSON string to OVSM value
pub struct JsonParseTool;

impl Tool for JsonParseTool {
    fn name(&self) -> &str {
        "parse-json"
    }

    fn description(&self) -> &str {
        "Parse a JSON string into an OVSM value"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (JSON string)".to_string(),
            });
        }

        let json_str = args[0].as_string()?;

        let parsed: serde_json::Value =
            serde_json::from_str(json_str).map_err(|e| Error::ToolExecutionError {
                tool: self.name().to_string(),
                reason: format!("Failed to parse JSON: {}", e),
            })?;

        Ok(json_to_ovsm(parsed))
    }
}

fn json_to_ovsm(val: serde_json::Value) -> Value {
    match val {
        serde_json::Value::Null => Value::Null,
        serde_json::Value::Bool(b) => Value::Bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::Null
            }
        }
        serde_json::Value::String(s) => Value::String(s),
        serde_json::Value::Array(arr) => Value::array(arr.into_iter().map(json_to_ovsm).collect()),
        serde_json::Value::Object(obj) => {
            let mut map = HashMap::new();
            for (k, v) in obj {
                map.insert(k, json_to_ovsm(v));
            }
            Value::object(map)
        }
    }
}

/// Stringify OVSM value to JSON
pub struct JsonStringifyTool;

impl Tool for JsonStringifyTool {
    fn name(&self) -> &str {
        "json-stringify"
    }

    fn description(&self) -> &str {
        "Convert an OVSM value to a JSON string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected at least 1 argument".to_string(),
            });
        }

        let pretty = args.len() > 1;

        let json_val = ovsm_to_json(&args[0]);
        let json_str = if pretty {
            serde_json::to_string_pretty(&json_val)
        } else {
            serde_json::to_string(&json_val)
        }
        .map_err(|e| Error::ToolExecutionError {
            tool: self.name().to_string(),
            reason: format!("Failed to stringify JSON: {}", e),
        })?;

        Ok(Value::String(json_str))
    }
}

fn ovsm_to_json(val: &Value) -> serde_json::Value {
    match val {
        Value::Null => serde_json::Value::Null,
        Value::Bool(b) => serde_json::Value::Bool(*b),
        Value::Int(n) => serde_json::json!(*n),
        Value::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        Value::String(s) => serde_json::Value::String(s.clone()),
        Value::Array(arr) => serde_json::Value::Array(arr.iter().map(ovsm_to_json).collect()),
        Value::Object(obj) => {
            let mut map = serde_json::Map::new();
            for (k, v) in obj.iter() {
                map.insert(k.clone(), ovsm_to_json(v));
            }
            serde_json::Value::Object(map)
        }
        _ => serde_json::Value::String(format!("<{}>", val.type_name())),
    }
}

// ============================================================================
// BASE58 (Solana addresses)
// ============================================================================

/// Decode a Base58 string to a byte array
pub struct Base58DecodeTool;

impl Tool for Base58DecodeTool {
    fn name(&self) -> &str {
        "base58-decode"
    }

    fn description(&self) -> &str {
        "Decode a Base58 string to a byte array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (Base58 string)".to_string(),
            });
        }

        let encoded = args[0].as_string()?;

        let decoded = bs58::decode(encoded)
            .into_vec()
            .map_err(|e| Error::ToolExecutionError {
                tool: self.name().to_string(),
                reason: format!("Failed to decode Base58: {}", e),
            })?;

        Ok(Value::array(
            decoded.into_iter().map(|b| Value::Int(b as i64)).collect(),
        ))
    }
}

/// Encode a byte array to a Base58 string
pub struct Base58EncodeTool;

impl Tool for Base58EncodeTool {
    fn name(&self) -> &str {
        "base58-encode"
    }

    fn description(&self) -> &str {
        "Encode a byte array to a Base58 string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (byte array)".to_string(),
            });
        }

        let bytes = args[0].as_array()?;
        let byte_vec: Vec<u8> = bytes
            .iter()
            .filter_map(|v| v.as_int().ok().map(|n| n as u8))
            .collect();

        let encoded = bs58::encode(byte_vec).into_string();
        Ok(Value::String(encoded))
    }
}

// ============================================================================
// BASE64
// ============================================================================

/// Decode a Base64 string to a byte array
pub struct Base64DecodeTool;

impl Tool for Base64DecodeTool {
    fn name(&self) -> &str {
        "base64-decode"
    }

    fn description(&self) -> &str {
        "Decode a Base64 string to a byte array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        use base64::{engine::general_purpose, Engine as _};

        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (Base64 string)".to_string(),
            });
        }

        let encoded = args[0].as_string()?;

        let decoded =
            general_purpose::STANDARD
                .decode(encoded)
                .map_err(|e| Error::ToolExecutionError {
                    tool: self.name().to_string(),
                    reason: format!("Failed to decode Base64: {}", e),
                })?;

        Ok(Value::array(
            decoded.into_iter().map(|b| Value::Int(b as i64)).collect(),
        ))
    }
}

/// Encode a byte array to a Base64 string
pub struct Base64EncodeTool;

impl Tool for Base64EncodeTool {
    fn name(&self) -> &str {
        "base64-encode"
    }

    fn description(&self) -> &str {
        "Encode a byte array to a Base64 string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        use base64::{engine::general_purpose, Engine as _};

        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (byte array)".to_string(),
            });
        }

        let bytes = args[0].as_array()?;
        let byte_vec: Vec<u8> = bytes
            .iter()
            .filter_map(|v| v.as_int().ok().map(|n| n as u8))
            .collect();

        let encoded = general_purpose::STANDARD.encode(byte_vec);
        Ok(Value::String(encoded))
    }
}

// ============================================================================
// HEX
// ============================================================================

/// Decode a hexadecimal string to a byte array
pub struct HexDecodeTool;

impl Tool for HexDecodeTool {
    fn name(&self) -> &str {
        "hex-decode"
    }

    fn description(&self) -> &str {
        "Decode a hexadecimal string to a byte array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (hex string)".to_string(),
            });
        }

        let hex_str = args[0].as_string()?;
        let hex_str = hex_str.strip_prefix("0x").unwrap_or(hex_str);

        let decoded = hex::decode(hex_str).map_err(|e| Error::ToolExecutionError {
            tool: self.name().to_string(),
            reason: format!("Failed to decode hex: {}", e),
        })?;

        Ok(Value::array(
            decoded.into_iter().map(|b| Value::Int(b as i64)).collect(),
        ))
    }
}

/// Encode a byte array to a hexadecimal string
pub struct HexEncodeTool;

impl Tool for HexEncodeTool {
    fn name(&self) -> &str {
        "hex-encode"
    }

    fn description(&self) -> &str {
        "Encode a byte array to a hexadecimal string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (byte array)".to_string(),
            });
        }

        let bytes = args[0].as_array()?;
        let prefix = args.len() > 1;

        let byte_vec: Vec<u8> = bytes
            .iter()
            .filter_map(|v| v.as_int().ok().map(|n| n as u8))
            .collect();

        let encoded = hex::encode(byte_vec);
        let result = if prefix {
            format!("0x{}", encoded)
        } else {
            encoded
        };

        Ok(Value::String(result))
    }
}

// ============================================================================
// URL PARSING
// ============================================================================

/// Parse a URL string into components
pub struct UrlParseTool;

impl Tool for UrlParseTool {
    fn name(&self) -> &str {
        "parse-url"
    }

    fn description(&self) -> &str {
        "Parse a URL string into components"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (URL string)".to_string(),
            });
        }

        let url_str = args[0].as_string()?;

        let url = url::Url::parse(url_str).map_err(|e| Error::ToolExecutionError {
            tool: self.name().to_string(),
            reason: format!("Failed to parse URL: {}", e),
        })?;

        let mut result = HashMap::new();
        result.insert(
            "scheme".to_string(),
            Value::String(url.scheme().to_string()),
        );
        result.insert(
            "host".to_string(),
            url.host_str()
                .map(|h| Value::String(h.to_string()))
                .unwrap_or(Value::Null),
        );
        result.insert(
            "port".to_string(),
            url.port()
                .map(|p| Value::Int(p as i64))
                .unwrap_or(Value::Null),
        );
        result.insert("path".to_string(), Value::String(url.path().to_string()));
        result.insert(
            "query".to_string(),
            url.query()
                .map(|q| Value::String(q.to_string()))
                .unwrap_or(Value::Null),
        );
        result.insert(
            "fragment".to_string(),
            url.fragment()
                .map(|f| Value::String(f.to_string()))
                .unwrap_or(Value::Null),
        );

        Ok(Value::object(result))
    }
}

// ============================================================================
// NUMBER PARSING
// ============================================================================

/// Parse a string to an integer
pub struct ParseIntTool;

impl Tool for ParseIntTool {
    fn name(&self) -> &str {
        "parse-int"
    }

    fn description(&self) -> &str {
        "Parse a string to an integer"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (string to parse)".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let base = if args.len() > 1 {
            args[1].as_int()? as u32
        } else {
            10
        };

        let parsed = i64::from_str_radix(s, base).map_err(|e| Error::ToolExecutionError {
            tool: self.name().to_string(),
            reason: format!("Failed to parse integer: {}", e),
        })?;

        Ok(Value::Int(parsed))
    }
}

/// Parse a string to a floating point number
pub struct ParseFloatTool;

impl Tool for ParseFloatTool {
    fn name(&self) -> &str {
        "parse-float"
    }

    fn description(&self) -> &str {
        "Parse a string to a floating point number"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (string to parse)".to_string(),
            });
        }

        let s = args[0].as_string()?;

        let parsed: f64 = s.parse().map_err(|e| Error::ToolExecutionError {
            tool: self.name().to_string(),
            reason: format!("Failed to parse float: {}", e),
        })?;

        Ok(Value::Float(parsed))
    }
}

// ============================================================================
// CSV PARSING
// ============================================================================

/// Parse a CSV string into an array of objects
pub struct ParseCsvTool;

impl Tool for ParseCsvTool {
    fn name(&self) -> &str {
        "parse-csv"
    }

    fn description(&self) -> &str {
        "Parse a CSV string into an array of objects"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (CSV string)".to_string(),
            });
        }

        let csv_str = args[0].as_string()?;

        let delimiter = if args.len() > 1 {
            args[1]
                .as_string()
                .ok()
                .and_then(|s| s.chars().next())
                .unwrap_or(',')
        } else {
            ','
        };

        let mut reader = csv::ReaderBuilder::new()
            .delimiter(delimiter as u8)
            .from_reader(csv_str.as_bytes());

        let headers = reader
            .headers()
            .map_err(|e| Error::ToolExecutionError {
                tool: self.name().to_string(),
                reason: format!("Failed to read CSV headers: {}", e),
            })?
            .iter()
            .map(|h| h.to_string())
            .collect::<Vec<_>>();

        let mut result = Vec::new();

        for record in reader.records() {
            let record = record.map_err(|e| Error::ToolExecutionError {
                tool: self.name().to_string(),
                reason: format!("Failed to read CSV record: {}", e),
            })?;

            let mut obj = HashMap::new();
            for (i, field) in record.iter().enumerate() {
                if let Some(header) = headers.get(i) {
                    obj.insert(header.clone(), Value::String(field.to_string()));
                }
            }
            result.push(Value::object(obj));
        }

        Ok(Value::array(result))
    }
}

// ============================================================================
// REGISTRATION
// ============================================================================

/// Register all parsing tools with the tool registry
pub fn register(registry: &mut ToolRegistry) {
    // JSON
    registry.register(JsonParseTool);
    registry.register(JsonStringifyTool);

    // Base58 (Solana addresses)
    registry.register(Base58DecodeTool);
    registry.register(Base58EncodeTool);

    // Base64
    registry.register(Base64DecodeTool);
    registry.register(Base64EncodeTool);

    // Hex
    registry.register(HexDecodeTool);
    registry.register(HexEncodeTool);

    // URL
    registry.register(UrlParseTool);

    // Numbers
    registry.register(ParseIntTool);
    registry.register(ParseFloatTool);

    // CSV
    registry.register(ParseCsvTool);
}
