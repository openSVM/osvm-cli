//! Debug logger for OVSM to understand API responses
use ovsm::runtime::Value as OvsmValue;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;

/// Verbosity levels for debug output
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub enum VerbosityLevel {
    Silent = 0,
    Basic = 1,
    Normal = 2,
    Detailed = 3,
    Verbose = 4,
}

lazy_static::lazy_static! {
    static ref VERBOSITY: Mutex<VerbosityLevel> = Mutex::new(VerbosityLevel::Silent);

    /// Global schema cache: Maps tool names to their response schemas
    static ref SCHEMA_CACHE: Arc<Mutex<HashMap<String, String>>> = Arc::new(Mutex::new(HashMap::new()));
}

/// Set the global verbosity level
pub fn set_verbosity(level: VerbosityLevel) {
    *VERBOSITY.lock().unwrap() = level;
}

/// Get the current verbosity level
pub fn get_verbosity() -> VerbosityLevel {
    *VERBOSITY.lock().unwrap()
}

/// Store schema for a tool response
pub fn cache_schema(tool_name: &str, schema: String) {
    if let Ok(mut cache) = SCHEMA_CACHE.lock() {
        cache.insert(tool_name.to_string(), schema);
    }
}

/// Get all cached schemas as a formatted string for AI context
pub fn get_cached_schemas() -> String {
    if let Ok(cache) = SCHEMA_CACHE.lock() {
        if cache.is_empty() {
            return String::new();
        }

        let mut result = String::from("\n\n## 🔍 ACTUAL API RESPONSE SCHEMAS (from this session):\n\n");
        result.push_str("These are the REAL schemas from tools you called. Use ONLY these fields:\n\n");

        for (tool, schema) in cache.iter() {
            result.push_str(&format!("### {}\n```\n{}\n```\n\n", tool, schema));
        }

        result
    } else {
        String::new()
    }
}

/// Clear schema cache (for testing)
pub fn clear_schema_cache() {
    if let Ok(mut cache) = SCHEMA_CACHE.lock() {
        cache.clear();
    }
}

/// Extract schema from OvsmValue as a human-readable string for AI
fn extract_schema(value: &OvsmValue, depth: usize) -> String {
    const MAX_DEPTH: usize = 2;
    let indent = "  ".repeat(depth);

    match value {
        OvsmValue::Null => "null".to_string(),
        OvsmValue::Bool(_) => "Bool".to_string(),
        OvsmValue::Int(_) => "Int".to_string(),
        OvsmValue::Float(_) => "Float".to_string(),
        OvsmValue::String(_) => "String".to_string(),
        OvsmValue::Array(arr) if arr.is_empty() => "[]".to_string(),
        OvsmValue::Array(arr) if depth < MAX_DEPTH => {
            // Show schema of first element
            format!("[{}]", extract_schema(&arr[0], depth + 1))
        }
        OvsmValue::Array(_) => "[...]".to_string(),
        OvsmValue::Object(obj) if depth < MAX_DEPTH => {
            let mut fields = Vec::new();
            for (key, val) in obj.iter() {
                let field_schema = extract_schema(val, depth + 1);
                let nullability = if matches!(val, OvsmValue::Null) {
                    " (⚠️ NULL)"
                } else {
                    ""
                };
                fields.push(format!("\n{}{}: {}{}", indent, key, field_schema, nullability));
            }
            format!("{{{}}}\n{}", fields.join(""), indent.trim_end())
        }
        OvsmValue::Object(_) => "{...}".to_string(),
        _ => "?".to_string(),
    }
}

/// Debug print macro
#[macro_export]
macro_rules! debug_print {
    ($($arg:tt)*) => {
        if $crate::utils::debug_logger::get_verbosity() >= $crate::utils::debug_logger::VerbosityLevel::Normal {
            eprintln!("🔍 DEBUG: {}", format!($($arg)*));
        }
    };
}

/// Debug warning macro
#[macro_export]
macro_rules! debug_warn {
    ($($arg:tt)*) => {
        if $crate::utils::debug_logger::get_verbosity() >= $crate::utils::debug_logger::VerbosityLevel::Normal {
            eprintln!("⚠️  WARN: {}", format!($($arg)*));
        }
    };
}

/// Debug error macro
#[macro_export]
macro_rules! debug_error {
    ($($arg:tt)*) => {
        eprintln!("❌ ERROR: {}", format!($($arg)*));
    };
}

/// Debug success macro
#[macro_export]
macro_rules! debug_success {
    ($($arg:tt)*) => {
        if $crate::utils::debug_logger::get_verbosity() >= $crate::utils::debug_logger::VerbosityLevel::Normal {
            eprintln!("✅ SUCCESS: {}", format!($($arg)*));
        }
    };
}

/// Log the type and structure of an OVSM value
pub fn log_ovsm_value(name: &str, value: &OvsmValue) {
    // Only log at Verbose level
    if get_verbosity() < VerbosityLevel::Verbose {
        return;
    }

    let type_str = match value {
        OvsmValue::Null => "null",
        OvsmValue::Bool(_) => "bool",
        OvsmValue::Int(_) => "int",
        OvsmValue::Float(_) => "float",
        OvsmValue::String(_) => "string",
        OvsmValue::Array(arr) => &format!("array[{}]", arr.len()),
        OvsmValue::Object(obj) => &format!(
            "object{{{}}}",
            obj.keys().cloned().collect::<Vec<_>>().join(", ")
        ),
        _ => "other",
    };

    eprintln!("🔍 DEBUG: {} returned type: {}", name, type_str);

    // For objects, show first-level fields with NULL highlighting
    if let OvsmValue::Object(obj) = value {
        eprintln!("📋 SCHEMA (fields that exist):");
        for (key, val) in obj.iter() {
            let (val_type, is_null) = match val {
                OvsmValue::Null => ("❌ NULL".to_string(), true),
                OvsmValue::Array(arr) => (format!("array[{}]", arr.len()), false),
                OvsmValue::Object(o) => (format!("object{{{}}}", o.keys().cloned().collect::<Vec<_>>().join(", ")), false),
                OvsmValue::String(s) if s.is_empty() => ("String(empty)".to_string(), false),
                OvsmValue::String(s) => (format!("String({:?})", s.chars().take(30).collect::<String>()), false),
                OvsmValue::Bool(b) => (format!("Bool({})", b), false),
                OvsmValue::Int(i) => (format!("Int({})", i), false),
                OvsmValue::Float(f) => (format!("Float({})", f), false),
                _ => (format!("{:?}", val).chars().take(30).collect(), false),
            };

            if is_null {
                eprintln!("    ⚠️  .{} = {} (check before using!)", key, val_type);
            } else {
                eprintln!("    ✓  .{} = {}", key, val_type);
            }
        }

        // Show nested schema for important objects
        for (key, val) in obj.iter() {
            if let OvsmValue::Object(nested) = val {
                eprintln!("  📂 Nested object .{}:", key);
                for (nested_key, nested_val) in nested.iter().take(10) {
                    let (nested_type, is_null) = match nested_val {
                        OvsmValue::Null => ("❌ NULL".to_string(), true),
                        OvsmValue::Array(arr) => (format!("array[{}]", arr.len()), false),
                        OvsmValue::Object(_) => ("object{{...}}".to_string(), false),
                        OvsmValue::String(s) => (format!("String({:?})", s.chars().take(15).collect::<String>()), false),
                        OvsmValue::Bool(b) => (format!("Bool({})", b), false),
                        OvsmValue::Int(i) => (format!("Int({})", i), false),
                        OvsmValue::Float(f) => (format!("Float({})", f), false),
                        _ => ("other".to_string(), false),
                    };

                    if is_null {
                        eprintln!("      ⚠️  .{}.{} = {}", key, nested_key, nested_type);
                    } else {
                        eprintln!("      ✓  .{}.{} = {}", key, nested_key, nested_type);
                    }
                }
            }
        }

        // Print content field details (useful for debugging MCP responses)
        if let Some(content) = obj.get("content") {
            eprintln!("📦 Content field details:");
            match content {
                OvsmValue::String(s) => eprintln!("    String: {}", s),
                OvsmValue::Array(arr) => {
                    eprintln!("    Array with {} items:", arr.len());
                    for (i, item) in arr.iter().take(5).enumerate() {
                        match item {
                            OvsmValue::String(s) => eprintln!("      [{}]: {}", i, s),
                            OvsmValue::Object(o) => {
                                eprintln!(
                                    "      [{}]: object with keys: {}",
                                    i,
                                    o.keys().cloned().collect::<Vec<_>>().join(", ")
                                );
                                // Print the actual error message fields
                                if let Some(OvsmValue::String(text)) = o.get("text") {
                                    eprintln!(
                                        "          📄 text: {}",
                                        text.chars().take(500).collect::<String>()
                                    );
                                    if text.len() > 500 {
                                        eprintln!(
                                            "          ... ({} more chars)",
                                            text.len() - 500
                                        );
                                    }
                                }
                                if let Some(OvsmValue::String(type_str)) = o.get("type") {
                                    eprintln!("          🏷️  type: {}", type_str);
                                }
                            }
                            _ => eprintln!("      [{}]: {:?}", i, item),
                        }
                    }
                }
                OvsmValue::Object(o) => {
                    eprintln!(
                        "    Object with keys: {}",
                        o.keys().cloned().collect::<Vec<_>>().join(", ")
                    );
                }
                _ => eprintln!("    {:?}", content),
            }
        }

        // If there's an isError field set to true, highlight it
        if let Some(OvsmValue::Bool(true)) = obj.get("isError") {
            eprintln!("⚠️  isError=true detected - this is an error response!");
        }
    }

    // Cache the schema for AI context (always, even at lower verbosity)
    let schema = extract_schema(value, 0);
    cache_schema(name, schema.clone());

    // Debug: show that schema was cached (only at Debug verbosity)
    if get_verbosity() >= VerbosityLevel::Verbose {
        eprintln!("💾 Cached schema for '{}' ({} chars)", name, schema.len());
    }
}

/// Create a type introspection tool for OVSM
pub struct TypeOfTool;

impl ovsm::tools::Tool for TypeOfTool {
    fn name(&self) -> &str {
        "typeof"
    }

    fn description(&self) -> &str {
        "Returns the type of a value as a string"
    }

    fn execute(&self, args: &[OvsmValue]) -> ovsm::error::Result<OvsmValue> {
        if args.is_empty() {
            return Ok(OvsmValue::String("null".to_string()));
        }

        let type_str = match &args[0] {
            OvsmValue::Null => "null",
            OvsmValue::Bool(_) => "bool",
            OvsmValue::Int(_) => "int",
            OvsmValue::Float(_) => "float",
            OvsmValue::String(_) => "string",
            OvsmValue::Array(_) => "array",
            OvsmValue::Object(_) => "object",
            _ => "unknown",
        };

        Ok(OvsmValue::String(type_str.to_string()))
    }
}

/// Create a keys inspection tool for objects
pub struct KeysTool;

impl ovsm::tools::Tool for KeysTool {
    fn name(&self) -> &str {
        "keys"
    }

    fn description(&self) -> &str {
        "Returns the keys of an object as an array of strings"
    }

    fn execute(&self, args: &[OvsmValue]) -> ovsm::error::Result<OvsmValue> {
        if args.is_empty() {
            return Ok(OvsmValue::Array(Arc::new(vec![])));
        }

        match &args[0] {
            OvsmValue::Object(obj) => {
                let keys: Vec<OvsmValue> =
                    obj.keys().map(|k| OvsmValue::String(k.clone())).collect();
                Ok(OvsmValue::Array(Arc::new(keys)))
            }
            _ => Ok(OvsmValue::Array(Arc::new(vec![]))),
        }
    }
}
