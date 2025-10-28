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
    static ref VERBOSITY: Mutex<VerbosityLevel> = Mutex::new(VerbosityLevel::Normal);
}

/// Set the global verbosity level
pub fn set_verbosity(level: VerbosityLevel) {
    *VERBOSITY.lock().unwrap() = level;
}

/// Get the current verbosity level
pub fn get_verbosity() -> VerbosityLevel {
    *VERBOSITY.lock().unwrap()
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

    // For objects, show first-level fields
    if let OvsmValue::Object(obj) = value {
        for (key, val) in obj.iter().take(5) {
            let val_type = match val {
                OvsmValue::Array(arr) => format!("array[{}]", arr.len()),
                OvsmValue::Object(_) => "object".to_string(),
                OvsmValue::String(s) => format!("String({:?})", s.chars().take(50).collect::<String>()),
                _ => format!("{:?}", val).chars().take(50).collect(),
            };
            eprintln!("    .{} = {}", key, val_type);
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
                                eprintln!("      [{}]: object with keys: {}", i, 
                                    o.keys().cloned().collect::<Vec<_>>().join(", "));
                                // Print the actual error message fields
                                if let Some(OvsmValue::String(text)) = o.get("text") {
                                    eprintln!("          📄 text: {}", text.chars().take(500).collect::<String>());
                                    if text.len() > 500 {
                                        eprintln!("          ... ({} more chars)", text.len() - 500);
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
                    eprintln!("    Object with keys: {}", 
                        o.keys().cloned().collect::<Vec<_>>().join(", "));
                }
                _ => eprintln!("    {:?}", content),
            }
        }
        
        // If there's an isError field set to true, highlight it
        if let Some(OvsmValue::Bool(true)) = obj.get("isError") {
            eprintln!("⚠️  isError=true detected - this is an error response!");
        }
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
