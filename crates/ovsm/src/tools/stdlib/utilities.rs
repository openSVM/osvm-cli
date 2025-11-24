//! Utility tools

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

/// Register utility tools
pub fn register(registry: &mut ToolRegistry) {
    registry.register(LogTool);
    registry.register(ErrorTool);
    registry.register(NowTool);
    registry.register(TypeOfTool);
    registry.register(KeysTool);
    registry.register(IsArrayTool);
    registry.register(IsObjectTool);
    registry.register(IsStringTool);
    registry.register(IsNumberTool);
    registry.register(IsBoolTool);
    registry.register(IsNullTool);
}

/// Tool for logging messages to stdout (debugging purposes)
///
/// Usage: `LOG(message, ...)` - accepts multiple arguments
/// Example: `LOG("Value:", $x)` prints `[LOG] Value: 42`
/// Note: Returns null and does not affect program flow
pub struct LogTool;

impl Tool for LogTool {
    fn name(&self) -> &str {
        "LOG"
    }

    fn description(&self) -> &str {
        "Log a message (for debugging)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        for arg in args {
            println!("[LOG] {}", arg);
        }
        Ok(Value::Null)
    }
}

/// Tool for raising user-defined errors with custom messages
///
/// Usage: `ERROR(message)` - raises a UserError with the given message
/// Example: `ERROR("Invalid input")` stops execution with error
/// Note: This terminates program execution immediately
pub struct ErrorTool;

impl Tool for ErrorTool {
    fn name(&self) -> &str {
        "ERROR"
    }

    fn description(&self) -> &str {
        "Raise an error with message"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        let message = if args.is_empty() {
            "User error".to_string()
        } else {
            args[0].to_string_value()
        };

        Err(Error::UserError(message))
    }
}

/// Tool for getting current Unix timestamp
///
/// Usage: `NOW()` - returns current Unix timestamp in seconds
/// Example: `$timestamp = NOW()` returns 1697123456
/// Note: Returns integer value representing seconds since Unix epoch
pub struct NowTool;

impl Tool for NowTool {
    fn name(&self) -> &str {
        "NOW"
    }

    fn description(&self) -> &str {
        "Get current Unix timestamp in seconds"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        use std::time::{SystemTime, UNIX_EPOCH};
        let now = SystemTime::now().duration_since(UNIX_EPOCH).map_err(|e| {
            Error::ToolExecutionError {
                tool: "NOW".to_string(),
                reason: format!("Failed to get system time: {}", e),
            }
        })?;
        Ok(Value::Int(now.as_secs() as i64))
    }
}

/// Tool for type introspection - returns the type of a value as a string
///
/// Usage: `TYPEOF(value)` - returns type as string
/// Example: `TYPEOF([1,2,3])` returns "array"
/// Example: `TYPEOF({:key "value"})` returns "object"
pub struct TypeOfTool;

impl Tool for TypeOfTool {
    fn name(&self) -> &str {
        "TYPEOF"
    }

    fn description(&self) -> &str {
        "Returns the type of a value as a string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::String("null".to_string()));
        }

        let type_str = match &args[0] {
            Value::Null => "null",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Object(_) => "object",
            Value::Function { .. } => "function",
            Value::Range { .. } => "range",
            Value::Multiple(_) => "multiple",
            Value::Macro { .. } => "macro",
            Value::AsyncHandle { .. } => "async-handle",
        };

        Ok(Value::String(type_str.to_string()))
    }
}

/// Tool for getting object keys - returns the keys of an object as an array
///
/// Usage: `KEYS(object)` - returns array of keys
/// Example: `KEYS({:a 1 :b 2})` returns ["a", "b"]
pub struct KeysTool;

impl Tool for KeysTool {
    fn name(&self) -> &str {
        "KEYS"
    }

    fn description(&self) -> &str {
        "Returns the keys of an object as an array of strings"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }

        match &args[0] {
            Value::Object(obj) => {
                let keys: Vec<Value> = obj.keys().map(|k| Value::String(k.clone())).collect();
                Ok(Value::Array(Arc::new(keys)))
            }
            _ => Ok(Value::Array(Arc::new(vec![]))),
        }
    }
}

/// Type predicate: is-array?
pub struct IsArrayTool;

impl Tool for IsArrayTool {
    fn name(&self) -> &str {
        "IS-ARRAY?"
    }

    fn description(&self) -> &str {
        "Check if value is an array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Array(_))))
    }
}

/// Type predicate: is-object?
pub struct IsObjectTool;

impl Tool for IsObjectTool {
    fn name(&self) -> &str {
        "IS-OBJECT?"
    }

    fn description(&self) -> &str {
        "Check if value is an object"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Object(_))))
    }
}

/// Type predicate: is-string?
pub struct IsStringTool;

impl Tool for IsStringTool {
    fn name(&self) -> &str {
        "IS-STRING?"
    }

    fn description(&self) -> &str {
        "Check if value is a string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::String(_))))
    }
}

/// Type predicate: is-number?
pub struct IsNumberTool;

impl Tool for IsNumberTool {
    fn name(&self) -> &str {
        "IS-NUMBER?"
    }

    fn description(&self) -> &str {
        "Check if value is a number (int or float)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(
            &args[0],
            Value::Int(_) | Value::Float(_)
        )))
    }
}

/// Type predicate: is-bool?
pub struct IsBoolTool;

impl Tool for IsBoolTool {
    fn name(&self) -> &str {
        "IS-BOOL?"
    }

    fn description(&self) -> &str {
        "Check if value is a boolean"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Bool(_))))
    }
}

/// Type predicate: is-null?
pub struct IsNullTool;

impl Tool for IsNullTool {
    fn name(&self) -> &str {
        "IS-NULL?"
    }

    fn description(&self) -> &str {
        "Check if value is null"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(true));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Null)))
    }
}
