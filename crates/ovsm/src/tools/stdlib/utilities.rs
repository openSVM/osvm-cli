//! Utility tools

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

/// Register utility tools
pub fn register(registry: &mut ToolRegistry) {
    registry.register(LogTool);
    registry.register(ErrorTool);
}

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
