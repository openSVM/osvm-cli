//! Utility tools

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

/// Register utility tools
pub fn register(registry: &mut ToolRegistry) {
    registry.register(LogTool);
    registry.register(ErrorTool);
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
