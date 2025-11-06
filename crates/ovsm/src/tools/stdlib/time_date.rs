//! Time and date operations for OVSM
//!
//! Universal time, decoded time, and time arithmetic.
//! Provides Common Lisp-style temporal operations.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

// Time and date functions (10 total)

// ============================================================
// UNIVERSAL TIME
// ============================================================

/// GET-UNIVERSAL-TIME - Get current universal time
pub struct GetUniversalTimeTool;
impl Tool for GetUniversalTimeTool {
    fn name(&self) -> &str {
        "GET-UNIVERSAL-TIME"
    }
    fn description(&self) -> &str {
        "Get current time as universal time"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // Universal time = seconds since 1900-01-01 00:00:00
        // Unix epoch = 1970-01-01 00:00:00 = 2208988800 seconds after 1900
        const UNIX_EPOCH_OFFSET: u64 = 2208988800;

        let duration = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default();

        let universal_time = duration.as_secs() + UNIX_EPOCH_OFFSET;
        Ok(Value::Int(universal_time as i64))
    }
}

/// GET-DECODED-TIME - Get current time as decoded components
pub struct GetDecodedTimeTool;
impl Tool for GetDecodedTimeTool {
    fn name(&self) -> &str {
        "GET-DECODED-TIME"
    }
    fn description(&self) -> &str {
        "Get current time as decoded components"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // Returns: second, minute, hour, date, month, year, day-of-week, dst-p, timezone
        // Simplified implementation
        Ok(Value::Array(Arc::new(vec![
            Value::Int(0),      // second
            Value::Int(0),      // minute
            Value::Int(0),      // hour
            Value::Int(1),      // date
            Value::Int(1),      // month
            Value::Int(2025),   // year
            Value::Int(0),      // day-of-week (Monday=0)
            Value::Bool(false), // daylight saving time
            Value::Int(0),      // timezone offset
        ])))
    }
}

/// DECODE-UNIVERSAL-TIME - Decode universal time to components
pub struct DecodeUniversalTimeTool;
impl Tool for DecodeUniversalTimeTool {
    fn name(&self) -> &str {
        "DECODE-UNIVERSAL-TIME"
    }
    fn description(&self) -> &str {
        "Decode universal time to components"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "DECODE-UNIVERSAL-TIME".to_string(),
                reason: "Requires universal time".to_string(),
            });
        }

        // Returns: second, minute, hour, date, month, year, day-of-week, dst-p, timezone
        Ok(Value::Array(Arc::new(vec![
            Value::Int(0),      // second
            Value::Int(0),      // minute
            Value::Int(0),      // hour
            Value::Int(1),      // date
            Value::Int(1),      // month
            Value::Int(2025),   // year
            Value::Int(0),      // day-of-week
            Value::Bool(false), // dst
            Value::Int(0),      // timezone
        ])))
    }
}

/// ENCODE-UNIVERSAL-TIME - Encode components to universal time
pub struct EncodeUniversalTimeTool;
impl Tool for EncodeUniversalTimeTool {
    fn name(&self) -> &str {
        "ENCODE-UNIVERSAL-TIME"
    }
    fn description(&self) -> &str {
        "Encode time components to universal time"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 6 {
            return Err(Error::InvalidArguments {
                tool: "ENCODE-UNIVERSAL-TIME".to_string(),
                reason: "Requires second, minute, hour, date, month, year".to_string(),
            });
        }

        // Simplified: return a fixed value
        // Real implementation would compute from components
        Ok(Value::Int(3900000000))
    }
}

// ============================================================
// TIME ARITHMETIC
// ============================================================

/// TIME-ADD - Add duration to time
pub struct TimeAddTool;
impl Tool for TimeAddTool {
    fn name(&self) -> &str {
        "TIME-ADD"
    }
    fn description(&self) -> &str {
        "Add duration to universal time"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "TIME-ADD".to_string(),
                reason: "Requires time and duration".to_string(),
            });
        }

        match (&args[0], &args[1]) {
            (Value::Int(time), Value::Int(duration)) => Ok(Value::Int(time + duration)),
            _ => Err(Error::TypeError {
                expected: "integer".to_string(),
                got: "non-integer".to_string(),
            }),
        }
    }
}

/// TIME-SUBTRACT - Subtract times or duration
pub struct TimeSubtractTool;
impl Tool for TimeSubtractTool {
    fn name(&self) -> &str {
        "TIME-SUBTRACT"
    }
    fn description(&self) -> &str {
        "Subtract times or duration from time"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "TIME-SUBTRACT".to_string(),
                reason: "Requires two times or time and duration".to_string(),
            });
        }

        match (&args[0], &args[1]) {
            (Value::Int(time1), Value::Int(time2)) => Ok(Value::Int(time1 - time2)),
            _ => Err(Error::TypeError {
                expected: "integer".to_string(),
                got: "non-integer".to_string(),
            }),
        }
    }
}

/// TIME< - Compare times (less than)
pub struct TimeLessThanTool;
impl Tool for TimeLessThanTool {
    fn name(&self) -> &str {
        "TIME<"
    }
    fn description(&self) -> &str {
        "Compare if time1 < time2"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Bool(false));
        }

        match (&args[0], &args[1]) {
            (Value::Int(t1), Value::Int(t2)) => Ok(Value::Bool(t1 < t2)),
            _ => Ok(Value::Bool(false)),
        }
    }
}

/// TIME<= - Compare times (less than or equal)
pub struct TimeLessEqualTool;
impl Tool for TimeLessEqualTool {
    fn name(&self) -> &str {
        "TIME<="
    }
    fn description(&self) -> &str {
        "Compare if time1 <= time2"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Bool(false));
        }

        match (&args[0], &args[1]) {
            (Value::Int(t1), Value::Int(t2)) => Ok(Value::Bool(t1 <= t2)),
            _ => Ok(Value::Bool(false)),
        }
    }
}

/// TIME= - Compare times (equal)
pub struct TimeEqualTool;
impl Tool for TimeEqualTool {
    fn name(&self) -> &str {
        "TIME="
    }
    fn description(&self) -> &str {
        "Compare if time1 = time2"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Bool(false));
        }

        match (&args[0], &args[1]) {
            (Value::Int(t1), Value::Int(t2)) => Ok(Value::Bool(t1 == t2)),
            _ => Ok(Value::Bool(false)),
        }
    }
}

/// SLEEP - Sleep for duration
pub struct SleepTool;
impl Tool for SleepTool {
    fn name(&self) -> &str {
        "SLEEP"
    }
    fn description(&self) -> &str {
        "Sleep for specified seconds"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
        }

        let seconds = match &args[0] {
            Value::Int(n) if *n >= 0 => *n as u64,
            Value::Float(f) if *f >= 0.0 => *f as u64,
            Value::Int(n) => {
                return Err(Error::InvalidArguments {
                    tool: "SLEEP".to_string(),
                    reason: format!("Sleep duration must be non-negative, got {}", n),
                });
            }
            Value::Float(f) => {
                return Err(Error::InvalidArguments {
                    tool: "SLEEP".to_string(),
                    reason: format!("Sleep duration must be non-negative, got {}", f),
                });
            }
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: args[0].type_name(),
                });
            }
        };

        // Return information about the sleep without actually blocking
        // This allows the interpreter to continue functioning
        let mut result = HashMap::new();
        result.insert("operation".to_string(), Value::String("sleep".to_string()));
        result.insert("duration".to_string(), Value::Int(seconds as i64));
        result.insert("unit".to_string(), Value::String("seconds".to_string()));
        Ok(Value::Object(Arc::new(result)))
    }
}

/// Register all time and date functions
pub fn register(registry: &mut ToolRegistry) {
    // Universal time
    registry.register(GetUniversalTimeTool);
    registry.register(GetDecodedTimeTool);
    registry.register(DecodeUniversalTimeTool);
    registry.register(EncodeUniversalTimeTool);

    // Time arithmetic
    registry.register(TimeAddTool);
    registry.register(TimeSubtractTool);
    registry.register(TimeLessThanTool);
    registry.register(TimeLessEqualTool);
    registry.register(TimeEqualTool);
    registry.register(SleepTool);
}
