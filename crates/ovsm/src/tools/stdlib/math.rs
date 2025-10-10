//! Math tools

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

/// Register math tools
pub fn register(registry: &mut ToolRegistry) {
    registry.register(AbsTool);
    registry.register(SqrtTool);
    registry.register(PowTool);
    registry.register(RoundTool);
    registry.register(FloorTool);
    registry.register(CeilTool);
}

pub struct AbsTool;

impl Tool for AbsTool {
    fn name(&self) -> &str {
        "ABS"
    }

    fn description(&self) -> &str {
        "Absolute value"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ABS".to_string(),
                reason: "Expected number argument".to_string(),
            });
        }

        match &args[0] {
            Value::Int(n) => Ok(Value::Int(n.abs())),
            Value::Float(f) => Ok(Value::Float(f.abs())),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

pub struct SqrtTool;

impl Tool for SqrtTool {
    fn name(&self) -> &str {
        "SQRT"
    }

    fn description(&self) -> &str {
        "Square root"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "SQRT".to_string(),
                reason: "Expected number argument".to_string(),
            });
        }

        let val = args[0].as_float()?;
        Ok(Value::Float(val.sqrt()))
    }
}

pub struct PowTool;

impl Tool for PowTool {
    fn name(&self) -> &str {
        "POW"
    }

    fn description(&self) -> &str {
        "Power (base^exponent)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "POW".to_string(),
                reason: "Expected base and exponent".to_string(),
            });
        }

        let base = args[0].as_float()?;
        let exp = args[1].as_float()?;
        Ok(Value::Float(base.powf(exp)))
    }
}

pub struct RoundTool;

impl Tool for RoundTool {
    fn name(&self) -> &str {
        "ROUND"
    }

    fn description(&self) -> &str {
        "Round to nearest integer"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ROUND".to_string(),
                reason: "Expected number argument".to_string(),
            });
        }

        let val = args[0].as_float()?;
        Ok(Value::Int(val.round() as i64))
    }
}

pub struct FloorTool;

impl Tool for FloorTool {
    fn name(&self) -> &str {
        "FLOOR"
    }

    fn description(&self) -> &str {
        "Round down to integer"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FLOOR".to_string(),
                reason: "Expected number argument".to_string(),
            });
        }

        let val = args[0].as_float()?;
        Ok(Value::Int(val.floor() as i64))
    }
}

pub struct CeilTool;

impl Tool for CeilTool {
    fn name(&self) -> &str {
        "CEIL"
    }

    fn description(&self) -> &str {
        "Round up to integer"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CEIL".to_string(),
                reason: "Expected number argument".to_string(),
            });
        }

        let val = args[0].as_float()?;
        Ok(Value::Int(val.ceil() as i64))
    }
}
