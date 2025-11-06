//! Extended bit operations for OVSM
//!
//! Bit arrays, bit vectors, and bit field operations.
//! Completes the Common Lisp bit manipulation suite.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// Extended bit operations (8 total)

// ============================================================
// BIT ARRAYS
// ============================================================

/// MAKE-BIT-ARRAY - Create bit array
pub struct MakeBitArrayTool;
impl Tool for MakeBitArrayTool {
    fn name(&self) -> &str {
        "MAKE-BIT-ARRAY"
    }
    fn description(&self) -> &str {
        "Create bit array of specified size"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MAKE-BIT-ARRAY".to_string(),
                reason: "Expected 1 argument: size".to_string(),
            });
        }

        let size = match args.first() {
            Some(Value::Int(n)) if *n >= 0 => *n as usize,
            Some(Value::Int(n)) => {
                return Err(Error::InvalidArguments {
                    tool: "MAKE-BIT-ARRAY".to_string(),
                    reason: format!("Size must be non-negative, got {}", n),
                });
            }
            _ => {
                return Err(Error::TypeError {
                    expected: "integer".to_string(),
                    got: args[0].type_name(),
                });
            }
        };

        let bits = vec![Value::Int(0); size];
        Ok(Value::Array(Arc::new(bits)))
    }
}

/// BIT - Access bit in bit array
pub struct BitTool;
impl Tool for BitTool {
    fn name(&self) -> &str {
        "BIT"
    }
    fn description(&self) -> &str {
        "Access bit at index in bit array"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Int(0));
        }

        let index = match &args[1] {
            Value::Int(n) => *n as usize,
            _ => return Ok(Value::Int(0)),
        };

        match &args[0] {
            Value::Array(arr) => Ok(arr.get(index).cloned().unwrap_or(Value::Int(0))),
            _ => Ok(Value::Int(0)),
        }
    }
}

/// SBIT - Access simple bit in simple bit array
pub struct SbitTool;
impl Tool for SbitTool {
    fn name(&self) -> &str {
        "SBIT"
    }
    fn description(&self) -> &str {
        "Access bit in simple bit array"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Int(0));
        }

        let index = match &args[1] {
            Value::Int(n) => *n as usize,
            _ => return Ok(Value::Int(0)),
        };

        match &args[0] {
            Value::Array(arr) => Ok(arr.get(index).cloned().unwrap_or(Value::Int(0))),
            _ => Ok(Value::Int(0)),
        }
    }
}

/// BIT-AND - Bitwise AND on bit arrays
pub struct BitAndTool;
impl Tool for BitAndTool {
    fn name(&self) -> &str {
        "BIT-AND"
    }
    fn description(&self) -> &str {
        "Bitwise AND on bit arrays"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Array(Arc::new(vec![])));
        }

        match (&args[0], &args[1]) {
            (Value::Array(arr1), Value::Array(arr2)) => {
                let result: Vec<Value> = arr1
                    .iter()
                    .zip(arr2.iter())
                    .map(|(a, b)| match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x & y),
                        _ => Value::Int(0),
                    })
                    .collect();
                Ok(Value::Array(Arc::new(result)))
            }
            _ => Ok(Value::Array(Arc::new(vec![]))),
        }
    }
}

/// BIT-IOR - Bitwise OR on bit arrays
pub struct BitIorTool;
impl Tool for BitIorTool {
    fn name(&self) -> &str {
        "BIT-IOR"
    }
    fn description(&self) -> &str {
        "Bitwise inclusive OR on bit arrays"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Array(Arc::new(vec![])));
        }

        match (&args[0], &args[1]) {
            (Value::Array(arr1), Value::Array(arr2)) => {
                let result: Vec<Value> = arr1
                    .iter()
                    .zip(arr2.iter())
                    .map(|(a, b)| match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x | y),
                        _ => Value::Int(0),
                    })
                    .collect();
                Ok(Value::Array(Arc::new(result)))
            }
            _ => Ok(Value::Array(Arc::new(vec![]))),
        }
    }
}

/// BIT-XOR - Bitwise XOR on bit arrays
pub struct BitXorTool;
impl Tool for BitXorTool {
    fn name(&self) -> &str {
        "BIT-XOR"
    }
    fn description(&self) -> &str {
        "Bitwise exclusive OR on bit arrays"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Array(Arc::new(vec![])));
        }

        match (&args[0], &args[1]) {
            (Value::Array(arr1), Value::Array(arr2)) => {
                let result: Vec<Value> = arr1
                    .iter()
                    .zip(arr2.iter())
                    .map(|(a, b)| match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x ^ y),
                        _ => Value::Int(0),
                    })
                    .collect();
                Ok(Value::Array(Arc::new(result)))
            }
            _ => Ok(Value::Array(Arc::new(vec![]))),
        }
    }
}

/// BIT-NOT - Bitwise NOT on bit array
pub struct BitNotTool;
impl Tool for BitNotTool {
    fn name(&self) -> &str {
        "BIT-NOT"
    }
    fn description(&self) -> &str {
        "Bitwise NOT on bit array"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }

        match &args[0] {
            Value::Array(arr) => {
                let result: Vec<Value> = arr
                    .iter()
                    .map(|v| match v {
                        Value::Int(0) => Value::Int(1),
                        Value::Int(1) => Value::Int(0),
                        Value::Int(n) => Value::Int(!n),
                        _ => Value::Int(0),
                    })
                    .collect();
                Ok(Value::Array(Arc::new(result)))
            }
            _ => Ok(Value::Array(Arc::new(vec![]))),
        }
    }
}

/// BIT-VECTOR-P - Check if bit vector
pub struct BitVectorPTool;
impl Tool for BitVectorPTool {
    fn name(&self) -> &str {
        "BIT-VECTOR-P"
    }
    fn description(&self) -> &str {
        "Check if object is bit vector"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        match args.first() {
            Some(Value::Array(arr)) => {
                let is_bit_vector = arr
                    .iter()
                    .all(|v| matches!(v, Value::Int(0) | Value::Int(1)));
                Ok(Value::Bool(is_bit_vector))
            }
            _ => Ok(Value::Bool(false)),
        }
    }
}

/// Register all extended bit operations
pub fn register(registry: &mut ToolRegistry) {
    // Bit arrays
    registry.register(MakeBitArrayTool);
    registry.register(BitTool);
    registry.register(SbitTool);

    // Bitwise operations
    registry.register(BitAndTool);
    registry.register(BitIorTool);
    registry.register(BitXorTool);
    registry.register(BitNotTool);

    // Bit vector predicates
    registry.register(BitVectorPTool);
}
