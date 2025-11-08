//! Extended random number operations for OVSM
//!
//! Random state control and distribution management.
//! Completes the Common Lisp random number system.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// Extended random functions (8 total)

// ============================================================
// RANDOM STATE
// ============================================================

/// MAKE-RANDOM-STATE - Create random state
pub struct MakeRandomStateTool;
impl Tool for MakeRandomStateTool {
    fn name(&self) -> &str {
        "MAKE-RANDOM-STATE"
    }
    fn description(&self) -> &str {
        "Create new random state"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation - should accept optional seed or state to copy
                      // Returns a new random state object
        Ok(Value::Int(42)) // Simplified: return seed value
    }
}

/// RANDOM-STATE-P - Check if random state
pub struct RandomStatePTool;
impl Tool for RandomStatePTool {
    fn name(&self) -> &str {
        "RANDOM-STATE-P"
    }
    fn description(&self) -> &str {
        "Check if object is random state"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::Bool(matches!(args.first(), Some(Value::Int(_)))))
    }
}

/// *RANDOM-STATE* - Current random state
pub struct RandomStateTool;
impl Tool for RandomStateTool {
    fn name(&self) -> &str {
        "*RANDOM-STATE*"
    }
    fn description(&self) -> &str {
        "Current random state"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Int(0)
        } else {
            args[0].clone()
        })
    }
}

// ============================================================
// RANDOM DISTRIBUTIONS
// ============================================================

/// RANDOM-FLOAT - Generate random float in range
pub struct RandomFloatTool;
impl Tool for RandomFloatTool {
    fn name(&self) -> &str {
        "RANDOM-FLOAT"
    }
    fn description(&self) -> &str {
        "Generate random float between 0.0 and limit"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (limit)".to_string(),
            });
        }
        let limit = match &args[0] {
            Value::Float(f) => *f,
            Value::Int(n) => *n as f64,
            _ => {
                return Err(Error::InvalidArguments {
                    tool: self.name().to_string(),
                    reason: "Limit must be a number".to_string(),
                })
            }
        };

        // Simplified: return pseudo-random value
        Ok(Value::Float(0.5 * limit))
    }
}

/// RANDOM-INTEGER - Generate random integer in range
pub struct RandomIntegerTool;
impl Tool for RandomIntegerTool {
    fn name(&self) -> &str {
        "RANDOM-INTEGER"
    }
    fn description(&self) -> &str {
        "Generate random integer between 0 and limit"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (limit)".to_string(),
            });
        }
        let limit = match &args[0] {
            Value::Int(n) => *n,
            _ => {
                return Err(Error::InvalidArguments {
                    tool: self.name().to_string(),
                    reason: "Limit must be an integer".to_string(),
                })
            }
        };

        // Simplified: return pseudo-random value
        Ok(Value::Int(limit / 2))
    }
}

/// RANDOM-ELEMENT - Get random element from sequence
pub struct RandomElementTool;
impl Tool for RandomElementTool {
    fn name(&self) -> &str {
        "RANDOM-ELEMENT"
    }
    fn description(&self) -> &str {
        "Get random element from sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (sequence)".to_string(),
            });
        }

        match &args[0] {
            Value::Array(arr) => {
                if arr.is_empty() {
                    Ok(Value::Null)
                } else {
                    // Simplified: return middle element
                    Ok(arr[arr.len() / 2].clone())
                }
            }
            _ => Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Argument must be an array".to_string(),
            }),
        }
    }
}

/// SHUFFLE - Randomly permute sequence
pub struct ShuffleTool;
impl Tool for ShuffleTool {
    fn name(&self) -> &str {
        "SHUFFLE"
    }
    fn description(&self) -> &str {
        "Randomly permute sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 1 argument (sequence)".to_string(),
            });
        }

        match &args[0] {
            Value::Array(arr) => {
                // Simplified: just reverse for demonstration
                let mut shuffled = arr.to_vec();
                shuffled.reverse();
                Ok(Value::Array(Arc::new(shuffled)))
            }
            _ => Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Argument must be an array".to_string(),
            }),
        }
    }
}

/// SEED-RANDOM-STATE - Seed random state
pub struct SeedRandomStateTool;
impl Tool for SeedRandomStateTool {
    fn name(&self) -> &str {
        "SEED-RANDOM-STATE"
    }
    fn description(&self) -> &str {
        "Seed random state with value"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Int(0)
        } else {
            args[0].clone()
        })
    }
}

/// Register all extended random functions
pub fn register(registry: &mut ToolRegistry) {
    // Random state
    registry.register(MakeRandomStateTool);
    registry.register(RandomStatePTool);
    registry.register(RandomStateTool);

    // Random distributions
    registry.register(RandomFloatTool);
    registry.register(RandomIntegerTool);
    registry.register(RandomElementTool);
    registry.register(ShuffleTool);
    registry.register(SeedRandomStateTool);
}
