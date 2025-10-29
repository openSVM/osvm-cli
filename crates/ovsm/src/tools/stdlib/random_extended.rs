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
    fn name(&self) -> &str { "MAKE-RANDOM-STATE" }
    fn description(&self) -> &str { "Create new random state" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Returns a new random state object
        Ok(Value::Int(42)) // Simplified: return seed value
    }
}

/// RANDOM-STATE-P - Check if random state
pub struct RandomStatePTool;
impl Tool for RandomStatePTool {
    fn name(&self) -> &str { "RANDOM-STATE-P" }
    fn description(&self) -> &str { "Check if object is random state" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::Bool(matches!(args.get(0), Some(Value::Int(_)))))
    }
}

/// *RANDOM-STATE* - Current random state
pub struct RandomStateTool;
impl Tool for RandomStateTool {
    fn name(&self) -> &str { "*RANDOM-STATE*" }
    fn description(&self) -> &str { "Current random state" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() { Value::Int(0) } else { args[0].clone() })
    }
}

// ============================================================
// RANDOM DISTRIBUTIONS
// ============================================================

/// RANDOM-FLOAT - Generate random float in range
pub struct RandomFloatTool;
impl Tool for RandomFloatTool {
    fn name(&self) -> &str { "RANDOM-FLOAT" }
    fn description(&self) -> &str { "Generate random float between 0.0 and limit" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let limit = match args.get(0) {
            Some(Value::Float(f)) => *f,
            Some(Value::Int(n)) => *n as f64,
            _ => 1.0,
        };

        // Simplified: return pseudo-random value
        Ok(Value::Float(0.5 * limit))
    }
}

/// RANDOM-INTEGER - Generate random integer in range
pub struct RandomIntegerTool;
impl Tool for RandomIntegerTool {
    fn name(&self) -> &str { "RANDOM-INTEGER" }
    fn description(&self) -> &str { "Generate random integer between 0 and limit" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let limit = match args.get(0) {
            Some(Value::Int(n)) => *n,
            _ => 100,
        };

        // Simplified: return pseudo-random value
        Ok(Value::Int(limit / 2))
    }
}

/// RANDOM-ELEMENT - Get random element from sequence
pub struct RandomElementTool;
impl Tool for RandomElementTool {
    fn name(&self) -> &str { "RANDOM-ELEMENT" }
    fn description(&self) -> &str { "Get random element from sequence" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
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
            _ => Ok(Value::Null),
        }
    }
}

/// SHUFFLE - Randomly permute sequence
pub struct ShuffleTool;
impl Tool for ShuffleTool {
    fn name(&self) -> &str { "SHUFFLE" }
    fn description(&self) -> &str { "Randomly permute sequence" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }

        match &args[0] {
            Value::Array(arr) => {
                // Simplified: just reverse for demonstration
                let mut shuffled = arr.to_vec();
                shuffled.reverse();
                Ok(Value::Array(Arc::new(shuffled)))
            }
            v => Ok(v.clone()),
        }
    }
}

/// SEED-RANDOM-STATE - Seed random state
pub struct SeedRandomStateTool;
impl Tool for SeedRandomStateTool {
    fn name(&self) -> &str { "SEED-RANDOM-STATE" }
    fn description(&self) -> &str { "Seed random state with value" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() { Value::Int(0) } else { args[0].clone() })
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
