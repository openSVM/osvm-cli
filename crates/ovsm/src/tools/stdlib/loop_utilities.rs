//! Loop utility functions for OVSM
//!
//! Simplified loop utilities instead of full LOOP macro implementation.
//! Provides functional programming alternatives to complex loop constructs.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// ============================================================================
// LOOP UTILITIES (8 functions)
// ============================================================================

/// LOOP-COLLECT - Collect values from iteration
pub struct LoopCollectTool;

impl Tool for LoopCollectTool {
    fn name(&self) -> &str {
        "LOOP-COLLECT"
    }

    fn description(&self) -> &str {
        "Collect values into a list (same as MAPCAR)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Returns array as-is (collection)
        if args.is_empty() {
            Ok(Value::Array(Arc::new(vec![])))
        } else {
            // If array, return it; otherwise wrap in array
            match &args[0] {
                Value::Array(_) => Ok(args[0].clone()),
                _ => Ok(Value::Array(Arc::new(args.to_vec()))),
            }
        }
    }
}

/// LOOP-APPEND - Append lists during iteration
pub struct LoopAppendTool;

impl Tool for LoopAppendTool {
    fn name(&self) -> &str {
        "LOOP-APPEND"
    }

    fn description(&self) -> &str {
        "Append multiple lists"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        let mut result = vec![];

        for arg in args {
            match arg {
                Value::Array(arr) => {
                    result.extend(arr.iter().cloned());
                }
                _ => result.push(arg.clone()),
            }
        }

        Ok(Value::Array(Arc::new(result)))
    }
}

/// LOOP-COUNT - Count elements satisfying predicate
pub struct LoopCountTool;

impl Tool for LoopCountTool {
    fn name(&self) -> &str {
        "LOOP-COUNT"
    }

    fn description(&self) -> &str {
        "Count truthy values in array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }

        let arr = args[0].as_array()?;
        let count = arr
            .iter()
            .filter(|v| match v {
                Value::Bool(true) => true,
                Value::Int(n) if *n != 0 => true,
                _ => false,
            })
            .count();

        Ok(Value::Int(count as i64))
    }
}

/// LOOP-SUM - Sum numeric values
pub struct LoopSumTool;

impl Tool for LoopSumTool {
    fn name(&self) -> &str {
        "LOOP-SUM"
    }

    fn description(&self) -> &str {
        "Sum all numeric values in array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }

        let arr = args[0].as_array()?;
        let mut sum = 0i64;
        let mut has_float = false;
        let mut float_sum = 0.0f64;

        for val in arr.iter() {
            match val {
                Value::Int(n) => {
                    sum += n;
                    float_sum += *n as f64;
                }
                Value::Float(f) => {
                    has_float = true;
                    float_sum += f;
                }
                _ => {}
            }
        }

        if has_float {
            Ok(Value::Float(float_sum))
        } else {
            Ok(Value::Int(sum))
        }
    }
}

/// LOOP-MAXIMIZE - Find maximum value
pub struct LoopMaximizeTool;

impl Tool for LoopMaximizeTool {
    fn name(&self) -> &str {
        "LOOP-MAXIMIZE"
    }

    fn description(&self) -> &str {
        "Find maximum value in array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
        }

        let arr = args[0].as_array()?;
        if arr.is_empty() {
            return Ok(Value::Null);
        }

        let mut max_int = i64::MIN;
        let mut max_float = f64::MIN;
        let mut has_float = false;
        let mut found_any = false;

        for val in arr.iter() {
            match val {
                Value::Int(n) => {
                    found_any = true;
                    if *n > max_int {
                        max_int = *n;
                    }
                    if (*n as f64) > max_float {
                        max_float = *n as f64;
                    }
                }
                Value::Float(f) => {
                    found_any = true;
                    has_float = true;
                    if *f > max_float {
                        max_float = *f;
                    }
                }
                _ => {}
            }
        }

        if !found_any {
            return Ok(Value::Null);
        }

        if has_float {
            Ok(Value::Float(max_float))
        } else {
            Ok(Value::Int(max_int))
        }
    }
}

/// LOOP-MINIMIZE - Find minimum value
pub struct LoopMinimizeTool;

impl Tool for LoopMinimizeTool {
    fn name(&self) -> &str {
        "LOOP-MINIMIZE"
    }

    fn description(&self) -> &str {
        "Find minimum value in array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
        }

        let arr = args[0].as_array()?;
        if arr.is_empty() {
            return Ok(Value::Null);
        }

        let mut min_int = i64::MAX;
        let mut min_float = f64::MAX;
        let mut has_float = false;
        let mut found_any = false;

        for val in arr.iter() {
            match val {
                Value::Int(n) => {
                    found_any = true;
                    if *n < min_int {
                        min_int = *n;
                    }
                    if (*n as f64) < min_float {
                        min_float = *n as f64;
                    }
                }
                Value::Float(f) => {
                    found_any = true;
                    has_float = true;
                    if *f < min_float {
                        min_float = *f;
                    }
                }
                _ => {}
            }
        }

        if !found_any {
            return Ok(Value::Null);
        }

        if has_float {
            Ok(Value::Float(min_float))
        } else {
            Ok(Value::Int(min_int))
        }
    }
}

/// LOOP-REPEAT - Repeat value N times
pub struct LoopRepeatTool;

impl Tool for LoopRepeatTool {
    fn name(&self) -> &str {
        "LOOP-REPEAT"
    }

    fn description(&self) -> &str {
        "Repeat value N times, return array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "LOOP-REPEAT".to_string(),
                reason: "Expected value and count arguments".to_string(),
            });
        }

        let value = &args[0];
        let count = args[1].as_int()? as usize;

        let result = vec![value.clone(); count];
        Ok(Value::Array(Arc::new(result)))
    }
}

/// LOOP-WHILE - Execute while condition is true (returns array of values)
pub struct LoopWhileTool;

impl Tool for LoopWhileTool {
    fn name(&self) -> &str {
        "LOOP-WHILE"
    }

    fn description(&self) -> &str {
        "Helper for while loops (returns input array)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Simplified: just returns the input as array
        if args.is_empty() {
            Ok(Value::Array(Arc::new(vec![])))
        } else {
            match &args[0] {
                Value::Array(_) => Ok(args[0].clone()),
                _ => Ok(Value::Array(Arc::new(args.to_vec()))),
            }
        }
    }
}

// ============================================================================
// REGISTRATION
// ============================================================================

/// Register all loop utility tools with the tool registry
///
/// This function registers all LOOP clause accumulation and iteration utilities.
pub fn register(registry: &mut ToolRegistry) {
    registry.register(LoopCollectTool);
    registry.register(LoopAppendTool);
    registry.register(LoopCountTool);
    registry.register(LoopSumTool);
    registry.register(LoopMaximizeTool);
    registry.register(LoopMinimizeTool);
    registry.register(LoopRepeatTool);
    registry.register(LoopWhileTool);
}
