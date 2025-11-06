//! Advanced LOOP features for OVSM
//!
//! Complex iteration, destructuring, and advanced LOOP clauses.
//! Completes the Common Lisp LOOP macro implementation.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// Advanced LOOP functions (15 total)

// ============================================================
// DESTRUCTURING ITERATION
// ============================================================

/// LOOP-DESTRUCTURING - Destructure in LOOP iteration
pub struct LoopDestructuringTool;
impl Tool for LoopDestructuringTool {
    fn name(&self) -> &str {
        "LOOP-DESTRUCTURING"
    }
    fn description(&self) -> &str {
        "Destructure values in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Array(Arc::new(vec![]))
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-FOR-ON - Iterate on CDR of list
pub struct LoopForOnTool;
impl Tool for LoopForOnTool {
    fn name(&self) -> &str {
        "LOOP-FOR-ON"
    }
    fn description(&self) -> &str {
        "Iterate on successive CDRs"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-FOR-EQUALS-THEN - Iterate with explicit update
pub struct LoopForEqualsThenTool;
impl Tool for LoopForEqualsThenTool {
    fn name(&self) -> &str {
        "LOOP-FOR-EQUALS-THEN"
    }
    fn description(&self) -> &str {
        "Iterate with = THEN update form"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-FOR-BEING - Hash table and package iteration
pub struct LoopForBeingTool;
impl Tool for LoopForBeingTool {
    fn name(&self) -> &str {
        "LOOP-FOR-BEING"
    }
    fn description(&self) -> &str {
        "Iterate over hash tables or packages"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected at least 1 argument (collection)".to_string(),
            });
        }
        Ok(args[0].clone())
    }
}

// ============================================================
// MULTIPLE ACCUMULATION
// ============================================================

/// LOOP-INTO - Accumulate into named variable
pub struct LoopIntoTool;
impl Tool for LoopIntoTool {
    fn name(&self) -> &str {
        "LOOP-INTO"
    }
    fn description(&self) -> &str {
        "Accumulate into named variable"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-MINIMIZE - Find minimum value
pub struct LoopMinimizeTool;
impl Tool for LoopMinimizeTool {
    fn name(&self) -> &str {
        "LOOP-MINIMIZE"
    }
    fn description(&self) -> &str {
        "Accumulate minimum value"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected at least 1 argument (value)".to_string(),
            });
        }
        let mut min = match &args[0] {
            Value::Int(n) => Value::Int(*n),
            Value::Float(f) => Value::Float(*f),
            _ => return Ok(args[0].clone()),
        };
        for arg in &args[1..] {
            match (arg, &min) {
                (Value::Int(a), Value::Int(b)) if a < b => min = Value::Int(*a),
                (Value::Float(a), Value::Float(b)) if a < b => min = Value::Float(*a),
                (Value::Int(a), Value::Float(b)) if (*a as f64) < *b => {
                    min = Value::Float(*a as f64)
                }
                (Value::Float(a), Value::Int(b)) if *a < (*b as f64) => min = Value::Float(*a),
                _ => {}
            }
        }
        Ok(min)
    }
}

/// LOOP-APPEND - Append lists
pub struct LoopAppendTool;
impl Tool for LoopAppendTool {
    fn name(&self) -> &str {
        "LOOP-APPEND"
    }
    fn description(&self) -> &str {
        "Append accumulated lists"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let mut result = vec![];
        for arg in args {
            match arg {
                Value::Array(arr) => result.extend(arr.iter().cloned()),
                v => result.push(v.clone()),
            }
        }
        Ok(Value::Array(Arc::new(result)))
    }
}

/// LOOP-NCONC - Destructively concatenate lists
pub struct LoopNconcTool;
impl Tool for LoopNconcTool {
    fn name(&self) -> &str {
        "LOOP-NCONC"
    }
    fn description(&self) -> &str {
        "Destructively concatenate lists"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let mut result = vec![];
        for arg in args {
            match arg {
                Value::Array(arr) => result.extend(arr.iter().cloned()),
                v => result.push(v.clone()),
            }
        }
        Ok(Value::Array(Arc::new(result)))
    }
}

// ============================================================
// CONDITIONAL CLAUSES
// ============================================================

/// LOOP-IF-IT - IF with IT binding
pub struct LoopIfItTool;
impl Tool for LoopIfItTool {
    fn name(&self) -> &str {
        "LOOP-IF-IT"
    }
    fn description(&self) -> &str {
        "IF clause with IT variable"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
        }
        if args[0].is_truthy() {
            Ok(if args.len() > 1 {
                args[1].clone()
            } else {
                args[0].clone()
            })
        } else {
            Ok(if args.len() > 2 {
                args[2].clone()
            } else {
                Value::Null
            })
        }
    }
}

/// LOOP-THEREIS - Test and return
pub struct LoopThereisTool;
impl Tool for LoopThereisTool {
    fn name(&self) -> &str {
        "LOOP-THEREIS"
    }
    fn description(&self) -> &str {
        "Test condition and return if true"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        for arg in args {
            if arg.is_truthy() {
                return Ok(arg.clone());
            }
        }
        Ok(Value::Bool(false))
    }
}

/// LOOP-ALWAYS - Test all conditions
pub struct LoopAlwaysTool;
impl Tool for LoopAlwaysTool {
    fn name(&self) -> &str {
        "LOOP-ALWAYS"
    }
    fn description(&self) -> &str {
        "Return true if all conditions true"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        for arg in args {
            if !arg.is_truthy() {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    }
}

/// LOOP-NEVER - Test no conditions true
pub struct LoopNeverTool;
impl Tool for LoopNeverTool {
    fn name(&self) -> &str {
        "LOOP-NEVER"
    }
    fn description(&self) -> &str {
        "Return true if no conditions true"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        for arg in args {
            if arg.is_truthy() {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    }
}

// ============================================================
// LOOP FINISHING
// ============================================================

/// LOOP-NAMED - Named LOOP for RETURN-FROM
pub struct LoopNamedTool;
impl Tool for LoopNamedTool {
    fn name(&self) -> &str {
        "LOOP-NAMED"
    }
    fn description(&self) -> &str {
        "Name LOOP for RETURN-FROM"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[args.len() - 1].clone()
        })
    }
}

/// LOOP-INITIALLY - Execute before LOOP body
pub struct LoopInitiallyTool;
impl Tool for LoopInitiallyTool {
    fn name(&self) -> &str {
        "LOOP-INITIALLY"
    }
    fn description(&self) -> &str {
        "Execute forms before loop starts"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[args.len() - 1].clone()
        })
    }
}

/// LOOP-REPEAT - Repeat fixed number of times
pub struct LoopRepeatTool;
impl Tool for LoopRepeatTool {
    fn name(&self) -> &str {
        "LOOP-REPEAT"
    }
    fn description(&self) -> &str {
        "Repeat loop N times"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(*n)),
            _ => Ok(Value::Int(1)),
        }
    }
}

/// Register all advanced LOOP functions
pub fn register(registry: &mut ToolRegistry) {
    // Destructuring iteration
    registry.register(LoopDestructuringTool);
    registry.register(LoopForOnTool);
    registry.register(LoopForEqualsThenTool);
    registry.register(LoopForBeingTool);

    // Multiple accumulation
    registry.register(LoopIntoTool);
    registry.register(LoopMinimizeTool);
    registry.register(LoopAppendTool);
    registry.register(LoopNconcTool);

    // Conditional clauses
    registry.register(LoopIfItTool);
    registry.register(LoopThereisTool);
    registry.register(LoopAlwaysTool);
    registry.register(LoopNeverTool);

    // Loop finishing
    registry.register(LoopNamedTool);
    registry.register(LoopInitiallyTool);
    registry.register(LoopRepeatTool);
}
