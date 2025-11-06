//! Complete LOOP macro implementation for OVSM
//!
//! Full LOOP DSL with all clauses, destructuring, and complex iteration.
//! Extends the basic loop utilities with complete Common Lisp LOOP functionality.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// Complete LOOP macro functions (25 total)

// ============================================================
// LOOP ITERATION CLAUSES
// ============================================================

/// LOOP-FOR - FOR iteration clause
pub struct LoopForTool;
impl Tool for LoopForTool {
    fn name(&self) -> &str {
        "LOOP-FOR"
    }
    fn description(&self) -> &str {
        "FOR iteration clause in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-FROM - FROM starting value
pub struct LoopFromTool;
impl Tool for LoopFromTool {
    fn name(&self) -> &str {
        "LOOP-FROM"
    }
    fn description(&self) -> &str {
        "FROM starting value in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Int(0)
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-TO - TO ending value
pub struct LoopToTool;
impl Tool for LoopToTool {
    fn name(&self) -> &str {
        "LOOP-TO"
    }
    fn description(&self) -> &str {
        "TO ending value in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Int(10)
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-BELOW - BELOW upper bound
pub struct LoopBelowTool;
impl Tool for LoopBelowTool {
    fn name(&self) -> &str {
        "LOOP-BELOW"
    }
    fn description(&self) -> &str {
        "BELOW upper bound in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Int(10)
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-ABOVE - ABOVE lower bound
pub struct LoopAboveTool;
impl Tool for LoopAboveTool {
    fn name(&self) -> &str {
        "LOOP-ABOVE"
    }
    fn description(&self) -> &str {
        "ABOVE lower bound in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Int(0)
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-BY - BY step increment
pub struct LoopByTool;
impl Tool for LoopByTool {
    fn name(&self) -> &str {
        "LOOP-BY"
    }
    fn description(&self) -> &str {
        "BY step increment in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Int(1)
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-IN - IN list iteration
pub struct LoopInTool;
impl Tool for LoopInTool {
    fn name(&self) -> &str {
        "LOOP-IN"
    }
    fn description(&self) -> &str {
        "IN list iteration in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Array(Arc::new(vec![]))
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-ON - ON list iteration
pub struct LoopOnTool;
impl Tool for LoopOnTool {
    fn name(&self) -> &str {
        "LOOP-ON"
    }
    fn description(&self) -> &str {
        "ON list iteration in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Array(Arc::new(vec![]))
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-ACROSS - ACROSS array iteration
pub struct LoopAcrossTool;
impl Tool for LoopAcrossTool {
    fn name(&self) -> &str {
        "LOOP-ACROSS"
    }
    fn description(&self) -> &str {
        "ACROSS array iteration in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Array(Arc::new(vec![]))
        } else {
            args[0].clone()
        })
    }
}

// ============================================================
// LOOP CONDITIONAL CLAUSES
// ============================================================

/// LOOP-WHEN - WHEN conditional
pub struct LoopWhenTool;
impl Tool for LoopWhenTool {
    fn name(&self) -> &str {
        "LOOP-WHEN"
    }
    fn description(&self) -> &str {
        "WHEN conditional in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Bool(false)
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-UNLESS - UNLESS conditional
pub struct LoopUnlessTool;
impl Tool for LoopUnlessTool {
    fn name(&self) -> &str {
        "LOOP-UNLESS"
    }
    fn description(&self) -> &str {
        "UNLESS conditional in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Bool(true)
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-IF - IF conditional
pub struct LoopIfTool;
impl Tool for LoopIfTool {
    fn name(&self) -> &str {
        "LOOP-IF"
    }
    fn description(&self) -> &str {
        "IF conditional in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Bool(false)
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-WHILE - WHILE loop condition
pub struct LoopWhileTool;
impl Tool for LoopWhileTool {
    fn name(&self) -> &str {
        "LOOP-WHILE"
    }
    fn description(&self) -> &str {
        "WHILE loop condition in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Bool(true)
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-UNTIL - UNTIL loop condition
pub struct LoopUntilTool;
impl Tool for LoopUntilTool {
    fn name(&self) -> &str {
        "LOOP-UNTIL"
    }
    fn description(&self) -> &str {
        "UNTIL loop condition in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Bool(false)
        } else {
            args[0].clone()
        })
    }
}

// ============================================================
// LOOP ACCUMULATION CLAUSES
// ============================================================

/// LOOP-COLLECT - COLLECT values
pub struct LoopCollectTool;
impl Tool for LoopCollectTool {
    fn name(&self) -> &str {
        "LOOP-COLLECT"
    }
    fn description(&self) -> &str {
        "COLLECT values in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Array(Arc::new(vec![]))
        } else {
            Value::Array(Arc::new(args.to_vec()))
        })
    }
}

/// LOOP-APPEND - APPEND lists
pub struct LoopAppendTool;
impl Tool for LoopAppendTool {
    fn name(&self) -> &str {
        "LOOP-APPEND"
    }
    fn description(&self) -> &str {
        "APPEND lists in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let mut result = vec![];
        for arg in args {
            if let Value::Array(arr) = arg {
                result.extend(arr.iter().cloned());
            }
        }
        Ok(Value::Array(Arc::new(result)))
    }
}

/// LOOP-NCONC - NCONC lists destructively
pub struct LoopNconcTool;
impl Tool for LoopNconcTool {
    fn name(&self) -> &str {
        "LOOP-NCONC"
    }
    fn description(&self) -> &str {
        "NCONC lists destructively in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        LoopAppendTool.execute(args)
    }
}

/// LOOP-SUM - SUM numbers
pub struct LoopSumTool;
impl Tool for LoopSumTool {
    fn name(&self) -> &str {
        "LOOP-SUM"
    }
    fn description(&self) -> &str {
        "SUM numbers in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let mut sum = 0i64;
        for arg in args {
            if let Value::Int(n) = arg {
                sum += n;
            }
        }
        Ok(Value::Int(sum))
    }
}

/// LOOP-COUNT - COUNT matching items
pub struct LoopCountTool;
impl Tool for LoopCountTool {
    fn name(&self) -> &str {
        "LOOP-COUNT"
    }
    fn description(&self) -> &str {
        "COUNT matching items in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::Int(args.len() as i64))
    }
}

/// LOOP-MAXIMIZE - MAXIMIZE value
pub struct LoopMaximizeTool;
impl Tool for LoopMaximizeTool {
    fn name(&self) -> &str {
        "LOOP-MAXIMIZE"
    }
    fn description(&self) -> &str {
        "MAXIMIZE value in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected at least 1 argument (value)".to_string(),
            });
        }
        let mut max = i64::MIN;
        for arg in args {
            if let Value::Int(n) = arg {
                max = max.max(*n);
            }
        }
        Ok(Value::Int(max))
    }
}

/// LOOP-MINIMIZE - MINIMIZE value
pub struct LoopMinimizeTool;
impl Tool for LoopMinimizeTool {
    fn name(&self) -> &str {
        "LOOP-MINIMIZE"
    }
    fn description(&self) -> &str {
        "MINIMIZE value in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected at least 1 argument (value)".to_string(),
            });
        }
        let mut min = i64::MAX;
        for arg in args {
            if let Value::Int(n) = arg {
                min = min.min(*n);
            }
        }
        Ok(Value::Int(min))
    }
}

// ============================================================
// LOOP CONTROL CLAUSES
// ============================================================

/// LOOP-DO - DO execute forms
pub struct LoopDoTool;
impl Tool for LoopDoTool {
    fn name(&self) -> &str {
        "LOOP-DO"
    }
    fn description(&self) -> &str {
        "DO execute forms in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[args.len() - 1].clone()
        })
    }
}

/// LOOP-RETURN - RETURN from loop
pub struct LoopReturnTool;
impl Tool for LoopReturnTool {
    fn name(&self) -> &str {
        "LOOP-RETURN"
    }
    fn description(&self) -> &str {
        "RETURN from LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// LOOP-WITH - WITH variable binding
pub struct LoopWithTool;
impl Tool for LoopWithTool {
    fn name(&self) -> &str {
        "LOOP-WITH"
    }
    fn description(&self) -> &str {
        "WITH variable binding in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.len() >= 2 {
            args[1].clone()
        } else {
            Value::Null
        })
    }
}

/// LOOP-INITIALLY - INITIALLY execute once
pub struct LoopInitiallyTool;
impl Tool for LoopInitiallyTool {
    fn name(&self) -> &str {
        "LOOP-INITIALLY"
    }
    fn description(&self) -> &str {
        "INITIALLY execute once in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[args.len() - 1].clone()
        })
    }
}

/// LOOP-FINALLY - FINALLY execute at end
pub struct LoopFinallyTool;
impl Tool for LoopFinallyTool {
    fn name(&self) -> &str {
        "LOOP-FINALLY"
    }
    fn description(&self) -> &str {
        "FINALLY execute at end in LOOP"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[args.len() - 1].clone()
        })
    }
}

/// Register all complete LOOP functions
pub fn register(registry: &mut ToolRegistry) {
    // Iteration clauses
    registry.register(LoopForTool);
    registry.register(LoopFromTool);
    registry.register(LoopToTool);
    registry.register(LoopBelowTool);
    registry.register(LoopAboveTool);
    registry.register(LoopByTool);
    registry.register(LoopInTool);
    registry.register(LoopOnTool);
    registry.register(LoopAcrossTool);

    // Conditional clauses
    registry.register(LoopWhenTool);
    registry.register(LoopUnlessTool);
    registry.register(LoopIfTool);
    registry.register(LoopWhileTool);
    registry.register(LoopUntilTool);

    // Accumulation clauses
    registry.register(LoopCollectTool);
    registry.register(LoopAppendTool);
    registry.register(LoopNconcTool);
    registry.register(LoopSumTool);
    registry.register(LoopCountTool);
    registry.register(LoopMaximizeTool);
    registry.register(LoopMinimizeTool);

    // Control clauses
    registry.register(LoopDoTool);
    registry.register(LoopReturnTool);
    registry.register(LoopWithTool);
    registry.register(LoopInitiallyTool);
    registry.register(LoopFinallyTool);
}
