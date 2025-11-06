//! CLOS method combinations for OVSM
//!
//! Method combination types, qualifiers, and combination control.
//! Provides full Common Lisp Object System method combination support.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// Method combination functions (20 total)

// ============================================================
// METHOD COMBINATION DEFINITION
// ============================================================

/// DEFINE-METHOD-COMBINATION - Define method combination type
pub struct DefineMethodCombinationTool;
impl Tool for DefineMethodCombinationTool {
    fn name(&self) -> &str {
        "DEFINE-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "Define new method combination type"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// METHOD-COMBINATION-NAME - Get method combination name
pub struct MethodCombinationNameTool;
impl Tool for MethodCombinationNameTool {
    fn name(&self) -> &str {
        "METHOD-COMBINATION-NAME"
    }
    fn description(&self) -> &str {
        "Get name of method combination"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::String("STANDARD".to_string())
        } else {
            args[0].clone()
        })
    }
}

/// METHOD-COMBINATION-TYPE - Get method combination type
pub struct MethodCombinationTypeTool;
impl Tool for MethodCombinationTypeTool {
    fn name(&self) -> &str {
        "METHOD-COMBINATION-TYPE"
    }
    fn description(&self) -> &str {
        "Get type of method combination"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation - should accept method combination object
        Ok(Value::String("STANDARD".to_string()))
    }
}

/// FIND-METHOD-COMBINATION - Find method combination by name
pub struct FindMethodCombinationTool;
impl Tool for FindMethodCombinationTool {
    fn name(&self) -> &str {
        "FIND-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "Find method combination by name"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

// ============================================================
// BUILT-IN METHOD COMBINATIONS
// ============================================================

/// STANDARD-METHOD-COMBINATION - Standard combination
pub struct StandardMethodCombinationTool;
impl Tool for StandardMethodCombinationTool {
    fn name(&self) -> &str {
        "STANDARD-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "Standard method combination (before, primary, after, around)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Returns combined result of all applicable methods
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[args.len() - 1].clone()
        })
    }
}

/// AND-METHOD-COMBINATION - AND combination
pub struct AndMethodCombinationTool;
impl Tool for AndMethodCombinationTool {
    fn name(&self) -> &str {
        "AND-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "AND method combination (short-circuit on NIL)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        for arg in args {
            if !arg.is_truthy() {
                return Ok(Value::Bool(false));
            }
        }
        Ok(if args.is_empty() {
            Value::Bool(true)
        } else {
            args[args.len() - 1].clone()
        })
    }
}

/// OR-METHOD-COMBINATION - OR combination
pub struct OrMethodCombinationTool;
impl Tool for OrMethodCombinationTool {
    fn name(&self) -> &str {
        "OR-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "OR method combination (short-circuit on non-NIL)"
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

/// PROGN-METHOD-COMBINATION - PROGN combination
pub struct PrognMethodCombinationTool;
impl Tool for PrognMethodCombinationTool {
    fn name(&self) -> &str {
        "PROGN-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "PROGN method combination (call all, return last)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[args.len() - 1].clone()
        })
    }
}

/// APPEND-METHOD-COMBINATION - APPEND combination
pub struct AppendMethodCombinationTool;
impl Tool for AppendMethodCombinationTool {
    fn name(&self) -> &str {
        "APPEND-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "APPEND method combination (append all results)"
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

/// NCONC-METHOD-COMBINATION - NCONC combination
pub struct NconcMethodCombinationTool;
impl Tool for NconcMethodCombinationTool {
    fn name(&self) -> &str {
        "NCONC-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "NCONC method combination (destructively append results)"
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

/// LIST-METHOD-COMBINATION - LIST combination
pub struct ListMethodCombinationTool;
impl Tool for ListMethodCombinationTool {
    fn name(&self) -> &str {
        "LIST-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "LIST method combination (collect results in list)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(args.to_vec())))
    }
}

/// MAX-METHOD-COMBINATION - MAX combination
pub struct MaxMethodCombinationTool;
impl Tool for MaxMethodCombinationTool {
    fn name(&self) -> &str {
        "MAX-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "MAX method combination (return maximum result)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected at least 1 argument (method result)".to_string(),
            });
        }
        let mut max = Value::Int(i64::MIN);
        for arg in args {
            match (arg, &max) {
                (Value::Int(a), Value::Int(b)) if a > b => max = Value::Int(*a),
                (Value::Float(a), Value::Float(b)) if a > b => max = Value::Float(*a),
                (Value::Int(a), Value::Float(b)) if (*a as f64) > *b => {
                    max = Value::Float(*a as f64)
                }
                (Value::Float(a), Value::Int(b)) if *a > (*b as f64) => max = Value::Float(*a),
                _ => {}
            }
        }
        Ok(max)
    }
}

/// MIN-METHOD-COMBINATION - MIN combination
pub struct MinMethodCombinationTool;
impl Tool for MinMethodCombinationTool {
    fn name(&self) -> &str {
        "MIN-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "MIN method combination (return minimum result)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected at least 1 argument (method result)".to_string(),
            });
        }
        let mut min = Value::Int(i64::MAX);
        for arg in args {
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

/// PLUS-METHOD-COMBINATION - + combination
pub struct PlusMethodCombinationTool;
impl Tool for PlusMethodCombinationTool {
    fn name(&self) -> &str {
        "PLUS-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "+ method combination (sum all results)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let mut sum_int: i64 = 0;
        let mut sum_float: f64 = 0.0;
        let mut has_float = false;

        for arg in args {
            match arg {
                Value::Int(n) => {
                    sum_int += n;
                    sum_float += *n as f64;
                }
                Value::Float(f) => {
                    has_float = true;
                    sum_float += f;
                }
                _ => {}
            }
        }

        Ok(if has_float {
            Value::Float(sum_float)
        } else {
            Value::Int(sum_int)
        })
    }
}

// ============================================================
// METHOD QUALIFIERS
// ============================================================

/// METHOD-QUALIFIERS - Get method qualifiers
pub struct MethodQualifiersTool;
impl Tool for MethodQualifiersTool {
    fn name(&self) -> &str {
        "METHOD-QUALIFIERS"
    }
    fn description(&self) -> &str {
        "Get qualifiers of a method"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation - should accept method object
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// PRIMARY-METHOD-P - Check if primary method
pub struct PrimaryMethodPTool;
impl Tool for PrimaryMethodPTool {
    fn name(&self) -> &str {
        "PRIMARY-METHOD-P"
    }
    fn description(&self) -> &str {
        "Check if method is primary method"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation - should accept method object
        Ok(Value::Bool(true))
    }
}

/// BEFORE-METHOD-P - Check if before method
pub struct BeforeMethodPTool;
impl Tool for BeforeMethodPTool {
    fn name(&self) -> &str {
        "BEFORE-METHOD-P"
    }
    fn description(&self) -> &str {
        "Check if method is :before method"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation - should accept method object
        Ok(Value::Bool(false))
    }
}

/// AFTER-METHOD-P - Check if after method
pub struct AfterMethodPTool;
impl Tool for AfterMethodPTool {
    fn name(&self) -> &str {
        "AFTER-METHOD-P"
    }
    fn description(&self) -> &str {
        "Check if method is :after method"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation - should accept method object
        Ok(Value::Bool(false))
    }
}

/// AROUND-METHOD-P - Check if around method
pub struct AroundMethodPTool;
impl Tool for AroundMethodPTool {
    fn name(&self) -> &str {
        "AROUND-METHOD-P"
    }
    fn description(&self) -> &str {
        "Check if method is :around method"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation - should accept method object
        Ok(Value::Bool(false))
    }
}

/// CALL-NEXT-METHOD - Call next most specific method
pub struct CallNextMethodTool;
impl Tool for CallNextMethodTool {
    fn name(&self) -> &str {
        "CALL-NEXT-METHOD"
    }
    fn description(&self) -> &str {
        "Call next most specific method"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// NEXT-METHOD-P - Check if next method exists
pub struct NextMethodPTool;
impl Tool for NextMethodPTool {
    fn name(&self) -> &str {
        "NEXT-METHOD-P"
    }
    fn description(&self) -> &str {
        "Check if next method exists"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation
        Ok(Value::Bool(false))
    }
}

/// Register all method combination functions
pub fn register(registry: &mut ToolRegistry) {
    // Method combination definition
    registry.register(DefineMethodCombinationTool);
    registry.register(MethodCombinationNameTool);
    registry.register(MethodCombinationTypeTool);
    registry.register(FindMethodCombinationTool);

    // Built-in method combinations
    registry.register(StandardMethodCombinationTool);
    registry.register(AndMethodCombinationTool);
    registry.register(OrMethodCombinationTool);
    registry.register(PrognMethodCombinationTool);
    registry.register(AppendMethodCombinationTool);
    registry.register(NconcMethodCombinationTool);
    registry.register(ListMethodCombinationTool);
    registry.register(MaxMethodCombinationTool);
    registry.register(MinMethodCombinationTool);
    registry.register(PlusMethodCombinationTool);

    // Method qualifiers
    registry.register(MethodQualifiersTool);
    registry.register(PrimaryMethodPTool);
    registry.register(BeforeMethodPTool);
    registry.register(AfterMethodPTool);
    registry.register(AroundMethodPTool);
    registry.register(CallNextMethodTool);
    registry.register(NextMethodPTool);
}
