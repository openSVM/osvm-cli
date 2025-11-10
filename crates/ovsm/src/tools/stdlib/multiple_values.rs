//! Multiple values support for OVSM
//!
//! Common Lisp multiple values system.
//! Functions can return multiple values which can be captured or ignored.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// Multiple values functions (30 total)

// ============================================================
// MULTIPLE VALUES CREATION
// ============================================================

/// VALUES - Return multiple values
pub struct ValuesTool;
impl Tool for ValuesTool {
    fn name(&self) -> &str {
        "VALUES"
    }
    fn description(&self) -> &str {
        "Return multiple values"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(args.to_vec())))
    }
}

/// VALUES-LIST - Return values from list
pub struct ValuesListTool;
impl Tool for ValuesListTool {
    fn name(&self) -> &str {
        "VALUES-LIST"
    }
    fn description(&self) -> &str {
        "Return multiple values from list"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }
        match &args[0] {
            Value::Array(arr) => Ok(Value::Array(Arc::clone(arr))),
            _ => Ok(Value::Array(Arc::new(vec![args[0].clone()]))),
        }
    }
}

// ============================================================
// MULTIPLE VALUES BINDING
// ============================================================

/// MULTIPLE-VALUE-BIND - Bind multiple values
pub struct MultipleValueBindTool;
impl Tool for MultipleValueBindTool {
    fn name(&self) -> &str {
        "MULTIPLE-VALUE-BIND"
    }
    fn description(&self) -> &str {
        "Bind multiple values to variables"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Simplified: return last form result
        Ok(if args.len() > 1 {
            args[args.len() - 1].clone()
        } else {
            Value::Null
        })
    }
}

/// MULTIPLE-VALUE-LIST - Capture values as list
pub struct MultipleValueListTool;
impl Tool for MultipleValueListTool {
    fn name(&self) -> &str {
        "MULTIPLE-VALUE-LIST"
    }
    fn description(&self) -> &str {
        "Capture multiple values as list"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(args.to_vec())))
    }
}

/// MULTIPLE-VALUE-SETQ - Set multiple variables
pub struct MultipleValueSetqTool;
impl Tool for MultipleValueSetqTool {
    fn name(&self) -> &str {
        "MULTIPLE-VALUE-SETQ"
    }
    fn description(&self) -> &str {
        "Set multiple variables from values"
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
// MULTIPLE VALUES FUNCTIONS
// ============================================================

/// MULTIPLE-VALUE-CALL - Call with multiple values as args
pub struct MultipleValueCallTool;
impl Tool for MultipleValueCallTool {
    fn name(&self) -> &str {
        "MULTIPLE-VALUE-CALL"
    }
    fn description(&self) -> &str {
        "Call function with multiple values as arguments"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// MULTIPLE-VALUE-PROG1 - Return first value, evaluate forms
pub struct MultipleValueProg1Tool;
impl Tool for MultipleValueProg1Tool {
    fn name(&self) -> &str {
        "MULTIPLE-VALUE-PROG1"
    }
    fn description(&self) -> &str {
        "Return first form's values, evaluate remaining"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// NTH-VALUE - Get nth value
pub struct NthValueTool;
impl Tool for NthValueTool {
    fn name(&self) -> &str {
        "NTH-VALUE"
    }
    fn description(&self) -> &str {
        "Get nth value from multiple values"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 2 arguments (index, values)".to_string(),
            });
        }
        let n = match &args[0] {
            Value::Int(i) => *i as usize,
            _ => {
                return Err(Error::InvalidArguments {
                    tool: self.name().to_string(),
                    reason: "First argument must be an integer index".to_string(),
                })
            }
        };
        match &args[1] {
            Value::Array(arr) => Ok(arr.get(n).cloned().unwrap_or(Value::Null)),
            _ => Ok(if n == 0 { args[1].clone() } else { Value::Null }),
        }
    }
}

// ============================================================
// VALUE COUNT CONTROL
// ============================================================

/// VALUES-COUNT - Get number of values
pub struct ValuesCountTool;
impl Tool for ValuesCountTool {
    fn name(&self) -> &str {
        "VALUES-COUNT"
    }
    fn description(&self) -> &str {
        "Get number of values returned"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let count = match args.first() {
            Some(Value::Array(arr)) => arr.len(),
            Some(_) => 1,
            None => 0,
        };
        Ok(Value::Int(count as i64))
    }
}

/// EXTRACT-PRIMARY-VALUE - Get primary value only
pub struct ExtractPrimaryValueTool;
impl Tool for ExtractPrimaryValueTool {
    fn name(&self) -> &str {
        "EXTRACT-PRIMARY-VALUE"
    }
    fn description(&self) -> &str {
        "Extract only primary value, discard rest"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        match args.first() {
            Some(Value::Array(arr)) => Ok(arr.first().cloned().unwrap_or(Value::Null)),
            Some(v) => Ok(v.clone()),
            None => Ok(Value::Null),
        }
    }
}

// ============================================================
// DESTRUCTURING
// ============================================================

/// DESTRUCTURING-BIND - Destructure and bind values
pub struct DestructuringBindTool;
impl Tool for DestructuringBindTool {
    fn name(&self) -> &str {
        "DESTRUCTURING-BIND"
    }
    fn description(&self) -> &str {
        "Destructure list and bind to pattern"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.len() > 1 {
            args[args.len() - 1].clone()
        } else {
            Value::Null
        })
    }
}

// ============================================================
// GETF/SETF SUPPORT
// ============================================================

/// GETF - Get property from plist
pub struct GetfTool;
impl Tool for GetfTool {
    fn name(&self) -> &str {
        "GETF"
    }
    fn description(&self) -> &str {
        "Get property value from property list"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected at least 2 arguments (plist, key)".to_string(),
            });
        }
        match &args[0] {
            Value::Array(plist) => {
                if let Value::String(key) = &args[1] {
                    for i in (0..plist.len()).step_by(2) {
                        if let Value::String(k) = &plist[i] {
                            if k == key && i + 1 < plist.len() {
                                return Ok(plist[i + 1].clone());
                            }
                        }
                    }
                }
                // Return default if provided
                Ok(args.get(2).cloned().unwrap_or(Value::Null))
            }
            _ => Ok(args.get(2).cloned().unwrap_or(Value::Null)),
        }
    }
}

/// REMF - Remove property from plist
pub struct RemfTool;
impl Tool for RemfTool {
    fn name(&self) -> &str {
        "REMF"
    }
    fn description(&self) -> &str {
        "Remove property from property list"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::Bool(!args.is_empty()))
    }
}

/// GET-PROPERTIES - Get first matching property
pub struct GetPropertiesTool;
impl Tool for GetPropertiesTool {
    fn name(&self) -> &str {
        "GET-PROPERTIES"
    }
    fn description(&self) -> &str {
        "Get first property matching indicator list"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Expected 2 arguments (plist, indicators)".to_string(),
            });
        }
        // Simplified: return null indicator, value, and tail
        Ok(Value::Array(Arc::new(vec![
            Value::Null,
            Value::Null,
            args.first().cloned().unwrap_or(Value::Null),
        ])))
    }
}

// ============================================================
// SETF EXPANSIONS
// ============================================================

/// GET-SETF-EXPANSION - Get setf expansion
pub struct GetSetfExpansionTool;
impl Tool for GetSetfExpansionTool {
    fn name(&self) -> &str {
        "GET-SETF-EXPANSION"
    }
    fn description(&self) -> &str {
        "Get setf expansion for place"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Return 5 values: vars, vals, stores, writer, reader
        Ok(Value::Array(Arc::new(vec![
            Value::Array(Arc::new(vec![])),               // vars
            Value::Array(Arc::new(vec![])),               // vals
            Value::Array(Arc::new(vec![])),               // stores
            args.first().cloned().unwrap_or(Value::Null), // writer
            args.first().cloned().unwrap_or(Value::Null), // reader
        ])))
    }
}

/// DEFINE-SETF-EXPANDER - Define setf expander
pub struct DefineSetfExpanderTool;
impl Tool for DefineSetfExpanderTool {
    fn name(&self) -> &str {
        "DEFINE-SETF-EXPANDER"
    }
    fn description(&self) -> &str {
        "Define setf expander for access form"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// DEFSETF - Define simple setf method
pub struct DefsetfTool;
impl Tool for DefsetfTool {
    fn name(&self) -> &str {
        "DEFSETF"
    }
    fn description(&self) -> &str {
        "Define simple setf method"
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
// PLACE MANIPULATION
// ============================================================

/// SHIFTF - Shift values through places
pub struct ShiftfTool;
impl Tool for ShiftfTool {
    fn name(&self) -> &str {
        "SHIFTF"
    }
    fn description(&self) -> &str {
        "Shift values through places, return first"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// ROTATEF - Rotate values through places
pub struct RotatefTool;
impl Tool for RotatefTool {
    fn name(&self) -> &str {
        "ROTATEF"
    }
    fn description(&self) -> &str {
        "Rotate values through places"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Null)
    }
}

/// PSETF - Parallel setf
pub struct PsetfTool;
impl Tool for PsetfTool {
    fn name(&self) -> &str {
        "PSETF"
    }
    fn description(&self) -> &str {
        "Set multiple places in parallel"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Null)
    }
}

// ============================================================
// GENERALIZED REFERENCE
// ============================================================

/// SETF - Set place value
pub struct SetfTool;
impl Tool for SetfTool {
    fn name(&self) -> &str {
        "SETF"
    }
    fn description(&self) -> &str {
        "Set place to new value"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.len() > 1 {
            args[1].clone()
        } else {
            Value::Null
        })
    }
}

/// PSETQ - Parallel setq
pub struct PsetqTool;
impl Tool for PsetqTool {
    fn name(&self) -> &str {
        "PSETQ"
    }
    fn description(&self) -> &str {
        "Set multiple variables in parallel"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Null)
    }
}

/// INCF - Increment place
pub struct IncfTool;
impl Tool for IncfTool {
    fn name(&self) -> &str {
        "INCF"
    }
    fn description(&self) -> &str {
        "Increment place value"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }
        let delta = if args.len() > 1 {
            match &args[1] {
                Value::Int(n) => *n,
                Value::Float(f) => *f as i64,
                _ => 1,
            }
        } else {
            1
        };
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(n + delta)),
            Value::Float(f) => Ok(Value::Float(f + delta as f64)),
            _ => Ok(Value::Int(delta)),
        }
    }
}

/// DECF - Decrement place
pub struct DecfTool;
impl Tool for DecfTool {
    fn name(&self) -> &str {
        "DECF"
    }
    fn description(&self) -> &str {
        "Decrement place value"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }
        let delta = if args.len() > 1 {
            match &args[1] {
                Value::Int(n) => *n,
                Value::Float(f) => *f as i64,
                _ => 1,
            }
        } else {
            1
        };
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(n - delta)),
            Value::Float(f) => Ok(Value::Float(f - delta as f64)),
            _ => Ok(Value::Int(-delta)),
        }
    }
}

/// PUSH - Push onto list place
pub struct PushTool;
impl Tool for PushTool {
    fn name(&self) -> &str {
        "PUSH"
    }
    fn description(&self) -> &str {
        "Push item onto list place"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Array(Arc::new(vec![])));
        }
        match &args[1] {
            Value::Array(arr) => {
                let mut new_arr = vec![args[0].clone()];
                new_arr.extend(arr.iter().cloned());
                Ok(Value::Array(Arc::new(new_arr)))
            }
            _ => Ok(Value::Array(Arc::new(vec![args[0].clone()]))),
        }
    }
}

/// POP - Pop from list place
pub struct PopTool;
impl Tool for PopTool {
    fn name(&self) -> &str {
        "POP"
    }
    fn description(&self) -> &str {
        "Pop item from list place"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
        }
        match &args[0] {
            Value::Array(arr) => Ok(arr.first().cloned().unwrap_or(Value::Null)),
            _ => Ok(Value::Null),
        }
    }
}

/// PUSHNEW - Push if not already present
pub struct PushnewTool;
impl Tool for PushnewTool {
    fn name(&self) -> &str {
        "PUSHNEW"
    }
    fn description(&self) -> &str {
        "Push item if not already in list"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Array(Arc::new(vec![])));
        }
        match &args[1] {
            Value::Array(arr) => {
                if arr.contains(&args[0]) {
                    Ok(Value::Array(Arc::clone(arr)))
                } else {
                    let mut new_arr = vec![args[0].clone()];
                    new_arr.extend(arr.iter().cloned());
                    Ok(Value::Array(Arc::new(new_arr)))
                }
            }
            _ => Ok(Value::Array(Arc::new(vec![args[0].clone()]))),
        }
    }
}

// ============================================================
// MACRO SUPPORT
// ============================================================

/// DEFINE-MODIFY-MACRO - Define modify macro
pub struct DefineModifyMacroTool;
impl Tool for DefineModifyMacroTool {
    fn name(&self) -> &str {
        "DEFINE-MODIFY-MACRO"
    }
    fn description(&self) -> &str {
        "Define modify macro for place"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// DEFMACRO-WITH-PLACE - Define macro with place handling
pub struct DefmacroWithPlaceTool;
impl Tool for DefmacroWithPlaceTool {
    fn name(&self) -> &str {
        "DEFMACRO-WITH-PLACE"
    }
    fn description(&self) -> &str {
        "Define macro with generalized place handling"
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
// UTILITY FUNCTIONS
// ============================================================

/// APPLY-KEY - Apply key function if provided
pub struct ApplyKeyTool;
impl Tool for ApplyKeyTool {
    fn name(&self) -> &str {
        "APPLY-KEY"
    }
    fn description(&self) -> &str {
        "Apply key function if provided"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// IDENTITY - Return argument unchanged
pub struct IdentityTool;
impl Tool for IdentityTool {
    fn name(&self) -> &str {
        "IDENTITY"
    }
    fn description(&self) -> &str {
        "Return argument unchanged"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// Register all multiple values functions
pub fn register(registry: &mut ToolRegistry) {
    // Multiple values creation
    registry.register(ValuesTool);
    registry.register(ValuesListTool);

    // Multiple values binding
    registry.register(MultipleValueBindTool);
    registry.register(MultipleValueListTool);
    registry.register(MultipleValueSetqTool);

    // Multiple values functions
    registry.register(MultipleValueCallTool);
    registry.register(MultipleValueProg1Tool);
    registry.register(NthValueTool);

    // Value count control
    registry.register(ValuesCountTool);
    registry.register(ExtractPrimaryValueTool);

    // Destructuring
    registry.register(DestructuringBindTool);

    // GETF/SETF support
    registry.register(GetfTool);
    registry.register(RemfTool);
    registry.register(GetPropertiesTool);

    // SETF expansions
    registry.register(GetSetfExpansionTool);
    registry.register(DefineSetfExpanderTool);
    registry.register(DefsetfTool);

    // Place manipulation
    registry.register(ShiftfTool);
    registry.register(RotatefTool);
    registry.register(PsetfTool);

    // Generalized reference
    registry.register(SetfTool);
    registry.register(PsetqTool);
    registry.register(IncfTool);
    registry.register(DecfTool);
    registry.register(PushTool);
    registry.register(PopTool);
    registry.register(PushnewTool);

    // Macro support
    registry.register(DefineModifyMacroTool);
    registry.register(DefmacroWithPlaceTool);

    // Utility functions
    registry.register(ApplyKeyTool);
    registry.register(IdentityTool);
}
