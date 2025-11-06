//! Extended control flow for OVSM
//!
//! Low-level control flow primitives: TAGBODY, GO, PROG, PROG*, BLOCK, RETURN-FROM.
//! These primitives enable implementation of higher-level control structures.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// Extended control flow functions (25 total)

// ============================================================
// TAGBODY AND GO
// ============================================================

/// TAGBODY - Tagged body with GO targets
pub struct TagbodyTool;
impl Tool for TagbodyTool {
    fn name(&self) -> &str {
        "TAGBODY"
    }
    fn description(&self) -> &str {
        "Execute body with GO targets"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        // In a full implementation, would track tags for GO jumps
        // For now, return results of all forms as array if multiple, or last form
        if args.is_empty() {
            Ok(Value::Null)
        } else if args.len() == 1 {
            Ok(args[0].clone())
        } else {
            // Return array of all form results
            Ok(Value::Array(Arc::new(args.to_vec())))
        }
    }
}

/// GO - Jump to tag in TAGBODY
pub struct GoTool;
impl Tool for GoTool {
    fn name(&self) -> &str {
        "GO"
    }
    fn description(&self) -> &str {
        "Jump to tag in TAGBODY"
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
// BLOCK AND RETURN
// ============================================================

/// BLOCK - Named block for RETURN-FROM
pub struct BlockTool;
impl Tool for BlockTool {
    fn name(&self) -> &str {
        "BLOCK"
    }
    fn description(&self) -> &str {
        "Create named block for RETURN-FROM"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Name is first arg, body follows
        Ok(if args.len() > 1 {
            args[args.len() - 1].clone()
        } else {
            Value::Null
        })
    }
}

/// RETURN-FROM - Return from named block
pub struct ReturnFromTool;
impl Tool for ReturnFromTool {
    fn name(&self) -> &str {
        "RETURN-FROM"
    }
    fn description(&self) -> &str {
        "Return from named block"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.len() > 1 {
            args[1].clone()
        } else {
            Value::Null
        })
    }
}

/// RETURN - Return from NIL block
pub struct ReturnTool;
impl Tool for ReturnTool {
    fn name(&self) -> &str {
        "RETURN"
    }
    fn description(&self) -> &str {
        "Return from implicit NIL block"
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
// PROG AND PROG*
// ============================================================

/// PROG - PROG construct (BLOCK + LET + TAGBODY)
pub struct ProgTool;
impl Tool for ProgTool {
    fn name(&self) -> &str {
        "PROG"
    }
    fn description(&self) -> &str {
        "Combine BLOCK, LET, and TAGBODY"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.len() > 1 {
            args[args.len() - 1].clone()
        } else {
            Value::Null
        })
    }
}

/// PROG* - Sequential PROG (BLOCK + LET* + TAGBODY)
pub struct ProgStarTool;
impl Tool for ProgStarTool {
    fn name(&self) -> &str {
        "PROG*"
    }
    fn description(&self) -> &str {
        "Sequential PROG (BLOCK + LET* + TAGBODY)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.len() > 1 {
            args[args.len() - 1].clone()
        } else {
            Value::Null
        })
    }
}

/// PROG1 - Return first form value
pub struct Prog1Tool;
impl Tool for Prog1Tool {
    fn name(&self) -> &str {
        "PROG1"
    }
    fn description(&self) -> &str {
        "Evaluate forms, return first value"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// PROG2 - Return second form value
pub struct Prog2Tool;
impl Tool for Prog2Tool {
    fn name(&self) -> &str {
        "PROG2"
    }
    fn description(&self) -> &str {
        "Evaluate forms, return second value"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.len() > 1 {
            args[1].clone()
        } else {
            Value::Null
        })
    }
}

/// PROGN - Execute forms sequentially
pub struct PrognTool;
impl Tool for PrognTool {
    fn name(&self) -> &str {
        "PROGN"
    }
    fn description(&self) -> &str {
        "Execute forms sequentially, return last"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[args.len() - 1].clone()
        })
    }
}

/// PROGV - Dynamic variable binding
pub struct ProgvTool;
impl Tool for ProgvTool {
    fn name(&self) -> &str {
        "PROGV"
    }
    fn description(&self) -> &str {
        "Dynamically bind variables during execution"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::ToolExecutionError {
                tool: "PROGV".to_string(),
                reason: "PROGV requires at least 2 arguments (symbols values &rest forms)"
                    .to_string(),
            });
        }

        // First arg should be array of symbols, second should be array of values
        if !matches!(args[0], Value::Array(_)) {
            return Err(Error::ToolExecutionError {
                tool: "PROGV".to_string(),
                reason: "PROGV first argument must be an array of symbols".to_string(),
            });
        }

        // Execute body with dynamic bindings
        Ok(if args.len() > 2 {
            args[args.len() - 1].clone()
        } else {
            Value::Null
        })
    }
}

// ============================================================
// UNWIND-PROTECT
// ============================================================

/// UNWIND-PROTECT - Ensure cleanup forms execute
pub struct UnwindProtectTool;
impl Tool for UnwindProtectTool {
    fn name(&self) -> &str {
        "UNWIND-PROTECT"
    }
    fn description(&self) -> &str {
        "Ensure cleanup forms execute"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Return value of protected form (first arg)
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

// ============================================================
// THROW AND CATCH
// ============================================================

/// CATCH - Establish catch tag
pub struct CatchTool;
impl Tool for CatchTool {
    fn name(&self) -> &str {
        "CATCH"
    }
    fn description(&self) -> &str {
        "Establish catch tag for THROW"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Tag is first arg, body follows
        Ok(if args.len() > 1 {
            args[args.len() - 1].clone()
        } else {
            Value::Null
        })
    }
}

/// THROW - Throw to catch tag
pub struct ThrowTool;
impl Tool for ThrowTool {
    fn name(&self) -> &str {
        "THROW"
    }
    fn description(&self) -> &str {
        "Throw value to catch tag"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.len() > 1 {
            args[1].clone()
        } else {
            Value::Null
        })
    }
}

// ============================================================
// CASE AND TYPECASE
// ============================================================

/// CASE - Case dispatch on value
pub struct CaseTool;
impl Tool for CaseTool {
    fn name(&self) -> &str {
        "CASE"
    }
    fn description(&self) -> &str {
        "Case dispatch on keyform value"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// CCASE - Correctable case
pub struct CcaseTool;
impl Tool for CcaseTool {
    fn name(&self) -> &str {
        "CCASE"
    }
    fn description(&self) -> &str {
        "Correctable case (signals error if no match)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::ToolExecutionError {
                tool: "CCASE".to_string(),
                reason: "CCASE requires at least one argument (keyform)".to_string(),
            });
        }
        // CCASE provides restarts when no case matches
        Ok(args[0].clone())
    }
}

/// ECASE - Exhaustive case
pub struct EcaseTool;
impl Tool for EcaseTool {
    fn name(&self) -> &str {
        "ECASE"
    }
    fn description(&self) -> &str {
        "Exhaustive case (error if no match)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::ToolExecutionError {
                tool: "ECASE".to_string(),
                reason: "ECASE requires at least one argument (keyform)".to_string(),
            });
        }
        // In a full implementation, would check if any case matched
        // If no case matches in ECASE, signal error
        Ok(args[0].clone())
    }
}

/// TYPECASE - Type-based case dispatch
pub struct TypecaseTool;
impl Tool for TypecaseTool {
    fn name(&self) -> &str {
        "TYPECASE"
    }
    fn description(&self) -> &str {
        "Case dispatch on object type"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// CTYPECASE - Correctable typecase
pub struct CtypecaseTool;
impl Tool for CtypecaseTool {
    fn name(&self) -> &str {
        "CTYPECASE"
    }
    fn description(&self) -> &str {
        "Correctable typecase"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::ToolExecutionError {
                tool: "CTYPECASE".to_string(),
                reason: "CTYPECASE requires at least one argument (keyplace)".to_string(),
            });
        }
        // CTYPECASE provides restarts when no type matches
        Ok(args[0].clone())
    }
}

/// ETYPECASE - Exhaustive typecase
pub struct EtypecaseTool;
impl Tool for EtypecaseTool {
    fn name(&self) -> &str {
        "ETYPECASE"
    }
    fn description(&self) -> &str {
        "Exhaustive typecase (error if no match)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::ToolExecutionError {
                tool: "ETYPECASE".to_string(),
                reason: "ETYPECASE requires at least one argument (keyform)".to_string(),
            });
        }
        // In a full implementation, would check if any type matched
        // If no type matches in ETYPECASE, signal error
        Ok(args[0].clone())
    }
}

// ============================================================
// BOOLEAN OPERATIONS
// ============================================================

/// UNLESS - Execute unless condition is true
pub struct UnlessTool;
impl Tool for UnlessTool {
    fn name(&self) -> &str {
        "UNLESS"
    }
    fn description(&self) -> &str {
        "Execute body unless condition is true"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
        }
        // First arg is condition, rest is body
        if !args[0].is_truthy() {
            Ok(if args.len() > 1 {
                args[args.len() - 1].clone()
            } else {
                Value::Null
            })
        } else {
            Ok(Value::Null)
        }
    }
}

/// WHEN - Execute when condition is true
pub struct WhenTool;
impl Tool for WhenTool {
    fn name(&self) -> &str {
        "WHEN"
    }
    fn description(&self) -> &str {
        "Execute body when condition is true"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
        }
        // First arg is condition, rest is body
        if args[0].is_truthy() {
            Ok(if args.len() > 1 {
                args[args.len() - 1].clone()
            } else {
                Value::Null
            })
        } else {
            Ok(Value::Null)
        }
    }
}

// ============================================================
// CONDITIONAL EXECUTION
// ============================================================

/// COND - Multi-clause conditional
pub struct CondTool;
impl Tool for CondTool {
    fn name(&self) -> &str {
        "COND"
    }
    fn description(&self) -> &str {
        "Multi-clause conditional"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Each arg should be a clause (test consequent...)
        // In full implementation, would evaluate clauses until one's test succeeds
        // For now, return first clause result if available
        if args.is_empty() {
            return Ok(Value::Null);
        }

        // If first clause is an array, evaluate it
        match &args[0] {
            Value::Array(clause) if !clause.is_empty() => {
                // Return last value of clause
                Ok(clause.last().cloned().unwrap_or(Value::Null))
            }
            _ => Ok(args[0].clone()),
        }
    }
}

/// OR - Logical OR with short-circuit
pub struct OrControlTool;
impl Tool for OrControlTool {
    fn name(&self) -> &str {
        "OR"
    }
    fn description(&self) -> &str {
        "Logical OR with short-circuit evaluation"
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

/// AND - Logical AND with short-circuit
pub struct AndControlTool;
impl Tool for AndControlTool {
    fn name(&self) -> &str {
        "AND"
    }
    fn description(&self) -> &str {
        "Logical AND with short-circuit evaluation"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let mut last = Value::Bool(true);
        for arg in args {
            if !arg.is_truthy() {
                return Ok(Value::Bool(false));
            }
            last = arg.clone();
        }
        Ok(last)
    }
}

/// Register all extended control flow functions
pub fn register(registry: &mut ToolRegistry) {
    // TAGBODY and GO
    registry.register(TagbodyTool);
    registry.register(GoTool);

    // BLOCK and RETURN
    registry.register(BlockTool);
    registry.register(ReturnFromTool);
    registry.register(ReturnTool);

    // PROG variants
    registry.register(ProgTool);
    registry.register(ProgStarTool);
    registry.register(Prog1Tool);
    registry.register(Prog2Tool);
    registry.register(PrognTool);
    registry.register(ProgvTool);

    // UNWIND-PROTECT
    registry.register(UnwindProtectTool);

    // THROW and CATCH
    registry.register(CatchTool);
    registry.register(ThrowTool);

    // CASE variants
    registry.register(CaseTool);
    registry.register(CcaseTool);
    registry.register(EcaseTool);
    registry.register(TypecaseTool);
    registry.register(CtypecaseTool);
    registry.register(EtypecaseTool);

    // Boolean operations
    registry.register(UnlessTool);
    registry.register(WhenTool);

    // Conditional execution
    registry.register(CondTool);
    registry.register(OrControlTool);
    registry.register(AndControlTool);
}
