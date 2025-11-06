//! Reader control for OVSM
//!
//! Read-time evaluation, reader dispatch, and custom syntax.
//! Provides Common Lisp reader customization capabilities.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// Reader control functions (12 total)

// ============================================================
// READ-TIME EVALUATION
// ============================================================

/// #. - Read-time evaluation
pub struct ReadTimeEvalTool;
impl Tool for ReadTimeEvalTool {
    fn name(&self) -> &str {
        "#."
    }
    fn description(&self) -> &str {
        "Evaluate form at read time"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "#.".to_string(),
                reason: "Expected form to evaluate".to_string(),
            });
        }
        Ok(args[0].clone())
    }
}

/// #, - Load-time evaluation
pub struct LoadTimeEvalTool;
impl Tool for LoadTimeEvalTool {
    fn name(&self) -> &str {
        "#,"
    }
    fn description(&self) -> &str {
        "Evaluate form at load time"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "#,".to_string(),
                reason: "Expected form to evaluate".to_string(),
            });
        }
        Ok(args[0].clone())
    }
}

// ============================================================
// READER DISPATCH
// ============================================================

/// GET-DISPATCH-MACRO-CHARACTER - Get dispatch macro
pub struct GetDispatchMacroCharacterTool;
impl Tool for GetDispatchMacroCharacterTool {
    fn name(&self) -> &str {
        "GET-DISPATCH-MACRO-CHARACTER"
    }
    fn description(&self) -> &str {
        "Get dispatch macro character function"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "GET-DISPATCH-MACRO-CHARACTER".to_string(),
                reason: "Expected dispatch character and sub-character".to_string(),
            });
        }
        // Placeholder: return empty array wrapped in Arc
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// SET-DISPATCH-MACRO-CHARACTER - Set dispatch macro
pub struct SetDispatchMacroCharacterTool;
impl Tool for SetDispatchMacroCharacterTool {
    fn name(&self) -> &str {
        "SET-DISPATCH-MACRO-CHARACTER"
    }
    fn description(&self) -> &str {
        "Set dispatch macro character function"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "SET-DISPATCH-MACRO-CHARACTER".to_string(),
                reason: "Expected dispatch character, sub-character, and function".to_string(),
            });
        }
        Ok(Value::Bool(true))
    }
}

/// MAKE-DISPATCH-MACRO-CHARACTER - Create dispatch character
pub struct MakeDispatchMacroCharacterTool;
impl Tool for MakeDispatchMacroCharacterTool {
    fn name(&self) -> &str {
        "MAKE-DISPATCH-MACRO-CHARACTER"
    }
    fn description(&self) -> &str {
        "Make character a dispatch macro character"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MAKE-DISPATCH-MACRO-CHARACTER".to_string(),
                reason: "Expected character argument".to_string(),
            });
        }
        Ok(Value::Bool(true))
    }
}

// ============================================================
// READER VARIABLES
// ============================================================

/// *READ-BASE* - Input radix
pub struct ReadBaseTool;
impl Tool for ReadBaseTool {
    fn name(&self) -> &str {
        "*READ-BASE*"
    }
    fn description(&self) -> &str {
        "Radix for reading integers"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Int(10)
        } else {
            args[0].clone()
        })
    }
}

/// *READ-DEFAULT-FLOAT-FORMAT* - Default float format
pub struct ReadDefaultFloatFormatTool;
impl Tool for ReadDefaultFloatFormatTool {
    fn name(&self) -> &str {
        "*READ-DEFAULT-FLOAT-FORMAT*"
    }
    fn description(&self) -> &str {
        "Default float type for reading"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::String("SINGLE-FLOAT".to_string())
        } else {
            args[0].clone()
        })
    }
}

/// *READ-EVAL* - Allow #. reader macro
pub struct ReadEvalTool;
impl Tool for ReadEvalTool {
    fn name(&self) -> &str {
        "*READ-EVAL*"
    }
    fn description(&self) -> &str {
        "Allow read-time evaluation"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Bool(true)
        } else {
            args[0].clone()
        })
    }
}

/// *READ-SUPPRESS* - Suppress reading
pub struct ReadSuppressTool;
impl Tool for ReadSuppressTool {
    fn name(&self) -> &str {
        "*READ-SUPPRESS*"
    }
    fn description(&self) -> &str {
        "Suppress reading and return NIL"
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
// READTABLE OPERATIONS
// ============================================================

/// COPY-READTABLE - Copy readtable
pub struct CopyReadtableTool;
impl Tool for CopyReadtableTool {
    fn name(&self) -> &str {
        "COPY-READTABLE"
    }
    fn description(&self) -> &str {
        "Copy readtable"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation - should accept optional from-readtable and to-readtable args
                      // Return an Arc-wrapped object representing the readtable
        let readtable = std::collections::HashMap::new();
        Ok(Value::Object(Arc::new(readtable)))
    }
}

/// READTABLEP - Check if readtable
pub struct ReadtablepTool;
impl Tool for ReadtablepTool {
    fn name(&self) -> &str {
        "READTABLEP"
    }
    fn description(&self) -> &str {
        "Check if object is readtable"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "READTABLEP".to_string(),
                reason: "Expected object argument".to_string(),
            });
        }
        // Check if argument is an object (readtables are represented as objects)
        Ok(Value::Bool(matches!(args[0], Value::Object(_))))
    }
}

/// READTABLE-CASE - Get/set readtable case
pub struct ReadtableCaseTool;
impl Tool for ReadtableCaseTool {
    fn name(&self) -> &str {
        "READTABLE-CASE"
    }
    fn description(&self) -> &str {
        "Get or set readtable case mode"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::String(":UPCASE".to_string())
        } else if args.len() == 1 {
            Value::String(":UPCASE".to_string())
        } else {
            args[1].clone()
        })
    }
}

/// Register all reader control functions
pub fn register(registry: &mut ToolRegistry) {
    // Read-time evaluation
    registry.register(ReadTimeEvalTool);
    registry.register(LoadTimeEvalTool);

    // Reader dispatch
    registry.register(GetDispatchMacroCharacterTool);
    registry.register(SetDispatchMacroCharacterTool);
    registry.register(MakeDispatchMacroCharacterTool);

    // Reader variables
    registry.register(ReadBaseTool);
    registry.register(ReadDefaultFloatFormatTool);
    registry.register(ReadEvalTool);
    registry.register(ReadSuppressTool);

    // Readtable operations
    registry.register(CopyReadtableTool);
    registry.register(ReadtablepTool);
    registry.register(ReadtableCaseTool);
}
