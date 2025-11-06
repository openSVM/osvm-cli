//! Documentation system for OVSM
//!
//! Documentation strings and introspection.
//! Provides Common Lisp DOCUMENTATION accessor system.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

// Documentation functions (5 total)

// ============================================================
// DOCUMENTATION STRINGS
// ============================================================

/// DOCUMENTATION - Get documentation string
pub struct DocumentationTool;
impl Tool for DocumentationTool {
    fn name(&self) -> &str {
        "DOCUMENTATION"
    }
    fn description(&self) -> &str {
        "Get documentation string for object"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "DOCUMENTATION".to_string(),
                reason: "Expected 2 arguments: object and doc-type".to_string(),
            });
        }

        // Validate doc-type is a string
        if !matches!(args[1], Value::String(_)) {
            return Err(Error::InvalidArguments {
                tool: "DOCUMENTATION".to_string(),
                reason: "doc-type (arg 2) must be a string (FUNCTION, VARIABLE, TYPE, etc.)"
                    .to_string(),
            });
        }

        // args[0] is the object, args[1] is the doc-type
        // doc-type can be: FUNCTION, VARIABLE, TYPE, STRUCTURE, SETF, etc.
        Ok(Value::String("No documentation available.".to_string()))
    }
}

/// SET-DOCUMENTATION - Set documentation string
pub struct SetDocumentationTool;
impl Tool for SetDocumentationTool {
    fn name(&self) -> &str {
        "SET-DOCUMENTATION"
    }
    fn description(&self) -> &str {
        "Set documentation string for object"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "SET-DOCUMENTATION".to_string(),
                reason: "Expected 3 arguments: object, doc-type, and documentation string"
                    .to_string(),
            });
        }

        // Validate doc-type is a string
        if !matches!(args[1], Value::String(_)) {
            return Err(Error::InvalidArguments {
                tool: "SET-DOCUMENTATION".to_string(),
                reason: "doc-type (arg 2) must be a string".to_string(),
            });
        }

        // Validate documentation is a string
        if !matches!(args[2], Value::String(_)) {
            return Err(Error::InvalidArguments {
                tool: "SET-DOCUMENTATION".to_string(),
                reason: "documentation (arg 3) must be a string".to_string(),
            });
        }

        // args[0] is object, args[1] is doc-type, args[2] is new doc string
        Ok(args[2].clone())
    }
}

/// FUNCTION-DOCUMENTATION - Get function documentation
pub struct FunctionDocumentationTool;
impl Tool for FunctionDocumentationTool {
    fn name(&self) -> &str {
        "FUNCTION-DOCUMENTATION"
    }
    fn description(&self) -> &str {
        "Get function documentation"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FUNCTION-DOCUMENTATION".to_string(),
                reason: "Expected 1 argument: function name".to_string(),
            });
        }

        // Validate function name is a string
        if !matches!(args[0], Value::String(_)) {
            return Err(Error::InvalidArguments {
                tool: "FUNCTION-DOCUMENTATION".to_string(),
                reason: "function name must be a string".to_string(),
            });
        }

        Ok(Value::String("No documentation available.".to_string()))
    }
}

/// VARIABLE-DOCUMENTATION - Get variable documentation
pub struct VariableDocumentationTool;
impl Tool for VariableDocumentationTool {
    fn name(&self) -> &str {
        "VARIABLE-DOCUMENTATION"
    }
    fn description(&self) -> &str {
        "Get variable documentation"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "VARIABLE-DOCUMENTATION".to_string(),
                reason: "Expected 1 argument: variable name".to_string(),
            });
        }

        // Validate variable name is a string
        if !matches!(args[0], Value::String(_)) {
            return Err(Error::InvalidArguments {
                tool: "VARIABLE-DOCUMENTATION".to_string(),
                reason: "variable name must be a string".to_string(),
            });
        }

        Ok(Value::String("No documentation available.".to_string()))
    }
}

/// TYPE-DOCUMENTATION - Get type documentation
pub struct TypeDocumentationTool;
impl Tool for TypeDocumentationTool {
    fn name(&self) -> &str {
        "TYPE-DOCUMENTATION"
    }
    fn description(&self) -> &str {
        "Get type documentation"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "TYPE-DOCUMENTATION".to_string(),
                reason: "Expected 1 argument: type name".to_string(),
            });
        }

        // Validate type name is a string
        if !matches!(args[0], Value::String(_)) {
            return Err(Error::InvalidArguments {
                tool: "TYPE-DOCUMENTATION".to_string(),
                reason: "type name must be a string".to_string(),
            });
        }

        Ok(Value::String("No documentation available.".to_string()))
    }
}

/// Register all documentation functions
pub fn register(registry: &mut ToolRegistry) {
    registry.register(DocumentationTool);
    registry.register(SetDocumentationTool);
    registry.register(FunctionDocumentationTool);
    registry.register(VariableDocumentationTool);
    registry.register(TypeDocumentationTool);
}
