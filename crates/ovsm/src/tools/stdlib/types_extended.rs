//! Extended type system for OVSM
//!
//! Type definitions, type checking, coercion, and compound types.
//! Provides Common Lisp-style advanced type system capabilities.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// Extended type system functions (20 total)

// ============================================================
// TYPE DEFINITIONS
// ============================================================

/// DEFTYPE - Define new type
pub struct DeftypeTool;
impl Tool for DeftypeTool {
    fn name(&self) -> &str {
        "DEFTYPE"
    }
    fn description(&self) -> &str {
        "Define new type"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// TYPE-OF - Get type of value
pub struct TypeOfTool;
impl Tool for TypeOfTool {
    fn name(&self) -> &str {
        "TYPE-OF"
    }
    fn description(&self) -> &str {
        "Get type of value"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::String("NULL".to_string()));
        }
        let type_name = match &args[0] {
            Value::Null => "NULL",
            Value::Bool(_) => "BOOLEAN",
            Value::Int(_) => "INTEGER",
            Value::Float(_) => "FLOAT",
            Value::String(_) => "STRING",
            Value::Array(_) => "ARRAY",
            Value::Object(_) => "OBJECT",
            _ => "T",
        };
        Ok(Value::String(type_name.to_string()))
    }
}

/// TYPEP - Check if value is of type
pub struct TypepTool;
impl Tool for TypepTool {
    fn name(&self) -> &str {
        "TYPEP"
    }
    fn description(&self) -> &str {
        "Check if value is of specified type"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "TYPEP requires value and type".to_string(),
            });
        }
        let value = &args[0];
        let type_spec = match &args[1] {
            Value::String(s) => s.to_uppercase(),
            _ => return Ok(Value::Bool(false)),
        };

        let matches = match type_spec.as_str() {
            "NULL" => matches!(value, Value::Null),
            "BOOLEAN" | "BOOL" => matches!(value, Value::Bool(_)),
            "INTEGER" | "INT" => matches!(value, Value::Int(_)),
            "FLOAT" => matches!(value, Value::Float(_)),
            "NUMBER" => matches!(value, Value::Int(_) | Value::Float(_)),
            "STRING" => matches!(value, Value::String(_)),
            "ARRAY" | "LIST" => matches!(value, Value::Array(_)),
            "OBJECT" => matches!(value, Value::Object(_)),
            "T" => true,
            _ => false,
        };
        Ok(Value::Bool(matches))
    }
}

/// SUBTYPEP - Check subtype relationship
pub struct SubtypepTool;
impl Tool for SubtypepTool {
    fn name(&self) -> &str {
        "SUBTYPEP"
    }
    fn description(&self) -> &str {
        "Check if type1 is subtype of type2"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Bool(false));
        }
        // Simplified: check if types are equal
        Ok(Value::Bool(args[0] == args[1]))
    }
}

/// COERCE - Coerce value to type
pub struct CoerceTool;
impl Tool for CoerceTool {
    fn name(&self) -> &str {
        "COERCE"
    }
    fn description(&self) -> &str {
        "Coerce value to specified type"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "COERCE requires value and type".to_string(),
            });
        }
        let value = &args[0];
        let type_spec = match &args[1] {
            Value::String(s) => s.to_uppercase(),
            _ => return Ok(value.clone()),
        };

        match type_spec.as_str() {
            "INTEGER" | "INT" => match value {
                Value::Int(n) => Ok(Value::Int(*n)),
                Value::Float(f) => Ok(Value::Int(*f as i64)),
                Value::String(s) => {
                    s.parse::<i64>()
                        .map(Value::Int)
                        .map_err(|_| Error::TypeError {
                            expected: "valid argument".to_string(),
                            got: "invalid".to_string(),
                        })
                }
                _ => Err(Error::TypeError {
                    expected: "valid argument".to_string(),
                    got: "invalid".to_string(),
                }),
            },
            "FLOAT" => match value {
                Value::Float(f) => Ok(Value::Float(*f)),
                Value::Int(n) => Ok(Value::Float(*n as f64)),
                Value::String(s) => {
                    s.parse::<f64>()
                        .map(Value::Float)
                        .map_err(|_| Error::TypeError {
                            expected: "valid argument".to_string(),
                            got: "invalid".to_string(),
                        })
                }
                _ => Err(Error::TypeError {
                    expected: "valid argument".to_string(),
                    got: "invalid".to_string(),
                }),
            },
            "STRING" => Ok(Value::String(
                (match value {
                    Value::String(s) => s.clone(),
                    Value::Int(n) => n.to_string(),
                    Value::Float(f) => f.to_string(),
                    Value::Bool(b) => b.to_string(),
                    Value::Null => "null".to_string(),
                    _ => "?".to_string(),
                })
                .to_string(),
            )),
            _ => Ok(value.clone()),
        }
    }
}

// ============================================================
// TYPE SPECIFIERS
// ============================================================

/// SATISFIES - Type satisfying predicate
pub struct SatisfiesTool;
impl Tool for SatisfiesTool {
    fn name(&self) -> &str {
        "SATISFIES"
    }
    fn description(&self) -> &str {
        "Type satisfying predicate"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Bool(true)
        } else {
            args[0].clone()
        })
    }
}

/// MEMBER - Member type specifier
pub struct MemberTypeTool;
impl Tool for MemberTypeTool {
    fn name(&self) -> &str {
        "MEMBER"
    }
    fn description(&self) -> &str {
        "Member type specifier"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(args.to_vec())))
    }
}

/// AND - Intersection type
pub struct AndTypeTool;
impl Tool for AndTypeTool {
    fn name(&self) -> &str {
        "AND"
    }
    fn description(&self) -> &str {
        "Intersection type specifier"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Bool(true)
        } else {
            Value::Bool(args.iter().all(|v| v.is_truthy()))
        })
    }
}

/// OR - Union type
pub struct OrTypeTool;
impl Tool for OrTypeTool {
    fn name(&self) -> &str {
        "OR"
    }
    fn description(&self) -> &str {
        "Union type specifier"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Bool(false)
        } else {
            Value::Bool(args.iter().any(|v| v.is_truthy()))
        })
    }
}

/// NOT - Complement type
pub struct NotTypeTool;
impl Tool for NotTypeTool {
    fn name(&self) -> &str {
        "NOT"
    }
    fn description(&self) -> &str {
        "Complement type specifier"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Bool(true)
        } else {
            Value::Bool(!args[0].is_truthy())
        })
    }
}

/// VALUES - Multiple values type
pub struct ValuesTool;
impl Tool for ValuesTool {
    fn name(&self) -> &str {
        "VALUES"
    }
    fn description(&self) -> &str {
        "Multiple values type specifier"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(args.to_vec())))
    }
}

/// EQL - EQL type specifier
pub struct EqlTypeTool;
impl Tool for EqlTypeTool {
    fn name(&self) -> &str {
        "EQL"
    }
    fn description(&self) -> &str {
        "EQL type specifier"
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
// NUMERIC TYPES
// ============================================================

/// INTEGER-TYPE - Integer type with range
pub struct IntegerTypeTool;
impl Tool for IntegerTypeTool {
    fn name(&self) -> &str {
        "INTEGER-TYPE"
    }
    fn description(&self) -> &str {
        "Integer type with optional range"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation - should accept optional min and max
        Ok(Value::String("INTEGER".to_string()))
    }
}

/// FLOAT-TYPE - Float type with range
pub struct FloatTypeTool;
impl Tool for FloatTypeTool {
    fn name(&self) -> &str {
        "FLOAT-TYPE"
    }
    fn description(&self) -> &str {
        "Float type with optional range"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation - should accept optional min and max
        Ok(Value::String("FLOAT".to_string()))
    }
}

/// RATIONAL-TYPE - Rational number type
pub struct RationalTypeTool;
impl Tool for RationalTypeTool {
    fn name(&self) -> &str {
        "RATIONAL-TYPE"
    }
    fn description(&self) -> &str {
        "Rational number type"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation
        Ok(Value::String("RATIONAL".to_string()))
    }
}

/// REAL-TYPE - Real number type
pub struct RealTypeTool;
impl Tool for RealTypeTool {
    fn name(&self) -> &str {
        "REAL-TYPE"
    }
    fn description(&self) -> &str {
        "Real number type"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation
        Ok(Value::String("REAL".to_string()))
    }
}

/// COMPLEX-TYPE - Complex number type
pub struct ComplexTypeTool;
impl Tool for ComplexTypeTool {
    fn name(&self) -> &str {
        "COMPLEX-TYPE"
    }
    fn description(&self) -> &str {
        "Complex number type"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation
        Ok(Value::String("COMPLEX".to_string()))
    }
}

// ============================================================
// SEQUENCE TYPES
// ============================================================

/// ARRAY-TYPE - Array type with dimensions
pub struct ArrayTypeTool;
impl Tool for ArrayTypeTool {
    fn name(&self) -> &str {
        "ARRAY-TYPE"
    }
    fn description(&self) -> &str {
        "Array type with optional dimensions"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation - should accept optional dimensions
        Ok(Value::String("ARRAY".to_string()))
    }
}

/// SIMPLE-ARRAY-TYPE - Simple array type
pub struct SimpleArrayTypeTool;
impl Tool for SimpleArrayTypeTool {
    fn name(&self) -> &str {
        "SIMPLE-ARRAY-TYPE"
    }
    fn description(&self) -> &str {
        "Simple array type"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation
        Ok(Value::String("SIMPLE-ARRAY".to_string()))
    }
}

/// VECTOR-TYPE - Vector type
pub struct VectorTypeTool;
impl Tool for VectorTypeTool {
    fn name(&self) -> &str {
        "VECTOR-TYPE"
    }
    fn description(&self) -> &str {
        "Vector type"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation
        Ok(Value::String("VECTOR".to_string()))
    }
}

/// Register all extended type system functions
pub fn register(registry: &mut ToolRegistry) {
    // Type definitions
    registry.register(DeftypeTool);
    registry.register(TypeOfTool);
    registry.register(TypepTool);
    registry.register(SubtypepTool);
    registry.register(CoerceTool);

    // Type specifiers
    registry.register(SatisfiesTool);
    registry.register(MemberTypeTool);
    registry.register(AndTypeTool);
    registry.register(OrTypeTool);
    registry.register(NotTypeTool);
    registry.register(ValuesTool);
    registry.register(EqlTypeTool);

    // Numeric types
    registry.register(IntegerTypeTool);
    registry.register(FloatTypeTool);
    registry.register(RationalTypeTool);
    registry.register(RealTypeTool);
    registry.register(ComplexTypeTool);

    // Sequence types
    registry.register(ArrayTypeTool);
    registry.register(SimpleArrayTypeTool);
    registry.register(VectorTypeTool);
}
