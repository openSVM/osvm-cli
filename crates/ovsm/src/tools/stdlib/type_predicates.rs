//! Type predicate tools - Common Lisp compatible type checking functions

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

/// Register all type predicate tools
pub fn register(registry: &mut ToolRegistry) {
    // Basic type predicates (already in utilities.rs, but adding Common Lisp names)
    registry.register(NumberpTool);
    registry.register(IntegerpTool);
    registry.register(FloatpTool);
    registry.register(StringpTool);
    registry.register(SymbolpTool);
    registry.register(KeywordpTool);
    registry.register(ConspTool);
    registry.register(AtomTool);
    registry.register(ListpTool);
    registry.register(NullTool);
    registry.register(ArraypTool);
    registry.register(VectorpTool);
    registry.register(SimpleVectorpTool);
    registry.register(BooleanpTool);
    registry.register(FunctionpTool);
    registry.register(MacropTool);
    registry.register(HashTablepTool);

    // Numeric type predicates
    registry.register(ZeropTool);
    registry.register(PluspTool);
    registry.register(MinuspTool);
    registry.register(EvenpTool);
    registry.register(OddpTool);

    // Comparison predicates
    registry.register(EqlTool);
    registry.register(EqualTool);
    registry.register(EqualpTool);

    // Object/collection predicates
    registry.register(EmptyTool);
}

// ============================================================================
// Basic Type Predicates
// ============================================================================

/// NUMBERP - Check if value is a number (int or float)
pub struct NumberpTool;

impl Tool for NumberpTool {
    fn name(&self) -> &str {
        "NUMBERP"
    }

    fn description(&self) -> &str {
        "Check if value is a number (Common Lisp)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(
            &args[0],
            Value::Int(_) | Value::Float(_)
        )))
    }
}

/// INTEGERP - Check if value is an integer
pub struct IntegerpTool;

impl Tool for IntegerpTool {
    fn name(&self) -> &str {
        "INTEGERP"
    }

    fn description(&self) -> &str {
        "Check if value is an integer"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Int(_))))
    }
}

/// FLOATP - Check if value is a float
pub struct FloatpTool;

impl Tool for FloatpTool {
    fn name(&self) -> &str {
        "FLOATP"
    }

    fn description(&self) -> &str {
        "Check if value is a float"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Float(_))))
    }
}

/// STRINGP - Check if value is a string
pub struct StringpTool;

impl Tool for StringpTool {
    fn name(&self) -> &str {
        "STRINGP"
    }

    fn description(&self) -> &str {
        "Check if value is a string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::String(_))))
    }
}

/// SYMBOLP - Check if value is a symbol (in OVSM, this checks for string identifiers)
pub struct SymbolpTool;

impl Tool for SymbolpTool {
    fn name(&self) -> &str {
        "SYMBOLP"
    }

    fn description(&self) -> &str {
        "Check if value is a symbol"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        // In OVSM, symbols are represented as strings
        Ok(Value::Bool(matches!(&args[0], Value::String(_))))
    }
}

/// KEYWORDP - Check if value is a keyword (starts with :)
pub struct KeywordpTool;

impl Tool for KeywordpTool {
    fn name(&self) -> &str {
        "KEYWORDP"
    }

    fn description(&self) -> &str {
        "Check if value is a keyword"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        match &args[0] {
            Value::String(s) => Ok(Value::Bool(s.starts_with(':'))),
            _ => Ok(Value::Bool(false)),
        }
    }
}

/// CONSP - Check if value is a cons (non-empty list in OVSM)
pub struct ConspTool;

impl Tool for ConspTool {
    fn name(&self) -> &str {
        "CONSP"
    }

    fn description(&self) -> &str {
        "Check if value is a cons cell (non-empty list)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        match &args[0] {
            Value::Array(arr) => Ok(Value::Bool(!arr.is_empty())),
            _ => Ok(Value::Bool(false)),
        }
    }
}

/// ATOM - Check if value is an atom (not a cons)
pub struct AtomTool;

impl Tool for AtomTool {
    fn name(&self) -> &str {
        "ATOM"
    }

    fn description(&self) -> &str {
        "Check if value is an atom (not a cons cell)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(true)); // nil is an atom
        }
        match &args[0] {
            Value::Array(arr) if !arr.is_empty() => Ok(Value::Bool(false)),
            Value::Null => Ok(Value::Bool(true)),
            _ => Ok(Value::Bool(true)),
        }
    }
}

/// LISTP - Check if value is a list (array or null)
pub struct ListpTool;

impl Tool for ListpTool {
    fn name(&self) -> &str {
        "LISTP"
    }

    fn description(&self) -> &str {
        "Check if value is a list"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(true)); // nil is a list
        }
        Ok(Value::Bool(matches!(
            &args[0],
            Value::Array(_) | Value::Null
        )))
    }
}

/// NULL - Check if value is null
pub struct NullTool;

impl Tool for NullTool {
    fn name(&self) -> &str {
        "NULL"
    }

    fn description(&self) -> &str {
        "Check if value is null (nil)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(true));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Null)))
    }
}

/// ARRAYP - Check if value is an array
pub struct ArraypTool;

impl Tool for ArraypTool {
    fn name(&self) -> &str {
        "ARRAYP"
    }

    fn description(&self) -> &str {
        "Check if value is an array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Array(_))))
    }
}

/// VECTORP - Check if value is a vector (same as array in OVSM)
pub struct VectorpTool;

impl Tool for VectorpTool {
    fn name(&self) -> &str {
        "VECTORP"
    }

    fn description(&self) -> &str {
        "Check if value is a vector"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Array(_))))
    }
}

/// SIMPLE-VECTOR-P - Check if value is a simple vector
pub struct SimpleVectorpTool;

impl Tool for SimpleVectorpTool {
    fn name(&self) -> &str {
        "SIMPLE-VECTOR-P"
    }

    fn description(&self) -> &str {
        "Check if value is a simple vector"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Array(_))))
    }
}

/// BOOLEANP - Check if value is a boolean
pub struct BooleanpTool;

impl Tool for BooleanpTool {
    fn name(&self) -> &str {
        "BOOLEANP"
    }

    fn description(&self) -> &str {
        "Check if value is a boolean"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Bool(_))))
    }
}

/// FUNCTIONP - Check if value is a function
pub struct FunctionpTool;

impl Tool for FunctionpTool {
    fn name(&self) -> &str {
        "FUNCTIONP"
    }

    fn description(&self) -> &str {
        "Check if value is a function"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Function { .. })))
    }
}

/// MACROP - Check if value is a macro
pub struct MacropTool;

impl Tool for MacropTool {
    fn name(&self) -> &str {
        "MACROP"
    }

    fn description(&self) -> &str {
        "Check if value is a macro"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Macro { .. })))
    }
}

/// HASH-TABLE-P - Check if value is a hash table (object in OVSM)
pub struct HashTablepTool;

impl Tool for HashTablepTool {
    fn name(&self) -> &str {
        "HASH-TABLE-P"
    }

    fn description(&self) -> &str {
        "Check if value is a hash table"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Object(_))))
    }
}

// ============================================================================
// Numeric Type Predicates
// ============================================================================

/// ZEROP - Check if number is zero
pub struct ZeropTool;

impl Tool for ZeropTool {
    fn name(&self) -> &str {
        "ZEROP"
    }

    fn description(&self) -> &str {
        "Check if number is zero"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ZEROP".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        match &args[0] {
            Value::Int(n) => Ok(Value::Bool(*n == 0)),
            Value::Float(f) => Ok(Value::Bool(*f == 0.0)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// PLUSP - Check if number is positive
pub struct PluspTool;

impl Tool for PluspTool {
    fn name(&self) -> &str {
        "PLUSP"
    }

    fn description(&self) -> &str {
        "Check if number is positive (> 0)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PLUSP".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        match &args[0] {
            Value::Int(n) => Ok(Value::Bool(*n > 0)),
            Value::Float(f) => Ok(Value::Bool(*f > 0.0)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// MINUSP - Check if number is negative
pub struct MinuspTool;

impl Tool for MinuspTool {
    fn name(&self) -> &str {
        "MINUSP"
    }

    fn description(&self) -> &str {
        "Check if number is negative (< 0)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MINUSP".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        match &args[0] {
            Value::Int(n) => Ok(Value::Bool(*n < 0)),
            Value::Float(f) => Ok(Value::Bool(*f < 0.0)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// EVENP - Check if integer is even
pub struct EvenpTool;

impl Tool for EvenpTool {
    fn name(&self) -> &str {
        "EVENP"
    }

    fn description(&self) -> &str {
        "Check if integer is even"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "EVENP".to_string(),
                reason: "Expected integer argument".to_string(),
            });
        }

        match &args[0] {
            Value::Int(n) => Ok(Value::Bool(n % 2 == 0)),
            _ => Err(Error::TypeError {
                expected: "integer".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// ODDP - Check if integer is odd
pub struct OddpTool;

impl Tool for OddpTool {
    fn name(&self) -> &str {
        "ODDP"
    }

    fn description(&self) -> &str {
        "Check if integer is odd"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ODDP".to_string(),
                reason: "Expected integer argument".to_string(),
            });
        }

        match &args[0] {
            Value::Int(n) => Ok(Value::Bool(n % 2 != 0)),
            _ => Err(Error::TypeError {
                expected: "integer".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

// ============================================================================
// Comparison Predicates
// ============================================================================

/// EQL - Check if two values are the same object
pub struct EqlTool;

impl Tool for EqlTool {
    fn name(&self) -> &str {
        "EQL"
    }

    fn description(&self) -> &str {
        "Check if two values are eql (same value)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "EQL".to_string(),
                reason: "Expected 2 arguments".to_string(),
            });
        }

        Ok(Value::Bool(args[0] == args[1]))
    }
}

/// EQUAL - Check if two values are structurally equal
pub struct EqualTool;

impl Tool for EqualTool {
    fn name(&self) -> &str {
        "EQUAL"
    }

    fn description(&self) -> &str {
        "Check if two values are equal (deep comparison)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "EQUAL".to_string(),
                reason: "Expected 2 arguments".to_string(),
            });
        }

        Ok(Value::Bool(args[0] == args[1]))
    }
}

/// EQUALP - Check if two values are equal (case-insensitive for strings)
pub struct EqualpTool;

impl Tool for EqualpTool {
    fn name(&self) -> &str {
        "EQUALP"
    }

    fn description(&self) -> &str {
        "Check if two values are equal (case-insensitive)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "EQUALP".to_string(),
                reason: "Expected 2 arguments".to_string(),
            });
        }

        let result = match (&args[0], &args[1]) {
            (Value::String(s1), Value::String(s2)) => s1.to_lowercase() == s2.to_lowercase(),
            _ => args[0] == args[1],
        };

        Ok(Value::Bool(result))
    }
}

// ============================================================================
// Collection Predicates
// ============================================================================

/// EMPTY - Check if collection is empty
pub struct EmptyTool;

impl Tool for EmptyTool {
    fn name(&self) -> &str {
        "EMPTY"
    }

    fn description(&self) -> &str {
        "Check if collection is empty"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(true));
        }

        let is_empty = match &args[0] {
            Value::Array(arr) => arr.is_empty(),
            Value::String(s) => s.is_empty(),
            Value::Object(obj) => obj.is_empty(),
            Value::Null => true,
            _ => false,
        };

        Ok(Value::Bool(is_empty))
    }
}
