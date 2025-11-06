//! Numeric comparison and conversion tools - Common Lisp compatible

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

/// Register all numeric tools
pub fn register(registry: &mut ToolRegistry) {
    // Numeric comparisons (variadic)
    registry.register(NumEqualTool);
    registry.register(NumNotEqualTool);
    registry.register(NumLessTool);
    registry.register(NumLessEqualTool);
    registry.register(NumGreaterTool);
    registry.register(NumGreaterEqualTool);

    // Min/Max (already in statistics, but add Common Lisp names)
    registry.register(MinimumTool);
    registry.register(MaximumTool);

    // Conversion functions
    registry.register(FloatTool);
    registry.register(FloorTool);
    registry.register(CeilingTool);
    registry.register(RoundTool);
    registry.register(RationalizeTool);
    registry.register(CoerceTool);

    // Parse numbers
    registry.register(ParseNumberTool);
    registry.register(ReadFromStringTool);

    // Number to string
    registry.register(WriteToStringTool);
    registry.register(PrincToStringTool);

    // Special values
    registry.register(IncfTool);
    registry.register(DecfTool);
    registry.register(OnePlusTool);
    registry.register(OneMinusTool);

    // Reciprocal and negation
    registry.register(ReciprocalTool);
    registry.register(NegateTool);
}

// ============================================================================
// Numeric Comparisons
// ============================================================================

/// = - Numeric equality (variadic)
pub struct NumEqualTool;

impl Tool for NumEqualTool {
    fn name(&self) -> &str {
        "="
    }

    fn description(&self) -> &str {
        "Check if all numbers are equal"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "=".to_string(),
                reason: "Expected at least 2 arguments".to_string(),
            });
        }

        let first = args[0].as_float()?;
        for arg in &args[1..] {
            if arg.as_float()? != first {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    }
}

/// /= - Numeric inequality (all different)
pub struct NumNotEqualTool;

impl Tool for NumNotEqualTool {
    fn name(&self) -> &str {
        "/="
    }

    fn description(&self) -> &str {
        "Check if all numbers are different"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "/=".to_string(),
                reason: "Expected at least 2 arguments".to_string(),
            });
        }

        let nums: Result<Vec<f64>> = args.iter().map(|v| v.as_float()).collect();
        let nums = nums?;

        for i in 0..nums.len() {
            for j in (i + 1)..nums.len() {
                if nums[i] == nums[j] {
                    return Ok(Value::Bool(false));
                }
            }
        }
        Ok(Value::Bool(true))
    }
}

/// < - Numeric less than (variadic, monotonic)
pub struct NumLessTool;

impl Tool for NumLessTool {
    fn name(&self) -> &str {
        "<"
    }

    fn description(&self) -> &str {
        "Check if numbers are in strictly increasing order"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "<".to_string(),
                reason: "Expected at least 2 arguments".to_string(),
            });
        }

        let mut prev = args[0].as_float()?;
        for arg in &args[1..] {
            let curr = arg.as_float()?;
            if prev >= curr {
                return Ok(Value::Bool(false));
            }
            prev = curr;
        }
        Ok(Value::Bool(true))
    }
}

/// <= - Numeric less than or equal (variadic, monotonic)
pub struct NumLessEqualTool;

impl Tool for NumLessEqualTool {
    fn name(&self) -> &str {
        "<="
    }

    fn description(&self) -> &str {
        "Check if numbers are in non-decreasing order"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "<=".to_string(),
                reason: "Expected at least 2 arguments".to_string(),
            });
        }

        let mut prev = args[0].as_float()?;
        for arg in &args[1..] {
            let curr = arg.as_float()?;
            if prev > curr {
                return Ok(Value::Bool(false));
            }
            prev = curr;
        }
        Ok(Value::Bool(true))
    }
}

/// > - Numeric greater than (variadic, monotonic)
pub struct NumGreaterTool;

impl Tool for NumGreaterTool {
    fn name(&self) -> &str {
        ">"
    }

    fn description(&self) -> &str {
        "Check if numbers are in strictly decreasing order"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: ">".to_string(),
                reason: "Expected at least 2 arguments".to_string(),
            });
        }

        let mut prev = args[0].as_float()?;
        for arg in &args[1..] {
            let curr = arg.as_float()?;
            if prev <= curr {
                return Ok(Value::Bool(false));
            }
            prev = curr;
        }
        Ok(Value::Bool(true))
    }
}

/// >= - Numeric greater than or equal (variadic, monotonic)
pub struct NumGreaterEqualTool;

impl Tool for NumGreaterEqualTool {
    fn name(&self) -> &str {
        ">="
    }

    fn description(&self) -> &str {
        "Check if numbers are in non-increasing order"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: ">=".to_string(),
                reason: "Expected at least 2 arguments".to_string(),
            });
        }

        let mut prev = args[0].as_float()?;
        for arg in &args[1..] {
            let curr = arg.as_float()?;
            if prev < curr {
                return Ok(Value::Bool(false));
            }
            prev = curr;
        }
        Ok(Value::Bool(true))
    }
}

// ============================================================================
// Min/Max
// ============================================================================

/// MINIMUM - Return smallest value (Common Lisp alias for MIN)
pub struct MinimumTool;

impl Tool for MinimumTool {
    fn name(&self) -> &str {
        "MINIMUM"
    }

    fn description(&self) -> &str {
        "Return the smallest value (alias for MIN)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MINIMUM".to_string(),
                reason: "Expected at least one argument".to_string(),
            });
        }

        let mut min_val = args[0].as_float()?;
        for arg in &args[1..] {
            let val = arg.as_float()?;
            if val < min_val {
                min_val = val;
            }
        }

        // Return int if all inputs were ints
        let all_ints = args.iter().all(|v| matches!(v, Value::Int(_)));
        if all_ints {
            Ok(Value::Int(min_val as i64))
        } else {
            Ok(Value::Float(min_val))
        }
    }
}

/// MAXIMUM - Return largest value (Common Lisp alias for MAX)
pub struct MaximumTool;

impl Tool for MaximumTool {
    fn name(&self) -> &str {
        "MAXIMUM"
    }

    fn description(&self) -> &str {
        "Return the largest value (alias for MAX)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MAXIMUM".to_string(),
                reason: "Expected at least one argument".to_string(),
            });
        }

        let mut max_val = args[0].as_float()?;
        for arg in &args[1..] {
            let val = arg.as_float()?;
            if val > max_val {
                max_val = val;
            }
        }

        // Return int if all inputs were ints
        let all_ints = args.iter().all(|v| matches!(v, Value::Int(_)));
        if all_ints {
            Ok(Value::Int(max_val as i64))
        } else {
            Ok(Value::Float(max_val))
        }
    }
}

// ============================================================================
// Conversion Functions
// ============================================================================

/// FLOAT - Convert to float
pub struct FloatTool;

impl Tool for FloatTool {
    fn name(&self) -> &str {
        "FLOAT"
    }

    fn description(&self) -> &str {
        "Convert number to float"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FLOAT".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let val = args[0].as_float()?;
        Ok(Value::Float(val))
    }
}

/// FLOOR - Floor and remainder
pub struct FloorTool;

impl Tool for FloorTool {
    fn name(&self) -> &str {
        "FLOOR"
    }

    fn description(&self) -> &str {
        "Return floor of number (as integer)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FLOOR".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let val = args[0].as_float()?;
        Ok(Value::Int(val.floor() as i64))
    }
}

/// CEILING - Ceiling function
pub struct CeilingTool;

impl Tool for CeilingTool {
    fn name(&self) -> &str {
        "CEILING"
    }

    fn description(&self) -> &str {
        "Return ceiling of number (as integer)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CEILING".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let val = args[0].as_float()?;
        Ok(Value::Int(val.ceil() as i64))
    }
}

/// ROUND - Round to nearest integer
pub struct RoundTool;

impl Tool for RoundTool {
    fn name(&self) -> &str {
        "ROUND"
    }

    fn description(&self) -> &str {
        "Round to nearest integer"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ROUND".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let val = args[0].as_float()?;
        Ok(Value::Int(val.round() as i64))
    }
}

/// RATIONALIZE - Convert to rational (returns float in OVSM)
pub struct RationalizeTool;

impl Tool for RationalizeTool {
    fn name(&self) -> &str {
        "RATIONALIZE"
    }

    fn description(&self) -> &str {
        "Convert to rational representation (returns value in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "RATIONALIZE".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        Ok(args[0].clone())
    }
}

/// COERCE - Type coercion
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
                tool: "COERCE".to_string(),
                reason: "Expected value and type".to_string(),
            });
        }

        let target_type = args[1].as_string()?;

        match target_type.to_uppercase().as_str() {
            "FLOAT" => Ok(Value::Float(args[0].as_float()?)),
            "INTEGER" => Ok(Value::Int(args[0].as_int()?)),
            "STRING" => Ok(Value::String(args[0].to_string_value())),
            "LIST" | "ARRAY" => match &args[0] {
                Value::Array(_) => Ok(args[0].clone()),
                Value::String(s) => {
                    let chars: Vec<Value> =
                        s.chars().map(|c| Value::String(c.to_string())).collect();
                    Ok(Value::array(chars))
                }
                _ => Err(Error::TypeError {
                    expected: "coercible to array".to_string(),
                    got: args[0].type_name(),
                }),
            },
            _ => Err(Error::InvalidArguments {
                tool: "COERCE".to_string(),
                reason: format!("Unknown type: {}", target_type),
            }),
        }
    }
}

// ============================================================================
// Parse Numbers
// ============================================================================

/// PARSE-NUMBER - Parse number from string
pub struct ParseNumberTool;

impl Tool for ParseNumberTool {
    fn name(&self) -> &str {
        "PARSE-NUMBER"
    }

    fn description(&self) -> &str {
        "Parse number from string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PARSE-NUMBER".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?.trim();

        // Try integer first
        if let Ok(i) = s.parse::<i64>() {
            return Ok(Value::Int(i));
        }

        // Try float
        if let Ok(f) = s.parse::<f64>() {
            return Ok(Value::Float(f));
        }

        Err(Error::InvalidArguments {
            tool: "PARSE-NUMBER".to_string(),
            reason: format!("Cannot parse '{}' as number", s),
        })
    }
}

/// READ-FROM-STRING - Read value from string
pub struct ReadFromStringTool;

impl Tool for ReadFromStringTool {
    fn name(&self) -> &str {
        "READ-FROM-STRING"
    }

    fn description(&self) -> &str {
        "Read/parse value from string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "READ-FROM-STRING".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;

        // Try parsing as JSON
        if let Ok(json_val) = serde_json::from_str::<serde_json::Value>(s) {
            // Convert JSON to OVSM value
            return json_to_value(&json_val);
        }

        // Otherwise return as string
        Ok(Value::String(s.to_string()))
    }
}

fn json_to_value(json: &serde_json::Value) -> Result<Value> {
    match json {
        serde_json::Value::Null => Ok(Value::Null),
        serde_json::Value::Bool(b) => Ok(Value::Bool(*b)),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(Value::Int(i))
            } else if let Some(f) = n.as_f64() {
                Ok(Value::Float(f))
            } else {
                Ok(Value::Null)
            }
        }
        serde_json::Value::String(s) => Ok(Value::String(s.clone())),
        serde_json::Value::Array(arr) => {
            let vals: Result<Vec<Value>> = arr.iter().map(json_to_value).collect();
            Ok(Value::array(vals?))
        }
        serde_json::Value::Object(obj) => {
            let mut map = std::collections::HashMap::new();
            for (k, v) in obj {
                map.insert(k.clone(), json_to_value(v)?);
            }
            Ok(Value::object(map))
        }
    }
}

// ============================================================================
// Number to String
// ============================================================================

/// WRITE-TO-STRING - Convert value to string representation
pub struct WriteToStringTool;

impl Tool for WriteToStringTool {
    fn name(&self) -> &str {
        "WRITE-TO-STRING"
    }

    fn description(&self) -> &str {
        "Convert value to string representation"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "WRITE-TO-STRING".to_string(),
                reason: "Expected value argument".to_string(),
            });
        }

        Ok(Value::String(args[0].to_string_value()))
    }
}

/// PRINC-TO-STRING - Convert value to string (no escape characters)
pub struct PrincToStringTool;

impl Tool for PrincToStringTool {
    fn name(&self) -> &str {
        "PRINC-TO-STRING"
    }

    fn description(&self) -> &str {
        "Convert value to string without escape characters"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PRINC-TO-STRING".to_string(),
                reason: "Expected value argument".to_string(),
            });
        }

        Ok(Value::String(args[0].to_string_value()))
    }
}

// ============================================================================
// Special Operations
// ============================================================================

/// INCF - Increment (returns incremented value)
pub struct IncfTool;

impl Tool for IncfTool {
    fn name(&self) -> &str {
        "INCF"
    }

    fn description(&self) -> &str {
        "Increment number by delta (default 1)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "INCF".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let delta = if args.len() > 1 { args[1].as_int()? } else { 1 };

        match &args[0] {
            Value::Int(n) => Ok(Value::Int(n + delta)),
            Value::Float(f) => Ok(Value::Float(f + delta as f64)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// DECF - Decrement (returns decremented value)
pub struct DecfTool;

impl Tool for DecfTool {
    fn name(&self) -> &str {
        "DECF"
    }

    fn description(&self) -> &str {
        "Decrement number by delta (default 1)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "DECF".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let delta = if args.len() > 1 { args[1].as_int()? } else { 1 };

        match &args[0] {
            Value::Int(n) => Ok(Value::Int(n - delta)),
            Value::Float(f) => Ok(Value::Float(f - delta as f64)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// 1+ - Add one
pub struct OnePlusTool;

impl Tool for OnePlusTool {
    fn name(&self) -> &str {
        "1+"
    }

    fn description(&self) -> &str {
        "Add one to number"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "1+".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        match &args[0] {
            Value::Int(n) => Ok(Value::Int(n + 1)),
            Value::Float(f) => Ok(Value::Float(f + 1.0)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// 1- - Subtract one
pub struct OneMinusTool;

impl Tool for OneMinusTool {
    fn name(&self) -> &str {
        "1-"
    }

    fn description(&self) -> &str {
        "Subtract one from number"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "1-".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        match &args[0] {
            Value::Int(n) => Ok(Value::Int(n - 1)),
            Value::Float(f) => Ok(Value::Float(f - 1.0)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// / (reciprocal when single arg)
pub struct ReciprocalTool;

impl Tool for ReciprocalTool {
    fn name(&self) -> &str {
        "RECIPROCAL"
    }

    fn description(&self) -> &str {
        "Compute reciprocal (1/x)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "RECIPROCAL".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        if x == 0.0 {
            return Err(Error::ToolExecutionError {
                tool: "RECIPROCAL".to_string(),
                reason: "Division by zero".to_string(),
            });
        }

        Ok(Value::Float(1.0 / x))
    }
}

/// - (negation when single arg)
pub struct NegateTool;

impl Tool for NegateTool {
    fn name(&self) -> &str {
        "NEGATE"
    }

    fn description(&self) -> &str {
        "Negate number (return -x)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "NEGATE".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        match &args[0] {
            Value::Int(n) => Ok(Value::Int(-n)),
            Value::Float(f) => Ok(Value::Float(-f)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}
