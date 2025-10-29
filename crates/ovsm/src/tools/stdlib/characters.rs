//! Character manipulation and predicate tools - Common Lisp compatible

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

/// Register all character tools
pub fn register(registry: &mut ToolRegistry) {
    // Character predicates
    registry.register(CharacterpTool);
    registry.register(AlphaCharPTool);
    registry.register(DigitCharPTool);
    registry.register(AlphanumericPTool);
    registry.register(WhitespacePTool);
    registry.register(UpperCasePTool);
    registry.register(LowerCasePTool);
    registry.register(BothCasePTool);

    // Character comparison
    registry.register(CharEqualTool);
    registry.register(CharLessTool);
    registry.register(CharGreaterTool);
    registry.register(CharNotEqualTool);
    registry.register(CharLessEqualTool);
    registry.register(CharGreaterEqualTool);

    // Case-insensitive comparison
    registry.register(CharEqualITool);
    registry.register(CharLessITool);
    registry.register(CharGreaterITool);

    // Character conversion (already in strings, but add CL names)
    registry.register(CharIntTool);
    registry.register(IntCharTool);

    // Character attributes
    registry.register(CharNameTool);
    registry.register(NameCharTool);
}

// ============================================================================
// Character Predicates
// ============================================================================

/// CHARACTERP - Check if value is a character
pub struct CharacterpTool;

impl Tool for CharacterpTool {
    fn name(&self) -> &str {
        "CHARACTERP"
    }

    fn description(&self) -> &str {
        "Check if value is a character (single-char string)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }

        match &args[0] {
            Value::String(s) => Ok(Value::Bool(s.chars().count() == 1)),
            _ => Ok(Value::Bool(false)),
        }
    }
}

/// ALPHA-CHAR-P - Check if character is alphabetic
pub struct AlphaCharPTool;

impl Tool for AlphaCharPTool {
    fn name(&self) -> &str {
        "ALPHA-CHAR-P"
    }

    fn description(&self) -> &str {
        "Check if character is alphabetic"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ALPHA-CHAR-P".to_string(),
                reason: "Expected character argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "ALPHA-CHAR-P".to_string(),
            reason: "Empty string".to_string(),
        })?;

        Ok(Value::Bool(ch.is_alphabetic()))
    }
}

/// DIGIT-CHAR-P - Check if character is a digit
pub struct DigitCharPTool;

impl Tool for DigitCharPTool {
    fn name(&self) -> &str {
        "DIGIT-CHAR-P"
    }

    fn description(&self) -> &str {
        "Check if character is a digit"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "DIGIT-CHAR-P".to_string(),
                reason: "Expected character argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "DIGIT-CHAR-P".to_string(),
            reason: "Empty string".to_string(),
        })?;

        Ok(Value::Bool(ch.is_ascii_digit()))
    }
}

/// ALPHANUMERICP - Check if character is alphanumeric
pub struct AlphanumericPTool;

impl Tool for AlphanumericPTool {
    fn name(&self) -> &str {
        "ALPHANUMERICP"
    }

    fn description(&self) -> &str {
        "Check if character is alphanumeric"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ALPHANUMERICP".to_string(),
                reason: "Expected character argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "ALPHANUMERICP".to_string(),
            reason: "Empty string".to_string(),
        })?;

        Ok(Value::Bool(ch.is_alphanumeric()))
    }
}

/// WHITESPACEP - Check if character is whitespace
pub struct WhitespacePTool;

impl Tool for WhitespacePTool {
    fn name(&self) -> &str {
        "WHITESPACEP"
    }

    fn description(&self) -> &str {
        "Check if character is whitespace"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "WHITESPACEP".to_string(),
                reason: "Expected character argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "WHITESPACEP".to_string(),
            reason: "Empty string".to_string(),
        })?;

        Ok(Value::Bool(ch.is_whitespace()))
    }
}

/// UPPER-CASE-P - Check if character is uppercase
pub struct UpperCasePTool;

impl Tool for UpperCasePTool {
    fn name(&self) -> &str {
        "UPPER-CASE-P"
    }

    fn description(&self) -> &str {
        "Check if character is uppercase"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UPPER-CASE-P".to_string(),
                reason: "Expected character argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "UPPER-CASE-P".to_string(),
            reason: "Empty string".to_string(),
        })?;

        Ok(Value::Bool(ch.is_uppercase()))
    }
}

/// LOWER-CASE-P - Check if character is lowercase
pub struct LowerCasePTool;

impl Tool for LowerCasePTool {
    fn name(&self) -> &str {
        "LOWER-CASE-P"
    }

    fn description(&self) -> &str {
        "Check if character is lowercase"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "LOWER-CASE-P".to_string(),
                reason: "Expected character argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "LOWER-CASE-P".to_string(),
            reason: "Empty string".to_string(),
        })?;

        Ok(Value::Bool(ch.is_lowercase()))
    }
}

/// BOTH-CASE-P - Check if character has both cases
pub struct BothCasePTool;

impl Tool for BothCasePTool {
    fn name(&self) -> &str {
        "BOTH-CASE-P"
    }

    fn description(&self) -> &str {
        "Check if character has both uppercase and lowercase forms"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "BOTH-CASE-P".to_string(),
                reason: "Expected character argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "BOTH-CASE-P".to_string(),
            reason: "Empty string".to_string(),
        })?;

        let has_upper = ch.to_uppercase().next() != Some(ch);
        let has_lower = ch.to_lowercase().next() != Some(ch);

        Ok(Value::Bool(has_upper || has_lower))
    }
}

// ============================================================================
// Character Comparison
// ============================================================================

/// CHAR= - Character equality
pub struct CharEqualTool;

impl Tool for CharEqualTool {
    fn name(&self) -> &str {
        "CHAR="
    }

    fn description(&self) -> &str {
        "Check if characters are equal"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CHAR=".to_string(),
                reason: "Expected at least 2 character arguments".to_string(),
            });
        }

        let first = args[0].as_string()?;
        for arg in &args[1..] {
            if arg.as_string()? != first {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    }
}

/// CHAR< - Character less than
pub struct CharLessTool;

impl Tool for CharLessTool {
    fn name(&self) -> &str {
        "CHAR<"
    }

    fn description(&self) -> &str {
        "Check if characters are in increasing order"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CHAR<".to_string(),
                reason: "Expected at least 2 character arguments".to_string(),
            });
        }

        let mut prev = args[0].as_string()?;
        for arg in &args[1..] {
            let curr = arg.as_string()?;
            if prev >= curr {
                return Ok(Value::Bool(false));
            }
            prev = curr;
        }
        Ok(Value::Bool(true))
    }
}

/// CHAR> - Character greater than
pub struct CharGreaterTool;

impl Tool for CharGreaterTool {
    fn name(&self) -> &str {
        "CHAR>"
    }

    fn description(&self) -> &str {
        "Check if characters are in decreasing order"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CHAR>".to_string(),
                reason: "Expected at least 2 character arguments".to_string(),
            });
        }

        let mut prev = args[0].as_string()?;
        for arg in &args[1..] {
            let curr = arg.as_string()?;
            if prev <= curr {
                return Ok(Value::Bool(false));
            }
            prev = curr;
        }
        Ok(Value::Bool(true))
    }
}

/// CHAR/= - Character inequality
pub struct CharNotEqualTool;

impl Tool for CharNotEqualTool {
    fn name(&self) -> &str {
        "CHAR/="
    }

    fn description(&self) -> &str {
        "Check if all characters are different"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CHAR/=".to_string(),
                reason: "Expected at least 2 character arguments".to_string(),
            });
        }

        for i in 0..args.len() {
            for j in (i + 1)..args.len() {
                if args[i].as_string()? == args[j].as_string()? {
                    return Ok(Value::Bool(false));
                }
            }
        }
        Ok(Value::Bool(true))
    }
}

/// CHAR<= - Character less than or equal
pub struct CharLessEqualTool;

impl Tool for CharLessEqualTool {
    fn name(&self) -> &str {
        "CHAR<="
    }

    fn description(&self) -> &str {
        "Check if characters are in non-decreasing order"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CHAR<=".to_string(),
                reason: "Expected at least 2 character arguments".to_string(),
            });
        }

        let mut prev = args[0].as_string()?;
        for arg in &args[1..] {
            let curr = arg.as_string()?;
            if prev > curr {
                return Ok(Value::Bool(false));
            }
            prev = curr;
        }
        Ok(Value::Bool(true))
    }
}

/// CHAR>= - Character greater than or equal
pub struct CharGreaterEqualTool;

impl Tool for CharGreaterEqualTool {
    fn name(&self) -> &str {
        "CHAR>="
    }

    fn description(&self) -> &str {
        "Check if characters are in non-increasing order"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CHAR>=".to_string(),
                reason: "Expected at least 2 character arguments".to_string(),
            });
        }

        let mut prev = args[0].as_string()?;
        for arg in &args[1..] {
            let curr = arg.as_string()?;
            if prev < curr {
                return Ok(Value::Bool(false));
            }
            prev = curr;
        }
        Ok(Value::Bool(true))
    }
}

// ============================================================================
// Case-Insensitive Comparison
// ============================================================================

/// CHAR-EQUAL - Case-insensitive character equality
pub struct CharEqualITool;

impl Tool for CharEqualITool {
    fn name(&self) -> &str {
        "CHAR-EQUAL"
    }

    fn description(&self) -> &str {
        "Check if characters are equal (case-insensitive)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CHAR-EQUAL".to_string(),
                reason: "Expected at least 2 character arguments".to_string(),
            });
        }

        let first = args[0].as_string()?.to_lowercase();
        for arg in &args[1..] {
            if arg.as_string()?.to_lowercase() != first {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    }
}

/// CHAR-LESSP - Case-insensitive less than
pub struct CharLessITool;

impl Tool for CharLessITool {
    fn name(&self) -> &str {
        "CHAR-LESSP"
    }

    fn description(&self) -> &str {
        "Check if characters are in increasing order (case-insensitive)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CHAR-LESSP".to_string(),
                reason: "Expected at least 2 character arguments".to_string(),
            });
        }

        let mut prev = args[0].as_string()?.to_lowercase();
        for arg in &args[1..] {
            let curr = arg.as_string()?.to_lowercase();
            if prev >= curr {
                return Ok(Value::Bool(false));
            }
            prev = curr;
        }
        Ok(Value::Bool(true))
    }
}

/// CHAR-GREATERP - Case-insensitive greater than
pub struct CharGreaterITool;

impl Tool for CharGreaterITool {
    fn name(&self) -> &str {
        "CHAR-GREATERP"
    }

    fn description(&self) -> &str {
        "Check if characters are in decreasing order (case-insensitive)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CHAR-GREATERP".to_string(),
                reason: "Expected at least 2 character arguments".to_string(),
            });
        }

        let mut prev = args[0].as_string()?.to_lowercase();
        for arg in &args[1..] {
            let curr = arg.as_string()?.to_lowercase();
            if prev <= curr {
                return Ok(Value::Bool(false));
            }
            prev = curr;
        }
        Ok(Value::Bool(true))
    }
}

// ============================================================================
// Character Conversion
// ============================================================================

/// CHAR-INT - Get integer code of character (alias for CHAR-CODE)
pub struct CharIntTool;

impl Tool for CharIntTool {
    fn name(&self) -> &str {
        "CHAR-INT"
    }

    fn description(&self) -> &str {
        "Get integer code of character"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CHAR-INT".to_string(),
                reason: "Expected character argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "CHAR-INT".to_string(),
            reason: "Empty string".to_string(),
        })?;

        Ok(Value::Int(ch as i64))
    }
}

/// INT-CHAR - Get character from integer code (alias for CODE-CHAR)
pub struct IntCharTool;

impl Tool for IntCharTool {
    fn name(&self) -> &str {
        "INT-CHAR"
    }

    fn description(&self) -> &str {
        "Get character from integer code"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "INT-CHAR".to_string(),
                reason: "Expected integer argument".to_string(),
            });
        }

        let code = args[0].as_int()?;
        let ch = char::from_u32(code as u32).ok_or_else(|| Error::InvalidArguments {
            tool: "INT-CHAR".to_string(),
            reason: format!("Invalid character code: {}", code),
        })?;

        Ok(Value::String(ch.to_string()))
    }
}

// ============================================================================
// Character Attributes
// ============================================================================

/// CHAR-NAME - Get name of character
pub struct CharNameTool;

impl Tool for CharNameTool {
    fn name(&self) -> &str {
        "CHAR-NAME"
    }

    fn description(&self) -> &str {
        "Get name of character (e.g., 'Space', 'Newline')"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CHAR-NAME".to_string(),
                reason: "Expected character argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "CHAR-NAME".to_string(),
            reason: "Empty string".to_string(),
        })?;

        let name = match ch {
            ' ' => "Space",
            '\n' => "Newline",
            '\t' => "Tab",
            '\r' => "Return",
            '\0' => "Null",
            _ => return Ok(Value::Null), // Most chars don't have names
        };

        Ok(Value::String(name.to_string()))
    }
}

/// NAME-CHAR - Get character from name
pub struct NameCharTool;

impl Tool for NameCharTool {
    fn name(&self) -> &str {
        "NAME-CHAR"
    }

    fn description(&self) -> &str {
        "Get character from name (e.g., 'Space' -> ' ')"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "NAME-CHAR".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let name = args[0].as_string()?.to_lowercase();

        let ch = match name.as_str() {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            "return" => '\r',
            "null" => '\0',
            _ => return Ok(Value::Null), // Unknown name
        };

        Ok(Value::String(ch.to_string()))
    }
}
