//! String manipulation tools - Common Lisp compatible string functions

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

/// Register all string manipulation tools
pub fn register(registry: &mut ToolRegistry) {
    // Case conversion
    registry.register(StringUpcaseTool);
    registry.register(StringDowncaseTool);
    registry.register(StringCapitalizeTool);

    // Trimming
    registry.register(StringTrimTool);
    registry.register(StringLeftTrimTool);
    registry.register(StringRightTrimTool);

    // Substring operations
    registry.register(SubseqTool);
    registry.register(SubstringTool);
    registry.register(CharAtTool);

    // String comparison
    registry.register(StringEqualTool);
    registry.register(StringLessTool);
    registry.register(StringGreaterTool);
    registry.register(StringNotEqualTool);
    registry.register(StringLessOrEqualTool);
    registry.register(StringGreaterOrEqualTool);

    // Case-insensitive comparison
    registry.register(StringEqualp);
    registry.register(StringLesspTool);
    registry.register(StringGreaterpTool);

    // String construction
    registry.register(MakeStringTool);
    registry.register(StringTool);
    registry.register(ConcatenateTool);

    // Character operations
    registry.register(CharCodeTool);
    registry.register(CodeCharTool);
    registry.register(CharUpcaseTool);
    registry.register(CharDowncaseTool);

    // String search
    registry.register(SearchTool);
    registry.register(PositionTool);
    registry.register(CountOccurrencesTool);

    // String modification
    registry.register(ReplaceTool);
    registry.register(ReplaceAllTool);
    registry.register(ReverseTool);

    // String extensions (Phase 5)
    registry.register(StringNotLesspTool);
    registry.register(StringNotGreaterpTool);
    registry.register(NstringUpcaseTool);
    registry.register(NstringDowncaseTool);
    registry.register(NstringCapitalizeTool);
    registry.register(StringpTool);
    registry.register(SimpleStringPTool);
    registry.register(BothCasePTool);
    registry.register(CharTool);
    registry.register(ScharTool);
    registry.register(StringUpcasePTool);
    registry.register(StringDowncasePTool);
    registry.register(StringConcatenateTool);
    registry.register(StringToListTool);
    registry.register(ListToStringTool);

    // Commonly expected functions (AI hallucination prevention)
    registry.register(StringSplitTool);
    registry.register(SplitTool);
    registry.register(StringJoinTool);
    registry.register(JoinTool);
    registry.register(StringAppendTool);
    registry.register(FormatTool);
    registry.register(SprintfTool);
    registry.register(ConcatTool);
    registry.register(StrTool);
    registry.register(ToStringTool);
    registry.register(StringContainsTool);
    registry.register(IncludesTool);
    registry.register(StringStartsWithTool);
    registry.register(StartsWithTool);
    registry.register(StringEndsWithTool);
    registry.register(EndsWithTool);
    registry.register(StringLengthTool);
    registry.register(CharAtIndexTool);
}

// ============================================================================
// Case Conversion
// ============================================================================

/// STRING-UPCASE - Convert string to uppercase
pub struct StringUpcaseTool;

impl Tool for StringUpcaseTool {
    fn name(&self) -> &str {
        "STRING-UPCASE"
    }

    fn description(&self) -> &str {
        "Convert string to uppercase"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "STRING-UPCASE".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        Ok(Value::String(s.to_uppercase()))
    }
}

/// STRING-DOWNCASE - Convert string to lowercase
pub struct StringDowncaseTool;

impl Tool for StringDowncaseTool {
    fn name(&self) -> &str {
        "STRING-DOWNCASE"
    }

    fn description(&self) -> &str {
        "Convert string to lowercase"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "STRING-DOWNCASE".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        Ok(Value::String(s.to_lowercase()))
    }
}

/// STRING-CAPITALIZE - Capitalize first letter of each word
pub struct StringCapitalizeTool;

impl Tool for StringCapitalizeTool {
    fn name(&self) -> &str {
        "STRING-CAPITALIZE"
    }

    fn description(&self) -> &str {
        "Capitalize first letter of each word"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "STRING-CAPITALIZE".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let mut result = String::new();
        let mut capitalize_next = true;

        for c in s.chars() {
            if c.is_whitespace() {
                result.push(c);
                capitalize_next = true;
            } else if capitalize_next {
                result.push(c.to_uppercase().next().unwrap());
                capitalize_next = false;
            } else {
                result.push(c.to_lowercase().next().unwrap());
            }
        }

        Ok(Value::String(result))
    }
}

// ============================================================================
// Trimming
// ============================================================================

/// STRING-TRIM - Trim whitespace from both ends
pub struct StringTrimTool;

impl Tool for StringTrimTool {
    fn name(&self) -> &str {
        "STRING-TRIM"
    }

    fn description(&self) -> &str {
        "Trim whitespace from both ends of string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "STRING-TRIM".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        Ok(Value::String(s.trim().to_string()))
    }
}

/// STRING-LEFT-TRIM - Trim whitespace from left end
pub struct StringLeftTrimTool;

impl Tool for StringLeftTrimTool {
    fn name(&self) -> &str {
        "STRING-LEFT-TRIM"
    }

    fn description(&self) -> &str {
        "Trim whitespace from left end of string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "STRING-LEFT-TRIM".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        Ok(Value::String(s.trim_start().to_string()))
    }
}

/// STRING-RIGHT-TRIM - Trim whitespace from right end
pub struct StringRightTrimTool;

impl Tool for StringRightTrimTool {
    fn name(&self) -> &str {
        "STRING-RIGHT-TRIM"
    }

    fn description(&self) -> &str {
        "Trim whitespace from right end of string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "STRING-RIGHT-TRIM".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        Ok(Value::String(s.trim_end().to_string()))
    }
}

// ============================================================================
// Substring Operations
// ============================================================================

/// SUBSEQ - Extract subsequence
pub struct SubseqTool;

impl Tool for SubseqTool {
    fn name(&self) -> &str {
        "SUBSEQ"
    }

    fn description(&self) -> &str {
        "Extract subsequence from string or array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SUBSEQ".to_string(),
                reason: "Expected sequence and start index".to_string(),
            });
        }

        let start = args[1].as_int()? as usize;
        let end = if args.len() > 2 {
            Some(args[2].as_int()? as usize)
        } else {
            None
        };

        match &args[0] {
            Value::String(s) => {
                let chars: Vec<char> = s.chars().collect();
                let end_idx = end.unwrap_or(chars.len());
                if start > chars.len() || end_idx > chars.len() || start > end_idx {
                    return Err(Error::InvalidArguments {
                        tool: "SUBSEQ".to_string(),
                        reason: "Invalid subsequence bounds".to_string(),
                    });
                }
                let substr: String = chars[start..end_idx].iter().collect();
                Ok(Value::String(substr))
            }
            Value::Array(arr) => {
                let end_idx = end.unwrap_or(arr.len());
                if start > arr.len() || end_idx > arr.len() || start > end_idx {
                    return Err(Error::InvalidArguments {
                        tool: "SUBSEQ".to_string(),
                        reason: "Invalid subsequence bounds".to_string(),
                    });
                }
                Ok(Value::Array(Arc::new(arr[start..end_idx].to_vec())))
            }
            _ => Err(Error::TypeError {
                expected: "string or array".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// SUBSTRING - Alias for SUBSEQ
pub struct SubstringTool;

impl Tool for SubstringTool {
    fn name(&self) -> &str {
        "SUBSTRING"
    }

    fn description(&self) -> &str {
        "Extract substring (alias for SUBSEQ)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        SubseqTool.execute(args)
    }
}

/// CHAR-AT - Get character at index
pub struct CharAtTool;

impl Tool for CharAtTool {
    fn name(&self) -> &str {
        "CHAR-AT"
    }

    fn description(&self) -> &str {
        "Get character at index"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CHAR-AT".to_string(),
                reason: "Expected string and index".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let index = args[1].as_int()? as usize;
        let chars: Vec<char> = s.chars().collect();

        if index >= chars.len() {
            return Err(Error::IndexOutOfBounds {
                index,
                length: chars.len(),
            });
        }

        Ok(Value::String(chars[index].to_string()))
    }
}

// ============================================================================
// String Comparison
// ============================================================================

/// STRING= - String equality (case-sensitive)
pub struct StringEqualTool;

impl Tool for StringEqualTool {
    fn name(&self) -> &str {
        "STRING="
    }

    fn description(&self) -> &str {
        "Check if strings are equal (case-sensitive)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING=".to_string(),
                reason: "Expected 2 string arguments".to_string(),
            });
        }

        let s1 = args[0].as_string()?;
        let s2 = args[1].as_string()?;
        Ok(Value::Bool(s1 == s2))
    }
}

/// STRING< - String less than
pub struct StringLessTool;

impl Tool for StringLessTool {
    fn name(&self) -> &str {
        "STRING<"
    }

    fn description(&self) -> &str {
        "Check if string1 < string2 (lexicographic)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING<".to_string(),
                reason: "Expected 2 string arguments".to_string(),
            });
        }

        let s1 = args[0].as_string()?;
        let s2 = args[1].as_string()?;
        Ok(Value::Bool(s1 < s2))
    }
}

/// STRING> - String greater than
pub struct StringGreaterTool;

impl Tool for StringGreaterTool {
    fn name(&self) -> &str {
        "STRING>"
    }

    fn description(&self) -> &str {
        "Check if string1 > string2 (lexicographic)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING>".to_string(),
                reason: "Expected 2 string arguments".to_string(),
            });
        }

        let s1 = args[0].as_string()?;
        let s2 = args[1].as_string()?;
        Ok(Value::Bool(s1 > s2))
    }
}

/// STRING/= - String not equal
pub struct StringNotEqualTool;

impl Tool for StringNotEqualTool {
    fn name(&self) -> &str {
        "STRING/="
    }

    fn description(&self) -> &str {
        "Check if strings are not equal"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING/=".to_string(),
                reason: "Expected 2 string arguments".to_string(),
            });
        }

        let s1 = args[0].as_string()?;
        let s2 = args[1].as_string()?;
        Ok(Value::Bool(s1 != s2))
    }
}

/// STRING<= - String less than or equal
pub struct StringLessOrEqualTool;

impl Tool for StringLessOrEqualTool {
    fn name(&self) -> &str {
        "STRING<="
    }

    fn description(&self) -> &str {
        "Check if string1 <= string2"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING<=".to_string(),
                reason: "Expected 2 string arguments".to_string(),
            });
        }

        let s1 = args[0].as_string()?;
        let s2 = args[1].as_string()?;
        Ok(Value::Bool(s1 <= s2))
    }
}

/// STRING>= - String greater than or equal
pub struct StringGreaterOrEqualTool;

impl Tool for StringGreaterOrEqualTool {
    fn name(&self) -> &str {
        "STRING>="
    }

    fn description(&self) -> &str {
        "Check if string1 >= string2"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING>=".to_string(),
                reason: "Expected 2 string arguments".to_string(),
            });
        }

        let s1 = args[0].as_string()?;
        let s2 = args[1].as_string()?;
        Ok(Value::Bool(s1 >= s2))
    }
}

// Case-insensitive comparison

/// STRING-EQUAL - Case-insensitive string equality
pub struct StringEqualp;

impl Tool for StringEqualp {
    fn name(&self) -> &str {
        "STRING-EQUAL"
    }

    fn description(&self) -> &str {
        "Check if strings are equal (case-insensitive)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING-EQUAL".to_string(),
                reason: "Expected 2 string arguments".to_string(),
            });
        }

        let s1 = args[0].as_string()?.to_lowercase();
        let s2 = args[1].as_string()?.to_lowercase();
        Ok(Value::Bool(s1 == s2))
    }
}

/// STRING-LESSP - Case-insensitive string less than
pub struct StringLesspTool;

impl Tool for StringLesspTool {
    fn name(&self) -> &str {
        "STRING-LESSP"
    }

    fn description(&self) -> &str {
        "Check if string1 < string2 (case-insensitive)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING-LESSP".to_string(),
                reason: "Expected 2 string arguments".to_string(),
            });
        }

        let s1 = args[0].as_string()?.to_lowercase();
        let s2 = args[1].as_string()?.to_lowercase();
        Ok(Value::Bool(s1 < s2))
    }
}

/// STRING-GREATERP - Case-insensitive string greater than
pub struct StringGreaterpTool;

impl Tool for StringGreaterpTool {
    fn name(&self) -> &str {
        "STRING-GREATERP"
    }

    fn description(&self) -> &str {
        "Check if string1 > string2 (case-insensitive)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING-GREATERP".to_string(),
                reason: "Expected 2 string arguments".to_string(),
            });
        }

        let s1 = args[0].as_string()?.to_lowercase();
        let s2 = args[1].as_string()?.to_lowercase();
        Ok(Value::Bool(s1 > s2))
    }
}

// ============================================================================
// String Construction
// ============================================================================

/// MAKE-STRING - Create string of specified length
pub struct MakeStringTool;

impl Tool for MakeStringTool {
    fn name(&self) -> &str {
        "MAKE-STRING"
    }

    fn description(&self) -> &str {
        "Create string of specified length filled with character"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MAKE-STRING".to_string(),
                reason: "Expected length argument".to_string(),
            });
        }

        let len = args[0].as_int()? as usize;
        let ch = if args.len() > 1 {
            let s = args[1].as_string()?;
            s.chars().next().unwrap_or(' ')
        } else {
            ' '
        };

        Ok(Value::String(ch.to_string().repeat(len)))
    }
}

/// STRING - Convert value to string
pub struct StringTool;

impl Tool for StringTool {
    fn name(&self) -> &str {
        "STRING"
    }

    fn description(&self) -> &str {
        "Convert value to string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::String(String::new()));
        }

        Ok(Value::String(args[0].to_string_value()))
    }
}

/// CONCATENATE - Concatenate sequences (strings or arrays)
pub struct ConcatenateTool;

impl Tool for ConcatenateTool {
    fn name(&self) -> &str {
        "CONCATENATE"
    }

    fn description(&self) -> &str {
        "Concatenate strings or arrays"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::String(String::new()));
        }

        // Check if all arguments are strings or all are arrays
        let all_strings = args.iter().all(|v| matches!(v, Value::String(_)));
        let all_arrays = args.iter().all(|v| matches!(v, Value::Array(_)));

        if all_strings {
            let mut result = String::new();
            for arg in args {
                result.push_str(arg.as_string()?);
            }
            Ok(Value::String(result))
        } else if all_arrays {
            let mut result = Vec::new();
            for arg in args {
                result.extend(arg.as_array()?.iter().cloned());
            }
            Ok(Value::Array(Arc::new(result)))
        } else {
            Err(Error::TypeError {
                expected: "all strings or all arrays".to_string(),
                got: "mixed types".to_string(),
            })
        }
    }
}

// ============================================================================
// Character Operations
// ============================================================================

/// CHAR-CODE - Get character code (Unicode code point)
pub struct CharCodeTool;

impl Tool for CharCodeTool {
    fn name(&self) -> &str {
        "CHAR-CODE"
    }

    fn description(&self) -> &str {
        "Get Unicode code point of character"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CHAR-CODE".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "CHAR-CODE".to_string(),
            reason: "Empty string".to_string(),
        })?;

        Ok(Value::Int(ch as i64))
    }
}

/// CODE-CHAR - Get character from code point
pub struct CodeCharTool;

impl Tool for CodeCharTool {
    fn name(&self) -> &str {
        "CODE-CHAR"
    }

    fn description(&self) -> &str {
        "Get character from Unicode code point"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CODE-CHAR".to_string(),
                reason: "Expected integer argument".to_string(),
            });
        }

        let code = args[0].as_int()?;
        let ch = char::from_u32(code as u32).ok_or_else(|| Error::InvalidArguments {
            tool: "CODE-CHAR".to_string(),
            reason: format!("Invalid Unicode code point: {}", code),
        })?;

        Ok(Value::String(ch.to_string()))
    }
}

/// CHAR-UPCASE - Convert character to uppercase
pub struct CharUpcaseTool;

impl Tool for CharUpcaseTool {
    fn name(&self) -> &str {
        "CHAR-UPCASE"
    }

    fn description(&self) -> &str {
        "Convert character to uppercase"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CHAR-UPCASE".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "CHAR-UPCASE".to_string(),
            reason: "Empty string".to_string(),
        })?;

        Ok(Value::String(ch.to_uppercase().next().unwrap().to_string()))
    }
}

/// CHAR-DOWNCASE - Convert character to lowercase
pub struct CharDowncaseTool;

impl Tool for CharDowncaseTool {
    fn name(&self) -> &str {
        "CHAR-DOWNCASE"
    }

    fn description(&self) -> &str {
        "Convert character to lowercase"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CHAR-DOWNCASE".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let ch = s.chars().next().ok_or_else(|| Error::InvalidArguments {
            tool: "CHAR-DOWNCASE".to_string(),
            reason: "Empty string".to_string(),
        })?;

        Ok(Value::String(ch.to_lowercase().next().unwrap().to_string()))
    }
}

// ============================================================================
// String Search
// ============================================================================

/// SEARCH - Search for substring
pub struct SearchTool;

impl Tool for SearchTool {
    fn name(&self) -> &str {
        "SEARCH"
    }

    fn description(&self) -> &str {
        "Search for substring, return index or null"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SEARCH".to_string(),
                reason: "Expected substring and string arguments".to_string(),
            });
        }

        let needle = args[0].as_string()?;
        let haystack = args[1].as_string()?;

        match haystack.find(needle) {
            Some(idx) => Ok(Value::Int(idx as i64)),
            None => Ok(Value::Null),
        }
    }
}

/// POSITION - Find position of character in string
pub struct PositionTool;

impl Tool for PositionTool {
    fn name(&self) -> &str {
        "POSITION"
    }

    fn description(&self) -> &str {
        "Find position of character in string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "POSITION".to_string(),
                reason: "Expected character and string arguments".to_string(),
            });
        }

        let needle = args[0].as_string()?;
        let haystack = args[1].as_string()?;
        let ch = needle
            .chars()
            .next()
            .ok_or_else(|| Error::InvalidArguments {
                tool: "POSITION".to_string(),
                reason: "Empty search string".to_string(),
            })?;

        match haystack.find(ch) {
            Some(idx) => Ok(Value::Int(idx as i64)),
            None => Ok(Value::Null),
        }
    }
}

/// COUNT-OCCURRENCES - Count occurrences of substring
pub struct CountOccurrencesTool;

impl Tool for CountOccurrencesTool {
    fn name(&self) -> &str {
        "COUNT-OCCURRENCES"
    }

    fn description(&self) -> &str {
        "Count occurrences of substring in string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "COUNT-OCCURRENCES".to_string(),
                reason: "Expected substring and string arguments".to_string(),
            });
        }

        let needle = args[0].as_string()?;
        let haystack = args[1].as_string()?;

        if needle.is_empty() {
            return Ok(Value::Int(0));
        }

        let count = haystack.matches(&needle).count();
        Ok(Value::Int(count as i64))
    }
}

// ============================================================================
// String Modification
// ============================================================================

/// REPLACE - Replace first occurrence of substring
pub struct ReplaceTool;

impl Tool for ReplaceTool {
    fn name(&self) -> &str {
        "REPLACE"
    }

    fn description(&self) -> &str {
        "Replace first occurrence of substring"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "REPLACE".to_string(),
                reason: "Expected string, old, and new arguments".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let old = args[1].as_string()?;
        let new = args[2].as_string()?;

        Ok(Value::String(s.replacen(old, new, 1)))
    }
}

/// REPLACE-ALL - Replace all occurrences of substring
pub struct ReplaceAllTool;

impl Tool for ReplaceAllTool {
    fn name(&self) -> &str {
        "REPLACE-ALL"
    }

    fn description(&self) -> &str {
        "Replace all occurrences of substring"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "REPLACE-ALL".to_string(),
                reason: "Expected string, old, and new arguments".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let old = args[1].as_string()?;
        let new = args[2].as_string()?;

        Ok(Value::String(s.replace(old, new)))
    }
}

/// STRING-REVERSE - Reverse a string
pub struct ReverseTool;

impl Tool for ReverseTool {
    fn name(&self) -> &str {
        "STRING-REVERSE"
    }

    fn description(&self) -> &str {
        "Reverse a string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "STRING-REVERSE".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let reversed: String = s.chars().rev().collect();
        Ok(Value::String(reversed))
    }
}

// ============================================================================
// STRING EXTENSIONS - Additional Common Lisp Functions
// ============================================================================

/// STRING-NOT-LESSP - Case-insensitive >= comparison
pub struct StringNotLesspTool;

impl Tool for StringNotLesspTool {
    fn name(&self) -> &str {
        "STRING-NOT-LESSP"
    }

    fn description(&self) -> &str {
        "Case-insensitive string >= comparison"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING-NOT-LESSP".to_string(),
                reason: "Expected two string arguments".to_string(),
            });
        }

        let s1 = args[0].as_string()?.to_lowercase();
        let s2 = args[1].as_string()?.to_lowercase();

        Ok(Value::Bool(s1 >= s2))
    }
}

/// STRING-NOT-GREATERP - Case-insensitive <= comparison
pub struct StringNotGreaterpTool;

impl Tool for StringNotGreaterpTool {
    fn name(&self) -> &str {
        "STRING-NOT-GREATERP"
    }

    fn description(&self) -> &str {
        "Case-insensitive string <= comparison"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING-NOT-GREATERP".to_string(),
                reason: "Expected two string arguments".to_string(),
            });
        }

        let s1 = args[0].as_string()?.to_lowercase();
        let s2 = args[1].as_string()?.to_lowercase();

        Ok(Value::Bool(s1 <= s2))
    }
}

/// NSTRING-UPCASE - Destructive upcase (same as STRING-UPCASE in immutable OVSM)
pub struct NstringUpcaseTool;

impl Tool for NstringUpcaseTool {
    fn name(&self) -> &str {
        "NSTRING-UPCASE"
    }

    fn description(&self) -> &str {
        "Convert string to uppercase (destructive variant, same as STRING-UPCASE in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "NSTRING-UPCASE".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        Ok(Value::String(s.to_uppercase()))
    }
}

/// NSTRING-DOWNCASE - Destructive downcase (same as STRING-DOWNCASE in immutable OVSM)
pub struct NstringDowncaseTool;

impl Tool for NstringDowncaseTool {
    fn name(&self) -> &str {
        "NSTRING-DOWNCASE"
    }

    fn description(&self) -> &str {
        "Convert string to lowercase (destructive variant, same as STRING-DOWNCASE in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "NSTRING-DOWNCASE".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        Ok(Value::String(s.to_lowercase()))
    }
}

/// NSTRING-CAPITALIZE - Destructive capitalize (same as STRING-CAPITALIZE in immutable OVSM)
pub struct NstringCapitalizeTool;

impl Tool for NstringCapitalizeTool {
    fn name(&self) -> &str {
        "NSTRING-CAPITALIZE"
    }

    fn description(&self) -> &str {
        "Capitalize first character (destructive variant, same as STRING-CAPITALIZE in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "NSTRING-CAPITALIZE".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let mut chars = s.chars();

        match chars.next() {
            None => Ok(Value::String(String::new())),
            Some(first) => {
                let capitalized = first.to_uppercase().collect::<String>() + chars.as_str();
                Ok(Value::String(capitalized))
            }
        }
    }
}

/// STRINGP - Type predicate for strings
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

        Ok(Value::Bool(matches!(args[0], Value::String(_))))
    }
}

/// SIMPLE-STRING-P - Check if value is a simple string
pub struct SimpleStringPTool;

impl Tool for SimpleStringPTool {
    fn name(&self) -> &str {
        "SIMPLE-STRING-P"
    }

    fn description(&self) -> &str {
        "Check if value is a simple string (same as STRINGP in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }

        Ok(Value::Bool(matches!(args[0], Value::String(_))))
    }
}

/// BOTH-CASE-P - Check if string contains both uppercase and lowercase characters
pub struct BothCasePTool;

impl Tool for BothCasePTool {
    fn name(&self) -> &str {
        "BOTH-CASE-P"
    }

    fn description(&self) -> &str {
        "Check if string contains both uppercase and lowercase characters"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "BOTH-CASE-P".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let has_upper = s.chars().any(|c| c.is_uppercase());
        let has_lower = s.chars().any(|c| c.is_lowercase());

        Ok(Value::Bool(has_upper && has_lower))
    }
}

/// CHAR - Get character at index (alias for CHAR-AT)
pub struct CharTool;

impl Tool for CharTool {
    fn name(&self) -> &str {
        "CHAR"
    }

    fn description(&self) -> &str {
        "Get character at index"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CHAR".to_string(),
                reason: "Expected string and index arguments".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let index = args[1].as_int()? as usize;

        s.chars()
            .nth(index)
            .map(|c| Value::String(c.to_string()))
            .ok_or_else(|| Error::InvalidArguments {
                tool: "CHAR".to_string(),
                reason: format!("Index {} out of bounds", index),
            })
    }
}

/// SCHAR - Simple character access (same as CHAR in OVSM)
pub struct ScharTool;

impl Tool for ScharTool {
    fn name(&self) -> &str {
        "SCHAR"
    }

    fn description(&self) -> &str {
        "Simple character access (same as CHAR in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Same implementation as CHAR
        CharTool.execute(args)
    }
}

/// STRING-UPCASE-P - Check if all cased characters are uppercase
pub struct StringUpcasePTool;

impl Tool for StringUpcasePTool {
    fn name(&self) -> &str {
        "UPPER-CASE-P"
    }

    fn description(&self) -> &str {
        "Check if all cased characters are uppercase"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UPPER-CASE-P".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let cased_chars: Vec<char> = s.chars().filter(|c| c.is_alphabetic()).collect();

        if cased_chars.is_empty() {
            return Ok(Value::Bool(false));
        }

        Ok(Value::Bool(cased_chars.iter().all(|c| c.is_uppercase())))
    }
}

/// STRING-DOWNCASE-P - Check if all cased characters are lowercase
pub struct StringDowncasePTool;

impl Tool for StringDowncasePTool {
    fn name(&self) -> &str {
        "LOWER-CASE-P"
    }

    fn description(&self) -> &str {
        "Check if all cased characters are lowercase"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "LOWER-CASE-P".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let cased_chars: Vec<char> = s.chars().filter(|c| c.is_alphabetic()).collect();

        if cased_chars.is_empty() {
            return Ok(Value::Bool(false));
        }

        Ok(Value::Bool(cased_chars.iter().all(|c| c.is_lowercase())))
    }
}

/// STRING-CONCATENATE - Concatenate multiple strings
pub struct StringConcatenateTool;

impl Tool for StringConcatenateTool {
    fn name(&self) -> &str {
        "STRING-CONCATENATE"
    }

    fn description(&self) -> &str {
        "Concatenate multiple strings"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        let mut result = String::new();

        for arg in args {
            result.push_str(arg.as_string()?);
        }

        Ok(Value::String(result))
    }
}

/// STRING-TO-LIST - Convert string to list of characters
pub struct StringToListTool;

impl Tool for StringToListTool {
    fn name(&self) -> &str {
        "STRING-TO-LIST"
    }

    fn description(&self) -> &str {
        "Convert string to list of characters"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "STRING-TO-LIST".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let chars: Vec<Value> = s.chars().map(|c| Value::String(c.to_string())).collect();

        Ok(Value::Array(Arc::new(chars)))
    }
}

/// LIST-TO-STRING - Convert list of characters to string
pub struct ListToStringTool;

impl Tool for ListToStringTool {
    fn name(&self) -> &str {
        "LIST-TO-STRING"
    }

    fn description(&self) -> &str {
        "Convert list of characters to string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "LIST-TO-STRING".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let arr = args[0].as_array()?;
        let mut result = String::new();

        for val in arr.iter() {
            let s = val.as_string()?;
            result.push_str(s);
        }

        Ok(Value::String(result))
    }
}

// ============================================================================
// COMMONLY EXPECTED STRING FUNCTIONS (AI hallucination prevention)
// ============================================================================

/// STRING-SPLIT - Split string by delimiter into array
pub struct StringSplitTool;

impl Tool for StringSplitTool {
    fn name(&self) -> &str {
        "STRING-SPLIT"
    }

    fn description(&self) -> &str {
        "Split string by delimiter into array of strings"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING-SPLIT".to_string(),
                reason: "Expected string and delimiter arguments".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let delimiter = args[1].as_string()?;

        let parts: Vec<Value> = if delimiter.is_empty() {
            // Split into individual characters
            s.chars().map(|c| Value::String(c.to_string())).collect()
        } else {
            s.split(&delimiter)
                .map(|part| Value::String(part.to_string()))
                .collect()
        };

        Ok(Value::Array(Arc::new(parts)))
    }
}

/// SPLIT - Alias for STRING-SPLIT
pub struct SplitTool;

impl Tool for SplitTool {
    fn name(&self) -> &str {
        "SPLIT"
    }

    fn description(&self) -> &str {
        "Split string by delimiter (alias for STRING-SPLIT)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        StringSplitTool.execute(args)
    }
}

/// STRING-JOIN - Join array of strings with delimiter
pub struct StringJoinTool;

impl Tool for StringJoinTool {
    fn name(&self) -> &str {
        "STRING-JOIN"
    }

    fn description(&self) -> &str {
        "Join array of strings with delimiter"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING-JOIN".to_string(),
                reason: "Expected array and delimiter arguments".to_string(),
            });
        }

        let arr = args[0].as_array()?;
        let delimiter = args[1].as_string()?;

        let strings: Result<Vec<String>> = arr
            .iter()
            .map(|v| v.as_string().map(|s| s.to_string()))
            .collect();

        let strings = strings?;
        Ok(Value::String(strings.join(delimiter)))
    }
}

/// JOIN - Alias for STRING-JOIN
pub struct JoinTool;

impl Tool for JoinTool {
    fn name(&self) -> &str {
        "JOIN"
    }

    fn description(&self) -> &str {
        "Join array of strings with delimiter (alias for STRING-JOIN)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        StringJoinTool.execute(args)
    }
}

/// STRING-APPEND - Convenient alias for CONCATENATE
pub struct StringAppendTool;

impl Tool for StringAppendTool {
    fn name(&self) -> &str {
        "STRING-APPEND"
    }

    fn description(&self) -> &str {
        "Append strings together (alias for CONCATENATE)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        ConcatenateTool.execute(args)
    }
}

/// FORMAT - Basic string formatting with placeholders
pub struct FormatTool;

impl Tool for FormatTool {
    fn name(&self) -> &str {
        "FORMAT"
    }

    fn description(&self) -> &str {
        "Format string with placeholders: (format \"Hello {}!\" \"World\") => \"Hello World!\""
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FORMAT".to_string(),
                reason: "Expected format string".to_string(),
            });
        }

        let format_str = args[0].as_string()?;
        let mut result = format_str.to_string();

        // Replace {} placeholders with arguments
        for arg in args[1..].iter() {
            let arg_str = match arg {
                Value::String(s) => s.clone(),
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Null => "null".to_string(),
                Value::Array(_) => format!("{:?}", arg),
                Value::Object(_) => format!("{:?}", arg),
                Value::Function { .. } => "<function>".to_string(),
                Value::Range { .. } => format!("{:?}", arg),
                Value::Multiple(_) => format!("{:?}", arg),
                Value::Macro { .. } => "<macro>".to_string(),
            };

            // Replace first occurrence of {}
            if let Some(pos) = result.find("{}") {
                result.replace_range(pos..pos + 2, &arg_str);
            } else {
                // No more placeholders, we're done
                break;
            }
        }

        Ok(Value::String(result))
    }
}

/// SPRINTF - Alias for FORMAT (commonly expected name)
pub struct SprintfTool;

impl Tool for SprintfTool {
    fn name(&self) -> &str {
        "SPRINTF"
    }

    fn description(&self) -> &str {
        "Format string (alias for FORMAT)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        FormatTool.execute(args)
    }
}

/// CONCAT - Alias for CONCATENATE (shorter name)
pub struct ConcatTool;

impl Tool for ConcatTool {
    fn name(&self) -> &str {
        "CONCAT"
    }

    fn description(&self) -> &str {
        "Concatenate strings (alias for CONCATENATE)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        ConcatenateTool.execute(args)
    }
}

/// STR - Convert value to string
pub struct StrTool;

impl Tool for StrTool {
    fn name(&self) -> &str {
        "STR"
    }

    fn description(&self) -> &str {
        "Convert value to string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::String(String::new()));
        }

        let s = match &args[0] {
            Value::String(s) => s.clone(),
            Value::Int(n) => n.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Null => "null".to_string(),
            v => format!("{:?}", v),
        };

        Ok(Value::String(s))
    }
}

/// TO-STRING - Alias for STR
pub struct ToStringTool;

impl Tool for ToStringTool {
    fn name(&self) -> &str {
        "TO-STRING"
    }

    fn description(&self) -> &str {
        "Convert value to string (alias for STR)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        StrTool.execute(args)
    }
}

/// STRING-CONTAINS - Check if string contains substring
pub struct StringContainsTool;

impl Tool for StringContainsTool {
    fn name(&self) -> &str {
        "STRING-CONTAINS"
    }

    fn description(&self) -> &str {
        "Check if string contains substring"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING-CONTAINS".to_string(),
                reason: "Expected string and substring arguments".to_string(),
            });
        }

        let haystack = args[0].as_string()?;
        let needle = args[1].as_string()?;

        Ok(Value::Bool(haystack.contains(needle)))
    }
}

/// INCLUDES - Alias for STRING-CONTAINS
pub struct IncludesTool;

impl Tool for IncludesTool {
    fn name(&self) -> &str {
        "INCLUDES"
    }

    fn description(&self) -> &str {
        "Check if string contains substring (alias for STRING-CONTAINS)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        StringContainsTool.execute(args)
    }
}

/// STRING-STARTS-WITH - Check if string starts with prefix
pub struct StringStartsWithTool;

impl Tool for StringStartsWithTool {
    fn name(&self) -> &str {
        "STRING-STARTS-WITH"
    }

    fn description(&self) -> &str {
        "Check if string starts with prefix"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING-STARTS-WITH".to_string(),
                reason: "Expected string and prefix arguments".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let prefix = args[1].as_string()?;

        Ok(Value::Bool(s.starts_with(prefix)))
    }
}

/// STARTS-WITH - Alias for STRING-STARTS-WITH
pub struct StartsWithTool;

impl Tool for StartsWithTool {
    fn name(&self) -> &str {
        "STARTS-WITH"
    }

    fn description(&self) -> &str {
        "Check if string starts with prefix (alias for STRING-STARTS-WITH)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        StringStartsWithTool.execute(args)
    }
}

/// STRING-ENDS-WITH - Check if string ends with suffix
pub struct StringEndsWithTool;

impl Tool for StringEndsWithTool {
    fn name(&self) -> &str {
        "STRING-ENDS-WITH"
    }

    fn description(&self) -> &str {
        "Check if string ends with suffix"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "STRING-ENDS-WITH".to_string(),
                reason: "Expected string and suffix arguments".to_string(),
            });
        }

        let s = args[0].as_string()?;
        let suffix = args[1].as_string()?;

        Ok(Value::Bool(s.ends_with(&suffix)))
    }
}

/// ENDS-WITH - Alias for STRING-ENDS-WITH
pub struct EndsWithTool;

impl Tool for EndsWithTool {
    fn name(&self) -> &str {
        "ENDS-WITH"
    }

    fn description(&self) -> &str {
        "Check if string ends with suffix (alias for STRING-ENDS-WITH)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        StringEndsWithTool.execute(args)
    }
}

/// STRING-LENGTH - Get length of string
pub struct StringLengthTool;

impl Tool for StringLengthTool {
    fn name(&self) -> &str {
        "STRING-LENGTH"
    }

    fn description(&self) -> &str {
        "Get length of string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "STRING-LENGTH".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        Ok(Value::Int(s.len() as i64))
    }
}

/// CHAR-AT-INDEX - Alias for CHAR-AT
pub struct CharAtIndexTool;

impl Tool for CharAtIndexTool {
    fn name(&self) -> &str {
        "CHAR-AT-INDEX"
    }

    fn description(&self) -> &str {
        "Get character at index (alias for CHAR-AT)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        CharAtTool.execute(args)
    }
}
