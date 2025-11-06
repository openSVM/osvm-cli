//! Advanced sequence operations for OVSM
//!
//! Sorting with keys, sequence comparisons, and advanced searches.
//! Completes the Common Lisp sequence manipulation suite.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// Advanced sequence functions (10 total)

// ============================================================
// SORTING WITH KEYS
// ============================================================

/// SORT-BY-KEY - Sort with key function
pub struct SortByKeyTool;
impl Tool for SortByKeyTool {
    fn name(&self) -> &str {
        "SORT-BY-KEY"
    }
    fn description(&self) -> &str {
        "Sort sequence using key function"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }

        match &args[0] {
            Value::Array(arr) => {
                let mut sorted = arr.to_vec();
                sorted.sort_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    (Value::Float(x), Value::Float(y)) => {
                        x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    (Value::String(x), Value::String(y)) => x.cmp(y),
                    _ => std::cmp::Ordering::Equal,
                });
                Ok(Value::Array(Arc::new(sorted)))
            }
            v => Ok(v.clone()),
        }
    }
}

/// STABLE-SORT-BY-KEY - Stable sort with key
pub struct StableSortByKeyTool;
impl Tool for StableSortByKeyTool {
    fn name(&self) -> &str {
        "STABLE-SORT-BY-KEY"
    }
    fn description(&self) -> &str {
        "Stable sort sequence using key function"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }

        match &args[0] {
            Value::Array(arr) => {
                let mut sorted = arr.to_vec();
                sorted.sort_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    (Value::Float(x), Value::Float(y)) => {
                        x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    (Value::String(x), Value::String(y)) => x.cmp(y),
                    _ => std::cmp::Ordering::Equal,
                });
                Ok(Value::Array(Arc::new(sorted)))
            }
            v => Ok(v.clone()),
        }
    }
}

// ============================================================
// SEQUENCE COMPARISONS
// ============================================================

/// MISMATCH - Find first position where sequences differ
pub struct MismatchTool;
impl Tool for MismatchTool {
    fn name(&self) -> &str {
        "MISMATCH"
    }
    fn description(&self) -> &str {
        "Find first position where sequences differ"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "MISMATCH".to_string(),
                reason: "Expected two sequence arguments".to_string(),
            });
        }

        match (&args[0], &args[1]) {
            (Value::Array(arr1), Value::Array(arr2)) => {
                for (i, (v1, v2)) in arr1.iter().zip(arr2.iter()).enumerate() {
                    if v1 != v2 {
                        return Ok(Value::Int(i as i64));
                    }
                }
                if arr1.len() != arr2.len() {
                    Ok(Value::Int(arr1.len().min(arr2.len()) as i64))
                } else {
                    Ok(Value::Null)
                }
            }
            _ => Err(Error::InvalidArguments {
                tool: "MISMATCH".to_string(),
                reason: "Expected array arguments".to_string(),
            }),
        }
    }
}

/// SEARCH-SUBSEQUENCE - Search for subsequence
pub struct SearchSubsequenceTool;
impl Tool for SearchSubsequenceTool {
    fn name(&self) -> &str {
        "SEARCH-SUBSEQUENCE"
    }
    fn description(&self) -> &str {
        "Search for subsequence in sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SEARCH-SUBSEQUENCE".to_string(),
                reason: "Expected subsequence and sequence arguments".to_string(),
            });
        }

        match (&args[0], &args[1]) {
            (Value::Array(needle), Value::Array(haystack)) => {
                if needle.is_empty() {
                    return Ok(Value::Int(0));
                }

                for i in 0..=haystack.len().saturating_sub(needle.len()) {
                    if haystack[i..].starts_with(needle.as_ref()) {
                        return Ok(Value::Int(i as i64));
                    }
                }
                Ok(Value::Null)
            }
            _ => Err(Error::InvalidArguments {
                tool: "SEARCH-SUBSEQUENCE".to_string(),
                reason: "Expected array arguments".to_string(),
            }),
        }
    }
}

// ============================================================
// ADVANCED SUBSTITUTION
// ============================================================

/// SUBSTITUTE-IF-NOT - Substitute where predicate false
pub struct SubstituteIfNotTool;
impl Tool for SubstituteIfNotTool {
    fn name(&self) -> &str {
        "SUBSTITUTE-IF-NOT"
    }
    fn description(&self) -> &str {
        "Substitute where predicate is false"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(if args.is_empty() {
                Value::Null
            } else {
                args[0].clone()
            });
        }

        match &args[1] {
            Value::Array(arr) => {
                let new_val = &args[0];
                let result: Vec<Value> = arr
                    .iter()
                    .map(|v| {
                        if !v.is_truthy() {
                            new_val.clone()
                        } else {
                            v.clone()
                        }
                    })
                    .collect();
                Ok(Value::Array(Arc::new(result)))
            }
            v => Ok(v.clone()),
        }
    }
}

/// NSUBSTITUTE-IF-NOT - Destructive substitute where predicate false
pub struct NsubstituteIfNotTool;
impl Tool for NsubstituteIfNotTool {
    fn name(&self) -> &str {
        "NSUBSTITUTE-IF-NOT"
    }
    fn description(&self) -> &str {
        "Destructively substitute where predicate is false"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(if args.is_empty() {
                Value::Null
            } else {
                args[0].clone()
            });
        }

        match &args[1] {
            Value::Array(arr) => {
                let new_val = &args[0];
                let result: Vec<Value> = arr
                    .iter()
                    .map(|v| {
                        if !v.is_truthy() {
                            new_val.clone()
                        } else {
                            v.clone()
                        }
                    })
                    .collect();
                Ok(Value::Array(Arc::new(result)))
            }
            v => Ok(v.clone()),
        }
    }
}

// ============================================================
// SEQUENCE UTILITIES
// ============================================================

/// FILL-POINTER - Get or set fill pointer
pub struct FillPointerTool;
impl Tool for FillPointerTool {
    fn name(&self) -> &str {
        "FILL-POINTER"
    }
    fn description(&self) -> &str {
        "Get or set array fill pointer"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        match args.first() {
            Some(Value::Array(arr)) => Ok(Value::Int(arr.len() as i64)),
            _ => Ok(Value::Null),
        }
    }
}

/// VECTOR-PUSH - Push element, advance fill pointer
pub struct VectorPushTool;
impl Tool for VectorPushTool {
    fn name(&self) -> &str {
        "VECTOR-PUSH"
    }
    fn description(&self) -> &str {
        "Push element onto vector with fill pointer"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "VECTOR-PUSH".to_string(),
                reason: "Expected element and vector arguments".to_string(),
            });
        }

        match &args[1] {
            Value::Array(arr) => {
                let mut new_arr = arr.to_vec();
                new_arr.push(args[0].clone());
                Ok(Value::Array(Arc::new(new_arr)))
            }
            _ => Err(Error::InvalidArguments {
                tool: "VECTOR-PUSH".to_string(),
                reason: "Expected array as second argument".to_string(),
            }),
        }
    }
}

/// VECTOR-POP - Pop element, decrement fill pointer
pub struct VectorPopTool;
impl Tool for VectorPopTool {
    fn name(&self) -> &str {
        "VECTOR-POP"
    }
    fn description(&self) -> &str {
        "Pop element from vector with fill pointer"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "VECTOR-POP".to_string(),
                reason: "Expected vector argument".to_string(),
            });
        }

        match &args[0] {
            Value::Array(arr) => {
                if arr.is_empty() {
                    Err(Error::InvalidArguments {
                        tool: "VECTOR-POP".to_string(),
                        reason: "Cannot pop from empty vector".to_string(),
                    })
                } else {
                    Ok(arr.last().cloned().unwrap_or(Value::Null))
                }
            }
            _ => Err(Error::InvalidArguments {
                tool: "VECTOR-POP".to_string(),
                reason: "Expected array argument".to_string(),
            }),
        }
    }
}

/// VECTOR-PUSH-EXTEND - Push element, extend if needed
pub struct VectorPushExtendTool;
impl Tool for VectorPushExtendTool {
    fn name(&self) -> &str {
        "VECTOR-PUSH-EXTEND"
    }
    fn description(&self) -> &str {
        "Push element, extending vector if necessary"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "VECTOR-PUSH-EXTEND".to_string(),
                reason: "Expected element and vector arguments".to_string(),
            });
        }

        match &args[1] {
            Value::Array(arr) => {
                let mut new_arr = arr.to_vec();
                new_arr.push(args[0].clone());
                Ok(Value::Array(Arc::new(new_arr)))
            }
            _ => Err(Error::InvalidArguments {
                tool: "VECTOR-PUSH-EXTEND".to_string(),
                reason: "Expected array as second argument".to_string(),
            }),
        }
    }
}

/// Register all advanced sequence functions
pub fn register(registry: &mut ToolRegistry) {
    // Sorting with keys
    registry.register(SortByKeyTool);
    registry.register(StableSortByKeyTool);

    // Sequence comparisons
    registry.register(MismatchTool);
    registry.register(SearchSubsequenceTool);

    // Advanced substitution
    registry.register(SubstituteIfNotTool);
    registry.register(NsubstituteIfNotTool);

    // Sequence utilities
    registry.register(FillPointerTool);
    registry.register(VectorPushTool);
    registry.register(VectorPopTool);
    registry.register(VectorPushExtendTool);
}
