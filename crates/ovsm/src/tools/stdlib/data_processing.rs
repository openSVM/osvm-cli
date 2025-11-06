//! Data processing tools

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

/// Register data processing tools
pub fn register(_registry: &mut ToolRegistry) {
    // IMPORTANT: All basic list/array operations are now built-in language functions
    // in lisp_evaluator.rs. MCP tools should only be for external integrations.

    // Commented out - these are all language builtins now:
    // registry.register(MapTool);      // Built-in: map
    // registry.register(FilterTool);   // Built-in: filter
    // registry.register(ReduceTool);   // Built-in: reduce
    // registry.register(SumTool);      // Should be built-in
    // registry.register(CountTool);    // Built-in: count (alias for length)
    // registry.register(FlattenTool);  // Built-in: flatten
    // registry.register(UniqueTool);   // Built-in: distinct
    // registry.register(SortTool);     // Built-in: sort
    // registry.register(ReverseTool);  // Built-in: reverse
    // registry.register(FirstTool);    // Built-in: first
    // registry.register(LastTool);     // Built-in: last
    // registry.register(AppendTool);   // Built-in: append
    // registry.register(PrependTool);  // Use cons
    // registry.register(SliceTool);    // Built-in: slice
    // registry.register(TopNTool);     // Built-in: take
    // registry.register(BottomNTool);  // Built-in: drop + take
    // registry.register(AnyTool);      // Built-in: some
    // registry.register(AllTool);      // Built-in: every
    // registry.register(FindTool);     // Built-in: find
    // registry.register(JoinTool);     // Built-in: join
    // registry.register(SplitTool);    // Built-in: split
    // registry.register(NthTool);      // Built-in: nth
    // registry.register(IndexOfTool);  // Should be built-in
    // registry.register(TakeTool);     // Built-in: take

    // Common Lisp list accessors - also built-in
    // registry.register(CarTool);      // Built-in: car
    // registry.register(CdrTool);      // Use rest
    // registry.register(CadrTool);     // Compose car/cdr
    // registry.register(CddrTool);     // Compose cdr/cdr
    // registry.register(CaarTool);     // Compose car/car
    // registry.register(CdarTool);     // Compose cdr/car
    // registry.register(RestTool);     // Built-in: rest
    // registry.register(ConsTool);     // Built-in: cons
    // registry.register(ListTool);     // Use array literal []
    // registry.register(LengthTool);   // Built-in: length

    // NOTE: This module should be refactored to only include actual
    // external tools that can't be implemented as language primitives.
    // For now, all tools are disabled to prevent conflicts with built-ins.
}

/// Tool for applying a function to each element of a collection
pub struct MapTool;

impl Tool for MapTool {
    fn name(&self) -> &str {
        "MAP"
    }

    fn description(&self) -> &str {
        "Apply function to each element of collection"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "MAP".to_string(),
                reason: "Expected collection and function".to_string(),
            });
        }

        let _collection = args[0].as_array()?;
        let _func = &args[1];

        // For now, func should be a lambda or tool reference
        // Since we don't have lambda support yet, we'll return an error
        Err(Error::NotImplemented {
            tool: "MAP (lambda support pending)".to_string(),
        })
    }
}

/// Tool for filtering a collection by a predicate function
pub struct FilterTool;

impl Tool for FilterTool {
    fn name(&self) -> &str {
        "FILTER"
    }

    fn description(&self) -> &str {
        "Filter collection by predicate"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "FILTER".to_string(),
                reason: "Expected collection and predicate".to_string(),
            });
        }

        // Placeholder - needs lambda support
        Err(Error::NotImplemented {
            tool: "FILTER (lambda support pending)".to_string(),
        })
    }
}

/// Tool for reducing a collection to a single value
pub struct ReduceTool;

impl Tool for ReduceTool {
    fn name(&self) -> &str {
        "REDUCE"
    }

    fn description(&self) -> &str {
        "Reduce collection to single value"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Err(Error::NotImplemented {
            tool: "REDUCE (lambda support pending)".to_string(),
        })
    }
}

/// Tool for summing all numbers in a collection
pub struct SumTool;

impl Tool for SumTool {
    fn name(&self) -> &str {
        "SUM"
    }

    fn description(&self) -> &str {
        "Sum all numbers in collection"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "SUM".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;

        let mut sum = 0.0;
        for val in collection.iter() {
            sum += val.as_float()?;
        }

        // Return int if it's a whole number, otherwise float
        if sum.fract() == 0.0 {
            Ok(Value::Int(sum as i64))
        } else {
            Ok(Value::Float(sum))
        }
    }
}

/// Tool for counting elements in a collection
pub struct CountTool;

impl Tool for CountTool {
    fn name(&self) -> &str {
        "COUNT"
    }

    fn description(&self) -> &str {
        "Count elements in collection"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "COUNT".to_string(),
                reason: "Expected collection argument".to_string(),
            });
        }

        let count = match &args[0] {
            Value::Array(arr) => arr.len(),
            Value::String(s) => s.len(),
            Value::Object(obj) => obj.len(),
            _ => {
                return Err(Error::TypeError {
                    expected: "array, string, or object".to_string(),
                    got: args[0].type_name(),
                })
            }
        };

        Ok(Value::Int(count as i64))
    }
}

/// Tool for flattening nested arrays into a single array
pub struct FlattenTool;

impl Tool for FlattenTool {
    fn name(&self) -> &str {
        "FLATTEN"
    }

    fn description(&self) -> &str {
        "Flatten nested arrays into single array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FLATTEN".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let mut result = Vec::new();

        for item in collection.iter() {
            match item {
                Value::Array(inner) => {
                    result.extend(inner.iter().cloned());
                }
                other => result.push(other.clone()),
            }
        }

        Ok(Value::array(result))
    }
}

/// Tool for getting unique elements from a collection
pub struct UniqueTool;

impl Tool for UniqueTool {
    fn name(&self) -> &str {
        "UNIQUE"
    }

    fn description(&self) -> &str {
        "Get unique elements from collection"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNIQUE".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let mut result = Vec::new();

        for item in collection.iter() {
            if !result.contains(item) {
                result.push(item.clone());
            }
        }

        Ok(Value::array(result))
    }
}

/// Tool for sorting a collection in ascending order
pub struct SortTool;

impl Tool for SortTool {
    fn name(&self) -> &str {
        "SORT"
    }

    fn description(&self) -> &str {
        "Sort collection in ascending order"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "SORT".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let mut sorted = collection.clone();

        // Simple sort for numbers
        sorted.sort_by(|a, b| match (a, b) {
            (Value::Int(x), Value::Int(y)) => x.cmp(y),
            (Value::Float(x), Value::Float(y)) => {
                x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
            }
            (Value::Int(x), Value::Float(y)) => (*x as f64)
                .partial_cmp(y)
                .unwrap_or(std::cmp::Ordering::Equal),
            (Value::Float(x), Value::Int(y)) => x
                .partial_cmp(&(*y as f64))
                .unwrap_or(std::cmp::Ordering::Equal),
            (Value::String(x), Value::String(y)) => x.cmp(y),
            _ => std::cmp::Ordering::Equal,
        });

        Ok(Value::array(sorted))
    }
}

/// Tool for reversing the order of elements in a collection
pub struct ReverseTool;

impl Tool for ReverseTool {
    fn name(&self) -> &str {
        "REVERSE"
    }

    fn description(&self) -> &str {
        "Reverse the order of elements"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "REVERSE".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let mut reversed = collection.clone();
        reversed.reverse();

        Ok(Value::array(reversed))
    }
}

/// Tool for getting the first element of a collection
pub struct FirstTool;

impl Tool for FirstTool {
    fn name(&self) -> &str {
        "FIRST"
    }

    fn description(&self) -> &str {
        "Get first element of collection"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FIRST".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        collection
            .first()
            .cloned()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "FIRST".to_string(),
            })
    }
}

/// Tool for getting the last element of a collection
pub struct LastTool;

impl Tool for LastTool {
    fn name(&self) -> &str {
        "LAST"
    }

    fn description(&self) -> &str {
        "Get last element of collection"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "LAST".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        collection
            .last()
            .cloned()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "LAST".to_string(),
            })
    }
}

/// Tool for appending an element to the end of an array
pub struct AppendTool;

impl Tool for AppendTool {
    fn name(&self) -> &str {
        "APPEND"
    }

    fn description(&self) -> &str {
        "Append element to array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "APPEND".to_string(),
                reason: "Expected array and element".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let mut result = collection.clone();
        result.push(args[1].clone());

        Ok(Value::array(result))
    }
}

/// Tool for prepending an element to the beginning of an array
pub struct PrependTool;

impl Tool for PrependTool {
    fn name(&self) -> &str {
        "PREPEND"
    }

    fn description(&self) -> &str {
        "Prepend element to array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "PREPEND".to_string(),
                reason: "Expected array and element".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let mut result = vec![args[1].clone()];
        result.extend(collection.iter().cloned());

        Ok(Value::array(result))
    }
}

/// Tool for extracting a slice from an array
pub struct SliceTool;

impl Tool for SliceTool {
    fn name(&self) -> &str {
        "SLICE"
    }

    fn description(&self) -> &str {
        "Extract slice from array (start, end)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "SLICE".to_string(),
                reason: "Expected array, start, and end".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let start = args[1].as_int()? as usize;
        let end = args[2].as_int()? as usize;

        if start > collection.len() || end > collection.len() || start > end {
            return Err(Error::InvalidArguments {
                tool: "SLICE".to_string(),
                reason: "Invalid slice range".to_string(),
            });
        }

        let result = collection[start..end].to_vec();
        Ok(Value::array(result))
    }
}

/// Tool for getting the first N elements from an array
pub struct TopNTool;

impl Tool for TopNTool {
    fn name(&self) -> &str {
        "TOP_N"
    }

    fn description(&self) -> &str {
        "Get top N elements from array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "TOP_N".to_string(),
                reason: "Expected array and count".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let n = args[1].as_int()? as usize;

        let result: Vec<Value> = collection.iter().take(n).cloned().collect();
        Ok(Value::array(result))
    }
}

/// Tool for getting the last N elements from an array
pub struct BottomNTool;

impl Tool for BottomNTool {
    fn name(&self) -> &str {
        "BOTTOM_N"
    }

    fn description(&self) -> &str {
        "Get bottom N elements from array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "BOTTOM_N".to_string(),
                reason: "Expected array and count".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let n = args[1].as_int()? as usize;

        let skip = if collection.len() > n {
            collection.len() - n
        } else {
            0
        };

        let result: Vec<Value> = collection.iter().skip(skip).cloned().collect();
        Ok(Value::array(result))
    }
}

/// Tool for checking if any element in a collection is truthy
pub struct AnyTool;

impl Tool for AnyTool {
    fn name(&self) -> &str {
        "ANY"
    }

    fn description(&self) -> &str {
        "Check if any element is truthy"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ANY".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let any_truthy = collection.iter().any(|v| v.is_truthy());
        Ok(Value::Bool(any_truthy))
    }
}

/// Tool for checking if all elements in a collection are truthy
pub struct AllTool;

impl Tool for AllTool {
    fn name(&self) -> &str {
        "ALL"
    }

    fn description(&self) -> &str {
        "Check if all elements are truthy"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ALL".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let all_truthy = collection.iter().all(|v| v.is_truthy());
        Ok(Value::Bool(all_truthy))
    }
}

/// Tool for finding the index of the first matching element in an array
pub struct FindTool;

impl Tool for FindTool {
    fn name(&self) -> &str {
        "FIND"
    }

    fn description(&self) -> &str {
        "Find first matching element in array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "FIND".to_string(),
                reason: "Expected array and value".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let target = &args[1];

        for (index, item) in collection.iter().enumerate() {
            if item == target {
                return Ok(Value::Int(index as i64));
            }
        }

        Ok(Value::Int(-1)) // Return -1 if not found
    }
}

/// Tool for joining array elements into a string with a separator
pub struct JoinTool;

impl Tool for JoinTool {
    fn name(&self) -> &str {
        "JOIN"
    }

    fn description(&self) -> &str {
        "Join array elements into string with separator"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "JOIN".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        let separator = if args.len() > 1 {
            args[1].to_string_value()
        } else {
            ",".to_string()
        };

        let strings: Vec<String> = collection.iter().map(|v| v.to_string_value()).collect();
        let result = strings.join(&separator);

        Ok(Value::String(result))
    }
}

/// Tool for splitting a string into an array by a separator
pub struct SplitTool;

impl Tool for SplitTool {
    fn name(&self) -> &str {
        "SPLIT"
    }

    fn description(&self) -> &str {
        "Split string into array by separator"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "SPLIT".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        let string = args[0].as_string()?;
        let separator = if args.len() > 1 {
            args[1].as_string()?
        } else {
            ","
        };

        let parts: Vec<Value> = string
            .split(separator)
            .map(|s| Value::String(s.to_string()))
            .collect();

        Ok(Value::array(parts))
    }
}

/// Tool for getting nth element from array
pub struct NthTool;

impl Tool for NthTool {
    fn name(&self) -> &str {
        "NTH"
    }

    fn description(&self) -> &str {
        "Get nth element from array (0-indexed)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "NTH".to_string(),
                reason: "Expected array and index".to_string(),
            });
        }

        let array = args[0].as_array()?;
        let index = args[1].as_int()? as usize;

        array
            .get(index)
            .cloned()
            .ok_or_else(|| Error::InvalidArguments {
                tool: "NTH".to_string(),
                reason: format!(
                    "Index {} out of bounds (array length: {})",
                    index,
                    array.len()
                ),
            })
    }
}

/// Tool for finding index of element in array
pub struct IndexOfTool;

impl Tool for IndexOfTool {
    fn name(&self) -> &str {
        "INDEXOF"
    }

    fn description(&self) -> &str {
        "Find index of element in array, returns -1 if not found"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "INDEXOF".to_string(),
                reason: "Expected array and value to find".to_string(),
            });
        }

        let array = args[0].as_array()?;
        let target = &args[1];

        for (i, val) in array.iter().enumerate() {
            if val == target {
                return Ok(Value::Int(i as i64));
            }
        }

        Ok(Value::Int(-1))
    }
}

/// Tool for taking first N elements (alias for TopN)
pub struct TakeTool;

impl Tool for TakeTool {
    fn name(&self) -> &str {
        "TAKE"
    }

    fn description(&self) -> &str {
        "Take first N elements from array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "TAKE".to_string(),
                reason: "Expected array and count".to_string(),
            });
        }

        let array = args[0].as_array()?;
        let n = args[1].as_int()? as usize;

        let taken: Vec<Value> = array.iter().take(n).cloned().collect();
        Ok(Value::array(taken))
    }
}

// ============================================================================
// Common Lisp List Accessors
// ============================================================================

/// CAR - get first element of list (same as FIRST)
pub struct CarTool;

impl Tool for CarTool {
    fn name(&self) -> &str {
        "CAR"
    }

    fn description(&self) -> &str {
        "Get first element of list (Common Lisp)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CAR".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        collection
            .first()
            .cloned()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CAR".to_string(),
            })
    }
}

/// CDR - get all elements except first (same as REST)
pub struct CdrTool;

impl Tool for CdrTool {
    fn name(&self) -> &str {
        "CDR"
    }

    fn description(&self) -> &str {
        "Get all elements except first (Common Lisp)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CDR".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        if collection.is_empty() {
            Ok(Value::array(vec![]))
        } else {
            Ok(Value::array(collection[1..].to_vec()))
        }
    }
}

/// REST - alias for CDR
pub struct RestTool;

impl Tool for RestTool {
    fn name(&self) -> &str {
        "REST"
    }

    fn description(&self) -> &str {
        "Get all elements except first (alias for CDR)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        CdrTool.execute(args)
    }
}

/// CADR - get second element (car of cdr)
pub struct CadrTool;

impl Tool for CadrTool {
    fn name(&self) -> &str {
        "CADR"
    }

    fn description(&self) -> &str {
        "Get second element of list (CAR of CDR)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CADR".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        if collection.len() < 2 {
            return Err(Error::IndexOutOfBounds {
                index: 1,
                length: collection.len(),
            });
        }
        Ok(collection[1].clone())
    }
}

/// CDDR - get all elements except first two
pub struct CddrTool;

impl Tool for CddrTool {
    fn name(&self) -> &str {
        "CDDR"
    }

    fn description(&self) -> &str {
        "Get all elements except first two (CDR of CDR)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CDDR".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        if collection.len() < 2 {
            Ok(Value::array(vec![]))
        } else {
            Ok(Value::array(collection[2..].to_vec()))
        }
    }
}

/// CAAR - get first element of first element
pub struct CaarTool;

impl Tool for CaarTool {
    fn name(&self) -> &str {
        "CAAR"
    }

    fn description(&self) -> &str {
        "Get first element of first element (CAR of CAR)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CAAR".to_string(),
                reason: "Expected nested array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        if collection.is_empty() {
            return Err(Error::EmptyCollection {
                operation: "CAAR".to_string(),
            });
        }

        let first_elem = collection.first().unwrap().as_array()?;
        first_elem
            .first()
            .cloned()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CAAR".to_string(),
            })
    }
}

/// CDAR - get rest of first element
pub struct CdarTool;

impl Tool for CdarTool {
    fn name(&self) -> &str {
        "CDAR"
    }

    fn description(&self) -> &str {
        "Get rest of first element (CDR of CAR)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CDAR".to_string(),
                reason: "Expected nested array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        if collection.is_empty() {
            return Err(Error::EmptyCollection {
                operation: "CDAR".to_string(),
            });
        }

        let first_elem = collection.first().unwrap().as_array()?;
        if first_elem.is_empty() {
            Ok(Value::array(vec![]))
        } else {
            Ok(Value::array(first_elem[1..].to_vec()))
        }
    }
}

/// CONS - construct a new list by prepending an element
pub struct ConsTool;

impl Tool for ConsTool {
    fn name(&self) -> &str {
        "CONS"
    }

    fn description(&self) -> &str {
        "Construct new list by prepending element"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "CONS".to_string(),
                reason: "Expected element and list".to_string(),
            });
        }

        let elem = &args[0];
        let list = args[1].as_array()?;

        let mut result = vec![elem.clone()];
        result.extend(list.iter().cloned());
        Ok(Value::array(result))
    }
}

/// LIST - create a list from arguments
pub struct ListTool;

impl Tool for ListTool {
    fn name(&self) -> &str {
        "LIST"
    }

    fn description(&self) -> &str {
        "Create a list from arguments"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::array(args.to_vec()))
    }
}

/// LENGTH - get length of collection
pub struct LengthTool;

impl Tool for LengthTool {
    fn name(&self) -> &str {
        "LENGTH"
    }

    fn description(&self) -> &str {
        "Get length of array or string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "LENGTH".to_string(),
                reason: "Expected collection argument".to_string(),
            });
        }

        let len = match &args[0] {
            Value::Array(arr) => arr.len(),
            Value::String(s) => s.len(),
            Value::Object(obj) => obj.len(),
            _ => {
                return Err(Error::TypeError {
                    expected: "array, string, or object".to_string(),
                    got: args[0].type_name(),
                })
            }
        };

        Ok(Value::Int(len as i64))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sum_tool() {
        let tool = SumTool;
        let arr = Value::array(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = tool.execute(&[arr]).unwrap();
        assert_eq!(result, Value::Int(6));
    }

    #[test]
    fn test_count_tool() {
        let tool = CountTool;
        let arr = Value::array(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = tool.execute(&[arr]).unwrap();
        assert_eq!(result, Value::Int(3));
    }

    #[test]
    fn test_flatten_tool() {
        let tool = FlattenTool;
        let nested = Value::array(vec![
            Value::array(vec![Value::Int(1), Value::Int(2)]),
            Value::array(vec![Value::Int(3), Value::Int(4)]),
        ]);
        let result = tool.execute(&[nested]).unwrap();
        let expected = vec![Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(4)];
        assert_eq!(result, expected);
    }

    #[test]
    fn test_unique_tool() {
        let tool = UniqueTool;
        let arr = Value::array(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(2),
            Value::Int(3),
        ]);
        let result = tool.execute(&[arr]).unwrap();
        let expected = vec![Value::Int(1), Value::Int(2), Value::Int(3)];
        assert_eq!(result, expected);
    }

    #[test]
    fn test_reverse_tool() {
        let tool = ReverseTool;
        let arr = Value::array(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = tool.execute(&[arr]).unwrap();
        let expected = vec![Value::Int(3), Value::Int(2), Value::Int(1)];
        assert_eq!(result, expected);
    }

    #[test]
    fn test_first_last_tools() {
        let arr = Value::array(vec![Value::Int(10), Value::Int(20), Value::Int(30)]);

        let first_tool = FirstTool;
        assert_eq!(
            first_tool.execute(std::slice::from_ref(&arr)).unwrap(),
            Value::Int(10)
        );

        let last_tool = LastTool;
        assert_eq!(last_tool.execute(&[arr]).unwrap(), Value::Int(30));
    }
}
