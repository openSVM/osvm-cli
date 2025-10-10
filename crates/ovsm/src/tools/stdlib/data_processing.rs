//! Data processing tools

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

/// Register data processing tools
pub fn register(registry: &mut ToolRegistry) {
    registry.register(MapTool);
    registry.register(FilterTool);
    registry.register(ReduceTool);
    registry.register(SumTool);
    registry.register(CountTool);
    registry.register(FlattenTool);
    registry.register(UniqueTool);
    registry.register(SortTool);
    registry.register(ReverseTool);
    registry.register(FirstTool);
    registry.register(LastTool);
    registry.register(AppendTool);
    registry.register(PrependTool);
    registry.register(SliceTool);
    registry.register(TopNTool);
    registry.register(BottomNTool);
    registry.register(AnyTool);
    registry.register(AllTool);
    registry.register(FindTool);
    registry.register(JoinTool);
    registry.register(SplitTool);
}

// MAP tool
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

        let collection = args[0].as_array()?;
        let func = &args[1];

        // For now, func should be a lambda or tool reference
        // Since we don't have lambda support yet, we'll return an error
        Err(Error::NotImplemented {
            tool: "MAP (lambda support pending)".to_string(),
        })
    }
}

// FILTER tool
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

// REDUCE tool
pub struct ReduceTool;

impl Tool for ReduceTool {
    fn name(&self) -> &str {
        "REDUCE"
    }

    fn description(&self) -> &str {
        "Reduce collection to single value"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        Err(Error::NotImplemented {
            tool: "REDUCE (lambda support pending)".to_string(),
        })
    }
}

// SUM tool
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

// COUNT tool
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

// FLATTEN tool
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

// UNIQUE tool
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

// SORT tool
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
        sorted.sort_by(|a, b| {
            match (a, b) {
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                (Value::Float(x), Value::Float(y)) => {
                    x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
                }
                (Value::Int(x), Value::Float(y)) => {
                    (*x as f64).partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
                }
                (Value::Float(x), Value::Int(y)) => {
                    x.partial_cmp(&(*y as f64)).unwrap_or(std::cmp::Ordering::Equal)
                }
                (Value::String(x), Value::String(y)) => x.cmp(y),
                _ => std::cmp::Ordering::Equal,
            }
        });

        Ok(Value::array(sorted))
    }
}

// REVERSE tool
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

// FIRST tool
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
        collection.first().cloned().ok_or_else(|| {
            Error::EmptyCollection {
                operation: "FIRST".to_string(),
            }
        })
    }
}

// LAST tool
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
        collection.last().cloned().ok_or_else(|| {
            Error::EmptyCollection {
                operation: "LAST".to_string(),
            }
        })
    }
}

// APPEND tool
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
        assert_eq!(first_tool.execute(&[arr.clone()]).unwrap(), Value::Int(10));

        let last_tool = LastTool;
        assert_eq!(last_tool.execute(&[arr]).unwrap(), Value::Int(30));
    }
}
