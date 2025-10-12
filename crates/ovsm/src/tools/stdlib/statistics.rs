//! Statistical tools

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

/// Register statistical tools
pub fn register(registry: &mut ToolRegistry) {
    registry.register(MeanTool);
    registry.register(MedianTool);
    registry.register(MinTool);
    registry.register(MaxTool);
    registry.register(StdDevTool);
}

/// Tool for calculating arithmetic mean of a collection
pub struct MeanTool;

impl Tool for MeanTool {
    fn name(&self) -> &str {
        "MEAN"
    }

    fn description(&self) -> &str {
        "Calculate arithmetic mean"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MEAN".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        if collection.is_empty() {
            return Err(Error::EmptyCollection {
                operation: "MEAN".to_string(),
            });
        }

        let mut sum = 0.0;
        for val in collection.iter() {
            sum += val.as_float()?;
        }

        Ok(Value::Float(sum / collection.len() as f64))
    }
}

/// Tool for calculating median value of a collection
pub struct MedianTool;

impl Tool for MedianTool {
    fn name(&self) -> &str {
        "MEDIAN"
    }

    fn description(&self) -> &str {
        "Calculate median value"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MEDIAN".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        if collection.is_empty() {
            return Err(Error::EmptyCollection {
                operation: "MEDIAN".to_string(),
            });
        }

        let mut sorted: Vec<f64> = collection
            .iter()
            .map(|v| v.as_float())
            .collect::<Result<Vec<_>>>()?;
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());

        #[allow(unknown_lints)]
        #[allow(clippy::manual_is_multiple_of)]
        let median = if sorted.len() % 2 == 0 {
            let mid = sorted.len() / 2;
            (sorted[mid - 1] + sorted[mid]) / 2.0
        } else {
            sorted[sorted.len() / 2]
        };

        Ok(Value::Float(median))
    }
}

// MIN tool
/// Tool for finding the minimum value in a collection
///
/// Usage: `MIN(array) -> number`
/// Example: `MIN([5, 2, 8, 1])` returns `1.0`
pub struct MinTool;

impl Tool for MinTool {
    fn name(&self) -> &str {
        "MIN"
    }

    fn description(&self) -> &str {
        "Find minimum value"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MIN".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        if collection.is_empty() {
            return Err(Error::EmptyCollection {
                operation: "MIN".to_string(),
            });
        }

        let mut min = collection[0].as_float()?;
        for val in collection.iter().skip(1) {
            let v = val.as_float()?;
            if v < min {
                min = v;
            }
        }

        Ok(Value::Float(min))
    }
}

// MAX tool
/// Tool for finding the maximum value in a collection
///
/// Usage: `MAX(array) -> number`
/// Example: `MAX([5, 2, 8, 1])` returns `8.0`
pub struct MaxTool;

impl Tool for MaxTool {
    fn name(&self) -> &str {
        "MAX"
    }

    fn description(&self) -> &str {
        "Find maximum value"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MAX".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        if collection.is_empty() {
            return Err(Error::EmptyCollection {
                operation: "MAX".to_string(),
            });
        }

        let mut max = collection[0].as_float()?;
        for val in collection.iter().skip(1) {
            let v = val.as_float()?;
            if v > max {
                max = v;
            }
        }

        Ok(Value::Float(max))
    }
}

// STDDEV tool (standard deviation)
/// Tool for calculating the standard deviation of a collection
pub struct StdDevTool;

impl Tool for StdDevTool {
    fn name(&self) -> &str {
        "STDDEV"
    }

    fn description(&self) -> &str {
        "Calculate standard deviation"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "STDDEV".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let collection = args[0].as_array()?;
        if collection.is_empty() {
            return Err(Error::EmptyCollection {
                operation: "STDDEV".to_string(),
            });
        }

        // Calculate mean
        let mut sum = 0.0;
        let values: Vec<f64> = collection
            .iter()
            .map(|v| v.as_float())
            .collect::<Result<Vec<_>>>()?;

        for &v in &values {
            sum += v;
        }
        let mean = sum / values.len() as f64;

        // Calculate variance
        let mut variance = 0.0;
        for &v in &values {
            let diff = v - mean;
            variance += diff * diff;
        }
        variance /= values.len() as f64;

        Ok(Value::Float(variance.sqrt()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mean() {
        let tool = MeanTool;
        let arr = Value::array(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = tool.execute(&[arr]).unwrap();
        assert_eq!(result, Value::Float(2.0));
    }

    #[test]
    fn test_median_odd() {
        let tool = MedianTool;
        let arr = Value::array(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = tool.execute(&[arr]).unwrap();
        assert_eq!(result, Value::Float(2.0));
    }

    #[test]
    fn test_median_even() {
        let tool = MedianTool;
        let arr = Value::array(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4),
        ]);
        let result = tool.execute(&[arr]).unwrap();
        assert_eq!(result, Value::Float(2.5));
    }

    #[test]
    fn test_min_max() {
        let arr = Value::array(vec![
            Value::Int(5),
            Value::Int(2),
            Value::Int(8),
            Value::Int(1),
        ]);

        let min_tool = MinTool;
        assert_eq!(
            min_tool.execute(std::slice::from_ref(&arr)).unwrap(),
            Value::Float(1.0)
        );

        let max_tool = MaxTool;
        assert_eq!(max_tool.execute(&[arr]).unwrap(), Value::Float(8.0));
    }
}
