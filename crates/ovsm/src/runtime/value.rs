use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use crate::error::{Error, Result};

/// Runtime value representation
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    // Primitives
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),

    // Collections (use Arc for large values)
    Array(Arc<Vec<Value>>),
    Object(Arc<HashMap<String, Value>>),

    // Special
    Range { start: i64, end: i64 },
}

impl Value {
    /// Create an array value
    pub fn array(values: Vec<Value>) -> Self {
        Value::Array(Arc::new(values))
    }

    /// Create an object value
    pub fn object(fields: HashMap<String, Value>) -> Self {
        Value::Object(Arc::new(fields))
    }

    /// Get type name as string
    pub fn type_name(&self) -> String {
        match self {
            Value::Null => "null".to_string(),
            Value::Bool(_) => "bool".to_string(),
            Value::Int(_) => "int".to_string(),
            Value::Float(_) => "float".to_string(),
            Value::String(_) => "string".to_string(),
            Value::Array(_) => "array".to_string(),
            Value::Object(_) => "object".to_string(),
            Value::Range { .. } => "range".to_string(),
        }
    }

    /// Check if value is truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Int(n) => *n != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array(arr) => !arr.is_empty(),
            Value::Object(obj) => !obj.is_empty(),
            Value::Range { .. } => true,
        }
    }

    // Type conversion methods

    /// Convert to boolean
    pub fn as_bool(&self) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            Value::Int(n) => Ok(*n != 0),
            Value::Float(f) => Ok(*f != 0.0),
            Value::Null => Ok(false),
            _ => Err(Error::TypeError {
                expected: "bool".to_string(),
                got: self.type_name(),
            }),
        }
    }

    /// Convert to integer
    pub fn as_int(&self) -> Result<i64> {
        match self {
            Value::Int(n) => Ok(*n),
            Value::Float(f) => Ok(*f as i64),
            Value::Bool(b) => Ok(if *b { 1 } else { 0 }),
            Value::String(s) => s.parse().map_err(|_| Error::TypeError {
                expected: "int".to_string(),
                got: self.type_name(),
            }),
            _ => Err(Error::TypeError {
                expected: "int".to_string(),
                got: self.type_name(),
            }),
        }
    }

    /// Convert to float
    pub fn as_float(&self) -> Result<f64> {
        match self {
            Value::Float(f) => Ok(*f),
            Value::Int(n) => Ok(*n as f64),
            Value::String(s) => s.parse().map_err(|_| Error::TypeError {
                expected: "float".to_string(),
                got: self.type_name(),
            }),
            _ => Err(Error::TypeError {
                expected: "float".to_string(),
                got: self.type_name(),
            }),
        }
    }

    /// Convert to string
    pub fn as_string(&self) -> Result<&str> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err(Error::TypeError {
                expected: "string".to_string(),
                got: self.type_name(),
            }),
        }
    }

    /// Get string owned
    pub fn to_string_value(&self) -> String {
        match self {
            Value::Null => "null".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Int(n) => n.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => s.clone(),
            Value::Array(arr) => format!("[{} items]", arr.len()),
            Value::Object(obj) => format!("{{{}  fields}}", obj.len()),
            Value::Range { start, end } => format!("[{}..{}]", start, end),
        }
    }

    /// Convert to array reference
    pub fn as_array(&self) -> Result<&Vec<Value>> {
        match self {
            Value::Array(arr) => Ok(arr),
            _ => Err(Error::TypeError {
                expected: "array".to_string(),
                got: self.type_name(),
            }),
        }
    }

    /// Convert to object reference
    pub fn as_object(&self) -> Result<&HashMap<String, Value>> {
        match self {
            Value::Object(obj) => Ok(obj),
            _ => Err(Error::TypeError {
                expected: "object".to_string(),
                got: self.type_name(),
            }),
        }
    }

    /// Get field from object
    pub fn get_field(&self, field: &str) -> Result<Value> {
        match self {
            Value::Object(obj) => obj
                .get(field)
                .cloned()
                .ok_or_else(|| Error::UndefinedVariable {
                    name: field.to_string(),
                }),
            _ => Err(Error::TypeError {
                expected: "object".to_string(),
                got: self.type_name(),
            }),
        }
    }

    /// Get element at index
    pub fn get_index(&self, index: &Value) -> Result<Value> {
        match self {
            Value::Array(arr) => {
                let idx = index.as_int()? as usize;
                if idx >= arr.len() {
                    return Err(Error::IndexOutOfBounds {
                        index: idx,
                        length: arr.len(),
                    });
                }
                Ok(arr[idx].clone())
            }
            Value::String(s) => {
                let idx = index.as_int()? as usize;
                if idx >= s.len() {
                    return Err(Error::IndexOutOfBounds {
                        index: idx,
                        length: s.len(),
                    });
                }
                Ok(Value::String(s.chars().nth(idx).unwrap().to_string()))
            }
            _ => Err(Error::TypeError {
                expected: "array or string".to_string(),
                got: self.type_name(),
            }),
        }
    }

    /// Expand range to array
    pub fn expand_range(&self) -> Result<Vec<Value>> {
        match self {
            Value::Range { start, end } => {
                let mut result = Vec::new();
                for i in *start..*end {
                    result.push(Value::Int(i));
                }
                Ok(result)
            }
            _ => Err(Error::TypeError {
                expected: "range".to_string(),
                got: self.type_name(),
            }),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Array(arr) => {
                write!(f, "[")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }
            Value::Object(obj) => {
                write!(f, "{{")?;
                for (i, (key, val)) in obj.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, val)?;
                }
                write!(f, "}}")
            }
            Value::Range { start, end } => write!(f, "[{}..{}]", start, end),
        }
    }
}

// Implement equality for objects that contain Arc
impl PartialEq<Vec<Value>> for Value {
    fn eq(&self, other: &Vec<Value>) -> bool {
        match self {
            Value::Array(arr) => arr.as_ref() == other,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_names() {
        assert_eq!(Value::Null.type_name(), "null");
        assert_eq!(Value::Bool(true).type_name(), "bool");
        assert_eq!(Value::Int(42).type_name(), "int");
        assert_eq!(Value::Float(2.71).type_name(), "float");
        assert_eq!(Value::String("test".to_string()).type_name(), "string");
    }

    #[test]
    fn test_truthiness() {
        assert!(!Value::Null.is_truthy());
        assert!(!Value::Bool(false).is_truthy());
        assert!(Value::Bool(true).is_truthy());
        assert!(!Value::Int(0).is_truthy());
        assert!(Value::Int(42).is_truthy());
        assert!(!Value::String(String::new()).is_truthy());
        assert!(Value::String("test".to_string()).is_truthy());
    }

    #[test]
    fn test_conversions() {
        let v = Value::Int(42);
        assert_eq!(v.as_int().unwrap(), 42);
        assert_eq!(v.as_float().unwrap(), 42.0);
        assert!(v.as_bool().unwrap());

        let v = Value::Float(3.15);
        assert_eq!(v.as_float().unwrap(), 3.15);
        assert_eq!(v.as_int().unwrap(), 3);

        let v = Value::String("test".to_string());
        assert_eq!(v.as_string().unwrap(), "test");
    }

    #[test]
    fn test_array_operations() {
        let arr = Value::array(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        assert_eq!(arr.as_array().unwrap().len(), 3);

        let elem = arr.get_index(&Value::Int(1)).unwrap();
        assert_eq!(elem, Value::Int(2));
    }

    #[test]
    fn test_object_operations() {
        let mut fields = HashMap::new();
        fields.insert("name".to_string(), Value::String("Alice".to_string()));
        fields.insert("age".to_string(), Value::Int(30));

        let obj = Value::object(fields);
        assert_eq!(obj.as_object().unwrap().len(), 2);

        let name = obj.get_field("name").unwrap();
        assert_eq!(name, Value::String("Alice".to_string()));
    }

    #[test]
    fn test_range_expansion() {
        let range = Value::Range { start: 1, end: 5 };
        let expanded = range.expand_range().unwrap();
        assert_eq!(expanded.len(), 4);
        assert_eq!(expanded[0], Value::Int(1));
        assert_eq!(expanded[3], Value::Int(4));
    }

    #[test]
    fn test_index_out_of_bounds() {
        let arr = Value::array(vec![Value::Int(1), Value::Int(2)]);
        let result = arr.get_index(&Value::Int(5));
        assert!(result.is_err());
    }
}
