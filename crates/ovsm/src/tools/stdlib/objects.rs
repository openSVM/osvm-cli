//! Object manipulation tools

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

/// Register object manipulation tools
pub fn register(registry: &mut ToolRegistry) {
    registry.register(KeysTool);
    registry.register(ValuesTool);
    registry.register(GetTool);
    registry.register(AssocTool);
    registry.register(HasKeyTool);
    registry.register(MergeTool);
}

/// Tool for getting all keys from an object
pub struct KeysTool;

impl Tool for KeysTool {
    fn name(&self) -> &str {
        "KEYS"
    }

    fn description(&self) -> &str {
        "Get all keys from an object as an array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "KEYS".to_string(),
                reason: "Expected 1 argument (object)".to_string(),
            });
        }

        match &args[0] {
            Value::Object(map) => {
                let keys: Vec<Value> = map.keys().map(|k| Value::String(k.clone())).collect();
                Ok(Value::array(keys)) // Use helper
            }
            _ => Err(Error::InvalidArguments {
                tool: "KEYS".to_string(),
                reason: "Expected object, got other type".to_string(),
            }),
        }
    }
}

/// Tool for getting all values from an object
pub struct ValuesTool;

impl Tool for ValuesTool {
    fn name(&self) -> &str {
        "VALUES"
    }

    fn description(&self) -> &str {
        "Get all values from an object as an array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "VALUES".to_string(),
                reason: "Expected 1 argument (object)".to_string(),
            });
        }

        match &args[0] {
            Value::Object(map) => {
                let values: Vec<Value> = map.values().cloned().collect();
                Ok(Value::array(values)) // Use helper
            }
            _ => Err(Error::InvalidArguments {
                tool: "VALUES".to_string(),
                reason: "Expected object, got other type".to_string(),
            }),
        }
    }
}

/// Tool for getting a value from an object by key
pub struct GetTool;

impl Tool for GetTool {
    fn name(&self) -> &str {
        "GET"
    }

    fn description(&self) -> &str {
        "Get value from object by key, returns null if key doesn't exist"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "GET".to_string(),
                reason: "Expected 2 arguments (object, key)".to_string(),
            });
        }

        let map = match &args[0] {
            Value::Object(m) => m,
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "GET".to_string(),
                    reason: "First argument must be an object".to_string(),
                })
            }
        };

        let key = match &args[1] {
            Value::String(s) => s,
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "GET".to_string(),
                    reason: "Second argument must be a string (key)".to_string(),
                })
            }
        };

        Ok(map.get(key).cloned().unwrap_or(Value::Null))
    }
}

/// Tool for associating a new key-value pair in an object (returns new object)
pub struct AssocTool;

impl Tool for AssocTool {
    fn name(&self) -> &str {
        "ASSOC"
    }

    fn description(&self) -> &str {
        "Associate a key-value pair in object, returns new object"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::InvalidArguments {
                tool: "ASSOC".to_string(),
                reason: "Expected 3 arguments (object, key, value)".to_string(),
            });
        }

        let map = match &args[0] {
            Value::Object(m) => (**m).clone(), // Deref Arc and clone HashMap
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "ASSOC".to_string(),
                    reason: "First argument must be an object".to_string(),
                })
            }
        };

        let key = match &args[1] {
            Value::String(s) => s.clone(),
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "ASSOC".to_string(),
                    reason: "Second argument must be a string (key)".to_string(),
                })
            }
        };

        let value = args[2].clone();

        let mut new_map = map;
        new_map.insert(key, value);

        Ok(Value::object(new_map)) // Use helper to create Arc
    }
}

/// Tool for checking if object has a key
pub struct HasKeyTool;

impl Tool for HasKeyTool {
    fn name(&self) -> &str {
        "HAS_KEY"
    }

    fn description(&self) -> &str {
        "Check if object has a given key"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "HAS_KEY".to_string(),
                reason: "Expected 2 arguments (object, key)".to_string(),
            });
        }

        let map = match &args[0] {
            Value::Object(m) => m,
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "HAS_KEY".to_string(),
                    reason: "First argument must be an object".to_string(),
                })
            }
        };

        let key = match &args[1] {
            Value::String(s) => s,
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "HAS_KEY".to_string(),
                    reason: "Second argument must be a string (key)".to_string(),
                })
            }
        };

        Ok(Value::Bool(map.contains_key(key)))
    }
}

/// Tool for merging two objects (second object's keys override first)
pub struct MergeTool;

impl Tool for MergeTool {
    fn name(&self) -> &str {
        "MERGE"
    }

    fn description(&self) -> &str {
        "Merge two objects, second object's keys override first"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "MERGE".to_string(),
                reason: "Expected 2 arguments (object1, object2)".to_string(),
            });
        }

        let map1 = match &args[0] {
            Value::Object(m) => (**m).clone(), // Deref Arc and clone HashMap
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "MERGE".to_string(),
                    reason: "First argument must be an object".to_string(),
                })
            }
        };

        let map2 = match &args[1] {
            Value::Object(m) => m,
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "MERGE".to_string(),
                    reason: "Second argument must be an object".to_string(),
                })
            }
        };

        let mut result = map1;
        for (k, v) in map2.iter() {
            result.insert(k.clone(), v.clone());
        }

        Ok(Value::object(result)) // Use helper to create Arc
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_keys() {
        let tool = KeysTool;
        let mut map = HashMap::new();
        map.insert("name".to_string(), Value::String("Alice".to_string()));
        map.insert("age".to_string(), Value::Int(30));

        let obj = Value::object(map); // Use helper
        let result = tool.execute(&[obj]).unwrap();

        match result {
            Value::Array(keys) => {
                assert_eq!(keys.len(), 2);
                assert!(keys.contains(&Value::String("name".to_string())));
                assert!(keys.contains(&Value::String("age".to_string())));
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_values() {
        let tool = ValuesTool;
        let mut map = HashMap::new();
        map.insert("name".to_string(), Value::String("Alice".to_string()));
        map.insert("age".to_string(), Value::Int(30));

        let obj = Value::object(map);
        let result = tool.execute(&[obj]).unwrap();

        match result {
            Value::Array(values) => {
                assert_eq!(values.len(), 2);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_get() {
        let tool = GetTool;
        let mut map = HashMap::new();
        map.insert("name".to_string(), Value::String("Alice".to_string()));

        let obj = Value::object(map);
        let key = Value::String("name".to_string());
        let result = tool.execute(&[obj.clone(), key]).unwrap();

        assert_eq!(result, Value::String("Alice".to_string()));

        // Test missing key
        let missing_key = Value::String("missing".to_string());
        let result = tool.execute(&[obj, missing_key]).unwrap();
        assert_eq!(result, Value::Null);
    }

    #[test]
    fn test_assoc() {
        let tool = AssocTool;
        let mut map = HashMap::new();
        map.insert("name".to_string(), Value::String("Alice".to_string()));

        let obj = Value::object(map);
        let key = Value::String("age".to_string());
        let value = Value::Int(30);

        let result = tool.execute(&[obj, key, value]).unwrap();

        match result {
            Value::Object(m) => {
                assert_eq!(m.len(), 2);
                assert_eq!(m.get("age"), Some(&Value::Int(30)));
            }
            _ => panic!("Expected object"),
        }
    }

    #[test]
    fn test_has_key() {
        let tool = HasKeyTool;
        let mut map = HashMap::new();
        map.insert("name".to_string(), Value::String("Alice".to_string()));

        let obj = Value::object(map);

        let key = Value::String("name".to_string());
        let result = tool.execute(&[obj.clone(), key]).unwrap();
        assert_eq!(result, Value::Bool(true));

        let missing = Value::String("missing".to_string());
        let result = tool.execute(&[obj, missing]).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_merge() {
        let tool = MergeTool;

        let mut map1 = HashMap::new();
        map1.insert("name".to_string(), Value::String("Alice".to_string()));
        map1.insert("age".to_string(), Value::Int(30));

        let mut map2 = HashMap::new();
        map2.insert("age".to_string(), Value::Int(31)); // Override
        map2.insert("city".to_string(), Value::String("NYC".to_string())); // New

        let obj1 = Value::object(map1);
        let obj2 = Value::object(map2);

        let result = tool.execute(&[obj1, obj2]).unwrap();

        match result {
            Value::Object(m) => {
                assert_eq!(m.len(), 3);
                assert_eq!(m.get("age"), Some(&Value::Int(31))); // Overridden
                assert_eq!(m.get("city"), Some(&Value::String("NYC".to_string())));
                // Added
            }
            _ => panic!("Expected object"),
        }
    }
}
