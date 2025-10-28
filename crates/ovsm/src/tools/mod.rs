//! Tool system for OVSM
//!
//! Provides the framework for built-in and custom tools.

pub mod stdlib;

use crate::error::Result;
use crate::runtime::Value;
use std::collections::HashMap;
use std::sync::Arc;

/// Tool trait - all OVSM tools must implement this
pub trait Tool: Send + Sync {
    /// Tool name
    fn name(&self) -> &str;

    /// Tool description
    fn description(&self) -> &str;

    /// Execute the tool
    fn execute(&self, args: &[Value]) -> Result<Value>;

    /// Check if tool requires specific number of arguments
    fn arity(&self) -> Option<usize> {
        None // None means variadic
    }
}

/// Tool arguments (positional and named)
#[derive(Debug, Clone)]
pub struct ToolArguments {
    /// Positional arguments passed to the tool
    pub positional: Vec<Value>,
    /// Named arguments passed to the tool (key-value pairs)
    pub named: HashMap<String, Value>,
}

impl ToolArguments {
    /// Creates a new empty ToolArguments
    pub fn new() -> Self {
        ToolArguments {
            positional: Vec::new(),
            named: HashMap::new(),
        }
    }

    /// Creates ToolArguments from a vector of positional arguments
    pub fn from_positional(args: Vec<Value>) -> Self {
        ToolArguments {
            positional: args,
            named: HashMap::new(),
        }
    }

    /// Get positional argument by index
    pub fn get_positional(&self, index: usize) -> Result<&Value> {
        self.positional
            .get(index)
            .ok_or_else(|| crate::error::Error::InvalidArguments {
                tool: "unknown".to_string(),
                reason: format!("Missing positional argument at index {}", index),
            })
    }

    /// Get named argument
    pub fn get_named(&self, name: &str) -> Result<&Value> {
        self.named
            .get(name)
            .ok_or_else(|| crate::error::Error::InvalidArguments {
                tool: "unknown".to_string(),
                reason: format!("Missing named argument: {}", name),
            })
    }

    /// Get named argument with fallback to positional
    pub fn get_arg(&self, name: &str, index: usize) -> Result<&Value> {
        if let Some(val) = self.named.get(name) {
            Ok(val)
        } else {
            self.get_positional(index)
        }
    }

    /// Try to get named argument or positional
    pub fn try_get_arg(&self, name: &str, index: usize) -> Option<&Value> {
        self.named.get(name).or_else(|| self.positional.get(index))
    }
}

impl Default for ToolArguments {
    fn default() -> Self {
        Self::new()
    }
}

/// Tool registry
pub struct ToolRegistry {
    tools: HashMap<String, Arc<dyn Tool>>,
}

impl ToolRegistry {
    /// Create new registry with standard library
    pub fn new() -> Self {
        let mut registry = ToolRegistry {
            tools: HashMap::new(),
        };

        // Register all standard library tools
        stdlib::register_all(&mut registry);

        registry
    }

    /// Create empty registry (for testing)
    pub fn empty() -> Self {
        ToolRegistry {
            tools: HashMap::new(),
        }
    }

    /// Register a tool
    pub fn register<T: Tool + 'static>(&mut self, tool: T) {
        let name = tool.name().to_string();
        self.tools.insert(name, Arc::new(tool));
    }

    /// Get tool by name (case-insensitive fallback)
    pub fn get(&self, name: &str) -> Result<Arc<dyn Tool>> {
        // Try exact match first
        if let Some(tool) = self.tools.get(name) {
            return Ok(tool.clone());
        }

        // Try case-insensitive match
        let name_lower = name.to_lowercase();
        for (key, tool) in &self.tools {
            if key.to_lowercase() == name_lower {
                return Ok(tool.clone());
            }
        }

        // No match found
        Err(crate::error::Error::UndefinedTool {
            name: name.to_string(),
        })
    }

    /// Check if tool exists
    pub fn has(&self, name: &str) -> bool {
        self.tools.contains_key(name)
    }

    /// List all tool names
    pub fn list_tools(&self) -> Vec<String> {
        let mut names: Vec<_> = self.tools.keys().cloned().collect();
        names.sort();
        names
    }

    /// Get tool count
    pub fn count(&self) -> usize {
        self.tools.len()
    }
}

impl Default for ToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestTool;

    impl Tool for TestTool {
        fn name(&self) -> &str {
            "TEST"
        }

        fn description(&self) -> &str {
            "A test tool"
        }

        fn execute(&self, args: &[Value]) -> Result<Value> {
            if args.is_empty() {
                Ok(Value::Int(42))
            } else {
                Ok(args[0].clone())
            }
        }

        fn arity(&self) -> Option<usize> {
            None
        }
    }

    #[test]
    fn test_tool_registration() {
        let mut registry = ToolRegistry::empty();
        registry.register(TestTool);

        assert!(registry.has("TEST"));
        assert!(!registry.has("UNKNOWN"));
    }

    #[test]
    fn test_tool_execution() {
        let tool = TestTool;
        let result = tool.execute(&[]).unwrap();
        assert_eq!(result, Value::Int(42));

        let result = tool.execute(&[Value::String("hello".to_string())]).unwrap();
        assert_eq!(result, Value::String("hello".to_string()));
    }

    #[test]
    fn test_tool_arguments() {
        let mut args = ToolArguments::new();
        args.positional.push(Value::Int(10));
        args.positional.push(Value::Int(20));
        args.named
            .insert("x".to_string(), Value::String("test".to_string()));

        assert_eq!(*args.get_positional(0).unwrap(), Value::Int(10));
        assert_eq!(*args.get_positional(1).unwrap(), Value::Int(20));
        assert_eq!(
            *args.get_named("x").unwrap(),
            Value::String("test".to_string())
        );
    }
}
