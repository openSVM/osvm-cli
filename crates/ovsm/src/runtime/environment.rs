use std::collections::HashMap;
use std::sync::Arc;

use crate::error::{Error, Result};
use crate::runtime::Value;

/// Environment for variable scoping
#[derive(Debug, Clone)]
pub struct Environment {
    scopes: Vec<Scope>,
    constants: Arc<HashMap<String, Value>>,
}

#[derive(Debug, Clone)]
struct Scope {
    variables: HashMap<String, Value>,
    parent: Option<usize>,
}

impl Environment {
    /// Create new environment
    pub fn new() -> Self {
        Environment {
            scopes: vec![Scope {
                variables: HashMap::new(),
                parent: None,
            }],
            constants: Arc::new(HashMap::new()),
        }
    }

    /// Create environment with constants
    pub fn with_constants(constants: HashMap<String, Value>) -> Self {
        Environment {
            scopes: vec![Scope {
                variables: HashMap::new(),
                parent: None,
            }],
            constants: Arc::new(constants),
        }
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        let parent_idx = self.scopes.len() - 1;
        self.scopes.push(Scope {
            variables: HashMap::new(),
            parent: Some(parent_idx),
        });
    }

    /// Exit current scope
    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Define variable in current scope
    pub fn define(&mut self, name: String, value: Value) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.variables.insert(name, value);
    }

    /// Define constant (immutable)
    pub fn define_constant(&mut self, name: String, value: Value) -> Result<()> {
        // Constants can only be defined once
        if self.constants.contains_key(&name) {
            return Err(Error::ConstantReassignment { name });
        }

        // Create new constants map with the new constant
        let mut new_constants = (*self.constants).clone();
        new_constants.insert(name, value);
        self.constants = Arc::new(new_constants);
        Ok(())
    }

    /// Get variable value
    pub fn get(&self, name: &str) -> Result<Value> {
        // Check constants first
        if let Some(val) = self.constants.get(name) {
            return Ok(val.clone());
        }

        // Walk scope chain from innermost to outermost
        let mut scope_idx = self.scopes.len() - 1;
        loop {
            let scope = &self.scopes[scope_idx];
            if let Some(val) = scope.variables.get(name) {
                return Ok(val.clone());
            }
            match scope.parent {
                Some(parent) => scope_idx = parent,
                None => {
                    return Err(Error::UndefinedVariable {
                        name: name.to_string(),
                    })
                }
            }
        }
    }

    /// Set variable value (updates existing or creates in current scope)
    pub fn set(&mut self, name: &str, value: Value) -> Result<()> {
        // Constants cannot be reassigned
        if self.constants.contains_key(name) {
            return Err(Error::ConstantReassignment {
                name: name.to_string(),
            });
        }

        // Try to find variable in scope chain and update
        let mut scope_idx = self.scopes.len() - 1;
        loop {
            let scope = &mut self.scopes[scope_idx];
            if scope.variables.contains_key(name) {
                scope.variables.insert(name.to_string(), value);
                return Ok(());
            }
            match scope.parent {
                Some(parent) => scope_idx = parent,
                None => {
                    // Variable doesn't exist, define in current scope
                    let current_scope = self.scopes.last_mut().unwrap();
                    current_scope.variables.insert(name.to_string(), value);
                    return Ok(());
                }
            }
        }
    }

    /// Get snapshot of all variables
    pub fn snapshot(&self) -> HashMap<String, Value> {
        let mut result = HashMap::new();

        // Add constants
        for (k, v) in self.constants.iter() {
            result.insert(k.clone(), v.clone());
        }

        // Add all variables from all scopes
        for scope in &self.scopes {
            for (k, v) in &scope.variables {
                result.insert(k.clone(), v.clone());
            }
        }

        result
    }

    /// Check if variable exists
    pub fn exists(&self, name: &str) -> bool {
        // Check constants
        if self.constants.contains_key(name) {
            return true;
        }

        // Check scopes
        let mut scope_idx = self.scopes.len() - 1;
        loop {
            let scope = &self.scopes[scope_idx];
            if scope.variables.contains_key(name) {
                return true;
            }
            match scope.parent {
                Some(parent) => scope_idx = parent,
                None => return false,
            }
        }
    }

    /// Get current scope depth
    pub fn scope_depth(&self) -> usize {
        self.scopes.len()
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_define_and_get() {
        let mut env = Environment::new();
        env.define("x".to_string(), Value::Int(42));

        let val = env.get("x").unwrap();
        assert_eq!(val, Value::Int(42));
    }

    #[test]
    fn test_undefined_variable() {
        let env = Environment::new();
        let result = env.get("undefined");
        assert!(result.is_err());
    }

    #[test]
    fn test_variable_scoping() {
        let mut env = Environment::new();

        // Define in global scope
        env.define("x".to_string(), Value::Int(10));

        // Enter new scope
        env.enter_scope();
        env.define("x".to_string(), Value::Int(20));
        env.define("y".to_string(), Value::Int(30));

        // Check values in inner scope
        assert_eq!(env.get("x").unwrap(), Value::Int(20));
        assert_eq!(env.get("y").unwrap(), Value::Int(30));

        // Exit scope
        env.exit_scope();

        // Check values in outer scope
        assert_eq!(env.get("x").unwrap(), Value::Int(10));
        assert!(env.get("y").is_err()); // y doesn't exist in outer scope
    }

    #[test]
    fn test_nested_scopes() {
        let mut env = Environment::new();

        env.define("x".to_string(), Value::Int(1));

        env.enter_scope();
        env.define("y".to_string(), Value::Int(2));

        env.enter_scope();
        env.define("z".to_string(), Value::Int(3));

        // All variables accessible
        assert_eq!(env.get("x").unwrap(), Value::Int(1));
        assert_eq!(env.get("y").unwrap(), Value::Int(2));
        assert_eq!(env.get("z").unwrap(), Value::Int(3));

        env.exit_scope();
        assert!(env.get("z").is_err());

        env.exit_scope();
        assert!(env.get("y").is_err());
    }

    #[test]
    fn test_constants() {
        let mut constants = HashMap::new();
        constants.insert("PI".to_string(), Value::Float(3.14159));

        let mut env = Environment::with_constants(constants);

        // Can read constant
        assert_eq!(env.get("PI").unwrap(), Value::Float(3.14159));

        // Cannot reassign constant
        let result = env.set("PI", Value::Float(3.0));
        assert!(result.is_err());
    }

    #[test]
    fn test_variable_update() {
        let mut env = Environment::new();

        env.define("x".to_string(), Value::Int(10));
        assert_eq!(env.get("x").unwrap(), Value::Int(10));

        env.set("x", Value::Int(20)).unwrap();
        assert_eq!(env.get("x").unwrap(), Value::Int(20));
    }

    #[test]
    fn test_variable_shadowing() {
        let mut env = Environment::new();

        env.define("x".to_string(), Value::Int(10));

        env.enter_scope();
        env.define("x".to_string(), Value::String("shadowed".to_string()));

        assert_eq!(
            env.get("x").unwrap(),
            Value::String("shadowed".to_string())
        );

        env.exit_scope();
        assert_eq!(env.get("x").unwrap(), Value::Int(10));
    }

    #[test]
    fn test_snapshot() {
        let mut env = Environment::new();

        env.define("x".to_string(), Value::Int(10));
        env.define("y".to_string(), Value::Int(20));

        let snapshot = env.snapshot();
        assert_eq!(snapshot.len(), 2);
        assert_eq!(snapshot.get("x"), Some(&Value::Int(10)));
        assert_eq!(snapshot.get("y"), Some(&Value::Int(20)));
    }

    #[test]
    fn test_exists() {
        let mut env = Environment::new();

        assert!(!env.exists("x"));

        env.define("x".to_string(), Value::Int(42));
        assert!(env.exists("x"));

        env.enter_scope();
        assert!(env.exists("x")); // Still accessible from parent scope

        env.define("y".to_string(), Value::Int(10));
        assert!(env.exists("y"));

        env.exit_scope();
        assert!(!env.exists("y")); // No longer accessible
    }

    #[test]
    fn test_scope_depth() {
        let mut env = Environment::new();
        assert_eq!(env.scope_depth(), 1);

        env.enter_scope();
        assert_eq!(env.scope_depth(), 2);

        env.enter_scope();
        assert_eq!(env.scope_depth(), 3);

        env.exit_scope();
        assert_eq!(env.scope_depth(), 2);

        env.exit_scope();
        assert_eq!(env.scope_depth(), 1);
    }
}
