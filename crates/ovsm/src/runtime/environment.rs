use std::collections::HashMap;
use std::sync::Arc;

use crate::error::{Error, Result};
use crate::runtime::Value;

/// Environment for variable scoping
#[derive(Debug, Clone)]
pub struct Environment {
    /// Stack of nested scopes
    scopes: Vec<Scope>,
    /// Immutable constants shared across all scopes
    constants: Arc<HashMap<String, Value>>,
    /// Dynamic (special) variables with dynamic binding stack
    /// Stack of (name, value) pairs for dynamic extent
    dynamic_bindings: Vec<HashMap<String, Value>>,
}

/// Single scope in the environment
#[derive(Debug, Clone)]
struct Scope {
    /// Variables defined in this scope
    variables: HashMap<String, Value>,
    /// Index of parent scope (None for global scope)
    parent: Option<usize>,
}

impl Environment {
    /// Creates a new environment with a global scope
    pub fn new() -> Self {
        Environment {
            scopes: vec![Scope {
                variables: HashMap::new(),
                parent: None,
            }],
            constants: Arc::new(HashMap::new()),
            dynamic_bindings: vec![HashMap::new()], // Start with global dynamic scope
        }
    }

    /// Creates a new environment with predefined constants
    pub fn with_constants(constants: HashMap<String, Value>) -> Self {
        Environment {
            scopes: vec![Scope {
                variables: HashMap::new(),
                parent: None,
            }],
            constants: Arc::new(constants),
            dynamic_bindings: vec![HashMap::new()], // Start with global dynamic scope
        }
    }

    /// Enters a new nested scope
    pub fn enter_scope(&mut self) {
        let parent_idx = self.scopes.len() - 1;
        self.scopes.push(Scope {
            variables: HashMap::new(),
            parent: Some(parent_idx),
        });
    }

    /// Exits the current scope and returns to parent scope
    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Defines a new variable in the current scope
    pub fn define(&mut self, name: String, value: Value) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.variables.insert(name, value);
    }

    /// Defines an immutable constant (cannot be reassigned)
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

    /// Gets the value of a variable or constant by name
    pub fn get(&self, name: &str) -> Result<Value> {
        // Check constants first
        if let Some(val) = self.constants.get(name) {
            return Ok(val.clone());
        }

        // Check dynamic variables (if this is a dynamic variable)
        if let Some(val) = self.get_dynamic(name) {
            return Ok(val);
        }

        // Walk scope chain from innermost to outermost for lexical variables
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
                        available_fields: None,
                    })
                }
            }
        }
    }

    /// Sets a variable value (updates existing or creates new in current scope)
    pub fn set(&mut self, name: &str, value: Value) -> Result<()> {
        // Constants cannot be reassigned
        if self.constants.contains_key(name) {
            return Err(Error::ConstantReassignment {
                name: name.to_string(),
            });
        }

        // If this is a dynamic variable, update it in the dynamic binding stack
        if self.is_dynamic(name) {
            // Update the most recent binding
            for frame in self.dynamic_bindings.iter_mut().rev() {
                if frame.contains_key(name) {
                    frame.insert(name.to_string(), value);
                    return Ok(());
                }
            }
        }

        // Try to find variable in lexical scope chain and update
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

    /// Returns a snapshot of all variables and constants in all scopes
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

    /// Returns the current environment snapshot for creating closures
    /// This captures all accessible variables from the current point in scope chain
    pub fn current_env_snapshot(&self) -> HashMap<String, Value> {
        // For flet, we want to capture the environment BEFORE entering flet scope
        // This is the same as the full snapshot
        self.snapshot()
    }

    /// Checks if a variable or constant exists in any scope
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

    /// Returns the current scope depth (1 for global scope)
    pub fn scope_depth(&self) -> usize {
        self.scopes.len()
    }

    // =========================================================================
    // DYNAMIC VARIABLES (Common Lisp special variables)
    // =========================================================================

    /// Defines a dynamic (special) variable
    /// Dynamic variables have dynamic scope, not lexical scope
    /// Convention: *name* with earmuffs
    pub fn defvar(&mut self, name: String, value: Value) {
        // Define in the global dynamic scope
        if let Some(global_dynamic) = self.dynamic_bindings.first_mut() {
            global_dynamic.insert(name, value);
        }
    }

    /// Pushes a new dynamic binding frame (for dynamic let)
    pub fn push_dynamic_bindings(&mut self, bindings: HashMap<String, Value>) {
        self.dynamic_bindings.push(bindings);
    }

    /// Pops the current dynamic binding frame
    pub fn pop_dynamic_bindings(&mut self) {
        if self.dynamic_bindings.len() > 1 {
            self.dynamic_bindings.pop();
        }
    }

    /// Gets a dynamic variable value (searches from top of stack down)
    pub fn get_dynamic(&self, name: &str) -> Option<Value> {
        // Search from most recent binding to oldest
        for frame in self.dynamic_bindings.iter().rev() {
            if let Some(value) = frame.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    /// Checks if a variable is a dynamic variable (exists in dynamic bindings)
    pub fn is_dynamic(&self, name: &str) -> bool {
        self.dynamic_bindings
            .iter()
            .any(|frame| frame.contains_key(name))
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
        constants.insert("PHI".to_string(), Value::Float(1.618034));

        let mut env = Environment::with_constants(constants);

        // Can read constant
        assert_eq!(env.get("PHI").unwrap(), Value::Float(1.618034));

        // Cannot reassign constant
        let result = env.set("PHI", Value::Float(1.0));
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

        assert_eq!(env.get("x").unwrap(), Value::String("shadowed".to_string()));

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
