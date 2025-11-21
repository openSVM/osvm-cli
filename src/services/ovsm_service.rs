//! OVSM Service - Integration layer for the OVSM language interpreter
//!
//! This service provides a bridge between the OSVM CLI and the OVSM language interpreter,
//! enabling users to write and execute automation scripts with full access to OSVM operations.

use anyhow::{Context, Result};
use ovsm::tools::ToolRegistry;
use ovsm::{Evaluator, Parser, Scanner, Value};
use std::fs;
use std::path::Path;

/// OVSM Service for executing scripts and managing the OVSM runtime
pub struct OvsmService {
    /// The OVSM evaluator instance
    evaluator: Evaluator,
    /// Verbose mode flag
    verbose: bool,
    /// Debug mode flag
    debug: bool,
}

impl OvsmService {
    /// Create a new OVSM service instance
    pub fn new() -> Self {
        Self {
            evaluator: Evaluator::new(),
            verbose: false,
            debug: false,
        }
    }

    /// Create a new OVSM service with verbose output
    pub fn with_verbose(verbose: bool) -> Self {
        Self {
            evaluator: Evaluator::new(),
            verbose,
            debug: false,
        }
    }

    /// Enable debug mode
    pub fn with_debug(mut self, debug: bool) -> Self {
        self.debug = debug;
        self
    }

    /// Create a new OVSM service with a custom tool registry
    ///
    /// This allows external tools to be registered for use in OVSM scripts
    pub fn with_registry(registry: ToolRegistry, verbose: bool, debug: bool) -> Self {
        Self {
            evaluator: Evaluator::with_registry(registry),
            verbose,
            debug,
        }
    }

    /// Execute OVSM code from a string
    ///
    /// # Arguments
    /// * `code` - The OVSM source code to execute
    ///
    /// # Returns
    /// The result value from the OVSM execution
    pub fn execute_code(&mut self, code: &str) -> Result<Value> {
        if self.debug {
            crate::tui_log!("🔍 Parsing OVSM code:");
            crate::tui_log!("{}", code);
        }

        // Auto-correct parenthesis imbalances
        let fixer = ovsm::parser::ParenFixer::new(code);
        let (fixed_code, report) = fixer.fix_with_report();

        if let Some(msg) = report {
            crate::tui_log!("{}", msg);
        }

        // Tokenize the code (using potentially fixed code)
        let mut scanner = Scanner::new(&fixed_code);
        let tokens = scanner
            .scan_tokens()
            .map_err(|e| anyhow::anyhow!("Tokenization error: {}", e))?;

        if self.debug {
            crate::tui_log!("✅ Tokenization successful ({} tokens)", tokens.len());
        }

        // Parse the tokens into an AST
        let mut parser = Parser::new(tokens);
        let program = parser
            .parse()
            .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;

        if self.debug {
            crate::tui_log!("✅ Parsing successful");
        }

        // Execute the program
        let result = self
            .evaluator
            .execute(&program)
            .map_err(|e| anyhow::anyhow!("Execution error: {}", e.enhanced_message()))?;

        if self.verbose {
            crate::tui_log!("✨ Execution completed successfully");
        }

        Ok(result)
    }

    /// Execute an OVSM script file
    ///
    /// # Arguments
    /// * `script_path` - Path to the OVSM script file
    ///
    /// # Returns
    /// The result value from the OVSM execution
    pub fn execute_file<P: AsRef<Path>>(&mut self, script_path: P) -> Result<Value> {
        let path = script_path.as_ref();

        if self.verbose {
            crate::tui_log!("📂 Reading script: {}", path.display());
        }

        // Read the script file
        let code = fs::read_to_string(path)
            .with_context(|| format!("Failed to read script file: {}", path.display()))?;

        if self.verbose {
            crate::tui_log!("📜 Script loaded ({} bytes)", code.len());
        }

        // Execute the code
        self.execute_code(&code)
    }

    /// Check the syntax of OVSM code without executing it
    ///
    /// # Arguments
    /// * `code` - The OVSM source code to check
    ///
    /// # Returns
    /// Ok(()) if the syntax is valid, Err otherwise
    pub fn check_syntax(&self, code: &str) -> Result<()> {
        // Tokenize
        let mut scanner = Scanner::new(code);
        let tokens = scanner
            .scan_tokens()
            .context("Syntax error during tokenization")?;

        // Parse
        let mut parser = Parser::new(tokens);
        parser.parse().context("Syntax error during parsing")?;

        if self.verbose {
            crate::tui_log!("✅ Syntax check passed");
        }

        Ok(())
    }

    /// Check the syntax of an OVSM script file
    ///
    /// # Arguments
    /// * `script_path` - Path to the OVSM script file
    ///
    /// # Returns
    /// Ok(()) if the syntax is valid, Err otherwise
    pub fn check_file_syntax<P: AsRef<Path>>(&self, script_path: P) -> Result<()> {
        let path = script_path.as_ref();

        if self.verbose {
            crate::tui_log!("📂 Checking script: {}", path.display());
        }

        let code = fs::read_to_string(path)
            .with_context(|| format!("Failed to read script file: {}", path.display()))?;

        self.check_syntax(&code)
    }

    /// Format a value for display
    #[allow(clippy::only_used_in_recursion)]
    pub fn format_value(&self, value: &Value) -> String {
        match value {
            Value::Int(i) => format!("{}", i),
            Value::Float(f) => format!("{}", f),
            Value::String(s) => format!("\"{}\"", s),
            Value::Bool(b) => format!("{}", b),
            Value::Null => "null".to_string(),
            Value::Array(arr) => {
                let items: Vec<String> = arr.iter().map(|v| self.format_value(v)).collect();
                format!("[{}]", items.join(", "))
            }
            Value::Object(obj) => {
                let pairs: Vec<String> = obj
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, self.format_value(v)))
                    .collect();
                format!("{{{}}}", pairs.join(", "))
            }
            Value::Function { params, .. } => {
                format!("<function({} params)>", params.len())
            }
            Value::Range { start, end } => {
                format!("[{}..{}]", start, end)
            }
            Value::Multiple(vals) => {
                // Format multiple values as (values ...)
                let items: Vec<String> = vals.iter().map(|v| self.format_value(v)).collect();
                format!("(values {})", items.join(" "))
            }
            Value::Macro { params, .. } => {
                format!("<macro({} params)>", params.len())
            }
        }
    }

    /// Format a value as JSON
    pub fn format_value_json(&self, value: &Value) -> Result<String> {
        // Convert OVSM Value to serde_json::Value
        let json_value = self.value_to_json(value)?;
        serde_json::to_string_pretty(&json_value).context("Failed to serialize value to JSON")
    }

    /// Convert OVSM Value to serde_json::Value
    #[allow(clippy::only_used_in_recursion)]
    fn value_to_json(&self, value: &Value) -> Result<serde_json::Value> {
        match value {
            Value::Int(i) => Ok(serde_json::json!(i)),
            Value::Float(f) => Ok(serde_json::json!(f)),
            Value::String(s) => Ok(serde_json::json!(s)),
            Value::Bool(b) => Ok(serde_json::json!(b)),
            Value::Null => Ok(serde_json::Value::Null),
            Value::Array(arr) => {
                let json_arr: Result<Vec<_>> = arr.iter().map(|v| self.value_to_json(v)).collect();
                Ok(serde_json::Value::Array(json_arr?))
            }
            Value::Object(obj) => {
                let mut json_obj = serde_json::Map::new();
                for (k, v) in obj.iter() {
                    json_obj.insert(k.clone(), self.value_to_json(v)?);
                }
                Ok(serde_json::Value::Object(json_obj))
            }
            Value::Function { params, .. } => {
                // Represent function as JSON object
                Ok(serde_json::json!({
                    "type": "function",
                    "params": params.len()
                }))
            }
            Value::Range { start, end } => {
                // Represent range as an object with start and end
                Ok(serde_json::json!({
                    "type": "range",
                    "start": start,
                    "end": end
                }))
            }
            Value::Multiple(vals) => {
                // Represent multiple values as an array
                let json_arr: Result<Vec<_>> = vals.iter().map(|v| self.value_to_json(v)).collect();
                Ok(serde_json::json!({
                    "type": "multiple-values",
                    "values": json_arr?
                }))
            }
            Value::Macro { params, .. } => {
                // Represent macro as JSON object
                Ok(serde_json::json!({
                    "type": "macro",
                    "params": params.len()
                }))
            }
        }
    }

    /// Get the execution trace from the evaluator
    pub fn get_execution_trace(&self) -> Vec<(String, Value)> {
        self.evaluator.get_execution_trace()
    }

    /// Clear the execution trace
    pub fn clear_execution_trace(&self) {
        self.evaluator.clear_execution_trace();
    }
}

impl Default for OvsmService {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_execute_simple_code() {
        let mut service = OvsmService::new();
        let result = service.execute_code("(define x 42) x").unwrap();

        match result {
            Value::Int(i) => assert_eq!(i, 42),
            _ => panic!("Expected Int value"),
        }
    }

    #[test]
    fn test_execute_arithmetic() {
        let mut service = OvsmService::new();
        let result = service
            .execute_code("(let ((a 10) (b 20)) (+ a b))")
            .unwrap();

        match result {
            Value::Int(i) => assert_eq!(i, 30),
            _ => panic!("Expected Int value"),
        }
    }

    #[test]
    fn test_execute_loop() {
        let mut service = OvsmService::new();
        let result = service
            .execute_code("(let ((sum 0)) (for (i (range 1 6)) (set! sum (+ sum i))) sum)")
            .unwrap();

        match result {
            Value::Int(i) => assert_eq!(i, 15), // 1+2+3+4+5 = 15
            _ => panic!("Expected Int value"),
        }
    }

    #[test]
    fn test_check_syntax_valid() {
        let service = OvsmService::new();
        let result = service.check_syntax("(define x 10) x");
        assert!(result.is_ok());
    }

    #[test]
    fn test_check_syntax_invalid() {
        let service = OvsmService::new();
        let result = service.check_syntax("(define x");
        assert!(result.is_err());
    }

    #[test]
    fn test_format_value() {
        let service = OvsmService::new();

        assert_eq!(service.format_value(&Value::Int(42)), "42");
        assert_eq!(service.format_value(&Value::Float(3.15)), "3.15");
        assert_eq!(
            service.format_value(&Value::String("hello".into())),
            "\"hello\""
        );
        assert_eq!(service.format_value(&Value::Bool(true)), "true");
        assert_eq!(service.format_value(&Value::Null), "null");
    }

    #[test]
    fn test_format_value_json() {
        let service = OvsmService::new();
        let value = Value::Int(42);
        let json = service.format_value_json(&value).unwrap();
        assert!(json.contains("42"));
    }
}
