//! Tests for &optional and &key parameters in OVSM functions
//!
//! This test suite covers:
//! - Optional parameters with defaults
//! - Optional parameters without explicit defaults
//! - Keyword (named) parameters
//! - Mixed parameter types (required + optional + keyword)
//! - Complex scenarios (required + optional + rest + keyword)
//! - Error cases

use ovsm::{LispEvaluator, SExprParser, SExprScanner, Value};

/// Helper function to execute OVSM LISP code
fn eval(code: &str) -> Result<Value, ovsm::Error> {
    // Scan tokens
    let mut scanner = SExprScanner::new(code);
    let tokens = scanner.scan_tokens()?;

    // Parse
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse()?;

    // Execute
    let mut evaluator = LispEvaluator::new();
    evaluator.execute(&program)
}

/// Helper to assert successful evaluation and check result
fn assert_eval(code: &str, expected: Value) {
    let result = eval(code);
    assert!(result.is_ok(), "Failed to evaluate: {:?}", result);
    assert_eq!(result.unwrap(), expected);
}

#[test]
fn test_optional_param_with_default() {
    // Function with one required and one optional parameter
    let code = r#"
        (defun greet (name &optional (greeting "Hello"))
          (str greeting " " name))
        (greet "World")
    "#;
    assert_eval(code, Value::String("Hello World".to_string()));
}

#[test]
fn test_optional_param_override_default() {
    // Override the default value
    let code = r#"
        (defun greet (name &optional (greeting "Hello"))
          (str greeting " " name))
        (greet "World" "Hi")
    "#;
    assert_eval(code, Value::String("Hi World".to_string()));
}

#[test]
fn test_multiple_optional_params() {
    // Multiple optional parameters
    let code = r#"
        (defun greet (name &optional (greeting "Hello") (punct "!"))
          (str greeting " " name punct))
        (greet "Alice")
    "#;
    assert_eval(code, Value::String("Hello Alice!".to_string()));
}

#[test]
fn test_multiple_optional_params_partial() {
    // Provide only first optional parameter
    let code = r#"
        (defun greet (name &optional (greeting "Hello") (punct "!"))
          (str greeting " " name punct))
        (greet "Bob" "Hi")
    "#;
    assert_eval(code, Value::String("Hi Bob!".to_string()));
}

#[test]
fn test_multiple_optional_params_all() {
    // Provide all optional parameters
    let code = r#"
        (defun greet (name &optional (greeting "Hello") (punct "!"))
          (str greeting " " name punct))
        (greet "Charlie" "Hey" "?")
    "#;
    assert_eval(code, Value::String("Hey Charlie?".to_string()));
}

#[test]
fn test_optional_param_without_default() {
    // Optional parameter without explicit default (defaults to null)
    let code = r#"
        (defun test (x &optional y)
          (if (null? y)
              x
              (+ x y)))
        (test 10)
    "#;
    assert_eval(code, Value::Int(10));
}

#[test]
fn test_optional_param_without_default_provided() {
    // Optional parameter without explicit default, but value provided
    let code = r#"
        (defun test (x &optional y)
          (if (null? y)
              x
              (+ x y)))
        (test 10 5)
    "#;
    assert_eval(code, Value::Int(15));
}

#[test]
fn test_keyword_param_basic() {
    // Basic keyword parameter
    let code = r#"
        (defun make-point (&key (x 0) (y 0))
          {:x x :y y})
        (make-point)
    "#;

    let expected = {
        let mut map = std::collections::HashMap::new();
        map.insert("x".to_string(), Value::Int(0));
        map.insert("y".to_string(), Value::Int(0));
        Value::object(map)
    };
    assert_eval(code, expected);
}

#[test]
fn test_keyword_param_single() {
    // Provide one keyword argument
    let code = r#"
        (defun make-point (&key (x 0) (y 0))
          {:x x :y y})
        (make-point :x 10)
    "#;

    let expected = {
        let mut map = std::collections::HashMap::new();
        map.insert("x".to_string(), Value::Int(10));
        map.insert("y".to_string(), Value::Int(0));
        Value::object(map)
    };
    assert_eval(code, expected);
}

#[test]
fn test_keyword_param_both() {
    // Provide both keyword arguments
    let code = r#"
        (defun make-point (&key (x 0) (y 0))
          {:x x :y y})
        (make-point :x 10 :y 20)
    "#;

    let expected = {
        let mut map = std::collections::HashMap::new();
        map.insert("x".to_string(), Value::Int(10));
        map.insert("y".to_string(), Value::Int(20));
        Value::object(map)
    };
    assert_eval(code, expected);
}

#[test]
fn test_keyword_param_order_independent() {
    // Keyword arguments can be in any order
    let code = r#"
        (defun make-point (&key (x 0) (y 0))
          {:x x :y y})
        (make-point :y 20 :x 10)
    "#;

    let expected = {
        let mut map = std::collections::HashMap::new();
        map.insert("x".to_string(), Value::Int(10));
        map.insert("y".to_string(), Value::Int(20));
        Value::object(map)
    };
    assert_eval(code, expected);
}

#[test]
fn test_keyword_param_without_default() {
    // Keyword parameter without default (defaults to null)
    let code = r#"
        (defun test (&key name)
          (if (null? name)
              "no name"
              name))
        (test)
    "#;
    assert_eval(code, Value::String("no name".to_string()));
}

#[test]
fn test_keyword_param_without_default_provided() {
    // Keyword parameter without default, value provided
    let code = r#"
        (defun test (&key name)
          (if (null? name)
              "no name"
              name))
        (test :name "Alice")
    "#;
    assert_eval(code, Value::String("Alice".to_string()));
}

#[test]
fn test_mixed_required_and_optional() {
    // Mix required and optional parameters
    let code = r#"
        (defun test (a b &optional (c 1))
          (+ a b c))
        (test 10 20)
    "#;
    assert_eval(code, Value::Int(31));
}

#[test]
fn test_mixed_required_and_optional_override() {
    // Mix required and optional with override
    let code = r#"
        (defun test (a b &optional (c 1))
          (+ a b c))
        (test 10 20 5)
    "#;
    assert_eval(code, Value::Int(35));
}

#[test]
fn test_mixed_required_and_keyword() {
    // Mix required and keyword parameters
    let code = r#"
        (defun test (name &key (age 0))
          {:name name :age age})
        (test "Alice")
    "#;

    let expected = {
        let mut map = std::collections::HashMap::new();
        map.insert("name".to_string(), Value::String("Alice".to_string()));
        map.insert("age".to_string(), Value::Int(0));
        Value::object(map)
    };
    assert_eval(code, expected);
}

#[test]
fn test_mixed_required_and_keyword_provided() {
    // Mix required and keyword with value
    let code = r#"
        (defun test (name &key (age 0))
          {:name name :age age})
        (test "Bob" :age 30)
    "#;

    let expected = {
        let mut map = std::collections::HashMap::new();
        map.insert("name".to_string(), Value::String("Bob".to_string()));
        map.insert("age".to_string(), Value::Int(30));
        Value::object(map)
    };
    assert_eval(code, expected);
}

#[test]
fn test_mixed_required_optional_keyword() {
    // Mix all three types
    let code = r#"
        (defun test (req &optional (opt 10) &key (key1 100))
          (+ req opt key1))
        (test 1)
    "#;
    assert_eval(code, Value::Int(111));
}

#[test]
fn test_mixed_required_optional_keyword_all() {
    // Provide all parameters
    let code = r#"
        (defun test (req &optional (opt 10) &key (key1 100))
          (+ req opt key1))
        (test 1 20 :key1 200)
    "#;
    assert_eval(code, Value::Int(221));
}

#[test]
fn test_complex_with_rest() {
    // Complex: required + optional + rest + keyword
    let code = r#"
        (defun complex (a b &optional (c 1) &rest args &key (debug false))
          [a b c args debug])
        (complex 1 2)
    "#;

    let expected = Value::array(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(1),
        Value::array(vec![]),
        Value::Bool(false),
    ]);
    assert_eval(code, expected);
}

#[test]
fn test_complex_with_rest_and_keyword() {
    // Complex with rest args and keyword
    let code = r#"
        (defun complex (a b &optional (c 1) &rest args &key (debug false))
          [a b c args debug])
        (complex 1 2 3 4 5 :debug true)
    "#;

    let expected = Value::array(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
        Value::array(vec![Value::Int(4), Value::Int(5)]),
        Value::Bool(true),
    ]);
    assert_eval(code, expected);
}

#[test]
fn test_error_missing_required() {
    // Error: missing required parameter
    let code = r#"
        (defun test (a &optional (b 1))
          (+ a b))
        (test)
    "#;
    let result = eval(code);
    assert!(result.is_err());
}

#[test]
fn test_str_function_with_variadic() {
    // Test that str function works (variadic function)
    let code = r#"(str "Hello" " " "World")"#;
    assert_eval(code, Value::String("Hello World".to_string()));
}

#[test]
fn test_optional_with_different_types() {
    // Optional parameters with different default types
    let code = r#"
        (defun test (&optional (num 42) (text "default") (flag true))
          [num text flag])
        (test)
    "#;

    let expected = Value::array(vec![
        Value::Int(42),
        Value::String("default".to_string()),
        Value::Bool(true),
    ]);
    assert_eval(code, expected);
}

#[test]
fn test_keyword_with_different_types() {
    // Keyword parameters with different default types
    let code = r#"
        (defun test (&key (num 42) (text "default") (flag false))
          [num text flag])
        (test :text "custom" :flag true)
    "#;

    let expected = Value::array(vec![
        Value::Int(42),
        Value::String("custom".to_string()),
        Value::Bool(true),
    ]);
    assert_eval(code, expected);
}

#[test]
fn test_lambda_with_optional() {
    // Lambda functions with optional parameters
    let code = r#"
        (define add (lambda (x &optional (y 0))
          (+ x y)))
        (add 10)
    "#;
    assert_eval(code, Value::Int(10));
}

#[test]
fn test_lambda_with_optional_provided() {
    // Lambda with optional parameter provided
    let code = r#"
        (define add (lambda (x &optional (y 0))
          (+ x y)))
        (add 10 5)
    "#;
    assert_eval(code, Value::Int(15));
}

#[test]
fn test_lambda_with_keyword() {
    // Lambda with keyword parameters
    let code = r#"
        (define make-obj (lambda (&key (x 0) (y 0))
          {:x x :y y}))
        (make-obj :x 5)
    "#;

    let expected = {
        let mut map = std::collections::HashMap::new();
        map.insert("x".to_string(), Value::Int(5));
        map.insert("y".to_string(), Value::Int(0));
        Value::object(map)
    };
    assert_eval(code, expected);
}
