//! Simple tests for &rest parameters

use ovsm::{Evaluator, Parser, Scanner, Value};

fn eval_lisp(source: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    Ok(evaluator.execute(&program)?)
}

#[test]
fn test_varargs_empty() {
    let source = r#"
(defun wrap (&rest items) items)
(wrap)
"#;
    let result = eval_lisp(source).unwrap();
    assert!(matches!(result, Value::Array(_)));
    if let Value::Array(arr) = result {
        assert_eq!(arr.len(), 0);
    }
}

#[test]
fn test_varargs_single() {
    let source = r#"
(defun wrap (&rest items) items)
(wrap 42)
"#;
    let result = eval_lisp(source).unwrap();
    if let Value::Array(arr) = result {
        assert_eq!(arr.len(), 1);
        assert_eq!(arr[0], Value::Int(42));
    } else {
        panic!("Expected array");
    }
}

#[test]
fn test_varargs_multiple() {
    let source = r#"
(defun wrap (&rest items) items)
(wrap 1 2 3)
"#;
    let result = eval_lisp(source).unwrap();
    if let Value::Array(arr) = result {
        assert_eq!(arr.len(), 3);
    } else {
        panic!("Expected array");
    }
}

#[test]
fn test_mixed_params() {
    let source = r#"
(defun first-and-rest (x &rest items) items)
(first-and-rest 1 2 3 4)
"#;
    let result = eval_lisp(source).unwrap();
    if let Value::Array(arr) = result {
        assert_eq!(arr.len(), 3); // Rest gets [2, 3, 4]
    } else {
        panic!("Expected array");
    }
}

#[test]
fn test_varargs_length() {
    let source = r#"
(defun count-args (&rest items) (length items))
(count-args 1 2 3 4 5)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(5));
}
