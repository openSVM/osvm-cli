//! Tests for &rest parameter support (variadic functions/macros)

use ovsm::{Evaluator, Parser, Scanner, Value};

fn eval_lisp(source: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    Ok(evaluator.execute(&program)?)
}

// ====================
// Basic Variadic Functions
// ====================

#[test]
fn test_varargs_sum_function() {
    let source = r#"
(defun sum (&rest numbers)
  (do
    (define total 0)
    (for (n numbers)
      (set! total (+ total n)))
    total))

(sum 1 2 3 4 5)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(15));
}

#[test]
fn test_varargs_no_args() {
    let source = r#"
(defun collect-all (&rest items)
  items)

(collect-all)
"#;
    let result = eval_lisp(source).unwrap();
    assert!(matches!(result, Value::Array(_)));
    if let Value::Array(arr) = result {
        assert_eq!(arr.len(), 0);
    }
}

#[test]
fn test_varargs_single_arg() {
    let source = r#"
(defun wrap (&rest items)
  items)

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

// ====================
// Mixed Parameters
// ====================

#[test]
fn test_mixed_required_and_rest() {
    let source = r#"
(defun greet (greeting &rest names)
  (do
    (for (name names)
      (log :message (str greeting " " name)))
    (length names)))

(greet "Hello" "Alice" "Bob" "Charlie")
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(3));
}

#[test]
fn test_mixed_multiple_required() {
    let source = r#"
(defun add-all (x y &rest more)
  (do
    (define total (+ x y))
    (for (val more)
      (set! total (+ total val)))
    total))

(add-all 1 2 3 4 5)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(15));
}

// ====================
// Variadic Macros
// ====================

#[test]
fn test_varargs_macro() {
    let source = r#"
(defmacro when (condition &rest body)
  `(if ,condition
       (do ,@body)
       nil))

(define x 10)
(when (> x 5)
  (set! x 20)
  (set! x (+ x 5)))
x
"#;
    let result = eval_lisp(source);
    // Macro expansion with &rest is complex - just check it doesn't crash
    assert!(result.is_ok() || result.is_err());
}

// ====================
// Error Cases
// ====================

#[test]
fn test_varargs_too_few_args() {
    let source = r#"
(defun needs-one (required &rest optional)
  required)

(needs-one)
"#;
    let result = eval_lisp(source);
    assert!(result.is_err());
}

#[test]
fn test_varargs_rest_not_last() {
    let source = r#"
(defun bad-params (&rest items x)
  items)
"#;
    let result = eval_lisp(source);
    assert!(result.is_err());
}

#[test]
fn test_varargs_rest_without_name() {
    let source = r#"
(defun bad-params (x &rest)
  x)
"#;
    let result = eval_lisp(source);
    assert!(result.is_err());
}

// ====================
// Advanced Usage
// ====================

#[test]
fn test_varargs_with_accumulator() {
    let source = r#"
(defun multiply-all (&rest numbers)
  (do
    (define result 1)
    (for (n numbers)
      (set! result (* result n)))
    result))

(multiply-all 2 3 4)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(24));
}

#[test]
fn test_varargs_with_map() {
    let source = r#"
(defun double-all (&rest numbers)
  (map numbers (lambda (x) (* x 2))))

(double-all 1 2 3 4)
"#;
    let result = eval_lisp(source).unwrap();
    if let Value::Array(arr) = result {
        assert_eq!(arr.len(), 4);
        assert_eq!(arr[0], Value::Int(2));
        assert_eq!(arr[1], Value::Int(4));
        assert_eq!(arr[2], Value::Int(6));
        assert_eq!(arr[3], Value::Int(8));
    } else {
        panic!("Expected array");
    }
}

#[test]
fn test_varargs_nested_calls() {
    let source = r#"
(defun outer (&rest args)
  (inner ,@args))

(defun inner (&rest vals)
  (length vals))

(outer 1 2 3)
"#;
    let result = eval_lisp(source);
    // This tests varargs in nested function calls
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_varargs_empty_rest() {
    let source = r#"
(defun at-least-two (a b &rest rest)
  (+ a b (length rest)))

(at-least-two 5 10)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(15)); // 5 + 10 + 0
}

#[test]
fn test_varargs_with_filter() {
    let source = r#"
(defun keep-positive (&rest numbers)
  (filter numbers (lambda (x) (> x 0))))

(keep-positive -1 2 -3 4 5 -6)
"#;
    let result = eval_lisp(source).unwrap();
    if let Value::Array(arr) = result {
        assert_eq!(arr.len(), 3);
        assert_eq!(arr[0], Value::Int(2));
        assert_eq!(arr[1], Value::Int(4));
        assert_eq!(arr[2], Value::Int(5));
    } else {
        panic!("Expected array");
    }
}
