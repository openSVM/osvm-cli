//! Tests for case and typecase (pattern matching)

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
// case - Basic Value Matching
// ====================

#[test]
fn test_case_single_match() {
    let source = r#"
(case 2
  (1 "one")
  (2 "two")
  (3 "three"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("two".to_string()));
}

#[test]
fn test_case_else_clause() {
    let source = r#"
(case 99
  (1 "one")
  (2 "two")
  (else "other"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("other".to_string()));
}

#[test]
fn test_case_no_match_no_else() {
    let source = r#"
(case 99
  (1 "one")
  (2 "two"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Null);
}

#[test]
fn test_case_with_strings() {
    let source = r#"
(case "hello"
  ("hi" "greeting1")
  ("hello" "greeting2")
  ("bye" "farewell"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("greeting2".to_string()));
}

#[test]
fn test_case_with_booleans() {
    let source = r#"
(case true
  (false "no")
  (true "yes"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("yes".to_string()));
}

// ====================
// case - Multiple Values
// ====================

#[test]
fn test_case_multiple_values_single_clause() {
    let source = r#"
(case 7
  ([6 7] "weekend")
  (else "weekday"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("weekend".to_string()));
}

#[test]
fn test_case_multiple_values_first_matches() {
    let source = r#"
(case 1
  ([1 2 3] "small")
  ([4 5 6] "medium")
  (else "large"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("small".to_string()));
}

// ====================
// case - With Variables
// ====================

#[test]
fn test_case_with_variable() {
    let source = r#"
(define day 3)
(case day
  (1 "Monday")
  (2 "Tuesday")
  (3 "Wednesday")
  (else "Other"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("Wednesday".to_string()));
}

#[test]
fn test_case_with_expression() {
    let source = r#"
(case (+ 1 2)
  (2 "two")
  (3 "three")
  (4 "four"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("three".to_string()));
}

// ====================
// case - Complex Results
// ====================

#[test]
fn test_case_with_computation_result() {
    let source = r#"
(case 2
  (1 (* 10 1))
  (2 (* 10 2))
  (3 (* 10 3)))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(20));
}

#[test]
fn test_case_nested_in_function() {
    let source = r#"
(defun day-name (n)
  (case n
    (1 "Mon")
    (2 "Tue")
    (3 "Wed")
    (else "Unknown")))

(day-name 2)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("Tue".to_string()));
}

// ====================
// typecase - Basic Type Matching
// ====================

#[test]
fn test_typecase_int() {
    let source = r#"
(typecase 42
  (int "integer")
  (string "string")
  (else "other"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("integer".to_string()));
}

#[test]
fn test_typecase_string() {
    let source = r#"
(typecase "hello"
  (int "integer")
  (string "string")
  (else "other"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("string".to_string()));
}

#[test]
fn test_typecase_float() {
    let source = r#"
(typecase 3.14
  (int "integer")
  (float "floating")
  (else "other"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("floating".to_string()));
}

#[test]
fn test_typecase_bool() {
    let source = r#"
(typecase true
  (int "integer")
  (bool "boolean")
  (else "other"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("boolean".to_string()));
}

#[test]
fn test_typecase_array() {
    let source = r#"
(typecase [1 2 3]
  (int "integer")
  (array "array")
  (else "other"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("array".to_string()));
}

#[test]
fn test_typecase_null() {
    let source = r#"
(typecase null
  (int "integer")
  (null "null")
  (else "other"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("null".to_string()));
}

// ====================
// typecase - Type Aliases
// ====================

#[test]
fn test_typecase_integer_alias() {
    let source = r#"
(typecase 42
  (integer "yes")
  (else "no"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("yes".to_string()));
}

#[test]
fn test_typecase_list_alias() {
    let source = r#"
(typecase [1 2 3]
  (list "yes")
  (else "no"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("yes".to_string()));
}

// ====================
// typecase - Multiple Types
// ====================

#[test]
fn test_typecase_multiple_types() {
    let source = r#"
(typecase 42
  ([string bool] "string or bool")
  ([int float] "numeric")
  (else "other"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("numeric".to_string()));
}

// ====================
// typecase - Practical Examples
// ====================

#[test]
fn test_typecase_describe_function() {
    let source = r#"
(defun describe (x)
  (typecase x
    (int "an integer")
    (float "a float")
    (string "a string")
    (array "an array")
    (else "something")))

(describe 42)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("an integer".to_string()));
}

#[test]
fn test_typecase_with_variable() {
    let source = r#"
(define x "hello")
(typecase x
  (int "number")
  (string "text")
  (else "other"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("text".to_string()));
}

// ====================
// Combined case and typecase
// ====================

#[test]
fn test_case_and_typecase_together() {
    let source = r#"
(define x 2)

(define value-result
  (case x
    (1 "one")
    (2 "two")
    (else "other")))

(define type-result
  (typecase x
    (int "integer")
    (else "not integer")))

(str value-result " " type-result)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("two integer".to_string()));
}

// ====================
// Edge Cases
// ====================

#[test]
fn test_case_first_match_wins() {
    let source = r#"
(case 2
  (2 "first")
  (2 "second")
  (else "else"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("first".to_string()));
}

#[test]
fn test_typecase_first_match_wins() {
    let source = r#"
(typecase 42
  (int "first")
  (integer "second")
  (else "else"))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::String("first".to_string()));
}
