//! Tests for let* (sequential binding)

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
// Basic Sequential Binding
// ====================

#[test]
fn test_let_star_sequential_simple() {
    let source = r#"
(let* ((x 10)
       (y (+ x 5)))
  y)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(15));
}

#[test]
fn test_let_star_chain_dependencies() {
    let source = r#"
(let* ((a 1)
       (b (+ a 1))
       (c (+ b 1))
       (d (+ c 1)))
  d)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(4));
}

#[test]
fn test_let_star_reference_previous() {
    let source = r#"
(let* ((x 5)
       (y (* x 2))
       (z (+ x y)))
  z)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(15)); // 5 + (5 * 2) = 15
}

// ====================
// Comparison with let (should fail with let)
// ====================

#[test]
fn test_let_cannot_reference_previous() {
    // This should fail because 'let' binds in parallel
    let source = r#"
(let ((x 10)
      (y (+ x 5)))
  y)
"#;
    let result = eval_lisp(source);
    // Should error because x is not defined in y's binding context
    assert!(result.is_err());
}

#[test]
fn test_let_star_vs_let_behavior() {
    // let* allows sequential reference
    let source_star = r#"
(let* ((x 10)
       (y x))
  y)
"#;
    let result_star = eval_lisp(source_star).unwrap();
    assert_eq!(result_star, Value::Int(10));
}

// ====================
// Multiple Body Expressions
// ====================

#[test]
fn test_let_star_multiple_body() {
    let source = r#"
(let* ((x 5)
       (y 10))
  (define sum (+ x y))
  (define product (* x y))
  (+ sum product))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(65)); // (5 + 10) + (5 * 10) = 15 + 50
}

#[test]
fn test_let_star_side_effects() {
    let source = r#"
(let* ((x 1)
       (y (+ x 1))
       (z (+ y 1)))
  (log :message "x:" :value x)
  (log :message "y:" :value y)
  (log :message "z:" :value z)
  (+ x y z))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(6)); // 1 + 2 + 3
}

// ====================
// Complex Expressions
// ====================

#[test]
fn test_let_star_nested_computation() {
    let source = r#"
(let* ((base 2)
       (squared (* base base))
       (cubed (* squared base)))
  cubed)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(8)); // 2^3
}

#[test]
fn test_let_star_with_conditionals() {
    let source = r#"
(let* ((x 10)
       (y (if (> x 5) 20 0))
       (z (+ x y)))
  z)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(30)); // 10 + 20
}

#[test]
fn test_let_star_with_lambda() {
    let source = r#"
(let* ((double (lambda (x) (* x 2)))
       (result (double 5)))
  result)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(10));
}

// ====================
// Scoping
// ====================

#[test]
fn test_let_star_scoping() {
    let source = r#"
(do
  (define x 100)
  (let* ((x 10)
         (y (+ x 5)))
    y))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(15)); // Inner x shadows outer
}

#[test]
fn test_let_star_outer_scope_preserved() {
    let source = r#"
(do
  (define x 100)
  (let* ((y 10)
         (z (+ y 5)))
    z)
  x)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(100)); // Outer x unchanged
}

// ====================
// Edge Cases
// ====================

#[test]
fn test_let_star_empty_bindings() {
    let source = r#"
(let* ()
  42)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_let_star_single_binding() {
    let source = r#"
(let* ((x 10))
  x)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(10));
}

#[test]
fn test_let_star_override_previous() {
    let source = r#"
(let* ((x 10)
       (x 20))
  x)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(20)); // Later binding shadows earlier
}

// ====================
// Advanced Usage
// ====================

#[test]
fn test_let_star_with_arrays() {
    let source = r#"
(let* ((arr [1 2 3])
       (first (nth arr 0))
       (doubled (* first 2)))
  doubled)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(2));
}

#[test]
fn test_let_star_with_objects() {
    let source = r#"
(let* ((person {:name "Alice" :age 30})
       (age (get person :age))
       (next-age (+ age 1)))
  next-age)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(31));
}

#[test]
fn test_let_star_accumulator_pattern() {
    let source = r#"
(let* ((a 1)
       (b (+ a 2))
       (c (+ a b 3))
       (d (+ a b c 4)))
  d)
"#;
    let result = eval_lisp(source).unwrap();
    // a=1, b=1+2=3, c=1+3+3=7, d=1+3+7+4=15
    assert_eq!(result, Value::Int(15));
}
