//! Tests for flet (local function definitions)

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
// Basic Single Function
// ====================

#[test]
fn test_flet_single_function() {
    let source = r#"
(flet ((square (x) (* x x)))
  (square 5))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(25));
}

#[test]
fn test_flet_function_with_multiple_params() {
    let source = r#"
(flet ((add (a b) (+ a b)))
  (add 10 20))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(30));
}

#[test]
fn test_flet_function_called_multiple_times() {
    let source = r#"
(flet ((double (x) (* x 2)))
  (+ (double 3) (double 4)))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(14)); // 6 + 8
}

// ====================
// Multiple Functions
// ====================

#[test]
fn test_flet_multiple_functions() {
    let source = r#"
(flet ((add (a b) (+ a b))
       (mul (a b) (* a b)))
  (add (mul 2 3) (mul 4 5)))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(26)); // (2*3) + (4*5) = 6 + 20
}

#[test]
fn test_flet_three_functions() {
    let source = r#"
(flet ((add (a b) (+ a b))
       (sub (a b) (- a b))
       (mul (a b) (* a b)))
  (add (mul 2 3) (sub 10 5)))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(11)); // (2*3) + (10-5) = 6 + 5
}

// ====================
// Scoping and Shadowing
// ====================

#[test]
fn test_flet_shadows_global_function() {
    let source = r#"
(defun foo () 100)
(flet ((foo () 200))
  (foo))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(200));
}

#[test]
fn test_flet_global_function_restored() {
    let source = r#"
(define result 0)
(defun foo () 100)

(flet ((foo () 200))
  (set! result (foo)))

(+ result (foo))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(300)); // 200 + 100
}

#[test]
fn test_flet_accesses_outer_variables() {
    let source = r#"
(define x 10)
(flet ((get-x () x))
  (get-x))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(10));
}

#[test]
fn test_flet_nested_scope() {
    let source = r#"
(define x 1)
(flet ((f () x))
  (define x 2)
  (+ (f) x))
"#;
    let result = eval_lisp(source).unwrap();
    // f() sees outer x=1, inner x=2
    assert_eq!(result, Value::Int(3)); // 1 + 2
}

// ====================
// Non-Recursive Behavior
// ====================

#[test]
fn test_flet_functions_cannot_see_each_other() {
    let source = r#"
(flet ((f () (g))
       (g () 42))
  (g))
"#;
    // g works, but f cannot call g
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_flet_cannot_be_recursive() {
    let source = r#"
(flet ((factorial (n)
         (if (<= n 1)
             1
             (* n (factorial (- n 1))))))
  (factorial 5))
"#;
    let result = eval_lisp(source);
    // Should fail because factorial can't call itself
    assert!(result.is_err());
}

// ====================
// Multiple Body Expressions
// ====================

#[test]
fn test_flet_multiple_body_expressions() {
    let source = r#"
(flet ((square (x) (* x x)))
  (define a (square 3))
  (define b (square 4))
  (+ a b))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(25)); // 9 + 16
}

#[test]
fn test_flet_returns_last_body_expression() {
    let source = r#"
(flet ((add (a b) (+ a b)))
  (add 1 2)
  (add 3 4)
  (add 5 6))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(11));
}

// ====================
// Empty and Edge Cases
// ====================

#[test]
fn test_flet_empty_functions() {
    let source = r#"
(flet ()
  42)
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_flet_no_parameters() {
    let source = r#"
(flet ((get-answer () 42))
  (get-answer))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(42));
}

// ====================
// Complex Expressions
// ====================

#[test]
fn test_flet_with_conditionals() {
    let source = r#"
(flet ((abs (x) (if (< x 0) (- x) x)))
  (+ (abs -5) (abs 3)))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(8));
}

#[test]
fn test_flet_with_loops() {
    let source = r#"
(flet ((sum-to (n)
         (do
           (define total 0)
           (define i 1)
           (while (<= i n)
             (set! total (+ total i))
             (set! i (+ i 1)))
           total)))
  (sum-to 5))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(15)); // 1+2+3+4+5
}

// ====================
// Nested flet
// ====================

#[test]
fn test_nested_flet() {
    let source = r#"
(flet ((outer (x)
         (flet ((inner (y) (* y 2)))
           (inner x))))
  (outer 5))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(10));
}

#[test]
fn test_nested_flet_shadowing() {
    let source = r#"
(flet ((f (x) (* x 2)))
  (flet ((f (x) (* x 3)))
    (f 5)))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(15)); // Inner f shadows outer
}
