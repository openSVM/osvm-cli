//! Tests for labels (recursive local functions)

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
// Basic Recursion
// ====================

#[test]
fn test_labels_factorial() {
    let source = r#"
(labels ((factorial (n)
           (if (<= n 1)
               1
               (* n (factorial (- n 1))))))
  (factorial 5))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(120));
}

#[test]
fn test_labels_countdown() {
    let source = r#"
(labels ((countdown (n)
           (if (<= n 0)
               0
               (countdown (- n 1)))))
  (countdown 10))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(0));
}

#[test]
fn test_labels_sum_to_n() {
    let source = r#"
(labels ((sum-to (n)
           (if (<= n 0)
               0
               (+ n (sum-to (- n 1))))))
  (sum-to 10))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(55)); // 1+2+...+10
}

// ====================
// Mutual Recursion
// ====================

#[test]
fn test_labels_even_odd() {
    let source = r#"
(labels ((is-even (n)
           (if (= n 0)
               true
               (is-odd (- n 1))))
         (is-odd (n)
           (if (= n 0)
               false
               (is-even (- n 1)))))
  (is-even 42))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Bool(true));
}

#[test]
fn test_labels_mutual_recursion_odd() {
    let source = r#"
(labels ((is-even (n)
           (if (= n 0)
               true
               (is-odd (- n 1))))
         (is-odd (n)
           (if (= n 0)
               false
               (is-even (- n 1)))))
  (is-odd 43))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Bool(true));
}

// ====================
// Multiple Functions
// ====================

#[test]
fn test_labels_multiple_non_recursive() {
    let source = r#"
(labels ((square (x) (* x x))
         (double (x) (* x 2)))
  (+ (square 3) (double 4)))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(17)); // 9 + 8
}

// ====================
// Scoping
// ====================

#[test]
fn test_labels_shadows_global() {
    let source = r#"
(defun foo () 100)
(labels ((foo () 200))
  (foo))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(200));
}

#[test]
fn test_labels_accesses_outer_variables() {
    let source = r#"
(define multiplier 10)
(labels ((times-mult (x) (* x multiplier)))
  (times-mult 5))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(50));
}

// ====================
// Complex Examples
// ====================

#[test]
fn test_labels_fibonacci() {
    let source = r#"
(labels ((fib (n)
           (if (<= n 1)
               n
               (+ (fib (- n 1))
                  (fib (- n 2))))))
  (fib 10))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(55));
}

#[test]
fn test_labels_gcd() {
    let source = r#"
(labels ((gcd (a b)
           (if (= b 0)
               a
               (gcd b (% a b)))))
  (gcd 48 18))
"#;
    let result = eval_lisp(source).unwrap();
    assert_eq!(result, Value::Int(6));
}
