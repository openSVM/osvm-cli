/// End-to-end integration test for LISP syntax
/// Demonstrates: Lexer → Parser → Evaluator working together

use ovsm::lexer::SExprScanner;
use ovsm::parser::SExprParser;
use ovsm::runtime::{LispEvaluator, Value};

#[test]
fn test_lisp_e2e_simple_arithmetic() {
    let source = "(+ 1 2 3)";
    
    // Lex
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    
    // Parse
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    
    // Evaluate
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    
    assert_eq!(result, Value::Int(6));
}

#[test]
fn test_lisp_e2e_variables() {
    let source = r#"
        (define x 10)
        (define y 20)
        (+ x y)
    "#;
    
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    
    assert_eq!(result, Value::Int(30));
}

#[test]
fn test_lisp_e2e_mutation() {
    let source = r#"
        (define counter 0)
        (set! counter (+ counter 1))
        (set! counter (+ counter 1))
        counter
    "#;
    
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    
    assert_eq!(result, Value::Int(2));
}

#[test]
fn test_lisp_e2e_if_expression() {
    let source = r#"
        (define x 10)
        (if (> x 5)
            "large"
            "small")
    "#;
    
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    
    assert_eq!(result, Value::String("large".to_string()));
}

#[test]
fn test_lisp_e2e_critical_if_in_while() {
    // THIS IS THE CRITICAL TEST - IF-THEN-ELSE INSIDE WHILE
    // This would be BUGGY in Python-style syntax but works in LISP!
    let source = r#"
        (define done false)
        (define count 0)
        
        (while (not done)
            (if (== count 0)
                (set! count 1)
                (set! count 2))
            (set! done true))
        
        count
    "#;
    
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    
    // This would fail to parse or execute incorrectly in Python-style
    // But works perfectly in LISP because parentheses are explicit!
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    
    // The while loop should execute once and set count to 1
    assert_eq!(result, Value::Int(1));
}

#[test]
fn test_while_simple() {
    let source = r#"
        (define x 0)
        (while (< x 3)
            (set! x (+ x 1)))
        x
    "#;
    
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    
    assert_eq!(result, Value::Int(3));
}

#[test]
fn test_lambda_creation_and_call() {
    // Test creating and calling a lambda function
    let source = r#"
        (define double (lambda (x) (* x 2)))
        (double 5)
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Int(10));
}

#[test]
fn test_lambda_multiple_params() {
    // Test lambda with multiple parameters
    let source = r#"
        (define add (lambda (x y) (+ x y)))
        (add 3 7)
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Int(10));
}

#[test]
fn test_map_with_lambda() {
    // Test map with lambda function - doubles each element
    let source = r#"
        (map [1 2 3 4 5] (lambda (x) (* x 2)))
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    let expected = Value::array(vec![
        Value::Int(2),
        Value::Int(4),
        Value::Int(6),
        Value::Int(8),
        Value::Int(10),
    ]);

    assert_eq!(result, expected);
}

#[test]
fn test_filter_with_lambda() {
    // Test filter with lambda - keep only even numbers
    let source = r#"
        (filter [1 2 3 4 5 6] (lambda (x) (== (% x 2) 0)))
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    let expected = Value::array(vec![
        Value::Int(2),
        Value::Int(4),
        Value::Int(6),
    ]);

    assert_eq!(result, expected);
}

#[test]
fn test_reduce_with_lambda() {
    // Test reduce with lambda - sum all elements
    let source = r#"
        (reduce [1 2 3 4 5] 0 (lambda (acc x) (+ acc x)))
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Int(15));
}

#[test]
fn test_reduce_product() {
    // Test reduce with lambda - multiply all elements
    let source = r#"
        (reduce [2 3 4] 1 (lambda (acc x) (* acc x)))
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Int(24)); // 2 * 3 * 4 = 24
}

#[test]
fn test_chained_higher_order_functions() {
    // Test chaining map, filter, and reduce together
    // Filter evens, double them, then sum
    let source = r#"
        (define nums [1 2 3 4 5 6])
        (define evens (filter nums (lambda (x) (== (% x 2) 0))))
        (define doubled (map evens (lambda (x) (* x 2))))
        (reduce doubled 0 (lambda (acc x) (+ acc x)))
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    // evens: [2, 4, 6]
    // doubled: [4, 8, 12]
    // sum: 24
    assert_eq!(result, Value::Int(24));
}

#[test]
fn test_lambda_closure_simple() {
    // Test lambda capturing variables from outer scope
    let source = r#"
        (define multiplier 3)
        (define times_three (lambda (x) (* x multiplier)))
        (times_three 5)
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Int(15));
}

#[test]
fn test_defun_syntax() {
    // Test function definition with defun
    // Note: defun uses array syntax for parameters
    let source = r#"
        (defun square [x] (* x x))
        (square 7)
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Int(49));
}

#[test]
fn test_map_with_defun() {
    // Test using defun-defined function with map
    // Note: defun uses array syntax for parameters
    let source = r#"
        (defun increment [x] (+ x 1))
        (map [10 20 30] increment)
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    let expected = Value::array(vec![
        Value::Int(11),
        Value::Int(21),
        Value::Int(31),
    ]);

    assert_eq!(result, expected);
}
