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
