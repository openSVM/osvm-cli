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

    let expected = Value::array(vec![Value::Int(2), Value::Int(4), Value::Int(6)]);

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

    let expected = Value::array(vec![Value::Int(11), Value::Int(21), Value::Int(31)]);

    assert_eq!(result, expected);
}

// ============================================
// Phase 1: Core LISP Essentials Tests
// ============================================

#[test]
fn test_cond_multi_branch() {
    // Test cond with multiple branches
    let source = r#"
        (define score 85)
        (cond
          ((>= score 90) "A")
          ((>= score 80) "B")
          ((>= score 70) "C")
          (else "F"))
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::String("B".to_string()));
}

#[test]
fn test_cond_first_match() {
    // Test that cond returns first matching branch
    let source = r#"
        (define x 50)
        (cond
          ((< x 100) "first")
          ((< x 200) "second")
          (else "third"))
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::String("first".to_string()));
}

#[test]
fn test_unless_basic() {
    // Test unless executes when condition is false
    let source = r#"
        (unless (> 3 5) "correct")
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::String("correct".to_string()));
}

#[test]
fn test_first_rest_cons() {
    // Test first, rest, cons
    let source = r#"
        (define nums [1 2 3 4 5])
        (define head (first nums))
        (define tail (rest nums))
        (define new_list (cons 0 nums))
        (+ head (first new_list))
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Int(1)); // 1 + 0 = 1
}

#[test]
fn test_recursive_list_sum() {
    // Test recursive processing with first and rest
    let source = r#"
        (defun sum-list [lst]
          (if (empty? lst)
              0
              (+ (first lst) (sum-list (rest lst)))))

        (sum-list [1 2 3 4 5])
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Int(15));
}

// ============================================================================
// Type Predicate Tests
// ============================================================================

#[test]
fn test_int_predicate() {
    // Test true case
    let source = "(int? 42)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Bool(true));

    // Test false case
    let source2 = "(int? 3.14)";
    let mut scanner = SExprScanner::new(source2);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_float_predicate() {
    let source = "(float? 3.14)";

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Bool(true));

    // Test false case
    let source2 = "(float? 42)";
    let mut scanner = SExprScanner::new(source2);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_number_predicate() {
    // Test int
    let source = "(number? 42)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Bool(true));

    // Test float
    let source2 = "(number? 3.14)";
    let mut scanner = SExprScanner::new(source2);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Bool(true));

    // Test false case
    let source3 = "(number? \"hello\")";
    let mut scanner = SExprScanner::new(source3);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_string_predicate() {
    let source = "(string? \"hello\")";

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Bool(true));

    // Test false case
    let source2 = "(string? 42)";
    let mut scanner = SExprScanner::new(source2);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_bool_predicate() {
    let source = "(bool? true)";

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Bool(true));

    // Test false case
    let source2 = "(bool? 1)";
    let mut scanner = SExprScanner::new(source2);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_array_predicate() {
    let source = "(array? [1 2 3])";

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Bool(true));

    // Test false case
    let source2 = "(array? \"hello\")";
    let mut scanner = SExprScanner::new(source2);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_object_predicate() {
    let source = r#"(object? {:name "Alice" :age 30})"#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Bool(true));

    // Test false case
    let source2 = "(object? [1 2 3])";
    let mut scanner = SExprScanner::new(source2);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_function_predicate() {
    // Test true case - check lambda directly
    let source = "(function? (lambda (x) (+ x 1)))";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Bool(true));

    // Test false case
    let source2 = "(function? 42)";
    let mut scanner = SExprScanner::new(source2);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_type_predicates_in_conditional() {
    // Practical use case: Type-based branching
    let source = r#"
        (define check-type (lambda (x)
            (cond
                ((int? x) "integer")
                ((float? x) "float")
                ((string? x) "string")
                ((array? x) "array")
                (true "unknown"))))

        (check-type 42)
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::String("integer".to_string()));

    // Test with string
    let source2 = r#"
        (define check-type (lambda (x)
            (cond
                ((int? x) "integer")
                ((float? x) "float")
                ((string? x) "string")
                ((array? x) "array")
                (true "unknown"))))

        (check-type "hello")
    "#;

    let mut scanner = SExprScanner::new(source2);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::String("string".to_string()));
}

// ============================================================================
// Assertion Tests
// ============================================================================

#[test]
fn test_assert_success() {
    // Assertion passes - should return true
    let source = "(assert (> 10 5) \"x must be greater than 5\")";

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Null);
}

#[test]
fn test_assert_failure() {
    // Assertion fails - should error
    let source = "(assert (> 3 5) \"x must be greater than 5\")";

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(format!("{}", err).contains("Assertion failed"));
    assert!(format!("{}", err).contains("x must be greater than 5"));
}

#[test]
fn test_assert_type_success() {
    // Type assertion passes
    let source = "(assert-type 42 (int? 42))";

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Null);
}

#[test]
fn test_assert_type_failure() {
    // Type assertion fails
    let source = "(assert-type \"hello\" (int? \"hello\"))";

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(format!("{}", err).contains("Type assertion failed"));
}

#[test]
fn test_assertions_in_function() {
    // Practical use case: Function with preconditions
    let source = r#"
        (defun safe-divide [a b]
          (do
            (assert (number? a) "First argument must be a number")
            (assert (number? b) "Second argument must be a number")
            (assert (!= b 0) "Cannot divide by zero")
            (/ a b)))

        (safe-divide 10 2)
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Int(5));
}

#[test]
fn test_assertions_guard_against_invalid_input() {
    // Test that assertions catch invalid input
    let source = r#"
        (defun safe-divide [a b]
          (do
            (assert (number? a) "First argument must be a number")
            (assert (number? b) "Second argument must be a number")
            (assert (!= b 0) "Cannot divide by zero")
            (/ a b)))

        (safe-divide 10 0)
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program);

    assert!(result.is_err());
    let err = result.unwrap_err();
    let err_msg = format!("{}", err);
    // Should catch the assertion error OR division by zero error
    assert!(
        err_msg.to_lowercase().contains("division by zero") || err_msg.contains("Assertion failed")
    );
}

// ============================================================================
// Error Handling Tests
// ============================================================================

#[test]
fn test_error_throw() {
    // Test error function
    let source = "(error \"Something went wrong\")";

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(format!("{}", err).contains("Something went wrong"));
}

#[test]
fn test_try_catch_success() {
    // Try block succeeds - catch not executed
    let source = r#"
        (try
          (+ 1 2 3)
          (catch e "error"))
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::Int(6));
}

#[test]
fn test_try_catch_failure() {
    // Try block fails - catch executes
    let source = r#"
        (try
          (error "boom")
          (catch e "caught"))
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::String("caught".to_string()));
}

#[test]
fn test_try_catch_finally() {
    // Test finally block always executes
    let source = r#"
        (try
          (+ 1 2)
          (catch e "error")
          (finally (log :message "cleanup")))
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
fn test_error_handling_in_function() {
    // Practical use case: Safe network operation
    let source = r#"
        (defun safe-fetch [url]
          (try
            (if (== url "")
                (error "URL cannot be empty")
                (str "Fetched: " url))
            (catch e
              (str "Error: " e))))

        (safe-fetch "")
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    let result_str = match result {
        Value::String(s) => s,
        _ => panic!("Expected string result"),
    };
    assert!(result_str.contains("Error:"));
    assert!(result_str.contains("URL cannot be empty"));
}

// ============================================================================
// ADVANCED MATH OPERATIONS TESTS
// ============================================================================

#[test]
fn test_sqrt_basic() {
    let source = "(sqrt 16)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Float(4.0));
}

#[test]
#[allow(clippy::approx_constant)]
fn test_sqrt_float() {
    let source = "(sqrt 2.0)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    match result {
        Value::Float(f) => {
            assert!((f - 1.4142135623730951).abs() < 0.0001);
        }
        _ => panic!("Expected float result"),
    }
}

#[test]
fn test_sqrt_negative_error() {
    let source = "(sqrt -4)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program);

    assert!(result.is_err());
    assert!(format!("{}", result.unwrap_err()).contains("Cannot take square root of negative"));
}

#[test]
fn test_pow_basic() {
    let source = "(pow 2 3)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Float(8.0));
}

#[test]
fn test_pow_negative_exponent() {
    let source = "(pow 2 -2)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Float(0.25));
}

#[test]
fn test_pow_fractional_exponent() {
    let source = "(pow 16 0.5)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Float(4.0));
}

#[test]
fn test_abs_positive_int() {
    let source = "(abs 42)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_abs_negative_int() {
    let source = "(abs -42)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
#[allow(clippy::approx_constant)]
fn test_abs_negative_float() {
    let source = "(abs -3.14)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Float(3.14));
}

#[test]
fn test_math_in_expression() {
    let source = r#"
        (define distance 100)
        (define time 2.5)
        (define speed (/ distance time))
        (pow speed 2)
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Float(1600.0));
}

#[test]
fn test_pythagorean_theorem() {
    let source = r#"
        (define a 3)
        (define b 4)
        (define c-squared (+ (pow a 2) (pow b 2)))
        (sqrt c-squared)
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Float(5.0));
}

#[test]
fn test_math_in_function() {
    let source = r#"
        (defun hypotenuse [a b]
          (sqrt (+ (pow a 2) (pow b 2))))
        (hypotenuse 3 4)
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Float(5.0));
}

#[test]
fn test_abs_in_distance_calculation() {
    let source = r#"
        (defun distance [x1 x2]
          (abs (- x1 x2)))
        (distance 10 25)
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(15));
}

// ============================================================================
// MULTIPLE VALUES TESTS (Common Lisp)
// ============================================================================

#[test]
fn test_values_single() {
    // Single value - returns unwrapped
    let source = "(values 42)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_values_multiple() {
    // Multiple values - wrapped in Value::Multiple
    let source = "(values 1 2 3)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    match result {
        Value::Multiple(vals) => {
            assert_eq!(vals.len(), 3);
            assert_eq!(vals[0], Value::Int(1));
            assert_eq!(vals[1], Value::Int(2));
            assert_eq!(vals[2], Value::Int(3));
        }
        _ => panic!("Expected Multiple, got {:?}", result),
    }
}

#[test]
fn test_values_empty() {
    // No values - returns null
    let source = "(values)";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Null);
}

#[test]
fn test_multiple_value_bind_basic() {
    let source = r#"
        (multiple-value-bind [x y z] (values 1 2 3)
          (+ x y z))
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(6));
}

#[test]
fn test_multiple_value_bind_excess_values() {
    // More values than variables - extra ignored
    let source = r#"
        (multiple-value-bind [x y] (values 1 2 3 4 5)
          (+ x y))
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
fn test_multiple_value_bind_missing_values() {
    // More variables than values - missing bound to null
    let source = r#"
        (multiple-value-bind [x y z] (values 1 2)
          (if (null? z)
              (+ x y)
              (+ x y z)))
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
fn test_multiple_value_bind_single_value() {
    // Bind from single value (not Multiple)
    let source = r#"
        (multiple-value-bind [x y] 42
          (if (null? y)
              x
              (+ x y)))
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_multiple_values_in_function() {
    let source = r#"
        (defun divmod [a b]
          (values (/ a b) (% a b)))

        (multiple-value-bind [quotient remainder] (divmod 17 5)
          (+ (* quotient 10) remainder))
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(32)); // 3*10 + 2
}

#[test]
fn test_multiple_values_nested() {
    let source = r#"
        (multiple-value-bind [a b] (values 1 2)
          (multiple-value-bind [c d] (values 3 4)
            (+ a b c d)))
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(10));
}

// ============================================================================
// DYNAMIC VARIABLES TESTS (Common Lisp special variables)
// ============================================================================

#[test]
fn test_defvar_basic() {
    let source = r#"
        (defvar counter 0)
        counter
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(0));
}

#[test]
fn test_defvar_mutation() {
    let source = r#"
        (defvar counter 0)
        (set! counter 42)
        counter
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_defvar_global_scope() {
    // Dynamic variables are globally accessible
    let source = r#"
        (defvar global-var 100)

        (defun get-global []
          global-var)

        (get-global)
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(100));
}

#[test]
fn test_defvar_mutation_in_function() {
    let source = r#"
        (defvar count 0)

        (defun increment []
          (set! count (+ count 1)))

        (increment)
        (increment)
        (increment)
        count
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
fn test_defvar_with_expression() {
    let source = r#"
        (defvar result-var (+ 10 20 30))
        result-var
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(60));
}

#[test]
fn test_multiple_defvars() {
    let source = r#"
        (defvar x 10)
        (defvar y 20)
        (defvar z 30)
        (+ x y z)
    "#;
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();
    assert_eq!(result, Value::Int(60));
}
