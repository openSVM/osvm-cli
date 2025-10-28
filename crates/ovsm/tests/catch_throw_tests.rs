//! Tests for catch/throw - Non-local exits in OVSM
//!
//! This test suite covers:
//! - Basic catch/throw mechanics
//! - Nested catch blocks
//! - Multiple throw points
//! - Tag matching
//! - Error cases (uncaught throws)

use ovsm::{LispEvaluator, SExprParser, SExprScanner, Value};

/// Helper function to execute OVSM LISP code
fn eval(code: &str) -> Result<Value, ovsm::Error> {
    let mut scanner = SExprScanner::new(code);
    let tokens = scanner.scan_tokens()?;
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = LispEvaluator::new();
    evaluator.execute(&program)
}

/// Helper to assert successful evaluation
fn assert_eval(code: &str, expected: Value) {
    let result = eval(code);
    assert!(result.is_ok(), "Failed to evaluate: {:?}", result);
    assert_eq!(result.unwrap(), expected);
}

#[test]
fn test_basic_catch_throw() {
    // Basic throw returns value
    let code = r#"
        (catch "done"
          (throw "done" 42))
    "#;
    assert_eval(code, Value::Int(42));
}

#[test]
fn test_catch_with_no_throw() {
    // Catch without throw returns last expression value
    let code = r#"
        (catch "done"
          (define x 10)
          (+ x 20))
    "#;
    assert_eval(code, Value::Int(30));
}

#[test]
fn test_throw_skips_remaining_code() {
    // Throw should skip remaining expressions
    let code = r#"
        (catch "exit"
          (define result 0)
          (set! result 10)
          (throw "exit" result)
          (set! result 999))  ; This should not execute
    "#;
    assert_eval(code, Value::Int(10));
}

#[test]
fn test_nested_catch_inner() {
    // Throw to inner catch
    let code = r#"
        (catch "outer"
          (catch "inner"
            (throw "inner" "caught by inner"))
          "after inner")
    "#;
    assert_eval(code, Value::String("after inner".to_string()));
}

#[test]
fn test_nested_catch_outer() {
    // Throw past inner to outer catch
    let code = r#"
        (catch "outer"
          (catch "inner"
            (throw "outer" "escaped to outer"))
          "should not reach")
    "#;
    assert_eval(code, Value::String("escaped to outer".to_string()));
}

#[test]
fn test_multiple_catches_same_tag() {
    // Multiple catches with same tag - should catch at innermost
    let code = r#"
        (catch "exit"
          (catch "exit"
            (throw "exit" "inner"))
          "middle")
    "#;
    assert_eval(code, Value::String("middle".to_string()));
}

#[test]
fn test_throw_with_complex_expression() {
    // Throw value can be complex expression
    let code = r#"
        (catch "result"
          (define nums [1 2 3 4 5])
          (define sum (+ (first nums) (last nums)))
          (throw "result" sum))
    "#;
    assert_eval(code, Value::Int(6));
}

#[test]
fn test_throw_with_object() {
    // Throw can return any value type
    let code = r#"
        (catch "data"
          (throw "data" {:status "ok" :value 123}))
    "#;

    let mut expected = std::collections::HashMap::new();
    expected.insert("status".to_string(), Value::String("ok".to_string()));
    expected.insert("value".to_string(), Value::Int(123));
    assert_eval(code, Value::object(expected));
}

#[test]
fn test_throw_with_array() {
    // Throw array value
    let code = r#"
        (catch "list"
          (throw "list" [1 2 3]))
    "#;
    assert_eval(
        code,
        Value::array(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
    );
}

#[test]
fn test_catch_in_function() {
    // Catch/throw works across function boundaries
    let code = r#"
        (defun risky-operation (x)
          (if (< x 0)
              (throw "error" "negative input")
              (* x 2)))

        (catch "error"
          (risky-operation 5))
    "#;
    assert_eval(code, Value::Int(10));
}

#[test]
fn test_throw_across_function() {
    // Throw from inside function
    let code = r#"
        (defun might-throw (x)
          (if (> x 10)
              (throw "too-big" x)
              (* x 2)))

        (catch "too-big"
          (might-throw 15))
    "#;
    assert_eval(code, Value::Int(15));
}

#[test]
fn test_catch_with_iteration() {
    // Early exit from loop using throw
    let code = r#"
        (catch "found"
          (define nums [1 2 3 4 5 6 7 8 9 10])
          (for (n nums)
            (if (> n 5)
                (throw "found" n)
                null)))
    "#;
    assert_eval(code, Value::Int(6));
}

#[test]
fn test_multiple_throws_in_body() {
    // Multiple conditional throws
    let code = r#"
        (catch "result"
          (define x 7)
          (if (< x 5)
              (throw "result" "small")
              null)
          (if (> x 10)
              (throw "result" "large")
              null)
          (throw "result" "medium"))
    "#;
    assert_eval(code, Value::String("medium".to_string()));
}

#[test]
fn test_catch_preserves_scope() {
    // Variables defined in catch are not visible outside
    let code = r#"
        (catch "exit"
          (define local-var 42)
          local-var)
    "#;
    assert_eval(code, Value::Int(42));
}

#[test]
fn test_throw_with_null() {
    // Can throw null
    let code = r#"
        (catch "null-test"
          (throw "null-test" null))
    "#;
    assert_eval(code, Value::Null);
}

#[test]
fn test_throw_with_boolean() {
    // Can throw booleans
    let code = r#"
        (catch "bool"
          (throw "bool" true))
    "#;
    assert_eval(code, Value::Bool(true));
}

#[test]
fn test_uncaught_throw_is_error() {
    // Throw without matching catch should error
    let code = r#"
        (throw "uncaught" 42)
    "#;
    let result = eval(code);
    assert!(result.is_err());

    // Verify it's a ThrowValue error
    if let Err(e) = result {
        let error_msg = format!("{}", e);
        assert!(error_msg.contains("Uncaught throw"));
        assert!(error_msg.contains("uncaught"));
    }
}

#[test]
fn test_wrong_tag_is_error() {
    // Throw with non-matching tag should error
    let code = r#"
        (catch "expected"
          (throw "unexpected" 42))
    "#;
    let result = eval(code);
    assert!(result.is_err());
}

#[test]
fn test_deeply_nested_throws() {
    // Complex nesting scenario
    let code = r#"
        (catch "level1"
          (catch "level2"
            (catch "level3"
              (throw "level1" "escaped 3 levels"))))
    "#;
    assert_eval(code, Value::String("escaped 3 levels".to_string()));
}

#[test]
fn test_throw_in_recursive_function() {
    // Throw from recursive function
    let code = r#"
        (defun countdown (n)
          (if (<= n 0)
              (throw "done" "finished")
              (countdown (- n 1))))

        (catch "done"
          (countdown 5))
    "#;
    assert_eval(code, Value::String("finished".to_string()));
}

#[test]
fn test_tag_evaluation() {
    // Tag can be an expression (evaluates to string)
    let code = r#"
        (define tag-name "my-tag")
        (catch tag-name
          (throw tag-name 100))
    "#;
    assert_eval(code, Value::Int(100));
}
