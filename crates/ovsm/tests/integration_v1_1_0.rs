//! Integration test for OVSM v1.1.0 - Verifies new features work together
//!
//! This test suite verifies that GUARD clauses and TRY-CATCH work correctly
//! in combination with all other features.
//!
//! NOTE: These tests use the obsolete indentation-based Python-like syntax
//! that has been removed in favor of LISP/S-expression syntax.
//! They are disabled until converted to LISP syntax.

#![cfg(feature = "incomplete_tests")]

use ovsm::{Evaluator, Parser, Scanner, Value};

/// Helper function to execute OVSM code
fn execute(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    Ok(evaluator.execute(&program)?)
}

// ====================
// GUARD Clause Tests
// ====================

#[test]
fn test_guard_with_arithmetic() {
    let result = execute(
        r#"
        $x = 10
        $y = 20

        GUARD $x > 0 ELSE
            RETURN -1

        RETURN $x + $y
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(30));
}

#[test]
fn test_guard_with_loops() {
    let result = execute(
        r#"
        $numbers = [1, 2, 3, 4, 5]
        $sum = 0

        GUARD COUNT($numbers) > 0 ELSE
            RETURN -1

        FOR $n IN $numbers:
            $sum = $sum + $n

        RETURN $sum
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(15));
}

#[test]
fn test_multiple_guards_sequential() {
    let result = execute(
        r#"
        $a = 10
        $b = 20
        $c = 30

        GUARD $a > 0 ELSE
            RETURN "a invalid"

        GUARD $b > $a ELSE
            RETURN "b invalid"

        GUARD $c > $b ELSE
            RETURN "c invalid"

        RETURN "all valid"
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::String("all valid".to_string()));
}

// ====================
// TRY-CATCH Tests
// ====================

#[test]
fn test_try_catch_with_tools() {
    let result = execute(
        r#"
        $data = []

        TRY:
            $max = MAX($data)
        CATCH:
            $max = 0

        RETURN $max
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(0));
}

#[test]
fn test_try_catch_preserves_variables() {
    let result = execute(
        r#"
        $x = 100

        TRY:
            $y = 10 / 0
        CATCH:
            $y = 0

        RETURN $x + $y
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(100));
}

#[test]
fn test_nested_try_catch() {
    let result = execute(
        r#"
        TRY:
            TRY:
                $inner = 10 / 0
            CATCH:
                $inner = 1

            $outer = 100 / $inner
        CATCH:
            $outer = -1

        RETURN $outer
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(100));
}

// ====================
// Combined GUARD + TRY-CATCH Tests
// ====================

#[test]
fn test_guard_then_try_catch() {
    let result = execute(
        r#"
        $input = [1, 2, 3, 4, 5]

        GUARD COUNT($input) > 0 ELSE
            RETURN "empty"

        TRY:
            $sum = SUM($input)
        CATCH:
            $sum = 0

        RETURN $sum
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(15));
}

#[test]
fn test_try_catch_with_guard_inside() {
    let result = execute(
        r#"
        TRY:
            $x = 10

            GUARD $x > 0 ELSE
                RETURN -1

            $result = $x * 2
        CATCH:
            $result = 0

        RETURN $result
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(20));
}

#[test]
fn test_complex_workflow() {
    let result = execute(
        r#"
        $username = "alice"
        $password = "secret"
        $age = 25
        $status = "init"

        GUARD $username != null ELSE
            RETURN "no_username"

        GUARD $password != null ELSE
            RETURN "no_password"

        GUARD $age >= 18 ELSE
            RETURN "too_young"

        TRY:
            $user_id = 12345
            $token = "abc123"
            $status = "success"
        CATCH:
            $status = "processing_failed"

        RETURN $status
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::String("success".to_string()));
}

// ====================
// Error Recovery Patterns
// ====================

#[test]
fn test_fallback_chain() {
    let result = execute(
        r#"
        // Try multiple sources with fallbacks
        TRY:
            // Primary source (will fail - undefined variable)
            $data = $primary_source
        CATCH:
            TRY:
                // Secondary source (will also fail)
                $data = $secondary_source
            CATCH:
                // Final fallback
                $data = [1, 2, 3]

        RETURN COUNT($data)
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(3));
}

#[test]
fn test_safe_data_pipeline() {
    let result = execute(
        r#"
        $raw_data = [5, 0, 10, 0, 15]
        $results = []

        FOR $value IN $raw_data:
            TRY:
                $processed = 100 / $value
                $results = APPEND($results, $processed)
            CATCH:
                $results = APPEND($results, 0)

        RETURN $results
    "#,
    )
    .unwrap();

    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 5);
            assert_eq!(arr[0], Value::Int(20)); // 100/5
            assert_eq!(arr[1], Value::Int(0)); // caught error
            assert_eq!(arr[2], Value::Int(10)); // 100/10
            assert_eq!(arr[3], Value::Int(0)); // caught error
            assert_eq!(arr[4], Value::Int(6)); // 100/15 (integer division)
        }
        _ => panic!("Expected array result"),
    }
}

// ====================
// Real-World Scenarios
// ====================

#[test]
fn test_configuration_validation() {
    let result = execute(
        r#"
        $config = {api_key: "test123", timeout: 30, max_retries: 3}

        GUARD $config.api_key != null ELSE
            RETURN "missing_api_key"

        TRY:
            $timeout = $config.timeout
        CATCH:
            $timeout = 60

        TRY:
            $retries = $config.max_retries
        CATCH:
            $retries = 5

        GUARD $timeout > 0 ELSE
            RETURN "invalid_timeout"

        GUARD $timeout <= 300 ELSE
            RETURN "timeout_too_large"

        RETURN "valid"
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::String("valid".to_string()));
}

#[test]
fn test_batch_processing_with_error_handling() {
    let result = execute(
        r#"
        $values = [10, 20, 0, 30]
        $processed = 0

        FOR $value IN $values:
            TRY:
                $result = 1000 / $value
                $processed = $processed + 1
            CATCH:
                $processed = $processed

        RETURN $processed
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(3));
}

// ====================
// Stress Tests
// ====================

#[test]
fn test_deeply_nested_guards() {
    let result = execute(
        r#"
        $a = 1
        $b = 2
        $c = 3
        $d = 4
        $e = 5

        GUARD $a > 0 ELSE RETURN 1
        GUARD $b > $a ELSE RETURN 2
        GUARD $c > $b ELSE RETURN 3
        GUARD $d > $c ELSE RETURN 4
        GUARD $e > $d ELSE RETURN 5

        RETURN $a + $b + $c + $d + $e
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(15));
}

#[test]
fn test_deeply_nested_try_catch() {
    let result = execute(
        r#"
        TRY:
            TRY:
                TRY:
                    $x = 10 / 2
                CATCH:
                    $x = 1
            CATCH:
                $x = 2
        CATCH:
            $x = 3

        RETURN $x
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(5));
}

// ====================
// Edge Cases
// ====================

#[test]
fn test_guard_with_complex_expression() {
    let result = execute(
        r#"
        $numbers = [1, 2, 3, 4, 5]

        GUARD (COUNT($numbers) > 0) AND (SUM($numbers) > 10) ELSE
            RETURN false

        RETURN true
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Bool(true));
}

#[test]
fn test_try_catch_with_return_in_catch() {
    let result = execute(
        r#"
        $value = 0

        TRY:
            $value = 10 / 0
        CATCH:
            RETURN -1

        RETURN $value
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::Int(-1));
}

#[test]
fn test_guard_failure_stops_execution() {
    let result = execute(
        r#"
        $executed = false

        GUARD false ELSE
            RETURN "guard_failed"

        $executed = true
        RETURN "should_not_reach"
    "#,
    )
    .unwrap();

    assert_eq!(result, Value::String("guard_failed".to_string()));
}
