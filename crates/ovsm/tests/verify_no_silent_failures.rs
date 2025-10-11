//! Verification Test - Ensures Silent Failures Are Eliminated
//!
//! This test suite would have FAILED before v1.1.0 and now PASSES.
//! It verifies that all previously-silent features now either work or error loudly.

use ovsm::{Error, Evaluator, Parser, Scanner, Value};

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
// VERIFICATION: Features that were silently failing now work or error
// ====================

#[test]
fn test_try_catch_actually_catches_errors() {
    // Before v1.1.0: This would have executed both TRY and the code after CATCH,
    // ignoring the CATCH block entirely, returning Null instead of 0
    let result = execute(
        r#"
        TRY:
            $x = 10 / 0
        CATCH:
            $x = 0
        RETURN $x
    "#,
    )
    .unwrap();

    // This assertion would FAIL before v1.1.0 (would be Null)
    assert_eq!(
        result,
        Value::Int(0),
        "TRY-CATCH must actually catch errors"
    );
}

#[test]
fn test_guard_else_actually_executes() {
    // Before v1.1.0: The ELSE body was parsed as empty,
    // so this would return Null instead of the error message
    let result = execute(
        r#"
        $x = -1
        GUARD $x > 0 ELSE
            RETURN "error"
        RETURN $x
    "#,
    )
    .unwrap();

    // This assertion would FAIL before v1.1.0 (would be Null)
    assert_eq!(
        result,
        Value::String("error".to_string()),
        "GUARD ELSE body must execute when condition is false"
    );
}

#[test]
fn test_parallel_errors_loudly() {
    // Before v1.1.0: This would silently succeed with no error
    // Now: Must return NotImplemented error
    let code = r#"
        PARALLEL {
            $task1 = 1
            $task2 = 2
        }
        RETURN $task1
    "#;

    let result = execute(code);

    // This assertion would FAIL before v1.1.0 (would succeed silently)
    assert!(result.is_err(), "PARALLEL must error, not silently fail");

    let err_msg = format!("{:?}", result.unwrap_err());
    assert!(
        err_msg.contains("NotImplemented") || err_msg.contains("PARALLEL"),
        "Error must mention PARALLEL or NotImplemented, got: {}",
        err_msg
    );
}

#[test]
fn test_decision_errors_loudly() {
    // Before v1.1.0: This would silently succeed with no error
    // Now: Must return NotImplemented error
    let code = r#"
        DECISION "test":
            BRANCH "a": $x = 1
            BRANCH "b": $x = 2
        RETURN $x
    "#;

    let result = execute(code);

    // This assertion would FAIL before v1.1.0 (would succeed silently)
    assert!(result.is_err(), "DECISION must error, not silently fail");

    let err_msg = format!("{:?}", result.unwrap_err());
    assert!(
        err_msg.contains("NotImplemented") || err_msg.contains("DECISION"),
        "Error must mention DECISION or NotImplemented, got: {}",
        err_msg
    );
}

#[test]
fn test_wait_strategy_errors_loudly() {
    // Before v1.1.0: This would silently succeed with no error
    // Now: Must return NotImplemented error
    let code = r#"
        WAIT exponential_backoff
        RETURN 1
    "#;

    let result = execute(code);

    // This assertion would FAIL before v1.1.0 (would succeed silently)
    assert!(result.is_err(), "WAIT must error, not silently fail");

    let err_msg = format!("{:?}", result.unwrap_err());
    assert!(
        err_msg.contains("NotImplemented")
            || err_msg.contains("WAIT")
            || err_msg.contains("WaitStrategy"),
        "Error must mention WAIT/WaitStrategy or NotImplemented, got: {}",
        err_msg
    );
}

// ====================
// REGRESSION TESTS: Prove the bug fix
// ====================

#[test]
fn test_nested_try_catch_preserves_outer_scope() {
    // Complex scenario that tests error propagation
    let result = execute(
        r#"
        $outer = "initial"

        TRY:
            TRY:
                $inner = 10 / 0
            CATCH:
                $inner = 1

            $outer = "success"
        CATCH:
            $outer = "outer_error"

        RETURN $outer
    "#,
    )
    .unwrap();

    assert_eq!(
        result,
        Value::String("success".to_string()),
        "Nested TRY-CATCH must handle errors correctly"
    );
}

#[test]
fn test_multiple_guards_stop_at_first_failure() {
    // Tests that GUARD actually stops execution
    let result = execute(
        r#"
        $step = 0

        $step = 1
        GUARD true ELSE
            RETURN "guard1"

        $step = 2
        GUARD false ELSE
            RETURN "guard2"

        $step = 3
        RETURN "should_not_reach"
    "#,
    )
    .unwrap();

    assert_eq!(
        result,
        Value::String("guard2".to_string()),
        "Second guard must execute and stop execution"
    );
}

#[test]
fn test_try_catch_with_return_in_catch() {
    // Tests that CATCH block can early-return
    let result = execute(
        r#"
        TRY:
            $x = 10 / 0
        CATCH:
            RETURN -1

        RETURN 999
    "#,
    )
    .unwrap();

    assert_eq!(
        result,
        Value::Int(-1),
        "CATCH block with RETURN must prevent further execution"
    );
}

// ====================
// EDGE CASES: Ensure robustness
// ====================

#[test]
fn test_try_catch_with_no_error() {
    // TRY succeeds, CATCH should not execute
    let result = execute(
        r#"
        TRY:
            $x = 10
        CATCH:
            $x = -1

        RETURN $x
    "#,
    )
    .unwrap();

    assert_eq!(
        result,
        Value::Int(10),
        "CATCH must not execute when TRY succeeds"
    );
}

#[test]
fn test_guard_with_true_condition() {
    // GUARD passes, ELSE should not execute
    let result = execute(
        r#"
        GUARD true ELSE
            RETURN "failed"

        RETURN "passed"
    "#,
    )
    .unwrap();

    assert_eq!(
        result,
        Value::String("passed".to_string()),
        "GUARD ELSE must not execute when condition is true"
    );
}

#[test]
fn test_try_catch_with_undefined_variable_error() {
    // Tests catching different error types
    let result = execute(
        r#"
        TRY:
            $x = $undefined_variable
        CATCH:
            $x = "caught"

        RETURN $x
    "#,
    )
    .unwrap();

    assert_eq!(
        result,
        Value::String("caught".to_string()),
        "TRY-CATCH must catch undefined variable errors"
    );
}

#[test]
fn test_try_catch_with_tool_error() {
    // Tests catching tool errors (empty array to MAX)
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

    assert_eq!(result, Value::Int(0), "TRY-CATCH must catch tool errors");
}

// ====================
// FALSIFICATION TEST: The Ultimate Verification
// ====================

#[test]
fn test_would_catch_original_bug() {
    // This test is specifically designed to FAIL if we reverted to the buggy code.
    // The original bug was: `_ => Ok(ExecutionFlow::Continue)`
    // This caused TRY-CATCH to be silently skipped.

    let code = r#"
        $marker = "before"

        TRY:
            $marker = "in_try"
            $error = 10 / 0
            $marker = "after_error"
        CATCH:
            $marker = "in_catch"

        RETURN $marker
    "#;

    let result = execute(code).unwrap();

    // With the bug: Would return "in_try" (TRY executed, CATCH ignored, error silently continued)
    // Without bug: Returns "in_catch" (error caught, CATCH executed)
    assert_eq!(
        result,
        Value::String("in_catch".to_string()),
        "CRITICAL: If this fails, the silent failure bug has returned!"
    );
}
