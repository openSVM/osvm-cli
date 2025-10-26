//! Comprehensive error handling tests for OVSM interpreter
//!
//! This test suite verifies that all error types are properly triggered
//! and handled across the lexer, parser, and runtime.
//!
//! NOTE: This file is currently disabled because it tests Python-style OVSM syntax
//! which has been completely removed in favor of LISP/S-expression syntax.
//! These tests should be rewritten using LISP syntax or removed entirely.

#![cfg(feature = "incomplete_tests")]

use ovsm::{Error, Evaluator, Parser, Scanner};

/// Helper to parse and execute OVSM code
fn execute(source: &str) -> Result<ovsm::Value, Error> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    evaluator.execute(&program)
}

// ====================
// Parse Errors
// ====================

#[test]
fn test_syntax_error_unclosed_string() {
    let result = Scanner::new(r#"$x = "unclosed"#).scan_tokens();
    assert!(result.is_err());
}

#[test]
fn test_parse_error_unexpected_token() {
    let result = execute("$x = + 5");
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(matches!(
            e,
            Error::ParseError(_) | Error::UnexpectedToken { .. }
        ));
    }
}

#[test]
fn test_unexpected_eof() {
    let result = execute("IF true THEN");
    assert!(result.is_err());
}

#[test]
fn test_unclosed_parenthesis() {
    let result = execute("$x = (1 + 2");
    assert!(result.is_err());
}

#[test]
fn test_unclosed_bracket() {
    let result = execute("$x = [1, 2, 3");
    assert!(result.is_err());
}

#[test]
fn test_unclosed_brace() {
    let result = execute(r#"$x = {a: 1, b: 2"#);
    assert!(result.is_err());
}

// ====================
// Runtime Errors
// ====================

#[test]
fn test_undefined_variable() {
    let result = execute("RETURN $undefined");
    assert!(result.is_err());
    if let Err(Error::UndefinedVariable { name }) = result {
        assert_eq!(name, "undefined");
    } else {
        panic!("Expected UndefinedVariable error");
    }
}

#[test]
fn test_undefined_tool() {
    let result = execute("$x = NONEXISTENT_TOOL()");
    assert!(result.is_err());
    if let Err(Error::UndefinedTool { name }) = result {
        assert_eq!(name, "NONEXISTENT_TOOL");
    } else {
        panic!("Expected UndefinedTool error");
    }
}

#[test]
fn test_type_error_wrong_type() {
    let result = execute(
        r#"
        $str = "hello"
        $result = SUM($str)
        RETURN $result
    "#,
    );
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(matches!(e, Error::TypeError { .. }));
    }
}

#[test]
fn test_type_error_array_expected() {
    let result = execute(
        r#"
        $num = 42
        $result = FIRST($num)
        RETURN $result
    "#,
    );
    assert!(result.is_err());
}

#[test]
fn test_constant_reassignment() {
    let result = execute(
        r#"
        CONST PI = 3.14159
        $PI = 3.14
        RETURN $PI
    "#,
    );
    assert!(result.is_err());
    // Constant reassignment should fail at runtime
    if let Err(e) = result {
        // Either ConstantReassignment or parse error for trying to use $ with constant name
        assert!(matches!(
            e,
            Error::ConstantReassignment { .. }
                | Error::ParseError(_)
                | Error::UnexpectedToken { .. }
        ));
    }
}

#[test]
fn test_division_by_zero() {
    let result = execute("$x = 10 / 0");
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(matches!(e, Error::DivisionByZero));
    }
}

#[test]
fn test_index_out_of_bounds() {
    let result = execute(
        r#"
        $arr = [1, 2, 3]
        $x = $arr[10]
        RETURN $x
    "#,
    );
    assert!(result.is_err());
    if let Err(Error::IndexOutOfBounds { index, length }) = result {
        assert_eq!(index, 10);
        assert_eq!(length, 3);
    } else {
        panic!("Expected IndexOutOfBounds error");
    }
}

#[test]
fn test_negative_index() {
    let result = execute(
        r#"
        $arr = [1, 2, 3]
        $x = $arr[-1]
        RETURN $x
    "#,
    );
    // Negative indices should either wrap or error
    // Current implementation treats -1 as unsigned, causing overflow
    assert!(result.is_err());
}

#[test]
fn test_empty_collection_first() {
    let result = execute(
        r#"
        $empty = []
        $x = FIRST($empty)
        RETURN $x
    "#,
    );
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(matches!(e, Error::EmptyCollection { .. }));
    }
}

#[test]
fn test_empty_collection_last() {
    let result = execute(
        r#"
        $empty = []
        $x = LAST($empty)
        RETURN $x
    "#,
    );
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(matches!(e, Error::EmptyCollection { .. }));
    }
}

// ====================
// Tool Errors
// ====================

#[test]
fn test_tool_invalid_arguments_count() {
    let result = execute("$x = SUM()");
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(matches!(e, Error::InvalidArguments { .. }));
    }
}

#[test]
fn test_tool_invalid_argument_type() {
    let result = execute(
        r#"
        $str = "not a number"
        $x = SQRT($str)
        RETURN $x
    "#,
    );
    assert!(result.is_err());
}

#[test]
fn test_tool_not_implemented_map() {
    let result = execute(
        r#"
        $arr = [1, 2, 3]
        $doubled = MAP($arr, $x => $x * 2)
        RETURN $doubled
    "#,
    );
    // MAP requires lambda support which isn't implemented yet
    assert!(result.is_err());
}

#[test]
fn test_slice_invalid_range() {
    let result = execute(
        r#"
        $arr = [1, 2, 3, 4, 5]
        $x = SLICE($arr, 5, 3)
        RETURN $x
    "#,
    );
    assert!(result.is_err());
}

#[test]
fn test_slice_out_of_bounds() {
    let result = execute(
        r#"
        $arr = [1, 2, 3]
        $x = SLICE($arr, 0, 10)
        RETURN $x
    "#,
    );
    assert!(result.is_err());
}

#[test]
fn test_sqrt_negative_number() {
    let result = execute(
        r#"
        $x = SQRT(-4)
        RETURN $x
    "#,
    );
    // SQRT of negative should return NaN or error
    // Check what our implementation does
    match result {
        Ok(val) => {
            // If it succeeds, should be NaN
            if let ovsm::Value::Float(f) = val {
                assert!(f.is_nan(), "SQRT(-4) should be NaN or error");
            }
        }
        Err(_) => {
            // Error is also acceptable
        }
    }
}

// ====================
// Control Flow Errors
// ====================

#[test]
fn test_break_outside_loop() {
    let result = execute(
        r#"
        $x = 10
        BREAK
        RETURN $x
    "#,
    );
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(matches!(e, Error::InvalidBreak));
    }
}

#[test]
fn test_continue_outside_loop() {
    let result = execute(
        r#"
        $x = 10
        CONTINUE
        RETURN $x
    "#,
    );
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(matches!(e, Error::InvalidContinue));
    }
}

// ====================
// Type Coercion Errors
// ====================

#[test]
fn test_invalid_operation_string_plus_array() {
    let result = execute(
        r#"
        $str = "hello"
        $arr = [1, 2, 3]
        $x = $str + $arr
        RETURN $x
    "#,
    );
    // Should fail with InvalidOperation or TypeError
    assert!(result.is_err());
}

#[test]
fn test_invalid_comparison_incompatible_types() {
    let result = execute(
        r#"
        $x = "hello" < [1, 2, 3]
        RETURN $x
    "#,
    );
    // Comparing string to array should fail or return false
    // Check implementation behavior
    match result {
        Ok(val) => {
            // If comparison succeeds, ensure it's boolean
            assert!(matches!(val, ovsm::Value::Bool(_)));
        }
        Err(e) => {
            // Error is more correct
            assert!(matches!(e, Error::InvalidComparison { .. }));
        }
    }
}

#[test]
fn test_not_callable_error() {
    let result = execute(
        r#"
        $x = 42
        $y = $x()
        RETURN $y
    "#,
    );
    assert!(result.is_err());
}

// ====================
// Edge Cases
// ====================

#[test]
fn test_deeply_nested_expressions() {
    let result = execute(
        r#"
        $x = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10
        RETURN $x
    "#,
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), ovsm::Value::Int(55));
}

#[test]
fn test_many_parentheses() {
    let result = execute(
        r#"
        $x = ((((((((((1 + 1))))))))))
        RETURN $x
    "#,
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), ovsm::Value::Int(2));
}

#[test]
fn test_empty_array_operations() {
    let result = execute(
        r#"
        $empty = []
        $count = COUNT($empty)
        RETURN $count
    "#,
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), ovsm::Value::Int(0));
}

#[test]
fn test_null_arithmetic() {
    let result = execute("$x = null + 5");
    // null + number should fail or coerce
    // Check implementation
    match result {
        Ok(_) => {
            // If it succeeds, that's a design choice
        }
        Err(_) => {
            // Error is more strict and arguably better
        }
    }
}

#[test]
fn test_boolean_arithmetic() {
    let result = execute("$x = true + false");
    // Boolean arithmetic might work (1 + 0) or fail
    match result {
        Ok(val) => {
            // Some languages allow this
            assert!(matches!(val, ovsm::Value::Int(_) | ovsm::Value::Bool(_)));
        }
        Err(_) => {
            // Strict type checking would reject this
        }
    }
}

// ====================
// Complex Scenarios
// ====================

#[test]
fn test_nested_if_with_undefined_variable() {
    let result = execute(
        r#"
        IF true THEN
            IF false THEN
                $x = 1
            ELSE
                $y = $undefined


        RETURN $x
    "#,
    );
    assert!(result.is_err());
}

#[test]
fn test_loop_with_type_error_in_body() {
    let result = execute(
        r#"
        $sum = 0
        FOR $i IN [1, 2, "three", 4] DO:
            $sum = $sum + $i

        RETURN $sum
    "#,
    );
    // Adding string to int should fail
    assert!(result.is_err());
}

#[test]
fn test_tool_chain_with_error() {
    let result = execute(
        r#"
        $data = [1, 2, 3, 4, 5]
        $sorted = SORT($data)
        $top = TOP_N($sorted, 100)
        $sum = SUM($top)
        RETURN $sum
    "#,
    );
    // TOP_N requesting 100 items from 5-item array should work (returns all 5)
    assert!(result.is_ok());
}

#[test]
fn test_object_missing_field() {
    let result = execute(
        r#"
        $obj = {name: "Alice", age: 30}
        $email = $obj.email
        RETURN $email
    "#,
    );
    // Accessing missing field should error
    assert!(result.is_err());
}

#[test]
fn test_string_indexing_out_of_bounds() {
    let result = execute(
        r#"
        $str = "hello"
        $ch = $str[10]
        RETURN $ch
    "#,
    );
    assert!(result.is_err());
}

// ====================
// Unimplemented Features (Must Error)
// ====================

#[test]
fn test_try_catch_works() {
    let result = execute(
        r#"
        TRY:
            $x = 10 / 0
        CATCH:
            $x = 0
        RETURN $x
    "#,
    );
    // TRY-CATCH is now implemented!
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), ovsm::Value::Int(0));
}

#[test]
fn test_parallel_not_implemented() {
    let result = execute(
        r#"
        PARALLEL:
            $task1 = 1 + 1
            $task2 = 2 + 2
        RETURN $task1
    "#,
    );
    assert!(result.is_err());
    // Parser may not support PARALLEL syntax yet, so accept parse errors too
    if let Err(e) = result {
        assert!(
            matches!(
                e,
                Error::NotImplemented { .. } | Error::ParseError(_) | Error::UnexpectedToken { .. }
            ),
            "Expected NotImplemented or parse error for PARALLEL, got: {:?}",
            e
        );
    }
}

#[test]
fn test_guard_not_implemented() {
    let result = execute(
        r#"
        $x = 10
        GUARD $x > 0 ELSE:
            RETURN -1
        RETURN $x
    "#,
    );
    assert!(result.is_err());
    // Parser may not support GUARD syntax yet, so accept parse errors too
    if let Err(e) = result {
        assert!(
            matches!(
                e,
                Error::NotImplemented { .. } | Error::ParseError(_) | Error::UnexpectedToken { .. }
            ),
            "Expected NotImplemented or parse error for GUARD, got: {:?}",
            e
        );
    }
}

#[test]
fn test_decision_not_implemented() {
    let result = execute(
        r#"
        DECISION "Choose deployment strategy":
            BRANCH "aggressive":
                $speed = "fast"
            BRANCH "conservative":
                $speed = "slow"
        RETURN $speed
    "#,
    );
    assert!(result.is_err());
    // Parser may not support DECISION syntax yet, so accept parse errors too
    if let Err(e) = result {
        assert!(
            matches!(
                e,
                Error::NotImplemented { .. } | Error::ParseError(_) | Error::UnexpectedToken { .. }
            ),
            "Expected NotImplemented or parse error for DECISION, got: {:?}",
            e
        );
    }
}

#[test]
fn test_wait_strategy_not_implemented() {
    let result = execute(
        r#"
        PARALLEL:
            $task1 = 1 + 1
            $task2 = 2 + 2
        WAIT_ALL
        RETURN $task1
    "#,
    );
    assert!(result.is_err());
    // This test might fail at PARALLEL first, which is fine
    // The goal is to ensure WAIT strategies also error
}
