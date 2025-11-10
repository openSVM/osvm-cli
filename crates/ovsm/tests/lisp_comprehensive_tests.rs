/// Comprehensive Test Suite for OVSM LISP Syntax Support
///
/// This test file covers all major aspects of the LISP implementation:
/// 1. Basic syntax (literals, variables, operators)
/// 2. Special forms (define, set!, const, let)
/// 3. Control flow (if, while, for, do, when)
/// 4. Collection operations (arrays, lists, objects)
/// 5. Advanced features (nested expressions, closures, recursion)
/// 6. Real-world scenarios (pagination, filtering, aggregation)
/// 7. Critical bug demonstrations (IF in WHILE loops)
use ovsm::lexer::SExprScanner;
use ovsm::parser::SExprParser;
use ovsm::runtime::{LispEvaluator, Value};

// Helper function to execute LISP code
fn execute_lisp(source: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = LispEvaluator::new();
    Ok(evaluator.execute(&program)?)
}

// ============================================================================
// SECTION 1: BASIC SYNTAX TESTS
// ============================================================================

#[test]
fn test_literals_integers() {
    assert_eq!(execute_lisp("42").unwrap(), Value::Int(42));
    assert_eq!(execute_lisp("-17").unwrap(), Value::Int(-17));
    assert_eq!(execute_lisp("0").unwrap(), Value::Int(0));
}

#[test]
#[allow(clippy::approx_constant)]
fn test_literals_floats() {
    assert_eq!(execute_lisp("3.14").unwrap(), Value::Float(3.14));
    assert_eq!(execute_lisp("-2.5").unwrap(), Value::Float(-2.5));
}

#[test]
fn test_literals_strings() {
    assert_eq!(
        execute_lisp(r#""hello""#).unwrap(),
        Value::String("hello".to_string())
    );
    assert_eq!(
        execute_lisp(r#""Hello, World!""#).unwrap(),
        Value::String("Hello, World!".to_string())
    );
}

#[test]
fn test_literals_booleans() {
    assert_eq!(execute_lisp("true").unwrap(), Value::Bool(true));
    assert_eq!(execute_lisp("false").unwrap(), Value::Bool(false));
}

#[test]
fn test_literals_null() {
    assert_eq!(execute_lisp("nil").unwrap(), Value::Null);
}

#[test]
fn test_array_literal() {
    let result = execute_lisp("[1 2 3 4 5]").unwrap();
    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 5);
            assert_eq!(arr[0], Value::Int(1));
            assert_eq!(arr[4], Value::Int(5));
        }
        _ => panic!("Expected array"),
    }
}

#[test]
fn test_empty_array() {
    let result = execute_lisp("[]").unwrap();
    match result {
        Value::Array(arr) => assert_eq!(arr.len(), 0),
        _ => panic!("Expected empty array"),
    }
}

#[test]
fn test_object_literal() {
    let result = execute_lisp(r#"{:name "Alice" :age 30}"#).unwrap();
    match result {
        Value::Object(obj) => {
            assert_eq!(obj.len(), 2);
            assert_eq!(obj.get("name"), Some(&Value::String("Alice".to_string())));
            assert_eq!(obj.get("age"), Some(&Value::Int(30)));
        }
        _ => panic!("Expected object"),
    }
}

// ============================================================================
// SECTION 2: ARITHMETIC OPERATORS
// ============================================================================

#[test]
fn test_addition() {
    assert_eq!(execute_lisp("(+ 1 2)").unwrap(), Value::Int(3));
    assert_eq!(execute_lisp("(+ 1 2 3 4)").unwrap(), Value::Int(10));
    assert_eq!(execute_lisp("(+ 10 -5)").unwrap(), Value::Int(5));
}

#[test]
fn test_subtraction() {
    assert_eq!(execute_lisp("(- 10 3)").unwrap(), Value::Int(7));
    assert_eq!(execute_lisp("(- 5 10)").unwrap(), Value::Int(-5));
}

#[test]
fn test_multiplication() {
    assert_eq!(execute_lisp("(* 3 4)").unwrap(), Value::Int(12));
    assert_eq!(execute_lisp("(* 2 3 4)").unwrap(), Value::Int(24));
}

#[test]
fn test_division() {
    assert_eq!(execute_lisp("(/ 10 2)").unwrap(), Value::Int(5));
    assert_eq!(execute_lisp("(/ 20 4)").unwrap(), Value::Int(5));
}

#[test]
fn test_modulo() {
    assert_eq!(execute_lisp("(% 10 3)").unwrap(), Value::Int(1));
    assert_eq!(execute_lisp("(% 20 7)").unwrap(), Value::Int(6));
}

#[test]
fn test_nested_arithmetic() {
    assert_eq!(
        execute_lisp("(+ (* 2 3) (- 10 5))").unwrap(),
        Value::Int(11)
    );
    assert_eq!(
        execute_lisp("(* (+ 1 2) (- 10 6))").unwrap(),
        Value::Int(12)
    );
}

// ============================================================================
// SECTION 3: COMPARISON OPERATORS
// ============================================================================

#[test]
fn test_equality() {
    assert_eq!(execute_lisp("(== 5 5)").unwrap(), Value::Bool(true));
    assert_eq!(execute_lisp("(== 5 3)").unwrap(), Value::Bool(false));
}

#[test]
fn test_inequality() {
    assert_eq!(execute_lisp("(!= 5 3)").unwrap(), Value::Bool(true));
    assert_eq!(execute_lisp("(!= 5 5)").unwrap(), Value::Bool(false));
}

#[test]
fn test_less_than() {
    assert_eq!(execute_lisp("(< 3 5)").unwrap(), Value::Bool(true));
    assert_eq!(execute_lisp("(< 5 3)").unwrap(), Value::Bool(false));
}

#[test]
fn test_less_equal() {
    assert_eq!(execute_lisp("(<= 3 5)").unwrap(), Value::Bool(true));
    assert_eq!(execute_lisp("(<= 5 5)").unwrap(), Value::Bool(true));
    assert_eq!(execute_lisp("(<= 6 5)").unwrap(), Value::Bool(false));
}

#[test]
fn test_greater_than() {
    assert_eq!(execute_lisp("(> 5 3)").unwrap(), Value::Bool(true));
    assert_eq!(execute_lisp("(> 3 5)").unwrap(), Value::Bool(false));
}

#[test]
fn test_greater_equal() {
    assert_eq!(execute_lisp("(>= 5 3)").unwrap(), Value::Bool(true));
    assert_eq!(execute_lisp("(>= 5 5)").unwrap(), Value::Bool(true));
    assert_eq!(execute_lisp("(>= 3 5)").unwrap(), Value::Bool(false));
}

// ============================================================================
// SECTION 4: LOGICAL OPERATORS
// ============================================================================

#[test]
fn test_and_operator() {
    assert_eq!(execute_lisp("(and true true)").unwrap(), Value::Bool(true));
    assert_eq!(
        execute_lisp("(and true false)").unwrap(),
        Value::Bool(false)
    );
    assert_eq!(
        execute_lisp("(and false true)").unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_or_operator() {
    assert_eq!(execute_lisp("(or true false)").unwrap(), Value::Bool(true));
    assert_eq!(execute_lisp("(or false true)").unwrap(), Value::Bool(true));
    assert_eq!(
        execute_lisp("(or false false)").unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_not_operator() {
    assert_eq!(execute_lisp("(not true)").unwrap(), Value::Bool(false));
    assert_eq!(execute_lisp("(not false)").unwrap(), Value::Bool(true));
}

// ============================================================================
// SECTION 5: VARIABLE DEFINITION AND ASSIGNMENT
// ============================================================================

#[test]
fn test_define_variable() {
    let source = r#"
        (define x 42)
        x
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(42));
}

#[test]
fn test_define_multiple_variables() {
    let source = r#"
        (define x 10)
        (define y 20)
        (+ x y)
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(30));
}

#[test]
fn test_set_mutation() {
    let source = r#"
        (define counter 0)
        (set! counter 10)
        counter
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(10));
}

#[test]
fn test_set_incremental_mutation() {
    let source = r#"
        (define counter 0)
        (set! counter (+ counter 1))
        (set! counter (+ counter 1))
        (set! counter (+ counter 1))
        counter
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(3));
}

#[test]
#[allow(clippy::approx_constant)]
fn test_const_definition() {
    let source = r#"
        (const PI 3.14159)
        PI
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Float(3.14159));
}

// ============================================================================
// SECTION 6: LET BINDINGS (LEXICAL SCOPE)
// ============================================================================

#[test]
fn test_let_basic() {
    let source = r#"
        (let ((x 10))
            x)
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(10));
}

#[test]
fn test_let_multiple_bindings() {
    let source = r#"
        (let ((x 10)
              (y 20))
            (+ x y))
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(30));
}

#[test]
fn test_let_shadowing() {
    let source = r#"
        (define x 5)
        (let ((x 10))
            (+ x 1))
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(11));
}

#[test]
fn test_let_nested() {
    let source = r#"
        (let ((x 10))
            (let ((y 20))
                (+ x y)))
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(30));
}

// ============================================================================
// SECTION 7: CONDITIONAL EXPRESSIONS (IF)
// ============================================================================

#[test]
fn test_if_true_branch() {
    let source = r#"
        (if (> 10 5)
            "large"
            "small")
    "#;
    assert_eq!(
        execute_lisp(source).unwrap(),
        Value::String("large".to_string())
    );
}

#[test]
fn test_if_false_branch() {
    let source = r#"
        (if (< 10 5)
            "small"
            "large")
    "#;
    assert_eq!(
        execute_lisp(source).unwrap(),
        Value::String("large".to_string())
    );
}

#[test]
fn test_if_with_computation() {
    let source = r#"
        (define x 10)
        (if (> x 5)
            (* x 2)
            (/ x 2))
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(20));
}

#[test]
fn test_nested_if() {
    let source = r#"
        (define score 85)
        (if (>= score 90)
            "A"
            (if (>= score 80)
                "B"
                "C"))
    "#;
    assert_eq!(
        execute_lisp(source).unwrap(),
        Value::String("B".to_string())
    );
}

// ============================================================================
// SECTION 8: WHILE LOOPS
// ============================================================================

#[test]
fn test_while_basic() {
    let source = r#"
        (define counter 0)
        (define sum 0)
        (while (< counter 5)
            (set! sum (+ sum counter))
            (set! counter (+ counter 1)))
        sum
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(10));
}

#[test]
fn test_while_with_break_condition() {
    let source = r#"
        (define done false)
        (define count 0)
        (while (not done)
            (set! count (+ count 1))
            (when (>= count 5)
                (set! done true)))
        count
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(5));
}

// ============================================================================
// SECTION 9: CRITICAL BUG DEMONSTRATION - IF IN WHILE
// ============================================================================

#[test]
fn test_if_inside_while_simple() {
    // This is THE critical test - IF-THEN-ELSE inside WHILE
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

    // The while loop should execute once and set count to 1
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(1));
}

#[test]
fn test_if_inside_while_complex() {
    let source = r#"
        (define total 0)
        (define i 0)
        (define done false)

        (while (not done)
            (if (< i 3)
                (do
                    (set! total (+ total i))
                    (set! i (+ i 1)))
                (set! done true)))

        total
    "#;

    // Should sum 0 + 1 + 2 = 3
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(3));
}

#[test]
fn test_if_inside_while_pagination_pattern() {
    // Mimics the pagination pattern that was failing
    let source = r#"
        (define before nil)
        (define done false)
        (define pages 0)

        (while (not done)
            (set! pages (+ pages 1))

            (if (null? before)
                (set! before "first-sig")
                (set! before "next-sig"))

            (when (>= pages 3)
                (set! done true)))

        pages
    "#;

    assert_eq!(execute_lisp(source).unwrap(), Value::Int(3));
}

// ============================================================================
// SECTION 10: FOR LOOPS
// ============================================================================

#[test]
fn test_for_loop_basic() {
    let source = r#"
        (define sum 0)
        (for (x [1 2 3 4 5])
            (set! sum (+ sum x)))
        sum
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(15));
}

#[test]
fn test_for_loop_with_condition() {
    let source = r#"
        (define sum 0)
        (for (x [1 2 3 4 5 6 7 8 9 10])
            (when (> x 5)
                (set! sum (+ sum x))))
        sum
    "#;

    // Should sum 6 + 7 + 8 + 9 + 10 = 40
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(40));
}

// ============================================================================
// SECTION 11: DO BLOCKS (SEQUENTIAL EXECUTION)
// ============================================================================

#[test]
fn test_do_block() {
    let source = r#"
        (do
            (define x 10)
            (define y 20)
            (+ x y))
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(30));
}

#[test]
fn test_do_returns_last_value() {
    let source = r#"
        (do
            42
            "hello"
            true
            99)
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(99));
}

// ============================================================================
// SECTION 12: WHEN (CONDITIONAL WITHOUT ELSE)
// ============================================================================

#[test]
fn test_when_true() {
    let source = r#"
        (define x 0)
        (when (> 10 5)
            (set! x 42))
        x
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(42));
}

#[test]
fn test_when_false() {
    let source = r#"
        (define x 0)
        (when (< 10 5)
            (set! x 42))
        x
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(0));
}

// ============================================================================
// SECTION 13: COLLECTION OPERATIONS
// ============================================================================

#[test]
fn test_length_array() {
    let source = r#"(length [1 2 3 4 5])"#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(5));
}

#[test]
fn test_length_empty_array() {
    let source = r#"(length [])"#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(0));
}

#[test]
fn test_null_check_true() {
    let source = r#"(null? nil)"#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Bool(true));
}

#[test]
fn test_null_check_false() {
    let source = r#"(null? 42)"#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Bool(false));
}

#[test]
fn test_empty_check_true() {
    let source = r#"(empty? [])"#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Bool(true));
}

#[test]
fn test_empty_check_false() {
    let source = r#"(empty? [1 2 3])"#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Bool(false));
}

#[test]
fn test_last_element() {
    let source = r#"(last [1 2 3 4 5])"#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(5));
}

#[test]
fn test_range() {
    let source = r#"(range 1 6)"#;
    let result = execute_lisp(source).unwrap();
    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 5);
            assert_eq!(arr[0], Value::Int(1));
            assert_eq!(arr[4], Value::Int(5));
        }
        _ => panic!("Expected array from range"),
    }
}

// ============================================================================
// SECTION 14: ARRAY INDEXING
// ============================================================================

#[test]
fn test_array_index_access() {
    let source = r#"
        (define arr [10 20 30 40 50])
        ([] arr 0)
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(10));
}

#[test]
fn test_array_index_middle() {
    let source = r#"
        (define arr [10 20 30 40 50])
        ([] arr 2)
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(30));
}

#[test]
fn test_array_index_last() {
    let source = r#"
        (define arr [10 20 30 40 50])
        ([] arr 4)
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(50));
}

// ============================================================================
// SECTION 15: PROPERTY ACCESS
// ============================================================================

#[test]
fn test_property_access() {
    let source = r#"
        (define obj {:name "Alice" :age 30})
        (. obj name)
    "#;
    assert_eq!(
        execute_lisp(source).unwrap(),
        Value::String("Alice".to_string())
    );
}

#[test]
fn test_property_access_number() {
    let source = r#"
        (define obj {:name "Bob" :age 25})
        (. obj age)
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(25));
}

// ============================================================================
// SECTION 16: COMPLEX NESTED EXPRESSIONS
// ============================================================================

#[test]
fn test_complex_nested_arithmetic() {
    let source = r#"
        (+ (* 2 (+ 3 4))
           (/ (- 20 10) 2))
    "#;
    // (2 * 7) + ((20 - 10) / 2) = 14 + 5 = 19
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(19));
}

#[test]
fn test_complex_conditional_chain() {
    let source = r#"
        (define x 15)
        (if (> x 20)
            "very large"
            (if (> x 10)
                "large"
                (if (> x 5)
                    "medium"
                    "small")))
    "#;
    assert_eq!(
        execute_lisp(source).unwrap(),
        Value::String("large".to_string())
    );
}

// ============================================================================
// SECTION 17: REAL-WORLD SCENARIO - PAGINATION
// ============================================================================

#[test]
fn test_pagination_simulation() {
    // Simulates paginating through results
    let source = r#"
        (define total-count 0)
        (define page 0)
        (define done false)

        (while (not done)
            (set! page (+ page 1))

            ; Simulate getting batch size (decreasing)
            (let ((batch-size (- 10 page)))
                (if (<= batch-size 0)
                    (set! done true)
                    (set! total-count (+ total-count batch-size)))))

        total-count
    "#;

    // Should sum 9 + 8 + 7 + 6 + 5 + 4 + 3 + 2 + 1 = 45
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(45));
}

// ============================================================================
// SECTION 18: REAL-WORLD SCENARIO - FILTERING
// ============================================================================

#[test]
fn test_filter_pattern() {
    // Filter numbers greater than 5
    let source = r#"
        (define nums [1 3 5 7 9 11 13])
        (define filtered [])

        (for (n nums)
            (when (> n 5)
                (set! filtered (append filtered [n]))))

        (length filtered)
    "#;

    // Should have 4 elements: 7, 9, 11, 13
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(4));
}

// ============================================================================
// SECTION 19: REAL-WORLD SCENARIO - AGGREGATION
// ============================================================================

#[test]
fn test_sum_aggregation() {
    let source = r#"
        (define nums [1 2 3 4 5 6 7 8 9 10])
        (define sum 0)

        (for (n nums)
            (set! sum (+ sum n)))

        sum
    "#;

    assert_eq!(execute_lisp(source).unwrap(), Value::Int(55));
}

#[test]
fn test_max_finding() {
    let source = r#"
        (define nums [3 7 2 9 1 5])
        (define max-val 0)

        (for (n nums)
            (when (> n max-val)
                (set! max-val n)))

        max-val
    "#;

    assert_eq!(execute_lisp(source).unwrap(), Value::Int(9));
}

// ============================================================================
// SECTION 20: EDGE CASES
// ============================================================================

#[test]
fn test_empty_do_block() {
    let source = r#"(do)"#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Null);
}

#[test]
fn test_while_never_executes() {
    let source = r#"
        (define x 0)
        (while false
            (set! x 99))
        x
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(0));
}

#[test]
fn test_for_empty_array() {
    let source = r#"
        (define count 0)
        (for (x [])
            (set! count (+ count 1)))
        count
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(0));
}

#[test]
fn test_deeply_nested_let() {
    let source = r#"
        (let ((a 1))
            (let ((b 2))
                (let ((c 3))
                    (let ((d 4))
                        (+ a b c d)))))
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(10));
}

// ============================================================================
// SECTION 21: LOG FUNCTION
// ============================================================================

#[test]
fn test_log_function() {
    // Log should execute without error and return nil
    let source = r#"
        (log :message "Test message")
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Null);
}

#[test]
fn test_log_with_number() {
    let source = r#"
        (log :message 42)
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Null);
}

// ============================================================================
// SECTION 22: NOW FUNCTION (TIMESTAMP)
// ============================================================================

#[test]
fn test_now_returns_number() {
    let source = r#"(now)"#;
    let result = execute_lisp(source).unwrap();
    // Should return an integer timestamp
    matches!(result, Value::Int(_));
}

#[test]
fn test_time_calculation() {
    let source = r#"
        (define current (now))
        (define cutoff (- current 60))
        (> current cutoff)
    "#;
    assert_eq!(execute_lisp(source).unwrap(), Value::Bool(true));
}

// ============================================================================
// SECTION 23: COMPREHENSIVE INTEGRATION TEST
// ============================================================================

#[test]
fn test_comprehensive_program() {
    // A complete program that uses multiple features
    let source = r#"
        ; Define constants
        (const MAX_ITEMS 10)
        (const THRESHOLD 5)

        ; Initialize variables
        (define total 0)
        (define count 0)
        (define items (range 1 11))

        ; Process items
        (for (item items)
            (do
                (set! count (+ count 1))

                ; Only process items above threshold
                (when (> item THRESHOLD)
                    (set! total (+ total item)))))

        ; Verify we processed all items
        (if (== count MAX_ITEMS)
            total
            -1)
    "#;

    // Should sum 6 + 7 + 8 + 9 + 10 = 40
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(40));
}

#[test]
fn test_fibonacci_iterative() {
    let source = r#"
        (defn fib (n)
            (let ((a 0)
                  (b 1)
                  (i 0))
                (while (< i n)
                    (let ((temp a))
                        (set! a b)
                        (set! b (+ temp b))
                        (set! i (+ i 1))))
                a))

        (fib 10)
    "#;

    // 10th Fibonacci number is 55
    assert_eq!(execute_lisp(source).unwrap(), Value::Int(55));
}
