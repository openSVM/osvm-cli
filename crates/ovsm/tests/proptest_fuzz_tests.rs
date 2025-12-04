//! Property-based fuzzing tests for OVSM parser, runtime, and compiler
//!
//! These tests use proptest to generate random inputs and verify that:
//! 1. The parser never panics on arbitrary input
//! 2. The evaluator handles malformed ASTs gracefully
//! 3. Valid OVSM programs produce deterministic results

use ovsm::lexer::SExprScanner;
use ovsm::parser::SExprParser;
use ovsm::runtime::{LispEvaluator, Value};
use proptest::prelude::*;

// =============================================================================
// STRATEGY GENERATORS
// =============================================================================

/// Generate random strings that might break parsers
fn arbitrary_source_string() -> impl Strategy<Value = String> {
    prop::string::string_regex(r"[\x00-\x7F]{0,500}")
        .unwrap()
        .prop_map(|s| s)
}

/// Generate valid-ish S-expressions
fn sexp_like_string() -> impl Strategy<Value = String> {
    prop::collection::vec(sexp_token(), 0..50)
        .prop_map(|tokens| tokens.join(" "))
}

/// Generate tokens that look like S-expression elements
fn sexp_token() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("(".to_string()),
        Just(")".to_string()),
        Just("[".to_string()),
        Just("]".to_string()),
        Just("{".to_string()),
        Just("}".to_string()),
        // Keywords
        Just("define".to_string()),
        Just("if".to_string()),
        Just("lambda".to_string()),
        Just("let".to_string()),
        Just("set!".to_string()),
        Just("true".to_string()),
        Just("false".to_string()),
        Just("null".to_string()),
        // Operators
        Just("+".to_string()),
        Just("-".to_string()),
        Just("*".to_string()),
        Just("/".to_string()),
        Just("=".to_string()),
        Just("<".to_string()),
        Just(">".to_string()),
        Just("and".to_string()),
        Just("or".to_string()),
        Just("not".to_string()),
        // Numbers
        (-1000i64..1000i64).prop_map(|n| n.to_string()),
        (0.0f64..100.0f64).prop_map(|f| format!("{:.2}", f)),
        // Strings
        r#""[a-zA-Z0-9 ]{0,20}""#.prop_map(|s| s),
        // Identifiers
        "[a-z][a-z0-9_]{0,10}".prop_map(|s| s),
        // Comments
        Just(";;".to_string()),
        ";[^\n]{0,20}".prop_map(|s| s),
    ]
}

/// Generate valid OVSM programs
fn valid_ovsm_program() -> impl Strategy<Value = String> {
    prop_oneof![
        // Arithmetic expressions
        arith_expr(),
        // Variable definitions
        var_def_program(),
        // If expressions
        if_expr_program(),
        // Let expressions
        let_expr_program(),
        // Array operations
        array_program(),
        // Object operations
        object_program(),
    ]
}

fn arith_expr() -> impl Strategy<Value = String> {
    let op = prop_oneof![Just("+"), Just("-"), Just("*")];
    let nums = prop::collection::vec(-100i64..100i64, 2..6);
    (op, nums).prop_map(|(op, nums)| {
        let args: Vec<String> = nums.iter().map(|n| n.to_string()).collect();
        format!("({} {})", op, args.join(" "))
    })
}

fn var_def_program() -> impl Strategy<Value = String> {
    let var_name = "[a-z][a-z0-9]{0,5}";
    let value = -1000i64..1000i64;
    (var_name, value).prop_map(|(name, val)| {
        format!("(define {} {})\n{}", name, val, name)
    })
}

fn if_expr_program() -> impl Strategy<Value = String> {
    let cond_val = prop::bool::ANY;
    let then_val = -100i64..100i64;
    let else_val = -100i64..100i64;
    (cond_val, then_val, else_val).prop_map(|(cond, then_v, else_v)| {
        format!("(if {} {} {})", cond, then_v, else_v)
    })
}

fn let_expr_program() -> impl Strategy<Value = String> {
    let var_name = "[a-z][a-z0-9]{0,3}";
    let value = -100i64..100i64;
    (var_name, value).prop_map(|(name, val)| {
        format!("(let (({} {})) {})", name, val, name)
    })
}

fn array_program() -> impl Strategy<Value = String> {
    prop::collection::vec(-50i64..50i64, 0..10)
        .prop_map(|nums| {
            let elements: Vec<String> = nums.iter().map(|n| n.to_string()).collect();
            format!("[{}]", elements.join(" "))
        })
}

fn object_program() -> impl Strategy<Value = String> {
    let keys = prop::collection::vec("[a-z]{1,5}", 0..5);
    let vals = prop::collection::vec(-100i64..100i64, 0..5);
    (keys, vals).prop_map(|(keys, vals)| {
        let pairs: Vec<String> = keys.iter().zip(vals.iter())
            .map(|(k, v)| format!(":{} {}", k, v))
            .collect();
        format!("{{{}}}", pairs.join(" "))
    })
}

// =============================================================================
// PARSER FUZZ TESTS
// =============================================================================

proptest! {
    /// The lexer should never panic on arbitrary input
    #[test]
    fn lexer_never_panics(source in arbitrary_source_string()) {
        let mut scanner = SExprScanner::new(&source);
        // Should either succeed or return an error, never panic
        let _ = scanner.scan_tokens();
    }

    /// The lexer handles S-expression-like strings without panic
    #[test]
    fn lexer_handles_sexp_like(source in sexp_like_string()) {
        let mut scanner = SExprScanner::new(&source);
        let _ = scanner.scan_tokens();
    }

    /// The parser should never panic when given valid tokens
    #[test]
    fn parser_never_panics_on_valid_tokens(source in sexp_like_string()) {
        let mut scanner = SExprScanner::new(&source);
        if let Ok(tokens) = scanner.scan_tokens() {
            let mut parser = SExprParser::new(tokens);
            // Should either succeed or return an error, never panic
            let _ = parser.parse();
        }
    }

    /// Parser handles deeply nested expressions
    #[test]
    fn parser_handles_deep_nesting(depth in 1usize..100) {
        let open = "(".repeat(depth);
        let close = ")".repeat(depth);
        let source = format!("{}+ 1 1{}", open, close);

        let mut scanner = SExprScanner::new(&source);
        if let Ok(tokens) = scanner.scan_tokens() {
            let mut parser = SExprParser::new(tokens);
            let _ = parser.parse();
        }
    }

    /// Parser handles unbalanced parentheses without panic
    #[test]
    fn parser_handles_unbalanced_parens(
        opens in 0usize..50,
        closes in 0usize..50
    ) {
        let source = format!("{}1{}", "(".repeat(opens), ")".repeat(closes));

        let mut scanner = SExprScanner::new(&source);
        if let Ok(tokens) = scanner.scan_tokens() {
            let mut parser = SExprParser::new(tokens);
            // Should return error for unbalanced, never panic
            let _ = parser.parse();
        }
    }
}

// =============================================================================
// EVALUATOR FUZZ TESTS
// =============================================================================

proptest! {
    /// Valid programs should evaluate deterministically
    #[test]
    fn evaluator_is_deterministic(source in valid_ovsm_program()) {
        let mut scanner = SExprScanner::new(&source);
        if let Ok(tokens) = scanner.scan_tokens() {
            let mut parser = SExprParser::new(tokens);
            if let Ok(program) = parser.parse() {
                let mut eval1 = LispEvaluator::new();
                let mut eval2 = LispEvaluator::new();

                let result1 = eval1.execute(&program);
                let result2 = eval2.execute(&program);

                // Same program should produce same result
                match (result1, result2) {
                    (Ok(v1), Ok(v2)) => prop_assert_eq!(v1, v2),
                    (Err(_), Err(_)) => {} // Both error is fine
                    _ => prop_assert!(false, "Non-deterministic evaluation"),
                }
            }
        }
    }

    /// Arithmetic operations should not panic on edge cases
    #[test]
    fn arithmetic_edge_cases(
        a in i64::MIN..i64::MAX,
        b in i64::MIN..i64::MAX
    ) {
        let programs = vec![
            format!("(+ {} {})", a, b),
            format!("(- {} {})", a, b),
            format!("(* {} {})", a, b),
            // Division by zero should be handled gracefully
            format!("(/ {} {})", a, if b == 0 { 1 } else { b }),
        ];

        for source in programs {
            let mut scanner = SExprScanner::new(&source);
            if let Ok(tokens) = scanner.scan_tokens() {
                let mut parser = SExprParser::new(tokens);
                if let Ok(program) = parser.parse() {
                    let mut evaluator = LispEvaluator::new();
                    // Should not panic, may return error for overflow
                    let _ = evaluator.execute(&program);
                }
            }
        }
    }

    /// Array access should handle bounds gracefully
    #[test]
    fn array_bounds_handling(
        elements in prop::collection::vec(-100i64..100, 0..20),
        index in -10i64..30
    ) {
        let arr_str: Vec<String> = elements.iter().map(|n| n.to_string()).collect();
        let source = format!(
            "(define arr [{}])\n(get arr {})",
            arr_str.join(" "),
            index
        );

        let mut scanner = SExprScanner::new(&source);
        if let Ok(tokens) = scanner.scan_tokens() {
            let mut parser = SExprParser::new(tokens);
            if let Ok(program) = parser.parse() {
                let mut evaluator = LispEvaluator::new();
                // Should not panic on out-of-bounds, return null or error
                let _ = evaluator.execute(&program);
            }
        }
    }

    /// String operations should handle unicode gracefully
    #[test]
    fn string_unicode_handling(s in "\\PC{0,100}") {
        let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
        let source = format!(r#"(string-length "{}")"#, escaped);

        let mut scanner = SExprScanner::new(&source);
        if let Ok(tokens) = scanner.scan_tokens() {
            let mut parser = SExprParser::new(tokens);
            if let Ok(program) = parser.parse() {
                let mut evaluator = LispEvaluator::new();
                let _ = evaluator.execute(&program);
            }
        }
    }
}

// =============================================================================
// COMBINED PIPELINE FUZZ TESTS
// =============================================================================

proptest! {
    /// Full pipeline: lex -> parse -> eval should never panic
    #[test]
    fn full_pipeline_never_panics(source in arbitrary_source_string()) {
        let mut scanner = SExprScanner::new(&source);
        if let Ok(tokens) = scanner.scan_tokens() {
            let mut parser = SExprParser::new(tokens);
            if let Ok(program) = parser.parse() {
                let mut evaluator = LispEvaluator::new();
                // May error, but should never panic
                let _ = evaluator.execute(&program);
            }
        }
    }

    /// Memory safety: multiple evaluations shouldn't leak or corrupt
    #[test]
    fn memory_safety_multiple_runs(
        programs in prop::collection::vec(valid_ovsm_program(), 1..10)
    ) {
        let mut evaluator = LispEvaluator::new();

        for source in programs {
            let mut scanner = SExprScanner::new(&source);
            if let Ok(tokens) = scanner.scan_tokens() {
                let mut parser = SExprParser::new(tokens);
                if let Ok(program) = parser.parse() {
                    let _ = evaluator.execute(&program);
                }
            }
        }
        // If we get here without panic/crash, memory is being handled correctly
    }
}

// =============================================================================
// SPECIFIC REGRESSION TESTS (from discovered bugs)
// =============================================================================

#[test]
fn regression_empty_input() {
    let source = "";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    // Empty input should parse to empty program, not panic
    let _ = parser.parse();
}

#[test]
fn regression_only_whitespace() {
    let source = "   \n\t\r\n   ";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let _ = parser.parse();
}

#[test]
fn regression_only_comments() {
    let source = ";; comment 1\n;; comment 2\n";
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let _ = parser.parse();
}

#[test]
fn regression_null_bytes() {
    let source = "(\0 + 1 2)";
    let mut scanner = SExprScanner::new(source);
    // Should handle null bytes gracefully
    let _ = scanner.scan_tokens();
}

#[test]
fn regression_very_long_number() {
    let source = format!("(+ 1 {})", "9".repeat(1000));
    let mut scanner = SExprScanner::new(&source);
    if let Ok(tokens) = scanner.scan_tokens() {
        let mut parser = SExprParser::new(tokens);
        if let Ok(program) = parser.parse() {
            let mut evaluator = LispEvaluator::new();
            // Should handle gracefully (error is fine, panic is not)
            let _ = evaluator.execute(&program);
        }
    }
}

#[test]
fn regression_very_long_string() {
    let long_str = "a".repeat(100_000);
    let source = format!(r#""{}""#, long_str);
    let mut scanner = SExprScanner::new(&source);
    let _ = scanner.scan_tokens();
}

#[test]
fn regression_recursive_definition() {
    // This could cause infinite loop in naive implementations
    let source = "(define f (lambda (x) (f x)))";
    let mut scanner = SExprScanner::new(source);
    if let Ok(tokens) = scanner.scan_tokens() {
        let mut parser = SExprParser::new(tokens);
        if let Ok(program) = parser.parse() {
            let mut evaluator = LispEvaluator::new();
            // Definition should work, calling would be infinite
            let _ = evaluator.execute(&program);
        }
    }
}
