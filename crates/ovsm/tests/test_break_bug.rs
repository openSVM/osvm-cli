//! Test for BREAK bug discovered during QA runner self-assessment
//!
//! NOTE: These tests use the obsolete indentation-based Python-like syntax
//! that has been removed in favor of LISP/S-expression syntax.
//! They are disabled until converted to LISP syntax.

#![cfg(feature = "incomplete_tests")]

use ovsm::{Evaluator, Parser, Scanner, Value};

fn execute(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    Ok(evaluator.execute(&program)?)
}

#[test]
fn test_for_loop_works_without_break() {
    let code = r#"
$sum = 0
FOR $i IN [1..5]:
    $sum = $sum + $i
RETURN $sum
"#;
    let result = execute(code.trim()).unwrap();

    assert_eq!(result, Value::Int(10));
}

#[test]
fn test_for_loop_with_break_in_if() {
    // This test verifies BREAK IF conditional works correctly
    // Using BREAK IF syntax which is the idiomatic OVSM way
    let code = r#"
$sum = 0
FOR $i IN [1..10]:
    BREAK IF $i > 5
    $sum = $sum + $i
RETURN $sum
"#;
    let result = execute(code.trim()).unwrap();

    // Expected: Int(15) = 1+2+3+4+5
    // When $i = 6, BREAK IF triggers, stopping the loop
    assert_eq!(result, Value::Int(15));
}

#[test]
fn test_break_without_if_works() {
    // Test if BREAK works when not inside IF
    let code = r#"
FOR $i IN [1..10]:
    BREAK
RETURN "done"
"#;
    let result = execute(code.trim());

    // This should work
    assert!(result.is_ok(), "BREAK should work even without IF");
}

#[test]
fn test_for_loop_with_if_then_else() {
    // Test FOR loop with IF/THEN/ELSE (without BREAK)
    // This tests conditional logic inside loops
    let code = r#"
$count = 0
FOR $i IN [1..11]:
    IF $i % 3 == 0 THEN
        $count = $count + 1
    ELSE
        $count = $count
RETURN $count
"#;
    let result = execute(code.trim()).unwrap();

    // Expected: 3 (numbers 3, 6, 9 are divisible by 3)
    assert_eq!(result, Value::Int(3), "FOR loop with IF/ELSE should execute loop body");
}

#[test]
fn test_for_loop_with_if_only() {
    // Test FOR loop with IF only (no ELSE)
    // NOTE: In OVSM, bare IF statements inside loops should have ELSE for clarity
    // This test verifies the behavior works correctly
    let code = r#"
$sum = 0
FOR $i IN [1..11]:
    IF $i > 5 THEN
        $sum = $sum + $i
    ELSE
        $sum = $sum
RETURN $sum
"#;
    let result = execute(code.trim()).unwrap();

    // Expected: 6+7+8+9+10 = 40
    assert_eq!(result, Value::Int(40), "FOR loop with IF should accumulate matching values");
}

#[test]
fn test_for_loop_simple_without_if() {
    // Baseline: FOR loop without any IF
    let code = r#"
$sum = 0
FOR $i IN [1..4]:
    $sum = $sum + $i
RETURN $sum
"#;
    let result = execute(code.trim()).unwrap();
    // Expected: 1+2+3 = 6
    assert_eq!(result, Value::Int(6), "FOR loop without IF should work correctly");
}

#[test]
fn debug_ast_for_if_else() {
    use ovsm::{Parser, Scanner};
    // Debug: Print the AST to understand structure
    let code = r#"
$count = 0
FOR $i IN [1..11]:
    IF $i % 3 == 0 THEN
        $count = $count + 1
    ELSE
        $count = $count
RETURN $count
"#;
    let mut scanner = Scanner::new(code.trim());
    let tokens = scanner.scan_tokens().unwrap();

    println!("\n=== TOKENS ===");
    for token in &tokens {
        println!("{:?}", token);
    }

    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    println!("\n=== AST (FOR loop body) ===");
    println!("{:#?}", program);
}
