//! Test comparison operators work correctly
//!
//! NOTE: These tests use the obsolete indentation-based Python-like syntax
//! that has been removed in favor of LISP/S-expression syntax.
//! They are disabled until converted to LISP syntax.

#![cfg(feature = "incomplete_tests")]

use ovsm::{Evaluator, Parser, Scanner, Value};

fn execute(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let code = code.trim();
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    Ok(evaluator.execute(&program)?)
}

#[test]
fn test_if_with_comparison() {
    let code = r#"
$x = 1
$y = 5
IF $x > $y THEN
    RETURN "x is bigger"
ELSE
    RETURN "x is smaller"
"#;
    let result = execute(code).unwrap();

    assert_eq!(result, Value::String("x is smaller".to_string()));
}

#[test]
fn test_for_with_if_comparison() {
    // Fixed: Use ELSE clause for proper block structure
    let code = r#"
$result = "all small"
FOR $i IN [1..3]:
    IF $i > 2 THEN
        $result = "found big"
    ELSE
        $result = $result
RETURN $result
"#;
    let result = execute(code).unwrap();

    assert_eq!(result, Value::String("all small".to_string()));
}

#[test]
fn test_break_with_comparison() {
    // Fixed: Use BREAK IF syntax which is idiomatic OVSM
    let code = r#"
$sum = 0
FOR $i IN [1..10]:
    BREAK IF $i > 5
    $sum = $sum + $i
RETURN $sum
"#;
    let result = execute(code).unwrap();

    // Should sum 1+2+3+4+5 = 15
    assert_eq!(result, Value::Int(15));
}
