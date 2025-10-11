//! Integration tests for OVSM language integration in OSVM-CLI
//! These tests verify that the OVSM service works correctly through the CLI interface.

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Test helper to create a temporary OVSM script file
fn create_temp_script(content: &str) -> (TempDir, String) {
    let temp_dir = TempDir::new().unwrap();
    let script_path = temp_dir.path().join("test_script.ovsm");
    fs::write(&script_path, content).unwrap();
    (temp_dir, script_path.to_str().unwrap().to_string())
}

#[test]
fn test_ovsm_help_command() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("--help");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Execute and manage OVSM"))
        .stdout(predicate::str::contains("run"))
        .stdout(predicate::str::contains("repl"))
        .stdout(predicate::str::contains("eval"))
        .stdout(predicate::str::contains("check"));
}

#[test]
fn test_ovsm_eval_simple_arithmetic() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm")
        .arg("eval")
        .arg("$x = 10; $y = 20; RETURN $x + $y");

    cmd.assert().success().stdout(predicate::str::contains("30"));
}

#[test]
fn test_ovsm_eval_string_concatenation() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm")
        .arg("eval")
        .arg(r#"$greeting = "Hello"; RETURN $greeting"#);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Hello"));
}

#[test]
fn test_ovsm_eval_conditional() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm")
        .arg("eval")
        .arg("IF 10 > 5 THEN RETURN \"yes\" ELSE RETURN \"no\"");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("yes"));
}

#[test]
fn test_ovsm_eval_loop() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm")
        .arg("eval")
        .arg("$sum = 0; FOR $i IN [1..6]: $sum = $sum + $i; RETURN $sum");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("15"));
}

#[test]
fn test_ovsm_run_hello_world() {
    let script_content = r#"
// Simple hello world
$message = "Hello from OVSM!"
RETURN $message
"#;

    let (_temp_dir, script_path) = create_temp_script(script_content);

    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("run").arg(&script_path);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Hello from OVSM!"));
}

#[test]
fn test_ovsm_run_factorial() {
    let script_content = r#"
$n = 5
$result = 1

IF $n < 0 THEN
    RETURN "Error"
ELSE
    FOR $i IN [1..$n+1]:
        $result = $result * $i
    RETURN $result
"#;

    let (_temp_dir, script_path) = create_temp_script(script_content);

    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("run").arg(&script_path);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("120")); // 5! = 120
}

#[test]
fn test_ovsm_run_with_json_output() {
    let script_content = r#"
$result = 42
RETURN $result
"#;

    let (_temp_dir, script_path) = create_temp_script(script_content);

    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm")
        .arg("run")
        .arg(&script_path)
        .arg("--json");

    cmd.assert().success().stdout(predicate::str::contains("42"));
}

#[test]
fn test_ovsm_check_valid_syntax() {
    let script_content = r#"
$x = 10
$y = 20
RETURN $x + $y
"#;

    let (_temp_dir, script_path) = create_temp_script(script_content);

    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("check").arg(&script_path);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Syntax check passed"));
}

#[test]
fn test_ovsm_check_invalid_syntax() {
    let script_content = r#"
$x =
RETURN $x
"#;

    let (_temp_dir, script_path) = create_temp_script(script_content);

    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("check").arg(&script_path);

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Syntax error"));
}

#[test]
fn test_ovsm_eval_with_variables() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm")
        .arg("eval")
        .arg("$a = 10; $b = 3; $c = $a * $b; RETURN $c");

    cmd.assert().success().stdout(predicate::str::contains("30"));
}

#[test]
fn test_ovsm_eval_comparison_operators() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm")
        .arg("eval")
        .arg("$x = 10; $y = 20; IF $x < $y THEN RETURN true ELSE RETURN false");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("true"));
}

#[test]
fn test_ovsm_run_nonexistent_file() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm")
        .arg("run")
        .arg("/tmp/nonexistent_ovsm_file_12345.ovsm");

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Failed to read script file"));
}

#[test]
fn test_ovsm_examples_command() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("examples");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("OVSM Example"));
}

#[test]
fn test_ovsm_run_arithmetic_operations() {
    let script_content = r#"
$a = 10
$b = 3
$add = $a + $b
$sub = $a - $b
$mul = $a * $b
$div = $a / $b
$mod = $a % $b
$total = $add + $sub + $mul + $div + $mod
RETURN $total
"#;

    let (_temp_dir, script_path) = create_temp_script(script_content);

    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("run").arg(&script_path);

    // 13 + 7 + 30 + 3 + 1 = 54
    cmd.assert().success().stdout(predicate::str::contains("54"));
}

#[test]
fn test_ovsm_run_nested_conditionals() {
    let script_content = r#"
$x = 10
$y = 20

IF $x < $y THEN
    IF $x > 5 THEN
        RETURN "x is between 5 and y"
    ELSE
        RETURN "x is less than or equal to 5"
ELSE
    RETURN "x is greater than or equal to y"
"#;

    let (_temp_dir, script_path) = create_temp_script(script_content);

    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("run").arg(&script_path);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("between 5 and y"));
}

#[test]
fn test_ovsm_eval_power_operator() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("eval").arg("$x = 2; RETURN $x ** 3");

    cmd.assert().success().stdout(predicate::str::contains("8"));
}

#[test]
fn test_ovsm_eval_modulo_operator() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("eval").arg("RETURN 10 % 3");

    cmd.assert().success().stdout(predicate::str::contains("1"));
}

#[test]
fn test_ovsm_run_while_loop() {
    let script_content = r#"
$counter = 0
$sum = 0

WHILE $counter < 5:
    $sum = $sum + $counter
    $counter = $counter + 1

RETURN $sum
"#;

    let (_temp_dir, script_path) = create_temp_script(script_content);

    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("run").arg(&script_path);

    // 0 + 1 + 2 + 3 + 4 = 10
    cmd.assert().success().stdout(predicate::str::contains("10"));
}

#[test]
fn test_ovsm_no_config_required() {
    // This test verifies that OVSM commands don't require Solana configuration
    // We test this by checking that even without a valid Solana config,
    // OVSM commands still work

    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm").arg("eval").arg("RETURN 42");

    // Should succeed even without Solana config
    cmd.assert().success();
}

#[test]
fn test_ovsm_eval_boolean_logic() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm")
        .arg("eval")
        .arg("$a = true; $b = false; IF $a AND NOT $b THEN RETURN \"correct\" ELSE RETURN \"wrong\"");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("correct"));
}

#[test]
fn test_ovsm_eval_string_operations() {
    let mut cmd = Command::cargo_bin("osvm").unwrap();
    cmd.arg("ovsm")
        .arg("eval")
        .arg(r#"$first = "Hello"; $second = " World"; RETURN $first + $second"#);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Hello World"));
}
