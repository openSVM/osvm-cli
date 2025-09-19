/// Test debug flag functionality for AI queries
use std::process::Command;

#[test]
fn test_ai_query_without_debug_flag() {
    let output = Command::new(env!("CARGO_BIN_EXE_osvm"))
        .args(["hello"])
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    
    // Should not contain debug information
    assert!(!stdout.contains("🔍 Interpreting as AI query"));
    assert!(!stdout.contains("✅ Loaded"));
    assert!(!stdout.contains("📤 OSVM AI Request"));
    assert!(!stdout.contains("📥 OSVM AI Response"));
    assert!(!stdout.contains("🔍 AI Response received"));
    assert!(!stdout.contains("🤖 AI Response:"));
    
    // Should contain the actual response (without debug prefix)
    assert!(!stdout.is_empty());
}

#[test]
fn test_ai_query_with_debug_flag() {
    let output = Command::new(env!("CARGO_BIN_EXE_osvm"))
        .args(["--debug", "hello"])
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    
    // Should contain debug information
    assert!(stdout.contains("🔍 Interpreting as AI query"));
    assert!(stdout.contains("✅ Loaded"));
    assert!(stdout.contains("📤 OSVM AI Request"));
    assert!(stdout.contains("📥 OSVM AI Response"));
    assert!(stdout.contains("🔍 AI Response received"));
    assert!(stdout.contains("🤖 AI Response:"));
}

#[test]
fn test_debug_flag_appears_in_help() {
    let output = Command::new(env!("CARGO_BIN_EXE_osvm"))
        .args(["--help"])
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    
    // Should contain debug flag in help
    assert!(stdout.contains("--debug"));
    assert!(stdout.contains("Show debug information"));
}