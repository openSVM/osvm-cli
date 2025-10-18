//! QA Test Runner - Executes OVSM code from markdown files
//!
//! This tool:
//! 1. Reads markdown files with ```ovsm code blocks
//! 2. Executes the OVSM code
//! 3. Displays results
//!
//! Usage:
//!   cargo run --example qa_test_runner -- path/to/qa_file.md

use ovsm::{Evaluator, Parser, Scanner, Value};
use std::env;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Get command line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <markdown-file>", args[0]);
        eprintln!(
            "Example: {} ../../../test_qa_categories/06_token_research/01_basic.md",
            args[0]
        );
        std::process::exit(1);
    }

    let file_path = &args[1];

    // Read the markdown file
    println!("📖 Reading: {}", file_path);
    let content = fs::read_to_string(file_path)?;

    // Find all OVSM code blocks using simple string matching
    let ovsm_blocks = extract_ovsm_blocks(&content);

    if ovsm_blocks.is_empty() {
        println!("\n⚠️  No OVSM code blocks found in file!");
        println!("   Looking for blocks marked with ```ovsm");
        return Ok(());
    }

    let mut test_count = 0;
    let mut passed_count = 0;
    let mut failed_count = 0;

    println!("\n🔍 Found {} OVSM code blocks:", ovsm_blocks.len());
    println!("═══════════════════════════════════════════════");

    for code in ovsm_blocks {
        test_count += 1;

        println!("\n📝 Test #{}: ", test_count);
        println!("───────────────────────────────────────────────");
        println!("Code:\n{}", code.trim());
        println!("───────────────────────────────────────────────");

        // Execute the OVSM code
        match execute_ovsm(&code) {
            Ok(result) => {
                println!("✅ Result: {}", format_value(&result));
                passed_count += 1;
            }
            Err(error) => {
                println!("❌ Error: {}", error);
                failed_count += 1;
            }
        }
    }

    println!("\n═══════════════════════════════════════════════");
    println!("📊 Summary:");
    println!("   Total tests: {}", test_count);
    println!("   ✅ Passed: {}", passed_count);
    println!("   ❌ Failed: {}", failed_count);
    println!(
        "   📈 Pass rate: {:.1}%",
        if test_count > 0 {
            (passed_count as f64 / test_count as f64) * 100.0
        } else {
            0.0
        }
    );

    Ok(())
}

/// Extract OVSM code blocks from markdown content
fn extract_ovsm_blocks(content: &str) -> Vec<String> {
    let mut blocks = Vec::new();
    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        // Look for ```ovsm start marker
        if lines[i].trim() == "```ovsm" {
            let mut code_lines = Vec::new();
            i += 1;

            // Collect lines until we find closing ```
            while i < lines.len() && lines[i].trim() != "```" {
                code_lines.push(lines[i]);
                i += 1;
            }

            if !code_lines.is_empty() {
                blocks.push(code_lines.join("\n"));
            }
        }
        i += 1;
    }

    blocks
}

/// Execute OVSM code and return the result
fn execute_ovsm(code: &str) -> Result<Value, String> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner
        .scan_tokens()
        .map_err(|e| format!("Scanner error: {:?}", e))?;

    let mut parser = Parser::new(tokens);
    let program = parser
        .parse()
        .map_err(|e| format!("Parser error: {:?}", e))?;

    let mut evaluator = Evaluator::new();
    evaluator
        .execute(&program)
        .map_err(|e| format!("Runtime error: {:?}", e))
}

/// Format a Value for display
fn format_value(value: &Value) -> String {
    match value {
        Value::Int(n) => format!("Int({})", n),
        Value::Float(f) => format!("Float({})", f),
        Value::String(s) => format!("String(\"{}\")", s),
        Value::Bool(b) => format!("Bool({})", b),
        Value::Null => "Null".to_string(),
        Value::Array(arr) => {
            let items: Vec<String> = arr.iter().map(format_value).collect();
            format!("Array([{}])", items.join(", "))
        }
        Value::Object(obj) => {
            let pairs: Vec<String> = obj
                .iter()
                .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                .collect();
            format!("Object({{{}}})", pairs.join(", "))
        }
        Value::Range { start, end } => format!("Range({}..{})", start, end),
        Value::Function { params, .. } => format!("Function({} params)", params.len()),
    }
}
