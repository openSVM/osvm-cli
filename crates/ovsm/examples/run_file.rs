//! Example: Execute OVSM scripts from files
//!
//! Usage: cargo run --example run_file <script.ovsm>

use ovsm::{Evaluator, Parser, Scanner, Value};
use std::env;
use std::fs;
use std::process;

fn main() {
    // Get script file from command line
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: cargo run --example run_file <script.ovsm>");
        eprintln!("\nExample scripts in examples/:");
        eprintln!("  - hello_world.ovsm");
        eprintln!("  - fibonacci.ovsm");
        eprintln!("  - factorial.ovsm");
        eprintln!("  - array_operations.ovsm");
        process::exit(1);
    }

    let file_path = &args[1];

    // Read the script file
    let code = match fs::read_to_string(file_path) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", file_path, err);
            process::exit(1);
        }
    };

    println!("🚀 Executing: {}", file_path);
    println!("{}", "=".repeat(60));

    // Execute the script
    match execute_ovsm(&code) {
        Ok(result) => {
            println!("\n✅ Result: {:?}", result);
        }
        Err(err) => {
            eprintln!("\n❌ Error: {}", err);
            process::exit(1);
        }
    }
}

fn execute_ovsm(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    // Tokenize
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens()?;

    // Parse
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;

    // Execute
    let mut evaluator = Evaluator::new();
    Ok(evaluator.execute(&program)?)
}
