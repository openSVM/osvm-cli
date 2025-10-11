//! Simple REPL (Read-Eval-Print Loop) for OVSM
//!
//! Usage: cargo run --example simple_repl

use ovsm::{Evaluator, Parser, Scanner};
use std::io::{self, Write};

fn main() {
    println!("╔══════════════════════════════════════════╗");
    println!("║   OVSM Interactive REPL v1.0.0           ║");
    println!("╚══════════════════════════════════════════╝");
    println!();
    println!("Type OVSM expressions and press Enter.");
    println!("Type 'exit' or press Ctrl+C to quit.");
    println!("Type 'help' for examples.");
    println!();

    let mut evaluator = Evaluator::new();
    let mut line_num = 1;

    loop {
        // Print prompt
        print!("ovsm[{}]> ", line_num);
        io::stdout().flush().unwrap();

        // Read input
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => break, // EOF
            Ok(_) => {}
            Err(err) => {
                eprintln!("Error reading input: {}", err);
                continue;
            }
        }

        let input = input.trim();

        // Handle special commands
        match input {
            "" => continue,
            "exit" | "quit" => {
                println!("Goodbye! 👋");
                break;
            }
            "help" => {
                print_help();
                continue;
            }
            "clear" => {
                evaluator = Evaluator::new();
                println!("Environment cleared! ✨");
                continue;
            }
            _ => {}
        }

        // Execute OVSM code
        match execute_line(&mut evaluator, input) {
            Ok(result) => {
                println!("  ⇒ {:?}", result);
            }
            Err(err) => {
                eprintln!("  ✗ Error: {}", err);
            }
        }

        line_num += 1;
    }
}

fn execute_line(
    evaluator: &mut Evaluator,
    code: &str,
) -> Result<ovsm::Value, Box<dyn std::error::Error>> {
    // Tokenize
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens()?;

    // Parse
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;

    // Execute
    Ok(evaluator.execute(&program)?)
}

fn print_help() {
    println!();
    println!("╔══════════════════════════════════════════╗");
    println!("║           OVSM REPL Help                 ║");
    println!("╚══════════════════════════════════════════╝");
    println!();
    println!("Commands:");
    println!("  help   - Show this help");
    println!("  clear  - Clear environment");
    println!("  exit   - Exit REPL");
    println!();
    println!("Examples:");
    println!();
    println!("  Basic arithmetic:");
    println!("    RETURN 2 + 3 * 4");
    println!("    RETURN 2 ** 8");
    println!();
    println!("  Variables:");
    println!("    $x = 10");
    println!("    $y = 20");
    println!("    RETURN $x + $y");
    println!();
    println!("  Note: Each line is a complete program!");
    println!("    Variables don't persist between lines yet.");
    println!();
    println!("  Arrays:");
    println!("    $nums = [1, 2, 3, 4, 5]");
    println!("    RETURN $nums");
    println!();
    println!("  Control flow:");
    println!("    IF 5 > 3 THEN RETURN \"yes\" ELSE RETURN \"no\"");
    println!();
    println!("  Loops:");
    println!("    $sum = 0");
    println!("    FOR $i IN [1..5]: $sum = $sum + $i");
    println!("    RETURN $sum");
    println!();
    println!("  Comparisons:");
    println!("    RETURN 10 > 5");
    println!("    RETURN \"hello\" == \"world\"");
    println!();
    println!("  Logical operators:");
    println!("    RETURN TRUE AND FALSE");
    println!("    RETURN TRUE OR FALSE");
    println!("    RETURN NOT FALSE");
    println!();
    println!("  Ternary:");
    println!("    RETURN 10 > 5 ? \"big\" : \"small\"");
    println!();
}
