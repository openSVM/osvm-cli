use ovsm::{Evaluator, Parser, Scanner};

fn main() {
    let source = "$x = true AND false\nRETURN $x";

    println!("Source: {}\n", source);

    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();

    println!("Tokens:");
    for t in &tokens {
        println!("  {:?}", t.kind);
    }

    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    println!("\nAST: {:#?}", program);

    let mut evaluator = Evaluator::new();
    match evaluator.execute(&program) {
        Ok(result) => println!("\nResult: {:?}", result),
        Err(e) => eprintln!("\nError: {}", e),
    }
}
