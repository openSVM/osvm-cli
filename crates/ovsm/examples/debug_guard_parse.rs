use ovsm::{Parser, Scanner};

fn main() {
    let code = r#"
        $x = 10
        GUARD $x > 0 ELSE
            RETURN -1
        RETURN $x
    "#;

    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens().unwrap();

    println!("Tokens:");
    for (i, tok) in tokens.iter().enumerate() {
        println!("  [{}] {:?} at line {}", i, tok.kind, tok.line);
    }

    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    println!("\nProgram AST:");
    println!("{:#?}", program);
}
