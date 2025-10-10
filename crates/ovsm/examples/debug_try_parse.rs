use ovsm::{Parser, Scanner};

fn main() {
    let code = r#"
        TRY:
            $x = 10 / 0
        CATCH:
            $x = -1
        RETURN $x
    "#;

    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    println!("Program AST:");
    println!("{:#?}", program);
}
