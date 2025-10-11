use ovsm::{Parser, Scanner};

fn main() {
    let source = r#"
        $sum = 0
        FOR $i IN [1..6]:
            $sum = $sum + $i
        RETURN $sum
    "#;

    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    println!("AST:\n{:#?}", program);
}
