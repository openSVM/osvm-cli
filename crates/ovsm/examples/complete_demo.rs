use ovsm::{Evaluator, Parser, Scanner};

fn main() {
    println!("=== OVSM Interpreter - Complete Demo ===\n");

    let examples = vec![
        (
            "Arithmetic",
            r#"
                $x = 10
                $y = 20
                $result = $x + $y * 2
                RETURN $result
            "#,
        ),
        (
            "If-Else",
            r#"
                $score = 85
                IF $score >= 60 THEN
                    $grade = "Pass"
                ELSE
                    $grade = "Fail"
                RETURN $grade
            "#,
        ),
        (
            "While Loop",
            r#"
                $sum = 0
                $i = 1
                WHILE $i <= 10:
                    $sum = $sum + $i
                    $i = $i + 1
                RETURN $sum
            "#,
        ),
        (
            "For Loop with Range",
            r#"
                $product = 1
                FOR $n IN [1..6]:
                    $product = $product * $n
                RETURN $product
            "#,
        ),
        (
            "For Loop with Array",
            r#"
                $total = 0
                FOR $val IN [5, 10, 15, 20]:
                    $total = $total + $val
                RETURN $total
            "#,
        ),
        (
            "Array Operations",
            r#"
                $numbers = [100, 200, 300]
                $first = $numbers[0]
                $second = $numbers[1]
                RETURN $first + $second
            "#,
        ),
        (
            "Object Operations",
            r#"
                $person = {name: "Bob", age: 25}
                $name = $person.name
                $age = $person.age
                RETURN $age
            "#,
        ),
        (
            "Logical Operators",
            r#"
                $a = true
                $b = false
                $result = $a AND NOT $b
                RETURN $result
            "#,
        ),
        (
            "Comparison",
            r#"
                $x = 15
                $y = 10
                $greater = $x > $y
                RETURN $greater
            "#,
        ),
        (
            "Ternary Operator",
            r#"
                $value = 42
                $message = $value > 50 ? "high" : "low"
                RETURN $message
            "#,
        ),
    ];

    for (name, source) in examples {
        println!("--- {} ---", name);
        println!("Source:\n{}", source.trim());

        match execute_ovsm(source) {
            Ok(result) => println!("Result: {}\n", result),
            Err(e) => println!("Error: {}\n", e),
        }
    }

    println!("=== All examples completed successfully! ===");
}

fn execute_ovsm(source: &str) -> Result<ovsm::Value, ovsm::Error> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    evaluator.execute(&program)
}
