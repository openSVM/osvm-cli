use ovsm::{Evaluator, Parser, Scanner};

fn main() {
    println!("=== OVSM Tools Demo ===\n");

    let examples = vec![
        (
            "SUM - Add numbers",
            r#"
                $numbers = [10, 20, 30, 40, 50]
                $total = SUM($numbers)
                RETURN $total
            "#,
        ),
        (
            "COUNT - Count elements",
            r#"
                $items = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
                $count = COUNT($items)
                RETURN $count
            "#,
        ),
        (
            "MEAN - Calculate average",
            r#"
                $scores = [85, 90, 78, 92, 88]
                $average = MEAN($scores)
                RETURN $average
            "#,
        ),
        (
            "MEDIAN - Find middle value",
            r#"
                $values = [1, 3, 5, 7, 9]
                $median = MEDIAN($values)
                RETURN $median
            "#,
        ),
        (
            "MIN/MAX - Find extremes",
            r#"
                $data = [45, 23, 67, 12, 89, 34]
                $minimum = MIN($data)
                $maximum = MAX($data)
                RETURN {min: $minimum, max: $maximum}
            "#,
        ),
        (
            "FLATTEN - Flatten nested arrays",
            r#"
                $nested = [[1, 2], [3, 4], [5, 6]]
                $flat = FLATTEN($nested)
                RETURN $flat
            "#,
        ),
        (
            "UNIQUE - Remove duplicates",
            r#"
                $items = [1, 2, 2, 3, 3, 3, 4, 5, 5]
                $unique = UNIQUE($items)
                RETURN $unique
            "#,
        ),
        (
            "SORT - Sort array",
            r#"
                $unsorted = [5, 2, 8, 1, 9]
                $sorted = SORT($unsorted)
                RETURN $sorted
            "#,
        ),
        (
            "REVERSE - Reverse order",
            r#"
                $original = [1, 2, 3, 4, 5]
                $reversed = REVERSE($original)
                RETURN $reversed
            "#,
        ),
        (
            "FIRST/LAST - Get endpoints",
            r#"
                $list = [100, 200, 300, 400]
                $first = FIRST($list)
                $last = LAST($list)
                RETURN {first: $first, last: $last}
            "#,
        ),
        (
            "ABS - Absolute value",
            r#"
                $negative = -42
                $positive = ABS($negative)
                RETURN $positive
            "#,
        ),
        (
            "SQRT - Square root",
            r#"
                $value = 16
                $root = SQRT($value)
                RETURN $root
            "#,
        ),
        (
            "POW - Power",
            r#"
                $base = 2
                $exp = 10
                $result = POW($base, $exp)
                RETURN $result
            "#,
        ),
        (
            "ROUND/FLOOR/CEIL",
            r#"
                $value = 3.7
                $rounded = ROUND($value)
                $floored = FLOOR($value)
                $ceiled = CEIL($value)
                RETURN {round: $rounded, floor: $floored, ceil: $ceiled}
            "#,
        ),
        (
            "Complex calculation",
            r#"
                $data = [12, 15, 18, 20, 22, 25, 28]
                $avg = MEAN($data)
                $med = MEDIAN($data)
                $total = SUM($data)
                $count = COUNT($data)
                $min_val = MIN($data)
                $max_val = MAX($data)

                RETURN {
                    average: $avg,
                    median: $med,
                    total: $total,
                    count: $count,
                    min: $min_val,
                    max: $max_val
                }
            "#,
        ),
    ];

    for (name, source) in examples {
        println!("--- {} ---", name);

        match execute_ovsm(source) {
            Ok(result) => println!("Result: {}\n", result),
            Err(e) => println!("Error: {}\n", e),
        }
    }

    // Show tool count
    let registry = ovsm::ToolRegistry::new();
    println!("=== Summary ===");
    println!("Total tools available: {}", registry.count());
    println!("Tools: {}", registry.list_tools().join(", "));
}

fn execute_ovsm(source: &str) -> Result<ovsm::Value, ovsm::Error> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    evaluator.execute(&program)
}
