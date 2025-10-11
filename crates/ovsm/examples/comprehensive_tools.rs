use ovsm::{Evaluator, Parser, Scanner};

fn main() {
    println!("=== Comprehensive OVSM Tools Demo ===\n");

    let examples = vec![
        // Data processing tools
        (
            "PREPEND - Add element to front",
            r#"
                $list = [2, 3, 4]
                $result = PREPEND($list, 1)
                RETURN $result
            "#,
        ),
        (
            "SLICE - Extract portion of array",
            r#"
                $numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
                $middle = SLICE($numbers, 3, 7)
                RETURN $middle
            "#,
        ),
        (
            "TOP_N - Get first N elements",
            r#"
                $scores = [95, 87, 92, 78, 85, 90]
                $top3 = TOP_N($scores, 3)
                RETURN $top3
            "#,
        ),
        (
            "BOTTOM_N - Get last N elements",
            r#"
                $numbers = [10, 20, 30, 40, 50]
                $last2 = BOTTOM_N($numbers, 2)
                RETURN $last2
            "#,
        ),
        (
            "ANY - Check if any element is truthy",
            r#"
                $values = [0, 0, 1, 0]
                $hasTrue = ANY($values)
                RETURN $hasTrue
            "#,
        ),
        (
            "ALL - Check if all elements are truthy",
            r#"
                $values = [1, 2, 3, 4]
                $allTrue = ALL($values)
                RETURN $allTrue
            "#,
        ),
        (
            "FIND - Find element index",
            r#"
                $items = [10, 20, 30, 40, 50]
                $index = FIND($items, 30)
                RETURN $index
            "#,
        ),
        (
            "JOIN - Join array to string",
            r#"
                $words = ["Hello", "World", "from", "OVSM"]
                $sentence = JOIN($words, " ")
                RETURN $sentence
            "#,
        ),
        (
            "SPLIT - Split string to array",
            r#"
                $csv = "apple,banana,cherry,date"
                $fruits = SPLIT($csv, ",")
                RETURN $fruits
            "#,
        ),
        // Math tools
        (
            "ABS - Absolute value",
            r#"
                $neg = -42
                $pos = ABS($neg)
                RETURN $pos
            "#,
        ),
        (
            "SQRT - Square root",
            r#"
                $area = 144
                $side = SQRT($area)
                RETURN $side
            "#,
        ),
        (
            "POW - Power function",
            r#"
                $base = 3
                $exp = 4
                $result = POW($base, $exp)
                RETURN $result
            "#,
        ),
        (
            "ROUND - Round to nearest integer",
            r#"
                $pi = 3.14159
                $rounded = ROUND($pi)
                RETURN $rounded
            "#,
        ),
        (
            "FLOOR - Round down",
            r#"
                $value = 7.9
                $floored = FLOOR($value)
                RETURN $floored
            "#,
        ),
        (
            "CEIL - Round up",
            r#"
                $value = 7.1
                $ceiled = CEIL($value)
                RETURN $ceiled
            "#,
        ),
        // Statistics tools
        (
            "SUM - Sum of array",
            r#"
                $expenses = [45.50, 23.75, 67.20, 15.00]
                $total = SUM($expenses)
                RETURN $total
            "#,
        ),
        (
            "MEAN - Average of array",
            r#"
                $grades = [88, 92, 76, 95, 84]
                $average = MEAN($grades)
                RETURN $average
            "#,
        ),
        (
            "MEDIAN - Middle value",
            r#"
                $scores = [65, 70, 75, 80, 85, 90, 95]
                $middle = MEDIAN($scores)
                RETURN $middle
            "#,
        ),
        (
            "MIN - Minimum value",
            r#"
                $temps = [72, 68, 75, 70, 73]
                $lowest = MIN($temps)
                RETURN $lowest
            "#,
        ),
        (
            "MAX - Maximum value",
            r#"
                $heights = [150, 165, 180, 172, 158]
                $tallest = MAX($heights)
                RETURN $tallest
            "#,
        ),
        (
            "STDDEV - Standard deviation",
            r#"
                $data = [2, 4, 4, 4, 5, 5, 7, 9]
                $deviation = STDDEV($data)
                RETURN $deviation
            "#,
        ),
        // Collection tools
        (
            "COUNT - Count elements",
            r#"
                $items = [1, 2, 3, 4, 5, 6, 7, 8]
                $count = COUNT($items)
                RETURN $count
            "#,
        ),
        (
            "FLATTEN - Flatten nested arrays",
            r#"
                $nested = [[1, 2], [3, 4], [5]]
                $flat = FLATTEN($nested)
                RETURN $flat
            "#,
        ),
        (
            "UNIQUE - Remove duplicates",
            r#"
                $items = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
                $unique = UNIQUE($items)
                RETURN $unique
            "#,
        ),
        (
            "SORT - Sort array",
            r#"
                $unsorted = [5, 2, 8, 1, 9, 3]
                $sorted = SORT($unsorted)
                RETURN $sorted
            "#,
        ),
        (
            "REVERSE - Reverse array",
            r#"
                $forward = [1, 2, 3, 4, 5]
                $backward = REVERSE($forward)
                RETURN $backward
            "#,
        ),
        (
            "FIRST - Get first element",
            r#"
                $list = [100, 200, 300]
                $first = FIRST($list)
                RETURN $first
            "#,
        ),
        (
            "LAST - Get last element",
            r#"
                $list = [100, 200, 300]
                $last = LAST($list)
                RETURN $last
            "#,
        ),
        (
            "APPEND - Add to end",
            r#"
                $list = [1, 2, 3]
                $extended = APPEND($list, 4)
                RETURN $extended
            "#,
        ),
        // Combined example
        (
            "Complex Pipeline",
            r#"
                $numbers = [5, 2, 8, 1, 9, 3, 7, 4, 6]
                $sorted = SORT($numbers)
                $top5 = TOP_N($sorted, 5)
                $sum = SUM($top5)
                $avg = MEAN($top5)
                $min = MIN($top5)
                $max = MAX($top5)
                $count = COUNT($top5)
                RETURN $count
            "#,
        ),
    ];

    let mut success = 0;
    let mut failed = 0;

    for (name, source) in examples {
        println!("--- {} ---", name);

        match execute_ovsm(source) {
            Ok(result) => {
                println!("✓ Result: {}\n", result);
                success += 1;
            }
            Err(e) => {
                println!("✗ Error: {}\n", e);
                failed += 1;
            }
        }
    }

    // Show tool count
    let registry = ovsm::ToolRegistry::new();
    println!("\n=== Summary ===");
    println!("Tests run: {}", success + failed);
    println!("✓ Passed: {}", success);
    println!("✗ Failed: {}", failed);
    println!("\nTotal tools available: {}", registry.count());

    // Group tools by category
    let tools = registry.list_tools();
    println!("\nTools by category:");

    let data_tools: Vec<_> = tools
        .iter()
        .filter(|t| {
            matches!(
                t.as_str(),
                "APPEND"
                    | "PREPEND"
                    | "SLICE"
                    | "TOP_N"
                    | "BOTTOM_N"
                    | "ANY"
                    | "ALL"
                    | "FIND"
                    | "JOIN"
                    | "SPLIT"
                    | "COUNT"
                    | "FLATTEN"
                    | "UNIQUE"
                    | "SORT"
                    | "REVERSE"
                    | "FIRST"
                    | "LAST"
                    | "MAP"
                    | "FILTER"
                    | "REDUCE"
            )
        })
        .cloned()
        .collect::<Vec<_>>();
    println!(
        "  Data Processing ({}): {}",
        data_tools.len(),
        data_tools.join(", ")
    );

    let math_tools: Vec<_> = tools
        .iter()
        .filter(|t| {
            matches!(
                t.as_str(),
                "ABS" | "SQRT" | "POW" | "ROUND" | "FLOOR" | "CEIL"
            )
        })
        .cloned()
        .collect::<Vec<_>>();
    println!("  Math ({}): {}", math_tools.len(), math_tools.join(", "));

    let stats_tools: Vec<_> = tools
        .iter()
        .filter(|t| {
            matches!(
                t.as_str(),
                "SUM" | "MEAN" | "MEDIAN" | "MIN" | "MAX" | "STDDEV"
            )
        })
        .cloned()
        .collect::<Vec<_>>();
    println!(
        "  Statistics ({}): {}",
        stats_tools.len(),
        stats_tools.join(", ")
    );

    let util_tools: Vec<_> = tools
        .iter()
        .filter(|t| matches!(t.as_str(), "LOG" | "ERROR"))
        .cloned()
        .collect::<Vec<_>>();
    println!(
        "  Utilities ({}): {}",
        util_tools.len(),
        util_tools.join(", ")
    );
}

fn execute_ovsm(source: &str) -> Result<ovsm::Value, ovsm::Error> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    evaluator.execute(&program)
}
