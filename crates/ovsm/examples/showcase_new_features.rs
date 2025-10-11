use ovsm::{Evaluator, Parser, Scanner};

fn main() {
    println!("🎉 OVSM Interpreter - New Features Showcase\n");
    println!("Demonstrating GUARD clauses and TRY-CATCH error handling\n");
    println!("{}", "=".repeat(70));

    // Example 1: GUARD clauses for input validation
    println!("\n📋 Example 1: Input Validation with GUARD\n");

    let code1 = r#"
        // Simulating a function that processes a withdrawal
        $balance = 100
        $withdrawal = 150

        GUARD $balance >= $withdrawal ELSE
            RETURN "Insufficient funds"

        $balance = $balance - $withdrawal
        RETURN "Withdrawal successful"
    "#;

    execute_and_print("Withdraw $150 from $100 balance", code1);

    // Example 2: Multiple GUARD clauses
    println!("\n📋 Example 2: Multiple Guard Clauses\n");

    let code2 = r#"
        $age = 25
        $has_license = true
        $has_insurance = true

        GUARD $age >= 18 ELSE
            RETURN "Too young to drive"

        GUARD $has_license ELSE
            RETURN "No driver's license"

        GUARD $has_insurance ELSE
            RETURN "No insurance"

        RETURN "Approved to drive"
    "#;

    execute_and_print("Check driving eligibility", code2);

    // Example 3: TRY-CATCH for error handling
    println!("\n📋 Example 3: TRY-CATCH Error Handling\n");

    let code3 = r#"
        $values = [10, 5, 0, 2]
        $results = []

        FOR $divisor IN $values:
            TRY:
                $result = 100 / $divisor
                $results = APPEND($results, $result)
            CATCH:
                $results = APPEND($results, "ERROR")

        RETURN $results
    "#;

    execute_and_print("Divide 100 by [10, 5, 0, 2]", code3);

    // Example 4: Nested TRY-CATCH with GUARD
    println!("\n📋 Example 4: Combined GUARD + TRY-CATCH\n");

    let code4 = r#"
        $data = [1, 2, 3, 4, 5]
        $index = 10

        GUARD COUNT($data) > 0 ELSE
            RETURN "Empty array"

        TRY:
            $value = $data[$index]
        CATCH:
            $value = "Index out of bounds"

        RETURN $value
    "#;

    execute_and_print("Safe array access with guards", code4);

    // Example 5: Error recovery with TRY-CATCH
    println!("\n📋 Example 5: Error Recovery Pattern\n");

    let code5 = r#"
        // Try primary data source
        TRY:
            $primary_data = $undefined_variable
        CATCH:
            // Fallback to default
            $primary_data = [1, 2, 3]

        // Process the data (guaranteed to exist now)
        $sum = SUM($primary_data)
        $count = COUNT($primary_data)
        $avg = $sum / $count

        RETURN $avg
    "#;

    execute_and_print("Graceful fallback with error recovery", code5);

    // Example 6: Practical use case - Configuration validation
    println!("\n📋 Example 6: Configuration Validation\n");

    let code6 = r#"
        // Simulated config
        $config = {
            timeout: 30,
            retries: 3,
            url: "https://api.example.com"
        }

        // Validate configuration
        GUARD $config.timeout > 0 ELSE
            RETURN "Invalid timeout"

        GUARD $config.retries >= 1 ELSE
            RETURN "Invalid retry count"

        TRY:
            $url = $config.url
        CATCH:
            $url = "https://default.example.com"

        RETURN "Config validated"
    "#;

    execute_and_print("Configuration validation", code6);

    // Example 7: Safe tool execution
    println!("\n📋 Example 7: Safe Tool Execution\n");

    let code7 = r#"
        $numbers = []
        $safe_result = 0

        TRY:
            // This would error on empty array
            $max = MAX($numbers)
            $min = MIN($numbers)
            $safe_result = $max - $min
        CATCH:
            // Provide sensible default
            $safe_result = -1

        RETURN $safe_result
    "#;

    execute_and_print("Safe MAX/MIN on empty array", code7);

    println!("\n{}", "=".repeat(70));
    println!("\n✅ All examples executed successfully!");
    println!("\n🎓 Key Takeaways:");
    println!("   1. GUARD clauses enable early-exit validation patterns");
    println!("   2. TRY-CATCH provides robust error handling");
    println!("   3. Both features can be combined for safe, readable code");
    println!("   4. Error recovery patterns are now possible");
    println!("   5. Complex validation logic becomes simple and clear");
}

fn execute_and_print(description: &str, code: &str) {
    println!("Description: {}", description);
    println!("Code:");
    for line in code.lines().filter(|l| !l.trim().is_empty()) {
        println!("  {}", line);
    }

    let mut scanner = Scanner::new(code);
    let tokens = match scanner.scan_tokens() {
        Ok(t) => t,
        Err(e) => {
            println!("❌ Scanner error: {:?}\n", e);
            return;
        }
    };

    let mut parser = Parser::new(tokens);
    let program = match parser.parse() {
        Ok(p) => p,
        Err(e) => {
            println!("❌ Parser error: {:?}\n", e);
            return;
        }
    };

    let mut evaluator = Evaluator::new();
    let result = match evaluator.execute(&program) {
        Ok(r) => r,
        Err(e) => {
            println!("❌ Runtime error: {:?}\n", e);
            return;
        }
    };

    println!("\n🎯 Result: {:?}", result);
    println!();
}
