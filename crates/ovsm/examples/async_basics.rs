//! Basic async/await examples in OVSM
//!
//! Run with: cargo run --example async_basics

use ovsm::{Evaluator, Parser, Result, Scanner, Value};

/// Helper function to execute OVSM code
fn execute_ovsm(evaluator: &mut Evaluator, code: &str) -> Result<Value> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    evaluator.execute(&program)
}

fn main() {
    println!("═══════════════════════════════════════");
    println!("  OVSM V6.1 - Async/Await Basics");
    println!("═══════════════════════════════════════\n");

    let mut evaluator = Evaluator::new();

    // Example 1: Simple async/await
    println!("Example 1: Simple async/await");
    println!("─────────────────────────────────────");
    let code = r#"
(do
  (defun compute (x y)
    (do
      (sleep 50)
      (* x y)))

  (define handle (async compute 5 7))
  (println (str "Created handle: " handle))

  (define result (await handle))
  (println (str "Result: " result))
  result)
"#;

    match execute_ovsm(&mut evaluator, code) {
        Ok(result) => println!("✅ Returned: {:?}\n", result),
        Err(e) => println!("❌ Error: {}\n", e),
    }

    // Example 2: Multiple concurrent tasks
    println!("Example 2: Multiple concurrent tasks");
    println!("─────────────────────────────────────");
    let code = r#"
(do
  (defun factorial (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))

  ;; Launch 5 concurrent factorial computations
  (define handles [])
  (define nums [5 6 7 8 9])

  (for (n nums)
    (set! handles (append handles [(async factorial n)])))

  (println (str "Launched " (length handles) " concurrent tasks"))

  ;; Collect results
  (define results [])
  (for (h handles)
    (set! results (append results [(await h)])))

  (println (str "Results: " results))
  results)
"#;

    match execute_ovsm(&mut evaluator, code) {
        Ok(result) => println!("✅ Returned: {:?}\n", result),
        Err(e) => println!("❌ Error: {}\n", e),
    }

    // Example 3: Fire-and-forget
    println!("Example 3: Fire-and-forget (no await)");
    println!("─────────────────────────────────────");
    let code = r#"
(do
  (defun background-task (id)
    (do
      (sleep 100)
      (println (str "Background task " id " completed"))))

  ;; Launch tasks without awaiting
  (async background-task 1)
  (async background-task 2)
  (async background-task 3)

  (println "Main thread continues immediately")
  "done")
"#;

    match execute_ovsm(&mut evaluator, code) {
        Ok(result) => println!("✅ Returned: {:?}\n", result),
        Err(e) => println!("❌ Error: {}\n", e),
    }

    // Example 4: Async with closures
    println!("Example 4: Async with lambda/closures");
    println!("─────────────────────────────────────");
    let code = r#"
(do
  (define multiplier 10)

  (define process (lambda (x)
    (do
      (sleep 30)
      (* x multiplier))))

  (define handles [])
  (for (i (range 1 6))
    (set! handles (append handles [(async process i)])))

  (define results [])
  (for (h handles)
    (set! results (append results [(await h)])))

  (println (str "Processed: " results))
  results)
"#;

    match execute_ovsm(&mut evaluator, code) {
        Ok(result) => println!("✅ Returned: {:?}\n", result),
        Err(e) => println!("❌ Error: {}\n", e),
    }

    // Example 5: Error handling in async
    println!("Example 5: Error handling in async tasks");
    println!("─────────────────────────────────────");
    let code = r#"
(do
  (defun safe-divide (a b)
    (if (= b 0)
        (do
          (println "Division by zero!")
          null)
        (/ a b)))

  (define h1 (async safe-divide 10 2))
  (define h2 (async safe-divide 10 0))
  (define h3 (async safe-divide 20 4))

  (define r1 (await h1))
  (define r2 (await h2))
  (define r3 (await h3))

  (println (str "Results: [" r1 ", " r2 ", " r3 "]"))
  [r1 r2 r3])
"#;

    match execute_ovsm(&mut evaluator, code) {
        Ok(result) => println!("✅ Returned: {:?}\n", result),
        Err(e) => println!("❌ Error: {}\n", e),
    }

    println!("═══════════════════════════════════════");
    println!("All examples completed!");
}
