//! Concurrent data processing examples
//!
//! Demonstrates batch processing, map-reduce patterns, and pipeline processing
//!
//! Run with: cargo run --example async_data_processing

use ovsm::{Evaluator, Parser, Scanner, Result, Value};

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
    println!("  Async Data Processing Examples");
    println!("═══════════════════════════════════════\n");

    let mut evaluator = Evaluator::new();

    // Example 1: Batch processing
    println!("Example 1: Batch Data Processing");
    println!("─────────────────────────────────────");
    let code = r#"
(do
  (defun process-record (record)
    (do
      (sleep 20)  ;; Simulate I/O
      {:id (get record :id)
       :value (* (get record :value) 2)
       :status "processed"}))

  (define records [
    {:id 1 :value 10}
    {:id 2 :value 20}
    {:id 3 :value 30}
    {:id 4 :value 40}
    {:id 5 :value 50}
  ])

  (println (str "Processing " (length records) " records concurrently..."))

  ;; Launch all tasks
  (define handles [])
  (for (rec records)
    (set! handles (append handles [(async process-record rec)])))

  ;; Collect results
  (define results [])
  (for (h handles)
    (set! results (append results [(await h)])))

  (println "Results:")
  (for (r results)
    (println (str "  ID " (get r :id) ": " (get r :value) " (" (get r :status) ")")))

  results)
"#;

    match execute_ovsm(&mut evaluator, code) {
        Ok(_) => println!("✅ Batch processing completed\n"),
        Err(e) => println!("❌ Error: {}\n", e),
    }

    // Example 2: Map-Reduce pattern
    println!("Example 2: Map-Reduce Pattern");
    println!("─────────────────────────────────────");
    let code = r#"
(do
  (defun mapper (chunk-id data-chunk)
    (do
      (sleep 30)  ;; Simulate computation
      (define sum 0)
      (for (val data-chunk)
        (set! sum (+ sum val)))
      {:chunk-id chunk-id :sum sum}))

  ;; Split data into chunks
  (define data [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20])
  (define chunk-size 5)
  (define chunks [
    (take 5 data)
    (take 5 (drop 5 data))
    (take 5 (drop 10 data))
    (take 5 (drop 15 data))
  ])

  (println "Map Phase: Processing chunks in parallel...")

  ;; Map phase
  (define handles [])
  (define chunk-id 0)
  (for (chunk chunks)
    (do
      (set! handles (append handles [(async mapper chunk-id chunk)]))
      (set! chunk-id (+ chunk-id 1))))

  (define map-results [])
  (for (h handles)
    (set! map-results (append map-results [(await h)])))

  (println "Map results:")
  (for (r map-results)
    (println (str "  Chunk " (get r :chunk-id) ": sum = " (get r :sum))))

  ;; Reduce phase
  (println "\nReduce Phase: Computing total...")
  (define total 0)
  (for (r map-results)
    (set! total (+ total (get r :sum))))

  (println (str "Total sum: " total))
  total)
"#;

    match execute_ovsm(&mut evaluator, code) {
        Ok(_) => println!("✅ Map-Reduce completed\n"),
        Err(e) => println!("❌ Error: {}\n", e),
    }

    // Example 3: Pipeline processing
    println!("Example 3: Pipeline Processing");
    println!("─────────────────────────────────────");
    let code = r#"
(do
  (defun stage1-fetch (id)
    (do
      (sleep 20)
      {:id id :data (str "raw-" id)}))

  (defun stage2-transform (item)
    (do
      (sleep 15)
      {:id (get item :id) :data (str "transformed-" (get item :data))}))

  (defun stage3-validate (item)
    (do
      (sleep 10)
      {:id (get item :id) :data (get item :data) :valid true}))

  (define ids [1 2 3 4 5])

  ;; Stage 1: Fetch (parallel)
  (println "Stage 1: Fetching data...")
  (define stage1-handles [])
  (for (id ids)
    (set! stage1-handles (append stage1-handles [(async stage1-fetch id)])))

  (define stage1-results [])
  (for (h stage1-handles)
    (set! stage1-results (append stage1-results [(await h)])))

  ;; Stage 2: Transform (parallel)
  (println "Stage 2: Transforming data...")
  (define stage2-handles [])
  (for (item stage1-results)
    (set! stage2-handles (append stage2-handles [(async stage2-transform item)])))

  (define stage2-results [])
  (for (h stage2-handles)
    (set! stage2-results (append stage2-results [(await h)])))

  ;; Stage 3: Validate (parallel)
  (println "Stage 3: Validating data...")
  (define stage3-handles [])
  (for (item stage2-results)
    (set! stage3-handles (append stage3-handles [(async stage3-validate item)])))

  (define final-results [])
  (for (h stage3-handles)
    (set! final-results (append final-results [(await h)])))

  (println "\nPipeline results:")
  (for (r final-results)
    (println (str "  ID " (get r :id) ": " (get r :data) " (valid: " (get r :valid) ")")))

  final-results)
"#;

    match execute_ovsm(&mut evaluator, code) {
        Ok(_) => println!("✅ Pipeline completed\n"),
        Err(e) => println!("❌ Error: {}\n", e),
    }

    // Example 4: Fan-out/Fan-in pattern
    println!("Example 4: Fan-out/Fan-in Pattern");
    println!("─────────────────────────────────────");
    let code = r#"
(do
  (defun worker (worker-id tasks)
    (do
      (sleep (* worker-id 10))  ;; Varying delays
      (define completed 0)
      (for (task tasks)
        (set! completed (+ completed 1)))
      {:worker-id worker-id :completed completed}))

  ;; Fan-out: Distribute work to 4 workers
  (define num-workers 4)
  (define tasks-per-worker 25)

  (println (str "Fan-out: Distributing work to " num-workers " workers..."))

  (define handles [])
  (for (i (range 1 (+ num-workers 1)))
    (set! handles (append handles [(async worker i tasks-per-worker)])))

  ;; Fan-in: Collect results
  (println "Fan-in: Collecting results...")

  (define results [])
  (for (h handles)
    (set! results (append results [(await h)])))

  (define total-completed 0)
  (for (r results)
    (do
      (println (str "  Worker " (get r :worker-id) " completed " (get r :completed) " tasks"))
      (set! total-completed (+ total-completed (get r :completed)))))

  (println (str "\nTotal tasks completed: " total-completed))
  total-completed)
"#;

    match execute_ovsm(&mut evaluator, code) {
        Ok(_) => println!("✅ Fan-out/Fan-in completed\n"),
        Err(e) => println!("❌ Error: {}\n", e),
    }

    println!("═══════════════════════════════════════");
    println!("All data processing examples completed!");
}
