//! Advanced MCP integration scenario tests
//!
//! These tests demonstrate complex real-world blockchain analysis scenarios
//! that combine multiple RPC calls, data processing, and OVSM language features.
//!
//! Run with: `cargo test --package ovsm --test mcp_advanced_scenarios -- --ignored`

use ovsm::{Evaluator, Parser, Scanner, Value};

fn eval_lisp(source: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    Ok(evaluator.execute(&program)?)
}

// ====================
// Scenario 1: Wallet Activity Dashboard
// ====================

#[test]
#[ignore] // Requires live RPC
fn test_scenario_wallet_activity_dashboard() {
    let source = r#"
;; Create a comprehensive wallet activity dashboard
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define lookback 50)

;; Fetch recent activity
(define signatures (getSignaturesForAddress :address wallet :limit lookback))

;; Calculate statistics
(define total_txs (length signatures))

(define successful
  (filter (lambda (sig) (null? (. sig err))) signatures))
(define success_count (length successful))

(define failed
  (filter (lambda (sig) (not (null? (. sig err)))) signatures))
(define fail_count (length failed))

;; Success rate
(define success_rate
  (if (> total_txs 0)
      (* (/ success_count total_txs) 100.0)
      0.0))

;; Build dashboard object
{
  :wallet wallet
  :total_transactions total_txs
  :successful success_count
  :failed fail_count
  :success_rate success_rate
  :sample_size lookback
}
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Object(_)));
    if let Value::Object(dashboard) = result {
        assert!(dashboard.contains_key(&"wallet".to_string()));
        assert!(dashboard.contains_key(&"total_transactions".to_string()));
        assert!(dashboard.contains_key(&"success_rate".to_string()));
    }
}

// ====================
// Scenario 2: Transaction Time Analysis
// ====================

#[test]
#[ignore] // Requires live RPC
fn test_scenario_transaction_timing_analysis() {
    let source = r#"
;; Analyze transaction timing patterns
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define current_time (now))
(define hour_ago (- current_time 3600))
(define day_ago (- current_time 86400))

(define recent_sigs (getSignaturesForAddress :address wallet :limit 100))

;; Count by time period
(define last_hour
  (filter
    (lambda (sig) (>= (. sig blockTime) hour_ago))
    recent_sigs))

(define last_day
  (filter
    (lambda (sig) (>= (. sig blockTime) day_ago))
    recent_sigs))

{
  :last_hour_count (length last_hour)
  :last_day_count (length last_day)
  :total_sample (length recent_sigs)
  :hourly_rate (/ (length last_day) 24.0)
}
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Object(_)));
}

// ====================
// Scenario 3: Multi-Wallet Comparison
// ====================

#[test]
#[ignore] // Requires live RPC
fn test_scenario_multi_wallet_comparison() {
    let source = r#"
;; Compare activity across multiple wallets
(define wallets [
  "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r"
  "Vote111111111111111111111111111111111111111"
])

(define analyze_wallet
  (lambda (addr)
    (define sigs (getSignaturesForAddress :address addr :limit 20))
    (define successful (filter (lambda (s) (null? (. s err))) sigs))
    {
      :address addr
      :total (length sigs)
      :successful (length successful)
      :fail_rate (* (/ (- (length sigs) (length successful)) (length sigs)) 100.0)
    }))

(define results (map analyze_wallet wallets))

{
  :wallet_count (length wallets)
  :analysis results
}
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Object(_)));
}

// ====================
// Scenario 4: Transaction Slot Distribution
// ====================

#[test]
#[ignore] // Requires live RPC
fn test_scenario_slot_distribution_analysis() {
    let source = r#"
;; Analyze how transactions are distributed across slots
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define sigs (getSignaturesForAddress :address wallet :limit 50))

;; Extract slots
(define slots (map (lambda (sig) (. sig slot)) sigs))

;; Calculate slot spread (if we have multiple transactions)
(if (> (length slots) 1)
    (do
      (define first_slot (first slots))
      (define last_slot (last slots))
      (define slot_range (- first_slot last_slot))
      (define avg_slot_gap (/ slot_range (- (length slots) 1)))

      {
        :first_slot first_slot
        :last_slot last_slot
        :slot_range slot_range
        :tx_count (length slots)
        :avg_gap avg_slot_gap
      })
    {:tx_count (length slots) :insufficient_data true})
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Object(_)));
}

// ====================
// Scenario 5: Error Pattern Detection
// ====================

#[test]
#[ignore] // Requires live RPC
fn test_scenario_error_pattern_detection() {
    let source = r#"
;; Detect patterns in transaction errors
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define sigs (getSignaturesForAddress :address wallet :limit 100))

;; Separate successful and failed
(define failed_txs (filter (lambda (sig) (not (null? (. sig err)))) sigs))
(define failed_count (length failed_txs))
(define total_count (length sigs))

;; Calculate failure rate
(define failure_rate
  (if (> total_count 0)
      (* (/ failed_count total_count) 100.0)
      0.0))

;; Identify if failures are recent or historical
(define current_time (now))
(define recent_cutoff (- current_time 3600))

(define recent_failures
  (filter
    (lambda (sig) (>= (. sig blockTime) recent_cutoff))
    failed_txs))

{
  :total_analyzed total_count
  :total_failures failed_count
  :failure_rate failure_rate
  :recent_failures (length recent_failures)
  :historical_failures (- failed_count (length recent_failures))
}
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Object(_)));
}

// ====================
// Scenario 6: Batch Processing Pipeline
// ====================

#[test]
#[ignore] // Requires live RPC
fn test_scenario_batch_processing_pipeline() {
    let source = r#"
;; Process signatures in batches with transformation pipeline
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")

;; Stage 1: Fetch
(define raw_sigs (getSignaturesForAddress :address wallet :limit 30))

;; Stage 2: Filter (only successful)
(define filtered (filter (lambda (sig) (null? (. sig err))) raw_sigs))

;; Stage 3: Transform (extract key fields)
(define transformed
  (map
    (lambda (sig)
      {
        :signature (substring (. sig signature) 0 8)
        :slot (. sig slot)
        :time (. sig blockTime)
      })
    filtered))

;; Stage 4: Aggregate
{
  :input_count (length raw_sigs)
  :filtered_count (length filtered)
  :output_count (length transformed)
  :first_item (if (> (length transformed) 0) (first transformed) null)
}
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Object(_)));
}

// ====================
// Scenario 7: Pagination Workflow
// ====================

#[test]
#[ignore] // Requires live RPC
fn test_scenario_complete_pagination_workflow() {
    let source = r#"
;; Demonstrate complete pagination to fetch all available data
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define page_size 25)
(define max_pages 3)

;; Fetch first page
(define page1 (getSignaturesForAddress :address wallet :limit page_size))
(define page1_count (length page1))

;; Fetch second page if first page was full
(define page2
  (if (>= page1_count page_size)
      (do
        (define cursor (. (last page1) signature))
        (getSignaturesForAddress
          :address wallet
          :limit page_size
          :before cursor))
      []))

(define page2_count (length page2))

;; Calculate total fetched
(define total_fetched (+ page1_count page2_count))

{
  :page1_count page1_count
  :page2_count page2_count
  :total_fetched total_fetched
  :pagination_used (> page2_count 0)
}
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Object(_)));
}

// ====================
// Scenario 8: Real-time Monitoring Simulation
// ====================

#[test]
#[ignore] // Requires live RPC
fn test_scenario_realtime_monitoring() {
    let source = r#"
;; Simulate real-time monitoring by comparing snapshots
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")

;; Snapshot 1
(define snapshot1 (getSignaturesForAddress :address wallet :limit 5))
(define slot1 (getSlot))

;; Snapshot 2 (simulated delay)
(define snapshot2 (getSignaturesForAddress :address wallet :limit 5))
(define slot2 (getSlot))

;; Compare snapshots
(define sig1_first (if (> (length snapshot1) 0) (. (first snapshot1) signature) "none"))
(define sig2_first (if (> (length snapshot2) 0) (. (first snapshot2) signature) "none"))

{
  :slot_diff (- slot2 slot1)
  :new_activity (!= sig1_first sig2_first)
  :snapshot1_count (length snapshot1)
  :snapshot2_count (length snapshot2)
}
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Object(_)));
}

// ====================
// Scenario 9: Data Quality Validation
// ====================

#[test]
#[ignore] // Requires live RPC
fn test_scenario_data_quality_validation() {
    let source = r#"
;; Validate data quality of RPC responses
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define sigs (getSignaturesForAddress :address wallet :limit 10))

;; Validation checks
(define validate_signature
  (lambda (sig)
    (define has_sig (not (null? (. sig signature))))
    (define has_slot (not (null? (. sig slot))))
    (define has_time (not (null? (. sig blockTime))))
    (define slot_positive (> (. sig slot) 0))
    (define time_reasonable (> (. sig blockTime) 1600000000))

    (and has_sig (and has_slot (and has_time (and slot_positive time_reasonable))))))

;; Check all signatures
(define valid_sigs (filter validate_signature sigs))
(define quality_score (* (/ (length valid_sigs) (length sigs)) 100.0))

{
  :total_signatures (length sigs)
  :valid_signatures (length valid_sigs)
  :quality_score quality_score
  :all_valid (= quality_score 100.0)
}
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Object(_)));
}

// ====================
// Scenario 10: Performance Benchmarking
// ====================

#[test]
#[ignore] // Requires live RPC
fn test_scenario_performance_benchmark() {
    let source = r#"
;; Benchmark different query patterns
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")

;; Small batch
(define small_batch (getSignaturesForAddress :address wallet :limit 10))
(define small_count (length small_batch))

;; Medium batch
(define medium_batch (getSignaturesForAddress :address wallet :limit 50))
(define medium_count (length medium_batch))

;; Large batch
(define large_batch (getSignaturesForAddress :address wallet :limit 100))
(define large_count (length large_batch))

{
  :small_batch small_count
  :medium_batch medium_count
  :large_batch large_count
  :scaling_linear (and
    (<= small_count 10)
    (and (<= medium_count 50) (<= large_count 100)))
}
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Object(_)));
}
