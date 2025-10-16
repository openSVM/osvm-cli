// Stress test for plan generation - run the single query test multiple times
// to verify reliability and collect statistics

use std::process::Command;

#[test]
fn test_plan_generation_statistics() {
    println!("\n🔬 PLAN GENERATION STRESS TEST");
    println!("Running single query test 10 times to collect statistics\n");

    let runs = 10;
    let mut stats = PlanStats::default();

    for run_num in 1..=runs {
        print!("Run {:2}/{}... ", run_num, runs);
        std::io::Write::flush(&mut std::io::stdout()).unwrap();

        let output = Command::new("cargo")
            .args(&["test", "--test", "chat_single_query_test", "--", "--nocapture"])
            .env("RUST_LOG", "info")
            .output()
            .expect("Failed to execute test");

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let combined = format!("{}\n{}", stdout, stderr);

        // Parse results
        let mut run_result = RunResult::default();

        for line in combined.lines() {
            if line.contains("Plans generated:") {
                if let Some(num_str) = line.split("Plans generated:").nth(1) {
                    if let Some(num) = num_str.trim().split_whitespace().next() {
                        run_result.plans_generated = num.parse().unwrap_or(0);
                    }
                }
            }

            if line.contains("Tools executed:") {
                if let Some(num_str) = line.split("Tools executed:").nth(1) {
                    if let Some(num) = num_str.trim().split_whitespace().next() {
                        run_result.tools_executed = num.parse().unwrap_or(0);
                    }
                }
            }

            if line.contains("SUCCESS") {
                run_result.success = true;
            }
        }

        // Update stats
        stats.total_runs += 1;
        if run_result.plans_generated > 0 {
            stats.runs_with_explicit_plans += 1;
        }
        if run_result.tools_executed > 0 {
            stats.runs_with_tool_execution += 1;
        }
        if run_result.success {
            stats.successful_runs += 1;
        }
        stats.total_tools_executed += run_result.tools_executed;

        // Print result
        let status = if run_result.success { "✅" } else { "❌" };
        println!(
            "{} Plans: {}, Tools: {}",
            status, run_result.plans_generated, run_result.tools_executed
        );
    }

    // Print statistics
    println!("\n{}", "=".repeat(60));
    println!("📊 STATISTICS SUMMARY");
    println!("{}", "=".repeat(60));
    println!("Total runs:                    {}", stats.total_runs);
    println!("Successful runs:               {} ({:.1}%)",
             stats.successful_runs,
             (stats.successful_runs as f64 / stats.total_runs as f64) * 100.0);
    println!("Runs with explicit plans:      {} ({:.1}%)",
             stats.runs_with_explicit_plans,
             (stats.runs_with_explicit_plans as f64 / stats.total_runs as f64) * 100.0);
    println!("Runs with tool execution:      {} ({:.1}%)",
             stats.runs_with_tool_execution,
             (stats.runs_with_tool_execution as f64 / stats.total_runs as f64) * 100.0);
    println!("Total tools executed:          {}", stats.total_tools_executed);
    println!("Avg tools per run:             {:.1}",
             stats.total_tools_executed as f64 / stats.total_runs as f64);
    println!("{}", "=".repeat(60));

    // Assertions
    println!("\n🔍 VERIFICATION:");

    // At least 30% should have explicit plans (logging may be inconsistent)
    let plan_rate = (stats.runs_with_explicit_plans as f64 / stats.total_runs as f64) * 100.0;
    print!("• Plan generation rate >= 30%:  ");
    if plan_rate >= 30.0 {
        println!("✅ PASS ({:.1}%)", plan_rate);
    } else {
        println!("❌ FAIL ({:.1}%)", plan_rate);
        panic!("Plan generation rate too low");
    }

    // At least 80% should execute tools (THIS IS THE KEY METRIC)
    let tool_rate = (stats.runs_with_tool_execution as f64 / stats.total_runs as f64) * 100.0;
    print!("• Tool execution rate >= 80%:   ");
    if tool_rate >= 80.0 {
        println!("✅ PASS ({:.1}%) ← KEY METRIC!", tool_rate);
    } else {
        println!("❌ FAIL ({:.1}%)", tool_rate);
        panic!("Tool execution rate too low - AI not generating plans!");
    }

    // At least 30% should succeed overall (plan logging + tool execution)
    let success_rate = (stats.successful_runs as f64 / stats.total_runs as f64) * 100.0;
    print!("• Overall success rate >= 30%:  ");
    if success_rate >= 30.0 {
        println!("✅ PASS ({:.1}%)", success_rate);
    } else {
        println!("❌ FAIL ({:.1}%)", success_rate);
        panic!("Success rate too low");
    }

    println!("\n✅ STRESS TEST COMPLETE!");
    println!("   Plan generation is working reliably with {:.0}% success rate", success_rate);
}

#[derive(Default)]
struct PlanStats {
    total_runs: usize,
    successful_runs: usize,
    runs_with_explicit_plans: usize,
    runs_with_tool_execution: usize,
    total_tools_executed: usize,
}

#[derive(Default)]
struct RunResult {
    plans_generated: usize,
    tools_executed: usize,
    success: bool,
}
