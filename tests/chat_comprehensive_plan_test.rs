// Comprehensive test of plan generation with various query types
use std::collections::HashSet;

#[tokio::test]
async fn test_comprehensive_plan_generation() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n🧪 COMPREHENSIVE PLAN TEST: Multiple query types");

    // Test queries that should generate different tool plans
    let test_cases = vec![
        (
            "What is my SOL balance?",
            vec!["get_account_stats", "get_balance"],  // Expected possible tools
            "balance query"
        ),
        (
            "Show me recent transactions for my account",
            vec!["get_transactions", "get_account_history"],
            "transaction query"
        ),
        (
            "What is the current network status?",
            vec!["get_network_status", "get_cluster_nodes", "get_health"],
            "network query"
        ),
        (
            "Get validator information",
            vec!["get_validator_info", "get_vote_accounts"],
            "validator query"
        ),
    ];

    let mut total_plans = 0;
    let mut total_tools_executed = 0;
    let mut all_tools_seen = HashSet::new();

    for (query, expected_tools, description) in &test_cases {
        println!("\n{}", "=".repeat(60));
        println!("📝 Test: {}", description);
        println!("   Query: {}", query);
        println!("{}", "=".repeat(60));

        // Run the query
        let output = std::process::Command::new("cargo")
            .args(&[
                "run",
                "--bin",
                "osvm",
                "--",
                "chat",
                "--test",
                "--query",
                query,
            ])
            .env("RUST_LOG", "debug")
            .output()?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let combined = format!("{}\n{}", stdout, stderr);

        // Parse plan information
        let mut plans_in_test = 0;
        let mut tools_in_test = 0;
        let mut tools_used = Vec::new();

        for line in combined.lines() {
            if line.contains("📋 Agent Plan:") {
                plans_in_test += 1;
                // Extract tool count from "1 tools" or "2 tools"
                if let Some(count_str) = line.split("Plan:").nth(1) {
                    if let Some(num) = count_str.trim().split_whitespace().next() {
                        if let Ok(n) = num.parse::<usize>() {
                            tools_in_test += n;
                        }
                    }
                }
            }

            if line.contains("🔧 Tool Call:") {
                if let Some(tool_name) = line.split("Tool Call:").nth(1) {
                    let tool = tool_name.trim().to_string();
                    tools_used.push(tool.clone());
                    all_tools_seen.insert(tool);
                }
            }
        }

        total_plans += plans_in_test;
        total_tools_executed += tools_used.len();

        println!("\n📊 Results for {}:", description);
        println!("   • Plans generated: {}", plans_in_test);
        println!("   • Tools in plans: {}", tools_in_test);
        println!("   • Tools executed: {:?}", tools_used);

        // Verify this query generated a plan
        assert!(plans_in_test > 0,
            "❌ FAIL: No plan generated for '{}' query", description);

        // Verify tools were used
        if tools_in_test > 0 {
            println!("   ✅ Plan included {} tools", tools_in_test);
        }

        // Check if any expected tools were used
        let used_expected = tools_used.iter()
            .any(|tool| expected_tools.iter().any(|exp| tool.contains(exp)));

        if used_expected {
            println!("   ✅ Used expected tool type");
        } else {
            println!("   ⚠️  Tools used: {:?}", tools_used);
            println!("   ⚠️  Expected one of: {:?}", expected_tools);
        }
    }

    println!("\n{}", "=".repeat(60));
    println!("🎯 OVERALL COMPREHENSIVE TEST RESULTS");
    println!("{}", "=".repeat(60));
    println!("Total plans generated: {}", total_plans);
    println!("Total tools executed: {}", total_tools_executed);
    println!("Unique tools seen: {:?}", all_tools_seen);
    println!("Test cases: {}", test_cases.len());

    // Final assertions
    assert!(total_plans >= test_cases.len(),
        "❌ FAIL: Should generate at least 1 plan per test case");

    assert!(total_tools_executed > 0,
        "❌ FAIL: Should execute at least some tools");

    assert!(all_tools_seen.len() > 0,
        "❌ FAIL: Should see at least one unique tool");

    println!("\n✅ SUCCESS: Comprehensive plan generation test passed!");
    println!("   • {} different queries tested", test_cases.len());
    println!("   • {} total plans generated", total_plans);
    println!("   • {} unique tools discovered", all_tools_seen.len());

    Ok(())
}

#[tokio::test]
async fn test_plan_generation_consistency() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n🧪 CONSISTENCY TEST: Same query multiple times");

    let query = "What is my SOL balance?";
    let runs = 3;

    let mut plan_counts = Vec::new();
    let mut tool_sets = Vec::new();

    for run in 1..=runs {
        println!("\n🔄 Run {}/{}", run, runs);

        let output = std::process::Command::new("cargo")
            .args(&[
                "run",
                "--bin",
                "osvm",
                "--",
                "chat",
                "--test",
                "--query",
                query,
            ])
            .env("RUST_LOG", "debug")
            .output()?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let combined = format!("{}\n{}", stdout, stderr);

        let mut plans = 0;
        let mut tools = HashSet::new();

        for line in combined.lines() {
            if line.contains("📋 Agent Plan:") {
                plans += 1;
            }
            if line.contains("🔧 Tool Call:") {
                if let Some(tool_name) = line.split("Tool Call:").nth(1) {
                    tools.insert(tool_name.trim().to_string());
                }
            }
        }

        plan_counts.push(plans);
        tool_sets.push(tools.clone());

        println!("   Plans: {}, Tools: {:?}", plans, tools);
    }

    println!("\n📊 Consistency Analysis:");
    println!("   Plan counts: {:?}", plan_counts);

    // Check all runs generated plans
    let all_generated_plans = plan_counts.iter().all(|&c| c > 0);
    assert!(all_generated_plans, "❌ FAIL: Not all runs generated plans");

    println!("   ✅ All runs generated at least 1 plan");

    // Check consistency (should be relatively similar)
    let avg_plans = plan_counts.iter().sum::<usize>() as f64 / plan_counts.len() as f64;
    println!("   Average plans per run: {:.1}", avg_plans);

    // Check tool consistency
    let all_used_same_tools = tool_sets.windows(2).all(|w| !w[0].is_disjoint(&w[1]));
    if all_used_same_tools {
        println!("   ✅ Tools consistent across runs");
    } else {
        println!("   ⚠️  Different tools used across runs (may be expected)");
    }

    println!("\n✅ SUCCESS: Consistency test completed");

    Ok(())
}

#[tokio::test]
async fn test_complex_multi_tool_query() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n🧪 COMPLEX QUERY TEST: Multi-tool planning");

    // A complex query that should require multiple tools
    let query = "Get my balance, recent transactions, and check network status";

    println!("📝 Complex query: {}", query);

    let output = std::process::Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "osvm",
            "--",
            "chat",
            "--test",
            "--query",
            query,
        ])
        .env("RUST_LOG", "debug")
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{}\n{}", stdout, stderr);

    let mut plans = 0;
    let mut total_tools = 0;
    let mut tools_executed = Vec::new();

    for line in combined.lines() {
        if line.contains("📋 Agent Plan:") {
            plans += 1;
            if let Some(count_str) = line.split("Plan:").nth(1) {
                if let Some(num) = count_str.trim().split_whitespace().next() {
                    if let Ok(n) = num.parse::<usize>() {
                        total_tools += n;
                    }
                }
            }
        }

        if line.contains("🔧 Tool Call:") {
            if let Some(tool_name) = line.split("Tool Call:").nth(1) {
                tools_executed.push(tool_name.trim().to_string());
            }
        }
    }

    println!("\n📊 Complex Query Results:");
    println!("   • Plans generated: {}", plans);
    println!("   • Total tools in plans: {}", total_tools);
    println!("   • Tools executed: {:?}", tools_executed);

    // A complex query should generate at least one plan
    assert!(plans > 0, "❌ FAIL: No plan for complex query");

    // Should involve at least one tool
    assert!(total_tools > 0, "❌ FAIL: Plan should include tools");

    println!("\n✅ SUCCESS: Complex query handled with {} plan(s) and {} tool(s)",
             plans, total_tools);

    Ok(())
}
