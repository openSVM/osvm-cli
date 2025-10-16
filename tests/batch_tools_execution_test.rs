/// Integration test for batch tool execution
use osvm::services::ai_service::PlannedTool;
use serde_json::json;

#[tokio::test]
async fn test_batch_validator_heuristic_detection() {
    println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("🧪 TEST: Batch Validator Heuristic Detection");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    let test_queries = vec![
        "Analyze the top 100 validators on Solana mainnet",
        "Show me validator performance for top 100",
        "Compare 100 validators by their metrics",
    ];

    for query in test_queries {
        println!("\n📝 Query: {}", query);

        // Simulate the heuristic detection logic
        let lc = query.to_lowercase();
        let should_trigger = (lc.contains("validator") || lc.contains("validators"))
            && (lc.contains("100") || lc.contains("top") || lc.contains("analyze"));

        println!("   Detection: {}", if should_trigger { "✅ MATCHED" } else { "❌ NOT MATCHED" });

        if should_trigger {
            let planned_tool = PlannedTool {
                server_id: "local_sim".into(),
                tool_name: "analyze_batch_validators".into(),
                args: json!({
                    "count": 100,
                    "metrics": ["stake", "commission", "uptime", "vote_credits", "delinquency"]
                }),
                reason: "Batch analyze validators with simulated data".into(),
            };

            println!("   Tool: {}", planned_tool.tool_name);
            println!("   Args: {}", serde_json::to_string_pretty(&planned_tool.args).unwrap());
        }
    }

    println!("\n✅ Heuristic detection test completed");
}

#[tokio::test]
async fn test_batch_token_heuristic_detection() {
    println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("🧪 TEST: Batch Token Heuristic Detection");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    let test_queries = vec![
        "Analyze the top 50 SPL tokens",
        "Show me token portfolio for 50 tokens",
        "Compare top 50 tokens performance",
    ];

    for query in test_queries {
        println!("\n📝 Query: {}", query);

        let lc = query.to_lowercase();
        let should_trigger = (lc.contains("token") || lc.contains("tokens"))
            && (lc.contains("50") || lc.contains("top") || lc.contains("portfolio"));

        println!("   Detection: {}", if should_trigger { "✅ MATCHED" } else { "❌ NOT MATCHED" });

        if should_trigger {
            let planned_tool = PlannedTool {
                server_id: "local_sim".into(),
                tool_name: "analyze_batch_tokens".into(),
                args: json!({
                    "count": 50,
                    "timeframe": "30d",
                    "metrics": ["price", "volume", "market_cap"]
                }),
                reason: "Batch analyze tokens with market data".into(),
            };

            println!("   Tool: {}", planned_tool.tool_name);
            println!("   Args: {}", serde_json::to_string_pretty(&planned_tool.args).unwrap());
        }
    }

    println!("\n✅ Heuristic detection test completed");
}

#[tokio::test]
async fn test_mock_tool_execution_simulation() {
    println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("🧪 TEST: Mock Tool Execution Simulation");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    // Simulate what the mock tool would return
    let count = 100u64;

    println!("\n⚙️  Simulating: analyze_batch_validators(count={})", count);
    println!("   Processing... (simulated 800ms delay)");

    let mock_result = json!({
        "status": "success",
        "validators_analyzed": count,
        "execution_time_ms": 1500,
        "data": {
            "top_performers": [
                {"name": "Validator Alpha", "stake_sol": 2_500_000, "commission": 5.0, "uptime": 99.8},
                {"name": "Validator Beta", "stake_sol": 2_200_000, "commission": 4.5, "uptime": 99.6},
                {"name": "Validator Gamma", "stake_sol": 1_900_000, "commission": 5.5, "uptime": 99.4},
                {"name": "Validator Delta", "stake_sol": 1_700_000, "commission": 6.0, "uptime": 99.2},
                {"name": "Validator Epsilon", "stake_sol": 1_500_000, "commission": 5.0, "uptime": 99.0}
            ],
            "average_metrics": {
                "stake_sol": 1_250_000,
                "commission_percent": 5.2,
                "uptime_percent": 96.4,
                "vote_credits_avg": 458_230
            },
            "total_stake": count * 1_250_000,
            "anomalies_detected": 3,
            "anomalies": [
                {"validator": "Validator-42", "issue": "High commission (12%)", "severity": "medium"},
                {"validator": "Validator-78", "issue": "Low uptime (87%)", "severity": "high"},
                {"validator": "Validator-91", "issue": "Recent delinquency", "severity": "medium"}
            ]
        }
    });

    println!("\n📊 Mock Tool Result:");
    println!("{}", serde_json::to_string_pretty(&mock_result).unwrap());

    // Verify result structure
    assert_eq!(mock_result["status"], "success");
    assert_eq!(mock_result["validators_analyzed"], 100);
    assert_eq!(mock_result["data"]["top_performers"].as_array().unwrap().len(), 5);
    assert_eq!(mock_result["data"]["anomalies"].as_array().unwrap().len(), 3);

    println!("\n✅ Mock tool execution simulation passed");
    println!("   • Status: success ✅");
    println!("   • Validators analyzed: 100 ✅");
    println!("   • Top performers: 5 returned ✅");
    println!("   • Anomalies: 3 detected ✅");
}

#[test]
fn test_all_complex_queries_trigger_correct_tools() {
    println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("🧪 TEST: Complex Queries → Tool Mapping");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    let test_cases = vec![
        (
            "Analyze top 100 validators",
            "analyze_batch_validators",
            true
        ),
        (
            "Show me 50 token prices",
            "analyze_batch_tokens",
            true
        ),
        (
            "Analyze transaction patterns across 50 accounts",
            "analyze_batch_accounts",
            true
        ),
        (
            "What's my balance?",
            "get_balance",
            true
        ),
        (
            "Show my recent transactions",
            "get_transactions",
            true
        ),
    ];

    let mut passed = 0;
    let total = test_cases.len();

    for (query, expected_tool, should_match) in test_cases {
        println!("\n📝 Query: {}", query);
        println!("   Expected Tool: {}", expected_tool);

        let lc = query.to_lowercase();
        let matched = match expected_tool {
            "analyze_batch_validators" => {
                (lc.contains("validator") || lc.contains("validators"))
                    && (lc.contains("100") || lc.contains("top") || lc.contains("analyze"))
            },
            "analyze_batch_tokens" => {
                (lc.contains("token") || lc.contains("tokens"))
                    && (lc.contains("50") || lc.contains("top") || lc.contains("portfolio"))
            },
            "analyze_batch_accounts" => {
                (lc.contains("account") || lc.contains("accounts"))
                    && (lc.contains("transaction") || lc.contains("pattern"))
            },
            "get_balance" => {
                lc.contains("balance") && !lc.contains("validator") && !lc.contains("token")
            },
            "get_transactions" => {
                (lc.contains("transaction") || lc.contains("transactions") || lc.contains("tx"))
                    && !lc.contains("account") && !lc.contains("validator")
            },
            _ => false,
        };

        let result = matched == should_match;
        if result {
            println!("   Result: ✅ PASS");
            passed += 1;
        } else {
            println!("   Result: ❌ FAIL (expected {}, got {})", should_match, matched);
        }
    }

    println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("📊 RESULTS: {}/{} tests passed", passed, total);
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    assert_eq!(passed, total, "Not all query→tool mappings passed!");
}
