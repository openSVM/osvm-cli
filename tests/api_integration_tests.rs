/// Integration tests using REAL osvm.ai APIs (NO mocks)
/// These tests validate actual integration with:
/// 1. osvm.ai/api/getAnswer - AI planning endpoint
/// 2. osvm.ai/api/proxy/rpc - Solana RPC proxy endpoint

use serde_json::json;

mod api_helpers;
use api_helpers::{call_osvm_ai_api, call_rpc_proxy};

/// Helper to format planning responses nicely for debugging
#[allow(dead_code)]
fn format_planning_response(response: &str) -> String {
    format!("Planning Response:\n{}\n", response)
}

#[tokio::test]
async fn test_osvm_ai_api_get_answer() {
    println!("🧪 Testing osvm.ai/api/getAnswer endpoint...");

    let response = match call_osvm_ai_api("What is Solana?").await {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ API call failed: {}", e);
            panic!("Failed to call osvm.ai API: {}", e);
        }
    };

    println!("📊 Response status: {}", response.status);
    println!("📝 Response size: {} bytes", response.body.len());
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("📝 Response body:");
    println!("{}", response.body);
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    // Verify we got a successful HTTP response
    assert_eq!(response.status, 200, "Expected 200 status, got {}", response.status);

    // Verify the response contains meaningful content
    assert!(
        !response.body.is_empty(),
        "Response body should not be empty"
    );

    println!("✅ osvm.ai/api/getAnswer test PASSED");
}

#[tokio::test]
async fn test_rpc_proxy_get_health() {
    println!("🧪 Testing osvm.ai/api/proxy/rpc with getHealth...");

    let response = match call_rpc_proxy("getHealth", vec![]).await {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ RPC proxy call failed: {}", e);
            panic!("Failed to call RPC proxy: {}", e);
        }
    };

    println!("📊 Response status: {}", response.status);
    println!("📝 Response body: {}", response.body);

    // Verify we got a successful HTTP response
    assert_eq!(response.status, 200, "Expected 200 status, got {}", response.status);

    // Verify response contains valid JSON-RPC response
    assert!(
        response.body.contains("result") || response.body.contains("error"),
        "Response should contain JSON-RPC result or error field"
    );

    println!("✅ RPC proxy getHealth test PASSED");
}

#[tokio::test]
async fn test_rpc_proxy_get_balance() {
    println!("🧪 Testing osvm.ai/api/proxy/rpc with getBalance...");

    // Query balance of System Program
    let response = match call_rpc_proxy(
        "getBalance",
        vec![json!("11111111111111111111111111111111")],
    )
    .await
    {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ RPC proxy call failed: {}", e);
            panic!("Failed to call RPC proxy: {}", e);
        }
    };

    println!("📊 Response status: {}", response.status);
    println!("📝 Response body: {}", response.body);

    // Verify we got a successful HTTP response
    assert_eq!(response.status, 200, "Expected 200 status, got {}", response.status);

    // Verify response contains valid JSON-RPC response with balance
    assert!(
        response.body.contains("result") || response.body.contains("value"),
        "Response should contain balance information"
    );

    println!("✅ RPC proxy getBalance test PASSED");
}

#[tokio::test]
async fn test_rpc_proxy_get_slot() {
    println!("🧪 Testing osvm.ai/api/proxy/rpc with getSlot...");

    let response = match call_rpc_proxy("getSlot", vec![]).await {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ RPC proxy call failed: {}", e);
            panic!("Failed to call RPC proxy: {}", e);
        }
    };

    println!("📊 Response status: {}", response.status);
    println!("📝 Response body: {}", response.body);

    assert_eq!(response.status, 200);
    assert!(
        response.body.contains("result"),
        "Response should contain slot number"
    );

    println!("✅ RPC proxy getSlot test PASSED");
}

#[tokio::test]
async fn test_rpc_proxy_get_version() {
    println!("🧪 Testing osvm.ai/api/proxy/rpc with getVersion...");

    let response = match call_rpc_proxy("getVersion", vec![]).await {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ RPC proxy call failed: {}", e);
            panic!("Failed to call RPC proxy: {}", e);
        }
    };

    println!("📊 Response status: {}", response.status);
    println!("📝 Response body: {}", response.body);

    assert_eq!(response.status, 200);
    assert!(response.body.contains("solana-core"), "Should contain Solana version info");

    println!("✅ RPC proxy getVersion test PASSED");
}

#[tokio::test]
async fn test_integration_ai_then_rpc() {
    println!("🧪 Testing integration flow: AI planning → RPC query...");

    // Step 1: Get AI recommendation
    println!("  Step 1: Querying AI for deployment advice...");
    let plan = match call_osvm_ai_api("Should I deploy on mainnet or devnet? List pros and cons.")
        .await
    {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ AI API call failed: {}", e);
            panic!("Failed to call AI API: {}", e);
        }
    };

    assert_eq!(plan.status, 200, "AI API should return 200");
    println!("  ✓ AI response received: {} bytes", plan.body.len());
    println!("  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("  AI Response:");
    println!("  {}", plan.body);
    println!("  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    // Step 2: Check network health
    println!("  Step 2: Checking Solana network health via RPC proxy...");
    let health = match call_rpc_proxy("getHealth", vec![]).await {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ RPC proxy call failed: {}", e);
            panic!("Failed to call RPC proxy: {}", e);
        }
    };

    assert_eq!(health.status, 200, "RPC proxy should return 200");
    println!("  ✓ Network health response received: {} bytes", health.body.len());
    println!("  Health: {}", health.body);

    println!("✅ Integration flow test PASSED");
}

#[tokio::test]
async fn test_concurrent_api_calls() {
    println!("🧪 Testing concurrent API calls...");

    let futures = vec![
        tokio::spawn(call_osvm_ai_api("What are SVMs?")),
        tokio::spawn(call_rpc_proxy("getSlot", vec![])),
        tokio::spawn(call_rpc_proxy("getHealth", vec![])),
    ];

    let results = futures::future::join_all(futures).await;

    for (i, result) in results.iter().enumerate() {
        match result {
            Ok(Ok(response)) => {
                println!("  ✓ API call {} succeeded (status: {}, {} bytes)", i + 1, response.status, response.body.len());
                if i == 0 {
                    println!("  AI Response ({}): {}", i + 1, response.body);
                }
                assert_eq!(response.status, 200, "API {} should return 200", i + 1);
            }
            Ok(Err(e)) => {
                eprintln!("  ❌ API call {} failed: {}", i + 1, e);
                panic!("Concurrent API call {} failed: {}", i + 1, e);
            }
            Err(e) => {
                eprintln!("  ❌ Task {} failed: {}", i + 1, e);
                panic!("Task {} panicked: {}", i + 1, e);
            }
        }
    }

    println!("✅ Concurrent API calls test PASSED");
}

#[tokio::test]
async fn test_planning_validator_deployment() {
    println!("🎯 PLANNING TEST: Validator Deployment Strategy");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    let query = "I want to deploy a Solana validator on mainnet. What are the steps, requirements, and best practices?";
    println!("📋 Planning Query: {}\n", query);

    let response = match call_osvm_ai_api(query).await {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ Planning API call failed: {}", e);
            panic!("Failed to get deployment plan: {}", e);
        }
    };

    assert_eq!(response.status, 200);
    println!("📊 Status: {} OK", response.status);
    println!("📏 Response Size: {} bytes\n", response.body.len());

    println!("🤖 AI PLANNING RESPONSE:");
    println!("┌─────────────────────────────────────────────────────────────────────────┐");
    for line in response.body.lines() {
        println!("│ {:<71} │", line);
    }
    println!("└─────────────────────────────────────────────────────────────────────────┘\n");

    assert!(
        !response.body.is_empty(),
        "Planning response should not be empty"
    );

    println!("✅ Validator deployment planning test PASSED");
}

#[tokio::test]
async fn test_planning_network_selection() {
    println!("🎯 PLANNING TEST: Network Selection Decision");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    let query = "I'm building a Solana application. Should I start with devnet, testnet, or go straight to mainnet? What are the tradeoffs?";
    println!("📋 Planning Query: {}\n", query);

    let response = match call_osvm_ai_api(query).await {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ Planning API call failed: {}", e);
            panic!("Failed to get planning advice: {}", e);
        }
    };

    assert_eq!(response.status, 200);
    println!("📊 Status: {} OK", response.status);
    println!("📏 Response Size: {} bytes\n", response.body.len());

    println!("🤖 AI PLANNING RESPONSE:");
    println!("┌─────────────────────────────────────────────────────────────────────────┐");
    for line in response.body.lines() {
        println!("│ {:<71} │", line);
    }
    println!("└─────────────────────────────────────────────────────────────────────────┘\n");

    assert!(
        !response.body.is_empty(),
        "Planning response should not be empty"
    );

    println!("✅ Network selection planning test PASSED");
}

#[tokio::test]
async fn test_planning_rpc_setup() {
    println!("🎯 PLANNING TEST: RPC Node Setup Strategy");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    let query = "How do I set up and maintain a reliable RPC node for my Solana application? What hardware and configuration do I need?";
    println!("📋 Planning Query: {}\n", query);

    let response = match call_osvm_ai_api(query).await {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ Planning API call failed: {}", e);
            panic!("Failed to get RPC planning: {}", e);
        }
    };

    assert_eq!(response.status, 200);
    println!("📊 Status: {} OK", response.status);
    println!("📏 Response Size: {} bytes\n", response.body.len());

    println!("🤖 AI PLANNING RESPONSE:");
    println!("┌─────────────────────────────────────────────────────────────────────────┐");
    for line in response.body.lines() {
        println!("│ {:<71} │", line);
    }
    println!("└─────────────────────────────────────────────────────────────────────────┘\n");

    assert!(
        !response.body.is_empty(),
        "Planning response should not be empty"
    );

    println!("✅ RPC setup planning test PASSED");
}

#[tokio::test]
async fn test_planning_security_considerations() {
    println!("🎯 PLANNING TEST: Security Planning");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    let query = "What security considerations should I think about when deploying to Solana mainnet? How do I protect my keypair and funds?";
    println!("📋 Planning Query: {}\n", query);

    let response = match call_osvm_ai_api(query).await {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ Planning API call failed: {}", e);
            panic!("Failed to get security planning: {}", e);
        }
    };

    assert_eq!(response.status, 200);
    println!("📊 Status: {} OK", response.status);
    println!("📏 Response Size: {} bytes\n", response.body.len());

    println!("🤖 AI PLANNING RESPONSE:");
    println!("┌─────────────────────────────────────────────────────────────────────────┐");
    for line in response.body.lines() {
        println!("│ {:<71} │", line);
    }
    println!("└─────────────────────────────────────────────────────────────────────────┘\n");

    assert!(
        !response.body.is_empty(),
        "Planning response should not be empty"
    );

    println!("✅ Security planning test PASSED");
}

#[tokio::test]
async fn test_planning_performance_optimization() {
    println!("🎯 PLANNING TEST: Performance Optimization");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    let query = "My Solana validator is struggling with performance. What optimizations should I consider for CPU, memory, and network?";
    println!("📋 Planning Query: {}\n", query);

    let response = match call_osvm_ai_api(query).await {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("❌ Planning API call failed: {}", e);
            panic!("Failed to get performance planning: {}", e);
        }
    };

    assert_eq!(response.status, 200);
    println!("📊 Status: {} OK", response.status);
    println!("📏 Response Size: {} bytes\n", response.body.len());

    println!("🤖 AI PLANNING RESPONSE:");
    println!("┌─────────────────────────────────────────────────────────────────────────┐");
    for line in response.body.lines() {
        println!("│ {:<71} │", line);
    }
    println!("└─────────────────────────────────────────────────────────────────────────┘\n");

    assert!(
        !response.body.is_empty(),
        "Planning response should not be empty"
    );

    println!("✅ Performance optimization planning test PASSED");
}
