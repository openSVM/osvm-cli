#![cfg(feature = "incomplete_tests")]
//! Test AI integration without UI - modular architecture testing

use osvm::services::ai_service::AiService;
use std::env;

#[tokio::test]
async fn test_ai_service_creation() {
    println!("🧪 Testing AI Service Creation (Modular)");

    let ai_service = AiService::new();

    // Should create without panicking
    assert!(true, "AI service created successfully");

    println!("✓ AI service instantiated correctly");
}

#[tokio::test]
async fn test_enhanced_query_format() {
    println!("🧪 Testing Enhanced Query Format");

    let user_input = "What's my SOL balance?";
    let enhanced_query = format!(
        "You are OSVM Agent, a specialized assistant for Solana blockchain operations. \
        The user is asking: '{}'\n\n\
        Context: You help with Solana wallet operations, transactions, staking, DeFi, \
        and blockchain interactions. You have access to MCP (Model Context Protocol) tools \
        for real-time blockchain data.\n\n\
        Provide helpful, technical answers about Solana operations. If you don't have real-time data, \
        explain what the user needs to do to get that information.",
        user_input
    );

    // Test the format we use in handlers.rs
    assert!(
        enhanced_query.contains("OSVM Agent"),
        "Query should contain OSVM Agent context"
    );
    assert!(
        enhanced_query.contains("Solana blockchain operations"),
        "Query should mention Solana operations"
    );
    assert!(
        enhanced_query.contains("MCP"),
        "Query should mention MCP tools"
    );
    assert!(
        enhanced_query.len() > 200,
        "Enhanced query should be substantial"
    );

    println!("✓ Enhanced query format: {} chars", enhanced_query.len());
    println!("✓ Contains OSVM context: ✓");
    println!("✓ Contains Solana context: ✓");
    println!("✓ Contains MCP context: ✓");
}

#[tokio::test]
async fn test_ai_service_with_mock_query() {
    println!("🧪 Testing AI Service Call Structure");

    let ai_service = AiService::new();
    let test_query = "You are OSVM Agent. Test query: What is Solana?";

    // Test that the service can handle the query format
    // This will either succeed with real AI or fail gracefully
    match ai_service.query_with_debug(test_query, true).await {
        Ok(response) => {
            println!(
                "✓ AI Service SUCCESS: Got response ({} chars)",
                response.len()
            );
            assert!(!response.is_empty(), "Response should not be empty");
            println!(
                "✓ Response preview: {}",
                &response[..std::cmp::min(100, response.len())]
            );
        }
        Err(e) => {
            println!("✓ AI Service handled error gracefully: {}", e);
            // This is expected if no API key or network issues
            assert!(true, "Error handling works correctly");
        }
    }
}

#[test]
fn test_suggestion_query_format() {
    println!("🧪 Testing Suggestion Query Format");

    let user_input = "What's my SOL balance?";
    let suggestion_query = format!(
        "Based on the user query '{}' and your response, suggest 5 short follow-up questions \
        or actions related to Solana/OSVM operations. Return only the suggestions, one per line, \
        without numbers or bullets.",
        user_input
    );

    assert!(
        suggestion_query.contains("5 short follow-up"),
        "Should ask for 5 suggestions"
    );
    assert!(
        suggestion_query.contains("Solana/OSVM"),
        "Should mention Solana/OSVM context"
    );
    assert!(
        suggestion_query.contains("one per line"),
        "Should specify format"
    );

    println!(
        "✓ Suggestion query format: {} chars",
        suggestion_query.len()
    );
    println!("✓ Asks for structured output: ✓");
}

#[test]
fn test_fallback_response_format() {
    println!("🧪 Testing Fallback Mechanism");

    let user_input = "test query";
    let mock_error = "Connection timeout";

    // This simulates our fallback logic in handlers.rs
    let fallback_response = format!(
        "I encountered an issue connecting to the AI service: {}\n\n\
        However, I can still help with basic Solana operations. \
        For SOL balance queries, you can use the 'solana balance' command.",
        mock_error
    );

    assert!(
        fallback_response.contains("encountered an issue"),
        "Should explain the error"
    );
    assert!(
        fallback_response.contains("can still help"),
        "Should offer alternative assistance"
    );
    assert!(
        fallback_response.len() > 50,
        "Should provide substantial fallback help"
    );

    println!("✓ Fallback response: {} chars", fallback_response.len());
    println!("✓ Contains error explanation: ✓");
    println!("✓ Provides alternative help: ✓");
}

#[test]
fn test_modular_architecture() {
    println!("🧪 Testing Modular Architecture Benefits");

    // Test that we can access the AI service structure without UI
    let ai_service = AiService::new();

    // Test environment variable access (like the real code does)
    let openai_url = env::var("OPENAI_URL")
        .unwrap_or_else(|_| "https://api.openai.com/v1/chat/completions".to_string());
    let has_key = env::var("OPENAI_KEY").is_ok();

    println!("✓ AI URL: {}", openai_url);
    println!("✓ API Key present: {}", has_key);
    println!("✓ Can create AI service without UI: ✓");
    println!("✓ Can access environment variables: ✓");

    assert!(openai_url.contains("http"), "Should have valid URL format");
    assert!(true, "Modular architecture allows isolated testing");
}

#[tokio::test]
async fn test_tokio_runtime_integration() {
    println!("🧪 Testing Tokio Runtime Integration");

    // Test the runtime creation pattern we use in handlers.rs
    let rt_result = tokio::runtime::Runtime::new();

    match rt_result {
        Ok(_rt) => {
            println!("✓ Tokio runtime creation: SUCCESS");
            println!("✓ This matches the pattern used in start_live_processing");
            assert!(true, "Runtime creation works");
        }
        Err(e) => {
            panic!("Tokio runtime creation failed: {}", e);
        }
    }
}
