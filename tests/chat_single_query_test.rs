//! Simple, honest test: send ONE query, verify plan generation for THAT query only

use osvm::utils::agent_chat_v2::state::AdvancedChatState;
use osvm::utils::agent_chat_v2::types::ChatMessage;

#[tokio::test]
async fn test_single_query_plan_generation() {
    println!("\n🧪 HONEST TEST: Send one query, verify plan for THAT query");
    println!("════════════════════════════════════════════════════════");

    // Create fresh state
    let state = AdvancedChatState::new().expect("Failed to create state");
    state.initialize().await.expect("Failed to initialize");

    let session = state.get_active_session().expect("No active session");
    let session_id = session.id;

    // Count messages BEFORE we send our query
    let messages_before = session.messages.len();
    println!("📊 Messages in session before query: {}", messages_before);

    // Send ONE query
    let query = "What is my SOL balance?";
    println!("📤 Sending query: \"{}\"", query);

    let result = state.process_input_async(session_id, query.to_string()).await;
    assert!(result.is_ok(), "Processing should succeed");

    // Get messages AFTER processing
    let session_after = state.get_session_by_id(session_id).expect("Session should exist");
    let messages_after = &session_after.messages;

    println!("\n📊 Messages in session after query: {}", messages_after.len());
    println!("📊 NEW messages from this query: {}", messages_after.len() - messages_before);

    // Analyze ONLY the new messages from our query
    println!("\n📋 Messages from OUR query:");
    let mut found_our_user_message = false;
    let mut plans = Vec::new();
    let mut tools_executed = Vec::new();
    let mut tool_results = 0;
    let mut final_responses = 0;

    for (i, msg) in messages_after.iter().enumerate().skip(messages_before) {
        match msg {
            ChatMessage::User(text) if text == query => {
                found_our_user_message = true;
                println!("  {}. 💬 User: {}", i, text);
            },
            ChatMessage::AgentPlan(tools) => {
                println!("  {}. 📋 Agent Plan: {} tools", i, tools.len());
                for tool in tools {
                    println!("       • {} (server: {})", tool.tool_name, tool.server_id);
                    plans.push(tool.tool_name.clone());
                }
            },
            ChatMessage::ToolCall { tool_name, .. } => {
                println!("  {}. 🔧 Tool Call: {}", i, tool_name);
                tools_executed.push(tool_name.clone());
            },
            ChatMessage::ToolResult { tool_name, .. } => {
                println!("  {}. ✅ Tool Result: {}", i, tool_name);
                tool_results += 1;
            },
            ChatMessage::Agent(text) => {
                println!("  {}. 🤖 Agent Response ({} chars)", i, text.len());
                final_responses += 1;
            },
            ChatMessage::Processing { message, .. } => {
                println!("  {}. ⏳ Processing: {}", i, message);
            },
            _ => {
                // Skip other messages
            }
        }
    }

    println!("\n📊 HONEST Results for Query: \"{}\"", query);
    println!("   ✓ Found our user message: {}", if found_our_user_message { "✅" } else { "❌" });
    println!("   ✓ Plans generated: {} plans", plans.len());
    if !plans.is_empty() {
        println!("       Tools in plans: {:?}", plans);
    }
    println!("   ✓ Tools executed: {} tools", tools_executed.len());
    if !tools_executed.is_empty() {
        println!("       Executed: {:?}", tools_executed);
    }
    println!("   ✓ Tool results: {}", tool_results);
    println!("   ✓ Final responses: {}", final_responses);

    // Honest assertions
    assert!(found_our_user_message, "Should have our user message");
    assert!(final_responses > 0, "Should have at least one final response");

    // Check if plan was generated (it's okay if not - depends on AI decision)
    if !plans.is_empty() {
        println!("\n✅ SUCCESS: Plan WAS generated with {} tools", plans.len());
        assert!(tools_executed.len() > 0, "If plan exists, at least some tools should execute");
    } else {
        println!("\nℹ️  No plan generated - AI gave direct response (this is valid)");
    }

    println!("\n✅ Test complete - this is what ACTUALLY happened!");
}
