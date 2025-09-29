//! UI-specific integration tests for agent_chat_v2
//!
//! These tests validate the UI interactions and user workflows
//! using headless testing approach.

mod ui_test_utils;

use anyhow::Result;
use std::time::Duration;
use tokio::time::timeout;

use osvm::utils::agent_chat_v2::AgentState;
use ui_test_utils::HeadlessUI;

#[tokio::test]
async fn test_basic_ui_setup_and_display() -> Result<()> {
    let mut ui = HeadlessUI::new().await?;

    // Verify initial state
    let chat_names = ui.get_chat_names();
    assert_eq!(chat_names.len(), 1);
    assert!(chat_names[0].contains("Test Session"));

    // Verify agent state is idle
    let agent_state = ui.get_agent_state();
    assert!(matches!(agent_state, Some(AgentState::Idle)));

    Ok(())
}

#[tokio::test]
async fn test_chat_input_and_display() -> Result<()> {
    let mut ui = HeadlessUI::new().await?;

    // Send a test message
    ui.send_input("Hello, this is a test message");

    // Verify input was cleared
    assert_eq!(ui.get_input_content(), "");

    // Verify message appears in chat display
    let content = ui.get_chat_content();
    assert!(content.contains("Hello, this is a test message"));

    Ok(())
}

#[tokio::test]
async fn test_multi_session_workflow() -> Result<()> {
    let mut ui = HeadlessUI::new().await?;

    // Create additional chat sessions
    ui.create_new_chat("Analysis Session");
    ui.create_new_chat("Debug Session");

    // Verify all sessions exist
    let chat_names = ui.get_chat_names();
    assert_eq!(chat_names.len(), 3);

    // Switch between sessions
    ui.select_chat(1); // Select "Analysis Session"
    ui.send_input("Analysis message");

    ui.select_chat(2); // Select "Debug Session"
    ui.send_input("Debug message");

    ui.select_chat(0); // Back to original
    ui.send_input("Original session message");

    // Verify messages are in correct sessions
    // Note: This would require more sophisticated state inspection
    // as the UI content shows only the active session

    Ok(())
}

#[tokio::test]
async fn test_suggestions_functionality() -> Result<()> {
    let mut ui = HeadlessUI::new().await?;

    // Send a message that might trigger suggestions
    ui.send_input("What's my wallet balance?");

    // Wait a bit for processing
    tokio::time::sleep(Duration::from_millis(500)).await;

    // Check if suggestions are generated
    // Note: This depends on AI service being available
    let suggestions = ui.get_suggestions();
    println!("Generated suggestions: {:?}", suggestions);

    // Test pressing number keys for suggestions
    if !suggestions.is_empty() {
        ui.press_char('1'); // Should insert first suggestion
        let input_content = ui.get_input_content();
        // Verify suggestion was inserted (if suggestions were available)
        if ui.are_suggestions_visible() {
            assert!(!input_content.is_empty());
        }
    }

    Ok(())
}

#[tokio::test]
async fn test_agent_state_transitions() -> Result<()> {
    let mut ui = HeadlessUI::new().await?;

    // Send input that will trigger agent processing
    ui.send_input("Test agent processing");

    // Agent should transition through states
    // Note: Due to async nature, we might not catch all intermediate states
    let final_state = timeout(Duration::from_secs(10), async {
        ui.wait_for_agent_idle(5).await
    })
    .await;

    assert!(final_state.is_ok(), "Agent should return to idle state");

    Ok(())
}

#[tokio::test]
async fn test_error_handling_in_ui() -> Result<()> {
    let mut ui = HeadlessUI::new().await?;

    // Test sending empty input
    ui.send_input("");

    // Should not crash or cause issues
    assert_eq!(ui.get_input_content(), "");

    // Test very long input
    let long_input = "a".repeat(5000);
    ui.send_input(&long_input);

    // Should handle gracefully
    let content = ui.get_chat_content();
    assert!(content.len() > 0); // Should have some content

    Ok(())
}

#[tokio::test]
async fn test_recording_functionality_ui() -> Result<()> {
    let mut ui = HeadlessUI::new().await?;

    // Start recording through UI
    // Note: This would require implementing the recording button interaction
    // For now, test through direct state access
    let session_id = ui.state().get_active_session().unwrap().id;

    {
        let mut sessions = ui.state().sessions.write().unwrap();
        let session = sessions.get_mut(&session_id).unwrap();
        let temp_file = format!("/tmp/ui_test_recording_{}.log", session_id);
        session.start_recording(temp_file.clone()).unwrap();
    }

    // Send messages while recording
    ui.send_input("This message should be recorded");
    ui.send_input("Another recorded message");

    // Stop recording
    {
        let mut sessions = ui.state().sessions.write().unwrap();
        let session = sessions.get_mut(&session_id).unwrap();
        session.stop_recording();
    }

    // Verify content appears in chat
    let content = ui.get_chat_content();
    assert!(content.contains("This message should be recorded"));
    assert!(content.contains("Recording started"));

    Ok(())
}

#[tokio::test]
async fn test_keyboard_shortcuts() -> Result<()> {
    let mut ui = HeadlessUI::new().await?;

    // Send a message first
    ui.send_input("Test message for shortcuts");

    // Test Escape key (should hide suggestions if visible)
    ui.press_key(cursive::event::Key::Esc);

    // Suggestions should be hidden
    assert!(!ui.are_suggestions_visible());

    // Test other keyboard shortcuts
    // Note: Alt combinations are harder to test in headless mode
    // but the key bindings are registered during setup

    Ok(())
}

#[tokio::test]
async fn test_concurrent_ui_operations() -> Result<()> {
    let mut ui = HeadlessUI::new().await?;

    // Simulate rapid user interactions
    let messages = vec![
        "First rapid message",
        "Second rapid message",
        "Third rapid message",
        "Fourth rapid message",
        "Fifth rapid message",
    ];

    for msg in messages {
        ui.send_input(msg);
        // Small delay to simulate real typing
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    // All messages should be handled
    let content = ui.get_chat_content();
    assert!(content.contains("First rapid message"));
    assert!(content.contains("Fifth rapid message"));

    Ok(())
}

#[tokio::test]
async fn test_memory_pressure_ui() -> Result<()> {
    let mut ui = HeadlessUI::new().await?;

    // Send many messages to test memory limits
    for i in 0..100 {
        ui.send_input(&format!("Stress test message {}", i));

        // Occasionally step the UI
        if i % 10 == 0 {
            ui.run_steps(5);
        }
    }

    // UI should still be responsive
    ui.send_input("Final test message");

    let content = ui.get_chat_content();
    assert!(content.contains("Final test message"));

    // Check that session doesn't have too many messages (due to cleanup)
    let session = ui.state().get_active_session().unwrap();
    assert!(
        session.messages.len() <= 1000,
        "Messages should be cleaned up to prevent memory issues"
    );

    Ok(())
}

/// Integration test for full user workflow
#[tokio::test]
async fn test_complete_user_workflow() -> Result<()> {
    let mut ui = HeadlessUI::new().await?;

    // Step 1: User starts with default session
    assert_eq!(ui.get_chat_names().len(), 1);

    // Step 2: User sends initial query
    ui.send_input("Help me check my wallet balance");

    // Step 3: User creates a new session for different topic
    ui.create_new_chat("Technical Analysis");

    // Step 4: User switches to new session and asks different question
    ui.select_chat(1);
    ui.send_input("Show me transaction patterns");

    // Step 5: User switches back to original session
    ui.select_chat(0);
    ui.send_input("What was my previous balance query result?");

    // Step 6: User tests suggestions if available
    if ui.are_suggestions_visible() {
        ui.press_char('1');
        ui.press_key(cursive::event::Key::Enter);
    }

    // Verify workflow completed successfully
    assert_eq!(ui.get_chat_names().len(), 2);

    let content = ui.get_chat_content();
    assert!(content.contains("wallet balance") || content.contains("balance query"));

    Ok(())
}
