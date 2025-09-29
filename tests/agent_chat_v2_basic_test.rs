//! Basic functionality tests for agent_chat_v2 refactored module
//!
//! These tests validate core functionality without complex UI testing

use anyhow::Result;
use std::time::Duration;
use uuid::Uuid;

// We'll use the existing structure since our module is properly integrated
use osvm::utils::agent_chat_v2::{
    types::{ChatMessage, AgentState},
    session::ChatSession,
};

#[test]
fn test_chat_message_serialization() -> Result<()> {
    // Test that our message types can be serialized/deserialized
    let messages = vec![
        ChatMessage::User("Hello".to_string()),
        ChatMessage::Agent("Hi there".to_string()),
        ChatMessage::System("System message".to_string()),
        ChatMessage::Error("Error message".to_string()),
        ChatMessage::Processing {
            message: "Processing...".to_string(),
            spinner_index: 0,
        },
    ];

    for msg in messages {
        let serialized = serde_json::to_string(&msg)?;
        let deserialized: ChatMessage = serde_json::from_str(&serialized)?;

        // Basic validation that serialization round-trip works
        match (&msg, &deserialized) {
            (ChatMessage::User(a), ChatMessage::User(b)) => assert_eq!(a, b),
            (ChatMessage::Agent(a), ChatMessage::Agent(b)) => assert_eq!(a, b),
            (ChatMessage::System(a), ChatMessage::System(b)) => assert_eq!(a, b),
            (ChatMessage::Error(a), ChatMessage::Error(b)) => assert_eq!(a, b),
            (ChatMessage::Processing { message: a, .. }, ChatMessage::Processing { message: b, .. }) => assert_eq!(a, b),
            _ => panic!("Message type mismatch after serialization"),
        }
    }

    Ok(())
}

#[test]
fn test_agent_state_transitions() {
    let states = vec![
        AgentState::Idle,
        AgentState::Thinking,
        AgentState::Planning,
        AgentState::ExecutingTool("test_tool".to_string()),
        AgentState::Waiting,
        AgentState::Paused,
        AgentState::Error("test error".to_string()),
    ];

    // Test that all states can be cloned and compared
    for state in &states {
        let cloned = state.clone();
        assert_eq!(state, &cloned);
    }

    // Test serialization
    for state in states {
        let serialized = serde_json::to_string(&state).unwrap();
        let deserialized: AgentState = serde_json::from_str(&serialized).unwrap();
        assert_eq!(state, deserialized);
    }
}

#[test]
fn test_chat_session_basic_functionality() -> Result<()> {
    let mut session = ChatSession::new("Test Session".to_string());

    // Test initial state
    assert_eq!(session.name, "Test Session");
    assert_eq!(session.messages.len(), 0);
    assert_eq!(session.agent_state, AgentState::Idle);
    assert!(!session.recording);

    // Test adding messages
    session.add_message(ChatMessage::User("Hello".to_string()));
    session.add_message(ChatMessage::Agent("Hi back".to_string()));

    assert_eq!(session.messages.len(), 2);

    // Test message content
    match &session.messages[0] {
        ChatMessage::User(text) => assert_eq!(text, "Hello"),
        _ => panic!("Expected User message"),
    }

    match &session.messages[1] {
        ChatMessage::Agent(text) => assert_eq!(text, "Hi back"),
        _ => panic!("Expected Agent message"),
    }

    Ok(())
}

#[test]
fn test_session_message_limit() -> Result<()> {
    let mut session = ChatSession::new("Stress Test".to_string());

    // Add more than 1000 messages to test cleanup
    for i in 0..1200 {
        session.add_message(ChatMessage::User(format!("Message {}", i)));
    }

    // Should be limited to 1000 messages
    assert_eq!(session.messages.len(), 1000);

    // Oldest messages should be removed (first 100 were dropped when we hit 1000 limit + 200 more)
    match &session.messages[0] {
        ChatMessage::User(text) => {
            // Should start from message 200 (since first 200 were removed)
            assert!(text.contains("Message 200"));
        },
        _ => panic!("Expected User message"),
    }

    // Latest message should still be there
    match &session.messages[999] {
        ChatMessage::User(text) => {
            assert!(text.contains("Message 1199"));
        },
        _ => panic!("Expected User message"),
    }

    Ok(())
}

#[test]
fn test_recording_file_creation() -> Result<()> {
    let mut session = ChatSession::new("Recording Test".to_string());
    let temp_file = format!("/tmp/test_recording_{}.log", session.id);

    // Start recording
    session.start_recording(temp_file.clone())?;
    assert!(session.recording);
    assert!(session.recording_file.is_some());

    // Add some messages
    session.add_message(ChatMessage::User("Test message 1".to_string()));
    session.add_message(ChatMessage::Agent("Response 1".to_string()));

    // Stop recording
    session.stop_recording();
    assert!(!session.recording);
    assert!(session.recording_file.is_none());

    // Verify file exists and has content
    assert!(std::path::Path::new(&temp_file).exists());
    let content = std::fs::read_to_string(&temp_file)?;
    assert!(content.contains("OSVM Agent Chat Session Recording"));
    assert!(content.contains("Test message 1"));
    assert!(content.contains("Recording started"));
    assert!(content.contains("Recording stopped"));

    // Cleanup
    std::fs::remove_file(temp_file)?;

    Ok(())
}

#[test]
fn test_session_id_uniqueness() {
    let mut session_ids = std::collections::HashSet::new();

    // Create multiple sessions and ensure IDs are unique
    for i in 0..100 {
        let session = ChatSession::new(format!("Session {}", i));
        assert!(session_ids.insert(session.id), "Duplicate session ID found");
    }

    assert_eq!(session_ids.len(), 100);
}

#[test]
fn test_message_types_comprehensive() {
    let test_value = serde_json::json!({"key": "value", "number": 42});

    let messages = vec![
        ChatMessage::ToolCall {
            tool_name: "test_tool".to_string(),
            description: "Test tool call".to_string(),
            args: Some(test_value.clone()),
            execution_id: "exec_123".to_string(),
        },
        ChatMessage::ToolResult {
            tool_name: "test_tool".to_string(),
            result: test_value.clone(),
            execution_id: "exec_123".to_string(),
        },
        ChatMessage::AgentThinking("I'm thinking...".to_string()),
        ChatMessage::AgentPlan(vec![]), // Empty plan for testing
    ];

    // Test that all message variants work
    for msg in messages {
        let serialized = serde_json::to_string(&msg).unwrap();
        let _deserialized: ChatMessage = serde_json::from_str(&serialized).unwrap();
        // If we get here without panic, serialization works
    }
}

#[test]
fn test_error_handling_edge_cases() -> Result<()> {
    let mut session = ChatSession::new("".to_string()); // Empty name

    // Should handle empty name gracefully
    assert_eq!(session.name, "");

    // Test recording with invalid path (should fail gracefully)
    let result = session.start_recording("/invalid/path/that/does/not/exist/file.log".to_string());
    assert!(result.is_err());
    assert!(!session.recording);

    // Session should still be usable after failed recording
    session.add_message(ChatMessage::User("Still works".to_string()));
    assert_eq!(session.messages.len(), 1);

    Ok(())
}

#[test]
fn test_concurrent_message_adding() -> Result<()> {
    use std::sync::{Arc, Mutex};
    use std::thread;

    let session = Arc::new(Mutex::new(ChatSession::new("Concurrent Test".to_string())));
    let mut handles = vec![];

    // Spawn multiple threads adding messages
    for i in 0..10 {
        let session_clone = session.clone();
        let handle = thread::spawn(move || {
            for j in 0..10 {
                let mut session = session_clone.lock().unwrap();
                session.add_message(ChatMessage::User(format!("Thread {} Message {}", i, j)));
            }
        });
        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().unwrap();
    }

    // Verify all messages were added
    let session = session.lock().unwrap();
    assert_eq!(session.messages.len(), 100); // 10 threads * 10 messages

    Ok(())
}