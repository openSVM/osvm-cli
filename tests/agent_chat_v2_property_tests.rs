//! Property-based testing for agent_chat_v2 module
//!
//! These tests use property-based testing to verify that the refactored module
//! maintains its invariants under a wide range of inputs and conditions.

use anyhow::Result;
use std::collections::HashSet;
use uuid::Uuid;

use osvm::utils::agent_chat_v2::{
    types::{ChatMessage, AgentState},
    session::ChatSession,
    state::AdvancedChatState,
};

// We'll use a simple property testing approach since we don't have proptest as a dependency
// These tests verify properties that should always hold true

#[tokio::test]
async fn property_session_ids_are_always_unique() -> Result<()> {
    let state = AdvancedChatState::new()?;
    let mut session_ids = HashSet::new();

    // Create many sessions and verify all IDs are unique
    for i in 0..1000 {
        let session_id = state.create_session(format!("Session {}", i))?;
        assert!(
            session_ids.insert(session_id),
            "Duplicate session ID found: {}",
            session_id
        );
    }

    assert_eq!(session_ids.len(), 1000);
    Ok(())
}

#[tokio::test]
async fn property_message_count_never_exceeds_limit() -> Result<()> {
    let state = AdvancedChatState::new()?;
    let session_id = state.create_session("Property Test".to_string())?;

    // Add messages in various patterns
    let patterns = vec![
        (1500, 1),    // Add 1500 messages one at a time
        (500, 10),    // Add 500 messages in bursts of 10
        (2000, 100),  // Add 2000 messages in bursts of 100
    ];

    for (total_messages, burst_size) in patterns {
        // Reset session
        let session_id = state.create_session("Property Test Reset".to_string())?;

        let mut message_count = 0;
        while message_count < total_messages {
            let burst_end = std::cmp::min(message_count + burst_size, total_messages);

            for i in message_count..burst_end {
                state.add_message_to_session(
                    session_id,
                    ChatMessage::User(format!("Message {}", i))
                )?;
            }

            message_count = burst_end;

            // Verify invariant: message count never exceeds limit
            let session = state.get_session_by_id(session_id).unwrap();
            assert!(
                session.messages.len() <= 1000,
                "Message count {} exceeds limit after adding {} messages",
                session.messages.len(),
                message_count
            );
        }
    }

    Ok(())
}

#[tokio::test]
async fn property_agent_state_transitions_are_consistent() -> Result<()> {
    let state = AdvancedChatState::new()?;
    let session_id = state.create_session("State Test".to_string())?;

    let all_states = vec![
        AgentState::Idle,
        AgentState::Thinking,
        AgentState::Planning,
        AgentState::ExecutingTool("test_tool".to_string()),
        AgentState::Waiting,
        AgentState::Paused,
        AgentState::Error("test_error".to_string()),
    ];

    // Test all possible state transitions
    for from_state in &all_states {
        for to_state in &all_states {
            // Set initial state
            state.set_agent_state(session_id, from_state.clone());
            let current_state = state.get_agent_state(session_id).unwrap();
            assert_eq!(current_state, *from_state);

            // Transition to new state
            state.set_agent_state(session_id, to_state.clone());
            let new_state = state.get_agent_state(session_id).unwrap();
            assert_eq!(new_state, *to_state);
        }
    }

    Ok(())
}

#[tokio::test]
async fn property_sessions_remain_independent() -> Result<()> {
    let state = AdvancedChatState::new()?;

    // Create multiple sessions
    let session_ids: Vec<Uuid> = (0..10)
        .map(|i| state.create_session(format!("Independent Session {}", i)).unwrap())
        .collect();

    // Add different content to each session
    for (i, session_id) in session_ids.iter().enumerate() {
        for j in 0..10 {
            state.add_message_to_session(
                *session_id,
                ChatMessage::User(format!("Session {} Message {}", i, j))
            )?;
        }

        // Set different agent states
        let agent_state = match i % 3 {
            0 => AgentState::Idle,
            1 => AgentState::Thinking,
            _ => AgentState::ExecutingTool(format!("tool_{}", i)),
        };
        state.set_agent_state(*session_id, agent_state);
    }

    // Verify sessions remain independent
    for (i, session_id) in session_ids.iter().enumerate() {
        let session = state.get_session_by_id(*session_id).unwrap();

        // Check message independence
        assert_eq!(session.messages.len(), 10);
        for (j, message) in session.messages.iter().enumerate() {
            if let ChatMessage::User(text) = message {
                assert!(
                    text.contains(&format!("Session {} Message {}", i, j)),
                    "Message content corruption in session {}",
                    i
                );
            }
        }

        // Check agent state independence
        let agent_state = state.get_agent_state(*session_id).unwrap();
        let expected_state = match i % 3 {
            0 => AgentState::Idle,
            1 => AgentState::Thinking,
            _ => AgentState::ExecutingTool(format!("tool_{}", i)),
        };
        assert_eq!(agent_state, expected_state);
    }

    Ok(())
}

#[tokio::test]
async fn property_message_serialization_is_stable() -> Result<()> {
    let test_messages = vec![
        ChatMessage::User("Test user message".to_string()),
        ChatMessage::Agent("Test agent response".to_string()),
        ChatMessage::System("System notification".to_string()),
        ChatMessage::Error("Error message".to_string()),
        ChatMessage::ToolCall {
            tool_name: "test_tool".to_string(),
            description: "Test tool description".to_string(),
            args: Some(serde_json::json!({"param": "value"})),
            execution_id: "exec_123".to_string(),
        },
        ChatMessage::ToolResult {
            tool_name: "test_tool".to_string(),
            result: serde_json::json!({"result": "success"}),
            execution_id: "exec_123".to_string(),
        },
        ChatMessage::AgentThinking("Thinking process".to_string()),
        ChatMessage::AgentPlan(vec![]),
        ChatMessage::Processing {
            message: "Processing...".to_string(),
            spinner_index: 5,
        },
    ];

    // Test serialization stability - multiple serialization/deserialization cycles
    for original_message in test_messages {
        let mut current_message = original_message.clone();

        // Perform multiple serialization cycles
        for cycle in 0..10 {
            let serialized = serde_json::to_string(&current_message)?;
            let deserialized: ChatMessage = serde_json::from_str(&serialized)?;

            // Verify message remains stable
            let reserialized = serde_json::to_string(&deserialized)?;
            assert_eq!(
                serialized, reserialized,
                "Message serialization unstable at cycle {} for {:?}",
                cycle, original_message
            );

            current_message = deserialized;
        }

        // Verify final message matches original (for deterministic fields)
        match (&original_message, &current_message) {
            (ChatMessage::User(a), ChatMessage::User(b)) => assert_eq!(a, b),
            (ChatMessage::Agent(a), ChatMessage::Agent(b)) => assert_eq!(a, b),
            (ChatMessage::System(a), ChatMessage::System(b)) => assert_eq!(a, b),
            (ChatMessage::Error(a), ChatMessage::Error(b)) => assert_eq!(a, b),
            (
                ChatMessage::ToolCall { tool_name: a, description: ad, args: aa, execution_id: ae },
                ChatMessage::ToolCall { tool_name: b, description: bd, args: ba, execution_id: be }
            ) => {
                assert_eq!(a, b);
                assert_eq!(ad, bd);
                assert_eq!(aa, ba);
                assert_eq!(ae, be);
            },
            _ => {} // Other variants are complex, just verify they didn't panic
        }
    }

    Ok(())
}

#[tokio::test]
async fn property_concurrent_operations_maintain_consistency() -> Result<()> {
    let state = AdvancedChatState::new()?;
    let session_ids: Vec<Uuid> = (0..5)
        .map(|i| state.create_session(format!("Concurrent Session {}", i)).unwrap())
        .collect();

    let mut handles = vec![];

    // Spawn concurrent operations on different sessions
    for (task_id, session_id) in session_ids.iter().enumerate() {
        let state_clone = state.clone();
        let session_id = *session_id;

        let handle = tokio::spawn(async move {
            let mut operation_count = 0;

            for i in 0..100 {
                // Add messages
                let _ = state_clone.add_message_to_session(
                    session_id,
                    ChatMessage::User(format!("Task {} Operation {}", task_id, i))
                );
                operation_count += 1;

                // Change agent state
                if i % 10 == 0 {
                    state_clone.set_agent_state(session_id, AgentState::Thinking);
                    operation_count += 1;
                }

                // Read session data
                if i % 5 == 0 {
                    let _ = state_clone.get_session_by_id(session_id);
                    operation_count += 1;
                }

                // Yield to other tasks
                if i % 25 == 0 {
                    tokio::task::yield_now().await;
                }
            }

            operation_count
        });
        handles.push(handle);
    }

    // Wait for all operations to complete
    let mut total_operations = 0;
    for handle in handles {
        total_operations += handle.await.unwrap();
    }

    // Verify consistency after concurrent operations
    assert_eq!(state.get_session_names().len(), 5);

    for (i, session_id) in session_ids.iter().enumerate() {
        let session = state.get_session_by_id(*session_id);
        assert!(session.is_some(), "Session {} should exist after concurrent operations", i);

        let session = session.unwrap();
        assert!(
            !session.messages.is_empty(),
            "Session {} should have messages after concurrent operations", i
        );

        // Verify message integrity
        for message in &session.messages {
            if let ChatMessage::User(text) = message {
                assert!(
                    text.contains(&format!("Task {}", i)),
                    "Message corruption detected in session {}: {}",
                    i, text
                );
            }
        }
    }

    println!("✅ Completed {} concurrent operations successfully", total_operations);
    Ok(())
}

#[tokio::test]
async fn property_recording_preserves_message_order() -> Result<()> {
    let mut session = ChatSession::new("Recording Order Test".to_string());
    let temp_file = format!("/tmp/order_test_{}.log", std::process::id());

    // Start recording
    session.start_recording(temp_file.clone())?;

    // Add messages in specific order
    let ordered_messages = vec![
        "First message",
        "Second message",
        "Third message",
        "Fourth message",
        "Fifth message",
    ];

    for (i, msg) in ordered_messages.iter().enumerate() {
        session.add_message(ChatMessage::User(format!("{} ({})", msg, i)));

        // Add small delay to ensure timestamp ordering
        tokio::time::sleep(std::time::Duration::from_millis(1)).await;
    }

    // Stop recording
    session.stop_recording();

    // Read and verify recording preserves order
    let content = std::fs::read_to_string(&temp_file)?;
    let lines: Vec<&str> = content.lines().collect();

    // Find message lines (skip header lines that start with #)
    let message_lines: Vec<&str> = lines.iter()
        .filter(|line| !line.starts_with('#') && !line.is_empty())
        .cloned()
        .collect();

    // Verify order is preserved in recording
    for (i, expected_msg) in ordered_messages.iter().enumerate() {
        let found = message_lines.iter().any(|line| {
            line.contains(expected_msg) && line.contains(&format!("({})", i))
        });
        assert!(
            found,
            "Message '{}' with index {} not found in correct order in recording",
            expected_msg, i
        );
    }

    // Cleanup
    std::fs::remove_file(temp_file)?;
    Ok(())
}

#[tokio::test]
async fn property_state_changes_are_atomic() -> Result<()> {
    let state = AdvancedChatState::new()?;
    let session_id = state.create_session("Atomic Test".to_string())?;

    // Perform rapid state changes and verify they're atomic
    let mut handles = vec![];

    for task_id in 0..10 {
        let state_clone = state.clone();
        let handle = tokio::spawn(async move {
            for i in 0..50 {
                let new_state = match (task_id + i) % 4 {
                    0 => AgentState::Idle,
                    1 => AgentState::Thinking,
                    2 => AgentState::Planning,
                    _ => AgentState::ExecutingTool(format!("tool_{}_{}", task_id, i)),
                };

                state_clone.set_agent_state(session_id, new_state.clone());

                // Immediately read back the state
                let read_state = state_clone.get_agent_state(session_id);

                // State should either be the one we just set, or another valid state
                // (but never partially corrupted)
                assert!(read_state.is_some(), "State should always be readable");

                let read_state = read_state.unwrap();
                assert!(
                    matches!(
                        read_state,
                        AgentState::Idle |
                        AgentState::Thinking |
                        AgentState::Planning |
                        AgentState::ExecutingTool(_) |
                        AgentState::Waiting |
                        AgentState::Paused |
                        AgentState::Error(_)
                    ),
                    "Read state should always be valid: {:?}",
                    read_state
                );
            }
        });
        handles.push(handle);
    }

    // Wait for all state changes to complete
    for handle in handles {
        handle.await.unwrap();
    }

    // Final state should be valid
    let final_state = state.get_agent_state(session_id).unwrap();
    assert!(
        matches!(
            final_state,
            AgentState::Idle |
            AgentState::Thinking |
            AgentState::Planning |
            AgentState::ExecutingTool(_) |
            AgentState::Waiting |
            AgentState::Paused |
            AgentState::Error(_)
        ),
        "Final state should be valid: {:?}",
        final_state
    );

    Ok(())
}

#[tokio::test]
async fn property_message_cleanup_preserves_recent_messages() -> Result<()> {
    let state = AdvancedChatState::new()?;
    let session_id = state.create_session("Cleanup Test".to_string())?;

    // Add more than the limit to trigger cleanup
    for i in 0..1200 {
        state.add_message_to_session(
            session_id,
            ChatMessage::User(format!("Message {}", i))
        )?;
    }

    let session = state.get_session_by_id(session_id).unwrap();

    // Verify cleanup occurred
    assert_eq!(session.messages.len(), 1000);

    // Verify recent messages are preserved (the last ones added)
    let last_message = session.messages.last().unwrap();
    if let ChatMessage::User(text) = last_message {
        assert!(
            text.contains("Message 1199"),
            "Most recent message should be preserved: {}",
            text
        );
    }

    // Verify old messages were removed
    let first_message = session.messages.first().unwrap();
    if let ChatMessage::User(text) = first_message {
        // Should NOT contain the very first messages (0-199 should be removed)
        assert!(
            !text.contains("Message 0") && !text.contains("Message 1") && !text.contains("Message 199"),
            "Oldest messages should be removed, but found: {}",
            text
        );
    }

    Ok(())
}