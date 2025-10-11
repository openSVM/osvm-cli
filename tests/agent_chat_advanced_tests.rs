//! Advanced tests for agent chat UI and state management
//!
//! Note: These tests treat ChatMessage as a struct but it's actually an enum.
//! The actual ChatMessage is defined in src/utils/agent_chat_v2/types.rs as an enum.
//!
//! These tests are ignored until they can be properly rewritten.

#[cfg(all(test, feature = "incomplete_tests"))]
mod session_management_tests {
    use super::*;

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_session_creation() -> Result<()> {
        let mut state = ChatState::new()?;

        let session_id = state.create_session("Test Session".to_string())?;
        assert_ne!(session_id, Uuid::nil());

        let sessions = state.list_sessions()?;
        assert_eq!(sessions.len(), 1);
        assert_eq!(sessions[0].name, "Test Session");

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_multiple_sessions() -> Result<()> {
        let mut state = ChatState::new()?;

        let session1 = state.create_session("Session 1".to_string())?;
        let session2 = state.create_session("Session 2".to_string())?;
        let session3 = state.create_session("Session 3".to_string())?;

        assert_ne!(session1, session2);
        assert_ne!(session2, session3);

        let sessions = state.list_sessions()?;
        assert_eq!(sessions.len(), 3);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_session_deletion() -> Result<()> {
        let mut state = ChatState::new()?;

        let session1 = state.create_session("Keep".to_string())?;
        let session2 = state.create_session("Delete".to_string())?;

        state.delete_session(session2)?;

        let sessions = state.list_sessions()?;
        assert_eq!(sessions.len(), 1);
        assert_eq!(sessions[0].id, session1);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_active_session_switching() -> Result<()> {
        let mut state = ChatState::new()?;

        let session1 = state.create_session("Session 1".to_string())?;
        let session2 = state.create_session("Session 2".to_string())?;

        state.set_active_session(session1)?;
        assert_eq!(state.get_active_session(), Some(session1));

        state.set_active_session(session2)?;
        assert_eq!(state.get_active_session(), Some(session2));

        Ok(())
    }
}

#[cfg(all(test, feature = "incomplete_tests"))]
mod message_history_tests {
    use super::*;

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_message_adding() -> Result<()> {
        let mut state = ChatState::new()?;
        let session_id = state.create_session("Test".to_string())?;

        state.add_message(
            session_id,
            ChatMessage {
                role: "user".to_string(),
                content: "Hello".to_string(),
                timestamp: Utc::now(),
                tool_calls: None,
            },
        )?;

        state.add_message(
            session_id,
            ChatMessage {
                role: "assistant".to_string(),
                content: "Hi there!".to_string(),
                timestamp: Utc::now(),
                tool_calls: None,
            },
        )?;

        let messages = state.get_messages(session_id)?;
        assert_eq!(messages.len(), 2);
        assert_eq!(messages[0].role, "user");
        assert_eq!(messages[1].role, "assistant");

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_message_history_limit() -> Result<()> {
        let mut state = ChatState::new()?;
        let session_id = state.create_session("Test".to_string())?;

        // Add many messages
        for i in 0..1500 {
            state.add_message(
                session_id,
                ChatMessage {
                    role: if i % 2 == 0 { "user" } else { "assistant" },
                    content: format!("Message {}", i),
                    timestamp: Utc::now(),
                    tool_calls: None,
                },
            )?;
        }

        let messages = state.get_messages(session_id)?;

        // Should be limited to 1000 most recent messages
        assert!(messages.len() <= 1000);

        // Should have most recent messages
        assert!(messages.last().unwrap().content.contains("1499"));

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_message_with_tool_calls() -> Result<()> {
        let mut state = ChatState::new()?;
        let session_id = state.create_session("Tool Test".to_string())?;

        state.add_message(
            session_id,
            ChatMessage {
                role: "assistant".to_string(),
                content: "I'll check your balance".to_string(),
                timestamp: Utc::now(),
                tool_calls: Some(vec![
                    "get_balance".to_string(),
                    "get_recent_transactions".to_string(),
                ]),
            },
        )?;

        let messages = state.get_messages(session_id)?;
        assert_eq!(messages.len(), 1);

        let tool_calls = messages[0].tool_calls.as_ref().unwrap();
        assert_eq!(tool_calls.len(), 2);
        assert_eq!(tool_calls[0], "get_balance");

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_message_ordering() -> Result<()> {
        let mut state = ChatState::new()?;
        let session_id = state.create_session("Order Test".to_string())?;

        let timestamps = vec![
            Utc::now(),
            Utc::now() + chrono::Duration::seconds(1),
            Utc::now() + chrono::Duration::seconds(2),
        ];

        for (i, ts) in timestamps.iter().enumerate() {
            state.add_message(
                session_id,
                ChatMessage {
                    role: "user".to_string(),
                    content: format!("Message {}", i),
                    timestamp: *ts,
                    tool_calls: None,
                },
            )?;
        }

        let messages = state.get_messages(session_id)?;

        // Messages should be in chronological order
        for i in 1..messages.len() {
            assert!(messages[i].timestamp >= messages[i - 1].timestamp);
        }

        Ok(())
    }
}

#[cfg(all(test, feature = "incomplete_tests"))]
mod agent_state_tests {
    use super::*;

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_agent_state_transitions() -> Result<()> {
        let mut state = ChatState::new()?;
        let session_id = state.create_session("State Test".to_string())?;

        // Test state transitions
        state.set_agent_state(session_id, AgentState::Idle)?;
        assert_eq!(state.get_agent_state(session_id)?, AgentState::Idle);

        state.set_agent_state(session_id, AgentState::Thinking)?;
        assert_eq!(state.get_agent_state(session_id)?, AgentState::Thinking);

        state.set_agent_state(session_id, AgentState::Planning)?;
        assert_eq!(state.get_agent_state(session_id)?, AgentState::Planning);

        state.set_agent_state(
            session_id,
            AgentState::ExecutingTool("get_balance".to_string()),
        )?;
        if let AgentState::ExecutingTool(tool) = state.get_agent_state(session_id)? {
            assert_eq!(tool, "get_balance");
        } else {
            panic!("Expected ExecutingTool state");
        }

        state.set_agent_state(session_id, AgentState::Waiting)?;
        assert_eq!(state.get_agent_state(session_id)?, AgentState::Waiting);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_agent_state_error_handling() -> Result<()> {
        let mut state = ChatState::new()?;
        let session_id = state.create_session("Error Test".to_string())?;

        state.set_agent_state(session_id, AgentState::Error("Test error".to_string()))?;

        if let AgentState::Error(msg) = state.get_agent_state(session_id)? {
            assert_eq!(msg, "Test error");
        } else {
            panic!("Expected Error state");
        }

        // Should be able to recover from error
        state.set_agent_state(session_id, AgentState::Idle)?;
        assert_eq!(state.get_agent_state(session_id)?, AgentState::Idle);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_concurrent_agent_states() -> Result<()> {
        let state = Arc::new(RwLock::new(ChatState::new()?));

        let mut handles = vec![];

        // Create multiple sessions concurrently
        for i in 0..5 {
            let state_clone = Arc::clone(&state);
            let handle = tokio::spawn(async move {
                let mut s = state_clone.write().await;
                let session_id = s.create_session(format!("Session {}", i)).unwrap();

                s.set_agent_state(session_id, AgentState::Thinking).unwrap();
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
                s.set_agent_state(session_id, AgentState::Idle).unwrap();

                session_id
            });
            handles.push(handle);
        }

        let session_ids = futures::future::join_all(handles).await;

        let state_read = state.read().await;
        let sessions = state_read.list_sessions().unwrap();
        assert_eq!(sessions.len(), 5);

        Ok(())
    }
}

#[cfg(all(test, feature = "incomplete_tests"))]
mod session_recording_tests {
    use super::*;
    use tempfile::TempDir;

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_session_recording_start_stop() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let mut state = ChatState::new()?;
        let session_id = state.create_session("Record Test".to_string())?;

        let recording_file = temp_dir.path().join("session.json");

        state.start_recording(session_id, recording_file.clone())?;
        assert!(state.is_recording(session_id)?);

        state.stop_recording(session_id)?;
        assert!(!state.is_recording(session_id)?);

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_session_replay() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let mut state = ChatState::new()?;
        let session_id = state.create_session("Replay Test".to_string())?;

        // Add messages
        for i in 0..5 {
            state.add_message(
                session_id,
                ChatMessage {
                    role: if i % 2 == 0 { "user" } else { "assistant" },
                    content: format!("Message {}", i),
                    timestamp: Utc::now(),
                    tool_calls: None,
                },
            )?;
        }

        let recording_file = temp_dir.path().join("session_replay.json");
        state.start_recording(session_id, recording_file.clone())?;

        // Add more messages during recording
        state.add_message(
            session_id,
            ChatMessage {
                role: "user".to_string(),
                content: "Recorded message".to_string(),
                timestamp: Utc::now(),
                tool_calls: None,
            },
        )?;

        state.stop_recording(session_id)?;

        // Create new session and replay
        let new_session_id = state.create_session("Replayed".to_string())?;
        state.replay_session(new_session_id, &recording_file)?;

        let replayed_messages = state.get_messages(new_session_id)?;
        assert!(replayed_messages.len() > 0);

        Ok(())
    }
}

#[cfg(all(test, feature = "incomplete_tests"))]
mod ui_rendering_tests {
    use super::*;

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    fn test_message_formatting() {
        let message = ChatMessage {
            role: "assistant".to_string(),
            content: "This is a test message with **bold** and *italic* text.".to_string(),
            timestamp: Utc::now(),
            tool_calls: None,
        };

        // Test markdown-like formatting
        assert!(message.content.contains("**bold**"));
        assert!(message.content.contains("*italic*"));
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    fn test_code_block_detection() {
        let message = ChatMessage {
            role: "assistant".to_string(),
            content: "Here's some code:\n```rust\nfn main() {\n    println!(\"Hello\");\n}\n```"
                .to_string(),
            timestamp: Utc::now(),
            tool_calls: None,
        };

        assert!(message.content.contains("```rust"));
        assert!(message.content.contains("```"));
    }

    #[cfg(feature = "incomplete_tests")]
    #[test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    fn test_long_message_handling() {
        let long_content = "x".repeat(10000);
        let message = ChatMessage {
            role: "assistant".to_string(),
            content: long_content.clone(),
            timestamp: Utc::now(),
            tool_calls: None,
        };

        assert_eq!(message.content.len(), 10000);

        // Test truncation for display
        let truncated = if message.content.len() > 5000 {
            format!("{}...", &message.content[..5000])
        } else {
            message.content.clone()
        };

        assert!(truncated.len() <= 5003); // 5000 + "..."
    }
}

#[cfg(all(test, feature = "incomplete_tests"))]
mod performance_tests {
    use super::*;

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_large_session_performance() -> Result<()> {
        let mut state = ChatState::new()?;
        let session_id = state.create_session("Performance Test".to_string())?;

        let start = std::time::Instant::now();

        // Add 1000 messages
        for i in 0..1000 {
            state.add_message(
                session_id,
                ChatMessage {
                    role: if i % 2 == 0 { "user" } else { "assistant" },
                    content: format!("Message {}", i),
                    timestamp: Utc::now(),
                    tool_calls: None,
                },
            )?;
        }

        let duration = start.elapsed();

        // Should complete in reasonable time
        assert!(duration < std::time::Duration::from_secs(1));

        // Retrieve messages should also be fast
        let start = std::time::Instant::now();
        let messages = state.get_messages(session_id)?;
        let duration = start.elapsed();

        assert_eq!(messages.len(), 1000);
        assert!(duration < std::time::Duration::from_millis(100));

        Ok(())
    }

    #[cfg(feature = "incomplete_tests")]
    #[tokio::test]
    #[ignore = "API mismatch - ChatMessage is an enum, not a struct"]
    async fn test_concurrent_session_access() -> Result<()> {
        let state = Arc::new(RwLock::new(ChatState::new()?));

        // Create session
        let session_id = {
            let mut s = state.write().await;
            s.create_session("Concurrent Test".to_string()).unwrap()
        };

        let mut handles = vec![];

        // Concurrent writes
        for i in 0..10 {
            let state_clone = Arc::clone(&state);
            let handle = tokio::spawn(async move {
                let mut s = state_clone.write().await;
                s.add_message(
                    session_id,
                    ChatMessage {
                        role: "user".to_string(),
                        content: format!("Concurrent message {}", i),
                        timestamp: Utc::now(),
                        tool_calls: None,
                    },
                )
                .unwrap();
            });
            handles.push(handle);
        }

        futures::future::join_all(handles).await;

        let state_read = state.read().await;
        let messages = state_read.get_messages(session_id)?;
        assert_eq!(messages.len(), 10);

        Ok(())
    }
}
