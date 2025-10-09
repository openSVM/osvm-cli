//! Unit test to verify the session.rs panic fix
//!
//! This test verifies that ChatSession::add_message() can be called
//! from a synchronous context without panicking due to "no reactor running"

use osvm::utils::agent_chat_v2::session::ChatSession;
use osvm::utils::agent_chat_v2::types::ChatMessage;

#[test]
fn test_add_message_from_sync_context() {
    // This test simulates what happens when the Cursive UI calls add_message()
    // The UI thread is synchronous and has NO Tokio runtime context

    println!("Creating ChatSession in synchronous context...");
    let mut session = ChatSession::new("Test Session".to_string());

    println!("Adding message from synchronous context (this previously caused panic)...");
    // This call previously would panic with "no reactor running"
    // because it called tokio::spawn() without a runtime
    session.add_message(ChatMessage::User("test".to_string()));

    println!("✅ Message added successfully without panic!");

    // Verify the message was actually added
    assert_eq!(session.messages.len(), 1);
    match &session.messages[0] {
        ChatMessage::User(text) => assert_eq!(text, "test"),
        _ => panic!("Expected User message"),
    }

    println!("✅ Message content verified!");

    // Add a few more messages to ensure it works multiple times
    session.add_message(ChatMessage::Agent("response".to_string()));
    session.add_message(ChatMessage::User("another test".to_string()));

    assert_eq!(session.messages.len(), 3);
    println!("✅ Multiple messages work correctly!");
}

#[test]
fn test_add_message_does_not_block() {
    // Verify that add_message() returns quickly and doesn't block
    use std::time::{Duration, Instant};

    let mut session = ChatSession::new("Performance Test".to_string());

    let start = Instant::now();
    session.add_message(ChatMessage::User("test".to_string()));
    let duration = start.elapsed();

    // Should complete in well under 100ms (the ClickHouse logging is fire-and-forget)
    assert!(
        duration < Duration::from_millis(100),
        "add_message() took too long: {:?}",
        duration
    );

    println!(
        "✅ add_message() completed in {:?} (non-blocking)",
        duration
    );
}
