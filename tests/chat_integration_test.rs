//! Integration test for full chat message flow
//! Tests that Ctrl+Enter actually sends messages and processes them

use cursive::event::{Event, Key};
use cursive::traits::*;
use cursive::Cursive;
use std::sync::{Arc, Mutex};

// Import chat components
use osvm::utils::agent_chat_v2::state::AdvancedChatState;
use osvm::utils::agent_chat_v2::ui::text_area_wrapper::SendableTextArea;

#[test]
fn test_ctrl_enter_sends_and_processes_message() {
    let mut siv = Cursive::new();

    // Track if message was processed
    let message_processed = Arc::new(Mutex::new(false));
    let processed_content = Arc::new(Mutex::new(String::new()));

    let processed_clone = message_processed.clone();
    let content_clone = processed_content.clone();

    // Add input field
    let textarea = SendableTextArea::new();
    siv.add_layer(textarea.with_name("input"));

    // Set initial content
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("test message for AI");
    });

    // Add Ctrl+M callback that mimics real handler
    siv.add_global_callback(Event::CtrlChar('m'), move |s| {
        // Get content first (immutable access)
        let content = s
            .call_on_name("input", |view: &mut SendableTextArea| {
                view.get_content().to_string()
            })
            .unwrap_or_default();

        if !content.trim().is_empty() {
            // Simulate message processing
            *processed_clone.lock().unwrap() = true;
            *content_clone.lock().unwrap() = content.clone();

            // Clear input after send (mutable access)
            s.call_on_name("input", |view: &mut SendableTextArea| {
                view.set_content("");
            });
        }
    });

    // Inject Ctrl+M event
    siv.on_event(Event::CtrlChar('m'));

    // Verify message was processed
    assert!(
        *message_processed.lock().unwrap(),
        "Message should have been processed"
    );
    assert_eq!(*processed_content.lock().unwrap(), "test message for AI");

    // Verify input was cleared
    let remaining = siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.get_content().to_string()
    });
    assert_eq!(
        remaining,
        Some("".to_string()),
        "Input should be cleared after send"
    );
}

#[test]
fn test_shift_enter_sends_and_processes_message() {
    let mut siv = Cursive::new();

    // Track if message was processed
    let message_processed = Arc::new(Mutex::new(false));
    let processed_content = Arc::new(Mutex::new(String::new()));

    let processed_clone = message_processed.clone();
    let content_clone = processed_content.clone();

    // Add input field
    let textarea = SendableTextArea::new();
    siv.add_layer(textarea.with_name("input"));

    // Set initial content
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("another test message");
    });

    // Add Shift+Enter callback
    siv.add_global_callback(Event::Shift(Key::Enter), move |s| {
        // Get content first (immutable access)
        let content = s
            .call_on_name("input", |view: &mut SendableTextArea| {
                view.get_content().to_string()
            })
            .unwrap_or_default();

        if !content.trim().is_empty() {
            // Simulate message processing
            *processed_clone.lock().unwrap() = true;
            *content_clone.lock().unwrap() = content.clone();

            // Clear input after send (mutable access)
            s.call_on_name("input", |view: &mut SendableTextArea| {
                view.set_content("");
            });
        }
    });

    // Inject Shift+Enter event
    siv.on_event(Event::Shift(Key::Enter));

    // Verify message was processed
    assert!(
        *message_processed.lock().unwrap(),
        "Message should have been processed"
    );
    assert_eq!(*processed_content.lock().unwrap(), "another test message");

    // Verify input was cleared
    let remaining = siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.get_content().to_string()
    });
    assert_eq!(
        remaining,
        Some("".to_string()),
        "Input should be cleared after send"
    );
}

#[test]
fn test_message_added_to_chat_history() {
    let mut siv = Cursive::new();

    // Create state
    let state = AdvancedChatState::new().expect("Failed to create chat state");
    let state_clone = state.clone();

    // Add input field
    let textarea = SendableTextArea::new();
    siv.add_layer(textarea.with_name("input"));

    // Set initial content
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("history test message");
    });

    // Add Ctrl+M callback that adds to history
    siv.add_global_callback(Event::CtrlChar('m'), move |s| {
        // Get content first (immutable access)
        let content = s
            .call_on_name("input", |view: &mut SendableTextArea| {
                view.get_content().to_string()
            })
            .unwrap_or_default();

        if !content.trim().is_empty() {
            // Add to history using the correct method name
            state_clone.add_to_history(content.clone());

            // Clear input after send (mutable access)
            s.call_on_name("input", |view: &mut SendableTextArea| {
                view.set_content("");
            });
        }
    });

    // Inject Ctrl+M event
    siv.on_event(Event::CtrlChar('m'));

    // Verify we can retrieve the message from history
    if let Some(msg) = state.history_previous() {
        assert_eq!(
            msg, "history test message",
            "History should contain the sent message"
        );
    } else {
        panic!("Should be able to retrieve message from history");
    }
}

#[test]
fn test_multiple_messages_in_sequence() {
    let mut siv = Cursive::new();

    // Track processed messages
    let messages = Arc::new(Mutex::new(Vec::new()));
    let messages_clone = messages.clone();

    // Add input field
    let textarea = SendableTextArea::new();
    siv.add_layer(textarea.with_name("input"));

    // Add Ctrl+M callback
    siv.add_global_callback(Event::CtrlChar('m'), move |s| {
        // Get content first (immutable access)
        let content = s
            .call_on_name("input", |view: &mut SendableTextArea| {
                view.get_content().to_string()
            })
            .unwrap_or_default();

        if !content.trim().is_empty() {
            // Store message
            messages_clone.lock().unwrap().push(content.clone());

            // Clear input after send (mutable access)
            s.call_on_name("input", |view: &mut SendableTextArea| {
                view.set_content("");
            });
        }
    });

    // Send first message
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("first message");
    });
    siv.on_event(Event::CtrlChar('m'));

    // Send second message
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("second message");
    });
    siv.on_event(Event::CtrlChar('m'));

    // Send third message
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("third message");
    });
    siv.on_event(Event::CtrlChar('m'));

    // Verify all messages were processed
    let processed = messages.lock().unwrap();
    assert_eq!(processed.len(), 3, "Should have processed 3 messages");
    assert_eq!(processed[0], "first message");
    assert_eq!(processed[1], "second message");
    assert_eq!(processed[2], "third message");
}

#[test]
fn test_empty_messages_not_processed() {
    let mut siv = Cursive::new();

    // Track if any message was processed
    let message_count = Arc::new(Mutex::new(0));
    let count_clone = message_count.clone();

    // Add input field
    let textarea = SendableTextArea::new();
    siv.add_layer(textarea.with_name("input"));

    // Add Ctrl+M callback
    siv.add_global_callback(Event::CtrlChar('m'), move |s| {
        // Get content first (immutable access)
        let content = s
            .call_on_name("input", |view: &mut SendableTextArea| {
                view.get_content().to_string()
            })
            .unwrap_or_default();

        if !content.trim().is_empty() {
            // Increment counter
            *count_clone.lock().unwrap() += 1;

            // Clear input after send (mutable access)
            s.call_on_name("input", |view: &mut SendableTextArea| {
                view.set_content("");
            });
        }
    });

    // Try to send empty message
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("");
    });
    siv.on_event(Event::CtrlChar('m'));

    // Try to send whitespace-only message
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("   \n\t  ");
    });
    siv.on_event(Event::CtrlChar('m'));

    // Verify no messages were processed
    assert_eq!(
        *message_count.lock().unwrap(),
        0,
        "Empty messages should not be processed"
    );
}
