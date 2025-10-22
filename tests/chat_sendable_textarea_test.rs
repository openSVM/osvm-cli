//! Integration test for chat with SendableTextArea
//! Verifies that our keyboard shortcuts work with the actual SendableTextArea type

use cursive::event::{Event, Key};
use cursive::traits::*;
use cursive::Cursive;

// Import the actual chat components
use osvm::utils::agent_chat_v2::ui::text_area_wrapper::SendableTextArea;

#[test]
fn test_sendable_textarea_type_works() {
    let mut siv = Cursive::new();

    // Create SendableTextArea (the actual type we use in chat)
    let textarea = SendableTextArea::new();
    siv.add_layer(textarea.with_name("input"));

    // Verify we can find it with the correct type
    let found = siv.find_name::<SendableTextArea>("input");
    assert!(found.is_some(), "Should find SendableTextArea with correct type");
}

#[test]
fn test_sendable_textarea_get_set_content() {
    let mut siv = Cursive::new();

    let textarea = SendableTextArea::new();
    siv.add_layer(textarea.with_name("input"));

    // Set content
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("test message");
    });

    // Get content
    let content = siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.get_content().to_string()
    });

    assert_eq!(content, Some("test message".to_string()));
}

#[test]
fn test_sendable_textarea_clear_after_send() {
    let mut siv = Cursive::new();

    let textarea = SendableTextArea::new();
    siv.add_layer(textarea.with_name("input"));

    // Set some content
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("message to send");
    });

    // Simulate send action - get content and clear
    let mut sent_message = String::new();
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        sent_message = view.get_content().to_string();
        view.set_content(""); // Clear after "sending"
    });

    assert_eq!(sent_message, "message to send");

    // Verify it was cleared
    let remaining_content = siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.get_content().to_string()
    });

    assert_eq!(remaining_content, Some("".to_string()));
}

#[test]
fn test_ctrl_m_with_sendable_textarea() {
    let mut siv = Cursive::new();

    let textarea = SendableTextArea::new();
    siv.add_layer(textarea.with_name("input"));

    // Set content
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("test");
    });

    // Add the same callback as in the real app (FIXED PATTERN)
    siv.add_global_callback(Event::CtrlChar('m'), |s| {
        // Get content first (immutable access)
        let content = s.call_on_name("input", |view: &mut SendableTextArea| {
            view.get_content().to_string()
        }).unwrap_or_default();

        if !content.trim().is_empty() {
            // Clear input after getting content (mutable access)
            s.call_on_name("input", |view: &mut SendableTextArea| {
                view.set_content("");
            });
        }
    });

    // Inject Ctrl+M event
    siv.on_event(Event::CtrlChar('m'));

    // Verify input was cleared (simulating message send)
    let remaining = siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.get_content().to_string()
    });

    assert_eq!(remaining, Some("".to_string()), "Input should be cleared after Ctrl+M");
}

#[test]
fn test_shift_enter_with_sendable_textarea() {
    let mut siv = Cursive::new();

    let textarea = SendableTextArea::new();
    siv.add_layer(textarea.with_name("input"));

    // Set content
    siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.set_content("test message");
    });

    // Add the same callback as in the real app (FIXED PATTERN)
    siv.add_global_callback(Event::Shift(Key::Enter), |s| {
        // Get content first (immutable access)
        let content = s.call_on_name("input", |view: &mut SendableTextArea| {
            view.get_content().to_string()
        }).unwrap_or_default();

        if !content.trim().is_empty() {
            // Clear input after getting content (mutable access)
            s.call_on_name("input", |view: &mut SendableTextArea| {
                view.set_content("");
            });
        }
    });

    // Inject Shift+Enter event
    siv.on_event(Event::Shift(Key::Enter));

    // Verify input was cleared
    let remaining = siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.get_content().to_string()
    });

    assert_eq!(remaining, Some("".to_string()), "Input should be cleared after Shift+Enter");
}

#[test]
fn test_empty_message_not_sent() {
    let mut siv = Cursive::new();

    let textarea = SendableTextArea::new();
    siv.add_layer(textarea.with_name("input"));

    // Leave content empty

    // Add callback that only clears non-empty messages (FIXED PATTERN)
    siv.add_global_callback(Event::CtrlChar('m'), |s| {
        // Get content first (immutable access)
        let content = s.call_on_name("input", |view: &mut SendableTextArea| {
            view.get_content().to_string()
        }).unwrap_or_default();

        if !content.trim().is_empty() {
            // Clear input (mutable access)
            s.call_on_name("input", |view: &mut SendableTextArea| {
                view.set_content("");
            });
        }
    });

    // Inject Ctrl+M with empty content
    siv.on_event(Event::CtrlChar('m'));

    // Content should still be empty (not modified)
    let content = siv.call_on_name("input", |view: &mut SendableTextArea| {
        view.get_content().to_string()
    });

    assert_eq!(content, Some("".to_string()));
}
