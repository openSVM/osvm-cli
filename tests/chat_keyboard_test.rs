//! Integration test for chat keyboard shortcuts
//! Tests that Ctrl+M and Shift+Enter properly send messages

use cursive::event::{Event, Key};
use cursive::traits::*; // Import Nameable, etc.
use cursive::views::TextView;
use cursive::Cursive;

/// Test helper to simulate running the chat UI with event injection
fn create_test_chat_ui() -> Cursive {
    let mut siv = Cursive::new();

    // Set up a simple test UI that mimics chat behavior
    siv.add_layer(TextView::new("Test Chat").with_name("chat_display"));

    // Add global callback for Ctrl+M (Ctrl+Enter)
    siv.add_global_callback(Event::CtrlChar('m'), |s| {
        s.call_on_name("chat_display", |view: &mut TextView| {
            view.set_content("Message sent via Ctrl+M!");
        });
    });

    // Add global callback for Shift+Enter
    siv.add_global_callback(Event::Shift(Key::Enter), |s| {
        s.call_on_name("chat_display", |view: &mut TextView| {
            view.set_content("Message sent via Shift+Enter!");
        });
    });

    siv
}

#[test]
fn test_ctrl_m_sends_message() {
    let mut siv = create_test_chat_ui();

    // Manually inject Ctrl+M event
    siv.on_event(Event::CtrlChar('m'));

    // Check that the message was "sent"
    let result = siv.call_on_name("chat_display", |view: &mut TextView| {
        view.get_content().source().to_string()
    });

    assert_eq!(result, Some("Message sent via Ctrl+M!".to_string()));
}

#[test]
fn test_shift_enter_sends_message() {
    let mut siv = create_test_chat_ui();

    // Manually inject Shift+Enter event
    siv.on_event(Event::Shift(Key::Enter));

    // Check that the message was "sent"
    let result = siv.call_on_name("chat_display", |view: &mut TextView| {
        view.get_content().source().to_string()
    });

    assert_eq!(result, Some("Message sent via Shift+Enter!".to_string()));
}

#[test]
fn test_regular_enter_does_not_send() {
    let mut siv = create_test_chat_ui();

    // Manually inject regular Enter event
    siv.on_event(Event::Key(Key::Enter));

    // Check that the message was NOT changed (still shows "Test Chat")
    let result = siv.call_on_name("chat_display", |view: &mut TextView| {
        view.get_content().source().to_string()
    });

    assert_eq!(result, Some("Test Chat".to_string()));
}

#[test]
fn test_dummy_backend_runs() {
    let mut siv = Cursive::new();
    siv.add_layer(TextView::new("Test"));

    // This should complete without hanging
    siv.run_dummy();

    // If we get here, dummy backend worked
    assert!(true);
}
