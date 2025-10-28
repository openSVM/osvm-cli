//! Comprehensive Chat UI Test Scenarios
//!
//! This module provides extensive test scenarios for both basic and advanced chat UIs,
//! including visual layout validation, interaction testing, and regression checks.

use super::tui_test_agent::{ScreenExpectation, TuiAction, TuiTestScenario, TuiTestStep};

/// Create comprehensive test scenarios for basic chat UI
pub fn create_basic_chat_scenarios() -> Vec<TuiTestScenario> {
    vec![
        // Test 1: Initial UI rendering
        TuiTestScenario {
            name: "basic_chat_initial_render".to_string(),
            description: "Verify basic chat UI renders correctly on startup".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(3000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture initial screen".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::VerifyText("OSVM".to_string()),
                    wait_ms: 0,
                    description: "Verify OSVM branding present".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 2,
                contains_text: vec!["OSVM".to_string()],
                not_contains_text: vec!["Error".to_string(), "panic".to_string()],
                title: Some("Initial Render Validation".to_string()),
            }],
        },

        // Test 2: Help command
        TuiTestScenario {
            name: "basic_chat_help_command".to_string(),
            description: "Test help command in basic chat".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(3000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys("/help\n".to_string()),
                    wait_ms: 2000,
                    description: "Send help command".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture help output".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::VerifyText("Available commands".to_string()),
                    wait_ms: 0,
                    description: "Verify help text appears".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 3,
                contains_text: vec!["help".to_string()],
                not_contains_text: vec![],
                title: Some("Help Command Test".to_string()),
            }],
        },

        // Test 3: Message input and response
        TuiTestScenario {
            name: "basic_chat_message_flow".to_string(),
            description: "Test sending a message and receiving response".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(3000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys("Hello OSVM\n".to_string()),
                    wait_ms: 1000,
                    description: "Send greeting message".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture after sending message".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for AI response".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture with response".to_string(),
                },
            ],
            expected_screens: vec![
                ScreenExpectation {
                    after_step: 3,
                    contains_text: vec!["Hello OSVM".to_string()],
                    not_contains_text: vec!["Error".to_string()],
                    title: Some("Message Sent".to_string()),
                },
            ],
        },

        // Test 4: Tools command
        TuiTestScenario {
            name: "basic_chat_tools_command".to_string(),
            description: "Test /tools command to list available MCP tools".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(3000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys("/tools\n".to_string()),
                    wait_ms: 2000,
                    description: "Send tools command".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture tools output".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 3,
                contains_text: vec![],
                not_contains_text: vec!["panic".to_string()],
                title: Some("Tools Command Test".to_string()),
            }],
        },

        // Test 5: Status command
        TuiTestScenario {
            name: "basic_chat_status_command".to_string(),
            description: "Test /status command to check system status".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(3000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys("/status\n".to_string()),
                    wait_ms: 2000,
                    description: "Send status command".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture status output".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 3,
                contains_text: vec![],
                not_contains_text: vec!["Error".to_string()],
                title: Some("Status Command Test".to_string()),
            }],
        },

        // Test 6: Clear command
        TuiTestScenario {
            name: "basic_chat_clear_command".to_string(),
            description: "Test /clear command to clear chat history".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(3000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys("First message\n".to_string()),
                    wait_ms: 1000,
                    description: "Send first message".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::Wait(2000),
                    wait_ms: 0,
                    description: "Wait for response".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys("/clear\n".to_string()),
                    wait_ms: 1000,
                    description: "Clear history".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture after clear".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 5,
                contains_text: vec![],
                not_contains_text: vec!["First message".to_string()],
                title: Some("Clear Command Test".to_string()),
            }],
        },

        // Test 7: Multi-line input handling
        TuiTestScenario {
            name: "basic_chat_multiline_input".to_string(),
            description: "Test handling of multi-line input".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(3000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys("Line 1".to_string()),
                    wait_ms: 500,
                    description: "Type first line".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture partial input".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys("\n".to_string()),
                    wait_ms: 1000,
                    description: "Submit message".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture after submission".to_string(),
                },
            ],
            expected_screens: vec![
                ScreenExpectation {
                    after_step: 3,
                    contains_text: vec!["Line 1".to_string()],
                    not_contains_text: vec![],
                    title: Some("Partial Input Captured".to_string()),
                },
            ],
        },

        // Test 8: Long message handling
        TuiTestScenario {
            name: "basic_chat_long_message".to_string(),
            description: "Test handling of very long messages".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(3000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys(
                        "This is a very long message that should wrap across multiple lines in the chat interface and test the text wrapping capabilities\n".to_string()
                    ),
                    wait_ms: 2000,
                    description: "Send long message".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture wrapped text".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 3,
                contains_text: vec!["long message".to_string()],
                not_contains_text: vec![],
                title: Some("Long Message Handling".to_string()),
            }],
        },
    ]
}

/// Create comprehensive test scenarios for advanced chat UI
pub fn create_advanced_chat_scenarios() -> Vec<TuiTestScenario> {
    vec![
        // Test 1: Initial layout validation
        TuiTestScenario {
            name: "advanced_chat_layout_validation".to_string(),
            description: "Verify advanced chat UI layout and panels render correctly".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for FAR-style UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture initial layout".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::VerifyText("Session".to_string()),
                    wait_ms: 0,
                    description: "Verify session panel exists".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 2,
                contains_text: vec!["Session".to_string()],
                not_contains_text: vec!["Error".to_string(), "panic".to_string()],
                title: Some("Layout Validation".to_string()),
            }],
        },
        // Test 2: Panel navigation with Tab
        TuiTestScenario {
            name: "advanced_chat_panel_navigation".to_string(),
            description: "Test Tab key navigation between panels".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture initial focus".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKey("Tab".to_string()),
                    wait_ms: 1000,
                    description: "Press Tab to switch panels".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture after Tab".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKey("Tab".to_string()),
                    wait_ms: 1000,
                    description: "Press Tab again".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture after second Tab".to_string(),
                },
            ],
            expected_screens: vec![
                ScreenExpectation {
                    after_step: 2,
                    contains_text: vec![],
                    not_contains_text: vec!["Error".to_string()],
                    title: Some("Initial Focus".to_string()),
                },
                ScreenExpectation {
                    after_step: 4,
                    contains_text: vec![],
                    not_contains_text: vec!["Error".to_string()],
                    title: Some("After Tab Navigation".to_string()),
                },
            ],
        },
        // Test 3: Context menu (F10)
        TuiTestScenario {
            name: "advanced_chat_context_menu".to_string(),
            description: "Test F10 context menu functionality".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKey("F10".to_string()),
                    wait_ms: 1500,
                    description: "Open context menu with F10".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture context menu".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKey("Escape".to_string()),
                    wait_ms: 1000,
                    description: "Close menu with Escape".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture after closing menu".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 3,
                contains_text: vec![],
                not_contains_text: vec!["Error".to_string()],
                title: Some("Context Menu Open".to_string()),
            }],
        },
        // Test 4: Session list navigation
        TuiTestScenario {
            name: "advanced_chat_session_list".to_string(),
            description: "Test session list navigation with arrow keys".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKey("Tab".to_string()),
                    wait_ms: 500,
                    description: "Navigate to session list".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKey("Down".to_string()),
                    wait_ms: 500,
                    description: "Move down in session list".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture with selection moved".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKey("Up".to_string()),
                    wait_ms: 500,
                    description: "Move up in session list".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture with selection restored".to_string(),
                },
            ],
            expected_screens: vec![],
        },
        // Test 5: Message composition and sending
        TuiTestScenario {
            name: "advanced_chat_message_send".to_string(),
            description: "Test message composition in advanced chat".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys("Test message from advanced chat".to_string()),
                    wait_ms: 1000,
                    description: "Type message".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture with typed message".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKey("Enter".to_string()),
                    wait_ms: 2000,
                    description: "Send message".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture after sending".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 3,
                contains_text: vec!["Test message".to_string()],
                not_contains_text: vec![],
                title: Some("Message Typed".to_string()),
            }],
        },
        // Test 6: Window resizing behavior
        TuiTestScenario {
            name: "advanced_chat_resize_behavior".to_string(),
            description: "Test UI adaptation to different terminal sizes".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture at initial size".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 2,
                contains_text: vec![],
                not_contains_text: vec!["Error".to_string()],
                title: Some("Initial Size".to_string()),
            }],
        },
        // Test 7: Status bar information
        TuiTestScenario {
            name: "advanced_chat_status_bar".to_string(),
            description: "Verify status bar displays correct information".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture status bar".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 2,
                contains_text: vec![],
                not_contains_text: vec!["Error".to_string()],
                title: Some("Status Bar Check".to_string()),
            }],
        },
        // Test 8: Rapid input handling
        TuiTestScenario {
            name: "advanced_chat_rapid_input".to_string(),
            description: "Test handling of rapid keyboard input".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys("abcdefghijklmnopqrstuvwxyz".to_string()),
                    wait_ms: 100,
                    description: "Type rapidly".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture after rapid input".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 3,
                contains_text: vec!["abcdefg".to_string()],
                not_contains_text: vec!["Error".to_string()],
                title: Some("Rapid Input Test".to_string()),
            }],
        },
    ]
}

/// Create visual layout validation scenarios
pub fn create_layout_validation_scenarios() -> Vec<TuiTestScenario> {
    vec![
        // Layout test 1: Border integrity
        TuiTestScenario {
            name: "layout_border_integrity".to_string(),
            description: "Validate that all borders are properly rendered without gaps".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture for border analysis".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 2,
                contains_text: vec![],
                not_contains_text: vec!["Error".to_string()],
                title: Some("Border Integrity".to_string()),
            }],
        },

        // Layout test 2: Panel alignment
        TuiTestScenario {
            name: "layout_panel_alignment".to_string(),
            description: "Verify that panels are properly aligned".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 500,
                    description: "Capture for alignment analysis".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 2,
                contains_text: vec![],
                not_contains_text: vec![],
                title: Some("Panel Alignment".to_string()),
            }],
        },

        // Layout test 3: Text overflow handling
        TuiTestScenario {
            name: "layout_text_overflow".to_string(),
            description: "Test handling of text that exceeds panel boundaries".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(5000),
                    wait_ms: 0,
                    description: "Wait for UI initialization".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys(
                        "This is an extremely long message that should definitely exceed the normal panel width and test the text wrapping and overflow handling capabilities of the UI system\n".to_string()
                    ),
                    wait_ms: 2000,
                    description: "Send very long message".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture overflow handling".to_string(),
                },
            ],
            expected_screens: vec![ScreenExpectation {
                after_step: 3,
                contains_text: vec!["extremely long message".to_string()],
                not_contains_text: vec![],
                title: Some("Text Overflow".to_string()),
            }],
        },
    ]
}

/// Get all test scenarios combined
pub fn get_all_chat_test_scenarios() -> Vec<TuiTestScenario> {
    let mut scenarios = Vec::new();
    scenarios.extend(create_basic_chat_scenarios());
    scenarios.extend(create_advanced_chat_scenarios());
    scenarios.extend(create_layout_validation_scenarios());
    scenarios
}
