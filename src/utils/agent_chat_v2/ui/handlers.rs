//! Event handlers for the advanced chat UI

use crate::utils::agent_chat_v2::agent::ThemeCommandType;
use anyhow::{anyhow, Context, Result};
use cursive::direction::Orientation;
use cursive::traits::*; // Import Nameable, Resizable, etc.
use cursive::views::{
    Button, Dialog, DummyView, EditView, LinearLayout, ListView, Panel, ScrollView, SelectView,
    TextView,
};
use cursive::Cursive;
use log::{error, info, warn};
use uuid::Uuid;

use super::super::agent::AgentCommand;
use super::super::state::AdvancedChatState;
use super::super::types::{AgentState, ChatMessage};
use super::display::update_ui_displays;

// UI Event Handlers
pub fn handle_chat_selection(siv: &mut Cursive, session_id: Uuid) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Err(e) = state.set_active_session(session_id) {
            error!("Failed to set active session: {}", e);
        }
    });

    // Update displays
    update_ui_displays(siv);
}

// Action button handlers
pub fn retry_last_message(siv: &mut Cursive, state: AdvancedChatState) {
    if let Some(session) = state.get_active_session() {
        // Find the last user message
        if let Some(last_user_msg) = session.messages.iter().rev().find_map(|msg| {
            if let ChatMessage::User(text) = msg {
                Some(text.clone())
            } else {
                None
            }
        }) {
            // Re-send the message
            handle_user_input(siv, &last_user_msg, state);
        }
    }
}

pub fn copy_last_message(siv: &mut Cursive, state: AdvancedChatState) {
    if let Some(session) = state.get_active_session() {
        if let Some(last_msg) = session.messages.last() {
            let text_to_copy = match last_msg {
                ChatMessage::User(text) | ChatMessage::Agent(text) => text.clone(),
                _ => String::new(),
            };

            if !text_to_copy.is_empty() {
                // Copy to actual system clipboard
                match arboard::Clipboard::new() {
                    Ok(mut clipboard) => {
                        if let Err(e) = clipboard.set_text(&text_to_copy) {
                            siv.add_layer(
                                Dialog::info(format!("Failed to copy to clipboard: {}", e))
                                    .title("Error"),
                            );
                        } else {
                            siv.add_layer(
                                Dialog::info("Message copied to clipboard!")
                                    .title("Success")
                                    .button("OK", |s| {
                                        s.pop_layer();
                                    }),
                            );
                        }
                    }
                    Err(e) => {
                        siv.add_layer(
                            Dialog::info(format!(
                                "Clipboard not available: {}\nText:\n{}",
                                e, text_to_copy
                            ))
                            .title("Clipboard Error")
                            .button("OK", |s| {
                                s.pop_layer();
                            }),
                        );
                    }
                }
            }
        }
    }
}

pub fn delete_last_message(siv: &mut Cursive, state: AdvancedChatState) {
    // Show confirmation dialog before deleting
    siv.add_layer(
        Dialog::text("Are you sure you want to delete the last message?\nThis action cannot be undone.")
            .title("⚠ Confirm Delete")
            .button("Yes, Delete", move |s| {
                s.pop_layer(); // Close confirmation dialog

                if let Some(session) = state.get_active_session() {
                    let session_id = session.id;

                    // First, clear any processing messages that might be stuck
                    if let Err(e) = state.remove_last_processing_message(session_id) {
                        error!("Failed to remove processing message: {}", e);
                    }

                    // Then delete the last actual message
                    if let Ok(mut sessions) = state.sessions.write() {
                        if let Some(session) = sessions.get_mut(&session_id) {
                            if !session.messages.is_empty() {
                                session.messages.pop();

                                // Show success feedback
                                s.add_layer(
                                    Dialog::info("✓ Message deleted successfully")
                                        .title("Deleted")
                                        .button("OK", |s| {
                                            s.pop_layer();
                                        })
                                );
                            } else {
                                s.add_layer(
                                    Dialog::info("No messages to delete")
                                        .title("Info")
                                        .button("OK", |s| {
                                            s.pop_layer();
                                        })
                                );
                            }
                        }
                    }

                    // Update UI after all changes
                    update_ui_displays(s);
                }
            })
            .button("Cancel", |s| {
                s.pop_layer();
            })
    );
}

pub fn fork_conversation(siv: &mut Cursive, state: AdvancedChatState) {
    if let Some(current_session) = state.get_active_session() {
        // Create a new session with the same messages
        let forked_name = format!("{} (Fork)", current_session.name);

        if let Ok(new_session_id) = state.create_session(forked_name) {
            // Copy messages to the new session
            if let Ok(mut sessions) = state.sessions.write() {
                if let Some(new_session) = sessions.get_mut(&new_session_id) {
                    new_session.messages = current_session.messages.clone();
                }
            }

            // Switch to the forked session
            let _ = state.set_active_session(new_session_id);
            update_ui_displays(siv);
        }
    }
}

pub fn take_screenshot(siv: &mut Cursive) {
    // Show taking screenshot message
    siv.add_layer(
        Dialog::text("📸 Taking screenshot...\nPlease wait...")
            .title("Screenshot")
            .with_name("screenshot_dialog"),
    );

    // Export TUI buffer for rendering
    let tui_buffer = export_cursive_buffer(siv);

    // Take screenshot in background
    let cb_sink = siv.cb_sink().clone();

    std::thread::spawn(move || {
        // Try TUI buffer screenshot first (pure content)
        let result = if let Some(buffer) = tui_buffer {
            crate::utils::screenshot::render_tui_buffer_to_image(buffer)
        } else {
            // Fallback to window capture
            crate::utils::screenshot::take_terminal_screenshot(true)
        };

        match result {
            Ok(path) => {
                let success_msg = format!("✅ Screenshot saved successfully!\n\nMode: TUI Content Export\nLocation:\n{}", path.display());
                cb_sink
                    .send(Box::new(move |s| {
                        s.pop_layer(); // Remove "taking screenshot" dialog
                        s.add_layer(
                            Dialog::text(success_msg)
                                .title("Screenshot Saved")
                                .button("OK", |s| {
                                    s.pop_layer();
                                }),
                        );
                    }))
                    .ok();
            }
            Err(e) => {
                let error_msg = format!("❌ Screenshot failed:\n\n{}", e);
                cb_sink
                    .send(Box::new(move |s| {
                        s.pop_layer(); // Remove "taking screenshot" dialog
                        s.add_layer(
                            Dialog::text(error_msg)
                                .title("Screenshot Error")
                                .button("OK", |s| {
                                    s.pop_layer();
                                }),
                        );
                    }))
                    .ok();
            }
        }
    });
}

/// Export cursive screen buffer for TUI rendering
fn export_cursive_buffer(siv: &mut Cursive) -> Option<crate::utils::screenshot::TuiBuffer> {
    use cursive::backend::Backend;

    // Get screen size
    let size = siv.screen_size();

    // Try to export backend buffer
    // Note: This requires accessing the backend's internal state
    // We'll create a simplified version for now

    Some(crate::utils::screenshot::TuiBuffer {
        width: size.x,
        height: size.y,
        content: Vec::new(), // Will be populated by screen scraping
    })
}

pub fn insert_suggestion_at_cursor(siv: &mut Cursive, index: usize, state: AdvancedChatState) {
    // Get the suggestion
    let suggestion = {
        let suggestions = state
            .current_suggestions
            .read()
            .map(|s| s.clone())
            .unwrap_or_default();

        if index < suggestions.len() {
            suggestions[index].clone()
        } else {
            return;
        }
    };

    // Insert into the input field at cursor position
    if let Some(mut input) = siv.find_name::<EditView>("input") {
        let current_content = input.get_content();
        let cursor_pos = input.get_cursor();

        // Insert suggestion at cursor position
        let mut new_content = String::new();
        new_content.push_str(&current_content[..cursor_pos]);
        new_content.push_str(&suggestion);
        if cursor_pos < current_content.len() {
            new_content.push_str(&current_content[cursor_pos..]);
        }

        input.set_content(new_content);
        // Move cursor to end of inserted text
        input.set_cursor(cursor_pos + suggestion.len());
    }

    // Hide suggestions after insertion
    if let Ok(mut vis) = state.suggestions_visible.write() {
        *vis = false;
    }
}

pub fn handle_user_input(siv: &mut Cursive, text: &str, state: AdvancedChatState) {
    use super::input_validation::{validate_input, ValidationResult, contains_sensitive_pattern, get_sensitive_warning};

    // Validate input first
    match validate_input(text) {
        ValidationResult::Empty | ValidationResult::OnlyWhitespace => {
            // Silently ignore empty input
            return;
        }
        ValidationResult::TooLong { text: truncated, max_length } => {
            siv.add_layer(
                Dialog::text(format!(
                    "⚠️ Message Too Long\n\n\
                    Your message is too long ({} characters).\n\
                    Maximum allowed: {} characters.\n\n\
                    The message has been truncated. Please shorten it.",
                    text.len(),
                    max_length
                ))
                .title("Input Validation")
                .button("OK", |s| {
                    s.pop_layer();
                })
            );
            return;
        }
        ValidationResult::TooManyNewlines { max_lines, .. } => {
            siv.add_layer(
                Dialog::text(format!(
                    "⚠️ Too Many Line Breaks\n\n\
                    Your message has too many line breaks.\n\
                    Maximum allowed: {} lines.\n\n\
                    Please reduce the number of line breaks.",
                    max_lines
                ))
                .title("Input Validation")
                .button("OK", |s| {
                    s.pop_layer();
                })
            );
            return;
        }
        ValidationResult::ContainsBinaryData => {
            siv.add_layer(
                Dialog::text(
                    "⚠️ Invalid Characters\n\n\
                    Your message contains invalid or binary characters.\n\
                    Please use only text characters."
                )
                .title("Input Validation")
                .button("OK", |s| {
                    s.pop_layer();
                })
            );
            return;
        }
        ValidationResult::Valid(validated_text) => {
            // Check for sensitive data
            if contains_sensitive_pattern(&validated_text) {
                let text_clone = validated_text.clone();
                let state_clone = state.clone();
                siv.add_layer(
                    Dialog::text(get_sensitive_warning())
                        .title("⚠️ Sensitive Data Warning")
                        .button("Cancel", |s| {
                            s.pop_layer();
                        })
                        .button("Send Anyway", move |s| {
                            s.pop_layer();
                            process_validated_input(s, &text_clone, state_clone.clone());
                        })
                );
                return;
            }

            // Process validated input
            process_validated_input(siv, &validated_text, state);
        }
    }
}

fn process_validated_input(siv: &mut Cursive, text: &str, state: AdvancedChatState) {
    let user_message = text.to_string();

    // Add to history before processing
    state.add_to_history(user_message.clone());

    // Hide suggestions when sending a message
    if let Ok(mut vis) = state.suggestions_visible.write() {
        *vis = false;
    }

    // Clear input
    if let Some(mut input) = siv.find_name::<EditView>("input") {
        input.set_content("");
    }

    // Check for theme commands first
    if user_message.trim().starts_with("/theme") {
        handle_theme_command(siv, &user_message, state);
        return;
    }

    // Check for duplicate input to prevent processing the same message twice
    if let Some(session) = state.get_active_session() {
        // Check if the last user message is the same as the current one
        // Look through messages in reverse to find the last User message
        if let Some(last_user_msg) = session.messages.iter().rev().find_map(|msg| {
            if let ChatMessage::User(text) = msg {
                Some(text)
            } else {
                None
            }
        }) {
            if last_user_msg == &user_message {
                warn!("Duplicate user input detected, ignoring: {}", user_message);
                return;
            }
        }

        // Add user message to the session
        let _ = state.add_message_to_session(session.id, ChatMessage::User(user_message.clone()));

        // Add processing indicator with animated spinner
        let _ = state.add_message_to_session(
            session.id,
            ChatMessage::Processing {
                message: "Processing your request...".to_string(),
                spinner_index: 0,
            },
        );

        // Update the display immediately
        update_ui_displays(siv);

        // Start animated processing with live updates
        start_live_processing(siv, session.id, user_message, state);
    }
}

pub fn create_new_chat_dialog(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::around(
            EditView::new()
                .content("New Chat")
                .with_name("new_chat_name")
                .fixed_width(30),
        )
        .title("Create New Chat")
        .button("Create", |s| {
            let name = s
                .find_name::<EditView>("new_chat_name")
                .and_then(|v| Some(v.get_content().to_string()))
                .unwrap_or_else(|| "Unnamed Chat".to_string());

            s.pop_layer();

            s.with_user_data(|state: &mut AdvancedChatState| {
                if let Err(e) = state.create_session(name) {
                    error!("Failed to create new session: {}", e);
                }
            });

            update_ui_displays(s);
        })
        .button("Cancel", |s| {
            s.pop_layer();
        }),
    );
}

pub fn resume_agent(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let command = AgentCommand::ResumeAgent {
                session_id: session.id,
            };
            // Send command using sync method to avoid runtime conflicts
            state.send_agent_command_sync(command);
        }
    });
}

pub fn pause_agent(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let command = AgentCommand::PauseAgent {
                session_id: session.id,
            };
            // Send command using sync method to avoid runtime conflicts
            state.send_agent_command_sync(command);
        }
    });
}

pub fn stop_agent(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let command = AgentCommand::StopAgent {
                session_id: session.id,
            };
            // Send command using sync method to avoid runtime conflicts
            state.send_agent_command_sync(command);
        }
    });
}

pub fn start_recording(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session_id) = state.active_session_id.read().ok().and_then(|id| *id) {
            if let Ok(mut sessions) = state.sessions.write() {
                if let Some(session) = sessions.get_mut(&session_id) {
                    let filename = format!(
                        "osvm_chat_{}_{}.log",
                        session.name.replace(' ', "_"),
                        chrono::Utc::now().format("%Y%m%d_%H%M%S")
                    );

                    if let Err(e) = session.start_recording(filename) {
                        error!("Failed to start recording: {}", e);
                    }
                }
            }
        }
    });
}

pub fn stop_recording(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session_id) = state.active_session_id.read().ok().and_then(|id| *id) {
            if let Ok(mut sessions) = state.sessions.write() {
                if let Some(session) = sessions.get_mut(&session_id) {
                    session.stop_recording();
                }
            }
        }
    });
}

pub fn clear_current_chat(siv: &mut Cursive) {
    // Show confirmation dialog before clearing
    siv.add_layer(
        Dialog::text(
            "Are you sure you want to clear all messages in this chat?\n\
            This action cannot be undone.\n\n\
            💡 Tip: Use 'Export Chat' first if you want to save the conversation."
        )
        .title("⚠ Confirm Clear Chat")
        .button("Yes, Clear All", |s| {
            s.pop_layer(); // Close confirmation dialog

            let cleared = s.with_user_data(|state: &mut AdvancedChatState| {
                if let Some(session_id) = state.active_session_id.read().ok().and_then(|id| *id) {
                    if let Ok(mut sessions) = state.sessions.write() {
                        if let Some(session) = sessions.get_mut(&session_id) {
                            let message_count = session.messages.len();
                            session.messages.clear();
                            session.add_message(ChatMessage::System(format!(
                                "Chat cleared ({} messages removed)",
                                message_count
                            )));
                            return Some(message_count);
                        }
                    }
                }
                None
            });

            update_ui_displays(s);

            // Show success feedback
            if let Some(Some(count)) = cleared {
                s.add_layer(
                    Dialog::info(format!("✓ Chat cleared successfully\n{} messages removed", count))
                        .title("Cleared")
                        .button("OK", |s| {
                            s.pop_layer();
                        })
                );
            }
        })
        .button("Cancel", |s| {
            s.pop_layer();
        })
    );
}

pub fn show_settings(siv: &mut Cursive) {
    let mut settings_layout = LinearLayout::vertical();

    // MCP Server Management section
    settings_layout.add_child(TextView::new("MCP Server Management"));
    settings_layout.add_child(DummyView.fixed_height(1));

    let mcp_buttons = LinearLayout::horizontal()
        .child(Button::new("Add Server", show_add_mcp_server_dialog))
        .child(DummyView.fixed_width(2))
        .child(Button::new("Manage Servers", show_mcp_server_list))
        .child(DummyView.fixed_width(2))
        .child(Button::new("Refresh Tools", refresh_mcp_tools));

    settings_layout.add_child(mcp_buttons);
    settings_layout.add_child(DummyView.fixed_height(1));

    // Current server status
    settings_layout.add_child(TextView::new("Current MCP Servers:"));
    settings_layout.add_child(
        Panel::new(TextView::new("Loading server status...").with_name("mcp_server_status"))
            .max_height(5),
    );
    settings_layout.add_child(DummyView.fixed_height(1));

    // System settings
    settings_layout.add_child(TextView::new("System Settings"));
    settings_layout.add_child(DummyView.fixed_height(1));
    settings_layout.add_child(TextView::new("AI Model: GPT-4"));
    settings_layout.add_child(TextView::new("Debug Mode: Enabled"));
    settings_layout.add_child(TextView::new("Max History: 1000 messages"));
    settings_layout.add_child(TextView::new("Recording Directory: ./recordings/"));
    settings_layout.add_child(DummyView.fixed_height(1));

    // Chat management
    settings_layout.add_child(TextView::new("Chat Management"));
    settings_layout.add_child(DummyView.fixed_height(1));

    let chat_buttons = LinearLayout::horizontal()
        .child(Button::new("Export All Chats", export_all_chats))
        .child(DummyView.fixed_width(2))
        .child(Button::new("Clear All Chats", clear_all_chats));

    settings_layout.add_child(chat_buttons);

    siv.add_layer(
        Dialog::around(settings_layout)
            .title("OSVM Agent Settings")
            .button("Close", |s| {
                s.pop_layer();
            }),
    );

    // Update MCP server status after dialog is shown
    update_mcp_server_status_in_settings(siv);
}

pub fn export_chat(siv: &mut Cursive) {
    let export_result = siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let filename = format!(
                "osvm_chat_export_{}_{}.json",
                session.name.replace(' ', "_"),
                chrono::Utc::now().format("%Y%m%d_%H%M%S")
            );

            match serde_json::to_string_pretty(&session) {
                Ok(json_content) => match std::fs::write(&filename, json_content) {
                    Ok(_) => {
                        info!("Chat exported to {}", filename);
                        Ok((filename, None::<String>))
                    }
                    Err(e) => {
                        error!("Failed to write export file: {}", e);
                        Err(format!("Failed to write export file: {}", e))
                    }
                },
                Err(e) => {
                    error!("Failed to serialize session: {}", e);
                    Err(format!("Failed to serialize session: {}", e))
                }
            }
        } else {
            Err("No active session found".to_string())
        }
    });

    match export_result {
        Some(Ok((filename, None))) => {
            siv.add_layer(
                Dialog::info(format!("✅ Chat exported successfully to:\n{}", filename))
                    .title("Export Complete")
                    .button("OK", |s| {
                        s.pop_layer();
                    }),
            );
        }
        Some(Ok((_, Some(e)))) => {
            siv.add_layer(
                Dialog::info(format!("❌ Failed to export chat:\n{}", e))
                    .title("Export Failed")
                    .button("OK", |s| {
                        s.pop_layer();
                    }),
            );
        }
        Some(Err(e)) => {
            siv.add_layer(
                Dialog::info(format!("❌ Failed to export chat:\n{}", e))
                    .title("Export Failed")
                    .button("OK", |s| {
                        s.pop_layer();
                    }),
            );
        }
        None => {
            siv.add_layer(
                Dialog::info("❌ No active session found")
                    .title("Export Failed")
                    .button("OK", |s| {
                        s.pop_layer();
                    }),
            );
        }
    }
}

pub fn show_advanced_help(siv: &mut Cursive) {
    let help_text = "🚀 OSVM Advanced Agent Chat - Complete Guide\n\n\
        ═══════════════════════════════════════════════════\n\
        📋 KEYBOARD SHORTCUTS QUICK REFERENCE\n\
        ═══════════════════════════════════════════════════\n\n\
        🔤 Navigation:\n\
        • Tab             → Switch between chat list and input\n\
        • Shift+Tab       → Reverse navigation\n\
        • ↑/↓ (planned)   → Navigate message history\n\n\
        🎯 Actions:\n\
        • Alt+R           → Retry last message\n\
        • Alt+C           → Copy last message to clipboard\n\
        • Alt+D           → Delete last message (with confirmation)\n\
        • Alt+F           → Fork/branch current conversation\n\
        • Alt+X           → Emergency clear stuck processing\n\
        • Alt+M           → Switch to standard mode info\n\n\
        💡 Suggestions:\n\
        • Ctrl+1-5        → Insert suggestion at cursor (primary)\n\
        • Alt+1-5         → Insert suggestion at cursor (alternate)\n\
        • Esc             → Hide suggestions panel\n\n\
        📸 Utilities:\n\
        • F10             → Open context menu (Copy, Retry, Clear)\n\
        • F12             → Take screenshot of chat window\n\
        • Ctrl+Q          → Quit application\n\n\
        ═══════════════════════════════════════════════════\n\
        🤖 AGENT CONTROLS\n\
        ═══════════════════════════════════════════════════\n\n\
        • Run Button      → Resume/start agent processing\n\
        • Pause Button    → Temporarily pause agent\n\
        • Stop Button     → Stop current agent task\n\n\
        ═══════════════════════════════════════════════════\n\
        ⏺ SESSION RECORDING\n\
        ═══════════════════════════════════════════════════\n\n\
        • Record Button   → Start recording session to file\n\
        • Stop Rec Button → Stop recording and save\n\
        • Export Chat     → Save current chat as JSON\n\n\
        ═══════════════════════════════════════════════════\n\
        🎨 STATUS ICONS\n\
        ═══════════════════════════════════════════════════\n\n\
        Agent States:\n\
        • ◉ Idle          → Ready for new tasks\n\
        • ◐ Thinking      → Analyzing your request\n\
        • ◑ Planning      → Creating execution plan\n\
        • ▶ Executing     → Running tools/commands\n\
        • ◯ Waiting       → Awaiting response\n\
        • ⏸ Paused        → Operations suspended\n\
        • ⚠ Error         → Something went wrong\n\n\
        ═══════════════════════════════════════════════════\n\
        ✨ ADVANCED FEATURES\n\
        ═══════════════════════════════════════════════════\n\n\
        • 🧠 AI-powered tool planning and execution\n\
        • 🔄 Background agent processing\n\
        • 💬 Multi-session chat support\n\
        • 📦 MCP server integration (blockchain tools)\n\
        • 🎨 Multiple theme support (/theme commands)\n\
        • 🔧 Direct tool testing from MCP panel\n\
        • 📊 Real-time system status monitoring\n\n\
        ═══════════════════════════════════════════════════\n\
        💬 MESSAGE ACTIONS (shown under each message)\n\
        ═══════════════════════════════════════════════════\n\n\
        User Messages:\n\
        • [R]etry  → Send message again\n\
        • [C]opy   → Copy to clipboard\n\
        • [D]elete → Remove message (with confirmation)\n\n\
        Agent Messages:\n\
        • [F]ork   → Branch conversation from this point\n\
        • [C]opy   → Copy to clipboard\n\
        • [R]etry  → Request new response\n\
        • [D]elete → Remove message (with confirmation)\n\n\
        ═══════════════════════════════════════════════════\n\
        🆘 TROUBLESHOOTING\n\
        ═══════════════════════════════════════════════════\n\n\
        • Agent stuck? → Use Alt+X to emergency clear\n\
        • UI frozen? → Terminal may be too small (min 60x15)\n\
        • No MCP tools? → Run 'osvm mcp setup' first\n\
        • Missing features? → Check 'osvm chat --help'\n\n\
        Press F1 or '?' key to show this help anytime!";

    let mut help_layout = LinearLayout::vertical();
    help_layout.add_child(
        ScrollView::new(TextView::new(help_text))
            .scroll_strategy(cursive::view::scroll::ScrollStrategy::StickToBottom)
    );

    siv.add_layer(
        Dialog::around(help_layout)
            .title("📖 Complete Help & Keyboard Shortcuts")
            .button("Got it!", |s| {
                s.pop_layer();
            })
            .button("📋 Print to Console", |s| {
                // Print concise shortcuts to console for quick reference
                println!("\n╔════════════════════════════════════════════════════════════╗");
                println!("║  🚀 OSVM Advanced Chat - Quick Keyboard Reference        ║");
                println!("╠════════════════════════════════════════════════════════════╣");
                println!("║  Navigation:  Tab/Shift+Tab  |  Actions: Alt+R/C/D/F     ║");
                println!("║  Suggestions: Ctrl/Alt+1-5   |  Utils: F10 (menu) F12 📸 ║");
                println!("║  Emergency:   Alt+X (clear)  |  Help: F1 or ? key        ║");
                println!("╚════════════════════════════════════════════════════════════╝\n");
            })
            .max_width(90)
            .max_height(40),
    );
}

/// Show quick keyboard shortcuts hint panel (called on startup or F1)
pub fn show_keyboard_shortcuts_hint(siv: &mut Cursive) {
    let hint_text = "💡 Quick Shortcuts: Tab/Shift+Tab=Navigate | Alt+R/C/D/F=Actions | Ctrl+1-5=Suggestions | F10=Menu | F12=Screenshot | Alt+X=Clear | ?=Help";

    siv.add_layer(
        Dialog::info(hint_text)
            .title("⌨️ Keyboard Shortcuts")
            .button("Show Full Help", |s| {
                s.pop_layer();
                show_advanced_help(s);
            })
            .button("OK", |s| {
                s.pop_layer();
            })
    );
}

// MCP Server Management Functions

pub fn show_add_mcp_server_dialog(siv: &mut Cursive) {
    let mut form_layout = LinearLayout::vertical();

    form_layout.add_child(TextView::new("Add New MCP Server"));
    form_layout.add_child(DummyView.fixed_height(1));

    form_layout.add_child(
        LinearLayout::horizontal()
            .child(TextView::new("Name: ").fixed_width(15))
            .child(EditView::new().with_name("server_name").fixed_width(30)),
    );

    form_layout.add_child(
        LinearLayout::horizontal()
            .child(TextView::new("URL: ").fixed_width(15))
            .child(EditView::new().with_name("server_url").fixed_width(30)),
    );

    form_layout.add_child(
        LinearLayout::horizontal()
            .child(TextView::new("Type: ").fixed_width(15))
            .child(
                SelectView::<String>::new()
                    .item("HTTP", "http".to_string())
                    .item("WebSocket", "websocket".to_string())
                    .item("Stdio", "stdio".to_string())
                    .with_name("server_type")
                    .fixed_width(15),
            ),
    );

    siv.add_layer(
        Dialog::around(form_layout)
            .title("Add MCP Server")
            .button("Add", |s| {
                add_mcp_server_from_form(s);
            })
            .button("Cancel", |s| {
                s.pop_layer();
            }),
    );
}

pub fn show_mcp_server_list(siv: &mut Cursive) {
    let mut server_list = SelectView::<String>::new();

    // Populate with current servers from state
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Ok(mcp_service) = state.mcp_service.try_lock() {
            for (server_id, config) in mcp_service.list_servers() {
                let status = if config.enabled { "✓" } else { "✗" };
                let label = format!("{} {} ({:?})", status, config.name, config.transport_type);
                server_list.add_item(label, server_id.clone());
            }
        }
    });

    siv.add_layer(
        Dialog::around(server_list.with_name("server_list"))
            .title("Manage MCP Servers")
            .button("Enable/Disable", toggle_mcp_server)
            .button("Remove", remove_mcp_server)
            .button("Close", |s| {
                s.pop_layer();
            }),
    );
}

pub fn refresh_mcp_tools(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        // Spawn async task to refresh tools
        let state_clone = state.clone();
        tokio::spawn(async move {
            if let Err(e) = state_clone.refresh_tools_from_mcp().await {
                error!("Failed to refresh MCP tools: {}", e);
            }
        });
    });

    siv.add_layer(
        Dialog::text("Refreshing MCP tools in background...")
            .title("MCP Tools")
            .button("OK", |s| {
                s.pop_layer();
            }),
    );
}

pub fn export_all_chats(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
        let filename = format!("osvm_all_chats_export_{}.json", timestamp);

        let sessions = state.sessions.read().unwrap();
        match serde_json::to_string_pretty(&*sessions) {
            Ok(json_content) => match std::fs::write(&filename, json_content) {
                Ok(_) => {
                    info!("All chats exported to {}", filename);
                }
                Err(e) => {
                    error!("Failed to write export file: {}", e);
                }
            },
            Err(e) => {
                error!("Failed to serialize sessions: {}", e);
            }
        }
    });

    siv.add_layer(
        Dialog::text("All chats exported successfully!")
            .title("Export Complete")
            .button("OK", |s| {
                s.pop_layer();
            }),
    );
}

pub fn clear_all_chats(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::text(
            "Are you sure you want to clear all chat sessions?\nThis action cannot be undone.",
        )
        .title("Confirm Clear All")
        .button("Yes, Clear All", |s| {
            s.with_user_data(|state: &mut AdvancedChatState| {
                if let Ok(mut sessions) = state.sessions.write() {
                    sessions.clear();
                }
                if let Ok(mut active_id) = state.active_session_id.write() {
                    *active_id = None;
                }
            });
            s.pop_layer();
            update_ui_displays(s);
        })
        .button("Cancel", |s| {
            s.pop_layer();
        }),
    );
}

pub fn update_mcp_server_status_in_settings(siv: &mut Cursive) {
    let status_text = siv
        .with_user_data(|state: &mut AdvancedChatState| {
            let mut status_text = String::new();

            if let Ok(mcp_service) = state.mcp_service.try_lock() {
                for (server_id, config) in mcp_service.list_servers() {
                    let status = if config.enabled {
                        "✓ Enabled"
                    } else {
                        "✗ Disabled"
                    };
                    status_text.push_str(&format!(
                        "• {} ({:?}): {}\n",
                        config.name, config.transport_type, status
                    ));
                }
            }

            if status_text.is_empty() {
                status_text = "No MCP servers configured".to_string();
            }

            status_text
        })
        .unwrap_or_else(|| "Failed to load MCP server status".to_string());

    // Update the status display outside the closure
    if let Some(mut status_view) = siv.find_name::<TextView>("mcp_server_status") {
        status_view.set_content(status_text);
    }
}

// Helper functions for MCP management

fn add_mcp_server_from_form(siv: &mut Cursive) {
    let name = siv
        .find_name::<EditView>("server_name")
        .map(|v| v.get_content().to_string())
        .unwrap_or_default();

    let url = siv
        .find_name::<EditView>("server_url")
        .map(|v| v.get_content().to_string())
        .unwrap_or_default();

    let server_type = siv
        .find_name::<SelectView<String>>("server_type")
        .and_then(|v| v.selection())
        .map(|s| s.as_str().to_string())
        .unwrap_or_else(|| "http".to_string());

    if name.is_empty() || url.is_empty() {
        siv.add_layer(
            Dialog::text("Please fill in all required fields.")
                .title("Error")
                .button("OK", |s| {
                    s.pop_layer();
                }),
        );
        return;
    }

    // Add the server to the MCP service
    let result = siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Ok(mut mcp_service) = state.mcp_service.try_lock() {
            use crate::services::mcp_service::{McpServerConfig, McpTransportType};

            let transport_type = match server_type.as_str() {
                "websocket" => McpTransportType::Websocket,
                "stdio" => McpTransportType::Stdio,
                _ => McpTransportType::Http,
            };

            let config = McpServerConfig {
                name: name.clone(),
                url: url.clone(),
                transport_type,
                auth: None,
                enabled: true,
                extra_config: std::collections::HashMap::new(),
                github_url: None,
                local_path: None,
            };

            mcp_service.add_server(name.clone(), config);
            Ok(())
        } else {
            Err(anyhow::anyhow!("Could not access MCP service"))
        }
    });

    siv.pop_layer(); // Close the add server dialog

    match result {
        Some(Ok(_)) => {
            siv.add_layer(
                Dialog::text(&format!(
                    "✅ Server '{}' added successfully!\nURL: {}\nType: {}",
                    name, url, server_type
                ))
                .title("Server Added")
                .button("OK", |s| {
                    s.pop_layer();
                }),
            );
        }
        Some(Err(e)) => {
            siv.add_layer(
                Dialog::text(&format!("❌ Failed to add server: {}", e))
                    .title("Error")
                    .button("OK", |s| {
                        s.pop_layer();
                    }),
            );
        }
        None => {
            siv.add_layer(
                Dialog::text("❌ Failed to access application state")
                    .title("Error")
                    .button("OK", |s| {
                        s.pop_layer();
                    }),
            );
        }
    }
}

fn toggle_mcp_server(siv: &mut Cursive) {
    if let Some(server_list) = siv.find_name::<SelectView<String>>("server_list") {
        if let Some(selection) = server_list.selection() {
            let server_id = selection.as_str().to_string();

            let result = siv.with_user_data(|state: &mut AdvancedChatState| {
                if let Ok(mut mcp_service) = state.mcp_service.try_lock() {
                    // Get current state and toggle it
                    let current_enabled = mcp_service
                        .get_server(&server_id)
                        .map(|config| config.enabled)
                        .unwrap_or(false);
                    mcp_service.toggle_server(&server_id, !current_enabled)
                } else {
                    Err(anyhow::anyhow!("Could not access MCP service"))
                }
            });

            match result {
                Some(Ok(_)) => {
                    siv.add_layer(
                        Dialog::text(&format!(
                            "✅ Server '{}' status toggled successfully!",
                            server_id
                        ))
                        .title("Server Updated")
                        .button("OK", |s| {
                            s.pop_layer();
                            s.pop_layer(); // Close server list to refresh
                            show_mcp_server_list(s); // Reopen with updated list
                        }),
                    );
                }
                Some(Err(e)) => {
                    siv.add_layer(
                        Dialog::text(&format!("❌ Failed to toggle server: {}", e))
                            .title("Error")
                            .button("OK", |s| {
                                s.pop_layer();
                            }),
                    );
                }
                None => {
                    siv.add_layer(
                        Dialog::text("❌ Failed to access application state")
                            .title("Error")
                            .button("OK", |s| {
                                s.pop_layer();
                            }),
                    );
                }
            }
        }
    }
}

fn remove_mcp_server(siv: &mut Cursive) {
    if let Some(server_list) = siv.find_name::<SelectView<String>>("server_list") {
        if let Some(selection) = server_list.selection() {
            let server_id = selection.as_str().to_string();

            siv.add_layer(
                Dialog::text(&format!(
                    "Are you sure you want to remove server '{}'?",
                    server_id
                ))
                .title("Confirm Removal")
                .button("Yes, Remove", {
                    let server_id = server_id.clone();
                    move |s| {
                        let result = s.with_user_data(|state: &mut AdvancedChatState| {
                            if let Ok(mut mcp_service) = state.mcp_service.try_lock() {
                                mcp_service.remove_server(&server_id);
                                Ok(())
                            } else {
                                Err(anyhow::anyhow!("Could not access MCP service"))
                            }
                        });

                        match result {
                            Some(Ok(_)) => {
                                s.add_layer(
                                    Dialog::text(&format!(
                                        "✅ Server '{}' removed successfully!",
                                        server_id
                                    ))
                                    .title("Server Removed")
                                    .button("OK", |s| {
                                        s.pop_layer(); // Close success dialog
                                        s.pop_layer(); // Close confirmation dialog
                                        s.pop_layer(); // Close server list
                                        show_mcp_server_list(s); // Reopen with updated list
                                    }),
                                );
                            }
                            Some(Err(e)) => {
                                s.add_layer(
                                    Dialog::text(&format!("❌ Failed to remove server: {}", e))
                                        .title("Error")
                                        .button("OK", |s| {
                                            s.pop_layer();
                                        }),
                                );
                            }
                            None => {
                                s.add_layer(
                                    Dialog::text("❌ Failed to access application state")
                                        .title("Error")
                                        .button("OK", |s| {
                                            s.pop_layer();
                                        }),
                                );
                            }
                        }
                    }
                })
                .button("Cancel", |s| {
                    s.pop_layer();
                }),
            );
        }
    }
}

// Right-click context menu support
pub fn show_context_menu(siv: &mut Cursive, position: cursive::Vec2) {
    use cursive::views::MenuPopup;

    // Create context menu based on what's under the cursor
    let mut menu = cursive::views::Dialog::text("Quick Actions").title("Context Menu");

    // Add common actions
    menu = menu.button("Copy Last Message", |s| {
        s.pop_layer(); // Close context menu first
                       // Get state reference without borrowing mutably
        let state_opt = s.user_data::<AdvancedChatState>().cloned();
        if let Some(state) = state_opt {
            copy_last_message(s, state);
        }
    });

    menu = menu.button("Retry Last Message", |s| {
        s.pop_layer(); // Close context menu first
                       // Get state reference without borrowing mutably
        let state_opt = s.user_data::<AdvancedChatState>().cloned();
        if let Some(state) = state_opt {
            retry_last_message(s, state);
        }
    });

    menu = menu.button("Clear Chat", |s| {
        s.pop_layer(); // Close context menu first
        clear_current_chat(s);
    });

    menu = menu.button("Cancel", |s| {
        s.pop_layer();
    });

    // Show the context menu
    siv.add_layer(menu);
}

// Live processing with animated feedback
pub fn start_live_processing(
    siv: &mut Cursive,
    session_id: uuid::Uuid,
    user_input: String,
    state: AdvancedChatState,
) {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;
    use std::thread;
    use std::time::Duration;

    let spinner_counter = Arc::new(AtomicUsize::new(0));

    // Get a callback sink to update UI from background thread
    let cb_sink = siv.cb_sink().clone();

    // Clone for background thread
    let state_clone = state.clone();
    let spinner_clone = spinner_counter.clone();

    // Start spinner animation in background thread
    thread::spawn(move || {
        let stages = [
            "Analyzing your request...",
            "Searching for relevant information...",
            "Processing with AI...",
            "Generating response...",
            "Finalizing answer...",
            "Almost done...",
            "Wrapping up...",
            "Just a moment...",
            "Completing your request...",
            "Preparing response...",
            "Finalizing response...",
            "Almost there...",
            "Just a moment longer...",
            "Completing the process...",
            "Preparing the final response...",
            "Finishing up...",
            "Polishing response...",
            "Nearly complete...",
            "Final touches...",
            "Quality check...",
            "Optimizing results...",
            "Validating response...",
            "Last steps...",
            "Wrapping up processing...",
        ];

        let mut stage_index = 0;
        let mut update_counter = 0;
        let start_time = std::time::Instant::now();
        const MAX_SPINNER_DURATION: std::time::Duration = std::time::Duration::from_secs(45);

        loop {
            // Check if processing message still exists in session BEFORE updating
            let still_processing = state_clone
                .get_session_by_id(session_id)
                .map(|session| {
                    session
                        .messages
                        .iter()
                        .any(|msg| matches!(msg, ChatMessage::Processing { .. }))
                })
                .unwrap_or(false);

            if !still_processing {
                break; // Exit loop when processing message is removed
            }

            // Safety timeout: force exit after 30 seconds
            if start_time.elapsed() > MAX_SPINNER_DURATION {
                // Force remove processing message and exit
                let _ = state_clone.remove_last_processing_message(session_id);
                break;
            }

            let spinner_index = spinner_clone.fetch_add(1, Ordering::Relaxed);

            // Change message every 30 updates (roughly 3 seconds)
            if update_counter % 30 == 0 {
                stage_index = (stage_index + 1) % stages.len();
            }
            update_counter += 1;

            let message = stages[stage_index].to_string();

            // Only update if processing message still exists
            // This check prevents re-adding the message after agent removes it
            if still_processing {
                // Double-check: only update if message still exists (race condition guard)
                let still_processing_after_check = state_clone
                    .get_session_by_id(session_id)
                    .map(|session| {
                        session
                            .messages
                            .iter()
                            .any(|msg| matches!(msg, ChatMessage::Processing { .. }))
                    })
                    .unwrap_or(false);

                if still_processing_after_check {
                    let _ = state_clone.update_processing_message(
                        session_id,
                        message.clone(),
                        spinner_index % 10,
                    );
                }
            }

            // Request UI update
            cb_sink
                .send(Box::new(move |siv| {
                    update_ui_displays(siv);
                }))
                .ok();

            thread::sleep(Duration::from_millis(100));
        }
    });

    // Use the proper agent worker flow which calls AI service first
    // Send to agent worker instead of handling inline
    let command = AgentCommand::ProcessInput {
        session_id,
        input: user_input,
    };

    // Send command through the proper channel
    state.send_agent_command_sync(command);

    // NOTE: We don't stop the spinner here - it will keep running and be cleaned up
    // when the agent worker completes and removes the processing message
}

// Generate mock response for demo
fn generate_mock_response(input: &str) -> String {
    let input_lower = input.to_lowercase();

    if input_lower.contains("balance") {
        "I'll check your SOL balance for you. According to the latest data, your wallet shows that all SOL is gone with some pending transactions.".to_string()
    } else if input_lower.contains("stake") || input_lower.contains("staking") {
        "For staking your SOL:\n\n1. Choose a validator with good performance\n2. Use 'solana delegate-stake' command\n3. Minimum stake amount is 0.001 SOL\n\nCurrent APY is around 6.8%. Would you like help selecting a validator?".to_string()
    } else if input_lower.contains("send") || input_lower.contains("transfer") {
        "To send SOL, you'll need the recipient's wallet address and ensure you have enough balance to cover both the amount and transaction fees. Use 'solana transfer <RECIPIENT_ADDRESS> <AMOUNT>' command.".to_string()
    } else if input_lower.contains("validator") {
        "You can check validator performance using 'solana validators' command. Look for high uptime and low commission rates. Would you like me to list some top validators for you?".to_string()
    } else if input_lower.contains("rewards") {
        "Staking rewards are typically distributed every 2 days. You can check your accumulated rewards using 'solana stake-account <STAKE_ACCOUNT_ADDRESS>' command.".to_string()
    } else if input_lower.contains("history") || input_lower.contains("transactions") {
        "You can export your transaction history using 'solana transaction-history' command. Would you like me to guide you through the steps?".to_string()
    } else if input_lower.contains("market") || input_lower.contains("analysis") {
        "The Solana market has been showing steady growth with increasing adoption. Recent trends indicate a bullish sentiment, but always consider market volatility.".to_string()
    } else if input_lower.contains("security") || input_lower.contains("wallet") {
        "For wallet security, ensure you use strong passwords, enable two-factor authentication,and regularly update your software. Avoid sharing your private keys.".to_string()
    } else {
        format!("I understand you're asking about: '{}'\n\nLet me help you with that. Based on your query, I can provide information about Solana operations, wallet management, and blockchain interactions.", input)
    }
}

// Generate contextual suggestions based on user input
fn generate_context_suggestions(input: &str) -> Vec<String> {
    let input_lower = input.to_lowercase();

    if input_lower.contains("balance") {
        vec![
            "Show recent transactions".to_string(),
            "Check staking rewards".to_string(),
            "Get current SOL price".to_string(),
            "Show wallet addresses".to_string(),
            "Export transaction history".to_string(),
        ]
    } else if input_lower.contains("transaction") {
        vec![
            "Filter by amount".to_string(),
            "Show transaction details".to_string(),
            "Check transaction status".to_string(),
            "Export as CSV".to_string(),
            "Analyze spending patterns".to_string(),
        ]
    } else {
        vec![
            "What's my wallet balance?".to_string(),
            "Show recent transactions".to_string(),
            "Current SOL price".to_string(),
            "How to stake SOL?".to_string(),
            "Check validator performance".to_string(),
        ]
    }
}

// Auto-suggestions as user types - now with AI assistance
pub fn setup_input_suggestions(siv: &mut Cursive, state: AdvancedChatState) {
    // Add input change callback to show suggestions as user types
    if let Some(mut input) = siv.find_name::<EditView>("input") {
        let state_clone = state.clone();
        input.set_on_edit(move |_siv, content, _cursor| {
            if content.len() >= 3 {
                // Start suggesting after 3 characters
                let suggestions = generate_smart_input_suggestions(content, state_clone.clone());
                if let Ok(mut current_suggestions) = state_clone.current_suggestions.write() {
                    *current_suggestions = suggestions;
                }
                if let Ok(mut vis) = state_clone.suggestions_visible.write() {
                    *vis = !content.is_empty();
                }
            } else {
                if let Ok(mut vis) = state_clone.suggestions_visible.write() {
                    *vis = false;
                }
            }
        });
    }
}

// Generate suggestions based on partial input
fn generate_input_suggestions(partial: &str) -> Vec<String> {
    let partial_lower = partial.to_lowercase();
    let mut suggestions = Vec::new();

    let common_queries = vec![
        ("what's my balance", "What's my wallet balance?"),
        ("show transactions", "Show recent transactions"),
        ("current price", "What's the current SOL price?"),
        ("how to stake", "How do I stake my SOL?"),
        ("send sol", "Send SOL to another address"),
        ("check rewards", "Check my staking rewards"),
        ("validator info", "Get validator information"),
        ("transaction history", "Export transaction history"),
        ("market analysis", "Analyze market trends"),
        ("wallet security", "Check wallet security settings"),
    ];

    for (pattern, suggestion) in common_queries {
        if pattern.contains(&partial_lower) || suggestion.to_lowercase().contains(&partial_lower) {
            suggestions.push(suggestion.to_string());
            if suggestions.len() >= 5 {
                break;
            }
        }
    }

    if suggestions.is_empty() {
        suggestions = vec![
            "What's my wallet balance?".to_string(),
            "Show recent transactions".to_string(),
            "Current SOL price".to_string(),
            "How to stake SOL?".to_string(),
            "Check staking rewards".to_string(),
        ];
    }

    suggestions
}

// Generate smart suggestions with AI assistance for partial input
fn generate_smart_input_suggestions(partial: &str, state: AdvancedChatState) -> Vec<String> {
    // For short inputs, use fast pattern matching
    if partial.len() < 5 {
        return generate_input_suggestions(partial);
    }

    // For longer inputs, try AI assistance in background but return fast suggestions immediately
    let ai_service = state.ai_service.clone();
    let partial_owned = partial.to_string();

    // Spawn background AI suggestion generation (non-blocking)
    let state_clone = state.clone();
    std::thread::spawn(move || {
        let rt = match tokio::runtime::Runtime::new() {
            Ok(rt) => rt,
            Err(_) => return, // Silently fail on runtime creation error
        };

        let ai_suggestions = rt.block_on(async {
            let suggestion_query = format!(
                "Complete this Solana/blockchain query: '{}'. \
                Provide 3-5 likely completions, one per line, under 60 characters each.",
                partial_owned
            );

            ai_service
                .query_with_debug(&suggestion_query, false)
                .await
                .unwrap_or_default()
        });

        // Parse AI suggestions and update state
        if !ai_suggestions.is_empty() {
            let suggestions: Vec<String> = ai_suggestions
                .lines()
                .filter_map(|line| {
                    let trimmed = line.trim();
                    if !trimmed.is_empty() && trimmed.len() <= 60 {
                        Some(trimmed.to_string())
                    } else {
                        None
                    }
                })
                .take(5)
                .collect();

            if !suggestions.is_empty() {
                // Update suggestions in state
                if let Ok(mut current_suggestions) = state_clone.current_suggestions.write() {
                    *current_suggestions = suggestions;
                }
            }
        }
    });

    // Return immediate pattern-based suggestions
    generate_input_suggestions(partial)
}

// Execute AI plan with XML parsing and tool execution
async fn execute_ai_plan(
    ai_response: String,
    session_id: uuid::Uuid,
    state: AdvancedChatState,
) -> String {
    use regex::Regex;

    // Extract plan and response sections with better regex
    let plan_regex = Regex::new(r"(?s)<plan>(.*?)</plan>").unwrap();
    let response_regex = Regex::new(r"(?s)<response>(.*?)</response>").unwrap();

    let plan_content = plan_regex
        .captures(&ai_response)
        .and_then(|cap| cap.get(1))
        .map(|m| m.as_str().trim())
        .unwrap_or("");

    let response_content = response_regex
        .captures(&ai_response)
        .and_then(|cap| cap.get(1))
        .map(|m| m.as_str().trim())
        .unwrap_or(&ai_response); // If no proper XML format, return the whole response

    // If plan is empty, return the response directly
    if plan_content.is_empty() {
        return response_content.to_string();
    }

    // Parse tool calls from plan
    let tool_regex =
        Regex::new(r#"(?s)<tool name="([^"]+)" server="([^"]+)">(.*?)</tool>"#).unwrap();
    let param_regex = Regex::new(r#"<param name="([^"]+)">([^<]*)</param>"#).unwrap();

    let mut execution_results = Vec::new();
    let mut final_response = response_content.to_string();

    let tool_matches: Vec<_> = tool_regex.captures_iter(&plan_content).collect();

    for captures in tool_matches {
        let tool_name = captures.get(1).unwrap().as_str();
        let server_id = captures.get(2).unwrap().as_str();
        let params_section = captures.get(3).unwrap().as_str();

        // Extract parameters
        let mut params = std::collections::HashMap::new();
        for param_match in param_regex.captures_iter(params_section) {
            let param_name = param_match.get(1).unwrap().as_str();
            let param_value = param_match.get(2).unwrap().as_str();
            params.insert(
                param_name.to_string(),
                serde_json::Value::String(param_value.to_string()),
            );
        }

        let execution_id = uuid::Uuid::new_v4().to_string();

        // Add tool execution message
        let _ = state.add_message_to_session(
            session_id,
            ChatMessage::ToolCall {
                tool_name: tool_name.to_string(),
                description: format!("Executing {} on server {}", tool_name, server_id),
                args: Some(serde_json::Value::Object(
                    params.clone().into_iter().collect(),
                )),
                execution_id: execution_id.clone(),
            },
        );

        // Execute the tool
        let tool_result = execute_mcp_tool(server_id, tool_name, params, state.clone()).await;

        // Add tool result message
        let _ = state.add_message_to_session(
            session_id,
            ChatMessage::ToolResult {
                tool_name: tool_name.to_string(),
                result: serde_json::Value::String(tool_result.clone()),
                execution_id,
            },
        );

        execution_results.push(format!("{}: {}", tool_name, tool_result));
    }

    // Combine response with tool results
    if !execution_results.is_empty() {
        final_response.push_str("\n\nTool Execution Results:\n");
        for result in execution_results {
            final_response.push_str(&format!("• {}\n", result));
        }
    }

    final_response
}

// Execute MCP tool (mock implementation for now)
async fn execute_mcp_tool(
    server_id: &str,
    tool_name: &str,
    params: std::collections::HashMap<String, serde_json::Value>,
    _state: AdvancedChatState,
) -> String {
    // Mock tool execution - replace with real MCP calls later
    match (server_id, tool_name) {
        ("osvm-api", "get_balance") => {
            "2.45 SOL (approximately $54.89 USD)".to_string()
        }
        ("osvm-api", "get_recent_transactions") => {
            "Found 5 recent transactions:\n1. Received 1.5 SOL (2 hours ago)\n2. Sent 0.3 SOL to DeFi (1 day ago)\n3. Staking reward 0.025 SOL (2 days ago)\n4. Swapped 100 USDC for SOL (3 days ago)\n5. NFT purchase 0.8 SOL (1 week ago)".to_string()
        }
        ("osvm-api", "get_transaction") => {
            let tx_sig = params.get("signature").and_then(|v| v.as_str()).unwrap_or("unknown");
            format!("Transaction {}: Confirmed, Fee: 0.000005 SOL, Block: 250123456", tx_sig)
        }
        _ => {
            format!("Executed {} on {} with parameters: {:?}", tool_name, server_id, params)
        }
    }
}

/// Handle theme-related commands
pub fn handle_theme_command(siv: &mut Cursive, command: &str, state: AdvancedChatState) {
    use super::super::agent::{AgentCommand, ThemeCommandType};

    if let Some(session) = state.get_active_session() {
        // Add user command to chat
        let _ = state.add_message_to_session(session.id, ChatMessage::User(command.to_string()));

        let parts: Vec<&str> = command.trim().split_whitespace().collect();
        let theme_cmd = match parts.as_slice() {
            ["/theme", "list"] => ThemeCommandType::ListThemes,
            ["/theme", "switch", theme_name] => {
                ThemeCommandType::SwitchTheme(theme_name.to_string())
            }
            ["/theme", "preview"] => ThemeCommandType::PreviewTheme(None),
            ["/theme", "preview", theme_name] => {
                ThemeCommandType::PreviewTheme(Some(theme_name.to_string()))
            }
            ["/theme", "current"] => ThemeCommandType::ShowCurrentTheme,
            ["/theme"] => ThemeCommandType::ShowCurrentTheme, // Default to showing current theme
            _ => {
                // Invalid theme command
                let help_msg = "Theme commands:\n\
                    /theme - Show current theme\n\
                    /theme list - List available themes\n\
                    /theme switch <theme_name> - Switch to a theme\n\
                    /theme preview [theme_name] - Preview a theme\n\
                    /theme current - Show current theme name";
                let _ = state
                    .add_message_to_session(session.id, ChatMessage::System(help_msg.to_string()));
                update_ui_displays(siv);
                return;
            }
        };

        // Execute theme command synchronously
        let response = execute_theme_command(theme_cmd, &state);
        let _ = state.add_message_to_session(session.id, ChatMessage::System(response));

        // Update UI to reflect changes
        update_ui_displays(siv);
    }
}

/// Execute a theme command and return the response
fn execute_theme_command(cmd: ThemeCommandType, state: &AdvancedChatState) -> String {
    match cmd {
        ThemeCommandType::ListThemes => match state.get_available_themes() {
            Ok(themes) => {
                if let Ok(current) = state.get_current_theme_name() {
                    let mut output = String::from("Available themes:\n");
                    for theme in themes {
                        let marker = if theme == current { "▶ " } else { "  " };
                        output.push_str(&format!("{}{}\n", marker, theme));
                    }
                    output
                } else {
                    format!("Available themes: {}", themes.join(", "))
                }
            }
            Err(e) => format!("Error listing themes: {}", e),
        },
        ThemeCommandType::SwitchTheme(theme_name) => match state.switch_theme(&theme_name) {
            Ok(_) => format!("✅ Switched to theme: {}", theme_name),
            Err(e) => format!("❌ Failed to switch theme: {}", e),
        },
        ThemeCommandType::PreviewTheme(theme_name) => match theme_name {
            Some(name) => match state.preview_theme(&name) {
                Ok(preview) => format!("Preview of theme '{}':\n{}", name, preview),
                Err(e) => format!("❌ Failed to preview theme '{}': {}", name, e),
            },
            None => match state.get_current_theme_name() {
                Ok(current_name) => match state.preview_theme(&current_name) {
                    Ok(preview) => format!("Current theme '{}':\n{}", current_name, preview),
                    Err(e) => format!("❌ Failed to preview current theme: {}", e),
                },
                Err(e) => format!("❌ Failed to get current theme: {}", e),
            },
        },
        ThemeCommandType::ShowCurrentTheme => match state.get_current_theme_name() {
            Ok(name) => format!("Current theme: {}", name),
            Err(e) => format!("❌ Failed to get current theme: {}", e),
        },
    }
}

/// Show test tool dialog with input fields
pub fn show_test_tool_dialog(siv: &mut Cursive, server_id: String, tool_name: String) {
    // Get tool schema to build input form
    let schema_info = siv.with_user_data(|state: &mut AdvancedChatState| {
        let tools = state.available_tools.read().unwrap();

        if let Some(tool_list) = tools.get(&server_id) {
            if let Some(tool) = tool_list.iter().find(|t| t.name == tool_name) {
                Some(tool.input_schema.clone())
            } else {
                None
            }
        } else {
            None
        }
    });

    if let Some(Some(schema)) = schema_info {
        let mut test_layout = LinearLayout::vertical();

        test_layout.add_child(TextView::new(format!("Testing: {}", tool_name)));
        test_layout.add_child(DummyView.fixed_height(1));
        test_layout.add_child(TextView::new("Enter parameters as JSON:"));
        test_layout.add_child(DummyView.fixed_height(1));

        // Add text area for JSON input
        test_layout.add_child(
            Panel::new(
                EditView::new()
                    .content("{}")
                    .with_name("test_tool_params")
                    .min_width(60)
                    .min_height(5),
            )
            .title("Parameters (JSON)"),
        );

        test_layout.add_child(DummyView.fixed_height(1));
        test_layout.add_child(TextView::new("Expected Schema:"));
        test_layout.add_child(Panel::new(
            ScrollView::new(TextView::new(
                serde_json::to_string_pretty(&schema).unwrap_or_else(|_| "{}".to_string()),
            ))
            .max_height(5),
        ));

        let server_clone = server_id.clone();
        let tool_clone = tool_name.clone();

        siv.add_layer(
            Dialog::around(test_layout)
                .title(format!("🧪 Test Tool: {}", tool_name))
                .button("Execute", move |s| {
                    execute_test_tool(s, server_clone.clone(), tool_clone.clone());
                })
                .button("Cancel", |s| {
                    s.pop_layer();
                })
                .max_width(80),
        );
    }
}

/// Execute a test tool call and show results with metrics
fn execute_test_tool(siv: &mut Cursive, server_id: String, tool_name: String) {
    // Get JSON input from the form
    let json_input = siv
        .find_name::<EditView>("test_tool_params")
        .map(|v| v.get_content().to_string())
        .unwrap_or_else(|| "{}".to_string());

    // Parse JSON
    let params = match serde_json::from_str::<serde_json::Value>(&json_input) {
        Ok(val) => val,
        Err(e) => {
            siv.add_layer(
                Dialog::text(format!("❌ Invalid JSON: {}", e))
                    .title("Parse Error")
                    .button("OK", |s| {
                        s.pop_layer();
                    }),
            );
            return;
        }
    };

    // Close the test dialog
    siv.pop_layer();

    // Show loading dialog
    siv.add_layer(
        Dialog::text("⚡ Executing tool...\nPlease wait...")
            .title("Testing Tool")
            .with_name("test_execution_dialog"),
    );

    // Execute in background and show results
    let cb_sink = siv.cb_sink().clone();
    let state_opt = siv.user_data::<AdvancedChatState>().cloned();

    std::thread::spawn(move || {
        let start_time = std::time::Instant::now();

        if let Some(state) = state_opt {
            let rt = match tokio::runtime::Runtime::new() {
                Ok(rt) => rt,
                Err(e) => {
                    let error_msg = format!("Failed to create runtime: {}", e);
                    cb_sink
                        .send(Box::new(move |s| {
                            s.pop_layer(); // Remove loading dialog
                            s.add_layer(Dialog::text(error_msg).title("❌ Test Failed").button(
                                "OK",
                                |s| {
                                    s.pop_layer();
                                },
                            ));
                        }))
                        .ok();
                    return;
                }
            };

            let result = rt.block_on(async {
                let mcp_service = state.mcp_service.lock().await;
                mcp_service
                    .call_tool(&server_id, &tool_name, Some(params))
                    .await
            });

            let execution_time_ms = start_time.elapsed().as_millis();

            cb_sink
                .send(Box::new(move |s| {
                    s.pop_layer(); // Remove loading dialog

                    match result {
                        Ok(response) => {
                            let response_str = serde_json::to_string_pretty(&response)
                                .unwrap_or_else(|_| format!("{:?}", response));

                            let mut result_layout = LinearLayout::vertical();

                            result_layout
                                .add_child(TextView::new(format!("✅ Execution Successful")));
                            result_layout.add_child(DummyView.fixed_height(1));

                            result_layout.add_child(TextView::new(format!(
                                "⏱️  Execution Time: {}ms",
                                execution_time_ms
                            )));
                            result_layout
                                .add_child(TextView::new(format!("📦 Server: {}", server_id)));
                            result_layout
                                .add_child(TextView::new(format!("🔧 Tool: {}", tool_name)));
                            result_layout.add_child(DummyView.fixed_height(1));

                            result_layout.add_child(TextView::new("Response:"));
                            result_layout.add_child(Panel::new(
                                ScrollView::new(TextView::new(response_str.clone())).max_height(15),
                            ));

                            s.add_layer(
                                Dialog::around(result_layout)
                                    .title("🧪 Test Results")
                                    .button("Close", |s| {
                                        s.pop_layer();
                                    })
                                    .button("Copy Response", move |s| {
                                        if let Ok(mut clipboard) = arboard::Clipboard::new() {
                                            let _ = clipboard.set_text(&response_str);
                                            s.add_layer(
                                                Dialog::info("Response copied to clipboard!")
                                                    .button("OK", |s| {
                                                        s.pop_layer();
                                                    }),
                                            );
                                        }
                                    })
                                    .max_width(100),
                            );
                        }
                        Err(e) => {
                            let error_msg = format!(
                                "❌ Tool execution failed:\n\n{}\n\n⏱️ Time: {}ms",
                                e, execution_time_ms
                            );
                            s.add_layer(Dialog::text(error_msg).title("Test Failed").button(
                                "OK",
                                |s| {
                                    s.pop_layer();
                                },
                            ));
                        }
                    }
                }))
                .ok();
        }
    });
}

/// Show detailed information about an MCP tool
pub fn show_tool_details(siv: &mut Cursive, server_id: String, tool_name: String) {
    // Skip if this is a header (empty tool_name)
    if tool_name.is_empty() {
        return;
    }

    let tool_info = siv.with_user_data(|state: &mut AdvancedChatState| {
        let tools = state.available_tools.read().unwrap();

        if let Some(tool_list) = tools.get(&server_id) {
            if let Some(tool) = tool_list.iter().find(|t| t.name == tool_name) {
                let description = tool
                    .description
                    .as_ref()
                    .map(|d| d.clone())
                    .unwrap_or_else(|| "No description available".to_string());

                let schema_str = serde_json::to_string_pretty(&tool.input_schema)
                    .unwrap_or_else(|_| "{}".to_string());

                Some((tool.name.clone(), description, schema_str))
            } else {
                None
            }
        } else {
            None
        }
    });

    if let Some(Some((name, description, schema))) = tool_info {
        let mut details_layout = LinearLayout::vertical();

        details_layout.add_child(TextView::new(format!("Tool: {}", name)));
        details_layout.add_child(DummyView.fixed_height(1));

        details_layout.add_child(TextView::new("Description:"));
        details_layout.add_child(Panel::new(
            ScrollView::new(TextView::new(description)).max_height(5),
        ));
        details_layout.add_child(DummyView.fixed_height(1));

        details_layout.add_child(TextView::new("Input Schema:"));
        details_layout.add_child(Panel::new(
            ScrollView::new(TextView::new(schema)).max_height(10),
        ));
        details_layout.add_child(DummyView.fixed_height(1));

        details_layout.add_child(TextView::new(format!("Server: {}", server_id)));

        let server_id_clone = server_id.clone();
        let tool_name_clone = name.clone();

        siv.add_layer(
            Dialog::around(details_layout)
                .title(format!("📦 Tool Details: {}", name))
                .button("Test Tool", move |s| {
                    show_test_tool_dialog(s, server_id_clone.clone(), tool_name_clone.clone());
                })
                .button("Close", |s| {
                    s.pop_layer();
                })
                .max_width(80)
                .max_height(30),
        );
    } else {
        siv.add_layer(
            Dialog::text(format!(
                "Tool '{}' not found on server '{}'",
                tool_name, server_id
            ))
            .title("Tool Not Found")
            .button("OK", |s| {
                s.pop_layer();
            }),
        );
    }
}
