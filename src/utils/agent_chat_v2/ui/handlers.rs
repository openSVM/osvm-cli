//! UI event handlers and callbacks

use cursive::{Cursive, CursiveExt};
use cursive::views::{Dialog, EditView, LinearLayout, TextView, SelectView};
use cursive::traits::*;
use log::{error, info};
use uuid::Uuid;

use super::super::state::AdvancedChatState;
use super::super::types::{ChatMessage, AgentState};
use super::super::agent::AgentCommand;
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
                                    .title("Error")
                            );
                        } else {
                            siv.add_layer(
                                Dialog::info("Message copied to clipboard!")
                                    .title("Success")
                                    .button("OK", |s| { s.pop_layer(); })
                            );
                        }
                    }
                    Err(e) => {
                        siv.add_layer(
                            Dialog::info(format!("Clipboard not available: {}\nText:\n{}", e, text_to_copy))
                                .title("Clipboard Error")
                                .button("OK", |s| { s.pop_layer(); })
                        );
                    }
                }
            }
        }
    }
}

pub fn delete_last_message(siv: &mut Cursive, state: AdvancedChatState) {
    if let Some(session) = state.get_active_session() {
        let session_id = session.id;
        if let Ok(mut sessions) = state.sessions.write() {
            if let Some(session) = sessions.get_mut(&session_id) {
                if !session.messages.is_empty() {
                    session.messages.pop();
                    update_ui_displays(siv);
                }
            }
        }
    }
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

pub fn insert_suggestion_at_cursor(siv: &mut Cursive, index: usize, state: AdvancedChatState) {
    // Get the suggestion
    let suggestion = {
        let suggestions = state.current_suggestions.read()
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
    if text.trim().is_empty() {
        return;
    }

    let user_message = text.to_string();

    // Hide suggestions when sending a message
    if let Ok(mut vis) = state.suggestions_visible.write() {
        *vis = false;
    }

    // Clear input
    if let Some(mut input) = siv.find_name::<EditView>("input") {
        input.set_content("");
    }

    // Immediately add user message to session and update display
    if let Some(session) = state.get_active_session() {
        // Add user message to the session
        let _ = state.add_message_to_session(session.id, ChatMessage::User(user_message.clone()));

        // Add processing indicator with spinner
        let _ = state.add_message_to_session(session.id, ChatMessage::Processing {
            message: "Processing your request...".to_string(),
            spinner_index: 0
        });

        // Update the display immediately
        update_ui_displays(siv);

        // Send to agent for processing
        let command = AgentCommand::ProcessInput {
            session_id: session.id,
            input: user_message,
        };

        // Send command using sync method to avoid runtime conflicts
        state.send_agent_command_sync(command);
    }
}

pub fn create_new_chat_dialog(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::around(
            EditView::new()
                .content("New Chat")
                .with_name("new_chat_name")
                .fixed_width(30)
        )
        .title("Create New Chat")
        .button("Create", |s| {
            let name = s.find_name::<EditView>("new_chat_name")
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
        })
    );
}

pub fn resume_agent(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let command = AgentCommand::ResumeAgent { session_id: session.id };
            // Send command using sync method to avoid runtime conflicts
            state.send_agent_command_sync(command);
        }
    });
}

pub fn pause_agent(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let command = AgentCommand::PauseAgent { session_id: session.id };
            // Send command using sync method to avoid runtime conflicts
            state.send_agent_command_sync(command);
        }
    });
}

pub fn stop_agent(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let command = AgentCommand::StopAgent { session_id: session.id };
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
                    let filename = format!("osvm_chat_{}_{}.log",
                        session.name.replace(' ', "_"),
                        chrono::Utc::now().format("%Y%m%d_%H%M%S"));

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
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session_id) = state.active_session_id.read().ok().and_then(|id| *id) {
            if let Ok(mut sessions) = state.sessions.write() {
                if let Some(session) = sessions.get_mut(&session_id) {
                    session.messages.clear();
                    session.add_message(ChatMessage::System("Chat cleared".to_string()));
                }
            }
        }
    });

    update_ui_displays(siv);
}

pub fn show_settings(siv: &mut Cursive) {
    let settings_dialog = Dialog::around(
        LinearLayout::vertical()
            .child(TextView::new("OSVM Agent Settings"))
            .child(cursive::views::DummyView.fixed_height(1))
            .child(TextView::new("MCP Servers:"))
            .child(cursive::views::Panel::new(
                TextView::new("• local_sim (enabled)\n• blockchain_tools (disabled)")
            ))
            .child(cursive::views::DummyView.fixed_height(1))
            .child(TextView::new("AI Model: GPT-4"))
            .child(TextView::new("Debug Mode: Disabled"))
            .child(TextView::new("Max History: 1000 messages"))
            .child(cursive::views::DummyView.fixed_height(1))
            .child(TextView::new("Recording Directory: ./recordings/"))
    )
    .title("Settings")
    .button("Configure MCP", |s| {
        s.add_layer(
            Dialog::text("MCP configuration will open in a separate interface.\nUse 'osvm mcp config' command.")
                .title("MCP Configuration")
                .button("OK", |s| { s.pop_layer(); })
        );
    })
    .button("Close", |s| {
        s.pop_layer();
    });

    siv.add_layer(settings_dialog);
}

pub fn export_chat(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let filename = format!("osvm_chat_export_{}_{}.json",
                session.name.replace(' ', "_"),
                chrono::Utc::now().format("%Y%m%d_%H%M%S"));

            match serde_json::to_string_pretty(&session) {
                Ok(json_content) => {
                    match std::fs::write(&filename, json_content) {
                        Ok(_) => {
                            info!("Chat exported to {}", filename);
                            // Add success message to current session
                            if let Ok(mut sessions) = state.sessions.write() {
                                if let Some(current_session) = sessions.get_mut(&session.id) {
                                    current_session.add_message(ChatMessage::System(
                                        format!("Chat exported to {}", filename)
                                    ));
                                }
                            }
                        }
                        Err(e) => {
                            error!("Failed to write export file: {}", e);
                        }
                    }
                }
                Err(e) => {
                    error!("Failed to serialize session: {}", e);
                }
            }
        }
    });

    update_ui_displays(siv);
}

pub fn show_advanced_help(siv: &mut Cursive) {
    let help_text = "OSVM Advanced Agent Chat Help\n\n\
        Agent Controls:\n\
        - Run: Resume agent processing\n\
        - Pause: Pause agent operations\n\
        - Stop: Stop current agent task\n\n\
        Recording:\n\
        - Record: Start session recording\n\
        - Stop Rec: Stop recording\n\n\
        Status Indicators:\n\
        - Idle - Thinking - Planning\n\
        - Executing - Waiting - Paused - Error\n\n\
        Features:\n\
        • AI-powered tool planning and execution\n\
        • Background agent processing\n\
        • Multi-chat session support\n\
        • Session recording and export\n\
        • MCP server integration for blockchain tools";

    siv.add_layer(
        Dialog::text(help_text)
            .title("Advanced Help")
            .button("OK", |s| {
                s.pop_layer();
            })
    );
}