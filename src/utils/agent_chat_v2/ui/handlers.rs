//! UI event handlers and callbacks

use cursive::{Cursive, CursiveExt};
use cursive::views::{Dialog, EditView, LinearLayout, TextView, SelectView, Panel, Button, DummyView};
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
    settings_layout.add_child(Panel::new(
        TextView::new("Loading server status...")
            .with_name("mcp_server_status")
    ).max_height(5));
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
            .button("Close", |s| { s.pop_layer(); })
    );

    // Update MCP server status after dialog is shown
    update_mcp_server_status_in_settings(siv);
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

// MCP Server Management Functions

pub fn show_add_mcp_server_dialog(siv: &mut Cursive) {
    let mut form_layout = LinearLayout::vertical();

    form_layout.add_child(TextView::new("Add New MCP Server"));
    form_layout.add_child(DummyView.fixed_height(1));

    form_layout.add_child(LinearLayout::horizontal()
        .child(TextView::new("Name: ").fixed_width(15))
        .child(EditView::new().with_name("server_name").fixed_width(30))
    );

    form_layout.add_child(LinearLayout::horizontal()
        .child(TextView::new("URL: ").fixed_width(15))
        .child(EditView::new().with_name("server_url").fixed_width(30))
    );

    form_layout.add_child(LinearLayout::horizontal()
        .child(TextView::new("Type: ").fixed_width(15))
        .child(SelectView::<String>::new()
            .item("HTTP", "http".to_string())
            .item("WebSocket", "websocket".to_string())
            .item("Stdio", "stdio".to_string())
            .with_name("server_type")
            .fixed_width(15)
        )
    );

    siv.add_layer(
        Dialog::around(form_layout)
            .title("Add MCP Server")
            .button("Add", |s| {
                add_mcp_server_from_form(s);
            })
            .button("Cancel", |s| {
                s.pop_layer();
            })
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
            .button("Close", |s| { s.pop_layer(); })
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
            .button("OK", |s| { s.pop_layer(); })
    );
}

pub fn export_all_chats(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
        let filename = format!("osvm_all_chats_export_{}.json", timestamp);

        let sessions = state.sessions.read().unwrap();
        match serde_json::to_string_pretty(&*sessions) {
            Ok(json_content) => {
                match std::fs::write(&filename, json_content) {
                    Ok(_) => {
                        info!("All chats exported to {}", filename);
                    }
                    Err(e) => {
                        error!("Failed to write export file: {}", e);
                    }
                }
            }
            Err(e) => {
                error!("Failed to serialize sessions: {}", e);
            }
        }
    });

    siv.add_layer(
        Dialog::text("All chats exported successfully!")
            .title("Export Complete")
            .button("OK", |s| { s.pop_layer(); })
    );
}

pub fn clear_all_chats(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::text("Are you sure you want to clear all chat sessions?\nThis action cannot be undone.")
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
            .button("Cancel", |s| { s.pop_layer(); })
    );
}

pub fn update_mcp_server_status_in_settings(siv: &mut Cursive) {
    let status_text = siv.with_user_data(|state: &mut AdvancedChatState| {
        let mut status_text = String::new();

        if let Ok(mcp_service) = state.mcp_service.try_lock() {
            for (server_id, config) in mcp_service.list_servers() {
                let status = if config.enabled { "✓ Enabled" } else { "✗ Disabled" };
                status_text.push_str(&format!("• {} ({:?}): {}\n", config.name, config.transport_type, status));
            }
        }

        if status_text.is_empty() {
            status_text = "No MCP servers configured".to_string();
        }

        status_text
    }).unwrap_or_else(|| "Failed to load MCP server status".to_string());

    // Update the status display outside the closure
    if let Some(mut status_view) = siv.find_name::<TextView>("mcp_server_status") {
        status_view.set_content(status_text);
    }
}

// Helper functions for MCP management

fn add_mcp_server_from_form(siv: &mut Cursive) {
    let name = siv.find_name::<EditView>("server_name")
        .map(|v| v.get_content().to_string())
        .unwrap_or_default();

    let url = siv.find_name::<EditView>("server_url")
        .map(|v| v.get_content().to_string())
        .unwrap_or_default();

    let server_type = siv.find_name::<SelectView<String>>("server_type")
        .and_then(|v| v.selection())
        .map(|s| s.as_str().to_string())
        .unwrap_or_else(|| "http".to_string());

    if name.is_empty() || url.is_empty() {
        siv.add_layer(
            Dialog::text("Please fill in all required fields.")
                .title("Error")
                .button("OK", |s| { s.pop_layer(); })
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
                Dialog::text(&format!("✅ Server '{}' added successfully!\nURL: {}\nType: {}", name, url, server_type))
                    .title("Server Added")
                    .button("OK", |s| { s.pop_layer(); })
            );
        }
        Some(Err(e)) => {
            siv.add_layer(
                Dialog::text(&format!("❌ Failed to add server: {}", e))
                    .title("Error")
                    .button("OK", |s| { s.pop_layer(); })
            );
        }
        None => {
            siv.add_layer(
                Dialog::text("❌ Failed to access application state")
                    .title("Error")
                    .button("OK", |s| { s.pop_layer(); })
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
                    let current_enabled = mcp_service.get_server(&server_id)
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
                        Dialog::text(&format!("✅ Server '{}' status toggled successfully!", server_id))
                            .title("Server Updated")
                            .button("OK", |s| {
                                s.pop_layer();
                                s.pop_layer(); // Close server list to refresh
                                show_mcp_server_list(s); // Reopen with updated list
                            })
                    );
                }
                Some(Err(e)) => {
                    siv.add_layer(
                        Dialog::text(&format!("❌ Failed to toggle server: {}", e))
                            .title("Error")
                            .button("OK", |s| { s.pop_layer(); })
                    );
                }
                None => {
                    siv.add_layer(
                        Dialog::text("❌ Failed to access application state")
                            .title("Error")
                            .button("OK", |s| { s.pop_layer(); })
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
                Dialog::text(&format!("Are you sure you want to remove server '{}'?", server_id))
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
                                        Dialog::text(&format!("✅ Server '{}' removed successfully!", server_id))
                                            .title("Server Removed")
                                            .button("OK", |s| {
                                                s.pop_layer(); // Close success dialog
                                                s.pop_layer(); // Close confirmation dialog
                                                s.pop_layer(); // Close server list
                                                show_mcp_server_list(s); // Reopen with updated list
                                            })
                                    );
                                }
                                Some(Err(e)) => {
                                    s.add_layer(
                                        Dialog::text(&format!("❌ Failed to remove server: {}", e))
                                            .title("Error")
                                            .button("OK", |s| { s.pop_layer(); })
                                    );
                                }
                                None => {
                                    s.add_layer(
                                        Dialog::text("❌ Failed to access application state")
                                            .title("Error")
                                            .button("OK", |s| { s.pop_layer(); })
                                    );
                                }
                            }
                        }
                    })
                    .button("Cancel", |s| { s.pop_layer(); })
            );
        }
    }
}