//! UI event handlers and callbacks

use cursive::traits::*;
use cursive::views::{
    Button, Dialog, DummyView, EditView, LinearLayout, Panel, SelectView, TextView,
};
use cursive::{Cursive, CursiveExt};
use log::{error, info};
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
                }
            }
        }

        // Update UI after all changes
        update_ui_displays(siv);
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

        // Add processing indicator with animated spinner
        let _ = state.add_message_to_session(
            session.id,
            ChatMessage::Processing {
                message: "🤖 Processing your request...".to_string(),
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
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let filename = format!(
                "osvm_chat_export_{}_{}.json",
                session.name.replace(' ', "_"),
                chrono::Utc::now().format("%Y%m%d_%H%M%S")
            );

            match serde_json::to_string_pretty(&session) {
                Ok(json_content) => {
                    match std::fs::write(&filename, json_content) {
                        Ok(_) => {
                            info!("Chat exported to {}", filename);
                            // Add success message to current session
                            if let Ok(mut sessions) = state.sessions.write() {
                                if let Some(current_session) = sessions.get_mut(&session.id) {
                                    current_session.add_message(ChatMessage::System(format!(
                                        "Chat exported to {}",
                                        filename
                                    )));
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
        🔤 Keyboard Shortcuts:\n\
        • Tab - Switch between chat list and input\n\
        • Shift+Tab - Reverse navigation\n\
        • F10 - Context menu (Copy, Retry, Clear)\n\
        • Ctrl+1-5 - Insert suggestions (when visible)\n\
        • Alt+1-5 - Alternative suggestion insertion\n\
        • Alt+R - Retry last message\n\
        • Alt+C - Copy last message\n\
        • Alt+D - Delete last message\n\
        • Alt+F - Fork conversation\n\
        • Alt+X - Emergency clear processing\n\
        • Alt+M - Switch to standard mode\n\
        • Esc - Hide suggestions\n\n\
        📋 Agent Controls:\n\
        • Run - Resume agent processing\n\
        • Pause - Pause agent operations\n\
        • Stop - Stop current agent task\n\n\
        📹 Recording:\n\
        • Record - Start session recording\n\
        • Stop Rec - Stop recording\n\n\
        🔄 Status Icons:\n\
        • ● Idle  ◐ Thinking  ◑ Planning\n\
        • ◒ Executing  ◯ Waiting  ⏸ Paused  ⚠ Error\n\n\
        ✨ Features:\n\
        • AI-powered tool planning and execution\n\
        • Background agent processing\n\
        • Multi-chat session support\n\
        • Session recording and export\n\
        • MCP server integration for blockchain tools";

    siv.add_layer(
        Dialog::text(help_text)
            .title("🚀 Advanced Help")
            .button("Got it!", |s| {
                s.pop_layer();
            })
            .button("Print Shortcuts", |s| {
                // Print shortcuts to console for reference
                println!("\n=== OSVM Advanced Chat Keyboard Shortcuts ===");
                println!("Tab/Shift+Tab: Navigate UI  |  F10: Context Menu");
                println!("Ctrl/Alt+1-5: Suggestions  |  Alt+R/C/D/F: Actions");
                println!("Alt+X: Emergency Clear  |  Esc: Hide Suggestions");
                println!("================================================\n");
            }),
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
    let processing_active = Arc::new(std::sync::atomic::AtomicBool::new(true));

    // Get a callback sink to update UI from background thread
    let cb_sink = siv.cb_sink().clone();
    let cb_sink2 = cb_sink.clone();

    // Clone for background thread
    let state_clone = state.clone();
    let spinner_clone = spinner_counter.clone();
    let active_clone = processing_active.clone();

    // Start spinner animation in background thread
    thread::spawn(move || {
        let stages = vec![
            "🤖 Analyzing your request...",
            "🔍 Searching for relevant information...",
            "⚙️ Processing with AI...",
            "📊 Generating response...",
            "✨ Finalizing answer...",
        ];

        let mut stage_index = 0;
        let mut update_counter = 0;

        while active_clone.load(Ordering::Relaxed) {
            let spinner_index = spinner_clone.fetch_add(1, Ordering::Relaxed);

            // Change message every 30 updates (roughly 3 seconds)
            if update_counter % 30 == 0 {
                stage_index = (stage_index + 1) % stages.len();
            }
            update_counter += 1;

            let message = stages[stage_index].to_string();

            // Update processing message in session
            let _ = state_clone.update_processing_message(
                session_id,
                message.clone(),
                spinner_index % 10,
            );

            // Request UI update
            cb_sink
                .send(Box::new(move |siv| {
                    update_ui_displays(siv);
                }))
                .ok();

            thread::sleep(Duration::from_millis(100));
        }
    });

    // Process with real AI service
    let state_final = state.clone();
    let processing_final = processing_active.clone();
    let ai_service = state_final.ai_service.clone();

    thread::spawn(move || {
        // Use tokio runtime for async AI call within thread
        let rt = match tokio::runtime::Runtime::new() {
            Ok(rt) => rt,
            Err(e) => {
                eprintln!("Failed to create tokio runtime: {}", e);
                processing_final.store(false, Ordering::Relaxed);
                let _ = state_final.remove_last_processing_message(session_id);
                let _ = state_final.add_message_to_session(
                    session_id,
                    ChatMessage::Error("Failed to initialize AI processing".to_string()),
                );
                cb_sink2
                    .send(Box::new(move |siv| {
                        update_ui_displays(siv);
                    }))
                    .ok();
                return;
            }
        };

        // Get available tools context
        let tools_context = state_final.get_available_tools_context();

        // Create STRICT structured planning prompt
        let planning_query = format!(
            "You are OSVM Agent. You MUST respond in the exact XML format specified.

\
            USER REQUEST: '{}'

\
            AVAILABLE TOOLS:
{}

\
            CRITICAL: You MUST respond ONLY in this format. Do not add any text before or after:

\
            <plan>
\
            <tool name=\"exact_tool_name\" server=\"exact_server_id\">
\
              <param name=\"param_name\">param_value</param>
\
            </tool>
\
            </plan>
\
            <response>
\
            Brief explanation of what you're doing.
\
            </response>

\
            EXACT MAPPINGS - Use these tools for these requests:

\
            \"balance\" or \"how much SOL\" → get_balance tool
\
            \"transactions\" or \"recent transactions\" → get_recent_transactions tool
\
            \"transaction details\" + signature → get_transaction tool
\
            \"analyze transaction\" → analyze_transaction tool

\
            EXAMPLES (COPY EXACTLY):

\
            Request: \"What's my SOL balance?\"
\
            <plan>
\
            <tool name=\"get_balance\" server=\"osvm-api\">
\
              <param name=\"address\">user_default</param>
\
            </tool>
\
            </plan>
\
            <response>
\
            I'll check your SOL balance now.
\
            </response>

\
            Request: \"Show my recent transactions\"
\
            <plan>
\
            <tool name=\"get_recent_transactions\" server=\"osvm-api\">
\
              <param name=\"limit\">10</param>
\
            </tool>
\
            </plan>
\
            <response>
\
            I'll get your 10 most recent transactions.
\
            </response>

\
            Request: \"Get details for transaction ABC123\"
\
            <plan>
\
            <tool name=\"get_transaction\" server=\"osvm-api\">
\
              <param name=\"signature\">ABC123</param>
\
            </tool>
\
            </plan>
\
            <response>
\
            I'll get the details for transaction ABC123.
\
            </response>

\
            For general questions (no blockchain data needed), use:
\
            <plan></plan>
\
            <response>Your answer here</response>

\
            NOW RESPOND TO: '{}' - USE EXACT FORMAT ABOVE",
            user_input, tools_context, user_input
        );

        let response = rt.block_on(async {
            match ai_service.query_with_debug(&planning_query, false).await {
                Ok(ai_response) => {
                    // Parse the structured response and execute tools
                    execute_ai_plan(ai_response, session_id, state_final.clone()).await
                }
                Err(e) => {
                    eprintln!("AI service error: {}", e);
                    // Fallback to mock response on AI service failure
                    format!(
                        "I encountered an issue connecting to the AI service: {}

\
                        However, I can still help with basic Solana operations. {}",
                        e,
                        generate_mock_response(&user_input)
                    )
                }
            }
        });

        // Stop spinner
        processing_final.store(false, Ordering::Relaxed);

        // Remove processing message and add agent response
        let _ = state_final.remove_last_processing_message(session_id);
        let _ = state_final.add_message_to_session(session_id, ChatMessage::Agent(response));

        // Generate intelligent follow-up suggestions using AI
        let suggestions = rt.block_on(async {
            let suggestion_query = format!(
                "Based on the user query '{}' and your response, suggest 5 short follow-up questions \
                or actions related to Solana/OSVM operations. Return only the suggestions, one per line, \
                without numbers or bullets.",
                user_input
            );

            match ai_service.query_with_debug(&suggestion_query, false).await {
                Ok(ai_suggestions) => {
                    // Parse AI suggestions into vec
                    ai_suggestions
                        .lines()
                        .map(|s| s.trim().to_string())
                        .filter(|s| !s.is_empty() && s.len() < 80) // Reasonable length limit
                        .take(5)
                        .collect::<Vec<String>>()
                }
                Err(_) => {
                    // Fallback to context suggestions on AI failure
                    generate_context_suggestions(&user_input)
                }
            }
        });

        if !suggestions.is_empty() {
            if let Ok(mut current_suggestions) = state_final.current_suggestions.write() {
                *current_suggestions = suggestions;
            }
            if let Ok(mut vis) = state_final.suggestions_visible.write() {
                *vis = true;
            }
        }

        // Final UI update
        cb_sink2
            .send(Box::new(move |siv| {
                update_ui_displays(siv);
            }))
            .ok();
    });
}

// Generate mock response for demo
fn generate_mock_response(input: &str) -> String {
    let input_lower = input.to_lowercase();

    if input_lower.contains("balance") {
        "I'll check your SOL balance for you. According to the latest data, your wallet shows 2.45 SOL with some pending transactions.".to_string()
    } else if input_lower.contains("transaction") {
        "Here are your recent transactions:\n\n1. → Received 1.5 SOL (2 hours ago)\n2. ← Sent 0.3 SOL (1 day ago)\n3. → Staking rewards 0.025 SOL (2 days ago)\n\nWould you like more details on any of these?".to_string()
    } else if input_lower.contains("price") {
        "The current SOL price is approximately $23.45 USD, showing a 2.3% increase over the last 24 hours. The market looks relatively stable.".to_string()
    } else if input_lower.contains("stake") || input_lower.contains("staking") {
        "For staking your SOL:\n\n1. Choose a validator with good performance\n2. Use 'solana delegate-stake' command\n3. Minimum stake amount is 0.001 SOL\n\nCurrent APY is around 6.8%. Would you like help selecting a validator?".to_string()
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
    std::thread::spawn(move || {
        let rt = match tokio::runtime::Runtime::new() {
            Ok(rt) => rt,
            Err(_) => return, // Silently fail on runtime creation error
        };

        let _ai_suggestions = rt.block_on(async {
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

        // TODO: Update suggestions in state (requires more complex async handling)
        // For now, we rely on the fast pattern matching below
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
