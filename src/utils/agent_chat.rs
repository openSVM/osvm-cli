//! Agent Chat UI using cursive-multiplex
//! 
//! This module provides an interactive chat interface that can use MCP servers as tools
//! to assist users with various tasks.

use cursive::{Cursive, CursiveExt};
use cursive::views::{Dialog, EditView, LinearLayout, TextView, ScrollView, Panel, Button};
use cursive::traits::*;
use cursive::direction::Orientation;
use cursive_multiplex::{Mux, Id};
use crate::services::mcp_service::{McpService, McpTool};
use anyhow::{Result, Context};
use serde_json::Value;
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;
use std::collections::HashMap;

/// Message types in the chat interface
#[derive(Clone, Debug)]
pub enum ChatMessage {
    User(String),
    Agent(String),
    System(String),
    ToolCall(String, String, Option<Value>), // tool_name, description, args
    ToolResult(String, Value), // tool_name, result
    Error(String),
}

/// Shared chat state
#[derive(Clone)]
pub struct ChatState {
    pub messages: Arc<Mutex<Vec<ChatMessage>>>,
    pub mcp_service: Arc<Mutex<McpService>>,
    pub available_tools: Arc<Mutex<HashMap<String, Vec<McpTool>>>>, // server_id -> tools
}

impl ChatState {
    pub fn new() -> Result<Self> {
        let mut mcp_service = McpService::new_with_debug(false);
        
        // Load existing MCP configurations
        if let Err(e) = mcp_service.load_config() {
            eprintln!("⚠️  Warning: Failed to load MCP config: {}", e);
        }

        Ok(ChatState {
            messages: Arc::new(Mutex::new(Vec::new())),
            mcp_service: Arc::new(Mutex::new(mcp_service)),
            available_tools: Arc::new(Mutex::new(HashMap::new())),
        })
    }

    pub fn refresh_tools_sync(&self) -> Result<()> {
        // For now, just load available servers without making async calls
        // In a full implementation, this would use a background task to refresh tools
        let mcp_service = self.mcp_service.lock().unwrap();
        let servers = mcp_service.list_servers();
        let mut available_tools = self.available_tools.lock().unwrap();
        available_tools.clear();

        for (server_id, config) in servers {
            if config.enabled {
                // For the initial implementation, we'll just mark servers as available
                // without actually fetching their tools
                available_tools.insert(server_id.clone(), vec![]);
            }
        }

        Ok(())
    }

    pub fn add_message(&self, message: ChatMessage) {
        let mut messages = self.messages.lock().unwrap();
        messages.push(message);
    }

    pub fn get_messages(&self) -> Vec<ChatMessage> {
        let messages = self.messages.lock().unwrap();
        messages.clone()
    }
}

/// Main chat UI application
pub struct AgentChatUI {
    state: ChatState,
}

impl AgentChatUI {
    pub fn new() -> Result<Self> {
        let state = ChatState::new()?;
        
        Ok(AgentChatUI { state })
    }

    pub async fn run(&mut self) -> Result<()> {
        // Refresh available tools at startup
        self.state.refresh_tools_sync()?;

        let mut siv = Cursive::default();
        
        // Set up the UI layout
        self.setup_ui(&mut siv);

        // Add initial welcome message
        self.state.add_message(ChatMessage::System("Welcome to OSVM Agent Chat! 🤖".to_string()));
        self.state.add_message(ChatMessage::System("I can help you with blockchain operations using connected MCP tools.".to_string()));
        
        // Show available tools
        {
            let tools = self.state.available_tools.lock().unwrap();
            if tools.is_empty() {
                self.state.add_message(ChatMessage::System("No MCP servers are currently configured. Use 'osvm mcp setup' to get started.".to_string()));
            } else {
                let server_names: Vec<String> = tools.keys().cloned().collect();
                self.state.add_message(ChatMessage::System(format!("Available MCP servers: {}", server_names.join(", "))));
            }
        }

        // Update the chat display
        self.update_chat_display(&mut siv);

        // Run the TUI
        siv.run();

        Ok(())
    }

    fn setup_ui(&self, siv: &mut Cursive) {
        let state = self.state.clone();

        // Create the main layout
        let mut main_layout = LinearLayout::vertical();

        // Chat display area (scrollable)
        let chat_view = ScrollView::new(
            TextView::new("")
                .with_name("chat_display")
                .full_width()
        ).scroll_strategy(cursive::view::scroll::ScrollStrategy::StickToBottom)
         .with_name("chat_scroll");

        let chat_panel = Panel::new(chat_view)
            .title("Agent Chat")
            .title_position(cursive::align::HAlign::Left);

        // Input area
        let input_layout = LinearLayout::horizontal()
            .child(TextView::new("You: "))
            .child(
                EditView::new()
                    .on_submit({
                        let state = state.clone();
                        move |siv, text| {
                            handle_user_input(siv, text, state.clone());
                        }
                    })
                    .with_name("input")
                    .full_width()
            );

        let input_panel = Panel::new(input_layout).title("Input");

        // Tool status area
        let tools_view = TextView::new("")
            .with_name("tools_display")
            .full_width();
        
        let tools_panel = Panel::new(tools_view)
            .title("Available Tools")
            .title_position(cursive::align::HAlign::Left)
            .max_height(8);

        // Control buttons
        let button_layout = LinearLayout::horizontal()
            .child(Button::new("Refresh Tools", {
                let state = state.clone();
                move |siv| {
                    refresh_tools_handler(siv, state.clone());
                }
            }))
            .child(Button::new("Clear Chat", {
                let state = state.clone();
                move |siv| {
                    clear_chat_handler(siv, state.clone());
                }
            }))
            .child(Button::new("Help", show_help))
            .child(Button::new("Quit", |siv| siv.quit()));

        // Assemble the main layout
        main_layout.add_child(chat_panel.full_height());
        main_layout.add_child(tools_panel);
        main_layout.add_child(input_panel);
        main_layout.add_child(button_layout);

        // Wrap in a dialog for the window frame
        let dialog = Dialog::around(main_layout)
            .title("OSVM Agent Chat Interface")
            .title_position(cursive::align::HAlign::Center);

        siv.add_fullscreen_layer(dialog);

        // Set focus to the input field
        siv.focus_name("input").ok();

        // Update displays
        self.update_tools_display(siv);
    }

    fn update_chat_display(&self, siv: &mut Cursive) {
        let messages = self.get_messages();
        let mut display_text = String::new();

        for message in messages {
            match message {
                ChatMessage::User(text) => {
                    display_text.push_str(&format!("👤 You: {}\n\n", text));
                }
                ChatMessage::Agent(text) => {
                    display_text.push_str(&format!("🤖 Agent: {}\n\n", text));
                }
                ChatMessage::System(text) => {
                    display_text.push_str(&format!("ℹ️  System: {}\n\n", text));
                }
                ChatMessage::ToolCall(tool_name, description, args) => {
                    display_text.push_str(&format!("🔧 Calling tool: {} - {}\n", tool_name, description));
                    if let Some(args) = args {
                        display_text.push_str(&format!("   Args: {}\n\n", args));
                    } else {
                        display_text.push_str("\n");
                    }
                }
                ChatMessage::ToolResult(tool_name, result) => {
                    display_text.push_str(&format!("✅ Tool {} result:\n{}\n\n", tool_name, 
                        serde_json::to_string_pretty(&result).unwrap_or_else(|_| format!("{}", result))));
                }
                ChatMessage::Error(text) => {
                    display_text.push_str(&format!("❌ Error: {}\n\n", text));
                }
            }
        }

        if let Some(mut chat_display) = siv.find_name::<TextView>("chat_display") {
            chat_display.set_content(display_text);
        }
    }

    fn update_tools_display(&self, siv: &mut Cursive) {
        let tools = self.state.available_tools.lock().unwrap();
        let mut tools_text = String::new();

        if tools.is_empty() {
            tools_text.push_str("No MCP servers configured. Use 'osvm mcp setup' to add tools.");
        } else {
            for (server_id, server_tools) in tools.iter() {
                tools_text.push_str(&format!("🔌 {}: ", server_id));
                let tool_names: Vec<String> = server_tools.iter().map(|t| t.name.clone()).collect();
                tools_text.push_str(&tool_names.join(", "));
                tools_text.push('\n');
            }
        }

        if let Some(mut tools_display) = siv.find_name::<TextView>("tools_display") {
            tools_display.set_content(tools_text);
        }
    }

    fn get_messages(&self) -> Vec<ChatMessage> {
        self.state.get_messages()
    }
}

/// Handle user input from the chat interface
fn handle_user_input(siv: &mut Cursive, text: &str, state: ChatState) {
    if text.trim().is_empty() {
        return;
    }

    // Add user message
    state.add_message(ChatMessage::User(text.to_string()));

    // Clear input
    if let Some(mut input) = siv.find_name::<EditView>("input") {
        input.set_content("");
    }

    // Update chat display
    update_chat_display_handler(siv, state.clone());

    // Process the user's message (this would be where we integrate AI/MCP logic)
    process_user_message(siv, text.to_string(), state);
}

/// Process user message and determine appropriate response/tool calls
fn process_user_message(siv: &mut Cursive, message: String, state: ChatState) {
    // This is a simplified version - in a full implementation, this would integrate
    // with an AI service to understand the user's intent and call appropriate tools
    
    // For now, we'll implement some basic commands and tool detection
    let message_lower = message.to_lowercase();
    
    if message_lower.contains("help") {
        state.add_message(ChatMessage::Agent("I can help you with blockchain operations using MCP tools. Try asking about account balances, transactions, or network status.".to_string()));
    } else if message_lower.contains("tools") || message_lower.contains("what can you do") {
        let tools = state.available_tools.lock().unwrap();
        if tools.is_empty() {
            state.add_message(ChatMessage::Agent("I don't have any MCP tools configured. Please configure some MCP servers first.".to_string()));
        } else {
            let mut response = "I have access to these tools:\n\n".to_string();
            for (server_id, server_tools) in tools.iter() {
                response.push_str(&format!("From {}:\n", server_id));
                for tool in server_tools {
                    response.push_str(&format!("  • {}: {}\n", tool.name, 
                        tool.description.as_deref().unwrap_or("No description")));
                }
                response.push('\n');
            }
            state.add_message(ChatMessage::Agent(response));
        }
    } else {
        // Default response for now
        state.add_message(ChatMessage::Agent("I understand you're asking about blockchain operations. In a full implementation, I would analyze your request and call appropriate MCP tools to help you.".to_string()));
    }

    // Update display
    update_chat_display_handler(siv, state);
}

/// Update chat display helper
fn update_chat_display_handler(siv: &mut Cursive, state: ChatState) {
    let messages = state.get_messages();
    let mut display_text = String::new();

    for message in messages {
        match message {
            ChatMessage::User(text) => {
                display_text.push_str(&format!("👤 You: {}\n\n", text));
            }
            ChatMessage::Agent(text) => {
                display_text.push_str(&format!("🤖 Agent: {}\n\n", text));
            }
            ChatMessage::System(text) => {
                display_text.push_str(&format!("ℹ️  System: {}\n\n", text));
            }
            ChatMessage::ToolCall(tool_name, description, args) => {
                display_text.push_str(&format!("🔧 Calling tool: {} - {}\n", tool_name, description));
                if let Some(args) = args {
                    display_text.push_str(&format!("   Args: {}\n\n", args));
                } else {
                    display_text.push_str("\n");
                }
            }
            ChatMessage::ToolResult(tool_name, result) => {
                display_text.push_str(&format!("✅ Tool {} result:\n{}\n\n", tool_name, 
                    serde_json::to_string_pretty(&result).unwrap_or_else(|_| format!("{}", result))));
            }
            ChatMessage::Error(text) => {
                display_text.push_str(&format!("❌ Error: {}\n\n", text));
            }
        }
    }

    if let Some(mut chat_display) = siv.find_name::<TextView>("chat_display") {
        chat_display.set_content(display_text);
    }
}

/// Refresh tools handler
fn refresh_tools_handler(siv: &mut Cursive, state: ChatState) {
    // In a real implementation, this would be async
    state.add_message(ChatMessage::System("Refreshing available tools...".to_string()));
    update_chat_display_handler(siv, state.clone());
    
    // For now, just update the tools display
    update_tools_display_handler(siv, state);
}

/// Clear chat handler
fn clear_chat_handler(siv: &mut Cursive, state: ChatState) {
    {
        let mut messages = state.messages.lock().unwrap();
        messages.clear();
    }
    
    // Add welcome message back
    state.add_message(ChatMessage::System("Chat cleared. Welcome back! 🤖".to_string()));
    update_chat_display_handler(siv, state);
}

/// Update tools display helper
fn update_tools_display_handler(siv: &mut Cursive, state: ChatState) {
    let tools = state.available_tools.lock().unwrap();
    let mut tools_text = String::new();

    if tools.is_empty() {
        tools_text.push_str("No MCP servers configured. Use 'osvm mcp setup' to add tools.");
    } else {
        for (server_id, server_tools) in tools.iter() {
            tools_text.push_str(&format!("🔌 {}: ", server_id));
            if server_tools.is_empty() {
                tools_text.push_str("Available (tools not fetched)");
            } else {
                let tool_names: Vec<String> = server_tools.iter().map(|t| t.name.clone()).collect();
                tools_text.push_str(&tool_names.join(", "));
            }
            tools_text.push('\n');
        }
    }

    if let Some(mut tools_display) = siv.find_name::<TextView>("tools_display") {
        tools_display.set_content(tools_text);
    }
}

/// Show help dialog
fn show_help(siv: &mut Cursive) {
    let help_text = "OSVM Agent Chat Help\n\n\
        • Type your questions or requests in the input box\n\
        • Press Enter to send messages\n\
        • Ask about blockchain operations, accounts, transactions\n\
        • Say 'tools' to see available MCP tools\n\
        • Use 'Refresh Tools' to reload available tools\n\
        • Use 'Clear Chat' to start fresh\n\
        • Press 'Quit' or Ctrl+C to exit\n\n\
        The agent will use MCP tools to help with your requests.";

    siv.add_layer(
        Dialog::text(help_text)
            .title("Help")
            .button("OK", |s| {
                s.pop_layer();
            })
    );
}

/// Main entry point for the agent chat UI
pub async fn run_agent_chat() -> Result<()> {
    println!("🚀 Starting OSVM Agent Chat Interface...");
    
    let mut chat_ui = AgentChatUI::new()
        .context("Failed to initialize chat UI")?;
    
    chat_ui.run().await
        .context("Failed to run chat interface")?;

    Ok(())
}