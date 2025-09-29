//! Display update logic and UI refresh

use cursive::{Cursive, CursiveExt};
use cursive::views::{TextView, SelectView, LinearLayout, ListView, Button};
use cursive::traits::*;
use cursive::theme::{Color, BaseColor, ColorStyle};
use cursive::utils::markup::StyledString;
use uuid::Uuid;

use super::super::state::AdvancedChatState;
use super::super::types::{ChatMessage, AgentState};
use super::super::utils::{sanitize_text_for_ui, sanitize_json_for_ui};
use super::super::utils::markdown::render_markdown;
use super::layout::AdvancedChatUI;
use super::handlers::handle_chat_selection;

impl AdvancedChatUI {
    pub fn update_all_displays(&self, siv: &mut Cursive) {
        self.update_chat_list(siv);
        self.update_chat_display(siv);
        self.update_agent_status(siv);
        self.update_mcp_tools_list(siv);
    }

    pub fn update_chat_list(&self, siv: &mut Cursive) {
        let session_names = self.state.get_session_names();
        let active_id = self.state.active_session_id.read().ok().and_then(|id| *id);

        if let Some(mut chat_list) = siv.find_name::<ListView>("chat_list") {
            // Smart in-place updates instead of clear() + rebuild
            // Only rebuild if session count changed, otherwise just update existing buttons
            let current_count = chat_list.len();
            let new_count = session_names.len();

            if current_count != new_count {
                // Session count changed - need full rebuild
                chat_list.clear();

                for (id, name, agent_state) in session_names.iter() {
                    let status_icon = match agent_state {
                        AgentState::Idle => "●",
                        AgentState::Thinking => "◐",
                        AgentState::Planning => "◑",
                        AgentState::ExecutingTool(_) => "◒",
                        AgentState::Waiting => "◯",
                        AgentState::Paused => "⏸",
                        AgentState::Error(_) => "⚠",
                    };

                    let display_name = format!("{} {}", status_icon, name);
                    let is_active = Some(*id) == active_id;
                    let button_text = if is_active {
                        format!("► {}", display_name)
                    } else {
                        format!("  {}", display_name)
                    };

                    let session_id = *id;
                    let mut button = Button::new(button_text.clone(), move |siv| {
                        handle_chat_selection(siv, session_id);
                    });

                    chat_list.add_child(&display_name, button);
                }
            } else {
                // Same session count - for now just do a full rebuild
                // TODO: Implement true in-place updates when cursive API supports it
                chat_list.clear();

                for (id, name, agent_state) in session_names.iter() {
                    let status_icon = match agent_state {
                        AgentState::Idle => "●",
                        AgentState::Thinking => "◐",
                        AgentState::Planning => "◑",
                        AgentState::ExecutingTool(_) => "◒",
                        AgentState::Waiting => "◯",
                        AgentState::Paused => "⏸",
                        AgentState::Error(_) => "⚠",
                    };

                    let display_name = format!("{} {}", status_icon, name);
                    let is_active = Some(*id) == active_id;
                    let button_text = if is_active {
                        format!("► {}", display_name)
                    } else {
                        format!("  {}", display_name)
                    };

                    let session_id = *id;
                    let mut button = Button::new(button_text.clone(), move |siv| {
                        handle_chat_selection(siv, session_id);
                    });

                    chat_list.add_child(&display_name, button);
                }
            }
        }
    }

    pub fn update_mcp_tools_list(&self, siv: &mut Cursive) {
        let tools = self.state.available_tools.read().unwrap();
        let mut content = String::new();
        if tools.is_empty() {
            content.push_str("No tools available.");
        } else {
            for (server_id, tool_list) in tools.iter() {
                content.push_str(&format!("- {} ({} tools)\n", server_id, tool_list.len()));
            }
        }

        if let Some(mut mcp_tools_view) = siv.find_name::<TextView>("mcp_tools_list") {
            mcp_tools_view.set_content(content);
        }
    }

    pub fn update_chat_display(&self, siv: &mut Cursive) {
        if let Some(session) = self.state.get_active_session() {
            let mut display_text = String::new();

            for message in &session.messages {
                match message {
                    ChatMessage::User(text) => {
                        let sanitized = self.sanitize_text(text);
                        display_text.push_str(&format!("You: {}\n", sanitized));
                        display_text.push_str("   [R]etry [C]opy [D]elete   (Alt+R/C/D)\n\n");
                    }
                    ChatMessage::Agent(text) => {
                        let rendered = self.render_markdown(text);
                        let sanitized = self.sanitize_text(&rendered);
                        display_text.push_str(&format!("Agent:\n{}", sanitized));
                        display_text.push_str("\n   [F]ork [C]opy [R]etry [D]elete   (Alt+F/C/R/D)\n\n");
                    }
                    ChatMessage::System(text) => {
                        let sanitized = self.sanitize_text(text);
                        display_text.push_str(&format!("System: {}\n\n", sanitized));
                    }
                    ChatMessage::ToolCall { tool_name, description, args, execution_id } => {
                        let sanitized_tool_name = self.sanitize_text(tool_name);
                        let sanitized_description = self.sanitize_text(description);
                        display_text.push_str(&format!("Calling tool: {} - {}\n", sanitized_tool_name, sanitized_description));
                        if let Some(args) = args {
                            let sanitized_args = self.sanitize_json(args);
                            display_text.push_str(&format!("   Args: {}\n", sanitized_args));
                        }
                        display_text.push_str(&format!("   ID: {}\n\n", execution_id));
                    }
                    ChatMessage::ToolResult { tool_name, result, execution_id } => {
                        let sanitized_tool_name = self.sanitize_text(tool_name);
                        display_text.push_str(&format!("Tool {} result (ID: {}):\n", sanitized_tool_name, execution_id));
                        let sanitized_result = self.sanitize_json(result);
                        display_text.push_str(&format!("{}\n\n", sanitized_result));
                    }
                    ChatMessage::Error(text) => {
                        let sanitized = self.sanitize_text(text);
                        display_text.push_str(&format!("Error: {}\n\n", sanitized));
                    }
                    ChatMessage::AgentThinking(text) => {
                        let sanitized = self.sanitize_text(text);
                        display_text.push_str(&format!("Agent (thinking): {}\n\n", sanitized));
                    }
                    ChatMessage::AgentPlan(plan) => {
                        display_text.push_str("Agent Plan:\n");
                        for (i, step) in plan.iter().enumerate() {
                            let step_text = format!("{}: {}", step.tool_name, step.reason);
                            let sanitized = self.sanitize_text(&step_text);
                            display_text.push_str(&format!("  {}. {}\n", i + 1, sanitized));
                        }
                        display_text.push_str("\n");
                    }
                    ChatMessage::Processing { message, spinner_index } => {
                        let spinner_chars = vec!["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
                        let spinner_char = spinner_chars[spinner_index % spinner_chars.len()];
                        let sanitized = self.sanitize_text(message);
                        display_text.push_str(&format!("{} {}\n\n", spinner_char, sanitized));
                    }
                }
            }

            // Stable content update - only set if content actually changed
            if let Some(mut chat_display) = siv.find_name::<TextView>("chat_display") {
                let content_ref = chat_display.get_content();
                let current_content = content_ref.source().to_string();
                if current_content != display_text {
                    chat_display.set_content(display_text);
                }
            }
        } else {
            let no_session_text = "No active session. Use Tab to navigate to the session list and select a session.";
            if let Some(mut chat_display) = siv.find_name::<TextView>("chat_display") {
                let content_ref = chat_display.get_content();
                let current_content = content_ref.source().to_string();
                if current_content != no_session_text {
                    chat_display.set_content(no_session_text);
                }
            }
        }
    }

    pub fn update_agent_status(&self, siv: &mut Cursive) {
        use std::sync::atomic::Ordering;
        use chrono::{DateTime, Utc};

        let spinner_index = self.state.spinner_state.load(Ordering::Relaxed);
        let spinner_chars = vec!["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
        let spinner_char = spinner_chars[spinner_index % spinner_chars.len()];

        let timestamp = Utc::now().format("%H:%M:%S");

        let status_text = if let Some(session) = self.state.get_active_session() {
            match session.agent_state {
                AgentState::Idle => format!("🤖 Agent: Idle | Ready for tasks | {}", timestamp),
                AgentState::Thinking => format!("{} Agent: Thinking... | Analyzing request | {}", spinner_char, timestamp),
                AgentState::Planning => format!("{} Agent: Planning... | Creating execution plan | {}", spinner_char, timestamp),
                AgentState::ExecutingTool(ref tool_name) => format!("{} Agent: Executing {} | Processing... | {}", spinner_char, tool_name, timestamp),
                AgentState::Waiting => format!("⏳ Agent: Waiting | Awaiting response | {}", timestamp),
                AgentState::Paused => format!("⏸️ Agent: Paused | Operations suspended | {}", timestamp),
                AgentState::Error(ref err) => format!("⚠️ Agent: Error | {} | {}", err, timestamp),
            }
        } else {
            format!("❓ Agent: No active session | Select a chat session | {}", timestamp)
        };

        if let Some(mut status_display) = siv.find_name::<TextView>("agent_status") {
            status_display.set_content(status_text);
        }
    }

    pub fn update_suggestions_display(&self, siv: &mut Cursive) {
        let suggestions_visible = self.state.suggestions_visible.read()
            .map(|v| *v)
            .unwrap_or(false);

        if let Some(mut container) = siv.find_name::<LinearLayout>("suggestions_container") {
            container.clear();

            if suggestions_visible {
                let suggestions = self.state.current_suggestions.read()
                    .map(|s| s.clone())
                    .unwrap_or_default();

                if !suggestions.is_empty() {
                    for (i, sugg) in suggestions.iter().enumerate() {
                        let mut styled = StyledString::new();

                        // Create blue background with white text style
                        let blue_style = ColorStyle::new(
                            Color::Light(BaseColor::White),
                            Color::Dark(BaseColor::Blue)
                        );

                        // Format: "1. Suggestion text" with full blue background
                        let display_text = format!(" {}. {} ", i + 1, sugg);
                        styled.append_styled(display_text, blue_style);

                        // Add as a styled TextView
                        let suggestion_view = TextView::new(styled);
                        container.add_child(suggestion_view);
                    }

                    // Add a small spacer after suggestions
                    container.add_child(cursive::views::DummyView.fixed_height(1));
                }
            }
        }
    }

    // Private helper methods that need to be accessible from display updates
    fn sanitize_text(&self, text: &str) -> String {
        sanitize_text_for_ui(text)
    }

    fn sanitize_json(&self, value: &serde_json::Value) -> String {
        sanitize_json_for_ui(value)
    }

    fn render_markdown(&self, text: &str) -> String {
        super::super::utils::markdown::render_markdown(text)
    }
}

pub fn update_ui_displays(siv: &mut Cursive) {
    // Update all displays using the comprehensive UI wrapper
    if let Some(state) = siv.user_data::<AdvancedChatState>() {
        let ui = AdvancedChatUI { state: state.clone() };
        ui.update_all_displays(siv);  // This calls all the update functions including chat list
    }
}