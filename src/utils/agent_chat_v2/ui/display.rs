//! Display update logic and UI refresh

use crate::utils::themes::{TextStyle, Theme};
use cursive::theme::{BaseColor, Color, ColorStyle};
use cursive::traits::*;
use cursive::utils::markup::StyledString;
use cursive::views::{Button, LinearLayout, ListView, SelectView, TextView};
use cursive::{Cursive, CursiveExt};
use uuid::Uuid;

use super::super::state::AdvancedChatState;
use super::super::types::{AgentState, ChatMessage};
use super::super::utils::markdown::render_markdown;
use super::super::utils::{sanitize_json_for_ui, sanitize_text_for_ui};
use super::handlers::handle_chat_selection;
use super::layout::AdvancedChatUI;

impl AdvancedChatUI {
    /// Apply theme styling to a message based on its type
    fn format_message_with_theme(&self, message: &ChatMessage) -> String {
        let theme_manager = match self.state.theme_manager.read() {
            Ok(tm) => tm,
            Err(_) => return self.format_message_fallback(message), // Fallback if lock fails
        };

        let theme = theme_manager.current_theme();

        match message {
            ChatMessage::User(text) => {
                let sanitized = self.sanitize_text(text);
                let user_style = theme.get_style("primary");
                let action_style = theme.get_style("muted");
                format!(
                    "{}\n{}\n\n",
                    user_style.apply(&format!("👤 You: {}", sanitized)),
                    action_style.apply("   [R]etry [C]opy [D]elete   (Alt+R/C/D)")
                )
            }
            ChatMessage::Agent(text) => {
                let rendered = self.render_markdown(text);
                let sanitized = self.sanitize_text(&rendered);
                let agent_style = theme.get_style("secondary");
                let action_style = theme.get_style("muted");
                format!(
                    "{}:\n{}\n{}\n\n",
                    agent_style.apply("🤖 Agent"),
                    theme.get_style("text").apply(&sanitized),
                    action_style.apply("   [F]ork [C]opy [R]etry [D]elete   (Alt+F/C/R/D)")
                )
            }
            ChatMessage::System(text) => {
                let sanitized = self.sanitize_text(text);
                let system_style = theme.get_style("info");
                format!(
                    "{}\n\n",
                    system_style.apply(&format!("ℹ️  System: {}", sanitized))
                )
            }
            ChatMessage::Error(text) => {
                let sanitized = self.sanitize_text(text);
                let error_style = theme.get_style("error");
                format!(
                    "{}\n\n",
                    error_style.apply(&format!("❌ Error: {}", sanitized))
                )
            }
            ChatMessage::ToolCall {
                tool_name,
                description,
                args,
                execution_id,
            } => {
                let sanitized_tool_name = self.sanitize_text(tool_name);
                let sanitized_description = self.sanitize_text(description);
                let tool_style = theme.get_style("command");
                let info_style = theme.get_style("muted");

                let mut result = format!(
                    "{}\n",
                    tool_style.apply(&format!(
                        "⚡ Calling tool: {} - {}",
                        sanitized_tool_name, sanitized_description
                    ))
                );

                if let Some(args) = args {
                    let sanitized_args = self.sanitize_json(args);
                    result.push_str(&format!(
                        "{}\n",
                        info_style.apply(&format!("   Args: {}", sanitized_args))
                    ));
                }
                result.push_str(&format!(
                    "{}\n\n",
                    info_style.apply(&format!("   ID: {}", execution_id))
                ));
                result
            }
            ChatMessage::ToolResult {
                tool_name,
                result,
                execution_id,
            } => {
                let sanitized_tool_name = self.sanitize_text(tool_name);
                let success_style = theme.get_style("success");
                let info_style = theme.get_style("muted");
                let result_text = format!(
                    "✅ Tool {} result (ID: {}):\n",
                    sanitized_tool_name, execution_id
                );
                let sanitized_result = self.sanitize_json(result);
                format!(
                    "{}{}\n\n",
                    success_style.apply(&result_text),
                    theme.get_style("text").apply(&sanitized_result)
                )
            }
            ChatMessage::AgentThinking(text) => {
                let sanitized = self.sanitize_text(text);
                let thinking_style = theme.get_style("accent");
                format!(
                    "{}\n\n",
                    thinking_style.apply(&format!("💭 Agent (thinking): {}", sanitized))
                )
            }
            ChatMessage::AgentPlan(plan) => {
                let plan_style = theme.get_style("accent");
                let mut result = format!("{}\n", plan_style.apply("📋 Agent Plan:"));
                let text_style = theme.get_style("text");
                for (i, step) in plan.iter().enumerate() {
                    let step_text = format!("{}: {}", step.tool_name, step.reason);
                    let sanitized = self.sanitize_text(&step_text);
                    result.push_str(&format!(
                        "{}\n",
                        text_style.apply(&format!("  {}. {}", i + 1, sanitized))
                    ));
                }
                result.push_str("\n");
                result
            }
            ChatMessage::Processing {
                message,
                spinner_index,
            } => {
                let spinner_chars = vec!["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
                let spinner_char = spinner_chars[spinner_index % spinner_chars.len()];
                let sanitized = self.sanitize_text(message);
                let processing_style = theme.get_style("accent");
                format!(
                    "{}\n\n",
                    processing_style.apply(&format!("{} {}", spinner_char, sanitized))
                )
            }
        }
    }

    /// Fallback message formatting without themes
    fn format_message_fallback(&self, message: &ChatMessage) -> String {
        match message {
            ChatMessage::User(text) => {
                let sanitized = self.sanitize_text(text);
                format!(
                    "You: {}\n   [R]etry [C]opy [D]elete   (Alt+R/C/D)\n\n",
                    sanitized
                )
            }
            ChatMessage::Agent(text) => {
                let rendered = self.render_markdown(text);
                let sanitized = self.sanitize_text(&rendered);
                format!(
                    "Agent:\n{}\n   [F]ork [C]opy [R]etry [D]elete   (Alt+F/C/R/D)\n\n",
                    sanitized
                )
            }
            ChatMessage::System(text) => {
                let sanitized = self.sanitize_text(text);
                format!("System: {}\n\n", sanitized)
            }
            ChatMessage::Error(text) => {
                let sanitized = self.sanitize_text(text);
                format!("Error: {}\n\n", sanitized)
            }
            _ => "Message formatting error\n\n".to_string(),
        }
    }

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
                display_text.push_str(&self.format_message_with_theme(message));
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
            let no_session_text =
                "No active session. Use Tab to navigate to the session list and select a session.";
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
        use chrono::{DateTime, Utc};
        use std::sync::atomic::Ordering;

        let spinner_index = self.state.spinner_state.load(Ordering::Relaxed);
        let spinner_chars = vec!["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
        let spinner_char = spinner_chars[spinner_index % spinner_chars.len()];

        let timestamp = Utc::now().format("%H:%M:%S");

        let status_text = if let Some(session) = self.state.get_active_session() {
            match session.agent_state {
                AgentState::Idle => format!("🤖 Agent: Idle | Ready for tasks | {}", timestamp),
                AgentState::Thinking => format!(
                    "{} Agent: Thinking... | Analyzing request | {}",
                    spinner_char, timestamp
                ),
                AgentState::Planning => format!(
                    "{} Agent: Planning... | Creating execution plan | {}",
                    spinner_char, timestamp
                ),
                AgentState::ExecutingTool(ref tool_name) => format!(
                    "{} Agent: Executing {} | Processing... | {}",
                    spinner_char, tool_name, timestamp
                ),
                AgentState::Waiting => {
                    format!("⏳ Agent: Waiting | Awaiting response | {}", timestamp)
                }
                AgentState::Paused => {
                    format!("⏸️ Agent: Paused | Operations suspended | {}", timestamp)
                }
                AgentState::Error(ref err) => format!("⚠️ Agent: Error | {} | {}", err, timestamp),
            }
        } else {
            format!(
                "❓ Agent: No active session | Select a chat session | {}",
                timestamp
            )
        };

        if let Some(mut status_display) = siv.find_name::<TextView>("agent_status") {
            status_display.set_content(status_text);
        }
    }

    pub fn update_suggestions_display(&self, siv: &mut Cursive) {
        let suggestions_visible = self
            .state
            .suggestions_visible
            .read()
            .map(|v| *v)
            .unwrap_or(false);

        if let Some(mut container) = siv.find_name::<LinearLayout>("suggestions_container") {
            container.clear();

            if suggestions_visible {
                let suggestions = self
                    .state
                    .current_suggestions
                    .read()
                    .map(|s| s.clone())
                    .unwrap_or_default();

                if !suggestions.is_empty() {
                    for (i, sugg) in suggestions.iter().enumerate() {
                        let mut styled = StyledString::new();

                        // Create blue background with white text style
                        let blue_style = ColorStyle::new(
                            Color::Light(BaseColor::White),
                            Color::Dark(BaseColor::Blue),
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
        let ui = AdvancedChatUI {
            state: state.clone(),
        };
        ui.update_all_displays(siv); // This calls all the update functions including chat list
    }
}
