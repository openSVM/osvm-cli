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
use super::theme::{Icons, ProgressBar, Spinners, StyledText};

impl AdvancedChatUI {
    /// Apply theme styling to a message based on its type
    fn format_message_with_theme(&self, message: &ChatMessage) -> String {
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
            ChatMessage::ToolCall {
                tool_name,
                description,
                args,
                execution_id,
            } => {
                let sanitized_tool_name = self.sanitize_text(tool_name);
                let sanitized_description = self.sanitize_text(description);

                let mut result = format!(
                    "Calling tool: {} - {}\n",
                    sanitized_tool_name, sanitized_description
                );

                if let Some(args) = args {
                    let sanitized_args = self.sanitize_json(args);
                    result.push_str(&format!("   Args: {}\n", sanitized_args));
                }
                result.push_str(&format!("   ID: {}\n\n", execution_id));
                result
            }
            ChatMessage::ToolResult {
                tool_name,
                result,
                execution_id,
            } => {
                let sanitized_tool_name = self.sanitize_text(tool_name);
                let sanitized_result = self.sanitize_json(result);
                format!(
                    "Tool {} result (ID: {}):\n{}\n\n",
                    sanitized_tool_name, execution_id, sanitized_result
                )
            }
            ChatMessage::AgentThinking(text) => {
                let sanitized = self.sanitize_text(text);
                format!("Agent (thinking): {}\n\n", sanitized)
            }
            ChatMessage::AgentPlan(plan) => {
                let mut result = String::from("Agent Plan:\n");
                for (i, step) in plan.iter().enumerate() {
                    let step_text = format!("{}: {}", step.tool_name, step.reason);
                    let sanitized = self.sanitize_text(&step_text);
                    result.push_str(&format!("  {}. {}\n", i + 1, sanitized));
                }
                result.push_str("\n");
                result
            }
            ChatMessage::Processing {
                message,
                spinner_index,
            } => {
                let spinner_chars = vec!["|", "/", "-", "\\", "|", "/", "-", "\\"];
                let spinner_char = spinner_chars[spinner_index % spinner_chars.len()];
                let sanitized = self.sanitize_text(message);
                format!("{} {}\n\n", spinner_char, sanitized)
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
        self.update_status_bar(siv);
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
                        AgentState::Idle => Icons::IDLE,
                        AgentState::Thinking => Icons::THINKING,
                        AgentState::Planning => Icons::PLANNING,
                        AgentState::ExecutingTool(_) => Icons::EXECUTING,
                        AgentState::Waiting => Icons::WAITING,
                        AgentState::Paused => Icons::PAUSED,
                        AgentState::Error(_) => Icons::ERROR,
                    };

                    let text_name = format!("");
                    let display_name = format!("{} {}", status_icon, name);
                    let is_active = Some(*id) == active_id;
                    let button_text = if is_active {
                        format!("{} {}", Icons::ARROW_RIGHT, display_name)
                    } else {
                        format!("  {}", display_name)
                    };

                    let session_id = *id;
                    let mut button = Button::new(button_text.clone(), move |siv| {
                        handle_chat_selection(siv, session_id);
                    });

                    chat_list.add_child(&text_name, button);
                }
            } else {
                // Same session count - for now just do a full rebuild
                // TODO: Implement true in-place updates when cursive API supports it
                chat_list.clear();

                for (id, name, agent_state) in session_names.iter() {
                    let status_icon = match agent_state {
                        AgentState::Idle => Icons::IDLE,
                        AgentState::Thinking => Icons::THINKING,
                        AgentState::Planning => Icons::PLANNING,
                        AgentState::ExecutingTool(_) => Icons::EXECUTING,
                        AgentState::Waiting => Icons::WAITING,
                        AgentState::Paused => Icons::PAUSED,
                        AgentState::Error(_) => Icons::ERROR,
                    };

                    let display_name = format!("{} {}", status_icon, name);
                    let is_active = Some(*id) == active_id;
                    let button_text = if is_active {
                        format!("{} {}", Icons::ARROW_RIGHT, display_name)
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

        if let Some(mut mcp_tools_view) =
            siv.find_name::<SelectView<(String, String)>>("mcp_tools_list")
        {
            mcp_tools_view.clear();

            if tools.is_empty() {
                mcp_tools_view.add_item("No tools available", ("".to_string(), "".to_string()));
            } else {
                // Add each tool individually
                for (server_id, tool_list) in tools.iter() {
                    // Add server header with icon
                    mcp_tools_view.add_item(
                        format!(
                            "{} {} ({} tools)",
                            Icons::FOLDER,
                            server_id,
                            tool_list.len()
                        ),
                        (server_id.clone(), "".to_string()),
                    );

                    // Add individual tools with icons
                    for tool in tool_list.iter() {
                        let display_name = format!("  {} {}", Icons::TOOL, tool.name);
                        mcp_tools_view
                            .add_item(display_name, (server_id.clone(), tool.name.clone()));
                    }
                }
            }
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
        let spinner_chars = Spinners::DOTS_WAVE;
        let spinner_char = spinner_chars[spinner_index % spinner_chars.len()];

        let timestamp = Utc::now().format("%H:%M:%S");

        let status_text = if let Some(session) = self.state.get_active_session() {
            match session.agent_state {
                AgentState::Idle => format!(
                    "{} Agent: Idle | Ready for tasks | {}",
                    Icons::IDLE,
                    timestamp
                ),
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
                    format!("Agent: Waiting | Awaiting response | {}", timestamp)
                }
                AgentState::Paused => {
                    format!("Agent: Paused | Operations suspended | {}", timestamp)
                }
                AgentState::Error(ref err) => format!("Agent: Error | {} | {}", err, timestamp),
            }
        } else {
            format!(
                "Agent: No active session | Select a chat session | {}",
                timestamp
            )
        };

        if let Some(mut status_display) = siv.find_name::<TextView>("agent_status") {
            status_display.set_content(status_text);
        }
    }

    pub fn update_status_bar(&self, siv: &mut Cursive) {
        use std::time::{Duration, Instant};

        // Check if we need to refresh the cached status (throttle to avoid blocking UI thread too often)
        const REFRESH_INTERVAL_SECS: u64 = 2; // Refresh every 2 seconds

        let should_refresh = {
            if let Ok(last_update) = self.state.last_status_update.read() {
                last_update
                    .map(|instant| instant.elapsed() > Duration::from_secs(REFRESH_INTERVAL_SECS))
                    .unwrap_or(true)
            } else {
                true
            }
        };

        // Only fetch new status if the cache is stale
        if should_refresh {
            // Spawn status fetch in background to avoid blocking UI thread
            // For now, we still need to block briefly, but we do it less frequently
            let status_text = tokio::task::block_in_place(|| {
                tokio::runtime::Handle::current().block_on(async {
                    let status =
                        crate::utils::agent_chat::system_status_bar::get_system_status().await;

                    // Format status similar to the carousel format
                    let mcp_info = if status.mcp_deployments.is_empty() {
                        "no MCP".to_string()
                    } else {
                        status
                            .mcp_deployments
                            .iter()
                            .map(|d| {
                                let server_short = d.server_name.replace("-mcp", "");
                                let microvm =
                                    d.microvm_id.as_ref().map(|id| id.as_str()).unwrap_or("?");
                                let extra_mounts = d.mounts.len().saturating_sub(1);
                                let readonly_count = d.mounts.iter().filter(|m| m.readonly).count();

                                if extra_mounts > 0 {
                                    if readonly_count > 0 {
                                        format!(
                                            "{}({}:cfg+{}:{}ro)",
                                            server_short, microvm, extra_mounts, readonly_count
                                        )
                                    } else {
                                        format!(
                                            "{}({}:cfg+{})",
                                            server_short, microvm, extra_mounts
                                        )
                                    }
                                } else {
                                    format!("{}({}:cfg)", server_short, microvm)
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(" ")
                    };

                    format!(
                        "OSVM: {}μVMs {}uK • {} • net:{} • up:{}s",
                        status.microvms_running,
                        status.unikernels_running,
                        mcp_info,
                        status.current_network,
                        status.uptime_seconds % 3600
                    )
                })
            });

            // Update the cache
            if let Ok(mut cached_status) = self.state.cached_status_text.write() {
                *cached_status = Some(status_text.clone());
            }
            if let Ok(mut last_update) = self.state.last_status_update.write() {
                *last_update = Some(Instant::now());
            }

            // Update the view
            if let Some(mut status_bar_view) = siv.find_name::<TextView>("system_status_bar") {
                let content_ref = status_bar_view.get_content();
                let current_content = content_ref.source().to_string();
                if current_content != status_text {
                    status_bar_view.set_content(status_text);
                }
            }
        } else {
            // Use cached status to avoid blocking
            if let Ok(cached_status) = self.state.cached_status_text.read() {
                if let Some(status_text) = cached_status.as_ref() {
                    if let Some(mut status_bar_view) =
                        siv.find_name::<TextView>("system_status_bar")
                    {
                        let content_ref = status_bar_view.get_content();
                        let current_content = content_ref.source().to_string();
                        if &current_content != status_text {
                            status_bar_view.set_content(status_text.clone());
                        }
                    }
                }
            }
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
