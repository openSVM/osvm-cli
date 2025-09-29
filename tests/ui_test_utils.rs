//! UI testing utilities for headless Cursive testing
//!
//! This module provides utilities to test the agent_chat_v2 UI components
//! without requiring a real terminal interface.

use cursive::backends::puppet::Backend as PuppetBackend;
use cursive::event::{Event, Key};
use cursive::{Cursive, CursiveExt, View};
use std::sync::Arc;
use uuid::Uuid;

use osvm::utils::agent_chat_v2::{update_ui_displays, AdvancedChatState, AdvancedChatUI};

/// Headless UI test environment
pub struct HeadlessUI {
    siv: Cursive,
    state: AdvancedChatState,
}

impl HeadlessUI {
    /// Create a new headless UI environment for testing
    pub async fn new() -> anyhow::Result<Self> {
        // Create a puppet backend (headless)
        let backend = Box::new(PuppetBackend::init(Some(cursive::Vec2::new(80, 24))));
        let mut siv = Cursive::new(backend);

        // Initialize state
        let state = AdvancedChatState::new()?;
        state.initialize().await?;
        state.start_agent_worker().await?;

        // Create a default session
        let _session_id = state.create_session("Test Session".to_string())?;

        // Set state as user data
        siv.set_user_data(state.clone());

        // Setup UI
        let ui = AdvancedChatUI {
            state: state.clone(),
        };
        ui.setup_far_ui(&mut siv);
        ui.setup_suggestion_hotkeys(&mut siv);
        ui.setup_action_hotkeys(&mut siv);

        update_ui_displays(&mut siv);

        Ok(Self { siv, state })
    }

    /// Send text input to the chat
    pub fn send_input(&mut self, text: &str) {
        // Focus the input field
        self.siv.focus_name("input").ok();

        // Type the text character by character
        for ch in text.chars() {
            self.siv.on_event(Event::Char(ch));
        }

        // Press Enter to send
        self.siv.on_event(Event::Key(Key::Enter));

        // Process events
        self.step();
    }

    /// Press a key
    pub fn press_key(&mut self, key: Key) {
        self.siv.on_event(Event::Key(key));
        self.step();
    }

    /// Press a character key
    pub fn press_char(&mut self, ch: char) {
        self.siv.on_event(Event::Char(ch));
        self.step();
    }

    /// Click a button by name
    pub fn click_button(&mut self, button_name: &str) -> bool {
        // Try to trigger button callback
        // This is a simplified version - in real testing you'd need more sophisticated event handling
        if let Some(mut button) = self.siv.find_name::<cursive::views::Button>(button_name) {
            button.on_event(Event::Key(Key::Enter));
            self.step();
            true
        } else {
            false
        }
    }

    /// Create a new chat session through UI
    pub fn create_new_chat(&mut self, name: &str) {
        // Simulate clicking "New Chat" button
        self.click_button("new_chat");

        // Type the name
        for ch in name.chars() {
            self.press_char(ch);
        }

        // Press Enter or click Create
        self.press_key(Key::Enter);
    }

    /// Select a chat session by index
    pub fn select_chat(&mut self, index: usize) {
        if let Some(mut chat_list) = self
            .siv
            .find_name::<cursive::views::SelectView<Uuid>>("chat_list")
        {
            chat_list.set_selection(index);
            self.step();
        }
    }

    /// Get current chat display content
    pub fn get_chat_content(&mut self) -> String {
        if let Some(chat_display) = self
            .siv
            .find_name::<cursive::views::TextView>("chat_display")
        {
            chat_display.get_content().source().to_string()
        } else {
            String::new()
        }
    }

    /// Get current input field content
    pub fn get_input_content(&mut self) -> String {
        if let Some(input) = self.siv.find_name::<cursive::views::EditView>("input") {
            input.get_content().to_string()
        } else {
            String::new()
        }
    }

    /// Get list of chat session names
    pub fn get_chat_names(&mut self) -> Vec<String> {
        if let Some(chat_list) = self
            .siv
            .find_name::<cursive::views::SelectView<Uuid>>("chat_list")
        {
            (0..chat_list.len())
                .map(|i| chat_list.get_item(i).unwrap().0.to_string())
                .collect()
        } else {
            vec![]
        }
    }

    /// Step the UI (process events)
    pub fn step(&mut self) {
        // Cursive puppet backend doesn't have step() method
        // Instead we just update displays
        update_ui_displays(&mut self.siv);
    }

    /// Run UI for a specific number of steps
    pub fn run_steps(&mut self, steps: usize) {
        for _ in 0..steps {
            self.step();
        }
    }

    /// Check if suggestions are visible
    pub fn are_suggestions_visible(&self) -> bool {
        self.state
            .suggestions_visible
            .read()
            .map(|v| *v)
            .unwrap_or(false)
    }

    /// Get current suggestions
    pub fn get_suggestions(&self) -> Vec<String> {
        self.state
            .current_suggestions
            .read()
            .map(|s| s.clone())
            .unwrap_or_default()
    }

    /// Get current agent state for active session
    pub fn get_agent_state(&self) -> Option<osvm::utils::agent_chat_v2::AgentState> {
        let session = self.state.get_active_session()?;
        Some(session.agent_state)
    }

    /// Wait for agent to finish processing (with timeout)
    pub async fn wait_for_agent_idle(&self, timeout_secs: u64) -> bool {
        let timeout = std::time::Duration::from_secs(timeout_secs);
        let start = std::time::Instant::now();

        while start.elapsed() < timeout {
            if let Some(session) = self.state.get_active_session() {
                if matches!(
                    session.agent_state,
                    osvm::utils::agent_chat_v2::AgentState::Idle
                ) {
                    return true;
                }
            }
            tokio::time::sleep(std::time::Duration::from_millis(100)).await;
        }

        false
    }

    /// Get underlying state for direct testing
    pub fn state(&self) -> &AdvancedChatState {
        &self.state
    }

    /// Access cursive instance directly
    pub fn cursive(&mut self) -> &mut Cursive {
        &mut self.siv
    }
}
