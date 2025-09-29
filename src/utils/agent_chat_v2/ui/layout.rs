//! Main UI layout and setup

use cursive::{Cursive, CursiveExt, View};
use cursive::views::{
    Dialog, EditView, LinearLayout, TextView, ScrollView, Panel, Button,
    ListView, SelectView, ResizedView, DummyView, NamedView
};
use cursive::traits::*;
use cursive::direction::Orientation;
use cursive_multiplex::{Mux, Id};
use anyhow::Result;
use uuid::Uuid;

use super::super::state::AdvancedChatState;
use super::super::types::AgentState;
use super::handlers::*;

/// FAR-style/Borland UI implementation
pub struct AdvancedChatUI {
    pub state: AdvancedChatState,
}

impl AdvancedChatUI {
    pub fn new() -> Result<Self> {
        let state = AdvancedChatState::new()?;
        Ok(AdvancedChatUI { state })
    }

    pub fn setup_far_ui(&self, siv: &mut Cursive) {
        let state = self.state.clone();

        // Main horizontal layout: Chat List | Chat History
        let mut main_layout = LinearLayout::horizontal();

        // Left panel: Chat sessions list
        let chat_list_panel = self.create_chat_list_panel();
        main_layout.add_child(ResizedView::with_fixed_width(30, chat_list_panel));

        // Right panel: Active chat and controls
        let chat_panel = self.create_chat_panel();
        main_layout.add_child(chat_panel.full_width());

        // Wrap in main dialog with dynamic title showing agent status
        let title = if let Some(session) = self.state.get_active_session() {
            match session.agent_state {
                AgentState::Idle => "OSVM Agent - Idle".to_string(),
                AgentState::Thinking => "OSVM Agent - Thinking...".to_string(),
                AgentState::Planning => "OSVM Agent - Planning...".to_string(),
                AgentState::ExecutingTool(ref tool) => format!("OSVM Agent - Executing {}", tool),
                AgentState::Waiting => "OSVM Agent - Waiting".to_string(),
                AgentState::Paused => "OSVM Agent - Paused".to_string(),
                AgentState::Error(ref err) => format!("OSVM Agent - Error: {}", err),
            }
        } else {
            "OSVM Agent".to_string()
        };
        let dialog = Dialog::around(main_layout)
            .title(title)
            .title_position(cursive::align::HAlign::Center);

        siv.add_fullscreen_layer(dialog);

        // Set focus to input field
        siv.focus_name("input").ok();
    }

    pub fn setup_action_hotkeys(&self, siv: &mut Cursive) {
        // Alt+M: Switch back to standard mode
        siv.add_global_callback(cursive::event::Event::AltChar('m'), |siv| {
            siv.add_layer(
                Dialog::text("Switching back to standard mode requires restarting.\nPlease restart with 'osvm chat' (without --advanced)")
                    .title("Switch Mode")
                    .button("OK", |s| { s.pop_layer(); })
            );
        });

        let state = self.state.clone();
        // Alt+R: Retry last message
        siv.add_global_callback(cursive::event::Event::AltChar('r'), move |s| {
            retry_last_message(s, state.clone());
        });

        let state = self.state.clone();
        // Alt+C: Copy last message
        siv.add_global_callback(cursive::event::Event::AltChar('c'), move |s| {
            copy_last_message(s, state.clone());
        });

        let state = self.state.clone();
        // Alt+D: Delete last message
        siv.add_global_callback(cursive::event::Event::AltChar('d'), move |s| {
            delete_last_message(s, state.clone());
        });

        let state = self.state.clone();
        // Alt+F: Fork conversation
        siv.add_global_callback(cursive::event::Event::AltChar('f'), move |s| {
            fork_conversation(s, state.clone());
        });
    }

    pub fn setup_suggestion_hotkeys(&self, siv: &mut Cursive) {
        // Add number key handlers for suggestions
        for i in 1..=5 {
            let state = self.state.clone();
            let key_char = char::from_digit(i as u32, 10).unwrap();

            siv.add_global_callback(key_char, move |s| {
                insert_suggestion_at_cursor(s, (i - 1) as usize, state.clone());
            });
        }

        // Hide suggestions on Escape
        let state = self.state.clone();
        siv.add_global_callback(cursive::event::Key::Esc, move |_s| {
            if let Ok(mut vis) = state.suggestions_visible.write() {
                *vis = false;
            }
        });
    }
}