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
use super::handlers::show_context_menu;

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

        // Left panel: Chat sessions list - responsive width
        let chat_list_panel = self.create_chat_list_panel();
        main_layout.add_child(ResizedView::with_min_width(25, chat_list_panel).max_width(40));

        // Right panel: Active chat and controls - takes remaining space
        let chat_panel = self.create_chat_panel();
        main_layout.add_child(ResizedView::with_min_width(50, chat_panel).full_width());

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

        // Set focus to input field initially
        siv.focus_name("input").ok();

        // Add Tab navigation between key UI elements - but don't interfere with other keys
        siv.add_global_callback(cursive::event::Key::Tab, |s| {
            // Try to focus the chat list first, then input as fallback
            if s.focus_name("chat_list").is_ok() {
                // Successfully focused chat list
            } else {
                let _ = s.focus_name("input");
            }
        });

        // Add Shift+Tab for reverse navigation
        siv.add_global_callback(cursive::event::Event::Shift(cursive::event::Key::Tab), |s| {
            // Try to focus input first, then chat list as fallback
            if s.focus_name("input").is_ok() {
                // Successfully focused input
            } else {
                let _ = s.focus_name("chat_list");
            }
        });

        // Tab and Arrow keys will handle navigation naturally through the ListView buttons

        // Add context menu via F10 key (standard UI convention)
        siv.add_global_callback(cursive::event::Key::F10, |s| {
            show_context_menu(s, cursive::Vec2::new(0, 0));
        });

        // Also add Shift+F10 for alternate context menu access
        siv.add_global_callback(cursive::event::Event::Shift(cursive::event::Key::F10), |s| {
            show_context_menu(s, cursive::Vec2::new(0, 0));
        });

        // Add resize handling to prevent crashes - gentle approach
        siv.add_global_callback(cursive::event::Event::WindowResize, |s| {
            // Gentle refresh - just update displays, don't recreate UI
            super::display::update_ui_displays(s);
        });
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
        // Add Ctrl+number key handlers for suggestions - only when input has focus
        for i in 1..=5 {
            let state = self.state.clone();
            let key_char = char::from_digit(i as u32, 10).unwrap();

            siv.add_global_callback(cursive::event::Event::CtrlChar(key_char), move |s| {
                // Only insert suggestion if suggestions are visible AND input has focus
                let suggestions_visible = state.suggestions_visible.read()
                    .map(|v| *v)
                    .unwrap_or(false);

                if suggestions_visible && s.find_name::<EditView>("input").is_some() {
                    insert_suggestion_at_cursor(s, (i - 1) as usize, state.clone());
                }
            });
        }

        // Also add Alt+number for easier access
        for i in 1..=5 {
            let state = self.state.clone();

            siv.add_global_callback(cursive::event::Event::AltChar(char::from_digit(i as u32, 10).unwrap()), move |s| {
                // Only insert suggestion if suggestions are visible AND input has focus
                let suggestions_visible = state.suggestions_visible.read()
                    .map(|v| *v)
                    .unwrap_or(false);

                if suggestions_visible && s.find_name::<EditView>("input").is_some() {
                    insert_suggestion_at_cursor(s, (i - 1) as usize, state.clone());
                }
            });
        }

        // Hide suggestions on Escape - but don't interfere with other Escape usage
        let state = self.state.clone();
        siv.add_global_callback(cursive::event::Key::Esc, move |_s| {
            if let Ok(mut vis) = state.suggestions_visible.write() {
                *vis = false;
            }
        });

        // Emergency clear - Ctrl+Alt+X to clear stuck processing states (safe combination)
        let state = self.state.clone();
        siv.add_global_callback(cursive::event::Event::AltChar('x'), move |siv| {
            // Clear all processing messages from active session
            if let Some(session) = state.get_active_session() {
                let session_id = session.id;
                if let Err(e) = state.remove_last_processing_message(session_id) {
                    eprintln!("Emergency clear failed: {}", e);
                }
                super::display::update_ui_displays(siv);
            }
        });
    }
}