//! Main UI layout and setup

use anyhow::Result;
use cursive::direction::Orientation;
use cursive::traits::*;
use cursive::views::{
    Button, Dialog, DummyView, EditView, LinearLayout, ListView, NamedView, Panel, ResizedView,
    ScrollView, SelectView, TextView,
};
use cursive::{Cursive, CursiveExt, View};
use cursive_multiplex::{Id, Mux};
use uuid::Uuid;

use super::super::state::AdvancedChatState;
use super::super::types::AgentState;
use super::handlers::show_context_menu;
use super::handlers::*;
use super::theme::{Decorations, Icons, ModernTheme};

/// Update input panel title to show history indicator
fn update_input_title(siv: &mut Cursive, state: &AdvancedChatState) {
    let indicator = state.get_history_indicator();
    let title = if indicator.is_empty() {
        "Input".to_string()
    } else {
        format!("Input{}", indicator)
    };

    // Update the input panel title if it exists
    if let Some(mut panel) = siv.find_name::<Panel<LinearLayout>>("input_panel") {
        panel.set_title(title);
    }
}

/// FAR-style/Borland UI implementation
pub struct AdvancedChatUI {
    pub state: AdvancedChatState,
}

impl AdvancedChatUI {
    pub fn new() -> Result<Self> {
        let state = AdvancedChatState::new()?;
        Ok(AdvancedChatUI { state })
    }

    /// Apply current theme to cursive interface
    pub fn apply_theme_to_cursive(&self, siv: &mut Cursive) -> Result<()> {
        let theme_manager = self
            .state
            .theme_manager
            .read()
            .map_err(|e| anyhow::anyhow!("Failed to acquire theme manager lock: {}", e))?;

        let theme = theme_manager.current_theme();

        // Convert our theme to cursive theme
        let mut cursive_theme = cursive::theme::Theme::default();

        // Apply background colors
        if let Some(bg) = &theme.background.background {
            cursive_theme.palette[cursive::theme::PaletteColor::Background] =
                self.convert_color_to_cursive(bg);
        }

        // Apply text colors
        cursive_theme.palette[cursive::theme::PaletteColor::View] =
            self.convert_color_to_cursive(&theme.text.color);

        // Apply accent colors for highlights
        cursive_theme.palette[cursive::theme::PaletteColor::Highlight] =
            self.convert_color_to_cursive(&theme.accent.color);

        // Apply border colors
        cursive_theme.palette[cursive::theme::PaletteColor::TitlePrimary] =
            self.convert_color_to_cursive(&theme.border.color);

        siv.set_theme(cursive_theme);
        Ok(())
    }

    /// Convert our color format to cursive Color
    fn convert_color_to_cursive(
        &self,
        color: &crate::utils::themes::Color,
    ) -> cursive::theme::Color {
        use crate::utils::themes::Color as ThemeColor;
        use cursive::theme::{BaseColor, Color as CursiveColor};

        match color {
            ThemeColor::Named(name) => match name.as_str() {
                "black" => CursiveColor::Dark(BaseColor::Black),
                "red" => CursiveColor::Dark(BaseColor::Red),
                "green" => CursiveColor::Dark(BaseColor::Green),
                "yellow" => CursiveColor::Dark(BaseColor::Yellow),
                "blue" => CursiveColor::Dark(BaseColor::Blue),
                "magenta" => CursiveColor::Dark(BaseColor::Magenta),
                "cyan" => CursiveColor::Dark(BaseColor::Cyan),
                "white" => CursiveColor::Dark(BaseColor::White),
                _ => CursiveColor::Dark(BaseColor::White), // Default fallback
            },
            ThemeColor::Rgb(r, g, b) => CursiveColor::Rgb(*r, *g, *b),
            ThemeColor::Hex(hex_str) => {
                // Parse hex color
                if let Ok(rgb) = self.parse_hex_color(hex_str) {
                    CursiveColor::Rgb(rgb.0, rgb.1, rgb.2)
                } else {
                    CursiveColor::Dark(BaseColor::White)
                }
            }
            ThemeColor::Indexed(index) => CursiveColor::from_256colors(*index as u8),
            _ => CursiveColor::Dark(BaseColor::White), // Default fallback
        }
    }

    /// Parse hex color string to RGB tuple
    fn parse_hex_color(&self, hex: &str) -> Result<(u8, u8, u8), std::num::ParseIntError> {
        let hex = hex.trim_start_matches('#');
        let r = u8::from_str_radix(&hex[0..2], 16)?;
        let g = u8::from_str_radix(&hex[2..4], 16)?;
        let b = u8::from_str_radix(&hex[4..6], 16)?;
        Ok((r, g, b))
    }

    pub fn setup_far_ui(&self, siv: &mut Cursive) {
        let state = self.state.clone();

        // Apply modern dark theme instead of default
        siv.set_theme(ModernTheme::dark());

        // Or try to apply current theme if available
        if let Err(e) = self.apply_theme_to_cursive(siv) {
            log::warn!("Failed to apply custom theme, using modern dark theme");
        }

        // Main horizontal layout: Chat List | Chat History
        let mut main_layout = LinearLayout::horizontal();

        // Left panel: Chat sessions list - responsive width based on terminal size
        let terminal_size = siv.screen_size();
        let left_width = if terminal_size.x > 120 {
            35 // Wide terminal - more space for session list
        } else if terminal_size.x > 80 {
            30 // Medium terminal
        } else {
            25 // Narrow terminal - minimal session list width
        };

        let chat_list_panel = self.create_chat_list_panel();
        main_layout.add_child(ResizedView::with_fixed_width(left_width, chat_list_panel));

        // Right panel: Active chat and controls - takes remaining space
        let chat_panel = self.create_chat_panel();
        main_layout.add_child(chat_panel.full_width());

        // Wrap in main dialog with dynamic title showing agent status with icons
        let title = if let Some(session) = self.state.get_active_session() {
            match session.agent_state {
                AgentState::Idle => format!("{} OSVM Agent - Idle", Icons::IDLE),
                AgentState::Thinking => format!("{} OSVM Agent - Thinking...", Icons::THINKING),
                AgentState::Planning => format!("{} OSVM Agent - Planning...", Icons::PLANNING),
                AgentState::ExecutingTool(ref tool) => {
                    format!("{} OSVM Agent - Executing {}", Icons::EXECUTING, tool)
                }
                AgentState::Waiting => format!("{} OSVM Agent - Waiting", Icons::WAITING),
                AgentState::Paused => format!("{} OSVM Agent - Paused", Icons::PAUSED),
                AgentState::Error(ref err) => {
                    format!("{} OSVM Agent - Error: {}", Icons::ERROR, err)
                }
            }
        } else {
            format!(
                "{} {} OSVM Agent Chat {}",
                Icons::ROCKET,
                Icons::SPARKLES,
                Icons::SPARKLES
            )
        };
        let dialog = Dialog::around(main_layout)
            .title(Decorations::fancy_header(&title))
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
        siv.add_global_callback(
            cursive::event::Event::Shift(cursive::event::Key::Tab),
            |s| {
                // Try to focus input first, then chat list as fallback
                if s.focus_name("input").is_ok() {
                    // Successfully focused input
                } else {
                    let _ = s.focus_name("chat_list");
                }
            },
        );

        // Tab and Arrow keys will handle navigation naturally through the ListView buttons

        // Add context menu via F10 key (standard UI convention)
        siv.add_global_callback(cursive::event::Key::F10, |s| {
            show_context_menu(s, cursive::Vec2::new(0, 0));
        });

        // Also add Shift+F10 for alternate context menu access
        siv.add_global_callback(
            cursive::event::Event::Shift(cursive::event::Key::F10),
            |s| {
                show_context_menu(s, cursive::Vec2::new(0, 0));
            },
        );

        // Add F1 for help (universal help key)
        siv.add_global_callback(cursive::event::Key::F1, |s| {
            show_advanced_help(s);
        });

        // Add '?' for help (alternative)
        siv.add_global_callback(cursive::event::Event::Char('?'), |s| {
            show_keyboard_shortcuts_hint(s);
        });

        // Add up/down arrow handlers for input history navigation
        let state_clone = self.state.clone();
        siv.add_global_callback(cursive::event::Key::Up, move |s| {
            // Only handle if input field has focus
            if s.find_name::<EditView>("input").is_some() {
                if let Some(prev_input) = state_clone.history_previous() {
                    if let Some(mut input) = s.find_name::<EditView>("input") {
                        input.set_content(prev_input);
                        // Move cursor to end
                        let len = input.get_content().len();
                        input.set_cursor(len);
                    }

                    // Update input panel title to show history indicator
                    update_input_title(s, &state_clone);
                }
            }
        });

        let state_clone = self.state.clone();
        siv.add_global_callback(cursive::event::Key::Down, move |s| {
            // Only handle if input field has focus
            if s.find_name::<EditView>("input").is_some() {
                if let Some(next_input) = state_clone.history_next() {
                    if let Some(mut input) = s.find_name::<EditView>("input") {
                        input.set_content(next_input);
                        // Move cursor to end
                        let len = input.get_content().len();
                        input.set_cursor(len);
                    }

                    // Update input panel title to show history indicator
                    update_input_title(s, &state_clone);
                }
            }
        });

        // Add resize handling with error protection
        siv.add_global_callback(cursive::event::Event::WindowResize, |s| {
            // Protect against resize-induced panics
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                // Gentle refresh - just update displays, don't recreate UI
                super::display::update_ui_displays(s);

                // Force screen refresh to prevent rendering artifacts
                s.clear();
            })) {
                Ok(_) => {
                    // Resize handled successfully
                }
                Err(e) => {
                    log::error!("Window resize caused panic: {:?}", e);
                    // Don't crash - just log and continue
                    // The UI will be slightly off but functional
                }
            }
        });
    }

    pub fn setup_action_hotkeys(&self, siv: &mut Cursive) {
        // F12: Take screenshot
        siv.add_global_callback(cursive::event::Key::F12, |siv| {
            take_screenshot(siv);
        });

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
                let suggestions_visible = state
                    .suggestions_visible
                    .read()
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

            siv.add_global_callback(
                cursive::event::Event::AltChar(char::from_digit(i as u32, 10).unwrap()),
                move |s| {
                    // Only insert suggestion if suggestions are visible AND input has focus
                    let suggestions_visible = state
                        .suggestions_visible
                        .read()
                        .map(|v| *v)
                        .unwrap_or(false);

                    if suggestions_visible && s.find_name::<EditView>("input").is_some() {
                        insert_suggestion_at_cursor(s, (i - 1) as usize, state.clone());
                    }
                },
            );
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
