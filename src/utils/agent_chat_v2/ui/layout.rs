//! Main UI layout and setup

use anyhow::Result;
use cursive::direction::Orientation;
use cursive::traits::*;
use cursive::views::{
    Button, Dialog, DummyView, EditView, LinearLayout, ListView, NamedView, Panel, ResizedView,
    ScrollView, SelectView, TextArea, TextView,
};
use cursive::{Cursive, CursiveExt, View};
use cursive_multiplex::{Id, Mux};
use uuid::Uuid;

use super::super::state::AdvancedChatState;
use super::super::types::AgentState;
use super::handlers::show_context_menu;
use super::handlers::*;
use super::text_area_wrapper::SendableTextArea;
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
    /// BUG-2019 fix: Validate length and characters before indexing to prevent panics
    fn parse_hex_color(&self, hex: &str) -> Result<(u8, u8, u8), Box<dyn std::error::Error>> {
        let hex_trimmed = hex.trim_start_matches('#').to_string();

        // Validate length FIRST - prevent index out of bounds panic
        if hex_trimmed.len() != 6 {
            return Err(format!(
                "Hex color must be exactly 6 characters, got {} from '{}'",
                hex_trimmed.len(),
                hex
            )
            .into());
        }

        // Use character iteration for UTF-8 safety (though hex should be ASCII)
        let hex_chars: Vec<char> = hex_trimmed.chars().collect();

        // Validate all characters are valid hex digits
        for (i, ch) in hex_chars.iter().enumerate() {
            if !ch.is_ascii_hexdigit() {
                return Err(format!(
                    "Invalid hex digit '{}' at position {} in color '{}'",
                    ch, i, hex
                )
                .into());
            }
        }

        // Now safe to parse - length and characters are validated
        let r = u8::from_str_radix(&hex_trimmed[0..2], 16)?;
        let g = u8::from_str_radix(&hex_trimmed[2..4], 16)?;
        let b = u8::from_str_radix(&hex_trimmed[4..6], 16)?;

        Ok((r, g, b))
    }

    pub fn setup_far_ui(&self, siv: &mut Cursive) {
        let state = self.state.clone();

        // Apply VS Code dark theme ONLY - no custom theme loading
        siv.set_theme(ModernTheme::dark());

        // Install key diagnostics logger for debugging key events
        super::key_diagnostics::install_key_logger(siv);

        // NEW LAYOUT: Vertical layout with menu bar at top
        let mut root_layout = LinearLayout::vertical();

        // Add top menu bar (Microsoft Edit style)
        root_layout.add_child(self.create_top_menu_bar());

        // Main content: Horizontal layout with sidebar | chat area
        let mut content_layout = LinearLayout::horizontal();

        // Left sidebar: Unified sessions + MCP tools - responsive width
        let terminal_size = siv.screen_size();
        let left_width = if terminal_size.x > 120 {
            30 // Wide terminal
        } else if terminal_size.x > 80 {
            28 // Medium terminal
        } else {
            25 // Narrow terminal
        };

        content_layout.add_child(ResizedView::with_fixed_width(
            left_width,
            self.create_unified_sidebar(),
        ));

        // Right panel: Simplified chat area (no duplicate buttons)
        content_layout.add_child(self.create_simplified_chat_panel().full_width());

        // Add content to root
        root_layout.add_child(content_layout.full_height());

        // Add fullscreen layer WITHOUT Dialog wrapper (menu bar is the title)
        siv.add_fullscreen_layer(root_layout);

        // BUG-2017 fix: Defer focus to after render event
        // Setting focus immediately after add_fullscreen_layer causes race condition
        // because named views aren't registered yet. Use refresh event to defer.
        siv.add_global_callback(cursive::event::Event::Refresh, |s| {
            // This callback fires after initial render, so named views are ready
            if let Err(e) = s.focus_name("input") {
                log::warn!("Failed to focus input field on initial render: {}", e);
            }
        });

        // Add Tab navigation between key UI elements - proper cycling without fallback trap
        // BUG-2024 fix: Implement better Tab navigation with cycling
        // BUG-2018 fix: Replace silent error drops with proper logging (from Phase 3 BUG-2011 pattern)
        // Fixed: Try chat_list first, if already focused move to input. Try only once per press.
        siv.add_global_callback(cursive::event::Key::Tab, |s| {
            // Try to cycle: input -> chat_list -> input
            // First attempt: focus chat_list
            if s.focus_name("chat_list").is_ok() {
                // Successfully moved to chat_list
                return;
            }

            // If chat_list focus failed (already focused or doesn't exist), try input
            if let Err(e) = s.focus_name("input") {
                log::warn!(
                    "Failed to navigate on Tab - both chat_list and input unavailable: {}",
                    e
                );
            }
        });

        // Add Shift+Tab for reverse navigation
        siv.add_global_callback(
            cursive::event::Event::Shift(cursive::event::Key::Tab),
            |s| {
                // Reverse cycle: input -> chat_list -> input (but reverse order)
                // First attempt: focus input
                if s.focus_name("input").is_ok() {
                    // Successfully moved to input
                    return;
                }

                // If input focus failed (already focused or doesn't exist), try chat_list
                if let Err(e) = s.focus_name("chat_list") {
                    log::warn!("Failed to navigate on Shift+Tab - both input and chat_list unavailable: {}", e);
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

        // Add F1/F2/F3 for tiered help system (progressive learning)
        siv.add_global_callback(cursive::event::Key::F1, |s| {
            show_essential_shortcuts(s); // Level 1: New users
        });

        siv.add_global_callback(cursive::event::Key::F2, |s| {
            show_common_shortcuts(s); // Level 2: Regular users
        });

        siv.add_global_callback(cursive::event::Key::F3, |s| {
            show_advanced_shortcuts(s); // Level 3: Power users
        });

        // Add '?' for help (alternative)
        siv.add_global_callback(cursive::event::Event::Char('?'), |s| {
            show_keyboard_shortcuts_hint(s);
        });

        // Add Ctrl+\ for key diagnostics (debug mode - not advertised)
        // Using Ctrl+\ (Ctrl+Backslash) as a hidden debug trigger since Ctrl+Shift isn't well supported
        siv.add_global_callback(cursive::event::Event::CtrlChar('\\'), |s| {
            super::key_diagnostics::show_key_diagnostics(s);
            log::info!("Key diagnostics panel opened. Press test keys to see events in logs.");
        });

        // Add Ctrl+Enter to send message (Microsoft Edit style)
        // Strategy: Since TextArea consumes Enter keys before global callbacks,
        // we intercept specific Ctrl+Enter mappings that ARE caught by global callbacks.
        // In most terminals, Ctrl+Enter maps to one of: CtrlChar('m'), CtrlChar('j'), or special sequences.
        // We handle each possible mapping.

        // Ctrl+M: Send message (Ctrl+Enter is transmitted as Ctrl+M in most terminals)
        // Our SendableTextArea wrapper now allows this to pass through
        siv.add_global_callback(cursive::event::Event::CtrlChar('m'), |s| {
            // Get content first (immutable access)
            let content = s
                .call_on_name("input", |view: &mut SendableTextArea| {
                    view.get_content().to_string()
                })
                .unwrap_or_default();

            if !content.trim().is_empty() {
                let state_opt = s.user_data::<AdvancedChatState>().map(|st| st.clone());
                if let Some(state) = state_opt {
                    handle_user_input(s, &content, state);
                    // Clear input after successful send (mutable access)
                    s.call_on_name("input", |view: &mut SendableTextArea| {
                        view.set_content("");
                    });
                }
            }
        });

        log::info!("✅ Ctrl+M (Ctrl+Enter) callback registered for sending messages");

        // Add Shift+Enter to send message (alternative shortcut for sending)
        // Note: Ctrl+E conflicts with cursive's default "end of line" binding
        siv.add_global_callback(
            cursive::event::Event::Shift(cursive::event::Key::Enter),
            |s| {
                log::info!("🔴 GLOBAL Shift+Enter handler triggered!");
                // Get content first (immutable access)
                let content = s
                    .call_on_name("input", |view: &mut SendableTextArea| {
                        view.get_content().to_string()
                    })
                    .unwrap_or_default();

                log::info!("🔴 Found input field with content: '{}'", content);
                if !content.trim().is_empty() {
                    log::info!("🔴 Content not empty, attempting to send...");
                    let state_opt = s.user_data::<AdvancedChatState>().map(|st| st.clone());
                    if let Some(state) = state_opt {
                        log::info!("🔴 Calling handle_user_input with content");
                        handle_user_input(s, &content, state);
                        // Clear input after successful send (mutable access)
                        s.call_on_name("input", |view: &mut SendableTextArea| {
                            view.set_content("");
                        });
                    } else {
                        log::error!("🔴 Failed to get state from user_data!");
                    }
                } else {
                    log::info!("🔴 Content is empty, not sending");
                }
            },
        );

        log::info!("✅ Shift+Enter callback registered for sending messages");

        // Add Ctrl+K to clear input (Microsoft Edit / VS Code style)
        siv.add_global_callback(cursive::event::Event::CtrlChar('k'), |s| {
            if let Some(mut input) = s.find_name::<SendableTextArea>("input") {
                input.set_content("");
            }
        });

        // Add Ctrl+F for session search (P1 UX improvement)
        siv.add_global_callback(cursive::event::Event::CtrlChar('f'), |s| {
            super::search::show_session_search(s);
        });

        // Add Ctrl+T for MCP tool search (Phase 2 improvement)
        siv.add_global_callback(cursive::event::Event::CtrlChar('t'), |s| {
            super::search::show_mcp_tool_search(s);
        });

        // Add Ctrl+R for input history search (P3 UX improvement, bash-style)
        siv.add_global_callback(cursive::event::Event::CtrlChar('r'), |s| {
            show_input_history_search(s);
        });

        // Add Alt+Up/Down for history navigation (avoid conflict with TextArea cursor movement)
        // BUG-2021 fix: Use user_data instead of Arc cloning
        siv.add_global_callback(cursive::event::Event::AltChar('p'), |s| {
            // Alt+P for Previous in history
            let state_opt = s.user_data::<AdvancedChatState>().map(|st| st.clone());
            if let Some(state) = state_opt {
                if let Some(prev_input) = state.history_previous() {
                    if let Some(mut input) = s.find_name::<SendableTextArea>("input") {
                        input.set_content(prev_input);
                    }
                    update_input_title(s, &state);
                }
            }
        });

        siv.add_global_callback(cursive::event::Event::AltChar('n'), |s| {
            // Alt+N for Next in history
            let state_opt = s.user_data::<AdvancedChatState>().map(|st| st.clone());
            if let Some(state) = state_opt {
                if let Some(next_input) = state.history_next() {
                    if let Some(mut input) = s.find_name::<SendableTextArea>("input") {
                        input.set_content(next_input);
                    }
                    update_input_title(s, &state);
                }
            }
        });

        // Add resize handling with graceful error handling
        siv.add_global_callback(cursive::event::Event::WindowResize, |s| {
            // Handle window resize gracefully - update displays safely
            // If an error occurs during UI update, log it and continue
            log::debug!("Window resize event triggered");

            // Try to update displays, but don't let errors crash the event handler
            // Most errors here are UI component lookup failures which are recoverable
            super::display::update_ui_displays(s);

            // Force screen refresh to prevent rendering artifacts
            // This operation is generally safe but handle errors gracefully if any occur
            s.clear();
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

        // Alt+T: Toggle MCP Tools (Progressive Disclosure)
        siv.add_global_callback(cursive::event::Event::AltChar('t'), |s| {
            if let Some(state) = s.user_data::<AdvancedChatState>() {
                // BUG-1010 fix: Clone state to release borrow, then handle race condition
                let state_clone = state.clone();

                // Handle race condition with proper error handling
                // and atomic operation (read-modify-write in one lock session)
                match state_clone.mcp_tools_visible.write() {
                    Ok(mut visible) => {
                        // Atomic toggle within lock
                        *visible = !*visible;
                    }
                    Err(e) => {
                        log::error!("Failed to toggle MCP tools visibility: {}", e);
                    }
                }

                // Update UI after lock is released and borrow is dropped
                super::display::update_ui_displays(s);
            }
        });

        // BUG-2021 fix: Use user_data instead of Arc cloning
        // Alt+R: Retry last message
        siv.add_global_callback(cursive::event::Event::AltChar('r'), |s| {
            // Get and clone state outside of mutable borrow
            let state_opt = s.user_data::<AdvancedChatState>().map(|st| st.clone());
            if let Some(state) = state_opt {
                retry_last_message(s, state);
            }
        });

        // Alt+C: Copy last message
        siv.add_global_callback(cursive::event::Event::AltChar('c'), |s| {
            // Get and clone state outside of mutable borrow
            let state_opt = s.user_data::<AdvancedChatState>().map(|st| st.clone());
            if let Some(state) = state_opt {
                copy_last_message(s, state);
            }
        });

        // Alt+D: Delete last message
        siv.add_global_callback(cursive::event::Event::AltChar('d'), |s| {
            // Get and clone state outside of mutable borrow
            let state_opt = s.user_data::<AdvancedChatState>().map(|st| st.clone());
            if let Some(state) = state_opt {
                delete_last_message(s, state);
            }
        });

        // Alt+F: Fork conversation
        siv.add_global_callback(cursive::event::Event::AltChar('f'), |s| {
            // Get and clone state outside of mutable borrow
            let state_opt = s.user_data::<AdvancedChatState>().map(|st| st.clone());
            if let Some(state) = state_opt {
                fork_conversation(s, state);
            }
        });
    }

    pub fn setup_suggestion_hotkeys(&self, siv: &mut Cursive) {
        // BUG-2021 fix: Use user_data instead of Arc cloning
        // Add Ctrl+number key handlers for suggestions - only when input has focus
        for i in 1..=5 {
            let key_char = char::from_digit(i as u32, 10).unwrap();

            siv.add_global_callback(cursive::event::Event::CtrlChar(key_char), move |s| {
                // Get and clone state outside of mutable borrow
                let state_opt = s.user_data::<AdvancedChatState>().map(|st| st.clone());

                if let Some(state) = state_opt {
                    // Only insert suggestion if suggestions are visible AND input has focus
                    let suggestions_visible = state
                        .suggestions_visible
                        .read()
                        .map(|v| *v)
                        .unwrap_or(false);

                    if suggestions_visible && s.find_name::<SendableTextArea>("input").is_some() {
                        insert_suggestion_at_cursor(s, (i - 1) as usize, state);
                    }
                }
            });
        }

        // Also add Alt+number for easier access
        for i in 1..=5 {
            siv.add_global_callback(
                cursive::event::Event::AltChar(char::from_digit(i as u32, 10).unwrap()),
                move |s| {
                    // Get and clone state outside of mutable borrow
                    let state_opt = s.user_data::<AdvancedChatState>().map(|st| st.clone());

                    if let Some(state) = state_opt {
                        // Only insert suggestion if suggestions are visible AND input has focus
                        let suggestions_visible = state
                            .suggestions_visible
                            .read()
                            .map(|v| *v)
                            .unwrap_or(false);

                        if suggestions_visible && s.find_name::<SendableTextArea>("input").is_some()
                        {
                            insert_suggestion_at_cursor(s, (i - 1) as usize, state);
                        }
                    }
                },
            );
        }

        // Hide suggestions on Escape - but don't interfere with other Escape usage
        siv.add_global_callback(cursive::event::Key::Esc, |s| {
            if let Some(state) = s.user_data::<AdvancedChatState>() {
                if let Ok(mut vis) = state.suggestions_visible.write() {
                    *vis = false;
                }
            }
        });

        // Emergency clear - Ctrl+Alt+X to clear stuck processing states (safe combination)
        siv.add_global_callback(cursive::event::Event::AltChar('x'), |siv| {
            // Get and clone state outside of mutable borrow
            let state_opt = siv.user_data::<AdvancedChatState>().map(|st| st.clone());

            if let Some(state) = state_opt {
                // Clear all processing messages from active session
                if let Some(session) = state.get_active_session() {
                    let session_id = session.id;
                    if let Err(e) = state.remove_last_processing_message(session_id) {
                        eprintln!("Emergency clear failed: {}", e);
                    }
                    super::display::update_ui_displays(siv);
                }
            }
        });
    }
}
