//! Individual UI components

use cursive::traits::*;
use cursive::views::{
    Button, Dialog, DummyView, EditView, LinearLayout, ListView, NamedView, Panel, ResizedView,
    ScrollView, SelectView, TextArea, TextView,
};
use cursive::{Cursive, CursiveExt, View};
use uuid::Uuid;

use super::super::state::AdvancedChatState;
use super::handlers::*;
use super::layout::AdvancedChatUI;

use super::theme::{Decorations, Icons, ProgressBar, StyledText};

impl AdvancedChatUI {
    pub fn create_chat_list_panel(&self) -> impl View {
        let mut chat_list_layout = LinearLayout::vertical();

        // Header with gradient effect and icon
        let header_text = format!("{} Chat Sessions {}", Icons::CHAT, Icons::SPARKLES);
        chat_list_layout.add_child(
            Panel::new(TextView::new(StyledText::gradient(&header_text)))
                .title(Decorations::section_divider("Sessions")),
        );

        // Chat list - use ListView instead for more reliable navigation
        let chat_list = ListView::new().with_name("chat_list");

        // We'll populate this manually in the update function

        // Add ListView directly inside a Panel - responsive height
        chat_list_layout.add_child(
            Panel::new(
                ScrollView::new(chat_list)
                    .scroll_strategy(cursive::view::scroll::ScrollStrategy::KeepRow),
            )
            .title("Sessions")
            .min_height(5)
            .max_height(20), // Allow more flexibility
        );

        // New chat button with icon
        chat_list_layout.add_child(DummyView.fixed_height(1));
        chat_list_layout.add_child(Button::new(format!("{} New Chat", Icons::NEW), |siv| {
            create_new_chat_dialog(siv);
        }));

        // Session controls with icons - responsive spacing
        chat_list_layout.add_child(DummyView.min_height(1));
        chat_list_layout.add_child(
            LinearLayout::horizontal()
                .child(Button::new(format!("{} Run", Icons::EXECUTING), |siv| {
                    resume_agent(siv)
                }))
                .child(DummyView.min_width(1))
                .child(Button::new(format!("{} Pause", Icons::PAUSED), |siv| {
                    pause_agent(siv)
                }))
                .child(DummyView.min_width(1))
                .child(Button::new(format!("{} Stop", Icons::STOP), |siv| {
                    stop_agent(siv)
                })),
        );

        // Recording controls with icons - responsive spacing
        chat_list_layout.add_child(DummyView.min_height(1));
        chat_list_layout.add_child(
            LinearLayout::horizontal()
                .child(Button::new(format!("{} Record", Icons::RECORD), |siv| {
                    start_recording(siv)
                }))
                .child(DummyView.min_width(1))
                .child(Button::new(format!("{} Stop Rec", Icons::STOP), |siv| {
                    stop_recording(siv)
                })),
        );

        chat_list_layout.add_child(DummyView.min_height(1));

        // MCP Tools Panel - responsive height with selectable tools
        let mcp_tools_view = SelectView::<(String, String)>::new()
            .on_submit(|siv, (server_id, tool_name)| {
                show_tool_details(siv, server_id.clone(), tool_name.clone());
            })
            .with_name("mcp_tools_list");
        let mcp_panel = Panel::new(ScrollView::new(mcp_tools_view))
            .title(format!("{} MCP Tools (Enter for details)", Icons::TOOL))
            .min_height(3)
            .max_height(15); // Allow more flexibility
        chat_list_layout.add_child(mcp_panel);

        chat_list_layout
    }

    pub fn create_chat_panel(&self) -> impl View {
        let mut chat_layout = LinearLayout::vertical();

        // Chat history area
        let chat_view = ScrollView::new(TextView::new("").with_name("chat_display").full_width())
            .scroll_strategy(cursive::view::scroll::ScrollStrategy::StickToBottom)
            .with_name("chat_scroll");

        let chat_panel = Panel::new(chat_view)
            .title(format!("{} Chat History", Icons::CHAT))
            .title_position(cursive::align::HAlign::Left);

        chat_layout.add_child(chat_panel.full_height());

        // Suggestions area (shown when available) - no frame, just colored text
        let suggestions_view = LinearLayout::vertical().with_name("suggestions_container");
        chat_layout.add_child(suggestions_view);

        // Input area - Standardized multi-line TextArea
        let state_for_send = self.state.clone();
        let input_layout = LinearLayout::vertical()
            .child(
                LinearLayout::horizontal()
                    .child(TextView::new("💬 "))
                    .child(
                        TextArea::new()
                            .content("")
                            .with_name("input")
                            .min_height(2)
                            .max_height(8)  // Auto-expand up to 8 lines
                            .full_width(),
                    )
            )
            .child(
                TextView::new("📝 Ctrl+Enter to send | Enter for new line | Tab to navigate")
                    .with_name("input_hint")
            );

        chat_layout.add_child(
            Panel::new(input_layout)
                .title("Message (Multi-line)")
                .with_name("input_panel")
        );

        // Agent status bar with live updates and icon
        let agent_status = TextView::new(format!("{} Agent: Initializing...", Icons::LIGHTNING))
            .with_name("agent_status");
        chat_layout
            .add_child(Panel::new(agent_status).title(format!("{} Agent Status", Icons::ROCKET)));

        // System status bar showing microVM/unikernel statuses and mounts with icon
        let system_status = TextView::new(format!("{} OSVM: Initializing...", Icons::STAR))
            .with_name("system_status_bar");
        chat_layout
            .add_child(Panel::new(system_status).title(format!("{} System Status", Icons::INFO)));

        // Control buttons with keyboard shortcuts hints
        let button_layout = LinearLayout::horizontal()
            .child(Button::new("Clear Chat", |siv| clear_current_chat(siv)))
            .child(Button::new("Export Chat", |siv| export_chat(siv)))
            .child(Button::new("Settings", |siv| show_settings(siv)))
            .child(Button::new("Help [F1/?]", show_advanced_help))
            .child(Button::new("Quit [Ctrl+Q]", |siv| siv.quit()));

        chat_layout.add_child(button_layout);

        // Add keyboard shortcuts hint footer
        let footer_hint = TextView::new(
            "💡 Quick: Tab=Nav | Alt+R/C/D/F=Actions | Ctrl+1-5=Suggest | F1=Help | F10=Menu | F12=📸"
        );
        chat_layout.add_child(
            Panel::new(footer_hint)
                .title("⌨️ Shortcuts")
        );

        chat_layout
    }

    /// Create top menu bar (Microsoft Edit style)
    pub fn create_top_menu_bar(&self) -> impl View {
        let mut menu_bar = LinearLayout::horizontal();

        // File menu
        menu_bar.add_child(Button::new_raw(" File ", |s| show_file_menu(s)));
        menu_bar.add_child(TextView::new(" "));

        // Edit menu
        menu_bar.add_child(Button::new_raw(" Edit ", |s| show_edit_menu(s)));
        menu_bar.add_child(TextView::new(" "));

        // Session menu
        menu_bar.add_child(Button::new_raw(" Session ", |s| show_session_menu(s)));
        menu_bar.add_child(TextView::new(" "));

        // Tools menu
        menu_bar.add_child(Button::new_raw(" Tools ", |s| show_tools_menu(s)));
        menu_bar.add_child(TextView::new(" "));

        // Help menu
        menu_bar.add_child(Button::new_raw(" Help ", |s| show_help_menu(s)));

        // Spacer to push status to right
        menu_bar.add_child(DummyView.full_width());

        // Agent status on right
        let status = if let Some(session) = self.state.get_active_session() {
            format!("[Agent: {}]", match session.agent_state {
                super::super::types::AgentState::Idle => "Idle",
                super::super::types::AgentState::Thinking => "Thinking",
                super::super::types::AgentState::Planning => "Planning",
                super::super::types::AgentState::ExecutingTool(_) => "Executing",
                super::super::types::AgentState::Waiting => "Waiting",
                super::super::types::AgentState::Paused => "Paused",
                super::super::types::AgentState::Error(_) => "Error",
            })
        } else {
            "[Agent: Idle]".to_string()
        };
        menu_bar.add_child(TextView::new(status).with_name("menu_status"));
        menu_bar.add_child(TextView::new("  F1=Help  "));

        Panel::new(menu_bar).title("").full_width()
    }

    /// Create unified sidebar with sessions and collapsible MCP tools
    pub fn create_unified_sidebar(&self) -> impl View {
        let mut sidebar = LinearLayout::vertical();

        // Sessions section header
        sidebar.add_child(
            Panel::new(TextView::new(format!("{} Sessions", Icons::CHAT)))
                .title("")
                .full_width(),
        );

        // Chat sessions list
        let chat_list = ListView::new().with_name("chat_list");
        sidebar.add_child(
            ScrollView::new(chat_list)
                .scroll_strategy(cursive::view::scroll::ScrollStrategy::KeepRow)
                .with_name("chat_list_scroll")
                .min_height(8)
                .max_height(15),
        );

        // New chat button
        sidebar.add_child(DummyView.fixed_height(1));
        sidebar.add_child(
            Button::new(format!("{} New Chat", Icons::NEW), |siv| {
                create_new_chat_dialog(siv);
            })
            .full_width(),
        );

        // Divider
        sidebar.add_child(DummyView.fixed_height(1));
        sidebar.add_child(TextView::new("─".repeat(25)));
        sidebar.add_child(DummyView.fixed_height(1));

        // MCP Tools - Progressive Disclosure (toggle with Alt+T)
        // Container will be dynamically updated based on visibility state
        let mcp_container = LinearLayout::vertical().with_name("mcp_tools_container");
        sidebar.add_child(mcp_container);

        // Agent controls at bottom
        sidebar.add_child(DummyView.fixed_height(1));
        sidebar.add_child(TextView::new("─".repeat(25)));
        sidebar.add_child(DummyView.fixed_height(1));

        // Compact control buttons
        sidebar.add_child(
            LinearLayout::horizontal()
                .child(Button::new(format!("{}", Icons::EXECUTING), |siv| resume_agent(siv)))
                .child(DummyView.min_width(1))
                .child(Button::new(format!("{}", Icons::PAUSED), |siv| pause_agent(siv)))
                .child(DummyView.min_width(1))
                .child(Button::new(format!("{}", Icons::STOP), |siv| stop_agent(siv)))
                .child(DummyView.min_width(1))
                .child(Button::new(format!("{}", Icons::RECORD), |siv| start_recording(siv))),
        );

        sidebar
    }

    /// Create collapsible MCP tools view
    pub fn create_collapsible_mcp_tools(&self) -> impl View {
        let mut tools_list = ListView::new();

        // Get available tools grouped by server
        if let Ok(available_tools) = self.state.available_tools.read() {
            for (server_id, tools) in available_tools.iter() {
                // Server header (clickable to collapse/expand)
                let server_display = format!("▼ {} ({} tools)", server_id, tools.len());
                let server_id_clone = server_id.clone();

                tools_list.add_child(
                    &format!("server_{}", server_id),
                    Button::new_raw(server_display, move |s| {
                        toggle_mcp_server_collapsed(s, server_id_clone.clone())
                    })
                    .full_width(),
                );

                // Tools under this server (visible when expanded)
                for tool in tools.iter().take(10) {
                    // Limit to first 10 tools
                    let tool_name = tool.name.clone();
                    let server_id_clone = server_id.clone();
                    let tool_display = format!("  · {}", tool.name);

                    tools_list.add_child(
                        &format!("tool_{}_{}", server_id, tool.name),
                        Button::new_raw(tool_display, move |s| {
                            show_tool_details(s, server_id_clone.clone(), tool_name.clone())
                        })
                        .full_width(),
                    );
                }

                if tools.len() > 10 {
                    tools_list.add_child(
                        &format!("more_{}", server_id),
                        TextView::new(format!("  ... and {} more", tools.len() - 10)),
                    );
                }
            }
        } else {
            tools_list.add_child("no_tools", TextView::new("No MCP tools available"));
        }

        ScrollView::new(tools_list.with_name("mcp_tools_list"))
            .scroll_strategy(cursive::view::scroll::ScrollStrategy::KeepRow)
    }

    /// Create simplified chat area (no duplicate buttons)
    pub fn create_simplified_chat_panel(&self) -> impl View {
        let mut chat_layout = LinearLayout::vertical();

        // Chat history area - takes most of the space
        let chat_view = ScrollView::new(TextView::new("").with_name("chat_display").full_width())
            .scroll_strategy(cursive::view::scroll::ScrollStrategy::StickToBottom)
            .with_name("chat_scroll");

        chat_layout.add_child(
            Panel::new(chat_view)
                .title(format!("{} Chat History", Icons::CHAT))
                .full_height(),
        );

        // Suggestions area (shown when available)
        let suggestions_view = LinearLayout::vertical().with_name("suggestions_container");
        chat_layout.add_child(suggestions_view);

        // Input area at bottom - Multi-line TextArea with Microsoft Edit style controls
        let state_clone_send = self.state.clone();
        let state_clone_enhance = self.state.clone();
        let state_clone_drafts = self.state.clone();

        let input_layout = LinearLayout::vertical()
            .child(
                // Multi-line input with helpful placeholder text
                LinearLayout::horizontal()
                    .child(TextView::new("💬 "))
                    .child(
                        TextArea::new()
                            .content("")
                            .with_name("input")
                            .min_height(3)
                            .max_height(10)  // Auto-expand up to 10 lines
                            .full_width(),
                    )
            )
            .child(
                LinearLayout::horizontal()
                    .child(Button::new("🤖 AI Enhance", move |s| {
                        enhance_message_with_ai(s, state_clone_enhance.clone());
                    }))
                    .child(DummyView.min_width(1))
                    .child(Button::new("📝 Drafts", move |s| {
                        show_drafts_dialog(s, state_clone_drafts.clone());
                    }))
                    .child(DummyView.fixed_width(2))
                    .child(Button::new("✅ SEND (Ctrl+Enter)", move |s| {
                        send_message_from_button(s, state_clone_send.clone());
                    }))
            )
            .child(
                TextView::new("💡 Ctrl+Enter=Send | Enter=Newline | Ctrl+K=Clear | ?=Help")
                    .with_name("input_hint")
            );

        chat_layout.add_child(
            Panel::new(input_layout)
                .title("✍️  Input Editor (Multi-line)")
                .with_name("input_panel"),
        );

        // UNIFIED status bar combining agent state, system info, and shortcuts
        // Format: [Agent Status] | [System Status] | [Shortcuts]
        let status_text = format!(
            "{} Agent: Idle  {}  {} OSVM: Initializing...  {}  {} F1/F2/F3=Help | F10=Menu | F12=Screenshot",
            Icons::LIGHTNING,
            Icons::SEPARATOR,
            Icons::STAR,
            Icons::SEPARATOR,
            Icons::KEYBOARD
        );
        let status_bar = TextView::new(status_text)
            .with_name("combined_status_bar")
            .full_width();

        chat_layout.add_child(Panel::new(status_bar).title(""));

        chat_layout
    }
}
