//! Individual UI components

use cursive::traits::*;
use cursive::views::{
    Button, Dialog, DummyView, EditView, LinearLayout, ListView, NamedView, Panel, ResizedView,
    ScrollView, SelectView, TextView,
};
use cursive::{Cursive, CursiveExt, View};
use uuid::Uuid;

use super::super::state::AdvancedChatState;
use super::handlers::*;
use super::layout::AdvancedChatUI;

impl AdvancedChatUI {
    pub fn create_chat_list_panel(&self) -> impl View {
        let mut chat_list_layout = LinearLayout::vertical();

        // Header
        chat_list_layout.add_child(Panel::new(TextView::new("Chat Sessions")).title("Sessions"));

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

        // New chat button
        chat_list_layout.add_child(DummyView.fixed_height(1));
        chat_list_layout.add_child(Button::new("+ New Chat", |siv| {
            create_new_chat_dialog(siv);
        }));

        // Session controls - responsive spacing
        chat_list_layout.add_child(DummyView.min_height(1));
        chat_list_layout.add_child(
            LinearLayout::horizontal()
                .child(Button::new("Run", |siv| resume_agent(siv)))
                .child(DummyView.min_width(1))
                .child(Button::new("Pause", |siv| pause_agent(siv)))
                .child(DummyView.min_width(1))
                .child(Button::new("Stop", |siv| stop_agent(siv))),
        );

        // Recording controls - responsive spacing
        chat_list_layout.add_child(DummyView.min_height(1));
        chat_list_layout.add_child(
            LinearLayout::horizontal()
                .child(Button::new("Record", |siv| start_recording(siv)))
                .child(DummyView.min_width(1))
                .child(Button::new("Stop Rec", |siv| stop_recording(siv))),
        );

        chat_list_layout.add_child(DummyView.min_height(1));

        // MCP Tools Panel - responsive height with selectable tools
        let mcp_tools_view = SelectView::<(String, String)>::new()
            .on_submit(|siv, (server_id, tool_name)| {
                show_tool_details(siv, server_id.clone(), tool_name.clone());
            })
            .with_name("mcp_tools_list");
        let mcp_panel = Panel::new(ScrollView::new(mcp_tools_view))
            .title("MCP Tools (Enter for details)")
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
            .title("Chat History")
            .title_position(cursive::align::HAlign::Left);

        chat_layout.add_child(chat_panel.full_height());

        // Suggestions area (shown when available) - no frame, just colored text
        let suggestions_view = LinearLayout::vertical().with_name("suggestions_container");
        chat_layout.add_child(suggestions_view);

        // Input area
        let input_layout = LinearLayout::horizontal()
            .child(TextView::new("You: "))
            .child(
                EditView::new()
                    .on_submit({
                        let state = self.state.clone();
                        move |siv, text| {
                            handle_user_input(siv, text, state.clone());
                        }
                    })
                    .with_name("input")
                    .full_width(),
            );

        chat_layout.add_child(Panel::new(input_layout).title("Input"));

        // Agent status bar with live updates
        let agent_status = TextView::new("🤖 Agent: Initializing...").with_name("agent_status");
        chat_layout.add_child(Panel::new(agent_status).title("🤖 Agent Status"));

        // System status bar showing microVM/unikernel statuses and mounts
        let system_status = TextView::new("OSVM: Initializing...").with_name("system_status_bar");
        chat_layout.add_child(Panel::new(system_status).title("⚙️ System Status"));

        // Control buttons
        let button_layout = LinearLayout::horizontal()
            .child(Button::new("Clear Chat", |siv| clear_current_chat(siv)))
            .child(Button::new("Export Chat", |siv| export_chat(siv)))
            .child(Button::new("Settings", |siv| show_settings(siv)))
            .child(Button::new("Help", show_advanced_help))
            .child(Button::new("Quit", |siv| siv.quit()));

        chat_layout.add_child(button_layout);

        chat_layout
    }
}
