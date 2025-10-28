//! Search and filtering functionality for UI components
//!
//! Provides search capabilities for MCP tools, sessions, and commands

use cursive::align::HAlign;
use cursive::traits::*;
use cursive::views::{
    Button, Dialog, DummyView, EditView, LinearLayout, ListView, Panel, ScrollView, SelectView,
    TextView,
};
use cursive::{Cursive, View};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use super::super::state::AdvancedChatState;
use super::handlers::show_tool_details;
use crate::services::mcp_service::McpTool;

/// MCP Tool search state
pub struct ToolSearchState {
    /// All available tools (server_id -> tools)
    all_tools: HashMap<String, Vec<McpTool>>,
    /// Currently filtered tools
    filtered_tools: Vec<(String, McpTool)>,
    /// Search query
    search_query: String,
    /// Favorite tools (tool_name -> server_id)
    favorites: Vec<(String, String)>,
    /// Recently used tools
    recent_tools: Vec<(String, String)>,
}

impl ToolSearchState {
    pub fn new() -> Self {
        ToolSearchState {
            all_tools: HashMap::new(),
            filtered_tools: Vec::new(),
            search_query: String::new(),
            favorites: Vec::new(),
            recent_tools: Vec::new(),
        }
    }

    /// Update the search filter
    pub fn update_filter(&mut self, query: &str) {
        self.search_query = query.to_lowercase();
        self.filtered_tools.clear();

        if self.search_query.is_empty() {
            // Show favorites and recent when no search
            for (tool_name, server_id) in &self.favorites {
                if let Some(tools) = self.all_tools.get(server_id) {
                    if let Some(tool) = tools.iter().find(|t| &t.name == tool_name) {
                        self.filtered_tools.push((server_id.clone(), tool.clone()));
                    }
                }
            }

            for (tool_name, server_id) in &self.recent_tools {
                if !self
                    .favorites
                    .iter()
                    .any(|(t, s)| t == tool_name && s == server_id)
                {
                    if let Some(tools) = self.all_tools.get(server_id) {
                        if let Some(tool) = tools.iter().find(|t| &t.name == tool_name) {
                            self.filtered_tools.push((server_id.clone(), tool.clone()));
                        }
                    }
                }
            }
        } else {
            // Filter tools by search query
            for (server_id, tools) in &self.all_tools {
                for tool in tools {
                    let matches = tool.name.to_lowercase().contains(&self.search_query)
                        || tool
                            .description
                            .as_ref()
                            .map(|d| d.to_lowercase().contains(&self.search_query))
                            .unwrap_or(false);

                    if matches {
                        self.filtered_tools.push((server_id.clone(), tool.clone()));
                    }
                }
            }
        }
    }

    /// Add tool to favorites
    pub fn add_favorite(&mut self, tool_name: String, server_id: String) {
        if !self
            .favorites
            .iter()
            .any(|(t, s)| t == &tool_name && s == &server_id)
        {
            self.favorites.push((tool_name, server_id));
            // Keep only last 10 favorites
            if self.favorites.len() > 10 {
                self.favorites.remove(0);
            }
        }
    }

    /// Add tool to recent
    pub fn add_recent(&mut self, tool_name: String, server_id: String) {
        // Remove if already in recent
        self.recent_tools
            .retain(|(t, s)| !(t == &tool_name && s == &server_id));
        // Add to front
        self.recent_tools.insert(0, (tool_name, server_id));
        // Keep only last 5 recent
        if self.recent_tools.len() > 5 {
            self.recent_tools.pop();
        }
    }
}

/// Show MCP tool search dialog
pub fn show_mcp_tool_search(s: &mut Cursive) {
    let state = match s.user_data::<AdvancedChatState>() {
        Some(state) => state.clone(),
        None => {
            log::error!("AdvancedChatState not found in user data");
            return;
        }
    };

    // Create search state
    let search_state = Arc::new(RwLock::new(ToolSearchState::new()));

    // Load all tools
    if let Ok(tools) = state.available_tools.read() {
        if let Ok(mut search) = search_state.write() {
            search.all_tools = tools.clone();
            search.update_filter(""); // Show favorites/recent initially
        }
    }

    let mut layout = LinearLayout::vertical();

    // Search input
    layout.add_child(TextView::new("Search MCP Tools:"));

    let search_state_clone = search_state.clone();
    let search_input = EditView::new()
        .on_edit(move |s, text, _cursor| {
            // Update filter as user types
            if let Ok(mut search) = search_state_clone.write() {
                search.update_filter(text);
            }
            // Update the results view with new filtered results
            rebuild_search_results_view(s, &search_state_clone);
        })
        .with_name("tool_search_input")
        .full_width();

    layout.add_child(search_input);
    layout.add_child(DummyView);

    // Results area
    let results_view = create_search_results_view(&search_state);
    layout.add_child(
        Panel::new(
            ScrollView::new(results_view.with_name("tool_search_results"))
                .scroll_strategy(cursive::view::scroll::ScrollStrategy::StickToTop),
        )
        .title("Results")
        .full_height(),
    );

    // Help text
    layout.add_child(DummyView);
    layout.add_child(TextView::new(
        "↑↓: Navigate | Enter: Select | *: Favorite | Esc: Cancel",
    ));

    // Create dialog
    let dialog = Dialog::around(layout.min_width(60).min_height(20))
        .title("MCP Tool Search")
        .dismiss_button("Cancel");

    // Store search state in user data for access in callbacks
    s.set_user_data(search_state.clone());

    // Store search state in dialog
    s.add_layer(dialog.with_name("tool_search_dialog"));

    // Focus search input
    s.focus_name("tool_search_input").ok();
}

/// Create the search results view
fn create_search_results_view(search_state: &Arc<RwLock<ToolSearchState>>) -> impl View {
    let mut select_view = SelectView::new();

    if let Ok(search) = search_state.read() {
        if search.filtered_tools.is_empty() && !search.search_query.is_empty() {
            select_view.add_item("No tools found", (String::new(), String::new()));
        } else if search.filtered_tools.is_empty() {
            // Show categories when no search
            select_view.add_item("── Favorites ──", (String::new(), String::new()));
            if search.favorites.is_empty() {
                select_view.add_item("  (No favorites yet)", (String::new(), String::new()));
            }

            select_view.add_item("── Recent ──", (String::new(), String::new()));
            if search.recent_tools.is_empty() {
                select_view.add_item("  (No recent tools)", (String::new(), String::new()));
            }

            select_view.add_item("── All Tools ──", (String::new(), String::new()));
            select_view.add_item("  (Start typing to search)", (String::new(), String::new()));
        } else {
            // Group results by category
            let mut favorites_shown = false;
            let mut recent_shown = false;
            let mut others_shown = false;

            for (server_id, tool) in &search.filtered_tools {
                let is_favorite = search
                    .favorites
                    .iter()
                    .any(|(t, s)| t == &tool.name && s == server_id);
                let is_recent = search
                    .recent_tools
                    .iter()
                    .any(|(t, s)| t == &tool.name && s == server_id);

                let prefix = if is_favorite {
                    if !favorites_shown {
                        select_view.add_item("── ★ Favorites ──", (String::new(), String::new()));
                        favorites_shown = true;
                    }
                    "  ★ "
                } else if is_recent && !search.search_query.is_empty() {
                    if !recent_shown {
                        select_view.add_item("── ⏱ Recent ──", (String::new(), String::new()));
                        recent_shown = true;
                    }
                    "  ⏱ "
                } else {
                    if !others_shown {
                        select_view
                            .add_item("── Search Results ──", (String::new(), String::new()));
                        others_shown = true;
                    }
                    "  "
                };

                let display = format!(
                    "{}{} ({})",
                    prefix,
                    tool.name,
                    server_id.chars().take(15).collect::<String>()
                );

                select_view.add_item(display, (server_id.clone(), tool.name.clone()));
            }
        }
    }

    // Handle selection
    select_view.set_on_submit(|s, (server_id, tool_name): &(String, String)| {
        if !server_id.is_empty() && !tool_name.is_empty() {
            // Add to recent using generic user_data
            // Note: We store Arc<RwLock<ToolSearchState>> in user_data during dialog creation
            if let Some(search_state) = s.user_data::<Arc<RwLock<ToolSearchState>>>() {
                if let Ok(mut search) = search_state.write() {
                    search.add_recent(tool_name.clone(), server_id.clone());
                }
            }

            // Close search dialog
            s.pop_layer();

            // Show tool details
            show_tool_details(s, server_id.clone(), tool_name.clone());
        }
    });

    select_view
}

/// Rebuild search results view after filter update
fn rebuild_search_results_view(s: &mut Cursive, search_state: &Arc<RwLock<ToolSearchState>>) {
    // Create new filtered results view
    let new_results = create_search_results_view(search_state);

    // Replace the old results view with the new one
    if let Some(mut old_results) =
        s.find_name::<SelectView<(String, String)>>("tool_search_results")
    {
        // Get current selection if any
        let current_selection = old_results.selected_id();

        // Replace with new view
        // Note: Cursive doesn't support direct view replacement, so we update the SelectView items
        old_results.clear();

        // Rebuild items from search state
        if let Ok(search) = search_state.read() {
            if search.filtered_tools.is_empty() && !search.search_query.is_empty() {
                old_results.add_item("No tools found", (String::new(), String::new()));
            } else if search.filtered_tools.is_empty() {
                old_results.add_item("── Favorites ──", (String::new(), String::new()));
                old_results.add_item("── Recent ──", (String::new(), String::new()));
                old_results.add_item("── All Tools ──", (String::new(), String::new()));
            } else {
                let mut favorites_shown = false;
                let mut recent_shown = false;
                let mut others_shown = false;

                for (server_id, tool) in &search.filtered_tools {
                    let is_favorite = search
                        .favorites
                        .iter()
                        .any(|(t, s)| t == &tool.name && s == server_id);
                    let is_recent = search
                        .recent_tools
                        .iter()
                        .any(|(t, s)| t == &tool.name && s == server_id);

                    if is_favorite && !favorites_shown {
                        old_results.add_item("── ★ Favorites ──", (String::new(), String::new()));
                        favorites_shown = true;
                    }
                    if is_recent && !recent_shown && !search.search_query.is_empty() {
                        old_results.add_item("── ⏱ Recent ──", (String::new(), String::new()));
                        recent_shown = true;
                    }
                    if !is_favorite && !is_recent && !others_shown {
                        old_results
                            .add_item("── Search Results ──", (String::new(), String::new()));
                        others_shown = true;
                    }

                    let prefix = if is_favorite {
                        "  ★ "
                    } else if is_recent {
                        "  ⏱ "
                    } else {
                        "  "
                    };

                    let display = format!(
                        "{}{} ({})",
                        prefix,
                        tool.name,
                        server_id.chars().take(15).collect::<String>()
                    );
                    old_results.add_item(display, (server_id.clone(), tool.name.clone()));
                }
            }
        }
    }
}

/// Show session search dialog
pub fn show_session_search(s: &mut Cursive) {
    let state = match s.user_data::<AdvancedChatState>() {
        Some(state) => state.clone(),
        None => {
            log::error!("AdvancedChatState not found in user data");
            return;
        }
    };

    let mut layout = LinearLayout::vertical();

    // Search input
    layout.add_child(TextView::new("Search Sessions:"));

    let search_input = EditView::new()
        .on_edit(move |s, text, _cursor| {
            // Update session list based on search
            update_session_search_results(s, text);
        })
        .with_name("session_search_input")
        .full_width();

    layout.add_child(search_input);
    layout.add_child(DummyView);

    // Results
    let mut session_list = SelectView::new();

    // Populate with all sessions initially
    let sessions = state.get_session_names();
    for (id, name, _agent_state) in sessions {
        let message_count = if let Some(session) = state.get_session_by_id(id) {
            session.messages.len()
        } else {
            0
        };

        let display = format!("{} ({} messages)", name, message_count);
        session_list.add_item(display, id);
    }

    session_list.set_on_submit(|s, session_id: &uuid::Uuid| {
        // Switch to selected session
        if let Some(state) = s.user_data::<AdvancedChatState>() {
            let _ = state.set_active_session(*session_id);
        }
        s.pop_layer();
        super::display::update_ui_displays(s);
    });

    layout.add_child(
        Panel::new(
            ScrollView::new(session_list.with_name("session_search_results"))
                .scroll_strategy(cursive::view::scroll::ScrollStrategy::StickToTop),
        )
        .title("Sessions")
        .full_height(),
    );

    // Create dialog
    let dialog = Dialog::around(layout.min_width(50).min_height(15))
        .title("Session Search (Ctrl+F)")
        .dismiss_button("Cancel");

    s.add_layer(dialog);
    s.focus_name("session_search_input").ok();
}

/// Update session search results
fn update_session_search_results(s: &mut Cursive, query: &str) {
    let state = s
        .user_data::<AdvancedChatState>()
        .cloned()
        .expect("AdvancedChatState should be set");

    if let Some(mut results) = s.find_name::<SelectView<uuid::Uuid>>("session_search_results") {
        results.clear();

        let query_lower = query.to_lowercase();
        let sessions = state.get_session_names();

        for (id, name, _agent_state) in sessions {
            if name.to_lowercase().contains(&query_lower) {
                let message_count = if let Some(session) = state.get_session_by_id(id) {
                    session.messages.len()
                } else {
                    0
                };

                let display = format!("{} ({} messages)", name, message_count);
                results.add_item(display, id);
            }
        }

        if results.is_empty() {
            results.add_item("No sessions found", uuid::Uuid::nil());
        }
    }
}
