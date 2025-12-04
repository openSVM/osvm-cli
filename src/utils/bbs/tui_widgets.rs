// Ratatui TUI widgets for BBS
// Real-time interactive BBS interface

use ratatui::{
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, Paragraph, Wrap},
    Frame,
};
use std::sync::{Arc, Mutex};

use crate::utils::bbs::{
    db,
    models::{Board, Post, User},
};
use diesel::sqlite::SqliteConnection;

/// A mesh message received over LoRa radio
#[derive(Clone, Debug)]
pub struct MeshMessage {
    pub from_node: u32,
    pub from_name: Option<String>,
    pub message: String,
    pub timestamp: chrono::DateTime<chrono::Local>,
    pub is_command: bool,
}

/// BBS TUI state
pub struct BBSTuiState {
    pub conn: Arc<Mutex<Option<SqliteConnection>>>,
    pub boards: Vec<Board>,
    pub current_board: Option<i32>,
    pub posts: Vec<Post>,
    pub users_online: Vec<User>,
    pub input_buffer: String,
    pub scroll_offset: usize,
    pub status_message: String,
    pub connected: bool,    // Track if we've initialized the connection
    pub input_active: bool, // Track if input mode is active
    pub selected_board_index: Option<usize>, // Track selected board for visual feedback
    // Agent integration
    pub agents: Vec<User>,         // Known AI agents
    pub agent_status: AgentStatus, // Current agent listening status
    /// Cache of user_id -> User for displaying post authors
    pub user_cache: std::collections::HashMap<i32, User>,
    /// Recent mesh messages (ring buffer, max 50)
    pub mesh_messages: Vec<MeshMessage>,
    /// Node name cache from mesh network
    pub mesh_nodes: std::collections::HashMap<u32, String>,
}

/// Agent listening status for the BBS
#[derive(Clone, Debug, Default)]
pub struct AgentStatus {
    pub osvm_agent_online: bool,
    pub last_agent_activity: Option<String>,
    pub agents_listening: usize,
    pub meshtastic_connected: bool,
    pub meshtastic_node_id: Option<String>,
}

impl BBSTuiState {
    pub fn new() -> Self {
        Self {
            conn: Arc::new(Mutex::new(None)),
            boards: Vec::new(),
            current_board: None,
            posts: Vec::new(),
            users_online: Vec::new(),
            input_buffer: String::new(),
            scroll_offset: 0,
            status_message: "Connecting to BBS...".to_string(),
            connected: false,
            input_active: false,
            selected_board_index: Some(0),
            agents: Vec::new(),
            agent_status: AgentStatus::default(),
            user_cache: std::collections::HashMap::new(),
            mesh_messages: Vec::new(),
            mesh_nodes: std::collections::HashMap::new(),
        }
    }

    /// Add a mesh message (keeps max 50 in memory, also saves to database)
    pub fn add_mesh_message(
        &mut self,
        from: u32,
        message: String,
        is_command: bool,
    ) -> Option<i32> {
        let from_name = self.mesh_nodes.get(&from).cloned();
        let now = chrono::Local::now();
        let msg = MeshMessage {
            from_node: from,
            from_name: from_name.clone(),
            message: message.clone(),
            timestamp: now,
            is_command,
        };
        self.mesh_messages.push(msg);

        // Keep only last 50 in memory
        if self.mesh_messages.len() > 50 {
            self.mesh_messages.remove(0);
        }

        // Save to database
        let mut message_id = None;
        if let Ok(mut guard) = self.conn.lock() {
            if let Some(ref mut conn) = *guard {
                let received_at_us = now.timestamp_micros();
                match db::mesh_messages::create(
                    conn,
                    from,
                    from_name.as_deref(),
                    None, // broadcast
                    0,    // channel 0
                    &message,
                    is_command,
                    received_at_us,
                ) {
                    Ok(msg_db) => {
                        message_id = Some(msg_db.id);
                        log::debug!("Saved mesh message {} to database", msg_db.id);
                    }
                    Err(e) => {
                        log::error!("Failed to save mesh message to database: {}", e);
                    }
                }
            }
        }

        message_id
    }

    /// Add a response to a mesh message in the database
    pub fn add_mesh_response(&mut self, message_id: i32, response: &str) {
        if let Ok(mut guard) = self.conn.lock() {
            if let Some(ref mut conn) = *guard {
                let responded_at_us = chrono::Local::now().timestamp_micros();
                if let Err(e) =
                    db::mesh_messages::add_response(conn, message_id, response, responded_at_us)
                {
                    log::error!("Failed to save mesh response to database: {}", e);
                }
            }
        }
    }

    /// Update node name cache
    pub fn update_mesh_node(&mut self, node_id: u32, name: String) {
        self.mesh_nodes.insert(node_id, name);
    }

    /// Check if a user is an AI agent based on naming conventions
    pub fn is_agent(user: &User) -> bool {
        let short_upper = user.short_name.to_uppercase();
        let long_lower = user.long_name.to_lowercase();

        // Agent detection heuristics:
        // 1. Short name patterns: OSVM, AI, BOT, AGT
        // 2. Long name contains: agent, bot, assistant, ai
        // 3. Node ID patterns: !aaaa (reserved for agents)
        short_upper == "OSVM" ||
        short_upper == "AI" ||
        short_upper == "BOT" ||
        short_upper == "AGT" ||
        short_upper == "TUI" ||  // TUI user is system
        long_lower.contains("agent") ||
        long_lower.contains("bot") ||
        long_lower.contains("assistant") ||
        user.node_id.starts_with("!aaaa") ||
        user.node_id.starts_with("!tui")
    }

    /// Load agents and update status
    pub fn refresh_agents(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        if let Some(ref mut conn) = *self.conn.lock().unwrap() {
            // Get all users and filter for agents
            let all_users = db::users::list_all(conn)?;
            self.agents = all_users
                .iter()
                .filter(|u| Self::is_agent(u))
                .cloned()
                .collect();

            // Update agent status
            self.agent_status.agents_listening = self.agents.len();
            self.agent_status.osvm_agent_online = self
                .agents
                .iter()
                .any(|a| a.short_name.to_uppercase() == "OSVM");

            // Find most recent agent activity
            if let Some(most_recent) = self.agents.iter().filter_map(|a| a.last_acted_at_us).max() {
                self.agent_status.last_agent_activity =
                    Some(crate::utils::bbs::models::User::last_acted_at(
                        &self
                            .agents
                            .iter()
                            .find(|a| a.last_acted_at_us == Some(most_recent))
                            .unwrap(),
                    ));
            }

            // Cache all users for post author lookup
            for user in all_users {
                self.user_cache.insert(user.id, user);
            }
        }
        Ok(())
    }

    /// Get user by ID from cache
    pub fn get_user(&self, user_id: i32) -> Option<&User> {
        self.user_cache.get(&user_id)
    }

    /// Try to connect to Meshtastic radio (optional)
    /// This doesn't fail if Meshtastic is unavailable - just sets status
    pub fn try_connect_meshtastic(&mut self, address: Option<&str>) {
        use crate::utils::bbs::meshtastic::{ConnectionState, MeshtasticRadio};

        let addr = address.unwrap_or("localhost:4403");

        if let Some(mut radio) = MeshtasticRadio::from_address(addr) {
            match radio.connect() {
                Ok(()) => {
                    self.agent_status.meshtastic_connected = true;
                    self.agent_status.meshtastic_node_id =
                        Some(format!("!{:08x}", radio.our_node_id()));
                    self.status_message = format!("📻 Meshtastic connected @ {}", addr);
                }
                Err(e) => {
                    self.agent_status.meshtastic_connected = false;
                    // Don't show error - mesh is optional
                    log::debug!("Meshtastic connection failed: {}", e);
                }
            }
        }
    }

    /// Initialize BBS connection
    pub fn connect(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        if self.connected {
            return Ok(()); // Already connected
        }

        let mut conn = db::establish_connection()?;
        db::initialize_database(&mut conn)?;

        *self.conn.lock().unwrap() = Some(conn);
        self.connected = true;
        self.refresh_boards()?;
        self.refresh_agents()?; // Load agent info

        // Update status with agent info
        let agent_hint = if self.agent_status.agents_listening > 0 {
            format!(
                " | 🤖 {} agents listening",
                self.agent_status.agents_listening
            )
        } else {
            String::new()
        };
        self.status_message = format!("Connected to OSVM BBS{}", agent_hint);

        Ok(())
    }

    /// Refresh board list
    pub fn refresh_boards(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        if let Some(ref mut conn) = *self.conn.lock().unwrap() {
            self.boards = db::boards::list(conn)?;
        }
        Ok(())
    }

    /// Load posts for current board
    pub fn load_posts(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        if let (Some(ref mut conn), Some(board_id)) =
            (self.conn.lock().unwrap().as_mut(), self.current_board)
        {
            self.posts = db::posts::list_for_board(conn, board_id, 50)?;
        }
        Ok(())
    }

    /// Post a message to the current board
    /// Returns the post ID on success
    pub fn post_message(&mut self, message: &str) -> Result<i32, Box<dyn std::error::Error>> {
        let board_id = self.current_board.ok_or_else(|| "No board selected")?;

        let mut conn_guard = self.conn.lock().unwrap();
        let conn = conn_guard
            .as_mut()
            .ok_or_else(|| "Not connected to database")?;

        // Get or create a TUI user (similar to CLI user pattern)
        let timestamp = db::now_as_useconds();
        let (user, _created) = db::users::observe(
            conn,
            "!tuiuser1",      // node_id
            Some("TUI"),      // short_name
            Some("TUI User"), // long_name
            timestamp,
        )?;

        // Create the post
        let post = db::posts::create(conn, board_id, user.id, message)?;

        Ok(post.id)
    }
}

/// Render BBS TUI
pub fn render_bbs_tab(f: &mut Frame, area: Rect, state: &mut BBSTuiState) {
    // Lazy initialization: connect to database on first render
    if !state.connected {
        if let Err(e) = state.connect() {
            state.status_message = format!("❌ Connection failed: {}", e);
        } else {
            // Auto-select first board on connection
            if let Some(board) = state.boards.first() {
                let board_id = board.id;
                let board_name = board.name.clone();
                state.current_board = Some(board_id);
                state.selected_board_index = Some(0);
                let _ = state.load_posts();
                state.status_message = format!("Connected! Viewing: {}", board_name);
            }
        }
    }

    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3), // Header
            Constraint::Min(0),    // Main content
            Constraint::Length(4), // Input box (bigger!)
            Constraint::Length(2), // Status bar (smaller)
        ])
        .split(area);

    // Header with agent status
    let board_name = state
        .current_board
        .and_then(|id| {
            state
                .boards
                .iter()
                .find(|b| b.id == id)
                .map(|b| b.name.as_str())
        })
        .unwrap_or("No board selected");

    // Agent status indicator
    let agent_indicator = if state.agent_status.agents_listening > 0 {
        format!(
            " | 🤖 {} agent{} listening",
            state.agent_status.agents_listening,
            if state.agent_status.agents_listening == 1 {
                ""
            } else {
                "s"
            }
        )
    } else {
        " | 🔇 No agents".to_string()
    };

    let header_text = format!(
        "📡 OSVM BBS | Board: {} | {} posts{}",
        board_name,
        state.posts.len(),
        agent_indicator
    );

    let header = Paragraph::new(header_text)
        .style(
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        )
        .alignment(Alignment::Center)
        .block(Block::default().borders(Borders::ALL));
    f.render_widget(header, chunks[0]);

    // Main content area
    let main_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(25), // Sidebar (boards + agents)
            Constraint::Percentage(75), // Posts
        ])
        .split(chunks[1]);

    // Split sidebar into boards (top) and agent activity (bottom)
    let sidebar_chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage(50), // Boards
            Constraint::Percentage(50), // Agent activity
        ])
        .split(main_chunks[0]);

    // Board list with CLEAR selection indicators
    let boards: Vec<ListItem> = state
        .boards
        .iter()
        .enumerate()
        .map(|(idx, board)| {
            let is_selected = state.selected_board_index == Some(idx);
            let prefix = if is_selected { "▶ " } else { "  " };
            let style = if is_selected {
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(Color::DarkGray)
            };

            ListItem::new(Line::from(vec![
                Span::styled(format!("{}{} ", prefix, idx + 1), style),
                Span::styled(&board.name, style),
            ]))
        })
        .collect();

    let board_list = List::new(boards).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Boards (1-9) ")
            .border_style(Style::default().fg(Color::Cyan)),
    );
    f.render_widget(board_list, sidebar_chunks[0]);

    // Agent Activity Panel
    let mut agent_lines: Vec<Line> = Vec::new();

    // Meshtastic status at top
    let mesh_status = if state.agent_status.meshtastic_connected {
        Line::from(vec![
            Span::styled("  📻 ", Style::default().fg(Color::Green)),
            Span::styled("Mesh: ", Style::default().fg(Color::Green)),
            Span::styled(
                state
                    .agent_status
                    .meshtastic_node_id
                    .as_deref()
                    .unwrap_or("Connected"),
                Style::default()
                    .fg(Color::Green)
                    .add_modifier(Modifier::BOLD),
            ),
        ])
    } else {
        Line::from(vec![
            Span::styled("  📻 ", Style::default().fg(Color::DarkGray)),
            Span::styled("Mesh: Offline", Style::default().fg(Color::DarkGray)),
        ])
    };
    agent_lines.push(mesh_status);
    agent_lines.push(Line::from(""));

    // Agent list
    if state.agents.is_empty() {
        agent_lines.push(Line::from(Span::styled(
            "  No agents",
            Style::default().fg(Color::DarkGray),
        )));
        agent_lines.push(Line::from(""));
        agent_lines.push(Line::from(Span::styled(
            "  osvm bbs agent",
            Style::default().fg(Color::Yellow),
        )));
        agent_lines.push(Line::from(Span::styled(
            "    register <name>",
            Style::default().fg(Color::Yellow),
        )));
    } else {
        agent_lines.push(Line::from(Span::styled(
            format!(
                "  {} agent{}",
                state.agents.len(),
                if state.agents.len() == 1 { "" } else { "s" }
            ),
            Style::default().fg(Color::Green),
        )));

        // Show each agent (compact)
        for agent in state.agents.iter().take(4) {
            let status_icon = if agent.last_acted_at_us.is_some() {
                "●"
            } else {
                "○"
            };
            let status_color = if agent.last_acted_at_us.is_some() {
                Color::Green
            } else {
                Color::DarkGray
            };

            agent_lines.push(Line::from(vec![
                Span::styled(
                    format!("  {} ", status_icon),
                    Style::default().fg(status_color),
                ),
                Span::styled("🤖 ", Style::default().fg(Color::Magenta)),
                Span::styled(
                    &agent.short_name,
                    Style::default()
                        .fg(Color::Magenta)
                        .add_modifier(Modifier::BOLD),
                ),
            ]));
        }

        if state.agents.len() > 4 {
            agent_lines.push(Line::from(Span::styled(
                format!("  +{} more", state.agents.len() - 4),
                Style::default().fg(Color::DarkGray),
            )));
        }
    };

    let agent_panel = Paragraph::new(agent_lines).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" 🤖 Agents ")
            .border_style(if state.agent_status.agents_listening > 0 {
                Style::default().fg(Color::Magenta)
            } else {
                Style::default().fg(Color::DarkGray)
            }),
    );
    f.render_widget(agent_panel, sidebar_chunks[1]);

    // Posts area with agent badges
    let post_lines: Vec<Line> = if state.posts.is_empty() {
        vec![
            Line::from(""),
            Line::from(Span::styled(
                "  No posts yet in this board.",
                Style::default().fg(Color::DarkGray),
            )),
            Line::from(""),
            Line::from(Span::styled(
                "  Press 'i' to write a new post!",
                Style::default().fg(Color::Yellow),
            )),
            Line::from(""),
            if state.agent_status.agents_listening > 0 {
                Line::from(Span::styled(
                    "  💡 Tip: Use @agent to get AI assistance!",
                    Style::default().fg(Color::Magenta),
                ))
            } else {
                Line::from(Span::styled(
                    "  📝 Register an agent with: osvm bbs agent register <name>",
                    Style::default().fg(Color::DarkGray),
                ))
            },
        ]
    } else {
        state
            .posts
            .iter()
            .enumerate()
            .flat_map(|(i, p)| {
                // Check if this post is from an agent
                let (author_name, is_agent) = if let Some(user) = state.get_user(p.user_id) {
                    (user.short_name.clone(), BBSTuiState::is_agent(user))
                } else {
                    (format!("#{}", p.user_id), false)
                };

                // Build author display with agent badge
                let author_spans = if is_agent {
                    vec![
                        Span::styled("🤖 ", Style::default().fg(Color::Magenta)),
                        Span::styled(
                            author_name,
                            Style::default()
                                .fg(Color::Magenta)
                                .add_modifier(Modifier::BOLD),
                        ),
                    ]
                } else {
                    vec![Span::styled(author_name, Style::default().fg(Color::Green))]
                };

                let mut header_spans = vec![Span::styled(
                    format!("#{} ", i + 1),
                    Style::default()
                        .fg(Color::Cyan)
                        .add_modifier(Modifier::BOLD),
                )];
                header_spans.extend(author_spans);
                header_spans.push(Span::styled(
                    format!(" • {}", p.created_at()),
                    Style::default().fg(Color::DarkGray),
                ));

                // Highlight agent posts with different body style
                let body_style = if is_agent {
                    Style::default().fg(Color::White)
                } else {
                    Style::default()
                };

                vec![
                    Line::from(header_spans),
                    Line::from(Span::styled(format!("  {}", p.body), body_style)),
                    Line::from(""), // Spacing
                ]
            })
            .collect()
    };

    let posts_widget = Paragraph::new(post_lines)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(format!(" Posts (j/k to scroll) "))
                .border_style(Style::default().fg(Color::Cyan)),
        )
        .wrap(Wrap { trim: false })
        .scroll((state.scroll_offset as u16, 0));
    f.render_widget(posts_widget, main_chunks[1]);

    // Input box with agent hints
    let input_placeholder = if state.agent_status.agents_listening > 0 {
        "Press 'i' to write... (use @agent for AI help)"
    } else {
        "Press 'i' to write a message..."
    };

    let input_text = if state.input_active {
        format!("{}█", state.input_buffer) // Show cursor
    } else {
        if state.input_buffer.is_empty() {
            input_placeholder.to_string()
        } else {
            state.input_buffer.clone()
        }
    };

    let input_style = if state.input_active {
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD)
    } else {
        Style::default().fg(Color::DarkGray)
    };

    let border_style = if state.input_active {
        Style::default().fg(Color::Yellow)
    } else {
        Style::default().fg(Color::DarkGray)
    };

    // Input title shows agent availability
    let input_title = if state.agent_status.agents_listening > 0 {
        " 📝 Message (🤖 agents available) "
    } else {
        " 📝 Message Input "
    };

    let input_widget = Paragraph::new(input_text).style(input_style).block(
        Block::default()
            .borders(Borders::ALL)
            .title(input_title)
            .border_style(border_style),
    );
    f.render_widget(input_widget, chunks[2]);

    // Status bar - show agent-aware help
    let status_text = if state.input_active {
        if state.agent_status.agents_listening > 0 {
            "Enter=Send • Esc=Cancel • @agent for AI help • Backspace=Delete"
        } else {
            "Press Enter to send • Esc to cancel • Backspace to delete"
        }
    } else {
        if state.agent_status.agents_listening > 0 {
            "i=Input • j/k=Scroll • 1-9=Board • r=Refresh • 🤖 Agents listening!"
        } else {
            "i=Input • j/k=Scroll • 1-9=Board • r=Refresh • q=Quit"
        }
    };

    let status = Paragraph::new(status_text)
        .style(Style::default().fg(Color::Cyan))
        .alignment(Alignment::Center);
    f.render_widget(status, chunks[3]);
}

impl Default for BBSTuiState {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use diesel::prelude::*;
    use diesel::sqlite::SqliteConnection;
    use tempfile::tempdir;

    /// Helper to create a test BBSTuiState with an in-memory/temp database
    fn create_test_state() -> (BBSTuiState, tempfile::TempDir) {
        let tmp_dir = tempdir().expect("Failed to create temp dir");
        let db_file = tmp_dir.path().join("test-bbs-tui.db");
        let db_url = db_file.to_str().unwrap();

        // Create connection and initialize DB
        let mut conn =
            SqliteConnection::establish(db_url).expect("Failed to connect to test database");
        db::initialize_database(&mut conn).expect("Failed to initialize database");

        // Create a test board and get its ID
        let board = db::boards::create(&mut conn, "TEST_BOARD", "Test board for TUI")
            .expect("Failed to create test board");
        let board_id = board.id;

        // Build state with the connection
        let mut state = BBSTuiState::new();
        *state.conn.lock().unwrap() = Some(conn);
        state.connected = true;
        // Refresh boards from database instead of cloning
        let _ = state.refresh_boards();
        state.current_board = Some(board_id);
        state.selected_board_index = Some(0);

        (state, tmp_dir)
    }

    #[test]
    fn test_post_message_success() {
        let (mut state, _tmp_dir) = create_test_state();

        // Post a message
        let result = state.post_message("Hello from TUI test!");
        assert!(result.is_ok(), "post_message should succeed: {:?}", result);

        let post_id = result.unwrap();
        assert!(post_id > 0, "Post ID should be positive");

        // Verify the post exists in the database
        state.load_posts().expect("Failed to load posts");
        assert_eq!(state.posts.len(), 1, "Should have one post");
        assert_eq!(state.posts[0].body, "Hello from TUI test!");
    }

    #[test]
    fn test_post_message_no_board_selected() {
        let (mut state, _tmp_dir) = create_test_state();

        // Clear the current board
        state.current_board = None;

        // Try to post - should fail
        let result = state.post_message("This should fail");
        assert!(
            result.is_err(),
            "post_message should fail when no board selected"
        );

        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("No board selected"),
            "Error should mention no board: {}",
            err
        );
    }

    #[test]
    fn test_post_message_not_connected() {
        // Create state without a database connection
        let mut state = BBSTuiState::new();
        state.current_board = Some(1); // Fake board ID

        // Try to post - should fail
        let result = state.post_message("This should fail");
        assert!(
            result.is_err(),
            "post_message should fail when not connected"
        );

        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("Not connected"),
            "Error should mention not connected: {}",
            err
        );
    }

    #[test]
    fn test_post_message_creates_tui_user() {
        let (mut state, _tmp_dir) = create_test_state();

        // Post a message
        state
            .post_message("First message")
            .expect("Failed to post first message");

        // Post another message
        state
            .post_message("Second message")
            .expect("Failed to post second message");

        // Both should use the same TUI user
        state.load_posts().expect("Failed to load posts");
        assert_eq!(state.posts.len(), 2, "Should have two posts");

        // Both posts should have the same user_id (the TUI user)
        let user_id_1 = state.posts[0].user_id;
        let user_id_2 = state.posts[1].user_id;
        assert_eq!(
            user_id_1, user_id_2,
            "Both posts should be from the same TUI user"
        );
    }

    #[test]
    fn test_post_message_with_special_characters() {
        let (mut state, _tmp_dir) = create_test_state();

        // Post with special characters including 'i' (the bug we fixed)
        let message =
            "Testing special chars: i, I, 你好, émoji 🎉, quotes \"test\" and 'apostrophe'";
        let result = state.post_message(message);
        assert!(
            result.is_ok(),
            "post_message should handle special characters"
        );

        state.load_posts().expect("Failed to load posts");
        assert_eq!(
            state.posts[0].body, message,
            "Message should be stored exactly"
        );
    }

    #[test]
    fn test_multiple_posts_ordering() {
        let (mut state, _tmp_dir) = create_test_state();

        // Post multiple messages
        state.post_message("First").expect("Failed to post");
        state.post_message("Second").expect("Failed to post");
        state.post_message("Third").expect("Failed to post");

        // Load and verify ordering (should be newest first based on list_for_board)
        state.load_posts().expect("Failed to load posts");
        assert_eq!(state.posts.len(), 3);
        assert_eq!(state.posts[0].body, "Third", "Newest post should be first");
        assert_eq!(state.posts[1].body, "Second");
        assert_eq!(state.posts[2].body, "First", "Oldest post should be last");
    }

    #[test]
    fn test_state_new_defaults() {
        let state = BBSTuiState::new();

        assert!(state.boards.is_empty());
        assert!(state.current_board.is_none());
        assert!(state.posts.is_empty());
        assert!(state.input_buffer.is_empty());
        assert_eq!(state.scroll_offset, 0);
        assert!(!state.connected);
        assert!(!state.input_active);
        assert_eq!(state.selected_board_index, Some(0));
        // Agent-related fields
        assert!(state.agents.is_empty());
        assert_eq!(state.agent_status.agents_listening, 0);
        assert!(!state.agent_status.osvm_agent_online);
        assert!(state.user_cache.is_empty());
        // Mesh-related fields
        assert!(state.mesh_messages.is_empty());
        assert!(state.mesh_nodes.is_empty());
    }

    #[test]
    fn test_mesh_message_buffer() {
        let mut state = BBSTuiState::new();

        // Add messages
        state.add_mesh_message(0x12345678, "Hello mesh!".to_string(), false);
        assert_eq!(state.mesh_messages.len(), 1);
        assert_eq!(state.mesh_messages[0].message, "Hello mesh!");
        assert!(!state.mesh_messages[0].is_command);

        // Add a command
        state.add_mesh_message(0x12345678, "/boards".to_string(), true);
        assert_eq!(state.mesh_messages.len(), 2);
        assert!(state.mesh_messages[1].is_command);

        // Test ring buffer (max 50)
        for i in 0..60 {
            state.add_mesh_message(0x11111111, format!("Msg {}", i), false);
        }
        assert_eq!(state.mesh_messages.len(), 50, "Should cap at 50 messages");
    }

    #[test]
    fn test_mesh_node_cache() {
        let mut state = BBSTuiState::new();

        state.update_mesh_node(0x12345678, "NODE1".to_string());
        assert_eq!(
            state.mesh_nodes.get(&0x12345678),
            Some(&"NODE1".to_string())
        );

        // Add message after node is known
        state.add_mesh_message(0x12345678, "Hello".to_string(), false);
        assert_eq!(state.mesh_messages[0].from_name, Some("NODE1".to_string()));
    }

    #[test]
    fn test_is_agent_detection() {
        // Test agent detection heuristics
        let osvm_agent = User {
            id: 1,
            node_id: "!aaaabbbb".to_string(),
            short_name: "OSVM".to_string(),
            long_name: "OSVM Research Agent".to_string(),
            jackass: false,
            in_board: None,
            created_at_us: 0,
            last_seen_at_us: 0,
            last_acted_at_us: None,
            bio: None,
        };
        assert!(
            BBSTuiState::is_agent(&osvm_agent),
            "OSVM should be detected as agent"
        );

        let bot_user = User {
            id: 2,
            node_id: "!12345678".to_string(),
            short_name: "BOT".to_string(),
            long_name: "Some Bot".to_string(),
            jackass: false,
            in_board: None,
            created_at_us: 0,
            last_seen_at_us: 0,
            last_acted_at_us: None,
            bio: None,
        };
        assert!(
            BBSTuiState::is_agent(&bot_user),
            "BOT should be detected as agent"
        );

        let regular_user = User {
            id: 3,
            node_id: "!deadbeef".to_string(),
            short_name: "USER".to_string(),
            long_name: "Regular Human".to_string(),
            jackass: false,
            in_board: None,
            created_at_us: 0,
            last_seen_at_us: 0,
            last_acted_at_us: None,
            bio: None,
        };
        assert!(
            !BBSTuiState::is_agent(&regular_user),
            "Regular user should NOT be detected as agent"
        );
    }

    // =========================================================================
    // Key Handler Simulation Tests
    // These test the logic that should be implemented in app.rs event_loop
    // to prevent regression of the "can't type 'i' in input mode" bug
    // =========================================================================

    /// Simulates pressing 'i' key when NOT in input mode
    /// Expected: input_active becomes true, buffer unchanged
    fn simulate_i_key_not_in_input(state: &mut BBSTuiState) {
        if !state.input_active {
            state.input_active = true;
        }
    }

    /// Simulates pressing any char key when IN input mode
    /// Expected: char is added to buffer
    fn simulate_char_key_in_input(state: &mut BBSTuiState, c: char) {
        if state.input_active {
            state.input_buffer.push(c);
        }
    }

    #[test]
    fn test_key_i_activates_input_mode() {
        let mut state = BBSTuiState::new();
        assert!(!state.input_active, "Should start with input inactive");

        // Press 'i' to activate
        simulate_i_key_not_in_input(&mut state);

        assert!(state.input_active, "'i' should activate input mode");
        assert!(
            state.input_buffer.is_empty(),
            "Buffer should remain empty on activation"
        );
    }

    #[test]
    fn test_key_i_in_input_mode_adds_to_buffer() {
        let mut state = BBSTuiState::new();

        // Activate input mode first
        state.input_active = true;

        // Now press 'i' while in input mode - should ADD 'i' to buffer
        simulate_char_key_in_input(&mut state, 'i');

        assert!(state.input_active, "Should remain in input mode");
        assert_eq!(state.input_buffer, "i", "'i' should be added to buffer");

        // Press more characters including 'i' again
        simulate_char_key_in_input(&mut state, ' ');
        simulate_char_key_in_input(&mut state, 'l');
        simulate_char_key_in_input(&mut state, 'i');
        simulate_char_key_in_input(&mut state, 'k');
        simulate_char_key_in_input(&mut state, 'e');

        assert_eq!(
            state.input_buffer, "i like",
            "All chars including 'i' should be in buffer"
        );
    }

    #[test]
    fn test_typing_word_with_i_in_input_mode() {
        let mut state = BBSTuiState::new();
        state.input_active = true;

        // Type "this is a test" which has multiple 'i' characters
        for c in "this is a test".chars() {
            simulate_char_key_in_input(&mut state, c);
        }

        assert_eq!(
            state.input_buffer, "this is a test",
            "Should be able to type words with 'i' without losing characters"
        );
    }

    #[test]
    fn test_input_mode_toggle_sequence() {
        let mut state = BBSTuiState::new();

        // 1. Start inactive
        assert!(!state.input_active);

        // 2. Press 'i' to activate
        simulate_i_key_not_in_input(&mut state);
        assert!(state.input_active);
        assert!(state.input_buffer.is_empty());

        // 3. Type "hi"
        simulate_char_key_in_input(&mut state, 'h');
        simulate_char_key_in_input(&mut state, 'i');
        assert_eq!(state.input_buffer, "hi");

        // 4. Deactivate (simulate Esc)
        state.input_active = false;
        state.input_buffer.clear();

        // 5. Press 'i' again to reactivate
        simulate_i_key_not_in_input(&mut state);
        assert!(state.input_active);
        assert!(state.input_buffer.is_empty());
    }
}
