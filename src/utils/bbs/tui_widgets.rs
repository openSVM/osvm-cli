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

use crate::utils::bbs::{db, models::{Board, Post, User}};
use diesel::sqlite::SqliteConnection;

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
    pub connected: bool,  // Track if we've initialized the connection
    pub input_active: bool,  // NEW: Track if input mode is active
    pub selected_board_index: Option<usize>,  // NEW: Track selected board for visual feedback
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
            input_active: false,  // NEW
            selected_board_index: Some(0),  // NEW: Default to first board
        }
    }

    /// Initialize BBS connection
    pub fn connect(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        if self.connected {
            return Ok(());  // Already connected
        }

        let mut conn = db::establish_connection()?;
        db::initialize_database(&mut conn)?;

        *self.conn.lock().unwrap() = Some(conn);
        self.connected = true;
        self.status_message = "Connected to OSVM BBS".to_string();
        self.refresh_boards()?;

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
        if let (Some(ref mut conn), Some(board_id)) = (
            self.conn.lock().unwrap().as_mut(),
            self.current_board,
        ) {
            self.posts = db::posts::list_for_board(conn, board_id, 50)?;
        }
        Ok(())
    }

    /// Post a message to the current board
    /// Returns the post ID on success
    pub fn post_message(&mut self, message: &str) -> Result<i32, Box<dyn std::error::Error>> {
        let board_id = self.current_board
            .ok_or_else(|| "No board selected")?;

        let mut conn_guard = self.conn.lock().unwrap();
        let conn = conn_guard.as_mut()
            .ok_or_else(|| "Not connected to database")?;

        // Get or create a TUI user (similar to CLI user pattern)
        let timestamp = db::now_as_useconds();
        let (user, _created) = db::users::observe(
            conn,
            "!tuiuser1",       // node_id
            Some("TUI"),       // short_name
            Some("TUI User"),  // long_name
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
            Constraint::Length(3),  // Header
            Constraint::Min(0),     // Main content
            Constraint::Length(4),  // Input box (bigger!)
            Constraint::Length(2),  // Status bar (smaller)
        ])
        .split(area);

    // Header with better info
    let board_name = state.current_board.and_then(|id| {
        state.boards.iter().find(|b| b.id == id).map(|b| b.name.as_str())
    }).unwrap_or("No board selected");

    let header_text = format!("📡 OSVM BBS - Meshtastic | Board: {} | {} posts",
        board_name, state.posts.len());

    let header = Paragraph::new(header_text)
        .style(Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
        .alignment(Alignment::Center)
        .block(Block::default().borders(Borders::ALL));
    f.render_widget(header, chunks[0]);

    // Main content area
    let main_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(25),  // Board list (smaller)
            Constraint::Percentage(75),  // Posts (bigger)
        ])
        .split(chunks[1]);

    // Board list with CLEAR selection indicators
    let boards: Vec<ListItem> = state
        .boards
        .iter()
        .enumerate()
        .map(|(idx, board)| {
            let is_selected = state.selected_board_index == Some(idx);
            let prefix = if is_selected { "▶ " } else { "  " };
            let style = if is_selected {
                Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(Color::DarkGray)
            };

            ListItem::new(Line::from(vec![
                Span::styled(format!("{}{} ", prefix, idx + 1), style),  // Show 1-based index
                Span::styled(&board.name, style),
            ]))
        })
        .collect();

    let board_list = List::new(boards)
        .block(Block::default()
            .borders(Borders::ALL)
            .title(" Boards (1-9 to select) ")
            .border_style(Style::default().fg(Color::Cyan)));
    f.render_widget(board_list, main_chunks[0]);

    // Posts area with MUCH better formatting
    let post_lines: Vec<Line> = if state.posts.is_empty() {
        vec![
            Line::from(""),
            Line::from(Span::styled("  No posts yet in this board.", Style::default().fg(Color::DarkGray))),
            Line::from(""),
            Line::from(Span::styled("  Press 'i' to write a new post!", Style::default().fg(Color::Yellow))),
        ]
    } else {
        state.posts.iter().enumerate().flat_map(|(i, p)| {
            vec![
                Line::from(vec![
                    Span::styled(format!("#{} ", i + 1), Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                    Span::styled(format!("user#{}", p.user_id), Style::default().fg(Color::Green)),
                    Span::styled(format!(" • {}", p.created_at()), Style::default().fg(Color::DarkGray)),
                ]),
                Line::from(Span::raw(format!("  {}", p.body))),
                Line::from(""),  // Spacing
            ]
        }).collect()
    };

    let posts_widget = Paragraph::new(post_lines)
        .block(Block::default()
            .borders(Borders::ALL)
            .title(format!(" Posts (j/k to scroll) "))
            .border_style(Style::default().fg(Color::Cyan)))
        .wrap(Wrap { trim: false })
        .scroll((state.scroll_offset as u16, 0));
    f.render_widget(posts_widget, main_chunks[1]);

    // ACTUAL INPUT BOX (like Chat tab!)
    let input_text = if state.input_active {
        format!("{}█", state.input_buffer)  // Show cursor
    } else {
        if state.input_buffer.is_empty() {
            "Press 'i' to write a message...".to_string()
        } else {
            state.input_buffer.clone()
        }
    };

    let input_style = if state.input_active {
        Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
    } else {
        Style::default().fg(Color::DarkGray)
    };

    let border_style = if state.input_active {
        Style::default().fg(Color::Yellow)
    } else {
        Style::default().fg(Color::DarkGray)
    };

    let input_widget = Paragraph::new(input_text)
        .style(input_style)
        .block(Block::default()
            .borders(Borders::ALL)
            .title(" 📝 Message Input ")
            .border_style(border_style));
    f.render_widget(input_widget, chunks[2]);

    // Status bar with USEFUL info
    let status_text = if state.input_active {
        "Press Enter to send • Esc to cancel • Backspace to delete"
    } else {
        "i=Input • j/k=Scroll • 1-9=Board • r=Refresh • ?=Help"
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
        let mut conn = SqliteConnection::establish(db_url)
            .expect("Failed to connect to test database");
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
        assert!(result.is_err(), "post_message should fail when no board selected");

        let err = result.unwrap_err().to_string();
        assert!(err.contains("No board selected"), "Error should mention no board: {}", err);
    }

    #[test]
    fn test_post_message_not_connected() {
        // Create state without a database connection
        let mut state = BBSTuiState::new();
        state.current_board = Some(1);  // Fake board ID

        // Try to post - should fail
        let result = state.post_message("This should fail");
        assert!(result.is_err(), "post_message should fail when not connected");

        let err = result.unwrap_err().to_string();
        assert!(err.contains("Not connected"), "Error should mention not connected: {}", err);
    }

    #[test]
    fn test_post_message_creates_tui_user() {
        let (mut state, _tmp_dir) = create_test_state();

        // Post a message
        state.post_message("First message").expect("Failed to post first message");

        // Post another message
        state.post_message("Second message").expect("Failed to post second message");

        // Both should use the same TUI user
        state.load_posts().expect("Failed to load posts");
        assert_eq!(state.posts.len(), 2, "Should have two posts");

        // Both posts should have the same user_id (the TUI user)
        let user_id_1 = state.posts[0].user_id;
        let user_id_2 = state.posts[1].user_id;
        assert_eq!(user_id_1, user_id_2, "Both posts should be from the same TUI user");
    }

    #[test]
    fn test_post_message_with_special_characters() {
        let (mut state, _tmp_dir) = create_test_state();

        // Post with special characters including 'i' (the bug we fixed)
        let message = "Testing special chars: i, I, 你好, émoji 🎉, quotes \"test\" and 'apostrophe'";
        let result = state.post_message(message);
        assert!(result.is_ok(), "post_message should handle special characters");

        state.load_posts().expect("Failed to load posts");
        assert_eq!(state.posts[0].body, message, "Message should be stored exactly");
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
    }
}
