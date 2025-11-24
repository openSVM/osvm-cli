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
        }
    }

    /// Initialize BBS connection
    pub fn connect(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let mut conn = db::establish_connection()?;
        db::initialize_database(&mut conn)?;

        *self.conn.lock().unwrap() = Some(conn);
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
}

/// Render BBS TUI
pub fn render_bbs_tab(f: &mut Frame, area: Rect, state: &mut BBSTuiState) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),  // Header
            Constraint::Min(0),     // Main content
            Constraint::Length(3),  // Input/Status
        ])
        .split(area);

    // Header
    let header = Paragraph::new("📡 OSVM BBS - Meshtastic Communication")
        .style(Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
        .alignment(Alignment::Center)
        .block(Block::default().borders(Borders::ALL));
    f.render_widget(header, chunks[0]);

    // Main content area
    let main_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(30),  // Board list
            Constraint::Percentage(70),  // Posts
        ])
        .split(chunks[1]);

    // Board list
    let boards: Vec<ListItem> = state
        .boards
        .iter()
        .map(|board| {
            let style = if Some(board.id) == state.current_board {
                Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
            } else {
                Style::default()
            };

            ListItem::new(Line::from(vec![
                Span::raw(format!("#{} ", board.id)),
                Span::styled(&board.name, style),
            ]))
        })
        .collect();

    let board_list = List::new(boards)
        .block(Block::default().borders(Borders::ALL).title("Boards"));
    f.render_widget(board_list, main_chunks[0]);

    // Posts area
    let post_text = if state.posts.is_empty() {
        "No posts in this board.\nType 'post <message>' to create one.".to_string()
    } else {
        state
            .posts
            .iter()
            .map(|p| format!("[{}] {}", p.created_at(), p.body))
            .collect::<Vec<_>>()
            .join("\n\n")
    };

    let posts_widget = Paragraph::new(post_text)
        .block(Block::default().borders(Borders::ALL).title("Posts"))
        .wrap(Wrap { trim: true })
        .scroll((state.scroll_offset as u16, 0));
    f.render_widget(posts_widget, main_chunks[1]);

    // Status/Input bar
    let status = Paragraph::new(format!("Status: {} | Commands: 'h' for help", state.status_message))
        .style(Style::default().fg(Color::Green))
        .block(Block::default().borders(Borders::ALL));
    f.render_widget(status, chunks[2]);
}

impl Default for BBSTuiState {
    fn default() -> Self {
        Self::new()
    }
}
