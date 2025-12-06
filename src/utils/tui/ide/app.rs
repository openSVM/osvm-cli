//! OVSM IDE TUI Application
//!
//! An integrated development environment for OVSM LISP scripting.

use super::super::common::centered_rect;

use anyhow::Result;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{
        Block, Borders, Clear, List, ListItem, Paragraph, Scrollbar, ScrollbarOrientation,
        ScrollbarState, Tabs, Wrap,
    },
    Frame, Terminal,
};
use std::collections::VecDeque;
use std::io;
use std::path::PathBuf;
use std::time::{Duration, Instant};

// ============================================================================
// Data Types
// ============================================================================

/// A source file in the editor
#[derive(Debug, Clone)]
pub struct SourceFile {
    pub path: Option<PathBuf>,
    pub name: String,
    pub content: Vec<String>,  // Lines of code
    pub cursor_row: usize,
    pub cursor_col: usize,
    pub scroll_offset: usize,
    pub modified: bool,
    pub syntax_errors: Vec<SyntaxError>,
}

/// A syntax error
#[derive(Debug, Clone)]
pub struct SyntaxError {
    pub line: usize,
    pub column: usize,
    pub message: String,
}

/// REPL history entry
#[derive(Debug, Clone)]
pub struct ReplEntry {
    pub input: String,
    pub output: String,
    pub is_error: bool,
}

/// Output panel entry
#[derive(Debug, Clone)]
pub struct OutputEntry {
    pub text: String,
    pub entry_type: OutputType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputType {
    Info,
    Success,
    Warning,
    Error,
    Result,
}

impl OutputType {
    pub fn color(&self) -> Color {
        match self {
            OutputType::Info => Color::Cyan,
            OutputType::Success => Color::Green,
            OutputType::Warning => Color::Yellow,
            OutputType::Error => Color::Red,
            OutputType::Result => Color::Magenta,
        }
    }
}

/// Focus area
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdeFocus {
    Editor,
    FileTree,
    Repl,
    Output,
}

// ============================================================================
// Application State
// ============================================================================

/// Main IDE application
pub struct IdeApp {
    // Working directory
    pub working_dir: PathBuf,

    // Files
    pub files: Vec<SourceFile>,
    pub active_file: usize,
    pub file_tree: Vec<FileTreeEntry>,
    pub selected_tree_item: usize,
    pub show_file_tree: bool,

    // REPL
    pub repl_history: VecDeque<ReplEntry>,
    pub repl_input: String,
    pub repl_cursor: usize,
    pub repl_scroll: usize,

    // Output
    pub output: VecDeque<OutputEntry>,
    pub output_scroll: usize,

    // UI State
    pub focus: IdeFocus,
    pub should_quit: bool,
    pub show_help: bool,
    pub show_save_dialog: bool,
    pub show_open_dialog: bool,

    // Status
    pub status_message: Option<String>,
    pub status_time: Option<Instant>,

    // Layout
    pub editor_height_pct: u16,
    pub file_tree_width: u16,
}

/// File tree entry
#[derive(Debug, Clone)]
pub struct FileTreeEntry {
    pub name: String,
    pub path: PathBuf,
    pub is_dir: bool,
    pub depth: usize,
    pub expanded: bool,
}

impl IdeApp {
    /// Create a new IDE app
    pub fn new(working_dir: PathBuf) -> Self {
        let mut app = Self {
            working_dir: working_dir.clone(),
            files: vec![SourceFile::new_untitled()],
            active_file: 0,
            file_tree: Vec::new(),
            selected_tree_item: 0,
            show_file_tree: true,
            repl_history: VecDeque::with_capacity(100),
            repl_input: String::new(),
            repl_cursor: 0,
            repl_scroll: 0,
            output: VecDeque::with_capacity(200),
            output_scroll: 0,
            focus: IdeFocus::Editor,
            should_quit: false,
            show_help: false,
            show_save_dialog: false,
            show_open_dialog: false,
            status_message: None,
            status_time: None,
            editor_height_pct: 70,
            file_tree_width: 25,
        };

        // Load welcome content
        app.files[0].content = vec![
            ";; Welcome to OSVM IDE".to_string(),
            ";; ──────────────────────────────".to_string(),
            "".to_string(),
            ";; Write OVSM LISP code here!".to_string(),
            ";; Press F5 to run, Ctrl+S to save".to_string(),
            "".to_string(),
            ";; Example: Define a variable".to_string(),
            "(define greeting \"Hello, OSVM!\")".to_string(),
            "".to_string(),
            ";; Example: Simple function".to_string(),
            "(define (add-two x)".to_string(),
            "  (+ x 2))".to_string(),
            "".to_string(),
            ";; Example: Call function".to_string(),
            "(log :result (add-two 40))".to_string(),
        ];
        app.files[0].name = "welcome.ovsm".to_string();

        app
    }

    /// Initialize the IDE
    pub async fn initialize(&mut self) -> Result<()> {
        // Scan working directory for .ovsm files
        self.scan_directory()?;

        // Add welcome message to output
        self.add_output("OSVM IDE initialized", OutputType::Info);
        self.add_output(&format!("Working directory: {}", self.working_dir.display()), OutputType::Info);
        self.add_output("Press ? for help, F5 to run, Ctrl+Q to quit", OutputType::Info);

        self.set_status("Ready");
        Ok(())
    }

    fn scan_directory(&mut self) -> Result<()> {
        self.file_tree.clear();

        let entries = std::fs::read_dir(&self.working_dir)?;
        let mut items: Vec<_> = entries.filter_map(|e| e.ok()).collect();
        items.sort_by(|a, b| {
            let a_is_dir = a.file_type().map(|t| t.is_dir()).unwrap_or(false);
            let b_is_dir = b.file_type().map(|t| t.is_dir()).unwrap_or(false);
            match (a_is_dir, b_is_dir) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => a.file_name().cmp(&b.file_name()),
            }
        });

        for entry in items {
            let name = entry.file_name().to_string_lossy().to_string();
            let is_dir = entry.file_type().map(|t| t.is_dir()).unwrap_or(false);

            // Only show .ovsm files and directories
            if is_dir || name.ends_with(".ovsm") {
                self.file_tree.push(FileTreeEntry {
                    name,
                    path: entry.path(),
                    is_dir,
                    depth: 0,
                    expanded: false,
                });
            }
        }

        Ok(())
    }

    fn add_output(&mut self, text: &str, entry_type: OutputType) {
        self.output.push_back(OutputEntry {
            text: text.to_string(),
            entry_type,
        });
        if self.output.len() > 200 {
            self.output.pop_front();
        }
        self.output_scroll = self.output.len().saturating_sub(1);
    }

    fn set_status(&mut self, msg: &str) {
        self.status_message = Some(msg.to_string());
        self.status_time = Some(Instant::now());
    }

    /// Run the main event loop
    pub async fn run(&mut self, terminal: &mut Terminal<CrosstermBackend<io::Stdout>>) -> Result<()> {
        loop {
            // Clear old status
            if let Some(time) = self.status_time {
                if time.elapsed() > Duration::from_secs(5) {
                    self.status_message = None;
                    self.status_time = None;
                }
            }

            // Render
            terminal.draw(|f| self.render(f))?;

            // Handle events
            if crossterm::event::poll(Duration::from_millis(100))? {
                if let Event::Key(key) = crossterm::event::read()? {
                    self.handle_key(key).await?;
                }
            }

            if self.should_quit {
                break;
            }
        }

        Ok(())
    }

    /// Handle key input
    async fn handle_key(&mut self, key: KeyEvent) -> Result<()> {
        // Help overlay
        if self.show_help {
            if matches!(key.code, KeyCode::Esc | KeyCode::Char('?')) {
                self.show_help = false;
            }
            return Ok(());
        }

        // Global shortcuts
        match (key.modifiers, key.code) {
            // Quit
            (KeyModifiers::CONTROL, KeyCode::Char('q')) => {
                self.should_quit = true;
                return Ok(());
            }
            // Help
            (_, KeyCode::Char('?')) if self.focus != IdeFocus::Editor => {
                self.show_help = true;
                return Ok(());
            }
            // Save
            (KeyModifiers::CONTROL, KeyCode::Char('s')) => {
                self.save_current_file()?;
                return Ok(());
            }
            // Run
            (_, KeyCode::F(5)) | (KeyModifiers::CONTROL, KeyCode::Char('r')) => {
                self.run_current_file().await?;
                return Ok(());
            }
            // New file
            (KeyModifiers::CONTROL, KeyCode::Char('n')) => {
                self.new_file();
                return Ok(());
            }
            // Toggle file tree
            (KeyModifiers::CONTROL, KeyCode::Char('b')) => {
                self.show_file_tree = !self.show_file_tree;
                return Ok(());
            }
            // Switch focus
            (_, KeyCode::Tab) => {
                self.focus = match self.focus {
                    IdeFocus::Editor => IdeFocus::Repl,
                    IdeFocus::Repl => IdeFocus::Output,
                    IdeFocus::Output => IdeFocus::FileTree,
                    IdeFocus::FileTree => IdeFocus::Editor,
                };
                return Ok(());
            }
            _ => {}
        }

        // Focus-specific handling
        match self.focus {
            IdeFocus::Editor => self.handle_editor_key(key),
            IdeFocus::FileTree => self.handle_file_tree_key(key),
            IdeFocus::Repl => self.handle_repl_key(key).await,
            IdeFocus::Output => self.handle_output_key(key),
        }
    }

    fn handle_editor_key(&mut self, key: KeyEvent) -> Result<()> {
        let file = &mut self.files[self.active_file];

        match (key.modifiers, key.code) {
            // Navigation
            (KeyModifiers::NONE, KeyCode::Up) => {
                if file.cursor_row > 0 {
                    file.cursor_row -= 1;
                    file.cursor_col = file.cursor_col.min(
                        file.content.get(file.cursor_row).map(|l| l.len()).unwrap_or(0)
                    );
                }
            }
            (KeyModifiers::NONE, KeyCode::Down) => {
                if file.cursor_row < file.content.len().saturating_sub(1) {
                    file.cursor_row += 1;
                    file.cursor_col = file.cursor_col.min(
                        file.content.get(file.cursor_row).map(|l| l.len()).unwrap_or(0)
                    );
                }
            }
            (KeyModifiers::NONE, KeyCode::Left) => {
                if file.cursor_col > 0 {
                    file.cursor_col -= 1;
                } else if file.cursor_row > 0 {
                    file.cursor_row -= 1;
                    file.cursor_col = file.content.get(file.cursor_row).map(|l| l.len()).unwrap_or(0);
                }
            }
            (KeyModifiers::NONE, KeyCode::Right) => {
                let line_len = file.content.get(file.cursor_row).map(|l| l.len()).unwrap_or(0);
                if file.cursor_col < line_len {
                    file.cursor_col += 1;
                } else if file.cursor_row < file.content.len().saturating_sub(1) {
                    file.cursor_row += 1;
                    file.cursor_col = 0;
                }
            }
            (KeyModifiers::NONE, KeyCode::Home) | (KeyModifiers::CONTROL, KeyCode::Char('a')) => {
                file.cursor_col = 0;
            }
            (KeyModifiers::NONE, KeyCode::End) | (KeyModifiers::CONTROL, KeyCode::Char('e')) => {
                file.cursor_col = file.content.get(file.cursor_row).map(|l| l.len()).unwrap_or(0);
            }
            (KeyModifiers::CONTROL, KeyCode::Home) => {
                file.cursor_row = 0;
                file.cursor_col = 0;
            }
            (KeyModifiers::CONTROL, KeyCode::End) => {
                file.cursor_row = file.content.len().saturating_sub(1);
                file.cursor_col = file.content.get(file.cursor_row).map(|l| l.len()).unwrap_or(0);
            }

            // Editing
            (KeyModifiers::NONE, KeyCode::Enter) => {
                // Split line at cursor
                let current_line = file.content.get(file.cursor_row).cloned().unwrap_or_default();
                let (before, after) = current_line.split_at(file.cursor_col.min(current_line.len()));
                file.content[file.cursor_row] = before.to_string();
                file.content.insert(file.cursor_row + 1, after.to_string());
                file.cursor_row += 1;
                file.cursor_col = 0;
                file.modified = true;
            }
            (KeyModifiers::NONE, KeyCode::Backspace) => {
                if file.cursor_col > 0 {
                    let line = &mut file.content[file.cursor_row];
                    line.remove(file.cursor_col - 1);
                    file.cursor_col -= 1;
                    file.modified = true;
                } else if file.cursor_row > 0 {
                    // Join with previous line
                    let current_line = file.content.remove(file.cursor_row);
                    file.cursor_row -= 1;
                    file.cursor_col = file.content[file.cursor_row].len();
                    file.content[file.cursor_row].push_str(&current_line);
                    file.modified = true;
                }
            }
            (KeyModifiers::NONE, KeyCode::Delete) => {
                let line_len = file.content.get(file.cursor_row).map(|l| l.len()).unwrap_or(0);
                if file.cursor_col < line_len {
                    file.content[file.cursor_row].remove(file.cursor_col);
                    file.modified = true;
                } else if file.cursor_row < file.content.len().saturating_sub(1) {
                    // Join with next line
                    let next_line = file.content.remove(file.cursor_row + 1);
                    file.content[file.cursor_row].push_str(&next_line);
                    file.modified = true;
                }
            }
            (KeyModifiers::NONE | KeyModifiers::SHIFT, KeyCode::Char(c)) => {
                // Insert character
                if file.content.is_empty() {
                    file.content.push(String::new());
                }
                file.content[file.cursor_row].insert(file.cursor_col, c);
                file.cursor_col += 1;
                file.modified = true;
            }
            (KeyModifiers::NONE, KeyCode::Tab) => {
                // Insert 2 spaces for tab
                file.content[file.cursor_row].insert_str(file.cursor_col, "  ");
                file.cursor_col += 2;
                file.modified = true;
            }

            // Page navigation
            (KeyModifiers::NONE, KeyCode::PageUp) => {
                file.cursor_row = file.cursor_row.saturating_sub(20);
            }
            (KeyModifiers::NONE, KeyCode::PageDown) => {
                file.cursor_row = (file.cursor_row + 20).min(file.content.len().saturating_sub(1));
            }

            // Escape to switch to file tree
            (KeyModifiers::NONE, KeyCode::Esc) => {
                if self.show_file_tree {
                    self.focus = IdeFocus::FileTree;
                }
            }

            _ => {}
        }

        // Update scroll to keep cursor visible
        let visible_height = 20; // Approximate
        if file.cursor_row < file.scroll_offset {
            file.scroll_offset = file.cursor_row;
        } else if file.cursor_row >= file.scroll_offset + visible_height {
            file.scroll_offset = file.cursor_row - visible_height + 1;
        }

        Ok(())
    }

    fn handle_file_tree_key(&mut self, key: KeyEvent) -> Result<()> {
        match key.code {
            KeyCode::Up | KeyCode::Char('k') => {
                if self.selected_tree_item > 0 {
                    self.selected_tree_item -= 1;
                }
            }
            KeyCode::Down | KeyCode::Char('j') => {
                if self.selected_tree_item < self.file_tree.len().saturating_sub(1) {
                    self.selected_tree_item += 1;
                }
            }
            KeyCode::Enter => {
                self.open_selected_file()?;
            }
            KeyCode::Esc => {
                self.focus = IdeFocus::Editor;
            }
            _ => {}
        }
        Ok(())
    }

    async fn handle_repl_key(&mut self, key: KeyEvent) -> Result<()> {
        match (key.modifiers, key.code) {
            (KeyModifiers::NONE, KeyCode::Enter) => {
                if !self.repl_input.is_empty() {
                    self.execute_repl().await?;
                }
            }
            (KeyModifiers::NONE | KeyModifiers::SHIFT, KeyCode::Char(c)) => {
                self.repl_input.insert(self.repl_cursor, c);
                self.repl_cursor += 1;
            }
            (KeyModifiers::NONE, KeyCode::Backspace) => {
                if self.repl_cursor > 0 {
                    self.repl_input.remove(self.repl_cursor - 1);
                    self.repl_cursor -= 1;
                }
            }
            (KeyModifiers::NONE, KeyCode::Left) => {
                if self.repl_cursor > 0 {
                    self.repl_cursor -= 1;
                }
            }
            (KeyModifiers::NONE, KeyCode::Right) => {
                if self.repl_cursor < self.repl_input.len() {
                    self.repl_cursor += 1;
                }
            }
            (KeyModifiers::NONE, KeyCode::Home) => {
                self.repl_cursor = 0;
            }
            (KeyModifiers::NONE, KeyCode::End) => {
                self.repl_cursor = self.repl_input.len();
            }
            (KeyModifiers::CONTROL, KeyCode::Char('u')) => {
                self.repl_input.clear();
                self.repl_cursor = 0;
            }
            (KeyModifiers::NONE, KeyCode::Esc) => {
                self.focus = IdeFocus::Editor;
            }
            _ => {}
        }
        Ok(())
    }

    fn handle_output_key(&mut self, key: KeyEvent) -> Result<()> {
        match key.code {
            KeyCode::Up | KeyCode::Char('k') => {
                if self.output_scroll > 0 {
                    self.output_scroll -= 1;
                }
            }
            KeyCode::Down | KeyCode::Char('j') => {
                if self.output_scroll < self.output.len().saturating_sub(1) {
                    self.output_scroll += 1;
                }
            }
            KeyCode::Home => {
                self.output_scroll = 0;
            }
            KeyCode::End => {
                self.output_scroll = self.output.len().saturating_sub(1);
            }
            KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                self.output.clear();
                self.output_scroll = 0;
            }
            KeyCode::Esc => {
                self.focus = IdeFocus::Editor;
            }
            _ => {}
        }
        Ok(())
    }

    fn new_file(&mut self) {
        self.files.push(SourceFile::new_untitled());
        self.active_file = self.files.len() - 1;
        self.set_status("New file created");
    }

    fn save_current_file(&mut self) -> Result<()> {
        let file = &self.files[self.active_file];
        let content = file.content.join("\n");
        let file_name = file.name.clone();

        let save_path = if let Some(path) = &file.path {
            path.clone()
        } else {
            self.working_dir.join(&file_name)
        };

        std::fs::write(&save_path, &content)?;

        // Update file state
        let file = &mut self.files[self.active_file];
        if file.path.is_none() {
            file.path = Some(save_path.clone());
        }
        file.modified = false;

        self.set_status(&format!("Saved: {}", save_path.display()));

        // Refresh file tree
        let _ = self.scan_directory();

        Ok(())
    }

    fn open_selected_file(&mut self) -> Result<()> {
        if let Some(entry) = self.file_tree.get(self.selected_tree_item).cloned() {
            if !entry.is_dir {
                self.open_file(&entry.path)?;
            }
        }
        Ok(())
    }

    pub fn open_file(&mut self, path: &PathBuf) -> Result<()> {
        // Check if already open
        for (i, file) in self.files.iter().enumerate() {
            if file.path.as_ref() == Some(path) {
                self.active_file = i;
                self.focus = IdeFocus::Editor;
                self.set_status(&format!("Switched to: {}", path.display()));
                return Ok(());
            }
        }

        // Open new file
        let content = std::fs::read_to_string(path)?;
        let lines: Vec<String> = content.lines().map(|l| l.to_string()).collect();

        let file = SourceFile {
            path: Some(path.clone()),
            name: path.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_else(|| "untitled".to_string()),
            content: if lines.is_empty() { vec![String::new()] } else { lines },
            cursor_row: 0,
            cursor_col: 0,
            scroll_offset: 0,
            modified: false,
            syntax_errors: Vec::new(),
        };

        self.files.push(file);
        self.active_file = self.files.len() - 1;
        self.focus = IdeFocus::Editor;
        self.set_status(&format!("Opened: {}", path.display()));

        Ok(())
    }

    async fn run_current_file(&mut self) -> Result<()> {
        let file = &self.files[self.active_file];
        let code = file.content.join("\n");
        let file_name = file.name.clone();

        self.add_output(&format!("▶ Running: {}", file_name), OutputType::Info);

        // Use the OVSM evaluator
        match self.evaluate_ovsm(&code) {
            Ok(result) => {
                self.add_output(&format!("→ {}", result), OutputType::Result);
                self.set_status("Execution completed");
            }
            Err(e) => {
                self.add_output(&format!("✗ Error: {}", e), OutputType::Error);
                self.set_status("Execution failed");
            }
        }

        Ok(())
    }

    async fn execute_repl(&mut self) -> Result<()> {
        let input = self.repl_input.clone();
        self.repl_input.clear();
        self.repl_cursor = 0;

        self.add_output(&format!("> {}", input), OutputType::Info);

        // Execute with OVSM
        match self.evaluate_ovsm(&input) {
            Ok(result) => {
                self.repl_history.push_back(ReplEntry {
                    input: input.clone(),
                    output: result.clone(),
                    is_error: false,
                });
                self.add_output(&format!("→ {}", result), OutputType::Result);
            }
            Err(e) => {
                let error_str = format!("{}", e);
                self.repl_history.push_back(ReplEntry {
                    input: input.clone(),
                    output: error_str.clone(),
                    is_error: true,
                });
                self.add_output(&format!("✗ {}", error_str), OutputType::Error);
            }
        }

        if self.repl_history.len() > 100 {
            self.repl_history.pop_front();
        }

        Ok(())
    }

    /// Evaluate OVSM code and return the result as a string
    fn evaluate_ovsm(&self, code: &str) -> Result<String> {
        use ovsm::{Evaluator, Parser, Scanner};

        // Tokenize
        let mut scanner = Scanner::new(code);
        let tokens = scanner.scan_tokens()
            .map_err(|e| anyhow::anyhow!("Tokenization error: {}", e))?;

        // Parse
        let mut parser = Parser::new(tokens);
        let program = parser.parse()
            .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;

        // Evaluate
        let mut evaluator = Evaluator::new();
        let result = evaluator.execute(&program)
            .map_err(|e| anyhow::anyhow!("Runtime error: {}", e))?;

        Ok(format!("{:?}", result))
    }

    // ========================================================================
    // Rendering
    // ========================================================================

    fn render(&self, f: &mut Frame) {
        let main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1),  // Tab bar
                Constraint::Min(10),    // Main content
                Constraint::Length(1),  // Status bar
            ])
            .split(f.area());

        self.render_tab_bar(f, main_chunks[0]);
        self.render_main_content(f, main_chunks[1]);
        self.render_status_bar(f, main_chunks[2]);

        // Help overlay
        if self.show_help {
            self.render_help(f);
        }
    }

    fn render_tab_bar(&self, f: &mut Frame, area: Rect) {
        let tabs: Vec<Span> = self.files
            .iter()
            .enumerate()
            .map(|(i, file)| {
                let name = if file.modified {
                    format!("• {} ", file.name)
                } else {
                    format!("  {} ", file.name)
                };
                if i == self.active_file {
                    Span::styled(name, Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
                } else {
                    Span::styled(name, Style::default().fg(Color::DarkGray))
                }
            })
            .collect();

        let mut line_spans = vec![Span::styled(" 📝 ", Style::default().fg(Color::Yellow))];
        line_spans.extend(tabs);

        f.render_widget(Paragraph::new(Line::from(line_spans)), area);
    }

    fn render_main_content(&self, f: &mut Frame, area: Rect) {
        // Split horizontally for file tree
        let h_chunks = if self.show_file_tree {
            Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Length(self.file_tree_width),
                    Constraint::Min(30),
                ])
                .split(area)
        } else {
            Layout::default()
                .direction(Direction::Horizontal)
                .constraints([Constraint::Min(30)])
                .split(area)
        };

        if self.show_file_tree && h_chunks.len() > 1 {
            self.render_file_tree(f, h_chunks[0]);
        }

        let editor_area = if self.show_file_tree && h_chunks.len() > 1 {
            h_chunks[1]
        } else {
            h_chunks[0]
        };

        // Split vertically for editor and bottom panels
        let v_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Percentage(self.editor_height_pct),
                Constraint::Min(5),
            ])
            .split(editor_area);

        self.render_editor(f, v_chunks[0]);

        // Split bottom for REPL and Output
        let bottom_chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Percentage(50),
                Constraint::Percentage(50),
            ])
            .split(v_chunks[1]);

        self.render_repl(f, bottom_chunks[0]);
        self.render_output(f, bottom_chunks[1]);
    }

    fn render_file_tree(&self, f: &mut Frame, area: Rect) {
        let is_focused = self.focus == IdeFocus::FileTree;
        let border_color = if is_focused { Color::Cyan } else { Color::DarkGray };

        let items: Vec<ListItem> = self.file_tree
            .iter()
            .enumerate()
            .map(|(i, entry)| {
                let icon = if entry.is_dir { "📁" } else { "📄" };
                let style = if i == self.selected_tree_item {
                    Style::default().bg(Color::DarkGray)
                } else {
                    Style::default()
                };
                ListItem::new(Line::from(vec![
                    Span::raw("  ".repeat(entry.depth)),
                    Span::raw(format!("{} {}", icon, entry.name)),
                ])).style(style)
            })
            .collect();

        let list = List::new(items)
            .block(Block::default()
                .title(" Files ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(border_color)));

        f.render_widget(list, area);
    }

    fn render_editor(&self, f: &mut Frame, area: Rect) {
        let is_focused = self.focus == IdeFocus::Editor;
        let border_color = if is_focused { Color::Cyan } else { Color::DarkGray };

        let file = &self.files[self.active_file];
        let inner_height = area.height.saturating_sub(2) as usize;

        // Build visible lines with line numbers and syntax highlighting
        let lines: Vec<Line> = file.content
            .iter()
            .enumerate()
            .skip(file.scroll_offset)
            .take(inner_height)
            .map(|(i, line)| {
                let line_num = format!("{:4} ", i + 1);
                let is_cursor_line = i == file.cursor_row;

                let mut spans = vec![
                    Span::styled(
                        line_num,
                        Style::default().fg(if is_cursor_line { Color::Yellow } else { Color::DarkGray })
                    ),
                ];

                // Simple syntax highlighting for OVSM LISP
                let highlighted = self.highlight_line(line);
                spans.extend(highlighted);

                // Show cursor
                if is_focused && is_cursor_line && file.cursor_col <= line.len() {
                    // Cursor is handled by terminal, but we can indicate position
                }

                Line::from(spans)
            })
            .collect();

        let editor = Paragraph::new(lines)
            .block(Block::default()
                .title(format!(" {} ", file.name))
                .borders(Borders::ALL)
                .border_style(Style::default().fg(border_color)));

        f.render_widget(editor, area);

        // Set cursor position
        if is_focused {
            let cursor_x = area.x + 6 + file.cursor_col as u16;
            let cursor_y = area.y + 1 + (file.cursor_row - file.scroll_offset) as u16;
            if cursor_y < area.y + area.height - 1 {
                f.set_cursor_position((cursor_x, cursor_y));
            }
        }
    }

    fn highlight_line(&self, line: &str) -> Vec<Span<'static>> {
        let mut spans = Vec::new();
        let mut chars = line.chars().peekable();
        let mut current = String::new();
        let mut in_string = false;
        let mut in_comment = false;

        while let Some(c) = chars.next() {
            if in_comment {
                current.push(c);
                continue;
            }

            if c == '"' {
                if in_string {
                    current.push(c);
                    spans.push(Span::styled(current.clone(), Style::default().fg(Color::Yellow)));
                    current.clear();
                    in_string = false;
                } else {
                    if !current.is_empty() {
                        spans.push(self.colorize_token(&current));
                        current.clear();
                    }
                    current.push(c);
                    in_string = true;
                }
            } else if in_string {
                current.push(c);
            } else if c == ';' {
                if !current.is_empty() {
                    spans.push(self.colorize_token(&current));
                    current.clear();
                }
                current.push(c);
                in_comment = true;
            } else if c == '(' || c == ')' {
                if !current.is_empty() {
                    spans.push(self.colorize_token(&current));
                    current.clear();
                }
                spans.push(Span::styled(c.to_string(), Style::default().fg(Color::Magenta)));
            } else if c.is_whitespace() {
                if !current.is_empty() {
                    spans.push(self.colorize_token(&current));
                    current.clear();
                }
                spans.push(Span::raw(c.to_string()));
            } else {
                current.push(c);
            }
        }

        if !current.is_empty() {
            if in_comment {
                spans.push(Span::styled(current, Style::default().fg(Color::DarkGray)));
            } else if in_string {
                spans.push(Span::styled(current, Style::default().fg(Color::Yellow)));
            } else {
                spans.push(self.colorize_token(&current));
            }
        }

        spans
    }

    fn colorize_token(&self, token: &str) -> Span<'static> {
        let keywords = ["define", "if", "cond", "let", "lambda", "set!", "begin", "for", "while", "loop"];
        let builtins = ["+", "-", "*", "/", "=", "<", ">", "<=", ">=", "not", "and", "or", "log", "get", "len", "map", "filter", "reduce"];

        if keywords.contains(&token) {
            Span::styled(token.to_string(), Style::default().fg(Color::Blue).add_modifier(Modifier::BOLD))
        } else if builtins.contains(&token) {
            Span::styled(token.to_string(), Style::default().fg(Color::Green))
        } else if token.starts_with(':') {
            Span::styled(token.to_string(), Style::default().fg(Color::Cyan))
        } else if token.parse::<f64>().is_ok() {
            Span::styled(token.to_string(), Style::default().fg(Color::Red))
        } else if token == "true" || token == "false" || token == "null" {
            Span::styled(token.to_string(), Style::default().fg(Color::Red))
        } else {
            Span::raw(token.to_string())
        }
    }

    fn render_repl(&self, f: &mut Frame, area: Rect) {
        let is_focused = self.focus == IdeFocus::Repl;
        let border_color = if is_focused { Color::Cyan } else { Color::DarkGray };

        let input_line = Line::from(vec![
            Span::styled("> ", Style::default().fg(Color::Yellow)),
            Span::raw(&self.repl_input),
            if is_focused {
                Span::styled("│", Style::default().fg(Color::Cyan))
            } else {
                Span::raw("")
            },
        ]);

        let repl = Paragraph::new(input_line)
            .block(Block::default()
                .title(" REPL ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(border_color)));

        f.render_widget(repl, area);
    }

    fn render_output(&self, f: &mut Frame, area: Rect) {
        let is_focused = self.focus == IdeFocus::Output;
        let border_color = if is_focused { Color::Cyan } else { Color::DarkGray };

        let inner_height = area.height.saturating_sub(2) as usize;
        let start = self.output_scroll.saturating_sub(inner_height / 2);

        let lines: Vec<Line> = self.output
            .iter()
            .skip(start)
            .take(inner_height)
            .map(|entry| {
                Line::from(vec![
                    Span::styled(&entry.text, Style::default().fg(entry.entry_type.color()))
                ])
            })
            .collect();

        let output = Paragraph::new(lines)
            .block(Block::default()
                .title(" Output ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(border_color)));

        f.render_widget(output, area);
    }

    fn render_status_bar(&self, f: &mut Frame, area: Rect) {
        let file = &self.files[self.active_file];
        let status = self.status_message.as_deref().unwrap_or("");

        let mode = match self.focus {
            IdeFocus::Editor => "EDIT",
            IdeFocus::FileTree => "FILES",
            IdeFocus::Repl => "REPL",
            IdeFocus::Output => "OUTPUT",
        };

        let line = Line::from(vec![
            Span::styled(format!(" {} ", mode), Style::default().bg(Color::Blue).fg(Color::White)),
            Span::raw(format!(" Ln {}, Col {} ", file.cursor_row + 1, file.cursor_col + 1)),
            Span::raw("│ "),
            Span::styled(status, Style::default().fg(Color::Yellow)),
            Span::raw(" │ "),
            Span::styled(" F5:Run  Ctrl+S:Save  Ctrl+Q:Quit  ?:Help ", Style::default().fg(Color::DarkGray)),
        ]);

        f.render_widget(Paragraph::new(line), area);
    }

    fn render_help(&self, f: &mut Frame) {
        let help_text = vec![
            Line::from(""),
            Line::from(vec![Span::styled("  OSVM IDE HELP", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD))]),
            Line::from(""),
            Line::from(vec![Span::styled("  EDITOR", Style::default().fg(Color::Cyan))]),
            Line::from("  Arrow keys    Navigate"),
            Line::from("  Home/End      Start/end of line"),
            Line::from("  Ctrl+Home/End Start/end of file"),
            Line::from("  PageUp/Down   Scroll page"),
            Line::from(""),
            Line::from(vec![Span::styled("  FILE OPERATIONS", Style::default().fg(Color::Cyan))]),
            Line::from("  Ctrl+S        Save file"),
            Line::from("  Ctrl+N        New file"),
            Line::from("  Ctrl+B        Toggle file tree"),
            Line::from(""),
            Line::from(vec![Span::styled("  EXECUTION", Style::default().fg(Color::Cyan))]),
            Line::from("  F5 / Ctrl+R   Run current file"),
            Line::from("  REPL          Enter expressions directly"),
            Line::from(""),
            Line::from(vec![Span::styled("  NAVIGATION", Style::default().fg(Color::Cyan))]),
            Line::from("  Tab           Switch focus"),
            Line::from("  Esc           Return to editor"),
            Line::from("  Ctrl+Q        Quit"),
            Line::from(""),
            Line::from("  Press Esc or ? to close"),
        ];

        let popup = Paragraph::new(help_text)
            .block(Block::default()
                .title(" Help ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Cyan)));

        let area = centered_rect(60, 70, f.area());
        f.render_widget(Clear, area);
        f.render_widget(popup, area);
    }
}

impl SourceFile {
    fn new_untitled() -> Self {
        Self {
            path: None,
            name: "untitled.ovsm".to_string(),
            content: vec![String::new()],
            cursor_row: 0,
            cursor_col: 0,
            scroll_offset: 0,
            modified: false,
            syntax_errors: Vec::new(),
        }
    }
}

// Helper functions are now in super::super::common module

impl Default for IdeApp {
    fn default() -> Self {
        Self::new(std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_app_creation() {
        let app = IdeApp::new(PathBuf::from("/tmp"));
        assert!(!app.files.is_empty());
        assert_eq!(app.focus, IdeFocus::Editor);
    }

    #[test]
    fn test_source_file_creation() {
        let file = SourceFile::new_untitled();
        assert_eq!(file.name, "untitled.ovsm");
        assert!(!file.modified);
    }

    #[test]
    fn test_output_type_colors() {
        assert_eq!(OutputType::Error.color(), Color::Red);
        assert_eq!(OutputType::Success.color(), Color::Green);
    }
}
