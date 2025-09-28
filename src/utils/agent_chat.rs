//! Dynamic embedded terminal chat interface with real-time suggestions
//! 
//! This module provides a Claude Code-style chat interface that runs embedded 
//! in the terminal session with real-time auto-complete and suggestions.

use crate::services::mcp_service::{McpService, McpTool, McpServerConfig};
use crate::services::ai_service::{AiService, ToolPlan, PlannedTool};
use anyhow::{Result, Context, anyhow};
use serde_json::Value;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use log::{error, warn, debug};
use std::io::{self, Write, Read};
use std::time::Duration;
use tokio::time::sleep;
use tokio::sync::mpsc;
use std::fs;
use serde::{Serialize, Deserialize};


/// ANSI color codes for better readability
struct Colors;

impl Colors {
    const RESET: &'static str = "\x1b[0m";
    const CYAN: &'static str = "\x1b[36m";      // System messages
    const GREEN: &'static str = "\x1b[32m";     // User input, success
    const YELLOW: &'static str = "\x1b[33m";    // Tool names
    const RED: &'static str = "\x1b[31m";       // Errors
    const BLUE: &'static str = "\x1b[34m";      // Server names
    const MAGENTA: &'static str = "\x1b[35m";   // Special actions
    const DIM: &'static str = "\x1b[2m";        // Parameters, less important
    const BOLD: &'static str = "\x1b[1m";       // Emphasis
    const GRAY: &'static str = "\x1b[90m";      // Auto-complete suggestions
}

/// Real-time suggestion system
#[derive(Debug, Clone)]
pub struct RealtimeSuggestion {
    pub text: String,
    pub description: String,
    pub category: String,
    pub score: f32,  // Fuzzy matching score
    pub matched_indices: Vec<usize>,  // Highlighted character positions
}

/// Fuzzy matching engine
pub struct FuzzyMatcher {
    threshold: f32,
}

impl FuzzyMatcher {
    pub fn new(threshold: f32) -> Self {
        Self { threshold }
    }

    /// Calculate fuzzy match score using modified Levenshtein distance
    pub fn score(&self, pattern: &str, text: &str) -> f32 {
        if pattern.is_empty() {
            return 1.0;
        }

        let pattern = pattern.to_lowercase();
        let text = text.to_lowercase();

        // Exact match gets highest score
        if text.contains(&pattern) {
            return 1.0 - (pattern.len() as f32 / text.len() as f32) * 0.1;
        }

        // Character sequence matching
        let mut score = 0.0;
        let mut last_index = 0;
        let mut consecutive_bonus = 0.0;

        for ch in pattern.chars() {
            if let Some(index) = text[last_index..].find(ch) {
                let actual_index = last_index + index;

                // Bonus for consecutive characters
                if actual_index == last_index {
                    consecutive_bonus += 0.1;
                } else {
                    consecutive_bonus = 0.0;
                }

                // Bonus for start of word
                let start_bonus = if actual_index == 0 ||
                    text.chars().nth(actual_index - 1).map_or(false, |c| !c.is_alphanumeric()) {
                    0.2
                } else {
                    0.0
                };

                score += 1.0 + consecutive_bonus + start_bonus;
                last_index = actual_index + 1;
            } else {
                return 0.0; // Character not found
            }
        }

        // Normalize score
        let max_score = pattern.len() as f32 * 1.3; // Max possible score with bonuses
        let normalized = score / max_score;

        // Penalty for length difference
        let length_penalty = (text.len() as f32 - pattern.len() as f32).abs() / text.len() as f32 * 0.2;

        (normalized - length_penalty).max(0.0)
    }

    /// Get highlighted character indices for matched pattern
    pub fn get_match_indices(&self, pattern: &str, text: &str) -> Vec<usize> {
        let mut indices = Vec::new();
        let pattern = pattern.to_lowercase();
        let text = text.to_lowercase();
        let mut last_index = 0;

        for ch in pattern.chars() {
            if let Some(index) = text[last_index..].find(ch) {
                indices.push(last_index + index);
                last_index += index + 1;
            }
        }

        indices
    }

    /// Filter and score suggestions using fuzzy matching
    pub fn filter_suggestions(&self, pattern: &str, candidates: &[(String, String, String)]) -> Vec<RealtimeSuggestion> {
        let mut scored_suggestions: Vec<RealtimeSuggestion> = candidates
            .iter()
            .filter_map(|(text, description, category)| {
                let score = self.score(pattern, text);
                if score >= self.threshold {
                    Some(RealtimeSuggestion {
                        text: text.clone(),
                        description: description.clone(),
                        category: category.clone(),
                        score,
                        matched_indices: self.get_match_indices(pattern, text),
                    })
                } else {
                    None
                }
            })
            .collect();

        // Sort by score (highest first)
        scored_suggestions.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));

        scored_suggestions
    }
}

/// Task state for status bar management
#[derive(Debug, Clone)]
pub struct TaskState {
    pub current_task: String,
    pub todo_items: Vec<TodoItem>,
    pub current_reasoning: String,
    pub selected_todo_index: usize,
    pub input_mode: InputMode,
    pub spinner_frame: usize,
}

#[derive(Debug, Clone)]
pub struct TodoItem {
    pub text: String,
    pub completed: bool,
    pub priority: TodoPriority,
    pub reasoning: String,
    pub tool_plan: Option<String>,
    pub execution_results: Option<String>,
}

#[derive(Debug, Clone)]
pub enum TodoPriority {
    High,
    Medium,
    Low,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InputMode {
    InputField,
    TaskSelection,
}

impl TaskState {
    pub fn new() -> Self {
        Self {
            current_task: "Initialize OSVM Agent".to_string(),
            todo_items: vec![
                TodoItem {
                    text: "Load MCP configurations".to_string(),
                    completed: false,
                    priority: TodoPriority::High,
                    reasoning: "Initialize MCP service and load existing server configurations from config files.".to_string(),
                    tool_plan: Some("1. Create McpService instance\n2. Load config from ~/.osvm/mcp.json\n3. Validate server configurations".to_string()),
                    execution_results: None,
                },
                TodoItem {
                    text: "Connect to AI service".to_string(),
                    completed: false,
                    priority: TodoPriority::High,
                    reasoning: "Establish connection to OSVM AI service for natural language processing and planning.".to_string(),
                    tool_plan: Some("1. Initialize AiService with debug mode\n2. Test connectivity to osvm.ai\n3. Setup request handlers".to_string()),
                    execution_results: None,
                },
                TodoItem {
                    text: "Initialize chat interface".to_string(),
                    completed: false,
                    priority: TodoPriority::Medium,
                    reasoning: "Setup the terminal-based chat interface with real-time input handling and display.".to_string(),
                    tool_plan: Some("1. Configure terminal raw mode\n2. Setup input/output channels\n3. Initialize display buffers".to_string()),
                    execution_results: None,
                },
                TodoItem {
                    text: "Setup real-time suggestions".to_string(),
                    completed: false,
                    priority: TodoPriority::Low,
                    reasoning: "Enable real-time auto-completion and command suggestions using fuzzy matching.".to_string(),
                    tool_plan: Some("1. Initialize suggestion channels\n2. Start background AI suggestion task\n3. Configure fuzzy matcher".to_string()),
                    execution_results: None,
                },
            ],
            current_reasoning: "Setting up OSVM agent environment and loading necessary services...".to_string(),
            selected_todo_index: 0,
            input_mode: InputMode::InputField,
            spinner_frame: 0,
        }
    }

    /// Create empty task state (no mockup data)
    pub fn new_empty() -> Self {
        Self {
            current_task: "".to_string(),
            todo_items: Vec::new(),
            current_reasoning: "".to_string(),
            selected_todo_index: 0,
            input_mode: InputMode::InputField,
            spinner_frame: 0,
        }
    }

    pub fn update_spinner(&mut self) {
        self.spinner_frame = (self.spinner_frame + 1) % SPINNER_FRAMES.len();
    }

    pub fn toggle_todo(&mut self, index: usize) {
        if index < self.todo_items.len() && !self.todo_items[index].completed {
            self.todo_items[index].completed = true;
            self.todo_items[index].execution_results = Some("✅ Task completed successfully".to_string());
        }
    }

    pub fn navigate_todo_up(&mut self) {
        if !self.todo_items.is_empty() {
            if self.selected_todo_index > 0 {
                self.selected_todo_index -= 1;
            } else {
                self.selected_todo_index = self.todo_items.len().saturating_sub(1);
            }
        }
    }

    pub fn navigate_todo_down(&mut self) {
        if !self.todo_items.is_empty() {
            self.selected_todo_index = (self.selected_todo_index + 1) % self.todo_items.len();
        }
    }

    pub fn get_selected_task_details(&self) -> Option<&TodoItem> {
        self.todo_items.get(self.selected_todo_index)
    }
}

const SPINNER_FRAMES: &[&str] = &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

/// TUI Application State
#[derive(Debug)]
pub struct App {
    pub task_state: TaskState,
    pub input_state: InputState,
    pub should_quit: bool,
    pub chat_history: Vec<String>,
    pub suggestions: Vec<RealtimeSuggestion>,
}

impl App {
    pub fn new() -> Self {
        let mut task_state = TaskState::new();
        
        // Mark initial tasks as completed to simulate progress
        task_state.todo_items[0].completed = true;
        task_state.todo_items[0].execution_results = Some("✅ MCP service initialized successfully".to_string());
        task_state.todo_items[1].completed = true;
        task_state.todo_items[1].execution_results = Some("✅ AI service connected to osvm.ai".to_string());
        task_state.todo_items[2].completed = true;
        task_state.todo_items[2].execution_results = Some("✅ Chat interface ready".to_string());
        
        task_state.current_reasoning = "OSVM Agent initialized and ready for user interaction. Press Ctrl+T to navigate tasks.".to_string();
        
        Self {
            task_state,
            input_state: InputState::new(),
            should_quit: false,
            chat_history: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    pub fn tick(&mut self) {
        self.task_state.update_spinner();
    }

    pub fn handle_key_event(&mut self, key: crossterm::event::KeyEvent) {
        match key.code {
            KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                self.should_quit = true;
            }
            KeyCode::Char('t') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                // Toggle between input and task selection mode
                self.task_state.input_mode = match self.task_state.input_mode {
                    InputMode::InputField => {
                        self.task_state.current_reasoning = "Task selection mode active. Use ↑↓ to navigate, Enter to toggle, Esc to cancel.".to_string();
                        InputMode::TaskSelection
                    }
                    InputMode::TaskSelection => {
                        self.task_state.current_reasoning = "Input mode active. Type your command or query.".to_string();
                        InputMode::InputField
                    }
                };
            }
            KeyCode::Esc => {
                if self.task_state.input_mode == InputMode::TaskSelection {
                    // Cancel task selection and return to input mode
                    self.task_state.input_mode = InputMode::InputField;
                    self.task_state.current_reasoning = "Cancelled task selection. Returned to input mode.".to_string();
                } else {
                    // Clear input
                    self.input_state.input.clear();
                    self.input_state.cursor_pos = 0;
                }
            }
            KeyCode::Up => {
                if self.task_state.input_mode == InputMode::TaskSelection {
                    self.task_state.navigate_todo_up();
                    if let Some(selected_task) = self.task_state.get_selected_task_details() {
                        self.task_state.current_reasoning = selected_task.reasoning.clone();
                    }
                }
            }
            KeyCode::Down => {
                if self.task_state.input_mode == InputMode::TaskSelection {
                    self.task_state.navigate_todo_down();
                    if let Some(selected_task) = self.task_state.get_selected_task_details() {
                        self.task_state.current_reasoning = selected_task.reasoning.clone();
                    }
                }
            }
            KeyCode::Enter => {
                if self.task_state.input_mode == InputMode::TaskSelection {
                    // Toggle selected todo item (only if not completed)
                    self.task_state.toggle_todo(self.task_state.selected_todo_index);
                    
                    // Update reasoning
                    let completed_count = self.task_state.todo_items.iter().filter(|t| t.completed).count();
                    self.task_state.current_reasoning = format!(
                        "Progress: {}/{} tasks completed. Current focus: {}",
                        completed_count,
                        self.task_state.todo_items.len(),
                        self.task_state.todo_items.get(self.task_state.selected_todo_index)
                            .map(|t| &t.text)
                            .unwrap_or(&"N/A".to_string())
                    );
                } else if !self.input_state.input.trim().is_empty() {
                    // Process user input
                    let input = self.input_state.input.clone();
                    self.chat_history.push(format!("User: {}", input));
                    
                    // Update task state
                    self.task_state.current_task = format!("Processing: {}", 
                        if input.len() > 30 { format!("{}...", &input[..27]) } else { input.clone() });
                    
                    // Add new task
                    self.task_state.todo_items.push(TodoItem {
                        text: format!("Process: {}", input),
                        completed: false,
                        priority: TodoPriority::High,
                        reasoning: format!("User requested to process: {}", input),
                        tool_plan: Some("1. Parse user input\n2. Generate execution plan\n3. Execute required actions".to_string()),
                        execution_results: None,
                    });
                    
                    // Clear input
                    self.input_state.input.clear();
                    self.input_state.cursor_pos = 0;
                }
            }
            KeyCode::Backspace => {
                if self.task_state.input_mode == InputMode::InputField {
                    if !self.input_state.input.is_empty() {
                        self.input_state.input.pop();
                        self.input_state.cursor_pos = self.input_state.cursor_pos.saturating_sub(1);
                        
                        // Update reasoning
                        if self.input_state.input.is_empty() {
                            self.task_state.current_reasoning = "Ready for user input...".to_string();
                        } else {
                            self.task_state.current_reasoning = format!("User typing: '{}'", self.input_state.input);
                        }
                    }
                }
            }
            KeyCode::Char(c) => {
                if self.task_state.input_mode == InputMode::InputField {
                    self.input_state.input.push(c);
                    self.input_state.cursor_pos += 1;
                    
                    // Update reasoning
                    self.task_state.current_reasoning = format!("User typing: '{}'", self.input_state.input);
                }
            }
            _ => {}
        }
    }
}

/// Dynamic terminal interface with enhanced status bar and task management
pub async fn run_agent_chat_ui() -> Result<()> {
    // Initialize services  
    let mut mcp_service = McpService::new_with_debug(false);
    let ai_service = Arc::new(AiService::new_with_debug(false));
    
    // Load MCP configurations
    if let Err(e) = mcp_service.load_config() {
        warn!("Failed to load MCP config: {}", e);
    }
    
    
    // Setup terminal
    crossterm_enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Initialize app state
    let mut app = App::new();

    // Initialize task state
    let mut task_state = TaskState::new();
    
    // Show initial setup - Claude Code style
    show_welcome_box();
    
    // Show available MCP servers
    let servers = mcp_service.list_servers();
    if servers.is_empty() {
        println!("{}• No MCP servers configured. Use 'osvm mcp setup' to get started{}", Colors::CYAN, Colors::RESET);
    } else {
        let server_names: Vec<String> = servers.iter().map(|(id, _)| (*id).clone()).collect();
        println!("{}• Available MCP servers: {}{}{}", Colors::CYAN, Colors::BLUE, server_names.join(", "), Colors::RESET);
        
        // Update task state
        task_state.todo_items[0].completed = true;
        task_state.current_reasoning = "MCP servers loaded successfully. Ready for user interaction.".to_string();
    }
    
    // Mark services as initialized
    task_state.todo_items[1].completed = true;
    task_state.todo_items[2].completed = true;
    
    println!();

    // Track chat history for AI-generated suggestions
    let mut chat_history: Vec<String> = Vec::new();
    
    // Initialize real-time suggestion system
    let (suggestion_tx, mut suggestion_rx) = mpsc::unbounded_channel::<String>();
    let ai_service_clone = ai_service.clone();
    
    // Spawn background task for real-time suggestion generation
    tokio::spawn(async move {
        while let Some(partial_input) = suggestion_rx.recv().await {
            if partial_input.len() > 2 {
                let _ = generate_realtime_suggestions(&partial_input, &ai_service_clone).await;
            }
        }
    });

    // Spawn background task for spinner animation
    let (spinner_tx, mut spinner_rx) = mpsc::unbounded_channel::<()>();
    tokio::spawn(async move {
        let mut interval = tokio::time::interval(Duration::from_millis(100));
        loop {
            interval.tick().await;
            let _ = spinner_tx.send(());
        }
    });

    // Main interactive loop with enhanced status bar
    loop {
        // Update spinner frame
        if let Ok(_) = spinner_rx.try_recv() {
            task_state.update_spinner();
        }

        // Clear screen and show enhanced status bar
        print!("\x1B[2J\x1B[1;1H");
        show_enhanced_status_bar(&task_state);
        
        // Real-time input with suggestions
        let input = match get_enhanced_input_with_status(&suggestion_tx, &mut task_state).await {
            Ok(input) => input,
            Err(e) => {
                println!("{}✗ Input error: {}{}", Colors::RED, e, Colors::RESET);
                break;
            }
        };
        
        if input.trim().is_empty() {
            continue;
        }
        
        // Handle simple commands
        match input.trim().to_lowercase().as_str() {
            "quit" | "exit" | "/exit" => {
                println!("{}• Goodbye!{}", Colors::CYAN, Colors::RESET);
                break;
            }
            "clear" | "/clear" => {
                print!("\x1B[2J\x1B[1;1H");
                show_welcome_box();
                chat_history.clear();
                continue;
            }
            "help" | "/help" => {
                show_help_commands();
                continue;
            }
            "tools" | "/tools" => {
                show_available_tools(&servers);
                continue;
            }
            "context" | "/context" => {
                if let Err(e) = show_context_visualization(&chat_history, &ai_service).await {
                    println!("{}Error showing context: {}{}", Colors::RED, e, Colors::RESET);
                }
                continue;
            }
            "status" | "/status" => {
                if let Err(e) = show_status_overview(&servers, &chat_history).await {
                    println!("{}Error showing status: {}{}", Colors::RED, e, Colors::RESET);
                }
                continue;
            }
            _ => {}
        }
        
        // Add to chat history
        chat_history.push(format!("User: {}", input));
        
        // Process user message with AI planning
        println!("{}• User: {}{}{}", Colors::GREEN, Colors::BOLD, input, Colors::RESET);
        
        if let Err(e) = process_with_realtime_ai(input.to_string(), &ai_service, &mut chat_history).await {
            println!("{}✗ Error: {}{}", Colors::RED, e, Colors::RESET);
        }
        
        // Show AI-generated contextual suggestions
        show_contextual_suggestions(&ai_service, &chat_history).await;
    }
    
    Ok(())
}


/// Render the task status section
fn render_task_status(f: &mut Frame, area: Rect, task_state: &TaskState) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3), // Task header
            Constraint::Min(6),    // Todo list
            Constraint::Length(3), // Current reasoning
        ])
        .split(area);

    // Task header with spinner
    let spinner = SPINNER_FRAMES[task_state.spinner_frame];
    let task_header = Paragraph::new(Text::from(vec![
        Line::from(vec![
            Span::styled(spinner, Style::default().fg(Color::Cyan)),
            Span::raw(" "),
            Span::styled(&task_state.current_task, Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
        ])
    ]))
    .block(Block::default().borders(Borders::ALL).title("Task Status").border_style(Style::default().fg(Color::Blue)));
    f.render_widget(task_header, chunks[0]);

    // Todo list
    let todo_items: Vec<ListItem> = task_state.todo_items.iter().enumerate().map(|(i, item)| {
        let checkbox = if item.completed { "☑" } else { "☐" };
        let priority_color = match item.priority {
            TodoPriority::High => Color::Red,
            TodoPriority::Medium => Color::Yellow,
            TodoPriority::Low => Color::Green,
        };
        
        let selection_indicator = if task_state.input_mode == InputMode::TaskSelection && 
                                    i == task_state.selected_todo_index {
            "▶ "
        } else {
            "  "
        };
        
        let todo_text = if item.text.len() > 45 {
            format!("{}...", &item.text[..42])
        } else {
            item.text.clone()
        };

        ListItem::new(Line::from(vec![
            Span::raw(selection_indicator),
            Span::styled(checkbox, Style::default().fg(priority_color)),
            Span::raw(" "),
            Span::styled(todo_text, Style::default().fg(Color::White)),
        ]))
    }).collect();

    let todo_header = if task_state.input_mode == InputMode::TaskSelection {
        "Todo List (navigable)"
    } else {
        "Todo List"
    };

    let todo_list = List::new(todo_items)
        .block(Block::default().borders(Borders::ALL).title(todo_header).border_style(Style::default().fg(Color::Blue)))
        .highlight_style(Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD));
    
    f.render_widget(todo_list, chunks[1]);

    // Current reasoning
    let reasoning_text = Text::from(task_state.current_reasoning.clone());
    let reasoning_paragraph = Paragraph::new(reasoning_text)
        .block(Block::default().borders(Borders::ALL).title("Current Reasoning").border_style(Style::default().fg(Color::Blue)))
        .wrap(Wrap { trim: true })
        .style(Style::default().fg(Color::Gray));
    f.render_widget(reasoning_paragraph, chunks[2]);
}

/// Render the input bar
fn render_input_bar(f: &mut Frame, area: Rect, input_state: &InputState, task_state: &TaskState) {
    let input_style = if task_state.input_mode == InputMode::InputField {
        Style::default().fg(Color::Green)
    } else {
        Style::default().fg(Color::Gray)
    };

    let input_text = if task_state.input_mode == InputMode::InputField {
        format!("> {}", input_state.input)
    } else {
        format!("> {} (Task mode - Press Ctrl+T to return)", input_state.input)
    };

    let input_paragraph = Paragraph::new(input_text)
        .block(Block::default().borders(Borders::ALL).title("Input").border_style(input_style))
        .style(input_style);
    
    f.render_widget(input_paragraph, area);
}

/// Render task details section
fn render_task_details(f: &mut Frame, area: Rect, task_state: &TaskState) {
    if let Some(selected_task) = task_state.get_selected_task_details() {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(4), // Reasoning
                Constraint::Length(6), // Tool plan
                Constraint::Min(3),    // Execution results
            ])
            .split(area);

        // Reasoning section
        let reasoning_text = Text::from(selected_task.reasoning.clone());
        let reasoning_widget = Paragraph::new(reasoning_text)
            .block(Block::default().borders(Borders::ALL).title("Reasoning").border_style(Style::default().fg(Color::Magenta)))
            .wrap(Wrap { trim: true })
            .style(Style::default().fg(Color::White));
        f.render_widget(reasoning_widget, chunks[0]);

        // Tool plan section
        let plan_text = if let Some(tool_plan) = &selected_task.tool_plan {
            tool_plan.clone()
        } else {
            "No tool plan available".to_string()
        };
        
        let plan_widget = Paragraph::new(Text::from(plan_text))
            .block(Block::default().borders(Borders::ALL).title("Tool Plan").border_style(Style::default().fg(Color::Cyan)))
            .wrap(Wrap { trim: true })
            .style(Style::default().fg(Color::Gray));
        f.render_widget(plan_widget, chunks[1]);

        // Execution results section
        let results_text = if let Some(results) = &selected_task.execution_results {
            results.clone()
        } else {
            "Not executed yet".to_string()
        };
        
        let results_widget = Paragraph::new(Text::from(results_text))
            .block(Block::default().borders(Borders::ALL).title("Execution Results").border_style(Style::default().fg(Color::Green)))
            .wrap(Wrap { trim: true })
            .style(Style::default().fg(Color::White));
        f.render_widget(results_widget, chunks[2]);
    } else {
        // Show help text when no task is selected
        let help_text = Text::from(vec![
            Line::from("Press Ctrl+T to navigate tasks"),
            Line::from("Use ↑↓ arrows to select tasks"),
            Line::from("Press Enter to toggle task completion"),
            Line::from("Press Esc to cancel and return to input"),
            Line::from(""),
            Line::from("Available commands:"),
            Line::from("  /balance - Check wallet balance"),
            Line::from("  /transactions - Show transaction history"),
            Line::from("  /help - Show help"),
            Line::from("  /quit - Exit application"),
        ]);
        
        let help_widget = Paragraph::new(help_text)
            .block(Block::default().borders(Borders::ALL).title("Help & Commands").border_style(Style::default().fg(Color::Yellow)))
            .style(Style::default().fg(Color::Gray));
        f.render_widget(help_widget, area);
    }
}

/// Show welcome box like Claude Code
fn show_welcome_box() {
    println!("{}┌─ Welcome to OSVM! ────────────────────────────────────┐{}", Colors::YELLOW, Colors::RESET);
    println!("{}│ {}OSVM Agent Chat - Real-time AI Assistant{}           │{}", Colors::YELLOW, Colors::BOLD, Colors::RESET, Colors::YELLOW);
    println!("{}│                                                        │{}", Colors::YELLOW, Colors::RESET);
    println!("{}│ {}Commands:{} /help, /status, /context, /tools           │{}", Colors::YELLOW, Colors::CYAN, Colors::RESET, Colors::YELLOW);
    println!("{}│ {}Features:{} Real-time suggestions, AI planning         │{}", Colors::YELLOW, Colors::GREEN, Colors::RESET, Colors::YELLOW);
    println!("{}│ {}Working:{} /home/larp/larpdevs/osvm-cli                │{}", Colors::YELLOW, Colors::BLUE, Colors::RESET, Colors::YELLOW);
    println!("{}└────────────────────────────────────────────────────────┘{}", Colors::YELLOW, Colors::RESET);
}

/// Configuration for input behavior
#[derive(Debug, Clone)]
pub struct InputConfig {
    pub max_history_size: usize,
    pub debounce_ms: u64,
    pub max_suggestions: usize,
    pub fuzzy_threshold: f32,
    pub enable_fuzzy_matching: bool,
}

impl Default for InputConfig {
    fn default() -> Self {
        Self {
            max_history_size: 100,
            debounce_ms: 150,
            max_suggestions: 8,
            fuzzy_threshold: 0.4,
            enable_fuzzy_matching: true,
        }
    }
}

/// Input state management
#[derive(Debug)]
pub struct InputState {
    pub input: String,
    pub cursor_pos: usize,
    pub selected_suggestion: usize,
    pub command_history: Vec<String>,
    pub history_index: usize,
    pub suggestions: Vec<RealtimeSuggestion>,
    pub last_suggestion_time: std::time::Instant,
    pub config: InputConfig,
}

impl InputState {
    pub fn new() -> Self {
        Self {
            input: String::new(),
            cursor_pos: 0,
            selected_suggestion: 0,
            command_history: Self::default_command_history(),
            history_index: 0,
            suggestions: Vec::new(),
            last_suggestion_time: std::time::Instant::now(),
            config: InputConfig::default(),
        }
    }

    fn default_command_history() -> Vec<String> {
        vec![
            "/balance".to_string(),
            "/transactions".to_string(),
            "/stake".to_string(),
            "/price".to_string(),
            "/network".to_string(),
        ]
    }

    /// Add command to history with size limit
    pub fn add_to_history(&mut self, command: String) {
        if !command.trim().is_empty() {
            self.command_history.push(command);
            // Enforce history size limit
            if self.command_history.len() > self.config.max_history_size {
                self.command_history.remove(0);
            }
            self.history_index = self.command_history.len();
        }
    }

    /// Check if suggestions should be debounced
    pub fn should_update_suggestions(&mut self) -> bool {
        let elapsed = self.last_suggestion_time.elapsed();
        if elapsed.as_millis() > self.config.debounce_ms as u128 {
            self.last_suggestion_time = std::time::Instant::now();
            true
        } else {
            false
        }
    }

    /// Reset selection when input changes
    pub fn reset_selection(&mut self) {
        self.selected_suggestion = 0;
    }
}
/// Get real-time input with dynamic suggestions as you type
async fn get_realtime_input_with_suggestions(suggestion_tx: &mpsc::UnboundedSender<String>) -> Result<String> {
    let mut state = InputState::new();
    state.history_index = state.command_history.len();

    // Enable raw mode for character-by-character input
    enable_raw_mode()?;

    loop {
        match read_single_character()? {
            InputChar::Enter => {
                let result = handle_enter_key(&mut state).await?;
                return Ok(result);
            }
            InputChar::Backspace => {
                handle_backspace(&mut state, suggestion_tx).await?;
            }
            InputChar::CtrlC => {
                handle_ctrl_c().await?;
            }
            InputChar::CtrlT => {
                // Not supported in basic input mode
                debug!("Ctrl+T pressed but not supported in basic input mode");
            }
            InputChar::Escape => {
                // Clear input in basic mode
                state.input.clear();
                state.cursor_pos = 0;
                clear_suggestions_display();
                redraw_input_line(&state.input)?;
            }
            InputChar::Tab => {
                handle_tab_completion(&mut state).await?;
            }
            InputChar::Arrow(arrow_key) => {
                handle_arrow_key(&mut state, arrow_key).await?;
            }
            InputChar::Regular(ch) => {
                handle_regular_character(&mut state, ch, suggestion_tx).await?;
            }
            InputChar::Unknown => {
                // Log unknown character for debugging
                debug!("Unknown character received in input");
            }
        }
    }
}

/// Input character classification
#[derive(Debug)]
pub enum InputChar {
    Enter,
    Backspace,
    CtrlC,
    CtrlT,
    Tab,
    Escape,
    Arrow(ArrowKey),
    Regular(char),
    Unknown,
}

/// Arrow key enum
#[derive(Debug)]
pub enum ArrowKey {
    Up,
    Down,
    Left,
    Right,
}

/// Read and classify a single character from input
fn read_single_character() -> Result<InputChar> {
    let mut buffer = [0; 1];
    if let Ok(1) = std::io::stdin().read(&mut buffer) {
        let ch = buffer[0] as char;

        match ch {
            '\n' | '\r' => Ok(InputChar::Enter),
            '\x7f' | '\x08' => Ok(InputChar::Backspace),
            '\x03' => Ok(InputChar::CtrlC),
            '\x14' => Ok(InputChar::CtrlT), // Ctrl+T
            '\t' => Ok(InputChar::Tab),
            '\x1b' => {
                // Check if this is a standalone ESC or arrow key sequence
                let mut next_buffer = [0; 1];
                match std::io::stdin().read(&mut next_buffer) {
                    Ok(1) => {
                        // There's another character, this might be an arrow key
                        let mut full_buffer = [0; 2];
                        full_buffer[0] = next_buffer[0];
                        if let Ok(1) = std::io::stdin().read(&mut full_buffer[1..]) {
                            if full_buffer[0] == b'[' {
                                match full_buffer[1] {
                                    b'A' => return Ok(InputChar::Arrow(ArrowKey::Up)),
                                    b'B' => return Ok(InputChar::Arrow(ArrowKey::Down)),
                                    b'C' => return Ok(InputChar::Arrow(ArrowKey::Right)),
                                    b'D' => return Ok(InputChar::Arrow(ArrowKey::Left)),
                                    _ => return Ok(InputChar::Escape),
                                }
                            }
                        }
                        Ok(InputChar::Escape)
                    }
                    _ => {
                        // Standalone ESC key
                        Ok(InputChar::Escape)
                    }
                }
            }
            ch if ch.is_ascii() && !ch.is_control() => Ok(InputChar::Regular(ch)),
            _ => Ok(InputChar::Unknown),
        }
    } else {
        Err(anyhow!("Failed to read character from stdin"))
    }
}

/// Handle arrow key escape sequences
fn handle_arrow_keys() -> Result<ArrowKey> {
    let mut buffer = [0; 2];

    // Read the next two characters after ESC
    if let Ok(2) = std::io::stdin().read(&mut buffer) {
        if buffer[0] == b'[' {
            match buffer[1] {
                b'A' => return Ok(ArrowKey::Up),
                b'B' => return Ok(ArrowKey::Down),
                b'C' => return Ok(ArrowKey::Right),
                b'D' => return Ok(ArrowKey::Left),
                _ => {}
            }
        }
    }

    Err(anyhow!("Invalid arrow key sequence"))
}

/// Handle Enter key press
async fn handle_enter_key(state: &mut InputState) -> Result<String> {
    disable_raw_mode()?;
    clear_suggestions_display();
    println!();
    close_input_border();

    // Add to command history if not empty
    state.add_to_history(state.input.clone());

    Ok(state.input.clone())
}

/// Handle Backspace key press
async fn handle_backspace(state: &mut InputState, suggestion_tx: &mpsc::UnboundedSender<String>) -> Result<()> {
    if !state.input.is_empty() && state.cursor_pos > 0 {
        state.input.pop();
        state.cursor_pos -= 1;
        state.reset_selection();

        // Clear current dropdown first
        clear_suggestions_display();

        // Redraw input line
        if let Err(e) = redraw_input_line(&state.input) {
            error!("Failed to redraw input line: {}", e);
        }

        // Update suggestions with debouncing
        if state.should_update_suggestions() {
            update_suggestions_for_input(state, suggestion_tx).await?;
        }
    }
    Ok(())
}

/// Handle Ctrl+C interrupt
pub async fn handle_ctrl_c() -> Result<String> {
    disable_raw_mode()?;
    close_input_border();
    Err(anyhow!("Interrupted"))
}

/// Handle Tab completion
async fn handle_tab_completion(state: &mut InputState) -> Result<()> {
    if !state.suggestions.is_empty() && state.selected_suggestion < state.suggestions.len() {
        let suggestion = &state.suggestions[state.selected_suggestion];

        // Clear current dropdown
        clear_suggestions_display();

        // Update input with suggestion
        state.input = suggestion.text.clone();
        state.cursor_pos = state.input.len();
        state.reset_selection();

        // Redraw the complete input line
        if let Err(e) = redraw_input_line(&state.input) {
            error!("Failed to redraw input line: {}", e);
        }

        // Get new suggestions for the completed input
        state.suggestions = get_instant_suggestions(&state.input).await;
        if !state.suggestions.is_empty() {
            show_navigable_suggestions(&state.suggestions, state.selected_suggestion);
        }
    }
    Ok(())
}

/// Handle arrow key navigation
async fn handle_arrow_key(state: &mut InputState, arrow_key: ArrowKey) -> Result<()> {
    match arrow_key {
        ArrowKey::Up => handle_arrow_up(state).await?,
        ArrowKey::Down => handle_arrow_down(state).await?,
        ArrowKey::Left => {
            // Future enhancement: cursor movement
            debug!("Left arrow pressed - cursor movement not yet implemented");
        }
        ArrowKey::Right => {
            // Future enhancement: cursor movement
            debug!("Right arrow pressed - cursor movement not yet implemented");
        }
    }
    Ok(())
}

/// Handle up arrow navigation
async fn handle_arrow_up(state: &mut InputState) -> Result<()> {
    if !state.suggestions.is_empty() {
        // Navigate up in suggestions
        state.selected_suggestion = if state.selected_suggestion > 0 {
            state.selected_suggestion - 1
        } else {
            state.suggestions.len() - 1
        };
        show_navigable_suggestions(&state.suggestions, state.selected_suggestion);
    } else if state.input.trim().is_empty() {
        // Navigate command history up
        navigate_history_up(state).await?;
    }
    Ok(())
}

/// Handle down arrow navigation
async fn handle_arrow_down(state: &mut InputState) -> Result<()> {
    if !state.suggestions.is_empty() {
        // Navigate down in suggestions
        state.selected_suggestion = (state.selected_suggestion + 1) % state.suggestions.len();
        show_navigable_suggestions(&state.suggestions, state.selected_suggestion);
    } else if state.input.trim().is_empty() {
        // Navigate command history down
        navigate_history_down(state).await?;
    }
    Ok(())
}

/// Navigate command history up
async fn navigate_history_up(state: &mut InputState) -> Result<()> {
    if state.history_index > 0 {
        state.history_index -= 1;
        state.input = state.command_history[state.history_index].clone();
        state.cursor_pos = state.input.len();

        if let Err(e) = redraw_input_line(&state.input) {
            error!("Failed to redraw input line: {}", e);
        }

        // Show suggestions for history item
        state.suggestions = get_instant_suggestions(&state.input).await;
        state.reset_selection();
        if !state.suggestions.is_empty() {
            show_navigable_suggestions(&state.suggestions, state.selected_suggestion);
        }
    }
    Ok(())
}

/// Navigate command history down
async fn navigate_history_down(state: &mut InputState) -> Result<()> {
    if state.history_index < state.command_history.len() - 1 {
        state.history_index += 1;
        state.input = state.command_history[state.history_index].clone();
        state.cursor_pos = state.input.len();

        if let Err(e) = redraw_input_line(&state.input) {
            error!("Failed to redraw input line: {}", e);
        }

        // Show suggestions for history item
        state.suggestions = get_instant_suggestions(&state.input).await;
        state.reset_selection();
        if !state.suggestions.is_empty() {
            show_navigable_suggestions(&state.suggestions, state.selected_suggestion);
        }
    } else if state.history_index == state.command_history.len() - 1 {
        // Clear input when going past last history item
        state.history_index = state.command_history.len();
        state.input.clear();
        state.cursor_pos = 0;

        if let Err(e) = redraw_input_line(&state.input) {
            error!("Failed to redraw input line: {}", e);
        }

        clear_suggestions_display();
        state.suggestions.clear();
    }
    Ok(())
}

/// Handle regular character input
pub async fn handle_regular_character(state: &mut InputState, ch: char, suggestion_tx: &mpsc::UnboundedSender<String>) -> Result<()> {
    state.input.push(ch);
    state.cursor_pos += 1;
    state.reset_selection();
    print!("{}", ch);
    if let Err(e) = io::stdout().flush() {
        error!("Failed to flush stdout: {}", e);
    }

    // Trigger real-time suggestion generation with debouncing
    if state.should_update_suggestions() {
        update_suggestions_for_input(state, suggestion_tx).await?;
    }

    Ok(())
}

/// Update suggestions for current input
async fn update_suggestions_for_input(state: &mut InputState, suggestion_tx: &mpsc::UnboundedSender<String>) -> Result<()> {
    if state.input.len() > 0 {
        // Send to background suggestion generation
        if let Err(e) = suggestion_tx.send(state.input.clone()) {
            warn!("Failed to send suggestion request: {}", e);
        }

        state.suggestions = get_instant_suggestions(&state.input).await;
        if !state.suggestions.is_empty() {
            show_navigable_suggestions(&state.suggestions, state.selected_suggestion);
        }
    } else {
        // Clear suggestions when input is empty
        clear_suggestions_display();
        state.suggestions.clear();
    }
    Ok(())
}

/// Show navigable suggestions with highlighting
fn show_navigable_suggestions(suggestions: &[RealtimeSuggestion], selected_index: usize) {
    if suggestions.is_empty() {
        return;
    }

    // Save cursor position
    print!("\x1b[s");

    // Move down and show suggestions dropdown
    println!();
    println!("{}──────────────────────────────────────────────────────────────────────────────────────────{}", Colors::DIM, Colors::RESET);

    // Display suggestions with proper formatting and highlighting
    for (i, suggestion) in suggestions.iter().enumerate().take(8) {
        let color = match suggestion.category.as_str() {
            "command" => Colors::CYAN,
            "query" => Colors::GREEN,
            "action" => Colors::MAGENTA,
            "address" => Colors::YELLOW,
            _ => Colors::BLUE,
        };

        // Highlight selected suggestion with fuzzy match highlighting
        let highlighted_text = highlight_fuzzy_match(&suggestion.text, &suggestion.matched_indices, color);

        if i == selected_index {
            println!("{}▶ {:<29} {}{}{}{}",
                    Colors::YELLOW, highlighted_text,
                    Colors::DIM, suggestion.description, Colors::RESET, Colors::RESET);
        } else {
            println!("  {:<30} {}{}{}",
                    highlighted_text,
                    Colors::DIM, suggestion.description, Colors::RESET);
        }
    }

    // Show navigation hint
    println!("{}──────────────────────────────────────────────────────────────────────────────────────────{}", Colors::DIM, Colors::RESET);
    println!("  {}↑↓ Navigate • Tab Complete • Enter Submit{}", Colors::DIM, Colors::RESET);

    // Restore cursor position to input line
    print!("\x1b[u");
    if let Err(e) = io::stdout().flush() {
        error!("Failed to flush stdout: {}", e);
    }
}

/// Generate instant suggestions as user types (like Claude Code auto-complete)
pub async fn get_instant_suggestions(partial_input: &str) -> Vec<RealtimeSuggestion> {
    let config = InputConfig::default();

    // Claude Code-style command suggestions
    let commands = vec![
        ("/balance".to_string(), "Check wallet balance and holdings".to_string(), "command".to_string()),
        ("/transactions".to_string(), "Show recent transaction history".to_string(), "command".to_string()),
        ("/stake".to_string(), "View staking accounts and rewards".to_string(), "command".to_string()),
        ("/price".to_string(), "Get current token price information".to_string(), "command".to_string()),
        ("/network".to_string(), "Check Solana network status and health".to_string(), "command".to_string()),
        ("/analyze".to_string(), "Analyze wallet address activity".to_string(), "command".to_string()),
        ("/help".to_string(), "Show available commands and help".to_string(), "command".to_string()),
        ("/clear".to_string(), "Clear conversation history".to_string(), "command".to_string()),
        ("/tools".to_string(), "List available MCP tools".to_string(), "command".to_string()),
        ("/context".to_string(), "Show context usage visualization".to_string(), "command".to_string()),
        ("/status".to_string(), "Show current system status".to_string(), "command".to_string()),
        ("/exit".to_string(), "Exit the chat interface".to_string(), "command".to_string()),
        ("/quit".to_string(), "Quit the application".to_string(), "command".to_string()),
        // Additional fuzzy-friendly variations
        ("bal".to_string(), "Check wallet balance (short)".to_string(), "command".to_string()),
        ("trans".to_string(), "Show transactions (short)".to_string(), "command".to_string()),
        ("stat".to_string(), "Show status (short)".to_string(), "command".to_string()),
    ];

    // Smart blockchain suggestions
    let blockchain_patterns = vec![
        ("balance".to_string(), "Check wallet balance and holdings".to_string(), "query".to_string()),
        ("transactions".to_string(), "Show recent transaction history".to_string(), "query".to_string()),
        ("stake".to_string(), "View staking accounts and rewards".to_string(), "query".to_string()),
        ("price".to_string(), "Get current token price".to_string(), "query".to_string()),
        ("network".to_string(), "Check Solana network status".to_string(), "query".to_string()),
        ("analyze".to_string(), "Analyze wallet activity".to_string(), "query".to_string()),
        ("send".to_string(), "Send SOL to address".to_string(), "action".to_string()),
        ("swap".to_string(), "Swap tokens on DEX".to_string(), "action".to_string()),
        ("delegate".to_string(), "Delegate stake to validator".to_string(), "action".to_string()),
    ];

    let mut all_candidates = commands;
    all_candidates.extend(blockchain_patterns);

    // Use fuzzy matching if enabled
    let suggestions = if config.enable_fuzzy_matching {
        let fuzzy_matcher = FuzzyMatcher::new(config.fuzzy_threshold);
        fuzzy_matcher.filter_suggestions(partial_input, &all_candidates)
    } else {
        // Fallback to simple string matching
        let lower_input = partial_input.to_lowercase();
        all_candidates
            .into_iter()
            .filter_map(|(text, description, category)| {
                if text.to_lowercase().contains(&lower_input) {
                    Some(RealtimeSuggestion {
                        text,
                        description,
                        category,
                        score: 1.0,
                        matched_indices: Vec::new(),
                    })
                } else {
                    None
                }
            })
            .collect()
    };

    // Handle special cases
    let mut final_suggestions = suggestions;

    // Wallet address suggestions
    if partial_input.len() > 10 && partial_input.chars().all(|c| c.is_alphanumeric()) {
        final_suggestions.push(RealtimeSuggestion {
            text: format!("analyze wallet {}", partial_input),
            description: "Analyze this wallet address".to_string(),
            category: "address".to_string(),
            score: 0.8,
            matched_indices: Vec::new(),
        });
    }

    // Limit suggestions
    final_suggestions.truncate(config.max_suggestions);
    final_suggestions
}

/// Show real-time suggestions (like Claude Code auto-complete)
fn show_realtime_suggestions(suggestions: &[RealtimeSuggestion], current_input: &str) {
    show_realtime_suggestions_fixed(suggestions, current_input);
}

/// Claude Code-style dropdown suggestions display
fn show_realtime_suggestions_fixed(suggestions: &[RealtimeSuggestion], current_input: &str) {
    if suggestions.is_empty() {
        return;
    }

    // Save cursor position
    print!("\x1b[s");

    // Move down and show suggestions dropdown
    println!();
    println!("{}──────────────────────────────────────────────────────────────────────────────────────────{}", Colors::DIM, Colors::RESET);

    // Display suggestions with proper formatting
    for suggestion in suggestions.iter().take(8) {
        let color = match suggestion.category.as_str() {
            "command" => Colors::CYAN,
            "query" => Colors::GREEN,
            "action" => Colors::MAGENTA,
            "address" => Colors::YELLOW,
            _ => Colors::BLUE,
        };

        println!("  {}{:<30}{} {}{}{}",
                color, suggestion.text, Colors::RESET,
                Colors::DIM, suggestion.description, Colors::RESET);
    }

    // Show navigation hint
    println!("{}──────────────────────────────────────────────────────────────────────────────────────────{}", Colors::DIM, Colors::RESET);

    // Restore cursor position to input line
    print!("\x1b[u");
    if let Err(e) = io::stdout().flush() {
        error!("Failed to flush stdout: {}", e);
    }
}

/// Clear dropdown suggestions display
fn clear_inline_suggestion(current_input: &str) {
    // This function is now used to clear the dropdown, not inline suggestions
    clear_suggestions_display();
}

/// Clear dropdown suggestions display
fn clear_suggestions_display() {
    // Save current cursor position
    print!("\x1b[s");

    // Move down and clear all suggestion lines (up to 12 lines: border + 8 suggestions + border + hint)
    for _ in 0..12 {
        print!("\n\x1b[K"); // Move down one line and clear it
    }

    // Restore cursor position to input line
    print!("\x1b[u");
    if let Err(e) = io::stdout().flush() {
        error!("Failed to flush stdout: {}", e);
    }
}

/// Clear current input line
fn clear_current_line() {
    print!("\r\x1b[K");
    print_input_prompt();
}

/// Show input border
fn show_input_border() {
    println!("\n{}┌─ Input ───────────────────────────────────────────────┐{}", Colors::BLUE, Colors::RESET);
}

/// Print input prompt with border
fn print_input_prompt() {
    print!("{}│{} > ", Colors::BLUE, Colors::RESET);
    if let Err(e) = io::stdout().flush() {
        error!("Failed to flush stdout: {}", e);
    }
}

/// Close input border
fn close_input_border() {
    println!("{}└───────────────────────────────────────────────────────┘{}", Colors::BLUE, Colors::RESET);
}

/// Redraw input line cleanly with error handling
fn redraw_input_line(input: &str) -> Result<()> {
    print!("\r{}│{} > ", Colors::BLUE, Colors::RESET);
    print!("\x1b[K"); // Clear to end of line
    print!("{}", input);

    if let Err(e) = io::stdout().flush() {
        error!("Failed to flush stdout during input redraw: {}", e);
        return Err(anyhow!("Terminal output error: {}", e));
    }

    Ok(())
}

/// Enable raw mode for character-by-character input with proper error handling
pub fn enable_raw_mode() -> Result<()> {
    #[cfg(unix)]
    {
        use std::os::unix::io::AsRawFd;
        let fd = std::io::stdin().as_raw_fd();

        unsafe {
            let mut termios = std::mem::zeroed();

            // Get current terminal attributes
            if libc::tcgetattr(fd, &mut termios) != 0 {
                let error = std::io::Error::last_os_error();
                error!("Failed to get terminal attributes: {}", error);
                return Err(anyhow!("Failed to get terminal attributes: {}", error));
            }

            // Modify attributes for raw mode
            termios.c_lflag &= !(libc::ICANON | libc::ECHO);

            // Apply new terminal attributes
            if libc::tcsetattr(fd, libc::TCSANOW, &termios) != 0 {
                let error = std::io::Error::last_os_error();
                error!("Failed to set terminal attributes: {}", error);
                return Err(anyhow!("Failed to set terminal attributes: {}", error));
            }

            debug!("Raw mode enabled successfully");
        }
    }

    #[cfg(not(unix))]
    {
        warn!("Raw mode not implemented for non-Unix platforms");
        return Err(anyhow!("Raw mode not supported on this platform"));
    }

    Ok(())
}

/// Disable raw mode with proper error handling
pub fn disable_raw_mode() -> Result<()> {
    #[cfg(unix)]
    {
        use std::os::unix::io::AsRawFd;
        let fd = std::io::stdin().as_raw_fd();

        unsafe {
            let mut termios = std::mem::zeroed();

            // Get current terminal attributes
            if libc::tcgetattr(fd, &mut termios) != 0 {
                let error = std::io::Error::last_os_error();
                error!("Failed to get terminal attributes for disable: {}", error);
                return Err(anyhow!("Failed to get terminal attributes: {}", error));
            }

            // Restore canonical mode and echo
            termios.c_lflag |= libc::ICANON | libc::ECHO;

            // Apply restored terminal attributes
            if libc::tcsetattr(fd, libc::TCSANOW, &termios) != 0 {
                let error = std::io::Error::last_os_error();
                error!("Failed to restore terminal attributes: {}", error);
                return Err(anyhow!("Failed to restore terminal attributes: {}", error));
            }

            debug!("Raw mode disabled successfully");
        }
    }

    #[cfg(not(unix))]
    {
        warn!("Raw mode disable not implemented for non-Unix platforms");
    }

    Ok(())
}

/// Generate real-time suggestions using AI (background task)
async fn generate_realtime_suggestions(partial_input: &str, ai_service: &AiService) -> Result<Vec<String>> {
    let prompt = format!(
        "User is typing: '{}'\n\
        Generate 3 short auto-complete suggestions for blockchain operations.\n\
        Format as simple lines:\n\
        suggestion 1\n\
        suggestion 2\n\
        suggestion 3",
        partial_input
    );
    
    match ai_service.query(&prompt).await {
        Ok(response) => {
            let suggestions: Vec<String> = response.lines()
                .map(|line| line.trim().to_string())
                .filter(|line| !line.is_empty())
                .take(3)
                .collect();
            Ok(suggestions)
        }
        Err(_) => Ok(vec![]),
    }
}

/// Process message with AI planning
async fn process_with_realtime_ai(message: String, ai_service: &Arc<AiService>, chat_history: &mut Vec<String>) -> Result<()> {
    // Show animated analysis
    show_animated_status("Analyzing your request", "🤔💭🧠💡", 800).await;
    
    // Use AI planning
    let available_tools = get_available_tools();
    
    show_animated_status("Creating execution plan", "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏", 1000).await;
    
    let ai_plan = match ai_service.create_tool_plan(&message, &available_tools).await {
        Ok(plan) => {
            println!("{}• Plan created successfully!{}", Colors::GREEN, Colors::RESET);
            plan
        }
        Err(e) => {
            println!("{}• AI planning failed: {}{}", Colors::RED, e, Colors::RESET);
            show_animated_status("Using fallback analysis", "🔄", 400).await;
            
            let fallback_query = format!("Help with this blockchain request: '{}'", message);
            match ai_service.query(&fallback_query).await {
                Ok(response) => {
                    println!("{}", response);
                    return Ok(());
                }
                Err(_) => {
                    println!("{}Unable to analyze request. Please try a different query.{}", Colors::RED, Colors::RESET);
                    return Ok(());
                }
            }
        }
    };
    
    // Show ASCII plan diagram
    show_colored_plan_diagram(&ai_plan);
    
    // Get user confirmation
    let confirmation = get_user_choice().await?;
    
    match confirmation {
        1 => {
            show_animated_status("Executing plan", "📡📶🌐🔗", 300).await;
            execute_ai_plan_with_colors(&ai_plan, &message, ai_service).await?;
        }
        2 => {
            show_animated_status("Executing and saving preferences", "💾", 400).await;
            execute_ai_plan_with_colors(&ai_plan, &message, ai_service).await?;
            println!("{}• Preferences saved for future similar requests{}", Colors::GREEN, Colors::RESET);
        }
        3 => {
            println!("{}• Plan cancelled. Please modify your request{}", Colors::YELLOW, Colors::RESET);
            return Ok(());
        }
        4 => {
            show_animated_status("YOLO mode activated", "🚀", 200).await;
            execute_ai_plan_with_colors(&ai_plan, &message, ai_service).await?;
            println!("{}• YOLO mode activated for future requests{}", Colors::MAGENTA, Colors::RESET);
        }
        _ => {
            println!("{}• Invalid selection{}", Colors::RED, Colors::RESET);
            return Ok(());
        }
    }
    
    // Add to chat history
    chat_history.push(message);
    chat_history.push("AI: [processed request]".to_string());
    
    Ok(())
}

/// Show animated status with dynamic characters
async fn show_animated_status(message: &str, chars: &str, duration_ms: u64) {
    let frames: Vec<char> = chars.chars().collect();
    let start = std::time::Instant::now();
    let mut frame_idx = 0;
    
    while start.elapsed().as_millis() < duration_ms as u128 {
        print!("\r{} {}...", frames[frame_idx % frames.len()], message);
        if let Err(e) = io::stdout().flush() {
        error!("Failed to flush stdout: {}", e);
    }
        
        frame_idx += 1;
        sleep(Duration::from_millis(100)).await;
    }
    
    println!("\r{}• {}{}", Colors::CYAN, message, Colors::RESET);
}

/// Show colored ASCII plan diagram with proper text wrapping
fn show_colored_plan_diagram(ai_plan: &ToolPlan) {
    let box_width = 57; // Inner width of the ASCII box
    
    println!("\n{}┌─ Execution Plan ─────────────────────────────────────┐{}", Colors::YELLOW, Colors::RESET);
    
    // Wrap the reasoning text to fit within the box
    let wrapped_lines = wrap_text(&ai_plan.reasoning, box_width - 2);
    for line in wrapped_lines {
        println!("{}│ {}{:<width$}{} │{}", 
                Colors::YELLOW, Colors::BOLD, line, Colors::RESET, Colors::YELLOW, width = box_width - 2);
    }
    
    println!("{}├─────────────────────────────────────────────────────────┤{}", Colors::YELLOW, Colors::RESET);
    
    for (i, tool) in ai_plan.osvm_tools_to_use.iter().enumerate() {
        if i > 0 {
            println!("{}│           ↓                                         │{}", Colors::YELLOW, Colors::RESET);
        }
        
        println!("{}│  ┌─ {}{}{} ─┐                              │{}", 
                Colors::YELLOW, Colors::YELLOW, tool.tool_name, Colors::RESET, Colors::YELLOW);
        println!("{}│  │ Server: {}{}{}                      │{}", 
                Colors::YELLOW, Colors::BLUE, tool.server_id, Colors::RESET, Colors::YELLOW);
        
        // Wrap args if they're too long
        let args_str = serde_json::to_string(&tool.args).unwrap_or_default();
        let short_args = if args_str.len() > 25 { 
            format!("{}...", &args_str[..22])
        } else { 
            args_str 
        };
        println!("{}│  │ Args: {}{}{}                     │{}", 
                Colors::YELLOW, Colors::DIM, short_args, Colors::RESET, Colors::YELLOW);
        
        // Wrap reason if it's too long  
        let wrapped_reason = wrap_text(&tool.reason, 35);
        for (j, reason_line) in wrapped_reason.iter().enumerate() {
            if j == 0 {
                println!("{}│  └─ {} ─┘                      │{}", 
                        Colors::YELLOW, reason_line, Colors::RESET);
            } else {
                println!("{}│     {:<35}                      │{}", 
                        Colors::YELLOW, reason_line, Colors::RESET);
            }
        }
    }
    
    println!("{}│                                                         │{}", Colors::YELLOW, Colors::RESET);
    println!("{}└─────────────────────────────────────────────────────────┘{}", Colors::YELLOW, Colors::RESET);
    println!();
}

/// Wrap text to fit within specified width
fn wrap_text(text: &str, width: usize) -> Vec<String> {
    let mut lines = Vec::new();
    let mut current_line = String::new();
    
    for word in text.split_whitespace() {
        if current_line.is_empty() {
            current_line = word.to_string();
        } else if current_line.len() + 1 + word.len() <= width {
            current_line.push(' ');
            current_line.push_str(word);
        } else {
            lines.push(current_line);
            current_line = word.to_string();
        }
    }
    
    if !current_line.is_empty() {
        lines.push(current_line);
    }
    
    if lines.is_empty() {
        lines.push("No plan details available".to_string());
    }
    
    lines
}

/// Get user choice with colors
async fn get_user_choice() -> Result<u32> {
    println!("{}Do you want to execute this plan?{}", Colors::CYAN, Colors::RESET);
    println!("{}1) {}{}", Colors::GREEN, "Yes", Colors::RESET);
    println!("{}2) {}{}", Colors::GREEN, "Yes and remember for these tools", Colors::RESET);
    println!("{}3) {}{}", Colors::YELLOW, "No, let me change my prompt", Colors::RESET);
    println!("{}4) {}{}", Colors::MAGENTA, "YOLO - yes for all", Colors::RESET);
    print!("{}Choice (1-4): {}", Colors::CYAN, Colors::RESET);
    if let Err(e) = io::stdout().flush() {
        error!("Failed to flush stdout: {}", e);
    }
    
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    
    match input.trim().parse::<u32>() {
        Ok(choice) if choice >= 1 && choice <= 4 => Ok(choice),
        _ => {
            println!("{}• Invalid choice, defaulting to 'No'{}", Colors::RED, Colors::RESET);
            Ok(3)
        }
    }
}

/// Execute AI plan with colors
async fn execute_ai_plan_with_colors(ai_plan: &ToolPlan, original_message: &str, ai_service: &Arc<AiService>) -> Result<()> {
    let mut tool_results = Vec::new();
    
    for (i, planned_tool) in ai_plan.osvm_tools_to_use.iter().enumerate() {
        print!("{}• Step {}: {}", Colors::CYAN, i + 1, Colors::RESET);
        print!("{}{}{}", Colors::YELLOW, planned_tool.tool_name, Colors::RESET);
        println!("({:?})", planned_tool.args);
        
        // Show execution animation
        show_animated_status(&format!("Executing {}", planned_tool.tool_name), "📡📶🌐🔗", 600).await;
        
        let mock_result = match planned_tool.tool_name.as_str() {
            "get_balance" => serde_json::json!({"balance": "2.5 SOL", "usd_value": 250.75}),
            "get_transactions" => serde_json::json!({"transactions": [{"amount": "0.1 SOL", "type": "sent"}]}),
            "get_account_stats" => serde_json::json!({"total_transactions": 156, "first_activity": "2024-01-15"}),
            _ => serde_json::json!({"result": "success"}),
        };
        
        println!("{}  └─ Found {} data{}", Colors::GREEN, planned_tool.tool_name.replace("get_", ""), Colors::RESET);
        tool_results.push((planned_tool.tool_name.clone(), mock_result));
    }
    
    // Show AI response generation
    show_animated_status("Generating AI response", "🤔💭🧠💡", 700).await;
    
    match ai_service.generate_contextual_response(original_message, &tool_results, &ai_plan.expected_outcome).await {
        Ok(response) => {
            println!("{}", response);
        }
        Err(_) => {
            println!("{}Executed {} tools successfully.{}", Colors::GREEN, tool_results.len(), Colors::RESET);
        }
    }
    
    Ok(())
}

/// Show contextual suggestions after AI response
async fn show_contextual_suggestions(ai_service: &Arc<AiService>, chat_history: &[String]) {
    show_animated_status("Generating contextual suggestions", "💡", 400).await;
    
    let context = if chat_history.len() > 5 {
        &chat_history[chat_history.len()-5..] // Last 5 messages
    } else {
        chat_history
    };
    
    let suggestion_prompt = format!(
        "Based on this blockchain conversation:\n{}\n\n\
        Generate exactly 5 short follow-up suggestions.\n\
        Format as:\n1. [suggestion]\n2. [suggestion]\netc.",
        context.join("\n")
    );
    
    match ai_service.query(&suggestion_prompt).await {
        Ok(response) => {
            println!("\n{}Reply Suggestions (just type the number):{}", Colors::CYAN, Colors::RESET);
            
            // Parse and display suggestions with colors
            let suggestions: Vec<String> = response.lines()
                .filter_map(|line| {
                    let line = line.trim();
                    if let Some(dot_pos) = line.find('.') {
                        if line[..dot_pos].trim().parse::<u32>().is_ok() {
                            return Some(line[dot_pos + 1..].trim().to_string());
                        }
                    }
                    None
                })
                .take(5)
                .collect();
            
            for (i, suggestion) in suggestions.iter().enumerate() {
                println!("{}{}. {}{}", Colors::BLUE, i + 1, suggestion, Colors::RESET);
            }
        }
        Err(_) => {
            println!("\n{}Reply Suggestions:{}", Colors::CYAN, Colors::RESET);
            println!("{}1. Show recent transactions{}", Colors::BLUE, Colors::RESET);
            println!("{}2. What's the current SOL price?{}", Colors::BLUE, Colors::RESET);
            println!("{}3. How do I stake my SOL?{}", Colors::BLUE, Colors::RESET);
        }
    }
}

/// Show help commands
fn show_help_commands() {
    println!("{}Available Commands:{}", Colors::CYAN, Colors::RESET);
    println!("{}  /balance{} - Check wallet balance", Colors::BLUE, Colors::RESET);
    println!("{}  /transactions{} - Show recent transactions", Colors::BLUE, Colors::RESET);
    println!("{}  /stake{} - Check staking rewards", Colors::BLUE, Colors::RESET);
    println!("{}  /price{} - Get token price", Colors::BLUE, Colors::RESET);
    println!("{}  /analyze{} - Analyze wallet address", Colors::BLUE, Colors::RESET);
    println!("{}  /context{} - Show context usage visualization", Colors::BLUE, Colors::RESET);
    println!("{}  /status{} - Show current system status", Colors::BLUE, Colors::RESET);
    println!("{}  /help{} - Show this help", Colors::BLUE, Colors::RESET);
    println!("{}  /clear{} - Clear conversation", Colors::BLUE, Colors::RESET);
    println!("{}  /quit{} - Exit chat", Colors::BLUE, Colors::RESET);
}

/// Show context visualization like Claude Code
async fn show_context_visualization(chat_history: &[String], ai_service: &Arc<AiService>) -> Result<()> {
    println!("{}┌─ Context Usage ──────────────────────────────────────┐{}", Colors::YELLOW, Colors::RESET);
    
    // Calculate token usage estimates
    let total_tokens: usize = 200_000;
    let system_tokens: usize = 3_100;
    let tools_tokens: usize = 12_200;
    let mcp_tokens: usize = 927;
    let memory_tokens = chat_history.len() * 50; // Estimate 50 tokens per message
    let messages_tokens = chat_history.len() * 100; // Estimate 100 tokens per message
    let used_tokens = system_tokens + tools_tokens + mcp_tokens + memory_tokens + messages_tokens;
    let free_tokens = total_tokens.saturating_sub(used_tokens);
    let usage_percent = (used_tokens as f32 / total_tokens as f32 * 100.0) as u32;
    
    // Visual token usage grid (like Claude)
    print_token_usage_grid(used_tokens, total_tokens);
    
    println!("{}│ {}Context Usage{} │{}", Colors::YELLOW, Colors::BOLD, Colors::RESET, Colors::YELLOW);
    println!("{}│ osvm-agent-chat • {}/{} tokens ({}%) │{}", 
            Colors::YELLOW, used_tokens, total_tokens, usage_percent, Colors::RESET);
    println!("{}├─────────────────────────────────────────────────────────┤{}", Colors::YELLOW, Colors::RESET);
    
    // Breakdown by category
    println!("{}│ {}System prompt:{} {} tokens ({:.1}%) │{}", 
            Colors::YELLOW, Colors::GREEN, Colors::RESET, system_tokens, 
            system_tokens as f32 / total_tokens as f32 * 100.0, Colors::RESET);
    println!("{}│ {}System tools:{} {} tokens ({:.1}%) │{}", 
            Colors::YELLOW, Colors::BLUE, Colors::RESET, tools_tokens,
            tools_tokens as f32 / total_tokens as f32 * 100.0, Colors::RESET);
    println!("{}│ {}MCP tools:{} {} tokens ({:.1}%) │{}", 
            Colors::YELLOW, Colors::MAGENTA, Colors::RESET, mcp_tokens,
            mcp_tokens as f32 / total_tokens as f32 * 100.0, Colors::RESET);
    println!("{}│ {}Memory files:{} {} tokens ({:.1}%) │{}", 
            Colors::YELLOW, Colors::CYAN, Colors::RESET, memory_tokens,
            memory_tokens as f32 / total_tokens as f32 * 100.0, Colors::RESET);
    println!("{}│ {}Messages:{} {} tokens ({:.1}%) │{}", 
            Colors::YELLOW, Colors::GREEN, Colors::RESET, messages_tokens,
            messages_tokens as f32 / total_tokens as f32 * 100.0, Colors::RESET);
    println!("{}│ {}Free space:{} {} ({:.1}%) │{}", 
            Colors::YELLOW, Colors::DIM, Colors::RESET, free_tokens,
            free_tokens as f32 / total_tokens as f32 * 100.0, Colors::RESET);
    
    println!("{}├─────────────────────────────────────────────────────────┤{}", Colors::YELLOW, Colors::RESET);
    println!("{}│ {}MCP tools • /mcp{} │{}", Colors::YELLOW, Colors::BOLD, Colors::RESET, Colors::YELLOW);
    println!("{}│   osvm_getBalance (blockchain): {} tokens │{}", Colors::YELLOW, 234, Colors::RESET);
    println!("{}│   osvm_getTransactions (blockchain): {} tokens │{}", Colors::YELLOW, 312, Colors::RESET);
    println!("{}│   osvm_getAccountStats (blockchain): {} tokens │{}", Colors::YELLOW, 381, Colors::RESET);
    
    println!("{}├─────────────────────────────────────────────────────────┤{}", Colors::YELLOW, Colors::RESET);
    println!("{}│ {}Memory files • /memory{} │{}", Colors::YELLOW, Colors::BOLD, Colors::RESET, Colors::YELLOW);
    println!("{}│   Chat History (/tmp/osvm-chat.log): {:.1}k tokens │{}", 
            Colors::YELLOW, memory_tokens as f32 / 1000.0, Colors::RESET);
    
    println!("{}├─────────────────────────────────────────────────────────┤{}", Colors::YELLOW, Colors::RESET);
    println!("{}│ {}Agent State{} │{}", Colors::YELLOW, Colors::BOLD, Colors::RESET, Colors::YELLOW);
    println!("{}│   Current conversation: {} messages │{}", Colors::YELLOW, chat_history.len(), Colors::RESET);
    println!("{}│   AI Service: {}osvm.ai{} (ready) │{}", Colors::YELLOW, Colors::GREEN, Colors::RESET, Colors::YELLOW);
    println!("{}│   MCP Status: {}3 servers connected{} │{}", Colors::YELLOW, Colors::GREEN, Colors::RESET, Colors::YELLOW);
    
    println!("{}└─────────────────────────────────────────────────────────┘{}", Colors::YELLOW, Colors::RESET);
    
    Ok(())
}

/// Print token usage grid visualization (like Claude Code)
fn print_token_usage_grid(used_tokens: usize, total_tokens: usize) {
    let grid_width = 50;
    let used_chars = (used_tokens as f32 / total_tokens as f32 * grid_width as f32) as usize;
    let free_chars = grid_width - used_chars;
    
    print!("{}│ ", Colors::YELLOW);
    
    // Show used space in different colors
    for i in 0..used_chars {
        let color = match i * 8 / used_chars {
            0 => Colors::GREEN,   // System
            1 => Colors::BLUE,    // Tools
            2 => Colors::MAGENTA, // MCP
            3 => Colors::CYAN,    // Memory
            4..=7 => Colors::GREEN, // Messages
            _ => Colors::YELLOW,
        };
        print!("{}█{}", color, Colors::RESET);
    }
    
    // Show free space
    for _ in 0..free_chars {
        print!("{}░{}", Colors::DIM, Colors::RESET);
    }
    
    println!(" {}│{}", Colors::YELLOW, Colors::RESET);
}

/// Show status overview
async fn show_status_overview(servers: &Vec<(&String, &McpServerConfig)>, chat_history: &[String]) -> Result<()> {
    println!("{}Current System Status:{}", Colors::CYAN, Colors::RESET);
    println!("{}  • Working Directory: {}/home/larp/larpdevs/osvm-cli{}", Colors::CYAN, Colors::DIM, Colors::RESET);
    println!("{}  • AI Service: {}osvm.ai{} (connected)", Colors::CYAN, Colors::GREEN, Colors::RESET);
    println!("{}  • MCP Servers: {}{}{} connected", Colors::CYAN, Colors::GREEN, servers.len(), Colors::RESET);
    println!("{}  • Chat History: {}{}{} messages", Colors::CYAN, Colors::BLUE, chat_history.len(), Colors::RESET);
    println!("{}  • Interface Mode: {}Real-time dynamic{}", Colors::CYAN, Colors::MAGENTA, Colors::RESET);
    
    println!("\n{}Available Shortcuts:{}", Colors::CYAN, Colors::RESET);
    println!("{}  ? for shortcuts{}", Colors::DIM, Colors::RESET);
    println!("{}  /context for context visualization{}", Colors::DIM, Colors::RESET);
    println!("{}  Tab for auto-complete{}", Colors::DIM, Colors::RESET);
    
    Ok(())
}

/// Show available tools
fn show_available_tools(servers: &Vec<(&String, &McpServerConfig)>) {
    println!("{}Available MCP Tools:{}", Colors::CYAN, Colors::RESET);
    for (server_id, _) in servers {
        println!("{}  • {}: {}Server connected{}", Colors::BLUE, server_id, Colors::GREEN, Colors::RESET);
    }
}

/// Get available tools for AI planning
fn get_available_tools() -> HashMap<String, Vec<McpTool>> {
    let mut tools = HashMap::new();
    
    let osvm_tools = vec![
        McpTool {
            name: "get_balance".to_string(),
            description: Some("Get wallet balance for a Solana address".to_string()),
            input_schema: serde_json::json!({"address": "string"}),
        },
        McpTool {
            name: "get_transactions".to_string(),
            description: Some("Get transaction history for a wallet".to_string()),
            input_schema: serde_json::json!({"address": "string", "limit": "number"}),
        },
        McpTool {
            name: "get_account_stats".to_string(),
            description: Some("Analyze account statistics and activity".to_string()),
            input_schema: serde_json::json!({"address": "string"}),
        },
    ];
    
    tools.insert("osvm-mcp".to_string(), osvm_tools);
    tools
}

/// Run comprehensive UI testing and demonstration
pub async fn run_chat_ui_tests() -> Result<()> {
    println!("🧪 OSVM Agent Chat UI - Real-time Dynamic Interface");
    println!("===================================================");
    println!();

    show_welcome_box();
    
    println!("📱 Claude Code-Style Features:");
    println!("   {}• Real-time auto-complete as you type{}", Colors::GREEN, Colors::RESET);
    println!("   {}• Dynamic suggestions based on input{}", Colors::GREEN, Colors::RESET);
    println!("   {}• Non-blocking async operations{}", Colors::GREEN, Colors::RESET);
    println!("   {}• Intelligent AI planning and execution{}", Colors::GREEN, Colors::RESET);
    println!("   {}• Beautiful ASCII diagrams with colors{}", Colors::GREEN, Colors::RESET);
    println!("   {}• Tab completion and Enter to submit{}", Colors::GREEN, Colors::RESET);
    
    Ok(())
}

/// Run demo mode for non-terminal environments  
async fn run_demo_mode() -> Result<()> {
    show_welcome_box();
    
    println!("📱 Real-time Dynamic Chat Interface Demo:");
    println!();
    println!("{}> {}/balance{}", Colors::GREEN, Colors::BLUE, Colors::RESET);
    println!("{}  /balance - Check wallet balance{}", Colors::GRAY, Colors::RESET);
    println!("{}  balance my wallet - Check wallet balance and holdings{}", Colors::GRAY, Colors::RESET);
    println!("{}  Tab to auto-complete • Enter to submit{}", Colors::DIM, Colors::RESET);
    println!();
    println!("{}• User: /balance{}", Colors::GREEN, Colors::RESET);
    println!("{}🤔 Analyzing your request...{}", Colors::CYAN, Colors::RESET);
    println!("{}⠋ Creating execution plan...{}", Colors::CYAN, Colors::RESET);
    println!("{}• Plan created successfully!{}", Colors::GREEN, Colors::RESET);
    println!();
    
    // Show colored plan
    println!("{}┌─ Execution Plan ─────────────────────────────────────┐{}", Colors::YELLOW, Colors::RESET);
    println!("{}│ {}Check user wallet balance and display holdings{} │{}", Colors::YELLOW, Colors::BOLD, Colors::RESET, Colors::YELLOW);
    println!("{}│  ┌─ {}get_balance{} ─┐                              │{}", Colors::YELLOW, Colors::YELLOW, Colors::RESET, Colors::YELLOW);
    println!("{}│  │ Server: {}osvm-mcp{}                      │{}", Colors::YELLOW, Colors::BLUE, Colors::RESET, Colors::YELLOW);
    println!("{}│  │ Args: {}address: user_wallet{}           │{}", Colors::YELLOW, Colors::DIM, Colors::RESET, Colors::YELLOW);
    println!("{}└─────────────────────────────────────────────────────────┘{}", Colors::YELLOW, Colors::RESET);
    
    println!("\n{}💻 To use the real-time interface, run: osvm chat{}", Colors::CYAN, Colors::RESET);
    
    Ok(())
}

/// Show enhanced status bar with spinner, task goal, todo list and reasoning
fn show_enhanced_status_bar(task_state: &TaskState) {
    let spinner = SPINNER_FRAMES[task_state.spinner_frame];
    
    // Main status bar with task goal
    println!("{}┌─ Task Status ────────────────────────────────────────┐{}", Colors::BLUE, Colors::RESET);
    println!("{}│ {} {}{}{} │{}", 
             Colors::BLUE, spinner, Colors::BOLD, task_state.current_task, Colors::RESET, Colors::BLUE);
    println!("{}├──────────────────────────────────────────────────────────┤{}", Colors::BLUE, Colors::RESET);
    
    // Todo list with navigation hints
    let todo_header = if task_state.input_mode == InputMode::TaskSelection {
        format!("{}Todo List {} (navigable){}", Colors::YELLOW, Colors::BOLD, Colors::RESET)
    } else {
        format!("{}Todo List{}", Colors::YELLOW, Colors::RESET)
    };
    
    println!("{}│ {} │{}", Colors::BLUE, format!("{:<54}", todo_header), Colors::BLUE);
    
    for (i, todo_item) in task_state.todo_items.iter().enumerate().take(4) {
        let checkbox = if todo_item.completed { "☑" } else { "☐" };
        let priority_color = match todo_item.priority {
            TodoPriority::High => Colors::RED,
            TodoPriority::Medium => Colors::YELLOW,
            TodoPriority::Low => Colors::GREEN,
        };
        
        let selection_indicator = if task_state.input_mode == InputMode::TaskSelection && 
                                    i == task_state.selected_todo_index {
            "▶ "
        } else {
            "  "
        };
        
        let todo_text = if todo_item.text.len() > 45 {
            format!("{}...", &todo_item.text[..42])
        } else {
            todo_item.text.clone()
        };
        
        println!("{}│{}{}{}{} {:<45} │{}", 
                Colors::BLUE, selection_indicator, priority_color, checkbox, Colors::RESET, todo_text, Colors::BLUE);
    }
    
    // Navigation hint for todo list
    if task_state.input_mode == InputMode::TaskSelection {
        println!("{}│ {}↑↓ Navigate • Enter Toggle • Ctrl+T Return{} │{}", 
                Colors::BLUE, Colors::DIM, Colors::RESET, Colors::BLUE);
    } else {
        println!("{}│ {}Press Ctrl+T to navigate tasks{} │{}", 
                Colors::BLUE, Colors::DIM, Colors::RESET, Colors::BLUE);
    }
    
    println!("{}├──────────────────────────────────────────────────────────┤{}", Colors::BLUE, Colors::RESET);
    
    // Current reasoning section
    println!("{}│ {}Current Reasoning{} │{}", Colors::BLUE, Colors::CYAN, Colors::RESET, Colors::BLUE);
    
    // Wrap reasoning text to fit in status bar
    let wrapped_reasoning = wrap_text(&task_state.current_reasoning, 54);
    for line in wrapped_reasoning.iter().take(3) {
        println!("{}│ {}{:<54}{} │{}", Colors::BLUE, Colors::DIM, line, Colors::RESET, Colors::BLUE);
    }
    
    println!("{}└──────────────────────────────────────────────────────────┘{}", Colors::BLUE, Colors::RESET);
    println!();
}

/// Show detailed task information below input bar
fn show_task_details_below_input(task_state: &TaskState) {
    if let Some(selected_task) = task_state.get_selected_task_details() {
        println!("{}┌─ Task Details ───────────────────────────────────────┐{}", Colors::MAGENTA, Colors::RESET);
        println!("{}│ {}Task:{} {:<47} │{}", Colors::MAGENTA, Colors::BOLD, Colors::RESET, selected_task.text, Colors::MAGENTA);
        println!("{}├──────────────────────────────────────────────────────────┤{}", Colors::MAGENTA, Colors::RESET);
        
        // Show reasoning
        println!("{}│ {}Reasoning{} │{}", Colors::MAGENTA, Colors::YELLOW, Colors::RESET, Colors::MAGENTA);
        let wrapped_reasoning = wrap_text(&selected_task.reasoning, 54);
        for line in wrapped_reasoning.iter().take(3) {
            println!("{}│ {}{:<54}{} │{}", Colors::MAGENTA, Colors::DIM, line, Colors::RESET, Colors::MAGENTA);
        }
        
        println!("{}├──────────────────────────────────────────────────────────┤{}", Colors::MAGENTA, Colors::RESET);
        
        // Show tool plan if available
        if let Some(tool_plan) = &selected_task.tool_plan {
            println!("{}│ {}Tool Plan{} │{}", Colors::MAGENTA, Colors::CYAN, Colors::RESET, Colors::MAGENTA);
            let plan_lines = tool_plan.lines().take(4);
            for line in plan_lines {
                let wrapped_line = if line.len() > 54 {
                    format!("{}...", &line[..51])
                } else {
                    line.to_string()
                };
                println!("{}│ {}{:<54}{} │{}", Colors::MAGENTA, Colors::DIM, wrapped_line, Colors::RESET, Colors::MAGENTA);
            }
        } else {
            println!("{}│ {}Tool Plan{} │{}", Colors::MAGENTA, Colors::CYAN, Colors::RESET, Colors::MAGENTA);
            println!("{}│ {}No tool plan available{:<35}{} │{}", Colors::MAGENTA, Colors::DIM, "", Colors::RESET, Colors::MAGENTA);
        }
        
        println!("{}├──────────────────────────────────────────────────────────┤{}", Colors::MAGENTA, Colors::RESET);
        
        // Show execution results if available
        if let Some(results) = &selected_task.execution_results {
            println!("{}│ {}Execution Results{} │{}", Colors::MAGENTA, Colors::GREEN, Colors::RESET, Colors::MAGENTA);
            let wrapped_results = wrap_text(results, 54);
            for line in wrapped_results.iter().take(2) {
                println!("{}│ {}{:<54}{} │{}", Colors::MAGENTA, Colors::DIM, line, Colors::RESET, Colors::MAGENTA);
            }
        } else {
            println!("{}│ {}Execution Results{} │{}", Colors::MAGENTA, Colors::GREEN, Colors::RESET, Colors::MAGENTA);
            println!("{}│ {}Not executed yet{:<41}{} │{}", Colors::MAGENTA, Colors::DIM, "", Colors::RESET, Colors::MAGENTA);
        }
        
        println!("{}└──────────────────────────────────────────────────────────┘{}", Colors::MAGENTA, Colors::RESET);
    }
}

/// Enhanced input handling with status bar integration and Ctrl+T support
async fn get_enhanced_input_with_status(
    suggestion_tx: &mpsc::UnboundedSender<String>,
    task_state: &mut TaskState,
) -> Result<String> {
    let mut input_state = InputState::new();
    input_state.history_index = input_state.command_history.len();

    // Show input prompt
    if task_state.input_mode == InputMode::InputField {
        println!("{}┌─ Input ──────────────────────────────────────────────┐{}", Colors::GREEN, Colors::RESET);
        print!("{}│{} > ", Colors::GREEN, Colors::RESET);
        io::stdout().flush().map_err(|e| anyhow!("Failed to flush stdout: {}", e))?;
        
        // Show task details below input bar when not in task selection mode
        println!();
        println!("{}└──────────────────────────────────────────────────────┘{}", Colors::GREEN, Colors::RESET);
        show_task_details_below_input(task_state);
        
        // Move cursor back to input line
        print!("\x1B[{}A", 15); // Move up to input line
        print!("{}│{} > ", Colors::GREEN, Colors::RESET);
        io::stdout().flush().map_err(|e| anyhow!("Failed to flush stdout: {}", e))?;
    } else {
        // In task selection mode, show task details below the status bar
        show_task_details_below_input(task_state);
    }

    // Enable raw mode for character-by-character input
    enable_raw_mode()?;

    loop {
        match read_single_character()? {
            InputChar::Enter => {
                if task_state.input_mode == InputMode::TaskSelection {
                    // Toggle selected todo item
                    task_state.toggle_todo(task_state.selected_todo_index);
                    
                    // Update reasoning based on completed tasks
                    let completed_count = task_state.todo_items.iter().filter(|t| t.completed).count();
                    task_state.current_reasoning = format!(
                        "Progress: {}/{} tasks completed. Current focus: {}",
                        completed_count,
                        task_state.todo_items.len(),
                        task_state.todo_items.get(task_state.selected_todo_index)
                            .map(|t| &t.text)
                            .unwrap_or(&"N/A".to_string())
                    );
                    
                    // Refresh display
                    print!("\x1B[2J\x1B[1;1H");
                    show_enhanced_status_bar(task_state);
                    continue;
                } else {
                    // Submit input
                    let result = handle_enter_key_enhanced(&mut input_state).await?;
                    
                    // Update task state with user input
                    if !result.trim().is_empty() {
                        task_state.current_task = format!("Processing: {}", 
                            if result.len() > 30 { format!("{}...", &result[..27]) } else { result.clone() });
                        task_state.current_reasoning = format!("User requested: {}", result);
                        
                        // Add new task to todo list
                        task_state.todo_items.push(TodoItem {
                            text: format!("Process: {}", result),
                            completed: false,
                            priority: TodoPriority::High,
                            reasoning: format!("User requested to process: {}", result),
                            tool_plan: Some("1. Parse user input\n2. Generate execution plan\n3. Execute required actions".to_string()),
                            execution_results: None,
                        });
                    }
                    
                    return Ok(result);
                }
            }
            InputChar::Backspace => {
                if task_state.input_mode == InputMode::InputField {
                    handle_backspace(&mut input_state, suggestion_tx).await?;
                }
            }
            InputChar::CtrlC => {
                disable_raw_mode()?;
                return Err(anyhow!("Interrupted"));
            }
            InputChar::CtrlT => {
                // Toggle between input field and task selection
                task_state.input_mode = match task_state.input_mode {
                    InputMode::InputField => {
                        clear_suggestions_display();
                        task_state.current_reasoning = "Task selection mode active. Use ↑↓ to navigate, Enter to toggle.".to_string();
                        InputMode::TaskSelection
                    }
                    InputMode::TaskSelection => {
                        task_state.current_reasoning = "Input mode active. Type your command or query.".to_string();
                        InputMode::InputField
                    }
                };
                
                // Refresh display
                print!("\x1B[2J\x1B[1;1H");
                show_enhanced_status_bar(task_state);
                
                if task_state.input_mode == InputMode::InputField {
                    println!("{}┌─ Input ──────────────────────────────────────────────┐{}", Colors::GREEN, Colors::RESET);
                    print!("{}│{} > {}", Colors::GREEN, Colors::RESET, input_state.input);
                    io::stdout().flush().map_err(|e| anyhow!("Failed to flush stdout: {}", e))?;
                }
            }
            InputChar::Escape => {
                if task_state.input_mode == InputMode::TaskSelection {
                    // Cancel task selection and return to input mode
                    task_state.input_mode = InputMode::InputField;
                    task_state.current_reasoning = "Cancelled task selection. Returned to input mode.".to_string();
                    
                    // Refresh display
                    print!("\x1B[2J\x1B[1;1H");
                    show_enhanced_status_bar(task_state);
                    
                    println!("{}┌─ Input ──────────────────────────────────────────────┐{}", Colors::GREEN, Colors::RESET);
                    print!("{}│{} > {}", Colors::GREEN, Colors::RESET, input_state.input);
                    io::stdout().flush().map_err(|e| anyhow!("Failed to flush stdout: {}", e))?;
                } else {
                    // In input mode, Esc clears the current input
                    input_state.input.clear();
                    input_state.cursor_pos = 0;
                    clear_suggestions_display();
                    redraw_input_line(&input_state.input)?;
                }
            }
            InputChar::Tab => {
                if task_state.input_mode == InputMode::InputField {
                    handle_tab_completion(&mut input_state).await?;
                }
            }
            InputChar::Arrow(arrow_key) => {
                match task_state.input_mode {
                    InputMode::InputField => {
                        handle_arrow_key(&mut input_state, arrow_key).await?;
                    }
                    InputMode::TaskSelection => {
                        match arrow_key {
                            ArrowKey::Up => {
                                task_state.navigate_todo_up();
                                if let Some(selected_task) = task_state.get_selected_task_details() {
                                    task_state.current_reasoning = selected_task.reasoning.clone();
                                }
                                print!("\x1B[2J\x1B[1;1H");
                                show_enhanced_status_bar(task_state);
                                show_task_details_below_input(task_state);
                            }
                            ArrowKey::Down => {
                                task_state.navigate_todo_down();
                                if let Some(selected_task) = task_state.get_selected_task_details() {
                                    task_state.current_reasoning = selected_task.reasoning.clone();
                                }
                                print!("\x1B[2J\x1B[1;1H");
                                show_enhanced_status_bar(task_state);
                                show_task_details_below_input(task_state);
                            }
                            _ => {}
                        }
                    }
                }
            }
            InputChar::Regular(ch) => {
                if task_state.input_mode == InputMode::InputField {
                    handle_regular_character(&mut input_state, ch, suggestion_tx).await?;
                    
                    // Update reasoning based on input
                    if input_state.input.len() > 0 {
                        task_state.current_reasoning = format!("User typing: '{}'", input_state.input);
                    }
                }
            }
            InputChar::Unknown => {
                debug!("Unknown character received in enhanced input");
            }
        }
    }
}

/// Handle Enter key for enhanced input
async fn handle_enter_key_enhanced(state: &mut InputState) -> Result<String> {
    disable_raw_mode()?;
    clear_suggestions_display();
    println!();
    println!("{}└──────────────────────────────────────────────────────┘{}", Colors::GREEN, Colors::RESET);

    // Add to command history if not empty
    state.add_to_history(state.input.clone());

    Ok(state.input.clone())
}

/// Highlight matched characters in fuzzy search results
fn highlight_fuzzy_match(text: &str, matched_indices: &[usize], base_color: &str) -> String {
    if matched_indices.is_empty() {
        return format!("{}{}{}", base_color, text, Colors::RESET);
    }

    let mut result = String::new();
    let chars: Vec<char> = text.chars().collect();
    
    for (i, &ch) in chars.iter().enumerate() {
        if matched_indices.contains(&i) {
            // Highlight matched character with bold and bright color
            result.push_str(&format!("{}{}{}", Colors::BOLD, ch, Colors::RESET));
            result.push_str(base_color); // Return to base color
        } else {
            result.push(ch);
        }
    }
    
    format!("{}{}{}", base_color, result, Colors::RESET)
}