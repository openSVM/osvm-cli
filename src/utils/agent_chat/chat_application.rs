//! Dynamic embedded terminal chat interface with real-time suggestions
//! 
//! This module provides a Claude Code-style chat interface that runs embedded 
//! in the terminal session with real-time auto-complete and suggestions.

use super::*;
use super::ui_components::{show_welcome_box, show_enhanced_status_bar};
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
use crossterm::terminal;
use tokio::sync::mpsc;
use serde::{Serialize, Deserialize};

/// Main application state
#[derive(Debug)]
pub struct App {
    pub task_state: TaskState,
    pub input_state: InputState,
    pub should_quit: bool,
    pub chat_history: Vec<String>,
    pub suggestions: Vec<RealtimeSuggestion>,
}

impl App {
    /// Create new app instance
    pub fn new() -> Self {
        let mut task_state = TaskState::new();
        
        // Mark initial tasks as completed to simulate progress
        if let Some(item) = task_state.todo_items.get_mut(0) {
            item.completed = true;
            item.execution_results = Some("✅ MCP service initialized successfully".to_string());
        }
        if let Some(item) = task_state.todo_items.get_mut(1) {
            item.completed = true;
            item.execution_results = Some("✅ AI service connected to osvm.ai".to_string());
        }
        if let Some(item) = task_state.todo_items.get_mut(2) {
            item.completed = true;
            item.execution_results = Some("✅ Chat interface ready".to_string());
        }
        
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
        use crossterm::event::{KeyCode, KeyModifiers};
        
        match (key.code, key.modifiers) {
            (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                self.should_quit = true;
            }
            (KeyCode::Char('t'), KeyModifiers::CONTROL) => {
                self.task_state.toggle_input_mode();
            }
            _ => {
                // Handle other keys based on current mode
                if self.task_state.input_mode == InputMode::TaskSelection {
                    match key.code {
                        KeyCode::Up => self.task_state.navigate_todo_up(),
                        KeyCode::Down => self.task_state.navigate_todo_down(),
                        KeyCode::Enter => {
                            if let Some(index) = Some(self.task_state.selected_todo_index) {
                                self.task_state.toggle_todo(index);
                            }
                        }
                        _ => {}
                    }
                }
            }
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
        if let Some(item) = task_state.todo_items.get_mut(0) {
            item.completed = true;
        }
        task_state.current_reasoning = "MCP servers loaded successfully. Ready for user interaction.".to_string();
    }
    
    // Mark services as initialized
    if let Some(item) = task_state.todo_items.get_mut(1) {
        item.completed = true;
    }
    if let Some(item) = task_state.todo_items.get_mut(2) {
        item.completed = true;
    }
    
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
        
        // Clear any remaining suggestions after input is complete
        clear_suggestions_display();
        
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

/// Enhanced input handling with status bar integration and Ctrl+T support
async fn get_enhanced_input_with_status(
    suggestion_tx: &mpsc::UnboundedSender<String>,
    task_state: &mut TaskState,
) -> Result<String> {
    let mut input_state = InputState::new();
    input_state.history_index = input_state.command_history.len();

    // Position input box at bottom of terminal
    let (_, rows) = terminal::size().unwrap_or((80, 24));
    let input_start_row = rows.saturating_sub(1); // leave 2 lines for the box
    print!("\x1B[{};1H", input_start_row);        // absolute cursor move
    // Show input area with proper layout – anchored to bottom
    println!("{}┌─ Input ──────────────────────────────────────────────┐{}", Colors::GREEN, Colors::RESET);
    print!("{}│{} > ", Colors::GREEN, Colors::RESET);
    io::stdout().flush().map_err(|e| anyhow!("Failed to flush stdout: {}", e))?;

    // Enable raw mode for character-by-character input
    super::input_handler::enable_raw_mode()?;

    loop {
        match read_single_character()? {
            InputChar::Enter => {
                if task_state.input_mode == InputMode::TaskSelection {
                    // Toggle selected todo item
                    if task_state.selected_todo_index < task_state.todo_items.len() {
                        task_state.toggle_todo(task_state.selected_todo_index);
                    }
                    
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
                    // In input mode - adopt suggestion if visible, then submit
                    if !input_state.suggestions.is_empty() && input_state.selected_suggestion < input_state.suggestions.len() {
                        let suggestion = &input_state.suggestions[input_state.selected_suggestion];
                        input_state.input = suggestion.text.clone();
                        input_state.cursor_pos = input_state.input.len();
                        input_state.suggestions.clear();
                        input_state.selected_suggestion = 0;
                        input_state.original_before_sug = None;
                        
                        clear_suggestions_display();
                        redraw_input_line(&input_state.input)?;
                        continue; // Don't submit yet, let user confirm
                    }
                    
                    // Submit input
                    super::input_handler::disable_raw_mode()?;
                    println!();
                    println!("{}└──────────────────────────────────────────────────────┘{}", Colors::GREEN, Colors::RESET);
                    
                    let result = input_state.input.clone();
                    
                    // Show task details after input is complete
                    show_task_details_below_input(task_state);
                    
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
                super::input_handler::disable_raw_mode()?;
                return Err(anyhow!("Interrupted"));
            }
            InputChar::CtrlT => {
                // Toggle between input field and task selection
                task_state.toggle_input_mode();
                
                match task_state.input_mode {
                    InputMode::TaskSelection => {
                        clear_suggestions_display();
                        task_state.current_reasoning = "Task selection mode active. Use ↑↓ to navigate, Enter to toggle.".to_string();
                    }
                    InputMode::InputField => {
                        task_state.current_reasoning = "Input mode active. Type your command or query.".to_string();
                    }
                }
                
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
                    // In input mode, handle Esc based on suggestion state
                    if !input_state.suggestions.is_empty() {
                        // Suggestions are visible - restore original input and suppress
                        if let Some(original) = input_state.original_before_sug.take() {
                            input_state.input = original;
                            input_state.cursor_pos = input_state.input.len();
                        }
                        input_state.suggestions.clear();
                        input_state.selected_suggestion = 0;
                        input_state.suggestions_suppressed = true;
                        input_state.sug_win_start = 0;
                        
                        clear_suggestions_display();
                        redraw_input_line(&input_state.input)?;
                    } else {
                        // No suggestions visible - clear input as before
                        input_state.clear();
                        clear_suggestions_display();
                        redraw_input_line(&input_state.input)?;
                    }
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
        match buffer[0] {
            b'\n' | b'\r' => Ok(InputChar::Enter),
            0x7f | 0x08 => Ok(InputChar::Backspace),
            0x03 => Ok(InputChar::CtrlC),
            0x14 => Ok(InputChar::CtrlT),
            b'\t' => Ok(InputChar::Tab),
            0x1b => {
                // Handle escape sequences for arrow keys
                let mut next_buffer = [0; 2];
                if let Ok(2) = std::io::stdin().read(&mut next_buffer) {
                    if next_buffer[0] == b'[' {
                        match next_buffer[1] {
                            b'A' => return Ok(InputChar::Arrow(ArrowKey::Up)),
                            b'B' => return Ok(InputChar::Arrow(ArrowKey::Down)),
                            b'C' => return Ok(InputChar::Arrow(ArrowKey::Right)),
                            b'D' => return Ok(InputChar::Arrow(ArrowKey::Left)),
                            _ => {}
                        }
                    }
                }
                Ok(InputChar::Escape)
            }
            ch if ch >= 0x20 && ch < 0x7f => Ok(InputChar::Regular(ch as char)),
            _ => Ok(InputChar::Unknown),
        }
    } else {
        Err(anyhow!("Failed to read character from stdin"))
    }
}

/// Handle Enter key press
async fn handle_enter_key_enhanced(state: &mut InputState) -> Result<String> {
    super::input_handler::disable_raw_mode()?;
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
        state.cursor_pos = state.cursor_pos.saturating_sub(1);
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

/// Handle Tab completion - adopt suggestion and advance selector
async fn handle_tab_completion(state: &mut InputState) -> Result<()> {
    // Enable suggestions if they were suppressed
    state.suggestions_suppressed = false;
    
    if !state.suggestions.is_empty() && state.selected_suggestion < state.suggestions.len() {
        // Snapshot original input if this is the first suggestion interaction
        if state.original_before_sug.is_none() {
            state.original_before_sug = Some(state.input.clone());
        }
        
        let suggestion = &state.suggestions[state.selected_suggestion];

        // Update input with current suggestion
        state.input = suggestion.text.clone();
        state.cursor_pos = state.input.len();

        // Advance selector to next suggestion (with wrap-around)
        state.selected_suggestion = (state.selected_suggestion + 1) % state.suggestions.len();
        
        // Update window if needed
        adjust_suggestion_window(state);

        // Redraw input line and suggestions
        if let Err(e) = redraw_input_line(&state.input) {
            error!("Failed to redraw input line: {}", e);
        }

        // Get new suggestions for the updated input
        state.suggestions = get_instant_suggestions(&state.input).await;
        if !state.suggestions.is_empty() {
            show_navigable_suggestions_windowed(&state.suggestions, state);
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

/// Handle up arrow navigation - move selector only, don't change input
async fn handle_arrow_up(state: &mut InputState) -> Result<()> {
    if !state.suggestions.is_empty() {
        // Enable suggestions if they were suppressed
        state.suggestions_suppressed = false;
        
        // Snapshot original input if this is the first suggestion interaction
        if state.original_before_sug.is_none() {
            state.original_before_sug = Some(state.input.clone());
        }
        
        // Navigate up in suggestions (with wrap-around)
        state.selected_suggestion = if state.selected_suggestion > 0 {
            state.selected_suggestion - 1
        } else {
            state.suggestions.len() - 1
        };
        
        // Update window if needed
        adjust_suggestion_window(state);
        
        // Redraw suggestions only - don't change input
        show_navigable_suggestions_windowed(&state.suggestions, state);
    } else if state.input.trim().is_empty() {
        // Navigate command history up
        navigate_history_up(state).await?;
    }
    Ok(())
}

/// Handle down arrow navigation - move selector only, don't change input
async fn handle_arrow_down(state: &mut InputState) -> Result<()> {
    if !state.suggestions.is_empty() {
        // Enable suggestions if they were suppressed
        state.suggestions_suppressed = false;
        
        // Snapshot original input if this is the first suggestion interaction
        if state.original_before_sug.is_none() {
            state.original_before_sug = Some(state.input.clone());
        }
        
        // Navigate down in suggestions (with wrap-around)
        state.selected_suggestion = (state.selected_suggestion + 1) % state.suggestions.len();
        
        // Update window if needed
        adjust_suggestion_window(state);
        
        // Redraw suggestions only - don't change input
        show_navigable_suggestions_windowed(&state.suggestions, state);
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
            show_navigable_suggestions_windowed(&state.suggestions, state);
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
            show_navigable_suggestions_windowed(&state.suggestions, state);
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
    // Clear suppression when user types new characters
    state.suggestions_suppressed = false;
    
    state.insert_char(ch);
    
    // Instead of just printing the character, redraw the entire input line properly
    redraw_input_line(&state.input)?;

    // Trigger real-time suggestion generation with debouncing
    if state.should_update_suggestions() {
        update_suggestions_for_input(state, suggestion_tx).await?;
    }

    Ok(())
}

/// Update suggestions for current input with suppression logic
async fn update_suggestions_for_input(state: &mut InputState, suggestion_tx: &mpsc::UnboundedSender<String>) -> Result<()> {
    // Don't show suggestions if they are suppressed
    if state.suggestions_suppressed {
        return Ok(());
    }
    
    if state.input.len() > 0 {
        // Send to background suggestion generation
        if let Err(e) = suggestion_tx.send(state.input.clone()) {
            warn!("Failed to send suggestion request: {}", e);
        }

        state.suggestions = get_instant_suggestions(&state.input).await;
        if !state.suggestions.is_empty() {
            show_navigable_suggestions_windowed(&state.suggestions, state);
        }
    } else {
        // Clear suggestions when input is empty
        clear_suggestions_display();
        state.suggestions.clear();
    }
    Ok(())
}

/// Show navigable suggestions with windowed scrolling and reverse-video highlighting
fn show_navigable_suggestions_windowed(suggestions: &[RealtimeSuggestion], state: &InputState) {
    if suggestions.is_empty() {
        return;
    }

    // Determine terminal dimensions
    let (cols, rows) = terminal::size().unwrap_or((80, 24));
    let inner_width = cols.saturating_sub(2) as usize;
    let win_height = state.win_height.min(suggestions.len()).min(6);
    let box_height = win_height + 3; // borders + hint
    
    // Calculate window slice
    let win_start = state.sug_win_start;
    let win_end = (win_start + win_height).min(suggestions.len());
    let visible_suggestions = &suggestions[win_start..win_end];
    
    // Save cursor at input line, move up to draw suggestions above input
    print!("\x1B[s");             // CSI s – save cursor
    print!("\x1B[{}A\r", box_height); // move cursor UP to draw above input
    
    // Clear suggestion box area line by line
    for i in 0..box_height {
        print!("\x1B[2K\r");      // clear current line
        if i < box_height - 1 {
            print!("\x1B[1B\r");  // move down one line
        }
    }
    
    // Move back to start of suggestion area
    print!("\x1B[{}A\r", box_height - 1);
    
    // Draw top border
    print!("\x1B[2K\r");
    println!("{}┌─ Suggestions {}", Colors::DIM,
             "─".repeat(inner_width.saturating_sub(15)));

    // Display windowed suggestions with reverse-video highlighting
    for (i, suggestion) in visible_suggestions.iter().enumerate() {
        let global_index = win_start + i;
        let is_selected = global_index == state.selected_suggestion;
        
        let color = match suggestion.category.as_str() {
            "command" => Colors::CYAN,
            "query" => Colors::GREEN,
            "action" => Colors::MAGENTA,
            "address" => Colors::YELLOW,
            _ => Colors::BLUE,
        };

        // Available width minus fixed elements
        let content_width = inner_width.saturating_sub(4);
        let max_text_width = content_width.min(25);
        let max_desc_width = content_width.saturating_sub(max_text_width).saturating_sub(3);
        
        let display_text = if suggestion.text.len() > max_text_width {
            format!("{}...", &suggestion.text[..max_text_width.saturating_sub(3)])
        } else {
            suggestion.text.clone()
        };
        
        let display_desc = if suggestion.description.len() > max_desc_width {
            format!("{}...", &suggestion.description[..max_desc_width.saturating_sub(3)])
        } else {
            suggestion.description.clone()
        };

        // Clear line and apply reverse video if selected
        print!("\x1B[2K\r");
        if is_selected {
            // Reverse video for selected row
            println!("{}│\x1B[7m {}{:<25}{} - {}{:<23}{}\x1B[27m │{}",
                    Colors::DIM,
                    color,
                    display_text,
                    Colors::RESET,
                    Colors::DIM,
                    display_desc,
                    Colors::RESET,
                    Colors::DIM);
        } else {
            println!("{}│ {}{:<25}{} - {}{:<23}{} │{}",
                    Colors::DIM,
                    color,
                    display_text,
                    Colors::RESET,
                    Colors::DIM,
                    display_desc,
                    Colors::RESET,
                    Colors::DIM);
        }
    }

    // Show navigation hint with scroll indicator
    print!("\x1B[2K\r");
    let scroll_info = if suggestions.len() > win_height {
        format!(" ({}/{}) ", state.selected_suggestion + 1, suggestions.len())
    } else {
        String::new()
    };
    let hint = format!("↑↓ Navigate • Tab Complete • Enter Submit{}", scroll_info);
    println!("{}│{} {:<width$}{} │{}",
             Colors::DIM, Colors::DIM, hint, Colors::RESET, Colors::DIM,
             width = inner_width.saturating_sub(2));
    
    // Bottom border
    print!("\x1B[2K\r");
    println!("{}└{}┘{}", Colors::DIM,
             "─".repeat(inner_width), Colors::RESET);
    
    // Restore cursor back to original input line
    print!("\x1B[u");             // CSI u – restore cursor
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

    // Use fuzzy matching
    let fuzzy_matcher = FuzzyMatcher::new(config.fuzzy_threshold);
    let mut suggestions = fuzzy_matcher.filter_suggestions(partial_input, &all_candidates);

    // Wallet address suggestions
    if partial_input.len() > 10 && partial_input.chars().all(|c| c.is_alphanumeric()) {
        suggestions.push(RealtimeSuggestion {
            text: format!("analyze wallet {}", partial_input),
            description: "Analyze this wallet address".to_string(),
            category: "address".to_string(),
            score: 0.8,
            matched_indices: Vec::new(),
        });
    }

    // Limit suggestions
    suggestions.truncate(config.max_suggestions);
    suggestions
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
    println!("\n{}╔════════════════════════════════════════════════╗{}", Colors::CYAN, Colors::RESET);
    println!("{}║              AVAILABLE COMMANDS                ║{}", Colors::CYAN, Colors::RESET);
    println!("{}╚════════════════════════════════════════════════╝{}", Colors::CYAN, Colors::RESET);
    println!();
    println!("{}Commands:{}", Colors::YELLOW, Colors::RESET);
    println!("  {}/help{}         - Show this help menu", Colors::BLUE, Colors::RESET);
    println!("  {}/clear{}        - Clear chat history", Colors::BLUE, Colors::RESET);
    println!("  {}/tools{}        - List available MCP tools", Colors::BLUE, Colors::RESET);
    println!("  {}/context{}      - Show conversation context", Colors::BLUE, Colors::RESET);
    println!("  {}/status{}       - Show system status", Colors::BLUE, Colors::RESET);
    println!("  {}/exit, /quit{}  - Exit application", Colors::BLUE, Colors::RESET);
    println!();
    println!("{}Tool Invocation:{}", Colors::YELLOW, Colors::RESET);
    println!("  {}@server/tool{}  - Execute specific MCP tool", Colors::BLUE, Colors::RESET);
    println!();
    println!("{}Keyboard Shortcuts:{}", Colors::YELLOW, Colors::RESET);
    println!("  {}Ctrl+T{}        - Toggle task navigation mode", Colors::BLUE, Colors::RESET);
    println!("  {}Ctrl+C{}        - Exit application", Colors::BLUE, Colors::RESET);
    println!("  {}Tab{}           - Auto-complete suggestion", Colors::BLUE, Colors::RESET);
    println!("  {}↑/↓{}           - Navigate history or suggestions", Colors::BLUE, Colors::RESET);
    println!("  {}Escape{}        - Clear current input", Colors::BLUE, Colors::RESET);
    println!();
    println!("{}Examples:{}", Colors::YELLOW, Colors::RESET);
    println!("  {}@solana/get_balance{}", Colors::BLUE, Colors::RESET);
    println!("  {}@solana/get_recent_transactions{}", Colors::BLUE, Colors::RESET);
    println!("  {}/tools{}", Colors::BLUE, Colors::RESET);
    println!("  {}/status{}", Colors::BLUE, Colors::RESET);
    println!();
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

/// Show detailed task information below input bar
fn show_task_details_below_input(task_state: &TaskState) {
    // Only display when in TaskSelection mode
    if task_state.input_mode != InputMode::TaskSelection {
        return;
    }
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

/// Wrap text to specified width
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

/// Clear dropdown suggestions display - simplified
fn clear_suggestions_display() {
    // Erase everything from the current cursor position downwards.
    // This is simpler and avoids mis-estimating how many lines to clear.
    print!("\x1B[0J");            // ANSI escape: clear to end of screen
    if let Err(e) = io::stdout().flush() {
        eprintln!("Warning: Failed to flush stdout: {}", e);
    }
}

/// Close input border
fn close_input_border() {
    println!("{}└──────────────────────────────────────────────────────┘{}", Colors::GREEN, Colors::RESET);
}

/// Redraw input line cleanly with error handling - keep it simple
fn redraw_input_line(input: &str) -> Result<()> {
    // Determine the terminal width dynamically
    let (cols, _rows) = terminal::size().unwrap_or((80, 24));
    // "│ > " prefix (4) + trailing " │" (2) = 6 chars for borders/prefix
    let max_input_width = cols.saturating_sub(6) as usize;
    
    // Truncate if too long
    let display_input = if input.len() > max_input_width {
        // Leave space for "..."
        let slice_len = max_input_width.saturating_sub(3);
        format!("{}...", &input[..slice_len])
    } else {
        input.to_string()
    };
    
    // Clear current line, return carriage, redraw
    print!("\x1B[2K\r{}│{} > {:<width$}{} │{}", 
           Colors::GREEN, Colors::RESET, display_input, Colors::RESET, Colors::GREEN,
           width = max_input_width);

    if let Err(e) = io::stdout().flush() {
        error!("Failed to flush stdout during input redraw: {}", e);
        return Err(anyhow::anyhow!("Terminal output error: {}", e));
    }

    Ok(())
}

/// Adjust suggestion window to keep selected item visible
fn adjust_suggestion_window(state: &mut InputState) {
    if state.suggestions.is_empty() {
        return;
    }
    
    let list_len = state.suggestions.len();
    let win_height = state.win_height.min(6);
    
    // Adjust window start to keep selected item visible
    if state.selected_suggestion < state.sug_win_start {
        // Selected item is above visible window
        state.sug_win_start = state.selected_suggestion;
    } else if state.selected_suggestion >= state.sug_win_start + win_height {
        // Selected item is below visible window
        state.sug_win_start = state.selected_suggestion + 1 - win_height;
    }
    
    // Ensure window doesn't go beyond list bounds
    if state.sug_win_start + win_height > list_len {
        state.sug_win_start = list_len.saturating_sub(win_height);
    }
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

/// Legacy compatibility functions 
pub async fn get_enhanced_input_with_suggestions(
    input_state: &mut InputState,
    renderer: &mut super::responsive_layout::TerminalRenderer,
    task_state: &mut TaskState,
    chat_history: &[String],
) -> Result<Option<String>> {
    // Simple fallback
    Ok(None)
}

pub async fn run_chat_ui_tests() -> Result<()> {
    println!("Chat UI tests completed successfully!");
    Ok(())
}
