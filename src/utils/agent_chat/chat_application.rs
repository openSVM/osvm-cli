//! Main chat application orchestrating all components

use super::*;
use crate::services::mcp_service::{McpService, McpServerConfig};
use crate::services::ai_service::AiService;
use anyhow::{Result, anyhow};
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;
use tokio::time::{sleep, Duration};
use log::{info, warn, error, debug};
use std::io::{self, Write};
use crossterm::{
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, enable_raw_mode as crossterm_enable_raw_mode, disable_raw_mode as crossterm_disable_raw_mode},
    event::{EnableMouseCapture, DisableMouseCapture}
};
use ratatui::{backend::CrosstermBackend, Terminal};

/// Terminal cleanup guard to ensure terminal is restored on panic/error
struct TerminalGuard;

impl TerminalGuard {
    fn new() -> Result<Self> {
        crossterm_enable_raw_mode()?;
        Ok(Self)
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        // Always attempt to restore terminal state
        let _ = crossterm_disable_raw_mode();
        let _ = execute!(io::stdout(), LeaveAlternateScreen, DisableMouseCapture);
    }
}

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

        // Mark initial tasks as completed for demo
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

    /// Handle key event
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

/// Main entry point for the agent chat UI
pub async fn run_agent_chat_ui() -> Result<()> {
    info!("Starting OSVM Agent Chat Interface");

    // Initialize services
    let mut mcp_service = McpService::new_with_debug(false);
    let ai_service = Arc::new(AiService::new_with_debug(false));

    // Load MCP configurations
    if let Err(e) = mcp_service.load_config() {
        warn!("Failed to load MCP config: {}", e);
    }

    // Setup terminal with proper error handling
    let terminal_guard = TerminalGuard::new()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Initialize app state
    let mut app = App::new();

    // Initialize task state
    let mut task_state = TaskState::new();

    // Show welcome
    ui_components::show_welcome_box();

    // Show available MCP servers
    let servers = mcp_service.list_servers();
    if servers.is_empty() {
        println!("{}• No MCP servers configured. Use 'osvm mcp setup' to get started{}",
            Colors::CYAN, Colors::RESET);
    } else {
        let server_names: Vec<String> = servers.iter().map(|(id, _)| (*id).clone()).collect();
        println!("{}• Available MCP servers: {}{}{}",
            Colors::CYAN, Colors::BLUE, server_names.join(", "), Colors::RESET);

        // Update task state safely
        if let Some(item) = task_state.todo_items.get_mut(0) {
            item.completed = true;
        }
        task_state.current_reasoning = "MCP servers loaded successfully. Ready for user interaction.".to_string();
    }

    // Mark services as initialized safely
    if let Some(item) = task_state.todo_items.get_mut(1) {
        item.completed = true;
    }
    if let Some(item) = task_state.todo_items.get_mut(2) {
        item.completed = true;
    }

    println!();

    // Track chat history (single source of truth)
    let mut chat_history: Vec<String> = Vec::new();
    let mut command_processor = command_processor::CommandProcessor::new();

    // Initialize real-time suggestion system
    let (suggestion_tx, mut suggestion_rx) = mpsc::unbounded_channel::<String>();
    let ai_service_clone = ai_service.clone();

    // Use a simple flag for cancellation since tokio_util is not available
    let shutdown_flag = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let shutdown_clone = shutdown_flag.clone();
    let shutdown_clone2 = shutdown_flag.clone();

    // Spawn background task for suggestions with cancellation
    let suggestion_handle = tokio::spawn(async move {
        while let Some(partial_input) = suggestion_rx.recv().await {
            if shutdown_clone.load(std::sync::atomic::Ordering::Relaxed) {
                debug!("Suggestion task shutting down");
                break;
            }
            if partial_input.len() > 2 {
                let _ = ai_integration::generate_realtime_suggestions(&partial_input, &ai_service_clone).await;
            }
        }
    });

    // Spawn background task for spinner animation with cancellation
    let (spinner_tx, mut spinner_rx) = mpsc::unbounded_channel::<()>();
    let spinner_handle = tokio::spawn(async move {
        let mut interval = tokio::time::interval(Duration::from_millis(100));
        loop {
            if shutdown_clone2.load(std::sync::atomic::Ordering::Relaxed) {
                debug!("Spinner task shutting down");
                break;
            }
            interval.tick().await;
            if spinner_tx.send(()).is_err() {
                break;
            }
        }
    });

    // Initial screen setup
    print!("\x1B[2J\x1B[1;1H");
    ui_components::show_enhanced_status_bar(&task_state);

    // Main interactive loop
    loop {
        // Update spinner frame (without clearing screen!)
        if let Ok(_) = spinner_rx.try_recv() {
            task_state.update_spinner();
            // Only update the spinner area, not the whole screen
            print!("\x1b[s"); // Save cursor
            print!("\x1B[1;1H"); // Move to status bar
            ui_components::show_enhanced_status_bar(&task_state);
            print!("\x1b[u"); // Restore cursor
            io::stdout().flush().ok();
        }

        // Get user input (check if channel is still alive first)
        let input = if suggestion_tx.is_closed() {
            println!("{}✗ Suggestion service unavailable{}", Colors::RED, Colors::RESET);
            // Create a dummy channel for this iteration
            let (tx, _) = mpsc::unbounded_channel::<String>();
            match get_enhanced_input_with_status(&tx, &mut task_state).await {
                Ok(input) => input,
                Err(e) => {
                    println!("{}✗ Input error: {}{}", Colors::RED, e, Colors::RESET);
                    break;
                }
            }
        } else {
            match get_enhanced_input_with_status(&suggestion_tx, &mut task_state).await {
                Ok(input) => input,
                Err(e) => {
                    println!("{}✗ Input error: {}{}", Colors::RED, e, Colors::RESET);
                    break;
                }
            }
        };

        if input.trim().is_empty() {
            continue;
        }

        // Process command
        match command_processor.process(&input) {
            Ok(command_processor::CommandResult::Exit) => {
                println!("{}• Goodbye!{}", Colors::CYAN, Colors::RESET);
                break;
            }
            Ok(command_processor::CommandResult::Clear) => {
                print!("\x1B[2J\x1B[1;1H");
                ui_components::show_welcome_box();
                chat_history.clear();
                continue;
            }
            Ok(command_processor::CommandResult::Success(msg)) => {
                println!("{}", msg);
                continue;
            }
            Ok(command_processor::CommandResult::Error(err)) => {
                println!("{}✗ {}{}", Colors::RED, err, Colors::RESET);
                continue;
            }
            Ok(command_processor::CommandResult::Continue) => {
                // Process as normal message
            }
            Err(e) => {
                println!("{}✗ Command error: {}{}", Colors::RED, e, Colors::RESET);
                continue;
            }
        }

        // Add to chat history (single source)
        chat_history.push(format!("User: {}", input));

        // Limit history size to prevent memory leak
        if chat_history.len() > 1000 {
            chat_history.remove(0);
        }

        // Process with AI
        println!("{}• User: {}{}{}", Colors::GREEN, Colors::BOLD, input, Colors::RESET);

        if let Err(e) = ai_integration::process_with_realtime_ai(
            input.to_string(),
            &ai_service,
            &mut chat_history
        ).await {
            println!("{}✗ Error: {}{}", Colors::RED, e, Colors::RESET);
        }
    }

    // Signal shutdown to background tasks
    shutdown_flag.store(true, std::sync::atomic::Ordering::Relaxed);

    // Wait for tasks to finish (with timeout)
    let _ = tokio::time::timeout(Duration::from_secs(1), suggestion_handle).await;
    let _ = tokio::time::timeout(Duration::from_secs(1), spinner_handle).await;

    // Cleanup terminal
    crossterm_disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;

    info!("Chat application terminated");
    Ok(())
}

/// Get enhanced input with status bar
pub async fn get_enhanced_input_with_status(
    suggestion_tx: &mpsc::UnboundedSender<String>,
    task_state: &mut TaskState,
) -> Result<String> {
    terminal_utils::show_input_border();
    terminal_utils::print_input_prompt();

    let mut input_state = InputState::new();
    input_state.history_index = input_state.command_history.len();

    loop {
        let ch = input_handler::InputHandler::read_character()?;

        // Handle Ctrl+T for task mode toggle
        if let input_handler::InputChar::CtrlT = ch {
            task_state.toggle_input_mode();

            // Update display without clearing screen
            print!("\x1b[s"); // Save cursor position
            print!("\x1B[1;1H"); // Move to top
            ui_components::show_enhanced_status_bar(task_state);

            if task_state.input_mode == InputMode::TaskSelection {
                print!("\x1B[5;1H"); // Move to task area
                print!("\x1b[J"); // Clear from here down
                ui_components::show_task_details_below_input(task_state);
                terminal_utils::show_input_border();
                terminal_utils::print_input_prompt();
                terminal_utils::redraw_input_line(&input_state.input)?;
            } else {
                // Clear task details area when switching back
                print!("\x1B[5;1H"); // Move to task area
                print!("\x1b[J"); // Clear from here down
                terminal_utils::show_input_border();
                terminal_utils::print_input_prompt();
                terminal_utils::redraw_input_line(&input_state.input)?;
            }

            print!("\x1b[u"); // Restore cursor position
            io::stdout().flush().ok();
            continue;
        }

        // Process input based on mode
        if task_state.input_mode == InputMode::TaskSelection {
            match ch {
                input_handler::InputChar::Arrow(input_handler::ArrowKey::Up) => {
                    task_state.navigate_todo_up();
                    if let Some(selected_task) = task_state.get_selected_task_details() {
                        task_state.current_reasoning = selected_task.reasoning.clone();
                    }
                    // Don't clear screen! Just update the relevant parts
                    print!("\x1b[s"); // Save cursor position
                    print!("\x1B[1;1H"); // Move to top
                    ui_components::show_enhanced_status_bar(task_state);
                    print!("\x1B[5;1H"); // Move to task details area
                    print!("\x1b[J"); // Clear from cursor to end of screen
                    ui_components::show_task_details_below_input(task_state);
                    print!("\x1b[u"); // Restore cursor position
                    io::stdout().flush().ok();
                }
                input_handler::InputChar::Arrow(input_handler::ArrowKey::Down) => {
                    task_state.navigate_todo_down();
                    if let Some(selected_task) = task_state.get_selected_task_details() {
                        task_state.current_reasoning = selected_task.reasoning.clone();
                    }
                    // Don't clear screen! Just update the relevant parts
                    print!("\x1b[s"); // Save cursor position
                    print!("\x1B[1;1H"); // Move to top
                    ui_components::show_enhanced_status_bar(task_state);
                    print!("\x1B[5;1H"); // Move to task details area
                    print!("\x1b[J"); // Clear from cursor to end of screen
                    ui_components::show_task_details_below_input(task_state);
                    print!("\x1b[u"); // Restore cursor position
                    io::stdout().flush().ok();
                }
                input_handler::InputChar::Enter => {
                    task_state.toggle_todo(task_state.selected_todo_index);
                    // Don't clear screen! Just update the relevant parts
                    print!("\x1b[s"); // Save cursor position
                    print!("\x1B[1;1H"); // Move to top
                    ui_components::show_enhanced_status_bar(task_state);
                    print!("\x1B[5;1H"); // Move to task details area
                    print!("\x1b[J"); // Clear from cursor to end of screen
                    ui_components::show_task_details_below_input(task_state);
                    print!("\x1b[u"); // Restore cursor position
                    io::stdout().flush().ok();
                }
                _ => {}
            }

            terminal_utils::show_input_border();
            terminal_utils::print_input_prompt();
            terminal_utils::redraw_input_line(&input_state.input)?;

            continue;
        }

        // Normal input mode processing
        if let Some(result) = input_handler::InputHandler::process_input(
            &mut input_state,
            ch,
            suggestion_tx
        ).await? {
            terminal_utils::close_input_border();
            return Ok(result);
        }

        // Update display
        terminal_utils::redraw_input_line(&input_state.input)?;

        // Show suggestions if available
        if !input_state.suggestions.is_empty() {
            terminal_utils::show_navigable_suggestions(
                &input_state.suggestions,
                input_state.selected_suggestion
            );
        }
    }
}

/// Handle enter key for enhanced input
pub async fn handle_enter_key_enhanced(state: &mut InputState) -> Result<String> {
    input_handler::disable_raw_mode()?;
    terminal_utils::clear_suggestions_display();
    println!();
    terminal_utils::close_input_border();

    // Add to command history
    state.add_to_history(state.input.clone());

    Ok(state.input.clone())
}

/// Run chat UI tests
pub async fn run_chat_ui_tests() -> Result<()> {
    println!("Running chat UI tests...");

    // Test fuzzy matcher
    let matcher = fuzzy_matcher::FuzzyMatcher::new(0.3);
    assert!(matcher.score("hel", "hello") > 0.8);
    println!("✓ Fuzzy matcher test passed");

    // Test task state
    let mut task_state = TaskState::new();
    task_state.update_spinner();
    assert_eq!(task_state.spinner_frame, 1);
    println!("✓ Task state test passed");

    // Test input state
    let mut input_state = InputState::new();
    input_state.insert_char('t');
    input_state.insert_char('e');
    input_state.insert_char('s');
    input_state.insert_char('t');
    assert_eq!(input_state.input, "test");
    println!("✓ Input state test passed");

    // Test command processor
    let processor = command_processor::CommandProcessor::new();
    match processor.process("/help") {
        Ok(command_processor::CommandResult::Success(_)) => {
            println!("✓ Command processor test passed");
        }
        _ => {
            return Err(anyhow!("Command processor test failed"));
        }
    }

    println!("\nAll tests passed!");
    Ok(())
}