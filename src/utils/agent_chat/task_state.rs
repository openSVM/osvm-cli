//! Task state management for tracking chat operations and UI state

use serde::{Serialize, Deserialize};

/// Priority levels for todo items
#[derive(Debug, Clone)]
pub enum TodoPriority {
    High,
    Medium,
    Low,
}

/// Input modes for the UI
#[derive(Debug, Clone, PartialEq)]
pub enum InputMode {
    InputField,
    TaskSelection,
}

/// Individual todo item with execution tracking
#[derive(Debug, Clone)]
pub struct TodoItem {
    pub text: String,
    pub completed: bool,
    pub priority: TodoPriority,
    pub reasoning: String,
    pub tool_plan: Option<String>,
    pub execution_results: Option<String>,
}

impl TodoItem {
    /// Create a new high-priority todo item
    pub fn new_high(text: String, reasoning: String) -> Self {
        Self {
            text,
            completed: false,
            priority: TodoPriority::High,
            reasoning,
            tool_plan: None,
            execution_results: None,
        }
    }

    /// Mark as completed with results
    pub fn complete(&mut self, results: String) {
        self.completed = true;
        self.execution_results = Some(results);
    }
}

/// Spinner animation frames
pub const SPINNER_FRAMES: &[&str] = &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

/// Main task state for the chat application
#[derive(Debug, Clone)]
pub struct TaskState {
    pub current_task: String,
    pub todo_items: Vec<TodoItem>,
    pub current_reasoning: String,
    pub selected_todo_index: usize,
    pub input_mode: InputMode,
    pub spinner_frame: usize,
}

impl TaskState {
    /// Create new task state with default initialization tasks
    pub fn new() -> Self {
        Self {
            current_task: "Initialize OSVM Agent".to_string(),
            todo_items: Self::default_todos(),
            current_reasoning: "Setting up OSVM agent environment and loading necessary services...".to_string(),
            selected_todo_index: 0,
            input_mode: InputMode::InputField,
            spinner_frame: 0,
        }
    }

    /// Create empty task state without any mockup data
    pub fn new_empty() -> Self {
        Self {
            current_task: String::new(),
            todo_items: Vec::new(),
            current_reasoning: String::new(),
            selected_todo_index: 0,
            input_mode: InputMode::InputField,
            spinner_frame: 0,
        }
    }

    /// Default initialization todos
    fn default_todos() -> Vec<TodoItem> {
        vec![
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
        ]
    }

    /// Update spinner animation frame
    pub fn update_spinner(&mut self) {
        self.spinner_frame = (self.spinner_frame + 1) % SPINNER_FRAMES.len();
    }

    /// Toggle todo item completion
    pub fn toggle_todo(&mut self, index: usize) {
        if index < self.todo_items.len() && !self.todo_items[index].completed {
            self.todo_items[index].complete("✅ Task completed successfully".to_string());
        }
    }

    /// Navigate to previous todo item
    pub fn navigate_todo_up(&mut self) {
        if !self.todo_items.is_empty() {
            if self.selected_todo_index > 0 {
                self.selected_todo_index -= 1;
            } else {
                self.selected_todo_index = self.todo_items.len().saturating_sub(1);
            }
        }
    }

    /// Navigate to next todo item
    pub fn navigate_todo_down(&mut self) {
        if !self.todo_items.is_empty() {
            self.selected_todo_index = (self.selected_todo_index + 1) % self.todo_items.len();
        }
    }

    /// Get currently selected task details
    pub fn get_selected_task_details(&self) -> Option<&TodoItem> {
        self.todo_items.get(self.selected_todo_index)
    }

    /// Switch between input modes
    pub fn toggle_input_mode(&mut self) {
        self.input_mode = match self.input_mode {
            InputMode::InputField => InputMode::TaskSelection,
            InputMode::TaskSelection => InputMode::InputField,
        };
    }
}