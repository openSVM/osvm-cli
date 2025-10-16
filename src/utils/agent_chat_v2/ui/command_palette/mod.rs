//! Command Palette - Universal quick action system (like VS Code's Ctrl+P)
//! This is the feature that makes users think "how did I live without this?"

use cursive::views::{Dialog, EditView, LinearLayout, ListView, TextView};
use cursive::{Cursive, View};
use fuzzy_matcher::FuzzyMatcher;
use fuzzy_matcher::skim::SkimMatcherV2;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

pub mod actions;
pub mod providers;
pub mod search;

/// The command that users can't live without - instant access to everything
pub struct CommandPalette {
    // Core components
    commands: Vec<Command>,
    providers: Vec<Box<dyn CommandProvider>>,
    matcher: SkimMatcherV2,

    // State
    search_query: String,
    filtered_commands: Vec<CommandMatch>,
    selected_index: usize,

    // History for smart sorting
    usage_history: HashMap<String, usize>,
    recent_commands: Vec<String>,

    // Configuration
    max_results: usize,
    show_descriptions: bool,
    show_shortcuts: bool,
}

/// A single command in the palette
#[derive(Clone, Debug)]
pub struct Command {
    pub id: String,
    pub label: String,
    pub description: Option<String>,
    pub category: CommandCategory,
    pub shortcut: Option<String>,
    pub icon: Option<String>,
    pub action: CommandAction,
    pub keywords: Vec<String>, // Additional search terms
}

#[derive(Clone, Debug, PartialEq)]
pub enum CommandCategory {
    Navigation,    // Go to file, session, etc.
    Action,        // Send, receive, stake, etc.
    Toggle,        // Settings, features on/off
    Theme,         // Theme switching
    Tool,          // MCP tools
    Help,          // Documentation, tips
    Recent,        // Recently used
    Custom,        // User-defined
}

#[derive(Clone, Debug)]
pub enum CommandAction {
    // Navigation
    GoToSession(String),
    GoToFile(String),
    GoToLine(usize),

    // Actions
    SendTransaction,
    CheckBalance,
    StakeSOL,
    ExecuteTool(String),

    // UI Actions
    SwitchTheme(String),
    ToggleAnimations,
    ToggleSound,
    OpenSettings,

    // Chat Actions
    ClearChat,
    ExportChat,
    StartRecording,

    // Custom callback
    Custom(Arc<dyn Fn(&mut Cursive) + Send + Sync>),
}

/// Fuzzy match result with score
struct CommandMatch {
    command: Command,
    score: i64,
    matched_indices: Vec<usize>,
}

impl CommandPalette {
    pub fn new() -> Self {
        let mut palette = Self {
            commands: Vec::new(),
            providers: Vec::new(),
            matcher: SkimMatcherV2::default(),
            search_query: String::new(),
            filtered_commands: Vec::new(),
            selected_index: 0,
            usage_history: HashMap::new(),
            recent_commands: Vec::with_capacity(10),
            max_results: 10,
            show_descriptions: true,
            show_shortcuts: true,
        };

        palette.register_default_commands();
        palette
    }

    /// Register all default commands - the essentials users need
    fn register_default_commands(&mut self) {
        // Navigation commands
        self.add_command(Command {
            id: "go_to_session".to_string(),
            label: "Go to Session...".to_string(),
            description: Some("Switch to a different chat session".to_string()),
            category: CommandCategory::Navigation,
            shortcut: Some("Ctrl+K S".to_string()),
            icon: Some("🔄".to_string()),
            action: CommandAction::Custom(Arc::new(|s| {
                // Would show session picker
            })),
            keywords: vec!["switch", "session", "chat", "tab"].into_iter()
                .map(String::from).collect(),
        });

        // Action commands - what users do most
        self.add_command(Command {
            id: "send_sol".to_string(),
            label: "Send SOL".to_string(),
            description: Some("Send SOL to an address".to_string()),
            category: CommandCategory::Action,
            shortcut: Some("Ctrl+S".to_string()),
            icon: Some("💸".to_string()),
            action: CommandAction::SendTransaction,
            keywords: vec!["transfer", "pay", "transaction"].into_iter()
                .map(String::from).collect(),
        });

        self.add_command(Command {
            id: "check_balance".to_string(),
            label: "Check Balance".to_string(),
            description: Some("View current wallet balance".to_string()),
            category: CommandCategory::Action,
            shortcut: Some("Ctrl+B".to_string()),
            icon: Some("💰".to_string()),
            action: CommandAction::CheckBalance,
            keywords: vec!["wallet", "funds", "sol"].into_iter()
                .map(String::from).collect(),
        });

        // Theme commands - visual customization
        for theme in &["vscode", "cyberpunk", "nord", "tokyo_night", "dracula"] {
            self.add_command(Command {
                id: format!("theme_{}", theme),
                label: format!("Theme: {}", theme.replace('_', " ").to_uppercase()),
                description: Some(format!("Switch to {} theme", theme)),
                category: CommandCategory::Theme,
                shortcut: None,
                icon: Some("🎨".to_string()),
                action: CommandAction::SwitchTheme(theme.to_string()),
                keywords: vec!["theme", "color", "appearance", theme].into_iter()
                    .map(String::from).collect(),
            });
        }

        // Toggle commands - quick settings
        self.add_command(Command {
            id: "toggle_animations".to_string(),
            label: "Toggle Animations".to_string(),
            description: Some("Enable/disable animations".to_string()),
            category: CommandCategory::Toggle,
            shortcut: Some("Ctrl+A".to_string()),
            icon: Some("✨".to_string()),
            action: CommandAction::ToggleAnimations,
            keywords: vec!["animation", "effects", "motion"].into_iter()
                .map(String::from).collect(),
        });

        // Help commands
        self.add_command(Command {
            id: "show_shortcuts".to_string(),
            label: "Keyboard Shortcuts".to_string(),
            description: Some("View all keyboard shortcuts".to_string()),
            category: CommandCategory::Help,
            shortcut: Some("?".to_string()),
            icon: Some("⌨️".to_string()),
            action: CommandAction::Custom(Arc::new(|s| {
                // Show shortcuts dialog
            })),
            keywords: vec!["keys", "hotkeys", "bindings"].into_iter()
                .map(String::from).collect(),
        });
    }

    /// Add a command to the palette
    pub fn add_command(&mut self, command: Command) {
        self.commands.push(command);
    }

    /// Search commands with fuzzy matching
    pub fn search(&mut self, query: &str) {
        self.search_query = query.to_string();
        self.filtered_commands.clear();

        if query.is_empty() {
            // Show recent commands when no query
            self.show_recent_commands();
            return;
        }

        // BUG-2010 fix: Use char-safe slicing for command palette prefixes
        // Prevents panics on emoji or multi-byte characters
        let (prefix, actual_query) = if let Some(first_char) = query.chars().next() {
            match first_char {
                '>' => {
                    let rest = query.chars().skip(1).collect::<String>();
                    (Some('>'), rest)
                },
                '@' => {
                    let rest = query.chars().skip(1).collect::<String>();
                    (Some('@'), rest)
                },
                '#' => {
                    let rest = query.chars().skip(1).collect::<String>();
                    (Some('#'), rest)
                },
                '?' => {
                    let rest = query.chars().skip(1).collect::<String>();
                    (Some('?'), rest)
                },
                _ => (None, query.to_string()),
            }
        } else {
            (None, query.to_string())
        };

        let actual_query_ref = actual_query.as_str();

        // Filter by prefix if present
        let commands_to_search: Vec<&Command> = match prefix {
            Some('>') => self.commands.iter()
                .filter(|c| c.category == CommandCategory::Action)
                .collect(),
            Some('@') => self.commands.iter()
                .filter(|c| c.category == CommandCategory::Navigation)
                .collect(),
            Some('#') => self.commands.iter()
                .filter(|c| c.category == CommandCategory::Theme)
                .collect(),
            Some('?') => self.commands.iter()
                .filter(|c| c.category == CommandCategory::Help)
                .collect(),
            _ => self.commands.iter().collect(),
        };

        // Fuzzy match each command
        for command in commands_to_search {
            // Build search text from label, description, and keywords
            let search_text = format!(
                "{} {} {}",
                command.label,
                command.description.as_ref().unwrap_or(&String::new()),
                command.keywords.join(" ")
            );

            if let Some(score) = self.matcher.fuzzy_match(&search_text, actual_query_ref) {
                // Boost score based on usage history
                let usage_boost = self.usage_history.get(&command.id)
                    .unwrap_or(&0) * 10;

                self.filtered_commands.push(CommandMatch {
                    command: command.clone(),
                    score: score + usage_boost as i64,
                    matched_indices: Vec::new(),
                });
            }
        }

        // Sort by score (highest first)
        self.filtered_commands.sort_by(|a, b| b.score.cmp(&a.score));

        // Limit results
        self.filtered_commands.truncate(self.max_results);
    }

    /// Show recent commands when no search query
    fn show_recent_commands(&mut self) {
        for cmd_id in self.recent_commands.iter().rev().take(5) {
            if let Some(command) = self.commands.iter().find(|c| &c.id == cmd_id) {
                self.filtered_commands.push(CommandMatch {
                    command: command.clone(),
                    score: 100,
                    matched_indices: Vec::new(),
                });
            }
        }
    }

    /// Execute the selected command
    pub fn execute_selected(&mut self, siv: &mut Cursive) {
        if let Some(matched) = self.filtered_commands.get(self.selected_index) {
            // Track usage for better ranking
            *self.usage_history.entry(matched.command.id.clone())
                .or_insert(0) += 1;

            // Add to recent commands
            self.recent_commands.retain(|id| id != &matched.command.id);
            self.recent_commands.push(matched.command.id.clone());
            if self.recent_commands.len() > 10 {
                self.recent_commands.remove(0);
            }

            // Execute the action
            self.execute_action(&matched.command.action, siv);

            // Close palette
            siv.pop_layer();
        }
    }

    /// Execute a command action
    fn execute_action(&self, action: &CommandAction, siv: &mut Cursive) {
        match action {
            CommandAction::Custom(callback) => callback(siv),
            CommandAction::SwitchTheme(theme) => {
                // Would switch theme here
                log::info!("Switching to theme: {}", theme);
            },
            CommandAction::CheckBalance => {
                // Would check balance
                log::info!("Checking balance...");
            },
            // ... handle other actions
            _ => {}
        }
    }
}

/// Trait for command providers (plugins can add commands)
pub trait CommandProvider: Send + Sync {
    fn get_commands(&self) -> Vec<Command>;
    fn refresh(&mut self);
}

/// Show the command palette dialog
pub fn show_command_palette(siv: &mut Cursive) {
    let palette = Arc::new(RwLock::new(CommandPalette::new()));

    let mut layout = LinearLayout::vertical();

    // Search input with placeholder
    let search_input = EditView::new()
        .on_edit({
            let palette = palette.clone();
            move |s, query, _| {
                if let Ok(mut p) = palette.write() {
                    p.search(query);
                    update_results_view(s);
                }
            }
        })
        .on_submit({
            let palette = palette.clone();
            move |s, _| {
                if let Ok(mut p) = palette.write() {
                    p.execute_selected(s);
                }
            }
        })
        .with_name("palette_search");

    layout.add_child(TextView::new("🔍 Type to search commands, > for actions, @ for sessions, # for themes"));
    layout.add_child(search_input);

    // Results list
    let results = ListView::new()
        .with_name("palette_results");

    layout.add_child(results);

    // Create dialog
    let dialog = Dialog::around(layout)
        .title("Command Palette")
        .padding_lrtb(1, 1, 1, 0)
        .dismiss_button("ESC");

    siv.add_layer(dialog);
    siv.focus_name("palette_search").unwrap();
}

fn update_results_view(siv: &mut Cursive) {
    // Update the results ListView based on filtered commands
    // This would be implemented to show the search results
}

/// Initialize command palette with global shortcut
pub fn init_command_palette(siv: &mut Cursive) {
    // Ctrl+P to open command palette
    siv.add_global_callback(cursive::event::Event::CtrlChar('p'), |s| {
        show_command_palette(s);
    });

    // Ctrl+Shift+P for command palette with '>' prefix
    siv.add_global_callback(
        cursive::event::Event::Shift(cursive::event::Key::F1),
        |s| {
            show_command_palette(s);
            // Pre-fill with '>' for actions
            if let Some(mut input) = s.find_name::<EditView>("palette_search") {
                input.set_content(">");
            }
        }
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuzzy_search() {
        let mut palette = CommandPalette::new();

        // Search for "send"
        palette.search("send");
        assert!(!palette.filtered_commands.is_empty());
        assert!(palette.filtered_commands[0].command.id == "send_sol");

        // Search with prefix
        palette.search(">bal");
        assert!(palette.filtered_commands.iter()
            .any(|m| m.command.id == "check_balance"));
    }
}