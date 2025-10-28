//! Smart autocomplete system for input field
//!
//! Provides intelligent suggestions for commands, tool names, and common patterns

use cursive::theme::{BaseColor, Color, ColorStyle};
use cursive::traits::*;
use cursive::utils::markup::StyledString;
use cursive::views::{LinearLayout, TextView};
use cursive::{Cursive, View};
use std::sync::{Arc, RwLock};

use super::super::state::AdvancedChatState;
use crate::services::mcp_service::McpTool;

/// Autocomplete context and suggestions
pub struct AutocompleteEngine {
    /// Available commands
    commands: Vec<CommandSuggestion>,
    /// Available MCP tools for @ mentions
    mcp_tools: Vec<ToolSuggestion>,
    /// Common phrases and templates
    templates: Vec<TemplateSuggestion>,
    /// Current input text
    current_text: String,
    /// Cursor position
    cursor_pos: usize,
    /// Active suggestions
    suggestions: Vec<String>,
}

#[derive(Clone)]
struct CommandSuggestion {
    trigger: String,     // e.g., "/help"
    description: String, // e.g., "Show help information"
    completion: String,  // Full text to insert
}

#[derive(Clone)]
struct ToolSuggestion {
    name: String,
    server: String,
    description: String,
}

#[derive(Clone)]
struct TemplateSuggestion {
    trigger: String,
    template: String,
}

impl AutocompleteEngine {
    pub fn new() -> Self {
        let commands = vec![
            CommandSuggestion {
                trigger: "/help".to_string(),
                description: "Show help information".to_string(),
                completion: "/help".to_string(),
            },
            CommandSuggestion {
                trigger: "/clear".to_string(),
                description: "Clear the chat history".to_string(),
                completion: "/clear".to_string(),
            },
            CommandSuggestion {
                trigger: "/export".to_string(),
                description: "Export chat to file".to_string(),
                completion: "/export".to_string(),
            },
            CommandSuggestion {
                trigger: "/theme".to_string(),
                description: "Change color theme".to_string(),
                completion: "/theme".to_string(),
            },
            CommandSuggestion {
                trigger: "/fork".to_string(),
                description: "Fork current conversation".to_string(),
                completion: "/fork".to_string(),
            },
            CommandSuggestion {
                trigger: "/session".to_string(),
                description: "Manage chat sessions".to_string(),
                completion: "/session".to_string(),
            },
            CommandSuggestion {
                trigger: "/tools".to_string(),
                description: "List available MCP tools".to_string(),
                completion: "/tools".to_string(),
            },
            CommandSuggestion {
                trigger: "/quit".to_string(),
                description: "Exit the application".to_string(),
                completion: "/quit".to_string(),
            },
        ];

        let templates = vec![
            TemplateSuggestion {
                trigger: "deploy".to_string(),
                template: "Deploy a validator to ".to_string(),
            },
            TemplateSuggestion {
                trigger: "analyze".to_string(),
                template: "Analyze the ".to_string(),
            },
            TemplateSuggestion {
                trigger: "explain".to_string(),
                template: "Explain how ".to_string(),
            },
            TemplateSuggestion {
                trigger: "debug".to_string(),
                template: "Help me debug ".to_string(),
            },
            TemplateSuggestion {
                trigger: "optimize".to_string(),
                template: "Optimize the performance of ".to_string(),
            },
        ];

        AutocompleteEngine {
            commands,
            mcp_tools: Vec::new(),
            templates,
            current_text: String::new(),
            cursor_pos: 0,
            suggestions: Vec::new(),
        }
    }

    /// Update with available MCP tools
    pub fn update_mcp_tools(&mut self, state: &AdvancedChatState) {
        self.mcp_tools.clear();

        if let Ok(tools) = state.available_tools.read() {
            for (server_id, tool_list) in tools.iter() {
                for tool in tool_list {
                    self.mcp_tools.push(ToolSuggestion {
                        name: tool.name.clone(),
                        server: server_id.clone(),
                        description: tool.description.clone().unwrap_or_default(),
                    });
                }
            }
        }
    }

    /// Process input and generate suggestions
    pub fn process_input(&mut self, text: &str, cursor_pos: usize) -> Vec<String> {
        self.current_text = text.to_string();
        self.cursor_pos = cursor_pos;
        self.suggestions.clear();

        // Get the word at cursor position
        let (word_start, current_word) = self.get_word_at_cursor();

        // Check for command completion (starts with /)
        if current_word.starts_with('/') {
            for cmd in &self.commands {
                if cmd.trigger.starts_with(&current_word) && cmd.trigger != current_word {
                    self.suggestions
                        .push(format!("{} - {}", cmd.trigger, cmd.description));
                }
            }
        }
        // Check for tool mention (starts with @)
        else if current_word.starts_with('@') {
            let search = if current_word.len() > 1 {
                current_word[1..].to_lowercase()
            } else {
                String::new()
            };
            for tool in &self.mcp_tools {
                if tool.name.to_lowercase().contains(&search) {
                    self.suggestions
                        .push(format!("@{} ({})", tool.name, tool.server));
                }
            }
            // Limit to 5 suggestions
            self.suggestions.truncate(5);
        }
        // Check for template triggers
        else if !current_word.is_empty() {
            for template in &self.templates {
                if template.trigger.starts_with(&current_word.to_lowercase()) {
                    self.suggestions.push(template.template.clone());
                }
            }
        }

        self.suggestions.clone()
    }

    /// Get the word at the current cursor position
    fn get_word_at_cursor(&self) -> (usize, String) {
        if self.current_text.is_empty() {
            return (0, String::new());
        }

        let bytes = self.current_text.as_bytes();
        let mut start = self.cursor_pos.min(bytes.len());

        // Find word start
        while start > 0 && !bytes[start - 1].is_ascii_whitespace() {
            start -= 1;
        }

        let mut end = self.cursor_pos.min(bytes.len());
        // Find word end
        while end < bytes.len() && !bytes[end].is_ascii_whitespace() {
            end += 1;
        }

        let word = self.current_text[start..end].to_string();
        (start, word)
    }

    /// Apply the selected suggestion
    pub fn apply_suggestion(&self, suggestion_index: usize) -> Option<String> {
        if suggestion_index >= self.suggestions.len() {
            return None;
        }

        let suggestion = &self.suggestions[suggestion_index];
        let (word_start, current_word) = self.get_word_at_cursor();

        // Extract the completion text
        let completion = if suggestion.starts_with('/') {
            // Command suggestion - take just the command part
            suggestion
                .split(" - ")
                .next()
                .unwrap_or(suggestion)
                .to_string()
        } else if suggestion.starts_with('@') {
            // Tool suggestion - take just the tool name
            suggestion
                .split(" (")
                .next()
                .unwrap_or(suggestion)
                .to_string()
        } else {
            // Template - use as is
            suggestion.clone()
        };

        // Build the new text with the completion
        let mut new_text = String::new();
        new_text.push_str(&self.current_text[..word_start]);
        new_text.push_str(&completion);

        // Add space after completion if it's a command or tool
        if completion.starts_with('/') || completion.starts_with('@') {
            new_text.push(' ');
        }

        // Add any text after the current word
        let word_end = word_start + current_word.len();
        if word_end < self.current_text.len() {
            new_text.push_str(&self.current_text[word_end..]);
        }

        Some(new_text)
    }
}

/// Create autocomplete suggestion view
pub fn create_autocomplete_view(suggestions: &[String]) -> impl View {
    let mut layout = LinearLayout::vertical();

    // Create styled suggestions
    for (i, suggestion) in suggestions.iter().enumerate() {
        let mut styled = StyledString::new();

        // Highlight style
        let highlight_style = if i == 0 {
            // First item is selected
            ColorStyle::new(Color::Dark(BaseColor::Black), Color::Light(BaseColor::Cyan))
        } else {
            ColorStyle::new(Color::Light(BaseColor::White), Color::Dark(BaseColor::Blue))
        };

        styled.append_styled(format!(" {} ", suggestion), highlight_style);

        layout.add_child(TextView::new(styled));
    }

    layout
}

/// Initialize autocomplete for the input field
pub fn init_autocomplete(s: &mut Cursive) {
    // This would integrate with the TextArea input field
    // to show suggestions as the user types
}

/// Handle Tab key for autocomplete
pub fn handle_autocomplete_tab(s: &mut Cursive) -> bool {
    // Get current input text
    if let Some(input) = s.find_name::<cursive::views::TextArea>("input") {
        let content = input.get_content();
        let cursor_pos = content.len(); // Simplified - would need real cursor position

        // Create autocomplete engine
        let mut engine = AutocompleteEngine::new();

        // Update with MCP tools if available
        if let Some(state) = s.user_data::<AdvancedChatState>() {
            engine.update_mcp_tools(state);
        }

        // Get suggestions
        let suggestions = engine.process_input(&content, cursor_pos);

        if !suggestions.is_empty() {
            // Apply first suggestion
            if let Some(completed) = engine.apply_suggestion(0) {
                if let Some(mut input) = s.find_name::<cursive::views::TextArea>("input") {
                    input.set_content(completed);
                }
                return true; // Handled
            }
        }
    }

    false // Not handled
}

/// Show inline autocomplete suggestions
pub fn show_inline_suggestions(s: &mut Cursive, suggestions: Vec<String>) {
    // Find or create suggestion container
    if let Some(mut container) = s.find_name::<LinearLayout>("autocomplete_container") {
        container.clear();

        if !suggestions.is_empty() {
            // Add suggestion hint
            container.add_child(TextView::new("↹ Tab to complete:"));

            // Add suggestions
            let suggestion_view = create_autocomplete_view(&suggestions);
            container.add_child(suggestion_view);
        }
    }
}
