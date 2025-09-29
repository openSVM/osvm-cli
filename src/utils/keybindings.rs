//! Custom keybinding system for agent chat interface
//!
//! This module provides a flexible keybinding system that allows users to
//! customize keyboard shortcuts and create their own command mappings.

use anyhow::{anyhow, Result};
use log::{debug, error, warn};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

/// Keybinding action types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum KeyAction {
    // Navigation actions
    MoveUp,
    MoveDown,
    MoveLeft,
    MoveRight,
    PageUp,
    PageDown,
    Home,
    End,

    // Input actions
    Accept,
    Cancel,
    Delete,
    Backspace,
    Clear,

    // Suggestion actions
    NextSuggestion,
    PrevSuggestion,
    AcceptSuggestion,

    // Command actions
    ShowHelp,
    ShowContext,
    ShowStatus,
    ShowTools,
    ClearHistory,

    // Custom commands
    ExecuteCommand(String),
    InsertText(String),

    // Mode switching
    ToggleFuzzySearch,
    ToggleVimMode,
    EnterCommandMode,

    // Advanced features
    RecordMacro,
    PlayMacro,
    SaveSession,
    LoadSession,
}

/// Key combination representation
#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct KeyCombo {
    pub key: String,
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub meta: bool,
}

impl KeyCombo {
    /// Create a new key combination
    pub fn new(key: &str) -> Self {
        Self {
            key: key.to_string(),
            ctrl: false,
            alt: false,
            shift: false,
            meta: false,
        }
    }

    /// Add Ctrl modifier
    pub fn ctrl(mut self) -> Self {
        self.ctrl = true;
        self
    }

    /// Add Alt modifier
    pub fn alt(mut self) -> Self {
        self.alt = true;
        self
    }

    /// Add Shift modifier
    pub fn shift(mut self) -> Self {
        self.shift = true;
        self
    }

    /// Add Meta/Super modifier
    pub fn meta(mut self) -> Self {
        self.meta = true;
        self
    }

    /// Parse key combination from string (e.g., "Ctrl+C", "Alt+Shift+F1")
    pub fn parse(input: &str) -> Result<Self> {
        let parts: Vec<&str> = input.split('+').collect();
        if parts.is_empty() {
            return Err(anyhow!("Empty key combination"));
        }

        let mut combo = KeyCombo::new(parts.last().unwrap());

        for modifier in &parts[..parts.len() - 1] {
            match modifier.to_lowercase().as_str() {
                "ctrl" | "control" => combo.ctrl = true,
                "alt" | "option" => combo.alt = true,
                "shift" => combo.shift = true,
                "meta" | "super" | "cmd" => combo.meta = true,
                _ => return Err(anyhow!("Unknown modifier: {}", modifier)),
            }
        }

        Ok(combo)
    }

    /// Format key combination as human-readable string
    pub fn format(&self) -> String {
        let mut parts = Vec::new();

        if self.ctrl {
            parts.push("Ctrl");
        }
        if self.alt {
            parts.push("Alt");
        }
        if self.shift {
            parts.push("Shift");
        }
        if self.meta {
            parts.push("Meta");
        }
        parts.push(&self.key);

        parts.join("+")
    }
}

/// Keybinding configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeybindingConfig {
    pub bindings: HashMap<KeyCombo, KeyAction>,
    pub vim_mode: bool,
    pub emacs_mode: bool,
    pub custom_commands: HashMap<String, String>,
}

impl Default for KeybindingConfig {
    fn default() -> Self {
        let mut bindings = HashMap::new();

        // Default navigation bindings
        bindings.insert(KeyCombo::new("ArrowUp"), KeyAction::MoveUp);
        bindings.insert(KeyCombo::new("ArrowDown"), KeyAction::MoveDown);
        bindings.insert(KeyCombo::new("ArrowLeft"), KeyAction::MoveLeft);
        bindings.insert(KeyCombo::new("ArrowRight"), KeyAction::MoveRight);
        bindings.insert(KeyCombo::new("PageUp"), KeyAction::PageUp);
        bindings.insert(KeyCombo::new("PageDown"), KeyAction::PageDown);
        bindings.insert(KeyCombo::new("Home"), KeyAction::Home);
        bindings.insert(KeyCombo::new("End"), KeyAction::End);

        // Input bindings
        bindings.insert(KeyCombo::new("Enter"), KeyAction::Accept);
        bindings.insert(KeyCombo::new("Escape"), KeyAction::Cancel);
        bindings.insert(KeyCombo::new("Delete"), KeyAction::Delete);
        bindings.insert(KeyCombo::new("Backspace"), KeyAction::Backspace);
        bindings.insert(KeyCombo::new("Tab"), KeyAction::AcceptSuggestion);

        // Command bindings with modifiers
        bindings.insert(KeyCombo::new("h").ctrl(), KeyAction::ShowHelp);
        bindings.insert(KeyCombo::new("c").ctrl(), KeyAction::Cancel);
        bindings.insert(KeyCombo::new("l").ctrl(), KeyAction::Clear);
        bindings.insert(KeyCombo::new("u").ctrl(), KeyAction::ClearHistory);

        // Function key bindings
        bindings.insert(KeyCombo::new("F1"), KeyAction::ShowHelp);
        bindings.insert(KeyCombo::new("F2"), KeyAction::ShowContext);
        bindings.insert(KeyCombo::new("F3"), KeyAction::ShowStatus);
        bindings.insert(KeyCombo::new("F4"), KeyAction::ShowTools);

        // Advanced bindings
        bindings.insert(KeyCombo::new("r").ctrl().shift(), KeyAction::RecordMacro);
        bindings.insert(KeyCombo::new("p").ctrl().shift(), KeyAction::PlayMacro);
        bindings.insert(KeyCombo::new("s").ctrl(), KeyAction::SaveSession);
        bindings.insert(KeyCombo::new("o").ctrl(), KeyAction::LoadSession);

        // Toggle features
        bindings.insert(
            KeyCombo::new("f").ctrl().alt(),
            KeyAction::ToggleFuzzySearch,
        );
        bindings.insert(KeyCombo::new("v").ctrl().alt(), KeyAction::ToggleVimMode);

        Self {
            bindings,
            vim_mode: false,
            emacs_mode: false,
            custom_commands: HashMap::new(),
        }
    }
}

impl KeybindingConfig {
    /// Load keybinding configuration from file
    pub fn load() -> Result<Self> {
        let config_path = Self::config_path()?;

        if !config_path.exists() {
            debug!("No keybinding config found, creating default");
            let default_config = Self::default();
            default_config.save()?;
            return Ok(default_config);
        }

        let content = fs::read_to_string(&config_path)
            .map_err(|e| anyhow!("Failed to read keybinding config: {}", e))?;

        let config: Self = serde_json::from_str(&content)
            .map_err(|e| anyhow!("Failed to parse keybinding config: {}", e))?;

        debug!(
            "Loaded keybinding config with {} bindings",
            config.bindings.len()
        );
        Ok(config)
    }

    /// Save keybinding configuration to file
    pub fn save(&self) -> Result<()> {
        let config_path = Self::config_path()?;

        if let Some(parent) = config_path.parent() {
            fs::create_dir_all(parent)
                .map_err(|e| anyhow!("Failed to create config directory: {}", e))?;
        }

        let content = serde_json::to_string_pretty(self)
            .map_err(|e| anyhow!("Failed to serialize keybinding config: {}", e))?;

        fs::write(&config_path, content)
            .map_err(|e| anyhow!("Failed to write keybinding config: {}", e))?;

        debug!("Saved keybinding config to {:?}", config_path);
        Ok(())
    }

    /// Get configuration file path
    fn config_path() -> Result<PathBuf> {
        let home =
            std::env::var("HOME").map_err(|_| anyhow!("HOME environment variable not set"))?;
        Ok(PathBuf::from(home).join(".osvm").join("keybindings.json"))
    }

    /// Get action for key combination
    pub fn get_action(&self, key_combo: &KeyCombo) -> Option<&KeyAction> {
        self.bindings.get(key_combo)
    }

    /// Add or update keybinding
    pub fn set_binding(&mut self, key_combo: KeyCombo, action: KeyAction) {
        self.bindings.insert(key_combo, action);
    }

    /// Remove keybinding
    pub fn remove_binding(&mut self, key_combo: &KeyCombo) -> Option<KeyAction> {
        self.bindings.remove(key_combo)
    }

    /// Add custom command
    pub fn add_custom_command(&mut self, name: String, command: String) {
        self.custom_commands.insert(name, command);
    }

    /// Get custom command
    pub fn get_custom_command(&self, name: &str) -> Option<&String> {
        self.custom_commands.get(name)
    }

    /// Enable Vim-style keybindings
    pub fn enable_vim_mode(&mut self) {
        self.vim_mode = true;
        self.emacs_mode = false;
        self.add_vim_bindings();
    }

    /// Enable Emacs-style keybindings
    pub fn enable_emacs_mode(&mut self) {
        self.emacs_mode = true;
        self.vim_mode = false;
        self.add_emacs_bindings();
    }

    /// Add Vim-style keybindings
    fn add_vim_bindings(&mut self) {
        // Vim navigation
        self.bindings
            .insert(KeyCombo::new("h"), KeyAction::MoveLeft);
        self.bindings
            .insert(KeyCombo::new("j"), KeyAction::MoveDown);
        self.bindings.insert(KeyCombo::new("k"), KeyAction::MoveUp);
        self.bindings
            .insert(KeyCombo::new("l"), KeyAction::MoveRight);

        // Vim commands
        self.bindings.insert(KeyCombo::new("0"), KeyAction::Home);
        self.bindings.insert(KeyCombo::new("$"), KeyAction::End);
        self.bindings.insert(KeyCombo::new("x"), KeyAction::Delete);
        self.bindings
            .insert(KeyCombo::new("i"), KeyAction::EnterCommandMode);

        // Vim shortcuts
        self.bindings
            .insert(KeyCombo::new("d").shift(), KeyAction::ClearHistory);
        self.bindings
            .insert(KeyCombo::new("y").shift(), KeyAction::SaveSession);
    }

    /// Add Emacs-style keybindings
    fn add_emacs_bindings(&mut self) {
        // Emacs navigation
        self.bindings
            .insert(KeyCombo::new("b").ctrl(), KeyAction::MoveLeft);
        self.bindings
            .insert(KeyCombo::new("f").ctrl(), KeyAction::MoveRight);
        self.bindings
            .insert(KeyCombo::new("p").ctrl(), KeyAction::MoveUp);
        self.bindings
            .insert(KeyCombo::new("n").ctrl(), KeyAction::MoveDown);

        // Emacs commands
        self.bindings
            .insert(KeyCombo::new("a").ctrl(), KeyAction::Home);
        self.bindings
            .insert(KeyCombo::new("e").ctrl(), KeyAction::End);
        self.bindings
            .insert(KeyCombo::new("d").ctrl(), KeyAction::Delete);
        self.bindings
            .insert(KeyCombo::new("k").ctrl(), KeyAction::Clear);

        // Emacs shortcuts
        self.bindings
            .insert(KeyCombo::new("x").ctrl().then("h"), KeyAction::ShowHelp);
        self.bindings
            .insert(KeyCombo::new("x").ctrl().then("s"), KeyAction::SaveSession);
    }

    /// List all keybindings
    pub fn list_bindings(&self) -> Vec<(String, String)> {
        self.bindings
            .iter()
            .map(|(combo, action)| (combo.format(), format!("{:?}", action)))
            .collect()
    }

    /// Export keybindings to user-readable format
    pub fn export_readable(&self) -> String {
        let mut output = String::from("# OSVM Agent Chat Keybindings\n\n");

        if self.vim_mode {
            output.push_str("Mode: Vim\n\n");
        } else if self.emacs_mode {
            output.push_str("Mode: Emacs\n\n");
        } else {
            output.push_str("Mode: Default\n\n");
        }

        output.push_str("## Navigation\n");
        output.push_str("| Key | Action |\n");
        output.push_str("|-----|--------|\n");

        for (combo, action) in &self.bindings {
            if matches!(
                action,
                KeyAction::MoveUp
                    | KeyAction::MoveDown
                    | KeyAction::MoveLeft
                    | KeyAction::MoveRight
                    | KeyAction::PageUp
                    | KeyAction::PageDown
                    | KeyAction::Home
                    | KeyAction::End
            ) {
                output.push_str(&format!("| {} | {:?} |\n", combo.format(), action));
            }
        }

        output.push_str("\n## Commands\n");
        output.push_str("| Key | Action |\n");
        output.push_str("|-----|--------|\n");

        for (combo, action) in &self.bindings {
            if !matches!(
                action,
                KeyAction::MoveUp
                    | KeyAction::MoveDown
                    | KeyAction::MoveLeft
                    | KeyAction::MoveRight
                    | KeyAction::PageUp
                    | KeyAction::PageDown
                    | KeyAction::Home
                    | KeyAction::End
            ) {
                output.push_str(&format!("| {} | {:?} |\n", combo.format(), action));
            }
        }

        if !self.custom_commands.is_empty() {
            output.push_str("\n## Custom Commands\n");
            output.push_str("| Name | Command |\n");
            output.push_str("|------|----------|\n");

            for (name, command) in &self.custom_commands {
                output.push_str(&format!("| {} | {} |\n", name, command));
            }
        }

        output
    }
}

/// Keybinding manager for handling key events
pub struct KeybindingManager {
    config: KeybindingConfig,
    recording_macro: bool,
    macro_sequence: Vec<KeyCombo>,
    last_macro: Vec<KeyCombo>,
}

impl KeybindingManager {
    /// Create new keybinding manager
    pub fn new() -> Result<Self> {
        let config = KeybindingConfig::load().unwrap_or_else(|e| {
            warn!("Failed to load keybinding config: {}, using defaults", e);
            KeybindingConfig::default()
        });

        Ok(Self {
            config,
            recording_macro: false,
            macro_sequence: Vec::new(),
            last_macro: Vec::new(),
        })
    }

    /// Process key input and return associated action
    pub fn process_key(&mut self, key_combo: KeyCombo) -> Option<KeyAction> {
        // Record macro if active
        if self.recording_macro {
            self.macro_sequence.push(key_combo.clone());
        }

        // Get action from configuration
        if let Some(action) = self.config.get_action(&key_combo) {
            let action_clone = action.clone();
            self.handle_special_actions(action_clone.clone());
            Some(action_clone)
        } else {
            None
        }
    }

    /// Handle special actions that affect the manager state
    fn handle_special_actions(&mut self, action: KeyAction) {
        match action {
            KeyAction::RecordMacro => {
                if self.recording_macro {
                    // Stop recording
                    self.recording_macro = false;
                    self.last_macro = self.macro_sequence.clone();
                    self.macro_sequence.clear();
                    debug!(
                        "Stopped macro recording, {} keys recorded",
                        self.last_macro.len()
                    );
                } else {
                    // Start recording
                    self.recording_macro = true;
                    self.macro_sequence.clear();
                    debug!("Started macro recording");
                }
            }
            KeyAction::PlayMacro => {
                if !self.last_macro.is_empty() {
                    debug!("Playing macro with {} keys", self.last_macro.len());
                    // Note: Actual playback would need to be handled by the caller
                }
            }
            _ => {}
        }
    }

    /// Get current configuration
    pub fn config(&self) -> &KeybindingConfig {
        &self.config
    }

    /// Get mutable configuration
    pub fn config_mut(&mut self) -> &mut KeybindingConfig {
        &mut self.config
    }

    /// Save current configuration
    pub fn save_config(&self) -> Result<()> {
        self.config.save()
    }

    /// Reload configuration from file
    pub fn reload_config(&mut self) -> Result<()> {
        self.config = KeybindingConfig::load()?;
        Ok(())
    }

    /// Is macro recording active?
    pub fn is_recording_macro(&self) -> bool {
        self.recording_macro
    }

    /// Get last recorded macro
    pub fn last_macro(&self) -> &[KeyCombo] {
        &self.last_macro
    }
}

// Helper trait for chaining key combinations
trait KeyComboExt {
    fn then(self, key: &str) -> Self;
}

impl KeyComboExt for KeyCombo {
    fn then(mut self, key: &str) -> Self {
        self.key = format!("{} {}", self.key, key);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_key_combo_parsing() {
        let combo = KeyCombo::parse("Ctrl+C").unwrap();
        assert_eq!(combo.key, "C");
        assert!(combo.ctrl);
        assert!(!combo.alt);

        let combo = KeyCombo::parse("Alt+Shift+F1").unwrap();
        assert_eq!(combo.key, "F1");
        assert!(combo.alt);
        assert!(combo.shift);
        assert!(!combo.ctrl);
    }

    #[test]
    fn test_key_combo_formatting() {
        let combo = KeyCombo::new("C").ctrl().alt();
        assert_eq!(combo.format(), "Ctrl+Alt+C");
    }

    #[test]
    fn test_default_config() {
        let config = KeybindingConfig::default();
        assert!(!config.bindings.is_empty());
        assert!(config.bindings.contains_key(&KeyCombo::new("Enter")));
    }

    #[test]
    fn test_keybinding_manager() {
        let mut manager = KeybindingManager::new().unwrap();

        let action = manager.process_key(KeyCombo::new("Enter"));
        assert!(matches!(action, Some(KeyAction::Accept)));
    }

    #[test]
    fn test_macro_recording() {
        let mut manager = KeybindingManager::new().unwrap();

        // Start recording
        manager.process_key(KeyCombo::new("r").ctrl().shift());
        assert!(manager.is_recording_macro());

        // Record some keys
        manager.process_key(KeyCombo::new("a"));
        manager.process_key(KeyCombo::new("b"));

        // Stop recording
        manager.process_key(KeyCombo::new("r").ctrl().shift());
        assert!(!manager.is_recording_macro());
        assert_eq!(manager.last_macro().len(), 3); // start, a, b
    }
}
