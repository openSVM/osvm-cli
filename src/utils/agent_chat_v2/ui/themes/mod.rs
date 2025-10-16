//! Modular theme system with multiple built-in themes
//! Each theme is optimized for different use cases and times of day

pub mod cyberpunk;
pub mod dracula;
pub mod gruvbox;
pub mod nord;
pub mod solarized;
pub mod tokyo_night;
pub mod vscode;

use cursive::theme::{Color, ColorStyle, Palette, PaletteColor, Theme};
use std::collections::HashMap;

/// Theme metadata for selection and preview
#[derive(Debug, Clone)]
pub struct ThemeInfo {
    pub id: String,
    pub name: String,
    pub description: String,
    pub best_for: Vec<String>, // e.g., ["night", "high-contrast", "long-sessions"]
    pub preview: String,
}

/// Central theme manager with hot-swapping support
pub struct ThemeManager {
    current_theme_id: String,
    themes: HashMap<String, Theme>,
    theme_info: HashMap<String, ThemeInfo>,
    auto_switch: bool,
    time_based: bool,
}

impl ThemeManager {
    pub fn new() -> Self {
        let mut manager = Self {
            current_theme_id: "vscode".to_string(),
            themes: HashMap::new(),
            theme_info: HashMap::new(),
            auto_switch: false,
            time_based: false,
        };

        // Register all built-in themes
        manager.register_builtin_themes();
        manager
    }

    fn register_builtin_themes(&mut self) {
        // VS Code Dark
        self.themes.insert("vscode".to_string(), vscode::theme());
        self.theme_info.insert(
            "vscode".to_string(),
            ThemeInfo {
                id: "vscode".to_string(),
                name: "VS Code Dark".to_string(),
                description: "Microsoft's popular dark theme".to_string(),
                best_for: vec!["coding".to_string(), "long-sessions".to_string()],
                preview: "Professional and familiar".to_string(),
            },
        );

        // Cyberpunk Neon
        self.themes.insert("cyberpunk".to_string(), cyberpunk::theme());
        self.theme_info.insert(
            "cyberpunk".to_string(),
            ThemeInfo {
                id: "cyberpunk".to_string(),
                name: "Cyberpunk Neon".to_string(),
                description: "High-contrast neon colors on deep space background".to_string(),
                best_for: vec!["night".to_string(), "high-energy".to_string()],
                preview: "⚡ Electrifying neon aesthetics".to_string(),
            },
        );

        // Nord Ice
        self.themes.insert("nord".to_string(), nord::theme());
        self.theme_info.insert(
            "nord".to_string(),
            ThemeInfo {
                id: "nord".to_string(),
                name: "Nord Ice".to_string(),
                description: "Cool, professional Nordic palette".to_string(),
                best_for: vec!["day".to_string(), "minimal".to_string()],
                preview: "❄️ Clean arctic vibes".to_string(),
            },
        );

        // Tokyo Night
        self.themes
            .insert("tokyo_night".to_string(), tokyo_night::theme());
        self.theme_info.insert(
            "tokyo_night".to_string(),
            ThemeInfo {
                id: "tokyo_night".to_string(),
                name: "Tokyo Night Storm".to_string(),
                description: "Modern Japanese-inspired storm palette".to_string(),
                best_for: vec!["evening".to_string(), "aesthetic".to_string()],
                preview: "🌸 Elegant Eastern beauty".to_string(),
            },
        );

        // Dracula
        self.themes.insert("dracula".to_string(), dracula::theme());
        self.theme_info.insert(
            "dracula".to_string(),
            ThemeInfo {
                id: "dracula".to_string(),
                name: "Dracula Enhanced".to_string(),
                description: "Dark theme with vibrant colors".to_string(),
                best_for: vec!["night".to_string(), "vibrant".to_string()],
                preview: "🦇 Rich vampire aesthetics".to_string(),
            },
        );

        // Gruvbox
        self.themes.insert("gruvbox".to_string(), gruvbox::theme());
        self.theme_info.insert(
            "gruvbox".to_string(),
            ThemeInfo {
                id: "gruvbox".to_string(),
                name: "Gruvbox Material".to_string(),
                description: "Retro groove with modern refinements".to_string(),
                best_for: vec!["retro".to_string(), "warm".to_string()],
                preview: "🎨 Warm retro comfort".to_string(),
            },
        );

        // Solarized
        self.themes.insert("solarized".to_string(), solarized::theme());
        self.theme_info.insert(
            "solarized".to_string(),
            ThemeInfo {
                id: "solarized".to_string(),
                name: "Solarized Pro".to_string(),
                description: "Enhanced Solarized with better contrast".to_string(),
                best_for: vec!["balanced".to_string(), "scientific".to_string()],
                preview: "☀️ Precision engineered colors".to_string(),
            },
        );
    }

    /// Get current theme
    pub fn current(&self) -> Theme {
        self.themes
            .get(&self.current_theme_id)
            .cloned()
            .unwrap_or_else(|| vscode::theme())
    }

    /// Switch to a different theme with smooth transition
    pub fn switch_theme(&mut self, theme_id: &str) -> bool {
        if self.themes.contains_key(theme_id) {
            self.current_theme_id = theme_id.to_string();
            true
        } else {
            false
        }
    }

    /// Get theme for current time of day
    pub fn theme_for_time(&self) -> Theme {
        use chrono::{Local, Timelike};
        let hour = Local::now().hour();

        let theme_id = match hour {
            0..=5 => "dracula",       // Late night: darker theme for sleep
            6..=9 => "nord",          // Morning: cool and fresh
            10..=16 => "vscode",      // Day: productive
            17..=20 => "tokyo_night", // Evening: aesthetic
            21..=23 => "dracula",     // Night: vibrant dark
            _ => {
                log::warn!("Unexpected hour value: {}, defaulting to vscode", hour);
                "vscode"
            }
        };

        self.themes
            .get(theme_id)
            .cloned()
            .unwrap_or_else(|| vscode::theme())
    }

    /// Get all available themes
    pub fn list_themes(&self) -> Vec<ThemeInfo> {
        self.theme_info.values().cloned().collect()
    }

    /// Enable automatic theme switching based on time
    pub fn enable_time_based_switching(&mut self, enabled: bool) {
        self.time_based = enabled;
    }
}

/// Message-specific colors for semantic highlighting
pub struct MessageColors;

impl MessageColors {
    pub fn user() -> ColorStyle {
        ColorStyle::new(Color::Rgb(15, 76, 117), Color::Rgb(30, 30, 30))
    }

    pub fn agent() -> ColorStyle {
        ColorStyle::new(Color::Rgb(86, 156, 214), Color::Rgb(30, 30, 30))
    }

    pub fn system() -> ColorStyle {
        ColorStyle::new(Color::Rgb(96, 96, 96), Color::Rgb(30, 30, 30))
    }

    pub fn tool_execution() -> ColorStyle {
        ColorStyle::new(Color::Rgb(243, 156, 18), Color::Rgb(30, 30, 30))
    }

    pub fn success() -> ColorStyle {
        ColorStyle::new(Color::Rgb(39, 174, 96), Color::Rgb(30, 30, 30))
    }

    pub fn error() -> ColorStyle {
        ColorStyle::new(Color::Rgb(231, 76, 60), Color::Rgb(30, 30, 30))
    }

    pub fn processing() -> ColorStyle {
        ColorStyle::new(Color::Rgb(52, 152, 219), Color::Rgb(30, 30, 30))
    }

    pub fn code_block() -> ColorStyle {
        ColorStyle::new(Color::Rgb(200, 200, 200), Color::Rgb(44, 62, 80))
    }
}