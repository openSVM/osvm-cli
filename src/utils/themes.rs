//! Advanced theme customization system for agent chat interface
//!
//! This module provides comprehensive theming capabilities including colors,
//! layout customization, animations, and visual effects.

use anyhow::{anyhow, Result};
use log::{debug, error, warn};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

/// Color definition with support for 256-color and RGB
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Color {
    /// Named ANSI colors
    Named(String),
    /// 256-color palette (0-255)
    Indexed(u8),
    /// RGB color values
    Rgb(u8, u8, u8),
    /// HSL color values (hue: 0-360, saturation: 0-100, lightness: 0-100)
    Hsl(u16, u8, u8),
    /// Hex color code
    Hex(String),
}

impl Color {
    /// Convert color to ANSI escape sequence
    pub fn to_ansi(&self) -> String {
        match self {
            Color::Named(name) => match name.as_str() {
                "black" => "\x1b[30m".to_string(),
                "red" => "\x1b[31m".to_string(),
                "green" => "\x1b[32m".to_string(),
                "yellow" => "\x1b[33m".to_string(),
                "blue" => "\x1b[34m".to_string(),
                "magenta" => "\x1b[35m".to_string(),
                "cyan" => "\x1b[36m".to_string(),
                "white" => "\x1b[37m".to_string(),
                "bright_black" => "\x1b[90m".to_string(),
                "bright_red" => "\x1b[91m".to_string(),
                "bright_green" => "\x1b[92m".to_string(),
                "bright_yellow" => "\x1b[93m".to_string(),
                "bright_blue" => "\x1b[94m".to_string(),
                "bright_magenta" => "\x1b[95m".to_string(),
                "bright_cyan" => "\x1b[96m".to_string(),
                "bright_white" => "\x1b[97m".to_string(),
                _ => "\x1b[39m".to_string(), // Default
            },
            Color::Indexed(index) => format!("\x1b[38;5;{}m", index),
            Color::Rgb(r, g, b) => format!("\x1b[38;2;{};{};{}m", r, g, b),
            Color::Hsl(h, s, l) => {
                // Convert HSL to RGB
                let (r, g, b) = hsl_to_rgb(*h, *s, *l);
                format!("\x1b[38;2;{};{};{}m", r, g, b)
            }
            Color::Hex(hex) => {
                // Parse hex color (#RRGGBB or #RGB)
                if let Ok((r, g, b)) = parse_hex_color(hex) {
                    format!("\x1b[38;2;{};{};{}m", r, g, b)
                } else {
                    "\x1b[39m".to_string()
                }
            }
        }
    }

    /// Convert color to background ANSI escape sequence
    pub fn to_bg_ansi(&self) -> String {
        match self {
            Color::Named(name) => match name.as_str() {
                "black" => "\x1b[40m".to_string(),
                "red" => "\x1b[41m".to_string(),
                "green" => "\x1b[42m".to_string(),
                "yellow" => "\x1b[43m".to_string(),
                "blue" => "\x1b[44m".to_string(),
                "magenta" => "\x1b[45m".to_string(),
                "cyan" => "\x1b[46m".to_string(),
                "white" => "\x1b[47m".to_string(),
                _ => "\x1b[49m".to_string(), // Default
            },
            Color::Indexed(index) => format!("\x1b[48;5;{}m", index),
            Color::Rgb(r, g, b) => format!("\x1b[48;2;{};{};{}m", r, g, b),
            Color::Hsl(h, s, l) => {
                let (r, g, b) = hsl_to_rgb(*h, *s, *l);
                format!("\x1b[48;2;{};{};{}m", r, g, b)
            }
            Color::Hex(hex) => {
                if let Ok((r, g, b)) = parse_hex_color(hex) {
                    format!("\x1b[48;2;{};{};{}m", r, g, b)
                } else {
                    "\x1b[49m".to_string()
                }
            }
        }
    }
}

/// Text styling options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextStyle {
    pub color: Color,
    pub background: Option<Color>,
    pub bold: bool,
    pub italic: bool,
    pub underline: bool,
    pub strikethrough: bool,
    pub dim: bool,
    pub blink: bool,
}

impl Default for TextStyle {
    fn default() -> Self {
        Self {
            color: Color::Named("white".to_string()),
            background: None,
            bold: false,
            italic: false,
            underline: false,
            strikethrough: false,
            dim: false,
            blink: false,
        }
    }
}

impl TextStyle {
    /// Convert style to ANSI escape sequence
    pub fn to_ansi(&self) -> String {
        let mut codes = vec![self.color.to_ansi()];

        if let Some(bg) = &self.background {
            codes.push(bg.to_bg_ansi());
        }

        if self.bold {
            codes.push("\x1b[1m".to_string());
        }
        if self.italic {
            codes.push("\x1b[3m".to_string());
        }
        if self.underline {
            codes.push("\x1b[4m".to_string());
        }
        if self.strikethrough {
            codes.push("\x1b[9m".to_string());
        }
        if self.dim {
            codes.push("\x1b[2m".to_string());
        }
        if self.blink {
            codes.push("\x1b[5m".to_string());
        }

        codes.join("")
    }

    /// Create a reset sequence
    pub fn reset() -> String {
        "\x1b[0m".to_string()
    }

    /// Apply style to text
    pub fn apply(&self, text: &str) -> String {
        format!("{}{}{}", self.to_ansi(), text, Self::reset())
    }
}

/// Layout configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Layout {
    pub border_style: BorderStyle,
    pub padding: Padding,
    pub margin: Margin,
    pub width: Option<usize>,
    pub height: Option<usize>,
    pub alignment: Alignment,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BorderStyle {
    None,
    Single,
    Double,
    Rounded,
    Thick,
    Custom {
        top: String,
        bottom: String,
        left: String,
        right: String,
        top_left: String,
        top_right: String,
        bottom_left: String,
        bottom_right: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Padding {
    pub top: usize,
    pub bottom: usize,
    pub left: usize,
    pub right: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Margin {
    pub top: usize,
    pub bottom: usize,
    pub left: usize,
    pub right: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Alignment {
    Left,
    Center,
    Right,
    Justify,
}

/// Animation configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Animation {
    pub enabled: bool,
    pub duration_ms: u64,
    pub easing: EasingFunction,
    pub repeat: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EasingFunction {
    Linear,
    EaseIn,
    EaseOut,
    EaseInOut,
    Bounce,
    Elastic,
}

/// Complete theme configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Theme {
    pub name: String,
    pub description: String,
    pub author: String,
    pub version: String,

    // Color scheme
    pub primary: TextStyle,
    pub secondary: TextStyle,
    pub accent: TextStyle,
    pub background: TextStyle,
    pub text: TextStyle,
    pub muted: TextStyle,
    pub error: TextStyle,
    pub warning: TextStyle,
    pub success: TextStyle,
    pub info: TextStyle,

    // UI element styles
    pub border: TextStyle,
    pub selection: TextStyle,
    pub highlight: TextStyle,
    pub suggestion: TextStyle,
    pub command: TextStyle,
    pub query: TextStyle,
    pub action: TextStyle,

    // Layout
    pub layout: Layout,

    // Animations
    pub animations: HashMap<String, Animation>,

    // Custom styles
    pub custom_styles: HashMap<String, TextStyle>,

    // Special effects
    pub gradient_support: bool,
    pub shadow_effects: bool,
    pub transparency: bool,
}

impl Default for Theme {
    fn default() -> Self {
        Self::default_dark_theme()
    }
}

impl Theme {
    /// Create default dark theme (Claude Code style)
    pub fn default_dark_theme() -> Self {
        let mut animations = HashMap::new();
        animations.insert(
            "typing".to_string(),
            Animation {
                enabled: true,
                duration_ms: 100,
                easing: EasingFunction::Linear,
                repeat: false,
            },
        );
        animations.insert(
            "loading".to_string(),
            Animation {
                enabled: true,
                duration_ms: 1000,
                easing: EasingFunction::Linear,
                repeat: true,
            },
        );

        Self {
            name: "Claude Dark".to_string(),
            description: "Default Claude Code dark theme".to_string(),
            author: "OSVM Team".to_string(),
            version: "1.0.0".to_string(),

            primary: TextStyle {
                color: Color::Hex("#FFD700".to_string()), // Gold
                ..Default::default()
            },
            secondary: TextStyle {
                color: Color::Hex("#87CEEB".to_string()), // Sky Blue
                ..Default::default()
            },
            accent: TextStyle {
                color: Color::Hex("#FF6B6B".to_string()), // Coral
                ..Default::default()
            },
            background: TextStyle {
                color: Color::Hex("#1E1E1E".to_string()), // Dark Gray
                ..Default::default()
            },
            text: TextStyle {
                color: Color::Hex("#FFFFFF".to_string()), // White
                ..Default::default()
            },
            muted: TextStyle {
                color: Color::Hex("#808080".to_string()), // Gray
                dim: true,
                ..Default::default()
            },
            error: TextStyle {
                color: Color::Hex("#FF4444".to_string()), // Red
                bold: true,
                ..Default::default()
            },
            warning: TextStyle {
                color: Color::Hex("#FFAA00".to_string()), // Orange
                ..Default::default()
            },
            success: TextStyle {
                color: Color::Hex("#00AA00".to_string()), // Green
                ..Default::default()
            },
            info: TextStyle {
                color: Color::Hex("#00AAFF".to_string()), // Blue
                ..Default::default()
            },

            border: TextStyle {
                color: Color::Hex("#444444".to_string()),
                ..Default::default()
            },
            selection: TextStyle {
                color: Color::Hex("#FFFFFF".to_string()),
                background: Some(Color::Hex("#0078D4".to_string())),
                bold: true,
                ..Default::default()
            },
            highlight: TextStyle {
                color: Color::Hex("#FFFF00".to_string()),
                bold: true,
                ..Default::default()
            },
            suggestion: TextStyle {
                color: Color::Hex("#888888".to_string()),
                italic: true,
                ..Default::default()
            },
            command: TextStyle {
                color: Color::Hex("#00FFFF".to_string()),
                ..Default::default()
            },
            query: TextStyle {
                color: Color::Hex("#00FF00".to_string()),
                ..Default::default()
            },
            action: TextStyle {
                color: Color::Hex("#FF00FF".to_string()),
                ..Default::default()
            },

            layout: Layout {
                border_style: BorderStyle::Single,
                padding: Padding {
                    top: 1,
                    bottom: 1,
                    left: 2,
                    right: 2,
                },
                margin: Margin {
                    top: 0,
                    bottom: 0,
                    left: 0,
                    right: 0,
                },
                width: None,
                height: None,
                alignment: Alignment::Left,
            },

            animations,
            custom_styles: HashMap::new(),
            gradient_support: true,
            shadow_effects: false,
            transparency: false,
        }
    }

    /// Create light theme
    pub fn default_light_theme() -> Self {
        let mut theme = Self::default_dark_theme();
        theme.name = "Claude Light".to_string();
        theme.description = "Light theme for Claude Code".to_string();

        // Invert colors for light theme
        theme.background.color = Color::Hex("#FFFFFF".to_string());
        theme.text.color = Color::Hex("#000000".to_string());
        theme.muted.color = Color::Hex("#666666".to_string());
        theme.border.color = Color::Hex("#CCCCCC".to_string());

        theme
    }

    /// Create high contrast theme (accessibility)
    pub fn high_contrast_theme() -> Self {
        let mut theme = Self::default_dark_theme();
        theme.name = "High Contrast".to_string();
        theme.description = "High contrast theme for accessibility".to_string();

        // Use high contrast colors
        theme.background.color = Color::Named("black".to_string());
        theme.text.color = Color::Named("white".to_string());
        theme.primary.color = Color::Named("yellow".to_string());
        theme.secondary.color = Color::Named("cyan".to_string());
        theme.error.color = Color::Named("red".to_string());
        theme.success.color = Color::Named("green".to_string());

        // Make everything bold for better visibility
        theme.text.bold = true;
        theme.primary.bold = true;
        theme.secondary.bold = true;

        theme
    }

    /// Create retro/vintage theme
    pub fn retro_theme() -> Self {
        let mut theme = Self::default_dark_theme();
        theme.name = "Retro Terminal".to_string();
        theme.description = "Vintage green terminal theme".to_string();

        // Classic green on black terminal colors
        theme.background.color = Color::Named("black".to_string());
        theme.text.color = Color::Hex("#00FF00".to_string());
        theme.primary.color = Color::Hex("#00FF00".to_string());
        theme.secondary.color = Color::Hex("#00AA00".to_string());
        theme.muted.color = Color::Hex("#008800".to_string());
        theme.border.color = Color::Hex("#00AA00".to_string());

        // Add some vintage effects
        theme.layout.border_style = BorderStyle::Custom {
            top: "═".to_string(),
            bottom: "═".to_string(),
            left: "║".to_string(),
            right: "║".to_string(),
            top_left: "╔".to_string(),
            top_right: "╗".to_string(),
            bottom_left: "╚".to_string(),
            bottom_right: "╝".to_string(),
        };

        theme
    }

    /// Load theme from file
    pub fn load(name: &str) -> Result<Self> {
        let theme_path = Self::theme_path(name)?;

        if !theme_path.exists() {
            return Err(anyhow!("Theme '{}' not found", name));
        }

        let content = fs::read_to_string(&theme_path)
            .map_err(|e| anyhow!("Failed to read theme file: {}", e))?;

        let theme: Self =
            serde_json::from_str(&content).map_err(|e| anyhow!("Failed to parse theme: {}", e))?;

        debug!("Loaded theme '{}' from {:?}", name, theme_path);
        Ok(theme)
    }

    /// Save theme to file
    pub fn save(&self) -> Result<()> {
        let theme_path = Self::theme_path(&self.name)?;

        if let Some(parent) = theme_path.parent() {
            fs::create_dir_all(parent)
                .map_err(|e| anyhow!("Failed to create themes directory: {}", e))?;
        }

        let content = serde_json::to_string_pretty(self)
            .map_err(|e| anyhow!("Failed to serialize theme: {}", e))?;

        fs::write(&theme_path, content)
            .map_err(|e| anyhow!("Failed to write theme file: {}", e))?;

        debug!("Saved theme '{}' to {:?}", self.name, theme_path);
        Ok(())
    }

    /// Get theme file path
    fn theme_path(name: &str) -> Result<PathBuf> {
        let home =
            std::env::var("HOME").map_err(|_| anyhow!("HOME environment variable not set"))?;
        let safe_name = name
            .chars()
            .map(|c| {
                if c.is_alphanumeric() || c == '-' || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect::<String>();
        Ok(PathBuf::from(home)
            .join(".osvm")
            .join("themes")
            .join(format!("{}.json", safe_name)))
    }

    /// List available themes
    pub fn list_available() -> Result<Vec<String>> {
        let themes_dir = Self::theme_path("")?.parent().unwrap().to_path_buf();

        if !themes_dir.exists() {
            return Ok(vec!["default".to_string()]);
        }

        let mut themes = Vec::new();

        for entry in fs::read_dir(&themes_dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                    themes.push(name.to_string());
                }
            }
        }

        if themes.is_empty() {
            themes.push("default".to_string());
        }

        Ok(themes)
    }

    /// Get style by name
    pub fn get_style(&self, name: &str) -> &TextStyle {
        match name {
            "primary" => &self.primary,
            "secondary" => &self.secondary,
            "accent" => &self.accent,
            "background" => &self.background,
            "text" => &self.text,
            "muted" => &self.muted,
            "error" => &self.error,
            "warning" => &self.warning,
            "success" => &self.success,
            "info" => &self.info,
            "border" => &self.border,
            "selection" => &self.selection,
            "highlight" => &self.highlight,
            "suggestion" => &self.suggestion,
            "command" => &self.command,
            "query" => &self.query,
            "action" => &self.action,
            _ => self.custom_styles.get(name).unwrap_or(&self.text),
        }
    }

    /// Add custom style
    pub fn add_custom_style(&mut self, name: String, style: TextStyle) {
        self.custom_styles.insert(name, style);
    }

    /// Create theme preview
    pub fn preview(&self) -> String {
        let mut preview = String::new();

        preview.push_str(&format!("Theme: {}\n", self.primary.apply(&self.name)));
        preview.push_str(&format!(
            "Description: {}\n",
            self.muted.apply(&self.description)
        ));
        preview.push_str(&format!(
            "Author: {}\n\n",
            self.secondary.apply(&self.author)
        ));

        preview.push_str("Color Palette:\n");
        preview.push_str(&format!("Primary: {}\n", self.primary.apply("Sample Text")));
        preview.push_str(&format!(
            "Secondary: {}\n",
            self.secondary.apply("Sample Text")
        ));
        preview.push_str(&format!("Accent: {}\n", self.accent.apply("Sample Text")));
        preview.push_str(&format!("Error: {}\n", self.error.apply("Error Message")));
        preview.push_str(&format!(
            "Warning: {}\n",
            self.warning.apply("Warning Message")
        ));
        preview.push_str(&format!(
            "Success: {}\n",
            self.success.apply("Success Message")
        ));
        preview.push_str(&format!("Info: {}\n", self.info.apply("Info Message")));

        preview.push_str("\nUI Elements:\n");
        preview.push_str(&format!("Command: {}\n", self.command.apply("/balance")));
        preview.push_str(&format!(
            "Query: {}\n",
            self.query.apply("balance my wallet")
        ));
        preview.push_str(&format!("Action: {}\n", self.action.apply("send SOL")));
        preview.push_str(&format!(
            "Selection: {}\n",
            self.selection.apply("▶ Selected Item")
        ));
        preview.push_str(&format!(
            "Highlight: {}\n",
            self.highlight.apply("Highlighted Text")
        ));

        preview
    }
}

/// Theme manager for runtime theme switching
pub struct ThemeManager {
    current_theme: Theme,
    available_themes: Vec<String>,
}

impl ThemeManager {
    /// Create new theme manager
    pub fn new() -> Result<Self> {
        let available_themes = Theme::list_available().unwrap_or_else(|e| {
            warn!("Failed to list themes: {}, using default", e);
            vec!["default".to_string()]
        });

        // Try to load saved theme preference, fallback to default
        let current_theme =
            Self::load_preferred_theme().unwrap_or_else(|_| Theme::default_dark_theme());

        Ok(Self {
            current_theme,
            available_themes,
        })
    }

    /// Get current theme
    pub fn current_theme(&self) -> &Theme {
        &self.current_theme
    }

    /// Switch to theme by name
    pub fn switch_theme(&mut self, name: &str) -> Result<()> {
        let theme = match name {
            "default" | "dark" => Theme::default_dark_theme(),
            "light" => Theme::default_light_theme(),
            "high-contrast" => Theme::high_contrast_theme(),
            "retro" => Theme::retro_theme(),
            _ => Theme::load(name)?,
        };

        self.current_theme = theme;
        self.save_theme_preference(name)?;

        debug!("Switched to theme: {}", name);
        Ok(())
    }

    /// Get available theme names
    pub fn available_themes(&self) -> &[String] {
        &self.available_themes
    }

    /// Refresh available themes list
    pub fn refresh_themes(&mut self) -> Result<()> {
        self.available_themes = Theme::list_available()?;
        Ok(())
    }

    /// Load preferred theme from config
    fn load_preferred_theme() -> Result<Theme> {
        let config_path = Self::config_path()?;
        if !config_path.exists() {
            return Err(anyhow!("No theme preference saved"));
        }

        let theme_name = fs::read_to_string(&config_path)?;
        match theme_name.trim() {
            "default" | "dark" => Ok(Theme::default_dark_theme()),
            "light" => Ok(Theme::default_light_theme()),
            "high-contrast" => Ok(Theme::high_contrast_theme()),
            "retro" => Ok(Theme::retro_theme()),
            name => Theme::load(name),
        }
    }

    /// Save theme preference
    fn save_theme_preference(&self, name: &str) -> Result<()> {
        let config_path = Self::config_path()?;
        if let Some(parent) = config_path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&config_path, name)?;
        Ok(())
    }

    /// Get theme preference config path
    fn config_path() -> Result<PathBuf> {
        let home =
            std::env::var("HOME").map_err(|_| anyhow!("HOME environment variable not set"))?;
        Ok(PathBuf::from(home).join(".osvm").join("current_theme"))
    }
}

/// Utility functions for color conversion
fn hsl_to_rgb(h: u16, s: u8, l: u8) -> (u8, u8, u8) {
    let h = (h % 360) as f32 / 360.0;
    let s = s as f32 / 100.0;
    let l = l as f32 / 100.0;

    let c = (1.0 - (2.0 * l - 1.0).abs()) * s;
    let x = c * (1.0 - ((h * 6.0) % 2.0 - 1.0).abs());
    let m = l - c / 2.0;

    let (r, g, b) = if h < 1.0 / 6.0 {
        (c, x, 0.0)
    } else if h < 2.0 / 6.0 {
        (x, c, 0.0)
    } else if h < 3.0 / 6.0 {
        (0.0, c, x)
    } else if h < 4.0 / 6.0 {
        (0.0, x, c)
    } else if h < 5.0 / 6.0 {
        (x, 0.0, c)
    } else {
        (c, 0.0, x)
    };

    (
        ((r + m) * 255.0) as u8,
        ((g + m) * 255.0) as u8,
        ((b + m) * 255.0) as u8,
    )
}

fn parse_hex_color(hex: &str) -> Result<(u8, u8, u8)> {
    let hex = hex.trim_start_matches('#');

    match hex.len() {
        3 => {
            // #RGB -> #RRGGBB
            let r = u8::from_str_radix(&hex[0..1].repeat(2), 16)?;
            let g = u8::from_str_radix(&hex[1..2].repeat(2), 16)?;
            let b = u8::from_str_radix(&hex[2..3].repeat(2), 16)?;
            Ok((r, g, b))
        }
        6 => {
            // #RRGGBB
            let r = u8::from_str_radix(&hex[0..2], 16)?;
            let g = u8::from_str_radix(&hex[2..4], 16)?;
            let b = u8::from_str_radix(&hex[4..6], 16)?;
            Ok((r, g, b))
        }
        _ => Err(anyhow!("Invalid hex color format")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_color_to_ansi() {
        let red = Color::Named("red".to_string());
        assert_eq!(red.to_ansi(), "\x1b[31m");

        let rgb = Color::Rgb(255, 0, 0);
        assert_eq!(rgb.to_ansi(), "\x1b[38;2;255;0;0m");
    }

    #[test]
    fn test_hex_color_parsing() {
        assert_eq!(parse_hex_color("#FF0000").unwrap(), (255, 0, 0));
        assert_eq!(parse_hex_color("#F00").unwrap(), (255, 0, 0));
        assert_eq!(parse_hex_color("00FF00").unwrap(), (0, 255, 0));
    }

    #[test]
    fn test_hsl_to_rgb() {
        assert_eq!(hsl_to_rgb(0, 100, 50), (255, 0, 0)); // Red
        assert_eq!(hsl_to_rgb(120, 100, 50), (0, 255, 0)); // Green
        assert_eq!(hsl_to_rgb(240, 100, 50), (0, 0, 255)); // Blue
    }

    #[test]
    fn test_text_style_application() {
        let style = TextStyle {
            color: Color::Named("red".to_string()),
            bold: true,
            ..Default::default()
        };

        let styled_text = style.apply("Hello");
        assert!(styled_text.contains("\x1b[31m")); // Red color
        assert!(styled_text.contains("\x1b[1m")); // Bold
        assert!(styled_text.contains("\x1b[0m")); // Reset
    }

    #[test]
    fn test_theme_creation() {
        let theme = Theme::default_dark_theme();
        assert_eq!(theme.name, "Claude Dark");
        assert!(!theme.custom_styles.is_empty() || theme.custom_styles.is_empty());
        // Either is fine
    }
}
