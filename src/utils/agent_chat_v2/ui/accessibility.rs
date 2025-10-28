//! Accessibility features for improved usability

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Accessibility configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AccessibilityConfig {
    /// High contrast mode for better visibility
    pub high_contrast: bool,

    /// Larger text mode (increases spacing)
    pub large_text: bool,

    /// Reduce animations for motion sensitivity
    pub reduce_motion: bool,

    /// Screen reader friendly mode
    pub screen_reader_mode: bool,

    /// Sound notifications enabled
    pub sound_notifications: bool,

    /// Verbose mode (more detailed messages)
    pub verbose_mode: bool,

    /// Focus indicators (for keyboard navigation)
    pub focus_indicators: bool,
}

impl Default for AccessibilityConfig {
    fn default() -> Self {
        Self {
            high_contrast: Self::detect_high_contrast(),
            large_text: false,
            reduce_motion: Self::detect_reduce_motion(),
            screen_reader_mode: Self::detect_screen_reader(),
            sound_notifications: false,
            verbose_mode: false,
            focus_indicators: true,
        }
    }
}

impl AccessibilityConfig {
    /// Load configuration from file
    pub fn load() -> Self {
        if let Ok(path) = Self::config_path() {
            if let Ok(content) = std::fs::read_to_string(&path) {
                if let Ok(config) = serde_json::from_str(&content) {
                    return config;
                }
            }
        }
        Self::default()
    }

    /// Save configuration to file
    pub fn save(&self) -> Result<(), Box<dyn std::error::Error>> {
        let path = Self::config_path()?;
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let json = serde_json::to_string_pretty(self)?;
        std::fs::write(path, json)?;
        Ok(())
    }

    /// Get configuration file path
    fn config_path() -> Result<PathBuf, std::env::VarError> {
        let home = std::env::var("HOME")?;
        Ok(PathBuf::from(home)
            .join(".config")
            .join("osvm")
            .join("accessibility.json"))
    }

    /// Detect if high contrast is preferred (from environment)
    fn detect_high_contrast() -> bool {
        // Check environment variables
        std::env::var("HIGH_CONTRAST").unwrap_or_default() == "1"
            || std::env::var("OSVM_HIGH_CONTRAST").unwrap_or_default() == "1"
            || std::env::var("NO_COLOR").is_ok() // NO_COLOR implies high contrast preference
    }

    /// Detect if reduced motion is preferred
    fn detect_reduce_motion() -> bool {
        std::env::var("REDUCE_MOTION").unwrap_or_default() == "1"
            || std::env::var("OSVM_REDUCE_MOTION").unwrap_or_default() == "1"
    }

    /// Detect if screen reader is likely in use
    fn detect_screen_reader() -> bool {
        // Check for common screen reader indicators
        std::env::var("SCREEN_READER").unwrap_or_default() == "1"
            || std::env::var("NVDA").is_ok()
            || std::env::var("JAWS").is_ok()
            || std::env::var("NARRATOR").is_ok()
    }

    /// Toggle high contrast mode
    pub fn toggle_high_contrast(&mut self) {
        self.high_contrast = !self.high_contrast;
    }

    /// Toggle reduce motion
    pub fn toggle_reduce_motion(&mut self) {
        self.reduce_motion = !self.reduce_motion;
    }

    /// Toggle screen reader mode
    pub fn toggle_screen_reader_mode(&mut self) {
        self.screen_reader_mode = !self.screen_reader_mode;
    }

    /// Get animation duration based on motion preferences
    pub fn animation_duration_ms(&self) -> u64 {
        if self.reduce_motion {
            0 // No animation
        } else {
            100 // Normal animation speed
        }
    }

    /// Should show verbose descriptions
    pub fn should_show_verbose(&self) -> bool {
        self.verbose_mode || self.screen_reader_mode
    }
}

/// Keyboard navigation hints
pub struct KeyboardNavigation;

impl KeyboardNavigation {
    /// Get keyboard navigation help text
    pub fn help_text() -> &'static str {
        "KEYBOARD NAVIGATION:\n\n\
        Tab           - Move to next element\n\
        Shift+Tab     - Move to previous element\n\
        Enter         - Activate selected element\n\
        Space         - Activate button or toggle\n\
        Esc           - Cancel or close dialog\n\
        Arrow Keys    - Navigate lists and history\n\
        F1            - Help\n\
        F10           - Context menu\n\
        Ctrl+Q        - Quit\n\n\
        Most UI elements are keyboard accessible.\n\
        Look for underlined letters for quick access."
    }

    /// Check if element should show focus indicator
    pub fn should_show_focus_indicator(config: &AccessibilityConfig) -> bool {
        config.focus_indicators
    }
}

/// Color contrast helpers for accessibility
pub struct ColorContrast;

impl ColorContrast {
    /// Calculate relative luminance (WCAG formula)
    pub fn relative_luminance(r: u8, g: u8, b: u8) -> f64 {
        let r = Self::srgb_to_linear(r);
        let g = Self::srgb_to_linear(g);
        let b = Self::srgb_to_linear(b);

        0.2126 * r + 0.7152 * g + 0.0722 * b
    }

    /// Convert sRGB to linear RGB
    fn srgb_to_linear(c: u8) -> f64 {
        let c = c as f64 / 255.0;
        if c <= 0.03928 {
            c / 12.92
        } else {
            ((c + 0.055) / 1.055).powf(2.4)
        }
    }

    /// Calculate contrast ratio between two colors
    pub fn contrast_ratio(r1: u8, g1: u8, b1: u8, r2: u8, g2: u8, b2: u8) -> f64 {
        let l1 = Self::relative_luminance(r1, g1, b1);
        let l2 = Self::relative_luminance(r2, g2, b2);

        let lighter = l1.max(l2);
        let darker = l1.min(l2);

        (lighter + 0.05) / (darker + 0.05)
    }

    /// Check if contrast meets WCAG AA standard (4.5:1 for normal text)
    pub fn meets_wcag_aa(r1: u8, g1: u8, b1: u8, r2: u8, g2: u8, b2: u8) -> bool {
        Self::contrast_ratio(r1, g1, b1, r2, g2, b2) >= 4.5
    }

    /// Check if contrast meets WCAG AAA standard (7:1 for normal text)
    pub fn meets_wcag_aaa(r1: u8, g1: u8, b1: u8, r2: u8, g2: u8, b2: u8) -> bool {
        Self::contrast_ratio(r1, g1, b1, r2, g2, b2) >= 7.0
    }
}

/// Text sizing helpers
pub struct TextSizing;

impl TextSizing {
    /// Get line spacing multiplier based on config
    pub fn line_spacing(config: &AccessibilityConfig) -> f32 {
        if config.large_text {
            1.5
        } else {
            1.0
        }
    }

    /// Get padding multiplier based on config
    pub fn padding(config: &AccessibilityConfig) -> usize {
        if config.large_text {
            2
        } else {
            1
        }
    }

    /// Wrap text at appropriate width
    pub fn wrap_text(text: &str, max_width: usize, config: &AccessibilityConfig) -> Vec<String> {
        let effective_width = if config.large_text {
            max_width.saturating_sub(4) // Leave more margin for large text
        } else {
            max_width
        };

        // Simple word wrapping
        let mut lines = Vec::new();
        let mut current_line = String::new();

        for word in text.split_whitespace() {
            if current_line.len() + word.len() + 1 > effective_width {
                if !current_line.is_empty() {
                    lines.push(current_line);
                    current_line = String::new();
                }
            }

            if !current_line.is_empty() {
                current_line.push(' ');
            }
            current_line.push_str(word);
        }

        if !current_line.is_empty() {
            lines.push(current_line);
        }

        lines
    }
}

/// Accessibility announcements (for screen readers)
pub struct Announcements;

impl Announcements {
    /// Create an announcement string
    pub fn create(message: &str, priority: AnnouncementPriority) -> String {
        let prefix = match priority {
            AnnouncementPriority::Polite => "[ANNOUNCE]",
            AnnouncementPriority::Assertive => "[ALERT]",
            AnnouncementPriority::Off => return message.to_string(),
        };

        format!("{} {}", prefix, message)
    }

    /// Announce state change
    pub fn state_change(old_state: &str, new_state: &str) -> String {
        Self::create(
            &format!("State changed from {} to {}", old_state, new_state),
            AnnouncementPriority::Polite,
        )
    }

    /// Announce action completed
    pub fn action_completed(action: &str) -> String {
        Self::create(
            &format!("{} completed successfully", action),
            AnnouncementPriority::Polite,
        )
    }

    /// Announce error
    pub fn error(message: &str) -> String {
        Self::create(
            &format!("Error: {}", message),
            AnnouncementPriority::Assertive,
        )
    }
}

/// Announcement priority levels (ARIA live regions)
#[derive(Debug, Clone, Copy)]
pub enum AnnouncementPriority {
    Polite,    // Announce when convenient
    Assertive, // Announce immediately
    Off,       // Don't announce
}

/// Quick accessibility tips
pub struct AccessibilityTips;

impl AccessibilityTips {
    pub fn all_tips() -> Vec<&'static str> {
        vec![
            "Enable high contrast mode with: export HIGH_CONTRAST=1",
            "Reduce animations with: export REDUCE_MOTION=1",
            "All features are keyboard accessible - use Tab to navigate",
            "Press F1 anytime for comprehensive help",
            "Focus indicators show where you are - they're always on",
            "Screen reader users: Enable verbose mode in settings",
            "Use Alt+letter shortcuts for quick access to common actions",
            "Increase terminal font size for better visibility",
            "Error messages include recovery steps - read them carefully",
            "Confirmation dialogs prevent accidental actions",
        ]
    }

    pub fn show_tip(tip_index: usize) -> &'static str {
        let tips = Self::all_tips();
        tips[tip_index % tips.len()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_contrast_ratio() {
        // White on black should have high contrast
        let ratio = ColorContrast::contrast_ratio(255, 255, 255, 0, 0, 0);
        assert!(ratio > 20.0);

        // Same color should have 1:1 ratio
        let ratio = ColorContrast::contrast_ratio(128, 128, 128, 128, 128, 128);
        assert!((ratio - 1.0).abs() < 0.01);
    }

    #[test]
    fn test_wcag_standards() {
        // White on black meets AAA
        assert!(ColorContrast::meets_wcag_aaa(255, 255, 255, 0, 0, 0));

        // Similar colors don't meet AA
        assert!(!ColorContrast::meets_wcag_aa(200, 200, 200, 210, 210, 210));
    }

    #[test]
    fn test_default_config() {
        let config = AccessibilityConfig::default();
        assert!(config.focus_indicators); // Should be on by default
    }

    #[test]
    fn test_announcements() {
        let announcement = Announcements::state_change("Idle", "Processing");
        assert!(announcement.contains("State changed"));
    }
}
