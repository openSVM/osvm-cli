//! Smart cursor management for optimal focus flow

use cursive::Cursive;
use std::collections::HashMap;

/// Context for determining cursor position
#[derive(Debug, Clone)]
pub struct CursorContext {
    pub last_action: UserAction,
    pub has_error: bool,
    pub suggestions_available: bool,
    pub active_panel: PanelType,
    pub has_notifications: bool,
    pub is_processing: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UserAction {
    SendMessage,
    SelectSession,
    OpenMenu,
    CloseDialog,
    NavigateUp,
    NavigateDown,
    SelectSuggestion,
    ClearInput,
    CopyMessage,
    ScrollChat,
    OpenSettings,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PanelType {
    Input,
    Chat,
    Sessions,
    Settings,
    Suggestions,
    Notifications,
}

/// Smart cursor manager that predicts optimal focus position
pub struct SmartCursorManager {
    current_focus: String,
    focus_history: Vec<String>,
    focus_rules: HashMap<UserAction, CursorRule>,
    predictive_mode: bool,
}

struct CursorRule {
    condition: Box<dyn Fn(&CursorContext) -> bool>,
    target: String,
    priority: u8,
}

impl SmartCursorManager {
    pub fn new() -> Self {
        let mut manager = Self {
            current_focus: "input".to_string(),
            focus_history: Vec::with_capacity(10),
            focus_rules: HashMap::new(),
            predictive_mode: true,
        };

        manager.setup_default_rules();
        manager
    }

    /// Set up intelligent cursor rules
    fn setup_default_rules(&mut self) {
        // After sending message
        self.add_rule(
            UserAction::SendMessage,
            Box::new(|ctx| ctx.suggestions_available),
            "suggestion_1".to_string(),
            10,
        );

        self.add_rule(
            UserAction::SendMessage,
            Box::new(|ctx| ctx.has_error),
            "error_dialog".to_string(),
            20,
        );

        self.add_rule(
            UserAction::SendMessage,
            Box::new(|ctx| !ctx.suggestions_available && !ctx.has_error),
            "input".to_string(),
            5,
        );

        // After selecting session
        self.add_rule(
            UserAction::SelectSession,
            Box::new(|_| true),
            "input".to_string(),
            10,
        );

        // After closing dialog
        self.add_rule(
            UserAction::CloseDialog,
            Box::new(|_| true),
            "input".to_string(),
            10,
        );

        // After selecting suggestion
        self.add_rule(
            UserAction::SelectSuggestion,
            Box::new(|_| true),
            "send_button".to_string(),
            10,
        );

        // After clearing input
        self.add_rule(
            UserAction::ClearInput,
            Box::new(|ctx| ctx.suggestions_available),
            "suggestion_1".to_string(),
            10,
        );
    }

    fn add_rule(
        &mut self,
        action: UserAction,
        condition: Box<dyn Fn(&CursorContext) -> bool>,
        target: String,
        priority: u8,
    ) {
        self.focus_rules.insert(
            action,
            CursorRule {
                condition,
                target,
                priority,
            },
        );
    }

    /// Determine optimal cursor position based on context
    pub fn get_optimal_focus(&self, context: &CursorContext) -> String {
        if !self.predictive_mode {
            return self.current_focus.clone();
        }

        // Check rules for the last action
        if let Some(rule) = self.focus_rules.get(&context.last_action) {
            if (rule.condition)(context) {
                return rule.target.clone();
            }
        }

        // Fallback logic based on panel
        match context.active_panel {
            PanelType::Input => {
                if context.is_processing {
                    "stop_button".to_string()
                } else if context.suggestions_available {
                    "suggestion_1".to_string()
                } else {
                    "input".to_string()
                }
            }
            PanelType::Chat => {
                if context.has_notifications {
                    "notification_panel".to_string()
                } else {
                    "input".to_string()
                }
            }
            PanelType::Sessions => "session_list".to_string(),
            PanelType::Settings => "settings_list".to_string(),
            PanelType::Suggestions => "suggestion_1".to_string(),
            PanelType::Notifications => "dismiss_button".to_string(),
        }
    }

    /// Move cursor to optimal position
    pub fn apply_smart_focus(&mut self, siv: &mut Cursive, context: &CursorContext) {
        let target = self.get_optimal_focus(context);

        // Record history
        if self.current_focus != target {
            self.focus_history.push(self.current_focus.clone());
            if self.focus_history.len() > 10 {
                self.focus_history.remove(0);
            }
        }

        // Apply focus
        if siv.focus_name(&target).is_ok() {
            self.current_focus = target;
            log::debug!("Smart cursor moved to: {}", self.current_focus);
        }
    }

    /// Go back to previous focus
    pub fn focus_previous(&mut self, siv: &mut Cursive) {
        if let Some(prev) = self.focus_history.pop() {
            if siv.focus_name(&prev).is_ok() {
                self.current_focus = prev;
            }
        }
    }

    /// Get focus breadcrumb trail
    pub fn get_breadcrumb(&self) -> Vec<&str> {
        self.focus_history.iter().map(|s| s.as_str()).collect()
    }

    /// Enable/disable predictive mode
    pub fn set_predictive_mode(&mut self, enabled: bool) {
        self.predictive_mode = enabled;
    }
}

/// Cursor parking positions for idle states
pub struct CursorParking {
    idle_positions: HashMap<PanelType, String>,
    idle_timeout: std::time::Duration,
    last_activity: std::time::Instant,
}

impl CursorParking {
    pub fn new() -> Self {
        let mut parking = Self {
            idle_positions: HashMap::new(),
            idle_timeout: std::time::Duration::from_secs(30),
            last_activity: std::time::Instant::now(),
        };

        // Set default idle positions
        parking
            .idle_positions
            .insert(PanelType::Input, "input".to_string());
        parking
            .idle_positions
            .insert(PanelType::Chat, "chat_scroll".to_string());
        parking
            .idle_positions
            .insert(PanelType::Sessions, "new_session".to_string());
        parking
            .idle_positions
            .insert(PanelType::Settings, "close_button".to_string());

        parking
    }

    pub fn update_activity(&mut self) {
        self.last_activity = std::time::Instant::now();
    }

    pub fn should_park(&self) -> bool {
        self.last_activity.elapsed() > self.idle_timeout
    }

    pub fn get_parking_position(&self, panel: PanelType) -> Option<&String> {
        self.idle_positions.get(&panel)
    }
}

/// Visual cursor indicator with animation
pub struct CursorIndicator {
    blink_phase: f32,
    style: CursorStyle,
    is_visible: bool,
}

#[derive(Clone, Copy)]
pub enum CursorStyle {
    Block,     // █
    Underline, // _
    Bar,       // |
    Hollow,    // ▯
}

impl CursorIndicator {
    pub fn new() -> Self {
        Self {
            blink_phase: 0.0,
            style: CursorStyle::Block,
            is_visible: true,
        }
    }

    pub fn update(&mut self, delta: f32, is_typing: bool) {
        // Faster blink when typing
        let speed = if is_typing { 3.0 } else { 1.5 };
        self.blink_phase += delta * speed;

        if self.blink_phase > std::f32::consts::PI * 2.0 {
            self.blink_phase -= std::f32::consts::PI * 2.0;
        }

        // Determine visibility based on phase
        self.is_visible = self.blink_phase.sin() > 0.0;
    }

    pub fn get_cursor_char(&self) -> char {
        if !self.is_visible {
            return ' ';
        }

        match self.style {
            CursorStyle::Block => '█',
            CursorStyle::Underline => '_',
            CursorStyle::Bar => '│',
            CursorStyle::Hollow => '▯',
        }
    }

    pub fn set_style(&mut self, style: CursorStyle) {
        self.style = style;
    }
}

/// Tab order manager for keyboard navigation
pub struct TabOrderManager {
    tab_order: Vec<String>,
    current_index: usize,
    wrap_around: bool,
    skip_disabled: bool,
}

impl TabOrderManager {
    pub fn new() -> Self {
        Self {
            tab_order: Vec::new(),
            current_index: 0,
            wrap_around: true,
            skip_disabled: true,
        }
    }

    pub fn set_tab_order(&mut self, order: Vec<String>) {
        self.tab_order = order;
        self.current_index = 0;
    }

    pub fn next(&mut self) -> Option<&String> {
        if self.tab_order.is_empty() {
            return None;
        }

        self.current_index += 1;
        if self.current_index >= self.tab_order.len() {
            if self.wrap_around {
                self.current_index = 0;
            } else {
                self.current_index = self.tab_order.len() - 1;
                return None;
            }
        }

        self.tab_order.get(self.current_index)
    }

    pub fn previous(&mut self) -> Option<&String> {
        if self.tab_order.is_empty() {
            return None;
        }

        if self.current_index == 0 {
            if self.wrap_around {
                self.current_index = self.tab_order.len() - 1;
            } else {
                return None;
            }
        } else {
            self.current_index -= 1;
        }

        self.tab_order.get(self.current_index)
    }

    pub fn focus_specific(&mut self, element: &str) -> bool {
        if let Some(index) = self.tab_order.iter().position(|e| e == element) {
            self.current_index = index;
            true
        } else {
            false
        }
    }
}

/// Integration helper for cursor management
pub fn setup_smart_cursor(siv: &mut Cursive) -> SmartCursorManager {
    let mut cursor_manager = SmartCursorManager::new();

    // Set up Tab navigation
    siv.add_global_callback(cursive::event::Key::Tab, move |s| {
        // Tab navigation logic would go here
        s.focus_name("next_element").ok();
    });

    // Set up Shift+Tab navigation
    siv.add_global_callback(
        cursive::event::Event::Shift(cursive::event::Key::Tab),
        move |s| {
            // Reverse tab navigation
            s.focus_name("prev_element").ok();
        },
    );

    cursor_manager
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_smart_cursor_rules() {
        let manager = SmartCursorManager::new();

        let context = CursorContext {
            last_action: UserAction::SendMessage,
            has_error: false,
            suggestions_available: true,
            active_panel: PanelType::Input,
            has_notifications: false,
            is_processing: false,
        };

        let focus = manager.get_optimal_focus(&context);
        assert_eq!(focus, "suggestion_1");
    }

    #[test]
    fn test_cursor_indicator() {
        let mut indicator = CursorIndicator::new();

        indicator.update(0.5, false);
        let ch = indicator.get_cursor_char();
        assert!(ch == '█' || ch == ' ');
    }

    #[test]
    fn test_tab_order() {
        let mut tab_manager = TabOrderManager::new();
        tab_manager.set_tab_order(vec![
            "input".to_string(),
            "send".to_string(),
            "settings".to_string(),
        ]);

        assert_eq!(tab_manager.next(), Some(&"send".to_string()));
        assert_eq!(tab_manager.next(), Some(&"settings".to_string()));
        assert_eq!(tab_manager.next(), Some(&"input".to_string())); // Wraps around
    }
}
