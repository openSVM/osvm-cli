//! Toast notifications and alert system

use super::{EffectIntensity, VisualEffect};
use std::time::{Duration, Instant};

/// Notification type determines appearance and behavior
#[derive(Clone, Copy, PartialEq)]
pub enum NotificationType {
    Success,
    Error,
    Warning,
    Info,
    Achievement,
}

/// Toast notification that appears and fades
pub struct ToastNotification {
    message: String,
    notification_type: NotificationType,
    position: ToastPosition,
    lifetime: Duration,
    created_at: Instant,
    fade_phase: f32,
    slide_offset: f32,
}

#[derive(Clone, Copy)]
pub enum ToastPosition {
    TopLeft,
    TopCenter,
    TopRight,
    BottomLeft,
    BottomCenter,
    BottomRight,
}

impl ToastNotification {
    pub fn new(message: String, notification_type: NotificationType) -> Self {
        Self {
            message,
            notification_type,
            position: ToastPosition::TopRight,
            lifetime: Duration::from_secs(3),
            created_at: Instant::now(),
            fade_phase: 0.0,
            slide_offset: -20.0, // Start off-screen
        }
    }

    pub fn with_duration(mut self, duration: Duration) -> Self {
        self.lifetime = duration;
        self
    }

    pub fn with_position(mut self, position: ToastPosition) -> Self {
        self.position = position;
        self
    }

    fn get_icon(&self) -> &str {
        match self.notification_type {
            NotificationType::Success => "✓",
            NotificationType::Error => "✗",
            NotificationType::Warning => "⚠",
            NotificationType::Info => "ℹ",
            NotificationType::Achievement => "🏆",
        }
    }

    fn get_border_style(&self) -> (&str, &str, &str, &str) {
        match self.notification_type {
            NotificationType::Success => ("╭", "─", "╮", "╯"),
            NotificationType::Error => ("┌", "═", "┐", "┘"),
            NotificationType::Warning => ("╔", "═", "╗", "╝"),
            NotificationType::Info => ("┏", "━", "┓", "┛"),
            NotificationType::Achievement => ("★", "═", "★", "★"),
        }
    }

    pub fn render_toast(&self) -> String {
        let icon = self.get_icon();
        let (tl, top, tr, br) = self.get_border_style();

        let width = self.message.len() + 4;
        let top_border = format!("{}{}{}", tl, top.repeat(width), tr);
        let content = format!("│ {} {} │", icon, self.message);
        let bottom_border = format!("└{}{}┘", "─".repeat(width), br);

        // Apply fade effect
        let opacity_indicator = if self.fade_phase < 0.3 {
            "░"
        } else if self.fade_phase < 0.7 {
            "▒"
        } else {
            "▓"
        };

        format!("{}\n{}\n{}", top_border, content, bottom_border)
    }
}

impl VisualEffect for ToastNotification {
    fn update(&mut self, delta: f32) {
        let elapsed = self.created_at.elapsed();
        let progress = elapsed.as_secs_f32() / self.lifetime.as_secs_f32();

        // Slide in animation (first 0.3s)
        if progress < 0.1 {
            self.slide_offset = self.slide_offset * 0.9; // Smooth slide in
            self.fade_phase = progress * 10.0;
        }
        // Stay visible
        else if progress < 0.8 {
            self.slide_offset = 0.0;
            self.fade_phase = 1.0;
        }
        // Fade out (last 0.2s)
        else {
            self.fade_phase = 1.0 - (progress - 0.8) * 5.0;
            self.slide_offset = (progress - 0.8) * 100.0; // Slide out
        }
    }

    fn render(&self) -> String {
        self.render_toast()
    }

    fn is_active(&self) -> bool {
        self.created_at.elapsed() < self.lifetime + Duration::from_millis(200) // Extra time for fade out
    }

    fn reset(&mut self) {
        self.created_at = Instant::now();
        self.fade_phase = 0.0;
        self.slide_offset = -20.0;
    }
}

/// Stack of toast notifications
pub struct NotificationStack {
    notifications: Vec<ToastNotification>,
    max_visible: usize,
    spacing: usize,
}

impl NotificationStack {
    pub fn new() -> Self {
        Self {
            notifications: Vec::new(),
            max_visible: 3,
            spacing: 1,
        }
    }

    pub fn push(&mut self, notification: ToastNotification) {
        // Remove oldest if at max
        if self.notifications.len() >= self.max_visible {
            self.notifications.remove(0);
        }

        self.notifications.push(notification);
    }

    pub fn render_stack(&self) -> Vec<String> {
        self.notifications
            .iter()
            .map(|n| n.render_toast())
            .collect()
    }

    pub fn clear(&mut self) {
        self.notifications.clear();
    }
}

impl VisualEffect for NotificationStack {
    fn update(&mut self, delta: f32) {
        // Update all notifications
        for notification in &mut self.notifications {
            notification.update(delta);
        }

        // Remove expired notifications
        self.notifications.retain(|n| n.is_active());
    }

    fn render(&self) -> String {
        self.render_stack().join("\n")
    }

    fn is_active(&self) -> bool {
        !self.notifications.is_empty()
    }

    fn reset(&mut self) {
        self.clear();
    }
}

/// Achievement popup with celebration effect
pub struct AchievementPopup {
    title: String,
    description: String,
    icon: String,
    animation_phase: f32,
    is_active: bool,
    sparkles: Vec<(f32, f32, char)>,
}

impl AchievementPopup {
    pub fn new(title: String, description: String, icon: String) -> Self {
        Self {
            title,
            description,
            icon,
            animation_phase: 0.0,
            is_active: true,
            sparkles: Vec::new(),
        }
    }

    pub fn trigger(&mut self) {
        self.is_active = true;
        self.animation_phase = 0.0;

        // Create sparkles
        use rand::Rng;
        let mut rng = rand::rng();

        for _ in 0..10 {
            self.sparkles.push((
                rng.random_range(-10.0..10.0),
                rng.random_range(-5.0..5.0),
                ['✨', '⭐', '✦', '･'][rng.random_range(0..4)],
            ));
        }
    }

    fn render_achievement(&self) -> String {
        let scale = if self.animation_phase < 0.5 {
            1.0 + self.animation_phase
        } else {
            1.5 - (self.animation_phase - 0.5)
        };

        let width = (30.0 * scale) as usize;

        let border = "═".repeat(width);
        let padding = " ".repeat((width - self.title.len()) / 2);

        format!(
            "╔{}╗\n║{}{}{}║\n║ {} ║\n║{}{}{}║\n╚{}╝",
            border,
            padding,
            self.icon,
            padding,
            self.title,
            " ".repeat((width - self.description.len()) / 2),
            self.description,
            " ".repeat((width - self.description.len()) / 2),
            border
        )
    }
}

impl VisualEffect for AchievementPopup {
    fn update(&mut self, delta: f32) {
        if self.is_active {
            self.animation_phase += delta * 0.5;

            // Update sparkles
            for sparkle in &mut self.sparkles {
                sparkle.0 += rand::random::<f32>() * 2.0 - 1.0; // Random drift
                sparkle.1 -= delta * 5.0; // Fall
            }

            if self.animation_phase > 3.0 {
                self.is_active = false;
            }
        }
    }

    fn render(&self) -> String {
        if self.is_active {
            self.render_achievement()
        } else {
            String::new()
        }
    }

    fn is_active(&self) -> bool {
        self.is_active
    }

    fn reset(&mut self) {
        self.is_active = false;
        self.animation_phase = 0.0;
        self.sparkles.clear();
    }
}

/// Alert dialog for important messages
pub struct AlertDialog {
    title: String,
    message: String,
    alert_type: AlertType,
    buttons: Vec<String>,
    selected_button: usize,
    is_visible: bool,
    shake_intensity: f32,
}

#[derive(Clone, Copy)]
pub enum AlertType {
    Info,
    Warning,
    Error,
    Confirm,
}

impl AlertDialog {
    pub fn new(title: String, message: String, alert_type: AlertType) -> Self {
        let buttons = match alert_type {
            AlertType::Confirm => vec!["OK".to_string(), "Cancel".to_string()],
            _ => vec!["OK".to_string()],
        };

        Self {
            title,
            message,
            alert_type,
            buttons,
            selected_button: 0,
            is_visible: false,
            shake_intensity: 0.0,
        }
    }

    pub fn show(&mut self) {
        self.is_visible = true;
        if matches!(self.alert_type, AlertType::Error | AlertType::Warning) {
            self.shake_intensity = 1.0;
        }
    }

    pub fn select_next_button(&mut self) {
        self.selected_button = (self.selected_button + 1) % self.buttons.len();
    }

    pub fn get_selected_button(&self) -> Option<&String> {
        self.buttons.get(self.selected_button)
    }

    fn render_dialog(&self) -> String {
        let icon = match self.alert_type {
            AlertType::Info => "ℹ",
            AlertType::Warning => "⚠",
            AlertType::Error => "✗",
            AlertType::Confirm => "?",
        };

        let shake_offset = if self.shake_intensity > 0.0 {
            ((self.shake_intensity * 10.0).sin() * 2.0) as i32
        } else {
            0
        };

        let padding = " ".repeat(shake_offset.abs() as usize);

        let buttons_str = self
            .buttons
            .iter()
            .enumerate()
            .map(|(i, b)| {
                if i == self.selected_button {
                    format!("[{}]", b)
                } else {
                    format!(" {} ", b)
                }
            })
            .collect::<Vec<_>>()
            .join("  ");

        format!(
            "{}╔════════════════════════════╗\n{}║ {} {} ║\n{}║ ║\n{}║ {} ║\n{}║ ║\n{}║ {} ║\n{}╚════════════════════════════╝",
            padding, padding, icon, self.title,
            padding, padding, self.message,
            padding, padding, buttons_str, padding
        )
    }
}

impl VisualEffect for AlertDialog {
    fn update(&mut self, delta: f32) {
        // Decay shake
        self.shake_intensity = (self.shake_intensity - delta * 2.0).max(0.0);
    }

    fn render(&self) -> String {
        if self.is_visible {
            self.render_dialog()
        } else {
            String::new()
        }
    }

    fn is_active(&self) -> bool {
        self.is_visible
    }

    fn reset(&mut self) {
        self.is_visible = false;
        self.selected_button = 0;
        self.shake_intensity = 0.0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_toast_notification() {
        let toast = ToastNotification::new(
            "Operation successful!".to_string(),
            NotificationType::Success,
        );

        let rendered = toast.render_toast();
        assert!(rendered.contains("✓"));
        assert!(rendered.contains("Operation successful!"));
    }

    #[test]
    fn test_notification_stack() {
        let mut stack = NotificationStack::new();

        stack.push(ToastNotification::new(
            "First".to_string(),
            NotificationType::Info,
        ));
        stack.push(ToastNotification::new(
            "Second".to_string(),
            NotificationType::Success,
        ));

        assert_eq!(stack.notifications.len(), 2);

        // Update for lifetime
        for _ in 0..100 {
            stack.update(0.1);
        }

        // Should be expired
        assert!(stack.notifications.is_empty() || stack.notifications.len() < 2);
    }
}
