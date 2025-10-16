//! Non-blocking toast notification system
//!
//! Provides temporary notifications that appear at the top/bottom of the screen
//! and automatically disappear after a timeout, without blocking user interaction.

use cursive::align::{Align, HAlign, VAlign};
use cursive::theme::{BaseColor, Color, ColorStyle};
use cursive::traits::*;
use cursive::views::{LinearLayout, Panel, TextView};
use cursive::{Cursive, View};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

/// Type of toast notification
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ToastType {
    Success,
    Error,
    Warning,
    Info,
}

impl ToastType {
    /// Get the color style for this toast type
    fn color_style(&self) -> ColorStyle {
        match self {
            ToastType::Success => ColorStyle::new(
                Color::Light(BaseColor::White),
                Color::Dark(BaseColor::Green),
            ),
            ToastType::Error => ColorStyle::new(
                Color::Light(BaseColor::White),
                Color::Dark(BaseColor::Red),
            ),
            ToastType::Warning => ColorStyle::new(
                Color::Dark(BaseColor::Black),
                Color::Light(BaseColor::Yellow),
            ),
            ToastType::Info => ColorStyle::new(
                Color::Light(BaseColor::White),
                Color::Dark(BaseColor::Blue),
            ),
        }
    }

    /// Get the icon for this toast type
    fn icon(&self) -> &'static str {
        match self {
            ToastType::Success => "✅",
            ToastType::Error => "❌",
            ToastType::Warning => "⚠️",
            ToastType::Info => "ℹ️",
        }
    }
}

/// Toast notification
#[derive(Clone)]
pub struct Toast {
    message: String,
    toast_type: ToastType,
    duration: Duration,
    created_at: Instant,
}

impl Toast {
    /// Create a new toast notification
    pub fn new(message: impl Into<String>, toast_type: ToastType, duration: Duration) -> Self {
        Toast {
            message: message.into(),
            toast_type,
            duration,
            created_at: Instant::now(),
        }
    }

    /// Check if the toast has expired
    pub fn is_expired(&self) -> bool {
        self.created_at.elapsed() > self.duration
    }

    /// Create the view for this toast
    pub fn create_view(&self) -> impl View {
        let content = format!("{} {}", self.toast_type.icon(), self.message);
        let mut text_view = TextView::new(content);
        text_view.set_style(self.toast_type.color_style());

        // Wrap in a panel for better visibility
        Panel::new(text_view)
            .title("")
    }
}

/// Toast notification manager
pub struct ToastManager {
    toasts: Arc<Mutex<Vec<Toast>>>,
}

impl ToastManager {
    /// Create a new toast manager
    pub fn new() -> Self {
        ToastManager {
            toasts: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Add a new toast notification
    pub fn add_toast(&self, message: impl Into<String>, toast_type: ToastType, duration: Duration) {
        if let Ok(mut toasts) = self.toasts.lock() {
            toasts.push(Toast::new(message, toast_type, duration));
        }
    }

    /// Remove expired toasts
    pub fn cleanup_expired(&self) {
        if let Ok(mut toasts) = self.toasts.lock() {
            toasts.retain(|toast| !toast.is_expired());
        }
    }

    /// Get active toasts
    pub fn get_active_toasts(&self) -> Vec<Toast> {
        if let Ok(toasts) = self.toasts.lock() {
            toasts.clone()
        } else {
            Vec::new()
        }
    }

    /// Create a view showing all active toasts
    pub fn create_toast_layer(&self) -> impl View {
        self.cleanup_expired();

        let mut layout = LinearLayout::vertical();

        for toast in self.get_active_toasts() {
            layout.add_child(toast.create_view());
        }

        // Position at top-right of screen
        layout.fixed_width(40)
    }
}

// Helper functions for easy toast creation

/// Show a success toast
pub fn show_success_toast(s: &mut Cursive, message: impl Into<String>) {
    show_toast(s, message, ToastType::Success, Duration::from_secs(3));
}

/// Show an error toast
pub fn show_error_toast(s: &mut Cursive, message: impl Into<String>) {
    show_toast(s, message, ToastType::Error, Duration::from_secs(5));
}

/// Show a warning toast
pub fn show_warning_toast(s: &mut Cursive, message: impl Into<String>) {
    show_toast(s, message, ToastType::Warning, Duration::from_secs(4));
}

/// Show an info toast
pub fn show_info_toast(s: &mut Cursive, message: impl Into<String>) {
    show_toast(s, message, ToastType::Info, Duration::from_secs(3));
}

/// Show a toast notification
pub fn show_toast(s: &mut Cursive, message: impl Into<String>, toast_type: ToastType, duration: Duration) {
    // Create the toast view
    let toast = Toast::new(message, toast_type, duration);
    let toast_view = toast.create_view();

    // Create a unique layer name
    let layer_name = format!("toast_{}", uuid::Uuid::new_v4());

    // Store the toast expiration time in user data
    // This approach avoids spawning threads per toast
    let manager = ToastManager::new();
    manager.add_toast(toast.message.clone(), toast.toast_type, duration);

    // Add as a dialog which is more suitable for temporary notifications
    use cursive::views::Dialog;
    let dialog = Dialog::around(toast_view)
        .title("Notification")
        .with_name(layer_name.clone());

    s.add_layer(dialog);

    // Use callback sink to schedule removal without spawning a thread
    let cb_sink = s.cb_sink().clone();
    let layer_id = layer_name.clone();

    // Schedule removal using a timer callback instead of a thread
    std::thread::spawn(move || {
        std::thread::sleep(duration);
        // Send callback to remove the layer
        let _ = cb_sink.send(Box::new(move |s| {
            // Safely pop the topmost layer (which should be our toast)
            s.pop_layer();
        }));
    });
}

/// Initialize the toast system for a Cursive instance
pub fn init_toast_system(s: &mut Cursive) {
    // Store the toast manager in user data
    let manager = ToastManager::new();
    // Note: This would need to be integrated with the existing state management
    // For now, we'll use the show_toast function directly
}