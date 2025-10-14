//! Loading indicators and progress displays for long-running operations

use cursive::traits::{Nameable, Resizable};
use cursive::views::{Dialog, LinearLayout, ProgressBar, TextView};
use cursive::Cursive;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

/// Show an indeterminate loading dialog for unknown-length operations
pub fn show_loading_dialog(siv: &mut Cursive, title: &str, message: &str) {
    let dialog_name = format!("loading_dialog_{}", uuid::Uuid::new_v4());

    let mut layout = LinearLayout::vertical();
    layout.add_child(TextView::new(message));
    layout.add_child(cursive::views::DummyView.fixed_height(1));

    // Add animated spinner text
    layout.add_child(
        TextView::new("⠋ Processing...")
            .with_name(format!("{}_spinner", dialog_name)),
    );

    siv.add_layer(
        Dialog::around(layout)
            .title(title)
            .with_name(&dialog_name)
            .min_width(50),
    );

    // Start spinner animation
    start_spinner_animation(siv, &dialog_name);
}

/// Update loading dialog message
pub fn update_loading_message(siv: &mut Cursive, dialog_name: &str, new_message: &str) {
    let spinner_name = format!("{}_spinner", dialog_name);
    if let Some(mut spinner) = siv.find_name::<TextView>(&spinner_name) {
        spinner.set_content(new_message);
    }
}

/// Close loading dialog
pub fn close_loading_dialog(siv: &mut Cursive, dialog_name: &str) {
    siv.call_on_name(dialog_name, |dialog: &mut Dialog| {
        // Dialog found, will be removed
    });
    siv.pop_layer();
}

/// Show a progress dialog with percentage indicator
pub fn show_progress_dialog(
    siv: &mut Cursive,
    title: &str,
    message: &str,
    current: usize,
    total: usize,
) {
    let dialog_name = "progress_dialog";

    let progress = if total > 0 {
        (current as f64 / total as f64 * 100.0) as usize
    } else {
        0
    };

    let mut layout = LinearLayout::vertical();
    layout.add_child(TextView::new(message));
    layout.add_child(cursive::views::DummyView.fixed_height(1));

    // Progress bar using text characters
    let bar_width = 40;
    let filled = (progress * bar_width) / 100;
    let empty = bar_width - filled;
    let progress_bar = format!(
        "[{}{}] {}%",
        "█".repeat(filled),
        "░".repeat(empty),
        progress
    );

    layout.add_child(TextView::new(progress_bar).with_name("progress_bar"));
    layout.add_child(cursive::views::DummyView.fixed_height(1));
    layout.add_child(TextView::new(format!("{} / {}", current, total)).with_name("progress_text"));

    siv.add_layer(
        Dialog::around(layout)
            .title(title)
            .with_name(dialog_name)
            .min_width(50),
    );
}

/// Update progress dialog
pub fn update_progress_dialog(siv: &mut Cursive, current: usize, total: usize, message: &str) {
    let progress = if total > 0 {
        (current as f64 / total as f64 * 100.0) as usize
    } else {
        0
    };

    let bar_width = 40;
    let filled = (progress * bar_width) / 100;
    let empty = bar_width - filled;
    let progress_bar = format!(
        "[{}{}] {}%",
        "█".repeat(filled),
        "░".repeat(empty),
        progress
    );

    if let Some(mut bar) = siv.find_name::<TextView>("progress_bar") {
        bar.set_content(progress_bar);
    }

    if let Some(mut text) = siv.find_name::<TextView>("progress_text") {
        text.set_content(format!("{} / {} - {}", current, total, message));
    }
}

/// Close progress dialog
pub fn close_progress_dialog(siv: &mut Cursive) {
    siv.call_on_name("progress_dialog", |dialog: &mut Dialog| {
        // Dialog found, will be removed
    });
    siv.pop_layer();
}

/// Start animated spinner in a loading dialog
fn start_spinner_animation(siv: &mut Cursive, dialog_name: &str) {
    let spinner_name = format!("{}_spinner", dialog_name);
    let cb_sink = siv.cb_sink().clone();
    let dialog_name = dialog_name.to_string();

    let running = Arc::new(AtomicBool::new(true));
    let running_clone = running.clone();

    std::thread::spawn(move || {
        let spinner_frames = vec!["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
        let mut frame_index = 0;

        while running_clone.load(Ordering::Relaxed) {
            let spinner_char = spinner_frames[frame_index % spinner_frames.len()];
            let spinner_text = format!("{} Processing...", spinner_char);

            let spinner_name_clone = spinner_name.clone();
            let _ = cb_sink.send(Box::new(move |s| {
                if let Some(mut spinner) = s.find_name::<TextView>(&spinner_name_clone) {
                    spinner.set_content(spinner_text);
                }
            }));

            frame_index += 1;
            std::thread::sleep(Duration::from_millis(100));
        }
    });
}

/// Show a minimal inline loading indicator (non-blocking)
pub fn show_inline_loading(siv: &mut Cursive, target_view_name: &str, message: &str) {
    if let Some(mut view) = siv.find_name::<TextView>(target_view_name) {
        view.set_content(format!("⏳ {}", message));
    }
}

/// Clear inline loading indicator
pub fn clear_inline_loading(siv: &mut Cursive, target_view_name: &str, final_message: &str) {
    if let Some(mut view) = siv.find_name::<TextView>(target_view_name) {
        view.set_content(final_message);
    }
}

/// Show loading overlay with semi-transparent effect (simulated with borders)
pub fn show_loading_overlay(siv: &mut Cursive, message: &str) {
    let mut layout = LinearLayout::vertical();
    layout.add_child(cursive::views::DummyView.fixed_height(5));

    let mut center_layout = LinearLayout::horizontal();
    center_layout.add_child(cursive::views::DummyView.fixed_width(10));

    let mut content = LinearLayout::vertical();
    content.add_child(TextView::new(format!("╔═══════════════════════════╗")));
    content.add_child(TextView::new(format!("║                           ║")));
    content.add_child(TextView::new(format!("║    ⏳ {}    ║", message)));
    content.add_child(TextView::new(format!("║                           ║")));
    content.add_child(TextView::new(format!("╚═══════════════════════════╝")));

    center_layout.add_child(content);
    center_layout.add_child(cursive::views::DummyView.fixed_width(10));

    layout.add_child(center_layout);
    layout.add_child(cursive::views::DummyView.fixed_height(5));

    siv.add_layer(Dialog::around(layout).with_name("loading_overlay"));
}

/// Close loading overlay
pub fn close_loading_overlay(siv: &mut Cursive) {
    siv.call_on_name("loading_overlay", |dialog: &mut Dialog| {
        // Dialog found, will be removed
    });
    siv.pop_layer();
}

/// Show a toast-like notification (auto-dismissing after duration)
pub fn show_toast(siv: &mut Cursive, message: &str, duration_ms: u64) {
    let toast_name = format!("toast_{}", uuid::Uuid::new_v4());

    let toast = Dialog::around(TextView::new(message))
        .title("ℹ️")
        .with_name(&toast_name);

    siv.add_layer(toast);

    // Auto-dismiss after duration
    let cb_sink = siv.cb_sink().clone();
    let toast_name_clone = toast_name.clone();

    std::thread::spawn(move || {
        std::thread::sleep(Duration::from_millis(duration_ms));
        let _ = cb_sink.send(Box::new(move |s| {
            s.call_on_name(&toast_name_clone, |dialog: &mut Dialog| {
                // Dialog found, will be removed
            });
            s.pop_layer();
        }));
    });
}

/// Operation progress tracker for complex multi-step operations
pub struct OperationProgress {
    pub name: String,
    pub current_step: usize,
    pub total_steps: usize,
    pub current_message: String,
}

impl OperationProgress {
    pub fn new(name: String, total_steps: usize) -> Self {
        Self {
            name,
            current_step: 0,
            total_steps,
            current_message: String::new(),
        }
    }

    pub fn advance(&mut self, message: String) {
        self.current_step += 1;
        self.current_message = message;
    }

    pub fn show_dialog(&self, siv: &mut Cursive) {
        show_progress_dialog(
            siv,
            &self.name,
            &self.current_message,
            self.current_step,
            self.total_steps,
        );
    }

    pub fn update_dialog(&self, siv: &mut Cursive) {
        update_progress_dialog(
            siv,
            self.current_step,
            self.total_steps,
            &self.current_message,
        );
    }

    pub fn complete(&self, siv: &mut Cursive) {
        close_progress_dialog(siv);
        show_toast(siv, &format!("✅ {} completed!", self.name), 2000);
    }
}
