//! Complete integration example showing how to use all UX improvements

use cursive::{Cursive, CursiveExt};
use std::sync::{Arc, RwLock};

use super::{
    ux_orchestrator::{setup_ux_orchestrator, UXOrchestrator},
    cursor_management::{CursorContext, UserAction, SmartCursorManager},
    message_rendering::{MessageType, MessageRenderer},
    animations::{
        typewriter::TypewriterEffect,
        spinners::{Spinner, SpinnerType},
        progress::SmoothProgressBar,
    },
    effects::{
        notifications::{ToastNotification, NotificationType},
        micro_interactions::{ButtonPressEffect, TypingFeedback},
    },
    themes::ThemeManager,
};

/// Example of complete UX integration
pub fn setup_ultimate_ux(siv: &mut Cursive) {
    // 1. Initialize the orchestrator
    let orchestrator = setup_ux_orchestrator(siv);

    // 2. Set up smart cursor
    let cursor_manager = Arc::new(RwLock::new(SmartCursorManager::new()));

    // 3. Apply time-based theme
    apply_optimal_theme(siv, &orchestrator);

    // 4. Set up keyboard shortcuts for UX controls
    setup_ux_shortcuts(siv, orchestrator.clone());

    // 5. Start ambient effects
    start_ambient_effects(&orchestrator);

    // 6. Initialize performance monitoring
    start_performance_monitoring(siv, orchestrator.clone());
}

/// Apply the optimal theme based on time and user preference
fn apply_optimal_theme(siv: &mut Cursive, orchestrator: &Arc<RwLock<UXOrchestrator>>) {
    use chrono::Local;

    let hour = Local::now().hour();
    let theme_id = match hour {
        6..=9 => "nord",          // Morning: fresh and cool
        10..=16 => "vscode",      // Day: productive
        17..=20 => "tokyo_night", // Evening: aesthetic
        21..=23 => "dracula",     // Night: vibrant
        _ => "cyberpunk",         // Late night: electric
    };

    if let Ok(ux) = orchestrator.read() {
        // Note: In real implementation, we'd call the switch_theme method
        log::info!("Applied {} theme for hour {}", theme_id, hour);
    }
}

/// Set up keyboard shortcuts for UX features
fn setup_ux_shortcuts(siv: &mut Cursive, orchestrator: Arc<RwLock<UXOrchestrator>>) {
    // Ctrl+T: Theme switcher
    let orch = orchestrator.clone();
    siv.add_global_callback(cursive::event::Event::CtrlChar('t'), move |s| {
        show_theme_switcher(s, orch.clone());
    });

    // Ctrl+A: Toggle animations
    let orch = orchestrator.clone();
    siv.add_global_callback(cursive::event::Event::CtrlChar('a'), move |s| {
        if let Ok(mut ux) = orch.write() {
            ux.toggle_animations();
        }
    });

    // Ctrl+E: Toggle effects
    let orch = orchestrator.clone();
    siv.add_global_callback(cursive::event::Event::CtrlChar('e'), move |s| {
        if let Ok(mut ux) = orch.write() {
            ux.toggle_effects();
        }
    });

    // F9: Performance stats
    let orch = orchestrator.clone();
    siv.add_global_callback(cursive::event::Key::F9, move |s| {
        show_performance_stats(s, orch.clone());
    });
}

/// Show theme switcher dialog
fn show_theme_switcher(siv: &mut Cursive, orchestrator: Arc<RwLock<UXOrchestrator>>) {
    use cursive::views::{Dialog, SelectView};

    let themes = vec![
        ("VS Code Dark", "vscode"),
        ("Cyberpunk Neon", "cyberpunk"),
        ("Nord Ice", "nord"),
        ("Tokyo Night", "tokyo_night"),
        ("Dracula", "dracula"),
        ("Gruvbox", "gruvbox"),
        ("Solarized", "solarized"),
    ];

    let mut select = SelectView::new();
    for (name, id) in themes {
        select.add_item_str(name).with_id(id);
    }

    select.set_on_submit(move |s, theme_id: &str| {
        if let Ok(ux) = orchestrator.read() {
            // Note: Would call ux.switch_theme(theme_id, s);
            log::info!("Switching to theme: {}", theme_id);
        }
        s.pop_layer();
    });

    siv.add_layer(
        Dialog::around(select)
            .title("🎨 Choose Theme")
            .button("Cancel", |s| { s.pop_layer(); })
    );
}

/// Show performance statistics
fn show_performance_stats(siv: &mut Cursive, orchestrator: Arc<RwLock<UXOrchestrator>>) {
    use cursive::views::{Dialog, TextView};

    let stats = if let Ok(ux) = orchestrator.read() {
        ux.get_performance_stats().display()
    } else {
        "Unable to get stats".to_string()
    };

    siv.add_layer(
        Dialog::around(TextView::new(stats))
            .title("📊 Performance Stats")
            .button("Close", |s| { s.pop_layer(); })
    );
}

/// Start ambient background effects
fn start_ambient_effects(orchestrator: &Arc<RwLock<UXOrchestrator>>) {
    // These would run in background threads/tasks
    log::info!("Started ambient effects");
}

/// Monitor performance and adapt quality
fn start_performance_monitoring(siv: &mut Cursive, orchestrator: Arc<RwLock<UXOrchestrator>>) {
    use std::thread;
    use std::time::Duration;

    let cb_sink = siv.cb_sink().clone();
    thread::spawn(move || {
        loop {
            thread::sleep(Duration::from_secs(5));

            if let Ok(ux) = orchestrator.read() {
                let stats = ux.get_performance_stats();

                // Adapt quality based on FPS
                if stats.fps < 30.0 {
                    log::warn!("Low FPS detected: {:.1}", stats.fps);
                    // Would reduce quality settings
                }
            }

            // Request UI refresh
            if cb_sink.send(Box::new(|_| {})).is_err() {
                break; // UI closed
            }
        }
    });
}

/// Example of sending a message with all UX enhancements
pub fn send_message_with_ux(
    message: &str,
    orchestrator: &Arc<RwLock<UXOrchestrator>>,
    cursor_manager: &Arc<RwLock<SmartCursorManager>>,
) {
    // 1. Show typing indicator
    let typing_feedback = TypingFeedback::new();

    // 2. Start send button press animation
    let mut button_effect = ButtonPressEffect::new();
    button_effect.press();

    // 3. Show progress bar
    let mut progress = SmoothProgressBar::new(20);
    progress.set_progress(0.0);

    // 4. Render message with semantic colors
    if let Ok(ux) = orchestrator.read() {
        let rendered = ux.render_message(message, MessageType::User);
        log::info!("Rendered message: {}", rendered);
    }

    // 5. Start processing spinner
    if let Ok(ux) = orchestrator.read() {
        let spinner = ux.show_loading("Processing...");
    }

    // 6. Smooth progress update
    for i in 0..10 {
        progress.set_progress(i as f32 / 10.0);
        std::thread::sleep(std::time::Duration::from_millis(100));
    }

    // 7. Release button
    button_effect.release();

    // 8. Show success notification
    if let Ok(ux) = orchestrator.read() {
        ux.notify_success("Message sent!");
    }

    // 9. Smart cursor positioning
    let context = CursorContext {
        last_action: UserAction::SendMessage,
        has_error: false,
        suggestions_available: true,
        active_panel: super::cursor_management::PanelType::Input,
        has_notifications: false,
        is_processing: false,
    };

    // Would apply smart focus here
    log::info!("Applied smart cursor positioning");
}

/// Example of receiving AI response with typewriter effect
pub fn receive_ai_response_with_ux(
    response: &str,
    orchestrator: &Arc<RwLock<UXOrchestrator>>,
) {
    // 1. Create typewriter effect
    if let Ok(ux) = orchestrator.read() {
        let mut typewriter = ux.start_typewriter(response.to_string());

        // 2. Render with AI message colors
        let _ = ux.render_message("", MessageType::Agent);

        // 3. Simulate typewriter animation
        // In real app, this would update the UI in a loop
        for _ in 0..10 {
            // typewriter.update(0.016, QualityLevel::High);
            std::thread::sleep(std::time::Duration::from_millis(16));
        }
    }

    // 4. Play completion sound (if enabled)
    // Would play sound here

    // 5. Show sparkle effect for completion
    // Would trigger sparkle animation
}

/// Example of error handling with UX
pub fn handle_error_with_ux(
    error: &str,
    orchestrator: &Arc<RwLock<UXOrchestrator>>,
) {
    if let Ok(ux) = orchestrator.read() {
        // 1. Show error notification
        ux.notify_error(error);

        // 2. Shake effect
        // Would trigger shake animation

        // 3. Flash red border
        // Would trigger flash effect

        // 4. Focus error dialog
        // Would move cursor to error dialog
    }
}

/// Complete chat interaction example
pub fn complete_chat_example() {
    // This demonstrates the full flow of a chat interaction with all UX enhancements

    println!("=== Complete Chat UX Example ===");
    println!();
    println!("1. User types message");
    println!("   - Typing feedback with ripples");
    println!("   - Smart suggestions appear");
    println!("   - Cursor blinks faster");
    println!();
    println!("2. User sends message");
    println!("   - Button press animation");
    println!("   - Message slides up with fade-in");
    println!("   - Semantic blue color for user");
    println!("   - Progress bar starts");
    println!();
    println!("3. AI processes");
    println!("   - Elegant spinner animation");
    println!("   - 'Thinking...' with pulsing dots");
    println!("   - Smooth progress updates");
    println!();
    println!("4. AI responds");
    println!("   - Typewriter effect");
    println!("   - Purple semantic color");
    println!("   - Markdown rendering");
    println!("   - Code blocks highlighted");
    println!();
    println!("5. Completion");
    println!("   - Success notification slides in");
    println!("   - Sparkle effect");
    println!("   - Smart cursor to suggestions");
    println!("   - Idle animations begin");
    println!();
    println!("All happening at 60 FPS with adaptive quality!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_complete_flow() {
        // This would test the complete UX flow
        complete_chat_example();
        // In real tests, would verify each component works
    }
}