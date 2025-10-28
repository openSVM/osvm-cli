//! Main UX orchestrator - coordinates all visual effects, animations, and interactions

use cursive::{Cursive, CursiveExt, Vec2};
use std::sync::{Arc, RwLock};
use std::time::Instant;

use super::{
    animations::{
        color_transitions::{ColorFade, RainbowCycle},
        progress::SmoothProgressBar,
        spinners::{Spinner, SpinnerType},
        typewriter::TypewriterEffect,
        AnimationManager, AnimationState, FrameRateController, QualityLevel,
    },
    effects::{
        focus::{FocusIndicator, FocusManager},
        micro_interactions::*,
        notifications::{NotificationStack, NotificationType, ToastNotification},
        AttentionDirector,
    },
    layouts::LayoutManager,
    message_rendering::{MessageRenderer, MessageType},
    themes::ThemeManager,
};

/// Central UX coordinator that manages all visual enhancements
pub struct UXOrchestrator {
    // Core managers
    pub theme_manager: Arc<RwLock<ThemeManager>>,
    pub layout_manager: Arc<RwLock<LayoutManager>>,
    pub animation_manager: Arc<RwLock<AnimationManager>>,
    pub attention_director: Arc<RwLock<AttentionDirector>>,
    pub focus_manager: Arc<RwLock<FocusManager>>,

    // Effects
    pub notifications: Arc<RwLock<NotificationStack>>,
    pub message_renderer: Arc<RwLock<MessageRenderer>>,

    // Performance
    frame_controller: FrameRateController,
    last_update: Instant,

    // Settings
    animations_enabled: bool,
    effects_enabled: bool,
    adaptive_quality: bool,
}

impl UXOrchestrator {
    pub fn new(terminal_size: Vec2) -> Self {
        Self {
            theme_manager: Arc::new(RwLock::new(ThemeManager::new())),
            layout_manager: Arc::new(RwLock::new(LayoutManager::new(terminal_size))),
            animation_manager: Arc::new(RwLock::new(AnimationManager::new(60))),
            attention_director: Arc::new(RwLock::new(AttentionDirector::new())),
            focus_manager: Arc::new(RwLock::new(FocusManager::new())),
            notifications: Arc::new(RwLock::new(NotificationStack::new())),
            message_renderer: Arc::new(RwLock::new(MessageRenderer::new())),
            frame_controller: FrameRateController::new(60),
            last_update: Instant::now(),
            animations_enabled: true,
            effects_enabled: true,
            adaptive_quality: true,
        }
    }

    /// Initialize with user preferences
    pub fn initialize(&mut self, siv: &mut Cursive) {
        // Set theme based on time of day
        self.apply_time_based_theme(siv);

        // Register focusable elements
        self.register_ui_elements();

        // Start background animations
        self.start_ambient_animations();

        // Apply initial layout
        self.apply_responsive_layout(siv);
    }

    /// Main update loop - call this every frame
    pub fn update(&mut self, siv: &mut Cursive) {
        if !self.frame_controller.should_update() {
            return; // Skip frame to maintain target FPS
        }

        let delta = self.last_update.elapsed().as_secs_f32();
        self.last_update = Instant::now();

        // Get quality level for adaptive rendering
        let quality = if self.adaptive_quality {
            self.frame_controller.quality_level()
        } else {
            QualityLevel::High
        };

        // Update all systems
        if self.animations_enabled {
            self.update_animations(delta, quality);
        }

        if self.effects_enabled {
            self.update_effects(delta);
        }

        self.update_notifications(delta);
        self.check_resize(siv);

        // UI updates automatically when views are modified
    }

    /// Handle terminal resize
    pub fn on_resize(&mut self, siv: &mut Cursive, new_size: Vec2) {
        if let Ok(mut layout_mgr) = self.layout_manager.write() {
            if layout_mgr.on_resize(new_size) {
                // Layout type changed, rebuild UI
                self.apply_responsive_layout(siv);
            }
        }
    }

    /// Apply time-based theme
    fn apply_time_based_theme(&self, siv: &mut Cursive) {
        if let Ok(theme_mgr) = self.theme_manager.read() {
            let theme = theme_mgr.theme_for_time();
            siv.set_theme(theme);
        }
    }

    /// Apply responsive layout based on terminal size
    fn apply_responsive_layout(&self, siv: &mut Cursive) {
        if let Ok(layout_mgr) = self.layout_manager.read() {
            let config = layout_mgr.config();

            // Apply layout configuration
            // This would update the actual UI layout
            log::info!(
                "Applied {:?} layout: sidebar={}, input={}",
                config.layout_type,
                config.sidebar_width,
                config.input_height
            );
        }
    }

    /// Register UI elements for focus management
    fn register_ui_elements(&self) {
        if let Ok(mut focus_mgr) = self.focus_manager.write() {
            focus_mgr.register_element("input".to_string());
            focus_mgr.register_element("send_button".to_string());
            focus_mgr.register_element("chat_list".to_string());
            focus_mgr.register_element("session_list".to_string());
            focus_mgr.register_element("settings".to_string());
        }
    }

    /// Start ambient background animations
    fn start_ambient_animations(&self) {
        // Add subtle background animations for visual interest
        if let Ok(mut anim_mgr) = self.animation_manager.write() {
            // Add a subtle color cycle for the title bar
            // let rainbow = Box::new(RainbowCycle::new());
            // anim_mgr.add_animation(rainbow);
        }
    }

    /// Update all animations
    fn update_animations(&mut self, delta: f32, quality: QualityLevel) {
        if let Ok(mut anim_mgr) = self.animation_manager.write() {
            anim_mgr.update();
        }
    }

    /// Update all visual effects
    fn update_effects(&mut self, delta: f32) {
        if let Ok(mut attention) = self.attention_director.write() {
            attention.update(delta);
        }
    }

    /// Update notification stack
    fn update_notifications(&mut self, delta: f32) {
        // FUTURE: Notification updates pending VisualEffect trait stabilization
        // if let Ok(mut notifications) = self.notifications.write() {
        //     notifications.update(delta);
        // }
    }

    /// Check for terminal resize
    fn check_resize(&mut self, siv: &mut Cursive) {
        let current_size = siv.screen_size();
        // Compare with stored size and trigger resize if changed
    }

    // ===== User Actions =====

    /// Show a success notification
    pub fn notify_success(&self, message: &str) {
        if let Ok(mut notifications) = self.notifications.write() {
            notifications.push(ToastNotification::new(
                message.to_string(),
                NotificationType::Success,
            ));
        }
    }

    /// Show an error notification
    pub fn notify_error(&self, message: &str) {
        if let Ok(mut notifications) = self.notifications.write() {
            notifications.push(ToastNotification::new(
                message.to_string(),
                NotificationType::Error,
            ));
        }
    }

    /// Start typewriter effect for a message
    pub fn start_typewriter(&self, text: String) -> TypewriterEffect {
        let mut typewriter = TypewriterEffect::new(text);
        typewriter.set_speed(1.2); // Slightly faster than normal typing
        typewriter
    }

    /// Show loading spinner
    pub fn show_loading(&self, label: &str) -> Spinner {
        Spinner::new(SpinnerType::Dots, label.to_string())
    }

    /// Create smooth progress bar
    pub fn create_progress(&self, width: usize) -> SmoothProgressBar {
        SmoothProgressBar::new(width)
    }

    /// Trigger button press effect
    pub fn press_button(&self, button_id: &str) {
        // Would trigger button press animation
        log::debug!("Button pressed: {}", button_id);
    }

    /// Focus next element
    pub fn focus_next(&self) -> Option<String> {
        if let Ok(mut focus_mgr) = self.focus_manager.write() {
            focus_mgr.focus_next().cloned()
        } else {
            None
        }
    }

    /// Focus previous element
    pub fn focus_previous(&self) -> Option<String> {
        if let Ok(mut focus_mgr) = self.focus_manager.write() {
            focus_mgr.focus_previous().cloned()
        } else {
            None
        }
    }

    /// Switch theme
    pub fn switch_theme(&self, theme_id: &str, siv: &mut Cursive) {
        if let Ok(mut theme_mgr) = self.theme_manager.write() {
            if theme_mgr.switch_theme(theme_id) {
                siv.set_theme(theme_mgr.current());
                self.notify_success(&format!("Switched to {} theme", theme_id));
            }
        }
    }

    /// Render message with semantic colors
    pub fn render_message(&self, content: &str, msg_type: MessageType) -> String {
        if let Ok(renderer) = self.message_renderer.read() {
            // Simplified rendering for now
            format!(
                "{} {}: {}",
                msg_type.ascii_icon(),
                if msg_type == MessageType::User {
                    "You"
                } else {
                    "Agent"
                },
                content
            )
        } else {
            content.to_string()
        }
    }

    /// Get current performance stats
    pub fn get_performance_stats(&self) -> PerformanceStats {
        PerformanceStats {
            fps: self.frame_controller.current_fps(),
            quality: self.frame_controller.quality_level(),
            animations_active: self.animations_enabled,
            effects_active: self.effects_enabled,
        }
    }

    /// Toggle animations
    pub fn toggle_animations(&mut self) {
        self.animations_enabled = !self.animations_enabled;
        let status = if self.animations_enabled {
            "enabled"
        } else {
            "disabled"
        };
        self.notify_success(&format!("Animations {}", status));
    }

    /// Toggle effects
    pub fn toggle_effects(&mut self) {
        self.effects_enabled = !self.effects_enabled;
        let status = if self.effects_enabled {
            "enabled"
        } else {
            "disabled"
        };
        self.notify_success(&format!("Effects {}", status));
    }
}

/// Performance statistics
pub struct PerformanceStats {
    pub fps: f32,
    pub quality: QualityLevel,
    pub animations_active: bool,
    pub effects_active: bool,
}

impl PerformanceStats {
    pub fn display(&self) -> String {
        format!(
            "FPS: {:.1} | Quality: {:?} | Animations: {} | Effects: {}",
            self.fps,
            self.quality,
            if self.animations_active { "ON" } else { "OFF" },
            if self.effects_active { "ON" } else { "OFF" }
        )
    }
}

/// Helper to integrate orchestrator with Cursive
pub fn setup_ux_orchestrator(siv: &mut Cursive) -> Arc<RwLock<UXOrchestrator>> {
    let size = siv.screen_size();
    let orchestrator = Arc::new(RwLock::new(UXOrchestrator::new(size)));

    // Initialize
    if let Ok(mut ux) = orchestrator.write() {
        ux.initialize(siv);
    }

    // Set as user data
    siv.set_user_data(orchestrator.clone());

    // Add update callback
    let orch_clone = orchestrator.clone();
    siv.add_global_callback(cursive::event::Event::Refresh, move |s| {
        if let Ok(mut ux) = orch_clone.write() {
            ux.update(s);
        }
    });

    // Add resize callback
    let orch_clone = orchestrator.clone();
    siv.add_global_callback(cursive::event::Event::WindowResize, move |s| {
        let new_size = s.screen_size();
        if let Ok(mut ux) = orch_clone.write() {
            ux.on_resize(s, new_size);
        }
    });

    orchestrator
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_orchestrator_creation() {
        let ux = UXOrchestrator::new(Vec2::new(80, 24));
        assert!(ux.animations_enabled);
        assert!(ux.effects_enabled);
    }

    #[test]
    fn test_performance_stats() {
        let stats = PerformanceStats {
            fps: 60.0,
            quality: QualityLevel::High,
            animations_active: true,
            effects_active: true,
        };

        let display = stats.display();
        assert!(display.contains("60"));
        assert!(display.contains("High"));
    }
}
