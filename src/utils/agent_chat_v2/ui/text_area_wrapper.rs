//! Custom TextArea wrapper that intercepts Ctrl+Enter before TextArea consumes it

use cursive::event::{Event, EventResult};
use cursive::view::ViewWrapper;
use cursive::views::TextArea;
use cursive::View;

/// Wraps TextArea to intercept Ctrl+Enter and pass it to parent instead of consuming it
pub struct SendableTextArea {
    inner: TextArea,
}

impl SendableTextArea {
    pub fn new() -> Self {
        SendableTextArea {
            inner: TextArea::new().content(""),
        }
    }

    pub fn get_content(&self) -> &str {
        self.inner.get_content()
    }

    pub fn set_content(&mut self, content: impl Into<String>) {
        self.inner.set_content(content);
    }
}

impl ViewWrapper for SendableTextArea {
    cursive::wrap_impl!(self.inner: TextArea);

    fn wrap_on_event(&mut self, event: Event) -> EventResult {
        // Log ALL events for debugging
        match &event {
            Event::CtrlChar(c) => {
                log::info!("📥 TextArea wrapper received: Ctrl+{}", c);
            }
            Event::Char(c) => {
                log::debug!("📥 TextArea wrapper received: Char({})", c);
            }
            _ => {
                log::debug!("📥 TextArea wrapper received: {:?}", event);
            }
        }

        // Intercept Ctrl+M (which is what Ctrl+Enter becomes in most terminals)
        if let Event::CtrlChar('m') = event {
            log::info!("🎯 INTERCEPTED: Ctrl+M at TextArea level - passing to global handler!");
            // Return Ignored so the event propagates to parent/global handlers
            return EventResult::Ignored;
        }

        // Intercept Ctrl+J as well
        if let Event::CtrlChar('j') = event {
            log::info!("🎯 INTERCEPTED: Ctrl+J at TextArea level - passing to global handler!");
            return EventResult::Ignored;
        }

        // Intercept Shift+Enter to allow it as send shortcut
        if let Event::Shift(cursive::event::Key::Enter) = event {
            log::info!("🎯 INTERCEPTED: Shift+Enter at TextArea level - passing to global handler for sending!");
            return EventResult::Ignored;
        }

        // Note: We don't intercept Ctrl+E because it's used by cursive for "end of line"

        // For all other events, let TextArea handle them normally
        let result = self.inner.on_event(event);
        if matches!(result, EventResult::Consumed(_)) {
            log::debug!("📥 TextArea consumed the event");
        }
        result
    }
}

impl Default for SendableTextArea {
    fn default() -> Self {
        Self::new()
    }
}
