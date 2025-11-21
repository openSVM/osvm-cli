// Event handling for OSVM TUI
use crossterm::event::{self, Event, KeyCode};
use std::time::Duration;

pub enum AppEvent {
    Input(KeyCode),
    Tick,
    Resize(u16, u16),
}

pub struct EventHandler;

impl EventHandler {
    pub fn new() -> Self {
        Self
    }

    pub fn next(&self) -> anyhow::Result<Option<AppEvent>> {
        if event::poll(Duration::from_millis(100))? {
            match event::read()? {
                Event::Key(key) => Ok(Some(AppEvent::Input(key.code))),
                Event::Resize(w, h) => Ok(Some(AppEvent::Resize(w, h))),
                _ => Ok(None),
            }
        } else {
            Ok(Some(AppEvent::Tick))
        }
    }
}
