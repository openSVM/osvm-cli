//! Event-driven state updates for the chat UI
//!
//! This module replaces polling-based updates with channel-based event-driven updates,
//! reducing lock contention and improving responsiveness.

use std::sync::Arc;
use tokio::sync::broadcast;

/// Events that can trigger UI updates
#[derive(Clone, Debug)]
pub enum ChatEvent {
    /// A new message was added to a session
    MessageAdded {
        session_id: uuid::Uuid,
        message_index: usize,
    },
    /// Session list changed (created, deleted, renamed)
    SessionsChanged,
    /// Active session changed
    ActiveSessionChanged { session_id: uuid::Uuid },
    /// Agent state changed
    AgentStateChanged {
        session_id: uuid::Uuid,
        state: super::types::AgentState,
    },
    /// MCP tools list updated
    ToolsRefreshed { tool_count: usize },
    /// Suggestions updated
    SuggestionsUpdated { count: usize },
    /// Theme changed
    ThemeChanged { theme_name: String },
    /// Recording state changed
    RecordingStateChanged {
        session_id: uuid::Uuid,
        recording: bool,
    },
    /// Force full UI refresh
    ForceRefresh,
}

/// Event bus for broadcasting state changes
///
/// Uses tokio broadcast channel for efficient one-to-many distribution.
/// The bus maintains a buffer of recent events for late subscribers.
#[derive(Clone)]
pub struct ChatEventBus {
    sender: broadcast::Sender<ChatEvent>,
    /// Capacity of the event buffer
    capacity: usize,
}

impl ChatEventBus {
    /// Create a new event bus with specified buffer capacity
    pub fn new(capacity: usize) -> Self {
        let (sender, _) = broadcast::channel(capacity);
        Self { sender, capacity }
    }

    /// Create with default capacity (256 events)
    pub fn default() -> Self {
        Self::new(256)
    }

    /// Publish an event to all subscribers
    ///
    /// Returns the number of subscribers that received the event.
    /// If there are no subscribers, the event is dropped.
    pub fn publish(&self, event: ChatEvent) -> usize {
        match self.sender.send(event) {
            Ok(count) => count,
            Err(_) => 0, // No receivers
        }
    }

    /// Subscribe to events
    ///
    /// Returns a receiver that will get all future events.
    /// Late subscribers may miss events that occurred before subscription.
    pub fn subscribe(&self) -> broadcast::Receiver<ChatEvent> {
        self.sender.subscribe()
    }

    /// Get the number of active subscribers
    pub fn subscriber_count(&self) -> usize {
        self.sender.receiver_count()
    }

    /// Get the buffer capacity
    pub fn capacity(&self) -> usize {
        self.capacity
    }
}

impl Default for ChatEventBus {
    fn default() -> Self {
        Self::default()
    }
}

/// Helper trait for publishing common events
pub trait EventPublisher {
    fn event_bus(&self) -> &ChatEventBus;

    fn emit_message_added(&self, session_id: uuid::Uuid, message_index: usize) {
        self.event_bus().publish(ChatEvent::MessageAdded {
            session_id,
            message_index,
        });
    }

    fn emit_sessions_changed(&self) {
        self.event_bus().publish(ChatEvent::SessionsChanged);
    }

    fn emit_active_session_changed(&self, session_id: uuid::Uuid) {
        self.event_bus()
            .publish(ChatEvent::ActiveSessionChanged { session_id });
    }

    fn emit_agent_state_changed(&self, session_id: uuid::Uuid, state: super::types::AgentState) {
        self.event_bus()
            .publish(ChatEvent::AgentStateChanged { session_id, state });
    }

    fn emit_tools_refreshed(&self, tool_count: usize) {
        self.event_bus()
            .publish(ChatEvent::ToolsRefreshed { tool_count });
    }

    fn emit_suggestions_updated(&self, count: usize) {
        self.event_bus()
            .publish(ChatEvent::SuggestionsUpdated { count });
    }

    fn emit_theme_changed(&self, theme_name: String) {
        self.event_bus()
            .publish(ChatEvent::ThemeChanged { theme_name });
    }

    fn emit_recording_state_changed(&self, session_id: uuid::Uuid, recording: bool) {
        self.event_bus().publish(ChatEvent::RecordingStateChanged {
            session_id,
            recording,
        });
    }

    fn emit_force_refresh(&self) {
        self.event_bus().publish(ChatEvent::ForceRefresh);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_event_bus_publish_subscribe() {
        let bus = ChatEventBus::new(16);
        let mut receiver = bus.subscribe();

        bus.publish(ChatEvent::SessionsChanged);

        let event = receiver.recv().await.unwrap();
        assert!(matches!(event, ChatEvent::SessionsChanged));
    }

    #[tokio::test]
    async fn test_event_bus_multiple_subscribers() {
        let bus = ChatEventBus::new(16);
        let mut rx1 = bus.subscribe();
        let mut rx2 = bus.subscribe();

        let count = bus.publish(ChatEvent::ForceRefresh);
        assert_eq!(count, 2);

        assert!(matches!(rx1.recv().await.unwrap(), ChatEvent::ForceRefresh));
        assert!(matches!(rx2.recv().await.unwrap(), ChatEvent::ForceRefresh));
    }

    #[test]
    fn test_event_bus_no_subscribers() {
        let bus = ChatEventBus::new(16);
        let count = bus.publish(ChatEvent::SessionsChanged);
        assert_eq!(count, 0);
    }

    #[test]
    fn test_subscriber_count() {
        let bus = ChatEventBus::new(16);
        assert_eq!(bus.subscriber_count(), 0);

        let _rx1 = bus.subscribe();
        assert_eq!(bus.subscriber_count(), 1);

        let _rx2 = bus.subscribe();
        assert_eq!(bus.subscriber_count(), 2);
    }
}
