//! User Presence and Cursor Tracking
//!
//! Tracks which users are connected, their activity, and cursor positions.

use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

use super::session::ParticipantColor;

/// Position of a user's cursor in the terminal
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct CursorPosition {
    /// X coordinate (column, 0-indexed)
    pub x: u16,
    /// Y coordinate (row, 0-indexed)
    pub y: u16,
    /// Which pane the cursor is in
    pub pane: usize,
    /// Timestamp of last update
    #[serde(with = "chrono::serde::ts_milliseconds")]
    pub updated_at: DateTime<Utc>,
}

impl CursorPosition {
    pub fn new(x: u16, y: u16, pane: usize) -> Self {
        Self {
            x,
            y,
            pane,
            updated_at: Utc::now(),
        }
    }
}

/// Activity status of a user
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ActivityStatus {
    /// Actively interacting
    Active,
    /// Connected but no recent activity
    Idle,
    /// Typing or entering input
    Typing,
    /// Viewing/scrolling
    Viewing,
    /// Away (connected but explicitly set as away)
    Away,
}

/// Presence information for a single user
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserPresence {
    /// User/participant ID
    pub user_id: String,
    /// Display name
    pub display_name: String,
    /// Current cursor position
    pub cursor: Option<CursorPosition>,
    /// Current activity status
    pub status: ActivityStatus,
    /// Color for this user's indicators
    pub color: ParticipantColor,
    /// Last activity timestamp
    pub last_activity: DateTime<Utc>,
    /// Currently focused pane
    pub focused_pane: Option<usize>,
    /// Currently selected text range (for highlighting what user is looking at)
    pub selection: Option<TextSelection>,
}

/// A selected range of text
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextSelection {
    pub start_x: u16,
    pub start_y: u16,
    pub end_x: u16,
    pub end_y: u16,
    pub pane: usize,
}

impl UserPresence {
    pub fn new(user_id: String, display_name: String, color: ParticipantColor) -> Self {
        Self {
            user_id,
            display_name,
            cursor: None,
            status: ActivityStatus::Active,
            color,
            last_activity: Utc::now(),
            focused_pane: Some(0),
            selection: None,
        }
    }

    /// Update cursor position
    pub fn update_cursor(&mut self, x: u16, y: u16, pane: usize) {
        self.cursor = Some(CursorPosition::new(x, y, pane));
        self.focused_pane = Some(pane);
        self.last_activity = Utc::now();
        self.status = ActivityStatus::Active;
    }

    /// Set activity status
    pub fn set_status(&mut self, status: ActivityStatus) {
        self.status = status;
        self.last_activity = Utc::now();
    }

    /// Check if user is idle (no activity for 30 seconds)
    pub fn is_idle(&self) -> bool {
        Utc::now() - self.last_activity > Duration::seconds(30)
    }

    /// Check if user is stale (no activity for 5 minutes)
    pub fn is_stale(&self) -> bool {
        Utc::now() - self.last_activity > Duration::minutes(5)
    }
}

/// Manages presence for all users in a session
pub struct PresenceManager {
    /// User presence data
    presences: RwLock<HashMap<String, UserPresence>>,
    /// Idle timeout in seconds
    idle_timeout_secs: i64,
    /// Stale timeout in seconds (remove after this)
    stale_timeout_secs: i64,
}

impl PresenceManager {
    pub fn new() -> Self {
        Self {
            presences: RwLock::new(HashMap::new()),
            idle_timeout_secs: 30,
            stale_timeout_secs: 300,
        }
    }

    /// Add or update a user's presence
    pub async fn upsert(&self, presence: UserPresence) {
        let mut presences = self.presences.write().await;
        presences.insert(presence.user_id.clone(), presence);
    }

    /// Get a user's presence
    pub async fn get(&self, user_id: &str) -> Option<UserPresence> {
        let presences = self.presences.read().await;
        presences.get(user_id).cloned()
    }

    /// Remove a user's presence
    pub async fn remove(&self, user_id: &str) -> Option<UserPresence> {
        let mut presences = self.presences.write().await;
        presences.remove(user_id)
    }

    /// Get all presences
    pub async fn get_all(&self) -> Vec<UserPresence> {
        let presences = self.presences.read().await;
        presences.values().cloned().collect()
    }

    /// Get active presences (non-stale)
    pub async fn get_active(&self) -> Vec<UserPresence> {
        let presences = self.presences.read().await;
        presences
            .values()
            .filter(|p| !p.is_stale())
            .cloned()
            .collect()
    }

    /// Update cursor position for a user
    pub async fn update_cursor(&self, user_id: &str, x: u16, y: u16, pane: usize) {
        let mut presences = self.presences.write().await;
        if let Some(presence) = presences.get_mut(user_id) {
            presence.update_cursor(x, y, pane);
        }
    }

    /// Update activity status for a user
    pub async fn update_status(&self, user_id: &str, status: ActivityStatus) {
        let mut presences = self.presences.write().await;
        if let Some(presence) = presences.get_mut(user_id) {
            presence.set_status(status);
        }
    }

    /// Update selection for a user
    pub async fn update_selection(&self, user_id: &str, selection: Option<TextSelection>) {
        let mut presences = self.presences.write().await;
        if let Some(presence) = presences.get_mut(user_id) {
            presence.selection = selection;
            presence.last_activity = Utc::now();
        }
    }

    /// Touch activity (update last_activity timestamp)
    pub async fn touch(&self, user_id: &str) {
        let mut presences = self.presences.write().await;
        if let Some(presence) = presences.get_mut(user_id) {
            presence.last_activity = Utc::now();
        }
    }

    /// Mark idle users based on timeout
    pub async fn mark_idle_users(&self) {
        let mut presences = self.presences.write().await;
        let threshold = Utc::now() - Duration::seconds(self.idle_timeout_secs);

        for presence in presences.values_mut() {
            if presence.last_activity < threshold && presence.status == ActivityStatus::Active {
                presence.status = ActivityStatus::Idle;
            }
        }
    }

    /// Remove stale users
    pub async fn cleanup_stale(&self) -> Vec<String> {
        let mut presences = self.presences.write().await;
        let threshold = Utc::now() - Duration::seconds(self.stale_timeout_secs);

        let stale_ids: Vec<String> = presences
            .iter()
            .filter(|(_, p)| p.last_activity < threshold)
            .map(|(id, _)| id.clone())
            .collect();

        for id in &stale_ids {
            presences.remove(id);
        }

        stale_ids
    }

    /// Get cursors for rendering (grouped by pane)
    pub async fn get_cursors_by_pane(
        &self,
    ) -> HashMap<usize, Vec<(String, CursorPosition, ParticipantColor)>> {
        let presences = self.presences.read().await;
        let mut by_pane: HashMap<usize, Vec<_>> = HashMap::new();

        for presence in presences.values() {
            if let Some(cursor) = &presence.cursor {
                by_pane.entry(cursor.pane).or_default().push((
                    presence.display_name.clone(),
                    *cursor,
                    presence.color,
                ));
            }
        }

        by_pane
    }

    /// Get a summary for display (e.g., "3 active, 1 idle")
    pub async fn summary(&self) -> PresenceSummary {
        let presences = self.presences.read().await;

        let mut active = 0;
        let mut idle = 0;
        let mut away = 0;
        let mut typing = 0;

        for presence in presences.values() {
            match presence.status {
                ActivityStatus::Active | ActivityStatus::Viewing => active += 1,
                ActivityStatus::Idle => idle += 1,
                ActivityStatus::Away => away += 1,
                ActivityStatus::Typing => typing += 1,
            }
        }

        PresenceSummary {
            total: presences.len(),
            active,
            idle,
            away,
            typing,
        }
    }
}

impl Default for PresenceManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Summary of presence states
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PresenceSummary {
    pub total: usize,
    pub active: usize,
    pub idle: usize,
    pub away: usize,
    pub typing: usize,
}

impl std::fmt::Display for PresenceSummary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.total == 0 {
            write!(f, "No participants")
        } else if self.total == 1 {
            write!(f, "1 participant")
        } else {
            let mut parts = vec![];
            if self.active > 0 {
                parts.push(format!("{} active", self.active));
            }
            if self.typing > 0 {
                parts.push(format!("{} typing", self.typing));
            }
            if self.idle > 0 {
                parts.push(format!("{} idle", self.idle));
            }
            if self.away > 0 {
                parts.push(format!("{} away", self.away));
            }
            write!(f, "{} participants ({})", self.total, parts.join(", "))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cursor_position() {
        let cursor = CursorPosition::new(10, 20, 0);
        assert_eq!(cursor.x, 10);
        assert_eq!(cursor.y, 20);
        assert_eq!(cursor.pane, 0);
    }

    #[test]
    fn test_user_presence_idle() {
        let mut presence = UserPresence::new(
            "user1".to_string(),
            "Alice".to_string(),
            ParticipantColor::from_index(0),
        );

        // Fresh presence is not idle
        assert!(!presence.is_idle());

        // Manually set last_activity to 60 seconds ago
        presence.last_activity = Utc::now() - Duration::seconds(60);
        assert!(presence.is_idle());
    }

    #[test]
    fn test_presence_summary_display() {
        let summary = PresenceSummary {
            total: 5,
            active: 3,
            idle: 1,
            away: 1,
            typing: 0,
        };

        let display = format!("{}", summary);
        assert!(display.contains("5 participants"));
        assert!(display.contains("3 active"));
        assert!(display.contains("1 idle"));
    }

    #[tokio::test]
    async fn test_presence_manager() {
        let manager = PresenceManager::new();

        let presence = UserPresence::new(
            "user1".to_string(),
            "Alice".to_string(),
            ParticipantColor::from_index(0),
        );

        manager.upsert(presence).await;

        let retrieved = manager.get("user1").await;
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().display_name, "Alice");

        let all = manager.get_all().await;
        assert_eq!(all.len(), 1);
    }

    #[tokio::test]
    async fn test_cursor_update() {
        let manager = PresenceManager::new();

        let presence = UserPresence::new(
            "user1".to_string(),
            "Alice".to_string(),
            ParticipantColor::from_index(0),
        );

        manager.upsert(presence).await;
        manager.update_cursor("user1", 50, 25, 1).await;

        let updated = manager.get("user1").await.unwrap();
        let cursor = updated.cursor.unwrap();
        assert_eq!(cursor.x, 50);
        assert_eq!(cursor.y, 25);
        assert_eq!(cursor.pane, 1);
    }
}
