//! Shared Annotations System
//!
//! Allows investigators to add notes and annotations to wallets, transactions,
//! and other blockchain entities that are visible to all session participants.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tokio::sync::RwLock;
use uuid::Uuid;

use super::session::ParticipantColor;

/// Type of annotation target
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AnnotationType {
    /// Annotation on a wallet address
    Wallet(String),
    /// Annotation on a transaction signature
    Transaction(String),
    /// Annotation on a token mint address
    Token(String),
    /// Annotation on a program ID
    Program(String),
    /// Annotation on a specific screen coordinate (temporary highlight)
    ScreenPosition { x: u16, y: u16, pane: usize },
    /// General session note (not tied to specific entity)
    Note,
}

impl std::fmt::Display for AnnotationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnnotationType::Wallet(addr) => write!(f, "Wallet: {}", &addr[..8]),
            AnnotationType::Transaction(sig) => write!(f, "Tx: {}", &sig[..8]),
            AnnotationType::Token(mint) => write!(f, "Token: {}", &mint[..8]),
            AnnotationType::Program(id) => write!(f, "Program: {}", &id[..8]),
            AnnotationType::ScreenPosition { x, y, pane } => {
                write!(f, "Screen: ({}, {}) pane {}", x, y, pane)
            }
            AnnotationType::Note => write!(f, "Note"),
        }
    }
}

/// Severity/importance level of annotation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AnnotationSeverity {
    /// Informational note
    Info,
    /// Important finding
    Important,
    /// Warning/caution
    Warning,
    /// Critical/suspicious activity
    Critical,
    /// Question or needs investigation
    Question,
}

impl AnnotationSeverity {
    /// Get display emoji
    pub fn emoji(&self) -> &'static str {
        match self {
            AnnotationSeverity::Info => "ℹ️",
            AnnotationSeverity::Important => "⭐",
            AnnotationSeverity::Warning => "⚠️",
            AnnotationSeverity::Critical => "🚨",
            AnnotationSeverity::Question => "❓",
        }
    }

    /// Get color for display
    pub fn color(&self) -> (u8, u8, u8) {
        match self {
            AnnotationSeverity::Info => (100, 149, 237), // Cornflower Blue
            AnnotationSeverity::Important => (255, 215, 0), // Gold
            AnnotationSeverity::Warning => (255, 165, 0), // Orange
            AnnotationSeverity::Critical => (255, 0, 0), // Red
            AnnotationSeverity::Question => (186, 85, 211), // Medium Orchid
        }
    }
}

/// A single annotation/note
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Annotation {
    /// Unique annotation ID
    pub id: String,
    /// What this annotation is attached to
    pub target: AnnotationType,
    /// The annotation text
    pub text: String,
    /// Severity level
    pub severity: AnnotationSeverity,
    /// Who created this annotation
    pub author_id: String,
    /// Author's display name
    pub author_name: String,
    /// Author's color
    pub author_color: ParticipantColor,
    /// When it was created
    pub created_at: DateTime<Utc>,
    /// When it was last updated
    pub updated_at: DateTime<Utc>,
    /// Tags for categorization
    pub tags: Vec<String>,
    /// Whether this annotation is pinned (stays visible)
    pub pinned: bool,
    /// Replies/comments on this annotation
    pub replies: Vec<AnnotationReply>,
}

impl Annotation {
    /// Create a new annotation
    pub fn new(
        target: AnnotationType,
        text: String,
        severity: AnnotationSeverity,
        author_id: String,
        author_name: String,
        author_color: ParticipantColor,
    ) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            target,
            text,
            severity,
            author_id,
            author_name,
            author_color,
            created_at: Utc::now(),
            updated_at: Utc::now(),
            tags: Vec::new(),
            pinned: false,
            replies: Vec::new(),
        }
    }

    /// Add a tag
    pub fn with_tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.push(tag.into());
        self
    }

    /// Set pinned state
    pub fn with_pinned(mut self, pinned: bool) -> Self {
        self.pinned = pinned;
        self
    }

    /// Add a reply
    pub fn add_reply(&mut self, reply: AnnotationReply) {
        self.replies.push(reply);
        self.updated_at = Utc::now();
    }

    /// Format for display
    pub fn display(&self) -> String {
        format!(
            "{} {} [{}]: {}",
            self.severity.emoji(),
            self.author_name,
            self.target,
            self.text
        )
    }
}

/// A reply to an annotation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnnotationReply {
    pub id: String,
    pub text: String,
    pub author_id: String,
    pub author_name: String,
    pub created_at: DateTime<Utc>,
}

impl AnnotationReply {
    pub fn new(text: String, author_id: String, author_name: String) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            text,
            author_id,
            author_name,
            created_at: Utc::now(),
        }
    }
}

/// Store for managing annotations in a session
pub struct AnnotationStore {
    /// All annotations by ID
    annotations: RwLock<HashMap<String, Annotation>>,
    /// Index by target for quick lookup
    by_target: RwLock<HashMap<AnnotationType, Vec<String>>>,
}

impl AnnotationStore {
    pub fn new() -> Self {
        Self {
            annotations: RwLock::new(HashMap::new()),
            by_target: RwLock::new(HashMap::new()),
        }
    }

    /// Add a new annotation
    pub async fn add(&self, annotation: Annotation) -> String {
        let id = annotation.id.clone();
        let target = annotation.target.clone();

        let mut annotations = self.annotations.write().await;
        let mut by_target = self.by_target.write().await;

        annotations.insert(id.clone(), annotation);
        by_target.entry(target).or_default().push(id.clone());

        id
    }

    /// Get an annotation by ID
    pub async fn get(&self, id: &str) -> Option<Annotation> {
        let annotations = self.annotations.read().await;
        annotations.get(id).cloned()
    }

    /// Get all annotations for a target
    pub async fn get_by_target(&self, target: &AnnotationType) -> Vec<Annotation> {
        let by_target = self.by_target.read().await;
        let annotations = self.annotations.read().await;

        by_target
            .get(target)
            .map(|ids| {
                ids.iter()
                    .filter_map(|id| annotations.get(id).cloned())
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get all annotations for a wallet address
    pub async fn get_for_wallet(&self, address: &str) -> Vec<Annotation> {
        self.get_by_target(&AnnotationType::Wallet(address.to_string()))
            .await
    }

    /// Get all annotations for a transaction
    pub async fn get_for_transaction(&self, signature: &str) -> Vec<Annotation> {
        self.get_by_target(&AnnotationType::Transaction(signature.to_string()))
            .await
    }

    /// Get all annotations
    pub async fn get_all(&self) -> Vec<Annotation> {
        let annotations = self.annotations.read().await;
        annotations.values().cloned().collect()
    }

    /// Get pinned annotations
    pub async fn get_pinned(&self) -> Vec<Annotation> {
        let annotations = self.annotations.read().await;
        annotations.values().filter(|a| a.pinned).cloned().collect()
    }

    /// Get annotations by severity
    pub async fn get_by_severity(&self, severity: AnnotationSeverity) -> Vec<Annotation> {
        let annotations = self.annotations.read().await;
        annotations
            .values()
            .filter(|a| a.severity == severity)
            .cloned()
            .collect()
    }

    /// Get annotations by author
    pub async fn get_by_author(&self, author_id: &str) -> Vec<Annotation> {
        let annotations = self.annotations.read().await;
        annotations
            .values()
            .filter(|a| a.author_id == author_id)
            .cloned()
            .collect()
    }

    /// Update an annotation
    pub async fn update(&self, id: &str, text: String) -> bool {
        let mut annotations = self.annotations.write().await;
        if let Some(annotation) = annotations.get_mut(id) {
            annotation.text = text;
            annotation.updated_at = Utc::now();
            true
        } else {
            false
        }
    }

    /// Toggle pinned state
    pub async fn toggle_pin(&self, id: &str) -> bool {
        let mut annotations = self.annotations.write().await;
        if let Some(annotation) = annotations.get_mut(id) {
            annotation.pinned = !annotation.pinned;
            annotation.updated_at = Utc::now();
            true
        } else {
            false
        }
    }

    /// Add a reply to an annotation
    pub async fn add_reply(&self, annotation_id: &str, reply: AnnotationReply) -> bool {
        let mut annotations = self.annotations.write().await;
        if let Some(annotation) = annotations.get_mut(annotation_id) {
            annotation.add_reply(reply);
            true
        } else {
            false
        }
    }

    /// Delete an annotation
    pub async fn delete(&self, id: &str) -> Option<Annotation> {
        let mut annotations = self.annotations.write().await;
        let annotation = annotations.remove(id)?;

        let mut by_target = self.by_target.write().await;
        if let Some(ids) = by_target.get_mut(&annotation.target) {
            ids.retain(|i| i != id);
        }

        Some(annotation)
    }

    /// Search annotations by text
    pub async fn search(&self, query: &str) -> Vec<Annotation> {
        let query_lower = query.to_lowercase();
        let annotations = self.annotations.read().await;

        annotations
            .values()
            .filter(|a| {
                a.text.to_lowercase().contains(&query_lower)
                    || a.tags
                        .iter()
                        .any(|t| t.to_lowercase().contains(&query_lower))
            })
            .cloned()
            .collect()
    }

    /// Get annotation statistics
    pub async fn stats(&self) -> AnnotationStats {
        let annotations = self.annotations.read().await;

        let mut by_severity = HashMap::new();
        let mut by_type = HashMap::new();
        let mut pinned_count = 0;

        for annotation in annotations.values() {
            *by_severity.entry(annotation.severity).or_insert(0) += 1;

            let type_key = match &annotation.target {
                AnnotationType::Wallet(_) => "wallet",
                AnnotationType::Transaction(_) => "transaction",
                AnnotationType::Token(_) => "token",
                AnnotationType::Program(_) => "program",
                AnnotationType::ScreenPosition { .. } => "screen",
                AnnotationType::Note => "note",
            };
            *by_type.entry(type_key.to_string()).or_insert(0) += 1;

            if annotation.pinned {
                pinned_count += 1;
            }
        }

        AnnotationStats {
            total: annotations.len(),
            pinned: pinned_count,
            by_severity,
            by_type,
        }
    }

    /// Export all annotations as JSON
    pub async fn export_json(&self) -> Result<String, serde_json::Error> {
        let annotations = self.annotations.read().await;
        let all: Vec<_> = annotations.values().collect();
        serde_json::to_string_pretty(&all)
    }

    /// Import annotations from JSON
    pub async fn import_json(&self, json: &str) -> Result<usize, serde_json::Error> {
        let imported: Vec<Annotation> = serde_json::from_str(json)?;
        let count = imported.len();

        for annotation in imported {
            self.add(annotation).await;
        }

        Ok(count)
    }
}

impl Default for AnnotationStore {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about annotations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnnotationStats {
    pub total: usize,
    pub pinned: usize,
    pub by_severity: HashMap<AnnotationSeverity, usize>,
    pub by_type: HashMap<String, usize>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_annotation_type_display() {
        let wallet =
            AnnotationType::Wallet("5Q544fKrFoe6tsEbD7S8EmxGTJYAKtTVhAW5Q5pge4j1".to_string());
        let display = format!("{}", wallet);
        assert!(display.starts_with("Wallet: 5Q544fKr"));
    }

    #[test]
    fn test_annotation_creation() {
        let annotation = Annotation::new(
            AnnotationType::Wallet("test".to_string()),
            "Suspicious activity".to_string(),
            AnnotationSeverity::Warning,
            "user1".to_string(),
            "Alice".to_string(),
            ParticipantColor::from_index(0),
        )
        .with_tag("suspicious");

        assert_eq!(annotation.tags.len(), 1);
        assert_eq!(annotation.severity, AnnotationSeverity::Warning);
    }

    #[tokio::test]
    async fn test_annotation_store() {
        let store = AnnotationStore::new();

        let annotation = Annotation::new(
            AnnotationType::Wallet("wallet123".to_string()),
            "Test annotation".to_string(),
            AnnotationSeverity::Info,
            "user1".to_string(),
            "Alice".to_string(),
            ParticipantColor::from_index(0),
        );

        let id = store.add(annotation).await;

        let retrieved = store.get(&id).await;
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().text, "Test annotation");
    }

    #[tokio::test]
    async fn test_get_by_wallet() {
        let store = AnnotationStore::new();

        let annotation1 = Annotation::new(
            AnnotationType::Wallet("wallet123".to_string()),
            "First annotation".to_string(),
            AnnotationSeverity::Info,
            "user1".to_string(),
            "Alice".to_string(),
            ParticipantColor::from_index(0),
        );

        let annotation2 = Annotation::new(
            AnnotationType::Wallet("wallet123".to_string()),
            "Second annotation".to_string(),
            AnnotationSeverity::Warning,
            "user2".to_string(),
            "Bob".to_string(),
            ParticipantColor::from_index(1),
        );

        store.add(annotation1).await;
        store.add(annotation2).await;

        let wallet_annotations = store.get_for_wallet("wallet123").await;
        assert_eq!(wallet_annotations.len(), 2);
    }

    #[tokio::test]
    async fn test_annotation_search() {
        let store = AnnotationStore::new();

        let annotation = Annotation::new(
            AnnotationType::Note,
            "This is a suspicious exchange wallet".to_string(),
            AnnotationSeverity::Warning,
            "user1".to_string(),
            "Alice".to_string(),
            ParticipantColor::from_index(0),
        );

        store.add(annotation).await;

        let results = store.search("exchange").await;
        assert_eq!(results.len(), 1);

        let no_results = store.search("token").await;
        assert_eq!(no_results.len(), 0);
    }
}
