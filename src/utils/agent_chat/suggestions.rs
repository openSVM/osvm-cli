//! Real-time suggestion system for the chat interface

use serde::{Serialize, Deserialize};

/// Real-time suggestion with fuzzy matching metadata
#[derive(Debug, Clone)]
pub struct RealtimeSuggestion {
    pub text: String,
    pub description: String,
    pub category: String,
    pub score: f32,
    pub matched_indices: Vec<usize>,
}

impl RealtimeSuggestion {
    /// Create a new suggestion
    pub fn new(text: String, description: String, category: String) -> Self {
        Self {
            text,
            description,
            category,
            score: 1.0,
            matched_indices: Vec::new(),
        }
    }

    /// Create with score and matched indices
    pub fn with_score(mut self, score: f32, matched_indices: Vec<usize>) -> Self {
        self.score = score;
        self.matched_indices = matched_indices;
        self
    }
}