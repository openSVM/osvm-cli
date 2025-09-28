//! Fuzzy matching engine for intelligent suggestion filtering

use super::suggestions::RealtimeSuggestion;

/// Fuzzy matching engine using modified Levenshtein distance
pub struct FuzzyMatcher {
    pub threshold: f32,
}

impl FuzzyMatcher {
    /// Create a new fuzzy matcher with given threshold
    pub fn new(threshold: f32) -> Self {
        Self { threshold }
    }

    /// Calculate fuzzy match score between pattern and text
    pub fn score(&self, pattern: &str, text: &str) -> f32 {
        if pattern.is_empty() {
            return 1.0;
        }

        let pattern = pattern.to_lowercase();
        let text = text.to_lowercase();

        // Exact match or substring gets highest score
        if text.contains(&pattern) {
            return 1.0 - (pattern.len() as f32 / text.len() as f32) * 0.1;
        }

        // Character sequence matching
        let mut score = 0.0;
        let mut last_index = 0;
        let mut consecutive_bonus = 0.0;

        for ch in pattern.chars() {
            if let Some(index) = text[last_index..].find(ch) {
                // Use saturating_add to prevent overflow
                let actual_index = last_index.saturating_add(index);

                // Guard against index going out of bounds
                if actual_index >= text.len() {
                    break;
                }

                // Bonus for consecutive characters
                if actual_index == last_index {
                    consecutive_bonus += 0.1;
                } else {
                    consecutive_bonus = 0.0;
                }

                // Bonus for start of word (with bounds check)
                let start_bonus = if actual_index == 0 ||
                    (actual_index > 0 && text.chars().nth(actual_index - 1).map_or(false, |c| !c.is_alphanumeric())) {
                    0.2
                } else {
                    0.0
                };

                score += 1.0 + consecutive_bonus + start_bonus;
                // Use saturating_add to prevent overflow when moving to next position
                last_index = actual_index.saturating_add(1);
            } else {
                return 0.0; // Character not found
            }
        }

        // Normalize score
        let max_score = pattern.len() as f32 * 1.3;
        let normalized = score / max_score;

        // Penalty for length difference
        let length_penalty = (text.len() as f32 - pattern.len() as f32).abs() / text.len() as f32 * 0.2;

        (normalized - length_penalty).max(0.0)
    }

    /// Get character indices that match the pattern
    pub fn get_match_indices(&self, pattern: &str, text: &str) -> Vec<usize> {
        let mut indices = Vec::new();
        let pattern = pattern.to_lowercase();
        let text = text.to_lowercase();
        let mut last_index = 0;

        for ch in pattern.chars() {
            if let Some(index) = text[last_index..].find(ch) {
                indices.push(last_index + index);
                last_index += index + 1;
            }
        }

        indices
    }

    /// Filter and score a list of candidates
    pub fn filter_suggestions(&self, pattern: &str, candidates: &[(String, String, String)]) -> Vec<RealtimeSuggestion> {
        let mut suggestions: Vec<RealtimeSuggestion> = candidates
            .iter()
            .filter_map(|(text, description, category)| {
                let score = self.score(pattern, text);
                if score >= self.threshold {
                    Some(RealtimeSuggestion::new(
                        text.clone(),
                        description.clone(),
                        category.clone(),
                    ).with_score(score, self.get_match_indices(pattern, text)))
                } else {
                    None
                }
            })
            .collect();

        // Sort by score (highest first)
        suggestions.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));
        suggestions
    }
}