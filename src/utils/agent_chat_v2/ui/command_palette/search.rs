//! Advanced search system for command palette - makes finding anything instant

use super::{Command, CommandCategory};
use fuzzy_matcher::FuzzyMatcher;
use fuzzy_matcher::skim::SkimMatcherV2;

/// Smart search that learns from user behavior
pub struct SmartSearch {
    matcher: SkimMatcherV2,
    search_history: Vec<SearchEntry>,
    abbreviations: std::collections::HashMap<String, String>,
    frequency_map: std::collections::HashMap<String, usize>,
}

#[derive(Clone)]
struct SearchEntry {
    query: String,
    selected_command: String,
    timestamp: std::time::Instant,
}

impl SmartSearch {
    pub fn new() -> Self {
        let mut search = Self {
            matcher: SkimMatcherV2::default(),
            search_history: Vec::new(),
            abbreviations: std::collections::HashMap::new(),
            frequency_map: std::collections::HashMap::new(),
        };

        // Pre-populate common abbreviations
        search.add_abbreviations();
        search
    }

    /// Add common abbreviations users expect
    fn add_abbreviations(&mut self) {
        // Action abbreviations
        self.abbreviations.insert("b".to_string(), "balance".to_string());
        self.abbreviations.insert("s".to_string(), "send".to_string());
        self.abbreviations.insert("tx".to_string(), "transaction".to_string());
        self.abbreviations.insert("hist".to_string(), "history".to_string());

        // UI abbreviations
        self.abbreviations.insert("cls".to_string(), "clear".to_string());
        self.abbreviations.insert("exp".to_string(), "export".to_string());
        self.abbreviations.insert("rec".to_string(), "record".to_string());
        self.abbreviations.insert("ss".to_string(), "screenshot".to_string());

        // Navigation abbreviations
        self.abbreviations.insert("gt".to_string(), "go to".to_string());
        self.abbreviations.insert("sw".to_string(), "switch".to_string());

        // Theme abbreviations
        self.abbreviations.insert("th".to_string(), "theme".to_string());
        self.abbreviations.insert("vs".to_string(), "vscode".to_string());
        self.abbreviations.insert("cp".to_string(), "cyberpunk".to_string());
    }

    /// Expand abbreviations in query
    pub fn expand_query(&self, query: &str) -> String {
        let mut expanded = query.to_string();

        // Check if entire query is an abbreviation
        if let Some(full) = self.abbreviations.get(query) {
            return full.clone();
        }

        // Expand abbreviations within query
        for (abbr, full) in &self.abbreviations {
            if query.contains(abbr) {
                expanded = expanded.replace(abbr, full);
            }
        }

        expanded
    }

    /// Score a command based on multiple factors
    pub fn score_command(&self, command: &Command, query: &str) -> Option<SearchScore> {
        let expanded_query = self.expand_query(query);

        // Build searchable text
        let search_text = format!(
            "{} {} {} {}",
            command.label,
            command.description.as_ref().unwrap_or(&String::new()),
            command.keywords.join(" "),
            command.shortcut.as_ref().unwrap_or(&String::new())
        );

        // Get base fuzzy match score
        let fuzzy_score = self.matcher.fuzzy_match(&search_text, &expanded_query)?;

        // Calculate bonus scores
        let mut total_score = fuzzy_score;

        // Frequency bonus - commands used more often score higher
        if let Some(frequency) = self.frequency_map.get(&command.id) {
            total_score += (*frequency as i64) * 10;
        }

        // Exact match bonus
        if command.label.to_lowercase().starts_with(&query.to_lowercase()) {
            total_score += 100;
        }

        // Shortcut match bonus
        if let Some(shortcut) = &command.shortcut {
            if shortcut.to_lowercase().contains(&query.to_lowercase()) {
                total_score += 50;
            }
        }

        // Category preference (actions are generally more important)
        match command.category {
            CommandCategory::Action => total_score += 20,
            CommandCategory::Recent => total_score += 30,
            _ => {}
        }

        // Recent usage bonus
        if self.was_recently_used(&command.id) {
            total_score += 40;
        }

        Some(SearchScore {
            command_id: command.id.clone(),
            score: total_score,
            matched_indices: self.get_match_indices(&search_text, &expanded_query),
        })
    }

    /// Check if command was recently used
    fn was_recently_used(&self, command_id: &str) -> bool {
        let recent_threshold = std::time::Duration::from_secs(300); // 5 minutes

        self.search_history.iter().any(|entry| {
            entry.selected_command == command_id &&
            entry.timestamp.elapsed() < recent_threshold
        })
    }

    /// Get character indices that matched
    fn get_match_indices(&self, text: &str, query: &str) -> Vec<usize> {
        // Simplified version - would use fuzzy_matcher's indices in real implementation
        let mut indices = Vec::new();
        let query_lower = query.to_lowercase();
        let text_lower = text.to_lowercase();

        let mut query_chars = query_lower.chars();
        let mut current_query_char = query_chars.next();

        for (i, text_char) in text_lower.chars().enumerate() {
            if let Some(qc) = current_query_char {
                if text_char == qc {
                    indices.push(i);
                    current_query_char = query_chars.next();
                }
            }
        }

        indices
    }

    /// Record a search and selection for learning
    pub fn record_selection(&mut self, query: String, command_id: String) {
        // Add to history
        self.search_history.push(SearchEntry {
            query,
            selected_command: command_id.clone(),
            timestamp: std::time::Instant::now(),
        });

        // Update frequency map
        *self.frequency_map.entry(command_id).or_insert(0) += 1;

        // Keep history size reasonable
        if self.search_history.len() > 1000 {
            self.search_history.remove(0);
        }
    }

    /// Get suggested queries based on partial input
    pub fn suggest_queries(&self, partial: &str) -> Vec<String> {
        let mut suggestions = Vec::new();

        // Suggest from history
        for entry in self.search_history.iter().rev().take(20) {
            if entry.query.starts_with(partial) {
                if !suggestions.contains(&entry.query) {
                    suggestions.push(entry.query.clone());
                }
            }
        }

        // Suggest abbreviation expansions
        for (abbr, full) in &self.abbreviations {
            if abbr.starts_with(partial) {
                suggestions.push(full.clone());
            }
        }

        suggestions.truncate(5);
        suggestions
    }
}

/// Score for a search result
pub struct SearchScore {
    pub command_id: String,
    pub score: i64,
    pub matched_indices: Vec<usize>,
}

/// Search result with highlighted matches
pub struct SearchResult {
    pub command: Command,
    pub score: i64,
    pub highlighted_label: String,
    pub highlighted_description: String,
}

impl SearchResult {
    /// Create highlighted text with matched characters emphasized
    pub fn highlight_text(text: &str, indices: &[usize]) -> String {
        let mut result = String::new();

        for (i, ch) in text.chars().enumerate() {
            if indices.contains(&i) {
                // Highlight matched characters
                result.push_str(&format!("[{}]", ch));
            } else {
                result.push(ch);
            }
        }

        result
    }
}

/// Intelligent query parser that understands user intent
pub struct QueryParser;

impl QueryParser {
    /// Parse special query patterns
    pub fn parse(query: &str) -> QueryIntent {
        // Check for special prefixes
        if query.starts_with('>') {
            let rest: String = query.chars().skip(1).collect();
            return QueryIntent::ActionOnly(rest);
        }

        if query.starts_with('@') {
            let rest: String = query.chars().skip(1).collect();
            return QueryIntent::SessionOnly(rest);
        }

        if query.starts_with('#') {
            let rest: String = query.chars().skip(1).collect();
            return QueryIntent::ThemeOnly(rest);
        }

        if query.starts_with('?') {
            let rest: String = query.chars().skip(1).collect();
            return QueryIntent::HelpOnly(rest);
        }

        if query.starts_with('/') {
            let rest: String = query.chars().skip(1).collect();
            return QueryIntent::FileOnly(rest);
        }

        // Check for natural language patterns
        if query.starts_with("how to") || query.starts_with("how do i") {
            return QueryIntent::Help(query.to_string());
        }

        if query.contains("send") || query.contains("transfer") {
            return QueryIntent::Transaction(query.to_string());
        }

        QueryIntent::General(query.to_string())
    }
}

/// User's search intent
pub enum QueryIntent {
    General(String),
    ActionOnly(String),
    SessionOnly(String),
    ThemeOnly(String),
    FileOnly(String),
    HelpOnly(String),
    Help(String),
    Transaction(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_abbreviation_expansion() {
        let search = SmartSearch::new();

        assert_eq!(search.expand_query("b"), "balance");
        assert_eq!(search.expand_query("tx"), "transaction");
        assert_eq!(search.expand_query("gt"), "go to");
    }

    #[test]
    fn test_query_parser() {
        match QueryParser::parse(">send") {
            QueryIntent::ActionOnly(q) => assert_eq!(q, "send"),
            _ => panic!("Expected ActionOnly"),
        }

        match QueryParser::parse("@main") {
            QueryIntent::SessionOnly(q) => assert_eq!(q, "main"),
            _ => panic!("Expected SessionOnly"),
        }

        match QueryParser::parse("how to stake") {
            QueryIntent::Help(q) => assert_eq!(q, "how to stake"),
            _ => panic!("Expected Help"),
        }
    }
}