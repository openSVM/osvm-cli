//! Diff generation for file edits
//!
//! Generates unified diff format for showing file changes before approval.

use similar::{ChangeTag, DiffTag, TextDiff};

/// Generate a unified diff between two strings
pub fn generate_diff(old: &str, new: &str, file_path: &str) -> String {
    let diff = TextDiff::from_lines(old, new);

    let mut output = String::new();
    output.push_str(&format!("--- a/{}\n", file_path));
    output.push_str(&format!("+++ b/{}\n", file_path));

    // Use the unified_diff method for simpler output
    for hunk in diff.unified_diff().context_radius(3).iter_hunks() {
        output.push_str(&hunk.to_string());
    }

    if output.ends_with(&format!("--- a/{}\n+++ b/{}\n", file_path, file_path)) {
        // No actual changes
        output = "No changes".to_string();
    }

    output
}

/// Generate a compact summary of changes
pub fn generate_change_summary(old: &str, new: &str) -> ChangeSummary {
    let diff = TextDiff::from_lines(old, new);

    let mut insertions = 0;
    let mut deletions = 0;
    let mut modifications = 0;

    for op in diff.ops() {
        match op.tag() {
            DiffTag::Insert => insertions += op.new_range().len(),
            DiffTag::Delete => deletions += op.old_range().len(),
            DiffTag::Replace => {
                let old_len = op.old_range().len();
                let new_len = op.new_range().len();
                modifications += old_len.max(new_len);
            }
            DiffTag::Equal => {}
        }
    }

    ChangeSummary {
        insertions,
        deletions,
        modifications,
    }
}

/// Summary of changes between two versions
#[derive(Debug, Clone)]
pub struct ChangeSummary {
    pub insertions: usize,
    pub deletions: usize,
    pub modifications: usize,
}

impl ChangeSummary {
    pub fn total_changes(&self) -> usize {
        self.insertions + self.deletions + self.modifications
    }

    pub fn to_string_compact(&self) -> String {
        format!(
            "+{} -{} ~{}",
            self.insertions, self.deletions, self.modifications
        )
    }
}

impl std::fmt::Display for ChangeSummary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} insertions(+), {} deletions(-), {} modifications(~)",
            self.insertions, self.deletions, self.modifications
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_diff() {
        let old = "line1\nline2\nline3\n";
        let new = "line1\nmodified\nline3\n";

        let diff = generate_diff(old, new, "test.txt");

        assert!(diff.contains("--- a/test.txt"));
        assert!(diff.contains("+++ b/test.txt"));
        assert!(diff.contains("-line2"));
        assert!(diff.contains("+modified"));
    }

    #[test]
    fn test_insertion_diff() {
        let old = "line1\nline3\n";
        let new = "line1\nline2\nline3\n";

        let diff = generate_diff(old, new, "test.txt");

        assert!(diff.contains("+line2"));
    }

    #[test]
    fn test_deletion_diff() {
        let old = "line1\nline2\nline3\n";
        let new = "line1\nline3\n";

        let diff = generate_diff(old, new, "test.txt");

        assert!(diff.contains("-line2"));
    }

    #[test]
    fn test_change_summary() {
        let old = "a\nb\nc\n";
        let new = "a\nx\nc\nd\n";

        let summary = generate_change_summary(old, new);

        assert!(summary.insertions > 0 || summary.modifications > 0);
        assert!(summary.total_changes() > 0);
    }
}
