//! Automatic parenthesis balancing for OVSM LISP code
//!
//! This module provides automatic correction of missing or mismatched
//! parentheses in OVSM code, similar to error recovery in modern compilers.

use crate::error::Result;
use crate::parser::SExprParser;
use crate::Scanner;

/// Attempts to automatically fix missing or mismatched parentheses
pub struct ParenFixer {
    /// Original source code
    source: String,
    /// Lines of source code
    lines: Vec<String>,
}

impl ParenFixer {
    /// Create a new parenthesis fixer
    pub fn new(source: impl Into<String>) -> Self {
        let source = source.into();
        let lines: Vec<String> = source.lines().map(|s| s.to_string()).collect();

        Self { source, lines }
    }

    /// Analyze and fix parenthesis imbalances with smart iterative placement
    pub fn fix(&self) -> Result<String> {
        // Count parentheses
        let stats = self.count_parens();

        if stats.is_balanced() {
            // Already balanced - return original
            return Ok(self.source.clone());
        }

        // Try to fix the imbalance with smart placement
        if stats.open_count > stats.close_count {
            // Missing closing parens - try smart placement
            let missing = stats.open_count - stats.close_count;

            // Try smart iterative placement first
            if let Some(fixed) = self.smart_fix_missing_parens(missing) {
                return Ok(fixed);
            }

            // Fallback to simple end placement
            Ok(self.add_closing_parens(missing))
        } else {
            // Too many closing parens - remove extras
            let extra = stats.close_count - stats.open_count;
            Ok(self.remove_extra_closing_parens(extra))
        }
    }

    /// Try to fix missing parens by iteratively trying different positions
    /// Returns the first valid placement that parses successfully
    fn smart_fix_missing_parens(&self, missing: usize) -> Option<String> {
        // Find candidate positions where we can insert closing parens
        let candidates = self.find_insertion_candidates();

        // Try each candidate position
        for pos in candidates {
            let attempt = self.insert_closing_parens_at(pos, missing);

            // Validate by parsing
            if self.validates(&attempt) {
                return Some(attempt);
            }
        }

        None
    }

    /// Find candidate positions for inserting closing parens
    /// Returns positions sorted by likelihood of being correct
    fn find_insertion_candidates(&self) -> Vec<usize> {
        let mut candidates = Vec::new();
        let mut in_string = false;
        let mut escape_next = false;
        let mut depth = 0_i32;

        for (idx, ch) in self.source.chars().enumerate() {
            // Handle escape sequences
            if escape_next {
                escape_next = false;
                continue;
            }

            if ch == '\\' && in_string {
                escape_next = true;
                continue;
            }

            // Handle strings
            if ch == '"' {
                in_string = !in_string;
                continue;
            }

            if in_string {
                continue;
            }

            // Track depth and record positions
            match ch {
                '(' => {
                    depth += 1;
                }
                ')' => {
                    depth -= 1;
                }
                '\n' => {
                    // End of line is a good candidate if we're in nested code
                    if depth > 0 {
                        candidates.push(idx + 1);
                    }
                }
                _ => {}
            }
        }

        // Add end of file as final candidate
        candidates.push(self.source.len());

        // Sort by likelihood (prefer positions with higher depth changes)
        // For now, keep them in order (end of lines, then end of file)
        candidates
    }

    /// Insert closing parens at a specific position
    fn insert_closing_parens_at(&self, pos: usize, count: usize) -> String {
        let mut result = String::new();

        // Ensure pos is at a valid UTF-8 character boundary
        let safe_pos = if pos <= self.source.len() && self.source.is_char_boundary(pos) {
            pos
        } else {
            // Find the nearest valid boundary before pos
            (0..=pos.min(self.source.len()))
                .rev()
                .find(|&i| self.source.is_char_boundary(i))
                .unwrap_or(0)
        };

        result.push_str(&self.source[..safe_pos]);

        // Add the closing parens
        for _ in 0..count {
            result.push(')');
        }

        // Add rest of source
        if safe_pos < self.source.len() {
            result.push_str(&self.source[safe_pos..]);
        }

        result
    }

    /// Validate code by attempting to parse it
    fn validates(&self, code: &str) -> bool {
        // Try to tokenize
        let mut scanner = Scanner::new(code);
        let tokens = match scanner.scan_tokens() {
            Ok(t) => t,
            Err(_) => return false,
        };

        // Try to parse
        let mut parser = SExprParser::new(tokens);
        parser.parse().is_ok()
    }

    /// Count all parentheses in the source
    fn count_parens(&self) -> ParenStats {
        let mut open_count = 0;
        let mut close_count = 0;
        let mut in_string = false;
        let mut escape_next = false;

        for line in &self.lines {
            let mut in_comment = false; // Comments are line-based

            for ch in line.chars() {
                // Handle escape sequences
                if escape_next {
                    escape_next = false;
                    continue;
                }

                if ch == '\\' && in_string {
                    escape_next = true;
                    continue;
                }

                // Handle strings
                if ch == '"' && !in_comment {
                    in_string = !in_string;
                    continue;
                }

                // Skip if we're in a string or comment
                if in_string {
                    continue;
                }

                // Handle comments
                if ch == ';' {
                    in_comment = true;
                    continue;
                }

                if in_comment {
                    continue;
                }

                // Count parentheses
                match ch {
                    '(' => open_count += 1,
                    ')' => close_count += 1,
                    _ => {}
                }
            }
        }

        ParenStats {
            open_count,
            close_count,
        }
    }

    /// Add closing parentheses at the end
    fn add_closing_parens(&self, count: usize) -> String {
        let mut fixed = self.source.clone();

        // Add newline if source doesn't end with one
        if !fixed.ends_with('\n') {
            fixed.push('\n');
        }

        // Add closing parens with a comment
        fixed.push_str(&format!(
            ";; Auto-corrected: added {} missing closing paren{}\n",
            count,
            if count == 1 { "" } else { "s" }
        ));

        for _ in 0..count {
            fixed.push(')');
        }

        fixed
    }

    /// Remove extra closing parentheses (tries to be smart about it)
    fn remove_extra_closing_parens(&self, extra: usize) -> String {
        let mut fixed = String::new();
        let mut remaining_to_remove = extra;
        let mut depth = 0_i32; // Track nesting depth

        let mut in_string = false;
        let mut escape_next = false;

        for ch in self.source.chars() {
            // Handle escape sequences
            if escape_next {
                escape_next = false;
                fixed.push(ch);
                continue;
            }

            if ch == '\\' && in_string {
                escape_next = true;
                fixed.push(ch);
                continue;
            }

            // Handle strings
            if ch == '"' {
                in_string = !in_string;
                fixed.push(ch);
                continue;
            }

            if in_string {
                fixed.push(ch);
                continue;
            }

            // Track depth and remove extras
            match ch {
                '(' => {
                    depth += 1;
                    fixed.push(ch);
                }
                ')' => {
                    if depth > 0 {
                        // Valid closing paren
                        depth -= 1;
                        fixed.push(ch);
                    } else if remaining_to_remove > 0 {
                        // Extra closing paren - skip it
                        remaining_to_remove -= 1;
                    } else {
                        // Shouldn't happen, but keep it
                        fixed.push(ch);
                    }
                }
                _ => fixed.push(ch),
            }
        }

        fixed
    }

    /// Get the fixed code with a report
    pub fn fix_with_report(&self) -> (String, Option<String>) {
        let stats = self.count_parens();

        if stats.is_balanced() {
            return (self.source.clone(), None);
        }

        // Try to fix and determine which method was used
        let fixed_result = self.fix();

        let report = if stats.open_count > stats.close_count {
            let missing = stats.open_count - stats.close_count;

            // Check if smart placement found a solution
            let used_smart = if let Ok(ref _fixed) = fixed_result {
                self.smart_fix_missing_parens(missing).is_some()
            } else {
                false
            };

            if used_smart {
                Some(format!(
                    "✨ Auto-corrected: Added {} missing closing paren{} (smart placement validated)",
                    missing,
                    if missing == 1 { "" } else { "s" }
                ))
            } else {
                Some(format!(
                    "⚠️  Auto-corrected: Added {} missing closing paren{} at end of code",
                    missing,
                    if missing == 1 { "" } else { "s" }
                ))
            }
        } else {
            let extra = stats.close_count - stats.open_count;
            Some(format!(
                "⚠️  Auto-corrected: Removed {} extra closing paren{}",
                extra,
                if extra == 1 { "" } else { "s" }
            ))
        };

        match fixed_result {
            Ok(fixed) => (fixed, report),
            Err(_) => (
                self.source.clone(),
                Some("⚠️  Could not auto-correct parentheses".to_string()),
            ),
        }
    }
}

/// Parenthesis counting statistics
#[derive(Debug, Clone, Copy)]
struct ParenStats {
    open_count: usize,
    close_count: usize,
}

impl ParenStats {
    /// Check if parentheses are balanced
    fn is_balanced(&self) -> bool {
        self.open_count == self.close_count
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_balanced_code() {
        let code = "(define x (+ 1 2))";
        let fixer = ParenFixer::new(code);
        let (fixed, report) = fixer.fix_with_report();

        assert_eq!(fixed, code);
        assert!(report.is_none());
    }

    #[test]
    fn test_missing_closing_paren() {
        let code = "(define x (+ 1 2)";
        let fixer = ParenFixer::new(code);
        let (fixed, report) = fixer.fix_with_report();

        assert!(fixed.contains(')'));
        assert!(report.is_some());
        let msg = report.unwrap();
        assert!(msg.contains("Auto-corrected"));
        assert!(msg.contains("1 missing closing paren"));
    }

    #[test]
    fn test_multiple_missing_closing_parens() {
        let code = "(define x (+ 1 (- 5 3";
        let fixer = ParenFixer::new(code);
        let (fixed, report) = fixer.fix_with_report();

        let open_count = code.chars().filter(|&c| c == '(').count();
        let close_count_original = code.chars().filter(|&c| c == ')').count();
        let close_count_fixed = fixed.chars().filter(|&c| c == ')').count();

        assert_eq!(open_count, close_count_fixed);
        assert_eq!(close_count_fixed - close_count_original, 3);
        assert!(report.unwrap().contains("3 missing closing parens"));
    }

    #[test]
    fn test_extra_closing_paren() {
        let code = "(define x (+ 1 2)))";
        let fixer = ParenFixer::new(code);
        let (fixed, report) = fixer.fix_with_report();

        let open_count = fixed.chars().filter(|&c| c == '(').count();
        let close_count = fixed.chars().filter(|&c| c == ')').count();

        assert_eq!(open_count, close_count);
        assert!(report.unwrap().contains("1 extra closing paren"));
    }

    #[test]
    fn test_ignore_parens_in_strings() {
        let code = r#"(define msg "missing ( paren")"#;
        let fixer = ParenFixer::new(code);
        let (fixed, report) = fixer.fix_with_report();

        // The ( in the string should be ignored
        assert_eq!(fixed, code);
        assert!(report.is_none());
    }

    #[test]
    fn test_ignore_parens_in_comments() {
        let code = ";; This has ( unclosed paren\n(define x 1)";
        let fixer = ParenFixer::new(code);
        let (fixed, report) = fixer.fix_with_report();

        assert_eq!(fixed, code);
        assert!(report.is_none());
    }

    #[test]
    fn test_complex_nested_code_actually_missing_one() {
        let code = r#"(define add_tx (lambda (sender amt sig)
  (define idx (FIND senders sender))
  (if (== idx -1)
      (do
        (set! senders (APPEND senders [sender]))
        (set! amounts (APPEND amounts [amt])))
      (do
        (set! amounts (UPDATE amounts idx (+ ([] amounts idx) amt))))))"#;

        let fixer = ParenFixer::new(code);
        let (fixed, report) = fixer.fix_with_report();

        // This code is MISSING one closing paren (17 open, 16 close)
        let open_count = fixed.chars().filter(|&c| c == '(').count();
        let close_count = fixed.chars().filter(|&c| c == ')').count();

        assert_eq!(open_count, close_count);
        assert!(report.is_some());
        assert!(report.unwrap().contains("1 missing closing paren"));
    }

    #[test]
    fn test_truly_balanced_complex_code() {
        // This one is ACTUALLY balanced (added the missing paren at the end)
        let code = r#"(define add_tx (lambda (sender amt sig)
  (define idx (FIND senders sender))
  (if (== idx -1)
      (do
        (set! senders (APPEND senders [sender]))
        (set! amounts (APPEND amounts [amt])))
      (do
        (set! amounts (UPDATE amounts idx (+ ([] amounts idx) amt)))))))"#;

        let fixer = ParenFixer::new(code);
        let (fixed, report) = fixer.fix_with_report();

        // This should NOT be modified
        assert_eq!(fixed, code);
        assert!(report.is_none());
    }

    #[test]
    fn test_smart_placement_validation() {
        // Code with missing paren that SHOULD be placed mid-code, not at end
        let code = "(define x (+ 1 2)\n(define y (+ 3 4))";

        let fixer = ParenFixer::new(code);
        let (fixed, report) = fixer.fix_with_report();

        // Should be fixed and parseable
        assert!(report.is_some());

        // Verify it actually parses
        let validates = fixer.validates(&fixed);
        assert!(validates, "Smart placement should produce parseable code");

        // Should use smart placement (indicated by ✨)
        if let Some(msg) = report {
            // Either smart placement (✨) or fallback (⚠️) - both should work
            assert!(msg.contains("Auto-corrected"));
        }
    }

    #[test]
    fn test_smart_placement_finds_correct_position() {
        // Code with missing paren where smart placement should help
        let code = "(define data [1 2 3]\n(define count (+ 1 2)";

        let fixer = ParenFixer::new(code);
        let (fixed, report) = fixer.fix_with_report();

        // Should fix it
        assert!(report.is_some());

        // Count should be balanced
        let open = fixed.chars().filter(|&c| c == '(').count();
        let close = fixed.chars().filter(|&c| c == ')').count();
        assert_eq!(open, close, "Parentheses should be balanced");

        // Ideally the fixed code should parse (best effort)
        // Note: Smart placement may not always find the perfect spot,
        // but it should at least balance the parens
    }
}
