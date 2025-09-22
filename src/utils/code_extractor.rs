//! Enhanced Code Snippet Extractor for DeepLogic AI Analysis
//!
//! This module provides functionality to extract relevant code snippets from source files
//! based on audit findings, and to generate AI-powered fix suggestions using AST analysis.

use crate::utils::ast_analyzer::AstAnalyzer;
use crate::utils::audit::{AuditFinding, CodeSnippet};
use anyhow::{Context, Result};
use std::fs;

/// Enhanced code extractor with AST analysis capabilities
pub struct CodeExtractor {
    ast_analyzer: AstAnalyzer,
}

impl CodeExtractor {
    pub fn new() -> Self {
        Self {
            ast_analyzer: AstAnalyzer::new(),
        }
    }

    /// Extract code snippet around a specific line with context
    pub fn extract_snippet_around_line(
        file_path: &str,
        target_line: usize,
        context_lines: usize,
    ) -> Result<CodeSnippet> {
        let content = fs::read_to_string(file_path)
            .with_context(|| format!("Failed to read file: {}", file_path))?;

        let lines: Vec<&str> = content.lines().collect();
        let total_lines = lines.len();

        if target_line == 0 || target_line > total_lines {
            anyhow::bail!(
                "Invalid line number: {} (file has {} lines)",
                target_line,
                total_lines
            );
        }

        let start_line = if target_line <= context_lines {
            1
        } else {
            target_line - context_lines
        };

        let end_line = std::cmp::min(target_line + context_lines, total_lines);

        let snippet_lines: Vec<&str> = lines
            .iter()
            .skip(start_line - 1)
            .take(end_line - start_line + 1)
            .cloned()
            .collect();

        let snippet_content = snippet_lines.join("\n");

        Ok(CodeSnippet {
            file_path: file_path.to_string(),
            start_line,
            end_line,
            content: snippet_content,
            context: Some(format!("Code context around line {}", target_line)),
        })
    }

    /// Extract code snippet based on pattern matching
    pub fn extract_snippet_by_pattern(
        file_path: &str,
        pattern: &str,
        context_lines: usize,
    ) -> Result<Vec<CodeSnippet>> {
        let content = fs::read_to_string(file_path)
            .with_context(|| format!("Failed to read file: {}", file_path))?;

        let lines: Vec<&str> = content.lines().collect();
        let mut snippets = Vec::new();

        for (line_num, line) in lines.iter().enumerate() {
            if line.contains(pattern) {
                let target_line = line_num + 1; // Convert to 1-based indexing

                if let Ok(snippet) =
                    Self::extract_snippet_around_line(file_path, target_line, context_lines)
                {
                    snippets.push(snippet);
                }
            }
        }

        Ok(snippets)
    }

    /// Extract problematic code based on audit finding
    pub fn extract_problematic_code(finding: &AuditFinding) -> Result<CodeSnippet> {
        if let Some(code_location) = &finding.code_location {
            // Try to parse line number from code_location if it has the format "file:line"
            if let Some((file_path, line_str)) = code_location.split_once(':') {
                if let Ok(line_num) = line_str.parse::<usize>() {
                    return Self::extract_snippet_around_line(file_path, line_num, 3);
                }
            }

            // If no line number, extract based on finding title/description patterns
            let patterns = Self::extract_patterns_from_finding(finding);

            if let Ok(snippets) = Self::extract_snippet_by_pattern(code_location, &patterns[0], 3) {
                if let Some(snippet) = snippets.first() {
                    return Ok(snippet.clone());
                }
            }

            // Fallback: return first few lines of the file
            Self::extract_snippet_around_line(code_location, 1, 10)
        } else {
            anyhow::bail!("No code location specified in finding")
        }
    }

    /// Extract relevant patterns from finding description
    fn extract_patterns_from_finding(finding: &AuditFinding) -> Vec<String> {
        let mut patterns = Vec::new();

        // Common vulnerability patterns based on finding categories
        match finding.category.as_str() {
            "Solana Security" => {
                patterns.extend(vec![
                    "is_signer".to_string(),
                    "invoke".to_string(),
                    "AccountInfo".to_string(),
                    "lamports".to_string(),
                ]);
            }
            "Authentication & Authorization" => {
                patterns.extend(vec![
                    "authority".to_string(),
                    "signer".to_string(),
                    "owner".to_string(),
                ]);
            }
            "Trading Security" => {
                patterns.extend(vec![
                    "slippage".to_string(),
                    "deadline".to_string(),
                    "swap".to_string(),
                    "trade".to_string(),
                ]);
            }
            _ => {
                // Generic patterns based on finding title
                if finding.title.to_lowercase().contains("token") {
                    patterns.push("token".to_string());
                }
                if finding.title.to_lowercase().contains("transfer") {
                    patterns.push("transfer".to_string());
                }
            }
        }

        // Fallback pattern
        if patterns.is_empty() {
            patterns.push("fn ".to_string()); // Look for any function
        }

        patterns
    }

    /// Generate enhanced fix suggestions using AST analysis
    pub fn generate_enhanced_fix(
        &self,
        problematic_code: &CodeSnippet,
        finding: &AuditFinding,
    ) -> Result<CodeSnippet> {
        // Try AST-based analysis first
        match self
            .ast_analyzer
            .generate_ast_based_fix(finding, problematic_code)
        {
            Ok(enhanced_fix) => {
                println!("✨ Generated AST-based fix suggestion");
                Ok(enhanced_fix)
            }
            Err(e) => {
                println!(
                    "⚠️  AST analysis failed, falling back to pattern-based: {}",
                    e
                );
                // Fallback to the original pattern-based approach
                Self::generate_suggested_fix(problematic_code, finding)
            }
        }
    }

    /// Generate a suggested fix snippet based on the problematic code (fallback method)
    pub fn generate_suggested_fix(
        problematic_code: &CodeSnippet,
        finding: &AuditFinding,
    ) -> Result<CodeSnippet> {
        let fixed_content = Self::apply_basic_fixes(&problematic_code.content, finding);

        Ok(CodeSnippet {
            file_path: problematic_code.file_path.clone(),
            start_line: problematic_code.start_line,
            end_line: problematic_code.end_line,
            content: fixed_content,
            context: Some("Pattern-based fix suggestion".to_string()),
        })
    }

    /// Apply basic pattern-based fixes
    fn apply_basic_fixes(content: &str, finding: &AuditFinding) -> String {
        let mut fixed_content = content.to_string();

        match finding.category.as_str() {
            "Authentication & Authorization" => {
                if !content.contains("is_signer")
                    && (content.contains("AccountInfo") || content.contains("ctx.accounts"))
                {
                    fixed_content = fixed_content.replace(
                        "let user_account = &ctx.accounts.user_account;",
                        "let user_account = &ctx.accounts.user_account;\n    require!(user_account.is_signer, ErrorCode::MissingSignature);"
                    );
                }
            }
            "Solana Security" => {
                if content.contains("user_account") && !content.contains("key()") {
                    fixed_content = fixed_content.replace(
                        "user_account.balance += amount;",
                        "require!(user_account.key() == expected_user_key, ErrorCode::InvalidUser);\n    user_account.balance += amount;"
                    );
                }
            }
            "Trading Security" => {
                if content.contains("claim_rewards") && !content.contains("slippage") {
                    fixed_content = fixed_content.replace(
                        "let rewards_to_claim = pool_state.accumulated_rewards * user_share;",
                        "let rewards_to_claim = pool_state.accumulated_rewards * user_share;\n    require!(rewards_to_claim >= min_expected_rewards, ErrorCode::SlippageExceeded);"
                    );
                }
            }
            _ => {
                if content.contains("unwrap()") {
                    fixed_content = fixed_content
                        .replace("unwrap()", "map_err(|_| ErrorCode::OperationFailed)?");
                }
            }
        }

        fixed_content
    }
}

impl Default for CodeExtractor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::audit::AuditSeverity;
    use std::fs;
    use tempfile::NamedTempFile;

    #[test]
    fn test_extract_snippet_around_line() {
        let temp_file = NamedTempFile::new().unwrap();
        let content = "line 1\nline 2\nline 3\nline 4\nline 5\n";
        fs::write(temp_file.path(), content).unwrap();

        let snippet =
            CodeExtractor::extract_snippet_around_line(temp_file.path().to_str().unwrap(), 3, 1)
                .unwrap();

        assert_eq!(snippet.start_line, 2);
        assert_eq!(snippet.end_line, 4);
        assert!(snippet.content.contains("line 2"));
        assert!(snippet.content.contains("line 3"));
        assert!(snippet.content.contains("line 4"));
    }

    #[test]
    fn test_extract_snippet_by_pattern() {
        let temp_file = NamedTempFile::new().unwrap();
        let content = "fn test1() {}\nlet x = 5;\nfn test2() {}\nlet y = 10;\n";
        fs::write(temp_file.path(), content).unwrap();

        let snippets =
            CodeExtractor::extract_snippet_by_pattern(temp_file.path().to_str().unwrap(), "fn ", 1)
                .unwrap();

        assert_eq!(snippets.len(), 2);
        assert!(snippets[0].content.contains("test1"));
        assert!(snippets[1].content.contains("test2"));
    }

    #[test]
    fn test_generate_suggested_fix() {
        let problematic_code = CodeSnippet {
            file_path: "test.rs".to_string(),
            start_line: 1,
            end_line: 2,
            content:
                "let user_account = &ctx.accounts.user_account;\nuser_account.balance += amount;"
                    .to_string(),
            context: None,
        };

        let finding = AuditFinding {
            id: "test".to_string(),
            title: "Missing signer validation".to_string(),
            description: "Test".to_string(),
            severity: AuditSeverity::High,
            category: "Authentication & Authorization".to_string(),
            cwe_id: None,
            cvss_score: None,
            impact: "Test".to_string(),
            recommendation: "Test".to_string(),
            code_location: None,
            references: vec![],
        };

        let fixed_code =
            CodeExtractor::generate_suggested_fix(&problematic_code, &finding).unwrap();
        assert!(fixed_code.content.contains("is_signer"));
    }
}
