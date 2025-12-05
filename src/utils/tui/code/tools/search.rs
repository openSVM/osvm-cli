//! Search tools: glob and grep
//!
//! Provides file pattern matching and content searching.

use super::{RiskLevel, Tool, ToolContext, ToolOutput};
use anyhow::{anyhow, Result};
use async_trait::async_trait;
use serde_json::{json, Value};
use std::path::Path;
use std::process::Stdio;
use tokio::process::Command;

/// Maximum number of results to return
const MAX_RESULTS: usize = 100;

// ============================================================================
// GlobTool
// ============================================================================

/// Find files matching a glob pattern
pub struct GlobTool;

#[async_trait]
impl Tool for GlobTool {
    fn name(&self) -> &str {
        "glob"
    }

    fn description(&self) -> &str {
        "Find files matching a glob pattern (e.g., '**/*.rs', 'src/**/*.ts'). Returns file paths."
    }

    fn parameters_schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "pattern": {
                    "type": "string",
                    "description": "Glob pattern to match files"
                },
                "path": {
                    "type": "string",
                    "description": "Directory to search in (default: project root)"
                }
            },
            "required": ["pattern"]
        })
    }

    fn risk_level(&self) -> RiskLevel {
        RiskLevel::Safe
    }

    fn requires_approval(&self, _params: &Value) -> bool {
        false // Search is always safe
    }

    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput> {
        let pattern = params["pattern"]
            .as_str()
            .ok_or_else(|| anyhow!("missing pattern parameter"))?;

        let search_path = params["path"]
            .as_str()
            .map(|p| ctx.resolve_path(p))
            .transpose()?
            .unwrap_or_else(|| ctx.project_root.clone());

        // Use glob crate for pattern matching
        let full_pattern = search_path.join(pattern);
        let pattern_str = full_pattern.to_string_lossy();

        let mut matches = Vec::new();
        for entry in glob::glob(&pattern_str)? {
            match entry {
                Ok(path) => {
                    // Make path relative to project root
                    if let Ok(relative) = path.strip_prefix(&ctx.project_root) {
                        matches.push(relative.to_string_lossy().to_string());
                    } else {
                        matches.push(path.to_string_lossy().to_string());
                    }

                    if matches.len() >= MAX_RESULTS {
                        break;
                    }
                }
                Err(e) => {
                    // Skip errors (permission denied, etc.)
                    log::debug!("Glob error: {}", e);
                }
            }
        }

        if matches.is_empty() {
            Ok(ToolOutput::text(format!(
                "No files found matching '{}'",
                pattern
            )))
        } else {
            let truncated = matches.len() >= MAX_RESULTS;
            let output = matches.join("\n");

            if truncated {
                Ok(ToolOutput::truncated(
                    format!("{}\n\n[Showing first {} matches]", output, MAX_RESULTS),
                    matches.len(),
                ))
            } else {
                Ok(ToolOutput::text(format!(
                    "{}\n\n[{} files found]",
                    output,
                    matches.len()
                )))
            }
        }
    }
}

// ============================================================================
// GrepTool
// ============================================================================

/// Search file contents using ripgrep or grep
pub struct GrepTool;

#[async_trait]
impl Tool for GrepTool {
    fn name(&self) -> &str {
        "grep"
    }

    fn description(&self) -> &str {
        "Search for a pattern in file contents. Uses ripgrep if available, falls back to grep."
    }

    fn parameters_schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "pattern": {
                    "type": "string",
                    "description": "Regex pattern to search for"
                },
                "path": {
                    "type": "string",
                    "description": "File or directory to search in (default: project root)"
                },
                "glob": {
                    "type": "string",
                    "description": "Only search files matching this glob (e.g., '*.rs')"
                },
                "case_insensitive": {
                    "type": "boolean",
                    "description": "Case insensitive search (default: false)"
                },
                "context": {
                    "type": "integer",
                    "description": "Number of context lines to show (default: 0)"
                }
            },
            "required": ["pattern"]
        })
    }

    fn risk_level(&self) -> RiskLevel {
        RiskLevel::Safe
    }

    fn requires_approval(&self, _params: &Value) -> bool {
        false // Search is always safe
    }

    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput> {
        let pattern = params["pattern"]
            .as_str()
            .ok_or_else(|| anyhow!("missing pattern parameter"))?;

        let search_path = params["path"]
            .as_str()
            .map(|p| ctx.resolve_path(p))
            .transpose()?
            .unwrap_or_else(|| ctx.project_root.clone());

        let file_glob = params["glob"].as_str();
        let case_insensitive = params["case_insensitive"].as_bool().unwrap_or(false);
        let context = params["context"].as_u64().unwrap_or(0) as usize;

        // Try ripgrep first, fall back to grep
        let rg_available = Command::new("rg")
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .await
            .map(|s| s.success())
            .unwrap_or(false);

        let output = if rg_available {
            self.search_with_ripgrep(pattern, &search_path, file_glob, case_insensitive, context)
                .await?
        } else {
            self.search_with_grep(pattern, &search_path, case_insensitive, context)
                .await?
        };

        // Make paths relative
        let relative_output = output
            .lines()
            .map(|line| {
                if let Some(rest) = line.strip_prefix(&ctx.project_root.to_string_lossy().as_ref()) {
                    rest.trim_start_matches('/').to_string()
                } else {
                    line.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        if relative_output.is_empty() {
            Ok(ToolOutput::text(format!(
                "No matches found for '{}'",
                pattern
            )))
        } else {
            let match_count = relative_output.lines().count();
            Ok(ToolOutput::text(format!(
                "{}\n\n[{} matches]",
                relative_output, match_count
            )))
        }
    }
}

impl GrepTool {
    async fn search_with_ripgrep(
        &self,
        pattern: &str,
        path: &Path,
        file_glob: Option<&str>,
        case_insensitive: bool,
        context: usize,
    ) -> Result<String> {
        let mut cmd = Command::new("rg");
        cmd.arg("--no-heading")
            .arg("--line-number")
            .arg("--color=never")
            .arg("--max-count=100");

        if case_insensitive {
            cmd.arg("-i");
        }

        if context > 0 {
            cmd.arg("-C").arg(context.to_string());
        }

        if let Some(glob) = file_glob {
            cmd.arg("--glob").arg(glob);
        }

        cmd.arg(pattern).arg(path);

        let output = cmd.output().await?;

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    async fn search_with_grep(
        &self,
        pattern: &str,
        path: &Path,
        case_insensitive: bool,
        context: usize,
    ) -> Result<String> {
        let mut cmd = Command::new("grep");
        cmd.arg("-r")
            .arg("-n")
            .arg("--color=never")
            .arg("-m").arg("100");

        if case_insensitive {
            cmd.arg("-i");
        }

        if context > 0 {
            cmd.arg("-C").arg(context.to_string());
        }

        cmd.arg(pattern).arg(path);

        let output = cmd.output().await?;

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_glob_tool() {
        let temp_dir = TempDir::new().unwrap();
        std::fs::write(temp_dir.path().join("foo.rs"), "fn main() {}").unwrap();
        std::fs::write(temp_dir.path().join("bar.rs"), "fn bar() {}").unwrap();
        std::fs::write(temp_dir.path().join("baz.txt"), "text file").unwrap();

        let ctx = ToolContext::new(temp_dir.path().to_path_buf());
        let tool = GlobTool;

        let result = tool
            .execute(json!({"pattern": "*.rs"}), &ctx)
            .await
            .unwrap();

        assert!(result.text.contains("foo.rs"));
        assert!(result.text.contains("bar.rs"));
        assert!(!result.text.contains("baz.txt"));
    }

    #[tokio::test]
    async fn test_grep_tool() {
        let temp_dir = TempDir::new().unwrap();
        std::fs::write(temp_dir.path().join("test.rs"), "fn main() {\n    println!(\"hello\");\n}").unwrap();

        let ctx = ToolContext::new(temp_dir.path().to_path_buf());
        let tool = GrepTool;

        let result = tool
            .execute(json!({"pattern": "println"}), &ctx)
            .await
            .unwrap();

        assert!(result.text.contains("println"));
    }
}
