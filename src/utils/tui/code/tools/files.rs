//! File operation tools: read, write, edit
//!
//! These tools provide safe file operations within the project directory.

use super::{RiskLevel, Tool, ToolContext, ToolOutput};
use anyhow::{anyhow, Result};
use async_trait::async_trait;
use serde_json::{json, Value};
use std::path::Path;

/// Maximum file size to read (50KB)
const MAX_READ_SIZE: usize = 50_000;

/// Maximum file size to write (1MB)
const MAX_WRITE_SIZE: usize = 1_000_000;

// ============================================================================
// ReadTool
// ============================================================================

/// Read file contents from the project
pub struct ReadTool;

#[async_trait]
impl Tool for ReadTool {
    fn name(&self) -> &str {
        "read"
    }

    fn description(&self) -> &str {
        "Read the contents of a file from the project. Returns the file contents with line numbers."
    }

    fn parameters_schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "file_path": {
                    "type": "string",
                    "description": "Path to the file to read, relative to project root"
                },
                "offset": {
                    "type": "integer",
                    "description": "Line number to start reading from (1-indexed)"
                },
                "limit": {
                    "type": "integer",
                    "description": "Maximum number of lines to read"
                }
            },
            "required": ["file_path"]
        })
    }

    fn risk_level(&self) -> RiskLevel {
        RiskLevel::Safe
    }

    fn requires_approval(&self, _params: &Value) -> bool {
        false // Read operations are always auto-approved
    }

    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput> {
        let file_path = params["file_path"]
            .as_str()
            .ok_or_else(|| anyhow!("missing file_path parameter"))?;

        let offset = params["offset"].as_u64().unwrap_or(1) as usize;
        let limit = params["limit"].as_u64().map(|l| l as usize);

        // Resolve and validate path
        let abs_path = ctx.resolve_path(file_path)?;

        if !abs_path.exists() {
            return Err(anyhow!("File not found: {}", file_path));
        }

        if !abs_path.is_file() {
            return Err(anyhow!("Path is not a file: {}", file_path));
        }

        // Check file size
        let metadata = tokio::fs::metadata(&abs_path).await?;
        if metadata.len() > MAX_READ_SIZE as u64 * 2 {
            // Allow reading but will truncate
        }

        // Read content
        let content = tokio::fs::read_to_string(&abs_path).await?;
        let lines: Vec<&str> = content.lines().collect();
        let total_lines = lines.len();

        // Apply offset and limit
        let start_idx = (offset.saturating_sub(1)).min(lines.len());
        let end_idx = match limit {
            Some(l) => (start_idx + l).min(lines.len()),
            None => lines.len(),
        };

        let selected_lines = &lines[start_idx..end_idx];

        // Format with line numbers
        let mut output = String::new();
        for (i, line) in selected_lines.iter().enumerate() {
            let line_num = start_idx + i + 1;
            output.push_str(&format!("{:>6}│ {}\n", line_num, line));
        }

        // Truncate if too large
        let original_size = output.len();
        let (text, truncated) = if output.len() > MAX_READ_SIZE {
            let truncated_output = format!(
                "{}...\n\n[Truncated: showing {} of {} lines, {} of {} bytes]",
                &output[..MAX_READ_SIZE],
                end_idx - start_idx,
                total_lines,
                MAX_READ_SIZE,
                original_size
            );
            (truncated_output, true)
        } else {
            (output, false)
        };

        if truncated {
            Ok(ToolOutput::truncated(text, original_size))
        } else {
            Ok(ToolOutput::text(text))
        }
    }
}

// ============================================================================
// WriteTool
// ============================================================================

/// Write content to a file (creates or overwrites)
pub struct WriteTool;

#[async_trait]
impl Tool for WriteTool {
    fn name(&self) -> &str {
        "write"
    }

    fn description(&self) -> &str {
        "Create a new file or overwrite an existing file with the provided content."
    }

    fn parameters_schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "file_path": {
                    "type": "string",
                    "description": "Path to the file to write, relative to project root"
                },
                "content": {
                    "type": "string",
                    "description": "Content to write to the file"
                }
            },
            "required": ["file_path", "content"]
        })
    }

    fn risk_level(&self) -> RiskLevel {
        RiskLevel::Low
    }

    fn requires_approval(&self, _params: &Value) -> bool {
        true // Write operations always require approval
    }

    fn generate_preview(&self, params: &Value, ctx: &ToolContext) -> Option<String> {
        let file_path = params["file_path"].as_str()?;
        let content = params["content"].as_str()?;

        let exists = ctx.path_exists(file_path);
        let action = if exists { "OVERWRITE" } else { "CREATE" };

        let preview_content = if content.len() > 500 {
            format!("{}...\n[{} bytes total]", &content[..500], content.len())
        } else {
            content.to_string()
        };

        Some(format!(
            "{} {}\n\n{}",
            action, file_path, preview_content
        ))
    }

    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput> {
        let file_path = params["file_path"]
            .as_str()
            .ok_or_else(|| anyhow!("missing file_path parameter"))?;

        let content = params["content"]
            .as_str()
            .ok_or_else(|| anyhow!("missing content parameter"))?;

        // Size check
        if content.len() > MAX_WRITE_SIZE {
            return Err(anyhow!(
                "Content too large: {} bytes (max {})",
                content.len(),
                MAX_WRITE_SIZE
            ));
        }

        // Resolve path (allows creating new files)
        let abs_path = if Path::new(file_path).is_absolute() {
            let p = std::path::PathBuf::from(file_path);
            if !p.starts_with(&ctx.project_root) {
                return Err(anyhow!("Path outside project root"));
            }
            p
        } else {
            ctx.project_root.join(file_path)
        };

        // Create parent directories
        if let Some(parent) = abs_path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        // Check if overwriting
        let action = if abs_path.exists() {
            "Overwrote"
        } else {
            "Created"
        };

        // Write file
        tokio::fs::write(&abs_path, content).await?;

        Ok(ToolOutput::text(format!(
            "{} {} ({} bytes)",
            action,
            file_path,
            content.len()
        )))
    }
}

// ============================================================================
// EditTool
// ============================================================================

/// Edit a file by replacing a specific string
pub struct EditTool;

#[async_trait]
impl Tool for EditTool {
    fn name(&self) -> &str {
        "edit"
    }

    fn description(&self) -> &str {
        "Edit a file by replacing a specific string with new content. The old_string must be unique in the file."
    }

    fn parameters_schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "file_path": {
                    "type": "string",
                    "description": "Path to the file to edit"
                },
                "old_string": {
                    "type": "string",
                    "description": "The exact string to find and replace (must be unique)"
                },
                "new_string": {
                    "type": "string",
                    "description": "The string to replace it with"
                }
            },
            "required": ["file_path", "old_string", "new_string"]
        })
    }

    fn risk_level(&self) -> RiskLevel {
        RiskLevel::Low
    }

    fn requires_approval(&self, _params: &Value) -> bool {
        true // Edit operations always require approval
    }

    fn generate_preview(&self, params: &Value, ctx: &ToolContext) -> Option<String> {
        let file_path = params["file_path"].as_str()?;
        let old_string = params["old_string"].as_str()?;
        let new_string = params["new_string"].as_str()?;

        // Try to read the file and generate diff
        let abs_path = ctx.resolve_path(file_path).ok()?;
        let content = std::fs::read_to_string(&abs_path).ok()?;

        let count = content.matches(old_string).count();
        if count != 1 {
            return Some(format!(
                "⚠️ old_string appears {} times (must be exactly 1)",
                count
            ));
        }

        let new_content = content.replace(old_string, new_string);

        // Generate simple diff
        Some(crate::utils::tui::code::diff::generate_diff(
            &content,
            &new_content,
            file_path,
        ))
    }

    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput> {
        let file_path = params["file_path"]
            .as_str()
            .ok_or_else(|| anyhow!("missing file_path parameter"))?;

        let old_string = params["old_string"]
            .as_str()
            .ok_or_else(|| anyhow!("missing old_string parameter"))?;

        let new_string = params["new_string"]
            .as_str()
            .ok_or_else(|| anyhow!("missing new_string parameter"))?;

        if old_string == new_string {
            return Err(anyhow!("old_string and new_string are identical"));
        }

        // Resolve and read
        let abs_path = ctx.resolve_path(file_path)?;
        let content = tokio::fs::read_to_string(&abs_path).await?;

        // Find occurrences
        let count = content.matches(old_string).count();
        if count == 0 {
            return Err(anyhow!(
                "old_string not found in file. Make sure to match exact whitespace and content."
            ));
        }
        if count > 1 {
            return Err(anyhow!(
                "old_string appears {} times in file. It must be unique. Include more surrounding context.",
                count
            ));
        }

        // Perform replacement
        let new_content = content.replace(old_string, new_string);

        // Write back
        tokio::fs::write(&abs_path, &new_content).await?;

        Ok(ToolOutput::text(format!(
            "Edited {}: replaced {} bytes with {} bytes",
            file_path,
            old_string.len(),
            new_string.len()
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_read_tool() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test.txt");
        std::fs::write(&file_path, "line1\nline2\nline3\n").unwrap();

        let ctx = ToolContext::new(temp_dir.path().to_path_buf());
        let tool = ReadTool;

        let result = tool
            .execute(json!({"file_path": "test.txt"}), &ctx)
            .await
            .unwrap();

        assert!(result.text.contains("line1"));
        assert!(result.text.contains("line2"));
        assert!(result.text.contains("line3"));
    }

    #[tokio::test]
    async fn test_write_tool() {
        let temp_dir = TempDir::new().unwrap();
        let ctx = ToolContext::new(temp_dir.path().to_path_buf());
        let tool = WriteTool;

        let result = tool
            .execute(
                json!({
                    "file_path": "new_file.txt",
                    "content": "Hello, World!"
                }),
                &ctx,
            )
            .await
            .unwrap();

        assert!(result.text.contains("Created"));

        let content = std::fs::read_to_string(temp_dir.path().join("new_file.txt")).unwrap();
        assert_eq!(content, "Hello, World!");
    }

    #[tokio::test]
    async fn test_edit_tool() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test.txt");
        std::fs::write(&file_path, "Hello, World!").unwrap();

        let ctx = ToolContext::new(temp_dir.path().to_path_buf());
        let tool = EditTool;

        let result = tool
            .execute(
                json!({
                    "file_path": "test.txt",
                    "old_string": "World",
                    "new_string": "Rust"
                }),
                &ctx,
            )
            .await
            .unwrap();

        assert!(result.text.contains("Edited"));

        let content = std::fs::read_to_string(&file_path).unwrap();
        assert_eq!(content, "Hello, Rust!");
    }

    #[tokio::test]
    async fn test_edit_tool_non_unique() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test.txt");
        std::fs::write(&file_path, "foo bar foo").unwrap();

        let ctx = ToolContext::new(temp_dir.path().to_path_buf());
        let tool = EditTool;

        let result = tool
            .execute(
                json!({
                    "file_path": "test.txt",
                    "old_string": "foo",
                    "new_string": "baz"
                }),
                &ctx,
            )
            .await;

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("2 times"));
    }
}
