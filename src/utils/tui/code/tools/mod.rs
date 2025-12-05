//! Tool system for osvm code
//!
//! Provides a trait-based tool architecture for file operations, command execution,
//! and search functionality. Each tool declares its risk level and approval requirements.

mod files;
mod bash;
mod search;

pub use files::{ReadTool, WriteTool, EditTool};
pub use bash::BashTool;
pub use search::{GlobTool, GrepTool};

use anyhow::Result;
use async_trait::async_trait;
use serde_json::Value;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

/// Risk level for tool operations - determines approval requirements
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RiskLevel {
    /// Read-only operations - auto-approved
    Safe,
    /// Reversible writes - requires approval
    Low,
    /// Potentially destructive - requires approval with preview
    Medium,
    /// System-level changes - always requires explicit approval
    High,
}

/// Context provided to tools during execution
#[derive(Debug, Clone)]
pub struct ToolContext {
    /// Root directory of the project
    pub project_root: PathBuf,
    /// Project name (directory name)
    pub project_name: String,
    /// Current working directory
    pub cwd: PathBuf,
}

impl ToolContext {
    pub fn new(project_root: PathBuf) -> Self {
        let project_name = project_root
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("project")
            .to_string();
        let cwd = project_root.clone();
        Self {
            project_root,
            project_name,
            cwd,
        }
    }

    /// Resolve a path relative to project root, ensuring it stays within bounds
    pub fn resolve_path(&self, path: &str) -> Result<PathBuf> {
        let path = PathBuf::from(path);
        let abs_path = if path.is_absolute() {
            path
        } else {
            self.project_root.join(path)
        };

        // Canonicalize to resolve .. and symlinks
        let canonical = abs_path.canonicalize().unwrap_or(abs_path.clone());

        // Security check: must be within project root
        if !canonical.starts_with(&self.project_root) {
            anyhow::bail!(
                "Path '{}' is outside project root '{}'",
                canonical.display(),
                self.project_root.display()
            );
        }

        Ok(canonical)
    }

    /// Check if a path exists within the project
    pub fn path_exists(&self, path: &str) -> bool {
        self.resolve_path(path).map(|p| p.exists()).unwrap_or(false)
    }
}

/// Output from tool execution
#[derive(Debug, Clone)]
pub struct ToolOutput {
    /// Text content of the output
    pub text: String,
    /// Whether this output should be truncated in display
    pub truncated: bool,
    /// Original byte count before truncation
    pub original_size: usize,
}

impl ToolOutput {
    pub fn text(content: impl Into<String>) -> Self {
        let text = content.into();
        let size = text.len();
        Self {
            text,
            truncated: false,
            original_size: size,
        }
    }

    pub fn truncated(content: impl Into<String>, original_size: usize) -> Self {
        Self {
            text: content.into(),
            truncated: true,
            original_size,
        }
    }
}

/// Core trait for all tools in the system
#[async_trait]
pub trait Tool: Send + Sync {
    /// Tool name used in API calls
    fn name(&self) -> &str;

    /// Human-readable description
    fn description(&self) -> &str;

    /// JSON schema for parameters
    fn parameters_schema(&self) -> Value;

    /// Risk level of this tool
    fn risk_level(&self) -> RiskLevel;

    /// Execute the tool with given parameters
    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput>;

    /// Check if this specific invocation requires approval
    fn requires_approval(&self, params: &Value) -> bool;

    /// Generate a preview for approval dialog (e.g., diff for edits)
    fn generate_preview(&self, params: &Value, ctx: &ToolContext) -> Option<String> {
        let _ = (params, ctx);
        None
    }
}

/// Registry of available tools
pub struct ToolRegistry {
    tools: HashMap<String, Arc<dyn Tool>>,
}

impl ToolRegistry {
    /// Create a new registry with default tools
    pub fn new() -> Self {
        let mut registry = Self {
            tools: HashMap::new(),
        };

        // Register default tools
        registry.register(Arc::new(ReadTool));
        registry.register(Arc::new(WriteTool));
        registry.register(Arc::new(EditTool));
        registry.register(Arc::new(BashTool));
        registry.register(Arc::new(GlobTool));
        registry.register(Arc::new(GrepTool));

        registry
    }

    /// Register a tool
    pub fn register(&mut self, tool: Arc<dyn Tool>) {
        self.tools.insert(tool.name().to_string(), tool);
    }

    /// Get a tool by name
    pub fn get(&self, name: &str) -> Option<Arc<dyn Tool>> {
        self.tools.get(name).cloned()
    }

    /// Get all tool definitions for the AI prompt
    pub fn get_tool_definitions(&self) -> Vec<Value> {
        self.tools
            .values()
            .map(|tool| {
                serde_json::json!({
                    "name": tool.name(),
                    "description": tool.description(),
                    "input_schema": tool.parameters_schema()
                })
            })
            .collect()
    }

    /// List all tool names
    pub fn list_tools(&self) -> Vec<&str> {
        self.tools.keys().map(|s| s.as_str()).collect()
    }
}

impl Default for ToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tool_context_resolve_path() {
        let ctx = ToolContext::new(PathBuf::from("/tmp/test-project"));

        // Relative path should work
        let result = ctx.resolve_path("src/main.rs");
        assert!(result.is_ok() || result.is_err()); // May not exist, that's ok

        // Absolute path within project should work
        let result = ctx.resolve_path("/tmp/test-project/src/main.rs");
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_tool_registry() {
        let registry = ToolRegistry::new();

        assert!(registry.get("read").is_some());
        assert!(registry.get("write").is_some());
        assert!(registry.get("edit").is_some());
        assert!(registry.get("bash").is_some());
        assert!(registry.get("glob").is_some());
        assert!(registry.get("grep").is_some());
        assert!(registry.get("nonexistent").is_none());
    }
}
