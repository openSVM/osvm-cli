//! System prompt builder for the AI agent
//!
//! Constructs the system prompt with project context and tool definitions.

use super::tools::{ToolContext, ToolRegistry};
use regex::Regex;
use serde_json::Value;

/// A parsed tool call from the AI response
#[derive(Debug, Clone)]
pub struct ParsedToolCall {
    pub name: String,
    pub params: Value,
}

/// Build the system prompt for the AI agent
pub fn build_system_prompt(ctx: &ToolContext, registry: &ToolRegistry) -> String {
    let tool_list = registry
        .list_tools()
        .iter()
        .map(|name| format!("  - {}", name))
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        r#"You are an AI coding assistant helping with a software project.

## Project Context
- **Project**: {}
- **Working Directory**: {}

## Available Tools
You have access to these tools to help complete tasks:

{}

## CRITICAL: Tool Call Syntax

When you need to use a tool, you MUST use this EXACT XML format:

<tool name="TOOL_NAME">
{{"param1": "value1", "param2": "value2"}}
</tool>

### Tool Examples

**Read a file:**
<tool name="read">
{{"file_path": "src/main.rs"}}
</tool>

**Write a new file:**
<tool name="write">
{{"file_path": "src/new_file.rs", "content": "// file content here"}}
</tool>

**Edit existing file (old_string must be EXACT and UNIQUE in the file):**
<tool name="edit">
{{"file_path": "src/main.rs", "old_string": "fn old_name()", "new_string": "fn new_name()"}}
</tool>

**Run a command:**
<tool name="bash">
{{"command": "cargo test"}}
</tool>

**Find files by pattern:**
<tool name="glob">
{{"pattern": "**/*.rs"}}
</tool>

**Search file contents:**
<tool name="grep">
{{"pattern": "TODO", "path": "src/"}}
</tool>

## Behavior Guidelines

1. **Read before editing**: ALWAYS read a file before editing it. You need the exact content to make edits.

2. **Minimal changes**: Make only the changes necessary. Don't refactor unrelated code.

3. **One tool at a time**: Use one tool, wait for results, then decide next action.

4. **Be precise with edits**: The old_string in edit MUST be an exact copy from the file. Copy-paste from read results.

5. **Verify changes**: After edits, consider running tests with bash to verify.

## Response Format

- Use tool calls when you need to interact with the filesystem or run commands
- Use plain text when explaining, asking questions, or reporting results
- After a task is complete, summarize what was done

Remember: You're assisting a developer. Be helpful, precise, and thorough."#,
        ctx.project_name,
        ctx.project_root.display(),
        tool_list
    )
}

/// Parse tool calls from AI response text
///
/// Looks for patterns like:
/// <tool name="read">
/// {"file_path": "src/main.rs"}
/// </tool>
pub fn parse_tool_calls(response: &str) -> Vec<ParsedToolCall> {
    let mut calls = Vec::new();

    // Match <tool name="...">...</tool> blocks
    let re = Regex::new(r#"<tool\s+name="([^"]+)">\s*([\s\S]*?)\s*</tool>"#).unwrap();

    for cap in re.captures_iter(response) {
        let name = cap.get(1).map(|m| m.as_str().to_string()).unwrap_or_default();
        let params_str = cap.get(2).map(|m| m.as_str()).unwrap_or("{}");

        // Try to parse the JSON parameters
        if let Ok(params) = serde_json::from_str::<Value>(params_str) {
            calls.push(ParsedToolCall { name, params });
        } else {
            log::warn!("Failed to parse tool params for {}: {}", name, params_str);
        }
    }

    calls
}

/// Extract text content from response (everything outside tool calls)
pub fn extract_text_content(response: &str) -> String {
    let re = Regex::new(r#"<tool\s+name="[^"]+">[\s\S]*?</tool>"#).unwrap();
    let text = re.replace_all(response, "").to_string();

    // Clean up extra whitespace
    text.lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

/// Build tool definitions for the API request
pub fn build_tool_definitions(registry: &ToolRegistry) -> Vec<serde_json::Value> {
    registry.get_tool_definitions()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_system_prompt_contains_project_info() {
        let ctx = ToolContext::new(PathBuf::from("/tmp/my-project"));
        let registry = ToolRegistry::new();

        let prompt = build_system_prompt(&ctx, &registry);

        assert!(prompt.contains("my-project"));
        assert!(prompt.contains("read"));
        assert!(prompt.contains("write"));
        assert!(prompt.contains("edit"));
        assert!(prompt.contains("bash"));
    }

    #[test]
    fn test_tool_definitions() {
        let registry = ToolRegistry::new();
        let definitions = build_tool_definitions(&registry);

        assert!(!definitions.is_empty());

        // Check that each definition has required fields
        for def in &definitions {
            assert!(def.get("name").is_some());
            assert!(def.get("description").is_some());
            assert!(def.get("input_schema").is_some());
        }
    }

    #[test]
    fn test_parse_single_tool_call() {
        let response = r#"Let me read that file for you.

<tool name="read">
{"file_path": "src/main.rs"}
</tool>

I'll show you the contents."#;

        let calls = parse_tool_calls(response);
        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].name, "read");
        assert_eq!(calls[0].params["file_path"], "src/main.rs");
    }

    #[test]
    fn test_parse_multiple_tool_calls() {
        let response = r#"First, let me search:

<tool name="glob">
{"pattern": "**/*.rs"}
</tool>

Now let me run tests:

<tool name="bash">
{"command": "cargo test"}
</tool>

Done!"#;

        let calls = parse_tool_calls(response);
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].name, "glob");
        assert_eq!(calls[1].name, "bash");
    }

    #[test]
    fn test_extract_text_content() {
        let response = r#"Let me read that file.

<tool name="read">
{"file_path": "test.rs"}
</tool>

Here's what I found."#;

        let text = extract_text_content(response);
        assert!(text.contains("Let me read that file"));
        assert!(text.contains("Here's what I found"));
        assert!(!text.contains("<tool"));
        assert!(!text.contains("file_path"));
    }

    #[test]
    fn test_no_tool_calls() {
        let response = "I can help you with that. What would you like me to do?";
        let calls = parse_tool_calls(response);
        assert!(calls.is_empty());
    }
}
