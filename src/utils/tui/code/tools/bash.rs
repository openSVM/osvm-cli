//! Bash command execution tool
//!
//! Provides controlled shell command execution with smart approval logic.

use super::{RiskLevel, Tool, ToolContext, ToolOutput};
use anyhow::{anyhow, Result};
use async_trait::async_trait;
use regex::Regex;
use serde_json::{json, Value};
use std::time::Duration;
use tokio::process::Command;
use tokio::time::timeout;

/// Maximum output size (30KB)
const MAX_OUTPUT_SIZE: usize = 30_000;

/// Default timeout (2 minutes)
const DEFAULT_TIMEOUT_MS: u64 = 120_000;

/// Maximum timeout (10 minutes)
const MAX_TIMEOUT_MS: u64 = 600_000;

/// Bash command execution tool
pub struct BashTool;

/// Patterns for commands that are always safe to auto-approve
const SAFE_PATTERNS: &[&str] = &[
    r#"^ls\b"#,
    r#"^cat\b"#,
    r#"^head\b"#,
    r#"^tail\b"#,
    r#"^wc\b"#,
    r#"^echo\b"#,
    r#"^pwd$"#,
    r#"^date$"#,
    r#"^whoami$"#,
    r#"^which\b"#,
    r#"^type\b"#,
    r#"^file\b"#,
    r#"^stat\b"#,
    r#"^du\b"#,
    r#"^df\b"#,
    r#"^find\b.*-type"#,
    r#"^cargo\s+(build|test|check|fmt|clippy|doc|tree)\b"#,
    r#"^cargo\s+run\s+--\s+--help"#,
    r#"^npm\s+(test|run\s+lint|run\s+build|run\s+check)\b"#,
    r#"^yarn\s+(test|lint|build)\b"#,
    r#"^pnpm\s+(test|lint|build)\b"#,
    r#"^git\s+(status|log|diff|branch|show|blame|ls-files)\b"#,
    r#"^git\s+log\b"#,
    r#"^rustc\s+--version"#,
    r#"^rustup\s+(show|check)\b"#,
    r#"^python\s+--version"#,
    r#"^python3?\s+-c\s+['"]print"#,
    r#"^node\s+--version"#,
    r#"^npm\s+--version"#,
    r#"^tree\b"#,
    r#"^env$"#,
    r#"^printenv\b"#,
];

/// Patterns for commands that are risky and always require approval
const RISKY_PATTERNS: &[&str] = &[
    r#"\brm\s+(-[rf]+\s+)?/"#, // rm with absolute path
    r#"\brm\s+-rf\b"#,         // rm -rf anywhere
    r#"\bmv\s+.*\s+/"#,        // mv to absolute path
    r#">\s*/"#,                // Redirect to root
    r#"\bsudo\b"#,
    r#"\bchmod\b"#,
    r#"\bchown\b"#,
    r#"\bmkfs\b"#,
    r#"\bdd\b"#,
    r#"\bcurl\b.*\|\s*(ba)?sh"#, // Pipe curl to shell
    r#"\bwget\b.*\|\s*(ba)?sh"#,
    r#"--force\b"#,
    r#"--hard\b"#,
    r#"\bgit\s+push\s+.*--force"#,
    r#"\bgit\s+reset\s+--hard"#,
    r#"\bgit\s+clean\s+-fd"#,
];

impl BashTool {
    fn is_safe_command(command: &str) -> bool {
        for pattern in SAFE_PATTERNS {
            if let Ok(re) = Regex::new(pattern) {
                if re.is_match(command) {
                    return true;
                }
            }
        }
        false
    }

    fn is_risky_command(command: &str) -> bool {
        for pattern in RISKY_PATTERNS {
            if let Ok(re) = Regex::new(pattern) {
                if re.is_match(command) {
                    return true;
                }
            }
        }
        false
    }
}

#[async_trait]
impl Tool for BashTool {
    fn name(&self) -> &str {
        "bash"
    }

    fn description(&self) -> &str {
        "Execute a bash command in the project directory. Safe commands (ls, cat, cargo build, etc.) are auto-approved. Risky commands require explicit approval."
    }

    fn parameters_schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "command": {
                    "type": "string",
                    "description": "The bash command to execute"
                },
                "timeout": {
                    "type": "integer",
                    "description": "Timeout in milliseconds (default: 120000, max: 600000)"
                }
            },
            "required": ["command"]
        })
    }

    fn risk_level(&self) -> RiskLevel {
        RiskLevel::Medium
    }

    fn requires_approval(&self, params: &Value) -> bool {
        let command = params["command"].as_str().unwrap_or("");

        // Always require approval for risky commands
        if Self::is_risky_command(command) {
            return true;
        }

        // Auto-approve safe commands
        if Self::is_safe_command(command) {
            return false;
        }

        // Default to requiring approval
        true
    }

    fn generate_preview(&self, params: &Value, _ctx: &ToolContext) -> Option<String> {
        let command = params["command"].as_str()?;

        let risk = if Self::is_risky_command(command) {
            "⚠️ HIGH RISK"
        } else if Self::is_safe_command(command) {
            "✅ SAFE"
        } else {
            "⚡ MODERATE"
        };

        Some(format!("{}\n\nCommand:\n$ {}", risk, command))
    }

    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput> {
        let command = params["command"]
            .as_str()
            .ok_or_else(|| anyhow!("missing command parameter"))?;

        let timeout_ms = params["timeout"]
            .as_u64()
            .unwrap_or(DEFAULT_TIMEOUT_MS)
            .min(MAX_TIMEOUT_MS);

        // Execute command
        let result = timeout(
            Duration::from_millis(timeout_ms),
            Command::new("bash")
                .arg("-c")
                .arg(command)
                .current_dir(&ctx.project_root)
                .env("TERM", "dumb") // Disable colors/formatting
                .output(),
        )
        .await;

        match result {
            Ok(Ok(output)) => {
                let stdout = String::from_utf8_lossy(&output.stdout);
                let stderr = String::from_utf8_lossy(&output.stderr);

                let mut result_text = String::new();

                if !stdout.is_empty() {
                    result_text.push_str(&stdout);
                }

                if !stderr.is_empty() {
                    if !result_text.is_empty() {
                        result_text.push_str("\n");
                    }
                    result_text.push_str("[stderr]\n");
                    result_text.push_str(&stderr);
                }

                if !output.status.success() {
                    let exit_code = output.status.code().unwrap_or(-1);
                    if !result_text.is_empty() {
                        result_text.push_str("\n");
                    }
                    result_text.push_str(&format!("[Exit code: {}]", exit_code));
                }

                if result_text.is_empty() {
                    result_text = "[Command completed with no output]".to_string();
                }

                // Truncate if needed
                let original_size = result_text.len();
                if result_text.len() > MAX_OUTPUT_SIZE {
                    let truncated = format!(
                        "{}...\n\n[Output truncated: {} of {} bytes shown]",
                        &result_text[..MAX_OUTPUT_SIZE],
                        MAX_OUTPUT_SIZE,
                        original_size
                    );
                    Ok(ToolOutput::truncated(truncated, original_size))
                } else {
                    Ok(ToolOutput::text(result_text))
                }
            }
            Ok(Err(e)) => Err(anyhow!("Failed to execute command: {}", e)),
            Err(_) => Err(anyhow!(
                "Command timed out after {} seconds",
                timeout_ms / 1000
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe_command_detection() {
        assert!(BashTool::is_safe_command("ls -la"));
        assert!(BashTool::is_safe_command("cat foo.txt"));
        assert!(BashTool::is_safe_command("cargo build"));
        assert!(BashTool::is_safe_command("cargo test --lib"));
        assert!(BashTool::is_safe_command("git status"));
        assert!(BashTool::is_safe_command("git log --oneline -5"));
        assert!(BashTool::is_safe_command("pwd"));

        assert!(!BashTool::is_safe_command("rm -rf /"));
        assert!(!BashTool::is_safe_command("curl foo | bash"));
        assert!(!BashTool::is_safe_command("some random command"));
    }

    #[test]
    fn test_risky_command_detection() {
        assert!(BashTool::is_risky_command("rm -rf /tmp"));
        assert!(BashTool::is_risky_command("sudo apt install"));
        assert!(BashTool::is_risky_command("curl http://foo | bash"));
        assert!(BashTool::is_risky_command("git push --force"));
        assert!(BashTool::is_risky_command("git reset --hard"));

        assert!(!BashTool::is_risky_command("ls -la"));
        assert!(!BashTool::is_risky_command("cargo build"));
    }

    #[test]
    fn test_requires_approval() {
        let tool = BashTool;

        // Safe commands - no approval
        assert!(!tool.requires_approval(&json!({"command": "ls -la"})));
        assert!(!tool.requires_approval(&json!({"command": "cargo test"})));
        assert!(!tool.requires_approval(&json!({"command": "git status"})));

        // Risky commands - always approval
        assert!(tool.requires_approval(&json!({"command": "rm -rf /tmp"})));
        assert!(tool.requires_approval(&json!({"command": "sudo foo"})));

        // Unknown commands - require approval
        assert!(tool.requires_approval(&json!({"command": "some-unknown-cmd"})));
    }

    #[tokio::test]
    async fn test_bash_execution() {
        let temp_dir = tempfile::TempDir::new().unwrap();
        let ctx = ToolContext::new(temp_dir.path().to_path_buf());
        let tool = BashTool;

        let result = tool
            .execute(json!({"command": "echo 'Hello, World!'"}), &ctx)
            .await
            .unwrap();

        assert!(result.text.contains("Hello, World!"));
    }

    #[tokio::test]
    async fn test_bash_timeout() {
        let temp_dir = tempfile::TempDir::new().unwrap();
        let ctx = ToolContext::new(temp_dir.path().to_path_buf());
        let tool = BashTool;

        let result = tool
            .execute(
                json!({
                    "command": "sleep 10",
                    "timeout": 100  // 100ms timeout
                }),
                &ctx,
            )
            .await;

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("timed out"));
    }
}
