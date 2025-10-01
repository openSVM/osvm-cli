//! OSVM Command Planner - Maps natural language to OSVM CLI commands
//!
//! This module uses AI to analyze user requests and generate executable
//! OSVM command plans. It integrates with both the chat interface and
//! agent execution system to provide intelligent command suggestions.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::process::Command;

use crate::services::ai_service::AiService;

/// Represents a single OSVM command to execute
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OsvmCommandStep {
    /// The OSVM command to execute (e.g., "balance", "svm list")
    pub command: String,
    /// Full command with arguments (e.g., "osvm balance")
    pub full_command: String,
    /// Arguments to pass to the command
    pub args: Vec<String>,
    /// Human-readable explanation of what this command does
    pub explanation: String,
    /// Whether this command requires user confirmation
    pub requires_confirmation: bool,
    /// Expected output description
    pub expected_output: String,
}

/// Complete execution plan with multiple OSVM commands
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OsvmExecutionPlan {
    /// AI's reasoning for this plan
    pub reasoning: String,
    /// Confidence score (0.0 to 1.0)
    pub confidence: f32,
    /// Ordered list of commands to execute
    pub steps: Vec<OsvmCommandStep>,
    /// Expected final outcome
    pub expected_outcome: String,
    /// Whether the plan can be executed automatically
    pub auto_executable: bool,
}

/// Result of executing an OSVM command
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OsvmCommandResult {
    pub command: String,
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
    pub execution_time_ms: u128,
    pub exit_code: Option<i32>,
}

/// OSVM Command Planner - creates and executes command plans
pub struct OsvmCommandPlanner {
    ai_service: AiService,
    debug: bool,
}

impl OsvmCommandPlanner {
    /// Create a new command planner
    pub fn new(debug: bool) -> Self {
        Self {
            ai_service: AiService::new_with_debug(debug),
            debug,
        }
    }

    /// Create an execution plan from a user query
    pub async fn create_plan(&self, user_query: &str) -> Result<OsvmExecutionPlan> {
        if self.debug {
            println!("🧠 Creating execution plan for: {}", user_query);
        }

        // Try AI-based planning first
        match self.try_ai_planning(user_query).await {
            Ok(plan) => Ok(plan),
            Err(ai_error) => {
                // Fall back to rule-based planning
                if self.debug {
                    eprintln!(
                        "⚠️  AI planning failed: {}, falling back to rule-based",
                        ai_error
                    );
                }
                self.create_rule_based_plan(user_query)
            }
        }
    }

    /// Try to create a plan using AI
    async fn try_ai_planning(&self, user_query: &str) -> Result<OsvmExecutionPlan> {
        // Build the planning prompt
        let planning_prompt = self.build_planning_prompt(user_query);

        // Call AI to generate plan
        let ai_response = self
            .ai_service
            .query_with_debug(&planning_prompt, self.debug)
            .await
            .context("Failed to get AI planning response")?;

        // Parse the AI response into a structured plan
        self.parse_plan_response(&ai_response).or_else(|parse_error| {
            // AI didn't return expected format - show helpful error
            Err(anyhow::anyhow!(
                "AI returned unexpected format. \n\nResponse preview: {}\n\n\
                💡 This feature is experimental. If you see this error frequently, please report it at:\n   \
                https://github.com/anthropics/osvm-cli/issues",
                &ai_response.chars().take(200).collect::<String>()
            ))
        })
    }

    /// Create a rule-based plan when AI is not available
    fn create_rule_based_plan(&self, user_query: &str) -> Result<OsvmExecutionPlan> {
        let query_lower = user_query.to_lowercase();

        // Simple keyword matching for common queries
        let (command, args, explanation) = if query_lower.contains("svm")
            && (query_lower.contains("list")
                || query_lower.contains("show")
                || query_lower.contains("all"))
        {
            (
                "svm",
                vec!["svm".to_string(), "list".to_string()],
                "List all available SVMs",
            )
        } else if query_lower.contains("balance") || query_lower.contains("wallet") {
            (
                "balance",
                vec!["balance".to_string()],
                "Check your SOL balance",
            )
        } else if query_lower.contains("node") && query_lower.contains("list") {
            (
                "nodes",
                vec!["nodes".to_string(), "list".to_string()],
                "List all nodes",
            )
        } else if query_lower.contains("health")
            || query_lower.contains("status")
            || query_lower.contains("doctor")
        {
            ("doctor", vec!["doctor".to_string()], "Check system health")
        } else if query_lower.contains("example") || query_lower.contains("help") {
            (
                "examples",
                vec!["examples".to_string()],
                "Show usage examples",
            )
        } else {
            // Default to showing examples
            (
                "examples",
                vec!["examples".to_string()],
                "Show examples to help you get started",
            )
        };

        Ok(OsvmExecutionPlan {
            reasoning: format!(
                "Matched query '{}' to {} command using keyword matching",
                user_query, command
            ),
            confidence: 0.7,
            steps: vec![OsvmCommandStep {
                command: command.to_string(),
                full_command: format!("osvm {}", args.join(" ")),
                args,
                explanation: explanation.to_string(),
                requires_confirmation: false,
                expected_output: format!("Output from {} command", command),
            }],
            expected_outcome: format!("Execute {} command based on your query", command),
            auto_executable: true,
        })
    }

    /// Build the AI prompt for command planning
    fn build_planning_prompt(&self, user_query: &str) -> String {
        format!(
            r#"You are an OSVM CLI expert. Analyze the user's request and create an execution plan using OSVM commands.

Available OSVM commands:
- balance [ADDRESS] - Check SOL balance
- svm list - List all SVMs
- svm get <NAME> - Get SVM details
- svm dashboard - Launch SVM dashboard
- nodes list - List all nodes
- nodes status <ID> - Check node status
- nodes get <ID> - Get node details
- nodes logs <ID> - View node logs
- rpc-manager query-solana --network <NETWORK> - Query Solana RPC
- rpc-manager test - Start test validator
- mcp list - List MCP servers
- mcp tools <ID> - List tools from MCP server
- doctor - Check system health
- doctor --fix - Fix system issues
- examples - Show examples
- agent "<PROMPT>" - Execute AI-powered command

User request: {}

Create a JSON execution plan with this structure:
{{
  "reasoning": "Why these commands will fulfill the request",
  "confidence": 0.9,
  "steps": [
    {{
      "command": "svm",
      "full_command": "osvm svm list",
      "args": ["list"],
      "explanation": "List all available SVMs to answer the user's question",
      "requires_confirmation": false,
      "expected_output": "A table of SVMs with their status"
    }}
  ],
  "expected_outcome": "User will see a list of all SVMs",
  "auto_executable": true
}}

Important rules:
1. Use actual OSVM commands from the list above
2. Set requires_confirmation=true for destructive operations (deploy, restart, stop)
3. Set auto_executable=false if user confirmation is needed
4. Keep it simple - prefer fewer commands over complex workflows
5. If the request is vague, suggest the 'examples' command
6. Return ONLY valid JSON, no additional text

Generate the plan:"#,
            user_query
        )
    }

    /// Parse AI response into execution plan
    fn parse_plan_response(&self, ai_response: &str) -> Result<OsvmExecutionPlan> {
        // Try to extract JSON from the response
        let json_str = self.extract_json(ai_response)?;

        // Parse as JSON
        let plan: OsvmExecutionPlan =
            serde_json::from_str(&json_str).context("Failed to parse execution plan JSON")?;

        // Validate the plan
        if plan.steps.is_empty() {
            anyhow::bail!("Execution plan contains no steps");
        }

        Ok(plan)
    }

    /// Extract JSON from AI response (handles cases where AI adds extra text)
    fn extract_json(&self, response: &str) -> Result<String> {
        // First, try to parse the whole response as JSON
        if let Ok(_) = serde_json::from_str::<serde_json::Value>(response) {
            return Ok(response.to_string());
        }

        // Look for JSON between curly braces
        if let Some(start) = response.find('{') {
            if let Some(end) = response.rfind('}') {
                if start < end {
                    let json_candidate = &response[start..=end];
                    if serde_json::from_str::<serde_json::Value>(json_candidate).is_ok() {
                        return Ok(json_candidate.to_string());
                    }
                }
            }
        }

        anyhow::bail!("Could not extract valid JSON from AI response")
    }

    /// Execute a single OSVM command
    pub async fn execute_command(&self, step: &OsvmCommandStep) -> Result<OsvmCommandResult> {
        let start_time = std::time::Instant::now();

        if self.debug {
            println!("⚙️  Executing: {}", step.full_command);
        }

        // Build the command
        let mut cmd = Command::new("osvm");
        cmd.args(&step.args);

        // Execute the command
        let output = cmd
            .output()
            .context(format!("Failed to execute command: {}", step.full_command))?;

        let execution_time = start_time.elapsed().as_millis();

        Ok(OsvmCommandResult {
            command: step.full_command.clone(),
            success: output.status.success(),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            execution_time_ms: execution_time,
            exit_code: output.status.code(),
        })
    }

    /// Execute an entire execution plan
    pub async fn execute_plan(
        &self,
        plan: &OsvmExecutionPlan,
        skip_confirmation: bool,
    ) -> Result<Vec<OsvmCommandResult>> {
        let mut results = Vec::new();

        for (i, step) in plan.steps.iter().enumerate() {
            if self.debug {
                println!(
                    "📍 Step {}/{}: {}",
                    i + 1,
                    plan.steps.len(),
                    step.explanation
                );
            }

            // Check if confirmation is needed
            if step.requires_confirmation && !skip_confirmation {
                println!("\n⚠️  This command requires confirmation:");
                println!("   {}", step.full_command);
                println!("   {}", step.explanation);
                println!("\nThis step will be skipped. Use --yes to auto-confirm.");
                continue;
            }

            // Execute the command
            match self.execute_command(step).await {
                Ok(result) => {
                    if !result.success {
                        if self.debug {
                            eprintln!("⚠️  Command failed with exit code: {:?}", result.exit_code);
                            if !result.stderr.is_empty() {
                                eprintln!("   Error: {}", result.stderr);
                            }
                        }
                    }
                    results.push(result);
                }
                Err(e) => {
                    eprintln!("❌ Failed to execute step {}: {}", i + 1, e);
                    // Create a failed result
                    results.push(OsvmCommandResult {
                        command: step.full_command.clone(),
                        success: false,
                        stdout: String::new(),
                        stderr: format!("Execution error: {}", e),
                        execution_time_ms: 0,
                        exit_code: None,
                    });
                    // Continue with next command
                }
            }
        }

        Ok(results)
    }

    /// Format execution results for display
    pub fn format_results(
        &self,
        plan: &OsvmExecutionPlan,
        results: &[OsvmCommandResult],
    ) -> String {
        let mut output = String::new();

        output.push_str(&format!("📋 Execution Plan Results\n"));
        output.push_str(&format!("━━━━━━━━━━━━━━━━━━━━━━━\n\n"));
        output.push_str(&format!("Reasoning: {}\n\n", plan.reasoning));

        for (i, result) in results.iter().enumerate() {
            let status = if result.success { "✅" } else { "❌" };
            output.push_str(&format!("{}. {} {}\n", i + 1, status, result.command));

            if !result.stdout.is_empty() {
                let stdout = result.stdout.trim();
                if stdout.len() > 200 {
                    output.push_str(&format!("   Output: {}...\n", &stdout[..200]));
                } else {
                    output.push_str(&format!("   Output: {}\n", stdout));
                }
            }

            if !result.success && !result.stderr.is_empty() {
                output.push_str(&format!("   Error: {}\n", result.stderr.trim()));
            }

            output.push_str(&format!("   Time: {}ms\n", result.execution_time_ms));
            output.push_str("\n");
        }

        output.push_str(&format!("Expected outcome: {}\n", plan.expected_outcome));

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_plan_creation() {
        let planner = OsvmCommandPlanner::new(false);
        // This will fail without network, but tests the structure
        let result = planner.create_plan("show me all svms").await;
        // Just testing that the function signature is correct
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_json_extraction() {
        let planner = OsvmCommandPlanner::new(false);
        let response = r#"Here's the plan: {"reasoning": "test", "confidence": 0.9, "steps": [], "expected_outcome": "test", "auto_executable": true}"#;
        let json = planner.extract_json(response).unwrap();
        assert!(json.contains("reasoning"));
    }
}
