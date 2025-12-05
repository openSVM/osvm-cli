//! Code agent - orchestrates AI interactions and tool execution

use super::permissions::{ApprovalRequest, ApprovalResponse, PermissionManager};
use super::prompt::build_system_prompt;
use super::tools::{Tool, ToolContext, ToolOutput, ToolRegistry};
use crate::services::ai_service::AiService;
use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::Arc;
use tokio::sync::mpsc;

/// Messages from the agent to the UI
#[derive(Debug, Clone)]
pub enum AgentMessage {
    /// Thinking/reasoning text
    Thinking(String),
    /// Final response text
    Response(String),
    /// Tool is being called
    ToolCall { name: String, params: Value },
    /// Tool completed
    ToolResult { name: String, output: String, success: bool },
    /// Approval needed
    ApprovalNeeded(ApprovalRequest),
    /// Error occurred
    Error(String),
    /// Agent finished
    Done,
}

/// Messages from the UI to the agent
#[derive(Debug, Clone)]
pub enum UserMessage {
    /// User input/prompt
    Input(String),
    /// Approval response
    Approval(String, ApprovalResponse), // (request_id, response)
    /// Cancel current operation
    Cancel,
}

/// Tool call from AI response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolCall {
    pub id: String,
    pub name: String,
    #[serde(rename = "input")]
    pub params: Value,
}

/// The code agent handles AI interactions
pub struct CodeAgent {
    pub registry: ToolRegistry,
    pub permissions: PermissionManager,
    pub context: ToolContext,
    ai_service: Arc<AiService>,
    conversation_history: Vec<ConversationMessage>,
}

#[derive(Debug, Clone, Serialize)]
struct ConversationMessage {
    role: String,
    content: String,
}

impl CodeAgent {
    pub fn new(project_root: std::path::PathBuf) -> Self {
        Self {
            registry: ToolRegistry::new(),
            permissions: PermissionManager::new(),
            context: ToolContext::new(project_root),
            ai_service: Arc::new(AiService::new()),
            conversation_history: Vec::new(),
        }
    }

    /// Process a user message and send responses via channel
    pub async fn process_message(
        &mut self,
        message: &str,
        tx: mpsc::Sender<AgentMessage>,
    ) -> Result<()> {
        // Add user message to history
        self.conversation_history.push(ConversationMessage {
            role: "user".to_string(),
            content: message.to_string(),
        });

        // Send thinking indicator
        let _ = tx.send(AgentMessage::Thinking("Analyzing your request...".to_string())).await;

        // Build prompt with context
        let system_prompt = build_system_prompt(&self.context, &self.registry);

        // Build full prompt with conversation history
        let full_prompt = self.build_full_prompt(&system_prompt, message);

        // Query AI using existing method
        let response = self.ai_service.query_with_debug(&full_prompt, false).await?;

        // Send response
        let _ = tx.send(AgentMessage::Response(response.clone())).await;

        // Add to history
        self.conversation_history.push(ConversationMessage {
            role: "assistant".to_string(),
            content: response,
        });

        let _ = tx.send(AgentMessage::Done).await;
        Ok(())
    }

    fn build_full_prompt(&self, system_prompt: &str, user_message: &str) -> String {
        let mut prompt = String::new();

        // Add system context
        prompt.push_str("System Context:\n");
        prompt.push_str(system_prompt);
        prompt.push_str("\n\n");

        // Add conversation history (last 5 exchanges)
        let history_start = self.conversation_history.len().saturating_sub(10);
        for msg in &self.conversation_history[history_start..] {
            prompt.push_str(&format!("{}: {}\n\n", msg.role.to_uppercase(), msg.content));
        }

        // Add current message
        prompt.push_str(&format!("USER: {}\n\nASSISTANT:", user_message));

        prompt
    }

    /// Execute a tool by name
    pub async fn execute_tool(
        &mut self,
        name: &str,
        params: &Value,
        tx: Option<mpsc::Sender<AgentMessage>>,
    ) -> Result<ToolOutput> {
        let tool = self
            .registry
            .get(name)
            .ok_or_else(|| anyhow::anyhow!("Unknown tool: {}", name))?;

        // Send tool call notification
        if let Some(ref tx) = tx {
            let _ = tx
                .send(AgentMessage::ToolCall {
                    name: name.to_string(),
                    params: params.clone(),
                })
                .await;
        }

        // Check permissions
        if self.permissions.needs_approval(tool.as_ref(), params) {
            let preview = tool.generate_preview(params, &self.context);
            let _request = ApprovalRequest::new(name, params.clone(), tool.risk_level(), preview);

            // For now, auto-approve (full approval flow would need UI integration)
            log::info!("Tool {} would need approval (auto-approving for now)", name);
        }

        // Execute tool
        let result = tool.execute(params.clone(), &self.context).await;

        if let Some(ref tx) = tx {
            match &result {
                Ok(output) => {
                    let _ = tx
                        .send(AgentMessage::ToolResult {
                            name: name.to_string(),
                            output: output.text.clone(),
                            success: true,
                        })
                        .await;
                }
                Err(e) => {
                    let _ = tx
                        .send(AgentMessage::ToolResult {
                            name: name.to_string(),
                            output: e.to_string(),
                            success: false,
                        })
                        .await;
                }
            }
        }

        result
    }

    /// Clear conversation history
    pub fn clear_history(&mut self) {
        self.conversation_history.clear();
    }

    /// Get conversation history length
    pub fn history_len(&self) -> usize {
        self.conversation_history.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_agent_creation() {
        let agent = CodeAgent::new(PathBuf::from("/tmp/test"));

        assert!(agent.registry.get("read").is_some());
        assert!(agent.registry.get("write").is_some());
        assert_eq!(agent.history_len(), 0);
    }
}
