//! AI-powered suggestion generation and handling

use anyhow::{anyhow, Result};
use log::warn;
use uuid::Uuid;

use super::super::state::AdvancedChatState;
use super::super::types::ChatMessage;

impl AdvancedChatState {
    pub async fn generate_reply_suggestions(&self, session_id: Uuid) -> Result<()> {
        // Get the chat context
        let session = self
            .get_session_by_id(session_id)
            .ok_or_else(|| anyhow!("Session not found"))?;

        // Build context from recent messages (last 10 messages)
        let recent_messages = session
            .messages
            .iter()
            .rev()
            .take(10)
            .rev()
            .map(|msg| match msg {
                ChatMessage::User(text) => format!("User: {}", text),
                ChatMessage::Agent(text) => format!("Agent: {}", text),
                ChatMessage::ToolResult {
                    tool_name, result, ..
                } => {
                    format!(
                        "Tool Result [{}]: {}",
                        tool_name,
                        serde_json::to_string(result).unwrap_or_default()
                    )
                }
                _ => String::new(),
            })
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join("\n");

        if recent_messages.is_empty() {
            return Ok(());
        }

        // Create prompt for AI to generate suggestions
        let prompt = format!(
            "Based on this conversation context:\n{}\n\n\
            Generate exactly 5 short, practical follow-up questions or commands. \
            Focus on blockchain operations, wallet actions, or clarifying questions. \
            Format: Just list them as:\n\
            1. Check wallet balance\n\
            2. View recent transactions\n\
            3. Get network status\n\
            4. Show stake accounts\n\
            5. Export transaction history\n\n\
            Keep each suggestion under 6 words and directly actionable.",
            recent_messages
        );

        // Call AI service to generate suggestions
        match self.ai_service.query_with_debug(&prompt, false).await {
            Ok(response) => {
                // Parse suggestions from response (expecting numbered list)
                let suggestions: Vec<String> = response
                    .lines()
                    .filter_map(|line| {
                        let line = line.trim();
                        // Match lines like "1. suggestion" or "1) suggestion" or just starting with a number
                        if let Some(dot_pos) = line.find('.') {
                            if line[..dot_pos].trim().parse::<u32>().is_ok() {
                                return Some(line[dot_pos + 1..].trim().to_string());
                            }
                        }
                        if let Some(paren_pos) = line.find(')') {
                            if line[..paren_pos].trim().parse::<u32>().is_ok() {
                                return Some(line[paren_pos + 1..].trim().to_string());
                            }
                        }
                        // Also accept lines starting with digits
                        if line.len() > 2 && line.chars().next()?.is_ascii_digit() {
                            if let Some(rest) = line.get(2..) {
                                return Some(rest.trim().to_string());
                            }
                        }
                        None
                    })
                    .take(5)
                    .collect();

                if !suggestions.is_empty() {
                    // Store suggestions
                    if let Ok(mut sugg_guard) = self.current_suggestions.write() {
                        *sugg_guard = suggestions;
                    }
                    if let Ok(mut vis_guard) = self.suggestions_visible.write() {
                        *vis_guard = true;
                    }

                    // Add a system message to show suggestions are available
                    self.add_message_to_session(
                        session_id,
                        ChatMessage::System(
                            "Reply suggestions available (press 1-5 to insert)".to_string(),
                        ),
                    )?;

                    // Note: UI refresh needs to happen from the main thread
                }
            }
            Err(e) => {
                warn!("Failed to generate suggestions: {}", e);
            }
        }

        Ok(())
    }
}
