//! Agent worker for background processing

use anyhow::Result;
use log::{debug, error, info, warn};
use tokio::sync::mpsc;

use super::super::state::AdvancedChatState;
use super::super::types::AgentState;
use super::commands::AgentCommand;

impl AdvancedChatState {
    pub async fn start_agent_worker(&self) -> Result<()> {
        let (sender, mut receiver) = mpsc::unbounded_channel::<AgentCommand>();

        // Store sender for use by UI
        // BUG-2014 fix: This initialization must complete before UI starts sending commands
        // The race window is minimal because:
        // 1. start_agent_worker() is awaited in main
        // 2. UI setup happens after this returns Ok(())
        // 3. send_agent_command_sync() gracefully handles None case (logs error, doesn't crash)
        {
            let mut sender_guard = self.agent_command_sender.lock().await;
            *sender_guard = Some(sender);
            debug!("Agent command sender initialized");
        }

        let state = self.clone();

        // Spawn agent worker task
        tokio::spawn(async move {
            while let Some(command) = receiver.recv().await {
                match command {
                    AgentCommand::ProcessInput { session_id, input } => {
                        if let Err(e) = state.process_input_async(session_id, input).await {
                            error!("Failed to process input for session {}: {}", session_id, e);
                        }
                    }
                    AgentCommand::PauseAgent { session_id } => {
                        state.set_agent_state(session_id, AgentState::Paused);
                    }
                    AgentCommand::ResumeAgent { session_id } => {
                        state.set_agent_state(session_id, AgentState::Idle);
                    }
                    AgentCommand::StopAgent { session_id } => {
                        state.set_agent_state(session_id, AgentState::Idle);
                    }
                    AgentCommand::GetStatus {
                        session_id,
                        response,
                    } => {
                        let status = state
                            .get_agent_state(session_id)
                            .unwrap_or(AgentState::Error("Session not found".to_string()));
                        let _ = response.send(status);
                    }
                    AgentCommand::ThemeCommand {
                        session_id: _,
                        command: _,
                    } => {
                        // Theme commands are handled synchronously in the UI layer
                        // This case should not be reached, but we handle it to avoid compiler errors
                        warn!("Theme command received in background worker - this should be handled in UI layer");
                    }
                }
            }
        });

        // Also start tool refresh worker
        self.start_tool_refresh_worker().await?;

        Ok(())
    }

    pub async fn start_tool_refresh_worker(&self) -> Result<()> {
        let state = self.clone();

        tokio::spawn(async move {
            let mut interval = tokio::time::interval(std::time::Duration::from_secs(30));
            loop {
                interval.tick().await;

                if let Err(e) = state.refresh_tools_from_mcp().await {
                    error!("Failed to refresh tools: {}", e);
                    // If there are persistent errors, consider backing off
                }
            }
        });

        Ok(())
    }
}
