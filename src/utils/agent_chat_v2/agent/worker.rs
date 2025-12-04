//! Agent worker for background processing

use anyhow::Result;
use log::{debug, error, info, warn};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;

use super::super::state::AdvancedChatState;
use super::super::types::AgentState;
use super::commands::AgentCommand;

/// Simple circuit breaker for tool refresh
/// Uses exponential backoff with jitter for resilience
struct ToolRefreshCircuitBreaker {
    consecutive_failures: AtomicU32,
    max_failures: u32,
    base_interval: Duration,
    max_interval: Duration,
}

impl ToolRefreshCircuitBreaker {
    fn new() -> Self {
        Self {
            consecutive_failures: AtomicU32::new(0),
            max_failures: 5,
            base_interval: Duration::from_secs(30),
            max_interval: Duration::from_secs(300), // 5 minutes max
        }
    }

    /// Record a failure and return the recommended backoff interval
    fn on_failure(&self) -> Duration {
        let failures = self.consecutive_failures.fetch_add(1, Ordering::SeqCst) + 1;

        if failures >= self.max_failures {
            // Circuit is "open" - use max interval
            warn!(
                "Tool refresh circuit breaker open after {} consecutive failures",
                failures
            );
            self.max_interval
        } else {
            // Exponential backoff: base * 2^(failures-1) with jitter
            let backoff_secs = self.base_interval.as_secs() * (1u64 << (failures - 1).min(4));
            let jitter = (backoff_secs / 4).max(1); // 25% jitter
            let jittered = backoff_secs + (rand::random::<u64>() % jitter);
            Duration::from_secs(jittered.min(self.max_interval.as_secs()))
        }
    }

    /// Record a success - reset the failure count
    fn on_success(&self) {
        let prev = self.consecutive_failures.swap(0, Ordering::SeqCst);
        if prev > 0 {
            info!("Tool refresh circuit breaker recovered after {} failures", prev);
        }
    }

    /// Check if the circuit is "open" (too many failures)
    fn is_open(&self) -> bool {
        self.consecutive_failures.load(Ordering::SeqCst) >= self.max_failures
    }

    /// Get current failure count
    fn failure_count(&self) -> u32 {
        self.consecutive_failures.load(Ordering::SeqCst)
    }
}

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
        let circuit_breaker = Arc::new(ToolRefreshCircuitBreaker::new());

        tokio::spawn(async move {
            let mut current_interval = Duration::from_secs(30);

            loop {
                tokio::time::sleep(current_interval).await;

                // Check if circuit breaker is open
                if circuit_breaker.is_open() {
                    debug!(
                        "Tool refresh circuit breaker is open ({} failures), using max interval",
                        circuit_breaker.failure_count()
                    );
                }

                match state.refresh_tools_from_mcp().await {
                    Ok(()) => {
                        circuit_breaker.on_success();
                        current_interval = Duration::from_secs(30); // Reset to base interval
                    }
                    Err(e) => {
                        error!("Failed to refresh tools: {}", e);
                        current_interval = circuit_breaker.on_failure();
                        debug!(
                            "Tool refresh backoff: {} seconds (failures: {})",
                            current_interval.as_secs(),
                            circuit_breaker.failure_count()
                        );
                    }
                }
            }
        });

        Ok(())
    }
}
