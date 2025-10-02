//! Chat session management and recording functionality

use anyhow::Result;
use chrono::{DateTime, Utc};
use fs2::FileExt;
use log::error;
use serde::{Deserialize, Serialize};
use std::fs::OpenOptions;
use std::io::Write;
use uuid::Uuid; // For file locking

use super::types::{AgentState, ChatMessage};

/// Chat session with full state tracking
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ChatSession {
    pub id: Uuid,
    pub name: String,
    pub created_at: DateTime<Utc>,
    pub messages: Vec<ChatMessage>,
    pub agent_state: AgentState,
    pub recording: bool,
    pub recording_file: Option<String>,
}

impl ChatSession {
    pub fn new(name: String) -> Self {
        Self {
            id: Uuid::new_v4(),
            name,
            created_at: Utc::now(),
            messages: Vec::new(),
            agent_state: AgentState::Idle,
            recording: false,
            recording_file: None,
        }
    }

    pub fn add_message(&mut self, message: ChatMessage) {
        self.messages.push(message);

        // Limit message history to prevent memory growth
        if self.messages.len() > 1000 {
            self.messages.drain(0..100); // Remove oldest 100 messages
        }

        // Save to recording if active
        if self.recording {
            if let Some(last_message) = self.messages.last() {
                if let Err(e) = self.save_message_to_recording(last_message) {
                    error!("Failed to save message to recording: {}", e);
                }
            }
        }

        // NEW: Log to ClickHouse if available (async operation, fire-and-forget)
        if let Some(last_message) = self.messages.last() {
            let session_clone = self.clone();
            let message_clone = last_message.clone();
            
            tokio::spawn(async move {
                // Try to get global activity logger if ClickHouse is running
                if let Ok(service) = crate::services::clickhouse_service::ClickHouseService::new() {
                    if let Ok(status) = service.status().await {
                        if status == crate::services::clickhouse_service::ClickHouseStatus::Running {
                            let logger = crate::services::activity_logger::ActivityLogger::new(std::sync::Arc::new(service));
                            let _ = logger.log_chat_message(&session_clone, &message_clone).await;
                        }
                    }
                }
            });
        }
    }

    fn save_message_to_recording(&self, message: &ChatMessage) -> Result<()> {
        if let Some(file_path) = &self.recording_file {
            // CRITICAL FIX: Use file locking to prevent corruption from concurrent writes
            use fs2::FileExt;

            let mut file = OpenOptions::new()
                .create(true)
                .append(true)
                .open(file_path)?;

            // Acquire exclusive lock (blocks until available)
            file.lock_exclusive()?;

            let timestamp = Utc::now().format("%Y-%m-%d %H:%M:%S UTC");
            let message_json = serde_json::to_string(message)?;
            writeln!(file, "[{}] {}", timestamp, message_json)?;

            // Force to disk before releasing lock
            file.sync_all()?;

            // Lock automatically released when file is dropped
            file.unlock()?;
        }
        Ok(())
    }

    pub fn start_recording(&mut self, file_path: String) -> Result<()> {
        // Create recording file with header first
        use std::fs::File;

        let mut file = File::create(&file_path)?;
        writeln!(file, "# OSVM Agent Chat Session Recording")?;
        writeln!(file, "# Session: {} ({})", self.name, self.id)?;
        writeln!(
            file,
            "# Started: {}",
            Utc::now().format("%Y-%m-%d %H:%M:%S UTC")
        )?;
        writeln!(file, "# Format: [timestamp] {{message_json}}")?;
        writeln!(file, "")?;

        // Now enable recording and add the message
        self.recording = true;
        self.recording_file = Some(file_path.clone());
        self.add_message(ChatMessage::System(format!(
            "Recording started: {}",
            file_path
        )));
        Ok(())
    }

    pub fn stop_recording(&mut self) {
        if self.recording {
            self.add_message(ChatMessage::System("Recording stopped".to_string()));
            self.recording = false;
            self.recording_file = None;
        }
    }
}
