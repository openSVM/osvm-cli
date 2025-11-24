// Agent communication bridge
// Allows AI agents to interact with BBS

use crate::utils::bbs::{db, commands, models::User};
use diesel::sqlite::SqliteConnection;
use std::sync::Arc;
use tokio::sync::Mutex;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

/// Bridge for AI agent communication with BBS
pub struct AgentBridge {
    conn: Arc<Mutex<SqliteConnection>>,
    agent_node_id: String,
}

impl AgentBridge {
    /// Create a new agent bridge
    pub fn new(conn: SqliteConnection) -> Result<Self> {
        // Create a unique node ID for the AI agent
        let agent_node_id = "!aaaabbbb".to_string(); // Fixed agent ID for now

        Ok(Self {
            conn: Arc::new(Mutex::new(conn)),
            agent_node_id,
        })
    }

    /// Register the AI agent as a BBS user
    pub async fn register_agent(&self) -> Result<User> {
        let mut conn = self.conn.lock().await;
        let timestamp = db::now_as_useconds();

        let (user, is_new) = db::users::observe(
            &mut conn,
            &self.agent_node_id,
            Some("OSVM"),
            Some("OSVM Research Agent"),
            timestamp,
        )?;

        if is_new {
            log::info!("Registered AI agent as BBS user: {}", user);
        }

        Ok(user)
    }

    /// Post a message from the agent
    pub async fn agent_post(&self, board_id: i32, message: &str) -> Result<()> {
        let mut conn = self.conn.lock().await;
        let user = db::users::get(&mut conn, &self.agent_node_id)?;

        db::posts::create(&mut conn, board_id, user.id, message)?;

        log::info!("Agent posted to board {}: {}", board_id, message);
        Ok(())
    }

    /// Execute a command as the agent
    pub async fn agent_command(&self, command: &str) -> Result<commands::CommandResult> {
        let mut conn = self.conn.lock().await;
        let user = db::users::get(&mut conn, &self.agent_node_id)?;

        commands::execute(&mut conn, &user, command)
    }

    /// Get recent posts for agent analysis
    pub async fn get_recent_posts(&self, board_id: i32, limit: i64) -> Result<Vec<String>> {
        let mut conn = self.conn.lock().await;
        let posts = db::posts::list_for_board(&mut conn, board_id, limit)?;

        Ok(posts.iter().map(|p| p.body.clone()).collect())
    }
}
