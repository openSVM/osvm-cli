// Database operations for BBS
// Adapted from FrozenBBS

use diesel::prelude::*;
use diesel::sqlite::SqliteConnection;
use std::path::Path;

pub mod users;
pub mod boards;
pub mod posts;
pub mod queued_messages;

use crate::utils::bbs::db_path;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

/// Establish connection to BBS database
pub fn establish_connection() -> Result<SqliteConnection> {
    let db_path = db_path();

    // Ensure parent directory exists
    if let Some(parent) = db_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let database_url = db_path.to_str().ok_or("Invalid database path")?;
    let mut connection = SqliteConnection::establish(database_url)?;

    // Enable foreign keys
    diesel::sql_query("PRAGMA foreign_keys = ON")
        .execute(&mut connection)?;

    Ok(connection)
}

/// Initialize BBS database with schema
pub fn initialize_database(conn: &mut SqliteConnection) -> Result<()> {
    diesel::sql_query(
        r#"
        CREATE TABLE IF NOT EXISTS boards (
            id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            name TEXT NOT NULL UNIQUE,
            description TEXT NOT NULL,
            created_at_us BIGINT NOT NULL
        );

        CREATE TABLE IF NOT EXISTS users (
            id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            node_id TEXT NOT NULL UNIQUE,
            short_name TEXT NOT NULL,
            long_name TEXT NOT NULL,
            jackass BOOL NOT NULL DEFAULT FALSE,
            in_board INTEGER,
            created_at_us BIGINT NOT NULL,
            last_seen_at_us BIGINT NOT NULL,
            last_acted_at_us BIGINT,
            bio TEXT,
            FOREIGN KEY (in_board) REFERENCES boards (id)
        );

        CREATE TABLE IF NOT EXISTS posts (
            id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            board_id INTEGER NOT NULL,
            user_id INTEGER NOT NULL,
            body TEXT NOT NULL,
            created_at_us BIGINT NOT NULL,
            UNIQUE(created_at_us),
            FOREIGN KEY (user_id) REFERENCES users (id),
            FOREIGN KEY (board_id) REFERENCES boards (id)
        );

        CREATE TABLE IF NOT EXISTS board_states (
            id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            user_id INTEGER NOT NULL,
            board_id INTEGER NOT NULL,
            last_post_us BIGINT NOT NULL,
            FOREIGN KEY (board_id) REFERENCES boards (id),
            FOREIGN KEY (user_id) REFERENCES users (id)
        );

        CREATE TABLE IF NOT EXISTS queued_messages (
            id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            sender_id INTEGER NOT NULL,
            recipient_id INTEGER NOT NULL,
            body TEXT NOT NULL,
            created_at_us BIGINT NOT NULL,
            sent_at_us BIGINT,
            FOREIGN KEY (sender_id) REFERENCES users (id),
            FOREIGN KEY (recipient_id) REFERENCES users (id)
        );
        "#,
    )
    .execute(conn)?;

    Ok(())
}

/// Get current timestamp in microseconds
pub fn now_as_useconds() -> i64 {
    chrono::Utc::now().timestamp_micros()
}
