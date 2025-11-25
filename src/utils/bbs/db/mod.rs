// Database operations for BBS
// Adapted from FrozenBBS

use diesel::prelude::*;
use diesel::sqlite::SqliteConnection;
use std::path::Path;

pub mod users;
pub mod boards;
pub mod posts;
pub mod queued_messages;
pub mod moderators;

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
/// Note: Diesel's sql_query only supports single statements, so we execute each separately
pub fn initialize_database(conn: &mut SqliteConnection) -> Result<()> {
    // Create boards table (with creator_id for ownership)
    diesel::sql_query(
        r#"CREATE TABLE IF NOT EXISTS boards (
            id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            name TEXT NOT NULL UNIQUE,
            description TEXT NOT NULL,
            created_at_us BIGINT NOT NULL,
            creator_id INTEGER
        )"#,
    )
    .execute(conn)?;

    // Create users table
    diesel::sql_query(
        r#"CREATE TABLE IF NOT EXISTS users (
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
        )"#,
    )
    .execute(conn)?;

    // Create posts table (with parent_id for reply threading)
    diesel::sql_query(
        r#"CREATE TABLE IF NOT EXISTS posts (
            id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            board_id INTEGER NOT NULL,
            user_id INTEGER NOT NULL,
            body TEXT NOT NULL,
            created_at_us BIGINT NOT NULL,
            parent_id INTEGER,
            UNIQUE(created_at_us),
            FOREIGN KEY (user_id) REFERENCES users (id),
            FOREIGN KEY (board_id) REFERENCES boards (id),
            FOREIGN KEY (parent_id) REFERENCES posts (id)
        )"#,
    )
    .execute(conn)?;

    // Create board_states table
    diesel::sql_query(
        r#"CREATE TABLE IF NOT EXISTS board_states (
            id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            user_id INTEGER NOT NULL,
            board_id INTEGER NOT NULL,
            last_post_us BIGINT NOT NULL,
            FOREIGN KEY (board_id) REFERENCES boards (id),
            FOREIGN KEY (user_id) REFERENCES users (id)
        )"#,
    )
    .execute(conn)?;

    // Create queued_messages table
    diesel::sql_query(
        r#"CREATE TABLE IF NOT EXISTS queued_messages (
            id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            sender_id INTEGER NOT NULL,
            recipient_id INTEGER NOT NULL,
            body TEXT NOT NULL,
            created_at_us BIGINT NOT NULL,
            sent_at_us BIGINT,
            FOREIGN KEY (sender_id) REFERENCES users (id),
            FOREIGN KEY (recipient_id) REFERENCES users (id)
        )"#,
    )
    .execute(conn)?;

    // Create moderators table (for board permission delegation)
    diesel::sql_query(
        r#"CREATE TABLE IF NOT EXISTS moderators (
            id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            user_id INTEGER NOT NULL,
            board_id INTEGER NOT NULL,
            granted_by INTEGER NOT NULL,
            granted_at_us BIGINT NOT NULL,
            UNIQUE(user_id, board_id),
            FOREIGN KEY (user_id) REFERENCES users (id),
            FOREIGN KEY (board_id) REFERENCES boards (id),
            FOREIGN KEY (granted_by) REFERENCES users (id)
        )"#,
    )
    .execute(conn)?;

    Ok(())
}

/// Run database migrations for schema updates
/// Call this after initialize_database to upgrade existing databases
pub fn run_migrations(conn: &mut SqliteConnection) -> Result<()> {
    // Add creator_id column to boards if it doesn't exist
    let _ = diesel::sql_query(
        "ALTER TABLE boards ADD COLUMN creator_id INTEGER"
    ).execute(conn);

    // Add parent_id column to posts if it doesn't exist
    let _ = diesel::sql_query(
        "ALTER TABLE posts ADD COLUMN parent_id INTEGER REFERENCES posts(id)"
    ).execute(conn);

    Ok(())
}

/// Get current timestamp in microseconds
pub fn now_as_useconds() -> i64 {
    chrono::Utc::now().timestamp_micros()
}
