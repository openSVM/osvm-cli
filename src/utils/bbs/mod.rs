// FrozenBBS Integration for OSVM
// Adapted from https://github.com/kstrauser/frozenbbs
//
// This module integrates Meshtastic-based bulletin board system (BBS)
// for communication between humans and AI agents over low-bandwidth radio networks.

pub mod agent_bridge;
pub mod commands;
pub mod db;
pub mod federation;
pub mod http_server;
pub mod meshtastic;
pub mod message_router;
pub mod models;
pub mod registry;
pub mod schema;
pub mod tui_widgets;

use dirs::data_dir;
use std::path::PathBuf;

pub const BBS_TAG: &str = "osvm-bbs";

/// Get the path to the BBS database file
pub fn db_path() -> PathBuf {
    data_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join(".osvm")
        .join("bbs")
        .join("osvm-bbs.db")
}

/// Convert a node ID like 12345678 or !abcdef12 to their u32 value.
pub fn hex_id_to_num(node_id: &str) -> Option<u32> {
    let node_id = if node_id.starts_with('!') {
        node_id.strip_prefix('!').unwrap()
    } else {
        node_id
    };
    if node_id.len() != 8 {
        return None;
    }
    u32::from_str_radix(node_id, 16).ok()
}

/// Convert a u32 node ID to its canonical !abcdef12 format.
pub fn num_id_to_hex(node_num: u32) -> String {
    format!("!{node_num:08x}")
}

/// Convert a possibly mixed case node ID, with or without the leading !, to its canonical format.
pub fn canonical_node_id(node_id: &str) -> Option<String> {
    Some(num_id_to_hex(hex_id_to_num(node_id)?))
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn user_id_with_leading_zero() {
        assert_eq!(num_id_to_hex(0x00010203), "!00010203");
    }

    #[test]
    fn hex_conversion() {
        assert_eq!(hex_id_to_num("!cafeb33d"), Some(0xcafeb33d));
        assert_eq!(hex_id_to_num("cafeb33d"), Some(0xcafeb33d));
        assert_eq!(canonical_node_id("CAFEB33D"), Some("!cafeb33d".to_string()));
    }

    #[test]
    fn test_db_connection_and_schema() {
        use diesel::prelude::*;
        use diesel::sqlite::SqliteConnection;

        // Create temp directory for test database
        let tmp_dir = tempdir().expect("Failed to create temp dir");
        let db_file = tmp_dir.path().join("test-bbs.db");
        let db_url = db_file.to_str().unwrap();

        // Establish connection
        let mut conn =
            SqliteConnection::establish(db_url).expect("Failed to connect to test database");

        // Initialize schema
        db::initialize_database(&mut conn).expect("Failed to initialize database");

        // Verify tables exist by querying them
        let boards = db::boards::list(&mut conn).expect("Failed to list boards");
        assert_eq!(boards.len(), 0, "New database should have no boards");
    }

    #[test]
    fn test_board_crud_operations() {
        use diesel::prelude::*;
        use diesel::sqlite::SqliteConnection;

        let tmp_dir = tempdir().expect("Failed to create temp dir");
        let db_file = tmp_dir.path().join("test-boards.db");
        let db_url = db_file.to_str().unwrap();

        let mut conn =
            SqliteConnection::establish(db_url).expect("Failed to connect to test database");
        db::initialize_database(&mut conn).expect("Failed to initialize database");

        // Create a board
        let board = db::boards::create(&mut conn, "TEST_BOARD", "A test board")
            .expect("Failed to create board");
        assert_eq!(board.name, "TEST_BOARD");
        assert_eq!(board.description, "A test board");

        // List boards
        let boards = db::boards::list(&mut conn).expect("Failed to list boards");
        assert_eq!(boards.len(), 1);
        assert_eq!(boards[0].name, "TEST_BOARD");

        // Get board by ID
        let retrieved = db::boards::get(&mut conn, board.id).expect("Failed to get board by ID");
        assert_eq!(retrieved.name, "TEST_BOARD");

        // Create another board
        db::boards::create(&mut conn, "SECOND_BOARD", "Another board")
            .expect("Failed to create second board");
        assert_eq!(db::boards::count(&mut conn), 2);
    }

    #[test]
    fn test_user_operations() {
        use diesel::prelude::*;
        use diesel::sqlite::SqliteConnection;

        let tmp_dir = tempdir().expect("Failed to create temp dir");
        let db_file = tmp_dir.path().join("test-users.db");
        let db_url = db_file.to_str().unwrap();

        let mut conn =
            SqliteConnection::establish(db_url).expect("Failed to connect to test database");
        db::initialize_database(&mut conn).expect("Failed to initialize database");

        // Create user via observe
        let timestamp = db::now_as_useconds();
        let (user, existed) = db::users::observe(
            &mut conn,
            "!testnode",
            Some("TEST"),
            Some("Test User"),
            timestamp,
        )
        .expect("Failed to observe user");

        assert!(!existed, "User should be newly created");
        assert_eq!(user.node_id, "!testnode");
        assert_eq!(user.short_name, "TEST");
        assert_eq!(user.long_name, "Test User");

        // Update user via observe
        let (updated_user, existed) = db::users::observe(
            &mut conn,
            "!testnode",
            Some("UPDT"),
            Some("Updated User"),
            timestamp + 1000,
        )
        .expect("Failed to update user");

        assert!(existed, "User should exist now");
        assert_eq!(updated_user.short_name, "UPDT");
        assert_eq!(updated_user.long_name, "Updated User");

        // Get user by node_id
        let retrieved = db::users::get(&mut conn, "!testnode").expect("Failed to get user");
        assert_eq!(retrieved.node_id, "!testnode");

        // Count users
        let (total, active) = db::users::counts(&mut conn);
        assert_eq!(total, 1);
        assert_eq!(active, 1);
    }

    #[test]
    fn test_post_operations() {
        use diesel::prelude::*;
        use diesel::sqlite::SqliteConnection;

        let tmp_dir = tempdir().expect("Failed to create temp dir");
        let db_file = tmp_dir.path().join("test-posts.db");
        let db_url = db_file.to_str().unwrap();

        let mut conn =
            SqliteConnection::establish(db_url).expect("Failed to connect to test database");
        db::initialize_database(&mut conn).expect("Failed to initialize database");

        // Create a board first
        let board = db::boards::create(&mut conn, "POSTS_TEST", "Test board for posts")
            .expect("Failed to create board");

        // Create a user
        let timestamp = db::now_as_useconds();
        let (user, _) = db::users::observe(
            &mut conn,
            "!poster01",
            Some("POST"),
            Some("Poster User"),
            timestamp,
        )
        .expect("Failed to create user");

        // Create a post
        let post = db::posts::create(&mut conn, board.id, user.id, "Hello, World!")
            .expect("Failed to create post");
        assert_eq!(post.body, "Hello, World!");
        assert_eq!(post.board_id, board.id);
        assert_eq!(post.user_id, user.id);

        // Create more posts
        std::thread::sleep(std::time::Duration::from_millis(1)); // Ensure unique timestamps
        db::posts::create(&mut conn, board.id, user.id, "Second post")
            .expect("Failed to create second post");
        std::thread::sleep(std::time::Duration::from_millis(1));
        db::posts::create(&mut conn, board.id, user.id, "Third post")
            .expect("Failed to create third post");

        // List posts for board
        let posts =
            db::posts::list_for_board(&mut conn, board.id, 10).expect("Failed to list posts");
        assert_eq!(posts.len(), 3);

        // Posts should be ordered by created_at_us descending
        assert_eq!(posts[0].body, "Third post");
        assert_eq!(posts[2].body, "Hello, World!");

        // Test limit
        let limited = db::posts::list_for_board(&mut conn, board.id, 2)
            .expect("Failed to list limited posts");
        assert_eq!(limited.len(), 2);

        // Count posts
        assert_eq!(db::posts::count(&mut conn), 3);
    }

    #[test]
    fn test_full_workflow() {
        use diesel::prelude::*;
        use diesel::sqlite::SqliteConnection;

        let tmp_dir = tempdir().expect("Failed to create temp dir");
        let db_file = tmp_dir.path().join("test-workflow.db");
        let db_url = db_file.to_str().unwrap();

        let mut conn =
            SqliteConnection::establish(db_url).expect("Failed to connect to test database");
        db::initialize_database(&mut conn).expect("Failed to initialize database");

        // Simulate a full BBS workflow
        // 1. Create multiple boards
        let general = db::boards::create(&mut conn, "GENERAL", "General discussion")
            .expect("Failed to create GENERAL");
        let alerts = db::boards::create(&mut conn, "ALERTS", "System alerts")
            .expect("Failed to create ALERTS");

        // 2. Register multiple users
        let ts = db::now_as_useconds();
        let (alice, _) =
            db::users::observe(&mut conn, "!alice001", Some("ALIC"), Some("Alice"), ts)
                .expect("Failed to create Alice");
        let (bob, _) = db::users::observe(&mut conn, "!bob00002", Some("BOB"), Some("Bob"), ts)
            .expect("Failed to create Bob");

        // 3. Users post messages
        db::posts::create(&mut conn, general.id, alice.id, "Hello from Alice!")
            .expect("Failed to post Alice's message");
        std::thread::sleep(std::time::Duration::from_millis(1));
        db::posts::create(&mut conn, general.id, bob.id, "Hi Alice! - Bob")
            .expect("Failed to post Bob's message");
        std::thread::sleep(std::time::Duration::from_millis(1));
        db::posts::create(
            &mut conn,
            alerts.id,
            alice.id,
            "ALERT: System update at 5PM",
        )
        .expect("Failed to post alert");

        // 4. Verify state
        assert_eq!(db::boards::count(&mut conn), 2);
        assert_eq!(db::posts::count(&mut conn), 3);
        let (total_users, _) = db::users::counts(&mut conn);
        assert_eq!(total_users, 2);

        // 5. Read messages from general board
        let general_posts = db::posts::list_for_board(&mut conn, general.id, 10)
            .expect("Failed to list general posts");
        assert_eq!(general_posts.len(), 2);

        // 6. Read messages from alerts board
        let alert_posts = db::posts::list_for_board(&mut conn, alerts.id, 10)
            .expect("Failed to list alert posts");
        assert_eq!(alert_posts.len(), 1);
        assert_eq!(alert_posts[0].body, "ALERT: System update at 5PM");
    }
}
