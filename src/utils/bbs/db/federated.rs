//! Database operations for federated messages and peers

use diesel::prelude::*;
use diesel::sqlite::SqliteConnection;
use std::time::{SystemTime, UNIX_EPOCH};

use super::Result;
use crate::utils::bbs::models::{
    FederatedMessageDb, KnownPeerDb, NewFederatedMessage, NewKnownPeer,
};
use crate::utils::bbs::schema::{federated_messages, known_peers};

// ============================================
// Federated Messages
// ============================================

/// Store a federated message from another node
pub fn store_message(
    conn: &mut SqliteConnection,
    message_id: &str,
    origin_node: &str,
    board: &str,
    author_node: &str,
    author_name: &str,
    body: &str,
    parent_id: Option<&str>,
    created_at: i64,
    signature: Option<&str>,
) -> Result<FederatedMessageDb> {
    let now = now_timestamp();

    let new_msg = NewFederatedMessage {
        message_id,
        origin_node,
        board: &board.to_uppercase(),
        author_node,
        author_name,
        body,
        parent_id,
        created_at,
        received_at: now,
        signature,
    };

    diesel::insert_into(federated_messages::table)
        .values(&new_msg)
        .execute(conn)?;

    // Return the inserted message
    federated_messages::table
        .filter(federated_messages::message_id.eq(message_id))
        .first(conn)
        .map_err(|e| e.into())
}

/// Check if a message already exists (by message_id)
pub fn message_exists(conn: &mut SqliteConnection, message_id: &str) -> bool {
    federated_messages::table
        .filter(federated_messages::message_id.eq(message_id))
        .count()
        .get_result::<i64>(conn)
        .unwrap_or(0)
        > 0
}

/// Get a single message by message_id
pub fn get_message(conn: &mut SqliteConnection, message_id: &str) -> Result<FederatedMessageDb> {
    federated_messages::table
        .filter(federated_messages::message_id.eq(message_id))
        .first(conn)
        .map_err(|e| e.into())
}

/// Get messages since a timestamp (for sync responses)
pub fn get_messages_since(
    conn: &mut SqliteConnection,
    since_timestamp: i64,
    limit: i64,
) -> Result<Vec<FederatedMessageDb>> {
    federated_messages::table
        .filter(federated_messages::created_at.gt(since_timestamp))
        .order(federated_messages::created_at.asc())
        .limit(limit)
        .load(conn)
        .map_err(|e| e.into())
}

/// Get messages for a specific board since a timestamp
pub fn get_messages_for_board(
    conn: &mut SqliteConnection,
    board: &str,
    since_timestamp: i64,
    limit: i64,
) -> Result<Vec<FederatedMessageDb>> {
    federated_messages::table
        .filter(federated_messages::board.eq(board.to_uppercase()))
        .filter(federated_messages::created_at.gt(since_timestamp))
        .order(federated_messages::created_at.asc())
        .limit(limit)
        .load(conn)
        .map_err(|e| e.into())
}

/// Count total federated messages
pub fn message_count(conn: &mut SqliteConnection) -> i64 {
    federated_messages::table
        .count()
        .get_result(conn)
        .unwrap_or(0)
}

/// Count replies to a specific message (by parent_id)
pub fn reply_count(conn: &mut SqliteConnection, parent_message_id: &str) -> i64 {
    federated_messages::table
        .filter(federated_messages::parent_id.eq(parent_message_id))
        .count()
        .get_result(conn)
        .unwrap_or(0)
}

// ============================================
// Known Peers (for persistence)
// ============================================

/// Store or update a peer
pub fn upsert_peer(
    conn: &mut SqliteConnection,
    node_id: &str,
    address: &str,
    name: Option<&str>,
    is_bootstrap: bool,
) -> Result<KnownPeerDb> {
    // Try to get existing peer
    let existing: Option<KnownPeerDb> = known_peers::table
        .filter(known_peers::node_id.eq(node_id))
        .first(conn)
        .optional()?;

    if let Some(peer) = existing {
        // Update existing peer
        diesel::update(known_peers::table.filter(known_peers::node_id.eq(node_id)))
            .set((
                known_peers::address.eq(address),
                known_peers::name.eq(name),
                known_peers::is_bootstrap.eq(is_bootstrap),
            ))
            .execute(conn)?;

        known_peers::table
            .filter(known_peers::node_id.eq(node_id))
            .first(conn)
            .map_err(|e| e.into())
    } else {
        // Insert new peer
        let new_peer = NewKnownPeer {
            node_id,
            address,
            name,
            last_sync: None,
            last_seen: None,
            failure_count: 0,
            is_bootstrap,
        };

        diesel::insert_into(known_peers::table)
            .values(&new_peer)
            .execute(conn)?;

        known_peers::table
            .filter(known_peers::node_id.eq(node_id))
            .first(conn)
            .map_err(|e| e.into())
    }
}

/// Get all known peers
pub fn list_peers(conn: &mut SqliteConnection) -> Result<Vec<KnownPeerDb>> {
    known_peers::table
        .order(known_peers::last_seen.desc())
        .load(conn)
        .map_err(|e| e.into())
}

/// Get peer by node_id
pub fn get_peer(conn: &mut SqliteConnection, node_id: &str) -> Result<KnownPeerDb> {
    known_peers::table
        .filter(known_peers::node_id.eq(node_id))
        .first(conn)
        .map_err(|e| e.into())
}

/// Update peer sync timestamp
pub fn update_peer_sync(conn: &mut SqliteConnection, node_id: &str) -> Result<()> {
    let now = now_timestamp();
    diesel::update(known_peers::table.filter(known_peers::node_id.eq(node_id)))
        .set((
            known_peers::last_sync.eq(now),
            known_peers::last_seen.eq(now),
            known_peers::failure_count.eq(0),
        ))
        .execute(conn)?;
    Ok(())
}

/// Update peer last_seen timestamp
pub fn update_peer_seen(conn: &mut SqliteConnection, node_id: &str) -> Result<()> {
    let now = now_timestamp();
    diesel::update(known_peers::table.filter(known_peers::node_id.eq(node_id)))
        .set(known_peers::last_seen.eq(now))
        .execute(conn)?;
    Ok(())
}

/// Increment peer failure count
pub fn increment_peer_failure(conn: &mut SqliteConnection, node_id: &str) -> Result<()> {
    diesel::update(known_peers::table.filter(known_peers::node_id.eq(node_id)))
        .set(known_peers::failure_count.eq(known_peers::failure_count + 1))
        .execute(conn)?;
    Ok(())
}

/// Remove a peer
pub fn remove_peer(conn: &mut SqliteConnection, node_id: &str) -> Result<bool> {
    let deleted = diesel::delete(known_peers::table.filter(known_peers::node_id.eq(node_id)))
        .execute(conn)?;
    Ok(deleted > 0)
}

/// Count known peers
pub fn peer_count(conn: &mut SqliteConnection) -> i64 {
    known_peers::table.count().get_result(conn).unwrap_or(0)
}

// ============================================
// Helpers
// ============================================

/// Get current Unix timestamp in seconds
fn now_timestamp() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs() as i64
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::{tempdir, TempDir};

    fn setup_test_db() -> (SqliteConnection, TempDir) {
        let tmp_dir = tempdir().expect("Failed to create temp dir");
        let db_file = tmp_dir.path().join("test-federation.db");
        let db_url = db_file.to_str().unwrap();

        let mut conn =
            SqliteConnection::establish(db_url).expect("Failed to connect to test database");

        crate::utils::bbs::db::initialize_database(&mut conn)
            .expect("Failed to initialize database");

        (conn, tmp_dir)
    }

    #[test]
    fn test_store_and_get_message() {
        let (mut conn, _tmp_dir) = setup_test_db();

        let msg = store_message(
            &mut conn,
            "!node123:1",
            "!node123",
            "GENERAL",
            "!author1",
            "Alice",
            "Hello from federation!",
            None,
            1700000000,
            None,
        )
        .expect("Failed to store message");

        assert_eq!(msg.message_id, "!node123:1");
        assert_eq!(msg.board, "GENERAL");
        assert_eq!(msg.body, "Hello from federation!");

        // Check it exists
        assert!(message_exists(&mut conn, "!node123:1"));
        assert!(!message_exists(&mut conn, "!node999:99"));

        // Get messages since
        let messages = get_messages_since(&mut conn, 0, 100).expect("Failed to get messages");
        assert_eq!(messages.len(), 1);
    }

    #[test]
    fn test_peer_operations() {
        let (mut conn, _tmp_dir) = setup_test_db();

        // Upsert peer
        let peer = upsert_peer(
            &mut conn,
            "!peer0001",
            "http://localhost:8080",
            Some("Test Peer"),
            false,
        )
        .expect("Failed to upsert peer");

        assert_eq!(peer.node_id, "!peer0001");
        assert_eq!(peer.address, "http://localhost:8080");

        // List peers
        let peers = list_peers(&mut conn).expect("Failed to list peers");
        assert_eq!(peers.len(), 1);

        // Update sync
        update_peer_sync(&mut conn, "!peer0001").expect("Failed to update sync");
        let updated = get_peer(&mut conn, "!peer0001").expect("Failed to get peer");
        assert!(updated.last_sync.is_some());

        // Remove peer
        let removed = remove_peer(&mut conn, "!peer0001").expect("Failed to remove peer");
        assert!(removed);

        assert_eq!(peer_count(&mut conn), 0);
    }
}
