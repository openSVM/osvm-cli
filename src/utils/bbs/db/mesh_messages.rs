//! Database operations for Meshtastic mesh messages

use diesel::prelude::*;
use diesel::sqlite::SqliteConnection;

use crate::utils::bbs::models::{MeshMessageDb, NewMeshMessage, MeshMessageUpdate};
use crate::utils::bbs::schema::mesh_messages;

use super::Result;

/// Create a new mesh message record
pub fn create(
    conn: &mut SqliteConnection,
    from_node_id: u32,
    from_name: Option<&str>,
    to_node_id: Option<u32>,
    channel: u8,
    body: &str,
    is_command: bool,
    received_at_us: i64,
) -> Result<MeshMessageDb> {
    let new_msg = NewMeshMessage {
        from_node_id: from_node_id as i64,
        from_name,
        to_node_id: to_node_id.map(|id| id as i64),
        channel: channel as i32,
        body,
        is_command,
        received_at_us,
        response: None,
        responded_at_us: None,
    };

    diesel::insert_into(mesh_messages::table)
        .values(&new_msg)
        .execute(conn)?;

    // Get the inserted message
    let msg = mesh_messages::table
        .order(mesh_messages::id.desc())
        .first::<MeshMessageDb>(conn)?;

    Ok(msg)
}

/// Update a mesh message with a response
pub fn add_response(
    conn: &mut SqliteConnection,
    message_id: i32,
    response: &str,
    responded_at_us: i64,
) -> Result<()> {
    let update = MeshMessageUpdate {
        response: Some(response.to_string()),
        responded_at_us: Some(responded_at_us),
    };

    diesel::update(mesh_messages::table.find(message_id))
        .set(&update)
        .execute(conn)?;

    Ok(())
}

/// Get recent mesh messages (most recent first)
pub fn get_recent(conn: &mut SqliteConnection, limit: i64) -> Result<Vec<MeshMessageDb>> {
    let messages = mesh_messages::table
        .order(mesh_messages::received_at_us.desc())
        .limit(limit)
        .load::<MeshMessageDb>(conn)?;

    Ok(messages)
}

/// Get mesh messages from a specific node
pub fn get_from_node(conn: &mut SqliteConnection, node_id: u32, limit: i64) -> Result<Vec<MeshMessageDb>> {
    let messages = mesh_messages::table
        .filter(mesh_messages::from_node_id.eq(node_id as i64))
        .order(mesh_messages::received_at_us.desc())
        .limit(limit)
        .load::<MeshMessageDb>(conn)?;

    Ok(messages)
}

/// Get mesh messages that were commands
pub fn get_commands(conn: &mut SqliteConnection, limit: i64) -> Result<Vec<MeshMessageDb>> {
    let messages = mesh_messages::table
        .filter(mesh_messages::is_command.eq(true))
        .order(mesh_messages::received_at_us.desc())
        .limit(limit)
        .load::<MeshMessageDb>(conn)?;

    Ok(messages)
}

/// Get mesh messages since a timestamp
pub fn get_since(conn: &mut SqliteConnection, since_us: i64) -> Result<Vec<MeshMessageDb>> {
    let messages = mesh_messages::table
        .filter(mesh_messages::received_at_us.gt(since_us))
        .order(mesh_messages::received_at_us.asc())
        .load::<MeshMessageDb>(conn)?;

    Ok(messages)
}

/// Count total mesh messages
pub fn count(conn: &mut SqliteConnection) -> Result<i64> {
    let count = mesh_messages::table
        .count()
        .get_result::<i64>(conn)?;

    Ok(count)
}

/// Count messages from a specific node
pub fn count_from_node(conn: &mut SqliteConnection, node_id: u32) -> Result<i64> {
    let count = mesh_messages::table
        .filter(mesh_messages::from_node_id.eq(node_id as i64))
        .count()
        .get_result::<i64>(conn)?;

    Ok(count)
}

/// Delete old messages (keeping only the most recent N)
pub fn prune_old(conn: &mut SqliteConnection, keep_count: i64) -> Result<usize> {
    // Get the ID threshold (oldest ID to keep)
    let threshold_result = mesh_messages::table
        .order(mesh_messages::id.desc())
        .offset(keep_count)
        .limit(1)
        .select(mesh_messages::id)
        .first::<i32>(conn);

    match threshold_result {
        Ok(threshold_id) => {
            let deleted = diesel::delete(
                mesh_messages::table.filter(mesh_messages::id.lt(threshold_id))
            ).execute(conn)?;
            Ok(deleted)
        }
        Err(diesel::NotFound) => Ok(0), // Not enough messages to prune
        Err(e) => Err(e.into()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::bbs::db;

    fn setup_test_db() -> SqliteConnection {
        let mut conn = SqliteConnection::establish(":memory:").unwrap();
        db::initialize_database(&mut conn).unwrap();
        conn
    }

    #[test]
    fn test_create_and_get_message() {
        let mut conn = setup_test_db();

        // Create a message
        let msg = create(
            &mut conn,
            0x12345678,
            Some("TestNode"),
            None, // Broadcast
            0,
            "Hello mesh!",
            false,
            1704096000000000, // Early 2024
        ).unwrap();

        assert_eq!(msg.from_node_id, 0x12345678);
        assert_eq!(msg.from_name, Some("TestNode".to_string()));
        assert_eq!(msg.body, "Hello mesh!");
        assert!(!msg.is_command);

        // Get recent messages
        let messages = get_recent(&mut conn, 10).unwrap();
        assert_eq!(messages.len(), 1);
        assert_eq!(messages[0].body, "Hello mesh!");
    }

    #[test]
    fn test_add_response() {
        let mut conn = setup_test_db();

        // Create a command message
        let msg = create(
            &mut conn,
            0x87654321,
            Some("User"),
            None,
            0,
            "/agent what is bitcoin",
            true,
            1704096000000000,
        ).unwrap();

        assert!(msg.response.is_none());

        // Add a response
        add_response(
            &mut conn,
            msg.id,
            "Bitcoin is a cryptocurrency",
            1704096001000000,
        ).unwrap();

        // Verify response was added
        let updated = get_recent(&mut conn, 1).unwrap();
        assert_eq!(updated[0].response, Some("Bitcoin is a cryptocurrency".to_string()));
        assert!(updated[0].responded_at_us.is_some());
    }

    #[test]
    fn test_get_commands() {
        let mut conn = setup_test_db();

        // Create some messages
        create(&mut conn, 0x11111111, None, None, 0, "Hello", false, 1704096000000000).unwrap();
        create(&mut conn, 0x22222222, None, None, 0, "/boards", true, 1704096001000000).unwrap();
        create(&mut conn, 0x33333333, None, None, 0, "World", false, 1704096002000000).unwrap();
        create(&mut conn, 0x44444444, None, None, 0, "/agent test", true, 1704096003000000).unwrap();

        // Get only commands
        let commands = get_commands(&mut conn, 10).unwrap();
        assert_eq!(commands.len(), 2);
        assert!(commands[0].body.starts_with('/'));
        assert!(commands[1].body.starts_with('/'));
    }

    #[test]
    fn test_count() {
        let mut conn = setup_test_db();

        assert_eq!(count(&mut conn).unwrap(), 0);

        create(&mut conn, 0x11111111, None, None, 0, "Msg1", false, 1704096000000000).unwrap();
        create(&mut conn, 0x11111111, None, None, 0, "Msg2", false, 1704096001000000).unwrap();
        create(&mut conn, 0x22222222, None, None, 0, "Msg3", false, 1704096002000000).unwrap();

        assert_eq!(count(&mut conn).unwrap(), 3);
        assert_eq!(count_from_node(&mut conn, 0x11111111).unwrap(), 2);
        assert_eq!(count_from_node(&mut conn, 0x22222222).unwrap(), 1);
    }
}
