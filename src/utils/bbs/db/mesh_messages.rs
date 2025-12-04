//! Database operations for Meshtastic mesh messages

use diesel::prelude::*;
use diesel::sqlite::SqliteConnection;

use crate::utils::bbs::models::{MeshMessageDb, MeshMessageUpdate, NewMeshMessage};
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
pub fn get_from_node(
    conn: &mut SqliteConnection,
    node_id: u32,
    limit: i64,
) -> Result<Vec<MeshMessageDb>> {
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
    let count = mesh_messages::table.count().get_result::<i64>(conn)?;

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
            let deleted =
                diesel::delete(mesh_messages::table.filter(mesh_messages::id.lt(threshold_id)))
                    .execute(conn)?;
            Ok(deleted)
        }
        Err(diesel::NotFound) => Ok(0), // Not enough messages to prune
        Err(e) => Err(e.into()),
    }
}

// ============================================
// Statistics Functions
// ============================================

/// Statistics about mesh messages
#[derive(Debug, Clone)]
pub struct MeshStats {
    pub total_messages: i64,
    pub total_commands: i64,
    pub total_responses: i64,
    pub unique_nodes: i64,
    pub messages_last_hour: i64,
    pub messages_last_24h: i64,
    pub top_nodes: Vec<NodeStats>,
    pub oldest_message: Option<i64>, // timestamp_us
    pub newest_message: Option<i64>, // timestamp_us
}

/// Statistics for a single node
#[derive(Debug, Clone)]
pub struct NodeStats {
    pub node_id: u32,
    pub node_name: Option<String>,
    pub message_count: i64,
    pub command_count: i64,
}

/// Get comprehensive mesh statistics
pub fn get_stats(conn: &mut SqliteConnection) -> Result<MeshStats> {
    use diesel::dsl::*;

    // Total messages
    let total_messages = mesh_messages::table.count().get_result::<i64>(conn)?;

    // Total commands
    let total_commands = mesh_messages::table
        .filter(mesh_messages::is_command.eq(true))
        .count()
        .get_result::<i64>(conn)?;

    // Total responses (messages that have a response)
    let total_responses = mesh_messages::table
        .filter(mesh_messages::response.is_not_null())
        .count()
        .get_result::<i64>(conn)?;

    // Unique nodes (using raw SQL for COUNT DISTINCT)
    let unique_nodes: i64 =
        diesel::sql_query("SELECT COUNT(DISTINCT from_node_id) as count FROM mesh_messages")
            .get_result::<CountResult>(conn)
            .map(|r| r.count)
            .unwrap_or(0);

    // Messages in last hour
    let one_hour_ago = chrono::Utc::now().timestamp_micros() - (3600 * 1_000_000);
    let messages_last_hour = mesh_messages::table
        .filter(mesh_messages::received_at_us.gt(one_hour_ago))
        .count()
        .get_result::<i64>(conn)?;

    // Messages in last 24 hours
    let one_day_ago = chrono::Utc::now().timestamp_micros() - (86400 * 1_000_000);
    let messages_last_24h = mesh_messages::table
        .filter(mesh_messages::received_at_us.gt(one_day_ago))
        .count()
        .get_result::<i64>(conn)?;

    // Oldest and newest message timestamps
    let oldest_message = mesh_messages::table
        .select(min(mesh_messages::received_at_us))
        .first::<Option<i64>>(conn)?;

    let newest_message = mesh_messages::table
        .select(max(mesh_messages::received_at_us))
        .first::<Option<i64>>(conn)?;

    // Top nodes by message count (using raw SQL for GROUP BY)
    let top_nodes = get_top_nodes(conn, 10)?;

    Ok(MeshStats {
        total_messages,
        total_commands,
        total_responses,
        unique_nodes,
        messages_last_hour,
        messages_last_24h,
        top_nodes,
        oldest_message,
        newest_message,
    })
}

/// Helper struct for COUNT queries
#[derive(QueryableByName)]
struct CountResult {
    #[diesel(sql_type = diesel::sql_types::BigInt)]
    count: i64,
}

/// Helper struct for top nodes query
#[derive(QueryableByName)]
struct TopNodeRow {
    #[diesel(sql_type = diesel::sql_types::BigInt)]
    from_node_id: i64,
    #[diesel(sql_type = diesel::sql_types::Nullable<diesel::sql_types::Text>)]
    from_name: Option<String>,
    #[diesel(sql_type = diesel::sql_types::BigInt)]
    msg_count: i64,
    #[diesel(sql_type = diesel::sql_types::BigInt)]
    cmd_count: i64,
}

/// Get top nodes by message count
pub fn get_top_nodes(conn: &mut SqliteConnection, limit: i64) -> Result<Vec<NodeStats>> {
    let rows: Vec<TopNodeRow> = diesel::sql_query(format!(
        "SELECT from_node_id, from_name,
                COUNT(*) as msg_count,
                SUM(CASE WHEN is_command THEN 1 ELSE 0 END) as cmd_count
         FROM mesh_messages
         GROUP BY from_node_id
         ORDER BY msg_count DESC
         LIMIT {}",
        limit
    ))
    .load(conn)?;

    Ok(rows
        .into_iter()
        .map(|r| NodeStats {
            node_id: r.from_node_id as u32,
            node_name: r.from_name,
            message_count: r.msg_count,
            command_count: r.cmd_count,
        })
        .collect())
}

/// Get message activity by hour (last 24 hours)
pub fn get_hourly_activity(conn: &mut SqliteConnection) -> Result<Vec<(i64, i64)>> {
    // Returns (hour_timestamp, message_count) pairs
    let one_day_ago = chrono::Utc::now().timestamp_micros() - (86400 * 1_000_000);

    let rows: Vec<HourlyRow> = diesel::sql_query(format!(
        "SELECT (received_at_us / 3600000000) * 3600000000 as hour_ts,
                COUNT(*) as msg_count
         FROM mesh_messages
         WHERE received_at_us > {}
         GROUP BY hour_ts
         ORDER BY hour_ts",
        one_day_ago
    ))
    .load(conn)?;

    Ok(rows.into_iter().map(|r| (r.hour_ts, r.msg_count)).collect())
}

#[derive(QueryableByName)]
struct HourlyRow {
    #[diesel(sql_type = diesel::sql_types::BigInt)]
    hour_ts: i64,
    #[diesel(sql_type = diesel::sql_types::BigInt)]
    msg_count: i64,
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
        )
        .unwrap();

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
        )
        .unwrap();

        assert!(msg.response.is_none());

        // Add a response
        add_response(
            &mut conn,
            msg.id,
            "Bitcoin is a cryptocurrency",
            1704096001000000,
        )
        .unwrap();

        // Verify response was added
        let updated = get_recent(&mut conn, 1).unwrap();
        assert_eq!(
            updated[0].response,
            Some("Bitcoin is a cryptocurrency".to_string())
        );
        assert!(updated[0].responded_at_us.is_some());
    }

    #[test]
    fn test_get_commands() {
        let mut conn = setup_test_db();

        // Create some messages
        create(
            &mut conn,
            0x11111111,
            None,
            None,
            0,
            "Hello",
            false,
            1704096000000000,
        )
        .unwrap();
        create(
            &mut conn,
            0x22222222,
            None,
            None,
            0,
            "/boards",
            true,
            1704096001000000,
        )
        .unwrap();
        create(
            &mut conn,
            0x33333333,
            None,
            None,
            0,
            "World",
            false,
            1704096002000000,
        )
        .unwrap();
        create(
            &mut conn,
            0x44444444,
            None,
            None,
            0,
            "/agent test",
            true,
            1704096003000000,
        )
        .unwrap();

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

        create(
            &mut conn,
            0x11111111,
            None,
            None,
            0,
            "Msg1",
            false,
            1704096000000000,
        )
        .unwrap();
        create(
            &mut conn,
            0x11111111,
            None,
            None,
            0,
            "Msg2",
            false,
            1704096001000000,
        )
        .unwrap();
        create(
            &mut conn,
            0x22222222,
            None,
            None,
            0,
            "Msg3",
            false,
            1704096002000000,
        )
        .unwrap();

        assert_eq!(count(&mut conn).unwrap(), 3);
        assert_eq!(count_from_node(&mut conn, 0x11111111).unwrap(), 2);
        assert_eq!(count_from_node(&mut conn, 0x22222222).unwrap(), 1);
    }
}
