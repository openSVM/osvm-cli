// Queued message operations

use super::{now_as_useconds, Result};
use crate::utils::bbs::{models::*, schema::queued_messages};
use diesel::prelude::*;

/// Get unsent messages for a user
pub fn get_unsent(conn: &mut SqliteConnection, user_id: i32) -> Result<Vec<QueuedMessage>> {
    queued_messages::table
        .filter(queued_messages::recipient_id.eq(user_id))
        .filter(queued_messages::sent_at_us.is_null())
        .load::<QueuedMessage>(conn)
        .map_err(|e| e.into())
}

/// Queue a new message
pub fn queue(
    conn: &mut SqliteConnection,
    sender_id: i32,
    recipient_id: i32,
    body: &str,
) -> Result<()> {
    let timestamp = now_as_useconds();
    let new_message = QueuedMessageNew {
        sender_id,
        recipient_id,
        body,
        created_at_us: &timestamp,
    };

    diesel::insert_into(queued_messages::table)
        .values(&new_message)
        .execute(conn)?;

    Ok(())
}

/// Mark message as sent
pub fn mark_sent(conn: &mut SqliteConnection, message_id: i32) -> Result<()> {
    let timestamp = now_as_useconds();

    diesel::update(queued_messages::table.find(message_id))
        .set(queued_messages::sent_at_us.eq(Some(timestamp)))
        .execute(conn)?;

    Ok(())
}
