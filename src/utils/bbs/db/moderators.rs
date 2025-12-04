// Moderator database operations

use super::{now_as_useconds, Result};
use crate::utils::bbs::{
    models::*,
    schema::{boards, moderators},
};
use diesel::prelude::*;

/// List all moderators for a board
pub fn list_for_board(conn: &mut SqliteConnection, board_id: i32) -> Result<Vec<Moderator>> {
    moderators::table
        .filter(moderators::board_id.eq(board_id))
        .load::<Moderator>(conn)
        .map_err(|e| e.into())
}

/// List all boards a user moderates
pub fn list_for_user(conn: &mut SqliteConnection, user_id: i32) -> Result<Vec<Moderator>> {
    moderators::table
        .filter(moderators::user_id.eq(user_id))
        .load::<Moderator>(conn)
        .map_err(|e| e.into())
}

/// Check if user is a moderator for a board
pub fn is_moderator(conn: &mut SqliteConnection, user_id: i32, board_id: i32) -> bool {
    moderators::table
        .filter(moderators::user_id.eq(user_id))
        .filter(moderators::board_id.eq(board_id))
        .first::<Moderator>(conn)
        .is_ok()
}

/// Check if user is board creator or moderator
pub fn has_mod_permissions(
    conn: &mut SqliteConnection,
    user_id: i32,
    board_id: i32,
) -> Result<bool> {
    // Check if user is board creator
    let board = boards::table.find(board_id).first::<Board>(conn)?;

    if board.creator_id == Some(user_id) {
        return Ok(true);
    }

    // Check if user is a moderator
    Ok(is_moderator(conn, user_id, board_id))
}

/// Add a moderator to a board (only board creator can do this)
pub fn add(
    conn: &mut SqliteConnection,
    board_id: i32,
    user_id: i32,
    granted_by: i32,
) -> Result<Option<Moderator>> {
    // Check if granter is the board creator
    let board = boards::table.find(board_id).first::<Board>(conn)?;

    if board.creator_id != Some(granted_by) {
        return Ok(None); // Only creator can add mods
    }

    // Check if user is already a moderator
    if is_moderator(conn, user_id, board_id) {
        return Ok(None); // Already a mod
    }

    let timestamp = now_as_useconds();
    let new_mod = NewModerator {
        user_id,
        board_id,
        granted_by,
        granted_at_us: timestamp,
    };

    diesel::insert_into(moderators::table)
        .values(&new_mod)
        .execute(conn)?;

    moderators::table
        .filter(moderators::user_id.eq(user_id))
        .filter(moderators::board_id.eq(board_id))
        .first::<Moderator>(conn)
        .map(Some)
        .map_err(|e| e.into())
}

/// Remove a moderator from a board (only board creator can do this)
pub fn remove(
    conn: &mut SqliteConnection,
    board_id: i32,
    user_id: i32,
    requester_id: i32,
) -> Result<bool> {
    // Check if requester is the board creator
    let board = boards::table.find(board_id).first::<Board>(conn)?;

    if board.creator_id != Some(requester_id) {
        return Ok(false); // Only creator can remove mods
    }

    let deleted = diesel::delete(
        moderators::table
            .filter(moderators::user_id.eq(user_id))
            .filter(moderators::board_id.eq(board_id)),
    )
    .execute(conn)?;

    Ok(deleted > 0)
}

/// Count moderators for a board
pub fn count_for_board(conn: &mut SqliteConnection, board_id: i32) -> i64 {
    moderators::table
        .filter(moderators::board_id.eq(board_id))
        .count()
        .get_result(conn)
        .unwrap_or(0)
}
