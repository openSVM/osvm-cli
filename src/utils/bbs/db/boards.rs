// Board database operations

use super::{now_as_useconds, Result};
use crate::utils::bbs::{
    models::*,
    schema::{boards, moderators},
};
use diesel::prelude::*;

/// List all boards
pub fn list(conn: &mut SqliteConnection) -> Result<Vec<Board>> {
    boards::table.load::<Board>(conn).map_err(|e| e.into())
}

/// Get board by ID
pub fn get(conn: &mut SqliteConnection, board_id: i32) -> Result<Board> {
    boards::table
        .find(board_id)
        .first::<Board>(conn)
        .map_err(|e| e.into())
}

/// Get board by name
pub fn get_by_name(conn: &mut SqliteConnection, name: &str) -> Result<Board> {
    boards::table
        .filter(boards::name.eq(name))
        .first::<Board>(conn)
        .map_err(|e| e.into())
}

/// Create new board (without owner - backwards compatible)
pub fn create(conn: &mut SqliteConnection, name: &str, description: &str) -> Result<Board> {
    create_with_creator(conn, name, description, None)
}

/// Create new board with creator (owner who can delete)
pub fn create_with_creator(
    conn: &mut SqliteConnection,
    name: &str,
    description: &str,
    creator_id: Option<i32>,
) -> Result<Board> {
    let timestamp = now_as_useconds();
    let new_board = NewBoard {
        name,
        description,
        created_at_us: &timestamp,
        creator_id,
    };

    diesel::insert_into(boards::table)
        .values(&new_board)
        .execute(conn)?;

    boards::table
        .filter(boards::name.eq(name))
        .first::<Board>(conn)
        .map_err(|e| e.into())
}

/// Delete a board (only if user is creator or moderator)
/// Returns Ok(true) if deleted, Ok(false) if no permission, Err on DB error
pub fn delete(conn: &mut SqliteConnection, board_id: i32, requester_user_id: i32) -> Result<bool> {
    // Get the board
    let board = get(conn, board_id)?;

    // Check if requester is the creator
    let is_creator = board.creator_id == Some(requester_user_id);

    // Check if requester is a moderator for this board
    let is_mod = moderators::table
        .filter(moderators::board_id.eq(board_id))
        .filter(moderators::user_id.eq(requester_user_id))
        .first::<Moderator>(conn)
        .is_ok();

    if !is_creator && !is_mod {
        return Ok(false); // No permission
    }

    // Delete all posts in the board first (cascade)
    diesel::delete(
        crate::utils::bbs::schema::posts::table
            .filter(crate::utils::bbs::schema::posts::board_id.eq(board_id)),
    )
    .execute(conn)?;

    // Delete all moderator assignments for this board
    diesel::delete(moderators::table.filter(moderators::board_id.eq(board_id))).execute(conn)?;

    // Delete board states
    diesel::delete(
        crate::utils::bbs::schema::board_states::table
            .filter(crate::utils::bbs::schema::board_states::board_id.eq(board_id)),
    )
    .execute(conn)?;

    // Delete the board
    diesel::delete(boards::table.find(board_id)).execute(conn)?;

    Ok(true)
}

/// Check if user can delete a board
pub fn can_delete(conn: &mut SqliteConnection, board_id: i32, user_id: i32) -> Result<bool> {
    let board = get(conn, board_id)?;

    // Creator can always delete
    if board.creator_id == Some(user_id) {
        return Ok(true);
    }

    // Check if user is a moderator
    let is_mod = moderators::table
        .filter(moderators::board_id.eq(board_id))
        .filter(moderators::user_id.eq(user_id))
        .first::<Moderator>(conn)
        .is_ok();

    Ok(is_mod)
}

/// Set creator for a board (for migrating existing boards)
pub fn set_creator(conn: &mut SqliteConnection, board_id: i32, creator_id: i32) -> Result<()> {
    diesel::update(boards::table.find(board_id))
        .set(boards::creator_id.eq(Some(creator_id)))
        .execute(conn)?;
    Ok(())
}

/// Count boards
pub fn count(conn: &mut SqliteConnection) -> i64 {
    boards::table.count().get_result(conn).unwrap_or(0)
}
