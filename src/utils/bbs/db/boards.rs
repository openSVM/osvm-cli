// Board database operations

use diesel::prelude::*;
use crate::utils::bbs::{models::*, schema::boards};
use super::{Result, now_as_useconds};

/// List all boards
pub fn list(conn: &mut SqliteConnection) -> Result<Vec<Board>> {
    boards::table
        .load::<Board>(conn)
        .map_err(|e| e.into())
}

/// Get board by ID
pub fn get(conn: &mut SqliteConnection, board_id: i32) -> Result<Board> {
    boards::table
        .find(board_id)
        .first::<Board>(conn)
        .map_err(|e| e.into())
}

/// Create new board
pub fn create(conn: &mut SqliteConnection, name: &str, description: &str) -> Result<Board> {
    let timestamp = now_as_useconds();
    let new_board = NewBoard {
        name,
        description,
        created_at_us: &timestamp,
    };

    diesel::insert_into(boards::table)
        .values(&new_board)
        .execute(conn)?;

    boards::table
        .filter(boards::name.eq(name))
        .first::<Board>(conn)
        .map_err(|e| e.into())
}

/// Count boards
pub fn count(conn: &mut SqliteConnection) -> i64 {
    boards::table.count().get_result(conn).unwrap_or(0)
}
