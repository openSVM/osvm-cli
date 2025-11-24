// Post database operations

use diesel::prelude::*;
use crate::utils::bbs::{models::*, schema::posts};
use super::{Result, now_as_useconds};

/// Get posts for a board
pub fn list_for_board(conn: &mut SqliteConnection, board_id: i32, limit: i64) -> Result<Vec<Post>> {
    posts::table
        .filter(posts::board_id.eq(board_id))
        .order(posts::created_at_us.desc())
        .limit(limit)
        .load::<Post>(conn)
        .map_err(|e| e.into())
}

/// Create new post
pub fn create(conn: &mut SqliteConnection, board_id: i32, user_id: i32, body: &str) -> Result<Post> {
    let timestamp = now_as_useconds();
    let new_post = NewPost {
        user_id,
        board_id,
        body,
        created_at_us: &timestamp,
    };

    diesel::insert_into(posts::table)
        .values(&new_post)
        .execute(conn)?;

    posts::table
        .order(posts::id.desc())
        .first::<Post>(conn)
        .map_err(|e| e.into())
}

/// Count posts
pub fn count(conn: &mut SqliteConnection) -> i64 {
    posts::table.count().get_result(conn).unwrap_or(0)
}
