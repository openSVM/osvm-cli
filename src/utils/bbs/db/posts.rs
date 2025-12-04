// Post database operations

use super::{now_as_useconds, Result};
use crate::utils::bbs::{models::*, schema::posts};
use diesel::prelude::*;

/// Get posts for a board (all posts, ordered by creation time descending)
pub fn list_for_board(conn: &mut SqliteConnection, board_id: i32, limit: i64) -> Result<Vec<Post>> {
    posts::table
        .filter(posts::board_id.eq(board_id))
        .order(posts::created_at_us.desc())
        .limit(limit)
        .load::<Post>(conn)
        .map_err(|e| e.into())
}

/// Get only top-level posts for a board (no parent)
pub fn list_top_level(conn: &mut SqliteConnection, board_id: i32, limit: i64) -> Result<Vec<Post>> {
    posts::table
        .filter(posts::board_id.eq(board_id))
        .filter(posts::parent_id.is_null())
        .order(posts::created_at_us.desc())
        .limit(limit)
        .load::<Post>(conn)
        .map_err(|e| e.into())
}

/// Get replies to a specific post
pub fn list_replies(conn: &mut SqliteConnection, parent_id: i32) -> Result<Vec<Post>> {
    posts::table
        .filter(posts::parent_id.eq(parent_id))
        .order(posts::created_at_us.asc()) // Replies in chronological order
        .load::<Post>(conn)
        .map_err(|e| e.into())
}

/// Get a post by ID
pub fn get(conn: &mut SqliteConnection, post_id: i32) -> Result<Post> {
    posts::table
        .find(post_id)
        .first::<Post>(conn)
        .map_err(|e| e.into())
}

/// Create new top-level post
pub fn create(
    conn: &mut SqliteConnection,
    board_id: i32,
    user_id: i32,
    body: &str,
) -> Result<Post> {
    create_full(conn, board_id, user_id, body, None, None)
}

/// Create new post (can be top-level or reply to local post)
pub fn create_with_parent(
    conn: &mut SqliteConnection,
    board_id: i32,
    user_id: i32,
    body: &str,
    parent_id: Option<i32>,
) -> Result<Post> {
    create_full(conn, board_id, user_id, body, parent_id, None)
}

/// Create new post with federated parent reference
pub fn create_with_federated_parent(
    conn: &mut SqliteConnection,
    board_id: i32,
    user_id: i32,
    body: &str,
    federated_parent_id: &str,
) -> Result<Post> {
    create_full(
        conn,
        board_id,
        user_id,
        body,
        None,
        Some(federated_parent_id),
    )
}

/// Create new post (full version with all parent options)
pub fn create_full(
    conn: &mut SqliteConnection,
    board_id: i32,
    user_id: i32,
    body: &str,
    parent_id: Option<i32>,
    federated_parent_id: Option<&str>,
) -> Result<Post> {
    let timestamp = now_as_useconds();
    let new_post = NewPost {
        user_id,
        board_id,
        body,
        created_at_us: &timestamp,
        parent_id,
        federated_parent_id,
        score: 0,
    };

    diesel::insert_into(posts::table)
        .values(&new_post)
        .execute(conn)?;

    posts::table
        .order(posts::id.desc())
        .first::<Post>(conn)
        .map_err(|e| e.into())
}

/// Create a reply to an existing local post
pub fn reply(
    conn: &mut SqliteConnection,
    parent_post_id: i32,
    user_id: i32,
    body: &str,
) -> Result<Post> {
    // Get the parent post to determine the board_id
    let parent = get(conn, parent_post_id)?;
    create_with_parent(conn, parent.board_id, user_id, body, Some(parent_post_id))
}

/// Update the score of a post (for upvote/downvote)
pub fn update_score(conn: &mut SqliteConnection, post_id: i32, delta: i32) -> Result<i32> {
    use diesel::dsl::*;
    diesel::update(posts::table.find(post_id))
        .set(posts::score.eq(posts::score + delta))
        .execute(conn)?;

    posts::table
        .find(post_id)
        .select(posts::score)
        .first::<i32>(conn)
        .map_err(|e| e.into())
}

/// Delete a post (only by author)
/// Also deletes all replies to this post
pub fn delete(conn: &mut SqliteConnection, post_id: i32, requester_user_id: i32) -> Result<bool> {
    let post = get(conn, post_id)?;

    // Only the post author can delete
    if post.user_id != requester_user_id {
        return Ok(false);
    }

    // Delete all replies first (recursive cascade)
    delete_replies(conn, post_id)?;

    // Delete the post itself
    diesel::delete(posts::table.find(post_id)).execute(conn)?;

    Ok(true)
}

/// Delete all replies to a post (recursive helper)
fn delete_replies(conn: &mut SqliteConnection, parent_id: i32) -> Result<()> {
    // Get all direct replies
    let replies = list_replies(conn, parent_id)?;

    // Delete their replies first (recursive)
    for reply in replies {
        delete_replies(conn, reply.id)?;
    }

    // Delete direct replies
    diesel::delete(posts::table.filter(posts::parent_id.eq(parent_id))).execute(conn)?;

    Ok(())
}

/// Count replies to a post
pub fn reply_count(conn: &mut SqliteConnection, post_id: i32) -> i64 {
    posts::table
        .filter(posts::parent_id.eq(post_id))
        .count()
        .get_result(conn)
        .unwrap_or(0)
}

/// Count all posts
pub fn count(conn: &mut SqliteConnection) -> i64 {
    posts::table.count().get_result(conn).unwrap_or(0)
}

/// Get thread (parent post + all replies) in hierarchical order
pub fn get_thread(conn: &mut SqliteConnection, post_id: i32) -> Result<Vec<Post>> {
    // Find the root post (walk up the parent chain)
    let mut current = get(conn, post_id)?;
    while let Some(parent_id) = current.parent_id {
        current = get(conn, parent_id)?;
    }

    // Now current is the root post - get all descendants
    let mut thread = vec![current];
    collect_replies(conn, post_id, &mut thread)?;
    Ok(thread)
}

/// Helper to collect replies recursively
fn collect_replies(
    conn: &mut SqliteConnection,
    parent_id: i32,
    thread: &mut Vec<Post>,
) -> Result<()> {
    let replies = list_replies(conn, parent_id)?;
    for reply in replies {
        let reply_id = reply.id;
        thread.push(reply);
        collect_replies(conn, reply_id, thread)?;
    }
    Ok(())
}
