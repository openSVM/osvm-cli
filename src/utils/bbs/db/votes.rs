//! Vote tracking database operations

use diesel::prelude::*;
use diesel::sqlite::SqliteConnection;

use super::{now_as_useconds, Result};
use crate::utils::bbs::models::{NewUserVote, UserVote};
use crate::utils::bbs::schema::user_votes;

/// Vote result with info about what happened
#[derive(Debug)]
pub enum VoteResult {
    /// New vote was recorded
    Voted { new_score: i32 },
    /// User changed their vote (e.g., upvote to downvote)
    Changed { new_score: i32, old_vote: i32 },
    /// User is voting the same way again (toggle = remove vote)
    Removed { new_score: i32 },
}

/// Get a user's existing vote on a post
pub fn get_user_vote(conn: &mut SqliteConnection, user_id: i32, post_id: i32) -> Option<UserVote> {
    user_votes::table
        .filter(user_votes::user_id.eq(user_id))
        .filter(user_votes::post_id.eq(post_id))
        .first(conn)
        .ok()
}

/// Cast a vote on a post. Handles all cases:
/// - New vote: creates vote record, adjusts post score
/// - Same vote again: removes vote (toggle behavior like Reddit)
/// - Different vote: changes vote, adjusts post score by 2 (remove old, add new)
pub fn cast_vote(
    conn: &mut SqliteConnection,
    user_id: i32,
    post_id: i32,
    vote_type: i32, // 1 = upvote, -1 = downvote
) -> Result<VoteResult> {
    use crate::utils::bbs::schema::posts;

    // Check for existing vote
    if let Some(existing) = get_user_vote(conn, user_id, post_id) {
        if existing.vote_type == vote_type {
            // Same vote again = toggle off (remove vote)
            diesel::delete(user_votes::table.find(existing.id)).execute(conn)?;

            // Adjust post score by removing the vote
            diesel::update(posts::table.find(post_id))
                .set(posts::score.eq(posts::score - vote_type))
                .execute(conn)?;

            let new_score = posts::table
                .find(post_id)
                .select(posts::score)
                .first::<i32>(conn)?;

            Ok(VoteResult::Removed { new_score })
        } else {
            // Different vote = change vote
            let old_vote = existing.vote_type;

            diesel::update(user_votes::table.find(existing.id))
                .set((
                    user_votes::vote_type.eq(vote_type),
                    user_votes::created_at_us.eq(now_as_useconds()),
                ))
                .execute(conn)?;

            // Adjust post score: remove old vote, add new vote = net change of 2 * vote_type
            // If old was -1 and new is +1: score += 2
            // If old was +1 and new is -1: score -= 2
            let delta = vote_type - old_vote;
            diesel::update(posts::table.find(post_id))
                .set(posts::score.eq(posts::score + delta))
                .execute(conn)?;

            let new_score = posts::table
                .find(post_id)
                .select(posts::score)
                .first::<i32>(conn)?;

            Ok(VoteResult::Changed {
                new_score,
                old_vote,
            })
        }
    } else {
        // New vote
        let new_vote = NewUserVote {
            user_id,
            post_id,
            vote_type,
            created_at_us: now_as_useconds(),
        };

        diesel::insert_into(user_votes::table)
            .values(&new_vote)
            .execute(conn)?;

        // Adjust post score
        diesel::update(posts::table.find(post_id))
            .set(posts::score.eq(posts::score + vote_type))
            .execute(conn)?;

        let new_score = posts::table
            .find(post_id)
            .select(posts::score)
            .first::<i32>(conn)?;

        Ok(VoteResult::Voted { new_score })
    }
}

/// Get all votes by a user
pub fn get_votes_by_user(conn: &mut SqliteConnection, user_id: i32) -> Result<Vec<UserVote>> {
    user_votes::table
        .filter(user_votes::user_id.eq(user_id))
        .load(conn)
        .map_err(|e| e.into())
}

/// Get vote counts for a post
pub fn get_vote_counts(conn: &mut SqliteConnection, post_id: i32) -> (i64, i64) {
    let upvotes = user_votes::table
        .filter(user_votes::post_id.eq(post_id))
        .filter(user_votes::vote_type.eq(1))
        .count()
        .get_result::<i64>(conn)
        .unwrap_or(0);

    let downvotes = user_votes::table
        .filter(user_votes::post_id.eq(post_id))
        .filter(user_votes::vote_type.eq(-1))
        .count()
        .get_result::<i64>(conn)
        .unwrap_or(0);

    (upvotes, downvotes)
}
