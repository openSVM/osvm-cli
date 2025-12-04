// User database operations

use super::Result;
use crate::utils::bbs::{models::*, schema::users};
use diesel::prelude::*;

/// Get user by node ID
pub fn get(conn: &mut SqliteConnection, node_id: &str) -> Result<User> {
    users::table
        .filter(users::node_id.eq(node_id))
        .first::<User>(conn)
        .map_err(|e| e.into())
}

/// Get user by internal ID
pub fn get_by_user_id(conn: &mut SqliteConnection, user_id: i32) -> Result<User> {
    users::table
        .find(user_id)
        .first::<User>(conn)
        .map_err(|e| e.into())
}

/// Record a user observation (create or update)
pub fn observe(
    conn: &mut SqliteConnection,
    node_id: &str,
    short_name: Option<&str>,
    long_name: Option<&str>,
    seen_at_us: i64,
) -> Result<(User, bool)> {
    use diesel::dsl::now;

    // Try to get existing user
    match get(conn, node_id) {
        Ok(user) => {
            // Update existing user
            diesel::update(users::table.filter(users::node_id.eq(node_id)))
                .set((
                    users::last_seen_at_us.eq(seen_at_us),
                    users::short_name.eq(short_name.unwrap_or(&user.short_name)),
                    users::long_name.eq(long_name.unwrap_or(&user.long_name)),
                ))
                .execute(conn)?;

            let updated_user = get(conn, node_id)?;
            Ok((updated_user, true))
        }
        Err(_) => {
            // Create new user
            let new_user = UserNew {
                node_id,
                short_name: short_name.unwrap_or("????"),
                long_name: long_name.unwrap_or("Unknown User"),
                created_at_us: &seen_at_us,
                last_seen_at_us: &seen_at_us,
                last_acted_at_us: None,
            };

            diesel::insert_into(users::table)
                .values(&new_user)
                .execute(conn)?;

            let user = get(conn, node_id)?;
            Ok((user, false))
        }
    }
}

/// Count users
pub fn counts(conn: &mut SqliteConnection) -> (i64, i64) {
    let total = users::table.count().get_result(conn).unwrap_or(0);
    let active = users::table
        .filter(users::jackass.eq(false))
        .count()
        .get_result(conn)
        .unwrap_or(0);
    (total, active)
}

/// List all users
pub fn list_all(conn: &mut SqliteConnection) -> Result<Vec<User>> {
    users::table
        .order(users::last_seen_at_us.desc())
        .load::<User>(conn)
        .map_err(|e| e.into())
}
