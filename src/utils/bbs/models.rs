use super::schema::{
    board_states, boards, federated_messages, known_peers, mesh_messages, moderators, posts,
    queued_messages, users,
};
use crate::utils::bbs::hex_id_to_num;
use chrono::{Local, MappedLocalTime, TimeZone};
use diesel::prelude::*;
use once_cell::sync::Lazy;
use regex::Regex;
use std::fmt;
use validator::Validate;

static RE_NODE_ID: Lazy<Regex> = Lazy::new(|| Regex::new(r"^![0-9a-f]{8}$").unwrap());
// This seems like a reasonable range to clamp timestamps to. Because we're dealing with
// microseconds, it's good to enforce a plausible range so that things will blow up if we
// inadvertently try to use seconds, milliseconds, or nanoseconds somewhere.
const EARLY_2024: i64 = 1_704_096_000_000_000;
const EARLY_2200: i64 = 7_258_147_200_000_000;

/// Format the number of microseconds since the Unix epoch as a local timestamp.
fn formatted_useconds(dstamp: i64) -> String {
    let fmt = "%Y-%m-%dT%H:%M:%S";
    match Local.timestamp_micros(dstamp) {
        MappedLocalTime::Single(t) => t.format(fmt).to_string(),
        MappedLocalTime::Ambiguous(t1, _) => t1.format(fmt).to_string(),
        MappedLocalTime::None => "No such local time.".to_string(),
    }
}

#[derive(Debug, Queryable, Selectable)]
#[diesel(table_name = crate::utils::bbs::schema::boards)]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct Board {
    pub id: i32,
    pub name: String,
    pub description: String,
    pub created_at_us: i64,
    pub creator_id: Option<i32>, // Owner who can delete
}

impl Board {
    pub fn created_at(&self) -> String {
        formatted_useconds(self.created_at_us)
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{} {}: {}", self.id, self.name, self.description)
    }
}

#[derive(Insertable, Validate)]
#[diesel(table_name = boards)]
pub struct NewBoard<'a> {
    #[validate(length(min = 1, max = 30))]
    pub name: &'a str,
    #[validate(length(min = 1, max = 100))]
    pub description: &'a str,
    #[validate(range(min = EARLY_2024, max=EARLY_2200))]
    pub created_at_us: &'a i64,
    pub creator_id: Option<i32>, // Owner who can delete
}

#[derive(Debug, Queryable, Selectable)]
#[diesel(belongs_to(Board))]
#[diesel(table_name = crate::utils::bbs::schema::posts)]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct Post {
    pub id: i32,
    pub board_id: i32,
    pub user_id: i32,
    pub body: String,
    pub created_at_us: i64,
    pub parent_id: Option<i32>, // For local reply threading (null = top-level post)
    pub federated_parent_id: Option<String>, // For federated parent references (e.g., "!abc123:5")
    pub score: i32,             // Upvotes minus downvotes
}

impl Post {
    pub fn created_at(&self) -> String {
        formatted_useconds(self.created_at_us)
    }

    pub fn is_reply(&self) -> bool {
        self.parent_id.is_some() || self.federated_parent_id.is_some()
    }

    /// Get the unified parent ID (federated takes precedence if set)
    pub fn unified_parent_id(&self) -> Option<String> {
        self.federated_parent_id
            .clone()
            .or_else(|| self.parent_id.map(|id| id.to_string()))
    }
}

#[derive(Insertable, Validate)]
#[diesel(table_name = posts)]
pub struct NewPost<'a> {
    #[validate(range(min = 1))]
    pub user_id: i32,
    #[validate(range(min = 1))]
    pub board_id: i32,
    #[validate(length(min = 1, max = 150))]
    pub body: &'a str,
    #[validate(range(min = EARLY_2024, max=EARLY_2200))]
    pub created_at_us: &'a i64,
    pub parent_id: Option<i32>,               // For local reply threading
    pub federated_parent_id: Option<&'a str>, // For federated parent references
    pub score: i32,                           // Initial score (default 0)
}

#[derive(Debug, Clone, Identifiable, Queryable, Selectable)]
#[diesel(table_name = crate::utils::bbs::schema::users)]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct User {
    pub id: i32,
    pub node_id: String,
    pub short_name: String,
    pub long_name: String,
    pub jackass: bool,
    pub in_board: Option<i32>,
    pub created_at_us: i64,
    pub last_seen_at_us: i64,
    pub last_acted_at_us: Option<i64>,
    pub bio: Option<String>,
}

impl fmt::Display for User {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}:{}", self.node_id, self.short_name, self.long_name)
    }
}

impl User {
    pub fn node_id_numeric(&self) -> u32 {
        hex_id_to_num(&self.node_id).expect("node_ids in the database should always be valid")
    }
    pub fn created_at(&self) -> String {
        formatted_useconds(self.created_at_us)
    }
    pub fn last_acted_at(&self) -> String {
        if let Some(acted) = self.last_acted_at_us {
            formatted_useconds(acted)
        } else {
            String::new()
        }
    }
    pub fn last_seen_at(&self) -> String {
        formatted_useconds(self.last_seen_at_us)
    }
}

#[derive(Insertable, Validate)]
#[diesel(table_name = users)]
pub struct UserNew<'a> {
    #[validate(regex(path = *RE_NODE_ID))]
    pub node_id: &'a str,
    #[validate(length(min = 1, max = 4))]
    pub short_name: &'a str,
    #[validate(length(min = 1, max = 40))]
    pub long_name: &'a str,
    #[validate(range(min = EARLY_2024, max=EARLY_2200))]
    pub created_at_us: &'a i64,
    #[validate(range(min = EARLY_2024, max=EARLY_2200))]
    pub last_seen_at_us: &'a i64,
    #[validate(range(min = EARLY_2024, max=EARLY_2200))]
    pub last_acted_at_us: Option<&'a i64>,
}

#[derive(AsChangeset, Insertable, Validate)]
#[diesel(table_name = users)]
pub struct UserUpdate<'a> {
    #[validate(length(min = 1, max = 4))]
    pub short_name: Option<&'a str>,
    #[validate(length(min = 1, max = 40))]
    pub long_name: Option<&'a str>,
    #[validate(range(min = EARLY_2024, max=EARLY_2200))]
    pub last_seen_at_us: Option<&'a i64>,
    #[validate(range(min = EARLY_2024, max=EARLY_2200))]
    pub last_acted_at_us: Option<&'a i64>,
    #[validate(length(min = 0, max = 200))]
    pub bio: Option<String>,
}

#[derive(Debug, Identifiable, Queryable, Selectable)]
#[diesel(table_name = crate::utils::bbs::schema::board_states)]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct BoardState {
    pub id: i32,
    pub user_id: i32,
    pub board_id: i32,
    pub last_post_us: i64,
}

#[derive(Debug, Insertable, Validate)]
#[diesel(table_name = board_states)]
pub struct NewBoardState {
    #[validate(range(min = 1))]
    pub user_id: i32,
    #[validate(range(min = 1))]
    pub board_id: i32,
    #[validate(range(min = EARLY_2024, max=EARLY_2200))]
    pub last_post_us: i64,
}

#[derive(Debug, Identifiable, Queryable, Selectable)]
#[diesel(table_name = crate::utils::bbs::schema::queued_messages)]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct QueuedMessage {
    pub id: i32,
    pub sender_id: i32,
    pub recipient_id: i32,
    pub body: String,
    pub created_at_us: i64,
    pub sent_at_us: Option<i64>,
}

impl QueuedMessage {
    pub fn created_at(&self) -> String {
        formatted_useconds(self.created_at_us)
    }
}

#[derive(Insertable, Validate)]
#[diesel(table_name = crate::utils::bbs::schema::queued_messages)]
pub struct QueuedMessageNew<'a> {
    #[validate(range(min = 1))]
    pub sender_id: i32,
    #[validate(range(min = 1))]
    pub recipient_id: i32,
    #[validate(length(min = 1, max = 200))]
    pub body: &'a str,
    #[validate(range(min = EARLY_2024, max=EARLY_2200))]
    pub created_at_us: &'a i64,
}

// ============================================
// Moderator - for board permission management
// ============================================

#[derive(Debug, Queryable, Selectable)]
#[diesel(table_name = crate::utils::bbs::schema::moderators)]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct Moderator {
    pub id: i32,
    pub user_id: i32,
    pub board_id: i32,
    pub granted_by: i32,
    pub granted_at_us: i64,
}

impl Moderator {
    pub fn granted_at(&self) -> String {
        formatted_useconds(self.granted_at_us)
    }
}

#[derive(Insertable, Validate)]
#[diesel(table_name = moderators)]
pub struct NewModerator {
    #[validate(range(min = 1))]
    pub user_id: i32,
    #[validate(range(min = 1))]
    pub board_id: i32,
    #[validate(range(min = 1))]
    pub granted_by: i32,
    #[validate(range(min = EARLY_2024, max=EARLY_2200))]
    pub granted_at_us: i64,
}

// ============================================
// FederatedMessage - for messages from other nodes
// ============================================

#[derive(Debug, Queryable, Selectable, Clone)]
#[diesel(table_name = crate::utils::bbs::schema::federated_messages)]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct FederatedMessageDb {
    pub id: i32,
    pub message_id: String,        // Unique: origin_node:local_id
    pub origin_node: String,       // Node that created this message
    pub board: String,             // Board name (uppercase)
    pub author_node: String,       // Author's node ID
    pub author_name: String,       // Author's display name
    pub body: String,              // Message content
    pub parent_id: Option<String>, // Parent message_id for replies
    pub created_at: i64,           // Original creation timestamp (seconds)
    pub received_at: i64,          // When we received this message (seconds)
    pub signature: Option<String>, // Optional signature
}

#[derive(Insertable)]
#[diesel(table_name = federated_messages)]
pub struct NewFederatedMessage<'a> {
    pub message_id: &'a str,
    pub origin_node: &'a str,
    pub board: &'a str,
    pub author_node: &'a str,
    pub author_name: &'a str,
    pub body: &'a str,
    pub parent_id: Option<&'a str>,
    pub created_at: i64,
    pub received_at: i64,
    pub signature: Option<&'a str>,
}

// ============================================
// KnownPeer - for peer persistence
// ============================================

#[derive(Debug, Queryable, Selectable, Clone)]
#[diesel(table_name = crate::utils::bbs::schema::known_peers)]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct KnownPeerDb {
    pub id: i32,
    pub node_id: String,
    pub address: String,
    pub name: Option<String>,
    pub last_sync: Option<i64>,
    pub last_seen: Option<i64>,
    pub failure_count: i32,
    pub is_bootstrap: bool,
}

#[derive(Insertable)]
#[diesel(table_name = known_peers)]
pub struct NewKnownPeer<'a> {
    pub node_id: &'a str,
    pub address: &'a str,
    pub name: Option<&'a str>,
    pub last_sync: Option<i64>,
    pub last_seen: Option<i64>,
    pub failure_count: i32,
    pub is_bootstrap: bool,
}

// ============================================
// MeshMessage - for Meshtastic radio messages
// ============================================

#[derive(Debug, Queryable, Selectable, Clone)]
#[diesel(table_name = crate::utils::bbs::schema::mesh_messages)]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct MeshMessageDb {
    pub id: i32,
    pub from_node_id: i64,            // Meshtastic node ID (u32 stored as i64)
    pub from_name: Option<String>,    // Sender's display name
    pub to_node_id: Option<i64>,      // Destination (null = broadcast)
    pub channel: i32,                 // Meshtastic channel
    pub body: String,                 // Message content
    pub is_command: bool,             // Was this a /command message?
    pub received_at_us: i64,          // When we received this (microseconds)
    pub response: Option<String>,     // Our response (if any)
    pub responded_at_us: Option<i64>, // When we responded
}

impl MeshMessageDb {
    pub fn received_at(&self) -> String {
        formatted_useconds(self.received_at_us)
    }

    pub fn from_node_hex(&self) -> String {
        format!("!{:08x}", self.from_node_id as u32)
    }

    pub fn to_node_hex(&self) -> Option<String> {
        self.to_node_id.map(|id| format!("!{:08x}", id as u32))
    }
}

#[derive(Insertable)]
#[diesel(table_name = mesh_messages)]
pub struct NewMeshMessage<'a> {
    pub from_node_id: i64,
    pub from_name: Option<&'a str>,
    pub to_node_id: Option<i64>,
    pub channel: i32,
    pub body: &'a str,
    pub is_command: bool,
    pub received_at_us: i64,
    pub response: Option<&'a str>,
    pub responded_at_us: Option<i64>,
}

#[derive(AsChangeset)]
#[diesel(table_name = mesh_messages)]
pub struct MeshMessageUpdate {
    pub response: Option<String>,
    pub responded_at_us: Option<i64>,
}

// ============================================
// UserVote - for tracking user votes on posts
// ============================================

#[derive(Debug, Queryable, Selectable)]
#[diesel(table_name = crate::utils::bbs::schema::user_votes)]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct UserVote {
    pub id: i32,
    pub user_id: i32,
    pub post_id: i32,
    pub vote_type: i32, // 1 = upvote, -1 = downvote
    pub created_at_us: i64,
}

#[derive(Insertable)]
#[diesel(table_name = crate::utils::bbs::schema::user_votes)]
pub struct NewUserVote {
    pub user_id: i32,
    pub post_id: i32,
    pub vote_type: i32,
    pub created_at_us: i64,
}
