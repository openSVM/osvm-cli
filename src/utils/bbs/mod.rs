// FrozenBBS Integration for OSVM
// Adapted from https://github.com/kstrauser/frozenbbs
//
// This module integrates Meshtastic-based bulletin board system (BBS)
// for communication between humans and AI agents over low-bandwidth radio networks.

pub mod db;
pub mod models;
pub mod schema;
pub mod commands;
pub mod message_router;
pub mod agent_bridge;
pub mod tui_widgets;

use std::path::PathBuf;
use dirs::data_dir;

pub const BBS_TAG: &str = "osvm-bbs";

/// Get the path to the BBS database file
pub fn db_path() -> PathBuf {
    data_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join(".osvm")
        .join("bbs")
        .join("osvm-bbs.db")
}

/// Convert a node ID like 12345678 or !abcdef12 to their u32 value.
pub fn hex_id_to_num(node_id: &str) -> Option<u32> {
    let node_id = if node_id.starts_with('!') {
        node_id.strip_prefix('!').unwrap()
    } else {
        node_id
    };
    if node_id.len() != 8 {
        return None;
    }
    u32::from_str_radix(node_id, 16).ok()
}

/// Convert a u32 node ID to its canonical !abcdef12 format.
pub fn num_id_to_hex(node_num: u32) -> String {
    format!("!{node_num:08x}")
}

/// Convert a possibly mixed case node ID, with or without the leading !, to its canonical format.
pub fn canonical_node_id(node_id: &str) -> Option<String> {
    Some(num_id_to_hex(hex_id_to_num(node_id)?))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn user_id_with_leading_zero() {
        assert_eq!(num_id_to_hex(0x00010203), "!00010203");
    }

    #[test]
    fn hex_conversion() {
        assert_eq!(hex_id_to_num("!cafeb33d"), Some(0xcafeb33d));
        assert_eq!(hex_id_to_num("cafeb33d"), Some(0xcafeb33d));
        assert_eq!(canonical_node_id("CAFEB33D"), Some("!cafeb33d".to_string()));
    }
}
