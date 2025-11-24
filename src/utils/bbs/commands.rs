// BBS command system
// Simplified command handling for OSVM integration

use crate::utils::bbs::{db, models::User};
use diesel::sqlite::SqliteConnection;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

/// BBS command result
#[derive(Debug)]
pub struct CommandResult {
    pub output: Vec<String>,
    pub broadcast: bool,  // If true, send to public channel
}

impl CommandResult {
    pub fn private(output: Vec<String>) -> Self {
        Self {
            output,
            broadcast: false,
        }
    }

    pub fn broadcast(output: Vec<String>) -> Self {
        Self {
            output,
            broadcast: true,
        }
    }
}

/// Execute a BBS command
pub fn execute(
    conn: &mut SqliteConnection,
    user: &User,
    command: &str,
) -> Result<CommandResult> {
    let cmd = command.trim().to_lowercase();

    match cmd.as_str() {
        "help" | "h" => Ok(CommandResult::private(vec![
            "OSVM BBS Commands:".to_string(),
            "  boards, b      - List all message boards".to_string(),
            "  enter <id>     - Enter a board".to_string(),
            "  read, r        - Read posts in current board".to_string(),
            "  post <text>    - Post to current board".to_string(),
            "  users          - List active users".to_string(),
            "  help, h        - Show this help".to_string(),
        ])),

        "boards" | "b" => {
            let boards = db::boards::list(conn)?;
            let mut output = vec!["Message Boards:".to_string()];
            for board in boards {
                output.push(format!("  {} - {}: {}", board.id, board.name, board.description));
            }
            Ok(CommandResult::private(output))
        }

        "users" => {
            let (total, active) = db::users::counts(conn);
            Ok(CommandResult::private(vec![
                format!("Total users: {}", total),
                format!("Active users: {}", active),
            ]))
        }

        _ if cmd.starts_with("enter ") => {
            let board_id: i32 = cmd.strip_prefix("enter ")
                .and_then(|s| s.trim().parse().ok())
                .ok_or("Invalid board ID")?;

            let board = db::boards::get(conn, board_id)?;
            Ok(CommandResult::private(vec![
                format!("Entered board: {}", board.name),
                format!("{}", board.description),
            ]))
        }

        _ if cmd.starts_with("post ") => {
            let text = cmd.strip_prefix("post ").ok_or("No message text")?;
            // TODO: Implement posting
            Ok(CommandResult::broadcast(vec![
                format!("{} posted: {}", user.short_name, text),
            ]))
        }

        _ => Ok(CommandResult::private(vec![
            "Unknown command. Type 'help' for available commands.".to_string(),
        ])),
    }
}
