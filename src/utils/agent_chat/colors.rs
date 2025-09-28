//! ANSI color codes for terminal output

/// ANSI color codes for better readability
pub struct Colors;

impl Colors {
    pub const RESET: &'static str = "\x1b[0m";
    pub const CYAN: &'static str = "\x1b[36m";      // System messages
    pub const GREEN: &'static str = "\x1b[32m";     // User input, success
    pub const YELLOW: &'static str = "\x1b[33m";    // Tool names
    pub const RED: &'static str = "\x1b[31m";       // Errors
    pub const BLUE: &'static str = "\x1b[34m";      // Server names
    pub const MAGENTA: &'static str = "\x1b[35m";   // Special actions
    pub const DIM: &'static str = "\x1b[2m";        // Parameters, less important
    pub const BOLD: &'static str = "\x1b[1m";       // Emphasis
    pub const GRAY: &'static str = "\x1b[90m";      // Auto-complete suggestions
}