//! Common TUI utilities shared across all TUI applications
//!
//! This module consolidates duplicate code that was previously scattered
//! across individual TUI modules (degen, amm, perp, ide).

use ratatui::layout::{Constraint, Direction, Layout, Rect};
use solana_sdk::signer::{keypair::Keypair, Signer};
use std::time::Duration;

// ============================================================================
// Layout Helpers
// ============================================================================

/// Create a centered rectangle within a parent rectangle.
///
/// This is commonly used for modal dialogs and popups.
///
/// # Arguments
/// * `percent_x` - Width as percentage of parent (0-100)
/// * `percent_y` - Height as percentage of parent (0-100)
/// * `r` - Parent rectangle
///
/// # Example
/// ```ignore
/// let popup_area = centered_rect(50, 40, f.area());
/// f.render_widget(Clear, popup_area);
/// f.render_widget(my_popup, popup_area);
/// ```
pub fn centered_rect(percent_x: u16, percent_y: u16, r: Rect) -> Rect {
    let popup_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage((100 - percent_y) / 2),
            Constraint::Percentage(percent_y),
            Constraint::Percentage((100 - percent_y) / 2),
        ])
        .split(r);

    Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage((100 - percent_x) / 2),
            Constraint::Percentage(percent_x),
            Constraint::Percentage((100 - percent_x) / 2),
        ])
        .split(popup_layout[1])[1]
}

// ============================================================================
// Formatting Helpers
// ============================================================================

/// Format a USD value with appropriate suffix (K, M, B).
///
/// # Examples
/// * `1_500_000_000.0` → `"$1.5B"`
/// * `45_000_000.0` → `"$45.0M"`
/// * `125_000.0` → `"$125.0K"`
/// * `500.0` → `"$500.00"`
pub fn format_usd(value: f64) -> String {
    if value >= 1_000_000_000.0 {
        format!("${:.1}B", value / 1_000_000_000.0)
    } else if value >= 1_000_000.0 {
        format!("${:.1}M", value / 1_000_000.0)
    } else if value >= 1_000.0 {
        format!("${:.1}K", value / 1_000.0)
    } else {
        format!("${:.2}", value)
    }
}

/// Format a price with appropriate decimal precision.
///
/// # Examples
/// * `95000.0` → `"$95000.00"` (large values: 2 decimals)
/// * `180.25` → `"$180.2500"` (medium values: 4 decimals)
/// * `0.0000325` → `"$0.00003250"` (small values: 8 decimals)
pub fn format_price(value: f64) -> String {
    if value >= 1000.0 {
        format!("${:.2}", value)
    } else if value >= 1.0 {
        format!("${:.4}", value)
    } else if value >= 0.001 {
        format!("${:.6}", value)
    } else {
        format!("${:.8}", value)
    }
}

/// Format a duration as human-readable string.
///
/// # Examples
/// * 3661 seconds → `"1h 1m 1s"`
/// * 125 seconds → `"2m 5s"`
/// * 45 seconds → `"45s"`
pub fn format_duration(d: Duration) -> String {
    let secs = d.as_secs();
    let hours = secs / 3600;
    let mins = (secs % 3600) / 60;
    let secs = secs % 60;

    if hours > 0 {
        format!("{}h {}m {}s", hours, mins, secs)
    } else if mins > 0 {
        format!("{}m {}s", mins, secs)
    } else {
        format!("{}s", secs)
    }
}

/// Format a SOL amount with appropriate precision.
///
/// # Examples
/// * `1.5` → `"1.5000 SOL"`
/// * `0.001` → `"0.0010 SOL"`
pub fn format_sol(value: f64) -> String {
    format!("{:.4} SOL", value)
}

/// Format a percentage value.
///
/// # Examples
/// * `3.5` → `"+3.5%"` (positive)
/// * `-2.1` → `"-2.1%"` (negative)
pub fn format_pct(value: f64) -> String {
    if value >= 0.0 {
        format!("+{:.1}%", value)
    } else {
        format!("{:.1}%", value)
    }
}

// ============================================================================
// Wallet Helpers
// ============================================================================

/// Load wallet address from the default Solana keypair location.
///
/// Checks:
/// 1. `SOLANA_KEYPAIR` environment variable
/// 2. `~/.config/solana/id.json` default path
///
/// Returns `None` if:
/// - No keypair file found
/// - File cannot be parsed as JSON
/// - Keypair is not 64 bytes
pub fn load_wallet_address() -> Option<String> {
    let keypair_path = std::env::var("SOLANA_KEYPAIR")
        .ok()
        .or_else(|| {
            dirs::home_dir().map(|h| {
                h.join(".config/solana/id.json")
                    .to_string_lossy()
                    .to_string()
            })
        })?;

    let keypair_bytes = std::fs::read_to_string(&keypair_path).ok()?;
    let keypair_vec: Vec<u8> = serde_json::from_str(&keypair_bytes).ok()?;

    if keypair_vec.len() != 64 {
        return None;
    }

    // Derive pubkey from secret key (first 32 bytes)
    let mut secret_key = [0u8; 32];
    secret_key.copy_from_slice(&keypair_vec[..32]);
    let keypair = Keypair::new_from_array(secret_key);

    Some(keypair.pubkey().to_string())
}

/// Truncate a wallet address for display (e.g., "ABC1...XYZ9").
///
/// # Arguments
/// * `address` - Full base58 wallet address
/// * `prefix_len` - Number of characters from start (default: 4)
/// * `suffix_len` - Number of characters from end (default: 4)
pub fn truncate_address(address: &str, prefix_len: usize, suffix_len: usize) -> String {
    if address.len() <= prefix_len + suffix_len + 3 {
        return address.to_string();
    }
    format!(
        "{}...{}",
        &address[..prefix_len],
        &address[address.len() - suffix_len..]
    )
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_usd() {
        assert_eq!(format_usd(1_500_000_000.0), "$1.5B");
        assert_eq!(format_usd(45_000_000.0), "$45.0M");
        assert_eq!(format_usd(125_000.0), "$125.0K");
        assert_eq!(format_usd(500.0), "$500.00");
        assert_eq!(format_usd(0.50), "$0.50");
    }

    #[test]
    fn test_format_price() {
        assert_eq!(format_price(95000.0), "$95000.00");
        assert_eq!(format_price(180.25), "$180.2500");
        assert_eq!(format_price(0.0000325), "$0.00003250");
    }

    #[test]
    fn test_format_duration() {
        assert_eq!(format_duration(Duration::from_secs(45)), "45s");
        assert_eq!(format_duration(Duration::from_secs(125)), "2m 5s");
        assert_eq!(format_duration(Duration::from_secs(3661)), "1h 1m 1s");
    }

    #[test]
    fn test_format_pct() {
        assert_eq!(format_pct(3.5), "+3.5%");
        assert_eq!(format_pct(-2.1), "-2.1%");
        assert_eq!(format_pct(0.0), "+0.0%");
    }

    #[test]
    fn test_truncate_address() {
        let addr = "5Q544fKrFoe6tsEbD7S8EmxGTJYAKtTVhAW5Q5pge4j1";
        assert_eq!(truncate_address(addr, 4, 4), "5Q54...e4j1");
        assert_eq!(truncate_address("short", 4, 4), "short");
    }

    #[test]
    fn test_centered_rect() {
        let parent = Rect::new(0, 0, 100, 50);
        let child = centered_rect(50, 50, parent);

        // Should be centered horizontally and vertically
        assert!(child.x > 0);
        assert!(child.y > 0);
        assert!(child.width < parent.width);
        assert!(child.height < parent.height);
    }
}
