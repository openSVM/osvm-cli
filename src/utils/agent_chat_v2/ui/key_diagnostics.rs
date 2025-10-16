//! Key event diagnostic logger for debugging keyboard input
//! This module helps identify which cursive event codes correspond to physical key presses

use cursive::event::{Event, Key};
use cursive::Cursive;
use std::fmt;

/// Format a cursive Event as a readable diagnostic string
pub fn format_event(event: &Event) -> String {
    match event {
        Event::Char(c) => {
            format!("Char('{}') - ASCII {}", c.escape_default(), *c as u32)
        }
        Event::CtrlChar(c) => {
            format!("CtrlChar('{}') - ASCII {} - Ctrl+{}",
                c.escape_default(),
                *c as u32,
                match *c {
                    'a' => "A",
                    'b' => "B",
                    'c' => "C",
                    'd' => "D",
                    'e' => "E",
                    'f' => "F",
                    'g' => "G",
                    'h' => "H",
                    'i' => "I",
                    'j' => "J",
                    'k' => "K",
                    'l' => "L",
                    'm' => "M (Enter?)",
                    'n' => "N",
                    'o' => "O",
                    'p' => "P",
                    'q' => "Q",
                    'r' => "R",
                    's' => "S",
                    't' => "T",
                    'u' => "U",
                    'v' => "V",
                    'w' => "W",
                    'x' => "X",
                    'y' => "Y",
                    'z' => "Z",
                    '[' => "[ (Escape)",
                    '\\' => "\\",
                    ']' => "]",
                    '^' => "^",
                    '_' => "_ (Underscore)",
                    _ => "Unknown"
                }
            )
        }
        Event::AltChar(c) => {
            format!("AltChar('{}') - Alt+{}", c.escape_default(), c)
        }
        Event::Key(k) => {
            format!("Key::{:?}", k)
        }
        Event::Shift(k) => {
            format!("Shift({:?})", k)
        }
        Event::Alt(k) => {
            format!("Alt({:?})", k)
        }
        Event::CtrlShift(k) => {
            format!("CtrlShift({:?})", k)
        }
        Event::AltShift(k) => {
            format!("AltShift({:?})", k)
        }
        Event::WindowResize => "WindowResize".to_string(),
        Event::Refresh => "Refresh".to_string(),
        _ => format!("{:?}", event),
    }
}

/// Create a diagnostic overlay showing recent key presses
pub fn show_key_diagnostics(siv: &mut Cursive) {
    use cursive::views::{Dialog, TextView};

    let diagnostics_text = vec![
        "KEY DIAGNOSTICS - PRESS KEYS TO TEST".to_string(),
        "".to_string(),
        "This panel helps identify which cursive event codes correspond to".to_string(),
        "physical key presses. Check the application logs for event details.".to_string(),
        "".to_string(),
        "Try pressing the keys listed below and watch the logs:".to_string(),
        "  • Ctrl+Enter (what we're trying to fix)".to_string(),
        "  • Ctrl+M".to_string(),
        "  • Ctrl+J".to_string(),
        "  • Regular Enter".to_string(),
        "".to_string(),
        "Watch the logs (run with RUST_LOG=info osvm chat --advanced)".to_string(),
        "to see which event code each key press generates.".to_string(),
        "".to_string(),
        "Close this dialog (Escape or click Close) and press your test keys.".to_string(),
    ].join("\n");

    siv.add_layer(
        Dialog::around(TextView::new(diagnostics_text))
            .title("Key Event Diagnostics")
            .button("Close", |s| { s.pop_layer(); })
    );
}

/// Install a global key event logger for diagnostics
pub fn install_key_logger(siv: &mut Cursive) {
    // Log key presses by adding callbacks for various combinations

    log::info!("╔════════════════════════════════════════════════════════════╗");
    log::info!("║         KEY EVENT DIAGNOSTICS INSTALLED                   ║");
    log::info!("║  Open diagnostics panel with: Ctrl+\\                      ║");
    log::info!("║  Then press keys you want to test (e.g., Ctrl+Enter)      ║");
    log::info!("║  Watch this log for captured events                       ║");
    log::info!("╚════════════════════════════════════════════════════════════╝");

    // Monitor common Ctrl combinations
    siv.add_global_callback(Event::CtrlChar('m'), |_s| {
        log::info!("🔍 EVENT: CtrlChar('m') = Ctrl+M detected");
    });

    siv.add_global_callback(Event::CtrlChar('j'), |_s| {
        log::info!("🔍 EVENT: CtrlChar('j') = Ctrl+J detected");
    });

    siv.add_global_callback(Event::CtrlChar('l'), |_s| {
        log::info!("🔍 EVENT: CtrlChar('l') = Ctrl+L detected");
    });

    siv.add_global_callback(Event::CtrlChar('n'), |_s| {
        log::info!("🔍 EVENT: CtrlChar('n') = Ctrl+N detected");
    });

    siv.add_global_callback(Event::Key(Key::Enter), |_s| {
        log::info!("🔍 EVENT: Key::Enter = Plain Enter detected");
    });

    log::info!("Key logger ready. Go to diagnostics panel (Ctrl+\\) and test your keys.");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_ctrl_char() {
        let event = Event::CtrlChar('m');
        let formatted = format_event(&event);
        assert!(formatted.contains("CtrlChar"));
        assert!(formatted.contains("m"));
    }

    #[test]
    fn test_format_char() {
        let event = Event::Char('x');
        let formatted = format_event(&event);
        assert!(formatted.contains("Char"));
        assert!(formatted.contains("120")); // ASCII for 'x'
    }
}
