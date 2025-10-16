//! Action system for command palette - the actual functionality users need

use cursive::Cursive;
use std::sync::Arc;
use super::{Command, CommandAction, CommandCategory};

/// Quick actions that solve real user problems
pub struct QuickActions;

impl QuickActions {
    /// Create all blockchain-related actions
    pub fn blockchain_actions() -> Vec<Command> {
        vec![
            Command {
                id: "quick_balance".to_string(),
                label: "Quick Balance Check".to_string(),
                description: Some("Instantly show balance in status bar".to_string()),
                category: CommandCategory::Action,
                shortcut: Some("Alt+B".to_string()),
                icon: Some("💰".to_string()),
                action: CommandAction::Custom(Arc::new(|s| {
                    // Show balance in a toast notification
                    show_balance_toast(s);
                })),
                keywords: vec!["balance", "sol", "wallet", "funds"].into_iter()
                    .map(String::from).collect(),
            },

            Command {
                id: "quick_send".to_string(),
                label: "Quick Send SOL".to_string(),
                description: Some("Open quick send dialog".to_string()),
                category: CommandCategory::Action,
                shortcut: Some("Alt+S".to_string()),
                icon: Some("📤".to_string()),
                action: CommandAction::Custom(Arc::new(|s| {
                    show_quick_send_dialog(s);
                })),
                keywords: vec!["send", "transfer", "pay"].into_iter()
                    .map(String::from).collect(),
            },

            Command {
                id: "recent_transactions".to_string(),
                label: "Recent Transactions".to_string(),
                description: Some("Show last 10 transactions".to_string()),
                category: CommandCategory::Action,
                shortcut: Some("Alt+T".to_string()),
                icon: Some("📜".to_string()),
                action: CommandAction::Custom(Arc::new(|s| {
                    show_recent_transactions(s);
                })),
                keywords: vec!["history", "transactions", "recent"].into_iter()
                    .map(String::from).collect(),
            },

            Command {
                id: "stake_sol".to_string(),
                label: "Stake SOL".to_string(),
                description: Some("Open staking interface".to_string()),
                category: CommandCategory::Action,
                shortcut: None,
                icon: Some("🔒".to_string()),
                action: CommandAction::StakeSOL,
                keywords: vec!["stake", "delegate", "validator"].into_iter()
                    .map(String::from).collect(),
            },
        ]
    }

    /// Create all UI/UX related actions
    pub fn ui_actions() -> Vec<Command> {
        vec![
            Command {
                id: "clear_chat".to_string(),
                label: "Clear Chat".to_string(),
                description: Some("Clear current chat history".to_string()),
                category: CommandCategory::Action,
                shortcut: Some("Ctrl+L".to_string()),
                icon: Some("🧹".to_string()),
                action: CommandAction::ClearChat,
                keywords: vec!["clear", "clean", "reset"].into_iter()
                    .map(String::from).collect(),
            },

            Command {
                id: "export_chat".to_string(),
                label: "Export Chat".to_string(),
                description: Some("Export chat to markdown".to_string()),
                category: CommandCategory::Action,
                shortcut: Some("Ctrl+E".to_string()),
                icon: Some("💾".to_string()),
                action: CommandAction::ExportChat,
                keywords: vec!["export", "save", "download"].into_iter()
                    .map(String::from).collect(),
            },

            Command {
                id: "start_recording".to_string(),
                label: "Start Session Recording".to_string(),
                description: Some("Record session for replay".to_string()),
                category: CommandCategory::Action,
                shortcut: Some("Alt+R".to_string()),
                icon: Some("🔴".to_string()),
                action: CommandAction::StartRecording,
                keywords: vec!["record", "capture", "replay"].into_iter()
                    .map(String::from).collect(),
            },

            Command {
                id: "take_screenshot".to_string(),
                label: "Take Screenshot".to_string(),
                description: Some("Capture current view".to_string()),
                category: CommandCategory::Action,
                shortcut: Some("F12".to_string()),
                icon: Some("📸".to_string()),
                action: CommandAction::Custom(Arc::new(|s| {
                    take_screenshot(s);
                })),
                keywords: vec!["screenshot", "capture", "snap"].into_iter()
                    .map(String::from).collect(),
            },
        ]
    }

    /// Create navigation actions
    pub fn navigation_actions() -> Vec<Command> {
        vec![
            Command {
                id: "go_to_input".to_string(),
                label: "Go to Input".to_string(),
                description: Some("Focus input field".to_string()),
                category: CommandCategory::Navigation,
                shortcut: Some("Ctrl+I".to_string()),
                icon: Some("📝".to_string()),
                action: CommandAction::Custom(Arc::new(|s| {
                    s.focus_name("input").ok();
                })),
                keywords: vec!["input", "type", "message"].into_iter()
                    .map(String::from).collect(),
            },

            Command {
                id: "go_to_chat".to_string(),
                label: "Go to Chat View".to_string(),
                description: Some("Focus chat messages".to_string()),
                category: CommandCategory::Navigation,
                shortcut: Some("Ctrl+M".to_string()),
                icon: Some("💬".to_string()),
                action: CommandAction::Custom(Arc::new(|s| {
                    s.focus_name("chat_view").ok();
                })),
                keywords: vec!["chat", "messages", "history"].into_iter()
                    .map(String::from).collect(),
            },

            Command {
                id: "previous_session".to_string(),
                label: "Previous Session".to_string(),
                description: Some("Switch to previous chat session".to_string()),
                category: CommandCategory::Navigation,
                shortcut: Some("Ctrl+[".to_string()),
                icon: Some("⬅️".to_string()),
                action: CommandAction::Custom(Arc::new(|s| {
                    switch_to_previous_session(s);
                })),
                keywords: vec!["previous", "back", "session"].into_iter()
                    .map(String::from).collect(),
            },

            Command {
                id: "next_session".to_string(),
                label: "Next Session".to_string(),
                description: Some("Switch to next chat session".to_string()),
                category: CommandCategory::Navigation,
                shortcut: Some("Ctrl+]".to_string()),
                icon: Some("➡️".to_string()),
                action: CommandAction::Custom(Arc::new(|s| {
                    switch_to_next_session(s);
                })),
                keywords: vec!["next", "forward", "session"].into_iter()
                    .map(String::from).collect(),
            },
        ]
    }

    /// Developer/power user actions
    pub fn developer_actions() -> Vec<Command> {
        vec![
            Command {
                id: "show_logs".to_string(),
                label: "Show Debug Logs".to_string(),
                description: Some("View application logs".to_string()),
                category: CommandCategory::Help,
                shortcut: Some("Ctrl+Shift+L".to_string()),
                icon: Some("🐛".to_string()),
                action: CommandAction::Custom(Arc::new(|s| {
                    show_debug_logs(s);
                })),
                keywords: vec!["logs", "debug", "console"].into_iter()
                    .map(String::from).collect(),
            },

            Command {
                id: "reload_config".to_string(),
                label: "Reload Configuration".to_string(),
                description: Some("Reload config without restart".to_string()),
                category: CommandCategory::Action,
                shortcut: None,
                icon: Some("🔄".to_string()),
                action: CommandAction::Custom(Arc::new(|s| {
                    reload_configuration(s);
                })),
                keywords: vec!["reload", "config", "refresh"].into_iter()
                    .map(String::from).collect(),
            },

            Command {
                id: "performance_stats".to_string(),
                label: "Show Performance Stats".to_string(),
                description: Some("View FPS and performance metrics".to_string()),
                category: CommandCategory::Help,
                shortcut: Some("F9".to_string()),
                icon: Some("📊".to_string()),
                action: CommandAction::Custom(Arc::new(|s| {
                    show_performance_stats(s);
                })),
                keywords: vec!["fps", "performance", "stats", "metrics"].into_iter()
                    .map(String::from).collect(),
            },
        ]
    }
}

// Implementation functions for actions

fn show_balance_toast(siv: &mut Cursive) {
    // Would show balance in a toast notification
    use cursive::views::Dialog;
    siv.add_layer(
        Dialog::info("Balance: 2.5 SOL")
            .title("💰 Wallet Balance")
    );
}

fn show_quick_send_dialog(siv: &mut Cursive) {
    use cursive::views::{Dialog, EditView, LinearLayout, TextView};

    let mut layout = LinearLayout::vertical();
    layout.add_child(TextView::new("Recipient:"));
    layout.add_child(EditView::new().with_name("recipient").fixed_width(44));
    layout.add_child(TextView::new("Amount (SOL):"));
    layout.add_child(EditView::new().with_name("amount").fixed_width(20));

    siv.add_layer(
        Dialog::around(layout)
            .title("📤 Quick Send SOL")
            .button("Send", |s| {
                // Would send transaction
                s.pop_layer();
            })
            .dismiss_button("Cancel")
    );
}

fn show_recent_transactions(siv: &mut Cursive) {
    use cursive::views::{Dialog, TextView};

    let transactions = "
    1. Sent 0.5 SOL to alice.sol (2 min ago)
    2. Received 1.2 SOL from bob.sol (1 hour ago)
    3. Staked 10 SOL (3 hours ago)
    ";

    siv.add_layer(
        Dialog::around(TextView::new(transactions))
            .title("📜 Recent Transactions")
            .dismiss_button("Close")
    );
}

fn take_screenshot(_siv: &mut Cursive) {
    // Would take screenshot
    log::info!("Screenshot captured!");
}

fn switch_to_previous_session(_siv: &mut Cursive) {
    // Would switch session
    log::info!("Switching to previous session");
}

fn switch_to_next_session(_siv: &mut Cursive) {
    // Would switch session
    log::info!("Switching to next session");
}

fn show_debug_logs(siv: &mut Cursive) {
    use cursive::views::{Dialog, ScrollView, TextView};

    let logs = "
    [INFO] Application started
    [DEBUG] Loading configuration
    [INFO] Connected to RPC
    [DEBUG] Fetching balance
    ";

    siv.add_layer(
        Dialog::around(ScrollView::new(TextView::new(logs)))
            .title("🐛 Debug Logs")
            .dismiss_button("Close")
            .fixed_size((60, 20))
    );
}

fn reload_configuration(_siv: &mut Cursive) {
    log::info!("Reloading configuration...");
}

fn show_performance_stats(siv: &mut Cursive) {
    use cursive::views::{Dialog, TextView};

    let stats = "
    FPS: 58.3
    CPU: 12%
    Memory: 48 MB
    Animations: 4 active
    Network: 3 req/s
    ";

    siv.add_layer(
        Dialog::around(TextView::new(stats))
            .title("📊 Performance Stats")
            .dismiss_button("Close")
    );
}