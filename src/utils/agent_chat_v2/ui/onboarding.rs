//! First-time user onboarding and tips

use cursive::traits::Resizable;
use cursive::views::{Dialog, LinearLayout, TextView};
use cursive::Cursive;
use std::path::PathBuf;

/// Check if user has seen the welcome dialog before
fn has_seen_welcome() -> bool {
    if let Ok(flag_path) = get_welcome_flag_path() {
        flag_path.exists()
    } else {
        true // If we can't check, assume they've seen it
    }
}

/// Mark welcome dialog as seen
fn mark_welcome_seen() -> Result<(), std::io::Error> {
    if let Ok(flag_path) = get_welcome_flag_path() {
        if let Some(parent) = flag_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(flag_path, "1")?;
    }
    Ok(())
}

/// Get path to welcome flag file
fn get_welcome_flag_path() -> Result<PathBuf, std::env::VarError> {
    let home = std::env::var("HOME")?;
    Ok(PathBuf::from(home)
        .join(".config")
        .join("osvm")
        .join(".chat_welcome_shown"))
}

/// Show welcome dialog for first-time users
pub fn show_welcome_dialog_if_first_time(siv: &mut Cursive) {
    if !has_seen_welcome() {
        show_welcome_dialog(siv);
        let _ = mark_welcome_seen();
    }
}

/// Show welcome/onboarding dialog
pub fn show_welcome_dialog(siv: &mut Cursive) {
    let welcome_text = "🎉 Welcome to OSVM Advanced Agent Chat!\n\n\
        This is your AI-powered blockchain command center.\n\n\
        ═══════════════════════════════════════════════════\n\
        🚀 QUICK START GUIDE\n\
        ═══════════════════════════════════════════════════\n\n\
        💬 BASIC USAGE:\n\
        • Type your question in the input field\n\
        • Press Enter to send\n\
        • Agent will process and respond with helpful info\n\n\
        ⌨️  ESSENTIAL SHORTCUTS:\n\
        • Tab           → Navigate between panels\n\
        • ↑/↓ Arrows    → Browse input history (like bash!)\n\
        • F1 or ?       → Show complete help\n\
        • Alt+R/C/D     → Retry/Copy/Delete last message\n\
        • F12           → Take screenshot\n\n\
        🎯 POWER FEATURES:\n\
        • Alt+F         → Fork conversation at any point\n\
        • Ctrl+1-5      → Quick-insert AI suggestions\n\
        • F10           → Context menu with actions\n\
        • Alt+X         → Emergency clear if stuck\n\n\
        🔧 MCP TOOLS:\n\
        • Click items in \"MCP Tools\" panel to see details\n\
        • Test tools directly from the UI\n\
        • Agent can automatically use tools to help you\n\n\
        💡 PRO TIPS:\n\
        • All actions have confirmation dialogs - safe to explore!\n\
        • Message timestamps help track conversation flow\n\
        • Export chats anytime for safekeeping\n\
        • Multiple chat sessions run independently\n\n\
        ═══════════════════════════════════════════════════\n\
        📖 Need more help? Press F1 anytime!\n\
        ═══════════════════════════════════════════════════";

    let mut layout = LinearLayout::vertical();
    layout.add_child(
        cursive::views::ScrollView::new(TextView::new(welcome_text))
            .scroll_strategy(cursive::view::scroll::ScrollStrategy::StickToTop)
            .max_height(25),
    );

    siv.add_layer(
        Dialog::around(layout)
            .title("🎉 Welcome to Advanced Chat!")
            .button("Let's Go!", |s| {
                s.pop_layer();
                show_tips_hint(s);
            })
            .button("Show Full Help (F1)", |s| {
                s.pop_layer();
                super::handlers::show_advanced_help(s);
            })
            .max_width(70),
    );
}

/// Show a quick tips hint after welcome
fn show_tips_hint(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::info(
            "💡 Quick Tip:\n\n\
            The footer bar always shows the most important shortcuts.\n\
            Press F1 anytime for complete help!\n\n\
            Try pressing ↑ (up arrow) to test the history feature!",
        )
        .title("💡 Pro Tip")
        .button("Got it!", |s| {
            s.pop_layer();
        }),
    );
}

/// Show tips of the day
pub fn show_random_tip(siv: &mut Cursive) {
    let tips = vec![
        "💡 Press ↑/↓ arrows to navigate your input history, just like bash!",
        "💡 Use Alt+F to fork a conversation and explore alternative paths.",
        "💡 All destructive actions ask for confirmation - it's safe to experiment!",
        "💡 Press F12 to take a screenshot of your chat session.",
        "💡 Ctrl+1-5 inserts AI suggestions directly at your cursor position.",
        "💡 The input panel shows (history X/Y) when you're browsing old messages.",
        "💡 Export your chats regularly to keep a backup of important conversations.",
        "💡 Each chat session runs independently with its own agent state.",
        "💡 Press ? for quick shortcuts or F1 for comprehensive help.",
        "💡 Right-click or F10 opens a context menu with common actions.",
        "💡 Message timestamps help you track when each interaction occurred.",
        "💡 The status bar shows real-time system info updated every 2 seconds.",
        "💡 Alt+X is your emergency button if the agent gets stuck processing.",
        "💡 Multiple sessions let you multitask on different blockchain tasks.",
        "💡 Recording sessions saves your entire conversation to a file.",
    ];

    let random_index = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs() as usize
        % tips.len();

    let tip = tips[random_index];

    siv.add_layer(
        Dialog::info(tip)
            .title("💡 Tip of the Day")
            .button("Thanks!", |s| {
                s.pop_layer();
            })
            .button("Show All Tips", |s| {
                s.pop_layer();
                show_all_tips(s);
            }),
    );
}

/// Show all tips in a scrollable dialog
fn show_all_tips(siv: &mut Cursive) {
    let tips_text = "💡 ALL PRO TIPS\n\n\
        ═══════════════════════════════════════════════════\n\
        NAVIGATION & INPUT:\n\
        ═══════════════════════════════════════════════════\n\
        • Press ↑/↓ arrows to navigate your input history, just like bash!\n\
        • The input panel shows (history X/Y) when browsing old messages\n\
        • Tab switches between chat list and input field\n\
        • Shift+Tab navigates in reverse\n\n\
        ═══════════════════════════════════════════════════\n\
        SHORTCUTS & EFFICIENCY:\n\
        ═══════════════════════════════════════════════════\n\
        • Ctrl+1-5 inserts AI suggestions at cursor position\n\
        • Alt+R retries your last message\n\
        • Alt+C copies the last message to clipboard\n\
        • Alt+D deletes the last message (with confirmation)\n\
        • Alt+F forks conversation to explore alternatives\n\
        • F10 opens context menu with common actions\n\
        • F12 takes a screenshot of your chat window\n\n\
        ═══════════════════════════════════════════════════\n\
        SAFETY & RELIABILITY:\n\
        ═══════════════════════════════════════════════════\n\
        • All destructive actions ask for confirmation\n\
        • Alt+X is emergency clear if agent gets stuck\n\
        • Export chats regularly for backup\n\
        • Message timestamps track conversation timing\n\n\
        ═══════════════════════════════════════════════════\n\
        ADVANCED FEATURES:\n\
        ═══════════════════════════════════════════════════\n\
        • Multiple sessions run independently\n\
        • Each session has its own agent state\n\
        • Recording saves entire conversations to file\n\
        • MCP tools can be tested directly from UI\n\
        • Status bar shows real-time system info\n\
        • Themes can be changed with /theme commands\n\n\
        ═══════════════════════════════════════════════════\n\
        HELP & DISCOVERY:\n\
        ═══════════════════════════════════════════════════\n\
        • Press ? for quick shortcuts\n\
        • Press F1 for comprehensive help\n\
        • Footer bar shows most important shortcuts\n\
        • Button labels include shortcut hints\n\
        • Error messages include recovery suggestions";

    let layout = LinearLayout::vertical().child(
        cursive::views::ScrollView::new(TextView::new(tips_text))
            .scroll_strategy(cursive::view::scroll::ScrollStrategy::StickToTop)
            .max_height(25),
    );

    siv.add_layer(
        Dialog::around(layout)
            .title("💡 All Pro Tips")
            .button("Close", |s| {
                s.pop_layer();
            })
            .max_width(70),
    );
}

/// Show contextual tip based on user action
pub fn show_contextual_tip(siv: &mut Cursive, context: TipContext) {
    let tip = match context {
        TipContext::FirstMessage => {
            "💡 Great! You just sent your first message.\n\
            The agent is now processing it. Watch for the animated spinner!\n\n\
            Tip: Press ↑ to recall this message later."
        }
        TipContext::FirstHistory => {
            "💡 You're using history navigation!\n\
            Press ↑ to go back further, ↓ to go forward.\n\
            The input panel shows your position in history."
        }
        TipContext::AgentStuck => {
            "💡 Agent taking a while?\n\
            Sometimes AI responses can be slow.\n\
            If it's really stuck, press Alt+X to emergency clear."
        }
        TipContext::FirstFork => {
            "💡 You forked the conversation!\n\
            This creates a copy of the chat at this point.\n\
            Great for exploring different paths."
        }
        TipContext::FirstScreenshot => {
            "💡 Screenshot saved!\n\
            Find it in ~/.osvm/screenshots/\n\
            Perfect for sharing conversations or bug reports."
        }
    };

    // Show as a non-blocking toast
    super::loading::show_toast(siv, tip, 4000);
}

/// Context for showing appropriate tips
pub enum TipContext {
    FirstMessage,
    FirstHistory,
    AgentStuck,
    FirstFork,
    FirstScreenshot,
}
