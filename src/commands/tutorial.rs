//! Interactive tutorial system for OSVM CLI
//!
//! Guides new users through first-time setup and basic operations.

use anyhow::Result;
use colored::*;
use std::io::{self, Write};
use std::process::Command;

/// Tutorial step structure
#[derive(Debug, Clone)]
pub struct TutorialStep {
    pub number: usize,
    pub title: String,
    pub description: String,
    pub command: Option<String>,
    pub explanation: String,
    pub completion_check: Option<fn() -> bool>,
}

/// Tutorial progress tracker
pub struct TutorialProgress {
    current_step: usize,
    completed_steps: Vec<usize>,
    total_steps: usize,
}

impl TutorialProgress {
    pub fn new(total_steps: usize) -> Self {
        Self {
            current_step: 1,
            completed_steps: Vec::new(),
            total_steps,
        }
    }

    pub fn complete_step(&mut self, step: usize) {
        if !self.completed_steps.contains(&step) {
            self.completed_steps.push(step);
        }
        self.current_step = step + 1;
    }

    pub fn is_complete(&self) -> bool {
        self.completed_steps.len() == self.total_steps
    }

    pub fn progress_percentage(&self) -> f32 {
        (self.completed_steps.len() as f32 / self.total_steps as f32) * 100.0
    }
}

/// Main tutorial entry point
pub async fn run_interactive_tutorial() -> Result<()> {
    clear_screen();
    show_welcome_banner();

    let steps = create_tutorial_steps();
    let mut progress = TutorialProgress::new(steps.len());

    println!(
        "\n{}This tutorial will guide you through:",
        "📚 ".bright_cyan().bold()
    );
    for step in &steps {
        println!("  {}. {}", step.number, step.title.bright_white());
    }

    println!("\n{}", "Press Enter to begin...".bright_yellow());
    wait_for_enter();

    // Execute each tutorial step
    for step in steps {
        clear_screen();

        if let Err(e) = execute_tutorial_step(&step, &mut progress).await {
            eprintln!("{} {}", "❌ Error:".bright_red(), e);
            println!(
                "\n{}",
                "Tutorial paused. Press Enter to continue or Ctrl+C to exit...".yellow()
            );
            wait_for_enter();
        }
    }

    // Show completion
    if progress.is_complete() {
        show_completion_certificate();
    }

    Ok(())
}

/// Execute a single tutorial step
async fn execute_tutorial_step(step: &TutorialStep, progress: &mut TutorialProgress) -> Result<()> {
    // Show step header
    show_step_header(step, progress);

    // Show description
    println!("\n{}", step.description.bright_white());
    println!("\n{}", step.explanation.white());

    // If there's a command to demonstrate
    if let Some(ref cmd) = step.command {
        println!("\n{}Try this command:", "💡 ".bright_yellow());
        println!("\n  {}\n", cmd.bright_cyan().bold());

        println!(
            "{}",
            "Press Enter to execute, or 's' to skip...".bright_black()
        );
        let choice = get_user_choice();

        if choice != "s" {
            println!(
                "\n{}Executing: {}\n",
                "⚡ ".bright_green(),
                cmd.bright_cyan()
            );

            // Execute the command
            let output = execute_command_safe(cmd)?;
            println!("{}", output);

            println!("\n{}Command completed!", "✅ ".bright_green());
        }
    }

    // Check completion condition
    let completed = if let Some(check_fn) = step.completion_check {
        check_fn()
    } else {
        true
    };

    if completed {
        progress.complete_step(step.number);
        println!(
            "\n{}Step {} complete! ({:.0}% total progress)",
            "🎉 ".bright_green(),
            step.number,
            progress.progress_percentage()
        );
    }

    println!("\n{}", "Press Enter to continue...".bright_yellow());
    wait_for_enter();

    Ok(())
}

/// Create all tutorial steps
fn create_tutorial_steps() -> Vec<TutorialStep> {
    vec![
        TutorialStep {
            number: 1,
            title: "Welcome & Installation Check".to_string(),
            description: "Let's verify that OSVM is properly installed.".to_string(),
            command: Some("osvm --version".to_string()),
            explanation: "The --version flag shows the current version of OSVM CLI and confirms \
                         it's correctly installed in your PATH."
                .to_string(),
            completion_check: Some(check_osvm_installed),
        },
        TutorialStep {
            number: 2,
            title: "System Health Check".to_string(),
            description: "OSVM includes a built-in doctor command to check your system."
                .to_string(),
            command: Some("osvm doctor".to_string()),
            explanation: "The doctor command checks:\n\
                         • Solana CLI installation\n\
                         • Keypair configuration\n\
                         • RPC connectivity\n\
                         • Firecracker support (optional)\n\n\
                         It will suggest fixes for any issues found."
                .to_string(),
            completion_check: None,
        },
        TutorialStep {
            number: 3,
            title: "OVSM LISP Introduction".to_string(),
            description: "OSVM includes OVSM, a LISP-dialect for blockchain scripting.".to_string(),
            command: Some("osvm ovsm eval '(+ 1 2 3)'".to_string()),
            explanation: "OVSM uses S-expression syntax:\n\
                         • (+ 1 2 3) adds numbers: 1 + 2 + 3 = 6\n\
                         • All operations use parentheses\n\
                         • No indentation sensitivity\n\
                         • Zero parser bugs!"
                .to_string(),
            completion_check: None,
        },
        TutorialStep {
            number: 4,
            title: "OVSM Examples".to_string(),
            description: "Let's see what OVSM can do.".to_string(),
            command: Some("osvm ovsm examples".to_string()),
            explanation: "OVSM can:\n\
                         • Query Solana RPC endpoints\n\
                         • Process blockchain data\n\
                         • Automate validator operations\n\
                         • Analyze transaction patterns\n\n\
                         All with a simple, functional syntax."
                .to_string(),
            completion_check: None,
        },
        TutorialStep {
            number: 5,
            title: "Interactive REPL".to_string(),
            description: "OVSM includes an interactive Read-Eval-Print Loop.".to_string(),
            command: None,
            explanation: "You can run 'osvm ovsm repl' anytime to experiment with OVSM code:\n\n\
                         >>> (define x 42)\n\
                         >>> (+ x 8)\n\
                         50\n\n\
                         Try it later when you want to test OVSM expressions!\n\
                         (Skipping in tutorial to continue)"
                .to_string(),
            completion_check: None,
        },
        TutorialStep {
            number: 6,
            title: "AI Chat Interface".to_string(),
            description: "OSVM includes an AI-powered chat assistant.".to_string(),
            command: None,
            explanation: "Run 'osvm chat' to start the AI assistant:\n\n\
                         • Ask questions about Solana\n\
                         • Get help with OSVM commands\n\
                         • Generate OVSM scripts\n\
                         • Debug issues\n\n\
                         The chat works out-of-the-box with no configuration!\n\
                         (Skipping in tutorial - try it later!)"
                .to_string(),
            completion_check: None,
        },
        TutorialStep {
            number: 7,
            title: "Next Steps".to_string(),
            description: "Congratulations! You've completed the OSVM tutorial.".to_string(),
            command: None,
            explanation: "Here's what you can explore next:\n\n\
                         1. Read the docs: https://docs.osvm.dev\n\
                         2. Try OVSM scripts: osvm ovsm examples\n\
                         3. Chat with AI: osvm chat\n\
                         4. Check examples/: Example OVSM scripts\n\
                         5. Join community: https://github.com/openSVM/osvm-cli\n\n\
                         Run 'osvm --help' anytime to see all available commands."
                .to_string(),
            completion_check: None,
        },
    ]
}

/// Show welcome banner
fn show_welcome_banner() {
    println!(
        "{}",
        "╔═══════════════════════════════════════════════════════════════╗".bright_cyan()
    );
    println!(
        "{}",
        "║                                                               ║".bright_cyan()
    );
    println!(
        "{}",
        "║           🚀  WELCOME TO OSVM INTERACTIVE TUTORIAL  🚀        ║"
            .bright_cyan()
            .bold()
    );
    println!(
        "{}",
        "║                                                               ║".bright_cyan()
    );
    println!(
        "{}",
        "║  This guided tutorial will teach you the basics of OSVM CLI  ║".bright_white()
    );
    println!(
        "{}",
        "║  in just 5-10 minutes.                                       ║".bright_white()
    );
    println!(
        "{}",
        "║                                                               ║".bright_cyan()
    );
    println!(
        "{}",
        "╚═══════════════════════════════════════════════════════════════╝".bright_cyan()
    );
}

/// Show step header with progress
fn show_step_header(step: &TutorialStep, progress: &TutorialProgress) {
    let progress_bar = create_progress_bar(progress.progress_percentage());

    println!("{}", "═".repeat(65).bright_cyan());
    println!(
        "\n{} {}/{}  {}",
        "📍 STEP".bright_cyan().bold(),
        step.number,
        progress.total_steps,
        step.title.bright_white().bold()
    );
    println!("\n{} {}", "Progress:".bright_black(), progress_bar);
    println!("{}", "═".repeat(65).bright_cyan());
}

/// Create ASCII progress bar
fn create_progress_bar(percentage: f32) -> String {
    let total_bars = 30;
    let filled_bars = (total_bars as f32 * (percentage / 100.0)) as usize;
    let empty_bars = total_bars - filled_bars;

    format!(
        "[{}{}] {:.0}%",
        "█".repeat(filled_bars).bright_green(),
        "░".repeat(empty_bars).bright_black(),
        percentage
    )
}

/// Show completion certificate
fn show_completion_certificate() {
    clear_screen();

    println!(
        "\n{}",
        "╔═══════════════════════════════════════════════════════════════╗".bright_green()
    );
    println!(
        "{}",
        "║                                                               ║".bright_green()
    );
    println!(
        "{}",
        "║              🎉  TUTORIAL COMPLETE!  🎉                       ║"
            .bright_green()
            .bold()
    );
    println!(
        "{}",
        "║                                                               ║".bright_green()
    );
    println!(
        "{}",
        "║  You've successfully completed the OSVM CLI tutorial!         ║".bright_white()
    );
    println!(
        "{}",
        "║                                                               ║".bright_green()
    );
    println!(
        "{}",
        "║  You now know how to:                                        ║".bright_white()
    );
    println!(
        "{}",
        "║    ✅ Check OSVM installation                                 ║".bright_white()
    );
    println!(
        "{}",
        "║    ✅ Run system diagnostics                                  ║".bright_white()
    );
    println!(
        "{}",
        "║    ✅ Use OVSM LISP language                                  ║".bright_white()
    );
    println!(
        "{}",
        "║    ✅ Access examples and REPL                                ║".bright_white()
    );
    println!(
        "{}",
        "║    ✅ Use the AI chat assistant                               ║".bright_white()
    );
    println!(
        "{}",
        "║                                                               ║".bright_green()
    );
    println!(
        "{}",
        "║  Next steps:                                                 ║"
            .bright_cyan()
            .bold()
    );
    println!(
        "{}",
        "║    • Try 'osvm chat' for AI assistance                       ║".bright_white()
    );
    println!(
        "{}",
        "║    • Explore 'examples/' directory                           ║".bright_white()
    );
    println!(
        "{}",
        "║    • Read docs at https://docs.osvm.dev                      ║".bright_white()
    );
    println!(
        "{}",
        "║    • Join us at https://github.com/openSVM/osvm-cli          ║".bright_white()
    );
    println!(
        "{}",
        "║                                                               ║".bright_green()
    );
    println!(
        "{}",
        "╚═══════════════════════════════════════════════════════════════╝".bright_green()
    );

    println!(
        "\n{}",
        "🌟 Happy building with OSVM! 🌟\n".bright_yellow().bold()
    );
}

/// Clear screen
fn clear_screen() {
    print!("\x1B[2J\x1B[1;1H");
    io::stdout().flush().unwrap_or(());
}

/// Wait for user to press Enter
fn wait_for_enter() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap_or(0);
}

/// Get user choice (Enter or 's' to skip)
fn get_user_choice() -> String {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap_or(0);
    input.trim().to_lowercase()
}

/// Execute command safely (prevent shell injection)
fn execute_command_safe(cmd: &str) -> Result<String> {
    // Parse command and args
    let parts: Vec<&str> = cmd.split_whitespace().collect();
    if parts.is_empty() {
        return Ok(String::new());
    }

    let output = Command::new(parts[0]).args(&parts[1..]).output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    Ok(format!("{}{}", stdout, stderr))
}

/// Check if OSVM is installed
fn check_osvm_installed() -> bool {
    Command::new("osvm").arg("--version").output().is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tutorial_progress() {
        let mut progress = TutorialProgress::new(5);
        assert_eq!(progress.progress_percentage(), 0.0);

        progress.complete_step(1);
        assert_eq!(progress.progress_percentage(), 20.0);

        progress.complete_step(2);
        progress.complete_step(3);
        assert!((progress.progress_percentage() - 60.0).abs() < 0.001);

        progress.complete_step(4);
        progress.complete_step(5);
        assert!(progress.is_complete());
        assert_eq!(progress.progress_percentage(), 100.0);
    }

    #[test]
    fn test_progress_bar_creation() {
        let bar_0 = create_progress_bar(0.0);
        assert!(bar_0.contains("0%"));

        let bar_50 = create_progress_bar(50.0);
        assert!(bar_50.contains("50%"));

        let bar_100 = create_progress_bar(100.0);
        assert!(bar_100.contains("100%"));
    }

    #[test]
    fn test_tutorial_steps_creation() {
        let steps = create_tutorial_steps();
        assert_eq!(steps.len(), 7);
        assert_eq!(steps[0].number, 1);
        assert_eq!(steps[6].number, 7);
    }
}
