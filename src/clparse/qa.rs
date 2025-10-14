use clap::{Arg, ArgAction, Command};

pub fn build_qa_command() -> Command {
    Command::new("qa")
        .about("Run QA automation tests and generate test reports")
        .arg_required_else_help(true)
        .subcommand(
            Command::new("run")
                .about("Run QA test scenarios")
                .arg(
                    Arg::new("scenario")
                        .long("scenario")
                        .short('s')
                        .value_name("NAME")
                        .help("Specific test scenario to run")
                )
                .arg(
                    Arg::new("all")
                        .long("all")
                        .action(ArgAction::SetTrue)
                        .help("Run all test scenarios")
                )
                .arg(
                    Arg::new("create_issues")
                        .long("create-issues")
                        .action(ArgAction::SetTrue)
                        .help("Create GitHub issues for bugs found")
                )
                .arg(
                    Arg::new("github_repo")
                        .long("github-repo")
                        .value_name("REPO")
                        .default_value("opensvm/osvm-cli")
                        .help("GitHub repository for issue creation (format: owner/repo)")
                )
        )
        .subcommand(
            Command::new("visual")
                .about("Run visual TUI tests with automated UI interaction")
                .long_about("Test the advanced chat TUI with automated keyboard events and screen capture.\n\
                           \n\
                           Uses PTY-based terminal emulation to run the TUI, inject keyboard events,\n\
                           capture screenshots, and validate UI state automatically.\n\
                           \n\
                           Examples:\n\
                           • osvm qa visual                       # Run all visual tests\n\
                           • osvm qa visual --test basic_nav      # Run specific test\n\
                           • osvm qa visual --screenshots         # Save screenshots\n\
                           • osvm qa visual --cols 160 --rows 40  # Custom terminal size")
                .arg(
                    Arg::new("test")
                        .long("test")
                        .short('t')
                        .value_name("TEST_NAME")
                        .help("Run specific test scenario (e.g., basic_navigation, help_dialog, message_input)")
                )
                .arg(
                    Arg::new("cols")
                        .long("cols")
                        .value_name("COLUMNS")
                        .default_value("120")
                        .help("Terminal width in columns")
                )
                .arg(
                    Arg::new("rows")
                        .long("rows")
                        .value_name("ROWS")
                        .default_value("30")
                        .help("Terminal height in rows")
                )
                .arg(
                    Arg::new("screenshots")
                        .long("screenshots")
                        .action(ArgAction::SetTrue)
                        .help("Save screenshots to ~/.osvm/qa/screenshots/")
                )
                .arg(
                    Arg::new("headless")
                        .long("headless")
                        .action(ArgAction::SetTrue)
                        .help("Run in headless mode (no display output)")
                )
                .arg(
                    Arg::new("png")
                        .long("png")
                        .action(ArgAction::SetTrue)
                        .help("Save screenshots as PNG images (requires --screenshots)")
                )
        )
        .subcommand(
            Command::new("interactive")
                .about("Run interactive QA testing with user prompts")
        )
        .subcommand(
            Command::new("list")
                .about("List available QA test scenarios")
        )
        .subcommand(
            Command::new("reports")
                .about("View QA test reports")
                .arg(
                    Arg::new("latest")
                        .long("latest")
                        .action(ArgAction::SetTrue)
                        .help("Show only the latest report")
                )
        )
}
