// Browser command parser for CLI
//
// Defines the browser automation subcommand and its options.

use clap::{Arg, ArgAction, Command};

pub fn browser_command() -> Command {
    Command::new("browser")
        .about("Browser automation using Playwright (headless browser control)")
        .long_about(
            "Browser automation commands for web scraping, testing, and interaction.\n\
            \n\
            Powered by Playwright with MCP integration for secure, isolated execution.\n\
            \n\
            Examples:\n\
              osvm browser install                                    Install/verify Playwright\n\
              osvm browser status                                     Show automation status\n\
              osvm browser tools                                      List available tools\n\
              osvm browser navigate --url https://example.com         Navigate to URL\n\
              osvm browser screenshot --filename page.png             Take screenshot\n\
              osvm browser click --selector \"#submit-button\"          Click element\n\
              osvm browser type --selector \"#search\" --text \"query\"  Type text\n\
              osvm browser snapshot                                   Get page snapshot\n\
              osvm browser evaluate --script \"document.title\"         Run JavaScript\n\
              osvm browser wait-for --selector \".loading\"             Wait for element\n\
            \n\
            Security:\n\
              • Runs in sandboxed environment\n\
              • Configurable timeouts\n\
              • Screenshot verification\n\
              • DOM access controls\n"
        )
        .subcommand_required(false)
        .arg_required_else_help(true)
        .subcommand(
            Command::new("install")
                .about("Install or verify Playwright installation")
                .long_about(
                    "Checks if Playwright is installed and available.\n\
                    In MCP environment, Playwright should be pre-installed."
                )
        )
        .subcommand(
            Command::new("status")
                .about("Show browser automation status and configuration")
        )
        .subcommand(
            Command::new("tools")
                .about("List all available browser automation tools")
                .long_about(
                    "Display all browser tools with their descriptions and input schemas.\n\
                    Use --debug for detailed schema information."
                )
        )
        .subcommand(
            Command::new("navigate")
                .about("Navigate to a URL")
                .arg(
                    Arg::new("url")
                        .long("url")
                        .short('u')
                        .value_name("URL")
                        .required(true)
                        .help("URL to navigate to")
                )
        )
        .subcommand(
            Command::new("screenshot")
                .about("Take a screenshot of the current page")
                .arg(
                    Arg::new("filename")
                        .long("filename")
                        .short('f')
                        .value_name("PATH")
                        .help("Optional filename for the screenshot (default: timestamped)")
                )
        )
        .subcommand(
            Command::new("click")
                .about("Click an element on the page")
                .arg(
                    Arg::new("selector")
                        .long("selector")
                        .short('s')
                        .value_name("SELECTOR")
                        .required(true)
                        .help("CSS selector for the element to click")
                )
        )
        .subcommand(
            Command::new("type")
                .about("Type text into an element")
                .arg(
                    Arg::new("selector")
                        .long("selector")
                        .short('s')
                        .value_name("SELECTOR")
                        .required(true)
                        .help("CSS selector for the element")
                )
                .arg(
                    Arg::new("text")
                        .long("text")
                        .short('t')
                        .value_name("TEXT")
                        .required(true)
                        .help("Text to type into the element")
                )
        )
        .subcommand(
            Command::new("snapshot")
                .about("Capture accessibility snapshot of the current page")
                .long_about(
                    "Captures the page structure in a machine-readable format.\n\
                    Useful for understanding page layout and element hierarchy."
                )
        )
        .subcommand(
            Command::new("evaluate")
                .about("Evaluate JavaScript on the page")
                .arg(
                    Arg::new("script")
                        .long("script")
                        .short('s')
                        .value_name("JAVASCRIPT")
                        .required(true)
                        .help("JavaScript code to evaluate")
                )
        )
        .subcommand(
            Command::new("wait-for")
                .about("Wait for an element to appear on the page")
                .arg(
                    Arg::new("selector")
                        .long("selector")
                        .short('s')
                        .value_name("SELECTOR")
                        .required(true)
                        .help("CSS selector to wait for")
                )
                .arg(
                    Arg::new("timeout")
                        .long("timeout")
                        .short('t')
                        .value_name("MILLISECONDS")
                        .help("Timeout in milliseconds (default: 30000)")
                )
        )
}
