use clap::{Arg, Command};

/// Build the settings command
pub fn build_settings_command() -> Command {
    Command::new("settings")
        .about("Configure OSVM settings (AI, themes, etc.)")
        .subcommand_required(false)
        .subcommand(
            Command::new("show")
                .about("Show all settings")
        )
        .subcommand(
            Command::new("ai")
                .about("Configure AI service settings")
                .subcommand_required(false)
                .subcommand(
                    Command::new("show")
                        .about("Show current AI configuration")
                )
                .subcommand(
                    Command::new("preset")
                        .about("Use a preset AI configuration")
                        .arg(
                            Arg::new("name")
                                .help("Preset name (openai, ollama, local, anthropic)")
                                .required(true)
                                .value_parser(["openai", "ollama", "local", "anthropic"])
                                .index(1),
                        )
                )
                .subcommand(
                    Command::new("set-url")
                        .about("Set AI API endpoint URL")
                        .arg(
                            Arg::new("url")
                                .help("API endpoint URL (e.g., https://api.openai.com/v1/chat/completions)")
                                .required(true)
                                .index(1),
                        )
                )
                .subcommand(
                    Command::new("set-key")
                        .about("Set AI API key")
                        .arg(
                            Arg::new("key")
                                .help("API key for the AI service")
                                .required(true)
                                .index(1),
                        )
                )
                .subcommand(
                    Command::new("set-model")
                        .about("Set AI model name")
                        .arg(
                            Arg::new("model")
                                .help("Model name (e.g., gpt-4o, claude-3-5-sonnet-20241022, qwen3-coder:30b)")
                                .required(true)
                                .index(1),
                        )
                )
        )
}
