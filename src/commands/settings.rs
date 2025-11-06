use crate::ai_config::AiConfig;
use anyhow::{Context, Result};
use colored::Colorize;

/// Handle settings command
pub async fn handle_settings_command(matches: &clap::ArgMatches) -> Result<()> {
    match matches.subcommand() {
        Some(("ai", ai_matches)) => handle_ai_settings(ai_matches).await,
        Some(("show", _)) | None => show_all_settings().await,
        _ => {
            eprintln!("{}", "❌ Unknown settings command".red());
            eprintln!("Try: osvm settings ai --help");
            std::process::exit(1);
        }
    }
}

/// Handle AI settings subcommand
async fn handle_ai_settings(matches: &clap::ArgMatches) -> Result<()> {
    match matches.subcommand() {
        Some(("preset", preset_matches)) => {
            let preset_name = preset_matches
                .get_one::<String>("name")
                .context("Preset name required")?;

            set_preset(preset_name).await
        }
        Some(("set-url", url_matches)) => {
            let url = url_matches
                .get_one::<String>("url")
                .context("URL required")?;

            set_ai_url(url).await
        }
        Some(("set-key", key_matches)) => {
            let key = key_matches
                .get_one::<String>("key")
                .context("API key required")?;

            set_ai_key(key).await
        }
        Some(("set-model", model_matches)) => {
            let model = model_matches
                .get_one::<String>("model")
                .context("Model name required")?;

            set_ai_model(model).await
        }
        Some(("show", _)) | None => show_ai_settings().await,
        _ => {
            eprintln!("{}", "❌ Unknown AI settings command".red());
            eprintln!("Try: osvm settings ai --help");
            std::process::exit(1);
        }
    }
}

/// Show all settings
async fn show_all_settings() -> Result<()> {
    println!("{}", "⚙️  OSVM Settings".cyan().bold());
    println!();
    show_ai_settings().await
}

/// Show AI settings
async fn show_ai_settings() -> Result<()> {
    let config = AiConfig::load()?;
    let config_path = AiConfig::default_config_path();

    println!("{}", "🤖 AI Configuration".cyan().bold());
    println!(
        "   {}: {}",
        "Config file".bright_black(),
        config_path.display()
    );
    println!();
    println!("   {}: {}", "Provider".yellow(), config.provider);
    println!("   {}: {}", "API URL".yellow(), config.api_url);

    if let Some(key) = &config.api_key {
        let masked_key = format!(
            "{}...{}",
            &key[..8.min(key.len())],
            &key[key.len().saturating_sub(4)..]
        );
        println!("   {}: {}", "API Key".yellow(), masked_key);
    } else {
        println!("   {}: {}", "API Key".yellow(), "Not set".bright_black());
    }

    println!("   {}: {}", "Model".yellow(), config.model);
    println!("   {}: {}", "Temperature".yellow(), config.temperature);
    println!("   {}: {}", "Max Tokens".yellow(), config.max_tokens);
    println!("   {}: {}s", "Timeout".yellow(), config.timeout_secs);
    println!();

    // Validation status
    match config.validate() {
        Ok(_) => println!("{}", "✅ Configuration is valid".green()),
        Err(e) => {
            println!("{}", format!("⚠️  Configuration warning: {}", e).yellow());
            println!(
                "{}",
                "   Some features may not work correctly.".bright_black()
            );
        }
    }

    println!();
    println!("{}", "Available Commands:".bright_black());
    println!(
        "   {} - Switch to preset configuration",
        "osvm settings ai preset <name>".bright_white()
    );
    println!(
        "   {} - Set API URL",
        "osvm settings ai set-url <url>".bright_white()
    );
    println!(
        "   {} - Set API key",
        "osvm settings ai set-key <key>".bright_white()
    );
    println!(
        "   {} - Set model name",
        "osvm settings ai set-model <model>".bright_white()
    );
    println!();
    println!(
        "{}",
        "Available Presets: openai, ollama, local, anthropic".bright_black()
    );

    Ok(())
}

/// Set AI configuration to a preset
async fn set_preset(preset_name: &str) -> Result<()> {
    let mut config = AiConfig::preset(preset_name)?;

    // If it's OpenAI or Anthropic, remind user to set the key
    if (config.is_openai_compatible() || preset_name == "anthropic") && config.api_key.is_none() {
        println!(
            "{}",
            format!("⚠️  {} requires an API key", preset_name.to_uppercase()).yellow()
        );
        println!(
            "{}",
            "   Set it with: osvm settings ai set-key <your-api-key>".bright_black()
        );
    }

    config.save()?;

    println!(
        "{}",
        format!("✅ AI configuration set to '{}' preset", preset_name).green()
    );
    println!();
    show_ai_settings().await
}

/// Set AI API URL
async fn set_ai_url(url: &str) -> Result<()> {
    let mut config = AiConfig::load()?;
    config.api_url = url.to_string();

    // Auto-detect provider from URL
    if url.contains("openai.com") {
        config.provider = "openai".to_string();
    } else if url.contains("anthropic.com") {
        config.provider = "anthropic".to_string();
    } else if url.contains("11434") || url.contains("ollama") {
        config.provider = "ollama".to_string();
    }

    config.save()?;

    println!("{}", "✅ AI API URL updated".green());
    println!();
    show_ai_settings().await
}

/// Set AI API key
async fn set_ai_key(key: &str) -> Result<()> {
    let mut config = AiConfig::load()?;
    config.api_key = Some(key.to_string());
    config.save()?;

    println!("{}", "✅ AI API key set successfully".green());
    println!(
        "{}",
        "   The key is stored securely in ~/.config/osvm/ai_config.yaml".bright_black()
    );

    Ok(())
}

/// Set AI model name
async fn set_ai_model(model: &str) -> Result<()> {
    let mut config = AiConfig::load()?;
    config.model = model.to_string();
    config.save()?;

    println!("{}", "✅ AI model updated".green());
    println!();
    show_ai_settings().await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_show_ai_settings() {
        // This test just ensures the function doesn't panic
        let result = show_ai_settings().await;
        // It's ok if it errors (no config file), we just don't want panic
        let _ = result;
    }
}
