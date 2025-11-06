// Browser automation command handler
//
// This module provides CLI commands for browser automation using Playwright.
// It integrates with the MCP service architecture for tool execution.

use crate::services::browser_service::{BrowserConfig, BrowserService, BrowserTool, BrowserType};
use anyhow::{anyhow, Context, Result};
use clap::ArgMatches;
use serde_json::json;

/// Handle browser automation commands
pub async fn handle_browser_command(
    app_matches: &ArgMatches,
    matches: &ArgMatches,
) -> Result<(), Box<dyn std::error::Error>> {
    let debug_mode = app_matches.get_flag("debug");

    let Some((browser_sub_command, browser_sub_matches)) = matches.subcommand() else {
        eprintln!("No browser subcommand provided");
        eprintln!("Available commands: install, status, navigate, screenshot, click, type, snapshot, tools");
        std::process::exit(1);
    };

    match browser_sub_command {
        "install" => {
            println!("🌐 Installing Playwright browsers...");
            println!();
            println!("Note: In MCP environment, Playwright tools are already available.");
            println!("This command verifies the installation.");
            println!();

            let mut service = BrowserService::new();
            match service.init().await {
                Ok(_) => {
                    println!("✅ Playwright is available and ready to use!");
                    println!("   Browser automation tools are operational.");
                }
                Err(e) => {
                    eprintln!("❌ Playwright is not available: {}", e);
                    eprintln!();
                    eprintln!("💡 In this environment, Playwright should be pre-installed.");
                    eprintln!("   If you see this error, check the environment configuration.");
                    std::process::exit(1);
                }
            }
        }

        "status" => {
            let service = BrowserService::new();
            
            println!("🌐 Browser Automation Status");
            println!("============================");
            println!();
            
            if service.check_playwright_available() {
                println!("✅ Playwright: Available");
            } else {
                println!("❌ Playwright: Not available");
            }
            
            let config = service.get_config();
            println!("🔧 Configuration:");
            println!("   Browser Type: {:?}", config.browser_type);
            println!("   Headless Mode: {}", config.headless);
            println!("   Default Timeout: {}s", config.timeout_secs);
            println!("   Viewport: {}x{}", config.viewport_width, config.viewport_height);
            println!("   Screenshots: {}", if config.enable_screenshots { "Enabled" } else { "Disabled" });
            println!("   Security Sandbox: {}", if config.sandbox { "Enabled" } else { "Disabled" });
        }

        "tools" => {
            println!("🛠️  Available Browser Automation Tools");
            println!("======================================");
            println!();

            let tools = BrowserTool::get_all_tools();
            for tool in tools {
                println!("📦 {}", tool.name);
                println!("   Description: {}", tool.description);
                if debug_mode {
                    println!("   Schema: {}", serde_json::to_string_pretty(&tool.input_schema)?);
                }
                println!();
            }

            println!("💡 Use 'osvm browser <tool-name> ...' to execute a tool");
            println!("   Example: osvm browser navigate --url https://example.com");
        }

        "navigate" => {
            let url = browser_sub_matches
                .get_one::<String>("url")
                .ok_or_else(|| anyhow!("URL is required"))?;

            println!("🌐 Navigating to: {}", url);

            let mut service = BrowserService::new();
            service.init().await?;

            let result = service.navigate(url).await?;
            println!("✅ {}", result["message"]);
            
            if debug_mode {
                println!("Debug: {}", serde_json::to_string_pretty(&result)?);
            }
        }

        "screenshot" => {
            let filename = browser_sub_matches
                .get_one::<String>("filename")
                .map(|s| s.as_str());

            println!("📸 Taking screenshot...");

            let mut service = BrowserService::new();
            service.init().await?;

            let result = service.screenshot(filename).await?;
            println!("✅ {}", result["message"]);
            
            if let Some(path) = result["path"].as_str() {
                println!("   Path: {}", path);
            }

            if debug_mode {
                println!("Debug: {}", serde_json::to_string_pretty(&result)?);
            }
        }

        "click" => {
            let selector = browser_sub_matches
                .get_one::<String>("selector")
                .ok_or_else(|| anyhow!("Selector is required"))?;

            println!("🖱️  Clicking element: {}", selector);

            let mut service = BrowserService::new();
            service.init().await?;

            let result = service.click(selector).await?;
            println!("✅ {}", result["message"]);

            if debug_mode {
                println!("Debug: {}", serde_json::to_string_pretty(&result)?);
            }
        }

        "type" => {
            let selector = browser_sub_matches
                .get_one::<String>("selector")
                .ok_or_else(|| anyhow!("Selector is required"))?;
            let text = browser_sub_matches
                .get_one::<String>("text")
                .ok_or_else(|| anyhow!("Text is required"))?;

            println!("⌨️  Typing into element: {}", selector);

            let mut service = BrowserService::new();
            service.init().await?;

            let result = service.type_text(selector, text).await?;
            println!("✅ {}", result["message"]);

            if debug_mode {
                println!("Debug: {}", serde_json::to_string_pretty(&result)?);
            }
        }

        "snapshot" => {
            println!("📋 Capturing page snapshot...");

            let mut service = BrowserService::new();
            service.init().await?;

            let result = service.snapshot().await?;
            println!("✅ {}", result["message"]);

            if debug_mode {
                println!("Debug: {}", serde_json::to_string_pretty(&result)?);
            }
        }

        "evaluate" => {
            let script = browser_sub_matches
                .get_one::<String>("script")
                .ok_or_else(|| anyhow!("Script is required"))?;

            println!("🔧 Evaluating JavaScript...");

            let mut service = BrowserService::new();
            service.init().await?;

            let result = service.evaluate(script).await?;
            println!("✅ {}", result["message"]);

            if debug_mode {
                println!("Debug: {}", serde_json::to_string_pretty(&result)?);
            }
        }

        "wait-for" => {
            let selector = browser_sub_matches
                .get_one::<String>("selector")
                .ok_or_else(|| anyhow!("Selector is required"))?;
            let timeout_ms = browser_sub_matches
                .get_one::<String>("timeout")
                .and_then(|s| s.parse::<u64>().ok());

            println!("⏳ Waiting for element: {}", selector);

            let mut service = BrowserService::new();
            service.init().await?;

            let result = service.wait_for_selector(selector, timeout_ms).await?;
            println!("✅ {}", result["message"]);

            if debug_mode {
                println!("Debug: {}", serde_json::to_string_pretty(&result)?);
            }
        }

        cmd => {
            eprintln!("❌ Unknown browser subcommand: {}", cmd);
            eprintln!();
            eprintln!("Available commands:");
            eprintln!("  install     - Install/verify Playwright installation");
            eprintln!("  status      - Show browser automation status");
            eprintln!("  tools       - List available browser tools");
            eprintln!("  navigate    - Navigate to a URL");
            eprintln!("  screenshot  - Take a screenshot");
            eprintln!("  click       - Click an element");
            eprintln!("  type        - Type text into an element");
            eprintln!("  snapshot    - Capture page snapshot");
            eprintln!("  evaluate    - Evaluate JavaScript");
            eprintln!("  wait-for    - Wait for an element");
            std::process::exit(1);
        }
    }

    Ok(())
}
