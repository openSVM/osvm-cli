use crate::services::ai_service::AiService;
use crate::utils::markdown_renderer::MarkdownRenderer;

/// Handle AI query with OVSM planning enabled (for p/plan/a/agent commands)
pub async fn handle_ai_query_with_planning(
    sub_command: &str,
    sub_matches: &clap::ArgMatches,
    app_matches: &clap::ArgMatches,
) -> Result<(), Box<dyn std::error::Error>> {
    // Extract the actual query from the planning subcommand args
    let query_parts = if let Some(external_args) = sub_matches.get_many::<std::ffi::OsString>("") {
        external_args
            .map(|os_str| os_str.to_string_lossy().to_string())
            .collect::<Vec<_>>()
    } else {
        // Fallback: parse from environment args
        std::env::args().skip(2).collect()
    };

    if query_parts.is_empty() {
        eprintln!("❌ No query provided for planning mode");
        eprintln!();
        eprintln!("💡 Usage:");
        eprintln!("   osvm {} <your query>", sub_command);
        eprintln!();
        eprintln!("📚 Examples:");
        eprintln!("   osvm p find top validators");
        eprintln!("   osvm agent show me recent transactions");
        return Ok(());
    }

    let query = sanitize_user_input(&query_parts.join(" "))?;
    let debug_mode = app_matches.get_flag("debug");

    if debug_mode {
        println!("📋 Planning mode enabled - using OVSM agent");
        println!("🔍 Query: \"{}\"", query);
    }

    // Use streaming agent with planning enabled for OVSM integration
    let verbose = app_matches.get_count("verbose");
    let plan_only = app_matches.get_flag("plan_only");

    // Route to streaming agent which includes OVSM planning capabilities
    crate::utils::streaming_agent::execute_streaming_agent(&query, verbose, plan_only)
        .await
        .map_err(|e| format!("OVSM planning agent failed: {}", e).into())
}

/// Handle regular AI query (without OVSM planning)
pub async fn handle_ai_query(
    sub_command: &str,
    sub_matches: &clap::ArgMatches,
    app_matches: &clap::ArgMatches,
) -> Result<(), Box<dyn std::error::Error>> {
    // For external subcommands, clap collects additional arguments in subcommand_value
    // This is the proper way to handle external subcommands with clap
    let mut query_parts = vec![sub_command.to_string()];

    // Get additional arguments from clap's external subcommand handling
    // External subcommands store arguments as OsString, not String
    if let Some(external_args) = sub_matches.get_many::<std::ffi::OsString>("") {
        query_parts.extend(external_args.map(|os_str| os_str.to_string_lossy().to_string()));
    }

    // If clap doesn't provide args (fallback), parse from environment
    // This maintains compatibility while documenting the limitation
    if query_parts.len() == 1 {
        let args: Vec<String> = std::env::args().collect();

        // Collect non-flag arguments starting from the subcommand
        let mut found_subcommand = false;
        for arg in args.iter().skip(1) {
            if found_subcommand {
                if !arg.starts_with('-') {
                    query_parts.push(arg.clone());
                }
            } else if arg == sub_command {
                found_subcommand = true;
            }
        }
    }

    let query = sanitize_user_input(&query_parts.join(" "))?;

    // Get debug flag from global args
    let debug_mode = app_matches.get_flag("debug");
    let plan_only = app_matches.get_flag("plan_only");

    // Check if this looks like a natural language query vs a typo/unknown command
    if !looks_like_natural_language_query(&query) {
        eprintln!("❌ Unknown command: '{}'", sub_command);
        eprintln!();
        eprintln!("💡 Did you mean one of these commands?");
        eprintln!("   osvm balance <ADDRESS>     - Check SOL balance");
        eprintln!("   osvm svm list              - List SVMs");
        eprintln!("   osvm mcp list              - List MCP servers");
        eprintln!("   osvm nodes list            - List nodes");
        eprintln!("   osvm chat                  - Start interactive chat");
        eprintln!();
        eprintln!("💬 For AI-powered queries, use quotes:");
        eprintln!("   osvm \"show me transactions for wallet ABC...\"");
        eprintln!("   osvm \"what is the balance of...\"");
        eprintln!();
        eprintln!("📚 Use 'osvm --help' to see all available commands");
        return Ok(());
    }

    if plan_only {
        let verbose = app_matches.get_count("verbose");
        return crate::utils::streaming_agent::execute_streaming_agent(&query, verbose, true)
            .await
            .map_err(|e| format!("OVSM planning agent failed: {}", e).into());
    }

    // Make AI request
    if debug_mode {
        println!("🔍 Interpreting as AI query: \"{}\"", query);
    }

    let ai_service = AiService::new_with_debug(debug_mode);
    match ai_service.query_with_debug(&query, debug_mode).await {
        Ok(response) => {
            if debug_mode {
                println!("🤖 AI Response:");
            }
            // Render the response as markdown for better formatting
            let renderer = MarkdownRenderer::new();
            renderer.render(&response);
        }
        Err(e) => {
            eprintln!("❌ AI query failed: {}", e);
            eprintln!("💡 Use 'osvm --help' to see available commands");
        }
    }

    Ok(())
}

/// Check if input looks like a natural language query rather than a CLI command
fn looks_like_natural_language_query(input: &str) -> bool {
    // Natural language indicators:
    // 1. Contains multiple words (at least 3)
    // 2. Contains question words
    // 3. Contains common natural language patterns
    // 4. Has spaces between non-address-like tokens

    let words: Vec<&str> = input.split_whitespace().collect();

    // Single word commands are likely typos (e.g., "balanc")
    if words.len() == 1 {
        return false;
    }

    // Two words might be a command with argument (e.g., "balance ABC123")
    // Only treat as NL if it has NL indicators
    if words.len() == 2 {
        let first_word = words[0].to_lowercase();
        // Question words or verbs indicate natural language
        let nl_indicators = [
            "show",
            "what",
            "how",
            "why",
            "where",
            "when",
            "who",
            "list",
            "find",
            "get",
            "check",
            "analyze",
            "explain",
            "tell",
            "give",
            "display",
            "calculate",
        ];
        return nl_indicators.contains(&first_word.as_str());
    }

    // 3+ words are likely natural language
    // But check for address-heavy queries (might be copy-paste errors)
    let long_hex_count = words
        .iter()
        .filter(|w| w.len() > 30 && w.chars().all(|c| c.is_alphanumeric()))
        .count();

    // If mostly long hex strings, might be malformed command
    if long_hex_count > words.len() / 2 {
        return false;
    }

    true
}

/// Sanitize user input to prevent command injection and ensure safe processing
fn sanitize_user_input(input: &str) -> Result<String, Box<dyn std::error::Error>> {
    // Remove potentially dangerous characters and sequences
    let sanitized = input
        .chars()
        .filter(|c| {
            // Allow alphanumeric, spaces, basic punctuation, but block command injection chars
            c.is_alphanumeric()
                || matches!(
                    *c,
                    ' ' | '.'
                        | ','
                        | '?'
                        | '!'
                        | ':'
                        | ';'
                        | '\''
                        | '"'
                        | '-'
                        | '_'
                        | '('
                        | ')'
                        | '['
                        | ']'
                        | '{'
                        | '}'
                )
        })
        .collect::<String>();

    // Remove potentially dangerous sequences
    let sanitized = sanitized
        .replace("&&", " and ")
        .replace("||", " or ")
        .replace("$(", " ")
        .replace("`", " ")
        .replace("${", " ");

    // Limit length to prevent resource exhaustion
    if sanitized.len() > 2048 {
        return Err("Input too long (max 2048 characters)".into());
    }

    // Ensure non-empty after sanitization
    if sanitized.trim().is_empty() {
        return Err("Input cannot be empty after sanitization".into());
    }

    Ok(sanitized.trim().to_string())
}
