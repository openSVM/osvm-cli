use crate::services::ai_service::AiService;
use crate::utils::markdown_renderer::MarkdownRenderer;

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
