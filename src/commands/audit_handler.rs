use crate::services::audit_service::{AuditRequest, AuditService};

/// Handle the audit command using the dedicated audit service
pub async fn handle_audit_command(
    app_matches: &clap::ArgMatches,
    matches: &clap::ArgMatches,
) -> Result<(), Box<dyn std::error::Error>> {
    let no_commit = matches.get_flag("no-commit");
    let default_output_dir = matches.get_one::<String>("output").unwrap().to_string();

    // If --no-commit is used and output directory is the default, use current directory
    let output_dir = if no_commit && default_output_dir == "audit_reports" {
        ".".to_string()
    } else {
        default_output_dir
    };

    let format = matches.get_one::<String>("format").unwrap().to_string();
    let verbose = matches.get_count("verbose");
    let test_mode = matches.get_flag("test");

    // AI analysis is enabled by default, disabled only if --noai is provided
    let ai_analysis = !matches.get_flag("noai");

    // Handle repository parsing - check positional argument first, then --gh flag
    let gh_repo = if let Some(repo) = matches.get_one::<String>("repository") {
        // Parse positional argument as GitHub repo
        if repo.contains('/') {
            // Looks like owner/repo format
            if repo.contains('#') {
                Some(repo.to_string())
            } else {
                // No branch specified, try main first, then default branch
                Some(format!("{}#main", repo))
            }
        } else {
            // Not a repo format, treat as regular path
            None
        }
    } else {
        matches.get_one::<String>("gh").map(|s| s.to_string())
    };

    let template_path = matches.get_one::<String>("template").map(|s| s.to_string());
    let api_url = matches.get_one::<String>("api-url").map(|s| s.to_string());

    let request = AuditRequest {
        output_dir,
        format,
        verbose,
        test_mode,
        ai_analysis,
        gh_repo,
        template_path,
        no_commit,
        api_url,
    };

    // Create the audit service with custom API URL if provided
    let service = if ai_analysis {
        if let Some(api_url) = &request.api_url {
            println!("🤖 Using custom AI API: {}", api_url);
            AuditService::with_custom_ai(api_url.clone())
        } else {
            // Use default (osvm.ai unless explicitly configured for OpenAI)
            println!("🤖 Using default OSVM AI service");
            AuditService::with_internal_ai()
        }
    } else {
        println!("🤖 AI analysis disabled");
        AuditService::new()
    };

    // Execute the audit
    let result = service.execute_audit(&request).await?;

    // Handle exit code for CI/CD systems
    if !result.success {
        println!("⚠️  Critical or high-severity findings detected. Please review and address them promptly.");
        println!("📋 This audit exits with code 1 to signal CI/CD systems about security issues.");
        std::process::exit(1);
    }

    Ok(())
}
