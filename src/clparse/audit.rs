use clap::{Arg, ArgAction, Command};

/// Build the audit command
pub fn build_audit_command() -> Command {
    Command::new("audit")
        .about("Generate comprehensive security audit report")
        .arg(
            Arg::new("repository")
                .help("Repository to audit (format: owner/repo or owner/repo#branch)")
                .value_name("REPOSITORY")
                .index(1),
        )
        .arg(
            Arg::new("output")
                .long("output")
                .short('o')
                .value_name("PATH")
                .help("Output directory for audit report files")
                .default_value("audit_reports"),
        )
        .arg(
            Arg::new("format")
                .long("format")
                .value_name("FORMAT")
                .value_parser(clap::builder::PossibleValuesParser::new([
                    "typst", "pdf", "both", "json", "html", "markdown",
                ]))
                .default_value("both")
                .help("Output format: typst source, PDF, both, JSON, HTML, or Markdown"),
        )
        .arg(
            Arg::new("verbose")
                .long("verbose")
                .short('v')
                .action(ArgAction::Count)
                .help("Verbose audit output"),
        )
        .arg(
            Arg::new("test")
                .long("test")
                .action(ArgAction::SetTrue)
                .help("Generate test audit report with sample data"),
        )
        .arg(
            Arg::new("noai")
                .long("noai")
                .action(ArgAction::SetTrue)
                .help("Disable AI-powered security analysis"),
        )
        .arg(
            Arg::new("api-url")
                .long("api-url")
                .value_name("URL")
                .help("Custom API URL for AI analysis (default: https://osvm.ai/api/getAnswer)"),
        )
        .arg(
            Arg::new("gh")
                .long("gh")
                .value_name("REPO#BRANCH")
                .help(
                    "Git repository to audit in format: owner/repo#branch

Examples:
  --gh opensvm/aeamcp#main           # Audit main branch of opensvm/aeamcp
  --gh solana-labs/solana#master     # Audit Solana Labs repository
  --gh myorg/myproject#develop       # Audit develop branch

The command will:
1. Clone the specified repository and branch
2. Create a new audit branch with timestamp
3. Run comprehensive security analysis
4. Generate audit reports (Typst/PDF)
5. Commit and push results to the new branch",
                ),
        )
        .arg(
            Arg::new("template")
                .long("template")
                .value_name("PATH")
                .help(
                    "Path to external template file to use instead of built-in templates

Examples:
  --template ./templates/custom.typst    # Use custom Typst template
  --template ./templates/custom.html     # Use custom HTML template
  --template ./templates/custom.json     # Use custom JSON template
  --template ./templates/custom.md       # Use custom Markdown template

If not specified, built-in templates embedded in the binary will be used.",
                ),
        )
        .arg(
            Arg::new("no-commit")
                .long("no-commit")
                .action(ArgAction::SetTrue)
                .help("Don't commit audit results to repository. If no output directory is provided, files will be copied to the current folder."),
        )
}
