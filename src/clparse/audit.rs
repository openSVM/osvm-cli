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
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm audit .
     Audit the current directory as a project.
     💡 Scans Rust/Solana code for security vulnerabilities.

  2. osvm audit --gh opensvm/aeamcp#main
     Audit a GitHub repository directly.
     💡 Clones, analyzes, and generates report automatically.

  3. osvm audit . --format pdf
     Generate a professional PDF audit report.
     💡 Includes: findings, severity, recommendations.

  4. osvm audit . --format html -o ./reports
     Generate HTML report in custom directory.
     💡 HTML format is best for sharing via web.

  5. osvm audit --test
     Generate test report with sample data.
     💡 Great for previewing report format.

  6. osvm audit . --noai
     Run audit without AI-powered analysis.
     💡 Faster, but less comprehensive findings.

  7. osvm audit . --template ./my-template.typst
     Use custom Typst template for PDF.
     💡 Brand your audit reports with custom styling.

  8. osvm audit --gh solana-labs/solana#master -v
     Verbose audit of Solana core repository.
     💡 Shows detailed progress and findings.

  9. osvm audit . --format json | jq '.findings[]'
     JSON output for programmatic processing.
     💡 Integrate with CI/CD pipelines.

 10. osvm audit --gh myorg/project#develop --no-commit
     Audit without committing results to repo.
     💡 Useful for local review before sharing.

💡 AUDIT PHASES:
  1. Static Analysis:   Rust code patterns, unsafe blocks
  2. Solana-Specific:   Account validation, signer checks
  3. AI Analysis:       Contextual vulnerability detection
  4. Report Generation: Professional formatted output

SEVERITY LEVELS:
  🔴 Critical: Immediate exploitation risk
  🟠 High:     Significant security impact
  🟡 Medium:   Moderate risk, should fix
  🔵 Low:      Minor issues, best practices
  ⚪ Info:     Informational findings

SUPPORTED FORMATS:
  • typst:    Source format for Typst typesetting
  • pdf:      Professional PDF document
  • html:     Web-viewable HTML report
  • markdown: GitHub/GitLab compatible
  • json:     Machine-readable for automation
"#)
}
