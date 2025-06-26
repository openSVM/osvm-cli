use crate::utils::audit::AuditCoordinator;
use std::path::Path;
use std::{error::Error, fs};

pub struct AuditService {
    coordinator: AuditCoordinator,
}

#[derive(Debug)]
pub enum AuditError {
    ConfigurationError(String),
    OutputError(String),
    AuditExecutionError(String),
    EnvironmentError(String),
}

impl std::fmt::Display for AuditError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AuditError::ConfigurationError(msg) => write!(f, "Configuration error: {}", msg),
            AuditError::OutputError(msg) => write!(f, "Output error: {}", msg),
            AuditError::AuditExecutionError(msg) => write!(f, "Audit execution error: {}", msg),
            AuditError::EnvironmentError(msg) => write!(f, "Environment error: {}", msg),
        }
    }
}

impl Error for AuditError {}

pub struct AuditRequest {
    pub output_dir: String,
    pub format: String,
    pub verbose: u8,
    pub test_mode: bool,
    pub ai_analysis: bool,
    pub gh_repo: Option<String>,
}

pub struct AuditResult {
    pub success: bool,
    pub security_score: f64,
    pub total_findings: usize,
    pub critical_findings: usize,
    pub high_findings: usize,
    pub medium_findings: usize,
    pub low_findings: usize,
    pub compliance_level: String,
    pub output_files: Vec<String>,
}

impl AuditService {
    pub fn new() -> Self {
        Self {
            coordinator: AuditCoordinator::new(),
        }
    }

    pub fn with_ai(api_key: String) -> Self {
        Self {
            coordinator: AuditCoordinator::with_ai(api_key),
        }
    }

    pub fn with_optional_ai(api_key: Option<String>) -> Self {
        Self {
            coordinator: AuditCoordinator::with_optional_ai(api_key),
        }
    }

    pub fn validate_environment(request: &AuditRequest) -> Result<(), AuditError> {
        // Always check OpenAI API key status for better user guidance
        match std::env::var("OPENAI_API_KEY") {
            Ok(key) if !key.trim().is_empty() => {
                if request.ai_analysis {
                    println!("🤖 AI analysis will be enabled with provided API key");
                } else {
                    println!("ℹ️  OPENAI_API_KEY detected but AI analysis is disabled. Use --ai-analysis to enable.");
                }
            }
            Ok(_) => {
                if request.ai_analysis {
                    return Err(AuditError::EnvironmentError(
                        "OPENAI_API_KEY is empty but AI analysis was requested. Please provide a valid API key or disable AI analysis.".to_string()
                    ));
                }
            }
            Err(_) => {
                if request.ai_analysis {
                    return Err(AuditError::EnvironmentError(
                        "OPENAI_API_KEY not found but AI analysis was requested. Please set the environment variable or disable AI analysis.".to_string()
                    ));
                }
            }
        }

        // Validate output format
        match request.format.as_str() {
            "typst" | "pdf" | "both" | "json" | "html" | "markdown" => {}
            _ => {
                return Err(AuditError::ConfigurationError(format!(
                    "Invalid format '{}'. Valid formats: typst, pdf, both, json, html, markdown",
                    request.format
                )))
            }
        }

        Ok(())
    }

    pub fn prepare_output_directory(output_dir: &str) -> Result<(), AuditError> {
        fs::create_dir_all(output_dir).map_err(|e| {
            AuditError::OutputError(format!(
                "Failed to create output directory '{}': {}",
                output_dir, e
            ))
        })?;
        Ok(())
    }

    pub async fn execute_audit(&self, request: &AuditRequest) -> Result<AuditResult, AuditError> {
        // Validate environment first
        Self::validate_environment(request)?;

        // Prepare output directory
        Self::prepare_output_directory(&request.output_dir)?;

        if request.verbose > 0 {
            println!("🔍 OSVM Security Audit");
            println!("======================");
            println!("📁 Output directory: {}", request.output_dir);
            println!("📄 Format: {}", request.format);
            if request.test_mode {
                println!("🧪 Test mode: generating sample audit report");
            }
            if request.ai_analysis {
                println!("🤖 AI analysis: enabled");
            }
            if let Some(repo) = &request.gh_repo {
                println!("🐙 GitHub repository: {}", repo);
            }
        }

        // Handle different audit modes
        let report = if let Some(repo_spec) = &request.gh_repo {
            // GitHub repository audit mode
            if request.verbose > 0 {
                println!("🐙 GitHub repository audit mode");
            }

            self.coordinator
                .audit_github_repository(repo_spec)
                .await
                .map_err(|e| {
                    AuditError::AuditExecutionError(format!("GitHub audit failed: {}", e))
                })?;

            // For GitHub mode, we return early as files are already generated and committed
            return Ok(AuditResult {
                success: true,
                security_score: 0.0, // Not applicable for GitHub mode
                total_findings: 0,
                critical_findings: 0,
                high_findings: 0,
                medium_findings: 0,
                low_findings: 0,
                compliance_level: "N/A".to_string(),
                output_files: vec!["GitHub repository".to_string()],
            });
        } else if request.test_mode {
            if request.verbose > 0 {
                println!("🧪 Generating test audit report...");
            }
            self.coordinator.create_test_audit_report()
        } else {
            // Run security audit
            self.coordinator.run_security_audit().await.map_err(|e| {
                AuditError::AuditExecutionError(format!("Security audit failed: {}", e))
            })?
        };

        if request.verbose > 0 {
            println!("✅ Security audit completed successfully");
            println!(
                "📊 Security Score: {:.1}/100",
                report.summary.security_score
            );
            println!("🔍 Total Findings: {}", report.summary.total_findings);

            if report.summary.critical_findings > 0 {
                println!("🔴 Critical: {}", report.summary.critical_findings);
            }
            if report.summary.high_findings > 0 {
                println!("🟠 High: {}", report.summary.high_findings);
            }
            if report.summary.medium_findings > 0 {
                println!("🟡 Medium: {}", report.summary.medium_findings);
            }
            if report.summary.low_findings > 0 {
                println!("🔵 Low: {}", report.summary.low_findings);
            }
        }

        // Generate outputs based on requested format
        let mut output_files = Vec::new();
        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
        let typst_path =
            Path::new(&request.output_dir).join(format!("osvm_audit_report_{}.typ", timestamp));
        let pdf_path =
            Path::new(&request.output_dir).join(format!("osvm_audit_report_{}.pdf", timestamp));
        let json_path =
            Path::new(&request.output_dir).join(format!("osvm_audit_report_{}.json", timestamp));
        let html_path =
            Path::new(&request.output_dir).join(format!("osvm_audit_report_{}.html", timestamp));
        let markdown_path =
            Path::new(&request.output_dir).join(format!("osvm_audit_summary_{}.md", timestamp));

        match request.format.as_str() {
            "typst" | "both" => {
                self.coordinator
                    .generate_typst_document(&report, &typst_path)
                    .map_err(|e| {
                        AuditError::OutputError(format!("Failed to generate Typst document: {}", e))
                    })?;

                if request.verbose > 0 {
                    println!("📄 Typst document generated: {}", typst_path.display());
                }
                output_files.push(typst_path.to_string_lossy().to_string());

                if request.format == "both" {
                    match self.coordinator.compile_to_pdf(&typst_path, &pdf_path) {
                        Ok(_) => {
                            if request.verbose > 0 {
                                println!("📋 PDF report generated: {}", pdf_path.display());
                            }
                            output_files.push(pdf_path.to_string_lossy().to_string());
                        }
                        Err(e) => {
                            eprintln!("❌ Failed to compile PDF: {}", e);
                            eprintln!(
                                "   Typst document is available at: {}",
                                typst_path.display()
                            );
                            eprintln!(
                                "   You can compile it manually using: typst compile {}",
                                typst_path.display()
                            );
                        }
                    }
                }
            }
            "pdf" => {
                // Generate Typst document first (temporary)
                self.coordinator
                    .generate_typst_document(&report, &typst_path)
                    .map_err(|e| {
                        AuditError::OutputError(format!("Failed to generate Typst document: {}", e))
                    })?;

                self.coordinator
                    .compile_to_pdf(&typst_path, &pdf_path)
                    .map_err(|e| {
                        AuditError::OutputError(format!("Failed to compile PDF: {}", e))
                    })?;

                // Remove temporary Typst file
                let _ = fs::remove_file(&typst_path);

                if request.verbose > 0 {
                    println!("📋 PDF report generated: {}", pdf_path.display());
                }
                output_files.push(pdf_path.to_string_lossy().to_string());
            }
            "json" => {
                self.coordinator
                    .generate_json_report(&report, &json_path)
                    .map_err(|e| {
                        AuditError::OutputError(format!("Failed to generate JSON report: {}", e))
                    })?;

                if request.verbose > 0 {
                    println!("📄 JSON report generated: {}", json_path.display());
                }
                output_files.push(json_path.to_string_lossy().to_string());
            }
            "html" => {
                self.coordinator
                    .generate_html_report(&report, &html_path)
                    .map_err(|e| {
                        AuditError::OutputError(format!("Failed to generate HTML report: {}", e))
                    })?;

                if request.verbose > 0 {
                    println!("📄 HTML report generated: {}", html_path.display());
                }
                output_files.push(html_path.to_string_lossy().to_string());
            }
            "markdown" => {
                self.coordinator
                    .generate_markdown_summary(&report, &markdown_path)
                    .map_err(|e| {
                        AuditError::OutputError(format!("Failed to generate Markdown summary: {}", e))
                    })?;

                if request.verbose > 0 {
                    println!("📄 Markdown summary generated: {}", markdown_path.display());
                }
                output_files.push(markdown_path.to_string_lossy().to_string());
            }
            _ => {
                return Err(AuditError::ConfigurationError(format!(
                    "Invalid format specified: {}",
                    request.format
                )))
            }
        }

        if request.verbose > 0 {
            println!("\n📋 Audit Summary:");
            println!("  Compliance Level: {}", report.summary.compliance_level);
            println!(
                "  System: {} {}",
                report.system_info.os_info, report.system_info.architecture
            );
            println!("  Rust Version: {}", report.system_info.rust_version);
            if let Some(ref solana_version) = report.system_info.solana_version {
                println!("  Solana Version: {}", solana_version);
            }

            println!("\n💡 To view the full report, open the generated file(s):");
            for file in &output_files {
                println!("   📁 {}", file);
            }
        }

        // Check if exit with error code is needed (critical or high findings)
        let has_serious_findings = !request.test_mode
            && (report.summary.critical_findings > 0 || report.summary.high_findings > 0);

        Ok(AuditResult {
            success: !has_serious_findings,
            security_score: report.summary.security_score as f64,
            total_findings: report.summary.total_findings,
            critical_findings: report.summary.critical_findings,
            high_findings: report.summary.high_findings,
            medium_findings: report.summary.medium_findings,
            low_findings: report.summary.low_findings,
            compliance_level: report.summary.compliance_level.clone(),
            output_files,
        })
    }
}
