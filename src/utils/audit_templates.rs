//! Template-based Report Generation
//!
//! This module provides template-based report generation using Tera template engine
//! to improve maintainability and reduce syntax errors in report generation.

use crate::utils::audit::{AuditFinding, AuditReport, AuditSeverity, AuditSummary, SystemInfo};
use anyhow::{Context, Result};
use serde_json::json;
use std::collections::HashMap;
use std::path::Path;
use tera::{Context as TeraContext, Tera};

/// Template-based report generator
pub struct TemplateReportGenerator {
    tera: Tera,
}

impl TemplateReportGenerator {
    /// Create a new template report generator
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        // Register built-in templates
        let typst_template = include_str!("../../templates/audit_report.typ");
        tera.add_raw_template("audit_report.typ", typst_template)
            .context("Failed to register Typst template")?;

        // JSON audit report template
        let json_template = include_str!("../../templates/audit_report.json");
        tera.add_raw_template("audit_report.json", json_template)
            .context("Failed to register JSON template")?;

        // HTML audit report template
        let html_template = include_str!("../../templates/audit_report.html");
        tera.add_raw_template("audit_report.html", html_template)
            .context("Failed to register HTML template")?;

        // Markdown summary template
        let md_template = include_str!("../../templates/audit_summary.md");
        tera.add_raw_template("audit_summary.md", md_template)
            .context("Failed to register Markdown template")?;

        Ok(Self { tera })
    }

    /// Generate report using template
    pub fn generate_report(
        &self,
        report: &AuditReport,
        template_name: &str,
        output_path: &Path,
    ) -> Result<()> {
        let context = self.create_template_context(report)?;
        let rendered = self
            .tera
            .render(template_name, &context)
            .context(format!("Failed to render template: {}", template_name))?;

        std::fs::write(output_path, rendered).context("Failed to write rendered report")?;

        Ok(())
    }

    /// Generate Typst document from audit report
    pub fn generate_typst_document(&self, report: &AuditReport, output_path: &Path) -> Result<()> {
        self.generate_report(report, "audit_report.typ", output_path)
    }

    /// Generate JSON report
    pub fn generate_json_report(&self, report: &AuditReport, output_path: &Path) -> Result<()> {
        self.generate_report(report, "audit_report.json", output_path)
    }

    /// Generate HTML report
    pub fn generate_html_report(&self, report: &AuditReport, output_path: &Path) -> Result<()> {
        self.generate_report(report, "audit_report.html", output_path)
    }

    /// Generate Markdown summary
    pub fn generate_markdown_summary(
        &self,
        report: &AuditReport,
        output_path: &Path,
    ) -> Result<()> {
        self.generate_report(report, "audit_summary.md", output_path)
    }

    /// Create template context from audit report
    fn create_template_context(&self, report: &AuditReport) -> Result<TeraContext> {
        let mut context = TeraContext::new();

        // Basic report information
        context.insert("report", report);
        context.insert(
            "timestamp",
            &report.timestamp.format("%Y-%m-%d %H:%M:%S UTC").to_string(),
        );
        context.insert("version", &report.version);

        // Summary information
        context.insert("summary", &report.summary);
        context.insert("security_score", &report.summary.security_score);
        context.insert("compliance_level", &report.summary.compliance_level);

        // Categorized findings
        let categorized_findings = self.categorize_findings(&report.findings);
        context.insert("categorized_findings", &categorized_findings);

        // Severity-based findings
        let severity_findings = self.group_by_severity(&report.findings);
        context.insert("severity_findings", &severity_findings);

        // System information
        context.insert("system_info", &report.system_info);

        // Statistics
        let stats = self.calculate_statistics(&report.findings);
        context.insert("statistics", &stats);

        // Recommendations and compliance notes
        context.insert("recommendations", &report.recommendations);
        context.insert("compliance_notes", &report.compliance_notes);

        // Helper data
        context.insert("has_critical", &(report.summary.critical_findings > 0));
        context.insert("has_high", &(report.summary.high_findings > 0));
        context.insert(
            "total_serious",
            &(report.summary.critical_findings + report.summary.high_findings),
        );

        Ok(context)
    }

    /// Categorize findings by category
    fn categorize_findings<'a>(
        &self,
        findings: &'a [AuditFinding],
    ) -> HashMap<String, Vec<&'a AuditFinding>> {
        let mut categorized = HashMap::new();

        for finding in findings {
            categorized
                .entry(finding.category.clone())
                .or_insert_with(Vec::new)
                .push(finding);
        }

        categorized
    }

    /// Group findings by severity
    fn group_by_severity<'a>(
        &self,
        findings: &'a [AuditFinding],
    ) -> HashMap<String, Vec<&'a AuditFinding>> {
        let mut grouped = HashMap::new();

        for finding in findings {
            let severity = format!("{:?}", finding.severity);
            grouped
                .entry(severity)
                .or_insert_with(Vec::new)
                .push(finding);
        }

        grouped
    }

    /// Calculate additional statistics
    fn calculate_statistics(&self, findings: &[AuditFinding]) -> serde_json::Value {
        let total = findings.len();
        let with_cwe = findings.iter().filter(|f| f.cwe_id.is_some()).count();
        let with_cvss = findings.iter().filter(|f| f.cvss_score.is_some()).count();
        let with_location = findings
            .iter()
            .filter(|f| f.code_location.is_some())
            .count();

        let categories: std::collections::HashSet<_> =
            findings.iter().map(|f| &f.category).collect();
        let unique_categories = categories.len();

        let avg_cvss = if with_cvss > 0 {
            findings.iter().filter_map(|f| f.cvss_score).sum::<f32>() / with_cvss as f32
        } else {
            0.0
        };

        json!({
            "total_findings": total,
            "findings_with_cwe": with_cwe,
            "findings_with_cvss": with_cvss,
            "findings_with_location": with_location,
            "unique_categories": unique_categories,
            "average_cvss_score": avg_cvss,
            "coverage_percentage": if total > 0 { (with_location as f32 / total as f32) * 100.0 } else { 0.0 }
        })
    }

    /// Add custom template
    pub fn add_template(&mut self, name: &str, content: &str) -> Result<()> {
        self.tera
            .add_raw_template(name, content)
            .context(format!("Failed to add template: {}", name))?;
        Ok(())
    }

    /// List available templates
    pub fn list_templates(&self) -> Vec<String> {
        self.tera
            .get_template_names()
            .map(|s| s.to_string())
            .collect()
    }
}

impl Default for TemplateReportGenerator {
    fn default() -> Self {
        Self::new().expect("Failed to create template generator")
    }
}

/// Enhanced AI error handling with rate limiting and better logging
pub struct EnhancedAIErrorHandler {
    last_error_log: std::sync::Arc<std::sync::Mutex<std::time::Instant>>,
    error_log_threshold: std::time::Duration,
}

impl EnhancedAIErrorHandler {
    /// Create a new AI error handler with rate limiting
    pub fn new() -> Self {
        Self {
            last_error_log: std::sync::Arc::new(std::sync::Mutex::new(
                std::time::Instant::now() - std::time::Duration::from_secs(60),
            )),
            error_log_threshold: std::time::Duration::from_secs(30), // Log at most once every 30 seconds
        }
    }

    /// Check if we should log the error (rate limiting)
    fn should_log_error(&self) -> bool {
        if let Ok(mut last_log) = self.last_error_log.try_lock() {
            let now = std::time::Instant::now();
            if now.duration_since(*last_log) > self.error_log_threshold {
                *last_log = now;
                true
            } else {
                false
            }
        } else {
            false // If we can't acquire the lock, skip logging
        }
    }

    /// Handle AI analysis error with comprehensive but rate-limited logging
    pub fn handle_ai_error_with_context(
        &self,
        error: &anyhow::Error,
        context: &str,
    ) -> Option<String> {
        // Always log the first occurrence or after the threshold period
        if self.should_log_error() {
            log::error!("AI Analysis Error in {}: {}", context, error);
            log::warn!("AI error logging rate-limited to prevent log flooding");

            // Log the full error chain for detailed diagnostics
            let mut current_error = error.source();
            let mut error_level = 1;
            while let Some(err) = current_error {
                log::error!("  Error level {}: {}", error_level, err);
                current_error = err.source();
                error_level += 1;
            }
        }

        // Always provide fallback handling regardless of logging
        Self::handle_ai_error(error, context)
    }

    /// Handle AI analysis error with comprehensive logging (static method for backward compatibility)
    pub fn handle_ai_error(error: &anyhow::Error, _context: &str) -> Option<String> {
        // Check error type and provide specific fallback
        let error_string = error.to_string();

        if error_string.contains("rate limit") || error_string.contains("429") {
            log::warn!("Rate limit exceeded - consider reducing API calls or increasing delays");
            Some("Rate limit exceeded".to_string())
        } else if error_string.contains("timeout") {
            log::warn!("AI request timeout - network or service issues");
            Some("Request timeout".to_string())
        } else if error_string.contains("api_key") || error_string.contains("unauthorized") {
            log::error!("AI API authentication failed - check API key");
            Some("Authentication failed".to_string())
        } else if error_string.contains("network") || error_string.contains("connection") {
            log::warn!("Network connectivity issues with AI service");
            Some("Network error".to_string())
        } else {
            log::error!("Unknown AI service error - disabling AI analysis for this session");
            None
        }
    }

    /// Generate fallback analysis when AI fails
    pub fn generate_fallback_analysis(finding_title: &str, finding_category: &str) -> String {
        format!(
            "AI analysis unavailable. Manual review recommended for {} in category {}. \
            Consider checking relevant security guidelines and best practices.",
            finding_title, finding_category
        )
    }

    /// Check if error should disable AI analysis
    pub fn should_disable_ai(error: &anyhow::Error) -> bool {
        let error_string = error.to_string();
        error_string.contains("api_key")
            || error_string.contains("unauthorized")
            || error_string.contains("quota")
            || error_string.contains("suspended")
    }

    /// Create retry strategy based on error type
    pub fn get_retry_strategy(error: &anyhow::Error) -> (bool, std::time::Duration) {
        let error_string = error.to_string();

        if error_string.contains("rate limit") || error_string.contains("429") {
            (true, std::time::Duration::from_secs(60)) // Retry after 1 minute
        } else if error_string.contains("timeout") || error_string.contains("network") {
            (true, std::time::Duration::from_secs(5)) // Quick retry for network issues
        } else if error_string.contains("server") || error_string.contains("503") {
            (true, std::time::Duration::from_secs(30)) // Retry after 30 seconds for server issues
        } else {
            (false, std::time::Duration::from_secs(0)) // Don't retry for other errors
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use std::collections::HashMap;

    fn create_test_report() -> AuditReport {
        let findings = vec![AuditFinding {
            id: "TEST-001".to_string(),
            title: "Test finding".to_string(),
            description: "Test description".to_string(),
            severity: AuditSeverity::High,
            category: "Test".to_string(),
            cwe_id: Some("CWE-123".to_string()),
            cvss_score: Some(7.5),
            impact: "Test impact".to_string(),
            recommendation: "Test recommendation".to_string(),
            code_location: Some("test.rs".to_string()),
            references: vec!["https://example.com".to_string()],
        }];

        let summary = AuditSummary {
            total_findings: 1,
            critical_findings: 0,
            high_findings: 1,
            medium_findings: 0,
            low_findings: 0,
            info_findings: 0,
            security_score: 75.0,
            compliance_level: "Good".to_string(),
        };

        let system_info = SystemInfo {
            rust_version: "1.70.0".to_string(),
            solana_version: Some("1.16.0".to_string()),
            os_info: "Linux".to_string(),
            architecture: "x86_64".to_string(),
            dependencies: HashMap::new(),
        };

        AuditReport {
            timestamp: Utc::now(),
            version: "1.0.0".to_string(),
            summary,
            findings,
            system_info,
            recommendations: vec!["Test recommendation".to_string()],
            compliance_notes: vec!["Test compliance note".to_string()],
        }
    }

    #[test]
    fn test_template_generator_creation() {
        let generator = TemplateReportGenerator::new();
        assert!(generator.is_ok());
    }

    #[test]
    fn test_categorize_findings() {
        let generator = TemplateReportGenerator::new().unwrap();
        let report = create_test_report();

        let categorized = generator.categorize_findings(&report.findings);
        assert!(categorized.contains_key("Test"));
        assert_eq!(categorized["Test"].len(), 1);
    }

    #[test]
    fn test_ai_error_handler() {
        let error = anyhow::anyhow!("rate limit exceeded");
        let result = EnhancedAIErrorHandler::handle_ai_error(&error, "test");
        assert_eq!(result, Some("Rate limit exceeded".to_string()));

        let retry_info = EnhancedAIErrorHandler::get_retry_strategy(&error);
        assert!(retry_info.0); // Should retry
        assert!(retry_info.1.as_secs() > 0); // Should have delay
    }

    #[test]
    fn test_fallback_analysis() {
        let fallback =
            EnhancedAIErrorHandler::generate_fallback_analysis("Test Finding", "Security");
        assert!(fallback.contains("Test Finding"));
        assert!(fallback.contains("Security"));
    }
}
