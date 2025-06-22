//! OSVM Security Audit System
//!
//! This module provides comprehensive security audit functionality for OSVM CLI,
//! including vulnerability analysis, report generation, and Typst PDF export.

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use std::process::Command;

use crate::utils::diagnostics::{DiagnosticCoordinator, DiagnosticResults, IssueSeverity, IssueCategory};

/// OpenAI API client for AI-powered analysis
pub struct OpenAIClient {
    api_key: String,
    client: reqwest::Client,
}

/// AI analysis result from OpenAI
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AIAnalysis {
    pub enhanced_description: String,
    pub risk_assessment: String,
    pub mitigation_strategy: String,
    pub confidence_score: f32,
    pub additional_cwe_ids: Vec<String>,
}

/// AI-enhanced security finding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AIEnhancedFinding {
    pub original_finding: AuditFinding,
    pub ai_analysis: Option<AIAnalysis>,
}

impl OpenAIClient {
    /// Create a new OpenAI client
    pub fn new(api_key: String) -> Self {
        Self {
            api_key,
            client: reqwest::Client::new(),
        }
    }

    /// Analyze a security finding using OpenAI
    pub async fn analyze_finding(&self, finding: &AuditFinding) -> Result<AIAnalysis> {
        let prompt = format!(
            "Analyze this security finding and provide enhanced insights:\n\n\
            Title: {}\n\
            Description: {}\n\
            Severity: {:?}\n\
            Category: {}\n\
            Current Recommendation: {}\n\n\
            Please provide:\n\
            1. Enhanced description with technical details\n\
            2. Comprehensive risk assessment\n\
            3. Specific mitigation strategy\n\
            4. Confidence score (0.0-1.0)\n\
            5. Additional relevant CWE IDs if applicable\n\n\
            Format your response as JSON with these exact fields:\n\
            - enhanced_description\n\
            - risk_assessment\n\
            - mitigation_strategy\n\
            - confidence_score\n\
            - additional_cwe_ids (array of strings)",
            finding.title,
            finding.description,
            finding.severity,
            finding.category,
            finding.recommendation
        );

        let request_body = serde_json::json!({
            "model": "gpt-4.1",
            "messages": [
                {
                    "role": "system",
                    "content": "You are a cybersecurity expert specializing in Rust and blockchain security. Analyze security findings and provide detailed technical insights in JSON format."
                },
                {
                    "role": "user",
                    "content": prompt
                }
            ],
            "max_tokens": 1000,
            "temperature": 0.3
        });

        let response = self
            .client
            .post("https://api.openai.com/v1/chat/completions")
            .header("Authorization", format!("Bearer {}", self.api_key))
            .header("Content-Type", "application/json")
            .json(&request_body)
            .send()
            .await
            .context("Failed to send request to OpenAI API")?;

        if !response.status().is_success() {
            let error_text = response.text().await.unwrap_or_default();
            anyhow::bail!("OpenAI API request failed: {}", error_text);
        }

        let response_json: serde_json::Value = response
            .json()
            .await
            .context("Failed to parse OpenAI response")?;

        let content = response_json
            .get("choices")
            .and_then(|choices| choices.get(0))
            .and_then(|choice| choice.get("message"))
            .and_then(|message| message.get("content"))
            .and_then(|content| content.as_str())
            .ok_or_else(|| anyhow::anyhow!("Invalid OpenAI response format"))?;

        // Parse the JSON response from OpenAI
        let ai_analysis: AIAnalysis = serde_json::from_str(content)
            .context("Failed to parse AI analysis JSON")?;

        Ok(ai_analysis)
    }

    /// Analyze code content for security issues
    pub async fn analyze_code(&self, code_content: &str, file_path: &str) -> Result<Vec<AuditFinding>> {
        let prompt = format!(
            "Analyze this Rust code file for security vulnerabilities:\n\n\
            File: {}\n\
            Code:\n```rust\n{}\n```\n\n\
            Please identify potential security issues and provide findings in JSON format.\n\
            For each finding, include: id, title, description, severity, category, impact, recommendation.\n\
            Severity should be one of: Critical, High, Medium, Low, Info.\n\
            Category should be one of: Security, Dependencies, Configuration, Network, Performance.\n\
            Respond with a JSON array of findings.",
            file_path,
            code_content
        );

        let request_body = serde_json::json!({
            "model": "gpt-4.1",
            "messages": [
                {
                    "role": "system",
                    "content": "You are a security expert specializing in Rust code analysis. Identify security vulnerabilities and respond with JSON array of findings."
                },
                {
                    "role": "user",
                    "content": prompt
                }
            ],
            "max_tokens": 2000,
            "temperature": 0.2
        });

        let response = self
            .client
            .post("https://api.openai.com/v1/chat/completions")
            .header("Authorization", format!("Bearer {}", self.api_key))
            .header("Content-Type", "application/json")
            .json(&request_body)
            .send()
            .await
            .context("Failed to send code analysis request to OpenAI API")?;

        if !response.status().is_success() {
            return Ok(Vec::new()); // Return empty findings on API failure
        }

        let response_json: serde_json::Value = response
            .json()
            .await
            .context("Failed to parse OpenAI response")?;

        let content = response_json
            .get("choices")
            .and_then(|choices| choices.get(0))
            .and_then(|choice| choice.get("message"))
            .and_then(|message| message.get("content"))
            .and_then(|content| content.as_str())
            .unwrap_or("[]");

        // Parse the JSON response and convert to AuditFinding objects
        let ai_findings: Vec<serde_json::Value> = serde_json::from_str(content)
            .unwrap_or_default();

        let mut findings = Vec::new();
        for (i, finding_json) in ai_findings.iter().enumerate() {
            if let Ok(finding) = self.parse_ai_finding(finding_json, i + 1000, file_path) {
                findings.push(finding);
            }
        }

        Ok(findings)
    }

    /// Parse AI finding JSON into AuditFinding
    fn parse_ai_finding(&self, finding_json: &serde_json::Value, id_offset: usize, file_path: &str) -> Result<AuditFinding> {
        let title = finding_json.get("title")
            .and_then(|v| v.as_str())
            .unwrap_or("AI-detected security issue")
            .to_string();

        let description = finding_json.get("description")
            .and_then(|v| v.as_str())
            .unwrap_or("Security issue identified by AI analysis")
            .to_string();

        let severity_str = finding_json.get("severity")
            .and_then(|v| v.as_str())
            .unwrap_or("Medium");

        let severity = match severity_str {
            "Critical" => AuditSeverity::Critical,
            "High" => AuditSeverity::High,
            "Medium" => AuditSeverity::Medium,
            "Low" => AuditSeverity::Low,
            _ => AuditSeverity::Info,
        };

        let category = finding_json.get("category")
            .and_then(|v| v.as_str())
            .unwrap_or("Security")
            .to_string();

        let impact = finding_json.get("impact")
            .and_then(|v| v.as_str())
            .unwrap_or("Potential security vulnerability")
            .to_string();

        let recommendation = finding_json.get("recommendation")
            .and_then(|v| v.as_str())
            .unwrap_or("Review and address the identified security concern")
            .to_string();

        Ok(AuditFinding {
            id: format!("AI-{:03}", id_offset),
            title,
            description,
            severity,
            category,
            cwe_id: Some("CWE-AI".to_string()),
            cvss_score: Some(5.0),
            impact,
            recommendation,
            code_location: Some(file_path.to_string()),
            references: vec!["AI-generated finding".to_string()],
        })
    }
}

/// Audit severity levels
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AuditSeverity {
    Critical,
    High,
    Medium,
    Low,
    Info,
}

impl From<IssueSeverity> for AuditSeverity {
    fn from(severity: IssueSeverity) -> Self {
        match severity {
            IssueSeverity::Critical => AuditSeverity::Critical,
            IssueSeverity::Error => AuditSeverity::High,
            IssueSeverity::Warning => AuditSeverity::Medium,
            IssueSeverity::Info => AuditSeverity::Info,
        }
    }
}

/// Security audit finding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditFinding {
    pub id: String,
    pub title: String,
    pub description: String,
    pub severity: AuditSeverity,
    pub category: String,
    pub cwe_id: Option<String>,
    pub cvss_score: Option<f32>,
    pub impact: String,
    pub recommendation: String,
    pub code_location: Option<String>,
    pub references: Vec<String>,
}

/// Audit summary statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditSummary {
    pub total_findings: usize,
    pub critical_findings: usize,
    pub high_findings: usize,
    pub medium_findings: usize,
    pub low_findings: usize,
    pub info_findings: usize,
    pub security_score: f32, // 0-100 scale
    pub compliance_level: String,
}

/// Complete audit report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditReport {
    pub timestamp: DateTime<Utc>,
    pub version: String,
    pub summary: AuditSummary,
    pub findings: Vec<AuditFinding>,
    pub system_info: SystemInfo,
    pub recommendations: Vec<String>,
    pub compliance_notes: Vec<String>,
}

/// System information for audit context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemInfo {
    pub rust_version: String,
    pub solana_version: Option<String>,
    pub os_info: String,
    pub architecture: String,
    pub dependencies: HashMap<String, String>,
}

/// Main audit coordinator
pub struct AuditCoordinator {
    ai_client: Option<OpenAIClient>,
}

impl Default for AuditCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

impl AuditCoordinator {
    /// Create a new audit coordinator
    pub fn new() -> Self {
        Self {
            ai_client: None,
        }
    }

    /// Create a new audit coordinator with AI capabilities
    pub fn with_ai(api_key: String) -> Self {
        Self {
            ai_client: Some(OpenAIClient::new(api_key)),
        }
    }

    /// Run comprehensive security audit with optional AI enhancement
    pub async fn run_security_audit(&self) -> Result<AuditReport> {
        println!("🔍 Starting comprehensive security audit...");
        
        if self.ai_client.is_some() {
            println!("🤖 AI-powered analysis enabled");
        }
        
        // Create diagnostic coordinator only when needed
        let diagnostic_coordinator = DiagnosticCoordinator::new();
        
        // Try to run diagnostic checks, but handle errors gracefully
        let diagnostic_results = match diagnostic_coordinator.run_detailed_diagnostics().await {
            Ok(results) => results,
            Err(e) => {
                println!("⚠️  Some diagnostic checks failed: {}", e);
                println!("📝 Proceeding with partial audit data...");
                // Create minimal diagnostic results for the audit
                return self.create_fallback_audit_report().await;
            }
        };

        // Convert diagnostic results to audit findings
        let mut findings = self.convert_diagnostics_to_findings(&diagnostic_results);
        
        // Perform additional security checks
        findings.extend(self.perform_additional_security_checks());
        
        // If AI is enabled, perform AI-enhanced analysis
        if let Some(ref ai_client) = self.ai_client {
            println!("🤖 Running AI-powered code analysis...");
            let ai_findings = self.perform_ai_code_analysis(ai_client).await;
            findings.extend(ai_findings);
            
            // Enhance existing findings with AI analysis
            findings = self.enhance_findings_with_ai(ai_client, findings).await;
        }
        
        // Generate system information
        let system_info = self.collect_system_info().await?;
        
        // Calculate audit summary
        let summary = self.calculate_audit_summary(&findings);
        
        // Generate recommendations
        let recommendations = self.generate_security_recommendations(&findings);
        
        // Generate compliance notes
        let compliance_notes = self.generate_compliance_notes(&findings);

        let audit_report = AuditReport {
            timestamp: Utc::now(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            summary,
            findings,
            system_info,
            recommendations,
            compliance_notes,
        };

        println!("✅ Security audit completed");
        Ok(audit_report)
    }

    /// Create a fallback audit report when diagnostics fail
    async fn create_fallback_audit_report(&self) -> Result<AuditReport> {
        let mut findings = Vec::new();
        
        // Add basic security checks that don't require full diagnostics
        findings.extend(self.perform_additional_security_checks());
        
        // Add a finding about the diagnostic failure
        findings.push(AuditFinding {
            id: "OSVM-999".to_string(),
            title: "Diagnostic system unavailable".to_string(),
            description: "Full system diagnostics could not be completed. This may indicate configuration issues.".to_string(),
            severity: AuditSeverity::Medium,
            category: "System".to_string(),
            cwe_id: Some("CWE-754".to_string()),
            cvss_score: Some(4.0),
            impact: "Limited visibility into system security posture".to_string(),
            recommendation: "Review system configuration and ensure all dependencies are properly installed".to_string(),
            code_location: None,
            references: vec!["https://cwe.mitre.org/data/definitions/754.html".to_string()],
        });
        
        let system_info = self.collect_system_info().await?;
        let summary = self.calculate_audit_summary(&findings);
        let recommendations = self.generate_security_recommendations(&findings);
        let compliance_notes = self.generate_compliance_notes(&findings);

        Ok(AuditReport {
            timestamp: Utc::now(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            summary,
            findings,
            system_info,
            recommendations,
            compliance_notes,
        })
    }

    /// Convert diagnostic results to audit findings
    fn convert_diagnostics_to_findings(&self, diagnostics: &DiagnosticResults) -> Vec<AuditFinding> {
        let mut findings = Vec::new();
        let mut finding_id = 1;

        for issue in &diagnostics.system_health.issues {
            let severity = AuditSeverity::from(issue.severity.clone());
            let category = match issue.category {
                IssueCategory::Security => "Security",
                IssueCategory::SystemDependencies => "Dependencies",
                IssueCategory::UserConfiguration => "Configuration",
                IssueCategory::NetworkConnectivity => "Network",
                IssueCategory::Performance => "Performance",
            };

            let (cwe_id, cvss_score, impact) = self.analyze_security_impact(&issue.title, &issue.description, &severity);

            findings.push(AuditFinding {
                id: format!("OSVM-{:03}", finding_id),
                title: issue.title.clone(),
                description: issue.description.clone(),
                severity,
                category: category.to_string(),
                cwe_id,
                cvss_score,
                impact,
                recommendation: issue.suggested_fix.clone().unwrap_or_else(|| "Manual review required".to_string()),
                code_location: None,
                references: self.get_security_references(category),
            });

            finding_id += 1;
        }

        // Add additional security-specific checks
        findings.extend(self.perform_additional_security_checks());

        findings
    }

    /// Analyze security impact and assign CVE/CVSS scores
    fn analyze_security_impact(&self, title: &str, _description: &str, severity: &AuditSeverity) -> (Option<String>, Option<f32>, String) {
        let (cwe_id, impact) = if title.contains("permissions") || title.contains("keypair") {
            (Some("CWE-276".to_string()), "Unauthorized access to sensitive cryptographic material")
        } else if title.contains("network") || title.contains("connectivity") {
            (Some("CWE-300".to_string()), "Potential network-based attacks or service disruption")
        } else if title.contains("dependency") || title.contains("outdated") {
            (Some("CWE-937".to_string()), "Known vulnerabilities in dependencies")
        } else if title.contains("configuration") {
            (Some("CWE-16".to_string()), "Misconfiguration leading to security weaknesses")
        } else {
            (None, "General security concern requiring assessment")
        };

        let cvss_score = match severity {
            AuditSeverity::Critical => Some(9.0),
            AuditSeverity::High => Some(7.5),
            AuditSeverity::Medium => Some(5.0),
            AuditSeverity::Low => Some(3.0),
            AuditSeverity::Info => Some(1.0),
        };

        (cwe_id, cvss_score, impact.to_string())
    }

    /// Get security references for categories
    fn get_security_references(&self, category: &str) -> Vec<String> {
        match category {
            "Security" => vec![
                "https://owasp.org/Top10/".to_string(),
                "https://cwe.mitre.org/".to_string(),
                "https://docs.solana.com/developing/programming-model/security".to_string(),
            ],
            "Dependencies" => vec![
                "https://rustsec.org/".to_string(),
                "https://docs.rs/cargo-audit/".to_string(),
            ],
            "Configuration" => vec![
                "https://docs.solana.com/running-validator/validator-start".to_string(),
            ],
            _ => vec![],
        }
    }

    /// Perform AI-powered code analysis
    async fn perform_ai_code_analysis(&self, ai_client: &OpenAIClient) -> Vec<AuditFinding> {
        let mut ai_findings = Vec::new();
        
        // Analyze Rust source files
        if let Ok(entries) = std::fs::read_dir("src") {
            for entry in entries.flatten() {
                if let Some(ext) = entry.path().extension() {
                //    if ext == "rs" {
                        if let Ok(content) = std::fs::read_to_string(&entry.path()) {
                            // Limit content size to avoid API limits
                            if content.len() < 80000 {
                                match ai_client.analyze_code(&content, &entry.path().display().to_string()).await {
                                    Ok(findings) => ai_findings.extend(findings),
                                    Err(e) => println!("⚠️  AI analysis failed for {}: {}", entry.path().display(), e),
                                }
                            }
                        }
                 //   }
                }
            }
        }
        
        ai_findings
    }

    /// Enhance existing findings with AI analysis
    async fn enhance_findings_with_ai(&self, ai_client: &OpenAIClient, findings: Vec<AuditFinding>) -> Vec<AuditFinding> {
        let mut enhanced_findings = Vec::new();
        
        for finding in findings {
            if finding.severity == AuditSeverity::Critical || finding.severity == AuditSeverity::High {
                match ai_client.analyze_finding(&finding).await {
                    Ok(ai_analysis) => {
                        let mut enhanced_finding = finding.clone();
                        // Enhance the finding with AI insights
                        enhanced_finding.description = format!("{}\n\nAI Analysis: {}", 
                            enhanced_finding.description, ai_analysis.enhanced_description);
                        enhanced_finding.recommendation = format!("{}\n\nAI Recommendation: {}", 
                            enhanced_finding.recommendation, ai_analysis.mitigation_strategy);
                        enhanced_findings.push(enhanced_finding);
                    }
                    Err(e) => {
                        println!("⚠️  AI enhancement failed for finding {}: {}", finding.id, e);
                        enhanced_findings.push(finding);
                    }
                }
            } else {
                enhanced_findings.push(finding);
            }
        }
        
        enhanced_findings
    }

    /// Perform additional security-specific checks
    fn perform_additional_security_checks(&self) -> Vec<AuditFinding> {
        let mut findings = Vec::new();
        let mut finding_id = 100; // Start from 100 to avoid conflicts

        // Check for hardcoded secrets (basic check)
        if let Ok(entries) = std::fs::read_dir("src") {
            for entry in entries.flatten() {
                if let Some(ext) = entry.path().extension() {
                    if ext == "rs" {
                        if let Ok(content) = std::fs::read_to_string(entry.path()) {
                            if self.contains_potential_secrets(&content) {
                                findings.push(AuditFinding {
                                    id: format!("OSVM-{:03}", finding_id),
                                    title: "Potential hardcoded secrets detected".to_string(),
                                    description: format!("File {} may contain hardcoded secrets", entry.path().display()),
                                    severity: AuditSeverity::High,
                                    category: "Security".to_string(),
                                    cwe_id: Some("CWE-798".to_string()),
                                    cvss_score: Some(8.0),
                                    impact: "Exposed secrets could lead to unauthorized access".to_string(),
                                    recommendation: "Review and remove any hardcoded secrets, use environment variables or secure key management".to_string(),
                                    code_location: Some(entry.path().display().to_string()),
                                    references: vec!["https://cwe.mitre.org/data/definitions/798.html".to_string()],
                                });
                                finding_id += 1;
                            }
                        }
                    }
                }
            }
        }

        findings
    }

    /// Check if content contains potential secrets
    fn contains_potential_secrets(&self, content: &str) -> bool {
        let secret_patterns = [
            r#"(?i)password\s*=\s*['"'][^'"']+['"']"#,
            r#"(?i)secret\s*=\s*['"'][^'"']+['"']"#,
            r#"(?i)key\s*=\s*['"'][^'"']+['"']"#,
            r#"(?i)token\s*=\s*['"'][^'"']+['"']"#,
        ];

        for pattern in &secret_patterns {
            if regex::Regex::new(pattern).unwrap().is_match(content) {
                return true;
            }
        }
        false
    }

    /// Collect system information
    async fn collect_system_info(&self) -> Result<SystemInfo> {
        let rust_version = self.get_rust_version();
        let solana_version = self.get_solana_version().await;
        let os_info = self.get_os_info();
        let architecture = std::env::consts::ARCH.to_string();
        let dependencies = self.get_dependencies();

        Ok(SystemInfo {
            rust_version,
            solana_version,
            os_info,
            architecture,
            dependencies,
        })
    }

    /// Get Rust version
    fn get_rust_version(&self) -> String {
        Command::new("rustc")
            .arg("--version")
            .output()
            .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_string())
            .unwrap_or_else(|_| "Unknown".to_string())
    }

    /// Get Solana version
    async fn get_solana_version(&self) -> Option<String> {
        Command::new("solana")
            .arg("--version")
            .output()
            .ok()
            .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_string())
    }

    /// Get OS information
    fn get_os_info(&self) -> String {
        format!("{} {}", std::env::consts::OS, std::env::consts::ARCH)
    }

    /// Get dependency information
    fn get_dependencies(&self) -> HashMap<String, String> {
        let mut deps = HashMap::new();
        
        // Parse Cargo.toml for dependencies
        if let Ok(content) = std::fs::read_to_string("Cargo.toml") {
            // Simple parsing - in production, use a proper TOML parser
            for line in content.lines() {
                if line.contains("=") && !line.starts_with('#') && !line.starts_with('[') {
                    if let Some((name, version)) = line.split_once('=') {
                        let name = name.trim().to_string();
                        let version = version.trim().trim_matches('"').trim_matches('\'').to_string();
                        if !name.is_empty() && !version.is_empty() && name.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '-') {
                            deps.insert(name, version);
                        }
                    }
                }
            }
        }
        
        deps
    }

    /// Calculate audit summary
    fn calculate_audit_summary(&self, findings: &[AuditFinding]) -> AuditSummary {
        let total_findings = findings.len();
        let critical_findings = findings.iter().filter(|f| f.severity == AuditSeverity::Critical).count();
        let high_findings = findings.iter().filter(|f| f.severity == AuditSeverity::High).count();
        let medium_findings = findings.iter().filter(|f| f.severity == AuditSeverity::Medium).count();
        let low_findings = findings.iter().filter(|f| f.severity == AuditSeverity::Low).count();
        let info_findings = findings.iter().filter(|f| f.severity == AuditSeverity::Info).count();

        // Calculate security score (0-100)
        let security_score = if total_findings == 0 {
            100.0
        } else {
            let weighted_score = (critical_findings * 10 + high_findings * 7 + medium_findings * 4 + low_findings * 2 + info_findings * 1) as f32;
            let max_possible_score = total_findings as f32 * 10.0;
            ((max_possible_score - weighted_score) / max_possible_score * 100.0).max(0.0)
        };

        let compliance_level = match security_score {
            90.0..=100.0 => "Excellent",
            80.0..=89.9 => "Good",
            70.0..=79.9 => "Fair",
            60.0..=69.9 => "Poor",
            _ => "Critical",
        }.to_string();

        AuditSummary {
            total_findings,
            critical_findings,
            high_findings,
            medium_findings,
            low_findings,
            info_findings,
            security_score,
            compliance_level,
        }
    }

    /// Generate security recommendations
    fn generate_security_recommendations(&self, findings: &[AuditFinding]) -> Vec<String> {
        let mut recommendations = Vec::new();

        // Add general recommendations
        recommendations.push("Implement regular security audits and penetration testing".to_string());
        recommendations.push("Keep all dependencies up to date and monitor for security advisories".to_string());
        recommendations.push("Use proper secret management and avoid hardcoding sensitive information".to_string());
        recommendations.push("Implement comprehensive logging and monitoring".to_string());
        recommendations.push("Follow the principle of least privilege for all system components".to_string());

        // Add specific recommendations based on findings
        let has_crypto_issues = findings.iter().any(|f| f.title.contains("keypair") || f.title.contains("crypto"));
        if has_crypto_issues {
            recommendations.push("Review cryptographic key management practices and ensure proper key rotation".to_string());
        }

        let has_network_issues = findings.iter().any(|f| f.category == "Network");
        if has_network_issues {
            recommendations.push("Implement network segmentation and proper firewall configurations".to_string());
        }

        recommendations
    }

    /// Generate compliance notes
    fn generate_compliance_notes(&self, findings: &[AuditFinding]) -> Vec<String> {
        let mut notes = Vec::new();

        notes.push("This audit report follows industry security standards and best practices".to_string());
        notes.push("Findings are categorized using the Common Weakness Enumeration (CWE) framework".to_string());
        notes.push("CVSS scores are provided where applicable to help prioritize remediation efforts".to_string());
        
        if findings.iter().any(|f| f.severity == AuditSeverity::Critical) {
            notes.push("Critical vulnerabilities require immediate attention and remediation".to_string());
        }

        notes.push("Regular security assessments are recommended to maintain security posture".to_string());

        notes
    }

    /// Generate Typst document from audit report
    pub fn generate_typst_document(&self, report: &AuditReport, output_path: &Path) -> Result<()> {
        let typst_content = self.create_typst_content(report)?;
        std::fs::write(output_path, typst_content)
            .context("Failed to write Typst document")?;
        Ok(())
    }

    /// Create a test audit report for demonstration
    pub fn create_test_audit_report(&self) -> AuditReport {
        let mut findings = Vec::new();
        
        // Add some example findings
        findings.push(AuditFinding {
            id: "OSVM-001".to_string(),
            title: "Example security finding".to_string(),
            description: "This is an example security finding for demonstration purposes".to_string(),
            severity: AuditSeverity::Medium,
            category: "Security".to_string(),
            cwe_id: Some("CWE-200".to_string()),
            cvss_score: Some(5.0),
            impact: "Potential information disclosure".to_string(),
            recommendation: "Review and implement proper access controls".to_string(),
            code_location: Some("src/example.rs".to_string()),
            references: vec!["https://cwe.mitre.org/data/definitions/200.html".to_string()],
        });

        // Use hardcoded system info to avoid triggering diagnostics
        let system_info = SystemInfo {
            rust_version: "rustc 1.87.0 (example)".to_string(),
            solana_version: Some("solana-cli 2.2.7 (example)".to_string()),
            os_info: "Linux x86_64".to_string(),
            architecture: "x86_64".to_string(),
            dependencies: {
                let mut deps = HashMap::new();
                deps.insert("clap".to_string(), "4.5.40".to_string());
                deps.insert("tokio".to_string(), "1.45.1".to_string());
                deps.insert("serde".to_string(), "1.0.219".to_string());
                deps
            },
        };

        let summary = AuditSummary {
            total_findings: findings.len(),
            critical_findings: 0,
            high_findings: 0,
            medium_findings: 1,
            low_findings: 0,
            info_findings: 0,
            security_score: 85.0,
            compliance_level: "Good".to_string(),
        };

        AuditReport {
            timestamp: Utc::now(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            summary,
            findings,
            system_info,
            recommendations: vec![
                "Implement regular security audits".to_string(),
                "Keep dependencies up to date".to_string(),
                "Follow security best practices".to_string(),
            ],
            compliance_notes: vec![
                "This audit follows industry security standards".to_string(),
                "Findings are categorized using CWE framework".to_string(),
            ],
        }
    }

    /// Create Typst document content
    fn create_typst_content(&self, report: &AuditReport) -> Result<String> {
        let mut content = String::new();

        // Document header and styling
        content.push_str(r#"#set document(title: "OSVM Security Audit Report")
#set page(numbering: "1")
#set text(font: "Linux Libertine", size: 11pt)
#set heading(numbering: "1.")

#align(center)[
  #text(size: 24pt, weight: "bold")[OSVM Security Audit Report]
  
  #v(1em)
  
  #text(size: 14pt)[Comprehensive Security Assessment]
  
  #v(2em)
  
  #text(size: 12pt)[
    Generated: #datetime.now().display()
    
    Version: "#);
        content.push_str(&report.version);
        content.push_str(r#"
    
    Security Score: "#);
        content.push_str(&format!("{:.1}", report.summary.security_score));
        content.push_str(r#"/100
  ]
]

#pagebreak()

= Executive Summary

This report presents the results of a comprehensive security audit conducted on the OSVM (Open SVM) CLI application. The audit identified "#);
        content.push_str(&report.summary.total_findings.to_string());
        content.push_str(" findings across various security domains.\n\n");

        // Summary table
        content.push_str(r#"#table(
  columns: (auto, auto),
  stroke: none,
  [*Metric*], [*Value*],
  [Total Findings], ["#);
        content.push_str(&report.summary.total_findings.to_string());
        content.push_str(r#"],
  [Critical], ["#);
        content.push_str(&report.summary.critical_findings.to_string());
        content.push_str(r#"],
  [High], ["#);
        content.push_str(&report.summary.high_findings.to_string());
        content.push_str(r#"],
  [Medium], ["#);
        content.push_str(&report.summary.medium_findings.to_string());
        content.push_str(r#"],
  [Low], ["#);
        content.push_str(&report.summary.low_findings.to_string());
        content.push_str(r#"],
  [Info], ["#);
        content.push_str(&report.summary.info_findings.to_string());
        content.push_str(r#"],
  [Security Score], ["#);
        content.push_str(&format!("{:.1}/100", report.summary.security_score));
        content.push_str(r#"],
  [Compliance Level], ["#);
        content.push_str(&report.summary.compliance_level);
        content.push_str(r#"],
)

= System Information

#table(
  columns: (auto, auto),
  stroke: none,
  [*Component*], [*Version*],
  [Rust], ["#);
        content.push_str(&report.system_info.rust_version);
        content.push_str(r#"],
  [Solana], ["#);
        content.push_str(&report.system_info.solana_version.as_deref().unwrap_or("Not installed"));
        content.push_str(r#"],
  [OS], ["#);
        content.push_str(&report.system_info.os_info);
        content.push_str(r#"],
  [Architecture], ["#);
        content.push_str(&report.system_info.architecture);
        content.push_str(r#"],
)

= Security Findings

"#);

        // Add findings
        for finding in &report.findings {
            content.push_str(&format!(
                r#"== {} - {}

*Severity:* {}
*Category:* {}
*CWE ID:* {}
*CVSS Score:* {}

*Description:*
{}

*Impact:*
{}

*Recommendation:*
{}

"#,
                finding.id,
                finding.title,
                format!("{:?}", finding.severity),
                finding.category,
                finding.cwe_id.as_deref().unwrap_or("N/A"),
                finding.cvss_score.map(|s| s.to_string()).unwrap_or("N/A".to_string()),
                finding.description,
                finding.impact,
                finding.recommendation
            ));

            if let Some(ref code_location) = finding.code_location {
                content.push_str(&format!("*Code Location:* {}\n\n", code_location));
            }

            if !finding.references.is_empty() {
                content.push_str("*References:*\n");
                for reference in &finding.references {
                    content.push_str(&format!("- {}\n", reference));
                }
                content.push_str("\n");
            }
        }

        // Add recommendations
        content.push_str(r#"= Security Recommendations

"#);
        for (i, recommendation) in report.recommendations.iter().enumerate() {
            content.push_str(&format!("{}. {}\n\n", i + 1, recommendation));
        }

        // Add compliance notes
        content.push_str(r#"= Compliance Notes

"#);
        for note in &report.compliance_notes {
            content.push_str(&format!("- {}\n", note));
        }

        content.push_str(r#"
= Conclusion

This security audit provides a comprehensive assessment of the OSVM CLI application's security posture. All identified findings should be addressed according to their severity level, with critical and high-severity issues taking priority. Regular security assessments and continuous monitoring are recommended to maintain a strong security stance.

#align(center)[
  #text(size: 10pt, style: "italic")[
    End of Report
  ]
]
"#);

        Ok(content)
    }

    /// Perform GitHub repository audit workflow
    pub async fn audit_github_repository(&self, repo_spec: &str) -> Result<AuditReport> {
        println!("🐙 Starting GitHub repository audit for: {}", repo_spec);
        
        // Parse repository specification (owner/repo#branch)
        let (repo_url, branch) = self.parse_repo_spec(repo_spec)?;
        
        // Clone repository
        let temp_dir = self.clone_repository(&repo_url, &branch)?;
        
        // Create audit branch
        let audit_branch = self.create_audit_branch(&temp_dir)?;
        
        // Perform audit
        let report = self.audit_repository_files(&temp_dir).await?;
        
        // Generate audit files
        self.generate_audit_files_in_repo(&temp_dir, &report).await?;
        
        // Commit and push audit results
        self.commit_and_push_audit(&temp_dir, &audit_branch)?;
        
        println!("✅ GitHub repository audit completed and pushed to branch: {}", audit_branch);
        
        Ok(report)
    }
    
    /// Parse repository specification (owner/repo#branch)
    fn parse_repo_spec(&self, repo_spec: &str) -> Result<(String, String)> {
        let parts: Vec<&str> = repo_spec.split('#').collect();
        if parts.len() != 2 {
            anyhow::bail!("Invalid repository specification. Expected format: owner/repo#branch");
        }
        
        let repo_path = parts[0];
        let branch = parts[1];
        
        if !repo_path.contains('/') {
            anyhow::bail!("Invalid repository path. Expected format: owner/repo");
        }
        
        let repo_url = format!("https://github.com/{}.git", repo_path);
        Ok((repo_url, branch.to_string()))
    }
    
    /// Clone repository to temporary directory
    fn clone_repository(&self, repo_url: &str, branch: &str) -> Result<std::path::PathBuf> {
        let temp_dir = std::env::temp_dir().join(format!("osvm-audit-{}", chrono::Utc::now().timestamp()));
        
        println!("📥 Cloning repository to: {}", temp_dir.display());
        
        let output = Command::new("git")
            .args(&["clone", "--branch", branch, "--single-branch", repo_url])
            .arg(&temp_dir)
            .output()
            .context("Failed to execute git clone")?;
        
        if !output.status.success() {
            let error_msg = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Git clone failed: {}", error_msg);
        }
        
        Ok(temp_dir)
    }
    
    /// Create audit branch with timestamp and commit hash
    fn create_audit_branch(&self, repo_dir: &Path) -> Result<String> {
        let now = chrono::Utc::now();
        let datetime = now.format("%Y%m%d-%H%M%S");
        
        // Get current commit hash
        let commit_output = Command::new("git")
            .args(&["rev-parse", "--short", "HEAD"])
            .current_dir(repo_dir)
            .output()
            .context("Failed to get commit hash")?;
        
        if !commit_output.status.success() {
            anyhow::bail!("Failed to get commit hash");
        }
        
        let commit_hash = String::from_utf8(commit_output.stdout)?.trim().to_string();
        let audit_branch = format!("osvm-audit-{}-{}", datetime, commit_hash);
        
        println!("🌿 Creating audit branch: {}", audit_branch);
        
        // Create and checkout new branch
        let output = Command::new("git")
            .args(&["checkout", "-b", &audit_branch])
            .current_dir(repo_dir)
            .output()
            .context("Failed to create audit branch")?;
        
        if !output.status.success() {
            let error_msg = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Failed to create audit branch: {}", error_msg);
        }
        
        Ok(audit_branch)
    }
    
    /// Audit files in the repository directory
    async fn audit_repository_files(&self, repo_dir: &Path) -> Result<AuditReport> {
        println!("🔍 Performing security audit on repository files...");
        
        // Change to repository directory for audit
        let original_dir = std::env::current_dir()?;
        std::env::set_current_dir(repo_dir)?;
        
        // Run audit
        let report = self.run_security_audit().await;
        
        // Restore original directory
        std::env::set_current_dir(original_dir)?;
        
        report
    }
    
    /// Generate audit files in the repository
    async fn generate_audit_files_in_repo(&self, repo_dir: &Path, report: &AuditReport) -> Result<()> {
        let audit_dir = repo_dir.join("osvm-audit");
        std::fs::create_dir_all(&audit_dir)?;
        
        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
        let typst_path = audit_dir.join(format!("osvm_audit_report_{}.typ", timestamp));
        let pdf_path = audit_dir.join(format!("osvm_audit_report_{}.pdf", timestamp));
        
        // Generate Typst document
        self.generate_typst_document(report, &typst_path)?;
        
        // Try to compile to PDF
        if let Err(e) = self.compile_to_pdf(&typst_path, &pdf_path) {
            println!("⚠️  PDF compilation failed (Typst not installed?): {}", e);
            println!("📄 Typst source file generated: {}", typst_path.display());
        } else {
            println!("📄 Generated audit files:");
            println!("  - Typst: {}", typst_path.display());
            println!("  - PDF: {}", pdf_path.display());
        }
        
        Ok(())
    }
    
    /// Commit and push audit results
    fn commit_and_push_audit(&self, repo_dir: &Path, branch: &str) -> Result<()> {
        println!("💾 Committing audit results...");
        
        // Add audit files
        let output = Command::new("git")
            .args(&["add", "osvm-audit/"])
            .current_dir(repo_dir)
            .output()
            .context("Failed to add audit files")?;
        
        if !output.status.success() {
            let error_msg = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Failed to add audit files: {}", error_msg);
        }
        
        // Commit changes
        let commit_message = format!("Add OSVM security audit report ({})", chrono::Utc::now().format("%Y-%m-%d %H:%M:%S UTC"));
        let output = Command::new("git")
            .args(&["commit", "-m", &commit_message])
            .current_dir(repo_dir)
            .output()
            .context("Failed to commit audit files")?;
        
        if !output.status.success() {
            let error_msg = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Failed to commit audit files: {}", error_msg);
        }
        
        // Push branch
        println!("🚀 Pushing audit branch to origin...");
        let output = Command::new("git")
            .args(&["push", "origin", branch])
            .current_dir(repo_dir)
            .output()
            .context("Failed to push audit branch")?;
        
        if !output.status.success() {
            let error_msg = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Failed to push audit branch: {}", error_msg);
        }
        
        Ok(())
    }

    /// Compile Typst document to PDF
    pub fn compile_to_pdf(&self, typst_file: &Path, output_path: &Path) -> Result<()> {
        println!("📄 Compiling Typst document to PDF...");
        
        let output = Command::new("typst")
            .arg("compile")
            .arg(typst_file)
            .arg(output_path)
            .output()
            .context("Failed to execute typst command")?;

        if !output.status.success() {
            let error_msg = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Typst compilation failed: {}", error_msg);
        }

        println!("✅ PDF generated successfully: {}", output_path.display());
        Ok(())
    }
}
