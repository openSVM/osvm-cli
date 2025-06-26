//! Modular Audit System
//!
//! This module provides a modular approach to security auditing with separate
//! modules for different vulnerability categories and testable components.

use crate::utils::audit::{AuditFinding, AuditSeverity};
use crate::utils::audit_parser::{ParsedCodeAnalysis, RustCodeParser};
use anyhow::Result;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Global finding ID allocator to ensure unique IDs
static FINDING_ID_COUNTER: AtomicUsize = AtomicUsize::new(1);

/// Cached regex patterns for performance
static REGEX_CACHE: Lazy<HashMap<&'static str, regex::Regex>> = Lazy::new(|| {
    let mut cache = HashMap::new();
    
    // Secret patterns
    cache.insert("password_pattern", regex::Regex::new(r#"(?i)password\s*=\s*['"'][^'"']+['"']"#).unwrap());
    cache.insert("api_key_pattern", regex::Regex::new(r#"(?i)api_key\s*=\s*['"'][^'"']+['"']"#).unwrap());
    cache.insert("secret_pattern", regex::Regex::new(r#"(?i)secret\s*=\s*['"'][^'"']+['"']"#).unwrap());
    cache.insert("hex_key_pattern", regex::Regex::new(r#"['"'][0-9a-fA-F]{32,}['"']"#).unwrap());
    cache.insert("base64_pattern", regex::Regex::new(r#"['"'][A-Za-z0-9+/]{20,}={0,2}['"']"#).unwrap());
    
    // Command injection patterns
    cache.insert("command_injection", regex::Regex::new(r#"Command::new\([^)]*format!"#).unwrap());
    cache.insert("shell_command", regex::Regex::new(r#"shell\("#).unwrap());
    
    // Path traversal patterns
    cache.insert("path_traversal", regex::Regex::new(r#"\.\./"#).unwrap());
    cache.insert("dynamic_path", regex::Regex::new(r#"Path::new\([^)]*format!"#).unwrap());
    
    // Network patterns
    cache.insert("http_insecure", regex::Regex::new(r#"http://.*(?!localhost|127\.0\.0\.1)"#).unwrap());
    cache.insert("tls_bypass", regex::Regex::new(r#"danger_accept_invalid_certs\(true\)"#).unwrap());
    
    // Solana-specific patterns
    cache.insert("solana_signer", regex::Regex::new(r#"AccountInfo.*without.*is_signer"#).unwrap());
    cache.insert("solana_pda", regex::Regex::new(r#"find_program_address"#).unwrap());
    cache.insert("solana_owner", regex::Regex::new(r#"AccountInfo.*owner"#).unwrap());
    
    cache
});

/// Finding ID allocator for unique ID generation
pub struct FindingIdAllocator;

impl FindingIdAllocator {
    /// Generate a unique finding ID
    pub fn next_id() -> String {
        let id = FINDING_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        format!("OSVM-{:03}", id)
    }

    /// Generate a category-specific finding ID
    pub fn next_category_id(category: &str) -> String {
        let id = FINDING_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        match category {
            "solana" => format!("OSVM-SOL-{:03}", id),
            "crypto" => format!("OSVM-CRYPTO-{:03}", id),
            "network" => format!("OSVM-NET-{:03}", id),
            "auth" => format!("OSVM-AUTH-{:03}", id),
            _ => format!("OSVM-{:03}", id),
        }
    }

    /// Reset counter (for testing)
    #[cfg(test)]
    pub fn reset() {
        FINDING_ID_COUNTER.store(1, Ordering::SeqCst);
    }
}

/// Trait for audit check modules
pub trait AuditCheck {
    /// Name of the audit check
    fn name(&self) -> &'static str;
    
    /// Category of vulnerabilities this check covers
    fn category(&self) -> &'static str;
    
    /// Run the audit check on parsed code
    fn check(&self, analysis: &ParsedCodeAnalysis, file_path: &str) -> Result<Vec<AuditFinding>>;
    
    /// Run the audit check on raw content (for backward compatibility)
    fn check_content(&self, content: &str, file_path: &str) -> Result<Vec<AuditFinding>> {
        let analysis = RustCodeParser::parse_code(content)?;
        self.check(&analysis, file_path)
    }
}

/// Memory safety audit checks
pub struct MemorySafetyCheck;

impl AuditCheck for MemorySafetyCheck {
    fn name(&self) -> &'static str {
        "Memory Safety"
    }

    fn category(&self) -> &'static str {
        "Memory"
    }

    fn check(&self, analysis: &ParsedCodeAnalysis, file_path: &str) -> Result<Vec<AuditFinding>> {
        let mut findings = Vec::new();

        // Check for unsafe blocks
        for unsafe_block in &analysis.unsafe_blocks {
            findings.push(AuditFinding {
                id: FindingIdAllocator::next_id(),
                title: "Unsafe code block detected".to_string(),
                description: format!(
                    "File {} contains unsafe code blocks that bypass Rust's memory safety guarantees at line {}",
                    file_path, unsafe_block.line
                ),
                severity: AuditSeverity::Medium,
                category: "Memory Safety".to_string(),
                cwe_id: Some("CWE-119".to_string()),
                cvss_score: Some(5.5),
                impact: "Potential memory safety violations and buffer overflows".to_string(),
                recommendation: "Review unsafe code blocks carefully, ensure proper bounds checking and memory management".to_string(),
                code_location: Some(file_path.to_string()),
                references: vec![
                    "https://doc.rust-lang.org/book/ch19-01-unsafe-rust.html".to_string(),
                    "https://cwe.mitre.org/data/definitions/119.html".to_string(),
                ],
            });
        }

        // Check for excessive unwrap usage
        let unwrap_count = analysis.unwrap_usages.len();
        if unwrap_count > 5 {
            findings.push(AuditFinding {
                id: FindingIdAllocator::next_id(),
                title: "Excessive unwrap/expect usage".to_string(),
                description: format!(
                    "File {} contains {} instances of unwrap/expect which can cause panics",
                    file_path, unwrap_count
                ),
                severity: AuditSeverity::Medium,
                category: "Error Handling".to_string(),
                cwe_id: Some("CWE-248".to_string()),
                cvss_score: Some(4.0),
                impact: "Application crashes due to unhandled panics, potential denial of service".to_string(),
                recommendation: "Replace unwrap/expect with proper error handling using match or if let patterns".to_string(),
                code_location: Some(file_path.to_string()),
                references: vec![
                    "https://doc.rust-lang.org/book/ch09-00-error-handling.html".to_string(),
                    "https://cwe.mitre.org/data/definitions/248.html".to_string(),
                ],
            });
        }

        Ok(findings)
    }
}

/// Cryptographic security audit checks
pub struct CryptographyCheck;

impl AuditCheck for CryptographyCheck {
    fn name(&self) -> &'static str {
        "Cryptography"
    }

    fn category(&self) -> &'static str {
        "Cryptography"
    }

    fn check(&self, analysis: &ParsedCodeAnalysis, file_path: &str) -> Result<Vec<AuditFinding>> {
        let mut findings = Vec::new();

        // Check for hardcoded secrets
        for secret in &analysis.hardcoded_secrets {
            findings.push(AuditFinding {
                id: FindingIdAllocator::next_category_id("crypto"),
                title: "Hardcoded secret detected".to_string(),
                description: format!(
                    "File {} contains a hardcoded {} at line {}",
                    file_path, secret.secret_type, secret.line
                ),
                severity: AuditSeverity::High,
                category: "Cryptography".to_string(),
                cwe_id: Some("CWE-798".to_string()),
                cvss_score: Some(8.0),
                impact: "Exposed secrets could lead to unauthorized access".to_string(),
                recommendation: "Remove hardcoded secrets and use environment variables or secure key management".to_string(),
                code_location: Some(file_path.to_string()),
                references: vec!["https://cwe.mitre.org/data/definitions/798.html".to_string()],
            });
        }

        // Check for weak crypto operations
        for crypto_op in &analysis.crypto_operations {
            if crypto_op.operation_type.contains("weak") || crypto_op.operation_type.contains("md5") {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("crypto"),
                    title: "Weak cryptographic algorithm".to_string(),
                    description: format!(
                        "File {} uses weak cryptographic algorithm at line {}",
                        file_path, crypto_op.line
                    ),
                    severity: AuditSeverity::Medium,
                    category: "Cryptography".to_string(),
                    cwe_id: Some("CWE-327".to_string()),
                    cvss_score: Some(5.0),
                    impact: "Use of weak cryptographic algorithms may allow attacks".to_string(),
                    recommendation: "Use strong, modern cryptographic algorithms".to_string(),
                    code_location: Some(file_path.to_string()),
                    references: vec!["https://cwe.mitre.org/data/definitions/327.html".to_string()],
                });
            }
        }

        Ok(findings)
    }
}

/// Network security audit checks
pub struct NetworkSecurityCheck;

impl AuditCheck for NetworkSecurityCheck {
    fn name(&self) -> &'static str {
        "Network Security"
    }

    fn category(&self) -> &'static str {
        "Network"
    }

    fn check(&self, analysis: &ParsedCodeAnalysis, file_path: &str) -> Result<Vec<AuditFinding>> {
        let mut findings = Vec::new();

        // Check for insecure network operations
        for network_op in &analysis.network_operations {
            if !network_op.uses_https {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("network"),
                    title: "Insecure HTTP usage detected".to_string(),
                    description: format!(
                        "File {} uses HTTP instead of HTTPS at line {}",
                        file_path, network_op.line
                    ),
                    severity: AuditSeverity::Medium,
                    category: "Network Security".to_string(),
                    cwe_id: Some("CWE-319".to_string()),
                    cvss_score: Some(5.0),
                    impact: "Data transmitted in plain text, susceptible to interception".to_string(),
                    recommendation: "Use HTTPS for all external network communications".to_string(),
                    code_location: Some(file_path.to_string()),
                    references: vec![
                        "https://cwe.mitre.org/data/definitions/319.html".to_string(),
                        "https://owasp.org/Top10/A02_2021-Cryptographic_Failures/".to_string(),
                    ],
                });
            }
        }

        Ok(findings)
    }
}

/// Solana-specific security audit checks
pub struct SolanaSecurityCheck;

impl AuditCheck for SolanaSecurityCheck {
    fn name(&self) -> &'static str {
        "Solana Security"
    }

    fn category(&self) -> &'static str {
        "Solana"
    }

    fn check(&self, analysis: &ParsedCodeAnalysis, file_path: &str) -> Result<Vec<AuditFinding>> {
        let mut findings = Vec::new();

        // Check for Solana operations without proper validation
        for solana_op in &analysis.solana_operations {
            if !solana_op.signer_check {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("solana"),
                    title: "Missing signer validation in Solana operation".to_string(),
                    description: format!(
                        "File {} contains Solana operation without signer validation at line {}",
                        file_path, solana_op.line
                    ),
                    severity: AuditSeverity::Critical,
                    category: "Solana Security".to_string(),
                    cwe_id: Some("CWE-862".to_string()),
                    cvss_score: Some(9.0),
                    impact: "Unauthorized users could execute privileged operations".to_string(),
                    recommendation: "Always validate that required accounts are signers using is_signer checks".to_string(),
                    code_location: Some(file_path.to_string()),
                    references: vec![
                        "https://book.anchor-lang.com/anchor_bts/security.html".to_string(),
                    ],
                });
            }

            if !solana_op.account_validation {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("solana"),
                    title: "Missing account validation in Solana operation".to_string(),
                    description: format!(
                        "File {} contains Solana operation without account validation at line {}",
                        file_path, solana_op.line
                    ),
                    severity: AuditSeverity::High,
                    category: "Solana Security".to_string(),
                    cwe_id: Some("CWE-284".to_string()),
                    cvss_score: Some(7.5),
                    impact: "Programs could operate on accounts owned by malicious programs".to_string(),
                    recommendation: "Always verify account ownership before performing operations".to_string(),
                    code_location: Some(file_path.to_string()),
                    references: vec![
                        "https://book.anchor-lang.com/anchor_bts/security.html".to_string(),
                    ],
                });
            }
        }

        Ok(findings)
    }
}

/// Input validation audit checks
pub struct InputValidationCheck;

impl AuditCheck for InputValidationCheck {
    fn name(&self) -> &'static str {
        "Input Validation"
    }

    fn category(&self) -> &'static str {
        "Input"
    }

    fn check(&self, analysis: &ParsedCodeAnalysis, file_path: &str) -> Result<Vec<AuditFinding>> {
        let mut findings = Vec::new();

        // Check for command injection vulnerabilities
        for cmd_exec in &analysis.command_executions {
            if cmd_exec.is_dynamic {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_id(),
                    title: "Potential command injection vulnerability".to_string(),
                    description: format!(
                        "File {} contains command execution with potentially unsafe input at line {}",
                        file_path, cmd_exec.line
                    ),
                    severity: AuditSeverity::High,
                    category: "Input Validation".to_string(),
                    cwe_id: Some("CWE-78".to_string()),
                    cvss_score: Some(7.5),
                    impact: "Arbitrary command execution on the host system".to_string(),
                    recommendation: "Validate and sanitize all input before using in commands, use parameterized commands".to_string(),
                    code_location: Some(file_path.to_string()),
                    references: vec![
                        "https://cwe.mitre.org/data/definitions/78.html".to_string(),
                        "https://owasp.org/Top10/A03_2021-Injection/".to_string(),
                    ],
                });
            }
        }

        // Check for path traversal vulnerabilities
        for path_op in &analysis.path_operations {
            if path_op.is_dynamic {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_id(),
                    title: "Potential path traversal vulnerability".to_string(),
                    description: format!(
                        "File {} contains file operations with potentially unsafe paths at line {}",
                        file_path, path_op.line
                    ),
                    severity: AuditSeverity::High,
                    category: "Input Validation".to_string(),
                    cwe_id: Some("CWE-22".to_string()),
                    cvss_score: Some(7.0),
                    impact: "Unauthorized access to files outside intended directory".to_string(),
                    recommendation: "Validate and canonicalize file paths, use safe path construction methods".to_string(),
                    code_location: Some(file_path.to_string()),
                    references: vec![
                        "https://cwe.mitre.org/data/definitions/22.html".to_string(),
                        "https://owasp.org/Top10/A01_2021-Broken_Access_Control/".to_string(),
                    ],
                });
            }
        }

        Ok(findings)
    }
}

/// Modular audit coordinator that uses individual check modules
pub struct ModularAuditCoordinator {
    checks: Vec<Box<dyn AuditCheck + Send + Sync>>,
}

impl ModularAuditCoordinator {
    /// Create a new modular audit coordinator with default checks
    pub fn new() -> Self {
        let checks: Vec<Box<dyn AuditCheck + Send + Sync>> = vec![
            Box::new(MemorySafetyCheck),
            Box::new(CryptographyCheck),
            Box::new(NetworkSecurityCheck),
            Box::new(SolanaSecurityCheck),
            Box::new(InputValidationCheck),
        ];

        Self { checks }
    }

    /// Add a custom audit check
    pub fn add_check(mut self, check: Box<dyn AuditCheck + Send + Sync>) -> Self {
        self.checks.push(check);
        self
    }

    /// Run all audit checks on a file
    pub fn audit_file(&self, content: &str, file_path: &str) -> Result<Vec<AuditFinding>> {
        let mut all_findings = Vec::new();

        // Parse the code once for all checks
        let analysis = RustCodeParser::parse_code(content)?;

        // Run each check
        for check in &self.checks {
            match check.check(&analysis, file_path) {
                Ok(findings) => all_findings.extend(findings),
                Err(e) => {
                    log::warn!("Check '{}' failed for file {}: {}", check.name(), file_path, e);
                    // Continue with other checks even if one fails
                }
            }
        }

        Ok(all_findings)
    }

    /// Get list of available checks
    pub fn list_checks(&self) -> Vec<(&str, &str)> {
        self.checks
            .iter()
            .map(|check| (check.name(), check.category()))
            .collect()
    }
}

impl Default for ModularAuditCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_finding_id_allocator() {
        FindingIdAllocator::reset();
        
        let id1 = FindingIdAllocator::next_id();
        let id2 = FindingIdAllocator::next_id();
        let id3 = FindingIdAllocator::next_category_id("solana");
        
        assert_eq!(id1, "OSVM-001");
        assert_eq!(id2, "OSVM-002");
        assert_eq!(id3, "OSVM-SOL-003");
    }

    #[test]
    fn test_memory_safety_check() {
        let code = r#"
            fn test() {
                unsafe {
                    let ptr = std::ptr::null_mut();
                }
                let result = Some(42).unwrap();
            }
        "#;

        let check = MemorySafetyCheck;
        let findings = check.check_content(code, "test.rs").unwrap();
        assert!(!findings.is_empty());
    }

    #[test]
    fn test_modular_coordinator() {
        let coordinator = ModularAuditCoordinator::new();
        let checks = coordinator.list_checks();
        
        assert!(checks.len() >= 5);
        assert!(checks.iter().any(|(name, _)| *name == "Memory Safety"));
        assert!(checks.iter().any(|(name, _)| *name == "Solana Security"));
    }

    #[test]
    fn test_regex_cache() {
        let pattern = REGEX_CACHE.get("password_pattern").unwrap();
        assert!(pattern.is_match(r#"password = "secret123""#));
        assert!(!pattern.is_match("not a password"));
    }
}