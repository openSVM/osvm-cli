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

/// Global finding ID allocator to ensure unique IDs across sessions
static FINDING_ID_COUNTER: AtomicUsize = AtomicUsize::new(1);

/// Session ID for this audit run based on timestamp
static SESSION_ID: Lazy<String> = Lazy::new(|| {
    use std::time::{SystemTime, UNIX_EPOCH};
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();
    format!("{:08x}", timestamp & 0xFFFFFFFF) // Use lower 32 bits for shorter IDs
});

/// Cached regex patterns for performance
static REGEX_CACHE: Lazy<HashMap<&'static str, regex::Regex>> = Lazy::new(|| {
    let mut cache = HashMap::new();
    
    // Secret patterns
    cache.insert("password_pattern", regex::Regex::new(r#"(?i)password\s*=\s*['"'][^'"']+['"']"#).unwrap());
    cache.insert("api_key_pattern", regex::Regex::new(r#"(?i)api_key\s*=\s*['"'][^'"']+['"']"#).unwrap());
    cache.insert("secret_pattern", regex::Regex::new(r#"(?i)secret\s*=\s*['"'][^'"']+['"']"#).unwrap());
    cache.insert("hex_key_pattern", regex::Regex::new(r#"['"'][0-9a-fA-F]{32,}['"']"#).unwrap());
    cache.insert("base64_pattern", regex::Regex::new(r#"['"'][A-Za-z0-9+/]{20,}={0,2}['"']"#).unwrap());
    
    // Command injection patterns - Enhanced with more comprehensive detection
    cache.insert("command_injection", regex::Regex::new(r#"Command::new\([^)]*(?:format!|user_input|param|arg)"#).unwrap());
    cache.insert("shell_command", regex::Regex::new(r#"(?:shell\(|system\(|exec\()"#).unwrap());
    cache.insert("unsafe_exec", regex::Regex::new(r#"(?:std::process::Command|tokio::process::Command).*(?:format!|user_input)"#).unwrap());
    
    // Path traversal patterns - Enhanced with context awareness
    cache.insert("path_traversal", regex::Regex::new(r#"(?:\.\./)+"#).unwrap());
    cache.insert("dynamic_path", regex::Regex::new(r#"Path::new\([^)]*(?:format!|user_input|param)"#).unwrap());
    cache.insert("unsafe_path_join", regex::Regex::new(r#"path\.join\([^)]*(?:format!|user_input)"#).unwrap());
    
    // Network patterns
    cache.insert("http_insecure", regex::Regex::new(r#"http://[^/]*\..*"#).unwrap()); // Simplified pattern for non-localhost HTTP
    cache.insert("tls_bypass", regex::Regex::new(r#"danger_accept_invalid_certs\(true\)"#).unwrap());
    
    // Solana-specific patterns - Adding missing patterns
    cache.insert("solana_signer", regex::Regex::new(r#"AccountInfo.*without.*is_signer"#).unwrap());
    cache.insert("solana_pda", regex::Regex::new(r#"find_program_address"#).unwrap());
    cache.insert("solana_owner", regex::Regex::new(r#"AccountInfo.*owner"#).unwrap());
    cache.insert("rent_exempt", regex::Regex::new(r#"(?:rent_exempt|Rent::exempt)"#).unwrap());
    cache.insert("lamports", regex::Regex::new(r#"(?:lamports|try_borrow_mut_lamports)"#).unwrap());
    cache.insert("solana_authority", regex::Regex::new(r#"(?:authority|Pubkey::default)"#).unwrap());
    cache.insert("solana_invoke", regex::Regex::new(r#"(?:invoke|invoke_signed)"#).unwrap());
    cache.insert("solana_realloc", regex::Regex::new(r#"realloc\([^)]*,\s*false\)"#).unwrap());
    
    cache
});

/// Finding ID allocator for unique ID generation
pub struct FindingIdAllocator;

impl FindingIdAllocator {
    /// Generate a unique finding ID with session context
    pub fn next_id() -> String {
        let id = FINDING_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        format!("OSVM-{}-{:03}", &*SESSION_ID, id)
    }

    /// Generate a category-specific finding ID with session context
    pub fn next_category_id(category: &str) -> String {
        let id = FINDING_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        match category {
            "solana" => format!("OSVM-{}-SOL-{:03}", &*SESSION_ID, id),
            "crypto" => format!("OSVM-{}-CRYPTO-{:03}", &*SESSION_ID, id),
            "network" => format!("OSVM-{}-NET-{:03}", &*SESSION_ID, id),
            "auth" => format!("OSVM-{}-AUTH-{:03}", &*SESSION_ID, id),
            _ => format!("OSVM-{}-{:03}", &*SESSION_ID, id),
        }
    }

    /// Generate UUID-based finding ID for maximum uniqueness
    pub fn next_uuid_id() -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        use std::time::{SystemTime, UNIX_EPOCH};
        
        let mut hasher = DefaultHasher::new();
        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos().hash(&mut hasher);
        std::thread::current().id().hash(&mut hasher);
        FINDING_ID_COUNTER.load(Ordering::SeqCst).hash(&mut hasher);
        
        let hash = hasher.finish();
        format!("OSVM-UUID-{:016x}", hash)
    }

    /// Reset counter (for testing)
    #[cfg(test)]
    pub fn reset() {
        FINDING_ID_COUNTER.store(1, Ordering::SeqCst);
    }

    /// Get current session ID
    pub fn get_session_id() -> &'static str {
        &*SESSION_ID
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
            // Missing signer validation
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
                    code_location: Some(format!("{}:{}", file_path, solana_op.line)),
                    references: vec![
                        "https://book.anchor-lang.com/anchor_bts/security.html".to_string(),
                        "https://solana-labs.github.io/solana-program-library/anchor/lang/macro.Program.html".to_string(),
                    ],
                });
            }

            // Missing account owner validation
            if !solana_op.owner_check && solana_op.operation_type.contains("account") {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("solana"),
                    title: "Missing account owner validation".to_string(),
                    description: format!(
                        "File {} contains account operation without owner validation at line {}",
                        file_path, solana_op.line
                    ),
                    severity: AuditSeverity::High,
                    category: "Solana Security".to_string(),
                    cwe_id: Some("CWE-284".to_string()),
                    cvss_score: Some(7.5),
                    impact: "Programs could operate on accounts owned by malicious programs".to_string(),
                    recommendation: "Always verify account ownership before performing operations: account.owner == expected_program_id".to_string(),
                    code_location: Some(format!("{}:{}", file_path, solana_op.line)),
                    references: vec![
                        "https://book.anchor-lang.com/anchor_bts/security.html".to_string(),
                    ],
                });
            }

            // Missing program ID validation for CPI calls
            if !solana_op.program_id_check && solana_op.operation_type.contains("invoke") {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("solana"),
                    title: "Missing program ID validation before CPI".to_string(),
                    description: format!(
                        "File {} contains Cross-Program Invocation without program ID validation at line {}",
                        file_path, solana_op.line
                    ),
                    severity: AuditSeverity::Critical,
                    category: "Solana Security".to_string(),
                    cwe_id: Some("CWE-20".to_string()),
                    cvss_score: Some(9.0),
                    impact: "Arbitrary program execution vulnerability - attacker can invoke malicious programs".to_string(),
                    recommendation: "Always validate the program ID before making cross-program invocations".to_string(),
                    code_location: Some(format!("{}:{}", file_path, solana_op.line)),
                    references: vec![
                        "https://docs.solana.com/developing/programming-model/calling-between-programs".to_string(),
                        "https://github.com/coral-xyz/sealevel-attacks/tree/master/programs/0-arbitrary-cpi".to_string(),
                    ],
                });
            }

            // Weak PDA seed uniqueness
            if !solana_op.pda_seeds.is_empty() && solana_op.pda_seeds.len() < 2 {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("solana"),
                    title: "Insufficient PDA seed uniqueness".to_string(),
                    description: format!(
                        "File {} uses PDA with insufficient seed uniqueness ({} seeds) at line {}",
                        file_path, solana_op.pda_seeds.len(), solana_op.line
                    ),
                    severity: AuditSeverity::High,
                    category: "Solana Security".to_string(),
                    cwe_id: Some("CWE-330".to_string()),
                    cvss_score: Some(7.0),
                    impact: "PDA collision attacks possible, unauthorized access to program accounts".to_string(),
                    recommendation: "Use multiple unique seeds for PDA creation including user-specific identifiers".to_string(),
                    code_location: Some(format!("{}:{}", file_path, solana_op.line)),
                    references: vec![
                        "https://book.anchor-lang.com/anchor_bts/security.html".to_string(),
                        "https://github.com/coral-xyz/sealevel-attacks/tree/master/programs/3-pda-sharing".to_string(),
                    ],
                });
            }

            // Missing account data validation
            if !solana_op.account_data_validation && solana_op.operation_type.contains("AccountInfo") {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("solana"),
                    title: "Missing account data validation".to_string(),
                    description: format!(
                        "File {} accesses account data without proper validation at line {}",
                        file_path, solana_op.line
                    ),
                    severity: AuditSeverity::Medium,
                    category: "Solana Security".to_string(),
                    cwe_id: Some("CWE-20".to_string()),
                    cvss_score: Some(6.0),
                    impact: "Account data confusion vulnerabilities, type safety bypasses".to_string(),
                    recommendation: "Always deserialize and validate account data before use, check discriminators".to_string(),
                    code_location: Some(format!("{}:{}", file_path, solana_op.line)),
                    references: vec![
                        "https://book.anchor-lang.com/anchor_bts/security.html".to_string(),
                    ],
                });
            }
        }

        // Additional Solana-specific pattern checks using cached regexes
        self.check_solana_patterns(analysis, file_path, &mut findings);
        
        // Enhanced Solana-specific security checks
        self.check_mev_protection(analysis, file_path, &mut findings);
        self.check_authority_transfer_patterns(analysis, file_path, &mut findings);
        self.check_duplicate_mutable_accounts(analysis, file_path, &mut findings);
        self.check_precision_arithmetic(analysis, file_path, &mut findings);
        self.check_atomic_validations(analysis, file_path, &mut findings);

        Ok(findings)
    }
}

impl SolanaSecurityCheck {
    /// Check if a string is a valid base58 encoded Solana public key
    fn is_valid_base58_pubkey(value: &str) -> bool {
        // Solana public keys are 32 bytes, which when base58 encoded are typically 43-44 characters
        if value.len() < 32 || value.len() > 44 {
            return false;
        }
        
        // Check if it contains only valid base58 characters
        let base58_chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
        if !value.chars().all(|c| base58_chars.contains(c)) {
            return false;
        }
        
        // Try to decode as base58 and check if it's 32 bytes (Solana pubkey size)
        match bs58::decode(value).into_vec() {
            Ok(decoded) => decoded.len() == 32,
            Err(_) => false,
        }
    }

    /// Check for additional Solana security patterns using regex cache
    fn check_solana_patterns(&self, analysis: &ParsedCodeAnalysis, file_path: &str, findings: &mut Vec<AuditFinding>) {
        // Check for hardcoded program IDs or keys in string literals
        for string_lit in &analysis.string_literals {
            // Check for potential base58 encoded Solana public keys
            if Self::is_valid_base58_pubkey(&string_lit.value) {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("solana"),
                    title: "Potential hardcoded Solana public key".to_string(),
                    description: format!(
                        "File {} contains what appears to be a hardcoded base58-encoded public key at line {}",
                        file_path, string_lit.line
                    ),
                    severity: AuditSeverity::Medium,
                    category: "Solana Security".to_string(),
                    cwe_id: Some("CWE-798".to_string()),
                    cvss_score: Some(5.0),
                    impact: "Hardcoded keys reduce flexibility and may expose sensitive information".to_string(),
                    recommendation: "Use environment variables or configuration for public keys, or use the Pubkey::from_str() function with constants".to_string(),
                    code_location: Some(format!("{}:{}", file_path, string_lit.line)),
                    references: vec![
                        "https://docs.solana.com/developing/programming-model/accounts".to_string(),
                        "https://docs.rs/solana-sdk/latest/solana_sdk/pubkey/struct.Pubkey.html".to_string(),
                    ],
                });
            }
        }

        // Check for potential rent exemption bypass
        let code_patterns = [
            ("rent_exempt", "Missing rent exemption check"),
            ("lamports", "Potential lamports manipulation"),
            ("close_account", "Account closure without proper validation"),
        ];

        for (pattern, issue) in code_patterns.iter() {
            if let Some(regex) = REGEX_CACHE.get(pattern) {
                // In a real implementation, we'd scan the actual file content here
                // For now, we'll check if any operations contain these patterns
                let has_pattern = analysis.solana_operations.iter()
                    .any(|op| op.operation_type.contains(pattern));
                
                if has_pattern {
                    findings.push(AuditFinding {
                        id: FindingIdAllocator::next_category_id("solana"),
                        title: issue.to_string(),
                        description: format!("File {} contains Solana operations related to {}", file_path, pattern),
                        severity: AuditSeverity::Medium,
                        category: "Solana Security".to_string(),
                        cwe_id: Some("CWE-20".to_string()),
                        cvss_score: Some(5.5),
                        impact: "Potential Solana-specific security vulnerability".to_string(),
                        recommendation: format!("Review {} operations for proper validation", pattern),
                        code_location: Some(file_path.to_string()),
                        references: vec![
                            "https://book.anchor-lang.com/anchor_bts/security.html".to_string(),
                        ],
                    });
                }
            }
        }
    }

    /// Check for MEV protection patterns
    fn check_mev_protection(&self, analysis: &ParsedCodeAnalysis, file_path: &str, findings: &mut Vec<AuditFinding>) {
        // Look for price/slippage protection patterns
        let mev_indicators = [
            ("slippage", "Missing slippage protection"),
            ("deadline", "Missing deadline protection"),
            ("oracle", "Oracle price manipulation risk"),
            ("swap", "Unprotected swap operation"),
        ];

        for (pattern, issue) in mev_indicators.iter() {
            let has_pattern = analysis.solana_operations.iter()
                .any(|op| op.operation_type.to_lowercase().contains(pattern));
            
            // Check for missing protection patterns
            let has_protection = analysis.string_literals.iter()
                .any(|lit| lit.value.to_lowercase().contains(&format!("{}_protection", pattern)));
            
            if has_pattern && !has_protection {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("solana"),
                    title: format!("MEV Risk: {}", issue),
                    description: format!(
                        "File {} contains {} operations without proper protection mechanisms",
                        file_path, pattern
                    ),
                    severity: AuditSeverity::High,
                    category: "Solana MEV Protection".to_string(),
                    cwe_id: Some("CWE-367".to_string()),
                    cvss_score: Some(7.5),
                    impact: format!("Potential {} manipulation and MEV attacks", pattern),
                    recommendation: format!("Implement {} protection with deadline checks and oracle validation", pattern),
                    code_location: Some(file_path.to_string()),
                    references: vec![
                        "https://docs.solana.com/developing/programming-model/transactions#atomic-transaction-processing".to_string(),
                        "https://book.anchor-lang.com/anchor_bts/security.html#mev-protection".to_string(),
                    ],
                });
            }
        }
    }

    /// Check for proper authority transfer patterns
    fn check_authority_transfer_patterns(&self, analysis: &ParsedCodeAnalysis, file_path: &str, findings: &mut Vec<AuditFinding>) {
        let has_authority_change = analysis.solana_operations.iter()
            .any(|op| op.operation_type.contains("authority") || op.operation_type.contains("owner"));
        
        if has_authority_change {
            // Check for two-step authority transfer pattern
            let has_two_step = analysis.string_literals.iter()
                .any(|lit| lit.value.contains("pending_authority") || lit.value.contains("accept_authority"));
            
            if !has_two_step {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("solana"),
                    title: "Unsafe authority transfer pattern".to_string(),
                    description: format!(
                        "File {} contains authority transfer operations without two-step verification",
                        file_path
                    ),
                    severity: AuditSeverity::High,
                    category: "Solana Authority Management".to_string(),
                    cwe_id: Some("CWE-269".to_string()),
                    cvss_score: Some(8.0),
                    impact: "Authority could be transferred to incorrect or malicious addresses".to_string(),
                    recommendation: "Implement two-step authority transfer with pending/accept pattern and proper validation".to_string(),
                    code_location: Some(file_path.to_string()),
                    references: vec![
                        "https://book.anchor-lang.com/anchor_bts/security.html#authority-transfer".to_string(),
                        "https://docs.solana.com/developing/programming-model/accounts#ownership".to_string(),
                    ],
                });
            }
        }
    }

    /// Check for duplicate mutable account patterns
    fn check_duplicate_mutable_accounts(&self, analysis: &ParsedCodeAnalysis, file_path: &str, findings: &mut Vec<AuditFinding>) {
        // Look for potential duplicate mutable account usage
        let has_mutable_accounts = analysis.solana_operations.iter()
            .any(|op| op.operation_type.contains("mutable") || op.operation_type.contains("mut"));
        
        if has_mutable_accounts {
            // Check for account deduplication logic
            let has_dedup_check = analysis.string_literals.iter()
                .any(|lit| lit.value.contains("duplicate") || lit.value.contains("unique"));
            
            if !has_dedup_check {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("solana"),
                    title: "Potential duplicate mutable accounts vulnerability".to_string(),
                    description: format!(
                        "File {} contains mutable account operations without duplicate checking",
                        file_path
                    ),
                    severity: AuditSeverity::Medium,
                    category: "Solana Account Management".to_string(),
                    cwe_id: Some("CWE-694".to_string()),
                    cvss_score: Some(6.5),
                    impact: "Same account could be used multiple times in different roles, leading to unexpected behavior".to_string(),
                    recommendation: "Implement account deduplication checks before processing mutable accounts".to_string(),
                    code_location: Some(file_path.to_string()),
                    references: vec![
                        "https://book.anchor-lang.com/anchor_bts/security.html#duplicate-accounts".to_string(),
                    ],
                });
            }
        }
    }

    /// Check for precision arithmetic issues
    fn check_precision_arithmetic(&self, analysis: &ParsedCodeAnalysis, file_path: &str, findings: &mut Vec<AuditFinding>) {
        // Look for floating-point arithmetic in financial calculations
        let has_float_ops = analysis.binary_operations.iter()
            .any(|op| op.operation_type.contains("f32") || op.operation_type.contains("f64"));
        
        if has_float_ops {
            findings.push(AuditFinding {
                id: FindingIdAllocator::next_category_id("solana"),
                title: "Floating-point arithmetic in financial calculations".to_string(),
                description: format!(
                    "File {} uses floating-point arithmetic which can cause precision loss in financial operations",
                    file_path
                ),
                severity: AuditSeverity::High,
                category: "Solana Financial Math".to_string(),
                cwe_id: Some("CWE-682".to_string()),
                cvss_score: Some(7.5),
                impact: "Precision loss in financial calculations can lead to incorrect token amounts".to_string(),
                recommendation: "Use fixed-point arithmetic or integer-based calculations for financial operations".to_string(),
                code_location: Some(file_path.to_string()),
                references: vec![
                    "https://book.anchor-lang.com/anchor_bts/security.html#numerical-precision".to_string(),
                ],
            });
        }

        // Check for division before multiplication (precision loss)
        let has_div_before_mul = analysis.binary_operations.iter()
            .zip(analysis.binary_operations.iter().skip(1))
            .any(|(op1, op2)| {
                op1.operation_type == "/" && op2.operation_type == "*" && 
                op2.line == op1.line + 1 // Adjacent operations
            });
        
        if has_div_before_mul {
            findings.push(AuditFinding {
                id: FindingIdAllocator::next_category_id("solana"),
                title: "Division before multiplication causing precision loss".to_string(),
                description: format!(
                    "File {} performs division before multiplication, which can cause precision loss",
                    file_path
                ),
                severity: AuditSeverity::Medium,
                category: "Solana Financial Math".to_string(),
                cwe_id: Some("CWE-682".to_string()),
                cvss_score: Some(5.5),
                impact: "Mathematical operations may lose precision, affecting financial calculations".to_string(),
                recommendation: "Reorder operations to multiply before dividing, or use higher precision arithmetic".to_string(),
                code_location: Some(file_path.to_string()),
                references: vec![
                    "https://book.anchor-lang.com/anchor_bts/security.html#numerical-precision".to_string(),
                ],
            });
        }
    }

    /// Check for atomic validation patterns and race conditions
    fn check_atomic_validations(&self, analysis: &ParsedCodeAnalysis, file_path: &str, findings: &mut Vec<AuditFinding>) {
        // Look for account validation patterns
        let has_validation = analysis.solana_operations.iter()
            .any(|op| op.operation_type.contains("validate") || op.operation_type.contains("check"));
        
        // Look for account reload after CPI
        let has_cpi = analysis.solana_operations.iter()
            .any(|op| op.operation_type.contains("cpi") || op.operation_type.contains("invoke"));
        
        let has_reload = analysis.solana_operations.iter()
            .any(|op| op.operation_type.contains("reload"));
        
        if has_cpi && !has_reload {
            findings.push(AuditFinding {
                id: FindingIdAllocator::next_category_id("solana"),
                title: "Missing account reload after CPI".to_string(),
                description: format!(
                    "File {} contains CPI operations without subsequent account reloading",
                    file_path
                ),
                severity: AuditSeverity::High,
                category: "Solana CPI Safety".to_string(),
                cwe_id: Some("CWE-362".to_string()),
                cvss_score: Some(7.0),
                impact: "Account data may be stale after CPI, leading to incorrect program behavior and potential race conditions".to_string(),
                recommendation: "Always reload account data after CPI operations to ensure data consistency and prevent race conditions. Use account.reload() or fetch fresh account data.".to_string(),
                code_location: Some(file_path.to_string()),
                references: vec![
                    "https://book.anchor-lang.com/anchor_bts/security.html#account-reloading".to_string(),
                    "https://docs.solana.com/developing/programming-model/calling-between-programs#reentrancy".to_string(),
                ],
            });
        }

        // Check for atomic validation patterns
        if has_validation {
            // Look for time-of-check-time-of-use patterns
            let has_toctou_risk = analysis.binary_operations.iter()
                .any(|op| op.operation_type.contains("==") || op.operation_type.contains("!="));
            
            if has_toctou_risk {
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_category_id("solana"),
                    title: "Potential TOCTOU race condition in account validation".to_string(),
                    description: format!(
                        "File {} contains account validation that may be susceptible to time-of-check-time-of-use attacks",
                        file_path
                    ),
                    severity: AuditSeverity::Medium,
                    category: "Solana Race Conditions".to_string(),
                    cwe_id: Some("CWE-367".to_string()),
                    cvss_score: Some(6.0),
                    impact: "Account state could change between validation and use, leading to security vulnerabilities".to_string(),
                    recommendation: "Ensure account validations are atomic and cannot be bypassed by concurrent operations. Consider using locks or ensuring operations are performed within the same transaction context.".to_string(),
                    code_location: Some(file_path.to_string()),
                    references: vec![
                        "https://book.anchor-lang.com/anchor_bts/security.html#atomic-operations".to_string(),
                        "https://en.wikipedia.org/wiki/Time-of-check_to_time-of-use".to_string(),
                    ],
                });
            }
        }
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

        // Enhanced command injection vulnerability checks with context awareness
        for cmd_exec in &analysis.command_executions {
            if cmd_exec.is_dynamic {
                // Check for sanitization patterns
                let has_sanitization = analysis.string_literals.iter()
                    .any(|lit| lit.value.contains("sanitize") || lit.value.contains("validate") || lit.value.contains("escape"));
                
                // Check for safe command execution patterns
                let has_safe_patterns = analysis.string_literals.iter()
                    .any(|lit| lit.value.contains("shellwords") || lit.value.contains("shlex") || lit.value.contains("quote"));
                
                let severity = if has_sanitization || has_safe_patterns {
                    AuditSeverity::Medium // Lower severity if mitigation patterns detected
                } else {
                    AuditSeverity::High // High severity for unmitigated dynamic commands
                };
                
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_id(),
                    title: "Potential command injection vulnerability".to_string(),
                    description: format!(
                        "File {} contains command execution with potentially unsafe input at line {}{}",
                        file_path, 
                        cmd_exec.line,
                        if has_sanitization || has_safe_patterns { " (mitigation patterns detected)" } else { " (no mitigation detected)" }
                    ),
                    severity,
                    category: "Input Validation".to_string(),
                    cwe_id: Some("CWE-78".to_string()),
                    cvss_score: Some(if has_sanitization || has_safe_patterns { 5.5 } else { 7.5 }),
                    impact: "Arbitrary command execution on the host system".to_string(),
                    recommendation: if has_sanitization || has_safe_patterns {
                        "Review current sanitization logic to ensure it's comprehensive. Consider using allowlists instead of blocklists.".to_string()
                    } else {
                        "Validate and sanitize all input before using in commands, use parameterized commands, or consider using safe command execution libraries.".to_string()
                    },
                    code_location: Some(format!("{}:{}", file_path, cmd_exec.line)),
                    references: vec![
                        "https://cwe.mitre.org/data/definitions/78.html".to_string(),
                        "https://owasp.org/Top10/A03_2021-Injection/".to_string(),
                        "https://docs.rs/shellwords/latest/shellwords/".to_string(),
                    ],
                });
            }
        }

        // Enhanced path traversal checks with context awareness
        for path_op in &analysis.path_operations {
            if path_op.is_dynamic {
                // Check for path sanitization patterns
                let has_path_sanitization = analysis.string_literals.iter()
                    .any(|lit| {
                        lit.value.contains("canonicalize") || 
                        lit.value.contains("Path::normalize") ||
                        lit.value.contains("path_clean") ||
                        lit.value.contains("resolve")
                    });
                
                // Check for path validation patterns
                let has_path_validation = analysis.binary_operations.iter()
                    .any(|op| {
                        op.line >= path_op.line.saturating_sub(5) && 
                        op.line <= path_op.line + 5 &&
                        (op.operation_type.contains("starts_with") || op.operation_type.contains("contains"))
                    });
                
                let severity = if has_path_sanitization || has_path_validation {
                    AuditSeverity::Low
                } else {
                    AuditSeverity::High
                };
                
                findings.push(AuditFinding {
                    id: FindingIdAllocator::next_id(),
                    title: "Potential path traversal vulnerability".to_string(),
                    description: format!(
                        "File {} contains file operations with potentially unsafe paths at line {}{}",
                        file_path,
                        path_op.line,
                        if has_path_sanitization || has_path_validation { " (validation patterns detected)" } else { " (no validation detected)" }
                    ),
                    severity,
                    category: "Input Validation".to_string(),
                    cwe_id: Some("CWE-22".to_string()),
                    cvss_score: Some(if has_path_sanitization || has_path_validation { 3.5 } else { 7.0 }),
                    impact: "Unauthorized access to files outside intended directory".to_string(),
                    recommendation: if has_path_sanitization || has_path_validation {
                        "Review path validation logic to ensure it prevents all traversal attempts including encoded sequences.".to_string()
                    } else {
                        "Validate and canonicalize file paths, use safe path construction methods, and implement proper bounds checking.".to_string()
                    },
                    code_location: Some(format!("{}:{}", file_path, path_op.line)),
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
    fn test_base58_validation() {
        // Test valid base58 Solana public keys
        assert!(SolanaSecurityCheck::is_valid_base58_pubkey("TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"));
        assert!(SolanaSecurityCheck::is_valid_base58_pubkey("11111111111111111111111111111112"));
        
        // Test invalid base58 (contains forbidden characters)
        assert!(!SolanaSecurityCheck::is_valid_base58_pubkey("1111111111111111111111111111111O")); // Contains 'O'
        assert!(!SolanaSecurityCheck::is_valid_base58_pubkey("1111111111111111111111111111111I")); // Contains 'I'
        assert!(!SolanaSecurityCheck::is_valid_base58_pubkey("111111111111111111111111111111l0")); // Contains 'l' and '0'
        
        // Test base64 strings (should not be detected as base58)
        assert!(!SolanaSecurityCheck::is_valid_base58_pubkey("SGVsbG8gV29ybGQ=")); // base64
        
        // Test wrong length
        assert!(!SolanaSecurityCheck::is_valid_base58_pubkey("123")); // Too short
        assert!(!SolanaSecurityCheck::is_valid_base58_pubkey("1".repeat(100).as_str())); // Too long
    }

    #[test] 
    fn test_hardcoded_key_detection() {
        let code_with_hardcoded_key = r#"
            const PROGRAM_ID: &str = "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA";
            const SYSTEM_PROGRAM: &str = "11111111111111111111111111111112";
        "#;

        let code_with_base64 = r#"
            const NOT_A_KEY: &str = "SGVsbG8gV29ybGQ="; // base64
            const INVALID_BASE58: &str = "1111111111111111111111111111111O"; // Contains 'O'
        "#;

        let check = SolanaSecurityCheck;
        
        let findings_hardcoded = check.check_content(code_with_hardcoded_key, "test.rs").unwrap();
        let hardcoded_findings: Vec<_> = findings_hardcoded.iter()
            .filter(|f| f.title.contains("hardcoded Solana public key"))
            .collect();
        assert_eq!(hardcoded_findings.len(), 2, "Should detect 2 hardcoded Solana keys");

        let findings_base64 = check.check_content(code_with_base64, "test.rs").unwrap();
        let base64_findings: Vec<_> = findings_base64.iter()
            .filter(|f| f.title.contains("hardcoded Solana public key"))
            .collect();
        assert_eq!(base64_findings.len(), 0, "Should not detect base64 or invalid base58 as Solana keys");
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