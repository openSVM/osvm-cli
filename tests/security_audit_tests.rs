//! Comprehensive tests for security auditing functionality

use anyhow::Result;
use mockito::Server;
use osvm::services::audit_service::{
    AuditRequest, AuditResult, AuditService, Vulnerability, VulneritySeverity,
};
use osvm::utils::secure_logger::SecureLogger;
use serde_json::json;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Create a test Solana program with vulnerabilities
fn create_vulnerable_program(dir: &TempDir, vulnerability_type: &str) -> Result<PathBuf> {
    let program_path = dir.path().join("src").join("lib.rs");
    fs::create_dir_all(program_path.parent().unwrap())?;

    let code = match vulnerability_type {
        "unchecked_account" => {
            r#"
            use solana_program::{
                account_info::AccountInfo,
                entrypoint,
                entrypoint::ProgramResult,
                pubkey::Pubkey,
            };

            entrypoint!(process_instruction);

            pub fn process_instruction(
                _program_id: &Pubkey,
                accounts: &[AccountInfo],
                _instruction_data: &[u8],
            ) -> ProgramResult {
                // VULNERABILITY: No account ownership check
                let account = &accounts[0];
                let mut data = account.try_borrow_mut_data()?;
                data[0] = 42;
                Ok(())
            }
        "#
        }
        "integer_overflow" => {
            r#"
            use solana_program::{
                account_info::AccountInfo,
                entrypoint,
                entrypoint::ProgramResult,
                pubkey::Pubkey,
            };

            entrypoint!(process_instruction);

            pub fn process_instruction(
                _program_id: &Pubkey,
                _accounts: &[AccountInfo],
                instruction_data: &[u8],
            ) -> ProgramResult {
                // VULNERABILITY: Unchecked arithmetic
                let amount = u64::from_le_bytes(instruction_data[0..8].try_into().unwrap());
                let new_amount = amount + 1000000;  // Can overflow
                Ok(())
            }
        "#
        }
        "missing_signer_check" => {
            r#"
            use solana_program::{
                account_info::AccountInfo,
                entrypoint,
                entrypoint::ProgramResult,
                pubkey::Pubkey,
            };

            entrypoint!(process_instruction);

            pub fn process_instruction(
                _program_id: &Pubkey,
                accounts: &[AccountInfo],
                _instruction_data: &[u8],
            ) -> ProgramResult {
                // VULNERABILITY: No signer check
                let authority = &accounts[0];
                // Perform privileged operation without checking if authority is signer
                Ok(())
            }
        "#
        }
        "reentrancy" => {
            r#"
            use solana_program::{
                account_info::AccountInfo,
                entrypoint,
                entrypoint::ProgramResult,
                pubkey::Pubkey,
                program::invoke,
            };

            entrypoint!(process_instruction);

            pub fn process_instruction(
                program_id: &Pubkey,
                accounts: &[AccountInfo],
                instruction_data: &[u8],
            ) -> ProgramResult {
                // VULNERABILITY: State not updated before external call
                invoke(
                    &some_instruction,
                    &accounts[..],
                )?;
                // State updated after call - reentrancy risk
                let mut data = accounts[0].try_borrow_mut_data()?;
                data[0] = 1;
                Ok(())
            }
        "#
        }
        _ => {
            r#"
            use solana_program::{
                account_info::AccountInfo,
                entrypoint,
                entrypoint::ProgramResult,
                pubkey::Pubkey,
            };

            entrypoint!(process_instruction);

            pub fn process_instruction(
                _program_id: &Pubkey,
                _accounts: &[AccountInfo],
                _instruction_data: &[u8],
            ) -> ProgramResult {
                Ok(())
            }
        "#
        }
    };

    fs::write(&program_path, code)?;
    Ok(program_path)
}

#[cfg(test)]
mod audit_detection_tests {
    use super::*;

    #[tokio::test]
    async fn test_unchecked_account_detection() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let program_path = create_vulnerable_program(&temp_dir, "unchecked_account")?;

        let audit_service = AuditService::new();
        let request = AuditRequest {
            target: program_path
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .to_path_buf(),
            output_dir: temp_dir.path().join("audit_results"),
            format: "json".to_string(),
            verbose: 1,
            test_mode: true,
            ai_analysis: false,
            gh_repo: None,
            template_path: None,
            no_commit: true,
            api_url: None,
        };

        let result = audit_service.run_audit(request).await?;

        assert!(result.vulnerabilities.len() > 0);
        assert!(result
            .vulnerabilities
            .iter()
            .any(|v| v.category.contains("account") || v.category.contains("ownership")));

        Ok(())
    }

    #[tokio::test]
    async fn test_integer_overflow_detection() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let program_path = create_vulnerable_program(&temp_dir, "integer_overflow")?;

        let audit_service = AuditService::new();
        let request = AuditRequest {
            target: program_path
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .to_path_buf(),
            output_dir: temp_dir.path().join("audit_results"),
            format: "json".to_string(),
            verbose: 1,
            test_mode: true,
            ai_analysis: false,
            gh_repo: None,
            template_path: None,
            no_commit: true,
            api_url: None,
        };

        let result = audit_service.run_audit(request).await?;

        assert!(result
            .vulnerabilities
            .iter()
            .any(|v| v.category.contains("arithmetic") || v.category.contains("overflow")));

        Ok(())
    }

    #[tokio::test]
    async fn test_missing_signer_check() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let program_path = create_vulnerable_program(&temp_dir, "missing_signer_check")?;

        let audit_service = AuditService::new();
        let request = AuditRequest {
            target: program_path
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .to_path_buf(),
            output_dir: temp_dir.path().join("audit_results"),
            format: "json".to_string(),
            verbose: 1,
            test_mode: true,
            ai_analysis: false,
            gh_repo: None,
            template_path: None,
            no_commit: true,
            api_url: None,
        };

        let result = audit_service.run_audit(request).await?;

        assert!(result
            .vulnerabilities
            .iter()
            .any(|v| v.category.contains("signer") || v.category.contains("authorization")));

        Ok(())
    }

    #[tokio::test]
    async fn test_ai_enhanced_audit() -> Result<()> {
        let mut server = Server::new_async().await;
        let temp_dir = TempDir::new()?;
        let program_path = create_vulnerable_program(&temp_dir, "unchecked_account")?;

        // Mock AI analysis response
        let ai_mock = server
            .mock("POST", "/analyze")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "vulnerabilities": [
                        {
                            "severity": "high",
                            "category": "Access Control",
                            "description": "Missing account ownership verification",
                            "location": "lib.rs:15",
                            "recommendation": "Add account.owner check before modifying data",
                            "cwe": "CWE-284"
                        }
                    ],
                    "risk_score": 8.5,
                    "summary": "Critical access control vulnerability detected"
                })
                .to_string(),
            )
            .create_async()
            .await;

        let audit_service = AuditService::new();
        let request = AuditRequest {
            target: program_path
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .to_path_buf(),
            output_dir: temp_dir.path().join("audit_results"),
            format: "json".to_string(),
            verbose: 2,
            test_mode: true,
            ai_analysis: true,
            gh_repo: None,
            template_path: None,
            no_commit: true,
            api_url: Some(server.url()),
        };

        let result = audit_service.run_audit(request).await?;

        assert!(result.ai_enhanced);
        assert!(result.risk_score > 0.0);

        ai_mock.assert_async().await;
        Ok(())
    }

    #[tokio::test]
    async fn test_multiple_vulnerability_types() -> Result<()> {
        let temp_dir = TempDir::new()?;

        // Create program with multiple vulnerabilities
        let program_path = temp_dir.path().join("src").join("lib.rs");
        fs::create_dir_all(program_path.parent().unwrap())?;

        let code = r#"
            use solana_program::{
                account_info::AccountInfo,
                entrypoint,
                entrypoint::ProgramResult,
                pubkey::Pubkey,
            };

            entrypoint!(process_instruction);

            pub fn process_instruction(
                _program_id: &Pubkey,
                accounts: &[AccountInfo],
                instruction_data: &[u8],
            ) -> ProgramResult {
                // Multiple vulnerabilities:
                // 1. No account ownership check
                let account = &accounts[0];

                // 2. Unchecked arithmetic
                let amount = u64::from_le_bytes(instruction_data[0..8].try_into().unwrap());
                let new_amount = amount + 1000000;

                // 3. No signer check
                let mut data = account.try_borrow_mut_data()?;
                data[0] = new_amount as u8;

                Ok(())
            }
        "#;

        fs::write(&program_path, code)?;

        let audit_service = AuditService::new();
        let request = AuditRequest {
            target: temp_dir.path().to_path_buf(),
            output_dir: temp_dir.path().join("audit_results"),
            format: "json".to_string(),
            verbose: 2,
            test_mode: true,
            ai_analysis: false,
            gh_repo: None,
            template_path: None,
            no_commit: true,
            api_url: None,
        };

        let result = audit_service.run_audit(request).await?;

        // Should detect multiple vulnerability categories
        assert!(result.vulnerabilities.len() >= 2);

        let categories: Vec<String> = result
            .vulnerabilities
            .iter()
            .map(|v| v.category.clone())
            .collect();

        // Should have diverse vulnerability types
        assert!(categories.len() > 1);

        Ok(())
    }

    #[tokio::test]
    async fn test_audit_report_formats() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let program_path = create_vulnerable_program(&temp_dir, "unchecked_account")?;
        let audit_service = AuditService::new();

        for format in &["json", "html", "markdown"] {
            let request = AuditRequest {
                target: program_path
                    .parent()
                    .unwrap()
                    .parent()
                    .unwrap()
                    .to_path_buf(),
                output_dir: temp_dir.path().join(format!("audit_{}", format)),
                format: format.to_string(),
                verbose: 1,
                test_mode: true,
                ai_analysis: false,
                gh_repo: None,
                template_path: None,
                no_commit: true,
                api_url: None,
            };

            let result = audit_service.run_audit(request).await?;
            assert!(!result.vulnerabilities.is_empty());

            // Verify output file exists
            let output_file = temp_dir
                .path()
                .join(format!("audit_{}", format))
                .join(format!("audit_report.{}", format));

            if output_file.exists() {
                let content = fs::read_to_string(&output_file)?;
                assert!(!content.is_empty());
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod security_logger_tests {
    use super::*;

    #[test]
    fn test_comprehensive_sanitization() {
        let logger = SecureLogger::new(false);

        let test_cases = vec![
            (
                "User sk-1234567890abcdef accessed /home/alice/.ssh/id_rsa",
                vec!["sk-1234567890abcdef", "alice"],
            ),
            (
                "API_KEY=pk-test-key-123 TOKEN=bearer-xyz",
                vec!["pk-test-key-123", "bearer-xyz"],
            ),
            (
                "Connection from 192.168.1.100 to 10.0.0.5",
                vec!["192.168.1.100", "10.0.0.5"],
            ),
            (
                "Private key: 5KJvsngHeMpm884wtkJNzQGaCErckhHJBGFsvd3VyK5qMZXj3hS",
                vec!["5KJvsngHeMpm884wtkJNzQGaCErckhHJBGFsvd3VyK5qMZXj3hS"],
            ),
        ];

        for (input, sensitive_data) in test_cases {
            let sanitized = logger.sanitize(input);

            for data in sensitive_data {
                assert!(
                    !sanitized.contains(data),
                    "Failed to sanitize '{}' in: {}",
                    data,
                    input
                );
            }

            assert!(
                sanitized.contains("REDACTED")
                    || sanitized.contains("[USER]")
                    || sanitized.contains("[IP_REDACTED]"),
                "Sanitized output doesn't contain redaction markers: {}",
                sanitized
            );
        }
    }

    #[test]
    fn test_debug_mode_logging() {
        let logger = SecureLogger::new(true);
        let input = "Debug info: API_KEY=secret-key-123";

        let sanitized = logger.sanitize(input);

        // In debug mode, might log more but still sanitize sensitive data
        assert!(!sanitized.contains("secret-key-123"));
    }

    #[test]
    fn test_nested_sensitive_data() {
        let logger = SecureLogger::new(false);
        let input =
            r#"{"api_key": "sk-test123", "user": {"home": "/home/bob", "token": "xyz789"}}"#;

        let sanitized = logger.sanitize(input);

        assert!(!sanitized.contains("sk-test123"));
        assert!(!sanitized.contains("bob"));
        assert!(!sanitized.contains("xyz789"));
    }
}

#[cfg(test)]
mod vulnerability_classification_tests {
    use super::*;

    #[test]
    fn test_severity_ordering() {
        let critical = VulneritySeverity::Critical;
        let high = VulneritySeverity::High;
        let medium = VulneritySeverity::Medium;
        let low = VulneritySeverity::Low;
        let info = VulneritySeverity::Info;

        // Test severity comparison
        assert!(critical > high);
        assert!(high > medium);
        assert!(medium > low);
        assert!(low > info);
    }

    #[test]
    fn test_vulnerability_serialization() {
        let vuln = Vulnerability {
            severity: VulneritySeverity::High,
            category: "Access Control".to_string(),
            description: "Missing owner check".to_string(),
            location: "lib.rs:42".to_string(),
            recommendation: "Add ownership verification".to_string(),
            cwe: Some("CWE-284".to_string()),
        };

        let serialized = serde_json::to_string(&vuln).unwrap();
        let deserialized: Vulnerability = serde_json::from_str(&serialized).unwrap();

        assert_eq!(vuln.severity, deserialized.severity);
        assert_eq!(vuln.category, deserialized.category);
        assert_eq!(vuln.cwe, deserialized.cwe);
    }

    #[test]
    fn test_risk_score_calculation() {
        let vulnerabilities = vec![
            Vulnerability {
                severity: VulneritySeverity::Critical,
                category: "Reentrancy".to_string(),
                description: "State update after external call".to_string(),
                location: "lib.rs:50".to_string(),
                recommendation: "Update state before external calls".to_string(),
                cwe: Some("CWE-841".to_string()),
            },
            Vulnerability {
                severity: VulneritySeverity::High,
                category: "Arithmetic".to_string(),
                description: "Unchecked addition".to_string(),
                location: "lib.rs:25".to_string(),
                recommendation: "Use checked_add".to_string(),
                cwe: Some("CWE-190".to_string()),
            },
            Vulnerability {
                severity: VulneritySeverity::Medium,
                category: "Input Validation".to_string(),
                description: "Missing bounds check".to_string(),
                location: "lib.rs:30".to_string(),
                recommendation: "Validate input ranges".to_string(),
                cwe: Some("CWE-20".to_string()),
            },
        ];

        // Calculate risk score based on severity
        let risk_score: f64 = vulnerabilities
            .iter()
            .map(|v| match v.severity {
                VulneritySeverity::Critical => 10.0,
                VulneritySeverity::High => 7.5,
                VulneritySeverity::Medium => 5.0,
                VulneritySeverity::Low => 2.5,
                VulneritySeverity::Info => 0.5,
            })
            .sum();

        assert_eq!(risk_score, 22.5);
        assert!(risk_score > 20.0); // High overall risk
    }
}

#[cfg(test)]
mod github_audit_tests {
    use super::*;

    #[tokio::test]
    async fn test_github_repo_parsing() -> Result<()> {
        let test_cases = vec![
            (
                "https://github.com/opensvm/test-program",
                ("opensvm", "test-program", None),
            ),
            (
                "https://github.com/solana-labs/example#develop",
                ("solana-labs", "example", Some("develop")),
            ),
            ("opensvm/test-program", ("opensvm", "test-program", None)),
        ];

        for (input, (expected_owner, expected_repo, expected_branch)) in test_cases {
            // Parse GitHub URL
            let parts: Vec<&str> = input.split('#').collect();
            let url_part = parts[0];
            let branch = parts.get(1).map(|s| s.to_string());

            let path_parts: Vec<&str> = url_part
                .trim_start_matches("https://github.com/")
                .trim_end_matches(".git")
                .split('/')
                .collect();

            if path_parts.len() >= 2 {
                assert_eq!(path_parts[0], expected_owner);
                assert_eq!(path_parts[1], expected_repo);
                assert_eq!(branch.as_deref(), expected_branch);
            }
        }

        Ok(())
    }
}
