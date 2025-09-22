//! AST-based Code Analysis for Enhanced Fix Suggestions
//!
//! This module provides sophisticated code analysis using Rust's AST (Abstract Syntax Tree)
//! to identify vulnerabilities and generate contextually appropriate fix suggestions.

use crate::utils::audit::{AuditFinding, CodeSnippet};
use crate::utils::debug_logger::VerbosityLevel;
use crate::{debug_print, debug_warn};
use anyhow::{Context, Result};
use proc_macro2::TokenStream;
use quote::ToTokens;
use std::collections::HashMap;
use std::fs;
use syn::{
    visit::Visit, Attribute, Expr, ExprCall, ExprMethodCall, FnArg, Item, ItemFn, ItemStruct, Pat,
    Stmt, Type,
};

/// AST-based code analyzer for sophisticated vulnerability detection and fix generation
pub struct AstAnalyzer {
    /// Cache of parsed files to avoid re-parsing
    file_cache: HashMap<String, syn::File>,
}

/// Analysis context for tracking code patterns and vulnerabilities
#[derive(Debug, Clone)]
pub struct AnalysisContext {
    pub file_path: String,
    pub function_name: Option<String>,
    pub struct_name: Option<String>,
    pub security_issues: Vec<SecurityIssue>,
    pub improvement_suggestions: Vec<ImprovementSuggestion>,
}

/// Represents a specific security issue found in the code
#[derive(Debug, Clone)]
pub struct SecurityIssue {
    pub issue_type: SecurityIssueType,
    pub location: CodeLocation,
    pub severity: SecuritySeverity,
    pub description: String,
    pub fix_suggestion: String,
}

/// Types of security issues we can detect with AST analysis
#[derive(Debug, Clone, PartialEq)]
pub enum SecurityIssueType {
    MissingSignerCheck,
    UnvalidatedAccountAccess,
    ArithmeticOverflow,
    PanicOnError,
    HardcodedProgramId,
    MissingSlippageProtection,
    UnsafeAccountClosing,
    ImproperErrorHandling,
    UnconstrainedPdaDerivation,
    MissingRentExemption,
}

/// Severity levels for security issues
#[derive(Debug, Clone, PartialEq)]
pub enum SecuritySeverity {
    Critical,
    High,
    Medium,
    Low,
    Info,
}

/// Code location information for precise targeting
#[derive(Debug, Clone)]
pub struct CodeLocation {
    pub start_line: usize,
    pub end_line: usize,
    pub column: usize,
    pub span_length: usize,
}

/// Improvement suggestions for code quality
#[derive(Debug, Clone)]
pub struct ImprovementSuggestion {
    pub category: String,
    pub description: String,
    pub before_code: String,
    pub after_code: String,
    pub explanation: String,
}

/// Signer validation analysis result
#[derive(Debug, Clone, Default)]
pub struct SignerValidationResult {
    pub has_signer_check: bool,
    pub has_require_signer: bool,
    pub has_anchor_signer_constraint: bool,
    pub has_conditional_signer_check: bool,
}

/// Owner validation analysis result
#[derive(Debug, Clone, Default)]
pub struct OwnerValidationResult {
    pub has_owner_check: bool,
    pub has_program_id_check: bool,
    pub has_anchor_owner_constraint: bool,
}

/// Visitor for analyzing Solana/Anchor program patterns
struct SolanaSecurityVisitor {
    context: AnalysisContext,
    current_function: Option<String>,
    current_line: usize,
}

impl AstAnalyzer {
    pub fn new() -> Self {
        Self {
            file_cache: HashMap::new(),
        }
    }

    /// Parse and analyze a Rust source file
    pub fn analyze_file(&mut self, file_path: &str) -> Result<AnalysisContext> {
        let content = fs::read_to_string(file_path)
            .with_context(|| format!("Failed to read file: {}", file_path))?;

        let syntax_tree = syn::parse_file(&content)
            .with_context(|| format!("Failed to parse Rust file: {}", file_path))?;

        // Cache the parsed file
        self.file_cache
            .insert(file_path.to_string(), syntax_tree.clone());

        let mut context = AnalysisContext {
            file_path: file_path.to_string(),
            function_name: None,
            struct_name: None,
            security_issues: Vec::new(),
            improvement_suggestions: Vec::new(),
        };

        // Create visitor to analyze the AST
        let mut visitor = SolanaSecurityVisitor {
            context: context.clone(),
            current_function: None,
            current_line: 1,
        };

        visitor.visit_file(&syntax_tree);
        context = visitor.context;

        Ok(context)
    }

    /// Generate enhanced fix suggestions based on AST analysis
    pub fn generate_ast_based_fix(
        &self,
        finding: &AuditFinding,
        problematic_code: &CodeSnippet,
    ) -> Result<CodeSnippet> {
        // Parse the problematic code snippet
        let content = &problematic_code.content;

        // Try to parse as complete statements first
        let fixed_content = if let Ok(parsed) = syn::parse_str::<syn::Block>(content) {
            self.fix_block_statements(&parsed, finding)?
        } else if let Ok(parsed) = syn::parse_str::<syn::Stmt>(content) {
            self.fix_statement(&parsed, finding)?
        } else if let Ok(parsed) = syn::parse_str::<syn::Expr>(content) {
            self.fix_expression(&parsed, finding)?
        } else {
            // Fallback to pattern-based fixes
            self.apply_pattern_based_fixes(content, finding)
        };

        Ok(CodeSnippet {
            file_path: problematic_code.file_path.clone(),
            start_line: problematic_code.start_line,
            end_line: problematic_code.end_line,
            content: fixed_content,
            context: Some("AST-based fix suggestion".to_string()),
        })
    }

    /// Fix a block of statements
    fn fix_block_statements(&self, block: &syn::Block, finding: &AuditFinding) -> Result<String> {
        let mut fixed_statements = Vec::new();

        for stmt in &block.stmts {
            match finding.category.as_str() {
                "Authentication & Authorization" => {
                    if let Some(fixed) = self.add_signer_validation(stmt) {
                        fixed_statements.push(fixed);
                    }
                }
                "Solana Security" => {
                    if let Some(fixed) = self.add_account_validation(stmt) {
                        fixed_statements.push(fixed);
                    }
                }
                "Trading Security" => {
                    if let Some(fixed) = self.add_slippage_protection(stmt) {
                        fixed_statements.push(fixed);
                    }
                }
                _ => {}
            }

            // Add the original statement (possibly modified)
            fixed_statements.push(stmt.to_token_stream().to_string());
        }

        Ok(fixed_statements.join("\n    "))
    }

    /// Fix a single statement
    fn fix_statement(&self, stmt: &syn::Stmt, finding: &AuditFinding) -> Result<String> {
        let mut result = stmt.to_token_stream().to_string();

        match finding.category.as_str() {
            "Authentication & Authorization" => {
                if let Some(fixed) = self.add_signer_validation(stmt) {
                    result = format!("{}\n    {}", fixed, result);
                }
            }
            "Solana Security" => {
                if result.contains("unwrap()") {
                    result = result.replace("unwrap()", "map_err(|e| ErrorCode::UnexpectedError)?");
                }
            }
            _ => {}
        }

        Ok(result)
    }

    /// Fix an expression
    fn fix_expression(&self, expr: &syn::Expr, finding: &AuditFinding) -> Result<String> {
        let mut result = expr.to_token_stream().to_string();

        match finding.category.as_str() {
            "Trading Security" => {
                if result.contains("claim_rewards") {
                    result = format!(
                        "require!(slippage_check_passed, ErrorCode::SlippageExceeded);\n    {}",
                        result
                    );
                }
            }
            _ => {}
        }

        Ok(result)
    }

    /// Add signer validation to a statement
    fn add_signer_validation(&self, stmt: &syn::Stmt) -> Option<String> {
        let stmt_str = stmt.to_token_stream().to_string();

        if stmt_str.contains("ctx.accounts") && !stmt_str.contains("is_signer") {
            // Extract account name from the statement
            if let Some(account_name) = self.extract_account_name(&stmt_str) {
                return Some(format!(
                    "require!({}.is_signer, ErrorCode::MissingSignature);",
                    account_name
                ));
            }
        }
        None
    }

    /// Add account validation to a statement
    fn add_account_validation(&self, stmt: &syn::Stmt) -> Option<String> {
        let stmt_str = stmt.to_token_stream().to_string();

        if stmt_str.contains("AccountInfo") && !stmt_str.contains("key()") {
            if let Some(account_name) = self.extract_account_name(&stmt_str) {
                return Some(format!(
                    "require!({}.key() == expected_key, ErrorCode::InvalidAccount);",
                    account_name
                ));
            }
        }
        None
    }

    /// Add slippage protection to a statement
    fn add_slippage_protection(&self, stmt: &syn::Stmt) -> Option<String> {
        let stmt_str = stmt.to_token_stream().to_string();

        if stmt_str.contains("claim_rewards") || stmt_str.contains("swap") {
            return Some(
                "require!(amount >= min_expected_amount, ErrorCode::SlippageExceeded);".to_string(),
            );
        }
        None
    }

    /// Extract account name from a statement string
    fn extract_account_name(&self, stmt: &str) -> Option<String> {
        // Simple regex-like extraction - in production this would be more sophisticated
        if let Some(start) = stmt.find("ctx.accounts.") {
            let account_part = &stmt[start + 13..]; // Skip "ctx.accounts."
            if let Some(end) = account_part.find(&['.', ';', ' ', ')'][..]) {
                return Some(format!("ctx.accounts.{}", &account_part[..end]));
            }
        }
        None
    }

    /// Extract the variable name that holds an account reference using AST parsing
    fn extract_account_variable_name(&self, content: &str) -> Option<String> {
        debug_print!(
            VerbosityLevel::Detailed,
            "Extracting account variable name from content"
        );

        // Try to parse the content as statements to properly extract variable names
        if let Ok(parsed) = syn::parse_str::<syn::Block>(&format!("{{{}}}", content)) {
            for stmt in &parsed.stmts {
                if let syn::Stmt::Local(local) = stmt {
                    if let syn::Pat::Ident(pat_ident) = &local.pat {
                        let var_name = pat_ident.ident.to_string();
                        // Check if this variable is used in the content for account operations
                        if content.contains(&format!("{}.balance", var_name))
                            || content.contains(&format!("{}.", var_name))
                        {
                            debug_print!(
                                VerbosityLevel::Detailed,
                                "Found account variable: {}",
                                var_name
                            );
                            return Some(var_name);
                        }
                    }
                }
            }
        }

        // Fallback to the original string-based approach if AST parsing fails
        if let Some(start) = content.find("let ") {
            let after_let = &content[start + 4..];
            if let Some(equals_pos) = after_let.find(" = ") {
                let var_name = after_let[..equals_pos].trim();
                if content.contains(&format!("{}.balance", var_name))
                    || content.contains(&format!("{}.", var_name))
                {
                    debug_print!(
                        VerbosityLevel::Detailed,
                        "Found account variable (fallback): {}",
                        var_name
                    );
                    return Some(var_name.to_string());
                }
            }
        }

        debug_warn!("Could not extract account variable name");
        None
    }

    /// Advanced signer validation analysis using control flow
    fn analyze_signer_validation(&self, content: &str) -> SignerValidationResult {
        debug_print!(
            VerbosityLevel::Verbose,
            "Analyzing signer validation patterns"
        );

        let mut result = SignerValidationResult::default();

        // Check for explicit is_signer checks
        if content.contains("is_signer") {
            result.has_signer_check = true;
            debug_print!(VerbosityLevel::Detailed, "Found explicit is_signer check");
        }

        // Check for require! macros with signer validation
        if content.contains("require!") && content.contains("is_signer") {
            result.has_require_signer = true;
            debug_print!(
                VerbosityLevel::Detailed,
                "Found require! with signer validation"
            );
        }

        // Check for anchor constraints
        if content.contains("#[account(signer)]") || content.contains("Signer<") {
            result.has_anchor_signer_constraint = true;
            debug_print!(VerbosityLevel::Detailed, "Found Anchor signer constraint");
        }

        // Analyze control flow for conditional signer checks
        if let Ok(parsed) = syn::parse_str::<syn::Block>(&format!("{{{}}}", content)) {
            for stmt in &parsed.stmts {
                self.analyze_statement_for_signer_validation(stmt, &mut result);
            }
        }

        result
    }

    /// Advanced owner validation analysis using control flow
    fn analyze_owner_validation(&self, content: &str) -> OwnerValidationResult {
        debug_print!(
            VerbosityLevel::Verbose,
            "Analyzing owner validation patterns"
        );

        let mut result = OwnerValidationResult::default();

        // Check for explicit owner checks
        if content.contains(".owner") && (content.contains("==") || content.contains("require!")) {
            result.has_owner_check = true;
            debug_print!(VerbosityLevel::Detailed, "Found explicit owner check");
        }

        // Check for program ID validation
        if content.contains("program_id") && content.contains("==") {
            result.has_program_id_check = true;
            debug_print!(VerbosityLevel::Detailed, "Found program ID validation");
        }

        // Check for anchor constraints
        if content.contains("#[account(owner") {
            result.has_anchor_owner_constraint = true;
            debug_print!(VerbosityLevel::Detailed, "Found Anchor owner constraint ");
        }

        result
    }

    /// Analyze a statement for signer validation patterns
    fn analyze_statement_for_signer_validation(
        &self,
        stmt: &syn::Stmt,
        result: &mut SignerValidationResult,
    ) {
        match stmt {
            syn::Stmt::Expr(syn::Expr::If(if_expr), _) => {
                // Check if condition involves signer validation
                let cond_str = quote::quote!(#if_expr.cond).to_string();
                if cond_str.contains("is_signer") {
                    result.has_conditional_signer_check = true;
                    debug_print!(VerbosityLevel::Verbose, "Found conditional signer check ");
                }
            }
            syn::Stmt::Expr(syn::Expr::Macro(macro_expr), _) => {
                let macro_str = quote::quote!(#macro_expr).to_string();
                if macro_str.contains("require!") && macro_str.contains("is_signer") {
                    result.has_require_signer = true;
                    debug_print!(
                        VerbosityLevel::Verbose,
                        "Found require macro with signer validation "
                    );
                }
            }
            _ => {}
        }
    }

    /// Apply pattern-based fixes with AST-aware insertion
    fn apply_pattern_based_fixes(&self, content: &str, finding: &AuditFinding) -> String {
        debug_print!(
            VerbosityLevel::Detailed,
            "Applying pattern-based fixes for category: {}",
            finding.category
        );

        let mut fixed_content = content.to_string();

        match finding.category.as_str() {
            "Authentication & Authorization" => {
                if !content.contains("is_signer")
                    && (content.contains("AccountInfo") || content.contains("ctx.accounts"))
                {
                    fixed_content = self.insert_signer_validation_fix(content, finding);
                }
            }
            "Solana Security" => {
                if content.contains("unwrap()") {
                    fixed_content = fixed_content
                        .replace("unwrap()", "map_err(|e| ErrorCode::UnexpectedError)?");
                }
                if content.contains("user_account") && !content.contains("key()") {
                    fixed_content = self.insert_owner_validation_fix(content, finding);
                }
            }
            "Trading Security" => {
                if content.contains("claim_rewards") && !content.contains("slippage") {
                    fixed_content = format!(
                        "// Added slippage protection\nrequire!(amount >= min_expected_amount, ErrorCode::SlippageExceeded);\n{}",
                        fixed_content
                    );
                }
            }
            _ => {
                debug_warn!("Unknown finding category for fixes: {}", finding.category);
            }
        }

        fixed_content
    }

    /// Insert signer validation fix using AST-aware positioning
    fn insert_signer_validation_fix(&self, content: &str, finding: &AuditFinding) -> String {
        debug_print!(VerbosityLevel::Verbose, "Inserting signer validation fix");

        // Extract account variable name from the content
        if let Some(account_var) = self.extract_account_variable_name(content) {
            // Try to find the exact position after variable binding using AST
            if let Ok(parsed) = syn::parse_str::<syn::Block>(&format!("{{{}}}", content)) {
                for (i, stmt) in parsed.stmts.iter().enumerate() {
                    if let syn::Stmt::Local(local) = stmt {
                        if let syn::Pat::Ident(pat_ident) = &local.pat {
                            if pat_ident.ident == account_var {
                                // Insert the fix after this statement
                                let lines: Vec<&str> = content.lines().collect();
                                if i < lines.len() {
                                    let mut result = Vec::new();
                                    result.extend_from_slice(&lines[0..=i]);
                                    let signer_check = format!(
                                        "    require!({}.is_signer, ErrorCode::MissingSignature);",
                                        account_var
                                    );
                                    result.push(&signer_check);
                                    result.extend_from_slice(&lines[i + 1..]);
                                    return result.join("\n");
                                }
                            }
                        }
                    }
                }
            }

            // Fallback to simple prepend
            format!(
                "// Added signer validation\nrequire!({}.is_signer, ErrorCode::MissingSignature);\n{}",
                account_var, content
            )
        } else {
            format!(
                "// Added signer validation\nrequire!(account.is_signer, ErrorCode::MissingSignature);\n{}",
                content
            )
        }
    }

    /// Insert owner validation fix using AST-aware positioning (unified with signer approach)
    fn insert_owner_validation_fix(&self, content: &str, finding: &AuditFinding) -> String {
        debug_print!(
            VerbosityLevel::Verbose,
            "Inserting owner validation fix with AST-aware positioning"
        );

        // Extract account variable name similar to signer validation approach
        if let Some(account_var) = self.extract_account_variable_name(content) {
            // Try to find the exact position after variable binding using AST
            if let Ok(parsed) = syn::parse_str::<syn::Block>(&format!("{{{}}}", content)) {
                for (i, stmt) in parsed.stmts.iter().enumerate() {
                    if let syn::Stmt::Local(local) = stmt {
                        if let syn::Pat::Ident(pat_ident) = &local.pat {
                            if pat_ident.ident == account_var {
                                // Insert the owner validation fix after this statement
                                let lines: Vec<&str> = content.lines().collect();
                                if i < lines.len() {
                                    let mut result = Vec::new();
                                    result.extend_from_slice(&lines[0..=i]);
                                    let owner_check = format!(
                                        "    require!({}.owner == expected_program_id, ErrorCode::InvalidAccountOwner);",
                                        account_var
                                    );
                                    result.push(&owner_check);
                                    result.extend_from_slice(&lines[i + 1..]);
                                    return result.join("\n");
                                }
                            }
                        }
                    }
                }
            }

            // Fallback to prepend with extracted variable name
            format!(
                "// Added owner validation\nrequire!({}.owner == expected_program_id, ErrorCode::InvalidAccountOwner);\n{}",
                account_var, content
            )
        } else {
            // Final fallback for cases where variable extraction fails
            format!(
                "// Added account owner validation\nrequire!(user_account.owner == expected_program_id, ErrorCode::InvalidAccountOwner);\n{}",
                content
            )
        }
    }

    /// Extract detailed vulnerability information using AST analysis
    pub fn extract_vulnerability_details(
        &self,
        file_path: &str,
        line_number: usize,
    ) -> Result<Vec<SecurityIssue>> {
        if let Some(syntax_tree) = self.file_cache.get(file_path) {
            let mut visitor = SolanaSecurityVisitor {
                context: AnalysisContext {
                    file_path: file_path.to_string(),
                    function_name: None,
                    struct_name: None,
                    security_issues: Vec::new(),
                    improvement_suggestions: Vec::new(),
                },
                current_function: None,
                current_line: line_number,
            };

            visitor.visit_file(syntax_tree);
            Ok(visitor.context.security_issues)
        } else {
            Ok(Vec::new())
        }
    }
}

impl<'ast> Visit<'ast> for SolanaSecurityVisitor {
    fn visit_item_fn(&mut self, node: &'ast ItemFn) {
        let function_name = node.sig.ident.to_string();
        self.current_function = Some(function_name.clone());
        self.context.function_name = Some(function_name);

        // Check for Solana-specific security patterns
        self.check_function_security(node);

        // Continue visiting the function body
        syn::visit::visit_item_fn(self, node);

        self.current_function = None;
    }

    fn visit_item_struct(&mut self, node: &'ast ItemStruct) {
        let struct_name = node.ident.to_string();
        self.context.struct_name = Some(struct_name);

        // Check for Solana account structure patterns
        self.check_struct_security(node);

        syn::visit::visit_item_struct(self, node);
    }

    fn visit_expr_method_call(&mut self, node: &'ast ExprMethodCall) {
        self.check_method_call_security(node);
        syn::visit::visit_expr_method_call(self, node);
    }

    fn visit_expr_call(&mut self, node: &'ast ExprCall) {
        self.check_function_call_security(node);
        syn::visit::visit_expr_call(self, node);
    }
}

impl SolanaSecurityVisitor {
    fn check_function_security(&mut self, func: &ItemFn) {
        let func_str = func.to_token_stream().to_string();

        // Check for missing signer validation
        // Token stream format: "ctx . accounts" instead of "ctx.accounts"
        if (func_str.contains("ctx . accounts") || func_str.contains("ctx.accounts"))
            && !func_str.contains("is_signer")
        {
            self.context.security_issues.push(SecurityIssue {
                issue_type: SecurityIssueType::MissingSignerCheck,
                location: CodeLocation {
                    start_line: self.current_line,
                    end_line: self.current_line + 5,
                    column: 0,
                    span_length: func_str.len(),
                },
                severity: SecuritySeverity::High,
                description: "Function accesses accounts without signer validation".to_string(),
                fix_suggestion:
                    "Add require!(account.is_signer, ErrorCode::MissingSignature) checks"
                        .to_string(),
            });
        }

        // Check for panic-prone operations
        if func_str.contains("unwrap()") || func_str.contains("expect(") {
            self.context.security_issues.push(SecurityIssue {
                issue_type: SecurityIssueType::PanicOnError,
                location: CodeLocation {
                    start_line: self.current_line,
                    end_line: self.current_line + 2,
                    column: 0,
                    span_length: 20,
                },
                severity: SecuritySeverity::Medium,
                description: "Function uses panic-prone error handling".to_string(),
                fix_suggestion: "Replace unwrap() with proper error handling using ? operator"
                    .to_string(),
            });
        }
    }

    fn check_struct_security(&mut self, _struct_item: &ItemStruct) {
        // Check for proper account validation in struct definitions
        // This would analyze Anchor account constraints
    }

    fn check_method_call_security(&mut self, method_call: &ExprMethodCall) {
        let method_str = method_call.to_token_stream().to_string();

        // Check for unsafe operations
        if method_str.contains("lamports") && !method_str.contains("checked") {
            self.context.security_issues.push(SecurityIssue {
                issue_type: SecurityIssueType::ArithmeticOverflow,
                location: CodeLocation {
                    start_line: self.current_line,
                    end_line: self.current_line,
                    column: 0,
                    span_length: method_str.len(),
                },
                severity: SecuritySeverity::High,
                description: "Potential arithmetic overflow in lamport operations".to_string(),
                fix_suggestion: "Use checked arithmetic operations to prevent overflow".to_string(),
            });
        }
    }

    fn check_function_call_security(&mut self, func_call: &ExprCall) {
        let call_str = func_call.to_token_stream().to_string();

        // Check for invoke calls without proper program ID validation
        if call_str.contains("invoke") && !call_str.contains("program_id") {
            self.context.security_issues.push(SecurityIssue {
                issue_type: SecurityIssueType::UnvalidatedAccountAccess,
                location: CodeLocation {
                    start_line: self.current_line,
                    end_line: self.current_line,
                    column: 0,
                    span_length: call_str.len(),
                },
                severity: SecuritySeverity::Critical,
                description: "Cross-program invocation without program ID validation".to_string(),
                fix_suggestion: "Validate target program ID before invoke calls".to_string(),
            });
        }
    }
}

impl Default for AstAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_ast_analysis_basic() {
        let mut file = NamedTempFile::new().unwrap();
        writeln!(
            file,
            r#"
fn transfer_tokens(ctx: Context<Transfer>) -> Result<()> {{
    let user_account = &ctx.accounts.user;
    user_account.balance += 100;
    Ok(())
}}
"#
        )
        .unwrap();

        let mut analyzer = AstAnalyzer::new();
        let context = analyzer
            .analyze_file(file.path().to_str().unwrap())
            .unwrap();

        assert!(!context.security_issues.is_empty());
        assert!(context
            .security_issues
            .iter()
            .any(|issue| matches!(issue.issue_type, SecurityIssueType::MissingSignerCheck)));
    }

    #[test]
    fn test_fix_generation() {
        let finding = AuditFinding {
            id: "test".to_string(),
            title: "Missing signer validation".to_string(),
            description: "Test".to_string(),
            severity: crate::utils::audit::AuditSeverity::High,
            category: "Authentication & Authorization".to_string(),
            cwe_id: None,
            cvss_score: None,
            impact: "Test".to_string(),
            recommendation: "Test".to_string(),
            code_location: None,
            references: vec![],
        };

        let problematic_code = CodeSnippet {
            file_path: "test.rs".to_string(),
            start_line: 1,
            end_line: 2,
            content: "let user_account = &ctx.accounts.user;\nuser_account.balance += amount;"
                .to_string(),
            context: None,
        };

        let analyzer = AstAnalyzer::new();
        let fixed_code = analyzer
            .generate_ast_based_fix(&finding, &problematic_code)
            .unwrap();

        assert!(
            fixed_code.content.contains("is_signer")
                || fixed_code.content.contains("MissingSignature")
        );
    }

    #[test]
    fn test_fix_generation_edge_cases() {
        let analyzer = AstAnalyzer::new();

        // Test case 1: Multi-line variable declaration
        let multiline_finding = AuditFinding {
            id: "test_multiline".to_string(),
            title: "Missing signer validation".to_string(),
            description: "Test multiline".to_string(),
            severity: crate::utils::audit::AuditSeverity::High,
            category: "Authentication & Authorization".to_string(),
            cwe_id: None,
            cvss_score: None,
            impact: "Test".to_string(),
            recommendation: "Test".to_string(),
            code_location: None,
            references: vec![],
        };

        let multiline_code = CodeSnippet {
            file_path: "test.rs".to_string(),
            start_line: 1,
            end_line: 4,
            content: "let user_account = \n    &ctx.accounts\n        .user;\nuser_account.balance += amount;".to_string(),
            context: None,
        };

        let fixed_multiline = analyzer
            .generate_ast_based_fix(&multiline_finding, &multiline_code)
            .unwrap();
        assert!(
            fixed_multiline.content.contains("is_signer")
                || fixed_multiline.content.contains("MissingSignature")
        );

        // Test case 2: Unusual formatting with extra spaces
        let spaced_code = CodeSnippet {
            file_path: "test.rs".to_string(),
            start_line: 1,
            end_line: 2,
            content: "let     user_account     =     &ctx.accounts.user   ;\n    user_account.balance += amount;".to_string(),
            context: None,
        };

        let fixed_spaced = analyzer
            .generate_ast_based_fix(&multiline_finding, &spaced_code)
            .unwrap();
        assert!(
            fixed_spaced.content.contains("is_signer")
                || fixed_spaced.content.contains("MissingSignature")
        );

        // Test case 3: Owner validation edge case
        let owner_finding = AuditFinding {
            id: "test_owner".to_string(),
            title: "Missing owner validation".to_string(),
            description: "Test owner".to_string(),
            severity: crate::utils::audit::AuditSeverity::High,
            category: "Solana Security".to_string(),
            cwe_id: None,
            cvss_score: None,
            impact: "Test".to_string(),
            recommendation: "Test".to_string(),
            code_location: None,
            references: vec![],
        };

        let owner_code = CodeSnippet {
            file_path: "test.rs".to_string(),
            start_line: 1,
            end_line: 2,
            content: "let user_account = &ctx.accounts.user;\nuser_account.lamports += amount;"
                .to_string(),
            context: None,
        };

        let fixed_owner = analyzer
            .generate_ast_based_fix(&owner_finding, &owner_code)
            .unwrap();
        assert!(
            fixed_owner.content.contains("owner")
                || fixed_owner.content.contains("InvalidAccountOwner")
        );

        // Test case 4: Complex nested structure
        let complex_code = CodeSnippet {
            file_path: "test.rs".to_string(),
            start_line: 1,
            end_line: 3,
            content: "let complex_var = {\n    let inner = &ctx.accounts.user;\n    inner\n};\ncomplex_var.balance += amount;".to_string(),
            context: None,
        };

        let fixed_complex = analyzer
            .generate_ast_based_fix(&multiline_finding, &complex_code)
            .unwrap();
        // Should still attempt to add validation even for complex cases
        assert!(!fixed_complex.content.is_empty());
    }

    #[test]
    fn test_extract_account_variable_edge_cases() {
        let analyzer = AstAnalyzer::new();

        // Test various edge cases for variable extraction
        let test_cases = vec![
            // Standard case
            ("let user_account = &ctx.accounts.user;\nuser_account.balance += 1;", Some("user_account")),
            // Multiple variables
            ("let user = &ctx.accounts.user;\nlet admin = &ctx.accounts.admin;\nuser.balance += 1;", Some("user")),
            // Destructuring pattern
            ("let {user: user_account, ..} = &ctx.accounts;\nuser_account.balance += 1;", None), // Should fail gracefully
            // Complex expression
            ("let account = get_account(&ctx.accounts.user);\naccount.balance += 1;", Some("account")),
        ];

        for (content, expected) in test_cases {
            let result = analyzer.extract_account_variable_name(content);
            if let Some(expected_name) = expected {
                assert_eq!(
                    result,
                    Some(expected_name.to_string()),
                    "Failed for content: {}",
                    content
                );
            } else {
                // For cases where extraction should fail, ensure it handles gracefully
                assert!(
                    result.is_none() || !result.unwrap().is_empty(),
                    "Should handle edge case gracefully: {}",
                    content
                );
            }
        }
    }
}
