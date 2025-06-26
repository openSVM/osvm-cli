//! Structured Rust Code Parser for Security Analysis
//!
//! This module provides structured parsing of Rust code using the `syn` crate
//! to eliminate false positives from text-based pattern matching.

use anyhow::{Context, Result};
use std::collections::HashMap;
use syn::{
    visit::Visit, Expr, File, Item, ItemFn, ItemImpl, ItemMod, LitStr, Path, PathSegment, Stmt,
};

/// Structured analysis result from parsing Rust code
#[derive(Debug, Clone)]
pub struct ParsedCodeAnalysis {
    pub unsafe_blocks: Vec<UnsafeBlock>,
    pub unwrap_usages: Vec<UnwrapUsage>,
    pub hardcoded_secrets: Vec<HardcodedSecret>,
    pub command_executions: Vec<CommandExecution>,
    pub path_operations: Vec<PathOperation>,
    pub network_operations: Vec<NetworkOperation>,
    pub crypto_operations: Vec<CryptoOperation>,
    pub solana_operations: Vec<SolanaOperation>,
    pub function_signatures: Vec<FunctionSignature>,
    pub imports: Vec<ImportDeclaration>,
    pub string_literals: Vec<StringLiteral>,
}

#[derive(Debug, Clone)]
pub struct UnsafeBlock {
    pub line: usize,
    pub context: String,
}

#[derive(Debug, Clone)]
pub struct UnwrapUsage {
    pub line: usize,
    pub method: String, // "unwrap" or "expect"
    pub context: String,
}

#[derive(Debug, Clone)]
pub struct HardcodedSecret {
    pub line: usize,
    pub secret_type: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct CommandExecution {
    pub line: usize,
    pub command_type: String,
    pub is_dynamic: bool,
}

#[derive(Debug, Clone)]
pub struct PathOperation {
    pub line: usize,
    pub operation_type: String,
    pub is_dynamic: bool,
}

#[derive(Debug, Clone)]
pub struct NetworkOperation {
    pub line: usize,
    pub operation_type: String,
    pub uses_https: bool,
    pub endpoint: Option<String>,
}

#[derive(Debug, Clone)]
pub struct CryptoOperation {
    pub line: usize,
    pub operation_type: String,
    pub algorithm: Option<String>,
}

#[derive(Debug, Clone)]
pub struct SolanaOperation {
    pub line: usize,
    pub operation_type: String,
    pub account_validation: bool,
    pub signer_check: bool,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub is_public: bool,
    pub is_async: bool,
    pub parameters: Vec<String>,
    pub return_type: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ImportDeclaration {
    pub path: String,
    pub items: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub line: usize,
    pub value: String,
    pub context: String,
}

/// Rust code parser using syn for structured analysis
pub struct RustCodeParser;

impl RustCodeParser {
    /// Parse Rust code and extract security-relevant information
    pub fn parse_code(content: &str) -> Result<ParsedCodeAnalysis> {
        let syntax_tree = syn::parse_file(content)
            .context("Failed to parse Rust syntax")?;

        let mut visitor = SecurityVisitor::new();
        visitor.visit_file(&syntax_tree);

        Ok(visitor.into_analysis())
    }

    /// Check if code contains specific patterns using structured analysis
    pub fn contains_pattern(analysis: &ParsedCodeAnalysis, pattern_type: &str) -> bool {
        match pattern_type {
            "unsafe" => !analysis.unsafe_blocks.is_empty(),
            "unwrap" => !analysis.unwrap_usages.is_empty(),
            "secrets" => !analysis.hardcoded_secrets.is_empty(),
            "command_injection" => analysis.command_executions.iter().any(|c| c.is_dynamic),
            "path_traversal" => analysis.path_operations.iter().any(|p| p.is_dynamic),
            "insecure_network" => analysis.network_operations.iter().any(|n| !n.uses_https),
            "solana_security" => !analysis.solana_operations.is_empty(),
            _ => false,
        }
    }
}

/// Visitor for collecting security-relevant information from the AST
struct SecurityVisitor {
    unsafe_blocks: Vec<UnsafeBlock>,
    unwrap_usages: Vec<UnwrapUsage>,
    hardcoded_secrets: Vec<HardcodedSecret>,
    command_executions: Vec<CommandExecution>,
    path_operations: Vec<PathOperation>,
    network_operations: Vec<NetworkOperation>,
    crypto_operations: Vec<CryptoOperation>,
    solana_operations: Vec<SolanaOperation>,
    function_signatures: Vec<FunctionSignature>,
    imports: Vec<ImportDeclaration>,
    string_literals: Vec<StringLiteral>,
    current_line: usize,
}

impl SecurityVisitor {
    fn new() -> Self {
        Self {
            unsafe_blocks: Vec::new(),
            unwrap_usages: Vec::new(),
            hardcoded_secrets: Vec::new(),
            command_executions: Vec::new(),
            path_operations: Vec::new(),
            network_operations: Vec::new(),
            crypto_operations: Vec::new(),
            solana_operations: Vec::new(),
            function_signatures: Vec::new(),
            imports: Vec::new(),
            string_literals: Vec::new(),
            current_line: 1,
        }
    }

    fn into_analysis(self) -> ParsedCodeAnalysis {
        ParsedCodeAnalysis {
            unsafe_blocks: self.unsafe_blocks,
            unwrap_usages: self.unwrap_usages,
            hardcoded_secrets: self.hardcoded_secrets,
            command_executions: self.command_executions,
            path_operations: self.path_operations,
            network_operations: self.network_operations,
            crypto_operations: self.crypto_operations,
            solana_operations: self.solana_operations,
            function_signatures: self.function_signatures,
            imports: self.imports,
            string_literals: self.string_literals,
        }
    }

    fn analyze_method_call(&mut self, expr: &Expr) {
        if let Expr::MethodCall(method_call) = expr {
            let method_name = method_call.method.to_string();
            
            match method_name.as_str() {
                "unwrap" => {
                    self.unwrap_usages.push(UnwrapUsage {
                        line: self.current_line,
                        method: "unwrap".to_string(),
                        context: "method_call".to_string(),
                    });
                }
                "expect" => {
                    self.unwrap_usages.push(UnwrapUsage {
                        line: self.current_line,
                        method: "expect".to_string(),
                        context: "method_call".to_string(),
                    });
                }
                _ => {}
            }
        }
    }

    fn analyze_function_call(&mut self, expr: &Expr) {
        if let Expr::Call(call) = expr {
            if let Expr::Path(path) = &*call.func {
                let path_string = path_to_string(&path.path);
                
                // Check for command execution patterns
                if path_string.contains("Command::new") || path_string.contains("std::process::Command") {
                    let is_dynamic = call.args.iter().any(|arg| is_dynamic_expr(arg));
                    self.command_executions.push(CommandExecution {
                        line: self.current_line,
                        command_type: "Command::new".to_string(),
                        is_dynamic,
                    });
                }

                // Check for path operations
                if path_string.contains("Path::new") || path_string.contains("PathBuf::from") {
                    let is_dynamic = call.args.iter().any(|arg| is_dynamic_expr(arg));
                    self.path_operations.push(PathOperation {
                        line: self.current_line,
                        operation_type: path_string.clone(),
                        is_dynamic,
                    });
                }

                // Check for Solana operations
                if path_string.contains("solana") || path_string.contains("anchor") {
                    self.solana_operations.push(SolanaOperation {
                        line: self.current_line,
                        operation_type: path_string,
                        account_validation: false, // TODO: Implement deeper analysis
                        signer_check: false,
                    });
                }
            }
        }
    }

    fn analyze_string_literal(&mut self, lit: &LitStr) {
        let value = lit.value();
        self.string_literals.push(StringLiteral {
            line: self.current_line,
            value: value.clone(),
            context: "string_literal".to_string(),
        });

        // Check for hardcoded secrets
        if self.is_potential_secret(&value) {
            self.hardcoded_secrets.push(HardcodedSecret {
                line: self.current_line,
                secret_type: self.classify_secret(&value),
                value: value.clone(),
            });
        }

        // Check for network operations
        if value.starts_with("http://") || value.starts_with("https://") {
            self.network_operations.push(NetworkOperation {
                line: self.current_line,
                operation_type: "url".to_string(),
                uses_https: value.starts_with("https://"),
                endpoint: Some(value),
            });
        }
    }

    fn is_potential_secret(&self, value: &str) -> bool {
        // More sophisticated secret detection using patterns
        let secret_patterns = [
            r"^[A-Za-z0-9+/]{20,}={0,2}$", // Base64
            r"^[0-9a-fA-F]{32,}$",         // Hex strings
            r"^sk-[A-Za-z0-9]{48}$",       // OpenAI API key pattern
        ];

        for pattern in &secret_patterns {
            if regex::Regex::new(pattern).unwrap().is_match(value) {
                return true;
            }
        }

        // Check for common secret keywords
        let lower_value = value.to_lowercase();
        lower_value.len() > 16 && (
            lower_value.contains("password") ||
            lower_value.contains("secret") ||
            lower_value.contains("key") ||
            lower_value.contains("token") ||
            lower_value.contains("api")
        )
    }

    fn classify_secret(&self, value: &str) -> String {
        if value.starts_with("sk-") {
            "openai_api_key".to_string()
        } else if regex::Regex::new(r"^[A-Za-z0-9+/]{20,}={0,2}$").unwrap().is_match(value) {
            "base64_encoded".to_string()
        } else if regex::Regex::new(r"^[0-9a-fA-F]{32,}$").unwrap().is_match(value) {
            "hex_encoded".to_string()
        } else {
            "generic_secret".to_string()
        }
    }
}

impl<'ast> Visit<'ast> for SecurityVisitor {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr {
            Expr::Unsafe(_) => {
                self.unsafe_blocks.push(UnsafeBlock {
                    line: self.current_line,
                    context: "unsafe block".to_string(),
                });
            }
            Expr::Lit(lit) => {
                if let syn::Lit::Str(lit_str) = &lit.lit {
                    self.analyze_string_literal(lit_str);
                }
            }
            Expr::MethodCall(_) => {
                self.analyze_method_call(expr);
            }
            Expr::Call(_) => {
                self.analyze_function_call(expr);
            }
            _ => {}
        }

        syn::visit::visit_expr(self, expr);
    }

    fn visit_item_fn(&mut self, item_fn: &'ast ItemFn) {
        let signature = FunctionSignature {
            name: item_fn.sig.ident.to_string(),
            is_public: matches!(item_fn.vis, syn::Visibility::Public(_)),
            is_async: item_fn.sig.asyncness.is_some(),
            parameters: item_fn.sig.inputs.iter().map(|_| "param".to_string()).collect(),
            return_type: None, // TODO: Implement return type analysis
        };
        self.function_signatures.push(signature);

        syn::visit::visit_item_fn(self, item_fn);
    }

    fn visit_item_use(&mut self, item_use: &'ast syn::ItemUse) {
        let import = ImportDeclaration {
            path: "use_item".to_string(), // Simplified for now
            items: vec![], // TODO: Implement detailed import analysis
        };
        self.imports.push(import);

        syn::visit::visit_item_use(self, item_use);
    }
}

/// Helper function to convert a Path to a string
fn path_to_string(path: &Path) -> String {
    path.segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

/// Check if an expression involves dynamic/runtime values
fn is_dynamic_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Lit(_) => false, // Literal values are static
        Expr::Path(_) => true, // Variables are dynamic
        Expr::Call(_) => true, // Function calls are dynamic
        Expr::MethodCall(_) => true, // Method calls are dynamic
        Expr::Macro(_) => true, // Macros could be dynamic
        _ => true, // Conservative assumption for other expressions
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_unsafe_code() {
        let code = r#"
            fn test() {
                unsafe {
                    let ptr = std::ptr::null_mut();
                }
            }
        "#;

        let analysis = RustCodeParser::parse_code(code).unwrap();
        assert_eq!(analysis.unsafe_blocks.len(), 1);
    }

    #[test]
    fn test_parse_unwrap_usage() {
        let code = r#"
            fn test() {
                let result = Some(42);
                let value = result.unwrap();
                let value2 = result.expect("failed");
            }
        "#;

        let analysis = RustCodeParser::parse_code(code).unwrap();
        assert_eq!(analysis.unwrap_usages.len(), 2);
    }

    #[test]
    fn test_parse_hardcoded_secrets() {
        let code = r#"
            fn test() {
                let api_key = "sk-1234567890abcdef1234567890abcdef12345678";
                let password = "super_secret_password_123";
            }
        "#;

        let analysis = RustCodeParser::parse_code(code).unwrap();
        assert!(!analysis.hardcoded_secrets.is_empty());
    }

    #[test]
    fn test_contains_pattern() {
        let code = r#"
            fn test() {
                unsafe { let ptr = std::ptr::null_mut(); }
                let result = Some(42).unwrap();
            }
        "#;

        let analysis = RustCodeParser::parse_code(code).unwrap();
        assert!(RustCodeParser::contains_pattern(&analysis, "unsafe"));
        assert!(RustCodeParser::contains_pattern(&analysis, "unwrap"));
        assert!(!RustCodeParser::contains_pattern(&analysis, "secrets"));
    }
}