//! OVSM Language Server Protocol Implementation
//!
//! Provides IDE features for OVSM LISP:
//! - Real-time syntax error detection
//! - Parenthesis matching and error highlighting
//! - Hover documentation for built-in functions
//! - Semantic token highlighting
//! - Go-to-definition for variables and functions
//! - AI-powered code suggestions (with LLM integration)
//! - **Self-improving behavior** via usage telemetry
//!
//! # Architecture
//!
//! The LSP server reuses the OVSM lexer and parser to provide accurate
//! diagnostics. When a document is opened or changed, we:
//! 1. Tokenize the source using `SExprScanner`
//! 2. Parse tokens using `SExprParser`
//! 3. Convert any errors to LSP Diagnostics with precise locations
//! 4. Build a symbol table for go-to-definition
//! 5. Provide hover info based on token positions
//! 6. Track usage patterns and adapt suggestions over time

pub mod ai_completion;
pub mod backend;
pub mod blockchain_rpc;
pub mod blockchain_types;
pub mod diagnostics;
pub mod documentation;
pub mod investigation_templates;
pub mod knowledge_transfer;
pub mod repl;
pub mod semantic_tokens;
pub mod symbols;
pub mod telemetry;

pub use backend::OvsmLanguageServer;
pub use blockchain_rpc::BlockchainRpc;
pub use investigation_templates::TemplateGenerator;
pub use knowledge_transfer::KnowledgeProfile;
pub use telemetry::{LearningConfig, LearningEngine, LearningStats};
