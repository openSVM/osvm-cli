# OVSM Interpreter - Rust Module Structure Guide

**Document Type**: Implementation Reference
**Related**: INTERPRETER_DESIGN.md, IMPLEMENTATION_TASKS.md
**Target Audience**: Rust Developers

---

## Table of Contents

1. [Project Structure](#project-structure)
2. [Module Organization](#module-organization)
3. [Cargo Configuration](#cargo-configuration)
4. [Module Dependencies](#module-dependencies)
5. [Code Examples](#code-examples)
6. [Integration with osvm-cli](#integration-with-osvm-cli)
7. [Build & Test Commands](#build--test-commands)
8. [Best Practices](#best-practices)

---

## 1. Project Structure

### 1.1 Complete Directory Layout

```
osvm-cli/
├── Cargo.toml                      # Workspace root
├── src/                            # Main osvm-cli code
│   ├── main.rs
│   ├── lib.rs
│   └── ... (existing code)
│
├── crates/
│   └── ovsm/                       # OVSM interpreter crate
│       ├── Cargo.toml
│       ├── src/
│       │   ├── lib.rs              # Public API
│       │   │
│       │   ├── lexer/
│       │   │   ├── mod.rs          # Module exports
│       │   │   ├── token.rs        # Token types
│       │   │   └── scanner.rs      # Tokenization logic
│       │   │
│       │   ├── parser/
│       │   │   ├── mod.rs          # Parser API
│       │   │   ├── ast.rs          # AST node types
│       │   │   ├── expr.rs         # Expression parsing
│       │   │   ├── stmt.rs         # Statement parsing
│       │   │   └── error.rs        # Parse errors
│       │   │
│       │   ├── runtime/
│       │   │   ├── mod.rs          # Runtime API
│       │   │   ├── value.rs        # Value type
│       │   │   ├── environment.rs  # Scoping & variables
│       │   │   ├── evaluator.rs    # Expression evaluation
│       │   │   ├── executor.rs     # Statement execution
│       │   │   └── context.rs      # Execution context
│       │   │
│       │   ├── tools/
│       │   │   ├── mod.rs          # Tool system API
│       │   │   ├── registry.rs     # Tool registry
│       │   │   ├── signature.rs    # Tool signatures
│       │   │   │
│       │   │   ├── stdlib/         # Standard library
│       │   │   │   ├── mod.rs      # Stdlib exports
│       │   │   │   ├── solana_rpc.rs
│       │   │   │   ├── data_processing.rs
│       │   │   │   ├── statistics.rs
│       │   │   │   ├── math.rs
│       │   │   │   ├── string_ops.rs
│       │   │   │   └── utilities.rs
│       │   │   │
│       │   │   └── agents/         # Agent tools
│       │   │       ├── mod.rs
│       │   │       ├── delegation.rs
│       │   │       ├── knowledge_graph.rs
│       │   │       ├── hypothesis.rs
│       │   │       └── meta_learning.rs
│       │   │
│       │   ├── ai/
│       │   │   ├── mod.rs          # AI service API
│       │   │   ├── service.rs      # AI service trait
│       │   │   ├── openai.rs       # OpenAI implementation
│       │   │   ├── decision.rs     # Decision making
│       │   │   └── planning.rs     # Plan generation
│       │   │
│       │   ├── parallel/
│       │   │   ├── mod.rs          # Parallel execution API
│       │   │   ├── executor.rs     # Async executor
│       │   │   └── strategy.rs     # Wait strategies
│       │   │
│       │   ├── metrics/
│       │   │   ├── mod.rs          # Metrics API
│       │   │   ├── tracker.rs      # Metric tracking
│       │   │   └── reporter.rs     # Metric reporting
│       │   │
│       │   └── error.rs            # Error types
│       │
│       ├── tests/                  # Integration tests
│       │   ├── lexer_tests.rs
│       │   ├── parser_tests.rs
│       │   ├── runtime_tests.rs
│       │   ├── tools_tests.rs
│       │   ├── parallel_tests.rs
│       │   ├── error_handling_tests.rs
│       │   └── integration/
│       │       ├── mod.rs
│       │       ├── basic_programs.rs
│       │       ├── control_flow.rs
│       │       ├── standard_library.rs
│       │       └── advanced_features.rs
│       │
│       ├── benches/                # Benchmarks
│       │   ├── lexer_bench.rs
│       │   ├── parser_bench.rs
│       │   └── execution_bench.rs
│       │
│       └── examples/               # Example OVSM programs
│           ├── average_fee.ovsm
│           ├── dex_comparison.ovsm
│           ├── validator_analysis.ovsm
│           └── knowledge_graph.ovsm
│
└── docs/
    └── ovsm/
        ├── INTERPRETER_SPECIFICATION.md
        ├── INTERPRETER_DESIGN.md
        ├── IMPLEMENTATION_TASKS.md
        └── RUST_MODULE_STRUCTURE.md (this file)
```

---

## 2. Module Organization

### 2.1 lib.rs (Public API)

```rust
// crates/ovsm/src/lib.rs

//! # OVSM Interpreter
//!
//! A runtime interpreter for the OVSM (Open Versatile Seeker Mind) language.
//!
//! ## Quick Start
//!
//! ```rust
//! use ovsm::{Interpreter, ToolRegistry};
//!
//! let registry = ToolRegistry::new();
//! let mut interpreter = Interpreter::new(registry);
//!
//! let source = r#"
//!     $x = 10
//!     $y = 20
//!     $result = $x + $y
//! "#;
//!
//! let result = interpreter.execute(source)?;
//! assert_eq!(result.as_int()?, 30);
//! ```

#![allow(dead_code)]  // Remove after implementation
#![warn(missing_docs)]
#![warn(clippy::all)]

// Re-export main types for convenience
pub use crate::lexer::{Token, TokenKind, Scanner};
pub use crate::parser::{Parser, Program, Statement, Expression};
pub use crate::runtime::{Value, Environment, Evaluator, Interpreter};
pub use crate::tools::{Tool, ToolRegistry, ToolSignature};
pub use crate::error::{Error, Result};

// Module declarations
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod tools;
pub mod ai;
pub mod parallel;
pub mod metrics;
pub mod error;

/// Main interpreter struct - high-level API
///
/// # Examples
///
/// ```
/// use ovsm::Interpreter;
///
/// let mut interp = Interpreter::new_default();
/// let result = interp.execute("$x = 42")?;
/// ```
pub struct Interpreter {
    evaluator: runtime::Evaluator,
    registry: tools::ToolRegistry,
}

impl Interpreter {
    /// Create new interpreter with given tool registry
    pub fn new(registry: tools::ToolRegistry) -> Self {
        Interpreter {
            evaluator: runtime::Evaluator::new(registry.clone()),
            registry,
        }
    }

    /// Create interpreter with all standard library tools
    pub fn new_default() -> Self {
        let registry = tools::ToolRegistry::new();
        Self::new(registry)
    }

    /// Execute OVSM source code
    pub async fn execute(&mut self, source: &str) -> Result<Value> {
        let tokens = lexer::Scanner::new(source).scan_tokens()?;
        let program = parser::Parser::new(tokens).parse()?;
        self.evaluator.evaluate_program(&program).await
    }

    /// Parse source code to AST (for debugging/analysis)
    pub fn parse(&self, source: &str) -> Result<Program> {
        let tokens = lexer::Scanner::new(source).scan_tokens()?;
        parser::Parser::new(tokens).parse()
    }

    /// Validate OVSM source code without executing
    pub fn validate(&self, source: &str) -> Result<()> {
        let _program = self.parse(source)?;
        // Additional validation could go here
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_basic_execution() {
        let mut interp = Interpreter::new_default();
        let result = interp.execute("$x = 42").await.unwrap();
        assert_eq!(result.as_int().unwrap(), 42);
    }
}
```

---

### 2.2 lexer/mod.rs

```rust
// crates/ovsm/src/lexer/mod.rs

//! Lexical analysis for OVSM
//!
//! Converts source text into a stream of tokens.

mod token;
mod scanner;

pub use token::{Token, TokenKind};
pub use scanner::Scanner;

#[cfg(test)]
mod tests;
```

---

### 2.3 lexer/token.rs

```rust
// crates/ovsm/src/lexer/token.rs

use serde::{Serialize, Deserialize};

/// A single token from the source code
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize, column: usize) -> Self {
        Token { kind, lexeme, line, column }
    }
}

/// All possible token types in OVSM
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TokenKind {
    // Literals
    Integer(i64),
    Float(f64),
    String(String),
    True,
    False,
    Null,

    // Identifiers
    Identifier(String),
    Variable(String),   // $name
    Constant(String),   // UPPERCASE

    // Keywords
    If, Else, Then,
    While, For, In,
    Break, Continue, Return,
    Try, Catch,
    Parallel, WaitAll, WaitAny, Race,
    Decision, Branch,
    Guard, Match,
    Define, DefineTool, Const, Tool,

    // Operators
    Plus, Minus, Star, Slash, Percent, StarStar,
    Eq, NotEq, Lt, Gt, LtEq, GtEq,
    And, Or, Not, Xor,
    Assign,
    Question, Colon,
    QuestionDot, QuestionQuestion,
    Arrow, FatArrow,

    // Delimiters
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    LeftBracket, RightBracket,
    Comma, Dot, Semicolon,
    Newline,

    // Special
    Eof,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenKind::Integer(n) => write!(f, "{}", n),
            TokenKind::Float(fl) => write!(f, "{}", fl),
            TokenKind::String(s) => write!(f, "\"{}\"", s),
            TokenKind::Identifier(id) => write!(f, "{}", id),
            TokenKind::Variable(name) => write!(f, "${}", name),
            _ => write!(f, "{:?}", self),
        }
    }
}
```

---

### 2.4 parser/mod.rs

```rust
// crates/ovsm/src/parser/mod.rs

//! Parser for OVSM
//!
//! Converts token stream into Abstract Syntax Tree (AST).

mod ast;
mod expr;
mod stmt;
mod error;

pub use ast::*;
pub use error::ParseError;

use crate::lexer::Token;
use crate::error::Result;

/// Parser for OVSM language
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    /// Create new parser from token stream
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    /// Parse tokens into a Program AST
    pub fn parse(&mut self) -> Result<Program> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            if let Some(stmt) = self.declaration()? {
                statements.push(stmt);
            }
        }

        Ok(Program {
            metadata: ProgramMetadata::default(),
            statements,
        })
    }

    fn declaration(&mut self) -> Result<Option<Statement>> {
        // Skip newlines
        while self.match_token(&[TokenKind::Newline]) {
            self.advance();
        }

        if self.is_at_end() {
            return Ok(None);
        }

        match &self.peek().kind {
            TokenKind::Const => self.const_declaration(),
            TokenKind::DefineTool => self.tool_definition(),
            _ => self.statement(),
        }
    }

    // ... more parser methods (see parser/expr.rs and parser/stmt.rs)

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        &self.tokens[self.current - 1]
    }

    fn match_token(&self, types: &[TokenKind]) -> bool {
        types.iter().any(|t| std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(t))
    }
}
```

---

### 2.5 runtime/mod.rs

```rust
// crates/ovsm/src/runtime/mod.rs

//! Runtime execution for OVSM programs

mod value;
mod environment;
mod evaluator;
mod executor;
mod context;

pub use value::Value;
pub use environment::Environment;
pub use evaluator::Evaluator;
pub use executor::Executor;
pub use context::ExecutionContext;

// Re-export from main lib for convenience
pub use crate::Interpreter;
```

---

### 2.6 tools/mod.rs

```rust
// crates/ovsm/src/tools/mod.rs

//! Tool system for OVSM
//!
//! Provides the framework for built-in and custom tools.

mod registry;
mod signature;

pub mod stdlib;
pub mod agents;

pub use registry::ToolRegistry;
pub use signature::{Tool, ToolSignature, Parameter, ToolArguments};

use crate::runtime::Value;
use crate::error::Result;
use async_trait::async_trait;

/// Trait for all OVSM tools
#[async_trait]
pub trait Tool: Send + Sync {
    /// Tool name
    fn name(&self) -> &str;

    /// Tool description
    fn description(&self) -> &str;

    /// Tool signature (parameters and return type)
    fn signature(&self) -> ToolSignature;

    /// Execute the tool (sync version)
    fn execute(
        &self,
        args: ToolArguments,
        context: &ExecutionContext,
    ) -> Result<Value> {
        // Default implementation returns error
        Err(crate::error::Error::NotImplemented {
            tool: self.name().to_string(),
        })
    }

    /// Execute the tool (async version)
    async fn execute_async(
        &self,
        args: ToolArguments,
        context: &ExecutionContext,
    ) -> Result<Value> {
        // Default: delegate to sync version
        self.execute(args, context)
    }

    /// Is this tool async?
    fn is_async(&self) -> bool {
        false
    }

    /// Estimated cost in SOL (for blockchain operations)
    fn estimated_cost(&self) -> f64 {
        0.0
    }

    /// Can this tool fail?
    fn can_fail(&self) -> bool {
        true
    }
}
```

---

### 2.7 tools/stdlib/mod.rs

```rust
// crates/ovsm/src/tools/stdlib/mod.rs

//! Standard library tools for OVSM

pub mod solana_rpc;
pub mod data_processing;
pub mod statistics;
pub mod math;
pub mod string_ops;
pub mod utilities;

use crate::tools::ToolRegistry;

/// Register all standard library tools
pub fn register_all(registry: &mut ToolRegistry) {
    // Solana RPC
    solana_rpc::register(registry);

    // Data processing
    data_processing::register(registry);

    // Statistics
    statistics::register(registry);

    // Math
    math::register(registry);

    // String operations
    string_ops::register(registry);

    // Utilities
    utilities::register(registry);
}
```

---

## 3. Cargo Configuration

### 3.1 Workspace Cargo.toml

```toml
# Root Cargo.toml

[workspace]
members = [".", "crates/ovsm"]
resolver = "2"

[workspace.dependencies]
# Async
tokio = { version = "1.35", features = ["full"] }
async-trait = "0.1"

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

# Error handling
anyhow = "1.0"
thiserror = "1.0"

# Logging
tracing = "0.1"
tracing-subscriber = "0.3"

# Testing
proptest = "1.4"
criterion = "0.5"

[package]
name = "osvm"
version = "0.1.0"
edition = "2021"

[dependencies]
# Existing osvm-cli dependencies...
ovsm = { path = "crates/ovsm" }

# ... rest of osvm-cli config
```

---

### 3.2 OVSM Crate Cargo.toml

```toml
# crates/ovsm/Cargo.toml

[package]
name = "ovsm"
version = "1.0.0"
edition = "2021"
authors = ["OSVM Team"]
description = "OVSM (Open Versatile Seeker Mind) language interpreter"
license = "MIT"
repository = "https://github.com/opensvm/osvm-cli"
keywords = ["blockchain", "solana", "language", "interpreter"]
categories = ["parser-implementations", "development-tools"]

[dependencies]
# Async runtime
tokio = { workspace = true }
async-trait = { workspace = true }

# Serialization
serde = { workspace = true }
serde_json = { workspace = true }

# Error handling
anyhow = { workspace = true }
thiserror = { workspace = true }

# Logging
tracing = { workspace = true }

# Solana
solana-sdk = "1.18"
solana-client = "1.18"
solana-transaction-status = "1.18"

# Data structures
dashmap = "5.5"
lru = "0.12"

# Time
chrono = "0.4"

# UUID for agents
uuid = { version = "1.6", features = ["v4", "serde"] }

[dev-dependencies]
proptest = { workspace = true }
criterion = { workspace = true }
bolero = "0.10"
tokio-test = "0.4"

[[bench]]
name = "execution_bench"
harness = false

[features]
default = ["stdlib", "agents"]
stdlib = []  # Standard library tools
agents = []  # Agent extension tools
```

---

## 4. Module Dependencies

### 4.1 Dependency Graph

```
lexer (no dependencies)
   ↓
parser (depends on: lexer)
   ↓
runtime (depends on: parser, lexer)
   ↓
   ├─→ tools (depends on: runtime)
   │     ├─→ stdlib (depends on: tools, runtime, solana-client)
   │     └─→ agents (depends on: tools, runtime, ai)
   │
   ├─→ ai (depends on: runtime, parser)
   │
   ├─→ parallel (depends on: runtime, tokio)
   │
   └─→ metrics (depends on: runtime)

error (no dependencies, used by all modules)
```

### 4.2 Import Examples

```rust
// In runtime/evaluator.rs
use crate::parser::ast::{Expression, Statement};
use crate::runtime::{Value, Environment};
use crate::tools::ToolRegistry;
use crate::error::{Error, Result};

// In tools/stdlib/data_processing.rs
use crate::tools::{Tool, ToolSignature, ToolArguments};
use crate::runtime::{Value, ExecutionContext};
use crate::error::Result;
use async_trait::async_trait;

// In ai/openai.rs
use crate::ai::AiService;
use crate::runtime::Value;
use crate::parser::ast::DecisionPoint;
use crate::error::Result;
```

---

## 5. Code Examples

### 5.1 Implementing a New Tool

```rust
// crates/ovsm/src/tools/stdlib/my_tool.rs

use crate::tools::{Tool, ToolSignature, Parameter, ToolArguments};
use crate::runtime::{Value, ExecutionContext};
use crate::parser::ast::TypeAnnotation;
use crate::error::Result;

pub struct MyCustomTool;

impl Tool for MyCustomTool {
    fn name(&self) -> &str {
        "MY_CUSTOM_TOOL"
    }

    fn description(&self) -> &str {
        "Does something custom"
    }

    fn signature(&self) -> ToolSignature {
        ToolSignature {
            params: vec![
                Parameter {
                    name: "input".to_string(),
                    type_annotation: TypeAnnotation::String,
                    required: true,
                    default: None,
                },
            ],
            returns: TypeAnnotation::String,
        }
    }

    fn execute(
        &self,
        args: ToolArguments,
        _context: &ExecutionContext,
    ) -> Result<Value> {
        let input = args.get_named("input")?.as_string()?;
        let result = format!("Processed: {}", input);
        Ok(Value::String(result))
    }
}

// Register in stdlib/mod.rs
pub fn register_custom_tools(registry: &mut ToolRegistry) {
    registry.register(MyCustomTool);
}
```

---

### 5.2 Using the Interpreter

```rust
// In osvm-cli src/main.rs

use ovsm::{Interpreter, ToolRegistry};

async fn run_ovsm_file(path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let source = std::fs::read_to_string(path)?;

    let mut registry = ToolRegistry::new();
    // Configure with Solana RPC client
    let rpc_client = Arc::new(RpcClient::new(config.json_rpc_url.clone()));
    registry.set_rpc_client(rpc_client);

    let mut interpreter = Interpreter::new(registry);
    let result = interpreter.execute(&source).await?;

    println!("Result: {:#?}", result);
    Ok(())
}
```

---

## 6. Integration with osvm-cli

### 6.1 Add OVSM Subcommand

```rust
// src/clparse.rs

pub fn parse_command_line() -> ArgMatches {
    Command::new("osvm")
        .subcommand(
            Command::new("ovsm")
                .about("Execute OVSM language programs")
                .subcommand(
                    Command::new("run")
                        .about("Execute OVSM file")
                        .arg(
                            Arg::new("file")
                                .help("Path to .ovsm file")
                                .required(true)
                        )
                        .arg(
                            Arg::new("debug")
                                .long("debug")
                                .help("Show debug output")
                        )
                )
                .subcommand(
                    Command::new("parse")
                        .about("Parse OVSM file and display AST")
                        .arg(
                            Arg::new("file")
                                .help("Path to .ovsm file")
                                .required(true)
                        )
                )
                .subcommand(
                    Command::new("validate")
                        .about("Validate OVSM file syntax")
                        .arg(
                            Arg::new("file")
                                .help("Path to .ovsm file")
                                .required(true)
                        )
                )
        )
        // ... other subcommands
}
```

---

### 6.2 Handle OVSM Commands

```rust
// src/main.rs

async fn handle_ovsm_command(matches: &ArgMatches, config: &Config) -> Result<()> {
    match matches.subcommand() {
        Some(("run", sub_matches)) => {
            let file_path = sub_matches.get_one::<String>("file").unwrap();
            let debug = sub_matches.get_flag("debug");

            run_ovsm_file(file_path, config, debug).await?;
        },

        Some(("parse", sub_matches)) => {
            let file_path = sub_matches.get_one::<String>("file").unwrap();
            parse_ovsm_file(file_path)?;
        },

        Some(("validate", sub_matches)) => {
            let file_path = sub_matches.get_one::<String>("file").unwrap();
            validate_ovsm_file(file_path)?;
        },

        _ => {
            eprintln!("Unknown OVSM subcommand. Use --help for usage.");
        }
    }

    Ok(())
}

async fn run_ovsm_file(
    path: &str,
    config: &Config,
    debug: bool,
) -> Result<()> {
    use ovsm::{Interpreter, ToolRegistry};

    let source = std::fs::read_to_string(path)?;

    let mut registry = ToolRegistry::new();

    // Configure RPC client from osvm-cli config
    let rpc_client = Arc::new(RpcClient::new(config.json_rpc_url.clone()));
    registry.set_rpc_client(rpc_client);

    // Configure AI service if available
    if let Some(ai_config) = &config.ai_config {
        let ai_service = OpenAiService::new(
            ai_config.api_url.clone(),
            ai_config.api_key.clone(),
        );
        registry.set_ai_service(Box::new(ai_service));
    }

    let mut interpreter = Interpreter::new(registry);

    if debug {
        tracing::info!("Executing OVSM file: {}", path);
    }

    let result = interpreter.execute(&source).await?;

    println!("{}", format_result(&result));

    Ok(())
}

fn format_result(value: &ovsm::Value) -> String {
    match value {
        ovsm::Value::Null => "null".to_string(),
        ovsm::Value::Bool(b) => b.to_string(),
        ovsm::Value::Int(n) => n.to_string(),
        ovsm::Value::Float(f) => f.to_string(),
        ovsm::Value::String(s) => s.clone(),
        ovsm::Value::Array(arr) => {
            let items: Vec<String> = arr.iter()
                .map(|v| format_result(v))
                .collect();
            format!("[{}]", items.join(", "))
        },
        ovsm::Value::Object(obj) => {
            serde_json::to_string_pretty(obj).unwrap_or_else(|_| format!("{:?}", obj))
        },
        _ => format!("{:?}", value),
    }
}
```

---

## 7. Build & Test Commands

### 7.1 Development Workflow

```bash
# Build everything
cargo build

# Build just OVSM crate
cargo build -p ovsm

# Run all tests
cargo test

# Run just OVSM tests
cargo test -p ovsm

# Run specific test
cargo test -p ovsm test_map_tool

# Run tests with output
cargo test -p ovsm -- --nocapture

# Run benchmarks
cargo bench -p ovsm

# Check code without building
cargo check -p ovsm

# Format code
cargo fmt -p ovsm

# Lint code
cargo clippy -p ovsm

# Generate documentation
cargo doc -p ovsm --open

# Run example OVSM program
cargo run -- ovsm run examples/average_fee.ovsm

# With debug output
cargo run -- ovsm run examples/average_fee.ovsm --debug
```

---

### 7.2 Testing Examples

```bash
# Run all unit tests
cargo test -p ovsm --lib

# Run all integration tests
cargo test -p ovsm --test integration_tests

# Run property-based tests
cargo test -p ovsm property_tests

# Run with coverage
cargo tarpaulin -p ovsm --out Html

# Run fuzz tests (1 hour)
cargo bolero test -p ovsm fuzz_parser --time 3600
```

---

## 8. Best Practices

### 8.1 Error Handling

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Parse error at line {line}, column {col}: {message}")]
    ParseError {
        line: usize,
        col: usize,
        message: String,
    },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String },

    #[error("Type error: expected {expected}, got {got}")]
    TypeError { expected: String, got: String },
}

pub type Result<T> = std::result::Result<T, Error>;
```

---

### 8.2 Async Code

```rust
use async_trait::async_trait;

#[async_trait]
impl Tool for GetBlockTool {
    async fn execute_async(
        &self,
        args: ToolArguments,
        context: &ExecutionContext,
    ) -> Result<Value> {
        let slot = args.get_named("slot")?.as_int()? as u64;

        let block = context
            .rpc_client
            .get_block(slot)
            .await
            .map_err(|e| Error::RpcError {
                message: e.to_string(),
            })?;

        Ok(convert_block_to_value(block))
    }

    fn is_async(&self) -> bool {
        true
    }
}
```

---

### 8.3 Testing

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let source = "$x = 42";
        let tokens = tokenize(source).unwrap();
        assert_eq!(tokens.len(), 4); // $x, =, 42, EOF
    }

    #[tokio::test]
    async fn test_async_tool() {
        let tool = GetSlotTool;
        let args = ToolArguments::empty();
        let context = ExecutionContext::mock();

        let result = tool.execute_async(args, &context).await.unwrap();
        assert!(matches!(result, Value::Int(_)));
    }

    #[test]
    fn test_property() {
        use proptest::prelude::*;

        proptest!(|(a in 0i64..1000, b in 1i64..1000)| {
            let program = format!("$result = {} / {}", a, b);
            let result = execute_ovsm(&program).unwrap();
            assert_eq!(result.as_int().unwrap(), a / b);
        });
    }
}
```

---

### 8.4 Documentation

```rust
/// Execute OVSM source code
///
/// # Arguments
///
/// * `source` - OVSM source code string
///
/// # Returns
///
/// The result value from executing the program
///
/// # Errors
///
/// Returns error if:
/// - Source code has syntax errors
/// - Runtime error occurs during execution
/// - Tool execution fails
///
/// # Examples
///
/// ```
/// use ovsm::Interpreter;
///
/// let mut interp = Interpreter::new_default();
/// let result = interp.execute("$x = 42").await?;
/// assert_eq!(result.as_int()?, 42);
/// ```
pub async fn execute(&mut self, source: &str) -> Result<Value> {
    // Implementation
}
```

---

## 9. Quick Reference

### 9.1 Common Tasks

| Task | Command |
|------|---------|
| Build OVSM crate | `cargo build -p ovsm` |
| Run OVSM tests | `cargo test -p ovsm` |
| Run specific test | `cargo test -p ovsm test_name` |
| Format code | `cargo fmt -p ovsm` |
| Lint code | `cargo clippy -p ovsm` |
| Generate docs | `cargo doc -p ovsm --open` |
| Run benchmark | `cargo bench -p ovsm` |
| Check coverage | `cargo tarpaulin -p ovsm` |
| Execute .ovsm file | `cargo run -- ovsm run file.ovsm` |

---

### 9.2 File Locations

| What | Where |
|------|-------|
| Lexer | `crates/ovsm/src/lexer/` |
| Parser | `crates/ovsm/src/parser/` |
| Runtime | `crates/ovsm/src/runtime/` |
| Tools | `crates/ovsm/src/tools/stdlib/` |
| Tests | `crates/ovsm/tests/` |
| Benchmarks | `crates/ovsm/benches/` |
| Examples | `crates/ovsm/examples/` |

---

**END OF MODULE STRUCTURE GUIDE**
