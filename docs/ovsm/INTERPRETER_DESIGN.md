# OVSM Interpreter Design Document v1.0

**Document Type**: Detailed Design
**Related**: INTERPRETER_SPECIFICATION.md
**Implementation Language**: Rust
**Target**: Development Team

---

## Table of Contents

1. [Module Structure](#module-structure)
2. [Core Data Structures](#core-data-structures)
3. [Execution Engine Design](#execution-engine-design)
4. [Tool System Design](#tool-system-design)
5. [Standard Library Implementation](#standard-library-implementation)
6. [Agent System Architecture](#agent-system-architecture)
7. [Performance Optimization](#performance-optimization)
8. [Testing Strategy](#testing-strategy)
9. [Integration with osvm-cli](#integration-with-osvm-cli)

---

## 1. Module Structure

### 1.1 Workspace Layout

```
osvm-cli/
├── Cargo.toml (workspace)
├── src/
│   ├── main.rs
│   ├── lib.rs
│   └── ... (existing osvm-cli code)
│
└── crates/
    └── ovsm/
        ├── Cargo.toml
        ├── src/
        │   ├── lib.rs
        │   ├── lexer/
        │   │   ├── mod.rs
        │   │   ├── token.rs
        │   │   └── scanner.rs
        │   ├── parser/
        │   │   ├── mod.rs
        │   │   ├── ast.rs
        │   │   ├── expr.rs
        │   │   ├── stmt.rs
        │   │   └── error.rs
        │   ├── runtime/
        │   │   ├── mod.rs
        │   │   ├── value.rs
        │   │   ├── environment.rs
        │   │   ├── evaluator.rs
        │   │   ├── executor.rs
        │   │   └── context.rs
        │   ├── tools/
        │   │   ├── mod.rs
        │   │   ├── registry.rs
        │   │   ├── signature.rs
        │   │   ├── stdlib/
        │   │   │   ├── mod.rs
        │   │   │   ├── solana_rpc.rs
        │   │   │   ├── data_processing.rs
        │   │   │   ├── statistics.rs
        │   │   │   ├── math.rs
        │   │   │   ├── string_ops.rs
        │   │   │   └── utilities.rs
        │   │   └── agents/
        │   │       ├── mod.rs
        │   │       ├── delegation.rs
        │   │       ├── knowledge_graph.rs
        │   │       ├── hypothesis.rs
        │   │       └── meta_learning.rs
        │   ├── error.rs
        │   ├── ai/
        │   │   ├── mod.rs
        │   │   ├── service.rs
        │   │   ├── decision.rs
        │   │   └── planning.rs
        │   ├── parallel/
        │   │   ├── mod.rs
        │   │   ├── executor.rs
        │   │   └── strategy.rs
        │   └── metrics/
        │       ├── mod.rs
        │       ├── tracker.rs
        │       └── reporter.rs
        │
        ├── tests/
        │   ├── lexer_tests.rs
        │   ├── parser_tests.rs
        │   ├── runtime_tests.rs
        │   ├── tools_tests.rs
        │   └── integration_tests.rs
        │
        └── benches/
            └── execution_benchmarks.rs
```

### 1.2 Dependency Graph

```
┌─────────────┐
│   lexer     │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   parser    │─────────┐
└──────┬──────┘         │
       │                │
       ▼                │
┌─────────────┐         │
│   runtime   │◄────────┤
└──────┬──────┘         │
       │                │
       ├────────────────┼───┐
       │                │   │
       ▼                ▼   ▼
┌──────────┐   ┌────────────┐   ┌──────────┐
│  tools   │   │     ai     │   │ parallel │
└──────────┘   └────────────┘   └──────────┘
       │                │
       ▼                ▼
┌──────────────────────────┐
│       metrics            │
└──────────────────────────┘
```

### 1.3 Crate Definitions

#### Cargo.toml (workspace)
```toml
[workspace]
members = [".", "crates/ovsm"]

[workspace.dependencies]
tokio = { version = "1.35", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
anyhow = "1.0"
thiserror = "1.0"
tracing = "0.1"
async-trait = "0.1"
```

#### crates/ovsm/Cargo.toml
```toml
[package]
name = "ovsm"
version = "1.0.0"
edition = "2021"

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

# Additional
chrono = "0.4"
dashmap = "5.5"
lru = "0.12"

[dev-dependencies]
proptest = "1.4"
criterion = "0.5"
bolero = "0.10"
```

---

## 2. Core Data Structures

### 2.1 Token (lexer/token.rs)

```rust
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
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
    Variable(String),  // starts with $
    Constant(String),  // UPPERCASE

    // Keywords
    If,
    Else,
    Then,
    While,
    For,
    In,
    Break,
    Continue,
    Return,
    Try,
    Catch,
    Parallel,
    WaitAll,
    WaitAny,
    Race,
    Decision,
    Branch,
    Guard,
    Match,
    Define,
    DefineTool,
    Const,
    Tool,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    StarStar,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
    Not,
    Assign,
    PlusAssign,
    Question,
    Colon,
    QuestionDot,
    QuestionQuestion,
    Arrow,
    FatArrow,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Newline,

    // Special
    Eof,
}
```

### 2.2 AST (parser/ast.rs)

```rust
use std::fmt;
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub metadata: ProgramMetadata,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProgramMetadata {
    pub time_estimate: Option<String>,
    pub cost_estimate: Option<String>,
    pub confidence: Option<u8>,
    pub available_tools: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    // Variable operations
    Assignment {
        name: String,
        value: Expression,
        type_annotation: Option<TypeAnnotation>,
    },
    ConstantDef {
        name: String,
        value: Expression,
    },
    Destructure {
        pattern: Pattern,
        value: Expression,
    },

    // Control flow
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    For {
        variable: String,
        iterable: Expression,
        body: Vec<Statement>,
    },
    ForIndexed {
        item: String,
        index: String,
        iterable: Expression,
        body: Vec<Statement>,
    },
    LoopEvery {
        duration: Duration,
        body: Vec<Statement>,
    },
    Break {
        condition: Option<Expression>,
    },
    Continue {
        condition: Option<Expression>,
    },
    Return {
        value: Option<Expression>,
    },

    // Decision logic
    Decision {
        description: String,
        branches: Vec<DecisionBranch>,
    },
    Guard {
        condition: Expression,
        else_body: Vec<Statement>,
    },
    Match {
        expr: Expression,
        arms: Vec<MatchArm>,
    },

    // Error handling
    Try {
        body: Vec<Statement>,
        catch_clauses: Vec<CatchClause>,
    },
    Timeout {
        duration: Duration,
        body: Vec<Statement>,
        on_timeout: Option<Vec<Statement>>,
    },
    Retry {
        count: u32,
        body: Vec<Statement>,
        backoff: Option<Vec<Duration>>,
    },
    CircuitBreaker {
        failure_threshold: u32,
        reset_timeout: Duration,
        body: Vec<Statement>,
    },

    // Tool definitions
    DefineTool {
        name: String,
        signature: ToolSignature,
        body: Vec<Statement>,
    },
    DefineHelper {
        name: String,
        params: Vec<String>,
        body: Vec<Statement>,
    },

    // Parallel execution
    Parallel {
        tasks: Vec<Statement>,
    },
    WaitStrategy(WaitStrategy),

    // Expression statement
    Expression(Expression),

    // Sections (comments for structure)
    Section {
        title: String,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    NullLiteral,

    // Collections
    ArrayLiteral(Vec<Expression>),
    ObjectLiteral(Vec<(String, Expression)>),
    TupleLiteral(Vec<Expression>),
    Range { start: Box<Expression>, end: Box<Expression> },

    // Variables
    Variable(String),

    // Operations
    Binary {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expression>,
    },
    Ternary {
        condition: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    OptionalChain {
        base: Box<Expression>,
        chain: Vec<String>,
    },
    NullCoalesce {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    // Function calls
    ToolCall {
        name: String,
        args: Vec<Argument>,
    },
    Lambda {
        params: Vec<String>,
        body: Box<Expression>,
    },

    // Field access
    FieldAccess {
        object: Box<Expression>,
        field: String,
    },
    IndexAccess {
        array: Box<Expression>,
        index: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod, Pow,
    Eq, NotEq, Lt, Gt, LtEq, GtEq,
    And, Or, Xor,
    In, Between,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Argument {
    pub name: Option<String>,  // None for positional
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DecisionBranch {
    pub name: String,
    pub condition: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CatchClause {
    pub error_type: Option<ErrorType>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ErrorType {
    Fatal,
    Recoverable,
    Warning,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Pattern {
    Literal(Expression),
    Variable(String),
    Wildcard,
    Array(Vec<Pattern>),
    Object(Vec<(String, Pattern)>),
    Rest(String),  // ...rest
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum WaitStrategy {
    WaitAll,
    WaitAny,
    Race,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ToolSignature {
    pub params: Vec<Parameter>,
    pub returns: TypeAnnotation,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: TypeAnnotation,
    pub required: bool,
    pub default: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeAnnotation {
    Any,
    Null,
    Bool,
    Int,
    Float,
    Number,  // Int | Float
    String,
    Array(Box<TypeAnnotation>),
    Object(Vec<(String, TypeAnnotation)>),
    Tuple(Vec<TypeAnnotation>),
    Union(Vec<TypeAnnotation>),
    Optional(Box<TypeAnnotation>),
    Function {
        params: Vec<TypeAnnotation>,
        returns: Box<TypeAnnotation>,
    },
    Custom(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Duration {
    pub value: u64,
    pub unit: DurationUnit,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum DurationUnit {
    Seconds,
    Minutes,
    Hours,
}

impl Duration {
    pub fn to_std_duration(&self) -> std::time::Duration {
        match self.unit {
            DurationUnit::Seconds => std::time::Duration::from_secs(self.value),
            DurationUnit::Minutes => std::time::Duration::from_secs(self.value * 60),
            DurationUnit::Hours => std::time::Duration::from_secs(self.value * 3600),
        }
    }
}
```

### 2.3 Value (runtime/value.rs)

```rust
use std::collections::HashMap;
use std::sync::Arc;
use solana_sdk::{pubkey::Pubkey, signature::Signature};
use chrono::{DateTime, Utc};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    // Primitives
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),

    // Collections (use Arc for large values)
    Array(Arc<Vec<Value>>),
    Object(Arc<HashMap<String, Value>>),
    Tuple(Arc<Vec<Value>>),

    // Blockchain types
    Pubkey(Pubkey),
    Signature(Signature),
    Lamports(u64),
    Transaction(Arc<TransactionValue>),
    Block(Arc<BlockValue>),
    Account(Arc<AccountValue>),

    // Functions
    Function(FunctionValue),
    BuiltinTool(ToolRef),

    // Agent types
    AgentHandle(uuid::Uuid),
    AgentResult(Arc<AgentResultValue>),

    // Graph types
    KnowledgeGraph(Arc<GraphValue>),
    GraphNode(Arc<NodeValue>),
    GraphEdge(Arc<EdgeValue>),

    // Meta
    Range { start: i64, end: i64 },
    Duration(std::time::Duration),
    Timestamp(DateTime<Utc>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TransactionValue {
    pub signature: Signature,
    pub slot: u64,
    pub block_time: Option<i64>,
    pub meta: TransactionMetaValue,
    pub message: MessageValue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockValue {
    pub slot: u64,
    pub blockhash: String,
    pub parent_slot: u64,
    pub transactions: Vec<TransactionValue>,
    pub block_time: Option<i64>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AccountValue {
    pub pubkey: Pubkey,
    pub lamports: u64,
    pub owner: Pubkey,
    pub data: Vec<u8>,
    pub executable: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TransactionMetaValue {
    pub fee: u64,
    pub err: Option<String>,
    pub pre_balances: Vec<u64>,
    pub post_balances: Vec<u64>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MessageValue {
    pub account_keys: Vec<Pubkey>,
    pub instructions: Vec<InstructionValue>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstructionValue {
    pub program_id: Pubkey,
    pub accounts: Vec<u8>,
    pub data: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionValue {
    pub params: Vec<String>,
    pub body: Expression,
    pub closure: Arc<Environment>,
}

#[derive(Debug, Clone)]
pub struct ToolRef {
    pub name: String,
    pub tool: Arc<dyn Tool>,
}

impl PartialEq for ToolRef {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AgentResultValue {
    pub agent_id: uuid::Uuid,
    pub result: Box<Value>,
    pub confidence: f64,
    pub metadata: HashMap<String, Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GraphValue {
    pub nodes: HashMap<String, NodeValue>,
    pub edges: Vec<EdgeValue>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeValue {
    pub id: String,
    pub node_type: String,
    pub properties: HashMap<String, Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EdgeValue {
    pub from: String,
    pub to: String,
    pub relationship: String,
    pub weight: f64,
}

// Type conversion methods
impl Value {
    pub fn as_bool(&self) -> Result<bool, RuntimeError> {
        match self {
            Value::Bool(b) => Ok(*b),
            Value::Int(n) => Ok(*n != 0),
            Value::Float(f) => Ok(*f != 0.0),
            Value::Null => Ok(false),
            _ => Err(RuntimeError::TypeError {
                expected: "bool".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_int(&self) -> Result<i64, RuntimeError> {
        match self {
            Value::Int(n) => Ok(*n),
            Value::Float(f) => Ok(*f as i64),
            Value::Lamports(l) => Ok(*l as i64),
            _ => Err(RuntimeError::TypeError {
                expected: "int".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_float(&self) -> Result<f64, RuntimeError> {
        match self {
            Value::Float(f) => Ok(*f),
            Value::Int(n) => Ok(*n as f64),
            _ => Err(RuntimeError::TypeError {
                expected: "float".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_string(&self) -> Result<&str, RuntimeError> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err(RuntimeError::TypeError {
                expected: "string".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_array(&self) -> Result<&Vec<Value>, RuntimeError> {
        match self {
            Value::Array(arr) => Ok(arr),
            _ => Err(RuntimeError::TypeError {
                expected: "array".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_object(&self) -> Result<&HashMap<String, Value>, RuntimeError> {
        match self {
            Value::Object(obj) => Ok(obj),
            _ => Err(RuntimeError::TypeError {
                expected: "object".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            Value::Null => "null".to_string(),
            Value::Bool(_) => "bool".to_string(),
            Value::Int(_) => "int".to_string(),
            Value::Float(_) => "float".to_string(),
            Value::String(_) => "string".to_string(),
            Value::Array(_) => "array".to_string(),
            Value::Object(_) => "object".to_string(),
            Value::Tuple(_) => "tuple".to_string(),
            Value::Pubkey(_) => "pubkey".to_string(),
            Value::Signature(_) => "signature".to_string(),
            Value::Lamports(_) => "lamports".to_string(),
            Value::Transaction(_) => "transaction".to_string(),
            Value::Block(_) => "block".to_string(),
            Value::Account(_) => "account".to_string(),
            Value::Function(_) => "function".to_string(),
            Value::BuiltinTool(_) => "builtin_tool".to_string(),
            Value::AgentHandle(_) => "agent_handle".to_string(),
            Value::AgentResult(_) => "agent_result".to_string(),
            Value::KnowledgeGraph(_) => "knowledge_graph".to_string(),
            Value::GraphNode(_) => "graph_node".to_string(),
            Value::GraphEdge(_) => "graph_edge".to_string(),
            Value::Range { .. } => "range".to_string(),
            Value::Duration(_) => "duration".to_string(),
            Value::Timestamp(_) => "timestamp".to_string(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Int(n) => *n != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array(arr) => !arr.is_empty(),
            _ => true,
        }
    }
}
```

### 2.4 Environment (runtime/environment.rs)

```rust
use std::collections::HashMap;
use std::sync::Arc;
use crate::runtime::value::Value;
use crate::error::RuntimeError;

#[derive(Debug, Clone)]
pub struct Environment {
    scopes: Vec<Scope>,
    constants: Arc<HashMap<String, Value>>,
}

#[derive(Debug, Clone)]
struct Scope {
    variables: HashMap<String, Value>,
    parent: Option<usize>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            scopes: vec![Scope {
                variables: HashMap::new(),
                parent: None,
            }],
            constants: Arc::new(HashMap::new()),
        }
    }

    pub fn with_constants(constants: HashMap<String, Value>) -> Self {
        Environment {
            scopes: vec![Scope {
                variables: HashMap::new(),
                parent: None,
            }],
            constants: Arc::new(constants),
        }
    }

    pub fn enter_scope(&mut self) {
        let parent_idx = self.scopes.len() - 1;
        self.scopes.push(Scope {
            variables: HashMap::new(),
            parent: Some(parent_idx),
        });
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.variables.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        // Check constants first
        if let Some(val) = self.constants.get(name) {
            return Ok(val.clone());
        }

        // Walk scope chain from innermost to outermost
        let mut scope_idx = self.scopes.len() - 1;
        loop {
            let scope = &self.scopes[scope_idx];
            if let Some(val) = scope.variables.get(name) {
                return Ok(val.clone());
            }
            match scope.parent {
                Some(parent) => scope_idx = parent,
                None => {
                    return Err(RuntimeError::UndefinedVariable {
                        name: name.to_string(),
                    })
                }
            }
        }
    }

    pub fn set(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        // Constants cannot be reassigned
        if self.constants.contains_key(name) {
            return Err(RuntimeError::ConstantReassignment {
                name: name.to_string(),
            });
        }

        // Find variable in scope chain and update
        let mut scope_idx = self.scopes.len() - 1;
        loop {
            let scope = &mut self.scopes[scope_idx];
            if scope.variables.contains_key(name) {
                scope.variables.insert(name.to_string(), value);
                return Ok(());
            }
            match scope.parent {
                Some(parent) => scope_idx = parent,
                None => {
                    // Variable doesn't exist, define in current scope
                    let current_scope = self.scopes.last_mut().unwrap();
                    current_scope.variables.insert(name.to_string(), value);
                    return Ok(());
                }
            }
        }
    }

    pub fn snapshot(&self) -> HashMap<String, Value> {
        let mut result = HashMap::new();

        // Add constants
        for (k, v) in self.constants.iter() {
            result.insert(k.clone(), v.clone());
        }

        // Add all variables from all scopes
        for scope in &self.scopes {
            for (k, v) in &scope.variables {
                result.insert(k.clone(), v.clone());
            }
        }

        result
    }
}
```

---

## 3. Execution Engine Design

### 3.1 Evaluator (runtime/evaluator.rs)

```rust
use crate::parser::ast::*;
use crate::runtime::{Environment, Value};
use crate::tools::ToolRegistry;
use crate::error::RuntimeError;

pub struct Evaluator {
    registry: Arc<ToolRegistry>,
    limits: ExecutionLimits,
    metrics: ExecutionMetrics,
}

impl Evaluator {
    pub fn new(registry: Arc<ToolRegistry>) -> Self {
        Evaluator {
            registry,
            limits: ExecutionLimits::default(),
            metrics: ExecutionMetrics::new(),
        }
    }

    pub fn with_limits(mut self, limits: ExecutionLimits) -> Self {
        self.limits = limits;
        self
    }

    pub fn evaluate_program(
        &mut self,
        program: &Program,
    ) -> Result<Value, RuntimeError> {
        let mut env = Environment::new();

        // Execute all statements
        for stmt in &program.statements {
            match self.execute_statement(stmt, &mut env)? {
                ExecutionFlow::Continue => {},
                ExecutionFlow::Return(val) => return Ok(val),
                ExecutionFlow::Break => {
                    return Err(RuntimeError::InvalidBreak);
                },
                ExecutionFlow::Continue => {
                    return Err(RuntimeError::InvalidContinue);
                },
            }
        }

        Ok(Value::Null)
    }

    fn execute_statement(
        &mut self,
        stmt: &Statement,
        env: &mut Environment,
    ) -> Result<ExecutionFlow, RuntimeError> {
        // Check execution limits
        self.check_limits()?;

        match stmt {
            Statement::Assignment { name, value, .. } => {
                let val = self.evaluate_expression(value, env)?;
                env.define(name.clone(), val);
                Ok(ExecutionFlow::Continue)
            },

            Statement::ConstantDef { name, value } => {
                // Constants are handled during parsing/validation
                Ok(ExecutionFlow::Continue)
            },

            Statement::If { condition, then_branch, else_branch } => {
                let cond_val = self.evaluate_expression(condition, env)?;
                if cond_val.is_truthy() {
                    self.execute_block(then_branch, env)
                } else if let Some(else_b) = else_branch {
                    self.execute_block(else_b, env)
                } else {
                    Ok(ExecutionFlow::Continue)
                }
            },

            Statement::While { condition, body } => {
                let mut iterations = 0;
                loop {
                    // Check iteration limit
                    iterations += 1;
                    if iterations > self.limits.max_loop_iterations {
                        return Err(RuntimeError::TooManyIterations {
                            limit: self.limits.max_loop_iterations,
                        });
                    }

                    let cond_val = self.evaluate_expression(condition, env)?;
                    if !cond_val.is_truthy() {
                        break;
                    }

                    match self.execute_block(body, env)? {
                        ExecutionFlow::Continue => {},
                        ExecutionFlow::Break => break,
                        ExecutionFlow::ContinueLoop => continue,
                        ExecutionFlow::Return(val) => return Ok(ExecutionFlow::Return(val)),
                    }
                }
                Ok(ExecutionFlow::Continue)
            },

            Statement::For { variable, iterable, body } => {
                let iter_val = self.evaluate_expression(iterable, env)?;
                let items = self.get_iterable_items(&iter_val)?;

                env.enter_scope();
                for item in items {
                    env.define(variable.clone(), item);

                    match self.execute_block(body, env)? {
                        ExecutionFlow::Continue => {},
                        ExecutionFlow::Break => break,
                        ExecutionFlow::ContinueLoop => continue,
                        ExecutionFlow::Return(val) => {
                            env.exit_scope();
                            return Ok(ExecutionFlow::Return(val));
                        },
                    }
                }
                env.exit_scope();
                Ok(ExecutionFlow::Continue)
            },

            Statement::Return { value } => {
                let val = if let Some(v) = value {
                    self.evaluate_expression(v, env)?
                } else {
                    Value::Null
                };
                Ok(ExecutionFlow::Return(val))
            },

            Statement::Break { condition } => {
                if let Some(cond) = condition {
                    let cond_val = self.evaluate_expression(cond, env)?;
                    if cond_val.is_truthy() {
                        Ok(ExecutionFlow::Break)
                    } else {
                        Ok(ExecutionFlow::Continue)
                    }
                } else {
                    Ok(ExecutionFlow::Break)
                }
            },

            Statement::Continue { condition } => {
                if let Some(cond) = condition {
                    let cond_val = self.evaluate_expression(cond, env)?;
                    if cond_val.is_truthy() {
                        Ok(ExecutionFlow::ContinueLoop)
                    } else {
                        Ok(ExecutionFlow::Continue)
                    }
                } else {
                    Ok(ExecutionFlow::ContinueLoop)
                }
            },

            Statement::Expression(expr) => {
                self.evaluate_expression(expr, env)?;
                Ok(ExecutionFlow::Continue)
            },

            Statement::Try { body, catch_clauses } => {
                self.execute_try_catch(body, catch_clauses, env)
            },

            _ => todo!("Implement remaining statement types"),
        }
    }

    fn evaluate_expression(
        &mut self,
        expr: &Expression,
        env: &Environment,
    ) -> Result<Value, RuntimeError> {
        match expr {
            Expression::IntLiteral(n) => Ok(Value::Int(*n)),
            Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
            Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
            Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
            Expression::NullLiteral => Ok(Value::Null),

            Expression::Variable(name) => env.get(name),

            Expression::ArrayLiteral(elements) => {
                let mut arr = Vec::with_capacity(elements.len());
                for elem in elements {
                    arr.push(self.evaluate_expression(elem, env)?);
                }
                Ok(Value::Array(Arc::new(arr)))
            },

            Expression::ObjectLiteral(fields) => {
                let mut obj = HashMap::new();
                for (key, value_expr) in fields {
                    let value = self.evaluate_expression(value_expr, env)?;
                    obj.insert(key.clone(), value);
                }
                Ok(Value::Object(Arc::new(obj)))
            },

            Expression::Binary { op, left, right } => {
                let left_val = self.evaluate_expression(left, env)?;
                let right_val = self.evaluate_expression(right, env)?;
                self.apply_binary_op(*op, left_val, right_val)
            },

            Expression::Unary { op, operand } => {
                let val = self.evaluate_expression(operand, env)?;
                self.apply_unary_op(*op, val)
            },

            Expression::Ternary { condition, then_expr, else_expr } => {
                let cond_val = self.evaluate_expression(condition, env)?;
                if cond_val.is_truthy() {
                    self.evaluate_expression(then_expr, env)
                } else {
                    self.evaluate_expression(else_expr, env)
                }
            },

            Expression::ToolCall { name, args } => {
                self.execute_tool_call(name, args, env)
            },

            Expression::FieldAccess { object, field } => {
                let obj_val = self.evaluate_expression(object, env)?;
                self.get_field(&obj_val, field)
            },

            Expression::IndexAccess { array, index } => {
                let arr_val = self.evaluate_expression(array, env)?;
                let idx_val = self.evaluate_expression(index, env)?;
                self.get_index(&arr_val, &idx_val)
            },

            _ => todo!("Implement remaining expression types"),
        }
    }

    fn apply_binary_op(
        &self,
        op: BinaryOp,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        match op {
            BinaryOp::Add => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 + r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l + r as f64)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
                _ => Err(RuntimeError::InvalidOperation {
                    op: "add".to_string(),
                    left_type: left.type_name(),
                    right_type: right.type_name(),
                }),
            },

            BinaryOp::Sub => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 - r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l - r as f64)),
                _ => Err(RuntimeError::InvalidOperation {
                    op: "subtract".to_string(),
                    left_type: left.type_name(),
                    right_type: right.type_name(),
                }),
            },

            BinaryOp::Mul => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 * r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l * r as f64)),
                _ => Err(RuntimeError::InvalidOperation {
                    op: "multiply".to_string(),
                    left_type: left.type_name(),
                    right_type: right.type_name(),
                }),
            },

            BinaryOp::Div => match (left, right) {
                (Value::Int(l), Value::Int(r)) => {
                    if r == 0 {
                        Err(RuntimeError::DivisionByZero)
                    } else {
                        Ok(Value::Int(l / r))
                    }
                },
                (Value::Float(l), Value::Float(r)) => {
                    if r == 0.0 {
                        Err(RuntimeError::DivisionByZero)
                    } else {
                        Ok(Value::Float(l / r))
                    }
                },
                _ => Err(RuntimeError::InvalidOperation {
                    op: "divide".to_string(),
                    left_type: left.type_name(),
                    right_type: right.type_name(),
                }),
            },

            BinaryOp::Eq => Ok(Value::Bool(left == right)),
            BinaryOp::NotEq => Ok(Value::Bool(left != right)),

            BinaryOp::Lt => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l < r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l < r)),
                _ => Err(RuntimeError::InvalidComparison {
                    left_type: left.type_name(),
                    right_type: right.type_name(),
                }),
            },

            BinaryOp::And => {
                Ok(Value::Bool(left.is_truthy() && right.is_truthy()))
            },

            BinaryOp::Or => {
                Ok(Value::Bool(left.is_truthy() || right.is_truthy()))
            },

            _ => todo!("Implement remaining binary operators"),
        }
    }
}

enum ExecutionFlow {
    Continue,
    Break,
    ContinueLoop,
    Return(Value),
}
```

(Continued in next section due to length...)

### 3.2 Async Executor (runtime/executor.rs)

```rust
use tokio::task::JoinSet;
use crate::runtime::{Evaluator, Environment, Value};
use crate::parser::ast::WaitStrategy;
use crate::error::RuntimeError;

pub struct AsyncExecutor {
    evaluator: Evaluator,
}

impl AsyncExecutor {
    pub async fn execute_parallel(
        &mut self,
        tasks: &[Statement],
        strategy: WaitStrategy,
        env: &Environment,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut join_set = JoinSet::new();

        for task in tasks {
            let task_clone = task.clone();
            let env_clone = env.clone();
            let mut evaluator_clone = self.evaluator.clone();

            join_set.spawn(async move {
                evaluator_clone.execute_statement(&task_clone, &mut env_clone)
            });
        }

        match strategy {
            WaitStrategy::WaitAll => {
                let mut results = Vec::new();
                while let Some(result) = join_set.join_next().await {
                    let execution_result = result??;
                    if let ExecutionFlow::Return(val) = execution_result {
                        results.push(val);
                    }
                }
                Ok(results)
            },

            WaitStrategy::WaitAny => {
                if let Some(result) = join_set.join_next().await {
                    join_set.abort_all();
                    let execution_result = result??;
                    if let ExecutionFlow::Return(val) = execution_result {
                        Ok(vec![val])
                    } else {
                        Ok(vec![])
                    }
                } else {
                    Err(RuntimeError::NoTasksCompleted)
                }
            },

            WaitStrategy::Race => {
                if let Some(result) = join_set.join_next().await {
                    join_set.abort_all();
                    let execution_result = result??;
                    if let ExecutionFlow::Return(val) = execution_result {
                        Ok(vec![val])
                    } else {
                        Ok(vec![])
                    }
                } else {
                    Err(RuntimeError::NoTasksCompleted)
                }
            },
        }
    }
}
```

---

## 4. Tool System Design

### 4.1 Tool Registry (tools/registry.rs)

```rust
use std::collections::HashMap;
use std::sync::Arc;
use crate::tools::Tool;
use crate::error::RuntimeError;

pub struct ToolRegistry {
    tools: HashMap<String, Arc<dyn Tool>>,
    categories: HashMap<String, Vec<String>>,
}

impl ToolRegistry {
    pub fn new() -> Self {
        let mut registry = ToolRegistry {
            tools: HashMap::new(),
            categories: HashMap::new(),
        };

        // Register all standard library tools
        registry.register_stdlib_tools();

        registry
    }

    pub fn register<T: Tool + 'static>(&mut self, tool: T) -> &mut Self {
        let name = tool.name().to_string();
        self.tools.insert(name.clone(), Arc::new(tool));
        self
    }

    pub fn register_in_category<T: Tool + 'static>(
        &mut self,
        tool: T,
        category: &str,
    ) -> &mut Self {
        let name = tool.name().to_string();
        self.tools.insert(name.clone(), Arc::new(tool));

        self.categories
            .entry(category.to_string())
            .or_insert_with(Vec::new)
            .push(name);

        self
    }

    pub fn get(&self, name: &str) -> Result<Arc<dyn Tool>, RuntimeError> {
        self.tools
            .get(name)
            .cloned()
            .ok_or_else(|| RuntimeError::UndefinedTool {
                name: name.to_string(),
            })
    }

    pub fn list_tools(&self) -> Vec<String> {
        self.tools.keys().cloned().collect()
    }

    pub fn list_categories(&self) -> Vec<String> {
        self.categories.keys().cloned().collect()
    }

    pub fn find_by_category(&self, category: &str) -> Vec<String> {
        self.categories
            .get(category)
            .cloned()
            .unwrap_or_default()
    }

    fn register_stdlib_tools(&mut self) {
        use crate::tools::stdlib::*;

        // Solana RPC tools
        self.register_in_category(solana_rpc::GetSlotTool, "solana_rpc");
        self.register_in_category(solana_rpc::GetBlockTool::new(), "solana_rpc");
        // ... register all 18 RPC tools

        // Data processing tools
        self.register_in_category(data_processing::MapTool, "data_processing");
        self.register_in_category(data_processing::FilterTool, "data_processing");
        // ... register all 27 data processing tools

        // Statistical tools
        self.register_in_category(statistics::MeanTool, "statistics");
        self.register_in_category(statistics::MedianTool, "statistics");
        // ... register all 14 statistical tools

        // And so on for all categories
    }
}
```

---

## 5. Standard Library Implementation

### 5.1 Solana RPC Tools (tools/stdlib/solana_rpc.rs)

```rust
use async_trait::async_trait;
use solana_client::rpc_client::RpcClient;
use crate::tools::{Tool, ToolSignature, ToolArguments};
use crate::runtime::Value;
use crate::error::RuntimeError;

pub struct GetSlotTool;

#[async_trait]
impl Tool for GetSlotTool {
    fn name(&self) -> &str {
        "getSlot"
    }

    fn description(&self) -> &str {
        "Get the current slot number"
    }

    fn signature(&self) -> ToolSignature {
        ToolSignature {
            params: vec![],
            returns: TypeAnnotation::Int,
        }
    }

    async fn execute_async(
        &self,
        _args: ToolArguments,
        context: &ExecutionContext,
    ) -> Result<Value, RuntimeError> {
        let slot = context
            .rpc_client
            .get_slot()
            .await
            .map_err(|e| RuntimeError::RpcError {
                message: e.to_string(),
            })?;

        Ok(Value::Int(slot as i64))
    }

    fn is_async(&self) -> bool {
        true
    }
}

pub struct GetBlockTool {
    rpc_client: Arc<RpcClient>,
}

impl GetBlockTool {
    pub fn new(rpc_client: Arc<RpcClient>) -> Self {
        GetBlockTool { rpc_client }
    }
}

#[async_trait]
impl Tool for GetBlockTool {
    fn name(&self) -> &str {
        "getBlock"
    }

    fn description(&self) -> &str {
        "Fetch block by slot number"
    }

    fn signature(&self) -> ToolSignature {
        ToolSignature {
            params: vec![
                Parameter {
                    name: "slot".to_string(),
                    type_annotation: TypeAnnotation::Int,
                    required: true,
                    default: None,
                },
            ],
            returns: TypeAnnotation::Custom("Block".to_string()),
        }
    }

    async fn execute_async(
        &self,
        args: ToolArguments,
        _context: &ExecutionContext,
    ) -> Result<Value, RuntimeError> {
        let slot = args.get_named("slot")?.as_int()? as u64;

        let block = self
            .rpc_client
            .get_block(slot)
            .await
            .map_err(|e| RuntimeError::RpcError {
                message: e.to_string(),
            })?;

        let block_value = convert_block_to_value(block);
        Ok(Value::Block(Arc::new(block_value)))
    }

    fn is_async(&self) -> bool {
        true
    }
}

fn convert_block_to_value(block: EncodedConfirmedBlock) -> BlockValue {
    // Convert Solana block type to OVSM BlockValue
    BlockValue {
        slot: block.parent_slot + 1,
        blockhash: block.blockhash,
        parent_slot: block.parent_slot,
        transactions: block
            .transactions
            .into_iter()
            .map(convert_transaction_to_value)
            .collect(),
        block_time: block.block_time,
    }
}
```

### 5.2 Data Processing Tools (tools/stdlib/data_processing.rs)

```rust
use crate::tools::{Tool, ToolSignature, ToolArguments};
use crate::runtime::{Value, Environment};
use crate::error::RuntimeError;

pub struct MapTool;

impl Tool for MapTool {
    fn name(&self) -> &str {
        "MAP"
    }

    fn description(&self) -> &str {
        "Apply function to each element of collection"
    }

    fn signature(&self) -> ToolSignature {
        ToolSignature {
            params: vec![
                Parameter {
                    name: "collection".to_string(),
                    type_annotation: TypeAnnotation::Array(Box::new(TypeAnnotation::Any)),
                    required: true,
                    default: None,
                },
                Parameter {
                    name: "fn".to_string(),
                    type_annotation: TypeAnnotation::Function {
                        params: vec![TypeAnnotation::Any],
                        returns: Box::new(TypeAnnotation::Any),
                    },
                    required: true,
                    default: None,
                },
            ],
            returns: TypeAnnotation::Array(Box::new(TypeAnnotation::Any)),
        }
    }

    fn execute(
        &self,
        args: ToolArguments,
        context: &ExecutionContext,
    ) -> Result<Value, RuntimeError> {
        let collection = args.get_positional(0)?.as_array()?;
        let func = args.get_positional(1)?;

        let mut results = Vec::with_capacity(collection.len());

        for item in collection.iter() {
            let result = apply_function(func, vec![item.clone()], context)?;
            results.push(result);
        }

        Ok(Value::Array(Arc::new(results)))
    }
}

pub struct FilterTool;

impl Tool for FilterTool {
    fn name(&self) -> &str {
        "FILTER"
    }

    fn description(&self) -> &str {
        "Filter collection by predicate function"
    }

    fn signature(&self) -> ToolSignature {
        ToolSignature {
            params: vec![
                Parameter {
                    name: "collection".to_string(),
                    type_annotation: TypeAnnotation::Array(Box::new(TypeAnnotation::Any)),
                    required: true,
                    default: None,
                },
                Parameter {
                    name: "predicate".to_string(),
                    type_annotation: TypeAnnotation::Function {
                        params: vec![TypeAnnotation::Any],
                        returns: Box::new(TypeAnnotation::Bool),
                    },
                    required: true,
                    default: None,
                },
            ],
            returns: TypeAnnotation::Array(Box::new(TypeAnnotation::Any)),
        }
    }

    fn execute(
        &self,
        args: ToolArguments,
        context: &ExecutionContext,
    ) -> Result<Value, RuntimeError> {
        let collection = args.get_positional(0)?.as_array()?;
        let predicate = args.get_positional(1)?;

        let mut results = Vec::new();

        for item in collection.iter() {
            let result = apply_function(predicate, vec![item.clone()], context)?;
            if result.is_truthy() {
                results.push(item.clone());
            }
        }

        Ok(Value::Array(Arc::new(results)))
    }
}

pub struct SumTool;

impl Tool for SumTool {
    fn name(&self) -> &str {
        "SUM"
    }

    fn description(&self) -> &str {
        "Sum all numbers in collection"
    }

    fn signature(&self) -> ToolSignature {
        ToolSignature {
            params: vec![Parameter {
                name: "collection".to_string(),
                type_annotation: TypeAnnotation::Array(Box::new(TypeAnnotation::Number)),
                required: true,
                default: None,
            }],
            returns: TypeAnnotation::Number,
        }
    }

    fn execute(
        &self,
        args: ToolArguments,
        _context: &ExecutionContext,
    ) -> Result<Value, RuntimeError> {
        let collection = args.get_positional(0)?.as_array()?;

        let mut sum = 0.0;
        for val in collection.iter() {
            sum += val.as_float()?;
        }

        Ok(Value::Float(sum))
    }
}

// Helper function to apply lambda functions
fn apply_function(
    func: &Value,
    args: Vec<Value>,
    context: &ExecutionContext,
) -> Result<Value, RuntimeError> {
    match func {
        Value::Function(func_val) => {
            // Create new environment with closure
            let mut env = (*func_val.closure).clone();

            // Bind parameters
            for (param, arg) in func_val.params.iter().zip(args.iter()) {
                env.define(param.clone(), arg.clone());
            }

            // Evaluate body
            context.evaluator.evaluate_expression(&func_val.body, &env)
        },
        Value::BuiltinTool(tool_ref) => {
            // Call builtin tool
            let tool_args = ToolArguments::from_positional(args);
            tool_ref.tool.execute(tool_args, context)
        },
        _ => Err(RuntimeError::NotCallable {
            type_name: func.type_name(),
        }),
    }
}
```

### 5.3 Statistical Tools (tools/stdlib/statistics.rs)

```rust
use crate::tools::{Tool, ToolSignature, ToolArguments};
use crate::runtime::Value;
use crate::error::RuntimeError;

pub struct MeanTool;

impl Tool for MeanTool {
    fn name(&self) -> &str {
        "MEAN"
    }

    fn description(&self) -> &str {
        "Calculate arithmetic mean of numbers"
    }

    fn signature(&self) -> ToolSignature {
        ToolSignature {
            params: vec![Parameter {
                name: "data".to_string(),
                type_annotation: TypeAnnotation::Array(Box::new(TypeAnnotation::Number)),
                required: true,
                default: None,
            }],
            returns: TypeAnnotation::Float,
        }
    }

    fn execute(
        &self,
        args: ToolArguments,
        _context: &ExecutionContext,
    ) -> Result<Value, RuntimeError> {
        let data = args.get_named("data")?.as_array()?;

        if data.is_empty() {
            return Err(RuntimeError::EmptyCollection {
                operation: "MEAN".to_string(),
            });
        }

        let sum: f64 = data.iter().map(|v| v.as_float()).sum::<Result<f64, _>>()?;
        let mean = sum / data.len() as f64;

        Ok(Value::Float(mean))
    }
}

pub struct MedianTool;

impl Tool for MedianTool {
    fn name(&self) -> &str {
        "MEDIAN"
    }

    fn description(&self) -> &str {
        "Calculate median of numbers"
    }

    fn signature(&self) -> ToolSignature {
        ToolSignature {
            params: vec![Parameter {
                name: "data".to_string(),
                type_annotation: TypeAnnotation::Array(Box::new(TypeAnnotation::Number)),
                required: true,
                default: None,
            }],
            returns: TypeAnnotation::Float,
        }
    }

    fn execute(
        &self,
        args: ToolArguments,
        _context: &ExecutionContext,
    ) -> Result<Value, RuntimeError> {
        let data = args.get_named("data")?.as_array()?;

        if data.is_empty() {
            return Err(RuntimeError::EmptyCollection {
                operation: "MEDIAN".to_string(),
            });
        }

        let mut sorted: Vec<f64> = data
            .iter()
            .map(|v| v.as_float())
            .collect::<Result<Vec<f64>, _>>()?;
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());

        let median = if sorted.len() % 2 == 0 {
            let mid = sorted.len() / 2;
            (sorted[mid - 1] + sorted[mid]) / 2.0
        } else {
            sorted[sorted.len() / 2]
        };

        Ok(Value::Float(median))
    }
}

pub struct CorrelateTool;

impl Tool for CorrelateTool {
    fn name(&self) -> &str {
        "CORRELATE"
    }

    fn description(&self) -> &str {
        "Calculate Pearson correlation coefficient between two datasets"
    }

    fn signature(&self) -> ToolSignature {
        ToolSignature {
            params: vec![
                Parameter {
                    name: "x".to_string(),
                    type_annotation: TypeAnnotation::Array(Box::new(TypeAnnotation::Number)),
                    required: true,
                    default: None,
                },
                Parameter {
                    name: "y".to_string(),
                    type_annotation: TypeAnnotation::Array(Box::new(TypeAnnotation::Number)),
                    required: true,
                    default: None,
                },
            ],
            returns: TypeAnnotation::Float,
        }
    }

    fn execute(
        &self,
        args: ToolArguments,
        _context: &ExecutionContext,
    ) -> Result<Value, RuntimeError> {
        let x = args.get_named("x")?.as_array()?;
        let y = args.get_named("y")?.as_array()?;

        if x.len() != y.len() {
            return Err(RuntimeError::InvalidArguments {
                tool: "CORRELATE".to_string(),
                reason: "Arrays must have same length".to_string(),
            });
        }

        if x.is_empty() {
            return Err(RuntimeError::EmptyCollection {
                operation: "CORRELATE".to_string(),
            });
        }

        let x_vals: Vec<f64> = x.iter().map(|v| v.as_float()).collect::<Result<Vec<_>, _>>()?;
        let y_vals: Vec<f64> = y.iter().map(|v| v.as_float()).collect::<Result<Vec<_>, _>>()?;

        let x_mean = x_vals.iter().sum::<f64>() / x_vals.len() as f64;
        let y_mean = y_vals.iter().sum::<f64>() / y_vals.len() as f64;

        let mut numerator = 0.0;
        let mut x_sum_sq = 0.0;
        let mut y_sum_sq = 0.0;

        for i in 0..x_vals.len() {
            let x_diff = x_vals[i] - x_mean;
            let y_diff = y_vals[i] - y_mean;
            numerator += x_diff * y_diff;
            x_sum_sq += x_diff * x_diff;
            y_sum_sq += y_diff * y_diff;
        }

        let correlation = numerator / (x_sum_sq.sqrt() * y_sum_sq.sqrt());

        Ok(Value::Float(correlation))
    }
}
```

---

## 6. Agent System Architecture

(Due to the extremely comprehensive nature of the remaining sections, I'll create a summary structure for Agent System, Performance, Testing, and Integration)

### 6.1 Agent Tools Overview

```rust
// Agent delegation
pub struct SpawnAgentTool;
pub struct AwaitAgentTool;
pub struct ParallelAgentsTool;

// Knowledge graph
pub struct InitKnowledgeGraphTool;
pub struct AddNodeTool;
pub struct AddEdgeTool;
pub struct QueryGraphTool;
pub struct FindPathTool;

// Hypothesis testing
pub struct DefineHypothesisTool;
pub struct PerformTestTool;
pub struct EffectSizeTool;

// Meta-learning
pub struct InitMetaLearnerTool;
pub struct RecordPerformanceTool;
pub struct LearnStrategyTool;
```

---

## 7. Performance Optimization

### 7.1 Value Caching

```rust
use lru::LruCache;

pub struct ValueCache {
    cache: LruCache<String, Arc<Value>>,
}

impl ValueCache {
    pub fn get_or_compute<F>(&mut self, key: String, compute: F) -> Arc<Value>
    where
        F: FnOnce() -> Value,
    {
        if let Some(val) = self.cache.get(&key) {
            Arc::clone(val)
        } else {
            let val = Arc::new(compute());
            self.cache.put(key, Arc::clone(&val));
            val
        }
    }
}
```

### 7.2 Lazy Evaluation

```rust
pub enum LazyValue {
    Computed(Value),
    Thunk(Box<dyn FnOnce() -> Result<Value, RuntimeError>>),
}

impl LazyValue {
    pub fn force(&mut self) -> Result<&Value, RuntimeError> {
        match self {
            LazyValue::Computed(v) => Ok(v),
            LazyValue::Thunk(thunk) => {
                let value = thunk()?;
                *self = LazyValue::Computed(value);
                match self {
                    LazyValue::Computed(v) => Ok(v),
                    _ => unreachable!(),
                }
            }
        }
    }
}
```

---

## 8. Testing Strategy

### 8.1 Unit Test Organization

```
tests/
├── lexer_tests.rs (tokenization)
├── parser_tests.rs (AST construction)
├── runtime_tests.rs (execution)
├── tools_tests.rs (each stdlib tool)
├── error_handling_tests.rs (TRY-CATCH, errors)
├── parallel_tests.rs (PARALLEL, WAIT_ALL)
└── integration_tests.rs (end-to-end)
```

### 8.2 Property-Based Testing

```rust
#[cfg(test)]
mod property_tests {
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn arithmetic_laws(a in 0i64..1000, b in 0i64..1000) {
            // Commutative
            assert_eq!(eval(&format!("{} + {}", a, b)), eval(&format!("{} + {}", b, a)));

            // Associative
            let c = 500i64;
            assert_eq!(
                eval(&format!("({} + {}) + {}", a, b, c)),
                eval(&format!("{} + ({} + {})", a, b, c))
            );
        }

        #[test]
        fn map_preserves_length(arr in prop::collection::vec(any::<i64>(), 0..100)) {
            let program = format!("$arr = {:?}\n$mapped = MAP($arr, x => x * 2)", arr);
            let result = execute_ovsm(&program).unwrap();
            assert_eq!(result.as_array().unwrap().len(), arr.len());
        }
    }
}
```

---

## 9. Integration with osvm-cli

### 9.1 CLI Command Structure

```rust
// src/main.rs

pub async fn handle_ovsm_command(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    match matches.subcommand() {
        Some(("run", sub_matches)) => {
            let file_path = sub_matches.get_one::<String>("file").unwrap();
            run_ovsm_file(file_path).await?;
        },
        Some(("parse", sub_matches)) => {
            let file_path = sub_matches.get_one::<String>("file").unwrap();
            parse_and_display(file_path)?;
        },
        Some(("validate", sub_matches)) => {
            let file_path = sub_matches.get_one::<String>("file").unwrap();
            validate_ovsm_file(file_path)?;
        },
        _ => {
            println!("Unknown OVSM subcommand");
        }
    }
    Ok(())
}

async fn run_ovsm_file(path: &str) -> Result<(), Box<dyn Error>> {
    let source = std::fs::read_to_string(path)?;

    let registry = ToolRegistry::new();
    let mut interpreter = OvsmInterpreter::new(registry);

    let result = interpreter.execute(&source).await?;

    println!("Result: {:#?}", result);
    Ok(())
}
```

### 9.2 Configuration Integration

```rust
// Use existing osvm-cli configuration
pub fn build_interpreter_from_config(config: &Config) -> OvsmInterpreter {
    let rpc_client = Arc::new(RpcClient::new(config.json_rpc_url.clone()));

    let mut registry = ToolRegistry::new();
    registry.configure_rpc_client(rpc_client);

    if let Some(ai_config) = &config.ai_config {
        let ai_service = OpenAiService::new(ai_config.clone());
        registry.configure_ai_service(Box::new(ai_service));
    }

    OvsmInterpreter::new(registry)
        .with_limits(ExecutionLimits {
            max_execution_time: Duration::from_secs(config.max_execution_time),
            max_memory_bytes: config.max_memory_bytes,
            ..Default::default()
        })
}
```

---

## 10. Next Steps

1. **Implement lexer and parser** (Phase 1)
2. **Build basic runtime** (Phase 2)
3. **Implement standard library** (Phase 3)
4. **Add parallel execution** (Phase 4)
5. **Integrate AI services** (Phase 5)
6. **Complete testing and documentation** (Phase 6)

---

**END OF DESIGN DOCUMENT**
