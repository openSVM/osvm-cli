use thiserror::Error;

/// OVSM interpreter errors
#[derive(Error, Debug, Clone)]
pub enum Error {
    // Parse errors
    #[error("Syntax error at line {line}, column {col}: {message}")]
    SyntaxError {
        line: usize,
        col: usize,
        message: String,
    },

    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Unexpected end of file")]
    UnexpectedEof,

    #[error("Unexpected token: expected {expected}, got {got}")]
    UnexpectedToken { expected: String, got: String },

    // Runtime errors
    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String },

    #[error("Undefined tool: {name}")]
    UndefinedTool { name: String },

    #[error("Type error: expected {expected}, got {got}")]
    TypeError { expected: String, got: String },

    #[error("Cannot reassign constant: {name}")]
    ConstantReassignment { name: String },

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Index out of bounds: {index} for array of length {length}")]
    IndexOutOfBounds { index: usize, length: usize },

    #[error("Invalid operation: {op} on types {left_type} and {right_type}")]
    InvalidOperation {
        op: String,
        left_type: String,
        right_type: String,
    },

    #[error("Invalid comparison between types {left_type} and {right_type}")]
    InvalidComparison {
        left_type: String,
        right_type: String,
    },

    #[error("Value is not callable: {type_name}")]
    NotCallable { type_name: String },

    #[error("Empty collection for operation: {operation}")]
    EmptyCollection { operation: String },

    // Tool errors
    #[error("Tool execution failed: {tool} - {reason}")]
    ToolExecutionError { tool: String, reason: String },

    #[error("Invalid arguments for tool {tool}: {reason}")]
    InvalidArguments { tool: String, reason: String },

    #[error("Tool not implemented: {tool}")]
    NotImplemented { tool: String },

    // Resource errors
    #[error("Timeout after {0:?}")]
    Timeout(std::time::Duration),

    #[error("Out of memory (limit: {0} bytes)")]
    OutOfMemory(usize),

    #[error("Execution limit exceeded (max: {limit} operations)")]
    ExecutionLimitExceeded { limit: usize },

    #[error("Too many iterations (limit: {limit})")]
    TooManyIterations { limit: usize },

    #[error("Circuit breaker is open")]
    CircuitOpen,

    // Control flow
    #[error("Break statement outside loop")]
    InvalidBreak,

    #[error("Continue statement outside loop")]
    InvalidContinue,

    // External errors
    #[error("RPC error: {message}")]
    RpcError { message: String },

    #[error("AI service error: {message}")]
    AiServiceError { message: String },

    #[error("Network error: {message}")]
    NetworkError { message: String },

    #[error("No tasks completed")]
    NoTasksCompleted,

    // User-defined
    #[error("User error: {0}")]
    UserError(String),
}

/// Error severity classification
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorSeverity {
    Fatal,
    Recoverable,
    Warning,
}

impl Error {
    /// Classify error severity
    pub fn classify(&self) -> ErrorSeverity {
        match self {
            Error::DivisionByZero => ErrorSeverity::Fatal,
            Error::OutOfMemory(_) => ErrorSeverity::Fatal,
            Error::SyntaxError { .. } => ErrorSeverity::Fatal,
            Error::UnexpectedEof => ErrorSeverity::Fatal,

            Error::ToolExecutionError { .. } => ErrorSeverity::Recoverable,
            Error::RpcError { .. } => ErrorSeverity::Recoverable,
            Error::NetworkError { .. } => ErrorSeverity::Recoverable,
            Error::Timeout(_) => ErrorSeverity::Recoverable,
            Error::AiServiceError { .. } => ErrorSeverity::Recoverable,

            Error::TypeError { .. } => ErrorSeverity::Warning,
            Error::IndexOutOfBounds { .. } => ErrorSeverity::Warning,

            _ => ErrorSeverity::Recoverable,
        }
    }
}

/// Result type for OVSM operations
pub type Result<T> = std::result::Result<T, Error>;
