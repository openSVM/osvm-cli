//! Error types for OVSM interpreter

use thiserror::Error;

/// OVSM interpreter errors
#[derive(Error, Debug, Clone)]
pub enum Error {
    // Parse errors
    /// Syntax error encountered during parsing
    ///
    /// **Triggered by:** Invalid OVSM syntax (missing keywords, malformed expressions)
    /// **Example:** `IF $x > 10` (missing THEN keyword)
    #[error("Syntax error at line {line}, column {col}: {message}")]
    SyntaxError {
        /// Line number where error occurred
        line: usize,
        /// Column number where error occurred
        col: usize,
        /// Error description
        message: String,
    },

    /// General parse error
    #[error("Parse error: {0}")]
    ParseError(String),

    /// Unexpected end of file during parsing
    #[error("Unexpected end of file")]
    UnexpectedEof,

    /// Unexpected token encountered during parsing
    #[error("Unexpected token: expected {expected}, got {got}")]
    UnexpectedToken {
        /// Expected token description
        expected: String,
        /// Actual token received
        got: String,
    },

    // Runtime errors
    /// Reference to undefined variable
    ///
    /// **Triggered by:** Using a variable before assignment
    /// **Example:** `RETURN $x` (when $x was never assigned)
    /// **Prevention:** Always initialize variables before use
    #[error("Undefined variable: {name}")]
    UndefinedVariable {
        /// Variable name
        name: String,
    },

    /// Reference to undefined tool
    #[error("Undefined tool: {name}")]
    UndefinedTool {
        /// Tool name
        name: String,
    },

    /// Type mismatch error
    ///
    /// **Triggered by:** Operation expecting one type but receiving another
    /// **Example:** `$x = "hello" + 5` (string + number), `IF "text" THEN` (string as boolean)
    /// **Prevention:** Ensure type compatibility in operations and conversions
    #[error("Type error: expected {expected}, got {got}")]
    TypeError {
        /// Expected type
        expected: String,
        /// Actual type
        got: String,
    },

    /// Attempt to reassign a constant value
    #[error("Cannot reassign constant: {name}")]
    ConstantReassignment {
        /// Constant name
        name: String,
    },

    /// Division by zero error
    ///
    /// **Triggered by:** Dividing by zero or taking modulo of zero
    /// **Example:** `$x = 10 / 0`, `$y = 5 % 0`
    /// **Prevention:** Check denominator before division operations
    #[error("Division by zero")]
    DivisionByZero,

    /// Assertion failed
    ///
    /// **Triggered by:** Assertion condition evaluated to false
    /// **Example:** `(assert (> x 0) "x must be positive")` when x <= 0
    /// **Prevention:** Ensure preconditions are met before assertions
    #[error("Assertion failed: {message}")]
    AssertionFailed {
        /// Assertion failure message
        message: String,
    },

    /// Array index out of bounds
    ///
    /// **Triggered by:** Accessing array element beyond valid range
    /// **Example:** `$arr = [1, 2, 3]; $x = $arr[5]` (index 5 when length is 3)
    /// **Prevention:** Check array length before indexing, use LEN() tool
    #[error("Index out of bounds: {index} for array of length {length}")]
    IndexOutOfBounds {
        /// Requested index
        index: usize,
        /// Array length
        length: usize,
    },

    /// Invalid operation for given types
    ///
    /// **Triggered by:** Performing unsupported operations on incompatible types
    /// **Example:** `[1,2] * "text"` (array multiplication with string)
    /// **Prevention:** Verify operand types support the intended operation
    #[error("Invalid operation: {op} on types {left_type} and {right_type}")]
    InvalidOperation {
        /// Operation name
        op: String,
        /// Left operand type
        left_type: String,
        /// Right operand type
        right_type: String,
    },

    /// Invalid comparison between incompatible types
    #[error("Invalid comparison between types {left_type} and {right_type}")]
    InvalidComparison {
        /// Left operand type
        left_type: String,
        /// Right operand type
        right_type: String,
    },

    /// Attempt to call a non-callable value
    #[error("Value is not callable: {type_name}")]
    NotCallable {
        /// Type of non-callable value
        type_name: String,
    },

    /// Operation on empty collection that requires elements
    #[error("Empty collection for operation: {operation}")]
    EmptyCollection {
        /// Operation name
        operation: String,
    },

    // Tool errors
    /// Tool execution failed
    ///
    /// **Triggered by:** Runtime failure during tool execution
    /// **Example:** `POW(2, 1000000)` (computation overflow), `SQRT(-5)` (negative sqrt)
    /// **Recovery:** Classified as Recoverable - may be retried with different inputs
    #[error("Tool execution failed: {tool} - {reason}")]
    ToolExecutionError {
        /// Tool name
        tool: String,
        /// Failure reason
        reason: String,
    },

    /// Invalid arguments provided to tool
    #[error("Invalid arguments for tool {tool}: {reason}")]
    InvalidArguments {
        /// Tool name
        tool: String,
        /// Reason for invalidity
        reason: String,
    },

    /// Tool not yet implemented
    #[error("Tool not implemented: {tool}")]
    NotImplemented {
        /// Tool name
        tool: String,
    },

    // Resource errors
    /// Operation timed out
    #[error("Timeout after {0:?}")]
    Timeout(std::time::Duration),

    /// Memory limit exceeded
    #[error("Out of memory (limit: {0} bytes)")]
    OutOfMemory(usize),

    /// Execution limit exceeded
    #[error("Execution limit exceeded (max: {limit} operations)")]
    ExecutionLimitExceeded {
        /// Maximum allowed operations
        limit: usize,
    },

    /// Too many loop iterations
    #[error("Too many iterations (limit: {limit})")]
    TooManyIterations {
        /// Maximum allowed iterations
        limit: usize,
    },

    /// Circuit breaker is open preventing operations
    #[error("Circuit breaker is open")]
    CircuitOpen,

    // Control flow
    /// Break statement used outside of loop
    #[error("Break statement outside loop")]
    InvalidBreak,

    /// Continue statement used outside of loop
    #[error("Continue statement outside loop")]
    InvalidContinue,

    // External errors
    /// RPC call failed
    #[error("RPC error: {message}")]
    RpcError {
        /// Error message
        message: String,
    },

    /// AI service error
    #[error("AI service error: {message}")]
    AiServiceError {
        /// Error message
        message: String,
    },

    /// Network operation failed
    #[error("Network error: {message}")]
    NetworkError {
        /// Error message
        message: String,
    },

    /// No parallel tasks completed successfully
    #[error("No tasks completed")]
    NoTasksCompleted,

    // User-defined
    /// User-defined error
    #[error("User error: {0}")]
    UserError(String),
}

/// Error severity classification
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorSeverity {
    /// Fatal error that cannot be recovered from
    Fatal,
    /// Recoverable error that may be retried
    Recoverable,
    /// Warning that doesn't prevent execution
    Warning,
}

impl Error {
    /// Classify error severity
    pub fn classify(&self) -> ErrorSeverity {
        match self {
            Error::DivisionByZero => ErrorSeverity::Fatal,
            Error::AssertionFailed { .. } => ErrorSeverity::Fatal,
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
