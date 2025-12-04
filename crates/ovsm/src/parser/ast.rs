use serde::{Deserialize, Serialize};
use std::fmt;

/// Complete OVSM program
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    /// Program metadata from header comments
    pub metadata: ProgramMetadata,
    /// Top-level statements in the program
    pub statements: Vec<Statement>,
}

/// Program metadata (from header comments)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct ProgramMetadata {
    /// Estimated execution time
    pub time_estimate: Option<String>,
    /// Estimated cost
    pub cost_estimate: Option<String>,
    /// Confidence level (0-100)
    pub confidence: Option<u8>,
    /// List of available tools
    pub available_tools: Vec<String>,
}

/// Statements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    /// Variable assignment: $x = expr
    Assignment {
        /// Name of the variable to assign to
        name: String,
        /// Expression value to assign
        value: Expression,
    },

    /// Constant definition: CONST NAME = value
    ConstantDef {
        /// Name of the constant (uppercase)
        name: String,
        /// Constant value expression
        value: Expression,
    },

    /// If statement
    If {
        /// Condition expression to evaluate
        condition: Expression,
        /// Statements to execute if condition is true
        then_branch: Vec<Statement>,
        /// Optional statements to execute if condition is false
        else_branch: Option<Vec<Statement>>,
    },

    /// While loop
    While {
        /// Loop condition expression
        condition: Expression,
        /// Statements to execute in loop body
        body: Vec<Statement>,
    },

    /// For loop: FOR $item IN collection
    For {
        /// Loop variable name
        variable: String,
        /// Expression to iterate over
        iterable: Expression,
        /// Statements to execute in loop body
        body: Vec<Statement>,
    },

    /// Break statement
    Break {
        /// Optional condition for conditional break
        condition: Option<Expression>,
    },

    /// Continue statement
    Continue {
        /// Optional condition for conditional continue
        condition: Option<Expression>,
    },

    /// Return statement
    Return {
        /// Optional value to return
        value: Option<Expression>,
    },

    /// Expression statement
    Expression(Expression),

    /// Try-catch block
    Try {
        /// Statements to execute in try block
        body: Vec<Statement>,
        /// Catch clauses to handle errors
        catch_clauses: Vec<CatchClause>,
    },

    /// Parallel execution block
    Parallel {
        /// Tasks to execute in parallel
        tasks: Vec<Statement>,
    },

    /// Wait strategy
    WaitStrategy(WaitStrategy),

    /// Decision point
    Decision {
        /// Description of the decision being made
        description: String,
        /// Branches representing different decision paths
        branches: Vec<DecisionBranch>,
    },

    /// Guard clause
    Guard {
        /// Guard condition expression
        condition: Expression,
        /// Statements to execute if guard fails
        else_body: Vec<Statement>,
    },
}

/// Expressions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    // Literals
    /// Integer literal expression
    IntLiteral(i64),
    /// Floating-point literal expression
    FloatLiteral(f64),
    /// String literal expression
    StringLiteral(String),
    /// Boolean literal expression
    BoolLiteral(bool),
    /// Null literal expression
    NullLiteral,

    // Collections
    /// Array literal expression
    ArrayLiteral(Vec<Expression>),
    /// Object literal expression with key-value pairs
    ObjectLiteral(Vec<(String, Expression)>),
    /// Range expression [start..end]
    Range {
        /// Start expression of the range
        start: Box<Expression>,
        /// End expression of the range (exclusive)
        end: Box<Expression>,
    },

    // Variables
    /// Variable reference expression
    Variable(String),

    // Binary operations
    /// Binary operation expression
    Binary {
        /// Binary operator to apply
        op: BinaryOp,
        /// Left operand expression
        left: Box<Expression>,
        /// Right operand expression
        right: Box<Expression>,
    },

    // Unary operations
    /// Unary operation expression
    Unary {
        /// Unary operator to apply
        op: UnaryOp,
        /// Operand expression
        operand: Box<Expression>,
    },

    // Ternary operator: condition ? then : else
    /// Ternary conditional expression
    Ternary {
        /// Condition expression to evaluate
        condition: Box<Expression>,
        /// Expression to evaluate if condition is true
        then_expr: Box<Expression>,
        /// Expression to evaluate if condition is false
        else_expr: Box<Expression>,
    },

    /// Tool or function call
    ToolCall {
        /// Name of the tool/function to call
        name: String,
        /// Arguments to pass to the tool
        args: Vec<Argument>,
    },

    /// Lambda function expression (x => x * 2)
    Lambda {
        /// Parameter names for the lambda
        params: Vec<String>,
        /// Body expression of the lambda
        body: Box<Expression>,
    },

    /// Field access expression (object.field)
    FieldAccess {
        /// Object being accessed
        object: Box<Expression>,
        /// Name of the field to access
        field: String,
    },

    /// Index access expression (array\[index\])
    IndexAccess {
        /// Array or collection being indexed
        array: Box<Expression>,
        /// Index expression
        index: Box<Expression>,
    },

    /// Grouping expression with parentheses (expr)
    Grouping(Box<Expression>),

    // Macro system (Common Lisp)
    /// Quasiquote expression `(...)
    /// Template for code generation in macros
    Quasiquote(Box<Expression>),

    /// Unquote expression ,(...)
    /// Evaluate and splice value into quasiquote template
    Unquote(Box<Expression>),

    /// Unquote-splice expression ,@(...)
    /// Evaluate list and splice elements into quasiquote template
    UnquoteSplice(Box<Expression>),

    /// Loop expression (Common Lisp loop macro)
    /// Declarative iteration with accumulation
    Loop(Box<LoopData>),

    /// Catch expression - establishes an exit point
    /// (catch 'tag body...)
    Catch {
        /// Tag to identify this catch point (usually a symbol)
        tag: Box<Expression>,
        /// Body expressions to evaluate
        body: Vec<Expression>,
    },

    /// Throw expression - non-local exit to matching catch
    /// (throw 'tag value)
    Throw {
        /// Tag to identify target catch point
        tag: Box<Expression>,
        /// Value to return from the catch
        value: Box<Expression>,
    },

    /// Destructuring-bind expression - pattern matching for variable binding
    /// (destructuring-bind (a b c) [1 2 3] body...)
    DestructuringBind {
        /// Pattern to match against (list of variable names, possibly nested)
        pattern: Box<Expression>,
        /// Value expression to destructure
        value: Box<Expression>,
        /// Body expressions evaluated with pattern bindings in scope
        body: Vec<Expression>,
    },

    // ============================================================================
    // Type System Expressions
    // ============================================================================
    /// Type annotation expression (: expr type)
    /// Explicitly annotates an expression with a type for bidirectional type checking
    /// Example: (: 42 u64) or (: (lambda (x) x) (-> i64 i64))
    TypeAnnotation {
        /// The expression being annotated
        expr: Box<Expression>,
        /// The type annotation (parsed as an expression for flexibility)
        /// Simple: Variable("u64"), Complex: ToolCall for generic types
        type_expr: Box<Expression>,
    },

    /// Typed lambda expression (lambda ((x : T) (y : U)) -> R body)
    /// Lambda with explicit parameter types and optional return type
    TypedLambda {
        /// Parameters with optional type annotations: (name, Option<type_expr>)
        typed_params: Vec<(String, Option<Box<Expression>>)>,
        /// Optional return type annotation
        return_type: Option<Box<Expression>>,
        /// Body expression of the lambda
        body: Box<Expression>,
    },

    // ============================================================================
    // Refinement Types
    // ============================================================================
    /// Refinement type expression: {x : T | predicate}
    /// Constrains a type with a predicate that must hold for all values.
    /// Example: {i : u64 | i < 10} - unsigned int less than 10
    /// Example: {arr : [u64; n] | len(arr) > 0} - non-empty array
    RefinedTypeExpr {
        /// Bound variable name (e.g., "x" in {x : u64 | x < 10})
        var: String,
        /// Base type expression (e.g., "u64")
        base_type: Box<Expression>,
        /// Predicate expression that must hold (e.g., "x < 10")
        predicate: Box<Expression>,
    },
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    /// Addition operator (+)
    Add,
    /// Subtraction operator (-)
    Sub,
    /// Multiplication operator (*)
    Mul,
    /// Division operator (/)
    Div,
    /// Modulo operator (%)
    Mod,
    /// Power operator (**)
    Pow,

    // Comparison
    /// Equality operator (==)
    Eq,
    /// Inequality operator (!=)
    NotEq,
    /// Less than operator (<)
    Lt,
    /// Greater than operator (>)
    Gt,
    /// Less than or equal operator (<=)
    LtEq,
    /// Greater than or equal operator (>=)
    GtEq,

    // Logical
    /// Logical AND operator
    And,
    /// Logical OR operator
    Or,

    // Special
    /// Membership test operator (IN)
    In,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    /// Negation operator (-x)
    Neg,
    /// Logical NOT operator (!x)
    Not,
}

/// Function/tool call argument
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Argument {
    /// Argument name (None for positional, Some for named)
    pub name: Option<String>,
    /// Argument value expression
    pub value: Expression,
}

impl Argument {
    /// Creates a positional argument
    pub fn positional(value: Expression) -> Self {
        Argument { name: None, value }
    }

    /// Creates a named argument
    pub fn named(name: String, value: Expression) -> Self {
        Argument {
            name: Some(name),
            value,
        }
    }
}

/// Decision branch
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DecisionBranch {
    /// Branch name
    pub name: String,
    /// Branch condition expression
    pub condition: Expression,
    /// Statements to execute if condition is true
    pub body: Vec<Statement>,
}

/// Catch clause
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CatchClause {
    /// Type of error to catch (None catches all)
    pub error_type: Option<ErrorType>,
    /// Statements to execute when error is caught
    pub body: Vec<Statement>,
}

/// Error types for catch
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ErrorType {
    /// Fatal error that cannot be recovered
    Fatal,
    /// Recoverable error
    Recoverable,
    /// Warning level error
    Warning,
}

/// Wait strategies for parallel execution
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WaitStrategy {
    /// Wait for all tasks to complete
    WaitAll,
    /// Wait for any task to complete
    WaitAny,
    /// Race tasks against each other
    Race,
}

/// Operator precedence levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    /// No precedence
    None,
    /// Assignment operators (=)
    Assignment,
    /// Logical OR operator
    Or,
    /// Logical AND operator
    And,
    /// Equality operators (==, !=)
    Equality,
    /// Comparison operators (<, >, <=, >=)
    Comparison,
    /// Addition and subtraction (+, -)
    Term,
    /// Multiplication, division, modulo (*, /, %)
    Factor,
    /// Unary operators (!, -)
    Unary,
    /// Power operator (**)
    Power,
    /// Call operators (., (), [])
    Call,
    /// Primary expressions (literals, identifiers)
    Primary,
}

impl BinaryOp {
    /// Returns the precedence level of this binary operator
    pub fn precedence(&self) -> Precedence {
        match self {
            BinaryOp::Or => Precedence::Or,
            BinaryOp::And => Precedence::And,
            BinaryOp::Eq | BinaryOp::NotEq => Precedence::Equality,
            BinaryOp::Lt | BinaryOp::Gt | BinaryOp::LtEq | BinaryOp::GtEq => Precedence::Comparison,
            BinaryOp::Add | BinaryOp::Sub => Precedence::Term,
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => Precedence::Factor,
            BinaryOp::Pow => Precedence::Power,
            BinaryOp::In => Precedence::Comparison,
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
            BinaryOp::Pow => write!(f, "**"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::LtEq => write!(f, "<="),
            BinaryOp::GtEq => write!(f, ">="),
            BinaryOp::And => write!(f, "AND"),
            BinaryOp::Or => write!(f, "OR"),
            BinaryOp::In => write!(f, "IN"),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_precedence_ordering() {
        assert!(BinaryOp::Add.precedence() > BinaryOp::And.precedence());
        assert!(BinaryOp::Mul.precedence() > BinaryOp::Add.precedence());
        assert!(BinaryOp::Pow.precedence() > BinaryOp::Mul.precedence());
    }

    #[test]
    fn test_argument_construction() {
        let pos_arg = Argument::positional(Expression::IntLiteral(42));
        assert!(pos_arg.name.is_none());
        assert_eq!(pos_arg.value, Expression::IntLiteral(42));

        let named_arg = Argument::named("x".to_string(), Expression::IntLiteral(42));
        assert_eq!(named_arg.name, Some("x".to_string()));
    }
}

// ============================================================================
// Loop Macro Structures (Common Lisp)
// ============================================================================

/// Loop expression data (Common Lisp loop macro)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LoopData {
    /// Iteration clause (required - defines what to iterate over)
    pub iteration: IterationClause,
    /// Optional accumulation clause (sum/collect/count)
    pub accumulation: Option<AccumulationClause>,
    /// Optional condition clause (when/unless)
    pub condition: Option<ConditionClause>,
    /// Optional early exit clause (while/until)
    pub early_exit: Option<ExitClause>,
    /// Body expressions (for 'do' clause)
    pub body: Vec<Expression>,
}

/// Iteration clause for loop
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IterationClause {
    /// Numeric iteration: (loop for i from 1 to 10 ...)
    Numeric {
        /// Iteration variable name
        var: String,
        /// Starting value expression
        from: Box<Expression>,
        /// Ending value expression
        to: Box<Expression>,
        /// Optional step value (default 1)
        by: Option<Box<Expression>>,
        /// True if using 'downfrom' instead of 'from'
        downfrom: bool,
        /// True if using 'below' instead of 'to' (exclusive upper bound)
        below: bool,
    },
    /// Collection iteration: (loop for item in collection ...)
    Collection {
        /// Iteration variable name
        var: String,
        /// Collection expression to iterate over
        collection: Box<Expression>,
    },
}

/// Accumulation clause for loop
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AccumulationClause {
    /// Sum accumulation: (loop ... sum expr)
    Sum(Option<Box<Expression>>),
    /// Collect accumulation: (loop ... collect expr)
    Collect(Option<Box<Expression>>),
    /// Count accumulation: (loop ... count expr)
    Count(Option<Box<Expression>>),
}

/// Condition clause for loop
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ConditionClause {
    /// When condition: (loop ... when test ...)
    When(Box<Expression>),
    /// Unless condition: (loop ... unless test ...)
    Unless(Box<Expression>),
}

/// Early exit clause for loop
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExitClause {
    /// While clause: continue while condition is true
    While(Box<Expression>),
    /// Until clause: continue until condition becomes true
    Until(Box<Expression>),
}
