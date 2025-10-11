use serde::{Deserialize, Serialize};
use std::fmt;

/// Complete OVSM program
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub metadata: ProgramMetadata,
    pub statements: Vec<Statement>,
}

/// Program metadata (from header comments)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct ProgramMetadata {
    pub time_estimate: Option<String>,
    pub cost_estimate: Option<String>,
    pub confidence: Option<u8>,
    pub available_tools: Vec<String>,
}

/// Statements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    /// Variable assignment: $x = expr
    Assignment { name: String, value: Expression },

    /// Constant definition: CONST NAME = value
    ConstantDef { name: String, value: Expression },

    /// If statement
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },

    /// While loop
    While {
        condition: Expression,
        body: Vec<Statement>,
    },

    /// For loop: FOR $item IN collection
    For {
        variable: String,
        iterable: Expression,
        body: Vec<Statement>,
    },

    /// Break statement
    Break { condition: Option<Expression> },

    /// Continue statement
    Continue { condition: Option<Expression> },

    /// Return statement
    Return { value: Option<Expression> },

    /// Expression statement
    Expression(Expression),

    /// Try-catch block
    Try {
        body: Vec<Statement>,
        catch_clauses: Vec<CatchClause>,
    },

    /// Parallel execution block
    Parallel { tasks: Vec<Statement> },

    /// Wait strategy
    WaitStrategy(WaitStrategy),

    /// Decision point
    Decision {
        description: String,
        branches: Vec<DecisionBranch>,
    },

    /// Guard clause
    Guard {
        condition: Expression,
        else_body: Vec<Statement>,
    },
}

/// Expressions
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
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
    },

    // Variables
    Variable(String),

    // Binary operations
    Binary {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },

    // Unary operations
    Unary {
        op: UnaryOp,
        operand: Box<Expression>,
    },

    // Ternary operator: condition ? then : else
    Ternary {
        condition: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },

    // Tool/function call
    ToolCall {
        name: String,
        args: Vec<Argument>,
    },

    // Lambda function: x => x * 2
    Lambda {
        params: Vec<String>,
        body: Box<Expression>,
    },

    // Field access: object.field
    FieldAccess {
        object: Box<Expression>,
        field: String,
    },

    // Index access: array[index]
    IndexAccess {
        array: Box<Expression>,
        index: Box<Expression>,
    },

    // Grouping: (expr)
    Grouping(Box<Expression>),
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    // Comparison
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,

    // Logical
    And,
    Or,

    // Special
    In,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg, // -x
    Not, // !x
}

/// Function/tool call argument
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Argument {
    pub name: Option<String>, // None for positional, Some for named
    pub value: Expression,
}

impl Argument {
    pub fn positional(value: Expression) -> Self {
        Argument { name: None, value }
    }

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
    pub name: String,
    pub condition: Expression,
    pub body: Vec<Statement>,
}

/// Catch clause
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CatchClause {
    pub error_type: Option<ErrorType>,
    pub body: Vec<Statement>,
}

/// Error types for catch
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ErrorType {
    Fatal,
    Recoverable,
    Warning,
}

/// Wait strategies for parallel execution
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WaitStrategy {
    WaitAll,
    WaitAny,
    Race,
}

/// Operator precedence levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    None,
    Assignment, // =
    Or,         // OR
    And,        // AND
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * / %
    Unary,      // ! -
    Power,      // **
    Call,       // . () []
    Primary,
}

impl BinaryOp {
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
