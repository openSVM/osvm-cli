//! # Refinement Types for OVSM
//!
//! This module implements refinement types - types augmented with predicates
//! that constrain their values. Refinement types enable compile-time verification
//! of array bounds, integer overflow, and other constraints.
//!
//! ## Syntax
//!
//! ```lisp
//! ;; Basic refinement type: {x : base-type | predicate}
//! (define idx : {x : u64 | x < 10} 5)
//!
//! ;; Function with refinement parameter
//! (defn safe-access ((arr : (Array u8 10)) (idx : {i : u64 | i < 10})) -> u8
//!   (get arr idx))
//!
//! ;; Refinement on return type
//! (defn bounded-add ((a : u64) (b : u64)) -> {r : u64 | r >= a}
//!   (+ a b))
//!
//! ;; Range refinement (syntactic sugar)
//! (define x : (u64 0..1000) 42)  ; Equivalent to {x : u64 | 0 <= x && x < 1000}
//! ```
//!
//! ## Verification Strategy
//!
//! Refinement predicates are verified in three stages:
//! 1. **Constant folding**: Predicates on literals are evaluated directly
//! 2. **Flow analysis**: Track refinements through control flow
//! 3. **SMT solving**: For complex predicates, encode as SMT queries (optional)

use super::Type;
use std::collections::HashMap;
use std::fmt;

/// A refinement type: a base type with a predicate constraint
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RefinementType {
    /// The variable name bound in the predicate (e.g., "x" in {x : T | P(x)})
    pub var: String,
    /// The base type being refined
    pub base: Type,
    /// The predicate constraining values (as an AST expression)
    pub predicate: Predicate,
}

/// A predicate expression that constrains refinement types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Predicate {
    /// Always true (no constraint)
    True,
    /// Always false (empty type)
    False,

    /// Comparison: var op const
    Compare {
        /// The comparison operator (e.g., <, <=, ==)
        op: CompareOp,
        /// Left-hand side of the comparison
        left: PredicateExpr,
        /// Right-hand side of the comparison
        right: PredicateExpr,
    },

    /// Logical AND of predicates
    And(Box<Predicate>, Box<Predicate>),

    /// Logical OR of predicates
    Or(Box<Predicate>, Box<Predicate>),

    /// Logical NOT of predicate
    Not(Box<Predicate>),

    /// Implication: P => Q
    Implies(Box<Predicate>, Box<Predicate>),

    /// Opaque predicate that cannot be analyzed (placeholder for complex cases)
    /// The u64 is a unique identifier for this opaque predicate
    Opaque(u64),
}

/// A predicate expression (simpler than full AST expressions)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PredicateExpr {
    /// The refinement variable
    Var,
    /// A constant value
    Const(i64),
    /// Addition
    Add(Box<PredicateExpr>, Box<PredicateExpr>),
    /// Subtraction
    Sub(Box<PredicateExpr>, Box<PredicateExpr>),
    /// Multiplication
    Mul(Box<PredicateExpr>, Box<PredicateExpr>),
    /// Length of array (for dependent bounds)
    Len(String),
    /// Field access
    Field(String, String),
}

/// Comparison operators for predicates
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompareOp {
    /// Less than comparison (<)
    Lt,
    /// Less than or equal comparison (<=)
    LtEq,
    /// Greater than comparison (>)
    Gt,
    /// Greater than or equal comparison (>=)
    GtEq,
    /// Equality comparison (==)
    Eq,
    /// Inequality comparison (!=)
    NotEq,
}

impl RefinementType {
    /// Create a simple bound refinement: {x : T | x < n}
    pub fn bounded_above(base: Type, bound: i64) -> Self {
        RefinementType {
            var: "x".to_string(),
            base,
            predicate: Predicate::Compare {
                op: CompareOp::Lt,
                left: PredicateExpr::Var,
                right: PredicateExpr::Const(bound),
            },
        }
    }

    /// Create a range refinement: {x : T | lo <= x && x < hi}
    pub fn range(base: Type, lo: i64, hi: i64) -> Self {
        RefinementType {
            var: "x".to_string(),
            base,
            predicate: Predicate::And(
                Box::new(Predicate::Compare {
                    op: CompareOp::GtEq,
                    left: PredicateExpr::Var,
                    right: PredicateExpr::Const(lo),
                }),
                Box::new(Predicate::Compare {
                    op: CompareOp::Lt,
                    left: PredicateExpr::Var,
                    right: PredicateExpr::Const(hi),
                }),
            ),
        }
    }

    /// Create a non-negative refinement: {x : T | x >= 0}
    pub fn non_negative(base: Type) -> Self {
        RefinementType {
            var: "x".to_string(),
            base,
            predicate: Predicate::Compare {
                op: CompareOp::GtEq,
                left: PredicateExpr::Var,
                right: PredicateExpr::Const(0),
            },
        }
    }

    /// Create an index refinement: {i : u64 | i < len(arr)}
    pub fn array_index(array_name: &str) -> Self {
        RefinementType {
            var: "i".to_string(),
            base: Type::U64,
            predicate: Predicate::Compare {
                op: CompareOp::Lt,
                left: PredicateExpr::Var,
                right: PredicateExpr::Len(array_name.to_string()),
            },
        }
    }

    /// Create a refinement type from an AST expression predicate.
    ///
    /// This converts the parsed predicate expression into our internal
    /// Predicate representation. For complex predicates that cannot be
    /// analyzed, we create an Opaque predicate with a unique ID.
    ///
    /// # Arguments
    /// * `var` - The bound variable name
    /// * `base` - The base type
    /// * `predicate_expr` - The AST expression for the predicate
    pub fn from_expr(var: String, base: Type, predicate_expr: &crate::parser::Expression) -> Self {
        use crate::parser::Expression;

        // Try to convert the AST expression into our Predicate representation
        let predicate = match predicate_expr {
            // Handle comparison expressions: (< x 10), (>= x 0), etc.
            Expression::ToolCall { name, args } if args.len() == 2 => {
                let op = match name.as_str() {
                    "<" => Some(CompareOp::Lt),
                    "<=" => Some(CompareOp::LtEq),
                    ">" => Some(CompareOp::Gt),
                    ">=" => Some(CompareOp::GtEq),
                    "==" | "=" => Some(CompareOp::Eq),
                    "!=" => Some(CompareOp::NotEq),
                    _ => None,
                };

                if let Some(op) = op {
                    let left = Self::expr_to_predicate_expr(&args[0].value, &var);
                    let right = Self::expr_to_predicate_expr(&args[1].value, &var);
                    if let (Some(l), Some(r)) = (left, right) {
                        Predicate::Compare {
                            op,
                            left: l,
                            right: r,
                        }
                    } else {
                        // Complex expression, use opaque
                        Predicate::Opaque(Self::hash_expr(predicate_expr))
                    }
                } else if name == "and" {
                    // Handle (and p q)
                    let p = Self::from_expr(var.clone(), base.clone(), &args[0].value);
                    let q = Self::from_expr(var.clone(), base.clone(), &args[1].value);
                    Predicate::And(Box::new(p.predicate), Box::new(q.predicate))
                } else if name == "or" {
                    // Handle (or p q)
                    let p = Self::from_expr(var.clone(), base.clone(), &args[0].value);
                    let q = Self::from_expr(var.clone(), base.clone(), &args[1].value);
                    Predicate::Or(Box::new(p.predicate), Box::new(q.predicate))
                } else {
                    // Unknown function, use opaque
                    Predicate::Opaque(Self::hash_expr(predicate_expr))
                }
            }

            Expression::ToolCall { name, args } if name == "not" && args.len() == 1 => {
                let inner = Self::from_expr(var.clone(), base.clone(), &args[0].value);
                Predicate::Not(Box::new(inner.predicate))
            }

            // Binary expression comparison
            Expression::Binary { op, left, right } => {
                use crate::parser::BinaryOp;
                let comp_op = match op {
                    BinaryOp::Lt => Some(CompareOp::Lt),
                    BinaryOp::LtEq => Some(CompareOp::LtEq),
                    BinaryOp::Gt => Some(CompareOp::Gt),
                    BinaryOp::GtEq => Some(CompareOp::GtEq),
                    BinaryOp::Eq => Some(CompareOp::Eq),
                    BinaryOp::NotEq => Some(CompareOp::NotEq),
                    BinaryOp::And => {
                        let p = Self::from_expr(var.clone(), base.clone(), left);
                        let q = Self::from_expr(var.clone(), base.clone(), right);
                        return RefinementType {
                            var,
                            base,
                            predicate: Predicate::And(Box::new(p.predicate), Box::new(q.predicate)),
                        };
                    }
                    BinaryOp::Or => {
                        let p = Self::from_expr(var.clone(), base.clone(), left);
                        let q = Self::from_expr(var.clone(), base.clone(), right);
                        return RefinementType {
                            var,
                            base,
                            predicate: Predicate::Or(Box::new(p.predicate), Box::new(q.predicate)),
                        };
                    }
                    _ => None,
                };

                if let Some(op) = comp_op {
                    let l = Self::expr_to_predicate_expr(left, &var);
                    let r = Self::expr_to_predicate_expr(right, &var);
                    if let (Some(l), Some(r)) = (l, r) {
                        Predicate::Compare {
                            op,
                            left: l,
                            right: r,
                        }
                    } else {
                        Predicate::Opaque(Self::hash_expr(predicate_expr))
                    }
                } else {
                    Predicate::Opaque(Self::hash_expr(predicate_expr))
                }
            }

            // Boolean literals
            Expression::BoolLiteral(true) => Predicate::True,
            Expression::BoolLiteral(false) => Predicate::False,

            // Anything else is opaque
            _ => Predicate::Opaque(Self::hash_expr(predicate_expr)),
        };

        RefinementType {
            var,
            base,
            predicate,
        }
    }

    /// Convert an AST expression to a predicate expression
    fn expr_to_predicate_expr(
        expr: &crate::parser::Expression,
        var: &str,
    ) -> Option<PredicateExpr> {
        use crate::parser::Expression;

        match expr {
            Expression::Variable(name) if name == var => Some(PredicateExpr::Var),
            Expression::IntLiteral(n) => Some(PredicateExpr::Const(*n)),

            Expression::ToolCall { name, args } if args.len() == 2 => {
                let left = Self::expr_to_predicate_expr(&args[0].value, var)?;
                let right = Self::expr_to_predicate_expr(&args[1].value, var)?;
                match name.as_str() {
                    "+" => Some(PredicateExpr::Add(Box::new(left), Box::new(right))),
                    "-" => Some(PredicateExpr::Sub(Box::new(left), Box::new(right))),
                    "*" => Some(PredicateExpr::Mul(Box::new(left), Box::new(right))),
                    _ => None,
                }
            }

            Expression::ToolCall { name, args } if name == "len" && args.len() == 1 => {
                if let Expression::Variable(arr_name) = &args[0].value {
                    Some(PredicateExpr::Len(arr_name.clone()))
                } else {
                    None
                }
            }

            Expression::Binary { op, left, right } => {
                use crate::parser::BinaryOp;
                let l = Self::expr_to_predicate_expr(left, var)?;
                let r = Self::expr_to_predicate_expr(right, var)?;
                match op {
                    BinaryOp::Add => Some(PredicateExpr::Add(Box::new(l), Box::new(r))),
                    BinaryOp::Sub => Some(PredicateExpr::Sub(Box::new(l), Box::new(r))),
                    BinaryOp::Mul => Some(PredicateExpr::Mul(Box::new(l), Box::new(r))),
                    _ => None,
                }
            }

            _ => None,
        }
    }

    /// Generate a hash for an expression (used for opaque predicates)
    fn hash_expr(expr: &crate::parser::Expression) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        format!("{:?}", expr).hash(&mut hasher);
        hasher.finish()
    }
}

impl Predicate {
    /// Evaluate predicate with a concrete value
    pub fn evaluate(&self, value: i64, env: &PredicateEnv) -> Option<bool> {
        match self {
            Predicate::True => Some(true),
            Predicate::False => Some(false),

            Predicate::Compare { op, left, right } => {
                let l = left.evaluate(value, env)?;
                let r = right.evaluate(value, env)?;
                Some(match op {
                    CompareOp::Lt => l < r,
                    CompareOp::LtEq => l <= r,
                    CompareOp::Gt => l > r,
                    CompareOp::GtEq => l >= r,
                    CompareOp::Eq => l == r,
                    CompareOp::NotEq => l != r,
                })
            }

            Predicate::And(p, q) => match (p.evaluate(value, env), q.evaluate(value, env)) {
                (Some(false), _) | (_, Some(false)) => Some(false),
                (Some(true), Some(true)) => Some(true),
                _ => None,
            },

            Predicate::Or(p, q) => match (p.evaluate(value, env), q.evaluate(value, env)) {
                (Some(true), _) | (_, Some(true)) => Some(true),
                (Some(false), Some(false)) => Some(false),
                _ => None,
            },

            Predicate::Not(p) => p.evaluate(value, env).map(|v| !v),

            Predicate::Implies(p, q) => match (p.evaluate(value, env), q.evaluate(value, env)) {
                (Some(false), _) => Some(true),
                (Some(true), Some(q_val)) => Some(q_val),
                _ => None,
            },

            Predicate::Opaque(_) => None, // Cannot evaluate opaque predicates directly
        }
    }

    /// Check if this predicate implies another (sound but incomplete)
    pub fn implies(&self, other: &Predicate) -> ImplicationResult {
        // Simple cases
        if self == other {
            return ImplicationResult::Proven;
        }

        match (self, other) {
            // True implies anything is just whether other is True
            (Predicate::True, Predicate::True) => ImplicationResult::Proven,
            (Predicate::True, _) => ImplicationResult::Unknown,

            // False implies anything
            (Predicate::False, _) => ImplicationResult::Proven,

            // x < n implies x < m when n <= m
            (
                Predicate::Compare {
                    op: CompareOp::Lt,
                    left: l1,
                    right: r1,
                },
                Predicate::Compare {
                    op: CompareOp::Lt,
                    left: l2,
                    right: r2,
                },
            ) if l1 == l2 => {
                if let (PredicateExpr::Const(n), PredicateExpr::Const(m)) = (r1, r2) {
                    if n <= m {
                        return ImplicationResult::Proven;
                    }
                }
                ImplicationResult::Unknown
            }

            // x < n implies x <= m when n <= m
            (
                Predicate::Compare {
                    op: CompareOp::Lt,
                    left: l1,
                    right: r1,
                },
                Predicate::Compare {
                    op: CompareOp::LtEq,
                    left: l2,
                    right: r2,
                },
            ) if l1 == l2 => {
                if let (PredicateExpr::Const(n), PredicateExpr::Const(m)) = (r1, r2) {
                    if n <= m {
                        return ImplicationResult::Proven;
                    }
                }
                ImplicationResult::Unknown
            }

            // P && Q implies P
            (Predicate::And(p, _), other) if p.as_ref() == other => ImplicationResult::Proven,
            (Predicate::And(_, q), other) if q.as_ref() == other => ImplicationResult::Proven,

            // P implies P || Q
            (p, Predicate::Or(q, _)) if p == q.as_ref() => ImplicationResult::Proven,
            (p, Predicate::Or(_, r)) if p == r.as_ref() => ImplicationResult::Proven,

            _ => ImplicationResult::Unknown,
        }
    }
}

impl PredicateExpr {
    /// Evaluate expression with variable value
    pub fn evaluate(&self, var_value: i64, env: &PredicateEnv) -> Option<i64> {
        match self {
            PredicateExpr::Var => Some(var_value),
            PredicateExpr::Const(n) => Some(*n),
            PredicateExpr::Add(l, r) => {
                Some(l.evaluate(var_value, env)? + r.evaluate(var_value, env)?)
            }
            PredicateExpr::Sub(l, r) => {
                Some(l.evaluate(var_value, env)? - r.evaluate(var_value, env)?)
            }
            PredicateExpr::Mul(l, r) => {
                Some(l.evaluate(var_value, env)? * r.evaluate(var_value, env)?)
            }
            PredicateExpr::Len(name) => env.get_length(name),
            PredicateExpr::Field(_, _) => None, // Cannot evaluate fields without runtime
        }
    }
}

/// Result of checking if one predicate implies another
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImplicationResult {
    /// Definitively proven
    Proven,
    /// Definitively disproven
    Disproven,
    /// Cannot determine statically
    Unknown,
}

/// Environment for predicate evaluation (known array lengths, etc.)
#[derive(Debug, Clone, Default)]
pub struct PredicateEnv {
    /// Known array lengths
    lengths: HashMap<String, i64>,
    /// Known constant values
    constants: HashMap<String, i64>,
}

impl PredicateEnv {
    /// Create a new empty predicate environment
    pub fn new() -> Self {
        Self::default()
    }

    /// Set a known array length for use in predicates
    pub fn set_length(&mut self, name: &str, len: i64) {
        self.lengths.insert(name.to_string(), len);
    }

    /// Get a known array length from the environment
    pub fn get_length(&self, name: &str) -> Option<i64> {
        self.lengths.get(name).copied()
    }

    /// Set a constant value for use in predicates
    pub fn set_constant(&mut self, name: &str, value: i64) {
        self.constants.insert(name.to_string(), value);
    }

    /// Get a constant value from the environment
    pub fn get_constant(&self, name: &str) -> Option<i64> {
        self.constants.get(name).copied()
    }
}

/// Refinement type checker
pub struct RefinementChecker {
    /// Current predicate environment
    env: PredicateEnv,
    /// Accumulated proof obligations that couldn't be discharged
    obligations: Vec<ProofObligation>,
    /// Errors encountered during checking
    errors: Vec<RefinementError>,
}

/// A proof obligation that needs to be verified
#[derive(Debug, Clone)]
pub struct ProofObligation {
    /// Description of what's being checked
    pub description: String,
    /// The predicate that needs to hold
    pub predicate: Predicate,
    /// Source location (if available)
    pub location: Option<(usize, usize)>,
}

/// Errors during refinement type checking
#[derive(Debug, Clone)]
pub struct RefinementError {
    /// Error message describing what went wrong
    pub message: String,
    /// Source location (line, column) where the error occurred, if available
    pub location: Option<(usize, usize)>,
}

impl RefinementChecker {
    /// Create a new refinement type checker with an empty environment
    pub fn new() -> Self {
        RefinementChecker {
            env: PredicateEnv::new(),
            obligations: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Check if a value satisfies a refinement type
    pub fn check_value(&mut self, value: i64, refined: &RefinementType) -> bool {
        match refined.predicate.evaluate(value, &self.env) {
            Some(true) => true,
            Some(false) => {
                self.errors.push(RefinementError {
                    message: format!("Value {} does not satisfy refinement {}", value, refined),
                    location: None,
                });
                false
            }
            None => {
                // Cannot evaluate statically - add proof obligation
                self.obligations.push(ProofObligation {
                    description: format!("Check that {} satisfies {}", value, refined.predicate),
                    predicate: refined.predicate.clone(),
                    location: None,
                });
                true // Assume valid unless disproven
            }
        }
    }

    /// Check if one refinement type is a subtype of another
    pub fn subtype(&mut self, sub: &RefinementType, sup: &RefinementType) -> bool {
        // Base types must match (or be subtypes)
        if sub.base != sup.base {
            // Could do numeric widening here
            return false;
        }

        // Check if sub's predicate implies sup's predicate
        match sub.predicate.implies(&sup.predicate) {
            ImplicationResult::Proven => true,
            ImplicationResult::Disproven => {
                self.errors.push(RefinementError {
                    message: format!(
                        "Refinement {} does not imply {}",
                        sub.predicate, sup.predicate
                    ),
                    location: None,
                });
                false
            }
            ImplicationResult::Unknown => {
                self.obligations.push(ProofObligation {
                    description: format!("Check that {} implies {}", sub.predicate, sup.predicate),
                    predicate: Predicate::Implies(
                        Box::new(sub.predicate.clone()),
                        Box::new(sup.predicate.clone()),
                    ),
                    location: None,
                });
                true // Assume valid
            }
        }
    }

    /// Set a known array length in the environment
    pub fn set_array_length(&mut self, name: &str, len: i64) {
        self.env.set_length(name, len);
    }

    /// Get accumulated errors
    pub fn errors(&self) -> &[RefinementError] {
        &self.errors
    }

    /// Get undischarged proof obligations
    pub fn obligations(&self) -> &[ProofObligation] {
        &self.obligations
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

impl Default for RefinementChecker {
    fn default() -> Self {
        Self::new()
    }
}

// Display implementations

impl fmt::Display for RefinementType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{{} : {} | {}}}", self.var, self.base, self.predicate)
    }
}

impl fmt::Display for Predicate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Predicate::True => write!(f, "true"),
            Predicate::False => write!(f, "false"),
            Predicate::Compare { op, left, right } => {
                write!(f, "{} {} {}", left, op, right)
            }
            Predicate::And(p, q) => write!(f, "({} && {})", p, q),
            Predicate::Or(p, q) => write!(f, "({} || {})", p, q),
            Predicate::Not(p) => write!(f, "!{}", p),
            Predicate::Implies(p, q) => write!(f, "({} => {})", p, q),
            Predicate::Opaque(id) => write!(f, "<opaque:{}>", id),
        }
    }
}

impl fmt::Display for PredicateExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PredicateExpr::Var => write!(f, "x"),
            PredicateExpr::Const(n) => write!(f, "{}", n),
            PredicateExpr::Add(l, r) => write!(f, "({} + {})", l, r),
            PredicateExpr::Sub(l, r) => write!(f, "({} - {})", l, r),
            PredicateExpr::Mul(l, r) => write!(f, "({} * {})", l, r),
            PredicateExpr::Len(name) => write!(f, "len({})", name),
            PredicateExpr::Field(obj, field) => write!(f, "{}.{}", obj, field),
        }
    }
}

impl fmt::Display for CompareOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompareOp::Lt => write!(f, "<"),
            CompareOp::LtEq => write!(f, "<="),
            CompareOp::Gt => write!(f, ">"),
            CompareOp::GtEq => write!(f, ">="),
            CompareOp::Eq => write!(f, "=="),
            CompareOp::NotEq => write!(f, "!="),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bounded_above() {
        let refined = RefinementType::bounded_above(Type::U64, 10);
        let env = PredicateEnv::new();

        // 5 < 10 should pass
        assert_eq!(refined.predicate.evaluate(5, &env), Some(true));

        // 10 < 10 should fail
        assert_eq!(refined.predicate.evaluate(10, &env), Some(false));

        // 15 < 10 should fail
        assert_eq!(refined.predicate.evaluate(15, &env), Some(false));
    }

    #[test]
    fn test_range() {
        let refined = RefinementType::range(Type::U64, 5, 10);
        let env = PredicateEnv::new();

        // 5 should pass (5 <= 5 && 5 < 10)
        assert_eq!(refined.predicate.evaluate(5, &env), Some(true));

        // 7 should pass
        assert_eq!(refined.predicate.evaluate(7, &env), Some(true));

        // 4 should fail (4 < 5)
        assert_eq!(refined.predicate.evaluate(4, &env), Some(false));

        // 10 should fail (10 >= 10)
        assert_eq!(refined.predicate.evaluate(10, &env), Some(false));
    }

    #[test]
    fn test_array_index() {
        let refined = RefinementType::array_index("arr");
        let mut env = PredicateEnv::new();
        env.set_length("arr", 10);

        // 0 < 10 should pass
        assert_eq!(refined.predicate.evaluate(0, &env), Some(true));

        // 9 < 10 should pass
        assert_eq!(refined.predicate.evaluate(9, &env), Some(true));

        // 10 < 10 should fail
        assert_eq!(refined.predicate.evaluate(10, &env), Some(false));
    }

    #[test]
    fn test_implication_lt() {
        let p1 = Predicate::Compare {
            op: CompareOp::Lt,
            left: PredicateExpr::Var,
            right: PredicateExpr::Const(5),
        };

        let p2 = Predicate::Compare {
            op: CompareOp::Lt,
            left: PredicateExpr::Var,
            right: PredicateExpr::Const(10),
        };

        // x < 5 implies x < 10
        assert_eq!(p1.implies(&p2), ImplicationResult::Proven);

        // x < 10 does NOT imply x < 5
        assert_eq!(p2.implies(&p1), ImplicationResult::Unknown);
    }

    #[test]
    fn test_checker_value() {
        let mut checker = RefinementChecker::new();
        let refined = RefinementType::bounded_above(Type::U64, 10);

        assert!(checker.check_value(5, &refined));
        assert!(!checker.check_value(15, &refined));
        assert!(checker.has_errors());
    }

    #[test]
    fn test_checker_subtype() {
        let mut checker = RefinementChecker::new();

        let sub = RefinementType::bounded_above(Type::U64, 5);
        let sup = RefinementType::bounded_above(Type::U64, 10);

        // {x : u64 | x < 5} <: {x : u64 | x < 10}
        assert!(checker.subtype(&sub, &sup));

        // NOT: {x : u64 | x < 10} <: {x : u64 | x < 5}
        let mut checker2 = RefinementChecker::new();
        assert!(checker2.subtype(&sup, &sub)); // Will add obligation since it's Unknown
        assert!(!checker2.obligations().is_empty());
    }

    #[test]
    fn test_display() {
        let refined = RefinementType::range(Type::U64, 0, 100);
        let display = format!("{}", refined);
        assert!(display.contains("u64"));
        assert!(display.contains(">="));
        assert!(display.contains("<"));
    }

    #[test]
    fn test_from_expr_simple_comparison() {
        use crate::parser::Argument;
        use crate::parser::Expression;

        // Create AST for (< x 10)
        let predicate = Expression::ToolCall {
            name: "<".to_string(),
            args: vec![
                Argument::positional(Expression::Variable("n".to_string())),
                Argument::positional(Expression::IntLiteral(10)),
            ],
        };

        let refined = RefinementType::from_expr("n".to_string(), Type::U64, &predicate);

        assert_eq!(refined.var, "n");
        assert_eq!(refined.base, Type::U64);

        // Verify the predicate works correctly
        let env = PredicateEnv::new();
        assert_eq!(refined.predicate.evaluate(5, &env), Some(true));
        assert_eq!(refined.predicate.evaluate(15, &env), Some(false));
    }

    #[test]
    fn test_from_expr_and_predicate() {
        use crate::parser::Argument;
        use crate::parser::Expression;

        // Create AST for (and (>= x 0) (< x 100))
        let predicate = Expression::ToolCall {
            name: "and".to_string(),
            args: vec![
                Argument::positional(Expression::ToolCall {
                    name: ">=".to_string(),
                    args: vec![
                        Argument::positional(Expression::Variable("x".to_string())),
                        Argument::positional(Expression::IntLiteral(0)),
                    ],
                }),
                Argument::positional(Expression::ToolCall {
                    name: "<".to_string(),
                    args: vec![
                        Argument::positional(Expression::Variable("x".to_string())),
                        Argument::positional(Expression::IntLiteral(100)),
                    ],
                }),
            ],
        };

        let refined = RefinementType::from_expr("x".to_string(), Type::I64, &predicate);

        let env = PredicateEnv::new();
        // 50 is in range [0, 100)
        assert_eq!(refined.predicate.evaluate(50, &env), Some(true));
        // -1 is not >= 0
        assert_eq!(refined.predicate.evaluate(-1, &env), Some(false));
        // 100 is not < 100
        assert_eq!(refined.predicate.evaluate(100, &env), Some(false));
    }

    #[test]
    fn test_parse_refinement_type_syntax() {
        // Test parsing {x : u64 | (< x 10)}
        use crate::lexer::SExprScanner;
        use crate::parser::SExprParser;

        let source = "{x : u64 | (< x 10)}";
        let mut scanner = SExprScanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let mut parser = SExprParser::new(tokens);
        let program = parser.parse().unwrap();

        // Should parse to a single RefinedTypeExpr statement
        assert_eq!(program.statements.len(), 1);
        if let crate::parser::Statement::Expression(crate::parser::Expression::RefinedTypeExpr {
            var,
            base_type,
            predicate,
        }) = &program.statements[0]
        {
            assert_eq!(var, "x");
            // base_type should be u64 (parsed as Variable)
            if let crate::parser::Expression::Variable(name) = base_type.as_ref() {
                assert_eq!(name, "u64");
            } else {
                panic!("Expected Variable for base type, got {:?}", base_type);
            }
            // predicate should be (< x 10) - which is parsed as a ToolCall with name "<"
            // In OVSM, `(< x 10)` gets parsed with "<" as the operator
            match predicate.as_ref() {
                crate::parser::Expression::ToolCall { name, args } => {
                    assert_eq!(name, "<");
                    assert_eq!(args.len(), 2);
                }
                crate::parser::Expression::Binary { op, .. } => {
                    // Also accept Binary expression for the comparison
                    assert_eq!(*op, crate::parser::BinaryOp::Lt);
                }
                other => {
                    panic!("Expected ToolCall or Binary for predicate, got {:?}", other);
                }
            }
        } else {
            panic!("Expected RefinedTypeExpr, got {:?}", program.statements[0]);
        }
    }
}
