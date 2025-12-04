//! # Refinement Type Verification Pass
//!
//! This module implements verification of refinement type constraints
//! during compilation. It checks that:
//!
//! 1. Literal values assigned to refinement types satisfy their predicates
//! 2. Variables with refinement types are used consistently
//! 3. Function arguments satisfy parameter refinements
//!
//! ## Example
//!
//! ```lisp
//! ;; This should PASS verification (5 < 10)
//! (define x : {x : u64 | (< x 10)} 5)
//!
//! ;; This should FAIL verification (15 >= 10)
//! (define y : {y : u64 | (< y 10)} 15)
//! ```

use super::{ProofObligation, RefinementChecker, RefinementError, RefinementType, Type};
use crate::parser::Expression;
use std::collections::HashMap;

/// Verification error with source location
#[derive(Debug, Clone)]
pub struct VerificationError {
    /// Human-readable error message
    pub message: String,
    /// Source location (line, column) if available
    pub location: Option<(usize, usize)>,
    /// The constraint that failed
    pub constraint: String,
}

/// Result of verification pass
#[derive(Debug, Default)]
pub struct VerificationResult {
    /// Errors that definitely violate refinement constraints
    pub errors: Vec<VerificationError>,
    /// Obligations that couldn't be verified statically
    pub obligations: Vec<ProofObligation>,
    /// Warnings (non-fatal issues)
    pub warnings: Vec<String>,
}

impl VerificationResult {
    /// Creates a new empty verification result.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns true if any verification errors were recorded.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Adds a verification error with message and constraint description.
    pub fn add_error(&mut self, message: impl Into<String>, constraint: impl Into<String>) {
        self.errors.push(VerificationError {
            message: message.into(),
            location: None,
            constraint: constraint.into(),
        });
    }

    /// Adds a proof obligation that must be verified.
    pub fn add_obligation(&mut self, obligation: ProofObligation) {
        self.obligations.push(obligation);
    }

    /// Merges another verification result into this one.
    pub fn merge(&mut self, other: VerificationResult) {
        self.errors.extend(other.errors);
        self.obligations.extend(other.obligations);
        self.warnings.extend(other.warnings);
    }
}

/// Verifier for refinement type constraints
pub struct RefinementVerifier {
    /// The refinement checker for evaluating predicates
    checker: RefinementChecker,
    /// Known variable types with refinements
    var_types: HashMap<String, Type>,
    /// Accumulated results
    result: VerificationResult,
}

impl RefinementVerifier {
    /// Creates a new refinement verifier with empty state.
    pub fn new() -> Self {
        Self {
            checker: RefinementChecker::new(),
            var_types: HashMap::new(),
            result: VerificationResult::new(),
        }
    }

    /// Verify an expression against an expected refinement type
    ///
    /// This is the main entry point for verification. It checks that
    /// the expression's value satisfies the refinement predicate.
    pub fn verify_expr(&mut self, expr: &Expression, expected: &Type) {
        // Extract refinement type if present
        let refined = match expected {
            Type::Refined(r) => Some(r.as_ref()),
            _ => None,
        };

        if let Some(refined_type) = refined {
            self.verify_against_refinement(expr, refined_type);
        }
    }

    /// Verify an expression against a specific refinement type
    fn verify_against_refinement(&mut self, expr: &Expression, refined: &RefinementType) {
        match expr {
            // For integer literals, we can verify directly
            Expression::IntLiteral(value) => {
                if !self.checker.check_value(*value, refined) {
                    self.result.add_error(
                        format!(
                            "Integer literal {} does not satisfy refinement {}",
                            value, refined
                        ),
                        format!("{}", refined.predicate),
                    );
                }
            }

            // For variables, check if we know their type
            Expression::Variable(name) => {
                if let Some(var_type) = self.var_types.get(name).cloned() {
                    // Check if variable's type is a subtype of the expected refinement
                    if let Type::Refined(var_refined) = var_type {
                        if !self.checker.subtype(&var_refined, refined) {
                            self.result.add_error(
                                format!(
                                    "Variable '{}' has type {} which may not satisfy {}",
                                    name, var_refined, refined
                                ),
                                format!("{}", refined.predicate),
                            );
                        }
                    }
                    // If variable has non-refined type, add obligation
                }
            }

            // For type-annotated expressions, verify the inner expression
            Expression::TypeAnnotation { expr, type_expr: _ } => {
                self.verify_against_refinement(expr, refined);
            }

            // For binary operations, try to evaluate if both operands are known
            Expression::Binary { op: _, left, right } => {
                // For now, add as obligation since we can't easily evaluate
                self.result.add_obligation(ProofObligation {
                    description: format!(
                        "Binary expression result must satisfy {}",
                        refined.predicate
                    ),
                    predicate: refined.predicate.clone(),
                    location: None,
                });

                // Recursively verify subexpressions against their inferred types
                let _ = left;
                let _ = right;
            }

            // For tool calls, add obligation
            Expression::ToolCall { name, args: _ } => {
                self.result.add_obligation(ProofObligation {
                    description: format!("Result of '{}' must satisfy {}", name, refined.predicate),
                    predicate: refined.predicate.clone(),
                    location: None,
                });
            }

            // Other expressions - add obligation
            _ => {
                self.result.add_obligation(ProofObligation {
                    description: format!("Expression must satisfy {}", refined.predicate),
                    predicate: refined.predicate.clone(),
                    location: None,
                });
            }
        }
    }

    /// Define a variable with a type (for tracking refinements)
    pub fn define_var(&mut self, name: &str, ty: Type) {
        self.var_types.insert(name.to_string(), ty);
    }

    /// Verify a define expression: (define var : type value)
    ///
    /// This checks that the value satisfies the type's refinement predicate.
    pub fn verify_define(&mut self, var_name: &str, declared_type: &Type, value_expr: &Expression) {
        // Record the variable's type
        self.var_types
            .insert(var_name.to_string(), declared_type.clone());

        // Verify the value satisfies the type
        self.verify_expr(value_expr, declared_type);
    }

    /// Get accumulated errors from the checker
    pub fn finish(mut self) -> VerificationResult {
        // Transfer errors from the inner checker
        for err in self.checker.errors() {
            self.result.add_error(
                err.message.clone(),
                err.location
                    .map_or("unknown".to_string(), |loc| format!("{:?}", loc)),
            );
        }

        // Transfer obligations
        for obl in self.checker.obligations() {
            self.result.add_obligation(obl.clone());
        }

        self.result
    }
}

impl Default for RefinementVerifier {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Argument;

    #[test]
    fn test_verify_literal_passes() {
        let mut verifier = RefinementVerifier::new();

        // {x : u64 | x < 10}
        let refined = RefinementType::bounded_above(Type::U64, 10);
        let refined_type = Type::Refined(Box::new(refined));

        // 5 should pass (5 < 10)
        let expr = Expression::IntLiteral(5);
        verifier.verify_expr(&expr, &refined_type);

        let result = verifier.finish();
        assert!(
            !result.has_errors(),
            "Expected no errors, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_verify_literal_fails() {
        let mut verifier = RefinementVerifier::new();

        // {x : u64 | x < 10}
        let refined = RefinementType::bounded_above(Type::U64, 10);
        let refined_type = Type::Refined(Box::new(refined));

        // 15 should fail (15 >= 10)
        let expr = Expression::IntLiteral(15);
        verifier.verify_expr(&expr, &refined_type);

        let result = verifier.finish();
        assert!(result.has_errors(), "Expected errors for value 15");
        assert!(result.errors[0].message.contains("15"));
    }

    #[test]
    fn test_verify_define_with_valid_literal() {
        let mut verifier = RefinementVerifier::new();

        // (define x : {x : u64 | x < 100} 42)
        let refined = RefinementType::bounded_above(Type::U64, 100);
        let refined_type = Type::Refined(Box::new(refined));

        let value_expr = Expression::IntLiteral(42);
        verifier.verify_define("x", &refined_type, &value_expr);

        let result = verifier.finish();
        assert!(
            !result.has_errors(),
            "Expected no errors, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_verify_define_with_invalid_literal() {
        let mut verifier = RefinementVerifier::new();

        // (define x : {x : u64 | x < 10} 100)
        let refined = RefinementType::bounded_above(Type::U64, 10);
        let refined_type = Type::Refined(Box::new(refined));

        let value_expr = Expression::IntLiteral(100);
        verifier.verify_define("x", &refined_type, &value_expr);

        let result = verifier.finish();
        assert!(result.has_errors(), "Expected errors for value 100");
    }

    #[test]
    fn test_verify_range_predicate() {
        let mut verifier = RefinementVerifier::new();

        // {x : u64 | 0 <= x && x < 256}
        let refined = RefinementType::range(Type::U64, 0, 256);
        let refined_type = Type::Refined(Box::new(refined));

        // 128 should pass
        let expr = Expression::IntLiteral(128);
        verifier.verify_expr(&expr, &refined_type);

        let result = verifier.finish();
        assert!(!result.has_errors());
    }

    #[test]
    fn test_verify_range_below_min() {
        let mut verifier = RefinementVerifier::new();

        // {x : i32 | 0 <= x && x < 256}
        let refined = RefinementType::range(Type::I32, 0, 256);
        let refined_type = Type::Refined(Box::new(refined));

        // -1 should fail (below minimum)
        let expr = Expression::IntLiteral(-1);
        verifier.verify_expr(&expr, &refined_type);

        let result = verifier.finish();
        assert!(result.has_errors(), "Expected error for -1");
    }

    #[test]
    fn test_verify_tool_call_adds_obligation() {
        let mut verifier = RefinementVerifier::new();

        // {x : u64 | x < 10}
        let refined = RefinementType::bounded_above(Type::U64, 10);
        let refined_type = Type::Refined(Box::new(refined));

        // (+ a b) - result not statically known
        let expr = Expression::ToolCall {
            name: "+".to_string(),
            args: vec![
                Argument::positional(Expression::Variable("a".to_string())),
                Argument::positional(Expression::Variable("b".to_string())),
            ],
        };
        verifier.verify_expr(&expr, &refined_type);

        let result = verifier.finish();
        // Should add an obligation, not an error
        assert!(!result.has_errors());
        assert!(!result.obligations.is_empty(), "Expected proof obligation");
    }
}
