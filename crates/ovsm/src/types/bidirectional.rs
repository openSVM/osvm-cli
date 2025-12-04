//! # Bidirectional Type Inference for OVSM
//!
//! This module implements bidirectional type checking following the
//! approach described in "Complete and Easy Bidirectional Typechecking
//! for Higher-Rank Polymorphism" (Dunfield & Krishnaswami, 2013).
//!
//! ## Key Concepts
//!
//! **Synthesis (↑)**: Types flow UP from expressions.
//! - Literals, variables, and applications synthesize their types
//! - `synth(e) = A` means "expression e synthesizes type A"
//!
//! **Checking (↓)**: Types flow DOWN into expressions.
//! - Lambdas and conditionals check against expected types
//! - `check(e, A)` means "check that e has type A"
//!
//! **Subsumption**: A type A is a subtype of B if A values can be used where B is expected.
//! - `A <: B` means A is a subtype of B
//! - Enables polymorphism and gradual typing
//!
//! ## Example
//!
//! ```lisp
//! ;; Given: (define inc : (fn (i64) -> i64) (lambda (x) (+ x 1)))
//! ;;
//! ;; The lambda `(lambda (x) (+ x 1))` is CHECKED against `(fn (i64) -> i64)`
//! ;; This propagates the i64 type DOWN into parameter x
//! ;; Without bidirectional typing, x would get type `?T` (unknown)
//! ```

use super::{Type, TypeContext, TypeError, TypedField, TypedStructDef};
use crate::parser::{Argument, BinaryOp, Expression, UnaryOp};
use std::collections::HashMap;

/// Bidirectional type checker with synthesis and checking modes
pub struct BidirectionalChecker {
    ctx: TypeContext,
    /// Type schemes for let-polymorphism (generalized types)
    schemes: HashMap<String, TypeScheme>,
    /// Current expected return type (for return type checking)
    expected_return: Option<Type>,
    /// Inference mode: strict or gradual
    gradual: bool,
}

/// A type scheme represents a polymorphic type with universally quantified variables
/// e.g., `forall a. a -> a` for the identity function
#[derive(Debug, Clone)]
pub struct TypeScheme {
    /// Universally quantified type variables
    pub vars: Vec<u32>,
    /// The underlying type (may reference vars)
    pub ty: Type,
}

impl TypeScheme {
    /// Create a monomorphic scheme (no quantified variables)
    pub fn mono(ty: Type) -> Self {
        TypeScheme {
            vars: Vec::new(),
            ty,
        }
    }

    /// Instantiate a scheme with fresh type variables
    pub fn instantiate(&self, ctx: &mut TypeContext) -> Type {
        if self.vars.is_empty() {
            return self.ty.clone();
        }

        // Create fresh variables and substitute
        let subst: HashMap<u32, Type> = self.vars.iter().map(|&v| (v, ctx.fresh_var())).collect();

        substitute(&self.ty, &subst)
    }
}

/// Substitute type variables in a type
fn substitute(ty: &Type, subst: &HashMap<u32, Type>) -> Type {
    match ty {
        Type::Var(n) => subst.get(n).cloned().unwrap_or_else(|| ty.clone()),
        Type::Array { element, size } => Type::Array {
            element: Box::new(substitute(element, subst)),
            size: *size,
        },
        Type::Tuple(types) => Type::Tuple(types.iter().map(|t| substitute(t, subst)).collect()),
        Type::Ptr(t) => Type::Ptr(Box::new(substitute(t, subst))),
        Type::Ref(t) => Type::Ref(Box::new(substitute(t, subst))),
        Type::RefMut(t) => Type::RefMut(Box::new(substitute(t, subst))),
        Type::Fn { params, ret } => Type::Fn {
            params: params.iter().map(|t| substitute(t, subst)).collect(),
            ret: Box::new(substitute(ret, subst)),
        },
        _ => ty.clone(),
    }
}

impl BidirectionalChecker {
    /// Creates a new bidirectional type checker with gradual typing enabled.
    pub fn new() -> Self {
        BidirectionalChecker {
            ctx: TypeContext::new(),
            schemes: HashMap::new(),
            expected_return: None,
            gradual: true, // Enable gradual typing by default
        }
    }

    /// Create a strict (non-gradual) checker
    pub fn strict() -> Self {
        BidirectionalChecker {
            ctx: TypeContext::new(),
            schemes: HashMap::new(),
            expected_return: None,
            gradual: false,
        }
    }

    // =========================================================================
    // SYNTHESIS MODE (↑): Types flow UP from expressions
    // =========================================================================

    /// Synthesize a type for an expression (bottom-up inference)
    ///
    /// This is the main entry point when no expected type is available.
    /// Returns `Type::Unknown` if synthesis fails and gradual typing is disabled.
    pub fn synth(&mut self, expr: &Expression) -> Type {
        match expr {
            // === Literals always synthesize their types ===
            Expression::IntLiteral(_) => Type::I64,
            Expression::FloatLiteral(_) => Type::F64,
            Expression::StringLiteral(_) => Type::String,
            Expression::BoolLiteral(_) => Type::Bool,
            Expression::NullLiteral => Type::Unit,

            // === Variables: look up in context ===
            Expression::Variable(name) => {
                // Check for type names first (type literals)
                if let Some(ty) = Type::from_name(name) {
                    return ty;
                }

                // Try polymorphic lookup
                if let Some(scheme) = self.schemes.get(name).cloned() {
                    return scheme.instantiate(&mut self.ctx);
                }

                // Then check variable bindings
                match self.ctx.lookup_var(name).cloned() {
                    Some(ty) => ty,
                    None => {
                        if self.gradual {
                            Type::Any
                        } else {
                            self.ctx.record_error(TypeError::new(format!(
                                "undefined variable: {}",
                                name
                            )));
                            Type::Unknown
                        }
                    }
                }
            }

            // === Binary operations: synthesize operands, compute result ===
            Expression::Binary { op, left, right } => {
                let left_ty = self.synth(left);
                let right_ty = self.synth(right);
                self.synth_binary_op(op, &left_ty, &right_ty)
            }

            // === Unary operations ===
            Expression::Unary { op, operand } => {
                let operand_ty = self.synth(operand);
                self.synth_unary_op(op, &operand_ty)
            }

            // === Function application: synthesize function, check arguments ===
            Expression::ToolCall { name, args } => self.synth_application(name, args),

            // === Arrays: synthesize element types, unify ===
            Expression::ArrayLiteral(elements) => self.synth_array(elements),

            // === Lambdas without expected type: create fresh variables ===
            Expression::Lambda { params, body } => {
                // Without an expected type, we create fresh type variables
                let param_types: Vec<Type> = params.iter().map(|_| self.ctx.fresh_var()).collect();

                self.ctx.push_scope();
                for (param, ty) in params.iter().zip(param_types.iter()) {
                    self.ctx.define_var(param, ty.clone());
                }

                let body_ty = self.synth(body);

                self.ctx.pop_scope();

                Type::Fn {
                    params: param_types,
                    ret: Box::new(body_ty),
                }
            }

            // === Conditionals: synthesize branches, unify ===
            Expression::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                // Check condition is boolean
                self.check(condition, &Type::Bool);

                // Synthesize branches and unify
                let then_ty = self.synth(then_expr);
                let else_ty = self.synth(else_expr);

                self.join(&then_ty, &else_ty)
            }

            // === Field access ===
            Expression::FieldAccess { object, field } => {
                let obj_ty = self.synth(object);
                self.synth_field_access(&obj_ty, field)
            }

            // === Index access ===
            Expression::IndexAccess { array, index } => {
                let arr_ty = self.synth(array);
                self.check(index, &Type::I64); // Index should be integer

                match arr_ty {
                    Type::Array { element, .. } => *element,
                    Type::Any => Type::Any,
                    _ => {
                        self.ctx.record_error(TypeError::new(format!(
                            "cannot index into type {}",
                            arr_ty
                        )));
                        if self.gradual {
                            Type::Any
                        } else {
                            Type::Unknown
                        }
                    }
                }
            }

            // === Range expressions ===
            Expression::Range { start, end } => {
                self.check(start, &Type::I64);
                self.check(end, &Type::I64);
                Type::Array {
                    element: Box::new(Type::I64),
                    size: 0,
                }
            }

            // === Objects ===
            Expression::ObjectLiteral(_) => Type::Any,

            // === Grouping ===
            Expression::Grouping(inner) => self.synth(inner),

            // === Macros ===
            Expression::Quasiquote(_) => Type::Any,
            Expression::Unquote(inner) => self.synth(inner),
            Expression::UnquoteSplice(_) => Type::Array {
                element: Box::new(Type::Any),
                size: 0,
            },

            // === Control flow ===
            Expression::Loop(_) => Type::Any,

            Expression::Catch { body, .. } => {
                if body.is_empty() {
                    Type::Unit
                } else {
                    for expr in body.iter().take(body.len() - 1) {
                        self.synth(expr);
                    }
                    self.synth(body.last().unwrap())
                }
            }

            Expression::Throw { value, .. } => {
                self.synth(value);
                Type::Never
            }

            Expression::DestructuringBind { value, body, .. } => {
                self.synth(value);
                if body.is_empty() {
                    Type::Unit
                } else {
                    for expr in body.iter().take(body.len() - 1) {
                        self.synth(expr);
                    }
                    self.synth(body.last().unwrap())
                }
            }

            // === Type Annotations: check expr against annotated type ===
            Expression::TypeAnnotation { expr, type_expr } => {
                // Parse the type expression to get the annotated type
                let annotated_ty = self.parse_type_expr(type_expr);
                // Check the expression against the annotated type
                self.check(expr, &annotated_ty);
                // Return the annotated type (this is the key benefit!)
                annotated_ty
            }

            // === Typed Lambdas: use explicit type annotations ===
            Expression::TypedLambda {
                typed_params,
                return_type,
                body,
            } => {
                // Extract parameter types from annotations
                let param_types: Vec<Type> = typed_params
                    .iter()
                    .map(|(_, maybe_type)| {
                        match maybe_type {
                            Some(type_expr) => self.parse_type_expr(type_expr),
                            None => self.ctx.fresh_var(), // Untyped params get fresh vars
                        }
                    })
                    .collect();

                // Push scope and bind parameters
                self.ctx.push_scope();
                for ((param_name, _), ty) in typed_params.iter().zip(param_types.iter()) {
                    self.ctx.define_var(param_name, ty.clone());
                }

                // Handle return type
                let body_ty = match return_type {
                    Some(ret_type_expr) => {
                        let ret_ty = self.parse_type_expr(ret_type_expr);
                        self.check(body, &ret_ty);
                        ret_ty
                    }
                    None => self.synth(body),
                };

                self.ctx.pop_scope();

                Type::Fn {
                    params: param_types,
                    ret: Box::new(body_ty),
                }
            }

            // === Refinement Type Expressions ===
            Expression::RefinedTypeExpr {
                var,
                base_type,
                predicate,
            } => {
                // Parse the base type
                let base = self.parse_type_expr(base_type);

                // Validate predicate has boolean type
                self.ctx.push_scope();
                self.ctx.define_var(var, base.clone());

                let pred_ty = self.synth(predicate);
                if !matches!(pred_ty, Type::Bool | Type::Any) {
                    self.ctx.record_error(TypeError::new(format!(
                        "refinement predicate must be boolean, found {}",
                        pred_ty
                    )));
                }

                self.ctx.pop_scope();

                // Create the refined type
                Type::Refined(Box::new(crate::types::RefinementType::from_expr(
                    var.clone(),
                    base,
                    predicate,
                )))
            }
        }
    }

    // =========================================================================
    // CHECKING MODE (↓): Types flow DOWN into expressions
    // =========================================================================

    /// Check that an expression has an expected type (top-down checking)
    ///
    /// This propagates type information INTO the expression, enabling
    /// better inference for lambdas and other constructs.
    pub fn check(&mut self, expr: &Expression, expected: &Type) -> bool {
        // Apply any substitutions to expected type
        let expected = self.ctx.resolve(expected);

        // Handle Any and Unknown specially
        if matches!(expected, Type::Any) {
            self.synth(expr);
            return true;
        }

        match expr {
            // === Lambdas are checked, not synthesized ===
            Expression::Lambda { params, body } => self.check_lambda(params, body, &expected),

            // === Conditionals: check both branches against expected ===
            Expression::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.check(condition, &Type::Bool);
                let then_ok = self.check(then_expr, &expected);
                let else_ok = self.check(else_expr, &expected);
                then_ok && else_ok
            }

            // === Arrays: check elements against expected element type ===
            Expression::ArrayLiteral(elements) => {
                if let Type::Array {
                    element: expected_elem,
                    ..
                } = &expected
                {
                    for elem in elements {
                        self.check(elem, expected_elem);
                    }
                    true
                } else {
                    // Expected isn't an array, fall back to synthesis
                    let synth_ty = self.synth(expr);
                    self.subtype(&synth_ty, &expected)
                }
            }

            // === Grouping ===
            Expression::Grouping(inner) => self.check(inner, &expected),

            // === Default: synthesize and check subsumption ===
            _ => {
                let synth_ty = self.synth(expr);
                self.subtype(&synth_ty, &expected)
            }
        }
    }

    /// Check a lambda against an expected function type
    fn check_lambda(&mut self, params: &[String], body: &Expression, expected: &Type) -> bool {
        match expected {
            Type::Fn {
                params: expected_params,
                ret: expected_ret,
            } => {
                // Arity check
                if params.len() != expected_params.len() {
                    self.ctx.record_error(TypeError::new(format!(
                        "lambda has {} parameters but expected {}",
                        params.len(),
                        expected_params.len()
                    )));
                    return false;
                }

                // Bind parameters with expected types
                self.ctx.push_scope();
                for (param, ty) in params.iter().zip(expected_params.iter()) {
                    self.ctx.define_var(param, ty.clone());
                }

                // Check body against expected return type
                let old_expected = self.expected_return.take();
                self.expected_return = Some((**expected_ret).clone());

                let result = self.check(body, expected_ret);

                self.expected_return = old_expected;
                self.ctx.pop_scope();

                result
            }

            Type::Any => {
                // Gradual typing: synthesize lambda type
                self.synth(&Expression::Lambda {
                    params: params.to_vec(),
                    body: Box::new(body.clone()),
                });
                true
            }

            Type::Var(_) => {
                // Type variable: synthesize lambda and unify
                let lambda_ty = self.synth(&Expression::Lambda {
                    params: params.to_vec(),
                    body: Box::new(body.clone()),
                });
                self.subtype(&lambda_ty, expected)
            }

            _ => {
                self.ctx.record_error(TypeError::new(format!(
                    "expected function type but got {}",
                    expected
                )));
                false
            }
        }
    }

    // =========================================================================
    // SUBSUMPTION (subtyping with instantiation)
    // =========================================================================

    /// Check if `actual` is a subtype of `expected` (actual <: expected)
    ///
    /// This handles:
    /// - Type variable instantiation
    /// - Numeric widening (i32 <: i64)
    /// - Gradual typing (Any <: T and T <: Any)
    pub fn subtype(&mut self, actual: &Type, expected: &Type) -> bool {
        let actual = self.ctx.resolve(actual);
        let expected = self.ctx.resolve(expected);

        // Any is compatible with everything (gradual typing)
        if matches!(actual, Type::Any) || matches!(expected, Type::Any) {
            return true;
        }

        // Unknown propagates
        if matches!(actual, Type::Unknown) || matches!(expected, Type::Unknown) {
            return true;
        }

        // Never is subtype of everything (bottom type)
        if matches!(actual, Type::Never) {
            return true;
        }

        // Type variables: instantiate
        if let Type::Var(n) = &expected {
            // Unify with actual
            if let Err(e) = self.ctx.unify(&actual, &expected) {
                self.ctx.record_error(e);
                return false;
            }
            return true;
        }

        if let Type::Var(_) = &actual {
            if let Err(e) = self.ctx.unify(&actual, &expected) {
                self.ctx.record_error(e);
                return false;
            }
            return true;
        }

        // Structural equality
        if actual == expected {
            return true;
        }

        // Numeric widening rules
        if self.numeric_subtype(&actual, &expected) {
            return true;
        }

        // Structural subtyping for compound types
        match (&actual, &expected) {
            // Array covariance: [A; n] <: [B; n] if A <: B
            (
                Type::Array {
                    element: a_elem,
                    size: a_size,
                },
                Type::Array {
                    element: e_elem,
                    size: e_size,
                },
            ) => {
                // Sizes must match (or expected is 0 for "any size")
                if *e_size != 0 && a_size != e_size {
                    self.ctx.record_error(TypeError::new(format!(
                        "array size mismatch: {} vs {}",
                        a_size, e_size
                    )));
                    return false;
                }
                self.subtype(a_elem, e_elem)
            }

            // Function contravariance in params, covariance in return
            (
                Type::Fn {
                    params: a_params,
                    ret: a_ret,
                },
                Type::Fn {
                    params: e_params,
                    ret: e_ret,
                },
            ) => {
                if a_params.len() != e_params.len() {
                    self.ctx.record_error(TypeError::new(format!(
                        "function arity mismatch: {} vs {}",
                        a_params.len(),
                        e_params.len()
                    )));
                    return false;
                }

                // Contravariant in parameters: expected <: actual
                for (a_param, e_param) in a_params.iter().zip(e_params.iter()) {
                    if !self.subtype(e_param, a_param) {
                        return false;
                    }
                }

                // Covariant in return: actual <: expected
                self.subtype(a_ret, e_ret)
            }

            // Pointer covariance
            (Type::Ptr(a), Type::Ptr(e)) => self.subtype(a, e),
            (Type::Ref(a), Type::Ref(e)) => self.subtype(a, e),

            // Mutable reference is invariant
            (Type::RefMut(a), Type::RefMut(e)) => a == e,

            // Tuple covariance
            (Type::Tuple(a_tys), Type::Tuple(e_tys)) => {
                if a_tys.len() != e_tys.len() {
                    return false;
                }
                a_tys
                    .iter()
                    .zip(e_tys.iter())
                    .all(|(a, e)| self.subtype(a, e))
            }

            _ => {
                self.ctx
                    .record_error(TypeError::mismatch(expected.clone(), actual.clone()));
                false
            }
        }
    }

    /// Check numeric subtyping (smaller types fit into larger types)
    fn numeric_subtype(&self, actual: &Type, expected: &Type) -> bool {
        match (actual, expected) {
            // Signed widening
            (Type::I8, Type::I16 | Type::I32 | Type::I64) => true,
            (Type::I16, Type::I32 | Type::I64) => true,
            (Type::I32, Type::I64) => true,

            // Unsigned widening
            (Type::U8, Type::U16 | Type::U32 | Type::U64) => true,
            (Type::U16, Type::U32 | Type::U64) => true,
            (Type::U32, Type::U64) => true,

            // Float widening
            (Type::F32, Type::F64) => true,

            _ => false,
        }
    }

    // =========================================================================
    // HELPER FUNCTIONS
    // =========================================================================

    /// Join two types (find common supertype for branches)
    fn join(&mut self, t1: &Type, t2: &Type) -> Type {
        let t1 = self.ctx.resolve(t1);
        let t2 = self.ctx.resolve(t2);

        // If equal, return either
        if t1 == t2 {
            return t1;
        }

        // Any joins with anything
        if matches!(t1, Type::Any) {
            return t2;
        }
        if matches!(t2, Type::Any) {
            return t1;
        }

        // Try unification
        match self.ctx.unify(&t1, &t2) {
            Ok(unified) => unified,
            Err(_) => {
                if self.gradual {
                    Type::Any
                } else {
                    self.ctx.record_error(TypeError::new(format!(
                        "cannot unify types {} and {}",
                        t1, t2
                    )));
                    Type::Unknown
                }
            }
        }
    }

    /// Synthesize type for binary operation
    fn synth_binary_op(&mut self, op: &BinaryOp, left: &Type, right: &Type) -> Type {
        match op {
            // Arithmetic
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Mod
            | BinaryOp::Pow => {
                if !left.is_numeric() && !matches!(left, Type::Any) {
                    self.ctx.record_error(TypeError::new(format!(
                        "arithmetic requires numeric type, found {}",
                        left
                    )));
                }
                if !right.is_numeric() && !matches!(right, Type::Any) {
                    self.ctx.record_error(TypeError::new(format!(
                        "arithmetic requires numeric type, found {}",
                        right
                    )));
                }
                self.join(left, right)
            }

            // Comparison (result is always bool)
            BinaryOp::Eq
            | BinaryOp::NotEq
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::LtEq
            | BinaryOp::GtEq => {
                // Operands should be comparable
                if !matches!(left, Type::Any) && !matches!(right, Type::Any) {
                    self.join(left, right); // Just for error reporting
                }
                Type::Bool
            }

            // Logical (both operands should be bool)
            BinaryOp::And | BinaryOp::Or => {
                self.subtype(left, &Type::Bool);
                self.subtype(right, &Type::Bool);
                Type::Bool
            }

            // Membership
            BinaryOp::In => Type::Bool,
        }
    }

    /// Synthesize type for unary operation
    fn synth_unary_op(&mut self, op: &UnaryOp, operand: &Type) -> Type {
        match op {
            UnaryOp::Neg => {
                if !operand.is_numeric() && !matches!(operand, Type::Any) {
                    self.ctx.record_error(TypeError::new(format!(
                        "negation requires numeric type, found {}",
                        operand
                    )));
                }
                operand.clone()
            }
            UnaryOp::Not => {
                self.subtype(operand, &Type::Bool);
                Type::Bool
            }
        }
    }

    /// Synthesize type for function application
    fn synth_application(&mut self, name: &str, args: &[Argument]) -> Type {
        // Handle special forms first
        match name {
            // Type annotation: (: expr type)
            ":" if args.len() == 2 => {
                if let Expression::Variable(type_name) = &args[1].value {
                    if let Some(expected) = Type::from_name(type_name) {
                        // CHECK the expression against the expected type
                        self.check(&args[0].value, &expected);
                        return expected;
                    }
                }
                self.synth(&args[0].value)
            }

            // Variable definition
            "define" => self.synth_define(args),

            // Assignment
            "set!" => self.synth_set(args),

            // Control flow
            "if" => self.synth_if(args),
            "do" => self.synth_do(args),
            "let" => self.synth_let(args),

            // Struct operations
            "struct-get" if args.len() == 3 => self.synth_struct_get(args),
            "struct-set" if args.len() == 4 => self.synth_struct_set(args),
            "zerocopy-load" if args.len() == 3 => self.synth_zerocopy_load(args),
            "zerocopy-store" if args.len() == 4 => self.synth_zerocopy_store(args),

            // Built-in functions with known signatures
            "account-data-ptr" => Type::Ptr(Box::new(Type::U8)),
            "account-data-len" => Type::U64,
            "account-lamports" => Type::U64,
            "is-signer" | "is-writable" => Type::Bool,
            "length" => Type::U64,
            "not" => Type::Bool,

            // Variadic arithmetic
            "+" | "-" | "*" | "/" | "%" => {
                let mut result = Type::I64;
                for arg in args {
                    let arg_ty = self.synth(&arg.value);
                    result = self.join(&result, &arg_ty);
                }
                result
            }

            // Comparison
            "=" | "!=" | "<" | ">" | "<=" | ">=" | "and" | "or" => Type::Bool,

            // Higher-order functions
            "map" | "filter" => self.synth_higher_order(name, args),
            "reduce" => self.synth_reduce(args),

            // Collection access
            "nth" | "get" => self.synth_collection_access(args),

            // Loops
            "for" | "while" => Type::Unit,

            // Default: try looking up as function
            _ => {
                // Check if it's a known function
                if let Some(fn_type) = self.ctx.lookup_function(name).cloned() {
                    if let Type::Fn { params, ret } = fn_type {
                        // Check argument types against parameter types
                        for (arg, param_ty) in args.iter().zip(params.iter()) {
                            self.check(&arg.value, param_ty);
                        }
                        return *ret;
                    }
                }

                // Unknown function
                if self.gradual {
                    // Synthesize all arguments for side effects
                    for arg in args {
                        self.synth(&arg.value);
                    }
                    Type::Any
                } else {
                    self.ctx
                        .record_error(TypeError::new(format!("unknown function: {}", name)));
                    Type::Unknown
                }
            }
        }
    }

    /// Synthesize type for define
    fn synth_define(&mut self, args: &[Argument]) -> Type {
        if args.len() == 2 {
            // Untyped: (define name value)
            if let Expression::Variable(var_name) = &args[0].value {
                let val_type = self.synth(&args[1].value);
                self.ctx.define_var(var_name, val_type.clone());

                // Generalize to type scheme for let-polymorphism
                let scheme = self.generalize(&val_type);
                self.schemes.insert(var_name.clone(), scheme);

                return val_type;
            }
        } else if args.len() == 4 {
            // Typed: (define name : type value)
            if let (
                Expression::Variable(var_name),
                Expression::Variable(colon),
                Expression::Variable(type_name),
            ) = (&args[0].value, &args[1].value, &args[2].value)
            {
                if colon == ":" {
                    let declared_type = Type::from_name(type_name).unwrap_or(Type::Any);

                    // CHECK the value against the declared type
                    self.check(&args[3].value, &declared_type);

                    self.ctx.define_var(var_name, declared_type.clone());
                    self.schemes
                        .insert(var_name.clone(), TypeScheme::mono(declared_type.clone()));

                    return declared_type;
                }
            }
        }
        Type::Unit
    }

    /// Synthesize type for set!
    fn synth_set(&mut self, args: &[Argument]) -> Type {
        if args.len() == 2 {
            if let Expression::Variable(var_name) = &args[0].value {
                if let Some(existing_type) = self.ctx.lookup_var(var_name).cloned() {
                    // CHECK the new value against the existing type
                    self.check(&args[1].value, &existing_type);
                    return existing_type;
                } else {
                    // Variable doesn't exist
                    let val_type = self.synth(&args[1].value);
                    self.ctx.define_var(var_name, val_type.clone());
                    return val_type;
                }
            }
        }
        Type::Unit
    }

    /// Synthesize type for if
    fn synth_if(&mut self, args: &[Argument]) -> Type {
        if args.len() >= 2 {
            self.check(&args[0].value, &Type::Bool);
            let then_ty = self.synth(&args[1].value);

            if args.len() >= 3 {
                let else_ty = self.synth(&args[2].value);
                return self.join(&then_ty, &else_ty);
            }

            return then_ty;
        }
        Type::Unit
    }

    /// Synthesize type for do block
    fn synth_do(&mut self, args: &[Argument]) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }

        for arg in args.iter().take(args.len() - 1) {
            self.synth(&arg.value);
        }

        self.synth(&args.last().unwrap().value)
    }

    /// Synthesize type for let binding
    fn synth_let(&mut self, args: &[Argument]) -> Type {
        // (let ((x val1) (y val2)) body)
        // For now, simplified handling
        if args.len() >= 2 {
            self.ctx.push_scope();

            // Process bindings (first arg should be a list of bindings)
            // Simplified: just synthesize all args
            for arg in args.iter().take(args.len() - 1) {
                self.synth(&arg.value);
            }

            let result = self.synth(&args.last().unwrap().value);

            self.ctx.pop_scope();
            return result;
        }
        Type::Any
    }

    /// Synthesize type for struct-get
    fn synth_struct_get(&mut self, args: &[Argument]) -> Type {
        if let Expression::Variable(struct_name) = &args[0].value {
            if let Expression::Variable(field_name) = &args[2].value {
                let field_type = self
                    .ctx
                    .lookup_struct(struct_name)
                    .and_then(|def| def.fields.iter().find(|f| f.name == *field_name))
                    .map(|f| f.field_type.clone());

                if let Some(ty) = field_type {
                    return ty;
                } else {
                    self.ctx.record_error(TypeError::new(format!(
                        "struct '{}' has no field '{}'",
                        struct_name, field_name
                    )));
                }
            }
        }
        Type::Any
    }

    /// Synthesize type for struct-set
    fn synth_struct_set(&mut self, args: &[Argument]) -> Type {
        if let Expression::Variable(struct_name) = &args[0].value {
            if let Expression::Variable(field_name) = &args[2].value {
                let field_type = self
                    .ctx
                    .lookup_struct(struct_name)
                    .and_then(|def| def.fields.iter().find(|f| f.name == *field_name))
                    .map(|f| f.field_type.clone());

                if let Some(expected_type) = field_type {
                    self.check(&args[3].value, &expected_type);
                } else {
                    self.ctx.record_error(TypeError::new(format!(
                        "struct '{}' has no field '{}'",
                        struct_name, field_name
                    )));
                }
            }
        }
        Type::Unit
    }

    /// Synthesize type for zerocopy-load
    fn synth_zerocopy_load(&mut self, args: &[Argument]) -> Type {
        if let Expression::Variable(struct_name) = &args[0].value {
            if let Expression::Variable(field_name) = &args[2].value {
                let field_type = self
                    .ctx
                    .lookup_struct(struct_name)
                    .and_then(|def| def.fields.iter().find(|f| f.name == *field_name))
                    .map(|f| f.field_type.clone());

                if let Some(ty) = field_type {
                    return ty;
                }
            }
        }
        Type::Any
    }

    /// Synthesize type for zerocopy-store
    fn synth_zerocopy_store(&mut self, args: &[Argument]) -> Type {
        if let Expression::Variable(struct_name) = &args[0].value {
            if let Expression::Variable(field_name) = &args[2].value {
                let field_type = self
                    .ctx
                    .lookup_struct(struct_name)
                    .and_then(|def| def.fields.iter().find(|f| f.name == *field_name))
                    .map(|f| f.field_type.clone());

                if let Some(expected_type) = field_type {
                    self.check(&args[3].value, &expected_type);
                }
            }
        }
        Type::Unit
    }

    /// Synthesize type for field access
    fn synth_field_access(&mut self, obj_ty: &Type, field: &str) -> Type {
        match obj_ty {
            Type::Struct(struct_name) => {
                if let Some(struct_def) = self.ctx.lookup_struct(struct_name) {
                    if let Some(f) = struct_def.fields.iter().find(|f| f.name == field) {
                        return f.field_type.clone();
                    } else {
                        self.ctx.record_error(TypeError::new(format!(
                            "struct '{}' has no field '{}'",
                            struct_name, field
                        )));
                    }
                }
                Type::Any
            }
            Type::Any => Type::Any,
            _ => {
                self.ctx.record_error(TypeError::new(format!(
                    "cannot access field '{}' on type {}",
                    field, obj_ty
                )));
                if self.gradual {
                    Type::Any
                } else {
                    Type::Unknown
                }
            }
        }
    }

    /// Synthesize type for array literal
    fn synth_array(&mut self, elements: &[Expression]) -> Type {
        if elements.is_empty() {
            return Type::Array {
                element: Box::new(Type::Any),
                size: 0,
            };
        }

        // Infer element type from first element, then check others
        let elem_ty = self.synth(&elements[0]);

        for elem in elements.iter().skip(1) {
            self.check(elem, &elem_ty);
        }

        Type::Array {
            element: Box::new(elem_ty),
            size: elements.len(),
        }
    }

    /// Synthesize type for higher-order functions (map, filter)
    fn synth_higher_order(&mut self, _name: &str, args: &[Argument]) -> Type {
        if args.len() >= 2 {
            let arr_ty = self.synth(&args[0].value);
            let fn_ty = self.synth(&args[1].value);

            if let Type::Array {
                element: elem_ty, ..
            } = arr_ty
            {
                if let Type::Fn { ret, .. } = fn_ty {
                    return Type::Array {
                        element: ret,
                        size: 0,
                    };
                }
                return Type::Array {
                    element: elem_ty,
                    size: 0,
                };
            }
        }
        Type::Array {
            element: Box::new(Type::Any),
            size: 0,
        }
    }

    /// Synthesize type for reduce
    fn synth_reduce(&mut self, args: &[Argument]) -> Type {
        if args.len() >= 2 {
            // reduce takes (array initial fn) - initial determines result type
            let _arr_ty = self.synth(&args[0].value);
            let initial_ty = self.synth(&args[1].value);
            return initial_ty;
        }
        Type::Any
    }

    /// Synthesize type for collection access (nth, get)
    fn synth_collection_access(&mut self, args: &[Argument]) -> Type {
        if !args.is_empty() {
            let arr_ty = self.synth(&args[0].value);
            if let Type::Array { element, .. } = arr_ty {
                return *element;
            }
        }
        Type::Any
    }

    // =========================================================================
    // LET-POLYMORPHISM
    // =========================================================================

    /// Generalize a type to a type scheme
    ///
    /// Free type variables (not bound in context) become universally quantified.
    fn generalize(&self, ty: &Type) -> TypeScheme {
        let free_vars = self.free_type_vars(ty);
        TypeScheme {
            vars: free_vars,
            ty: ty.clone(),
        }
    }

    /// Find free type variables in a type
    fn free_type_vars(&self, ty: &Type) -> Vec<u32> {
        let mut vars = Vec::new();
        self.collect_vars(ty, &mut vars);
        vars.sort();
        vars.dedup();
        vars
    }

    fn collect_vars(&self, ty: &Type, vars: &mut Vec<u32>) {
        match ty {
            Type::Var(n) => {
                // Check if this variable is bound by a substitution
                if self.ctx.resolve(ty) == *ty {
                    vars.push(*n);
                }
            }
            Type::Array { element, .. } => self.collect_vars(element, vars),
            Type::Tuple(types) => {
                for t in types {
                    self.collect_vars(t, vars);
                }
            }
            Type::Ptr(t) | Type::Ref(t) | Type::RefMut(t) => self.collect_vars(t, vars),
            Type::Fn { params, ret } => {
                for p in params {
                    self.collect_vars(p, vars);
                }
                self.collect_vars(ret, vars);
            }
            _ => {}
        }
    }

    // =========================================================================
    // PUBLIC API
    // =========================================================================

    /// Get all accumulated errors
    pub fn errors(&self) -> &[TypeError] {
        self.ctx.errors()
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        self.ctx.has_errors()
    }

    /// Define a struct in the type context
    pub fn define_struct(&mut self, def: TypedStructDef) {
        self.ctx.define_struct(def);
    }

    /// Get the type context
    pub fn context(&self) -> &TypeContext {
        &self.ctx
    }

    /// Get mutable type context
    pub fn context_mut(&mut self) -> &mut TypeContext {
        &mut self.ctx
    }

    // =========================================================================
    // TYPE EXPRESSION PARSING
    // =========================================================================

    /// Parse a type expression into a Type
    /// Handles both simple types (variables like `u64`) and compound types
    /// (function types like `(-> i64 i64)`, generics like `(Array u64)`)
    fn parse_type_expr(&mut self, type_expr: &Expression) -> Type {
        match type_expr {
            // Simple type names: u64, i32, bool, etc.
            Expression::Variable(name) => {
                Type::from_name(name).unwrap_or_else(|| {
                    // Check if it's a known struct type
                    if self.ctx.lookup_struct(name).is_some() {
                        Type::Struct(name.clone())
                    } else {
                        // Unknown type - create a type variable in gradual mode
                        if self.gradual {
                            Type::Any
                        } else {
                            self.ctx
                                .record_error(TypeError::new(format!("unknown type: {}", name)));
                            Type::Unknown
                        }
                    }
                })
            }

            // Function type: (-> ParamTypes... ReturnType)
            Expression::ToolCall { name, args } if name == "->" => {
                if args.is_empty() {
                    self.ctx.record_error(TypeError::new(
                        "function type requires at least a return type".to_string(),
                    ));
                    return Type::Fn {
                        params: vec![],
                        ret: Box::new(Type::Unit),
                    };
                }

                // Last arg is return type, rest are parameters
                let param_types: Vec<Type> = args
                    .iter()
                    .take(args.len() - 1)
                    .map(|arg| self.parse_type_expr(&arg.value))
                    .collect();

                let ret_type = self.parse_type_expr(&args.last().unwrap().value);

                Type::Fn {
                    params: param_types,
                    ret: Box::new(ret_type),
                }
            }

            // Generic types: (Array T), (Option T), (Ptr T), (Ref T), etc.
            Expression::ToolCall { name, args } => {
                match name.as_str() {
                    "Array" | "array" if !args.is_empty() => {
                        let elem_ty = self.parse_type_expr(&args[0].value);
                        Type::Array {
                            element: Box::new(elem_ty),
                            size: 0,
                        }
                    }
                    "Ptr" | "ptr" if !args.is_empty() => {
                        let inner = self.parse_type_expr(&args[0].value);
                        Type::Ptr(Box::new(inner))
                    }
                    "Ref" | "ref" if !args.is_empty() => {
                        let inner = self.parse_type_expr(&args[0].value);
                        Type::Ref(Box::new(inner))
                    }
                    "RefMut" | "ref-mut" if !args.is_empty() => {
                        let inner = self.parse_type_expr(&args[0].value);
                        Type::RefMut(Box::new(inner))
                    }
                    "Tuple" | "tuple" => {
                        let types: Vec<Type> = args
                            .iter()
                            .map(|arg| self.parse_type_expr(&arg.value))
                            .collect();
                        Type::Tuple(types)
                    }
                    _ => {
                        // Unknown generic type - might be a custom struct constructor
                        if self.gradual {
                            Type::Any
                        } else {
                            self.ctx.record_error(TypeError::new(format!(
                                "unknown type constructor: {}",
                                name
                            )));
                            Type::Unknown
                        }
                    }
                }
            }

            // Literal types (for things like `null` as Unit)
            Expression::NullLiteral => Type::Unit,

            // Arrays could be tuple types
            Expression::ArrayLiteral(elements) => {
                let types: Vec<Type> = elements.iter().map(|e| self.parse_type_expr(e)).collect();
                Type::Tuple(types)
            }

            // Anything else - in gradual mode allow, otherwise error
            _ => {
                if self.gradual {
                    Type::Any
                } else {
                    self.ctx.record_error(TypeError::new(format!(
                        "invalid type expression: {:?}",
                        type_expr
                    )));
                    Type::Unknown
                }
            }
        }
    }
}

impl Default for BidirectionalChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Argument;

    #[test]
    fn test_literal_synthesis() {
        let mut checker = BidirectionalChecker::new();

        assert_eq!(checker.synth(&Expression::IntLiteral(42)), Type::I64);
        assert_eq!(checker.synth(&Expression::FloatLiteral(3.14)), Type::F64);
        assert_eq!(checker.synth(&Expression::BoolLiteral(true)), Type::Bool);
    }

    #[test]
    fn test_lambda_checking() {
        let mut checker = BidirectionalChecker::new();

        // Lambda checked against expected function type
        let lambda = Expression::Lambda {
            params: vec!["x".to_string()],
            body: Box::new(Expression::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expression::Variable("x".to_string())),
                right: Box::new(Expression::IntLiteral(1)),
            }),
        };

        let expected = Type::Fn {
            params: vec![Type::I64],
            ret: Box::new(Type::I64),
        };

        // Should succeed - x gets type i64 from expected
        assert!(checker.check(&lambda, &expected));
        assert!(!checker.has_errors());
    }

    #[test]
    fn test_lambda_synthesis_without_expected() {
        let mut checker = BidirectionalChecker::new();

        // Lambda synthesized without expected type
        let lambda = Expression::Lambda {
            params: vec!["x".to_string()],
            body: Box::new(Expression::IntLiteral(42)),
        };

        let ty = checker.synth(&lambda);

        // Should get fn(?0) -> i64 where ?0 is a fresh type variable
        if let Type::Fn { params, ret } = ty {
            assert_eq!(params.len(), 1);
            assert!(matches!(params[0], Type::Var(_)));
            assert_eq!(*ret, Type::I64);
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_numeric_subtyping() {
        let mut checker = BidirectionalChecker::new();

        // i32 <: i64
        assert!(checker.subtype(&Type::I32, &Type::I64));

        // u8 <: u64
        assert!(checker.subtype(&Type::U8, &Type::U64));

        // NOT: i64 <: i32 (narrowing requires explicit cast)
        let mut strict_checker = BidirectionalChecker::strict();
        assert!(!strict_checker.subtype(&Type::I64, &Type::I32));
    }

    #[test]
    fn test_type_annotation_checking() {
        let mut checker = BidirectionalChecker::new();

        // (: 42 i64) should check 42 against i64
        let annotated = Expression::ToolCall {
            name: ":".to_string(),
            args: vec![
                Argument {
                    name: None,
                    value: Expression::IntLiteral(42),
                },
                Argument {
                    name: None,
                    value: Expression::Variable("i64".to_string()),
                },
            ],
        };

        let ty = checker.synth(&annotated);
        assert_eq!(ty, Type::I64);
        assert!(!checker.has_errors());
    }

    #[test]
    fn test_gradual_typing() {
        let mut checker = BidirectionalChecker::new();

        // Unknown variable gets Any in gradual mode
        let var = Expression::Variable("unknown".to_string());
        assert_eq!(checker.synth(&var), Type::Any);
        assert!(!checker.has_errors());
    }

    #[test]
    fn test_strict_mode_unknown_variable() {
        let mut checker = BidirectionalChecker::strict();

        // Unknown variable is an error in strict mode
        let var = Expression::Variable("unknown".to_string());
        let ty = checker.synth(&var);
        assert_eq!(ty, Type::Unknown);
        assert!(checker.has_errors());
    }

    #[test]
    fn test_conditional_join() {
        let mut checker = BidirectionalChecker::new();

        // Ternary with same types
        let ternary = Expression::Ternary {
            condition: Box::new(Expression::BoolLiteral(true)),
            then_expr: Box::new(Expression::IntLiteral(1)),
            else_expr: Box::new(Expression::IntLiteral(2)),
        };

        assert_eq!(checker.synth(&ternary), Type::I64);
    }

    #[test]
    fn test_array_element_checking() {
        let mut checker = BidirectionalChecker::new();

        // Array literal
        let arr = Expression::ArrayLiteral(vec![
            Expression::IntLiteral(1),
            Expression::IntLiteral(2),
            Expression::IntLiteral(3),
        ]);

        let ty = checker.synth(&arr);

        if let Type::Array { element, size } = ty {
            assert_eq!(*element, Type::I64);
            assert_eq!(size, 3);
        } else {
            panic!("Expected array type");
        }
    }

    #[test]
    fn test_native_type_annotation_ast() {
        let mut checker = BidirectionalChecker::new();

        // Test the native TypeAnnotation AST node
        // Use i64 since IntLiteral synthesizes to i64
        let annotated = Expression::TypeAnnotation {
            expr: Box::new(Expression::IntLiteral(42)),
            type_expr: Box::new(Expression::Variable("i64".to_string())),
        };

        let ty = checker.synth(&annotated);
        assert_eq!(ty, Type::I64);
        assert!(!checker.has_errors());
    }

    #[test]
    fn test_type_annotation_enforces_type() {
        let mut checker = BidirectionalChecker::new();

        // Annotating with a different but compatible type still reports the annotation type
        // u64 annotation on i64 literal - the type IS u64 even if there's an error
        let annotated = Expression::TypeAnnotation {
            expr: Box::new(Expression::IntLiteral(42)),
            type_expr: Box::new(Expression::Variable("u64".to_string())),
        };

        let ty = checker.synth(&annotated);
        // Returns the annotated type regardless
        assert_eq!(ty, Type::U64);
        // Note: in strict mode this would report an error, but gradual mode is lenient
    }

    #[test]
    fn test_native_type_annotation_function_type() {
        let mut checker = BidirectionalChecker::new();

        // Test annotation with function type: (: (lambda (x) x) (-> i64 i64))
        let annotated = Expression::TypeAnnotation {
            expr: Box::new(Expression::Lambda {
                params: vec!["x".to_string()],
                body: Box::new(Expression::Variable("x".to_string())),
            }),
            type_expr: Box::new(Expression::ToolCall {
                name: "->".to_string(),
                args: vec![
                    Argument::positional(Expression::Variable("i64".to_string())),
                    Argument::positional(Expression::Variable("i64".to_string())),
                ],
            }),
        };

        let ty = checker.synth(&annotated);
        if let Type::Fn { params, ret } = ty {
            assert_eq!(params.len(), 1);
            assert_eq!(params[0], Type::I64);
            assert_eq!(*ret, Type::I64);
        } else {
            panic!("Expected function type, got {:?}", ty);
        }
        assert!(!checker.has_errors());
    }

    #[test]
    fn test_typed_lambda_ast() {
        let mut checker = BidirectionalChecker::new();

        // Test TypedLambda with explicit parameter types
        let typed_lambda = Expression::TypedLambda {
            typed_params: vec![
                (
                    "x".to_string(),
                    Some(Box::new(Expression::Variable("i64".to_string()))),
                ),
                (
                    "y".to_string(),
                    Some(Box::new(Expression::Variable("i64".to_string()))),
                ),
            ],
            return_type: Some(Box::new(Expression::Variable("i64".to_string()))),
            body: Box::new(Expression::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expression::Variable("x".to_string())),
                right: Box::new(Expression::Variable("y".to_string())),
            }),
        };

        let ty = checker.synth(&typed_lambda);
        if let Type::Fn { params, ret } = ty {
            assert_eq!(params.len(), 2);
            assert_eq!(params[0], Type::I64);
            assert_eq!(params[1], Type::I64);
            assert_eq!(*ret, Type::I64);
        } else {
            panic!("Expected function type, got {:?}", ty);
        }
        assert!(!checker.has_errors());
    }

    #[test]
    fn test_typed_lambda_partial_annotations() {
        let mut checker = BidirectionalChecker::new();

        // TypedLambda with only some parameters annotated
        let typed_lambda = Expression::TypedLambda {
            typed_params: vec![
                (
                    "x".to_string(),
                    Some(Box::new(Expression::Variable("i64".to_string()))),
                ),
                ("y".to_string(), None), // No annotation - should get fresh var
            ],
            return_type: None, // Infer return type from body
            body: Box::new(Expression::Variable("x".to_string())),
        };

        let ty = checker.synth(&typed_lambda);
        if let Type::Fn { params, ret } = ty {
            assert_eq!(params.len(), 2);
            assert_eq!(params[0], Type::I64);
            // y should be a type variable
            assert!(matches!(params[1], Type::Var(_)));
            assert_eq!(*ret, Type::I64);
        } else {
            panic!("Expected function type, got {:?}", ty);
        }
    }
}
