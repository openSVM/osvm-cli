//! # OVSM Source-Level Type System
//!
//! This module provides static typing for OVSM programs with gradual typing support.
//! Types can be explicitly annotated or inferred, and untyped code interoperates
//! seamlessly through the `Any` type.
//!
//! ## Type Annotation Syntax
//!
//! ```lisp
//! ;; Variable with type annotation
//! (define x : u64 42)
//!
//! ;; Expression type annotation
//! (: (+ a b) i32)
//!
//! ;; Function with typed parameters and return
//! (defn add (a : i32) (b : i32) -> i32
//!   (+ a b))
//!
//! ;; Pointer types
//! (define ptr : (ptr u8) (account-data-ptr 0))
//!
//! ;; Array types
//! (define arr : [u8 32] (make-array 32 0))
//! ```
//!
//! ## Gradual Typing
//!
//! Untyped code works without modification - types are inferred where possible
//! and default to `Any` otherwise:
//!
//! ```lisp
//! ;; This works - x is inferred as Any
//! (define x 42)
//!
//! ;; This also works - explicit type
//! (define y : i64 42)
//!
//! ;; Mixed typed/untyped code
//! (+ x y)  ; Works: Any + i64 -> Any
//! ```

pub mod bidirectional;
pub mod bridge;
pub mod checker;
pub mod refinement;
pub mod verify;

// Re-export TypeChecker for convenience
pub use bidirectional::BidirectionalChecker;
pub use bridge::{TypeBridge, TypeEnvSourceExt};
pub use checker::TypeChecker;
pub use refinement::{
    CompareOp, Predicate, PredicateExpr, ProofObligation, RefinementChecker, RefinementError,
    RefinementType,
};
pub use verify::{RefinementVerifier, VerificationError, VerificationResult};

use std::collections::HashMap;
use std::fmt;

/// Source-level type representation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    // === Primitives ===
    /// Unsigned 8-bit integer
    U8,
    /// Unsigned 16-bit integer
    U16,
    /// Unsigned 32-bit integer
    U32,
    /// Unsigned 64-bit integer
    U64,
    /// Signed 8-bit integer
    I8,
    /// Signed 16-bit integer
    I16,
    /// Signed 32-bit integer
    I32,
    /// Signed 64-bit integer
    I64,
    /// 32-bit floating point
    F32,
    /// 64-bit floating point
    F64,
    /// Boolean type
    Bool,
    /// Unit type (void)
    Unit,

    // === Compound Types ===
    /// Fixed-size array: [T; N]
    Array {
        /// Type of elements in the array
        element: Box<Type>,
        /// Number of elements in the array
        size: usize,
    },
    /// Tuple: (T1, T2, ...)
    Tuple(Vec<Type>),
    /// Named struct type
    Struct(String),
    /// Solana pubkey (32 bytes)
    Pubkey,
    /// String type
    String,

    // === Reference Types ===
    /// Raw pointer: *T
    Ptr(Box<Type>),
    /// Immutable reference: &T
    Ref(Box<Type>),
    /// Mutable reference: &mut T
    RefMut(Box<Type>),

    // === Function Type ===
    /// Function: (T1, T2, ...) -> R
    Fn {
        /// Parameter types of the function
        params: Vec<Type>,
        /// Return type of the function
        ret: Box<Type>,
    },

    // === Special Types ===
    /// Dynamic type for gradual typing (accepts any value)
    Any,
    /// Bottom type (never returns, e.g., panic)
    Never,
    /// Type variable for inference (internal use)
    Var(u32),
    /// Unknown type (placeholder during inference)
    Unknown,

    // === Refinement Types ===
    /// Refined type with predicate: {x : T | P(x)}
    /// Example: {x : u64 | x < 10} for array index bounds
    Refined(Box<RefinementType>),
}

impl Type {
    /// Parse a type from a string name
    pub fn from_name(name: &str) -> Option<Type> {
        match name {
            "u8" => Some(Type::U8),
            "u16" => Some(Type::U16),
            "u32" => Some(Type::U32),
            "u64" => Some(Type::U64),
            "i8" => Some(Type::I8),
            "i16" => Some(Type::I16),
            "i32" => Some(Type::I32),
            "i64" => Some(Type::I64),
            "f32" => Some(Type::F32),
            "f64" => Some(Type::F64),
            "bool" => Some(Type::Bool),
            "unit" | "()" => Some(Type::Unit),
            "pubkey" | "Pubkey" => Some(Type::Pubkey),
            "string" | "String" => Some(Type::String),
            "any" | "Any" => Some(Type::Any),
            "never" | "Never" | "!" => Some(Type::Never),
            _ => None,
        }
    }

    /// Check if this type is a numeric type
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::F32
                | Type::F64
        )
    }

    /// Check if this type is an integer type
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
        )
    }

    /// Check if this type is a signed integer
    pub fn is_signed(&self) -> bool {
        matches!(self, Type::I8 | Type::I16 | Type::I32 | Type::I64)
    }

    /// Check if this type is a pointer type
    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Ptr(_) | Type::Ref(_) | Type::RefMut(_))
    }

    /// Get the size in bytes for primitive types
    pub fn size_bytes(&self) -> Option<usize> {
        match self {
            Type::U8 | Type::I8 | Type::Bool => Some(1),
            Type::U16 | Type::I16 => Some(2),
            Type::U32 | Type::I32 | Type::F32 => Some(4),
            Type::U64 | Type::I64 | Type::F64 | Type::Ptr(_) => Some(8),
            Type::Pubkey => Some(32),
            Type::Array { element, size } => element.size_bytes().map(|s| s * size),
            _ => None,
        }
    }

    /// Get the inner type for pointer/reference types
    pub fn pointee(&self) -> Option<&Type> {
        match self {
            Type::Ptr(t) | Type::Ref(t) | Type::RefMut(t) => Some(t),
            _ => None,
        }
    }

    /// Get the element type for array types
    pub fn element_type(&self) -> Option<&Type> {
        match self {
            Type::Array { element, .. } => Some(element),
            _ => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "()"),
            Type::String => write!(f, "String"),
            Type::Pubkey => write!(f, "Pubkey"),
            Type::Array { element, size } => write!(f, "[{}; {}]", element, size),
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Struct(name) => write!(f, "{}", name),
            Type::Ptr(t) => write!(f, "*{}", t),
            Type::Ref(t) => write!(f, "&{}", t),
            Type::RefMut(t) => write!(f, "&mut {}", t),
            Type::Fn { params, ret } => {
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ") -> {}", ret)
            }
            Type::Any => write!(f, "Any"),
            Type::Never => write!(f, "!"),
            Type::Var(n) => write!(f, "?{}", n),
            Type::Unknown => write!(f, "?"),
            Type::Refined(refined) => write!(f, "{}", refined),
        }
    }
}

/// Type error with location information
#[derive(Debug, Clone)]
pub struct TypeError {
    /// Error message describing the type mismatch or problem
    pub message: String,
    /// Expected type in a type mismatch error
    pub expected: Option<Type>,
    /// Actual type found in a type mismatch error
    pub found: Option<Type>,
    /// Source location where the error occurred
    pub location: Option<String>,
}

impl TypeError {
    /// Creates a new type error with a custom message
    pub fn new(message: impl Into<String>) -> Self {
        TypeError {
            message: message.into(),
            expected: None,
            found: None,
            location: None,
        }
    }

    /// Creates a type mismatch error with expected and found types
    pub fn mismatch(expected: Type, found: Type) -> Self {
        TypeError {
            message: format!("type mismatch: expected {}, found {}", expected, found),
            expected: Some(expected),
            found: Some(found),
            location: None,
        }
    }

    /// Adds source location information to this error
    pub fn with_location(mut self, loc: impl Into<String>) -> Self {
        self.location = Some(loc.into());
        self
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)?;
        if let Some(loc) = &self.location {
            write!(f, " at {}", loc)?;
        }
        Ok(())
    }
}

impl std::error::Error for TypeError {}

/// Typed struct field definition (mirrors IR but for source level)
#[derive(Debug, Clone)]
pub struct TypedField {
    /// Name of the field
    pub name: String,
    /// Type of the field
    pub field_type: Type,
    /// Byte offset of the field within the struct
    pub offset: usize,
}

/// Typed struct definition
#[derive(Debug, Clone)]
pub struct TypedStructDef {
    /// Name of the struct
    pub name: String,
    /// List of fields in the struct
    pub fields: Vec<TypedField>,
    /// Total size of the struct in bytes
    pub total_size: usize,
}

/// Type context for type checking
pub struct TypeContext {
    /// Variable bindings: name -> type
    variables: HashMap<String, Type>,
    /// Struct definitions: name -> definition
    structs: HashMap<String, TypedStructDef>,
    /// Function types: name -> function type
    functions: HashMap<String, Type>,
    /// Type variable substitutions (for inference)
    substitutions: HashMap<u32, Type>,
    /// Next type variable ID
    next_var: u32,
    /// Accumulated type errors
    errors: Vec<TypeError>,
}

impl TypeContext {
    /// Creates a new empty type context
    pub fn new() -> Self {
        TypeContext {
            variables: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            substitutions: HashMap::new(),
            next_var: 0,
            errors: Vec::new(),
        }
    }

    /// Create a fresh type variable for inference
    pub fn fresh_var(&mut self) -> Type {
        let var = Type::Var(self.next_var);
        self.next_var += 1;
        var
    }

    /// Define a variable with a type
    pub fn define_var(&mut self, name: &str, ty: Type) {
        self.variables.insert(name.to_string(), ty);
    }

    /// Look up a variable's type
    pub fn lookup_var(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }

    /// Define a struct
    pub fn define_struct(&mut self, def: TypedStructDef) {
        self.structs.insert(def.name.clone(), def);
    }

    /// Look up a struct definition
    pub fn lookup_struct(&self, name: &str) -> Option<&TypedStructDef> {
        self.structs.get(name)
    }

    /// Define a function type
    pub fn define_function(&mut self, name: &str, ty: Type) {
        self.functions.insert(name.to_string(), ty);
    }

    /// Look up a function type
    pub fn lookup_function(&self, name: &str) -> Option<&Type> {
        self.functions.get(name)
    }

    /// Record a type error
    pub fn record_error(&mut self, error: TypeError) {
        self.errors.push(error);
    }

    /// Check if there are any type errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get all type errors
    pub fn errors(&self) -> &[TypeError] {
        &self.errors
    }

    /// Apply type substitutions to resolve type variables
    pub fn resolve(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(n) => {
                if let Some(subst) = self.substitutions.get(n) {
                    self.resolve(subst)
                } else {
                    ty.clone()
                }
            }
            Type::Array { element, size } => Type::Array {
                element: Box::new(self.resolve(element)),
                size: *size,
            },
            Type::Tuple(types) => Type::Tuple(types.iter().map(|t| self.resolve(t)).collect()),
            Type::Ptr(t) => Type::Ptr(Box::new(self.resolve(t))),
            Type::Ref(t) => Type::Ref(Box::new(self.resolve(t))),
            Type::RefMut(t) => Type::RefMut(Box::new(self.resolve(t))),
            Type::Fn { params, ret } => Type::Fn {
                params: params.iter().map(|t| self.resolve(t)).collect(),
                ret: Box::new(self.resolve(ret)),
            },
            _ => ty.clone(),
        }
    }

    /// Unify two types, recording substitutions for type variables.
    /// Returns the unified type if successful, or a type error if the types are incompatible.
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<Type, TypeError> {
        // Resolve any existing substitutions first
        let t1 = self.resolve(t1);
        let t2 = self.resolve(t2);

        // Handle type variables
        if let Type::Var(n) = &t1 {
            self.substitutions.insert(*n, t2.clone());
            return Ok(t2);
        }
        if let Type::Var(n) = &t2 {
            self.substitutions.insert(*n, t1.clone());
            return Ok(t1);
        }

        // Handle Any (gradual typing) - Any unifies with anything
        if matches!(&t1, Type::Any) {
            return Ok(t2);
        }
        if matches!(&t2, Type::Any) {
            return Ok(t1);
        }

        // Handle Unknown
        if matches!(&t1, Type::Unknown) {
            return Ok(t2);
        }
        if matches!(&t2, Type::Unknown) {
            return Ok(t1);
        }

        // Structural equality
        if t1 == t2 {
            return Ok(t1);
        }

        // Structural unification for compound types
        match (&t1, &t2) {
            (
                Type::Array {
                    element: e1,
                    size: s1,
                },
                Type::Array {
                    element: e2,
                    size: s2,
                },
            ) => {
                if s1 != s2 {
                    return Err(TypeError::mismatch(t1.clone(), t2.clone()));
                }
                let unified_elem = self.unify(e1, e2)?;
                Ok(Type::Array {
                    element: Box::new(unified_elem),
                    size: *s1,
                })
            }

            (Type::Ptr(inner1), Type::Ptr(inner2)) => {
                let unified = self.unify(inner1, inner2)?;
                Ok(Type::Ptr(Box::new(unified)))
            }

            (Type::Ref(inner1), Type::Ref(inner2)) => {
                let unified = self.unify(inner1, inner2)?;
                Ok(Type::Ref(Box::new(unified)))
            }

            (Type::RefMut(inner1), Type::RefMut(inner2)) => {
                let unified = self.unify(inner1, inner2)?;
                Ok(Type::RefMut(Box::new(unified)))
            }

            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.len() {
                    return Err(TypeError::mismatch(t1.clone(), t2.clone()));
                }
                let unified: Result<Vec<_>, _> = types1
                    .iter()
                    .zip(types2.iter())
                    .map(|(a, b)| self.unify(a, b))
                    .collect();
                Ok(Type::Tuple(unified?))
            }

            (
                Type::Fn {
                    params: p1,
                    ret: r1,
                },
                Type::Fn {
                    params: p2,
                    ret: r2,
                },
            ) => {
                if p1.len() != p2.len() {
                    return Err(TypeError::mismatch(t1.clone(), t2.clone()));
                }
                let unified_params: Result<Vec<_>, _> = p1
                    .iter()
                    .zip(p2.iter())
                    .map(|(a, b)| self.unify(a, b))
                    .collect();
                let unified_ret = self.unify(r1, r2)?;
                Ok(Type::Fn {
                    params: unified_params?,
                    ret: Box::new(unified_ret),
                })
            }

            // Numeric coercion: smaller -> larger is allowed implicitly
            (Type::I8, Type::I16)
            | (Type::I8, Type::I32)
            | (Type::I8, Type::I64)
            | (Type::I16, Type::I32)
            | (Type::I16, Type::I64)
            | (Type::I32, Type::I64)
            | (Type::U8, Type::U16)
            | (Type::U8, Type::U32)
            | (Type::U8, Type::U64)
            | (Type::U16, Type::U32)
            | (Type::U16, Type::U64)
            | (Type::U32, Type::U64)
            | (Type::F32, Type::F64) => Ok(t2),

            // Reverse direction: larger -> smaller requires explicit cast
            (Type::I64, Type::I32)
            | (Type::I64, Type::I16)
            | (Type::I64, Type::I8)
            | (Type::I32, Type::I16)
            | (Type::I32, Type::I8)
            | (Type::I16, Type::I8)
            | (Type::U64, Type::U32)
            | (Type::U64, Type::U16)
            | (Type::U64, Type::U8)
            | (Type::U32, Type::U16)
            | (Type::U32, Type::U8)
            | (Type::U16, Type::U8)
            | (Type::F64, Type::F32) => {
                // For now, allow with a warning (implicit truncation)
                // In strict mode, this would be an error
                Ok(t2)
            }

            _ => Err(TypeError::mismatch(t1.clone(), t2.clone())),
        }
    }

    /// Enter a new scope (e.g., for let bindings).
    /// Currently uses flat namespace; could be extended for lexical scoping.
    pub fn push_scope(&mut self) {
        // For now, we use a flat namespace
        // Could implement proper scoping with Vec<HashMap> if needed
    }

    /// Exit a scope.
    /// Currently uses flat namespace; could be extended for lexical scoping.
    pub fn pop_scope(&mut self) {
        // For now, we use a flat namespace
    }
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_from_name() {
        assert_eq!(Type::from_name("u64"), Some(Type::U64));
        assert_eq!(Type::from_name("i32"), Some(Type::I32));
        assert_eq!(Type::from_name("bool"), Some(Type::Bool));
        assert_eq!(Type::from_name("Pubkey"), Some(Type::Pubkey));
        assert_eq!(Type::from_name("unknown_type"), None);
    }

    #[test]
    fn test_type_display() {
        assert_eq!(format!("{}", Type::U64), "u64");
        assert_eq!(
            format!(
                "{}",
                Type::Array {
                    element: Box::new(Type::U8),
                    size: 32
                }
            ),
            "[u8; 32]"
        );
        assert_eq!(format!("{}", Type::Ptr(Box::new(Type::U64))), "*u64");
    }

    #[test]
    fn test_unify_same_types() {
        let mut ctx = TypeContext::new();
        let result = ctx.unify(&Type::I64, &Type::I64);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::I64);
    }

    #[test]
    fn test_unify_with_any() {
        let mut ctx = TypeContext::new();

        // Any unifies with anything
        let result = ctx.unify(&Type::Any, &Type::U64);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::U64);

        let result = ctx.unify(&Type::I32, &Type::Any);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::I32);
    }

    #[test]
    fn test_unify_type_variable() {
        let mut ctx = TypeContext::new();
        let var = ctx.fresh_var();

        let result = ctx.unify(&var, &Type::U64);
        assert!(result.is_ok());

        // The variable should now resolve to U64
        let resolved = ctx.resolve(&var);
        assert_eq!(resolved, Type::U64);
    }

    #[test]
    fn test_unify_mismatch() {
        let mut ctx = TypeContext::new();

        // Incompatible types should fail
        let result = ctx.unify(&Type::Bool, &Type::U64);
        assert!(result.is_err());
    }

    #[test]
    fn test_type_size() {
        assert_eq!(Type::U8.size_bytes(), Some(1));
        assert_eq!(Type::U64.size_bytes(), Some(8));
        assert_eq!(Type::Pubkey.size_bytes(), Some(32));
        assert_eq!(
            Type::Array {
                element: Box::new(Type::U8),
                size: 32
            }
            .size_bytes(),
            Some(32)
        );
    }
}
