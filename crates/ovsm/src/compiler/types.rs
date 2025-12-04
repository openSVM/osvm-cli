//! # Type System for OVSM Compilation
//!
//! Static type inference and checking for OVSM programs.
//! sBPF is statically typed at the bytecode level, so we need
//! to infer types before code generation.

use crate::{Error, Expression, Program, Result, Statement};
use std::collections::HashMap;

/// OVSM Types for sBPF compilation
#[derive(Debug, Clone, PartialEq)]
pub enum OvsmType {
    /// 64-bit signed integer (native sBPF type)
    I64,
    /// 64-bit floating point (emulated via fixed-point in sBPF)
    F64,
    /// Boolean (represented as i64: 0/1)
    Bool,
    /// Null value
    Null,
    /// UTF-8 string (heap-allocated pointer)
    String,
    /// Homogeneous array
    Array(Box<OvsmType>),
    /// Object with typed fields
    Object(HashMap<String, OvsmType>),
    /// Function type: (params) -> return
    Function {
        /// Parameter types for the function
        params: Vec<OvsmType>,
        /// Return type of the function
        returns: Box<OvsmType>,
    },
    /// Unknown type (needs inference)
    Unknown,
    /// Any type (dynamic, requires runtime checks)
    Any,
    /// Solana pubkey (32 bytes)
    Pubkey,
    /// Account info pointer
    AccountInfo,
}

impl OvsmType {
    /// Get the sBPF representation size in bytes
    pub fn size_bytes(&self) -> usize {
        match self {
            OvsmType::I64 | OvsmType::F64 | OvsmType::Bool => 8,
            OvsmType::Null => 0,
            OvsmType::String => 8,          // Pointer
            OvsmType::Array(_) => 8,        // Pointer
            OvsmType::Object(_) => 8,       // Pointer
            OvsmType::Function { .. } => 8, // Function pointer
            OvsmType::Unknown | OvsmType::Any => 8,
            OvsmType::Pubkey => 32,
            OvsmType::AccountInfo => 8, // Pointer
        }
    }

    /// Check if type is a primitive (fits in register)
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            OvsmType::I64 | OvsmType::F64 | OvsmType::Bool | OvsmType::Null
        )
    }

    /// Check if type requires heap allocation
    pub fn is_heap_allocated(&self) -> bool {
        matches!(
            self,
            OvsmType::String | OvsmType::Array(_) | OvsmType::Object(_)
        )
    }
}

/// Type environment for tracking variable types
#[derive(Debug, Clone)]
pub struct TypeEnv {
    /// Stack of scopes (innermost last)
    scopes: Vec<HashMap<String, OvsmType>>,
}

impl TypeEnv {
    /// Creates a new type environment with a single global scope
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    /// Push a new scope
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Pop the current scope
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Define a variable in current scope
    pub fn define(&mut self, name: &str, ty: OvsmType) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), ty);
        }
    }

    /// Look up a variable's type
    pub fn lookup(&self, name: &str) -> Option<&OvsmType> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

/// A complete program with type-annotated statements
#[derive(Debug, Clone)]
pub struct TypedProgram {
    /// All statements in the program with their inferred types
    pub statements: Vec<TypedStatement>,
}

/// A statement with inferred type information
#[derive(Debug, Clone)]
pub struct TypedStatement {
    /// The original AST statement
    pub statement: Statement,
    /// The inferred type of this statement
    pub ty: OvsmType,
}

/// An expression with inferred type information
#[derive(Debug, Clone)]
pub struct TypedExpression {
    /// The original AST expression
    pub expression: Expression,
    /// The inferred type of this expression
    pub ty: OvsmType,
}

/// Type checker performs type inference and validation
pub struct TypeChecker {
    env: TypeEnv,
    warnings: Vec<String>,
}

impl TypeChecker {
    /// Creates a new type checker with Solana program builtins pre-defined
    pub fn new() -> Self {
        let mut env = TypeEnv::new();
        // Pre-define Solana program builtins
        env.define("accounts", OvsmType::Array(Box::new(OvsmType::AccountInfo)));
        env.define("instruction-data", OvsmType::Array(Box::new(OvsmType::I64)));

        Self {
            env,
            warnings: Vec::new(),
        }
    }

    /// Type check a program
    pub fn check(&mut self, program: &Program) -> Result<TypedProgram> {
        let mut typed_statements = Vec::new();

        for stmt in &program.statements {
            let typed = self.check_statement(stmt)?;
            typed_statements.push(typed);
        }

        Ok(TypedProgram {
            statements: typed_statements,
        })
    }

    /// Get warnings generated during type checking
    pub fn warnings(&self) -> &[String] {
        &self.warnings
    }

    fn check_statement(&mut self, stmt: &Statement) -> Result<TypedStatement> {
        let ty = match stmt {
            Statement::Expression(expr) => {
                // Check if this is a (define name value) expression
                if let Expression::ToolCall { name, args } = expr {
                    if name == "define" && args.len() == 2 {
                        // Extract variable name from first arg
                        if let Some(var_name) = self.extract_var_name(&args[0].value) {
                            let value_ty = self.infer_type(&args[1].value)?;
                            self.env.define(&var_name, value_ty.clone());
                            return Ok(TypedStatement {
                                statement: stmt.clone(),
                                ty: value_ty,
                            });
                        }
                    }
                }
                self.infer_type(expr)?
            }

            Statement::Assignment { name, value } => {
                let value_ty = self.infer_type(value)?;
                self.env.define(name, value_ty.clone());
                value_ty
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_ty = self.infer_type(condition)?;
                if cond_ty != OvsmType::Bool && cond_ty != OvsmType::Any {
                    self.warnings
                        .push(format!("Condition should be Bool, got {:?}", cond_ty));
                }

                self.env.push_scope();
                for s in then_branch {
                    self.check_statement(s)?;
                }
                self.env.pop_scope();

                if let Some(else_stmts) = else_branch {
                    self.env.push_scope();
                    for s in else_stmts {
                        self.check_statement(s)?;
                    }
                    self.env.pop_scope();
                }

                OvsmType::Null
            }

            Statement::While { condition, body } => {
                let cond_ty = self.infer_type(condition)?;
                if cond_ty != OvsmType::Bool && cond_ty != OvsmType::Any {
                    self.warnings
                        .push(format!("While condition should be Bool, got {:?}", cond_ty));
                }

                self.env.push_scope();
                for s in body {
                    self.check_statement(s)?;
                }
                self.env.pop_scope();

                OvsmType::Null
            }

            Statement::For {
                variable,
                iterable,
                body,
            } => {
                let iter_ty = self.infer_type(iterable)?;

                let elem_ty = match iter_ty {
                    OvsmType::Array(inner) => *inner,
                    OvsmType::String => OvsmType::String, // Iterating chars
                    _ => OvsmType::Any,
                };

                self.env.push_scope();
                self.env.define(variable, elem_ty);
                for s in body {
                    self.check_statement(s)?;
                }
                self.env.pop_scope();

                OvsmType::Null
            }

            Statement::Return { value } => {
                if let Some(expr) = value {
                    self.infer_type(expr)?
                } else {
                    OvsmType::Null
                }
            }

            _ => OvsmType::Null,
        };

        Ok(TypedStatement {
            statement: stmt.clone(),
            ty,
        })
    }

    /// Extract variable name from Expression::Variable
    fn extract_var_name(&self, expr: &Expression) -> Option<String> {
        match expr {
            Expression::Variable(name) => Some(name.clone()),
            _ => None,
        }
    }

    /// Infer the type of an expression
    pub fn infer_type(&mut self, expr: &Expression) -> Result<OvsmType> {
        match expr {
            Expression::IntLiteral(_) => Ok(OvsmType::I64),
            Expression::FloatLiteral(_) => Ok(OvsmType::F64),
            Expression::StringLiteral(_) => Ok(OvsmType::String),
            Expression::BoolLiteral(_) => Ok(OvsmType::Bool),
            Expression::NullLiteral => Ok(OvsmType::Null),

            Expression::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    Ok(OvsmType::Array(Box::new(OvsmType::Any)))
                } else {
                    let elem_ty = self.infer_type(&elements[0])?;
                    Ok(OvsmType::Array(Box::new(elem_ty)))
                }
            }

            Expression::ObjectLiteral(pairs) => {
                let mut fields = HashMap::new();
                for (key, value) in pairs {
                    let ty = self.infer_type(value)?;
                    fields.insert(key.clone(), ty);
                }
                Ok(OvsmType::Object(fields))
            }

            Expression::Variable(name) => self
                .env
                .lookup(name)
                .cloned()
                .ok_or_else(|| Error::runtime(format!("Undefined variable: {}", name))),

            Expression::Binary { op, left, right } => {
                let left_ty = self.infer_type(left)?;
                let right_ty = self.infer_type(right)?;

                use crate::BinaryOp;
                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        // Numeric operations
                        if left_ty == OvsmType::F64 || right_ty == OvsmType::F64 {
                            Ok(OvsmType::F64)
                        } else {
                            Ok(OvsmType::I64)
                        }
                    }
                    BinaryOp::Eq
                    | BinaryOp::NotEq
                    | BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::LtEq
                    | BinaryOp::GtEq => Ok(OvsmType::Bool),
                    BinaryOp::And | BinaryOp::Or => Ok(OvsmType::Bool),
                    _ => Ok(OvsmType::Any),
                }
            }

            Expression::Unary { op, operand } => {
                let operand_ty = self.infer_type(operand)?;
                use crate::UnaryOp;
                match op {
                    UnaryOp::Neg => Ok(operand_ty),
                    UnaryOp::Not => Ok(OvsmType::Bool),
                }
            }

            Expression::Lambda { params, body } => {
                self.env.push_scope();
                for param in params {
                    self.env.define(param, OvsmType::Any);
                }
                let return_ty = self.infer_type(body)?;
                self.env.pop_scope();

                Ok(OvsmType::Function {
                    params: vec![OvsmType::Any; params.len()],
                    returns: Box::new(return_ty),
                })
            }

            Expression::ToolCall { name, args: _ } => {
                // Built-in function return types
                match name.as_str() {
                    "length" => Ok(OvsmType::I64),
                    "range" => Ok(OvsmType::Array(Box::new(OvsmType::I64))),
                    "get" => Ok(OvsmType::Any),
                    "log" => Ok(OvsmType::Null),
                    "now" => Ok(OvsmType::I64),
                    "abs" | "sqrt" | "floor" | "ceil" => Ok(OvsmType::F64),
                    _ => Ok(OvsmType::Any),
                }
            }

            Expression::IndexAccess { array, index: _ } => {
                let array_ty = self.infer_type(array)?;
                match array_ty {
                    OvsmType::Array(inner) => Ok(*inner),
                    OvsmType::String => Ok(OvsmType::String),
                    _ => Ok(OvsmType::Any),
                }
            }

            Expression::FieldAccess { object, field } => {
                let obj_ty = self.infer_type(object)?;
                match obj_ty {
                    OvsmType::Object(fields) => fields
                        .get(field)
                        .cloned()
                        .ok_or_else(|| Error::runtime(format!("Unknown field: {}", field))),
                    _ => Ok(OvsmType::Any),
                }
            }

            Expression::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                let _cond_ty = self.infer_type(condition)?;
                let then_ty = self.infer_type(then_expr)?;
                let else_ty = self.infer_type(else_expr)?;

                if then_ty == else_ty {
                    Ok(then_ty)
                } else {
                    Ok(OvsmType::Any)
                }
            }

            Expression::Grouping(inner) => self.infer_type(inner),

            _ => Ok(OvsmType::Any),
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_env() {
        let mut env = TypeEnv::new();
        env.define("x", OvsmType::I64);
        assert_eq!(env.lookup("x"), Some(&OvsmType::I64));

        env.push_scope();
        env.define("y", OvsmType::String);
        assert_eq!(env.lookup("y"), Some(&OvsmType::String));
        assert_eq!(env.lookup("x"), Some(&OvsmType::I64)); // Still visible

        env.pop_scope();
        assert_eq!(env.lookup("y"), None); // Gone
        assert_eq!(env.lookup("x"), Some(&OvsmType::I64)); // Still there
    }

    #[test]
    fn test_type_size() {
        assert_eq!(OvsmType::I64.size_bytes(), 8);
        assert_eq!(OvsmType::Pubkey.size_bytes(), 32);
        assert!(OvsmType::I64.is_primitive());
        assert!(!OvsmType::String.is_primitive());
        assert!(OvsmType::Array(Box::new(OvsmType::I64)).is_heap_allocated());
    }

    #[test]
    fn test_define_creates_binding() {
        use crate::{SExprParser, SExprScanner};

        let source = "(define x 42)\nx";
        let mut scanner = SExprScanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let mut parser = SExprParser::new(tokens);
        let program = parser.parse().unwrap();

        eprintln!("Parsed program: {:?}", program);

        let mut checker = TypeChecker::new();
        let result = checker.check(&program);

        eprintln!("Result: {:?}", result);
        assert!(
            result.is_ok(),
            "Should type-check successfully: {:?}",
            result
        );
    }

    #[test]
    fn test_arithmetic_expression() {
        use crate::compiler::{CompileOptions, Compiler};
        use crate::{SExprParser, SExprScanner};

        // Test nested arithmetic like AMM
        let source = r#"
(define a 100)
(define b 200)
(define c (+ a b))
c
"#;
        let options = CompileOptions {
            opt_level: 0, // Disable optimization to see actual IR
            ..CompileOptions::default()
        };
        let compiler = Compiler::new(options);
        let result = compiler.compile(source);

        eprintln!("Compile result: {:?}", result);
        assert!(result.is_ok(), "Should compile: {:?}", result);

        let result = result.unwrap();
        eprintln!("IR instructions: {}", result.ir_instruction_count);
        eprintln!("sBPF instructions: {}", result.sbpf_instruction_count);

        // Should have more than 3 instructions for the arithmetic
        assert!(result.ir_instruction_count > 3, "Should have arithmetic IR");
    }
}
