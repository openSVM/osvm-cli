//! # Type Checker for OVSM Programs
//!
//! This module implements type inference and checking for OVSM source programs.
//! It supports gradual typing - untyped code works unchanged while typed code
//! gets full type checking.

use std::collections::HashMap;
use super::{Type, TypeContext, TypedStructDef, TypedField, TypeError};
use crate::parser::{Program, Statement, Expression, Argument, BinaryOp, UnaryOp};
use crate::compiler::ir::{StructDef, FieldType, PrimitiveType};

/// Type checker for OVSM programs
pub struct TypeChecker {
    ctx: TypeContext,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            ctx: TypeContext::new(),
        }
    }

    /// Import struct definitions from the IR generator
    pub fn import_struct_defs(&mut self, defs: &HashMap<String, StructDef>) {
        for (name, def) in defs {
            let typed_def = TypedStructDef {
                name: name.clone(),
                fields: def.fields.iter().map(|f| TypedField {
                    name: f.name.clone(),
                    field_type: field_type_to_type(&f.field_type),
                    offset: f.offset as usize,
                }).collect(),
                total_size: def.total_size as usize,
            };
            self.ctx.define_struct(typed_def);
        }
    }

    /// Type check a complete program
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<TypeError>> {
        // First pass: collect function and struct definitions
        for stmt in &program.statements {
            self.collect_definitions(stmt);
        }

        // Second pass: type check each statement
        for stmt in &program.statements {
            self.check_statement(stmt);
        }

        if self.ctx.has_errors() {
            Err(self.ctx.errors().to_vec())
        } else {
            Ok(())
        }
    }

    /// Collect definitions from a statement (first pass)
    fn collect_definitions(&mut self, stmt: &Statement) {
        if let Statement::Expression(expr) = stmt {
            // Check for define-struct
            if let Expression::ToolCall { name, args } = expr {
                if name == "define-struct" && !args.is_empty() {
                    if let Expression::Variable(struct_name) = &args[0].value {
                        let mut fields = Vec::new();
                        let mut offset = 0usize;

                        for arg in args.iter().skip(1) {
                            // Fields are represented as ToolCall with the field name
                            // In OVSM: (define-struct MyStruct (field1 u64) (field2 pubkey))
                            if let Expression::ToolCall { name: field_name, args: field_args } = &arg.value {
                                if !field_args.is_empty() {
                                    if let Expression::Variable(type_name) = &field_args[0].value {
                                        let field_type = Type::from_name(type_name).unwrap_or(Type::Any);
                                        let size = field_type.size_bytes().unwrap_or(8);
                                        fields.push(TypedField {
                                            name: field_name.clone(),
                                            field_type: field_type.clone(),
                                            offset,
                                        });
                                        offset += size;
                                    }
                                }
                            }
                        }

                        self.ctx.define_struct(TypedStructDef {
                            name: struct_name.clone(),
                            fields,
                            total_size: offset,
                        });
                    }
                }
            }
        }
    }

    /// Type check a statement
    fn check_statement(&mut self, stmt: &Statement) {
        if let Statement::Expression(expr) = stmt {
            self.infer_type(expr);
        }
        // For gradual typing, unhandled statement types are allowed
    }

    /// Infer the type of an expression
    pub fn infer_type(&mut self, expr: &Expression) -> Type {
        match expr {
            // === Literals ===
            Expression::IntLiteral(_) => Type::I64,
            Expression::FloatLiteral(_) => Type::F64,
            Expression::StringLiteral(_) => Type::String,
            Expression::BoolLiteral(_) => Type::Bool,
            Expression::NullLiteral => Type::Unit,

            // === Variables ===
            Expression::Variable(name) => {
                // First check if it's a type name
                if let Some(ty) = Type::from_name(name) {
                    return ty;
                }

                // Then check variable bindings
                match self.ctx.lookup_var(name) {
                    Some(ty) => ty.clone(),
                    None => {
                        // In gradual typing, unknown variables get type Any
                        Type::Any
                    }
                }
            }

            // === Binary Operations ===
            Expression::Binary { op, left, right } => {
                let left_type = self.infer_type(left);
                let right_type = self.infer_type(right);
                self.check_binary_op(op, &left_type, &right_type)
            }

            // === Unary Operations ===
            Expression::Unary { op, operand } => {
                let operand_type = self.infer_type(operand);
                self.check_unary_op(op, &operand_type)
            }

            // === Tool/Function Calls ===
            Expression::ToolCall { name, args } => {
                self.check_tool_call(name, args)
            }

            // === Arrays ===
            Expression::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    Type::Array { element: Box::new(Type::Any), size: 0 }
                } else {
                    let elem_type = self.infer_type(&elements[0]);
                    for elem in elements.iter().skip(1) {
                        let t = self.infer_type(elem);
                        if let Err(e) = self.ctx.unify(&elem_type, &t) {
                            self.ctx.record_error(e);
                        }
                    }
                    Type::Array {
                        element: Box::new(elem_type),
                        size: elements.len(),
                    }
                }
            }

            // === Objects ===
            Expression::ObjectLiteral(_) => {
                // Objects are dynamically typed for now
                Type::Any
            }

            // === Range ===
            Expression::Range { start, end } => {
                let start_type = self.infer_type(start);
                let end_type = self.infer_type(end);

                // Both should be integers
                if !matches!(start_type, Type::I64 | Type::Any) {
                    self.ctx.record_error(TypeError::new("range start must be integer"));
                }
                if !matches!(end_type, Type::I64 | Type::Any) {
                    self.ctx.record_error(TypeError::new("range end must be integer"));
                }

                Type::Array { element: Box::new(Type::I64), size: 0 } // Size unknown at compile time
            }

            // === Ternary (if-then-else) ===
            Expression::Ternary { condition, then_expr, else_expr } => {
                let cond_type = self.infer_type(condition);

                // Condition should be bool
                if !matches!(cond_type, Type::Bool | Type::Any) {
                    self.ctx.record_error(TypeError::mismatch(Type::Bool, cond_type));
                }

                let then_type = self.infer_type(then_expr);
                let else_type = self.infer_type(else_expr);

                // Try to unify branch types
                match self.ctx.unify(&then_type, &else_type) {
                    Ok(t) => t,
                    Err(_) => Type::Any, // Branches have incompatible types
                }
            }

            // === Lambda ===
            Expression::Lambda { params, body } => {
                // Create fresh type variables for untyped parameters
                let param_types: Vec<Type> = params.iter()
                    .map(|_| self.ctx.fresh_var())
                    .collect();

                // Enter a new scope and bind parameters
                self.ctx.push_scope();
                for (param, ty) in params.iter().zip(param_types.iter()) {
                    self.ctx.define_var(param, ty.clone());
                }

                // Infer body type
                let body_type = self.infer_type(body);

                self.ctx.pop_scope();

                Type::Fn {
                    params: param_types,
                    ret: Box::new(body_type),
                }
            }

            // === Field Access ===
            Expression::FieldAccess { object, field } => {
                let obj_type = self.infer_type(object);

                match &obj_type {
                    Type::Struct(struct_name) => {
                        if let Some(struct_def) = self.ctx.lookup_struct(struct_name) {
                            if let Some(f) = struct_def.fields.iter().find(|f| &f.name == field) {
                                f.field_type.clone()
                            } else {
                                self.ctx.record_error(TypeError::new(
                                    format!("struct '{}' has no field '{}'", struct_name, field)
                                ));
                                Type::Any
                            }
                        } else {
                            Type::Any
                        }
                    }
                    Type::Any => Type::Any,
                    _ => {
                        self.ctx.record_error(TypeError::new(
                            format!("cannot access field '{}' on type {}", field, obj_type)
                        ));
                        Type::Any
                    }
                }
            }

            // === Index Access ===
            Expression::IndexAccess { array, index } => {
                let arr_type = self.infer_type(array);
                let _idx_type = self.infer_type(index);

                match arr_type {
                    Type::Array { element, .. } => *element,
                    Type::Any => Type::Any,
                    _ => {
                        self.ctx.record_error(TypeError::new(
                            format!("cannot index into type {}", arr_type)
                        ));
                        Type::Any
                    }
                }
            }

            // === Grouping ===
            Expression::Grouping(inner) => self.infer_type(inner),

            // === Quasiquote/Unquote (macros) ===
            Expression::Quasiquote(_) => Type::Any,
            Expression::Unquote(inner) => self.infer_type(inner),
            Expression::UnquoteSplice(_) => Type::Array { element: Box::new(Type::Any), size: 0 },

            // === Loop ===
            Expression::Loop(_) => Type::Any, // Loop results are dynamic

            // === Catch/Throw ===
            Expression::Catch { body, .. } => {
                // Evaluate body and return last expression type
                if body.is_empty() {
                    Type::Unit
                } else {
                    for expr in body.iter().take(body.len() - 1) {
                        self.infer_type(expr);
                    }
                    self.infer_type(body.last().unwrap())
                }
            }

            Expression::Throw { value, .. } => {
                self.infer_type(value);
                Type::Never // throw never returns normally
            }

            // === Destructuring Bind ===
            Expression::DestructuringBind { pattern: _, value, body } => {
                self.infer_type(value);
                // For now, we don't track the pattern bindings
                if body.is_empty() {
                    Type::Unit
                } else {
                    for expr in body.iter().take(body.len() - 1) {
                        self.infer_type(expr);
                    }
                    self.infer_type(body.last().unwrap())
                }
            }
        }
    }

    /// Check a binary operation and return the result type
    fn check_binary_op(&mut self, op: &BinaryOp, left: &Type, right: &Type) -> Type {
        match op {
            // Arithmetic: both operands should be numeric, result is numeric
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod | BinaryOp::Pow => {
                if matches!(left, Type::Any) || matches!(right, Type::Any) {
                    return Type::Any;
                }

                if !left.is_numeric() {
                    self.ctx.record_error(TypeError::new(
                        format!("binary operation requires numeric type, found {}", left)
                    ));
                }
                if !right.is_numeric() {
                    self.ctx.record_error(TypeError::new(
                        format!("binary operation requires numeric type, found {}", right)
                    ));
                }

                // Result type is the "wider" of the two
                match self.ctx.unify(left, right) {
                    Ok(t) => t,
                    Err(_) => Type::Any,
                }
            }

            // Comparison: both operands should be same type, result is bool
            BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Lt | BinaryOp::Gt |
            BinaryOp::LtEq | BinaryOp::GtEq => {
                if !matches!(left, Type::Any) && !matches!(right, Type::Any) {
                    if let Err(e) = self.ctx.unify(left, right) {
                        self.ctx.record_error(e);
                    }
                }
                Type::Bool
            }

            // Logical: both operands should be bool, result is bool
            BinaryOp::And | BinaryOp::Or => {
                if !matches!(left, Type::Bool | Type::Any) {
                    self.ctx.record_error(TypeError::mismatch(Type::Bool, left.clone()));
                }
                if !matches!(right, Type::Bool | Type::Any) {
                    self.ctx.record_error(TypeError::mismatch(Type::Bool, right.clone()));
                }
                Type::Bool
            }

            // Membership: result is bool
            BinaryOp::In => Type::Bool,
        }
    }

    /// Check a unary operation and return the result type
    fn check_unary_op(&mut self, op: &UnaryOp, operand: &Type) -> Type {
        match op {
            UnaryOp::Neg => {
                if !operand.is_numeric() && !matches!(operand, Type::Any) {
                    self.ctx.record_error(TypeError::new(
                        format!("negation requires numeric type, found {}", operand)
                    ));
                }
                operand.clone()
            }
            UnaryOp::Not => {
                if !matches!(operand, Type::Bool | Type::Any) {
                    self.ctx.record_error(TypeError::mismatch(Type::Bool, operand.clone()));
                }
                Type::Bool
            }
        }
    }

    /// Check a tool/function call and return the result type
    fn check_tool_call(&mut self, name: &str, args: &[Argument]) -> Type {
        match name {
            // Type annotation: (: expr type)
            ":" if args.len() == 2 => {
                let expr_type = self.infer_type(&args[0].value);

                if let Expression::Variable(type_name) = &args[1].value {
                    if let Some(declared_type) = Type::from_name(type_name) {
                        if let Err(e) = self.ctx.unify(&expr_type, &declared_type) {
                            self.ctx.record_error(e);
                        }
                        return declared_type;
                    }
                }
                expr_type
            }

            // Variable definition: (define name value) or (define name : type value)
            "define" => {
                if args.len() == 2 {
                    // Untyped: (define name value)
                    if let Expression::Variable(var_name) = &args[0].value {
                        let val_type = self.infer_type(&args[1].value);
                        self.ctx.define_var(var_name, val_type.clone());
                        return val_type;
                    }
                } else if args.len() == 4 {
                    // Typed: (define name : type value)
                    if let (
                        Expression::Variable(var_name),
                        Expression::Variable(colon),
                        Expression::Variable(type_name),
                    ) = (&args[0].value, &args[1].value, &args[2].value) {
                        if colon == ":" {
                            let declared_type = Type::from_name(type_name).unwrap_or(Type::Any);
                            let val_type = self.infer_type(&args[3].value);

                            if let Err(e) = self.ctx.unify(&declared_type, &val_type) {
                                self.ctx.record_error(e);
                            }

                            self.ctx.define_var(var_name, declared_type.clone());
                            return declared_type;
                        }
                    }
                }
                Type::Unit
            }

            // Assignment: (set! name value)
            "set!" => {
                if args.len() == 2 {
                    if let Expression::Variable(var_name) = &args[0].value {
                        let val_type = self.infer_type(&args[1].value);

                        if let Some(existing_type) = self.ctx.lookup_var(var_name).cloned() {
                            if let Err(e) = self.ctx.unify(&existing_type, &val_type) {
                                self.ctx.record_error(e);
                            }
                        }

                        return val_type;
                    }
                }
                Type::Unit
            }

            // Struct field access: (struct-get StructName ptr field)
            "struct-get" if args.len() == 3 => {
                if let Expression::Variable(struct_name) = &args[0].value {
                    if let Expression::Variable(field_name) = &args[2].value {
                        // Clone the field type to avoid borrow conflicts
                        let field_type = self.ctx.lookup_struct(struct_name)
                            .and_then(|def| def.fields.iter().find(|f| f.name == *field_name))
                            .map(|f| f.field_type.clone());

                        if let Some(ty) = field_type {
                            return ty;
                        } else {
                            self.ctx.record_error(TypeError::new(
                                format!("struct '{}' has no field '{}'", struct_name, field_name)
                            ));
                        }
                    }
                }
                Type::Any
            }

            // Struct field set: (struct-set StructName ptr field value)
            "struct-set" if args.len() == 4 => {
                if let Expression::Variable(struct_name) = &args[0].value {
                    if let Expression::Variable(field_name) = &args[2].value {
                        let val_type = self.infer_type(&args[3].value);

                        // Clone the field type to avoid borrow conflicts
                        let field_type = self.ctx.lookup_struct(struct_name)
                            .and_then(|def| def.fields.iter().find(|f| f.name == *field_name))
                            .map(|f| f.field_type.clone());

                        if let Some(expected_type) = field_type {
                            if let Err(e) = self.ctx.unify(&expected_type, &val_type) {
                                self.ctx.record_error(e);
                            }
                        } else {
                            self.ctx.record_error(TypeError::new(
                                format!("struct '{}' has no field '{}'", struct_name, field_name)
                            ));
                        }
                    }
                }
                Type::Unit
            }

            // Zerocopy load: (zerocopy-load StructName account_idx field)
            "zerocopy-load" if args.len() == 3 => {
                if let Expression::Variable(struct_name) = &args[0].value {
                    if let Expression::Variable(field_name) = &args[2].value {
                        let field_type = self.ctx.lookup_struct(struct_name)
                            .and_then(|def| def.fields.iter().find(|f| f.name == *field_name))
                            .map(|f| f.field_type.clone());

                        if let Some(ty) = field_type {
                            return ty;
                        }
                    }
                }
                Type::Any
            }

            // Zerocopy store: (zerocopy-store StructName account_idx field value)
            "zerocopy-store" if args.len() == 4 => {
                if let Expression::Variable(struct_name) = &args[0].value {
                    if let Expression::Variable(field_name) = &args[2].value {
                        let val_type = self.infer_type(&args[3].value);

                        let field_type = self.ctx.lookup_struct(struct_name)
                            .and_then(|def| def.fields.iter().find(|f| f.name == *field_name))
                            .map(|f| f.field_type.clone());

                        if let Some(expected_type) = field_type {
                            if let Err(e) = self.ctx.unify(&expected_type, &val_type) {
                                self.ctx.record_error(e);
                            }
                        }
                    }
                }
                Type::Unit
            }

            // Account data pointer: returns ptr type
            "account-data-ptr" => Type::Ptr(Box::new(Type::U8)),
            "account-data-len" => Type::U64,
            "account-lamports" => Type::U64,
            "is-signer" | "is-writable" => Type::Bool,

            // Arithmetic operations (variadic)
            "+" | "-" | "*" | "/" | "%" => {
                let mut result_type = Type::I64;

                for arg in args {
                    let arg_type = self.infer_type(&arg.value);
                    match self.ctx.unify(&result_type, &arg_type) {
                        Ok(t) => result_type = t,
                        Err(_) => {
                            if matches!(arg_type, Type::F64 | Type::F32) {
                                result_type = arg_type;
                            }
                        }
                    }
                }

                result_type
            }

            // Comparison operations
            "=" | "!=" | "<" | ">" | "<=" | ">=" => Type::Bool,

            // Logical operations
            "and" | "or" => Type::Bool,
            "not" => Type::Bool,

            // Collection operations
            "length" => Type::U64,
            "nth" | "get" => {
                if !args.is_empty() {
                    let arr_type = self.infer_type(&args[0].value);
                    if let Type::Array { element, .. } = arr_type {
                        return *element;
                    }
                }
                Type::Any
            }

            // Higher-order functions
            "map" | "filter" => {
                // Returns array of same/different element type
                Type::Array { element: Box::new(Type::Any), size: 0 }
            }
            "reduce" => Type::Any,

            // Control flow that returns last expression
            "if" => {
                // (if condition then else)
                if args.len() >= 2 {
                    let cond_type = self.infer_type(&args[0].value);
                    if !matches!(cond_type, Type::Bool | Type::Any) {
                        self.ctx.record_error(TypeError::mismatch(Type::Bool, cond_type));
                    }
                    let then_type = self.infer_type(&args[1].value);
                    if args.len() >= 3 {
                        let else_type = self.infer_type(&args[2].value);
                        return self.ctx.unify(&then_type, &else_type).unwrap_or(Type::Any);
                    }
                    return then_type;
                }
                Type::Unit
            }

            "do" => {
                // (do expr1 expr2 ... exprN) returns exprN
                if args.is_empty() {
                    Type::Unit
                } else {
                    for arg in args.iter().take(args.len() - 1) {
                        self.infer_type(&arg.value);
                    }
                    self.infer_type(&args.last().unwrap().value)
                }
            }

            "let" => {
                // (let ((x val1) (y val2)) body)
                // For now, just return Any
                Type::Any
            }

            "for" | "while" => Type::Unit, // Loops return unit

            // Default: unknown function returns Any
            _ => {
                // Check if it's a known function
                if let Some(fn_type) = self.ctx.lookup_function(name).cloned() {
                    if let Type::Fn { ret, .. } = fn_type {
                        return *ret;
                    }
                }
                Type::Any
            }
        }
    }

    /// Get all accumulated errors
    pub fn errors(&self) -> &[TypeError] {
        self.ctx.errors()
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        self.ctx.has_errors()
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert IR field type to source-level Type
fn field_type_to_type(ft: &FieldType) -> Type {
    match ft {
        FieldType::Primitive(prim) => match prim {
            PrimitiveType::U8 => Type::U8,
            PrimitiveType::I8 => Type::I8,
            PrimitiveType::U16 => Type::U16,
            PrimitiveType::I16 => Type::I16,
            PrimitiveType::U32 => Type::U32,
            PrimitiveType::I32 => Type::I32,
            PrimitiveType::U64 => Type::U64,
            PrimitiveType::I64 => Type::I64,
        },
        FieldType::Pubkey => Type::Pubkey,
        FieldType::Array { element_type, count } => Type::Array {
            element: Box::new(field_type_to_type(&FieldType::Primitive(*element_type))),
            size: *count,
        },
        FieldType::Struct(name) => Type::Struct(name.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_types() {
        let mut checker = TypeChecker::new();

        assert_eq!(checker.infer_type(&Expression::IntLiteral(42)), Type::I64);
        assert_eq!(checker.infer_type(&Expression::FloatLiteral(3.14)), Type::F64);
        assert_eq!(checker.infer_type(&Expression::BoolLiteral(true)), Type::Bool);
        assert_eq!(checker.infer_type(&Expression::StringLiteral("hello".to_string())), Type::String);
    }

    #[test]
    fn test_array_type() {
        let mut checker = TypeChecker::new();

        let arr = Expression::ArrayLiteral(vec![
            Expression::IntLiteral(1),
            Expression::IntLiteral(2),
            Expression::IntLiteral(3),
        ]);

        let ty = checker.infer_type(&arr);
        assert!(matches!(ty, Type::Array { element, size: 3 } if *element == Type::I64));
    }

    #[test]
    fn test_binary_op_types() {
        let mut checker = TypeChecker::new();

        // Arithmetic returns numeric
        let add = Expression::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expression::IntLiteral(1)),
            right: Box::new(Expression::IntLiteral(2)),
        };
        assert_eq!(checker.infer_type(&add), Type::I64);

        // Comparison returns bool
        let lt = Expression::Binary {
            op: BinaryOp::Lt,
            left: Box::new(Expression::IntLiteral(1)),
            right: Box::new(Expression::IntLiteral(2)),
        };
        assert_eq!(checker.infer_type(&lt), Type::Bool);
    }

    #[test]
    fn test_gradual_typing() {
        let mut checker = TypeChecker::new();

        // Unknown variable gets type Any
        let var = Expression::Variable("unknown_var".to_string());
        assert_eq!(checker.infer_type(&var), Type::Any);

        // No errors for gradual typing
        assert!(!checker.has_errors());
    }
}
