//! IR Generator - transforms typed AST to IR
//!
//! This is the main code generation module containing all macro implementations.
//!
//! ## Module Organization
//!
//! The macro implementations are organized by domain. Use your editor's search
//! to jump to sections:
//!
//! | Section | Search Term | Line Range |
//! |---------|-------------|------------|
//! | Struct Macros | `STRUCT MACROS` | ~350-880 |
//! | Account Access | `ACCOUNT ACCESS` | ~920-1380 |
//! | Memory Operations | `MEMORY OPERATIONS` | ~1380-1500 |
//! | Logging & Debug | `LOGGING MACROS` | ~1520-1600 |
//! | System CPI | `SYSTEM PROGRAM CPI` | ~1600-2330 |
//! | SPL Token CPI | `SPL TOKEN CPI` | ~2330-2950 |
//! | System Create/Allocate | `SYSTEM ALLOCATE/ASSIGN` | ~2950-3970 |
//! | Anchor Errors | `ANCHOR ERROR HANDLING` | ~3970-4080 |
//! | PDA Operations | `PDA OPERATIONS` | ~4080-4280 |
//! | Account Assertions | `ACCOUNT ASSERTIONS` | ~4280-4480 |
//! | Zerocopy Access | `ZEROCOPY ACCESS` | ~4480-4620 |
//! | Events | `EVENT EMISSION` | ~4680-4800 |
//! | Sysvars | `SYSVAR ACCESS` | ~4800-5020 |
//! | PDA Cache | `PDA CACHE` | ~5020-5220 |
//! | Control Flow | `CONTROL FLOW` | ~5330-5480 |
//! | Helper Macros | `HELPER MACROS` | ~5480-5700 |
//!
//! ## Adding New Macros
//!
//! 1. Find the appropriate section based on functionality
//! 2. Add an `if name == "macro-name" && args.len() == N` block
//! 3. Use `self.alloc_reg()` for temp registers
//! 4. Use `self.emit(IrInstruction::...)` to generate IR
//! 5. Return `Ok(Some(result_reg))` or `Ok(None)` for void

use super::instruction::{IrInstruction, IrReg};
use super::memory_model::{
    account_layout, Alignment, MemoryError, MemoryRegion, PointerType, RegType, TypeEnv, TypedReg,
};
use super::program::{BasicBlock, IrProgram};
use super::types::{FieldType, PrimitiveType, StructDef, StructField};
use crate::compiler::types::{OvsmType, TypedProgram, TypedStatement};
use crate::types::{Type, TypeBridge, TypeContext};
use crate::{BinaryOp, Expression, Statement, UnaryOp};
use crate::{Error, Result};
use std::collections::HashMap;

/// IR Generator - transforms typed AST to IR
///
/// Now includes a `TypeEnv` for tracking register types during code generation.
/// This enables compile-time detection of:
/// - Misaligned memory access
/// - Out-of-bounds field access
/// - Type confusion (pointer vs value)
/// - Write to read-only memory
pub struct IrGenerator {
    /// Next available register
    next_reg: u32,
    /// Label counter for generating unique labels
    label_counter: u32,
    /// Variable to register mapping
    var_map: HashMap<String, IrReg>,
    /// String table
    strings: Vec<String>,
    /// Generated instructions
    instructions: Vec<IrInstruction>,
    /// Struct definitions (compile-time metadata for field layout)
    struct_defs: HashMap<String, StructDef>,
    /// Type environment for pointer provenance tracking
    type_env: TypeEnv,
    /// Bridge between source-level types and IR-level types
    type_bridge: TypeBridge,
    /// Source-level type context for bidirectional type checking
    source_type_ctx: TypeContext,
}

impl IrGenerator {
    pub fn new() -> Self {
        let mut type_env = TypeEnv::new();

        // Pre-allocate registers for Solana builtins (R1=accounts, R2=instruction-data per ABI)
        let accounts_reg = IrReg::new(1);
        let instr_data_reg = IrReg::new(2);

        // Register the types for the built-in registers
        type_env.set_type(
            accounts_reg,
            RegType::Pointer(PointerType {
                region: MemoryRegion::InputBuffer,
                bounds: None, // Size unknown until we parse header
                struct_type: None,
                offset: 0,
                alignment: super::memory_model::Alignment::Byte8,
                writable: false, // Accounts metadata region
            }),
        );

        type_env.set_type(
            instr_data_reg,
            RegType::Pointer(PointerType::instruction_data(None)),
        );

        let mut gen = Self {
            next_reg: 0,
            label_counter: 0,
            var_map: HashMap::new(),
            strings: Vec::new(),
            instructions: Vec::new(),
            struct_defs: HashMap::new(),
            type_env,
            type_bridge: TypeBridge::new(),
            source_type_ctx: TypeContext::new(),
        };

        gen.var_map.insert("accounts".to_string(), accounts_reg);
        gen.var_map
            .insert("instruction-data".to_string(), instr_data_reg);
        gen.next_reg = 3; // Start allocating from R3
        gen
    }

    /// Generate IR from typed program
    pub fn generate(&mut self, program: &TypedProgram) -> Result<IrProgram> {
        // Entry point
        self.emit(IrInstruction::Label("entry".to_string()));

        // CRITICAL: Save the accounts pointer (R1) and instruction data (R2) into
        // caller-saved registers before any syscalls clobber them.
        // Virtual reg 1,2 = R1,R2 at entry (accounts, instr data)
        // Save to virtual reg 6,7 which map to R6,R7 (callee-saved)
        let saved_accounts = IrReg::new(6);
        let saved_instr_data = IrReg::new(7);
        self.emit(IrInstruction::Move(saved_accounts, IrReg::new(1)));
        self.emit(IrInstruction::Move(saved_instr_data, IrReg::new(2)));

        // Update var_map to use the saved registers
        self.var_map.insert("accounts".to_string(), saved_accounts);
        self.var_map
            .insert("instruction-data".to_string(), saved_instr_data);

        // CRITICAL: Ensure next_reg skips past the reserved registers (6 and 7)
        // Otherwise alloc_reg() will return IrReg(6) or IrReg(7) for temporaries,
        // which will clobber the saved accounts/instruction-data pointers!
        if self.next_reg <= 7 {
            self.next_reg = 8;
        }

        // ═══════════════════════════════════════════════════════════════════════════
        // BUILD ACCOUNT OFFSET TABLE
        // ═══════════════════════════════════════════════════════════════════════════
        // Solana accounts have VARIABLE sizes based on their data length.
        // We iterate through all accounts at program start and store their
        // starting offsets in a heap table for O(1) indexed access later.
        //
        // Input format:
        //   8 bytes: num_accounts
        //   For each account:
        //     1 byte: duplicate marker (0xff if not dup)
        //     1 byte: is_signer
        //     1 byte: is_writable
        //     1 byte: is_executable
        //     4 bytes: padding
        //     32 bytes: pubkey
        //     32 bytes: owner
        //     8 bytes: lamports
        //     8 bytes: data_len
        //     data_len bytes: data
        //     10240 bytes: realloc padding
        //     alignment padding to 8 bytes
        //     8 bytes: rent_epoch
        //
        // Heap table at 0x300000000:
        //   8 bytes per account: offset from input start to account's first byte
        // ═══════════════════════════════════════════════════════════════════════════
        self.emit_account_offset_table_init(saved_accounts);

        eprintln!(
            "🔍 IR DEBUG: Generating IR for {} statements",
            program.statements.len()
        );

        // Generate IR for each statement, tracking last result
        let mut _last_result: Option<IrReg> = None;
        for (i, typed_stmt) in program.statements.iter().enumerate() {
            eprintln!("  Statement {}: {:?}", i, typed_stmt.statement);
            _last_result = self.generate_statement(&typed_stmt.statement)?;
        }

        // CRITICAL: For Solana BPF programs, always return 0 (success)
        // R0 = 0 indicates successful execution
        // The user's OVSM code runs for side effects (syscalls, state changes)
        // but the entrypoint MUST return a proper Solana exit code
        let success_reg = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(success_reg, 0));
        self.emit(IrInstruction::Return(Some(success_reg)));

        Ok(IrProgram {
            instructions: std::mem::take(&mut self.instructions),
            blocks: HashMap::new(), // Built by optimizer
            string_table: std::mem::take(&mut self.strings),
            entry_label: "entry".to_string(),
            var_registers: self.var_map.clone(),
        })
    }

    fn generate_statement(&mut self, stmt: &Statement) -> Result<Option<IrReg>> {
        match stmt {
            Statement::Expression(expr) => self.generate_expr(expr),

            Statement::Assignment { name, value } => {
                let value_reg = self
                    .generate_expr(value)?
                    .ok_or_else(|| Error::runtime("Assignment value has no result"))?;

                // Store in variable map
                self.var_map.insert(name.clone(), value_reg);
                Ok(Some(value_reg))
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_reg = self
                    .generate_expr(condition)?
                    .ok_or_else(|| Error::runtime("Condition has no result"))?;

                let then_label = self.new_label("then");
                let else_label = self.new_label("else");
                let end_label = self.new_label("endif");

                // Branch on condition
                self.emit(IrInstruction::JumpIf(cond_reg, then_label.clone()));
                self.emit(IrInstruction::Jump(else_label.clone()));

                // Then branch
                self.emit(IrInstruction::Label(then_label));
                for s in then_branch {
                    self.generate_statement(s)?;
                }
                self.emit(IrInstruction::Jump(end_label.clone()));

                // Else branch
                self.emit(IrInstruction::Label(else_label));
                if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        self.generate_statement(s)?;
                    }
                }

                // End
                self.emit(IrInstruction::Label(end_label));
                Ok(None)
            }

            Statement::While { condition, body } => {
                let loop_label = self.new_label("while");
                let end_label = self.new_label("endwhile");

                // Loop header
                self.emit(IrInstruction::Label(loop_label.clone()));

                let cond_reg = self
                    .generate_expr(condition)?
                    .ok_or_else(|| Error::runtime("While condition has no result"))?;

                self.emit(IrInstruction::JumpIfNot(cond_reg, end_label.clone()));

                // Body
                for s in body {
                    self.generate_statement(s)?;
                }

                // Loop back
                self.emit(IrInstruction::Jump(loop_label));

                // End
                self.emit(IrInstruction::Label(end_label));
                Ok(None)
            }

            Statement::For {
                variable,
                iterable,
                body,
            } => {
                // Generate iterable
                let iter_reg = self
                    .generate_expr(iterable)?
                    .ok_or_else(|| Error::runtime("For iterable has no result"))?;

                // Get length
                let len_reg = self.alloc_reg();
                self.emit(IrInstruction::Call(
                    Some(len_reg),
                    "length".to_string(),
                    vec![iter_reg],
                ));

                // Index register
                let idx_reg = self.alloc_reg();
                self.emit(IrInstruction::ConstI64(idx_reg, 0));

                let loop_label = self.new_label("for");
                let end_label = self.new_label("endfor");

                // Loop header
                self.emit(IrInstruction::Label(loop_label.clone()));

                // Check if idx < len
                let cmp_reg = self.alloc_reg();
                self.emit(IrInstruction::Lt(cmp_reg, idx_reg, len_reg));
                self.emit(IrInstruction::JumpIfNot(cmp_reg, end_label.clone()));

                // Get current element
                let elem_reg = self.alloc_reg();
                self.emit(IrInstruction::Call(
                    Some(elem_reg),
                    "get".to_string(),
                    vec![iter_reg, idx_reg],
                ));
                self.var_map.insert(variable.clone(), elem_reg);

                // Body
                for s in body {
                    self.generate_statement(s)?;
                }

                // Increment index
                let one_reg = self.alloc_reg();
                self.emit(IrInstruction::ConstI64(one_reg, 1));
                self.emit(IrInstruction::Add(idx_reg, idx_reg, one_reg));

                // Loop back
                self.emit(IrInstruction::Jump(loop_label));

                // End
                self.emit(IrInstruction::Label(end_label));
                Ok(None)
            }

            Statement::Return { value } => {
                if let Some(expr) = value {
                    let reg = self.generate_expr(expr)?;
                    self.emit(IrInstruction::Return(reg));
                } else {
                    self.emit(IrInstruction::Return(None));
                }
                Ok(None)
            }

            _ => Ok(None),
        }
    }

    fn generate_expr(&mut self, expr: &Expression) -> Result<Option<IrReg>> {
        match expr {
            Expression::IntLiteral(n) => {
                let reg = self.alloc_reg();
                self.emit(IrInstruction::ConstI64(reg, *n));
                Ok(Some(reg))
            }

            Expression::FloatLiteral(f) => {
                let reg = self.alloc_reg();
                self.emit(IrInstruction::ConstF64(reg, f.to_bits()));
                Ok(Some(reg))
            }

            Expression::StringLiteral(s) => {
                let idx = self.strings.len();
                self.strings.push(s.clone());
                let reg = self.alloc_reg();
                self.emit(IrInstruction::ConstString(reg, idx));
                Ok(Some(reg))
            }

            Expression::BoolLiteral(b) => {
                let reg = self.alloc_reg();
                self.emit(IrInstruction::ConstBool(reg, *b));
                Ok(Some(reg))
            }

            Expression::NullLiteral => {
                let reg = self.alloc_reg();
                self.emit(IrInstruction::ConstNull(reg));
                Ok(Some(reg))
            }

            Expression::Variable(name) => self
                .var_map
                .get(name)
                .copied()
                .map(Some)
                .ok_or_else(|| Error::runtime(format!("Undefined variable: {}", name))),

            Expression::Binary { op, left, right } => {
                let left_reg = self
                    .generate_expr(left)?
                    .ok_or_else(|| Error::runtime("Binary left has no result"))?;
                let right_reg = self
                    .generate_expr(right)?
                    .ok_or_else(|| Error::runtime("Binary right has no result"))?;
                let dst = self.alloc_reg();

                let instr = match op {
                    BinaryOp::Add => IrInstruction::Add(dst, left_reg, right_reg),
                    BinaryOp::Sub => IrInstruction::Sub(dst, left_reg, right_reg),
                    BinaryOp::Mul => IrInstruction::Mul(dst, left_reg, right_reg),
                    BinaryOp::Div => IrInstruction::Div(dst, left_reg, right_reg),
                    BinaryOp::Mod => IrInstruction::Mod(dst, left_reg, right_reg),
                    BinaryOp::Eq => IrInstruction::Eq(dst, left_reg, right_reg),
                    BinaryOp::NotEq => IrInstruction::Ne(dst, left_reg, right_reg),
                    BinaryOp::Lt => IrInstruction::Lt(dst, left_reg, right_reg),
                    BinaryOp::Gt => IrInstruction::Gt(dst, left_reg, right_reg),
                    BinaryOp::LtEq => IrInstruction::Le(dst, left_reg, right_reg),
                    BinaryOp::GtEq => IrInstruction::Ge(dst, left_reg, right_reg),
                    BinaryOp::And => IrInstruction::And(dst, left_reg, right_reg),
                    BinaryOp::Or => IrInstruction::Or(dst, left_reg, right_reg),
                    _ => return Err(Error::runtime(format!("Unsupported binary op: {:?}", op))),
                };
                self.emit(instr);
                Ok(Some(dst))
            }

            Expression::Unary { op, operand } => {
                let operand_reg = self
                    .generate_expr(operand)?
                    .ok_or_else(|| Error::runtime("Unary operand has no result"))?;
                let dst = self.alloc_reg();

                let instr = match op {
                    UnaryOp::Neg => IrInstruction::Neg(dst, operand_reg),
                    UnaryOp::Not => IrInstruction::Not(dst, operand_reg),
                };
                self.emit(instr);
                Ok(Some(dst))
            }

            Expression::ToolCall { name, args } => {
                // Handle (define var value) specially
                if name == "define" && args.len() == 2 {
                    if let Expression::Variable(var_name) = &args[0].value {
                        let value_reg = self
                            .generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("Define value has no result"))?;
                        self.var_map.insert(var_name.clone(), value_reg);
                        return Ok(Some(value_reg));
                    }
                }

                // Handle (set! var value) specially
                // For mutable variables, we need to emit a Move to update the existing register
                if name == "set!" && args.len() == 2 {
                    if let Expression::Variable(var_name) = &args[0].value {
                        // Get the existing register for this variable
                        let old_reg = self.var_map.get(var_name).copied().ok_or_else(|| {
                            Error::runtime(format!("Cannot set! undefined variable: {}", var_name))
                        })?;

                        // Compute the new value
                        let value_reg = self
                            .generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("Set! value has no result"))?;

                        // Emit Move instruction to copy new value into old register
                        self.emit(IrInstruction::Move(old_reg, value_reg));

                        return Ok(Some(old_reg));
                    }
                }

                // =============================================================
                // STRUCT MACROS (compile-time layout generation)
                // =============================================================

                // Handle (define-struct StructName (field1 type1) (field2 type2) ...)
                // Example: (define-struct MyState (counter u32) (owner u64) (flag u8))
                // Extended syntax:
                //   (owner pubkey)           - 32-byte Solana public key
                //   (scores [u32 10])        - Array of 10 u32s (40 bytes)
                //   (inner OtherStruct)      - Nested struct (size from struct_defs)
                // This is a compile-time macro - no code is generated, just metadata
                if name == "define-struct" && args.len() >= 2 {
                    if let Expression::Variable(struct_name) = &args[0].value {
                        let mut fields = Vec::new();
                        let mut current_offset: i64 = 0;

                        // Parse each field: (field_name type_spec)
                        for field_arg in args.iter().skip(1) {
                            if let Expression::ToolCall {
                                name: field_name,
                                args: field_args,
                            } = &field_arg.value
                            {
                                if field_args.len() == 1 {
                                    // Parse the type specification
                                    let (field_type, elem_size, arr_count) = match &field_args[0]
                                        .value
                                    {
                                        // Simple type: u8, u16, u32, u64, i8, i16, i32, i64, pubkey
                                        Expression::Variable(type_name) => {
                                            if let Some(ft) = FieldType::parse(type_name) {
                                                (ft, None, None)
                                            } else {
                                                // Check if it's a reference to another struct
                                                if self.struct_defs.contains_key(type_name) {
                                                    (
                                                        FieldType::Struct(type_name.clone()),
                                                        None,
                                                        None,
                                                    )
                                                } else {
                                                    return Err(Error::runtime(format!(
                                                        "Unknown field type '{}' in struct '{}'. Valid types: u8, u16, u32, u64, i8, i16, i32, i64, pubkey, [type count], or a defined struct name",
                                                        type_name, struct_name
                                                    )));
                                                }
                                            }
                                        }
                                        // Array type: [element_type count]
                                        Expression::ArrayLiteral(elements)
                                            if elements.len() == 2 =>
                                        {
                                            if let (
                                                Expression::Variable(elem_type),
                                                Expression::IntLiteral(count),
                                            ) = (&elements[0], &elements[1])
                                            {
                                                let primitive = PrimitiveType::parse(elem_type)
                                                    .ok_or_else(|| Error::runtime(format!(
                                                        "Array element type '{}' must be a primitive (u8-u64, i8-i64) in struct '{}'",
                                                        elem_type, struct_name
                                                    )))?;
                                                let cnt = *count as usize;
                                                (
                                                    FieldType::Array {
                                                        element_type: primitive,
                                                        count: cnt,
                                                    },
                                                    Some(primitive.size()),
                                                    Some(cnt),
                                                )
                                            } else {
                                                return Err(Error::runtime(format!(
                                                    "Array type must be [primitive_type count] in struct '{}'", struct_name
                                                )));
                                            }
                                        }
                                        _ => {
                                            return Err(Error::runtime(format!(
                                                "Invalid type specification in struct '{}'. Use: type_name, pubkey, [type count], or StructName",
                                                struct_name
                                            )));
                                        }
                                    };

                                    // Calculate size, handling nested structs
                                    let type_size = field_type.size_with_structs(&self.struct_defs);

                                    fields.push(StructField {
                                        name: field_name.clone(),
                                        field_type,
                                        offset: current_offset,
                                        element_size: elem_size,
                                        array_count: arr_count,
                                    });

                                    current_offset += type_size;
                                }
                            }
                        }

                        let struct_def = StructDef {
                            name: struct_name.clone(),
                            fields,
                            total_size: current_offset,
                        };

                        eprintln!(
                            "📦 Defined struct '{}' with {} bytes:",
                            struct_name, current_offset
                        );
                        for field in &struct_def.fields {
                            eprintln!(
                                "   +{}: {} ({:?})",
                                field.offset, field.name, field.field_type
                            );
                        }

                        self.struct_defs
                            .insert(struct_name.clone(), struct_def.clone());

                        // Sync to type environment for memory model validation
                        let mut defs = std::collections::HashMap::new();
                        defs.insert(struct_name.clone(), struct_def);
                        self.type_env.add_struct_defs(defs);

                        // define-struct produces no runtime value
                        return Ok(None);
                    }
                }

                // Handle (struct-get StructName base_ptr field_name)
                // Example: (struct-get MyState state_ptr counter)
                // Generates the appropriate Load1/2/4/8 based on field type
                // MEMORY MODEL: Validates field access and registers result type
                if name == "struct-get" && args.len() == 3 {
                    if let (Expression::Variable(struct_name), Expression::Variable(field_name)) =
                        (&args[0].value, &args[2].value)
                    {
                        let struct_def = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .clone();

                        let field = struct_def
                            .fields
                            .iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| {
                                Error::runtime(format!(
                                    "Unknown field '{}' in struct '{}'",
                                    field_name, struct_name
                                ))
                            })?;

                        let base_reg = self
                            .generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("struct-get base_ptr has no result"))?;

                        // MEMORY MODEL: Validate struct field access (checks bounds if base_reg has type info)
                        self.type_env
                            .validate_struct_field(struct_name, field_name, base_reg);

                        let dst = self.alloc_reg();
                        let offset = field.offset;
                        let field_type_clone = field.field_type.clone();

                        // Emit the appropriate load instruction based on field type
                        match &field.field_type {
                            FieldType::Primitive(PrimitiveType::U8)
                            | FieldType::Primitive(PrimitiveType::I8) => {
                                self.emit(IrInstruction::Load1(dst, base_reg, offset));
                                // Register result as u8 value
                                let signed = matches!(
                                    field.field_type,
                                    FieldType::Primitive(PrimitiveType::I8)
                                );
                                self.type_env
                                    .set_type(dst, RegType::Value { size: 1, signed });
                            }
                            FieldType::Primitive(PrimitiveType::U16)
                            | FieldType::Primitive(PrimitiveType::I16) => {
                                self.emit(IrInstruction::Load2(dst, base_reg, offset));
                                let signed = matches!(
                                    field.field_type,
                                    FieldType::Primitive(PrimitiveType::I16)
                                );
                                self.type_env
                                    .set_type(dst, RegType::Value { size: 2, signed });
                            }
                            FieldType::Primitive(PrimitiveType::U32)
                            | FieldType::Primitive(PrimitiveType::I32) => {
                                self.emit(IrInstruction::Load4(dst, base_reg, offset));
                                let signed = matches!(
                                    field.field_type,
                                    FieldType::Primitive(PrimitiveType::I32)
                                );
                                self.type_env
                                    .set_type(dst, RegType::Value { size: 4, signed });
                            }
                            FieldType::Primitive(PrimitiveType::U64)
                            | FieldType::Primitive(PrimitiveType::I64) => {
                                self.emit(IrInstruction::Load(dst, base_reg, offset));
                                let signed = matches!(
                                    field.field_type,
                                    FieldType::Primitive(PrimitiveType::I64)
                                );
                                self.type_env
                                    .set_type(dst, RegType::Value { size: 8, signed });
                            }
                            FieldType::Pubkey | FieldType::Array { .. } | FieldType::Struct(_) => {
                                // For pubkey/array/struct, return pointer to field (not value)
                                let offset_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(offset_reg, offset));
                                self.emit(IrInstruction::Add(dst, base_reg, offset_reg));

                                // Register result as pointer to the field
                                let field_size = match &field_type_clone {
                                    FieldType::Pubkey => 32,
                                    FieldType::Array {
                                        element_type,
                                        count,
                                    } => element_type.size() * (*count as i64),
                                    FieldType::Struct(nested_name) => self
                                        .struct_defs
                                        .get(nested_name)
                                        .map(|s| s.total_size)
                                        .unwrap_or(0),
                                    _ => 0,
                                };
                                // If base_reg has pointer info, derive field pointer from it
                                if let Some(RegType::Pointer(base_ptr)) =
                                    self.type_env.get_type(base_reg).cloned()
                                {
                                    let field_ptr = base_ptr.field_access(
                                        offset,
                                        field_size,
                                        field_name.clone(),
                                    );
                                    self.type_env.set_type(dst, RegType::Pointer(field_ptr));
                                } else {
                                    // Unknown base, create a generic pointer
                                    self.type_env.set_type(
                                        dst,
                                        RegType::Pointer(PointerType {
                                            region: MemoryRegion::Unknown,
                                            bounds: Some((0, field_size)),
                                            struct_type: Some(format!(
                                                "{}.{}",
                                                struct_name, field_name
                                            )),
                                            offset: 0,
                                            alignment: Alignment::Byte1,
                                            writable: true,
                                        }),
                                    );
                                }
                            }
                        }

                        return Ok(Some(dst));
                    }
                }

                // Handle (struct-set StructName base_ptr field_name value)
                // Example: (struct-set MyState state_ptr counter 42)
                // Generates the appropriate Store1/2/4/8 based on field type
                // MEMORY MODEL: Validates field access and checks writability
                if name == "struct-set" && args.len() == 4 {
                    if let (Expression::Variable(struct_name), Expression::Variable(field_name)) =
                        (&args[0].value, &args[2].value)
                    {
                        let struct_def = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .clone();

                        let field = struct_def
                            .fields
                            .iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| {
                                Error::runtime(format!(
                                    "Unknown field '{}' in struct '{}'",
                                    field_name, struct_name
                                ))
                            })?;

                        let base_reg = self
                            .generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("struct-set base_ptr has no result"))?;

                        // MEMORY MODEL: Validate struct field access and writability
                        self.type_env
                            .validate_struct_field(struct_name, field_name, base_reg);

                        // Also validate writability if base_reg has type info
                        if let Some(RegType::Pointer(ptr)) = self.type_env.get_type(base_reg) {
                            if !ptr.writable {
                                self.type_env.record_error(MemoryError::ReadOnlyWrite {
                                    region: ptr.region,
                                });
                            }
                        }

                        let value_reg = self
                            .generate_expr(&args[3].value)?
                            .ok_or_else(|| Error::runtime("struct-set value has no result"))?;

                        let offset = field.offset;

                        // Emit the appropriate store instruction based on field type
                        match &field.field_type {
                            FieldType::Primitive(PrimitiveType::U8)
                            | FieldType::Primitive(PrimitiveType::I8) => {
                                self.emit(IrInstruction::Store1(base_reg, value_reg, offset));
                            }
                            FieldType::Primitive(PrimitiveType::U16)
                            | FieldType::Primitive(PrimitiveType::I16) => {
                                self.emit(IrInstruction::Store2(base_reg, value_reg, offset));
                            }
                            FieldType::Primitive(PrimitiveType::U32)
                            | FieldType::Primitive(PrimitiveType::I32) => {
                                self.emit(IrInstruction::Store4(base_reg, value_reg, offset));
                            }
                            FieldType::Primitive(PrimitiveType::U64)
                            | FieldType::Primitive(PrimitiveType::I64) => {
                                self.emit(IrInstruction::Store(base_reg, value_reg, offset));
                            }
                            FieldType::Pubkey | FieldType::Array { .. } | FieldType::Struct(_) => {
                                // For pubkey/array/struct, value_reg is expected to be a source pointer
                                // Use memcpy-style (currently not supported - use struct-ptr and manual copy)
                                return Err(Error::runtime(format!(
                                    "Cannot use struct-set for field '{}' of type {:?}. Use struct-ptr to get a pointer and copy manually.",
                                    field_name, field.field_type
                                )));
                            }
                        }

                        return Ok(None); // Store has no result
                    }
                }

                // Handle (struct-size StructName) - returns the total size of a struct
                // Example: (struct-size MyState) => 13 (if counter=4 + owner=8 + flag=1)
                if name == "struct-size" && args.len() == 1 {
                    if let Expression::Variable(struct_name) = &args[0].value {
                        // Get the size first to avoid borrow conflict
                        let total_size = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .total_size;

                        let dst = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(dst, total_size));
                        return Ok(Some(dst));
                    }
                }

                // Handle (struct-ptr StructName base_ptr field_name)
                // Returns a pointer to a field, useful for nested structs or arrays
                // Example: (struct-ptr MyState state_ptr inner_struct)
                if name == "struct-ptr" && args.len() == 3 {
                    if let (Expression::Variable(struct_name), Expression::Variable(field_name)) =
                        (&args[0].value, &args[2].value)
                    {
                        let struct_def = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .clone();

                        let field = struct_def
                            .fields
                            .iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| {
                                Error::runtime(format!(
                                    "Unknown field '{}' in struct '{}'",
                                    field_name, struct_name
                                ))
                            })?;

                        let base_reg = self
                            .generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("struct-ptr base_ptr has no result"))?;

                        let dst = self.alloc_reg();
                        let offset_reg = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(offset_reg, field.offset));
                        self.emit(IrInstruction::Add(dst, base_reg, offset_reg));

                        return Ok(Some(dst));
                    }
                }

                // Handle (struct-offset StructName field_name)
                // Returns the compile-time offset of a field (no base pointer needed)
                // Example: (struct-offset MyState counter) => 0
                if name == "struct-offset" && args.len() == 2 {
                    if let (Expression::Variable(struct_name), Expression::Variable(field_name)) =
                        (&args[0].value, &args[1].value)
                    {
                        let struct_def = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .clone();

                        let field = struct_def
                            .fields
                            .iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| {
                                Error::runtime(format!(
                                    "Unknown field '{}' in struct '{}'",
                                    field_name, struct_name
                                ))
                            })?;

                        let dst = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(dst, field.offset));
                        return Ok(Some(dst));
                    }
                }

                // Handle (struct-field-size StructName field_name)
                // Returns the size of a specific field
                // Example: (struct-field-size MyState counter) => 4
                if name == "struct-field-size" && args.len() == 2 {
                    if let (Expression::Variable(struct_name), Expression::Variable(field_name)) =
                        (&args[0].value, &args[1].value)
                    {
                        let struct_def = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .clone();

                        let field = struct_def
                            .fields
                            .iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| {
                                Error::runtime(format!(
                                    "Unknown field '{}' in struct '{}'",
                                    field_name, struct_name
                                ))
                            })?;

                        let dst = self.alloc_reg();
                        let field_size = field.field_type.size_with_structs(&self.struct_defs);
                        self.emit(IrInstruction::ConstI64(dst, field_size));
                        return Ok(Some(dst));
                    }
                }

                // Handle (struct-idl StructName)
                // Prints the Anchor IDL JSON for a struct at compile time
                // Example: (struct-idl MyState) => prints JSON and returns 0
                if name == "struct-idl" && args.len() == 1 {
                    if let Expression::Variable(struct_name) = &args[0].value {
                        let struct_def = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .clone();

                        // Print the IDL at compile time
                        eprintln!("📋 Anchor IDL for struct '{}':", struct_name);
                        eprintln!("{}", struct_def.to_anchor_idl());

                        // Return 0 at runtime (this is a compile-time-only operation)
                        let dst = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(dst, 0));
                        return Ok(Some(dst));
                    }
                }

                // =============================================================
                // BORSH SERIALIZATION HELPERS
                // =============================================================
                // Borsh uses little-endian format which is what x86/sBPF uses natively
                // Our struct-get/set already produce the correct Borsh-compatible layout

                // Handle (borsh-serialize StructName src_ptr dst_buffer offset)
                // Serializes struct fields to a buffer in Borsh format
                // Returns the number of bytes written
                if name == "borsh-serialize" && args.len() >= 3 {
                    if let Expression::Variable(struct_name) = &args[0].value {
                        let struct_def = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .clone();

                        let src_ptr = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                            Error::runtime("borsh-serialize src_ptr has no result")
                        })?;

                        let dst_buffer = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                            Error::runtime("borsh-serialize dst_buffer has no result")
                        })?;

                        // Optional offset argument (defaults to 0)
                        let base_offset = if args.len() >= 4 {
                            match &args[3].value {
                                Expression::IntLiteral(n) => *n,
                                _ => 0,
                            }
                        } else {
                            0
                        };

                        // Copy each field from struct to buffer using native endianness (LE)
                        for field in &struct_def.fields {
                            let field_offset = field.offset;
                            let dst_offset = base_offset + field_offset;

                            // Load from source struct and store to buffer
                            match &field.field_type {
                                FieldType::Primitive(PrimitiveType::U8)
                                | FieldType::Primitive(PrimitiveType::I8) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load1(
                                        temp_reg,
                                        src_ptr,
                                        field_offset,
                                    ));
                                    self.emit(IrInstruction::Store1(
                                        dst_buffer, temp_reg, dst_offset,
                                    ));
                                }
                                FieldType::Primitive(PrimitiveType::U16)
                                | FieldType::Primitive(PrimitiveType::I16) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load2(
                                        temp_reg,
                                        src_ptr,
                                        field_offset,
                                    ));
                                    self.emit(IrInstruction::Store2(
                                        dst_buffer, temp_reg, dst_offset,
                                    ));
                                }
                                FieldType::Primitive(PrimitiveType::U32)
                                | FieldType::Primitive(PrimitiveType::I32) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load4(
                                        temp_reg,
                                        src_ptr,
                                        field_offset,
                                    ));
                                    self.emit(IrInstruction::Store4(
                                        dst_buffer, temp_reg, dst_offset,
                                    ));
                                }
                                FieldType::Primitive(PrimitiveType::U64)
                                | FieldType::Primitive(PrimitiveType::I64) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load(temp_reg, src_ptr, field_offset));
                                    self.emit(IrInstruction::Store(
                                        dst_buffer, temp_reg, dst_offset,
                                    ));
                                }
                                FieldType::Pubkey => {
                                    // Copy 32 bytes (4 x 8-byte loads/stores)
                                    for i in 0..4 {
                                        let temp_reg = self.alloc_reg();
                                        self.emit(IrInstruction::Load(
                                            temp_reg,
                                            src_ptr,
                                            field_offset + i * 8,
                                        ));
                                        self.emit(IrInstruction::Store(
                                            dst_buffer,
                                            temp_reg,
                                            dst_offset + i * 8,
                                        ));
                                    }
                                }
                                FieldType::Array {
                                    element_type,
                                    count,
                                } => {
                                    // Copy array elements
                                    let elem_size = element_type.size();
                                    for i in 0..(*count as i64) {
                                        let temp_reg = self.alloc_reg();
                                        let elem_offset = field_offset + i * elem_size;
                                        let dst_elem_offset = dst_offset + i * elem_size;
                                        match element_type {
                                            PrimitiveType::U8 | PrimitiveType::I8 => {
                                                self.emit(IrInstruction::Load1(
                                                    temp_reg,
                                                    src_ptr,
                                                    elem_offset,
                                                ));
                                                self.emit(IrInstruction::Store1(
                                                    dst_buffer,
                                                    temp_reg,
                                                    dst_elem_offset,
                                                ));
                                            }
                                            PrimitiveType::U16 | PrimitiveType::I16 => {
                                                self.emit(IrInstruction::Load2(
                                                    temp_reg,
                                                    src_ptr,
                                                    elem_offset,
                                                ));
                                                self.emit(IrInstruction::Store2(
                                                    dst_buffer,
                                                    temp_reg,
                                                    dst_elem_offset,
                                                ));
                                            }
                                            PrimitiveType::U32 | PrimitiveType::I32 => {
                                                self.emit(IrInstruction::Load4(
                                                    temp_reg,
                                                    src_ptr,
                                                    elem_offset,
                                                ));
                                                self.emit(IrInstruction::Store4(
                                                    dst_buffer,
                                                    temp_reg,
                                                    dst_elem_offset,
                                                ));
                                            }
                                            PrimitiveType::U64 | PrimitiveType::I64 => {
                                                self.emit(IrInstruction::Load(
                                                    temp_reg,
                                                    src_ptr,
                                                    elem_offset,
                                                ));
                                                self.emit(IrInstruction::Store(
                                                    dst_buffer,
                                                    temp_reg,
                                                    dst_elem_offset,
                                                ));
                                            }
                                        }
                                    }
                                }
                                FieldType::Struct(_) => {
                                    // Skip nested structs in basic Borsh serialization
                                    // Use recursive approach or manual handling
                                }
                            }
                        }

                        // Return the number of bytes written (total struct size)
                        let dst = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(dst, struct_def.total_size));
                        return Ok(Some(dst));
                    }
                }

                // Handle (borsh-deserialize StructName src_buffer dst_ptr offset)
                // Deserializes buffer to struct fields in Borsh format
                // Returns the number of bytes read
                if name == "borsh-deserialize" && args.len() >= 3 {
                    if let Expression::Variable(struct_name) = &args[0].value {
                        let struct_def = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .clone();

                        let src_buffer = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                            Error::runtime("borsh-deserialize src_buffer has no result")
                        })?;

                        let dst_ptr = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                            Error::runtime("borsh-deserialize dst_ptr has no result")
                        })?;

                        // Optional offset argument (defaults to 0)
                        let base_offset = if args.len() >= 4 {
                            match &args[3].value {
                                Expression::IntLiteral(n) => *n,
                                _ => 0,
                            }
                        } else {
                            0
                        };

                        // Copy each field from buffer to struct using native endianness (LE)
                        for field in &struct_def.fields {
                            let field_offset = field.offset;
                            let src_offset = base_offset + field_offset;

                            // Load from source buffer and store to struct
                            match &field.field_type {
                                FieldType::Primitive(PrimitiveType::U8)
                                | FieldType::Primitive(PrimitiveType::I8) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load1(
                                        temp_reg, src_buffer, src_offset,
                                    ));
                                    self.emit(IrInstruction::Store1(
                                        dst_ptr,
                                        temp_reg,
                                        field_offset,
                                    ));
                                }
                                FieldType::Primitive(PrimitiveType::U16)
                                | FieldType::Primitive(PrimitiveType::I16) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load2(
                                        temp_reg, src_buffer, src_offset,
                                    ));
                                    self.emit(IrInstruction::Store2(
                                        dst_ptr,
                                        temp_reg,
                                        field_offset,
                                    ));
                                }
                                FieldType::Primitive(PrimitiveType::U32)
                                | FieldType::Primitive(PrimitiveType::I32) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load4(
                                        temp_reg, src_buffer, src_offset,
                                    ));
                                    self.emit(IrInstruction::Store4(
                                        dst_ptr,
                                        temp_reg,
                                        field_offset,
                                    ));
                                }
                                FieldType::Primitive(PrimitiveType::U64)
                                | FieldType::Primitive(PrimitiveType::I64) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load(
                                        temp_reg, src_buffer, src_offset,
                                    ));
                                    self.emit(IrInstruction::Store(
                                        dst_ptr,
                                        temp_reg,
                                        field_offset,
                                    ));
                                }
                                FieldType::Pubkey => {
                                    // Copy 32 bytes (4 x 8-byte loads/stores)
                                    for i in 0..4 {
                                        let temp_reg = self.alloc_reg();
                                        self.emit(IrInstruction::Load(
                                            temp_reg,
                                            src_buffer,
                                            src_offset + i * 8,
                                        ));
                                        self.emit(IrInstruction::Store(
                                            dst_ptr,
                                            temp_reg,
                                            field_offset + i * 8,
                                        ));
                                    }
                                }
                                FieldType::Array {
                                    element_type,
                                    count,
                                } => {
                                    // Copy array elements
                                    let elem_size = element_type.size();
                                    for i in 0..(*count as i64) {
                                        let temp_reg = self.alloc_reg();
                                        let src_elem_offset = src_offset + i * elem_size;
                                        let dst_elem_offset = field_offset + i * elem_size;
                                        match element_type {
                                            PrimitiveType::U8 | PrimitiveType::I8 => {
                                                self.emit(IrInstruction::Load1(
                                                    temp_reg,
                                                    src_buffer,
                                                    src_elem_offset,
                                                ));
                                                self.emit(IrInstruction::Store1(
                                                    dst_ptr,
                                                    temp_reg,
                                                    dst_elem_offset,
                                                ));
                                            }
                                            PrimitiveType::U16 | PrimitiveType::I16 => {
                                                self.emit(IrInstruction::Load2(
                                                    temp_reg,
                                                    src_buffer,
                                                    src_elem_offset,
                                                ));
                                                self.emit(IrInstruction::Store2(
                                                    dst_ptr,
                                                    temp_reg,
                                                    dst_elem_offset,
                                                ));
                                            }
                                            PrimitiveType::U32 | PrimitiveType::I32 => {
                                                self.emit(IrInstruction::Load4(
                                                    temp_reg,
                                                    src_buffer,
                                                    src_elem_offset,
                                                ));
                                                self.emit(IrInstruction::Store4(
                                                    dst_ptr,
                                                    temp_reg,
                                                    dst_elem_offset,
                                                ));
                                            }
                                            PrimitiveType::U64 | PrimitiveType::I64 => {
                                                self.emit(IrInstruction::Load(
                                                    temp_reg,
                                                    src_buffer,
                                                    src_elem_offset,
                                                ));
                                                self.emit(IrInstruction::Store(
                                                    dst_ptr,
                                                    temp_reg,
                                                    dst_elem_offset,
                                                ));
                                            }
                                        }
                                    }
                                }
                                FieldType::Struct(_) => {
                                    // Skip nested structs in basic Borsh deserialization
                                    // Use recursive approach or manual handling
                                }
                            }
                        }

                        // Return the number of bytes read (total struct size)
                        let dst = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(dst, struct_def.total_size));
                        return Ok(Some(dst));
                    }
                }

                // Handle (borsh-size StructName)
                // Returns the serialized size of a struct (same as struct-size for fixed-size structs)
                if name == "borsh-size" && args.len() == 1 {
                    if let Expression::Variable(struct_name) = &args[0].value {
                        let total_size = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .total_size;

                        let dst = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(dst, total_size));
                        return Ok(Some(dst));
                    }
                }

                // Handle (get array index) - array/object access
                if name == "get" && args.len() == 2 {
                    let base_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("Get base has no result"))?;
                    let idx_reg = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("Get index has no result"))?;
                    let dst = self.alloc_reg();
                    // Calculate offset: base + idx * 8
                    let offset_reg = self.alloc_reg();
                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));
                    self.emit(IrInstruction::Mul(offset_reg, idx_reg, eight_reg));
                    let addr_reg = self.alloc_reg();
                    self.emit(IrInstruction::Add(addr_reg, base_reg, offset_reg));
                    self.emit(IrInstruction::Load(dst, addr_reg, 0));
                    return Ok(Some(dst));
                }

                // Handle (mem-load base offset) - direct memory load
                if name == "mem-load" && args.len() == 2 {
                    let base_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-load base has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => {
                            let off_reg = self
                                .generate_expr(&args[1].value)?
                                .ok_or_else(|| Error::runtime("mem-load offset has no result"))?;
                            let dst = self.alloc_reg();
                            let addr_reg = self.alloc_reg();
                            self.emit(IrInstruction::Add(addr_reg, base_reg, off_reg));
                            self.emit(IrInstruction::Load(dst, addr_reg, 0));
                            return Ok(Some(dst));
                        }
                    };
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load(dst, base_reg, offset));
                    return Ok(Some(dst));
                }

                // Handle (num-accounts) - get number of accounts from saved accounts pointer
                if name == "num-accounts" && args.is_empty() {
                    // accounts pointer was saved to virtual register 6 (R6) at entry
                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load(dst, accounts_ptr, 0));
                    return Ok(Some(dst));
                }

                // Handle (account-lamports idx) - get lamports for account at index
                // Solana serialized format (from sol_deserialize in deserialize.h):
                // After num_accounts (8 bytes), each account entry:
                //   +0:  u8  dup_info (0xFF = new, else = index)
                //   +1:  u8  is_signer
                //   +2:  u8  is_writable
                //   +3:  u8  executable
                //   +4:  4 bytes padding
                //   +8:  32 bytes pubkey
                //   +40: 32 bytes owner
                //   +72: u64 lamports (THE VALUE, not a pointer!)
                //   +80: u64 data_len
                //   +88: data_len bytes of data
                //   +88+data_len: 10240 bytes MAX_PERMITTED_DATA_INCREASE
                //   +aligned: u64 rent_epoch
                //
                // IMPORTANT: Account size is VARIABLE due to data_len!
                // For idx=0, lamports is at offset 8 + 72 = 80 from input start
                // For subsequent accounts, we'd need to iterate and sum data_lens
                //
                // For now: only support account 0 correctly
                // (account-lamports idx) - get lamport balance for account
                // Uses precomputed account offset table for dynamic account sizes
                if name == "account-lamports" && args.len() == 1 {
                    let idx_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-lamports index has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get precomputed account base offset from table
                    let account_base = self.emit_get_account_offset(idx_reg);

                    // lamports offset from account start = 1+1+1+1+4+32+32 = 72
                    let lamports_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(lamports_offset, 72));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        total_offset,
                        account_base,
                        lamports_offset,
                    ));

                    let addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(addr, accounts_ptr, total_offset));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load(dst, addr, 0));
                    return Ok(Some(dst));
                }

                // Handle (set-lamports idx value) - set lamport balance for account
                // Uses precomputed account offset table for dynamic account sizes
                if name == "set-lamports" && args.len() == 2 {
                    let idx_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("set-lamports index has no result"))?;
                    let value_reg = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("set-lamports value has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get precomputed account base offset from table
                    let account_base = self.emit_get_account_offset(idx_reg);

                    // lamports offset from account start = 72
                    let lamports_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(lamports_offset, 72));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        total_offset,
                        account_base,
                        lamports_offset,
                    ));

                    let addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(addr, accounts_ptr, total_offset));

                    // Store the new lamports value
                    self.emit(IrInstruction::Store(addr, value_reg, 0));
                    return Ok(None);
                }

                // Handle (account-executable idx) - check if account is executable (1 byte at offset 3)
                // Uses precomputed account offset table for dynamic account sizes
                if name == "account-executable" && args.len() == 1 {
                    let idx_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-executable index has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get precomputed account base offset from table
                    let account_base = self.emit_get_account_offset(idx_reg);

                    // executable is at offset 3 from account start
                    let exec_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(exec_offset, 3));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, account_base, exec_offset));

                    let addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(addr, accounts_ptr, total_offset));

                    let raw = self.alloc_reg();
                    self.emit(IrInstruction::Load(raw, addr, 0));

                    let mask = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(mask, 0xFF));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::And(dst, raw, mask));
                    return Ok(Some(dst));
                }

                // Handle (instruction-data-len) - get length of instruction data
                // In Solana sBPF V1, instruction data comes after ALL accounts in the buffer:
                //   [num_accounts: 8][account_0...][account_1...]...[account_N][instr_len: 8][instr_data...]
                //
                // Uses precomputed account offset table: the offset AFTER the last account
                // is computed by looking up offset[num_accounts] (or iterating to end).
                if name == "instruction-data-len" && args.is_empty() {
                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get the instruction data offset from the precomputed table
                    let instr_offset = self.emit_get_instruction_data_offset();

                    // Read instruction data length at that offset
                    let instr_len_addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        instr_len_addr,
                        accounts_ptr,
                        instr_offset,
                    ));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load(dst, instr_len_addr, 0));
                    return Ok(Some(dst));
                }

                // Handle (instruction-data-ptr) - get pointer to instruction data
                // Same calculation as instruction-data-len, but return ptr + 8 (skip length)
                if name == "instruction-data-ptr" && args.is_empty() {
                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get the instruction data offset from the precomputed table
                    let instr_len_offset = self.emit_get_instruction_data_offset();

                    // Skip past the length (8 bytes) to get to actual data
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));

                    let instr_data_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        instr_data_offset,
                        instr_len_offset,
                        eight,
                    ));

                    // Return pointer to instruction data
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Add(dst, accounts_ptr, instr_data_offset));

                    // Register as instruction data pointer
                    self.type_env
                        .set_type(dst, RegType::Pointer(PointerType::instruction_data(None)));

                    return Ok(Some(dst));
                }

                // Handle (account-data-ptr idx) - get pointer to account data
                // Uses precomputed account offset table for dynamic account sizes
                // Data starts at offset 88 from account start (after data_len at 80)
                if name == "account-data-ptr" && args.len() == 1 {
                    // Try to extract constant account index for type tracking
                    let account_idx: Option<u8> = match &args[0].value {
                        Expression::IntLiteral(n) => Some(*n as u8),
                        _ => None,
                    };

                    let idx_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-data-ptr index has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get precomputed account base offset from table
                    let account_base = self.emit_get_account_offset(idx_reg);

                    // Data starts at offset 88 from account start
                    // = 1+1+1+1+4+32+32+8+8 = 88
                    let data_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(data_offset, account_layout::DATA));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, account_base, data_offset));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Add(dst, accounts_ptr, total_offset));

                    // Register pointer type for this result
                    if let Some(idx) = account_idx {
                        let ptr_type = PointerType::account_data(idx, None, None);
                        self.type_env.set_type(dst, RegType::Pointer(ptr_type));
                    } else {
                        // Dynamic index - use Unknown region since we can't track statically
                        self.type_env.set_type(
                            dst,
                            RegType::Pointer(PointerType {
                                region: MemoryRegion::AccountData(255), // Marker for "unknown account"
                                bounds: None,
                                struct_type: None,
                                offset: 0,
                                alignment: super::memory_model::Alignment::Byte1,
                                writable: true,
                            }),
                        );
                    }

                    return Ok(Some(dst));
                }

                // Handle (account-data-len idx) - get data length for account
                // Uses precomputed account offset table for dynamic account sizes
                // data_len is at offset 80 from account start
                if name == "account-data-len" && args.len() == 1 {
                    let idx_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-data-len index has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get precomputed account base offset from table
                    let account_base = self.emit_get_account_offset(idx_reg);

                    // data_len is at offset 80 from account start
                    // = 1+1+1+1+4+32+32+8 = 80
                    let len_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(len_offset, 80));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, account_base, len_offset));

                    let addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(addr, accounts_ptr, total_offset));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load(dst, addr, 0));
                    return Ok(Some(dst));
                }

                // Handle (account-pubkey idx) - get pointer to 32-byte account pubkey
                // Uses precomputed account offset table for dynamic account sizes
                // Pubkey is at offset 8 from account start
                if name == "account-pubkey" && args.len() == 1 {
                    // Try to extract constant account index for type tracking
                    let account_idx: Option<u8> = match &args[0].value {
                        Expression::IntLiteral(n) => Some(*n as u8),
                        _ => None,
                    };

                    let idx_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-pubkey index has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get precomputed account base offset from table
                    let account_base = self.emit_get_account_offset(idx_reg);

                    // Pubkey offset within account = 8 (after flags and padding)
                    let pubkey_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(
                        pubkey_offset,
                        account_layout::PUBKEY,
                    ));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        total_offset,
                        account_base,
                        pubkey_offset,
                    ));

                    // Return pointer to the pubkey (not the value itself - it's 32 bytes)
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Add(dst, accounts_ptr, total_offset));

                    // Register as pointer to account pubkey field (32 bytes, read-only)
                    if let Some(idx) = account_idx {
                        self.type_env.set_type(
                            dst,
                            RegType::Pointer(PointerType::account_field(
                                idx,
                                account_layout::PUBKEY,
                                account_layout::PUBKEY_LEN,
                            )),
                        );
                    }

                    return Ok(Some(dst));
                }

                // Handle (account-owner idx) - get pointer to 32-byte account owner
                // Uses precomputed account offset table for dynamic account sizes
                // Owner is at offset 40 from account start (after pubkey)
                if name == "account-owner" && args.len() == 1 {
                    // Try to extract constant account index for type tracking
                    let account_idx: Option<u8> = match &args[0].value {
                        Expression::IntLiteral(n) => Some(*n as u8),
                        _ => None,
                    };

                    let idx_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-owner index has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get precomputed account base offset from table
                    let account_base = self.emit_get_account_offset(idx_reg);

                    // Owner offset within account = 40 (8 + 32)
                    let owner_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(owner_offset, account_layout::OWNER));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, account_base, owner_offset));

                    // Return pointer to the owner pubkey (32 bytes)
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Add(dst, accounts_ptr, total_offset));

                    // Register as pointer to account owner field (32 bytes, read-only)
                    if let Some(idx) = account_idx {
                        self.type_env.set_type(
                            dst,
                            RegType::Pointer(PointerType::account_field(
                                idx,
                                account_layout::OWNER,
                                account_layout::OWNER_LEN,
                            )),
                        );
                    }

                    return Ok(Some(dst));
                }

                // Handle (account-is-signer idx) - check if account is signer (1 byte at offset 1)
                // Uses precomputed account offset table for dynamic account sizes
                if name == "account-is-signer" && args.len() == 1 {
                    let idx_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-is-signer index has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get precomputed account base offset from table
                    let account_base = self.emit_get_account_offset(idx_reg);

                    // is_signer is at offset 1 from account start
                    let signer_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(signer_offset, 1));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        total_offset,
                        account_base,
                        signer_offset,
                    ));

                    let addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(addr, accounts_ptr, total_offset));

                    // Load 1 byte (will be 0 or 1) using proper single-byte load
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load1(dst, addr, 0));
                    return Ok(Some(dst));
                }

                // Handle (account-is-writable idx) - check if account is writable (1 byte at offset 2)
                // Uses precomputed account offset table for dynamic account sizes
                if name == "account-is-writable" && args.len() == 1 {
                    let idx_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-is-writable index has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get precomputed account base offset from table
                    let account_base = self.emit_get_account_offset(idx_reg);

                    // is_writable is at offset 2 from account start
                    let writable_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(writable_offset, 2));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        total_offset,
                        account_base,
                        writable_offset,
                    ));

                    let addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(addr, accounts_ptr, total_offset));

                    // Load 1 byte (will be 0 or 1) using proper single-byte load
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load1(dst, addr, 0));
                    return Ok(Some(dst));
                }

                // Handle (mem-load ptr offset) - load 8 bytes from memory
                // Returns: u64 value at ptr+offset
                if name == "mem-load" && args.len() == 2 {
                    let ptr_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-load ptr has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => return Err(Error::runtime("mem-load offset must be constant")),
                    };
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load(dst, ptr_reg, offset));
                    return Ok(Some(dst));
                }

                // Handle (mem-load1 ptr offset) - load 1 byte (8-bit) from memory
                // Returns: u8 value at ptr+offset (zero-extended to u64)
                if name == "mem-load1" && args.len() == 2 {
                    let ptr_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-load1 ptr has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => return Err(Error::runtime("mem-load1 offset must be constant")),
                    };
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load1(dst, ptr_reg, offset));
                    return Ok(Some(dst));
                }

                // Handle (mem-load2 ptr offset) - load 2 bytes (16-bit) from memory
                // Returns: u16 value at ptr+offset (zero-extended to u64)
                if name == "mem-load2" && args.len() == 2 {
                    let ptr_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-load2 ptr has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => return Err(Error::runtime("mem-load2 offset must be constant")),
                    };
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load2(dst, ptr_reg, offset));
                    return Ok(Some(dst));
                }

                // Handle (mem-load4 ptr offset) - load 4 bytes (32-bit) from memory
                // Returns: u32 value at ptr+offset (zero-extended to u64)
                if name == "mem-load4" && args.len() == 2 {
                    let ptr_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-load4 ptr has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => return Err(Error::runtime("mem-load4 offset must be constant")),
                    };
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load4(dst, ptr_reg, offset));
                    return Ok(Some(dst));
                }

                // Handle (mem-store base offset value) - direct memory store
                // Supports both constant and dynamic offsets
                if name == "mem-store" && args.len() == 3 {
                    let base_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-store base has no result"))?;
                    let value_reg = self
                        .generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("mem-store value has no result"))?;

                    // Try constant offset first (most common case)
                    if let Expression::IntLiteral(offset) = &args[1].value {
                        self.emit(IrInstruction::Store(base_reg, value_reg, *offset));
                    } else {
                        // Dynamic offset: compute effective address = base + offset
                        let offset_reg = self
                            .generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("mem-store offset has no result"))?;
                        let addr_reg = self.alloc_reg();
                        self.emit(IrInstruction::Add(addr_reg, base_reg, offset_reg));
                        // Store at computed address with 0 offset
                        self.emit(IrInstruction::Store(addr_reg, value_reg, 0));
                    }
                    return Ok(None); // Store has no result
                }

                // Handle (mem-store1 base offset value) - store 1 byte to memory
                // Stores the low byte of value at ptr+offset
                // Supports both constant and dynamic offsets
                if name == "mem-store1" && args.len() == 3 {
                    let base_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-store1 base has no result"))?;
                    let value_reg = self
                        .generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("mem-store1 value has no result"))?;

                    // Try constant offset first (most common case)
                    if let Expression::IntLiteral(offset) = &args[1].value {
                        self.emit(IrInstruction::Store1(base_reg, value_reg, *offset));
                    } else {
                        // Dynamic offset: compute effective address = base + offset
                        let offset_reg = self
                            .generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("mem-store1 offset has no result"))?;
                        let addr_reg = self.alloc_reg();
                        self.emit(IrInstruction::Add(addr_reg, base_reg, offset_reg));
                        // Store at computed address with 0 offset
                        self.emit(IrInstruction::Store1(addr_reg, value_reg, 0));
                    }
                    return Ok(None); // Store has no result
                }

                // Handle (mem-store2 base offset value) - store 2 bytes (16-bit) to memory
                if name == "mem-store2" && args.len() == 3 {
                    let base_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-store2 base has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => return Err(Error::runtime("mem-store2 offset must be constant")),
                    };
                    let value_reg = self
                        .generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("mem-store2 value has no result"))?;
                    self.emit(IrInstruction::Store2(base_reg, value_reg, offset));
                    return Ok(None); // Store has no result
                }

                // Handle (mem-store4 base offset value) - store 4 bytes (32-bit) to memory
                if name == "mem-store4" && args.len() == 3 {
                    let base_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-store4 base has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => return Err(Error::runtime("mem-store4 offset must be constant")),
                    };
                    let value_reg = self
                        .generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("mem-store4 value has no result"))?;
                    self.emit(IrInstruction::Store4(base_reg, value_reg, offset));
                    return Ok(None); // Store has no result
                }

                // Handle (syscall "name" args...) - Solana syscall
                if name == "syscall" && !args.is_empty() {
                    // First arg must be the syscall name as a string literal
                    let syscall_name = match &args[0].value {
                        Expression::StringLiteral(s) => s.clone(),
                        _ => {
                            return Err(Error::runtime(
                                "syscall first argument must be a string literal",
                            ))
                        }
                    };

                    // Evaluate remaining arguments
                    let mut arg_regs = Vec::new();
                    for arg in &args[1..] {
                        if let Some(reg) = self.generate_expr(&arg.value)? {
                            arg_regs.push(reg);
                        }
                    }

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(Some(dst), syscall_name, arg_regs));
                    return Ok(Some(dst));
                }

                // Handle (sol_log_ msg) - shorthand for logging syscall
                if name == "sol_log_" && args.len() == 1 {
                    // Check if the argument is a string literal
                    if let Expression::StringLiteral(ref s) = args[0].value {
                        // Get message pointer register
                        let msg_reg = self
                            .generate_expr(&args[0].value)?
                            .ok_or_else(|| Error::runtime("log message has no result"))?;

                        // sol_log_ requires: R1 = pointer, R2 = length
                        // Generate length register
                        let len_reg = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(len_reg, s.len() as i64));

                        let dst = self.alloc_reg();
                        self.emit(IrInstruction::Syscall(
                            Some(dst),
                            name.clone(),
                            vec![msg_reg, len_reg],
                        ));
                        return Ok(Some(dst));
                    } else {
                        return Err(Error::runtime(
                            "sol_log_ requires a string literal argument",
                        ));
                    }
                }

                // Handle (sol_log_64_ ...) - log up to 5 numeric values
                if name == "sol_log_64_" && !args.is_empty() && args.len() <= 5 {
                    let mut arg_regs = Vec::new();
                    for arg in args {
                        let reg = self
                            .generate_expr(&arg.value)?
                            .ok_or_else(|| Error::runtime("log argument has no result"))?;
                        arg_regs.push(reg);
                    }

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(Some(dst), name.clone(), arg_regs));
                    return Ok(Some(dst));
                }

                // Handle (sol_log_pubkey ptr) - log a 32-byte public key
                // Takes a pointer to 32 bytes and logs it in base58 format
                if name == "sol_log_pubkey" && args.len() == 1 {
                    let ptr_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("sol_log_pubkey ptr has no result"))?;

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_log_pubkey".to_string(),
                        vec![ptr_reg],
                    ));
                    return Ok(Some(dst));
                }

                // =================================================================
                // CROSS-PROGRAM INVOCATION (CPI)
                // =================================================================
                //
                // Solana CPI via sol_invoke_signed_c syscall
                //
                // Data Structures (C ABI):
                //
                // SolInstruction (40 bytes):
                //   +0:  program_id (u64 ptr to 32-byte pubkey)
                //   +8:  accounts (u64 ptr to SolAccountMeta array)
                //   +16: account_len (u64)
                //   +24: data (u64 ptr to instruction data)
                //   +32: data_len (u64)
                //
                // SolAccountMeta (34 bytes, but aligned to 40 for arrays):
                //   +0:  pubkey (u64 ptr to 32-byte pubkey)
                //   +8:  is_writable (u8)
                //   +9:  is_signer (u8)
                //   padding to align
                //
                // SolAccountInfo (88 bytes): Already in serialized input buffer
                //
                // System Program Transfer Instruction Data (12 bytes):
                //   +0: instruction index (u32) = 2 for Transfer
                //   +4: amount in lamports (u64)
                //
                // sol_invoke_signed_c signature:
                //   R1: instruction* (SolInstruction)
                //   R2: account_infos* (SolAccountInfo array from input)
                //   R3: account_infos_len
                //   R4: signers_seeds* (NULL for non-PDA signing)
                //   R5: signers_seeds_len (0 for non-PDA signing)
                //
                // =================================================================

                // Handle (system-transfer src_idx dest_idx amount) - Transfer SOL via CPI
                // src_idx: account index of source (must be signer)
                // dest_idx: account index of destination
                // amount: lamports to transfer
                if name == "system-transfer" && args.len() == 3 {
                    let src_idx = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("system-transfer src_idx has no result"))?;
                    let dest_idx = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("system-transfer dest_idx has no result"))?;
                    let amount_reg = self
                        .generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("system-transfer amount has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // System Program ID: 11111111111111111111111111111111 (all 0 bytes in binary)
                    // Stored in .rodata, need to allocate space on stack for the structure

                    // We need to build:
                    // 1. System Program pubkey (32 bytes of zeros)
                    // 2. Instruction data (12 bytes: u32=2, u64=amount)
                    // 3. SolAccountMeta array (2 entries, 34 bytes each)
                    // 4. SolInstruction struct (40 bytes)

                    // Use a fixed heap address offset from the account offset table.
                    // Account offset table uses heap[0..num_accounts*8+8], so we start
                    // CPI data at a safe offset: 0x300000000 + 256 (supports up to 31 accounts)
                    let heap_cpi_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_cpi_base, 0x300000100_i64)); // +256 bytes

                    let heap_base = heap_cpi_base;

                    // Layout in heap:
                    //   +0:   System Program ID (32 bytes of zeros)
                    //   +32:  instruction_data (12 bytes)
                    //   +48:  SolAccountMeta[0] (16 bytes: ptr, is_writable, is_signer, padding)
                    //   +64:  SolAccountMeta[1] (16 bytes)
                    //   +80:  SolInstruction (40 bytes)

                    // 1. Write System Program ID (all zeros)
                    // Store 4 u64 zeros (32 bytes total)
                    let zero_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero_reg, 0));
                    self.emit(IrInstruction::Store(heap_base, zero_reg, 0));
                    self.emit(IrInstruction::Store(heap_base, zero_reg, 8));
                    self.emit(IrInstruction::Store(heap_base, zero_reg, 16));
                    self.emit(IrInstruction::Store(heap_base, zero_reg, 24));

                    // 2. Write instruction data at +32
                    // System Transfer instruction: u32 index = 2, then u64 amount
                    let instr_data_ptr = self.alloc_reg();
                    let thirty_two_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(thirty_two_offset, 32));
                    self.emit(IrInstruction::Add(
                        instr_data_ptr,
                        heap_base,
                        thirty_two_offset,
                    ));

                    // Write transfer instruction index (2) as first 4 bytes
                    let transfer_idx = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(transfer_idx, 2));
                    self.emit(IrInstruction::Store(instr_data_ptr, transfer_idx, 0));

                    // Write amount at offset 4 (but we use 8-byte stores, so this is tricky)
                    // Actually, the System Transfer uses a specific encoding:
                    // [4 bytes: instruction variant (2)] [8 bytes: lamports]
                    // We store as u64 at offset 0 with value 2, then amount at offset 8
                    // But instruction_data needs to be: [02 00 00 00] [amount as u64 LE]
                    // Let's write the full 12 bytes correctly

                    // For proper byte layout, we need to write:
                    // Byte 0-3: 0x00000002 (little endian)
                    // Byte 4-11: amount (little endian u64)
                    //
                    // Since we can only store 8 bytes at a time, and the index is 4 bytes:
                    // Store low 8 bytes as: (amount << 32) | 2
                    let two = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(two, 2));
                    let amount_shifted = self.alloc_reg();
                    // amount_shifted = amount << 32
                    // We need a shift instruction - but IR doesn't have one yet
                    // Workaround: multiply by 2^32 = 4294967296
                    let shift_amount = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(shift_amount, 4294967296)); // 2^32
                    self.emit(IrInstruction::Mul(amount_shifted, amount_reg, shift_amount));
                    let instr_low = self.alloc_reg();
                    self.emit(IrInstruction::Or(instr_low, amount_shifted, two));
                    self.emit(IrInstruction::Store(instr_data_ptr, instr_low, 0));

                    // Store high 4 bytes of amount at offset 8
                    // amount_high = amount >> 32
                    let amount_high = self.alloc_reg();
                    self.emit(IrInstruction::Div(amount_high, amount_reg, shift_amount));
                    self.emit(IrInstruction::Store(instr_data_ptr, amount_high, 8));

                    // 3. Build SolAccountMeta array at +48
                    // Each SolAccountMeta: pubkey_ptr (8), is_writable (1), is_signer (1), padding (6)
                    // Total: 16 bytes each

                    let meta_array_ptr = self.alloc_reg();
                    let forty_eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(forty_eight, 48));
                    self.emit(IrInstruction::Add(meta_array_ptr, heap_base, forty_eight));

                    // Get source account pubkey pointer using dynamic offset table
                    let src_base = self.emit_get_account_offset(src_idx);
                    // Pubkey is at offset 8 from account start
                    let pubkey_field_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(pubkey_field_offset, 8));
                    let src_pubkey_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        src_pubkey_offset,
                        src_base,
                        pubkey_field_offset,
                    ));
                    let src_pubkey_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        src_pubkey_ptr,
                        accounts_ptr,
                        src_pubkey_offset,
                    ));

                    // Meta[0]: source (signer, writable)
                    self.emit(IrInstruction::Store(meta_array_ptr, src_pubkey_ptr, 0)); // pubkey ptr
                    let one_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one_reg, 1));
                    // is_writable (1) and is_signer (1) at bytes 8 and 9
                    // Store as single u64: 0x0101 at offset 8
                    let flags = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(flags, 0x0101)); // is_writable=1, is_signer=1
                    self.emit(IrInstruction::Store(meta_array_ptr, flags, 8));

                    // Get dest account pubkey pointer using dynamic offset table
                    let dest_base = self.emit_get_account_offset(dest_idx);
                    let dest_pubkey_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        dest_pubkey_offset,
                        dest_base,
                        pubkey_field_offset,
                    ));
                    let dest_pubkey_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        dest_pubkey_ptr,
                        accounts_ptr,
                        dest_pubkey_offset,
                    ));

                    // Meta[1]: dest (writable, not signer)
                    let meta1_ptr = self.alloc_reg();
                    let sixteen = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(sixteen, 16));
                    self.emit(IrInstruction::Add(meta1_ptr, meta_array_ptr, sixteen));
                    self.emit(IrInstruction::Store(meta1_ptr, dest_pubkey_ptr, 0)); // pubkey ptr
                    let flags_writable = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(flags_writable, 0x0001)); // is_writable=1, is_signer=0
                    self.emit(IrInstruction::Store(meta1_ptr, flags_writable, 8));

                    // 4. Build SolInstruction at +80
                    let instr_ptr = self.alloc_reg();
                    let eighty = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eighty, 80));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, eighty));

                    // SolInstruction.program_id = ptr to System Program ID (heap_base + 0)
                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 0));

                    // SolInstruction.accounts = ptr to SolAccountMeta array
                    self.emit(IrInstruction::Store(instr_ptr, meta_array_ptr, 8));

                    // SolInstruction.account_len = 2
                    self.emit(IrInstruction::Store(instr_ptr, two, 16));

                    // SolInstruction.data = ptr to instruction data
                    self.emit(IrInstruction::Store(instr_ptr, instr_data_ptr, 24));

                    // SolInstruction.data_len = 12
                    let twelve = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(twelve, 12));
                    self.emit(IrInstruction::Store(instr_ptr, twelve, 32));

                    // 5. Build account_infos array for CPI
                    // We need to pass pointers to the serialized account data
                    // The runtime expects SolAccountInfo* array, but we have raw serialized data
                    //
                    // CRITICAL: The CPI syscall expects the SAME account_infos format as program entry!
                    // So we can reuse the accounts_ptr directly if we include both accounts
                    //
                    // Actually, we need to build proper SolAccountInfo structs (88 bytes each):
                    //   +0:  key* (ptr to 32-byte pubkey)
                    //   +8:  lamports* (ptr to u64)
                    //   +16: data_len
                    //   +24: data* (ptr to account data)
                    //   +32: owner* (ptr to 32-byte owner pubkey)
                    //   +40: rent_epoch
                    //   +48: is_signer (bool as u64)
                    //   +56: is_writable (bool as u64)
                    //   +64: executable (bool as u64)
                    //
                    // For CPI, we can pass the original serialized input buffer's accounts
                    // since it contains SolAccountInfo-compatible data

                    // Read num_accounts from the serialized buffer
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));

                    // The accounts_ptr + 8 points to the first account's serialized data
                    // But the CPI expects an array of SolAccountInfo pointers, not raw data!
                    //
                    // We need to construct this array. Let's allocate more heap space.
                    //
                    // Actually, looking at Solana's implementation more carefully:
                    // sol_invoke_signed_c expects:
                    //   R2: const SolAccountInfo* account_infos
                    //
                    // The account_infos we receive at program entry are already in this format!
                    // We just need to pass the pointer to where our SolAccountInfo array starts.
                    //
                    // Wait - the serialized format IS different from SolAccountInfo!
                    // Serialized: [dup_info, is_signer, is_writable, executable, padding, pubkey, owner, lamports, data_len, data, rent_epoch]
                    // SolAccountInfo: different layout with pointers
                    //
                    // The CPI syscall actually handles re-serialization internally.
                    // We pass the instruction + account infos, and the runtime handles the rest.
                    //
                    // For SVM v2, the account_infos parameter expects raw pointers to
                    // our input buffer's serialized accounts!

                    // Let's simplify: pass accounts_ptr + 8 as the account_infos
                    let eight_for_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_for_offset, 8));
                    let acct_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        acct_infos_ptr,
                        accounts_ptr,
                        eight_for_offset,
                    ));

                    // 6. Call sol_invoke_signed_c
                    // R1: instruction*
                    // R2: account_infos*
                    // R3: account_infos_len
                    // R4: signers_seeds* (NULL)
                    // R5: signers_seeds_len (0)

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, acct_infos_ptr, num_accounts, zero_reg, zero_reg],
                    ));

                    return Ok(Some(dst));
                }

                // Handle (invoke instruction-ptr account-infos-ptr num-accounts) - Low-level CPI
                // For advanced users who build their own instruction structures
                if name == "invoke" && args.len() == 3 {
                    let instr_ptr = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("invoke instruction-ptr has no result"))?;
                    let acct_infos_ptr = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("invoke account-infos-ptr has no result"))?;
                    let num_accounts = self
                        .generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("invoke num-accounts has no result"))?;

                    let zero_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero_reg, 0));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, acct_infos_ptr, num_accounts, zero_reg, zero_reg],
                    ));

                    return Ok(Some(dst));
                }

                // Handle (invoke-signed instr-ptr acct-infos-ptr num-accts signers-seeds-ptr num-signers)
                // For PDA-signed CPIs
                if name == "invoke-signed" && args.len() == 5 {
                    let instr_ptr = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("invoke-signed instruction-ptr has no result")
                    })?;
                    let acct_infos_ptr = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("invoke-signed account-infos-ptr has no result")
                    })?;
                    let num_accounts = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                        Error::runtime("invoke-signed num-accounts has no result")
                    })?;
                    let signers_seeds_ptr =
                        self.generate_expr(&args[3].value)?.ok_or_else(|| {
                            Error::runtime("invoke-signed signers-seeds-ptr has no result")
                        })?;
                    let num_signers = self
                        .generate_expr(&args[4].value)?
                        .ok_or_else(|| Error::runtime("invoke-signed num-signers has no result"))?;

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![
                            instr_ptr,
                            acct_infos_ptr,
                            num_accounts,
                            signers_seeds_ptr,
                            num_signers,
                        ],
                    ));

                    return Ok(Some(dst));
                }

                // Handle (cpi-call program-idx discriminator) - High-level CPI helper
                // Builds instruction and invokes another program
                // Uses heap memory at 0x300000500 for instruction structure
                // SolInstruction layout (40 bytes):
                //   +0:  pubkey* program_id (8 bytes)
                //   +8:  SolAccountMeta* accounts (8 bytes)
                //   +16: u64 accounts_len (8 bytes)
                //   +24: u8* data (8 bytes)
                //   +32: u64 data_len (8 bytes)
                if name == "cpi-call" && args.len() >= 2 {
                    // Use heap region 0x300000500 for CPI structures
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000500_i64));

                    // Get program pubkey from account index
                    let program_idx = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("cpi-call program-idx has no result"))?;

                    // Get discriminator
                    let discriminator = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("cpi-call discriminator has no result"))?;

                    // Store discriminator as instruction data at heap+100
                    // Store(base, value, offset) signature
                    let data_ptr_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(data_ptr_reg, 0x300000564_i64)); // heap + 100
                    self.emit(IrInstruction::Store(data_ptr_reg, discriminator, 0));

                    // Build SolInstruction at heap_base
                    // program_id pointer = account_pubkey(program_idx)
                    let program_pk = self.alloc_reg();
                    let pk_offset = self.alloc_reg();
                    // Account pubkey offset in input: 8 + account_idx * 64 + 8
                    // Simplified: use input_ptr base calculation
                    self.emit(IrInstruction::ConstI64(pk_offset, 8));
                    let acct_offset = self.alloc_reg();
                    let sixty_four = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(sixty_four, 64));
                    self.emit(IrInstruction::Mul(acct_offset, program_idx, sixty_four));
                    self.emit(IrInstruction::Add(pk_offset, pk_offset, acct_offset));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    self.emit(IrInstruction::Add(pk_offset, pk_offset, eight));
                    // R1 is input ptr
                    let r1 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(r1, 1)); // R1 holds input ptr at entry
                    self.emit(IrInstruction::Add(program_pk, r1, pk_offset));

                    // Store(base, value, offset) - store program_id pointer at heap_base+0
                    self.emit(IrInstruction::Store(heap_base, program_pk, 0));

                    // accounts pointer = NULL for now (empty accounts)
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    self.emit(IrInstruction::Store(heap_base, zero, 8)); // accounts ptr
                    self.emit(IrInstruction::Store(heap_base, zero, 16)); // accounts_len = 0

                    // data pointer and length
                    self.emit(IrInstruction::Store(heap_base, data_ptr_reg, 24)); // data ptr
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Store(heap_base, one, 32)); // data_len = 1

                    // Now invoke: sol_invoke_signed_c(instr, acct_infos, num_accts, seeds, num_seeds)
                    // For simplicity, pass 0 accounts and no signers
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![heap_base, zero, zero, zero, zero],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // ENHANCED CPI: cpi-invoke with accounts and instruction data
                // =================================================================
                // (cpi-invoke program-idx data-ptr data-len [[acct-idx writable signer] ...])
                //
                // Example usage for attestation.VerifyThreshold:
                //   (cpi-invoke 4 instr-data-ptr 4 [[3 1 0] [2 0 0]])
                //   - program-idx: 4 (attestation program)
                //   - data-ptr: pointer to instruction data (4 bytes: [2, min_tasks, min_rating, max_decay])
                //   - data-len: 4
                //   - accounts: [[3 1 0] [2 0 0]] = [output_buffer writable=1 signer=0, nft readable]
                //
                // Memory layout at heap 0x300000700:
                //   +0:    SolAccountMeta array (16 bytes each: pubkey_ptr, is_writable, is_signer, padding)
                //   +256:  SolInstruction struct (40 bytes)
                // =================================================================
                if name == "cpi-invoke" && args.len() >= 3 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000700_i64));

                    // Get program account index
                    let program_idx = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("cpi-invoke program-idx has no result"))?;

                    // Get instruction data pointer
                    let data_ptr = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("cpi-invoke data-ptr has no result"))?;

                    // Get instruction data length
                    let data_len = self
                        .generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("cpi-invoke data-len has no result"))?;

                    // Get program pubkey pointer (account pubkey at program_idx)
                    // Uses dynamic offset table for variable-size accounts
                    let program_pk_ptr = self.emit_get_account_pubkey_ptr(program_idx);

                    // Process accounts array if provided (arg 3)
                    let mut num_accounts = 0usize;
                    if args.len() > 3 {
                        // Parse accounts array: [[idx writable signer] ...]
                        if let Expression::ArrayLiteral(account_specs) = &args[3].value {
                            num_accounts = account_specs.len();

                            for (i, spec) in account_specs.iter().enumerate() {
                                if let Expression::ArrayLiteral(triple) = spec {
                                    if triple.len() >= 3 {
                                        // Get account index
                                        let acct_idx =
                                            self.generate_expr(&triple[0])?.ok_or_else(|| {
                                                Error::runtime(
                                                    "cpi-invoke account idx has no result",
                                                )
                                            })?;
                                        // Get is_writable
                                        let is_writable =
                                            self.generate_expr(&triple[1])?.ok_or_else(|| {
                                                Error::runtime(
                                                    "cpi-invoke is_writable has no result",
                                                )
                                            })?;
                                        // Get is_signer
                                        let is_signer =
                                            self.generate_expr(&triple[2])?.ok_or_else(|| {
                                                Error::runtime("cpi-invoke is_signer has no result")
                                            })?;

                                        // Calculate pubkey pointer for this account using dynamic offset table
                                        let acct_pk_ptr =
                                            self.emit_get_account_pubkey_ptr(acct_idx);

                                        // Write SolAccountMeta at heap_base + i*16
                                        // SolAccountMeta: pubkey* (8), is_writable (1), is_signer (1), padding (6)
                                        let meta_offset = (i * 16) as i64;
                                        self.emit(IrInstruction::Store(
                                            heap_base,
                                            acct_pk_ptr,
                                            meta_offset,
                                        ));
                                        // Store writable and signer as bytes at +8 and +9
                                        // We need to combine them into a u64 for the store
                                        let shift_8 = self.alloc_reg();
                                        self.emit(IrInstruction::ConstI64(shift_8, 256)); // 2^8
                                        let signer_shifted = self.alloc_reg();
                                        self.emit(IrInstruction::Mul(
                                            signer_shifted,
                                            is_signer,
                                            shift_8,
                                        ));
                                        let flags_combined = self.alloc_reg();
                                        self.emit(IrInstruction::Or(
                                            flags_combined,
                                            is_writable,
                                            signer_shifted,
                                        ));
                                        self.emit(IrInstruction::Store(
                                            heap_base,
                                            flags_combined,
                                            meta_offset + 8,
                                        ));
                                    }
                                }
                            }
                        }
                    }

                    // Build SolInstruction at heap_base + 256
                    let instr_ptr = self.alloc_reg();
                    let two_fifty_six = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(two_fifty_six, 256));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, two_fifty_six));

                    // SolInstruction layout (40 bytes):
                    //   +0:  program_id* (8)
                    //   +8:  accounts* (8)
                    //   +16: accounts_len (8)
                    //   +24: data* (8)
                    //   +32: data_len (8)
                    self.emit(IrInstruction::Store(instr_ptr, program_pk_ptr, 0)); // program_id

                    if num_accounts > 0 {
                        self.emit(IrInstruction::Store(instr_ptr, heap_base, 8));
                    // accounts ptr
                    } else {
                        let zero = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(zero, 0));
                        self.emit(IrInstruction::Store(instr_ptr, zero, 8));
                    }

                    let num_accts_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_accts_reg, num_accounts as i64));
                    self.emit(IrInstruction::Store(instr_ptr, num_accts_reg, 16)); // accounts_len

                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24)); // data
                    self.emit(IrInstruction::Store(instr_ptr, data_len, 32)); // data_len

                    // Get account_infos pointer from accounts (accounts_ptr + 8 points to first account)
                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available for cpi-invoke"))?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));

                    // Get number of accounts from input header (at accounts_ptr + 0)
                    let num_input_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_input_accounts, accounts_ptr, 0));

                    // Invoke: sol_invoke_signed_c(instr*, account_infos*, num_accounts, seeds*, num_seeds)
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, account_infos_ptr, num_input_accounts, zero, zero],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // CPI-INVOKE-SIGNED: CPI with PDA signer seeds
                // =================================================================
                // (cpi-invoke-signed program-idx data-ptr data-len accounts signers)
                //
                // accounts: [[acct-idx writable signer] ...]
                // signers:  [[[seed1-ptr seed1-len] [seed2-ptr seed2-len]] ...]
                //
                // Each inner array in signers represents seeds for one PDA signer.
                // The PDA must be marked as signer in the accounts array.
                //
                // Memory layout at heap 0x300000700:
                //   +0:      SolAccountMeta array (16 bytes each)
                //   +256:    SolInstruction struct (40 bytes)
                //   +512:    SolSignerSeeds array (16 bytes each: addr*, len)
                //   +768:    SolSignerSeed arrays (16 bytes each: addr*, len)
                //
                // Example: PDA transfer with seeds ["escrow", job_id, bump]
                //   (cpi-invoke-signed
                //     system-program-idx
                //     transfer-instr-ptr
                //     12
                //     [[pda-idx 1 1] [to-idx 1 0]]
                //     [[[escrow-ptr 6] [job-id-ptr 8] [bump-ptr 1]]])
                // =================================================================
                if name == "cpi-invoke-signed" && args.len() >= 5 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000700_i64));

                    // Get program account index
                    let program_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("cpi-invoke-signed program-idx has no result")
                    })?;

                    // Get instruction data pointer
                    let data_ptr = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("cpi-invoke-signed data-ptr has no result")
                    })?;

                    // Get instruction data length
                    let data_len = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                        Error::runtime("cpi-invoke-signed data-len has no result")
                    })?;

                    // Get program pubkey pointer using dynamic offset table
                    let program_pk_ptr = self.emit_get_account_pubkey_ptr(program_idx);

                    // Process accounts array (arg 3)
                    let mut num_accounts = 0usize;
                    if let Expression::ArrayLiteral(account_specs) = &args[3].value {
                        num_accounts = account_specs.len();

                        for (i, spec) in account_specs.iter().enumerate() {
                            if let Expression::ArrayLiteral(triple) = spec {
                                if triple.len() >= 3 {
                                    let acct_idx =
                                        self.generate_expr(&triple[0])?.ok_or_else(|| {
                                            Error::runtime(
                                                "cpi-invoke-signed account idx has no result",
                                            )
                                        })?;
                                    let is_writable =
                                        self.generate_expr(&triple[1])?.ok_or_else(|| {
                                            Error::runtime(
                                                "cpi-invoke-signed is_writable has no result",
                                            )
                                        })?;
                                    let is_signer =
                                        self.generate_expr(&triple[2])?.ok_or_else(|| {
                                            Error::runtime(
                                                "cpi-invoke-signed is_signer has no result",
                                            )
                                        })?;

                                    // Calculate pubkey pointer using dynamic offset table
                                    let acct_pk_ptr = self.emit_get_account_pubkey_ptr(acct_idx);

                                    // Write SolAccountMeta at heap_base + i*16
                                    let meta_offset = (i * 16) as i64;
                                    self.emit(IrInstruction::Store(
                                        heap_base,
                                        acct_pk_ptr,
                                        meta_offset,
                                    ));
                                    let shift_8 = self.alloc_reg();
                                    self.emit(IrInstruction::ConstI64(shift_8, 256));
                                    let signer_shifted = self.alloc_reg();
                                    self.emit(IrInstruction::Mul(
                                        signer_shifted,
                                        is_signer,
                                        shift_8,
                                    ));
                                    let flags_combined = self.alloc_reg();
                                    self.emit(IrInstruction::Or(
                                        flags_combined,
                                        is_writable,
                                        signer_shifted,
                                    ));
                                    self.emit(IrInstruction::Store(
                                        heap_base,
                                        flags_combined,
                                        meta_offset + 8,
                                    ));
                                }
                            }
                        }
                    }

                    // Build SolInstruction at heap_base + 256
                    let instr_ptr = self.alloc_reg();
                    let const_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_256, 256));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_256));

                    self.emit(IrInstruction::Store(instr_ptr, program_pk_ptr, 0)); // program_id
                    if num_accounts > 0 {
                        self.emit(IrInstruction::Store(instr_ptr, heap_base, 8));
                    // accounts ptr
                    } else {
                        let zero = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(zero, 0));
                        self.emit(IrInstruction::Store(instr_ptr, zero, 8));
                    }
                    let num_accts_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_accts_reg, num_accounts as i64));
                    self.emit(IrInstruction::Store(instr_ptr, num_accts_reg, 16)); // accounts_len
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24)); // data
                    self.emit(IrInstruction::Store(instr_ptr, data_len, 32)); // data_len

                    // =================================================================
                    // Process PDA signer seeds (arg 4)
                    // =================================================================
                    // signers: [[[seed1-ptr seed1-len] ...] ...]
                    //
                    // Memory layout:
                    //   heap_base + 512: SolSignerSeeds array (16 bytes each)
                    //   heap_base + 768: SolSignerSeed individual entries (16 bytes each)
                    //
                    // SolSignerSeeds: { addr: *SolSignerSeed, len: u64 }
                    // SolSignerSeed:  { addr: *u8, len: u64 }
                    // =================================================================
                    let mut num_signers = 0usize;
                    let signer_seeds_base = self.alloc_reg();
                    let const_512 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_512, 512));
                    self.emit(IrInstruction::Add(signer_seeds_base, heap_base, const_512));

                    let seed_entries_base = self.alloc_reg();
                    let const_768 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_768, 768));
                    self.emit(IrInstruction::Add(seed_entries_base, heap_base, const_768));

                    let mut total_seed_entries = 0usize;

                    if let Expression::ArrayLiteral(signers) = &args[4].value {
                        num_signers = signers.len();

                        for (signer_idx, signer_seeds) in signers.iter().enumerate() {
                            if let Expression::ArrayLiteral(seeds) = signer_seeds {
                                let num_seeds_this_signer = seeds.len();

                                // Write SolSignerSeeds entry at signer_seeds_base + signer_idx*16
                                // { addr: pointer to first SolSignerSeed, len: number of seeds }
                                let signer_entry_offset = (signer_idx * 16) as i64;

                                // Calculate pointer to this signer's first SolSignerSeed entry
                                let seeds_ptr_offset = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(
                                    seeds_ptr_offset,
                                    (total_seed_entries * 16) as i64,
                                ));
                                let seeds_ptr = self.alloc_reg();
                                self.emit(IrInstruction::Add(
                                    seeds_ptr,
                                    seed_entries_base,
                                    seeds_ptr_offset,
                                ));

                                // Store addr and len in SolSignerSeeds
                                self.emit(IrInstruction::Store(
                                    signer_seeds_base,
                                    seeds_ptr,
                                    signer_entry_offset,
                                ));
                                let num_seeds_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(
                                    num_seeds_reg,
                                    num_seeds_this_signer as i64,
                                ));
                                self.emit(IrInstruction::Store(
                                    signer_seeds_base,
                                    num_seeds_reg,
                                    signer_entry_offset + 8,
                                ));

                                // Now write each SolSignerSeed entry
                                for (seed_idx, seed) in seeds.iter().enumerate() {
                                    if let Expression::ArrayLiteral(seed_pair) = seed {
                                        if seed_pair.len() >= 2 {
                                            let seed_addr = self
                                                .generate_expr(&seed_pair[0])?
                                                .ok_or_else(|| {
                                                    Error::runtime(
                                                        "cpi-invoke-signed seed addr has no result",
                                                    )
                                                })?;
                                            let seed_len = self
                                                .generate_expr(&seed_pair[1])?
                                                .ok_or_else(|| {
                                                    Error::runtime(
                                                        "cpi-invoke-signed seed len has no result",
                                                    )
                                                })?;

                                            // Write at seed_entries_base + (total_seed_entries + seed_idx) * 16
                                            let entry_offset =
                                                ((total_seed_entries + seed_idx) * 16) as i64;
                                            self.emit(IrInstruction::Store(
                                                seed_entries_base,
                                                seed_addr,
                                                entry_offset,
                                            ));
                                            self.emit(IrInstruction::Store(
                                                seed_entries_base,
                                                seed_len,
                                                entry_offset + 8,
                                            ));
                                        }
                                    }
                                }

                                total_seed_entries += num_seeds_this_signer;
                            }
                        }
                    }

                    // Get account_infos pointer from accounts
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for cpi-invoke-signed")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));

                    // Get number of accounts from input header
                    let num_input_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_input_accounts, accounts_ptr, 0));

                    // Invoke with signer seeds
                    let num_signers_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_signers_reg, num_signers as i64));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![
                            instr_ptr,
                            account_infos_ptr,
                            num_input_accounts,
                            signer_seeds_base,
                            num_signers_reg,
                        ],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // SPL-TOKEN-TRANSFER: High-level helper for SPL Token transfers
                // =================================================================
                // (spl-token-transfer token-prog-idx source-idx dest-idx authority-idx amount)
                // (spl-token-transfer-signed token-prog-idx source-idx dest-idx authority-idx amount seeds)
                //
                // Builds and executes SPL Token Transfer instruction via CPI.
                // Instruction data: [3, amount (8 bytes)] = 9 bytes (discriminator 3 = Transfer)
                //
                // Accounts (in order):
                //   - Source token account (writable)
                //   - Destination token account (writable)
                //   - Authority (signer) - owner of source account
                //
                // Example usage:
                //   (spl-token-transfer 5 0 1 2 1000000)  ;; Transfer 1M tokens
                //   (spl-token-transfer-signed 5 0 1 2 1000000 [[[seed-ptr len]]])  ;; PDA authority
                // =================================================================
                if name == "spl-token-transfer" && args.len() == 5 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000800_i64)); // Use different heap region

                    // Get arguments
                    let token_prog_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-transfer token-prog-idx has no result")
                    })?;
                    let source_idx = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-transfer source-idx has no result")
                    })?;
                    let dest_idx = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-transfer dest-idx has no result")
                    })?;
                    let authority_idx = self.generate_expr(&args[3].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-transfer authority-idx has no result")
                    })?;
                    let amount = self
                        .generate_expr(&args[4].value)?
                        .ok_or_else(|| Error::runtime("spl-token-transfer amount has no result"))?;

                    // Build instruction data at heap: [3, amount (8 bytes)]
                    let three = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(three, 3)); // Transfer discriminator
                    self.emit(IrInstruction::Store(heap_base, three, 0));
                    self.emit(IrInstruction::Store(heap_base, amount, 8)); // Actually at offset 1, but we'll use 8-byte alignment

                    // For proper byte layout: discriminator at offset 0 (1 byte), amount at offset 1 (8 bytes)
                    // We need to store discriminator as a single byte
                    let data_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(data_ptr, 0x300000850_i64)); // Data at offset 0x50 from heap_base
                                                                                   // Store discriminator byte
                    let disc_byte = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(disc_byte, 3));
                    self.emit(IrInstruction::Store(data_ptr, disc_byte, 0)); // Byte 0 = 3
                                                                             // Store amount starting at byte 1 (as little-endian u64)
                                                                             // For simplicity, store at offset 0 as u64 where low byte is discriminator
                                                                             // Actual SPL token expects: [3, amount_le_bytes...]
                                                                             // We'll build it properly:
                    let combined = self.alloc_reg();
                    let shift_multiplier = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(shift_multiplier, 256)); // 256 = 2^8, effectively shifts left by 8 bits
                    self.emit(IrInstruction::Mul(combined, amount, shift_multiplier)); // amount << 8
                    self.emit(IrInstruction::Or(combined, combined, disc_byte)); // combined = (amount << 8) | 3
                    self.emit(IrInstruction::Store(data_ptr, combined, 0));

                    // Get account pubkey pointers using dynamic offset table
                    let token_pk_ptr = self.emit_get_account_pubkey_ptr(token_prog_idx);
                    let src_pk_ptr = self.emit_get_account_pubkey_ptr(source_idx);
                    let dst_pk_ptr = self.emit_get_account_pubkey_ptr(dest_idx);
                    let auth_pk_ptr = self.emit_get_account_pubkey_ptr(authority_idx);

                    // Build SolAccountMeta array at heap_base (3 accounts)
                    // Account 0: Source (writable, not signer)
                    self.emit(IrInstruction::Store(heap_base, src_pk_ptr, 0)); // pubkey ptr
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1)); // writable=1, signer=0
                    self.emit(IrInstruction::Store(heap_base, one, 8)); // flags

                    // Account 1: Dest (writable, not signer)
                    self.emit(IrInstruction::Store(heap_base, dst_pk_ptr, 16)); // pubkey ptr at +16
                    self.emit(IrInstruction::Store(heap_base, one, 24)); // writable=1, signer=0

                    // Account 2: Authority (not writable, signer)
                    self.emit(IrInstruction::Store(heap_base, auth_pk_ptr, 32)); // pubkey ptr at +32
                    let signer_flag = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(signer_flag, 256)); // writable=0, signer=1 (1 << 8)
                    self.emit(IrInstruction::Store(heap_base, signer_flag, 40)); // flags

                    // Build SolInstruction at heap_base + 256
                    let instr_ptr = self.alloc_reg();
                    let const_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_256, 256));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_256));

                    self.emit(IrInstruction::Store(instr_ptr, token_pk_ptr, 0)); // program_id
                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 8)); // accounts ptr
                    let three_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(three_reg, 3)); // 3 accounts
                    self.emit(IrInstruction::Store(instr_ptr, three_reg, 16)); // accounts_len
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24)); // data ptr
                    let nine = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(nine, 9)); // Transfer instruction is 9 bytes
                    self.emit(IrInstruction::Store(instr_ptr, nine, 32)); // data_len

                    // Get account_infos from accounts
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for spl-token-transfer")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));

                    // Invoke without signer seeds (authority must be an actual signer)
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, account_infos_ptr, num_accounts, zero, zero],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // SPL-TOKEN-TRANSFER-SIGNED: Token transfer with PDA authority
                // =================================================================
                // (spl-token-transfer-signed token-prog-idx source-idx dest-idx authority-idx amount signers)
                //
                // signers: [[[seed1-ptr seed1-len] [seed2-ptr seed2-len] ...]]
                //
                // Uses same PDA signing pattern as cpi-invoke-signed but specialized for SPL Token Transfer.
                // The authority must be a PDA that can be derived from the provided seeds.
                //
                // Memory layout at heap 0x300000900:
                //   +0:      Instruction data (9 bytes: 1 byte discriminator + 8 bytes amount)
                //   +64:     SolAccountMeta array (3 accounts * 16 bytes each = 48 bytes)
                //   +128:    SolInstruction struct (40 bytes)
                //   +256:    SolSignerSeeds array (16 bytes each)
                //   +384:    SolSignerSeed entries (16 bytes each)
                // =================================================================
                if name == "spl-token-transfer-signed" && args.len() == 6 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000900_i64));

                    // Get arguments
                    let token_prog_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-transfer-signed token-prog-idx has no result")
                    })?;
                    let source_idx = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-transfer-signed source-idx has no result")
                    })?;
                    let dest_idx = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-transfer-signed dest-idx has no result")
                    })?;
                    let authority_idx = self.generate_expr(&args[3].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-transfer-signed authority-idx has no result")
                    })?;
                    let amount = self.generate_expr(&args[4].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-transfer-signed amount has no result")
                    })?;

                    // Build instruction data: [3, amount_le_bytes (8)]
                    // SPL Token Transfer discriminator = 3
                    // Data is stored at heap_base (0x300000900)
                    let disc_byte = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(disc_byte, 3));
                    let combined = self.alloc_reg();
                    let shift_multiplier = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(shift_multiplier, 256)); // 256 = 2^8
                    self.emit(IrInstruction::Mul(combined, amount, shift_multiplier)); // amount << 8
                    self.emit(IrInstruction::Or(combined, combined, disc_byte)); // combined = (amount << 8) | 3
                    self.emit(IrInstruction::Store(heap_base, combined, 0)); // Store at heap_base

                    // Get account pubkey pointers using dynamic offset table
                    let token_pk_ptr = self.emit_get_account_pubkey_ptr(token_prog_idx);
                    let src_pk_ptr = self.emit_get_account_pubkey_ptr(source_idx);
                    let dst_pk_ptr = self.emit_get_account_pubkey_ptr(dest_idx);
                    let auth_pk_ptr = self.emit_get_account_pubkey_ptr(authority_idx);

                    // SolAccountMeta array at heap_base + 64
                    let accounts_base = self.alloc_reg();
                    let const_64 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_64, 64));
                    self.emit(IrInstruction::Add(accounts_base, heap_base, const_64));

                    // Account 0: Source (writable, not signer)
                    self.emit(IrInstruction::Store(accounts_base, src_pk_ptr, 0));
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1)); // writable=1, signer=0
                    self.emit(IrInstruction::Store(accounts_base, one, 8));

                    // Account 1: Dest (writable, not signer)
                    self.emit(IrInstruction::Store(accounts_base, dst_pk_ptr, 16));
                    self.emit(IrInstruction::Store(accounts_base, one, 24));

                    // Account 2: Authority (not writable, signer via PDA)
                    self.emit(IrInstruction::Store(accounts_base, auth_pk_ptr, 32));
                    let signer_flag = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(signer_flag, 256)); // writable=0, signer=1 (1 << 8)
                    self.emit(IrInstruction::Store(accounts_base, signer_flag, 40));

                    // Build SolInstruction at heap_base + 128
                    let instr_ptr = self.alloc_reg();
                    let const_128 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_128, 128));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_128));

                    self.emit(IrInstruction::Store(instr_ptr, token_pk_ptr, 0)); // program_id
                    self.emit(IrInstruction::Store(instr_ptr, accounts_base, 8)); // accounts ptr
                    let three_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(three_reg, 3)); // 3 accounts
                    self.emit(IrInstruction::Store(instr_ptr, three_reg, 16)); // accounts_len
                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 24)); // data ptr (at heap_base)
                    let nine = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(nine, 9)); // Transfer instruction is 9 bytes
                    self.emit(IrInstruction::Store(instr_ptr, nine, 32)); // data_len

                    // =================================================================
                    // Process PDA signer seeds (arg 5)
                    // =================================================================
                    let mut num_signers = 0usize;
                    let signer_seeds_base = self.alloc_reg();
                    let const_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_256, 256));
                    self.emit(IrInstruction::Add(signer_seeds_base, heap_base, const_256));

                    let seed_entries_base = self.alloc_reg();
                    let const_384 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_384, 384));
                    self.emit(IrInstruction::Add(seed_entries_base, heap_base, const_384));

                    let mut total_seed_entries = 0usize;

                    if let Expression::ArrayLiteral(signers) = &args[5].value {
                        num_signers = signers.len();

                        for (signer_idx, signer_seeds) in signers.iter().enumerate() {
                            if let Expression::ArrayLiteral(seeds) = signer_seeds {
                                let num_seeds_this_signer = seeds.len();

                                // Calculate pointer to first seed entry for this signer
                                let seeds_entry_ptr = self.alloc_reg();
                                let seed_entry_offset = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(
                                    seed_entry_offset,
                                    (total_seed_entries * 16) as i64,
                                ));
                                self.emit(IrInstruction::Add(
                                    seeds_entry_ptr,
                                    seed_entries_base,
                                    seed_entry_offset,
                                ));

                                // Write SolSignerSeeds at signer_seeds_base + signer_idx*16
                                let signer_offset = (signer_idx * 16) as i64;
                                self.emit(IrInstruction::Store(
                                    signer_seeds_base,
                                    seeds_entry_ptr,
                                    signer_offset,
                                ));
                                let num_seeds_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(
                                    num_seeds_reg,
                                    num_seeds_this_signer as i64,
                                ));
                                self.emit(IrInstruction::Store(
                                    signer_seeds_base,
                                    num_seeds_reg,
                                    signer_offset + 8,
                                ));

                                // Write each SolSignerSeed entry
                                for (seed_idx, seed_pair) in seeds.iter().enumerate() {
                                    if let Expression::ArrayLiteral(pair) = seed_pair {
                                        if pair.len() >= 2 {
                                            let seed_addr =
                                                self.generate_expr(&pair[0])?.ok_or_else(|| {
                                                    Error::runtime("seed addr has no result")
                                                })?;
                                            let seed_len =
                                                self.generate_expr(&pair[1])?.ok_or_else(|| {
                                                    Error::runtime("seed len has no result")
                                                })?;

                                            let entry_offset =
                                                ((total_seed_entries + seed_idx) * 16) as i64;
                                            self.emit(IrInstruction::Store(
                                                seed_entries_base,
                                                seed_addr,
                                                entry_offset,
                                            ));
                                            self.emit(IrInstruction::Store(
                                                seed_entries_base,
                                                seed_len,
                                                entry_offset + 8,
                                            ));
                                        }
                                    }
                                }

                                total_seed_entries += num_seeds_this_signer;
                            }
                        }
                    }

                    // Get account_infos from accounts
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for spl-token-transfer-signed")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));

                    // Call sol_invoke_signed_c
                    let num_signers_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_signers_reg, num_signers as i64));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![
                            instr_ptr,
                            account_infos_ptr,
                            num_accounts,
                            signer_seeds_base,
                            num_signers_reg,
                        ],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // SPL-TOKEN-MINT-TO: Mint new tokens to an account
                // =================================================================
                // (spl-token-mint-to token-prog-idx mint-idx dest-idx authority-idx amount)
                //
                // Builds and executes SPL Token MintTo instruction via CPI.
                // Instruction data: [7, amount (8 bytes)] = 9 bytes (discriminator 7 = MintTo)
                //
                // Accounts (in order):
                //   - Mint (writable) - the token mint
                //   - Destination token account (writable)
                //   - Mint authority (signer)
                // =================================================================
                if name == "spl-token-mint-to" && args.len() == 5 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000A00_i64));

                    let token_prog_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-mint-to token-prog-idx has no result")
                    })?;
                    let mint_idx = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-mint-to mint-idx has no result")
                    })?;
                    let dest_idx = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-mint-to dest-idx has no result")
                    })?;
                    let authority_idx = self.generate_expr(&args[3].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-mint-to authority-idx has no result")
                    })?;
                    let amount = self
                        .generate_expr(&args[4].value)?
                        .ok_or_else(|| Error::runtime("spl-token-mint-to amount has no result"))?;

                    // Build instruction data: discriminator 7 (MintTo) + amount
                    let data_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(data_ptr, 0x300000A50_i64));
                    let disc = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(disc, 7)); // MintTo discriminator
                    let shift = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(shift, 256));
                    let combined = self.alloc_reg();
                    self.emit(IrInstruction::Mul(combined, amount, shift));
                    self.emit(IrInstruction::Or(combined, combined, disc));
                    self.emit(IrInstruction::Store(data_ptr, combined, 0));

                    // Get account pubkey pointers using dynamic offset table
                    let token_pk_ptr = self.emit_get_account_pubkey_ptr(token_prog_idx);
                    let mint_pk_ptr = self.emit_get_account_pubkey_ptr(mint_idx);
                    let dst_pk_ptr = self.emit_get_account_pubkey_ptr(dest_idx);
                    let auth_pk_ptr = self.emit_get_account_pubkey_ptr(authority_idx);

                    // Build SolAccountMeta array (3 accounts)
                    // Account 0: Mint (writable)
                    self.emit(IrInstruction::Store(heap_base, mint_pk_ptr, 0));
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Store(heap_base, one, 8));

                    // Account 1: Dest (writable)
                    self.emit(IrInstruction::Store(heap_base, dst_pk_ptr, 16));
                    self.emit(IrInstruction::Store(heap_base, one, 24));

                    // Account 2: Authority (signer)
                    self.emit(IrInstruction::Store(heap_base, auth_pk_ptr, 32));
                    let signer_flag = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(signer_flag, 256));
                    self.emit(IrInstruction::Store(heap_base, signer_flag, 40));

                    // Build SolInstruction
                    let instr_ptr = self.alloc_reg();
                    let const_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_256, 256));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_256));

                    self.emit(IrInstruction::Store(instr_ptr, token_pk_ptr, 0));
                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 8));
                    let three = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(three, 3));
                    self.emit(IrInstruction::Store(instr_ptr, three, 16));
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24));
                    let nine = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(nine, 9));
                    self.emit(IrInstruction::Store(instr_ptr, nine, 32));

                    // Invoke - get account_infos from accounts
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for spl-token-mint-to")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, account_infos_ptr, num_accounts, zero, zero],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // SPL-TOKEN-BURN: Burn tokens from an account
                // =================================================================
                // (spl-token-burn token-prog-idx source-idx mint-idx authority-idx amount)
                //
                // Instruction data: [8, amount (8 bytes)] = 9 bytes (discriminator 8 = Burn)
                //
                // Accounts:
                //   - Source token account (writable)
                //   - Mint (writable) - to decrease supply
                //   - Authority (signer) - owner of source account
                // =================================================================
                if name == "spl-token-burn" && args.len() == 5 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000B00_i64));

                    let token_prog_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-burn token-prog-idx has no result")
                    })?;
                    let source_idx = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("spl-token-burn source-idx has no result"))?;
                    let mint_idx = self
                        .generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("spl-token-burn mint-idx has no result"))?;
                    let authority_idx = self.generate_expr(&args[3].value)?.ok_or_else(|| {
                        Error::runtime("spl-token-burn authority-idx has no result")
                    })?;
                    let amount = self
                        .generate_expr(&args[4].value)?
                        .ok_or_else(|| Error::runtime("spl-token-burn amount has no result"))?;

                    // Build instruction data: discriminator 8 (Burn) + amount
                    let data_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(data_ptr, 0x300000B50_i64));
                    let disc = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(disc, 8)); // Burn discriminator
                    let shift = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(shift, 256));
                    let combined = self.alloc_reg();
                    self.emit(IrInstruction::Mul(combined, amount, shift));
                    self.emit(IrInstruction::Or(combined, combined, disc));
                    self.emit(IrInstruction::Store(data_ptr, combined, 0));

                    // Get account pubkey pointers using dynamic offset table
                    let token_pk_ptr = self.emit_get_account_pubkey_ptr(token_prog_idx);
                    let src_pk_ptr = self.emit_get_account_pubkey_ptr(source_idx);
                    let mint_pk_ptr = self.emit_get_account_pubkey_ptr(mint_idx);
                    let auth_pk_ptr = self.emit_get_account_pubkey_ptr(authority_idx);

                    // Account 0: Source (writable)
                    self.emit(IrInstruction::Store(heap_base, src_pk_ptr, 0));
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Store(heap_base, one, 8));

                    // Account 1: Mint (writable)
                    self.emit(IrInstruction::Store(heap_base, mint_pk_ptr, 16));
                    self.emit(IrInstruction::Store(heap_base, one, 24));

                    // Account 2: Authority (signer)
                    self.emit(IrInstruction::Store(heap_base, auth_pk_ptr, 32));
                    let signer_flag = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(signer_flag, 256));
                    self.emit(IrInstruction::Store(heap_base, signer_flag, 40));

                    let instr_ptr = self.alloc_reg();
                    let const_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_256, 256));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_256));

                    self.emit(IrInstruction::Store(instr_ptr, token_pk_ptr, 0));
                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 8));
                    let three = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(three, 3));
                    self.emit(IrInstruction::Store(instr_ptr, three, 16));
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24));
                    let nine = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(nine, 9));
                    self.emit(IrInstruction::Store(instr_ptr, nine, 32));

                    // Get accounts_ptr from var_map
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for spl-token-burn")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, account_infos_ptr, num_accounts, zero, zero],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // SYSTEM-CREATE-ACCOUNT: Create a new account via System Program CPI
                // =================================================================
                // (system-create-account payer-idx new-acct-idx lamports space owner-pubkey-ptr)
                //
                // Instruction data (52 bytes):
                //   [0, lamports (8), space (8), owner (32)]
                //   Discriminator 0 = CreateAccount
                //
                // Accounts:
                //   - Payer (writable, signer) - pays for account creation
                //   - New account (writable, signer) - the account being created
                // =================================================================
                if name == "system-create-account" && args.len() == 5 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000C00_i64));

                    let payer_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("system-create-account payer-idx has no result")
                    })?;
                    let new_acct_idx = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("system-create-account new-acct-idx has no result")
                    })?;
                    let lamports = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                        Error::runtime("system-create-account lamports has no result")
                    })?;
                    let space = self.generate_expr(&args[3].value)?.ok_or_else(|| {
                        Error::runtime("system-create-account space has no result")
                    })?;
                    let owner_ptr = self.generate_expr(&args[4].value)?.ok_or_else(|| {
                        Error::runtime("system-create-account owner-pubkey-ptr has no result")
                    })?;

                    // Build System Program ID (all zeros) at heap_base
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    self.emit(IrInstruction::Store(heap_base, zero, 0));
                    self.emit(IrInstruction::Store(heap_base, zero, 8));
                    self.emit(IrInstruction::Store(heap_base, zero, 16));
                    self.emit(IrInstruction::Store(heap_base, zero, 24));

                    // Build instruction data at heap_base + 32
                    // [discriminant (4 bytes), lamports (8), space (8), owner (32)] = 52 bytes
                    let data_ptr = self.alloc_reg();
                    let const_32 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_32, 32));
                    self.emit(IrInstruction::Add(data_ptr, heap_base, const_32));

                    // Store discriminant 0 (CreateAccount)
                    self.emit(IrInstruction::Store(data_ptr, zero, 0));
                    // Store lamports at offset 4 (but we align to 8)
                    self.emit(IrInstruction::Store(data_ptr, lamports, 8));
                    // Store space at offset 16
                    self.emit(IrInstruction::Store(data_ptr, space, 16));

                    // Copy owner pubkey (32 bytes) at offset 24
                    // Load and store 4 u64s
                    let owner_chunk0 = self.alloc_reg();
                    let owner_chunk1 = self.alloc_reg();
                    let owner_chunk2 = self.alloc_reg();
                    let owner_chunk3 = self.alloc_reg();
                    self.emit(IrInstruction::Load(owner_chunk0, owner_ptr, 0));
                    self.emit(IrInstruction::Load(owner_chunk1, owner_ptr, 8));
                    self.emit(IrInstruction::Load(owner_chunk2, owner_ptr, 16));
                    self.emit(IrInstruction::Load(owner_chunk3, owner_ptr, 24));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk0, 24));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk1, 32));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk2, 40));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk3, 48));

                    // Build SolAccountMeta array at heap_base + 128
                    let meta_base = self.alloc_reg();
                    let const_128 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_128, 128));
                    self.emit(IrInstruction::Add(meta_base, heap_base, const_128));

                    // Account 0: Payer (writable, signer) - use dynamic offset
                    let payer_pk_ptr = self.emit_get_account_pubkey_ptr(payer_idx);
                    self.emit(IrInstruction::Store(meta_base, payer_pk_ptr, 0));
                    let writable_signer = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(writable_signer, 0x0101)); // writable + signer
                    self.emit(IrInstruction::Store(meta_base, writable_signer, 8));

                    // Account 1: New account (writable, signer) - use dynamic offset
                    let new_pk_ptr = self.emit_get_account_pubkey_ptr(new_acct_idx);
                    self.emit(IrInstruction::Store(meta_base, new_pk_ptr, 16));
                    self.emit(IrInstruction::Store(meta_base, writable_signer, 24));

                    // Build SolInstruction at heap_base + 256
                    let instr_ptr = self.alloc_reg();
                    let const_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_256, 256));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_256));

                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 0)); // System Program ID
                    self.emit(IrInstruction::Store(instr_ptr, meta_base, 8)); // accounts ptr
                    let two = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(two, 2));
                    self.emit(IrInstruction::Store(instr_ptr, two, 16)); // accounts_len
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24)); // data
                    let data_len = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(data_len, 52));
                    self.emit(IrInstruction::Store(instr_ptr, data_len, 32)); // data_len

                    // Get account_infos from accounts (dynamic)
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for system-create-account")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, account_infos_ptr, num_accounts, zero, zero],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // ROUND 4 MACROS: SPL Close Account, System Allocate, Anchor Error
                // =================================================================

                // =================================================================
                // SPL-CLOSE-ACCOUNT: Close a token account and reclaim lamports
                // =================================================================
                // (spl-close-account token-prog-idx account-idx destination-idx authority-idx)
                //
                // Instruction data: [9] = 1 byte (discriminator 9 = CloseAccount)
                //
                // Accounts:
                //   - Account to close (writable)
                //   - Destination for lamports (writable)
                //   - Authority (signer)
                // =================================================================
                if name == "spl-close-account" && args.len() == 4 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000E00_i64));

                    let token_prog_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("spl-close-account token-prog-idx has no result")
                    })?;
                    let account_idx = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("spl-close-account account-idx has no result")
                    })?;
                    let destination_idx = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                        Error::runtime("spl-close-account destination-idx has no result")
                    })?;
                    let authority_idx = self.generate_expr(&args[3].value)?.ok_or_else(|| {
                        Error::runtime("spl-close-account authority-idx has no result")
                    })?;

                    // Build instruction data: discriminator 9 (CloseAccount)
                    let data_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(data_ptr, 0x300000E80_i64));
                    let nine = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(nine, 9));
                    self.emit(IrInstruction::Store1(data_ptr, nine, 0));

                    // Get token program pubkey pointer using dynamic offset
                    let token_pk_ptr = self.emit_get_account_pubkey_ptr(token_prog_idx);

                    // Account 0: Account to close (writable) - dynamic offset
                    let acct_pk_ptr = self.emit_get_account_pubkey_ptr(account_idx);
                    self.emit(IrInstruction::Store(heap_base, acct_pk_ptr, 0));
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Store(heap_base, one, 8)); // writable

                    // Account 1: Destination (writable) - dynamic offset
                    let dest_pk_ptr = self.emit_get_account_pubkey_ptr(destination_idx);
                    self.emit(IrInstruction::Store(heap_base, dest_pk_ptr, 16));
                    self.emit(IrInstruction::Store(heap_base, one, 24)); // writable

                    // Account 2: Authority (signer) - dynamic offset
                    let auth_pk_ptr = self.emit_get_account_pubkey_ptr(authority_idx);
                    self.emit(IrInstruction::Store(heap_base, auth_pk_ptr, 32));
                    let signer_flag = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(signer_flag, 256)); // signer
                    self.emit(IrInstruction::Store(heap_base, signer_flag, 40));

                    // Build instruction
                    let instr_ptr = self.alloc_reg();
                    let const_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_256, 256));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_256));

                    self.emit(IrInstruction::Store(instr_ptr, token_pk_ptr, 0));
                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 8));
                    let three = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(three, 3));
                    self.emit(IrInstruction::Store(instr_ptr, three, 16));
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24));
                    self.emit(IrInstruction::Store(instr_ptr, one, 32)); // data_len = 1

                    // Get account_infos from accounts (dynamic)
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for spl-close-account")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, account_infos_ptr, num_accounts, zero, zero],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // SPL-CLOSE-ACCOUNT-SIGNED: Close token account with PDA authority
                // =================================================================
                // (spl-close-account-signed token-prog-idx account-idx destination-idx authority-idx signers)
                //
                // Same as spl-close-account but with PDA signing.
                // signers: [[[seed1-ptr seed1-len] [seed2-ptr seed2-len] ...]]
                //
                // Memory layout at heap 0x300000E00:
                //   +0:     SolAccountMeta array (3 accounts * 16 bytes each = 48 bytes)
                //   +128:   SolInstruction struct (40 bytes)
                //   +256:   SolSignerSeeds array (16 bytes each)
                //   +384:   SolSignerSeed entries (16 bytes each)
                //   +512:   Instruction data (1 byte discriminator)
                // =================================================================
                if name == "spl-close-account-signed" && args.len() == 5 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000E00_i64));

                    let token_prog_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("spl-close-account-signed token-prog-idx has no result")
                    })?;
                    let account_idx = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("spl-close-account-signed account-idx has no result")
                    })?;
                    let destination_idx = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                        Error::runtime("spl-close-account-signed destination-idx has no result")
                    })?;
                    let authority_idx = self.generate_expr(&args[3].value)?.ok_or_else(|| {
                        Error::runtime("spl-close-account-signed authority-idx has no result")
                    })?;

                    // Build instruction data: discriminator 9 (CloseAccount)
                    let data_ptr = self.alloc_reg();
                    let const_512 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_512, 512));
                    self.emit(IrInstruction::Add(data_ptr, heap_base, const_512));
                    let nine = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(nine, 9));
                    self.emit(IrInstruction::Store1(data_ptr, nine, 0));

                    // Get token program pubkey pointer using dynamic offset
                    let token_pk_ptr = self.emit_get_account_pubkey_ptr(token_prog_idx);

                    // Account 0: Account to close (writable) - dynamic offset
                    let acct_pk_ptr = self.emit_get_account_pubkey_ptr(account_idx);
                    self.emit(IrInstruction::Store(heap_base, acct_pk_ptr, 0));
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Store(heap_base, one, 8)); // writable

                    // Account 1: Destination (writable) - dynamic offset
                    let dest_pk_ptr = self.emit_get_account_pubkey_ptr(destination_idx);
                    self.emit(IrInstruction::Store(heap_base, dest_pk_ptr, 16));
                    self.emit(IrInstruction::Store(heap_base, one, 24)); // writable

                    // Account 2: Authority (signer via PDA) - dynamic offset
                    let auth_pk_ptr = self.emit_get_account_pubkey_ptr(authority_idx);
                    self.emit(IrInstruction::Store(heap_base, auth_pk_ptr, 32));
                    let signer_flag = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(signer_flag, 256)); // signer
                    self.emit(IrInstruction::Store(heap_base, signer_flag, 40));

                    // Build instruction at heap_base + 128
                    let instr_ptr = self.alloc_reg();
                    let const_128 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_128, 128));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_128));

                    self.emit(IrInstruction::Store(instr_ptr, token_pk_ptr, 0));
                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 8));
                    let three = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(three, 3));
                    self.emit(IrInstruction::Store(instr_ptr, three, 16));
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24));
                    self.emit(IrInstruction::Store(instr_ptr, one, 32)); // data_len = 1

                    // =================================================================
                    // Process PDA signer seeds (arg 4)
                    // =================================================================
                    let mut num_signers = 0usize;
                    let signer_seeds_base = self.alloc_reg();
                    let const_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_256, 256));
                    self.emit(IrInstruction::Add(signer_seeds_base, heap_base, const_256));

                    let seed_entries_base = self.alloc_reg();
                    let const_384 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_384, 384));
                    self.emit(IrInstruction::Add(seed_entries_base, heap_base, const_384));

                    let mut total_seed_entries = 0usize;

                    if let Expression::ArrayLiteral(signers) = &args[4].value {
                        num_signers = signers.len();

                        for (signer_idx, signer_seeds) in signers.iter().enumerate() {
                            if let Expression::ArrayLiteral(seeds) = signer_seeds {
                                let num_seeds_this_signer = seeds.len();

                                let seeds_entry_ptr = self.alloc_reg();
                                let seed_entry_offset = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(
                                    seed_entry_offset,
                                    (total_seed_entries * 16) as i64,
                                ));
                                self.emit(IrInstruction::Add(
                                    seeds_entry_ptr,
                                    seed_entries_base,
                                    seed_entry_offset,
                                ));

                                let signer_offset = (signer_idx * 16) as i64;
                                self.emit(IrInstruction::Store(
                                    signer_seeds_base,
                                    seeds_entry_ptr,
                                    signer_offset,
                                ));
                                let num_seeds_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(
                                    num_seeds_reg,
                                    num_seeds_this_signer as i64,
                                ));
                                self.emit(IrInstruction::Store(
                                    signer_seeds_base,
                                    num_seeds_reg,
                                    signer_offset + 8,
                                ));

                                for (seed_idx, seed_pair) in seeds.iter().enumerate() {
                                    if let Expression::ArrayLiteral(pair) = seed_pair {
                                        if pair.len() >= 2 {
                                            let seed_addr =
                                                self.generate_expr(&pair[0])?.ok_or_else(|| {
                                                    Error::runtime("seed addr has no result")
                                                })?;
                                            let seed_len =
                                                self.generate_expr(&pair[1])?.ok_or_else(|| {
                                                    Error::runtime("seed len has no result")
                                                })?;

                                            let entry_offset =
                                                ((total_seed_entries + seed_idx) * 16) as i64;
                                            self.emit(IrInstruction::Store(
                                                seed_entries_base,
                                                seed_addr,
                                                entry_offset,
                                            ));
                                            self.emit(IrInstruction::Store(
                                                seed_entries_base,
                                                seed_len,
                                                entry_offset + 8,
                                            ));
                                        }
                                    }
                                }

                                total_seed_entries += num_seeds_this_signer;
                            }
                        }
                    }

                    // Get account_infos from accounts (dynamic)
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for spl-close-account-signed")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));

                    let num_signers_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_signers_reg, num_signers as i64));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![
                            instr_ptr,
                            account_infos_ptr,
                            num_accounts,
                            signer_seeds_base,
                            num_signers_reg,
                        ],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // SYSTEM-ALLOCATE: Allocate space in an account via System Program
                // =================================================================
                // (system-allocate account-idx space)
                //
                // Instruction data: [8, space (8 bytes)] = 9 bytes (discriminator 8 = Allocate)
                //
                // Accounts:
                //   - Account to allocate (writable, signer)
                // =================================================================
                if name == "system-allocate" && args.len() == 2 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000F00_i64));

                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("system-allocate account-idx has no result")
                    })?;
                    let space = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("system-allocate space has no result"))?;

                    // Build System Program ID (all zeros)
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    self.emit(IrInstruction::Store(heap_base, zero, 0));
                    self.emit(IrInstruction::Store(heap_base, zero, 8));
                    self.emit(IrInstruction::Store(heap_base, zero, 16));
                    self.emit(IrInstruction::Store(heap_base, zero, 24));

                    // Build instruction data: [discriminator (4 bytes), space (8 bytes)]
                    let data_ptr = self.alloc_reg();
                    let const_32 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_32, 32));
                    self.emit(IrInstruction::Add(data_ptr, heap_base, const_32));

                    let eight_disc = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_disc, 8)); // Allocate = 8
                    self.emit(IrInstruction::Store4(data_ptr, eight_disc, 0));
                    self.emit(IrInstruction::Store(data_ptr, space, 8));

                    // Build SolAccountMeta at heap_base + 64
                    let meta_base = self.alloc_reg();
                    let const_64 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_64, 64));
                    self.emit(IrInstruction::Add(meta_base, heap_base, const_64));

                    // Account 0: Account to allocate (writable, signer) - dynamic offset
                    let acct_pk_ptr = self.emit_get_account_pubkey_ptr(account_idx);
                    self.emit(IrInstruction::Store(meta_base, acct_pk_ptr, 0));
                    let writable_signer = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(writable_signer, 257)); // writable + signer
                    self.emit(IrInstruction::Store(meta_base, writable_signer, 8));

                    // Build instruction
                    let instr_ptr = self.alloc_reg();
                    let const_128 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_128, 128));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_128));

                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 0)); // program_id
                    self.emit(IrInstruction::Store(instr_ptr, meta_base, 8)); // accounts
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Store(instr_ptr, one, 16)); // accounts_len
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24)); // data
                    let twelve = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(twelve, 12));
                    self.emit(IrInstruction::Store(instr_ptr, twelve, 32)); // data_len

                    // Get account_infos from accounts (dynamic)
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for system-allocate")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, account_infos_ptr, num_accounts, zero, zero],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // SYSTEM-ALLOCATE-SIGNED: Allocate space with PDA as signer
                // =================================================================
                // (system-allocate-signed account-idx space signers)
                //
                // Same as system-allocate but with PDA signing.
                // signers: [[[seed1-ptr seed1-len] [seed2-ptr seed2-len] ...]]
                //
                // Memory layout at heap 0x300000F00:
                //   +0:     System Program ID (32 bytes, all zeros)
                //   +32:    Instruction data (12 bytes: 4-byte discriminator + 8-byte space)
                //   +64:    SolAccountMeta array (1 account * 16 bytes = 16 bytes)
                //   +128:   SolInstruction struct (40 bytes)
                //   +256:   SolSignerSeeds array (16 bytes each)
                //   +384:   SolSignerSeed entries (16 bytes each)
                // =================================================================
                if name == "system-allocate-signed" && args.len() == 3 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000F00_i64));

                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("system-allocate-signed account-idx has no result")
                    })?;
                    let space = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("system-allocate-signed space has no result")
                    })?;

                    // Build System Program ID (all zeros)
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    self.emit(IrInstruction::Store(heap_base, zero, 0));
                    self.emit(IrInstruction::Store(heap_base, zero, 8));
                    self.emit(IrInstruction::Store(heap_base, zero, 16));
                    self.emit(IrInstruction::Store(heap_base, zero, 24));

                    // Build instruction data: [discriminator (4 bytes), space (8 bytes)]
                    let data_ptr = self.alloc_reg();
                    let const_32 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_32, 32));
                    self.emit(IrInstruction::Add(data_ptr, heap_base, const_32));

                    let eight_disc = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_disc, 8)); // Allocate = 8
                    self.emit(IrInstruction::Store4(data_ptr, eight_disc, 0));
                    self.emit(IrInstruction::Store(data_ptr, space, 8));

                    // Build SolAccountMeta at heap_base + 64
                    let meta_base = self.alloc_reg();
                    let const_64 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_64, 64));
                    self.emit(IrInstruction::Add(meta_base, heap_base, const_64));

                    // Account 0: Account to allocate (writable, signer via PDA) - dynamic offset
                    let acct_pk_ptr = self.emit_get_account_pubkey_ptr(account_idx);
                    self.emit(IrInstruction::Store(meta_base, acct_pk_ptr, 0));
                    let writable_signer = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(writable_signer, 257)); // writable + signer
                    self.emit(IrInstruction::Store(meta_base, writable_signer, 8));

                    // Build instruction at heap_base + 128
                    let instr_ptr = self.alloc_reg();
                    let const_128 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_128, 128));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_128));

                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 0)); // program_id
                    self.emit(IrInstruction::Store(instr_ptr, meta_base, 8)); // accounts
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Store(instr_ptr, one, 16)); // accounts_len
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24)); // data
                    let twelve = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(twelve, 12));
                    self.emit(IrInstruction::Store(instr_ptr, twelve, 32)); // data_len

                    // =================================================================
                    // Process PDA signer seeds (arg 2)
                    // =================================================================
                    let mut num_signers = 0usize;
                    let signer_seeds_base = self.alloc_reg();
                    let const_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_256, 256));
                    self.emit(IrInstruction::Add(signer_seeds_base, heap_base, const_256));

                    let seed_entries_base = self.alloc_reg();
                    let const_384 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_384, 384));
                    self.emit(IrInstruction::Add(seed_entries_base, heap_base, const_384));

                    let mut total_seed_entries = 0usize;

                    if let Expression::ArrayLiteral(signers) = &args[2].value {
                        num_signers = signers.len();

                        for (signer_idx, signer_seeds) in signers.iter().enumerate() {
                            if let Expression::ArrayLiteral(seeds) = signer_seeds {
                                let num_seeds_this_signer = seeds.len();

                                let seeds_entry_ptr = self.alloc_reg();
                                let seed_entry_offset = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(
                                    seed_entry_offset,
                                    (total_seed_entries * 16) as i64,
                                ));
                                self.emit(IrInstruction::Add(
                                    seeds_entry_ptr,
                                    seed_entries_base,
                                    seed_entry_offset,
                                ));

                                let signer_offset = (signer_idx * 16) as i64;
                                self.emit(IrInstruction::Store(
                                    signer_seeds_base,
                                    seeds_entry_ptr,
                                    signer_offset,
                                ));
                                let num_seeds_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(
                                    num_seeds_reg,
                                    num_seeds_this_signer as i64,
                                ));
                                self.emit(IrInstruction::Store(
                                    signer_seeds_base,
                                    num_seeds_reg,
                                    signer_offset + 8,
                                ));

                                for (seed_idx, seed_pair) in seeds.iter().enumerate() {
                                    if let Expression::ArrayLiteral(pair) = seed_pair {
                                        if pair.len() >= 2 {
                                            let seed_addr =
                                                self.generate_expr(&pair[0])?.ok_or_else(|| {
                                                    Error::runtime("seed addr has no result")
                                                })?;
                                            let seed_len =
                                                self.generate_expr(&pair[1])?.ok_or_else(|| {
                                                    Error::runtime("seed len has no result")
                                                })?;

                                            let entry_offset =
                                                ((total_seed_entries + seed_idx) * 16) as i64;
                                            self.emit(IrInstruction::Store(
                                                seed_entries_base,
                                                seed_addr,
                                                entry_offset,
                                            ));
                                            self.emit(IrInstruction::Store(
                                                seed_entries_base,
                                                seed_len,
                                                entry_offset + 8,
                                            ));
                                        }
                                    }
                                }

                                total_seed_entries += num_seeds_this_signer;
                            }
                        }
                    }

                    // Get account_infos from accounts (dynamic)
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for system-allocate-signed")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));

                    let num_signers_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_signers_reg, num_signers as i64));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![
                            instr_ptr,
                            account_infos_ptr,
                            num_accounts,
                            signer_seeds_base,
                            num_signers_reg,
                        ],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // SYSTEM-ASSIGN: Assign account to a program
                // =================================================================
                // (system-assign account-idx owner-pubkey-ptr)
                //
                // Instruction data: [1, owner (32 bytes)] = 33 bytes (discriminator 1 = Assign)
                //
                // Accounts:
                //   - Account to assign (writable, signer)
                // =================================================================
                if name == "system-assign" && args.len() == 2 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300001000_i64));

                    let account_idx = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("system-assign account-idx has no result"))?;
                    let owner_ptr = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("system-assign owner-pubkey-ptr has no result")
                    })?;

                    // Build System Program ID (all zeros)
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    self.emit(IrInstruction::Store(heap_base, zero, 0));
                    self.emit(IrInstruction::Store(heap_base, zero, 8));
                    self.emit(IrInstruction::Store(heap_base, zero, 16));
                    self.emit(IrInstruction::Store(heap_base, zero, 24));

                    // Build instruction data at heap_base + 32
                    let data_ptr = self.alloc_reg();
                    let const_32 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_32, 32));
                    self.emit(IrInstruction::Add(data_ptr, heap_base, const_32));

                    // Discriminator 1 = Assign
                    let one_disc = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one_disc, 1));
                    self.emit(IrInstruction::Store4(data_ptr, one_disc, 0));

                    // Copy owner pubkey (32 bytes)
                    let owner_chunk0 = self.alloc_reg();
                    let owner_chunk1 = self.alloc_reg();
                    let owner_chunk2 = self.alloc_reg();
                    let owner_chunk3 = self.alloc_reg();
                    self.emit(IrInstruction::Load(owner_chunk0, owner_ptr, 0));
                    self.emit(IrInstruction::Load(owner_chunk1, owner_ptr, 8));
                    self.emit(IrInstruction::Load(owner_chunk2, owner_ptr, 16));
                    self.emit(IrInstruction::Load(owner_chunk3, owner_ptr, 24));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk0, 4));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk1, 12));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk2, 20));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk3, 28));

                    // Build SolAccountMeta at heap_base + 80
                    let meta_base = self.alloc_reg();
                    let const_80 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_80, 80));
                    self.emit(IrInstruction::Add(meta_base, heap_base, const_80));

                    // Account to assign (writable, signer) - dynamic offset
                    let acct_pk_ptr = self.emit_get_account_pubkey_ptr(account_idx);
                    self.emit(IrInstruction::Store(meta_base, acct_pk_ptr, 0));
                    let writable_signer = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(writable_signer, 257));
                    self.emit(IrInstruction::Store(meta_base, writable_signer, 8));

                    // Build instruction
                    let instr_ptr = self.alloc_reg();
                    let const_128 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_128, 128));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_128));

                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 0)); // program_id
                    self.emit(IrInstruction::Store(instr_ptr, meta_base, 8)); // accounts
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Store(instr_ptr, one, 16)); // accounts_len
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24)); // data
                    let thirty_six = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(thirty_six, 36));
                    self.emit(IrInstruction::Store(instr_ptr, thirty_six, 32)); // data_len

                    // Get account_infos from accounts (dynamic)
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for system-assign")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, account_infos_ptr, num_accounts, zero, zero],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // SYSTEM-ASSIGN-SIGNED: Assign account with PDA as signer
                // =================================================================
                // (system-assign-signed account-idx owner-pubkey-ptr signers)
                //
                // Same as system-assign but with PDA signing.
                // signers: [[[seed1-ptr seed1-len] [seed2-ptr seed2-len] ...]]
                //
                // Memory layout at heap 0x300001000:
                //   +0:     System Program ID (32 bytes, all zeros)
                //   +32:    Instruction data (36 bytes: 4-byte discriminator + 32-byte owner)
                //   +80:    SolAccountMeta array (1 account * 16 bytes = 16 bytes)
                //   +128:   SolInstruction struct (40 bytes)
                //   +256:   SolSignerSeeds array (16 bytes each)
                //   +384:   SolSignerSeed entries (16 bytes each)
                // =================================================================
                if name == "system-assign-signed" && args.len() == 3 {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300001000_i64));

                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("system-assign-signed account-idx has no result")
                    })?;
                    let owner_ptr = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("system-assign-signed owner-pubkey-ptr has no result")
                    })?;

                    // Build System Program ID (all zeros)
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    self.emit(IrInstruction::Store(heap_base, zero, 0));
                    self.emit(IrInstruction::Store(heap_base, zero, 8));
                    self.emit(IrInstruction::Store(heap_base, zero, 16));
                    self.emit(IrInstruction::Store(heap_base, zero, 24));

                    // Build instruction data at heap_base + 32
                    let data_ptr = self.alloc_reg();
                    let const_32 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_32, 32));
                    self.emit(IrInstruction::Add(data_ptr, heap_base, const_32));

                    // Discriminator 1 = Assign
                    let one_disc = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one_disc, 1));
                    self.emit(IrInstruction::Store4(data_ptr, one_disc, 0));

                    // Copy owner pubkey (32 bytes)
                    let owner_chunk0 = self.alloc_reg();
                    let owner_chunk1 = self.alloc_reg();
                    let owner_chunk2 = self.alloc_reg();
                    let owner_chunk3 = self.alloc_reg();
                    self.emit(IrInstruction::Load(owner_chunk0, owner_ptr, 0));
                    self.emit(IrInstruction::Load(owner_chunk1, owner_ptr, 8));
                    self.emit(IrInstruction::Load(owner_chunk2, owner_ptr, 16));
                    self.emit(IrInstruction::Load(owner_chunk3, owner_ptr, 24));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk0, 4));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk1, 12));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk2, 20));
                    self.emit(IrInstruction::Store(data_ptr, owner_chunk3, 28));

                    // Build SolAccountMeta at heap_base + 80
                    let meta_base = self.alloc_reg();
                    let const_80 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_80, 80));
                    self.emit(IrInstruction::Add(meta_base, heap_base, const_80));

                    // Account to assign (writable, signer via PDA) - dynamic offset
                    let acct_pk_ptr = self.emit_get_account_pubkey_ptr(account_idx);
                    self.emit(IrInstruction::Store(meta_base, acct_pk_ptr, 0));
                    let writable_signer = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(writable_signer, 257));
                    self.emit(IrInstruction::Store(meta_base, writable_signer, 8));

                    // Build instruction at heap_base + 128
                    let instr_ptr = self.alloc_reg();
                    let const_128 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_128, 128));
                    self.emit(IrInstruction::Add(instr_ptr, heap_base, const_128));

                    self.emit(IrInstruction::Store(instr_ptr, heap_base, 0)); // program_id
                    self.emit(IrInstruction::Store(instr_ptr, meta_base, 8)); // accounts
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Store(instr_ptr, one, 16)); // accounts_len
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24)); // data
                    let thirty_six = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(thirty_six, 36));
                    self.emit(IrInstruction::Store(instr_ptr, thirty_six, 32)); // data_len

                    // =================================================================
                    // Process PDA signer seeds (arg 2)
                    // =================================================================
                    let mut num_signers = 0usize;
                    let signer_seeds_base = self.alloc_reg();
                    let const_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_256, 256));
                    self.emit(IrInstruction::Add(signer_seeds_base, heap_base, const_256));

                    let seed_entries_base = self.alloc_reg();
                    let const_384 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_384, 384));
                    self.emit(IrInstruction::Add(seed_entries_base, heap_base, const_384));

                    let mut total_seed_entries = 0usize;

                    if let Expression::ArrayLiteral(signers) = &args[2].value {
                        num_signers = signers.len();

                        for (signer_idx, signer_seeds) in signers.iter().enumerate() {
                            if let Expression::ArrayLiteral(seeds) = signer_seeds {
                                let num_seeds_this_signer = seeds.len();

                                let seeds_entry_ptr = self.alloc_reg();
                                let seed_entry_offset = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(
                                    seed_entry_offset,
                                    (total_seed_entries * 16) as i64,
                                ));
                                self.emit(IrInstruction::Add(
                                    seeds_entry_ptr,
                                    seed_entries_base,
                                    seed_entry_offset,
                                ));

                                let signer_offset = (signer_idx * 16) as i64;
                                self.emit(IrInstruction::Store(
                                    signer_seeds_base,
                                    seeds_entry_ptr,
                                    signer_offset,
                                ));
                                let num_seeds_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(
                                    num_seeds_reg,
                                    num_seeds_this_signer as i64,
                                ));
                                self.emit(IrInstruction::Store(
                                    signer_seeds_base,
                                    num_seeds_reg,
                                    signer_offset + 8,
                                ));

                                for (seed_idx, seed_pair) in seeds.iter().enumerate() {
                                    if let Expression::ArrayLiteral(pair) = seed_pair {
                                        if pair.len() >= 2 {
                                            let seed_addr =
                                                self.generate_expr(&pair[0])?.ok_or_else(|| {
                                                    Error::runtime("seed addr has no result")
                                                })?;
                                            let seed_len =
                                                self.generate_expr(&pair[1])?.ok_or_else(|| {
                                                    Error::runtime("seed len has no result")
                                                })?;

                                            let entry_offset =
                                                ((total_seed_entries + seed_idx) * 16) as i64;
                                            self.emit(IrInstruction::Store(
                                                seed_entries_base,
                                                seed_addr,
                                                entry_offset,
                                            ));
                                            self.emit(IrInstruction::Store(
                                                seed_entries_base,
                                                seed_len,
                                                entry_offset + 8,
                                            ));
                                        }
                                    }
                                }

                                total_seed_entries += num_seeds_this_signer;
                            }
                        }
                    }

                    // Get account_infos from accounts (dynamic)
                    let accounts_ptr = *self.var_map.get("accounts").ok_or_else(|| {
                        Error::runtime("accounts not available for system-assign-signed")
                    })?;
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, accounts_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));

                    let num_signers_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_signers_reg, num_signers as i64));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![
                            instr_ptr,
                            account_infos_ptr,
                            num_accounts,
                            signer_seeds_base,
                            num_signers_reg,
                        ],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // ANCHOR-ERROR: Return an Anchor-compatible error code
                // =================================================================
                // (anchor-error error-code)
                //
                // Converts to Anchor error format: 6000 + custom_code
                // Logs the error and returns the value (caller should abort)
                // =================================================================
                if name == "anchor-error" && args.len() == 1 {
                    let error_code = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("anchor-error error-code has no result"))?;

                    // Anchor errors start at 6000
                    let base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(base, 6000));
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Add(result, base, error_code));

                    // Log the error
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    self.emit(IrInstruction::Syscall(
                        None,
                        "sol_log_64_".to_string(),
                        vec![result, zero, zero, zero, zero],
                    ));

                    return Ok(Some(result));
                }

                // =================================================================
                // REQUIRE: Assert condition or return Anchor error
                // =================================================================
                // (require condition error-code)
                //
                // If condition is false, logs error and aborts with error code
                // =================================================================
                if name == "require" && args.len() == 2 {
                    let condition = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("require condition has no result"))?;
                    let error_code = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("require error-code has no result"))?;

                    // If condition is true (non-zero), skip the abort
                    let ok_label = self.new_label("require_ok");

                    self.emit(IrInstruction::JumpIf(condition, ok_label.clone()));

                    // Condition is false - compute anchor error and abort
                    let base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(base, 6000));
                    let anchor_error = self.alloc_reg();
                    self.emit(IrInstruction::Add(anchor_error, base, error_code));

                    // Log the error before aborting
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    self.emit(IrInstruction::Syscall(
                        None,
                        "sol_log_64_".to_string(),
                        vec![anchor_error, zero, zero, zero, zero],
                    ));

                    // Return with error code (abort)
                    self.emit(IrInstruction::Return(Some(anchor_error)));

                    self.emit(IrInstruction::Label(ok_label));

                    // Return success (0)
                    let success = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(success, 0));
                    return Ok(Some(success));
                }

                // =================================================================
                // MSG: Log a formatted message (Anchor-style)
                // =================================================================
                // (msg "format string")
                //
                // Logs a message using sol_log_
                // =================================================================
                if name == "msg" && args.len() == 1 {
                    let msg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("msg argument has no result"))?;

                    self.emit(IrInstruction::Syscall(
                        None,
                        "sol_log_".to_string(),
                        vec![msg],
                    ));

                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    return Ok(Some(zero));
                }

                // =================================================================
                // DERIVE-PDA: Compute Program Derived Address
                // =================================================================
                // (derive-pda program-pubkey-ptr seeds-ptr bump-ptr) -> 0 on success, 1 on failure
                //
                // Calls sol_try_find_program_address syscall
                // The result is stored at the provided destination pointer
                //
                // seeds-ptr: pointer to array of seed slices
                // bump-ptr: pointer to u8 where bump seed will be stored
                //
                // Note: This is a compile-time helper that generates a syscall.
                // For fully static PDAs, use a pre-computed constant instead.
                // =================================================================
                if name == "derive-pda" && args.len() == 3 {
                    let program_pk_ptr = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("derive-pda program-pubkey-ptr has no result")
                    })?;
                    let seeds_ptr = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("derive-pda seeds-ptr has no result"))?;
                    let bump_ptr = self
                        .generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("derive-pda bump-ptr has no result"))?;

                    // sol_try_find_program_address(seeds_ptr, seeds_len, program_id, bump_seed_ptr)
                    // For now, assume single seed - user provides seeds array length separately
                    // Returns 0 on success
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_try_find_program_address".to_string(),
                        vec![seeds_ptr, one, program_pk_ptr, bump_ptr],
                    ));

                    return Ok(Some(dst));
                }

                // =================================================================
                // CREATE-PDA: Higher-level PDA helper
                // =================================================================
                // (create-pda dest-ptr program-pubkey-ptr [[seed-ptr seed-len] ...])
                //
                // Writes the derived PDA to dest-ptr (32 bytes)
                // Returns the bump seed
                // =================================================================
                if name == "create-pda" && args.len() == 3 {
                    let dest_ptr = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("create-pda dest-ptr has no result"))?;
                    let program_pk_ptr = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("create-pda program-pubkey-ptr has no result")
                    })?;

                    // Build seeds array in heap
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000D00_i64));

                    let mut num_seeds = 0usize;
                    if let Expression::ArrayLiteral(seeds) = &args[2].value {
                        num_seeds = seeds.len();

                        for (i, seed) in seeds.iter().enumerate() {
                            if let Expression::ArrayLiteral(pair) = seed {
                                if pair.len() >= 2 {
                                    let seed_addr =
                                        self.generate_expr(&pair[0])?.ok_or_else(|| {
                                            Error::runtime("create-pda seed addr has no result")
                                        })?;
                                    let seed_len =
                                        self.generate_expr(&pair[1])?.ok_or_else(|| {
                                            Error::runtime("create-pda seed len has no result")
                                        })?;

                                    let offset = (i * 16) as i64;
                                    self.emit(IrInstruction::Store(heap_base, seed_addr, offset));
                                    self.emit(IrInstruction::Store(
                                        heap_base,
                                        seed_len,
                                        offset + 8,
                                    ));
                                }
                            }
                        }
                    }

                    // Bump storage at heap_base + 256
                    let bump_ptr = self.alloc_reg();
                    let const_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_256, 256));
                    self.emit(IrInstruction::Add(bump_ptr, heap_base, const_256));

                    let num_seeds_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_seeds_reg, num_seeds as i64));

                    // Call sol_create_program_address to get the PDA
                    // sol_create_program_address(seeds, seeds_len, program_id, result_address)
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(result),
                        "sol_create_program_address".to_string(),
                        vec![heap_base, num_seeds_reg, program_pk_ptr, dest_ptr],
                    ));

                    // Load and return the bump seed
                    let bump = self.alloc_reg();
                    self.emit(IrInstruction::Load1(bump, bump_ptr, 0));

                    return Ok(Some(bump));
                }

                // =================================================================
                // GET-ATA: Derive Associated Token Account address
                // =================================================================
                // (get-ata dest-ptr wallet-pubkey-ptr mint-pubkey-ptr)
                //
                // Derives the Associated Token Account address for a wallet/mint pair.
                // Uses the standard ATA derivation:
                //   seeds = [wallet, TOKEN_PROGRAM_ID, mint]
                //   program = ATA_PROGRAM_ID (ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL)
                //
                // The result is written to dest-ptr (32 bytes)
                // =================================================================
                if name == "get-ata" && args.len() == 3 {
                    let dest_ptr = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("get-ata dest-ptr has no result"))?;
                    let wallet_ptr = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("get-ata wallet-pubkey-ptr has no result"))?;
                    let mint_ptr = self
                        .generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("get-ata mint-pubkey-ptr has no result"))?;

                    // Build seeds array in heap at 0x300000E00
                    // Seeds: [wallet (32), token_program (32), mint (32)]
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000E00_i64));

                    // Store seed structures at heap_base
                    // Each seed: { addr: *u8, len: u64 } = 16 bytes

                    // Seed 0: wallet pubkey (32 bytes)
                    self.emit(IrInstruction::Store(heap_base, wallet_ptr, 0));
                    let thirty_two = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(thirty_two, 32));
                    self.emit(IrInstruction::Store(heap_base, thirty_two, 8));

                    // Seed 1: Token Program ID - store at heap_base + 64 (32 bytes)
                    // TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in bytes
                    let token_prog_ptr = self.alloc_reg();
                    let const_64 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_64, 64));
                    self.emit(IrInstruction::Add(token_prog_ptr, heap_base, const_64));

                    // Token Program ID bytes (hard-coded)
                    // We'll write the known bytes for TOKEN_PROGRAM_ID
                    // TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA
                    // = [6, 221, 246, ...] - first 8 bytes as u64
                    let token_prog_bytes_0 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(
                        token_prog_bytes_0,
                        0x8c97258f4e2489f1_u64 as i64,
                    ));
                    self.emit(IrInstruction::Store(token_prog_ptr, token_prog_bytes_0, 0));
                    let token_prog_bytes_1 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(
                        token_prog_bytes_1,
                        0x39d0a8d9b3b71f14_u64 as i64,
                    ));
                    self.emit(IrInstruction::Store(token_prog_ptr, token_prog_bytes_1, 8));
                    let token_prog_bytes_2 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(
                        token_prog_bytes_2,
                        0x9d5bce6b0c6a1de5_u64 as i64,
                    ));
                    self.emit(IrInstruction::Store(token_prog_ptr, token_prog_bytes_2, 16));
                    let token_prog_bytes_3 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(
                        token_prog_bytes_3,
                        0x06ddf6e1d765a193_u64 as i64,
                    ));
                    self.emit(IrInstruction::Store(token_prog_ptr, token_prog_bytes_3, 24));

                    // Seed 1 structure at heap_base + 16
                    self.emit(IrInstruction::Store(heap_base, token_prog_ptr, 16));
                    self.emit(IrInstruction::Store(heap_base, thirty_two, 24));

                    // Seed 2: mint pubkey
                    self.emit(IrInstruction::Store(heap_base, mint_ptr, 32));
                    self.emit(IrInstruction::Store(heap_base, thirty_two, 40));

                    // ATA Program ID at heap_base + 128
                    // ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL
                    let ata_prog_ptr = self.alloc_reg();
                    let const_128 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_128, 128));
                    self.emit(IrInstruction::Add(ata_prog_ptr, heap_base, const_128));

                    let ata_bytes_0 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(
                        ata_bytes_0,
                        0x8c97258f4e2489f1_u64 as i64,
                    )); // placeholder
                    self.emit(IrInstruction::Store(ata_prog_ptr, ata_bytes_0, 0));
                    let ata_bytes_1 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(
                        ata_bytes_1,
                        0x39d0a8d9b3b71f14_u64 as i64,
                    ));
                    self.emit(IrInstruction::Store(ata_prog_ptr, ata_bytes_1, 8));
                    let ata_bytes_2 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(
                        ata_bytes_2,
                        0x9d5bce6b0c6a1de5_u64 as i64,
                    ));
                    self.emit(IrInstruction::Store(ata_prog_ptr, ata_bytes_2, 16));
                    let ata_bytes_3 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(
                        ata_bytes_3,
                        0x06ddf6e1d765a193_u64 as i64,
                    ));
                    self.emit(IrInstruction::Store(ata_prog_ptr, ata_bytes_3, 24));

                    // Call sol_create_program_address(seeds, num_seeds, program_id, result)
                    let three = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(three, 3));

                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(result),
                        "sol_create_program_address".to_string(),
                        vec![heap_base, three, ata_prog_ptr, dest_ptr],
                    ));

                    return Ok(Some(result));
                }

                // =================================================================
                // ACCOUNT VALIDATION MACROS
                // =================================================================
                // These provide runtime checks for account properties.
                // On failure, they abort the program with an error code.
                // =================================================================

                // (assert-signer account-idx) - Abort if account is not a signer
                // Returns 0 on success, aborts on failure
                if name == "assert-signer" && args.len() == 1 {
                    let account_idx = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("assert-signer account-idx has no result"))?;

                    // Get pointer to is_signer field (offset +1 from account base) using dynamic offset
                    let is_signer_ptr = self.emit_get_account_field_ptr(account_idx, 1);

                    // Load is_signer byte
                    let is_signer = self.alloc_reg();
                    self.emit(IrInstruction::Load1(is_signer, is_signer_ptr, 0));

                    // If is_signer == 0, abort with error
                    let label_ok = self.new_label("signer_ok");
                    let label_fail = self.new_label("signer_fail");

                    self.emit(IrInstruction::JumpIf(is_signer, label_ok.clone()));
                    self.emit(IrInstruction::Label(label_fail));
                    // Abort: sol_log_ "Missing signer" then return error
                    let error_code = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(error_code, 0x1000000)); // Custom error
                    self.emit(IrInstruction::Syscall(
                        None,
                        "sol_panic_".to_string(),
                        vec![error_code, error_code, error_code, error_code, error_code],
                    ));

                    self.emit(IrInstruction::Label(label_ok));
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));

                    return Ok(Some(zero));
                }

                // (assert-writable account-idx) - Abort if account is not writable
                if name == "assert-writable" && args.len() == 1 {
                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("assert-writable account-idx has no result")
                    })?;

                    // Get pointer to is_writable field (offset +2 from account base) using dynamic offset
                    let is_writable_ptr = self.emit_get_account_field_ptr(account_idx, 2);

                    let is_writable = self.alloc_reg();
                    self.emit(IrInstruction::Load1(is_writable, is_writable_ptr, 0));

                    let label_ok = self.new_label("writable_ok");
                    self.emit(IrInstruction::JumpIf(is_writable, label_ok.clone()));
                    let error_code = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(error_code, 0x2000000));
                    self.emit(IrInstruction::Syscall(
                        None,
                        "sol_panic_".to_string(),
                        vec![error_code, error_code, error_code, error_code, error_code],
                    ));

                    self.emit(IrInstruction::Label(label_ok));
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));

                    return Ok(Some(zero));
                }

                // (assert-owner account-idx expected-owner-ptr) - Abort if account owner doesn't match
                if name == "assert-owner" && args.len() == 2 {
                    let account_idx = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("assert-owner account-idx has no result"))?;
                    let expected_owner = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("assert-owner expected-owner-ptr has no result")
                    })?;

                    // Get pointer to owner field (offset +40 from account base) using dynamic offset
                    let owner_ptr = self.emit_get_account_field_ptr(account_idx, 40);

                    // Compare 32 bytes using sol_memcmp_
                    let thirty_two = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(thirty_two, 32));

                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(result),
                        "sol_memcmp_".to_string(),
                        vec![owner_ptr, expected_owner, thirty_two, result],
                    ));

                    // If result != 0, abort
                    let label_ok = self.new_label("owner_ok");
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    self.emit(IrInstruction::Eq(result, result, zero));
                    self.emit(IrInstruction::JumpIf(result, label_ok.clone()));

                    let error_code = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(error_code, 0x3000000));
                    self.emit(IrInstruction::Syscall(
                        None,
                        "sol_panic_".to_string(),
                        vec![error_code, error_code, error_code, error_code, error_code],
                    ));

                    self.emit(IrInstruction::Label(label_ok));

                    return Ok(Some(zero));
                }

                // (is-signer account-idx) - Returns 1 if signer, 0 if not (no abort)
                // Uses precomputed account offset table for dynamic account sizes
                if name == "is-signer" && args.len() == 1 {
                    let account_idx = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("is-signer account-idx has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get precomputed account base offset from table
                    let acct_offset = self.emit_get_account_offset(account_idx);

                    // is_signer is at offset 1 from account start
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, acct_offset, one));

                    let is_signer_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        is_signer_ptr,
                        accounts_ptr,
                        total_offset,
                    ));

                    let is_signer = self.alloc_reg();
                    self.emit(IrInstruction::Load1(is_signer, is_signer_ptr, 0));

                    return Ok(Some(is_signer));
                }

                // (is-writable account-idx) - Returns 1 if writable, 0 if not
                // Uses precomputed account offset table for dynamic account sizes
                if name == "is-writable" && args.len() == 1 {
                    let account_idx = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("is-writable account-idx has no result"))?;

                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get precomputed account base offset from table
                    let acct_offset = self.emit_get_account_offset(account_idx);

                    // is_writable is at offset 2 from account start
                    let two = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(two, 2));
                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, acct_offset, two));

                    let is_writable_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        is_writable_ptr,
                        accounts_ptr,
                        total_offset,
                    ));

                    let is_writable = self.alloc_reg();
                    self.emit(IrInstruction::Load1(is_writable, is_writable_ptr, 0));

                    return Ok(Some(is_writable));
                }

                // =================================================================
                // ZERO-COPY ACCOUNT ACCESS
                // =================================================================
                // These macros provide direct memory access to account data
                // without copying. Essential for high-performance programs.
                // =================================================================

                // (zerocopy-load StructName account-idx field-name) -> value
                // Directly loads a field from account data without struct-get overhead
                // Uses the struct definition for offset calculation
                // MEMORY MODEL: Validates field access and registers result type
                if name == "zerocopy-load" && args.len() == 3 {
                    if let (Expression::Variable(struct_name), Expression::Variable(field_name)) =
                        (&args[0].value, &args[2].value)
                    {
                        let account_idx = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                            Error::runtime("zerocopy-load account-idx has no result")
                        })?;

                        // MEMORY MODEL: Try to extract constant account index for type tracking
                        let account_idx_const: Option<u8> = match &args[1].value {
                            Expression::IntLiteral(n) => Some(*n as u8),
                            _ => None,
                        };

                        let struct_def = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .clone();

                        let field = struct_def
                            .fields
                            .iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| {
                                Error::runtime(format!(
                                    "Unknown field '{}' in struct '{}'",
                                    field_name, struct_name
                                ))
                            })?;

                        // Calculate account data pointer using dynamic offset table
                        let accounts_ptr = *self
                            .var_map
                            .get("accounts")
                            .ok_or_else(|| Error::runtime("accounts not available"))?;

                        let acct_offset = self.emit_get_account_offset(account_idx);

                        // Data starts at offset 88 from account start (after headers)
                        // = 1+1+1+1+4+32+32+8+8 = 88
                        let data_offset = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(data_offset, 88));
                        let total_offset = self.alloc_reg();
                        self.emit(IrInstruction::Add(total_offset, acct_offset, data_offset));

                        let data_ptr = self.alloc_reg();
                        self.emit(IrInstruction::Add(data_ptr, accounts_ptr, total_offset));

                        // MEMORY MODEL: Register data_ptr as typed account data pointer
                        if let Some(idx) = account_idx_const {
                            self.type_env.set_type(
                                data_ptr,
                                RegType::Pointer(PointerType::struct_ptr(
                                    idx,
                                    struct_name.clone(),
                                    struct_def.total_size,
                                    None,
                                )),
                            );
                        }

                        // Load field at offset
                        let dst = self.alloc_reg();
                        let field_offset = field.offset;
                        let field_type_clone = field.field_type.clone();

                        match &field.field_type {
                            FieldType::Primitive(PrimitiveType::U8)
                            | FieldType::Primitive(PrimitiveType::I8) => {
                                self.emit(IrInstruction::Load1(dst, data_ptr, field_offset));
                                let signed = matches!(
                                    field.field_type,
                                    FieldType::Primitive(PrimitiveType::I8)
                                );
                                self.type_env
                                    .set_type(dst, RegType::Value { size: 1, signed });
                            }
                            FieldType::Primitive(PrimitiveType::U16)
                            | FieldType::Primitive(PrimitiveType::I16) => {
                                self.emit(IrInstruction::Load2(dst, data_ptr, field_offset));
                                let signed = matches!(
                                    field.field_type,
                                    FieldType::Primitive(PrimitiveType::I16)
                                );
                                self.type_env
                                    .set_type(dst, RegType::Value { size: 2, signed });
                            }
                            FieldType::Primitive(PrimitiveType::U32)
                            | FieldType::Primitive(PrimitiveType::I32) => {
                                self.emit(IrInstruction::Load4(dst, data_ptr, field_offset));
                                let signed = matches!(
                                    field.field_type,
                                    FieldType::Primitive(PrimitiveType::I32)
                                );
                                self.type_env
                                    .set_type(dst, RegType::Value { size: 4, signed });
                            }
                            FieldType::Primitive(PrimitiveType::U64)
                            | FieldType::Primitive(PrimitiveType::I64) => {
                                self.emit(IrInstruction::Load(dst, data_ptr, field_offset));
                                let signed = matches!(
                                    field.field_type,
                                    FieldType::Primitive(PrimitiveType::I64)
                                );
                                self.type_env
                                    .set_type(dst, RegType::Value { size: 8, signed });
                            }
                            FieldType::Pubkey | FieldType::Array { .. } | FieldType::Struct(_) => {
                                // Return pointer to field for complex types
                                let offset_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(offset_reg, field_offset));
                                self.emit(IrInstruction::Add(dst, data_ptr, offset_reg));

                                // Register result as pointer
                                let field_size = match &field_type_clone {
                                    FieldType::Pubkey => 32,
                                    FieldType::Array {
                                        element_type,
                                        count,
                                    } => element_type.size() * (*count as i64),
                                    FieldType::Struct(nested_name) => self
                                        .struct_defs
                                        .get(nested_name)
                                        .map(|s| s.total_size)
                                        .unwrap_or(0),
                                    _ => 0,
                                };
                                if let Some(idx) = account_idx_const {
                                    self.type_env.set_type(
                                        dst,
                                        RegType::Pointer(PointerType {
                                            region: MemoryRegion::AccountData(idx),
                                            bounds: Some((field_offset, field_size)),
                                            struct_type: Some(format!(
                                                "{}.{}",
                                                struct_name, field_name
                                            )),
                                            offset: field_offset,
                                            alignment: Alignment::Byte1,
                                            writable: true,
                                        }),
                                    );
                                }
                            }
                        }

                        return Ok(Some(dst));
                    }
                }

                // (zerocopy-store StructName account-idx field-name value) -> void
                // Directly stores a value to account data
                // MEMORY MODEL: Validates field access and checks account writability
                if name == "zerocopy-store" && args.len() == 4 {
                    if let (Expression::Variable(struct_name), Expression::Variable(field_name)) =
                        (&args[0].value, &args[2].value)
                    {
                        let account_idx = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                            Error::runtime("zerocopy-store account-idx has no result")
                        })?;

                        // MEMORY MODEL: Try to extract constant account index for validation
                        let account_idx_const: Option<u8> = match &args[1].value {
                            Expression::IntLiteral(n) => Some(*n as u8),
                            _ => None,
                        };

                        // MEMORY MODEL: Validate account index if known
                        if let Some(idx) = account_idx_const {
                            if let Err(e) = self.type_env.validate_account_index(idx) {
                                self.type_env.record_error(e);
                            }
                        }

                        let value = self
                            .generate_expr(&args[3].value)?
                            .ok_or_else(|| Error::runtime("zerocopy-store value has no result"))?;

                        let struct_def = self
                            .struct_defs
                            .get(struct_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown struct '{}'", struct_name))
                            })?
                            .clone();

                        let field = struct_def
                            .fields
                            .iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| {
                                Error::runtime(format!(
                                    "Unknown field '{}' in struct '{}'",
                                    field_name, struct_name
                                ))
                            })?;

                        // Calculate account data pointer using dynamic offset table
                        let accounts_ptr = *self
                            .var_map
                            .get("accounts")
                            .ok_or_else(|| Error::runtime("accounts not available"))?;

                        let acct_offset = self.emit_get_account_offset(account_idx);

                        // Data starts at offset 88 from account start (after headers)
                        // = 1+1+1+1+4+32+32+8+8 = 88
                        let data_offset = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(data_offset, 88));
                        let total_offset = self.alloc_reg();
                        self.emit(IrInstruction::Add(total_offset, acct_offset, data_offset));

                        let data_ptr = self.alloc_reg();
                        self.emit(IrInstruction::Add(data_ptr, accounts_ptr, total_offset));

                        // MEMORY MODEL: Register data_ptr as typed account data pointer
                        if let Some(idx) = account_idx_const {
                            self.type_env.set_type(
                                data_ptr,
                                RegType::Pointer(PointerType::struct_ptr(
                                    idx,
                                    struct_name.clone(),
                                    struct_def.total_size,
                                    None,
                                )),
                            );
                        }

                        let field_offset = field.offset;

                        match &field.field_type {
                            FieldType::Primitive(PrimitiveType::U8)
                            | FieldType::Primitive(PrimitiveType::I8) => {
                                self.emit(IrInstruction::Store1(data_ptr, value, field_offset));
                            }
                            FieldType::Primitive(PrimitiveType::U16)
                            | FieldType::Primitive(PrimitiveType::I16) => {
                                self.emit(IrInstruction::Store2(data_ptr, value, field_offset));
                            }
                            FieldType::Primitive(PrimitiveType::U32)
                            | FieldType::Primitive(PrimitiveType::I32) => {
                                self.emit(IrInstruction::Store4(data_ptr, value, field_offset));
                            }
                            FieldType::Primitive(PrimitiveType::U64)
                            | FieldType::Primitive(PrimitiveType::I64) => {
                                self.emit(IrInstruction::Store(data_ptr, value, field_offset));
                            }
                            _ => {
                                return Err(Error::runtime(format!(
                                    "zerocopy-store cannot directly store complex type '{}' - use memcpy",
                                    field_name
                                )));
                            }
                        }

                        let zero = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(zero, 0));
                        return Ok(Some(zero));
                    }
                }

                // (account-data-len account-idx) -> length of account data
                if name == "account-data-len" && args.len() == 1 {
                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("account-data-len account-idx has no result")
                    })?;

                    // Get pointer to data_len field (offset +80 from account base) using dynamic offset
                    let len_ptr = self.emit_get_account_field_ptr(account_idx, 80);

                    let data_len = self.alloc_reg();
                    self.emit(IrInstruction::Load(data_len, len_ptr, 0));

                    return Ok(Some(data_len));
                }

                // (account-lamports account-idx) -> lamports balance
                if name == "account-lamports" && args.len() == 1 {
                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("account-lamports account-idx has no result")
                    })?;

                    // Get pointer to lamports field (offset +72 from account base) using dynamic offset
                    let lamports_ptr = self.emit_get_account_field_ptr(account_idx, 72);

                    let lamports = self.alloc_reg();
                    self.emit(IrInstruction::Load(lamports, lamports_ptr, 0));

                    return Ok(Some(lamports));
                }

                // =================================================================
                // EVENT EMISSION HELPERS
                // =================================================================
                // Anchor-style event emission for indexer compatibility
                // Events are logged with a discriminator prefix for easy parsing
                // =================================================================

                // (emit-event EventStruct data-ptr)
                // Emits a structured event by logging: [discriminator][borsh-serialized-data]
                // Discriminator is first 8 bytes of sha256("event:EventName")
                if name == "emit-event" && args.len() == 2 {
                    if let Expression::Variable(event_name) = &args[0].value {
                        let data_ptr = self
                            .generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("emit-event data-ptr has no result"))?;

                        // Get struct definition for size
                        let struct_def = self
                            .struct_defs
                            .get(event_name)
                            .ok_or_else(|| {
                                Error::runtime(format!("Unknown event struct '{}'", event_name))
                            })?
                            .clone();

                        // Build event buffer in heap: 8-byte discriminator + data
                        let event_buffer = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(event_buffer, 0x300001000_i64));

                        // Generate simple discriminator based on event name hash
                        // In production, this would be sha256("event:EventName")[0..8]
                        // For now, use a simple hash of the event name
                        let discriminator: i64 =
                            event_name.bytes().enumerate().fold(0i64, |acc, (i, b)| {
                                acc.wrapping_add((b as i64) << ((i % 8) * 8))
                            });

                        let disc_reg = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(disc_reg, discriminator));
                        self.emit(IrInstruction::Store(event_buffer, disc_reg, 0));

                        // Copy struct data after discriminator
                        let data_size = struct_def.total_size;
                        let size_reg = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(size_reg, data_size));

                        let dest_ptr = self.alloc_reg();
                        let eight = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(eight, 8));
                        self.emit(IrInstruction::Add(dest_ptr, event_buffer, eight));

                        // Use sol_memcpy_ to copy the data
                        self.emit(IrInstruction::Syscall(
                            None,
                            "sol_memcpy_".to_string(),
                            vec![dest_ptr, data_ptr, size_reg],
                        ));

                        // Log the entire event (8 + data_size bytes)
                        let total_size = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(total_size, 8 + data_size));

                        self.emit(IrInstruction::Syscall(
                            None,
                            "sol_log_data".to_string(),
                            vec![event_buffer, total_size],
                        ));

                        let zero = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(zero, 0));
                        return Ok(Some(zero));
                    }
                }

                // (emit-log "message" value1 value2 ...)
                // Simple event logging with up to 5 u64 values (uses sol_log_64_)
                if name == "emit-log" && !args.is_empty() {
                    // First arg is message string
                    if let Expression::StringLiteral(ref msg) = args[0].value {
                        // Generate message pointer (same as sol_log_)
                        let msg_ptr = self
                            .generate_expr(&args[0].value)?
                            .ok_or_else(|| Error::runtime("emit-log message has no result"))?;
                        let msg_len = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(msg_len, msg.len() as i64));
                        self.emit(IrInstruction::Syscall(
                            None,
                            "sol_log_".to_string(),
                            vec![msg_ptr, msg_len],
                        ));

                        // Log values if provided (up to 5)
                        if args.len() > 1 {
                            let mut values = Vec::new();
                            for i in 1..=5 {
                                if i < args.len() {
                                    let val =
                                        self.generate_expr(&args[i].value)?.ok_or_else(|| {
                                            Error::runtime("emit-log value has no result")
                                        })?;
                                    values.push(val);
                                } else {
                                    let zero = self.alloc_reg();
                                    self.emit(IrInstruction::ConstI64(zero, 0));
                                    values.push(zero);
                                }
                            }
                            self.emit(IrInstruction::Syscall(
                                None,
                                "sol_log_64_".to_string(),
                                values,
                            ));
                        }

                        let zero = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(zero, 0));
                        return Ok(Some(zero));
                    }
                }

                // =================================================================
                // SYSVAR ACCESS HELPERS
                // =================================================================
                // Read Solana sysvars from their fixed addresses
                // Sysvars are passed as accounts in the transaction
                // =================================================================

                // (clock-slot) -> current slot number (from Clock sysvar)
                // Clock sysvar layout: slot(u64), epoch_start_timestamp(i64), epoch(u64), leader_schedule_epoch(u64), unix_timestamp(i64)
                if name == "clock-slot" && args.is_empty() {
                    // Clock sysvar must be passed as an account
                    // Clock address: SysvarC1ock11111111111111111111111111111111
                    // For simplicity, assume clock is at account index specified or we read from a known location
                    // Actually, we need the clock account to be passed in - this reads from account data

                    // This simplified version expects clock at a specific account index
                    // In production, you'd verify the pubkey matches the Clock sysvar
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(result, 0)); // Placeholder - needs runtime sysvar access
                    return Ok(Some(result));
                }

                // (clock-unix-timestamp sysvar-account-idx) -> unix timestamp
                // Reads from Clock sysvar account data
                if name == "clock-unix-timestamp" && args.len() == 1 {
                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("clock-unix-timestamp account-idx has no result")
                    })?;

                    // Data starts at +88, unix_timestamp is at offset 32 in Clock struct
                    // Total offset = 88 + 32 = 120
                    let ts_ptr = self.emit_get_account_field_ptr(account_idx, 88 + 32);

                    let timestamp = self.alloc_reg();
                    self.emit(IrInstruction::Load(timestamp, ts_ptr, 0));

                    return Ok(Some(timestamp));
                }

                // (clock-epoch sysvar-account-idx) -> current epoch
                if name == "clock-epoch" && args.len() == 1 {
                    let account_idx = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("clock-epoch account-idx has no result"))?;

                    // epoch is at offset 16 in Clock struct (after slot and epoch_start_timestamp)
                    // Data starts at +88, so total offset = 88 + 16 = 104
                    let epoch_ptr = self.emit_get_account_field_ptr(account_idx, 88 + 16);

                    let epoch = self.alloc_reg();
                    self.emit(IrInstruction::Load(epoch, epoch_ptr, 0));

                    return Ok(Some(epoch));
                }

                // (rent-minimum-balance sysvar-account-idx data-size) -> minimum lamports for rent exemption
                // Rent sysvar layout: lamports_per_byte_year(u64), exemption_threshold(f64), burn_percent(u8)
                if name == "rent-minimum-balance" && args.len() == 2 {
                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("rent-minimum-balance account-idx has no result")
                    })?;
                    let data_size = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("rent-minimum-balance data-size has no result")
                    })?;

                    // lamports_per_byte_year at offset 0 in Rent struct
                    // Data starts at +88, so total offset = 88
                    let rent_ptr = self.emit_get_account_field_ptr(account_idx, 88);

                    let lamports_per_byte = self.alloc_reg();
                    self.emit(IrInstruction::Load(lamports_per_byte, rent_ptr, 0));

                    // Simple calculation: lamports_per_byte * (data_size + 128) * 2
                    // The 128 accounts for account overhead, *2 for 2-year exemption
                    let overhead = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(overhead, 128));
                    let total_size = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_size, data_size, overhead));

                    let base_cost = self.alloc_reg();
                    self.emit(IrInstruction::Mul(base_cost, lamports_per_byte, total_size));

                    let two = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(two, 2));
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Mul(result, base_cost, two));

                    return Ok(Some(result));
                }

                // =================================================================
                // INSTRUCTION INTROSPECTION
                // =================================================================
                // Read from the Instructions sysvar for security checks
                // Enables CPI guards and re-entrancy protection
                // =================================================================

                // (instruction-count sysvar-account-idx) -> number of instructions in transaction
                // Instructions sysvar: first 2 bytes are u16 count, then serialized instructions
                if name == "instruction-count" && args.len() == 1 {
                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("instruction-count account-idx has no result")
                    })?;

                    // Data starts at +88, instruction count is first 2 bytes
                    let count_ptr = self.emit_get_account_field_ptr(account_idx, 88);

                    // Read u16 count (2 bytes)
                    let count = self.alloc_reg();
                    self.emit(IrInstruction::Load2(count, count_ptr, 0));

                    return Ok(Some(count));
                }

                // (current-instruction-index sysvar-account-idx) -> index of current instruction
                // Uses sol_get_processed_sibling_instruction or reads from sysvar directly
                if name == "current-instruction-index" && args.len() == 1 {
                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("current-instruction-index account-idx has no result")
                    })?;

                    // The current instruction index is typically passed by the runtime
                    // For now, use a syscall to get it
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(result),
                        "sol_get_return_data".to_string(), // Placeholder - actual syscall varies
                        vec![account_idx],
                    ));

                    return Ok(Some(result));
                }

                // (assert-not-cpi) -> Abort if called via CPI (instruction index > 0)
                // Used to prevent re-entrancy attacks
                if name == "assert-not-cpi" && args.len() == 1 {
                    let sysvar_idx = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("assert-not-cpi sysvar-idx has no result"))?;

                    // Get pointer to instruction data (skip count at offset +88, check first instruction at +90)
                    let _data_ptr = self.emit_get_account_field_ptr(sysvar_idx, 88 + 2);

                    // If we're executing as part of CPI, stack depth > 1
                    // For now, use a simple check - in production you'd verify the call stack
                    let label_ok = self.new_label("not_cpi_ok");
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));

                    // This is a simplified version - real implementation would check call stack
                    self.emit(IrInstruction::Jump(label_ok.clone()));
                    self.emit(IrInstruction::Label(label_ok));

                    return Ok(Some(zero));
                }

                // =================================================================
                // PDA BUMP CACHING
                // =================================================================
                // Cache discovered PDA bumps to avoid repeated derivation costs
                // Each derivation costs ~1500 CU; caching saves 90%+ on repeated calls
                // =================================================================

                // (pda-cache-init cache-account-idx) -> Initialize a bump cache in account data
                // Cache layout: [magic: u32][count: u32][entries: (hash: [u8; 8], bump: u8)*]
                if name == "pda-cache-init" && args.len() == 1 {
                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("pda-cache-init account-idx has no result")
                    })?;

                    // Get data pointer (offset +88)
                    let cache_ptr = self.emit_get_account_field_ptr(account_idx, 88);

                    // Write magic number: 0x50444143 ("PDAC")
                    let magic = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(magic, 0x50444143));
                    self.emit(IrInstruction::Store4(cache_ptr, magic, 0));

                    // Write count: 0
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    self.emit(IrInstruction::Store4(cache_ptr, zero, 4));

                    return Ok(Some(zero));
                }

                // (pda-cache-lookup cache-account-idx seed-hash-ptr) -> bump or 0 if not found
                // seed-hash-ptr points to 8-byte hash of seeds
                if name == "pda-cache-lookup" && args.len() == 2 {
                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("pda-cache-lookup account-idx has no result")
                    })?;
                    let _seed_hash = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("pda-cache-lookup seed-hash has no result")
                    })?;

                    // Get cache pointer (offset +88)
                    let cache_ptr = self.emit_get_account_field_ptr(account_idx, 88);

                    // Read count
                    let count_ptr = self.alloc_reg();
                    let four = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(four, 4));
                    self.emit(IrInstruction::Add(count_ptr, cache_ptr, four));

                    let count = self.alloc_reg();
                    self.emit(IrInstruction::Load4(count, count_ptr, 0));

                    // Linear search through entries (each entry is 9 bytes: 8-byte hash + 1-byte bump)
                    // For now, return 0 (not found) - full implementation would loop
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(result, 0));

                    // TODO: Implement loop to search entries
                    // This is a placeholder - full implementation needs loop constructs

                    return Ok(Some(result));
                }

                // (pda-cache-store cache-account-idx seed-hash bump) -> success (0)
                // Stores a bump for the given seed hash
                if name == "pda-cache-store" && args.len() == 3 {
                    let account_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("pda-cache-store account-idx has no result")
                    })?;
                    let seed_hash = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("pda-cache-store seed-hash has no result"))?;
                    let bump = self
                        .generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("pda-cache-store bump has no result"))?;

                    // Get cache pointer (offset +88)
                    let cache_ptr = self.emit_get_account_field_ptr(account_idx, 88);

                    // Read current count
                    let count_ptr = self.alloc_reg();
                    let four = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(four, 4));
                    self.emit(IrInstruction::Add(count_ptr, cache_ptr, four));

                    let count = self.alloc_reg();
                    self.emit(IrInstruction::Load4(count, count_ptr, 0));

                    // Calculate entry offset: 8 (header) + count * 9
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let nine = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(nine, 9));
                    let entry_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(entry_offset, count, nine));
                    self.emit(IrInstruction::Add(entry_offset, entry_offset, eight));

                    let entry_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(entry_ptr, cache_ptr, entry_offset));

                    // Store hash (8 bytes)
                    self.emit(IrInstruction::Store(entry_ptr, seed_hash, 0));

                    // Store bump (1 byte)
                    self.emit(IrInstruction::Store1(entry_ptr, bump, 8));

                    // Increment count
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    let new_count = self.alloc_reg();
                    self.emit(IrInstruction::Add(new_count, count, one));
                    self.emit(IrInstruction::Store4(count_ptr, new_count, 0));

                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    return Ok(Some(zero));
                }

                // (derive-pda-cached cache-account-idx program-ptr seeds-ptr bump-ptr dest-ptr) -> success
                // First checks cache, then derives if not found and stores in cache
                if name == "derive-pda-cached" && args.len() == 5 {
                    let cache_idx = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("derive-pda-cached cache-idx has no result")
                    })?;
                    let program_ptr = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("derive-pda-cached program-ptr has no result")
                    })?;
                    let seeds_ptr = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                        Error::runtime("derive-pda-cached seeds-ptr has no result")
                    })?;
                    let bump_ptr = self.generate_expr(&args[3].value)?.ok_or_else(|| {
                        Error::runtime("derive-pda-cached bump-ptr has no result")
                    })?;
                    let dest_ptr = self.generate_expr(&args[4].value)?.ok_or_else(|| {
                        Error::runtime("derive-pda-cached dest-ptr has no result")
                    })?;

                    // For now, just call the underlying PDA derivation
                    // Full implementation would check cache first
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(result),
                        "sol_try_find_program_address".to_string(),
                        vec![seeds_ptr, program_ptr, dest_ptr, bump_ptr],
                    ));

                    return Ok(Some(result));
                }

                // Handle (build-instruction program-ptr data-ptr data-len) -> instruction ptr
                // Returns pointer to a SolInstruction struct built in heap memory
                if name == "build-instruction" && args.len() == 3 {
                    let program_ptr = self.generate_expr(&args[0].value)?.ok_or_else(|| {
                        Error::runtime("build-instruction program-ptr has no result")
                    })?;
                    let data_ptr = self.generate_expr(&args[1].value)?.ok_or_else(|| {
                        Error::runtime("build-instruction data-ptr has no result")
                    })?;
                    let data_len = self.generate_expr(&args[2].value)?.ok_or_else(|| {
                        Error::runtime("build-instruction data-len has no result")
                    })?;

                    // Use heap region 0x300000600 for instruction structure
                    let instr_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(instr_ptr, 0x300000600_i64));

                    // Store(base, value, offset) - store program_id pointer
                    self.emit(IrInstruction::Store(instr_ptr, program_ptr, 0));

                    // accounts = NULL, accounts_len = 0 for simple CPIs
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));
                    self.emit(IrInstruction::Store(instr_ptr, zero, 8));
                    self.emit(IrInstruction::Store(instr_ptr, zero, 16));

                    // Store data pointer and length
                    self.emit(IrInstruction::Store(instr_ptr, data_ptr, 24));
                    self.emit(IrInstruction::Store(instr_ptr, data_len, 32));

                    return Ok(Some(instr_ptr));
                }

                // Handle (println msg) - no-op for local testing (just evaluate and discard)
                if name == "println" && args.len() == 1 {
                    // Evaluate the argument (so side effects happen) but discard the result
                    let _result = self.generate_expr(&args[0].value)?;

                    // Return success (0)
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(dst, 0));
                    return Ok(Some(dst));
                }

                // Handle (log :message msg :value val) - keyword-arg logging with sol_log_ syscall
                if name == "log" {
                    eprintln!("🔍 IR DEBUG: log handler called with {} args", args.len());
                    for (i, arg) in args.iter().enumerate() {
                        eprintln!(
                            "  arg[{}]: name={:?}, value type={:?}",
                            i, arg.name, arg.value
                        );
                    }
                    // Parse keyword arguments
                    let mut message_expr: Option<&Expression> = None;
                    let mut value_expr: Option<&Expression> = None;

                    for arg in args {
                        if let Some(ref kw) = arg.name {
                            match kw.as_str() {
                                "message" => message_expr = Some(&arg.value),
                                "value" => value_expr = Some(&arg.value),
                                _ => {} // Ignore unknown keywords
                            }
                        }
                    }

                    // Build the log message
                    let mut log_parts = Vec::new();

                    // Add message part if present
                    if let Some(msg_expr) = message_expr {
                        match msg_expr {
                            Expression::StringLiteral(s) => {
                                log_parts.push(s.clone());
                            }
                            _ => {
                                // For non-literals, evaluate and try to convert
                                // For now, we'll just skip non-string literals
                                // TODO: Add runtime string formatting
                                return Err(Error::runtime(
                                    "log :message must be a string literal for compilation",
                                ));
                            }
                        }
                    }

                    // Add value part if present
                    if let Some(val_expr) = value_expr {
                        match val_expr {
                            Expression::IntLiteral(n) => {
                                log_parts.push(n.to_string());
                            }
                            Expression::FloatLiteral(f) => {
                                log_parts.push(f.to_string());
                            }
                            Expression::BoolLiteral(b) => {
                                log_parts.push(b.to_string());
                            }
                            Expression::StringLiteral(s) => {
                                log_parts.push(s.clone());
                            }
                            _ => {
                                // For dynamic values, use sol_log_64_ syscall instead
                                // Evaluate the value expression
                                let val_reg = self
                                    .generate_expr(val_expr)?
                                    .ok_or_else(|| Error::runtime("log value has no result"))?;

                                // If we have a message, log it first with sol_log_
                                if !log_parts.is_empty() {
                                    let msg = log_parts.join(" ");
                                    let idx = self.strings.len();
                                    self.strings.push(msg.clone());
                                    let msg_reg = self.alloc_reg();
                                    self.emit(IrInstruction::ConstString(msg_reg, idx));
                                    self.emit(IrInstruction::Log(msg_reg, msg.len()));
                                }

                                // Then log the dynamic value with sol_log_64_
                                let dst = self.alloc_reg();
                                self.emit(IrInstruction::Syscall(
                                    Some(dst),
                                    "sol_log_64_".to_string(),
                                    vec![val_reg],
                                ));
                                return Ok(Some(dst));
                            }
                        }
                    }

                    // If we have any log parts, emit a single Log instruction
                    if !log_parts.is_empty() {
                        let full_message = log_parts.join(" ");
                        let idx = self.strings.len();
                        self.strings.push(full_message.clone());

                        let msg_reg = self.alloc_reg();
                        self.emit(IrInstruction::ConstString(msg_reg, idx));
                        self.emit(IrInstruction::Log(msg_reg, full_message.len()));

                        // Return success register
                        let dst = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(dst, 0));
                        return Ok(Some(dst));
                    } else {
                        // No arguments - just return success
                        let dst = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(dst, 0));
                        return Ok(Some(dst));
                    }
                }

                // Handle (do expr1 expr2 ... exprN) - sequence of expressions, return last
                if name == "do" {
                    let mut last_reg = None;
                    for arg in args {
                        last_reg = self.generate_expr(&arg.value)?;
                    }
                    return Ok(last_reg);
                }

                // Handle (while condition body...) - while loop
                if name == "while" && !args.is_empty() {
                    let loop_label = self.new_label("while");
                    let end_label = self.new_label("endwhile");

                    // Loop header
                    self.emit(IrInstruction::Label(loop_label.clone()));

                    // Evaluate condition
                    let cond_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("While condition has no result"))?;

                    // Jump to end if condition is false
                    self.emit(IrInstruction::JumpIfNot(cond_reg, end_label.clone()));

                    // Body - all expressions after the condition
                    for arg in args.iter().skip(1) {
                        self.generate_expr(&arg.value)?;
                    }

                    // Jump back to loop header
                    self.emit(IrInstruction::Jump(loop_label));

                    // End label
                    self.emit(IrInstruction::Label(end_label));

                    // While returns 0 (or null)
                    let result_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(result_reg, 0));
                    return Ok(Some(result_reg));
                }

                // =================================================================
                // CLOCK SYSVAR ACCESS
                // =================================================================
                //
                // Solana Clock Sysvar Structure (40 bytes):
                //   offset 0:  u64 slot (current slot number)
                //   offset 8:  i64 epoch_start_timestamp (unix timestamp of epoch start)
                //   offset 16: u64 epoch (current epoch)
                //   offset 24: u64 leader_schedule_epoch
                //   offset 32: i64 unix_timestamp (current unix timestamp in seconds)
                //
                // sol_get_clock_sysvar signature:
                //   R1: pointer to 40-byte buffer to receive clock data
                //   Returns: 0 on success, error code on failure
                //
                // =================================================================

                // Handle (get-clock-timestamp) - get current Unix timestamp
                if name == "get-clock-timestamp" && args.is_empty() {
                    // Allocate 40 bytes on heap for clock sysvar
                    let heap_base = self.alloc_reg();
                    // Use offset 0x300000200 to avoid conflicting with system-transfer heap usage
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000200_i64));

                    // Call sol_get_clock_sysvar
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(result),
                        "sol_get_clock_sysvar".to_string(),
                        vec![heap_base],
                    ));

                    // Read unix_timestamp from offset 32
                    let timestamp = self.alloc_reg();
                    self.emit(IrInstruction::Load(timestamp, heap_base, 32));

                    return Ok(Some(timestamp));
                }

                // Handle (get-slot) - get current slot number
                if name == "get-slot" && args.is_empty() {
                    // Allocate 40 bytes on heap for clock sysvar
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000200_i64));

                    // Call sol_get_clock_sysvar
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(result),
                        "sol_get_clock_sysvar".to_string(),
                        vec![heap_base],
                    ));

                    // Read slot from offset 0
                    let slot = self.alloc_reg();
                    self.emit(IrInstruction::Load(slot, heap_base, 0));

                    return Ok(Some(slot));
                }

                // Handle (get-epoch) - get current epoch
                if name == "get-epoch" && args.is_empty() {
                    // Allocate 40 bytes on heap for clock sysvar
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000200_i64));

                    // Call sol_get_clock_sysvar
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(result),
                        "sol_get_clock_sysvar".to_string(),
                        vec![heap_base],
                    ));

                    // Read epoch from offset 16
                    let epoch = self.alloc_reg();
                    self.emit(IrInstruction::Load(epoch, heap_base, 16));

                    return Ok(Some(epoch));
                }

                // =================================================================
                // PDA DERIVATION
                // =================================================================
                //
                // sol_try_find_program_address signature:
                //   R1: seeds (SolBytes array) - array of seed buffers
                //   R2: seeds_len (u64) - number of seeds
                //   R3: program_id (u64 ptr) - 32-byte program ID
                //   R4: address_out (u64 ptr) - 32-byte output for derived address
                //   R5: bump_seed_out (u64 ptr) - 1-byte output for bump seed
                //   Returns: 0 on success, error code on failure
                //
                // SolBytes structure (16 bytes):
                //   offset 0: ptr (u64) - pointer to data
                //   offset 8: len (u64) - length of data
                //
                // =================================================================

                // Handle (find-pda program_id_idx seed1 [seed2 ...]) - derive PDA
                // program_id_idx: account index containing the program ID
                // seeds: one or more seed values (strings or integers)
                // Returns: pointer to 32-byte derived address, bump stored at +32
                if name == "find-pda" && args.len() >= 2 {
                    let accounts_ptr = *self
                        .var_map
                        .get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get program_id account index
                    let prog_idx_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("find-pda program_id_idx has no result"))?;

                    // Calculate program_id pubkey pointer using dynamic offset table
                    let prog_base = self.emit_get_account_offset(prog_idx_reg);
                    let pubkey_field_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(pubkey_field_offset, 8));
                    let prog_pubkey_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        prog_pubkey_offset,
                        prog_base,
                        pubkey_field_offset,
                    ));
                    let program_id_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(
                        program_id_ptr,
                        accounts_ptr,
                        prog_pubkey_offset,
                    ));

                    // Use heap for structures
                    // Layout at 0x300000300:
                    //   +0:    SolBytes array (16 bytes per seed, max 16 seeds = 256 bytes)
                    //   +256:  Seed data buffers (variable)
                    //   +768:  Output address (32 bytes)
                    //   +800:  Bump seed (8 bytes, aligned)
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000300_i64));

                    let seeds_array_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Move(seeds_array_ptr, heap_base));

                    let seed_data_base = self.alloc_reg();
                    let offset_256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(offset_256, 256));
                    self.emit(IrInstruction::Add(seed_data_base, heap_base, offset_256));

                    let num_seeds = args.len() - 1; // First arg is program_id_idx
                    let mut seed_data_offset = 0i64;

                    // Build SolBytes array for each seed
                    for (i, arg) in args.iter().skip(1).enumerate() {
                        let sol_bytes_offset = (i * 16) as i64;

                        match &arg.value {
                            Expression::StringLiteral(s) => {
                                // String seed: store string data and create SolBytes
                                let str_len = s.len() as i64;

                                // Store string data
                                let str_idx = self.strings.len();
                                self.strings.push(s.clone());
                                let str_ptr_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstString(str_ptr_reg, str_idx));

                                // Calculate actual data ptr in heap
                                let data_ptr = self.alloc_reg();
                                let offset_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(offset_reg, seed_data_offset));
                                self.emit(IrInstruction::Add(data_ptr, seed_data_base, offset_reg));

                                // Copy string bytes to heap (simplified: just copy ptr for now)
                                // For real implementation, would need memcpy
                                // Instead, use string literal pointer directly

                                // Write SolBytes: ptr at +0, len at +8
                                let sol_bytes_ptr = self.alloc_reg();
                                let sb_offset = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(sb_offset, sol_bytes_offset));
                                self.emit(IrInstruction::Add(
                                    sol_bytes_ptr,
                                    seeds_array_ptr,
                                    sb_offset,
                                ));
                                self.emit(IrInstruction::Store(sol_bytes_ptr, str_ptr_reg, 0));
                                let len_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(len_reg, str_len));
                                self.emit(IrInstruction::Store(sol_bytes_ptr, len_reg, 8));

                                seed_data_offset += str_len;
                            }
                            Expression::IntLiteral(n) => {
                                // Integer seed: convert to 8-byte LE
                                let val_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(val_reg, *n));

                                // Store value in heap data area
                                let data_ptr = self.alloc_reg();
                                let offset_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(offset_reg, seed_data_offset));
                                self.emit(IrInstruction::Add(data_ptr, seed_data_base, offset_reg));
                                self.emit(IrInstruction::Store(data_ptr, val_reg, 0));

                                // Write SolBytes
                                let sol_bytes_ptr = self.alloc_reg();
                                let sb_offset = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(sb_offset, sol_bytes_offset));
                                self.emit(IrInstruction::Add(
                                    sol_bytes_ptr,
                                    seeds_array_ptr,
                                    sb_offset,
                                ));
                                self.emit(IrInstruction::Store(sol_bytes_ptr, data_ptr, 0));
                                let eight = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(eight, 8));
                                self.emit(IrInstruction::Store(sol_bytes_ptr, eight, 8));

                                seed_data_offset += 8;
                            }
                            _ => {
                                // Dynamic value: evaluate and store as 8-byte value
                                let val_reg = self
                                    .generate_expr(&arg.value)?
                                    .ok_or_else(|| Error::runtime("find-pda seed has no result"))?;

                                let data_ptr = self.alloc_reg();
                                let offset_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(offset_reg, seed_data_offset));
                                self.emit(IrInstruction::Add(data_ptr, seed_data_base, offset_reg));
                                self.emit(IrInstruction::Store(data_ptr, val_reg, 0));

                                let sol_bytes_ptr = self.alloc_reg();
                                let sb_offset = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(sb_offset, sol_bytes_offset));
                                self.emit(IrInstruction::Add(
                                    sol_bytes_ptr,
                                    seeds_array_ptr,
                                    sb_offset,
                                ));
                                self.emit(IrInstruction::Store(sol_bytes_ptr, data_ptr, 0));
                                let eight = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(eight, 8));
                                self.emit(IrInstruction::Store(sol_bytes_ptr, eight, 8));

                                seed_data_offset += 8;
                            }
                        }
                    }

                    // Output address at +768
                    let address_out = self.alloc_reg();
                    let offset_768 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(offset_768, 768));
                    self.emit(IrInstruction::Add(address_out, heap_base, offset_768));

                    // Bump seed at +800
                    let bump_out = self.alloc_reg();
                    let offset_800 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(offset_800, 800));
                    self.emit(IrInstruction::Add(bump_out, heap_base, offset_800));

                    // Number of seeds
                    let num_seeds_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_seeds_reg, num_seeds as i64));

                    // Call sol_try_find_program_address
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(result),
                        "sol_try_find_program_address".to_string(),
                        vec![
                            seeds_array_ptr,
                            num_seeds_reg,
                            program_id_ptr,
                            address_out,
                            bump_out,
                        ],
                    ));

                    // Return pointer to derived address
                    // Caller can use (mem-load address_out 0..24) to read the 32 bytes
                    // And (mem-load1 bump_out 0) to get the bump seed
                    return Ok(Some(address_out));
                }

                // Handle (get-pda-bump) - get the bump seed from last find-pda call
                // Returns the bump seed stored at heap offset 800
                if name == "get-pda-bump" && args.is_empty() {
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000300_i64));
                    let bump_ptr = self.alloc_reg();
                    let bump_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(bump_offset, 800));
                    self.emit(IrInstruction::Add(bump_ptr, heap_base, bump_offset));

                    // Load bump byte (as u64, masked to u8)
                    let raw = self.alloc_reg();
                    self.emit(IrInstruction::Load(raw, bump_ptr, 0));
                    let mask = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(mask, 0xFF));
                    let bump = self.alloc_reg();
                    self.emit(IrInstruction::And(bump, raw, mask));

                    return Ok(Some(bump));
                }

                // =================================================================
                // LOGICAL OPERATORS AS TOOL CALLS
                // =================================================================
                // Handle (and expr1 expr2) - logical AND
                if name == "and" && args.len() == 2 {
                    let left_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("and left operand has no result"))?;
                    let right_reg = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("and right operand has no result"))?;
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::And(dst, left_reg, right_reg));
                    return Ok(Some(dst));
                }

                // Handle (or expr1 expr2) - logical OR
                if name == "or" && args.len() == 2 {
                    let left_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("or left operand has no result"))?;
                    let right_reg = self
                        .generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("or right operand has no result"))?;
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Or(dst, left_reg, right_reg));
                    return Ok(Some(dst));
                }

                // Handle (not expr) - logical NOT
                if name == "not" && args.len() == 1 {
                    let operand_reg = self
                        .generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("not operand has no result"))?;
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Not(dst, operand_reg));
                    return Ok(Some(dst));
                }

                // Generic tool call
                let mut arg_regs = Vec::new();
                for arg in args {
                    if let Some(reg) = self.generate_expr(&arg.value)? {
                        arg_regs.push(reg);
                    }
                }
                let dst = self.alloc_reg();
                self.emit(IrInstruction::Call(Some(dst), name.clone(), arg_regs));
                Ok(Some(dst))
            }

            Expression::Grouping(inner) => self.generate_expr(inner),

            Expression::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                let cond_reg = self
                    .generate_expr(condition)?
                    .ok_or_else(|| Error::runtime("Ternary condition has no result"))?;

                let then_label = self.new_label("tern_then");
                let else_label = self.new_label("tern_else");
                let end_label = self.new_label("tern_end");
                let result_reg = self.alloc_reg();

                self.emit(IrInstruction::JumpIf(cond_reg, then_label.clone()));
                self.emit(IrInstruction::Jump(else_label.clone()));

                // Then - if branch returns None, use 0 (null) as default value
                self.emit(IrInstruction::Label(then_label));
                let then_result = self.generate_expr(then_expr)?;
                if let Some(then_reg) = then_result {
                    self.emit(IrInstruction::Move(result_reg, then_reg));
                } else {
                    // Side-effect only branch - use null (0) as value
                    self.emit(IrInstruction::ConstNull(result_reg));
                }
                self.emit(IrInstruction::Jump(end_label.clone()));

                // Else - if branch returns None, use 0 (null) as default value
                self.emit(IrInstruction::Label(else_label));
                let else_result = self.generate_expr(else_expr)?;
                if let Some(else_reg) = else_result {
                    self.emit(IrInstruction::Move(result_reg, else_reg));
                } else {
                    // Side-effect only branch - use null (0) as value
                    self.emit(IrInstruction::ConstNull(result_reg));
                }

                // End
                self.emit(IrInstruction::Label(end_label));
                Ok(Some(result_reg))
            }

            // =========================================================================
            // TYPE ANNOTATION EXPRESSIONS
            // =========================================================================

            // Handle (: expr type) - type annotation expression
            // Generate code for the inner expression and record the type from annotation
            Expression::TypeAnnotation { expr, type_expr } => {
                // First, generate the inner expression
                let result = self.generate_expr(expr)?;

                // Parse the type annotation to get a source-level Type
                if let Some(reg) = result {
                    let source_type = self.parse_type_expression(type_expr);
                    if let Some(ty) = source_type {
                        // Set the register's type from the source annotation
                        self.set_reg_from_source(reg, &ty);
                    }
                }

                Ok(result)
            }

            // Handle {x : T | predicate} - refinement type expression
            // These are type-level expressions, not value-level, so return the base type
            Expression::RefinedTypeExpr {
                var: _,
                base_type,
                predicate: _,
            } => {
                // In an expression context, a refinement type is treated as its base type.
                // The predicate is for verification purposes only.
                // Generate a type reference to the base type (this is unusual - typically
                // refinement types appear in type annotations, not as standalone expressions)
                self.generate_expr(base_type)
            }

            // Handle typed lambda: (lambda ((x : T) (y : U)) -> R body)
            // For now, lambdas/closures are not fully supported in sBPF codegen.
            // We parse and record the type annotations but can't return a first-class function.
            Expression::TypedLambda {
                typed_params,
                return_type: _,
                body: _,
            } => {
                // Just record the parameter types in the source type context for future use
                for (param_name, type_annotation) in typed_params.iter() {
                    if let Some(type_expr) = type_annotation {
                        if let Some(source_ty) = self.parse_type_expression(type_expr) {
                            // Record in source context for bidirectional typing
                            self.source_type_ctx.define_var(param_name, source_ty);
                        }
                    }
                }
                // Lambdas in expression context return null for now
                // Full first-class functions require heap allocation in sBPF
                let reg = self.alloc_reg();
                self.emit(IrInstruction::ConstNull(reg));
                Ok(Some(reg))
            }

            _ => Ok(None),
        }
    }

    /// Parse a type expression (AST) into a source-level Type.
    ///
    /// This converts parsed type annotations like `u64`, `{x : u64 | x < 10}`,
    /// or `(-> i64 bool)` into our Type enum for use with the TypeBridge.
    fn parse_type_expression(&self, type_expr: &Expression) -> Option<Type> {
        match type_expr {
            // Simple type names
            Expression::Variable(name) => Type::from_name(name),

            // Array type: (Array T) or (Array T N)
            Expression::ToolCall { name, args } if name == "Array" => {
                if args.is_empty() {
                    return None;
                }
                let elem_type = self.parse_type_expression(&args[0].value)?;
                let size = if args.len() > 1 {
                    if let Expression::IntLiteral(n) = &args[1].value {
                        *n as usize
                    } else {
                        0
                    }
                } else {
                    0
                };
                Some(Type::Array {
                    element: Box::new(elem_type),
                    size,
                })
            }

            // Pointer type: (Ptr T)
            Expression::ToolCall { name, args } if name == "Ptr" && args.len() == 1 => {
                let inner = self.parse_type_expression(&args[0].value)?;
                Some(Type::Ptr(Box::new(inner)))
            }

            // Reference type: (Ref T)
            Expression::ToolCall { name, args } if name == "Ref" && args.len() == 1 => {
                let inner = self.parse_type_expression(&args[0].value)?;
                Some(Type::Ref(Box::new(inner)))
            }

            // Function type: (-> T1 T2 ... R)
            Expression::ToolCall { name, args } if name == "->" && !args.is_empty() => {
                let mut param_types = Vec::new();
                for arg in args.iter().take(args.len() - 1) {
                    if let Some(ty) = self.parse_type_expression(&arg.value) {
                        param_types.push(ty);
                    }
                }
                let ret_type = self.parse_type_expression(&args.last().unwrap().value)?;
                Some(Type::Fn {
                    params: param_types,
                    ret: Box::new(ret_type),
                })
            }

            // Refinement type: {x : T | predicate}
            Expression::RefinedTypeExpr {
                var,
                base_type,
                predicate,
            } => {
                let base = self.parse_type_expression(base_type)?;
                let refined = crate::types::RefinementType::from_expr(var.clone(), base, predicate);
                Some(Type::Refined(Box::new(refined)))
            }

            _ => None,
        }
    }

    fn alloc_reg(&mut self) -> IrReg {
        let reg = IrReg(self.next_reg);
        self.next_reg += 1;
        reg
    }

    /// Allocate a register and record its type as a value
    fn alloc_value_reg(&mut self, size: i64, signed: bool) -> TypedReg {
        let reg = self.alloc_reg();
        let ty = RegType::Value { size, signed };
        self.type_env.set_type(reg, ty.clone());
        TypedReg { reg, ty }
    }

    /// Allocate a register for an unsigned 64-bit value
    fn alloc_u64_reg(&mut self) -> TypedReg {
        self.alloc_value_reg(8, false)
    }

    /// Allocate a register for a boolean value
    fn alloc_bool_reg(&mut self) -> TypedReg {
        let reg = self.alloc_reg();
        let ty = RegType::Bool;
        self.type_env.set_type(reg, ty.clone());
        TypedReg { reg, ty }
    }

    /// Allocate a register and record it as a pointer with given type info
    fn alloc_pointer_reg(&mut self, ptr_type: PointerType) -> TypedReg {
        let reg = self.alloc_reg();
        let ty = RegType::Pointer(ptr_type);
        self.type_env.set_type(reg, ty.clone());
        TypedReg { reg, ty }
    }

    /// Allocate a register for a pointer to account data
    fn alloc_account_data_ptr(&mut self, account_idx: u8, struct_name: Option<String>) -> TypedReg {
        self.alloc_pointer_reg(PointerType::account_data(account_idx, struct_name, None))
    }

    /// Allocate a register for a pointer to an account field (is_signer, lamports, etc.)
    fn alloc_account_field_ptr(
        &mut self,
        account_idx: u8,
        field_offset: i64,
        field_size: i64,
    ) -> TypedReg {
        self.alloc_pointer_reg(PointerType::account_field(
            account_idx,
            field_offset,
            field_size,
        ))
    }

    /// Record that a register holds an unknown type (from external sources)
    fn set_reg_unknown(&mut self, reg: IrReg) {
        self.type_env.set_type(reg, RegType::Unknown);
    }

    // =========================================================================
    // SOURCE TYPE BRIDGE METHODS
    // =========================================================================

    /// Allocate a register and set its type from a source-level Type annotation.
    ///
    /// This bridges the type system gap: source types (u64, {x : u64 | x < 10})
    /// are converted to IR types (Value { size: 8, signed: false }) via the
    /// TypeBridge, enabling full provenance tracking.
    ///
    /// For refinement types, the base type is used for code generation while
    /// the predicate is tracked for verification.
    fn alloc_typed_reg(&mut self, source_type: &Type) -> TypedReg {
        let reg = self.alloc_reg();
        let ir_type = self
            .type_bridge
            .source_to_ir(source_type, &self.source_type_ctx);
        self.type_env.set_type(reg, ir_type.clone());
        TypedReg { reg, ty: ir_type }
    }

    /// Set a register's type from a source-level Type annotation.
    ///
    /// Use this when the register is already allocated but you want to
    /// record its type based on source annotations.
    fn set_reg_from_source(&mut self, reg: IrReg, source_type: &Type) {
        let ir_type = self
            .type_bridge
            .source_to_ir(source_type, &self.source_type_ctx);
        self.type_env.set_type(reg, ir_type);
    }

    /// Convert a source-level Type to an IR-level RegType.
    ///
    /// This is a convenience wrapper around the TypeBridge.
    fn source_to_ir_type(&self, source_type: &Type) -> RegType {
        self.type_bridge
            .source_to_ir(source_type, &self.source_type_ctx)
    }

    /// Allocate a pointer register with account data provenance from source type.
    ///
    /// This combines source type information with account data region tracking
    /// for full memory safety verification.
    fn alloc_account_typed_ptr(&mut self, source_type: &Type, account_idx: u8) -> TypedReg {
        let reg = self.alloc_reg();
        let ir_type = self.type_bridge.source_to_account_ptr(
            source_type,
            &self.source_type_ctx,
            account_idx,
            None, // Data length unknown at compile time
        );
        self.type_env.set_type(reg, ir_type.clone());
        TypedReg { reg, ty: ir_type }
    }

    /// Get type environment errors accumulated during codegen
    pub fn type_errors(&self) -> &[super::memory_model::MemoryError] {
        self.type_env.errors()
    }

    /// Check if type environment has errors
    pub fn has_type_errors(&self) -> bool {
        self.type_env.has_errors()
    }

    fn new_label(&mut self, prefix: &str) -> String {
        let label = format!("{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    /// Emit an IR instruction with optional type validation.
    ///
    /// For memory operations (Load/Store), this validates:
    /// - The base register is a known pointer (if tracked)
    /// - The access size matches alignment requirements
    /// - Write operations target writable memory
    ///
    /// Validation errors are accumulated in type_env for later reporting.
    fn emit(&mut self, instr: IrInstruction) {
        // Validate and track types for memory operations
        match &instr {
            // Track types for constant loads
            IrInstruction::ConstI64(dst, _) => {
                self.type_env.set_type(
                    *dst,
                    RegType::Value {
                        size: 8,
                        signed: true,
                    },
                );
            }
            IrInstruction::ConstBool(dst, _) => {
                self.type_env.set_type(*dst, RegType::Bool);
            }

            // Validate and track Load operations
            IrInstruction::Load(dst, base, offset) => {
                if let Err(e) = self.type_env.validate_load(*base, *offset, 8) {
                    self.type_env.record_error(e);
                }
                // Result is a value (could be pointer, but we don't know without more context)
                self.type_env.set_type(
                    *dst,
                    RegType::Value {
                        size: 8,
                        signed: false,
                    },
                );
            }
            IrInstruction::Load4(dst, base, offset) => {
                if let Err(e) = self.type_env.validate_load(*base, *offset, 4) {
                    self.type_env.record_error(e);
                }
                self.type_env.set_type(
                    *dst,
                    RegType::Value {
                        size: 4,
                        signed: false,
                    },
                );
            }
            IrInstruction::Load2(dst, base, offset) => {
                if let Err(e) = self.type_env.validate_load(*base, *offset, 2) {
                    self.type_env.record_error(e);
                }
                self.type_env.set_type(
                    *dst,
                    RegType::Value {
                        size: 2,
                        signed: false,
                    },
                );
            }
            IrInstruction::Load1(dst, base, offset) => {
                if let Err(e) = self.type_env.validate_load(*base, *offset, 1) {
                    self.type_env.record_error(e);
                }
                self.type_env.set_type(
                    *dst,
                    RegType::Value {
                        size: 1,
                        signed: false,
                    },
                );
            }

            // Validate Store operations (also check writability)
            IrInstruction::Store(base, _src, offset) => {
                if let Err(e) = self.type_env.validate_store(*base, *offset, 8) {
                    self.type_env.record_error(e);
                }
            }
            IrInstruction::Store4(base, _src, offset) => {
                if let Err(e) = self.type_env.validate_store(*base, *offset, 4) {
                    self.type_env.record_error(e);
                }
            }
            IrInstruction::Store2(base, _src, offset) => {
                if let Err(e) = self.type_env.validate_store(*base, *offset, 2) {
                    self.type_env.record_error(e);
                }
            }
            IrInstruction::Store1(base, _src, offset) => {
                if let Err(e) = self.type_env.validate_store(*base, *offset, 1) {
                    self.type_env.record_error(e);
                }
            }

            // Track pointer arithmetic - propagate pointer type with offset
            IrInstruction::Add(dst, lhs, rhs) => {
                // If either operand is a pointer, result is a pointer (offset by value)
                let lhs_type = self.type_env.get_type(*lhs).cloned();
                let rhs_type = self.type_env.get_type(*rhs).cloned();

                match (lhs_type, rhs_type) {
                    (Some(RegType::Pointer(ptr)), Some(RegType::Value { .. })) => {
                        // Pointer + value = pointer (offset unknown statically)
                        let mut new_ptr = ptr.clone();
                        new_ptr.bounds = None; // Lost precision due to dynamic offset
                        self.type_env.set_type(*dst, RegType::Pointer(new_ptr));
                    }
                    (Some(RegType::Value { .. }), Some(RegType::Pointer(ptr))) => {
                        // Value + pointer = pointer
                        let mut new_ptr = ptr.clone();
                        new_ptr.bounds = None;
                        self.type_env.set_type(*dst, RegType::Pointer(new_ptr));
                    }
                    (
                        Some(RegType::Value {
                            size: s1,
                            signed: sg1,
                        }),
                        Some(RegType::Value {
                            size: s2,
                            signed: sg2,
                        }),
                    ) => {
                        // Value + value = value (max size)
                        self.type_env.set_type(
                            *dst,
                            RegType::Value {
                                size: s1.max(s2),
                                signed: sg1 || sg2,
                            },
                        );
                    }
                    _ => {
                        // Unknown operand types, result is unknown
                        self.type_env.set_type(*dst, RegType::Unknown);
                    }
                }
            }

            // Multiplication always produces a value
            IrInstruction::Mul(dst, _, _)
            | IrInstruction::Div(dst, _, _)
            | IrInstruction::Mod(dst, _, _)
            | IrInstruction::Sub(dst, _, _) => {
                self.type_env.set_type(
                    *dst,
                    RegType::Value {
                        size: 8,
                        signed: false,
                    },
                );
            }

            // Comparisons produce booleans
            IrInstruction::Eq(dst, _, _)
            | IrInstruction::Ne(dst, _, _)
            | IrInstruction::Lt(dst, _, _)
            | IrInstruction::Le(dst, _, _)
            | IrInstruction::Gt(dst, _, _)
            | IrInstruction::Ge(dst, _, _) => {
                self.type_env.set_type(*dst, RegType::Bool);
            }

            // Logical operations on values
            IrInstruction::And(dst, _, _) | IrInstruction::Or(dst, _, _) => {
                self.type_env.set_type(
                    *dst,
                    RegType::Value {
                        size: 8,
                        signed: false,
                    },
                );
            }
            IrInstruction::Not(dst, _) => {
                self.type_env.set_type(*dst, RegType::Bool);
            }

            // Move preserves type
            IrInstruction::Move(dst, src) => {
                if let Some(ty) = self.type_env.get_type(*src).cloned() {
                    self.type_env.set_type(*dst, ty);
                }
            }

            // Other instructions don't affect type tracking
            _ => {}
        }

        self.instructions.push(instr);
    }

    /// Emit an instruction without type validation (for bootstrap/internal use)
    fn emit_unchecked(&mut self, instr: IrInstruction) {
        self.instructions.push(instr);
    }

    /// Emit runtime bounds check for memory access.
    ///
    /// Generates sBPF code that aborts with error code 0x05 (MemoryAccessViolation)
    /// if the access would be out of bounds.
    ///
    /// Parameters:
    /// - `offset_reg`: Register containing the offset from data start
    /// - `access_size`: Size of the memory access in bytes
    /// - `max_len_reg`: Register containing the maximum valid offset (data_len)
    ///
    /// The check is: `if (offset + access_size > max_len) abort(5);`
    #[allow(dead_code)]
    fn emit_runtime_bounds_check(
        &mut self,
        offset_reg: IrReg,
        access_size: i64,
        max_len_reg: IrReg,
    ) {
        // Calculate end of access: offset + access_size
        let access_size_reg = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(access_size_reg, access_size));

        let access_end = self.alloc_reg();
        self.emit(IrInstruction::Add(access_end, offset_reg, access_size_reg));

        // Compare: access_end > max_len
        let out_of_bounds = self.alloc_reg();
        self.emit(IrInstruction::Gt(out_of_bounds, access_end, max_len_reg));

        // If out of bounds, abort with error code 5 (MemoryAccessViolation)
        let ok_label = self.new_label("bounds_ok");
        self.emit(IrInstruction::JumpIfNot(out_of_bounds, ok_label.clone()));

        // Abort: sol_panic_ or return error
        // For Solana, we use syscall abort
        let error_code = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(error_code, 0x05)); // MemoryAccessViolation
        self.emit(IrInstruction::Syscall(None, "abort".to_string(), vec![]));

        self.emit(IrInstruction::Label(ok_label));
    }

    /// Emit runtime account index bounds check.
    ///
    /// Generates sBPF code that aborts if the account index is >= num_accounts.
    ///
    /// Parameters:
    /// - `idx_reg`: Register containing the account index
    /// - `num_accounts_reg`: Register containing the number of accounts
    #[allow(dead_code)]
    fn emit_runtime_account_index_check(&mut self, idx_reg: IrReg, num_accounts_reg: IrReg) {
        // Compare: idx >= num_accounts
        let invalid_idx = self.alloc_reg();
        self.emit(IrInstruction::Ge(invalid_idx, idx_reg, num_accounts_reg));

        // If invalid, abort
        let ok_label = self.new_label("idx_ok");
        self.emit(IrInstruction::JumpIfNot(invalid_idx, ok_label.clone()));

        // Abort with error code 6 (InvalidAccountIndex)
        let error_code = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(error_code, 0x06));
        self.emit(IrInstruction::Syscall(None, "abort".to_string(), vec![]));

        self.emit(IrInstruction::Label(ok_label));
    }

    /// Emit code to look up an account's base offset from the precomputed table.
    /// Returns a register containing the offset from input start to the account's first byte.
    fn emit_get_account_offset(&mut self, idx_reg: IrReg) -> IrReg {
        // Table is at heap base (0x300000000), each entry is 8 bytes
        let heap_base = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(heap_base, 0x300000000_i64));

        let eight = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(eight, 8));

        let table_offset = self.alloc_reg();
        self.emit(IrInstruction::Mul(table_offset, idx_reg, eight));

        let table_addr = self.alloc_reg();
        self.emit(IrInstruction::Add(table_addr, heap_base, table_offset));

        let account_offset = self.alloc_reg();
        self.emit(IrInstruction::Load(account_offset, table_addr, 0));

        account_offset
    }

    /// Emit code to get the instruction data offset.
    /// Instruction data starts right after the last account's data.
    /// We store this at index [num_accounts] in the offset table during init.
    fn emit_get_instruction_data_offset(&mut self) -> IrReg {
        let accounts_ptr = *self
            .var_map
            .get("accounts")
            .expect("accounts not available");

        // Read num_accounts from offset 0
        let num_accounts = self.alloc_reg();
        self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));

        // Look up the instruction data offset at index [num_accounts] in the table
        self.emit_get_account_offset(num_accounts)
    }

    /// Emit code to get a pointer to an account's pubkey (32 bytes).
    /// The pubkey is at offset +8 from the account base.
    fn emit_get_account_pubkey_ptr(&mut self, idx_reg: IrReg) -> IrReg {
        self.emit_get_account_field_ptr(idx_reg, 8)
    }

    /// Emit code to get a pointer to any field within an account.
    /// field_offset: offset from account base (0=dup, 1=is_signer, 2=is_writable, etc.)
    fn emit_get_account_field_ptr(&mut self, idx_reg: IrReg, field_offset: i64) -> IrReg {
        let accounts_ptr = *self
            .var_map
            .get("accounts")
            .expect("accounts not available for field ptr");

        // Get the base offset for this account
        let account_offset = self.emit_get_account_offset(idx_reg);

        // Add field offset
        let field_off_reg = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(field_off_reg, field_offset));

        let total_offset = self.alloc_reg();
        self.emit(IrInstruction::Add(
            total_offset,
            account_offset,
            field_off_reg,
        ));

        let field_ptr = self.alloc_reg();
        self.emit(IrInstruction::Add(field_ptr, accounts_ptr, total_offset));

        field_ptr
    }

    /// Emit code to build the account offset table at program start.
    /// This iterates through all accounts and stores their base offsets
    /// in a heap table for O(1) indexed access.
    ///
    /// The table is stored at heap address 0x300000000 (Solana heap start).
    /// Each entry is 8 bytes containing the offset from input start to account start.
    fn emit_account_offset_table_init(&mut self, accounts_ptr: IrReg) {
        // Constants
        let heap_base = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(heap_base, 0x300000000_i64));

        let eight = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(eight, 8));

        // Fixed header size: 1 (dup) + 1 (signer) + 1 (writable) + 1 (exec) + 4 (pad) +
        //                    32 (pubkey) + 32 (owner) + 8 (lamports) + 8 (data_len) = 88
        let header_size = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(header_size, 88));

        // Realloc padding = 10240 + 8 (rent_epoch) = 10248
        // Plus alignment to 8 bytes (handled in loop)
        let realloc_padding = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(realloc_padding, 10248));

        // Read num_accounts from offset 0
        let num_accounts = self.alloc_reg();
        self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));

        // Current offset starts at 8 (skip num_accounts header)
        let current_offset = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(current_offset, 8));

        // Loop counter
        let counter = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(counter, 0));

        // Loop labels
        let loop_label = self.new_label("acct_loop");
        let end_label = self.new_label("acct_loop_end");

        // Loop start
        self.emit(IrInstruction::Label(loop_label.clone()));

        // Check if counter >= num_accounts (use Ge for >=)
        let cmp_done = self.alloc_reg();
        self.emit(IrInstruction::Ge(cmp_done, counter, num_accounts));
        self.emit(IrInstruction::JumpIf(cmp_done, end_label.clone()));

        // Store current_offset in heap table at heap_base + (counter * 8)
        let table_offset = self.alloc_reg();
        self.emit(IrInstruction::Mul(table_offset, counter, eight));
        let table_addr = self.alloc_reg();
        self.emit(IrInstruction::Add(table_addr, heap_base, table_offset));
        self.emit(IrInstruction::Store(table_addr, current_offset, 0));

        // Read data_len at current_offset + 80 (offset to data_len within account)
        // data_len offset = 1 + 1 + 1 + 1 + 4 + 32 + 32 + 8 = 80
        let data_len_offset_const = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(data_len_offset_const, 80));
        let data_len_addr = self.alloc_reg();
        self.emit(IrInstruction::Add(
            data_len_addr,
            accounts_ptr,
            current_offset,
        ));
        self.emit(IrInstruction::Add(
            data_len_addr,
            data_len_addr,
            data_len_offset_const,
        ));
        let data_len = self.alloc_reg();
        self.emit(IrInstruction::Load(data_len, data_len_addr, 0));

        // Calculate next account offset:
        // next = current + header_size + data_len + realloc_padding
        // Then align to 8 bytes: next = (next + 7) & ~7
        let next_offset = self.alloc_reg();
        self.emit(IrInstruction::Add(next_offset, current_offset, header_size));
        self.emit(IrInstruction::Add(next_offset, next_offset, data_len));
        self.emit(IrInstruction::Add(
            next_offset,
            next_offset,
            realloc_padding,
        ));

        // Align to 8 bytes: (next + 7) & ~7
        let seven = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(seven, 7));
        self.emit(IrInstruction::Add(next_offset, next_offset, seven));
        let mask = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(mask, !7_i64));
        self.emit(IrInstruction::And(next_offset, next_offset, mask));

        // Update current_offset for next iteration
        self.emit(IrInstruction::Move(current_offset, next_offset));

        // Increment counter
        let one = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(one, 1));
        self.emit(IrInstruction::Add(counter, counter, one));

        // Jump back to loop start
        self.emit(IrInstruction::Jump(loop_label));

        // End label
        self.emit(IrInstruction::Label(end_label));

        // IMPORTANT: Store the instruction data offset at table[num_accounts]
        // This is the offset right after all accounts, where instruction data begins
        // We use current_offset which now points to where the next account would be
        let final_table_offset = self.alloc_reg();
        self.emit(IrInstruction::Mul(final_table_offset, num_accounts, eight));
        let final_table_addr = self.alloc_reg();
        self.emit(IrInstruction::Add(
            final_table_addr,
            heap_base,
            final_table_offset,
        ));
        self.emit(IrInstruction::Store(final_table_addr, current_offset, 0));

        // Store the number of accounts in a reserved variable for later use
        self.var_map
            .insert("__num_accounts".to_string(), num_accounts);
        self.var_map
            .insert("__acct_table_base".to_string(), heap_base);
    }
}

impl Default for IrGenerator {
    fn default() -> Self {
        Self::new()
    }
}
