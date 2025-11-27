//! # Intermediate Representation (IR) for OVSM Compilation
//!
//! Three-address code IR that bridges the gap between OVSM AST
//! and sBPF bytecode. This representation makes optimization
//! and register allocation tractable.

use std::collections::HashMap;
use crate::{Result, Error};
use super::types::{TypedProgram, TypedStatement, OvsmType};
use crate::{Statement, Expression, BinaryOp, UnaryOp};

// =============================================================================
// STRUCT TYPES (compile-time layout)
// =============================================================================

/// Primitive field types (fixed-size scalars)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    U8,   // 1 byte
    U16,  // 2 bytes
    U32,  // 4 bytes
    U64,  // 8 bytes (default for untyped)
    I8,   // 1 byte signed
    I16,  // 2 bytes signed
    I32,  // 4 bytes signed
    I64,  // 8 bytes signed (default)
}

impl PrimitiveType {
    pub fn size(&self) -> i64 {
        match self {
            PrimitiveType::U8 | PrimitiveType::I8 => 1,
            PrimitiveType::U16 | PrimitiveType::I16 => 2,
            PrimitiveType::U32 | PrimitiveType::I32 => 4,
            PrimitiveType::U64 | PrimitiveType::I64 => 8,
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "u8" => Some(PrimitiveType::U8),
            "u16" => Some(PrimitiveType::U16),
            "u32" => Some(PrimitiveType::U32),
            "u64" => Some(PrimitiveType::U64),
            "i8" => Some(PrimitiveType::I8),
            "i16" => Some(PrimitiveType::I16),
            "i32" => Some(PrimitiveType::I32),
            "i64" => Some(PrimitiveType::I64),
            _ => None,
        }
    }

    pub fn to_idl_type(&self) -> &'static str {
        match self {
            PrimitiveType::U8 => "u8",
            PrimitiveType::U16 => "u16",
            PrimitiveType::U32 => "u32",
            PrimitiveType::U64 => "u64",
            PrimitiveType::I8 => "i8",
            PrimitiveType::I16 => "i16",
            PrimitiveType::I32 => "i32",
            PrimitiveType::I64 => "i64",
        }
    }
}

/// Extended field type supporting primitives, arrays, pubkeys, and nested structs
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldType {
    /// Primitive integer types (u8-u64, i8-i64)
    Primitive(PrimitiveType),
    /// Fixed-size array: [element_type count], e.g., [u32 10] = 40 bytes
    Array { element_type: PrimitiveType, count: usize },
    /// Solana public key (32 bytes, special handling)
    Pubkey,
    /// Nested struct reference (resolved at struct definition time)
    Struct(String),
}

impl FieldType {
    /// Get size in bytes (for Array and Struct, needs struct_defs for resolution)
    pub fn size(&self) -> i64 {
        match self {
            FieldType::Primitive(p) => p.size(),
            FieldType::Array { element_type, count } => element_type.size() * (*count as i64),
            FieldType::Pubkey => 32, // Solana pubkey is always 32 bytes
            FieldType::Struct(_) => 0, // Requires struct_defs lookup - use size_with_structs
        }
    }

    /// Get size with struct definitions for nested struct resolution
    pub fn size_with_structs(&self, struct_defs: &HashMap<String, StructDef>) -> i64 {
        match self {
            FieldType::Struct(name) => {
                struct_defs.get(name).map(|s| s.total_size).unwrap_or(0)
            }
            _ => self.size(),
        }
    }

    /// Parse from type string (simple types only - arrays/structs handled separately)
    pub fn from_str(s: &str) -> Option<Self> {
        if s == "pubkey" {
            return Some(FieldType::Pubkey);
        }
        PrimitiveType::from_str(s).map(FieldType::Primitive)
    }

    /// Convert to Anchor IDL type string
    pub fn to_idl_type(&self) -> String {
        match self {
            FieldType::Primitive(p) => p.to_idl_type().to_string(),
            FieldType::Array { element_type, count } => {
                format!("{{ \"array\": [\"{}\", {}] }}", element_type.to_idl_type(), count)
            }
            FieldType::Pubkey => "publicKey".to_string(),
            FieldType::Struct(name) => format!("{{ \"defined\": \"{}\" }}", name),
        }
    }

    /// Check if this is a primitive type for load/store instruction selection
    pub fn primitive(&self) -> Option<PrimitiveType> {
        match self {
            FieldType::Primitive(p) => Some(*p),
            _ => None,
        }
    }
}

/// A field in a struct definition
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub field_type: FieldType,
    pub offset: i64,
    /// For array types, the element size
    pub element_size: Option<i64>,
    /// For array types, the element count
    pub array_count: Option<usize>,
}

/// A struct definition (compile-time metadata)
#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructField>,
    pub total_size: i64,
}

impl StructDef {
    /// Generate Anchor IDL JSON for this struct
    /// This enables TypeScript clients to interact with OVSM programs
    pub fn to_anchor_idl(&self) -> String {
        let mut fields_json = Vec::new();
        for field in &self.fields {
            fields_json.push(format!(
                r#"        {{ "name": "{}", "type": "{}" }}"#,
                field.name,
                field.field_type.to_idl_type()
            ));
        }

        format!(
            r#"{{
  "name": "{}",
  "type": {{
    "kind": "struct",
    "fields": [
{}
    ]
  }}
}}"#,
            self.name,
            fields_json.join(",\n")
        )
    }
}

/// Virtual register (infinite supply, mapped to physical during codegen)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrReg(pub u32);

impl IrReg {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

/// IR instruction (three-address code)
#[derive(Debug, Clone)]
pub enum IrInstruction {
    // Constants
    /// Load 64-bit integer constant into register
    ConstI64(IrReg, i64),
    /// Load 64-bit float constant (as bits)
    ConstF64(IrReg, u64),
    /// Load boolean constant
    ConstBool(IrReg, bool),
    /// Load null
    ConstNull(IrReg),
    /// Load string literal (index into string table)
    ConstString(IrReg, usize),

    // Arithmetic (dst = src1 op src2)
    Add(IrReg, IrReg, IrReg),
    Sub(IrReg, IrReg, IrReg),
    Mul(IrReg, IrReg, IrReg),
    Div(IrReg, IrReg, IrReg),
    Mod(IrReg, IrReg, IrReg),

    // Comparison (dst = src1 op src2, result is 0 or 1)
    Eq(IrReg, IrReg, IrReg),
    Ne(IrReg, IrReg, IrReg),
    Lt(IrReg, IrReg, IrReg),
    Le(IrReg, IrReg, IrReg),
    Gt(IrReg, IrReg, IrReg),
    Ge(IrReg, IrReg, IrReg),

    // Logical
    And(IrReg, IrReg, IrReg),
    Or(IrReg, IrReg, IrReg),
    Not(IrReg, IrReg),

    // Unary
    Neg(IrReg, IrReg),

    // Register operations
    Move(IrReg, IrReg),

    // Control flow
    Label(String),
    Jump(String),
    /// Jump if register is non-zero
    JumpIf(IrReg, String),
    /// Jump if register is zero
    JumpIfNot(IrReg, String),

    // Function calls
    /// Call function, store result in optional dst
    Call(Option<IrReg>, String, Vec<IrReg>),
    /// Return with optional value
    Return(Option<IrReg>),

    // Memory operations
    /// Load from memory: dst = *(base + offset) (64-bit)
    Load(IrReg, IrReg, i64),
    /// Load 1 byte (8-bit) from memory: dst = (u8)*(base + offset)
    Load1(IrReg, IrReg, i64),
    /// Load 2 bytes (16-bit) from memory: dst = (u16)*(base + offset)
    Load2(IrReg, IrReg, i64),
    /// Load 4 bytes (32-bit) from memory: dst = (u32)*(base + offset)
    Load4(IrReg, IrReg, i64),
    /// Store to memory: *(base + offset) = src (64-bit)
    Store(IrReg, IrReg, i64),
    /// Store 1 byte to memory: *(base + offset) = (u8)src
    Store1(IrReg, IrReg, i64),
    /// Store 2 bytes (16-bit) to memory: *(base + offset) = (u16)src
    Store2(IrReg, IrReg, i64),
    /// Store 4 bytes (32-bit) to memory: *(base + offset) = (u32)src
    Store4(IrReg, IrReg, i64),
    /// Allocate heap memory: dst = alloc(size)
    Alloc(IrReg, IrReg),

    // Syscalls (Solana-specific)
    /// dst = syscall(name, args...)
    Syscall(Option<IrReg>, String, Vec<IrReg>),

    // Debug
    /// Debug log (will be sol_log syscall): Log(ptr_reg, length)
    Log(IrReg, usize),

    // No-op (placeholder, removed by optimizer)
    Nop,
}

/// Basic block in the control flow graph
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<IrInstruction>,
    pub successors: Vec<String>,
    pub predecessors: Vec<String>,
}

impl BasicBlock {
    pub fn new(label: &str) -> Self {
        Self {
            label: label.to_string(),
            instructions: Vec::new(),
            successors: Vec::new(),
            predecessors: Vec::new(),
        }
    }
}

/// Complete IR program
#[derive(Debug, Clone)]
pub struct IrProgram {
    /// All instructions in linear order
    pub instructions: Vec<IrInstruction>,
    /// Basic blocks for CFG analysis
    pub blocks: HashMap<String, BasicBlock>,
    /// String table for string literals
    pub string_table: Vec<String>,
    /// Entry point label
    pub entry_label: String,
    /// Variable to register mapping
    pub var_registers: HashMap<String, IrReg>,
}

impl IrProgram {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            blocks: HashMap::new(),
            string_table: Vec::new(),
            entry_label: "entry".to_string(),
            var_registers: HashMap::new(),
        }
    }
}

impl Default for IrProgram {
    fn default() -> Self {
        Self::new()
    }
}

/// IR Generator - transforms typed AST to IR
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
}

impl IrGenerator {
    pub fn new() -> Self {
        let mut gen = Self {
            next_reg: 0,
            label_counter: 0,
            var_map: HashMap::new(),
            strings: Vec::new(),
            instructions: Vec::new(),
            struct_defs: HashMap::new(),
        };
        // Pre-allocate registers for Solana builtins (R1=accounts, R2=instruction-data per ABI)
        let accounts_reg = IrReg::new(1);
        let instr_data_reg = IrReg::new(2);
        gen.var_map.insert("accounts".to_string(), accounts_reg);
        gen.var_map.insert("instruction-data".to_string(), instr_data_reg);
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
        self.var_map.insert("instruction-data".to_string(), saved_instr_data);

        // CRITICAL: Ensure next_reg skips past the reserved registers (6 and 7)
        // Otherwise alloc_reg() will return IrReg(6) or IrReg(7) for temporaries,
        // which will clobber the saved accounts/instruction-data pointers!
        if self.next_reg <= 7 {
            self.next_reg = 8;
        }

        eprintln!("🔍 IR DEBUG: Generating IR for {} statements", program.statements.len());

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
            Statement::Expression(expr) => {
                self.generate_expr(expr)
            }

            Statement::Assignment { name, value } => {
                let value_reg = self.generate_expr(value)?
                    .ok_or_else(|| Error::runtime("Assignment value has no result"))?;

                // Store in variable map
                self.var_map.insert(name.clone(), value_reg);
                Ok(Some(value_reg))
            }

            Statement::If { condition, then_branch, else_branch } => {
                let cond_reg = self.generate_expr(condition)?
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

                let cond_reg = self.generate_expr(condition)?
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

            Statement::For { variable, iterable, body } => {
                // Generate iterable
                let iter_reg = self.generate_expr(iterable)?
                    .ok_or_else(|| Error::runtime("For iterable has no result"))?;

                // Get length
                let len_reg = self.alloc_reg();
                self.emit(IrInstruction::Call(Some(len_reg), "length".to_string(), vec![iter_reg]));

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
                self.emit(IrInstruction::Call(Some(elem_reg), "get".to_string(), vec![iter_reg, idx_reg]));
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

            Expression::Variable(name) => {
                self.var_map.get(name)
                    .copied()
                    .map(Some)
                    .ok_or_else(|| Error::runtime(format!("Undefined variable: {}", name)))
            }

            Expression::Binary { op, left, right } => {
                let left_reg = self.generate_expr(left)?
                    .ok_or_else(|| Error::runtime("Binary left has no result"))?;
                let right_reg = self.generate_expr(right)?
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
                let operand_reg = self.generate_expr(operand)?
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
                        let value_reg = self.generate_expr(&args[1].value)?
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
                        let old_reg = self.var_map.get(var_name)
                            .copied()
                            .ok_or_else(|| Error::runtime(format!("Cannot set! undefined variable: {}", var_name)))?;

                        // Compute the new value
                        let value_reg = self.generate_expr(&args[1].value)?
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
                            if let Expression::ToolCall { name: field_name, args: field_args } = &field_arg.value {
                                if field_args.len() == 1 {
                                    // Parse the type specification
                                    let (field_type, elem_size, arr_count) = match &field_args[0].value {
                                        // Simple type: u8, u16, u32, u64, i8, i16, i32, i64, pubkey
                                        Expression::Variable(type_name) => {
                                            if let Some(ft) = FieldType::from_str(type_name) {
                                                (ft, None, None)
                                            } else {
                                                // Check if it's a reference to another struct
                                                if self.struct_defs.contains_key(type_name) {
                                                    (FieldType::Struct(type_name.clone()), None, None)
                                                } else {
                                                    return Err(Error::runtime(format!(
                                                        "Unknown field type '{}' in struct '{}'. Valid types: u8, u16, u32, u64, i8, i16, i32, i64, pubkey, [type count], or a defined struct name",
                                                        type_name, struct_name
                                                    )));
                                                }
                                            }
                                        }
                                        // Array type: [element_type count]
                                        Expression::ArrayLiteral(elements) if elements.len() == 2 => {
                                            if let (Expression::Variable(elem_type), Expression::IntLiteral(count)) =
                                                (&elements[0], &elements[1])
                                            {
                                                let primitive = PrimitiveType::from_str(elem_type)
                                                    .ok_or_else(|| Error::runtime(format!(
                                                        "Array element type '{}' must be a primitive (u8-u64, i8-i64) in struct '{}'",
                                                        elem_type, struct_name
                                                    )))?;
                                                let cnt = *count as usize;
                                                (
                                                    FieldType::Array { element_type: primitive, count: cnt },
                                                    Some(primitive.size()),
                                                    Some(cnt)
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

                        eprintln!("📦 Defined struct '{}' with {} bytes:", struct_name, current_offset);
                        for field in &struct_def.fields {
                            eprintln!("   +{}: {} ({:?})", field.offset, field.name, field.field_type);
                        }

                        self.struct_defs.insert(struct_name.clone(), struct_def);

                        // define-struct produces no runtime value
                        return Ok(None);
                    }
                }

                // Handle (struct-get StructName base_ptr field_name)
                // Example: (struct-get MyState state_ptr counter)
                // Generates the appropriate Load1/2/4/8 based on field type
                if name == "struct-get" && args.len() == 3 {
                    if let (Expression::Variable(struct_name), Expression::Variable(field_name)) =
                        (&args[0].value, &args[2].value)
                    {
                        let struct_def = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
                            .clone();

                        let field = struct_def.fields.iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| Error::runtime(format!(
                                "Unknown field '{}' in struct '{}'", field_name, struct_name
                            )))?;

                        let base_reg = self.generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("struct-get base_ptr has no result"))?;

                        let dst = self.alloc_reg();
                        let offset = field.offset;

                        // Emit the appropriate load instruction based on field type
                        match &field.field_type {
                            FieldType::Primitive(PrimitiveType::U8) | FieldType::Primitive(PrimitiveType::I8) => {
                                self.emit(IrInstruction::Load1(dst, base_reg, offset));
                            }
                            FieldType::Primitive(PrimitiveType::U16) | FieldType::Primitive(PrimitiveType::I16) => {
                                self.emit(IrInstruction::Load2(dst, base_reg, offset));
                            }
                            FieldType::Primitive(PrimitiveType::U32) | FieldType::Primitive(PrimitiveType::I32) => {
                                self.emit(IrInstruction::Load4(dst, base_reg, offset));
                            }
                            FieldType::Primitive(PrimitiveType::U64) | FieldType::Primitive(PrimitiveType::I64) => {
                                self.emit(IrInstruction::Load(dst, base_reg, offset));
                            }
                            FieldType::Pubkey | FieldType::Array { .. } | FieldType::Struct(_) => {
                                // For pubkey/array/struct, return pointer to field (not value)
                                // Use struct-ptr for these types instead
                                let offset_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(offset_reg, offset));
                                self.emit(IrInstruction::Add(dst, base_reg, offset_reg));
                            }
                        }

                        return Ok(Some(dst));
                    }
                }

                // Handle (struct-set StructName base_ptr field_name value)
                // Example: (struct-set MyState state_ptr counter 42)
                // Generates the appropriate Store1/2/4/8 based on field type
                if name == "struct-set" && args.len() == 4 {
                    if let (Expression::Variable(struct_name), Expression::Variable(field_name)) =
                        (&args[0].value, &args[2].value)
                    {
                        let struct_def = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
                            .clone();

                        let field = struct_def.fields.iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| Error::runtime(format!(
                                "Unknown field '{}' in struct '{}'", field_name, struct_name
                            )))?;

                        let base_reg = self.generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("struct-set base_ptr has no result"))?;

                        let value_reg = self.generate_expr(&args[3].value)?
                            .ok_or_else(|| Error::runtime("struct-set value has no result"))?;

                        let offset = field.offset;

                        // Emit the appropriate store instruction based on field type
                        match &field.field_type {
                            FieldType::Primitive(PrimitiveType::U8) | FieldType::Primitive(PrimitiveType::I8) => {
                                self.emit(IrInstruction::Store1(base_reg, value_reg, offset));
                            }
                            FieldType::Primitive(PrimitiveType::U16) | FieldType::Primitive(PrimitiveType::I16) => {
                                self.emit(IrInstruction::Store2(base_reg, value_reg, offset));
                            }
                            FieldType::Primitive(PrimitiveType::U32) | FieldType::Primitive(PrimitiveType::I32) => {
                                self.emit(IrInstruction::Store4(base_reg, value_reg, offset));
                            }
                            FieldType::Primitive(PrimitiveType::U64) | FieldType::Primitive(PrimitiveType::I64) => {
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
                        let total_size = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
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
                        let struct_def = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
                            .clone();

                        let field = struct_def.fields.iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| Error::runtime(format!(
                                "Unknown field '{}' in struct '{}'", field_name, struct_name
                            )))?;

                        let base_reg = self.generate_expr(&args[1].value)?
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
                        let struct_def = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
                            .clone();

                        let field = struct_def.fields.iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| Error::runtime(format!(
                                "Unknown field '{}' in struct '{}'", field_name, struct_name
                            )))?;

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
                        let struct_def = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
                            .clone();

                        let field = struct_def.fields.iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| Error::runtime(format!(
                                "Unknown field '{}' in struct '{}'", field_name, struct_name
                            )))?;

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
                        let struct_def = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
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
                        let struct_def = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
                            .clone();

                        let src_ptr = self.generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("borsh-serialize src_ptr has no result"))?;

                        let dst_buffer = self.generate_expr(&args[2].value)?
                            .ok_or_else(|| Error::runtime("borsh-serialize dst_buffer has no result"))?;

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
                                FieldType::Primitive(PrimitiveType::U8) | FieldType::Primitive(PrimitiveType::I8) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load1(temp_reg, src_ptr, field_offset));
                                    self.emit(IrInstruction::Store1(dst_buffer, temp_reg, dst_offset));
                                }
                                FieldType::Primitive(PrimitiveType::U16) | FieldType::Primitive(PrimitiveType::I16) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load2(temp_reg, src_ptr, field_offset));
                                    self.emit(IrInstruction::Store2(dst_buffer, temp_reg, dst_offset));
                                }
                                FieldType::Primitive(PrimitiveType::U32) | FieldType::Primitive(PrimitiveType::I32) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load4(temp_reg, src_ptr, field_offset));
                                    self.emit(IrInstruction::Store4(dst_buffer, temp_reg, dst_offset));
                                }
                                FieldType::Primitive(PrimitiveType::U64) | FieldType::Primitive(PrimitiveType::I64) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load(temp_reg, src_ptr, field_offset));
                                    self.emit(IrInstruction::Store(dst_buffer, temp_reg, dst_offset));
                                }
                                FieldType::Pubkey => {
                                    // Copy 32 bytes (4 x 8-byte loads/stores)
                                    for i in 0..4 {
                                        let temp_reg = self.alloc_reg();
                                        self.emit(IrInstruction::Load(temp_reg, src_ptr, field_offset + i * 8));
                                        self.emit(IrInstruction::Store(dst_buffer, temp_reg, dst_offset + i * 8));
                                    }
                                }
                                FieldType::Array { element_type, count } => {
                                    // Copy array elements
                                    let elem_size = element_type.size();
                                    for i in 0..(*count as i64) {
                                        let temp_reg = self.alloc_reg();
                                        let elem_offset = field_offset + i * elem_size;
                                        let dst_elem_offset = dst_offset + i * elem_size;
                                        match element_type {
                                            PrimitiveType::U8 | PrimitiveType::I8 => {
                                                self.emit(IrInstruction::Load1(temp_reg, src_ptr, elem_offset));
                                                self.emit(IrInstruction::Store1(dst_buffer, temp_reg, dst_elem_offset));
                                            }
                                            PrimitiveType::U16 | PrimitiveType::I16 => {
                                                self.emit(IrInstruction::Load2(temp_reg, src_ptr, elem_offset));
                                                self.emit(IrInstruction::Store2(dst_buffer, temp_reg, dst_elem_offset));
                                            }
                                            PrimitiveType::U32 | PrimitiveType::I32 => {
                                                self.emit(IrInstruction::Load4(temp_reg, src_ptr, elem_offset));
                                                self.emit(IrInstruction::Store4(dst_buffer, temp_reg, dst_elem_offset));
                                            }
                                            PrimitiveType::U64 | PrimitiveType::I64 => {
                                                self.emit(IrInstruction::Load(temp_reg, src_ptr, elem_offset));
                                                self.emit(IrInstruction::Store(dst_buffer, temp_reg, dst_elem_offset));
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
                        let struct_def = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
                            .clone();

                        let src_buffer = self.generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("borsh-deserialize src_buffer has no result"))?;

                        let dst_ptr = self.generate_expr(&args[2].value)?
                            .ok_or_else(|| Error::runtime("borsh-deserialize dst_ptr has no result"))?;

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
                                FieldType::Primitive(PrimitiveType::U8) | FieldType::Primitive(PrimitiveType::I8) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load1(temp_reg, src_buffer, src_offset));
                                    self.emit(IrInstruction::Store1(dst_ptr, temp_reg, field_offset));
                                }
                                FieldType::Primitive(PrimitiveType::U16) | FieldType::Primitive(PrimitiveType::I16) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load2(temp_reg, src_buffer, src_offset));
                                    self.emit(IrInstruction::Store2(dst_ptr, temp_reg, field_offset));
                                }
                                FieldType::Primitive(PrimitiveType::U32) | FieldType::Primitive(PrimitiveType::I32) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load4(temp_reg, src_buffer, src_offset));
                                    self.emit(IrInstruction::Store4(dst_ptr, temp_reg, field_offset));
                                }
                                FieldType::Primitive(PrimitiveType::U64) | FieldType::Primitive(PrimitiveType::I64) => {
                                    let temp_reg = self.alloc_reg();
                                    self.emit(IrInstruction::Load(temp_reg, src_buffer, src_offset));
                                    self.emit(IrInstruction::Store(dst_ptr, temp_reg, field_offset));
                                }
                                FieldType::Pubkey => {
                                    // Copy 32 bytes (4 x 8-byte loads/stores)
                                    for i in 0..4 {
                                        let temp_reg = self.alloc_reg();
                                        self.emit(IrInstruction::Load(temp_reg, src_buffer, src_offset + i * 8));
                                        self.emit(IrInstruction::Store(dst_ptr, temp_reg, field_offset + i * 8));
                                    }
                                }
                                FieldType::Array { element_type, count } => {
                                    // Copy array elements
                                    let elem_size = element_type.size();
                                    for i in 0..(*count as i64) {
                                        let temp_reg = self.alloc_reg();
                                        let src_elem_offset = src_offset + i * elem_size;
                                        let dst_elem_offset = field_offset + i * elem_size;
                                        match element_type {
                                            PrimitiveType::U8 | PrimitiveType::I8 => {
                                                self.emit(IrInstruction::Load1(temp_reg, src_buffer, src_elem_offset));
                                                self.emit(IrInstruction::Store1(dst_ptr, temp_reg, dst_elem_offset));
                                            }
                                            PrimitiveType::U16 | PrimitiveType::I16 => {
                                                self.emit(IrInstruction::Load2(temp_reg, src_buffer, src_elem_offset));
                                                self.emit(IrInstruction::Store2(dst_ptr, temp_reg, dst_elem_offset));
                                            }
                                            PrimitiveType::U32 | PrimitiveType::I32 => {
                                                self.emit(IrInstruction::Load4(temp_reg, src_buffer, src_elem_offset));
                                                self.emit(IrInstruction::Store4(dst_ptr, temp_reg, dst_elem_offset));
                                            }
                                            PrimitiveType::U64 | PrimitiveType::I64 => {
                                                self.emit(IrInstruction::Load(temp_reg, src_buffer, src_elem_offset));
                                                self.emit(IrInstruction::Store(dst_ptr, temp_reg, dst_elem_offset));
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
                        let total_size = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
                            .total_size;

                        let dst = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(dst, total_size));
                        return Ok(Some(dst));
                    }
                }

                // Handle (get array index) - array/object access
                if name == "get" && args.len() == 2 {
                    let base_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("Get base has no result"))?;
                    let idx_reg = self.generate_expr(&args[1].value)?
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
                    let base_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-load base has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => {
                            let off_reg = self.generate_expr(&args[1].value)?
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
                    let accounts_ptr = *self.var_map.get("accounts")
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
                if name == "account-lamports" && args.len() == 1 {
                    let idx_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-lamports index has no result"))?;

                    // Get accounts base pointer (saved to R6 at entry)
                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // For account 0: skip num_accounts (8 bytes) + offset within account (72 bytes)
                    // lamports offset from account start = 1+1+1+1+4+32+32 = 72
                    // Total for account 0: 8 + 72 = 80
                    //
                    // For other accounts, we'd need to scan forward through variable-length entries
                    // For now, approximate: each account has ~10334 bytes minimum (with MAX_PERMITTED_DATA_INCREASE)
                    // But this is wrong for accounts with non-zero data_len
                    //
                    // Better approach: For account 0, use hardcoded offset 80
                    // TODO: Implement proper iteration for account > 0

                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));

                    // For accounts with no data: 1+1+1+1+4+32+32+8+8+0+10240+padding(~8)+8 ≈ 10344
                    // Use a large approximate size for subsequent accounts
                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10344));

                    let account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(account_offset, idx_reg, account_size));

                    let base_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(base_offset, eight_reg, account_offset));

                    // Add lamports offset within account (72 bytes, not 80!)
                    let lamports_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(lamports_offset, 72));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, base_offset, lamports_offset));

                    let addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(addr, accounts_ptr, total_offset));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load(dst, addr, 0));
                    return Ok(Some(dst));
                }

                // Handle (set-lamports idx value) - set lamport balance for account
                // This is how SOL transfers work - directly modify lamports
                // IMPORTANT: Account must be writable and owned by your program (or system program)
                if name == "set-lamports" && args.len() == 2 {
                    let idx_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("set-lamports index has no result"))?;
                    let value_reg = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("set-lamports value has no result"))?;

                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10344));

                    let account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(account_offset, idx_reg, account_size));

                    let base_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(base_offset, eight_reg, account_offset));

                    // lamports offset = 72
                    let lamports_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(lamports_offset, 72));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, base_offset, lamports_offset));

                    let addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(addr, accounts_ptr, total_offset));

                    // Store the new lamports value
                    self.emit(IrInstruction::Store(addr, value_reg, 0));
                    return Ok(None); // Store has no result
                }

                // Handle (account-executable idx) - check if account is executable (1 byte at offset 3)
                if name == "account-executable" && args.len() == 1 {
                    let idx_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-executable index has no result"))?;

                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10344));

                    let account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(account_offset, idx_reg, account_size));

                    let base_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(base_offset, eight_reg, account_offset));

                    // executable is at offset 3 from account start
                    let exec_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(exec_offset, 3));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, base_offset, exec_offset));

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
                // For accounts with ZERO data (common case: wallet accounts), each account is 10336 bytes:
                //   dup_info(1) + is_signer(1) + is_writable(1) + executable(1) + padding(4) +
                //   pubkey(32) + owner(32) + lamports(8) + data_len(8) + data(0) +
                //   MAX_PERMITTED_DATA_INCREASE(10240) + rent_epoch(8) = 10336 bytes
                //
                // LIMITATION: This implementation assumes all accounts have data_len=0.
                // For programs with data-bearing accounts, use a more sophisticated approach.
                if name == "instruction-data-len" && args.is_empty() {
                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Read num_accounts from offset 0
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));

                    // Calculate offset to instruction data length:
                    // offset = 8 + num_accounts * 10336
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336));

                    let accounts_total = self.alloc_reg();
                    self.emit(IrInstruction::Mul(accounts_total, num_accounts, account_size));

                    let instr_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(instr_offset, eight, accounts_total));

                    // Read instruction data length at that offset
                    let instr_len_addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(instr_len_addr, accounts_ptr, instr_offset));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load(dst, instr_len_addr, 0));
                    return Ok(Some(dst));
                }

                // Handle (instruction-data-ptr) - get pointer to instruction data
                // Same calculation as instruction-data-len, but return ptr + 8 (skip length)
                if name == "instruction-data-ptr" && args.is_empty() {
                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Read num_accounts from offset 0
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, accounts_ptr, 0));

                    // Calculate offset to instruction data length:
                    // offset = 8 + num_accounts * 10336
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336));

                    let accounts_total = self.alloc_reg();
                    self.emit(IrInstruction::Mul(accounts_total, num_accounts, account_size));

                    let instr_len_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(instr_len_offset, eight, accounts_total));

                    // Skip past the length (8 bytes) to get to actual data
                    let instr_data_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(instr_data_offset, instr_len_offset, eight));

                    // Return pointer to instruction data
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Add(dst, accounts_ptr, instr_data_offset));
                    return Ok(Some(dst));
                }

                // Handle (account-data-ptr idx) - get pointer to account data
                // Data starts at offset 88 from account start (after data_len at 80)
                // For account 0: 8 + 88 = 96 from input start
                if name == "account-data-ptr" && args.len() == 1 {
                    let idx_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-data-ptr index has no result"))?;

                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));

                    // Same approximate account size as lamports
                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10344));

                    let account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(account_offset, idx_reg, account_size));

                    let base_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(base_offset, eight_reg, account_offset));

                    // Data starts at offset 88 from account start
                    // = 1+1+1+1+4+32+32+8+8 = 88
                    let data_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(data_offset, 88));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, base_offset, data_offset));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Add(dst, accounts_ptr, total_offset));
                    return Ok(Some(dst));
                }

                // Handle (account-data-len idx) - get data length for account
                // data_len is at offset 80 from account start
                // For account 0: 8 + 80 = 88 from input start
                if name == "account-data-len" && args.len() == 1 {
                    let idx_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-data-len index has no result"))?;

                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10344));

                    let account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(account_offset, idx_reg, account_size));

                    let base_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(base_offset, eight_reg, account_offset));

                    // data_len is at offset 80 from account start (right after lamports)
                    // = 1+1+1+1+4+32+32+8 = 80
                    let len_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(len_offset, 80));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, base_offset, len_offset));

                    let addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(addr, accounts_ptr, total_offset));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Load(dst, addr, 0));
                    return Ok(Some(dst));
                }

                // Handle (account-pubkey idx) - get pointer to 32-byte account pubkey
                // Pubkey is at offset 8 from account start (after dup_info, is_signer, is_writable, executable, padding)
                // Layout: dup_info(1) + is_signer(1) + is_writable(1) + executable(1) + padding(4) = 8
                if name == "account-pubkey" && args.len() == 1 {
                    let idx_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-pubkey index has no result"))?;

                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10344));

                    let account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(account_offset, idx_reg, account_size));

                    let base_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(base_offset, eight_reg, account_offset));

                    // Pubkey offset within account = 8 (after flags and padding)
                    let pubkey_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(pubkey_offset, 8));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, base_offset, pubkey_offset));

                    // Return pointer to the pubkey (not the value itself - it's 32 bytes)
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Add(dst, accounts_ptr, total_offset));
                    return Ok(Some(dst));
                }

                // Handle (account-owner idx) - get pointer to 32-byte account owner
                // Owner is at offset 40 from account start (after pubkey)
                // Layout: flags(8) + pubkey(32) = 40
                if name == "account-owner" && args.len() == 1 {
                    let idx_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-owner index has no result"))?;

                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10344));

                    let account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(account_offset, idx_reg, account_size));

                    let base_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(base_offset, eight_reg, account_offset));

                    // Owner offset within account = 40 (8 + 32)
                    let owner_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(owner_offset, 40));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, base_offset, owner_offset));

                    // Return pointer to the owner pubkey (32 bytes)
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Add(dst, accounts_ptr, total_offset));
                    return Ok(Some(dst));
                }

                // Handle (account-is-signer idx) - check if account is signer (1 byte at offset 1)
                if name == "account-is-signer" && args.len() == 1 {
                    let idx_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-is-signer index has no result"))?;

                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10344));

                    let account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(account_offset, idx_reg, account_size));

                    let base_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(base_offset, eight_reg, account_offset));

                    // is_signer is at offset 1 from account start
                    let signer_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(signer_offset, 1));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, base_offset, signer_offset));

                    let addr = self.alloc_reg();
                    self.emit(IrInstruction::Add(addr, accounts_ptr, total_offset));

                    // Load 1 byte (will be 0 or 1)
                    // Note: sBPF Load always loads 8 bytes, but is_signer is just 1 byte
                    // The value will be in the low byte, need to mask
                    let raw = self.alloc_reg();
                    self.emit(IrInstruction::Load(raw, addr, 0));

                    // Mask to get just the lowest byte
                    let mask = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(mask, 0xFF));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::And(dst, raw, mask));
                    return Ok(Some(dst));
                }

                // Handle (account-is-writable idx) - check if account is writable (1 byte at offset 2)
                if name == "account-is-writable" && args.len() == 1 {
                    let idx_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-is-writable index has no result"))?;

                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10344));

                    let account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(account_offset, idx_reg, account_size));

                    let base_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(base_offset, eight_reg, account_offset));

                    // is_writable is at offset 2 from account start
                    let writable_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(writable_offset, 2));

                    let total_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(total_offset, base_offset, writable_offset));

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

                // Handle (mem-load ptr offset) - load 8 bytes from memory
                // Returns: u64 value at ptr+offset
                if name == "mem-load" && args.len() == 2 {
                    let ptr_reg = self.generate_expr(&args[0].value)?
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
                    let ptr_reg = self.generate_expr(&args[0].value)?
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
                    let ptr_reg = self.generate_expr(&args[0].value)?
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
                    let ptr_reg = self.generate_expr(&args[0].value)?
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
                if name == "mem-store" && args.len() == 3 {
                    let base_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-store base has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => return Err(Error::runtime("mem-store offset must be constant")),
                    };
                    let value_reg = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("mem-store value has no result"))?;
                    self.emit(IrInstruction::Store(base_reg, value_reg, offset));
                    return Ok(None); // Store has no result
                }

                // Handle (mem-store1 base offset value) - store 1 byte to memory
                // Stores the low byte of value at ptr+offset
                if name == "mem-store1" && args.len() == 3 {
                    let base_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-store1 base has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => return Err(Error::runtime("mem-store1 offset must be constant")),
                    };
                    let value_reg = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("mem-store1 value has no result"))?;
                    self.emit(IrInstruction::Store1(base_reg, value_reg, offset));
                    return Ok(None); // Store has no result
                }

                // Handle (mem-store2 base offset value) - store 2 bytes (16-bit) to memory
                if name == "mem-store2" && args.len() == 3 {
                    let base_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-store2 base has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => return Err(Error::runtime("mem-store2 offset must be constant")),
                    };
                    let value_reg = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("mem-store2 value has no result"))?;
                    self.emit(IrInstruction::Store2(base_reg, value_reg, offset));
                    return Ok(None); // Store has no result
                }

                // Handle (mem-store4 base offset value) - store 4 bytes (32-bit) to memory
                if name == "mem-store4" && args.len() == 3 {
                    let base_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("mem-store4 base has no result"))?;
                    let offset = match &args[1].value {
                        Expression::IntLiteral(n) => *n,
                        _ => return Err(Error::runtime("mem-store4 offset must be constant")),
                    };
                    let value_reg = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("mem-store4 value has no result"))?;
                    self.emit(IrInstruction::Store4(base_reg, value_reg, offset));
                    return Ok(None); // Store has no result
                }

                // Handle (syscall "name" args...) - Solana syscall
                if name == "syscall" && !args.is_empty() {
                    // First arg must be the syscall name as a string literal
                    let syscall_name = match &args[0].value {
                        Expression::StringLiteral(s) => s.clone(),
                        _ => return Err(Error::runtime("syscall first argument must be a string literal")),
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
                        let msg_reg = self.generate_expr(&args[0].value)?
                            .ok_or_else(|| Error::runtime("log message has no result"))?;

                        // sol_log_ requires: R1 = pointer, R2 = length
                        // Generate length register
                        let len_reg = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(len_reg, s.len() as i64));

                        let dst = self.alloc_reg();
                        self.emit(IrInstruction::Syscall(Some(dst), name.clone(), vec![msg_reg, len_reg]));
                        return Ok(Some(dst));
                    } else {
                        return Err(Error::runtime("sol_log_ requires a string literal argument"));
                    }
                }

                // Handle (sol_log_64_ ...) - log up to 5 numeric values
                if name == "sol_log_64_" && args.len() >= 1 && args.len() <= 5 {
                    let mut arg_regs = Vec::new();
                    for arg in args {
                        let reg = self.generate_expr(&arg.value)?
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
                    let ptr_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("sol_log_pubkey ptr has no result"))?;

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(Some(dst), "sol_log_pubkey".to_string(), vec![ptr_reg]));
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
                    let src_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("system-transfer src_idx has no result"))?;
                    let dest_idx = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("system-transfer dest_idx has no result"))?;
                    let amount_reg = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("system-transfer amount has no result"))?;

                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // System Program ID: 11111111111111111111111111111111 (all 0 bytes in binary)
                    // Stored in .rodata, need to allocate space on stack for the structure

                    // We need to build:
                    // 1. System Program pubkey (32 bytes of zeros)
                    // 2. Instruction data (12 bytes: u32=2, u64=amount)
                    // 3. SolAccountMeta array (2 entries, 34 bytes each)
                    // 4. SolInstruction struct (40 bytes)

                    // Use the fixed heap address at 0x300000000 (no allocator needed!)
                    // Solana sBPF provides a 32KB heap region that programs can use directly.
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000000_i64));

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
                    self.emit(IrInstruction::Add(instr_data_ptr, heap_base, thirty_two_offset));

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

                    // Get source account pubkey pointer
                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));
                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336));
                    let src_account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(src_account_offset, src_idx, account_size));
                    let src_base = self.alloc_reg();
                    self.emit(IrInstruction::Add(src_base, eight_reg, src_account_offset));
                    // Pubkey is at offset 8 from account start
                    let pubkey_field_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(pubkey_field_offset, 8));
                    let src_pubkey_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(src_pubkey_offset, src_base, pubkey_field_offset));
                    let src_pubkey_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(src_pubkey_ptr, accounts_ptr, src_pubkey_offset));

                    // Meta[0]: source (signer, writable)
                    self.emit(IrInstruction::Store(meta_array_ptr, src_pubkey_ptr, 0)); // pubkey ptr
                    let one_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one_reg, 1));
                    // is_writable (1) and is_signer (1) at bytes 8 and 9
                    // Store as single u64: 0x0101 at offset 8
                    let flags = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(flags, 0x0101)); // is_writable=1, is_signer=1
                    self.emit(IrInstruction::Store(meta_array_ptr, flags, 8));

                    // Get dest account pubkey pointer
                    let dest_account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(dest_account_offset, dest_idx, account_size));
                    let dest_base = self.alloc_reg();
                    self.emit(IrInstruction::Add(dest_base, eight_reg, dest_account_offset));
                    let dest_pubkey_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(dest_pubkey_offset, dest_base, pubkey_field_offset));
                    let dest_pubkey_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(dest_pubkey_ptr, accounts_ptr, dest_pubkey_offset));

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
                    let acct_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(acct_infos_ptr, accounts_ptr, eight_reg));

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
                    let instr_ptr = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("invoke instruction-ptr has no result"))?;
                    let acct_infos_ptr = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("invoke account-infos-ptr has no result"))?;
                    let num_accounts = self.generate_expr(&args[2].value)?
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
                    let instr_ptr = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("invoke-signed instruction-ptr has no result"))?;
                    let acct_infos_ptr = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("invoke-signed account-infos-ptr has no result"))?;
                    let num_accounts = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("invoke-signed num-accounts has no result"))?;
                    let signers_seeds_ptr = self.generate_expr(&args[3].value)?
                        .ok_or_else(|| Error::runtime("invoke-signed signers-seeds-ptr has no result"))?;
                    let num_signers = self.generate_expr(&args[4].value)?
                        .ok_or_else(|| Error::runtime("invoke-signed num-signers has no result"))?;

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, acct_infos_ptr, num_accounts, signers_seeds_ptr, num_signers],
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
                    let program_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("cpi-call program-idx has no result"))?;

                    // Get discriminator
                    let discriminator = self.generate_expr(&args[1].value)?
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
                    let program_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("cpi-invoke program-idx has no result"))?;

                    // Get instruction data pointer
                    let data_ptr = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("cpi-invoke data-ptr has no result"))?;

                    // Get instruction data length
                    let data_len = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("cpi-invoke data-len has no result"))?;

                    // Get program pubkey pointer (account pubkey at program_idx)
                    // Account structure in input: 8 bytes header, then each account at 10336 byte intervals
                    // Pubkey is at offset 8 from each account start
                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let program_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(program_offset, program_idx, account_size));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    self.emit(IrInstruction::Add(program_offset, program_offset, eight)); // skip header
                    self.emit(IrInstruction::Add(program_offset, program_offset, eight)); // pubkey at +8
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1)); // R1 at entry is input ptr
                    let program_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(program_pk_ptr, input_ptr, program_offset));

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
                                        let acct_idx = self.generate_expr(&triple[0])?
                                            .ok_or_else(|| Error::runtime("cpi-invoke account idx has no result"))?;
                                        // Get is_writable
                                        let is_writable = self.generate_expr(&triple[1])?
                                            .ok_or_else(|| Error::runtime("cpi-invoke is_writable has no result"))?;
                                        // Get is_signer
                                        let is_signer = self.generate_expr(&triple[2])?
                                            .ok_or_else(|| Error::runtime("cpi-invoke is_signer has no result"))?;

                                        // Calculate pubkey pointer for this account
                                        let acct_offset = self.alloc_reg();
                                        self.emit(IrInstruction::Mul(acct_offset, acct_idx, account_size));
                                        let temp = self.alloc_reg();
                                        self.emit(IrInstruction::Add(temp, acct_offset, eight)); // skip header
                                        self.emit(IrInstruction::Add(temp, temp, eight)); // pubkey at +8
                                        let acct_pk_ptr = self.alloc_reg();
                                        self.emit(IrInstruction::Add(acct_pk_ptr, input_ptr, temp));

                                        // Write SolAccountMeta at heap_base + i*16
                                        // SolAccountMeta: pubkey* (8), is_writable (1), is_signer (1), padding (6)
                                        let meta_offset = (i * 16) as i64;
                                        self.emit(IrInstruction::Store(heap_base, acct_pk_ptr, meta_offset));
                                        // Store writable and signer as bytes at +8 and +9
                                        // We need to combine them into a u64 for the store
                                        let shift_8 = self.alloc_reg();
                                        self.emit(IrInstruction::ConstI64(shift_8, 256)); // 2^8
                                        let signer_shifted = self.alloc_reg();
                                        self.emit(IrInstruction::Mul(signer_shifted, is_signer, shift_8));
                                        let flags_combined = self.alloc_reg();
                                        self.emit(IrInstruction::Or(flags_combined, is_writable, signer_shifted));
                                        self.emit(IrInstruction::Store(heap_base, flags_combined, meta_offset + 8));
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
                        self.emit(IrInstruction::Store(instr_ptr, heap_base, 8)); // accounts ptr
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

                    // Get account_infos pointer from input (R1 + 8 points to first account)
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, input_ptr, eight));

                    // Get number of accounts from input header (at R1 + 0)
                    let num_input_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_input_accounts, input_ptr, 0));

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
                    let program_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("cpi-invoke-signed program-idx has no result"))?;

                    // Get instruction data pointer
                    let data_ptr = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("cpi-invoke-signed data-ptr has no result"))?;

                    // Get instruction data length
                    let data_len = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("cpi-invoke-signed data-len has no result"))?;

                    // Account size constant
                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));

                    // Get program pubkey pointer
                    let program_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(program_offset, program_idx, account_size));
                    self.emit(IrInstruction::Add(program_offset, program_offset, eight)); // skip header
                    self.emit(IrInstruction::Add(program_offset, program_offset, eight)); // pubkey at +8
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1)); // R1 at entry is input ptr
                    let program_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(program_pk_ptr, input_ptr, program_offset));

                    // Process accounts array (arg 3)
                    let mut num_accounts = 0usize;
                    if let Expression::ArrayLiteral(account_specs) = &args[3].value {
                        num_accounts = account_specs.len();

                        for (i, spec) in account_specs.iter().enumerate() {
                            if let Expression::ArrayLiteral(triple) = spec {
                                if triple.len() >= 3 {
                                    let acct_idx = self.generate_expr(&triple[0])?
                                        .ok_or_else(|| Error::runtime("cpi-invoke-signed account idx has no result"))?;
                                    let is_writable = self.generate_expr(&triple[1])?
                                        .ok_or_else(|| Error::runtime("cpi-invoke-signed is_writable has no result"))?;
                                    let is_signer = self.generate_expr(&triple[2])?
                                        .ok_or_else(|| Error::runtime("cpi-invoke-signed is_signer has no result"))?;

                                    // Calculate pubkey pointer
                                    let acct_offset = self.alloc_reg();
                                    self.emit(IrInstruction::Mul(acct_offset, acct_idx, account_size));
                                    let temp = self.alloc_reg();
                                    self.emit(IrInstruction::Add(temp, acct_offset, eight));
                                    self.emit(IrInstruction::Add(temp, temp, eight));
                                    let acct_pk_ptr = self.alloc_reg();
                                    self.emit(IrInstruction::Add(acct_pk_ptr, input_ptr, temp));

                                    // Write SolAccountMeta at heap_base + i*16
                                    let meta_offset = (i * 16) as i64;
                                    self.emit(IrInstruction::Store(heap_base, acct_pk_ptr, meta_offset));
                                    let shift_8 = self.alloc_reg();
                                    self.emit(IrInstruction::ConstI64(shift_8, 256));
                                    let signer_shifted = self.alloc_reg();
                                    self.emit(IrInstruction::Mul(signer_shifted, is_signer, shift_8));
                                    let flags_combined = self.alloc_reg();
                                    self.emit(IrInstruction::Or(flags_combined, is_writable, signer_shifted));
                                    self.emit(IrInstruction::Store(heap_base, flags_combined, meta_offset + 8));
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
                        self.emit(IrInstruction::Store(instr_ptr, heap_base, 8)); // accounts ptr
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
                                self.emit(IrInstruction::ConstI64(seeds_ptr_offset, (total_seed_entries * 16) as i64));
                                let seeds_ptr = self.alloc_reg();
                                self.emit(IrInstruction::Add(seeds_ptr, seed_entries_base, seeds_ptr_offset));

                                // Store addr and len in SolSignerSeeds
                                self.emit(IrInstruction::Store(signer_seeds_base, seeds_ptr, signer_entry_offset));
                                let num_seeds_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(num_seeds_reg, num_seeds_this_signer as i64));
                                self.emit(IrInstruction::Store(signer_seeds_base, num_seeds_reg, signer_entry_offset + 8));

                                // Now write each SolSignerSeed entry
                                for (seed_idx, seed) in seeds.iter().enumerate() {
                                    if let Expression::ArrayLiteral(seed_pair) = seed {
                                        if seed_pair.len() >= 2 {
                                            let seed_addr = self.generate_expr(&seed_pair[0])?
                                                .ok_or_else(|| Error::runtime("cpi-invoke-signed seed addr has no result"))?;
                                            let seed_len = self.generate_expr(&seed_pair[1])?
                                                .ok_or_else(|| Error::runtime("cpi-invoke-signed seed len has no result"))?;

                                            // Write at seed_entries_base + (total_seed_entries + seed_idx) * 16
                                            let entry_offset = ((total_seed_entries + seed_idx) * 16) as i64;
                                            self.emit(IrInstruction::Store(seed_entries_base, seed_addr, entry_offset));
                                            self.emit(IrInstruction::Store(seed_entries_base, seed_len, entry_offset + 8));
                                        }
                                    }
                                }

                                total_seed_entries += num_seeds_this_signer;
                            }
                        }
                    }

                    // Get account_infos pointer from input
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, input_ptr, eight));

                    // Get number of accounts from input header
                    let num_input_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_input_accounts, input_ptr, 0));

                    // Invoke with signer seeds
                    let num_signers_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_signers_reg, num_signers as i64));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, account_infos_ptr, num_input_accounts, signer_seeds_base, num_signers_reg],
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
                    let token_prog_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("spl-token-transfer token-prog-idx has no result"))?;
                    let source_idx = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("spl-token-transfer source-idx has no result"))?;
                    let dest_idx = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("spl-token-transfer dest-idx has no result"))?;
                    let authority_idx = self.generate_expr(&args[3].value)?
                        .ok_or_else(|| Error::runtime("spl-token-transfer authority-idx has no result"))?;
                    let amount = self.generate_expr(&args[4].value)?
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

                    // Constants
                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    // Get token program pubkey pointer
                    let prog_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(prog_offset, token_prog_idx, account_size));
                    self.emit(IrInstruction::Add(prog_offset, prog_offset, eight));
                    self.emit(IrInstruction::Add(prog_offset, prog_offset, eight));
                    let token_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(token_pk_ptr, input_ptr, prog_offset));

                    // Build SolAccountMeta array at heap_base (3 accounts)
                    // Account 0: Source (writable, not signer)
                    let src_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(src_offset, source_idx, account_size));
                    let temp = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp, src_offset, eight));
                    self.emit(IrInstruction::Add(temp, temp, eight));
                    let src_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(src_pk_ptr, input_ptr, temp));
                    self.emit(IrInstruction::Store(heap_base, src_pk_ptr, 0)); // pubkey ptr
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1)); // writable=1, signer=0
                    self.emit(IrInstruction::Store(heap_base, one, 8)); // flags

                    // Account 1: Dest (writable, not signer)
                    let dst_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(dst_offset, dest_idx, account_size));
                    let temp2 = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp2, dst_offset, eight));
                    self.emit(IrInstruction::Add(temp2, temp2, eight));
                    let dst_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(dst_pk_ptr, input_ptr, temp2));
                    self.emit(IrInstruction::Store(heap_base, dst_pk_ptr, 16)); // pubkey ptr at +16
                    self.emit(IrInstruction::Store(heap_base, one, 24)); // writable=1, signer=0

                    // Account 2: Authority (not writable, signer)
                    let auth_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(auth_offset, authority_idx, account_size));
                    let temp3 = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp3, auth_offset, eight));
                    self.emit(IrInstruction::Add(temp3, temp3, eight));
                    let auth_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(auth_pk_ptr, input_ptr, temp3));
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

                    // Get account_infos from input
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, input_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, input_ptr, 0));

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
                    let token_prog_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("spl-token-transfer-signed token-prog-idx has no result"))?;
                    let source_idx = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("spl-token-transfer-signed source-idx has no result"))?;
                    let dest_idx = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("spl-token-transfer-signed dest-idx has no result"))?;
                    let authority_idx = self.generate_expr(&args[3].value)?
                        .ok_or_else(|| Error::runtime("spl-token-transfer-signed authority-idx has no result"))?;
                    let amount = self.generate_expr(&args[4].value)?
                        .ok_or_else(|| Error::runtime("spl-token-transfer-signed amount has no result"))?;

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

                    // Constants
                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    // Get token program pubkey pointer
                    let prog_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(prog_offset, token_prog_idx, account_size));
                    self.emit(IrInstruction::Add(prog_offset, prog_offset, eight));
                    self.emit(IrInstruction::Add(prog_offset, prog_offset, eight));
                    let token_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(token_pk_ptr, input_ptr, prog_offset));

                    // SolAccountMeta array at heap_base + 64
                    let accounts_base = self.alloc_reg();
                    let const_64 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_64, 64));
                    self.emit(IrInstruction::Add(accounts_base, heap_base, const_64));

                    // Account 0: Source (writable, not signer)
                    let src_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(src_offset, source_idx, account_size));
                    let temp = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp, src_offset, eight));
                    self.emit(IrInstruction::Add(temp, temp, eight));
                    let src_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(src_pk_ptr, input_ptr, temp));
                    self.emit(IrInstruction::Store(accounts_base, src_pk_ptr, 0));
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1)); // writable=1, signer=0
                    self.emit(IrInstruction::Store(accounts_base, one, 8));

                    // Account 1: Dest (writable, not signer)
                    let dst_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(dst_offset, dest_idx, account_size));
                    let temp2 = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp2, dst_offset, eight));
                    self.emit(IrInstruction::Add(temp2, temp2, eight));
                    let dst_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(dst_pk_ptr, input_ptr, temp2));
                    self.emit(IrInstruction::Store(accounts_base, dst_pk_ptr, 16));
                    self.emit(IrInstruction::Store(accounts_base, one, 24));

                    // Account 2: Authority (not writable, signer via PDA)
                    let auth_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(auth_offset, authority_idx, account_size));
                    let temp3 = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp3, auth_offset, eight));
                    self.emit(IrInstruction::Add(temp3, temp3, eight));
                    let auth_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(auth_pk_ptr, input_ptr, temp3));
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
                                            let seed_addr = self.generate_expr(&pair[0])?
                                                .ok_or_else(|| Error::runtime("seed addr has no result"))?;
                                            let seed_len = self.generate_expr(&pair[1])?
                                                .ok_or_else(|| Error::runtime("seed len has no result"))?;

                                            let entry_offset = ((total_seed_entries + seed_idx) * 16) as i64;
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

                    // Get account_infos from input
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, input_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, input_ptr, 0));

                    // Call sol_invoke_signed_c
                    let num_signers_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_signers_reg, num_signers as i64));

                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, account_infos_ptr, num_accounts, signer_seeds_base, num_signers_reg],
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

                    let token_prog_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("spl-token-mint-to token-prog-idx has no result"))?;
                    let mint_idx = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("spl-token-mint-to mint-idx has no result"))?;
                    let dest_idx = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("spl-token-mint-to dest-idx has no result"))?;
                    let authority_idx = self.generate_expr(&args[3].value)?
                        .ok_or_else(|| Error::runtime("spl-token-mint-to authority-idx has no result"))?;
                    let amount = self.generate_expr(&args[4].value)?
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

                    // Constants
                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    // Get token program pubkey
                    let prog_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(prog_offset, token_prog_idx, account_size));
                    self.emit(IrInstruction::Add(prog_offset, prog_offset, eight));
                    self.emit(IrInstruction::Add(prog_offset, prog_offset, eight));
                    let token_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(token_pk_ptr, input_ptr, prog_offset));

                    // Build SolAccountMeta array (3 accounts)
                    // Account 0: Mint (writable)
                    let mint_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(mint_offset, mint_idx, account_size));
                    let temp = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp, mint_offset, eight));
                    self.emit(IrInstruction::Add(temp, temp, eight));
                    let mint_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(mint_pk_ptr, input_ptr, temp));
                    self.emit(IrInstruction::Store(heap_base, mint_pk_ptr, 0));
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Store(heap_base, one, 8));

                    // Account 1: Dest (writable)
                    let dst_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(dst_offset, dest_idx, account_size));
                    let temp2 = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp2, dst_offset, eight));
                    self.emit(IrInstruction::Add(temp2, temp2, eight));
                    let dst_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(dst_pk_ptr, input_ptr, temp2));
                    self.emit(IrInstruction::Store(heap_base, dst_pk_ptr, 16));
                    self.emit(IrInstruction::Store(heap_base, one, 24));

                    // Account 2: Authority (signer)
                    let auth_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(auth_offset, authority_idx, account_size));
                    let temp3 = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp3, auth_offset, eight));
                    self.emit(IrInstruction::Add(temp3, temp3, eight));
                    let auth_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(auth_pk_ptr, input_ptr, temp3));
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

                    // Invoke
                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, input_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, input_ptr, 0));
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

                    let token_prog_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("spl-token-burn token-prog-idx has no result"))?;
                    let source_idx = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("spl-token-burn source-idx has no result"))?;
                    let mint_idx = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("spl-token-burn mint-idx has no result"))?;
                    let authority_idx = self.generate_expr(&args[3].value)?
                        .ok_or_else(|| Error::runtime("spl-token-burn authority-idx has no result"))?;
                    let amount = self.generate_expr(&args[4].value)?
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

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    let prog_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(prog_offset, token_prog_idx, account_size));
                    self.emit(IrInstruction::Add(prog_offset, prog_offset, eight));
                    self.emit(IrInstruction::Add(prog_offset, prog_offset, eight));
                    let token_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(token_pk_ptr, input_ptr, prog_offset));

                    // Account 0: Source (writable)
                    let src_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(src_offset, source_idx, account_size));
                    let temp = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp, src_offset, eight));
                    self.emit(IrInstruction::Add(temp, temp, eight));
                    let src_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(src_pk_ptr, input_ptr, temp));
                    self.emit(IrInstruction::Store(heap_base, src_pk_ptr, 0));
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Store(heap_base, one, 8));

                    // Account 1: Mint (writable)
                    let mint_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(mint_offset, mint_idx, account_size));
                    let temp2 = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp2, mint_offset, eight));
                    self.emit(IrInstruction::Add(temp2, temp2, eight));
                    let mint_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(mint_pk_ptr, input_ptr, temp2));
                    self.emit(IrInstruction::Store(heap_base, mint_pk_ptr, 16));
                    self.emit(IrInstruction::Store(heap_base, one, 24));

                    // Account 2: Authority (signer)
                    let auth_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(auth_offset, authority_idx, account_size));
                    let temp3 = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp3, auth_offset, eight));
                    self.emit(IrInstruction::Add(temp3, temp3, eight));
                    let auth_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(auth_pk_ptr, input_ptr, temp3));
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

                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, input_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, input_ptr, 0));
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

                    let payer_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("system-create-account payer-idx has no result"))?;
                    let new_acct_idx = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("system-create-account new-acct-idx has no result"))?;
                    let lamports = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("system-create-account lamports has no result"))?;
                    let space = self.generate_expr(&args[3].value)?
                        .ok_or_else(|| Error::runtime("system-create-account space has no result"))?;
                    let owner_ptr = self.generate_expr(&args[4].value)?
                        .ok_or_else(|| Error::runtime("system-create-account owner-pubkey-ptr has no result"))?;

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

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    // Build SolAccountMeta array at heap_base + 128
                    let meta_base = self.alloc_reg();
                    let const_128 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(const_128, 128));
                    self.emit(IrInstruction::Add(meta_base, heap_base, const_128));

                    // Account 0: Payer (writable, signer)
                    let payer_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(payer_offset, payer_idx, account_size));
                    let temp = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp, payer_offset, eight));
                    self.emit(IrInstruction::Add(temp, temp, eight));
                    let payer_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(payer_pk_ptr, input_ptr, temp));
                    self.emit(IrInstruction::Store(meta_base, payer_pk_ptr, 0));
                    let writable_signer = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(writable_signer, 0x0101)); // writable + signer
                    self.emit(IrInstruction::Store(meta_base, writable_signer, 8));

                    // Account 1: New account (writable, signer)
                    let new_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(new_offset, new_acct_idx, account_size));
                    let temp2 = self.alloc_reg();
                    self.emit(IrInstruction::Add(temp2, new_offset, eight));
                    self.emit(IrInstruction::Add(temp2, temp2, eight));
                    let new_pk_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(new_pk_ptr, input_ptr, temp2));
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

                    let account_infos_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(account_infos_ptr, input_ptr, eight));
                    let num_accounts = self.alloc_reg();
                    self.emit(IrInstruction::Load(num_accounts, input_ptr, 0));
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(dst),
                        "sol_invoke_signed_c".to_string(),
                        vec![instr_ptr, account_infos_ptr, num_accounts, zero, zero],
                    ));

                    return Ok(Some(dst));
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
                    let program_pk_ptr = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("derive-pda program-pubkey-ptr has no result"))?;
                    let seeds_ptr = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("derive-pda seeds-ptr has no result"))?;
                    let bump_ptr = self.generate_expr(&args[2].value)?
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
                    let dest_ptr = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("create-pda dest-ptr has no result"))?;
                    let program_pk_ptr = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("create-pda program-pubkey-ptr has no result"))?;

                    // Build seeds array in heap
                    let heap_base = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(heap_base, 0x300000D00_i64));

                    let mut num_seeds = 0usize;
                    if let Expression::ArrayLiteral(seeds) = &args[2].value {
                        num_seeds = seeds.len();

                        for (i, seed) in seeds.iter().enumerate() {
                            if let Expression::ArrayLiteral(pair) = seed {
                                if pair.len() >= 2 {
                                    let seed_addr = self.generate_expr(&pair[0])?
                                        .ok_or_else(|| Error::runtime("create-pda seed addr has no result"))?;
                                    let seed_len = self.generate_expr(&pair[1])?
                                        .ok_or_else(|| Error::runtime("create-pda seed len has no result"))?;

                                    let offset = (i * 16) as i64;
                                    self.emit(IrInstruction::Store(heap_base, seed_addr, offset));
                                    self.emit(IrInstruction::Store(heap_base, seed_len, offset + 8));
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
                    let dest_ptr = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("get-ata dest-ptr has no result"))?;
                    let wallet_ptr = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("get-ata wallet-pubkey-ptr has no result"))?;
                    let mint_ptr = self.generate_expr(&args[2].value)?
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
                    self.emit(IrInstruction::ConstI64(token_prog_bytes_0, 0x8c97258f4e2489f1_u64 as i64));
                    self.emit(IrInstruction::Store(token_prog_ptr, token_prog_bytes_0, 0));
                    let token_prog_bytes_1 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(token_prog_bytes_1, 0x39d0a8d9b3b71f14_u64 as i64));
                    self.emit(IrInstruction::Store(token_prog_ptr, token_prog_bytes_1, 8));
                    let token_prog_bytes_2 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(token_prog_bytes_2, 0x9d5bce6b0c6a1de5_u64 as i64));
                    self.emit(IrInstruction::Store(token_prog_ptr, token_prog_bytes_2, 16));
                    let token_prog_bytes_3 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(token_prog_bytes_3, 0x06ddf6e1d765a193_u64 as i64));
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
                    self.emit(IrInstruction::ConstI64(ata_bytes_0, 0x8c97258f4e2489f1_u64 as i64)); // placeholder
                    self.emit(IrInstruction::Store(ata_prog_ptr, ata_bytes_0, 0));
                    let ata_bytes_1 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(ata_bytes_1, 0x39d0a8d9b3b71f14_u64 as i64));
                    self.emit(IrInstruction::Store(ata_prog_ptr, ata_bytes_1, 8));
                    let ata_bytes_2 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(ata_bytes_2, 0x9d5bce6b0c6a1de5_u64 as i64));
                    self.emit(IrInstruction::Store(ata_prog_ptr, ata_bytes_2, 16));
                    let ata_bytes_3 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(ata_bytes_3, 0x06ddf6e1d765a193_u64 as i64));
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
                    let account_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("assert-signer account-idx has no result"))?;

                    // Account structure in serialized input:
                    // Offset 1 from account start = is_signer byte
                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    // Calculate offset to is_signer byte
                    let acct_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(acct_offset, account_idx, account_size));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, eight)); // skip num_accounts
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, one)); // is_signer at +1

                    let is_signer_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(is_signer_ptr, input_ptr, acct_offset));

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
                    self.emit(IrInstruction::Syscall(None, "sol_panic_".to_string(), vec![error_code, error_code, error_code, error_code, error_code]));

                    self.emit(IrInstruction::Label(label_ok));
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));

                    return Ok(Some(zero));
                }

                // (assert-writable account-idx) - Abort if account is not writable
                if name == "assert-writable" && args.len() == 1 {
                    let account_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("assert-writable account-idx has no result"))?;

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    // is_writable at offset 2 from account start
                    let acct_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(acct_offset, account_idx, account_size));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, eight));
                    let two = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(two, 2));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, two));

                    let is_writable_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(is_writable_ptr, input_ptr, acct_offset));

                    let is_writable = self.alloc_reg();
                    self.emit(IrInstruction::Load1(is_writable, is_writable_ptr, 0));

                    let label_ok = self.new_label("writable_ok");
                    self.emit(IrInstruction::JumpIf(is_writable, label_ok.clone()));
                    let error_code = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(error_code, 0x2000000));
                    self.emit(IrInstruction::Syscall(None, "sol_panic_".to_string(), vec![error_code, error_code, error_code, error_code, error_code]));

                    self.emit(IrInstruction::Label(label_ok));
                    let zero = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(zero, 0));

                    return Ok(Some(zero));
                }

                // (assert-owner account-idx expected-owner-ptr) - Abort if account owner doesn't match
                if name == "assert-owner" && args.len() == 2 {
                    let account_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("assert-owner account-idx has no result"))?;
                    let expected_owner = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("assert-owner expected-owner-ptr has no result"))?;

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    // Owner pubkey at offset 40 from account start (after pubkey at 8)
                    let acct_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(acct_offset, account_idx, account_size));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, eight));
                    let forty = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(forty, 40));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, forty));

                    let owner_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(owner_ptr, input_ptr, acct_offset));

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
                    self.emit(IrInstruction::Syscall(None, "sol_panic_".to_string(), vec![error_code, error_code, error_code, error_code, error_code]));

                    self.emit(IrInstruction::Label(label_ok));

                    return Ok(Some(zero));
                }

                // (is-signer account-idx) - Returns 1 if signer, 0 if not (no abort)
                if name == "is-signer" && args.len() == 1 {
                    let account_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("is-signer account-idx has no result"))?;

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    let acct_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(acct_offset, account_idx, account_size));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, eight));
                    let one = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(one, 1));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, one));

                    let is_signer_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(is_signer_ptr, input_ptr, acct_offset));

                    let is_signer = self.alloc_reg();
                    self.emit(IrInstruction::Load1(is_signer, is_signer_ptr, 0));

                    return Ok(Some(is_signer));
                }

                // (is-writable account-idx) - Returns 1 if writable, 0 if not
                if name == "is-writable" && args.len() == 1 {
                    let account_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("is-writable account-idx has no result"))?;

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    let acct_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(acct_offset, account_idx, account_size));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, eight));
                    let two = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(two, 2));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, two));

                    let is_writable_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(is_writable_ptr, input_ptr, acct_offset));

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
                if name == "zerocopy-load" && args.len() == 3 {
                    if let (Expression::Variable(struct_name), Expression::Variable(field_name)) =
                        (&args[0].value, &args[2].value)
                    {
                        let account_idx = self.generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("zerocopy-load account-idx has no result"))?;

                        let struct_def = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
                            .clone();

                        let field = struct_def.fields.iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| Error::runtime(format!(
                                "Unknown field '{}' in struct '{}'", field_name, struct_name
                            )))?;

                        // Calculate account data pointer
                        // Account data starts at offset 72 from account base
                        let account_size = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                        let eight = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(eight, 8));
                        let input_ptr = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(input_ptr, 1));

                        let acct_offset = self.alloc_reg();
                        self.emit(IrInstruction::Mul(acct_offset, account_idx, account_size));
                        self.emit(IrInstruction::Add(acct_offset, acct_offset, eight)); // skip header

                        // Data starts at offset 72 from account start (after headers)
                        let data_offset = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(data_offset, 72));
                        self.emit(IrInstruction::Add(acct_offset, acct_offset, data_offset));

                        let data_ptr = self.alloc_reg();
                        self.emit(IrInstruction::Add(data_ptr, input_ptr, acct_offset));

                        // Load field at offset
                        let dst = self.alloc_reg();
                        let field_offset = field.offset;

                        match &field.field_type {
                            FieldType::Primitive(PrimitiveType::U8) | FieldType::Primitive(PrimitiveType::I8) => {
                                self.emit(IrInstruction::Load1(dst, data_ptr, field_offset));
                            }
                            FieldType::Primitive(PrimitiveType::U16) | FieldType::Primitive(PrimitiveType::I16) => {
                                self.emit(IrInstruction::Load2(dst, data_ptr, field_offset));
                            }
                            FieldType::Primitive(PrimitiveType::U32) | FieldType::Primitive(PrimitiveType::I32) => {
                                self.emit(IrInstruction::Load4(dst, data_ptr, field_offset));
                            }
                            FieldType::Primitive(PrimitiveType::U64) | FieldType::Primitive(PrimitiveType::I64) => {
                                self.emit(IrInstruction::Load(dst, data_ptr, field_offset));
                            }
                            FieldType::Pubkey | FieldType::Array { .. } | FieldType::Struct(_) => {
                                // Return pointer to field for complex types
                                let offset_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(offset_reg, field_offset));
                                self.emit(IrInstruction::Add(dst, data_ptr, offset_reg));
                            }
                        }

                        return Ok(Some(dst));
                    }
                }

                // (zerocopy-store StructName account-idx field-name value) -> void
                // Directly stores a value to account data
                if name == "zerocopy-store" && args.len() == 4 {
                    if let (Expression::Variable(struct_name), Expression::Variable(field_name)) =
                        (&args[0].value, &args[2].value)
                    {
                        let account_idx = self.generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("zerocopy-store account-idx has no result"))?;
                        let value = self.generate_expr(&args[3].value)?
                            .ok_or_else(|| Error::runtime("zerocopy-store value has no result"))?;

                        let struct_def = self.struct_defs.get(struct_name)
                            .ok_or_else(|| Error::runtime(format!("Unknown struct '{}'", struct_name)))?
                            .clone();

                        let field = struct_def.fields.iter()
                            .find(|f| &f.name == field_name)
                            .ok_or_else(|| Error::runtime(format!(
                                "Unknown field '{}' in struct '{}'", field_name, struct_name
                            )))?;

                        let account_size = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                        let eight = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(eight, 8));
                        let input_ptr = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(input_ptr, 1));

                        let acct_offset = self.alloc_reg();
                        self.emit(IrInstruction::Mul(acct_offset, account_idx, account_size));
                        self.emit(IrInstruction::Add(acct_offset, acct_offset, eight));

                        let data_offset = self.alloc_reg();
                        self.emit(IrInstruction::ConstI64(data_offset, 72));
                        self.emit(IrInstruction::Add(acct_offset, acct_offset, data_offset));

                        let data_ptr = self.alloc_reg();
                        self.emit(IrInstruction::Add(data_ptr, input_ptr, acct_offset));

                        let field_offset = field.offset;

                        match &field.field_type {
                            FieldType::Primitive(PrimitiveType::U8) | FieldType::Primitive(PrimitiveType::I8) => {
                                self.emit(IrInstruction::Store1(data_ptr, value, field_offset));
                            }
                            FieldType::Primitive(PrimitiveType::U16) | FieldType::Primitive(PrimitiveType::I16) => {
                                self.emit(IrInstruction::Store2(data_ptr, value, field_offset));
                            }
                            FieldType::Primitive(PrimitiveType::U32) | FieldType::Primitive(PrimitiveType::I32) => {
                                self.emit(IrInstruction::Store4(data_ptr, value, field_offset));
                            }
                            FieldType::Primitive(PrimitiveType::U64) | FieldType::Primitive(PrimitiveType::I64) => {
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
                    let account_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-data-len account-idx has no result"))?;

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    let acct_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(acct_offset, account_idx, account_size));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, eight));

                    // Data length at offset 64 from account start
                    let len_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(len_offset, 64));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, len_offset));

                    let len_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(len_ptr, input_ptr, acct_offset));

                    let data_len = self.alloc_reg();
                    self.emit(IrInstruction::Load(data_len, len_ptr, 0));

                    return Ok(Some(data_len));
                }

                // (account-lamports account-idx) -> lamports balance
                if name == "account-lamports" && args.len() == 1 {
                    let account_idx = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("account-lamports account-idx has no result"))?;

                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336_i64));
                    let eight = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight, 8));
                    let input_ptr = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(input_ptr, 1));

                    let acct_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(acct_offset, account_idx, account_size));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, eight));

                    // Lamports at offset 56 from account start
                    let lamports_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(lamports_offset, 56));
                    self.emit(IrInstruction::Add(acct_offset, acct_offset, lamports_offset));

                    let lamports_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(lamports_ptr, input_ptr, acct_offset));

                    let lamports = self.alloc_reg();
                    self.emit(IrInstruction::Load(lamports, lamports_ptr, 0));

                    return Ok(Some(lamports));
                }

                // Handle (build-instruction program-ptr data-ptr data-len) -> instruction ptr
                // Returns pointer to a SolInstruction struct built in heap memory
                if name == "build-instruction" && args.len() == 3 {
                    let program_ptr = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("build-instruction program-ptr has no result"))?;
                    let data_ptr = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("build-instruction data-ptr has no result"))?;
                    let data_len = self.generate_expr(&args[2].value)?
                        .ok_or_else(|| Error::runtime("build-instruction data-len has no result"))?;

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
                        eprintln!("  arg[{}]: name={:?}, value type={:?}", i, arg.name, arg.value);
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
                                    "log :message must be a string literal for compilation"
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
                                let val_reg = self.generate_expr(val_expr)?
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
                    let cond_reg = self.generate_expr(&args[0].value)?
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
                    let accounts_ptr = *self.var_map.get("accounts")
                        .ok_or_else(|| Error::runtime("accounts not available"))?;

                    // Get program_id account index
                    let prog_idx_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("find-pda program_id_idx has no result"))?;

                    // Calculate program_id pubkey pointer
                    let eight_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(eight_reg, 8));
                    let account_size = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(account_size, 10336));
                    let prog_account_offset = self.alloc_reg();
                    self.emit(IrInstruction::Mul(prog_account_offset, prog_idx_reg, account_size));
                    let prog_base = self.alloc_reg();
                    self.emit(IrInstruction::Add(prog_base, eight_reg, prog_account_offset));
                    let pubkey_field_offset = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(pubkey_field_offset, 8));
                    let prog_pubkey_offset = self.alloc_reg();
                    self.emit(IrInstruction::Add(prog_pubkey_offset, prog_base, pubkey_field_offset));
                    let program_id_ptr = self.alloc_reg();
                    self.emit(IrInstruction::Add(program_id_ptr, accounts_ptr, prog_pubkey_offset));

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
                    let _256 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(_256, 256));
                    self.emit(IrInstruction::Add(seed_data_base, heap_base, _256));

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
                                self.emit(IrInstruction::Add(sol_bytes_ptr, seeds_array_ptr, sb_offset));
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
                                self.emit(IrInstruction::Add(sol_bytes_ptr, seeds_array_ptr, sb_offset));
                                self.emit(IrInstruction::Store(sol_bytes_ptr, data_ptr, 0));
                                let eight = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(eight, 8));
                                self.emit(IrInstruction::Store(sol_bytes_ptr, eight, 8));

                                seed_data_offset += 8;
                            }
                            _ => {
                                // Dynamic value: evaluate and store as 8-byte value
                                let val_reg = self.generate_expr(&arg.value)?
                                    .ok_or_else(|| Error::runtime("find-pda seed has no result"))?;

                                let data_ptr = self.alloc_reg();
                                let offset_reg = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(offset_reg, seed_data_offset));
                                self.emit(IrInstruction::Add(data_ptr, seed_data_base, offset_reg));
                                self.emit(IrInstruction::Store(data_ptr, val_reg, 0));

                                let sol_bytes_ptr = self.alloc_reg();
                                let sb_offset = self.alloc_reg();
                                self.emit(IrInstruction::ConstI64(sb_offset, sol_bytes_offset));
                                self.emit(IrInstruction::Add(sol_bytes_ptr, seeds_array_ptr, sb_offset));
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
                    let _768 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(_768, 768));
                    self.emit(IrInstruction::Add(address_out, heap_base, _768));

                    // Bump seed at +800
                    let bump_out = self.alloc_reg();
                    let _800 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(_800, 800));
                    self.emit(IrInstruction::Add(bump_out, heap_base, _800));

                    // Number of seeds
                    let num_seeds_reg = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(num_seeds_reg, num_seeds as i64));

                    // Call sol_try_find_program_address
                    let result = self.alloc_reg();
                    self.emit(IrInstruction::Syscall(
                        Some(result),
                        "sol_try_find_program_address".to_string(),
                        vec![seeds_array_ptr, num_seeds_reg, program_id_ptr, address_out, bump_out],
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
                    let _800 = self.alloc_reg();
                    self.emit(IrInstruction::ConstI64(_800, 800));
                    self.emit(IrInstruction::Add(bump_ptr, heap_base, _800));

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
                    let left_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("and left operand has no result"))?;
                    let right_reg = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("and right operand has no result"))?;
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::And(dst, left_reg, right_reg));
                    return Ok(Some(dst));
                }

                // Handle (or expr1 expr2) - logical OR
                if name == "or" && args.len() == 2 {
                    let left_reg = self.generate_expr(&args[0].value)?
                        .ok_or_else(|| Error::runtime("or left operand has no result"))?;
                    let right_reg = self.generate_expr(&args[1].value)?
                        .ok_or_else(|| Error::runtime("or right operand has no result"))?;
                    let dst = self.alloc_reg();
                    self.emit(IrInstruction::Or(dst, left_reg, right_reg));
                    return Ok(Some(dst));
                }

                // Handle (not expr) - logical NOT
                if name == "not" && args.len() == 1 {
                    let operand_reg = self.generate_expr(&args[0].value)?
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

            Expression::Ternary { condition, then_expr, else_expr } => {
                let cond_reg = self.generate_expr(condition)?
                    .ok_or_else(|| Error::runtime("Ternary condition has no result"))?;

                let then_label = self.new_label("tern_then");
                let else_label = self.new_label("tern_else");
                let end_label = self.new_label("tern_end");
                let result_reg = self.alloc_reg();

                self.emit(IrInstruction::JumpIf(cond_reg, then_label.clone()));
                self.emit(IrInstruction::Jump(else_label.clone()));

                // Then
                self.emit(IrInstruction::Label(then_label));
                let then_reg = self.generate_expr(then_expr)?
                    .ok_or_else(|| Error::runtime("Ternary then has no result"))?;
                self.emit(IrInstruction::Move(result_reg, then_reg));
                self.emit(IrInstruction::Jump(end_label.clone()));

                // Else
                self.emit(IrInstruction::Label(else_label));
                let else_reg = self.generate_expr(else_expr)?
                    .ok_or_else(|| Error::runtime("Ternary else has no result"))?;
                self.emit(IrInstruction::Move(result_reg, else_reg));

                // End
                self.emit(IrInstruction::Label(end_label));
                Ok(Some(result_reg))
            }

            _ => Ok(None),
        }
    }

    fn alloc_reg(&mut self) -> IrReg {
        let reg = IrReg(self.next_reg);
        self.next_reg += 1;
        reg
    }

    fn new_label(&mut self, prefix: &str) -> String {
        let label = format!("{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    fn emit(&mut self, instr: IrInstruction) {
        self.instructions.push(instr);
    }
}

impl Default for IrGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ir_reg() {
        let r1 = IrReg::new(0);
        let r2 = IrReg::new(1);
        assert_ne!(r1, r2);
    }

    #[test]
    fn test_basic_block() {
        let mut block = BasicBlock::new("test");
        block.instructions.push(IrInstruction::ConstI64(IrReg(0), 42));
        assert_eq!(block.label, "test");
        assert_eq!(block.instructions.len(), 1);
    }
}
