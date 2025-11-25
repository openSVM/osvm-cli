//! # Intermediate Representation (IR) for OVSM Compilation
//!
//! Three-address code IR that bridges the gap between OVSM AST
//! and sBPF bytecode. This representation makes optimization
//! and register allocation tractable.

use std::collections::HashMap;
use crate::{Result, Error};
use super::types::{TypedProgram, TypedStatement, OvsmType};
use crate::{Statement, Expression, BinaryOp, UnaryOp};

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
    /// Load from memory: dst = *(base + offset)
    Load(IrReg, IrReg, i64),
    /// Store to memory: *(base + offset) = src
    Store(IrReg, IrReg, i64),
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
}

impl IrGenerator {
    pub fn new() -> Self {
        let mut gen = Self {
            next_reg: 0,
            label_counter: 0,
            var_map: HashMap::new(),
            strings: Vec::new(),
            instructions: Vec::new(),
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
