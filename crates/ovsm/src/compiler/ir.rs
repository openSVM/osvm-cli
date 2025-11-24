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
    /// Debug log (will be sol_log syscall)
    Log(IrReg),

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

        // Generate IR for each statement, tracking last result
        let mut last_result: Option<IrReg> = None;
        for typed_stmt in &program.statements {
            last_result = self.generate_statement(&typed_stmt.statement)?;
        }

        // Return last expression result, or null if none
        if let Some(ret_reg) = last_result {
            self.emit(IrInstruction::Return(Some(ret_reg)));
        } else {
            let null_reg = self.alloc_reg();
            self.emit(IrInstruction::ConstNull(null_reg));
            self.emit(IrInstruction::Return(Some(null_reg)));
        }

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
                if name == "set!" && args.len() == 2 {
                    if let Expression::Variable(var_name) = &args[0].value {
                        let value_reg = self.generate_expr(&args[1].value)?
                            .ok_or_else(|| Error::runtime("Set! value has no result"))?;
                        self.var_map.insert(var_name.clone(), value_reg);
                        return Ok(Some(value_reg));
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

                // Handle (do expr1 expr2 ... exprN) - sequence of expressions, return last
                if name == "do" {
                    let mut last_reg = None;
                    for arg in args {
                        last_reg = self.generate_expr(&arg.value)?;
                    }
                    return Ok(last_reg);
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
