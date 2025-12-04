//! IR instruction definitions

/// Virtual register (infinite supply, mapped to physical during codegen)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrReg(pub u32);

impl IrReg {
    /// Creates a new virtual register with the given ID
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
    /// Addition: dst = lhs + rhs
    Add(IrReg, IrReg, IrReg),
    /// Subtraction: dst = lhs - rhs
    Sub(IrReg, IrReg, IrReg),
    /// Multiplication: dst = lhs * rhs
    Mul(IrReg, IrReg, IrReg),
    /// Division: dst = lhs / rhs
    Div(IrReg, IrReg, IrReg),
    /// Modulo: dst = lhs % rhs
    Mod(IrReg, IrReg, IrReg),

    // Comparison (dst = src1 op src2, result is 0 or 1)
    /// Equality: dst = (lhs == rhs)
    Eq(IrReg, IrReg, IrReg),
    /// Not equal: dst = (lhs != rhs)
    Ne(IrReg, IrReg, IrReg),
    /// Less than: dst = (lhs < rhs)
    Lt(IrReg, IrReg, IrReg),
    /// Less than or equal: dst = (lhs <= rhs)
    Le(IrReg, IrReg, IrReg),
    /// Greater than: dst = (lhs > rhs)
    Gt(IrReg, IrReg, IrReg),
    /// Greater than or equal: dst = (lhs >= rhs)
    Ge(IrReg, IrReg, IrReg),

    // Logical
    /// Logical AND: dst = lhs && rhs
    And(IrReg, IrReg, IrReg),
    /// Logical OR: dst = lhs || rhs
    Or(IrReg, IrReg, IrReg),
    /// Logical NOT: dst = !src
    Not(IrReg, IrReg),

    // Unary
    /// Negation: dst = -src
    Neg(IrReg, IrReg),

    // Register operations
    /// Move/copy register: dst = src
    Move(IrReg, IrReg),

    // Control flow
    /// Define a jump target label
    Label(String),
    /// Unconditional jump to label
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
    /// No operation (placeholder instruction, removed during optimization)
    Nop,
}
