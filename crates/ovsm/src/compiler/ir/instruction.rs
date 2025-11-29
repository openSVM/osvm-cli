//! IR instruction definitions

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
