//! # sBPF Code Generator
//!
//! Transforms IR into Solana BPF bytecode instructions.
//! sBPF uses 11 64-bit registers (R0-R10) and a RISC-like instruction set.
//!
//! ## Key Technical Details
//! - Syscalls use Murmur3 hashes, not numeric IDs
//! - 64-bit constants require `lddw` (16-byte instruction)
//! - Stack: 4KB per frame, max 5 nested calls
//! - Heap: 32KB total
//! - R10 is frame pointer (read-only)

use super::ir::{IrInstruction, IrProgram, IrReg};
use crate::{Result, Error};
use std::collections::HashMap;

// =============================================================================
// INSTRUCTION ENCODING (per sBPF spec)
// =============================================================================

/// Instruction classes (lower 3 bits)
mod class {
    pub const LD: u8 = 0x00;    // Non-standard load
    pub const LDX: u8 = 0x01;   // Load from memory
    pub const ST: u8 = 0x02;    // Store immediate
    pub const STX: u8 = 0x03;   // Store register
    pub const ALU: u8 = 0x04;   // 32-bit ALU
    pub const JMP: u8 = 0x05;   // 64-bit jumps
    pub const JMP32: u8 = 0x06; // 32-bit jumps
    pub const ALU64: u8 = 0x07; // 64-bit ALU
}

/// ALU operation codes (bits 4-7)
mod alu {
    pub const ADD: u8 = 0x00;
    pub const SUB: u8 = 0x10;
    pub const MUL: u8 = 0x20;
    pub const DIV: u8 = 0x30;
    pub const OR: u8 = 0x40;
    pub const AND: u8 = 0x50;
    pub const LSH: u8 = 0x60;
    pub const RSH: u8 = 0x70;
    pub const NEG: u8 = 0x80;
    pub const MOD: u8 = 0x90;
    pub const XOR: u8 = 0xa0;
    pub const MOV: u8 = 0xb0;
    pub const ARSH: u8 = 0xc0;
    pub const END: u8 = 0xd0;   // Byte swap
}

/// Jump operation codes
mod jmp {
    pub const JA: u8 = 0x00;    // Unconditional
    pub const JEQ: u8 = 0x10;   // ==
    pub const JGT: u8 = 0x20;   // > unsigned
    pub const JGE: u8 = 0x30;   // >= unsigned
    pub const JSET: u8 = 0x40;  // & != 0
    pub const JNE: u8 = 0x50;   // !=
    pub const JSGT: u8 = 0x60;  // > signed
    pub const JSGE: u8 = 0x70;  // >= signed
    pub const CALL: u8 = 0x80;  // Function call
    pub const EXIT: u8 = 0x90;  // Return
    pub const JLT: u8 = 0xa0;   // < unsigned
    pub const JLE: u8 = 0xb0;   // <= unsigned
    pub const JSLT: u8 = 0xc0;  // < signed
    pub const JSLE: u8 = 0xd0;  // <= signed
}

/// Memory size modifiers
mod size {
    pub const W: u8 = 0x00;     // 32-bit word
    pub const H: u8 = 0x08;     // 16-bit half
    pub const B: u8 = 0x10;     // 8-bit byte
    pub const DW: u8 = 0x18;    // 64-bit double word
}

/// Memory mode modifiers
mod mode {
    pub const IMM: u8 = 0x00;   // Immediate (lddw)
    pub const ABS: u8 = 0x20;   // Absolute
    pub const IND: u8 = 0x40;   // Indirect
    pub const MEM: u8 = 0x60;   // Memory (reg + offset)
    pub const ATOMIC: u8 = 0xc0; // Atomic operations
}

/// Atomic operation codes (used with ATOMIC mode)
mod atomic {
    pub const ADD: u8 = 0x00;   // Atomic add
    pub const OR: u8 = 0x40;    // Atomic or
    pub const AND: u8 = 0x50;   // Atomic and
    pub const XOR: u8 = 0xa0;   // Atomic xor
    pub const XCHG: u8 = 0xe0;  // Atomic exchange
    pub const CMPXCHG: u8 = 0xf0; // Compare and exchange
    pub const FETCH: u8 = 0x01; // Fetch flag (return old value)
}

/// Source modifier
const SRC_IMM: u8 = 0x00;  // Immediate value
const SRC_REG: u8 = 0x08;  // Register value

// =============================================================================
// MEMORY REGIONS (Solana virtual address space)
// =============================================================================

/// Solana sBPF virtual memory layout
pub mod memory {
    /// Program code region (read-only)
    pub const PROGRAM_START: u64 = 0x100000000;
    /// Stack region (read/write, grows downward)
    pub const STACK_START: u64 = 0x200000000;
    /// Heap region (read/write)
    pub const HEAP_START: u64 = 0x300000000;
    /// Input data region (read-only)
    pub const INPUT_START: u64 = 0x400000000;

    /// Stack frame size per function call
    pub const STACK_FRAME_SIZE: u64 = 4096; // 4KB
    /// Maximum heap size
    pub const HEAP_MAX_SIZE: u64 = 32768; // 32KB
    /// Maximum call depth
    pub const MAX_CALL_DEPTH: usize = 5;
    /// Maximum instruction count
    pub const MAX_INSTRUCTIONS: usize = 65536; // 512KB bytecode
}

// =============================================================================
// MURMUR3 HASH FOR SYSCALLS
// =============================================================================

/// Compute Murmur3 32-bit hash for syscall names
/// Solana uses this to resolve syscall symbols at runtime
pub fn murmur3_32(data: &[u8], seed: u32) -> u32 {
    const C1: u32 = 0xcc9e2d51;
    const C2: u32 = 0x1b873593;
    const R1: u32 = 15;
    const R2: u32 = 13;
    const M: u32 = 5;
    const N: u32 = 0xe6546b64;

    let mut hash = seed;
    let len = data.len();

    // Process 4-byte chunks
    let chunks = len / 4;
    for i in 0..chunks {
        let mut k = u32::from_le_bytes([
            data[i * 4],
            data[i * 4 + 1],
            data[i * 4 + 2],
            data[i * 4 + 3],
        ]);
        k = k.wrapping_mul(C1);
        k = k.rotate_left(R1);
        k = k.wrapping_mul(C2);

        hash ^= k;
        hash = hash.rotate_left(R2);
        hash = hash.wrapping_mul(M).wrapping_add(N);
    }

    // Process remaining bytes
    let remainder = len % 4;
    if remainder > 0 {
        let mut k: u32 = 0;
        for i in 0..remainder {
            k |= (data[chunks * 4 + i] as u32) << (8 * i);
        }
        k = k.wrapping_mul(C1);
        k = k.rotate_left(R1);
        k = k.wrapping_mul(C2);
        hash ^= k;
    }

    // Finalization
    hash ^= len as u32;
    hash ^= hash >> 16;
    hash = hash.wrapping_mul(0x85ebca6b);
    hash ^= hash >> 13;
    hash = hash.wrapping_mul(0xc2b2ae35);
    hash ^= hash >> 16;

    hash
}

/// Get syscall hash for a Solana syscall name
pub fn syscall_hash(name: &str) -> u32 {
    murmur3_32(name.as_bytes(), 0)
}

// =============================================================================
// KNOWN SYSCALLS
// =============================================================================

/// Known Solana syscalls with their symbol names
pub struct SolanaSymbols;

impl SolanaSymbols {
    pub const SOL_LOG: &'static str = "sol_log_";
    pub const SOL_LOG_64: &'static str = "sol_log_64_";
    pub const SOL_LOG_COMPUTE_UNITS: &'static str = "sol_log_compute_units_";
    pub const SOL_LOG_PUBKEY: &'static str = "sol_log_pubkey";
    pub const SOL_PANIC: &'static str = "sol_panic_";
    pub const SOL_SHA256: &'static str = "sol_sha256";
    pub const SOL_KECCAK256: &'static str = "sol_keccak256";
    pub const SOL_BLAKE3: &'static str = "sol_blake3";
    pub const SOL_SECP256K1_RECOVER: &'static str = "sol_secp256k1_recover";
    pub const SOL_CREATE_PROGRAM_ADDRESS: &'static str = "sol_create_program_address";
    pub const SOL_TRY_FIND_PROGRAM_ADDRESS: &'static str = "sol_try_find_program_address";
    pub const SOL_INVOKE_SIGNED_C: &'static str = "sol_invoke_signed_c";
    pub const SOL_INVOKE_SIGNED_RUST: &'static str = "sol_invoke_signed_rust";
    pub const SOL_ALLOC_FREE: &'static str = "sol_alloc_free_";
    pub const SOL_MEMCPY: &'static str = "sol_memcpy_";
    pub const SOL_MEMMOVE: &'static str = "sol_memmove_";
    pub const SOL_MEMCMP: &'static str = "sol_memcmp_";
    pub const SOL_MEMSET: &'static str = "sol_memset_";
    pub const SOL_GET_CLOCK_SYSVAR: &'static str = "sol_get_clock_sysvar";
    pub const SOL_GET_RENT_SYSVAR: &'static str = "sol_get_rent_sysvar";
    pub const SOL_GET_EPOCH_SCHEDULE_SYSVAR: &'static str = "sol_get_epoch_schedule_sysvar";

    /// Build a lookup table of hash -> name for decompilation
    pub fn hash_to_name() -> HashMap<u32, &'static str> {
        let names = [
            Self::SOL_LOG, Self::SOL_LOG_64, Self::SOL_LOG_COMPUTE_UNITS,
            Self::SOL_LOG_PUBKEY, Self::SOL_PANIC, Self::SOL_SHA256,
            Self::SOL_KECCAK256, Self::SOL_BLAKE3, Self::SOL_SECP256K1_RECOVER,
            Self::SOL_CREATE_PROGRAM_ADDRESS, Self::SOL_TRY_FIND_PROGRAM_ADDRESS,
            Self::SOL_INVOKE_SIGNED_C, Self::SOL_INVOKE_SIGNED_RUST,
            Self::SOL_ALLOC_FREE, Self::SOL_MEMCPY, Self::SOL_MEMMOVE,
            Self::SOL_MEMCMP, Self::SOL_MEMSET, Self::SOL_GET_CLOCK_SYSVAR,
            Self::SOL_GET_RENT_SYSVAR, Self::SOL_GET_EPOCH_SCHEDULE_SYSVAR,
        ];
        names.iter().map(|&n| (syscall_hash(n), n)).collect()
    }
}

// =============================================================================
// SBPF REGISTERS
// =============================================================================

/// sBPF physical registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SbpfReg {
    R0 = 0,   // Return value
    R1 = 1,   // Arg 1 / caller-saved
    R2 = 2,   // Arg 2 / caller-saved
    R3 = 3,   // Arg 3 / caller-saved
    R4 = 4,   // Arg 4 / caller-saved
    R5 = 5,   // Arg 5 / caller-saved
    R6 = 6,   // Callee-saved
    R7 = 7,   // Callee-saved
    R8 = 8,   // Callee-saved
    R9 = 9,   // Callee-saved
    R10 = 10, // Frame pointer (read-only)
}

impl SbpfReg {
    pub fn is_callee_saved(self) -> bool {
        matches!(self, SbpfReg::R6 | SbpfReg::R7 | SbpfReg::R8 | SbpfReg::R9)
    }

    pub fn is_arg_reg(self) -> bool {
        matches!(self, SbpfReg::R1 | SbpfReg::R2 | SbpfReg::R3 | SbpfReg::R4 | SbpfReg::R5)
    }
}

// =============================================================================
// SBPF INSTRUCTION
// =============================================================================

/// sBPF instruction (8 bytes standard, 16 bytes for lddw)
#[derive(Debug, Clone)]
pub struct SbpfInstruction {
    /// Full opcode byte
    pub opcode: u8,
    /// Destination register (0-10)
    pub dst: u8,
    /// Source register (0-10)
    pub src: u8,
    /// Signed offset for memory/jumps
    pub offset: i16,
    /// 32-bit immediate value
    pub imm: i32,
    /// For lddw: upper 32 bits of 64-bit immediate
    pub imm64_hi: Option<u32>,
}

impl SbpfInstruction {
    /// Create a standard 8-byte instruction
    pub fn new(opcode: u8, dst: u8, src: u8, offset: i16, imm: i32) -> Self {
        Self { opcode, dst, src, offset, imm, imm64_hi: None }
    }

    /// Create a lddw instruction (16 bytes) for 64-bit constant
    pub fn lddw(dst: u8, value: u64) -> Self {
        Self {
            opcode: class::LD | size::DW | mode::IMM,
            dst,
            src: 0,
            offset: 0,
            imm: value as i32,
            imm64_hi: Some((value >> 32) as u32),
        }
    }

    /// ALU64 with immediate
    pub fn alu64_imm(op: u8, dst: u8, imm: i32) -> Self {
        Self::new(class::ALU64 | op | SRC_IMM, dst, 0, 0, imm)
    }

    /// ALU64 with register
    pub fn alu64_reg(op: u8, dst: u8, src: u8) -> Self {
        Self::new(class::ALU64 | op | SRC_REG, dst, src, 0, 0)
    }

    /// Load from memory: dst = *(src + offset)
    pub fn ldx(sz: u8, dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::LDX | sz | mode::MEM, dst, src, offset, 0)
    }

    /// Store to memory: *(dst + offset) = src
    pub fn stx(sz: u8, dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::STX | sz | mode::MEM, dst, src, offset, 0)
    }

    /// Unconditional jump
    pub fn ja(offset: i16) -> Self {
        Self::new(class::JMP | jmp::JA, 0, 0, offset, 0)
    }

    /// Conditional jump with immediate
    pub fn jmp_imm(op: u8, dst: u8, imm: i32, offset: i16) -> Self {
        Self::new(class::JMP | op | SRC_IMM, dst, 0, offset, imm)
    }

    /// Conditional jump with register
    pub fn jmp_reg(op: u8, dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::JMP | op | SRC_REG, dst, src, offset, 0)
    }

    /// Call syscall (hash in imm)
    pub fn call_syscall(hash: u32) -> Self {
        Self::new(class::JMP | jmp::CALL, 0, 0, 0, hash as i32)
    }

    /// Call internal function (relative offset)
    pub fn call_internal(offset: i32) -> Self {
        Self::new(class::JMP | jmp::CALL | SRC_REG, 0, 1, 0, offset)
    }

    /// Exit/return
    pub fn exit() -> Self {
        Self::new(class::JMP | jmp::EXIT, 0, 0, 0, 0)
    }

    // =========================================================================
    // ATOMIC OPERATIONS
    // =========================================================================

    /// Atomic add: *(dst + offset) += src
    pub fn atomic_add(dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::STX | size::DW | mode::ATOMIC, dst, src, offset, atomic::ADD as i32)
    }

    /// Atomic fetch-add: tmp = *(dst + offset); *(dst + offset) += src; src = tmp
    pub fn atomic_fetch_add(dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::STX | size::DW | mode::ATOMIC, dst, src, offset, (atomic::ADD | atomic::FETCH) as i32)
    }

    /// Atomic or: *(dst + offset) |= src
    pub fn atomic_or(dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::STX | size::DW | mode::ATOMIC, dst, src, offset, atomic::OR as i32)
    }

    /// Atomic and: *(dst + offset) &= src
    pub fn atomic_and(dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::STX | size::DW | mode::ATOMIC, dst, src, offset, atomic::AND as i32)
    }

    /// Atomic xor: *(dst + offset) ^= src
    pub fn atomic_xor(dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::STX | size::DW | mode::ATOMIC, dst, src, offset, atomic::XOR as i32)
    }

    /// Atomic exchange: tmp = *(dst + offset); *(dst + offset) = src; src = tmp
    pub fn atomic_xchg(dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::STX | size::DW | mode::ATOMIC, dst, src, offset, atomic::XCHG as i32)
    }

    /// Atomic compare-and-exchange: if *(dst + offset) == R0 { *(dst + offset) = src }
    pub fn atomic_cmpxchg(dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::STX | size::DW | mode::ATOMIC, dst, src, offset, atomic::CMPXCHG as i32)
    }

    // =========================================================================
    // SIGNED JUMPS (for signed comparison)
    // =========================================================================

    /// Signed greater than with immediate
    pub fn jsgt_imm(dst: u8, imm: i32, offset: i16) -> Self {
        Self::new(class::JMP | jmp::JSGT | SRC_IMM, dst, 0, offset, imm)
    }

    /// Signed greater than with register
    pub fn jsgt_reg(dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::JMP | jmp::JSGT | SRC_REG, dst, src, offset, 0)
    }

    /// Signed greater or equal with immediate
    pub fn jsge_imm(dst: u8, imm: i32, offset: i16) -> Self {
        Self::new(class::JMP | jmp::JSGE | SRC_IMM, dst, 0, offset, imm)
    }

    /// Signed greater or equal with register
    pub fn jsge_reg(dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::JMP | jmp::JSGE | SRC_REG, dst, src, offset, 0)
    }

    /// Signed less than with immediate
    pub fn jslt_imm(dst: u8, imm: i32, offset: i16) -> Self {
        Self::new(class::JMP | jmp::JSLT | SRC_IMM, dst, 0, offset, imm)
    }

    /// Signed less than with register
    pub fn jslt_reg(dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::JMP | jmp::JSLT | SRC_REG, dst, src, offset, 0)
    }

    /// Signed less or equal with immediate
    pub fn jsle_imm(dst: u8, imm: i32, offset: i16) -> Self {
        Self::new(class::JMP | jmp::JSLE | SRC_IMM, dst, 0, offset, imm)
    }

    /// Signed less or equal with register
    pub fn jsle_reg(dst: u8, src: u8, offset: i16) -> Self {
        Self::new(class::JMP | jmp::JSLE | SRC_REG, dst, src, offset, 0)
    }

    // =========================================================================
    // BYTESWAP / ENDIANNESS
    // =========================================================================

    /// Byte swap to little-endian (16-bit)
    pub fn le16(dst: u8) -> Self {
        Self::new(class::ALU | alu::END | SRC_IMM, dst, 0, 0, 16)
    }

    /// Byte swap to little-endian (32-bit)
    pub fn le32(dst: u8) -> Self {
        Self::new(class::ALU | alu::END | SRC_IMM, dst, 0, 0, 32)
    }

    /// Byte swap to little-endian (64-bit)
    pub fn le64(dst: u8) -> Self {
        Self::new(class::ALU | alu::END | SRC_IMM, dst, 0, 0, 64)
    }

    /// Byte swap to big-endian (16-bit)
    pub fn be16(dst: u8) -> Self {
        Self::new(class::ALU | alu::END | SRC_REG, dst, 0, 0, 16)
    }

    /// Byte swap to big-endian (32-bit)
    pub fn be32(dst: u8) -> Self {
        Self::new(class::ALU | alu::END | SRC_REG, dst, 0, 0, 32)
    }

    /// Byte swap to big-endian (64-bit)
    pub fn be64(dst: u8) -> Self {
        Self::new(class::ALU | alu::END | SRC_REG, dst, 0, 0, 64)
    }

    // =========================================================================
    // SHIFT OPERATIONS
    // =========================================================================

    /// Left shift with immediate
    pub fn lsh64_imm(dst: u8, imm: i32) -> Self {
        Self::new(class::ALU64 | alu::LSH | SRC_IMM, dst, 0, 0, imm)
    }

    /// Left shift with register
    pub fn lsh64_reg(dst: u8, src: u8) -> Self {
        Self::new(class::ALU64 | alu::LSH | SRC_REG, dst, src, 0, 0)
    }

    /// Logical right shift with immediate
    pub fn rsh64_imm(dst: u8, imm: i32) -> Self {
        Self::new(class::ALU64 | alu::RSH | SRC_IMM, dst, 0, 0, imm)
    }

    /// Logical right shift with register
    pub fn rsh64_reg(dst: u8, src: u8) -> Self {
        Self::new(class::ALU64 | alu::RSH | SRC_REG, dst, src, 0, 0)
    }

    /// Arithmetic right shift with immediate (sign-extending)
    pub fn arsh64_imm(dst: u8, imm: i32) -> Self {
        Self::new(class::ALU64 | alu::ARSH | SRC_IMM, dst, 0, 0, imm)
    }

    /// Arithmetic right shift with register (sign-extending)
    pub fn arsh64_reg(dst: u8, src: u8) -> Self {
        Self::new(class::ALU64 | alu::ARSH | SRC_REG, dst, src, 0, 0)
    }

    /// Encode to bytes (8 or 16 bytes)
    pub fn encode(&self) -> Vec<u8> {
        let mut bytes = vec![0u8; 8];
        bytes[0] = self.opcode;
        bytes[1] = (self.dst & 0xf) | ((self.src & 0xf) << 4);
        bytes[2..4].copy_from_slice(&self.offset.to_le_bytes());
        bytes[4..8].copy_from_slice(&self.imm.to_le_bytes());

        // lddw needs second 8-byte slot
        if let Some(hi) = self.imm64_hi {
            bytes.extend_from_slice(&[0u8; 4]); // padding
            bytes.extend_from_slice(&hi.to_le_bytes());
        }

        bytes
    }

    /// Instruction size in bytes
    pub fn size(&self) -> usize {
        if self.imm64_hi.is_some() { 16 } else { 8 }
    }

    /// Estimate compute units for this instruction
    pub fn compute_cost(&self) -> u64 {
        let op_class = self.opcode & 0x07;
        let op_code = self.opcode & 0xf0;

        match (op_class, op_code) {
            // Moves are cheap
            (class::ALU64, alu::MOV) | (class::ALU, alu::MOV) => 1,
            // Basic arithmetic
            (class::ALU64, alu::ADD) | (class::ALU64, alu::SUB) => 1,
            (class::ALU, alu::ADD) | (class::ALU, alu::SUB) => 1,
            // Multiply is more expensive
            (class::ALU64, alu::MUL) | (class::ALU, alu::MUL) => 4,
            // Division is expensive
            (class::ALU64, alu::DIV) | (class::ALU64, alu::MOD) => 16,
            (class::ALU, alu::DIV) | (class::ALU, alu::MOD) => 16,
            // Memory ops
            (class::LDX, _) | (class::STX, _) => 2,
            // Syscalls vary wildly
            (class::JMP, jmp::CALL) => 100,
            // Exit
            (class::JMP, jmp::EXIT) => 1,
            // Default
            _ => 1,
        }
    }
}

// =============================================================================
// REGISTER ALLOCATOR
// =============================================================================

/// Linear scan register allocator
struct RegisterAllocator {
    /// Virtual -> physical register mapping
    allocation: HashMap<IrReg, SbpfReg>,
    /// Available registers (R6-R9 for general use)
    available: Vec<SbpfReg>,
    /// Spill locations on stack (offset from R10)
    spills: HashMap<IrReg, i16>,
    /// Next spill offset
    next_spill: i16,
    /// Registers that were used (need save/restore)
    used_callee_saved: Vec<SbpfReg>,
}

impl RegisterAllocator {
    fn new() -> Self {
        Self {
            allocation: HashMap::new(),
            available: vec![SbpfReg::R9, SbpfReg::R8, SbpfReg::R7, SbpfReg::R6],
            spills: HashMap::new(),
            next_spill: -8, // Grow downward from frame pointer
            used_callee_saved: Vec::new(),
        }
    }

    /// Allocate a physical register for a virtual register
    fn allocate(&mut self, virt: IrReg) -> SbpfReg {
        // Already allocated?
        if let Some(&phys) = self.allocation.get(&virt) {
            return phys;
        }

        // Try to get a free register
        if let Some(phys) = self.available.pop() {
            if phys.is_callee_saved() && !self.used_callee_saved.contains(&phys) {
                self.used_callee_saved.push(phys);
            }
            self.allocation.insert(virt, phys);
            return phys;
        }

        // Must spill - use R1 as scratch and record spill location
        self.spills.insert(virt, self.next_spill);
        self.next_spill -= 8;
        SbpfReg::R1 // Scratch register
    }

    /// Check if a virtual register is spilled
    fn is_spilled(&self, virt: IrReg) -> bool {
        self.spills.contains_key(&virt)
    }

    /// Get spill offset for a register
    fn spill_offset(&self, virt: IrReg) -> Option<i16> {
        self.spills.get(&virt).copied()
    }

    /// Get stack frame size needed
    fn frame_size(&self) -> i16 {
        (-self.next_spill).max(0)
    }
}

// =============================================================================
// CODE GENERATOR
// =============================================================================

/// sBPF code generator
pub struct SbpfCodegen {
    instructions: Vec<SbpfInstruction>,
    labels: HashMap<String, usize>,
    pending_jumps: Vec<(usize, String)>,
    reg_alloc: RegisterAllocator,
    /// Syscall name -> hash cache
    syscall_cache: HashMap<String, u32>,
}

impl SbpfCodegen {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            labels: HashMap::new(),
            pending_jumps: Vec::new(),
            reg_alloc: RegisterAllocator::new(),
            syscall_cache: HashMap::new(),
        }
    }

    /// Generate sBPF from IR
    pub fn generate(&mut self, ir: &IrProgram) -> Result<Vec<SbpfInstruction>> {
        for ir_instr in &ir.instructions {
            self.gen_instruction(ir_instr)?;
        }

        self.resolve_jumps()?;
        Ok(std::mem::take(&mut self.instructions))
    }

    fn gen_instruction(&mut self, ir: &IrInstruction) -> Result<()> {
        match ir {
            // Constants
            IrInstruction::ConstI64(dst, value) => {
                let dst_reg = self.reg_alloc.allocate(*dst);
                if *value >= i32::MIN as i64 && *value <= i32::MAX as i64 {
                    // Fits in 32-bit immediate
                    self.emit(SbpfInstruction::alu64_imm(alu::MOV, dst_reg as u8, *value as i32));
                } else {
                    // Need lddw for full 64-bit
                    self.emit(SbpfInstruction::lddw(dst_reg as u8, *value as u64));
                }
            }

            IrInstruction::ConstF64(dst, bits) => {
                let dst_reg = self.reg_alloc.allocate(*dst);
                self.emit(SbpfInstruction::lddw(dst_reg as u8, *bits));
            }

            IrInstruction::ConstBool(dst, value) => {
                let dst_reg = self.reg_alloc.allocate(*dst);
                self.emit(SbpfInstruction::alu64_imm(alu::MOV, dst_reg as u8, if *value { 1 } else { 0 }));
            }

            IrInstruction::ConstNull(dst) => {
                let dst_reg = self.reg_alloc.allocate(*dst);
                self.emit(SbpfInstruction::alu64_imm(alu::MOV, dst_reg as u8, 0));
            }

            // Binary arithmetic
            IrInstruction::Add(dst, src1, src2) => self.gen_binop(alu::ADD, *dst, *src1, *src2),
            IrInstruction::Sub(dst, src1, src2) => self.gen_binop(alu::SUB, *dst, *src1, *src2),
            IrInstruction::Mul(dst, src1, src2) => self.gen_binop(alu::MUL, *dst, *src1, *src2),
            IrInstruction::Div(dst, src1, src2) => self.gen_binop(alu::DIV, *dst, *src1, *src2),
            IrInstruction::Mod(dst, src1, src2) => self.gen_binop(alu::MOD, *dst, *src1, *src2),
            IrInstruction::And(dst, src1, src2) => self.gen_binop(alu::AND, *dst, *src1, *src2),
            IrInstruction::Or(dst, src1, src2) => self.gen_binop(alu::OR, *dst, *src1, *src2),

            // Comparisons
            IrInstruction::Eq(dst, src1, src2) => self.gen_compare(jmp::JEQ, *dst, *src1, *src2),
            IrInstruction::Ne(dst, src1, src2) => self.gen_compare(jmp::JNE, *dst, *src1, *src2),
            IrInstruction::Lt(dst, src1, src2) => self.gen_compare(jmp::JLT, *dst, *src1, *src2),
            IrInstruction::Le(dst, src1, src2) => self.gen_compare(jmp::JLE, *dst, *src1, *src2),
            IrInstruction::Gt(dst, src1, src2) => self.gen_compare(jmp::JGT, *dst, *src1, *src2),
            IrInstruction::Ge(dst, src1, src2) => self.gen_compare(jmp::JGE, *dst, *src1, *src2),

            // Unary
            IrInstruction::Neg(dst, src) => {
                let dst_reg = self.reg_alloc.allocate(*dst);
                let src_reg = self.reg_alloc.allocate(*src);
                // neg = 0 - src
                self.emit(SbpfInstruction::alu64_imm(alu::MOV, dst_reg as u8, 0));
                self.emit(SbpfInstruction::alu64_reg(alu::SUB, dst_reg as u8, src_reg as u8));
            }

            IrInstruction::Not(dst, src) => {
                let dst_reg = self.reg_alloc.allocate(*dst);
                let src_reg = self.reg_alloc.allocate(*src);
                // not (bool) = 1 - src, or xor -1 for bitwise
                self.emit(SbpfInstruction::alu64_imm(alu::MOV, dst_reg as u8, 1));
                self.emit(SbpfInstruction::alu64_reg(alu::SUB, dst_reg as u8, src_reg as u8));
            }

            // Move
            IrInstruction::Move(dst, src) => {
                let dst_reg = self.reg_alloc.allocate(*dst);
                let src_reg = self.reg_alloc.allocate(*src);
                self.emit(SbpfInstruction::alu64_reg(alu::MOV, dst_reg as u8, src_reg as u8));
            }

            // Control flow
            IrInstruction::Label(name) => {
                self.labels.insert(name.clone(), self.current_offset());
            }

            IrInstruction::Jump(target) => {
                let idx = self.instructions.len();
                self.pending_jumps.push((idx, target.clone()));
                self.emit(SbpfInstruction::ja(0)); // Placeholder
            }

            IrInstruction::JumpIf(cond, target) => {
                let cond_reg = self.reg_alloc.allocate(*cond);
                let idx = self.instructions.len();
                self.pending_jumps.push((idx, target.clone()));
                // Jump if != 0
                self.emit(SbpfInstruction::jmp_imm(jmp::JNE, cond_reg as u8, 0, 0));
            }

            IrInstruction::JumpIfNot(cond, target) => {
                let cond_reg = self.reg_alloc.allocate(*cond);
                let idx = self.instructions.len();
                self.pending_jumps.push((idx, target.clone()));
                // Jump if == 0
                self.emit(SbpfInstruction::jmp_imm(jmp::JEQ, cond_reg as u8, 0, 0));
            }

            // Function calls
            IrInstruction::Call(dst, name, args) => {
                // Move args to R1-R5
                for (i, arg) in args.iter().enumerate().take(5) {
                    let arg_reg = self.reg_alloc.allocate(*arg);
                    let target = (i + 1) as u8; // R1-R5
                    if arg_reg as u8 != target {
                        self.emit(SbpfInstruction::alu64_reg(alu::MOV, target, arg_reg as u8));
                    }
                }

                // Get syscall hash
                let hash = self.get_syscall_hash(name);
                self.emit(SbpfInstruction::call_syscall(hash));

                // Move result from R0
                if let Some(dst_ir) = dst {
                    let dst_reg = self.reg_alloc.allocate(*dst_ir);
                    if dst_reg != SbpfReg::R0 {
                        self.emit(SbpfInstruction::alu64_reg(alu::MOV, dst_reg as u8, SbpfReg::R0 as u8));
                    }
                }
            }

            IrInstruction::Return(value) => {
                if let Some(val_reg) = value {
                    let src_reg = self.reg_alloc.allocate(*val_reg);
                    if src_reg != SbpfReg::R0 {
                        self.emit(SbpfInstruction::alu64_reg(alu::MOV, SbpfReg::R0 as u8, src_reg as u8));
                    }
                }
                self.emit(SbpfInstruction::exit());
            }

            // Memory
            IrInstruction::Load(dst, base, offset) => {
                let dst_reg = self.reg_alloc.allocate(*dst);
                let base_reg = self.reg_alloc.allocate(*base);
                self.emit(SbpfInstruction::ldx(size::DW, dst_reg as u8, base_reg as u8, *offset as i16));
            }

            IrInstruction::Store(base, src, offset) => {
                let base_reg = self.reg_alloc.allocate(*base);
                let src_reg = self.reg_alloc.allocate(*src);
                self.emit(SbpfInstruction::stx(size::DW, base_reg as u8, src_reg as u8, *offset as i16));
            }

            _ => {} // Unhandled
        }

        Ok(())
    }

    /// Generate binary operation: dst = src1 op src2
    fn gen_binop(&mut self, op: u8, dst: IrReg, src1: IrReg, src2: IrReg) {
        let dst_reg = self.reg_alloc.allocate(dst);
        let src1_reg = self.reg_alloc.allocate(src1);
        let src2_reg = self.reg_alloc.allocate(src2);

        // dst = src1
        self.emit(SbpfInstruction::alu64_reg(alu::MOV, dst_reg as u8, src1_reg as u8));
        // dst op= src2
        self.emit(SbpfInstruction::alu64_reg(op, dst_reg as u8, src2_reg as u8));
    }

    /// Generate comparison: dst = (src1 cmp src2) ? 1 : 0
    fn gen_compare(&mut self, cmp_op: u8, dst: IrReg, src1: IrReg, src2: IrReg) {
        let dst_reg = self.reg_alloc.allocate(dst);
        let src1_reg = self.reg_alloc.allocate(src1);
        let src2_reg = self.reg_alloc.allocate(src2);

        // Strategy: dst = 0, then conditionally set to 1
        // if (src1 cmp src2) { dst = 1 } else { dst = 0 }
        //
        // sBPF:
        //   mov dst, 0          ; assume false
        //   jxx src1, src2, +1  ; if condition TRUE, skip the jump-over
        //   ja +1               ; skip the "set to 1" (condition was FALSE)
        //   mov dst, 1          ; condition was TRUE

        // dst = 0 (default: false)
        self.emit(SbpfInstruction::alu64_imm(alu::MOV, dst_reg as u8, 0));
        // if condition is TRUE, jump over the unconditional jump
        self.emit(SbpfInstruction::jmp_reg(cmp_op, src1_reg as u8, src2_reg as u8, 1));
        // condition was FALSE, skip setting dst=1
        self.emit(SbpfInstruction::ja(1));
        // condition was TRUE, set dst=1
        self.emit(SbpfInstruction::alu64_imm(alu::MOV, dst_reg as u8, 1));
    }

    fn emit(&mut self, instr: SbpfInstruction) {
        self.instructions.push(instr);
    }

    fn current_offset(&self) -> usize {
        self.instructions.iter().map(|i| i.size()).sum::<usize>() / 8
    }

    fn get_syscall_hash(&mut self, name: &str) -> u32 {
        if let Some(&hash) = self.syscall_cache.get(name) {
            return hash;
        }

        // Map OVSM function names to Solana syscalls
        let syscall_name = match name {
            "log" => SolanaSymbols::SOL_LOG,
            "length" | "len" => return 0, // Built-in, not syscall
            _ => name,
        };

        let hash = syscall_hash(syscall_name);
        self.syscall_cache.insert(name.to_string(), hash);
        hash
    }

    fn resolve_jumps(&mut self) -> Result<()> {
        for (instr_idx, target) in &self.pending_jumps {
            let target_offset = self.labels.get(target)
                .ok_or_else(|| Error::runtime(format!("Undefined label: {}", target)))?;

            // Calculate instruction offset (in 8-byte units)
            let current_offset: usize = self.instructions[..*instr_idx]
                .iter()
                .map(|i| i.size() / 8)
                .sum();

            let offset = (*target_offset as i64) - (current_offset as i64) - 1;

            if offset > i16::MAX as i64 || offset < i16::MIN as i64 {
                return Err(Error::runtime(format!("Jump offset too large: {}", offset)));
            }

            self.instructions[*instr_idx].offset = offset as i16;
        }

        Ok(())
    }
}

impl Default for SbpfCodegen {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// TESTS
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_murmur3_sol_log() {
        // Known hash for sol_log_
        let hash = syscall_hash("sol_log_");
        // The actual hash value - verify against Solana runtime
        assert_ne!(hash, 0);
    }

    #[test]
    fn test_instruction_encoding() {
        // mov64 r0, 42
        let instr = SbpfInstruction::alu64_imm(alu::MOV, 0, 42);
        let bytes = instr.encode();
        assert_eq!(bytes.len(), 8);
        assert_eq!(bytes[0], class::ALU64 | alu::MOV | SRC_IMM); // 0xb7
    }

    #[test]
    fn test_lddw_encoding() {
        let instr = SbpfInstruction::lddw(0, 0x123456789ABCDEF0);
        let bytes = instr.encode();
        assert_eq!(bytes.len(), 16); // lddw is 16 bytes
    }

    #[test]
    fn test_register_allocation() {
        let mut alloc = RegisterAllocator::new();
        let r1 = alloc.allocate(IrReg(0));
        let r2 = alloc.allocate(IrReg(1));
        assert_ne!(r1, r2);

        // Same virtual reg should return same physical reg
        let r1_again = alloc.allocate(IrReg(0));
        assert_eq!(r1, r1_again);
    }
}
