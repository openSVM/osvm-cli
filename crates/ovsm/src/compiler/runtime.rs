//! # Runtime Support for Compiled OVSM Programs
//!
//! Provides stack frame management, heap allocation, and runtime data structures
//! for programs compiled to sBPF.

use super::sbpf_codegen::{memory, syscall_hash, SbpfInstruction, SbpfReg, SolanaSymbols};
use super::SbpfVersion;

/// Stack frame layout for OVSM functions
///
/// ```text
/// High addresses
/// +------------------+ <- Previous R10 (caller's frame pointer)
/// | Return address   | [R10 - 8]
/// | Saved R6         | [R10 - 16]
/// | Saved R7         | [R10 - 24]
/// | Saved R8         | [R10 - 32]
/// | Saved R9         | [R10 - 40]
/// | Local var 0      | [R10 - 48]
/// | Local var 1      | [R10 - 56]
/// | ...              |
/// +------------------+ <- Current R10 (this frame's base)
/// Low addresses
/// ```
pub struct StackFrame {
    /// Offset for next local variable (grows negative from R10)
    next_local_offset: i16,
    /// Number of callee-saved registers to preserve
    saved_regs: Vec<SbpfReg>,
    /// Total frame size
    frame_size: i16,
}

impl StackFrame {
    /// Reserved space for return address + saved registers
    const RESERVED_SIZE: i16 = 40; // 8 bytes each: ret addr + R6-R9

    /// Create a new stack frame with space for callee-saved registers
    pub fn new() -> Self {
        Self {
            next_local_offset: -Self::RESERVED_SIZE - 8,
            saved_regs: vec![SbpfReg::R6, SbpfReg::R7, SbpfReg::R8, SbpfReg::R9],
            frame_size: Self::RESERVED_SIZE,
        }
    }

    /// Allocate space for a local variable, return its offset from R10
    pub fn alloc_local(&mut self) -> i16 {
        let offset = self.next_local_offset;
        self.next_local_offset -= 8;
        self.frame_size += 8;
        offset
    }

    /// Generate function prologue (save registers, setup frame)
    pub fn gen_prologue(&self) -> Vec<SbpfInstruction> {
        let mut instrs = Vec::new();

        // Save callee-saved registers to stack
        // R10 points to current frame, we save below it
        let mut offset: i16 = -8;

        for &reg in &self.saved_regs {
            // stxdw [r10 + offset], reg
            instrs.push(SbpfInstruction::stx(
                0x18,
                SbpfReg::R10 as u8,
                reg as u8,
                offset,
            ));
            offset -= 8;
        }

        instrs
    }

    /// Generate function epilogue (restore registers, return)
    pub fn gen_epilogue(&self) -> Vec<SbpfInstruction> {
        let mut instrs = Vec::new();

        // Restore callee-saved registers
        let mut offset: i16 = -8;

        for &reg in &self.saved_regs {
            // ldxdw reg, [r10 + offset]
            instrs.push(SbpfInstruction::ldx(
                0x18,
                reg as u8,
                SbpfReg::R10 as u8,
                offset,
            ));
            offset -= 8;
        }

        // Exit
        instrs.push(SbpfInstruction::exit());

        instrs
    }

    /// Get frame size in bytes
    pub fn size(&self) -> i16 {
        self.frame_size
    }
}

impl Default for StackFrame {
    fn default() -> Self {
        Self::new()
    }
}

/// Heap allocator interface
///
/// Uses sol_alloc_free_ syscall for dynamic memory allocation.
/// Solana heap is 32KB max, arena-style (no deallocation).
pub struct HeapAllocator {
    /// Syscall hash for sol_alloc_free_
    alloc_hash: u32,
}

impl HeapAllocator {
    /// Create a new heap allocator using sol_alloc_free_ syscall
    pub fn new() -> Self {
        Self {
            alloc_hash: syscall_hash(SolanaSymbols::SOL_ALLOC_FREE),
        }
    }

    /// Generate code to allocate `size` bytes on heap
    /// Result pointer is returned in R0
    ///
    /// Args: R1 = size in bytes
    /// Returns: R0 = pointer to allocated memory (or 0 on failure)
    pub fn gen_alloc(&self, size_reg: u8) -> Vec<SbpfInstruction> {
        let mut instrs = Vec::new();

        // Move size to R1 if not already there
        if size_reg != SbpfReg::R1 as u8 {
            instrs.push(SbpfInstruction::alu64_reg(
                0xb0,
                SbpfReg::R1 as u8,
                size_reg,
            ));
        }

        // R2 = 0 (allocate, not free)
        instrs.push(SbpfInstruction::alu64_imm(0xb0, SbpfReg::R2 as u8, 0));

        // Call sol_alloc_free_
        instrs.push(SbpfInstruction::call_syscall(
            self.alloc_hash,
            SbpfVersion::V1,
        ));

        // Result is in R0

        instrs
    }

    /// Generate code to allocate a fixed size
    pub fn gen_alloc_const(&self, size: i32) -> Vec<SbpfInstruction> {
        vec![
            // R1 = size
            SbpfInstruction::alu64_imm(0xb0, SbpfReg::R1 as u8, size),
            // R2 = 0 (allocate)
            SbpfInstruction::alu64_imm(0xb0, SbpfReg::R2 as u8, 0),
            // Call sol_alloc_free_
            SbpfInstruction::call_syscall(self.alloc_hash, SbpfVersion::V1),
        ]
    }
}

impl Default for HeapAllocator {
    fn default() -> Self {
        Self::new()
    }
}

/// String runtime representation
///
/// Heap layout: [length: u64][data: u8...]
pub struct StringRuntime {
    heap: HeapAllocator,
    memcpy_hash: u32,
    memcmp_hash: u32,
}

impl StringRuntime {
    /// Create a new string runtime with heap allocator and memory syscalls
    pub fn new() -> Self {
        Self {
            heap: HeapAllocator::new(),
            memcpy_hash: syscall_hash(SolanaSymbols::SOL_MEMCPY),
            memcmp_hash: syscall_hash(SolanaSymbols::SOL_MEMCMP),
        }
    }

    /// Generate code to create a string from static data
    /// Assumes string data is at `data_ptr` with `length` bytes
    ///
    /// Returns pointer to heap-allocated string in R0
    pub fn gen_string_create(&self, length: i32) -> Vec<SbpfInstruction> {
        let mut instrs = Vec::new();

        // Allocate length + 8 bytes (8 for length prefix)
        let total_size = length + 8;
        instrs.extend(self.heap.gen_alloc_const(total_size));

        // R0 now has pointer. Store length at [R0]
        // First save R0 to R6 (callee-saved)
        instrs.push(SbpfInstruction::alu64_reg(
            0xb0,
            SbpfReg::R6 as u8,
            SbpfReg::R0 as u8,
        ));

        // Store length: R1 = length, then stxdw [R0], R1
        instrs.push(SbpfInstruction::alu64_imm(0xb0, SbpfReg::R1 as u8, length));
        instrs.push(SbpfInstruction::stx(
            0x18,
            SbpfReg::R0 as u8,
            SbpfReg::R1 as u8,
            0,
        ));

        // Move result back to R0
        instrs.push(SbpfInstruction::alu64_reg(
            0xb0,
            SbpfReg::R0 as u8,
            SbpfReg::R6 as u8,
        ));

        instrs
    }

    /// Generate code to get string length
    /// Input: string pointer in `src_reg`
    /// Output: length in `dst_reg`
    pub fn gen_string_length(&self, dst_reg: u8, src_reg: u8) -> Vec<SbpfInstruction> {
        vec![
            // ldxdw dst, [src + 0]
            SbpfInstruction::ldx(0x18, dst_reg, src_reg, 0),
        ]
    }

    /// Generate memcpy call
    /// R1 = dst, R2 = src, R3 = len
    pub fn gen_memcpy(&self) -> Vec<SbpfInstruction> {
        vec![SbpfInstruction::call_syscall(
            self.memcpy_hash,
            SbpfVersion::V1,
        )]
    }

    /// Generate memcmp call
    /// R1 = s1, R2 = s2, R3 = len
    /// Returns comparison result in R0
    pub fn gen_memcmp(&self) -> Vec<SbpfInstruction> {
        vec![SbpfInstruction::call_syscall(
            self.memcmp_hash,
            SbpfVersion::V1,
        )]
    }
}

impl Default for StringRuntime {
    fn default() -> Self {
        Self::new()
    }
}

/// Array runtime representation
///
/// Heap layout: `[length: u64][capacity: u64][element_size: u64][data...]`
pub struct ArrayRuntime {
    heap: HeapAllocator,
}

impl ArrayRuntime {
    /// Header size: length + capacity + element_size
    const HEADER_SIZE: i32 = 24;

    /// Create a new array runtime with heap allocator
    pub fn new() -> Self {
        Self {
            heap: HeapAllocator::new(),
        }
    }

    /// Generate code to create array with given capacity and element size
    pub fn gen_array_create(&self, capacity: i32, elem_size: i32) -> Vec<SbpfInstruction> {
        let mut instrs = Vec::new();

        let total_size = Self::HEADER_SIZE + (capacity * elem_size);
        instrs.extend(self.heap.gen_alloc_const(total_size));

        // R0 = pointer to array
        // Save to R6
        instrs.push(SbpfInstruction::alu64_reg(
            0xb0,
            SbpfReg::R6 as u8,
            SbpfReg::R0 as u8,
        ));

        // Store length = 0 at [R0]
        instrs.push(SbpfInstruction::alu64_imm(0xb0, SbpfReg::R1 as u8, 0));
        instrs.push(SbpfInstruction::stx(
            0x18,
            SbpfReg::R0 as u8,
            SbpfReg::R1 as u8,
            0,
        ));

        // Store capacity at [R0 + 8]
        instrs.push(SbpfInstruction::alu64_imm(
            0xb0,
            SbpfReg::R1 as u8,
            capacity,
        ));
        instrs.push(SbpfInstruction::stx(
            0x18,
            SbpfReg::R0 as u8,
            SbpfReg::R1 as u8,
            8,
        ));

        // Store elem_size at [R0 + 16]
        instrs.push(SbpfInstruction::alu64_imm(
            0xb0,
            SbpfReg::R1 as u8,
            elem_size,
        ));
        instrs.push(SbpfInstruction::stx(
            0x18,
            SbpfReg::R0 as u8,
            SbpfReg::R1 as u8,
            16,
        ));

        // Restore pointer to R0
        instrs.push(SbpfInstruction::alu64_reg(
            0xb0,
            SbpfReg::R0 as u8,
            SbpfReg::R6 as u8,
        ));

        instrs
    }

    /// Generate code to get array length
    pub fn gen_array_length(&self, dst_reg: u8, arr_reg: u8) -> Vec<SbpfInstruction> {
        vec![SbpfInstruction::ldx(0x18, dst_reg, arr_reg, 0)]
    }

    /// Generate code to get array element
    /// arr_reg = array pointer, idx_reg = index
    /// Result in dst_reg
    pub fn gen_array_get(&self, dst_reg: u8, arr_reg: u8, idx_reg: u8) -> Vec<SbpfInstruction> {
        vec![
            // Load elem_size from [arr + 16]
            SbpfInstruction::ldx(0x18, SbpfReg::R1 as u8, arr_reg, 16),
            // offset = idx * elem_size
            SbpfInstruction::alu64_reg(0xb0, SbpfReg::R2 as u8, idx_reg),
            SbpfInstruction::alu64_reg(0x20, SbpfReg::R2 as u8, SbpfReg::R1 as u8), // mul
            // addr = arr + HEADER_SIZE + offset
            SbpfInstruction::alu64_reg(0xb0, SbpfReg::R3 as u8, arr_reg),
            SbpfInstruction::alu64_imm(0x00, SbpfReg::R3 as u8, Self::HEADER_SIZE), // add
            SbpfInstruction::alu64_reg(0x00, SbpfReg::R3 as u8, SbpfReg::R2 as u8), // add
            // Load value from [R3]
            SbpfInstruction::ldx(0x18, dst_reg, SbpfReg::R3 as u8, 0),
        ]
    }

    /// Generate code to set array element
    pub fn gen_array_set(&self, arr_reg: u8, idx_reg: u8, val_reg: u8) -> Vec<SbpfInstruction> {
        vec![
            // Load elem_size
            SbpfInstruction::ldx(0x18, SbpfReg::R1 as u8, arr_reg, 16),
            // offset = idx * elem_size
            SbpfInstruction::alu64_reg(0xb0, SbpfReg::R2 as u8, idx_reg),
            SbpfInstruction::alu64_reg(0x20, SbpfReg::R2 as u8, SbpfReg::R1 as u8),
            // addr = arr + HEADER_SIZE + offset
            SbpfInstruction::alu64_reg(0xb0, SbpfReg::R3 as u8, arr_reg),
            SbpfInstruction::alu64_imm(0x00, SbpfReg::R3 as u8, Self::HEADER_SIZE),
            SbpfInstruction::alu64_reg(0x00, SbpfReg::R3 as u8, SbpfReg::R2 as u8),
            // Store value at [R3]
            SbpfInstruction::stx(0x18, SbpfReg::R3 as u8, val_reg, 0),
        ]
    }
}

impl Default for ArrayRuntime {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stack_frame_alloc() {
        let mut frame = StackFrame::new();
        let off1 = frame.alloc_local();
        let off2 = frame.alloc_local();
        assert!(off1 > off2); // Growing downward (more negative)
        assert!(off1 < 0);
        assert!(off2 < 0);
    }

    #[test]
    fn test_stack_frame_prologue() {
        let frame = StackFrame::new();
        let prologue = frame.gen_prologue();
        assert_eq!(prologue.len(), 4); // Save R6-R9
    }

    #[test]
    fn test_heap_alloc() {
        let heap = HeapAllocator::new();
        let instrs = heap.gen_alloc_const(64);
        assert!(!instrs.is_empty());
        // Should end with a call instruction
        assert_eq!(instrs.last().unwrap().opcode & 0xf0, 0x80);
    }

    #[test]
    fn test_array_runtime() {
        let arr = ArrayRuntime::new();
        let instrs = arr.gen_array_create(10, 8);
        assert!(!instrs.is_empty());
    }
}
