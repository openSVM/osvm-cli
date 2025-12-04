//! # sBPF Program Verifier
//!
//! Validates compiled sBPF programs before deployment.
//! Ensures programs meet Solana runtime constraints.

use super::sbpf_codegen::{memory, SbpfInstruction};
use crate::{Error, Result};

/// Verification result with warnings
#[derive(Debug)]
pub struct VerifyResult {
    /// Program is valid for deployment
    pub valid: bool,
    /// Errors that prevent deployment
    pub errors: Vec<VerifyError>,
    /// Warnings (non-fatal)
    pub warnings: Vec<String>,
    /// Statistics
    pub stats: ProgramStats,
}

/// Program statistics
#[derive(Debug, Default)]
pub struct ProgramStats {
    /// Total instruction count
    pub instruction_count: usize,
    /// Total bytecode size in bytes
    pub bytecode_size: usize,
    /// Estimated compute units
    pub estimated_cu: u64,
    /// Maximum stack depth (static analysis)
    pub max_stack_depth: usize,
    /// Number of syscalls
    pub syscall_count: usize,
    /// Number of internal calls
    pub internal_call_count: usize,
}

/// Verification error types
#[derive(Debug, Clone)]
pub enum VerifyError {
    /// Program exceeds instruction limit.
    TooManyInstructions {
        /// Actual instruction count in the program
        count: usize,
        /// Maximum allowed instructions
        limit: usize,
    },

    /// Program exceeds bytecode size limit.
    BytecodeTooLarge {
        /// Actual bytecode size in bytes
        size: usize,
        /// Maximum allowed bytecode size in bytes
        limit: usize,
    },

    /// Call depth exceeds limit.
    CallDepthExceeded {
        /// Detected call depth
        depth: usize,
        /// Maximum allowed call depth
        limit: usize,
    },

    /// Invalid opcode encountered.
    InvalidOpcode {
        /// Byte offset of the invalid instruction
        offset: usize,
        /// The invalid opcode value
        opcode: u8,
    },

    /// Jump target out of bounds.
    JumpOutOfBounds {
        /// Byte offset of the jump instruction
        offset: usize,
        /// Target slot position (negative or beyond program end)
        target: i64,
    },

    /// Invalid register number.
    InvalidRegister {
        /// Byte offset of the instruction
        offset: usize,
        /// Invalid register number (must be 0-10)
        reg: u8,
    },

    /// Division by zero possible.
    PossibleDivisionByZero {
        /// Byte offset of the division/modulo instruction
        offset: usize,
    },

    /// Memory access out of bounds.
    MemoryAccessOutOfBounds {
        /// Byte offset of the memory access instruction
        offset: usize,
        /// Out-of-bounds memory address
        address: u64,
    },

    /// Missing exit instruction.
    ///
    /// All sBPF programs must end with an exit instruction (opcode 0x95).
    NoExitInstruction,

    /// Unreachable code detected.
    UnreachableCode {
        /// Byte offset of the unreachable code
        offset: usize,
    },
}

impl std::fmt::Display for VerifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerifyError::TooManyInstructions { count, limit } => {
                write!(f, "Too many instructions: {} (limit: {})", count, limit)
            }
            VerifyError::BytecodeTooLarge { size, limit } => {
                write!(f, "Bytecode too large: {} bytes (limit: {})", size, limit)
            }
            VerifyError::CallDepthExceeded { depth, limit } => {
                write!(f, "Call depth exceeded: {} (limit: {})", depth, limit)
            }
            VerifyError::InvalidOpcode { offset, opcode } => {
                write!(f, "Invalid opcode 0x{:02x} at offset {}", opcode, offset)
            }
            VerifyError::JumpOutOfBounds { offset, target } => {
                write!(
                    f,
                    "Jump at offset {} targets out of bounds: {}",
                    offset, target
                )
            }
            VerifyError::InvalidRegister { offset, reg } => {
                write!(f, "Invalid register {} at offset {}", reg, offset)
            }
            VerifyError::PossibleDivisionByZero { offset } => {
                write!(f, "Possible division by zero at offset {}", offset)
            }
            VerifyError::MemoryAccessOutOfBounds { offset, address } => {
                write!(
                    f,
                    "Memory access out of bounds at offset {}: address 0x{:x}",
                    offset, address
                )
            }
            VerifyError::NoExitInstruction => {
                write!(f, "Program has no exit instruction")
            }
            VerifyError::UnreachableCode { offset } => {
                write!(f, "Unreachable code at offset {}", offset)
            }
        }
    }
}

/// sBPF program verifier
pub struct Verifier {
    /// Maximum allowed instructions
    max_instructions: usize,
    /// Maximum call depth
    max_call_depth: usize,
    /// Strict mode (treat warnings as errors)
    strict: bool,
}

impl Verifier {
    /// Creates a new verifier with default Solana runtime limits.
    ///
    /// Defaults:
    /// - `max_instructions`: Set from `memory::MAX_INSTRUCTIONS`
    /// - `max_call_depth`: Set from `memory::MAX_CALL_DEPTH`
    /// - `strict`: `false` (warnings don't fail verification)
    pub fn new() -> Self {
        Self {
            max_instructions: memory::MAX_INSTRUCTIONS,
            max_call_depth: memory::MAX_CALL_DEPTH,
            strict: false,
        }
    }

    /// Enable strict mode
    pub fn strict(mut self) -> Self {
        self.strict = true;
        self
    }

    /// Set custom instruction limit
    pub fn max_instructions(mut self, limit: usize) -> Self {
        self.max_instructions = limit;
        self
    }

    /// Verify a program
    pub fn verify(&self, program: &[SbpfInstruction]) -> VerifyResult {
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        // Calculate basic stats
        let mut stats = ProgramStats {
            instruction_count: program.len(),
            bytecode_size: program.iter().map(|i| i.size()).sum(),
            estimated_cu: program.iter().map(|i| i.compute_cost()).sum(),
            ..ProgramStats::default()
        };

        // Check instruction count limit
        if stats.instruction_count > self.max_instructions {
            errors.push(VerifyError::TooManyInstructions {
                count: stats.instruction_count,
                limit: self.max_instructions,
            });
        }

        // Check bytecode size (512KB max)
        let max_bytecode = self.max_instructions * 8;
        if stats.bytecode_size > max_bytecode {
            errors.push(VerifyError::BytecodeTooLarge {
                size: stats.bytecode_size,
                limit: max_bytecode,
            });
        }

        // Verify each instruction
        let mut has_exit = false;
        let mut offset = 0;

        // Build slot-to-instruction mapping for proper jump validation
        // LDDW instructions take 2 slots, all others take 1 slot
        let mut slot_positions: Vec<usize> = Vec::with_capacity(program.len());
        let mut current_slot: usize = 0;
        for instr in program.iter() {
            slot_positions.push(current_slot);
            current_slot += instr.size() / 8; // LDDW = 2, others = 1
        }
        let total_slots = current_slot;

        for (idx, instr) in program.iter().enumerate() {
            // Check for exit instruction
            if instr.opcode == 0x95 {
                has_exit = true;
            }

            // Check register numbers (must be 0-10)
            if instr.dst > 10 {
                errors.push(VerifyError::InvalidRegister {
                    offset,
                    reg: instr.dst,
                });
            }
            if instr.src > 10 {
                errors.push(VerifyError::InvalidRegister {
                    offset,
                    reg: instr.src,
                });
            }

            // Check jump targets using slot-based coordinates
            // sBPF jump semantics: PC = current_slot + 1 + offset
            // LDDW advances PC by 2, others by 1
            if self.is_jump_opcode(instr.opcode) && instr.opcode != 0x95 {
                let current_slot_pos = slot_positions[idx];
                let target_slot = current_slot_pos as i64 + 1 + instr.offset as i64;
                if target_slot < 0 || target_slot as usize > total_slots {
                    errors.push(VerifyError::JumpOutOfBounds {
                        offset,
                        target: target_slot,
                    });
                }
            }

            // Count calls
            if instr.opcode == 0x85 {
                if instr.src == 0 {
                    stats.syscall_count += 1;
                } else {
                    stats.internal_call_count += 1;
                }
            }

            // Check for division by immediate zero
            // DIV64 imm = 0x37, MOD64 imm = 0x97
            // DIV32 imm = 0x34, MOD32 imm = 0x94
            let is_div_or_mod_imm = matches!(instr.opcode, 0x34 | 0x37 | 0x94 | 0x97);
            if is_div_or_mod_imm && instr.imm == 0 {
                errors.push(VerifyError::PossibleDivisionByZero { offset });
            }

            offset += instr.size();
        }

        // Check for exit instruction
        if !has_exit && !program.is_empty() {
            errors.push(VerifyError::NoExitInstruction);
        }

        // Estimate max call depth (simplified - assumes worst case)
        stats.max_stack_depth = if stats.internal_call_count > 0 {
            stats.internal_call_count.min(self.max_call_depth)
        } else {
            1
        };

        // Check call depth
        if stats.internal_call_count > self.max_call_depth {
            warnings.push(format!(
                "High internal call count ({}) may exceed call depth limit ({})",
                stats.internal_call_count, self.max_call_depth
            ));
        }

        // High CU warning
        if stats.estimated_cu > 200_000 {
            warnings.push(format!(
                "High estimated compute units: {} (default budget: 200,000)",
                stats.estimated_cu
            ));
        }

        let valid = errors.is_empty() && (!self.strict || warnings.is_empty());

        VerifyResult {
            valid,
            errors,
            warnings,
            stats,
        }
    }

    fn is_jump_opcode(&self, opcode: u8) -> bool {
        let class = opcode & 0x07;
        class == 0x05 || class == 0x06 // JMP or JMP32
    }
}

impl Default for Verifier {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_program() {
        let verifier = Verifier::new();
        let result = verifier.verify(&[]);
        assert!(result.valid);
        assert_eq!(result.stats.instruction_count, 0);
    }

    #[test]
    fn test_simple_valid_program() {
        let verifier = Verifier::new();
        let program = vec![
            SbpfInstruction::alu64_imm(0xb0, 0, 42), // mov64 r0, 42
            SbpfInstruction::exit(),
        ];
        let result = verifier.verify(&program);
        // Debug output
        if !result.valid {
            for err in &result.errors {
                eprintln!("Error: {:?}", err);
            }
        }
        assert!(result.valid, "Errors: {:?}", result.errors);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_no_exit_error() {
        let verifier = Verifier::new();
        let program = vec![SbpfInstruction::alu64_imm(0xb0, 0, 42)];
        let result = verifier.verify(&program);
        assert!(!result.valid);
        assert!(result
            .errors
            .iter()
            .any(|e| matches!(e, VerifyError::NoExitInstruction)));
    }

    #[test]
    fn test_invalid_register() {
        let verifier = Verifier::new();
        let program = vec![
            SbpfInstruction::new(0xb7, 15, 0, 0, 42), // Invalid dst register
            SbpfInstruction::exit(),
        ];
        let result = verifier.verify(&program);
        assert!(!result.valid);
        assert!(result
            .errors
            .iter()
            .any(|e| matches!(e, VerifyError::InvalidRegister { .. })));
    }

    #[test]
    fn test_division_by_zero_detection() {
        let verifier = Verifier::new();
        let program = vec![
            SbpfInstruction::new(0x37, 0, 0, 0, 0), // div64 r0, 0
            SbpfInstruction::exit(),
        ];
        let result = verifier.verify(&program);
        assert!(!result.valid);
        assert!(result
            .errors
            .iter()
            .any(|e| matches!(e, VerifyError::PossibleDivisionByZero { .. })));
    }
}
