//! # OVSM Compiler - LISP to sBPF Bytecode
//!
//! This module compiles OVSM LISP programs to Solana BPF (sBPF) bytecode
//! that can be deployed and executed on the Solana blockchain.
//!
//! ## Architecture
//!
//! ```text
//! OVSM Source → AST → Type Check → IR → Optimize → sBPF → ELF (.so)
//! ```
//!
//! ## Usage
//!
//! ```ignore
//! use ovsm::compiler::{Compiler, CompileOptions};
//!
//! let source = "(define x 42)";
//! let compiler = Compiler::new(CompileOptions::default());
//! let elf_bytes = compiler.compile(source)?;
//! std::fs::write("program.so", elf_bytes)?;
//! ```

pub mod types;
pub mod ir;
pub mod optimizer;
pub mod sbpf_codegen;
pub mod elf;
pub mod verifier;
pub mod runtime;

pub use types::{OvsmType, TypeChecker, TypeEnv};
pub use ir::{IrProgram, IrInstruction, IrReg, IrGenerator};
pub use optimizer::Optimizer;
pub use sbpf_codegen::{SbpfCodegen, SbpfInstruction, SbpfReg, memory, syscall_hash, SolanaSymbols};
pub use elf::ElfWriter;
pub use verifier::{Verifier, VerifyResult, VerifyError};
pub use runtime::{StackFrame, HeapAllocator, StringRuntime, ArrayRuntime};

use crate::{Scanner, Parser, Program, Result, Error};

/// Compilation options
#[derive(Debug, Clone)]
pub struct CompileOptions {
    /// Optimization level (0-3)
    pub opt_level: u8,
    /// Target compute budget (for CU estimation)
    pub compute_budget: u64,
    /// Emit debug info
    pub debug_info: bool,
    /// Generate source map
    pub source_map: bool,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            opt_level: 2,
            compute_budget: 200_000,
            debug_info: false,
            source_map: false,
        }
    }
}

/// Compilation result with metadata
#[derive(Debug)]
pub struct CompileResult {
    /// ELF binary bytes (ready to deploy)
    pub elf_bytes: Vec<u8>,
    /// Estimated compute units
    pub estimated_cu: u64,
    /// Number of IR instructions
    pub ir_instruction_count: usize,
    /// Number of sBPF instructions
    pub sbpf_instruction_count: usize,
    /// Warnings generated during compilation
    pub warnings: Vec<String>,
    /// Verification result
    pub verification: Option<VerifyResult>,
}

/// OVSM to sBPF Compiler
pub struct Compiler {
    options: CompileOptions,
}

impl Compiler {
    /// Create a new compiler with options
    pub fn new(options: CompileOptions) -> Self {
        Self { options }
    }

    /// Compile OVSM source code to ELF binary
    pub fn compile(&self, source: &str) -> Result<CompileResult> {
        // Phase 1: Parse
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let program = parser.parse()?;

        // Phase 2: Type check
        let mut type_checker = TypeChecker::new();
        let typed_program = type_checker.check(&program)?;

        // Phase 3: Generate IR
        let mut ir_gen = IrGenerator::new();
        let mut ir_program = ir_gen.generate(&typed_program)?;

        // Phase 4: Optimize
        if self.options.opt_level > 0 {
            let mut optimizer = Optimizer::new(self.options.opt_level);
            optimizer.optimize(&mut ir_program);
        }

        // Phase 5: Generate sBPF
        let mut codegen = SbpfCodegen::new();
        let sbpf_program = codegen.generate(&ir_program)?;

        // Phase 6: Verify
        let verifier = Verifier::new();
        let verification = verifier.verify(&sbpf_program);

        // Check for fatal verification errors
        if !verification.valid {
            let error_msgs: Vec<String> = verification.errors.iter()
                .map(|e| e.to_string())
                .collect();
            return Err(Error::compiler(format!(
                "Verification failed: {}",
                error_msgs.join("; ")
            )));
        }

        // Phase 7: Package as ELF
        let mut elf_writer = ElfWriter::new();
        let elf_bytes = elf_writer.write(&sbpf_program, self.options.debug_info)?;

        // Combine warnings
        let mut warnings = type_checker.warnings().to_vec();
        warnings.extend(verification.warnings.clone());

        Ok(CompileResult {
            elf_bytes,
            estimated_cu: verification.stats.estimated_cu,
            ir_instruction_count: ir_program.instructions.len(),
            sbpf_instruction_count: sbpf_program.len(),
            warnings,
            verification: Some(verification),
        })
    }

    /// Compile from already-parsed AST
    pub fn compile_ast(&self, program: &Program) -> Result<CompileResult> {
        let mut type_checker = TypeChecker::new();
        let typed_program = type_checker.check(program)?;

        let mut ir_gen = IrGenerator::new();
        let mut ir_program = ir_gen.generate(&typed_program)?;

        if self.options.opt_level > 0 {
            let mut optimizer = Optimizer::new(self.options.opt_level);
            optimizer.optimize(&mut ir_program);
        }

        let mut codegen = SbpfCodegen::new();
        let sbpf_program = codegen.generate(&ir_program)?;

        // Verify
        let verifier = Verifier::new();
        let verification = verifier.verify(&sbpf_program);

        if !verification.valid {
            let error_msgs: Vec<String> = verification.errors.iter()
                .map(|e| e.to_string())
                .collect();
            return Err(Error::compiler(format!(
                "Verification failed: {}",
                error_msgs.join("; ")
            )));
        }

        let mut elf_writer = ElfWriter::new();
        let elf_bytes = elf_writer.write(&sbpf_program, self.options.debug_info)?;

        let mut warnings = type_checker.warnings().to_vec();
        warnings.extend(verification.warnings.clone());

        Ok(CompileResult {
            elf_bytes,
            estimated_cu: verification.stats.estimated_cu,
            ir_instruction_count: ir_program.instructions.len(),
            sbpf_instruction_count: sbpf_program.len(),
            warnings,
            verification: Some(verification),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_creation() {
        let compiler = Compiler::new(CompileOptions::default());
        assert_eq!(compiler.options.opt_level, 2);
    }
}
