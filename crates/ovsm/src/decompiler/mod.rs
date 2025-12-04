//! # sBPF Decompiler - Bytecode to OVSM LISP
//!
//! Reverse engineers sBPF bytecode back into readable OVSM LISP code.
//! Supports IDL-enhanced decompilation for improved naming and readability.
//!
//! ## Features
//!
//! - Disassemble sBPF ELF binaries
//! - Recover control flow graphs
//! - Generate readable OVSM LISP
//! - Use Anchor IDL for semantic naming
//!
//! ## Usage
//!
//! ```ignore
//! use ovsm::decompiler::{Decompiler, DecompileOptions};
//!
//! let elf_bytes = std::fs::read("program.so")?;
//! let decompiler = Decompiler::new(DecompileOptions::default());
//! let ovsm_source = decompiler.decompile(&elf_bytes)?;
//! println!("{}", ovsm_source);
//! ```

pub mod cfg;
pub mod disassembler;
pub mod idl;
pub mod ovsm_emitter;

pub use cfg::{BasicBlock, ControlFlowGraph};
pub use disassembler::{DisassembledInstr, Disassembler};
pub use idl::{AnchorIdl, IdlAccount, IdlInstruction};
pub use ovsm_emitter::OvsmEmitter;

use crate::{Error, Result};

/// Decompiler options
#[derive(Debug, Clone, Default)]
pub struct DecompileOptions {
    /// Path to Anchor IDL JSON file (optional)
    pub idl_path: Option<String>,
    /// Generate comments with addresses
    pub show_addresses: bool,
    /// Inline constant expressions
    pub inline_constants: bool,
    /// Use semantic names from IDL
    pub use_idl_names: bool,
}

/// Decompilation result
#[derive(Debug)]
pub struct DecompileResult {
    /// Generated OVSM source code
    pub source: String,
    /// Disassembled instructions
    pub instructions: Vec<DisassembledInstr>,
    /// Recovered control flow graph
    pub cfg: ControlFlowGraph,
    /// IDL metadata (if available)
    pub idl: Option<AnchorIdl>,
    /// Warnings during decompilation
    pub warnings: Vec<String>,
}

/// sBPF to OVSM Decompiler
pub struct Decompiler {
    options: DecompileOptions,
}

impl Decompiler {
    /// Create a new decompiler with options
    pub fn new(options: DecompileOptions) -> Self {
        Self { options }
    }

    /// Decompile sBPF ELF bytes to OVSM source
    pub fn decompile(&self, elf_bytes: &[u8]) -> Result<DecompileResult> {
        let mut warnings = Vec::new();

        // Step 1: Validate ELF
        crate::compiler::elf::validate_sbpf_elf(elf_bytes)?;

        // Step 2: Disassemble
        let disasm = Disassembler::new();
        let instructions = disasm.disassemble(elf_bytes)?;

        if instructions.is_empty() {
            return Err(Error::runtime("No instructions found in ELF"));
        }

        // Step 3: Recover CFG
        let cfg = ControlFlowGraph::build(&instructions);

        // Step 4: Load IDL (optional)
        let idl = if let Some(ref idl_path) = self.options.idl_path {
            match AnchorIdl::load(idl_path) {
                Ok(idl) => Some(idl),
                Err(e) => {
                    warnings.push(format!("Failed to load IDL: {}", e));
                    None
                }
            }
        } else {
            None
        };

        // Step 5: Emit OVSM
        let emitter = OvsmEmitter::new(&self.options, idl.as_ref());
        let source = emitter.emit(&cfg, &instructions)?;

        Ok(DecompileResult {
            source,
            instructions,
            cfg,
            idl,
            warnings,
        })
    }

    /// Decompile from file path
    pub fn decompile_file(&self, path: &str) -> Result<DecompileResult> {
        let elf_bytes = std::fs::read(path)
            .map_err(|e| Error::runtime(format!("Failed to read file {}: {}", path, e)))?;
        self.decompile(&elf_bytes)
    }
}

impl Default for Decompiler {
    fn default() -> Self {
        Self::new(DecompileOptions::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decompiler_creation() {
        let decompiler = Decompiler::default();
        assert!(!decompiler.options.show_addresses);
    }
}
