//! # OVSM LISP Code Emitter
//!
//! Generates readable OVSM LISP code from disassembled sBPF instructions.

use super::{
    cfg::{BasicBlock, ControlFlowGraph},
    idl::AnchorIdl,
    DecompileOptions, DisassembledInstr,
};
use crate::{Error, Result};
use std::collections::HashMap;

/// OVSM code emitter
pub struct OvsmEmitter<'a> {
    options: &'a DecompileOptions,
    idl: Option<&'a AnchorIdl>,
    /// Track register assignments
    register_names: HashMap<u8, String>,
    /// Next variable number
    next_var: u32,
    /// Indentation level
    indent: usize,
}

impl<'a> OvsmEmitter<'a> {
    /// Creates a new OVSM code emitter with the given options and optional Anchor IDL.
    pub fn new(options: &'a DecompileOptions, idl: Option<&'a AnchorIdl>) -> Self {
        Self {
            options,
            idl,
            register_names: HashMap::new(),
            next_var: 0,
            indent: 0,
        }
    }

    /// Emit OVSM code from CFG
    pub fn emit(
        &self,
        cfg: &ControlFlowGraph,
        instructions: &[DisassembledInstr],
    ) -> Result<String> {
        let mut output = String::new();

        // Header comment
        output.push_str(";;; Decompiled from sBPF bytecode\n");
        if let Some(idl) = self.idl {
            output.push_str(&format!(";;; Program: {} v{}\n", idl.name, idl.version));
        }
        output.push_str(";;;\n\n");

        // Emit program structure
        output.push_str("(define-program decompiled\n");

        // Emit main entrypoint
        output.push_str("  (entrypoint (accounts instruction-data)\n");

        // Process blocks in order
        let block_order = cfg.blocks_topo_order();

        for block_id in block_order {
            if let Some(block) = cfg.get_block(block_id) {
                let block_code = self.emit_block(block, instructions, cfg)?;
                output.push_str(&block_code);
            }
        }

        output.push_str("    ))\n"); // Close entrypoint and define-program

        Ok(output)
    }

    fn emit_block(
        &self,
        block: &BasicBlock,
        instructions: &[DisassembledInstr],
        cfg: &ControlFlowGraph,
    ) -> Result<String> {
        let mut output = String::new();
        let indent = "    ";

        // Block label comment
        if self.options.show_addresses {
            output.push_str(&format!(
                "{}  ;; Block {} (offset 0x{:x})\n",
                indent, block.id, block.start_offset
            ));
        }

        // Check if this is a loop header
        if cfg.is_loop_header(block.id) {
            output.push_str(&format!("{}  ;; Loop header\n", indent));
        }

        // Emit instructions
        for &instr_idx in &block.instructions {
            if instr_idx >= instructions.len() {
                continue;
            }

            let instr = &instructions[instr_idx];
            let ovsm = self.emit_instruction(instr)?;

            if !ovsm.is_empty() {
                if self.options.show_addresses {
                    output.push_str(&format!(
                        "{}  ;; 0x{:04x}: {}\n",
                        indent,
                        instr.offset,
                        instr.to_asm()
                    ));
                }
                output.push_str(&format!("{}  {}\n", indent, ovsm));
            }
        }

        Ok(output)
    }

    fn emit_instruction(&self, instr: &DisassembledInstr) -> Result<String> {
        let reg_name = |r: u8| -> String {
            match r {
                0 => "result".into(),
                1 => "arg1".into(),
                2 => "arg2".into(),
                3 => "arg3".into(),
                4 => "arg4".into(),
                5 => "arg5".into(),
                10 => "frame-ptr".into(),
                r => format!("r{}", r),
            }
        };

        match instr.opcode {
            // MOV immediate
            0xb7 => {
                let dst = reg_name(instr.dst);
                Ok(format!("(define {} {})", dst, instr.imm))
            }

            // MOV register
            0xbf => {
                let dst = reg_name(instr.dst);
                let src = reg_name(instr.src);
                Ok(format!("(define {} {})", dst, src))
            }

            // ADD immediate
            0x07 => {
                let dst = reg_name(instr.dst);
                Ok(format!("(set! {} (+ {} {}))", dst, dst, instr.imm))
            }

            // ADD register
            0x0f => {
                let dst = reg_name(instr.dst);
                let src = reg_name(instr.src);
                Ok(format!("(set! {} (+ {} {}))", dst, dst, src))
            }

            // SUB immediate
            0x17 => {
                let dst = reg_name(instr.dst);
                Ok(format!("(set! {} (- {} {}))", dst, dst, instr.imm))
            }

            // SUB register
            0x1f => {
                let dst = reg_name(instr.dst);
                let src = reg_name(instr.src);
                Ok(format!("(set! {} (- {} {}))", dst, dst, src))
            }

            // MUL immediate
            0x27 => {
                let dst = reg_name(instr.dst);
                Ok(format!("(set! {} (* {} {}))", dst, dst, instr.imm))
            }

            // MUL register
            0x2f => {
                let dst = reg_name(instr.dst);
                let src = reg_name(instr.src);
                Ok(format!("(set! {} (* {} {}))", dst, dst, src))
            }

            // DIV immediate
            0x37 => {
                let dst = reg_name(instr.dst);
                Ok(format!("(set! {} (/ {} {}))", dst, dst, instr.imm))
            }

            // DIV register
            0x3f => {
                let dst = reg_name(instr.dst);
                let src = reg_name(instr.src);
                Ok(format!("(set! {} (/ {} {}))", dst, dst, src))
            }

            // MOD immediate
            0x97 => {
                let dst = reg_name(instr.dst);
                Ok(format!("(set! {} (% {} {}))", dst, dst, instr.imm))
            }

            // MOD register
            0x9f => {
                let dst = reg_name(instr.dst);
                let src = reg_name(instr.src);
                Ok(format!("(set! {} (% {} {}))", dst, dst, src))
            }

            // AND immediate
            0x57 => {
                let dst = reg_name(instr.dst);
                Ok(format!("(set! {} (and {} {}))", dst, dst, instr.imm))
            }

            // AND register
            0x5f => {
                let dst = reg_name(instr.dst);
                let src = reg_name(instr.src);
                Ok(format!("(set! {} (and {} {}))", dst, dst, src))
            }

            // OR immediate
            0x47 => {
                let dst = reg_name(instr.dst);
                Ok(format!("(set! {} (or {} {}))", dst, dst, instr.imm))
            }

            // OR register
            0x4f => {
                let dst = reg_name(instr.dst);
                let src = reg_name(instr.src);
                Ok(format!("(set! {} (or {} {}))", dst, dst, src))
            }

            // NEG
            0x87 => {
                let dst = reg_name(instr.dst);
                Ok(format!("(set! {} (- 0 {}))", dst, dst))
            }

            // Load double-word
            0x79 => {
                let dst = reg_name(instr.dst);
                let base = reg_name(instr.src);
                if self.options.use_idl_names {
                    // Try to use IDL for semantic names
                    Ok(format!("(define {} (load {} {}))", dst, base, instr.off))
                } else {
                    Ok(format!(
                        "(define {} (mem-load {} {}))",
                        dst, base, instr.off
                    ))
                }
            }

            // Store double-word
            0x7b => {
                let dst = reg_name(instr.dst);
                let src = reg_name(instr.src);
                Ok(format!("(mem-store {} {} {})", dst, instr.off, src))
            }

            // Unconditional jump
            0x05 => Ok(format!(";; jump +{}", instr.off)),

            // Conditional jumps
            0x15 => {
                // jeq imm
                let reg = reg_name(instr.dst);
                Ok(format!("(if (= {} {}) ...)", reg, instr.imm))
            }
            0x1d => {
                // jeq reg
                let r1 = reg_name(instr.dst);
                let r2 = reg_name(instr.src);
                Ok(format!("(if (= {} {}) ...)", r1, r2))
            }
            0x55 => {
                // jne imm
                let reg = reg_name(instr.dst);
                Ok(format!("(if (!= {} {}) ...)", reg, instr.imm))
            }
            0x5d => {
                // jne reg
                let r1 = reg_name(instr.dst);
                let r2 = reg_name(instr.src);
                Ok(format!("(if (!= {} {}) ...)", r1, r2))
            }
            0x25 => {
                // jgt imm
                let reg = reg_name(instr.dst);
                Ok(format!("(if (> {} {}) ...)", reg, instr.imm))
            }
            0x2d => {
                // jgt reg
                let r1 = reg_name(instr.dst);
                let r2 = reg_name(instr.src);
                Ok(format!("(if (> {} {}) ...)", r1, r2))
            }
            0x35 => {
                // jge imm
                let reg = reg_name(instr.dst);
                Ok(format!("(if (>= {} {}) ...)", reg, instr.imm))
            }
            0x3d => {
                // jge reg
                let r1 = reg_name(instr.dst);
                let r2 = reg_name(instr.src);
                Ok(format!("(if (>= {} {}) ...)", r1, r2))
            }
            0xa5 => {
                // jlt imm
                let reg = reg_name(instr.dst);
                Ok(format!("(if (< {} {}) ...)", reg, instr.imm))
            }
            0xad => {
                // jlt reg
                let r1 = reg_name(instr.dst);
                let r2 = reg_name(instr.src);
                Ok(format!("(if (< {} {}) ...)", r1, r2))
            }
            0xb5 => {
                // jle imm
                let reg = reg_name(instr.dst);
                Ok(format!("(if (<= {} {}) ...)", reg, instr.imm))
            }
            0xbd => {
                // jle reg
                let r1 = reg_name(instr.dst);
                let r2 = reg_name(instr.src);
                Ok(format!("(if (<= {} {}) ...)", r1, r2))
            }

            // Call
            0x85 => {
                let syscall_name = self.syscall_name(instr.imm);
                Ok(format!("({})", syscall_name))
            }

            // Exit
            0x95 => Ok("(return result)".into()),

            _ => {
                if self.options.show_addresses {
                    Ok(format!(";; unknown: {}", instr.to_asm()))
                } else {
                    Ok(String::new())
                }
            }
        }
    }

    /// Get syscall name from hash
    fn syscall_name(&self, hash: i32) -> String {
        use crate::compiler::sbpf_codegen::SolanaSymbols;

        let hash_u32 = hash as u32;
        let lookup = SolanaSymbols::hash_to_name();

        if let Some(&name) = lookup.get(&hash_u32) {
            // Convert to OVSM-friendly name
            name.trim_end_matches('_')
                .replace("sol_", "sol-")
                .replace('_', "-")
        } else {
            format!("syscall-{:#x}", hash_u32)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_emitter_creation() {
        let options = DecompileOptions::default();
        let emitter = OvsmEmitter::new(&options, None);
        assert_eq!(emitter.indent, 0);
    }

    #[test]
    fn test_emit_mov() {
        let options = DecompileOptions::default();
        let emitter = OvsmEmitter::new(&options, None);

        let instr = DisassembledInstr {
            offset: 0,
            opcode: 0xb7,
            dst: 0,
            src: 0,
            off: 0,
            imm: 42,
            mnemonic: "mov64".into(),
            operands: "r0, 42".into(),
        };

        let ovsm = emitter.emit_instruction(&instr).unwrap();
        assert_eq!(ovsm, "(define result 42)");
    }

    #[test]
    fn test_emit_exit() {
        let options = DecompileOptions::default();
        let emitter = OvsmEmitter::new(&options, None);

        let instr = DisassembledInstr {
            offset: 0,
            opcode: 0x95,
            dst: 0,
            src: 0,
            off: 0,
            imm: 0,
            mnemonic: "exit".into(),
            operands: String::new(),
        };

        let ovsm = emitter.emit_instruction(&instr).unwrap();
        assert_eq!(ovsm, "(return result)");
    }
}
