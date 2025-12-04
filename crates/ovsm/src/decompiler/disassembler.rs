//! # sBPF Disassembler
//!
//! Parses sBPF ELF binaries and disassembles bytecode into readable instructions.

use crate::{Error, Result};

/// Disassembled instruction with metadata
#[derive(Debug, Clone)]
pub struct DisassembledInstr {
    /// Instruction offset in bytes
    pub offset: usize,
    /// Raw opcode
    pub opcode: u8,
    /// Destination register
    pub dst: u8,
    /// Source register
    pub src: u8,
    /// Offset field
    pub off: i16,
    /// Immediate value
    pub imm: i32,
    /// Human-readable mnemonic
    pub mnemonic: String,
    /// Operand string
    pub operands: String,
}

impl DisassembledInstr {
    /// Format as assembly string
    pub fn to_asm(&self) -> String {
        if self.operands.is_empty() {
            self.mnemonic.clone()
        } else {
            format!("{} {}", self.mnemonic, self.operands)
        }
    }

    /// Check if this is a jump instruction
    pub fn is_jump(&self) -> bool {
        matches!(
            self.opcode,
            0x05 | 0x15
                | 0x1d
                | 0x25
                | 0x2d
                | 0x35
                | 0x3d
                | 0x45
                | 0x55
                | 0x5d
                | 0xa5
                | 0xad
                | 0xb5
                | 0xbd
        )
    }

    /// Check if this is a return/exit
    pub fn is_exit(&self) -> bool {
        self.opcode == 0x95
    }

    /// Check if this is a call
    pub fn is_call(&self) -> bool {
        self.opcode == 0x85
    }

    /// Get jump target offset (relative to next instruction)
    pub fn jump_target(&self) -> Option<i64> {
        if self.is_jump() {
            Some(self.off as i64)
        } else {
            None
        }
    }
}

/// sBPF Disassembler
pub struct Disassembler {
    /// ELF header size
    ehdr_size: usize,
}

impl Disassembler {
    /// Creates a new disassembler with default ELF header size.
    pub fn new() -> Self {
        Self { ehdr_size: 64 }
    }

    /// Disassemble sBPF ELF bytes
    pub fn disassemble(&self, elf_bytes: &[u8]) -> Result<Vec<DisassembledInstr>> {
        // Find .text section
        let text_section = self.find_text_section(elf_bytes)?;

        let mut instructions = Vec::new();
        let mut offset = 0;

        while offset + 8 <= text_section.len() {
            let instr = self.decode_instruction(&text_section[offset..], offset)?;
            offset += 8;

            // Handle lddw (16-byte instruction)
            if instr.opcode == 0x18 {
                offset += 8;
            }

            instructions.push(instr);
        }

        Ok(instructions)
    }

    /// Find .text section in ELF
    fn find_text_section<'a>(&self, elf: &'a [u8]) -> Result<&'a [u8]> {
        if elf.len() < 64 {
            return Err(Error::runtime("ELF too small"));
        }

        // Read section header offset
        let shoff = u64::from_le_bytes(elf[40..48].try_into().unwrap()) as usize;
        let shentsize = u16::from_le_bytes(elf[58..60].try_into().unwrap()) as usize;
        let shnum = u16::from_le_bytes(elf[60..62].try_into().unwrap()) as usize;
        let shstrndx = u16::from_le_bytes(elf[62..64].try_into().unwrap()) as usize;

        if shoff == 0 || shnum == 0 {
            // No section headers, assume code starts after header
            return Ok(&elf[self.ehdr_size..]);
        }

        // Get string table section
        let strtab_hdr_off = shoff + shstrndx * shentsize;
        if strtab_hdr_off + 64 > elf.len() {
            return Ok(&elf[self.ehdr_size..]);
        }

        let strtab_off = u64::from_le_bytes(
            elf[strtab_hdr_off + 24..strtab_hdr_off + 32]
                .try_into()
                .unwrap(),
        ) as usize;
        let strtab_size = u64::from_le_bytes(
            elf[strtab_hdr_off + 32..strtab_hdr_off + 40]
                .try_into()
                .unwrap(),
        ) as usize;

        if strtab_off + strtab_size > elf.len() {
            return Ok(&elf[self.ehdr_size..]);
        }

        // Find .text section
        for i in 0..shnum {
            let hdr_off = shoff + i * shentsize;
            if hdr_off + 64 > elf.len() {
                continue;
            }

            let name_idx =
                u32::from_le_bytes(elf[hdr_off..hdr_off + 4].try_into().unwrap()) as usize;

            // Get section name
            if strtab_off + name_idx < elf.len() {
                let name_end = elf[strtab_off + name_idx..]
                    .iter()
                    .position(|&b| b == 0)
                    .unwrap_or(0);
                let name = std::str::from_utf8(
                    &elf[strtab_off + name_idx..strtab_off + name_idx + name_end],
                )
                .unwrap_or("");

                if name == ".text" {
                    let sec_off =
                        u64::from_le_bytes(elf[hdr_off + 24..hdr_off + 32].try_into().unwrap())
                            as usize;
                    let sec_size =
                        u64::from_le_bytes(elf[hdr_off + 32..hdr_off + 40].try_into().unwrap())
                            as usize;

                    if sec_off + sec_size <= elf.len() {
                        return Ok(&elf[sec_off..sec_off + sec_size]);
                    }
                }
            }
        }

        // Fallback: assume code after header
        Ok(&elf[self.ehdr_size..])
    }

    /// Decode a single instruction
    fn decode_instruction(&self, bytes: &[u8], offset: usize) -> Result<DisassembledInstr> {
        if bytes.len() < 8 {
            return Err(Error::runtime("Incomplete instruction"));
        }

        let opcode = bytes[0];
        let dst_src = bytes[1];
        let dst = dst_src & 0x0f;
        let src = (dst_src >> 4) & 0x0f;
        let off = i16::from_le_bytes([bytes[2], bytes[3]]);
        let imm = i32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);

        let (mnemonic, operands) = self.format_instruction(opcode, dst, src, off, imm);

        Ok(DisassembledInstr {
            offset,
            opcode,
            dst,
            src,
            off,
            imm,
            mnemonic,
            operands,
        })
    }

    /// Format instruction as mnemonic and operands
    fn format_instruction(
        &self,
        opcode: u8,
        dst: u8,
        src: u8,
        off: i16,
        imm: i32,
    ) -> (String, String) {
        let reg_name = |r: u8| format!("r{}", r);

        match opcode {
            // ALU64 immediate
            0x07 => ("add64".into(), format!("{}, {}", reg_name(dst), imm)),
            0x17 => ("sub64".into(), format!("{}, {}", reg_name(dst), imm)),
            0x27 => ("mul64".into(), format!("{}, {}", reg_name(dst), imm)),
            0x37 => ("div64".into(), format!("{}, {}", reg_name(dst), imm)),
            0x47 => ("or64".into(), format!("{}, {}", reg_name(dst), imm)),
            0x57 => ("and64".into(), format!("{}, {}", reg_name(dst), imm)),
            0x97 => ("mod64".into(), format!("{}, {}", reg_name(dst), imm)),
            0xa7 => ("xor64".into(), format!("{}, {}", reg_name(dst), imm)),
            0xb7 => ("mov64".into(), format!("{}, {}", reg_name(dst), imm)),

            // ALU64 register
            0x0f => (
                "add64".into(),
                format!("{}, {}", reg_name(dst), reg_name(src)),
            ),
            0x1f => (
                "sub64".into(),
                format!("{}, {}", reg_name(dst), reg_name(src)),
            ),
            0x2f => (
                "mul64".into(),
                format!("{}, {}", reg_name(dst), reg_name(src)),
            ),
            0x3f => (
                "div64".into(),
                format!("{}, {}", reg_name(dst), reg_name(src)),
            ),
            0x4f => (
                "or64".into(),
                format!("{}, {}", reg_name(dst), reg_name(src)),
            ),
            0x5f => (
                "and64".into(),
                format!("{}, {}", reg_name(dst), reg_name(src)),
            ),
            0x9f => (
                "mod64".into(),
                format!("{}, {}", reg_name(dst), reg_name(src)),
            ),
            0xaf => (
                "xor64".into(),
                format!("{}, {}", reg_name(dst), reg_name(src)),
            ),
            0xbf => (
                "mov64".into(),
                format!("{}, {}", reg_name(dst), reg_name(src)),
            ),
            0x87 => ("neg64".into(), reg_name(dst)),

            // Memory
            0x79 => (
                "ldxdw".into(),
                format!("{}, [{}+{}]", reg_name(dst), reg_name(src), off),
            ),
            0x7b => (
                "stxdw".into(),
                format!("[{}+{}], {}", reg_name(dst), off, reg_name(src)),
            ),
            0x18 => ("lddw".into(), format!("{}, {}", reg_name(dst), imm as u64)),

            // Jump unconditional
            0x05 => ("ja".into(), format!("+{}", off)),

            // Jump conditional immediate
            0x15 => (
                "jeq".into(),
                format!("{}, {}, +{}", reg_name(dst), imm, off),
            ),
            0x25 => (
                "jgt".into(),
                format!("{}, {}, +{}", reg_name(dst), imm, off),
            ),
            0x35 => (
                "jge".into(),
                format!("{}, {}, +{}", reg_name(dst), imm, off),
            ),
            0x45 => (
                "jset".into(),
                format!("{}, {}, +{}", reg_name(dst), imm, off),
            ),
            0x55 => (
                "jne".into(),
                format!("{}, {}, +{}", reg_name(dst), imm, off),
            ),
            0xa5 => (
                "jlt".into(),
                format!("{}, {}, +{}", reg_name(dst), imm, off),
            ),
            0xb5 => (
                "jle".into(),
                format!("{}, {}, +{}", reg_name(dst), imm, off),
            ),

            // Jump conditional register
            0x1d => (
                "jeq".into(),
                format!("{}, {}, +{}", reg_name(dst), reg_name(src), off),
            ),
            0x2d => (
                "jgt".into(),
                format!("{}, {}, +{}", reg_name(dst), reg_name(src), off),
            ),
            0x3d => (
                "jge".into(),
                format!("{}, {}, +{}", reg_name(dst), reg_name(src), off),
            ),
            0x5d => (
                "jne".into(),
                format!("{}, {}, +{}", reg_name(dst), reg_name(src), off),
            ),
            0xad => (
                "jlt".into(),
                format!("{}, {}, +{}", reg_name(dst), reg_name(src), off),
            ),
            0xbd => (
                "jle".into(),
                format!("{}, {}, +{}", reg_name(dst), reg_name(src), off),
            ),

            // Call/Exit
            0x85 => ("call".into(), format!("{}", imm)),
            0x95 => ("exit".into(), String::new()),

            // Unknown
            _ => (
                format!("unknown_{:02x}", opcode),
                format!("{} {} {} {}", dst, src, off, imm),
            ),
        }
    }
}

impl Default for Disassembler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_disassemble_instruction() {
        let disasm = Disassembler::new();

        // mov64 r0, 42
        let bytes = [0xb7, 0x00, 0x00, 0x00, 0x2a, 0x00, 0x00, 0x00];
        let instr = disasm.decode_instruction(&bytes, 0).unwrap();

        assert_eq!(instr.mnemonic, "mov64");
        assert_eq!(instr.dst, 0);
        assert_eq!(instr.imm, 42);
    }

    #[test]
    fn test_exit_detection() {
        let disasm = Disassembler::new();

        // exit
        let bytes = [0x95, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let instr = disasm.decode_instruction(&bytes, 0).unwrap();

        assert!(instr.is_exit());
        assert!(!instr.is_call());
    }
}
