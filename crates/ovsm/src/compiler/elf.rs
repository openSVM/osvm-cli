//! # ELF Writer for sBPF Programs
//!
//! Packages sBPF bytecode into an ELF shared object (.so) file
//! that can be deployed to Solana.

use super::sbpf_codegen::SbpfInstruction;
use crate::{Result, Error};

/// ELF magic number
const ELF_MAGIC: [u8; 4] = [0x7f, b'E', b'L', b'F'];

/// ELF class: 64-bit
const ELFCLASS64: u8 = 2;

/// ELF data encoding: little-endian
const ELFDATA2LSB: u8 = 1;

/// ELF version
const EV_CURRENT: u8 = 1;

/// ELF OS/ABI: None (ELFOSABI_NONE)
const ELFOSABI_NONE: u8 = 0;

/// ELF type: Shared object (ET_DYN)
const ET_DYN: u16 = 3;

/// ELF machine: eBPF
const EM_BPF: u16 = 247;

/// Section header types
const SHT_NULL: u32 = 0;
const SHT_PROGBITS: u32 = 1;
const SHT_STRTAB: u32 = 3;

/// Section flags
const SHF_ALLOC: u64 = 0x2;
const SHF_EXECINSTR: u64 = 0x4;

/// ELF writer for sBPF programs
pub struct ElfWriter {
    /// String table
    strtab: Vec<u8>,
}

impl ElfWriter {
    pub fn new() -> Self {
        Self {
            strtab: vec![0], // Start with null byte
        }
    }

    /// Write sBPF program to ELF format
    pub fn write(&mut self, program: &[SbpfInstruction], debug_info: bool) -> Result<Vec<u8>> {
        let _ = debug_info; // TODO: Add debug info support

        // Encode all instructions
        let mut text_section: Vec<u8> = Vec::new();
        for instr in program {
            text_section.extend_from_slice(&instr.encode());
        }

        if text_section.is_empty() {
            return Err(Error::runtime("Cannot create ELF with empty program"));
        }

        // Build string table
        let shstrtab_name_idx = self.add_string(".shstrtab");
        let text_name_idx = self.add_string(".text");

        // Calculate offsets
        let ehdr_size = 64; // ELF64 header size
        let shdr_size = 64; // Section header size
        let num_sections = 3; // NULL, .text, .shstrtab

        let text_offset = ehdr_size;
        let text_size = text_section.len();

        let shstrtab_offset = text_offset + text_size;
        let shstrtab_size = self.strtab.len();

        let shdr_offset = shstrtab_offset + shstrtab_size;
        // Align to 8 bytes
        let shdr_offset_aligned = (shdr_offset + 7) & !7;

        // Build ELF
        let mut elf = Vec::new();

        // ==================== ELF Header ====================
        // e_ident (16 bytes)
        elf.extend_from_slice(&ELF_MAGIC);           // Magic
        elf.push(ELFCLASS64);                        // 64-bit
        elf.push(ELFDATA2LSB);                       // Little-endian
        elf.push(EV_CURRENT);                        // Version
        elf.push(ELFOSABI_NONE);                     // OS/ABI
        elf.extend_from_slice(&[0u8; 8]);            // Padding

        // e_type (2 bytes) - ET_DYN
        elf.extend_from_slice(&ET_DYN.to_le_bytes());

        // e_machine (2 bytes) - EM_BPF
        elf.extend_from_slice(&EM_BPF.to_le_bytes());

        // e_version (4 bytes)
        elf.extend_from_slice(&1u32.to_le_bytes());

        // e_entry (8 bytes) - entry point
        elf.extend_from_slice(&(text_offset as u64).to_le_bytes());

        // e_phoff (8 bytes) - program header offset (none)
        elf.extend_from_slice(&0u64.to_le_bytes());

        // e_shoff (8 bytes) - section header offset
        elf.extend_from_slice(&(shdr_offset_aligned as u64).to_le_bytes());

        // e_flags (4 bytes)
        elf.extend_from_slice(&0u32.to_le_bytes());

        // e_ehsize (2 bytes)
        elf.extend_from_slice(&(ehdr_size as u16).to_le_bytes());

        // e_phentsize (2 bytes)
        elf.extend_from_slice(&0u16.to_le_bytes());

        // e_phnum (2 bytes)
        elf.extend_from_slice(&0u16.to_le_bytes());

        // e_shentsize (2 bytes)
        elf.extend_from_slice(&(shdr_size as u16).to_le_bytes());

        // e_shnum (2 bytes)
        elf.extend_from_slice(&(num_sections as u16).to_le_bytes());

        // e_shstrndx (2 bytes) - index of .shstrtab
        elf.extend_from_slice(&2u16.to_le_bytes());

        assert_eq!(elf.len(), ehdr_size);

        // ==================== .text Section ====================
        elf.extend_from_slice(&text_section);

        // ==================== .shstrtab Section ====================
        elf.extend_from_slice(&self.strtab);

        // ==================== Padding ====================
        while elf.len() < shdr_offset_aligned {
            elf.push(0);
        }

        // ==================== Section Headers ====================

        // Section 0: NULL
        elf.extend_from_slice(&[0u8; 64]);

        // Section 1: .text
        // sh_name (4 bytes)
        elf.extend_from_slice(&(text_name_idx as u32).to_le_bytes());
        // sh_type (4 bytes)
        elf.extend_from_slice(&SHT_PROGBITS.to_le_bytes());
        // sh_flags (8 bytes)
        elf.extend_from_slice(&(SHF_ALLOC | SHF_EXECINSTR).to_le_bytes());
        // sh_addr (8 bytes)
        elf.extend_from_slice(&0u64.to_le_bytes());
        // sh_offset (8 bytes)
        elf.extend_from_slice(&(text_offset as u64).to_le_bytes());
        // sh_size (8 bytes)
        elf.extend_from_slice(&(text_size as u64).to_le_bytes());
        // sh_link (4 bytes)
        elf.extend_from_slice(&0u32.to_le_bytes());
        // sh_info (4 bytes)
        elf.extend_from_slice(&0u32.to_le_bytes());
        // sh_addralign (8 bytes)
        elf.extend_from_slice(&8u64.to_le_bytes());
        // sh_entsize (8 bytes)
        elf.extend_from_slice(&0u64.to_le_bytes());

        // Section 2: .shstrtab
        // sh_name (4 bytes)
        elf.extend_from_slice(&(shstrtab_name_idx as u32).to_le_bytes());
        // sh_type (4 bytes)
        elf.extend_from_slice(&SHT_STRTAB.to_le_bytes());
        // sh_flags (8 bytes)
        elf.extend_from_slice(&0u64.to_le_bytes());
        // sh_addr (8 bytes)
        elf.extend_from_slice(&0u64.to_le_bytes());
        // sh_offset (8 bytes)
        elf.extend_from_slice(&(shstrtab_offset as u64).to_le_bytes());
        // sh_size (8 bytes)
        elf.extend_from_slice(&(shstrtab_size as u64).to_le_bytes());
        // sh_link (4 bytes)
        elf.extend_from_slice(&0u32.to_le_bytes());
        // sh_info (4 bytes)
        elf.extend_from_slice(&0u32.to_le_bytes());
        // sh_addralign (8 bytes)
        elf.extend_from_slice(&1u64.to_le_bytes());
        // sh_entsize (8 bytes)
        elf.extend_from_slice(&0u64.to_le_bytes());

        Ok(elf)
    }

    /// Add a string to the string table, return its index
    fn add_string(&mut self, s: &str) -> usize {
        let idx = self.strtab.len();
        self.strtab.extend_from_slice(s.as_bytes());
        self.strtab.push(0); // Null terminator
        idx
    }
}

impl Default for ElfWriter {
    fn default() -> Self {
        Self::new()
    }
}

/// Validate an ELF file is a valid sBPF program
pub fn validate_sbpf_elf(data: &[u8]) -> Result<()> {
    if data.len() < 64 {
        return Err(Error::runtime("ELF file too small"));
    }

    // Check magic
    if data[0..4] != ELF_MAGIC {
        return Err(Error::runtime("Invalid ELF magic number"));
    }

    // Check class (64-bit)
    if data[4] != ELFCLASS64 {
        return Err(Error::runtime("ELF must be 64-bit"));
    }

    // Check endianness
    if data[5] != ELFDATA2LSB {
        return Err(Error::runtime("ELF must be little-endian"));
    }

    // Check machine type
    let machine = u16::from_le_bytes([data[18], data[19]]);
    if machine != EM_BPF {
        return Err(Error::runtime(format!(
            "ELF machine type must be BPF ({}), got {}",
            EM_BPF, machine
        )));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_elf_writer() {
        let mut writer = ElfWriter::new();

        let program = vec![
            SbpfInstruction::alu64_imm(0xb0, 0, 42), // mov64 r0, 42
            SbpfInstruction::exit(),
        ];

        let elf = writer.write(&program, false).unwrap();

        // Validate the ELF
        assert_eq!(&elf[0..4], &ELF_MAGIC);
        assert!(validate_sbpf_elf(&elf).is_ok());
    }

    #[test]
    fn test_empty_program_error() {
        let mut writer = ElfWriter::new();
        let result = writer.write(&[], false);
        assert!(result.is_err());
    }
}
