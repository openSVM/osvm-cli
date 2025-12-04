//! # ELF Writer for sBPF Programs
//!
//! Packages sBPF bytecode into an ELF shared object (.so) file
//! that can be deployed to Solana.

use super::sbpf_codegen::SbpfInstruction;
use crate::{Error, Result};

/// ELF magic number
const ELF_MAGIC: [u8; 4] = [0x7f, b'E', b'L', b'F'];

/// ELF class: 64-bit
const ELFCLASS64: u8 = 2;

/// ELF data encoding: little-endian
const ELFDATA2LSB: u8 = 1;

/// ELF version
const EV_CURRENT: u8 = 1;

/// ELF OS/ABI: None
const ELFOSABI_NONE: u8 = 0;

/// ELF type: Shared object (ET_DYN)
const ET_DYN: u16 = 3;

/// ELF machine: SBF (Solana BPF v2)
const EM_SBF: u16 = 263; // 0x107

/// ELF flags for SBPF versions
const EF_SBF_V1: u32 = 0x0; // V1 with relocations
const EF_SBF_V2: u32 = 0x20; // V2 with static syscalls

/// Section header types
const SHT_NULL: u32 = 0;
const SHT_PROGBITS: u32 = 1;
const SHT_SYMTAB: u32 = 2;
const SHT_STRTAB: u32 = 3;
const SHT_REL: u32 = 9;
const SHT_DYNSYM: u32 = 11;
const SHT_DYNAMIC: u32 = 6;
#[allow(dead_code)]
const SHT_NOBITS: u32 = 8;

/// Section flags
const SHF_ALLOC: u64 = 0x2;
const SHF_EXECINSTR: u64 = 0x4;
const SHF_WRITE: u64 = 0x1;

/// Program header types
const PT_LOAD: u32 = 1;
const PT_DYNAMIC: u32 = 2;
#[allow(dead_code)]
const PT_NULL: u32 = 0;

/// Dynamic tags
const DT_NULL: u64 = 0;
const DT_STRTAB: u64 = 5;
const DT_SYMTAB: u64 = 6;
const DT_STRSZ: u64 = 10;
const DT_SYMENT: u64 = 11;
const DT_REL: u64 = 17;
const DT_RELSZ: u64 = 18;
const DT_RELENT: u64 = 19;
const DT_TEXTREL: u64 = 22;
const DT_FLAGS: u64 = 30;
const DT_RELCOUNT: u64 = 0x6ffffffa;

/// Relocation types (Solana BPF standard)
const R_BPF_64_64: u32 = 1; // For 64-bit absolute relocations (syscalls)
const R_BPF_64_RELATIVE: u32 = 8; // For relative relocations (add MM_PROGRAM_START at load time)
const R_BPF_64_32: u32 = 10; // For 32-bit relocations

/// Program header flags
const PF_X: u32 = 0x1;
const PF_W: u32 = 0x2;
const PF_R: u32 = 0x4;

/// Virtual addresses for Solana memory regions (SBPFv1)
const TEXT_VADDR: u64 = 0x120; // .text at 0x120 (matching Solana's working ELFs)
const RODATA_VADDR: u64 = 0x150; // .rodata follows .text
const STACK_VADDR: u64 = 0x200000000;
const HEAP_VADDR: u64 = 0x300000000;
/// MM_PROGRAM_START: Base address added to all program memory at runtime
const MM_PROGRAM_START: u64 = 0x100000000;

/// Default stack/heap size
const STACK_SIZE: u64 = 0x1000;
const HEAP_SIZE: u64 = 0x1000;

/// Symbol binding
const STB_GLOBAL: u8 = 1;
/// Symbol type
const STT_FUNC: u8 = 2;

/// Syscall reference in the code
#[derive(Clone, Debug)]
pub struct SyscallRef {
    /// Instruction offset (in bytes from .text start)
    pub offset: usize,
    /// Syscall name (e.g., "sol_log_64_")
    pub name: String,
}

/// String load site (for rodata address patching)
#[derive(Clone, Debug)]
pub struct StringLoadRef {
    /// Instruction offset (in bytes from .text start, points to LDDW)
    pub offset: usize,
    /// String offset within rodata section
    pub rodata_offset: usize,
}

/// ELF writer for sBPF programs
pub struct ElfWriter {
    strtab: Vec<u8>,
    shstrtab: Vec<u8>,
    dynstr: Vec<u8>,
}

impl ElfWriter {
    /// Create a new ELF writer with empty string tables
    pub fn new() -> Self {
        Self {
            strtab: vec![0],
            shstrtab: vec![0],
            dynstr: vec![0],
        }
    }

    fn add_dynstr(&mut self, s: &str) -> usize {
        let idx = self.dynstr.len();
        self.dynstr.extend_from_slice(s.as_bytes());
        self.dynstr.push(0);
        idx
    }

    /// Write sBPF program to proper Solana ELF format
    pub fn write(
        &mut self,
        program: &[SbpfInstruction],
        _debug_info: bool,
        sbpf_version: super::SbpfVersion,
    ) -> Result<Vec<u8>> {
        // Encode instructions
        let mut text_section: Vec<u8> = Vec::new();
        for instr in program {
            text_section.extend_from_slice(&instr.encode());
        }

        if text_section.is_empty() {
            return Err(Error::runtime("Cannot create ELF with empty program"));
        }

        // Build string tables
        let entrypoint_str_idx = self.add_strtab("entrypoint");

        let _shstrtab_name = self.add_shstrtab(".shstrtab");
        let text_name = self.add_shstrtab(".text");
        let strtab_name = self.add_shstrtab(".strtab");
        let symtab_name = self.add_shstrtab(".symtab");
        let stack_name = self.add_shstrtab(".bss.stack");
        let heap_name = self.add_shstrtab(".bss.heap");

        // Layout (matching Solana's working ELFs):
        // [ELF Header: 64 bytes]
        // [Program Headers: 1 * 56 bytes]
        // [Padding to 0x120]
        // [.text section at 0x120]
        // [.strtab section]
        // [.symtab section]
        // [.shstrtab section]
        // [Section Headers]

        let ehdr_size = 64usize;
        let phdr_size = 56usize;
        let shdr_size = 64usize;
        let num_phdrs = 1usize; // Just .text PT_LOAD
        let num_sections = 5usize; // NULL, .text, .strtab, .symtab, .shstrtab

        let phdr_offset = ehdr_size;
        let text_offset = 0x120usize; // Match Solana's working ELFs
        let text_size = text_section.len();

        let strtab_offset = text_offset + text_size;
        let strtab_size = self.strtab.len();

        // Symbol table: one NULL entry + one entrypoint entry
        let symtab_offset = strtab_offset + strtab_size;
        let symtab_entry_size = 24usize; // Elf64_Sym
        let symtab_size = symtab_entry_size * 2; // NULL + entrypoint

        let shstrtab_offset = symtab_offset + symtab_size;
        let shstrtab_size = self.shstrtab.len();

        let shdr_offset = ((shstrtab_offset + shstrtab_size) + 7) & !7;

        // Build ELF
        let mut elf = Vec::new();

        // ==================== ELF Header ====================
        elf.extend_from_slice(&ELF_MAGIC);
        elf.push(ELFCLASS64);
        elf.push(ELFDATA2LSB);
        elf.push(EV_CURRENT);
        elf.push(ELFOSABI_NONE);
        elf.extend_from_slice(&[0u8; 8]);

        elf.extend_from_slice(&ET_DYN.to_le_bytes());
        elf.extend_from_slice(&EM_SBF.to_le_bytes());
        elf.extend_from_slice(&1u32.to_le_bytes());
        elf.extend_from_slice(&TEXT_VADDR.to_le_bytes()); // e_entry
        elf.extend_from_slice(&(phdr_offset as u64).to_le_bytes());
        elf.extend_from_slice(&(shdr_offset as u64).to_le_bytes());
        // Use appropriate flags based on SBPF version
        let ef_flags = match sbpf_version {
            super::SbpfVersion::V1 => EF_SBF_V1,
            super::SbpfVersion::V2 => EF_SBF_V2,
        };
        elf.extend_from_slice(&ef_flags.to_le_bytes()); // e_flags
        elf.extend_from_slice(&(ehdr_size as u16).to_le_bytes());
        elf.extend_from_slice(&(phdr_size as u16).to_le_bytes());
        elf.extend_from_slice(&(num_phdrs as u16).to_le_bytes());
        elf.extend_from_slice(&(shdr_size as u16).to_le_bytes());
        elf.extend_from_slice(&(num_sections as u16).to_le_bytes());
        elf.extend_from_slice(&((num_sections - 1) as u16).to_le_bytes()); // e_shstrndx

        assert_eq!(elf.len(), ehdr_size);

        // ==================== Program Headers ====================
        // Single PT_LOAD for .text (page-aligned like reference)
        self.write_phdr_aligned(
            &mut elf,
            PT_LOAD,
            PF_R | PF_X,
            text_offset,
            TEXT_VADDR,
            text_size,
        );

        // Padding to 0x120 for .text section
        while elf.len() < text_offset {
            elf.push(0);
        }

        // ==================== .text Section ====================
        elf.extend_from_slice(&text_section);

        // ==================== .strtab Section ====================
        elf.extend_from_slice(&self.strtab);

        // ==================== .symtab Section ====================
        // NULL symbol
        elf.extend_from_slice(&[0u8; 24]);
        // entrypoint symbol
        elf.extend_from_slice(&(entrypoint_str_idx as u32).to_le_bytes()); // st_name
        elf.push((STB_GLOBAL << 4) | STT_FUNC); // st_info
        elf.push(0); // st_other
        elf.extend_from_slice(&1u16.to_le_bytes()); // st_shndx (.text = 1)
        elf.extend_from_slice(&TEXT_VADDR.to_le_bytes()); // st_value
        elf.extend_from_slice(&(text_size as u64).to_le_bytes()); // st_size

        // ==================== .shstrtab Section ====================
        elf.extend_from_slice(&self.shstrtab);

        // ==================== Padding ====================
        while elf.len() < shdr_offset {
            elf.push(0);
        }

        // ==================== Section Headers ====================
        // 0: NULL
        elf.extend_from_slice(&[0u8; 64]);

        // 1: .text
        self.write_shdr(
            &mut elf,
            text_name,
            SHT_PROGBITS,
            SHF_ALLOC | SHF_EXECINSTR,
            TEXT_VADDR,
            text_offset,
            text_size,
            0,
            0,
            0x1000,
            0,
        );

        // 2: .strtab
        self.write_shdr(
            &mut elf,
            strtab_name,
            SHT_STRTAB,
            0,
            0,
            strtab_offset,
            strtab_size,
            0,
            0,
            1,
            0,
        );

        // 3: .symtab
        self.write_shdr(
            &mut elf,
            symtab_name,
            SHT_SYMTAB,
            0,
            0,
            symtab_offset,
            symtab_size,
            2,
            1,
            8,
            symtab_entry_size,
        );

        // 4: .shstrtab
        self.write_shdr(
            &mut elf,
            1,
            SHT_STRTAB,
            0,
            0,
            shstrtab_offset,
            shstrtab_size,
            0,
            0,
            1,
            0,
        );

        let _ = (stack_name, heap_name); // Suppress unused warnings

        Ok(elf)
    }

    /// Write sBPF program with syscall support (dynamic linking)
    pub fn write_with_syscalls(
        &mut self,
        program: &[SbpfInstruction],
        syscalls: &[SyscallRef],
        string_loads: &[StringLoadRef],
        rodata: &[u8],
        _debug_info: bool,
        sbpf_version: super::SbpfVersion,
    ) -> Result<Vec<u8>> {
        if syscalls.is_empty() {
            return self.write(program, _debug_info, sbpf_version);
        }

        // Encode instructions
        let mut text_section: Vec<u8> = Vec::new();
        for instr in program {
            text_section.extend_from_slice(&instr.encode());
        }

        if text_section.is_empty() {
            return Err(Error::runtime("Cannot create ELF with empty program"));
        }

        // Build dynamic symbol table
        let mut dynsym_entries: Vec<(usize, String)> = Vec::new(); // (name_idx, name)
        let mut seen_syscalls: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();

        for sc in syscalls {
            if !seen_syscalls.contains_key(&sc.name) {
                let name_idx = self.add_dynstr(&sc.name);
                let sym_idx = dynsym_entries.len() + 1; // +1 for NULL entry
                seen_syscalls.insert(sc.name.clone(), sym_idx);
                dynsym_entries.push((name_idx, sc.name.clone()));
            }
        }

        // Build section names
        let _shstrtab_name = self.add_shstrtab(".shstrtab");
        let text_name = self.add_shstrtab(".text");
        let rodata_name = self.add_shstrtab(".rodata");
        let dynamic_name = self.add_shstrtab(".dynamic");
        let dynsym_name = self.add_shstrtab(".dynsym");
        let dynstr_name = self.add_shstrtab(".dynstr");
        let reldyn_name = self.add_shstrtab(".rel.dyn");
        // Removed .strtab and .symtab to match reference (not needed for deployment)

        // Layout (page-aligned):
        // [ELF Header: 64 bytes]
        // [Program Headers: 2 * 56 = 112 bytes]
        // [Padding to 0x120]
        // [.text section at 0x120]
        // [.dynamic section]
        // [.dynsym section]
        // [.dynstr section]
        // [.rel.dyn section]
        // [.strtab section]
        // [.symtab section]
        // [.shstrtab section]
        // [Section Headers]

        let ehdr_size = 64usize;
        let phdr_size = 56usize;
        let shdr_size = 64usize;
        let num_phdrs = 4usize; // PT_LOAD (.text), PT_LOAD (.rodata), PT_LOAD (dynamic sections), PT_DYNAMIC
        let num_sections = 8usize; // NULL, .text, .rodata, .dynamic, .dynsym, .dynstr, .rel.dyn, .shstrtab (matching reference)

        let text_offset = 0x120usize; // Match Solana's working ELFs
        let text_size = text_section.len();

        // .rodata section with actual string literals
        let rodata_offset = text_offset + text_size;
        let rodata_size = if rodata.is_empty() {
            8usize // Minimum 8 bytes if no strings
        } else {
            rodata.len()
        };
        let rodata_data = if rodata.is_empty() {
            vec![0u8; 8] // Fallback to zeros if empty
        } else {
            rodata.to_vec() // Use actual string data
        };

        // .dynamic section (11 entries * 16 bytes = 176 bytes) - MUST be 8-byte aligned!
        // FLAGS, REL, RELSZ, RELENT, RELCOUNT, SYMTAB, SYMENT, STRTAB, STRSZ, TEXTREL, NULL
        // CRITICAL: Must align to 8 bytes because .dynamic entries are 16 bytes each
        let dynamic_offset = ((rodata_offset + rodata_size) + 7) & !7;
        let dynamic_size = 11 * 16; // 11 entries (includes DT_TEXTREL required by Solana)

        // .dynsym section (NULL + N symbols * 24 bytes)
        let dynsym_offset = dynamic_offset + dynamic_size;
        let dynsym_entry_size = 24usize;
        let dynsym_size = dynsym_entry_size * (1 + dynsym_entries.len());

        // .dynstr section
        let dynstr_offset = dynsym_offset + dynsym_size;
        let dynstr_size = self.dynstr.len();

        // .rel.dyn section (N relocations * 16 bytes)
        // CRITICAL: Must be 8-byte aligned for Elf64Rel casting
        let reldyn_offset = ((dynstr_offset + dynstr_size) + 7) & !7;
        let reldyn_entry_size = 16usize;
        let reldyn_size = reldyn_entry_size * syscalls.len();

        // .shstrtab section (removed .strtab and .symtab to match reference)
        let shstrtab_offset = reldyn_offset + reldyn_size;
        let shstrtab_size = self.shstrtab.len();

        let shdr_offset = ((shstrtab_offset + shstrtab_size) + 7) & !7;

        // Virtual addresses (continuous in memory)
        // Virtual addresses must not overlap
        let rodata_vaddr = TEXT_VADDR + text_size as u64;
        // CRITICAL: .dynamic vaddr must also be 8-byte aligned like its file offset
        let dynamic_vaddr = ((rodata_vaddr + rodata_size as u64) + 7) & !7;
        let dynsym_vaddr = dynamic_vaddr + dynamic_size as u64;
        let dynstr_vaddr = dynsym_vaddr + dynsym_size as u64;
        // Must align to 8 bytes to match file offset alignment (for Elf64Rel)
        let reldyn_vaddr = ((dynstr_vaddr + dynstr_size as u64) + 7) & !7;

        // Patch LDDW instructions that load string pointers
        // LDDW is a 16-byte instruction: [8 bytes first half] [8 bytes second half]
        // First half: opcode=0x18, dst, src=0, off=0, imm=low32
        // Second half: opcode=0x00, dst=0, src=0, off=0, imm=high32
        //
        // CRITICAL: For SBPFv1, Solana's VM loads programs at MM_PROGRAM_START (0x100000000)
        // so string addresses must include this base address!
        for load_site in string_loads {
            // Full runtime address: MM_PROGRAM_START + ELF vaddr + string offset
            let abs_addr = MM_PROGRAM_START + rodata_vaddr + load_site.rodata_offset as u64;
            let low32 = (abs_addr & 0xFFFF_FFFF) as u32;
            let high32 = (abs_addr >> 32) as u32;

            // Patch the immediate fields in both halves of LDDW
            let offset = load_site.offset;
            if offset + 16 <= text_section.len() {
                // First half: bytes 4-7 contain low32 immediate
                text_section[offset + 4..offset + 8].copy_from_slice(&low32.to_le_bytes());
                // Second half: bytes 12-15 contain high32 immediate
                text_section[offset + 12..offset + 16].copy_from_slice(&high32.to_le_bytes());
            }
        }

        // Build ELF
        let mut elf = Vec::new();

        // ==================== ELF Header ====================
        elf.extend_from_slice(&ELF_MAGIC);
        elf.push(ELFCLASS64);
        elf.push(ELFDATA2LSB);
        elf.push(EV_CURRENT);
        elf.push(ELFOSABI_NONE);
        elf.extend_from_slice(&[0u8; 8]);

        elf.extend_from_slice(&ET_DYN.to_le_bytes());
        elf.extend_from_slice(&EM_SBF.to_le_bytes());
        elf.extend_from_slice(&1u32.to_le_bytes());
        elf.extend_from_slice(&TEXT_VADDR.to_le_bytes()); // e_entry
        elf.extend_from_slice(&(ehdr_size as u64).to_le_bytes()); // e_phoff
        elf.extend_from_slice(&(shdr_offset as u64).to_le_bytes()); // e_shoff
                                                                    // Use appropriate flags based on SBPF version
        let ef_flags = match sbpf_version {
            super::SbpfVersion::V1 => EF_SBF_V1,
            super::SbpfVersion::V2 => EF_SBF_V2,
        };
        elf.extend_from_slice(&ef_flags.to_le_bytes()); // e_flags
        elf.extend_from_slice(&(ehdr_size as u16).to_le_bytes());
        elf.extend_from_slice(&(phdr_size as u16).to_le_bytes());
        elf.extend_from_slice(&(num_phdrs as u16).to_le_bytes());
        elf.extend_from_slice(&(shdr_size as u16).to_le_bytes());
        elf.extend_from_slice(&(num_sections as u16).to_le_bytes());
        elf.extend_from_slice(&((num_sections - 1) as u16).to_le_bytes()); // e_shstrndx

        // ==================== Program Headers ====================
        // PT_LOAD #1: .text only (R+X)
        self.write_phdr_aligned(
            &mut elf,
            PT_LOAD,
            PF_R | PF_X,
            text_offset,
            TEXT_VADDR,
            text_size,
        );

        // PT_LOAD #2: .rodata (R+W) - matches reference structure
        self.write_phdr_aligned(
            &mut elf,
            PT_LOAD,
            PF_R | PF_W,
            rodata_offset,
            rodata_vaddr,
            rodata_size,
        );

        // PT_LOAD #3: Dynamic sections (.dynsym, .dynstr, .rel.dyn) - READ-ONLY like reference!
        // These sections are metadata and don't need write access
        // IMPORTANT: Calculate actual file span including alignment padding between sections
        let dyn_sections_size = (reldyn_offset + reldyn_size) - dynsym_offset;
        self.write_phdr_aligned(
            &mut elf,
            PT_LOAD,
            PF_R,
            dynsym_offset,
            dynsym_vaddr,
            dyn_sections_size,
        );

        // PT_DYNAMIC: Points to .dynamic section (needs 8-byte alignment, not page alignment)
        elf.extend_from_slice(&PT_DYNAMIC.to_le_bytes());
        elf.extend_from_slice(&(PF_R | PF_W).to_le_bytes());
        elf.extend_from_slice(&(dynamic_offset as u64).to_le_bytes());
        elf.extend_from_slice(&dynamic_vaddr.to_le_bytes());
        elf.extend_from_slice(&dynamic_vaddr.to_le_bytes());
        elf.extend_from_slice(&(dynamic_size as u64).to_le_bytes());
        elf.extend_from_slice(&(dynamic_size as u64).to_le_bytes());
        elf.extend_from_slice(&0x8u64.to_le_bytes()); // 8-byte alignment for dynamic entries

        // Padding to 0x120 for .text section
        while elf.len() < text_offset {
            elf.push(0);
        }

        // ==================== .text Section ====================
        elf.extend_from_slice(&text_section);

        // ==================== .rodata Section ====================
        elf.extend_from_slice(&rodata_data);

        // Add padding to align .dynamic section to 8 bytes
        let padding_needed = dynamic_offset - (rodata_offset + rodata_size);
        elf.resize(elf.len() + padding_needed, 0);

        // ==================== .dynamic Section ====================
        // Match Solana's test ELF format
        // DT_FLAGS (TEXTREL flag = 0x4, matching Solana's test ELF)
        elf.extend_from_slice(&DT_FLAGS.to_le_bytes());
        elf.extend_from_slice(&0x4u64.to_le_bytes()); // DF_TEXTREL flag
                                                      // DT_REL
        elf.extend_from_slice(&DT_REL.to_le_bytes());
        elf.extend_from_slice(&reldyn_vaddr.to_le_bytes());
        // DT_RELSZ
        elf.extend_from_slice(&DT_RELSZ.to_le_bytes());
        elf.extend_from_slice(&(reldyn_size as u64).to_le_bytes());
        // DT_RELENT
        elf.extend_from_slice(&DT_RELENT.to_le_bytes());
        elf.extend_from_slice(&(reldyn_entry_size as u64).to_le_bytes());
        // DT_RELCOUNT (number of relocations = number of syscalls)
        elf.extend_from_slice(&DT_RELCOUNT.to_le_bytes());
        elf.extend_from_slice(&(syscalls.len() as u64).to_le_bytes());
        // DT_SYMTAB
        elf.extend_from_slice(&DT_SYMTAB.to_le_bytes());
        elf.extend_from_slice(&dynsym_vaddr.to_le_bytes());
        // DT_SYMENT (size of symbol table entry = 24 bytes)
        elf.extend_from_slice(&DT_SYMENT.to_le_bytes());
        elf.extend_from_slice(&24u64.to_le_bytes());
        // DT_STRTAB
        elf.extend_from_slice(&DT_STRTAB.to_le_bytes());
        elf.extend_from_slice(&dynstr_vaddr.to_le_bytes());
        // DT_STRSZ
        elf.extend_from_slice(&DT_STRSZ.to_le_bytes());
        elf.extend_from_slice(&(dynstr_size as u64).to_le_bytes());
        // DT_TEXTREL (required by Solana loader, even with DF_TEXTREL in FLAGS)
        elf.extend_from_slice(&DT_TEXTREL.to_le_bytes());
        elf.extend_from_slice(&0u64.to_le_bytes());
        // DT_NULL
        elf.extend_from_slice(&DT_NULL.to_le_bytes());
        elf.extend_from_slice(&0u64.to_le_bytes());

        // ==================== .dynsym Section ====================
        // NULL symbol
        elf.extend_from_slice(&[0u8; 24]);
        // Syscall symbols
        for (name_idx, _name) in &dynsym_entries {
            elf.extend_from_slice(&(*name_idx as u32).to_le_bytes()); // st_name
            elf.push((STB_GLOBAL << 4) | STT_FUNC); // st_info
            elf.push(0); // st_other
            elf.extend_from_slice(&0u16.to_le_bytes()); // st_shndx (undefined)
            elf.extend_from_slice(&0u64.to_le_bytes()); // st_value
            elf.extend_from_slice(&0u64.to_le_bytes()); // st_size
        }

        // ==================== .dynstr Section ====================
        elf.extend_from_slice(&self.dynstr);

        // Add padding to align .rel.dyn to 8 bytes (Elf64Rel requires 8-byte alignment)
        let padding_needed = reldyn_offset - (dynstr_offset + dynstr_size);
        elf.resize(elf.len() + padding_needed, 0);

        // ==================== .rel.dyn Section ====================
        for sc in syscalls {
            let sym_idx = *seen_syscalls.get(&sc.name).unwrap();
            // r_offset: address of the call instruction START (loader adds +4 for R_BPF_64_32)
            let r_offset = TEXT_VADDR + sc.offset as u64;
            elf.extend_from_slice(&r_offset.to_le_bytes());
            // r_info: symbol index + relocation type
            // Use R_BPF_64_32 for 32-bit immediate field relocations (syscalls)
            let r_info = ((sym_idx as u64) << 32) | (R_BPF_64_32 as u64);
            elf.extend_from_slice(&r_info.to_le_bytes());
        }

        // ==================== .shstrtab Section ====================
        // (Removed .strtab and .symtab sections to match reference - not needed for deployment)
        elf.extend_from_slice(&self.shstrtab);

        // ==================== Padding ====================
        while elf.len() < shdr_offset {
            elf.push(0);
        }

        // ==================== Section Headers ====================
        // 0: NULL
        elf.extend_from_slice(&[0u8; 64]);

        // 1: .text
        self.write_shdr(
            &mut elf,
            text_name,
            SHT_PROGBITS,
            SHF_ALLOC | SHF_EXECINSTR,
            TEXT_VADDR,
            text_offset,
            text_size,
            0,
            0,
            0x1000,
            0,
        );

        // 2: .rodata
        self.write_shdr(
            &mut elf,
            rodata_name,
            SHT_PROGBITS,
            SHF_ALLOC | SHF_WRITE,
            rodata_vaddr,
            rodata_offset,
            rodata_size,
            0,
            0,
            0x1,
            0,
        );

        // 3: .dynamic (Link=5 for .dynstr)
        self.write_shdr(
            &mut elf,
            dynamic_name,
            SHT_DYNAMIC,
            SHF_ALLOC | SHF_WRITE,
            dynamic_vaddr,
            dynamic_offset,
            dynamic_size,
            5,
            0,
            8,
            16,
        );

        // 4: .dynsym (Link=5 for .dynstr, Info=1)
        self.write_shdr(
            &mut elf,
            dynsym_name,
            SHT_DYNSYM,
            SHF_ALLOC,
            dynsym_vaddr,
            dynsym_offset,
            dynsym_size,
            5,
            1,
            8,
            dynsym_entry_size,
        );

        // 5: .dynstr
        self.write_shdr(
            &mut elf,
            dynstr_name,
            SHT_STRTAB,
            SHF_ALLOC,
            dynstr_vaddr,
            dynstr_offset,
            dynstr_size,
            0,
            0,
            1,
            0,
        );

        // 6: .rel.dyn (Link=4 for .dynsym)
        self.write_shdr(
            &mut elf,
            reldyn_name,
            SHT_REL,
            SHF_ALLOC,
            reldyn_vaddr,
            reldyn_offset,
            reldyn_size,
            4,
            0,
            8,
            reldyn_entry_size,
        );

        // 7: .shstrtab (removed .strtab and .symtab to match reference)
        self.write_shdr(
            &mut elf,
            1,
            SHT_STRTAB,
            0,
            0,
            shstrtab_offset,
            shstrtab_size,
            0,
            0,
            1,
            0,
        );

        Ok(elf)
    }

    fn write_phdr_aligned(
        &self,
        elf: &mut Vec<u8>,
        p_type: u32,
        p_flags: u32,
        p_offset: usize,
        p_vaddr: u64,
        p_size: usize,
    ) {
        elf.extend_from_slice(&p_type.to_le_bytes());
        elf.extend_from_slice(&p_flags.to_le_bytes());
        elf.extend_from_slice(&(p_offset as u64).to_le_bytes());
        elf.extend_from_slice(&p_vaddr.to_le_bytes());
        elf.extend_from_slice(&p_vaddr.to_le_bytes());
        elf.extend_from_slice(&(p_size as u64).to_le_bytes());
        elf.extend_from_slice(&(p_size as u64).to_le_bytes());
        elf.extend_from_slice(&0x1000u64.to_le_bytes()); // Page alignment
    }

    #[allow(clippy::too_many_arguments)]
    fn write_shdr(
        &self,
        elf: &mut Vec<u8>,
        sh_name: usize,
        sh_type: u32,
        sh_flags: u64,
        sh_addr: u64,
        sh_offset: usize,
        sh_size: usize,
        sh_link: u32,
        sh_info: u32,
        sh_addralign: u64,
        sh_entsize: usize,
    ) {
        elf.extend_from_slice(&(sh_name as u32).to_le_bytes());
        elf.extend_from_slice(&sh_type.to_le_bytes());
        elf.extend_from_slice(&sh_flags.to_le_bytes());
        elf.extend_from_slice(&sh_addr.to_le_bytes());
        elf.extend_from_slice(&(sh_offset as u64).to_le_bytes());
        elf.extend_from_slice(&(sh_size as u64).to_le_bytes());
        elf.extend_from_slice(&sh_link.to_le_bytes());
        elf.extend_from_slice(&sh_info.to_le_bytes());
        elf.extend_from_slice(&sh_addralign.to_le_bytes());
        elf.extend_from_slice(&(sh_entsize as u64).to_le_bytes());
    }

    fn add_strtab(&mut self, s: &str) -> usize {
        let idx = self.strtab.len();
        self.strtab.extend_from_slice(s.as_bytes());
        self.strtab.push(0);
        idx
    }

    fn add_shstrtab(&mut self, s: &str) -> usize {
        let idx = self.shstrtab.len();
        self.shstrtab.extend_from_slice(s.as_bytes());
        self.shstrtab.push(0);
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

    if data[0..4] != ELF_MAGIC {
        return Err(Error::runtime("Invalid ELF magic"));
    }

    if data[4] != ELFCLASS64 {
        return Err(Error::runtime("Not a 64-bit ELF"));
    }

    let machine = u16::from_le_bytes([data[18], data[19]]);
    if machine != EM_SBF && machine != 247 {
        return Err(Error::runtime(format!(
            "Not a BPF ELF: machine={}",
            machine
        )));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::SbpfVersion;

    #[test]
    fn test_elf_writer() {
        use crate::compiler::sbpf_codegen::{alu, SbpfInstruction};

        let mut writer = ElfWriter::new();
        let program = vec![
            SbpfInstruction::alu64_imm(alu::MOV, 0, 42),
            SbpfInstruction::exit(),
        ];

        let elf = writer.write(&program, false, SbpfVersion::V1).unwrap();
        assert!(elf.len() > 64);
        validate_sbpf_elf(&elf).unwrap();
    }
}
