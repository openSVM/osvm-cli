// Create a minimal valid sBPF ELF that RBPF can load
// This version has proper layout and addresses
use std::fs;

fn main() {
    // Memory layout (matching Solana's working ELF):
    // 0x000-0x040: ELF header
    // 0x040-0x1b0: Program headers (4 * 56 = 224 bytes)
    // 0x1b0-0x11f: Padding
    // 0x120-0x150: .text section (48 bytes)
    // 0x150-0x154: .rodata section (4 bytes)
    // 0x158-0x208: .dynamic section (176 bytes)
    // 0x3b0-0x3ff: Padding
    // 0x400-0x448: .dynsym section (72 bytes = 3 symbols)
    // 0x448-0x44f: Padding
    // 0x450-0x460: .dynstr section (16 bytes)
    // 0x460-0x47f: Padding
    // 0x480-0x4a0: .rel.dyn section (32 bytes = 2 relocations)
    // 0x4a0-0x4ff: Padding
    // 0x500-0x504: .rodata section (4 bytes)
    // 0x504-0x5ff: Padding
    // 0x600-0x9c0: Section headers (7 * 64 = 448 bytes)
    // 0x9c0-0xa00: .shstrtab section

    let mut elf = Vec::new();

    // ==================== ELF Header (64 bytes) ====================
    elf.extend_from_slice(&[
        0x7f, 0x45, 0x4c, 0x46, // Magic
        0x02, 0x01, 0x01, 0x00, // 64-bit, little-endian, version 1
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // padding
        0x03, 0x00, 0x07, 0x01, // ET_DYN, Machine 0x107 like Solana
        0x01, 0x00, 0x00, 0x00, // version
    ]);

    // Entry point at start of .text
    elf.extend_from_slice(&0x120u64.to_le_bytes()); // Use 0x120 like Solana
                                                    // Program headers immediately after ELF header
    elf.extend_from_slice(&0x40u64.to_le_bytes());
    // Section headers at 0x600
    elf.extend_from_slice(&0x600u64.to_le_bytes());
    // Flags - 0x0 for V1 like Solana's working ELF
    elf.extend_from_slice(&0x00u32.to_le_bytes());
    // Header sizes
    elf.extend_from_slice(&[
        0x40, 0x00, // ehsize = 64
        0x38, 0x00, // phentsize = 56
        0x04, 0x00, // phnum = 4
        0x40, 0x00, // shentsize = 64
        0x08, 0x00, // shnum = 8 sections
        0x07, 0x00, // shstrndx = 7
    ]);

    // ==================== Program Headers (4 * 56 = 224 bytes) ====================
    // NOTE: Virtual addresses MUST be in ascending order!

    // PT_LOAD #1: .text (vaddr 0x200)
    elf.extend_from_slice(&[
        0x01, 0x00, 0x00, 0x00, // PT_LOAD
        0x05, 0x00, 0x00, 0x00, // PF_R | PF_X
    ]);
    elf.extend_from_slice(&0x200u64.to_le_bytes()); // p_offset
    elf.extend_from_slice(&0x200u64.to_le_bytes()); // p_vaddr
    elf.extend_from_slice(&0x200u64.to_le_bytes()); // p_paddr
    elf.extend_from_slice(&0x30u64.to_le_bytes()); // p_filesz (48 bytes)
    elf.extend_from_slice(&0x30u64.to_le_bytes()); // p_memsz
    elf.extend_from_slice(&0x1000u64.to_le_bytes()); // p_align

    // PT_DYNAMIC (vaddr 0x300)
    elf.extend_from_slice(&[
        0x02, 0x00, 0x00, 0x00, // PT_DYNAMIC
        0x06, 0x00, 0x00, 0x00, // PF_R | PF_W
    ]);
    elf.extend_from_slice(&0x300u64.to_le_bytes()); // p_offset
    elf.extend_from_slice(&0x300u64.to_le_bytes()); // p_vaddr
    elf.extend_from_slice(&0x300u64.to_le_bytes()); // p_paddr
    elf.extend_from_slice(&0xb0u64.to_le_bytes()); // p_filesz
    elf.extend_from_slice(&0xb0u64.to_le_bytes()); // p_memsz
    elf.extend_from_slice(&0x8u64.to_le_bytes()); // p_align

    // PT_LOAD #2: Dynamic sections (vaddr 0x400)
    elf.extend_from_slice(&[
        0x01, 0x00, 0x00, 0x00, // PT_LOAD
        0x04, 0x00, 0x00, 0x00, // PF_R
    ]);
    elf.extend_from_slice(&0x400u64.to_le_bytes()); // p_offset (.dynsym start)
    elf.extend_from_slice(&0x400u64.to_le_bytes()); // p_vaddr
    elf.extend_from_slice(&0x400u64.to_le_bytes()); // p_paddr
    elf.extend_from_slice(&0xa0u64.to_le_bytes()); // p_filesz (covers all dynamic sections)
    elf.extend_from_slice(&0xa0u64.to_le_bytes()); // p_memsz
    elf.extend_from_slice(&0x1000u64.to_le_bytes()); // p_align

    // PT_LOAD #3: .rodata (vaddr 0x500)
    elf.extend_from_slice(&[
        0x01, 0x00, 0x00, 0x00, // PT_LOAD
        0x04, 0x00, 0x00, 0x00, // PF_R
    ]);
    elf.extend_from_slice(&0x500u64.to_le_bytes()); // p_offset
    elf.extend_from_slice(&0x500u64.to_le_bytes()); // p_vaddr
    elf.extend_from_slice(&0x500u64.to_le_bytes()); // p_paddr
    elf.extend_from_slice(&0x4u64.to_le_bytes()); // p_filesz
    elf.extend_from_slice(&0x4u64.to_le_bytes()); // p_memsz
    elf.extend_from_slice(&0x1000u64.to_le_bytes()); // p_align

    // Padding to 0x200
    while elf.len() < 0x200 {
        elf.push(0);
    }

    // ==================== .text Section (at 0x200, 48 bytes) ====================
    // LDDW r0, 0 (will be patched with sol_log_ hash)
    elf.extend_from_slice(&[
        0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00,
    ]);
    // LDDW r1, 0x500 (address of our string in .rodata)
    elf.extend_from_slice(&[
        0x18, 0x01, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00,
    ]);
    // MOV r2, 4 (string length)
    elf.extend_from_slice(&[0xb7, 0x02, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00]);
    // CALL (syscall - src=0 for static syscall)
    elf.extend_from_slice(&[0x85, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
    // EXIT
    elf.extend_from_slice(&[0x95, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);

    // Padding to 0x300
    while elf.len() < 0x300 {
        elf.push(0);
    }

    // ==================== .dynamic Section (at 0x300, 176 bytes) ====================
    // DT_FLAGS (TEXTREL)
    elf.extend_from_slice(&30u64.to_le_bytes());
    elf.extend_from_slice(&0x4u64.to_le_bytes());
    // DT_REL
    elf.extend_from_slice(&17u64.to_le_bytes());
    elf.extend_from_slice(&0x480u64.to_le_bytes());
    // DT_RELSZ
    elf.extend_from_slice(&18u64.to_le_bytes());
    elf.extend_from_slice(&0x20u64.to_le_bytes());
    // DT_RELENT
    elf.extend_from_slice(&19u64.to_le_bytes());
    elf.extend_from_slice(&16u64.to_le_bytes());
    // DT_RELCOUNT
    elf.extend_from_slice(&0x6ffffffau64.to_le_bytes());
    elf.extend_from_slice(&0u64.to_le_bytes());
    // DT_SYMTAB
    elf.extend_from_slice(&6u64.to_le_bytes());
    elf.extend_from_slice(&0x400u64.to_le_bytes());
    // DT_SYMENT
    elf.extend_from_slice(&11u64.to_le_bytes());
    elf.extend_from_slice(&24u64.to_le_bytes());
    // DT_STRTAB
    elf.extend_from_slice(&5u64.to_le_bytes());
    elf.extend_from_slice(&0x450u64.to_le_bytes());
    // DT_STRSZ
    elf.extend_from_slice(&10u64.to_le_bytes());
    elf.extend_from_slice(&16u64.to_le_bytes());
    // DT_SONAME
    elf.extend_from_slice(&14u64.to_le_bytes());
    elf.extend_from_slice(&0u64.to_le_bytes());
    // DT_NULL
    elf.extend_from_slice(&0u64.to_le_bytes());
    elf.extend_from_slice(&0u64.to_le_bytes());

    // Padding to 0x400
    while elf.len() < 0x400 {
        elf.push(0);
    }

    // ==================== .dynsym Section (at 0x400, 72 bytes) ====================
    // Symbol 0: NULL
    elf.extend_from_slice(&[0u8; 24]);
    // Symbol 1: sol_log_
    elf.extend_from_slice(&1u32.to_le_bytes()); // st_name
    elf.push(0x12); // st_info (STB_GLOBAL | STT_FUNC)
    elf.push(0); // st_other
    elf.extend_from_slice(&0u16.to_le_bytes()); // st_shndx
    elf.extend_from_slice(&0u64.to_le_bytes()); // st_value
    elf.extend_from_slice(&0u64.to_le_bytes()); // st_size
                                                // Symbol 2: padding
    elf.extend_from_slice(&[0u8; 24]);

    // Padding to 0x450
    while elf.len() < 0x450 {
        elf.push(0);
    }

    // ==================== .dynstr Section (at 0x450, 16 bytes) ====================
    elf.push(0); // null
    elf.extend_from_slice(b"sol_log_\0");
    while elf.len() < 0x460 {
        elf.push(0); // padding
    }

    // Padding to 0x480
    while elf.len() < 0x480 {
        elf.push(0);
    }

    // ==================== .rel.dyn Section (at 0x480, 32 bytes) ====================
    // Relocation for the CALL instruction's immediate field
    elf.extend_from_slice(&0x204u64.to_le_bytes()); // r_offset (.text+4 for imm field)
    let r_info = (1u64 << 32) | 10; // symbol 1, type R_BPF_64_32
    elf.extend_from_slice(&r_info.to_le_bytes());
    // Padding relocation
    elf.extend_from_slice(&[0u8; 16]);

    // Padding to 0x500
    while elf.len() < 0x500 {
        elf.push(0);
    }

    // ==================== .rodata Section (at 0x500, 4 bytes) ====================
    elf.extend_from_slice(b"hi!\0");

    // Padding to 0x600
    while elf.len() < 0x600 {
        elf.push(0);
    }

    // ==================== Section Headers (at 0x600, 8 * 64 = 512 bytes) ====================
    // NOTE: Sections MUST be in ascending order by virtual address!
    // Section 0: NULL
    elf.extend_from_slice(&[0u8; 64]);

    // Section 1: .text (vaddr 0x200)
    write_section_header(&mut elf, 1, 1, 0x200, 0x200, 0x30, 0, 0x6, 0, 0, 8);

    // Section 2: .dynamic (vaddr 0x300)
    write_section_header(&mut elf, 15, 6, 0x300, 0x300, 0xb0, 0x10, 0x3, 4, 0, 8);

    // Section 3: .dynsym (vaddr 0x400)
    write_section_header(&mut elf, 24, 11, 0x400, 0x400, 0x48, 0x18, 0x2, 4, 1, 8);

    // Section 4: .dynstr (vaddr 0x450)
    write_section_header(&mut elf, 32, 3, 0x450, 0x450, 0x10, 0, 0x2, 0, 0, 1);

    // Section 5: .rel.dyn (vaddr 0x480)
    write_section_header(&mut elf, 40, 9, 0x480, 0x480, 0x20, 0x10, 0x2, 3, 1, 8);

    // Section 6: .rodata (vaddr 0x500)
    write_section_header(&mut elf, 7, 1, 0x500, 0x500, 4, 0, 0x2, 0, 0, 1);

    // Section 7: .shstrtab (no vaddr - not loaded)
    // It starts right after section headers: 0x600 + 8*64 = 0x600 + 0x200 = 0x800
    write_section_header(&mut elf, 49, 3, 0, 0x800, 58, 0, 0, 0, 0, 1);

    // ==================== .shstrtab Section (at 0x800) ====================
    elf.push(0); // null
    elf.extend_from_slice(b".text\0"); // 1
    elf.extend_from_slice(b".rodata\0"); // 7
    elf.extend_from_slice(b".dynamic\0"); // 15
    elf.extend_from_slice(b".dynsym\0"); // 24
    elf.extend_from_slice(b".dynstr\0"); // 32
    elf.extend_from_slice(b".rel.dyn\0"); // 40
    elf.extend_from_slice(b".shstrtab\0"); // 49

    // Pad with extra bytes so RBPF can safely read up to 16 bytes from any offset
    for _ in 0..32 {
        elf.push(0);
    }

    // Write the file
    fs::write("/tmp/minimal_sbpf.so", &elf).expect("Failed to write ELF");
    println!("✅ Generated /tmp/minimal_sbpf.so ({} bytes)", elf.len());
    println!("\nThis ELF has proper memory layout for sBPF:");
    println!("  .text:     0x200-0x230");
    println!("  .rodata:   0x500-0x504");
    println!("  .dynamic:  0x300-0x3b0");
    println!("  .dynsym:   0x400-0x448");
    println!("  .dynstr:   0x450-0x460");
    println!("  .rel.dyn:  0x480-0x4a0");
}

fn write_section_header(
    elf: &mut Vec<u8>,
    sh_name: u32,
    sh_type: u32,
    sh_addr: u64,
    sh_offset: u64,
    sh_size: u64,
    sh_entsize: u64,
    sh_flags: u64,
    sh_link: u32,
    sh_info: u32,
    sh_addralign: u64,
) {
    elf.extend_from_slice(&sh_name.to_le_bytes());
    elf.extend_from_slice(&sh_type.to_le_bytes());
    elf.extend_from_slice(&sh_flags.to_le_bytes());
    elf.extend_from_slice(&sh_addr.to_le_bytes());
    elf.extend_from_slice(&sh_offset.to_le_bytes());
    elf.extend_from_slice(&sh_size.to_le_bytes());
    elf.extend_from_slice(&sh_link.to_le_bytes());
    elf.extend_from_slice(&sh_info.to_le_bytes());
    elf.extend_from_slice(&sh_addralign.to_le_bytes());
    elf.extend_from_slice(&sh_entsize.to_le_bytes());
}
