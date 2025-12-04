// Create an ELF that exactly matches Solana's working structure
use std::fs;

fn main() {
    let mut elf = Vec::new();

    // ==================== ELF Header (matching Solana exactly) ====================
    elf.extend_from_slice(&[
        0x7f, 0x45, 0x4c, 0x46, // Magic
        0x02, 0x01, 0x01, 0x00, // 64-bit, little-endian, version 1
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // padding
        0x03, 0x00, 0x07, 0x01, // ET_DYN, Machine 0x107
        0x01, 0x00, 0x00, 0x00, // version
    ]);

    // Entry point at 0x120
    elf.extend_from_slice(&0x120u64.to_le_bytes());
    // Program headers at 64
    elf.extend_from_slice(&0x40u64.to_le_bytes());
    // Section headers (we'll place them later)
    let shoff = 0x3a0u64; // Like Solana
    elf.extend_from_slice(&shoff.to_le_bytes());
    // Flags - 0x0 for V1
    elf.extend_from_slice(&0x00u32.to_le_bytes());
    // Header sizes
    elf.extend_from_slice(&[
        0x40, 0x00, // ehsize = 64
        0x38, 0x00, // phentsize = 56
        0x04, 0x00, // phnum = 4
        0x40, 0x00, // shentsize = 64
        0x0b, 0x00, // shnum = 11 sections like Solana
        0x09, 0x00, // shstrndx = 9
    ]);

    // ==================== Program Headers ====================
    // PT_LOAD #1: .text (at 0x120)
    elf.extend_from_slice(&[
        0x01, 0x00, 0x00, 0x00, // PT_LOAD
        0x05, 0x00, 0x00, 0x00, // PF_R | PF_X
    ]);
    elf.extend_from_slice(&0x120u64.to_le_bytes()); // p_offset
    elf.extend_from_slice(&0x120u64.to_le_bytes()); // p_vaddr
    elf.extend_from_slice(&0x120u64.to_le_bytes()); // p_paddr
    elf.extend_from_slice(&0x30u64.to_le_bytes()); // p_filesz
    elf.extend_from_slice(&0x30u64.to_le_bytes()); // p_memsz
    elf.extend_from_slice(&0x1000u64.to_le_bytes()); // p_align

    // PT_LOAD #2: .rodata (at 0x150)
    elf.extend_from_slice(&[
        0x01, 0x00, 0x00, 0x00, // PT_LOAD
        0x04, 0x00, 0x00, 0x00, // PF_R
    ]);
    elf.extend_from_slice(&0x150u64.to_le_bytes()); // p_offset
    elf.extend_from_slice(&0x150u64.to_le_bytes()); // p_vaddr
    elf.extend_from_slice(&0x150u64.to_le_bytes()); // p_paddr
    elf.extend_from_slice(&0x4u64.to_le_bytes()); // p_filesz
    elf.extend_from_slice(&0x4u64.to_le_bytes()); // p_memsz
    elf.extend_from_slice(&0x1u64.to_le_bytes()); // p_align = 1

    // PT_LOAD #3: Dynamic sections
    elf.extend_from_slice(&[
        0x01, 0x00, 0x00, 0x00, // PT_LOAD
        0x04, 0x00, 0x00, 0x00, // PF_R
    ]);
    elf.extend_from_slice(&0x208u64.to_le_bytes()); // p_offset
    elf.extend_from_slice(&0x208u64.to_le_bytes()); // p_vaddr
    elf.extend_from_slice(&0x208u64.to_le_bytes()); // p_paddr
    elf.extend_from_slice(&0x78u64.to_le_bytes()); // p_filesz
    elf.extend_from_slice(&0x78u64.to_le_bytes()); // p_memsz
    elf.extend_from_slice(&0x1000u64.to_le_bytes()); // p_align

    // PT_DYNAMIC
    elf.extend_from_slice(&[
        0x02, 0x00, 0x00, 0x00, // PT_DYNAMIC
        0x06, 0x00, 0x00, 0x00, // PF_R | PF_W
    ]);
    elf.extend_from_slice(&0x158u64.to_le_bytes()); // p_offset
    elf.extend_from_slice(&0x158u64.to_le_bytes()); // p_vaddr
    elf.extend_from_slice(&0x158u64.to_le_bytes()); // p_paddr
    elf.extend_from_slice(&0xb0u64.to_le_bytes()); // p_filesz
    elf.extend_from_slice(&0xb0u64.to_le_bytes()); // p_memsz
    elf.extend_from_slice(&0x8u64.to_le_bytes()); // p_align

    // Padding to 0x120
    while elf.len() < 0x120 {
        elf.push(0);
    }

    // ==================== .text Section (at 0x120) ====================
    // Simple program that calls sol_log_ syscall
    // LDDW r0, <hash will be patched>
    elf.extend_from_slice(&[
        0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00,
    ]);
    // LDDW r1, 0x150 (rodata address)
    elf.extend_from_slice(&[
        0x18, 0x01, 0x00, 0x00, 0x50, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00,
    ]);
    // MOV r2, 4
    elf.extend_from_slice(&[0xb7, 0x02, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00]);
    // CALL with imm=-1 for V1 external call (doesn't validate as relative jump)
    elf.extend_from_slice(&[
        0x85, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, // imm=-1 for V1 syscall
    ]);
    // EXIT
    elf.extend_from_slice(&[0x95, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);

    // ==================== .rodata Section (at 0x150) ====================
    elf.extend_from_slice(&[0x04, 0x00, 0x00, 0x00]); // 4 as u32 (matching Solana)

    // Padding to 0x158
    while elf.len() < 0x158 {
        elf.push(0);
    }

    // ==================== .dynamic Section (at 0x158) ====================
    // Exact same order as Solana's file
    // DT_FLAGS (30) = 0x4
    elf.extend_from_slice(&30u64.to_le_bytes());
    elf.extend_from_slice(&0x4u64.to_le_bytes());
    // DT_REL (17)
    elf.extend_from_slice(&17u64.to_le_bytes());
    elf.extend_from_slice(&0x260u64.to_le_bytes());
    // DT_RELSZ (18)
    elf.extend_from_slice(&18u64.to_le_bytes());
    elf.extend_from_slice(&0x20u64.to_le_bytes());
    // DT_RELENT (19)
    elf.extend_from_slice(&19u64.to_le_bytes());
    elf.extend_from_slice(&0x10u64.to_le_bytes());
    // DT_RELCOUNT (0x6ffffffa)
    elf.extend_from_slice(&0x6ffffffau64.to_le_bytes());
    elf.extend_from_slice(&1u64.to_le_bytes());
    // DT_SYMTAB (6)
    elf.extend_from_slice(&6u64.to_le_bytes());
    elf.extend_from_slice(&0x208u64.to_le_bytes());
    // DT_SYMENT (11)
    elf.extend_from_slice(&11u64.to_le_bytes());
    elf.extend_from_slice(&24u64.to_le_bytes());
    // DT_STRTAB (5)
    elf.extend_from_slice(&5u64.to_le_bytes());
    elf.extend_from_slice(&0x250u64.to_le_bytes());
    // DT_STRSZ (10)
    elf.extend_from_slice(&10u64.to_le_bytes());
    elf.extend_from_slice(&0x10u64.to_le_bytes());
    // DT_SONAME (14)
    elf.extend_from_slice(&14u64.to_le_bytes());
    elf.extend_from_slice(&0u64.to_le_bytes());
    // DT_NULL
    elf.extend_from_slice(&0u64.to_le_bytes());
    elf.extend_from_slice(&0u64.to_le_bytes());

    // Padding to 0x208
    while elf.len() < 0x208 {
        elf.push(0);
    }

    // ==================== .dynsym Section (at 0x208) ====================
    // NULL symbol
    elf.extend_from_slice(&[0u8; 24]);
    // sol_log_ symbol
    elf.extend_from_slice(&1u32.to_le_bytes()); // st_name
    elf.push(0x12); // st_info
    elf.push(0); // st_other
    elf.extend_from_slice(&0u16.to_le_bytes()); // st_shndx
    elf.extend_from_slice(&0u64.to_le_bytes()); // st_value
    elf.extend_from_slice(&0u64.to_le_bytes()); // st_size
                                                // Empty symbol for padding
    elf.extend_from_slice(&[0u8; 24]);

    // ==================== .dynstr Section (at 0x250) ====================
    elf.push(0); // null
    elf.extend_from_slice(b"sol_log_\0");
    // Pad to 16 bytes
    while elf.len() < 0x260 {
        elf.push(0);
    }

    // ==================== .rel.dyn Section (at 0x260) ====================
    // Relocation for syscall
    elf.extend_from_slice(&0x124u64.to_le_bytes()); // r_offset
    let r_info = (1u64 << 32) | 10; // symbol 1, R_BPF_64_32
    elf.extend_from_slice(&r_info.to_le_bytes());
    // Padding entry
    elf.extend_from_slice(&[0u8; 16]);

    // ==================== .comment Section (at 0x280) ====================
    elf.extend_from_slice(b"Solana eBPF v1\0");
    while elf.len() < 0x293 {
        elf.push(0);
    }

    // Padding to 0x298 for .symtab
    while elf.len() < 0x298 {
        elf.push(0);
    }

    // ==================== .symtab Section (at 0x298) ====================
    // NULL symbol
    elf.extend_from_slice(&[0u8; 24]);
    // entrypoint symbol
    elf.extend_from_slice(&21u32.to_le_bytes()); // st_name (offset in .strtab)
    elf.push(0x12); // st_info (GLOBAL | FUNC)
    elf.push(0); // st_other
    elf.extend_from_slice(&1u16.to_le_bytes()); // st_shndx (.text)
    elf.extend_from_slice(&0x120u64.to_le_bytes()); // st_value
    elf.extend_from_slice(&0x30u64.to_le_bytes()); // st_size
                                                   // sol_log_ symbol
    elf.extend_from_slice(&1u32.to_le_bytes());
    elf.push(0x10); // st_info (GLOBAL)
    elf.push(0);
    elf.extend_from_slice(&0u16.to_le_bytes());
    elf.extend_from_slice(&0u64.to_le_bytes());
    elf.extend_from_slice(&0u64.to_le_bytes());
    // Another symbol
    elf.extend_from_slice(&[0u8; 24]);

    // ==================== .shstrtab Section (at 0x310) ====================
    while elf.len() < 0x310 {
        elf.push(0);
    }
    elf.push(0);
    elf.extend_from_slice(b".text\0");
    elf.extend_from_slice(b".rodata\0");
    elf.extend_from_slice(b".dynamic\0");
    elf.extend_from_slice(b".dynsym\0");
    elf.extend_from_slice(b".dynstr\0");
    elf.extend_from_slice(b".rel.dyn\0");
    elf.extend_from_slice(b".comment\0");
    elf.extend_from_slice(b".symtab\0");
    elf.extend_from_slice(b".shstrtab\0");
    elf.extend_from_slice(b".strtab\0");
    // Total should be 84 bytes (0x54)

    // ==================== .strtab Section (at 0x364) ====================
    while elf.len() < 0x364 {
        elf.push(0);
    }
    elf.push(0);
    elf.extend_from_slice(b"sol_log_\0");
    elf.extend_from_slice(b".Lmain\0");
    elf.extend_from_slice(b"entrypoint\0");
    elf.extend_from_slice(b"rodata\0");

    // Padding to section headers
    while elf.len() < 0x3a0 {
        elf.push(0);
    }

    // ==================== Section Headers (at 0x3a0) ====================
    // Section 0: NULL
    elf.extend_from_slice(&[0u8; 64]);

    // Section 1: .text
    write_section_header(&mut elf, 1, 1, 0x120, 0x120, 0x30, 0, 0x6, 0, 0, 8);

    // Section 2: .rodata
    write_section_header(&mut elf, 7, 1, 0x150, 0x150, 4, 4, 0x12, 0, 0, 1);

    // Section 3: .dynamic
    write_section_header(&mut elf, 15, 6, 0x158, 0x158, 0xb0, 0x10, 0x3, 5, 0, 8);

    // Section 4: .dynsym
    write_section_header(&mut elf, 24, 11, 0x208, 0x208, 0x48, 0x18, 0x2, 5, 1, 8);

    // Section 5: .dynstr
    write_section_header(&mut elf, 32, 3, 0x250, 0x250, 0x10, 0, 0x2, 0, 0, 1);

    // Section 6: .rel.dyn
    write_section_header(&mut elf, 40, 9, 0x260, 0x260, 0x20, 0x10, 0x2, 4, 0, 8);

    // Section 7: .comment
    write_section_header(&mut elf, 49, 1, 0, 0x280, 0x13, 1, 0x30, 0, 0, 1);

    // Section 8: .symtab
    write_section_header(&mut elf, 58, 2, 0, 0x298, 0x78, 0x18, 0, 10, 3, 8);

    // Section 9: .shstrtab (THIS IS THE KEY SECTION)
    write_section_header(&mut elf, 66, 3, 0, 0x310, 0x54, 0, 0, 0, 0, 1);

    // Section 10: .strtab
    write_section_header(&mut elf, 76, 3, 0, 0x364, 0x3c, 0, 0, 0, 0, 1);

    // Write the file
    fs::write("/tmp/solana_v1.so", &elf).expect("Failed to write ELF");
    println!("✅ Generated /tmp/solana_v1.so ({} bytes)", elf.len());
    println!("\nThis ELF closely matches Solana's working syscall_reloc_64_32.so");
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
