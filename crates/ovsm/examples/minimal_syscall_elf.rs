// Create a minimal ELF with syscall that exactly mirrors Solana's structure
use std::fs;

fn main() {
    // Build a minimal ELF that exactly matches Solana's syscall_reloc_64_32.so structure
    let mut elf = Vec::new();

    // ELF Header (64 bytes)
    elf.extend_from_slice(&[
        0x7f, 0x45, 0x4c, 0x46, // Magic
        0x02, 0x01, 0x01, 0x00, // 64-bit, little-endian, version 1
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // padding
        0x03, 0x00, 0xf7, 0x00, // ET_DYN, EM_BPF (0xf7)
        0x01, 0x00, 0x00, 0x00, // version
    ]);

    // Entry point at 0x120 (like Solana's)
    elf.extend_from_slice(&0x120u64.to_le_bytes());

    // Program header offset (immediately after header)
    elf.extend_from_slice(&0x40u64.to_le_bytes());

    // Section header offset (we'll calculate this)
    let shoff = 0x300u64; // Put sections at end
    elf.extend_from_slice(&shoff.to_le_bytes());

    // Flags - Set EF_SBPF_V2 (0x20) to enable static syscalls
    elf.extend_from_slice(&0x00000020u32.to_le_bytes());

    // Header sizes
    elf.extend_from_slice(&[
        0x40, 0x00, // ehsize = 64
        0x38, 0x00, // phentsize = 56
        0x04, 0x00, // phnum = 4 (matching Solana)
        0x40, 0x00, // shentsize = 64
        0x07, 0x00, // shnum = 7 sections
        0x06, 0x00, // shstrndx = 6
    ]);

    // Program Headers (4 * 56 bytes = 224 bytes)
    // PT_LOAD #1: .text
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

    // PT_LOAD #2: .rodata (small, like Solana's)
    elf.extend_from_slice(&[
        0x01, 0x00, 0x00, 0x00, // PT_LOAD
        0x04, 0x00, 0x00, 0x00, // PF_R
    ]);
    elf.extend_from_slice(&0x1000u64.to_le_bytes()); // p_offset - after .text in next page
    elf.extend_from_slice(&0x1000u64.to_le_bytes()); // p_vaddr
    elf.extend_from_slice(&0x1000u64.to_le_bytes()); // p_paddr
    elf.extend_from_slice(&0x4u64.to_le_bytes()); // p_filesz
    elf.extend_from_slice(&0x4u64.to_le_bytes()); // p_memsz
    elf.extend_from_slice(&0x1000u64.to_le_bytes()); // p_align

    // PT_LOAD #3: .dynsym, .dynstr, .rel.dyn
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
    elf.extend_from_slice(&0x8u64.to_le_bytes()); // p_align = 8!

    // Pad to 0x120 (start of .text)
    while elf.len() < 0x120 {
        elf.push(0);
    }

    // .text section (48 bytes) - simple syscall instruction
    // This is a minimal sBPF program that calls sol_log_
    elf.extend_from_slice(&[
        // Load immediate for syscall number (will be patched by relocation)
        0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // lddw r0, 0
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        // Load address of string into r1
        0x18, 0x01, 0x00, 0x00, 0x50, 0x01, 0x00, 0x00, // lddw r1, 0x150
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Load length into r2
        0xb7, 0x02, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, // mov r2, 4
        // Syscall
        0x85, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // call 0
        // Return
        0x95, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // exit
    ]);

    // .rodata at 0x150 (4 bytes)
    while elf.len() < 0x150 {
        elf.push(0);
    }
    elf.extend_from_slice(b"hi!\0");

    // Pad to 0x154 (0x150 + 4 bytes for "hi!\0")
    while elf.len() < 0x154 {
        elf.push(0);
    }

    // .dynamic at 0x158 (176 bytes) - NOW we're at the right place
    while elf.len() < 0x158 {
        elf.push(0);
    }

    // Dynamic entries (matching Solana's order exactly)
    // DT_FLAGS
    elf.extend_from_slice(&30u64.to_le_bytes());
    elf.extend_from_slice(&0x4u64.to_le_bytes());
    // DT_REL
    elf.extend_from_slice(&17u64.to_le_bytes());
    elf.extend_from_slice(&0x260u64.to_le_bytes());
    // DT_RELSZ
    elf.extend_from_slice(&18u64.to_le_bytes());
    elf.extend_from_slice(&0x20u64.to_le_bytes());
    // DT_RELENT
    elf.extend_from_slice(&19u64.to_le_bytes());
    elf.extend_from_slice(&16u64.to_le_bytes());
    // DT_RELCOUNT
    elf.extend_from_slice(&0x6ffffffau64.to_le_bytes());
    elf.extend_from_slice(&1u64.to_le_bytes()); // Set to 1 like Solana
                                                // DT_SYMTAB
    elf.extend_from_slice(&6u64.to_le_bytes());
    elf.extend_from_slice(&0x208u64.to_le_bytes());
    // DT_SYMENT
    elf.extend_from_slice(&11u64.to_le_bytes());
    elf.extend_from_slice(&24u64.to_le_bytes());
    // DT_STRTAB
    elf.extend_from_slice(&5u64.to_le_bytes());
    elf.extend_from_slice(&0x250u64.to_le_bytes());
    // DT_STRSZ
    elf.extend_from_slice(&10u64.to_le_bytes());
    elf.extend_from_slice(&16u64.to_le_bytes());
    // DT_SONAME
    elf.extend_from_slice(&14u64.to_le_bytes());
    elf.extend_from_slice(&0u64.to_le_bytes());
    // DT_NULL
    elf.extend_from_slice(&0u64.to_le_bytes());
    elf.extend_from_slice(&0u64.to_le_bytes());

    // .dynsym at 0x208 (72 bytes = 3 symbols)
    while elf.len() < 0x208 {
        elf.push(0);
    }

    // Symbol 0: NULL
    elf.extend_from_slice(&[0u8; 24]);

    // Symbol 1: sol_log_
    elf.extend_from_slice(&1u32.to_le_bytes()); // st_name
    elf.push(0x12); // st_info (STB_GLOBAL | STT_FUNC)
    elf.push(0); // st_other
    elf.extend_from_slice(&0u16.to_le_bytes()); // st_shndx
    elf.extend_from_slice(&0u64.to_le_bytes()); // st_value
    elf.extend_from_slice(&0u64.to_le_bytes()); // st_size

    // Symbol 2: another symbol for padding
    elf.extend_from_slice(&[0u8; 24]);

    // .dynstr at 0x250 (16 bytes)
    while elf.len() < 0x250 {
        elf.push(0);
    }
    elf.push(0); // null
    elf.extend_from_slice(b"sol_log_\0");
    while elf.len() < 0x260 {
        elf.push(0); // padding
    }

    // .rel.dyn at 0x260 (32 bytes = 2 relocations)
    // Relocation 1
    elf.extend_from_slice(&0x124u64.to_le_bytes()); // r_offset (in .text)
    let r_info = (1u64 << 32) | 10; // symbol 1, type R_BPF_64_32
    elf.extend_from_slice(&r_info.to_le_bytes());

    // Relocation 2 (padding)
    elf.extend_from_slice(&[0u8; 16]);

    // Pad to section headers
    while elf.len() < 0x300 {
        elf.push(0);
    }

    // Section headers (7 * 64 bytes)
    // Section 0: NULL
    elf.extend_from_slice(&[0u8; 64]);

    // Section 1: .text
    write_section_header(&mut elf, 1, 1, 0x120, 0x120, 0x30, 0, 0x6, 0, 0, 8);

    // Section 2: .rodata
    write_section_header(&mut elf, 7, 1, 0x150, 0x150, 4, 0, 0x2, 0, 0, 1);

    // Section 3: .dynamic
    write_section_header(&mut elf, 15, 6, 0x158, 0x158, 0xb0, 0x10, 0x3, 5, 0, 8);

    // Section 4: .dynsym
    write_section_header(&mut elf, 24, 11, 0x208, 0x208, 0x48, 0x18, 0x2, 5, 1, 8);

    // Section 5: .dynstr
    write_section_header(&mut elf, 32, 3, 0x250, 0x250, 0x10, 0, 0x2, 0, 0, 1);

    // Section 6: .shstrtab (section name string table)
    let shstrtab_offset = elf.len() + 64;
    write_section_header(
        &mut elf,
        40,
        3,
        0,
        shstrtab_offset as u64,
        50,
        0,
        0,
        0,
        0,
        1,
    );

    // .shstrtab contents
    elf.push(0); // null
    elf.extend_from_slice(b".text\0"); // 1
    elf.extend_from_slice(b".rodata\0"); // 7
    elf.extend_from_slice(b".dynamic\0"); // 15
    elf.extend_from_slice(b".dynsym\0"); // 24
    elf.extend_from_slice(b".dynstr\0"); // 32
    elf.extend_from_slice(b".shstrtab\0"); // 40

    // Write the file
    fs::write("/tmp/minimal_syscall.so", &elf).expect("Failed to write ELF");
    println!("✅ Generated /tmp/minimal_syscall.so ({} bytes)", elf.len());
    println!("\nThis ELF closely mirrors Solana's syscall_reloc_64_32.so structure");
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
