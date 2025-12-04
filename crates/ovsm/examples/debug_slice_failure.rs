// Debug exactly why slice_from_section_header fails
use std::fs;
use std::mem;

fn main() {
    let elf_bytes = fs::read("/tmp/hello_final.so").expect("Failed to read ELF");

    println!("🔍 Debugging slice_from_section_header failure\n");

    // Find the .dynamic section header
    let e_shoff = u64::from_le_bytes(elf_bytes[40..48].try_into().unwrap());
    let e_shnum = u16::from_le_bytes(elf_bytes[60..62].try_into().unwrap());

    for i in 0..e_shnum {
        let shdr_offset = e_shoff as usize + (i as usize * 64);
        let sh_type = u32::from_le_bytes(
            elf_bytes[shdr_offset + 4..shdr_offset + 8]
                .try_into()
                .unwrap(),
        );

        if sh_type == 6 {
            // SHT_DYNAMIC
            println!("📊 Found .dynamic section header at index {}", i);

            let sh_offset = u64::from_le_bytes(
                elf_bytes[shdr_offset + 24..shdr_offset + 32]
                    .try_into()
                    .unwrap(),
            );
            let sh_size = u64::from_le_bytes(
                elf_bytes[shdr_offset + 32..shdr_offset + 40]
                    .try_into()
                    .unwrap(),
            );

            println!("  sh_offset: 0x{:x} ({})", sh_offset, sh_offset);
            println!("  sh_size:   0x{:x} ({})", sh_size, sh_size);

            // Check what slice_from_bytes would do
            let start = sh_offset as usize;
            let end = start + sh_size as usize;

            println!("\n🔍 slice_from_bytes checks:");
            println!("  Range: 0x{:x}..0x{:x}", start, end);
            println!("  File size: 0x{:x} ({})", elf_bytes.len(), elf_bytes.len());

            // Check 1: Is range within bounds?
            if end > elf_bytes.len() {
                println!("  ❌ FAIL: Range exceeds file size!");
                println!("     end ({}) > file_size ({})", end, elf_bytes.len());
                return;
            } else {
                println!("  ✅ Range is within bounds");
            }

            // Check 2: Is size divisible by entry size (16 bytes for Elf64Dyn)?
            let entry_size = 16; // sizeof(Elf64Dyn)
            if sh_size % entry_size != 0 {
                println!("  ❌ FAIL: Size not divisible by entry size!");
                println!(
                    "     {} % {} = {}",
                    sh_size,
                    entry_size,
                    sh_size % entry_size
                );
                return;
            } else {
                println!(
                    "  ✅ Size is divisible by {} (entries = {})",
                    entry_size,
                    sh_size / entry_size
                );
            }

            // Check 3: Is the pointer aligned?
            let bytes_slice = &elf_bytes[start..end];
            let ptr = bytes_slice.as_ptr();
            let alignment = mem::align_of::<u64>(); // Elf64Dyn contains u64s

            if (ptr as usize) % alignment != 0 {
                println!("  ❌ FAIL: Pointer not aligned!");
                println!(
                    "     ptr=0x{:x}, alignment={}, remainder={}",
                    ptr as usize,
                    alignment,
                    (ptr as usize) % alignment
                );
                return;
            } else {
                println!(
                    "  ✅ Pointer is aligned (ptr=0x{:x}, alignment={})",
                    ptr as usize, alignment
                );
            }

            // If we get here, everything should work
            println!("\n✅ All checks pass - slice_from_bytes should succeed!");

            // Let's also check the actual content
            println!("\n📊 Dynamic section content:");
            for j in 0..(sh_size / entry_size) {
                let entry_offset = start + (j as usize * entry_size as usize);
                let d_tag = u64::from_le_bytes(
                    elf_bytes[entry_offset..entry_offset + 8]
                        .try_into()
                        .unwrap(),
                );
                let d_val = u64::from_le_bytes(
                    elf_bytes[entry_offset + 8..entry_offset + 16]
                        .try_into()
                        .unwrap(),
                );

                let tag_name = match d_tag {
                    0 => "DT_NULL",
                    5 => "DT_STRTAB",
                    6 => "DT_SYMTAB",
                    10 => "DT_STRSZ",
                    11 => "DT_SYMENT",
                    17 => "DT_REL",
                    18 => "DT_RELSZ",
                    19 => "DT_RELENT",
                    30 => "DT_FLAGS",
                    0x6ffffffa => "DT_RELCOUNT",
                    _ => "Unknown",
                };

                println!(
                    "  Entry {}: {} (0x{:x}) = 0x{:x}",
                    j, tag_name, d_tag, d_val
                );

                if d_tag == 0 {
                    break;
                } // DT_NULL terminates
            }

            break;
        }
    }
}
