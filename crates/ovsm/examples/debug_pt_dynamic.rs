// Debug why PT_DYNAMIC parsing might fail
use std::fs;
use std::mem;

fn main() {
    let elf_bytes = fs::read("/tmp/hello_final.so").expect("Failed to read ELF");

    println!("🔍 Debugging PT_DYNAMIC parsing\n");

    // Find PT_DYNAMIC
    let e_phoff = u64::from_le_bytes(elf_bytes[32..40].try_into().unwrap());
    let e_phnum = u16::from_le_bytes(elf_bytes[56..58].try_into().unwrap());

    for i in 0..e_phnum {
        let phdr_offset = e_phoff as usize + (i as usize * 56);
        let p_type =
            u32::from_le_bytes(elf_bytes[phdr_offset..phdr_offset + 4].try_into().unwrap());

        if p_type == 2 {
            // PT_DYNAMIC
            println!("📊 Found PT_DYNAMIC at program header {}", i);

            let p_offset = u64::from_le_bytes(
                elf_bytes[phdr_offset + 8..phdr_offset + 16]
                    .try_into()
                    .unwrap(),
            );
            let p_filesz = u64::from_le_bytes(
                elf_bytes[phdr_offset + 32..phdr_offset + 40]
                    .try_into()
                    .unwrap(),
            );

            println!("  p_offset: 0x{:x} ({})", p_offset, p_offset);
            println!("  p_filesz: 0x{:x} ({})", p_filesz, p_filesz);

            // Check what slice_from_program_header would do
            println!("\n🔍 slice_from_program_header checks:");

            let start = p_offset as usize;
            let end = start + p_filesz as usize;

            println!("  Range: 0x{:x}..0x{:x}", start, end);
            println!("  File size: 0x{:x} ({})", elf_bytes.len(), elf_bytes.len());

            // Check 1: Range within bounds?
            if end > elf_bytes.len() {
                println!("  ❌ FAIL: Range exceeds file size!");
                return;
            }
            println!("  ✅ Range is within bounds");

            // Check 2: Size divisible by sizeof(Elf64Dyn) = 16?
            let entry_size = 16;
            if p_filesz % entry_size != 0 {
                println!("  ❌ FAIL: Size not divisible by {}", entry_size);
                return;
            }
            println!(
                "  ✅ Size is divisible by {} (entries = {})",
                entry_size,
                p_filesz / entry_size
            );

            // Check 3: Alignment
            let slice = &elf_bytes[start..end];
            let ptr = slice.as_ptr();
            let alignment = mem::align_of::<u64>();

            if (ptr as usize) % alignment != 0 {
                println!("  ❌ FAIL: Pointer not aligned!");
                println!("     ptr=0x{:x}, alignment={}", ptr as usize, alignment);
                return;
            }
            println!("  ✅ Pointer is aligned (ptr=0x{:x})", ptr as usize);

            // If we get here, slice_from_program_header should succeed
            println!("\n✅ PT_DYNAMIC slice_from_program_header should succeed!");

            // Let's also check if the content is valid
            println!("\n📊 PT_DYNAMIC content:");
            for j in 0..(p_filesz / entry_size) {
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

            // Now check if this matches what's in SHT_DYNAMIC
            println!("\n🔍 Comparing with SHT_DYNAMIC:");

            let e_shoff = u64::from_le_bytes(elf_bytes[40..48].try_into().unwrap());
            let e_shnum = u16::from_le_bytes(elf_bytes[60..62].try_into().unwrap());

            for k in 0..e_shnum {
                let shdr_offset = e_shoff as usize + (k as usize * 64);
                let sh_type = u32::from_le_bytes(
                    elf_bytes[shdr_offset + 4..shdr_offset + 8]
                        .try_into()
                        .unwrap(),
                );

                if sh_type == 6 {
                    // SHT_DYNAMIC
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

                    println!("  SHT_DYNAMIC section header:");
                    println!("    sh_offset: 0x{:x}", sh_offset);
                    println!("    sh_size:   0x{:x}", sh_size);

                    if sh_offset == p_offset && sh_size == p_filesz {
                        println!("  ✅ PT_DYNAMIC and SHT_DYNAMIC point to same data!");
                    } else {
                        println!("  ⚠️  PT_DYNAMIC and SHT_DYNAMIC differ!");
                        println!(
                            "     PT_DYNAMIC: offset=0x{:x}, size=0x{:x}",
                            p_offset, p_filesz
                        );
                        println!(
                            "     SHT_DYNAMIC: offset=0x{:x}, size=0x{:x}",
                            sh_offset, sh_size
                        );
                    }
                    break;
                }
            }

            return;
        }
    }

    println!("❌ No PT_DYNAMIC found!");
}
