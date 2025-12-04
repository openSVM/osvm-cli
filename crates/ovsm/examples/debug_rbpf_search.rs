// Debug exactly what RBPF is looking for when it fails
use std::fs;

fn main() {
    let elf_bytes = fs::read("/tmp/hello_final.so").expect("Failed to read ELF");

    println!("🔍 Debugging RBPF section header search\n");

    // Parse dynamic entries from PT_DYNAMIC
    let e_phoff = u64::from_le_bytes(elf_bytes[32..40].try_into().unwrap());
    let e_phnum = u16::from_le_bytes(elf_bytes[56..58].try_into().unwrap());

    let mut dt_symtab = 0u64;
    let mut dt_rel = 0u64;

    for i in 0..e_phnum {
        let phdr_offset = e_phoff as usize + (i as usize * 56);
        let p_type =
            u32::from_le_bytes(elf_bytes[phdr_offset..phdr_offset + 4].try_into().unwrap());

        if p_type == 2 {
            // PT_DYNAMIC
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

            // Parse dynamic entries
            for j in 0..(p_filesz / 16) {
                let entry_offset = p_offset as usize + (j as usize * 16);
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

                match d_tag {
                    6 => dt_symtab = d_val,
                    17 => dt_rel = d_val,
                    0 => break,
                    _ => {}
                }
            }
            break;
        }
    }

    println!("📊 Dynamic table values:");
    println!("  DT_SYMTAB = 0x{:x}", dt_symtab);
    println!("  DT_REL    = 0x{:x}", dt_rel);

    // Now check section headers
    let e_shoff = u64::from_le_bytes(elf_bytes[40..48].try_into().unwrap());
    let e_shnum = u16::from_le_bytes(elf_bytes[60..62].try_into().unwrap());

    println!("\n📊 Section headers (total: {}):", e_shnum);

    let mut found_symtab_section = false;
    let mut found_rel_section = false;

    for i in 0..e_shnum {
        let shdr_offset = e_shoff as usize + (i as usize * 64);

        if shdr_offset + 64 > elf_bytes.len() {
            println!("  ❌ Section {} header out of bounds!", i);
            continue;
        }

        let sh_name =
            u32::from_le_bytes(elf_bytes[shdr_offset..shdr_offset + 4].try_into().unwrap());
        let sh_type = u32::from_le_bytes(
            elf_bytes[shdr_offset + 4..shdr_offset + 8]
                .try_into()
                .unwrap(),
        );
        let sh_flags = u64::from_le_bytes(
            elf_bytes[shdr_offset + 8..shdr_offset + 16]
                .try_into()
                .unwrap(),
        );
        let sh_addr = u64::from_le_bytes(
            elf_bytes[shdr_offset + 16..shdr_offset + 24]
                .try_into()
                .unwrap(),
        );
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

        let type_str = match sh_type {
            0 => "NULL",
            1 => "PROGBITS",
            2 => "SYMTAB",
            3 => "STRTAB",
            6 => "DYNAMIC",
            9 => "REL",
            11 => "DYNSYM",
            _ => "OTHER",
        };

        println!(
            "  [{}] type={} sh_addr=0x{:x} sh_offset=0x{:x} sh_size=0x{:x}",
            i, type_str, sh_addr, sh_offset, sh_size
        );

        // Check if this matches what RBPF is looking for
        if sh_addr == dt_symtab {
            println!("    ✅ MATCH for DT_SYMTAB!");
            found_symtab_section = true;
        }
        if sh_addr == dt_rel {
            println!("    ✅ MATCH for DT_REL!");
            found_rel_section = true;
        }
    }

    println!("\n🔍 RBPF search results:");
    if !found_symtab_section {
        println!("  ❌ NO section with sh_addr=0x{:x} (DT_SYMTAB)", dt_symtab);
        println!("     This would trigger InvalidDynamicSectionTable at line 420!");
    }
    if !found_rel_section {
        println!("  ❌ NO section with sh_addr=0x{:x} (DT_REL)", dt_rel);
        println!("     This would trigger InvalidDynamicSectionTable at line 401!");
    }
    if found_symtab_section && found_rel_section {
        println!("  ✅ All required sections found!");
        println!("  The error must be elsewhere...");
    }
}
