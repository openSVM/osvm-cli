// Trace exactly what RBPF is checking when it fails
use std::fs;

fn main() {
    let elf_bytes = fs::read("/tmp/hello_final.so").expect("Failed to read ELF");

    // Get ELF header values
    let e_phoff = u64::from_le_bytes(elf_bytes[32..40].try_into().unwrap());
    let e_phnum = u16::from_le_bytes(elf_bytes[56..58].try_into().unwrap());
    let e_shoff = u64::from_le_bytes(elf_bytes[40..48].try_into().unwrap());
    let e_shnum = u16::from_le_bytes(elf_bytes[60..62].try_into().unwrap());

    println!("🔍 Tracing RBPF parse_dynamic_symbol_table logic\n");

    // Find PT_DYNAMIC (exactly as RBPF does)
    let mut dynamic_offset = None;
    let mut dynamic_size = 0usize;

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
            dynamic_offset = Some(p_offset as usize);
            dynamic_size = p_filesz as usize;
            println!(
                "✅ Found PT_DYNAMIC at offset 0x{:x}, size 0x{:x}",
                p_offset, p_filesz
            );
            break;
        }
    }

    let dynamic_offset = dynamic_offset.expect("No PT_DYNAMIC found");

    // Parse dynamic section
    let mut dt_symtab = None;
    let mut offset = dynamic_offset;

    println!("\n📊 Parsing dynamic entries:");
    while offset < dynamic_offset + dynamic_size {
        let d_tag = u64::from_le_bytes(elf_bytes[offset..offset + 8].try_into().unwrap());
        let d_val = u64::from_le_bytes(elf_bytes[offset + 8..offset + 16].try_into().unwrap());

        match d_tag {
            0 => {
                println!("  DT_NULL");
                break;
            }
            6 => {
                dt_symtab = Some(d_val);
                println!("  DT_SYMTAB = 0x{:x}", d_val);
            }
            _ => {}
        }

        offset += 16;
    }

    let dt_symtab = dt_symtab.expect("No DT_SYMTAB found");

    // Now the critical part: RBPF searches for a section header with sh_addr == dt_symtab
    println!(
        "\n🔍 Critical check: Finding section with sh_addr = 0x{:x}",
        dt_symtab
    );

    let mut found = false;
    for i in 0..e_shnum {
        let shdr_offset = e_shoff as usize + (i as usize * 64);

        if shdr_offset + 24 > elf_bytes.len() {
            println!("  ❌ Section {} header out of bounds!", i);
            continue;
        }

        let sh_type = u32::from_le_bytes(
            elf_bytes[shdr_offset + 4..shdr_offset + 8]
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

        println!(
            "  Section [{}]: sh_type=0x{:x}, sh_addr=0x{:x}, sh_offset=0x{:x}, sh_size=0x{:x}",
            i, sh_type, sh_addr, sh_offset, sh_size
        );

        if sh_addr == dt_symtab {
            println!("    ✅ MATCH! This is what RBPF needs");

            // RBPF then uses sh_offset and sh_size to slice from the file
            if sh_offset == 0 || sh_size == 0 {
                println!("    ❌ BUT sh_offset or sh_size is 0!");
            } else if sh_offset as usize + sh_size as usize > elf_bytes.len() {
                println!("    ❌ BUT sh_offset + sh_size exceeds file size!");
            } else {
                println!("    ✅ sh_offset and sh_size are valid");

                // RBPF would call slice_from_program_header here
                // It needs to find a PT_LOAD that contains [sh_offset..sh_offset+sh_size]
                println!("\n🔍 Checking if sh_offset range is in a PT_LOAD:");
                for j in 0..e_phnum {
                    let phdr_offset = e_phoff as usize + (j as usize * 56);
                    let p_type = u32::from_le_bytes(
                        elf_bytes[phdr_offset..phdr_offset + 4].try_into().unwrap(),
                    );

                    if p_type == 1 {
                        // PT_LOAD
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
                        let p_end = p_offset + p_filesz;

                        println!(
                            "      PT_LOAD[{}]: file range 0x{:x}-0x{:x}",
                            j, p_offset, p_end
                        );

                        if sh_offset >= p_offset && sh_offset + sh_size <= p_end {
                            println!("        ✅ Contains section data!");
                        }
                    }
                }
            }

            found = true;
            break;
        }
    }

    if !found {
        println!(
            "\n❌ CRITICAL FAILURE: No section header with sh_addr = 0x{:x}!",
            dt_symtab
        );
        println!("This is why RBPF fails with 'invalid dynamic section table'");
    }
}
