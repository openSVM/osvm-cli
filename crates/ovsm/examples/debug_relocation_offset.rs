// Debug the exact relocation offset calculation that RBPF does
use std::fs;

fn main() {
    let elf_bytes = fs::read("/tmp/hello_final.so").expect("Failed to read ELF");

    println!("🔍 Debugging RBPF relocation offset calculation\n");

    // Get DT_REL from dynamic table
    let e_phoff = u64::from_le_bytes(elf_bytes[32..40].try_into().unwrap());
    let e_phnum = u16::from_le_bytes(elf_bytes[56..58].try_into().unwrap());

    let mut dt_rel = 0u64;
    let mut dt_relsz = 0u64;

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
                    17 => dt_rel = d_val,   // DT_REL
                    18 => dt_relsz = d_val, // DT_RELSZ
                    0 => break,
                    _ => {}
                }
            }
            break;
        }
    }

    println!("📊 Dynamic relocation info:");
    println!("  DT_REL   = 0x{:x} (vaddr)", dt_rel);
    println!("  DT_RELSZ = 0x{:x} ({} bytes)", dt_relsz, dt_relsz);

    // Find PT_LOAD containing this vaddr
    println!("\n🔍 Looking for PT_LOAD containing vaddr 0x{:x}:", dt_rel);

    let mut found_phdr = false;
    let mut calc_offset = 0usize;

    for i in 0..e_phnum {
        let phdr_offset = e_phoff as usize + (i as usize * 56);
        let p_type =
            u32::from_le_bytes(elf_bytes[phdr_offset..phdr_offset + 4].try_into().unwrap());

        if p_type == 1 {
            // PT_LOAD
            let p_vaddr = u64::from_le_bytes(
                elf_bytes[phdr_offset + 16..phdr_offset + 24]
                    .try_into()
                    .unwrap(),
            );
            let p_memsz = u64::from_le_bytes(
                elf_bytes[phdr_offset + 40..phdr_offset + 48]
                    .try_into()
                    .unwrap(),
            );
            let p_offset = u64::from_le_bytes(
                elf_bytes[phdr_offset + 8..phdr_offset + 16]
                    .try_into()
                    .unwrap(),
            );

            let p_end = p_vaddr + p_memsz;

            println!(
                "  PT_LOAD[{}]: vaddr 0x{:x}-0x{:x}, file offset 0x{:x}",
                i, p_vaddr, p_end, p_offset
            );

            if dt_rel >= p_vaddr && dt_rel < p_end {
                println!("    ✅ CONTAINS DT_REL!");

                // Calculate file offset as RBPF does
                calc_offset = ((dt_rel - p_vaddr) + p_offset) as usize;
                println!("    Calculated file offset: 0x{:x}", calc_offset);
                println!(
                    "    Formula: (0x{:x} - 0x{:x}) + 0x{:x} = 0x{:x}",
                    dt_rel, p_vaddr, p_offset, calc_offset
                );

                found_phdr = true;
                break;
            }
        }
    }

    if !found_phdr {
        println!("  ❌ No PT_LOAD found containing vaddr!");
        println!("  RBPF would fall back to section header sh_offset");

        // Find section header
        let e_shoff = u64::from_le_bytes(elf_bytes[40..48].try_into().unwrap());
        let e_shnum = u16::from_le_bytes(elf_bytes[60..62].try_into().unwrap());

        for i in 0..e_shnum {
            let shdr_offset = e_shoff as usize + (i as usize * 64);
            let sh_addr = u64::from_le_bytes(
                elf_bytes[shdr_offset + 16..shdr_offset + 24]
                    .try_into()
                    .unwrap(),
            );

            if sh_addr == dt_rel {
                let sh_offset = u64::from_le_bytes(
                    elf_bytes[shdr_offset + 24..shdr_offset + 32]
                        .try_into()
                        .unwrap(),
                );
                calc_offset = sh_offset as usize;
                println!(
                    "  Found section with sh_addr=0x{:x}, sh_offset=0x{:x}",
                    sh_addr, sh_offset
                );
                break;
            }
        }
    }

    // Now check if slice_from_bytes would succeed
    println!("\n🔍 slice_from_bytes check:");
    println!(
        "  Range: 0x{:x}..0x{:x}",
        calc_offset,
        calc_offset + dt_relsz as usize
    );
    println!("  File size: 0x{:x}", elf_bytes.len());

    if calc_offset + dt_relsz as usize > elf_bytes.len() {
        println!("  ❌ FAIL: Range exceeds file size!");
        println!("  This would cause InvalidDynamicSectionTable at line 407!");
    } else {
        println!("  ✅ Range is valid");

        // Check the actual content
        println!("\n📊 Relocation entry at offset 0x{:x}:", calc_offset);
        if calc_offset + 16 <= elf_bytes.len() {
            let r_offset =
                u64::from_le_bytes(elf_bytes[calc_offset..calc_offset + 8].try_into().unwrap());
            let r_info = u64::from_le_bytes(
                elf_bytes[calc_offset + 8..calc_offset + 16]
                    .try_into()
                    .unwrap(),
            );
            println!("  r_offset: 0x{:x}", r_offset);
            println!(
                "  r_info:   0x{:x} (sym={}, type={})",
                r_info,
                r_info >> 32,
                r_info & 0xffffffff
            );
        }
    }
}
