// Detailed ELF parser debugging
use std::mem;

fn main() {
    let elf_path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "/tmp/hello_final.so".to_string());

    println!("📂 Loading: {}", elf_path);
    let elf_bytes = std::fs::read(&elf_path).expect("Failed to read ELF");
    println!("   Size: {} bytes\n", elf_bytes.len());

    // Parse ELF header
    if elf_bytes.len() < 64 {
        eprintln!("❌ File too small for ELF header");
        return;
    }

    // Read e_phoff and e_phnum
    let e_phoff = u64::from_le_bytes(elf_bytes[32..40].try_into().unwrap());
    let e_phnum = u16::from_le_bytes(elf_bytes[56..58].try_into().unwrap());

    println!(
        "📊 Program Headers: {} entries at offset 0x{:x}",
        e_phnum, e_phoff
    );

    // Find PT_DYNAMIC
    let mut pt_dynamic_offset = 0usize;
    let mut pt_dynamic_size = 0usize;

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
            pt_dynamic_offset = p_offset as usize;
            pt_dynamic_size = p_filesz as usize;
            println!(
                "✅ Found PT_DYNAMIC: offset=0x{:x}, size=0x{:x}",
                p_offset, p_filesz
            );
            break;
        }
    }

    if pt_dynamic_offset == 0 {
        println!("⚠️  No PT_DYNAMIC found");
        return;
    }

    // Parse dynamic entries
    println!("\n📊 Dynamic Entries:");
    let mut dt_symtab = 0u64;
    let mut dt_strtab = 0u64;
    let mut dt_rel = 0u64;
    let mut dt_relsz = 0u64;
    let mut dt_relent = 0u64;

    let mut offset = pt_dynamic_offset;
    while offset < pt_dynamic_offset + pt_dynamic_size {
        let d_tag = u64::from_le_bytes(elf_bytes[offset..offset + 8].try_into().unwrap());
        let d_val = u64::from_le_bytes(elf_bytes[offset + 8..offset + 16].try_into().unwrap());

        match d_tag {
            0 => {
                println!("   DT_NULL");
                break;
            }
            5 => {
                dt_strtab = d_val;
                println!("   DT_STRTAB: 0x{:x}", d_val);
            }
            6 => {
                dt_symtab = d_val;
                println!("   DT_SYMTAB: 0x{:x}", d_val);
            }
            17 => {
                dt_rel = d_val;
                println!("   DT_REL: 0x{:x}", d_val);
            }
            18 => {
                dt_relsz = d_val;
                println!("   DT_RELSZ: {} bytes", d_val);
            }
            19 => {
                dt_relent = d_val;
                println!("   DT_RELENT: {} bytes", d_val);
            }
            30 => {
                println!("   DT_FLAGS: 0x{:x}", d_val);
            }
            _ => {}
        }

        offset += 16;
    }

    // Validation checks that RBPF does
    println!("\n🔍 RBPF Validation Checks:");

    // Check 1: DT_RELENT must be 16
    if dt_relent != 0 && dt_relent != 16 {
        println!("❌ DT_RELENT is {} but must be 16", dt_relent);
    } else if dt_relent == 16 {
        println!("✅ DT_RELENT = 16");
    }

    // Check 2: DT_RELSZ must be non-zero if DT_REL is set
    if dt_rel != 0 && dt_relsz == 0 {
        println!("❌ DT_REL is set but DT_RELSZ is 0");
    } else if dt_rel != 0 {
        println!("✅ DT_RELSZ = {} bytes", dt_relsz);
    }

    // Check 3: Find section header for DT_SYMTAB
    let e_shoff = u64::from_le_bytes(elf_bytes[40..48].try_into().unwrap());
    let e_shnum = u16::from_le_bytes(elf_bytes[60..62].try_into().unwrap());

    println!(
        "\n📊 Checking section headers for DT_SYMTAB=0x{:x}",
        dt_symtab
    );

    let mut found_symtab_section = false;
    for i in 0..e_shnum {
        let shdr_offset = e_shoff as usize + (i as usize * 64);
        let sh_addr = u64::from_le_bytes(
            elf_bytes[shdr_offset + 16..shdr_offset + 24]
                .try_into()
                .unwrap(),
        );
        let sh_type = u32::from_le_bytes(
            elf_bytes[shdr_offset + 4..shdr_offset + 8]
                .try_into()
                .unwrap(),
        );

        if sh_addr == dt_symtab {
            println!(
                "✅ Found section [{}] with sh_addr=0x{:x}, sh_type=0x{:x}",
                i, sh_addr, sh_type
            );
            found_symtab_section = true;

            // Check if it's DYNSYM type (11)
            if sh_type != 11 {
                println!("   ⚠️  Type is 0x{:x}, expected SHT_DYNSYM (0xb)", sh_type);
            }
        }
    }

    if !found_symtab_section {
        println!("❌ No section header found with sh_addr matching DT_SYMTAB!");
        println!("   This is likely why RBPF fails with 'invalid dynamic section table'");
    }

    // Check PT_LOAD coverage
    println!("\n📊 Checking PT_LOAD coverage for dynamic addresses:");

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
            let p_end = p_vaddr + p_memsz;

            println!("   PT_LOAD[{}]: 0x{:x}-0x{:x}", i, p_vaddr, p_end);

            if dt_symtab >= p_vaddr && dt_symtab < p_end {
                println!("      ✅ Contains DT_SYMTAB (0x{:x})", dt_symtab);
            }
            if dt_strtab >= p_vaddr && dt_strtab < p_end {
                println!("      ✅ Contains DT_STRTAB (0x{:x})", dt_strtab);
            }
            if dt_rel >= p_vaddr && dt_rel < p_end {
                println!("      ✅ Contains DT_REL (0x{:x})", dt_rel);
            }
        }
    }
}
