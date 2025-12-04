// Byte-by-byte ELF comparison tool to find exact differences
use std::fs;

fn main() {
    // Load both ELF files
    let our_elf = fs::read("/tmp/hello_final.so").expect("Failed to read our ELF");
    let solana_elf = fs::read("/home/larp/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/solana_rbpf-0.8.5/tests/elfs/syscall_reloc_64_32.so")
        .expect("Failed to read Solana ELF");

    println!("🔍 ELF Comparison");
    println!("Our ELF: {} bytes", our_elf.len());
    println!("Solana ELF: {} bytes\n", solana_elf.len());

    // Compare headers
    println!("📊 ELF Header Comparison:");
    compare_bytes(&our_elf[0..64], &solana_elf[0..64], "ELF Header");

    // Get program header info
    let our_phoff = u64::from_le_bytes(our_elf[32..40].try_into().unwrap());
    let our_phnum = u16::from_le_bytes(our_elf[56..58].try_into().unwrap());
    let sol_phoff = u64::from_le_bytes(solana_elf[32..40].try_into().unwrap());
    let sol_phnum = u16::from_le_bytes(solana_elf[56..58].try_into().unwrap());

    println!("\n📊 Program Headers:");
    println!("Our: {} headers at 0x{:x}", our_phnum, our_phoff);
    println!("Solana: {} headers at 0x{:x}", sol_phnum, sol_phoff);

    // Find and compare PT_DYNAMIC
    let mut our_dynamic_off = 0usize;
    let mut our_dynamic_size = 0usize;
    let mut sol_dynamic_off = 0usize;
    let mut sol_dynamic_size = 0usize;

    for i in 0..our_phnum.min(sol_phnum) {
        let our_ph_off = our_phoff as usize + (i as usize * 56);
        let sol_ph_off = sol_phoff as usize + (i as usize * 56);

        let our_type = u32::from_le_bytes(our_elf[our_ph_off..our_ph_off + 4].try_into().unwrap());
        let sol_type =
            u32::from_le_bytes(solana_elf[sol_ph_off..sol_ph_off + 4].try_into().unwrap());

        if our_type == 2 {
            // PT_DYNAMIC
            our_dynamic_off =
                u64::from_le_bytes(our_elf[our_ph_off + 8..our_ph_off + 16].try_into().unwrap())
                    as usize;
            our_dynamic_size = u64::from_le_bytes(
                our_elf[our_ph_off + 32..our_ph_off + 40]
                    .try_into()
                    .unwrap(),
            ) as usize;
        }

        if sol_type == 2 {
            // PT_DYNAMIC
            sol_dynamic_off = u64::from_le_bytes(
                solana_elf[sol_ph_off + 8..sol_ph_off + 16]
                    .try_into()
                    .unwrap(),
            ) as usize;
            sol_dynamic_size = u64::from_le_bytes(
                solana_elf[sol_ph_off + 32..sol_ph_off + 40]
                    .try_into()
                    .unwrap(),
            ) as usize;
        }
    }

    println!("\n📊 PT_DYNAMIC Segments:");
    println!(
        "Our: offset=0x{:x}, size=0x{:x}",
        our_dynamic_off, our_dynamic_size
    );
    println!(
        "Solana: offset=0x{:x}, size=0x{:x}",
        sol_dynamic_off, sol_dynamic_size
    );

    // Compare dynamic sections byte by byte
    if our_dynamic_off > 0 && sol_dynamic_off > 0 {
        println!("\n🔍 Dynamic Section Comparison:");

        let our_end = our_dynamic_off + our_dynamic_size.min(256); // Compare first 256 bytes
        let sol_end = sol_dynamic_off + sol_dynamic_size.min(256);

        // Parse and compare dynamic entries
        let mut our_offset = our_dynamic_off;
        let mut sol_offset = sol_dynamic_off;
        let mut entry_num = 0;

        println!("\n📊 Dynamic Entries:");
        while our_offset < our_end && sol_offset < sol_end {
            let our_tag = u64::from_le_bytes(
                our_elf[our_offset..our_offset + 8]
                    .try_into()
                    .unwrap_or([0u8; 8]),
            );
            let our_val = u64::from_le_bytes(
                our_elf[our_offset + 8..our_offset + 16]
                    .try_into()
                    .unwrap_or([0u8; 8]),
            );

            let sol_tag = u64::from_le_bytes(
                solana_elf[sol_offset..sol_offset + 8]
                    .try_into()
                    .unwrap_or([0u8; 8]),
            );
            let sol_val = u64::from_le_bytes(
                solana_elf[sol_offset + 8..sol_offset + 16]
                    .try_into()
                    .unwrap_or([0u8; 8]),
            );

            let tag_name = match our_tag {
                0 => "DT_NULL",
                5 => "DT_STRTAB",
                6 => "DT_SYMTAB",
                17 => "DT_REL",
                18 => "DT_RELSZ",
                19 => "DT_RELENT",
                30 => "DT_FLAGS",
                _ => "Unknown",
            };

            if our_tag != sol_tag || our_val != sol_val {
                println!("  ❌ Entry {}: {} differs!", entry_num, tag_name);
                println!("     Our:    tag=0x{:x} val=0x{:x}", our_tag, our_val);
                println!("     Solana: tag=0x{:x} val=0x{:x}", sol_tag, sol_val);
            } else {
                println!(
                    "  ✅ Entry {}: {} matches (tag=0x{:x}, val=0x{:x})",
                    entry_num, tag_name, our_tag, our_val
                );
            }

            if our_tag == 0 || sol_tag == 0 {
                break;
            }
            our_offset += 16;
            sol_offset += 16;
            entry_num += 1;
        }
    }

    // Compare section headers
    let our_shoff = u64::from_le_bytes(our_elf[40..48].try_into().unwrap());
    let our_shnum = u16::from_le_bytes(our_elf[60..62].try_into().unwrap());
    let sol_shoff = u64::from_le_bytes(solana_elf[40..48].try_into().unwrap());
    let sol_shnum = u16::from_le_bytes(solana_elf[60..62].try_into().unwrap());

    println!("\n📊 Section Headers:");
    println!("Our: {} sections at 0x{:x}", our_shnum, our_shoff);
    println!("Solana: {} sections at 0x{:x}", sol_shnum, sol_shoff);

    // Find .dynsym sections and compare sh_addr
    println!("\n🔍 Looking for .dynsym sections (type=11):");

    for i in 0..our_shnum.min(sol_shnum) {
        let our_sh_off = our_shoff as usize + (i as usize * 64);
        let sol_sh_off = sol_shoff as usize + (i as usize * 64);

        let our_type =
            u32::from_le_bytes(our_elf[our_sh_off + 4..our_sh_off + 8].try_into().unwrap());
        let sol_type = u32::from_le_bytes(
            solana_elf[sol_sh_off + 4..sol_sh_off + 8]
                .try_into()
                .unwrap(),
        );

        if our_type == 11 || sol_type == 11 {
            // SHT_DYNSYM
            let our_addr = u64::from_le_bytes(
                our_elf[our_sh_off + 16..our_sh_off + 24]
                    .try_into()
                    .unwrap(),
            );
            let sol_addr = u64::from_le_bytes(
                solana_elf[sol_sh_off + 16..sol_sh_off + 24]
                    .try_into()
                    .unwrap(),
            );

            println!("  Section [{}]:", i);
            println!("    Our:    type=0x{:x} sh_addr=0x{:x}", our_type, our_addr);
            println!("    Solana: type=0x{:x} sh_addr=0x{:x}", sol_type, sol_addr);

            if our_type != sol_type || our_addr == 0 && sol_addr != 0 {
                println!("    ❌ Mismatch!");
            }
        }
    }
}

fn compare_bytes(our: &[u8], sol: &[u8], name: &str) {
    let mut diffs = Vec::new();
    let len = our.len().min(sol.len());

    for i in 0..len {
        if our[i] != sol[i] {
            diffs.push(i);
        }
    }

    if diffs.is_empty() {
        println!("  ✅ {} matches exactly", name);
    } else {
        println!(
            "  ❌ {} has {} differences at offsets: {:?}",
            name,
            diffs.len(),
            &diffs[..diffs.len().min(10)]
        );
    }
}
