// Compare how RBPF parses our ELF vs Solana's working ELF
use solana_rbpf::{elf::Executable, program::BuiltinProgram, vm::TestContextObject};
use std::sync::Arc;

fn parse_and_report(name: &str, path: &str) {
    println!("\n{}", "=".repeat(60));
    println!("Parsing {}: {}", name, path);
    println!("{}\n", "=".repeat(60));

    let elf_bytes = std::fs::read(path).expect("Failed to read ELF");
    println!("📦 File size: {} bytes", elf_bytes.len());

    // Check header
    println!("\n📋 ELF Header:");
    let e_machine = u16::from_le_bytes(elf_bytes[18..20].try_into().unwrap());
    let e_flags = u32::from_le_bytes(elf_bytes[48..52].try_into().unwrap());
    let e_shnum = u16::from_le_bytes(elf_bytes[60..62].try_into().unwrap());
    let e_shstrndx = u16::from_le_bytes(elf_bytes[62..64].try_into().unwrap());

    println!("  Machine: 0x{:x}", e_machine);
    println!("  Flags: 0x{:x}", e_flags);
    println!("  Section count: {}", e_shnum);
    println!("  shstrtab index: {}", e_shstrndx);

    // Try to parse with RBPF
    println!("\n🔍 RBPF Parsing:");
    let loader = Arc::new(BuiltinProgram::new_mock());

    match Executable::<TestContextObject>::load(&elf_bytes, loader.clone()) {
        Ok(executable) => {
            println!("  ✅ Parsed successfully!");

            // Get some info about the executable
            println!("\n📊 Executable Info:");
            println!("  SBPF Version: {:?}", executable.get_sbpf_version());
            let (_text_offset, text_bytes) = executable.get_text_bytes();
            println!("  Text section: 0x{:x} bytes", text_bytes.len());
            println!("  RO section: {} bytes", executable.get_ro_section().len());

            // Check function registry
            let function_registry = executable.get_function_registry();
            println!("\n📞 Function registry:");
            for (hash, value) in function_registry.iter().take(5) {
                println!("    Hash: 0x{:08x}, Value: (pc bytes, flags)", hash);
            }
        }
        Err(e) => {
            println!("  ❌ Failed to parse!");
            println!("  Error: {:?}", e);

            // Try to get more details about the error
            let error_str = format!("{:?}", e);

            if error_str.contains("StringTooLong") {
                println!("\n🔍 String length issue detected!");

                // Check shstrtab
                let e_shoff = u64::from_le_bytes(elf_bytes[40..48].try_into().unwrap());
                let shstrtab_offset = e_shoff + (e_shstrndx as u64 * 64);

                if shstrtab_offset + 64 <= elf_bytes.len() as u64 {
                    let sh_name = u32::from_le_bytes(
                        elf_bytes[shstrtab_offset as usize..shstrtab_offset as usize + 4]
                            .try_into()
                            .unwrap(),
                    );
                    let sh_offset = u64::from_le_bytes(
                        elf_bytes[shstrtab_offset as usize + 24..shstrtab_offset as usize + 32]
                            .try_into()
                            .unwrap(),
                    );
                    let sh_size = u64::from_le_bytes(
                        elf_bytes[shstrtab_offset as usize + 32..shstrtab_offset as usize + 40]
                            .try_into()
                            .unwrap(),
                    );

                    println!("  shstrtab section header:");
                    println!("    sh_name: 0x{:x}", sh_name);
                    println!("    sh_offset: 0x{:x}", sh_offset);
                    println!("    sh_size: 0x{:x}", sh_size);

                    // Check if we can read the name
                    let name_start = sh_offset as usize + sh_name as usize;
                    if name_start < elf_bytes.len() {
                        let available = elf_bytes.len() - name_start;
                        println!("    Bytes available from name start: {}", available);

                        // Try to find the null terminator
                        let mut null_found = false;
                        for i in 0..available.min(20) {
                            if elf_bytes[name_start + i] == 0 {
                                println!("    Null terminator found at offset {}", i);
                                null_found = true;
                                break;
                            }
                        }
                        if !null_found {
                            println!("    ⚠️  No null terminator found in first 20 bytes!");
                        }
                    }
                }
            }
        }
    }
}

fn main() {
    // Parse Solana's working ELF
    parse_and_report(
        "Solana's syscall_reloc_64_32.so",
        "/home/larp/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/solana_rbpf-0.8.5/tests/elfs/syscall_reloc_64_32.so"
    );

    // Parse our original ELF that fails
    parse_and_report("Our minimal_sbpf.so (V2 attempt)", "/tmp/minimal_sbpf.so");

    // Parse our new working V1 ELF
    parse_and_report("Our solana_v1.so (WORKING!)", "/tmp/solana_v1.so");
}
