// Direct low-level ELF parser test to pinpoint failure
use solana_rbpf::elf_parser::Elf64;

fn main() {
    let elf_bytes = std::fs::read("/tmp/hello_final.so").expect("Failed to read ELF");

    println!("📂 ELF size: {} bytes", elf_bytes.len());
    println!("🔍 Parsing with Elf64::parse()...\n");

    match Elf64::parse(&elf_bytes) {
        Ok(parser) => {
            println!("✅ ELF parsed successfully!\n");

            println!("📊 File Header:");
            println!("   Entry: 0x{:x}", parser.file_header().e_entry);
            println!("   Type: 0x{:x}", parser.file_header().e_type);

            println!("\n📊 Program Headers:");
            for (i, phdr) in parser.program_header_table().iter().enumerate() {
                println!(
                    "   [{}] Type: 0x{:x}, VAddr: 0x{:x}, Size: 0x{:x}",
                    i, phdr.p_type, phdr.p_vaddr, phdr.p_memsz
                );
            }

            println!("\n📊 Section Headers:");
            for (i, shdr) in parser.section_header_table().iter().enumerate() {
                println!(
                    "   [{}] Addr: 0x{:x}, Offset: 0x{:x}, Size: 0x{:x}, Type: 0x{:x}",
                    i, shdr.sh_addr, shdr.sh_offset, shdr.sh_size, shdr.sh_type
                );
            }

            if let Some(dynsym) = parser.dynamic_symbol_table() {
                println!("\n✅ Dynamic Symbol Table: {} entries", dynsym.len());
                for (i, sym) in dynsym.iter().enumerate() {
                    println!(
                        "   [{}] st_name: 0x{:x}, st_value: 0x{:x}, st_info: 0x{:x}",
                        i, sym.st_name, sym.st_value, sym.st_info
                    );
                }
            } else {
                println!("\n⚠️  No dynamic symbol table");
            }

            if let Some(relocs) = parser.dynamic_relocations_table() {
                println!("\n✅ Dynamic Relocations: {} entries", relocs.len());
                for (i, rel) in relocs.iter().enumerate() {
                    println!(
                        "   [{}] r_offset: 0x{:x}, r_info: 0x{:x} (type={}, sym={})",
                        i,
                        rel.r_offset,
                        rel.r_info,
                        rel.r_type(),
                        rel.r_sym()
                    );
                }
            } else {
                println!("\n⚠️  No dynamic relocations");
            }
        }
        Err(e) => {
            println!("❌ Parse failed: {:?}", e);
        }
    }
}
