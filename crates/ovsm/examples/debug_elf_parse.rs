// Debug ELF parsing to find exact failure point
use solana_rbpf::elf_parser::{consts::*, types::*, Elf64Parser};

fn main() {
    let elf_path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "/tmp/hello_final.so".to_string());

    println!("📂 Loading ELF: {}", elf_path);
    let elf_bytes = std::fs::read(&elf_path).expect("Failed to read ELF");
    println!("   Size: {} bytes\n", elf_bytes.len());

    println!("🔍 Parsing ELF...");

    match Elf64Parser::parse(&elf_bytes) {
        Ok(parser) => {
            println!("✅ ELF parsed successfully!\n");

            // Check dynamic table
            println!("📊 Dynamic Table:");
            if let Some(dynsym) = parser.dynamic_symbol_table() {
                println!("   Dynamic symbols: {} entries", dynsym.len());
            } else {
                println!("   No dynamic symbol table");
            }

            if let Some(relocs) = parser.dynamic_relocations_table() {
                println!("   Dynamic relocations: {} entries", relocs.len());
            } else {
                println!("   No dynamic relocations");
            }
        }
        Err(e) => {
            println!("❌ ELF parsing failed!");
            println!("\n🔍 Error: {:?}", e);
        }
    }
}
