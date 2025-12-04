//! Debug ELF parsing example
//!
//! Note: This example was designed for an older version of solana_rbpf that
//! included Elf64Parser. The parser API has since changed, and this example
//! is kept as a stub for historical reference.
//!
//! For ELF debugging, see instead:
//! - debug_elf_parser.rs - Uses goblin crate for ELF parsing
//! - debug_elf_symbols.rs - Symbol table inspection
//! - validate_elf.rs - ELF validation

fn main() {
    println!("⚠️  This example is deprecated.");
    println!();
    println!("The Elf64Parser from solana_rbpf has been removed in newer versions.");
    println!("For ELF debugging, use these alternatives:");
    println!();
    println!("  cargo run --example debug_elf_parser -- <file.so>");
    println!("  cargo run --example debug_elf_symbols -- <file.so>");
    println!("  cargo run --example validate_elf -- <file.so>");
}
