// Debug the actual symbol table entries that RBPF tries to parse
use std::fs;

fn main() {
    let elf_bytes = fs::read("/tmp/hello_final.so").expect("Failed to read ELF");

    println!("🔍 Debugging Symbol Table Entries\n");

    // Find .dynsym section at 0x10e8
    let dynsym_offset = 0x10e8;
    let dynsym_size = 0x30; // 48 bytes = 2 entries of 24 bytes each

    println!(
        "📊 .dynsym section at offset 0x{:x}, size 0x{:x}",
        dynsym_offset, dynsym_size
    );
    println!("Should contain {} symbol entries\n", dynsym_size / 24);

    // Parse symbol table entries
    for i in 0..(dynsym_size / 24) {
        let sym_offset = dynsym_offset + (i * 24);

        let st_name = u32::from_le_bytes(elf_bytes[sym_offset..sym_offset + 4].try_into().unwrap());
        let st_info = elf_bytes[sym_offset + 4];
        let st_other = elf_bytes[sym_offset + 5];
        let st_shndx = u16::from_le_bytes(
            elf_bytes[sym_offset + 6..sym_offset + 8]
                .try_into()
                .unwrap(),
        );
        let st_value = u64::from_le_bytes(
            elf_bytes[sym_offset + 8..sym_offset + 16]
                .try_into()
                .unwrap(),
        );
        let st_size = u64::from_le_bytes(
            elf_bytes[sym_offset + 16..sym_offset + 24]
                .try_into()
                .unwrap(),
        );

        println!("Symbol [{}] at offset 0x{:x}:", i, sym_offset);
        println!("  st_name:  0x{:08x} (string table offset)", st_name);
        println!(
            "  st_info:  0x{:02x} (bind={}, type={})",
            st_info,
            st_info >> 4,
            st_info & 0xf
        );
        println!("  st_other: 0x{:02x}", st_other);
        println!("  st_shndx: 0x{:04x}", st_shndx);
        println!("  st_value: 0x{:016x}", st_value);
        println!("  st_size:  0x{:016x}", st_size);

        // Symbol type check
        let sym_type = st_info & 0xf;
        let type_str = match sym_type {
            0 => "STT_NOTYPE",
            1 => "STT_OBJECT",
            2 => "STT_FUNC",
            3 => "STT_SECTION",
            4 => "STT_FILE",
            _ => "Unknown",
        };
        println!("  Type: {} ({})", type_str, sym_type);

        if i == 0 && (st_name != 0 || st_info != 0 || st_value != 0) {
            println!("  ⚠️  First symbol should be NULL symbol (all zeros)!");
        }

        if st_name != 0 {
            // Try to read the name from .dynstr
            let dynstr_offset = 0x1118;
            let name_offset = dynstr_offset + st_name as usize;

            if name_offset < elf_bytes.len() {
                // Read null-terminated string
                let mut name_end = name_offset;
                while name_end < elf_bytes.len() && elf_bytes[name_end] != 0 {
                    name_end += 1;
                }

                if let Ok(name) = std::str::from_utf8(&elf_bytes[name_offset..name_end]) {
                    println!("  Name: \"{}\"", name);
                }
            }
        }

        println!();
    }

    // Also check .dynstr
    println!("📊 .dynstr section at offset 0x1118:");
    let dynstr_offset = 0x1118;
    let dynstr_end = 0x1122; // Until .rel.dyn

    println!("String table bytes:");
    for i in 0..(dynstr_end - dynstr_offset) {
        let byte = elf_bytes[dynstr_offset + i];
        if byte == 0 {
            print!("\\0");
        } else if byte.is_ascii_graphic() {
            print!("{}", byte as char);
        } else {
            print!("\\x{:02x}", byte);
        }
    }
    println!("\n");

    // Check relocation entries
    println!("📊 .rel.dyn section at offset 0x1122:");
    let reldyn_offset = 0x1122;
    let reldyn_size = 16; // One relocation entry

    let r_offset = u64::from_le_bytes(
        elf_bytes[reldyn_offset..reldyn_offset + 8]
            .try_into()
            .unwrap(),
    );
    let r_info = u64::from_le_bytes(
        elf_bytes[reldyn_offset + 8..reldyn_offset + 16]
            .try_into()
            .unwrap(),
    );

    let r_sym = (r_info >> 32) as u32;
    let r_type = (r_info & 0xffffffff) as u32;

    println!(
        "  r_offset: 0x{:016x} (where to apply relocation)",
        r_offset
    );
    println!("  r_sym:    {} (symbol index)", r_sym);
    println!("  r_type:   {} (R_BPF_64_32 = type 10)", r_type);

    if r_sym >= 2 {
        println!(
            "  ⚠️  Symbol index {} is out of bounds (only have 2 symbols)!",
            r_sym
        );
    }
}
