// Figure out RBPF's instruction counting
use std::fs;

fn main() {
    let elf_bytes = fs::read("/tmp/minimal_syscall.so").expect("Failed to read ELF");

    println!("🔍 Debugging RBPF instruction counting\n");

    // The .text section starts at 0x120 according to our ELF
    let text_start = 0x120;
    let text_size = 48;

    println!("📝 Instructions by RBPF count:");

    let mut instr_count = 0;
    let mut byte_offset = 0;

    while byte_offset < text_size {
        let abs_offset = text_start + byte_offset;

        if abs_offset >= elf_bytes.len() {
            break;
        }

        let opcode = elf_bytes[abs_offset];

        print!(
            "  Instr[{}]: byte_offset=0x{:02x}, abs=0x{:04x}, opcode=0x{:02x} ",
            instr_count, byte_offset, abs_offset, opcode
        );

        // Check if this is the problematic instruction 5
        if instr_count == 5 {
            println!("← THIS IS INSTRUCTION 5!");

            // This would be at byte offset 0x28 (40) if single instructions
            // Or at 0x18 (24) if LDDW counts as 1
            let bytes = &elf_bytes[abs_offset..abs_offset.min(elf_bytes.len()).min(abs_offset + 8)];
            println!("           Bytes: {:02x?}", bytes);

            // If this is interpreted as a jump
            if opcode == 0x05 || (opcode & 0x07) == 0x05 {
                let imm = if abs_offset + 4 < elf_bytes.len() {
                    i16::from_le_bytes([elf_bytes[abs_offset + 2], elf_bytes[abs_offset + 3]])
                } else {
                    0
                };
                println!("           → Jump offset would be: {}", imm);
                println!(
                    "           → Target would be instruction: {}",
                    instr_count as i32 + imm as i32 + 1
                );
            }
        } else {
            println!();
        }

        // Move to next instruction
        if opcode == 0x18 {
            // LDDW is 16 bytes but might count as 2 instructions in RBPF
            byte_offset += 16;
            instr_count += 2; // RBPF counts LDDW as 2!
        } else {
            byte_offset += 8;
            instr_count += 1;
        }
    }

    println!("\n💡 Key insight: RBPF counts LDDW (0x18) as 2 instructions!");
    println!("   So our instruction sequence is:");
    println!("   [0-1]: LDDW r0, 0");
    println!("   [2-3]: LDDW r1, 0x150");
    println!("   [4]:   MOV r2, 4");
    println!("   [5]:   CALL 0");
    println!("   [6]:   EXIT");

    // Check if instruction 5 has any issues
    let instr5_offset = text_start + 0x28; // 5 instructions * 8 bytes (but LDDW is 16)
                                           // Actually: 2*16 (two LDDW) + 8 (MOV) = 40 = 0x28

    if instr5_offset < elf_bytes.len() {
        println!("\n🎯 Instruction 5 is at offset 0x{:x}", instr5_offset);
        let opcode = elf_bytes[instr5_offset];
        println!("   Opcode: 0x{:02x}", opcode);

        if opcode == 0x85 {
            println!("   This is CALL - should be fine unless RBPF has different validation");
        }
    }
}
