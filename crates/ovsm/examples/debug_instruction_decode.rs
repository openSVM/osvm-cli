// Debug RBPF instruction decoding to find the exact issue
use std::fs;

fn main() {
    let elf_bytes = fs::read("/tmp/minimal_syscall.so").expect("Failed to read ELF");

    println!("🔍 Debugging RBPF instruction decode at .text (0x120)\n");

    // The .text section starts at 0x120 according to our ELF
    let text_start = 0x120;
    let text_size = 48;

    println!("📝 Instructions in .text:");

    let mut pc = 0;
    while pc < text_size {
        let offset = text_start + pc;

        if offset >= elf_bytes.len() {
            println!("  ❌ Offset 0x{:x} out of bounds!", offset);
            break;
        }

        let opcode = elf_bytes[offset];
        let regs = if offset + 1 < elf_bytes.len() {
            elf_bytes[offset + 1]
        } else {
            0
        };
        let dst = regs & 0xf;
        let src = (regs >> 4) & 0xf;

        // Read immediate values based on instruction class
        let imm = if offset + 4 < elf_bytes.len() {
            i32::from_le_bytes([
                elf_bytes[offset + 4],
                elf_bytes[offset + 5],
                elf_bytes[offset + 6],
                elf_bytes[offset + 7],
            ])
        } else {
            0
        };

        // Decode instruction
        print!("  [{:2}] 0x{:04x}: 0x{:02x} ", pc / 8, offset, opcode);

        match opcode {
            0x18 => {
                // LDDW - 16 byte instruction
                let imm64 = if offset + 12 < elf_bytes.len() {
                    let low = imm as u64;
                    let high = i32::from_le_bytes([
                        elf_bytes[offset + 12],
                        elf_bytes[offset + 13],
                        elf_bytes[offset + 14],
                        elf_bytes[offset + 15],
                    ]) as u64;
                    (high << 32) | (low as u32 as u64)
                } else {
                    imm as u64
                };
                println!("LDDW r{}, 0x{:x}", dst, imm64);
                pc += 16; // LDDW is 16 bytes
            }
            0x85 => {
                println!("CALL {}", imm);
                // Check if this is a relative jump
                if imm != 0 {
                    let target = (pc as i32 + imm * 8) as usize;
                    println!(
                        "       → Jump target would be: 0x{:x} (relative offset {})",
                        target, imm
                    );
                    if target >= text_size {
                        println!(
                            "       ❌ JUMP OUT OF BOUNDS! Text size is only 0x{:x}",
                            text_size
                        );
                    }
                }
                pc += 8;
            }
            0x95 => {
                println!("EXIT");
                pc += 8;
            }
            0xb7 => {
                println!("MOV r{}, {}", dst, imm);
                pc += 8;
            }
            0x05 => {
                let offset = imm as i16;
                println!("JA {}", offset);
                let target = (pc as i32 + (offset as i32) * 8) as usize;
                println!("       → Jump target: 0x{:x}", target);
                if target >= text_size {
                    println!("       ❌ JUMP OUT OF BOUNDS!");
                }
                pc += 8;
            }
            _ => {
                println!("UNKNOWN (0x{:02x})", opcode);
                pc += 8;
            }
        }
    }

    // Now analyze the actual bytes at position 5
    println!("\n🔍 Instruction at position 5 (byte offset 40 = 0x28 in .text):");
    let instr_offset = text_start + 40;
    if instr_offset + 8 <= elf_bytes.len() {
        let bytes = &elf_bytes[instr_offset..instr_offset + 8];
        println!("  Bytes: {:02x?}", bytes);
        println!("  Opcode: 0x{:02x}", bytes[0]);

        // Check if it's interpreted as a jump
        if bytes[0] & 0x07 == 0x05 {
            let imm = i32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);
            println!(
                "  ❌ This is being interpreted as a JUMP with offset {}",
                imm
            );
        }
    }

    // Also check what RBPF might be seeing differently
    println!(
        "\n🎯 Theory: RBPF might be starting from a different offset or seeing corrupted data"
    );
}
