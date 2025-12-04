//! Debug utilities for OVSM→sBPF compilation
//!
//! Tools for inspecting IR, bytecode, and register allocation.

use super::ir::{IrInstruction, IrProgram, IrReg};
use super::sbpf_codegen::SbpfReg;
use std::collections::HashMap;

/// Print IR program in human-readable format
pub fn dump_ir(program: &IrProgram) {
    println!("═══════════════════════════════════════════════════════════");
    println!("                    IR DUMP");
    println!("═══════════════════════════════════════════════════════════");
    println!("Entry: {}", program.entry_label);
    println!("Variables: {:?}", program.var_registers);
    println!("Strings: {:?}", program.string_table);
    println!("───────────────────────────────────────────────────────────");

    for (i, instr) in program.instructions.iter().enumerate() {
        println!("{:04}: {}", i, format_ir_instr(instr));
    }
    println!("═══════════════════════════════════════════════════════════\n");
}

/// Format a single IR instruction
pub fn format_ir_instr(instr: &IrInstruction) -> String {
    match instr {
        IrInstruction::ConstI64(dst, val) => format!("r{} = {}", dst.0, val),
        IrInstruction::ConstF64(dst, bits) => format!("r{} = f64(0x{:016x})", dst.0, bits),
        IrInstruction::ConstBool(dst, val) => format!("r{} = {}", dst.0, val),
        IrInstruction::ConstNull(dst) => format!("r{} = null", dst.0),
        IrInstruction::ConstString(dst, idx) => format!("r{} = str[{}]", dst.0, idx),

        IrInstruction::Add(dst, a, b) => format!("r{} = r{} + r{}", dst.0, a.0, b.0),
        IrInstruction::Sub(dst, a, b) => format!("r{} = r{} - r{}", dst.0, a.0, b.0),
        IrInstruction::Mul(dst, a, b) => format!("r{} = r{} * r{}", dst.0, a.0, b.0),
        IrInstruction::Div(dst, a, b) => format!("r{} = r{} / r{}", dst.0, a.0, b.0),
        IrInstruction::Mod(dst, a, b) => format!("r{} = r{} % r{}", dst.0, a.0, b.0),
        IrInstruction::And(dst, a, b) => format!("r{} = r{} & r{}", dst.0, a.0, b.0),
        IrInstruction::Or(dst, a, b) => format!("r{} = r{} | r{}", dst.0, a.0, b.0),

        IrInstruction::Eq(dst, a, b) => format!("r{} = r{} == r{}", dst.0, a.0, b.0),
        IrInstruction::Ne(dst, a, b) => format!("r{} = r{} != r{}", dst.0, a.0, b.0),
        IrInstruction::Lt(dst, a, b) => format!("r{} = r{} < r{}", dst.0, a.0, b.0),
        IrInstruction::Le(dst, a, b) => format!("r{} = r{} <= r{}", dst.0, a.0, b.0),
        IrInstruction::Gt(dst, a, b) => format!("r{} = r{} > r{}", dst.0, a.0, b.0),
        IrInstruction::Ge(dst, a, b) => format!("r{} = r{} >= r{}", dst.0, a.0, b.0),

        IrInstruction::Neg(dst, src) => format!("r{} = -r{}", dst.0, src.0),
        IrInstruction::Not(dst, src) => format!("r{} = !r{}", dst.0, src.0),
        IrInstruction::Move(dst, src) => format!("r{} = r{}", dst.0, src.0),

        IrInstruction::Load(dst, base, off) => format!("r{} = [r{} + {}]", dst.0, base.0, off),
        IrInstruction::Load1(dst, base, off) => format!("r{} = (u8)[r{} + {}]", dst.0, base.0, off),
        IrInstruction::Load2(dst, base, off) => {
            format!("r{} = (u16)[r{} + {}]", dst.0, base.0, off)
        }
        IrInstruction::Load4(dst, base, off) => {
            format!("r{} = (u32)[r{} + {}]", dst.0, base.0, off)
        }
        IrInstruction::Store(base, src, off) => format!("[r{} + {}] = r{}", base.0, off, src.0),
        IrInstruction::Store1(base, src, off) => {
            format!("(u8)[r{} + {}] = r{}", base.0, off, src.0)
        }
        IrInstruction::Store2(base, src, off) => {
            format!("(u16)[r{} + {}] = r{}", base.0, off, src.0)
        }
        IrInstruction::Store4(base, src, off) => {
            format!("(u32)[r{} + {}] = r{}", base.0, off, src.0)
        }

        IrInstruction::Label(name) => format!("{}:", name),
        IrInstruction::Jump(target) => format!("jmp {}", target),
        IrInstruction::JumpIf(cond, target) => format!("jif r{} -> {}", cond.0, target),
        IrInstruction::JumpIfNot(cond, target) => format!("jifnot r{} -> {}", cond.0, target),

        IrInstruction::Call(dst, name, args) => {
            let args_str: Vec<String> = args.iter().map(|r| format!("r{}", r.0)).collect();
            match dst {
                Some(d) => format!("r{} = call {}({})", d.0, name, args_str.join(", ")),
                None => format!("call {}({})", name, args_str.join(", ")),
            }
        }
        IrInstruction::Return(val) => match val {
            Some(r) => format!("ret r{}", r.0),
            None => "ret".to_string(),
        },

        IrInstruction::Nop => "nop".to_string(),
        IrInstruction::Alloc(dst, size) => format!("r{} = alloc(r{})", dst.0, size.0),
        IrInstruction::Syscall(dst, name, args) => {
            let args_str: Vec<String> = args.iter().map(|r| format!("r{}", r.0)).collect();
            match dst {
                Some(d) => format!("r{} = syscall {}({})", d.0, name, args_str.join(", ")),
                None => format!("syscall {}({})", name, args_str.join(", ")),
            }
        }
        IrInstruction::Log(reg, len) => format!("log r{}, len={}", reg.0, len),
    }
}

/// Disassemble sBPF bytecode with detailed annotations
pub fn disassemble_sbpf(code: &[u8], base_addr: u64) {
    println!("═══════════════════════════════════════════════════════════");
    println!("                  sBPF DISASSEMBLY");
    println!("═══════════════════════════════════════════════════════════");
    println!("  ADDR  │ BYTES                      │ INSTRUCTION");
    println!("────────┼────────────────────────────┼──────────────────────");

    let mut pc = 0;
    while pc < code.len() {
        if pc + 8 > code.len() {
            break;
        }

        let bytes = &code[pc..pc + 8];
        let opcode = bytes[0];
        let regs = bytes[1];
        let dst = regs & 0xf;
        let src = (regs >> 4) & 0xf;
        let off = i16::from_le_bytes([bytes[2], bytes[3]]);
        let imm = i32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);

        let hex = format!(
            "{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}",
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]
        );

        let (mnemonic, extra_bytes) = decode_sbpf(opcode, dst, src, off, imm, &code[pc..]);

        let addr = base_addr + pc as u64;
        println!("{:08x}│ {} │ {}", addr, hex, mnemonic);

        pc += 8 + extra_bytes;
    }
    println!("═══════════════════════════════════════════════════════════\n");
}

fn decode_sbpf(opcode: u8, dst: u8, src: u8, off: i16, imm: i32, rest: &[u8]) -> (String, usize) {
    let op_class = opcode & 0x07;
    let op_code = opcode & 0xf0;
    let op_src = opcode & 0x08;

    let mnemonic = match (op_class, op_code, op_src) {
        // ALU64 immediate
        (0x07, 0xb0, 0x00) => format!("mov64    r{}, {}", dst, imm),
        (0x07, 0x00, 0x00) => format!("add64    r{}, {}", dst, imm),
        (0x07, 0x10, 0x00) => format!("sub64    r{}, {}", dst, imm),
        (0x07, 0x20, 0x00) => format!("mul64    r{}, {}", dst, imm),
        (0x07, 0x30, 0x00) => format!("div64    r{}, {}", dst, imm),
        (0x07, 0x90, 0x00) => format!("mod64    r{}, {}", dst, imm),
        (0x07, 0x40, 0x00) => format!("or64     r{}, {}", dst, imm),
        (0x07, 0x50, 0x00) => format!("and64    r{}, {}", dst, imm),
        (0x07, 0x60, 0x00) => format!("lsh64    r{}, {}", dst, imm),
        (0x07, 0x70, 0x00) => format!("rsh64    r{}, {}", dst, imm),
        (0x07, 0x80, 0x00) => format!("neg64    r{}", dst),
        (0x07, 0xa0, 0x00) => format!("xor64    r{}, {}", dst, imm),
        (0x07, 0xc0, 0x00) => format!("arsh64   r{}, {}", dst, imm),

        // ALU64 register
        (0x07, 0xb0, 0x08) => format!("mov64    r{}, r{}", dst, src),
        (0x07, 0x00, 0x08) => format!("add64    r{}, r{}", dst, src),
        (0x07, 0x10, 0x08) => format!("sub64    r{}, r{}", dst, src),
        (0x07, 0x20, 0x08) => format!("mul64    r{}, r{}", dst, src),
        (0x07, 0x30, 0x08) => format!("div64    r{}, r{}", dst, src),
        (0x07, 0x90, 0x08) => format!("mod64    r{}, r{}", dst, src),
        (0x07, 0x40, 0x08) => format!("or64     r{}, r{}", dst, src),
        (0x07, 0x50, 0x08) => format!("and64    r{}, r{}", dst, src),
        (0x07, 0x60, 0x08) => format!("lsh64    r{}, r{}", dst, src),
        (0x07, 0x70, 0x08) => format!("rsh64    r{}, r{}", dst, src),
        (0x07, 0xa0, 0x08) => format!("xor64    r{}, r{}", dst, src),
        (0x07, 0xc0, 0x08) => format!("arsh64   r{}, r{}", dst, src),

        // Memory load
        (0x01, _, _) => {
            let sz = match opcode & 0x18 {
                0x00 => "w",
                0x08 => "h",
                0x10 => "b",
                0x18 => "dw",
                _ => "?",
            };
            format!("ldx{}    r{}, [r{}+{}]", sz, dst, src, off)
        }

        // Memory store
        (0x03, _, _) => {
            let sz = match opcode & 0x18 {
                0x00 => "w",
                0x08 => "h",
                0x10 => "b",
                0x18 => "dw",
                _ => "?",
            };
            format!("stx{}    [r{}+{}], r{}", sz, dst, off, src)
        }

        // Store immediate
        (0x02, _, _) => {
            let sz = match opcode & 0x18 {
                0x00 => "w",
                0x08 => "h",
                0x10 => "b",
                0x18 => "dw",
                _ => "?",
            };
            format!("st{}     [r{}+{}], {}", sz, dst, off, imm)
        }

        // lddw (64-bit immediate load)
        (0x00, _, _) if opcode == 0x18 => {
            if rest.len() >= 16 {
                let hi = u32::from_le_bytes([rest[12], rest[13], rest[14], rest[15]]);
                let val = ((hi as u64) << 32) | (imm as u32 as u64);
                return (format!("lddw     r{}, 0x{:016x}", dst, val), 8);
            }
            format!("lddw     r{}, {}", dst, imm)
        }

        // Jumps
        (0x05, 0x00, _) => format!("ja       {:+}", off),
        (0x05, 0x10, 0x00) => format!("jeq      r{}, {}, {:+}", dst, imm, off),
        (0x05, 0x10, 0x08) => format!("jeq      r{}, r{}, {:+}", dst, src, off),
        (0x05, 0x20, 0x00) => format!("jgt      r{}, {}, {:+}", dst, imm, off),
        (0x05, 0x20, 0x08) => format!("jgt      r{}, r{}, {:+}", dst, src, off),
        (0x05, 0x30, 0x00) => format!("jge      r{}, {}, {:+}", dst, imm, off),
        (0x05, 0x30, 0x08) => format!("jge      r{}, r{}, {:+}", dst, src, off),
        (0x05, 0x40, 0x00) => format!("jset     r{}, {}, {:+}", dst, imm, off),
        (0x05, 0x40, 0x08) => format!("jset     r{}, r{}, {:+}", dst, src, off),
        (0x05, 0x50, 0x00) => format!("jne      r{}, {}, {:+}", dst, imm, off),
        (0x05, 0x50, 0x08) => format!("jne      r{}, r{}, {:+}", dst, src, off),
        (0x05, 0x60, 0x00) => format!("jsgt     r{}, {}, {:+}", dst, imm, off),
        (0x05, 0x60, 0x08) => format!("jsgt     r{}, r{}, {:+}", dst, src, off),
        (0x05, 0x70, 0x00) => format!("jsge     r{}, {}, {:+}", dst, imm, off),
        (0x05, 0x70, 0x08) => format!("jsge     r{}, r{}, {:+}", dst, src, off),
        (0x05, 0x80, _) => format!("call     0x{:08x}", imm),
        (0x05, 0x90, _) => "exit".to_string(),
        (0x05, 0xa0, 0x00) => format!("jlt      r{}, {}, {:+}", dst, imm, off),
        (0x05, 0xa0, 0x08) => format!("jlt      r{}, r{}, {:+}", dst, src, off),
        (0x05, 0xb0, 0x00) => format!("jle      r{}, {}, {:+}", dst, imm, off),
        (0x05, 0xb0, 0x08) => format!("jle      r{}, r{}, {:+}", dst, src, off),
        (0x05, 0xc0, 0x00) => format!("jslt     r{}, {}, {:+}", dst, imm, off),
        (0x05, 0xc0, 0x08) => format!("jslt     r{}, r{}, {:+}", dst, src, off),
        (0x05, 0xd0, 0x00) => format!("jsle     r{}, {}, {:+}", dst, imm, off),
        (0x05, 0xd0, 0x08) => format!("jsle     r{}, r{}, {:+}", dst, src, off),

        _ => format!("???      opcode=0x{:02x}", opcode),
    };

    (mnemonic, 0)
}

/// Track register allocation decisions
#[derive(Debug, Default)]
pub struct RegAllocTrace {
    /// Virtual->physical register allocations with reason
    pub allocations: Vec<(IrReg, SbpfReg, &'static str)>,
    /// Spilled registers with stack offsets
    pub spills: Vec<(IrReg, i16)>,
    /// Reloaded registers with stack offset and destination
    pub reloads: Vec<(IrReg, i16, SbpfReg)>,
}

impl RegAllocTrace {
    /// Create a new register allocation trace
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a virtual-to-physical register allocation
    pub fn record_alloc(&mut self, virt: IrReg, phys: SbpfReg, reason: &'static str) {
        self.allocations.push((virt, phys, reason));
    }

    /// Record a register spill to stack
    pub fn record_spill(&mut self, virt: IrReg, offset: i16) {
        self.spills.push((virt, offset));
    }

    /// Record a register reload from stack
    pub fn record_reload(&mut self, virt: IrReg, offset: i16, into: SbpfReg) {
        self.reloads.push((virt, offset, into));
    }

    /// Print formatted register allocation trace
    pub fn dump(&self) {
        println!("═══════════════════════════════════════════════════════════");
        println!("              REGISTER ALLOCATION TRACE");
        println!("═══════════════════════════════════════════════════════════");

        println!("\nAllocations:");
        for (virt, phys, reason) in &self.allocations {
            println!("  IR r{} -> {} ({})", virt.0, phys_name(*phys), reason);
        }

        println!("\nSpills:");
        for (virt, offset) in &self.spills {
            println!("  IR r{} -> [r10{:+}]", virt.0, offset);
        }

        println!("\nReloads:");
        for (virt, offset, into) in &self.reloads {
            println!(
                "  IR r{} <- [r10{:+}] into {}",
                virt.0,
                offset,
                phys_name(*into)
            );
        }

        println!("═══════════════════════════════════════════════════════════\n");
    }
}

fn phys_name(reg: SbpfReg) -> &'static str {
    match reg {
        SbpfReg::R0 => "R0",
        SbpfReg::R1 => "R1",
        SbpfReg::R2 => "R2",
        SbpfReg::R3 => "R3",
        SbpfReg::R4 => "R4",
        SbpfReg::R5 => "R5",
        SbpfReg::R6 => "R6",
        SbpfReg::R7 => "R7",
        SbpfReg::R8 => "R8",
        SbpfReg::R9 => "R9",
        SbpfReg::R10 => "R10",
    }
}

/// Validate sBPF bytecode for common issues
pub fn validate_sbpf(code: &[u8]) -> Vec<String> {
    let mut errors = Vec::new();

    if code.is_empty() {
        errors.push("Empty bytecode".to_string());
        return errors;
    }

    if !code.len().is_multiple_of(8) {
        errors.push(format!(
            "Bytecode length {} not aligned to 8 bytes",
            code.len()
        ));
    }

    let mut pc = 0;
    let mut has_exit = false;
    let mut instruction_count = 0;

    while pc < code.len() {
        if pc + 8 > code.len() {
            errors.push(format!("Truncated instruction at offset 0x{:x}", pc));
            break;
        }

        let opcode = code[pc];
        let regs = code[pc + 1];
        let dst = regs & 0xf;
        let src = (regs >> 4) & 0xf;

        // Check register bounds
        if dst > 10 {
            errors.push(format!("Invalid dst register {} at 0x{:x}", dst, pc));
        }
        if src > 10 {
            errors.push(format!("Invalid src register {} at 0x{:x}", src, pc));
        }

        // Check for same-register operations that are likely bugs
        let op_class = opcode & 0x07;
        let op_code = opcode & 0xf0;
        let op_src = opcode & 0x08;

        if op_class == 0x07 && op_src == 0x08 {
            // ALU64 register operation
            if dst == src && op_code != 0xb0 {
                // Same src and dst is suspicious for non-mov operations
                let op_name = match op_code {
                    0x00 => "add",
                    0x10 => "sub",
                    0x20 => "mul",
                    0x30 => "div",
                    0x90 => "mod",
                    _ => "alu",
                };
                if op_code == 0x10 || op_code == 0x30 || op_code == 0x90 {
                    // sub r, r -> 0; div r, r -> 1; mod r, r -> 0
                    errors.push(format!(
                        "Suspicious {}64 r{}, r{} at 0x{:x} (always produces constant)",
                        op_name, dst, src, pc
                    ));
                }
            }
        }

        // Check for exit
        if opcode == 0x95 {
            has_exit = true;
        }

        // Check for R10 write (illegal)
        if dst == 10 {
            let is_store = op_class == 0x02 || op_class == 0x03;
            if !is_store {
                errors.push(format!("Write to R10 (frame pointer) at 0x{:x}", pc));
            }
        }

        // Handle lddw (16-byte instruction)
        if opcode == 0x18 {
            pc += 16;
        } else {
            pc += 8;
        }

        instruction_count += 1;

        if instruction_count > 65536 {
            errors.push("Program exceeds 65536 instructions".to_string());
            break;
        }
    }

    if !has_exit {
        errors.push("No exit instruction found".to_string());
    }

    errors
}

/// Extract .text section from ELF
pub fn extract_text_section(elf: &[u8]) -> Option<(u64, Vec<u8>)> {
    if elf.len() < 64 {
        return None;
    }

    // Check ELF magic
    if &elf[0..4] != b"\x7fELF" {
        return None;
    }

    let e_shoff = u64::from_le_bytes(elf[40..48].try_into().ok()?) as usize;
    let e_shentsize = u16::from_le_bytes(elf[58..60].try_into().ok()?) as usize;
    let e_shnum = u16::from_le_bytes(elf[60..62].try_into().ok()?) as usize;

    for i in 0..e_shnum {
        let sh_offset = e_shoff + i * e_shentsize;
        if sh_offset + e_shentsize > elf.len() {
            break;
        }

        let sh_type = u32::from_le_bytes(elf[sh_offset + 4..sh_offset + 8].try_into().ok()?);
        let sh_flags = u64::from_le_bytes(elf[sh_offset + 8..sh_offset + 16].try_into().ok()?);
        let sh_addr = u64::from_le_bytes(elf[sh_offset + 16..sh_offset + 24].try_into().ok()?);
        let offset =
            u64::from_le_bytes(elf[sh_offset + 24..sh_offset + 32].try_into().ok()?) as usize;
        let size =
            u64::from_le_bytes(elf[sh_offset + 32..sh_offset + 40].try_into().ok()?) as usize;

        // SHT_PROGBITS (1) with SHF_EXECINSTR (4)
        if sh_type == 1 && (sh_flags & 4) != 0 && offset + size <= elf.len() {
            return Some((sh_addr, elf[offset..offset + size].to_vec()));
        }
    }

    None
}

/// Full debug dump of compilation result
pub fn debug_compile(source: &str) {
    use crate::compiler::{CompileOptions, Compiler, IrGenerator, TypeChecker};
    use crate::{SExprParser, SExprScanner};

    println!("\n");
    println!("╔═════════════════════════════════════════════════════════════╗");
    println!("║              OVSM→sBPF COMPILATION DEBUG                    ║");
    println!("╚═════════════════════════════════════════════════════════════╝");

    println!("\n┌─────────────────────────────────────────────────────────────┐");
    println!("│ SOURCE                                                      │");
    println!("└─────────────────────────────────────────────────────────────┘");
    for (i, line) in source.lines().enumerate() {
        println!("{:3}│ {}", i + 1, line);
    }

    // Generate IR for debugging
    let ir_result = (|| -> Result<IrProgram, Box<dyn std::error::Error>> {
        let mut scanner = SExprScanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let mut parser = SExprParser::new(tokens);
        let program = parser.parse()?;
        let mut type_checker = TypeChecker::new();
        let typed = type_checker.check(&program)?;
        let mut ir_gen = IrGenerator::new();
        Ok(ir_gen.generate(&typed)?)
    })();

    if let Ok(ir) = &ir_result {
        println!();
        dump_ir(ir);
    }

    let options = CompileOptions {
        opt_level: 0,
        debug_info: true,
        ..Default::default()
    };
    let compiler = Compiler::new(options);

    match compiler.compile(source) {
        Ok(result) => {
            println!("\n┌─────────────────────────────────────────────────────────────┐");
            println!("│ COMPILATION RESULT                                          │");
            println!("└─────────────────────────────────────────────────────────────┘");
            println!("  Status:        ✅ SUCCESS");
            println!("  IR instrs:     {}", result.ir_instruction_count);
            println!("  sBPF instrs:   {}", result.sbpf_instruction_count);
            println!("  Estimated CU:  {}", result.estimated_cu);
            println!("  ELF size:      {} bytes", result.elf_bytes.len());

            if let Some(verify) = &result.verification {
                println!(
                    "  Verification:  {}",
                    if verify.valid {
                        "✅ VALID"
                    } else {
                        "❌ INVALID"
                    }
                );
                for err in &verify.errors {
                    println!("    Error: {}", err);
                }
                for warn in &verify.warnings {
                    println!("    Warning: {}", warn);
                }
            }

            for warn in &result.warnings {
                println!("  Warning: {}", warn);
            }

            // Extract and disassemble
            if let Some((addr, text)) = extract_text_section(&result.elf_bytes) {
                println!();
                disassemble_sbpf(&text, addr);

                // Validate
                let errors = validate_sbpf(&text);
                if !errors.is_empty() {
                    println!("┌─────────────────────────────────────────────────────────────┐");
                    println!("│ VALIDATION ISSUES                                           │");
                    println!("└─────────────────────────────────────────────────────────────┘");
                    for err in errors {
                        println!("  ⚠️  {}", err);
                    }
                }
            }
        }
        Err(e) => {
            println!("\n┌─────────────────────────────────────────────────────────────┐");
            println!("│ COMPILATION FAILED                                          │");
            println!("└─────────────────────────────────────────────────────────────┘");
            println!("  Error: {:?}", e);
        }
    }

    println!();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_sbpf() {
        // Valid minimal program: mov r0, 0; exit
        let valid = [
            0xb7, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x95, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00,
        ];
        assert!(validate_sbpf(&valid).is_empty());

        // No exit
        let no_exit = [0xb7, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let errs = validate_sbpf(&no_exit);
        assert!(errs.iter().any(|e| e.contains("No exit")));
    }

    #[test]
    fn test_debug_compile() {
        debug_compile("(define x 42)\n(+ x 10)");
    }
}
