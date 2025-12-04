//! Register Pressure Analyzer for OVSM sBPF Codegen
//!
//! This module provides tooling to analyze register allocation and detect
//! potential issues like:
//! - Register spilling problems
//! - Values clobbered before use
//! - High register pressure points
//! - Syscall-induced register loss
//!
//! Usage:
//! ```rust
//! let analyzer = RegAllocAnalyzer::new();
//! let report = analyzer.analyze(&ir_program);
//! println!("{}", report.format());
//! ```

use super::ir::{IrInstruction, IrProgram, IrReg};
use super::sbpf_codegen::SbpfReg;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

/// Analysis result for a single IR instruction
#[derive(Debug, Clone)]
pub struct InstructionAnalysis {
    /// IR instruction index
    pub index: usize,
    /// Virtual registers defined (written to) by this instruction
    pub defs: Vec<IrReg>,
    /// Virtual registers used (read from) by this instruction
    pub uses: Vec<IrReg>,
    /// Live virtual registers at this point
    pub live_regs: HashSet<IrReg>,
    /// Number of physical registers needed at this point
    pub pressure: usize,
    /// Whether this instruction causes spilling
    pub causes_spill: bool,
    /// Whether this instruction is a syscall (clobbers R1-R5)
    pub is_syscall: bool,
    /// Human-readable description
    pub description: String,
}

/// Potential bug or issue detected
#[derive(Debug, Clone)]
pub struct RegAllocIssue {
    /// IR instruction index where issue occurs
    pub index: usize,
    /// Severity: "critical", "warning", "info"
    pub severity: &'static str,
    /// Description of the issue
    pub message: String,
    /// Affected virtual register (if applicable)
    pub affected_reg: Option<IrReg>,
}

/// Complete analysis report
#[derive(Debug)]
pub struct RegAllocReport {
    /// Per-instruction analysis
    pub instructions: Vec<InstructionAnalysis>,
    /// Detected issues
    pub issues: Vec<RegAllocIssue>,
    /// Peak register pressure
    pub peak_pressure: usize,
    /// Index of peak pressure instruction
    pub peak_pressure_index: usize,
    /// Total number of spills
    pub total_spills: usize,
    /// Physical registers available
    pub available_regs: usize,
}

impl RegAllocReport {
    /// Format report as human-readable text
    pub fn format(&self) -> String {
        let mut output = String::new();

        writeln!(
            output,
            "╔══════════════════════════════════════════════════════════════════╗"
        )
        .unwrap();
        writeln!(
            output,
            "║            REGISTER ALLOCATION ANALYSIS REPORT                  ║"
        )
        .unwrap();
        writeln!(
            output,
            "╠══════════════════════════════════════════════════════════════════╣"
        )
        .unwrap();
        writeln!(
            output,
            "║ Available Registers: {} (R3-R5, R8-R9)                          ║",
            self.available_regs
        )
        .unwrap();
        writeln!(
            output,
            "║ Peak Register Pressure: {} at instruction #{}                    ║",
            self.peak_pressure, self.peak_pressure_index
        )
        .unwrap();
        writeln!(
            output,
            "║ Total Spills: {}                                                 ║",
            self.total_spills
        )
        .unwrap();
        writeln!(
            output,
            "║ Issues Found: {}                                                 ║",
            self.issues.len()
        )
        .unwrap();
        writeln!(
            output,
            "╚══════════════════════════════════════════════════════════════════╝"
        )
        .unwrap();

        // Issues section
        if !self.issues.is_empty() {
            writeln!(output, "\n🚨 ISSUES DETECTED:").unwrap();
            writeln!(
                output,
                "─────────────────────────────────────────────────────────────────────"
            )
            .unwrap();
            for issue in &self.issues {
                let icon = match issue.severity {
                    "critical" => "🔴",
                    "warning" => "🟡",
                    _ => "🔵",
                };
                writeln!(
                    output,
                    "{} [{}] IR #{}: {}",
                    icon,
                    issue.severity.to_uppercase(),
                    issue.index,
                    issue.message
                )
                .unwrap();
                if let Some(reg) = issue.affected_reg {
                    writeln!(output, "   Affected register: R{}", reg.0).unwrap();
                }
            }
        }

        // Pressure timeline
        writeln!(output, "\n📊 REGISTER PRESSURE TIMELINE:").unwrap();
        writeln!(
            output,
            "─────────────────────────────────────────────────────────────────────"
        )
        .unwrap();

        let max_bar_width = 50;
        for instr in &self.instructions {
            let bar_len = (instr.pressure * max_bar_width) / self.available_regs.max(1);
            let bar = "█".repeat(bar_len.min(max_bar_width));
            let overflow = if instr.pressure > self.available_regs {
                " ⚠️ SPILL"
            } else {
                ""
            };
            let syscall_marker = if instr.is_syscall { " 📞" } else { "" };

            writeln!(
                output,
                "{:4} │{:<50}│ {}/{}{}{}",
                instr.index, bar, instr.pressure, self.available_regs, overflow, syscall_marker
            )
            .unwrap();
        }

        // Detailed instruction trace (first 20 instructions)
        writeln!(output, "\n📋 DETAILED INSTRUCTION TRACE (first 30):").unwrap();
        writeln!(
            output,
            "─────────────────────────────────────────────────────────────────────"
        )
        .unwrap();
        writeln!(
            output,
            "{:4} {:40} {:15} {:15} {:6}",
            "IDX", "INSTRUCTION", "DEFS", "USES", "LIVE"
        )
        .unwrap();
        writeln!(
            output,
            "─────────────────────────────────────────────────────────────────────"
        )
        .unwrap();

        for instr in self.instructions.iter().take(30) {
            let defs_str: String = instr
                .defs
                .iter()
                .map(|r| format!("R{}", r.0))
                .collect::<Vec<_>>()
                .join(",");
            let uses_str: String = instr
                .uses
                .iter()
                .map(|r| format!("R{}", r.0))
                .collect::<Vec<_>>()
                .join(",");
            let live_count = instr.live_regs.len();

            let desc = if instr.description.len() > 38 {
                format!("{}...", &instr.description[..35])
            } else {
                instr.description.clone()
            };

            writeln!(
                output,
                "{:4} {:40} {:15} {:15} {:6}",
                instr.index,
                desc,
                if defs_str.is_empty() {
                    "-".to_string()
                } else {
                    defs_str
                },
                if uses_str.is_empty() {
                    "-".to_string()
                } else {
                    uses_str
                },
                live_count
            )
            .unwrap();
        }

        output
    }

    /// Get JSON representation for programmatic analysis
    pub fn to_json(&self) -> String {
        format!(
            r#"{{
  "available_regs": {},
  "peak_pressure": {},
  "peak_pressure_index": {},
  "total_spills": {},
  "issue_count": {},
  "issues": [{}]
}}"#,
            self.available_regs,
            self.peak_pressure,
            self.peak_pressure_index,
            self.total_spills,
            self.issues.len(),
            self.issues
                .iter()
                .map(|i| format!(
                    r#"{{"index": {}, "severity": "{}", "message": "{}"}}"#,
                    i.index,
                    i.severity,
                    i.message.replace('"', "'")
                ))
                .collect::<Vec<_>>()
                .join(",\n    ")
        )
    }
}

/// Register allocation analyzer
pub struct RegAllocAnalyzer {
    /// Number of available registers (R3-R5, R8-R9 = 5)
    available_regs: usize,
    /// Reserved registers (R0=return, R1-R2=ABI, R6-R7=saved, R10=FP)
    reserved: HashSet<u32>,
}

impl Default for RegAllocAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl RegAllocAnalyzer {
    /// Create a new register allocator analyzer with Solana ABI constraints
    pub fn new() -> Self {
        let mut reserved = HashSet::new();
        reserved.insert(0); // R0 - return value
        reserved.insert(1); // R1 - accounts pointer (ABI)
        reserved.insert(2); // R2 - instruction data (ABI)
        reserved.insert(6); // R6 - saved accounts
        reserved.insert(7); // R7 - saved instr data
        reserved.insert(10); // R10 - frame pointer

        Self {
            available_regs: 5, // R3, R4, R5, R8, R9
            reserved,
        }
    }

    /// Analyze an IR program for register allocation issues
    pub fn analyze(&self, program: &IrProgram) -> RegAllocReport {
        let mut instructions = Vec::new();
        let mut issues = Vec::new();
        let mut peak_pressure = 0;
        let mut peak_pressure_index = 0;
        let mut total_spills = 0;

        // First pass: collect all uses and defs
        let mut all_uses: Vec<Vec<IrReg>> = Vec::new();
        let mut all_defs: Vec<Vec<IrReg>> = Vec::new();
        let mut descriptions: Vec<String> = Vec::new();
        let mut syscall_flags: Vec<bool> = Vec::new();

        for (idx, ir) in program.instructions.iter().enumerate() {
            let (defs, uses, desc, is_syscall) = self.analyze_instruction(ir, idx);
            all_defs.push(defs);
            all_uses.push(uses);
            descriptions.push(desc);
            syscall_flags.push(is_syscall);
        }

        // Second pass: compute liveness (backward)
        let mut live_out: Vec<HashSet<IrReg>> = vec![HashSet::new(); program.instructions.len()];

        // Initialize with uses in last instruction
        if let Some(last_uses) = all_uses.last() {
            for r in last_uses {
                live_out[program.instructions.len() - 1].insert(*r);
            }
        }

        // Backward liveness propagation
        for i in (0..program.instructions.len()).rev() {
            let mut live_at = if i + 1 < live_out.len() {
                live_out[i + 1].clone()
            } else {
                HashSet::new()
            };

            // Remove defs (they're defined here, not live before)
            for def in &all_defs[i] {
                live_at.remove(def);
            }

            // Add uses (they must be live before this instruction)
            for use_reg in &all_uses[i] {
                live_at.insert(*use_reg);
            }

            live_out[i] = live_at;
        }

        // Third pass: generate analysis with liveness info
        for (idx, ir) in program.instructions.iter().enumerate() {
            let defs = all_defs[idx].clone();
            let uses = all_uses[idx].clone();
            let desc = descriptions[idx].clone();
            let is_syscall = syscall_flags[idx];

            // Compute pressure: live registers at this point
            let live_regs = live_out[idx].clone();
            let pressure = live_regs.len();

            // Check for spilling
            let causes_spill = pressure > self.available_regs;
            if causes_spill {
                total_spills += 1;
            }

            // Track peak pressure
            if pressure > peak_pressure {
                peak_pressure = pressure;
                peak_pressure_index = idx;
            }

            // Detect issues

            // Issue: Syscall with high pressure (clobbers R1-R5)
            if is_syscall && pressure > 2 {
                let caller_saved_live: Vec<_> =
                    live_regs.iter().filter(|r| r.0 >= 3 && r.0 <= 5).collect();
                if !caller_saved_live.is_empty() {
                    issues.push(RegAllocIssue {
                        index: idx,
                        severity: "warning",
                        message: format!(
                            "Syscall may clobber live values in caller-saved registers: {:?}",
                            caller_saved_live
                                .iter()
                                .map(|r| format!("R{}", r.0))
                                .collect::<Vec<_>>()
                        ),
                        affected_reg: caller_saved_live.first().copied().copied(),
                    });
                }
            }

            // Issue: Very high pressure
            if pressure > self.available_regs + 2 {
                issues.push(RegAllocIssue {
                    index: idx,
                    severity: "critical",
                    message: format!(
                        "Register pressure ({}) significantly exceeds available ({}) - multiple spills likely",
                        pressure, self.available_regs
                    ),
                    affected_reg: None,
                });
            }

            // Issue: Large constant potentially spilled
            if let IrInstruction::ConstI64(dst, val) = ir {
                if (*val > i32::MAX as i64 || *val < i32::MIN as i64)
                    && pressure >= self.available_regs
                {
                    issues.push(RegAllocIssue {
                            index: idx,
                            severity: "critical",
                            message: format!(
                                "64-bit constant 0x{:X} loaded at high pressure - may be spilled before use",
                                val
                            ),
                            affected_reg: Some(*dst),
                        });
                }
            }

            instructions.push(InstructionAnalysis {
                index: idx,
                defs,
                uses,
                live_regs,
                pressure,
                causes_spill,
                is_syscall,
                description: desc,
            });
        }

        // Check for use-before-def
        for (idx, instr) in instructions.iter().enumerate() {
            for use_reg in &instr.uses {
                // Check if this register was ever defined before this point
                let defined_before = instructions
                    .iter()
                    .take(idx)
                    .any(|i| i.defs.contains(use_reg));

                // Also check if it's a pre-allocated register (1,2,6,7)
                let is_prealloc = use_reg.0 <= 7 && use_reg.0 != 0;

                if !defined_before && !is_prealloc {
                    issues.push(RegAllocIssue {
                        index: idx,
                        severity: "critical",
                        message: format!("Register R{} used before definition!", use_reg.0),
                        affected_reg: Some(*use_reg),
                    });
                }
            }
        }

        RegAllocReport {
            instructions,
            issues,
            peak_pressure,
            peak_pressure_index,
            total_spills,
            available_regs: self.available_regs,
        }
    }

    /// Analyze a single IR instruction
    fn analyze_instruction(
        &self,
        ir: &IrInstruction,
        _idx: usize,
    ) -> (Vec<IrReg>, Vec<IrReg>, String, bool) {
        let mut defs = Vec::new();
        let mut uses = Vec::new();
        let desc: String;
        let mut is_syscall = false;

        match ir {
            IrInstruction::ConstI64(dst, val) => {
                defs.push(*dst);
                desc = format!("ConstI64 R{} = 0x{:X}", dst.0, val);
            }
            IrInstruction::ConstF64(dst, _) => {
                defs.push(*dst);
                desc = format!("ConstF64 R{}", dst.0);
            }
            IrInstruction::ConstBool(dst, val) => {
                defs.push(*dst);
                desc = format!("ConstBool R{} = {}", dst.0, val);
            }
            IrInstruction::ConstNull(dst) => {
                defs.push(*dst);
                desc = format!("ConstNull R{}", dst.0);
            }
            IrInstruction::ConstString(dst, idx) => {
                defs.push(*dst);
                desc = format!("ConstString R{} = str[{}]", dst.0, idx);
            }
            IrInstruction::Add(dst, a, b) => {
                defs.push(*dst);
                uses.push(*a);
                uses.push(*b);
                desc = format!("Add R{} = R{} + R{}", dst.0, a.0, b.0);
            }
            IrInstruction::Sub(dst, a, b) => {
                defs.push(*dst);
                uses.push(*a);
                uses.push(*b);
                desc = format!("Sub R{} = R{} - R{}", dst.0, a.0, b.0);
            }
            IrInstruction::Mul(dst, a, b) => {
                defs.push(*dst);
                uses.push(*a);
                uses.push(*b);
                desc = format!("Mul R{} = R{} * R{}", dst.0, a.0, b.0);
            }
            IrInstruction::Div(dst, a, b) => {
                defs.push(*dst);
                uses.push(*a);
                uses.push(*b);
                desc = format!("Div R{} = R{} / R{}", dst.0, a.0, b.0);
            }
            IrInstruction::Mod(dst, a, b) => {
                defs.push(*dst);
                uses.push(*a);
                uses.push(*b);
                desc = format!("Mod R{} = R{} % R{}", dst.0, a.0, b.0);
            }
            IrInstruction::Eq(dst, a, b)
            | IrInstruction::Ne(dst, a, b)
            | IrInstruction::Lt(dst, a, b)
            | IrInstruction::Le(dst, a, b)
            | IrInstruction::Gt(dst, a, b)
            | IrInstruction::Ge(dst, a, b) => {
                defs.push(*dst);
                uses.push(*a);
                uses.push(*b);
                desc = format!("Cmp R{} = R{} op R{}", dst.0, a.0, b.0);
            }
            IrInstruction::And(dst, a, b) | IrInstruction::Or(dst, a, b) => {
                defs.push(*dst);
                uses.push(*a);
                uses.push(*b);
                desc = format!("Logic R{} = R{} op R{}", dst.0, a.0, b.0);
            }
            IrInstruction::Not(dst, src) => {
                defs.push(*dst);
                uses.push(*src);
                desc = format!("Not R{} = !R{}", dst.0, src.0);
            }
            IrInstruction::Neg(dst, src) => {
                defs.push(*dst);
                uses.push(*src);
                desc = format!("Neg R{} = -R{}", dst.0, src.0);
            }
            IrInstruction::Move(dst, src) => {
                defs.push(*dst);
                uses.push(*src);
                desc = format!("Move R{} = R{}", dst.0, src.0);
            }
            IrInstruction::Load(dst, base, offset) => {
                defs.push(*dst);
                uses.push(*base);
                desc = format!("Load R{} = [R{}+{}]", dst.0, base.0, offset);
            }
            IrInstruction::Load1(dst, base, offset) => {
                defs.push(*dst);
                uses.push(*base);
                desc = format!("Load1 R{} = (u8)[R{}+{}]", dst.0, base.0, offset);
            }
            IrInstruction::Load2(dst, base, offset) => {
                defs.push(*dst);
                uses.push(*base);
                desc = format!("Load2 R{} = (u16)[R{}+{}]", dst.0, base.0, offset);
            }
            IrInstruction::Load4(dst, base, offset) => {
                defs.push(*dst);
                uses.push(*base);
                desc = format!("Load4 R{} = (u32)[R{}+{}]", dst.0, base.0, offset);
            }
            IrInstruction::Store(base, src, offset) => {
                uses.push(*base);
                uses.push(*src);
                desc = format!("Store [R{}+{}] = R{}", base.0, offset, src.0);
            }
            IrInstruction::Store1(base, src, offset) => {
                uses.push(*base);
                uses.push(*src);
                desc = format!("Store1 (u8)[R{}+{}] = R{}", base.0, offset, src.0);
            }
            IrInstruction::Store2(base, src, offset) => {
                uses.push(*base);
                uses.push(*src);
                desc = format!("Store2 (u16)[R{}+{}] = R{}", base.0, offset, src.0);
            }
            IrInstruction::Store4(base, src, offset) => {
                uses.push(*base);
                uses.push(*src);
                desc = format!("Store4 (u32)[R{}+{}] = R{}", base.0, offset, src.0);
            }
            IrInstruction::Alloc(dst, size) => {
                defs.push(*dst);
                uses.push(*size);
                desc = format!("Alloc R{} = alloc(R{})", dst.0, size.0);
            }
            IrInstruction::Call(dst, name, args) => {
                if let Some(d) = dst {
                    defs.push(*d);
                }
                for arg in args {
                    uses.push(*arg);
                }
                is_syscall = true;
                desc = format!("Call {} ({} args)", name, args.len());
            }
            IrInstruction::Syscall(dst, name, args) => {
                if let Some(d) = dst {
                    defs.push(*d);
                }
                for arg in args {
                    uses.push(*arg);
                }
                is_syscall = true;
                desc = format!("Syscall {} ({} args)", name, args.len());
            }
            IrInstruction::Return(val) => {
                if let Some(v) = val {
                    uses.push(*v);
                }
                desc = "Return".to_string();
            }
            IrInstruction::Label(name) => {
                desc = format!("Label: {}", name);
            }
            IrInstruction::Jump(target) => {
                desc = format!("Jump -> {}", target);
            }
            IrInstruction::JumpIf(cond, target) => {
                uses.push(*cond);
                desc = format!("JumpIf R{} -> {}", cond.0, target);
            }
            IrInstruction::JumpIfNot(cond, target) => {
                uses.push(*cond);
                desc = format!("JumpIfNot R{} -> {}", cond.0, target);
            }
            IrInstruction::Log(ptr, len) => {
                uses.push(*ptr);
                is_syscall = true;
                desc = format!("Log R{} len={}", ptr.0, len);
            }
            IrInstruction::Nop => {
                desc = "Nop".to_string();
            }
        }

        (defs, uses, desc, is_syscall)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn make_program(instructions: Vec<IrInstruction>) -> IrProgram {
        IrProgram {
            instructions,
            blocks: HashMap::new(),
            string_table: vec![],
            entry_label: "entry".to_string(),
            var_registers: HashMap::new(),
        }
    }

    #[test]
    fn test_basic_analysis() {
        let analyzer = RegAllocAnalyzer::new();
        let program = make_program(vec![
            IrInstruction::ConstI64(IrReg(10), 100),
            IrInstruction::ConstI64(IrReg(11), 200),
            IrInstruction::Add(IrReg(12), IrReg(10), IrReg(11)),
        ]);

        let report = analyzer.analyze(&program);
        assert_eq!(report.instructions.len(), 3);
        assert!(report.peak_pressure <= 3);
    }

    #[test]
    fn test_high_pressure_detection() {
        let analyzer = RegAllocAnalyzer::new();
        // Create a program that uses many registers simultaneously
        // All registers must be used in a chain where each is live at the same point
        let mut instructions = vec![];

        // Load 10 constants
        for i in 10..20 {
            instructions.push(IrInstruction::ConstI64(IrReg(i), i as i64));
        }

        // Use ALL of them in a chain - this keeps all registers live
        instructions.push(IrInstruction::Add(IrReg(20), IrReg(10), IrReg(11)));
        instructions.push(IrInstruction::Add(IrReg(21), IrReg(20), IrReg(12)));
        instructions.push(IrInstruction::Add(IrReg(22), IrReg(21), IrReg(13)));
        instructions.push(IrInstruction::Add(IrReg(23), IrReg(22), IrReg(14)));
        instructions.push(IrInstruction::Add(IrReg(24), IrReg(23), IrReg(15)));
        instructions.push(IrInstruction::Add(IrReg(25), IrReg(24), IrReg(16)));
        instructions.push(IrInstruction::Add(IrReg(26), IrReg(25), IrReg(17)));
        instructions.push(IrInstruction::Add(IrReg(27), IrReg(26), IrReg(18)));
        instructions.push(IrInstruction::Add(IrReg(28), IrReg(27), IrReg(19)));

        let program = make_program(instructions);

        let report = analyzer.analyze(&program);
        // Should detect high pressure (10 registers needed at peak)
        assert!(
            report.peak_pressure > 5,
            "Expected peak_pressure > 5, got {}",
            report.peak_pressure
        );
        // The spill count depends on whether graph coloring finds better solutions
        // We just verify we detected high pressure; spills may or may not occur
    }
}
