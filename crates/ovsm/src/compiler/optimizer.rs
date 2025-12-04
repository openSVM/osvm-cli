//! # IR Optimizer for OVSM Compilation
//!
//! Optimization passes for the IR:
//! - Constant folding
//! - Dead code elimination
//! - Common subexpression elimination
//! - Peephole optimizations

use super::ir::{IrInstruction, IrProgram, IrReg};
use std::collections::{HashMap, HashSet};

/// Optimizer with configurable optimization level
pub struct Optimizer {
    level: u8,
}

impl Optimizer {
    /// Create a new optimizer with the specified optimization level (0-3)
    pub fn new(level: u8) -> Self {
        Self { level }
    }

    /// Run all optimization passes
    pub fn optimize(&mut self, program: &mut IrProgram) {
        if self.level >= 1 {
            self.constant_folding(program);
            self.dead_code_elimination(program);
        }

        if self.level >= 2 {
            self.common_subexpression_elimination(program);
            self.peephole_optimizations(program);
        }

        if self.level >= 3 {
            // More aggressive optimizations
            self.constant_propagation(program);
            self.strength_reduction(program);
        }

        // Always remove Nops
        self.remove_nops(program);
    }

    /// Constant folding - evaluate constant expressions at compile time
    fn constant_folding(&mut self, program: &mut IrProgram) {
        let mut constants: HashMap<IrReg, i64> = HashMap::new();

        for instr in program.instructions.iter_mut() {
            match instr {
                IrInstruction::ConstI64(dst, value) => {
                    constants.insert(*dst, *value);
                }

                IrInstruction::ConstBool(dst, value) => {
                    constants.insert(*dst, if *value { 1 } else { 0 });
                }

                IrInstruction::Add(dst, src1, src2) => {
                    let (dst, src1, src2) = (*dst, *src1, *src2);
                    if let (Some(&v1), Some(&v2)) = (constants.get(&src1), constants.get(&src2)) {
                        let result = v1.wrapping_add(v2);
                        *instr = IrInstruction::ConstI64(dst, result);
                        constants.insert(dst, result);
                    }
                }

                IrInstruction::Sub(dst, src1, src2) => {
                    let (dst, src1, src2) = (*dst, *src1, *src2);
                    if let (Some(&v1), Some(&v2)) = (constants.get(&src1), constants.get(&src2)) {
                        let result = v1.wrapping_sub(v2);
                        *instr = IrInstruction::ConstI64(dst, result);
                        constants.insert(dst, result);
                    }
                }

                IrInstruction::Mul(dst, src1, src2) => {
                    let (dst, src1, src2) = (*dst, *src1, *src2);
                    if let (Some(&v1), Some(&v2)) = (constants.get(&src1), constants.get(&src2)) {
                        let result = v1.wrapping_mul(v2);
                        *instr = IrInstruction::ConstI64(dst, result);
                        constants.insert(dst, result);
                    }
                }

                IrInstruction::Div(dst, src1, src2) => {
                    let (dst, src1, src2) = (*dst, *src1, *src2);
                    if let (Some(&v1), Some(&v2)) = (constants.get(&src1), constants.get(&src2)) {
                        if v2 != 0 {
                            let result = v1 / v2;
                            *instr = IrInstruction::ConstI64(dst, result);
                            constants.insert(dst, result);
                        }
                    }
                }

                IrInstruction::Mod(dst, src1, src2) => {
                    let (dst, src1, src2) = (*dst, *src1, *src2);
                    if let (Some(&v1), Some(&v2)) = (constants.get(&src1), constants.get(&src2)) {
                        if v2 != 0 {
                            let result = v1 % v2;
                            *instr = IrInstruction::ConstI64(dst, result);
                            constants.insert(dst, result);
                        }
                    }
                }

                IrInstruction::Eq(dst, src1, src2) => {
                    let (dst, src1, src2) = (*dst, *src1, *src2);
                    if let (Some(&v1), Some(&v2)) = (constants.get(&src1), constants.get(&src2)) {
                        let result = if v1 == v2 { 1 } else { 0 };
                        *instr = IrInstruction::ConstI64(dst, result);
                        constants.insert(dst, result);
                    }
                }

                IrInstruction::Lt(dst, src1, src2) => {
                    let (dst, src1, src2) = (*dst, *src1, *src2);
                    if let (Some(&v1), Some(&v2)) = (constants.get(&src1), constants.get(&src2)) {
                        let result = if v1 < v2 { 1 } else { 0 };
                        *instr = IrInstruction::ConstI64(dst, result);
                        constants.insert(dst, result);
                    }
                }

                IrInstruction::Neg(dst, src) => {
                    let (dst, src) = (*dst, *src);
                    if let Some(&v) = constants.get(&src) {
                        let result = -v;
                        *instr = IrInstruction::ConstI64(dst, result);
                        constants.insert(dst, result);
                    }
                }

                IrInstruction::Not(dst, src) => {
                    let (dst, src) = (*dst, *src);
                    if let Some(&v) = constants.get(&src) {
                        let result = if v == 0 { 1 } else { 0 };
                        *instr = IrInstruction::ConstI64(dst, result);
                        constants.insert(dst, result);
                    }
                }

                // Labels and jumps invalidate our constant tracking
                IrInstruction::Label(_)
                | IrInstruction::Jump(_)
                | IrInstruction::JumpIf(_, _)
                | IrInstruction::JumpIfNot(_, _) => {
                    constants.clear();
                }

                _ => {}
            }
        }
    }

    /// Dead code elimination - remove instructions whose results are never used
    fn dead_code_elimination(&mut self, program: &mut IrProgram) {
        // Find all used registers
        let mut used_regs: HashSet<IrReg> = HashSet::new();

        // First pass: find all registers that are used
        for instr in &program.instructions {
            match instr {
                IrInstruction::Add(_, src1, src2)
                | IrInstruction::Sub(_, src1, src2)
                | IrInstruction::Mul(_, src1, src2)
                | IrInstruction::Div(_, src1, src2)
                | IrInstruction::Mod(_, src1, src2)
                | IrInstruction::Eq(_, src1, src2)
                | IrInstruction::Ne(_, src1, src2)
                | IrInstruction::Lt(_, src1, src2)
                | IrInstruction::Le(_, src1, src2)
                | IrInstruction::Gt(_, src1, src2)
                | IrInstruction::Ge(_, src1, src2)
                | IrInstruction::And(_, src1, src2)
                | IrInstruction::Or(_, src1, src2) => {
                    used_regs.insert(*src1);
                    used_regs.insert(*src2);
                }

                IrInstruction::Neg(_, src)
                | IrInstruction::Not(_, src)
                | IrInstruction::Move(_, src) => {
                    used_regs.insert(*src);
                }

                IrInstruction::JumpIf(cond, _) | IrInstruction::JumpIfNot(cond, _) => {
                    used_regs.insert(*cond);
                }

                IrInstruction::Return(Some(reg)) => {
                    used_regs.insert(*reg);
                }

                IrInstruction::Log(reg, _len) => {
                    used_regs.insert(*reg);
                }

                IrInstruction::Call(_, _, args) | IrInstruction::Syscall(_, _, args) => {
                    for arg in args {
                        used_regs.insert(*arg);
                    }
                }

                IrInstruction::Load(_, base, _) | IrInstruction::Store(base, _, _) => {
                    used_regs.insert(*base);
                }

                IrInstruction::Alloc(_, size) => {
                    used_regs.insert(*size);
                }

                _ => {}
            }
        }

        // Second pass: mark dead instructions as Nop
        for instr in program.instructions.iter_mut() {
            let dst = match instr {
                IrInstruction::ConstI64(dst, _)
                | IrInstruction::ConstF64(dst, _)
                | IrInstruction::ConstBool(dst, _)
                | IrInstruction::ConstNull(dst)
                | IrInstruction::ConstString(dst, _)
                | IrInstruction::Add(dst, _, _)
                | IrInstruction::Sub(dst, _, _)
                | IrInstruction::Mul(dst, _, _)
                | IrInstruction::Div(dst, _, _)
                | IrInstruction::Mod(dst, _, _)
                | IrInstruction::Eq(dst, _, _)
                | IrInstruction::Ne(dst, _, _)
                | IrInstruction::Lt(dst, _, _)
                | IrInstruction::Le(dst, _, _)
                | IrInstruction::Gt(dst, _, _)
                | IrInstruction::Ge(dst, _, _)
                | IrInstruction::And(dst, _, _)
                | IrInstruction::Or(dst, _, _)
                | IrInstruction::Neg(dst, _)
                | IrInstruction::Not(dst, _)
                | IrInstruction::Move(dst, _) => Some(*dst),

                _ => None,
            };

            if let Some(dst_reg) = dst {
                if !used_regs.contains(&dst_reg) {
                    *instr = IrInstruction::Nop;
                }
            }
        }
    }

    /// Common subexpression elimination
    fn common_subexpression_elimination(&mut self, program: &mut IrProgram) {
        // Track computed expressions: (op, src1, src2) -> result_reg
        let mut computed: HashMap<(u8, u32, u32), IrReg> = HashMap::new();

        for instr in program.instructions.iter_mut() {
            match instr {
                IrInstruction::Add(dst, src1, src2) => {
                    let key = (0, src1.0, src2.0);
                    if let Some(&existing) = computed.get(&key) {
                        *instr = IrInstruction::Move(*dst, existing);
                    } else {
                        computed.insert(key, *dst);
                    }
                }
                IrInstruction::Sub(dst, src1, src2) => {
                    let key = (1, src1.0, src2.0);
                    if let Some(&existing) = computed.get(&key) {
                        *instr = IrInstruction::Move(*dst, existing);
                    } else {
                        computed.insert(key, *dst);
                    }
                }
                IrInstruction::Mul(dst, src1, src2) => {
                    let key = (2, src1.0, src2.0);
                    if let Some(&existing) = computed.get(&key) {
                        *instr = IrInstruction::Move(*dst, existing);
                    } else {
                        computed.insert(key, *dst);
                    }
                }

                // Labels invalidate computed expressions
                IrInstruction::Label(_) | IrInstruction::Jump(_) => {
                    computed.clear();
                }

                _ => {}
            }
        }
    }

    /// Peephole optimizations - local pattern matching
    fn peephole_optimizations(&mut self, program: &mut IrProgram) {
        let instructions = &mut program.instructions;

        for i in 0..instructions.len() {
            // x + 0 = x
            if let IrInstruction::Add(dst, src1, src2) = &instructions[i] {
                // Check if src2 is constant 0 in previous instruction
                if i > 0 {
                    if let IrInstruction::ConstI64(const_reg, 0) = &instructions[i - 1] {
                        if const_reg == src2 {
                            instructions[i] = IrInstruction::Move(*dst, *src1);
                        }
                    }
                }
            }

            // x * 1 = x
            if let IrInstruction::Mul(dst, src1, src2) = &instructions[i] {
                if i > 0 {
                    if let IrInstruction::ConstI64(const_reg, 1) = &instructions[i - 1] {
                        if const_reg == src2 {
                            instructions[i] = IrInstruction::Move(*dst, *src1);
                        }
                    }
                }
            }

            // x * 0 = 0
            if let IrInstruction::Mul(dst, _, src2) = &instructions[i] {
                if i > 0 {
                    if let IrInstruction::ConstI64(const_reg, 0) = &instructions[i - 1] {
                        if const_reg == src2 {
                            instructions[i] = IrInstruction::ConstI64(*dst, 0);
                        }
                    }
                }
            }

            // x * 2 = x + x (cheaper on some architectures)
            if let IrInstruction::Mul(dst, src1, src2) = &instructions[i] {
                if i > 0 {
                    if let IrInstruction::ConstI64(const_reg, 2) = &instructions[i - 1] {
                        if const_reg == src2 {
                            instructions[i] = IrInstruction::Add(*dst, *src1, *src1);
                        }
                    }
                }
            }
        }
    }

    /// Constant propagation
    fn constant_propagation(&mut self, program: &mut IrProgram) {
        let mut constants: HashMap<IrReg, i64> = HashMap::new();

        for instr in program.instructions.iter_mut() {
            match instr {
                IrInstruction::ConstI64(dst, value) => {
                    constants.insert(*dst, *value);
                }
                IrInstruction::Move(dst, src) => {
                    let (dst, src) = (*dst, *src);
                    if let Some(&value) = constants.get(&src) {
                        *instr = IrInstruction::ConstI64(dst, value);
                        constants.insert(dst, value);
                    }
                }
                IrInstruction::Label(_) | IrInstruction::Jump(_) => {
                    constants.clear();
                }
                _ => {}
            }
        }
    }

    /// Strength reduction - replace expensive ops with cheaper ones
    fn strength_reduction(&mut self, program: &mut IrProgram) {
        // Power-of-2 division -> right shift
        // Power-of-2 multiplication -> left shift
        // (Not implemented yet - requires adding shift instructions to IR)
        let _ = program;
    }

    /// Remove all Nop instructions
    fn remove_nops(&mut self, program: &mut IrProgram) {
        program
            .instructions
            .retain(|instr| !matches!(instr, IrInstruction::Nop));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_optimizer_creation() {
        let optimizer = Optimizer::new(2);
        assert_eq!(optimizer.level, 2);
    }

    #[test]
    fn test_constant_folding() {
        let mut program = IrProgram::new();
        program.instructions = vec![
            IrInstruction::ConstI64(IrReg(0), 10),
            IrInstruction::ConstI64(IrReg(1), 20),
            IrInstruction::Add(IrReg(2), IrReg(0), IrReg(1)),
        ];

        let mut optimizer = Optimizer::new(1);
        optimizer.constant_folding(&mut program);

        // The Add should be replaced with ConstI64(2, 30)
        if let IrInstruction::ConstI64(reg, value) = &program.instructions[2] {
            assert_eq!(reg.0, 2);
            assert_eq!(*value, 30);
        } else {
            panic!("Expected constant folding to work");
        }
    }
}
