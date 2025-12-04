//! IR program and basic block definitions

use super::instruction::IrInstruction;
use super::instruction::IrReg;
use std::collections::HashMap;

/// Basic block in the control flow graph
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<IrInstruction>,
    pub successors: Vec<String>,
    pub predecessors: Vec<String>,
}

impl BasicBlock {
    pub fn new(label: &str) -> Self {
        Self {
            label: label.to_string(),
            instructions: Vec::new(),
            successors: Vec::new(),
            predecessors: Vec::new(),
        }
    }
}

/// Complete IR program
#[derive(Debug, Clone)]
pub struct IrProgram {
    /// All instructions in linear order
    pub instructions: Vec<IrInstruction>,
    /// Basic blocks for CFG analysis
    pub blocks: HashMap<String, BasicBlock>,
    /// String table for string literals
    pub string_table: Vec<String>,
    /// Entry point label
    pub entry_label: String,
    /// Variable to register mapping
    pub var_registers: HashMap<String, IrReg>,
}

impl IrProgram {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            blocks: HashMap::new(),
            string_table: Vec::new(),
            entry_label: "entry".to_string(),
            var_registers: HashMap::new(),
        }
    }
}

impl Default for IrProgram {
    fn default() -> Self {
        Self::new()
    }
}
