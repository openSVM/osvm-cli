//! # Control Flow Graph Recovery
//!
//! Recovers the control flow graph from disassembled sBPF instructions.

use super::disassembler::DisassembledInstr;
use std::collections::{HashMap, HashSet};

/// A basic block in the control flow graph
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// Unique block ID
    pub id: usize,
    /// Starting instruction offset
    pub start_offset: usize,
    /// Ending instruction offset
    pub end_offset: usize,
    /// Instruction indices in this block
    pub instructions: Vec<usize>,
    /// Successor block IDs
    pub successors: Vec<usize>,
    /// Predecessor block IDs
    pub predecessors: Vec<usize>,
    /// Block label (if any)
    pub label: Option<String>,
}

impl BasicBlock {
    fn new(id: usize, start_offset: usize) -> Self {
        Self {
            id,
            start_offset,
            end_offset: start_offset,
            instructions: Vec::new(),
            successors: Vec::new(),
            predecessors: Vec::new(),
            label: None,
        }
    }
}

/// Control Flow Graph
#[derive(Debug, Clone, Default)]
pub struct ControlFlowGraph {
    /// Basic blocks indexed by ID
    pub blocks: HashMap<usize, BasicBlock>,
    /// Entry block ID
    pub entry: usize,
    /// Exit block IDs
    pub exits: Vec<usize>,
    /// Map from instruction offset to block ID
    pub offset_to_block: HashMap<usize, usize>,
}

impl ControlFlowGraph {
    /// Build CFG from disassembled instructions
    pub fn build(instructions: &[DisassembledInstr]) -> Self {
        let mut cfg = ControlFlowGraph {
            blocks: HashMap::new(),
            entry: 0,
            exits: Vec::new(),
            offset_to_block: HashMap::new(),
        };

        if instructions.is_empty() {
            return cfg;
        }

        // Step 1: Find block leaders (first instruction of each block)
        let mut leaders: HashSet<usize> = HashSet::new();
        leaders.insert(0); // First instruction is always a leader

        for (i, instr) in instructions.iter().enumerate() {
            if instr.is_jump() {
                // Target of jump is a leader
                if let Some(target_off) = instr.jump_target() {
                    let target_idx = (i as i64 + 1 + target_off) as usize;
                    if target_idx < instructions.len() {
                        leaders.insert(target_idx);
                    }
                }
                // Instruction after jump is a leader
                if i + 1 < instructions.len() {
                    leaders.insert(i + 1);
                }
            }
            if instr.is_call() {
                // Instruction after call is a leader (for fall-through)
                if i + 1 < instructions.len() {
                    leaders.insert(i + 1);
                }
            }
        }

        // Step 2: Create basic blocks
        let mut sorted_leaders: Vec<usize> = leaders.into_iter().collect();
        sorted_leaders.sort();

        let mut leader_to_block: HashMap<usize, usize> = HashMap::new();

        for (block_id, &leader_idx) in sorted_leaders.iter().enumerate() {
            let mut block = BasicBlock::new(block_id, instructions[leader_idx].offset);
            block.label = Some(format!("block_{}", block_id));
            leader_to_block.insert(leader_idx, block_id);
            cfg.blocks.insert(block_id, block);
        }

        // Step 3: Assign instructions to blocks
        let mut current_block_id = 0;

        for (i, instr) in instructions.iter().enumerate() {
            if let Some(&block_id) = leader_to_block.get(&i) {
                current_block_id = block_id;
            }

            if let Some(block) = cfg.blocks.get_mut(&current_block_id) {
                block.instructions.push(i);
                block.end_offset = instr.offset + 8;
                cfg.offset_to_block.insert(instr.offset, current_block_id);
            }
        }

        // Step 4: Build edges
        for (i, instr) in instructions.iter().enumerate() {
            let Some(&src_block) = cfg.offset_to_block.get(&instr.offset) else {
                continue;
            };

            if instr.is_exit() {
                cfg.exits.push(src_block);
                continue;
            }

            if instr.is_jump() {
                // Add edge to jump target
                if let Some(target_off) = instr.jump_target() {
                    let target_idx = (i as i64 + 1 + target_off) as usize;
                    if target_idx < instructions.len() {
                        if let Some(&dst_block) = leader_to_block.get(&target_idx) {
                            cfg.add_edge(src_block, dst_block);
                        }
                    }
                }

                // Add fall-through edge for conditional jumps
                if instr.opcode != 0x05 {
                    // Not unconditional
                    if i + 1 < instructions.len() {
                        if let Some(&dst_block) = leader_to_block.get(&(i + 1)) {
                            cfg.add_edge(src_block, dst_block);
                        }
                    }
                }
            } else if i + 1 < instructions.len() {
                // Fall-through to next instruction
                if let Some(&next_block) = leader_to_block.get(&(i + 1)) {
                    // Only add if this is the last instruction in current block
                    let is_last_in_block = cfg
                        .blocks
                        .get(&src_block)
                        .map(|b| b.instructions.last() == Some(&i))
                        .unwrap_or(false);

                    if is_last_in_block && next_block != src_block {
                        cfg.add_edge(src_block, next_block);
                    }
                }
            }
        }

        cfg
    }

    /// Add an edge between blocks
    fn add_edge(&mut self, from: usize, to: usize) {
        if let Some(block) = self.blocks.get_mut(&from) {
            if !block.successors.contains(&to) {
                block.successors.push(to);
            }
        }
        if let Some(block) = self.blocks.get_mut(&to) {
            if !block.predecessors.contains(&from) {
                block.predecessors.push(from);
            }
        }
    }

    /// Get block by ID
    pub fn get_block(&self, id: usize) -> Option<&BasicBlock> {
        self.blocks.get(&id)
    }

    /// Iterate blocks in topological order
    pub fn blocks_topo_order(&self) -> Vec<usize> {
        let mut visited: HashSet<usize> = HashSet::new();
        let mut order: Vec<usize> = Vec::new();

        fn dfs(
            cfg: &ControlFlowGraph,
            block_id: usize,
            visited: &mut HashSet<usize>,
            order: &mut Vec<usize>,
        ) {
            if visited.contains(&block_id) {
                return;
            }
            visited.insert(block_id);

            if let Some(block) = cfg.blocks.get(&block_id) {
                for &succ in &block.successors {
                    dfs(cfg, succ, visited, order);
                }
            }

            order.push(block_id);
        }

        dfs(self, self.entry, &mut visited, &mut order);
        order.reverse();
        order
    }

    /// Check if this is a loop header
    pub fn is_loop_header(&self, block_id: usize) -> bool {
        if let Some(block) = self.blocks.get(&block_id) {
            // A block is a loop header if any predecessor has a higher block ID
            // (back edge from later block)
            block.predecessors.iter().any(|&pred| pred > block_id)
        } else {
            false
        }
    }

    /// Get the loop body for a header
    pub fn get_loop_body(&self, header_id: usize) -> Vec<usize> {
        let mut body = Vec::new();
        let mut visited: HashSet<usize> = HashSet::new();
        let mut stack = vec![header_id];

        while let Some(block_id) = stack.pop() {
            if visited.contains(&block_id) {
                continue;
            }
            visited.insert(block_id);
            body.push(block_id);

            if let Some(block) = self.blocks.get(&block_id) {
                for &succ in &block.successors {
                    if succ <= block_id || succ == header_id {
                        // Stay within loop
                        stack.push(succ);
                    }
                }
            }
        }

        body
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::decompiler::disassembler::Disassembler;

    #[test]
    fn test_empty_cfg() {
        let cfg = ControlFlowGraph::build(&[]);
        assert!(cfg.blocks.is_empty());
    }

    #[test]
    fn test_linear_cfg() {
        let disasm = Disassembler::new();

        // Two instructions: mov64 r0, 42; exit
        let bytes = [
            0xb7, 0x00, 0x00, 0x00, 0x2a, 0x00, 0x00, 0x00, // mov64 r0, 42
            0x95, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // exit
        ];

        // Create mock instructions directly
        let instructions = vec![
            DisassembledInstr {
                offset: 0,
                opcode: 0xb7,
                dst: 0,
                src: 0,
                off: 0,
                imm: 42,
                mnemonic: "mov64".into(),
                operands: "r0, 42".into(),
            },
            DisassembledInstr {
                offset: 8,
                opcode: 0x95,
                dst: 0,
                src: 0,
                off: 0,
                imm: 0,
                mnemonic: "exit".into(),
                operands: String::new(),
            },
        ];

        let cfg = ControlFlowGraph::build(&instructions);

        // Should have 1 block
        assert_eq!(cfg.blocks.len(), 1);
        assert_eq!(cfg.exits.len(), 1);
    }
}
