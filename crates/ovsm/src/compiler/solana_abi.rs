//! Solana ABI Compliance Layer
//!
//! This module provides the necessary deserialization and memory management
//! for proper Solana program entrypoint handling.

use super::ir::{IrInstruction, IrReg};
use std::collections::VecDeque;

/// Solana AccountInfo structure layout (bytes)
pub const ACCOUNT_INFO_SIZE: usize = 258; // Typical size with padding
/// Size of a Solana public key in bytes
pub const PUBKEY_SIZE: usize = 32;
/// Size of lamports field in bytes
pub const LAMPORTS_SIZE: usize = 8;
/// Size of data length field in bytes
pub const DATA_LEN_SIZE: usize = 8;
/// Size of owner pubkey in bytes
pub const OWNER_SIZE: usize = 32;

/// Generate entrypoint wrapper with proper ABI handling
pub struct EntrypointGenerator {
    instructions: VecDeque<IrInstruction>,
    next_reg: u32,
    heap_offset: u32,
}

impl Default for EntrypointGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl EntrypointGenerator {
    /// Create a new entrypoint generator for Solana ABI deserialization
    pub fn new() -> Self {
        Self {
            instructions: VecDeque::new(),
            next_reg: 10, // Start after reserved registers
            heap_offset: 0,
        }
    }

    fn alloc_reg(&mut self) -> IrReg {
        let reg = IrReg::new(self.next_reg);
        self.next_reg += 1;
        reg
    }

    fn emit(&mut self, instr: IrInstruction) {
        self.instructions.push_back(instr);
    }

    /// Generate complete entrypoint with deserialization
    pub fn generate_entrypoint(&mut self) -> Vec<IrInstruction> {
        // Label for the actual entrypoint
        self.emit(IrInstruction::Label("_solana_entrypoint".to_string()));

        // R1 contains pointer to serialized accounts array
        // R2 contains pointer to instruction data
        // These are already pre-allocated by the IR generator

        // First, deserialize the number of accounts
        // The format is: [u64 num_accounts][AccountInfo 1][AccountInfo 2]...
        let num_accounts_ptr = IrReg::new(1); // R1 already has accounts pointer
        let num_accounts = self.alloc_reg();

        // Load number of accounts (first 8 bytes)
        self.emit(IrInstruction::Load(num_accounts, num_accounts_ptr, 0));

        // Allocate space for deserialized account info array
        let accounts_array_size = self.alloc_reg();
        let account_info_size = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(
            account_info_size,
            ACCOUNT_INFO_SIZE as i64,
        ));
        self.emit(IrInstruction::Mul(
            accounts_array_size,
            num_accounts,
            account_info_size,
        ));

        let accounts_array_ptr = self.alloc_reg();
        self.emit(IrInstruction::Alloc(
            accounts_array_ptr,
            accounts_array_size,
        ));

        // Generate loop to deserialize each account
        let loop_counter = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(loop_counter, 0));

        self.emit(IrInstruction::Label(
            "deserialize_accounts_loop".to_string(),
        ));

        // Check if we've processed all accounts
        let done_check = self.alloc_reg();
        self.emit(IrInstruction::Ge(done_check, loop_counter, num_accounts));
        self.emit(IrInstruction::JumpIf(
            done_check,
            "deserialize_accounts_done".to_string(),
        ));

        // Calculate offset for current account in serialized data
        // Skip the 8-byte count + (account_index * serialized_account_size)
        let current_offset = self.alloc_reg();
        let base_offset = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(base_offset, 8)); // Skip count

        let serialized_size = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(serialized_size, 165)); // Typical serialized size

        let account_offset = self.alloc_reg();
        self.emit(IrInstruction::Mul(
            account_offset,
            loop_counter,
            serialized_size,
        ));
        self.emit(IrInstruction::Add(
            current_offset,
            base_offset,
            account_offset,
        ));

        // Deserialize AccountInfo fields
        self.deserialize_account_info(
            num_accounts_ptr,
            current_offset,
            accounts_array_ptr,
            loop_counter,
        );

        // Increment loop counter
        let one = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(one, 1));
        self.emit(IrInstruction::Add(loop_counter, loop_counter, one));

        self.emit(IrInstruction::Jump("deserialize_accounts_loop".to_string()));
        self.emit(IrInstruction::Label(
            "deserialize_accounts_done".to_string(),
        ));

        // Now deserialize instruction data
        let instruction_data_len_ptr = IrReg::new(2); // R2 has instruction data
        let instruction_data_len = self.alloc_reg();

        // First 8 bytes contain the data length
        self.emit(IrInstruction::Load(
            instruction_data_len,
            instruction_data_len_ptr,
            0,
        ));

        // Allocate buffer for instruction data
        let instruction_data_buffer = self.alloc_reg();
        self.emit(IrInstruction::Alloc(
            instruction_data_buffer,
            instruction_data_len,
        ));

        // Copy instruction data to buffer
        self.copy_memory(
            instruction_data_len_ptr,
            instruction_data_buffer,
            instruction_data_len,
            8,
        );

        // Update R1 and R2 to point to deserialized data
        self.emit(IrInstruction::Move(IrReg::new(1), accounts_array_ptr));
        self.emit(IrInstruction::Move(IrReg::new(2), instruction_data_buffer));

        // Jump to user code entry point
        self.emit(IrInstruction::Jump("entry".to_string()));

        // Convert to Vec
        self.instructions.drain(..).collect()
    }

    /// Deserialize a single AccountInfo structure
    fn deserialize_account_info(
        &mut self,
        serialized_base: IrReg,
        offset: IrReg,
        dest_array: IrReg,
        index: IrReg,
    ) {
        // Calculate destination offset in accounts array
        let dest_offset = self.alloc_reg();
        let account_size = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(
            account_size,
            ACCOUNT_INFO_SIZE as i64,
        ));
        self.emit(IrInstruction::Mul(dest_offset, index, account_size));

        // Add base + offset to get source pointer
        let src_ptr = self.alloc_reg();
        self.emit(IrInstruction::Add(src_ptr, serialized_base, offset));

        let field_offset = self.alloc_reg();
        let current_src_offset = 0i64;

        // 1. Deserialize is_duplicate (1 byte)
        self.emit(IrInstruction::ConstI64(field_offset, current_src_offset));
        let is_dup_ptr = self.alloc_reg();
        self.emit(IrInstruction::Add(is_dup_ptr, src_ptr, field_offset));
        let is_dup = self.alloc_reg();
        self.emit(IrInstruction::Load(is_dup, is_dup_ptr, 0));

        // Store in destination
        let dest_ptr = self.alloc_reg();
        self.emit(IrInstruction::Add(dest_ptr, dest_array, dest_offset));
        self.emit(IrInstruction::Store(dest_ptr, is_dup, 0));

        // 2. Deserialize pubkey (32 bytes)
        let pubkey_offset = 1;
        for i in 0..4 {
            // Copy as 4 u64 values
            let src_offset = pubkey_offset + (i * 8);
            let src_field = self.alloc_reg();
            self.emit(IrInstruction::Load(src_field, src_ptr, src_offset as i64));
            self.emit(IrInstruction::Store(
                dest_ptr,
                src_field,
                (8 + i * 8) as i64,
            ));
        }

        // 3. Deserialize is_signer (1 byte)
        let is_signer = self.alloc_reg();
        self.emit(IrInstruction::Load(is_signer, src_ptr, 33));
        self.emit(IrInstruction::Store(dest_ptr, is_signer, 40));

        // 4. Deserialize is_writable (1 byte)
        let is_writable = self.alloc_reg();
        self.emit(IrInstruction::Load(is_writable, src_ptr, 34));
        self.emit(IrInstruction::Store(dest_ptr, is_writable, 41));

        // 5. Deserialize lamports (8 bytes)
        let lamports = self.alloc_reg();
        self.emit(IrInstruction::Load(lamports, src_ptr, 35));
        self.emit(IrInstruction::Store(dest_ptr, lamports, 48));

        // 6. Deserialize data length (8 bytes)
        let data_len = self.alloc_reg();
        self.emit(IrInstruction::Load(data_len, src_ptr, 43));
        self.emit(IrInstruction::Store(dest_ptr, data_len, 56));

        // 7. Allocate and copy account data
        let data_ptr = self.alloc_reg();
        self.emit(IrInstruction::Alloc(data_ptr, data_len));

        // Copy data bytes
        let data_src = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(field_offset, 51));
        self.emit(IrInstruction::Add(data_src, src_ptr, field_offset));
        self.copy_memory(data_src, data_ptr, data_len, 0);

        // Store data pointer
        self.emit(IrInstruction::Store(dest_ptr, data_ptr, 64));

        // 8. Deserialize owner (32 bytes)
        let owner_offset = 51; // This would be dynamic based on data_len
        for i in 0..4 {
            // Copy as 4 u64 values
            let src_field = self.alloc_reg();
            // Note: In real implementation, this offset needs to be calculated based on data_len
            self.emit(IrInstruction::Load(
                src_field,
                src_ptr,
                (owner_offset + i * 8) as i64,
            ));
            self.emit(IrInstruction::Store(
                dest_ptr,
                src_field,
                (72 + i * 8) as i64,
            ));
        }

        // 9. Deserialize executable (1 byte)
        let executable = self.alloc_reg();
        // Note: Offset needs to be calculated
        self.emit(IrInstruction::Load(executable, src_ptr, 83));
        self.emit(IrInstruction::Store(dest_ptr, executable, 104));

        // 10. Deserialize rent_epoch (8 bytes)
        let rent_epoch = self.alloc_reg();
        self.emit(IrInstruction::Load(rent_epoch, src_ptr, 84));
        self.emit(IrInstruction::Store(dest_ptr, rent_epoch, 112));
    }

    /// Generate memory copy loop
    fn copy_memory(&mut self, src: IrReg, dest: IrReg, len: IrReg, src_offset: i64) {
        let loop_counter = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(loop_counter, 0));

        let loop_label = format!("copy_loop_{}", self.next_reg);
        let done_label = format!("copy_done_{}", self.next_reg);

        self.emit(IrInstruction::Label(loop_label.clone()));

        // Check if done
        let done = self.alloc_reg();
        self.emit(IrInstruction::Ge(done, loop_counter, len));
        self.emit(IrInstruction::JumpIf(done, done_label.clone()));

        // Load byte from source
        let src_ptr = self.alloc_reg();
        let offset = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(offset, src_offset));
        let total_offset = self.alloc_reg();
        self.emit(IrInstruction::Add(total_offset, loop_counter, offset));
        self.emit(IrInstruction::Add(src_ptr, src, total_offset));

        let byte_val = self.alloc_reg();
        self.emit(IrInstruction::Load(byte_val, src_ptr, 0));

        // Store byte to destination
        let dest_ptr = self.alloc_reg();
        self.emit(IrInstruction::Add(dest_ptr, dest, loop_counter));
        self.emit(IrInstruction::Store(dest_ptr, byte_val, 0));

        // Increment counter
        let one = self.alloc_reg();
        self.emit(IrInstruction::ConstI64(one, 1));
        self.emit(IrInstruction::Add(loop_counter, loop_counter, one));

        self.emit(IrInstruction::Jump(loop_label));
        self.emit(IrInstruction::Label(done_label));
    }
}

/// Inject entrypoint wrapper into IR program
pub fn inject_entrypoint_wrapper(instructions: &mut Vec<IrInstruction>) {
    let mut gen = EntrypointGenerator::new();
    let wrapper = gen.generate_entrypoint();

    // Insert wrapper at the beginning
    for instr in wrapper.into_iter().rev() {
        instructions.insert(0, instr);
    }
}
