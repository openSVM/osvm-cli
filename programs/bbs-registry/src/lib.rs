//! BBS Registry - On-chain peer discovery for federated BBS nodes
//!
//! This program allows BBS nodes to register their presence on-chain,
//! enabling trustless, decentralized peer discovery.
//!
//! Account Structure:
//! - Each node gets a PDA seeded by ["bbs_node", owner_pubkey]
//! - Fixed-size account data for easy parsing and iteration
//!
//! Instructions:
//! - RegisterNode: Create a new node registration
//! - UpdateNode: Update address/name of existing registration
//! - Heartbeat: Update last_heartbeat timestamp
//! - Deregister: Mark node as inactive (closes account, returns rent)

use borsh::{BorshDeserialize, BorshSerialize};
use solana_program::{
    account_info::{next_account_info, AccountInfo},
    clock::Clock,
    entrypoint,
    entrypoint::ProgramResult,
    msg,
    program::invoke_signed,
    program_error::ProgramError,
    pubkey::Pubkey,
    rent::Rent,
    system_instruction,
    sysvar::Sysvar,
};

// BBS Registry Program ID (deployed on devnet)
solana_program::declare_id!("B6XtpamL7mSefkiRDQb84wT7wBWGLbULjqo5BZZanQyL");

// ============================================
// Constants
// ============================================

/// Discriminator for node registration accounts
pub const NODE_DISCRIMINATOR: [u8; 8] = *b"BBSNODE\0";

/// Maximum length for HTTP address
pub const MAX_ADDRESS_LEN: usize = 128;

/// Maximum length for node name
pub const MAX_NAME_LEN: usize = 32;

/// Seed prefix for node PDAs
pub const NODE_SEED: &[u8] = b"bbs_node";

/// Size of NodeRegistration account data
pub const NODE_ACCOUNT_SIZE: usize = 8 + 8 + MAX_ADDRESS_LEN + MAX_NAME_LEN + 32 + 8 + 8 + 1;
// discriminator(8) + node_id(8) + address(128) + name(32) + owner(32) + registered_at(8) + last_heartbeat(8) + is_active(1) = 225

// ============================================
// Account State
// ============================================

/// On-chain registration data for a BBS node
#[derive(BorshSerialize, BorshDeserialize, Debug, Clone)]
pub struct NodeRegistration {
    /// Account discriminator: "BBSNODE\0"
    pub discriminator: [u8; 8],

    /// Node ID bytes (the !xxxxxxxx identifier as raw bytes)
    /// Format: first 4 bytes = CRC32 hash, last 4 bytes reserved
    pub node_id: [u8; 8],

    /// HTTP address where the BBS server is reachable
    /// Null-padded to MAX_ADDRESS_LEN
    pub address: [u8; MAX_ADDRESS_LEN],

    /// Human-readable node name
    /// Null-padded to MAX_NAME_LEN
    pub name: [u8; MAX_NAME_LEN],

    /// Owner pubkey - only this key can modify/deregister
    pub owner: Pubkey,

    /// Unix timestamp when node was first registered
    pub registered_at: i64,

    /// Unix timestamp of last heartbeat/activity
    pub last_heartbeat: i64,

    /// Whether the node is currently active
    pub is_active: bool,
}

impl NodeRegistration {
    /// Create a new registration
    pub fn new(node_id: [u8; 8], address: &str, name: &str, owner: Pubkey, now: i64) -> Self {
        let mut addr_bytes = [0u8; MAX_ADDRESS_LEN];
        let addr_slice = address.as_bytes();
        let addr_len = addr_slice.len().min(MAX_ADDRESS_LEN);
        addr_bytes[..addr_len].copy_from_slice(&addr_slice[..addr_len]);

        let mut name_bytes = [0u8; MAX_NAME_LEN];
        let name_slice = name.as_bytes();
        let name_len = name_slice.len().min(MAX_NAME_LEN);
        name_bytes[..name_len].copy_from_slice(&name_slice[..name_len]);

        Self {
            discriminator: NODE_DISCRIMINATOR,
            node_id,
            address: addr_bytes,
            name: name_bytes,
            owner,
            registered_at: now,
            last_heartbeat: now,
            is_active: true,
        }
    }

    /// Get address as string (trimmed)
    pub fn get_address(&self) -> String {
        let end = self.address.iter().position(|&b| b == 0).unwrap_or(MAX_ADDRESS_LEN);
        String::from_utf8_lossy(&self.address[..end]).to_string()
    }

    /// Get name as string (trimmed)
    pub fn get_name(&self) -> String {
        let end = self.name.iter().position(|&b| b == 0).unwrap_or(MAX_NAME_LEN);
        String::from_utf8_lossy(&self.name[..end]).to_string()
    }

    /// Get node_id as hex string with ! prefix
    pub fn get_node_id_string(&self) -> String {
        format!("!{:02x}{:02x}{:02x}{:02x}",
            self.node_id[0], self.node_id[1],
            self.node_id[2], self.node_id[3])
    }

    /// Update address
    pub fn set_address(&mut self, address: &str) {
        self.address = [0u8; MAX_ADDRESS_LEN];
        let addr_slice = address.as_bytes();
        let addr_len = addr_slice.len().min(MAX_ADDRESS_LEN);
        self.address[..addr_len].copy_from_slice(&addr_slice[..addr_len]);
    }

    /// Update name
    pub fn set_name(&mut self, name: &str) {
        self.name = [0u8; MAX_NAME_LEN];
        let name_slice = name.as_bytes();
        let name_len = name_slice.len().min(MAX_NAME_LEN);
        self.name[..name_len].copy_from_slice(&name_slice[..name_len]);
    }
}

// ============================================
// Instructions
// ============================================

/// Program instructions
#[derive(BorshSerialize, BorshDeserialize, Debug)]
pub enum RegistryInstruction {
    /// Register a new BBS node
    ///
    /// Accounts:
    /// 0. `[writable, signer]` Payer/Owner
    /// 1. `[writable]` Node PDA account (to be created)
    /// 2. `[]` System program
    ///
    /// Data:
    /// - node_id: [u8; 8] - The node identifier
    /// - address: String - HTTP address (max 128 chars)
    /// - name: String - Display name (max 32 chars)
    RegisterNode {
        node_id: [u8; 8],
        address: String,
        name: String,
    },

    /// Update an existing registration
    ///
    /// Accounts:
    /// 0. `[signer]` Owner
    /// 1. `[writable]` Node PDA account
    ///
    /// Data:
    /// - address: Option<String> - New address (if Some)
    /// - name: Option<String> - New name (if Some)
    UpdateNode {
        address: Option<String>,
        name: Option<String>,
    },

    /// Update heartbeat timestamp
    ///
    /// Accounts:
    /// 0. `[signer]` Owner
    /// 1. `[writable]` Node PDA account
    Heartbeat,

    /// Deregister and close account (returns rent)
    ///
    /// Accounts:
    /// 0. `[signer]` Owner
    /// 1. `[writable]` Node PDA account
    /// 2. `[writable]` Rent destination (usually owner)
    Deregister,
}

// ============================================
// Program Entrypoint
// ============================================

entrypoint!(process_instruction);

pub fn process_instruction(
    program_id: &Pubkey,
    accounts: &[AccountInfo],
    instruction_data: &[u8],
) -> ProgramResult {
    let instruction = RegistryInstruction::try_from_slice(instruction_data)
        .map_err(|_| ProgramError::InvalidInstructionData)?;

    match instruction {
        RegistryInstruction::RegisterNode { node_id, address, name } => {
            process_register_node(program_id, accounts, node_id, &address, &name)
        }
        RegistryInstruction::UpdateNode { address, name } => {
            process_update_node(accounts, address.as_deref(), name.as_deref())
        }
        RegistryInstruction::Heartbeat => {
            process_heartbeat(accounts)
        }
        RegistryInstruction::Deregister => {
            process_deregister(accounts)
        }
    }
}

// ============================================
// Instruction Processors
// ============================================

/// Derive the PDA for a node registration
pub fn get_node_pda(owner: &Pubkey, program_id: &Pubkey) -> (Pubkey, u8) {
    Pubkey::find_program_address(&[NODE_SEED, owner.as_ref()], program_id)
}

fn process_register_node(
    program_id: &Pubkey,
    accounts: &[AccountInfo],
    node_id: [u8; 8],
    address: &str,
    name: &str,
) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let payer = next_account_info(account_iter)?;
    let node_account = next_account_info(account_iter)?;
    let system_program = next_account_info(account_iter)?;

    // Verify payer is signer
    if !payer.is_signer {
        msg!("Error: Payer must be a signer");
        return Err(ProgramError::MissingRequiredSignature);
    }

    // Derive and verify PDA
    let (expected_pda, bump) = get_node_pda(payer.key, program_id);
    if *node_account.key != expected_pda {
        msg!("Error: Invalid node account PDA");
        return Err(ProgramError::InvalidSeeds);
    }

    // Check if account already exists
    if !node_account.data_is_empty() {
        msg!("Error: Node already registered");
        return Err(ProgramError::AccountAlreadyInitialized);
    }

    // Validate input lengths
    if address.len() > MAX_ADDRESS_LEN {
        msg!("Error: Address too long (max {} chars)", MAX_ADDRESS_LEN);
        return Err(ProgramError::InvalidArgument);
    }
    if name.len() > MAX_NAME_LEN {
        msg!("Error: Name too long (max {} chars)", MAX_NAME_LEN);
        return Err(ProgramError::InvalidArgument);
    }

    // Get current timestamp
    let clock = Clock::get()?;
    let now = clock.unix_timestamp;

    // Calculate rent
    let rent = Rent::get()?;
    let lamports = rent.minimum_balance(NODE_ACCOUNT_SIZE);

    // Create the PDA account
    let signer_seeds: &[&[u8]] = &[NODE_SEED, payer.key.as_ref(), &[bump]];

    invoke_signed(
        &system_instruction::create_account(
            payer.key,
            node_account.key,
            lamports,
            NODE_ACCOUNT_SIZE as u64,
            program_id,
        ),
        &[payer.clone(), node_account.clone(), system_program.clone()],
        &[signer_seeds],
    )?;

    // Initialize account data
    let registration = NodeRegistration::new(node_id, address, name, *payer.key, now);
    registration.serialize(&mut *node_account.data.borrow_mut())?;

    msg!("BBS Node registered: {} at {}", registration.get_node_id_string(), address);

    Ok(())
}

fn process_update_node(
    accounts: &[AccountInfo],
    address: Option<&str>,
    name: Option<&str>,
) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let owner = next_account_info(account_iter)?;
    let node_account = next_account_info(account_iter)?;

    // Verify owner is signer
    if !owner.is_signer {
        msg!("Error: Owner must be a signer");
        return Err(ProgramError::MissingRequiredSignature);
    }

    // Deserialize account data
    let mut registration = NodeRegistration::try_from_slice(&node_account.data.borrow())?;

    // Verify discriminator
    if registration.discriminator != NODE_DISCRIMINATOR {
        msg!("Error: Invalid account discriminator");
        return Err(ProgramError::InvalidAccountData);
    }

    // Verify ownership
    if registration.owner != *owner.key {
        msg!("Error: Not the owner of this registration");
        return Err(ProgramError::IllegalOwner);
    }

    // Update fields
    if let Some(addr) = address {
        if addr.len() > MAX_ADDRESS_LEN {
            return Err(ProgramError::InvalidArgument);
        }
        registration.set_address(addr);
        msg!("Updated address to: {}", addr);
    }

    if let Some(n) = name {
        if n.len() > MAX_NAME_LEN {
            return Err(ProgramError::InvalidArgument);
        }
        registration.set_name(n);
        msg!("Updated name to: {}", n);
    }

    // Update heartbeat too
    let clock = Clock::get()?;
    registration.last_heartbeat = clock.unix_timestamp;

    // Write back
    registration.serialize(&mut *node_account.data.borrow_mut())?;

    Ok(())
}

fn process_heartbeat(accounts: &[AccountInfo]) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let owner = next_account_info(account_iter)?;
    let node_account = next_account_info(account_iter)?;

    // Verify owner is signer
    if !owner.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    // Deserialize account data
    let mut registration = NodeRegistration::try_from_slice(&node_account.data.borrow())?;

    // Verify discriminator and ownership
    if registration.discriminator != NODE_DISCRIMINATOR {
        return Err(ProgramError::InvalidAccountData);
    }
    if registration.owner != *owner.key {
        return Err(ProgramError::IllegalOwner);
    }

    // Update heartbeat
    let clock = Clock::get()?;
    registration.last_heartbeat = clock.unix_timestamp;

    // Write back
    registration.serialize(&mut *node_account.data.borrow_mut())?;

    msg!("Heartbeat updated for {}", registration.get_node_id_string());

    Ok(())
}

fn process_deregister(accounts: &[AccountInfo]) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let owner = next_account_info(account_iter)?;
    let node_account = next_account_info(account_iter)?;
    let destination = next_account_info(account_iter)?;

    // Verify owner is signer
    if !owner.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    // Deserialize account data
    let registration = NodeRegistration::try_from_slice(&node_account.data.borrow())?;

    // Verify discriminator and ownership
    if registration.discriminator != NODE_DISCRIMINATOR {
        return Err(ProgramError::InvalidAccountData);
    }
    if registration.owner != *owner.key {
        return Err(ProgramError::IllegalOwner);
    }

    msg!("Deregistering node: {}", registration.get_node_id_string());

    // Transfer lamports to destination and zero out account
    let lamports = node_account.lamports();
    **node_account.try_borrow_mut_lamports()? = 0;
    **destination.try_borrow_mut_lamports()? = destination
        .lamports()
        .checked_add(lamports)
        .ok_or(ProgramError::ArithmeticOverflow)?;

    // Zero out data
    let mut data = node_account.data.borrow_mut();
    data.fill(0);

    Ok(())
}

// ============================================
// Client Helpers (for use in osvm-cli)
// ============================================

#[cfg(not(target_os = "solana"))]
pub mod client {
    use super::*;
    use solana_program::instruction::{AccountMeta, Instruction};

    /// Create instruction to register a new node
    pub fn register_node(
        program_id: &Pubkey,
        payer: &Pubkey,
        node_id: [u8; 8],
        address: String,
        name: String,
    ) -> Instruction {
        let (node_pda, _) = get_node_pda(payer, program_id);

        let data = RegistryInstruction::RegisterNode { node_id, address, name };

        Instruction {
            program_id: *program_id,
            accounts: vec![
                AccountMeta::new(*payer, true),
                AccountMeta::new(node_pda, false),
                AccountMeta::new_readonly(solana_program::system_program::ID, false),
            ],
            data: borsh::to_vec(&data).unwrap(),
        }
    }

    /// Create instruction to update node registration
    pub fn update_node(
        program_id: &Pubkey,
        owner: &Pubkey,
        address: Option<String>,
        name: Option<String>,
    ) -> Instruction {
        let (node_pda, _) = get_node_pda(owner, program_id);

        let data = RegistryInstruction::UpdateNode { address, name };

        Instruction {
            program_id: *program_id,
            accounts: vec![
                AccountMeta::new_readonly(*owner, true),
                AccountMeta::new(node_pda, false),
            ],
            data: borsh::to_vec(&data).unwrap(),
        }
    }

    /// Create instruction to send heartbeat
    pub fn heartbeat(program_id: &Pubkey, owner: &Pubkey) -> Instruction {
        let (node_pda, _) = get_node_pda(owner, program_id);

        let data = RegistryInstruction::Heartbeat;

        Instruction {
            program_id: *program_id,
            accounts: vec![
                AccountMeta::new_readonly(*owner, true),
                AccountMeta::new(node_pda, false),
            ],
            data: borsh::to_vec(&data).unwrap(),
        }
    }

    /// Create instruction to deregister node
    pub fn deregister(
        program_id: &Pubkey,
        owner: &Pubkey,
        rent_destination: &Pubkey,
    ) -> Instruction {
        let (node_pda, _) = get_node_pda(owner, program_id);

        let data = RegistryInstruction::Deregister;

        Instruction {
            program_id: *program_id,
            accounts: vec![
                AccountMeta::new_readonly(*owner, true),
                AccountMeta::new(node_pda, false),
                AccountMeta::new(*rent_destination, false),
            ],
            data: borsh::to_vec(&data).unwrap(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_node_registration_serialization() {
        let node_id = [0x12, 0x34, 0x56, 0x78, 0, 0, 0, 0];
        let owner = Pubkey::new_unique();
        let reg = NodeRegistration::new(
            node_id,
            "http://192.168.1.100:8080",
            "Test Node",
            owner,
            1700000000,
        );

        assert_eq!(reg.discriminator, NODE_DISCRIMINATOR);
        assert_eq!(reg.get_node_id_string(), "!12345678");
        assert_eq!(reg.get_address(), "http://192.168.1.100:8080");
        assert_eq!(reg.get_name(), "Test Node");
        assert!(reg.is_active);
    }

    #[test]
    fn test_account_size() {
        // Ensure our size constant matches actual serialized size
        let node_id = [0u8; 8];
        let owner = Pubkey::new_unique();
        let reg = NodeRegistration::new(node_id, "", "", owner, 0);

        let serialized = borsh::to_vec(&reg).unwrap();
        assert_eq!(serialized.len(), NODE_ACCOUNT_SIZE);
    }
}
