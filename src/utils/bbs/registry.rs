//! BBS Registry Client - On-chain peer discovery via Solana
//!
//! This module provides client functionality to interact with the BBS Registry
//! program deployed on Solana devnet for decentralized peer discovery.

use anyhow::{anyhow, Result};
use borsh::{BorshDeserialize, BorshSerialize};
use solana_client::rpc_client::RpcClient;
use solana_commitment_config::CommitmentConfig;
use solana_sdk::{
    instruction::{AccountMeta, Instruction},
    pubkey::Pubkey,
    signature::{read_keypair_file, Keypair, Signer},
    transaction::Transaction,
};
use std::str::FromStr;

/// System program ID constant
const SYSTEM_PROGRAM_ID: Pubkey = solana_sdk::pubkey!("11111111111111111111111111111111");

/// BBS Registry Program ID (deployed on devnet)
pub const PROGRAM_ID: &str = "B6XtpamL7mSefkiRDQb84wT7wBWGLbULjqo5BZZanQyL";

/// Default devnet RPC URL
pub const DEVNET_RPC: &str = "https://api.devnet.solana.com";

/// Seed prefix for node PDAs
pub const NODE_SEED: &[u8] = b"bbs_node";

/// Account discriminator
pub const NODE_DISCRIMINATOR: [u8; 8] = *b"BBSNODE\0";

/// Maximum address length
pub const MAX_ADDRESS_LEN: usize = 128;

/// Maximum name length
pub const MAX_NAME_LEN: usize = 32;

/// Node account size
pub const NODE_ACCOUNT_SIZE: usize = 8 + 8 + MAX_ADDRESS_LEN + MAX_NAME_LEN + 32 + 8 + 8 + 1;

// ============================================
// Instructions (must match program)
// ============================================

#[derive(BorshSerialize, BorshDeserialize, Debug)]
pub enum RegistryInstruction {
    RegisterNode {
        node_id: [u8; 8],
        address: String,
        name: String,
    },
    UpdateNode {
        address: Option<String>,
        name: Option<String>,
    },
    Heartbeat,
    Deregister,
}

// ============================================
// Account Data (must match program)
// ============================================

#[derive(BorshDeserialize, Debug, Clone)]
pub struct NodeRegistration {
    pub discriminator: [u8; 8],
    pub node_id: [u8; 8],
    pub address: [u8; MAX_ADDRESS_LEN],
    pub name: [u8; MAX_NAME_LEN],
    pub owner: Pubkey,
    pub registered_at: i64,
    pub last_heartbeat: i64,
    pub is_active: bool,
}

impl NodeRegistration {
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
}

// ============================================
// Client
// ============================================

pub struct RegistryClient {
    rpc_client: RpcClient,
    program_id: Pubkey,
}

impl RegistryClient {
    /// Create a new registry client
    pub fn new(rpc_url: Option<&str>) -> Result<Self> {
        let url = rpc_url.unwrap_or(DEVNET_RPC);
        let rpc_client = RpcClient::new_with_commitment(
            url.to_string(),
            CommitmentConfig::confirmed(),
        );
        let program_id = Pubkey::from_str(PROGRAM_ID)
            .map_err(|e| anyhow!("Invalid program ID: {}", e))?;

        Ok(Self { rpc_client, program_id })
    }

    /// Get the PDA for a node registration
    pub fn get_node_pda(&self, owner: &Pubkey) -> (Pubkey, u8) {
        Pubkey::find_program_address(&[NODE_SEED, owner.as_ref()], &self.program_id)
    }

    /// Register a new node
    pub fn register(
        &self,
        payer: &Keypair,
        node_id: [u8; 8],
        address: &str,
        name: &str,
    ) -> Result<String> {
        let (node_pda, _) = self.get_node_pda(&payer.pubkey());

        let instruction = RegistryInstruction::RegisterNode {
            node_id,
            address: address.to_string(),
            name: name.to_string(),
        };

        let ix = Instruction {
            program_id: self.program_id,
            accounts: vec![
                AccountMeta::new(payer.pubkey(), true),
                AccountMeta::new(node_pda, false),
                AccountMeta::new_readonly(SYSTEM_PROGRAM_ID, false),
            ],
            data: borsh::to_vec(&instruction)?,
        };

        let recent_blockhash = self.rpc_client.get_latest_blockhash()?;
        let tx = Transaction::new_signed_with_payer(
            &[ix],
            Some(&payer.pubkey()),
            &[payer],
            recent_blockhash,
        );

        let sig = self.rpc_client.send_and_confirm_transaction(&tx)?;
        Ok(sig.to_string())
    }

    /// Update node registration
    pub fn update(
        &self,
        owner: &Keypair,
        address: Option<&str>,
        name: Option<&str>,
    ) -> Result<String> {
        let (node_pda, _) = self.get_node_pda(&owner.pubkey());

        let instruction = RegistryInstruction::UpdateNode {
            address: address.map(|s| s.to_string()),
            name: name.map(|s| s.to_string()),
        };

        let ix = Instruction {
            program_id: self.program_id,
            accounts: vec![
                AccountMeta::new_readonly(owner.pubkey(), true),
                AccountMeta::new(node_pda, false),
            ],
            data: borsh::to_vec(&instruction)?,
        };

        let recent_blockhash = self.rpc_client.get_latest_blockhash()?;
        let tx = Transaction::new_signed_with_payer(
            &[ix],
            Some(&owner.pubkey()),
            &[owner],
            recent_blockhash,
        );

        let sig = self.rpc_client.send_and_confirm_transaction(&tx)?;
        Ok(sig.to_string())
    }

    /// Send heartbeat to update last_heartbeat timestamp
    pub fn heartbeat(&self, owner: &Keypair) -> Result<String> {
        let (node_pda, _) = self.get_node_pda(&owner.pubkey());

        let instruction = RegistryInstruction::Heartbeat;

        let ix = Instruction {
            program_id: self.program_id,
            accounts: vec![
                AccountMeta::new_readonly(owner.pubkey(), true),
                AccountMeta::new(node_pda, false),
            ],
            data: borsh::to_vec(&instruction)?,
        };

        let recent_blockhash = self.rpc_client.get_latest_blockhash()?;
        let tx = Transaction::new_signed_with_payer(
            &[ix],
            Some(&owner.pubkey()),
            &[owner],
            recent_blockhash,
        );

        let sig = self.rpc_client.send_and_confirm_transaction(&tx)?;
        Ok(sig.to_string())
    }

    /// Deregister node and close account (returns rent)
    pub fn deregister(
        &self,
        owner: &Keypair,
        rent_destination: Option<&Pubkey>,
    ) -> Result<String> {
        let (node_pda, _) = self.get_node_pda(&owner.pubkey());
        let owner_pubkey = owner.pubkey();
        let destination = rent_destination.unwrap_or(&owner_pubkey);

        let instruction = RegistryInstruction::Deregister;

        let ix = Instruction {
            program_id: self.program_id,
            accounts: vec![
                AccountMeta::new_readonly(owner.pubkey(), true),
                AccountMeta::new(node_pda, false),
                AccountMeta::new(*destination, false),
            ],
            data: borsh::to_vec(&instruction)?,
        };

        let recent_blockhash = self.rpc_client.get_latest_blockhash()?;
        let tx = Transaction::new_signed_with_payer(
            &[ix],
            Some(&owner.pubkey()),
            &[owner],
            recent_blockhash,
        );

        let sig = self.rpc_client.send_and_confirm_transaction(&tx)?;
        Ok(sig.to_string())
    }

    /// Get a single node registration by owner
    pub fn get_node(&self, owner: &Pubkey) -> Result<Option<NodeRegistration>> {
        let (node_pda, _) = self.get_node_pda(owner);

        match self.rpc_client.get_account(&node_pda) {
            Ok(account) => {
                let registration = NodeRegistration::try_from_slice(&account.data)?;
                if registration.discriminator == NODE_DISCRIMINATOR && registration.is_active {
                    Ok(Some(registration))
                } else {
                    Ok(None)
                }
            }
            Err(_) => Ok(None),
        }
    }

    /// List all registered nodes
    pub fn list_nodes(&self) -> Result<Vec<(Pubkey, NodeRegistration)>> {
        let accounts = self.rpc_client.get_program_accounts(&self.program_id)?;

        let mut nodes = Vec::new();
        for (pubkey, account) in accounts {
            if account.data.len() >= NODE_ACCOUNT_SIZE {
                if let Ok(registration) = NodeRegistration::try_from_slice(&account.data) {
                    if registration.discriminator == NODE_DISCRIMINATOR && registration.is_active {
                        nodes.push((pubkey, registration));
                    }
                }
            }
        }

        // Sort by last_heartbeat descending (most recent first)
        nodes.sort_by(|a, b| b.1.last_heartbeat.cmp(&a.1.last_heartbeat));

        Ok(nodes)
    }
}

// ============================================
// Helper functions
// ============================================

/// Generate node_id bytes from a string address using CRC32
pub fn generate_node_id(address: &str) -> [u8; 8] {
    // Simple hash using std - avoid extra dependency
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    address.hash(&mut hasher);
    let hash = hasher.finish();

    let mut node_id = [0u8; 8];
    node_id.copy_from_slice(&hash.to_be_bytes());
    node_id
}

/// Load keypair from file
pub fn load_keypair(path: &str) -> Result<Keypair> {
    read_keypair_file(path)
        .map_err(|e| anyhow!("Failed to read keypair file '{}': {}", path, e))
}

/// Format timestamp for display
pub fn format_timestamp(ts: i64) -> String {
    use chrono::{Local, TimeZone};
    match Local.timestamp_opt(ts, 0) {
        chrono::LocalResult::Single(dt) => dt.format("%Y-%m-%d %H:%M:%S").to_string(),
        _ => "Invalid".to_string(),
    }
}

/// Expand ~ in path
pub fn expand_path(path: &str) -> String {
    if path.starts_with("~/") {
        if let Some(home) = dirs::home_dir() {
            return home.join(&path[2..]).to_string_lossy().to_string();
        }
    }
    path.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_node_id() {
        let node_id = generate_node_id("http://192.168.1.100:8080");
        // Should generate consistent hash-based ID
        assert_ne!(node_id, [0u8; 8]);

        // Same input should give same output
        let node_id2 = generate_node_id("http://192.168.1.100:8080");
        assert_eq!(node_id, node_id2);

        // Different input should give different output
        let node_id3 = generate_node_id("http://192.168.1.101:8080");
        assert_ne!(node_id, node_id3);
    }

    #[test]
    fn test_node_registration_parsing() {
        // Test that we can parse what we serialize
        let mut data = vec![0u8; NODE_ACCOUNT_SIZE];

        // Write discriminator
        data[0..8].copy_from_slice(&NODE_DISCRIMINATOR);

        // Write node_id
        data[8..16].copy_from_slice(&[0x12, 0x34, 0x56, 0x78, 0, 0, 0, 0]);

        // Write address
        let addr = b"http://test:8080";
        data[16..16 + addr.len()].copy_from_slice(addr);

        // Write name
        let name = b"TestNode";
        data[16 + MAX_ADDRESS_LEN..16 + MAX_ADDRESS_LEN + name.len()].copy_from_slice(name);

        // Write owner (32 bytes of zeros for test)
        // Already zeros

        // Write timestamps (registered_at and last_heartbeat)
        let ts: i64 = 1700000000;
        let ts_offset = 16 + MAX_ADDRESS_LEN + MAX_NAME_LEN + 32;
        data[ts_offset..ts_offset + 8].copy_from_slice(&ts.to_le_bytes());
        data[ts_offset + 8..ts_offset + 16].copy_from_slice(&ts.to_le_bytes());

        // Write is_active
        data[ts_offset + 16] = 1;

        let registration = NodeRegistration::try_from_slice(&data).unwrap();
        assert_eq!(registration.get_node_id_string(), "!12345678");
        assert_eq!(registration.get_address(), "http://test:8080");
        assert_eq!(registration.get_name(), "TestNode");
        assert!(registration.is_active);
    }

    #[test]
    fn test_expand_path() {
        // Test non-tilde path unchanged
        assert_eq!(expand_path("/foo/bar"), "/foo/bar");

        // Test tilde expansion (just check it doesn't panic and returns something)
        let expanded = expand_path("~/.config/test");
        assert!(!expanded.starts_with("~/"));
    }
}
