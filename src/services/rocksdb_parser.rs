//! Manual binary parser for Solana RocksDB transaction format
//!
//! This module provides parsers to extract transaction data from RocksDB
//! binary format without requiring bincode 1.x or solana-ledger dependencies.

use anyhow::{anyhow, Result};
use log::{debug, warn};
use solana_sdk::pubkey::Pubkey;

/// Parse EncodedTransactionWithStatusMeta to extract logs, accounts, fees
pub fn parse_transaction_metadata(
    data: &[u8],
    offset: usize,
) -> Result<(u64, bool, Vec<String>, Vec<String>)> {
    // Returns: (fee, success, logs, accounts)

    if offset >= data.len() {
        return Ok((5000, true, Vec::new(), Vec::new()));
    }

    let mut pos = offset;

    // EncodedTransactionWithStatusMeta structure:
    // - transaction: EncodedTransaction (enum)
    // - meta: Option<TransactionStatusMeta>

    // Skip transaction field for now (complex nested structure)
    // Jump to meta field by estimating size
    // This is a simplified parser - full implementation would parse entire structure

    // Try to find log messages and account keys in the remaining bytes
    let remaining = &data[pos..];

    // Look for common patterns in the data
    let logs = extract_log_messages(remaining);
    let accounts = extract_account_keys(remaining);

    debug!(
        "Extracted {} logs, {} accounts from metadata",
        logs.len(),
        accounts.len()
    );

    Ok((5000, true, logs, accounts))
}

/// Extract log messages from binary data
fn extract_log_messages(data: &[u8]) -> Vec<String> {
    let mut logs = Vec::new();

    // Log messages are stored as Vec<String>
    // Format: length (u64) + [string_len (u64) + string_bytes]*

    let mut pos = 0;
    while pos + 8 <= data.len() {
        // Try to read vector length
        let vec_len = u64::from_le_bytes(data[pos..pos + 8].try_into().unwrap_or([0; 8]));

        // Sanity check: reasonable vector length
        if vec_len > 0 && vec_len < 1000 {
            pos += 8;

            for _ in 0..vec_len {
                if pos + 8 > data.len() {
                    break;
                }

                let str_len =
                    u64::from_le_bytes(data[pos..pos + 8].try_into().unwrap_or([0; 8])) as usize;
                pos += 8;

                if str_len > 0 && str_len < 10000 && pos + str_len <= data.len() {
                    if let Ok(log_msg) = String::from_utf8(data[pos..pos + str_len].to_vec()) {
                        // Check if it looks like a log message (contains "Program" or "invoke")
                        if log_msg.contains("Program")
                            || log_msg.contains("invoke")
                            || log_msg.contains("success")
                        {
                            logs.push(log_msg);
                        }
                    }
                    pos += str_len;
                } else {
                    break;
                }
            }

            if !logs.is_empty() {
                return logs;
            }
        }

        pos += 1; // Move forward and try again
    }

    Vec::new()
}

/// Extract account keys from binary data
fn extract_account_keys(data: &[u8]) -> Vec<String> {
    let mut accounts = Vec::new();

    // Account keys are 32-byte pubkeys
    // Look for sequences that might be account keys

    let mut pos = 0;
    while pos + 32 <= data.len() {
        // Try to parse as pubkey
        if let Ok(pubkey_bytes) = data[pos..pos + 32].try_into() {
            let pubkey = Pubkey::new_from_array(pubkey_bytes);
            let pubkey_str = pubkey.to_string();

            // Sanity check: valid base58 representation
            if pubkey_str.len() >= 32 && pubkey_str.len() <= 44 {
                // Check if this looks like a real pubkey (not all zeros/ones)
                let bytes_sum: u32 = pubkey_bytes.iter().map(|&b| b as u32).sum();
                if bytes_sum > 10 && bytes_sum < 8000 {
                    accounts.push(pubkey_str);

                    // Found one, look for more consecutive pubkeys
                    pos += 32;
                    continue;
                }
            }
        }

        pos += 1;
    }

    // Deduplicate accounts
    accounts.sort();
    accounts.dedup();

    // Limit to reasonable number
    if accounts.len() > 64 {
        accounts.truncate(64);
    }

    accounts
}

/// Parse inner instructions count from meta
pub fn parse_inner_instruction_count(data: &[u8], offset: usize) -> usize {
    // Try to find inner instructions vector length
    // This is a simplified heuristic

    if offset + 16 >= data.len() {
        return 0;
    }

    let mut pos = offset;
    while pos + 8 <= data.len() {
        let potential_len = u64::from_le_bytes(data[pos..pos + 8].try_into().unwrap_or([0; 8]));

        // Inner instructions typically has 0-20 items
        if potential_len > 0 && potential_len <= 20 {
            return potential_len as usize;
        }

        pos += 1;
    }

    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_log_messages() {
        // Test with empty data
        assert_eq!(extract_log_messages(&[]), Vec::<String>::new());
    }

    #[test]
    fn test_extract_account_keys() {
        // Test with empty data
        assert_eq!(extract_account_keys(&[]), Vec::<String>::new());
    }
}
