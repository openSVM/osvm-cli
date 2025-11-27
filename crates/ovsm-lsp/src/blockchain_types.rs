//! Blockchain type definitions for smart completions
//!
//! This module defines Solana-specific data structures and their fields
//! to provide context-aware completions when working with blockchain data.

use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind};

/// A blockchain type with its fields and documentation
#[derive(Debug, Clone)]
pub struct BlockchainType {
    /// Name of the type
    pub name: &'static str,
    /// Description of the type
    pub description: &'static str,
    /// Fields available on this type
    pub fields: &'static [BlockchainField],
}

/// A field on a blockchain type
#[derive(Debug, Clone)]
pub struct BlockchainField {
    /// Field name
    pub name: &'static str,
    /// Field type (as string)
    pub field_type: &'static str,
    /// Description
    pub description: &'static str,
}

/// All known blockchain types
pub static BLOCKCHAIN_TYPES: &[BlockchainType] = &[
    // ========================================================================
    // Account Types
    // ========================================================================
    BlockchainType {
        name: "AccountInfo",
        description: "Solana account information returned by getAccountInfo",
        fields: &[
            BlockchainField {
                name: "lamports",
                field_type: "u64",
                description: "Balance in lamports (1 SOL = 1,000,000,000 lamports)",
            },
            BlockchainField {
                name: "owner",
                field_type: "Pubkey",
                description: "Program that owns this account",
            },
            BlockchainField {
                name: "data",
                field_type: "bytes",
                description: "Account data as base64-encoded string",
            },
            BlockchainField {
                name: "executable",
                field_type: "bool",
                description: "Whether this account contains a program",
            },
            BlockchainField {
                name: "rentEpoch",
                field_type: "u64",
                description: "Epoch at which this account will next owe rent",
            },
        ],
    },

    // ========================================================================
    // Token Types
    // ========================================================================
    BlockchainType {
        name: "TokenAccount",
        description: "SPL Token account data",
        fields: &[
            BlockchainField {
                name: "mint",
                field_type: "Pubkey",
                description: "The mint associated with this token account",
            },
            BlockchainField {
                name: "owner",
                field_type: "Pubkey",
                description: "The owner of this token account",
            },
            BlockchainField {
                name: "amount",
                field_type: "u64",
                description: "Token balance (in smallest unit)",
            },
            BlockchainField {
                name: "delegate",
                field_type: "Pubkey?",
                description: "Optional delegate authorized to transfer tokens",
            },
            BlockchainField {
                name: "delegatedAmount",
                field_type: "u64",
                description: "Amount delegated to the delegate",
            },
            BlockchainField {
                name: "state",
                field_type: "string",
                description: "Account state: initialized, frozen, or uninitialized",
            },
            BlockchainField {
                name: "isNative",
                field_type: "bool",
                description: "Whether this is a native SOL token account",
            },
            BlockchainField {
                name: "closeAuthority",
                field_type: "Pubkey?",
                description: "Optional authority that can close this account",
            },
        ],
    },
    BlockchainType {
        name: "TokenMint",
        description: "SPL Token mint data",
        fields: &[
            BlockchainField {
                name: "mintAuthority",
                field_type: "Pubkey?",
                description: "Authority allowed to mint new tokens",
            },
            BlockchainField {
                name: "supply",
                field_type: "u64",
                description: "Total token supply",
            },
            BlockchainField {
                name: "decimals",
                field_type: "u8",
                description: "Number of decimals for display (e.g., 9 for SOL)",
            },
            BlockchainField {
                name: "isInitialized",
                field_type: "bool",
                description: "Whether the mint has been initialized",
            },
            BlockchainField {
                name: "freezeAuthority",
                field_type: "Pubkey?",
                description: "Authority allowed to freeze token accounts",
            },
        ],
    },

    // ========================================================================
    // Transaction Types
    // ========================================================================
    BlockchainType {
        name: "Transaction",
        description: "A Solana transaction",
        fields: &[
            BlockchainField {
                name: "signatures",
                field_type: "string[]",
                description: "Transaction signatures (first is the transaction ID)",
            },
            BlockchainField {
                name: "message",
                field_type: "Message",
                description: "The transaction message containing instructions",
            },
        ],
    },
    BlockchainType {
        name: "TransactionMeta",
        description: "Transaction metadata from getTransaction",
        fields: &[
            BlockchainField {
                name: "fee",
                field_type: "u64",
                description: "Transaction fee in lamports",
            },
            BlockchainField {
                name: "err",
                field_type: "object?",
                description: "Error if transaction failed, null if successful",
            },
            BlockchainField {
                name: "preBalances",
                field_type: "u64[]",
                description: "Account balances before the transaction",
            },
            BlockchainField {
                name: "postBalances",
                field_type: "u64[]",
                description: "Account balances after the transaction",
            },
            BlockchainField {
                name: "preTokenBalances",
                field_type: "TokenBalance[]",
                description: "Token balances before the transaction",
            },
            BlockchainField {
                name: "postTokenBalances",
                field_type: "TokenBalance[]",
                description: "Token balances after the transaction",
            },
            BlockchainField {
                name: "logMessages",
                field_type: "string[]",
                description: "Program log messages from execution",
            },
            BlockchainField {
                name: "innerInstructions",
                field_type: "InnerInstruction[]",
                description: "CPI (Cross-Program Invocation) instructions",
            },
        ],
    },

    // ========================================================================
    // Transfer Types (from get_account_transfers)
    // ========================================================================
    BlockchainType {
        name: "Transfer",
        description: "A transfer from get_account_transfers API",
        fields: &[
            BlockchainField {
                name: "signature",
                field_type: "string",
                description: "Transaction signature",
            },
            BlockchainField {
                name: "timestamp",
                field_type: "i64",
                description: "Unix timestamp of the transaction",
            },
            BlockchainField {
                name: "source",
                field_type: "Pubkey",
                description: "Source wallet address",
            },
            BlockchainField {
                name: "destination",
                field_type: "Pubkey",
                description: "Destination wallet address",
            },
            BlockchainField {
                name: "amount",
                field_type: "f64",
                description: "Transfer amount (in human-readable units)",
            },
            BlockchainField {
                name: "transferType",
                field_type: "string",
                description: "Type: sol_transfer, token, swap, nft_transfer, nft_sale, nft_mint",
            },
            BlockchainField {
                name: "mint",
                field_type: "Pubkey?",
                description: "Token mint address (for token transfers)",
            },
            BlockchainField {
                name: "tokenSymbol",
                field_type: "string?",
                description: "Token symbol if known (e.g., USDC, SOL)",
            },
            BlockchainField {
                name: "fee",
                field_type: "u64",
                description: "Transaction fee in lamports",
            },
            BlockchainField {
                name: "status",
                field_type: "string",
                description: "Transaction status: success or failed",
            },
        ],
    },

    // ========================================================================
    // DEX Types
    // ========================================================================
    BlockchainType {
        name: "SwapInfo",
        description: "DEX swap information",
        fields: &[
            BlockchainField {
                name: "inputMint",
                field_type: "Pubkey",
                description: "Token being sold",
            },
            BlockchainField {
                name: "outputMint",
                field_type: "Pubkey",
                description: "Token being bought",
            },
            BlockchainField {
                name: "inputAmount",
                field_type: "u64",
                description: "Amount of input token",
            },
            BlockchainField {
                name: "outputAmount",
                field_type: "u64",
                description: "Amount of output token received",
            },
            BlockchainField {
                name: "slippage",
                field_type: "f64",
                description: "Price slippage percentage",
            },
            BlockchainField {
                name: "dex",
                field_type: "string",
                description: "DEX name: Raydium, Orca, Jupiter, etc.",
            },
            BlockchainField {
                name: "poolAddress",
                field_type: "Pubkey",
                description: "Liquidity pool address",
            },
        ],
    },

    // ========================================================================
    // NFT Types
    // ========================================================================
    BlockchainType {
        name: "NFTMetadata",
        description: "NFT metadata from Metaplex",
        fields: &[
            BlockchainField {
                name: "name",
                field_type: "string",
                description: "NFT name",
            },
            BlockchainField {
                name: "symbol",
                field_type: "string",
                description: "NFT symbol/collection ticker",
            },
            BlockchainField {
                name: "uri",
                field_type: "string",
                description: "URI to off-chain metadata (usually IPFS or Arweave)",
            },
            BlockchainField {
                name: "sellerFeeBasisPoints",
                field_type: "u16",
                description: "Royalty fee in basis points (100 = 1%)",
            },
            BlockchainField {
                name: "creators",
                field_type: "Creator[]",
                description: "List of creators with share percentages",
            },
            BlockchainField {
                name: "collection",
                field_type: "Pubkey?",
                description: "Collection this NFT belongs to",
            },
            BlockchainField {
                name: "isMutable",
                field_type: "bool",
                description: "Whether metadata can be updated",
            },
            BlockchainField {
                name: "primarySaleHappened",
                field_type: "bool",
                description: "Whether the primary sale has occurred",
            },
        ],
    },

    // ========================================================================
    // Program Types
    // ========================================================================
    BlockchainType {
        name: "ProgramAccount",
        description: "A program-owned account with parsed data",
        fields: &[
            BlockchainField {
                name: "pubkey",
                field_type: "Pubkey",
                description: "Account public key",
            },
            BlockchainField {
                name: "account",
                field_type: "AccountInfo",
                description: "Account information",
            },
        ],
    },

    // ========================================================================
    // Signature Types
    // ========================================================================
    BlockchainType {
        name: "SignatureInfo",
        description: "Transaction signature information from getSignaturesForAddress",
        fields: &[
            BlockchainField {
                name: "signature",
                field_type: "string",
                description: "Transaction signature (base58)",
            },
            BlockchainField {
                name: "slot",
                field_type: "u64",
                description: "Slot when transaction was processed",
            },
            BlockchainField {
                name: "blockTime",
                field_type: "i64?",
                description: "Unix timestamp of the block",
            },
            BlockchainField {
                name: "err",
                field_type: "object?",
                description: "Error object if transaction failed",
            },
            BlockchainField {
                name: "memo",
                field_type: "string?",
                description: "Transaction memo if present",
            },
            BlockchainField {
                name: "confirmationStatus",
                field_type: "string",
                description: "Confirmation status: processed, confirmed, finalized",
            },
        ],
    },
];

/// Well-known program IDs
pub static KNOWN_PROGRAMS: &[(&str, &str, &str)] = &[
    ("11111111111111111111111111111111", "System Program", "Native Solana system operations"),
    ("TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", "Token Program", "SPL Token standard operations"),
    ("ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL", "Associated Token Program", "Create associated token accounts"),
    ("metaqbxxUerdq28cj1RbAWkYQm3ybzjb6a8bt518x1s", "Metaplex Token Metadata", "NFT metadata program"),
    ("cndy3Z4yapfJBmL3ShUp5exZKqR3z33thTzeNMm2gRZ", "Candy Machine v2", "NFT minting program"),
    ("9xQeWvG816bUx9EPjHmaT23yvVM2ZWbrrpZb9PusVFin", "Serum DEX v3", "Order book DEX"),
    ("whirLbMiicVdio4qvUfM5KAg6Ct8VwpYzGff3uctyCc", "Orca Whirlpool", "Concentrated liquidity AMM"),
    ("675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8", "Raydium AMM", "Automated market maker"),
    ("JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4", "Jupiter v6", "DEX aggregator"),
    ("ComputeBudget111111111111111111111111111111", "Compute Budget", "Set compute unit limit/price"),
];

/// Transfer types for filtering
pub static TRANSFER_TYPES: &[(&str, &str)] = &[
    ("sol_transfer", "Native SOL transfer between wallets"),
    ("token", "SPL token transfer"),
    ("swap", "DEX swap/trade"),
    ("nft_transfer", "NFT transfer between wallets"),
    ("nft_sale", "NFT sale on marketplace"),
    ("nft_mint", "NFT minting operation"),
    ("stake", "Staking operation"),
    ("unstake", "Unstaking operation"),
    ("create_account", "Account creation"),
    ("close_account", "Account closure"),
];

/// Get completions for a blockchain type's fields
pub fn get_field_completions(type_name: &str) -> Vec<CompletionItem> {
    let type_name_lower = type_name.to_lowercase();

    // Find matching type
    let matching_type = BLOCKCHAIN_TYPES.iter().find(|t| {
        t.name.to_lowercase() == type_name_lower
            || t.name.to_lowercase().contains(&type_name_lower)
    });

    match matching_type {
        Some(bt) => bt
            .fields
            .iter()
            .map(|field| CompletionItem {
                label: field.name.to_string(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(field.field_type.to_string()),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("**{}**: {}\n\nType: `{}`", field.name, field.description, field.field_type),
                })),
                insert_text: Some(format!("\"{}\"", field.name)),
                sort_text: Some(format!("0{}", field.name)),
                ..Default::default()
            })
            .collect(),
        None => vec![],
    }
}

/// Get completions for transfer type values
pub fn get_transfer_type_completions() -> Vec<CompletionItem> {
    TRANSFER_TYPES
        .iter()
        .map(|(name, desc)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Transfer type".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: desc.to_string(),
            })),
            insert_text: Some(format!("\"{}\"", name)),
            sort_text: Some(format!("0{}", name)),
            ..Default::default()
        })
        .collect()
}

/// Get completions for known program IDs
pub fn get_program_id_completions() -> Vec<CompletionItem> {
    KNOWN_PROGRAMS
        .iter()
        .map(|(id, name, desc)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::CONSTANT),
            detail: Some(format!("{}", &id[..8])),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("{}\n\n**Program ID:** `{}`", desc, id),
            })),
            insert_text: Some(format!("\"{}\"", id)),
            sort_text: Some(format!("0{}", name)),
            ..Default::default()
        })
        .collect()
}

/// Determine if we're accessing a blockchain type based on context
pub fn infer_type_from_context(prefix: &str, full_line: &str) -> Option<&'static str> {
    // Check for common patterns
    let patterns = [
        ("getAccountInfo", "AccountInfo"),
        ("getBalance", "AccountInfo"),
        ("getTokenAccountsByOwner", "TokenAccount"),
        ("get_account_transfers", "Transfer"),
        ("getTransaction", "TransactionMeta"),
        ("getSignaturesForAddress", "SignatureInfo"),
    ];

    for (func, type_name) in patterns {
        if full_line.contains(func) {
            return Some(type_name);
        }
    }

    // Check for variable names that suggest types
    let var_patterns = [
        ("transfer", "Transfer"),
        ("tx", "Transaction"),
        ("token", "TokenAccount"),
        ("account", "AccountInfo"),
        ("nft", "NFTMetadata"),
        ("swap", "SwapInfo"),
        ("sig", "SignatureInfo"),
    ];

    let prefix_lower = prefix.to_lowercase();
    for (pattern, type_name) in var_patterns {
        if prefix_lower.contains(pattern) {
            return Some(type_name);
        }
    }

    None
}

/// Get all blockchain type completions (for top-level suggestions)
pub fn get_all_type_completions() -> Vec<CompletionItem> {
    BLOCKCHAIN_TYPES
        .iter()
        .map(|bt| CompletionItem {
            label: bt.name.to_string(),
            kind: Some(CompletionItemKind::STRUCT),
            detail: Some("Blockchain type".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("**{}**\n\n{}\n\n**Fields:**\n{}",
                    bt.name,
                    bt.description,
                    bt.fields.iter()
                        .map(|f| format!("- `{}`: {} - {}", f.name, f.field_type, f.description))
                        .collect::<Vec<_>>()
                        .join("\n")
                ),
            })),
            insert_text: Some(bt.name.to_string()),
            sort_text: Some(format!("0{}", bt.name)),
            ..Default::default()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_field_completions() {
        let completions = get_field_completions("Transfer");
        assert!(!completions.is_empty());
        assert!(completions.iter().any(|c| c.label == "signature"));
        assert!(completions.iter().any(|c| c.label == "amount"));
        assert!(completions.iter().any(|c| c.label == "transferType"));
    }

    #[test]
    fn test_get_transfer_type_completions() {
        let completions = get_transfer_type_completions();
        assert!(!completions.is_empty());
        assert!(completions.iter().any(|c| c.label == "sol_transfer"));
        assert!(completions.iter().any(|c| c.label == "swap"));
    }

    #[test]
    fn test_infer_type_from_context() {
        assert_eq!(infer_type_from_context("", "(get_account_transfers wallet)"), Some("Transfer"));
        assert_eq!(infer_type_from_context("transfer", ""), Some("Transfer"));
        assert_eq!(infer_type_from_context("tx", ""), Some("Transaction"));
    }

    #[test]
    fn test_known_programs() {
        let completions = get_program_id_completions();
        assert!(completions.iter().any(|c| c.label == "Token Program"));
        assert!(completions.iter().any(|c| c.label == "System Program"));
    }
}
