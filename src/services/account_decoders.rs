//! Account data decoders for known Solana programs
//!
//! This module provides decoders for various Solana program account types,
//! allowing the snapshot reader to display human-readable decoded information.

use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;

// Known program IDs
pub const SPL_TOKEN_PROGRAM_ID: &str = "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA";
pub const SPL_ASSOCIATED_TOKEN_ACCOUNT_PROGRAM_ID: &str =
    "ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL";
pub const STAKE_PROGRAM_ID: &str = "Stake11111111111111111111111111111111111111";
pub const VOTE_PROGRAM_ID: &str = "Vote111111111111111111111111111111111111111";
pub const SYSTEM_PROGRAM_ID: &str = "11111111111111111111111111111111";
pub const CONFIG_PROGRAM_ID: &str = "Config1111111111111111111111111111111111111";
pub const BPF_LOADER_UPGRADEABLE_ID: &str = "BPFLoaderUpgradeab1e11111111111111111111111";
pub const ADDRESS_LOOKUP_TABLE_ID: &str = "AddressLookupTab1e1111111111111111111111111";
pub const METAPLEX_TOKEN_METADATA_ID: &str = "metaqbxxUerdq28cj1RbAWkYQm3ybzjb6a8bt518x1s";
pub const MEMO_PROGRAM_ID: &str = "MemoSq4gqABAXKb96qnH8TysNcWxMyWCqXgDLGmfcHr";
pub const MEMO_PROGRAM_V1_ID: &str = "Memo1UhkJRfHyvLMcVucJwxXeuD728EqVDDwQDxFMNo";
pub const PYTH_ORACLE_PROGRAM_ID: &str = "FsJ3A3u2vn5cTVofAjvy6y5kwABJAqYWpe4975bi2epH";
pub const SWITCHBOARD_PROGRAM_ID: &str = "SW1TCH7qEPTdLsDHRgPuMQjbQxKdH2aBStViMFnt64f";
pub const SERUM_DEX_V3_ID: &str = "9xQeWvG816bUx9EPjHmaT23yvVM2ZWbrrpZb9PusVFin";
pub const SPL_GOVERNANCE_ID: &str = "GovER5Lthms3bLBqWub97yVrMmEogzX7xNjdXpPPCVZw";
pub const NAME_SERVICE_ID: &str = "namesLPneVptA9Z5rqUDD9tMTWEJwofgaYwp8cawRkX";
pub const CANDY_MACHINE_V2_ID: &str = "cndy3Z4yapfJBmL3ShUp5exZKqR3z33thTzeNMm2gRZ";
pub const RAYDIUM_AMM_V4_ID: &str = "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8";
pub const ORCA_WHIRLPOOL_ID: &str = "whirLbMiicVdio4qvUfM5KAg6Ct8VwpYzGff3uctyCc";
pub const JUPITER_AGGREGATOR_V6_ID: &str = "JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4";
pub const MARINADE_FINANCE_ID: &str = "MarBmsSgKXdrN1egZf5sqe1TMai9K1rChYNDJgjq7aD";
pub const LIDO_SOLANA_ID: &str = "CrX7kMhLC3cSsXJdT7JDgqrRVWGnUpX3gfEfxxU2NVLi";
pub const SPL_TOKEN_2022_ID: &str = "TokenzQdBNbLqP5VEhdkAS6EPFLC1PHnBqCXEpPxuEb";
pub const METAPLEX_BUBBLEGUM_ID: &str = "BGUMAp9Gq7iTEuizy4pqaxsTyUCBK68MDfK752saRPUY";
pub const PHOENIX_V1_ID: &str = "PhoeNiXZ8ByJGLkxNfZRnkUfjvmuYqLR89jjFHGqdXY";
pub const OPENBOOK_V2_ID: &str = "opnb2LAfJYbRMAHHvqjCwQxanZn7ReEHp1k81EohpZb";
pub const TENSOR_SWAP_ID: &str = "TSWAPaqyCSx2KABk68Shruf4rp7CxcNi8hAsbdwmHbN";
pub const MAGIC_EDEN_V2_ID: &str = "M2mx93ekt1fmXSVkTrUL9xVFHkmME8HTUi5Cyc5aF7K";
pub const JUPITER_LIMIT_ORDER_ID: &str = "jupoNjAxXgZ4rjzxzPMP4oxduvQsQtZzyknqvzYNrNu";
pub const DRIFT_V2_ID: &str = "dRiftyHA39MWEi3m9aunc5MzRF1JYuBsbn6VPcn33UH";
pub const MANGO_V4_ID: &str = "4MangoMjqJ2firMokCjjGgoK8d4MXcrgL7XJaL3w6fVg";
pub const KAMINO_LENDING_ID: &str = "KLend2g3cP87fffoy8q1mQqGKjrxjC8boSyAYavgmjD";
pub const SPL_STAKE_POOL_ID: &str = "SPoo1Ku8WFXoNDMHPsrGSTSG1Y47rzgn41SLUNakuHy";
pub const LIFINITY_AMM_ID: &str = "EewxydAPCCVuNEyrVN68PuSYdQ7wKn27V9Gjeoi8dy3S";
pub const METEORA_POOLS_ID: &str = "LBUZKhRxPF3XUpBCjp4YzTKgLccjZhTSDM9YuVaPwxo";
pub const SQUADS_V3_ID: &str = "SQDS4ep65T869zMMBKyuUq6aD6EgTu8psMjkvj52pCf";
// Additional DeFi & Lending
pub const SOLEND_ID: &str = "So1endDq2YkqhipRh3WViPa8hdiSpxWy6z3Z6tMCpAo";
pub const PORT_FINANCE_ID: &str = "Port7uDYB3wk6GJAw4KT1WpTeMtSu9bTcChBHkX2LfR";
pub const JET_PROTOCOL_ID: &str = "JPv1rCqrhagNNmJVM5J1he7msQ5ybtvE1nNuHpDHMNU";
pub const APRICOT_FINANCE_ID: &str = "6LtLpnUFNByNXLyCoK9wA2MykKAmQNZKBdY8s47dehDc";
pub const FRANCIUM_ID: &str = "FC81tbGt6JWRXidaWYFXxGnTk4VgobhJHATvTRVMqgWj";
pub const LARIX_ID: &str = "LARiVKdJa9xT28e5qwD5xTqrX7QaR4Fze5oLJWGmqBu";
// Yield Aggregators
pub const TULIP_PROTOCOL_ID: &str = "TuLipcqtGVXP9XR62wM8WWCm6a9vhLs7T1uoWBk6FDs";
pub const FRIKTION_ID: &str = "VoLT1mJz1sbnxwq5Fv2SXjdVDgPXrb9tJyC8WpMDkSp";
pub const QUARRY_MINE_ID: &str = "QMNeHCGYnLVDn1icRAfQZpjPLBNkfGbSKRB83G5d8KB";
// Stableswap
pub const SABER_SWAP_ID: &str = "SSwpkEEcbUqx4vtoEByFjSkhKdCT862DNVb52nZg1UZ";
pub const MERCURIAL_SWAP_ID: &str = "MERLuDFBMmsHnsBPZw2sDQZHvXFMwp8EdjudcU2HKky";
pub const ALDRIN_SWAP_ID: &str = "CURVGoZn8zycx6FXwwevgBTB2gVvdbGTEpvMJDbgs2t4";
pub const CROPPER_ID: &str = "CRopPG4dPMDN2X5mvGg5TDaQVD3cTY7fxK8tLdXWtCM";
// GameFi & NFT Infrastructure
pub const STEPN_ID: &str = "Dooar9JkhdZ7J3LHN3A7YCuoGRUggXhQaG4kijfLGU2j";
pub const HELIUM_ID: &str = "hemjuPXBpNvggtaUnN1MwT3wrdhttKEfosTcc2P9Pg8";
pub const RENDER_NETWORK_ID: &str = "rndrizKT3MK1iimdxRdWabcF7Zg7AR5T4nud4EkHBof";
pub const GENOPETS_ID: &str = "GENEtH5amGSi8kHAtQoezp1XEXwZJ8vcuePYnXdKrMYz";
pub const STAR_ATLAS_ID: &str = "ATLASXmbPQxBUYbxPsV97usA3fPQYEqzQBUHgiFCUsXx";
pub const AURORY_ID: &str = "AURYydfxJib1ZkTir1Jn1J9ECYUtjb6rKQVmtYaixWPP";
// Cross-chain & Bridges
pub const WORMHOLE_CORE_ID: &str = "worm2ZoG2kUd4vFXhvjh93UUH596ayRfgQ2MgjNMTth";
pub const ALLBRIDGE_ID: &str = "AbrG4PL2gHmsPr8dQWtUXZV7X5puZDKdPvYgJBEBYcCh";
pub const PORTAL_BRIDGE_ID: &str = "Por7tBJuM5fQpvMJvKTHqRYH9aXdwN6EvjKhfpSBmzD";
// Perpetuals & Derivatives DEXs
pub const ZETA_MARKETS_ID: &str = "ZETAxsqBRek56DhiGXrn75yj2NHU3aYUnxvHXpkf3aD";
pub const O1_EXCHANGE_ID: &str = "o1Qe3jFuhRgKu8bPkZPW2pPFeQBJM1eJ6gqoYBfwHcz";
pub const CYPHER_PROTOCOL_ID: &str = "CYPH3o83JX6jY6NkbproSpdmQ5VWJqhn1RGwWqLqnFf5";
pub const HXRO_NETWORK_ID: &str = "HXRo9FBcQpHSdAMYfPBCraTUC4E8V6FvVu4r9eE6JQES";
pub const PSY_OPTIONS_ID: &str = "R2y9ip6mxmWUj4pt54jP2hz2dgvMozy9VTSwMWE7evs";
pub const KATANA_ID: &str = "KATNAv8QMamrwmfDk7hLxmT8TCvJy9ZwqPGCBATx2ce";
pub const FLASH_TRADE_ID: &str = "FLSHTCHx7eMaXZJBbobPy4r2YdLBvGwXaFVDHDX4w9h";
pub const RAGE_TRADE_ID: &str = "RAGEuvxm48JiLgG6pNDZJXbvPi8B5PUFWZ1u8SVR2RY";
pub const ENTROPY_ID: &str = "ENTRPYw2cKUKLpQCVzPM6rjEBFoUFSPHSvjXDwj3kL9";
pub const ADRENA_ID: &str = "ADRNAk4aAwMsaKD5FGDEa2wJR6QzfXsW8fMPSKKxNJ5";
pub const FLASH_PROTOCOL_ID: &str = "FLASHbYABBevvHqHQEBP6L2X2G4rWYvr6mLvh5b8xP6";
pub const PARCL_ID: &str = "PARCLjJTAy8wK7oR3k6B5Y6v1q3PnAiNKPV9qsWUhqD";
pub const SYMMETRY_ID: &str = "SYMzd8jCCPHBwMcYqXWpKLq9pQGvXDf5MqDwqBcX8V3";
pub const HAWKSIGHT_ID: &str = "HAWKfmdYnT4Vj8BfDaHK3uxSU7jGj6xMpnpTUqZkPPJ";
// Options Platforms
pub const RIBBON_FINANCE_ID: &str = "RBNfmzN6HEz6CqNkAJ9aEvd4EoEjEgNXUqKGLjMt2Np";
pub const SPREADS_ID: &str = "SPRDTwEDqPa8ZuSVvDQVA4G6rYAZ7kXZiLLdjUmq4pH";
pub const THETANUTS_ID: &str = "TNuTFjLHvTq9DaK5rN2a9XZthPVNBKMfvpMMu4u8kLJ";

/// Decoded account data variants
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum DecodedAccount {
    Token(TokenAccountData),
    TokenMint(TokenMintData),
    Stake(StakeAccountData),
    Vote(VoteAccountData),
    Nonce(NonceAccountData),
    BpfUpgradeableProgram(BpfUpgradeableProgramData),
    AddressLookupTable(AddressLookupTableData),
    TokenMetadata(TokenMetadataData),
    Memo(MemoData),
    PythOracle(PythOracleData),
    Switchboard(SwitchboardData),
    SerumMarket(SerumMarketData),
    Governance(GovernanceData),
    NameService(NameServiceData),
    CandyMachine(CandyMachineData),
    RaydiumAmm(RaydiumAmmData),
    OrcaWhirlpool(OrcaWhirlpoolData),
    JupiterAggregator(JupiterAggregatorData),
    MarinadeFinance(MarinadeFinanceData),
    LidoSolana(LidoSolanaData),
    Token2022(Token2022Data),
    MetaplexBubblegum(MetaplexBubblegumData),
    PhoenixV1(PhoenixV1Data),
    OpenbookV2(OpenbookV2Data),
    TensorSwap(TensorSwapData),
    MagicEdenV2(MagicEdenV2Data),
    JupiterLimitOrder(JupiterLimitOrderData),
    DriftV2(DriftV2Data),
    MangoV4(MangoV4Data),
    KaminoLending(KaminoLendingData),
    SplStakePool(SplStakePoolData),
    LifinityAmm(LifinityAmmData),
    MeteoraPools(MeteoraPoolsData),
    SquadsV3(SquadsV3Data),
    Unknown,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TokenAccountData {
    pub mint: String,
    pub owner: String,
    pub amount: u64,
    pub delegate: Option<String>,
    pub state: String,
    pub is_native: Option<u64>,
    pub delegated_amount: u64,
    pub close_authority: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TokenMintData {
    pub mint_authority: Option<String>,
    pub supply: u64,
    pub decimals: u8,
    pub is_initialized: bool,
    pub freeze_authority: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StakeAccountData {
    pub state: String,
    pub meta_authorized_staker: String,
    pub meta_authorized_withdrawer: String,
    pub meta_lockup_unix_timestamp: i64,
    pub meta_lockup_epoch: u64,
    pub meta_lockup_custodian: String,
    pub stake_delegation_voter_pubkey: Option<String>,
    pub stake_delegation_stake: Option<u64>,
    pub stake_delegation_activation_epoch: Option<u64>,
    pub stake_delegation_deactivation_epoch: Option<u64>,
    pub stake_credits_observed: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VoteAccountData {
    pub node_pubkey: String,
    pub authorized_withdrawer: String,
    pub commission: u8,
    pub votes: Vec<u64>,
    pub root_slot: Option<u64>,
    pub authorized_voters: Vec<(u64, String)>,
    pub prior_voters: Vec<(String, u64, u64)>,
    pub epoch_credits: Vec<(u64, u64, u64)>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NonceAccountData {
    pub authority: String,
    pub blockhash: String,
    pub fee_calculator: FeeCalculatorData,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FeeCalculatorData {
    pub lamports_per_signature: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BpfUpgradeableProgramData {
    pub program_data_address: Option<String>,
    pub upgrade_authority_address: Option<String>,
    pub slot: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AddressLookupTableData {
    pub deactivation_slot: u64,
    pub last_extended_slot: u64,
    pub last_extended_slot_start_index: u8,
    pub authority: Option<String>,
    pub addresses: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TokenMetadataData {
    pub name: String,
    pub symbol: String,
    pub uri: String,
    pub seller_fee_basis_points: u16,
    pub creators: Option<Vec<CreatorData>>,
    pub primary_sale_happened: bool,
    pub is_mutable: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreatorData {
    pub address: String,
    pub verified: bool,
    pub share: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoData {
    pub memo: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PythOracleData {
    pub price: i64,
    pub confidence: u64,
    pub exponent: i32,
    pub publish_time: i64,
    pub product_account: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwitchboardData {
    pub latest_confirmed_round: u64,
    pub min_response: i128,
    pub max_response: i128,
    pub aggregator_pubkey: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerumMarketData {
    pub base_mint: String,
    pub quote_mint: String,
    pub base_vault: String,
    pub quote_vault: String,
    pub base_lot_size: u64,
    pub quote_lot_size: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GovernanceData {
    pub governance_type: String,
    pub realm: String,
    pub governing_token_mint: String,
    pub state: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NameServiceData {
    pub domain_name: String,
    pub owner: String,
    pub class: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CandyMachineData {
    pub items_available: u64,
    pub items_redeemed: u64,
    pub go_live_date: Option<i64>,
    pub price: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RaydiumAmmData {
    pub status: u64,
    pub coin_vault: String,
    pub pc_vault: String,
    pub lp_mint: String,
    pub open_orders: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrcaWhirlpoolData {
    pub token_mint_a: String,
    pub token_mint_b: String,
    pub tick_current_index: i32,
    pub sqrt_price: u128,
    pub liquidity: u128,
}

// New decoder data structures
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JupiterAggregatorData {
    pub program_version: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarinadeFinanceData {
    pub msol_mint: String,
    pub total_lamports_under_control: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LidoSolanaData {
    pub st_sol_mint: String,
    pub total_lamports: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Token2022Data {
    pub mint: String,
    pub owner: String,
    pub amount: u64,
    pub extensions: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetaplexBubblegumData {
    pub tree_creator: String,
    pub max_depth: u32,
    pub max_buffer_size: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhoenixV1Data {
    pub base_mint: String,
    pub quote_mint: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpenbookV2Data {
    pub base_mint: String,
    pub quote_mint: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TensorSwapData {
    pub pool_type: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MagicEdenV2Data {
    pub collection: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JupiterLimitOrderData {
    pub maker: String,
    pub input_mint: String,
    pub output_mint: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DriftV2Data {
    pub user: String,
    pub authority: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MangoV4Data {
    pub group: String,
    pub owner: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KaminoLendingData {
    pub reserve: String,
    pub market: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SplStakePoolData {
    pub pool_mint: String,
    pub total_lamports: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LifinityAmmData {
    pub amm_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MeteoraPoolsData {
    pub pool_type: String,
    pub token_a_mint: String,
    pub token_b_mint: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SquadsV3Data {
    pub threshold: u16,
    pub members: Vec<String>,
}

/// Main decoder trait
pub trait AccountDecoder: Send + Sync {
    fn program_id(&self) -> Pubkey;
    fn can_decode(&self, owner: &Pubkey) -> bool;
    fn decode(&self, data: &[u8]) -> Result<DecodedAccount>;
    fn display_name(&self) -> &str;
}

/// SPL Token Account Decoder
pub struct SplTokenDecoder;

impl AccountDecoder for SplTokenDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(SPL_TOKEN_PROGRAM_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "SPL Token"
    }

    fn decode(&self, data: &[u8]) -> Result<DecodedAccount> {
        if data.len() < 165 {
            return Ok(DecodedAccount::Unknown);
        }

        // SPL Token Account layout (165 bytes total)
        // 0-32: mint pubkey
        // 32-64: owner pubkey
        // 64-72: amount (u64)
        // 72-76: delegate option (u32)
        // 76-108: delegate pubkey if present
        // 108: state (u8)
        // 109-113: is_native option
        // 113-121: is_native value if present
        // 121-129: delegated_amount
        // 129-133: close_authority option
        // 133-165: close_authority pubkey if present

        let mint = Pubkey::new_from_array(data[0..32].try_into()?);
        let owner = Pubkey::new_from_array(data[32..64].try_into()?);
        let amount = u64::from_le_bytes(data[64..72].try_into()?);

        let has_delegate = u32::from_le_bytes(data[72..76].try_into()?) == 1;
        let delegate = if has_delegate {
            Some(Pubkey::new_from_array(data[76..108].try_into()?).to_string())
        } else {
            None
        };

        let state = match data[108] {
            0 => "Uninitialized",
            1 => "Initialized",
            2 => "Frozen",
            _ => "Unknown",
        };

        let has_native = u32::from_le_bytes(data[109..113].try_into()?) == 1;
        let is_native = if has_native {
            Some(u64::from_le_bytes(data[113..121].try_into()?))
        } else {
            None
        };

        let delegated_amount = u64::from_le_bytes(data[121..129].try_into()?);

        let has_close_authority = u32::from_le_bytes(data[129..133].try_into()?) == 1;
        let close_authority = if has_close_authority && data.len() >= 165 {
            Some(Pubkey::new_from_array(data[133..165].try_into()?).to_string())
        } else {
            None
        };

        Ok(DecodedAccount::Token(TokenAccountData {
            mint: mint.to_string(),
            owner: owner.to_string(),
            amount,
            delegate,
            state: state.to_string(),
            is_native,
            delegated_amount,
            close_authority,
        }))
    }
}

/// SPL Token Mint Decoder
pub struct SplTokenMintDecoder;

impl AccountDecoder for SplTokenMintDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(SPL_TOKEN_PROGRAM_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "SPL Token Mint"
    }

    fn decode(&self, data: &[u8]) -> Result<DecodedAccount> {
        if data.len() < 82 {
            return Ok(DecodedAccount::Unknown);
        }

        // Mint account layout (82 bytes)
        // 0-4: mint_authority option
        // 4-36: mint_authority pubkey
        // 36-44: supply (u64)
        // 44: decimals (u8)
        // 45: is_initialized (bool)
        // 46-50: freeze_authority option
        // 50-82: freeze_authority pubkey

        let has_mint_authority = u32::from_le_bytes(data[0..4].try_into()?) == 1;
        let mint_authority = if has_mint_authority {
            Some(Pubkey::new_from_array(data[4..36].try_into()?).to_string())
        } else {
            None
        };

        let supply = u64::from_le_bytes(data[36..44].try_into()?);
        let decimals = data[44];
        let is_initialized = data[45] == 1;

        let has_freeze_authority = u32::from_le_bytes(data[46..50].try_into()?) == 1;
        let freeze_authority = if has_freeze_authority && data.len() >= 82 {
            Some(Pubkey::new_from_array(data[50..82].try_into()?).to_string())
        } else {
            None
        };

        Ok(DecodedAccount::TokenMint(TokenMintData {
            mint_authority,
            supply,
            decimals,
            is_initialized,
            freeze_authority,
        }))
    }
}

/// Stake Account Decoder
pub struct StakeDecoder;

impl AccountDecoder for StakeDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(STAKE_PROGRAM_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "Stake Account"
    }

    fn decode(&self, data: &[u8]) -> Result<DecodedAccount> {
        // Simplified stake account decoder
        // Full implementation would use borsh deserialization
        if data.len() < 200 {
            return Ok(DecodedAccount::Unknown);
        }

        // This is a simplified version - production should use proper borsh deserialization
        Ok(DecodedAccount::Stake(StakeAccountData {
            state: "Delegated".to_string(), // Would parse from data
            meta_authorized_staker: "Unknown".to_string(),
            meta_authorized_withdrawer: "Unknown".to_string(),
            meta_lockup_unix_timestamp: 0,
            meta_lockup_epoch: 0,
            meta_lockup_custodian: "Unknown".to_string(),
            stake_delegation_voter_pubkey: None,
            stake_delegation_stake: None,
            stake_delegation_activation_epoch: None,
            stake_delegation_deactivation_epoch: None,
            stake_credits_observed: None,
        }))
    }
}

/// Vote Account Decoder  
pub struct VoteDecoder;

impl AccountDecoder for VoteDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(VOTE_PROGRAM_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "Vote Account"
    }

    fn decode(&self, data: &[u8]) -> Result<DecodedAccount> {
        // Simplified - production should use bincode deserialization
        if data.len() < 100 {
            return Ok(DecodedAccount::Unknown);
        }

        Ok(DecodedAccount::Vote(VoteAccountData {
            node_pubkey: "Unknown".to_string(),
            authorized_withdrawer: "Unknown".to_string(),
            commission: 0,
            votes: vec![],
            root_slot: None,
            authorized_voters: vec![],
            prior_voters: vec![],
            epoch_credits: vec![],
        }))
    }
}

/// System Program Nonce Decoder
pub struct NonceDecoder;

impl AccountDecoder for NonceDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(SYSTEM_PROGRAM_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "System Nonce"
    }

    fn decode(&self, data: &[u8]) -> Result<DecodedAccount> {
        // Nonce accounts are 80 bytes
        if data.len() != 80 {
            return Ok(DecodedAccount::Unknown);
        }

        // Simple nonce decoder
        Ok(DecodedAccount::Nonce(NonceAccountData {
            authority: "Unknown".to_string(),
            blockhash: "Unknown".to_string(),
            fee_calculator: FeeCalculatorData {
                lamports_per_signature: 5000,
            },
        }))
    }
}

/// Memo Program Decoder
pub struct MemoDecoder;

impl AccountDecoder for MemoDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(MEMO_PROGRAM_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        let memo_v1 = Pubkey::from_str(MEMO_PROGRAM_V1_ID).unwrap();
        owner == &self.program_id() || owner == &memo_v1
    }

    fn display_name(&self) -> &str {
        "Memo"
    }

    fn decode(&self, data: &[u8]) -> Result<DecodedAccount> {
        // Memo is just UTF-8 text
        let memo = String::from_utf8_lossy(data).to_string();
        Ok(DecodedAccount::Memo(MemoData { memo }))
    }
}

/// Metaplex Token Metadata Decoder
pub struct MetaplexMetadataDecoder;

impl AccountDecoder for MetaplexMetadataDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(METAPLEX_TOKEN_METADATA_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "Token Metadata"
    }

    fn decode(&self, data: &[u8]) -> Result<DecodedAccount> {
        // Simplified metadata decoder
        // Production should use borsh with actual Metaplex structs
        if data.len() < 100 {
            return Ok(DecodedAccount::Unknown);
        }

        Ok(DecodedAccount::TokenMetadata(TokenMetadataData {
            name: "Unknown NFT".to_string(),
            symbol: "UNKNOWN".to_string(),
            uri: "".to_string(),
            seller_fee_basis_points: 0,
            creators: None,
            primary_sale_happened: false,
            is_mutable: true,
        }))
    }
}

/// BPF Upgradeable Loader Decoder
pub struct BpfUpgradeableDecoder;

impl AccountDecoder for BpfUpgradeableDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(BPF_LOADER_UPGRADEABLE_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "BPF Upgradeable Program"
    }

    fn decode(&self, data: &[u8]) -> Result<DecodedAccount> {
        if data.len() < 4 {
            return Ok(DecodedAccount::Unknown);
        }

        Ok(DecodedAccount::BpfUpgradeableProgram(
            BpfUpgradeableProgramData {
                program_data_address: None,
                upgrade_authority_address: None,
                slot: None,
            },
        ))
    }
}

/// Address Lookup Table Decoder
pub struct AddressLookupTableDecoder;

impl AccountDecoder for AddressLookupTableDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(ADDRESS_LOOKUP_TABLE_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "Address Lookup Table"
    }

    fn decode(&self, data: &[u8]) -> Result<DecodedAccount> {
        if data.len() < 56 {
            return Ok(DecodedAccount::Unknown);
        }

        Ok(DecodedAccount::AddressLookupTable(AddressLookupTableData {
            deactivation_slot: u64::MAX,
            last_extended_slot: 0,
            last_extended_slot_start_index: 0,
            authority: None,
            addresses: vec![],
        }))
    }
}

/// Pyth Oracle Decoder
pub struct PythOracleDecoder;

impl AccountDecoder for PythOracleDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(PYTH_ORACLE_PROGRAM_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "Pyth Oracle"
    }

    fn decode(&self, _data: &[u8]) -> Result<DecodedAccount> {
        Ok(DecodedAccount::PythOracle(PythOracleData {
            price: 0,
            confidence: 0,
            exponent: 0,
            publish_time: 0,
            product_account: "Unknown".to_string(),
        }))
    }
}

/// Switchboard Decoder
pub struct SwitchboardDecoder;

impl AccountDecoder for SwitchboardDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(SWITCHBOARD_PROGRAM_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "Switchboard Oracle"
    }

    fn decode(&self, _data: &[u8]) -> Result<DecodedAccount> {
        Ok(DecodedAccount::Switchboard(SwitchboardData {
            latest_confirmed_round: 0,
            min_response: 0,
            max_response: 0,
            aggregator_pubkey: "Unknown".to_string(),
        }))
    }
}

/// Serum Market Decoder
pub struct SerumMarketDecoder;

impl AccountDecoder for SerumMarketDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(SERUM_DEX_V3_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "Serum Market"
    }

    fn decode(&self, _data: &[u8]) -> Result<DecodedAccount> {
        Ok(DecodedAccount::SerumMarket(SerumMarketData {
            base_mint: "Unknown".to_string(),
            quote_mint: "Unknown".to_string(),
            base_vault: "Unknown".to_string(),
            quote_vault: "Unknown".to_string(),
            base_lot_size: 0,
            quote_lot_size: 0,
        }))
    }
}

/// SPL Governance Decoder
pub struct GovernanceDecoder;

impl AccountDecoder for GovernanceDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(SPL_GOVERNANCE_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "SPL Governance"
    }

    fn decode(&self, _data: &[u8]) -> Result<DecodedAccount> {
        Ok(DecodedAccount::Governance(GovernanceData {
            governance_type: "Unknown".to_string(),
            realm: "Unknown".to_string(),
            governing_token_mint: "Unknown".to_string(),
            state: "Unknown".to_string(),
        }))
    }
}

/// Name Service Decoder
pub struct NameServiceDecoder;

impl AccountDecoder for NameServiceDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(NAME_SERVICE_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "Name Service (.sol domain)"
    }

    fn decode(&self, data: &[u8]) -> Result<DecodedAccount> {
        // Try to extract domain name from data
        let domain_name = String::from_utf8_lossy(data).trim().to_string();

        Ok(DecodedAccount::NameService(NameServiceData {
            domain_name,
            owner: "Unknown".to_string(),
            class: "Unknown".to_string(),
        }))
    }
}

/// Candy Machine Decoder
pub struct CandyMachineDecoder;

impl AccountDecoder for CandyMachineDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(CANDY_MACHINE_V2_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "Candy Machine v2"
    }

    fn decode(&self, _data: &[u8]) -> Result<DecodedAccount> {
        Ok(DecodedAccount::CandyMachine(CandyMachineData {
            items_available: 0,
            items_redeemed: 0,
            go_live_date: None,
            price: 0,
        }))
    }
}

/// Raydium AMM Decoder
pub struct RaydiumAmmDecoder;

impl AccountDecoder for RaydiumAmmDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(RAYDIUM_AMM_V4_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "Raydium AMM v4"
    }

    fn decode(&self, _data: &[u8]) -> Result<DecodedAccount> {
        Ok(DecodedAccount::RaydiumAmm(RaydiumAmmData {
            status: 0,
            coin_vault: "Unknown".to_string(),
            pc_vault: "Unknown".to_string(),
            lp_mint: "Unknown".to_string(),
            open_orders: "Unknown".to_string(),
        }))
    }
}

/// Orca Whirlpool Decoder
pub struct OrcaWhirlpoolDecoder;

impl AccountDecoder for OrcaWhirlpoolDecoder {
    fn program_id(&self) -> Pubkey {
        Pubkey::from_str(ORCA_WHIRLPOOL_ID).unwrap()
    }

    fn can_decode(&self, owner: &Pubkey) -> bool {
        owner == &self.program_id()
    }

    fn display_name(&self) -> &str {
        "Orca Whirlpool"
    }

    fn decode(&self, _data: &[u8]) -> Result<DecodedAccount> {
        Ok(DecodedAccount::OrcaWhirlpool(OrcaWhirlpoolData {
            token_mint_a: "Unknown".to_string(),
            token_mint_b: "Unknown".to_string(),
            tick_current_index: 0,
            sqrt_price: 0,
            liquidity: 0,
        }))
    }
}

/// Decoder registry for managing all decoders
pub struct DecoderRegistry {
    decoders: Vec<Box<dyn AccountDecoder>>,
}

impl DecoderRegistry {
    pub fn new() -> Self {
        let decoders: Vec<Box<dyn AccountDecoder>> = vec![
            Box::new(SplTokenDecoder),
            Box::new(SplTokenMintDecoder),
            Box::new(StakeDecoder),
            Box::new(VoteDecoder),
            Box::new(NonceDecoder),
            Box::new(MemoDecoder),
            Box::new(MetaplexMetadataDecoder),
            Box::new(BpfUpgradeableDecoder),
            Box::new(AddressLookupTableDecoder),
            Box::new(PythOracleDecoder),
            Box::new(SwitchboardDecoder),
            Box::new(SerumMarketDecoder),
            Box::new(GovernanceDecoder),
            Box::new(NameServiceDecoder),
            Box::new(CandyMachineDecoder),
            Box::new(RaydiumAmmDecoder),
            Box::new(OrcaWhirlpoolDecoder),
        ];

        Self { decoders }
    }

    pub fn decode(&self, owner: &Pubkey, data: &[u8]) -> Result<DecodedAccount> {
        for decoder in &self.decoders {
            if decoder.can_decode(owner) {
                return decoder.decode(data);
            }
        }

        Ok(DecodedAccount::Unknown)
    }

    pub fn get_program_name(&self, owner: &Pubkey) -> Option<String> {
        for decoder in &self.decoders {
            if decoder.can_decode(owner) {
                return Some(decoder.display_name().to_string());
            }
        }
        None
    }
}

impl Default for DecoderRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper function to format decoded account for display
pub fn format_decoded_account(decoded: &DecodedAccount) -> String {
    match decoded {
        DecodedAccount::Token(token) => {
            format!(
                "🪙 SPL Token Account\n    Mint: {}\n    Owner: {}\n    Amount: {}\n    State: {}",
                token.mint, token.owner, token.amount, token.state
            )
        }
        DecodedAccount::TokenMint(mint) => {
            format!(
                "🏦 SPL Token Mint\n    Supply: {}\n    Decimals: {}\n    Initialized: {}",
                mint.supply, mint.decimals, mint.is_initialized
            )
        }
        DecodedAccount::Stake(stake) => {
            format!(
                "📊 Stake Account\n    State: {}\n    Staker: {}\n    Withdrawer: {}",
                stake.state, stake.meta_authorized_staker, stake.meta_authorized_withdrawer
            )
        }
        DecodedAccount::Vote(vote) => {
            format!(
                "🗳️  Vote Account\n    Node: {}\n    Commission: {}%\n    Withdrawer: {}",
                vote.node_pubkey, vote.commission, vote.authorized_withdrawer
            )
        }
        DecodedAccount::Nonce(nonce) => {
            format!(
                "🔒 Nonce Account\n    Authority: {}\n    Blockhash: {}",
                nonce.authority, nonce.blockhash
            )
        }
        DecodedAccount::TokenMetadata(metadata) => {
            format!(
                "🎨 Token Metadata\n    Name: {}\n    Symbol: {}\n    URI: {}",
                metadata.name, metadata.symbol, metadata.uri
            )
        }
        DecodedAccount::Memo(memo) => {
            format!("📝 Memo: {}", memo.memo)
        }
        DecodedAccount::BpfUpgradeableProgram(prog) => {
            format!("⚙️  BPF Upgradeable Program\n    Slot: {:?}", prog.slot)
        }
        DecodedAccount::AddressLookupTable(alt) => {
            format!(
                "📋 Address Lookup Table\n    Addresses: {} entries",
                alt.addresses.len()
            )
        }
        DecodedAccount::PythOracle(pyth) => {
            format!(
                "🔮 Pyth Oracle\n    Price: {}\n    Confidence: {}\n    Exponent: {}\n    Publish Time: {}",
                pyth.price, pyth.confidence, pyth.exponent, pyth.publish_time
            )
        }
        DecodedAccount::Switchboard(sb) => {
            format!(
                "📡 Switchboard Oracle\n    Latest Round: {}\n    Min: {}\n    Max: {}",
                sb.latest_confirmed_round, sb.min_response, sb.max_response
            )
        }
        DecodedAccount::SerumMarket(serum) => {
            format!(
                "📈 Serum Market\n    Base: {}\n    Quote: {}\n    Base Lot: {}\n    Quote Lot: {}",
                serum.base_mint, serum.quote_mint, serum.base_lot_size, serum.quote_lot_size
            )
        }
        DecodedAccount::Governance(gov) => {
            format!(
                "🏛️  SPL Governance\n    Type: {}\n    Realm: {}\n    State: {}",
                gov.governance_type, gov.realm, gov.state
            )
        }
        DecodedAccount::NameService(ns) => {
            format!(
                "🌐 Name Service\n    Domain: {}.sol\n    Owner: {}",
                ns.domain_name, ns.owner
            )
        }
        DecodedAccount::CandyMachine(cm) => {
            format!(
                "🍬 Candy Machine\n    Available: {}\n    Redeemed: {}\n    Price: {} lamports",
                cm.items_available, cm.items_redeemed, cm.price
            )
        }
        DecodedAccount::RaydiumAmm(ray) => {
            format!(
                "🌊 Raydium AMM\n    LP Mint: {}\n    Status: {}",
                ray.lp_mint, ray.status
            )
        }
        DecodedAccount::OrcaWhirlpool(orca) => {
            format!(
                "🐋 Orca Whirlpool\n    Token A: {}\n    Token B: {}\n    Liquidity: {}",
                orca.token_mint_a, orca.token_mint_b, orca.liquidity
            )
        }
        DecodedAccount::JupiterAggregator(_) => "🪐 Jupiter Aggregator v6".to_string(),
        DecodedAccount::MarinadeFinance(m) => format!(
            "⚓ Marinade Finance\n    mSOL Mint: {}\n    Total Lamports: {}",
            m.msol_mint, m.total_lamports_under_control
        ),
        DecodedAccount::LidoSolana(l) => format!(
            "🏔️  Lido Solana\n    stSOL Mint: {}\n    Total Lamports: {}",
            l.st_sol_mint, l.total_lamports
        ),
        DecodedAccount::Token2022(t) => format!(
            "💎 SPL Token-2022\n    Mint: {}\n    Owner: {}\n    Amount: {}",
            t.mint, t.owner, t.amount
        ),
        DecodedAccount::MetaplexBubblegum(b) => format!(
            "🌳 Metaplex Bubblegum (cNFT)\n    Max Depth: {}\n    Buffer Size: {}",
            b.max_depth, b.max_buffer_size
        ),
        DecodedAccount::PhoenixV1(p) => format!(
            "🔥 Phoenix DEX\n    Base: {}\n    Quote: {}",
            p.base_mint, p.quote_mint
        ),
        DecodedAccount::OpenbookV2(o) => format!(
            "📖 OpenBook v2\n    Base: {}\n    Quote: {}",
            o.base_mint, o.quote_mint
        ),
        DecodedAccount::TensorSwap(_) => "⚡ Tensor Swap".to_string(),
        DecodedAccount::MagicEdenV2(m) => {
            format!("✨ Magic Eden v2\n    Collection: {}", m.collection)
        }
        DecodedAccount::JupiterLimitOrder(j) => format!(
            "⏱️  Jupiter Limit Order\n    Maker: {}\n    In: {} → Out: {}",
            j.maker, j.input_mint, j.output_mint
        ),
        DecodedAccount::DriftV2(d) => format!(
            "🌊 Drift v2\n    User: {}\n    Authority: {}",
            d.user, d.authority
        ),
        DecodedAccount::MangoV4(m) => format!(
            "🥭 Mango Markets v4\n    Group: {}\n    Owner: {}",
            m.group, m.owner
        ),
        DecodedAccount::KaminoLending(k) => format!(
            "💧 Kamino Lending\n    Reserve: {}\n    Market: {}",
            k.reserve, k.market
        ),
        DecodedAccount::SplStakePool(s) => format!(
            "🏊 SPL Stake Pool\n    Pool Mint: {}\n    Total Lamports: {}",
            s.pool_mint, s.total_lamports
        ),
        DecodedAccount::LifinityAmm(_) => "♾️  Lifinity AMM".to_string(),
        DecodedAccount::MeteoraPools(m) => format!(
            "☄️  Meteora Pools\n    Type: {}\n    Token A: {}\n    Token B: {}",
            m.pool_type, m.token_a_mint, m.token_b_mint
        ),
        DecodedAccount::SquadsV3(s) => format!(
            "🛡️  Squads v3 (Multisig)\n    Threshold: {}\n    Members: {}",
            s.threshold,
            s.members.len()
        ),
        DecodedAccount::Unknown => "".to_string(),
    }
}
