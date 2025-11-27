//! Agent Escrow & Slashing - Economic accountability for BBS agents
//!
//! This program creates real economic skin-in-the-game for agents:
//! - Agents stake SOL when accepting tasks
//! - Successful deliveries return stake + reward
//! - Failed deliveries trigger slashing (stake forfeiture)
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────┐
//! │                    AGENT ESCROW SYSTEM                          │
//! │                                                                 │
//! │  1. Agent Stakes SOL    2. Task Assignment    3. Resolution     │
//! │  ┌──────────┐          ┌──────────┐          ┌──────────┐      │
//! │  │  Agent   │─stake───▶│  Escrow  │◀─assign──│ Requester│      │
//! │  │ Wallet   │          │   PDA    │          │  Wallet  │      │
//! │  └──────────┘          └─────┬────┘          └──────────┘      │
//! │                              │                                  │
//! │                       ┌──────┴──────┐                          │
//! │                       │  Outcome?   │                          │
//! │                       └──────┬──────┘                          │
//! │                     ┌────────┴────────┐                        │
//! │               SUCCESS               FAILURE                     │
//! │                 │                     │                        │
//! │            stake + reward        slash stake                   │
//! │               ▼                       ▼                        │
//! │           Agent gets             Requester gets                │
//! │          payment back           slashed amount                 │
//! └─────────────────────────────────────────────────────────────────┘
//! ```
//!
//! ## Account Types
//!
//! - **AgentStake**: Agent's staked SOL account (PDA: ["agent_stake", agent_pubkey])
//! - **TaskEscrow**: Individual task escrow (PDA: ["task_escrow", task_id])
//! - **EscrowConfig**: Global configuration (PDA: ["escrow_config"])

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

// Agent Escrow Program ID (placeholder - update after deployment)
solana_program::declare_id!("Escrow1111111111111111111111111111111111111");

// ============================================
// Constants
// ============================================

/// Account discriminators
pub const AGENT_STAKE_DISCRIMINATOR: [u8; 8] = *b"AGTSTAKE";
pub const TASK_ESCROW_DISCRIMINATOR: [u8; 8] = *b"TASKESCR";
pub const CONFIG_DISCRIMINATOR: [u8; 8] = *b"ESCRCONF";

/// PDA seeds
pub const AGENT_STAKE_SEED: &[u8] = b"agent_stake";
pub const TASK_ESCROW_SEED: &[u8] = b"task_escrow";
pub const CONFIG_SEED: &[u8] = b"escrow_config";
pub const VAULT_SEED: &[u8] = b"escrow_vault";

/// Maximum capabilities string length
pub const MAX_CAPABILITIES_LEN: usize = 128;

/// Maximum task description length
pub const MAX_TASK_DESC_LEN: usize = 256;

/// Minimum stake in lamports (0.01 SOL)
pub const MIN_STAKE_LAMPORTS: u64 = 10_000_000;

/// Default slashing percentage (50% = 5000 basis points)
pub const DEFAULT_SLASH_BPS: u16 = 5000;

/// Timeout for task completion (24 hours in seconds)
pub const DEFAULT_TASK_TIMEOUT: i64 = 86400;

// Account sizes
pub const AGENT_STAKE_SIZE: usize = 8 + 32 + 8 + MAX_CAPABILITIES_LEN + 8 + 8 + 4 + 4 + 1;
// discriminator(8) + owner(32) + staked_amount(8) + capabilities(128) +
// total_tasks(8) + successful_tasks(8) + reputation_score(4) + tasks_in_progress(4) + is_active(1) = 233

pub const TASK_ESCROW_SIZE: usize = 8 + 8 + 32 + 32 + 8 + 8 + MAX_TASK_DESC_LEN + 8 + 8 + 1 + 8;
// discriminator(8) + task_id(8) + requester(32) + agent(32) + stake_amount(8) + reward_amount(8) +
// description(256) + deadline(8) + created_at(8) + status(1) + result_hash(8) = 377

pub const CONFIG_SIZE: usize = 8 + 32 + 8 + 2 + 8 + 1;
// discriminator(8) + authority(32) + min_stake(8) + slash_bps(2) + task_timeout(8) + is_initialized(1) = 59

// ============================================
// Task Status
// ============================================

#[derive(BorshSerialize, BorshDeserialize, Debug, Clone, Copy, PartialEq)]
#[borsh(use_discriminant = true)]
#[repr(u8)]
pub enum TaskStatus {
    /// Task created, awaiting agent acceptance
    Pending = 0,
    /// Agent accepted, work in progress
    InProgress = 1,
    /// Agent submitted result, awaiting confirmation
    Submitted = 2,
    /// Requester confirmed successful delivery
    Completed = 3,
    /// Delivery failed, stake slashed
    Failed = 4,
    /// Task timed out, stake slashed
    TimedOut = 5,
    /// Task cancelled by requester (before acceptance)
    Cancelled = 6,
}

impl TryFrom<u8> for TaskStatus {
    type Error = ProgramError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(TaskStatus::Pending),
            1 => Ok(TaskStatus::InProgress),
            2 => Ok(TaskStatus::Submitted),
            3 => Ok(TaskStatus::Completed),
            4 => Ok(TaskStatus::Failed),
            5 => Ok(TaskStatus::TimedOut),
            6 => Ok(TaskStatus::Cancelled),
            _ => Err(ProgramError::InvalidAccountData),
        }
    }
}

// ============================================
// Account Structures
// ============================================

/// Agent's staked account - represents an agent's stake and reputation
#[derive(BorshSerialize, BorshDeserialize, Debug, Clone)]
pub struct AgentStake {
    /// Discriminator: "AGTSTAKE"
    pub discriminator: [u8; 8],

    /// Agent's wallet pubkey (owner)
    pub owner: Pubkey,

    /// Amount of SOL staked (in lamports)
    pub staked_amount: u64,

    /// Agent capabilities (comma-separated: "research,coding,analysis")
    pub capabilities: [u8; MAX_CAPABILITIES_LEN],

    /// Total tasks completed (successful + failed)
    pub total_tasks: u64,

    /// Successfully completed tasks
    pub successful_tasks: u64,

    /// Reputation score (0-10000 basis points, starts at 5000)
    pub reputation_score: u32,

    /// Current tasks in progress
    pub tasks_in_progress: u32,

    /// Whether the agent is actively accepting tasks
    pub is_active: bool,
}

impl AgentStake {
    pub fn new(owner: Pubkey, staked_amount: u64, capabilities: &str) -> Self {
        let mut cap_bytes = [0u8; MAX_CAPABILITIES_LEN];
        let cap_slice = capabilities.as_bytes();
        let cap_len = cap_slice.len().min(MAX_CAPABILITIES_LEN);
        cap_bytes[..cap_len].copy_from_slice(&cap_slice[..cap_len]);

        Self {
            discriminator: AGENT_STAKE_DISCRIMINATOR,
            owner,
            staked_amount,
            capabilities: cap_bytes,
            total_tasks: 0,
            successful_tasks: 0,
            reputation_score: 5000, // Start at 50%
            tasks_in_progress: 0,
            is_active: true,
        }
    }

    /// Get capabilities as string
    pub fn get_capabilities(&self) -> String {
        let end = self.capabilities.iter().position(|&b| b == 0).unwrap_or(MAX_CAPABILITIES_LEN);
        String::from_utf8_lossy(&self.capabilities[..end]).to_string()
    }

    /// Calculate success rate in basis points
    pub fn success_rate_bps(&self) -> u32 {
        if self.total_tasks == 0 {
            return 5000; // 50% for new agents
        }
        ((self.successful_tasks * 10000) / self.total_tasks) as u32
    }

    /// Update reputation after task completion
    pub fn update_reputation(&mut self, success: bool) {
        self.total_tasks += 1;
        if success {
            self.successful_tasks += 1;
            // Increase reputation (capped at 10000)
            self.reputation_score = self.reputation_score.saturating_add(100).min(10000);
        } else {
            // Decrease reputation more heavily for failures
            self.reputation_score = self.reputation_score.saturating_sub(300);
        }
    }
}

/// Individual task escrow
#[derive(BorshSerialize, BorshDeserialize, Debug, Clone)]
pub struct TaskEscrow {
    /// Discriminator: "TASKESCR"
    pub discriminator: [u8; 8],

    /// Unique task ID
    pub task_id: u64,

    /// Requester who created the task
    pub requester: Pubkey,

    /// Agent assigned to the task (zero if pending)
    pub agent: Pubkey,

    /// Amount agent must stake for this task
    pub stake_amount: u64,

    /// Reward for successful completion (paid by requester)
    pub reward_amount: u64,

    /// Task description
    pub description: [u8; MAX_TASK_DESC_LEN],

    /// Deadline timestamp
    pub deadline: i64,

    /// Creation timestamp
    pub created_at: i64,

    /// Task status
    pub status: u8, // TaskStatus as u8

    /// Hash of submitted result (for verification)
    pub result_hash: u64,
}

impl TaskEscrow {
    pub fn new(task_id: u64, requester: Pubkey, stake_amount: u64, reward_amount: u64,
               description: &str, deadline: i64, now: i64) -> Self {
        let mut desc_bytes = [0u8; MAX_TASK_DESC_LEN];
        let desc_slice = description.as_bytes();
        let desc_len = desc_slice.len().min(MAX_TASK_DESC_LEN);
        desc_bytes[..desc_len].copy_from_slice(&desc_slice[..desc_len]);

        Self {
            discriminator: TASK_ESCROW_DISCRIMINATOR,
            task_id,
            requester,
            agent: Pubkey::default(),
            stake_amount,
            reward_amount,
            description: desc_bytes,
            deadline,
            created_at: now,
            status: TaskStatus::Pending as u8,
            result_hash: 0,
        }
    }

    pub fn get_description(&self) -> String {
        let end = self.description.iter().position(|&b| b == 0).unwrap_or(MAX_TASK_DESC_LEN);
        String::from_utf8_lossy(&self.description[..end]).to_string()
    }

    pub fn get_status(&self) -> TaskStatus {
        TaskStatus::try_from(self.status).unwrap_or(TaskStatus::Pending)
    }
}

/// Global escrow configuration
#[derive(BorshSerialize, BorshDeserialize, Debug, Clone)]
pub struct EscrowConfig {
    /// Discriminator: "ESCRCONF"
    pub discriminator: [u8; 8],

    /// Program authority (can update config)
    pub authority: Pubkey,

    /// Minimum stake required
    pub min_stake: u64,

    /// Slashing percentage in basis points (10000 = 100%)
    pub slash_bps: u16,

    /// Default task timeout
    pub task_timeout: i64,

    /// Whether config is initialized
    pub is_initialized: bool,
}

impl EscrowConfig {
    pub fn new(authority: Pubkey) -> Self {
        Self {
            discriminator: CONFIG_DISCRIMINATOR,
            authority,
            min_stake: MIN_STAKE_LAMPORTS,
            slash_bps: DEFAULT_SLASH_BPS,
            task_timeout: DEFAULT_TASK_TIMEOUT,
            is_initialized: true,
        }
    }
}

// ============================================
// Instructions
// ============================================

#[derive(BorshSerialize, BorshDeserialize, Debug)]
pub enum EscrowInstruction {
    /// Initialize global config (one-time)
    ///
    /// Accounts:
    /// 0. `[writable, signer]` Authority/Payer
    /// 1. `[writable]` Config PDA
    /// 2. `[]` System program
    InitializeConfig,

    /// Agent registers and stakes SOL
    ///
    /// Accounts:
    /// 0. `[writable, signer]` Agent wallet
    /// 1. `[writable]` Agent stake PDA
    /// 2. `[writable]` Vault PDA (holds staked SOL)
    /// 3. `[]` System program
    ///
    /// Data:
    /// - stake_amount: u64 - Amount to stake
    /// - capabilities: String - Agent capabilities
    RegisterAgent {
        stake_amount: u64,
        capabilities: String,
    },

    /// Add more stake to agent account
    ///
    /// Accounts:
    /// 0. `[writable, signer]` Agent wallet
    /// 1. `[writable]` Agent stake PDA
    /// 2. `[writable]` Vault PDA
    /// 3. `[]` System program
    AddStake {
        amount: u64,
    },

    /// Withdraw stake (only if no tasks in progress)
    ///
    /// Accounts:
    /// 0. `[writable, signer]` Agent wallet
    /// 1. `[writable]` Agent stake PDA
    /// 2. `[writable]` Vault PDA
    WithdrawStake {
        amount: u64,
    },

    /// Requester creates a new task with reward
    ///
    /// Accounts:
    /// 0. `[writable, signer]` Requester wallet
    /// 1. `[writable]` Task escrow PDA
    /// 2. `[writable]` Vault PDA (receives reward deposit)
    /// 3. `[]` System program
    CreateTask {
        task_id: u64,
        stake_required: u64,
        reward: u64,
        description: String,
        deadline: i64,
    },

    /// Agent accepts a task
    ///
    /// Accounts:
    /// 0. `[writable, signer]` Agent wallet
    /// 1. `[writable]` Agent stake PDA
    /// 2. `[writable]` Task escrow PDA
    AcceptTask,

    /// Agent submits task result
    ///
    /// Accounts:
    /// 0. `[signer]` Agent wallet
    /// 1. `[]` Agent stake PDA
    /// 2. `[writable]` Task escrow PDA
    SubmitResult {
        result_hash: u64,
    },

    /// Requester confirms successful delivery
    ///
    /// Accounts:
    /// 0. `[signer]` Requester wallet
    /// 1. `[writable]` Task escrow PDA
    /// 2. `[writable]` Agent stake PDA
    /// 3. `[writable]` Agent wallet (receives stake + reward)
    /// 4. `[writable]` Vault PDA
    ConfirmDelivery,

    /// Requester rejects delivery (triggers slashing)
    ///
    /// Accounts:
    /// 0. `[signer]` Requester wallet
    /// 1. `[writable]` Task escrow PDA
    /// 2. `[writable]` Agent stake PDA
    /// 3. `[writable]` Requester wallet (receives slashed stake)
    /// 4. `[writable]` Agent wallet (receives remaining stake if any)
    /// 5. `[writable]` Vault PDA
    /// 6. `[]` Config PDA (for slash percentage)
    RejectDelivery {
        reason: String,
    },

    /// Anyone can call to timeout an overdue task
    ///
    /// Accounts:
    /// 0. `[]` Caller (anyone can timeout)
    /// 1. `[writable]` Task escrow PDA
    /// 2. `[writable]` Agent stake PDA
    /// 3. `[writable]` Requester wallet (receives slashed stake + reward)
    /// 4. `[writable]` Vault PDA
    /// 5. `[]` Config PDA
    TimeoutTask,

    /// Cancel a pending task (before agent accepts)
    ///
    /// Accounts:
    /// 0. `[signer]` Requester wallet
    /// 1. `[writable]` Task escrow PDA
    /// 2. `[writable]` Requester wallet (receives reward back)
    /// 3. `[writable]` Vault PDA
    CancelTask,
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
    let instruction = EscrowInstruction::try_from_slice(instruction_data)
        .map_err(|_| ProgramError::InvalidInstructionData)?;

    match instruction {
        EscrowInstruction::InitializeConfig => {
            process_initialize_config(program_id, accounts)
        }
        EscrowInstruction::RegisterAgent { stake_amount, capabilities } => {
            process_register_agent(program_id, accounts, stake_amount, &capabilities)
        }
        EscrowInstruction::AddStake { amount } => {
            process_add_stake(program_id, accounts, amount)
        }
        EscrowInstruction::WithdrawStake { amount } => {
            process_withdraw_stake(program_id, accounts, amount)
        }
        EscrowInstruction::CreateTask { task_id, stake_required, reward, description, deadline } => {
            process_create_task(program_id, accounts, task_id, stake_required, reward, &description, deadline)
        }
        EscrowInstruction::AcceptTask => {
            process_accept_task(accounts)
        }
        EscrowInstruction::SubmitResult { result_hash } => {
            process_submit_result(accounts, result_hash)
        }
        EscrowInstruction::ConfirmDelivery => {
            process_confirm_delivery(program_id, accounts)
        }
        EscrowInstruction::RejectDelivery { reason } => {
            process_reject_delivery(program_id, accounts, &reason)
        }
        EscrowInstruction::TimeoutTask => {
            process_timeout_task(program_id, accounts)
        }
        EscrowInstruction::CancelTask => {
            process_cancel_task(program_id, accounts)
        }
    }
}

// ============================================
// PDA Helpers
// ============================================

pub fn get_config_pda(program_id: &Pubkey) -> (Pubkey, u8) {
    Pubkey::find_program_address(&[CONFIG_SEED], program_id)
}

pub fn get_vault_pda(program_id: &Pubkey) -> (Pubkey, u8) {
    Pubkey::find_program_address(&[VAULT_SEED], program_id)
}

pub fn get_agent_stake_pda(agent: &Pubkey, program_id: &Pubkey) -> (Pubkey, u8) {
    Pubkey::find_program_address(&[AGENT_STAKE_SEED, agent.as_ref()], program_id)
}

pub fn get_task_pda(task_id: u64, program_id: &Pubkey) -> (Pubkey, u8) {
    Pubkey::find_program_address(&[TASK_ESCROW_SEED, &task_id.to_le_bytes()], program_id)
}

// ============================================
// Instruction Processors
// ============================================

fn process_initialize_config(
    program_id: &Pubkey,
    accounts: &[AccountInfo],
) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let authority = next_account_info(account_iter)?;
    let config_account = next_account_info(account_iter)?;
    let system_program = next_account_info(account_iter)?;

    if !authority.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    let (expected_pda, bump) = get_config_pda(program_id);
    if *config_account.key != expected_pda {
        return Err(ProgramError::InvalidSeeds);
    }

    if !config_account.data_is_empty() {
        msg!("Config already initialized");
        return Err(ProgramError::AccountAlreadyInitialized);
    }

    let rent = Rent::get()?;
    let lamports = rent.minimum_balance(CONFIG_SIZE);

    let signer_seeds: &[&[u8]] = &[CONFIG_SEED, &[bump]];

    invoke_signed(
        &system_instruction::create_account(
            authority.key,
            config_account.key,
            lamports,
            CONFIG_SIZE as u64,
            program_id,
        ),
        &[authority.clone(), config_account.clone(), system_program.clone()],
        &[signer_seeds],
    )?;

    let config = EscrowConfig::new(*authority.key);
    config.serialize(&mut *config_account.data.borrow_mut())?;

    msg!("Escrow config initialized");
    Ok(())
}

fn process_register_agent(
    program_id: &Pubkey,
    accounts: &[AccountInfo],
    stake_amount: u64,
    capabilities: &str,
) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let agent = next_account_info(account_iter)?;
    let stake_account = next_account_info(account_iter)?;
    let vault_account = next_account_info(account_iter)?;
    let system_program = next_account_info(account_iter)?;

    if !agent.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    // Verify PDAs
    let (expected_stake_pda, stake_bump) = get_agent_stake_pda(agent.key, program_id);
    if *stake_account.key != expected_stake_pda {
        return Err(ProgramError::InvalidSeeds);
    }

    let (expected_vault_pda, _) = get_vault_pda(program_id);
    if *vault_account.key != expected_vault_pda {
        return Err(ProgramError::InvalidSeeds);
    }

    if !stake_account.data_is_empty() {
        msg!("Agent already registered");
        return Err(ProgramError::AccountAlreadyInitialized);
    }

    if stake_amount < MIN_STAKE_LAMPORTS {
        msg!("Stake too low. Minimum: {} lamports", MIN_STAKE_LAMPORTS);
        return Err(ProgramError::InsufficientFunds);
    }

    // Create stake account
    let rent = Rent::get()?;
    let lamports = rent.minimum_balance(AGENT_STAKE_SIZE);

    let signer_seeds: &[&[u8]] = &[AGENT_STAKE_SEED, agent.key.as_ref(), &[stake_bump]];

    invoke_signed(
        &system_instruction::create_account(
            agent.key,
            stake_account.key,
            lamports,
            AGENT_STAKE_SIZE as u64,
            program_id,
        ),
        &[agent.clone(), stake_account.clone(), system_program.clone()],
        &[signer_seeds],
    )?;

    // Transfer stake to vault
    invoke_signed(
        &system_instruction::transfer(agent.key, vault_account.key, stake_amount),
        &[agent.clone(), vault_account.clone(), system_program.clone()],
        &[],
    )?;

    // Initialize stake account
    let agent_stake = AgentStake::new(*agent.key, stake_amount, capabilities);
    agent_stake.serialize(&mut *stake_account.data.borrow_mut())?;

    msg!("Agent registered with {} lamports stake", stake_amount);
    Ok(())
}

fn process_add_stake(
    program_id: &Pubkey,
    accounts: &[AccountInfo],
    amount: u64,
) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let agent = next_account_info(account_iter)?;
    let stake_account = next_account_info(account_iter)?;
    let vault_account = next_account_info(account_iter)?;
    let system_program = next_account_info(account_iter)?;

    if !agent.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    // Verify stake account
    let (expected_pda, _) = get_agent_stake_pda(agent.key, program_id);
    if *stake_account.key != expected_pda {
        return Err(ProgramError::InvalidSeeds);
    }

    let mut agent_stake = AgentStake::try_from_slice(&stake_account.data.borrow())?;

    if agent_stake.discriminator != AGENT_STAKE_DISCRIMINATOR {
        return Err(ProgramError::InvalidAccountData);
    }
    if agent_stake.owner != *agent.key {
        return Err(ProgramError::IllegalOwner);
    }

    // Transfer to vault
    invoke_signed(
        &system_instruction::transfer(agent.key, vault_account.key, amount),
        &[agent.clone(), vault_account.clone(), system_program.clone()],
        &[],
    )?;

    agent_stake.staked_amount = agent_stake.staked_amount.saturating_add(amount);
    agent_stake.serialize(&mut *stake_account.data.borrow_mut())?;

    msg!("Added {} lamports stake. Total: {}", amount, agent_stake.staked_amount);
    Ok(())
}

fn process_withdraw_stake(
    program_id: &Pubkey,
    accounts: &[AccountInfo],
    amount: u64,
) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let agent = next_account_info(account_iter)?;
    let stake_account = next_account_info(account_iter)?;
    let vault_account = next_account_info(account_iter)?;

    if !agent.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    let mut agent_stake = AgentStake::try_from_slice(&stake_account.data.borrow())?;

    if agent_stake.discriminator != AGENT_STAKE_DISCRIMINATOR {
        return Err(ProgramError::InvalidAccountData);
    }
    if agent_stake.owner != *agent.key {
        return Err(ProgramError::IllegalOwner);
    }

    // Cannot withdraw if tasks in progress
    if agent_stake.tasks_in_progress > 0 {
        msg!("Cannot withdraw: {} tasks in progress", agent_stake.tasks_in_progress);
        return Err(ProgramError::InvalidAccountData);
    }

    if amount > agent_stake.staked_amount {
        return Err(ProgramError::InsufficientFunds);
    }

    // Transfer from vault to agent
    let (_, vault_bump) = get_vault_pda(program_id);
    let vault_seeds: &[&[u8]] = &[VAULT_SEED, &[vault_bump]];

    **vault_account.try_borrow_mut_lamports()? -= amount;
    **agent.try_borrow_mut_lamports()? += amount;

    agent_stake.staked_amount = agent_stake.staked_amount.saturating_sub(amount);
    agent_stake.serialize(&mut *stake_account.data.borrow_mut())?;

    msg!("Withdrew {} lamports. Remaining: {}", amount, agent_stake.staked_amount);
    Ok(())
}

fn process_create_task(
    program_id: &Pubkey,
    accounts: &[AccountInfo],
    task_id: u64,
    stake_required: u64,
    reward: u64,
    description: &str,
    deadline: i64,
) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let requester = next_account_info(account_iter)?;
    let task_account = next_account_info(account_iter)?;
    let vault_account = next_account_info(account_iter)?;
    let system_program = next_account_info(account_iter)?;

    if !requester.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    let (expected_task_pda, task_bump) = get_task_pda(task_id, program_id);
    if *task_account.key != expected_task_pda {
        return Err(ProgramError::InvalidSeeds);
    }

    if !task_account.data_is_empty() {
        msg!("Task already exists");
        return Err(ProgramError::AccountAlreadyInitialized);
    }

    let clock = Clock::get()?;
    let now = clock.unix_timestamp;

    // Create task account
    let rent = Rent::get()?;
    let lamports = rent.minimum_balance(TASK_ESCROW_SIZE);

    let signer_seeds: &[&[u8]] = &[TASK_ESCROW_SEED, &task_id.to_le_bytes(), &[task_bump]];

    invoke_signed(
        &system_instruction::create_account(
            requester.key,
            task_account.key,
            lamports,
            TASK_ESCROW_SIZE as u64,
            program_id,
        ),
        &[requester.clone(), task_account.clone(), system_program.clone()],
        &[signer_seeds],
    )?;

    // Transfer reward to vault
    invoke_signed(
        &system_instruction::transfer(requester.key, vault_account.key, reward),
        &[requester.clone(), vault_account.clone(), system_program.clone()],
        &[],
    )?;

    let task = TaskEscrow::new(task_id, *requester.key, stake_required, reward, description, deadline, now);
    task.serialize(&mut *task_account.data.borrow_mut())?;

    msg!("Task {} created. Stake: {}, Reward: {}", task_id, stake_required, reward);
    Ok(())
}

fn process_accept_task(accounts: &[AccountInfo]) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let agent = next_account_info(account_iter)?;
    let stake_account = next_account_info(account_iter)?;
    let task_account = next_account_info(account_iter)?;

    if !agent.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    let mut agent_stake = AgentStake::try_from_slice(&stake_account.data.borrow())?;
    let mut task = TaskEscrow::try_from_slice(&task_account.data.borrow())?;

    // Verify agent owns stake account
    if agent_stake.owner != *agent.key {
        return Err(ProgramError::IllegalOwner);
    }

    // Task must be pending
    if task.get_status() != TaskStatus::Pending {
        msg!("Task not pending");
        return Err(ProgramError::InvalidAccountData);
    }

    // Agent must have enough stake
    let available_stake = agent_stake.staked_amount.saturating_sub(
        (agent_stake.tasks_in_progress as u64) * MIN_STAKE_LAMPORTS
    );
    if available_stake < task.stake_amount {
        msg!("Insufficient stake. Required: {}, Available: {}", task.stake_amount, available_stake);
        return Err(ProgramError::InsufficientFunds);
    }

    // Accept task
    task.agent = *agent.key;
    task.status = TaskStatus::InProgress as u8;
    agent_stake.tasks_in_progress += 1;

    task.serialize(&mut *task_account.data.borrow_mut())?;
    agent_stake.serialize(&mut *stake_account.data.borrow_mut())?;

    msg!("Agent accepted task {}", task.task_id);
    Ok(())
}

fn process_submit_result(accounts: &[AccountInfo], result_hash: u64) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let agent = next_account_info(account_iter)?;
    let _stake_account = next_account_info(account_iter)?;
    let task_account = next_account_info(account_iter)?;

    if !agent.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    let mut task = TaskEscrow::try_from_slice(&task_account.data.borrow())?;

    if task.agent != *agent.key {
        msg!("Not assigned agent");
        return Err(ProgramError::IllegalOwner);
    }

    if task.get_status() != TaskStatus::InProgress {
        msg!("Task not in progress");
        return Err(ProgramError::InvalidAccountData);
    }

    task.result_hash = result_hash;
    task.status = TaskStatus::Submitted as u8;
    task.serialize(&mut *task_account.data.borrow_mut())?;

    msg!("Result submitted for task {}", task.task_id);
    Ok(())
}

fn process_confirm_delivery(program_id: &Pubkey, accounts: &[AccountInfo]) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let requester = next_account_info(account_iter)?;
    let task_account = next_account_info(account_iter)?;
    let stake_account = next_account_info(account_iter)?;
    let agent_wallet = next_account_info(account_iter)?;
    let vault_account = next_account_info(account_iter)?;

    if !requester.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    let mut task = TaskEscrow::try_from_slice(&task_account.data.borrow())?;
    let mut agent_stake = AgentStake::try_from_slice(&stake_account.data.borrow())?;

    if task.requester != *requester.key {
        msg!("Not task requester");
        return Err(ProgramError::IllegalOwner);
    }

    if task.get_status() != TaskStatus::Submitted {
        msg!("Task not submitted");
        return Err(ProgramError::InvalidAccountData);
    }

    if agent_stake.owner != task.agent {
        msg!("Agent mismatch");
        return Err(ProgramError::InvalidAccountData);
    }

    // Transfer reward to agent from vault
    let (_, vault_bump) = get_vault_pda(program_id);

    **vault_account.try_borrow_mut_lamports()? -= task.reward_amount;
    **agent_wallet.try_borrow_mut_lamports()? += task.reward_amount;

    // Update task and agent
    task.status = TaskStatus::Completed as u8;
    agent_stake.tasks_in_progress = agent_stake.tasks_in_progress.saturating_sub(1);
    agent_stake.update_reputation(true);

    task.serialize(&mut *task_account.data.borrow_mut())?;
    agent_stake.serialize(&mut *stake_account.data.borrow_mut())?;

    msg!("Task {} completed! Agent received {} lamports reward", task.task_id, task.reward_amount);
    Ok(())
}

fn process_reject_delivery(program_id: &Pubkey, accounts: &[AccountInfo], reason: &str) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let requester = next_account_info(account_iter)?;
    let task_account = next_account_info(account_iter)?;
    let stake_account = next_account_info(account_iter)?;
    let requester_wallet = next_account_info(account_iter)?;
    let agent_wallet = next_account_info(account_iter)?;
    let vault_account = next_account_info(account_iter)?;
    let config_account = next_account_info(account_iter)?;

    if !requester.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    let mut task = TaskEscrow::try_from_slice(&task_account.data.borrow())?;
    let mut agent_stake = AgentStake::try_from_slice(&stake_account.data.borrow())?;
    let config = EscrowConfig::try_from_slice(&config_account.data.borrow())?;

    if task.requester != *requester.key {
        return Err(ProgramError::IllegalOwner);
    }

    if task.get_status() != TaskStatus::Submitted {
        return Err(ProgramError::InvalidAccountData);
    }

    // Calculate slashing
    let slash_amount = (task.stake_amount * config.slash_bps as u64) / 10000;
    let return_amount = task.stake_amount.saturating_sub(slash_amount);

    // Slash from agent's stake
    agent_stake.staked_amount = agent_stake.staked_amount.saturating_sub(slash_amount);

    // Transfer slashed amount + reward to requester
    let total_to_requester = slash_amount + task.reward_amount;
    **vault_account.try_borrow_mut_lamports()? -= total_to_requester;
    **requester_wallet.try_borrow_mut_lamports()? += total_to_requester;

    // Update task and agent
    task.status = TaskStatus::Failed as u8;
    agent_stake.tasks_in_progress = agent_stake.tasks_in_progress.saturating_sub(1);
    agent_stake.update_reputation(false);

    task.serialize(&mut *task_account.data.borrow_mut())?;
    agent_stake.serialize(&mut *stake_account.data.borrow_mut())?;

    msg!("Task {} FAILED! Agent slashed {} lamports. Reason: {}",
         task.task_id, slash_amount, reason);
    msg!("Agent reputation: {}/10000", agent_stake.reputation_score);

    Ok(())
}

fn process_timeout_task(program_id: &Pubkey, accounts: &[AccountInfo]) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let _caller = next_account_info(account_iter)?;
    let task_account = next_account_info(account_iter)?;
    let stake_account = next_account_info(account_iter)?;
    let requester_wallet = next_account_info(account_iter)?;
    let vault_account = next_account_info(account_iter)?;
    let config_account = next_account_info(account_iter)?;

    let mut task = TaskEscrow::try_from_slice(&task_account.data.borrow())?;
    let config = EscrowConfig::try_from_slice(&config_account.data.borrow())?;

    let clock = Clock::get()?;
    let now = clock.unix_timestamp;

    // Check if task is timed out
    if now <= task.deadline {
        msg!("Task not yet timed out. Deadline: {}, Now: {}", task.deadline, now);
        return Err(ProgramError::InvalidArgument);
    }

    // Only in-progress or submitted tasks can timeout
    if task.get_status() != TaskStatus::InProgress && task.get_status() != TaskStatus::Submitted {
        msg!("Task not in progress or submitted");
        return Err(ProgramError::InvalidAccountData);
    }

    // If agent was assigned, slash their stake
    if task.agent != Pubkey::default() {
        let mut agent_stake = AgentStake::try_from_slice(&stake_account.data.borrow())?;

        let slash_amount = (task.stake_amount * config.slash_bps as u64) / 10000;
        agent_stake.staked_amount = agent_stake.staked_amount.saturating_sub(slash_amount);
        agent_stake.tasks_in_progress = agent_stake.tasks_in_progress.saturating_sub(1);
        agent_stake.update_reputation(false);

        // Transfer slashed amount + reward to requester
        let total_to_requester = slash_amount + task.reward_amount;
        **vault_account.try_borrow_mut_lamports()? -= total_to_requester;
        **requester_wallet.try_borrow_mut_lamports()? += total_to_requester;

        agent_stake.serialize(&mut *stake_account.data.borrow_mut())?;

        msg!("Task {} TIMED OUT! Agent slashed {} lamports", task.task_id, slash_amount);
    } else {
        // No agent, just return reward
        **vault_account.try_borrow_mut_lamports()? -= task.reward_amount;
        **requester_wallet.try_borrow_mut_lamports()? += task.reward_amount;

        msg!("Task {} timed out with no agent. Reward returned.", task.task_id);
    }

    task.status = TaskStatus::TimedOut as u8;
    task.serialize(&mut *task_account.data.borrow_mut())?;

    Ok(())
}

fn process_cancel_task(program_id: &Pubkey, accounts: &[AccountInfo]) -> ProgramResult {
    let account_iter = &mut accounts.iter();

    let requester = next_account_info(account_iter)?;
    let task_account = next_account_info(account_iter)?;
    let requester_wallet = next_account_info(account_iter)?;
    let vault_account = next_account_info(account_iter)?;

    if !requester.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    let mut task = TaskEscrow::try_from_slice(&task_account.data.borrow())?;

    if task.requester != *requester.key {
        return Err(ProgramError::IllegalOwner);
    }

    // Can only cancel pending tasks
    if task.get_status() != TaskStatus::Pending {
        msg!("Cannot cancel: task already accepted");
        return Err(ProgramError::InvalidAccountData);
    }

    // Return reward to requester
    **vault_account.try_borrow_mut_lamports()? -= task.reward_amount;
    **requester_wallet.try_borrow_mut_lamports()? += task.reward_amount;

    task.status = TaskStatus::Cancelled as u8;
    task.serialize(&mut *task_account.data.borrow_mut())?;

    msg!("Task {} cancelled. Reward returned.", task.task_id);
    Ok(())
}

// ============================================
// Client Helpers
// ============================================

#[cfg(not(target_os = "solana"))]
pub mod client {
    use super::*;
    use solana_program::instruction::{AccountMeta, Instruction};

    pub fn register_agent(
        program_id: &Pubkey,
        agent: &Pubkey,
        stake_amount: u64,
        capabilities: String,
    ) -> Instruction {
        let (stake_pda, _) = get_agent_stake_pda(agent, program_id);
        let (vault_pda, _) = get_vault_pda(program_id);

        Instruction {
            program_id: *program_id,
            accounts: vec![
                AccountMeta::new(*agent, true),
                AccountMeta::new(stake_pda, false),
                AccountMeta::new(vault_pda, false),
                AccountMeta::new_readonly(solana_program::system_program::ID, false),
            ],
            data: borsh::to_vec(&EscrowInstruction::RegisterAgent { stake_amount, capabilities }).unwrap(),
        }
    }

    pub fn create_task(
        program_id: &Pubkey,
        requester: &Pubkey,
        task_id: u64,
        stake_required: u64,
        reward: u64,
        description: String,
        deadline: i64,
    ) -> Instruction {
        let (task_pda, _) = get_task_pda(task_id, program_id);
        let (vault_pda, _) = get_vault_pda(program_id);

        Instruction {
            program_id: *program_id,
            accounts: vec![
                AccountMeta::new(*requester, true),
                AccountMeta::new(task_pda, false),
                AccountMeta::new(vault_pda, false),
                AccountMeta::new_readonly(solana_program::system_program::ID, false),
            ],
            data: borsh::to_vec(&EscrowInstruction::CreateTask {
                task_id,
                stake_required,
                reward,
                description,
                deadline,
            }).unwrap(),
        }
    }

    pub fn accept_task(
        program_id: &Pubkey,
        agent: &Pubkey,
        task_id: u64,
    ) -> Instruction {
        let (stake_pda, _) = get_agent_stake_pda(agent, program_id);
        let (task_pda, _) = get_task_pda(task_id, program_id);

        Instruction {
            program_id: *program_id,
            accounts: vec![
                AccountMeta::new(*agent, true),
                AccountMeta::new(stake_pda, false),
                AccountMeta::new(task_pda, false),
            ],
            data: borsh::to_vec(&EscrowInstruction::AcceptTask).unwrap(),
        }
    }

    pub fn confirm_delivery(
        program_id: &Pubkey,
        requester: &Pubkey,
        agent: &Pubkey,
        task_id: u64,
    ) -> Instruction {
        let (task_pda, _) = get_task_pda(task_id, program_id);
        let (stake_pda, _) = get_agent_stake_pda(agent, program_id);
        let (vault_pda, _) = get_vault_pda(program_id);

        Instruction {
            program_id: *program_id,
            accounts: vec![
                AccountMeta::new_readonly(*requester, true),
                AccountMeta::new(task_pda, false),
                AccountMeta::new(stake_pda, false),
                AccountMeta::new(*agent, false),
                AccountMeta::new(vault_pda, false),
            ],
            data: borsh::to_vec(&EscrowInstruction::ConfirmDelivery).unwrap(),
        }
    }

    pub fn reject_delivery(
        program_id: &Pubkey,
        requester: &Pubkey,
        agent: &Pubkey,
        task_id: u64,
        reason: String,
    ) -> Instruction {
        let (task_pda, _) = get_task_pda(task_id, program_id);
        let (stake_pda, _) = get_agent_stake_pda(agent, program_id);
        let (vault_pda, _) = get_vault_pda(program_id);
        let (config_pda, _) = get_config_pda(program_id);

        Instruction {
            program_id: *program_id,
            accounts: vec![
                AccountMeta::new_readonly(*requester, true),
                AccountMeta::new(task_pda, false),
                AccountMeta::new(stake_pda, false),
                AccountMeta::new(*requester, false),
                AccountMeta::new(*agent, false),
                AccountMeta::new(vault_pda, false),
                AccountMeta::new_readonly(config_pda, false),
            ],
            data: borsh::to_vec(&EscrowInstruction::RejectDelivery { reason }).unwrap(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_agent_stake_serialization() {
        let owner = Pubkey::new_unique();
        let stake = AgentStake::new(owner, 1_000_000_000, "research,coding");

        assert_eq!(stake.discriminator, AGENT_STAKE_DISCRIMINATOR);
        assert_eq!(stake.staked_amount, 1_000_000_000);
        assert_eq!(stake.get_capabilities(), "research,coding");
        assert_eq!(stake.reputation_score, 5000);
    }

    #[test]
    fn test_reputation_update() {
        let owner = Pubkey::new_unique();
        let mut stake = AgentStake::new(owner, 1_000_000_000, "test");

        stake.update_reputation(true);
        assert_eq!(stake.successful_tasks, 1);
        assert_eq!(stake.total_tasks, 1);
        assert_eq!(stake.reputation_score, 5100);

        stake.update_reputation(false);
        assert_eq!(stake.successful_tasks, 1);
        assert_eq!(stake.total_tasks, 2);
        assert_eq!(stake.reputation_score, 4800); // -300 for failure
    }

    #[test]
    fn test_task_creation() {
        let requester = Pubkey::new_unique();
        let task = TaskEscrow::new(
            1,
            requester,
            50_000_000,
            100_000_000,
            "Complete research task",
            1700000000,
            1699900000,
        );

        assert_eq!(task.discriminator, TASK_ESCROW_DISCRIMINATOR);
        assert_eq!(task.task_id, 1);
        assert_eq!(task.stake_amount, 50_000_000);
        assert_eq!(task.reward_amount, 100_000_000);
        assert_eq!(task.get_description(), "Complete research task");
        assert_eq!(task.get_status(), TaskStatus::Pending);
    }
}
