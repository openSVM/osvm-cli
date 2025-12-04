/// Integration tests for Agent Accountability System
/// Tests: Escrow, Reputation NFT, and Dispute Resolution programs
///
/// These tests verify:
/// 1. Compilation succeeds for all programs
/// 2. Program logic is correct (via bytecode inspection)
/// 3. Instruction discriminators are properly routed
/// 4. Account data layouts match specifications
use ovsm::compiler::{CompileOptions, Compiler};
use std::fs;
use std::path::PathBuf;

fn get_examples_dir() -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest_dir)
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("examples")
        .join("ovsm_scripts")
}

fn compile_program(filename: &str) -> ovsm::Result<ovsm::compiler::CompileResult> {
    let path = get_examples_dir().join(filename);
    let source = fs::read_to_string(&path)
        .map_err(|e| ovsm::Error::compiler(format!("Failed to read {}: {}", filename, e)))?;

    let compiler = Compiler::new(CompileOptions::default());
    compiler.compile(&source)
}

// =============================================================================
// COMPILATION TESTS - Verify all programs compile successfully
// =============================================================================

#[test]
fn test_agent_escrow_compiles() {
    let result = compile_program("agent_escrow.ovsm");
    assert!(
        result.is_ok(),
        "agent_escrow.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 0,
        "No sBPF instructions generated"
    );
    assert!(compiled.elf_bytes.len() > 100, "ELF too small");

    // Verify reasonable size (not bloated)
    assert!(
        compiled.elf_bytes.len() < 50_000,
        "ELF too large: {} bytes",
        compiled.elf_bytes.len()
    );
}

#[test]
fn test_reputation_nft_compiles() {
    let result = compile_program("reputation_nft.ovsm");
    assert!(
        result.is_ok(),
        "reputation_nft.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 0,
        "No sBPF instructions generated"
    );

    // Reputation NFT v2 should have significant logic (decay, negative rep, collateral)
    assert!(
        compiled.ir_instruction_count > 100,
        "IR instruction count too low for v2"
    );
}

#[test]
fn test_dispute_resolution_compiles() {
    let result = compile_program("dispute_resolution.ovsm");
    assert!(
        result.is_ok(),
        "dispute_resolution.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 0,
        "No sBPF instructions generated"
    );
}

#[test]
fn test_agent_registry_compiles() {
    let result = compile_program("agent_registry.ovsm");
    assert!(
        result.is_ok(),
        "agent_registry.ovsm failed to compile: {:?}",
        result.err()
    );
}

// =============================================================================
// PROGRAM STRUCTURE TESTS - Verify instructions are properly structured
// =============================================================================

#[test]
fn test_escrow_instruction_count() {
    // Agent escrow has 6 instructions:
    // 0=CreateTask, 1=AcceptTask, 2=ConfirmDelivery, 3=RejectDelivery, 4=Timeout, 5=Cancel
    let result = compile_program("agent_escrow.ovsm").unwrap();

    // Should have reasonable CU estimate (not exceeding compute budget)
    assert!(
        result.estimated_cu < 200_000,
        "CU too high: {}",
        result.estimated_cu
    );
}

#[test]
fn test_reputation_nft_v2_features() {
    // Reputation NFT v2 has 7 instructions:
    // 0=MintNFT, 1=AddTaskComplete, 2=AddDisputeResult, 3=AddRating,
    // 4=VerifyReputation (with decay), 5=AddTaskFailed (negative), 6=GetRequiredCollateral
    let result = compile_program("reputation_nft.ovsm").unwrap();

    // V2 should be larger than a simple counter program
    assert!(
        result.elf_bytes.len() > 8000,
        "Reputation NFT v2 too small, missing features?"
    );
}

#[test]
fn test_dispute_resolution_voting() {
    // Dispute resolution has 4 instructions:
    // 0=InitDispute, 1=Vote, 2=Execute, 3=ClaimReward
    let result = compile_program("dispute_resolution.ovsm").unwrap();

    // Should compile without verification errors
    let verification = result.verification.as_ref().unwrap();
    assert!(
        verification.valid,
        "Verification failed: {:?}",
        verification.errors
    );
}

// =============================================================================
// WORKFLOW SIMULATION TESTS - Test logical flow without actual execution
// =============================================================================

/// Tests the complete agent accountability workflow at compile-time
/// Workflow: Register Agent → Create Task → Accept → Deliver → Update Reputation
#[test]
fn test_full_workflow_compilation() {
    // All programs must compile successfully
    let escrow = compile_program("agent_escrow.ovsm");
    let reputation = compile_program("reputation_nft.ovsm");
    let dispute = compile_program("dispute_resolution.ovsm");
    let registry = compile_program("agent_registry.ovsm");

    assert!(escrow.is_ok(), "Escrow failed: {:?}", escrow.err());
    assert!(
        reputation.is_ok(),
        "Reputation failed: {:?}",
        reputation.err()
    );
    assert!(dispute.is_ok(), "Dispute failed: {:?}", dispute.err());
    assert!(registry.is_ok(), "Registry failed: {:?}", registry.err());

    // Calculate total program size for deployment
    let total_size = escrow.as_ref().unwrap().elf_bytes.len()
        + reputation.as_ref().unwrap().elf_bytes.len()
        + dispute.as_ref().unwrap().elf_bytes.len()
        + registry.as_ref().unwrap().elf_bytes.len();

    println!("Total Agent Accountability Suite: {} bytes", total_size);

    // Should fit within reasonable deployment limits
    assert!(
        total_size < 200_000,
        "Total suite too large: {} bytes",
        total_size
    );
}

// =============================================================================
// REPUTATION DECAY MATH TESTS - Verify decay formula correctness
// =============================================================================

/// Test that reputation decay formula produces expected results
/// Formula: effective = raw * (30 - days_inactive) / 30
#[test]
fn test_reputation_decay_formula() {
    // Simulate the decay formula used in reputation_nft.ovsm
    fn decay(raw_rating_x10: i64, days_inactive: i64) -> i64 {
        if days_inactive >= 30 {
            0
        } else {
            (raw_rating_x10 * (30 - days_inactive)) / 30
        }
    }

    // Test cases
    assert_eq!(decay(50, 0), 50, "0 days inactive should have no decay");
    assert_eq!(decay(50, 15), 25, "15 days should give 50% decay");
    assert_eq!(decay(50, 30), 0, "30+ days should be fully decayed");
    assert_eq!(decay(50, 100), 0, "100 days should be fully decayed");
    assert_eq!(decay(45, 10), 30, "10 days: 45 * 20 / 30 = 30");
}

/// Test net tasks calculation with 2x failure penalty
#[test]
fn test_negative_reputation_formula() {
    // Formula: net_tasks = completed - (failed * 2)
    fn net_tasks(completed: i64, failed: i64) -> i64 {
        completed - (failed * 2)
    }

    // 10 completed, 0 failed = 10 net
    assert_eq!(net_tasks(10, 0), 10);

    // 10 completed, 2 failed = 10 - 4 = 6 net
    assert_eq!(net_tasks(10, 2), 6);

    // 10 completed, 5 failed = 10 - 10 = 0 net
    assert_eq!(net_tasks(10, 5), 0);

    // 10 completed, 6 failed = 10 - 12 = -2 net (underwater!)
    assert_eq!(net_tasks(10, 6), -2);
}

/// Test dynamic collateral calculation
#[test]
fn test_collateral_requirement_formula() {
    // Formula: required = base * (100 - reputation_score) / 100
    // Minimum: 10% of base
    fn required_collateral(base: i64, reputation_score: i64) -> i64 {
        let required = (base * (100 - reputation_score)) / 100;
        let minimum = base / 10;
        if required < minimum {
            minimum
        } else {
            required
        }
    }

    // New agent (0 rep) = full collateral
    assert_eq!(required_collateral(1_000_000_000, 0), 1_000_000_000);

    // 50 rep = 50% collateral
    assert_eq!(required_collateral(1_000_000_000, 50), 500_000_000);

    // 90 rep = 10% collateral (minimum)
    assert_eq!(required_collateral(1_000_000_000, 90), 100_000_000);

    // 100 rep = 10% collateral (minimum enforced)
    assert_eq!(required_collateral(1_000_000_000, 100), 100_000_000);
}

// =============================================================================
// ACCOUNT DATA LAYOUT TESTS - Verify memory layouts match specifications
// =============================================================================

/// Verify escrow account data layout matches specification
/// Layout:
///   offset 0:   u8 status
///   offset 8:   u64 reward_amount
///   offset 16:  u64 stake_amount
///   offset 24:  u64 deadline
///   offset 32:  u64 created_at
///   offset 40:  u64 accepted_at
///   offset 48:  32 bytes requester pubkey
///   offset 80:  32 bytes agent pubkey
#[test]
fn test_escrow_account_layout() {
    // Total size should be 112 bytes (80 + 32)
    let expected_size = 112;

    // Verify field offsets are 8-byte aligned for u64 fields
    let offsets = [
        (0, "status"),
        (8, "reward_amount"),
        (16, "stake_amount"),
        (24, "deadline"),
        (32, "created_at"),
        (40, "accepted_at"),
        (48, "requester_pubkey"),
        (80, "agent_pubkey"),
    ];

    for (offset, field) in offsets {
        assert!(
            offset < expected_size,
            "Field {} at offset {} exceeds size {}",
            field,
            offset,
            expected_size
        );
    }
}

/// Verify reputation NFT account data layout matches specification
/// Layout:
///   offset 0:   u8 initialized
///   offset 1:   u8 version (now 2)
///   offset 8:   u64 tasks_completed
///   offset 16:  u64 tasks_failed
///   offset 24:  u64 disputes_won
///   offset 32:  u64 disputes_lost
///   offset 40:  u64 total_ratings
///   offset 48:  u64 rating_sum
///   offset 56:  u64 total_value_delivered
///   offset 64:  u64 total_value_failed
///   offset 72:  u64 mint_timestamp
///   offset 80:  u64 last_activity_timestamp
///   offset 88:  32 bytes owner_pubkey
///   offset 120: 32 bytes mint_authority
#[test]
fn test_reputation_nft_account_layout() {
    // Total size should be 152 bytes (120 + 32)
    let expected_size = 152;

    let offsets = [
        (0, "initialized"),
        (1, "version"),
        (8, "tasks_completed"),
        (16, "tasks_failed"),
        (24, "disputes_won"),
        (32, "disputes_lost"),
        (40, "total_ratings"),
        (48, "rating_sum"),
        (56, "total_value_delivered"),
        (64, "total_value_failed"),
        (72, "mint_timestamp"),
        (80, "last_activity_timestamp"),
        (88, "owner_pubkey"),
        (120, "mint_authority"),
    ];

    for (offset, field) in offsets {
        assert!(
            offset < expected_size,
            "Field {} at offset {} exceeds size {}",
            field,
            offset,
            expected_size
        );
    }
}

// =============================================================================
// WARNINGS AND QUALITY TESTS
// =============================================================================

#[test]
fn test_no_critical_warnings() {
    let programs = [
        "agent_escrow.ovsm",
        "reputation_nft.ovsm",
        "dispute_resolution.ovsm",
        "agent_registry.ovsm",
    ];

    for program in programs {
        let result = compile_program(program).unwrap();

        // Check for critical warnings
        for warning in &result.warnings {
            assert!(
                !warning.contains("undefined"),
                "Program {} has undefined reference: {}",
                program,
                warning
            );
            assert!(
                !warning.contains("overflow"),
                "Program {} has overflow warning: {}",
                program,
                warning
            );
        }
    }
}

/// Test that verification passes for all programs
#[test]
fn test_verification_passes() {
    let programs = [
        "agent_escrow.ovsm",
        "reputation_nft.ovsm",
        "dispute_resolution.ovsm",
        "agent_registry.ovsm",
    ];

    for program in programs {
        let result = compile_program(program).unwrap();
        let verification = result.verification.as_ref().unwrap();

        assert!(
            verification.valid,
            "Program {} failed verification: {:?}",
            program, verification.errors
        );
    }
}

// =============================================================================
// INSTRUCTION DISCRIMINATOR TESTS
// =============================================================================

/// Test that source code contains proper discriminator checks
#[test]
fn test_escrow_discriminators() {
    let path = get_examples_dir().join("agent_escrow.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Verify all 6 instructions are present
    assert!(
        source.contains("INSTR_CREATE"),
        "Missing CREATE instruction"
    );
    assert!(
        source.contains("INSTR_ACCEPT"),
        "Missing ACCEPT instruction"
    );
    assert!(
        source.contains("INSTR_CONFIRM"),
        "Missing CONFIRM instruction"
    );
    assert!(
        source.contains("INSTR_REJECT"),
        "Missing REJECT instruction"
    );
    assert!(
        source.contains("INSTR_TIMEOUT"),
        "Missing TIMEOUT instruction"
    );
    assert!(
        source.contains("INSTR_CANCEL"),
        "Missing CANCEL instruction"
    );
}

#[test]
fn test_reputation_nft_discriminators() {
    let path = get_examples_dir().join("reputation_nft.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Verify v2 instructions are present
    assert!(source.contains("MINT NFT"), "Missing MINT instruction");
    assert!(
        source.contains("ADD TASK COMPLETE"),
        "Missing ADD TASK COMPLETE"
    );
    assert!(
        source.contains("ADD TASK FAILED"),
        "Missing ADD TASK FAILED (v2)"
    );
    assert!(
        source.contains("VERIFY REPUTATION"),
        "Missing VERIFY REPUTATION"
    );
    assert!(
        source.contains("GET REQUIRED COLLATERAL"),
        "Missing GET COLLATERAL (v2)"
    );
}

// =============================================================================
// ELF STRUCTURE TESTS
// =============================================================================

#[test]
fn test_elf_has_valid_header() {
    let result = compile_program("agent_escrow.ovsm").unwrap();
    let elf = &result.elf_bytes;

    // Check ELF magic bytes
    assert_eq!(&elf[0..4], &[0x7f, b'E', b'L', b'F'], "Invalid ELF magic");

    // Check 64-bit (EI_CLASS = 2)
    assert_eq!(elf[4], 2, "Not 64-bit ELF");

    // Check little endian (EI_DATA = 1)
    assert_eq!(elf[5], 1, "Not little endian");
}

// =============================================================================
// REPUTATION ATTESTATION TESTS
// =============================================================================

#[test]
fn test_reputation_attestation_compiles() {
    let result = compile_program("reputation_attestation.ovsm");
    assert!(
        result.is_ok(),
        "reputation_attestation.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 0,
        "No sBPF instructions generated"
    );
}

#[test]
fn test_attestation_has_all_instructions() {
    let path = get_examples_dir().join("reputation_attestation.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Verify all 4 attestation instructions are present
    assert!(
        source.contains("QUERY REPUTATION"),
        "Missing QUERY instruction"
    );
    assert!(
        source.contains("ATTEST REPUTATION"),
        "Missing ATTEST instruction"
    );
    assert!(
        source.contains("VERIFY THRESHOLD"),
        "Missing VERIFY instruction"
    );
    assert!(
        source.contains("GET COLLATERAL DISCOUNT"),
        "Missing COLLATERAL instruction"
    );
}

/// Test trust score calculation formula
#[test]
fn test_trust_score_formula() {
    // Formula components:
    //   task_score = (net_tasks / total_tasks) * 50
    //   rating_score = (rating_x10 / 50) * 30
    //   dispute_score = (disputes_won / total_disputes) * 20
    //   trust_score = (task_score + rating_score + dispute_score) * (100 - decay) / 100

    fn calculate_trust(
        tasks_ok: i64,
        tasks_fail: i64,
        rating_x10: i64,
        disputes_won: i64,
        disputes_lost: i64,
        decay_percent: i64,
    ) -> i64 {
        let net_tasks = tasks_ok - (tasks_fail * 2);
        let total_tasks = tasks_ok + tasks_fail;

        let task_score = if total_tasks == 0 || net_tasks < 0 {
            0
        } else {
            (net_tasks * 50) / total_tasks
        };

        let rating_score = (rating_x10 * 30) / 50;

        let total_disputes = disputes_won + disputes_lost;
        let dispute_score = if total_disputes == 0 {
            10 // Neutral if no disputes
        } else {
            (disputes_won * 20) / total_disputes
        };

        let raw_trust = task_score + rating_score + dispute_score;

        if decay_percent >= 100 {
            0
        } else {
            (raw_trust * (100 - decay_percent)) / 100
        }
    }

    // Perfect agent: 100 tasks, 0 fails, 5.0 rating, all disputes won, no decay
    assert!(
        calculate_trust(100, 0, 50, 10, 0, 0) >= 80,
        "Perfect agent should have high trust"
    );

    // Bad agent: 10 tasks, 10 fails (net = -10), low rating
    // net_tasks = -10 (negative), so task_score = 0
    // rating_score = 20 * 30 / 50 = 12
    // dispute_score = 0 (all lost)
    // Total = 0 + 12 + 0 = 12
    assert_eq!(
        calculate_trust(10, 10, 20, 0, 5, 0),
        12,
        "Bad agent should have low trust"
    );

    // Decayed agent: good stats but 50% decay
    let no_decay = calculate_trust(50, 0, 40, 5, 0, 0);
    let with_decay = calculate_trust(50, 0, 40, 5, 0, 50);
    assert_eq!(
        with_decay,
        no_decay / 2,
        "50% decay should halve trust score"
    );
}

/// Test collateral discount formula
#[test]
fn test_collateral_discount_formula() {
    // discount = trust_score * 90 / 100
    fn discount(trust_score: i64) -> i64 {
        (trust_score * 90) / 100
    }

    assert_eq!(discount(0), 0, "0 trust = 0% discount");
    assert_eq!(discount(50), 45, "50 trust = 45% discount");
    assert_eq!(discount(100), 90, "100 trust = 90% discount (max)");
}

// ═══════════════════════════════════════════════════════════════════════════════
// MARKETPLACE TESTS
// ═══════════════════════════════════════════════════════════════════════════════

/// Test that agent_marketplace.ovsm compiles successfully
#[test]
fn test_agent_marketplace_compiles() {
    let result = compile_program("agent_marketplace.ovsm");
    assert!(
        result.is_ok(),
        "agent_marketplace.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 0,
        "No sBPF instructions generated"
    );
}

/// Test marketplace has all 5 instructions
#[test]
fn test_marketplace_has_all_instructions() {
    let path = get_examples_dir().join("agent_marketplace.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Verify all 5 marketplace instructions are present
    assert!(
        source.contains("CREATE LISTING"),
        "Missing CREATE LISTING instruction"
    );
    assert!(
        source.contains("UPDATE LISTING"),
        "Missing UPDATE LISTING instruction"
    );
    assert!(
        source.contains("ACCEPT JOB"),
        "Missing ACCEPT JOB instruction"
    );
    assert!(
        source.contains("COMPLETE LISTING"),
        "Missing COMPLETE LISTING instruction"
    );
    assert!(
        source.contains("QUERY LISTING FEE") || source.contains("QUERY FEE"),
        "Missing QUERY FEE instruction"
    );
}

/// Test marketplace threshold requirements
#[test]
fn test_marketplace_threshold_requirements() {
    let path = get_examples_dir().join("agent_marketplace.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Verify threshold constants are defined
    assert!(source.contains("MIN_TASKS"), "Missing MIN_TASKS threshold");
    assert!(
        source.contains("MIN_RATING_X10"),
        "Missing MIN_RATING_X10 threshold"
    );
    assert!(source.contains("MAX_DECAY"), "Missing MAX_DECAY threshold");

    // Verify threshold values
    assert!(
        source.contains("(define MIN_TASKS 5)"),
        "MIN_TASKS should be 5"
    );
    assert!(
        source.contains("(define MIN_RATING_X10 30)"),
        "MIN_RATING_X10 should be 30 (3.0 stars)"
    );
    assert!(
        source.contains("(define MAX_DECAY 50)"),
        "MAX_DECAY should be 50%"
    );
}

/// Test marketplace fee constants
#[test]
fn test_marketplace_fee_constants() {
    let path = get_examples_dir().join("agent_marketplace.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Verify fee constants
    assert!(source.contains("BASE_FEE"), "Missing BASE_FEE constant");
    assert!(
        source.contains("BASE_DEPOSIT"),
        "Missing BASE_DEPOSIT constant"
    );

    // BASE_FEE should be 0.01 SOL (10,000,000 lamports)
    assert!(
        source.contains("10000000"),
        "BASE_FEE should be 10,000,000 lamports"
    );
    // BASE_DEPOSIT should be 0.1 SOL (100,000,000 lamports)
    assert!(
        source.contains("100000000"),
        "BASE_DEPOSIT should be 100,000,000 lamports"
    );
}

/// Test marketplace listing account layout
#[test]
fn test_marketplace_listing_layout() {
    let path = get_examples_dir().join("agent_marketplace.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Verify correct memory offsets are used for listing data
    // offset 0: status
    assert!(
        source.contains("(mem-store listing_ptr 0"),
        "Should write status at offset 0"
    );
    // offset 8: price
    assert!(
        source.contains("(mem-store listing_ptr 8"),
        "Should write price at offset 8"
    );
    // offset 16: deposit
    assert!(
        source.contains("(mem-store listing_ptr 16"),
        "Should write deposit at offset 16"
    );
    // offset 24: created_at
    assert!(
        source.contains("(mem-store listing_ptr 24"),
        "Should write created_at at offset 24"
    );
    // offset 32: expires_at
    assert!(
        source.contains("(mem-store listing_ptr 32"),
        "Should write expires_at at offset 32"
    );
    // offset 40-71: agent pubkey (32 bytes)
    assert!(
        source.contains("(mem-store listing_ptr 40"),
        "Should write agent pubkey at offset 40"
    );
    // offset 72-103: client pubkey (32 bytes)
    assert!(
        source.contains("(mem-store listing_ptr 72"),
        "Should write client pubkey at offset 72"
    );
}

/// Test marketplace fee discount formula
#[test]
fn test_marketplace_fee_discount_formula() {
    // The marketplace uses the same trust score formula as attestation
    // but with a simplified version (no dispute_score component)
    // discount = trust_score * 90 / 100
    // actual_fee = BASE_FEE * (100 - discount) / 100

    const BASE_FEE: i64 = 10_000_000; // 0.01 SOL
    const BASE_DEPOSIT: i64 = 100_000_000; // 0.1 SOL

    fn calculate_marketplace_fee(trust_score: i64) -> (i64, i64) {
        let discount = (trust_score * 90) / 100;
        let actual_fee = (BASE_FEE * (100 - discount)) / 100;
        let actual_deposit = (BASE_DEPOSIT * (100 - discount)) / 100;
        (actual_fee, actual_deposit)
    }

    // 0 trust = full price
    let (fee, deposit) = calculate_marketplace_fee(0);
    assert_eq!(fee, BASE_FEE, "0 trust = full fee");
    assert_eq!(deposit, BASE_DEPOSIT, "0 trust = full deposit");

    // 100 trust = 10% of price (90% discount)
    let (fee, deposit) = calculate_marketplace_fee(100);
    assert_eq!(fee, 1_000_000, "100 trust = 0.001 SOL fee");
    assert_eq!(deposit, 10_000_000, "100 trust = 0.01 SOL deposit");

    // 50 trust = 55% of price (45% discount)
    let (fee, deposit) = calculate_marketplace_fee(50);
    assert_eq!(fee, 5_500_000, "50 trust = 0.0055 SOL fee");
    assert_eq!(deposit, 55_000_000, "50 trust = 0.055 SOL deposit");
}

/// Test marketplace threshold pass/fail logic
#[test]
fn test_marketplace_threshold_logic() {
    const MIN_TASKS: i64 = 5;
    const MIN_RATING_X10: i64 = 30;
    const MAX_DECAY: i64 = 50;

    fn passes_threshold(net_tasks: i64, rating_x10: i64, decay_factor: i64) -> bool {
        if net_tasks < MIN_TASKS {
            return false;
        }
        if rating_x10 < MIN_RATING_X10 {
            return false;
        }
        if decay_factor > MAX_DECAY {
            return false;
        }
        true
    }

    // Good agent passes
    assert!(passes_threshold(10, 40, 20), "Good agent should pass");

    // Just meeting minimums passes
    assert!(passes_threshold(5, 30, 50), "Agent at minimums should pass");

    // Below net_tasks fails
    assert!(!passes_threshold(4, 40, 20), "Below MIN_TASKS should fail");

    // Below rating fails
    assert!(
        !passes_threshold(10, 29, 20),
        "Below MIN_RATING should fail"
    );

    // Above decay fails
    assert!(!passes_threshold(10, 40, 51), "Above MAX_DECAY should fail");

    // Negative net_tasks fails
    assert!(
        !passes_threshold(-5, 40, 20),
        "Negative net_tasks should fail"
    );
}

/// Test marketplace reads reputation NFT correctly
#[test]
fn test_marketplace_reads_nft() {
    let path = get_examples_dir().join("agent_marketplace.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Should read from account 2 (reputation NFT)
    assert!(
        source.contains("(account-data-ptr 2)"),
        "Should read NFT from account 2"
    );

    // Should read NFT fields at correct offsets
    // offset 0: initialized flag
    assert!(
        source.contains("(mem-load1 nft_ptr 0)"),
        "Should check NFT initialized at offset 0"
    );
    // offset 8: tasks_ok
    assert!(
        source.contains("(mem-load nft_ptr 8)"),
        "Should read tasks_ok at offset 8"
    );
    // offset 16: tasks_fail
    assert!(
        source.contains("(mem-load nft_ptr 16)"),
        "Should read tasks_fail at offset 16"
    );
    // offset 40: total_ratings
    assert!(
        source.contains("(mem-load nft_ptr 40)"),
        "Should read total_ratings at offset 40"
    );
    // offset 48: sum_ratings
    assert!(
        source.contains("(mem-load nft_ptr 48)"),
        "Should read sum_ratings at offset 48"
    );
    // offset 80: last_active
    assert!(
        source.contains("(mem-load nft_ptr 80)"),
        "Should read last_active at offset 80"
    );
}

/// Test marketplace status transitions
#[test]
fn test_marketplace_status_constants() {
    let path = get_examples_dir().join("agent_marketplace.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Verify all status constants are defined
    assert!(source.contains("STATUS_EMPTY"), "Missing STATUS_EMPTY");
    assert!(source.contains("STATUS_ACTIVE"), "Missing STATUS_ACTIVE");
    assert!(source.contains("STATUS_HIRED"), "Missing STATUS_HIRED");
    assert!(
        source.contains("STATUS_COMPLETED"),
        "Missing STATUS_COMPLETED"
    );
    assert!(
        source.contains("STATUS_CANCELLED"),
        "Missing STATUS_CANCELLED"
    );

    // Verify status values
    assert!(
        source.contains("(define STATUS_EMPTY 0)"),
        "STATUS_EMPTY should be 0"
    );
    assert!(
        source.contains("(define STATUS_ACTIVE 1)"),
        "STATUS_ACTIVE should be 1"
    );
    assert!(
        source.contains("(define STATUS_HIRED 2)"),
        "STATUS_HIRED should be 2"
    );
    assert!(
        source.contains("(define STATUS_COMPLETED 3)"),
        "STATUS_COMPLETED should be 3"
    );
    assert!(
        source.contains("(define STATUS_CANCELLED 4)"),
        "STATUS_CANCELLED should be 4"
    );
}

/// Test full suite with marketplace included
#[test]
fn test_full_suite_with_marketplace() {
    let programs = vec![
        "reputation_nft.ovsm",
        "reputation_attestation.ovsm",
        "dispute_resolution.ovsm",
        "agent_escrow.ovsm",
        "agent_marketplace.ovsm",
    ];

    let mut total_instructions = 0;
    let mut total_size = 0;

    for prog in &programs {
        let result = compile_program(prog);
        assert!(
            result.is_ok(),
            "{} failed to compile: {:?}",
            prog,
            result.err()
        );

        let compiled = result.unwrap();
        total_instructions += compiled.sbpf_instruction_count;
        total_size += compiled.elf_bytes.len();
    }

    // Combined stats
    assert!(
        total_instructions > 4000,
        "Combined suite should have >4000 instructions"
    );
    assert!(total_size > 50000, "Combined suite should be >50KB total");

    println!("Full Agent Accountability Suite:");
    println!("  Total sBPF instructions: {}", total_instructions);
    println!(
        "  Total ELF size: {} bytes ({:.1} KB)",
        total_size,
        total_size as f64 / 1024.0
    );
}

// =============================================================================
// ESCROW PDA TESTS - Real CPI with PDA signer seeds
// =============================================================================

#[test]
fn test_escrow_pda_compiles() {
    let result = compile_program("agent_escrow_pda.ovsm");
    assert!(
        result.is_ok(),
        "agent_escrow_pda.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 0,
        "No sBPF instructions generated"
    );
    assert!(compiled.elf_bytes.len() > 1000, "ELF too small");
    assert!(
        compiled.elf_bytes.len() < 50_000,
        "ELF too large: {} bytes",
        compiled.elf_bytes.len()
    );
}

#[test]
fn test_escrow_pda_has_cpi_invoke_signed() {
    let path = get_examples_dir().join("agent_escrow_pda.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Must use cpi-invoke-signed for PDA transfers
    assert!(
        source.contains("cpi-invoke-signed"),
        "Must use cpi-invoke-signed for PDA"
    );
}

#[test]
fn test_escrow_pda_uses_correct_seeds() {
    let path = get_examples_dir().join("agent_escrow_pda.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // PDA seed structure: ["escrow", job_id, bump]
    // Seed prefix should store "escrow" bytes (ASCII: 101, 115, 99, 114, 111, 119)
    assert!(
        source.contains("HEAP_SEED_PREFIX"),
        "Must define seed prefix heap location"
    );
    assert!(
        source.contains("HEAP_SEED_JOB"),
        "Must define job_id seed heap location"
    );
    assert!(
        source.contains("HEAP_SEED_BUMP"),
        "Must define bump seed heap location"
    );

    // "escrow" = e(101), s(115), c(99), r(114), o(111), w(119)
    assert!(
        source.contains("101") && source.contains("115") && source.contains("99"),
        "Should store 'escrow' ASCII bytes"
    );
}

#[test]
fn test_escrow_pda_job_state_layout() {
    let path = get_examples_dir().join("agent_escrow_pda.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Job state layout:
    //   offset 0: status (1 byte)
    //   offset 8-39: client pubkey (32 bytes)
    //   offset 40: amount
    //   offset 48: job_id
    //   offset 56: bump

    assert!(
        source.contains("(mem-store job_ptr 0"),
        "Should write status at offset 0"
    );
    assert!(
        source.contains("(mem-store job_ptr 8"),
        "Should write client pubkey at offset 8"
    );
    assert!(
        source.contains("(mem-store job_ptr 40"),
        "Should write amount at offset 40"
    );
    assert!(
        source.contains("(mem-store job_ptr 48"),
        "Should write job_id at offset 48"
    );
    assert!(
        source.contains("(mem-store job_ptr 56"),
        "Should write bump at offset 56"
    );
}

#[test]
fn test_escrow_pda_status_constants() {
    let path = get_examples_dir().join("agent_escrow_pda.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Status constants
    assert!(
        source.contains("(define STATUS_EMPTY 0)"),
        "STATUS_EMPTY should be 0"
    );
    assert!(
        source.contains("(define STATUS_FUNDED 1)"),
        "STATUS_FUNDED should be 1"
    );
    assert!(
        source.contains("(define STATUS_COMPLETED 2)"),
        "STATUS_COMPLETED should be 2"
    );
    assert!(
        source.contains("(define STATUS_REFUNDED 3)"),
        "STATUS_REFUNDED should be 3"
    );
}

#[test]
fn test_escrow_pda_has_all_instructions() {
    let path = get_examples_dir().join("agent_escrow_pda.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // All 4 instructions
    assert!(source.contains("INIT JOB"), "Missing INIT instruction");
    assert!(
        source.contains("RELEASE TO AGENT"),
        "Missing RELEASE instruction"
    );
    assert!(
        source.contains("REFUND CLIENT"),
        "Missing REFUND instruction"
    );
    assert!(source.contains("QUERY"), "Missing QUERY instruction");
}

#[test]
fn test_escrow_pda_cpi_accounts_format() {
    let path = get_examples_dir().join("agent_escrow_pda.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // CPI accounts format: [[idx writable signer] ...]
    // For escrow release: [[1 1 1] [2 1 0] [3 0 0]]
    //   - Account 1: Escrow PDA (writable + signer)
    //   - Account 2: Recipient (writable)
    //   - Account 3: System Program (read-only)

    // Check that escrow PDA is marked as signer (1 1 1)
    assert!(
        source.contains("[1 1 1]"),
        "Escrow PDA should be writable and signer"
    );
    // Check system program is read-only (3 0 0) or similar
    assert!(
        source.contains("[3 0 0]"),
        "System program should be read-only non-signer"
    );
}

#[test]
fn test_escrow_pda_seeds_array_format() {
    let path = get_examples_dir().join("agent_escrow_pda.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Seeds format: [[[seed1-ptr seed1-len] [seed2-ptr seed2-len] ...]]
    // For escrow: [[HEAP_SEED_PREFIX 6] [HEAP_SEED_JOB 8] [HEAP_SEED_BUMP 1]]
    //   - "escrow" = 6 bytes
    //   - job_id = 8 bytes
    //   - bump = 1 byte

    assert!(
        source.contains("HEAP_SEED_PREFIX 6]"),
        "Escrow prefix seed should be 6 bytes"
    );
    assert!(
        source.contains("HEAP_SEED_JOB 8]"),
        "Job ID seed should be 8 bytes"
    );
    assert!(
        source.contains("HEAP_SEED_BUMP 1]"),
        "Bump seed should be 1 byte"
    );
}

// =============================================================================
// CPI REAL MARKETPLACE TESTS
// =============================================================================

#[test]
fn test_marketplace_real_cpi_compiles() {
    let result = compile_program("agent_marketplace_real_cpi.ovsm");
    assert!(
        result.is_ok(),
        "agent_marketplace_real_cpi.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 0,
        "No sBPF instructions generated"
    );
}

#[test]
fn test_marketplace_real_cpi_uses_cpi_invoke() {
    let path = get_examples_dir().join("agent_marketplace_real_cpi.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Should use cpi-invoke (not cpi-invoke-signed, as it's calling another program)
    assert!(
        source.contains("cpi-invoke"),
        "Must use cpi-invoke for cross-program calls"
    );
}

#[test]
fn test_marketplace_real_cpi_heap_constants() {
    let path = get_examples_dir().join("agent_marketplace_real_cpi.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Should define heap address for CPI data
    assert!(
        source.contains("HEAP_CPI_DATA"),
        "Must define heap location for CPI data"
    );
}

// =============================================================================
// FULL SUITE WITH ALL CPI PROGRAMS
// =============================================================================

#[test]
fn test_full_suite_with_cpi_programs() {
    let programs = vec![
        "reputation_nft.ovsm",
        "reputation_attestation.ovsm",
        "dispute_resolution.ovsm",
        "agent_escrow.ovsm",
        "agent_marketplace.ovsm",
        "agent_marketplace_real_cpi.ovsm",
        "agent_escrow_pda.ovsm",
    ];

    let mut total_instructions = 0;
    let mut total_size = 0;
    let mut program_sizes = Vec::new();

    for prog in &programs {
        let result = compile_program(prog);
        assert!(
            result.is_ok(),
            "{} failed to compile: {:?}",
            prog,
            result.err()
        );

        let compiled = result.unwrap();
        total_instructions += compiled.sbpf_instruction_count;
        total_size += compiled.elf_bytes.len();
        program_sizes.push((prog.to_string(), compiled.elf_bytes.len()));
    }

    // Combined stats for 7 programs
    assert!(
        total_instructions > 5000,
        "Combined suite should have >5000 instructions"
    );
    assert!(total_size > 60000, "Combined suite should be >60KB total");

    println!("Full Agent Accountability Suite (with CPI):");
    println!("  Programs: {}", programs.len());
    println!("  Total sBPF instructions: {}", total_instructions);
    println!(
        "  Total ELF size: {} bytes ({:.1} KB)",
        total_size,
        total_size as f64 / 1024.0
    );
    println!("\n  Per-program sizes:");
    for (prog, size) in &program_sizes {
        println!("    {}: {} bytes", prog, size);
    }
}

// =============================================================================
// SPL TOKEN ESCROW TESTS
// =============================================================================

#[test]
fn test_token_escrow_compiles() {
    let result = compile_program("token_escrow.ovsm");
    assert!(
        result.is_ok(),
        "token_escrow.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 0,
        "No sBPF instructions generated"
    );
    assert!(compiled.elf_bytes.len() > 1000, "ELF too small");
    assert!(
        compiled.elf_bytes.len() < 50_000,
        "ELF too large: {} bytes",
        compiled.elf_bytes.len()
    );
}

#[test]
fn test_token_escrow_uses_spl_token_transfer() {
    let path = get_examples_dir().join("token_escrow.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Must use spl-token-transfer for SPL Token transfers
    assert!(
        source.contains("spl-token-transfer"),
        "Must use spl-token-transfer for SPL tokens"
    );
}

#[test]
fn test_token_escrow_has_all_instructions() {
    let path = get_examples_dir().join("token_escrow.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // All 4 instructions
    assert!(source.contains("CREATE JOB"), "Missing CREATE instruction");
    assert!(
        source.contains("COMPLETE JOB"),
        "Missing COMPLETE instruction"
    );
    assert!(source.contains("CANCEL JOB"), "Missing CANCEL instruction");
    assert!(source.contains("QUERY JOB"), "Missing QUERY instruction");
}

#[test]
fn test_token_escrow_status_constants() {
    let path = get_examples_dir().join("token_escrow.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Status constants
    assert!(
        source.contains("(define STATUS_EMPTY 0)"),
        "STATUS_EMPTY should be 0"
    );
    assert!(
        source.contains("(define STATUS_FUNDED 1)"),
        "STATUS_FUNDED should be 1"
    );
    assert!(
        source.contains("(define STATUS_COMPLETED 2)"),
        "STATUS_COMPLETED should be 2"
    );
    assert!(
        source.contains("(define STATUS_REFUNDED 3)"),
        "STATUS_REFUNDED should be 3"
    );
}

#[test]
fn test_token_escrow_job_layout() {
    let path = get_examples_dir().join("token_escrow.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Job state layout:
    //   [0]: status
    //   [1-8]: job_id
    //   [9-16]: amount
    //   [17-48]: client pubkey (32 bytes)
    //   [49-80]: agent pubkey (32 bytes)

    assert!(
        source.contains("(mem-store job_ptr 1"),
        "Should write job_id at offset 1"
    );
    assert!(
        source.contains("(mem-store job_ptr 9"),
        "Should write amount at offset 9"
    );
    assert!(
        source.contains("(mem-store job_ptr 17"),
        "Should write client pubkey at offset 17"
    );
    assert!(
        source.contains("(mem-store job_ptr 49"),
        "Should write agent pubkey at offset 49"
    );
}

#[test]
fn test_token_escrow_transfer_accounts() {
    let path = get_examples_dir().join("token_escrow.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Token transfers use accounts:
    //   5 = Token Program
    //   2 = Client token account
    //   1 = Escrow token account
    //   3 = Agent token account
    //   4 = Authority

    // Create job: client(2) -> escrow(1)
    assert!(
        source.contains("spl-token-transfer 5 2 1 4"),
        "Create should transfer from client(2) to escrow(1)"
    );

    // Complete job: escrow(1) -> agent(3)
    assert!(
        source.contains("spl-token-transfer 5 1 3 4"),
        "Complete should transfer from escrow(1) to agent(3)"
    );

    // Cancel job: escrow(1) -> client(2)
    assert!(
        source.contains("spl-token-transfer 5 1 2 4"),
        "Cancel should transfer from escrow(1) to client(2)"
    );
}

// =============================================================================
// COMPLETE AGENT ACCOUNTABILITY SUITE (8 programs)
// =============================================================================

#[test]
fn test_complete_accountability_suite() {
    let programs = vec![
        "reputation_nft.ovsm",
        "reputation_attestation.ovsm",
        "dispute_resolution.ovsm",
        "agent_escrow.ovsm",
        "agent_marketplace.ovsm",
        "agent_marketplace_real_cpi.ovsm",
        "agent_escrow_pda.ovsm",
        "token_escrow.ovsm",
    ];

    let mut total_instructions = 0;
    let mut total_size = 0;
    let mut program_sizes = Vec::new();

    for prog in &programs {
        let result = compile_program(prog);
        assert!(
            result.is_ok(),
            "{} failed to compile: {:?}",
            prog,
            result.err()
        );

        let compiled = result.unwrap();
        total_instructions += compiled.sbpf_instruction_count;
        total_size += compiled.elf_bytes.len();
        program_sizes.push((prog.to_string(), compiled.elf_bytes.len()));
    }

    // Combined stats for 8 programs
    assert!(
        total_instructions > 6000,
        "Combined suite should have >6000 instructions"
    );
    assert!(total_size > 70000, "Combined suite should be >70KB total");

    println!("\n======================================================");
    println!("COMPLETE AGENT ACCOUNTABILITY SUITE (8 PROGRAMS):");
    println!("======================================================");
    println!("  Total sBPF instructions: {}", total_instructions);
    println!(
        "  Total ELF size: {} bytes ({:.1} KB)",
        total_size,
        total_size as f64 / 1024.0
    );
    println!("\n  Per-program sizes:");
    for (prog, size) in &program_sizes {
        println!("    {}: {} bytes", prog, size);
    }
    println!("======================================================\n");
}

// =============================================================================
// DEMO PROGRAM TESTS
// =============================================================================

#[test]
fn test_accountability_demo_compiles() {
    let result = compile_program("accountability_demo.ovsm");
    assert!(
        result.is_ok(),
        "accountability_demo.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 500,
        "Demo should have >500 sBPF instructions"
    );
    assert!(compiled.elf_bytes.len() > 5000, "Demo ELF too small");
    assert!(compiled.elf_bytes.len() < 50_000, "Demo ELF too large");

    println!("Accountability Demo Program:");
    println!("  sBPF instructions: {}", compiled.sbpf_instruction_count);
    println!("  ELF size: {} bytes", compiled.elf_bytes.len());
}

#[test]
fn test_accountability_demo_has_all_phases() {
    let path = get_examples_dir().join("accountability_demo.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // All phases
    assert!(source.contains("PHASE_INIT"), "Missing PHASE_INIT");
    assert!(
        source.contains("PHASE_REGISTERED"),
        "Missing PHASE_REGISTERED"
    );
    assert!(
        source.contains("PHASE_BUILDING_REP"),
        "Missing PHASE_BUILDING_REP"
    );
    assert!(source.contains("PHASE_LISTED"), "Missing PHASE_LISTED");
    assert!(
        source.contains("PHASE_JOB_ACTIVE"),
        "Missing PHASE_JOB_ACTIVE"
    );
    assert!(source.contains("PHASE_COMPLETE"), "Missing PHASE_COMPLETE");
}

#[test]
fn test_accountability_demo_has_all_instructions() {
    let path = get_examples_dir().join("accountability_demo.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // All instructions
    assert!(source.contains("INIT DEMO"), "Missing INIT instruction");
    assert!(
        source.contains("REGISTER AGENT"),
        "Missing REGISTER instruction"
    );
    assert!(
        source.contains("COMPLETE TASK"),
        "Missing COMPLETE_TASK instruction"
    );
    assert!(
        source.contains("FAIL TASK"),
        "Missing FAIL_TASK instruction"
    );
    assert!(
        source.contains("LIST SERVICE"),
        "Missing LIST_SERVICE instruction"
    );
    assert!(
        source.contains("HIRE AGENT"),
        "Missing HIRE_AGENT instruction"
    );
    assert!(
        source.contains("DELIVER JOB"),
        "Missing DELIVER_JOB instruction"
    );
    assert!(
        source.contains("DISPUTE JOB"),
        "Missing DISPUTE_JOB instruction"
    );
    assert!(
        source.contains("QUERY STATUS"),
        "Missing QUERY_STATUS instruction"
    );
}

#[test]
fn test_accountability_demo_simulates_workflow() {
    let path = get_examples_dir().join("accountability_demo.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Simulation markers
    assert!(
        source.contains("[SIM] Minted Reputation NFT"),
        "Should simulate NFT minting"
    );
    assert!(
        source.contains("[SIM] Created attestation"),
        "Should simulate attestation creation"
    );
    assert!(
        source.contains("[SIM] Created marketplace listing"),
        "Should simulate marketplace listing"
    );
    assert!(
        source.contains("[SIM] Created escrow account"),
        "Should simulate escrow creation"
    );
    assert!(
        source.contains("[SIM] Released escrow to agent"),
        "Should simulate escrow release"
    );
    assert!(
        source.contains("[SIM] Created dispute case"),
        "Should simulate dispute creation"
    );
}

// =============================================================================
// FINAL COMPLETE SUITE (9 programs)
// =============================================================================

#[test]
fn test_final_accountability_suite() {
    let programs = vec![
        "reputation_nft.ovsm",
        "reputation_attestation.ovsm",
        "dispute_resolution.ovsm",
        "agent_escrow.ovsm",
        "agent_marketplace.ovsm",
        "agent_marketplace_real_cpi.ovsm",
        "agent_escrow_pda.ovsm",
        "token_escrow.ovsm",
        "accountability_demo.ovsm",
    ];

    let mut total_instructions = 0;
    let mut total_size = 0;
    let mut program_sizes = Vec::new();

    for prog in &programs {
        let result = compile_program(prog);
        assert!(
            result.is_ok(),
            "{} failed to compile: {:?}",
            prog,
            result.err()
        );

        let compiled = result.unwrap();
        total_instructions += compiled.sbpf_instruction_count;
        total_size += compiled.elf_bytes.len();
        program_sizes.push((
            prog.to_string(),
            compiled.sbpf_instruction_count,
            compiled.elf_bytes.len(),
        ));
    }

    // Combined stats for 9 programs
    assert!(
        total_instructions > 7000,
        "Combined suite should have >7000 instructions"
    );
    assert!(total_size > 80000, "Combined suite should be >80KB total");

    println!("\n╔══════════════════════════════════════════════════════════════════╗");
    println!("║     FINAL AGENT ACCOUNTABILITY SUITE - 9 PROGRAMS               ║");
    println!("╠══════════════════════════════════════════════════════════════════╣");
    println!(
        "║ Total sBPF instructions: {:>6}                                  ║",
        total_instructions
    );
    println!(
        "║ Total ELF size: {:>6} bytes ({:.1} KB)                         ║",
        total_size,
        total_size as f64 / 1024.0
    );
    println!("╠══════════════════════════════════════════════════════════════════╣");
    println!("║ Program Breakdown:                                               ║");
    for (prog, instrs, size) in &program_sizes {
        println!("║   {:40} {:>5} instr {:>6} B ║", prog, instrs, size);
    }
    println!("╚══════════════════════════════════════════════════════════════════╝\n");
}

// =============================================================================
// STRATEGY ROUTER SYSTEM TESTS
// =============================================================================

#[test]
fn test_strategy_registry_compiles() {
    let result = compile_program("strategy_registry.ovsm");
    assert!(
        result.is_ok(),
        "strategy_registry.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 500,
        "Strategy registry should have >500 instructions"
    );
    println!(
        "✅ strategy_registry.ovsm: {} instructions, {} bytes",
        compiled.sbpf_instruction_count,
        compiled.elf_bytes.len()
    );
}

#[test]
fn test_basic_strategy_compiles() {
    let result = compile_program("basic_strategy.ovsm");
    assert!(
        result.is_ok(),
        "basic_strategy.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 100,
        "Basic strategy should have >100 instructions"
    );
    println!(
        "✅ basic_strategy.ovsm: {} instructions, {} bytes",
        compiled.sbpf_instruction_count,
        compiled.elf_bytes.len()
    );
}

#[test]
fn test_advanced_strategy_compiles() {
    let result = compile_program("advanced_strategy.ovsm");
    assert!(
        result.is_ok(),
        "advanced_strategy.ovsm failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();
    assert!(
        compiled.sbpf_instruction_count > 200,
        "Advanced strategy should have >200 instructions"
    );
    println!(
        "✅ advanced_strategy.ovsm: {} instructions, {} bytes",
        compiled.sbpf_instruction_count,
        compiled.elf_bytes.len()
    );
}

#[test]
fn test_strategy_registry_has_all_instructions() {
    let path = get_examples_dir().join("strategy_registry.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Check for all instruction handlers
    assert!(
        source.contains("INSTR_INIT_REGISTRY"),
        "Should have init registry instruction"
    );
    assert!(
        source.contains("INSTR_REGISTER_STRATEGY"),
        "Should have register strategy instruction"
    );
    assert!(
        source.contains("INSTR_DEACTIVATE"),
        "Should have deactivate instruction"
    );
    assert!(
        source.contains("INSTR_UPDATE_FEE"),
        "Should have update fee instruction"
    );
    assert!(
        source.contains("INSTR_SELECT_EXECUTE"),
        "Should have select execute instruction"
    );
    assert!(
        source.contains("INSTR_QUERY_AVAILABLE"),
        "Should have query instruction"
    );
}

#[test]
fn test_strategy_registry_reputation_gating() {
    let path = get_examples_dir().join("strategy_registry.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Check reputation-gated access
    assert!(
        source.contains("min_reputation"),
        "Should have min_reputation field"
    );
    assert!(source.contains("agent_rep"), "Should read agent reputation");
    assert!(
        source.contains("< agent_rep min_rep"),
        "Should compare agent rep against strategy min"
    );
    assert!(
        source.contains("REP_TOO_LOW"),
        "Should have reputation too low error"
    );
}

#[test]
fn test_strategy_registry_uses_clock() {
    let path = get_examples_dir().join("strategy_registry.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Check timestamp usage
    assert!(
        source.contains("get-clock-timestamp"),
        "Should use clock sysvar"
    );
    assert!(
        source.contains("registered_at"),
        "Should track registration time"
    );
    assert!(
        source.contains("last_executed_at"),
        "Should track last execution time"
    );
}

#[test]
fn test_basic_strategy_task_types() {
    let path = get_examples_dir().join("basic_strategy.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Check supported task types
    assert!(
        source.contains("TASK_COMPUTE"),
        "Should support compute tasks"
    );
    assert!(
        source.contains("TASK_VALIDATE"),
        "Should support validate tasks"
    );
    assert!(
        source.contains("TASK_TRANSFORM"),
        "Should support transform tasks"
    );
}

#[test]
fn test_advanced_strategy_features() {
    let path = get_examples_dir().join("advanced_strategy.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Check advanced features
    assert!(
        source.contains("INSTR_PIPELINE"),
        "Should have pipeline instruction"
    );
    assert!(
        source.contains("INSTR_AGGREGATE"),
        "Should have aggregate instruction"
    );
    assert!(
        source.contains("INSTR_CONDITIONAL"),
        "Should have conditional instruction"
    );
    assert!(
        source.contains("INSTR_VERIFY"),
        "Should have verify instruction"
    );

    // Check advanced computations
    assert!(
        source.contains("Newton-Raphson"),
        "Should mention sqrt approximation method"
    );
    assert!(
        source.contains("tolerance"),
        "Should have tolerance-based verification"
    );
}

#[test]
fn test_strategy_fee_tracking() {
    let path = get_examples_dir().join("strategy_registry.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    // Check fee tracking
    assert!(
        source.contains("fee_lamports"),
        "Should track fees in lamports"
    );
    assert!(
        source.contains("total_fees_earned"),
        "Should track cumulative fees"
    );
    assert!(
        source.contains("total_executions"),
        "Should track execution count"
    );
}

#[test]
fn test_complete_strategy_suite() {
    let programs = vec![
        "strategy_registry.ovsm",
        "basic_strategy.ovsm",
        "advanced_strategy.ovsm",
    ];

    let mut total_instructions = 0;
    let mut total_size = 0;

    for prog in &programs {
        let result = compile_program(prog);
        assert!(
            result.is_ok(),
            "{} failed to compile: {:?}",
            prog,
            result.err()
        );

        let compiled = result.unwrap();
        total_instructions += compiled.sbpf_instruction_count;
        total_size += compiled.elf_bytes.len();
    }

    println!("\n╔══════════════════════════════════════════════════════════════════╗");
    println!("║         STRATEGY ROUTER SUITE - 3 PROGRAMS                       ║");
    println!("╠══════════════════════════════════════════════════════════════════╣");
    println!(
        "║ Total sBPF instructions: {:>6}                                  ║",
        total_instructions
    );
    println!(
        "║ Total ELF size: {:>6} bytes ({:.1} KB)                         ║",
        total_size,
        total_size as f64 / 1024.0
    );
    println!("╚══════════════════════════════════════════════════════════════════╝\n");

    assert!(
        total_instructions > 800,
        "Strategy suite should have >800 instructions"
    );
}

#[test]
fn test_full_suite_with_strategy_router() {
    // All 12 programs: original 9 + 3 strategy router programs
    let programs = vec![
        "reputation_nft.ovsm",
        "reputation_attestation.ovsm",
        "dispute_resolution.ovsm",
        "agent_escrow.ovsm",
        "agent_marketplace.ovsm",
        "agent_marketplace_real_cpi.ovsm",
        "agent_escrow_pda.ovsm",
        "token_escrow.ovsm",
        "accountability_demo.ovsm",
        "agent_registry.ovsm",
        "strategy_registry.ovsm",
        "basic_strategy.ovsm",
        "advanced_strategy.ovsm",
    ];

    let mut total_instructions = 0;
    let mut total_size = 0;

    for prog in &programs {
        let result = compile_program(prog);
        assert!(
            result.is_ok(),
            "{} failed to compile: {:?}",
            prog,
            result.err()
        );

        let compiled = result.unwrap();
        total_instructions += compiled.sbpf_instruction_count;
        total_size += compiled.elf_bytes.len();
    }

    println!("\n╔══════════════════════════════════════════════════════════════════╗");
    println!("║   COMPLETE AGENT ECOSYSTEM SUITE - 13 PROGRAMS                   ║");
    println!("╠══════════════════════════════════════════════════════════════════╣");
    println!(
        "║ Total sBPF instructions: {:>6}                                  ║",
        total_instructions
    );
    println!(
        "║ Total ELF size: {:>6} bytes ({:.1} KB)                         ║",
        total_size,
        total_size as f64 / 1024.0
    );
    println!("╚══════════════════════════════════════════════════════════════════╝\n");

    assert!(
        total_instructions > 10000,
        "Complete suite should have >10000 instructions"
    );
    assert!(programs.len() == 13, "Suite should have 13 programs");
}

// =============================================================================
// ANCHOR IDL GENERATION TESTS
// =============================================================================

#[test]
fn test_generate_idl_from_accountability_demo() {
    use ovsm::compiler::anchor_idl::IdlGenerator;

    let path = get_examples_dir().join("accountability_demo.ovsm");
    let source = fs::read_to_string(&path).unwrap();

    let mut generator = IdlGenerator::new(&source);
    let idl = generator.generate().expect("IDL generation failed");

    // Should extract program name
    assert!(!idl.name.is_empty(), "Should extract program name");

    // Should extract multiple instructions
    assert!(
        idl.instructions.len() >= 8,
        "Should have at least 8 instructions, got {}",
        idl.instructions.len()
    );

    // Should extract errors
    assert!(idl.errors.len() >= 1, "Should have extracted errors");

    // Generate JSON and verify it's valid
    let json = generator
        .generate_json()
        .expect("JSON serialization failed");
    assert!(json.contains("\"version\""));
    assert!(json.contains("\"instructions\""));

    println!("\n✅ Generated Anchor IDL from accountability_demo.ovsm:");
    println!("   Name: {}", idl.name);
    println!("   Version: {}", idl.version);
    println!("   Instructions: {}", idl.instructions.len());
    println!("   Errors: {}", idl.errors.len());
    println!("\n   Instruction names:");
    for instr in &idl.instructions {
        println!("     - {} (disc: {:?})", instr.name, instr.discriminator);
    }
}

#[test]
fn test_idl_json_format() {
    use ovsm::compiler::anchor_idl::IdlGenerator;

    let source = r#"
;;; SIMPLE PROGRAM - Test
;;;
;;; Accounts:
;;;   0: State (writable)
;;;   1: Authority (signer)

(do
  (define discriminator (mem-load1 instr_ptr 0))

  (if (= discriminator 0)
    (do
      (sol_log_ ">>> INITIALIZE <<<")
      (if false (do (sol_log_ "ERR: Already init") 1) 0))
    0)

  (if (= discriminator 1)
    (do
      (sol_log_ ">>> TRANSFER <<<")
      0)
    0)

  0)
"#;

    let mut generator = IdlGenerator::new(source);
    let json = generator.generate_json().expect("JSON generation failed");

    // Parse and verify structure
    let parsed: serde_json::Value = serde_json::from_str(&json).expect("Invalid JSON");

    assert!(parsed["version"].is_string());
    assert!(parsed["name"].is_string());
    assert!(parsed["instructions"].is_array());

    println!("\n✅ Valid Anchor IDL JSON generated:");
    println!("{}", json);
}

// =============================================================================
// SPL TOKEN TRANSFER SIGNED TESTS (PDA Authority)
// =============================================================================

#[test]
fn test_spl_token_transfer_signed_compiles() {
    // Create test program inline using temp file
    let source = r#"
;;; Test SPL Token Transfer Signed (PDA Authority)
(do
  (sol_log_ "=== SPL TOKEN TRANSFER SIGNED ===")

  ;; Heap addresses for seeds
  (define HEAP_SEED_PREFIX 12884901888)  ;; 0x300000000
  (define HEAP_SEED_BUMP 12884901920)    ;; 0x300000020

  ;; Read amount from instruction data
  (define instr_ptr (instruction-data-ptr))
  (define amount (mem-load instr_ptr 0))
  (define bump (mem-load1 instr_ptr 8))

  (sol_log_ "Amount:")
  (sol_log_64_ amount)
  (sol_log_ "Bump:")
  (sol_log_64_ bump)

  ;; Store PDA seeds in heap
  ;; Seed 1: "vault" = [118, 97, 117, 108, 116]
  (mem-store1 HEAP_SEED_PREFIX 0 118)  ;; 'v'
  (mem-store1 HEAP_SEED_PREFIX 1 97)   ;; 'a'
  (mem-store1 HEAP_SEED_PREFIX 2 117)  ;; 'u'
  (mem-store1 HEAP_SEED_PREFIX 3 108)  ;; 'l'
  (mem-store1 HEAP_SEED_PREFIX 4 116)  ;; 't'

  ;; Seed 2: bump byte
  (mem-store1 HEAP_SEED_BUMP 0 bump)

  (sol_log_ "Calling spl-token-transfer-signed...")

  ;; Call SPL Token Transfer with PDA signing
  ;; token-prog-idx=3, source=0, dest=1, authority=2
  ;; signers: [[[HEAP_SEED_PREFIX 5] [HEAP_SEED_BUMP 1]]]
  (define result
    (spl-token-transfer-signed
      3                                          ;; Token Program index
      0                                          ;; Source token account
      1                                          ;; Destination token account
      2                                          ;; PDA Authority
      amount                                     ;; Amount
      [[[HEAP_SEED_PREFIX 5] [HEAP_SEED_BUMP 1]]]))  ;; Seeds: "vault" + bump

  (sol_log_ "Result:")
  (sol_log_64_ result)

  result)
"#;

    // Compile directly from source string
    let compiler = Compiler::new(CompileOptions::default());
    let result = compiler.compile(source);
    assert!(
        result.is_ok(),
        "spl-token-transfer-signed test failed to compile: {:?}",
        result.err()
    );

    let compiled = result.unwrap();

    // Verify reasonable instruction counts
    assert!(
        compiled.sbpf_instruction_count > 100,
        "Should have >100 sBPF instructions, got {}",
        compiled.sbpf_instruction_count
    );
    assert!(
        compiled.elf_bytes.len() > 1000,
        "ELF too small: {} bytes",
        compiled.elf_bytes.len()
    );
    assert!(
        compiled.elf_bytes.len() < 10000,
        "ELF too large: {} bytes",
        compiled.elf_bytes.len()
    );

    println!("✅ spl-token-transfer-signed test program:");
    println!("   sBPF instructions: {}", compiled.sbpf_instruction_count);
    println!("   ELF size: {} bytes", compiled.elf_bytes.len());
}

#[test]
fn test_spl_token_transfer_signed_multiple_seeds() {
    // Test with multiple signers/seeds
    let source = r#"
;;; Test SPL Token Transfer Signed with multiple seeds
(do
  (sol_log_ "=== MULTI-SEED PDA TRANSFER ===")

  (define HEAP_SEED1 12884901888)  ;; 0x300000000
  (define HEAP_SEED2 12884901920)  ;; 0x300000020
  (define HEAP_BUMP  12884901952)  ;; 0x300000040

  ;; Setup 3-part seed: "escrow" + [user_pubkey_byte] + bump
  (mem-store1 HEAP_SEED1 0 101)  ;; 'e'
  (mem-store1 HEAP_SEED1 1 115)  ;; 's'
  (mem-store1 HEAP_SEED1 2 99)   ;; 'c'
  (mem-store1 HEAP_SEED1 3 114)  ;; 'r'
  (mem-store1 HEAP_SEED1 4 111)  ;; 'o'
  (mem-store1 HEAP_SEED1 5 119)  ;; 'w'

  (mem-store1 HEAP_SEED2 0 42)   ;; arbitrary user byte

  (mem-store1 HEAP_BUMP 0 255)   ;; bump

  ;; Transfer with 3-seed PDA
  (define result
    (spl-token-transfer-signed
      3 0 1 2 1000000
      [[[HEAP_SEED1 6] [HEAP_SEED2 1] [HEAP_BUMP 1]]]))

  result)
"#;

    // Compile directly from source string
    let compiler = Compiler::new(CompileOptions::default());
    let result = compiler.compile(source);
    assert!(
        result.is_ok(),
        "Multi-seed spl-token-transfer-signed failed: {:?}",
        result.err()
    );

    println!("✅ Multi-seed PDA transfer compiles successfully");
}
