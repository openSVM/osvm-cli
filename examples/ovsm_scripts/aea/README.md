# AEA Protocol - Autonomous Economic Agents

A comprehensive on-chain protocol for human-AI economic coordination, written in OVSM LISP and compiling to Solana sBPF bytecode.

## Overview

The AEA (Autonomous Economic Agents) Protocol enables:

- **Identity Registry**: Humans and AI agents register on-chain with staked collateral
- **Service Marketplace**: Providers list services, buyers create escrow-protected orders
- **Reputation System**: Track record builds trust through successful transactions
- **Autonomous Trading**: Agents can bid, negotiate, and trade without human intervention
- **DAO Governance**: Stake-weighted voting on protocol parameters

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           AEA PROTOCOL SUITE                                │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─────────────────────┐    ┌──────────────────────┐    ┌──────────────────┐│
│  │  ON-CHAIN PROGRAMS  │    │  OFF-CHAIN SCRIPTS   │    │    TOOLING       ││
│  │   (Compile to sBPF) │    │  (OVSM Interpreter)  │    │                  ││
│  ├─────────────────────┤    ├──────────────────────┤    ├──────────────────┤│
│  │                     │    │                      │    │                  ││
│  │  aea_protocol.ovsm  │    │  aea_agent_brain.ovsm│    │  IDL JSON files  ││
│  │  - Identity mgmt    │    │  - AI decision layer │    │  - TypeScript    ││
│  │  - Service registry │    │  - Risk assessment   │    │    client gen    ││
│  │  - Order escrow     │    │  - Opportunity scan  │    │                  ││
│  │  - Reputation       │    │                      │    │  validate-       ││
│  │                     │    │  aea_game_theory.ovsm│    │  instruction     ││
│  │  aea_negotiation    │    │  - Nash equilibrium  │    │  - Debug txs     ││
│  │  .ovsm              │    │  - Price strategy    │    │                  ││
│  │  - Bidding system   │    │  - Market analysis   │    │                  ││
│  │  - Counter-offers   │    │                      │    │                  ││
│  │  - Auto-accept      │    │  aea_economics_tests │    │                  ││
│  │                     │    │  .ovsm               │    │                  ││
│  │  aea_governance     │    │  - Token economics   │    │                  ││
│  │  .ovsm              │    │  - Staking tests     │    │                  ││
│  │  - DAO proposals    │    │  - Slashing tests    │    │                  ││
│  │  - Voting           │    │                      │    │                  ││
│  │  - Execution        │    │                      │    │                  ││
│  │                     │    │                      │    │                  ││
│  └─────────────────────┘    └──────────────────────┘    └──────────────────┘│
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Module Descriptions

### 1. `aea_protocol.ovsm` - Core Protocol (On-Chain)

The foundation layer providing identity, services, and escrow functionality.

**Discriminator Range**: 0-65

| Discriminator | Instruction | Description |
|--------------|-------------|-------------|
| 0 | `initializeProtocol` | Deploy protocol config |
| 10 | `registerUser` | Register human participant (free) |
| 11 | `registerAgent` | Register AI agent (requires stake) |
| 12 | `registerProvider` | Register service provider |
| 13 | `registerValidator` | Register dispute validator |
| 14 | `updateProfile` | Update participant metadata |
| 15 | `startCooldown` | Begin unstaking cooldown |
| 40 | `createOrder` | Create service order with escrow |
| 41 | `acceptOrder` | Provider accepts order |
| 43 | `confirmDelivery` | Buyer confirms receipt |
| 44 | `cancelOrder` | Buyer cancels before acceptance |
| 45 | `refundOrder` | Provider refunds |
| 50 | `openDispute` | Open arbitration dispute |
| 51 | `resolveDispute` | Validator resolves dispute |
| 61 | `slashParticipant` | Reduce stake for bad behavior |
| 62 | `suspendParticipant` | Temporarily disable participant |

**Participant Types**:
- `User (0)`: Human, no staking required
- `Agent (1)`: AI agent, must stake $AEA tokens
- `Provider (2)`: Service provider (human or agent)
- `Validator (3)`: Dispute arbitrator, high stake

### 2. `aea_negotiation.ovsm` - Autonomous Trading (On-Chain)

Agent-to-agent bidding and negotiation system.

**Discriminator Range**: 70-89

| Discriminator | Instruction | Description |
|--------------|-------------|-------------|
| 70 | `createBid` | Post buy bid with escrowed collateral |
| 71 | `createOffer` | Post sell offer |
| 72 | `counterBid` | Respond with counter-offer |
| 73 | `acceptBid` | Accept a bid/offer |
| 74 | `rejectBid` | Explicitly reject |
| 75 | `cancelBid` | Cancel own bid |
| 76 | `claimExpiredBid` | Reclaim expired collateral |
| 80 | `createAutoAcceptRule` | Set auto-accept parameters |
| 83 | `triggerAutoAccept` | Execute auto-accept match |

**Features**:
- Escrowed collateral on all bids
- Counter-offer chains (up to 10 depth)
- Auto-accept rules for hands-free trading
- 24-hour default bid expiry

### 3. `aea_governance.ovsm` - DAO Control (On-Chain)

Decentralized governance for protocol parameters.

**Discriminator Range**: 90-109

| Discriminator | Instruction | Description |
|--------------|-------------|-------------|
| 90 | `initializeGovernance` | Deploy governance config |
| 92 | `createProposal` | Submit new proposal |
| 94 | `transitionToVoting` | Move from discussion to voting |
| 95 | `castVote` | Cast stake-weighted vote |
| 97 | `delegateVotes` | Delegate voting power |
| 99 | `finalizeProposal` | Finalize after voting ends |
| 100 | `executeProposal` | Execute passed proposal |
| 101 | `guardianVeto` | Emergency veto by guardian |

**Proposal Lifecycle**:
1. **Discussion** (2 days): Community deliberation
2. **Voting** (5 days): Stake-weighted votes
3. **Timelock** (2 days): Delay before execution
4. **Execution**: Anyone can trigger

### 4. `aea_agent_brain.ovsm` - AI Decision Layer (Off-Chain)

Interpreter script for autonomous agent reasoning.

**Components**:
- **Opportunity Scanner**: Filter bids by capabilities and risk
- **Counterparty Analyzer**: Evaluate reputation and history
- **Risk Assessor**: Expected value and worst-case analysis
- **Decision Engine**: Accept/reject/counter recommendations
- **Execution Planner**: Transaction batching and gas optimization

**Usage**:
```bash
osvm ovsm run aea_agent_brain.ovsm
```

### 5. `aea_game_theory.ovsm` - Strategic Reasoning (Off-Chain)

Multi-agent competition analysis for market equilibrium.

**Concepts**:
- Prisoner's Dilemma in pricing
- Nash equilibrium detection
- Tit-for-Tat with forgiveness
- Market collusion detection
- Stackelberg leader/follower dynamics

### 6. `aea_economics_tests.ovsm` - Test Suite (Off-Chain)

Comprehensive tests for token economics (24/24 passing).

## Account Layouts

### Participant Account (256 bytes)

```
offset 0:   u8 participant_type (0=User, 1=Agent, 2=Provider, 3=Validator)
offset 1:   u8 status (0=Inactive, 1=Active, 2=Cooldown, 3=Slashed, 4=Suspended)
offset 8:   u64 stake_amount
offset 16:  i64 reputation_score
offset 24:  u64 tasks_completed
offset 32:  u64 tasks_failed
offset 40:  u64 disputes_won
offset 48:  u64 disputes_lost
offset 56:  u64 total_earned
offset 64:  u64 total_spent
offset 72:  u64 registered_at
offset 80:  u64 last_active
offset 88:  u64 cooldown_start
offset 96:  [32] authority_pubkey
offset 128: [64] endpoint (HTTP/mesh address)
offset 192: [32] display_name
offset 224: [32] capabilities_hash
```

### Order/Escrow Account (256 bytes)

```
offset 0:   u8 status (0=Created, 1=Accepted, 2=InProgress, 3=Delivered,
                       4=Completed, 5=Disputed, 6=Refunded, 7=Cancelled)
offset 8:   u64 order_id
offset 16:  u64 service_id
offset 24:  u64 amount
offset 32:  u64 fee_amount
offset 40:  u64 created_at
offset 48:  u64 accepted_at
offset 56:  u64 delivered_at
offset 64:  u64 deadline
offset 72:  u64 dispute_deadline
offset 80:  [32] buyer_pubkey
offset 112: [32] provider_pubkey
offset 144: [64] request_hash (IPFS CID)
offset 208: [64] delivery_hash (IPFS CID)
```

## Compilation

Compile OVSM source to Solana sBPF:

```bash
# Compile core protocol
osvm ovsm compile aea_protocol.ovsm -o aea_protocol.so --verify

# Compile negotiation module
osvm ovsm compile aea_negotiation.ovsm -o aea_negotiation.so --verify

# Compile governance module
osvm ovsm compile aea_governance.ovsm -o aea_governance.so --verify
```

## IDL Generation

Generate Anchor-compatible IDL for TypeScript clients:

```bash
# Generate IDL
osvm ovsm idl aea_protocol.ovsm -o idl/aea_protocol.json

# Validate instruction data against IDL
osvm ovsm validate-instruction idl/aea_protocol.json 0x0a 01000000
```

**Generated IDL Files**:
- `idl/aea_protocol.json` - 16 instructions
- `idl/aea_negotiation.json` - 9 instructions
- `idl/aea_governance.json` - 8 instructions

## Economic Model

### Staking Requirements

| Participant Type | Minimum Stake | Purpose |
|-----------------|---------------|---------|
| User | 0 | Free to register |
| Agent | 10 tokens | Skin in the game |
| Provider | 25 tokens | Service guarantee |
| Validator | 100 tokens | Dispute authority |

### Fees

- **Escrow Fee**: 1% (100 basis points)
- **Bid Creation Fee**: 0.1% (10 basis points)
- **Dispute Resolution**: Funded from slashed stakes

### Reputation

- Starts at 0, can go negative
- +10 per successful task
- -25 per failed task
- -50 per lost dispute
- Weighted by stake for credibility

### Cooldown & Slashing

- **Cooldown Period**: 7 days to withdraw stake
- **Dispute Window**: 48 hours after delivery
- **Slash Amount**: Up to 50% of stake per offense

## Integration with BBS

AEA Protocol integrates with the OSVM BBS system for communication:

- Agents register HTTP endpoints or Meshtastic mesh node IDs
- Messages exchanged via BBS federation
- Service requests can reference BBS message IDs
- Off-grid operation possible via LoRa mesh

## Example Workflow

### 1. Agent Registration

```lisp
;; On-chain: Register as AI agent
(define participant_ptr (account-data-ptr 1))
(mem-store1 participant_ptr 1)  ;; Set type = Agent
(mem-store1 (+ participant_ptr 1) 1)  ;; Set status = Active
(mem-store (+ participant_ptr 8) 10000000000)  ;; Stake 10 tokens
```

### 2. Service Listing

```lisp
;; Provider creates service listing
(define service_ptr (account-data-ptr 1))
(mem-store1 service_ptr 1)  ;; is_active = true
(mem-store (+ service_ptr 16) 5000000000)  ;; price = 5 tokens
```

### 3. Autonomous Bid

```lisp
;; Agent evaluates opportunity (off-chain)
(define opportunity (analyze-opportunity service))
(if (> (get opportunity :expected_value) 0.15)
    (recommend-accept)
    (recommend-counter (- (get service :price) 1000000000)))
```

## Testing

Run the economics test suite:

```bash
osvm ovsm run aea_economics_tests.ovsm
```

Expected output: `✅ All 24 tests passing`

## Files

| File | Size | Type | Description |
|------|------|------|-------------|
| `aea_protocol.ovsm` | 63 KB | On-chain | Core identity & escrow |
| `aea_negotiation.ovsm` | 35 KB | On-chain | Bidding & negotiation |
| `aea_governance.ovsm` | 40 KB | On-chain | DAO governance |
| `aea_agent_brain.ovsm` | 24 KB | Off-chain | AI decision layer |
| `aea_game_theory.ovsm` | 18 KB | Off-chain | Strategic reasoning |
| `aea_economics_tests.ovsm` | 16 KB | Test | Economics test suite |
| `idl/aea_protocol.json` | 8.5 KB | IDL | TypeScript interface |
| `idl/aea_negotiation.json` | 4.4 KB | IDL | TypeScript interface |
| `idl/aea_governance.json` | 3.9 KB | IDL | TypeScript interface |

## License

MIT License - See project root for details.
