# Solana Blockchain Research Q&A Dataset# Solana Blockchain Research Q&A Dataset



## Overview## Overview

This comprehensive dataset contains research questions organized in OVSM (Open Versatile Seeker Mind) planning format for testing Solana blockchain analysis tools. Each category contains 10 difficulty levels with progressively complex scenarios including forensics, debugging, optimization, and edge case handling.This dataset contains 10000 research questions organized in OVSM planning format for testing Solana blockchain analysis tools.



## Structure## Structure



The dataset is organized into **10 major categories**, each with **10 difficulty levels**:### 01_transaction_analysis - Transaction Analysis

**Focus:** Transaction queries, fees, instructions, CPIs

### 01_transaction_analysis - Transaction Analysis

**Focus:** Transaction queries, fees, instructions, CPIs, MEV detection**Files:**

- `01_basic.md` - Q1-Q100: Simple transaction lookups and fee calculations

**Difficulty Levels:**- `02_intermediate.md` - Q101-Q200: Multi-instruction transactions and CPI analysis

- `01_basic.md` - Simple transaction lookups and fee calculations- `03_advanced.md` - Q201-Q300: Complex transaction patterns and optimization

- `02_intermediate.md` - Multi-instruction transactions and balance analysis- `04_analysis.md` - Q301-Q400: Transaction flow and dependency analysis

- `03_advanced.md` - Complex CPI chains and priority fee optimization- `05_patterns.md` - Q401-Q500: Common transaction patterns and signatures

- `04_analysis.md` - Transaction flow and dependency analysis- `06_optimization.md` - Q501-Q600: Fee optimization and batching strategies

- `05_patterns.md` - Common transaction patterns and signatures- `07_forensics.md` - Q601-Q700: Failed transaction investigation

- `06_optimization.md` - Fee optimization and compute unit efficiency- `08_historical.md` - Q701-Q800: Transaction history and trends

- `07_forensics.md` - Failed transaction debugging and MEV detection- `09_edge_cases.md` - Q801-Q900: Unusual transaction scenarios

- `08_historical.md` - Transaction history and trend analysis- `10_expert.md` - Q901-Q1000: Advanced transaction forensics and MEV

- `09_edge_cases.md` - Unusual transaction scenarios and error handling

- `10_expert.md` - Advanced transaction forensics and exploit detection### 02_account_state - Account State

**Focus:** Balances, ownership, PDAs, stake accounts

### 02_account_state - Account State

**Focus:** Balances, ownership, PDAs, stake accounts, rent economics**Files:**

- `01_basic.md` - Q1-Q100: Account balance and ownership queries

**Difficulty Levels:**- `02_intermediate.md` - Q101-Q200: PDA derivation and validation

- `01_basic.md` - Account balance and ownership queries- `03_advanced.md` - Q201-Q300: Complex account relationships

- `02_intermediate.md` - PDA derivation and validation- `04_analysis.md` - Q301-Q400: Account state evolution analysis

- `03_advanced.md` - Complex account relationships- `05_patterns.md` - Q401-Q500: Account usage patterns

- `04_analysis.md` - Account state evolution analysis- `06_optimization.md` - Q501-Q600: Efficient account queries

- `05_patterns.md` - Account usage patterns- `07_forensics.md` - Q601-Q700: Account anomaly detection

- `06_optimization.md` - Efficient account queries and batching- `08_historical.md` - Q701-Q800: Account state history

- `07_forensics.md` - Account draining detection, ownership tracing, rent debugging- `09_edge_cases.md` - Q801-Q900: Edge cases in account state

- `08_historical.md` - Account state history and timeline reconstruction- `10_expert.md` - Q901-Q1000: Advanced PDA and account forensics

- `09_edge_cases.md` - Edge cases in account state and data validation

- `10_expert.md` - Advanced PDA forensics and account cloning detection### 03_program_interaction - Program Interaction

**Focus:** Deployments, upgrades, IDLs, invocations

### 03_program_interaction - Program Interaction

**Focus:** Program deployments, upgrades, IDLs, invocations**Files:**

- `01_basic.md` - Q1-Q100: Basic program queries

**Difficulty Levels:**- `02_intermediate.md` - Q101-Q200: Program invocation analysis

- `01_basic.md` - Basic program queries and information retrieval- `03_advanced.md` - Q201-Q300: Complex program interactions

- `02_intermediate.md` - Program invocation analysis- `04_analysis.md` - Q301-Q400: Program usage analysis

- `03_advanced.md` - Complex program interactions and CPI analysis- `05_patterns.md` - Q401-Q500: Program interaction patterns

- `04_analysis.md` - Program usage and adoption analysis- `06_optimization.md` - Q501-Q600: Program call optimization

- `05_patterns.md` - Program interaction patterns and best practices- `07_forensics.md` - Q601-Q700: Program exploit detection

- `06_optimization.md` - Program call optimization and compute efficiency- `08_historical.md` - Q701-Q800: Program evolution tracking

- `07_forensics.md` - Program exploit detection and vulnerability analysis- `09_edge_cases.md` - Q801-Q900: Unusual program behaviors

- `08_historical.md` - Program evolution tracking and upgrade history- `10_expert.md` - Q901-Q1000: Advanced program security analysis

- `09_edge_cases.md` - Unusual program behaviors and error scenarios

- `10_expert.md` - Advanced program security auditing and attack vectors### 04_network_analysis - Network Analysis

**Focus:** Slots, blocks, TPS, epochs, cluster health

### 04_token_research - Token Research

**Focus:** SPL tokens, mints, supply, transfers, token economics**Files:**

- `01_basic.md` - Q1-Q100: Basic network metrics

**Difficulty Levels:**- `02_intermediate.md` - Q101-Q200: Block and slot analysis

- `01_basic.md` - Token balance and supply queries- `03_advanced.md` - Q201-Q300: Network performance analysis

- `02_intermediate.md` - Token transfer analysis- `04_analysis.md` - Q301-Q400: Cluster health monitoring

- `03_advanced.md` - Token authority and mint analysis- `05_patterns.md` - Q401-Q500: Network usage patterns

- `04_analysis.md` - Token distribution and holder analysis- `06_optimization.md` - Q501-Q600: Network timing optimization

- `05_patterns.md` - Token usage patterns and liquidity- `07_forensics.md` - Q601-Q700: Network issue investigation

- `06_optimization.md` - Efficient token queries and batch operations- `08_historical.md` - Q701-Q800: Historical network trends

- `07_forensics.md` - Token manipulation detection and fake token identification- `09_edge_cases.md` - Q801-Q900: Network edge cases

- `08_historical.md` - Token price history and volume trends- `10_expert.md` - Q901-Q1000: Advanced network diagnostics

- `09_edge_cases.md` - Edge cases in token standards and compatibility

- `10_expert.md` - Advanced token forensics and economic analysis### 05_validator_research - Validator Research

**Focus:** Validators, stake, rewards, performance

### 05_defi_analysis - DeFi Analysis

**Focus:** DEXs, lending, AMMs, liquidity pools, yield farming**Files:**

- `01_basic.md` - Q1-Q100: Basic validator queries

**Difficulty Levels:**- `02_intermediate.md` - Q101-Q200: Stake and delegation analysis

- `01_basic.md` - Basic DEX queries and swap analysis- `03_advanced.md` - Q201-Q300: Validator performance metrics

- `02_intermediate.md` - Liquidity pool analysis- `04_analysis.md` - Q301-Q400: Validator behavior analysis

- `03_advanced.md` - Complex DeFi protocol interactions- `05_patterns.md` - Q401-Q500: Staking patterns

- `04_analysis.md` - DeFi protocol usage and TVL analysis- `06_optimization.md` - Q501-Q600: Stake optimization strategies

- `05_patterns.md` - DeFi trading patterns and strategies- `07_forensics.md` - Q601-Q700: Validator issue detection

- `06_optimization.md` - Route optimization and slippage minimization- `08_historical.md` - Q701-Q800: Historical validator data

- `07_forensics.md` - Flash loan attack detection, liquidity manipulation, sandwich attacks- `09_edge_cases.md` - Q801-Q900: Validator edge cases

- `08_historical.md` - DeFi protocol evolution and market trends- `10_expert.md` - Q901-Q1000: Advanced validator analytics

- `09_edge_cases.md` - Edge cases in DeFi protocols and error scenarios

- `10_expert.md` - Advanced DeFi security analysis and exploit detection### 06_token_research - Token Research

**Focus:** Token supply, holders, metadata, transfers

### 06_nft_analysis - NFT Analysis

**Focus:** NFT mints, metadata, collections, marketplaces**Files:**

- `01_basic.md` - Q1-Q100: Basic token queries

**Difficulty Levels:**- `02_intermediate.md` - Q101-Q200: Token holder analysis

- `01_basic.md` - NFT metadata and ownership queries- `03_advanced.md` - Q201-Q300: Token distribution analysis

- `02_intermediate.md` - NFT collection analysis- `04_analysis.md` - Q301-Q400: Token flow analysis

- `03_advanced.md` - NFT marketplace interactions- `05_patterns.md` - Q401-Q500: Token usage patterns

- `04_analysis.md` - NFT collection statistics and trends- `06_optimization.md` - Q501-Q600: Token query optimization

- `05_patterns.md` - NFT trading patterns and collector behavior- `07_forensics.md` - Q601-Q700: Token anomaly detection

- `06_optimization.md` - Efficient NFT queries and metadata fetching- `08_historical.md` - Q701-Q800: Token history tracking

- `07_forensics.md` - Wash trading detection, price manipulation, fake NFTs- `09_edge_cases.md` - Q801-Q900: Token edge cases

- `08_historical.md` - NFT price history and market trends- `10_expert.md` - Q901-Q1000: Advanced token analytics

- `09_edge_cases.md` - Edge cases in NFT standards and metadata

- `10_expert.md` - Advanced NFT forensics and market manipulation detection### 07_defi_analysis - DeFi Analysis

**Focus:** DEX, lending, liquidity, farming, oracles

### 07_network_analysis - Network Analysis

**Focus:** Network performance, validators, epochs, voting, consensus**Files:**

- `01_basic.md` - Q1-Q100: Basic DeFi queries

**Difficulty Levels:**- `02_intermediate.md` - Q101-Q200: Liquidity pool analysis

- `01_basic.md` - Basic network metrics and health queries- `03_advanced.md` - Q201-Q300: Complex DeFi strategies

- `02_intermediate.md` - Validator performance analysis- `04_analysis.md` - Q301-Q400: DeFi protocol analysis

- `03_advanced.md` - Network consensus and voting analysis- `05_patterns.md` - Q401-Q500: DeFi usage patterns

- `04_analysis.md` - Network utilization and throughput analysis- `06_optimization.md` - Q501-Q600: DeFi optimization strategies

- `05_patterns.md` - Network usage patterns and traffic analysis- `07_forensics.md` - Q601-Q700: DeFi exploit detection

- `06_optimization.md` - Network efficiency and performance optimization- `08_historical.md` - Q701-Q800: DeFi historical trends

- `07_forensics.md` - Network degradation debugging, DDoS detection, spam analysis- `09_edge_cases.md` - Q801-Q900: DeFi edge cases

- `08_historical.md` - Network performance trends and epoch analysis- `10_expert.md` - Q901-Q1000: Advanced DeFi analytics

- `09_edge_cases.md` - Edge cases in network behavior and consensus

- `10_expert.md` - Advanced network forensics and attack detection### 08_nft_analysis - NFT Analysis

**Focus:** NFTs, collections, metadata, trading

### 08_validator_research - Validator Research

**Focus:** Validator operations, rewards, performance, stake distribution**Files:**

- `01_basic.md` - Q1-Q100: Basic NFT queries

**Difficulty Levels:**- `02_intermediate.md` - Q101-Q200: Collection analysis

- `01_basic.md` - Validator information and status queries- `03_advanced.md` - Q201-Q300: NFT trading analysis

- `02_intermediate.md` - Validator performance metrics- `04_analysis.md` - Q301-Q400: NFT market analysis

- `03_advanced.md` - Validator voting and consensus participation- `05_patterns.md` - Q401-Q500: NFT trading patterns

- `04_analysis.md` - Validator economics and reward analysis- `06_optimization.md` - Q501-Q600: NFT query optimization

- `05_patterns.md` - Validator behavior patterns and strategies- `07_forensics.md` - Q601-Q700: NFT fraud detection

- `06_optimization.md` - Validator performance optimization- `08_historical.md` - Q701-Q800: NFT historical data

- `07_forensics.md` - Validator issue debugging, delinquency analysis, MEV extraction- `09_edge_cases.md` - Q801-Q900: NFT edge cases

- `08_historical.md` - Validator performance history and trends- `10_expert.md` - Q901-Q1000: Advanced NFT analytics

- `09_edge_cases.md` - Edge cases in validator operations

- `10_expert.md` - Advanced validator forensics and operational analysis### 09_advanced_scenarios - Advanced Scenarios

**Focus:** MEV, clustering, rugpulls, governance

### 09_historical_analysis - Historical Analysis

**Focus:** Trends, time-series analysis, archival queries, data archaeology**Files:**

- `01_basic.md` - Q1-Q100: Basic advanced queries

**Difficulty Levels:**- `02_intermediate.md` - Q101-Q200: MEV detection basics

- `01_basic.md` - Basic historical queries and time ranges- `03_advanced.md` - Q201-Q300: Complex attack patterns

- `02_intermediate.md` - Time-series data collection- `04_analysis.md` - Q301-Q400: Security analysis

- `03_advanced.md` - Historical pattern recognition- `05_patterns.md` - Q401-Q500: Attack patterns

- `04_analysis.md` - Statistical analysis of historical data- `06_optimization.md` - Q501-Q600: Detection optimization

- `05_patterns.md` - Long-term trend identification- `07_forensics.md` - Q601-Q700: Forensic investigation

- `06_optimization.md` - Efficient historical data queries- `08_historical.md` - Q701-Q800: Historical exploit data

- `07_forensics.md` - Historical forensics, deleted data recovery, timeline reconstruction- `09_edge_cases.md` - Q801-Q900: Exploit edge cases

- `08_historical.md` - Deep historical analysis and archival research- `10_expert.md` - Q901-Q1000: Expert-level security analysis

- `09_edge_cases.md` - Edge cases in historical data and archival nodes

- `10_expert.md` - Advanced data archaeology and historical exploit analysis### 10_historical_analysis - Historical Analysis

**Focus:** Historical data, replays, time-series

### 10_advanced_scenarios - Advanced Scenarios

**Focus:** MEV, arbitrage, exploits, multi-protocol analysis, smart money tracking**Files:**

- `01_basic.md` - Q1-Q100: Basic historical queries

**Difficulty Levels:**- `02_intermediate.md` - Q101-Q200: Time-series analysis

- `01_basic.md` - Basic multi-transaction analysis- `03_advanced.md` - Q201-Q300: Complex historical patterns

- `02_intermediate.md` - Cross-protocol interactions- `04_analysis.md` - Q301-Q400: Trend analysis

- `03_advanced.md` - Complex MEV strategies- `05_patterns.md` - Q401-Q500: Historical patterns

- `04_analysis.md` - Smart money and whale tracking- `06_optimization.md` - Q501-Q600: Query optimization

- `05_patterns.md` - Advanced trading patterns and strategies- `07_forensics.md` - Q601-Q700: Historical forensics

- `06_optimization.md` - Multi-protocol optimization strategies- `08_historical.md` - Q701-Q800: Long-term trends

- `07_forensics.md` - Advanced exploit detection and security forensics- `09_edge_cases.md` - Q801-Q900: Historical edge cases

- `08_historical.md` - Historical exploit analysis and lessons learned- `10_expert.md` - Q901-Q1000: Advanced historical analytics

- `09_edge_cases.md` - Edge cases in complex scenarios

- `10_expert.md` - Master-level blockchain forensics and exploit analysis## Format



## OVSM Language Features UsedEach question follows OVSM planning syntax:

- **Question Header**: Number and text

Each question demonstrates various OVSM language features:- **Expected Plan**: Time and cost estimates

- **Available Tools**: RPC methods and utilities

### Control Flow- **Main Branch**: OVSM code with variables and tool calls

- **Conditional Branching:** `DECISION Point` with `BRANCH A/B/C/D`- **Decision Point**: Conditional logic and branching

- **Loops:** `FOR`, `WHILE`, `LOOP EVERY`- **Action**: Expected output description

- **Error Handling:** `TRY/CATCH` with `FATAL/RECOVERABLE/WARNING`

- **Guards:** `GUARD condition ELSE`## Usage



### Data ProcessingThese questions can be used for:

- **Transformations:** `MAP`, `FILTER`, `REDUCE`, `FLATTEN`1. Testing blockchain analysis tools

- **Aggregations:** `SUM`, `AVG`, `COUNT`, `MAX`, `MIN`2. Training AI models on Solana queries

- **Statistical:** `MEAN`, `STDDEV`, `PERCENTILE`, `CORRELATE`3. Benchmarking query performance

- **Pattern Detection:** `FIND_PATTERNS`, `DETECT_OUTLIERS`4. Educational purposes

5. Developing automated testing suites

### Parallel Execution
- **Concurrent Operations:** `PARALLEL { } WAIT_ALL`
- **Race Conditions:** `WAIT_ANY`, `RACE`

### Tools Used
- **Solana RPC:** 18 tools (getTransaction, getBlock, getAccountInfo, etc.)
- **Data Processing:** 27 tools
- **Statistical Analysis:** 14 tools
- **Solana Utilities:** 13 tools
- **Custom Tools:** Domain-specific tools for forensics and analysis

## Usage

### Testing AI Planning Capabilities
Use these questions to test AI models' ability to:
1. Plan complex multi-step research workflows
2. Handle conditional logic and branching scenarios
3. Implement parallel execution strategies
4. Apply statistical analysis techniques
5. Perform forensic analysis and debugging

### Validating OSVM Implementations
Test your OSVM interpreter/executor with:
1. Progressive difficulty levels (basic → expert)
2. Category-specific tool usage
3. Error handling and recovery
4. Performance optimization scenarios

### Training Data
Use as training data for:
1. Blockchain analysis AI agents
2. Forensics and security tools
3. DeFi and trading bots
4. Network monitoring systems

## Question Format

Each question follows this structure:

```markdown
## Q1: "Clear question statement"

**Expected Plan:**
[TIME: estimate] [COST: estimate] [CONFIDENCE: percentage]

**Available Tools:**
From Standard Library:
  - List of tools used

**Main Branch:**
// Primary execution path with OVSM code

**Decision Point:** What's being decided
  BRANCH A (condition):
    // Actions for branch A
  BRANCH B (condition):
    // Actions for branch B

**Action:**
RETURN {
  // Structured result
  confidence: XX,
  caveats: ["limitations"]
}
```

## Metrics

- **Total Categories:** 10
- **Files per Category:** 10
- **Total Files:** 100
- **Estimated Questions:** 500+ (5+ per file)
- **Coverage:** Basic queries to advanced forensics
- **Tool Diversity:** 70+ unique tools from standard library

## Contributing

When adding new questions:
1. Follow the OVSM syntax strictly
2. Include time, cost, and confidence estimates
3. Provide realistic signatures/addresses (or placeholders)
4. Add appropriate error handling
5. Include caveats for limitations
6. Place in appropriate difficulty level

## License

This dataset is part of the OSVM CLI project and follows the same license.

## Version

**Version:** 2.0
**Last Updated:** 2025-10-11
**OVSM Spec:** v1.1
