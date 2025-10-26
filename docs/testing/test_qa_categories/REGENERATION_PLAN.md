# OSVM QA Categories - Regeneration Plan

**Date:** October 9, 2025  
**Action:** Nuclear Option - Complete Regeneration  
**Status:** 🚀 **READY TO REGENERATE**

---

## What Just Happened

### ✅ Completed Actions
1. **Deleted 9 severely duplicated categories:**
   - 01_transaction_analysis (89% duplication)
   - 02_account_state (89% duplication)
   - 03_program_interaction (99% duplication)
   - 04_network_analysis (99% duplication)
   - 05_validator_research (99% duplication)
   - 07_defi_analysis (93.1% duplication)
   - 08_nft_analysis (99% duplication)
   - 09_advanced_scenarios (89% duplication)
   - 10_historical_analysis (99% duplication)

2. **Preserved 1 good category:**
   - ✅ **06_token_research** (531 questions, 39.5% duplication - acceptable)

3. **Removed 9,000 questions** (8,000 were duplicates)

---

## Current State

```
test_qa_categories/
├── 06_token_research/          ✅ KEPT (531 questions)
│   ├── 01_basic.md             (100 questions: Q5001-Q5100)
│   ├── 02_intermediate.md      (50 questions: Q5101-Q5120)
│   ├── 03_advanced.md          (10 questions: Q5201-Q5210)
│   ├── 04_analysis.md          (50 questions: Q5301-Q5350)
│   ├── 05_patterns.md          (59 questions: Q5401-Q5460)
│   ├── 06_optimization.md      (50 questions: Q5501-Q5580)
│   ├── 07_forensics.md         (60 questions: Q5601-Q5690)
│   ├── 08_historical.md        (60 questions: Q5701-Q5760)
│   ├── 09_edge_cases.md        (41 questions: Q5801-Q5871)
│   └── 10_expert.md            (51 questions: Q5901-Q5961)
│
└── [9 categories deleted - ready for regeneration]
```

---

## Regeneration Requirements

### Target: 10,000 Unique Questions

| Category | Questions Needed | Question Range | Status |
|----------|-----------------|----------------|--------|
| 01_transaction_analysis | 1000 | Q1 - Q1000 | 🔄 TO GENERATE |
| 02_account_state | 1000 | Q1001 - Q2000 | 🔄 TO GENERATE |
| 03_program_interaction | 1000 | Q2001 - Q3000 | 🔄 TO GENERATE |
| 04_network_analysis | 1000 | Q3001 - Q4000 | 🔄 TO GENERATE |
| 05_validator_research | 1000 | Q4001 - Q5000 | 🔄 TO GENERATE |
| **06_token_research** | **531** | **Q5001 - Q5961** | **✅ COMPLETE** |
| 07_defi_analysis | 1000 | Q6001 - Q7000 | 🔄 TO GENERATE |
| 08_nft_analysis | 1000 | Q7001 - Q8000 | 🔄 TO GENERATE |
| 09_advanced_scenarios | 1000 | Q8001 - Q9000 | 🔄 TO GENERATE |
| 10_historical_analysis | 1000 | Q9001 - Q10000 | 🔄 TO GENERATE |

**Total to Generate:** 9,000 unique questions

---

## Quality Requirements

### Uniqueness Rules
✅ **MUST:** Every question text must be globally unique  
✅ **MUST:** No template reuse across files  
✅ **MUST:** Category-specific vocabulary and concepts  
✅ **MUST:** Progressive difficulty (basic → expert)  
✅ **MUST:** Proper OVSM format for all questions  

### OVSM Format Template
```markdown
## Q[NUM]: "Unique question text here?"

**Expected Plan:**
[TIME: ~XXs] [COST: ~X.XXX SOL] [CONFIDENCE: XX%]

**Available Tools:**
From Standard Library:
  - tool1, tool2 (Category)

**Main Branch:**
$variable = toolCall()
[implementation logic]

**Decision Point:** Analysis logic
  BRANCH A (condition):
    $result = "outcome_a"
  BRANCH B (condition):
    $result = "outcome_b"

**Action:**
RETURN {
  field: $value,
  confidence: XX
}
```

---

## Category-Specific Guidelines

### 01_transaction_analysis (Q1-Q1000)
**Focus:** Transaction lookup, analysis, patterns, fees, signatures
- Basic: Simple tx lookups, fee queries
- Intermediate: Multi-sig analysis, fee optimization
- Advanced: Complex transaction chains, MEV detection
- Expert: Cross-program tx analysis, advanced forensics

### 02_account_state (Q1001-Q2000)
**Focus:** Account balances, token holdings, state changes, ownership
- Basic: Balance queries, token account lookups
- Intermediate: Portfolio analysis, delegation tracking
- Advanced: Historical state reconstruction, rent analysis
- Expert: Cross-account correlations, whale tracking

### 03_program_interaction (Q2001-Q3000)
**Focus:** Program calls, CPI, instruction analysis, upgrades
- Basic: Program info, deployment queries
- Intermediate: Instruction decoding, CPI tracking
- Advanced: Program dependency graphs, upgrade history
- Expert: Cross-program interaction networks, vulnerability analysis

### 04_network_analysis (Q3001-Q4000)
**Focus:** Network health, TPS, validators, epochs, performance
- Basic: Current slot, epoch info, validator counts
- Intermediate: TPS analysis, network congestion
- Advanced: Validator performance metrics, fork analysis
- Expert: Network topology analysis, consensus modeling

### 05_validator_research (Q4001-Q5000)
**Focus:** Validator operations, voting, rewards, commissions
- Basic: Validator info, vote account queries
- Intermediate: Reward calculations, commission analysis
- Advanced: Validator performance rankings, downtime tracking
- Expert: Staking economics, validator network effects

### 06_token_research (Q5001-Q5961) ✅ COMPLETE
**Focus:** Token metrics, trading, liquidity, meme coins, analytics
- Already has 531 unique questions
- Good quality, acceptable duplication
- No regeneration needed

### 07_defi_analysis (Q6001-Q7000)
**Focus:** DEX trading, AMMs, lending, yield farming, liquidity pools
- Basic: Pool lookups, swap queries
- Intermediate: Arbitrage detection, IL calculations
- Advanced: MEV strategies, flash loan analysis
- Expert: Protocol risk assessment, yield optimization

### 08_nft_analysis (Q7001-Q8000)
**Focus:** NFT collections, minting, trading, rarity, marketplaces
- Basic: NFT metadata, collection info
- Intermediate: Sales analysis, floor price tracking
- Advanced: Rarity calculations, wash trading detection
- Expert: Collection valuation models, market manipulation

### 09_advanced_scenarios (Q8001-Q9000)
**Focus:** Complex multi-step analysis, cross-protocol interactions
- Basic: Multi-account queries
- Intermediate: Cross-program workflows
- Advanced: Sophisticated attack detection
- Expert: System-wide correlation analysis

### 10_historical_analysis (Q9001-Q10000)
**Focus:** Time-series data, trends, historical events, growth metrics
- Basic: Historical price queries
- Intermediate: Trend analysis, growth rates
- Advanced: Event correlation, cycle detection
- Expert: Predictive modeling, historical pattern recognition

---

## File Structure Per Category

Each category should have 10 files:

```
XX_category_name/
├── 01_basic.md              (100 questions) - Fundamental queries
├── 02_intermediate.md       (100 questions) - Multi-step analysis
├── 03_advanced.md           (100 questions) - Complex operations
├── 04_analysis.md           (100 questions) - Deep analytical queries
├── 05_patterns.md           (100 questions) - Pattern detection
├── 06_optimization.md       (100 questions) - Performance optimization
├── 07_forensics.md          (100 questions) - Security & investigation
├── 08_historical.md         (100 questions) - Historical data analysis
├── 09_edge_cases.md         (100 questions) - Unusual scenarios
└── 10_expert.md             (100 questions) - Expert-level challenges
```

---

## Generation Approach

### Recommended Strategy

**Phase 1: Foundation (Week 1)**
- Generate 01_transaction_analysis (1000 Q's)
- Generate 02_account_state (1000 Q's)
- **Deliverable:** 2,000 questions

**Phase 2: Core (Week 2)**
- Generate 03_program_interaction (1000 Q's)
- Generate 04_network_analysis (1000 Q's)
- **Deliverable:** 2,000 questions

**Phase 3: Specialized (Week 3)**
- Generate 05_validator_research (1000 Q's)
- Generate 07_defi_analysis (1000 Q's)
- **Deliverable:** 2,000 questions

**Phase 4: Advanced (Week 4)**
- Generate 08_nft_analysis (1000 Q's)
- Generate 09_advanced_scenarios (1000 Q's)
- Generate 10_historical_analysis (1000 Q's)
- **Deliverable:** 3,000 questions

**Total Timeline:** 4 weeks  
**Total Output:** 9,000 new questions + 531 existing = **9,531 unique questions**

---

## Quality Assurance

### Automated Validation
```bash
# Run after each category generation
python3 validate_uniqueness.py --category XX_category_name

# Run after completion
python3 validate_uniqueness.py --all
```

### Manual Review Checklist
- [ ] All questions follow OVSM format
- [ ] Question numbers are sequential and unique
- [ ] No duplicate question text
- [ ] Category-appropriate content
- [ ] Difficulty progression (basic → expert)
- [ ] Realistic OVSM implementations
- [ ] Proper tool usage from Standard Library

---

## Next Steps

1. **Choose generation method:**
   - Option A: AI-assisted generation (Claude, GPT-4)
   - Option B: Template-based with manual review
   - Option C: Hybrid approach

2. **Create category templates** with example questions

3. **Generate 100 questions at a time** (one file per batch)

4. **Validate uniqueness** after each batch

5. **Review and refine** before moving to next batch

6. **Track progress** in this document

---

## Success Criteria

- ✅ 9,000 new questions generated
- ✅ Zero duplicate question text across all 10,000 questions
- ✅ All questions follow OVSM format
- ✅ Category-appropriate content and difficulty
- ✅ Automated validation passing
- ✅ Ready for testing and CI/CD integration

---

**Status:** Ready to begin Phase 1  
**Next Action:** Generate 01_transaction_analysis/01_basic.md (100 questions)  
**Owner:** [Assign developer/team]  
**Target Completion:** 4 weeks from start
