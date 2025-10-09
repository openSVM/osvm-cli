# Test QA Categories - File Status

## Refined Files (Proper OVSM Syntax) ✅

These files have been manually refined with correct OVSM syntax:

### Category 01: Transaction Analysis
- ✅ `01_basic.md` - Q1-Q12 (12 questions with full OVSM syntax)
  - Proper $variables, tool calls, DECISION/BRANCH, error handling
  - Uses: getTransaction, getSlot, getBlock, MAP, FILTER, MEAN, etc.

### Category 02: Account State
- ✅ `01_basic.md` - Q1-Q2 (2 questions with full OVSM syntax)
  - Account balance, token holdings queries
  - Uses: getBalance, getTokenAccountsByOwner, FILTER, COUNT

### Category 07: DeFi Analysis
- ✅ `01_basic.md` - Q1-Q4 (4 questions with full OVSM syntax)
  - Pool analysis, swap detection, MEV detection
  - Uses: getAccountInfo, parseU64, parseU128, POW, GROUP_BY

### Category 09: Advanced Scenarios
- ✅ `01_basic.md` - Q1-Q4 (4 questions with full OVSM syntax)
  - Sandwich attacks, wallet clustering, rugpull detection, MEV bots
  - Uses: CORRELATE, FIND, GROUP_BY, pattern analysis

**Total Refined**: 22 questions with proper OVSM syntax

---

## Generated Files (Template Format) 📝

These files were auto-generated and use simplified template format:

### All Categories (01-10)
Each category has 10 files with placeholder format:
- Uses generic `TOOL.method()` syntax
- Has structure but needs OVSM refinement
- Contains 100 questions per file

**Total Generated**: 9,978 questions in template format

---

## File Structure

```
test_qa_categories/
├── 01_transaction_analysis/
│   ├── 01_basic.md ✅ (REFINED - 12 questions proper OVSM)
│   ├── 02_intermediate.md 📝 (100 questions, needs refinement)
│   ├── 03_advanced.md 📝 (100 questions, needs refinement)
│   └── ... (files 04-10, all need refinement)
│
├── 02_account_state/
│   ├── 01_basic.md ✅ (REFINED - 2 questions proper OVSM)
│   ├── 02-10 📝 (all need refinement)
│
├── 03-06/ 📝 (all files need refinement)
│
├── 07_defi_analysis/
│   ├── 01_basic.md ✅ (REFINED - 4 questions proper OVSM)
│   ├── 02-10 📝 (all need refinement)
│
├── 08/ 📝 (all files need refinement)
│
├── 09_advanced_scenarios/
│   ├── 01_basic.md ✅ (REFINED - 4 questions proper OVSM)
│   ├── 02-10 📝 (all need refinement)
│
└── 10/ 📝 (all files need refinement)
```

---

## Status Summary

**Refined Questions**: 22 / 10,000 (0.22%)
**Generated Questions**: 9,978 / 10,000 (99.78%)

**Refined Files**: 4 / 100 (4%)
**Generated Files**: 96 / 100 (96%)

---

## Next Steps

### Priority Refinement (Recommended)
Focus on completing the 01_basic.md file for each category:

1. ✅ 01_transaction_analysis/01_basic.md (DONE)
2. ✅ 02_account_state/01_basic.md (DONE)
3. ⏳ 03_program_interaction/01_basic.md (TODO)
4. ⏳ 04_network_analysis/01_basic.md (TODO)
5. ⏳ 05_validator_research/01_basic.md (TODO)
6. ⏳ 06_token_research/01_basic.md (TODO)
7. ✅ 07_defi_analysis/01_basic.md (DONE)
8. ⏳ 08_nft_analysis/01_basic.md (TODO)
9. ✅ 09_advanced_scenarios/01_basic.md (DONE)
10. ⏳ 10_historical_analysis/01_basic.md (TODO)

**Estimated Work**: 6 more basic files × 10-15 questions each = ~60-90 questions

### Full Refinement (Long-term)
To refine all 10,000 questions:
- Estimated time: 40-60 hours
- Could be done incrementally
- Or use AI assistance to batch convert

---

## Template vs OVSM Comparison

### Template Format (Generated)
```
$result = TOOL.getTransaction($sig)
PARSE($result)
ANALYZE($data)
```
**Issues**:
- Uses TOOL.method() syntax
- Undefined functions (PARSE, ANALYZE)
- Generic placeholders

### Proper OVSM (Refined)
```
$signature = "5J8..."
TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$fee = $tx.meta.fee
```
**Correct**:
- Uses real tool calls
- Proper $variables
- Error handling
- Returns real data

---

## Usage Recommendation

**For Testing**:
- Use refined files as golden examples
- Use generated files for structure reference

**For Production**:
- Prioritize refining categories you'll actually use
- Or build converter to fix template → OVSM automatically

---

**Status**: 4 categories have refined basic examples
**Next**: Refine remaining 6 category basic files for complete coverage
