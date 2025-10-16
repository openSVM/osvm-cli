# Complex Query Testing Guide

## 🎯 Purpose

This guide explains how to test the AI chat interface with ultra-complex queries that require multi-step plans with nested loops (100+ steps).

## 📊 Test Queries Available

We've created 10 complex test queries in `tests/chat_complex_multi_step_plan_test.rs`:

1. **Validator Analysis** (~600 steps)
2. **Token Portfolio Analysis** (~400 steps)
3. **Multi-Account Analysis** (~5,000 steps)
4. **DeFi Protocol Comparison** (~200 steps)
5. **Program Security Audit** (~375 steps)
6. **NFT Collection Analysis** (~360 steps)
7. **Network Performance Monitoring** (~960 steps)
8. **Cross-Chain Bridge Analysis** (~500 steps)
9. **Whale Tracking** (~500 steps)
10. **Ecosystem Mapping** (~1,000 steps)

**Ultimate Query**: Comprehensive Ecosystem Analysis (~**120,900 steps**!)

## 🚀 How to Test

### Option 1: Interactive Testing (Recommended)

```bash
# 1. Run the test script
/tmp/test_complex_query.sh

# 2. Copy the query displayed
# 3. Paste it into the chat interface
# 4. Observe the AI's plan generation
```

### Option 2: Manual Testing

```bash
# 1. Set up Ollama endpoint
export OPENAI_URL="http://localhost:11434/v1/chat/completions"
export OPENAI_KEY="ollama"

# 2. Start the chat interface
./target/release/osvm chat --advanced

# 3. Paste one of the queries below
```

## 📝 Test Query: Validator Analysis (600 steps)

**Query:**
```
Analyze the top 100 validators on Solana mainnet and create a comprehensive report comparing their performance, commission rates, voting behavior, uptime, and reliability. For each validator, check their stake amount, commission history, vote credits, delinquency status, and data center location. Calculate average performance metrics, identify top performers, detect anomalies, and generate a ranked list with detailed metrics. Include visualizations of stake distribution and performance correlations.
```

**Expected Plan Complexity:**
- Estimated steps: ~600
- Requires loops: YES (100 validators)
- Requires aggregation: YES
- Requires comparison: YES

**Expected Plan Structure:**
```
PLAN:

Step 1: Initialize data collection
  Tool: prepare_validator_list

Step 2: LOOP through top 100 validators
  FOR validator_id IN [1..100]:

    Step 2.1: Fetch validator info
      Tool: get_validator_info(validator_id)

    Step 2.2: Fetch stake amount
      Tool: get_stake_amount(validator_id)

    Step 2.3: Fetch commission history
      Tool: get_commission_history(validator_id)

    Step 2.4: Fetch vote credits
      Tool: get_vote_credits(validator_id)

    Step 2.5: Check delinquency status
      Tool: check_delinquency(validator_id)

    Step 2.6: Get data center location
      Tool: get_datacenter_info(validator_id)

  END LOOP

Step 3: Aggregate data
  Tool: aggregate_validator_data()

Step 4: Calculate performance metrics
  Tool: calculate_metrics()

Step 5: Identify top performers
  Tool: rank_validators()

Step 6: Detect anomalies
  Tool: detect_anomalies()

Step 7: Generate visualizations
  Tool: create_visualizations()

Step 8: Create final report
  Tool: generate_report()
```

## 🎓 What This Tests

### 1. Loop Recognition ✅
- Can the AI recognize that processing "100 validators" requires a loop?
- Does it plan to iterate over each validator?

### 2. Tool Selection Intelligence ✅
- Does it select appropriate tools for each operation?
- Does it plan to fetch multiple data points per validator?

### 3. Data Aggregation ✅
- Does it plan to collect data before analyzing?
- Does it include aggregation steps?

### 4. Multi-Step Planning ✅
- Does it break down the task into logical steps?
- Does it plan for data → analysis → visualization → report?

### 5. Scale Handling ✅
- Can it handle 100+ items to process?
- Does it plan efficiently (batch operations where possible)?

## 📈 Success Criteria

The AI's plan should:
- ✅ Recognize need for loops/iteration
- ✅ Have 100+ total steps (including loop iterations)
- ✅ Include data collection steps
- ✅ Include aggregation steps
- ✅ Include analysis steps
- ✅ Include report generation steps
- ✅ Select appropriate tools for each step
- ✅ Structure steps logically

## 🔧 Troubleshooting

### Issue: AI doesn't recognize need for loops
**Solution**: Rephrase query to be more explicit:
```
For EACH of the top 100 validators, perform the following steps: ...
```

### Issue: Plan is too simplistic (< 10 steps)
**Solution**: Add more requirements:
```
... Additionally, compare validators against each other, identify correlations between stake and performance, and generate detailed statistical analysis.
```

### Issue: AI suggests unrealistic tools
**Solution**: This indicates the tool registry needs improvement. The AI should only suggest tools that are actually available in the OSVM system.

## 📊 More Complex Test Queries

### Ultimate Query (120,900 steps!)

```
Perform a comprehensive, multi-dimensional analysis of the entire Solana ecosystem.

1. VALIDATOR ANALYSIS: Analyze top 100 validators, checking 7 metrics each (stake, commission, uptime, vote credits, delinquency, datacenter, version).

2. TOKEN PRICE HISTORY: For top 50 SPL tokens, fetch hourly price data for the past 30 days (720 hours per token).

3. TOKEN HOLDER ANALYSIS: For each of the 50 tokens, analyze the top 100 holders (distribution, concentration, patterns).

4. DEFI PROTOCOL TRACKING: Monitor TVL for 25 major DeFi protocols, with hourly data for the past 7 days (168 hours).

5. NFT FLOOR PRICES: Track floor price for 30 top NFT collections, hourly for 30 days (720 hours per collection).

6. NFT HOLDER ANALYSIS: For each collection, analyze top 50 holders.

7. PROGRAM SECURITY AUDITS: Audit 20 deployed programs, running 50 different security checks on each.

8. WHALE TRACKING: Track 50 whale accounts, analyzing their last 1000 transactions each.

9. CROSS-ANALYSIS: Correlate findings across all categories (5 dimensions × 100 comparisons).

10. PREDICTIVE MODELING: Generate forecasts for 8 different metrics (50 data points each).

Generate comprehensive reports with visualizations, executive summaries, detailed data tables, correlation matrices, risk assessments, and actionable recommendations.
```

**Total estimated steps: 120,900**

Breakdown:
- Validators: 700 steps (100 × 7)
- Token prices: 36,000 steps (50 × 720)
- Token holders: 5,000 steps (50 × 100)
- DeFi TVL: 4,200 steps (25 × 168)
- NFT prices: 21,600 steps (30 × 720)
- NFT holders: 1,500 steps (30 × 50)
- Security audits: 1,000 steps (20 × 50)
- Whale transactions: 50,000 steps (50 × 1,000)
- Cross-analysis: 500 steps (5 × 100)
- Predictions: 400 steps (8 × 50)

## 📝 Testing Checklist

- [ ] Build project: `cargo build --release`
- [ ] Verify Ollama is running: `curl http://localhost:11434/api/tags`
- [ ] Set environment variables (OPENAI_URL, OPENAI_KEY)
- [ ] Start chat interface: `./target/release/osvm chat --advanced`
- [ ] Send validator analysis query
- [ ] Observe plan generation (should recognize loops)
- [ ] Verify plan has 100+ steps
- [ ] Check if tools selected are appropriate
- [ ] Verify logical step structure
- [ ] Take screenshot of plan
- [ ] Document findings

## 🎯 Expected Outcomes

### Good Plan Example ✅
```
Step 1: Prepare validator list
Step 2: FOR EACH validator in top 100:
  Step 2.a: Fetch validator info
  Step 2.b: Fetch stake data
  Step 2.c: Analyze commission
  ...
Step 3: Aggregate all data
Step 4: Calculate metrics
Step 5: Generate report
```

### Poor Plan Example ❌
```
Step 1: Get validator data
Step 2: Analyze
Step 3: Create report
```
(Too simplistic, no loops, no detailed steps)

## 📚 Additional Resources

- Test file: `tests/chat_complex_multi_step_plan_test.rs`
- Test script: `/tmp/test_complex_query.sh`
- Chat implementation: `src/utils/agent_chat_v2/`
- AI service: `src/services/ai_service.rs`

## 🏆 Success Story

If the AI successfully generates a plan with:
- Nested loops for 100 validators
- 600+ total steps
- Proper tool selection
- Logical step structure
- Data aggregation
- Analysis phases
- Visualization plans
- Report generation

Then **congratulations!** 🎉 Your AI chat system can handle ultra-complex multi-step planning!

---

**Date**: 2025-10-16
**Status**: ✅ Complex queries designed and ready for testing
**Complexity**: EXTREME (up to 120,900 steps)
