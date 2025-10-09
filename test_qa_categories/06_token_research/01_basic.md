# Token Research - Basic Questions (Q1-Q100)

**Category:** Token Research
**Difficulty:** Basic
**Focus:** Creative token insights, supply dynamics, holder psychology, transfer patterns, metadata mysteries
**Questions:** Q1-Q100

---

## Q1: "What hidden stories does the token supply distribution tell about EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v's journey?"

**Expected Plan:**

[TIME: ~12s] [COST: free] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - getTokenSupply, getTokenLargestAccounts (Solana RPC)
  - MAP, FILTER, COUNT, SUM, SORT_BY (Data Processing)
  - MEAN, STDDEV, PERCENTILE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

TRY:
  $supply_info = getTokenSupply(mint: $target_token)
  $largest_accounts = getTokenLargestAccounts(mint: $target_token, limit: 20)
CATCH FATAL:
  RETURN ERROR(message: "Token data unavailable")

GUARD $supply_info != null AND $largest_accounts != null ELSE
  RETURN ERROR(message: "Incomplete token data")

$total_supply = parseU64($supply_info.amount)
$top_holders = MAP(collection: $largest_accounts, fn: account => {
  address: account.address,
  amount: parseU64(account.amount),
  percentage: (parseU64(account.amount) / $total_supply) * 100
})

// Analyze concentration patterns
$concentration_analysis = {
  top_10_percentage: SUM(collection: SLICE($top_holders, 0, 10), field: "percentage"),
  top_1_percentage: $top_holders[0].percentage,
  gini_coefficient: CALCULATE_GINI($top_holders),
  herfindahl_index: SUM(collection: MAP($top_holders, h => POW(h.percentage / 100, 2)))
}

**Decision Point:** Interpret supply distribution narrative
  BRANCH A ($concentration_analysis.top_1_percentage > 50):
    $distribution_story = "whale_dominance_single_entity_controls_majority"
    $market_implications = "high_volatility_potential_manipulation_risks"
    $confidence = 90
  BRANCH B ($concentration_analysis.top_10_percentage > 80):
    $distribution_story = "oligarchic_distribution_few_entities_dominate"
    $market_implications = "stable_but_concentrated_power_structure"
    $confidence = 88
  BRANCH C ($concentration_analysis.gini_coefficient < 0.3):
    $distribution_story = "democratic_distribution_widespread_ownership"
    $market_implications = "resilient_but_potentially_illiquid"
    $confidence = 85

**Action:**
RETURN {
  token_mint: $target_token,
  supply_narrative: $distribution_story,
  market_implications: $market_implications,
  concentration_metrics: {
    top_holder_percentage: $concentration_analysis.top_1_percentage,
    top_10_percentage: $concentration_analysis.top_10_percentage,
    gini_coefficient: $concentration_analysis.gini_coefficient,
    herfindahl_index: $concentration_analysis.herfindahl_index
  },
  holder_insights: {
    total_top_holders_analyzed: COUNT($top_holders),
    average_top_holder_percentage: MEAN(collection: $top_holders, field: "percentage"),
    concentration_trend: $concentration_analysis.top_10_percentage > 70 ? "highly_concentrated" : "moderately_distributed"
  },
  confidence: $confidence,
  note: "Token supply distribution reveals power dynamics, market structure, and potential behavioral patterns"
}

---

## Q2: "How does the token's heartbeat - its transfer frequency - reveal community engagement patterns?"

**Expected Plan:**

[TIME: ~15s] [COST: free] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, COUNT, GROUP_BY, SORT_BY (Data Processing)
  - MEAN, STDDEV, CORRELATE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$analysis_window = 1000 // transactions to analyze

TRY:
  $recent_signatures = getSignaturesForAddress(address: $target_token, limit: $analysis_window)
CATCH FATAL:
  RETURN ERROR(message: "Transaction history unavailable")

GUARD COUNT($recent_signatures) > 0 ELSE
  RETURN ERROR(message: "No recent transactions found")

// Analyze transfer patterns over time
$transfer_patterns = {
  transaction_count: COUNT($recent_signatures),
  time_span: $recent_signatures[0].blockTime - $recent_signatures[COUNT($recent_signatures) - 1].blockTime,
  average_interval: ($recent_signatures[0].blockTime - $recent_signatures[COUNT($recent_signatures) - 1].blockTime) / COUNT($recent_signatures),
  hourly_distribution: GROUP_BY(collection: $recent_signatures, key: tx => tx.blockTime % 86400 / 3600),
  daily_volume_trend: GROUP_BY(collection: $recent_signatures, key: tx => tx.blockTime / 86400)
}

// Calculate engagement metrics
$engagement_metrics = {
  transactions_per_hour: COUNT($recent_signatures) / ($time_span / 3600),
  transactions_per_day: COUNT($recent_signatures) / ($time_span / 86400),
  activity_consistency: STDDEV(collection: MAP($daily_volume_trend, day => day.count)) / MEAN(collection: MAP($daily_volume_trend, day => day.count)),
  peak_activity_hour: MAX_KEY($hourly_distribution, field: "count")
}

**Decision Point:** Interpret community engagement patterns
  BRANCH A ($engagement_metrics.transactions_per_hour > 50 AND $engagement_metrics.activity_consistency < 0.3):
    $engagement_pattern = "highly_active_consistent_community"
    $community_health = "vibrant_engaged_ecosystem"
    $confidence = 87
  BRANCH B ($engagement_metrics.transactions_per_hour < 10 AND $engagement_metrics.activity_consistency > 0.7):
    $engagement_pattern = "low_activity_sporadic_usage"
    $community_health = "dormant_or_speculative_token"
    $confidence = 85
  BRANCH C ($engagement_metrics.transactions_per_hour > 20 AND $engagement_metrics.activity_consistency > 0.6):
    $engagement_pattern = "bursty_activity_pattern"
    $community_health = "event_driven_or_whale_influenced"
    $confidence = 82

**Action:**
RETURN {
  token_mint: $target_token,
  engagement_pattern: $engagement_pattern,
  community_health: $community_health,
  activity_metrics: {
    total_transactions_analyzed: $transfer_patterns.transaction_count,
    average_transactions_per_hour: $engagement_metrics.transactions_per_hour,
    average_transactions_per_day: $engagement_metrics.transactions_per_day,
    activity_consistency_score: 1 - $engagement_metrics.activity_consistency,
    peak_activity_hour: $engagement_metrics.peak_activity_hour
  },
  behavioral_insights: {
    time_span_hours: $transfer_patterns.time_span / 3600,
    activity_rhythm: $engagement_metrics.activity_consistency < 0.4 ? "steady_flow" : "erratic_pulses",
    community_maturity: $engagement_metrics.transactions_per_day > 100 ? "mature_active" : "emerging_or_niche"
  },
  confidence: $confidence,
  note: "Transfer frequency patterns reveal community vitality, behavioral rhythms, and token lifecycle stage"
}

---

## Q3: "What does the token's metadata whisper about its creator's intentions and token design philosophy?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - JSON_PARSE, borshDeserialize (Solana Utilities)
  - MAP, FILTER, COUNT (Data Processing)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

TRY:
  $account_info = getAccountInfo(account: $target_token)
CATCH FATAL:
  RETURN ERROR(message: "Token account not found")

GUARD $account_info != null ELSE
  RETURN ERROR(message: "Account data unavailable")

// Analyze token metadata structure
$metadata_analysis = {
  account_size: $account_info.data.length,
  owner_program: $account_info.owner,
  is_executable: $account_info.executable,
  lamports_balance: $account_info.lamports,
  rent_epoch: $account_info.rentEpoch
}

// Parse token mint data (simplified for basic analysis)
$token_data = borshDeserialize(data: $account_info.data, schema: "spl_token_mint")

$design_philosophy = {
  mint_authority: $token_data.mintAuthority,
  freeze_authority: $token_data.freezeAuthority,
  supply_cap_design: $token_data.supply == 0 ? "uncapped_supply" : "capped_supply",
  authority_structure: ($token_data.mintAuthority == $token_data.freezeAuthority) ? "centralized_control" : "dual_authority",
  decimal_precision: $token_data.decimals
}

**Decision Point:** Interpret token design intentions
  BRANCH A ($design_philosophy.authority_structure == "centralized_control" AND $design_philosophy.supply_cap_design == "uncapped_supply"):
    $creator_intentions = "maximum_flexibility_creator_control"
    $design_philosophy = "trust_based_decentralized_governance"
    $confidence = 89
  BRANCH B ($design_philosophy.authority_structure == "dual_authority" AND $design_philosophy.supply_cap_design == "capped_supply"):
    $creator_intentions = "controlled_supply_dual_governance"
    $design_philosophy = "balanced_centralization_decentralization"
    $confidence = 87
  BRANCH C ($token_data.mintAuthority == null AND $token_data.freezeAuthority == null):
    $creator_intentions = "full_decentralization_renounced_control"
    $design_philosophy = "pure_decentralized_autonomous_token"
    $confidence = 92

**Action:**
RETURN {
  token_mint: $target_token,
  creator_intentions: $creator_intentions,
  design_philosophy: $design_philosophy,
  metadata_insights: {
    authority_structure: $design_philosophy.authority_structure,
    supply_design: $design_philosophy.supply_cap_design,
    decimal_precision: $design_philosophy.decimal_precision,
    control_mechanism: $token_data.mintAuthority != null ? "active_control" : "renounced_control"
  },
  structural_analysis: {
    account_size_bytes: $metadata_analysis.account_size,
    owner_program: $metadata_analysis.owner_program,
    current_balance_lamports: $metadata_analysis.lamports_balance,
    governance_model: $design_philosophy.authority_structure == "centralized_control" ? "centralized" : "decentralized"
  },
  confidence: $confidence,
  note: "Token metadata reveals creator philosophy, governance intentions, and fundamental design choices"
}

---

## Q4: "How do token holder clusters form behavioral tribes with distinct trading personalities?"

**Expected Plan:**

[TIME: ~18s] [COST: free] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getProgramAccounts (Solana RPC)
  - MAP, FILTER, COUNT, GROUP_BY, CLUSTER (Data Processing)
  - CORRELATE, DETECT_PATTERNS (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$sample_size = 100 // holders to analyze

TRY:
  $token_accounts = getTokenAccountsByOwner(owner: "*", mint: $target_token, limit: $sample_size)
CATCH FATAL:
  RETURN ERROR(message: "Token holder data unavailable")

GUARD COUNT($token_accounts) > 10 ELSE
  RETURN ERROR(message: "Insufficient holder data for clustering")

// Analyze holder balance distributions
$holder_analysis = {
  balances: MAP(collection: $token_accounts, fn: account => parseU64(account.account.data.parsed.info.tokenAmount.amount)),
  balance_percentiles: {
    p25: PERCENTILE(data: $balances, percentile: 25),
    p50: PERCENTILE(data: $balances, percentile: 50),
    p75: PERCENTILE(data: $balances, percentile: 75),
    p95: PERCENTILE(data: $balances, percentile: 95)
  }
}

// Cluster holders by balance patterns
$holder_clusters = CLUSTER(
  data: $token_accounts,
  features: ["balance", "account_age", "transaction_frequency"],
  method: "k_means",
  clusters: 4
)

$cluster_personalities = {
  whale_cluster: FILTER(collection: $holder_clusters, condition: cluster.id == 0),
  institutional_cluster: FILTER(collection: $holder_clusters, condition: cluster.id == 1),
  retail_cluster: FILTER(collection: $holder_clusters, condition: cluster.id == 2),
  dust_cluster: FILTER(collection: $holder_clusters, condition: cluster.id == 3)
}

**Decision Point:** Characterize holder tribe personalities
  BRANCH A (COUNT($cluster_personalities.whale_cluster) > 0 AND MEAN(collection: $cluster_personalities.whale_cluster, field: "balance") > $holder_analysis.balance_percentiles.p95):
    $dominant_tribe = "whale_dominance_price_influencers"
    $market_behavior = "volatility_driven_large_holder_actions"
    $confidence = 84
  BRANCH B (COUNT($cluster_personalities.institutional_cluster) > COUNT($cluster_personalities.retail_cluster) AND STDDEV(collection: $cluster_personalities.institutional_cluster, field: "balance") < 0.3):
    $dominant_tribe = "institutional_stability_long_term_holders"
    $market_behavior = "steady_accumulation_strategic_positions"
    $confidence = 86
  BRANCH C (COUNT($cluster_personalities.dust_cluster) > COUNT($token_accounts) * 0.6):
    $dominant_tribe = "retail_fragmentation_airdrop_or_giveaway_tokens"
    $market_behavior = "speculative_trading_high_churn_potential"
    $confidence = 82

**Action:**
RETURN {
  token_mint: $target_token,
  dominant_tribe: $dominant_tribe,
  market_behavior: $market_behavior,
  cluster_analysis: {
    total_holders_analyzed: COUNT($token_accounts),
    cluster_distribution: {
      whales: COUNT($cluster_personalities.whale_cluster),
      institutional: COUNT($cluster_personalities.institutional_cluster),
      retail: COUNT($cluster_personalities.retail_cluster),
      dust: COUNT($cluster_personalities.dust_cluster)
    },
    balance_distribution: $holder_analysis.balance_percentiles
  },
  behavioral_insights: {
    concentration_index: COUNT($cluster_personalities.whale_cluster) / COUNT($token_accounts),
    stability_score: 1 - (STDDEV($balances) / MEAN($balances)),
    tribe_diversity: COUNT(collection: FILTER($holder_clusters, c => COUNT(c.members) > 5)),
    market_maturity: COUNT($cluster_personalities.institutional_cluster) > COUNT($cluster_personalities.dust_cluster) ? "mature" : "speculative"
  },
  confidence: $confidence,
  note: "Holder clustering reveals behavioral tribes, market psychology, and ecosystem maturity patterns"
}

---

## Q5: "What seasonal rhythms does the token exhibit in its transfer patterns throughout the day?"

**Expected Plan:**

[TIME: ~16s] [COST: free] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getBlock (Solana RPC)
  - MAP, FILTER, GROUP_BY, SORT_BY (Data Processing)
  - CORRELATE, DETECT_PATTERNS, MEAN (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$analysis_days = 7 // analyze last week

TRY:
  $recent_signatures = getSignaturesForAddress(address: $target_token, limit: 1000)
CATCH FATAL:
  RETURN ERROR(message: "Transaction history unavailable")

GUARD COUNT($recent_signatures) > 50 ELSE
  RETURN ERROR(message: "Insufficient transaction data")

// Analyze temporal patterns
$temporal_patterns = {
  hourly_distribution: GROUP_BY(collection: $recent_signatures, key: tx => (tx.blockTime % 86400) / 3600),
  daily_distribution: GROUP_BY(collection: $recent_signatures, key: tx => tx.blockTime / 86400),
  weekday_patterns: GROUP_BY(collection: $recent_signatures, key: tx => (tx.blockTime / 86400 + 4) % 7) // Monday = 0
}

// Calculate rhythm metrics
$rhythm_metrics = {
  peak_hour: MAX_KEY($hourly_distribution, field: "count"),
  peak_hour_volume: MAX(collection: MAP($hourly_distribution, h => h.count)),
  activity_consistency: 1 - (STDDEV(collection: MAP($hourly_distribution, h => h.count)) / MEAN(collection: MAP($hourly_distribution, h => h.count))),
  weekend_vs_weekday_ratio: MEAN(collection: FILTER($weekday_patterns, w => w.key >= 5), field: "count") / MEAN(collection: FILTER($weekday_patterns, w => w.key < 5), field: "count")
}

**Decision Point:** Interpret token's daily and weekly rhythms
  BRANCH A ($rhythm_metrics.peak_hour >= 9 AND $rhythm_metrics.peak_hour <= 17 AND $rhythm_metrics.weekend_vs_weekday_ratio < 0.7):
    $activity_rhythm = "business_hours_trading_pattern"
    $market_characteristics = "professional_institutional_dominance"
    $confidence = 86
  BRANCH B ($rhythm_metrics.peak_hour >= 0 AND $rhythm_metrics.peak_hour <= 6 AND $rhythm_metrics.activity_consistency > 0.8):
    $activity_rhythm = "consistent_global_distribution"
    $market_characteristics = "international_24_7_trading"
    $confidence = 88
  BRANCH C ($rhythm_metrics.weekend_vs_weekday_ratio > 1.3 AND $rhythm_metrics.activity_consistency < 0.5):
    $activity_rhythm = "weekend_speculative_spikes"
    $market_characteristics = "retail_gamification_driven"
    $confidence = 84

**Action:**
RETURN {
  token_mint: $target_token,
  activity_rhythm: $activity_rhythm,
  market_characteristics: $market_characteristics,
  temporal_analysis: {
    peak_activity_hour: $rhythm_metrics.peak_hour,
    peak_hour_transaction_count: $rhythm_metrics.peak_hour_volume,
    activity_consistency_score: $rhythm_metrics.activity_consistency,
    weekend_weekday_ratio: $rhythm_metrics.weekend_vs_weekday_ratio
  },
  pattern_insights: {
    trading_timezone: $rhythm_metrics.peak_hour >= 9 && $rhythm_metrics.peak_hour <= 17 ? "business_hours" : "off_hours",
    market_maturity: $rhythm_metrics.activity_consistency > 0.7 ? "mature_liquid" : "volatile_speculative",
    participant_geography: $rhythm_metrics.weekend_vs_weekday_ratio < 0.8 ? "professional_focused" : "retail_oriented"
  },
  confidence: $confidence,
  note: "Temporal transfer patterns reveal market timezone, participant demographics, and trading culture"
}

---

## Q6: "How does the token's transfer velocity indicate market sentiment and liquidity health?"

**Expected Plan:**

[TIME: ~14s] [COST: free] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, COUNT, SORT_BY (Data Processing)
  - MEAN, STDDEV, CORRELATE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$velocity_window = 24 // hours to analyze

TRY:
  $recent_txs = getSignaturesForAddress(address: $target_token, limit: 500)
CATCH FATAL:
  RETURN ERROR(message: "Transaction data unavailable")

GUARD COUNT($recent_txs) > 20 ELSE
  RETURN ERROR(message: "Insufficient transaction volume")

// Calculate transfer velocity metrics
$velocity_metrics = {
  total_transactions: COUNT($recent_txs),
  time_window_hours: ($recent_txs[0].blockTime - $recent_txs[COUNT($recent_txs) - 1].blockTime) / 3600,
  transactions_per_hour: COUNT($recent_txs) / (($recent_txs[0].blockTime - $recent_txs[COUNT($recent_txs) - 1].blockTime) / 3600),
  transaction_frequency: 3600 / (COUNT($recent_txs) / (($recent_txs[0].blockTime - $recent_txs[COUNT($recent_txs) - 1].blockTime) / 3600))
}

// Analyze transaction sizes and patterns
$transaction_analysis = {
  average_interval_seconds: $velocity_metrics.transaction_frequency,
  velocity_trend: $velocity_metrics.transactions_per_hour > 10 ? "high_velocity" : "moderate_velocity",
  market_liquidity: $velocity_metrics.transactions_per_hour > 20 ? "highly_liquid" : "moderately_liquid"
}

**Decision Point:** Assess market sentiment through transfer velocity
  BRANCH A ($velocity_metrics.transactions_per_hour > 50 AND $velocity_metrics.transaction_frequency < 30):
    $market_sentiment = "frenetic_trading_high_excitement"
    $liquidity_health = "excellent_very_active_market"
    $confidence = 85
  BRANCH B ($velocity_metrics.transactions_per_hour < 5 AND $velocity_metrics.transaction_frequency > 300):
    $market_sentiment = "stagnant_trading_low_interest"
    $liquidity_health = "poor_illiquid_market"
    $confidence = 87
  BRANCH C ($velocity_metrics.transactions_per_hour > 15 AND $velocity_metrics.transactions_per_hour < 40):
    $market_sentiment = "steady_trading_healthy_activity"
    $liquidity_health = "good_balanced_liquidity"
    $confidence = 86

**Action:**
RETURN {
  token_mint: $target_token,
  market_sentiment: $market_sentiment,
  liquidity_health: $liquidity_health,
  velocity_metrics: {
    transactions_per_hour: $velocity_metrics.transactions_per_hour,
    average_transaction_interval_seconds: $velocity_metrics.transaction_frequency,
    total_transactions_analyzed: $velocity_metrics.total_transactions,
    analysis_window_hours: $velocity_metrics.time_window_hours
  },
  market_insights: {
    trading_intensity: $velocity_metrics.transactions_per_hour > 30 ? "high_intensity" : "moderate_intensity",
    market_maturity: $velocity_metrics.transaction_frequency < 60 ? "mature_liquid" : "emerging_volatile",
    investor_engagement: $velocity_metrics.transactions_per_hour > 10 ? "actively_traded" : "passively_held"
  },
  confidence: $confidence,
  note: "Transfer velocity reveals market sentiment, liquidity health, and community engagement levels"
}

---

## Q7: "What does the token's holder age distribution reveal about its adoption timeline and growth phases?"

**Expected Plan:**

[TIME: ~20s] [COST: free] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getAccountInfo (Solana RPC)
  - MAP, FILTER, SORT_BY, GROUP_BY (Data Processing)
  - MEAN, PERCENTILE, CORRELATE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$current_time = NOW()

TRY:
  $token_accounts = getTokenAccountsByOwner(owner: "*", mint: $target_token, limit: 200)
CATCH FATAL:
  RETURN ERROR(message: "Token holder data unavailable")

GUARD COUNT($token_accounts) > 20 ELSE
  RETURN ERROR(message: "Insufficient holder data")

// Analyze account ages and holding patterns
$age_analysis = {
  account_ages: MAP(collection: $token_accounts, fn: account => {
    account_info: getAccountInfo(account: account.account.owner),
    age_days: ($current_time - account_info.created_at) / 86400
  }),
  age_distribution: GROUP_BY(collection: $age_analysis.account_ages, key: account => FLOOR(account.age_days / 30)) // monthly cohorts
}

$cohort_analysis = {
  oldest_holders: SORT_BY(collection: $age_analysis.account_ages, field: "age_days", descending: true),
  newest_holders: SORT_BY(collection: $age_analysis.account_ages, field: "age_days", descending: false),
  average_holder_age: MEAN(collection: $age_analysis.account_ages, field: "age_days"),
  holder_retention_score: COUNT(collection: FILTER($age_analysis.account_ages, a => a.age_days > 90)) / COUNT($age_analysis.account_ages)
}

**Decision Point:** Interpret adoption timeline and growth phases
  BRANCH A ($cohort_analysis.average_holder_age > 180 AND $cohort_analysis.holder_retention_score > 0.7):
    $adoption_timeline = "mature_established_token_long_term_holders"
    $growth_phase = "stability_phase_high_retention"
    $confidence = 81
  BRANCH B ($cohort_analysis.average_holder_age < 30 AND COUNT($cohort_analysis.newest_holders) > COUNT($age_analysis.account_ages) * 0.6):
    $adoption_timeline = "recent_launch_rapid_adoption_phase"
    $growth_phase = "growth_acceleration_new_investors"
    $confidence = 84
  BRANCH C ($cohort_analysis.holder_retention_score < 0.4 AND STDDEV(collection: $age_analysis.account_ages, field: "age_days") > 100):
    $adoption_timeline = "volatile_adoption_high_churn_environment"
    $growth_phase = "experimental_phase_finding_product_market_fit"
    $confidence = 79

**Action:**
RETURN {
  token_mint: $target_token,
  adoption_timeline: $adoption_timeline,
  growth_phase: $growth_phase,
  cohort_analysis: {
    total_holders_analyzed: COUNT($age_analysis.account_ages),
    average_holder_age_days: $cohort_analysis.average_holder_age,
    holder_retention_score: $cohort_analysis.holder_retention_score,
    oldest_holder_age_days: $cohort_analysis.oldest_holders[0].age_days,
    newest_holder_age_days: $cohort_analysis.newest_holders[0].age_days
  },
  adoption_insights: {
    adoption_maturity: $cohort_analysis.average_holder_age > 90 ? "mature_adoption" : "early_adoption",
    holder_loyalty: $cohort_analysis.holder_retention_score > 0.6 ? "high_loyalty" : "low_loyalty",
    growth_trajectory: COUNT(collection: FILTER($age_analysis.account_ages, a => a.age_days < 7)) > COUNT($age_analysis.account_ages) * 0.3 ? "accelerating" : "stabilizing"
  },
  confidence: $confidence,
  note: "Holder age distribution reveals adoption timeline, growth phases, and community evolution patterns"
}

---

## Q8: "How do token transfer amounts correlate with market volatility and whale movements?"

**Expected Plan:**

[TIME: ~22s] [COST: free] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, SORT_BY, GROUP_BY (Data Processing)
  - CORRELATE, STDDEV, PERCENTILE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$analysis_depth = 200 // transactions to analyze

TRY:
  $transaction_signatures = getSignaturesForAddress(address: $target_token, limit: $analysis_depth)
  $transaction_details = MAP(collection: $transaction_signatures, fn: sig => getTransaction(signature: sig.signature))
CATCH FATAL:
  RETURN ERROR(message: "Transaction data unavailable")

GUARD COUNT($transaction_details) > 20 ELSE
  RETURN ERROR(message: "Insufficient transaction data")

// Analyze transfer amounts and patterns
$transfer_analysis = {
  transfer_amounts: MAP(collection: $transaction_details, fn: tx => {
    token_transfers: FILTER(tx.transaction.message.instructions, i => i.programId == "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"),
    amount: SUM(collection: token_transfers, field: "amount")
  }),
  amount_distribution: {
    small_transfers: COUNT(collection: FILTER($transfer_amounts, t => t.amount < 1000)),
    medium_transfers: COUNT(collection: FILTER($transfer_amounts, t => t.amount >= 1000 && t.amount < 10000)),
    large_transfers: COUNT(collection: FILTER($transfer_amounts, t => t.amount >= 10000 && t.amount < 100000)),
    whale_transfers: COUNT(collection: FILTER($transfer_amounts, t => t.amount >= 100000))
  }
}

$correlation_analysis = {
  amount_volatility: STDDEV($transfer_amounts),
  transfer_frequency: COUNT($transfer_amounts) / (($transaction_signatures[0].blockTime - $transaction_signatures[COUNT($transaction_signatures) - 1].blockTime) / 3600),
  whale_activity_ratio: $transfer_analysis.amount_distribution.whale_transfers / COUNT($transfer_amounts)
}

**Decision Point:** Analyze correlation between transfer amounts and market dynamics
  BRANCH A ($correlation_analysis.whale_activity_ratio > 0.3 AND $correlation_analysis.amount_volatility > PERCENTILE($transfer_amounts, 75)):
    $market_dynamics = "whale_dominated_high_volatility_market"
    $transfer_characteristics = "large_movements_drive_price_action"
    $confidence = 82
  BRANCH B ($transfer_analysis.amount_distribution.small_transfers > COUNT($transfer_amounts) * 0.7 AND $correlation_analysis.amount_volatility < PERCENTILE($transfer_amounts, 25)):
    $market_dynamics = "retail_focused_stable_micro_transactions"
    $transfer_characteristics = "steady_accumulation_distribution"
    $confidence = 84
  BRANCH C ($correlation_analysis.transfer_frequency > 20 AND $correlation_analysis.whale_activity_ratio < 0.1):
    $market_dynamics = "high_frequency_trading_environment"
    $transfer_characteristics = "algorithmic_trading_dominance"
    $confidence = 81

**Action:**
RETURN {
  token_mint: $target_token,
  market_dynamics: $market_dynamics,
  transfer_characteristics: $transfer_characteristics,
  amount_analysis: {
    total_transfers_analyzed: COUNT($transfer_amounts),
    transfer_distribution: $transfer_analysis.amount_distribution,
    amount_volatility: $correlation_analysis.amount_volatility,
    whale_activity_ratio: $correlation_analysis.whale_activity_ratio
  },
  correlation_insights: {
    market_structure: $correlation_analysis.whale_activity_ratio > 0.2 ? "whale_influenced" : "democratic_distribution",
    trading_style: $correlation_analysis.transfer_frequency > 15 ? "high_frequency" : "position_trading",
    volatility_profile: $correlation_analysis.amount_volatility > MEAN($transfer_amounts) * 2 ? "high_volatility" : "stable_flow"
  },
  confidence: $confidence,
  note: "Transfer amount patterns reveal market structure, whale influence, and trading behavior characteristics"
}

---

## Q9: "What does the token's utility pattern reveal about its real-world adoption and ecosystem integration?"

**Expected Plan:**

[TIME: ~17s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getSignaturesForAddress (Solana RPC)
  - MAP, FILTER, COUNT, GROUP_BY, UNIQUE (Data Processing)
  - CORRELATE, DETECT_PATTERNS (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

TRY:
  $program_accounts = getProgramAccounts(programId: "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", filters: [
    { memcmp: { offset: 0, bytes: $target_token } }
  ])
  $recent_activity = getSignaturesForAddress(address: $target_token, limit: 300)
CATCH FATAL:
  RETURN ERROR(message: "Token ecosystem data unavailable")

GUARD COUNT($program_accounts) > 0 ELSE
  RETURN ERROR(message: "No token accounts found")

// Analyze token utility patterns
$utility_analysis = {
  total_accounts: COUNT($program_accounts),
  active_accounts: COUNT(collection: FILTER($recent_activity, tx => tx.blockTime > (NOW() - 86400 * 7))), // last 7 days
  account_types: GROUP_BY(collection: $program_accounts, key: account => {
    is_ata: account.account.data.parsed.type == "account",
    has_balance: parseU64(account.account.data.parsed.info.tokenAmount.amount) > 0,
    is_frozen: account.account.data.parsed.info.isFrozen
  }),
  interaction_patterns: {
    daily_active_users: UNIQUE(collection: MAP($recent_activity, tx => tx.transaction.message.accountKeys[0])),
    transaction_velocity: COUNT($recent_activity) / 7, // transactions per day
    utility_breadth: COUNT(collection: UNIQUE(MAP($recent_activity, tx => tx.transaction.message.instructions[0].programId)))
  }
}

$ecosystem_integration = {
  adoption_depth: $utility_analysis.active_accounts / $utility_analysis.total_accounts,
  utility_diversity: COUNT($utility_analysis.interaction_patterns.utility_breadth),
  user_engagement: COUNT($utility_analysis.interaction_patterns.daily_active_users) / 7 // daily active users
}

**Decision Point:** Evaluate real-world utility and ecosystem adoption
  BRANCH A ($ecosystem_integration.adoption_depth > 0.6 AND $ecosystem_integration.utility_diversity > 5 AND $utility_analysis.interaction_patterns.transaction_velocity > 100):
    $utility_pattern = "deeply_integrated_multi_utility_token"
    $adoption_level = "mature_ecosystem_widespread_usage"
    $confidence = 83
  BRANCH B ($ecosystem_integration.adoption_depth < 0.3 AND $ecosystem_integration.utility_diversity < 3 AND $utility_analysis.interaction_patterns.transaction_velocity < 20):
    $utility_pattern = "limited_utility_speculative_token"
    $adoption_level = "niche_or_experimental_usage"
    $confidence = 85
  BRANCH C ($ecosystem_integration.adoption_depth > 0.4 AND $ecosystem_integration.utility_diversity > 3 AND $utility_analysis.interaction_patterns.transaction_velocity > 50):
    $utility_pattern = "growing_utility_developing_ecosystem"
    $adoption_level = "emerging_adoption_product_market_fit"
    $confidence = 84

**Action:**
RETURN {
  token_mint: $target_token,
  utility_pattern: $utility_pattern,
  adoption_level: $adoption_level,
  ecosystem_metrics: {
    total_token_accounts: $utility_analysis.total_accounts,
    active_accounts_7d: $utility_analysis.active_accounts,
    adoption_depth_percentage: $ecosystem_integration.adoption_depth * 100,
    utility_diversity_score: $ecosystem_integration.utility_diversity,
    daily_active_users: $ecosystem_integration.user_engagement
  },
  integration_insights: {
    ecosystem_maturity: $ecosystem_integration.adoption_depth > 0.5 ? "mature_integration" : "early_integration",
    utility_breadth: $ecosystem_integration.utility_diversity > 4 ? "versatile_token" : "specialized_token",
    user_adoption: $ecosystem_integration.user_engagement > 50 ? "strong_adoption" : "limited_adoption"
  },
  confidence: $confidence,
  note: "Token utility patterns reveal real-world adoption, ecosystem integration, and practical value creation"
}

---

## Q10: "How does the token's transfer network reveal hidden influencer relationships and community clusters?"

**Expected Plan:**

[TIME: ~25s] [COST: free] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, GROUP_BY, CLUSTER (Data Processing)
  - CORRELATE, DETECT_PATTERNS, NETWORK_ANALYZE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$network_depth = 150 // transactions to build network

TRY:
  $transaction_history = getSignaturesForAddress(address: $target_token, limit: $network_depth)
  $detailed_transactions = MAP(collection: $transaction_history, fn: sig => getTransaction(signature: sig.signature))
CATCH FATAL:
  RETURN ERROR(message: "Transaction network data unavailable")

GUARD COUNT($detailed_transactions) > 30 ELSE
  RETURN ERROR(message: "Insufficient network data")

// Build transfer network graph
$network_graph = {
  nodes: UNIQUE(collection: FLATTEN(MAP($detailed_transactions, tx => tx.transaction.message.accountKeys))),
  edges: MAP(collection: $detailed_transactions, fn: tx => {
    from: tx.transaction.message.accountKeys[0],
    to: tx.transaction.message.accountKeys[1],
    amount: SUM(collection: FILTER(tx.transaction.message.instructions, i => i.programId == "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"), field: "amount"),
    timestamp: tx.blockTime
  })
}

// Analyze network structure
$network_analysis = {
  centrality_scores: CALCULATE_CENTRALITY($network_graph),
  cluster_coefficient: CALCULATE_CLUSTERING_COEFFICIENT($network_graph),
  community_detection: DETECT_COMMUNITIES($network_graph, method: "louvain"),
  influencer_nodes: FILTER($network_analysis.centrality_scores, score => score > PERCENTILE($network_analysis.centrality_scores, 90))
}

$relationship_insights = {
  network_density: COUNT($network_graph.edges) / (COUNT($network_graph.nodes) * (COUNT($network_graph.nodes) - 1) / 2),
  community_count: COUNT($network_analysis.community_detection),
  influencer_concentration: COUNT($network_analysis.influencer_nodes) / COUNT($network_graph.nodes)
}

**Decision Point:** Interpret network structure and relationship patterns
  BRANCH A ($relationship_insights.network_density > 0.1 AND COUNT($network_analysis.community_detection) > 5 AND $network_analysis.cluster_coefficient > 0.6):
    $network_structure = "tightly_connected_community_clusters"
    $relationship_dynamics = "strong_social_bonds_influential_communities"
    $confidence = 79
  BRANCH B ($relationship_insights.network_density < 0.05 AND COUNT($network_analysis.influencer_nodes) > COUNT($network_graph.nodes) * 0.3):
    $network_structure = "star_topology_influencer_driven"
    $relationship_dynamics = "centralized_influence_few_key_actors"
    $confidence = 81
  BRANCH C ($relationship_insights.community_count < 3 AND $network_analysis.cluster_coefficient < 0.3):
    $network_structure = "sparse_network_loose_connections"
    $relationship_dynamics = "independent_actors_weak_community_ties"
    $confidence = 80

**Action:**
RETURN {
  token_mint: $target_token,
  network_structure: $network_structure,
  relationship_dynamics: $relationship_dynamics,
  network_metrics: {
    total_nodes: COUNT($network_graph.nodes),
    total_edges: COUNT($network_graph.edges),
    network_density: $relationship_insights.network_density,
    community_count: $relationship_insights.community_count,
    clustering_coefficient: $network_analysis.cluster_coefficient
  },
  influence_analysis: {
    influencer_count: COUNT($network_analysis.influencer_nodes),
    influencer_concentration: $relationship_insights.influencer_concentration,
    centrality_distribution: PERCENTILE($network_analysis.centrality_scores, percentiles: [25, 50, 75, 95])
  },
  confidence: $confidence,
  note: "Transfer network analysis reveals community structure, influence patterns, and social dynamics"
}

---

## Q11: "What does the token's transaction fee pattern reveal about user sophistication and market efficiency?"

**Expected Plan:**

[TIME: ~15s] [COST: free] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, COUNT, GROUP_BY (Data Processing)
  - MEAN, PERCENTILE, CORRELATE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$fee_analysis_window = 100 // transactions to analyze

TRY:
  $transaction_signatures = getSignaturesForAddress(address: $target_token, limit: $fee_analysis_window)
  $transaction_details = MAP(collection: $transaction_signatures, fn: sig => getTransaction(signature: sig.signature))
CATCH FATAL:
  RETURN ERROR(message: "Transaction fee data unavailable")

GUARD COUNT($transaction_details) > 20 ELSE
  RETURN ERROR(message: "Insufficient fee data")

// Analyze fee patterns and user behavior
$fee_analysis = {
  fees_paid: MAP(collection: $transaction_details, fn: tx => tx.meta.fee),
  fee_distribution: {
    min_fee: MIN($fees_paid),
    max_fee: MAX($fees_paid),
    median_fee: PERCENTILE(data: $fees_paid, percentile: 50),
    p95_fee: PERCENTILE(data: $fees_paid, percentile: 95)
  },
  fee_efficiency: MEAN($fees_paid) / MEDIAN($fees_paid), // efficiency ratio
  fee_consistency: 1 - (STDDEV($fees_paid) / MEAN($fees_paid)) // consistency score
}

$user_sophistication = {
  fee_optimization_score: $fee_analysis.fee_efficiency < 1.2 ? "highly_optimized" : "standard_pricing",
  fee_variability: $fee_analysis.fee_consistency > 0.8 ? "consistent_pricing" : "variable_pricing",
  market_maturity: $fee_analysis.fee_distribution.median_fee < 5000 ? "efficient_market" : "premium_market"
}

**Decision Point:** Interpret fee patterns and user sophistication
  BRANCH A ($fee_analysis.fee_efficiency < 1.1 AND $fee_analysis.fee_consistency > 0.85 AND $fee_analysis.fee_distribution.median_fee < 3000):
    $user_profile = "sophisticated_traders_efficient_pricing"
    $market_efficiency = "highly_optimized_low_fee_environment"
    $confidence = 85
  BRANCH B ($fee_analysis.fee_efficiency > 1.5 AND $fee_analysis.fee_consistency < 0.6 AND $fee_analysis.fee_distribution.p95_fee > 10000):
    $user_profile = "casual_users_premium_pricing_tolerance"
    $market_efficiency = "inefficient_high_fee_environment"
    $confidence = 86
  BRANCH C ($fee_analysis.fee_efficiency > 1.2 AND $fee_analysis.fee_efficiency < 1.4 AND $fee_analysis.fee_consistency > 0.7):
    $user_profile = "balanced_users_standard_pricing_behavior"
    $market_efficiency = "moderately_efficient_market"
    $confidence = 87

**Action:**
RETURN {
  token_mint: $target_token,
  user_profile: $user_profile,
  market_efficiency: $market_efficiency,
  fee_analysis: {
    total_transactions_analyzed: COUNT($transaction_details),
    fee_distribution: $fee_analysis.fee_distribution,
    fee_efficiency_ratio: $fee_analysis.fee_efficiency,
    fee_consistency_score: $fee_analysis.fee_consistency
  },
  sophistication_insights: {
    pricing_strategy: $user_sophistication.fee_optimization_score,
    fee_predictability: $user_sophistication.fee_variability,
    market_segment: $fee_analysis.fee_distribution.median_fee < 5000 ? "cost_conscious" : "premium_segment"
  },
  confidence: $confidence,
  note: "Transaction fee patterns reveal user sophistication, market efficiency, and pricing behavior"
}

---

## Q12: "How does the token's holder concentration evolve over time, revealing accumulation vs distribution phases?"

**Expected Plan:**

[TIME: ~20s] [COST: free] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - getTokenLargestAccounts, getBlock (Solana RPC)
  - MAP, FILTER, SORT_BY, GROUP_BY (Data Processing)
  - CORRELATE, TREND_ANALYZE, MEAN (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$historical_depth = 10 // blocks to analyze concentration evolution

TRY:
  $current_slot = getSlot()
  $historical_blocks = []
  
  FOR $i IN 0..$historical_depth:
    $block = getBlock(slot: $current_slot - ($i * 100)) // sample every 100 slots
    $historical_blocks = APPEND(array: $historical_blocks, item: $block)
  CATCH FATAL:
    RETURN ERROR(message: "Historical concentration data unavailable")

GUARD COUNT($historical_blocks) > 3 ELSE
  RETURN ERROR(message: "Insufficient historical data")

// Analyze concentration evolution
$concentration_evolution = {
  time_series: MAP(collection: $historical_blocks, fn: block => {
    slot: block.slot,
    timestamp: block.blockTime,
    top_holders: getTokenLargestAccounts(mint: $target_token, limit: 10),
    concentration: SUM(collection: MAP(top_holders, h => parseU64(h.amount))) / getTokenSupply(mint: $target_token).amount
  }),
  trend_analysis: TREND_ANALYZE(data: $concentration_evolution.time_series, field: "concentration"),
  volatility_measure: STDDEV(collection: MAP($concentration_evolution.time_series, ts => ts.concentration))
}

$phase_analysis = {
  concentration_trend: $concentration_evolution.trend_analysis.slope > 0 ? "increasing_concentration" : "decreasing_concentration",
  concentration_volatility: $concentration_evolution.volatility_measure,
  current_phase: $concentration_evolution.time_series[0].concentration > 0.7 ? "high_concentration_phase" : "distributed_phase"
}

**Decision Point:** Interpret concentration evolution and market phases
  BRANCH A ($phase_analysis.concentration_trend == "increasing_concentration" AND $phase_analysis.concentration_volatility > 0.1):
    $market_phase = "accumulation_phase_whale_buying"
    $distribution_dynamics = "concentrating_ownership_power_consolidation"
    $confidence = 82
  BRANCH B ($phase_analysis.concentration_trend == "decreasing_concentration" AND $phase_analysis.concentration_volatility < 0.05):
    $market_phase = "distribution_phase_broadening_ownership"
    $distribution_dynamics = "democratizing_token_access_widening_participation"
    $confidence = 84
  BRANCH C ($phase_analysis.concentration_volatility > 0.15 AND ABS($concentration_evolution.trend_analysis.slope) < 0.01):
    $market_phase = "volatile_phase_whale_trading_activity"
    $distribution_dynamics = "unstable_ownership_frequent_large_transfers"
    $confidence = 81

**Action:**
RETURN {
  token_mint: $target_token,
  market_phase: $market_phase,
  distribution_dynamics: $distribution_dynamics,
  concentration_evolution: {
    analysis_period_blocks: $historical_depth,
    current_concentration: $concentration_evolution.time_series[0].concentration,
    concentration_trend: $phase_analysis.concentration_trend,
    concentration_volatility: $phase_analysis.concentration_volatility,
    trend_slope: $concentration_evolution.trend_analysis.slope
  },
  phase_insights: {
    ownership_stability: $phase_analysis.concentration_volatility < 0.08 ? "stable_ownership" : "volatile_ownership",
    market_maturity: $concentration_evolution.time_series[0].concentration < 0.6 ? "mature_distribution" : "early_concentration",
    participation_trend: $phase_analysis.concentration_trend == "decreasing_concentration" ? "broadening_access" : "consolidating_power"
  },
  confidence: $confidence,
  note: "Concentration evolution reveals market phases, ownership dynamics, and participation trends"
}

---

## Q13: "What does the token's interaction with DeFi protocols reveal about its utility and ecosystem role?"

**Expected Plan:**

[TIME: ~18s] [COST: free] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getSignaturesForAddress (Solana RPC)
  - MAP, FILTER, COUNT, GROUP_BY, UNIQUE (Data Processing)
  - CORRELATE, DETECT_PATTERNS (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

TRY:
  // Analyze bridge protocol interactions
  $bridge_protocols = ["wormDTUJ6AWPNvk59vGQbDvGJmqbDTdgWgAqcLBCgUb", "WormT3McKhFJ2RkiGpdw9GKvNCrB2aB54gb2uV9MfQC"] // Wormhole bridges
  $bridge_activity = []
  
  FOR $bridge IN $bridge_protocols:
    $bridge_transactions = getSignaturesForAddress(address: $bridge, limit: 100)
    $token_bridge_txs = FILTER($bridge_transactions, tx => {
      tx_details: getTransaction(signature: tx.signature),
      involves_token: CONTAINS(tx_details.transaction.message.accountKeys, $target_token)
    })
    $bridge_activity = APPEND(array: $bridge_activity, item: {
      bridge: $bridge,
      token_transfers: COUNT($token_bridge_txs),
      volume_estimate: SUM(collection: MAP($token_bridge_txs, tx => getTransaction(signature: tx.signature).meta.fee))
    })
  CATCH FATAL:
    RETURN ERROR(message: "Bridge activity data unavailable")

// Analyze interoperability patterns
$interoperability_analysis = {
  total_bridge_transfers: SUM(collection: $bridge_activity, field: "token_transfers"),
  active_bridges: COUNT(collection: FILTER($bridge_activity, b => b.token_transfers > 0)),
  bridge_diversity: COUNT(collection: UNIQUE(MAP($bridge_activity, b => b.bridge))),
  cross_chain_volume: SUM(collection: $bridge_activity, field: "volume_estimate")
}

$global_adoption = {
  interoperability_score: $interoperability_analysis.active_bridges / COUNT($bridge_protocols),
  bridge_maturity: $interoperability_analysis.total_bridge_transfers > 50 ? "mature_cross_chain" : "emerging_interoperability",
  global_reach: $interoperability_analysis.bridge_diversity > 1 ? "multi_chain_presence" : "solana_centric"
}

**Decision Point:** Evaluate interoperability and global adoption
  BRANCH A ($global_adoption.interoperability_score > 0.7 AND $global_adoption.bridge_maturity == "mature_cross_chain" AND $interoperability_analysis.bridge_diversity > 2):
    $interoperability_profile = "highly_interoperable_global_token"
    $adoption_scope = "worldwide_adoption_cross_chain_ecosystem"
    $confidence = 83
  BRANCH B ($global_adoption.interoperability_score < 0.3 AND $global_adoption.bridge_maturity == "emerging_interoperability"):
    $interoperability_profile = "solana_native_limited_cross_chain"
    $adoption_scope = "regional_adoption_solana_ecosystem_focus"
    $confidence = 85
  BRANCH C ($global_adoption.interoperability_score > 0.4 AND $interoperability_analysis.total_bridge_transfers > 20 AND $interoperability_analysis.bridge_diversity > 1):
    $interoperability_profile = "developing_interoperability_growing_global_reach"
    $adoption_scope = "expanding_adoption_multi_chain_presence"
    $confidence = 84

**Action:**
RETURN {
  token_mint: $target_token,
  interoperability_profile: $interoperability_profile,
  adoption_scope: $adoption_scope,
  bridge_analysis: {
    total_bridge_transfers: $interoperability_analysis.total_bridge_transfers,
    active_bridges: $interoperability_analysis.active_bridges,
    bridge_diversity: $interoperability_analysis.bridge_diversity,
    estimated_cross_chain_volume: $interoperability_analysis.cross_chain_volume
  },
  global_insights: {
    interoperability_score: $global_adoption.interoperability_score,
    bridge_maturity: $global_adoption.bridge_maturity,
    geographic_reach: $global_adoption.global_reach
  },
  confidence: $confidence,
  note: "Bridge activity reveals interoperability level, global adoption scope, and cross-chain ecosystem integration"
}

---

## Q14: "How do token holder transaction patterns reveal behavioral psychology and market sentiment?"

**Expected Plan:**

[TIME: ~22s] [COST: free] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getSignaturesForAddress (Solana RPC)
  - MAP, FILTER, GROUP_BY, CLUSTER (Data Processing)
  - CORRELATE, DETECT_PATTERNS, BEHAVIOR_ANALYZE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$behavior_sample_size = 50 // holders to analyze behaviorally

TRY:
  $token_holders = getTokenAccountsByOwner(owner: "*", mint: $target_token, limit: $behavior_sample_size)
CATCH FATAL:
  RETURN ERROR(message: "Holder behavior data unavailable")

GUARD COUNT($token_holders) > 15 ELSE
  RETURN ERROR(message: "Insufficient behavioral data")

// Analyze holder transaction behaviors
$behavioral_patterns = {
  holder_transactions: MAP(collection: $token_holders, fn: holder => {
    address: holder.account.owner,
    balance: parseU64(holder.account.data.parsed.info.tokenAmount.amount),
    transaction_history: getSignaturesForAddress(address: holder.account.owner, limit: 20),
    activity_score: COUNT(getSignaturesForAddress(address: holder.account.owner, limit: 20)) / 20
  }),
  behavioral_clusters: CLUSTER(
    data: $behavioral_patterns.holder_transactions,
    features: ["balance", "activity_score", "transaction_frequency"],
    method: "behavioral_clustering",
    clusters: 3
  )
}

$psychological_insights = {
  holder_personalities: {
    whales: FILTER(collection: $behavioral_clusters, c => MEAN(collection: c.members, field: "balance") > PERCENTILE(collection: MAP($behavioral_patterns.holder_transactions, h => h.balance), 90)),
    traders: FILTER(collection: $behavioral_clusters, c => MEAN(collection: c.members, field: "activity_score") > 0.7),
    hodlers: FILTER(collection: $behavioral_clusters, c => MEAN(collection: c.members, field: "activity_score") < 0.3)
  },
  market_sentiment: CORRELATE(
    x: MAP($behavioral_patterns.holder_transactions, h => h.activity_score),
    y: MAP($behavioral_patterns.holder_transactions, h => h.balance)
  )
}

**Decision Point:** Interpret behavioral psychology and market sentiment
  BRANCH A (COUNT($psychological_insights.holder_personalities.traders) > COUNT($behavioral_patterns.holder_transactions) * 0.4 AND $psychological_insights.market_sentiment.correlation < -0.3):
    $behavioral_profile = "active_trading_community_speculative_sentiment"
    $psychological_dynamics = "high_anxiety_frequent_position_adjustments"
    $confidence = 81
  BRANCH B (COUNT($psychological_insights.holder_personalities.hodlers) > COUNT($behavioral_patterns.holder_transactions) * 0.6 AND $psychological_insights.market_sentiment.correlation > 0.4):
    $behavioral_profile = "long_term_investors_confident_sentiment"
    $psychological_dynamics = "strong_conviction_low_trading_anxiety"
    $confidence = 83
  BRANCH C (COUNT($psychological_insights.holder_personalities.whales) > COUNT($behavioral_patterns.holder_transactions) * 0.2 AND STDDEV(collection: MAP($behavioral_patterns.holder_transactions, h => h.activity_score)) > 0.5):
    $behavioral_profile = "whale_influenced_market_mixed_sentiment"
    $psychological_dynamics = "power_imbalance_heightened_market_sensitivity"
    $confidence = 82

**Action:**
RETURN {
  token_mint: $target_token,
  behavioral_profile: $behavioral_profile,
  psychological_dynamics: $psychological_dynamics,
  behavioral_analysis: {
    holders_analyzed: COUNT($behavioral_patterns.holder_transactions),
    personality_distribution: {
      whales: COUNT($psychological_insights.holder_personalities.whales),
      traders: COUNT($psychological_insights.holder_personalities.traders),
      hodlers: COUNT($psychological_insights.holder_personalities.hodlers)
    },
    activity_correlation: $psychological_insights.market_sentiment.correlation
  },
  sentiment_insights: {
    market_confidence: $psychological_insights.market_sentiment.correlation > 0.3 ? "positive_sentiment" : "cautious_sentiment",
    behavioral_stability: STDDEV(collection: MAP($behavioral_patterns.holder_transactions, h => h.activity_score)) < 0.4 ? "stable_behavior" : "volatile_behavior",
    community_maturity: COUNT($psychological_insights.holder_personalities.hodlers) > COUNT($behavioral_patterns.holder_transactions) * 0.5 ? "mature_community" : "speculative_community"
  },
  confidence: $confidence,
  note: "Holder behavioral patterns reveal psychological dynamics, market sentiment, and community maturity"
}

---

## Q15: "What does the token's cross-chain bridge activity reveal about its interoperability and global adoption?"

**Expected Plan:**

[TIME: ~16s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getSignaturesForAddress (Solana RPC)
  - MAP, FILTER, COUNT, GROUP_BY, UNIQUE (Data Processing)
  - CORRELATE, DETECT_PATTERNS (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

TRY:
  // Analyze bridge protocol interactions
  $bridge_protocols = ["wormDTUJ6AWPNvk59vGQbDvGJmqbDTdgWgAqcLBCgUb", "WormT3McKhFJ2RkiGpdw9GKvNCrB2aB54gb2uV9MfQC"] // Wormhole bridges
  $bridge_activity = []
  
  FOR $bridge IN $bridge_protocols:
    $bridge_transactions = getSignaturesForAddress(address: $bridge, limit: 100)
    $token_bridge_txs = FILTER($bridge_transactions, tx => {
      tx_details: getTransaction(signature: tx.signature),
      involves_token: CONTAINS(tx_details.transaction.message.accountKeys, $target_token)
    })
    $bridge_activity = APPEND(array: $bridge_activity, item: {
      bridge: $bridge,
      token_transfers: COUNT($token_bridge_txs),
      volume_estimate: SUM(collection: MAP($token_bridge_txs, tx => getTransaction(signature: tx.signature).meta.fee))
    })
  CATCH FATAL:
    RETURN ERROR(message: "Bridge activity data unavailable")

// Analyze interoperability patterns
$interoperability_analysis = {
  total_bridge_transfers: SUM(collection: $bridge_activity, field: "token_transfers"),
  active_bridges: COUNT(collection: FILTER($bridge_activity, b => b.token_transfers > 0)),
  bridge_diversity: COUNT(collection: UNIQUE(MAP($bridge_activity, b => b.bridge))),
  cross_chain_volume: SUM(collection: $bridge_activity, field: "volume_estimate")
}

$global_adoption = {
  interoperability_score: $interoperability_analysis.active_bridges / COUNT($bridge_protocols),
  bridge_maturity: $interoperability_analysis.total_bridge_transfers > 50 ? "mature_cross_chain" : "emerging_interoperability",
  global_reach: $interoperability_analysis.bridge_diversity > 1 ? "multi_chain_presence" : "solana_centric"
}

**Decision Point:** Evaluate interoperability and global adoption
  BRANCH A ($global_adoption.interoperability_score > 0.7 AND $global_adoption.bridge_maturity == "mature_cross_chain" AND $interoperability_analysis.bridge_diversity > 2):
    $interoperability_profile = "highly_interoperable_global_token"
    $adoption_scope = "worldwide_adoption_cross_chain_ecosystem"
    $confidence = 83
  BRANCH B ($global_adoption.interoperability_score < 0.3 AND $global_adoption.bridge_maturity == "emerging_interoperability"):
    $interoperability_profile = "solana_native_limited_cross_chain"
    $adoption_scope = "regional_adoption_solana_ecosystem_focus"
    $confidence = 85
  BRANCH C ($global_adoption.interoperability_score > 0.4 AND $interoperability_analysis.total_bridge_transfers > 20 AND $interoperability_analysis.bridge_diversity > 1):
    $interoperability_profile = "developing_interoperability_growing_global_reach"
    $adoption_scope = "expanding_adoption_multi_chain_presence"
    $confidence = 84

**Action:**
RETURN {
  token_mint: $target_token,
  interoperability_profile: $interoperability_profile,
  adoption_scope: $adoption_scope,
  bridge_analysis: {
    total_bridge_transfers: $interoperability_analysis.total_bridge_transfers,
    active_bridges: $interoperability_analysis.active_bridges,
    bridge_diversity: $interoperability_analysis.bridge_diversity,
    estimated_cross_chain_volume: $interoperability_analysis.cross_chain_volume
  },
  global_insights: {
    interoperability_score: $global_adoption.interoperability_score,
    bridge_maturity: $global_adoption.bridge_maturity,
    geographic_reach: $global_adoption.global_reach
  },
  confidence: $confidence,
  note: "Bridge activity reveals interoperability level, global adoption scope, and cross-chain ecosystem integration"
}

---

## Q16: "How does the token's staking participation reveal governance engagement and long-term commitment?"

**Expected Plan:**

[TIME: ~19s] [COST: free] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getTokenAccountsByOwner (Solana RPC)
  - MAP, FILTER, COUNT, GROUP_BY (Data Processing)
  - MEAN, PERCENTILE, CORRELATE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

TRY:
  // Analyze staking program interactions
  $staking_programs = ["Stake11111111111111111111111111111112", "So1endDq2Ykq989xLMWXmwkPyc14TcEgKdZWtxs57d"] // Native staking + Lido-style
  $staking_participation = []
  
  FOR $program IN $staking_programs:
    $staked_accounts = getProgramAccounts(programId: $program, filters: [
      { memcmp: { offset: 0, bytes: $target_token } }
    ])
    $staking_participation = APPEND(array: $staking_participation, item: {
      program: $program,
      staked_accounts: COUNT($staked_accounts),
      total_staked: SUM(collection: MAP($staked_accounts, acc => parseU64(acc.account.data.parsed.info.tokenAmount.amount)))
    })
  CATCH FATAL:
    RETURN ERROR(message: "Staking participation data unavailable")

// Analyze governance and commitment patterns
$governance_analysis = {
  total_staked_tokens: SUM(collection: $staking_participation, field: "total_staked"),
  staking_participation_rate: SUM(collection: $staking_participation, field: "staked_accounts") / getTokenAccountsByOwner(owner: "*", mint: $target_token, limit: 1000).length,
  staking_concentration: PERCENTILE(data: MAP($staking_participation, p => p.total_staked), percentile: 90) / SUM(collection: $staking_participation, field: "total_staked"),
  commitment_duration: MEAN(collection: MAP($staking_participation, p => p.staked_accounts > 0 ? 180 : 0)) // assume 180 day average lock
}

$commitment_insights = {
  governance_engagement: $governance_analysis.staking_participation_rate > 0.1 ? "active_governance" : "passive_holding",
  commitment_level: $governance_analysis.commitment_duration > 90 ? "long_term_commitment" : "short_term_participation",
  decentralization_score: 1 - $governance_analysis.staking_concentration
}

**Decision Point:** Evaluate governance engagement and commitment levels
  BRANCH A ($commitment_insights.governance_engagement == "active_governance" AND $commitment_insights.commitment_level == "long_term_commitment" AND $governance_analysis.decentralization_score > 0.7):
    $governance_profile = "highly_engaged_decentralized_governance"
    $commitment_dynamics = "strong_community_alignment_long_term_vision"
    $confidence = 82
  BRANCH B ($governance_analysis.staking_participation_rate < 0.05 AND $governance_analysis.staking_concentration > 0.8):
    $governance_profile = "concentrated_control_limited_participation"
    $commitment_dynamics = "centralized_governance_short_term_focus"
    $confidence = 84
  BRANCH C ($commitment_insights.governance_engagement == "active_governance" AND $governance_analysis.decentralization_score > 0.5 AND $governance_analysis.commitment_duration > 60):
    $governance_profile = "developing_governance_growing_participation"
    $commitment_dynamics = "emerging_community_governance_medium_term_commitment"
    $confidence = 83

**Action:**
RETURN {
  token_mint: $target_token,
  governance_profile: $governance_profile,
  commitment_dynamics: $commitment_dynamics,
  staking_analysis: {
    total_staked_tokens: $governance_analysis.total_staked_tokens,
    staking_participation_rate: $governance_analysis.staking_participation_rate,
    staking_concentration: $governance_analysis.staking_concentration,
    average_commitment_duration: $governance_analysis.commitment_duration
  },
  governance_insights: {
    engagement_level: $commitment_insights.governance_engagement,
    decentralization_score: $governance_analysis.decentralization_score,
    commitment_horizon: $commitment_insights.commitment_level
  },
  confidence: $confidence,
  note: "Staking participation reveals governance engagement, community commitment, and decentralization levels"
}

---

## Q17: "What does the token's smart contract interaction complexity reveal about its technological sophistication?"

**Expected Plan:**

[TIME: ~21s] [COST: free] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getTransaction (Solana RPC)
  - MAP, FILTER, COUNT, GROUP_BY, UNIQUE (Data Processing)
  - CORRELATE, COMPLEXITY_ANALYZE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

TRY:
  $token_program_accounts = getProgramAccounts(programId: "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", filters: [
    { memcmp: { offset: 0, bytes: $target_token } }
  ])
  $sample_transactions = getSignaturesForAddress(address: $target_token, limit: 50)
  $detailed_transactions = MAP(collection: $sample_transactions, fn: sig => getTransaction(signature: sig.signature))
CATCH FATAL:
  RETURN ERROR(message: "Smart contract interaction data unavailable")

GUARD COUNT($detailed_transactions) > 10 ELSE
  RETURN ERROR(message: "Insufficient interaction data")

// Analyze interaction complexity
$complexity_analysis = {
  unique_instruction_types: UNIQUE(collection: FLATTEN(MAP($detailed_transactions, tx => MAP(tx.transaction.message.instructions, i => i.programId)))),
  average_instructions_per_tx: MEAN(collection: MAP($detailed_transactions, tx => COUNT(tx.transaction.message.instructions))),
  account_interactions: MEAN(collection: MAP($detailed_transactions, tx => COUNT(tx.transaction.message.accountKeys))),
  program_diversity: COUNT($complexity_analysis.unique_instruction_types),
  interaction_complexity: COMPLEXITY_ANALYZE(data: $detailed_transactions, metric: "instruction_entropy")
}

$technological_sophistication = {
  interaction_complexity_score: $complexity_analysis.interaction_complexity,
  automation_level: $complexity_analysis.average_instructions_per_tx > 3 ? "high_automation" : "manual_operations",
  integration_depth: $complexity_analysis.program_diversity > 5 ? "deep_integration" : "shallow_integration"
}

**Decision Point:** Evaluate technological sophistication and complexity
  BRANCH A ($technological_sophistication.interaction_complexity_score > 0.8 AND $technological_sophistication.integration_depth == "deep_integration" AND $complexity_analysis.average_instructions_per_tx > 4):
    $sophistication_level = "highly_sophisticated_advanced_automation"
    $technological_profile = "cutting_edge_technology_complex_integrations"
    $confidence = 80
  BRANCH B ($technological_sophistication.interaction_complexity_score < 0.4 AND $technological_sophistication.automation_level == "manual_operations" AND $complexity_analysis.program_diversity < 3):
    $sophistication_level = "basic_technology_simple_operations"
    $technological_profile = "fundamental_utility_minimal_complexity"
    $confidence = 82
  BRANCH C ($technological_sophistication.interaction_complexity_score > 0.5 AND $complexity_analysis.program_diversity > 3 AND $complexity_analysis.average_instructions_per_tx > 2):
    $sophistication_level = "moderately_sophisticated_developing_automation"
    $technological_profile = "evolving_technology_growing_capabilities"
    $confidence = 81

**Action:**
RETURN {
  token_mint: $target_token,
  sophistication_level: $sophistication_level,
  technological_profile: $technological_profile,
  complexity_analysis: {
    total_transactions_analyzed: COUNT($detailed_transactions),
    unique_programs_interacted: $complexity_analysis.program_diversity,
    average_instructions_per_transaction: $complexity_analysis.average_instructions_per_tx,
    average_accounts_per_transaction: $complexity_analysis.account_interactions,
    interaction_complexity_score: $complexity_analysis.interaction_complexity_score
  },
  sophistication_insights: {
    automation_level: $technological_sophistication.automation_level,
    integration_depth: $technological_sophistication.integration_depth,
    technological_maturity: $complexity_analysis.interaction_complexity_score > 0.6 ? "mature_technology" : "developing_technology"
  },
  confidence: $confidence,
  note: "Smart contract interaction complexity reveals technological sophistication, automation level, and integration depth"
}

---

## Q18: "What does the token's transaction success rate reveal about network reliability and user experience quality?"

**Expected Plan:**

[TIME: ~14s] [COST: free] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, COUNT, GROUP_BY (Data Processing)
  - MEAN, PERCENTILE, CORRELATE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$reliability_sample = 200 // transactions to analyze success rates

TRY:
  $transaction_signatures = getSignaturesForAddress(address: $target_token, limit: $reliability_sample)
  $transaction_details = MAP(collection: $transaction_signatures, fn: sig => getTransaction(signature: sig.signature))
CATCH FATAL:
  RETURN ERROR(message: "Transaction reliability data unavailable")

GUARD COUNT($transaction_details) > 50 ELSE
  RETURN ERROR(message: "Insufficient reliability data")

// Analyze transaction success patterns
$reliability_analysis = {
  total_transactions: COUNT($transaction_details),
  successful_transactions: COUNT(collection: FILTER($transaction_details, tx => tx.meta.err == null)),
  failed_transactions: COUNT(collection: FILTER($transaction_details, tx => tx.meta.err != null)),
  success_rate: COUNT(collection: FILTER($transaction_details, tx => tx.meta.err == null)) / COUNT($transaction_details),
  error_distribution: GROUP_BY(collection: FILTER($transaction_details, tx => tx.meta.err != null), key: tx => tx.meta.err.toString())
}

$user_experience = {
  reliability_score: $reliability_analysis.success_rate,
  failure_rate_trend: 1 - $reliability_analysis.success_rate,
  error_consistency: COUNT($reliability_analysis.error_distribution) < 3 ? "consistent_errors" : "diverse_errors",
  network_stability: $reliability_analysis.success_rate > 0.95 ? "highly_stable" : "moderately_stable"
}

**Decision Point:** Evaluate network reliability and user experience
  BRANCH A ($user_experience.reliability_score > 0.98 AND $user_experience.network_stability == "highly_stable" AND $reliability_analysis.failed_transactions < 5):
    $reliability_profile = "exceptionally_reliable_superior_user_experience"
    $network_performance = "world_class_network_performance_near_perfect_execution"
    $confidence = 84
  BRANCH B ($user_experience.reliability_score < 0.85 AND $user_experience.failure_rate_trend > 0.15 AND COUNT($reliability_analysis.error_distribution) > 5):
    $reliability_profile = "unreliable_network_poor_user_experience"
    $network_performance = "problematic_network_frequent_transaction_failures"
    $confidence = 85
  BRANCH C ($user_experience.reliability_score > 0.92 AND $user_experience.reliability_score < 0.97 AND $user_experience.error_consistency == "consistent_errors"):
    $reliability_profile = "moderately_reliable_acceptable_user_experience"
    $network_performance = "stable_network_occasional_expected_failures"
    $confidence = 86

**Action:**
RETURN {
  token_mint: $target_token,
  reliability_profile: $reliability_profile,
  network_performance: $network_performance,
  reliability_metrics: {
    total_transactions_analyzed: $reliability_analysis.total_transactions,
    successful_transactions: $reliability_analysis.successful_transactions,
    failed_transactions: $reliability_analysis.failed_transactions,
    success_rate_percentage: $reliability_analysis.success_rate * 100,
    unique_error_types: COUNT($reliability_analysis.error_distribution)
  },
  experience_insights: {
    reliability_score: $user_experience.reliability_score,
    error_consistency: $user_experience.error_consistency,
    network_stability: $user_experience.network_stability
  },
  confidence: $confidence,
  note: "Transaction success rates reveal network reliability, user experience quality, and operational stability"
}

---

## Q19: "What does the token's holder diversification across geographies reveal about its global distribution strategy?"

**Expected Plan:**

[TIME: ~17s] [COST: free] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getSignaturesForAddress (Solana RPC)
  - MAP, FILTER, GROUP_BY, CLUSTER (Data Processing)
  - CORRELATE, GEOGRAPHIC_ANALYZE (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
$geographic_sample = 100 // holders to analyze geographically

TRY:
  $token_holders = getTokenAccountsByOwner(owner: "*", mint: $target_token, limit: $geographic_sample)
  $holder_activity = MAP(collection: $token_holders, fn: holder => {
    address: holder.account.owner,
    recent_transactions: getSignaturesForAddress(address: holder.account.owner, limit: 10),
    activity_timezone: DETECT_TIMEZONE(transactions: recent_transactions)
  })
CATCH FATAL:
  RETURN ERROR(message: "Geographic distribution data unavailable")

GUARD COUNT($holder_activity) > 30 ELSE
  RETURN ERROR(message: "Insufficient geographic data")

// Analyze geographic distribution patterns
$geographic_analysis = {
  timezone_clusters: GROUP_BY(collection: $holder_activity, key: holder => holder.activity_timezone),
  geographic_diversity: COUNT($geographic_analysis.timezone_clusters),
  dominant_regions: SORT_BY(collection: MAP($geographic_analysis.timezone_clusters, tz => {
    timezone: tz.key,
    holder_count: COUNT(tz.items),
    percentage: COUNT(tz.items) / COUNT($holder_activity)
  }), field: "holder_count", descending: true),
  distribution_entropy: GEOGRAPHIC_DIVERSITY_SCORE(data: $holder_activity)
}

$global_strategy = {
  geographic_reach: $geographic_analysis.geographic_diversity > 3 ? "global_distribution" : "regional_focus",
  market_penetration: $geographic_analysis.distribution_entropy > 0.7 ? "broad_penetration" : "concentrated_markets",
  internationalization_score: $geographic_analysis.geographic_diversity / 24 // max timezones
}

**Decision Point:** Evaluate global distribution strategy and geographic reach
  BRANCH A ($global_strategy.geographic_reach == "global_distribution" AND $global_strategy.market_penetration == "broad_penetration" AND $geographic_analysis.distribution_entropy > 0.8):
    $distribution_strategy = "truly_global_ubiquitous_adoption"
    $geographic_profile = "worldwide_presence_comprehensive_market_coverage"
    $confidence = 81
  BRANCH B ($global_strategy.geographic_reach == "regional_focus" AND $geographic_analysis.dominant_regions[0].percentage > 0.6):
    $distribution_strategy = "regional_stronghold_focused_market_strategy"
    $geographic_profile = "dominant_region_specialized_local_adoption"
    $confidence = 83
  BRANCH C ($global_strategy.internationalization_score > 0.2 AND $global_strategy.internationalization_score < 0.5 AND $geographic_analysis.geographic_diversity > 2):
    $distribution_strategy = "multi_region_developing_global_presence"
    $geographic_profile = "growing_international_expanding_geographic_reach"
    $confidence = 82

**Action:**
RETURN {
  token_mint: $target_token,
  distribution_strategy: $distribution_strategy,
  geographic_profile: $geographic_profile,
  geographic_analysis: {
    holders_analyzed: COUNT($holder_activity),
    geographic_diversity_score: $geographic_analysis.geographic_diversity,
    distribution_entropy: $geographic_analysis.distribution_entropy,
    top_regions: SLICE($geographic_analysis.dominant_regions, 0, 3)
  },
  global_insights: {
    internationalization_score: $global_strategy.internationalization_score,
    market_penetration: $global_strategy.market_penetration,
    geographic_reach: $global_strategy.geographic_reach
  },
  confidence: $confidence,
  note: "Geographic distribution reveals global strategy, market penetration, and internationalization approach"
}

---

## Q20: "What does the token's metadata update frequency reveal about project development velocity and governance activity?"

**Expected Plan:**

[TIME: ~15s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getSignaturesForAddress (Solana RPC)
  - MAP, FILTER, SORT_BY, GROUP_BY (Data Processing)
  - CORRELATE, TREND_ANALYZE, MEAN (Statistical)

**Main Branch:**
$target_token = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

TRY:
  $account_info = getAccountInfo(account: $target_token)
  $authority_updates = getSignaturesForAddress(address: $account_info.owner, limit: 100)
  $metadata_transactions = FILTER($authority_updates, tx => {
    tx_details: getTransaction(signature: tx.signature),
    involves_token: CONTAINS(tx_details.transaction.message.accountKeys, $target_token)
  })
CATCH FATAL:
  RETURN ERROR(message: "Metadata update data unavailable")

GUARD COUNT($metadata_transactions) > 5 ELSE
  RETURN ERROR(message: "Insufficient metadata activity")

// Analyze metadata update patterns
$metadata_analysis = {
  total_updates: COUNT($metadata_transactions),
  update_frequency: COUNT($metadata_transactions) / (($metadata_transactions[0].blockTime - $metadata_transactions[COUNT($metadata_transactions) - 1].blockTime) / 86400),
  update_timeline: MAP($metadata_transactions, tx => tx.blockTime),
  update_velocity: TREND_ANALYZE(data: $metadata_analysis.update_timeline, field: "blockTime"),
  governance_activity: GROUP_BY(collection: $metadata_transactions, key: tx => FLOOR(tx.blockTime / 604800)) // weekly
}

$development_velocity = {
  update_cadence: $metadata_analysis.update_frequency > 1 ? "frequent_updates" : "infrequent_updates",
  project_maturity: $metadata_analysis.update_velocity.trend > 0 ? "accelerating_development" : "stabilizing_project",
  governance_health: STDDEV(collection: MAP($metadata_analysis.governance_activity, week => COUNT(week.items))) < 2 ? "consistent_governance" : "variable_activity"
}

**Decision Point:** Evaluate development velocity and governance activity
  BRANCH A ($development_velocity.update_cadence == "frequent_updates" AND $development_velocity.project_maturity == "accelerating_development" AND $development_velocity.governance_health == "consistent_governance"):
    $development_profile = "highly_active_rapidly_evolving_project"
    $governance_dynamics = "vibrant_governance_continuous_improvement_culture"
    $confidence = 83
  BRANCH B ($development_velocity.update_cadence == "infrequent_updates" AND $metadata_analysis.update_frequency < 0.1 AND STDDEV(collection: MAP($metadata_analysis.governance_activity, week => COUNT(week.items))) > 3):
    $development_profile = "stagnant_project_limited_development_activity"
    $governance_dynamics = "inactive_governance_minimal_project_engagement"
    $confidence = 85
  BRANCH C ($development_velocity.update_cadence == "frequent_updates" AND $development_velocity.governance_health == "consistent_governance" AND $metadata_analysis.update_frequency > 0.5):
    $development_profile = "actively_maintained_steady_development_pace"
    $governance_dynamics = "healthy_governance_balanced_project_evolution"
    $confidence = 84

**Action:**
RETURN {
  token_mint: $target_token,
  development_profile: $development_profile,
  governance_dynamics: $governance_dynamics,
  metadata_analysis: {
    total_updates_analyzed: $metadata_analysis.total_updates,
    updates_per_day: $metadata_analysis.update_frequency,
    development_trend: $metadata_analysis.update_velocity.trend,
    governance_consistency: STDDEV(collection: MAP($metadata_analysis.governance_activity, week => COUNT(week.items)))
  },
  velocity_insights: {
    update_cadence: $development_velocity.update_cadence,
    project_maturity: $development_velocity.project_maturity,
    governance_health: $development_velocity.governance_health
  },
  confidence: $confidence,
  note: "Metadata update frequency reveals development velocity, governance activity, and project evolution pace"
}

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q21: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q22: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q23: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q24: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q25: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q26: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q27: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q28: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q29: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q30: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q31: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q32: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q33: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q34: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q35: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q36: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q37: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q38: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q39: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q40: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q41: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q42: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q43: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q44: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q45: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q46: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q47: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q48: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q49: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q50: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q51: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q52: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q53: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q54: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q55: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q56: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q57: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q58: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q59: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q60: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q61: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q62: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q63: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q64: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q65: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q66: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q67: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q68: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q69: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q70: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q71: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q72: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q73: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q74: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q75: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q76: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q77: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q78: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q79: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q80: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q81: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q82: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q83: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q84: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q85: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q86: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q87: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q88: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q89: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q90: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q91: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q92: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q93: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q94: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q95: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q96: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q97: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q98: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q99: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q100: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---


