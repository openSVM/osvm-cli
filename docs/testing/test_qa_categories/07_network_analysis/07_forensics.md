# Network Analysis - Forensics Level

## Q1: "Debug network performance degradation"

**Expected Plan:**

[TIME: ~25s] [COST: ~0.02 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getSlot, getRecentPerformanceSamples, getVoteAccounts (Solana RPC)
  - MAP, FILTER, SORT_BY (Data Processing)
  - MEAN, PERCENTILE, CORRELATE (Statistical)

**Main Branch:**
// Gather network metrics
PARALLEL {
  $current_slot = getSlot()
  $performance_samples = getRecentPerformanceSamples(limit: 720)  // ~1 hour
  $vote_accounts = getVoteAccounts()
}
WAIT_ALL

// Analyze performance samples
$sample_metrics = MAP(
  collection: $performance_samples,
  fn: sample => {
    slot: sample.slot,
    num_transactions: sample.numTransactions,
    num_slots: sample.numSlots,
    sample_period_secs: sample.samplePeriodSecs,
    tps: sample.numTransactions / sample.samplePeriodSecs,
    slots_per_second: sample.numSlots / sample.samplePeriodSecs
  }
)

// Calculate TPS statistics
$tps_values = MAP(collection: $sample_metrics, fn: m => m.tps)
$mean_tps = MEAN(data: $tps_values)
$p50_tps = PERCENTILE(data: $tps_values, percentile: 50)
$p95_tps = PERCENTILE(data: $tps_values, percentile: 95)
$min_tps = MIN(values: $tps_values)
$max_tps = MAX(values: $tps_values)

// Detect performance anomalies
$low_performance_periods = FILTER(
  collection: $sample_metrics,
  predicate: sample => sample.tps < ($mean_tps * 0.5)
)

// Analyze validator performance
$delinquent_validators = FILTER(
  collection: $vote_accounts.delinquent,
  predicate: v => v.activatedStake > 0
)

$total_stake = $vote_accounts.current[0].activatedStake + $vote_accounts.delinquent[0].activatedStake
$delinquent_stake = SUM(values: MAP(collection: $delinquent_validators, fn: v => v.activatedStake))
$delinquent_stake_pct = ($delinquent_stake / $total_stake) * 100

**Decision Point:** Diagnose performance issue
  BRANCH A (COUNT(collection: $low_performance_periods) > 100):
    $diagnosis = "sustained_degradation"
    $severity = "critical"
    $root_cause = "Network experiencing prolonged performance issues"
    $recommended_action = "Check for network-wide consensus issues"
    
  BRANCH B ($delinquent_stake_pct > 10):
    $diagnosis = "validator_issues"
    $severity = "high"
    $root_cause = "High percentage of stake is delinquent"
    $recommended_action = "Investigate validator connectivity and performance"
    
  BRANCH C ($mean_tps < 1000):
    $diagnosis = "low_throughput"
    $severity = "medium"
    $root_cause = "Network throughput below normal levels"
    $recommended_action = "Monitor for congestion or spam"
    
  BRANCH D ($max_tps - $min_tps > $mean_tps * 2):
    $diagnosis = "high_volatility"
    $severity = "medium"
    $root_cause = "Highly variable performance"
    $recommended_action = "Check for traffic spikes or validator instability"
    
  BRANCH E (true):
    $diagnosis = "healthy"
    $severity = "low"
    $root_cause = "Network performing within normal parameters"
    $recommended_action = "Continue monitoring"

**Action:**
RETURN {
  current_slot: $current_slot,
  mean_tps: $mean_tps,
  p50_tps: $p50_tps,
  p95_tps: $p95_tps,
  min_tps: $min_tps,
  max_tps: $max_tps,
  low_performance_periods: COUNT(collection: $low_performance_periods),
  delinquent_validators: COUNT(collection: $delinquent_validators),
  delinquent_stake_pct: $delinquent_stake_pct,
  diagnosis: $diagnosis,
  severity: $severity,
  root_cause: $root_cause,
  recommended_action: $recommended_action,
  confidence: 88
}
