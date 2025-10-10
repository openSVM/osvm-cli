# Token Research - Practical Token Analysis (Q301-Q400)

**Category:** Token Research
**Difficulty:** Intermediate
**Focus:** Practical token analysis, tokenomics, liquidity, holder behavior, competitive analysis, fundamental value
**Questions:** Q301-Q400

---

## Q5301: "What is the tokenomics breakdown for 'Jupiter' (JUP), including total supply, circulating supply, and the distribution between team, investors, and community?"

**Expected Plan:**

[TIME: ~30s] [COST: ~0.002 SOL] [CONFIDENCE: 98%]

**Available Tools:**
From Standard Library:
  - getTokenSupply (Solana RPC)
  - getAccountInfo (Solana RPC)
  - ANALYZE_TOKENOMICS_DISTRIBUTION (Analysis)
  - SEARCH_DOCS (Web/External)

**Main Branch:**
$token_mint = "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN"

// Get on-chain supply data
$total_supply = getTokenSupply(mint: $token_mint).value.uiAmount
$circulating_supply_onchain = GET_CIRCULATING_SUPPLY(mint: $token_mint) // A conceptual tool

// Find official tokenomics documentation
$docs = SEARCH_DOCS(query: "Jupiter JUP tokenomics official")
$documented_allocations = PARSE_TOKENOMICS_DOCS($docs)

// Analyze known large wallets (team, treasury)
$distribution = ANALYZE_TOKENOMICS_DISTRIBUTION(
  mint: $token_mint,
  known_allocations: $documented_allocations
)

**Decision Point:** Compare on-chain data with documented tokenomics.
  BRANCH A ($distribution.discrepancy_found):
    $verdict = "Discrepancy Found"
    $details = "On-chain holder distribution does not perfectly match the officially stated tokenomics. Further investigation of large wallets is needed."
  BRANCH B (default):
    $verdict = "Matches Documentation"
    $details = "On-chain data aligns with the documented allocation for team, investors, and community."

**Action:**
RETURN {
  verdict: $verdict,
  details: $details,
  total_supply: $total_supply,
  circulating_supply: $circulating_supply_onchain,
  allocations: $distribution.allocations,
  confidence: 98
}

---

## Q5302: "How deep is the liquidity for the 'dogwifhat' (WIF) token on its primary Raydium pool, and how large of a trade can be made before experiencing 3% price impact?"

**Expected Plan:**

[TIME: ~40s] [COST: ~0.004 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - RAYDIUM_GET_POOL_INFO (Raydium API)
  - CALCULATE_TRADE_IMPACT (DeFi)

**Main Branch:**
$pool_id = "..." // The primary WIF/SOL pool on Raydium

// Get the current state of the liquidity pool
$pool_info = RAYDIUM_GET_POOL_INFO(pool_id: $pool_id)
$sol_reserves = $pool_info.reserves.sol
$wif_reserves = $pool_info.reserves.wif

// Calculate trade sizes for different price impact levels
$impact_analysis = CALCULATE_TRADE_IMPACT(
  base_reserves: $sol_reserves,
  quote_reserves: $wif_reserves,
  impact_levels_bps: [100, 200, 300, 500] // 1%, 2%, 3%, 5%
)

$trade_for_3_percent_impact = FILTER(collection: $impact_analysis, predicate: i => i.impact_bps == 300)[0]

**Decision Point:** Evaluate the liquidity depth.
  BRANCH A ($trade_for_3_percent_impact.trade_size_usd > 100000):
    $liquidity_level = "Deep"
    $assessment = "The pool is very deep. A trade of over $100,000 can be executed with less than 3% price impact."
  BRANCH B ($trade_for_3_percent_impact.trade_size_usd > 20000):
    $liquidity_level = "Moderate"
    $assessment = "The pool has moderate liquidity. Whales need to be cautious, but retail-sized trades will have minimal impact."
  BRANCH C (default):
    $liquidity_level = "Shallow"
    $assessment = "The pool is shallow. Even trades above a few thousand dollars will cause significant slippage."

**Action:**
RETURN {
  liquidity_level: $liquidity_level,
  assessment: $assessment,
  total_liquidity_usd: $pool_info.liquidity_usd,
  trade_size_for_3_percent_impact_usd: $trade_for_3_percent_impact.trade_size_usd,
  full_impact_analysis: $impact_analysis,
  confidence: 95
}

---

## Q5303: "What are the primary on-chain use cases and demand drivers for the 'Pyth Network' (PYTH) token?"

**Expected Plan:**

[TIME: ~60s] [COST: ~0.005 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts (Solana RPC)
  - ANALYZE_PROGRAM_INTERACTIONS (Analysis)
  - SEARCH_DOCS (Web/External)

**Main Branch:**
$token_mint = "HZ1JovNiVvGrGNiiYvEozEVgZ58AQuUnqeEhw2jwF7ht" // PYTH
$pyth_program_id = "..." // Pyth's main oracle program

// 1. Find documented utility
$docs = SEARCH_DOCS(query: "Pyth Network PYTH token utility")
$documented_uses = PARSE_UTILITY_DOCS($docs) // e.g., Governance, Staking

// 2. Find on-chain usage by seeing which programs hold/interact with PYTH
$pyth_holders = getProgramAccounts(programId: "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", filters: [{tokenMint: $token_mint}])
$interacting_programs = ANALYZE_PROGRAM_INTERACTIONS(token_holders: $pyth_holders)

**Decision Point:** Synthesize the findings.
  BRANCH A (CONTAINS($documented_uses, "Governance") && $interacting_programs.has_governance_usage):
    $primary_use = "Governance"
    $details = "The main use case is participating in Pyth DAO governance to vote on proposals. This is confirmed by on-chain data showing large amounts of PYTH locked in the governance contract."
  BRANCH B (CONTAINS($documented_uses, "Staking") && $interacting_programs.has_staking_usage):
    $details = "The token is used for staking by data publishers. On-chain analysis shows significant flows to and from known publisher wallets and staking contracts."
  BRANCH C (default):
    $primary_use = "Speculation"
    $details = "While documentation mentions utility, the majority of on-chain volume is on DEXs, suggesting speculation is the primary driver currently."

**Action:**
RETURN {
  details: $details,
  documented_utilities: $documented_uses,
  onchain_interactions: $interacting_programs.summary,
  confidence: 90
}

---

## Q5304: "What percentage of 'Marinade Staked SOL' (mSOL) holders are actively using it in DeFi protocols versus just holding it in their wallets?"

**Expected Plan:**

[TIME: ~75s] [COST: ~0.01 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - getTokenLargestAccounts (Solana RPC)
  - ANALYZE_HOLDER_BEHAVIOR (Analysis)
  - MAP, FILTER, SUM (Data Processing)

**Main Branch:**
$token_mint = "mSoLzYCxHdYgdzU16g5QSh3i5K3z3KZK7ytfqcJm7So"

// Get a large sample of mSOL holders
$holders = getTokenLargestAccounts(mint: $token_mint, limit: 1000)

// Analyze the behavior of each holder
$behavior_analysis = ANALYZE_HOLDER_BEHAVIOR(holders: $holders)

// Categorize holders
$defi_users = FILTER(collection: $behavior_analysis, predicate: h => h.is_using_in_defi)
$passive_holders = FILTER(collection: $behavior_analysis, predicate: h => !h.is_using_in_defi)

// Calculate percentages based on volume
$total_volume = SUM(MAP(collection: $holders, fn: h => h.amount))
$defi_volume = SUM(MAP(collection: $defi_users, fn: h => h.amount))
$passive_volume = SUM(MAP(collection: $passive_holders, fn: h => h.amount))

$defi_percentage = ($defi_volume / $total_volume) * 100
$passive_percentage = ($passive_volume / $total_volume) * 100

**Decision Point:** Interpret the holder behavior.
  BRANCH A ($defi_percentage > 60):
    $conclusion = "Highly active in DeFi."
    $summary = "A strong majority of mSOL is being actively used as collateral or in liquidity pools, indicating high composability and utility."
  BRANCH B ($defi_percentage > 30):
    $conclusion = "Moderately active in DeFi."
    $summary = "A significant portion of mSOL is used in DeFi, but a large amount is also held passively, suggesting users value it as both a productive asset and a store of value."
  BRANCH C (default):

    ## Q321: "Which meme tokens have the highest percentage of holders in profit, and what entry prices did they buy at?"

    ## Q322: "How can you analyze the liquidity depth of a new meme token to determine the maximum safe buy size for a trader?"

    ## Q323: "What is the best way to track the top 10 wallets accumulating a meme token before a major event?"

    ## Q324: "How can you use prediction market odds to inform entry and exit points for meme token trades?"

    ## Q325: "What is the average time between a meme token's launch and its first major whale sell-off?"

    ## Q326: "How can you identify wallets that are likely to dump a meme token based on on-chain behavioral patterns?"

    ## Q327: "What is the most profitable strategy for rotating between meme tokens and prediction market bets during high volatility?"

    ## Q328: "How can you use on-chain volume and order flow to predict the next meme token to trend on social media?"

    ## Q329: "What is the best way to hedge a meme token position using prediction markets on Solana?"

    ## Q330: "How can you detect and avoid wallets that are likely to be exit liquidity in a meme token pump and dump?"
    $summary = "The majority of mSOL is held in wallets, not actively used in other protocols. Its primary value to holders may be the underlying staking yield itself."

**Action:**
RETURN {
  conclusion: $conclusion,
  summary: $summary,
  percentage_in_defi: $defi_percentage,
  percentage_held_passively: $passive_percentage,
  top_defi_protocols_used: $behavior_analysis.top_protocols,
  confidence: 92
}

---

## Q5305: "How does the on-chain development activity for 'Helium Network Token' (HNT) compare to its competitor 'Render' (RNDR) over the last 90 days?"

**Expected Plan:**

[TIME: ~50s] [COST: ~0.005 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - SEARCH_GITHUB (Web/External)
  - GET_ONCHAIN_PROGRAM_UPDATES (Analysis)

**Main Branch:**
$hnt_program_id = "..."
$rndr_program_id = "..."
$hnt_github_repo = "helium/helium-program-library"
$rndr_github_repo = "rendernetwork/render-program-library"

// 1. Get on-chain program updates
$hnt_updates = GET_ONCHAIN_PROGRAM_UPDATES(program_id: $hnt_program_id, time_period: "90d")
$rndr_updates = GET_ONCHAIN_PROGRAM_UPDATES(program_id: $rndr_program_id, time_period: "90d")

// 2. Get GitHub commit activity
$hnt_commits = SEARCH_GITHUB(repo: $hnt_github_repo, query: "commits in last 90 days")
$rndr_commits = SEARCH_GITHUB(repo: $rndr_github_repo, query: "commits in last 90 days")

**Decision Point:** Compare the development activity levels.
  BRANCH A (COUNT($hnt_commits) > COUNT($rndr_commits) && COUNT($hnt_updates) > COUNT($rndr_updates)):
    $winner = "Helium (HNT)"
    $comparison = "HNT shows significantly more development activity both on-chain and in its public GitHub repository compared to RNDR."
  BRANCH B (COUNT($rndr_commits) > COUNT($hnt_commits) && COUNT($rndr_updates) > COUNT($hnt_updates)):
    $winner = "Render (RNDR)"
    $comparison = "RNDR shows significantly more development activity both on-chain and in its public GitHub repository compared to HNT."
  BRANCH C (default):
    $winner = "Comparable"
    $comparison = "Both projects show a similar level of development activity, indicating both are actively being maintained and improved."

**Action:**
RETURN {
  winner: $winner,
  comparison_summary: $comparison,
  hnt_metrics: {
    github_commits_90d: COUNT($hnt_commits),
    onchain_updates_90d: COUNT($hnt_updates)
  },
  rndr_metrics: {
    github_commits_90d: COUNT($rndr_commits),
    onchain_updates_90d: COUNT($rndr_updates)
  },
  confidence: 90
}

---

## Q5306: "What is the current social media sentiment for 'Bonk' (BONK), and how does it correlate with its trading volume over the past 7 days?"

**Expected Plan:**

[TIME: ~60s] [COST: ~0.006 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - TWITTER_GET_SENTIMENT_HISTORY (Social API)
  - BIRDEYE_GET_VOLUME_HISTORY (Birdeye API)
  - CORRELATE_DATASETS (Statistical)

**Main Branch:**
$cashtag = "$BONK"
$token_address = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263"

// Get 7-day history for sentiment and volume
$sentiment_history = TWITTER_GET_SENTIMENT_HISTORY(query: $cashtag, time_period: "7d", interval: "1h")
$volume_history = BIRDEYE_GET_VOLUME_HISTORY(token_address: $token_address, time_period: "7d", interval: "1h")

// Correlate the two time series
$correlation = CORRELATE_DATASETS(
  dataset_A: $sentiment_history,
  dataset_B: $volume_history,
  lag_hours: 1 // Check if sentiment leads volume by 1 hour
)

**Decision Point:** Interpret the correlation between sentiment and volume.
  BRANCH A ($correlation.score > 0.7):
    $finding = "Strong Positive Correlation"
    $interpretation = "Social media sentiment is a strong leading indicator for trading volume. Spikes in positive sentiment are quickly followed by spikes in trading activity."
  BRANCH B ($correlation.score < -0.7):
    $finding = "Strong Negative Correlation"
    $interpretation = "Spikes in negative sentiment are strongly correlated with trading volume, suggesting fear and panic selling are major drivers of activity."
  BRANCH C (default):
    $finding = "Weak Correlation"
    $interpretation = "Social media sentiment does not appear to be a significant driver of trading volume. Volume is likely driven by other factors like market structure or news."

**Action:**
RETURN {
  finding: $finding,
  interpretation: $interpretation,
  correlation_score: $correlation.score,
  sentiment_leading_volume_by_hours: $correlation.lag_hours,
  current_sentiment_score: LAST($sentiment_history).score,
  confidence: 85
}

---

## Q5307: "Is the smart contract for the 'Jeo Boden' (BODEN) token immutable, or does the owner retain potentially dangerous permissions like freezing transfers or minting new tokens?"

**Expected Plan:**

[TIME: ~25s] [COST: ~0.001 SOL] [CONFIDENCE: 99%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - ANALYZE_CONTRACT_PERMISSIONS (Security)

**Main Branch:**
$token_mint = "3psH1Mj1f7yUfaD5gh6a1sHk4S_... " // BODEN mint address

// Get the token's mint account data
$mint_info = getAccountInfo(pubkey: $token_mint)

// Analyze the permissions from the mint account
$permissions = ANALYZE_CONTRACT_PERMISSIONS(mint_account_data: $mint_info)

**Decision Point:** Assess the contract's security and immutability.
  BRANCH A ($permissions.is_mutable && $permissions.has_mint_authority):
    $risk = "EXTREMELY HIGH"
    $details = "The contract is not immutable, and a wallet still holds mint authority. This wallet can create an infinite number of new tokens at any time, destroying the token's value."
  BRANCH B ($permissions.is_mutable && $permissions.has_freeze_authority):
    $risk = "HIGH"
    $details = "The contract is not immutable, and a wallet still holds freeze authority. This wallet can freeze transfers for any holder, effectively locking their assets."
  BRANCH C (!$permissions.is_mutable):
    $risk = "LOW"
    $details = "The contract is immutable. Both the mint and freeze authorities have been revoked, which is the security standard for a decentralized token."
  BRANCH D (default):
    $risk = "MODERATE"
    $details = "The contract may have some remaining authorities, but they are not critical ones like minting new supply."

**Action:**
RETURN {
  risk_level: $risk,
  details: $details,
  is_immutable: !$permissions.is_mutable,
  mint_authority: $permissions.mint_authority,
  freeze_authority: $permissions.freeze_authority,
  confidence: 99
}

---

## Q5308: "What is the distribution of 'JitoSOL' (JITOSOL) holders, and are there any red flags like an excessive concentration in a few non-protocol wallets?"

**Expected Plan:**

[TIME: ~45s] [COST: ~0.004 SOL] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library:
  - getTokenLargestAccounts (Solana RPC)
  - CALCULATE_GINI_COEFFICIENT (Statistical)
  - IDENTIFY_KNOWN_WALLETS (Analysis)

**Main Branch:**
$token_mint = "J1toso1uCk3RLmjorhTtrVwY9HJ7X8V9yYac6Y7kGCPn"

// Get the top 1000 holders
$holders = getTokenLargestAccounts(mint: $token_mint, limit: 1000)

// Identify which of these are known protocol/CEX wallets
$identified_holders = IDENTIFY_KNOWN_WALLETS(wallets: MAP($holders, h => h.address))

// Filter out known wallets to analyze the distribution among individuals/unknowns
$unknown_holders = FILTER($holders, h => !$identified_holders[h.address].is_known)

// Calculate concentration metrics for the unknown holders
$gini_coefficient = CALCULATE_GINI_COEFFICIENT(holders: $unknown_holders)
$top_10_unknown_pct = SUM(MAP(SLICE($unknown_holders, 0, 10), h => h.percentage))

**Decision Point:** Evaluate the holder concentration for red flags.
  BRANCH A ($top_10_unknown_pct > 50):
    $flag = "RED FLAG"
    $assessment = "Holder distribution is dangerously concentrated. The top 10 unknown wallets control over 50% of the supply outside of protocols, giving them immense power over the market."
  BRANCH B ($top_10_unknown_pct > 20):
    $flag = "YELLOW FLAG"
    $assessment = "Holder distribution is somewhat concentrated. While not critical, the top 10 unknown wallets have significant influence."
  BRANCH C (default):
    $flag = "GREEN FLAG"
    $assessment = "Holder distribution appears healthy and decentralized among individual holders."

**Action:**
RETURN {
  red_flag_status: $flag,
  assessment: $assessment,
  gini_coefficient_unknowns: $gini_coefficient,
  top_10_unknown_holders_own_pct: $top_10_unknown_pct,
  top_10_unknown_wallets: SLICE($unknown_holders, 0, 10),
  confidence: 94
}

---

## Q5309: "Based on the vesting schedule, what is the projected inflation rate and increase in circulating supply for the 'Tensor' (TNSR) token over the next 12 months?"

**Expected Plan:**

[TIME: ~30s] [COST: ~0.001 SOL] [CONFIDENCE: 96%]

**Available Tools:**
From Standard Library:
  - SEARCH_DOCS (Web/External)
  - PARSE_VESTING_SCHEDULE (Analysis)
  - PROJECT_SUPPLY_INFLATION (Analysis)

**Main Branch:**
$token_symbol = "TNSR"

// 1. Find the official documentation for the vesting schedule
$docs = SEARCH_DOCS(query: "Tensor TNSR token vesting schedule official")
$vesting_schedule = PARSE_VESTING_SCHEDULE(docs: $docs)

// 2. Project the supply unlocks over the next 12 months
$inflation_projection = PROJECT_SUPPLY_INFLATION(
  schedule: $vesting_schedule,
  time_horizon_months: 12
)

**Decision Point:** Characterize the upcoming inflation.
  BRANCH A ($inflation_projection.next_12m_increase_pct > 100):
    $impact = "High Inflation"
    $summary = "The circulating supply is projected to more than double in the next 12 months, which could create significant sell pressure."
  BRANCH B ($inflation_projection.next_12m_increase_pct > 40):
    $impact = "Moderate Inflation"
    $summary = "A significant amount of new supply will enter circulation over the next year. Key unlock dates may cause volatility."
  BRANCH C (default):
    $impact = "Low Inflation"
    $summary = "Most of the supply is already circulating. Upcoming unlocks are minor and unlikely to have a major market impact."

**Action:**
RETURN {
  inflation_impact: $impact,
  summary: $summary,
  current_circulating_supply: $vesting_schedule.current_supply,
  projected_supply_in_12m: $inflation_projection.final_supply,
  circulating_supply_increase_pct: $inflation_projection.next_12m_increase_pct,
  major_unlock_dates: $inflation_projection.unlock_dates,
  confidence: 96
}

---

## Q5310: "How does the ratio of daily active users to market cap for 'Helium Mobile' (MOBILE) compare to other DePIN projects on Solana?"

**Expected Plan:**

[TIME: ~70s] [COST: ~0.008 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - GET_DAILY_ACTIVE_USERS (Analysis)
  - BIRDEYE_GET_MARKET_CAP (Birdeye API)
  - MAP, SORT_BY (Data Processing)

**Main Branch:**
$target_project = "MOBILE"
$competitors = ["HNT", "RENDER", "NOS"] // Other DePIN projects

// Get DAUs and Market Cap for the target project
$mobile_dau = GET_DAILY_ACTIVE_USERS(project: $target_project)
$mobile_mc = BIRDEYE_GET_MARKET_CAP(token_symbol: $target_project)
$mobile_ratio = $mobile_mc / $mobile_dau

// Get DAUs and Market Cap for competitors
$competitor_ratios = MAP(collection: $competitors, fn: p => {
  project: p,
  dau: GET_DAILY_ACTIVE_USERS(project: p),
  mc: BIRDEYE_GET_MARKET_CAP(token_symbol: p),
  ratio: mc / dau
})

$all_projects = APPEND(array: $competitor_ratios, item: { project: $target_project, ratio: $mobile_ratio })
$sorted_projects = SORT_BY(collection: $all_projects, field: "ratio", direction: "asc") // Lower is better

**Decision Point:** Evaluate MOBILE's valuation relative to its user base.
  BRANCH A (FIRST($sorted_projects).project == $target_project):
    $valuation = "Potentially Undervalued"
    $conclusion = "Helium Mobile has the lowest Market Cap to Daily Active User ratio, suggesting it is valued more cheaply per user compared to its peers."
  BRANCH B (LAST($sorted_projects).project == $target_project):
    $valuation = "Potentially Overvalued"
    $conclusion = "Helium Mobile has the highest Market Cap to Daily Active User ratio, suggesting it carries a premium valuation per user compared to its peers."
  BRANCH C (default):
    $valuation = "Fairly Valued"
    $conclusion = "Helium Mobile's valuation ratio is in the middle of the pack compared to its peers."

**Action:**
RETURN {
  valuation_assessment: $valuation,
  conclusion: $conclusion,
  mobile_mc_per_dau: $mobile_ratio,
  peer_comparison: $sorted_projects,
  confidence: 88
}

---

## Q5311: "Map a 'crypto laundering service' that accepts dirty funds and returns 'clean' crypto for a fee. Track all clients, calculate service volume, and find operator wallets."

**Expected Plan:**

[TIME: ~14m] [COST: ~0.14 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library:
  - DETECT_LAUNDERING_SERVICE (Advanced Forensics)
  - BUILD_SERVICE_CLIENT_MAP (Analysis)

**Main Branch:**
$suspected_service_wallet = "..."

// Detect laundering service pattern
$service_analysis = DETECT_LAUNDERING_SERVICE(
  suspected_wallet: $suspected_service_wallet,
  time_period: "180d"
)

// Build client map
$clients = BUILD_SERVICE_CLIENT_MAP(
  service_wallet: $suspected_service_wallet,
  identify_dirty_sources: true
)

// Calculate service metrics
$total_volume = SUM(MAP($clients, c => c.total_sent_sol))
$estimated_fee = $total_volume * 0.05 // Assume 5% service fee

**Action:**
RETURN {
  investigation: "Crypto Laundering Service",
  service_wallet: $suspected_service_wallet,
  total_clients: COUNT($clients),
  total_volume_sol: $total_volume,
  estimated_fees_sol: $estimated_fee,
  operator_wallets: $service_analysis.operator_cluster,
  confidence: 81
}

---

## Q5312: "Detect 'insider trading' on token launches by identifying wallets that buy large amounts seconds after deployment but before public announcement."

**Expected Plan:**

[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - FIND_STEALTH_LAUNCH_INSIDERS (Forensics)

**Main Branch:**
$token_mint = "..."
$public_announcement_time = 1728432000

$launch_time = getAccountInfo($token_mint).data.created_at

// Find buyers between deployment and announcement
$insider_buyers = FIND_STEALTH_LAUNCH_INSIDERS(
  token: $token_mint,
  launch_time: $launch_time,
  announcement_time: $public_announcement_time,
  min_buy_amount_sol: 10
)

**Action:**
RETURN {
  investigation: "Insider Trading Detection",
  token: $token_mint,
  stealth_period_minutes: ($public_announcement_time - $launch_time) / 60,
  insider_buyers: COUNT($insider_buyers),
  total_insider_volume_sol: SUM(MAP($insider_buyers, i => i.buy_amount_sol)),
  insider_wallets: MAP($insider_buyers, i => i.wallet),
  confidence: 86
}

---

## Q5313: "Identify 'MEV protection failures' by finding transactions that paid for MEV protection but still got sandwiched. Calculate wasted protection fees."

**Expected Plan:**

[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - FIND_FAILED_MEV_PROTECTION (MEV Analysis)

**Main Branch:**
$time_period = "30d"

$failed_protections = FIND_FAILED_MEV_PROTECTION(
  period: $time_period,
  protection_services: ["Jito", "Flashbots", "Eden"]
)

$wasted_fees = SUM(MAP($failed_protections, f => f.protection_fee_sol))

**Action:**
RETURN {
  investigation: "MEV Protection Failures",
  failed_protections: COUNT($failed_protections),
  total_wasted_fees_sol: $wasted_fees,
  most_common_failure_reason: MOST_COMMON(MAP($failed_protections, f => f.failure_reason)),
  confidence: 84
}

---

## Q5314: "Map a 'Ponzi token' with referral rewards. Build the referral tree, calculate each level's profitability, and identify the collapse point."

**Expected Plan:**

[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - BUILD_REFERRAL_TREE (Analysis)
  - CALCULATE_PONZI_COLLAPSE_POINT (Financial Analysis)

**Main Branch:**
$ponzi_token = "..."

$referral_tree = BUILD_REFERRAL_TREE(
  token: $ponzi_token,
  root_wallet: getAccountInfo($ponzi_token).data.authority
)

$collapse_point = CALCULATE_PONZI_COLLAPSE_POINT(
  tree: $referral_tree,
  promised_returns: 10 // 10% monthly
)

**Action:**
RETURN {
  investigation: "Ponzi Token Referral Analysis",
  token: $ponzi_token,
  total_participants: COUNT($referral_tree.all_participants),
  pyramid_levels: COUNT($referral_tree.levels),
  collapse_date: $collapse_point.estimated_collapse_date,
  winners: COUNT(FILTER($referral_tree.all_participants, p => p.net_profit > 0)),
  losers: COUNT(FILTER($referral_tree.all_participants, p => p.net_profit < 0)),
  confidence: 87
}

---

## Q5315: "Detect 'fake volume bots' by analyzing trading patterns: repetitive amounts, perfect timing intervals, circular fund flows between limited wallets."

**Expected Plan:**

[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - DETECT_FAKE_VOLUME_BOTS (Pattern Analysis)

**Main Branch:**
$token_mint = "..."

$fake_volume = DETECT_FAKE_VOLUME_BOTS(
  token: $token_mint,
  detection_methods: ["repetitive_amounts", "timing_regularity", "circular_flows"]
)

$real_volume = GET_TOKEN_VOLUME(token: $token_mint, period: "24h") - $fake_volume.estimated_fake_volume_sol
$fake_percentage = ($fake_volume.estimated_fake_volume_sol / GET_TOKEN_VOLUME(token: $token_mint, period: "24h")) * 100

**Action:**
RETURN {
  investigation: "Fake Volume Bot Detection",
  token: $token_mint,
  fake_volume_bots: COUNT($fake_volume.detected_bots),
  estimated_fake_volume_sol: $fake_volume.estimated_fake_volume_sol,
  estimated_real_volume_sol: $real_volume,
  fake_volume_percentage: $fake_percentage,
  bot_wallets: $fake_volume.bot_wallets,
  confidence: 88
}

---

## Q5316: "Investigate a 'liquidity migration scam' where devs drain LP from one DEX, promise to migrate to another, but never do. Track fund flow."

**Expected Plan:**

[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - TRACE_LIQUIDITY_REMOVAL (Analysis)

**Main Branch:**
$token_mint = "..."
$old_pool = "..." // Raydium pool
$promised_new_pool = "..." // Orca pool (never created)

$lp_removal = TRACE_LIQUIDITY_REMOVAL(
  token: $token_mint,
  pool: $old_pool
)

// Check if new pool was ever created
$new_pool_exists = getAccountInfo($promised_new_pool) != null

// Trace removed funds
$fund_trace = TRACE_FUNDS(
  start: $lp_removal.remover_wallet,
  depth: 5
)

**Action:**
RETURN {
  investigation: "Liquidity Migration Scam",
  token: $token_mint,
  old_pool: $old_pool,
  lp_removed_sol: $lp_removal.total_removed_sol,
  new_pool_created: $new_pool_exists,
  removed_funds_destination: $fund_trace.final_addresses,
  is_confirmed_scam: !$new_pool_exists,
  confidence: 85
}

---

## Q5317: "Map 'connected scammer networks' by finding common funding sources, shared infrastructure wallets, or coordinated launch timing across multiple rugs."

**Expected Plan:**

[TIME: ~13m] [COST: ~0.13 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - BUILD_SCAMMER_NETWORK_MAP (Advanced Forensics)

**Main Branch:**
$known_rugs = [...] // List of known rug pulls

$network_map = BUILD_SCAMMER_NETWORK_MAP(
  scam_projects: $known_rugs,
  link_types: ["funding_source", "infrastructure_wallets", "launch_timing"]
)

**Action:**
RETURN {
  investigation: "Connected Scammer Networks",
  rugs_analyzed: COUNT($known_rugs),
  distinct_networks: COUNT($network_map.detected_networks),
  largest_network_size: MAX(MAP($network_map.detected_networks, n => COUNT(n.projects))),
  network_details: $network_map.detected_networks,
  confidence: 83
}

---

## Q5318: "Detect 'pump and dump discord groups' by correlating coordinated buying with Discord/Telegram message timestamps and member wallets."

**Expected Plan:**

[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library:
  - CORRELATE_SOCIAL_WITH_ONCHAIN (Cross-Platform Analysis)

**Main Branch:**
$suspected_tokens = [...]
$discord_timestamps = [...] // Call timestamps from known pump groups

$correlation = CORRELATE_SOCIAL_WITH_ONCHAIN(
  tokens: $suspected_tokens,
  social_signals: $discord_timestamps,
  correlation_window: 300 // 5 minutes
)

**Action:**
RETURN {
  investigation: "Discord Pump Group Detection",
  tokens_analyzed: COUNT($suspected_tokens),
  strong_correlations: COUNT(FILTER($correlation, c => c.correlation_strength > 0.8)),
  pump_group_members: $correlation.suspected_member_wallets,
  confidence: 79
}

---

## Q5319: "Identify 'token lockup bypass exploits' where teams find ways to access supposedly locked tokens early. Trace the exploit path."

**Expected Plan:**

[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - DETECT_LOCKUP_BYPASS (Security Analysis)

**Main Branch:**
$token_mint = "..."
$lockup_contract = "..."

$bypass_detection = DETECT_LOCKUP_BYPASS(
  token: $token_mint,
  lockup_contract: $lockup_contract
)

**Action:**
RETURN {
  investigation: "Token Lockup Bypass Detection",
  token: $token_mint,
  bypass_detected: $bypass_detection.bypass_found,
  bypass_method: $bypass_detection.exploit_method,
  tokens_unlocked_early: $bypass_detection.early_unlock_amount,
  bypass_transactions: $bypass_detection.exploit_transactions,
  confidence: 86
}

---

## Q5320: "Map 'validator collusion' where multiple validators coordinate to censor specific transactions or prioritize their own MEV extraction."

**Expected Plan:**

[TIME: ~12m] [COST: ~0.12 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - DETECT_VALIDATOR_COLLUSION (Advanced Forensics)

**Main Branch:**
$suspected_validators = [...]
$time_period = {start: 1727222400, end: 1728432000}

$collusion_analysis = DETECT_VALIDATOR_COLLUSION(
  validators: $suspected_validators,
  period: $time_period,
  detection_methods: ["censorship_patterns", "coordinated_mev", "shared_infrastructure"]
)

**Action:**
RETURN {
  investigation: "Validator Collusion Detection",
  validators_analyzed: COUNT($suspected_validators),
  collusion_detected: $collusion_analysis.collusion_found,
  colluding_validators: $collusion_analysis.colluding_group,
  evidence: $collusion_analysis.evidence,
  estimated_unfair_mev_sol: $collusion_analysis.total_coordinated_mev_sol,
  confidence: 82
}


---

