# Token Research - Quantitative & Algorithmic Trading (Q701-Q800)

**Difficulty:** Expert
**Focus:** Crypto-native statistical arbitrage, quantitative risk modeling for DeFi, algorithmic on-chain trading, ML-based prediction for crypto assets.
**Questions:** Q701-Q800

---

## Q701: "How can you build a statistical arbitrage model to exploit pricing inefficiencies between a token's spot price on Raydium and its perpetual futures price on a decentralized derivatives exchange like Zeta Markets?"
**Expected Plan:**

[TIME: ~3m] [COST: ~0.02 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - GET_PRICE_HISTORY (Oracle/DEX Tool)
  - COINTEGRATION_TEST, SPREAD_TRADING_MODEL (Statistical Modeling)
  - MAP, FILTER, CORRELATE (Data Processing)

**Main Branch:**
$spot_market = "Raydium"
$spot_pair = "WIF-USDC"
$perp_market = "Zeta"
$perp_pair = "WIF-PERP"

// Step 1: Get synchronized price history for both markets
$spot_prices = GET_PRICE_HISTORY(market: $spot_market, pair: $spot_pair, period: "7d", interval: "1m")
$perp_prices = GET_PRICE_HISTORY(market: $perp_market, pair: $perp_pair, period: "7d", interval: "1m")

// Step 2: Test for cointegration
// If the two price series are cointegrated, they have a long-term statistical relationship.
$coint_test = COINTEGRATION_TEST(series1: $spot_prices, series2: $perp_prices)

GUARD $coint_test.is_cointegrated == true ELSE
  RETURN ERROR(message: "Spot and perp prices are not cointegrated. Statistical arbitrage is not viable.")

// Step 3: Model the spread
$spread = MAP($spot_prices, (price, index) => price - $perp_prices[index])
$spread_mean = MEAN($spread)
$spread_stddev = STDDEV($spread)

// Step 4: Generate trading signals based on spread deviation
// Trade when the spread deviates significantly from its historical mean.
$z_score_threshold = 2.0
$signals = MAP($spread, s => {
  $z_score = (s - $spread_mean) / $spread_stddev
  IF $z_score > $z_score_threshold: RETURN "SHORT_SPREAD" // Sell spot, buy perp
  IF $z_score < -$z_score_threshold: RETURN "LONG_SPREAD"  // Buy spot, sell perp
  RETURN "HOLD"
})

// Step 5: Backtest the strategy
$backtest_results = SPREAD_TRADING_MODEL(
  signals: $signals,
  spot_prices: $spot_prices,
  perp_prices: $perp_prices,
  transaction_cost: 0.001 // 0.1%
)

**Action:**
RETURN {
  analysis: "Statistical Arbitrage: Spot vs. Perp",
  spot_market: $spot_market,
  perp_market: $perp_market,
  cointegration_test_p_value: $coint_test.p_value,
  spread_mean: $spread_mean,
  spread_std_dev: $spread_stddev,
  backtest_results: $backtest_results,
  confidence: 85,
  caveats: ["Past performance is not indicative of future results. Slippage and fees can significantly impact profitability."]
}

---

## Q702: "How can you use quantitative risk metrics (e.g., Value-at-Risk, Conditional Value-at-Risk) to quantify the tail risk of holding a highly volatile memecoin like WIF in a diversified portfolio of blue-chip assets like SOL and JUP?"

## Q711: "What was the historical correlation between the price of SOL and the price of major Ethereum L2 tokens (e.g., ARB, OP) during the last market cycle?"

**Expected Plan:**
[TIME: ~45s] [COST: ~0.004 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: GET_PRICE_HISTORY, CORRELATE

**Main Branch:**
$sol_prices = GET_PRICE_HISTORY(token: "SOL", period: "2y", interval: "1d")
$arb_prices = GET_PRICE_HISTORY(token: "ARB", period: "2y", interval: "1d")
$op_prices = GET_PRICE_HISTORY(token: "OP", period: "2y", interval: "1d")

// Calculate correlation coefficients
$sol_arb_correlation = CORRELATE(data1: $sol_prices, data2: $arb_prices)
$sol_op_correlation = CORRELATE(data1: $sol_prices, data2: $op_prices)

**Decision Point:** Interpret the correlation strength.
  BRANCH A ($sol_arb_correlation.coefficient > 0.7):
    $summary = "SOL and ARB have shown a strong positive correlation, suggesting they are often influenced by similar market-wide factors."
  BRANCH B ($sol_arb_correlation.coefficient < 0.3):
    $summary = "SOL and ARB have shown a weak correlation, indicating that their price movements have been largely independent."
  BRANCH C (default):
    $summary = "SOL and ARB have a moderate positive correlation."

**Action:**
RETURN {
  analysis_period: "Last 2 years",
  correlation_summary: $summary,
  sol_vs_arb_correlation: $sol_arb_correlation.coefficient,
  sol_vs_op_correlation: $sol_op_correlation.coefficient,
  confidence: 95
}

---

## Q712: "Analyze the historical gas fees (priority fees) on Solana during the 2021 bull run. What was the average and peak fee paid for a standard DEX swap?"

**Expected Plan:**
[TIME: ~60s] [COST: ~0.01 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: GET_HISTORICAL_TRANSACTIONS, FILTER, MEAN, MAX

**Main Branch:**
$start_date = "2021-01-01"
$end_date = "2021-12-31"

// Get a sample of DEX swap transactions from the period
$swap_txs = GET_HISTORICAL_TRANSACTIONS(
  start_date: $start_date,
  end_date: $end_date,
  tx_type: "DEX_SWAP",
  sample_size: 10000
)

// Extract priority fees
$priority_fees = MAP(collection: $swap_txs, fn: tx => tx.priority_fee)

$average_fee_lamports = MEAN($priority_fees)
$peak_fee_lamports = MAX($priority_fees)

// Convert to SOL and estimate USD value at the time
$avg_sol_price_2021 = 100 // Approximate average
$average_fee_usd = ($average_fee_lamports / 1e9) * $avg_sol_price_2021
$peak_fee_usd = ($peak_fee_lamports / 1e9) * $avg_sol_price_2021

**Action:**
RETURN {
  period: "2021 Bull Run",
  average_priority_fee_usd: $average_fee_usd,
  peak_priority_fee_usd: $peak_fee_usd,
  average_fee_lamports: $average_fee_lamports,
  peak_fee_lamports: $peak_fee_lamports,
  confidence: 90
}

---

## Q713: "What was the average daily active user count for the top 5 Solana NFT marketplaces during the 'DeGods' mint event?"

**Expected Plan:**
[TIME: ~50s] [COST: ~0.006 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library: GET_HISTORICAL_DAU, SEARCH_WEB

**Main Branch:**
$degods_mint_date = SEARCH_WEB(query: "DeGods NFT mint date")
$marketplaces = ["Magic Eden", "OpenSea", "Solanart", "Tensor", "HadeSwap"]

// Get DAU for each marketplace on the mint date
$dau_data = MAP(collection: $marketplaces, fn: mp => {
  $dau = GET_HISTORICAL_DAU(platform: mp, date: $degods_mint_date)
  RETURN { marketplace: mp, dau: $dau }
})

$total_dau = SUM(MAP($dau_data, d => d.dau))

**Action:**
RETURN {
  event: "DeGods Mint",
  date: $degods_mint_date,
  total_dau_across_top_5_marketplaces: $total_dau,
  dau_breakdown: $dau_data,
  confidence: 88
}

---

## Q714: "Trace the flow of funds from the original 'Solana-Wormhole' hack in 2022. How much of the stolen ETH was eventually recovered, and where did the unrecovered funds go?"

**Expected Plan:**
[TIME: ~90s] [COST: ~0.02 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library: TRACE_FUNDS, SEARCH_WEB

**Main Branch:**
$hacker_address_eth = "0x629e7da20197a5429d30da36e77d06cdf796b71a" // Known hacker address
$hack_details = SEARCH_WEB(query: "Solana Wormhole hack 2022 details")

// Trace funds on the Ethereum side
$eth_trace = TRACE_FUNDS(start_address: $hacker_address_eth, blockchain: "ethereum", depth: 10)

$recovered_amount = $hack_details.recovered_amount // From web search
$unrecovered_funds_trace = FILTER($eth_trace.paths, path => !path.leads_to_recovery_address)

**Action:**
RETURN {
  hack: "Solana-Wormhole 2022",
  total_stolen_eth: 120000,
  amount_recovered_eth: $recovered_amount,
  unrecovered_funds_destinations: MAP($unrecovered_funds_trace, p => p.final_summary),
  confidence: 92
}

---

## Q715: "Analyze the historical performance of staking SOL with Marinade Finance vs. Lido Finance since their inception. Which protocol offered a better APY?"

**Expected Plan:**
[TIME: ~40s] [COST: ~0.004 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: GET_HISTORICAL_APY

**Main Branch:**
$marinade_apy_history = GET_HISTORICAL_APY(protocol: "Marinade", period: "all_time")
$lido_apy_history = GET_HISTORICAL_APY(protocol: "Lido", period: "all_time")

$avg_marinade_apy = MEAN(MAP($marinade_apy_history, h => h.apy))
$avg_lido_apy = MEAN(MAP($lido_apy_history, h => h.apy))

**Decision Point:** Determine the better performer.
  BRANCH A ($avg_marinade_apy > $avg_lido_apy):
    $winner = "Marinade Finance"
  BRANCH B (default):
    $winner = "Lido Finance"

**Action:**
RETURN {
  analysis: "Historical Staking APY Comparison",
  winner: $winner,
  average_marinade_apy: $avg_marinade_apy,
  average_lido_apy: $avg_lido_apy,
  confidence: 90
}

---

## Q716: "What was the market cap of the top 3 Solana DEXs (Serum, Raydium, Orca) at the peak of the 2021 bull market?"

**Expected Plan:**
[TIME: ~30s] [COST: ~0.003 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: GET_HISTORICAL_MARKET_CAP

**Main Branch:**
$peak_date = "2021-11-09" // Approximate peak date
$dex_tokens = ["SRM", "RAY", "ORCA"]

$peak_market_caps = MAP(collection: $dex_tokens, fn: token => {
  $mc = GET_HISTORICAL_MARKET_CAP(token: token, date: $peak_date)
  RETURN { token: token, market_cap_usd: $mc }
})

**Action:**
RETURN {
  event: "2021 Bull Market Peak",
  date: $peak_date,
  dex_market_caps: $peak_market_caps,
  confidence: 95
}

---

## Q717: "How did the launch of the 'STEPN' (GMT) token affect the floor price of their sneaker NFTs in the first month?"

**Expected Plan:**
[TIME: ~50s] [COST: ~0.005 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: GET_NFT_FLOOR_PRICE_HISTORY, GET_PRICE_HISTORY, CORRELATE

**Main Branch:**
$gmt_launch_date = "2022-03-09"
$sneaker_collection = "STEPN-Sneakers"

// Get NFT floor price and GMT price history for the first month
$floor_history = GET_NFT_FLOOR_PRICE_HISTORY(collection: $sneaker_collection, start_date: $gmt_launch_date, end_date: "2022-04-09")
$gmt_price_history = GET_PRICE_HISTORY(token: "GMT", start_date: $gmt_launch_date, end_date: "2022-04-09")

$correlation = CORRELATE(data1: $floor_history, data2: $gmt_price_history)

**Action:**
RETURN {
  analysis: "STEPN GMT Launch Impact",
  correlation_gmt_price_vs_nft_floor: $correlation.coefficient,
  summary: "The analysis shows a correlation of " + $correlation.coefficient + " between the GMT token price and the sneaker NFT floor price in the first month after launch.",
  confidence: 91
}

---

## Q718: "Identify the wallets that held the 'Saber' (SBR) token from its launch and through the Cashio exploit. Did they sell off before, during, or after the crisis?"

**Expected Plan:**
[TIME: ~75s] [COST: ~0.015 SOL] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library: GET_TOKEN_HOLDERS_AT_DATE, GET_WALLET_TRANSACTION_HISTORY

**Main Branch:**
$sbr_launch_date = "2021-06-01"
$cashio_exploit_date = "2022-03-23"
$sbr_mint = "Saber2gLauYim4Mvftnrasomsv6NvAuncvMEZwcLpD1"

// Get wallets that held SBR at launch
$og_holders = GET_TOKEN_HOLDERS_AT_DATE(token: $sbr_mint, date: $sbr_launch_date)

// Analyze the behavior of these OG holders around the exploit
$holder_behavior = MAP(collection: $og_holders, fn: holder => {
  $tx_history = GET_WALLET_TRANSACTION_HISTORY(wallet: holder, token: $sbr_mint, start_date: "2022-03-20", end_date: "2022-03-26")
  $sell_before = COUNT(FILTER($tx_history, tx => tx.date < $cashio_exploit_date && tx.type == "sell")) > 0
  $sell_during = COUNT(FILTER($tx_history, tx => tx.date == $cashio_exploit_date && tx.type == "sell")) > 0
  $sell_after = COUNT(FILTER($tx_history, tx => tx.date > $cashio_exploit_date && tx.type == "sell")) > 0
  RETURN { wallet: holder, sell_before: $sell_before, sell_during: $sell_during, sell_after: $sell_after }
})

**Action:**
RETURN {
  analysis: "Saber OG Holder Behavior during Cashio Exploit",
  sold_before_crisis: COUNT(FILTER($holder_behavior, h => h.sell_before)),
  sold_during_crisis: COUNT(FILTER($holder_behavior, h => h.sell_during)),
  sold_after_crisis: COUNT(FILTER($holder_behavior, h => h.sell_after)),
  held_through: COUNT(FILTER($holder_behavior, h => !h.sell_before && !h.sell_during && !h.sell_after)),
  confidence: 89
}

---

## Q719: "What was the historical ratio of SOL staked vs. total SOL supply in early 2022 versus today?"

**Expected Plan:**
[TIME: ~30s] [COST: ~0.003 SOL] [CONFIDENCE: 96%]

**Available Tools:**
From Standard Library: GET_HISTORICAL_STAKE_STATS

**Main Branch:**
$date_early_2022 = "2022-01-15"
$date_today = NOW()

// Get historical staking data
$stats_2022 = GET_HISTORICAL_STAKE_STATS(date: $date_early_2022)
$stats_today = GET_HISTORICAL_STAKE_STATS(date: $date_today)

$ratio_2022 = ($stats_2022.staked_sol / $stats_2022.total_supply) * 100
$ratio_today = ($stats_today.staked_sol / $stats_today.total_supply) * 100

**Action:**
RETURN {
  staked_ratio_early_2022: $ratio_2022,
  staked_ratio_today: $ratio_today,
  change_in_staked_ratio: $ratio_today - $ratio_2022,
  confidence: 96
}

---

## Q720: "Analyze the trading volume and price action of the 'COPE' token during the 2021 Solana network outage. How did the market react once the network came back online?"

**Expected Plan:**
[TIME: ~50s] [COST: ~0.005 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: GET_PRICE_HISTORY, GET_TOKEN_TRADES_HISTORY, SEARCH_WEB

**Main Branch:**
$outage_date = SEARCH_WEB(query: "Solana outage date September 2021") // e.g., "2021-09-14"
$cope_mint = "8HGyAAB1yoM1ttS7pXjHMa3dukTFGQggnFFH3hJZgzQh"

// Get price and volume data around the outage
$price_before = GET_PRICE_HISTORY(token: $cope_mint, date: $outage_date - "1d")
$price_after = GET_PRICE_HISTORY(token: $cope_mint, date: $outage_date + "1d")
$volume_after = GET_TOKEN_TRADES_HISTORY(token: $cope_mint, start_date: $outage_date + "1d", end_date: $outage_date + "2d")

$price_change = (($price_after - $price_before) / $price_before) * 100
$volume_spike = SUM(MAP($volume_after, t => t.volume_usd))

**Action:**
RETURN {
  event: "Solana Network Outage September 2021",
  token: "COPE",
  price_change_post_recovery_pct: $price_change,
  trading_volume_first_24h_post_recovery_usd: $volume_spike,
  summary: "Following the network restart, COPE's price changed by " + $price_change + "% and saw a surge in volume as queued trades executed.",
  confidence: 90
}

**Expected Plan:**

[TIME: ~2m] [COST: ~0.01 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - GET_PRICE_HISTORY (Oracle Tool)
  - VALUE_AT_RISK_CALCULATE, CONDITIONAL_VALUE_AT_RISK (Risk Modeling)
  - MAP, FILTER (Data Processing)

**Main Branch:**
$portfolio_assets = [
  { token: "WIF", allocation: 0.10 }, // 10% in memecoin
  { token: "SOL", allocation: 0.50 }, // 50% in SOL
  { token: "JUP", allocation: 0.40 }  // 40% in JUP
]
$historical_period = "90d"

// Step 1: Get historical returns for all assets
$asset_returns = PARALLEL MAP($portfolio_assets, asset => {
  $prices = GET_PRICE_HISTORY(token: asset.token, period: $historical_period, interval: "1d")
  $daily_returns = calculateDailyReturns($prices)
  RETURN { token: asset.token, returns: $daily_returns }
})
WAIT_ALL

// Step 2: Calculate portfolio historical returns
$portfolio_returns = calculatePortfolioReturns(
  asset_returns: $asset_returns,
  allocations: $portfolio_assets
)

// Step 3: Calculate VaR and CVaR
// VaR: What is the most I can expect to lose with 99% confidence?
$value_at_risk_99 = VALUE_AT_RISK_CALCULATE(
  returns: $portfolio_returns,
  confidence_level: 99
)

// CVaR: If I do have a bad day (beyond VaR), what is my average expected loss?
$conditional_value_at_risk_99 = CONDITIONAL_VALUE_AT_RISK(
  returns: $portfolio_returns,
  confidence_level: 99
)

// Step 4: Compare with a portfolio without the memecoin
$safe_portfolio_returns = calculatePortfolioReturns(
  asset_returns: FILTER($asset_returns, a => a.token != "WIF"),
  allocations: [{ token: "SOL", allocation: 0.60 }, { token: "JUP", allocation: 0.40 }]
)
$safe_cvar = CONDITIONAL_VALUE_AT_RISK($safe_portfolio_returns, 99)

**Action:**
RETURN {
  analysis: "Portfolio Tail Risk with Memecoin",
  portfolio_composition: $portfolio_assets,
  historical_period: $historical_period,
  ninety_nine_percent_daily_var: $value_at_risk_99,
  ninety_nine_percent_daily_cvar: $conditional_value_at_risk_99,
  cvar_increase_due_to_memecoin: $conditional_value_at_risk_99 - $safe_cvar,
  confidence: 90,
  note: "CVaR is a crucial metric for understanding the risk of extreme, 'black swan' losses, which is particularly relevant when including memecoins in a portfolio."
}

---

## Q703: "How can you design and backtest a TWAP (Time-Weighted Average Price) algorithmic trading strategy to accumulate a $1M position in a low-liquidity token on Orca over 24 hours, while minimizing market impact and slippage?"

**Expected Plan:**

[TIME: ~4m] [COST: ~0.03 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - GET_PRICE_HISTORY, GET_LIQUIDITY_DEPTH (DEX Tools)
  - BACKTEST_ALGO_STRATEGY (Trading Modeling)
  - MAP, FILTER (Data Processing)

**Main Branch:**
$target_token = "7i5KKsX2weiTkry7jA4sDxaBCgfxhT3PvaZos21iCsUN" // Example low-liquidity
$total_usd_to_invest = 1000000
$execution_duration_hours = 24
$trade_interval_minutes = 5

// Step 1: Get historical price and liquidity data
$price_history = GET_PRICE_HISTORY(token: $target_token, period: "7d", interval: "1m")
$liquidity_history = GET_LIQUIDITY_DEPTH(token: $target_token, period: "7d", interval: "1m")

// Step 2: Define the TWAP strategy logic
$num_trades = ($execution_duration_hours * 60) / $trade_interval_minutes
$usd_per_trade = $total_usd_to_invest / $num_trades

$strategy_logic = {
  type: "TWAP",
  usd_per_trade: $usd_per_trade,
  interval_minutes: $trade_interval_minutes,
  // Add a rule to pause if price impact is too high
  max_price_impact: 0.01 // 1%
}

// Step 3: Backtest the strategy against historical data
$backtest_results = BACKTEST_ALGO_STRATEGY(
  strategy: $strategy_logic,
  price_history: $price_history,
  liquidity_history: $liquidity_history
)

// Step 4: Compare with a single large trade
$single_trade_impact = calculatePriceImpact(
  trade_size_usd: $total_usd_to_invest,
  liquidity: $liquidity_history[0]
)

**Action:**
RETURN {
  analysis: "TWAP Execution Strategy Backtest",
  target_token: $target_token,
  total_investment_usd: $total_usd_to_invest,
  twap_strategy_results: {
    average_execution_price: $backtest_results.avg_price,
    total_slippage_paid_usd: $backtest_results.total_slippage,
    benchmark_price: $backtest_results.benchmark_price,
    performance_vs_benchmark: $backtest_results.avg_price - $backtest_results.benchmark_price
  },
  estimated_impact_of_single_trade: $single_trade_impact,
  confidence: 88
}

---

## Q704: "Using a portfolio optimization framework like Mean-Variance Optimization, what is the 'efficient frontier' for a portfolio consisting of SOL, JTO (LST), and BONK (memecoin), and what is the optimal allocation for maximizing the Sharpe Ratio?"

**Expected Plan:**

[TIME: ~2m] [COST: ~0.01 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - GET_PRICE_HISTORY (Oracle Tool)
  - EFFICIENT_FRONTIER_CALCULATE (Portfolio Modeling)
  - MAP, FILTER (Data Processing)

**Main Branch:**
$assets = ["SOL", "JTO", "BONK"]
$historical_period = "180d"

// Step 1: Get historical daily returns for all assets
$returns_data = PARALLEL MAP($assets, asset => {
  $prices = GET_PRICE_HISTORY(token: asset, period: $historical_period, interval: "1d")
  RETURN calculateDailyReturns($prices)
})
WAIT_ALL

// Step 2: Calculate the covariance matrix of the assets
$covariance_matrix = calculateCovarianceMatrix($returns_data)
$expected_returns = MAP($returns_data, r => MEAN(r))

// Step 3: Calculate the efficient frontier
// This generates a series of optimal portfolios for different levels of risk.
$efficient_frontier = EFFICIENT_FRONTIER_CALCULATE(
  expected_returns: $expected_returns,
  covariance_matrix: $covariance_matrix,
  num_portfolios: 100
)

// Step 4: Find the portfolio with the maximum Sharpe Ratio
// Sharpe Ratio = (Portfolio Return - Risk-Free Rate) / Portfolio Volatility
$risk_free_rate = 0.05 / 365 // Annualized 5% risk-free rate
$max_sharpe_portfolio = MAX_BY(
  collection: $efficient_frontier.portfolios,
  fn: p => (p.return - $risk_free_rate) / p.volatility
)

**Action:**
RETURN {
  analysis: "Efficient Frontier & Max Sharpe Portfolio",
  assets: $assets,
  efficient_frontier_data: $efficient_frontier,
  optimal_portfolio_max_sharpe: {
    allocation: $max_sharpe_portfolio.weights,
    expected_return: $max_sharpe_portfolio.return,
    expected_volatility: $max_sharpe_portfolio.volatility,
    sharpe_ratio: ($max_sharpe_portfolio.return - $risk_free_rate) / $max_sharpe_portfolio.volatility
  },
  confidence: 92,
  note: "The efficient frontier shows the best possible return for a given level of risk. The max Sharpe portfolio is often considered the 'optimal' portfolio."
}

---

## Q705: "How can you train a machine learning model (e.g., Gradient Boosting) to forecast the probability of a >30% price drop for a given token in the next 24 hours, using on-chain metrics like transaction velocity, whale movements, and DEX liquidity changes as features?"

**Expected Plan:**

[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 75%]

**Available Tools:**
From Standard Library:
  - getBlock, getTransaction (Solana RPC)
  - TRAIN_CLASSIFICATION_MODEL, PREDICT_PROBABILITY (Machine Learning)
  - MAP, FILTER (Data Processing)

**Main Branch:**
$target_token = "WIF"
$historical_days = 180

// Step 1: Generate a labeled dataset from historical data
$dataset = []
FOR $i IN 0..$historical_days:
  $current_day_start = NOW() - ($i * 86400)
  // Features from the current day
  $features = {
    tx_velocity: getTransactionVelocity($target_token, $current_day_start),
    whale_inflow_usd: getWhaleInflow($target_token, $current_day_start),
    dex_liquidity_change: getDexLiquidityChange($target_token, $current_day_start),
    social_sentiment: getSocialSentiment($target_token, $current_day_start)
  }
  // Label: Did a >30% drop occur in the *next* 24 hours?
  $label = didPriceDropOccur(
    token: $target_token,
    start_time: $current_day_start + 86400,
    threshold: 0.30
  )
  $dataset = APPEND(array: $dataset, item: { features: $features, label: $label })

// Step 2: Train the classification model
$model = TRAIN_CLASSIFICATION_MODEL(
  data: $dataset,
  algorithm: "GradientBoosting",
  test_split: 0.2
)

// Step 3: Get current features to make a new prediction
$current_features = {
  tx_velocity: getTransactionVelocity($target_token, NOW()),
  whale_inflow_usd: getWhaleInflow($target_token, NOW()),
  dex_liquidity_change: getDexLiquidityChange($target_token, NOW()),
  social_sentiment: getSocialSentiment($target_token, NOW())
}

// Step 4: Predict the probability for the next 24 hours
$prediction = PREDICT_PROBABILITY(
  model: $model.id,
  features: $current_features
)

**Action:**
RETURN {
  analysis: "ML Price Drop Prediction",
  target_token: $target_token,
  model_performance: {
    accuracy: $model.accuracy,
    precision: $model.precision,
    recall: $model.recall,
    feature_importance: $model.feature_importance
  },
  prediction_for_next_24h: {
    probability_of_30_percent_drop: $prediction.probability_class_1,
  },
  confidence: 75,
  caveats: ["ML models are probabilistic and not guaranteed to be correct. Performance depends heavily on data quality and market conditions."]
}

---

## Q706: "How can you use quantitative momentum analysis to identify if a token is in a 'strong trend' or 'mean-reverting' regime, and backtest a strategy that switches between trend-following and mean-reversion based on this classification?"

**Expected Plan:**

[TIME: ~5m] [COST: ~0.04 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - GET_PRICE_HISTORY (Oracle Tool)
  - HURST_EXPONENT_CALCULATE, BACKTEST_ALGO_STRATEGY (Quantitative Modeling)
  - MAP, FILTER (Data Processing)

**Main Branch:**
$target_token = "JTO"
$historical_period = "180d"

// Step 1: Get historical price data
$prices = GET_PRICE_HISTORY(token: $target_token, period: $historical_period, interval: "1h")

// Step 2: Calculate the rolling Hurst Exponent
// The Hurst Exponent (H) helps classify the time series.
// H > 0.5 -> Trending (persistent)
// H < 0.5 -> Mean-reverting (anti-persistent)
// H = 0.5 -> Random Walk
$rolling_hurst = HURST_EXPONENT_CALCULATE(data: $prices, window: 100) // 100-hour rolling window

// Step 3: Define the regime-switching strategy logic
$strategy_logic = {
  type: "RegimeSwitching",
  // Define the two sub-strategies
  trending_strategy: { type: "MovingAverageCrossover", fast: 10, slow: 50 },
  mean_reverting_strategy: { type: "BollingerBands", window: 20, std_dev: 2 },
  // Define the switching condition
  switch_condition: (hurst_value) => {
    IF hurst_value > 0.6: RETURN "trending_strategy"
    IF hurst_value < 0.4: RETURN "mean_reverting_strategy"
    RETURN "HOLD" // No clear regime
  }
}

// Step 4: Backtest the strategy
$backtest_results = BACKTEST_ALGO_STRATEGY(
  strategy: $strategy_logic,
  price_history: $prices,
  supplemental_data: { hurst: $rolling_hurst }
)

**Action:**
RETURN {
  analysis: "Regime-Switching Momentum Strategy",
  target_token: $target_token,
  current_hurst_exponent: LAST($rolling_hurst),
  current_regime: $strategy_logic.switch_condition(LAST($rolling_hurst)),
  backtest_results: {
    total_return: $backtest_results.total_return,
    sharpe_ratio: $backtest_results.sharpe_ratio,
    max_drawdown: $backtest_results.max_drawdown
  },
  confidence: 80
}

---

## Q707: "How can you use statistical factor analysis (e.g., Principal Component Analysis) to decompose a token's returns into underlying drivers, such as 'market beta', 'DeFi sentiment', and 'memecoin hype'?"

**Expected Plan:**

[TIME: ~6m] [COST: ~0.05 SOL] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library:
  - GET_PRICE_HISTORY (Oracle Tool)
  - PRINCIPAL_COMPONENT_ANALYZE (Statistical Modeling)
  - MAP, FILTER (Data Processing)

**Main Branch:**
$target_token = "WIF"
$historical_period = "180d"

// Step 1: Define the factors and get their historical data
$factors = {
  market_beta: getMarketIndexReturns("SOL"), // e.g., SOL as market proxy
  defi_sentiment: getDefiSentimentIndex(), // A custom index of top DeFi tokens
  memecoin_hype: getMemeIndexReturns() // A custom index of top memecoins
}
$token_returns = calculateDailyReturns(GET_PRICE_HISTORY(token: $target_token, period: $historical_period))

// Step 2: Perform a multiple linear regression
// Regress the token's returns against the factor returns.
$regression_model = LINEAR_REGRESSION(
  y: $token_returns,
  x_vars: [$factors.market_beta, $factors.defi_sentiment, $factors.memecoin_hype]
)

// The coefficients (betas) show how sensitive the token is to each factor.
$factor_loadings = $regression_model.coefficients

// Step 3: Use PCA for a more abstract, data-driven approach
$all_returns = [$token_returns, $factors.market_beta, $factors.defi_sentiment, $factors.memecoin_hype]
$pca_results = PRINCIPAL_COMPONENT_ANALYZE(data: $all_returns)

// Analyze the principal components to understand the main drivers of variance in the system.
$pc1_interpretation = interpretPrincipalComponent($pca_results.components[0])

**Action:**
RETURN {
  analysis: "Factor Analysis of Token Returns",
  target_token: $target_token,
  linear_regression_results: {
    market_beta_loading: $factor_loadings[0],
    defi_sentiment_loading: $factor_loadings[1],
    memecoin_hype_loading: $factor_loadings[2],
    r_squared: $regression_model.r_squared // How much of the variance is explained by these factors
  },
  principal_component_analysis: {
    explained_variance_ratio: $pca_results.explained_variance_ratio,
    pc1_interpretation: $pc1_interpretation
  },
  confidence: 78
}

---

## Q708: "How can you design an 'alpha decay' model to determine how quickly the predictive power of a new on-chain signal (e.g., a large whale transfer) fades, and what is the optimal trading horizon to act on it?"

**Expected Plan:**

[TIME: ~5m] [COST: ~0.04 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - GET_PRICE_HISTORY (Oracle Tool)
  - FIND_ONCHAIN_EVENTS (On-chain Indexer)
  - MAP, FILTER, CORRELATE (Data Processing)

**Main Branch:**
$target_token = "BONK"
$event_type = "LargeWhaleTransferToCEX"
$max_horizon_hours = 48

// Step 1: Find all historical instances of the on-chain event
$events = FIND_ONCHAIN_EVENTS(
  token: $target_token,
  event_type: $event_type,
  period: "365d"
)

// Step 2: For each event, calculate the token's price return over different future time horizons
$all_horizon_returns = []
FOR $event IN $events:
  $event_time = $event.timestamp
  $price_at_event = GET_PRICE_HISTORY(token: $target_token, time: $event_time)
  FOR $h IN 1..$max_horizon_hours:
    $price_at_horizon = GET_PRICE_HISTORY(token: $target_token, time: $event_time + ($h * 3600))
    $return = ($price_at_horizon - $price_at_event) / $price_at_event
    $all_horizon_returns = APPEND(
      array: $all_horizon_returns,
      item: { horizon_hours: $h, return: $return }
    )

// Step 3: Calculate the average return and its statistical significance for each horizon
$alpha_decay_analysis = GROUP_BY(collection: $all_horizon_returns, key: "horizon_hours")
$decay_curve = MAP(collection: $alpha_decay_analysis, group => {
  $returns = MAP(group.values, v => v.return)
  $t_test = T_TEST(data: $returns) // Test if the mean return is significantly different from zero
  RETURN {
    horizon_hours: group.key,
    average_return: MEAN($returns),
    p_value: $t_test.p_value
  }
})

// Step 4: Find the optimal horizon (e.g., where the t-statistic is maximized)
$optimal_horizon = MAX_BY(
  collection: $decay_curve,
  fn: item => ABS(item.average_return) / (STDDEV(MAP($alpha_decay_analysis[item.key].values, v => v.return)) / SQRT(COUNT($alpha_decay_analysis[item.key].values)))
)

**Action:**
RETURN {
  analysis: "Alpha Decay Modeling",
  onchain_signal: $event_type,
  target_token: $target_token,
  alpha_decay_curve: $decay_curve,
  optimal_trading_horizon_hours: $optimal_horizon.horizon_hours,
  return_at_optimal_horizon: $optimal_horizon.average_return,
  confidence: 82,
  note: "The alpha decay curve shows how the predictive power of a signal diminishes over time. The optimal horizon is the point where the signal's predictive power is strongest."
}

---

## Q709: "How can you use a GARCH (Generalized Autoregressive Conditional Heteroskedasticity) model to forecast the 24-hour volatility of SOL, and how does this forecast change in response to major market news (e.g., a Fed announcement)?"

**Expected Plan:**

[TIME: ~3m] [COST: ~0.02 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - GET_PRICE_HISTORY (Oracle Tool)
  - FIT_GARCH_MODEL, FORECAST_VOLATILITY (Volatility Modeling)
  - MAP, FILTER (Data Processing)

**Main Branch:**
$target_token = "SOL"
$historical_period = "180d"

// Step 1: Get historical daily returns
$prices = GET_PRICE_HISTORY(token: $target_token, period: $historical_period, interval: "1d")
$returns = calculateDailyReturns($prices)

// Step 2: Fit a GARCH(1,1) model to the returns data
// This model assumes today's volatility depends on yesterday's volatility and yesterday's price shock.
$garch_model = FIT_GARCH_MODEL(
  returns: $returns,
  p: 1, // AR order
  q: 1  // MA order
)

// Step 3: Forecast the next 24 hours' volatility
$volatility_forecast = FORECAST_VOLATILITY(
  model: $garch_model,
  steps: 1 // Forecast 1 day ahead
)

// Step 4: Simulate the impact of a market shock
$market_shock_event = "Fed rate hike announcement"
// Find the last time this happened and measure the price change
$last_shock_return = findReturnOnEventDate($target_token, $market_shock_event)

// Re-run the forecast, but inject the shock as the most recent "price shock"
$volatility_forecast_post_shock = FORECAST_VOLATILITY(
  model: $garch_model,
  steps: 1,
  last_shock: $last_shock_return
)

**Action:**
RETURN {
  analysis: "GARCH Volatility Forecasting",
  target_token: $target_token,
  garch_model_parameters: $garch_model.parameters,
  forecasted_24h_volatility_annualized: $volatility_forecast.volatility,
  market_shock_scenario: $market_shock_event,
  forecasted_24h_volatility_post_shock: $volatility_forecast_post_shock.volatility,
  confidence: 85,
  note: "GARCH models are standard in traditional finance for modeling volatility clustering (periods of high volatility followed by high volatility). This shows how expected volatility can change based on news."
}

---

## Q710: "How can you analyze high-frequency order book data from a DEX to identify 'iceberg' orders for a specific token, where a large order is broken into many small, visible chunks to hide its true size?"

**Expected Plan:**

[TIME: ~7m] [COST: ~0.06 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - GET_ORDER_BOOK_UPDATES (HFT Data Tool)
  - MAP, FILTER, GROUP_BY (Data Processing)

**Main Branch:**
$dex_market_id = "WIF/USDC"
$analysis_duration_minutes = 60

// Step 1: Stream high-frequency order book updates
$order_book_stream = GET_ORDER_BOOK_UPDATES(
  market: $dex_market_id,
  duration: $analysis_duration_minutes
)

// Step 2: Analyze the stream to detect iceberg patterns
// Pattern: A small order at a specific price level gets filled, and is immediately replaced by another small order at the exact same price level.
$refreshed_orders = {}
FOR $update IN $order_book_stream:
  IF $update.type == "fill":
    $filled_order = $update.order
    // Look for a new order placed immediately after the fill at the same price
    $next_update = PEEK($order_book_stream)
    IF $next_update.type == "new_order" AND $next_update.order.price == $filled_order.price AND $next_update.order.size == $filled_order.size:
      $key = $filled_order.price + "_" + $filled_order.side
      $refreshed_orders[$key] = ($refreshed_orders[$key] || 0) + 1
      $refreshed_orders[$key + "_total_size"] = ($refreshed_orders[$key + "_total_size"] || 0) + $filled_order.size

// Step 3: Identify likely iceberg orders
$iceberg_orders = []
FOR $key, $count IN $refreshed_orders:
  IF $count > 5: // If an order at the same price/size is refreshed more than 5 times
    $total_size = $refreshed_orders[$key + "_total_size"]
    $price = SPLIT($key, "_")[0]
    $side = SPLIT($key, "_")[1]
    $iceberg_orders = APPEND(array: $iceberg_orders, item: {
      price_level: $price,
      side: $side,
      num_refreshes: $count,
      revealed_size: $total_size,
      visible_chunk_size: $total_size / $count
    })

**Action:**
RETURN {
  analysis: "Iceberg Order Detection",
  dex_market: $dex_market_id,
  iceberg_orders_detected: $iceberg_orders,
  confidence: 80,
  note: "Iceberg orders are used by large traders to mask their intentions. Detecting them provides insight into hidden liquidity and potential future price pressure.",
  caveats: ["This analysis requires access to high-frequency, tick-level order book data, which may not always be available."]
}

---

## Q721: "Track the complete lifecycle of a notorious serial rugger across 15+ token launches spanning 2 years. Map all wallets, funding sources, and victim count/losses per rug."

**Expected Plan:**

[TIME: ~15m] [COST: ~0.15 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - FIND_TOKEN_DEPLOYMENTS_BY_WALLET (Historical Analysis)
  - BUILD_RUGGER_WALLET_CLUSTER (Forensics)
  - AGGREGATE_VICTIM_LOSSES (Statistics)

**Main Branch:**
$known_rugger_wallet = "..." // Seed wallet for the serial rugger

// Build complete wallet cluster
$rugger_cluster = BUILD_RUGGER_WALLET_CLUSTER(
  seed_wallet: $known_rugger_wallet,
  max_depth: 5,
  include_funding_sources: true
)

// Find all token deployments across the cluster
$all_tokens = []
FOR $wallet IN $rugger_cluster.all_wallets:
  $tokens = FIND_TOKEN_DEPLOYMENTS_BY_WALLET(wallet: $wallet)
  $all_tokens = APPEND($all_tokens, $tokens)

// Analyze each rug
$rug_analysis = MAP(collection: $all_tokens, fn: token => {
  $outcome = ANALYZE_TOKEN_OUTCOME(mint: token.mint)
  $victims = GET_ALL_BUYERS(token: token.mint)
  
  RETURN {
    token_mint: token.mint,
    token_name: token.metadata.name,
    launch_date: token.launch_date,
    deployer_wallet: token.deployer,
    was_rugged: $outcome.was_rugged,
    rug_date: $outcome.rug_date,
    lifetime_days: $outcome.lifetime_days,
    victim_count: COUNT($victims),
    total_victim_loss_usd: SUM(MAP($victims, v => v.net_loss_usd)),
    liquidity_pulled_usd: $outcome.exit_liquidity_usd
  }
})

// Aggregate statistics
$total_rugs = COUNT(FILTER($rug_analysis, r => r.was_rugged))
$total_victims = SUM(MAP($rug_analysis, r => r.victim_count))
$total_stolen_usd = SUM(MAP($rug_analysis, r => r.liquidity_pulled_usd))

**Action:**
RETURN {
  investigation: "Serial Rugger Complete Profile",
  rugger_cluster: $rugger_cluster,
  total_wallets: COUNT($rugger_cluster.all_wallets),
  total_tokens_deployed: COUNT($all_tokens),
  successful_rugs: $total_rugs,
  total_victims: $total_victims,
  total_stolen_usd: $total_stolen_usd,
  avg_rug_lifetime_days: MEAN(MAP(FILTER($rug_analysis, r => r.was_rugged), r => r.lifetime_days)),
  rug_timeline: $rug_analysis,
  confidence: 88
}

---

## Q722: "Analyze the Mt. Gox-style 'slow distribution' of stolen funds from a major exploit 6 months ago. Track all distribution wallets and identify which have cashed out vs. still holding."

**Expected Plan:**

[TIME: ~12m] [COST: ~0.12 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - TRACE_EXPLOIT_FUNDS (Forensics)
  - CLASSIFY_WALLET_STATUS (Analysis)

**Main Branch:**
$exploit_wallet = "..." // Main attacker wallet
$exploit_amount_usd = 50000000 // $50M exploit
$exploit_date = 1712448000 // 6 months ago

// Trace all fund distributions
$fund_trace = TRACE_EXPLOIT_FUNDS(
  source: $exploit_wallet,
  max_depth: 10,
  time_window: 15552000 // 6 months
)

// Identify all distribution wallets
$distribution_wallets = UNIQUE(FLATTEN(MAP($fund_trace.paths, path => path.wallets)))

// Classify each wallet's status
$wallet_statuses = MAP(collection: $distribution_wallets, fn: wallet => {
  $current_balance = getBalance($wallet).value / LAMPORTS_PER_SOL
  $total_received = SUM(MAP(FILTER($fund_trace.transactions, tx => tx.to == wallet), tx => tx.amount))
  $total_sent = SUM(MAP(FILTER($fund_trace.transactions, tx => tx.from == wallet), tx => tx.amount))
  
  $cex_deposits = FILTER(
    getSignaturesForAddress(address: wallet, direction: "out"),
    tx => isCexAddress(tx.to)
  )
  
  RETURN {
    wallet: wallet,
    total_received_sol: $total_received,
    total_sent_sol: $total_sent,
    current_balance_sol: $current_balance,
    has_cashed_out: COUNT($cex_deposits) > 0,
    cex_cashout_amount: SUM(MAP($cex_deposits, tx => tx.amount)),
    status: $current_balance < 1 ? "EMPTIED" : ($current_balance > 100 ? "HOLDING" : "PARTIAL")
  }
})

// Aggregate by status
$status_summary = GROUP_BY(collection: $wallet_statuses, key: "status")

**Action:**
RETURN {
  investigation: "Slow Distribution Tracking",
  exploit_source: $exploit_wallet,
  exploit_amount_usd: $exploit_amount_usd,
  months_since_exploit: 6,
  total_distribution_wallets: COUNT($distribution_wallets),
  wallets_emptied: COUNT(FILTER($wallet_statuses, w => w.status == "EMPTIED")),
  wallets_holding: COUNT(FILTER($wallet_statuses, w => w.status == "HOLDING")),
  wallets_partial: COUNT(FILTER($wallet_statuses, w => w.status == "PARTIAL")),
  total_cashed_out_usd: SUM(MAP(FILTER($wallet_statuses, w => w.has_cashed_out), w => w.cex_cashout_amount)) * getCurrentSolPrice(),
  funds_still_distributed: SUM(MAP($wallet_statuses, w => w.current_balance_sol)),
  wallet_details: $wallet_statuses,
  confidence: 85
}

---

## Q723: "Investigate a 'pump and dump' group that operated on Telegram in 2023. Find all coordinated pumps, member wallets, and calculate their collective profit vs. victim losses."

**Expected Plan:**

[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - DETECT_COORDINATED_PUMPS (Pattern Analysis)
  - BUILD_PUMP_GROUP_CLUSTER (Forensics)

**Main Branch:**
$suspected_tokens = [...] // List of tokens suspected to be pumped
$suspected_time_range = {start: 1672531200, end: 1704067199} // 2023

// Detect coordinated buying patterns
$pump_analysis = MAP(collection: $suspected_tokens, fn: token => {
  $buyers = GET_ALL_BUYERS(
    token: token,
    start_time: $suspected_time_range.start,
    end_time: $suspected_time_range.end
  )
  
  $coordination = DETECT_COORDINATED_PUMPS(
    buyers: $buyers,
    time_correlation_threshold: 300 // 5 minutes
  )
  
  RETURN {
    token: token,
    was_coordinated: $coordination.is_coordinated,
    coordinated_buyers: $coordination.coordinated_wallets,
    pump_time: $coordination.pump_timestamp,
    dump_time: $coordination.dump_timestamp
  }
})

// Build pump group wallet cluster
$coordinated_pumps = FILTER($pump_analysis, p => p.was_coordinated)
$all_pump_wallets = UNIQUE(FLATTEN(MAP($coordinated_pumps, p => p.coordinated_buyers)))

$pump_group_cluster = BUILD_PUMP_GROUP_CLUSTER(
  member_wallets: $all_pump_wallets,
  check_funding_links: true
)

// Calculate profits for group vs. losses for victims
$profit_analysis = MAP(collection: $coordinated_pumps, fn: pump => {
  $group_profits = SUM(MAP(pump.coordinated_buyers, buyer => {
    calculateNetProfit(wallet: buyer, token: pump.token)
  }))
  
  $all_buyers = GET_ALL_BUYERS(token: pump.token)
  $victim_wallets = FILTER($all_buyers, buyer => !CONTAINS($all_pump_wallets, buyer.wallet))
  
  $victim_losses = SUM(MAP($victim_wallets, victim => ABS(victim.net_loss_usd)))
  
  RETURN {
    token: pump.token,
    group_profit_usd: $group_profits,
    victim_loss_usd: $victim_losses,
    victim_count: COUNT($victim_wallets)
  }
})

**Action:**
RETURN {
  investigation: "Telegram Pump Group 2023",
  total_coordinated_pumps: COUNT($coordinated_pumps),
  pump_group_size: COUNT($pump_group_cluster.core_members),
  total_group_profit_usd: SUM(MAP($profit_analysis, p => p.group_profit_usd)),
  total_victim_losses_usd: SUM(MAP($profit_analysis, p => p.victim_loss_usd)),
  total_victims: SUM(MAP($profit_analysis, p => p.victim_count)),
  pump_timeline: $profit_analysis,
  confidence: 80
}

---

## Q724: "Map the evolution of a scammer's tactics over 3 years by analyzing 50+ scams. Identify pattern changes, improvements in obfuscation, and victim targeting shifts."

**Expected Plan:**

[TIME: ~18m] [COST: ~0.18 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - ANALYZE_SCAM_EVOLUTION (Advanced Analysis)
  - CLUSTER_SCAM_TACTICS (Pattern Recognition)

**Main Branch:**
$scammer_root_wallet = "..."
$analysis_period = {start: 1640995200, end: 1704067199} // 2022-2024

// Build complete scammer operation cluster
$scammer_cluster = BUILD_RUGGER_WALLET_CLUSTER(
  seed_wallet: $scammer_root_wallet,
  max_depth: 7
)

// Find all scams chronologically
$all_scams = FIND_ALL_SCAMS_BY_CLUSTER(
  cluster: $scammer_cluster,
  start_date: $analysis_period.start,
  end_date: $analysis_period.end
)

// Group by time periods (quarters)
$scams_by_quarter = GROUP_BY_TIME_PERIOD(
  collection: $all_scams,
  period: "quarter"
)

// Analyze tactics evolution
$tactics_evolution = MAP(collection: $scams_by_quarter, fn: quarter => {
  $scams = quarter.scams
  
  $tactic_analysis = {
    quarter: quarter.period,
    scam_count: COUNT($scams),
    
    // Obfuscation complexity
    avg_wallet_hops: MEAN(MAP($scams, s => s.fund_flow_depth)),
    mixer_usage: COUNT(FILTER($scams, s => s.used_mixer)) / COUNT($scams),
    
    // Smart contract sophistication
    contract_complexity: MEAN(MAP($scams, s => s.contract_instruction_count)),
    uses_timelock: COUNT(FILTER($scams, s => s.has_timelock)) / COUNT($scams),
    uses_whitelist: COUNT(FILTER($scams, s => s.has_whitelist)) / COUNT($scams),
    
    // Victim targeting
    avg_victim_wallet_age_days: MEAN(MAP($scams, s => s.avg_victim_wallet_age)),
    targets_new_wallets: COUNT(FILTER($scams, s => s.avg_victim_wallet_age < 30)) / COUNT($scams),
    
    // Financial metrics
    avg_stolen_per_scam: MEAN(MAP($scams, s => s.stolen_amount_usd)),
    avg_victim_count: MEAN(MAP($scams, s => s.victim_count))
  }
  
  RETURN $tactic_analysis
})

**Action:**
RETURN {
  investigation: "Scammer Tactics Evolution 2022-2024",
  scammer_cluster: $scammer_cluster,
  total_scams: COUNT($all_scams),
  analysis_periods: COUNT($tactics_evolution),
  tactics_evolution: $tactics_evolution,
  key_findings: {
    obfuscation_improvement: ($tactics_evolution[LAST].mixer_usage - $tactics_evolution[0].mixer_usage) * 100,
    sophistication_increase: ($tactics_evolution[LAST].contract_complexity - $tactics_evolution[0].contract_complexity) / $tactics_evolution[0].contract_complexity * 100,
    victim_targeting_shift: $tactics_evolution[LAST].targets_new_wallets > $tactics_evolution[0].targets_new_wallets ? "Shifted to newer wallets" : "Maintained targeting"
  },
  confidence: 82
}

---

## Q725: "Trace a stolen NFT collection (10,000+ NFTs) from a Discord hack through all resale platforms, wash trades, and identify current holders who may be unknowing buyers vs. laundering accomplices."

**Expected Plan:**

[TIME: ~14m] [COST: ~0.14 SOL] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library:
  - TRACE_NFT_TRANSFERS (NFT Analysis)
  - DETECT_WASH_TRADING (Pattern Analysis)
  - CLASSIFY_HOLDER_INTENT (Forensics)

**Main Branch:**
$stolen_nft_collection = "..." // Collection mint
$hack_timestamp = 1728432000
$victim_wallets = [...] // Wallets drained in the Discord hack

// Get all NFTs from the collection
$all_nfts = GET_COLLECTION_NFTS(collection: $stolen_nft_collection)

// Filter for NFTs that were in victim wallets at hack time
$stolen_nfts = FILTER($all_nfts, nft => {
  $owner_at_hack = GET_NFT_OWNER_AT_TIME(nft: nft.mint, timestamp: $hack_timestamp)
  RETURN CONTAINS($victim_wallets, $owner_at_hack)
})

// Trace each stolen NFT's journey
$nft_journeys = MAP(collection: $stolen_nfts, fn: nft => {
  $transfers = TRACE_NFT_TRANSFERS(
    nft: nft.mint,
    start_time: $hack_timestamp
  )
  
  $sales = FILTER($transfers, t => t.sale_price > 0)
  $wash_trades = DETECT_WASH_TRADING(transactions: $sales)
  
  RETURN {
    nft: nft.mint,
    transfer_count: COUNT($transfers),
    sale_count: COUNT($sales),
    wash_trade_count: COUNT($wash_trades.detected_wash_trades),
    current_owner: LAST($transfers).to,
    total_sale_volume: SUM(MAP($sales, s => s.sale_price))
  }
})

// Classify current holders
$current_holders = UNIQUE(MAP($nft_journeys, j => j.current_owner))

$holder_classification = MAP(collection: $current_holders, fn: holder => {
  $nfts_held = FILTER($nft_journeys, j => j.current_owner == holder)
  $holder_history = GET_WALLET_NFT_HISTORY(wallet: holder)
  
  // Check for connections to hacker
  $connection_to_hacker = FIND_WALLET_CONNECTIONS(
    target: holder,
    suspects: $victim_wallets,
    max_depth: 3
  )
  
  // Determine classification
  $classification = "UNKNOWN"
  IF COUNT($nfts_held) > 50:
    $classification = "LIKELY_ACCOMPLICE" // Holding too many
  ELSE IF $connection_to_hacker.has_connection:
    $classification = "CONNECTED_TO_HACKER"
  ELSE IF $holder_history.purchase_count > 100 && $holder_history.avg_hold_time > 2592000:
    $classification = "LIKELY_INNOCENT_BUYER"
  
  RETURN {
    holder: holder,
    stolen_nfts_held: COUNT($nfts_held),
    classification: $classification,
    wallet_age_days: ($hack_timestamp - getWalletCreationDate(holder)) / 86400,
    connection_to_hacker: $connection_to_hacker.has_connection
  }
})

**Action:**
RETURN {
  investigation: "Stolen NFT Collection Tracing",
  collection: $stolen_nft_collection,
  total_nfts_stolen: COUNT($stolen_nfts),
  nfts_with_wash_trading: COUNT(FILTER($nft_journeys, j => j.wash_trade_count > 0)),
  current_unique_holders: COUNT($current_holders),
  holder_breakdown: {
    likely_accomplices: COUNT(FILTER($holder_classification, h => h.classification == "LIKELY_ACCOMPLICE")),
    connected_to_hacker: COUNT(FILTER($holder_classification, h => h.classification == "CONNECTED_TO_HACKER")),
    likely_innocent: COUNT(FILTER($holder_classification, h => h.classification == "LIKELY_INNOCENT_BUYER"))
  },
  holder_details: $holder_classification,
  confidence: 78
}

---

## Q726: "Investigate a 'VC insider trading' case where early token allocations were sold before public announcement. Map the insider wallet cluster and calculate illegal front-running profits."

**Expected Plan:**

[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - FIND_EARLY_ALLOCATIONS (Token Analysis)
  - DETECT_INSIDER_TRADING (Forensics)

**Main Branch:**
$token_mint = "..."
$public_announcement_time = 1728432000
$token_launch_time = 1728518400 // 24h after announcement

// Find all early token holders
$early_allocations = FIND_EARLY_ALLOCATIONS(
  token: $token_mint,
  before: $token_launch_time
)

// Identify sales before public announcement
$suspicious_early_sales = FILTER($early_allocations, holder => {
  $sales = GET_TOKEN_SALES(
    wallet: holder.wallet,
    token: $token_mint,
    start_time: holder.allocation_time,
    end_time: $public_announcement_time
  )
  
  RETURN COUNT($sales) > 0
})

// Build insider cluster
$insider_wallets = MAP($suspicious_early_sales, s => s.wallet)
$insider_cluster = BUILD_WALLET_CLUSTER(
  seed_wallets: $insider_wallets,
  check_funding_links: true
)

// Calculate front-running profits
$profit_analysis = MAP(collection: $suspicious_early_sales, fn: insider => {
  $sales = GET_TOKEN_SALES(
    wallet: insider.wallet,
    token: $token_mint,
    start_time: insider.allocation_time,
    end_time: $public_announcement_time
  )
  
  $avg_sell_price = MEAN(MAP($sales, s => s.price))
  
  $public_launch_price = GET_TOKEN_PRICE_AT_TIME(
    token: $token_mint,
    timestamp: $token_launch_time
  )
  
  $tokens_sold = SUM(MAP($sales, s => s.amount))
  $profit_vs_launch = ($public_launch_price - $avg_sell_price) * $tokens_sold
  
  RETURN {
    insider_wallet: insider.wallet,
    allocation_amount: insider.amount,
    tokens_sold_early: $tokens_sold,
    avg_sell_price: $avg_sell_price,
    public_launch_price: $public_launch_price,
    illegal_profit_usd: $profit_vs_launch
  }
})

**Action:**
RETURN {
  investigation: "VC Insider Trading",
  token: $token_mint,
  public_announcement: $public_announcement_time,
  token_launch: $token_launch_time,
  total_early_allocations: COUNT($early_allocations),
  insiders_who_sold_early: COUNT($suspicious_early_sales),
  insider_cluster_size: COUNT($insider_cluster.all_wallets),
  total_illegal_profits_usd: SUM(MAP($profit_analysis, p => p.illegal_profit_usd)),
  insider_details: $profit_analysis,
  confidence: 85
}

---

## Q727: "Map a 'validator bribery ring' where validators accepted payments to prioritize specific transactions. Identify all participating validators, bribe payers, and calculate unfair MEV profits."

**Expected Plan:**

[TIME: ~11m] [COST: ~0.11 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - DETECT_VALIDATOR_BRIBERY (Forensics)
  - ANALYZE_TX_PRIORITIZATION_ANOMALIES (MEV Analysis)

**Main Branch:**
$suspected_time_period = {start: 1727222400, end: 1728432000} // 2 weeks

// Get all validators during the period
$validators = getVoteAccounts().current

// Analyze each validator for bribery signs
$bribery_analysis = MAP(collection: $validators, fn: validator => {
  // Check for unusual incoming payments
  $incoming = getSignaturesForAddress(
    address: validator.nodePubkey,
    start: $suspected_time_period.start,
    end: $suspected_time_period.end
  )
  
  $unusual_payments = FILTER($incoming, tx => {
    // Detect patterns: regular amounts, same senders
    tx.amount > 10 && !isStakeRelated(tx)
  })
  
  // Check for transaction prioritization anomalies
  $blocks_produced = GET_BLOCKS_BY_VALIDATOR(
    validator: validator.votePubkey,
    start: $suspected_time_period.start,
    end: $suspected_time_period.end
  )
  
  $prioritization = ANALYZE_TX_PRIORITIZATION_ANOMALIES(blocks: $blocks_produced)
  
  RETURN {
    validator: validator.votePubkey,
    node_pubkey: validator.nodePubkey,
    unusual_payments_count: COUNT($unusual_payments),
    total_bribe_amount: SUM(MAP($unusual_payments, p => p.amount)),
    has_prioritization_anomalies: $prioritization.has_anomalies,
    suspected_bribe_payers: UNIQUE(MAP($unusual_payments, p => p.from))
  }
})

// Filter for validators with evidence
$bribed_validators = FILTER($bribery_analysis, v => {
  v.unusual_payments_count > 5 && v.has_prioritization_anomalies
})

// Build bribe payer cluster
$all_bribe_payers = UNIQUE(FLATTEN(MAP($bribed_validators, v => v.suspected_bribe_payers)))
$bribe_payer_cluster = BUILD_WALLET_CLUSTER(seed_wallets: $all_bribe_payers)

// Calculate unfair MEV profits
$mev_profits = MAP(collection: $bribe_payer_cluster.all_wallets, fn: payer => {
  $mev_txs = GET_MEV_TRANSACTIONS(
    wallet: payer,
    start: $suspected_time_period.start,
    end: $suspected_time_period.end
  )
  
  $profit = SUM(MAP($mev_txs, tx => tx.profit_usd))
  $bribes_paid = SUM(MAP(FILTER($bribery_analysis.FLATTEN.unusual_payments, p => p.from == payer), p => p.amount))
  
  RETURN {
    payer: payer,
    mev_profit_usd: $profit,
    bribes_paid_sol: $bribes_paid,
    net_profit_usd: $profit - ($bribes_paid * getCurrentSolPrice())
  }
})

**Action:**
RETURN {
  investigation: "Validator Bribery Ring",
  time_period: $suspected_time_period,
  total_validators_analyzed: COUNT($validators),
  bribed_validators: COUNT($bribed_validators),
  bribe_payer_cluster_size: COUNT($bribe_payer_cluster.all_wallets),
  total_bribes_paid_sol: SUM(MAP($bribed_validators, v => v.total_bribe_amount)),
  total_unfair_mev_profits_usd: SUM(MAP($mev_profits, p => p.mev_profit_usd)),
  validator_details: $bribed_validators,
  payer_details: $mev_profits,
  confidence: 83
}

---

## Q728: "Investigate a 'fake liquidity' scheme where the same wallets repeatedly add/remove liquidity to create the appearance of healthy trading. Identify the Sybil cluster and their coordination patterns."

**Expected Plan:**

[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - DETECT_FAKE_LIQUIDITY (Pattern Analysis)
  - BUILD_LP_SYBIL_CLUSTER (Forensics)

**Main Branch:**
$token_mint = "..."
$pool_address = "..."

// Get all LP position changes
$lp_events = GET_LP_EVENTS(
  pool: $pool_address,
  event_types: ["add_liquidity", "remove_liquidity"]
)

// Detect suspicious add/remove patterns
$suspicious_lp_providers = DETECT_FAKE_LIQUIDITY(
  events: $lp_events,
  pattern_threshold: 0.75 // High repeatability = suspicious
)

// Build Sybil cluster
$sybil_cluster = BUILD_LP_SYBIL_CLUSTER(
  seed_wallets: MAP($suspicious_lp_providers, p => p.wallet),
  check_funding: true,
  check_creation_time: true
)

// Analyze coordination patterns
$coordination_analysis = ANALYZE_TEMPORAL_COORDINATION(
  wallets: $sybil_cluster.all_wallets,
  actions: $lp_events,
  time_windows: [300, 900, 3600] // 5min, 15min, 1h
})

// Calculate fake vs. real liquidity
$real_lp_providers = FILTER($lp_events, event => {
  !CONTAINS($sybil_cluster.all_wallets, event.wallet)
})

$fake_liquidity_pct = (COUNT(FILTER($lp_events, e => CONTAINS($sybil_cluster.all_wallets, e.wallet))) / COUNT($lp_events)) * 100

**Action:**
RETURN {
  investigation: "Fake Liquidity Scheme",
  token: $token_mint,
  pool: $pool_address,
  sybil_cluster_size: COUNT($sybil_cluster.all_wallets),
  total_lp_events: COUNT($lp_events),
  fake_lp_events: COUNT(FILTER($lp_events, e => CONTAINS($sybil_cluster.all_wallets, e.wallet))),
  fake_liquidity_percentage: $fake_liquidity_pct,
  coordination_detected: $coordination_analysis.is_coordinated,
  most_common_time_window: $coordination_analysis.strongest_correlation_window,
  sybil_wallets: $sybil_cluster.all_wallets,
  confidence: 88
}

---

## Q729: "Trace funds from a DeFi protocol exploit through multiple DEX swaps, cross-chain bridges, and privacy mixers to identify the final cash-out point 3 months later."

**Expected Plan:**

[TIME: ~20m] [COST: ~0.2 SOL] [CONFIDENCE: 70%]

**Available Tools:**
From Standard Library:
  - TRACE_MULTI_HOP_FUNDS (Advanced Forensics)
  - DETECT_BRIDGE_TRANSFERS (Cross-chain Analysis)
  - CORRELATE_MIXER_EXITS (Pattern Analysis)

**Main Branch:**
$exploit_wallet = "..."
$exploit_amount = 10000 // SOL
$exploit_time = 1720396800 // 3 months ago

// Phase 1: Trace through DEX swaps
$dex_trace = TRACE_FUNDS_THROUGH_DEX(
  source: $exploit_wallet,
  start_time: $exploit_time,
  max_hops: 20
)

// Phase 2: Detect bridge transfers
$bridge_transfers = DETECT_BRIDGE_TRANSFERS(
  wallets: MAP($dex_trace.all_wallets, w => w),
  timeframe: {start: $exploit_time, end: $exploit_time + 7776000} // 3 months
)

// Phase 3: Trace through mixers
$mixer_interactions = FILTER($dex_trace.transactions, tx => isMixerProgram(tx.program))

$mixer_exit_correlation = []
IF COUNT($mixer_interactions) > 0:
  $mixer_exit_correlation = CORRELATE_MIXER_EXITS(
    mixer_deposits: $mixer_interactions,
    time_window: 7776000 // 3 months
  )

// Phase 4: Find final cashout
$all_suspect_wallets = UNIQUE(FLATTEN([
  MAP($dex_trace.all_wallets, w => w),
  MAP($bridge_transfers, b => b.destination_wallet),
  MAP($mixer_exit_correlation, e => e.exit_wallet)
]))

$cex_deposits = []
FOR $wallet IN $all_suspect_wallets:
  $deposits = FILTER(
    getSignaturesForAddress(address: $wallet, direction: "out"),
    tx => isCexAddress(tx.to)
  )
  IF COUNT($deposits) > 0:
    $cex_deposits = APPEND($cex_deposits, {
      wallet: $wallet,
      cex_address: $deposits[0].to,
      cex_name: getCexName($deposits[0].to),
      deposit_amount: SUM(MAP($deposits, d => d.amount)),
      deposit_times: MAP($deposits, d => d.timestamp)
    })

**Action:**
RETURN {
  investigation: "Long-term Exploit Fund Tracing",
  exploit_source: $exploit_wallet,
  exploit_amount_sol: $exploit_amount,
  months_traced: 3,
  total_wallet_hops: COUNT($dex_trace.all_wallets),
  dex_swaps: COUNT($dex_trace.transactions),
  bridge_transfers_detected: COUNT($bridge_transfers),
  mixer_usage: COUNT($mixer_interactions) > 0,
  final_cashout_wallets: COUNT($cex_deposits),
  primary_cashout_cex: FIRST(SORT_BY($cex_deposits, "deposit_amount", desc)).cex_name,
  total_cashed_out_sol: SUM(MAP($cex_deposits, c => c.deposit_amount)),
  cashout_details: $cex_deposits,
  confidence: 70,
  caveats: ["Long time window reduces correlation confidence", "Multiple bridge hops may introduce false positives"]
}

---

## Q730: "Map a 'validator vote buying' scandal where a token project paid validators to vote for governance proposals. Identify all bribed validators, payment amounts, and proposal outcomes."

**Expected Plan:**

[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - ANALYZE_GOVERNANCE_VOTES (DAO Analysis)
  - DETECT_VOTE_BUYING (Forensics)

**Main Branch:**
$dao_program = "..." // The DAO/governance program
$proposal_id = "..." // Suspicious proposal
$project_treasury = "..." // Project's known wallet

// Get all votes on the proposal
$all_votes = ANALYZE_GOVERNANCE_VOTES(
  dao: $dao_program,
  proposal: $proposal_id
)

// Check for payments from project to voters before voting
$vote_buying_analysis = MAP(collection: $all_votes, fn: vote => {
  $voter_wallet = vote.voter
  
  // Check for incoming payments from project before vote
  $pre_vote_payments = FILTER(
    getSignaturesForAddress(address: $voter_wallet, direction: "in"),
    tx => tx.from == $project_treasury && tx.timestamp < vote.vote_time && tx.timestamp > (vote.vote_time - 604800) // 7 days before
  )
  
  RETURN {
    voter: $voter_wallet,
    vote_direction: vote.vote_direction,
    vote_weight: vote.vote_weight,
    vote_time: vote.vote_time,
    received_payment_before_vote: COUNT($pre_vote_payments) > 0,
    payment_amount: SUM(MAP($pre_vote_payments, p => p.amount)),
    payment_token: COUNT($pre_vote_payments) > 0 ? $pre_vote_payments[0].token : null
  }
})

// Filter for likely bribed validators
$bribed_validators = FILTER($vote_buying_analysis, v => v.received_payment_before_vote)

// Calculate impact on proposal outcome
$total_votes = SUM(MAP($all_votes, v => v.vote_weight))
$bribed_vote_weight = SUM(MAP($bribed_validators, v => v.vote_weight))
$bribed_impact_pct = ($bribed_vote_weight / $total_votes) * 100

**Action:**
RETURN {
  investigation: "Validator Vote Buying Scandal",
  dao: $dao_program,
  proposal: $proposal_id,
  total_voters: COUNT($all_votes),
  bribed_validators: COUNT($bribed_validators),
  total_bribes_paid_sol: SUM(MAP($bribed_validators, v => v.payment_amount)),
  bribed_vote_weight: $bribed_vote_weight,
  bribed_impact_percentage: $bribed_impact_pct,
  proposal_passed: getProposalOutcome($dao_program, $proposal_id).passed,
  would_have_passed_without_bribes: ($total_votes - $bribed_vote_weight) > getProposalQuorum($dao_program, $proposal_id),
  validator_details: $bribed_validators,
  confidence: 86
}

---

## Q731: "Detect 'spoofed LP tokens' where scammers create fake LP tokens that look identical to real ones to trick users into depositing value."

**Expected Plan:**
[TIME: ~5m] [COST: ~0.05 SOL] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library: VERIFY_LP_TOKEN_AUTHENTICITY, DETECT_FAKE_LP_TOKENS

**Main Branch:**
$suspected_lp_token = "..."
$real_pool_address = "..."
$verification = VERIFY_LP_TOKEN_AUTHENTICITY(lp_token: $suspected_lp_token, known_pool: $real_pool_address)
IF !$verification.is_authentic:
  $victims = FIND_FAKE_LP_TOKEN_VICTIMS(fake_token: $suspected_lp_token)

**Action:**
RETURN {investigation: "Spoofed LP Token Scam", is_fake: !$verification.is_authentic, victim_count: COUNT($victims), total_loss_sol: SUM(MAP($victims, v => v.loss)), confidence: 89}

---

## Q732: "Map a 'DAO treasury raid' where malicious proposal drains funds. Trace the proposal creation, voting patterns, and fund destinations."

**Expected Plan:**
[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library: ANALYZE_DAO_PROPOSAL, TRACE_TREASURY_FUNDS

**Main Branch:**
$dao_program = "..."
$malicious_proposal = "..."
$proposal_analysis = ANALYZE_DAO_PROPOSAL(dao: $dao_program, proposal: $malicious_proposal)
$fund_trace = TRACE_TREASURY_FUNDS(source: $proposal_analysis.treasury_address, drain_tx: $proposal_analysis.execution_tx)

**Action:**
RETURN {investigation: "DAO Treasury Raid", drained_amount_sol: $proposal_analysis.amount_drained, voting_manipulation: $proposal_analysis.suspicious_votes, fund_destinations: $fund_trace.final_addresses, confidence: 84}

---

## Q733: "Identify 'circular trading bots' that create artificial volume by trading between their own wallets with no real market impact."

**Expected Plan:**
[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: DETECT_CIRCULAR_TRADING, BUILD_BOT_CLUSTER

**Main Branch:**
$token = "..."
$trades = GET_TOKEN_TRADES(token: $token, period: "24h")
$circular = DETECT_CIRCULAR_TRADING(trades: $trades, max_circle_size: 10)
$bot_cluster = BUILD_BOT_CLUSTER(circular_traders: $circular.involved_wallets)

**Action:**
RETURN {investigation: "Circular Trading Bot Network", bot_count: COUNT($bot_cluster.wallets), fake_volume_sol: $circular.total_circular_volume, real_volume_pct: $circular.real_volume_percentage, confidence: 87}

---

## Q734: "Investigate a 'bridge exploit' where attacker mints unlimited tokens by exploiting cross-chain message verification."

**Expected Plan:**
[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library: ANALYZE_BRIDGE_EXPLOIT, TRACE_MINTED_TOKENS

**Main Branch:**
$bridge_contract = "..."
$exploit_tx = "..."
$analysis = ANALYZE_BRIDGE_EXPLOIT(bridge: $bridge_contract, exploit: $exploit_tx)
$minted_trace = TRACE_MINTED_TOKENS(source: $analysis.attacker_wallet, token: $analysis.exploited_token)

**Action:**
RETURN {investigation: "Bridge Exploit - Unlimited Mint", exploit_method: $analysis.vulnerability_type, tokens_minted: $analysis.mint_amount, attacker_profit_usd: $analysis.realized_profit, confidence: 81}

---

## Q735: "Detect 'governance token rental markets' where users rent voting power, potentially enabling vote manipulation."

**Expected Plan:**
[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: FIND_TOKEN_RENTAL_PLATFORMS, ANALYZE_RENTAL_VOTING_IMPACT

**Main Branch:**
$gov_token = "..."
$rentals = FIND_TOKEN_RENTAL_PLATFORMS(token: $gov_token)
$voting_impact = ANALYZE_RENTAL_VOTING_IMPACT(rentals: $rentals, proposals: GET_RECENT_PROPOSALS($gov_token))

**Action:**
RETURN {investigation: "Governance Token Rental Market", rental_volume_daily: $rentals.avg_daily_volume, proposals_affected: COUNT($voting_impact.influenced_proposals), manipulation_risk: $voting_impact.risk_score, confidence: 85}

---

## Q736: "Map a 'MEV sandwich cartel' where multiple bots coordinate to avoid competing on the same victim transactions."

**Expected Plan:**
[TIME: ~11m] [COST: ~0.11 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library: DETECT_MEV_BOT_COORDINATION, ANALYZE_VICTIM_ALLOCATION

**Main Branch:**
$mev_bots = FIND_SANDWICH_BOTS(period: "7d", min_profit: 10)
$coordination = DETECT_MEV_BOT_COORDINATION(bots: $mev_bots)
$allocation = ANALYZE_VICTIM_ALLOCATION(coordinated_bots: $coordination.cartel_members)

**Action:**
RETURN {investigation: "MEV Sandwich Cartel", cartel_size: COUNT($coordination.cartel_members), coordination_evidence: $coordination.evidence_score, profit_sharing_detected: $allocation.shows_coordination, confidence: 82}

---

## Q737: "Investigate 'NFT metadata poisoning' where scammers inject malicious URLs into NFT metadata to phish holders."

**Expected Plan:**
[TIME: ~6m] [COST: ~0.06 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: SCAN_NFT_METADATA_URLS, DETECT_PHISHING_LINKS

**Main Branch:**
$collection = "..."
$all_nfts = GET_COLLECTION_NFTS(collection: $collection)
$url_scan = SCAN_NFT_METADATA_URLS(nfts: $all_nfts)
$malicious = DETECT_PHISHING_LINKS(urls: $url_scan.all_urls)

**Action:**
RETURN {investigation: "NFT Metadata Poisoning", poisoned_nfts: COUNT($malicious.nfts_with_bad_urls), phishing_domains: $malicious.malicious_domains, holder_risk: $malicious.estimated_victims, confidence: 90}

---

## Q738: "Detect 'stablecoin depeg manipulation' where whales intentionally cause depegs to profit from panic selling."

**Expected Plan:**
[TIME: ~12m] [COST: ~0.12 SOL] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library: ANALYZE_DEPEG_EVENT, DETECT_MANIPULATION_TRADING

**Main Branch:**
$stablecoin = "..."
$depeg_timestamp = 1728432000
$depeg_analysis = ANALYZE_DEPEG_EVENT(token: $stablecoin, timestamp: $depeg_timestamp)
$manipulation = DETECT_MANIPULATION_TRADING(event: $depeg_analysis)

**Action:**
RETURN {investigation: "Stablecoin Depeg Manipulation", manipulation_detected: $manipulation.intentional_probability > 0.7, whale_traders: $manipulation.suspicious_wallets, manipulator_profit_usd: $manipulation.estimated_profit, confidence: 79}

---

## Q739: "Map 'validator sandwich attack coordination' where validators collude to sandwich user transactions in their own blocks."

**Expected Plan:**
[TIME: ~13m] [COST: ~0.13 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: ANALYZE_VALIDATOR_SANDWICHES, DETECT_CROSS_VALIDATOR_COLLUSION

**Main Branch:**
$validators = [...]
$sandwiches = ANALYZE_VALIDATOR_SANDWICHES(validators: $validators, period: "14d")
$collusion = DETECT_CROSS_VALIDATOR_COLLUSION(sandwich_events: $sandwiches)

**Action:**
RETURN {investigation: "Validator Sandwich Collusion", colluding_validators: COUNT($collusion.coordinated_validators), total_extracted_sol: $collusion.total_mev, victim_transactions: COUNT($sandwiches.all_victims), confidence: 80}

---

## Q740: "Investigate 'pump-and-dump as a service' platforms that coordinate paid pump campaigns for tokens."

**Expected Plan:**
[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: IDENTIFY_PUMP_SERVICE_WALLETS, MAP_PUMP_CAMPAIGNS

**Main Branch:**
$service_operator = "..."
$campaigns = MAP_PUMP_CAMPAIGNS(operator: $service_operator, period: "60d")
$client_analysis = ANALYZE_PUMP_CLIENTS(campaigns: $campaigns)

**Action:**
RETURN {investigation: "Pump-as-a-Service Platform", total_campaigns: COUNT($campaigns), client_count: COUNT($client_analysis.unique_clients), service_revenue_sol: $campaigns.total_fees_collected, confidence: 83}

---

## Q741: "Detect 'fake liquidity lock' where scammers lock liquidity in a contract they control and can withdraw anytime."

**Expected Plan:**
[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library: ANALYZE_LIQUIDITY_LOCK, DETECT_BACKDOOR_ACCESS

**Main Branch:**
$lock_contract = "..."
$lock_analysis = ANALYZE_LIQUIDITY_LOCK(contract: $lock_contract)
$backdoors = DETECT_BACKDOOR_ACCESS(contract: $lock_contract, lock_data: $lock_analysis)

**Action:**
RETURN {investigation: "Fake Liquidity Lock", is_fake_lock: $backdoors.can_bypass_lock, bypass_method: $backdoors.method, locked_amount_sol: $lock_analysis.locked_value, confidence: 88}

---

## Q742: "Map 'cNFT minting exploits' where attackers mint compressed NFTs beyond the tree capacity by exploiting Merkle tree verification."

**Expected Plan:**
[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library: ANALYZE_CNFT_TREE, DETECT_OVERMINT_EXPLOIT

**Main Branch:**
$tree_address = "..."
$tree_data = ANALYZE_CNFT_TREE(tree: $tree_address)
$overmint = DETECT_OVERMINT_EXPLOIT(tree: $tree_address, expected_capacity: $tree_data.max_capacity)

**Action:**
RETURN {investigation: "cNFT Overmint Exploit", exploit_detected: $overmint.detected, extra_mints: $overmint.mints_beyond_capacity, attacker_wallet: $overmint.exploiter, confidence: 81}

---

## Q743: "Investigate 'prediction market oracle manipulation' where insiders with early knowledge manipulate market odds before public announcement."

**Expected Plan:**
[TIME: ~11m] [COST: ~0.11 SOL] [CONFIDENCE: 76%]

**Available Tools:**
From Standard Library: ANALYZE_PREDICTION_MARKET, DETECT_INSIDER_TRADING

**Main Branch:**
$market_address = "..."
$event_result = "..."
$market_analysis = ANALYZE_PREDICTION_MARKET(market: $market_address, result: $event_result)
$insider_trades = DETECT_INSIDER_TRADING(trades: $market_analysis.pre_result_trades, result_time: $market_analysis.result_timestamp)

**Action:**
RETURN {investigation: "Prediction Market Insider Trading", suspicious_traders: COUNT($insider_trades.likely_insiders), insider_profit_sol: $insider_trades.total_profit, trade_timing_correlation: $insider_trades.correlation_score, confidence: 76}

---

## Q744: "Detect 'token supply inflation' attacks where minter role is exploited to create unlimited tokens and dump on market."

**Expected Plan:**
[TIME: ~6m] [COST: ~0.06 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: ANALYZE_TOKEN_SUPPLY_CHANGES, DETECT_UNAUTHORIZED_MINTS

**Main Branch:**
$token_mint = "..."
$supply_history = ANALYZE_TOKEN_SUPPLY_CHANGES(mint: $token_mint, period: "30d")
$unauthorized = DETECT_UNAUTHORIZED_MINTS(supply_changes: $supply_history)

**Action:**
RETURN {investigation: "Token Supply Inflation Attack", inflation_detected: $unauthorized.detected, tokens_minted: $unauthorized.total_minted, dumped_amount: $unauthorized.sold_amount, price_impact_pct: $unauthorized.price_drop_pct, confidence: 91}

---

## Q745: "Map a 'liquidation bot frontrunning cartel' where bots coordinate to avoid competing on the same liquidation opportunities."

**Expected Plan:**
[TIME: ~12m] [COST: ~0.12 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library: FIND_LIQUIDATION_BOTS, DETECT_BOT_COORDINATION

**Main Branch:**
$lending_protocol = "..."
$bots = FIND_LIQUIDATION_BOTS(protocol: $lending_protocol, period: "14d")
$coordination = DETECT_BOT_COORDINATION(bots: $bots, activity_type: "liquidations")

**Action:**
RETURN {investigation: "Liquidation Bot Cartel", cartel_size: COUNT($coordination.coordinated_bots), non_competitive_rate: $coordination.overlap_rate, shared_infrastructure: $coordination.common_funding, confidence: 82}

---

## Q746: "Investigate 'fake airdrop claim' scams that require users to approve unlimited token spending to claim worthless tokens."

**Expected Plan:**
[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library: ANALYZE_AIRDROP_CONTRACT, DETECT_APPROVAL_DRAIN_PATTERN

**Main Branch:**
$airdrop_contract = "..."
$contract_analysis = ANALYZE_AIRDROP_CONTRACT(contract: $airdrop_contract)
$drain_pattern = DETECT_APPROVAL_DRAIN_PATTERN(contract: $airdrop_contract, claimers: GET_AIRDROP_CLAIMERS($airdrop_contract))

**Action:**
RETURN {investigation: "Fake Airdrop Drain Scam", is_drainer: $drain_pattern.is_malicious, victims_drained: COUNT($drain_pattern.drained_wallets), total_stolen_usd: $drain_pattern.total_stolen, confidence: 89}

---

## Q747: "Detect 'validator censorship' where specific validators consistently exclude certain transactions or protocols from their blocks."

**Expected Plan:**
[TIME: ~14m] [COST: ~0.14 SOL] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library: ANALYZE_VALIDATOR_BLOCK_CONTENT, DETECT_CENSORSHIP_PATTERNS

**Main Branch:**
$validator = "..."
$blocks = GET_VALIDATOR_BLOCKS(validator: $validator, period: "30d")
$content_analysis = ANALYZE_VALIDATOR_BLOCK_CONTENT(blocks: $blocks)
$censorship = DETECT_CENSORSHIP_PATTERNS(content: $content_analysis)

**Action:**
RETURN {investigation: "Validator Censorship", censorship_detected: $censorship.is_censoring, censored_protocols: $censorship.excluded_programs, censorship_rate_pct: $censorship.exclusion_rate, confidence: 78}

---

## Q748: "Map 'cross-DEX arbitrage bot networks' that share infrastructure and coordinate to reduce gas competition."

**Expected Plan:**
[TIME: ~13m] [COST: ~0.13 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: FIND_ARBITRAGE_BOTS, BUILD_BOT_NETWORK

**Main Branch:**
$dexes = ["Raydium", "Orca", "Jupiter"]
$arb_bots = FIND_ARBITRAGE_BOTS(dexes: $dexes, period: "14d", min_profit: 5)
$network = BUILD_BOT_NETWORK(bots: $arb_bots, link_by: ["funding", "timing", "strategy"])

**Action:**
RETURN {investigation: "Arbitrage Bot Network", network_clusters: COUNT($network.clusters), largest_cluster: MAX(MAP($network.clusters, c => COUNT(c.bots))), cluster_profit_sol: SUM(MAP($network.clusters, c => c.total_profit)), confidence: 83}

---

## Q749: "Investigate 'mempool frontrunning on Solana' by analyzing Jito bundle patterns and identifying systematic frontrunners."

**Expected Plan:**
[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library: ANALYZE_JITO_BUNDLES, DETECT_SYSTEMATIC_FRONTRUNNING

**Main Branch:**
$time_period = {start: 1728000000, end: 1728432000}
$bundles = ANALYZE_JITO_BUNDLES(period: $time_period)
$frontrunners = DETECT_SYSTEMATIC_FRONTRUNNING(bundles: $bundles)

**Action:**
RETURN {investigation: "Jito Bundle Frontrunning", systematic_frontrunners: COUNT($frontrunners.serial_frontrunners), total_frontrun_profit_sol: $frontrunners.total_profit, avg_tip_sol: $frontrunners.avg_jito_tip, confidence: 84}

---

## Q750: "Detect 'rug pull via upgrade authority' where devs use program upgrade authority to inject malicious code after launch."

**Expected Plan:**
[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: MONITOR_PROGRAM_UPGRADES, DETECT_MALICIOUS_UPGRADES

**Main Branch:**
$program_id = "..."
$upgrades = MONITOR_PROGRAM_UPGRADES(program: $program_id)
$malicious = DETECT_MALICIOUS_UPGRADES(upgrades: $upgrades, check_for: ["drain_functions", "backdoors", "authority_changes"])

**Action:**
RETURN {investigation: "Malicious Program Upgrade", malicious_upgrade_detected: COUNT($malicious.suspicious_upgrades) > 0, upgrade_type: $malicious.suspicious_upgrades[0].malicious_changes, funds_at_risk_sol: $malicious.tvl_at_upgrade, confidence: 87}


## Q751: "Identify wallets that consistently front-run trades on a specific DEX."

**Expected Plan:**
[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, FILTER, DETECT_PATTERNS

**Main Branch:**
$dex_program_id = "9xQeWvG816bUx9EPjHmaT23yvVM2ZWbrrpZb9PusVFin" // Serum DEX example
$signatures = getSignaturesForAddress(address: $dex_program_id, limit: 1000)
$transactions = MAP(collection: $signatures, fn: s => getTransaction(s.signature))

// Detect front-running patterns
$front_run_patterns = DETECT_PATTERNS(data: $transactions, pattern: "front_run")

// Extract front-runner wallets
$front_runners = MAP(collection: $front_run_patterns, fn: p => p.front_runner_wallet)
$unique_front_runners = UNIQUE($front_runners)

**Action:**
RETURN {
  front_runner_wallets: $unique_front_runners,
  confidence: 85,
  sample_size: COUNT($transactions)
}

## Q752: "Find the original funding source for a known scam wallet."

**Expected Plan:**
[TIME: ~5m] [COST: ~0.05 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: TRACE_FUNDS

**Main Branch:**
$scam_wallet = "..."
$trace = TRACE_FUNDS(start_address: $scam_wallet, depth: 10, direction: "inbound")

// The first transaction in the inbound trace is the origin
$funding_source = $trace.transactions.first().signer

**Action:**
RETURN {
  scam_wallet: $scam_wallet,
  original_funding_source: $funding_source,
  trace: $trace,
  confidence: 90
}

## Q753: "List all tokens held by a wallet that are not on any major token list."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.02 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: getTokenAccountsByOwner, SEARCH_WEB, MAP, FILTER

**Main Branch:**
$wallet_address = "..."
$token_accounts = getTokenAccountsByOwner(owner: $wallet_address)
$token_mints = MAP(collection: $token_accounts, fn: acc => acc.mint)

// Check each mint against a known list (simulated via web search)
$major_token_lists = SEARCH_WEB(query: "Solana major token lists")
$unlisted_tokens = FILTER(collection: $token_mints, predicate: mint => {
  RETURN !IS_IN_LIST(mint, $major_token_lists)
})

**Action:**
RETURN {
  wallet: $wallet_address,
  unlisted_tokens: $unlisted_tokens,
  confidence: 95
}

## Q754: "Detect if a token has a 'honeypot' mechanism where tokens can be bought but not sold."

**Expected Plan:**
[TIME: ~8m] [COST: ~0.1 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: simulateTransaction, getTransaction, MAP, FILTER

**Main Branch:**
$token_mint = "..."
$test_wallet = create_temp_wallet()

// Simulate a buy transaction
$buy_tx = createSwapTx(from: "SOL", to: $token_mint, amount: 0.1, wallet: $test_wallet)
$sim_buy = simulateTransaction($buy_tx)

GUARD $sim_buy.err == null ELSE RETURN { verdict: "Cannot even buy the token." }

// Simulate a sell transaction
$sell_tx = createSwapTx(from: $token_mint, to: "SOL", amount: "all", wallet: $test_wallet)
$sim_sell = simulateTransaction($sell_tx)

**Decision Point:** Analyze simulation results
  BRANCH A ($sim_buy.err == null && $sim_sell.err != null):
    $verdict = "Honeypot detected. Sell transaction failed."
    $reason = $sim_sell.err
  BRANCH B ($sim_buy.err == null && $sim_sell.err == null):
    $verdict = "Not a honeypot. Buy and sell simulations were successful."
  BRANCH C (default):
    $verdict = "Analysis inconclusive."
    $reason = "Could not simulate buy or other error."

**Action:**
RETURN {
  token_mint: $token_mint,
  verdict: $verdict,
  reason: $reason,
  confidence: 80
}

## Q755: "Find accounts that received tokens from multiple different airdrops."

**Expected Plan:**
[TIME: ~15m] [COST: ~0.2 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, FLATTEN, GROUP_BY

**Main Branch:**
$airdrop_contracts = ["...", "...", "..."] // List of known airdrop contracts

$all_recipients = MAP(collection: $airdrop_contracts, fn: contract => {
  $signatures = getSignaturesForAddress(address: contract, limit: 1000)
  $txs = MAP(collection: $signatures, fn: s => getTransaction(s.signature))
  RETURN MAP($txs, tx => extractAirdropRecipient(tx))
})

$flat_recipients = FLATTEN($all_recipients)
$grouped_recipients = GROUP_BY(collection: $flat_recipients, key: r => r)

$multi_airdrop_hunters = FILTER(collection: $grouped_recipients, predicate: group => COUNT(group.items) > 1)

**Action:**
RETURN {
  airdrop_hunters: MAP($multi_airdrop_hunters, g => g.key),
  count: COUNT($multi_airdrop_hunters),
  confidence: 85
}

## Q756: "Analyze the distribution of a token's supply across its top 100 holders."

**Expected Plan:**
[TIME: ~2m] [COST: ~0.01 SOL] [CONFIDENCE: 99%]

**Available Tools:**
From Standard Library: getTokenLargestAccounts, getTokenSupply

**Main Branch:**
$token_mint = "..."
$top_100_holders = getTokenLargestAccounts(mint: $token_mint, limit: 100)
$total_supply = getTokenSupply(mint: $token_mint).uiAmount

$top_100_supply = SUM(MAP($top_100_holders, h => h.uiAmount))
$percentage_held_by_top_100 = ($top_100_supply / $total_supply) * 100

**Action:**
RETURN {
  token_mint: $token_mint,
  total_supply: $total_supply,
  top_100_holders_supply: $top_100_supply,
  percentage_held_by_top_100: $percentage_held_by_top_100,
  top_holders: $top_100_holders,
  confidence: 99
}

## Q757: "Identify if a wallet has interacted with any known malicious dApps or contracts."

**Expected Plan:**
[TIME: ~5m] [COST: ~0.05 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, SEARCH_WEB, MAP, FILTER

**Main Branch:**
$wallet_address = "..."
$known_malicious_list = SEARCH_WEB(query: "list of known malicious solana programs")

$signatures = getSignaturesForAddress(address: $wallet_address, limit: 100)
$transactions = MAP(collection: $signatures, fn: s => getTransaction(s.signature))

$interacted_programs = FLATTEN(MAP($transactions, tx => tx.instructions.programId))
$unique_interacted = UNIQUE($interacted_programs)

$malicious_interactions = FILTER(collection: $unique_interacted, predicate: program => {
  RETURN IS_IN_LIST(program, $known_malicious_list)
})

**Action:**
RETURN {
  wallet: $wallet_address,
  has_interacted_with_malicious: COUNT($malicious_interactions) > 0,
  malicious_programs_interacted_with: $malicious_interactions,
  confidence: 80,
  caveats: ["Depends on the completeness of the malicious programs list."]
}

## Q758: "Find the transaction where a specific NFT was minted."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, FIND

**Main Branch:**
$nft_mint_address = "..."
// The mint address is an account, so we look for signatures for it.
$signatures = getSignaturesForAddress(address: $nft_mint_address, limit: 100)

// The minting transaction is usually the first one.
$first_tx_sig = $signatures.last().signature
$mint_tx = getTransaction($first_tx_sig)

// Verify it's a minting instruction
$mint_instruction = FIND(collection: $mint_tx.instructions, predicate: ix => isMintToInstruction(ix))

**Decision Point:** Check if it's the correct mint tx
  BRANCH A ($mint_instruction != null):
    $result = $mint_tx
  BRANCH B (default):
    // This would require a more complex search if the first tx isn't it
    $result = "Could not definitively find mint transaction in first 100 signatures."

**Action:**
RETURN {
  nft_mint: $nft_mint_address,
  mint_transaction: $result,
  confidence: 95
}

## Q759: "Calculate the total fees paid by a wallet in a given time period."

**Expected Plan:**
[TIME: ~7m] [COST: ~0.1 SOL] [CONFIDENCE: 98%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, SUM

**Main Branch:**
$wallet_address = "..."
// This might require pagination for an active wallet
$signatures = getSignaturesForAddress(address: $wallet_address, limit: 1000)
$transactions = MAP(collection: $signatures, fn: s => getTransaction(s.signature))

$fees = MAP(collection: $transactions, fn: tx => tx.meta.fee)
$total_fees_lamports = SUM($fees)
$total_fees_sol = $total_fees_lamports / 1000000000

**Action:**
RETURN {
  wallet: $wallet_address,
  transaction_count: COUNT($transactions),
  total_fees_lamports: $total_fees_lamports,
  total_fees_sol: $total_fees_sol,
  confidence: 98
}

## Q760: "Check if a program is upgradeable and who the upgrade authority is."

**Expected Plan:**
[TIME: ~1m] [COST: ~0.01 SOL] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library: getAccountInfo

**Main Branch:**
$program_id = "..."
$account_info = getAccountInfo($program_id)

// For an upgradeable program, the owner is the BPF Upgradeable Loader
$bpf_loader_upgradeable = "BPFLoaderUpgradeab1e11111111111111111111111"
$is_upgradeable = $account_info.owner == $bpf_loader_upgradeable

$upgrade_authority = null
IF $is_upgradeable THEN
  // The program data account holds the upgrade authority
  $program_data_address = findProgramDataAddress($program_id)
  $program_data_info = getAccountInfo($program_data_address)
  $upgrade_authority = parseUpgradeAuthority($program_data_info.data)

**Action:**
RETURN {
  program_id: $program_id,
  is_upgradeable: $is_upgradeable,
  upgrade_authority: $upgrade_authority,
  confidence: 100
}
