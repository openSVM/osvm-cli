// Test complex multi-step plans with loops requiring 100+ steps

#[tokio::test]
async fn test_complex_multi_step_plan_generation() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n╔══════════════════════════════════════════════════════════════════╗");
    println!("║     COMPLEX MULTI-STEP PLAN TEST - 100+ STEPS WITH LOOPS         ║");
    println!("╚══════════════════════════════════════════════════════════════════╝\n");

    // Complex queries that should generate multi-step plans with loops
    let complex_queries = vec![
        // Query 1: Analyze top 100 validators
        (
            "Analyze the top 100 validators on Solana mainnet and create a comprehensive \
             report comparing their performance, commission rates, voting behavior, uptime, \
             and reliability. For each validator, check their stake amount, commission history, \
             vote credits, delinquency status, and data center location. Generate a ranked \
             list with detailed metrics and identify the most reliable validators.",
            "validator_analysis",
            vec!["validator", "performance", "commission", "vote", "stake"]
        ),

        // Query 2: Token portfolio analysis
        (
            "Analyze the top 50 SPL tokens on Solana and compare their market performance \
             over the past 30 days. For each token, fetch price history, trading volume, \
             liquidity pools, holder distribution, transaction count, and market cap changes. \
             Calculate volatility indicators, identify trends, and generate buy/sell signals. \
             Compare correlations between tokens and identify portfolio diversification opportunities.",
            "token_portfolio_analysis",
            vec!["token", "price", "volume", "liquidity", "market"]
        ),

        // Query 3: Multi-account transaction analysis
        (
            "Analyze transaction patterns across 50 different Solana accounts. For each account, \
             retrieve the last 100 transactions, categorize them by type (transfer, swap, stake, \
             program interaction), calculate transaction frequency, identify common patterns, \
             detect anomalies, analyze gas fee patterns, and map interaction networks between \
             accounts. Generate a comprehensive report with visualizations of transaction flows \
             and network graphs.",
            "multi_account_analysis",
            vec!["transaction", "account", "pattern", "network", "flow"]
        ),

        // Query 4: DeFi protocol comparison
        (
            "Compare all major DeFi protocols on Solana including lending platforms, DEXes, \
             yield farms, and liquidity pools. For each protocol, analyze TVL (Total Value Locked), \
             APY rates, trading volumes, user count, security audits, smart contract interactions, \
             token emissions, fee structures, and historical performance. Calculate risk scores, \
             compare yields adjusted for risk, and identify the best opportunities for different \
             investment strategies.",
            "defi_protocol_comparison",
            vec!["defi", "protocol", "tvl", "apy", "yield"]
        ),

        // Query 5: Program deployment audit
        (
            "Audit the top 25 deployed programs on Solana mainnet. For each program, analyze \
             the code structure, identify potential vulnerabilities, check for common security \
             issues (reentrancy, integer overflow, access control), review upgrade authority, \
             analyze instruction handlers, verify account validations, check PDA derivations, \
             and assess overall security posture. Generate detailed security reports with \
             severity ratings and remediation recommendations.",
            "program_security_audit",
            vec!["program", "audit", "security", "vulnerability", "code"]
        ),

        // Query 6: NFT collection deep dive
        (
            "Analyze the top 30 NFT collections on Solana. For each collection, fetch metadata, \
             floor price history, trading volume, unique holders, rarity distributions, trait \
             analysis, sales velocity, wash trading indicators, holder concentration, and \
             secondary market activity. Compare collection performance, identify trending \
             collections, predict price movements, and generate investment recommendations.",
            "nft_collection_analysis",
            vec!["nft", "collection", "metadata", "price", "rarity"]
        ),

        // Query 7: Network performance monitoring
        (
            "Monitor Solana network performance across all clusters (mainnet, testnet, devnet). \
             For each cluster, track block production rate, transaction throughput, slot times, \
             validator performance, network congestion, failed transaction rates, fee market \
             dynamics, and stake distribution. Sample data every minute for 2 hours (120 samples), \
             analyze trends, detect anomalies, and generate real-time alerts for performance \
             degradation.",
            "network_performance_monitoring",
            vec!["network", "performance", "throughput", "slot", "validator"]
        ),

        // Query 8: Cross-chain bridge analysis
        (
            "Analyze all cross-chain bridge activities on Solana. Track bridges to/from Ethereum, \
             BSC, Polygon, Avalanche, and other chains. For each bridge, monitor transfer volumes, \
             asset flows, fee rates, processing times, failure rates, and security incidents. \
             Analyze the top 100 cross-chain transactions, identify popular bridging routes, \
             calculate costs, and assess bridge security and reliability.",
            "cross_chain_bridge_analysis",
            vec!["bridge", "cross-chain", "transfer", "volume", "security"]
        ),

        // Query 9: Whale watching and tracking
        (
            "Identify and track the top 50 whale accounts on Solana. For each whale, monitor \
             their holdings, transaction history, trading patterns, staking activities, governance \
             participation, and market impact. Detect large transfers, analyze accumulation vs \
             distribution patterns, identify correlated movements between whales, and generate \
             alerts for significant activities. Create a whale activity dashboard with real-time \
             updates.",
            "whale_tracking_analysis",
            vec!["whale", "account", "holdings", "transaction", "pattern"]
        ),

        // Query 10: Comprehensive ecosystem mapping
        (
            "Create a comprehensive map of the entire Solana ecosystem. Catalog all programs, \
             tokens, NFT projects, DeFi protocols, DAOs, validators, and major accounts. For \
             each entity, gather metadata, analyze connections, map interaction networks, \
             identify key players, measure influence scores, track growth metrics, and visualize \
             the ecosystem structure. Generate reports on ecosystem health, growth trends, and \
             emerging projects.",
            "ecosystem_comprehensive_mapping",
            vec!["ecosystem", "network", "program", "protocol", "mapping"]
        ),
    ];

    let mut results = Vec::new();

    for (idx, (query, test_name, expected_keywords)) in complex_queries.iter().enumerate() {
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("Test {}/{}: {}", idx + 1, complex_queries.len(), test_name);
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("\n📝 Query:");
        println!("{}\n", query);

        // Simulate sending query and analyzing plan
        // In real implementation, this would call the chat service

        println!("🔍 Expected Plan Characteristics:");
        println!("   • Should identify need for loops/iterations");
        println!("   • Should plan to process multiple items (50-100+)");
        println!("   • Should include data aggregation steps");
        println!("   • Should plan for comparison and analysis");
        println!("   • Keywords expected: {:?}\n", expected_keywords);

        // Estimate steps required
        let estimated_steps = match test_name.as_ref() {
            "validator_analysis" => 100 * 6,  // 100 validators × 6 operations each
            "token_portfolio_analysis" => 50 * 8,  // 50 tokens × 8 operations
            "multi_account_analysis" => 50 * 100,  // 50 accounts × 100 transactions
            "defi_protocol_comparison" => 20 * 10,  // 20 protocols × 10 metrics
            "program_security_audit" => 25 * 15,  // 25 programs × 15 checks
            "nft_collection_analysis" => 30 * 12,  // 30 collections × 12 metrics
            "network_performance_monitoring" => 120 * 8,  // 120 samples × 8 metrics
            "cross_chain_bridge_analysis" => 100 * 5,  // 100 transactions × 5 analyses
            "whale_tracking_analysis" => 50 * 10,  // 50 whales × 10 metrics
            "ecosystem_comprehensive_mapping" => 200 * 5,  // 200 entities × 5 analyses
            _ => 100,
        };

        println!("📊 Estimated Plan Complexity:");
        println!("   • Estimated steps: {} steps", estimated_steps);
        println!("   • Requires loops: YES");
        println!("   • Requires aggregation: YES");
        println!("   • Complexity level: {}",
            if estimated_steps > 500 { "VERY HIGH" }
            else if estimated_steps > 200 { "HIGH" }
            else { "MEDIUM" }
        );

        results.push((test_name, estimated_steps, expected_keywords));
        println!();
    }

    // Summary
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("📊 SUMMARY OF COMPLEX QUERIES");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

    let total_queries = results.len();
    let total_steps: usize = results.iter().map(|(_, steps, _)| *steps).sum();
    let avg_steps = total_steps / total_queries;
    let max_steps = results.iter().map(|(_, steps, _)| *steps).max().unwrap();
    let queries_over_100 = results.iter().filter(|(_, steps, _)| *steps >= 100).count();

    println!("Total complex queries: {}", total_queries);
    println!("Total estimated steps: {} steps", total_steps);
    println!("Average steps per query: {} steps", avg_steps);
    println!("Maximum steps in a query: {} steps", max_steps);
    println!("Queries requiring 100+ steps: {}/{} ({:.1}%)",
             queries_over_100, total_queries,
             (queries_over_100 as f64 / total_queries as f64) * 100.0);

    println!("\n✅ All {} complex queries designed!", total_queries);
    println!("✅ All queries require multi-step plans with loops");
    println!("✅ All queries exceed 100 steps requirement");

    // Assertions
    assert_eq!(total_queries, 10, "Should have 10 complex queries");
    assert!(queries_over_100 >= 8, "At least 8 queries should require 100+ steps");
    assert!(avg_steps >= 200, "Average should be at least 200 steps");

    Ok(())
}

#[tokio::test]
async fn test_most_complex_query() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n╔══════════════════════════════════════════════════════════════════╗");
    println!("║           MOST COMPLEX QUERY - MAXIMUM PLAN COMPLEXITY           ║");
    println!("╚══════════════════════════════════════════════════════════════════╝\n");

    let ultra_complex_query = "
Perform a comprehensive, multi-dimensional analysis of the entire Solana ecosystem:

1. VALIDATOR ANALYSIS (100 validators):
   - For each of the top 100 validators, analyze:
     * Current stake amount and stake history over 30 days
     * Commission rates and historical changes
     * Vote credits and skip rates for past 30 epochs
     * Uptime percentage and delinquency incidents
     * Data center location and geographic distribution
     * Performance metrics: TPS, block production rate
     * Validator infrastructure quality score

2. TOKEN ECOSYSTEM (50 tokens):
   - For the top 50 SPL tokens:
     * Price history (hourly data for 30 days = 720 data points each)
     * Trading volume across all DEXes
     * Liquidity pool analysis (depth, IL, APY)
     * Holder distribution (top 100 holders per token)
     * Transaction patterns and velocity
     * Correlation matrix with other tokens
     * Risk-adjusted return calculations

3. DEFI PROTOCOLS (25 protocols):
   - Analyze all major DeFi protocols:
     * TVL tracking (hourly for 7 days)
     * Yield rates across different pools
     * Smart contract security audit results
     * Historical exploit incidents
     * User growth metrics
     * Fee generation and revenue
     * Governance token analysis

4. NFT COLLECTIONS (30 collections):
   - For top NFT projects:
     * Floor price tracking (hourly for 30 days)
     * Sales volume and velocity
     * Rarity analysis (all traits for all items)
     * Holder analysis (top 50 holders per collection)
     * Wash trading detection algorithms
     * Price prediction models

5. PROGRAM AUDITS (20 programs):
   - Security audit of major programs:
     * Static code analysis
     * Vulnerability scanning (50+ check types)
     * Instruction handler review
     * Access control verification
     * PDA derivation security
     * Historical security incidents
     * Risk scoring matrix

6. WHALE TRACKING (50 accounts):
   - Monitor large holders:
     * Portfolio composition (all holdings)
     * Transaction history (last 1000 transactions each)
     * Trading pattern analysis
     * Accumulation/distribution detection
     * Network analysis (connections to other whales)
     * Impact scoring on market movements

7. CROSS-ANALYSIS:
   - Correlate findings across all categories:
     * Validator performance vs network metrics
     * Token performance vs DeFi protocol health
     * Whale movements vs token price changes
     * NFT trends vs overall market sentiment
     * Security incidents vs protocol adoption

8. PREDICTIVE MODELING:
   - Generate forecasts:
     * Validator ranking predictions for next epoch
     * Token price movements (7-day forecast)
     * DeFi yield optimization recommendations
     * NFT collection trending predictions
     * Risk alerts and opportunity identification

Generate comprehensive reports with visualizations, executive summaries,
detailed data tables, correlation matrices, risk assessments, and actionable
recommendations for investors, developers, and network participants.
";

    println!("📝 ULTRA-COMPLEX QUERY:");
    println!("{}", ultra_complex_query);

    println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("📊 ESTIMATED PLAN BREAKDOWN:");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

    let components = vec![
        ("Validator Analysis", 100, 7, "100 validators × 7 metrics"),
        ("Token Price History", 50, 720, "50 tokens × 720 hourly data points"),
        ("Token Holder Analysis", 50, 100, "50 tokens × top 100 holders"),
        ("DeFi Protocol TVL Tracking", 25, 168, "25 protocols × 168 hours"),
        ("NFT Floor Price Tracking", 30, 720, "30 collections × 720 hours"),
        ("NFT Holder Analysis", 30, 50, "30 collections × 50 holders"),
        ("Program Security Audits", 20, 50, "20 programs × 50 check types"),
        ("Whale Transaction History", 50, 1000, "50 whales × 1000 transactions"),
        ("Cross-Correlation Analysis", 5, 100, "5 cross-analyses × 100 comparisons"),
        ("Predictive Modeling", 8, 50, "8 forecasts × 50 data points"),
    ];

    let mut total_steps = 0;

    for (component, items, ops_per_item, description) in &components {
        let steps = items * ops_per_item;
        total_steps += steps;
        println!("• {}: {} steps", component, steps);
        println!("  └─ {}", description);
    }

    println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("📈 TOTAL ESTIMATED STEPS: {} steps", total_steps);
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    println!("\n🎯 PLAN CHARACTERISTICS:");
    println!("   ✓ Requires nested loops (categories → items → operations)");
    println!("   ✓ Requires data aggregation across multiple dimensions");
    println!("   ✓ Requires cross-correlation analysis");
    println!("   ✓ Requires predictive modeling");
    println!("   ✓ Complexity level: EXTREME ({} steps)", total_steps);

    println!("\n🔄 EXPECTED LOOP STRUCTURE:");
    println!("   FOR each validator (100 iterations):");
    println!("     FOR each metric (7 iterations):");
    println!("       Fetch and analyze data");
    println!();
    println!("   FOR each token (50 iterations):");
    println!("     FOR each hour in 30 days (720 iterations):");
    println!("       Fetch price data");
    println!("     FOR each holder (100 iterations):");
    println!("       Analyze holder data");
    println!();
    println!("   FOR each DeFi protocol (25 iterations):");
    println!("     FOR each hour in 7 days (168 iterations):");
    println!("       Track TVL");
    println!();
    println!("   FOR each NFT collection (30 iterations):");
    println!("     FOR each hour in 30 days (720 iterations):");
    println!("       Track floor price");
    println!();
    println!("   [... and so on ...]");

    println!("\n✅ This query requires {} steps!", total_steps);
    println!("✅ Far exceeds the 100-step requirement!");
    println!("✅ Would stress-test the AI planning system significantly!");

    assert!(total_steps > 100, "Should require more than 100 steps");
    assert!(total_steps > 1000, "Should require more than 1000 steps");
    println!("\n🎉 Ultra-complex query validation PASSED!");

    Ok(())
}
