# Complete Tool Index - All 207 Tools

Quick reference index of all tools in the AI Planning Meta-Language.

---

## Solana RPC Tools (14)

| Tool | Returns | Purpose |
|------|---------|---------|
| getSlot | u64 | Get current slot number |
| getBlock | Block | Fetch block by slot |
| getTransaction | Transaction | Fetch transaction details |
| getSignaturesForAddress | Array | Get transaction signatures for address |
| getAccountInfo | AccountInfo | Fetch account data |
| getBalance | u64 | Get SOL balance |
| getEpochInfo | EpochInfo | Get current epoch information |
| getRecentBlockhash | string | Get recent blockhash |
| getRecentPrioritizationFees | Array | Get priority fee data |
| getVoteAccounts | VoteAccounts | Get validator vote accounts |
| getTokenAccountsByOwner | Array | Get token accounts for owner |
| simulateTransaction | SimulationResult | Simulate transaction execution |
| getRecentPerformanceSamples | Array | Get network performance data |
| getProgramAccounts | Array | Get accounts owned by program |
| getTokenSupply | TokenSupply | Get token supply |
| getTokenLargestAccounts | Array | Get largest token holders |
| getLargestAccounts | Array | Get largest accounts by balance |
| getClusterNodes | Array | Get cluster node information |

## Data Processing Tools (27)

| Tool | Purpose |
|------|---------|
| MAP | Transform each element |
| FILTER | Select elements matching predicate |
| REDUCE | Aggregate to single value |
| SUM | Sum of numeric collection |
| AVG | Average of numbers |
| MAX | Maximum value |
| MIN | Minimum value |
| MEDIAN | Median value |
| SORT | Sort collection |
| COUNT | Count elements |
| UNIQUE | Remove duplicates |
| FLATTEN | Flatten nested arrays |
| ANY | Check if any element matches |
| ALL | Check if all elements match |
| FIND | Find first matching element |
| APPEND | Add element to end |
| PREPEND | Add element to start |
| TOP_N | Get top N elements |
| BOTTOM_N | Get bottom N elements |
| MAX_BY | Find element with max value by key |
| MIN_BY | Find element with min value by key |
| SORT_BY | Sort by key/function |
| GROUP_BY | Group elements by key |
| SLICE | Extract portion of array |
| REVERSE | Reverse array order |
| FIRST | Get first element |
| LAST | Get last element |

## Statistical Tools (14)

| Tool | Purpose |
|------|---------|
| MEAN | Arithmetic mean |
| STDDEV | Standard deviation |
| PERCENTILE | Calculate percentile |
| CORRELATE | Correlation coefficient |
| T_TEST | Student's t-test |
| DETECT_OUTLIERS | Find statistical outliers |
| FIND_PATTERNS | Pattern detection |
| identifyPatterns | Alias for FIND_PATTERNS |
| findCorrelations | Find correlations in datasets |
| calculateConfidence | Calculate confidence score |
| identifyCaveats | Identify analysis limitations |
| calculateRange | Calculate uncertainty range |
| novelty | Calculate pattern novelty score |
| WEIGHTED_AVERAGE | Weighted average calculation |

## Math Tools (8)

| Tool | Purpose |
|------|---------|
| ABS | Absolute value |
| SQRT | Square root |
| POW | Power/exponentiation |
| ROUND | Round to decimals |
| FLOOR | Floor function |
| CEIL | Ceiling function |
| MIN_OF | Minimum of two values |
| MAX_OF | Maximum of two values |

## Solana Utility Tools (13)

| Tool | Purpose |
|------|---------|
| derivePDA | Derive Program Derived Address |
| deriveATA | Derive Associated Token Account |
| parseU64 | Parse u64 from bytes |
| parseU128 | Parse u128 from bytes |
| parseI64 | Parse i64 from bytes |
| parsePubkey | Parse public key from bytes |
| parseString | Parse UTF-8 string |
| JSON_PARSE | Parse JSON string |
| JSON_STRINGIFY | Convert object to JSON |
| borshDeserialize | Deserialize Borsh data |
| anchorDeserialize | Deserialize Anchor data |
| extractPriorityFee | Extract priority fee from tx |
| extractTransactions | Extract transactions from block |
| extractHour | Extract hour from timestamp |
| calculateSeverity | Calculate alert severity |

## String Tools (5)

| Tool | Purpose |
|------|---------|
| UPPERCASE | Convert to uppercase |
| LOWERCASE | Convert to lowercase |
| TRIM | Trim whitespace |
| SPLIT | Split string by delimiter |
| JOIN | Join array to string |

## Utility Tools (11)

| Tool | Purpose |
|------|---------|
| NOW | Get current timestamp |
| SLEEP | Sleep for duration |
| LOG | Log message |
| ASSERT | Assert condition |
| INPUT | Get user input |
| ERROR | Throw error |
| WARN | Warning message |
| TOOL_EXISTS | Check tool availability |
| REQUIRE_TOOL | Require tool exists |
| MEASURE | Measure metric value |
| SCORE | Calculate composite score |

## Analysis Tools (5)

| Tool | Purpose |
|------|---------|
| MODE | Most frequent value |
| EXTRACT | Extract field from objects |
| QUERY | Generic query executor |
| THRESHOLD_EXCEEDED | Check threshold |

## Web/External Tools (4)

| Tool | Purpose |
|------|---------|
| SEARCH_WEB | Web search |
| FETCH_URL | Fetch URL content |
| SEARCH_GITHUB | Search GitHub repositories |
| SEARCH_DOCS | Search documentation |

---

## Agent Extension Tools (103)

### Agent Delegation (6)
SPAWN_AGENT, AWAIT_AGENT, PARALLEL_AGENTS, AGENT_STATUS, KILL_AGENT, MERGE_RESULTS

### Research State Management (5)
INIT_RESEARCH_STATE, UPDATE_EVIDENCE, CALCULATE_CONFIDENCE, CONCLUDE_RESEARCH, SUGGEST_NEXT_STEPS

### Knowledge Graph (7)
INIT_KNOWLEDGE_GRAPH, ADD_NODE, ADD_EDGE, QUERY_GRAPH, FIND_PATH, FIND_CLUSTERS, CENTRALITY

### Hypothesis Testing (4)
DEFINE_HYPOTHESIS, COLLECT_SAMPLES, PERFORM_TEST, EFFECT_SIZE

### Progressive Refinement (4)
GENERATE_SUBQUESTIONS, RECURSIVE_RESEARCH, MERGE_INSIGHTS, BUILD_RESEARCH_TREE

### Cross-Validation (4)
CROSS_VALIDATE, COMPARE_RESULTS, CONSENSUS, INVESTIGATE_DISCREPANCY

### Contextual Memory (6)
INIT_MEMORY, STORE_SHORT_TERM, STORE_LONG_TERM, RECALL, UPDATE_MEMORY, FORGET

### Confidence Scoring (4)
CALCULATE_CONFIDENCE, UNCERTAINTY_RANGE, IDENTIFY_CAVEATS, CONFIDENCE_BREAKDOWN

### Literature Review (10)
INIT_LITERATURE_REVIEW, SEARCH_GITHUB, SEARCH_DOCS, SEARCH_FORUMS, SEARCH_ARXIV, EXTRACT_CONCEPTS, MAP_RELATIONSHIPS, CALCULATE_RELEVANCE, ASSESS_CREDIBILITY, SYNTHESIZE_REVIEW

### Dynamic Planning (6)
ENUMERATE_PLANS, ESTIMATE_PLAN, SCORE_PLAN, SELECT_OPTIMAL_PLAN, EXECUTE_PLAN, SWITCH_PLAN

### Causal Inference (5)
DESIGN_EXPERIMENT, PROPENSITY_SCORE_MATCHING, ESTIMATE_CAUSAL_EFFECT, GRANGER_CAUSALITY_TEST, INSTRUMENTAL_VARIABLE

### Multi-Modal Fusion (8)
ANALYZE_CHAIN, ANALYZE_SOCIAL, ANALYZE_CODE, ANALYZE_PRICE, ALIGN_TIMELINES, CORRELATE_MODALITIES, CALCULATE_TIME_LAGS, EXTRACT_PREDICTIVE_SIGNALS

### Explanation Generation (7)
GENERATE_EXPLANATION, EXTRACT_KEY_CONCEPTS, BUILD_EXPLANATION_TREE, FORMAT_TEXT, EXPLAIN_CONCEPT, GATHER_EVIDENCE, ANNOTATE_WITH_CITATIONS

### Real-Time Monitoring (5)
INIT_MONITOR, CREATE_WATCH, MEASURE_METRIC, CREATE_ALERT, TRIGGER_INVESTIGATION

### Meta-Learning (6)
INIT_META_LEARNER, RECORD_QUERY_PERFORMANCE, LEARN_STRATEGY, LOOKUP_STRATEGY, EXPLORE_STRATEGY, EXECUTE_STRATEGY

### Additional Agent Utilities (26)
GATHER_TRANSACTIONS, GATHER_CONTEXT, RECOMMEND_ACTIONS, QUERY_MEMORY, UPDATE_EXISTING_PATTERN, SWITCH_TO_FASTER_PLAN, SWITCH_TO_CHEAPER_PLAN, EXPLORE_NEW_STRATEGY, USE_STRATEGY, GROUP_BY_THEME, SUMMARIZE_THEME, FIND_CONSENSUS, IDENTIFY_CONFLICTS, CONSTRUCT_EXPLANATION_TREE, EXPLAIN, BASED_ON, CALCULATE_SCORE, CHECK_CONSTRAINTS, CALCULATE_P_VALUE, EXTRACT_ENTITIES, EXTRACT_RELATIONSHIPS, FIND_CENTRAL_NODES, ALIGN_BY_TIMESTAMP, EXECUTE

---

## Total: 207 Tools

**By Type:**
- Solana-specific: 32 tools
- Data processing: 27 tools
- Statistical analysis: 14 tools
- Agent coordination: 103 tools
- Utilities: 16 tools
- Math: 8 tools
- String: 5 tools
- Web/External: 4 tools

**By Usage:**
- Read-only (queries): 95 tools
- Modifies state: 45 tools
- Async operations: 15 tools
- Pure functions: 52 tools
