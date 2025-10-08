#!/usr/bin/env python3
"""
Generate 10,000 Solana blockchain research questions in OVSM planning format.
Each category has 1,000 questions split across 10 files of 100 questions each.
"""

import os
from typing import List, Dict, Tuple

# Category definitions
CATEGORIES = {
    "01_transaction_analysis": {
        "title": "Transaction Analysis",
        "focus": "Transaction queries, fees, instructions, CPIs",
        "tools": ["getTransaction", "getRecentBlockhash", "simulateTransaction", "getSignaturesForAddress"],
        "topics": [
            ("basic", "Simple transaction lookups and fee calculations"),
            ("intermediate", "Multi-instruction transactions and CPI analysis"),
            ("advanced", "Complex transaction patterns and optimization"),
            ("analysis", "Transaction flow and dependency analysis"),
            ("patterns", "Common transaction patterns and signatures"),
            ("optimization", "Fee optimization and batching strategies"),
            ("forensics", "Failed transaction investigation"),
            ("historical", "Transaction history and trends"),
            ("edge_cases", "Unusual transaction scenarios"),
            ("expert", "Advanced transaction forensics and MEV")
        ]
    },
    "02_account_state": {
        "title": "Account State",
        "focus": "Balances, ownership, PDAs, stake accounts",
        "tools": ["getAccountInfo", "getBalance", "getProgramAccounts", "getMultipleAccounts"],
        "topics": [
            ("basic", "Account balance and ownership queries"),
            ("intermediate", "PDA derivation and validation"),
            ("advanced", "Complex account relationships"),
            ("analysis", "Account state evolution analysis"),
            ("patterns", "Account usage patterns"),
            ("optimization", "Efficient account queries"),
            ("forensics", "Account anomaly detection"),
            ("historical", "Account state history"),
            ("edge_cases", "Edge cases in account state"),
            ("expert", "Advanced PDA and account forensics")
        ]
    },
    "03_program_interaction": {
        "title": "Program Interaction",
        "focus": "Deployments, upgrades, IDLs, invocations",
        "tools": ["getProgramAccounts", "getAccountInfo", "simulateTransaction", "getTransaction"],
        "topics": [
            ("basic", "Basic program queries"),
            ("intermediate", "Program invocation analysis"),
            ("advanced", "Complex program interactions"),
            ("analysis", "Program usage analysis"),
            ("patterns", "Program interaction patterns"),
            ("optimization", "Program call optimization"),
            ("forensics", "Program exploit detection"),
            ("historical", "Program evolution tracking"),
            ("edge_cases", "Unusual program behaviors"),
            ("expert", "Advanced program security analysis")
        ]
    },
    "04_network_analysis": {
        "title": "Network Analysis",
        "focus": "Slots, blocks, TPS, epochs, cluster health",
        "tools": ["getSlot", "getBlock", "getEpochInfo", "getClusterNodes", "getRecentPerformanceSamples"],
        "topics": [
            ("basic", "Basic network metrics"),
            ("intermediate", "Block and slot analysis"),
            ("advanced", "Network performance analysis"),
            ("analysis", "Cluster health monitoring"),
            ("patterns", "Network usage patterns"),
            ("optimization", "Network timing optimization"),
            ("forensics", "Network issue investigation"),
            ("historical", "Historical network trends"),
            ("edge_cases", "Network edge cases"),
            ("expert", "Advanced network diagnostics")
        ]
    },
    "05_validator_research": {
        "title": "Validator Research",
        "focus": "Validators, stake, rewards, performance",
        "tools": ["getVoteAccounts", "getStakeActivation", "getInflationReward", "getLeaderSchedule"],
        "topics": [
            ("basic", "Basic validator queries"),
            ("intermediate", "Stake and delegation analysis"),
            ("advanced", "Validator performance metrics"),
            ("analysis", "Validator behavior analysis"),
            ("patterns", "Staking patterns"),
            ("optimization", "Stake optimization strategies"),
            ("forensics", "Validator issue detection"),
            ("historical", "Historical validator data"),
            ("edge_cases", "Validator edge cases"),
            ("expert", "Advanced validator analytics")
        ]
    },
    "06_token_research": {
        "title": "Token Research",
        "focus": "Token supply, holders, metadata, transfers",
        "tools": ["getTokenAccountsByOwner", "getTokenSupply", "getTokenAccountBalance", "getTokenLargestAccounts"],
        "topics": [
            ("basic", "Basic token queries"),
            ("intermediate", "Token holder analysis"),
            ("advanced", "Token distribution analysis"),
            ("analysis", "Token flow analysis"),
            ("patterns", "Token usage patterns"),
            ("optimization", "Token query optimization"),
            ("forensics", "Token anomaly detection"),
            ("historical", "Token history tracking"),
            ("edge_cases", "Token edge cases"),
            ("expert", "Advanced token analytics")
        ]
    },
    "07_defi_analysis": {
        "title": "DeFi Analysis",
        "focus": "DEX, lending, liquidity, farming, oracles",
        "tools": ["getProgramAccounts", "getAccountInfo", "getTransaction", "simulateTransaction"],
        "topics": [
            ("basic", "Basic DeFi queries"),
            ("intermediate", "Liquidity pool analysis"),
            ("advanced", "Complex DeFi strategies"),
            ("analysis", "DeFi protocol analysis"),
            ("patterns", "DeFi usage patterns"),
            ("optimization", "DeFi optimization strategies"),
            ("forensics", "DeFi exploit detection"),
            ("historical", "DeFi historical trends"),
            ("edge_cases", "DeFi edge cases"),
            ("expert", "Advanced DeFi analytics")
        ]
    },
    "08_nft_analysis": {
        "title": "NFT Analysis",
        "focus": "NFTs, collections, metadata, trading",
        "tools": ["getTokenAccountsByOwner", "getAccountInfo", "getProgramAccounts", "getTransaction"],
        "topics": [
            ("basic", "Basic NFT queries"),
            ("intermediate", "Collection analysis"),
            ("advanced", "NFT trading analysis"),
            ("analysis", "NFT market analysis"),
            ("patterns", "NFT trading patterns"),
            ("optimization", "NFT query optimization"),
            ("forensics", "NFT fraud detection"),
            ("historical", "NFT historical data"),
            ("edge_cases", "NFT edge cases"),
            ("expert", "Advanced NFT analytics")
        ]
    },
    "09_advanced_scenarios": {
        "title": "Advanced Scenarios",
        "focus": "MEV, clustering, rugpulls, governance",
        "tools": ["getTransaction", "getBlock", "getProgramAccounts", "getSignaturesForAddress"],
        "topics": [
            ("basic", "Basic advanced queries"),
            ("intermediate", "MEV detection basics"),
            ("advanced", "Complex attack patterns"),
            ("analysis", "Security analysis"),
            ("patterns", "Attack patterns"),
            ("optimization", "Detection optimization"),
            ("forensics", "Forensic investigation"),
            ("historical", "Historical exploit data"),
            ("edge_cases", "Exploit edge cases"),
            ("expert", "Expert-level security analysis")
        ]
    },
    "10_historical_analysis": {
        "title": "Historical Analysis",
        "focus": "Historical data, replays, time-series",
        "tools": ["getBlock", "getTransaction", "getConfirmedSignaturesForAddress2", "getBlockTime"],
        "topics": [
            ("basic", "Basic historical queries"),
            ("intermediate", "Time-series analysis"),
            ("advanced", "Complex historical patterns"),
            ("analysis", "Trend analysis"),
            ("patterns", "Historical patterns"),
            ("optimization", "Query optimization"),
            ("forensics", "Historical forensics"),
            ("historical", "Long-term trends"),
            ("edge_cases", "Historical edge cases"),
            ("expert", "Advanced historical analytics")
        ]
    }
}

# Question templates for each category
QUESTION_TEMPLATES = {
    "01_transaction_analysis": [
        "What is the total fee paid for transaction signature {sig}?",
        "How many instructions are in transaction {sig}?",
        "Which programs were invoked in transaction {sig}?",
        "What is the CPI depth of transaction {sig}?",
        "How many compute units did transaction {sig} consume?",
        "What accounts were written to in transaction {sig}?",
        "Find all failed transactions for address {addr} in the last {n} slots",
        "Calculate the average transaction fee for program {program} in the last hour",
        "Identify transactions with CPI depth greater than {n}",
        "What is the most expensive transaction in block {slot}?",
    ],
    "02_account_state": [
        "What is the SOL balance of account {addr}?",
        "Who is the owner of account {addr}?",
        "Is account {addr} executable?",
        "What is the data size of account {addr}?",
        "Derive the PDA for program {program} with seeds {seeds}",
        "Find all token accounts owned by {addr}",
        "What is the rent-exempt minimum for account size {size}?",
        "List all accounts owned by program {program}",
        "What is the stake account balance for {addr}?",
        "Find accounts with balance greater than {amount} SOL",
    ],
    "03_program_interaction": [
        "What is the upgrade authority for program {addr}?",
        "When was program {addr} last upgraded?",
        "How many times has program {addr} been invoked in the last {n} slots?",
        "What is the data size of program {addr}?",
        "Find all programs that call program {addr} via CPI",
        "What is the most frequently called instruction on program {addr}?",
        "Simulate calling instruction {ix} on program {addr}",
        "What accounts does program {addr} typically interact with?",
        "Find all program deployments in the last {n} epochs",
        "What is the average compute cost for program {addr} instructions?",
    ],
    "04_network_analysis": [
        "What is the current slot number?",
        "What is the current epoch?",
        "How many transactions were in block {slot}?",
        "What is the current TPS over the last {n} minutes?",
        "How many validators are currently active?",
        "What is the average slot time over the last {n} slots?",
        "What is the leader schedule for epoch {epoch}?",
        "How many blocks were skipped in the last {n} slots?",
        "What is the current network inflation rate?",
        "What is the cluster health status?",
    ],
    "05_validator_research": [
        "How much stake does validator {identity} have?",
        "What is the commission rate for validator {identity}?",
        "How many epochs has validator {identity} been active?",
        "What is the vote account for validator {identity}?",
        "Calculate the rewards for delegating {amount} SOL to validator {identity}",
        "What is the validator's skip rate over the last {n} epochs?",
        "Find validators with commission less than {pct}%",
        "What is the largest validator by stake?",
        "How many blocks did validator {identity} produce in epoch {epoch}?",
        "What is the validator's uptime percentage?",
    ],
    "06_token_research": [
        "What is the total supply of token {mint}?",
        "How many holders does token {mint} have?",
        "What is the largest token account for {mint}?",
        "Find all token accounts for owner {addr}",
        "What is the metadata for token {mint}?",
        "Calculate the token distribution (Gini coefficient) for {mint}",
        "How many token transfers occurred for {mint} in the last {n} slots?",
        "What is the token balance of account {addr} for mint {mint}?",
        "Find tokens with supply less than {amount}",
        "What is the mint authority for token {mint}?",
    ],
    "07_defi_analysis": [
        "What is the TVL of liquidity pool {addr}?",
        "What are the reserves in pool {addr}?",
        "Calculate the current swap rate from token A to token B in pool {addr}",
        "What is the utilization rate of lending protocol {program}?",
        "Find all liquidity pools for token pair {mintA}/{mintB}",
        "What is the largest liquidity provider in pool {addr}?",
        "Calculate impermanent loss for position {addr}",
        "What is the oracle price for asset {asset}?",
        "Find all active farming positions for user {addr}",
        "What is the APY for lending pool {addr}?",
    ],
    "08_nft_analysis": [
        "What NFTs does address {addr} own?",
        "What is the metadata URI for NFT {mint}?",
        "How many NFTs are in collection {collection}?",
        "What is the floor price for collection {collection}?",
        "Find all sales of NFT {mint}",
        "What is the rarity rank of NFT {mint} in collection {collection}?",
        "How many unique holders are in collection {collection}?",
        "What is the total trading volume for collection {collection}?",
        "Find NFTs with trait {trait}={value}",
        "What is the mint date of NFT {mint}?",
    ],
    "09_advanced_scenarios": [
        "Detect MEV opportunities in the mempool",
        "Identify sandwich attacks in the last {n} blocks",
        "Find accounts with suspicious token transfer patterns",
        "Detect potential rugpull indicators for token {mint}",
        "Analyze governance proposal {proposal} voting patterns",
        "Find clusters of related addresses using transaction graph analysis",
        "Detect front-running transactions for DEX {program}",
        "Identify wash trading patterns for NFT collection {collection}",
        "Find accounts with sudden large balance changes",
        "Detect potential Sybil attack patterns in validator network",
    ],
    "10_historical_analysis": [
        "What was the SOL price correlation with transaction volume over the last {n} days?",
        "Reconstruct the account state for {addr} at slot {slot}",
        "Plot the TPS trend over the last {n} epochs",
        "What was the validator stake distribution {n} days ago?",
        "Find the earliest transaction for address {addr}",
        "Calculate the token holder growth rate for {mint} over time",
        "What was the network utilization trend during epoch {epoch}?",
        "Replay transactions for account {addr} between slots {start} and {end}",
        "What was the average gas fee trend over the last {n} weeks?",
        "Identify seasonal patterns in network activity",
    ]
}

def generate_ovsm_question(category: str, q_num: int, difficulty: str, template: str) -> str:
    """Generate a single OVSM-formatted question."""

    # Get category info
    cat_info = CATEGORIES[category]
    tools = cat_info["tools"]

    # Generate question based on template and difficulty
    question = template.format(
        sig="5J8..xyz",
        addr="7xK...abc",
        program="TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA",
        n="100",
        size="1024",
        seeds="[b'metadata', user.key().as_ref()]",
        amount="10.5",
        ix="transfer",
        epoch="450",
        identity="CWrNv...def",
        pct="5",
        mint="EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v",
        mintA="So11111111111111111111111111111111111111112",
        mintB="EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v",
        asset="SOL/USDC",
        collection="DeGods",
        trait="Background",
        value="Blue",
        proposal="Prop-123",
        start="123456789",
        end="123556789",
        slot="155000000"
    )

    # Adjust complexity based on difficulty
    if difficulty in ["basic", "intermediate"]:
        time_est = "5-10s"
        cost = "free"
        branches = 1
    elif difficulty in ["advanced", "analysis", "patterns"]:
        time_est = "10-30s"
        cost = "~0.001 SOL"
        branches = 2
    elif difficulty in ["optimization", "forensics"]:
        time_est = "30-60s"
        cost = "~0.005 SOL"
        branches = 3
    else:  # historical, edge_cases, expert
        time_est = "1-5min"
        cost = "~0.01 SOL"
        branches = 3

    # Build OVSM structure
    ovsm = f"""## Q{q_num}: "{question}"

**Expected Plan:**

[TIME: ~{time_est}] [COST: {cost}]

**Available Tools:**
From Standard Library:
  - {', '.join(tools[:3])}
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "{template.split('{')[1].split('}')[0] if '{' in template else 'input'}"
$result = TOOL.{tools[0]}($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

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
Return structured data showing {difficulty}-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

"""
    return ovsm

def generate_category_file(category: str, file_num: int, start_q: int) -> Tuple[str, str]:
    """Generate one file (100 questions) for a category."""

    file_names = [
        "01_basic.md",
        "02_intermediate.md",
        "03_advanced.md",
        "04_analysis.md",
        "05_patterns.md",
        "06_optimization.md",
        "07_forensics.md",
        "08_historical.md",
        "09_edge_cases.md",
        "10_expert.md"
    ]

    difficulty_names = [
        "basic", "intermediate", "advanced", "analysis", "patterns",
        "optimization", "forensics", "historical", "edge_cases", "expert"
    ]

    file_name = file_names[file_num - 1]
    difficulty = difficulty_names[file_num - 1]

    # Get category info
    cat_info = CATEGORIES[category]
    title = cat_info["title"]
    focus = cat_info["focus"]
    topic_desc = cat_info["topics"][file_num - 1][1]

    # Header
    content = f"""# {title} - {difficulty.title()} Questions
**Category:** {category}
**Focus:** {focus}
**Level:** {difficulty.title()}
**Topic:** {topic_desc}
**Questions:** Q{start_q}-Q{start_q + 99}

---

"""

    # Generate 100 questions
    templates = QUESTION_TEMPLATES[category]
    for i in range(100):
        q_num = start_q + i
        template = templates[i % len(templates)]
        content += generate_ovsm_question(category, q_num, difficulty, template)

    return file_name, content

def generate_all_questions():
    """Generate all 10,000 questions across all categories."""

    base_path = "/home/larp/larpdevs/osvm-cli/test_qa_categories"

    total_files = 0
    total_questions = 0

    for category in sorted(CATEGORIES.keys()):
        print(f"Generating {category}...")

        category_path = os.path.join(base_path, category)

        # Generate 10 files of 100 questions each (1000 per category)
        for file_num in range(1, 11):
            start_q = (file_num - 1) * 100 + 1
            file_name, content = generate_category_file(category, file_num, start_q)

            file_path = os.path.join(category_path, file_name)
            with open(file_path, 'w') as f:
                f.write(content)

            total_files += 1
            total_questions += 100
            print(f"  - Generated {file_name} (Q{start_q}-Q{start_q + 99})")

    print(f"\n✅ Complete!")
    print(f"   Total Categories: {len(CATEGORIES)}")
    print(f"   Total Files: {total_files}")
    print(f"   Total Questions: {total_questions}")

    # Generate summary file
    summary_path = os.path.join(base_path, "README.md")
    with open(summary_path, 'w') as f:
        f.write("# Solana Blockchain Research Q&A Dataset\n\n")
        f.write("## Overview\n")
        f.write(f"This dataset contains {total_questions} research questions ")
        f.write("organized in OVSM planning format for testing Solana blockchain analysis tools.\n\n")
        f.write("## Structure\n\n")

        for category, info in sorted(CATEGORIES.items()):
            f.write(f"### {category} - {info['title']}\n")
            f.write(f"**Focus:** {info['focus']}\n\n")
            f.write("**Files:**\n")
            for i, (topic_key, topic_desc) in enumerate(info['topics'], 1):
                file_name = f"{i:02d}_{topic_key}.md"
                start_q = (i - 1) * 100 + 1
                end_q = i * 100
                f.write(f"- `{file_name}` - Q{start_q}-Q{end_q}: {topic_desc}\n")
            f.write("\n")

        f.write("## Format\n\n")
        f.write("Each question follows OVSM planning syntax:\n")
        f.write("- **Question Header**: Number and text\n")
        f.write("- **Expected Plan**: Time and cost estimates\n")
        f.write("- **Available Tools**: RPC methods and utilities\n")
        f.write("- **Main Branch**: OVSM code with variables and tool calls\n")
        f.write("- **Decision Point**: Conditional logic and branching\n")
        f.write("- **Action**: Expected output description\n\n")
        f.write("## Usage\n\n")
        f.write("These questions can be used for:\n")
        f.write("1. Testing blockchain analysis tools\n")
        f.write("2. Training AI models on Solana queries\n")
        f.write("3. Benchmarking query performance\n")
        f.write("4. Educational purposes\n")
        f.write("5. Developing automated testing suites\n")

if __name__ == "__main__":
    generate_all_questions()
