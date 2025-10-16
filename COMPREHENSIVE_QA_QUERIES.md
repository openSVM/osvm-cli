# Comprehensive QA Test Queries - 100 Diverse Queries

This document contains 100 carefully curated test queries designed to comprehensively test the OSVM CLI chat AI across all categories and difficulty levels.

## Overview

- **Total Queries:** 100
- **Categories:** 9 (Basic, RPC, SVM, Nodes, Deploy, Audit, MCP, OVSM, Chat)
- **Difficulty Levels:** 4 (Easy, Medium, Hard, Expert)
- **Test File:** `tests/chat_ai_comprehensive_qa_test.rs`

## Distribution

### By Category
- **Basic:** 15 queries - Basic commands and system operations
- **RPC:** 15 queries - RPC operations and network queries
- **SVM:** 12 queries - SVM management and multi-chain operations
- **Nodes:** 12 queries - Node deployment and management
- **Deploy:** 10 queries - Program deployment workflows
- **Audit:** 8 queries - Security auditing and vulnerability detection
- **MCP:** 10 queries - Model Context Protocol operations
- **OVSM:** 10 queries - OVSM scripting language
- **Chat:** 8 queries - Chat functionality and interactions

### By Difficulty
- **Easy:** 18 queries - Simple, direct questions
- **Medium:** 44 queries - Multi-step or contextual queries
- **Hard:** 22 queries - Complex or ambiguous queries
- **Expert:** 16 queries - Technical deep-dives and advanced topics

---

## All 100 Test Queries

### BASIC CATEGORY (15 queries)

#### Easy
1. "What version of OSVM am I running?"
2. "Check my SOL balance"
3. "Show me all available commands"
4. "How do I get help with OSVM?"
5. "What's my wallet address?"

#### Medium
6. "Can you show me examples of common workflows?"
7. "What are the main features of OSVM?"
9. "What system requirements do I need to run OSVM?"
10. "How do I update OSVM to the latest version?"
11. "What configuration files does OSVM use?"
12. "Can I use OSVM with multiple keypairs?"
14. "What are the default RPC endpoints OSVM uses?"
15. "Is there a debug mode for verbose logging?"

#### Hard
8. "How is OSVM different from the standard Solana CLI?"
13. "How do I troubleshoot connection issues?"

---

### RPC CATEGORY (15 queries)

#### Easy
16. "Query the Solana network health"
17. "What's the current slot number?"
18. "Get the block height"

#### Medium
19. "How many validators are currently active?"
20. "What's the current epoch and progress?"
21. "Check the transaction count in the latest block"
23. "Get cluster nodes information"
24. "What's the minimum rent-exempt balance for an account?"
26. "How do I query a specific account's data?"
28. "Get transaction history for my wallet"

#### Hard
22. "What are the recent performance samples?"
25. "Show me the inflation rate and rewards"
27. "What's the current recommended commitment level?"

#### Expert
29. "How do I subscribe to real-time slot updates?"
30. "What are the RPC rate limits on public endpoints?"

---

### SVM CATEGORY (12 queries)

#### Easy
31. "List all available SVMs"
32. "What SVMs are currently supported?"
39. "Show me the SVM dashboard"

#### Medium
33. "Show me details about Sonic SVM"
38. "How do I switch between different SVM networks?"
42. "What's the latest version of Sonic SVM?"
43. "How do I monitor SVM health and status?"

#### Hard
34. "How do I deploy a node to Eclipse?"
36. "What's the difference between Solana and Sonic?"
41. "Can I run multiple SVMs simultaneously?"

#### Expert
35. "Compare the features of different SVMs"
40. "What are the performance characteristics of each SVM?"

---

### NODES CATEGORY (12 queries)

#### Easy
44. "List all my validator nodes"
45. "What's the status of my nodes?"
46. "Show me the node dashboard"

#### Medium
48. "Restart my node on mainnet"
49. "Check the logs for my validator"
52. "Show me my node's uptime and performance"
54. "What are the hardware requirements for validators?"
55. "How do I gracefully stop a running node?"

#### Hard
47. "How do I deploy a new validator node?"
50. "What's my validator's vote credit?"
51. "Explain the validator consensus mechanism"

#### Expert
51. "How do I update my node's identity?"
53. "How do I configure node monitoring and alerts?"

---

### DEPLOY CATEGORY (10 queries)

#### Medium
56. "How do I deploy a program to devnet?"
59. "What are the steps to deploy a Solana program?"
62. "How do I verify my program deployment?"
63. "What's the cost of deploying a program?"

#### Hard
57. "Deploy my eBPF program to mainnet"
60. "How do I upgrade an existing program?"
65. "What's the deployment workflow for production?"

#### Expert
61. "Can I deploy to multiple networks at once?"
64. "How do I publish an IDL file with my program?"
65. "Deploy a validator using SSH"

---

### AUDIT CATEGORY (8 queries)

#### Medium
67. "How does the security audit work?"
68. "Can you audit a GitHub repository?"
69. "What security issues should I watch for?"

#### Hard
66. "Audit my Solana program for vulnerabilities"
70. "Generate an audit report for my program"

#### Expert
71. "What are common Solana security vulnerabilities?"
72. "How does AI-powered auditing work?"
73. "Can the audit check for reentrancy attacks?"

---

### MCP CATEGORY (10 queries)

#### Medium
74. "What MCP servers are available?"
75. "How do I add a new MCP server?"
76. "List all MCP tools I can use"
79. "How do I test an MCP server connection?"
82. "Enable the Solana MCP server"

#### Hard
77. "What is the Model Context Protocol?"
80. "Can I add an MCP server from GitHub?"

#### Expert
78. "How do I call a specific MCP tool?"
81. "What's the difference between MCP and RPC?"
83. "How does MCP authentication work?"

---

### OVSM CATEGORY (10 queries)

#### Easy
85. "Show me OVSM examples"
92. "Start the OVSM REPL"

#### Medium
84. "What is OVSM?"
86. "Evaluate this OVSM code: $x = 42; RETURN $x"
87. "How do I write a loop in OVSM?"
89. "What data types does OVSM support?"
90. "How do I use conditional logic in OVSM?"

#### Hard
88. "Run an OVSM script to calculate factorial"
93. "What's the syntax for OVSM functions?"

#### Expert
91. "Can OVSM interact with MCP servers?"

---

### CHAT CATEGORY (8 queries)

#### Easy
94. "Hello, can you help me?"
95. "What can you do?"

#### Medium
96. "Explain how the agent chat works"
97. "How do I use the advanced chat mode?"
99. "What AI model powers this chat?"
100. "How do I create a new chat session?"

#### Hard
98. "Can you remember our previous conversation?"
101. "Explain the difference between basic and advanced chat"

---

## Running the Tests

### Test All 100 Queries (Metadata)
```bash
cargo test --test chat_ai_comprehensive_qa_test test_query_metadata -- --nocapture
```

### Test Difficulty Distribution
```bash
cargo test --test chat_ai_comprehensive_qa_test test_queries_by_difficulty -- --nocapture
```

### Test Random Sample (Default: 10 queries)
```bash
cargo test --test chat_ai_comprehensive_qa_test test_comprehensive_qa_100_queries -- --nocapture
```

### Test Custom Sample Size
```bash
QUERY_SAMPLE_SIZE=20 cargo test --test chat_ai_comprehensive_qa_test test_comprehensive_qa_100_queries -- --nocapture
```

### Test All 100 Queries (Full Run)
```bash
QUERY_SAMPLE_SIZE=100 cargo test --test chat_ai_comprehensive_qa_test test_comprehensive_qa_100_queries -- --nocapture
```

---

## Test Output Example

```
╔═══════════════════════════════════════════════════════════════╗
║       COMPREHENSIVE CHAT AI TEST - 100 DIVERSE QUERIES        ║
╚═══════════════════════════════════════════════════════════════╝

📊 Test Suite Statistics:
   Total queries: 100

   By Category:
     • Basic: 15 queries
     • Rpc: 15 queries
     • Svm: 12 queries
     • Nodes: 12 queries
     • Deploy: 10 queries
     • Audit: 8 queries
     • Mcp: 10 queries
     • Ovsm: 10 queries
     • Chat: 8 queries

   By Difficulty:
     • Easy: 18 queries
     • Medium: 44 queries
     • Hard: 22 queries
     • Expert: 16 queries
```

---

## Integration with Chat AI

These queries are designed to test:

1. **Plan Generation**: Does the AI generate appropriate tool execution plans?
2. **Tool Selection**: Does the AI select the correct MCP tools?
3. **Multi-Step Reasoning**: Can the AI handle complex multi-part queries?
4. **Context Understanding**: Does the AI understand domain-specific terminology?
5. **Error Handling**: How does the AI respond to ambiguous queries?
6. **Response Quality**: Are responses accurate, helpful, and well-formatted?

---

## Query Design Principles

Each query was designed with:

1. **Realism**: Based on actual user needs and workflows
2. **Coverage**: Spanning all OSVM CLI features
3. **Difficulty Progression**: From simple to expert-level
4. **Keyword Validation**: Expected keywords for automatic verification
5. **Category Diversity**: Balanced across all 9 categories

---

## Future Enhancements

- [ ] Add expected response templates for validation
- [ ] Integrate with actual chat AI service for live testing
- [ ] Add timing benchmarks for response speed
- [ ] Track tool execution patterns per query type
- [ ] Generate detailed HTML reports with query analytics
- [ ] Add multi-turn conversation tests
- [ ] Test edge cases and error conditions
- [ ] Add performance regression testing

---

## Statistics Summary

| Metric | Value |
|--------|-------|
| Total Queries | 100 |
| Categories | 9 |
| Difficulty Levels | 4 |
| Avg Queries per Category | 11.1 |
| Easy Queries | 18 (18%) |
| Medium Queries | 44 (44%) |
| Hard Queries | 22 (22%) |
| Expert Queries | 16 (16%) |

---

**Created:** 2025-10-16
**Test File:** `tests/chat_ai_comprehensive_qa_test.rs`
**Purpose:** Comprehensive testing of OSVM CLI chat AI capabilities
**Status:** ✅ Ready for Testing
