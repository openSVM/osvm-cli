# OVSM Language
## Open Versatile Seeker Mind

**A language for AI agents that seek knowledge**

---

## What is OVSM?

OVSM (pronounced "OH-vism") is a domain-specific language designed for expressing AI agent research plans and multi-agent workflows. It provides a clear, executable syntax for planning complex investigations with conditional branching, parallel execution, and adaptive learning.

**Key Features:**
- 🔍 **Research-Optimized**: Built for multi-step investigation workflows
- 🤖 **Multi-Agent**: Native support for agent delegation and coordination
- 📊 **Statistical**: Built-in hypothesis testing and causal inference
- 🌐 **Knowledge Graphs**: First-class support for graph construction and querying
- 🧠 **Adaptive**: Learning from past queries to optimize future ones
- 🔄 **Extensible**: 207 built-in tools, easy to add more

---

## Quick Example

```ovsm
## Q: "What is the average transaction fee on Solana?"

**Expected Plan:**

[TIME: ~15s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: getSlot, getBlock, MAP, FLATTEN, MEAN, MEDIAN

**Main Branch:**
$current_slot = getSlot()
$blocks = []

FOR $i IN 0..10:
  $block = getBlock(slot: $current_slot - $i)
  $blocks = APPEND(array: $blocks, item: $block)

$all_txs = FLATTEN(collection: MAP($blocks, b => b.transactions))
$fees = MAP(collection: $all_txs, fn: tx => tx.meta.fee)

$mean_fee = MEAN(data: $fees)
$median_fee = MEDIAN(data: $fees)
$stddev = STDDEV(data: $fees)

DECISION: Check distribution
  BRANCH A ($stddev / $mean_fee < 0.5):
    $result = $mean_fee
  BRANCH B ($stddev / $mean_fee >= 0.5):
    $result = $median_fee

**Action:**
RETURN {
  average_fee: $result,
  confidence: 95,
  sample_size: COUNT(collection: $fees)
}
```

---

## Language Features

### Core Syntax
- **Variables**: `$variable_name` (shell-style)
- **Constants**: `CONST NAME = value` (immutable)
- **Types**: Optional type annotations
- **Comments**: `//` and `/* */`

### Control Flow
- **Conditionals**: IF/ELSE/THEN
- **Loops**: WHILE, FOR, LOOP EVERY
- **Decisions**: DECISION/BRANCH structure
- **Guards**: GUARD condition ELSE action

### Tool System
- **Standard Library**: 104 tools (Solana RPC, data processing, statistics, math)
- **Agent Extensions**: 103 tools (multi-agent, learning, knowledge graphs)
- **Custom Tools**: DEFINE_TOOL for reusable logic
- **Local Helpers**: DEFINE for scoped functions

### Error Handling
- **Try-Catch**: TRY/CATCH with error types (FATAL, RECOVERABLE, WARNING)
- **Fallback Chains**: Elegant error recovery
- **Circuit Breakers**: Prevent cascading failures
- **Retry Logic**: Automatic retry with backoff

### Parallel Execution
- **PARALLEL blocks**: Concurrent tool execution
- **Wait Strategies**: WAIT_ALL, WAIT_ANY, RACE
- **Async Operations**: Native async support
- **Timeout Guards**: Prevent hanging

### Advanced Features
- **Agent Delegation**: Spawn specialized sub-agents
- **Knowledge Graphs**: Build and query relationship graphs
- **Hypothesis Testing**: Statistical significance testing
- **Causal Inference**: Beyond correlation to causation
- **Cross-Validation**: Verify across multiple sources
- **Meta-Learning**: Learn optimal strategies from history
- **Real-Time Monitoring**: Alerts with automated investigation

---

## Documentation

### Core Specification
**File**: `ovsm-spec.md` (2,811 lines)
- Complete syntax reference
- 104 standard library tools
- 19 Solana constants
- 25 keywords
- 4 executable examples
- EBNF grammar
- Best practices guide

### Agent Extensions
**File**: `ovsm-agents.md` (1,908 lines)
- 15 advanced features
- 103 specialized tools
- Multi-agent patterns
- Research workflow examples

### Supporting Documents
- `COMPLETE_TOOL_INDEX.md` - All 207 tools categorized
- `PLANNING_FORMAT.md` - Planning methodology
- `SYNTAX_IMPROVEMENTS_BRAINSTORM.md` - Design evolution
- `ROUND_7_FINAL.md` - Validation report

---

## Installation & Usage

### File Extension
OVSM plans use the `.ovsm` extension:
```
blockchain_research.ovsm
validator_analysis.ovsm
token_investigation.ovsm
```

### Basic Structure
Every OVSM plan follows this structure:
```ovsm
**Expected Plan:**

[METADATA tags]

**Available Tools:**
  [declarations]

**Main Branch:**
  [primary execution path]

**Decision Point:** [condition]
  [conditional branches]

**Action:** [final output description]
```

---

## Tool Naming Conventions

- **Solana RPC**: lowercase camelCase (`getSlot`, `getBlock`)
- **Data Processing**: UPPERCASE (`MAP`, `FILTER`, `SUM`)
- **Custom Tools**: snake_case (`analyze_fees`, `query_pool`)
- **Statistical**: UPPERCASE (`MEAN`, `CORRELATE`, `T_TEST`)
- **Utilities**: UPPERCASE (`NOW`, `LOG`, `ERROR`)

---

## Language Stats

- **Total Tools**: 207
- **Constants**: 19
- **Keywords**: 25
- **Examples**: 19 (4 base + 15 agent)
- **Lines of Specification**: 4,719
- **Validation Rounds**: 8 (exhaustive)
- **Production-Ready**: YES ✅

---

## Version

**Current**: v1.1 (Stable)
**Released**: 2025-10-08
**Status**: Production-Ready

---

## Philosophy

OVSM embodies the idea that **AI research should be**:
- **Explicit**: Clear plans, no hidden steps
- **Adaptive**: Learn and improve over time
- **Collaborative**: Multi-agent by design
- **Rigorous**: Statistical testing and validation
- **Transparent**: Show confidence and limitations

**"Plan like you think, seek like a mind"** - OVSM

---

## Quick Links

- [Core Specification](ovsm-spec.md) - Complete language reference
- [Agent Extensions](ovsm-agents.md) - Advanced multi-agent features
- [Tool Index](COMPLETE_TOOL_INDEX.md) - All 207 tools
- [Validation Report](ROUND_7_FINAL.md) - Quality assurance

---

## License

MIT License - Free to use, modify, and distribute

---

## Contributing

To propose language improvements:
1. Document the use case
2. Provide example syntax
3. Explain benefits
4. Show backward compatibility

---

**OVSM - Where AI minds plan their quest for knowledge** 🔍
