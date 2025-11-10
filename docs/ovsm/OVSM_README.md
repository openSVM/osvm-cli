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
;; Q: "What is the average transaction fee on Solana?"

;; Expected Plan:
;; [TIME: ~15s] [COST: free] [CONFIDENCE: 95%]

;; Available Tools:
;; From Standard Library: getSlot, getBlock, MAP, FLATTEN, MEAN, MEDIAN

;; Main Branch:
(do
  (define current_slot (getSlot))
  (define blocks [])

  (for (i (range 0 10))
    (define block (getBlock :slot (- current_slot i)))
    (set! blocks (append blocks block)))

  (define all_txs (flatten (map blocks (lambda (b) (. b transactions)))))
  (define fees (map all_txs (lambda (tx) (. tx meta fee))))

  ;; Statistics:
  (define mean_fee (mean :data fees))
  (define median_fee (median :data fees))
  (define stddev (stddev :data fees))

  ;; DECISION: Check distribution
  (define result
    (if (< (/ stddev mean_fee) 0.5)
        mean_fee          ;; BRANCH A: Normal distribution, use mean
        median_fee))      ;; BRANCH B: High variance, use median

  ;; Action:
  {:average_fee result
   :confidence 95
   :sample_size (length fees)})
```

---

## Language Features

### Core Syntax
- **Variables**: `(define variable_name value)` (LISP-style)
- **Constants**: `(const NAME value)` (immutable)
- **Types**: S-expressions with keyword arguments
- **Comments**: `;;` (semicolons for comments)

### Control Flow
- **Conditionals**: `(if condition then-expr else-expr)`
- **Loops**: `(while condition body...)`, `(for (item collection) body...)`
- **Decisions**: Explicit if/else branching
- **Guards**: Conditional expressions within if blocks

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
Every OVSM plan uses LISP S-expression syntax:
```ovsm
;; Expected Plan:
;; [METADATA tags]

;; Available Tools:
;;   [declarations]

;; Main execution:
(do
  ;; Primary execution path
  (define result (someFunction :param value))

  ;; Conditional branching
  (if condition
      (thenBranch)
      (elseBranch))

  ;; Final output
  result)
```

---

## Tool Naming Conventions

- **Solana RPC**: lowercase camelCase `(getSlot)`, `(getBlock)`
- **Data Processing**: lowercase `(map collection fn)`, `(filter collection pred)`, `(sum data)`
- **Custom Tools**: camelCase `(analyzeFees :block block)`
- **Statistical**: lowercase `(mean :data data)`, `(correlate :x x :y y)`
- **Utilities**: lowercase `(now)`, `(log :message msg)`, `(error :message err)`

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
