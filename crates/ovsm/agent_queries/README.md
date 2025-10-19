# OVSM Agent Queries

This directory contains 100 diverse OVSM-LISP query examples demonstrating various language features and use cases.

## Directory Structure

```
agent_queries/
├── basic/              # 25 queries: arithmetic, variables, conditionals
├── loops/              # 25 queries: while, for, iteration patterns
├── data_structures/    # 25 queries: arrays, objects, manipulation
└── advanced/           # 25 queries: blockchain, complex algorithms
```

## Usage

Execute any query with:
```bash
# Using OVSM service
osvm ovsm run agent_queries/basic/001_simple_addition.ovsm

# Or directly via cargo
cargo run --bin osvm -- ovsm run agent_queries/basic/001_simple_addition.ovsm
```

## Query Format

Each query file includes:
- **Query description**: What the code does
- **Category**: Feature category
- **Expected result**: What the output should be
- **OVSM code**: The actual implementation

## Categories

### Basic (001-025)
Simple operations like arithmetic, variables, conditionals, and basic helper functions.

### Loops (026-050)
Iteration patterns including while loops, for loops, nested loops, and loop control.

### Data Structures (051-075)
Array operations, object manipulation, filtering, mapping, and data transformations.

### Advanced (076-100)
Complex algorithms, blockchain operations, time-based queries, and real-world use cases.
