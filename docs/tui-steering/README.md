# OSVM TUI Steering Documents

This directory contains comprehensive steering documents for five major TUI features planned for the OSVM CLI.

---

## Overview

| Feature | Command | Document | Priority | Status |
|---------|---------|----------|----------|--------|
| Token Swap | `osvm swap` | [SWAP_TUI.md](./SWAP_TUI.md) | High | 📋 Planned |
| AMM LP Management | `osvm amm` | [AMM_TUI.md](./AMM_TUI.md) | High | 📋 Planned |
| Perp Trading | `osvm perp` | [PERP_TUI.md](./PERP_TUI.md) | High | 📋 Planned |
| Terminal IDE | `osvm ide` | [IDE_TUI.md](./IDE_TUI.md) | Medium-High | 📋 Planned |
| AI Code Assistant | `osvm code` | [CODE_TUI.md](./CODE_TUI.md) | High | 📋 Planned |

---

## Feature Summaries

### 🔄 osvm swap
Cross-market token swap interface with real-time quotes from Jupiter, Raydium, and Orca. Features price comparison, slippage protection, and keyboard-driven execution.

**Estimated Implementation:** 4 weeks

### 📊 osvm amm
Unified AMM LP management across Raydium, Meteora, and Orca. Add/remove liquidity, track positions, claim rewards, and analyze performance with impermanent loss calculations.

**Estimated Implementation:** 8 weeks

### 📈 osvm perp
Professional perpetual futures trading interface for Drift, Jupiter Perps, and Flash Trade. Real-time order book, position management, risk controls, and WebSocket data feeds.

**Estimated Implementation:** 8 weeks

### 🖥️ osvm ide
VS Code-like terminal IDE with extension support. Tree-sitter syntax highlighting, LSP integration, split panes, integrated terminal, and a compatibility layer for VS Code extensions.

**Estimated Implementation:** 16 weeks

### 🤖 osvm code
AI-powered coding assistant (Claude Code clone) in ratatui. Agentic file operations, command execution, streaming responses, permission system, and MCP integration.

**Estimated Implementation:** 10 weeks

---

## Shared Infrastructure

All TUIs share common infrastructure:

### Existing Dependencies
```toml
ratatui = "0.29.0"
crossterm = "0.29.0"
tokio = { features = ["full"] }
```

### Shared Components
- **Event Loop** - Unified input handling
- **Theme System** - Consistent styling
- **Keyboard System** - Standard shortcuts
- **Status Bar** - Common status display
- **Modal System** - Overlays and dialogs

### Directory Structure
```
src/utils/tui/
├── mod.rs              # Existing TUI code
├── app.rs              # Existing (research TUI)
├── graph.rs            # Existing
├── shared/             # NEW: Shared components
│   ├── mod.rs
│   ├── theme.rs
│   ├── input.rs
│   ├── modal.rs
│   └── status.rs
├── swap/               # NEW: Swap TUI
├── amm/                # NEW: AMM TUI
├── perp/               # NEW: Perp TUI
├── ide/                # NEW: IDE TUI
└── code/               # NEW: Code TUI
```

---

## Implementation Priority

Recommended order based on complexity and dependencies:

1. **osvm swap** (4 weeks)
   - Simpler scope, builds foundation
   - Tests API integration patterns

2. **osvm code** (10 weeks)
   - High value, differentiating feature
   - Can start in parallel with swap

3. **osvm amm** (8 weeks)
   - Builds on swap patterns
   - More complex state management

4. **osvm perp** (8 weeks)
   - Similar patterns to AMM
   - Adds WebSocket complexity

5. **osvm ide** (16 weeks)
   - Most complex
   - Can be developed incrementally

---

## Development Approach

### Phase-Based Development
Each TUI should be developed in phases:

1. **Core Infrastructure** - State, basic UI, input handling
2. **Data Integration** - APIs, WebSockets, caching
3. **Operations** - Transactions, commands, mutations
4. **Polish** - Settings, history, help, edge cases

### Testing Strategy
- **Unit Tests** - State management, calculations
- **Integration Tests** - API mocking, transaction building
- **E2E Tests** - Full TUI interaction simulation
- **Screenshot Tests** - Visual regression (existing infrastructure)

### Documentation
Each feature should have:
- Steering document (this directory) ✓
- User guide (post-implementation)
- API documentation (rustdoc)
- Examples directory

---

## Getting Started

To begin implementing a feature:

1. Read the relevant steering document thoroughly
2. Create the directory structure under `src/utils/tui/`
3. Start with Phase 1 tasks from the implementation plan
4. Use existing TUI code as reference (`src/utils/tui/app.rs`)
5. Test incrementally with `cargo test --lib`

---

## Contributing

When updating these documents:
- Keep UI mockups in sync with implementation
- Update "Open Questions" as decisions are made
- Add new sections as patterns emerge
- Cross-reference related documents

---

## References

- [Existing TUI Guide](../TUI_GUIDE.md)
- [TUI Integration Summary](../TUI_INTEGRATION_SUMMARY.md)
- [TUI Troubleshooting](../TUI_TROUBLESHOOTING.md)
- [ratatui Documentation](https://docs.rs/ratatui)
