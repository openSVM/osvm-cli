# OSVM CLI - AI Context Document

## Current Branch
`copilot/fix-61ce1f52-d43c-4fc5-8502-1813e5b2f744`

## Recent Work Summary

### 1. Ephemeral MicroVM Implementation (MAJOR FEATURE)
**Status**: ✅ Complete and tested

**What was built**:
- Created `src/services/ephemeral_microvm.rs` - Complete ephemeral VM management system
- Created `src/utils/agent_chat_microvm.rs` - MicroVM-enabled chat interface
- Created `src/commands/mcp_microvm.rs` - MCP microVM command handlers
- Modified `src/services/mcp_service.rs` to use ephemeral VMs by default

**Key Feature**: ALL MCP tools now execute in ephemeral microVMs that are:
- Created fresh for each tool call
- Execute in complete isolation
- Destroyed immediately after returning results
- Zero state persistence between executions

**Architecture**:
```
User Chat (Persistent VM)
    ↓
MCP Service (with use_ephemeral_vms: true by default)
    ↓
Ephemeral VM Manager
    ↓
[Tool VM₁] → Execute → Destroy
[Tool VM₂] → Execute → Destroy
[Tool VM₃] → Execute → Destroy
```

**Usage**:
- `osvm chat --microvm` - Run chat in isolated microVM
- `osvm mcp call <server> <tool>` - Automatically uses ephemeral VM

### 2. DOS Terminal Documentation UI
**Status**: ✅ Complete with interactive features

**What was built**:
- `docs/assets/dos-terminal.css` - Complete DOS terminal styling (NO GREEN, only white/amber)
- `docs/assets/dos-interactive.js` - Interactive terminal with command simulation
- Created missing documentation pages:
  - `docs/pages/installation.html`
  - `docs/pages/ai-integration.html`
  - `docs/pages/mcp-servers.html`
  - `docs/pages/node-deployment.html`
  - `docs/pages/api-reference.html`

**Features**:
- Authentic DOS aesthetic (white/amber text on black)
- Interactive terminal where users can click commands to see simulated output
- Copy-to-clipboard functionality
- Command history with arrow keys
- Teaching interface with example commands

**Current Issues**: None - layout is fixed, navigation on one line, content properly centered

### 3. Test Organization
**Status**: ✅ Complete

Moved all test scripts to organized structure:
```
test-scripts/
├── chat/         # Chat and AI planning tests
├── microvm/      # MicroVM isolation tests
├── integration/  # General integration tests
└── performance/  # Load and performance tests
```

## File Structure

### Key Files Modified
```
src/
├── services/
│   ├── ephemeral_microvm.rs (NEW - 502 lines)
│   ├── mcp_service.rs (MODIFIED - ephemeral VMs enabled by default)
│   └── mod.rs (UPDATED - added ephemeral_microvm module)
├── utils/
│   ├── agent_chat_microvm.rs (NEW - 223 lines)
│   └── mod.rs (UPDATED)
├── commands/
│   ├── mcp_microvm.rs (NEW - 273 lines)
│   └── mod.rs (UPDATED)
├── main.rs (MODIFIED - added --microvm flag handling)
└── clparse.rs (MODIFIED - added --microvm option)

docs/
├── assets/
│   ├── dos-terminal.css (NEW - 700+ lines)
│   └── dos-interactive.js (NEW - 541 lines)
├── pages/
│   ├── installation.html (NEW)
│   ├── ai-integration.html (NEW)
│   ├── mcp-servers.html (NEW)
│   ├── node-deployment.html (NEW)
│   └── api-reference.html (NEW)
└── index.html (MODIFIED - uses DOS terminal theme)
```

## Current Server Status
- HTTP server running on port 8080 serving docs/
- Background process IDs: 5a34c4, 2841b1
- Access at: http://localhost:8080

## Compilation Status
✅ Code compiles successfully with warnings:
- 2 naming warnings in ai_service.rs (systemPrompt, ownPlan)
- No errors

## Git Status
- Branch is up to date with origin
- Some untracked Docker and test files remain
- All major features are committed and pushed

## Key Technical Details

### Ephemeral VM Configuration
```rust
EphemeralVmConfig {
    memory_mb: 256,        // Per tool VM
    cpus: 1,               // Per tool VM
    timeout_secs: 30,      // Tool execution timeout
    vsock_cid: atomic,     // Unique per VM
}
```

### MCP Service Settings
```rust
McpService {
    use_ephemeral_vms: true,  // DEFAULT - all tools use ephemeral VMs
    ephemeral_vm_manager: EphemeralVmManager::new(),
}
```

### DOS Terminal Colors (NO GREEN!)
```css
:root {
  --dos-white: #c0c0c0;
  --dos-bright-white: #ffffff;
  --dos-amber: #ffaa00;
  --dos-bright-amber: #ffff55;
  --dos-gray: #808080;
  /* NO GREEN COLORS */
}
```

## Testing Commands

### Test Ephemeral VMs
```bash
./test-scripts/microvm/test_ephemeral_microvms.sh
cargo test ephemeral_vm --lib
```

### Test Documentation
```bash
# Server already running on :8080
chromium-browser --headless --screenshot=test.png http://localhost:8080
```

## Next Steps / Known Issues

1. **Documentation**: The home page has been modified by the user with better ASCII art and styling
2. **Server**: Two Python HTTP server processes running (can be killed if needed)
3. **Docker files**: Several untracked Docker-related files that could be reviewed

## How to Continue

1. **To work on ephemeral VMs**:
   - Main code in `src/services/ephemeral_microvm.rs`
   - Integration in `src/services/mcp_service.rs`
   - Test with `osvm chat --microvm`

2. **To work on documentation**:
   - Server running on :8080
   - Main styles in `docs/assets/dos-terminal.css`
   - Interactive logic in `docs/assets/dos-interactive.js`

3. **To test changes**:
   ```bash
   # Build
   cargo build --release

   # Test ephemeral VMs
   ./target/release/osvm mcp list
   ./target/release/osvm chat --microvm

   # Test docs (server already running)
   curl http://localhost:8080
   ```

## Important Notes

1. **Ephemeral VMs are ON by default** - Every MCP tool call creates and destroys a VM
2. **DOS terminal has NO GREEN** - Only white/amber colors per user preference
3. **Navigation must stay on one line** - Already fixed with flex-wrap: nowrap
4. **Content max-width is 1200px** - Interactive terminal is 900px

## Recent Commits
- `6274d84` - fix: Perfect layout centering and width constraints
- `f49f3dc` - fix: Fix ASCII art alignment in DOS terminal
- `1b7dde7` - fix: Repair DOS terminal documentation UX
- `2c2052e` - feat: Add interactive DOS terminal UI
- `ccec03d` - refactor: Organize test scripts
- `d6b2ecc` - chore: Clean up obsolete docs
- `d1d293b` - feat: Add ephemeral microVM isolation for MCP tool execution

This context should allow another AI to continue exactly where we left off.