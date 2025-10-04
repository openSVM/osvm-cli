# MicroVM Architecture Gap Analysis

## Current State vs. Intended Architecture

### What You're Seeing
```
$ osvm
🚀 OSVM Agent Starting (Direct Mode - OSVM_SKIP_MICROVM=1)
OSVM: 1μVMs 0uK • osvm-mcp(μVM-1:cfg) • net:unknown • up:0s
```

- System shows `1μVMs 0uK` - 1 fake microVM count, 0 unikernels
- OSVM_SKIP_MICROVM=1 is set automatically
- Tools execute directly via MCP, no unikernel spawning
- No real microVM isolation

### Intended Architecture (From Original Plan)

```
Host Machine
├── osvm (bootstrap binary)
│   └── Launches → Firecracker MicroVM (μVM-1)
│       └── OSVM Agent Runtime
│           ├── When user sends prompt:
│           │   ├── AI plans tool calls
│           │   └── For each tool:
│           │       └── Spawn ephemeral unikernel (~100ms)
│           │           ├── Execute tool in isolation
│           │           └── Terminate immediately
│           └── Returns results to user
```

## Root Causes

### 1. Bootstrap Not Actually Used
**File:** `src/main.rs` (lines 1900-1930)
```rust
// No subcommand - bootstrap OSVM agent with microVM isolation
if skip_microvm || is_running_in_microvm() {
    // Already in microVM or explicitly skipping - run agent directly
    ...
} else {
    // On host - launch in microVM for enhanced security
    println!("🚀 Launching OSVM Agent in microVM...");
    
    // For now, Firecracker setup is complex - use direct mode with unikernel support
    println!("⚠️  MicroVM launch requires Firecracker + kernel/rootfs setup");
    println!("   Falling back to direct execution with unikernel tool isolation");
    ...
    
    // Run agent directly with unikernel support for tools
    return crate::utils::agent_chat_v2::run_advanced_agent_chat()
        .await
        .map_err(|e| format!("Failed to start advanced chat: {}", e).into());
}
```

**Problem:** Code always falls back to direct execution with a warning message. The actual microVM launch logic is never executed.

### 2. Isolation Config Not Auto-Created
**File:** `src/services/isolation_config.rs`
- Default config exists but only creates example servers ("solana", "github")
- No "osvm-mcp" server in default configuration
- Without config entry, tools fall back to direct MCP execution

**Code:**
```rust
fn create_default_config() -> Self {
    let mut config = Self::default();
    
    // Example Solana MCP server configuration
    let mut solana_tools = HashMap::new();
    // ... adds "solana" server
    
    config.mcp_servers.insert("github".to_string(), ...);
    // No osvm-mcp entry!
}
```

### 3. Tool Execution Always Falls Back
**File:** `src/utils/agent_chat_v2/agent/execution.rs` (lines 230-260)
```rust
async fn call_mcp_tool(&self, planned_tool: &PlannedTool) -> Result<Value> {
    // Load isolation configuration
    let isolation_config = match IsolationConfig::load() {
        Ok(config) => config,
        Err(e) => {
            warn!("Failed to load isolation config: {}, using direct MCP execution", e);
            return self.call_mcp_tool_direct(planned_tool).await;
        }
    };

    // Check if this tool should be executed in a unikernel
    if isolation_config.should_use_unikernel(&planned_tool.server_id, &planned_tool.tool_name) {
        // Attempt unikernel execution with fallback
        match self.execute_tool_in_unikernel(planned_tool, &isolation_config).await {
            Ok(result) => return Ok(result),
            Err(e) => {
                warn!("Unikernel execution failed: {}, falling back to direct MCP", e);
                return self.call_mcp_tool_direct(planned_tool).await;
            }
        }
    }

    // Use direct MCP execution (MicroVM mode or no isolation config)
    self.call_mcp_tool_direct(planned_tool).await
}
```

**Problem:** Without osvm-mcp in isolation_config.json, `should_use_unikernel()` returns false, so tools never attempt unikernel execution.

## What Needs to Happen

### Phase 1: Fix Isolation Config (Immediate)
1. Auto-configure osvm-mcp server in isolation_config.json when detected
2. Set default execution mode to `ExecutionMode::Unikernel`
3. Provide default unikernel image paths (even if they don't exist yet, for proper fallback)

### Phase 2: Enable MicroVM Bootstrap (Short-term)
1. Create minimal Firecracker configuration
2. Build/download minimal kernel + rootfs images
3. Enable actual microVM launch in main.rs
4. Remove the "complex setup" fallback

### Phase 3: Create Unikernel Images (Medium-term)
1. Build OSv-based unikernel images for common tools
2. Package with OSVM distribution
3. Enable actual unikernel spawning for tool execution

## Immediate Fix Options

### Option A: Auto-Configure Isolation (Easiest)
Modify `src/utils/agent_chat_v2/agent/execution.rs` to:
- Detect when osvm-mcp tools are called
- Auto-create isolation config entry with Unikernel mode
- Tools will attempt unikernel spawn → fail gracefully → fall back to MCP
- At least the architecture will be in place for when images are available

### Option B: Create Stub Unikernel Images
- Create placeholder unikernel images
- Tools will actually spawn unikernel processes
- Proper isolation for demo purposes
- More complex but demonstrates full architecture

### Option C: Enable MicroVM Bootstrap
- Fix main.rs to actually launch Firecracker
- Requires Firecracker + kernel/rootfs setup
- Most complete but highest setup burden

## Recommended Approach

**Start with Option A** to fix the immediate issue:

1. When osvm-mcp is auto-cloned in `mod.rs`, also create isolation config entry
2. Set execution mode to Unikernel with placeholder image paths
3. Update system status to show "attempting unikernel spawn" when tools run
4. Document in logs why fallback occurs (missing images)

This gets the architecture in place and makes the issue visible to users, setting them up for when actual unikernel images become available.

## Files to Modify

1. `src/utils/agent_chat_v2/mod.rs` - Add isolation config creation after osvm-mcp clone
2. `src/services/isolation_config.rs` - Add helper method to add osvm-mcp config
3. `src/utils/agent_chat_v2/agent/execution.rs` - Better logging of why fallback occurs
4. Optional: `src/main.rs` - Remove misleading "0uK" count, show "0uK (images pending)" or similar

## Success Criteria

After fix, users should see:
```
$ osvm
🚀 OSVM Agent Starting (Direct Mode - OSVM_SKIP_MICROVM=1)
OSVM: 1μVMs 0uK* • osvm-mcp(μVM-1:cfg) • net:unknown • up:0s
      ^^^^^
      Attempted unikernel spawn, fell back (no images)
```

And in logs when tool is called:
```
INFO: Executing tool 'get_balance' from osvm-mcp
INFO: Checking isolation config for osvm-mcp/get_balance
INFO: Tool configured for Unikernel execution mode
INFO: Attempting to spawn unikernel with image: /var/osvm/unikernels/osvm-mcp-get_balance.img
WARN: Unikernel image not found: /var/osvm/unikernels/osvm-mcp-get_balance.img
WARN: Falling back to direct MCP execution
```

This makes the architecture visible and sets up for future unikernel image distribution.
