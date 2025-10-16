# Priority Bug Fixes

## P0: Critical - Breaks Core Features

### BUG-003: Search Results Don't Update (BLOCKING)
**File:** `src/utils/agent_chat_v2/ui/search.rs:265-269`

**Current Code:**
```rust
fn update_search_results(s: &mut Cursive) {
    // This would need access to the search state stored somewhere
    // For now, we'll just trigger a refresh
}
```

**Problem:** User types in search box but nothing happens

**Fix Strategy:**
- Store search_state in Cursive user_data using a unique key
- In on_edit callback, update the filter AND refresh the SelectView
- Create a helper to rebuild SelectView with filtered results

---

## P1: High - Causes Crashes or Data Loss

### BUG-005: Panic on Missing State
**File:** Multiple locations in search.rs

**Current Code:**
```rust
let state = s.user_data::<AdvancedChatState>()
    .cloned()
    .expect("AdvancedChatState should be set");
```

**Fix:**
```rust
let state = match s.user_data::<AdvancedChatState>() {
    Some(s) => s.clone(),
    None => {
        log::error!("AdvancedChatState not found");
        return;
    }
};
```

---

### BUG-001: Toast Layer Removal Race Condition
**File:** `src/utils/agent_chat_v2/ui/toast.rs:186-191`

**Current Code:**
```rust
s.pop_layer();  // Wrong - pops ANY top layer, not the toast!
```

**Fix Options:**

**Option A (Simple):** Don't use pop_layer, rely on expiration only
```rust
// Remove the timer-based removal entirely
// Let toasts expire naturally as they're cleared during updates
```

**Option B (Better):** Use callback sink with specific layer name
```rust
let layer_name_to_remove = layer_name.clone();
cb_sink.send(Box::new(move |s| {
    // Remove by finding and popping - need to track layer stack
    // This requires extending Cursive's layer management
})).ok();
```

**Option C (Best):** Use a toast manager approach
```rust
// Store toasts in a centralized manager with timestamps
// Check expiration on each render cycle, don't spawn threads
```

---

## P2: High - Resource/Performance Issues

### BUG-004: Thread Per Toast Spawning
**File:** `src/utils/agent_chat_v2/ui/toast.rs:186-191`

**Problem:** Creates a new OS thread for every toast

**Fix:** Replace thread spawning with Cursive's callback system
```rust
// Instead of:
std::thread::spawn(move || {
    std::thread::sleep(duration);
    cb_sink.send(...);
});

// Use timer callbacks or background task manager
// Tokio task instead of std::thread if async available
```

---

### BUG-002: Race Condition in Search Initialization
**File:** `src/utils/agent_chat_v2/ui/search.rs:119-151`

**Problem:** Filter updates race with initial view creation

**Fix:** Defer view creation until state is fully initialized
```rust
// Create search_state
let search_state = Arc::new(RwLock::new(ToolSearchState::new()));

// Load tools INTO search_state
if let Ok(tools) = state.available_tools.read() {
    if let Ok(mut search) = search_state.write() {
        search.all_tools = tools.clone();
    }
}

// ONLY AFTER tools loaded, create view
let results_view = create_search_results_view(&search_state);
```

---

## P3: Medium - Feature Incomplete

### BUG-007: Recent Tools Not Saved
**File:** `src/utils/agent_chat_v2/ui/search.rs:245-250`

**Fix:**
```rust
select_view.set_on_submit(|s, (server_id, tool_name): &(String, String)| {
    if !server_id.is_empty() && !tool_name.is_empty() {
        // IMPLEMENT: Add to recent tools
        // if let Some(state) = s.user_data::<AdvancedChatState>() {
        //     // Need to get search_state from somewhere and call add_recent()
        // }

        s.pop_layer();
        show_tool_details(s, server_id.clone(), tool_name.clone());
    }
});
```

---

### BUG-006: Empty Search Results No Feedback
**File:** `src/utils/agent_chat_v2/ui/search.rs:363-366`

**Fix:**
```rust
if results.is_empty() {
    results.add_item("No sessions found", uuid::Uuid::nil());
}
```

---

## Implementation Path

### Immediate (BUG-003, BUG-005)
1. Fix panic handling first - prevents crashes
2. Fix search update - restores core feature

### Short-term (BUG-001, BUG-004)
1. Replace toast threading with simple expiration checking
2. Use render-cycle cleanup instead of spawned threads

### Medium-term (BUG-002, BUG-007)
1. Fix race conditions
2. Complete unfinished features

### Polish (BUG-006, ISSUE-001)
1. Improve UX with empty state handling
2. Clean up code

