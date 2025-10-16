# Second-Pass Code Review: Additional Bugs Found

**Total: 10 bugs identified**
- HIGH: 3 bugs
- MEDIUM: 6 bugs
- LOW: 1 bug

---

## HIGH SEVERITY BUGS

### BUG-1001: Time-Based Theme Selection Without Early Morning Coverage
**File:** `src/utils/agent_chat_v2/ui/themes/mod.rs:162-178`
**Severity:** HIGH

The theme_for_time() function doesn't explicitly handle hours 0-5 (midnight to 5 AM). It silently uses "cyberpunk" which may not be intended for early morning.

**Fix:** Add explicit early morning handler with better documentation

---

### BUG-1004: UTF-8 Boundary Violation in Autocomplete Cursor Handling
**File:** `src/utils/agent_chat_v2/ui/autocomplete.rs:187-208`
**Severity:** HIGH

The `get_word_at_cursor()` function manipulates string byte indices without UTF-8 boundary checking. Multi-byte characters cause invalid string slices and panics.

**Fix:** Use char positions instead of byte positions

---

### BUG-1006: Regex Compilation Without Lazy Initialization
**File:** `src/utils/agent_chat_v2/ui/handlers.rs:~700+`
**Severity:** HIGH

Multiple regex patterns compiled with `.unwrap()` on every function call. Expensive and crash-prone if regex compilation fails.

**Fix:** Use once_cell::sync::Lazy for static regex compilation

---

## MEDIUM SEVERITY BUGS

### BUG-1002: Unbounded Particle Growth in SparkleEffect
**File:** `src/utils/agent_chat_v2/ui/effects/mod.rs:273-367`
**Severity:** MEDIUM

SparkleEffect particles vector grows without bounds. Spawn rate is 10/sec with no maximum particle cap. Causes memory leak in long-running UI.

**Fix:** Add MAX_PARTICLES constant and capacity check

---

### BUG-1003: Array Index Calculation Flaw in RippleEffect
**File:** `src/utils/agent_chat_v2/ui/effects/mod.rs:250-259`
**Severity:** MEDIUM

Index calculation `((1.0 - progress) * rings.len() as f32) as usize` doesn't properly clamp values. Relies on `.min()` instead of fixing calculation.

**Fix:** Use proper normalization before indexing

---

### BUG-1005: Empty String Panic in Typewriter Glitch Characters
**File:** `src/utils/agent_chat_v2/ui/animations/typewriter.rs`
**Severity:** MEDIUM

No bounds check before `random_range(0..chars.len())`. Empty string causes panic. Uses `.unwrap()` without fallback.

**Fix:** Add empty check and use fallback character

---

### BUG-1007: Potential Deadlock in Animation Frame Update
**File:** `src/utils/agent_chat_v2/ui/animations/mod.rs:42-46`
**Severity:** MEDIUM

Two sequential `.write()` calls on different RwLocks. If first succeeds but second fails, state becomes inconsistent.

**Fix:** Acquire both locks together or use atomics

---

### BUG-1008: Missing Lock Error Handling in Display Update
**File:** `src/utils/agent_chat_v2/ui/display.rs:256-289`
**Severity:** MEDIUM

Uses `.unwrap()` on lock acquisition without error handling. If lock is poisoned, app panics. Race condition between read and iteration.

**Fix:** Add proper error handling and clone data to release lock

---

### BUG-1010: Race Condition in Visibility Toggle Handler
**File:** `src/utils/agent_chat_v2/ui/layout.rs:318-327`
**Severity:** MEDIUM

State modified then UI updated separately. Between operations, other threads see inconsistent state. UI update failure leaves state out of sync.

**Fix:** Handle errors and ensure atomic state changes

---

## LOW SEVERITY BUGS

### BUG-1009: Non-Idiomatic Empty Check in Grid Layout
**File:** `src/utils/agent_chat_v2/ui/layouts/adaptive.rs:147`
**Severity:** LOW

Uses `if current_row.len() > 0` instead of `!is_empty()`. Empty row added to grid wastes space.

**Fix:** Use `.is_empty()` and prevent empty row addition

---

## Implementation Priority

**P0 (Critical):** BUG-1004, BUG-1006 - Cause crashes
**P1 (High):** BUG-1001 - Logic error
**P2 (Medium):** BUG-1002, BUG-1003, BUG-1005, BUG-1007, BUG-1008, BUG-1010
**P3 (Low):** BUG-1009

