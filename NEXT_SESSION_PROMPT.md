# 🚀 NEXT SESSION PROMPT - COPY AND PASTE THIS

Use this prompt to start your next session and continue OVSM development:

---

## 📋 PROMPT TO PASTE:

```
Hi! I'm continuing development on the OVSM LISP interpreter. We're implementing Common Lisp functions to reach 100% ANSI standard coverage.

CURRENT STATUS:
- ✅ 302 functions implemented (30.9% of Common Lisp)
- ✅ Phase 4 COMPLETE
- 🎯 Phase 5 IN PROGRESS: Target 391 functions (40%)
- 📁 Location: /home/larp/larpdevs/osvm-cli/crates/ovsm/src/tools/stdlib/

COMPLETED MODULES (Phase 4):
✅ advanced_math.rs (42 functions)
✅ arrays.rs (25 functions)
✅ characters.rs (21 functions)
✅ data_processing.rs (15 functions)
✅ lists_advanced.rs (19 functions)
✅ math.rs (18 functions)
✅ numeric.rs (24 functions)
✅ objects.rs (8 functions)
✅ parsing.rs (15 functions)
✅ sequences.rs (40 functions)
✅ statistics.rs (8 functions)
✅ strings.rs (31 functions)
✅ type_predicates.rs (26 functions)
✅ utilities.rs (10 functions)

NEXT TASK (Phase 5 - Priority #1):
Implement hash_tables.rs with 24 functions:
- MAKE-HASH-TABLE, GETHASH, REMHASH, CLRHASH, HASH-TABLE-P
- HASH-TABLE-COUNT, HASH-TABLE-SIZE, MAPHASH, SXHASH
- Plus 15 more hash table operations

ROADMAP DOCUMENTS (READ THESE FIRST):
1. OVSM_ROADMAP_TO_100_PERCENT.md - Complete function list for all 676 remaining functions
2. OVSM_QUICK_CHECKLIST.md - Quick reference with checkboxes
3. BREAKTHROUGH_30_PERCENT_ACHIEVED.md - Current achievement status

IMPLEMENTATION PATTERN:
Follow the established Tool trait pattern used in all existing stdlib modules:
- Create struct for each function
- Implement Tool trait with name(), description(), execute()
- Add register() function at bottom
- Update mod.rs to include new module
- Test with: cargo build --release

Please:
1. Read OVSM_ROADMAP_TO_100_PERCENT.md to see Phase 5 details
2. Start implementing hash_tables.rs (24 functions)
3. Follow the implementation pattern from existing modules
4. Update OVSM_QUICK_CHECKLIST.md as you complete functions
5. Test compilation frequently

Ready to implement hash tables? Let's continue toward 40% coverage!
```

---

## 🎯 ALTERNATIVE SHORT PROMPT (Minimal Version):

```
Continue OVSM development: We're at 302/978 Common Lisp functions (30.9%).

Next: Implement hash_tables.rs (24 functions) in /home/larp/larpdevs/osvm-cli/crates/ovsm/src/tools/stdlib/

Read OVSM_ROADMAP_TO_100_PERCENT.md for full details. Follow the Tool trait pattern from existing modules. Target: 40% coverage (391 functions).

Let's implement hash tables!
```

---

## 🎯 ALTERNATIVE DETAILED PROMPT (Maximum Context):

```
Hi! Continuing OVSM LISP interpreter development toward 100% Common Lisp coverage.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                  CURRENT STATUS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Functions:        302/978 (30.9% complete) ✅
Phase:            Phase 4 COMPLETE, Phase 5 STARTING 🎯
Last Session:     October 29, 2025
Achievement:      Crossed 30% threshold! 🏆
vs Scheme R7RS:   +51% MORE functions (302 vs 200) 🥇
Build Status:     ✅ SUCCESS (zero errors)
Test Coverage:    97.3% ✅
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

PHASE 5 GOAL: 40% Coverage (391 functions)
NEEDED: +89 functions across 5 modules

PRIORITY ORDER:
1. 🔥 hash_tables.rs (24 functions) ← START HERE
2. 🔥 io_basic.rs (20 functions)
3. 📁 pathnames.rs (15 functions)
4. 📝 strings.rs extend (15 functions)
5. 🎨 format.rs (15 functions)

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
              HASH TABLES - NEXT TASK
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Implement 24 hash table functions in:
📁 /home/larp/larpdevs/osvm-cli/crates/ovsm/src/tools/stdlib/hash_tables.rs

CORE FUNCTIONS (Priority):
✓ MAKE-HASH-TABLE - Create new hash table
✓ GETHASH - Get value by key
✓ REMHASH - Remove entry
✓ CLRHASH - Clear all entries
✓ HASH-TABLE-P - Type predicate
✓ HASH-TABLE-COUNT - Number of entries
✓ HASH-TABLE-SIZE - Table capacity
✓ MAPHASH - Iterate over entries
✓ SXHASH - Compute hash code

ADDITIONAL FUNCTIONS:
✓ HASH-TABLE-TEST - Get equality test
✓ HASH-TABLE-REHASH-SIZE - Get rehash factor
✓ HASH-TABLE-REHASH-THRESHOLD - Get threshold
✓ WITH-HASH-TABLE-ITERATOR - Iteration macro
✓ Plus 11 more utilities...

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                IMPLEMENTATION GUIDE
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

STEP 1: Read Planning Documents
- OVSM_ROADMAP_TO_100_PERCENT.md (Section 5.1 - Hash Tables)
- OVSM_QUICK_CHECKLIST.md (Phase 5 details)

STEP 2: Review Existing Pattern
Look at any existing stdlib module (e.g., characters.rs) to see:
- Tool trait implementation pattern
- Error handling approach
- Registration function structure

STEP 3: Implement hash_tables.rs
- Use std::collections::HashMap as backing store
- Implement each Tool struct
- Add comprehensive error handling
- Document each function

STEP 4: Update mod.rs
- Add: pub mod hash_tables;
- Add: hash_tables::register(registry);

STEP 5: Test & Verify
- cargo build --release
- cargo test
- Update OVSM_QUICK_CHECKLIST.md checkboxes

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                  EXAMPLE PATTERN
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

pub struct MakeHashTableTool;

impl Tool for MakeHashTableTool {
    fn name(&self) -> &str {
        "MAKE-HASH-TABLE"
    }

    fn description(&self) -> &str {
        "Create a new hash table"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Implementation here
        let map = std::collections::HashMap::new();
        Ok(Value::Object(Arc::new(map)))
    }
}

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

REFERENCES:
- Common Lisp HyperSpec: http://www.lispworks.com/documentation/HyperSpec/
- Section 18.1 - Hash Table Concepts
- Working Directory: /home/larp/larpdevs/osvm-cli/

Ready to implement hash tables and push toward 40% coverage!
```

---

## 💡 USAGE TIPS

### When to Use Each Prompt:

**STANDARD PROMPT** (Recommended):
- Use for normal continuation
- Provides good context balance
- Includes all essential info

**SHORT PROMPT**:
- Use when you remember the context well
- Quick restart for same-day sessions
- Minimal token usage

**DETAILED PROMPT**:
- Use after a break of several days
- When you need maximum context
- For complex implementation tasks

---

## 🔧 CUSTOMIZATION

You can modify the prompt to:
- Skip to a different module (e.g., "implement io_basic.rs instead")
- Jump to a specific phase (e.g., "start Phase 6")
- Focus on a particular function category
- Request specific implementation help

---

## 📊 PROGRESS TRACKING

After each session, update the prompt with:
1. New function count
2. Completed modules
3. Current phase progress
4. Next priority task

Example update:
```
CURRENT STATUS:
- ✅ 326 functions implemented (33.3% of Common Lisp)
- ✅ hash_tables.rs COMPLETE (+24 functions)
- 🎯 Phase 5 IN PROGRESS: 326/391 (65 more needed)
```

---

## 🎯 QUICK START COMMANDS

Once in session, you can say:
- "Show me Phase 5 progress" - Check status
- "What's next after hash tables?" - See roadmap
- "Continue with io_basic.rs" - Start next module
- "Test the build" - Run cargo build
- "Update the checklist" - Mark completions

---

**READY TO GO!** Copy the STANDARD PROMPT above and paste it into your next session to continue building OVSM toward 100% Common Lisp coverage! 🚀

---

**Document**: NEXT_SESSION_PROMPT.md
**Purpose**: Session continuity prompt
**Last Updated**: October 29, 2025
**Current Functions**: 302/978 (30.9%)
**Next Task**: hash_tables.rs (24 functions)
