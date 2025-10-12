# OSVM-CLI Project Reorganization Plan

**Date:** 2025-10-12
**Status:** Ready for Review
**Purpose:** Clean up project structure, remove outdated files, sync documentation

---

## Executive Summary

**Issues Identified:**
1. ✅ **CRITICAL FIX APPLIED:** Dockerfile missing `COPY crates/ ./crates/` (CI blocker)
2. Multiple session summary files at root (some tracked in git)
3. Temporary files that should be gitignored but aren't
4. Redundant status/assessment files in `test_qa_categories/` and `crates/ovsm/`
5. Backup HTML files in `docs/`
6. Large temporary text dumps (1.4MB snapshot file)
7. Temporary Python/shell scripts at root
8. Old validator log files and chat exports

**Total Files to Review:** ~150+ files identified
**Estimated Cleanup:** 50-100 files can be removed/reorganized

---

## 1. Critical Fix Applied ✅

### Dockerfile - CI Build Failure

**Issue:** Workspace member `crates/ovsm` not copied into Docker build context
**Error:** `failed to load manifest for dependency ovsm`
**Fix Applied:** Added `COPY crates/ ./crates/` at line 26 in Dockerfile

```dockerfile
# Copy workspace crates (includes ovsm crate)
COPY crates/ ./crates/
```

**Status:** ✅ Fixed - Ready for CI validation

---

## 2. Root Directory Cleanup

### 2.1 Session Summary Files (Tracked in Git)

**Current State:** Multiple session summaries committed to git:
- `AI_CONTEXT.md` (6.4K) - MicroVM implementation context
- `FINAL_SESSION_SUMMARY.md` (11K) - Integration test restoration (Oct 9)
- `FINAL_TEST_STATUS.md` (8.2K) - Test status (Oct 9)
- `SESSION_SUMMARY.md` (5.5K) - OVSM language spec (Oct 8)
- `QA_PROGRESS_REPORT.md` (9.1K) - Token research (Jan 14)
- `UI_ENHANCEMENTS.md` (4.8K)
- `ULTIMATE_DECODER_SUITE.md` (6.3K)

**Analysis:**
- These appear to be development session notes
- May have historical value but clutter root directory
- Not referenced by main documentation

**Recommendation:**
```bash
# Option A: Archive to docs/archive/
mkdir -p docs/archive/session-notes
mv AI_CONTEXT.md FINAL_SESSION_SUMMARY.md FINAL_TEST_STATUS.md \
   SESSION_SUMMARY.md QA_PROGRESS_REPORT.md UI_ENHANCEMENTS.md \
   ULTIMATE_DECODER_SUITE.md docs/archive/session-notes/

# Option B: Delete if not needed
# git rm AI_CONTEXT.md FINAL_SESSION_SUMMARY.md ...
```

**Decision Needed:** Archive or Delete?

### 2.2 Temporary Files (Should Be Gitignored)

**Log Files:**
```
agave-validator-*.log (5 files)
osvm_chat_*.log (5 files)
osvm.log
```

**Action:**
```bash
# These match .gitignore patterns but still exist
rm -f *.log agave-validator-*.log osvm_chat_*.log
```

**Chat Exports:**
```
osvm_chat_export_*.json (9 files)
```

**Action:**
```bash
# Matches .gitignore pattern
rm -f osvm_chat_export_*.json
```

**Large Text Dumps:**
```
snapshot_stream_output.txt (1.4MB)
snapshot_with_data_sample.txt (9.8K)
stderr.txt (1.8K)
stdout.txt (0 bytes)
```

**Action:**
```bash
# These match .gitignore patterns
rm -f snapshot_*.txt stderr.txt stdout.txt
```

**Temporary Scripts:**
```
complete_09.py
fix_syntax.py
fix_remaining.sh
generate_qa.py
```

**Action:**
```bash
# These match .gitignore patterns
rm -f complete_*.py fix_syntax.py generate_qa.py fix_remaining.sh
```

### 2.3 Ledger Directories

**Found:**
```
devnet-ledger/
mainnet-ledger/
test-ledger/
test-devnet-ledger/
```

**Analysis:** These match `*-ledger/` in .gitignore but still exist locally

**Action:**
```bash
# Not tracked in git, safe to remove locally
rm -rf *-ledger/
```

### 2.4 Screenshot Files

**Found:**
```
osvm-docs-fixed.png
osvm-docs-screenshot.png
```

**Analysis:** Match .gitignore pattern `osvm-docs-*.png`

**Action:**
```bash
rm -f osvm-docs-*.png
```

### 2.5 Shell Scripts Organization

**Current:** 15+ shell scripts at root level

**Scripts to Keep at Root:**
- `install.sh` - Primary installation
- `install-release.sh` - Release installation
- `install-pre-commit-hook.sh` - Development setup
- `Makefile` - Build system

**Scripts to Move to `scripts/`:**
```bash
mv update-transitive-dependencies.sh scripts/deps/
mv update-solana-dependencies.sh scripts/deps/
mv update-all-dependencies.sh scripts/deps/
mv manage-local-rpc.sh scripts/dev/
mv agave-tunnel-manager.sh scripts/dev/
mv build-docker.sh scripts/build/
mv docker-build.sh scripts/build/
mv build-rootfs.sh scripts/build/
mv setup-microvm-environment.sh scripts/setup/
mv demo_enhanced_ui.sh scripts/demo/
mv validate_full_history.sh scripts/validation/
```

---

## 3. Documentation Directory Cleanup

### 3.1 Backup HTML Files

**Found in `docs/`:**
```
index.html.bak
index-new.html
index-new.html.bak
index-old.html
index-old.html.bak
```

**Recommendation:**
```bash
cd docs/
rm -f index*.bak index-new.html* index-old.html*
```

### 3.2 JavaScript/CSS Files

**Current:**
- `docs/script.js` - Possibly obsolete?
- `docs/styles.css` - Possibly obsolete?
- `docs/assets/dos-terminal.css` - Active (DOS theme)
- `docs/assets/dos-interactive.js` - Active (DOS theme)
- `docs/css/multipage.css` - Active
- `docs/js/*.js` - Active (navigation, search, animations)

**Recommendation:** Verify if `script.js` and `styles.css` are referenced, if not, remove.

---

## 4. test_qa_categories/ Cleanup

### 4.1 Status File Explosion

**Found:** 29 markdown status files with redundant names:
```
ABSOLUTE_FINAL_VALIDATION.md
ABSOLUTELY_FINAL.md
CERTIFICATION.md
COMPLETION_STATUS.md
COMPLETION_SUMMARY.md
FINAL_QA_STATUS.md
FINAL_REPORT.txt
FINAL_STATUS.md
FINAL_VALIDATION.md
FINAL_VERIFICATION.txt
QA_COMPLETION_STATUS.md
STATUS.md
VALIDATION_ISSUES.md
VALIDATION_REPORT.md
VALIDATION_REPORT_V2.md
... and more
```

**Analysis:** This is a classic case of "final final FINAL_v2 really_final" syndrome

**Recommendation:**
```bash
cd test_qa_categories/

# Keep only essential files:
# - README.md (main index)
# - INDEX.md (tool registry)
# - QUICK_REFERENCE.md (if useful)

# Archive the rest
mkdir -p archive/
mv ABSOLUTE_FINAL_VALIDATION.md ABSOLUTELY_FINAL.md CERTIFICATION.md \
   COMPLETION_STATUS.md FINAL_* STATUS*.md VALIDATION_* \
   REGENERATION_PLAN.md SPEC_REVIEW.md GENERATION_SUMMARY.md \
   SYNTAX_FIX_SUMMARY.md MANUAL_FIX_GUIDE.md REAL_ISSUES_FOUND.md \
   ROUND_7_FINAL.md archive/
```

**Keep:**
- `README.md` - Main documentation
- `INDEX.md` - Tool registry
- Question files (`01_transaction_analysis/*.md`, etc.)

---

## 5. crates/ovsm/ Cleanup

### 5.1 Assessment/Status Files

**Found:** 9 assessment/status files:
```
COMPLETE_SUMMARY.md
FIXES_SUMMARY.md
HONEST_ASSESSMENT.md
IMPLEMENTATION_STATUS.md
QA_RUNNER_SELF_ASSESSMENT.md
QA_TEST_STATUS.md
SELF_ASSESSMENT_REPORT.md
TEST_RESULTS_SUMMARY.md
THIRD_ASSESSMENT.md
```

**Also:**
```
BUG_COMPARISON_ALWAYS_TRUE.md
BUG_REPORT_NESTED_CONTROL_FLOW.md
```

**Recommendation:**
```bash
cd crates/ovsm/

# Keep essential docs:
# - README.md
# - CHANGELOG.md
# - HOW_TO_USE.md
# - USAGE_GUIDE.md
# - QUICK_START.md
# - RELEASE_NOTES.md
# - PUBLISH_CHECKLIST.md
# - PUBLISHING.md
# - GITHUB_ACTIONS_SETUP.md

# Archive assessments and bug reports
mkdir -p docs/archive/
mv *ASSESSMENT*.md *STATUS*.md *SUMMARY*.md BUG_*.md docs/archive/
```

---

## 6. Examples Directory Review

### 6.1 Current Structure
```
examples/
├── firecracker_demo.rs
├── isolation_demo.rs
├── mcp_integration_demo.rs
├── test_command_planner.rs
├── simple_test_mcp_server.js
├── ISOLATION_GUIDE.md
├── mcp_microvm_usage.md
├── ovsm_scripts/ (directory)
├── simple_mcp_server/ (directory)
├── test_mcp_servers/ (directory)
└── github-actions/ (directory)
```

**Status:** Well-organized, no changes needed

### 6.2 ovsm_scripts Review

**Contents:** Example OVSM scripts
**Status:** Good - properly documented in README.md

---

## 7. Scripts Directory Organization

### 7.1 Current Structure
```
scripts/
├── benchmark-unikernel-performance.sh
├── build-guest-rootfs-flatpak.sh
├── build-guest-rootfs.sh
├── build-guest-rootfs-with-rust.sh
├── build-minimal-rootfs.sh
├── build-unikraft-tool-executor.sh
├── debug-microvm.sh
├── enhance_forensics.py
├── generate_qa_complete.rs
├── run-e2e-tests.sh
├── setup-unikraft-toolchain.sh
├── test_agent_chat_v2_comprehensive.sh
├── test_agent_chat_v2.sh
├── test-single-microvm.sh
├── test-unikernel-execution.sh
└── validate-unikernel-security.sh
```

### 7.2 Proposed Reorganization
```bash
scripts/
├── build/
│   ├── build-guest-rootfs-flatpak.sh
│   ├── build-guest-rootfs.sh
│   ├── build-guest-rootfs-with-rust.sh
│   ├── build-minimal-rootfs.sh
│   └── build-unikraft-tool-executor.sh
├── test/
│   ├── run-e2e-tests.sh
│   ├── test_agent_chat_v2_comprehensive.sh
│   ├── test_agent_chat_v2.sh
│   ├── test-single-microvm.sh
│   └── test-unikernel-execution.sh
├── setup/
│   └── setup-unikraft-toolchain.sh
├── debug/
│   └── debug-microvm.sh
├── benchmark/
│   └── benchmark-unikernel-performance.sh
├── validation/
│   └── validate-unikernel-security.sh
└── qa/
    ├── enhance_forensics.py
    └── generate_qa_complete.rs
```

---

## 8. Tests Directory Review

### 8.1 Current Structure
```
tests/
├── e2e/
│   └── README.md
├── integration/
│   └── (test files)
├── README.md
└── (*.rs test files - 50 total)
```

**Status:** Some tests disabled (*.rs.disabled), documented in FINAL_TEST_STATUS.md

**Recommendation:** No changes needed - tests are well-documented

---

## 9. Implementation Plan

### Phase 1: Critical Fixes ✅
- [x] Fix Dockerfile (DONE)

### Phase 2: Safe Cleanup (No Git Changes)
```bash
# Remove untracked temporary files
rm -f *.log agave-validator-*.log osvm_chat_*.log
rm -f osvm_chat_export_*.json
rm -f snapshot_*.txt stderr.txt stdout.txt
rm -f complete_*.py fix_syntax.py generate_qa.py
rm -f osvm-docs-*.png
rm -rf *-ledger/

# Remove backup files in docs
cd docs/ && rm -f index*.bak index-new.html* index-old.html*
```

### Phase 3: Archive Session Notes
```bash
# Move session summaries to archive
mkdir -p docs/archive/session-notes
mv AI_CONTEXT.md FINAL_SESSION_SUMMARY.md FINAL_TEST_STATUS.md \
   SESSION_SUMMARY.md QA_PROGRESS_REPORT.md UI_ENHANCEMENTS.md \
   ULTIMATE_DECODER_SUITE.md docs/archive/session-notes/
```

### Phase 4: Clean test_qa_categories/
```bash
cd test_qa_categories/
mkdir -p archive/
mv ABSOLUTE_FINAL_VALIDATION.md ABSOLUTELY_FINAL.md CERTIFICATION.md \
   COMPLETION_STATUS.md FINAL_* STATUS*.md VALIDATION_* REGENERATION_PLAN.md \
   SPEC_REVIEW.md GENERATION_SUMMARY.md SYNTAX_FIX_SUMMARY.md \
   MANUAL_FIX_GUIDE.md REAL_ISSUES_FOUND.md ROUND_7_FINAL.md \
   CRITICAL_ISSUES*.md DEDUPLICATION*.md CONTENT_DEDUPLICATION*.md \
   FILE_STATUS.md STATUS_AND_NEXT_STEPS.md archive/
```

### Phase 5: Clean crates/ovsm/
```bash
cd crates/ovsm/
mkdir -p docs/archive/
mv *ASSESSMENT*.md *STATUS*.md *SUMMARY*.md BUG_*.md docs/archive/
```

### Phase 6: Reorganize Root Scripts
```bash
mkdir -p scripts/{deps,dev,build,setup,demo,validation}
mv update-*-dependencies.sh scripts/deps/
mv manage-local-rpc.sh agave-tunnel-manager.sh scripts/dev/
mv build-docker.sh docker-build.sh build-rootfs.sh scripts/build/
mv setup-microvm-environment.sh scripts/setup/
mv demo_enhanced_ui.sh scripts/demo/
mv validate_full_history.sh scripts/validation/
```

### Phase 7: Reorganize scripts/ Directory
```bash
cd scripts/
mkdir -p build test setup debug benchmark validation qa
# Move files as per section 7.2
```

### Phase 8: Update .gitignore Enforcement
```bash
# Ensure .gitignore patterns are respected
git rm --cached -r *-ledger/ 2>/dev/null || true
git rm --cached *.log 2>/dev/null || true
git rm --cached osvm_chat_export_*.json 2>/dev/null || true
```

### Phase 9: Documentation Updates
- Update CLAUDE.md if any paths changed
- Update README.md if necessary
- Create CHANGELOG entry for reorganization

---

## 10. Verification Checklist

After reorganization:
- [ ] `cargo build --release` succeeds
- [ ] `cargo test` passes
- [ ] Docker build succeeds
- [ ] All documentation links valid
- [ ] CLAUDE.md paths updated
- [ ] Git status clean
- [ ] CI/CD pipeline green

---

## 11. Risk Assessment

**Low Risk:**
- Removing untracked temporary files
- Archiving session notes
- Organizing scripts directory

**Medium Risk:**
- Moving tracked session summary files
- Cleaning test_qa_categories status files

**High Risk:**
- None identified (critical Dockerfile fix already applied)

---

## 12. Rollback Plan

**For Archived Files:**
```bash
# Restore from archive if needed
cp -r docs/archive/session-notes/* .
cp -r test_qa_categories/archive/* test_qa_categories/
cp -r crates/ovsm/docs/archive/* crates/ovsm/
```

**For Git Changes:**
```bash
git reset --hard HEAD  # Before commit
git revert <commit>    # After commit
```

---

## 13. Expected Outcomes

**Before:**
- 150+ files at various locations
- Cluttered root directory (30+ files)
- Redundant status documentation
- Untracked files violating .gitignore

**After:**
- Clean root directory (~15 essential files)
- Organized script structure
- Archived historical notes
- Enforced .gitignore rules
- Improved discoverability

---

## 14. Next Steps

1. **Review this plan** - Approve/modify sections
2. **Execute Phase 2** - Safe cleanup of untracked files
3. **Execute Phase 3-7** - Structural reorganization
4. **Test thoroughly** - Run verification checklist
5. **Commit changes** - With clear commit message
6. **Update documentation** - Reflect new structure

---

**Status:** Ready for approval and execution
**Time Estimate:** 30-45 minutes for full execution
**Reversibility:** High (most changes are moves/archives, not deletions)
