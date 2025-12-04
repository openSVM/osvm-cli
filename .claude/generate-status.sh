#!/bin/bash
# Generates comprehensive codebase health report for .claude/status.md
# Includes: version, clippy analysis, build health, code metrics, recommendations
#
# Usage: ./.claude/generate-status.sh [--quick]
#   --quick: Skip slow operations (clippy, build check)

set -e
cd "$(dirname "$0")/.."

OUTPUT=".claude/status.md"
QUICK_MODE=false
CLIPPY_OUTPUT="/tmp/osvm-clippy-$$"
BUILD_OUTPUT="/tmp/osvm-build-$$"

# Parse args
if [[ "$1" == "--quick" ]]; then
    QUICK_MODE=true
fi

# Cleanup on exit
trap "rm -f $CLIPPY_OUTPUT $BUILD_OUTPUT" EXIT

# Helper function to count occurrences
count_pattern() {
    grep -r "$1" src/ crates/ 2>/dev/null | wc -l || echo 0
}

echo "Generating codebase health report..."

cat > "$OUTPUT" << 'HEADER'
# Codebase Health Report (Auto-Generated)

This report provides actionable insights for Claude Code agents working on this repository.

HEADER

echo "Generated: $(date -u +"%Y-%m-%d %H:%M:%S UTC")" >> "$OUTPUT"
echo "" >> "$OUTPUT"

# =============================================================================
# SECTION 1: Basic Info
# =============================================================================
echo "## Overview" >> "$OUTPUT"
echo "" >> "$OUTPUT"

VERSION=$(grep '^version' Cargo.toml | head -1 | cut -d'"' -f2)
BRANCH=$(git branch --show-current 2>/dev/null || echo 'unknown')
MODIFIED=$(git status --porcelain 2>/dev/null | wc -l)

echo "| Metric | Value |" >> "$OUTPUT"
echo "|--------|-------|" >> "$OUTPUT"
echo "| Version | $VERSION |" >> "$OUTPUT"
echo "| Branch | $BRANCH |" >> "$OUTPUT"
echo "| Modified Files | $MODIFIED |" >> "$OUTPUT"

# Count lines of code
if command -v tokei &> /dev/null; then
    RUST_LINES=$(tokei -t Rust -o json 2>/dev/null | jq '.Rust.code // 0' 2>/dev/null || echo "N/A")
elif command -v cloc &> /dev/null; then
    RUST_LINES=$(cloc --json src/ crates/ 2>/dev/null | jq '.Rust.code // 0' 2>/dev/null || echo "N/A")
else
    RUST_LINES=$(find src/ crates/ -name "*.rs" -exec cat {} \; 2>/dev/null | wc -l)
fi
echo "| Rust Lines | ~$RUST_LINES |" >> "$OUTPUT"
echo "" >> "$OUTPUT"

# =============================================================================
# SECTION 2: Build Health
# =============================================================================
echo "## Build Health" >> "$OUTPUT"
echo "" >> "$OUTPUT"

if [[ "$QUICK_MODE" == "false" ]]; then
    echo "Running cargo check..."
    if cargo check --quiet 2>"$BUILD_OUTPUT"; then
        echo "✅ **Build Status:** PASSING" >> "$OUTPUT"
        BUILD_OK=true
    else
        echo "❌ **Build Status:** FAILING" >> "$OUTPUT"
        BUILD_OK=false
        echo "" >> "$OUTPUT"
        echo "**Build Errors:**" >> "$OUTPUT"
        echo '```' >> "$OUTPUT"
        head -30 "$BUILD_OUTPUT" >> "$OUTPUT"
        echo '```' >> "$OUTPUT"
    fi
else
    echo "⏭️ **Build Status:** Skipped (--quick mode)" >> "$OUTPUT"
    BUILD_OK="unknown"
fi
echo "" >> "$OUTPUT"

# =============================================================================
# SECTION 3: Clippy Analysis
# =============================================================================
echo "## Code Quality (Clippy)" >> "$OUTPUT"
echo "" >> "$OUTPUT"

if [[ "$QUICK_MODE" == "false" ]]; then
    echo "Running cargo clippy..."
    # Run clippy and capture output (allow failures since we want to count warnings)
    cargo clippy --message-format=short 2>&1 | tee "$CLIPPY_OUTPUT" > /dev/null || true

    # Count different warning types (ensure single integer output)
    WARNINGS=$(grep -c "warning:" "$CLIPPY_OUTPUT" 2>/dev/null | tr -d '\n' || echo 0)
    ERRORS=$(grep -c "error\[" "$CLIPPY_OUTPUT" 2>/dev/null | tr -d '\n' || echo 0)
    # Ensure numeric values
    WARNINGS=${WARNINGS:-0}
    ERRORS=${ERRORS:-0}

    # Extract unique warning categories
    CLIPPY_CATEGORIES=$(grep -oE "warning: [a-z_]+" "$CLIPPY_OUTPUT" 2>/dev/null | sort | uniq -c | sort -rn | head -10)

    if [[ $ERRORS -gt 0 ]]; then
        echo "❌ **Clippy Status:** $ERRORS errors, $WARNINGS warnings" >> "$OUTPUT"
    elif [[ $WARNINGS -gt 50 ]]; then
        echo "⚠️ **Clippy Status:** $WARNINGS warnings (high)" >> "$OUTPUT"
    elif [[ $WARNINGS -gt 0 ]]; then
        echo "🔶 **Clippy Status:** $WARNINGS warnings" >> "$OUTPUT"
    else
        echo "✅ **Clippy Status:** Clean" >> "$OUTPUT"
    fi
    echo "" >> "$OUTPUT"

    if [[ -n "$CLIPPY_CATEGORIES" && $WARNINGS -gt 0 ]]; then
        echo "**Top Warning Categories:**" >> "$OUTPUT"
        echo '```' >> "$OUTPUT"
        echo "$CLIPPY_CATEGORIES" | head -8 >> "$OUTPUT"
        echo '```' >> "$OUTPUT"
        echo "" >> "$OUTPUT"
    fi
else
    echo "⏭️ **Clippy Status:** Skipped (--quick mode)" >> "$OUTPUT"
    echo "" >> "$OUTPUT"
    WARNINGS=0
fi

# =============================================================================
# SECTION 4: Code Metrics & Patterns
# =============================================================================
echo "## Code Metrics" >> "$OUTPUT"
echo "" >> "$OUTPUT"

# Count important patterns
UNWRAP_COUNT=$(count_pattern "\.unwrap()" | tr -d ' ')
EXPECT_COUNT=$(count_pattern "\.expect(" | tr -d ' ')
TODO_COUNT=$(count_pattern "TODO" | tr -d ' ')
FIXME_COUNT=$(count_pattern "FIXME" | tr -d ' ')
UNSAFE_COUNT=$(count_pattern "unsafe " | tr -d ' ')
PANIC_COUNT=$(count_pattern "panic!" | tr -d ' ')
CLONE_COUNT=$(count_pattern "\.clone()" | tr -d ' ')

echo "| Pattern | Count | Status |" >> "$OUTPUT"
echo "|---------|-------|--------|" >> "$OUTPUT"

# Evaluate each metric with thresholds
if [[ $UNWRAP_COUNT -gt 200 ]]; then
    echo "| \`.unwrap()\` | $UNWRAP_COUNT | ⚠️ High - consider \`?\` operator |" >> "$OUTPUT"
else
    echo "| \`.unwrap()\` | $UNWRAP_COUNT | ✅ Acceptable |" >> "$OUTPUT"
fi

if [[ $TODO_COUNT -gt 50 ]]; then
    echo "| \`TODO\` | $TODO_COUNT | ⚠️ High - address or remove stale |" >> "$OUTPUT"
else
    echo "| \`TODO\` | $TODO_COUNT | ✅ Normal |" >> "$OUTPUT"
fi

if [[ $FIXME_COUNT -gt 10 ]]; then
    echo "| \`FIXME\` | $FIXME_COUNT | ⚠️ Address these |" >> "$OUTPUT"
else
    echo "| \`FIXME\` | $FIXME_COUNT | ✅ Low |" >> "$OUTPUT"
fi

if [[ $UNSAFE_COUNT -gt 20 ]]; then
    echo "| \`unsafe\` | $UNSAFE_COUNT | ⚠️ Review needed |" >> "$OUTPUT"
else
    echo "| \`unsafe\` | $UNSAFE_COUNT | ✅ Acceptable |" >> "$OUTPUT"
fi

if [[ $PANIC_COUNT -gt 30 ]]; then
    echo "| \`panic!\` | $PANIC_COUNT | ⚠️ Consider Result types |" >> "$OUTPUT"
else
    echo "| \`panic!\` | $PANIC_COUNT | ✅ Acceptable |" >> "$OUTPUT"
fi

echo "| \`.expect()\` | $EXPECT_COUNT | ℹ️ Info |" >> "$OUTPUT"
echo "| \`.clone()\` | $CLONE_COUNT | ℹ️ Info |" >> "$OUTPUT"
echo "" >> "$OUTPUT"

# =============================================================================
# SECTION 5: Dependency Health
# =============================================================================
echo "## Dependencies" >> "$OUTPUT"
echo "" >> "$OUTPUT"

# Count total dependencies
DEP_COUNT=$(grep -c "^[a-z]" Cargo.toml 2>/dev/null | head -1 || echo "N/A")
echo "**Total dependencies:** ~$DEP_COUNT (in root Cargo.toml)" >> "$OUTPUT"
echo "" >> "$OUTPUT"

echo "**Key dependencies:**" >> "$OUTPUT"
echo '```' >> "$OUTPUT"
grep -E '^(solana-|tokio|clap|ratatui|diesel|reqwest|serde)' Cargo.toml | head -10 >> "$OUTPUT"
echo '```' >> "$OUTPUT"
echo "" >> "$OUTPUT"

# Check for duplicate dependencies
if command -v cargo &> /dev/null; then
    DUPLICATES=$(cargo tree -d 2>/dev/null | head -5 || echo "")
    if [[ -n "$DUPLICATES" ]]; then
        echo "**Duplicate dependencies (first 5):**" >> "$OUTPUT"
        echo '```' >> "$OUTPUT"
        echo "$DUPLICATES" >> "$OUTPUT"
        echo '```' >> "$OUTPUT"
        echo "" >> "$OUTPUT"
    fi
fi

# =============================================================================
# SECTION 6: Git Status
# =============================================================================
echo "## Git Status" >> "$OUTPUT"
echo "" >> "$OUTPUT"

echo "**Recent Commits:**" >> "$OUTPUT"
echo '```' >> "$OUTPUT"
git log --oneline -5 2>/dev/null >> "$OUTPUT" || echo "git not available" >> "$OUTPUT"
echo '```' >> "$OUTPUT"
echo "" >> "$OUTPUT"

if [[ $MODIFIED -gt 0 ]]; then
    echo "**Modified Files by Category:**" >> "$OUTPUT"
    echo '```' >> "$OUTPUT"
    git status --porcelain 2>/dev/null | cut -c4- | sed 's|/.*||' | sort | uniq -c | sort -rn | head -10 >> "$OUTPUT"
    echo '```' >> "$OUTPUT"
    echo "" >> "$OUTPUT"
fi

# =============================================================================
# SECTION 7: Recommendations
# =============================================================================
echo "## Recommendations" >> "$OUTPUT"
echo "" >> "$OUTPUT"

RECOMMENDATIONS=()

# Build recommendations based on metrics
if [[ "$BUILD_OK" == "false" ]]; then
    RECOMMENDATIONS+=("🔴 **CRITICAL:** Fix build errors before any other work")
fi

if [[ $WARNINGS -gt 100 ]]; then
    RECOMMENDATIONS+=("🟡 **HIGH:** Consider running \`cargo clippy --fix\` to auto-fix simple warnings")
fi

if [[ $UNWRAP_COUNT -gt 200 ]]; then
    RECOMMENDATIONS+=("🟡 **MEDIUM:** High \`.unwrap()\` count ($UNWRAP_COUNT). Replace with \`?\` operator or \`.unwrap_or_default()\` in non-critical paths")
fi

if [[ $TODO_COUNT -gt 50 ]]; then
    RECOMMENDATIONS+=("🟡 **MEDIUM:** $TODO_COUNT TODOs in codebase. Review and remove stale ones")
fi

if [[ $FIXME_COUNT -gt 10 ]]; then
    RECOMMENDATIONS+=("🟡 **MEDIUM:** $FIXME_COUNT FIXMEs need attention")
fi

if [[ $UNSAFE_COUNT -gt 20 ]]; then
    RECOMMENDATIONS+=("🟡 **MEDIUM:** $UNSAFE_COUNT unsafe blocks. Ensure each has safety documentation")
fi

if [[ $MODIFIED -gt 50 ]]; then
    RECOMMENDATIONS+=("🔵 **INFO:** $MODIFIED modified files. Consider committing or stashing to reduce cognitive load")
fi

if [[ $PANIC_COUNT -gt 30 ]]; then
    RECOMMENDATIONS+=("🔵 **INFO:** $PANIC_COUNT \`panic!\` calls. Consider replacing with \`Result\` for library code")
fi

# Output recommendations
if [[ ${#RECOMMENDATIONS[@]} -eq 0 ]]; then
    echo "✅ **No critical issues detected.** Codebase is in good health." >> "$OUTPUT"
else
    echo "| Priority | Recommendation |" >> "$OUTPUT"
    echo "|----------|----------------|" >> "$OUTPUT"
    for rec in "${RECOMMENDATIONS[@]}"; do
        echo "| ${rec} |" >> "$OUTPUT"
    done
fi
echo "" >> "$OUTPUT"

# =============================================================================
# SECTION 8: Quick Actions
# =============================================================================
echo "## Quick Actions" >> "$OUTPUT"
echo "" >> "$OUTPUT"
echo '```bash' >> "$OUTPUT"
echo "# Fix formatting" >> "$OUTPUT"
echo "cargo fmt --all" >> "$OUTPUT"
echo "" >> "$OUTPUT"
echo "# Auto-fix simple clippy warnings" >> "$OUTPUT"
echo "cargo clippy --fix --allow-dirty" >> "$OUTPUT"
echo "" >> "$OUTPUT"
echo "# Run tests" >> "$OUTPUT"
echo "cargo test --lib --bins" >> "$OUTPUT"
echo "" >> "$OUTPUT"
echo "# Check for outdated dependencies" >> "$OUTPUT"
echo "cargo outdated  # requires: cargo install cargo-outdated" >> "$OUTPUT"
echo '```' >> "$OUTPUT"
echo "" >> "$OUTPUT"

# =============================================================================
# SECTION 9: Workspace Summary
# =============================================================================
echo "## Workspace Members" >> "$OUTPUT"
echo "" >> "$OUTPUT"
echo '```' >> "$OUTPUT"
grep -A 5 '^\[workspace\]' Cargo.toml | grep 'members' | sed 's/members = //' >> "$OUTPUT"
echo '```' >> "$OUTPUT"
echo "" >> "$OUTPUT"

echo "---" >> "$OUTPUT"
echo "*Report generated by \`.claude/generate-status.sh\`. Run with \`--quick\` to skip clippy/build checks.*" >> "$OUTPUT"

echo ""
echo "✅ Health report written to $OUTPUT"
echo ""

# Print summary to terminal
echo "=== SUMMARY ==="
echo "Version: $VERSION"
echo "Branch: $BRANCH"
echo "Modified files: $MODIFIED"
if [[ "$QUICK_MODE" == "false" ]]; then
    echo "Clippy warnings: $WARNINGS"
    echo "Build: $BUILD_OK"
fi
echo "Recommendations: ${#RECOMMENDATIONS[@]}"
