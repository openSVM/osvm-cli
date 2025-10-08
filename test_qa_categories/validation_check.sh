#!/bin/bash

echo "=== Solana Research Q&A Dataset Validation ==="
echo ""

# Check directory structure
echo "1. Directory Structure:"
for i in {01..10}; do
    dir_count=$(find . -maxdepth 1 -type d -name "${i}_*" | wc -l)
    if [ $dir_count -eq 1 ]; then
        echo "   ✓ Category ${i} exists"
    else
        echo "   ✗ Category ${i} missing or duplicate"
    fi
done
echo ""

# Check file counts
echo "2. File Counts per Category:"
total_files=0
for dir in {01..10}_*/; do
    file_count=$(ls "$dir"*.md 2>/dev/null | wc -l)
    total_files=$((total_files + file_count))
    if [ $file_count -eq 10 ]; then
        echo "   ✓ $(basename "$dir"): $file_count files"
    else
        echo "   ✗ $(basename "$dir"): $file_count files (expected 10)"
    fi
done
echo "   Total files: $total_files (expected 100)"
echo ""

# Check question counts
echo "3. Question Counts:"
total_questions=$(grep -r "^## Q" {01..10}_*/ 2>/dev/null | wc -l)
echo "   Total questions: $total_questions (expected 10,000)"

for dir in {01..10}_*/; do
    q_count=$(grep -r "^## Q" "$dir" 2>/dev/null | wc -l)
    if [ $q_count -eq 1000 ]; then
        echo "   ✓ $(basename "$dir"): $q_count questions"
    else
        echo "   ✗ $(basename "$dir"): $q_count questions (expected 1000)"
    fi
done
echo ""

# Check OVSM structure
echo "4. OVSM Structure Validation:"
expected_plan_count=$(grep -r "^\*\*Expected Plan:\*\*" {01..10}_*/ 2>/dev/null | wc -l)
available_tools_count=$(grep -r "^\*\*Available Tools:\*\*" {01..10}_*/ 2>/dev/null | wc -l)
main_branch_count=$(grep -r "^\*\*Main Branch:\*\*" {01..10}_*/ 2>/dev/null | wc -l)
decision_point_count=$(grep -r "^\*\*Decision Point:\*\*" {01..10}_*/ 2>/dev/null | wc -l)
action_count=$(grep -r "^\*\*Action:\*\*" {01..10}_*/ 2>/dev/null | wc -l)

echo "   Expected Plan sections: $expected_plan_count (expected 10,000)"
echo "   Available Tools sections: $available_tools_count (expected 10,000)"
echo "   Main Branch sections: $main_branch_count (expected 10,000)"
echo "   Decision Point sections: $decision_point_count (expected 10,000)"
echo "   Action sections: $action_count (expected 10,000)"
echo ""

# Check file sizes
echo "5. File Size Summary:"
for dir in {01..10}_*/; do
    size=$(du -sh "$dir" | cut -f1)
    echo "   $(basename "$dir"): $size"
done
echo ""

total_size=$(du -sh . | cut -f1)
echo "   Total size: $total_size"
echo ""

# Summary
echo "=== Validation Complete ==="
if [ $total_questions -eq 10000 ] && [ $total_files -eq 100 ]; then
    echo "✅ All checks passed!"
else
    echo "⚠️  Some checks failed. Review output above."
fi

