#!/bin/bash
echo "╔══════════════════════════════════════════════════════════════════╗"
echo "║              QA CATEGORIES - COMPREHENSIVE CHECK                 ║"
echo "╚══════════════════════════════════════════════════════════════════╝"
echo ""

for dir in */; do
    if [[ "$dir" == "06_token_research/" ]]; then
        echo "✅ $dir (COMPLETED)"
        echo "   Files: $(find "$dir" -name "*.md" ! -name "*STATUS*" ! -name "*SUMMARY*" | wc -l)"
        total=0
        for file in "$dir"/*.md; do
            if [[ ! "$file" =~ (STATUS|SUMMARY|PROJECT) ]]; then
                count=$(grep -c "^## Q" "$file" 2>/dev/null || echo "0")
                if [[ $count -gt 0 ]]; then
                    total=$((total + count))
                    echo "   - $(basename "$file"): $count questions"
                fi
            fi
        done
        echo "   Total: $total questions"
        echo ""
    else
        echo "📂 $dir"
        md_files=$(find "$dir" -name "*.md" -type f 2>/dev/null | wc -l)
        if [[ $md_files -eq 0 ]]; then
            echo "   ⚠️  No markdown files"
        else
            echo "   Files: $md_files"
            total=0
            for file in "$dir"*.md; do
                if [[ -f "$file" ]]; then
                    count=$(grep -c "^## Q" "$file" 2>/dev/null || echo "0")
                    if [[ $count -gt 0 ]]; then
                        total=$((total + count))
                        echo "   - $(basename "$file"): $count questions"
                    fi
                fi
            done
            if [[ $total -gt 0 ]]; then
                echo "   Total: $total questions"
            else
                echo "   ⚠️  No questions found (may need ## Q header format)"
            fi
        fi
        echo ""
    fi
done
