#!/usr/bin/env python3
"""
OVSM Query Validator and Self-Ask Refinement Tool

This script validates, scores, and suggests refinements for OVSM queries
using the Self-Ask methodology.
"""

import os
import re
import json
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass, asdict


@dataclass
class ValidationResult:
    """Results from validating a query"""
    query_file: str
    correctness_score: int  # 0-40
    clarity_score: int      # 0-30
    educational_score: int  # 0-30
    total_score: int        # 0-100
    issues: List[str]
    suggestions: List[str]
    self_ask_questions: Dict[str, str]


class QueryValidator:
    """Validates and scores OVSM queries"""

    def __init__(self, base_dir: str):
        self.base_dir = Path(base_dir)
        self.results: List[ValidationResult] = []

    def parse_query_file(self, filepath: Path) -> Dict[str, str]:
        """Extract metadata and code from query file"""
        with open(filepath, 'r') as f:
            content = f.read()

        # Extract comments
        query_match = re.search(r';; Query: (.+)', content)
        category_match = re.search(r';; Category: (.+)', content)
        expected_match = re.search(r';; Expected: (.+)', content)

        # Extract code (everything after the header comments)
        code_start = content.find('\n\n')
        code = content[code_start:].strip() if code_start != -1 else ""

        return {
            'query_desc': query_match.group(1) if query_match else "",
            'category': category_match.group(1) if category_match else "",
            'expected': expected_match.group(1) if expected_match else "",
            'code': code,
            'full_content': content
        }

    def score_correctness(self, metadata: Dict[str, str]) -> Tuple[int, List[str]]:
        """Score correctness (0-40 points)"""
        score = 0
        issues = []

        # Has expected output (10 pts)
        if metadata['expected'] and metadata['expected'] != "":
            score += 10
        else:
            issues.append("Missing expected output")

        # Has code (10 pts)
        if len(metadata['code']) > 0:
            score += 10
        else:
            issues.append("No code found")

        # Proper LISP syntax (10 pts)
        code = metadata['code']
        if '(' in code and ')' in code:
            score += 10
        else:
            issues.append("Missing LISP parentheses")

        # Has clear test condition (10 pts)
        if metadata['expected'] and any(term in metadata['expected'].lower()
                                       for term in ['true', 'false', 'null', 'number', 'string', 'array', 'object']):
            score += 10
        else:
            issues.append("Expected output not clearly typed")

        return score, issues

    def score_clarity(self, metadata: Dict[str, str]) -> Tuple[int, List[str]]:
        """Score clarity (0-30 points)"""
        score = 0
        issues = []

        # Clear description (10 pts)
        desc = metadata['query_desc']
        if desc and len(desc) > 10 and '?' in desc or 'Calculate' in desc or 'Check' in desc:
            score += 10
        elif desc and len(desc) > 5:
            score += 5
            issues.append("Description could be more specific")
        else:
            issues.append("Description too vague or missing")

        # Descriptive variable names (10 pts)
        code = metadata['code']
        # Check for meaningful names vs single letters
        good_names = re.findall(r'\(define ([a-z][a-z-]+)', code)
        bad_names = re.findall(r'\(define ([a-z])\s', code)

        if good_names and len(good_names) > len(bad_names):
            score += 10
        elif good_names:
            score += 5
            issues.append("Some variable names could be more descriptive")
        else:
            if bad_names:
                issues.append("Use descriptive variable names instead of single letters")

        # Has inline comments (10 pts)
        comment_lines = [line for line in code.split('\n') if line.strip().startswith(';;')]
        if len(comment_lines) >= 2:
            score += 10
        elif len(comment_lines) == 1:
            score += 5
            issues.append("Could use more inline comments")
        else:
            issues.append("Add inline comments to explain complex logic")

        return score, issues

    def score_educational(self, metadata: Dict[str, str]) -> Tuple[int, List[str]]:
        """Score educational value (0-30 points)"""
        score = 0
        issues = []

        # Teaches a clear concept (10 pts)
        desc = metadata['query_desc'].lower()
        code = metadata['code']

        concepts = ['loop', 'variable', 'function', 'array', 'object',
                   'conditional', 'iteration', 'recursion', 'pattern']
        if any(concept in desc for concept in concepts):
            score += 10
        else:
            issues.append("Unclear what concept this teaches")

        # Shows best practice (10 pts)
        # Check for good patterns
        good_patterns = [
            r'\(define .+ .+\)',  # Variable definitions
            r'\(if .+ .+ .+\)',   # Proper if structure
            r'\(for \(.+ .+\)',   # For loop structure
        ]

        matches = sum(1 for pattern in good_patterns if re.search(pattern, code))
        if matches >= 2:
            score += 10
        elif matches == 1:
            score += 5

        # Avoids anti-patterns (10 pts)
        # Check for bad patterns
        bad_patterns = [
            r'\(define [a-z]\s',  # Single-letter variables
            r';; TODO',           # Unfinished code
            r';; FIXME',          # Known issues
        ]

        bad_matches = sum(1 for pattern in bad_patterns if re.search(pattern, code))
        if bad_matches == 0:
            score += 10
        else:
            score += max(0, 10 - (bad_matches * 3))
            issues.append(f"Found {bad_matches} anti-pattern(s)")

        return score, issues

    def generate_self_ask_questions(self, metadata: Dict[str, str], score: int) -> Dict[str, str]:
        """Generate self-ask questions based on validation"""
        questions = {}

        # Correctness questions
        questions['Q1_Parse'] = "Does this query use valid LISP syntax?"
        questions['Q2_Execute'] = "Will this execute without runtime errors?"
        questions['Q3_Output'] = f"Does it produce: {metadata['expected']}?"
        questions['Q4_EdgeCases'] = "What edge cases should be tested?"

        # Clarity questions
        questions['Q6_Description'] = f"Is '{metadata['query_desc']}' clear enough?"
        questions['Q7_Variables'] = "Are all variable names descriptive?"
        questions['Q9_Comments'] = "Are there enough inline comments?"

        # Educational questions
        questions['Q21_Teaching'] = "What core concept does this demonstrate?"
        questions['Q22_Prerequisites'] = "What should learners know before this?"
        questions['Q23_NextSteps'] = "What query should come after this?"

        # Score-based questions
        if score < 70:
            questions['Q_Improvement'] = "How can we raise the score above 70?"
        if score >= 85:
            questions['Q_Excellence'] = "What makes this query particularly good?"

        return questions

    def generate_suggestions(self, metadata: Dict[str, str], issues: List[str]) -> List[str]:
        """Generate refinement suggestions"""
        suggestions = []

        # Suggestion based on issues
        if "Missing expected output" in ' '.join(issues):
            suggestions.append("Add ';; Expected: <result>' comment")

        if "single letter" in ' '.join(issues).lower():
            suggestions.append("Rename variables: x -> count, n -> number, etc.")

        if "inline comments" in ' '.join(issues).lower():
            suggestions.append("Add comments explaining the logic flow")

        if "vague" in ' '.join(issues).lower():
            suggestions.append("Make description more specific: 'Calculate X using Y technique'")

        # Positive suggestions
        code = metadata['code']
        if '(while' in code and '(if' in code:
            suggestions.append("✅ Great! Demonstrates the critical parser fix")

        if len(code.split('\n')) > 8:
            suggestions.append("Consider splitting into simpler queries")

        if 'define' in code and 'set!' in code:
            suggestions.append("✅ Shows both immutable and mutable patterns")

        return suggestions

    def validate_query(self, filepath: Path) -> ValidationResult:
        """Validate a single query file"""
        metadata = self.parse_query_file(filepath)

        # Score each dimension
        correctness, correctness_issues = self.score_correctness(metadata)
        clarity, clarity_issues = self.score_clarity(metadata)
        educational, educational_issues = self.score_educational(metadata)

        total_score = correctness + clarity + educational
        all_issues = correctness_issues + clarity_issues + educational_issues

        # Generate self-ask questions and suggestions
        questions = self.generate_self_ask_questions(metadata, total_score)
        suggestions = self.generate_suggestions(metadata, all_issues)

        result = ValidationResult(
            query_file=filepath.name,
            correctness_score=correctness,
            clarity_score=clarity,
            educational_score=educational,
            total_score=total_score,
            issues=all_issues,
            suggestions=suggestions,
            self_ask_questions=questions
        )

        self.results.append(result)
        return result

    def validate_all(self) -> List[ValidationResult]:
        """Validate all queries in the directory"""
        categories = ['basic', 'loops', 'data_structures', 'advanced']

        for category in categories:
            category_path = self.base_dir / category
            if not category_path.exists():
                continue

            for query_file in sorted(category_path.glob('*.ovsm')):
                self.validate_query(query_file)

        return self.results

    def generate_report(self) -> str:
        """Generate validation report"""
        if not self.results:
            return "No results to report"

        # Calculate statistics
        total = len(self.results)
        avg_score = sum(r.total_score for r in self.results) / total
        high_scorers = [r for r in self.results if r.total_score >= 85]
        low_scorers = [r for r in self.results if r.total_score < 70]

        report = f"""
# OVSM Query Validation Report

## Summary Statistics

- **Total Queries:** {total}
- **Average Score:** {avg_score:.1f}/100
- **High Quality (≥85):** {len(high_scorers)} queries ({len(high_scorers)/total*100:.1f}%)
- **Needs Improvement (<70):** {len(low_scorers)} queries ({len(low_scorers)/total*100:.1f}%)

## Score Distribution

"""

        # Score breakdown
        correctness_avg = sum(r.correctness_score for r in self.results) / total
        clarity_avg = sum(r.clarity_score for r in self.results) / total
        educational_avg = sum(r.educational_score for r in self.results) / total

        report += f"""
| Dimension     | Average | Max |
|---------------|---------|-----|
| Correctness   | {correctness_avg:.1f}/40 | 40  |
| Clarity       | {clarity_avg:.1f}/30 | 30  |
| Educational   | {educational_avg:.1f}/30 | 30  |
| **Total**     | **{avg_score:.1f}/100** | **100** |

"""

        # Top performers
        report += "\n## Top 10 Queries\n\n"
        top_queries = sorted(self.results, key=lambda r: r.total_score, reverse=True)[:10]
        for i, result in enumerate(top_queries, 1):
            report += f"{i}. **{result.query_file}** - Score: {result.total_score}/100\n"

        # Queries needing improvement
        if low_scorers:
            report += "\n## Queries Needing Improvement\n\n"
            for result in sorted(low_scorers, key=lambda r: r.total_score)[:10]:
                report += f"- **{result.query_file}** - Score: {result.total_score}/100\n"
                for issue in result.issues[:3]:
                    report += f"  - ⚠️ {issue}\n"
                if result.suggestions:
                    report += f"  - 💡 {result.suggestions[0]}\n"

        # Common issues
        all_issues = {}
        for result in self.results:
            for issue in result.issues:
                all_issues[issue] = all_issues.get(issue, 0) + 1

        report += "\n## Most Common Issues\n\n"
        sorted_issues = sorted(all_issues.items(), key=lambda x: x[1], reverse=True)
        for issue, count in sorted_issues[:10]:
            report += f"- {issue}: {count} queries ({count/total*100:.1f}%)\n"

        return report

    def export_results(self, output_path: str):
        """Export results to JSON"""
        with open(output_path, 'w') as f:
            json.dump([asdict(r) for r in self.results], f, indent=2)


def main():
    """Main execution"""
    base_dir = "/home/larp/larpdevs/osvm-cli/crates/ovsm/agent_queries"

    print("🔍 OVSM Query Validator - Self-Ask Refinement Tool\n")
    print(f"Scanning directory: {base_dir}\n")

    validator = QueryValidator(base_dir)
    results = validator.validate_all()

    print(f"✅ Validated {len(results)} queries\n")

    # Generate and print report
    report = validator.generate_report()
    print(report)

    # Export results
    output_json = os.path.join(base_dir, "validation_results.json")
    validator.export_results(output_json)
    print(f"\n📊 Detailed results exported to: {output_json}")

    # Save report
    report_path = os.path.join(base_dir, "VALIDATION_REPORT.md")
    with open(report_path, 'w') as f:
        f.write(report)
    print(f"📝 Report saved to: {report_path}")


if __name__ == "__main__":
    main()
