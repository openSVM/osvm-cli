# Changelog

All notable changes to the OVSM crate will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.3] - 2025-10-26

### Changed
- 🎯 **Achieved 356/356 tests passing (100%)**
- ✅ Fixed all 5 varargs test failures (map/filter argument order, defun body wrapping)
- ✅ Fixed 2 doctest examples (reduce signature)
- 🗑️ Deleted 1,667 lines of obsolete Python syntax tests
- 🧹 Complete LISP-only codebase (zero Python syntax remnants)
- 📝 Updated all documentation examples to LISP syntax

### Removed
- Obsolete Python syntax test files:
  - `error_handling_tests.rs` (642 lines, 42 tests)
  - `integration_v1_1_0.rs` (463 lines, 18 tests)
  - `verify_no_silent_failures.rs` (349 lines, 13 tests)
  - `test_break_bug.rs` (145 lines, 7 tests)
  - `test_comparisons.rs` (68 lines, 3 tests)
- Python validation tool `query_validator.py` (388 lines)

## [1.0.0] - 2025-10-11

### Added
- ✨ Complete OVSM language interpreter
- 🔧 Scanner (lexer) with full token support
- 🌳 Parser with AST generation
- ⚡ Runtime evaluator with proper scoping
- 📝 Comprehensive documentation

#### Language Features
- **Control Flow**: IF/THEN/ELSE, FOR loops, WHILE loops
- **Loop Control**: BREAK, CONTINUE (including conditional variants)
- **Data Types**: Int, Float, String, Bool, Null, Arrays, Objects, Ranges
- **Operators**: Arithmetic (+, -, *, /, %, **), Comparison (<, >, <=, >=, ==, !=), Logical (AND, OR, NOT)
- **Variables**: Assignment, constants, proper scoping with shadowing
- **Expressions**: Ternary operator, IN operator, function calls
- **Return Statements**: Early return support

#### Tools & Examples
- 📦 Example runner (`run_file.rs`) for executing `.ovsm` scripts
- 🎮 Interactive REPL (`simple_repl.rs`) for experimentation
- 📚 6 example scripts demonstrating language features:
  - `hello_world.ovsm` - Basic greeting
  - `factorial.ovsm` - Recursive-style calculation
  - `fibonacci.ovsm` - Sequence generation
  - `array_operations.ovsm` - Array iteration and operations
  - `conditional_logic.ovsm` - Nested conditionals
  - `loop_control.ovsm` - BREAK and CONTINUE usage

#### Documentation
- 📖 `USAGE_GUIDE.md` - Complete language reference
- 🚀 `HOW_TO_USE.md` - Getting started guide
- 📝 `TEST_RESULTS_SUMMARY.md` - Implementation status
- 🔧 `PUBLISHING.md` - Publishing guide
- 📂 `examples/README.md` - Example documentation

#### Testing
- ✅ 107/110 tests passing (97.3% success rate)
- 🧪 65/65 runtime evaluator tests
- 🧪 42/42 parser tests
- 🧪 Comparison operator tests
- 🧪 BREAK/CONTINUE flow control tests

### Fixed
- 🐛 Critical parser bug: IF/FOR/WHILE incorrectly treated as block terminators
- 🐛 RETURN in IF branches causing empty THEN/ELSE blocks
- 🐛 Newline handling in loop body parsing
- 🔧 Nested control flow scope isolation
- 🔧 Variable scoping with proper shadowing

### Known Issues
- ⚠️ TRY/CATCH blocks have block termination issues (8 tests failing)
- ⚠️ Syntax ambiguity without explicit block delimiters
- ⚠️ Some advanced features not yet implemented (DECISION/BRANCH, lambdas, PARALLEL, etc.)

### Implementation Status

#### ✅ Production Ready (100% Working)
- Core control flow (IF/FOR/WHILE)
- All operators (arithmetic, comparison, logical)
- All basic data types
- Variable scoping and constants
- BREAK/CONTINUE flow control
- Nested constructs

#### ⚠️ Experimental (Has Issues)
- TRY/CATCH error handling (parsed but buggy)

#### ❌ Not Implemented
- DECISION/BRANCH constructs
- Lambda functions (`fn:` syntax)
- PARALLEL execution
- WAIT strategies (WAIT_ALL, WAIT_ANY, RACE)
- GUARD statements
- MATCH expressions
- Advanced tools (MAP, FILTER, REDUCE, SUM, MEAN, etc.)
- Network/RPC tools (getSlot, getBlock, etc.)

## [0.1.0] - 2025-10-10

### Added
- Initial project structure
- Basic lexer implementation
- Basic parser implementation
- Basic evaluator implementation

---

## Version History

| Version | Release Date | Status | Notes |
|---------|--------------|--------|-------|
| 1.0.0 | 2025-10-11 | Stable | Initial public release |
| 0.1.0 | 2025-10-10 | Alpha | Internal development |

---

## Migration Guides

### Upgrading to 1.0.0

This is the first public release. No migration needed.

---

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for contribution guidelines.

## License

Licensed under MIT License. See [LICENSE](../../LICENSE) for details.

---

[Unreleased]: https://github.com/opensvm/osvm-cli/compare/ovsm-v1.0.0...HEAD
[1.0.0]: https://github.com/opensvm/osvm-cli/releases/tag/ovsm-v1.0.0
[0.1.0]: https://github.com/opensvm/osvm-cli/releases/tag/ovsm-v0.1.0
