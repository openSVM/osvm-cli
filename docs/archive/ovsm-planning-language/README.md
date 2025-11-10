# OVSM Planning Language (Legacy)

**⚠️ ARCHIVED - Not Implemented**

This directory contains documentation for the original OVSM planning language design, which used Python-style syntax (`$variables`, `FOR`, `IF`, `RETURN`). This language was **never implemented**.

## What Actually Got Implemented

The current OVSM implementation is a **LISP dialect** with S-expression syntax. See the active documentation:

- **[README_LISP.md](../../ovsm/README_LISP.md)** - LISP implementation overview
- **[OVSM_LISP_SYNTAX_SPEC.md](../../ovsm/OVSM_LISP_SYNTAX_SPEC.md)** - Complete LISP syntax spec
- **[BUILTIN_FUNCTIONS.md](../../ovsm/BUILTIN_FUNCTIONS.md)** - 91+ built-in functions
- **[ovsm-language.html](../../pages/ovsm-language.html)** - Visual documentation

## Why This Was Archived

**Timeline**:
1. **Initial Design** (2024-Q3): Python-style planning language with 207 tools
2. **Pivot Decision** (2024-Q4): Switched to LISP for better AI agent execution
3. **LISP Implementation** (2024-Q4): 356/356 tests passing, 100% coverage, production-ready
4. **Archive** (2024-11): Moved old planning docs to prevent confusion

**Key Differences**:

| Feature | Planning Language (OLD) | LISP Implementation (NEW) |
|---------|-------------------------|---------------------------|
| Syntax | `$var = value` | `(define var value)` |
| Conditionals | `IF $x > 10 THEN ...` | `(if (> x 10) ...)` |
| Loops | `FOR $i IN 0..10:` | `(for (i (range 0 10)) ...)` |
| Functions | `DEFINE_TOOL name(args):` | `(defun name (args) ...)` |
| Status | **Never implemented** | **Production-ready ✅** |
| Test Coverage | N/A | 100% (356/356 tests) |
| Tools | 207 planned | 91+ implemented |

## Legacy Documentation Files

This archive contains:

- **OVSM_README.md** - Overview of planning language concept
- **ovsm-spec.md** - Core language specification (2,811 lines)
- **ovsm-agents.md** - Multi-agent extensions (1,908 lines)
- **COMPLETE_TOOL_INDEX.md** - Index of 207 planned tools
- **PLANNING_FORMAT.md** - Planning methodology guide
- **SYNTAX_IMPROVEMENTS_BRAINSTORM.md** - Design evolution document
- **INDEX.md** - Navigation index

## Why Keep These Docs?

**Historical value**:
- Shows design evolution and decision-making process
- Contains useful ideas for future features
- Documents the multi-agent planning concepts that inspired the LISP dialect

**⚠️ Do not use these docs for current development!**

Use the active LISP documentation instead:
- `/docs/ovsm/` - Current LISP implementation docs
- `/docs/pages/ovsm-language.html` - Web documentation
- `/crates/ovsm/` - Rust implementation with inline docs

---

**Archived**: 2024-11-10
**Status**: Reference only, not for implementation
**Replacement**: LISP dialect (see `/docs/ovsm/`)
