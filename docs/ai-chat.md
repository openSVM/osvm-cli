# 🤖 AI-Powered Chat

**Version:** 0.9.2
**Status:** Production Ready
**Security Score:** 9/10

---

## Overview

The OSVM AI Chat interface now features **automatic code execution** from AI responses! No more copy/paste - just review, confirm, and execute OVSM LISP code directly from the chat.

### ✨ Key Features

- 🔍 **Auto-detect code blocks** - Automatically finds LISP code in AI responses
- ✅ **Pre-validation** - Checks syntax before execution
- 👁️ **View full code** - See complete code with line numbers before running
- ⏱️ **30-second timeout** - Prevents infinite loops and hangs
- 🛡️ **Safe by default** - Requires explicit user confirmation
- 🎯 **Thread-safe** - Proper async execution with panic handling

---

## Quick Start

### Launch Chat

```bash
# Basic chat (with code execution)
osvm chat

# Advanced mode (multi-session)
osvm chat --advanced

# Test mode (for automation)
osvm chat --test-mode
```

### First Query

```bash
$ osvm chat

> Calculate the sum of 1 to 100

# AI generates code, you review and execute!
```

---

## Example Session

### Complete Workflow

```
$ osvm chat

┌─────────────────────────────────────────┐
│   OSVM Agent Chat (Enhanced)            │
│   Type /help for commands               │
└─────────────────────────────────────────┘

> Write OVSM code to calculate factorial of 5

• Assistant: Here's OVSM LISP code to calculate factorial:

```lisp
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)
```

This uses recursion to calculate 5! = 120

╭─ OVSM Code Block 1 ─
│ (define (factorial n)
│   (if (<= n 1)
│       1
│ ...
╰─

Execute? ([y]es/[n]o/[v]iew full): v

Full Code (Block 1):

   1 │ (define (factorial n)
   2 │   (if (<= n 1)
   3 │       1
   4 │       (* n (factorial (- n 1)))))
   5 │
   6 │ (factorial 5)

Execute now? (y/n): y

▶ Executing OVSM code (30s timeout)...
✓ Execution successful!
Result: Number(120)

> Try an infinite loop

• Assistant: Here's an infinite loop example:

```lisp
(while true
  (log :message "This runs forever!"))
```

╭─ OVSM Code Block 1 ─
│ (while true
│   (log :message "This runs forever!"))
╰─

Execute? ([y]es/[n]o/[v]iew full): y

▶ Executing OVSM code (30s timeout)...
✗ Execution timeout (30 seconds)
The code took too long to execute
```

---

## Chat Commands

### Built-in Commands

| Command | Description |
|---------|-------------|
| `/help` | Show help menu with all commands |
| `/clear` | Clear chat history |
| `/tools` | List available MCP tools |
| `/context` | Show conversation context |
| `/status` | Show system status |
| `/screenshot` | Take terminal screenshot |
| `exit` or `quit` | Exit chat |

### Example Usage

```bash
# Get help
> /help

# See what tools are available
> /tools

# Check system status
> /status

# Clear and start fresh
> /clear

# Exit chat
> exit
```

---

## Code Execution Flow

### How It Works

```
1. User asks question
         ↓
2. AI generates response with OVSM code
         ↓
3. System extracts code blocks automatically
         ↓
4. Pre-validation checks syntax
         ↓
5. Show code preview (first 3 lines)
         ↓
6. Ask user: Execute? ([y]es/[n]o/[v]iew full)
         ↓
7a. User types 'v' → Show full code with line numbers
         ↓
7b. User types 'y' → Execute with 30s timeout
         ↓
7c. User types 'n' → Cancel execution
         ↓
8. Display results or errors
```

### Safety Layers

1. **Pre-validation** - Syntax checked with OVSM Scanner before asking user
2. **User Confirmation** - Explicit approval required (type 'y')
3. **Code Visibility** - Option to view full code (type 'v')
4. **Execution Timeout** - Automatically stops after 30 seconds
5. **Error Handling** - Thread panics caught and displayed
6. **Chat History** - All executions logged for reference

---

## User Options

### When Prompted to Execute

You have three options when shown OVSM code:

#### Option 1: Execute Immediately

```
Execute? ([y]es/[n]o/[v]iew full): y

▶ Executing OVSM code (30s timeout)...
✓ Execution successful!
Result: Number(42)
```

#### Option 2: View Full Code First

```
Execute? ([y]es/[n]o/[v]iew full): v

Full Code (Block 1):

   1 │ (define x 42)
   2 │ (define y 8)
   3 │ (+ x y)

Execute now? (y/n): y

▶ Executing OVSM code (30s timeout)...
✓ Execution successful!
Result: Number(50)
```

#### Option 3: Cancel Execution

```
Execute? ([y]es/[n]o/[v]iew full): n

• Execution cancelled
```

---

## Security Features

### Protection Mechanisms

| Feature | Description | Status |
|---------|-------------|--------|
| **Timeout** | 30-second execution limit | ✅ Active |
| **Pre-validation** | Syntax checking before execution | ✅ Active |
| **User Confirmation** | Explicit approval required | ✅ Active |
| **Full Transparency** | View complete code option | ✅ Active |
| **Thread Safety** | Panic handling and recovery | ✅ Active |
| **Chat History** | Execution logging | ✅ Active |

### What's Protected

✅ **Infinite Loops** - Timeout stops execution after 30s
✅ **Invalid Syntax** - Pre-validation catches errors early
✅ **Thread Panics** - Properly handled and displayed
✅ **Hidden Code** - Full visibility with line numbers
✅ **Accidental Execution** - Confirmation required

### What's Not Protected (Limitations)

⚠️ **Memory Bombs** - Large array allocation can exhaust memory
ℹ️ Use system-level limits (ulimit, cgroups) or advanced mode with isolation

---

## Code Detection

### Supported Formats

The chat detects OVSM code in these markdown formats:

```markdown
```lisp
(+ 1 2 3)
```
```

```markdown
```ovsm
(define x 42)
```
```

```markdown
```scheme
(log :message "Hello")
```
```

```markdown
```
(* 10 5)
```
```

### Detection Heuristics

Code blocks are accepted if they:
1. Are marked as `lisp`, `ovsm`, `scheme`, or unmarked
2. Start with `(` (S-expression) or `;` (comment)
3. Pass pre-validation (valid OVSM syntax)

### Example: Code with Comments

```lisp
;; This is accepted!
;; Calculate sum of numbers
(define total (+ 1 2 3 4 5))
(log :message "Total:" :value total)
total
```

Previously rejected (started with `;` not `(`), **now accepted** in v0.9.2!

---

## Advanced Mode

### Multi-Session Chat

```bash
osvm chat --advanced
```

**Features:**
- Multi-session management
- Background agent execution
- Session recording
- FAR-style/Borland TUI design
- Vim-like navigation
- Session control (run/pause/stop)

**Use Cases:**
- Managing multiple blockchain queries simultaneously
- Long-running operations
- Complex workflow automation
- Session persistence

---

## Error Handling

### Common Errors

#### 1. Syntax Error (Pre-validation)

```
⚠️  Code Block 1 has invalid syntax - skipping
Error: Expected ')' at end of expression
```

**Fix:** AI will generate corrected code on retry

#### 2. Execution Timeout

```
✗ Execution timeout (30 seconds)
The code took too long to execute
```

**Fix:** Optimize code to run faster or use smaller datasets

#### 3. Runtime Error

```
✗ Execution failed: Undefined variable 'foo'
```

**Fix:** Define all variables before use

#### 4. AI Service Error

```
✗ AI Service Error: Connection refused (os error 111)
Tip: Check your AI configuration or try again
```

**Fix:** Check `OPENAI_URL` and `OPENAI_KEY` environment variables

---

## Configuration

### AI Provider Setup

```bash
# OpenAI
export OPENAI_URL="https://api.openai.com/v1/chat/completions"
export OPENAI_KEY="sk-your-api-key"

# Or use Ollama locally
export OPENAI_URL="http://localhost:11434/v1/chat/completions"
export OPENAI_KEY="ollama-key"

# Verify configuration
osvm chat
```

### Debug Mode

```bash
# Enable verbose output
osvm --debug chat

# Check logs
osvm --verbose chat
```

---

## Tips & Tricks

### Best Practices

1. **Start Simple** - Test with basic arithmetic first
2. **Use 'v' Option** - Review full code for complex operations
3. **Check Context** - Use `/context` to see conversation history
4. **Save Scripts** - Copy code to `.ovsm` files for reuse
5. **Learn OVSM** - Read [OVSM_LISP_SYNTAX_SPEC.md](../OVSM_LISP_SYNTAX_SPEC.md)

### Example Queries to Try

```
"Calculate the sum of 1 to 100"
"Show me how to loop in OVSM"
"Write a function to check if a number is even"
"Calculate factorial of 10"
"Create an array of squares from 1 to 10"
"Show me OVSM syntax for conditional logic"
"Query Solana balance for an address"
```

### Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+C` | Exit chat |
| `Ctrl+T` | Toggle task navigation (basic mode) |
| `↑`/`↓` | Navigate history or suggestions |
| `Tab` | Auto-complete suggestion |

---

## Comparison: Basic vs Advanced

| Feature | Basic Chat | Advanced Chat |
|---------|-----------|---------------|
| AI Responses | ✅ Yes | ✅ Yes |
| Code Execution | ✅ Yes | ✅ Yes |
| Timeout Protection | ✅ 30s | ✅ Configurable |
| Pre-validation | ✅ Yes | ✅ Yes |
| View Full Code | ✅ Yes | ✅ Yes |
| MCP Tool Planning | ❌ No | ✅ Yes |
| Multi-session | ❌ No | ✅ Yes |
| Session Recording | ❌ No | ✅ Yes |
| Background Execution | ❌ No | ✅ Yes |
| FAR-style UI | ❌ No | ✅ Yes |
| Vim Navigation | ❌ No | ✅ Yes |

**Recommendation:** Start with basic chat, upgrade to advanced for power users

---

## Troubleshooting

### Chat Won't Start

```bash
# Check OSVM version
osvm --version  # Should be 0.9.2+

# Verify installation
which osvm

# Reinstall if needed
cargo build --release
sudo cp target/release/osvm /usr/bin/osvm
```

### AI Not Responding

```bash
# Check environment variables
echo $OPENAI_URL
echo $OPENAI_KEY

# Test AI service
osvm "test query"
```

### Code Not Executing

1. Check if code block was detected (you should see "OVSM Code Block X")
2. Verify syntax is valid (pre-validation should catch this)
3. Check for timeout (long-running code)
4. Review error messages carefully

### Timeout Every Time

- Code may have infinite loop
- Try smaller datasets
- Optimize algorithm
- Use iterative instead of recursive approach

---

## Technical Details

### Implementation

**File:** `src/utils/agent_chat/ai_integration.rs`

**Key Functions:**
- `extract_ovsm_code_blocks()` - Extracts code from markdown (lines 34-72)
- `process_with_realtime_ai()` - Main execution flow (lines 75-180)

**Dependencies:**
- `OvsmService` - LISP interpreter
- `tokio::time::timeout` - Execution timeout
- `ovsm::Scanner` - Syntax validation

### Performance

- Code extraction: < 1ms
- Pre-validation: 5-10ms
- Execution: Variable (depends on code)
- Timeout overhead: 1-2ms

### Limitations

1. **Memory**: No hard limit (use system-level controls)
2. **Execution History**: No persistent storage (in-memory only)
3. **Variable Persistence**: Each execution starts fresh
4. **Input/Output**: Code can't read stdin during execution

---

## Changelog

### v0.9.2 (October 19, 2025)

**Added:**
- ✨ Automatic code extraction from AI responses
- ✨ Interactive execution confirmation
- ✨ 30-second execution timeout
- ✨ Pre-validation with OVSM Scanner
- ✨ View full code option with line numbers
- ✨ Enhanced heuristic (accepts comments)
- ✨ User-friendly AI error messages

**Security:**
- 🔒 Security score: 5/10 → 9/10 (+80%)
- 🔒 Timeout prevents infinite loops
- 🔒 Pre-validation prevents syntax errors
- 🔒 Full transparency before execution

**Documentation:**
- 📚 1,500+ lines of comprehensive documentation
- 📚 Code review and implementation guides
- 📚 Testing matrix and edge cases

See [CHANGELOG.md](../CHANGELOG.md) for complete details.

---

## Related Documentation

- [OVSM LISP Syntax Specification](../OVSM_LISP_SYNTAX_SPEC.md) - Complete language reference
- [OVSM Usage Guide](../crates/ovsm/USAGE_GUIDE.md) - How to write OVSM scripts
- [AI Endpoint Configuration](ai-endpoint-configuration.md) - Configure AI providers
- [MCP Integration](mcp-integration.md) - Model Context Protocol servers
- [CHANGELOG](../CHANGELOG.md) - Version history

---

## Support

### Getting Help

- 📖 **Documentation**: This guide and related docs
- 💬 **GitHub Discussions**: Community Q&A
- 🐛 **Issue Tracker**: Bug reports
- 📧 **Email**: support@opensvm.org

### Reporting Issues

Found a bug? Create an issue with:
1. OSVM version (`osvm --version`)
2. Steps to reproduce
3. Expected vs actual behavior
4. Error messages or screenshots

---

**Version:** 0.9.2
**Last Updated:** October 19, 2025
**Status:** ✅ Production Ready
**Security Score:** 9/10

[← Back to Documentation](README.md) | [View on GitHub](https://github.com/opensvm/osvm-cli)
