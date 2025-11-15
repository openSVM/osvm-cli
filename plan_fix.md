# Plan to Fix OSVM Research Agent Output

## User's Requirements (Re-read Analysis):

1. **"where is beatiful chart like this?"**
   - User wants the HORIZONTAL PIPELINE renderer (from `/tmp/pipeline_with_metadata.rs`)
   - NOT the simple depth-based sections
   - Want the ASCII art with arrows flowing LEFT→RIGHT like the prototype showed

2. **"why you expand only surface level?"**
   - Currently showing depth-based sections separately
   - User wants TRUE horizontal expansion with wallets side-by-side
   - Like columns in a table, not separate sections

3. **"stop printing out ovsm out"**
   - Don't show raw OVSM LISP code execution in output
   - Save it to file instead

4. **"save it to file, not to the output"**
   - All OVSM execution details → file
   - Only show final results to user

5. **"in ai call specify maxTokens: 128"**
   - Make AI responses more concise
   - Add maxTokens parameter to AI service calls

6. **"it gives us markdown - you must render markdown"**
   - AI responses come as markdown
   - Need to render markdown to terminal (colors, formatting)

7. **"all osvm output must be written to the log file"**
   - Path: `~/.osvm/logs/log-{unixtimestamp}.log`
   - ALL events, output, execution details
   - Keep logging until program exits

## Tasks to Implement:

### Task 1: Fix Horizontal Pipeline Renderer ✅ HIGH PRIORITY
**Problem**: Current implementation shows depth sections VERTICALLY, not true horizontal pipeline
**Solution**:
- Modify `render_horizontal_pipeline()` to render wallets SIDE-BY-SIDE at same depth
- Create true LEFT→RIGHT flow like the prototype
- Use columnar layout with proper ASCII art arrows

**Files to modify**:
- `src/services/research_agent.rs` - Fix renderer to be truly horizontal

### Task 2: Add Logging System ✅ HIGH PRIORITY
**Problem**: No centralized logging to file
**Solution**:
- Create `~/.osvm/logs/` directory
- Generate log filename with Unix timestamp
- Redirect all output to log file
- Use `tracing` or `log` crate with file appender

**Files to modify**:
- `src/main.rs` - Initialize logging on startup
- `src/services/research_agent.rs` - Log all OVSM executions

### Task 3: Hide OVSM Execution Output ✅ MEDIUM PRIORITY
**Problem**: OVSM LISP code shown in terminal
**Solution**:
- Don't print OVSM execution to stdout
- Only log to file
- Show only final results to user

**Files to modify**:
- `src/services/ovsm_service.rs` or wherever OVSM is executed
- Capture stdout/stderr and redirect to log

### Task 4: Add maxTokens to AI Calls ✅ MEDIUM PRIORITY
**Problem**: AI responses too verbose
**Solution**:
- Add `max_tokens: 128` parameter to AI API calls
- Make responses more concise

**Files to modify**:
- `src/services/ai_service.rs` - Add maxTokens parameter
- `src/utils/ai_client.rs` - Pass parameter to API

### Task 5: Render Markdown in Terminal ✅ MEDIUM PRIORITY
**Problem**: AI returns markdown, we show raw text
**Solution**:
- Use `termimad` or `bat` crate to render markdown
- Convert markdown to ANSI-colored terminal output

**Files to modify**:
- `src/services/research_agent.rs` - Render AI responses as markdown
- Add markdown rendering utility

## Implementation Order:

1. **FIRST**: Fix horizontal pipeline renderer (user's main complaint)
2. **SECOND**: Add logging system (infrastructure for other changes)
3. **THIRD**: Hide OVSM output + redirect to logs
4. **FOURTH**: Add maxTokens parameter
5. **FIFTH**: Render markdown

## Expected Result:

When user runs `osvm research --agent ADDRESS`:
- ✅ Beautiful horizontal ASCII chart with LEFT→RIGHT flow
- ✅ No OVSM execution output visible
- ✅ All details logged to `~/.osvm/logs/log-{timestamp}.log`
- ✅ Concise AI responses (max 128 tokens)
- ✅ Markdown rendered with colors/formatting
- ✅ Clean, professional terminal output

## Self-Check Questions:

- ❓ Do I understand what "horizontal" means? → YES: Side-by-side wallets, not vertical sections
- ❓ Do I know what output to hide? → YES: OVSM LISP execution details
- ❓ Do I know where to log? → YES: `~/.osvm/logs/log-{unixtimestamp}.log`
- ❓ Do I know what maxTokens does? → YES: Limits AI response length
- ❓ Do I know what markdown rendering means? → YES: Convert `**bold**` to ANSI colors

## Files to Read/Modify:

1. `/home/larp/larpdevs/osvm-cli/src/services/research_agent.rs` - Fix renderer
2. `/home/larp/larpdevs/osvm-cli/src/main.rs` - Add logging init
3. `/home/larp/larpdevs/osvm-cli/src/services/ai_service.rs` - Add maxTokens
4. `/home/larp/larpdevs/osvm-cli/Cargo.toml` - Add logging/markdown deps
5. `/tmp/pipeline_with_metadata.rs` - Reference for beautiful chart

## Notes:

- User emphasized "read this message again" - did that ✅
- User wants self-asking - doing that now ✅
- User wants plan in plan_fix.md - writing this ✅
- User's tone suggests frustration with current output - prioritize visual fixes
