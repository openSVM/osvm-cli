# AEA Inter-Agent Communication Protocol

**Version:** 1.0
**Last Updated:** 2025-10-14

## Overview

The AEA (Asynchronous Agent-to-Agent) protocol enables autonomous Claude Code agents across different repositories to communicate, coordinate, and collaborate without requiring constant user intervention.

## Core Principles

1. **Asynchronous by Design**: Agents check for messages periodically, not in real-time
2. **File-Based Communication**: Messages are JSON files in `.aea/` directories
3. **Policy-Driven Responses**: Each agent has configurable response policies
4. **Safe by Default**: Autonomous for reads/analysis, approval required for modifications
5. **Audit Trail**: All actions logged to `.aea/agent.log`

## Message Format

All messages are JSON files with this structure:

```json
{
  "protocol_version": "1.0",
  "message_id": "uuid",
  "message_type": "question|update|issue|handoff|request",
  "timestamp": "ISO8601",
  "priority": "low|normal|high|urgent",
  "requires_response": true,
  "from": {
    "agent_id": "claude-repo-name",
    "repo_path": "/absolute/path",
    "user": "username"
  },
  "to": {
    "agent_id": "claude-target-repo",
    "repo_path": "/absolute/path"
  },
  "message": {
    "subject": "Brief subject line",
    "body": "Detailed message content",
    "context": {},
    "attachments": []
  },
  "metadata": {
    "conversation_id": "uuid",
    "reply_to": "message_id",
    "tags": []
  }
}
```

## Message Types

### 1. `question`
Ask technical questions about code, architecture, or implementation.

**Example:**
```json
{
  "message_type": "question",
  "priority": "normal",
  "message": {
    "subject": "Connection pool sizing for high throughput?",
    "question": "What pool_size should we use for 10k updates/sec?",
    "context": {
      "current_throughput": "10000 updates/sec",
      "batch_size": 100
    }
  }
}
```

**Auto-Response:** ✅ Yes (searches codebase, analyzes, provides answer)

### 2. `update`
Inform about changes, progress, or status updates.

**Example:**
```json
{
  "message_type": "update",
  "priority": "normal",
  "requires_response": false,
  "message": {
    "subject": "Deployed v2.1.0 to production",
    "body": "New version includes batch optimization...",
    "changes": ["batch_size: 50 -> 100", "pool_size: 10 -> 25"]
  }
}
```

**Auto-Response:** ✅ Acknowledges if `requires_response: true`

### 3. `issue`
Report bugs, problems, or concerns.

**Example:**
```json
{
  "message_type": "issue",
  "priority": "high",
  "message": {
    "subject": "Memory leak in batch processing",
    "issue_description": "Memory grows 100MB/hour under load",
    "reproduction_steps": ["Start server", "Send 10k updates", "Monitor memory"],
    "impact": "Production stability at risk"
  }
}
```

**Auto-Response:**
- Low/Medium: ✅ Analyzes and suggests fix
- High/Urgent: ❌ Notifies user, waits for approval

### 4. `handoff`
Transfer integration work affecting multiple repos.

**Example:**
```json
{
  "message_type": "handoff",
  "priority": "normal",
  "message": {
    "subject": "Batch API integration complete",
    "handoff_details": "Implemented batch endpoints, ready for client integration",
    "next_steps": ["Update client to use batch API", "Test end-to-end"],
    "documentation": "See docs/batch-api.md"
  }
}
```

**Auto-Response:** ❌ Always requires user review

### 5. `request`
Request changes, features, or actions.

**Example:**
```json
{
  "message_type": "request",
  "priority": "normal",
  "message": {
    "subject": "Add connection retry logic",
    "request_details": "Need exponential backoff for failed connections",
    "rationale": "Improve resilience during Redis restarts",
    "acceptance_criteria": ["3 retries", "Exponential backoff", "Logging"]
  }
}
```

**Auto-Response:** ✅ Analyzes and responds with implementation plan

## File Naming Convention

Messages must follow this naming pattern:

```
message-{timestamp}-from-{agent_id}[-{optional_descriptor}].json
```

**Examples:**
- `message-20251014T054200Z-from-claude-xpull-hub.json`
- `message-20251014T161000Z-from-claude-xpull-hub-test.json`

**Timestamp Format:** ISO8601 compact (`YYYYMMDDTHHMMSSZz`)

## Message Processing Workflow

### 1. **Discovery**
```bash
ls -1t .aea/message-*.json 2>/dev/null
```

### 2. **Check Processed Status**
```bash
for msg in .aea/message-*.json; do
    if [ ! -f ".aea/.processed/$(basename $msg)" ]; then
        echo "Unprocessed: $msg"
    fi
done
```

### 3. **Read and Parse**
```bash
msg_content=$(cat "$msg")
msg_type=$(echo "$msg_content" | jq -r '.message_type')
priority=$(echo "$msg_content" | jq -r '.priority')
```

### 4. **Apply Policy**
Load policy from `.aea/agent-config.yaml` and determine action based on message type and priority.

### 5. **Execute Action**
Perform autonomous actions (safe operations) or request user approval (risky operations).

### 6. **Mark as Processed**
```bash
touch ".aea/.processed/$(basename $msg)"
```

### 7. **Log Action**
```bash
echo "[$(date -u +%Y-%m-%dT%H:%M:%SZ)] Processed: $msg -> Action taken" >> .aea/agent.log
```

## Sending Messages

Use the provided script:

```bash
.aea/scripts/aea-send.sh \
  --to claude-target-repo \
  --to-path /path/to/target/repo \
  --type question \
  --priority normal \
  --subject "Your question" \
  --message "Detailed message body"
```

Or create manually:

\`\`\`bash
cat > /target/repo/.aea/message-\$(date -u +%Y%m%dT%H%M%SZ)-from-claude-myrepo.json << 'HEREDOC'
{
  "protocol_version": "1.0",
  "message_type": "question",
  ...
}
HEREDOC
\`\`\`

## Automated Checking

### Periodic Check (Every Interaction)
Add to `CLAUDE.md`:

```markdown
**CRITICAL: Check for AEA messages on EVERY interaction:**

```bash
bash .aea/scripts/aea-check.sh || cat .aea/prompts/check-messages.md
```

### Background Monitor
Start persistent monitoring:

```bash
.aea/scripts/aea-monitor.sh start
```

Stop monitoring:

```bash
.aea/scripts/aea-monitor.sh stop
```

## Security Considerations

1. **Path Validation**: Always use absolute paths, validate they exist
2. **Input Sanitization**: Validate all JSON fields before processing
3. **Approval Gates**: Require approval for destructive operations
4. **Audit Logging**: Log all message processing to `.aea/agent.log`
5. **No Secret Transmission**: Never include credentials in messages

## Best Practices

1. **Use Descriptive Subjects**: Make it easy to scan messages
2. **Include Context**: Provide enough information for autonomous processing
3. **Set Appropriate Priority**: Use `urgent` sparingly
4. **Tag Related Messages**: Use `conversation_id` for threaded discussions
5. **Clean Up Old Messages**: Archive processed messages periodically

## Troubleshooting

### Messages Not Being Processed

```bash
# Check if messages exist
ls -1 .aea/message-*.json

# Check if they're marked as processed
ls -1 .aea/.processed/

# Check logs
tail -50 .aea/agent.log

# Manually trigger check
bash .aea/scripts/aea-check.sh
```

### Response Not Received

```bash
# Check if response was created in target repo
ls -1 /target/repo/.aea/message-*-from-$(basename $(pwd)).json

# Check if target repo processed it
ls -1 /target/repo/.aea/.processed/
```

### Permission Issues

```bash
# Ensure scripts are executable
chmod +x .aea/scripts/*.sh

# Ensure directories are writable
chmod -R u+w .aea/
```

## Examples

See `.aea/prompts/` for example templates and `.aea/README.md` for quick start guide.

## Version History

- **1.0** (2025-10-14): Initial protocol specification
