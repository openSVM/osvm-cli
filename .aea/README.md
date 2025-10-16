# AEA Inter-Agent Communication

**Asynchronous Agent-to-Agent communication system for Claude Code**

## Quick Start

### 1. Check for Messages

```bash
# Manual check
bash .aea/scripts/aea-check.sh

# Or use slash command in Claude
/aea
```

### 2. Send a Message

```bash
.aea/scripts/aea-send.sh \
  --to claude-other-repo \
  --to-path /path/to/other/repo \
  --type question \
  --priority normal \
  --subject "Your question here" \
  --message "Detailed message body"
```

### 3. Start Background Monitoring

```bash
.aea/scripts/aea-monitor.sh start
```

## Message Types

| Type | Description | Auto-Response |
|------|-------------|---------------|
| `question` | Technical questions | ✅ Yes |
| `update` | Status updates | ✅ Acknowledges if requested |
| `issue` | Bug reports | ⚠️ Depends on priority |
| `handoff` | Integration handoffs | ❌ Requires approval |
| `request` | Feature requests | ✅ Responds with plan |

## Files

- **agent-config.yaml**: Response policies and configuration
- **aea-rules.md**: Complete protocol documentation
- **prompts/check-messages.md**: Auto-check prompt template
- **scripts/aea-check.sh**: Message checking script
- **scripts/aea-send.sh**: Message sending script
- **scripts/aea-monitor.sh**: Background monitoring daemon
- **agent.log**: Processing audit trail
- **.processed/**: Tracks processed messages

## Configuration

Edit `.aea/agent-config.yaml` to customize:

- Agent ID and capabilities
- Response policies per message type
- Auto-safe vs. approval-required operations
- Monitoring intervals

## Documentation

See `aea-rules.md` for complete protocol specification and examples.

## Integration

This repository is integrated with:

- `xpull_hub`: Redis connection pooling and batch operations

Check `.aea/agent.log` for recent activity.
