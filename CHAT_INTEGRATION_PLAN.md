# Chat Interface Integration Plan

## Current Status

✅ **Completed:**
- Chat UI fully implemented and functional
- Message history display with proper styling
- Input handling with vi-like keybindings (i/Enter/Esc)
- Auto-scrolling and manual scroll (j/k)
- Chat tab as default view (Tab 0)
- All UI components rendering correctly

❌ **Not Yet Connected:**
- Chat messages don't actually call AI/research agent
- No background processing of queries
- No streaming response updates
- Placeholder "Thinking..." message never gets replaced

## Architecture Overview

### Current Research Flow

```
User runs:  osvm research <wallet> --agent --tui

Flow:
1. handle_tui_research() initializes services (AI, OVSM, MCP)
2. Creates OsvmApp with Arc<Mutex<>> handles
3. Spawns background thread running ResearchAgent
4. Agent uses callbacks to update TUI state
5. TUI renders updates in real-time
```

### Proposed Chat Integration

```
User types in Chat tab and presses Enter

Flow:
1. send_chat_message() captures user query
2. Classify query type (simple vs complex)
3a. SIMPLE: Direct AI + OVSM → immediate response
3b. COMPLEX: Spawn ResearchAgent background task
4. Update chat_messages with streaming response
5. TUI renders new messages automatically
```

## Implementation Steps

### Step 1: Add Service Handles to OsvmApp

**File:** `src/utils/tui/app.rs`

**Changes needed:**

```rust
pub struct OsvmApp {
    // ... existing fields ...

    // Add service handles for chat
    pub ai_service: Option<Arc<Mutex<AiService>>>,
    pub ovsm_service: Option<Arc<Mutex<OvsmService>>>,
    pub mcp_service: Option<Arc<tokio::sync::Mutex<McpService>>>,
    pub runtime_handle: Option<tokio::runtime::Handle>,
}
```

**Why:** Chat needs access to services to process queries

### Step 2: Update Research Command Initialization

**File:** `src/commands/research.rs` - `handle_tui_research()`

**Changes needed:**

Around line 559 where `OsvmApp::new()` is called:

```rust
// Create TUI app
let mut app = OsvmApp::new(wallet.to_string());

// NEW: Inject service handles for chat
app.ai_service = Some(Arc::clone(&ai_service));
app.ovsm_service = Some(Arc::clone(&ovsm_service));
app.mcp_service = Some(Arc::clone(&mcp_arc));
app.runtime_handle = Some(runtime_handle.clone());
```

**Why:** Allows chat to access existing initialized services

### Step 3: Implement Chat Query Processing

**File:** `src/utils/tui/app.rs` - `send_chat_message()`

**Replace placeholder TODO with:**

```rust
fn send_chat_message(&mut self) {
    let user_message = self.chat_input.trim().to_string();
    if user_message.is_empty() {
        return;
    }

    // Add user message to chat history
    if let Ok(mut messages) = self.chat_messages.lock() {
        messages.push(ChatMessage {
            role: "user".to_string(),
            content: user_message.clone(),
            timestamp: chrono::Local::now().format("%H:%M:%S").to_string(),
            status: MessageStatus::Delivered,
        });

        // Add placeholder for assistant response
        messages.push(ChatMessage {
            role: "assistant".to_string(),
            content: "Thinking...".to_string(),
            timestamp: chrono::Local::now().format("%H:%M:%S").to_string(),
            status: MessageStatus::Streaming,
        });
    }

    // Clear input
    self.chat_input.clear();
    self.chat_auto_scroll = true;
    self.chat_scroll = 0;

    // Log the query
    self.add_log(format!("🤖 Processing query: {}", user_message));

    // Process query in background
    self.process_chat_query(user_message);
}

fn process_chat_query(&mut self, query: String) {
    // Check if services are available
    let ai_service = match &self.ai_service {
        Some(svc) => Arc::clone(svc),
        None => {
            self.update_last_chat_message("❌ Error: AI service not initialized. Please use research mode (--tui flag).", MessageStatus::Error);
            return;
        }
    };

    let runtime = match &self.runtime_handle {
        Some(rt) => rt.clone(),
        None => {
            self.update_last_chat_message("❌ Error: Runtime not initialized.", MessageStatus::Error);
            return;
        }
    };

    let chat_messages = Arc::clone(&self.chat_messages);
    let target_wallet = self.target_wallet.clone();

    // Spawn background task to process query
    runtime.spawn(async move {
        // Call AI service
        let response = match ai_service.lock().await.query_with_system_prompt(
            &format!("You are a Solana blockchain expert. Answer questions about wallet {} concisely and accurately.", target_wallet),
            &query
        ).await {
            Ok(resp) => resp,
            Err(e) => format!("❌ Error: {}", e),
        };

        // Update chat with response
        if let Ok(mut messages) = chat_messages.lock() {
            if let Some(last_msg) = messages.last_mut() {
                last_msg.content = response;
                last_msg.status = MessageStatus::Complete;
                last_msg.timestamp = chrono::Local::now().format("%H:%M:%S").to_string();
            }
        }
    });
}

fn update_last_chat_message(&mut self, content: &str, status: MessageStatus) {
    if let Ok(mut messages) = self.chat_messages.lock() {
        if let Some(last_msg) = messages.last_mut() {
            last_msg.content = content.to_string();
            last_msg.status = status;
            last_msg.timestamp = chrono::Local::now().format("%H:%M:%S").to_string();
        }
    }
}
```

**Why:** Provides working chat query processing with error handling

### Step 4: Enhanced Query Classification (Future)

**File:** `src/utils/tui/app.rs`

**Add method for smart query routing:**

```rust
async fn classify_and_route_query(&self, query: &str) -> QueryType {
    // Simple heuristics (can be enhanced with AI later)
    let query_lower = query.to_lowercase();

    // Complex research indicators
    if query_lower.contains("investigate") ||
       query_lower.contains("analyze") ||
       query_lower.contains("trace") ||
       query_lower.contains("find all") ||
       query_lower.contains("connections") {
        return QueryType::Complex;
    }

    // Simple query indicators
    if query_lower.starts_with("what is") ||
       query_lower.starts_with("show me") ||
       query_lower.starts_with("get") {
        return QueryType::Simple;
    }

    // Default to simple for now
    QueryType::Simple
}

enum QueryType {
    Simple,   // Direct AI response
    Complex,  // Full research agent
}
```

**Why:** Intelligently routes queries to appropriate handler

## Testing Plan

### Manual Testing Steps

1. **Build and Run:**
   ```bash
   cargo build
   ./target/debug/osvm research <WALLET_ADDRESS> --agent --tui
   ```

2. **Test Chat Tab:**
   - Press `0` to switch to Chat tab (should be default)
   - Press `i` to activate input
   - Type: "What is this wallet?"
   - Press Enter
   - Verify: Message appears and "Thinking..." is replaced with AI response

3. **Test Error Handling:**
   - Start TUI without services (if possible)
   - Verify graceful error messages

4. **Test Scrolling:**
   - Send multiple messages
   - Use `j/k` to scroll through history
   - Verify auto-scroll works for new messages

### Automated Testing (Future)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_chat_query_processing() {
        // Test that send_chat_message adds messages correctly
        // Test that responses update the last message
    }

    #[test]
    fn test_query_classification() {
        // Test simple vs complex query detection
    }
}
```

## Future Enhancements

### Priority 1: Streaming Responses

Instead of waiting for complete response:

```rust
// Use async streams to update message character-by-character
let mut stream = ai_service.query_stream(&query);
while let Some(chunk) = stream.next().await {
    // Update last message with new chunk
    update_chat_message_chunk(chunk);
}
```

### Priority 2: Query History Persistence

```rust
// Save chat history to file
fn save_chat_history(&self) -> Result<()> {
    let messages = self.chat_messages.lock().unwrap();
    let json = serde_json::to_string_pretty(&*messages)?;
    std::fs::write(
        format!("~/.osvm/chat_history_{}.json", self.target_wallet),
        json
    )?;
    Ok(())
}

// Load on startup
fn load_chat_history(&mut self) -> Result<()> {
    let path = format!("~/.osvm/chat_history_{}.json", self.target_wallet);
    if let Ok(json) = std::fs::read_to_string(path) {
        let messages: Vec<ChatMessage> = serde_json::from_str(&json)?;
        *self.chat_messages.lock().unwrap() = messages;
    }
    Ok(())
}
```

### Priority 3: Context-Aware Queries

Keep conversation context:

```rust
fn build_context_prompt(&self, new_query: &str) -> String {
    let messages = self.chat_messages.lock().unwrap();
    let recent_context: String = messages
        .iter()
        .rev()
        .take(5)  // Last 5 messages
        .map(|m| format!("{}: {}", m.role, m.content))
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        "Previous conversation:\n{}\n\nNew question: {}",
        recent_context,
        new_query
    )
}
```

### Priority 4: MCP Tool Integration

Allow chat to use MCP tools directly:

```rust
// Detect if query needs blockchain data
if needs_blockchain_data(&query) {
    // Execute relevant MCP tool
    let result = execute_mcp_tool(&query).await?;
    // Format result for user
    format_mcp_response(result)
}
```

## Migration Guide

### For Developers

**Before integrating:**
1. Read `src/commands/research.rs` to understand service initialization
2. Review `src/services/research_agent.rs` for agent patterns
3. Check `src/utils/tui/app.rs` for TUI patterns

**After integrating:**
1. Test both simple and complex queries
2. Verify error messages are helpful
3. Check memory usage (Arc clones should be lightweight)
4. Profile async task spawning performance

### For Users

**No changes required!**
- Chat interface works exactly as before
- Now it actually processes queries instead of showing placeholder
- All existing keybindings remain the same

## Known Limitations

1. **Services Only Available in TUI Mode**
   - Chat won't work if `OsvmApp` is created standalone
   - Solution: Always initialize with services, or show clear error

2. **No Streaming Yet**
   - Responses appear all at once
   - Solution: Implement streaming in Priority 1

3. **No Persistence**
   - Chat history lost on exit
   - Solution: Implement in Priority 2

4. **Simple AI Queries Only**
   - Can't trigger full research agent investigations yet
   - Solution: Implement query classification in Step 4

## Success Criteria

✅ Chat interface successfully integrated when:
1. User can type questions and get AI responses
2. Responses are contextually relevant to the target wallet
3. Error messages are clear and actionable
4. Performance is acceptable (<2s for simple queries)
5. No crashes or panics during normal usage

## Timeline Estimate

- **Step 1-2:** 30 minutes (add service handles)
- **Step 3:** 2 hours (implement query processing + error handling)
- **Step 4:** 1 hour (query classification)
- **Testing:** 1 hour
- **Total:** ~4-5 hours for basic integration

**Future enhancements:** 5-10 hours additional

## Resources

- **Research agent code:** `src/services/research_agent.rs`
- **TUI research handler:** `src/commands/research.rs:506-700`
- **AI service:** `src/services/ai_service.rs`
- **MCP service:** `src/services/mcp_service.rs`
- **Chat UI:** `src/utils/tui/app.rs:1461-1613`

## Notes

- Keep chat responses concise (max 500 words)
- Always include wallet address context
- Prioritize speed over completeness for simple queries
- Log all queries for debugging and improvement
