# Chat Interface - Quick Implementation Guide

## 🚀 Quick Start: Connect Chat to AI (15 minutes)

### 1. Add Service Fields to OsvmApp

**File:** `src/utils/tui/app.rs` (around line 135)

```rust
pub struct OsvmApp {
    // ... existing fields ...

    // NEW: Add these 4 lines
    pub ai_service: Option<Arc<Mutex<AiService>>>,
    pub ovsm_service: Option<Arc<Mutex<OvsmService>>>,
    pub mcp_service: Option<Arc<tokio::sync::Mutex<McpService>>>,
    pub runtime_handle: Option<tokio::runtime::Handle>,
}
```

### 2. Initialize Fields in new()

**File:** `src/utils/tui/app.rs` (around line 288)

```rust
impl OsvmApp {
    pub fn new(target_wallet: String) -> Self {
        Self {
            // ... existing fields ...

            // NEW: Add these 4 lines at the end
            ai_service: None,
            ovsm_service: None,
            mcp_service: None,
            runtime_handle: None,
        }
    }
}
```

### 3. Inject Services in Research Command

**File:** `src/commands/research.rs` (around line 566)

```rust
// Create TUI app
let mut app = OsvmApp::new(wallet.to_string());

// NEW: Add these 4 lines
app.ai_service = Some(Arc::clone(&ai_service));
app.ovsm_service = Some(Arc::clone(&ovsm_service));
app.mcp_service = Some(Arc::clone(&mcp_arc));
app.runtime_handle = Some(runtime_handle.clone());
```

### 4. Replace TODO in send_chat_message()

**File:** `src/utils/tui/app.rs` (around line 887)

Replace the entire TODO section with:

```rust
// Process query in background
self.process_chat_query(user_message);
```

### 5. Add Query Processing Methods

**File:** `src/utils/tui/app.rs` (add after send_chat_message)

```rust
/// Process a chat query using AI service
fn process_chat_query(&mut self, query: String) {
    // Check if services are available
    let ai_service = match &self.ai_service {
        Some(svc) => Arc::clone(svc),
        None => {
            self.update_last_chat_message(
                "❌ Chat is only available in research mode.\n\nTo use chat, run:\n  osvm research <wallet> --agent --tui",
                MessageStatus::Error
            );
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

    // Spawn background task
    runtime.spawn(async move {
        let system_prompt = format!(
            "You are a Solana blockchain expert analyzing wallet: {}\n\
            Provide concise, accurate answers. Include specific numbers and addresses when relevant.\n\
            If you need blockchain data to answer, say so clearly.",
            target_wallet
        );

        let response = match ai_service.lock().await.query_with_system_prompt(
            &system_prompt,
            &query
        ).await {
            Ok(resp) => resp,
            Err(e) => format!("❌ Error processing query: {}\n\nPlease try again or rephrase your question.", e),
        };

        // Update last message with response
        if let Ok(mut messages) = chat_messages.lock() {
            if let Some(last_msg) = messages.last_mut() {
                last_msg.content = response;
                last_msg.status = MessageStatus::Complete;
                last_msg.timestamp = chrono::Local::now().format("%H:%M:%S").to_string();
            }
        }
    });
}

/// Update the last chat message (helper for error handling)
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

### 6. Add Required Imports

**File:** `src/utils/tui/app.rs` (top of file)

Check if these are already imported, if not add:

```rust
use crate::services::ai_service::AiService;
use crate::services::ovsm_service::OvsmService;
use crate::services::mcp_service::McpService;
```

## ✅ Test It!

```bash
# Build
cargo build

# Run research mode with TUI
./target/debug/osvm research <WALLET_ADDRESS> --agent --tui

# In the TUI:
# 1. Chat tab should be active (Tab 0)
# 2. Press 'i' to activate input
# 3. Type: "What can you tell me about this wallet?"
# 4. Press Enter
# 5. Wait for response (should replace "Thinking...")
```

## 🎯 Expected Behavior

### Success Case:
```
[12:34:56] USER ✓
What can you tell me about this wallet?

[12:34:58] ASSISTANT
This wallet ABC...XYZ has been active on Solana mainnet.
Based on available data, I can analyze its activity patterns
when you provide specific questions about transfers, tokens,
or interactions.

What would you like to know specifically?
```

### Error Case (if not in research mode):
```
[12:34:56] USER ✓
What can you tell me about this wallet?

[12:34:56] ASSISTANT ✗
❌ Chat is only available in research mode.

To use chat, run:
  osvm research <wallet> --agent --tui
```

## 🐛 Troubleshooting

**Problem:** "Error: Runtime not initialized"
- **Fix:** Make sure you're running `osvm research --agent --tui`, not just creating OsvmApp standalone

**Problem:** Chat sends but no response
- **Check:** Console logs for errors (logs go to `/tmp/osvm_research.log` in TUI mode)
- **Fix:** Verify AI service is configured (check `OPENAI_KEY` env var)

**Problem:** Compilation errors about Arc/Mutex
- **Fix:** Add missing imports at top of `app.rs`

**Problem:** Response takes too long
- **Expected:** Simple queries should respond in 2-5 seconds
- **If longer:** Check network connection and AI service health

## 🚀 Next Steps

Once basic integration works:

1. **Add Streaming** - Show responses as they're generated (not all at once)
2. **Add History** - Save/load chat sessions
3. **Add Context** - Remember previous messages in conversation
4. **Add MCP Tools** - Let chat execute blockchain queries directly

See `CHAT_INTEGRATION_PLAN.md` for detailed roadmap.

## 📚 Related Files

- Full integration plan: `CHAT_INTEGRATION_PLAN.md`
- TUI research handler: `src/commands/research.rs:506`
- Research agent: `src/services/research_agent.rs`
- AI service: `src/services/ai_service.rs`
