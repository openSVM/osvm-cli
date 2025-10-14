# Visual TUI Testing Agent - Complete Documentation

## Overview

The Visual TUI Testing Agent (`tui_test_agent.rs`) provides **automated visual testing** for the OSVM advanced chat TUI. It runs the TUI in a virtual terminal (PTY), captures screen output, sends keyboard events programmatically, and validates the UI state.

This is similar to Selenium for web browsers, but for terminal user interfaces!

## Architecture

```
┌─────────────────────────────────────────────────────┐
│          Visual TUI Testing Agent                    │
├─────────────────────────────────────────────────────┤
│                                                       │
│  ┌──────────────┐        ┌──────────────┐          │
│  │ Test Scenario│───────>│  PTY Process │          │
│  │   Manager    │        │    Spawner   │          │
│  └──────────────┘        └──────────────┘          │
│          │                       │                   │
│          │                       ▼                   │
│          │              ┌──────────────┐            │
│          │              │ Virtual      │            │
│          │              │ Terminal     │            │
│          │              │ (script cmd) │            │
│          │              └──────────────┘            │
│          │                       │                   │
│          ▼                       ▼                   │
│  ┌──────────────┐        ┌──────────────┐          │
│  │  Keyboard    │───────>│   Screen     │          │
│  │  Injector    │        │   Capture    │          │
│  └──────────────┘        └──────────────┘          │
│          │                       │                   │
│          │                       ▼                   │
│          │              ┌──────────────┐            │
│          └──────────────│  Screenshot  │            │
│                         │   Storage    │            │
│                         └──────────────┘            │
└─────────────────────────────────────────────────────┘
```

## Key Components

### 1. **TuiTestAgent** - Main Service

**Location:** `src/services/tui_test_agent.rs`

**Responsibilities:**
- Launch OSVM chat in a virtual terminal
- Send keyboard events programmatically
- Capture terminal screen buffer
- Take screenshots (text-based)
- Run automated test scenarios
- Generate test reports

**API:**
```rust
// Create agent with terminal size
let agent = TuiTestAgent::new(120, 30)?;

// Launch the TUI
agent.launch_advanced_chat().await?;

// Send keyboard input
agent.send_keys("Hello, OSVM!\n").await?;
agent.send_key_event(KeyEvent::Tab).await?;
agent.send_key_event(KeyEvent::F(10)).await?;

// Capture screen
let screenshot = agent.capture_screen().await?;

// Verify UI state
let has_text = agent.verify_text_on_screen("Welcome").await?;

// Save screenshot to file
let path = agent.save_screenshot("test_welcome").await?;

// Stop the TUI
agent.stop().await?;
```

### 2. **PTY-Based Terminal Emulation**

Uses the `script` command to create a pseudo-terminal (PTY):

```rust
let mut cmd = TokioCommand::new("script");
cmd.arg("-q")                    // Quiet mode
    .arg("/dev/null")            // Don't save to file
    .arg("-c")
    .arg("osvm chat --advanced") // Command to run
    .env("TERM", "xterm-256color")
    .env("COLUMNS", "120")
    .env("LINES", "30")
    .stdin(Stdio::piped())
    .stdout(Stdio::piped())
    .stderr(Stdio::piped());
```

**Why `script`?**
- ✅ Portable (works on Linux/macOS)
- ✅ Provides real TTY for TUI applications
- ✅ Captures all terminal output including ANSI codes
- ✅ No external dependencies needed

### 3. **Keyboard Event Injection**

**Supported Key Events:**
```rust
pub enum KeyEvent {
    Char(char),           // Regular characters
    Enter,                // Return/Enter key
    Tab,                  // Tab key
    Escape,               // Esc key
    Backspace,            // Backspace
    Delete,               // Delete key
    Up, Down, Left, Right,// Arrow keys
    F(u8),                // Function keys F1-F12
    Ctrl(char),           // Ctrl+key combinations
    Alt(char),            // Alt+key combinations
    Shift(char),          // Shift+key combinations
}
```

**ANSI Escape Sequence Conversion:**
Each key event is converted to proper ANSI escape sequences:
- `Tab` → `\t`
- `Enter` → `\r`
- `Escape` → `\x1b`
- `F10` → `\x1bO[0-9]+`
- `Ctrl-C` → `\x03`
- `Alt-R` → `\x1br`
- `Arrow Up` → `\x1b[A`

### 4. **Screen Capture System**

**ScreenCapture Structure:**
```rust
pub struct ScreenCapture {
    pub timestamp: DateTime<Utc>,
    pub width: u16,
    pub height: u16,
    pub content: String,        // Clean text (no ANSI codes)
    pub ansi_content: String,   // With ANSI codes preserved
}
```

**Capture Process:**
1. Read from PTY stdout buffer
2. Store raw ANSI output
3. Strip ANSI codes for clean text
4. Preserve both versions for analysis

**ANSI Code Stripping:**
```rust
fn strip_ansi_codes(text: &str) -> String {
    let re = regex::Regex::new(r"\x1b\[[0-9;]*[a-zA-Z]").unwrap();
    re.replace_all(text, "").to_string()
}
```

### 5. **Test Scenarios**

**TestScenario Structure:**
```rust
pub struct TuiTestScenario {
    pub name: String,
    pub description: String,
    pub steps: Vec<TuiTestStep>,
    pub expected_screens: Vec<ScreenExpectation>,
}

pub struct TuiTestStep {
    pub action: TuiAction,
    pub wait_ms: u64,
    pub description: String,
}

pub enum TuiAction {
    SendKeys(String),           // Type text
    SendKey(String),            // Press key (Tab, F10, etc.)
    Wait(u64),                  // Wait milliseconds
    CaptureScreen,              // Take screenshot
    VerifyText(String),         // Assert text present
    VerifyNotText(String),      // Assert text absent
}
```

### 6. **Built-in Test Scenarios**

**1. Basic Navigation Test:**
```rust
TuiTestScenario {
    name: "basic_navigation",
    steps: [
        Wait(2000),              // Wait for UI to load
        CaptureScreen,           // Initial screenshot
        SendKey("Tab"),          // Navigate with Tab
        CaptureScreen,           // After Tab screenshot
    ],
    expected_screens: [
        { after_step: 2, contains_text: ["OSVM", "Chat"] }
    ],
}
```

**2. Help Dialog Test:**
```rust
TuiTestScenario {
    name: "help_dialog",
    steps: [
        Wait(2000),
        SendKey("F10"),          // Open context menu
        CaptureScreen,
        SendKey("Escape"),       // Close menu
        CaptureScreen,
    ],
    expected_screens: [
        { after_step: 3, contains_text: ["Context Menu"] }
    ],
}
```

**3. Message Input Test:**
```rust
TuiTestScenario {
    name: "message_input",
    steps: [
        Wait(2000),
        SendKeys("Hello, OSVM!"),  // Type message
        CaptureScreen,
        VerifyText("Hello, OSVM!"), // Verify it appears
    ],
    expected_screens: [
        { after_step: 3, contains_text: ["Hello, OSVM!"] }
    ],
}
```

## Usage Examples

### Example 1: Simple Test
```rust
use osvm::services::tui_test_agent::TuiTestAgent;

#[tokio::main]
async fn main() -> Result<()> {
    // Create agent (120 cols x 30 rows)
    let agent = TuiTestAgent::new(120, 30)?;

    // Launch TUI
    agent.launch_advanced_chat().await?;

    // Wait for UI to initialize
    tokio::time::sleep(Duration::from_secs(2)).await;

    // Take initial screenshot
    agent.save_screenshot("initial").await?;

    // Press Tab to navigate
    agent.send_key_event(KeyEvent::Tab).await?;
    tokio::time::sleep(Duration::from_millis(500)).await;

    // Take screenshot after Tab
    agent.save_screenshot("after_tab").await?;

    // Verify UI contains expected text
    assert!(agent.verify_text_on_screen("OSVM").await?);

    // Stop the TUI
    agent.stop().await?;

    Ok(())
}
```

### Example 2: Run Scenarios
```rust
use osvm::services::tui_test_agent::{TuiTestAgent, create_default_tui_scenarios};

#[tokio::main]
async fn main() -> Result<()> {
    let agent = TuiTestAgent::new(120, 30)?;
    agent.launch_advanced_chat().await?;

    let scenarios = create_default_tui_scenarios();

    for scenario in scenarios {
        println!("Running: {}", scenario.name);
        let result = agent.run_scenario(&scenario).await?;

        if result.passed {
            println!("✅ PASSED in {:.2}s", result.duration_seconds);
        } else {
            println!("❌ FAILED in {:.2}s", result.duration_seconds);
            for error in &result.errors {
                println!("  - {}", error);
            }
        }

        println!("Screenshots: {:?}", result.screenshots);
    }

    agent.stop().await?;
    Ok(())
}
```

### Example 3: Custom Scenario
```rust
let custom_scenario = TuiTestScenario {
    name: "custom_test".to_string(),
    description: "Test custom functionality".to_string(),
    steps: vec![
        TuiTestStep {
            action: TuiAction::Wait(2000),
            wait_ms: 0,
            description: "Wait for load".to_string(),
        },
        TuiTestStep {
            action: TuiAction::SendKeys("test message".to_string()),
            wait_ms: 500,
            description: "Type test message".to_string(),
        },
        TuiTestStep {
            action: TuiAction::SendKey("Enter".to_string()),
            wait_ms: 1000,
            description: "Send message".to_string(),
        },
        TuiTestStep {
            action: TuiAction::VerifyText("test message".to_string()),
            wait_ms: 0,
            description: "Verify message sent".to_string(),
        },
    ],
    expected_screens: vec![],
};

let result = agent.run_scenario(&custom_scenario).await?;
```

## Screenshot Storage

**Default Location:**
```
~/.osvm/qa/screenshots/
├── basic_navigation_step_1_20251013_225843.txt
├── basic_navigation_step_2_20251013_225844.txt
├── help_dialog_step_3_20251013_225845.txt
└── message_input_step_3_20251013_225846.txt
```

**Screenshot Format:**
```
=== OSVM Advanced Chat Screenshot ===
Timestamp: 2025-10-13 22:58:43
Terminal: 120x30

--- Clean Text ---
Welcome to OSVM Advanced Chat
Main Chat | Analysis | Work Chat

[Your clean terminal output here]

--- With ANSI Codes ---
\x1b[1mWelcome to OSVM Advanced Chat\x1b[0m
\x1b[32mMain Chat\x1b[0m | Analysis | Work Chat

[Your ANSI-formatted terminal output here]
```

## Integration with QA System

The visual TUI testing agent integrates with the existing QA agent system:

```rust
// In handle_qa_command (main.rs):
match matches.subcommand() {
    Some(("visual", sub_m)) => {
        println!("🎨 Running visual TUI tests...");

        let agent = TuiTestAgent::new(120, 30)?;
        agent.launch_advanced_chat().await?;

        let scenarios = create_default_tui_scenarios();
        let mut all_passed = true;

        for scenario in scenarios {
            let result = agent.run_scenario(&scenario).await?;
            if !result.passed {
                all_passed = false;
            }
        }

        agent.stop().await?;

        if !all_passed {
            std::process::exit(1);
        }
    }
    // ... other commands
}
```

## CLI Commands (To Be Added)

```bash
# Run visual TUI tests
osvm qa visual

# Run specific visual test
osvm qa visual --test basic_navigation

# Run with custom terminal size
osvm qa visual --cols 160 --rows 40

# Save screenshots
osvm qa visual --screenshots

# Run in headless mode
osvm qa visual --headless
```

## Testing Best Practices

### 1. **Wait for UI Initialization**
Always wait 2-3 seconds after launching for the UI to fully load:
```rust
agent.launch_advanced_chat().await?;
tokio::time::sleep(Duration::from_secs(2)).await;
```

### 2. **Add Delays Between Actions**
Terminal UIs need time to render:
```rust
agent.send_key_event(KeyEvent::Tab).await?;
tokio::time::sleep(Duration::from_millis(500)).await;
```

### 3. **Capture Before and After**
Take screenshots before and after important actions:
```rust
agent.save_screenshot("before_action").await?;
agent.send_key_event(KeyEvent::F10).await?;
tokio::time::sleep(Duration::from_millis(500)).await;
agent.save_screenshot("after_action").await?;
```

### 4. **Use Text Verification**
Verify expected text appears on screen:
```rust
assert!(agent.verify_text_on_screen("Expected Text").await?);
```

### 5. **Check for Errors**
Verify error messages don't appear:
```rust
let has_error = agent.verify_text_on_screen("Error").await?;
assert!(!has_error, "Unexpected error message found");
```

## Debugging Tips

### View Raw Terminal Output
```rust
let output = agent.get_recent_output(20).await;
println!("Last 20 lines:\n{}", output);
```

### Inspect Screen Buffer
```rust
let capture = agent.capture_screen().await?;
println!("Clean text:\n{}", capture.content);
println!("ANSI text:\n{}", capture.ansi_content);
```

### Check Process Status
```rust
let process = agent.process.lock().await;
if let Some(child) = process.as_ref() {
    println!("Process running: PID {:?}", child.id());
}
```

## Limitations

### Current Limitations:
1. **No image screenshots** - Text-based only (could be extended with `termshot`)
2. **No mouse events** - Keyboard only
3. **No clipboard verification** - Can't test clipboard operations
4. **Limited ANSI parsing** - Basic ANSI code stripping only

### Future Enhancements:
1. **Image screenshots** using `termshot` or similar
2. **Mouse event injection** for click testing
3. **Advanced ANSI parsing** to detect colors, styles
4. **Visual regression testing** comparing screenshots
5. **Performance metrics** (render time, responsiveness)
6. **CI/CD integration** with GitHub Actions

## Dependencies

**Required:**
- `tokio` - Async runtime
- `regex` - ANSI code stripping
- `chrono` - Timestamps
- `serde` / `serde_json` - Serialization
- `anyhow` - Error handling
- `log` - Logging

**System Requirements:**
- `script` command (Linux/macOS)
- Terminal with PTY support
- ANSI-capable terminal emulator

## Testing the Test Agent

```bash
# Build
cargo build

# Run unit tests
cargo test tui_test_agent

# Run with actual TUI (needs TTY)
cargo run --bin osvm -- qa visual
```

## Summary

The Visual TUI Testing Agent provides:
✅ **Automated visual testing** for terminal UIs
✅ **PTY-based terminal emulation** for real TUI execution
✅ **Keyboard event injection** with full key support
✅ **Screen capture** with ANSI code handling
✅ **Test scenarios** with expectations and validation
✅ **Screenshot storage** for visual debugging
✅ **Integration ready** with QA system

This enables comprehensive automated testing of the OSVM advanced chat TUI without manual interaction!

## Contact & Contribution

For issues or enhancements:
- GitHub: https://github.com/opensvm/osvm-cli/issues
- Documentation: `docs/Visual_TUI_Testing.md`
- Source: `src/services/tui_test_agent.rs`
