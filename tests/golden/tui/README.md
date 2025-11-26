# TUI Screenshot Testing Framework

This directory contains golden images for visual regression testing of the OSVM research TUI.

## Overview

The screenshot testing framework captures terminal UI output and compares it against known-good "golden" images. This catches visual regressions that unit tests might miss:

- Layout changes
- Border/box drawing issues
- Color coding changes
- Text alignment problems
- Widget sizing issues

## How It Works

1. **Capture**: Uses ratatui's `TestBackend` to render widgets to an in-memory buffer
2. **Convert**: Converts the buffer to plain text (strips ANSI codes)
3. **Compare**: Compares against golden images line-by-line
4. **Report**: Shows detailed diff on mismatch

## Running Tests

```bash
# Run screenshot tests (compares against golden images)
cargo test --test tui_screenshot_tests

# Update golden images (after intentional UI changes)
UPDATE_GOLDENS=1 cargo test --test tui_screenshot_tests

# Run with verbose output
cargo test --test tui_screenshot_tests -- --nocapture
```

## Golden Images

| File | Description |
|------|-------------|
| `ai_insights_low_risk.txt` | AI Insights panel with low risk score (green) |
| `ai_insights_medium_risk.txt` | AI Insights panel with medium risk (yellow) |
| `ai_insights_high_risk.txt` | AI Insights panel with high risk + alerts (orange) |
| `ai_insights_critical_risk.txt` | AI Insights panel with critical risk + multiple alerts (red) |
| `behavior_bot.txt` | Wallet behavior: Bot classification |
| `behavior_mixer.txt` | Wallet behavior: Mixer classification |
| `behavior_exchange.txt` | Wallet behavior: Exchange classification |
| `combined_dashboard_panel.txt` | Combined AI Insights + Behavior + Stats layout |
| `empty_state.txt` | Loading/empty state display |

## Adding New Tests

1. Create a render function for your widget:

```rust
fn render_my_widget(f: &mut Frame, area: Rect) {
    let widget = MyWidget::new();
    f.render_widget(widget, area);
}
```

2. Add a test case:

```rust
#[test]
fn screenshot_my_widget() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("my_widget", 80, 24, |f, area| {
        render_my_widget(f, area);
    }).unwrap();
}
```

3. Generate the golden image:

```bash
UPDATE_GOLDENS=1 cargo test --test tui_screenshot_tests screenshot_my_widget
```

4. Review the generated `tests/golden/tui/my_widget.txt` and commit it

## When to Update Golden Images

Update golden images when you **intentionally** change the UI:

- Adding new information to panels
- Changing layout or spacing
- Modifying colors or formatting
- Updating icons or labels

**Never** update goldens without reviewing the diff to ensure changes are intentional!

## Debugging Test Failures

When a test fails, it saves the actual output to `*.actual.txt`:

```
tests/golden/tui/my_widget.actual.txt
```

Compare this against the golden to see the difference:

```bash
diff tests/golden/tui/my_widget.txt tests/golden/tui/my_widget.actual.txt
```

## Framework Location

- Framework code: `src/utils/tui/screenshot_test.rs`
- Test cases: `tests/tui_screenshot_tests.rs`
- Golden images: `tests/golden/tui/`

## Integration with CI

These tests run automatically in CI. Any visual regression will fail the build:

```yaml
- name: Run TUI screenshot tests
  run: cargo test --test tui_screenshot_tests
```

To update goldens in CI (for intentional changes), set the environment variable:

```yaml
- name: Update golden images
  env:
    UPDATE_GOLDENS: 1
  run: cargo test --test tui_screenshot_tests
```

## tmux-Based Testing (Advanced)

For integration tests that need real terminal behavior, the framework includes `TmuxCapture`:

```rust
let capture = TmuxCapture::new(80, 24)?;
capture.run_command("./target/release/osvm research WALLET --tui")?;
std::thread::sleep(Duration::from_secs(2));
let screenshot = capture.capture()?;
```

This is useful for testing:
- Keyboard input handling
- Animation/refresh behavior
- Real terminal escape code rendering
