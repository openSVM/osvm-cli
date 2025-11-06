//! Browser automation tools for OVSM
//!
//! Provides headless browser control via Playwright integration.
//! These tools enable web scraping, testing, and automated interactions.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::Tool;

/// Browser navigation tool
pub struct BrowserNavigateTool;

impl Tool for BrowserNavigateTool {
    fn name(&self) -> &str {
        "browser_navigate"
    }

    fn description(&self) -> &str {
        "Navigate browser to a URL"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Missing URL argument".to_string(),
            });
        }

        let url = match &args[0] {
            Value::String(s) => s.clone(),
            _ => {
                return Err(Error::InvalidArguments {
                    tool: self.name().to_string(),
                    reason: "URL must be a string".to_string(),
                })
            }
        };

        // Return a placeholder response - actual execution handled by MCP
        Ok(Value::String(format!(
            "{{\"action\":\"navigate\",\"url\":\"{}\",\"status\":\"queued\"}}",
            url
        )))
    }

    fn arity(&self) -> Option<usize> {
        Some(1)
    }
}

/// Browser screenshot tool
pub struct BrowserScreenshotTool;

impl Tool for BrowserScreenshotTool {
    fn name(&self) -> &str {
        "browser_screenshot"
    }

    fn description(&self) -> &str {
        "Take a screenshot of the current browser page"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        let filename = if !args.is_empty() {
            match &args[0] {
                Value::String(s) => Some(s.clone()),
                _ => None,
            }
        } else {
            None
        };

        let result = if let Some(fname) = filename {
            format!(
                "{{\"action\":\"screenshot\",\"filename\":\"{}\",\"status\":\"queued\"}}",
                fname
            )
        } else {
            "{\"action\":\"screenshot\",\"status\":\"queued\"}".to_string()
        };

        Ok(Value::String(result))
    }

    fn arity(&self) -> Option<usize> {
        None // Variadic - 0 or 1 args
    }
}

/// Browser click tool
pub struct BrowserClickTool;

impl Tool for BrowserClickTool {
    fn name(&self) -> &str {
        "browser_click"
    }

    fn description(&self) -> &str {
        "Click an element on the page using CSS selector"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Missing selector argument".to_string(),
            });
        }

        let selector = match &args[0] {
            Value::String(s) => s.clone(),
            _ => {
                return Err(Error::InvalidArguments {
                    tool: self.name().to_string(),
                    reason: "Selector must be a string".to_string(),
                })
            }
        };

        Ok(Value::String(format!(
            "{{\"action\":\"click\",\"selector\":\"{}\",\"status\":\"queued\"}}",
            selector
        )))
    }

    fn arity(&self) -> Option<usize> {
        Some(1)
    }
}

/// Browser type tool
pub struct BrowserTypeTool;

impl Tool for BrowserTypeTool {
    fn name(&self) -> &str {
        "browser_type"
    }

    fn description(&self) -> &str {
        "Type text into an element using CSS selector"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Missing selector and/or text arguments".to_string(),
            });
        }

        let selector = match &args[0] {
            Value::String(s) => s.clone(),
            _ => {
                return Err(Error::InvalidArguments {
                    tool: self.name().to_string(),
                    reason: "Selector must be a string".to_string(),
                })
            }
        };

        let text = match &args[1] {
            Value::String(s) => s.clone(),
            _ => {
                return Err(Error::InvalidArguments {
                    tool: self.name().to_string(),
                    reason: "Text must be a string".to_string(),
                })
            }
        };

        Ok(Value::String(format!(
            "{{\"action\":\"type\",\"selector\":\"{}\",\"text\":\"{}\",\"status\":\"queued\"}}",
            selector, text
        )))
    }

    fn arity(&self) -> Option<usize> {
        Some(2)
    }
}

/// Browser snapshot tool
pub struct BrowserSnapshotTool;

impl Tool for BrowserSnapshotTool {
    fn name(&self) -> &str {
        "browser_snapshot"
    }

    fn description(&self) -> &str {
        "Capture accessibility snapshot of the current page"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String(
            "{\"action\":\"snapshot\",\"status\":\"queued\"}".to_string(),
        ))
    }

    fn arity(&self) -> Option<usize> {
        Some(0)
    }
}

/// Browser evaluate tool
pub struct BrowserEvaluateTool;

impl Tool for BrowserEvaluateTool {
    fn name(&self) -> &str {
        "browser_evaluate"
    }

    fn description(&self) -> &str {
        "Evaluate JavaScript in the page context"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Missing script argument".to_string(),
            });
        }

        let script = match &args[0] {
            Value::String(s) => s.clone(),
            _ => {
                return Err(Error::InvalidArguments {
                    tool: self.name().to_string(),
                    reason: "Script must be a string".to_string(),
                })
            }
        };

        Ok(Value::String(format!(
            "{{\"action\":\"evaluate\",\"script\":\"{}\",\"status\":\"queued\"}}",
            script.replace("\"", "\\\"")
        )))
    }

    fn arity(&self) -> Option<usize> {
        Some(1)
    }
}

/// Browser wait tool
pub struct BrowserWaitForTool;

impl Tool for BrowserWaitForTool {
    fn name(&self) -> &str {
        "browser_wait_for"
    }

    fn description(&self) -> &str {
        "Wait for an element to appear on the page"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: self.name().to_string(),
                reason: "Missing selector argument".to_string(),
            });
        }

        let selector = match &args[0] {
            Value::String(s) => s.clone(),
            _ => {
                return Err(Error::InvalidArguments {
                    tool: self.name().to_string(),
                    reason: "Selector must be a string".to_string(),
                })
            }
        };

        let timeout = if args.len() > 1 {
            match &args[1] {
                Value::Float(n) => Some(*n as u64),
                Value::Int(n) => Some(*n as u64),
                _ => None,
            }
        } else {
            None
        };

        let result = if let Some(timeout_ms) = timeout {
            format!(
                "{{\"action\":\"wait_for\",\"selector\":\"{}\",\"timeout_ms\":{},\"status\":\"queued\"}}",
                selector, timeout_ms
            )
        } else {
            format!(
                "{{\"action\":\"wait_for\",\"selector\":\"{}\",\"status\":\"queued\"}}",
                selector
            )
        };

        Ok(Value::String(result))
    }

    fn arity(&self) -> Option<usize> {
        None // Variadic - 1 or 2 args
    }
}

/// Register all browser automation tools
pub fn register_browser_tools(
    registry: &mut crate::tools::ToolRegistry,
) -> Result<()> {
    registry.register(BrowserNavigateTool);
    registry.register(BrowserScreenshotTool);
    registry.register(BrowserClickTool);
    registry.register(BrowserTypeTool);
    registry.register(BrowserSnapshotTool);
    registry.register(BrowserEvaluateTool);
    registry.register(BrowserWaitForTool);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_browser_navigate_tool() {
        let tool = BrowserNavigateTool;
        assert_eq!(tool.name(), "browser_navigate");
        assert_eq!(tool.arity(), Some(1));

        let result = tool
            .execute(&[Value::String("https://example.com".to_string())])
            .unwrap();
        match result {
            Value::String(s) => assert!(s.contains("navigate")),
            _ => panic!("Expected string result"),
        }
    }

    #[test]
    fn test_browser_click_tool() {
        let tool = BrowserClickTool;
        assert_eq!(tool.name(), "browser_click");

        let result = tool
            .execute(&[Value::String("#button".to_string())])
            .unwrap();
        match result {
            Value::String(s) => assert!(s.contains("click")),
            _ => panic!("Expected string result"),
        }
    }

    #[test]
    fn test_browser_type_tool() {
        let tool = BrowserTypeTool;
        assert_eq!(tool.name(), "browser_type");
        assert_eq!(tool.arity(), Some(2));

        let result = tool
            .execute(&[
                Value::String("#input".to_string()),
                Value::String("test text".to_string()),
            ])
            .unwrap();
        match result {
            Value::String(s) => {
                assert!(s.contains("type"));
                assert!(s.contains("test text"));
            }
            _ => panic!("Expected string result"),
        }
    }

    #[test]
    fn test_browser_snapshot_tool() {
        let tool = BrowserSnapshotTool;
        assert_eq!(tool.name(), "browser_snapshot");
        assert_eq!(tool.arity(), Some(0));

        let result = tool.execute(&[]).unwrap();
        match result {
            Value::String(s) => assert!(s.contains("snapshot")),
            _ => panic!("Expected string result"),
        }
    }

    #[test]
    fn test_browser_evaluate_tool() {
        let tool = BrowserEvaluateTool;
        assert_eq!(tool.name(), "browser_evaluate");

        let result = tool
            .execute(&[Value::String("document.title".to_string())])
            .unwrap();
        match result {
            Value::String(s) => assert!(s.contains("evaluate")),
            _ => panic!("Expected string result"),
        }
    }

    #[test]
    fn test_browser_wait_for_tool() {
        let tool = BrowserWaitForTool;
        assert_eq!(tool.name(), "browser_wait_for");

        // Test with just selector
        let result = tool
            .execute(&[Value::String(".loading".to_string())])
            .unwrap();
        match result {
            Value::String(s) => assert!(s.contains("wait_for")),
            _ => panic!("Expected string result"),
        }

        // Test with selector and timeout
        let result = tool
            .execute(&[Value::String(".loading".to_string()), Value::Float(5000.0)])
            .unwrap();
        match result {
            Value::String(s) => {
                assert!(s.contains("wait_for"));
                assert!(s.contains("5000"));
            }
            _ => panic!("Expected string result"),
        }
    }
}
