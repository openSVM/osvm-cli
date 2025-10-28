//! Test for pump.fun monitoring plan with WHILE and FOR loops

use anyhow::Result;
use osvm::services::ovsm_executor::{McpToolExecutor, OvsmExecutor};
use serde_json::json;
use std::sync::{Arc, Mutex};

struct TimestampTool {
    time: Arc<Mutex<i64>>,
}

impl McpToolExecutor for TimestampTool {
    fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
        let mut t = self.time.lock().unwrap();
        let current = *t;
        *t += 35; // Advance 35 seconds each call
        Ok(json!(current))
    }
}

struct FetchTool;
impl McpToolExecutor for FetchTool {
    fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
        Ok(json!({
            "transaction_count": 1000,
            "timestamp": "2024-01-01T00:00:00Z"
        }))
    }
}

struct AnalyzeTool {
    call_count: Arc<Mutex<usize>>,
}

impl McpToolExecutor for AnalyzeTool {
    fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
        let mut count = self.call_count.lock().unwrap();
        *count += 1;

        // Return patterns every other call
        if *count % 2 == 0 {
            Ok(json!({
                "patterns_found": 2,
                "patterns": [
                    {"type": "cabal", "profit_score": 95},
                    {"type": "anomaly", "deviation": 3.2}
                ]
            }))
        } else {
            Ok(json!({"patterns_found": 0, "patterns": []}))
        }
    }
}

struct RankTool;
impl McpToolExecutor for RankTool {
    fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
        Ok(json!([
            {"type": "cabal", "profit_score": 95, "details": "Coordinated wallet activity detected"},
            {"type": "anomaly", "deviation": 3.2, "details": "Unusual volume spike"}
        ]))
    }
}

struct ReportTool {
    reports: Arc<Mutex<Vec<String>>>,
}

impl McpToolExecutor for ReportTool {
    fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
        let mut reports = self.reports.lock().unwrap();
        reports.push("Pattern reported to chat".to_string());
        Ok(json!({"status": "reported"}))
    }
}

struct SleepTool;
impl McpToolExecutor for SleepTool {
    fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
        // In real execution this would sleep for 30s
        Ok(json!({"slept": "30s"}))
    }
}

struct NoopTool;
impl McpToolExecutor for NoopTool {
    fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
        Ok(json!(null))
    }
}

struct SummaryTool;
impl McpToolExecutor for SummaryTool {
    fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
        Ok(json!({"summary": "Monitoring complete"}))
    }
}

#[tokio::test]
async fn test_pumpfun_monitor_plan() -> Result<()> {
    let time = Arc::new(Mutex::new(0i64));
    let call_count = Arc::new(Mutex::new(0usize));
    let reports = Arc::new(Mutex::new(Vec::new()));

    let executor = OvsmExecutor::new(false);

    // Register all tools
    executor
        .register_tool(
            "get_current_timestamp".to_string(),
            Box::new(TimestampTool { time: time.clone() }),
        )
        .await?;
    executor
        .register_tool(
            "fetch_pumpfun_transactions".to_string(),
            Box::new(FetchTool),
        )
        .await?;
    executor
        .register_tool(
            "analyze_transaction_patterns".to_string(),
            Box::new(AnalyzeTool {
                call_count: call_count.clone(),
            }),
        )
        .await?;
    executor
        .register_tool("rank_patterns_by_profit".to_string(), Box::new(RankTool))
        .await?;
    executor
        .register_tool(
            "report_to_chat".to_string(),
            Box::new(ReportTool {
                reports: reports.clone(),
            }),
        )
        .await?;
    executor
        .register_tool("sleep".to_string(), Box::new(SleepTool))
        .await?;
    executor
        .register_tool("noop".to_string(), Box::new(NoopTool))
        .await?;
    executor
        .register_tool("report_final_summary".to_string(), Box::new(SummaryTool))
        .await?;

    // Load the plan
    let plan = std::fs::read_to_string("examples/pumpfun_monitor.ovsm")?;

    // Execute
    let result = executor.execute_plan(&plan).await?;

    // Verify execution
    let iterations = result.value.as_i64().unwrap_or(0);

    println!("✓ Pump.fun monitor executed:");
    println!("  Iterations: {}", iterations);
    println!("  Tools called: {}", result.tools_called.len());
    println!("  Reports generated: {}", reports.lock().unwrap().len());

    // Should run multiple iterations (time advances 35s per call, need 3600s)
    assert!(iterations >= 100, "Should run ~100+ iterations for 1 hour");

    // Should have called various tools
    assert!(result
        .tools_called
        .contains(&"fetch_pumpfun_transactions".to_string()));
    assert!(result
        .tools_called
        .contains(&"analyze_transaction_patterns".to_string()));

    // Should have generated some reports
    assert!(
        reports.lock().unwrap().len() > 0,
        "Should generate pattern reports"
    );

    Ok(())
}
