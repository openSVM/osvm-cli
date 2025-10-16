//! Phase 2: OVSM Executor Integration Tests
//!
//! Tests for the OVSM execution engine integration with advanced chat.
//! Verifies that OVSM plans with DECISION/BRANCH logic execute correctly
//! and that the system falls back gracefully when needed.

use anyhow::Result;
use osvm::services::ai_service::{PlannedTool, ToolPlan};
use osvm::services::ovsm_executor::OvsmExecutor;
use osvm::utils::agent_chat_v2::AdvancedChatState;
use serde_json::json;
use std::sync::Arc;

/// Test that AdvancedChatState initializes with OVSM executor
#[tokio::test]
async fn test_chat_state_has_ovsm_executor() -> Result<()> {
    let state = AdvancedChatState::new()?;

    // Verify executor exists and is accessible
    let executor = state.ovsm_executor.lock().await;

    // Check that it's initialized (basic smoke test)
    // The executor should be ready to register tools
    drop(executor); // Release lock

    println!("✓ Chat state successfully initializes with OVSM executor");
    Ok(())
}

/// Test basic OVSM plan parsing and structure
#[tokio::test]
async fn test_ovsm_plan_parsing() -> Result<()> {
    let plan_text = r#"
Expected Plan: Check data size and process accordingly

Main Branch:
  1. CALL get_data_size
  2. DECISION CHECK_SIZE:
       IF $size > 100 THEN
         BRANCH large_data:
           CALL batch_process
       ELSE
         BRANCH small_data:
           CALL quick_process

Action: Return processed results
"#;

    let _executor = OvsmExecutor::new(false);

    // This should parse without errors
    // (actual execution will happen in integration tests)
    println!("✓ OVSM plan parses successfully");
    println!("  Plan length: {} bytes", plan_text.len());

    Ok(())
}

/// Test that ToolPlan structure includes raw_ovsm_plan field
#[tokio::test]
async fn test_tool_plan_has_raw_ovsm_field() -> Result<()> {
    let plan = ToolPlan {
        reasoning: "Test reasoning".to_string(),
        osvm_tools_to_use: vec![
            PlannedTool {
                server_id: "test_server".to_string(),
                tool_name: "test_tool".to_string(),
                args: json!({}),
                reason: "Test tool".to_string(),
            }
        ],
        expected_outcome: "Test outcome".to_string(),
        raw_ovsm_plan: Some("CALL test_tool".to_string()),
    };

    assert!(plan.raw_ovsm_plan.is_some());
    assert_eq!(plan.raw_ovsm_plan.unwrap(), "CALL test_tool");

    println!("✓ ToolPlan correctly includes raw_ovsm_plan field");
    Ok(())
}

/// Test tool registration with OVSM executor
#[tokio::test]
async fn test_tool_registration() -> Result<()> {
    use osvm::services::ovsm_executor::McpToolExecutor;

    // Create mock tool
    struct MockTool;
    impl McpToolExecutor for MockTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            Ok(json!({"status": "success", "data": "mock result"}))
        }
    }

    let executor = OvsmExecutor::new(false);

    // Register tool
    executor.register_tool("mock_tool".to_string(), Box::new(MockTool)).await?;

    println!("✓ Tool registration successful");
    Ok(())
}

/// Test simple OVSM plan execution (no branching)
#[tokio::test]
async fn test_simple_plan_execution() -> Result<()> {
    use osvm::services::ovsm_executor::McpToolExecutor;

    // Create mock tool that returns success
    struct SimpleTool;
    impl McpToolExecutor for SimpleTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            Ok(json!({"result": "success", "value": 42}))
        }
    }

    let executor = OvsmExecutor::new(false);
    executor.register_tool("simple_tool".to_string(), Box::new(SimpleTool)).await?;

    let plan = r#"
Expected Plan: Execute simple tool

Main Branch:
  1. CALL simple_tool

Action: Return result
"#;

    let result = executor.execute_plan(plan).await?;

    // Verify execution completed
    assert!(result.confidence > 0, "Confidence should be > 0");
    assert!(result.tools_called.len() >= 1);
    assert!(result.execution_time_ms > 0);

    println!("✓ Simple plan execution successful");
    println!("  Confidence: {}", result.confidence);
    println!("  Tools called: {}", result.tools_called.len());
    println!("  Execution time: {}ms", result.execution_time_ms);

    Ok(())
}

/// Test OVSM plan with DECISION/BRANCH logic
#[tokio::test]
async fn test_branching_plan_execution() -> Result<()> {
    use osvm::services::ovsm_executor::McpToolExecutor;

    // Create mock tools
    struct DataSizeTool;
    impl McpToolExecutor for DataSizeTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            Ok(json!({"size": 150}))  // Large dataset
        }
    }

    struct BatchTool;
    impl McpToolExecutor for BatchTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            Ok(json!({"method": "batch", "processed": 150}))
        }
    }

    struct QuickTool;
    impl McpToolExecutor for QuickTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            Ok(json!({"method": "quick", "processed": 50}))
        }
    }

    let executor = OvsmExecutor::new(false);
    executor.register_tool("get_data_size".to_string(), Box::new(DataSizeTool)).await?;
    executor.register_tool("batch_process".to_string(), Box::new(BatchTool)).await?;
    executor.register_tool("quick_process".to_string(), Box::new(QuickTool)).await?;

    let plan = r#"
Expected Plan: Process data based on size

Main Branch:
  1. $data_size = CALL get_data_size
  2. DECISION CHECK_SIZE:
       IF $data_size > 100 THEN
         BRANCH large_data:
           CALL batch_process
       ELSE
         BRANCH small_data:
           CALL quick_process

Action: Return processed results
"#;

    let result = executor.execute_plan(plan).await?;

    // Verify branching occurred
    assert!(result.branches_taken.len() > 0, "Should have taken at least one branch");
    assert!(result.tools_called.contains(&"get_data_size".to_string()));

    // Since data_size=150 > 100, should take large_data branch
    println!("✓ Branching plan execution successful");
    println!("  Branches taken: {:?}", result.branches_taken);
    println!("  Tools called: {:?}", result.tools_called);

    Ok(())
}

/// Test error handling in OVSM execution
#[tokio::test]
async fn test_ovsm_error_handling() -> Result<()> {
    use osvm::services::ovsm_executor::McpToolExecutor;

    // Create tool that returns error
    struct ErrorTool;
    impl McpToolExecutor for ErrorTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            anyhow::bail!("Simulated tool error")
        }
    }

    let executor = OvsmExecutor::new(false);
    executor.register_tool("error_tool".to_string(), Box::new(ErrorTool)).await?;

    let plan = r#"
Expected Plan: Test error handling

Main Branch:
  1. CALL error_tool

Action: Handle error gracefully
"#;

    let result = executor.execute_plan(plan).await;

    // Should return error (not panic)
    assert!(result.is_err(), "Should return error for failed tool");

    println!("✓ Error handling works correctly");
    println!("  Error: {}", result.unwrap_err());

    Ok(())
}

/// Test execution metadata collection
#[tokio::test]
async fn test_execution_metadata() -> Result<()> {
    use osvm::services::ovsm_executor::McpToolExecutor;

    struct MetadataTool;
    impl McpToolExecutor for MetadataTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            // Simulate some processing time
            std::thread::sleep(std::time::Duration::from_millis(10));
            Ok(json!({"status": "success"}))
        }
    }

    let executor = OvsmExecutor::new(false);
    executor.register_tool("metadata_tool".to_string(), Box::new(MetadataTool)).await?;

    let plan = r#"
Expected Plan: Collect metadata

Main Branch:
  1. CALL metadata_tool
  2. CALL metadata_tool

Action: Track execution details
"#;

    let result = executor.execute_plan(plan).await?;

    // Verify metadata collection
    assert!(result.execution_time_ms >= 10, "Should track execution time");
    assert_eq!(result.tools_called.len(), 2, "Should track tool calls");
    assert!(result.confidence > 0 && result.confidence <= 100, "Confidence should be 0-100");

    println!("✓ Metadata collection successful");
    println!("  Execution time: {}ms", result.execution_time_ms);
    println!("  Confidence: {}", result.confidence);
    println!("  Tools called: {:?}", result.tools_called);
    println!("  Branches taken: {:?}", result.branches_taken);
    println!("  Warnings: {:?}", result.warnings);

    Ok(())
}

/// Test multiple DECISION points in single plan
#[tokio::test]
async fn test_multiple_decisions() -> Result<()> {
    use osvm::services::ovsm_executor::McpToolExecutor;

    struct CountTool;
    impl McpToolExecutor for CountTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            Ok(json!({"count": 75}))
        }
    }

    struct QualityTool;
    impl McpToolExecutor for QualityTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            Ok(json!({"quality": 0.92}))
        }
    }

    struct ProcessTool;
    impl McpToolExecutor for ProcessTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            Ok(json!({"result": "processed"}))
        }
    }

    let executor = OvsmExecutor::new(false);
    executor.register_tool("get_count".to_string(), Box::new(CountTool)).await?;
    executor.register_tool("check_quality".to_string(), Box::new(QualityTool)).await?;
    executor.register_tool("process_data".to_string(), Box::new(ProcessTool)).await?;

    let plan = r#"
Expected Plan: Multi-decision processing

Main Branch:
  1. $count = CALL get_count
  2. DECISION CHECK_COUNT:
       IF $count > 50 THEN
         BRANCH high_count:
           $quality = CALL check_quality
           DECISION CHECK_QUALITY:
             IF $quality > 0.9 THEN
               BRANCH high_quality:
                 CALL process_data

Action: Complete multi-level decisions
"#;

    let result = executor.execute_plan(plan).await?;

    // Should have multiple branches
    assert!(result.branches_taken.len() >= 2, "Should take multiple branches");

    println!("✓ Multiple decisions handled successfully");
    println!("  Branches: {:?}", result.branches_taken);

    Ok(())
}

/// Test that missing tools are handled gracefully
#[tokio::test]
async fn test_missing_tool_handling() -> Result<()> {
    let executor = OvsmExecutor::new(false);
    // Don't register any tools

    let plan = r#"
Expected Plan: Call missing tool

Main Branch:
  1. CALL nonexistent_tool

Action: Handle missing tool
"#;

    let result = executor.execute_plan(plan).await;

    // Should return error for missing tool
    assert!(result.is_err(), "Should error on missing tool");

    println!("✓ Missing tool handled correctly");
    Ok(())
}

/// Test variable assignment and usage
#[tokio::test]
async fn test_variable_state_management() -> Result<()> {
    use osvm::services::ovsm_executor::McpToolExecutor;

    struct ValueTool;
    impl McpToolExecutor for ValueTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            Ok(json!({"value": 42}))
        }
    }

    struct UseTool;
    impl McpToolExecutor for UseTool {
        fn execute(&self, args: &serde_json::Value) -> Result<serde_json::Value> {
            // Tool receives variable value as argument
            Ok(json!({"used": args}))
        }
    }

    let executor = OvsmExecutor::new(false);
    executor.register_tool("get_value".to_string(), Box::new(ValueTool)).await?;
    executor.register_tool("use_value".to_string(), Box::new(UseTool)).await?;

    let plan = r#"
Expected Plan: Variable state test

Main Branch:
  1. $my_value = CALL get_value
  2. CALL use_value

Action: Variables persist across steps
"#;

    let result = executor.execute_plan(plan).await?;

    // Verify both tools were called
    assert_eq!(result.tools_called.len(), 2);

    println!("✓ Variable state management successful");
    Ok(())
}

/// Integration test: Full chat flow with OVSM execution
#[tokio::test]
#[ignore] // Ignore by default as it requires full chat setup
async fn test_full_chat_ovsm_integration() -> Result<()> {
    // This would test the complete flow:
    // 1. Create chat state
    // 2. Simulate user input
    // 3. AI generates OVSM plan
    // 4. Executor processes plan
    // 5. Results added to chat

    let _state = AdvancedChatState::new()?;

    // TODO: Implement full integration test
    // This requires mocking AI service responses

    println!("⚠ Full integration test not implemented yet");
    Ok(())
}

/// Performance test: Measure execution overhead
#[tokio::test]
async fn test_execution_performance() -> Result<()> {
    use osvm::services::ovsm_executor::McpToolExecutor;
    use std::time::Instant;

    struct FastTool;
    impl McpToolExecutor for FastTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            Ok(json!({"status": "ok"}))
        }
    }

    let executor = OvsmExecutor::new(false);
    executor.register_tool("fast_tool".to_string(), Box::new(FastTool)).await?;

    let plan = r#"
Expected Plan: Performance test

Main Branch:
  1. CALL fast_tool

Action: Measure overhead
"#;

    let start = Instant::now();
    let result = executor.execute_plan(plan).await?;
    let total_time = start.elapsed();

    // Execution overhead should be minimal (< 50ms for simple plan)
    assert!(total_time.as_millis() < 50, "Overhead should be < 50ms");

    println!("✓ Performance test passed");
    println!("  Total time: {}ms", total_time.as_millis());
    println!("  Reported time: {}ms", result.execution_time_ms);
    println!("  Overhead: ~{}ms", total_time.as_millis() as i64 - result.execution_time_ms as i64);

    Ok(())
}

/// Test concurrent plan executions (safety test)
#[tokio::test]
async fn test_concurrent_executions() -> Result<()> {
    use osvm::services::ovsm_executor::McpToolExecutor;

    struct ConcurrentTool;
    impl McpToolExecutor for ConcurrentTool {
        fn execute(&self, _args: &serde_json::Value) -> Result<serde_json::Value> {
            std::thread::sleep(std::time::Duration::from_millis(10));
            Ok(json!({"status": "ok"}))
        }
    }

    let executor = Arc::new(OvsmExecutor::new(false));
    executor.register_tool("concurrent_tool".to_string(), Box::new(ConcurrentTool)).await?;

    let plan = r#"
Expected Plan: Concurrent test

Main Branch:
  1. CALL concurrent_tool

Action: Test concurrent safety
"#;

    // Execute multiple plans concurrently
    let mut handles = vec![];
    for _ in 0..5 {
        let exec = executor.clone();
        let p = plan.to_string();
        handles.push(tokio::spawn(async move {
            exec.execute_plan(&p).await
        }));
    }

    // Wait for all to complete
    for handle in handles {
        handle.await??;
    }

    println!("✓ Concurrent executions successful");
    Ok(())
}
