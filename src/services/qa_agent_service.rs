//! QA Agent Service - Automated testing and bug reporting for OSVM CLI
//!
//! This service uses the advanced chat interface programmatically to:
//! 1. Execute test scenarios as chat conversations
//! 2. Detect bugs and unexpected behaviors
//! 3. Use AI to analyze issues and suggest fixes
//! 4. Automatically create GitHub issues with detailed reports
//!
//! Usage:
//! ```bash
//! # Run QA tests with default scenarios
//! osvm qa run
//!
//! # Run specific test category
//! osvm qa run --category rpc
//!
//! # Run tests and create GitHub issues for found bugs
//! osvm qa run --create-issues
//!
//! # Run interactive QA session
//! osvm qa interactive
//! ```

use crate::services::ai_service::AiService;
use crate::services::mcp_service::McpService;
use crate::utils::agent_chat::{BasicAgentState, BasicChatMessage, ProgrammaticChatState};
use crate::utils::agent_chat_v2::{
    AdvancedChatState, AgentState as AdvancedAgentState, ChatMessage as AdvancedChatMessage,
    ChatSession,
};
use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use log::{error, info, warn};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::time::{timeout, Duration};
use uuid::Uuid;

/// Chat mode for QA testing
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Default)]
pub enum ChatMode {
    /// Basic terminal chat mode (without --advanced)
    #[default]
    Basic,
    /// Advanced FAR-style TUI chat mode (with --advanced)
    Advanced,
}

/// Test scenario category
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum TestCategory {
    Basic,  // Basic commands (balance, version, help)
    Rpc,    // RPC operations
    Svm,    // SVM management
    Nodes,  // Node operations
    Deploy, // Deployment operations
    Audit,  // Security auditing
    Mcp,    // MCP server operations
    Ovsm,   // OVSM script execution
    Chat,   // Chat functionality
    All,    // All categories
}

/// Test scenario definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestScenario {
    pub id: String,
    pub name: String,
    pub category: TestCategory,
    pub description: String,
    pub steps: Vec<TestStep>,
    pub expected_behavior: String,
    pub timeout_seconds: u64,
}

/// Individual test step
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestStep {
    pub action: String,                    // User input to send to chat
    pub expected_response: Option<String>, // Optional expected response pattern
    pub timeout_seconds: u64,              // Max wait time for response
}

/// Bug detection result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BugReport {
    pub id: Uuid,
    pub detected_at: DateTime<Utc>,
    pub test_scenario: String,
    pub category: TestCategory,
    pub severity: BugSeverity,
    pub title: String,
    pub description: String,
    pub reproduction_steps: Vec<String>,
    pub actual_behavior: String,
    pub expected_behavior: String,
    pub error_messages: Vec<String>,
    pub chat_transcript: Vec<String>, // Changed to Vec<String> to support both chat modes
    pub ai_analysis: Option<String>,
    pub suggested_fix: Option<String>,
    pub github_issue_url: Option<String>,
}

/// Bug severity level
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum BugSeverity {
    Critical, // Crashes, data loss, security issues
    High,     // Major functionality broken
    Medium,   // Minor functionality issues
    Low,      // UI glitches, typos
    Info,     // Suggestions, improvements
}

/// QA test result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestResult {
    pub scenario_id: String,
    pub passed: bool,
    pub duration_seconds: f64,
    pub bugs_found: Vec<BugReport>,
    pub warnings: Vec<String>,
}

/// QA Agent Service configuration
#[derive(Debug, Clone)]
pub struct QaAgentConfig {
    pub scenarios_dir: PathBuf,
    pub reports_dir: PathBuf,
    pub create_github_issues: bool,
    pub github_repo: String,
    pub run_in_isolation: bool,
    pub max_parallel_tests: usize,
    pub ai_analysis_enabled: bool,
    pub chat_mode: ChatMode,
}

impl Default for QaAgentConfig {
    fn default() -> Self {
        let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
        Self {
            scenarios_dir: PathBuf::from(format!("{}/.osvm/qa/scenarios", home)),
            reports_dir: PathBuf::from(format!("{}/.osvm/qa/reports", home)),
            create_github_issues: false,
            github_repo: "opensvm/osvm-cli".to_string(),
            run_in_isolation: true,
            max_parallel_tests: 3,
            ai_analysis_enabled: true,
            chat_mode: ChatMode::default(),
        }
    }
}

/// Main QA Agent Service
pub struct QaAgentService {
    config: QaAgentConfig,
    ai_service: Arc<AiService>,
    mcp_service: Arc<Mutex<McpService>>,
    scenarios: HashMap<String, TestScenario>,
}

impl QaAgentService {
    /// Create a new QA Agent Service with simple parameters
    pub fn new(debug_mode: bool, verbose: u8) -> Self {
        let config = QaAgentConfig {
            ai_analysis_enabled: !debug_mode, // Disable AI in debug mode for faster testing
            ..Default::default()
        };

        // Create service or fall back to minimal config on error
        let ai_service = Arc::new(AiService::new());
        let mcp_service = Arc::new(Mutex::new(McpService::new()));

        // Ensure directories exist
        let _ = std::fs::create_dir_all(&config.scenarios_dir);
        let _ = std::fs::create_dir_all(&config.reports_dir);

        let mut service = Self {
            config,
            ai_service,
            mcp_service,
            scenarios: HashMap::new(),
        };

        // Load default scenarios
        let _ = service.load_default_scenarios();

        // Create example scenario files if needed
        let _ = service.create_example_scenarios();

        service
    }

    /// Create a new QA Agent Service with full config
    pub fn with_config(config: QaAgentConfig) -> Result<Self> {
        // Ensure directories exist
        std::fs::create_dir_all(&config.scenarios_dir)
            .context("Failed to create scenarios directory")?;
        std::fs::create_dir_all(&config.reports_dir)
            .context("Failed to create reports directory")?;

        let ai_service = Arc::new(AiService::new());
        let mcp_service = Arc::new(Mutex::new(McpService::new()));

        let mut service = Self {
            config,
            ai_service,
            mcp_service,
            scenarios: HashMap::new(),
        };

        // Load default scenarios
        service.load_default_scenarios()?;

        Ok(service)
    }

    /// Load default test scenarios
    fn load_default_scenarios(&mut self) -> Result<()> {
        // Basic command tests
        self.scenarios.insert(
            "basic_version".to_string(),
            TestScenario {
                id: "basic_version".to_string(),
                name: "Version Command".to_string(),
                category: TestCategory::Basic,
                description: "Test osvm --version command".to_string(),
                steps: vec![TestStep {
                    action: "What version of OSVM am I running?".to_string(),
                    expected_response: Some("version".to_string()),
                    timeout_seconds: 10,
                }],
                expected_behavior: "Should return version number".to_string(),
                timeout_seconds: 30,
            },
        );

        self.scenarios.insert(
            "basic_balance".to_string(),
            TestScenario {
                id: "basic_balance".to_string(),
                name: "Balance Query".to_string(),
                category: TestCategory::Basic,
                description: "Test balance query functionality".to_string(),
                steps: vec![TestStep {
                    action: "Check my SOL balance".to_string(),
                    expected_response: Some("balance".to_string()),
                    timeout_seconds: 15,
                }],
                expected_behavior: "Should query and return SOL balance".to_string(),
                timeout_seconds: 45,
            },
        );

        // RPC tests
        self.scenarios.insert(
            "rpc_query".to_string(),
            TestScenario {
                id: "rpc_query".to_string(),
                name: "RPC Query".to_string(),
                category: TestCategory::Rpc,
                description: "Test RPC query functionality".to_string(),
                steps: vec![TestStep {
                    action: "Query Solana network health".to_string(),
                    expected_response: Some("health".to_string()),
                    timeout_seconds: 20,
                }],
                expected_behavior: "Should query RPC and return network health".to_string(),
                timeout_seconds: 60,
            },
        );

        // Chat functionality tests
        self.scenarios.insert(
            "chat_basic".to_string(),
            TestScenario {
                id: "chat_basic".to_string(),
                name: "Basic Chat".to_string(),
                category: TestCategory::Chat,
                description: "Test basic chat interaction".to_string(),
                steps: vec![
                    TestStep {
                        action: "Hello, can you help me?".to_string(),
                        expected_response: Some("help".to_string()),
                        timeout_seconds: 10,
                    },
                    TestStep {
                        action: "What commands are available?".to_string(),
                        expected_response: Some("command".to_string()),
                        timeout_seconds: 10,
                    },
                ],
                expected_behavior: "Should respond to user queries".to_string(),
                timeout_seconds: 60,
            },
        );

        // OVSM tests
        self.scenarios.insert(
            "ovsm_eval".to_string(),
            TestScenario {
                id: "ovsm_eval".to_string(),
                name: "OVSM Eval".to_string(),
                category: TestCategory::Ovsm,
                description: "Test OVSM code evaluation".to_string(),
                steps: vec![TestStep {
                    action: "Evaluate this OVSM code: $x = 42; RETURN $x".to_string(),
                    expected_response: Some("42".to_string()),
                    timeout_seconds: 15,
                }],
                expected_behavior: "Should execute OVSM code and return result".to_string(),
                timeout_seconds: 45,
            },
        );

        info!("Loaded {} default test scenarios", self.scenarios.len());
        Ok(())
    }

    /// Run all test scenarios in a category
    pub async fn run_tests(&self, category: TestCategory) -> Result<Vec<TestResult>> {
        info!("Starting QA tests for category: {:?}", category);
        let start_time = std::time::Instant::now();

        // Filter scenarios by category
        let scenarios: Vec<&TestScenario> = self
            .scenarios
            .values()
            .filter(|s| category == TestCategory::All || s.category == category)
            .collect();

        info!("Running {} test scenarios", scenarios.len());

        let mut results = Vec::new();

        // Run tests (could be parallelized with max_parallel_tests)
        for scenario in scenarios {
            match self.run_single_test(scenario).await {
                Ok(result) => {
                    if !result.passed {
                        warn!(
                            "Test '{}' FAILED with {} bugs",
                            scenario.name,
                            result.bugs_found.len()
                        );
                    } else {
                        info!("Test '{}' PASSED", scenario.name);
                    }
                    results.push(result);
                }
                Err(e) => {
                    error!("Test '{}' errored: {}", scenario.name, e);
                    results.push(TestResult {
                        scenario_id: scenario.id.clone(),
                        passed: false,
                        duration_seconds: 0.0,
                        bugs_found: vec![],
                        warnings: vec![format!("Test execution error: {}", e)],
                    });
                }
            }
        }

        let duration = start_time.elapsed();
        info!(
            "QA tests completed in {:.2}s: {} passed, {} failed",
            duration.as_secs_f64(),
            results.iter().filter(|r| r.passed).count(),
            results.iter().filter(|r| !r.passed).count()
        );

        // Generate summary report
        self.generate_summary_report(&results, category).await?;

        // Create GitHub issues if requested
        if self.config.create_github_issues {
            self.create_github_issues_for_bugs(&results).await?;
        }

        Ok(results)
    }

    /// Run a single test scenario
    async fn run_single_test(&self, scenario: &TestScenario) -> Result<TestResult> {
        info!(
            "Running test: {} ({}) in {:?} mode",
            scenario.name, scenario.id, self.config.chat_mode
        );
        let start_time = std::time::Instant::now();

        let mut bugs_found = Vec::new();
        let mut warnings = Vec::new();
        let mut test_passed = true;

        // Execute test based on chat mode
        match self.config.chat_mode {
            ChatMode::Advanced => {
                // Create advanced chat state for this test
                let state = AdvancedChatState::new()?;
                state.initialize().await?;

                // Create a test session
                let session_name = format!("QA Test: {}", scenario.name);
                let session_id = state.create_session(session_name)?;

                // Execute each test step
                for (step_idx, step) in scenario.steps.iter().enumerate() {
                    info!("  Step {}: {}", step_idx + 1, step.action);

                    // Send user message
                    let user_msg = AdvancedChatMessage::User(step.action.clone());
                    state.add_message_to_session(session_id, user_msg)?;

                    // Wait for agent to process (with timeout)
                    let step_timeout = Duration::from_secs(step.timeout_seconds);
                    match timeout(
                        step_timeout,
                        self.wait_for_advanced_agent_response(&state, session_id),
                    )
                    .await
                    {
                        Ok(Ok(messages)) => {
                            // Check for errors in messages
                            if let Some(bug) = self
                                .detect_bugs_in_advanced_messages(&messages, scenario, step_idx)
                                .await?
                            {
                                bugs_found.push(bug);
                                test_passed = false;
                            }

                            // Validate expected response if specified
                            if let Some(expected) = &step.expected_response {
                                let has_expected = messages.iter().any(|msg| {
                                    if let AdvancedChatMessage::Agent(content) = msg {
                                        content.to_lowercase().contains(&expected.to_lowercase())
                                    } else {
                                        false
                                    }
                                });

                                if !has_expected {
                                    warnings.push(format!(
                                        "Step {}: Expected response containing '{}' not found",
                                        step_idx + 1,
                                        expected
                                    ));
                                }
                            }
                        }
                        Ok(Err(e)) => {
                            error!("  Step {} failed: {}", step_idx + 1, e);
                            test_passed = false;
                            warnings.push(format!("Step {}: {}", step_idx + 1, e));
                        }
                        Err(_) => {
                            error!(
                                "  Step {} timed out after {}s",
                                step_idx + 1,
                                step.timeout_seconds
                            );
                            test_passed = false;
                            warnings.push(format!("Step {} timed out", step_idx + 1));
                        }
                    }
                }
            }
            ChatMode::Basic => {
                // Create basic chat state for this test
                let state = ProgrammaticChatState::new().await?;
                state.initialize().await?;

                // Execute each test step
                for (step_idx, step) in scenario.steps.iter().enumerate() {
                    info!("  Step {}: {}", step_idx + 1, step.action);

                    // Send user message
                    state.send_message(step.action.clone()).await?;

                    // Wait for agent to process (with timeout)
                    let step_timeout = Duration::from_secs(step.timeout_seconds);
                    match timeout(step_timeout, state.wait_for_idle(step.timeout_seconds)).await {
                        Ok(Ok(())) => {
                            // Get recent messages
                            let messages = state.get_recent_messages(10).await;

                            // Check for errors in messages
                            if let Some(bug) = self
                                .detect_bugs_in_basic_messages(&messages, scenario, step_idx)
                                .await?
                            {
                                bugs_found.push(bug);
                                test_passed = false;
                            }

                            // Validate expected response if specified
                            if let Some(expected) = &step.expected_response {
                                let has_expected = messages.iter().any(|msg| {
                                    if let BasicChatMessage::Agent(content) = msg {
                                        content.to_lowercase().contains(&expected.to_lowercase())
                                    } else {
                                        false
                                    }
                                });

                                if !has_expected {
                                    warnings.push(format!(
                                        "Step {}: Expected response containing '{}' not found",
                                        step_idx + 1,
                                        expected
                                    ));
                                }
                            }
                        }
                        Ok(Err(e)) => {
                            error!("  Step {} failed: {}", step_idx + 1, e);
                            test_passed = false;
                            warnings.push(format!("Step {}: {}", step_idx + 1, e));
                        }
                        Err(_) => {
                            error!(
                                "  Step {} timed out after {}s",
                                step_idx + 1,
                                step.timeout_seconds
                            );
                            test_passed = false;
                            warnings.push(format!("Step {} timed out", step_idx + 1));
                        }
                    }
                }
            }
        }

        let duration = start_time.elapsed();

        Ok(TestResult {
            scenario_id: scenario.id.clone(),
            passed: test_passed && bugs_found.is_empty(),
            duration_seconds: duration.as_secs_f64(),
            bugs_found,
            warnings,
        })
    }

    /// Wait for advanced agent to respond to user message
    async fn wait_for_advanced_agent_response(
        &self,
        state: &AdvancedChatState,
        session_id: Uuid,
    ) -> Result<Vec<AdvancedChatMessage>> {
        // Poll for agent state to become Idle (indicating completion)
        let mut attempts = 0;
        const MAX_ATTEMPTS: u32 = 100;

        while attempts < MAX_ATTEMPTS {
            tokio::time::sleep(Duration::from_millis(500)).await;
            attempts += 1;

            let agent_state = state
                .get_agent_state(session_id)
                .ok_or_else(|| anyhow::anyhow!("Session not found"))?;

            match agent_state {
                AdvancedAgentState::Idle => {
                    // Agent finished processing - get messages from session
                    let session = state
                        .get_session_by_id(session_id)
                        .ok_or_else(|| anyhow::anyhow!("Session not found"))?;
                    // Return last 10 messages
                    let messages = session
                        .messages
                        .iter()
                        .rev()
                        .take(10)
                        .cloned()
                        .collect::<Vec<_>>();
                    return Ok(messages.into_iter().rev().collect());
                }
                AdvancedAgentState::Error(ref err) => {
                    return Err(anyhow::anyhow!("Agent error: {}", err));
                }
                _ => {
                    // Still processing (Thinking, Planning, ExecutingTool, etc.)
                    continue;
                }
            }
        }

        Err(anyhow::anyhow!("Agent did not respond within timeout"))
    }

    /// Detect bugs in advanced chat messages
    async fn detect_bugs_in_advanced_messages(
        &self,
        messages: &[AdvancedChatMessage],
        scenario: &TestScenario,
        step_idx: usize,
    ) -> Result<Option<BugReport>> {
        // Look for error messages
        let mut error_messages = Vec::new();
        let mut has_critical_error = false;

        for msg in messages {
            match msg {
                AdvancedChatMessage::Error(err) => {
                    error_messages.push(err.clone());
                    has_critical_error = true;
                }
                AdvancedChatMessage::Agent(content) => {
                    // Detect error patterns in agent responses
                    if content.to_lowercase().contains("error:")
                        || content.to_lowercase().contains("failed:")
                        || content.to_lowercase().contains("panic")
                        || content.to_lowercase().contains("crash")
                    {
                        error_messages.push(content.clone());
                    }
                }
                AdvancedChatMessage::System(content) => {
                    if content.to_lowercase().contains("error") {
                        error_messages.push(content.clone());
                    }
                }
                _ => {}
            }
        }

        if error_messages.is_empty() {
            return Ok(None);
        }

        // Create bug report
        let severity = if has_critical_error {
            BugSeverity::High
        } else {
            BugSeverity::Medium
        };

        let title = format!("[QA] Bug found in {}: Step {}", scenario.name, step_idx + 1);

        let description = format!(
            "Automated QA testing detected an issue in test scenario '{}'.\n\n\
            **Test Category:** {:?}\n\
            **Test Step:** {}/{}\n\
            **Test Description:** {}\n\n\
            **Error Messages:**\n{}",
            scenario.name,
            scenario.category,
            step_idx + 1,
            scenario.steps.len(),
            scenario.description,
            error_messages.join("\n")
        );

        let mut bug_report = BugReport {
            id: Uuid::new_v4(),
            detected_at: Utc::now(),
            test_scenario: scenario.name.clone(),
            category: scenario.category,
            severity,
            title: title.clone(),
            description: description.clone(),
            reproduction_steps: scenario.steps.iter().map(|s| s.action.clone()).collect(),
            actual_behavior: error_messages.join("; "),
            expected_behavior: scenario.expected_behavior.clone(),
            error_messages: error_messages.clone(),
            chat_transcript: messages.iter().map(|m| format!("{:?}", m)).collect(),
            ai_analysis: None,
            suggested_fix: None,
            github_issue_url: None,
        };

        // Use AI to analyze the bug and suggest fixes
        if self.config.ai_analysis_enabled {
            match self.analyze_bug_with_ai(&bug_report).await {
                Ok((analysis, fix)) => {
                    bug_report.ai_analysis = Some(analysis);
                    bug_report.suggested_fix = Some(fix);
                }
                Err(e) => {
                    warn!("AI analysis failed: {}", e);
                }
            }
        }

        info!("Bug detected: {}", title);
        Ok(Some(bug_report))
    }

    /// Detect bugs in basic chat messages
    async fn detect_bugs_in_basic_messages(
        &self,
        messages: &[BasicChatMessage],
        scenario: &TestScenario,
        step_idx: usize,
    ) -> Result<Option<BugReport>> {
        // Look for error messages
        let mut error_messages = Vec::new();
        let mut has_critical_error = false;

        for msg in messages {
            if msg.is_error() {
                error_messages.push(msg.content());
                has_critical_error = true;
            } else {
                let content = msg.content();
                // Detect error patterns in responses
                if content.to_lowercase().contains("error:")
                    || content.to_lowercase().contains("failed:")
                    || content.to_lowercase().contains("panic")
                    || content.to_lowercase().contains("crash")
                {
                    error_messages.push(content);
                }
            }
        }

        if error_messages.is_empty() {
            return Ok(None);
        }

        // Create bug report
        let severity = if has_critical_error {
            BugSeverity::High
        } else {
            BugSeverity::Medium
        };

        let title = format!("[QA] Bug found in {}: Step {}", scenario.name, step_idx + 1);

        let description = format!(
            "Automated QA testing detected an issue in test scenario '{}'.\n\n\
            **Test Category:** {:?}\n\
            **Test Step:** {}/{}\n\
            **Test Description:** {}\n\n\
            **Error Messages:**\n{}",
            scenario.name,
            scenario.category,
            step_idx + 1,
            scenario.steps.len(),
            scenario.description,
            error_messages.join("\n")
        );

        let mut bug_report = BugReport {
            id: Uuid::new_v4(),
            detected_at: Utc::now(),
            test_scenario: scenario.name.clone(),
            category: scenario.category,
            severity,
            title: title.clone(),
            description: description.clone(),
            reproduction_steps: scenario.steps.iter().map(|s| s.action.clone()).collect(),
            actual_behavior: error_messages.join("; "),
            expected_behavior: scenario.expected_behavior.clone(),
            error_messages: error_messages.clone(),
            chat_transcript: vec![], // Basic chat messages don't map directly to AdvancedChatMessage
            ai_analysis: None,
            suggested_fix: None,
            github_issue_url: None,
        };

        // Use AI to analyze the bug and suggest fixes
        if self.config.ai_analysis_enabled {
            match self.analyze_bug_with_ai(&bug_report).await {
                Ok((analysis, fix)) => {
                    bug_report.ai_analysis = Some(analysis);
                    bug_report.suggested_fix = Some(fix);
                }
                Err(e) => {
                    warn!("AI analysis failed: {}", e);
                }
            }
        }

        info!("Bug detected: {}", title);
        Ok(Some(bug_report))
    }

    /// Use AI to analyze bug and suggest fixes
    async fn analyze_bug_with_ai(&self, bug: &BugReport) -> Result<(String, String)> {
        let prompt = format!(
            r#"You are a QA engineer analyzing a bug found in the OSVM CLI tool.

Test Scenario: {}
Category: {:?}
Severity: {:?}

Error Messages:
{}

Expected Behavior: {}
Actual Behavior: {}

Reproduction Steps:
{}

Please provide:
1. A detailed analysis of what likely went wrong
2. A specific suggestion for how to fix this bug (be concrete - mention file names, function names, etc.)

Format your response as:
## Analysis
[Your analysis here]

## Suggested Fix
[Your specific fix suggestion here]
"#,
            bug.test_scenario,
            bug.category,
            bug.severity,
            bug.error_messages.join("\n"),
            bug.expected_behavior,
            bug.actual_behavior,
            bug.reproduction_steps
                .iter()
                .enumerate()
                .map(|(i, step)| format!("{}. {}", i + 1, step))
                .collect::<Vec<_>>()
                .join("\n")
        );

        let response = self.ai_service.query(&prompt).await?;

        // Parse response to extract analysis and fix
        let parts: Vec<&str> = response.split("## Suggested Fix").collect();
        let analysis = parts
            .first()
            .unwrap_or(&"")
            .replace("## Analysis", "")
            .trim()
            .to_string();
        let fix = parts.get(1).unwrap_or(&"").trim().to_string();

        Ok((analysis, fix))
    }

    /// Generate summary report for test results
    pub async fn generate_summary_report(
        &self,
        results: &[TestResult],
        category: TestCategory,
    ) -> Result<()> {
        let timestamp = Utc::now().format("%Y%m%d_%H%M%S");
        let report_path = self
            .config
            .reports_dir
            .join(format!("qa_report_{:?}_{}.md", category, timestamp));

        let mut report = String::new();
        report.push_str(&format!("# QA Test Report - {:?}\n\n", category));
        report.push_str(&format!("**Generated:** {}\n\n", Utc::now()));
        report.push_str(&format!("**Total Tests:** {}\n", results.len()));
        report.push_str(&format!(
            "**Passed:** {}\n",
            results.iter().filter(|r| r.passed).count()
        ));
        report.push_str(&format!(
            "**Failed:** {}\n",
            results.iter().filter(|r| !r.passed).count()
        ));
        report.push_str(&format!(
            "**Total Bugs Found:** {}\n\n",
            results.iter().map(|r| r.bugs_found.len()).sum::<usize>()
        ));

        report.push_str("## Test Results\n\n");
        for result in results {
            let status = if result.passed {
                "✅ PASS"
            } else {
                "❌ FAIL"
            };
            report.push_str(&format!(
                "### {} - {} ({:.2}s)\n",
                status, result.scenario_id, result.duration_seconds
            ));

            if !result.bugs_found.is_empty() {
                report.push_str("\n**Bugs Found:**\n");
                for bug in &result.bugs_found {
                    report.push_str(&format!("- {:?}: {}\n", bug.severity, bug.title));
                }
            }

            if !result.warnings.is_empty() {
                report.push_str("\n**Warnings:**\n");
                for warning in &result.warnings {
                    report.push_str(&format!("- {}\n", warning));
                }
            }
            report.push('\n');
        }

        // Add bug details
        report.push_str("## Bug Details\n\n");
        for result in results {
            for bug in &result.bugs_found {
                report.push_str(&format!("### {}\n\n", bug.title));
                report.push_str(&format!("**ID:** {}\n", bug.id));
                report.push_str(&format!("**Severity:** {:?}\n", bug.severity));
                report.push_str(&format!("**Detected:** {}\n\n", bug.detected_at));
                report.push_str(&format!("{}\n\n", bug.description));

                if let Some(analysis) = &bug.ai_analysis {
                    report.push_str("**AI Analysis:**\n");
                    report.push_str(&format!("{}\n\n", analysis));
                }

                if let Some(fix) = &bug.suggested_fix {
                    report.push_str("**Suggested Fix:**\n");
                    report.push_str(&format!("{}\n\n", fix));
                }

                report.push_str("---\n\n");
            }
        }

        std::fs::write(&report_path, report)?;
        info!("Summary report written to: {}", report_path.display());

        Ok(())
    }

    /// Create GitHub issues for all bugs found
    async fn create_github_issues_for_bugs(&self, results: &[TestResult]) -> Result<()> {
        info!("Creating GitHub issues for bugs...");

        for result in results {
            for bug in &result.bugs_found {
                match self.create_github_issue(bug).await {
                    Ok(url) => {
                        info!("Created GitHub issue: {}", url);
                    }
                    Err(e) => {
                        error!("Failed to create GitHub issue for bug {}: {}", bug.id, e);
                    }
                }
            }
        }

        Ok(())
    }

    /// Create a GitHub issue for a bug
    pub async fn create_github_issue(&self, bug: &BugReport) -> Result<String> {
        // Build issue body
        let mut body = String::new();
        body.push_str("## Bug Report\n\n");
        body.push_str(&format!("**Severity:** {:?}\n", bug.severity));
        body.push_str(&format!("**Test Scenario:** {}\n", bug.test_scenario));
        body.push_str(&format!("**Category:** {:?}\n", bug.category));
        body.push_str(&format!("**Detected:** {}\n\n", bug.detected_at));

        body.push_str(&format!("{}\n\n", bug.description));

        body.push_str("## Reproduction Steps\n\n");
        for (i, step) in bug.reproduction_steps.iter().enumerate() {
            body.push_str(&format!("{}. {}\n", i + 1, step));
        }
        body.push('\n');

        body.push_str(&format!(
            "## Expected Behavior\n{}\n\n",
            bug.expected_behavior
        ));
        body.push_str(&format!("## Actual Behavior\n{}\n\n", bug.actual_behavior));

        if !bug.error_messages.is_empty() {
            body.push_str("## Error Messages\n```\n");
            for err in &bug.error_messages {
                body.push_str(&format!("{}\n", err));
            }
            body.push_str("```\n\n");
        }

        if let Some(analysis) = &bug.ai_analysis {
            body.push_str("## AI Analysis\n");
            body.push_str(&format!("{}\n\n", analysis));
        }

        if let Some(fix) = &bug.suggested_fix {
            body.push_str("## Suggested Fix\n");
            body.push_str(&format!("{}\n\n", fix));
        }

        body.push_str(&format!(
            "\n---\n*Automatically generated by QA Agent (Bug ID: {})*\n",
            bug.id
        ));

        // Add labels based on severity and category
        let labels = format!(
            "bug,qa-automated,severity:{:?},category:{:?}",
            bug.severity, bug.category
        )
        .to_lowercase();

        // Use gh CLI to create issue
        let output = tokio::process::Command::new("gh")
            .arg("issue")
            .arg("create")
            .arg("--repo")
            .arg(&self.config.github_repo)
            .arg("--title")
            .arg(&bug.title)
            .arg("--body")
            .arg(&body)
            .arg("--label")
            .arg(&labels)
            .output()
            .await?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow::anyhow!("Failed to create GitHub issue: {}", stderr));
        }

        // Extract issue URL from output
        let stdout = String::from_utf8_lossy(&output.stdout);
        let issue_url = stdout.trim().to_string();

        Ok(issue_url)
    }

    /// Run interactive QA session
    pub async fn run_interactive(&self) -> Result<()> {
        println!("Starting interactive QA session...");
        println!("You can interact with the advanced chat and manually test features.");
        println!("Press Ctrl+C to exit.\n");

        // Run the advanced chat interface
        crate::utils::agent_chat_v2::run_advanced_agent_chat().await
    }

    /// List all available test scenarios
    pub fn list_scenarios(&self) {
        println!("\n📋 Available Test Scenarios:\n");

        // Group scenarios by category
        let mut categories: HashMap<TestCategory, Vec<&TestScenario>> = HashMap::new();
        for scenario in self.scenarios.values() {
            categories
                .entry(scenario.category)
                .or_default()
                .push(scenario);
        }

        // Print scenarios by category
        for (category, scenarios) in categories.iter() {
            println!("╔═══════════════════════════════════════════════════╗");
            println!("║ {:?} Tests ({} scenarios)", category, scenarios.len());
            println!("╠═══════════════════════════════════════════════════╣");

            for scenario in scenarios {
                println!("║ • {} ({})", scenario.name, scenario.id);
                println!("║   {}", scenario.description);
                println!(
                    "║   Timeout: {}s, Steps: {}",
                    scenario.timeout_seconds,
                    scenario.steps.len()
                );
                println!("╠───────────────────────────────────────────────────╣");
            }
            println!("╚═══════════════════════════════════════════════════╝\n");
        }

        println!("Total scenarios: {}", self.scenarios.len());
    }

    /// Create example scenario files using AI to generate intelligent test cases
    fn create_example_scenarios(&self) -> Result<()> {
        // Only generate if scenarios directory is empty or has less than 2 files
        let existing_files = std::fs::read_dir(&self.config.scenarios_dir)
            .ok()
            .map(|entries| entries.filter_map(|e| e.ok()).count())
            .unwrap_or(0);

        if existing_files >= 2 {
            return Ok(()); // Already has scenario files
        }

        info!("Generating AI-powered test scenarios...");

        // Create static examples as fallback (if AI is disabled or fails)
        self.create_static_examples()?;

        // Note: AI-generated test scenarios planned for v2.0 release
        // This would use ai_service.query() to generate comprehensive test scenarios
        // based on analyzing OSVM CLI structure and available commands

        Ok(())
    }

    /// Create static example scenarios as fallback
    fn create_static_examples(&self) -> Result<()> {
        // Create example basic test
        let example_basic = self.config.scenarios_dir.join("example_basic_test.json");
        if !example_basic.exists() {
            let scenario = serde_json::json!({
                "id": "basic_custom_test",
                "name": "Custom Basic Test",
                "category": "Basic",
                "description": "Example custom test scenario for basic commands",
                "steps": [
                    {
                        "action": "Show me the OSVM version",
                        "expected_response": "version",
                        "timeout_seconds": 10
                    },
                    {
                        "action": "What commands are available?",
                        "expected_response": "command",
                        "timeout_seconds": 10
                    }
                ],
                "expected_behavior": "Should respond to basic queries about the system",
                "timeout_seconds": 30
            });

            let json_str = serde_json::to_string_pretty(&scenario)?;
            std::fs::write(&example_basic, json_str)?;
            info!("Created example scenario: {}", example_basic.display());
        }

        // Create example RPC test
        let example_rpc = self.config.scenarios_dir.join("example_rpc_test.json");
        if !example_rpc.exists() {
            let scenario = serde_json::json!({
                "id": "rpc_custom_test",
                "name": "Custom RPC Test",
                "category": "Rpc",
                "description": "Example custom test for RPC operations",
                "steps": [
                    {
                        "action": "Query the Solana network health",
                        "expected_response": "health",
                        "timeout_seconds": 20
                    },
                    {
                        "action": "Check the current slot",
                        "expected_response": "slot",
                        "timeout_seconds": 20
                    }
                ],
                "expected_behavior": "Should successfully query RPC endpoints and return network information",
                "timeout_seconds": 60
            });

            let json_str = serde_json::to_string_pretty(&scenario)?;
            std::fs::write(&example_rpc, json_str)?;
            info!("Created example scenario: {}", example_rpc.display());
        }

        // Create README file
        let readme = self.config.scenarios_dir.join("README.md");
        if !readme.exists() {
            let content = "# QA Test Scenarios

This directory contains custom test scenarios for OSVM's automated QA system.

## Creating Test Scenarios

Create JSON files with this structure:

```json
{
  \"id\": \"unique_id\",
  \"name\": \"Test Name\",
  \"category\": \"Basic|Rpc|Svm|Nodes\",
  \"description\": \"What this tests\",
  \"steps\": [
    {
      \"action\": \"User message to send\",
      \"expected_response\": \"keyword to find\",
      \"timeout_seconds\": 10
    }
  ],
  \"expected_behavior\": \"Expected outcome\",
  \"timeout_seconds\": 30
}
```

## Running Tests

```bash
osvm qa run --category all
osvm qa list  # List all scenarios
```
";
            std::fs::write(&readme, content)?;
            info!("Created README: {}", readme.display());
        }

        Ok(())
    }

    /// Show recent QA reports
    pub async fn show_reports(&self) -> Result<()> {
        println!("\n📊 Recent QA Reports:\n");

        // List all markdown files in reports directory
        let reports_dir = &self.config.reports_dir;
        if !reports_dir.exists() {
            println!("No reports directory found at: {}", reports_dir.display());
            return Ok(());
        }

        let mut reports = Vec::new();
        for entry in std::fs::read_dir(reports_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("md") {
                if let Ok(metadata) = entry.metadata() {
                    if let Ok(modified) = metadata.modified() {
                        reports.push((path, modified));
                    }
                }
            }
        }

        // Sort by modification time (newest first)
        reports.sort_by(|a, b| b.1.cmp(&a.1));

        if reports.is_empty() {
            println!("No reports found in {}", reports_dir.display());
            return Ok(());
        }

        println!("╔═══════════════════════════════════════════════════════════════╗");
        println!("║ Recent Reports ({})", reports.len());
        println!("╠═══════════════════════════════════════════════════════════════╣");

        for (i, (path, modified)) in reports.iter().take(10).enumerate() {
            let filename = path
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown");
            let modified_time: DateTime<Utc> = (*modified).into();
            let time_ago = Utc::now().signed_duration_since(modified_time);

            let time_str = if time_ago.num_days() > 0 {
                format!("{} days ago", time_ago.num_days())
            } else if time_ago.num_hours() > 0 {
                format!("{} hours ago", time_ago.num_hours())
            } else if time_ago.num_minutes() > 0 {
                format!("{} minutes ago", time_ago.num_minutes())
            } else {
                "just now".to_string()
            };

            println!("║ {}. {}", i + 1, filename);
            println!(
                "║    Created: {} ({})",
                modified_time.format("%Y-%m-%d %H:%M:%S"),
                time_str
            );
            println!("║    Path: {}", path.display());

            if i < reports.len() - 1 {
                println!("╠───────────────────────────────────────────────────────────────╣");
            }
        }

        println!("╚═══════════════════════════════════════════════════════════════╝");

        if reports.len() > 10 {
            println!("\n... and {} more reports", reports.len() - 10);
        }

        println!(
            "\nView full report with: cat {}/[report-name].md",
            reports_dir.display()
        );

        Ok(())
    }
}
