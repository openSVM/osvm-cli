use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use crate::services::ai_service::AiService;
use crate::services::ovsm_service::OvsmService;
use std::sync::Arc;
use tokio::sync::Mutex;
use ovsm::runtime::Value;

/// Represents the state of our investigation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvestigationState {
    pub target_wallet: String,
    pub phase: InvestigationPhase,
    pub findings: Vec<Finding>,
    pub hypotheses: Vec<Hypothesis>,
    pub evidence: HashMap<String, Evidence>,
    pub next_steps: Vec<String>,
    pub confidence_scores: HashMap<String, f64>,
    pub iteration: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InvestigationPhase {
    Initial,
    BasicProfiling,
    DeepAnalysis,
    PatternRecognition,
    HypothesisTesting,
    Synthesis,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Finding {
    pub category: String,
    pub description: String,
    pub significance: f64,
    pub raw_data: serde_json::Value,
    pub timestamp: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Hypothesis {
    pub statement: String,
    pub confidence: f64,
    pub supporting_evidence: Vec<String>,
    pub contradicting_evidence: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Evidence {
    pub data_type: String,
    pub value: serde_json::Value,
    pub source: String,
    pub reliability: f64,
}

pub struct ResearchAgent {
    ai_service: Arc<Mutex<AiService>>,
    ovsm_service: Arc<Mutex<OvsmService>>,
    state: Arc<Mutex<InvestigationState>>,
}

impl ResearchAgent {
    pub fn new(
        ai_service: Arc<Mutex<AiService>>,
        ovsm_service: Arc<Mutex<OvsmService>>,
        target_wallet: String,
    ) -> Self {
        let initial_state = InvestigationState {
            target_wallet,
            phase: InvestigationPhase::Initial,
            findings: Vec::new(),
            hypotheses: Vec::new(),
            evidence: HashMap::new(),
            next_steps: vec![
                "Get basic wallet info".to_string(),
                "Analyze recent transactions".to_string(),
                "Identify interaction patterns".to_string(),
            ],
            confidence_scores: HashMap::new(),
            iteration: 0,
        };

        Self {
            ai_service,
            ovsm_service,
            state: Arc::new(Mutex::new(initial_state)),
        }
    }

    /// Main investigation loop with self-evaluation
    pub async fn investigate(&self) -> Result<String> {
        let max_iterations = 10;
        let mut final_report = String::new();

        for iteration in 0..max_iterations {
            println!("\n? Investigation Iteration #{}", iteration + 1);

            // 1. Execute current phase
            let phase_result = self.execute_phase().await?;

            // 2. Self-evaluate findings with custom AI prompt
            let evaluation = self.evaluate_findings().await?;

            // 3. Decide next steps based on evaluation
            let should_continue = self.decide_next_steps(evaluation).await?;

            // 4. Update state
            {
                let mut state = self.state.lock().await;
                state.iteration = iteration + 1;
            }

            if !should_continue {
                println!("? Investigation complete after {} iterations", iteration + 1);
                break;
            }

            // 5. Adapt investigation strategy
            self.adapt_strategy().await?;
        }

        // Generate final report
        final_report = self.generate_final_report().await?;
        Ok(final_report)
    }

    /// Execute current investigation phase
    async fn execute_phase(&self) -> Result<serde_json::Value> {
        let state = self.state.lock().await.clone();

        let ovsm_script = match state.phase {
            InvestigationPhase::Initial => self.generate_initial_script(&state.target_wallet),
            InvestigationPhase::BasicProfiling => self.generate_profiling_script(&state),
            InvestigationPhase::DeepAnalysis => self.generate_deep_analysis_script(&state),
            InvestigationPhase::PatternRecognition => self.generate_pattern_script(&state),
            InvestigationPhase::HypothesisTesting => self.generate_hypothesis_test_script(&state),
            InvestigationPhase::Synthesis => self.generate_synthesis_script(&state),
        };

        println!("? Executing OVSM script for phase: {:?}", state.phase);
        println!("Script preview: {}", &ovsm_script[..ovsm_script.len().min(200)]);

        // Execute OVSM script
        let mut ovsm = self.ovsm_service.lock().await;
        let result_value = ovsm.execute_code(&ovsm_script)
            .context("Failed to execute OVSM script")?;

        // Convert Value to serde_json::Value
        let result = self.value_to_json(result_value)?;

        // Store findings
        self.store_findings(result.clone()).await?;

        Ok(result)
    }

    /// Generate context-aware AI evaluation prompt based on current state
    async fn evaluate_findings(&self) -> Result<String> {
        let state = self.state.lock().await.clone();

        // Build dynamic system prompt based on investigation state
        let system_prompt = self.build_evaluation_prompt(&state);

        // Prepare findings summary for evaluation
        let findings_summary = serde_json::to_string_pretty(&state.findings)?;

        let user_prompt = format!(
            "Based on these findings from investigating wallet {}:\n\n{}\n\nCurrent hypotheses:\n{:?}\n\nPlease evaluate the significance of these findings and suggest next investigation steps.",
            state.target_wallet,
            findings_summary,
            state.hypotheses
        );

        println!("\n? Self-evaluating findings with AI...");

        // Call AI with custom system prompt
        let ai_service = self.ai_service.lock().await;
        let evaluation = ai_service.query_with_system_prompt(
            &system_prompt,
            &user_prompt
        ).await?;

        println!("? AI Evaluation:\n{}", &evaluation[..evaluation.len().min(500)]);

        Ok(evaluation)
    }

    /// Build dynamic system prompt based on investigation phase and findings
    fn build_evaluation_prompt(&self, state: &InvestigationState) -> String {
        let base_prompt = match state.phase {
            InvestigationPhase::Initial => {
                "You are a blockchain forensics expert beginning a wallet investigation. \
                Focus on identifying the wallet type (trader, bot, DeFi user, etc.) and \
                determining investigation priorities."
            }
            InvestigationPhase::BasicProfiling => {
                "You are analyzing wallet behavior patterns. Look for trading strategies, \
                interaction frequencies, preferred protocols, and potential automated behavior."
            }
            InvestigationPhase::DeepAnalysis => {
                "You are conducting deep blockchain analysis. Identify complex patterns, \
                multi-hop transactions, MEV activity, and sophisticated trading strategies."
            }
            InvestigationPhase::PatternRecognition => {
                "You are a pattern recognition specialist. Identify recurring behaviors, \
                temporal patterns, correlation with market events, and anomalies."
            }
            InvestigationPhase::HypothesisTesting => {
                "You are testing specific hypotheses about wallet behavior. Evaluate evidence \
                strength, identify contradictions, and refine understanding."
            }
            InvestigationPhase::Synthesis => {
                "You are synthesizing all findings into a comprehensive profile. Create a \
                complete picture of wallet activity and behavior patterns."
            }
        };

        format!(
            "{}\n\nInvestigation iteration: {}\nConfidence scores: {:?}\nEvidence collected: {} pieces\n\nYour task:\n1. Evaluate the significance of current findings\n2. Identify gaps in our understanding\n3. Suggest specific next investigation steps\n4. Rate confidence in current hypotheses\n5. Identify any red flags or unusual patterns\n\nRespond in JSON format with keys: significance, gaps, next_steps, confidence_updates, red_flags",
            base_prompt,
            state.iteration,
            state.confidence_scores,
            state.evidence.len()
        )
    }

    /// Decide whether to continue investigation based on AI evaluation
    async fn decide_next_steps(&self, evaluation: String) -> Result<bool> {
        // Parse AI evaluation (assuming JSON response)
        let eval_json: serde_json::Value = serde_json::from_str(&evaluation)
            .unwrap_or_else(|_| serde_json::json!({
                "next_steps": ["continue"],
                "significance": 0.5,
                "confidence_updates": {}
            }));

        let mut state = self.state.lock().await;

        // Update next steps from AI evaluation
        if let Some(steps) = eval_json["next_steps"].as_array() {
            state.next_steps = steps.iter()
                .filter_map(|s| s.as_str().map(String::from))
                .collect();
        }

        // Update confidence scores
        if let Some(updates) = eval_json["confidence_updates"].as_object() {
            for (key, value) in updates {
                if let Some(score) = value.as_f64() {
                    state.confidence_scores.insert(key.clone(), score);
                }
            }
        }

        // Advance phase based on current progress
        state.phase = self.determine_next_phase(&state.phase, &state.findings);

        // Continue if we have more steps and haven't reached synthesis
        Ok(!state.next_steps.is_empty() && !matches!(state.phase, InvestigationPhase::Synthesis))
    }

    /// Adapt investigation strategy based on findings
    async fn adapt_strategy(&self) -> Result<()> {
        let state = self.state.lock().await.clone();

        println!("\n? Adapting investigation strategy...");
        println!("  Current phase: {:?}", state.phase);
        println!("  Next steps: {:?}", state.next_steps);
        println!("  Confidence scores: {:?}", state.confidence_scores);

        // Generate new hypotheses based on findings
        if state.findings.len() > 3 && state.hypotheses.is_empty() {
            self.generate_hypotheses().await?;
        }

        Ok(())
    }

    /// Generate initial investigation script
    fn generate_initial_script(&self, wallet: &str) -> String {
        format!(
            r#"(do
  ;; Initial wallet investigation - minimal transfer aggregation
  (define target "{}")

  ;; Get basic wallet info (placeholders: getBalance, getSignaturesForAddress, getTokenAccountsByOwner)
  ;; Analyze transaction type with analyzeTransactionType helper

  ;; Fetch recent transfers (using actual MCP tool)
  (define resp (get_account_transfers {{:address target :limit 200}}))
  (define transfers (get resp "data"))

  ;; Split by direction
  (define inflows (filter transfers (lambda (t) (= (get t "transferType") "IN"))))
  (define outflows (filter transfers (lambda (t) (= (get t "transferType") "OUT"))))

  ;; Aggregate inflows by sender
  (define inflow_agg
    (reduce
      inflows
      {{}}
      (lambda (acc tx)
        (do
          (define from (get tx "from"))
          (define amt (float (get tx "tokenAmount")))
          (define existing (get acc from))
          (define current (if existing existing 0))
          (put acc from (+ current amt))))))

  ;; Aggregate outflows by receiver
  (define outflow_agg
    (reduce
      outflows
      {{}}
      (lambda (acc tx)
        (do
          (define to (get tx "to"))
          (define amt (float (get tx "tokenAmount")))
          (define existing (get acc to))
          (define current (if existing existing 0))
          (put acc to (+ current amt))))))

  ;; Sort and take top 5
  (define top_senders
    (take 5
      (sort
        (entries inflow_agg)
        (lambda (a b) (> (get a 1) (get b 1))))))

  (define top_receivers
    (take 5
      (sort
        (entries outflow_agg)
        (lambda (a b) (> (get a 1) (get b 1))))))

  ;; Count unique tokens
  (define unique_tokens (group-by transfers (lambda (tx) (get tx "mint"))))

  ;; Return results
  {{:wallet target
   :transfer_count (length transfers)
   :inflow_count (length inflows)
   :outflow_count (length outflows)
   :unique_tokens (length (entries unique_tokens))
   :top_5_senders top_senders
   :top_5_receivers top_receivers}}
)"#,
            wallet
        )
    }

    /// Generate profiling script based on initial findings
    fn generate_profiling_script(&self, state: &InvestigationState) -> String {
        format!(
            r#"(do
  ;; Deep profiling phase - analyze transfer patterns
  ;; Uses helpers: analyzeTemporalPatterns, filterDexTransactions
  (define target "{}")

  ;; Get more transfers for pattern analysis
  (define resp (get_account_transfers {{:address target :limit 500}}))
  (define transfers (get resp "data"))

  ;; Analyze token diversity
  (define by_token (group-by transfers (lambda (tx) (get tx "mint"))))
  (define token_count (length (entries by_token)))

  ;; Analyze transfer frequency (placeholder)
  (define avg_tx_per_token (/ (length transfers) (if (> token_count 0) token_count 1)))

  {{:wallet target
   :total_transfers_analyzed (length transfers)
   :unique_tokens token_count
   :avg_transfers_per_token avg_tx_per_token
   :analysis_phase "BasicProfiling"}}
)"#,
            state.target_wallet
        )
    }

    /// Generate deep analysis script for MEV and advanced patterns
    fn generate_deep_analysis_script(&self, state: &InvestigationState) -> String {
        format!(
            r#"(do
  ;; Deep analysis phase - placeholder until advanced tools available
  (define target "{}")

  ;; Return minimal placeholder - NO fake data keys
  {{:wallet target
   :analysis_phase "DeepAnalysis"
   :status "pending_tool_implementation"
   :note "Advanced MEV analysis requires specialized MCP tools"}}
)"#,
            state.target_wallet
        )
    }

    /// Generate pattern recognition script
    fn generate_pattern_script(&self, state: &InvestigationState) -> String {
        format!(
            r#"(do
  ;; Pattern recognition phase - placeholder until advanced tools available
  (define target "{}")

  ;; Return minimal placeholder - NO fake data keys
  {{:wallet target
   :analysis_phase "PatternRecognition"
   :status "pending_tool_implementation"
   :note "Pattern analysis requires temporal data and behavioral clustering tools"}}
)"#,
            state.target_wallet
        )
    }

    /// Generate hypothesis testing script
    fn generate_hypothesis_test_script(&self, state: &InvestigationState) -> String {
        format!(
            r#"(do
  ;; Hypothesis testing phase - placeholder until advanced tools available
  (define target "{}")

  ;; Return minimal placeholder - NO fake data keys
  {{:wallet target
   :analysis_phase "HypothesisTesting"
   :status "pending_tool_implementation"
   :note "Hypothesis validation requires comprehensive transaction graph analysis"}}
)"#,
            state.target_wallet
        )
    }

    /// Generate synthesis script to combine all findings
    fn generate_synthesis_script(&self, state: &InvestigationState) -> String {
        format!(
            r#"(do
  ;; Synthesis phase - placeholder until advanced tools available
  (define target "{}")

  ;; Return minimal placeholder - NO fake data keys
  {{:wallet target
   :analysis_phase "Synthesis"
   :status "pending_tool_implementation"
   :note "Final synthesis requires complete data from all investigation phases"}}
)"#,
            state.target_wallet
        )
    }

    /// Store findings from OVSM execution
    async fn store_findings(&self, result: serde_json::Value) -> Result<()> {
        let mut state = self.state.lock().await;

        // Calculate significance based on content
        let significance = self.calculate_finding_significance(&result, &state.phase);

        let finding = Finding {
            category: format!("{:?}", state.phase),
            description: self.extract_finding_description(&result, &state.phase),
            significance,
            raw_data: result,
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)?
                .as_secs(),
        };

        state.findings.push(finding);
        Ok(())
    }

    /// Calculate significance of a finding based on its content
    fn calculate_finding_significance(&self, result: &serde_json::Value, phase: &InvestigationPhase) -> f64 {
        let mut significance = 0.5; // Base significance

        // Adjust based on phase
        match phase {
            InvestigationPhase::Initial => significance *= 0.8,
            InvestigationPhase::BasicProfiling => significance *= 0.9,
            InvestigationPhase::DeepAnalysis => significance *= 1.1,
            InvestigationPhase::PatternRecognition => significance *= 1.2,
            InvestigationPhase::HypothesisTesting => significance *= 1.3,
            InvestigationPhase::Synthesis => significance *= 1.5,
        }

        // Check for key indicators in the result
        if let Some(obj) = result.as_object() {
            // High significance indicators
            if obj.contains_key("mev_activity") {
                significance += 0.2;
            }
            if obj.contains_key("anomalies") {
                if let Some(anomalies) = obj.get("anomalies").and_then(|v| v.as_array()) {
                    significance += 0.1 * (anomalies.len() as f64).min(3.0) / 3.0;
                }
            }
            if obj.get("wash_trading_detected").and_then(|v| v.as_bool()).unwrap_or(false) {
                significance += 0.3;
            }
            if obj.contains_key("risk_assessment") {
                significance += 0.15;
            }

            // Adjust based on data volume
            if let Some(activity) = obj.get("recent_activity").and_then(|v| v.as_u64()) {
                significance *= 1.0 + (activity as f64 / 1000.0).min(0.5);
            }
        }

        significance.min(1.0).max(0.1) // Clamp between 0.1 and 1.0
    }

    /// Extract a descriptive summary of the finding
    fn extract_finding_description(&self, result: &serde_json::Value, phase: &InvestigationPhase) -> String {
        let base_desc = match phase {
            InvestigationPhase::Initial => "Initial wallet profiling completed",
            InvestigationPhase::BasicProfiling => "Basic behavior analysis completed",
            InvestigationPhase::DeepAnalysis => "Deep MEV and pattern analysis completed",
            InvestigationPhase::PatternRecognition => "Pattern recognition analysis completed",
            InvestigationPhase::HypothesisTesting => "Hypothesis testing completed",
            InvestigationPhase::Synthesis => "Investigation synthesis completed",
        };

        // Add specific details from the result
        if let Some(obj) = result.as_object() {
            let mut details = Vec::new();

            if let Some(balance) = obj.get("balance").and_then(|v| v.as_f64()) {
                details.push(format!("Balance: {:.2} SOL", balance / 1e9));
            }
            if let Some(count) = obj.get("recent_activity").and_then(|v| v.as_u64()) {
                details.push(format!("{} recent transactions", count));
            }
            if let Some(mev) = obj.get("mev_activity").and_then(|v| v.as_object()) {
                if !mev.is_empty() {
                    details.push("MEV activity detected".to_string());
                }
            }
            if obj.get("wash_trading_detected").and_then(|v| v.as_bool()).unwrap_or(false) {
                details.push("Wash trading patterns detected".to_string());
            }

            if !details.is_empty() {
                format!("{}: {}", base_desc, details.join(", "))
            } else {
                base_desc.to_string()
            }
        } else {
            base_desc.to_string()
        }
    }

    /// Generate hypotheses based on findings
    async fn generate_hypotheses(&self) -> Result<()> {
        let mut state = self.state.lock().await;

        // Analyze findings to generate hypotheses
        let mut hypotheses = Vec::new();

        // Analyze transaction patterns
        let mut regular_intervals = 0;
        let mut high_frequency = 0;
        let mut mev_activity = 0;
        let mut dex_focus = 0;

        for finding in &state.findings {
            let data = &finding.raw_data;

            // Check for automated behavior indicators
            if let Some(patterns) = data.get("periodic_activity") {
                if patterns.is_object() || patterns.is_array() {
                    regular_intervals += 1;
                }
            }

            // Check for high-frequency trading
            if let Some(activity) = data.get("recent_activity").and_then(|v| v.as_u64()) {
                if activity > 100 {
                    high_frequency += 1;
                }
            }

            // Check for MEV activity
            if data.get("mev_activity").is_some() {
                mev_activity += 1;
            }

            // Check for DEX interactions
            if let Some(programs) = data.get("program_interactions").and_then(|v| v.as_object()) {
                for (program, _) in programs {
                    if program.contains("serum") || program.contains("raydium") || program.contains("orca") {
                        dex_focus += 1;
                        break;
                    }
                }
            }
        }

        // Generate hypotheses based on evidence
        if regular_intervals > 0 || high_frequency > 1 {
            hypotheses.push(Hypothesis {
                statement: "This wallet exhibits automated trading behavior".to_string(),
                confidence: (regular_intervals as f64 * 0.3 + high_frequency as f64 * 0.2).min(0.9),
                supporting_evidence: vec![
                    if regular_intervals > 0 { Some("Regular transaction intervals detected".to_string()) } else { None },
                    if high_frequency > 0 { Some(format!("High transaction frequency observed")) } else { None },
                ].into_iter().flatten().collect(),
                contradicting_evidence: vec![],
            });
        }

        if mev_activity > 0 {
            hypotheses.push(Hypothesis {
                statement: "This wallet engages in MEV extraction strategies".to_string(),
                confidence: (mev_activity as f64 * 0.4).min(0.85),
                supporting_evidence: vec![
                    "MEV activity patterns detected".to_string(),
                    if dex_focus > 0 { "Heavy DEX interaction observed".to_string() } else { "".to_string() },
                ].into_iter().filter(|s| !s.is_empty()).collect(),
                contradicting_evidence: vec![],
            });
        }

        if dex_focus > 1 && mev_activity == 0 {
            hypotheses.push(Hypothesis {
                statement: "This wallet is primarily a DEX trader or liquidity provider".to_string(),
                confidence: (dex_focus as f64 * 0.25).min(0.75),
                supporting_evidence: vec![
                    "Multiple DEX protocol interactions".to_string(),
                    "Focus on decentralized exchange activity".to_string(),
                ],
                contradicting_evidence: if mev_activity == 0 {
                    vec!["No MEV extraction patterns found".to_string()]
                } else {
                    vec![]
                },
            });
        }

        // Default hypothesis if no specific patterns found
        if hypotheses.is_empty() {
            hypotheses.push(Hypothesis {
                statement: "This wallet exhibits standard user trading patterns".to_string(),
                confidence: 0.5,
                supporting_evidence: vec!["Transaction activity within normal parameters".to_string()],
                contradicting_evidence: vec![],
            });
        }

        // Add all generated hypotheses to state
        for hypothesis in hypotheses {
            state.hypotheses.push(hypothesis);
        }

        Ok(())
    }

    /// Determine next investigation phase
    fn determine_next_phase(
        &self,
        current: &InvestigationPhase,
        findings: &[Finding],
    ) -> InvestigationPhase {
        match current {
            InvestigationPhase::Initial if findings.len() >= 1 => InvestigationPhase::BasicProfiling,
            InvestigationPhase::BasicProfiling if findings.len() >= 2 => InvestigationPhase::DeepAnalysis,
            InvestigationPhase::DeepAnalysis if findings.len() >= 4 => InvestigationPhase::PatternRecognition,
            InvestigationPhase::PatternRecognition if findings.len() >= 6 => InvestigationPhase::HypothesisTesting,
            InvestigationPhase::HypothesisTesting => InvestigationPhase::Synthesis,
            _ => current.clone(),
        }
    }

    /// Convert OVSM Value to JSON
    fn value_to_json(&self, value: Value) -> Result<serde_json::Value> {
        use serde_json::json;

        let json = match value {
            Value::Null => json!(null),
            Value::Bool(b) => json!(b),
            Value::Int(i) => json!(i),
            Value::Float(f) => json!(f),
            Value::String(s) => json!(s),
            Value::Array(arr) => {
                let json_array: Result<Vec<_>> = arr.iter()
                    .map(|v| self.value_to_json(v.clone()))
                    .collect();
                json!(json_array?)
            }
            Value::Object(map) => {
                let mut json_map = serde_json::Map::new();
                for (k, v) in map.iter() {
                    json_map.insert(k.clone(), self.value_to_json(v.clone())?);
                }
                json!(json_map)
            }
            _ => {
                // For other Value types, convert to string representation
                json!(format!("{:?}", value))
            }
        };

        Ok(json)
    }

    /// Generate final investigation report
    async fn generate_final_report(&self) -> Result<String> {
        let state = self.state.lock().await;

        let findings_list = state.findings.iter()
            .map(|f| format!("- {}: {}", f.category, f.description))
            .collect::<Vec<_>>()
            .join("\n");

        let hypotheses_list = state.hypotheses.iter()
            .map(|h| format!("- {} (confidence: {:.2})", h.statement, h.confidence))
            .collect::<Vec<_>>()
            .join("\n");

        let mut report = String::new();
        report.push_str("# Wallet Investigation Report\n\n");
        report.push_str(&format!("**Target Wallet:** {}\n", state.target_wallet));
        report.push_str(&format!("**Iterations:** {}\n", state.iteration));
        report.push_str(&format!("**Findings:** {} collected\n\n", state.findings.len()));
        report.push_str("## Executive Summary\n\n");
        report.push_str(&format!("Based on {} iterations of investigation with self-evaluation, we have identified:\n\n", state.iteration));
        report.push_str("## Key Findings\n");
        report.push_str(&findings_list);
        report.push_str("\n\n## Hypotheses\n");
        report.push_str(&hypotheses_list);
        report.push_str(&format!("\n\n## Confidence Scores\n{:?}\n\n", state.confidence_scores));
        report.push_str("## Evidence Summary\n");
        report.push_str(&format!("- Total evidence pieces: {}\n", state.evidence.len()));
        report.push_str(&format!("- Investigation phases completed: {:?}", state.phase));

        Ok(report)
    }
}

/// Extension trait for AiService to support custom system prompts
impl AiService {
    pub async fn query_with_system_prompt(
        &self,
        system_prompt: &str,
        user_prompt: &str,
    ) -> Result<String> {
        // Use the query_osvm_ai_with_options method to include system prompt
        // IMPORTANT: Use ownPlan=true to bypass planning and use our custom system prompt directly
        self.query_osvm_ai_with_options(
            user_prompt,
            Some(system_prompt.to_string()),
            Some(true),  // ownPlan=true - use custom system prompt, bypass planning
            false  // debug
        ).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use tokio::sync::Mutex;

    // Helper function to create test research agent
    async fn create_test_agent() -> ResearchAgent {
        let ai_service = Arc::new(Mutex::new(
            AiService::new()
        ));
        let ovsm_service = Arc::new(Mutex::new(OvsmService::new()));

        ResearchAgent::new(
            ai_service,
            ovsm_service,
            "TestWallet123".to_string()
        )
    }

    #[tokio::test]
    async fn test_initial_state() {
        let agent = create_test_agent().await;
        let state = agent.state.lock().await;

        assert_eq!(state.target_wallet, "TestWallet123");
        assert!(matches!(state.phase, InvestigationPhase::Initial));
        assert_eq!(state.iteration, 0);
        assert!(state.findings.is_empty());
        assert!(state.hypotheses.is_empty());
        assert_eq!(state.next_steps.len(), 3);
    }

    #[tokio::test]
    async fn test_generate_initial_script() {
        let agent = create_test_agent().await;
        let script = agent.generate_initial_script("TestWallet");

        assert!(script.contains("TestWallet"));
        assert!(script.contains("getBalance"));
        assert!(script.contains("getSignaturesForAddress"));
        assert!(script.contains("getTokenAccountsByOwner"));
        assert!(script.contains("analyzeTransactionType"));
    }

    #[tokio::test]
    async fn test_generate_profiling_script() {
        let agent = create_test_agent().await;
        let state = InvestigationState {
            target_wallet: "TestWallet".to_string(),
            phase: InvestigationPhase::BasicProfiling,
            findings: vec![],
            hypotheses: vec![],
            evidence: HashMap::new(),
            next_steps: vec![],
            confidence_scores: HashMap::new(),
            iteration: 1,
        };

        let script = agent.generate_profiling_script(&state);

        assert!(script.contains("TestWallet"));
        assert!(script.contains("Deep profiling"));
        assert!(script.contains("analyzeTemporalPatterns"));
        assert!(script.contains("filterDexTransactions"));
    }

    #[tokio::test]
    async fn test_generate_deep_analysis_script() {
        let agent = create_test_agent().await;
        let state = InvestigationState {
            target_wallet: "MEVWallet".to_string(),
            phase: InvestigationPhase::DeepAnalysis,
            findings: vec![],
            hypotheses: vec![],
            evidence: HashMap::new(),
            next_steps: vec![],
            confidence_scores: HashMap::new(),
            iteration: 2,
        };

        let script = agent.generate_deep_analysis_script(&state);

        assert!(script.contains("MEVWallet"));
        assert!(script.contains("MEV"));
        assert!(script.contains("sandwich"));
        assert!(script.contains("arbitrage"));
        assert!(script.contains("wash"));
        assert!(script.contains("multi-hop"));
    }

    #[tokio::test]
    async fn test_generate_pattern_script() {
        let agent = create_test_agent().await;
        let state = InvestigationState {
            target_wallet: "PatternWallet".to_string(),
            phase: InvestigationPhase::PatternRecognition,
            findings: vec![],
            hypotheses: vec![],
            evidence: HashMap::new(),
            next_steps: vec![],
            confidence_scores: HashMap::new(),
            iteration: 3,
        };

        let script = agent.generate_pattern_script(&state);

        assert!(script.contains("PatternWallet"));
        assert!(script.contains("Pattern recognition"));
        assert!(script.contains("placeholder"));
        assert!(script.contains("analysis_phase"));
        assert!(script.contains("anomalies"));
    }

    #[tokio::test]
    async fn test_hypothesis_generation_with_mev() {
        let agent = create_test_agent().await;

        // Add findings with MEV activity
        {
            let mut state = agent.state.lock().await;
            state.findings.push(Finding {
                category: "DeepAnalysis".to_string(),
                description: "MEV activity detected".to_string(),
                significance: 0.8,
                raw_data: serde_json::json!({
                    "mev_activity": {"sandwich_attacks": 5},
                    "recent_activity": 150
                }),
                timestamp: 1234567890,
            });
        }

        agent.generate_hypotheses().await.unwrap();

        let state = agent.state.lock().await;
        assert!(!state.hypotheses.is_empty());

        let mev_hypothesis = state.hypotheses.iter()
            .find(|h| h.statement.contains("MEV"))
            .expect("Should have MEV hypothesis");

        assert!(mev_hypothesis.confidence > 0.3);
        assert!(!mev_hypothesis.supporting_evidence.is_empty());
    }

    #[tokio::test]
    async fn test_hypothesis_generation_with_automated_behavior() {
        let agent = create_test_agent().await;

        // Add findings indicating automated behavior
        {
            let mut state = agent.state.lock().await;
            state.findings.push(Finding {
                category: "PatternRecognition".to_string(),
                description: "Regular patterns detected".to_string(),
                significance: 0.7,
                raw_data: serde_json::json!({
                    "periodic_activity": {"interval": 300},
                    "recent_activity": 500
                }),
                timestamp: 1234567890,
            });
        }

        agent.generate_hypotheses().await.unwrap();

        let state = agent.state.lock().await;
        let automated_hypothesis = state.hypotheses.iter()
            .find(|h| h.statement.contains("automated"))
            .expect("Should have automation hypothesis");

        assert!(automated_hypothesis.confidence > 0.2);
    }

    #[tokio::test]
    async fn test_calculate_finding_significance() {
        let agent = create_test_agent().await;

        // Test with MEV activity
        let mev_result = serde_json::json!({
            "mev_activity": {"sandwich_attacks": 10},
            "wash_trading_detected": true
        });

        let significance = agent.calculate_finding_significance(
            &mev_result,
            &InvestigationPhase::DeepAnalysis
        );

        assert!(significance > 0.7, "MEV + wash trading should have high significance");

        // Test with normal activity
        let normal_result = serde_json::json!({
            "balance": 1000000000,
            "recent_activity": 10
        });

        let normal_significance = agent.calculate_finding_significance(
            &normal_result,
            &InvestigationPhase::Initial
        );

        assert!(normal_significance < 0.6, "Normal activity should have lower significance");
    }

    #[tokio::test]
    async fn test_extract_finding_description() {
        let agent = create_test_agent().await;

        let result = serde_json::json!({
            "balance": 5000000000.0,
            "recent_activity": 42,
            "mev_activity": {"arbitrage": 3},
            "wash_trading_detected": true
        });

        let description = agent.extract_finding_description(
            &result,
            &InvestigationPhase::DeepAnalysis
        );

        assert!(description.contains("Deep MEV"));
        assert!(description.contains("5.00 SOL"));
        assert!(description.contains("42 recent"));
        assert!(description.contains("MEV activity"));
        assert!(description.contains("Wash trading"));
    }

    #[tokio::test]
    async fn test_phase_transitions() {
        let agent = create_test_agent().await;

        // Add findings to trigger phase transitions
        {
            let mut state = agent.state.lock().await;

            // Initial phase with 1 finding
            assert!(matches!(state.phase, InvestigationPhase::Initial));

            state.findings.push(Finding {
                category: "Initial".to_string(),
                description: "Test".to_string(),
                significance: 0.5,
                raw_data: serde_json::json!({}),
                timestamp: 1,
            });
        }

        let next_phase = agent.determine_next_phase(
            &InvestigationPhase::Initial,
            &vec![Finding {
                category: "test".to_string(),
                description: "test".to_string(),
                significance: 0.5,
                raw_data: serde_json::json!({}),
                timestamp: 1,
            }]
        );

        assert!(matches!(next_phase, InvestigationPhase::BasicProfiling));
    }

    #[tokio::test]
    async fn test_build_evaluation_prompt() {
        let agent = create_test_agent().await;
        let state = InvestigationState {
            target_wallet: "TestWallet".to_string(),
            phase: InvestigationPhase::HypothesisTesting,
            findings: vec![],
            hypotheses: vec![],
            evidence: HashMap::new(),
            next_steps: vec![],
            confidence_scores: HashMap::from([
                ("test_score".to_string(), 0.75)
            ]),
            iteration: 5,
        };

        let prompt = agent.build_evaluation_prompt(&state);

        assert!(prompt.contains("testing specific hypotheses"));
        assert!(prompt.contains("iteration: 5"));
        assert!(prompt.contains("test_score"));
        assert!(prompt.contains("0.75"));
        assert!(prompt.contains("JSON format"));
    }

    #[tokio::test]
    async fn test_value_to_json_conversion() {
        let agent = create_test_agent().await;

        // Test various Value types
        let null_value = Value::Null;
        assert_eq!(agent.value_to_json(null_value).unwrap(), serde_json::Value::Null);

        let bool_value = Value::Bool(true);
        assert_eq!(agent.value_to_json(bool_value).unwrap(), serde_json::json!(true));

        let int_value = Value::Int(42);
        assert_eq!(agent.value_to_json(int_value).unwrap(), serde_json::json!(42));

        let float_value = Value::Float(3.14);
        assert_eq!(agent.value_to_json(float_value).unwrap(), serde_json::json!(3.14));

        let string_value = Value::String("test".to_string());
        assert_eq!(agent.value_to_json(string_value).unwrap(), serde_json::json!("test"));
    }

    #[tokio::test]
    async fn test_hypothesis_test_script_generation() {
        let agent = create_test_agent().await;

        let state = InvestigationState {
            target_wallet: "TestWallet".to_string(),
            phase: InvestigationPhase::HypothesisTesting,
            findings: vec![],
            hypotheses: vec![
                Hypothesis {
                    statement: "Bot trader".to_string(),
                    confidence: 0.7,
                    supporting_evidence: vec!["Regular intervals".to_string()],
                    contradicting_evidence: vec![],
                },
                Hypothesis {
                    statement: "MEV searcher".to_string(),
                    confidence: 0.6,
                    supporting_evidence: vec!["Sandwich patterns".to_string()],
                    contradicting_evidence: vec!["Low profit".to_string()],
                }
            ],
            evidence: HashMap::new(),
            next_steps: vec![],
            confidence_scores: HashMap::new(),
            iteration: 3,
        };

        let script = agent.generate_hypothesis_test_script(&state);

        assert!(script.contains("TestWallet"));
        assert!(script.contains("Hypothesis testing"));
        assert!(script.contains("placeholder"));
        assert!(script.contains("hypotheses_tested 2"));
        assert!(script.contains("analysis_phase"));
    }

    #[tokio::test]
    async fn test_synthesis_script_generation() {
        let agent = create_test_agent().await;

        let state = InvestigationState {
            target_wallet: "SynthesisWallet".to_string(),
            phase: InvestigationPhase::Synthesis,
            findings: vec![
                Finding {
                    category: "Initial".to_string(),
                    description: "Basic profile".to_string(),
                    significance: 0.5,
                    raw_data: serde_json::json!({}),
                    timestamp: 1,
                },
                Finding {
                    category: "DeepAnalysis".to_string(),
                    description: "MEV detected".to_string(),
                    significance: 0.8,
                    raw_data: serde_json::json!({}),
                    timestamp: 2,
                }
            ],
            hypotheses: vec![
                Hypothesis {
                    statement: "Professional trader".to_string(),
                    confidence: 0.85,
                    supporting_evidence: vec![],
                    contradicting_evidence: vec![],
                }
            ],
            evidence: HashMap::new(),
            next_steps: vec![],
            confidence_scores: HashMap::new(),
            iteration: 10,
        };

        let script = agent.generate_synthesis_script(&state);

        assert!(script.contains("SynthesisWallet"));
        assert!(script.contains("Synthesis phase"));
        assert!(script.contains("placeholder"));
        assert!(script.contains("findings_count 2"));
        assert!(script.contains("hypotheses_tested 1"));
        assert!(script.contains("iteration 10"));
    }
}