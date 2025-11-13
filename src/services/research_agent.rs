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
  ;; Initial wallet investigation - find top 5 inflow/outflow addresses
  (define wallet "{}")

  ;; Get recent transactions using MCP tool
  (define tx_response (get_account_transactions {{:address wallet :limit 200}}))
  (define tx_content (get tx_response "content"))
  (define tx_data (if tx_content tx_content tx_response))
  (define transactions (get tx_data "transactions"))

  ;; Initialize maps to track counterparties
  (define inflow_map {{}})   ;; addresses that sent TO wallet
  (define outflow_map {{}})  ;; addresses that received FROM wallet

  ;; Process each transaction to find transfers
  (if (and transactions (array? transactions))
    (for (tx transactions)
      ;; Check for transfers in the transaction
      (define transfers (get tx "transfers"))
      (when (and transfers (array? transfers))
        (for (transfer transfers)
          (define from_addr (get transfer "from"))
          (define to_addr (get transfer "to"))
          (define amount_raw (get transfer "amount"))
          (define amount (if amount_raw amount_raw 0))

          ;; Track inflows (someone sent TO our wallet)
          (when (and to_addr (== to_addr wallet) from_addr (!= from_addr wallet))
            (define current_in (get inflow_map from_addr))
            (define new_amount (if current_in (+ current_in amount) amount))
            (set! inflow_map (merge inflow_map {{from_addr new_amount}})))

          ;; Track outflows (we sent FROM our wallet)
          (when (and from_addr (== from_addr wallet) to_addr (!= to_addr wallet))
            (define current_out (get outflow_map to_addr))
            (define new_amount (if current_out (+ current_out amount) amount))
            (set! outflow_map (merge outflow_map {{to_addr new_amount}}))))))
    (log :message "No transactions found"))

  ;; Convert maps to lists for sorting
  (define inflow_list [])
  (for (addr (keys inflow_map))
    (set! inflow_list (append inflow_list [{{:address addr :total (get inflow_map addr)}}])))

  (define outflow_list [])
  (for (addr (keys outflow_map))
    (set! outflow_list (append outflow_list [{{:address addr :total (get outflow_map addr)}}])))

  ;; Sort by total amount and take top 5
  (define top_inflows (take 5 (sort-by inflow_list (lambda (x) (get x "total")) :desc)))
  (define top_outflows (take 5 (sort-by outflow_list (lambda (x) (get x "total")) :desc)))

  ;; Return results
  {{:wallet wallet
   :transaction_count (if transactions (count transactions) 0)
   :top_5_inflow_addresses top_inflows
   :top_5_outflow_addresses top_outflows
   :unique_inflows (count (keys inflow_map))
   :unique_outflows (count (keys outflow_map))}}
)"#,
            wallet
        )
    }

    /// Generate profiling script based on initial findings
    fn generate_profiling_script(&self, state: &InvestigationState) -> String {
        format!(
            r#"(do
  ;; Deep profiling of wallet behavior
  (define wallet "{}")

  ;; Analyze interaction patterns
  (define signatures (getSignaturesForAddress wallet :limit 200))
  (define programs {{}})

  (for (sig signatures)
    (define tx (getTransaction sig))
    (define prog (extractProgramId tx))
    (set! programs (addToMap programs prog 1)))

  ;; Time-based analysis
  (define time-patterns (analyzeTemporalPatterns signatures))

  ;; DEX interaction analysis
  (define dex-activity (filterDexTransactions signatures))

  {{:wallet wallet
   :program_interactions programs
   :temporal_patterns time-patterns
   :dex_activity dex-activity}}
)"#,
            state.target_wallet
        )
    }

    /// Generate deep analysis script for MEV and advanced patterns
    fn generate_deep_analysis_script(&self, state: &InvestigationState) -> String {
        format!(
            r#"(do
  ;; Deep analysis of wallet behavior and MEV activity
  (define wallet "{}")

  ;; Analyze MEV opportunities
  (define recent-txs (getSignaturesForAddress wallet :limit 500))
  (define mev-data {{}})

  ;; Check for sandwich attacks
  (for (sig recent-txs)
    (define tx (getTransaction sig))
    (define pre-tx (getPreviousTransaction sig))
    (define post-tx (getNextTransaction sig))

    (if (and pre-tx post-tx)
      (do
        (define is-sandwich (detectSandwichPattern pre-tx tx post-tx))
        (if is-sandwich
          (set! mev-data (addToMap mev-data "sandwich_attacks" 1))))))

  ;; Analyze arbitrage patterns
  (define arb-txs (filterArbitrageTransactions recent-txs))
  (define arb-profits (calculateArbitrageProfits arb-txs))

  ;; Multi-hop transaction tracking
  (define multi-hops {{}})
  (for (sig recent-txs)
    (define tx (getTransaction sig))
    (define hops (traceTransactionHops tx :max-depth 5))
    (if (> (length hops) 2)
      (set! multi-hops (addToMap multi-hops sig hops))))

  ;; Check for wash trading patterns
  (define wash-trades (detectWashTrading wallet recent-txs))

  ;; Analyze gas optimization patterns
  (define gas-patterns (analyzeGasUsage recent-txs))

  ;; Protocol interaction depth
  (define protocol-depth (measureProtocolComplexity recent-txs))

  {{:wallet wallet
   :mev_activity mev-data
   :arbitrage {{:count (length arb-txs)
               :total_profit arb-profits}}
   :multi_hop_count (length multi-hops)
   :wash_trading_detected (> wash-trades 0)
   :gas_optimization_score gas-patterns
   :protocol_complexity protocol-depth
   :analysis_depth "deep"}}
)"#,
            state.target_wallet
        )
    }

    /// Generate pattern recognition script
    fn generate_pattern_script(&self, state: &InvestigationState) -> String {
        format!(
            r#"(do
  ;; Pattern recognition for wallet behavior
  (define wallet "{}")

  ;; Time-based pattern analysis
  (define signatures (getSignaturesForAddress wallet :limit 1000))
  (define timestamps (extractTimestamps signatures))

  ;; Detect periodic patterns
  (define periodic-patterns (detectPeriodicPatterns timestamps))

  ;; Transaction volume patterns
  (define volumes (extractTransactionVolumes signatures))
  (define volume-patterns (analyzeVolumeDistribution volumes))

  ;; Interaction patterns with specific programs
  (define program-patterns {{}})
  (for (sig signatures)
    (define tx (getTransaction sig))
    (define programs (extractProgramIds tx))
    (for (prog programs)
      (define pattern (analyzeProgramInteraction prog tx))
      (set! program-patterns (addToMap program-patterns prog pattern))))

  ;; Token movement patterns
  (define token-flows (analyzeTokenFlows wallet signatures))

  ;; Behavioral clustering
  (define behavior-clusters (clusterBehaviors {{
    :time-patterns periodic-patterns
    :volume-patterns volume-patterns
    :program-patterns program-patterns
    :token-flows token-flows}}))

  ;; Anomaly detection in patterns
  (define anomalies (detectPatternAnomalies behavior-clusters))

  {{:wallet wallet
   :periodic_activity periodic-patterns
   :volume_distribution volume-patterns
   :program_interaction_patterns program-patterns
   :token_flow_patterns token-flows
   :behavioral_clusters behavior-clusters
   :anomalies anomalies
   :pattern_confidence (calculatePatternConfidence behavior-clusters)}}
)"#,
            state.target_wallet
        )
    }

    /// Generate hypothesis testing script
    fn generate_hypothesis_test_script(&self, state: &InvestigationState) -> String {
        // Build dynamic hypothesis tests based on current hypotheses
        let mut hypothesis_tests = String::new();

        for (i, hypothesis) in state.hypotheses.iter().enumerate() {
            let evidence_for = hypothesis.supporting_evidence.iter()
                .map(|e| format!("\"{}\"", e.replace('"', "\\\"")))
                .collect::<Vec<_>>()
                .join(" ");

            let evidence_against = hypothesis.contradicting_evidence.iter()
                .map(|e| format!("\"{}\"", e.replace('"', "\\\"")))
                .collect::<Vec<_>>()
                .join(" ");

            let test_code = format!(
                "\n  ;; Test hypothesis {}: {}\n  (define h{}_result (testHypothesis {{\n    :statement \"{}\"\n    :confidence {}\n    :evidence_for [{}]\n    :evidence_against [{}]}}))\n  ",
                i,
                hypothesis.statement,
                i,
                hypothesis.statement.replace('"', "\\\""),
                hypothesis.confidence,
                evidence_for,
                evidence_against
            );

            hypothesis_tests.push_str(&test_code);
        }

        format!(
            r#"(do
  ;; Hypothesis testing for wallet {}
  (define wallet "{}")

  ;; Get comprehensive data for hypothesis testing
  (define all-signatures (getSignaturesForAddress wallet :limit 2000))
  (define account-data (getAccountInfo wallet))
  (define token-accounts (getTokenAccountsByOwner wallet))

  {}

  ;; Collect all hypothesis test results
  (define test-results [{}])

  ;; Statistical validation
  (define statistical-validation (performStatisticalTests test-results))

  ;; Correlation analysis
  (define correlations (analyzeHypothesisCorrelations test-results))

  ;; Generate counter-evidence search
  (define counter-evidence (searchCounterEvidence wallet test-results))

  {{:wallet wallet
   :hypotheses_tested {}
   :test_results test-results
   :statistical_validation statistical-validation
   :correlations correlations
   :counter_evidence counter-evidence
   :overall_confidence (calculateOverallConfidence test-results)}}
)"#,
            state.target_wallet,
            state.target_wallet,
            hypothesis_tests,
            (0..state.hypotheses.len())
                .map(|i| format!("h{}_result", i))
                .collect::<Vec<_>>()
                .join(" "),
            state.hypotheses.len()
        )
    }

    /// Generate synthesis script to combine all findings
    fn generate_synthesis_script(&self, state: &InvestigationState) -> String {
        // Build findings array string
        let findings_str = state.findings.iter()
            .map(|f| format!(
                "{{:category \"{}\" :significance {} :description \"{}\"}}",
                f.category,
                f.significance,
                f.description.replace('"', "\\\"")
            ))
            .collect::<Vec<_>>()
            .join(" ");

        format!(
            r#"(do
  ;; Synthesis of all findings for wallet {}
  (define wallet "{}")

  ;; Compile all findings from previous phases
  (define all-findings [{}])

  ;; Key metrics summary
  (define metrics {{
    :total_transactions (getTransactionCount wallet)
    :active_days (getActiveDays wallet)
    :unique_programs (getUniqueProgramsInteracted wallet)
    :total_volume (getTotalVolume wallet)
    :risk_score (calculateRiskScore wallet)}})

  ;; Behavioral profile synthesis
  (define behavioral-profile {{
    :trader_type (classifyTraderType wallet all-findings)
    :activity_level (determineActivityLevel metrics)
    :sophistication (measureSophistication all-findings)
    :automation_likelihood (detectAutomation all-findings)}})

  ;; Network analysis
  (define network {{
    :connected_wallets (getConnectedWallets wallet)
    :centrality_score (calculateCentrality wallet)
    :cluster_id (identifyCluster wallet)}})

  ;; Risk assessment
  (define risk-assessment {{
    :aml_risk (assessAMLRisk wallet all-findings)
    :fraud_indicators (detectFraudIndicators all-findings)
    :wash_trading_risk (assessWashTradingRisk all-findings)}})

  ;; Generate final insights
  (define insights (generateInsights {{
    :findings all-findings
    :metrics metrics
    :profile behavioral-profile
    :network network
    :risk risk-assessment}}))

  ;; Confidence scores for each conclusion
  (define confidence-scores {{
    :profile_confidence (calculateProfileConfidence behavioral-profile)
    :risk_confidence (calculateRiskConfidence risk-assessment)
    :network_confidence (calculateNetworkConfidence network)}})

  {{:wallet wallet
   :investigation_summary {{
     :phases_completed {}
     :findings_count {}
     :hypotheses_tested {}
     :iteration {}}}
   :final_profile behavioral-profile
   :metrics metrics
   :network_analysis network
   :risk_assessment risk-assessment
   :key_insights insights
   :confidence_scores confidence-scores
   :recommendations (generateRecommendations insights)}}
)"#,
            state.target_wallet,
            state.target_wallet,
            findings_str,
            format!("\"{:?}\"", state.phase),  // Make it a string for LISP
            state.findings.len(),
            state.hypotheses.len(),
            state.iteration
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
        self.query_osvm_ai_with_options(
            user_prompt,
            Some(system_prompt.to_string()),
            None,  // only_plan
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

        assert!(script.contains("Pattern recognition"));
        assert!(script.contains("periodic-patterns"));
        assert!(script.contains("volume-patterns"));
        assert!(script.contains("behavioral_clusters"));
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

        assert!(script.contains("Bot trader"));
        assert!(script.contains("MEV searcher"));
        assert!(script.contains("Regular intervals"));
        assert!(script.contains("Sandwich patterns"));
        assert!(script.contains("Low profit"));
        assert!(script.contains("h0_result"));
        assert!(script.contains("h1_result"));
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
        assert!(script.contains("Synthesis of all findings"));
        assert!(script.contains("behavioral-profile"));
        assert!(script.contains("risk-assessment"));
        assert!(script.contains("network_analysis"));
        assert!(script.contains("findings_count 2"));
        assert!(script.contains("hypotheses_tested 1"));
        assert!(script.contains("iteration 10"));
    }
}