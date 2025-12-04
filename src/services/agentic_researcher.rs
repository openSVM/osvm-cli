use crate::services::ai_service::AiService;
use crate::services::ovsm_service::OvsmService;
use anyhow::{Context, Result};
use ovsm::runtime::Value;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
use tokio::sync::Mutex;

/// Represents a self-directed investigation agent with recursive questioning
#[derive(Clone)]
pub struct AgenticResearcher {
    ai_service: Arc<Mutex<AiService>>,
    ovsm_service: Arc<Mutex<OvsmService>>,
    investigation_graph: Arc<Mutex<InvestigationGraph>>,
    metacognition: Arc<Mutex<MetaCognition>>,
}

// Manual Debug implementation
impl std::fmt::Debug for AgenticResearcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AgenticResearcher")
            .field("ai_service", &"<AiService>")
            .field("ovsm_service", &"<OvsmService>")
            .field("investigation_graph", &"<InvestigationGraph>")
            .field("metacognition", &"<MetaCognition>")
            .finish()
    }
}

/// Graph structure representing investigation paths and discoveries
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvestigationGraph {
    pub nodes: HashMap<String, InvestigationNode>,
    pub edges: Vec<InvestigationEdge>,
    pub active_branches: VecDeque<String>,
    pub unexplored_questions: VecDeque<Question>,
    pub convergence_points: Vec<ConvergencePoint>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvestigationNode {
    pub id: String,
    pub node_type: NodeType,
    pub data: serde_json::Value,
    pub confidence: f64,
    pub timestamp: u64,
    pub derived_questions: Vec<Question>,
    pub supporting_evidence: Vec<String>,
    pub contradicting_evidence: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NodeType {
    Observation,
    Hypothesis,
    Question,
    Evidence,
    Anomaly,
    Pattern,
    Contradiction,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvestigationEdge {
    pub from: String,
    pub to: String,
    pub relationship: EdgeRelationship,
    pub strength: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EdgeRelationship {
    LeadsTo,
    Contradicts,
    Supports,
    Refines,
    Triggers,
    Explains,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Question {
    pub id: String,
    pub content: String,
    pub priority: f64,
    pub question_type: QuestionType,
    pub parent_node: Option<String>,
    pub exploration_depth: u32,
    pub expected_impact: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum QuestionType {
    Exploratory,   // "What else might be happening?"
    Verificatory,  // "Is this actually true?"
    Contradictory, // "What evidence contradicts this?"
    Causal,        // "Why is this happening?"
    Predictive,    // "What will happen next?"
    Comparative,   // "How does this compare to...?"
    Anomalistic,   // "Why is this unusual?"
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConvergencePoint {
    pub hypothesis: String,
    pub supporting_paths: Vec<String>,
    pub confidence: f64,
    pub timestamp: u64,
}

/// Metacognitive layer that monitors and guides the investigation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetaCognition {
    pub investigation_strategy: InvestigationStrategy,
    pub curiosity_level: f64,
    pub exploration_vs_exploitation: f64,
    pub confidence_thresholds: ConfidenceThresholds,
    pub investigation_patterns: Vec<RecognizedPattern>,
    pub dead_ends: HashSet<String>,
    pub breakthrough_indicators: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InvestigationStrategy {
    BreadthFirst,  // Explore many shallow paths
    DepthFirst,    // Deep dive into promising leads
    Opportunistic, // Follow anomalies and surprises
    Systematic,    // Methodical coverage
    Adversarial,   // Actively seek contradictions
    Creative,      // Generate novel hypotheses
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfidenceThresholds {
    pub hypothesis_acceptance: f64,
    pub question_generation: f64,
    pub branch_abandonment: f64,
    pub convergence_declaration: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecognizedPattern {
    pub pattern_type: String,
    pub instances: Vec<String>,
    pub confidence: f64,
    pub implications: Vec<String>,
}

/// Sub-agent for parallel investigation threads
#[derive(Debug, Clone)]
pub struct SubInvestigator {
    pub id: String,
    pub focus_area: String,
    pub investigation_thread: Arc<Mutex<InvestigationThread>>,
    pub parent_researcher: Arc<AgenticResearcher>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvestigationThread {
    pub focus: String,
    pub depth: u32,
    pub findings: Vec<serde_json::Value>,
    pub local_hypotheses: Vec<String>,
    pub status: ThreadStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ThreadStatus {
    Active,
    Blocked,
    Converged,
    Abandoned,
}

impl AgenticResearcher {
    pub fn new(
        ai_service: Arc<Mutex<AiService>>,
        ovsm_service: Arc<Mutex<OvsmService>>,
        target: String,
    ) -> Self {
        let investigation_graph = InvestigationGraph {
            nodes: HashMap::new(),
            edges: Vec::new(),
            active_branches: VecDeque::new(),
            unexplored_questions: VecDeque::from([Question {
                id: Uuid::new_v4().to_string(),
                content: format!("What is the primary activity pattern of {}?", target),
                priority: 1.0,
                question_type: QuestionType::Exploratory,
                parent_node: None,
                exploration_depth: 0,
                expected_impact: 0.9,
            }]),
            convergence_points: Vec::new(),
        };

        let metacognition = MetaCognition {
            investigation_strategy: InvestigationStrategy::Opportunistic,
            curiosity_level: 0.8,
            exploration_vs_exploitation: 0.6,
            confidence_thresholds: ConfidenceThresholds {
                hypothesis_acceptance: 0.7,
                question_generation: 0.3,
                branch_abandonment: 0.1,
                convergence_declaration: 0.85,
            },
            investigation_patterns: Vec::new(),
            dead_ends: HashSet::new(),
            breakthrough_indicators: Vec::new(),
        };

        Self {
            ai_service,
            ovsm_service,
            investigation_graph: Arc::new(Mutex::new(investigation_graph)),
            metacognition: Arc::new(Mutex::new(metacognition)),
        }
    }

    /// Main investigation loop with recursive self-questioning
    pub async fn investigate_autonomously(&self) -> Result<String> {
        let max_depth = 20;
        let mut iteration = 0;

        println!("\n🧠 Autonomous Investigation Initiated");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

        while iteration < max_depth {
            iteration += 1;
            println!("\n🔄 Cognitive Iteration #{}", iteration);

            // 1. Self-reflect on current state
            let reflection = self.metacognitive_reflection().await?;
            println!(
                "💭 Reflection: {}",
                &reflection[..reflection.len().min(200)]
            );

            // 2. Generate new questions based on current knowledge
            let new_questions = self.generate_questions().await?;
            println!("❓ Generated {} new questions", new_questions.len());

            // 3. Prioritize and select next investigation
            let next_question = self.select_next_question().await?;
            if next_question.is_none() {
                println!("✅ Investigation converged - no more high-priority questions");
                break;
            }
            let question = next_question.unwrap();
            println!("🎯 Investigating: {}", question.content);

            // 4. Execute investigation for selected question
            let findings = self.investigate_question(&question).await?;

            // 5. Seek contradictory evidence actively
            let contradictions = self.seek_contradictions(&findings).await?;

            // 6. Update investigation graph with findings
            self.update_graph(findings, contradictions).await?;

            // 7. Detect anomalies and spawn sub-investigations
            let anomalies = self.detect_anomalies().await?;
            for anomaly in anomalies {
                self.spawn_sub_investigation(anomaly).await?;
            }

            // 8. Check for convergence patterns
            if self.check_convergence().await? {
                println!("🎊 Strong convergence detected!");
                break;
            }

            // 9. Adjust strategy based on progress
            self.adapt_strategy().await?;
        }

        // Generate final synthesis
        self.synthesize_investigation().await
    }

    /// Metacognitive reflection on investigation progress
    async fn metacognitive_reflection(&self) -> Result<String> {
        let meta = self.metacognition.lock().await;
        let graph = self.investigation_graph.lock().await;

        let reflection_prompt = format!(
            "As a metacognitive observer of an investigation:\n\
            Current strategy: {:?}\n\
            Exploration vs Exploitation: {:.2}\n\
            Active branches: {}\n\
            Unexplored questions: {}\n\
            Dead ends encountered: {}\n\n\
            Reflect on:\n\
            1. Are we asking the right questions?\n\
            2. What patterns are emerging?\n\
            3. Where are our blind spots?\n\
            4. Should we change strategy?\n\
            5. What's most surprising so far?\n\n\
            Provide a brief metacognitive assessment.",
            meta.investigation_strategy,
            meta.exploration_vs_exploitation,
            graph.active_branches.len(),
            graph.unexplored_questions.len(),
            meta.dead_ends.len()
        );

        let ai = self.ai_service.lock().await;
        ai.query(&reflection_prompt).await
    }

    /// Generate new questions based on current knowledge
    async fn generate_questions(&self) -> Result<Vec<Question>> {
        let graph = self.investigation_graph.lock().await;

        // Build context from recent discoveries
        let recent_nodes: Vec<_> = graph
            .nodes
            .values()
            .filter(|n| matches!(n.node_type, NodeType::Observation | NodeType::Pattern))
            .take(5)
            .collect();

        let context = serde_json::to_string(&recent_nodes)?;

        let question_prompt = format!(
            "You are an infinitely curious investigator. Based on these findings:\n{}\n\n\
            Generate 5-10 follow-up questions that:\n\
            1. Challenge assumptions\n\
            2. Explore edge cases\n\
            3. Seek root causes\n\
            4. Identify patterns\n\
            5. Test boundaries\n\
            6. Find contradictions\n\n\
            Format each question with:\n\
            - Question: <the question>\n\
            - Type: <Exploratory/Verificatory/Contradictory/Causal/Predictive/Comparative/Anomalistic>\n\
            - Priority: <0.0-1.0>\n\
            - Expected Impact: <0.0-1.0>",
            context
        );

        let ai = self.ai_service.lock().await;
        let response = ai.query(&question_prompt).await?;

        // Parse response into Question objects
        self.parse_questions(response).await
    }

    /// Select the next question to investigate based on multiple factors
    async fn select_next_question(&self) -> Result<Option<Question>> {
        let mut graph = self.investigation_graph.lock().await;
        let meta = self.metacognition.lock().await;

        if graph.unexplored_questions.is_empty() {
            return Ok(None);
        }

        // Score questions based on strategy
        let scored_questions: Vec<_> = graph
            .unexplored_questions
            .iter()
            .map(|q| {
                let mut score = q.priority * q.expected_impact;

                // Adjust based on current strategy
                match meta.investigation_strategy {
                    InvestigationStrategy::BreadthFirst => {
                        score *= 1.0 / (q.exploration_depth as f64 + 1.0);
                    }
                    InvestigationStrategy::DepthFirst => {
                        score *= q.exploration_depth as f64 + 1.0;
                    }
                    InvestigationStrategy::Opportunistic => {
                        if matches!(q.question_type, QuestionType::Anomalistic) {
                            score *= 2.0;
                        }
                    }
                    InvestigationStrategy::Adversarial => {
                        if matches!(q.question_type, QuestionType::Contradictory) {
                            score *= 2.0;
                        }
                    }
                    _ => {}
                }

                // Boost score based on curiosity level
                score *= meta.curiosity_level;

                (q.clone(), score)
            })
            .collect();

        // Select highest scoring question
        let best = scored_questions
            .into_iter()
            .max_by(|a, b| a.1.partial_cmp(&b.1).unwrap());

        if let Some((question, _score)) = best {
            // Remove from queue
            graph.unexplored_questions.retain(|q| q.id != question.id);
            Ok(Some(question))
        } else {
            Ok(None)
        }
    }

    /// Investigate a specific question
    async fn investigate_question(&self, question: &Question) -> Result<serde_json::Value> {
        // Generate OVSM script based on question type
        let ovsm_script = self.generate_investigation_script(question).await?;

        println!("📜 Executing investigation script...");

        // Execute OVSM script
        let mut ovsm = self.ovsm_service.lock().await;
        let result = ovsm
            .execute_code(&ovsm_script)
            .context("Failed to execute investigation script")?;

        // Convert to JSON
        let json_result = value_to_json(result)?;

        // Interpret findings with AI
        let interpretation = self.interpret_findings(&json_result, question).await?;

        Ok(serde_json::json!({
            "question": question.content,
            "raw_data": json_result,
            "interpretation": interpretation,
            "timestamp": std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)?
                .as_secs()
        }))
    }

    /// Actively seek evidence that contradicts current findings
    async fn seek_contradictions(
        &self,
        findings: &serde_json::Value,
    ) -> Result<Vec<serde_json::Value>> {
        let contradiction_prompt = format!(
            "Given these findings:\n{}\n\n\
            Generate OVSM queries that would find contradictory evidence.\n\
            Think adversarially - what would disprove or complicate these findings?\n\
            Focus on:\n\
            1. Alternative explanations\n\
            2. Missing context\n\
            3. Temporal inconsistencies\n\
            4. Hidden relationships\n\
            5. Statistical outliers",
            serde_json::to_string_pretty(findings)?
        );

        let ai = self.ai_service.lock().await;
        let contradiction_queries = ai.query(&contradiction_prompt).await?;

        // Execute contradiction-seeking queries
        let mut contradictions = Vec::new();
        for query in self.parse_ovsm_queries(contradiction_queries).await? {
            let mut ovsm = self.ovsm_service.lock().await;
            if let Ok(result) = ovsm.execute_code(&query) {
                contradictions.push(value_to_json(result)?);
            }
        }

        Ok(contradictions)
    }

    /// Detect anomalies that warrant deeper investigation
    async fn detect_anomalies(&self) -> Result<Vec<Anomaly>> {
        let graph = self.investigation_graph.lock().await;

        let anomaly_prompt = format!(
            "Analyze this investigation graph for anomalies:\n\
            Nodes: {} total\n\
            Recent patterns: {:?}\n\n\
            Identify:\n\
            1. Statistical outliers\n\
            2. Unexpected connections\n\
            3. Timing irregularities\n\
            4. Behavioral inconsistencies\n\
            5. Emergent patterns\n\n\
            Rate each anomaly's significance (0-1) and suggest investigation approach.",
            graph.nodes.len(),
            graph
                .nodes
                .values()
                .filter(|n| matches!(n.node_type, NodeType::Pattern))
                .take(3)
                .collect::<Vec<_>>()
        );

        let ai = self.ai_service.lock().await;
        let response = ai.query(&anomaly_prompt).await?;

        self.parse_anomalies(response).await
    }

    /// Spawn a sub-investigation for parallel exploration
    async fn spawn_sub_investigation(&self, anomaly: Anomaly) -> Result<()> {
        println!("🚀 Spawning sub-investigation for: {}", anomaly.description);

        let sub_investigator = SubInvestigator {
            id: Uuid::new_v4().to_string(),
            focus_area: anomaly.description.clone(),
            investigation_thread: Arc::new(Mutex::new(InvestigationThread {
                focus: anomaly.focus,
                depth: 0,
                findings: Vec::new(),
                local_hypotheses: Vec::new(),
                status: ThreadStatus::Active,
            })),
            parent_researcher: Arc::new(self.clone()),
        };

        // Launch async sub-investigation
        let investigator = Arc::new(sub_investigator);
        tokio::spawn(async move {
            if let Err(e) = investigator.run_investigation().await {
                eprintln!("Sub-investigation failed: {}", e);
            }
        });

        Ok(())
    }

    /// Check if investigation has converged to stable conclusions
    async fn check_convergence(&self) -> Result<bool> {
        let graph = self.investigation_graph.lock().await;
        let meta = self.metacognition.lock().await;

        // Count high-confidence hypotheses
        let strong_hypotheses = graph
            .nodes
            .values()
            .filter(|n| {
                matches!(n.node_type, NodeType::Hypothesis)
                    && n.confidence > meta.confidence_thresholds.hypothesis_acceptance
            })
            .count();

        // Check for convergence patterns
        let convergence_score = (strong_hypotheses as f64) / (graph.nodes.len() as f64 + 1.0);

        Ok(convergence_score > meta.confidence_thresholds.convergence_declaration)
    }

    /// Adapt investigation strategy based on progress
    async fn adapt_strategy(&self) -> Result<()> {
        let mut meta = self.metacognition.lock().await;
        let graph = self.investigation_graph.lock().await;

        // Calculate progress metrics
        let question_velocity = graph.unexplored_questions.len() as f64;
        let dead_end_ratio = meta.dead_ends.len() as f64 / (graph.nodes.len() as f64 + 1.0);

        // Adapt strategy based on metrics
        if question_velocity > 20.0 {
            // Too many questions, focus on exploitation
            meta.exploration_vs_exploitation = (meta.exploration_vs_exploitation - 0.1).max(0.0);
            meta.investigation_strategy = InvestigationStrategy::DepthFirst;
            println!("📊 Switching to depth-first strategy (too many branches)");
        } else if dead_end_ratio > 0.3 {
            // Many dead ends, try creative approach
            meta.investigation_strategy = InvestigationStrategy::Creative;
            meta.curiosity_level = (meta.curiosity_level + 0.1).min(1.0);
            println!("🎨 Switching to creative strategy (many dead ends)");
        }

        // Adjust curiosity based on discovery rate
        if graph.nodes.len() % 5 == 0 {
            let recent_breakthroughs = meta.breakthrough_indicators.len();
            if recent_breakthroughs == 0 {
                meta.curiosity_level = (meta.curiosity_level + 0.05).min(1.0);
            }
        }

        Ok(())
    }

    /// Update investigation graph with new findings
    async fn update_graph(
        &self,
        findings: serde_json::Value,
        contradictions: Vec<serde_json::Value>,
    ) -> Result<()> {
        let mut graph = self.investigation_graph.lock().await;

        // Add finding node
        let finding_id = Uuid::new_v4().to_string();
        let finding_node = InvestigationNode {
            id: finding_id.clone(),
            node_type: NodeType::Observation,
            data: findings,
            confidence: 0.6, // Initial confidence
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)?
                .as_secs(),
            derived_questions: Vec::new(),
            supporting_evidence: Vec::new(),
            contradicting_evidence: contradictions
                .iter()
                .map(|c| Uuid::new_v4().to_string())
                .collect(),
        };
        graph.nodes.insert(finding_id.clone(), finding_node);

        // Add contradiction nodes
        for contradiction in contradictions {
            let contra_id = Uuid::new_v4().to_string();
            let contra_node = InvestigationNode {
                id: contra_id.clone(),
                node_type: NodeType::Contradiction,
                data: contradiction,
                confidence: 0.5,
                timestamp: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)?
                    .as_secs(),
                derived_questions: Vec::new(),
                supporting_evidence: Vec::new(),
                contradicting_evidence: vec![finding_id.clone()],
            };
            graph.nodes.insert(contra_id.clone(), contra_node);

            // Add edge
            graph.edges.push(InvestigationEdge {
                from: finding_id.clone(),
                to: contra_id,
                relationship: EdgeRelationship::Contradicts,
                strength: 0.7,
            });
        }

        Ok(())
    }

    /// Synthesize investigation into final report
    async fn synthesize_investigation(&self) -> Result<String> {
        let graph = self.investigation_graph.lock().await;
        let meta = self.metacognition.lock().await;

        let synthesis_prompt = format!(
            "Synthesize this investigation:\n\
            Total nodes explored: {}\n\
            Hypotheses formed: {}\n\
            Contradictions found: {}\n\
            Questions generated: {}\n\
            Dead ends: {}\n\
            Convergence points: {}\n\n\
            Key findings:\n{}\n\n\
            Generate a comprehensive investigation report that:\n\
            1. Summarizes main discoveries\n\
            2. Presents strongest hypotheses with confidence levels\n\
            3. Acknowledges uncertainties and contradictions\n\
            4. Suggests future investigation directions\n\
            5. Highlights surprising or anomalous findings",
            graph.nodes.len(),
            graph
                .nodes
                .values()
                .filter(|n| matches!(n.node_type, NodeType::Hypothesis))
                .count(),
            graph
                .nodes
                .values()
                .filter(|n| matches!(n.node_type, NodeType::Contradiction))
                .count(),
            graph.unexplored_questions.len(),
            meta.dead_ends.len(),
            graph.convergence_points.len(),
            self.extract_key_findings(&graph)?
        );

        let ai = self.ai_service.lock().await;
        ai.query(&synthesis_prompt).await
    }

    // Helper methods

    async fn generate_investigation_script(&self, question: &Question) -> Result<String> {
        // Generate OVSM script based on question type
        let script_template = match question.question_type {
            QuestionType::Exploratory => {
                "(do
                  (define target (getTarget))
                  (define signatures (getSignaturesForAddress target :limit 100))
                  (define patterns (analyzePatterns signatures))
                  (define clusters (clusterBehaviors patterns))
                  {:patterns patterns :clusters clusters})"
            }
            QuestionType::Verificatory => {
                "(do
                  (define claim (getClaim))
                  (define evidence (gatherEvidence claim))
                  (define verification (verifyEvidence evidence))
                  {:claim claim :verified verification})"
            }
            QuestionType::Contradictory => {
                "(do
                  (define hypothesis (getHypothesis))
                  (define counter (findCounterExamples hypothesis))
                  (define conflicts (identifyConflicts counter))
                  {:contradictions conflicts})"
            }
            _ => "(do (define data (getData)) {:result data})",
        };

        Ok(script_template.to_string())
    }

    async fn interpret_findings(
        &self,
        data: &serde_json::Value,
        question: &Question,
    ) -> Result<String> {
        let interpret_prompt = format!(
            "Question investigated: {}\n\
            Data found: {}\n\n\
            Provide a concise interpretation focusing on:\n\
            1. Direct answer to the question\n\
            2. Unexpected discoveries\n\
            3. New questions raised\n\
            4. Confidence level in findings",
            question.content,
            serde_json::to_string_pretty(data)?
        );

        let ai = self.ai_service.lock().await;
        ai.query(&interpret_prompt).await
    }

    async fn parse_questions(&self, response: String) -> Result<Vec<Question>> {
        // Parse AI response into Question objects
        let mut questions = Vec::new();

        for line in response.lines() {
            if line.starts_with("- Question:") {
                questions.push(Question {
                    id: Uuid::new_v4().to_string(),
                    content: line.trim_start_matches("- Question:").trim().to_string(),
                    priority: 0.5,
                    question_type: QuestionType::Exploratory,
                    parent_node: None,
                    exploration_depth: 0,
                    expected_impact: 0.5,
                });
            }
        }

        Ok(questions)
    }

    async fn parse_ovsm_queries(&self, response: String) -> Result<Vec<String>> {
        // Extract OVSM queries from AI response
        let mut queries = Vec::new();
        let mut in_query = false;
        let mut current_query = String::new();

        for line in response.lines() {
            if line.starts_with("(do") || line.starts_with("(define") {
                in_query = true;
                current_query.push_str(line);
                current_query.push('\n');
            } else if in_query {
                current_query.push_str(line);
                current_query.push('\n');
                if line.contains("))") {
                    queries.push(current_query.clone());
                    current_query.clear();
                    in_query = false;
                }
            }
        }

        Ok(queries)
    }

    async fn parse_anomalies(&self, response: String) -> Result<Vec<Anomaly>> {
        // Parse anomalies from AI response
        Ok(vec![Anomaly {
            description: "Placeholder anomaly".to_string(),
            significance: 0.5,
            focus: "general".to_string(),
        }])
    }

    fn extract_key_findings(&self, graph: &InvestigationGraph) -> Result<String> {
        let key_findings: Vec<_> = graph
            .nodes
            .values()
            .filter(|n| n.confidence > 0.6)
            .take(5)
            .map(|n| format!("- {:?}: confidence={:.2}", n.node_type, n.confidence))
            .collect();

        Ok(key_findings.join("\n"))
    }

}

/// Convert OVSM Value to JSON (free function to avoid clippy only_used_in_recursion)
fn value_to_json(value: Value) -> Result<serde_json::Value> {
    use serde_json::json;

    let json = match value {
        Value::Null => json!(null),
        Value::Bool(b) => json!(b),
        Value::Int(i) => json!(i),
        Value::Float(f) => json!(f),
        Value::String(s) => json!(s),
        Value::Array(arr) => {
            let json_array: Result<Vec<_>> =
                arr.iter().map(|v| value_to_json(v.clone())).collect();
            json!(json_array?)
        }
        Value::Object(map) => {
            let mut json_map = serde_json::Map::new();
            for (k, v) in map.iter() {
                json_map.insert(k.clone(), value_to_json(v.clone())?);
            }
            json!(json_map)
        }
        _ => json!(format!("{:?}", value)),
    };

    Ok(json)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Anomaly {
    pub description: String,
    pub significance: f64,
    pub focus: String,
}

impl SubInvestigator {
    async fn run_investigation(&self) -> Result<()> {
        let mut thread = self.investigation_thread.lock().await;

        println!("🔬 Sub-investigation started: {}", self.focus_area);

        // Conduct focused investigation
        for depth in 0..5 {
            thread.depth = depth;

            // Generate focused questions
            let question = Question {
                id: Uuid::new_v4().to_string(),
                content: format!(
                    "In context of {}, what is happening at depth {}?",
                    self.focus_area, depth
                ),
                priority: 0.8,
                question_type: QuestionType::Exploratory,
                parent_node: None,
                exploration_depth: depth,
                expected_impact: 0.7,
            };

            // Investigate
            // Note: would implement actual investigation here
            thread.findings.push(serde_json::json!({
                "depth": depth,
                "focus": self.focus_area,
                "data": "placeholder"
            }));

            // Check if we should continue
            if thread.findings.len() > 10 {
                thread.status = ThreadStatus::Converged;
                break;
            }
        }

        println!("✅ Sub-investigation completed: {}", self.focus_area);
        Ok(())
    }
}

// Use real UUID crate
use uuid::Uuid;
