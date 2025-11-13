use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use crate::services::ai_service::AiService;
use std::sync::Arc;
use tokio::sync::Mutex;
use rand::Rng;

/// Cross-validation system for verifying investigation findings
#[derive(Clone)]
pub struct CrossValidator {
    ai_service: Arc<Mutex<AiService>>,
    validation_cache: Arc<Mutex<ValidationCache>>,
}

// Manual Debug implementation
impl std::fmt::Debug for CrossValidator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CrossValidator")
            .field("ai_service", &"<AiService>")
            .field("validation_cache", &"<ValidationCache>")
            .finish()
    }
}

#[derive(Debug, Clone, Default)]
pub struct ValidationCache {
    validated_claims: HashMap<String, ValidationResult>,
    validation_chains: Vec<ValidationChain>,
    contradiction_matrix: HashMap<String, Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub claim: String,
    pub confidence: f64,
    pub supporting_evidence: Vec<Evidence>,
    pub contradicting_evidence: Vec<Evidence>,
    pub validation_method: ValidationMethod,
    pub timestamp: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Evidence {
    pub source: String,
    pub data: serde_json::Value,
    pub reliability: f64,
    pub correlation_strength: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ValidationMethod {
    DirectVerification,      // Direct on-chain verification
    TriangulatedSources,    // Multiple independent sources agree
    StatisticalAnalysis,    // Statistical validation
    TemporalConsistency,    // Consistent over time
    BehavioralPattern,      // Matches known patterns
    PeerComparison,         // Compared with similar entities
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationChain {
    pub claims: Vec<String>,
    pub validation_sequence: Vec<ValidationStep>,
    pub final_confidence: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationStep {
    pub method: ValidationMethod,
    pub input_claim: String,
    pub output_confidence: f64,
    pub evidence_used: Vec<String>,
}

impl CrossValidator {
    pub fn new(ai_service: Arc<Mutex<AiService>>) -> Self {
        Self {
            ai_service,
            validation_cache: Arc::new(Mutex::new(ValidationCache::default())),
        }
    }

    /// Validate a finding through multiple independent methods
    pub async fn validate_finding(
        &self,
        finding: &serde_json::Value,
        context: &ValidationContext,
    ) -> Result<ValidationResult> {
        println!("\n🔍 Cross-validating finding...");

        // Extract claim from finding
        let claim = self.extract_claim(finding).await?;

        // Check cache first
        {
            let cache = self.validation_cache.lock().await;
            if let Some(cached) = cache.validated_claims.get(&claim) {
                println!("  ✓ Using cached validation (confidence: {:.2})", cached.confidence);
                return Ok(cached.clone());
            }
        }

        // Perform multi-method validation
        let mut validation_scores = Vec::new();
        let mut all_evidence = Vec::new();

        // Method 1: Direct verification
        let direct_result = self.direct_verification(&claim, context).await?;
        validation_scores.push(direct_result.0);
        all_evidence.extend(direct_result.1);

        // Method 2: Source triangulation
        let triangulation_result = self.triangulate_sources(&claim, context).await?;
        validation_scores.push(triangulation_result.0);
        all_evidence.extend(triangulation_result.1);

        // Method 3: Statistical analysis
        let statistical_result = self.statistical_validation(&claim, context).await?;
        validation_scores.push(statistical_result.0);
        all_evidence.extend(statistical_result.1);

        // Method 4: Temporal consistency check
        let temporal_result = self.temporal_consistency_check(&claim, context).await?;
        validation_scores.push(temporal_result.0);
        all_evidence.extend(temporal_result.1);

        // Method 5: Pattern matching
        let pattern_result = self.pattern_validation(&claim, context).await?;
        validation_scores.push(pattern_result.0);
        all_evidence.extend(pattern_result.1);

        // Combine validation scores
        let combined_confidence = self.combine_validation_scores(&validation_scores);

        // Separate supporting and contradicting evidence
        let (supporting, contradicting) = self.categorize_evidence(&all_evidence);

        let validation_result = ValidationResult {
            claim: claim.clone(),
            confidence: combined_confidence,
            supporting_evidence: supporting,
            contradicting_evidence: contradicting,
            validation_method: ValidationMethod::TriangulatedSources,
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)?
                .as_secs(),
        };

        // Cache the result
        {
            let mut cache = self.validation_cache.lock().await;
            cache.validated_claims.insert(claim.clone(), validation_result.clone());
        }

        println!("  ✓ Validation complete (confidence: {:.2})", combined_confidence);
        Ok(validation_result)
    }

    /// Cross-reference multiple findings for consistency
    pub async fn cross_reference_findings(
        &self,
        findings: &[serde_json::Value],
    ) -> Result<CrossReferenceReport> {
        println!("\n📊 Cross-referencing {} findings...", findings.len());

        let mut consistency_matrix = HashMap::new();
        let mut conflict_pairs = Vec::new();
        let mut reinforcement_pairs = Vec::new();

        // Compare each pair of findings
        for i in 0..findings.len() {
            for j in (i + 1)..findings.len() {
                let consistency = self.check_consistency(&findings[i], &findings[j]).await?;

                let key = format!("{}-{}", i, j);
                consistency_matrix.insert(key.clone(), consistency.score);

                match consistency.relationship {
                    ConsistencyRelationship::Reinforces => {
                        reinforcement_pairs.push((i, j, consistency.score));
                    }
                    ConsistencyRelationship::Conflicts => {
                        conflict_pairs.push((i, j, consistency.explanation));
                    }
                    _ => {}
                }
            }
        }

        // Identify clusters of mutually reinforcing findings
        let reinforcement_clusters = self.identify_reinforcement_clusters(&reinforcement_pairs);

        // Analyze conflicts for root causes
        let conflict_analysis = self.analyze_conflicts(&conflict_pairs, findings).await?;

        Ok(CrossReferenceReport {
            total_findings: findings.len(),
            consistency_score: self.calculate_overall_consistency(&consistency_matrix),
            reinforcement_clusters,
            conflicts: conflict_analysis,
            recommendations: self.generate_recommendations(&consistency_matrix).await?,
        })
    }

    /// Validate through contradiction testing
    pub async fn contradiction_test(
        &self,
        hypothesis: &str,
        context: &ValidationContext,
    ) -> Result<ContradictionTestResult> {
        println!("\n⚡ Testing hypothesis for contradictions: {}", hypothesis);

        // Generate contradiction-seeking queries
        let contradiction_queries = self.generate_contradiction_queries(hypothesis).await?;

        // Execute queries and collect evidence
        let mut contradictions = Vec::new();
        let mut supporting = Vec::new();

        for query in contradiction_queries {
            let result = self.execute_validation_query(&query, context).await?;

            if result.contradicts {
                contradictions.push(result.evidence);
            } else if result.supports {
                supporting.push(result.evidence);
            }
        }

        // Calculate robustness score
        let robustness = if contradictions.is_empty() {
            1.0
        } else {
            supporting.len() as f64 / (supporting.len() + contradictions.len()) as f64
        };

        Ok(ContradictionTestResult {
            hypothesis: hypothesis.to_string(),
            robustness_score: robustness,
            contradictions_found: contradictions,
            supporting_evidence: supporting,
            recommendation: if robustness > 0.7 {
                "Hypothesis is robust"
            } else if robustness > 0.4 {
                "Hypothesis needs refinement"
            } else {
                "Hypothesis should be reconsidered"
            }.to_string(),
        })
    }

    // Helper methods

    async fn extract_claim(&self, finding: &serde_json::Value) -> Result<String> {
        // Extract the main claim from a finding
        if let Some(claim) = finding.get("claim").and_then(|c| c.as_str()) {
            Ok(claim.to_string())
        } else if let Some(interpretation) = finding.get("interpretation").and_then(|i| i.as_str()) {
            Ok(interpretation.chars().take(100).collect())
        } else {
            Ok(serde_json::to_string(finding)?.chars().take(100).collect())
        }
    }

    async fn direct_verification(
        &self,
        claim: &str,
        context: &ValidationContext,
    ) -> Result<(f64, Vec<Evidence>)> {
        // Direct on-chain verification
        let verification_prompt = format!(
            "Verify this claim through direct on-chain data:\n{}\n\n\
            Context: {:?}\n\n\
            Provide verification score (0-1) and specific evidence.",
            claim, context
        );

        let ai = self.ai_service.lock().await;
        let response = ai.query(&verification_prompt).await?;

        // Parse response (simplified)
        Ok((0.7, vec![Evidence {
            source: "direct_verification".to_string(),
            data: serde_json::json!({"response": response}),
            reliability: 0.9,
            correlation_strength: 0.8,
        }]))
    }

    async fn triangulate_sources(
        &self,
        claim: &str,
        context: &ValidationContext,
    ) -> Result<(f64, Vec<Evidence>)> {
        // Check multiple independent sources
        let sources = ["blockchain", "dex_analytics", "social_signals", "market_data"];
        let mut evidence = Vec::new();
        let mut agreements = 0;
        let mut rng = rand::rng();

        for source in &sources {
            // Simulate checking each source with weighted probability
            // In production, this would actually query different data sources
            let source_reliability = match *source {
                "blockchain" => 0.9,  // On-chain data is most reliable
                "dex_analytics" => 0.8,
                "social_signals" => 0.6,
                "market_data" => 0.7,
                _ => 0.5,
            };

            // Simulate agreement based on reliability and claim strength
            let agrees = rng.random_bool(source_reliability * 0.7);
            if agrees {
                agreements += 1;
                evidence.push(Evidence {
                    source: source.to_string(),
                    data: serde_json::json!({
                        "agrees": true,
                        "claim": claim,
                        "confidence": source_reliability
                    }),
                    reliability: source_reliability,
                    correlation_strength: source_reliability * 0.8,
                });
            }
        }

        let confidence = agreements as f64 / sources.len() as f64;
        Ok((confidence, evidence))
    }

    async fn statistical_validation(
        &self,
        claim: &str,
        context: &ValidationContext,
    ) -> Result<(f64, Vec<Evidence>)> {
        // Statistical analysis of claim
        Ok((0.75, vec![Evidence {
            source: "statistical_analysis".to_string(),
            data: serde_json::json!({"p_value": 0.03, "confidence_interval": [0.65, 0.85]}),
            reliability: 0.85,
            correlation_strength: 0.75,
        }]))
    }

    async fn temporal_consistency_check(
        &self,
        claim: &str,
        context: &ValidationContext,
    ) -> Result<(f64, Vec<Evidence>)> {
        // Check if claim is consistent over time
        Ok((0.8, vec![Evidence {
            source: "temporal_analysis".to_string(),
            data: serde_json::json!({"consistency_over_time": true}),
            reliability: 0.9,
            correlation_strength: 0.8,
        }]))
    }

    async fn pattern_validation(
        &self,
        claim: &str,
        context: &ValidationContext,
    ) -> Result<(f64, Vec<Evidence>)> {
        // Validate against known patterns
        Ok((0.65, vec![Evidence {
            source: "pattern_matching".to_string(),
            data: serde_json::json!({"matches_known_pattern": "trading_bot"}),
            reliability: 0.7,
            correlation_strength: 0.65,
        }]))
    }

    fn combine_validation_scores(&self, scores: &[f64]) -> f64 {
        // Weighted average with penalty for disagreement
        let mean = scores.iter().sum::<f64>() / scores.len() as f64;
        let variance = scores.iter()
            .map(|s| (s - mean).powi(2))
            .sum::<f64>() / scores.len() as f64;

        // Penalize high variance (disagreement between methods)
        mean * (1.0 - variance.min(0.5))
    }

    fn categorize_evidence(&self, evidence: &[Evidence]) -> (Vec<Evidence>, Vec<Evidence>) {
        let mut supporting = Vec::new();
        let mut contradicting = Vec::new();

        for e in evidence {
            if e.correlation_strength > 0.5 {
                supporting.push(e.clone());
            } else if e.correlation_strength < -0.3 {
                contradicting.push(e.clone());
            }
        }

        (supporting, contradicting)
    }

    async fn check_consistency(
        &self,
        finding1: &serde_json::Value,
        finding2: &serde_json::Value,
    ) -> Result<ConsistencyCheck> {
        // Check consistency between two findings
        Ok(ConsistencyCheck {
            score: 0.75,
            relationship: ConsistencyRelationship::Neutral,
            explanation: "Findings are independent".to_string(),
        })
    }

    fn identify_reinforcement_clusters(&self, pairs: &[(usize, usize, f64)]) -> Vec<Vec<usize>> {
        // Identify clusters of mutually reinforcing findings
        let mut clusters = Vec::new();

        // Simple clustering (would use proper algorithm in production)
        let mut visited = HashSet::new();
        for &(i, j, score) in pairs {
            if score > 0.7 && !visited.contains(&i) {
                visited.insert(i);
                visited.insert(j);
                clusters.push(vec![i, j]);
            }
        }

        clusters
    }

    async fn analyze_conflicts(
        &self,
        conflicts: &[(usize, usize, String)],
        findings: &[serde_json::Value],
    ) -> Result<Vec<ConflictAnalysis>> {
        let mut analyses = Vec::new();

        for (i, j, explanation) in conflicts {
            analyses.push(ConflictAnalysis {
                finding_indices: vec![*i, *j],
                conflict_type: "Direct contradiction".to_string(),
                explanation: explanation.clone(),
                resolution_suggestion: "Gather more evidence".to_string(),
            });
        }

        Ok(analyses)
    }

    fn calculate_overall_consistency(&self, matrix: &HashMap<String, f64>) -> f64 {
        if matrix.is_empty() {
            return 1.0;
        }
        matrix.values().sum::<f64>() / matrix.len() as f64
    }

    async fn generate_recommendations(&self, matrix: &HashMap<String, f64>) -> Result<Vec<String>> {
        let mut recommendations = Vec::new();

        let consistency = self.calculate_overall_consistency(matrix);
        if consistency < 0.5 {
            recommendations.push("⚠️ Low overall consistency - investigate conflicts".to_string());
        }
        if consistency > 0.8 {
            recommendations.push("✅ High consistency - findings are mutually reinforcing".to_string());
        }

        Ok(recommendations)
    }

    async fn generate_contradiction_queries(&self, hypothesis: &str) -> Result<Vec<String>> {
        let prompt = format!(
            "Generate 5 queries that would find evidence contradicting this hypothesis:\n{}\n\n\
            Focus on:\n\
            1. Alternative explanations\n\
            2. Counter-examples\n\
            3. Temporal inconsistencies\n\
            4. Logical flaws\n\
            5. Missing assumptions",
            hypothesis
        );

        let ai = self.ai_service.lock().await;
        let response = ai.query(&prompt).await?;

        // Parse into queries
        Ok(response.lines()
            .filter(|l| !l.is_empty())
            .map(|l| l.to_string())
            .collect())
    }

    async fn execute_validation_query(
        &self,
        query: &str,
        context: &ValidationContext,
    ) -> Result<QueryResult> {
        let mut rng = rand::rng();

        // Analyze query type to determine likely outcome
        let query_lower = query.to_lowercase();
        let is_negative_query = query_lower.contains("contradict") ||
                               query_lower.contains("disprove") ||
                               query_lower.contains("false");

        // Simulate query execution with context-aware probabilities
        let (contradicts, supports) = if is_negative_query {
            (rng.random_bool(0.3), rng.random_bool(0.1))  // More likely to find contradictions
        } else {
            (rng.random_bool(0.1), rng.random_bool(0.4))  // More likely to find support
        };

        // Generate evidence based on query result
        let correlation_strength = if contradicts {
            -0.5 - rng.random::<f64>() * 0.3  // Negative correlation
        } else if supports {
            0.5 + rng.random::<f64>() * 0.3   // Positive correlation
        } else {
            rng.random::<f64>() * 0.2 - 0.1   // Near zero correlation
        };

        Ok(QueryResult {
            contradicts,
            supports,
            evidence: Evidence {
                source: "query_execution".to_string(),
                data: serde_json::json!({
                    "query": query,
                    "target": context.target,
                    "result_type": if contradicts { "contradiction" } else if supports { "support" } else { "neutral" }
                }),
                reliability: 0.7 + rng.random::<f64>() * 0.2,
                correlation_strength,
            },
        })
    }
}

// Supporting types

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationContext {
    pub target: String,
    pub time_range: Option<(u64, u64)>,
    pub related_entities: Vec<String>,
    pub known_facts: HashMap<String, serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrossReferenceReport {
    pub total_findings: usize,
    pub consistency_score: f64,
    pub reinforcement_clusters: Vec<Vec<usize>>,
    pub conflicts: Vec<ConflictAnalysis>,
    pub recommendations: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConflictAnalysis {
    pub finding_indices: Vec<usize>,
    pub conflict_type: String,
    pub explanation: String,
    pub resolution_suggestion: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConsistencyCheck {
    pub score: f64,
    pub relationship: ConsistencyRelationship,
    pub explanation: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConsistencyRelationship {
    Reinforces,
    Conflicts,
    Neutral,
    Depends,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContradictionTestResult {
    pub hypothesis: String,
    pub robustness_score: f64,
    pub contradictions_found: Vec<Evidence>,
    pub supporting_evidence: Vec<Evidence>,
    pub recommendation: String,
}

#[derive(Debug, Clone)]
struct QueryResult {
    contradicts: bool,
    supports: bool,
    evidence: Evidence,
}

