//! Investigation Templates
//!
//! Auto-generate complete OVSM investigation scripts from natural language queries.
//! Uses learned patterns and domain knowledge to create working code.
//!
//! # Usage
//!
//! When a user types a special comment like:
//! ```ovsm
//! ; Investigate: wash trading for wallet ABC123
//! ```
//!
//! The LSP generates a complete investigation script.

use std::collections::HashMap;

/// Template categories for different investigation types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TemplateCategory {
    WalletActivity,
    TokenFlow,
    DexTrading,
    NftActivity,
    WashTrading,
    ArbDetection,
    WhaleTracking,
    Custom,
}

impl TemplateCategory {
    /// Parse category from natural language
    pub fn from_query(query: &str) -> Self {
        let q = query.to_lowercase();

        if q.contains("wash") || q.contains("circular") {
            TemplateCategory::WashTrading
        } else if q.contains("whale") || q.contains("large") {
            TemplateCategory::WhaleTracking
        } else if q.contains("arb") || q.contains("arbitrage") {
            TemplateCategory::ArbDetection
        } else if q.contains("dex") || q.contains("swap") || q.contains("trade") {
            TemplateCategory::DexTrading
        } else if q.contains("nft") || q.contains("collection") || q.contains("mint") {
            TemplateCategory::NftActivity
        } else if q.contains("flow") || q.contains("transfer") || q.contains("send") {
            TemplateCategory::TokenFlow
        } else if q.contains("wallet") || q.contains("account") || q.contains("activity") {
            TemplateCategory::WalletActivity
        } else {
            TemplateCategory::Custom
        }
    }
}

/// Generated investigation script
#[derive(Debug, Clone)]
pub struct InvestigationScript {
    /// Script title
    pub title: String,
    /// Category
    pub category: TemplateCategory,
    /// Generated OVSM code
    pub code: String,
    /// Variables that need to be filled in
    pub parameters: Vec<ScriptParameter>,
    /// Explanation of what the script does
    pub explanation: String,
}

/// A parameter that needs to be filled in
#[derive(Debug, Clone)]
pub struct ScriptParameter {
    /// Parameter name
    pub name: String,
    /// Expected type
    pub param_type: String,
    /// Description
    pub description: String,
    /// Default value if any
    pub default: Option<String>,
}

/// Investigation template generator
pub struct TemplateGenerator {
    /// Custom templates (user-defined)
    custom_templates: HashMap<String, String>,
}

impl TemplateGenerator {
    /// Create a new template generator
    pub fn new() -> Self {
        Self {
            custom_templates: HashMap::new(),
        }
    }

    /// Generate an investigation script from a natural language query
    pub fn generate(&self, query: &str) -> InvestigationScript {
        let category = TemplateCategory::from_query(query);
        let params = self.extract_parameters(query);

        match category {
            TemplateCategory::WalletActivity => self.wallet_activity_template(&params),
            TemplateCategory::TokenFlow => self.token_flow_template(&params),
            TemplateCategory::DexTrading => self.dex_trading_template(&params),
            TemplateCategory::NftActivity => self.nft_activity_template(&params),
            TemplateCategory::WashTrading => self.wash_trading_template(&params),
            TemplateCategory::ArbDetection => self.arb_detection_template(&params),
            TemplateCategory::WhaleTracking => self.whale_tracking_template(&params),
            TemplateCategory::Custom => self.custom_template(query, &params),
        }
    }

    /// Extract parameters from query (wallet addresses, time ranges, etc.)
    fn extract_parameters(&self, query: &str) -> HashMap<String, String> {
        let mut params = HashMap::new();

        // Look for wallet addresses (base58, 32-44 chars)
        let words: Vec<&str> = query.split_whitespace().collect();
        for word in words {
            let clean = word.trim_matches(|c: char| !c.is_alphanumeric());
            if clean.len() >= 32 && clean.len() <= 44 && clean.chars().all(|c| c.is_alphanumeric())
            {
                // Likely a Solana address
                if params.get("wallet").is_none() {
                    params.insert("wallet".to_string(), clean.to_string());
                } else {
                    params.insert("wallet2".to_string(), clean.to_string());
                }
            }
        }

        // Look for time ranges
        if query.contains("24 hour") || query.contains("today") || query.contains("last day") {
            params.insert("time_range".to_string(), "86400".to_string()); // 24 hours in seconds
        } else if query.contains("week") || query.contains("7 day") {
            params.insert("time_range".to_string(), "604800".to_string());
        } else if query.contains("hour") {
            params.insert("time_range".to_string(), "3600".to_string());
        }

        // Look for limits
        if let Some(n) = self.extract_number(query, &["top", "last", "first", "limit"]) {
            params.insert("limit".to_string(), n.to_string());
        }

        params
    }

    /// Extract a number following specific keywords
    fn extract_number(&self, query: &str, keywords: &[&str]) -> Option<usize> {
        let q = query.to_lowercase();
        for keyword in keywords {
            if let Some(pos) = q.find(keyword) {
                let after = &q[pos + keyword.len()..];
                let num_str: String = after
                    .trim()
                    .chars()
                    .take_while(|c| c.is_ascii_digit())
                    .collect();
                if !num_str.is_empty() {
                    return num_str.parse().ok();
                }
            }
        }
        None
    }

    /// Wallet activity investigation template
    fn wallet_activity_template(&self, params: &HashMap<String, String>) -> InvestigationScript {
        let wallet = params
            .get("wallet")
            .cloned()
            .unwrap_or_else(|| "WALLET_ADDRESS".to_string());
        let limit = params
            .get("limit")
            .cloned()
            .unwrap_or_else(|| "50".to_string());

        let code = format!(
            r#"; ============================================
; Wallet Activity Investigation
; ============================================

; Target wallet
(define target-wallet "{}")

; Get recent transactions
(define signatures (getSignaturesForAddress target-wallet :limit {}))

; Fetch transaction details
(define transactions
  (map signatures
    (lambda (sig)
      (getTransaction (get sig "signature")))))

; Analyze transfers
(define transfers
  (filter transactions
    (lambda (tx)
      (not (null? (get tx "meta" "preTokenBalances"))))))

; Calculate statistics
(define total-txs (length signatures))
(define transfer-count (length transfers))

; Output results
(log :message "Wallet Activity Summary")
(log :message "========================")
(log :message "Total transactions:" :value total-txs)
(log :message "Token transfers:" :value transfer-count)

; Return structured result
{{:wallet target-wallet
  :total_transactions total-txs
  :transfers transfer-count
  :recent_signatures (take 10 signatures)}}"#,
            wallet, limit
        );

        InvestigationScript {
            title: "Wallet Activity Investigation".to_string(),
            category: TemplateCategory::WalletActivity,
            code,
            parameters: vec![
                ScriptParameter {
                    name: "wallet".to_string(),
                    param_type: "string".to_string(),
                    description: "Target wallet address".to_string(),
                    default: Some(wallet),
                },
                ScriptParameter {
                    name: "limit".to_string(),
                    param_type: "number".to_string(),
                    description: "Number of transactions to analyze".to_string(),
                    default: Some(limit),
                },
            ],
            explanation: "Fetches recent transactions for a wallet and analyzes transfer patterns."
                .to_string(),
        }
    }

    /// Token flow tracking template
    fn token_flow_template(&self, params: &HashMap<String, String>) -> InvestigationScript {
        let wallet = params
            .get("wallet")
            .cloned()
            .unwrap_or_else(|| "WALLET_ADDRESS".to_string());

        let code = format!(
            r#"; ============================================
; Token Flow Tracking
; ============================================

(define source-wallet "{}")

; Get token accounts
(define token-accounts (getTokenAccountsByOwner source-wallet))

; For each token, trace recent movements
(define token-flows
  (map token-accounts
    (lambda (account)
      (define mint (get account "mint"))
      (define sigs (getSignaturesForAddress (get account "pubkey") :limit 20))
      {{:mint mint
        :balance (get account "amount")
        :recent_txs (length sigs)}})))

; Identify high-activity tokens
(define active-tokens
  (filter token-flows
    (lambda (flow)
      (> (get flow "recent_txs") 5))))

; Output
(log :message "Token Flow Analysis")
(log :message "===================")
(log :message "Total tokens held:" :value (length token-accounts))
(log :message "Active tokens (>5 txs):" :value (length active-tokens))

{{:wallet source-wallet
  :total_tokens (length token-accounts)
  :active_tokens active-tokens
  :all_flows token-flows}}"#,
            wallet
        );

        InvestigationScript {
            title: "Token Flow Tracking".to_string(),
            category: TemplateCategory::TokenFlow,
            code,
            parameters: vec![ScriptParameter {
                name: "wallet".to_string(),
                param_type: "string".to_string(),
                description: "Source wallet to trace flows from".to_string(),
                default: Some(wallet),
            }],
            explanation: "Tracks token holdings and recent transfer activity for a wallet."
                .to_string(),
        }
    }

    /// DEX trading analysis template
    fn dex_trading_template(&self, params: &HashMap<String, String>) -> InvestigationScript {
        let wallet = params
            .get("wallet")
            .cloned()
            .unwrap_or_else(|| "WALLET_ADDRESS".to_string());
        let limit = params
            .get("limit")
            .cloned()
            .unwrap_or_else(|| "100".to_string());

        let code = format!(
            r#"; ============================================
; DEX Trading Analysis
; ============================================

; Known DEX program IDs
(define raydium-amm "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8")
(define orca-whirlpool "whirLbMiicVdio4qvUfM5KAg6Ct8VwpYzGff3uctyCc")
(define jupiter-v6 "JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4")

(define trader "{}")

; Get recent transactions
(define signatures (getSignaturesForAddress trader :limit {}))

; Fetch and filter for DEX interactions
(define dex-txs
  (filter
    (map signatures (lambda (s) (getTransaction (get s "signature"))))
    (lambda (tx)
      (define programs (get tx "transaction" "message" "accountKeys"))
      (or
        (contains? programs raydium-amm)
        (contains? programs orca-whirlpool)
        (contains? programs jupiter-v6)))))

; Analyze trading patterns
(define trade-count (length dex-txs))

(log :message "DEX Trading Analysis")
(log :message "====================")
(log :message "Trader:" :value trader)
(log :message "Total DEX trades:" :value trade-count)
(log :message "Analysis period: last" :value {} :message "transactions")

{{:trader trader
  :dex_trades trade-count
  :transactions dex-txs}}"#,
            wallet, limit, limit
        );

        InvestigationScript {
            title: "DEX Trading Analysis".to_string(),
            category: TemplateCategory::DexTrading,
            code,
            parameters: vec![ScriptParameter {
                name: "wallet".to_string(),
                param_type: "string".to_string(),
                description: "Trader wallet address".to_string(),
                default: Some(wallet),
            }],
            explanation: "Analyzes DEX trading activity across major Solana DEXes.".to_string(),
        }
    }

    /// NFT activity template
    fn nft_activity_template(&self, params: &HashMap<String, String>) -> InvestigationScript {
        let wallet = params
            .get("wallet")
            .cloned()
            .unwrap_or_else(|| "WALLET_ADDRESS".to_string());

        let code = format!(
            r#"; ============================================
; NFT Activity Investigation
; ============================================

(define collector "{}")

; Get token accounts (NFTs are SPL tokens with 0 decimals and supply 1)
(define token-accounts (getTokenAccountsByOwner collector))

; Filter for NFTs (amount = 1, decimals = 0)
(define nfts
  (filter token-accounts
    (lambda (account)
      (and
        (= (get account "amount") 1)
        (= (get account "decimals") 0)))))

; Get recent NFT transactions
(define nft-activity
  (map nfts
    (lambda (nft)
      (define mint (get nft "mint"))
      (define sigs (getSignaturesForAddress mint :limit 5))
      {{:mint mint
        :recent_activity (length sigs)}})))

(log :message "NFT Collection Analysis")
(log :message "=======================")
(log :message "Collector:" :value collector)
(log :message "NFTs held:" :value (length nfts))

{{:collector collector
  :nft_count (length nfts)
  :nfts nfts
  :activity nft-activity}}"#,
            wallet
        );

        InvestigationScript {
            title: "NFT Activity Investigation".to_string(),
            category: TemplateCategory::NftActivity,
            code,
            parameters: vec![ScriptParameter {
                name: "wallet".to_string(),
                param_type: "string".to_string(),
                description: "Collector wallet address".to_string(),
                default: Some(wallet),
            }],
            explanation: "Analyzes NFT holdings and recent activity for a collector.".to_string(),
        }
    }

    /// Wash trading detection template
    fn wash_trading_template(&self, params: &HashMap<String, String>) -> InvestigationScript {
        let wallet = params
            .get("wallet")
            .cloned()
            .unwrap_or_else(|| "WALLET_ADDRESS".to_string());

        let code = format!(
            r#"; ============================================
; Wash Trading Detection
; ============================================

(define suspect "{}")

; Get transaction history
(define signatures (getSignaturesForAddress suspect :limit 100))
(define transactions (map signatures (lambda (s) (getTransaction (get s "signature")))))

; Extract unique counterparties
(define counterparties
  (reduce transactions
    (lambda (acc tx)
      (define accounts (get tx "transaction" "message" "accountKeys"))
      (concat acc (filter accounts (lambda (a) (!= a suspect)))))
    []))

; Find repeated counterparties (potential wash trading)
(define party-counts
  (reduce counterparties
    (lambda (acc party)
      (set! acc party (+ 1 (get acc party 0)))
      acc)
    {{}}))

; Flag suspicious patterns (same counterparty > 10 times)
(define suspicious
  (filter (keys party-counts)
    (lambda (party)
      (> (get party-counts party) 10))))

(define circular-risk
  (if (> (length suspicious) 0) "HIGH" "LOW"))

(log :message "Wash Trading Analysis")
(log :message "=====================")
(log :message "Suspect wallet:" :value suspect)
(log :message "Unique counterparties:" :value (length (keys party-counts)))
(log :message "Suspicious (>10 interactions):" :value (length suspicious))
(log :message "Circular trading risk:" :value circular-risk)

{{:suspect suspect
  :total_transactions (length transactions)
  :unique_counterparties (length (keys party-counts))
  :suspicious_counterparties suspicious
  :risk_level circular-risk}}"#,
            wallet
        );

        InvestigationScript {
            title: "Wash Trading Detection".to_string(),
            category: TemplateCategory::WashTrading,
            code,
            parameters: vec![ScriptParameter {
                name: "wallet".to_string(),
                param_type: "string".to_string(),
                description: "Suspect wallet address".to_string(),
                default: Some(wallet),
            }],
            explanation: "Detects potential wash trading by analyzing counterparty patterns."
                .to_string(),
        }
    }

    /// Arbitrage detection template
    fn arb_detection_template(&self, params: &HashMap<String, String>) -> InvestigationScript {
        let wallet = params
            .get("wallet")
            .cloned()
            .unwrap_or_else(|| "WALLET_ADDRESS".to_string());

        let code = format!(
            r#"; ============================================
; Arbitrage Detection
; ============================================

(define trader "{}")

; Get recent transactions
(define signatures (getSignaturesForAddress trader :limit 200))
(define transactions (map signatures (lambda (s) (getTransaction (get s "signature")))))

; Look for multi-DEX transactions in quick succession
(define rapid-trades
  (filter transactions
    (lambda (tx)
      ; Check for multiple program interactions
      (define programs (get tx "transaction" "message" "programIds"))
      (> (length programs) 2))))

; Analyze for profitability patterns
(define arb-candidates
  (filter rapid-trades
    (lambda (tx)
      (define pre (get tx "meta" "preBalances"))
      (define post (get tx "meta" "postBalances"))
      (> (get post 0) (get pre 0))))) ; Profit check

(log :message "Arbitrage Analysis")
(log :message "==================")
(log :message "Trader:" :value trader)
(log :message "Total transactions:" :value (length transactions))
(log :message "Multi-hop trades:" :value (length rapid-trades))
(log :message "Profitable arbs:" :value (length arb-candidates))

{{:trader trader
  :total_txs (length transactions)
  :multi_hop_trades (length rapid-trades)
  :profitable_arbs (length arb-candidates)
  :arb_rate (/ (length arb-candidates) (max 1 (length rapid-trades)))}}"#,
            wallet
        );

        InvestigationScript {
            title: "Arbitrage Detection".to_string(),
            category: TemplateCategory::ArbDetection,
            code,
            parameters: vec![ScriptParameter {
                name: "wallet".to_string(),
                param_type: "string".to_string(),
                description: "Trader wallet address".to_string(),
                default: Some(wallet),
            }],
            explanation:
                "Detects arbitrage activity by analyzing multi-hop trades and profitability."
                    .to_string(),
        }
    }

    /// Whale tracking template
    fn whale_tracking_template(&self, params: &HashMap<String, String>) -> InvestigationScript {
        let wallet = params
            .get("wallet")
            .cloned()
            .unwrap_or_else(|| "WALLET_ADDRESS".to_string());

        let code = format!(
            r#"; ============================================
; Whale Tracking
; ============================================

(define whale "{}")

; Get balance and token holdings
(define sol-balance (getBalance whale))
(define token-accounts (getTokenAccountsByOwner whale))

; Calculate total holdings value (simplified)
(define total-tokens (length token-accounts))
(define large-holdings
  (filter token-accounts
    (lambda (account)
      (> (get account "amount") 1000000)))) ; > 1M tokens

; Get recent large transfers
(define signatures (getSignaturesForAddress whale :limit 50))
(define transactions (map signatures (lambda (s) (getTransaction (get s "signature")))))

(log :message "Whale Profile")
(log :message "=============")
(log :message "Address:" :value whale)
(log :message "SOL Balance:" :value (/ sol-balance 1000000000))
(log :message "Token types held:" :value total-tokens)
(log :message "Large holdings (>1M):" :value (length large-holdings))
(log :message "Recent transactions:" :value (length transactions))

{{:whale whale
  :sol_balance (/ sol-balance 1000000000)
  :token_count total-tokens
  :large_holdings large-holdings
  :recent_activity (length transactions)}}"#,
            wallet
        );

        InvestigationScript {
            title: "Whale Tracking".to_string(),
            category: TemplateCategory::WhaleTracking,
            code,
            parameters: vec![ScriptParameter {
                name: "wallet".to_string(),
                param_type: "string".to_string(),
                description: "Whale wallet address".to_string(),
                default: Some(wallet),
            }],
            explanation: "Profiles a whale wallet's holdings and recent activity.".to_string(),
        }
    }

    /// Custom/generic template
    fn custom_template(
        &self,
        query: &str,
        params: &HashMap<String, String>,
    ) -> InvestigationScript {
        let wallet = params
            .get("wallet")
            .cloned()
            .unwrap_or_else(|| "WALLET_ADDRESS".to_string());

        let code = format!(
            r#"; ============================================
; Custom Investigation: {}
; ============================================

; Target
(define target "{}")

; Get basic info
(define balance (getBalance target))
(define signatures (getSignaturesForAddress target :limit 20))

; Basic analysis
(log :message "Investigation Target:" :value target)
(log :message "Balance (lamports):" :value balance)
(log :message "Recent transactions:" :value (length signatures))

; TODO: Add custom investigation logic
; Based on your query: "{}"

{{:target target
  :balance balance
  :tx_count (length signatures)}}"#,
            query.trim(),
            wallet,
            query.trim()
        );

        InvestigationScript {
            title: format!("Custom: {}", query),
            category: TemplateCategory::Custom,
            code,
            parameters: vec![ScriptParameter {
                name: "wallet".to_string(),
                param_type: "string".to_string(),
                description: "Target wallet address".to_string(),
                default: Some(wallet),
            }],
            explanation: format!("Custom investigation script for: {}", query),
        }
    }

    /// Add a custom template
    pub fn add_custom_template(&mut self, name: &str, template: &str) {
        self.custom_templates
            .insert(name.to_string(), template.to_string());
    }
}

impl Default for TemplateGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse investigation comment from code
pub fn parse_investigation_comment(line: &str) -> Option<String> {
    let trimmed = line.trim();
    if trimmed.starts_with("; Investigate:") || trimmed.starts_with(";; Investigate:") {
        let query = trimmed
            .trim_start_matches(';')
            .trim()
            .trim_start_matches("Investigate:")
            .trim();
        if !query.is_empty() {
            return Some(query.to_string());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_category_detection() {
        assert_eq!(
            TemplateCategory::from_query("wash trading for wallet"),
            TemplateCategory::WashTrading
        );
        assert_eq!(
            TemplateCategory::from_query("find whale activity"),
            TemplateCategory::WhaleTracking
        );
        assert_eq!(
            TemplateCategory::from_query("dex trades today"),
            TemplateCategory::DexTrading
        );
        assert_eq!(
            TemplateCategory::from_query("nft mints"),
            TemplateCategory::NftActivity
        );
    }

    #[test]
    fn test_parameter_extraction() {
        let gen = TemplateGenerator::new();
        let params = gen.extract_parameters("analyze wallet ABC123DEF456 for last 24 hours");
        // Note: ABC123DEF456 is only 12 chars, not a valid Solana address
        assert!(params.get("time_range").is_some());
    }

    #[test]
    fn test_parse_investigation_comment() {
        assert_eq!(
            parse_investigation_comment("; Investigate: wash trading"),
            Some("wash trading".to_string())
        );
        assert_eq!(
            parse_investigation_comment(";; Investigate: whale tracking for ABC"),
            Some("whale tracking for ABC".to_string())
        );
        assert_eq!(parse_investigation_comment("; Just a normal comment"), None);
    }

    #[test]
    fn test_template_generation() {
        let gen = TemplateGenerator::new();
        let script = gen.generate("wash trading detection");
        assert_eq!(script.category, TemplateCategory::WashTrading);
        assert!(script.code.contains("Wash Trading"));
    }

    #[test]
    fn test_script_has_parameters() {
        let gen = TemplateGenerator::new();
        let script = gen.generate("wallet activity");
        assert!(!script.parameters.is_empty());
        assert!(script.parameters.iter().any(|p| p.name == "wallet"));
    }
}
