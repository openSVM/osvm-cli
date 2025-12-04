//! AI-powered code completion for OVSM
//!
//! Integrates with LLM APIs to provide context-aware code suggestions
//! for blockchain investigation patterns and OVSM LISP code.
//!
//! # Configuration
//!
//! Set these environment variables:
//! - `OVSM_AI_URL`: LLM API endpoint (default: http://localhost:11434/v1/chat/completions for Ollama)
//! - `OVSM_AI_KEY`: API key (use "ollama" for local Ollama)
//! - `OVSM_AI_MODEL`: Model name (default: "qwen2.5-coder:7b" for Ollama)

use serde::{Deserialize, Serialize};
use std::env;
use std::time::Duration;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind,
};

/// AI completion configuration
#[derive(Debug, Clone)]
pub struct AiConfig {
    /// API endpoint URL
    pub url: String,
    /// API key
    pub api_key: String,
    /// Model name
    pub model: String,
    /// Whether AI completions are enabled
    pub enabled: bool,
}

impl Default for AiConfig {
    fn default() -> Self {
        let url = env::var("OVSM_AI_URL")
            .unwrap_or_else(|_| "http://localhost:11434/v1/chat/completions".to_string());
        let api_key = env::var("OVSM_AI_KEY").unwrap_or_else(|_| "ollama".to_string());
        let model = env::var("OVSM_AI_MODEL").unwrap_or_else(|_| "qwen2.5-coder:7b".to_string());
        let enabled = env::var("OVSM_AI_ENABLED")
            .map(|v| v == "1" || v.to_lowercase() == "true")
            .unwrap_or(false); // Disabled by default

        Self {
            url,
            api_key,
            model,
            enabled,
        }
    }
}

/// Request to the LLM API
#[derive(Debug, Serialize)]
struct ChatRequest {
    model: String,
    messages: Vec<ChatMessage>,
    max_tokens: u32,
    temperature: f32,
    stop: Option<Vec<String>>,
}

/// Chat message
#[derive(Debug, Serialize)]
struct ChatMessage {
    role: String,
    content: String,
}

/// Response from the LLM API
#[derive(Debug, Deserialize)]
struct ChatResponse {
    choices: Vec<ChatChoice>,
}

#[derive(Debug, Deserialize)]
struct ChatChoice {
    message: ResponseMessage,
}

#[derive(Debug, Deserialize)]
struct ResponseMessage {
    content: String,
}

/// AI completion provider
pub struct AiCompletionProvider {
    config: AiConfig,
    client: reqwest::Client,
}

impl AiCompletionProvider {
    /// Create a new AI completion provider
    pub fn new() -> Self {
        Self {
            config: AiConfig::default(),
            client: reqwest::Client::builder()
                .timeout(Duration::from_secs(10))
                .build()
                .unwrap_or_default(),
        }
    }

    /// Check if AI completions are enabled
    pub fn is_enabled(&self) -> bool {
        self.config.enabled
    }

    /// Get AI-powered completions for the given context
    pub async fn get_completions(
        &self,
        prefix: &str,
        document_content: &str,
        cursor_line: u32,
    ) -> Vec<CompletionItem> {
        if !self.config.enabled {
            return vec![];
        }

        // Build context for the LLM
        let context = self.build_context(prefix, document_content, cursor_line);

        // Call the LLM API
        match self.call_llm(&context).await {
            Ok(suggestions) => self.parse_suggestions(&suggestions, prefix),
            Err(e) => {
                tracing::warn!("AI completion failed: {}", e);
                vec![]
            }
        }
    }

    /// Build context for the LLM
    fn build_context(&self, prefix: &str, document_content: &str, cursor_line: u32) -> String {
        // Get relevant lines around the cursor
        let lines: Vec<&str> = document_content.lines().collect();
        let start_line = cursor_line.saturating_sub(10) as usize;
        let end_line = (cursor_line as usize + 5).min(lines.len());
        let context_lines: Vec<&str> = lines[start_line..end_line].to_vec();

        format!(
            r#"You are an expert OVSM LISP programmer specializing in Solana blockchain analysis.

OVSM is a LISP dialect for blockchain automation with these key features:
- S-expression syntax: (function arg1 arg2)
- Built-ins: define, defun, if, let, for, while, mapcar, filter, reduce
- Blockchain: getBalance, getSignaturesForAddress, getTransaction
- Data: Arrays [1 2 3], Objects {{:key "value"}}

Context (code around cursor):
```ovsm
{}
```

Current line prefix: "{}"

Provide 3 OVSM code completions. Format each as:
COMPLETION: <code>
DESCRIPTION: <brief description>

Focus on:
1. Common blockchain patterns (wallet analysis, token transfers, DEX trades)
2. Syntactically correct OVSM LISP
3. Practical investigation patterns"#,
            context_lines.join("\n"),
            prefix
        )
    }

    /// Call the LLM API
    async fn call_llm(
        &self,
        prompt: &str,
    ) -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
        let request = ChatRequest {
            model: self.config.model.clone(),
            messages: vec![ChatMessage {
                role: "user".to_string(),
                content: prompt.to_string(),
            }],
            max_tokens: 500,
            temperature: 0.3,
            stop: Some(vec!["```".to_string()]),
        };

        let response = self
            .client
            .post(&self.config.url)
            .header("Authorization", format!("Bearer {}", self.config.api_key))
            .header("Content-Type", "application/json")
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(format!("LLM API error: {}", response.status()).into());
        }

        let chat_response: ChatResponse = response.json().await?;

        if let Some(choice) = chat_response.choices.first() {
            Ok(choice.message.content.clone())
        } else {
            Err("No response from LLM".into())
        }
    }

    /// Parse LLM response into completion items
    fn parse_suggestions(&self, response: &str, _prefix: &str) -> Vec<CompletionItem> {
        let mut items = Vec::new();
        let mut current_completion: Option<String> = None;
        let mut current_description: Option<String> = None;

        for line in response.lines() {
            let line = line.trim();

            if line.starts_with("COMPLETION:") {
                // Save previous completion if exists
                if let Some(code) = current_completion.take() {
                    items.push(
                        self.create_completion_item(&code, current_description.take().as_deref()),
                    );
                }
                current_completion =
                    Some(line.trim_start_matches("COMPLETION:").trim().to_string());
            } else if line.starts_with("DESCRIPTION:") {
                current_description =
                    Some(line.trim_start_matches("DESCRIPTION:").trim().to_string());
            }
        }

        // Don't forget the last one
        if let Some(code) = current_completion {
            items.push(self.create_completion_item(&code, current_description.as_deref()));
        }

        items
    }

    /// Create a completion item from code and description
    fn create_completion_item(&self, code: &str, description: Option<&str>) -> CompletionItem {
        CompletionItem {
            label: truncate_label(code, 50),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("AI suggestion".to_string()),
            documentation: description.map(|d| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("{}\n\n```ovsm\n{}\n```", d, code),
                })
            }),
            insert_text: Some(code.to_string()),
            sort_text: Some("zzz".to_string()), // Sort AI suggestions last
            ..Default::default()
        }
    }
}

/// Truncate label for display
fn truncate_label(s: &str, max_len: usize) -> String {
    let s = s.lines().next().unwrap_or(s);
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[..max_len - 3])
    }
}

/// Pre-defined blockchain investigation snippets
/// These are always available, even without AI
pub fn get_blockchain_snippets() -> Vec<CompletionItem> {
    vec![
        CompletionItem {
            label: "wallet-analysis".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("Analyze wallet transactions".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Analyze recent transactions for a wallet address".to_string(),
            })),
            insert_text: Some(
                r#"(define wallet "$1")
(define signatures (getSignaturesForAddress wallet :limit 100))
(define transactions
  (mapcar (lambda (sig)
            (getTransaction (get sig "signature")))
          signatures))
(log :message "Found transactions:" :value (length transactions))"#
                    .to_string(),
            ),
            insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "token-transfers".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("Find token transfers".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Find all token transfers for a wallet".to_string(),
            })),
            insert_text: Some(
                r#"(define wallet "$1")
(define transfers (get_account_transfers wallet :compress true))
(define filtered
  (filter (lambda (tx)
            (= (get tx "transferType") "token"))
          transfers))
(log :message "Token transfers:" :value (length filtered))"#
                    .to_string(),
            ),
            insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "dex-trades".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("Analyze DEX trades".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Find and analyze DEX swap transactions".to_string(),
            })),
            insert_text: Some(
                r#"(define wallet "$1")
(define transfers (get_account_transfers wallet :compress true))
(define swaps
  (filter (lambda (tx)
            (= (get tx "transferType") "swap"))
          transfers))
(define total-volume
  (reduce swaps 0
          (lambda (acc tx)
            (+ acc (get tx "amount")))))
(log :message "DEX volume:" :value total-volume)"#
                    .to_string(),
            ),
            insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "wash-trading-detection".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("Detect wash trading patterns".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Detect potential wash trading by finding circular transfers".to_string(),
            })),
            insert_text: Some(
                r#"(define wallet "$1")
(define transfers (get_account_transfers wallet :compress true))

;; Find transfers that return to the same wallet
(define potential-wash
  (filter (lambda (tx)
            (and (= (get tx "source") wallet)
                 (let ((dest (get tx "destination")))
                   ;; Check if dest sends back to wallet
                   (member wallet
                           (mapcar (lambda (t) (get t "destination"))
                                   (get_account_transfers dest :limit 50))))))
          transfers))

(log :message "Potential wash trades:" :value (length potential-wash))"#
                    .to_string(),
            ),
            insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "whale-tracking".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("Track whale movements".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Identify and track large wallet movements".to_string(),
            })),
            insert_text: Some(
                r#"(define wallet "$1")
(define threshold 1000) ;; SOL threshold for "whale" transfer

(define transfers (get_account_transfers wallet :compress true))
(define whale-transfers
  (filter (lambda (tx)
            (> (get tx "amount") threshold))
          transfers))

(define sorted-whales
  (sort whale-transfers
        (lambda (a b)
          (> (get a "amount") (get b "amount")))))

(log :message "Whale transfers found:" :value (length whale-transfers))
(log :message "Top whale transfer:" :value (first sorted-whales))"#
                    .to_string(),
            ),
            insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "nft-activity".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("Analyze NFT activity".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Find NFT mints, transfers, and sales for a wallet".to_string(),
            })),
            insert_text: Some(
                r#"(define wallet "$1")
(define transfers (get_account_transfers wallet :compress true))

(define nft-activity
  (filter (lambda (tx)
            (or (= (get tx "transferType") "nft_mint")
                (= (get tx "transferType") "nft_transfer")
                (= (get tx "transferType") "nft_sale")))
          transfers))

(define grouped
  {:mints (filter (lambda (t) (= (get t "transferType") "nft_mint")) nft-activity)
   :transfers (filter (lambda (t) (= (get t "transferType") "nft_transfer")) nft-activity)
   :sales (filter (lambda (t) (= (get t "transferType") "nft_sale")) nft-activity)})

(log :message "NFT Activity:" :value grouped)"#
                    .to_string(),
            ),
            insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_default() {
        let config = AiConfig::default();
        assert!(!config.enabled); // Disabled by default
        assert!(config.url.contains("11434")); // Default Ollama port
    }

    #[test]
    fn test_blockchain_snippets() {
        let snippets = get_blockchain_snippets();
        assert!(!snippets.is_empty());
        assert!(snippets.iter().any(|s| s.label == "wallet-analysis"));
        assert!(snippets.iter().any(|s| s.label == "wash-trading-detection"));
    }

    #[test]
    fn test_truncate_label() {
        assert_eq!(truncate_label("short", 10), "short");
        assert_eq!(
            truncate_label("this is a very long label", 10),
            "this is..."
        );
    }
}
