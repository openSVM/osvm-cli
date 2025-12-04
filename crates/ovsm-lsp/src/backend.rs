//! OVSM Language Server Backend
//!
//! Implements the Language Server Protocol for OVSM LISP.
//! This is the main entry point that handles all LSP requests.
//!
//! # Self-Improving Behavior
//!
//! The LSP tracks usage patterns and adapts over time:
//! - Frequently used functions are prioritized in completions
//! - Accepted completions are boosted in future sessions
//! - Common investigation sequences are learned and suggested

use crate::ai_completion::{get_blockchain_snippets, AiCompletionProvider};
use crate::blockchain_types::{
    get_field_completions, get_program_id_completions, get_transfer_type_completions,
    infer_type_from_context, BLOCKCHAIN_TYPES,
};
use crate::diagnostics::{analyze_document, get_word_at_position};
use crate::documentation::{format_hover, get_documentation};
use crate::repl::{evaluate_all, evaluate_expression, format_result_inline, generate_code_lenses};
use crate::semantic_tokens::{get_legend, tokens_to_semantic};
use crate::symbols::{extract_symbols, SymbolTable};
use crate::telemetry::{LearningConfig, LearningEngine};

use dashmap::DashMap;
use ovsm::lexer::Token;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

/// OVSM Language Server backend
pub struct OvsmLanguageServer {
    /// LSP client for sending notifications
    client: Client,
    /// Document contents cache
    documents: DashMap<Url, DocumentState>,
    /// AI completion provider
    ai_provider: AiCompletionProvider,
    /// Self-improving learning engine
    learning_engine: Arc<LearningEngine>,
}

/// Cached state for an open document
struct DocumentState {
    /// Document content
    content: String,
    /// Cached tokens from last analysis
    tokens: Vec<Token>,
    /// Symbol table for go-to-definition
    symbols: SymbolTable,
}

impl OvsmLanguageServer {
    /// Create a new OVSM language server
    pub fn new(client: Client) -> Self {
        let learning_engine = Arc::new(LearningEngine::new(LearningConfig::default()));
        tracing::info!("🧠 Learning engine initialized - LSP will adapt to your usage patterns");

        Self {
            client,
            documents: DashMap::new(),
            ai_provider: AiCompletionProvider::new(),
            learning_engine,
        }
    }

    /// Create with custom learning config
    pub fn with_learning_config(client: Client, learning_config: LearningConfig) -> Self {
        let learning_engine = Arc::new(LearningEngine::new(learning_config));

        Self {
            client,
            documents: DashMap::new(),
            ai_provider: AiCompletionProvider::new(),
            learning_engine,
        }
    }

    /// Analyze a document and publish diagnostics
    async fn analyze_and_publish(&self, uri: Url, content: &str) {
        let result = analyze_document(content, &uri);

        // Extract symbols for go-to-definition
        let symbols = extract_symbols(content);

        // Cache tokens and symbols
        self.documents.insert(
            uri.clone(),
            DocumentState {
                content: content.to_string(),
                tokens: result.tokens,
                symbols,
            },
        );

        // Publish diagnostics
        self.client
            .publish_diagnostics(uri, result.diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for OvsmLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // Full document sync - we want the entire document on change
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),

                // Hover support for documentation
                hover_provider: Some(HoverProviderCapability::Simple(true)),

                // Completion support
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec!["(".to_string(), ":".to_string()]),
                    ..Default::default()
                }),

                // Semantic token support for syntax highlighting
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: get_legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: Some(false),
                            ..Default::default()
                        },
                    ),
                ),

                // Document formatting (future)
                document_formatting_provider: Some(OneOf::Left(false)),

                // Signature help (future)
                signature_help_provider: None,

                // Go to definition support
                definition_provider: Some(OneOf::Left(true)),

                // Document symbols (outline)
                document_symbol_provider: Some(OneOf::Left(true)),

                // Code lens for inline evaluation
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),

                // Execute command support for REPL and learning
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec![
                        "ovsm.runExpression".to_string(),
                        "ovsm.runAll".to_string(),
                        "ovsm.learningStats".to_string(),
                        "ovsm.nextSteps".to_string(),
                    ],
                    ..Default::default()
                }),

                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "ovsm-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "OVSM Language Server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    // ========================================================================
    // Document Synchronization
    // ========================================================================

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;

        self.client
            .log_message(MessageType::INFO, format!("Opened: {}", uri))
            .await;

        self.analyze_and_publish(uri, &content).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;

        // With FULL sync, we get the entire document content
        if let Some(change) = params.content_changes.into_iter().next() {
            self.analyze_and_publish(uri, &change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;

        // Remove from cache
        self.documents.remove(&uri);

        // Clear diagnostics
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        // Re-analyze on save if we have the text
        if let Some(text) = params.text {
            self.analyze_and_publish(params.text_document.uri, &text)
                .await;
        }
    }

    // ========================================================================
    // Hover Support
    // ========================================================================

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Get the document from cache
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        // Try to find the word at the cursor position
        let Some(word) = get_word_at_position(&doc.content, position) else {
            return Ok(None);
        };

        // Log hover for learning (user is interested in this symbol)
        self.learning_engine.log_hover(&word).await;

        // First, check if it's a built-in function
        if let Some(entry) = get_documentation(&word) {
            let hover_content = format_hover(entry);
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_content,
                }),
                range: None,
            }));
        }

        // Otherwise, check if it's a user-defined symbol
        if let Some(definitions) = doc.symbols.find_definitions(&word) {
            if let Some(def) = definitions.first() {
                let kind_str = match def.kind {
                    crate::symbols::SymbolKind::Variable => "Variable",
                    crate::symbols::SymbolKind::Function => "Function",
                    crate::symbols::SymbolKind::Constant => "Constant",
                    crate::symbols::SymbolKind::Parameter => "Parameter",
                    crate::symbols::SymbolKind::LocalBinding => "Local binding",
                };

                let doc_str = def.documentation.as_deref().unwrap_or("");
                let hover_content = format!(
                    "```ovsm\n{}\n```\n\n**{}**: {}\n\n{}",
                    word, kind_str, word, doc_str
                );

                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: hover_content,
                    }),
                    range: None,
                }));
            }
        }

        Ok(None)
    }

    // ========================================================================
    // Go to Definition
    // ========================================================================

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Get the document from cache
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        // Try to find the word at the cursor position
        let Some(word) = get_word_at_position(&doc.content, position) else {
            return Ok(None);
        };

        // Look up the symbol definition
        let Some(definitions) = doc.symbols.find_definitions(&word) else {
            return Ok(None);
        };

        // Return all definition locations
        let locations: Vec<Location> = definitions
            .iter()
            .map(|def| Location {
                uri: uri.clone(),
                range: def.definition_location,
            })
            .collect();

        if locations.is_empty() {
            Ok(None)
        } else if locations.len() == 1 {
            Ok(Some(GotoDefinitionResponse::Scalar(
                locations.into_iter().next().unwrap(),
            )))
        } else {
            Ok(Some(GotoDefinitionResponse::Array(locations)))
        }
    }

    // ========================================================================
    // Document Symbols (Outline)
    // ========================================================================

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        // Get the document from cache
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        // Convert symbol table to document symbols
        let mut symbols = Vec::new();

        for (name, definitions) in doc.symbols.definitions.iter() {
            for def in definitions {
                let kind = match def.kind {
                    crate::symbols::SymbolKind::Variable => SymbolKind::VARIABLE,
                    crate::symbols::SymbolKind::Function => SymbolKind::FUNCTION,
                    crate::symbols::SymbolKind::Constant => SymbolKind::CONSTANT,
                    crate::symbols::SymbolKind::Parameter => SymbolKind::VARIABLE,
                    crate::symbols::SymbolKind::LocalBinding => SymbolKind::VARIABLE,
                };

                #[allow(deprecated)] // SymbolInformation deprecated but still supported
                symbols.push(SymbolInformation {
                    name: name.clone(),
                    kind,
                    tags: None,
                    deprecated: None,
                    location: Location {
                        uri: uri.clone(),
                        range: def.definition_location,
                    },
                    container_name: None,
                });
            }
        }

        Ok(Some(DocumentSymbolResponse::Flat(symbols)))
    }

    // ========================================================================
    // Completion Support
    // ========================================================================

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        // Get learned boosts for adaptive sorting
        let learning_engine = self.learning_engine.clone();

        // Start with built-in completions
        let mut items: Vec<CompletionItem> = Vec::new();

        for (name, entry) in crate::documentation::all_documentation() {
            let kind = match entry.category {
                crate::documentation::DocCategory::SpecialForm => CompletionItemKind::KEYWORD,
                crate::documentation::DocCategory::ControlFlow => CompletionItemKind::KEYWORD,
                crate::documentation::DocCategory::Arithmetic
                | crate::documentation::DocCategory::Comparison
                | crate::documentation::DocCategory::Logical => CompletionItemKind::OPERATOR,
                _ => CompletionItemKind::FUNCTION,
            };

            // Get learned boost for this function
            let boost = learning_engine.get_completion_boost(name).await;
            // Convert boost to sort priority (higher boost = lower sort value = appears first)
            // Base priority is 0 for builtins, subtract boost to prioritize learned items
            let sort_priority = if boost > 0.1 {
                format!("00{:03}{}", (100.0 - boost * 100.0) as u32, name)
            } else {
                format!("0{}", name)
            };

            items.push(CompletionItem {
                label: name.to_string(),
                kind: Some(kind),
                detail: Some(entry.signature.to_string()),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: entry.description.to_string(),
                })),
                insert_text: Some(name.to_string()),
                sort_text: Some(sort_priority),
                ..Default::default()
            });
        }

        // Add user-defined symbols from the current document
        if let Some(doc) = self.documents.get(&uri) {
            for (name, definitions) in doc.symbols.definitions.iter() {
                if let Some(def) = definitions.first() {
                    let kind = match def.kind {
                        crate::symbols::SymbolKind::Variable => CompletionItemKind::VARIABLE,
                        crate::symbols::SymbolKind::Function => CompletionItemKind::FUNCTION,
                        crate::symbols::SymbolKind::Constant => CompletionItemKind::CONSTANT,
                        crate::symbols::SymbolKind::Parameter => CompletionItemKind::VARIABLE,
                        crate::symbols::SymbolKind::LocalBinding => CompletionItemKind::VARIABLE,
                    };

                    items.push(CompletionItem {
                        label: name.clone(),
                        kind: Some(kind),
                        detail: def.documentation.clone(),
                        sort_text: Some(format!("1{}", name)), // Sort user symbols second
                        ..Default::default()
                    });
                }
            }

            // Add blockchain investigation snippets (always available)
            for mut snippet in get_blockchain_snippets() {
                snippet.sort_text = Some(format!("2{}", snippet.label)); // Sort snippets third
                items.push(snippet);
            }

            // Add blockchain-aware contextual completions
            let lines: Vec<&str> = doc.content.lines().collect();
            let current_line = lines.get(position.line as usize).unwrap_or(&"");
            let prefix = if position.character as usize <= current_line.len() {
                &current_line[..position.character as usize]
            } else {
                current_line
            };

            // Check if we're in a (get ...) context - suggest field names
            if prefix.contains("(get ") || prefix.contains("(get\n") {
                // Infer the type from context
                if let Some(type_name) = infer_type_from_context(prefix, current_line) {
                    for mut field in get_field_completions(type_name) {
                        field.sort_text = Some(format!("00{}", field.label)); // Highest priority
                        items.push(field);
                    }
                }

                // Also suggest all common blockchain fields
                for bt in BLOCKCHAIN_TYPES {
                    for field in bt.fields {
                        if !items.iter().any(|i| i.label == field.name) {
                            items.push(CompletionItem {
                                label: field.name.to_string(),
                                kind: Some(CompletionItemKind::FIELD),
                                detail: Some(format!("{} ({})", field.field_type, bt.name)),
                                documentation: Some(Documentation::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: field.description.to_string(),
                                })),
                                insert_text: Some(format!("\"{}\"", field.name)),
                                sort_text: Some(format!("01{}", field.name)),
                                ..Default::default()
                            });
                        }
                    }
                }
            }

            // Check if we're filtering by transferType - suggest types
            if prefix.contains("transferType") || prefix.contains("\"transferType\"") {
                for mut tt in get_transfer_type_completions() {
                    tt.sort_text = Some(format!("00{}", tt.label));
                    items.push(tt);
                }
            }

            // Check if we're working with program IDs
            if prefix.contains("program") || prefix.contains("owner") || prefix.contains("mint") {
                for mut prog in get_program_id_completions() {
                    prog.sort_text = Some(format!("01{}", prog.label));
                    items.push(prog);
                }
            }

            // Add AI-powered completions if enabled
            if self.ai_provider.is_enabled() {
                // Get the current line prefix for context
                let lines: Vec<&str> = doc.content.lines().collect();
                let current_line = lines.get(position.line as usize).unwrap_or(&"");
                let prefix = &current_line[..position.character as usize].trim_start();

                let ai_completions = self
                    .ai_provider
                    .get_completions(prefix, &doc.content, position.line)
                    .await;

                for mut completion in ai_completions {
                    completion.sort_text = Some(format!("3{}", completion.label)); // Sort AI last
                    items.push(completion);
                }
            }

            // Add learned next-step suggestions based on previous expressions
            // Look at the previous expression to suggest what typically comes next
            if position.line > 0 {
                let lines: Vec<&str> = doc.content.lines().collect();
                for i in (0..position.line as usize).rev() {
                    let line = lines.get(i).unwrap_or(&"");
                    let func_name = extract_function_name(line.trim());
                    if !func_name.is_empty()
                        && func_name
                            .chars()
                            .next()
                            .map_or(false, |c| c.is_alphabetic())
                    {
                        // Found a previous function call, get suggestions
                        let suggestions = learning_engine.get_next_suggestions(&func_name).await;
                        for (idx, suggestion) in suggestions.into_iter().enumerate() {
                            items.push(CompletionItem {
                                label: format!("🧠 {}", suggestion),
                                kind: Some(CompletionItemKind::SNIPPET),
                                detail: Some(format!("Learned: typically follows {}", func_name)),
                                documentation: Some(Documentation::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: format!(
                                        "**Suggested next step**\n\n\
                                         Based on your usage patterns, `{}` often follows `{}`.\n\n\
                                         _The LSP learns from your investigation workflows._",
                                        suggestion, func_name
                                    ),
                                })),
                                insert_text: Some(format!("({})", suggestion)),
                                sort_text: Some(format!("000{:02}{}", idx, suggestion)), // Highest priority
                                ..Default::default()
                            });
                        }
                        break; // Only look at the most recent function
                    }
                }
            }
        }

        Ok(Some(CompletionResponse::Array(items)))
    }

    // ========================================================================
    // Semantic Tokens
    // ========================================================================

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;

        // Get cached tokens
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        let semantic = tokens_to_semantic(&doc.tokens);

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic,
        })))
    }

    // ========================================================================
    // Code Lens (REPL evaluation)
    // ========================================================================

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = params.text_document.uri;

        // Get the document from cache
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        let lenses = generate_code_lenses(&doc.content, uri.as_str());
        Ok(Some(lenses))
    }

    // ========================================================================
    // Execute Command (REPL)
    // ========================================================================

    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        let args = params.arguments;

        match params.command.as_str() {
            "ovsm.runExpression" => {
                // Arguments: [uri, index, source]
                if args.len() >= 3 {
                    if let Some(source) = args[2].as_str() {
                        let result = evaluate_expression(source);
                        let message = format_result_inline(&result);

                        // Extract function name for learning
                        let func_name = extract_function_name(source);
                        self.learning_engine
                            .log_execution(&func_name, result.success)
                            .await;

                        // Show result to user
                        if result.success {
                            self.client.show_message(MessageType::INFO, &message).await;
                        } else {
                            // Log error for learning
                            if let Some(ref err) = result.error {
                                self.learning_engine.log_error("runtime", err).await;
                            }
                            self.client.show_message(MessageType::ERROR, &message).await;
                        }

                        return Ok(Some(serde_json::json!({
                            "success": result.success,
                            "value": result.value,
                            "duration_ms": result.duration_ms,
                            "error": result.error
                        })));
                    }
                }
                Ok(None)
            }
            "ovsm.runAll" => {
                // Arguments: [uri]
                if let Some(uri_str) = args.get(0).and_then(|v| v.as_str()) {
                    if let Ok(uri) = uri_str.parse::<Url>() {
                        if let Some(doc) = self.documents.get(&uri) {
                            let results = evaluate_all(&doc.content);

                            let success_count = results.iter().filter(|r| r.success).count();
                            let total = results.len();
                            let total_time: f64 = results.iter().map(|r| r.duration_ms).sum();

                            let message = format!(
                                "Executed {} expressions: {} succeeded, {} failed ({:.2}ms total)",
                                total,
                                success_count,
                                total - success_count,
                                total_time
                            );

                            if success_count == total {
                                self.client.show_message(MessageType::INFO, &message).await;
                            } else {
                                self.client
                                    .show_message(MessageType::WARNING, &message)
                                    .await;
                            }

                            // Return last result
                            if let Some(last) = results.last() {
                                return Ok(Some(serde_json::json!({
                                    "success": last.success,
                                    "value": last.value,
                                    "duration_ms": total_time,
                                    "results_count": total
                                })));
                            }
                        }
                    }
                }
                Ok(None)
            }
            "ovsm.learningStats" => {
                // Return learning statistics
                let stats = self.learning_engine.get_stats().await;
                let frequent = self.learning_engine.get_frequent_functions(10).await;

                let message = format!(
                    "🧠 Learning Stats:\n\
                     • Events tracked: {}\n\
                     • Unique functions: {}\n\
                     • Sequences learned: {}\n\
                     • Acceptance rate: {:.1}%\n\
                     • Top functions: {}",
                    stats.total_events,
                    stats.unique_functions,
                    stats.sequences_learned,
                    stats.avg_acceptance_rate * 100.0,
                    if frequent.is_empty() {
                        "(none yet)".to_string()
                    } else {
                        frequent.join(", ")
                    }
                );

                self.client.show_message(MessageType::INFO, &message).await;

                Ok(Some(serde_json::json!({
                    "total_events": stats.total_events,
                    "unique_functions": stats.unique_functions,
                    "sequences_learned": stats.sequences_learned,
                    "avg_acceptance_rate": stats.avg_acceptance_rate,
                    "frequent_functions": frequent
                })))
            }

            "ovsm.nextSteps" => {
                // Get suggested next functions based on last executed function
                if let Some(current_func) = args.get(0).and_then(|v| v.as_str()) {
                    let suggestions = self
                        .learning_engine
                        .get_next_suggestions(current_func)
                        .await;

                    if suggestions.is_empty() {
                        self.client
                            .show_message(
                                MessageType::INFO,
                                "No learned sequences yet. Keep using the REPL!",
                            )
                            .await;
                    } else {
                        let message = format!(
                            "📊 Suggested next steps after {}:\n{}",
                            current_func,
                            suggestions
                                .iter()
                                .enumerate()
                                .map(|(i, s)| format!("  {}. {}", i + 1, s))
                                .collect::<Vec<_>>()
                                .join("\n")
                        );
                        self.client.show_message(MessageType::INFO, &message).await;
                    }

                    return Ok(Some(serde_json::json!({
                        "suggestions": suggestions
                    })));
                }
                Ok(None)
            }

            _ => Ok(None),
        }
    }
}

/// Extract the first function name from an OVSM expression
/// e.g., "(getBalance wallet)" -> "getBalance"
fn extract_function_name(source: &str) -> String {
    let trimmed = source.trim();
    if trimmed.starts_with('(') {
        // Find the first word after the opening paren
        let after_paren = &trimmed[1..];
        let end = after_paren
            .find(|c: char| c.is_whitespace() || c == ')')
            .unwrap_or(after_paren.len());
        after_paren[..end].to_string()
    } else {
        // Just a symbol or literal
        let end = trimmed
            .find(|c: char| c.is_whitespace())
            .unwrap_or(trimmed.len());
        trimmed[..end].to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_function_name() {
        assert_eq!(extract_function_name("(getBalance wallet)"), "getBalance");
        assert_eq!(extract_function_name("(+ 1 2 3)"), "+");
        assert_eq!(extract_function_name("(define x 10)"), "define");
        assert_eq!(extract_function_name("  (  spaced  )  "), "");
        assert_eq!(extract_function_name("symbol"), "symbol");
    }
}
