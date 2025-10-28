use crate::utils::circuit_breaker::{
    AnalysisVector as CircuitAnalysisVector, EndpointId, GranularCircuitBreaker,
};
use crate::utils::debug_logger::VerbosityLevel;
use crate::utils::prompt_templates::{
    AnalysisVector as TemplateAnalysisVector, PromptTemplateManager, TemplateCategory,
};
use crate::{debug_error, debug_print, debug_success, debug_warn};
use anyhow::{Context, Result};
use reqwest;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;
use std::time::Duration;

// Import OVSM parser for validation
use ovsm::{Parser as OvsmParser, Scanner as OvsmScanner};

#[derive(Serialize, Debug, Clone)]
struct AiRequest {
    question: String,
    #[serde(skip_serializing_if = "Option::is_none", rename = "systemPrompt")]
    system_prompt: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", rename = "ownPlan")]
    own_plan: Option<bool>,
}

#[derive(Serialize)]
struct OpenAiRequest {
    model: String,
    messages: Vec<OpenAiMessage>,
    max_tokens: u32,
    temperature: f32,
}

#[derive(Serialize, Deserialize)]
struct OpenAiMessage {
    role: String,
    content: String,
}

#[derive(Deserialize)]
struct AiResponse {
    #[serde(default)]
    answer: String,
    #[serde(default)]
    error: Option<String>,
}

#[derive(Deserialize)]
struct OpenAiResponse {
    choices: Vec<OpenAiChoice>,
}

#[derive(Deserialize)]
struct OpenAiChoice {
    message: OpenAiMessage,
}

pub struct AiService {
    client: reqwest::Client,
    api_url: String,
    api_key: Option<String>,
    use_openai: bool,
    circuit_breaker: GranularCircuitBreaker,
    template_manager: PromptTemplateManager,
}

impl AiService {
    /// Get the templates directory path
    fn get_templates_dir() -> String {
        if let Ok(home) = env::var("HOME") {
            format!("{}/.config/osvm/templates", home)
        } else {
            // Fallback to current directory if HOME not set
            "./templates/ai_prompts".to_string()
        }
    }

    /// Ensure default templates exist in user config directory
    fn ensure_default_templates(templates_dir: &str) -> Result<()> {
        use std::fs;
        use std::path::Path;

        let templates_path = Path::new(templates_dir);

        // Create directory if it doesn't exist
        if !templates_path.exists() {
            fs::create_dir_all(templates_path)?;
        }

        // Default templates to copy
        let default_templates = [
            (
                "fix_suggestion_general.yaml",
                include_str!("../../templates/ai_prompts/fix_suggestion_general.yaml"),
            ),
            (
                "deeplogic_economic_exploit.yaml",
                include_str!("../../templates/ai_prompts/deeplogic_economic_exploit.yaml"),
            ),
        ];

        // CRITICAL SECURITY FIX: Atomic create-if-not-exists to prevent TOCTOU attacks
        use std::io::Write;
        #[cfg(unix)]
        use std::os::unix::fs::OpenOptionsExt;

        for (filename, content) in &default_templates {
            let target_path = templates_path.join(filename);

            // Atomic create-if-not-exists with O_EXCL (prevents symlink attacks)
            let file_result = {
                let mut opts = std::fs::OpenOptions::new();
                opts.write(true).create_new(true); // Atomically fails if file exists

                #[cfg(unix)]
                opts.mode(0o644); // Secure permissions: owner rw, group/other r

                opts.open(&target_path)
            };

            match file_result {
                Ok(mut file) => {
                    file.write_all(content.as_bytes())?;
                    file.sync_all()?; // Force to disk
                    log::info!("✅ Copied default template: {}", filename);
                }
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
                    // File exists, skip silently (user may have customized it)
                    continue;
                }
                Err(e) => {
                    return Err(anyhow::anyhow!(
                        "Failed to create template {}: {}",
                        filename,
                        e
                    ));
                }
            }
        }

        Ok(())
    }

    pub fn new() -> Self {
        Self::new_with_debug(true)
    }

    pub fn new_with_debug(debug_mode: bool) -> Self {
        Self::with_api_url_and_debug(None, debug_mode)
    }

    pub fn with_api_url(custom_api_url: Option<String>) -> Self {
        Self::with_api_url_and_debug(custom_api_url, true)
    }

    pub fn with_api_url_and_debug(custom_api_url: Option<String>, debug_mode: bool) -> Self {
        // Set debug verbosity based on debug mode
        if debug_mode {
            crate::utils::debug_logger::set_verbosity(VerbosityLevel::Detailed);
        } else {
            crate::utils::debug_logger::set_verbosity(VerbosityLevel::Silent);
        }

        debug_print!("Initializing AI service with debug mode: {}", debug_mode);

        let (api_url, use_openai) = match custom_api_url {
            Some(url) => {
                // Always use localhost:3000 for non-OpenAI URLs
                if url.contains("openai.com") || url.contains("api.openai.com") {
                    if let Some(key) = env::var("OPENAI_KEY").ok().filter(|k| !k.trim().is_empty())
                    {
                        (url, true)
                    } else {
                        eprintln!("⚠️  OpenAI URL provided but no OPENAI_KEY found, falling back to localhost:3000");
                        ("http://localhost:3000/api/getAnswer".to_string(), false)
                    }
                } else {
                    // Use localhost:3000 - user's special AI backend
                    ("http://localhost:3000/api/getAnswer".to_string(), false)
                }
            }
            None => {
                // CRITICAL: Always use localhost:3000 backend - user's special AI
                ("http://localhost:3000/api/getAnswer".to_string(), false)
            }
        };

        let api_key = if use_openai {
            env::var("OPENAI_KEY").ok()
        } else {
            None
        };

        let mut circuit_breaker = GranularCircuitBreaker::new();
        let mut template_manager = PromptTemplateManager::new();

        // Initialize template manager from user config directory
        let templates_dir = Self::get_templates_dir();

        // Ensure templates directory exists and copy defaults if needed
        if let Err(e) = Self::ensure_default_templates(&templates_dir) {
            if debug_mode {
                debug_warn!("Failed to setup template directory: {}", e);
            }
        }

        // Load templates from user config directory
        if let Err(e) = template_manager.load_from_directory_with_debug(&templates_dir, debug_mode)
        {
            if debug_mode {
                debug_warn!(
                    "Failed to load AI prompt templates from {}: {}",
                    templates_dir,
                    e
                );
            }
        }

        // Build client with extended timeout for large OVSM planning prompts
        let client = reqwest::Client::builder()
            .timeout(Duration::from_secs(120)) // 2 minutes for AI processing
            .build()
            .expect("Failed to build HTTP client");

        Self {
            client,
            api_url,
            api_key,
            use_openai,
            circuit_breaker,
            template_manager,
        }
    }

    pub async fn query(&self, question: &str) -> Result<String> {
        self.query_with_debug(question, false).await
    }

    pub async fn query_with_debug(&self, question: &str, debug_mode: bool) -> Result<String> {
        // Fail fast if OpenAI is configured but no API key is provided
        if self.use_openai && self.api_key.is_none() {
            anyhow::bail!(
                "OpenAI API key not configured. Please set the OPENAI_KEY environment variable or use the default OSVM AI service."
            );
        }

        let endpoint = if self.use_openai {
            EndpointId::openai()
        } else {
            EndpointId::osvm_ai()
        };

        // Check circuit breaker
        if !self.circuit_breaker.can_execute_endpoint(&endpoint) {
            anyhow::bail!("Circuit breaker is open for endpoint: {:?}", endpoint);
        }

        if self.use_openai {
            if debug_mode {
                println!("🤖 Asking OpenAI ({}): {}", self.api_url, question);
            }
        } else if debug_mode {
            println!("🤖 Asking OSVM AI ({}): {}", self.api_url, question);
        }

        let result = if self.use_openai {
            self.query_openai(question, debug_mode).await
        } else {
            self.query_osvm_ai(question, debug_mode).await
        };

        // Record success/failure with circuit breaker
        match &result {
            Ok(_) => {
                self.circuit_breaker.on_success_endpoint(&endpoint);
                if debug_mode {
                    if let Ok(ref response) = result {
                        println!("🔍 AI Response received ({} chars)", response.len());
                    }
                }
            }
            Err(e) => {
                self.circuit_breaker.on_failure_endpoint(&endpoint);
                if debug_mode {
                    println!("❌ AI Response error: {}", e);
                }
            }
        }

        result
    }

    /// Enhanced DeepLogic analysis with configurable templates
    pub async fn analyze_deeplogic(
        &self,
        code: &str,
        filename: &str,
        vulnerability_description: &str,
        analysis_vector: TemplateAnalysisVector,
    ) -> Result<String> {
        let circuit_vector = match analysis_vector {
            TemplateAnalysisVector::StateTransition => CircuitAnalysisVector::StateTransition,
            TemplateAnalysisVector::EconomicExploit => CircuitAnalysisVector::EconomicExploit,
            TemplateAnalysisVector::AccessControl => CircuitAnalysisVector::AccessControl,
            TemplateAnalysisVector::MathematicalIntegrity => {
                CircuitAnalysisVector::MathematicalIntegrity
            }
            TemplateAnalysisVector::General => CircuitAnalysisVector::General,
        };

        // Check circuit breaker for this analysis vector
        if !self.circuit_breaker.can_execute_vector(&circuit_vector) {
            anyhow::bail!(
                "Circuit breaker is open for analysis vector: {:?}",
                circuit_vector
            );
        }

        // Get appropriate template
        let template = self
            .template_manager
            .get_best_template(&TemplateCategory::DeepLogic, Some(&analysis_vector))
            .ok_or_else(|| anyhow::anyhow!("No suitable DeepLogic template found"))?;

        // Prepare template variables
        let mut variables = HashMap::new();
        variables.insert("code".to_string(), code.to_string());
        variables.insert("filename".to_string(), filename.to_string());
        variables.insert(
            "vulnerability_description".to_string(),
            vulnerability_description.to_string(),
        );

        // Render the prompt
        let prompt = self
            .template_manager
            .render_template(&template.id, &variables)
            .with_context(|| "Failed to render DeepLogic template")?;

        println!(
            "🧠 Performing DeepLogic analysis using template: {}",
            template.name
        );

        // Execute the query
        let result = self.query(&prompt).await;

        // Record success/failure for the analysis vector
        match &result {
            Ok(_) => self.circuit_breaker.on_success_vector(&circuit_vector),
            Err(_) => self.circuit_breaker.on_failure_vector(&circuit_vector),
        }

        result
    }

    pub fn get_endpoint_info(&self) -> String {
        if self.use_openai {
            format!("OpenAI endpoint: {}", self.api_url)
        } else {
            format!("OSVM AI endpoint: {}", self.api_url)
        }
    }

    /// Check if an error is a timeout that should be retried
    ///
    /// Detects API timeouts, gateway timeouts, and connection timeouts
    fn is_timeout_error(error: &anyhow::Error) -> bool {
        let error_str = error.to_string().to_lowercase();
        error_str.contains("504")
            || error_str.contains("gateway timeout")
            || error_str.contains("timeout")
            || error_str.contains("timed out")
            || error_str.contains("connection timeout")
            || error_str.contains("request timeout")
    }

    /// Retry an API call with exponential backoff on timeout errors
    ///
    /// This handles transient infrastructure issues like:
    /// - 504 Gateway Timeout
    /// - Connection timeouts
    /// - Request timeouts
    ///
    /// Retry schedule: 5s, 10s, 20s, 40s (max 4 attempts, up to 75s total wait)
    ///
    /// This is patient enough for:
    /// - Large system prompts (30KB+)
    /// - Complex blockchain queries
    /// - AI server under load
    async fn with_timeout_retry<F, Fut>(
        &self,
        operation: F,
        max_attempts: u32,
        debug_mode: bool,
    ) -> Result<String>
    where
        F: Fn() -> Fut,
        Fut: std::future::Future<Output = Result<String>>,
    {
        let mut attempt = 1;
        let base_delay_ms = 5000; // Start with 5 seconds for AI processing

        loop {
            match operation().await {
                Ok(response) => {
                    if attempt > 1 && debug_mode {
                        println!("✅ API call succeeded on attempt #{}", attempt);
                    }
                    return Ok(response);
                }
                Err(e) => {
                    let is_timeout = Self::is_timeout_error(&e);

                    if !is_timeout || attempt >= max_attempts {
                        // Not a timeout or max attempts reached - fail
                        if debug_mode {
                            if is_timeout {
                                println!(
                                    "⛔ Max timeout retry attempts ({}) exceeded",
                                    max_attempts
                                );
                            } else {
                                println!("⛔ Non-timeout error, not retrying");
                            }
                        }
                        return Err(e);
                    }

                    // Calculate exponential backoff delay
                    let delay_ms = base_delay_ms * (1 << (attempt - 1)); // 5s, 10s, 20s, 40s

                    println!(
                        "⏱️  API timeout on attempt #{}/{} ({}s wait). Retrying in {}s...",
                        attempt,
                        max_attempts,
                        if attempt == 1 {
                            0
                        } else {
                            base_delay_ms * ((1 << (attempt - 2)) - 1) / 1000
                        },
                        delay_ms / 1000
                    );

                    // Wait before retrying
                    tokio::time::sleep(tokio::time::Duration::from_millis(delay_ms)).await;

                    attempt += 1;
                }
            }
        }
    }

    async fn query_osvm_ai(&self, question: &str, debug_mode: bool) -> Result<String> {
        // Wrap with timeout retry logic (max 4 attempts with exponential backoff)
        self.with_timeout_retry(
            || self.query_osvm_ai_internal(question, None, None, debug_mode),
            4, // 4 attempts = up to 75 seconds total wait time
            debug_mode,
        )
        .await
    }

    pub async fn query_osvm_ai_with_options(
        &self,
        question: &str,
        system_prompt: Option<String>,
        only_plan: Option<bool>,
        debug_mode: bool,
    ) -> Result<String> {
        // Wrap with timeout retry logic
        let mut response = self.with_timeout_retry(
            || self.query_osvm_ai_internal(question, system_prompt.clone(), only_plan, debug_mode),
            4,
            debug_mode,
        )
        .await?;
        
        // Check for truncation and handle continuation if needed
        if self.is_response_truncated(&response) {
            if debug_mode {
                println!("⚠️ Response appears truncated ({} chars). Requesting continuation...", response.len());
            }
            response = self.handle_truncated_response(
                question,
                system_prompt,
                only_plan,
                response,
                debug_mode
            ).await?;
        }
        
        Ok(response)
    }

    /// Internal API call without retry logic
    async fn query_osvm_ai_internal(
        &self,
        question: &str,
        system_prompt: Option<String>,
        only_plan: Option<bool>,
        debug_mode: bool,
    ) -> Result<String> {
        let request_body = AiRequest {
            question: question.to_string(),
            system_prompt: system_prompt.clone(),
            own_plan: only_plan,
        };

        if debug_mode {
            println!("📤 OSVM AI Request:");
            println!("  question: {} chars", question.len());
            println!(
                "  systemPrompt: {} chars",
                system_prompt.as_ref().map(|s| s.len()).unwrap_or(0)
            );
            println!("  ownPlan: {:?}", only_plan);
            // Hide system prompt from logs for security/readability
            // Removed Full JSON output as it's not useful in logs
            // let mut debug_body = request_body.clone();
            // if debug_body.system_prompt.is_some() {
            //     debug_body.system_prompt = Some("[HIDDEN - see length above]".to_string());
            // }
            // println!("  Full JSON: {}", serde_json::to_string_pretty(&debug_body)?);
        }

        let response = self
            .client
            .post(&self.api_url)
            .header("Content-Type", "application/json")
            .json(&request_body)
            .send()
            .await?;

        let status = response.status();
        let response_text = response.text().await?;

        if debug_mode {
            println!("📥 OSVM AI Response ({}): {}", status, response_text);
        }

        if !status.is_success() {
            // Try to parse error response as JSON first
            if let Ok(ai_response) = serde_json::from_str::<AiResponse>(&response_text) {
                if let Some(error) = ai_response.error {
                    anyhow::bail!("AI API returned error: {}", error);
                }
            }
            // If JSON parsing fails, return the generic error
            anyhow::bail!(
                "AI API request failed with status: {} - Response: {}",
                status,
                response_text
            );
        }

        // Try to parse as JSON first
        match serde_json::from_str::<AiResponse>(&response_text) {
            Ok(ai_response) => {
                if let Some(error) = ai_response.error {
                    anyhow::bail!("AI API returned error: {}", error);
                }
                Ok(ai_response.answer)
            }
            Err(_) => {
                // If JSON parsing fails, return the raw text as the answer
                // This handles cases where the API might return plain text
                if response_text.trim().is_empty() {
                    Ok("No response from AI service".to_string())
                } else {
                    Ok(response_text)
                }
            }
        }
    }

    async fn query_openai(&self, question: &str, debug_mode: bool) -> Result<String> {
        let api_key = self.api_key.as_ref().ok_or_else(|| {
            anyhow::anyhow!(
                "OpenAI API key not available. Please set OPENAI_KEY environment variable."
            )
        })?;

        let request_body = OpenAiRequest {
            model: "gpt-3.5-turbo".to_string(),
            messages: vec![OpenAiMessage {
                role: "user".to_string(),
                content: question.to_string(),
            }],
            max_tokens: 1000,
            temperature: 0.7,
        };

        if debug_mode {
            println!(
                "📤 OpenAI Request: {}",
                serde_json::to_string_pretty(&request_body)?
            );
        }

        let response = self
            .client
            .post(&self.api_url)
            .header("Content-Type", "application/json")
            .header("Authorization", format!("Bearer {}", api_key))
            .json(&request_body)
            .send()
            .await?;

        let status = response.status();
        let response_text = response.text().await?;

        if debug_mode {
            println!("📥 OpenAI Response ({}): {}", status, response_text);
        }

        if !status.is_success() {
            anyhow::bail!(
                "OpenAI API request failed with status: {} - Response: {}",
                status,
                response_text
            );
        }

        let openai_response: OpenAiResponse =
            serde_json::from_str(&response_text).context("Failed to parse OpenAI response")?;

        if let Some(choice) = openai_response.choices.first() {
            Ok(choice.message.content.clone())
        } else {
            anyhow::bail!("No response choices returned from OpenAI API");
        }
    }

    /// Plan which tools to use for a given user request using OVSM language
    pub async fn create_tool_plan(
        &self,
        user_request: &str,
        available_tools: &HashMap<String, Vec<crate::services::mcp_service::McpTool>>,
    ) -> Result<ToolPlan> {
        // Build OVSM planning prompt with system prompt
        let (planning_prompt, ovsm_system_prompt) =
            self.build_ovsm_planning_prompt(user_request, available_tools)?;

        // For OSVM AI, use ownPlan parameter to get plan directly
        let ai_response = if self.use_openai {
            // OpenAI: send system prompt in messages
            self.query_with_debug(&planning_prompt, false).await?
        } else {
            // OSVM AI: use ownPlan=true to get structured plan
            debug_print!("Sending OVSM plan request with custom system prompt");
            self.query_osvm_ai_with_options(
                &planning_prompt,
                Some(ovsm_system_prompt.clone()),
                Some(true), // ownPlan=true
                true,       // DEBUG MODE ON
            )
            .await?
        };

        // Try to parse as OVSM plan first
        if let Ok(ovsm_plan) = self.parse_ovsm_plan(&ai_response) {
            debug_success!("Successfully parsed OVSM plan");
            return Ok(ovsm_plan);
        }

        // Try to parse as legacy XML format
        if ai_response.contains("<osvm_plan>") {
            if let Ok(xml_plan) = self.parse_osvm_plan_xml(&ai_response) {
                debug_success!("Parsed legacy XML plan format");
                return Ok(xml_plan);
            }
        }

        // Try to parse as JSON
        if let Ok(json_plan) = self.parse_json_tool_plan(&ai_response) {
            debug_success!("Parsed JSON plan format");
            return Ok(json_plan);
        }

        // Try salvaging from non-standard format
        if let Some(salvaged) = self.salvage_tool_plan_from_response(&ai_response) {
            debug_success!("Salvaged tool plan from non-standard response");
            return Ok(salvaged);
        }

        // Retry with explicit OVSM format request
        let retry_prompt = format!(
            "Please respond with ONLY the OVSM plan structure. Re-plan for: {}",
            user_request
        );

        let retry_response = if self.use_openai {
            self.query_with_debug(&retry_prompt, false).await.ok()
        } else {
            self.query_osvm_ai_with_options(
                &retry_prompt,
                Some(ovsm_system_prompt),
                Some(true),
                false,
            )
            .await
            .ok()
        };

        if let Some(rr) = retry_response {
            if let Ok(plan) = self.parse_ovsm_plan(&rr) {
                debug_success!("Recovered OVSM plan on retry");
                return Ok(plan);
            }
            if let Some(salvaged_retry) = self.salvage_tool_plan_from_response(&rr) {
                debug_success!("Salvaged plan on retry");
                return Ok(salvaged_retry);
            }
        }

        // Final fallback: return an empty plan
        debug_warn!("Falling back to empty tool plan after all parse attempts");
        Ok(ToolPlan {
            reasoning: "I'll help you directly with your request.".to_string(),
            osvm_tools_to_use: vec![],
            expected_outcome: "Provide a helpful answer based on the request.".to_string(),
            raw_ovsm_plan: None, // No OVSM plan for empty fallback
        })
    }

    /// Get the OVSM system prompt for plan generation (V3 - Refined & Concise)
    ///
    /// This is the production system prompt that instructs AI models how to generate
    /// executable OVSM plans using LISP/S-expression syntax.
    ///
    /// V3 improvements:
    /// - 67% smaller (216 lines vs 660 lines)
    /// - Critical rules at the top for better AI attention
    /// - Removed redundancy (scoping rule stated 5+ times → stated once clearly)
    /// - Concise examples without losing functionality
    /// - Better structured for quick reference
    ///
    /// All OVSM scripts use LISP syntax exclusively.
    ///
    /// The prompt is loaded from src/prompts/ovsm_system_prompt_v3.md
    fn get_ovsm_system_prompt() -> &'static str {
        // Load refined V3 prompt from external file for better maintainability
        include_str!("../prompts/ovsm_system_prompt_v3.md")
    }

    /// Build OVSM planning prompt with tools context
    fn build_ovsm_planning_prompt(
        &self,
        user_request: &str,
        available_tools: &HashMap<String, Vec<crate::services::mcp_service::McpTool>>,
    ) -> Result<(String, String)> {
        let mut tools_context = String::new();

        if available_tools.is_empty() {
            tools_context.push_str("No MCP tools are currently available.\n");
        } else {
            // Concise tool listing - just names, grouped by category
            tools_context.push_str("Available MCP Tools (call with UPPERCASE names):\n\n");

            for (server_id, tools) in available_tools {
                let tool_names: Vec<String> = tools
                    .iter()
                    .filter(|t| !matches!(t.name.as_str(), "NOW" | "LOG" | "now" | "log"))
                    .map(|t| t.name.clone())
                    .collect();

                if !tool_names.is_empty() {
                    tools_context.push_str(&format!("Server '{}': ", server_id));

                    // Group tools by category for readability
                    let mut tx_tools = Vec::new();
                    let mut account_tools = Vec::new();
                    let mut block_tools = Vec::new();
                    let mut token_tools = Vec::new();
                    let mut defi_tools = Vec::new();
                    let mut util_tools = Vec::new();

                    for name in &tool_names {
                        let lower = name.to_lowercase();
                        if lower.contains("transaction") || lower.contains("tx") {
                            tx_tools.push(name);
                        } else if lower.contains("account")
                            || lower.contains("balance")
                            || lower.contains("portfolio")
                        {
                            account_tools.push(name);
                        } else if lower.contains("block") || lower.contains("slot") {
                            block_tools.push(name);
                        } else if lower.contains("token") || lower.contains("nft") {
                            token_tools.push(name);
                        } else if lower.contains("defi")
                            || lower.contains("dex")
                            || lower.contains("validator")
                        {
                            defi_tools.push(name);
                        } else {
                            util_tools.push(name);
                        }
                    }

                    let mut categories = Vec::new();
                    if !tx_tools.is_empty() {
                        let names: Vec<&str> = tx_tools.iter().map(|s| s.as_str()).collect();
                        categories.push(format!("Transactions({})", names.join(", ")));
                    }
                    if !account_tools.is_empty() {
                        let names: Vec<&str> = account_tools.iter().map(|s| s.as_str()).collect();
                        categories.push(format!("Accounts({})", names.join(", ")));
                    }
                    if !block_tools.is_empty() {
                        let names: Vec<&str> = block_tools.iter().map(|s| s.as_str()).collect();
                        categories.push(format!("Blocks({})", names.join(", ")));
                    }
                    if !token_tools.is_empty() {
                        let names: Vec<&str> = token_tools.iter().map(|s| s.as_str()).collect();
                        categories.push(format!("Tokens({})", names.join(", ")));
                    }
                    if !defi_tools.is_empty() {
                        let names: Vec<&str> = defi_tools.iter().map(|s| s.as_str()).collect();
                        categories.push(format!("DeFi({})", names.join(", ")));
                    }
                    if !util_tools.is_empty() {
                        let names: Vec<&str> = util_tools.iter().map(|s| s.as_str()).collect();
                        categories.push(format!("Utils({})", names.join(", ")));
                    }

                    tools_context.push_str(&categories.join(" | "));
                    tools_context.push('\n');
                }
            }

            tools_context.push_str(
                "\nNote: Tool names are case-sensitive. Use exact names from list above.\n",
            );
        }

        // Create system prompt with OVSM instructions and available tools
        let system_prompt = format!(
            "{}\n\n# Your Available MCP Tools\n\n{}\n\nRemember: Use OVSM syntax and structure your plan with **Expected Plan**, **Available Tools**, **Main Branch**, **Decision Point** (if needed), and **Action** sections.",
            Self::get_ovsm_system_prompt(),
            tools_context
        );

        // For ownPlan mode, send the raw user request without additional instructions
        // The system prompt already contains all the instructions needed
        let user_prompt = user_request.to_string();

        Ok((user_prompt, system_prompt))
    }

    /// Parse OVSM-formatted plan
    fn parse_ovsm_plan(&self, plan_text: &str) -> Result<ToolPlan> {
        eprintln!(
            "DEBUG parse_ovsm_plan: called with {} chars",
            plan_text.len()
        );
        eprintln!(
            "DEBUG parse_ovsm_plan: first 200 chars: {}",
            &plan_text[..plan_text.len().min(200)]
        );

        // PRIORITY 1: Check for XML format first (new preferred format)
        if plan_text.contains("<ovsm_plan") || plan_text.contains("<ovsv_plan") {
            if plan_text.contains("<ovsv_plan") {
                eprintln!("DEBUG parse_ovsm_plan: Found XML format with <ovsv_plan> tag! Normalizing to <ovsm_plan>.");
            } else {
                eprintln!("DEBUG parse_ovsm_plan: Found XML format with <ovsm_plan> tag!");
            }
            return self.parse_osvm_plan_xml(plan_text);
        }

        // PRIORITY 2: Check for simplified format with TIME marker and Main Branch
        let has_time_marker = plan_text.contains("[TIME:") || plan_text.contains("[CONFIDENCE:");
        let has_main_branch = plan_text.contains("Main Branch") && plan_text.contains("```");

        if has_time_marker && has_main_branch {
            eprintln!(
                "DEBUG parse_ovsm_plan: Found simplified format with TIME marker and Main Branch!"
            );
            // This is a valid OVSM plan in simplified format - continue parsing
        } else if !plan_text.contains("Expected Plan") {
            eprintln!("DEBUG parse_ovsm_plan: FAILED - no Expected Plan marker found");
            anyhow::bail!("No OVSM plan structure found");
        } else {
            eprintln!("DEBUG parse_ovsm_plan: Found Expected Plan marker!");
        }

        // Extract reasoning from Main Branch or overview
        let reasoning = if let Some(main_start) = plan_text.find("**Main Branch:**") {
            let main_end = plan_text[main_start..]
                .find("**Decision Point:**")
                .or_else(|| plan_text[main_start..].find("**Action:**"))
                .unwrap_or(plan_text.len() - main_start);

            let main_section = &plan_text[main_start..main_start + main_end];
            // Take first few lines as reasoning
            main_section
                .lines()
                .skip(1)
                .take(3)
                .collect::<Vec<_>>()
                .join("\n")
                .trim()
                .to_string()
        } else {
            "Analyzing request and creating execution plan".to_string()
        };

        // Extract available tools section to identify tool calls
        let mut tools = Vec::new();
        if let Some(tools_start) = plan_text.find("**Available Tools:**") {
            let tools_end = plan_text[tools_start..]
                .find("**Main Branch:**")
                .unwrap_or(plan_text.len() - tools_start);

            let tools_section = &plan_text[tools_start..tools_start + tools_end];

            // Parse tool names from the Available Tools section
            for line in tools_section.lines().skip(1) {
                let line = line.trim();
                if line.is_empty() || line.starts_with("From") || line.starts_with("Custom") {
                    continue;
                }

                // Extract tool names (comma or space separated)
                for word in line.split(|c: char| c == ',' || c == ' ' || c == '-') {
                    let tool_name = word.trim();
                    if !tool_name.is_empty() && tool_name.len() > 2 {
                        tools.push(PlannedTool {
                            server_id: "osvm-mcp".to_string(),
                            tool_name: tool_name.to_string(),
                            args: serde_json::json!({}),
                            reason: format!("Tool from OVSM plan: {}", tool_name),
                        });
                    }
                }
            }
        }

        // Extract expected outcome from Action section
        let expected_outcome = if let Some(action_start) = plan_text.find("**Action:**") {
            let action_section = &plan_text[action_start + 11..];
            action_section
                .lines()
                .take(3)
                .collect::<Vec<_>>()
                .join(" ")
                .trim()
                .to_string()
        } else {
            "Execute the planned operations".to_string()
        };

        // Extract confidence if present
        let confidence_regex = regex::Regex::new(r"\[CONFIDENCE:\s*(\d+)%\]").ok();
        let _confidence = confidence_regex
            .and_then(|re| re.captures(plan_text))
            .and_then(|caps| caps.get(1))
            .and_then(|m| m.as_str().parse::<u8>().ok())
            .unwrap_or(80);

        Ok(ToolPlan {
            reasoning,
            osvm_tools_to_use: tools,
            expected_outcome,
            raw_ovsm_plan: Some(plan_text.to_string()), // Store raw OVSM plan for Phase 2 executor
        })
    }

    fn build_planning_prompt(
        &self,
        user_request: &str,
        available_tools: &HashMap<String, Vec<crate::services::mcp_service::McpTool>>,
    ) -> Result<(String, String)> {
        let mut tools_context = String::new();

        if available_tools.is_empty() {
            tools_context.push_str("No MCP tools are currently available.\n");
        } else {
            tools_context.push_str("Available MCP Tools:\n");
            for (server_id, tools) in available_tools {
                tools_context.push_str(&format!("\nServer: {}\n", server_id));
                for tool in tools {
                    // Skip built-in LISP functions that are incorrectly listed as MCP tools
                    // These are implemented directly in the LISP evaluator
                    if matches!(tool.name.as_str(), "NOW" | "LOG" | "now" | "log") {
                        continue;
                    }

                    let description = tool
                        .description
                        .as_deref()
                        .unwrap_or("No description available");
                    tools_context.push_str(&format!("  - {}: {}\n", tool.name, description));

                    // Include schema information if available
                    if let Ok(schema_str) = serde_json::to_string_pretty(&tool.input_schema) {
                        if schema_str.len() < 2000 {
                            // Only include small schemas inline
                            tools_context.push_str(&format!("    Schema: {}\n", schema_str));
                        }
                    }
                }
            }
        }

        // Create system prompt with all tool descriptions
        let system_prompt = format!(
            r#"You are an AI assistant specialized in creating structured execution plans. When given a user query and available tools, analyze what needs to be done and return ONLY a JSON plan.

            {}
            
            ## OUTPUT FORMAT REQUIREMENTS
            You MUST format your response using the following XML structure:
            
            <osvm_plan>
              <overview>Brief description of what the plan will accomplish</overview>
            
              <tools>
                <tool name="tool_name" priority="high|medium|low">
                  <description>What this tool does</description>
                  <endpoint>API endpoint or RPC method</endpoint>
                  <parameters>
                    <param name="param_name" type="string|number|object" required="true|false">Description</param>
                  </parameters>
                  <expected_output>What data this will return</expected_output>
                </tool>
                <!-- Add more tools as needed -->
              </tools>
            
              <steps>
                <step number="1">
                  <action>What action to take</action>
                  <tool_ref>tool_name from the available tools list</tool_ref>
                  <dependencies>None or step numbers this depends on</dependencies>
                  <data_flow>How data flows from/to this step</data_flow>
                </step>
                <!-- Add more steps as needed -->
              </steps>
            
              <error_handling>
                <scenario>Potential error scenario</scenario>
                <mitigation>How to handle this error</mitigation>
              </error_handling>
            
              <validation>
                <check>What to validate after execution</check>
              </validation>
            
              <estimated_time>Estimated total execution time</estimated_time>
            </osvm_plan>
            
            IMPORTANT:
            - Use ONLY the tools defined in the system prompt above
            - Return ONLY the XML structure. No additional text before or after the XML
            - Ensure tool_ref in steps matches the available tools from the system prompt
            - Be specific about endpoints and parameters
            - Do NOT execute anything, only plan"#,
            tools_context
        );

        // Create user prompt
        let user_prompt = format!(
            "Create an OSVM execution plan for this request: {}",
            user_request
        );

        Ok((user_prompt, system_prompt))
    }

    /// Parse AI response as JSON tool plan
    fn parse_json_tool_plan(&self, ai_response: &str) -> Result<ToolPlan> {
        // First try to parse as JSON
        if let Ok(json_val) = serde_json::from_str::<serde_json::Value>(ai_response) {
            // Use the existing salvage method which handles our JSON structure
            return self
                .salvage_from_json(&json_val)
                .ok_or_else(|| anyhow::anyhow!("Failed to extract tool plan from JSON response"));
        }

        // If JSON parsing fails, try to parse as XML (osvm_plan format)
        if ai_response.contains("<osvm_plan>") {
            return self.parse_osvm_plan_xml(ai_response);
        }

        // If both fail, return error
        anyhow::bail!("Response is neither valid JSON nor XML osvm_plan format");
    }

    /// Parse OSVM plan XML format
    fn parse_osvm_plan_xml(&self, xml_response: &str) -> Result<ToolPlan> {
        let normalized_xml = if xml_response.contains("<ovsv_plan") {
            eprintln!("DEBUG parse_osvm_plan_xml: Normalizing <ovsv_plan> wrapper to <ovsm_plan>");
            xml_response
                .replace("<ovsv_plan", "<ovsm_plan")
                .replace("</ovsv_plan>", "</ovsm_plan>")
        } else {
            xml_response.to_string()
        };
        let xml_str = normalized_xml.as_str();

        // Extract overview
        let overview = self
            .extract_xml_value(xml_str, "overview")
            .unwrap_or_else(|_| "Analyzing request and creating execution plan".to_string());

        // Extract tools from XML
        let mut tools = Vec::new();
        if let Some(tools_section) = self.extract_xml_section(xml_str, "tools") {
            // Find all tool sections
            let mut pos = 0;
            while let Some(tool_start) = tools_section[pos..].find("<tool") {
                let start = pos + tool_start;
                if let Some(tool_end) = tools_section[start..].find("</tool>") {
                    let tool_xml = &tools_section[start..start + tool_end + 7];

                    // Extract tool name
                    let tool_name = self
                        .extract_xml_attribute(tool_xml, "name")
                        .or_else(|| self.extract_xml_value(tool_xml, "name").ok())
                        .unwrap_or_else(|| "unknown".to_string());

                    // Extract tool description
                    let description = self
                        .extract_xml_value(tool_xml, "description")
                        .unwrap_or_else(|_| "Tool execution".to_string());

                    // Extract priority if available
                    let _priority = self.extract_xml_attribute(tool_xml, "priority");

                    tools.push(PlannedTool {
                        server_id: "osvm-mcp".to_string(), // Default server
                        tool_name,
                        args: serde_json::json!({}), // Empty args for now
                        reason: description,
                    });

                    pos = start + tool_end + 7;
                } else {
                    break;
                }
            }
        }

        // Extract expected outcome from overview or separate tag
        let expected_outcome = self
            .extract_xml_value(xml_str, "expected_outcome")
            .or_else(|_| self.extract_xml_value(xml_str, "estimatedTime"))
            .unwrap_or_else(|_| overview.clone());

        Ok(ToolPlan {
            reasoning: overview,
            osvm_tools_to_use: tools,
            expected_outcome,
            raw_ovsm_plan: Some(xml_response.to_string()), // Store full XML for extract_ovsm_code()
        })
    }

    /// Extract attribute value from XML tag
    fn extract_xml_attribute(&self, xml: &str, attribute: &str) -> Option<String> {
        // Look for attribute="value" pattern
        let attr_pattern = format!(r#"{}="([^"]*)""#, attribute);
        if let Ok(re) = regex::Regex::new(&attr_pattern) {
            if let Some(caps) = re.captures(xml) {
                return Some(caps.get(1)?.as_str().to_string());
            }
        }
        None
    }

    /// Parse AI response into structured ToolPlan from OSVM XML format
    fn parse_tool_plan_response(&self, ai_response: &str) -> Result<ToolPlan> {
        // Extract OSVM plan XML
        let plan_start = ai_response.find("<osvm_plan>").ok_or_else(|| {
            anyhow::anyhow!("No <osvm_plan> tag found in AI response: {}", ai_response)
        })?;

        let plan_end = ai_response.find("</osvm_plan>").ok_or_else(|| {
            anyhow::anyhow!("No </osvm_plan> tag found in AI response: {}", ai_response)
        })? + "</osvm_plan>".len();

        let xml_str = &ai_response[plan_start..plan_end];

        // Parse XML using simple string extraction - handle both formats
        let reasoning = self
            .extract_xml_value(xml_str, "reasoning")
            .or_else(|_| self.extract_xml_value(xml_str, "overview"))
            .unwrap_or_else(|_| "Analyzing request and planning execution".to_string());

        let expected_outcome = self
            .extract_xml_value(xml_str, "expected_outcome")
            .or_else(|_| {
                // Try to extract from steps or estimated_time
                if let Some(steps) = self.extract_xml_section(xml_str, "steps") {
                    if let Ok(action) = self.extract_xml_value(&steps, "action") {
                        return Ok(action);
                    }
                }
                self.extract_xml_value(xml_str, "estimated_time")
            })
            .unwrap_or_else(|_| "Execute the planned operations".to_string());

        // Extract tools
        let mut tools = Vec::new();
        if let Some(tools_section) = self.extract_xml_section(xml_str, "tools") {
            // Find all tool sections
            let mut pos = 0;
            while let Some(tool_start) = tools_section[pos..].find("<tool") {
                let start = pos + tool_start;
                if let Some(tool_end) = tools_section[start..].find("</tool>") {
                    let tool_xml = &tools_section[start..start + tool_end + 7];

                    // Try to extract tool name from attribute (e.g., <tool name="accountAnalysis">)
                    let tool_name = if let Some(name_start) = tool_xml.find("name=\"") {
                        let name_start = name_start + 6;
                        if let Some(name_end) = tool_xml[name_start..].find("\"") {
                            tool_xml[name_start..name_start + name_end].to_string()
                        } else {
                            self.extract_xml_value(tool_xml, "tool_name")
                                .ok()
                                .or(self.extract_xml_value(tool_xml, "endpoint").ok())
                                .unwrap_or_else(|| "unknown".to_string())
                        }
                    } else {
                        self.extract_xml_value(tool_xml, "tool_name")
                            .ok()
                            .or(self.extract_xml_value(tool_xml, "endpoint").ok())
                            .unwrap_or_else(|| "unknown".to_string())
                    };

                    // Extract tool fields
                    let server_id = self
                        .extract_xml_value(tool_xml, "server_id")
                        .unwrap_or_else(|_| "osvm-mcp".to_string());
                    let reason = self
                        .extract_xml_value(tool_xml, "reason")
                        .ok()
                        .or(self.extract_xml_value(tool_xml, "description").ok())
                        .unwrap_or_else(|| "Tool execution".to_string());

                    // Extract args as JSON object
                    let args =
                        if let Some(args_section) = self.extract_xml_section(tool_xml, "args") {
                            self.parse_args_to_json(args_section)
                        } else {
                            serde_json::json!({})
                        };

                    tools.push(PlannedTool {
                        server_id,
                        tool_name,
                        args,
                        reason,
                    });

                    pos = start + tool_end + 7;
                } else {
                    break;
                }
            }
        }

        // If we found no tools but detected tool references, create a basic tool entry
        if tools.is_empty() && xml_str.contains("accountAnalysis") {
            tools.push(PlannedTool {
                server_id: "osvm-mcp".to_string(),
                tool_name: "get_balance".to_string(),
                args: serde_json::json!({}),
                reason: "Get wallet balance".to_string(),
            });
        }

        Ok(ToolPlan {
            reasoning,
            osvm_tools_to_use: tools,
            expected_outcome,
            raw_ovsm_plan: None, // XML plans don't have full OVSM text
        })
    }

    /// Attempt to salvage a tool plan from non-standard / malformed responses.
    /// Supports a loose JSON format or partial XML fragments.
    fn salvage_tool_plan_from_response(&self, raw: &str) -> Option<ToolPlan> {
        // 1. Try JSON parsing first
        if let Ok(val) = serde_json::from_str::<serde_json::Value>(raw) {
            if let Some(plan) = self.salvage_from_json(&val) {
                return Some(plan);
            }
        }

        // 2. Try to find partial XML tags even if <osvm_plan> wrapper missing
        let reasoning = self.extract_loose_tag(raw, "reasoning");
        let expected = self
            .extract_loose_tag(raw, "expected_outcome")
            .or_else(|| self.extract_loose_tag(raw, "expectedOutcome"));

        let mut tools: Vec<PlannedTool> = Vec::new();
        // Basic heuristic: lines containing 'tool' and 'server' or 'tool_name'
        // Pre-compile regex outside the loop
        let name_re = regex::Regex::new(r#"(?i)tool[_\- ]?name['"]?[:=]['"]?([A-Za-z0-9_\-]+)"#);

        for line in raw.lines() {
            let lower = line.to_lowercase();
            if lower.contains("tool")
                && (lower.contains("server")
                    || lower.contains("tool_name")
                    || lower.contains("name"))
            {
                // Extract plausible tool_name via regex
                if let Ok(ref name_regex) = name_re {
                    if let Some(caps) = name_regex.captures(line) {
                        let tool_name = caps
                            .get(1)
                            .map(|m| m.as_str())
                            .unwrap_or("unknown")
                            .to_string();
                        tools.push(PlannedTool {
                            server_id: "default".to_string(),
                            tool_name,
                            args: serde_json::json!({}),
                            reason: "Tool identified from response".to_string(),
                        });
                    }
                }
            }
        }

        if reasoning.is_some() || expected.is_some() || !tools.is_empty() {
            return Some(ToolPlan {
                reasoning: reasoning
                    .unwrap_or_else(|| "Processing your request directly".to_string()),
                osvm_tools_to_use: tools,
                expected_outcome: expected
                    .unwrap_or_else(|| "Provide helpful assistance".to_string()),
                raw_ovsm_plan: None, // Salvaged plans don't have full OVSM text
            });
        }

        None
    }

    fn salvage_from_json(&self, val: &serde_json::Value) -> Option<ToolPlan> {
        use serde_json::Value::{Array, Object};
        let reasoning = val
            .get("reasoning")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let expected = val
            .get("expected_outcome")
            .or_else(|| val.get("expectedOutcome"))
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let tools_val = val
            .get("osvm_tools_to_use")
            .or_else(|| val.get("tools"))
            .cloned();

        let mut tools: Vec<PlannedTool> = Vec::new();
        if let Some(Array(arr)) = tools_val {
            for t in arr {
                if let Object(map) = t {
                    let tool_name = map
                        .get("tool_name")
                        .or_else(|| map.get("name"))
                        .and_then(|v| v.as_str())
                        .unwrap_or("unknown");
                    let server_id = map
                        .get("server_id")
                        .and_then(|v| v.as_str())
                        .unwrap_or("default");
                    let reason = map
                        .get("reason")
                        .and_then(|v| v.as_str())
                        .unwrap_or("Tool execution");
                    let args = map
                        .get("args")
                        .cloned()
                        .unwrap_or_else(|| serde_json::json!({}));
                    tools.push(PlannedTool {
                        server_id: server_id.to_string(),
                        tool_name: tool_name.to_string(),
                        args,
                        reason: reason.to_string(),
                    });
                }
            }
        }

        if !reasoning.is_empty() || !expected.is_empty() || !tools.is_empty() {
            return Some(ToolPlan {
                reasoning: if reasoning.is_empty() {
                    "Salvaged reasoning".to_string()
                } else {
                    reasoning
                },
                osvm_tools_to_use: tools,
                expected_outcome: if expected.is_empty() {
                    "Attempt to fulfill request".to_string()
                } else {
                    expected
                },
                raw_ovsm_plan: None, // Salvaged JSON doesn't have full OVSM text
            });
        }
        None
    }

    fn extract_loose_tag(&self, raw: &str, tag: &str) -> Option<String> {
        if let Ok(re) = regex::Regex::new(&format!(r"(?s)<{}>(.*?)</{}>", tag, tag)) {
            if let Some(caps) = re.captures(raw) {
                return Some(caps.get(1)?.as_str().trim().to_string());
            }
        }
        None
    }

    fn truncate_for_reason(s: &str, max: usize) -> String {
        if s.len() <= max {
            s.to_string()
        } else {
            format!("{}…", &s[..max])
        }
    }

    /// Extract value from XML tag
    fn extract_xml_value(&self, xml: &str, tag: &str) -> Result<String> {
        let start_tag = format!("<{}>", tag);
        let end_tag = format!("</{}>", tag);

        let start = xml
            .find(&start_tag)
            .ok_or_else(|| anyhow::anyhow!("Tag {} not found", tag))?
            + start_tag.len();
        let end = xml
            .find(&end_tag)
            .ok_or_else(|| anyhow::anyhow!("Closing tag {} not found", tag))?;

        Ok(xml[start..end].trim().to_string())
    }

    /// Extract XML section content
    fn extract_xml_section(&self, xml: &str, tag: &str) -> Option<String> {
        let start_tag = format!("<{}>", tag);
        let end_tag = format!("</{}>", tag);

        let start = xml.find(&start_tag)? + start_tag.len();
        let end = xml.find(&end_tag)?;

        Some(xml[start..end].to_string())
    }

    /// Parse args XML section to JSON
    fn parse_args_to_json(&self, args_xml: String) -> serde_json::Value {
        let mut args_object = serde_json::Map::new();

        // Simple XML to JSON conversion - find all tags
        let mut remaining = args_xml.trim();
        while !remaining.is_empty() {
            // Find next tag
            if let Some(tag_start) = remaining.find('<') {
                if let Some(tag_end) = remaining[tag_start..].find('>') {
                    let tag = &remaining[tag_start + 1..tag_start + tag_end];
                    if !tag.starts_with('/') {
                        // Opening tag found
                        let close_tag = format!("</{}>", tag);
                        if let Some(close_pos) = remaining.find(&close_tag) {
                            let value_start = tag_start + tag_end + 1;
                            let value = remaining[value_start..close_pos].trim();
                            args_object.insert(
                                tag.to_string(),
                                serde_json::Value::String(value.to_string()),
                            );
                            remaining = &remaining[close_pos + close_tag.len()..];
                            continue;
                        }
                    }
                }
            }
            break;
        }

        serde_json::Value::Object(args_object)
    }

    /// Detect if an AI response has been truncated
    ///
    /// Common indicators of truncation:
    /// - Missing closing XML tags (</code>, </ovsm_plan>)
    /// - Unclosed code blocks (missing closing ```)
    /// - Unbalanced parentheses in LISP code
    /// - Response ends abruptly at ~3KB mark
    fn is_response_truncated(&self, response: &str) -> bool {
        // Check response length is suspiciously close to truncation limits
        let length = response.len();
        let near_3kb = length >= 2900 && length <= 3100;
        
        // Check for missing closing tags in XML-formatted responses
        let has_unclosed_xml = (response.contains("<ovsm_plan>") || response.contains("<ovsv_plan>"))
            && !response.contains("</ovsm_plan>")
            && !response.contains("</ovsv_plan>");
            
        let has_unclosed_code = response.contains("<code>") && !response.contains("</code>");
        
        // Check for unclosed code blocks (markdown)
        let code_blocks = response.matches("```").count();
        let has_unclosed_markdown = code_blocks % 2 != 0;
        
        // Check for severely unbalanced parentheses (LISP code)
        let open_parens = response.matches('(').count();
        let close_parens = response.matches(')').count();
        let severely_unbalanced = open_parens > close_parens + 5; // Allow small imbalance
        
        // Detect if response ends mid-sentence or mid-tag
        let ends_abruptly = response.trim_end().ends_with(',')
            || response.trim_end().ends_with('-')
            || response.trim_end().ends_with('(')
            || response.trim_end().ends_with('<')
            || response.trim_end().ends_with("define");
            
        // Return true if any strong truncation indicator is present
        near_3kb || has_unclosed_xml || has_unclosed_code || has_unclosed_markdown || 
        (severely_unbalanced && length > 1000) || ends_abruptly
    }
    
    /// Handle truncated response by requesting continuation
    ///
    /// This method detects truncation and requests the AI to continue
    /// generating the response from where it left off. It can handle
    /// multiple continuation requests to build the complete response.
    async fn handle_truncated_response(
        &self,
        original_question: &str,
        system_prompt: Option<String>,
        only_plan: Option<bool>,
        partial_response: String,
        debug_mode: bool,
    ) -> Result<String> {
        let mut full_response = partial_response.clone();
        let mut continuation_count = 0;
        const MAX_CONTINUATIONS: u32 = 5; // Prevent infinite loops
        
        while continuation_count < MAX_CONTINUATIONS && self.is_response_truncated(&full_response) {
            continuation_count += 1;
            
            if debug_mode {
                println!("📝 Requesting continuation #{} (current length: {} chars)", 
                    continuation_count, full_response.len());
            }
            
            // Create continuation prompt
            let continuation_prompt = self.create_continuation_prompt(
                original_question,
                &full_response,
                continuation_count,
            );
            
            // Request continuation
            let continuation = self.with_timeout_retry(
                || self.query_osvm_ai_internal(
                    &continuation_prompt,
                    system_prompt.clone(),
                    only_plan,
                    debug_mode
                ),
                2, // Fewer retries for continuations
                debug_mode,
            )
            .await?;
            
            // Append continuation to full response
            full_response = self.merge_continuation(&full_response, &continuation);
            
            if debug_mode {
                println!("✅ Continuation #{} added ({} chars, total: {} chars)",
                    continuation_count, continuation.len(), full_response.len());
            }
        }
        
        if continuation_count >= MAX_CONTINUATIONS && self.is_response_truncated(&full_response) {
            if debug_mode {
                println!("⚠️ Max continuations reached but response still appears truncated");
            }
        }
        
        Ok(full_response)
    }
    
    /// Create a continuation prompt for truncated responses
    fn create_continuation_prompt(
        &self,
        original_question: &str,
        partial_response: &str,
        attempt: u32,
    ) -> String {
        // Find the last complete element to help AI understand where to continue
        let last_200_chars = if partial_response.len() > 200 {
            &partial_response[partial_response.len() - 200..]
        } else {
            partial_response
        };
        
        format!(
            r#"Your previous OVSM code was truncated. Continue ONLY the code, not the entire response.

DO NOT regenerate the plan from scratch. DO NOT add <ovsm_plan> or <code> tags.
Just continue the LISP code from where it was cut off.

Your code was cut off at (last 200 chars):
...{}

IMPORTANT RULES:
1. Start immediately with the next line of LISP code (no preamble)
2. Do NOT repeat any code already written above
3. Do NOT restart the plan or add XML tags
4. Close any open parentheses and complete the code
5. Keep it concise - finish the current logic block

Continue the code now:"#,
            last_200_chars
        )
    }
    
    /// Merge a continuation with the partial response
    ///
    /// This method intelligently merges the continuation, handling cases where
    /// the AI might repeat some context or add transitional text.
    fn merge_continuation(&self, partial: &str, continuation: &str) -> String {
        // Remove common continuation phrases the AI might add
        let cleaned_continuation = continuation
            .trim_start_matches("Continuing from where I left off:")
            .trim_start_matches("Continuing the plan:")
            .trim_start_matches("Continuing:")
            .trim_start_matches("...")
            .trim_start();
        
        // Look for overlap between end of partial and start of continuation
        // (AI might repeat last few tokens for context)
        let overlap_window = 50; // Check last 50 chars for overlap
        if partial.len() > overlap_window {
            let partial_end = &partial[partial.len() - overlap_window..];
            
            // Find if continuation starts with any substring of partial_end
            for i in 0..overlap_window {
                let potential_overlap = &partial_end[i..];
                if cleaned_continuation.starts_with(potential_overlap) {
                    // Found overlap, skip it in continuation
                    let continuation_without_overlap = &cleaned_continuation[potential_overlap.len()..];
                    return format!("{}{}", partial, continuation_without_overlap);
                }
            }
        }
        
        // No overlap found, just concatenate
        format!("{}{}", partial, cleaned_continuation)
    }

    /// Generate a contextual response based on tool results
    pub async fn generate_contextual_response(
        &self,
        original_request: &str,
        tool_results: &[(String, serde_json::Value)], // (tool_name, result)
        expected_outcome: &str,
    ) -> Result<String> {
        let results_summary = tool_results
            .iter()
            .map(|(tool, result)| {
                format!(
                    "Tool '{}' returned: {}",
                    tool,
                    serde_json::to_string_pretty(result).unwrap_or_else(|_| format!("{}", result))
                )
            })
            .collect::<Vec<_>>()
            .join("\n\n");

        let response_prompt = format!(
            r#"
You are an OSVM Agent assistant helping with blockchain operations.

Original User Request: "{}"
Expected Outcome: "{}"

Tool Execution Results:
{}

Generate a helpful response to the user based on the tool results.
Be conversational and explain what was found or accomplished.
If tools failed, explain what went wrong and suggest alternatives.
Focus on what matters to the user.
"#,
            original_request, expected_outcome, results_summary
        );

        self.query_with_debug(&response_prompt, false).await
    }

    /// Create a refinement prompt from OVSM execution error
    ///
    /// This enables self-healing by sending error context back to AI for automatic correction.
    ///
    /// # Arguments
    /// * `original_query` - The user's original blockchain investigation query
    /// * `broken_code` - The OVSM code that failed to execute
    /// * `error_message` - The parse/execution error message
    /// * `attempt_number` - Which retry attempt this is (for context)
    ///
    /// # Returns
    /// A prompt that asks the AI to fix the error and generate corrected OVSM code
    pub fn create_error_refinement_prompt(
        &self,
        original_query: &str,
        broken_code: &str,
        error_message: &str,
        attempt_number: u32,
    ) -> String {
        format!(
            r#"The previous OVSM plan had an error. Please analyze and fix it.

**Original Query:** {}

**Broken Code (Attempt #{}):**
```lisp
{}
```

**Error Message:**
{}

**Common OVSM Errors to Check:**
1. ❌ Infix notation: `(COUNT arr - 1)` → ✅ Use prefix: `(- (COUNT arr) 1)`
2. ❌ Field assignment: `(set! (. obj field) value)` → ✅ Use merge or parallel arrays
3. ❌ Define in loop: Variables defined inside when/if/while → ✅ Define at top
4. ❌ Missing parens: Incomplete expressions → ✅ Balance all parentheses
5. ❌ Wrong tool names: Undefined MCP tools → ✅ Check available tools

**Please provide a CORRECTED OVSM plan with:**
- Fixed syntax errors
- Proper LISP prefix notation
- All variables defined at the top
- Correct tool names from available MCP tools

Respond ONLY with the corrected OVSM plan structure (Expected Plan, Available Tools, Main Branch, Action)."#,
            original_query, attempt_number, broken_code, error_message
        )
    }

    /// Check if an OVSM error is retryable (can be fixed by AI)
    ///
    /// Parse and syntax errors are retryable, but runtime/network errors are not.
    ///
    /// # Arguments
    /// * `error_message` - The error message from OVSM execution
    ///
    /// # Returns
    /// true if the error can potentially be fixed by regenerating code
    pub fn is_retryable_ovsm_error(error_message: &str) -> bool {
        // Parse errors, syntax errors, and type errors are retryable
        error_message.contains("Parse error")
            || error_message.contains("Tokenization error")
            || error_message.contains("Expected identifier")
            || error_message.contains("Expected RightParen")
            || error_message.contains("Expected LeftParen")
            || error_message.contains("Undefined variable")
            || error_message.contains("Undefined tool")
            || error_message.contains("syntax error")
            || error_message.contains("Unexpected token")
            || error_message.contains("Type error")
            || error_message.contains("type mismatch")
            || error_message.contains("expected array")
            || error_message.contains("expected object")
            || error_message.contains("expected string")
            || error_message.contains("expected number")
    }

    /// Create a semantic refinement prompt when code runs but doesn't achieve goal
    ///
    /// This enables ITERATIVE RESEARCH STRATEGY REFINEMENT:
    /// OVSM is an autonomous research agent that iterates on its approach
    /// until the investigation goal is achieved.
    ///
    /// # Arguments
    /// * `original_query` - The user's original research question
    /// * `expected_outcome` - What the current strategy aimed to discover
    /// * `executed_code` - The OVSM strategy that ran successfully
    /// * `actual_result` - What was discovered (intermediate findings)
    /// * `attempt_number` - Strategy iteration number
    ///
    /// # Returns
    /// A prompt that asks AI to devise a NEW RESEARCH STRATEGY based on findings
    pub fn create_semantic_refinement_prompt(
        &self,
        original_query: &str,
        expected_outcome: &str,
        executed_code: &str,
        actual_result: &str,
        attempt_number: u32,
    ) -> String {
        self.create_semantic_refinement_prompt_with_history(
            original_query,
            expected_outcome,
            executed_code,
            actual_result,
            attempt_number,
            &[], // No history for backward compatibility
        )
    }

    /// Create semantic refinement prompt with strategy history
    pub fn create_semantic_refinement_prompt_with_history(
        &self,
        original_query: &str,
        expected_outcome: &str,
        executed_code: &str,
        actual_result: &str,
        attempt_number: u32,
        strategy_history: &[String],
    ) -> String {
        let history_section = if strategy_history.is_empty() {
            String::new()
        } else {
            format!(
                "\n## Previous Strategy Attempts (AVOID REPEATING THESE):\n{}\n",
                strategy_history
                    .iter()
                    .enumerate()
                    .map(|(i, s)| format!("{}. {}", i + 1, s))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        };

        format!(
            r#"## OVSM Research Strategy Iteration Required

**OVSM Philosophy:** OVSM is an autonomous research agent that makes automated decisions based on calling MCP tools iteratively until the investigation goal is achieved. Each iteration should refine the research strategy based on what was learned.

**Original Research Question:** {}

**Current Strategy Goal (Iteration #{}):** {}
{}
**Previous Research Strategy (OVSM Code):**
```lisp
{}
```

**Findings from Previous Strategy:**
```
{}
```

## Research Progress Analysis

The previous OVSM strategy executed successfully and discovered some information, but the investigation is not complete. This is **Strategy Iteration #{}**.

**What We've Learned:**
- The previous strategy revealed: [analyze the actual_result]
- This gives us new information about: [what aspect of the problem]
- We now know: [key insights from the result]

**Why a New Strategy is Needed:**
The investigation needs to continue because:
1. We have partial information but need more details
2. The discovered data points to additional areas to explore
3. The original question requires deeper analysis

## Design Your Next Research Strategy

**CRITICAL REQUIREMENTS:**

1. **DO NOT REPEAT** any approaches from the "Previous Strategy Attempts" list above
2. **MUST BE DIFFERENT** - Use completely different tools, parameters, or analysis angles
3. **BUILD ON FINDINGS** - Use what was discovered, don't start from scratch

**Your NEW strategy must:**

1. **Take a Different Angle**
   - If previous strategies looked at transactions, try accounts
   - If previous strategies analyzed recent data, try historical
   - If previous strategies counted items, try analyzing patterns
   - If previous strategies used one tool, combine multiple tools

2. **Go Deeper Based on Findings**
   - Use specific values discovered (addresses, signatures, amounts)
   - Follow up on anomalies or interesting patterns found
   - Drill into details that were summarized before

3. **Advance Toward the Goal**
   - Each iteration should get closer to answering the original question
   - Focus on what's still unknown or unclear
   - Be more specific based on what we've discovered

**Strategy Pivot Examples:**
- Found wallet address → Now track its token movements
- Got transaction count → Now analyze transaction patterns
- Discovered high activity → Now investigate the cause
- Found totals → Now break down the components

**IMPORTANT:** Start your response with a brief explanation of WHY you're taking this new approach and HOW it differs from previous attempts.

Respond with:
1. **Strategy Reasoning:** (1-2 sentences on why this approach)
2. **OVSM Plan:** (The actual XML structure with code)

Remember: This is **Research Strategy Iteration #{}** - make it meaningfully different!"#,
            original_query,
            expected_outcome,
            attempt_number,
            history_section,
            executed_code,
            actual_result,
            attempt_number + 1,
            attempt_number + 1
        )
    }

    /// Validate if OVSM result matches the expected outcome (semantic validation)
    ///
    /// This checks if the code ran successfully AND produced meaningful results
    /// that align with the stated goal.
    ///
    /// # Arguments
    /// * `result` - The result from OVSM execution
    /// * `expected_outcome` - What the plan said it would achieve
    /// * `original_query` - The user's original request
    ///
    /// # Returns
    /// (is_valid, explanation) - Whether result achieves goal and why/why not
    pub fn validate_ovsm_result(
        result: &str,
        expected_outcome: &str,
        original_query: &str,
    ) -> (bool, String) {
        let result_lower = result.to_lowercase();
        let query_lower = original_query.to_lowercase();

        // Check for clear failure indicators
        if result == "null" || result == "[]" || result.trim().is_empty() {
            return (
                false,
                format!(
                    "Result is empty ({}). Expected: {}",
                    result, expected_outcome
                ),
            );
        }

        // Check if query asked for "find" or "get" but result is null/empty
        if (query_lower.contains("find") || query_lower.contains("get"))
            && (result_lower.contains("null") || result_lower == "0")
        {
            return (
                false,
                format!(
                    "Query asked to find/get data but result is null/empty. Expected: {}",
                    expected_outcome
                ),
            );
        }

        // Check if query asked for "list" or "all" but got single item
        if (query_lower.contains("all") || query_lower.contains("list"))
            && !result_lower.contains("[")
            && !result_lower.contains("array")
        {
            return (false, format!(
                "Query asked for 'all' or 'list' but result appears to be single value. Expected: {}",
                expected_outcome
            ));
        }

        // Check if query asked for "count" or "total" but got non-numeric
        if (query_lower.contains("count")
            || query_lower.contains("total")
            || query_lower.contains("how many"))
            && !result.chars().any(|c| c.is_numeric())
        {
            return (
                false,
                format!(
                    "Query asked for count/total but result is not numeric. Expected: {}",
                    expected_outcome
                ),
            );
        }

        // Check if query asked for "sort" but result doesn't show ordering
        if query_lower.contains("sort") && !result_lower.contains("[") {
            return (false, format!(
                "Query asked for sorted results but result doesn't appear to be an array. Expected: {}",
                expected_outcome
            ));
        }

        // If we get here, result looks reasonable
        (true, "Result appears to match expected outcome".to_string())
    }
    
    /// Validate OVSM code syntax using the parser
    ///
    /// This method parses the OVSM code to check for syntax errors before execution.
    /// It extracts LISP code from various plan formats and validates it.
    ///
    /// # Arguments
    /// * `code` - The OVSM code to validate (can be raw LISP or wrapped in XML/markdown)
    ///
    /// # Returns
    /// Ok(()) if the code is syntactically valid, Err with detailed error message otherwise
    pub fn validate_ovsm_syntax(&self, code: &str) -> Result<()> {
        // Extract LISP code from various formats
        let lisp_code = self.extract_ovsm_lisp_code(code);
        
        // Try to parse the code using OVSM parser
        let mut scanner = OvsmScanner::new(&lisp_code);
        let tokens = scanner.scan_tokens()
            .map_err(|e| anyhow::anyhow!("Tokenization error: {}", e))?;
        
        let mut parser = OvsmParser::new(tokens);
        let _program = parser.parse()
            .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
        
        Ok(())
    }
    
    /// Extract LISP code from various OVSM plan formats
    ///
    /// Handles:
    /// - Raw LISP code
    /// - XML-wrapped plans (<code> tags)
    /// - Markdown code blocks (```lisp)
    /// - Mixed format plans with sections
    ///
    /// # Arguments
    /// * `plan_text` - The full plan text that may contain LISP code
    ///
    /// # Returns
    /// The extracted LISP code, or the original text if no wrapper found
    pub fn extract_ovsm_lisp_code(&self, plan_text: &str) -> String {
        // Try to extract from <code> XML tags
        if let Some(code_start) = plan_text.find("<code>") {
            if let Some(code_end) = plan_text.find("</code>") {
                return plan_text[code_start + 6..code_end].trim().to_string();
            }
        }
        
        // Try to extract from markdown code blocks
        if let Some(code_start) = plan_text.find("```lisp") {
            let start = code_start + 7;
            if let Some(code_end) = plan_text[start..].find("```") {
                return plan_text[start..start + code_end].trim().to_string();
            }
        } else if let Some(code_start) = plan_text.find("```") {
            let start = code_start + 3;
            // Check if it's not another language marker
            if !plan_text[start..].starts_with("xml") && !plan_text[start..].starts_with("json") {
                if let Some(code_end) = plan_text[start..].find("```") {
                    return plan_text[start..start + code_end].trim().to_string();
                }
            }
        }
        
        // Try to extract from Main Branch section (structured plans)
        if plan_text.contains("**Main Branch:**") {
            let mut collected_code = Vec::new();
            let mut in_code_block = false;
            
            for line in plan_text.lines() {
                if line.trim().starts_with("```") {
                    in_code_block = !in_code_block;
                    continue;
                }
                if in_code_block && !line.starts_with("**") {
                    collected_code.push(line);
                }
            }
            
            if !collected_code.is_empty() {
                return collected_code.join("\n");
            }
        }
        
        // If it looks like raw LISP (starts with parenthesis), return as-is
        if plan_text.trim().starts_with('(') {
            return plan_text.trim().to_string();
        }
        
        // Default: return the original text
        plan_text.trim().to_string()
    }
    
    /// Generate a validated OVSM plan with automatic error correction
    ///
    /// This method generates an OVSM plan and validates it, automatically
    /// requesting corrections from the AI if syntax errors are found.
    ///
    /// # Arguments
    /// * `user_request` - The user's original request
    /// * `available_tools` - Available MCP tools for planning
    /// * `max_retries` - Maximum number of correction attempts (default: 3)
    ///
    /// # Returns
    /// A validated ToolPlan with syntactically correct OVSM code
    pub async fn create_validated_tool_plan(
        &self,
        user_request: &str,
        available_tools: &HashMap<String, Vec<crate::services::mcp_service::McpTool>>,
        max_retries: u32,
    ) -> Result<ToolPlan> {
        let mut attempt = 0;
        let mut last_error: Option<String> = None;
        
        loop {
            attempt += 1;
            
            // Generate or regenerate the plan
            let plan = if attempt == 1 {
                // First attempt: normal plan generation
                self.create_tool_plan(user_request, available_tools).await?
            } else {
                // Retry: use error refinement prompt
                let error_msg = last_error.as_ref().unwrap();
                let refinement_prompt = self.create_error_refinement_prompt(
                    user_request,
                    "", // We don't have the broken code in this context
                    error_msg,
                    attempt - 1,
                );
                
                // Generate corrected plan
                let (_, ovsm_system_prompt) = self.build_ovsm_planning_prompt(user_request, available_tools)?;
                let corrected_response = self.query_osvm_ai_with_options(
                    &refinement_prompt,
                    Some(ovsm_system_prompt),
                    Some(true),
                    true,
                ).await?;
                
                // Parse the corrected plan
                self.parse_ovsm_plan(&corrected_response)
                    .or_else(|_| self.parse_osvm_plan_xml(&corrected_response))
                    .unwrap_or_else(|_| ToolPlan {
                        reasoning: "Attempting to correct the plan".to_string(),
                        osvm_tools_to_use: vec![],
                        expected_outcome: "Execute corrected plan".to_string(),
                        raw_ovsm_plan: Some(corrected_response),
                    })
            };
            
            // If we have raw OVSM plan code, validate it
            if let Some(ref raw_plan) = plan.raw_ovsm_plan {
                match self.validate_ovsm_syntax(raw_plan) {
                    Ok(()) => {
                        debug_success!("OVSM plan validated successfully on attempt {}", attempt);
                        return Ok(plan);
                    }
                    Err(e) => {
                        debug_warn!("OVSM validation failed on attempt {}/{}: {}", attempt, max_retries, e);
                        last_error = Some(e.to_string());
                        
                        if attempt >= max_retries {
                            debug_error!("Max validation retries exceeded");
                            // Return the plan anyway, let runtime handle errors
                            return Ok(plan);
                        }
                        
                        // Continue loop for retry
                    }
                }
            } else {
                // No raw OVSM code to validate, return as-is
                return Ok(plan);
            }
        }
    }
}

/// Enhanced tool plan structure for better planning
#[derive(Debug, Serialize, Deserialize)]
struct EnhancedToolPlan {
    reasoning: String,
    confidence: f32,
    osvm_tools_to_use: Vec<EnhancedPlannedTool>,
    expected_outcome: String,
    fallback_plan: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct EnhancedPlannedTool {
    server_id: String,
    tool_name: String,
    args: serde_json::Value,
    reason: String,
    sequence_order: u32,
    depends_on: Vec<String>,
}

/// Tool planning structures
#[derive(Debug, Serialize, Deserialize)]
pub struct ToolPlan {
    pub reasoning: String,
    pub osvm_tools_to_use: Vec<PlannedTool>,
    pub expected_outcome: String,
    pub raw_ovsm_plan: Option<String>, // Phase 2: Store raw OVSM plan for executor
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlannedTool {
    pub server_id: String,
    pub tool_name: String,
    pub args: serde_json::Value,
    pub reason: String,
}

impl Default for AiService {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockito::Server;
    use serde_json::json;

    #[test]
    fn test_parse_ovsv_plan_wrapper() {
        let ai_service = AiService::new();

        let ovsv_plan = r#"
<ovsv_plan>
  <overview>Test overview</overview>
  <tools>
    <tool name="get_account_stats">
      <description>Fetch account stats</description>
    </tool>
  </tools>
  <steps></steps>
  <expected_outcome>Return stats</expected_outcome>
</ovsv_plan>
"#;

        let plan = ai_service
            .parse_ovsm_plan(ovsv_plan)
            .expect("Plan should parse");
        assert_eq!(plan.reasoning, "Test overview");
        assert_eq!(plan.expected_outcome, "Return stats");
        assert!(plan
            .osvm_tools_to_use
            .iter()
            .any(|tool| tool.tool_name == "get_account_stats"));
        assert_eq!(
            plan.raw_ovsm_plan.as_ref().map(|s| s.trim()),
            Some(ovsv_plan.trim())
        );
    }

    #[tokio::test]
    async fn test_ai_service_success() {
        let mut server = Server::new_async().await;

        // Mock successful response
        let mock = server
            .mock("POST", "/api/getAnswer")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "answer": "This is a test AI response"
                })
                .to_string(),
            )
            .create_async()
            .await;

        let mut ai_service = AiService::new();
        ai_service.api_url = server.url() + "/api/getAnswer";
        ai_service.use_openai = false; // Force osvm.ai mode

        let result = ai_service.query("test question").await;

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "This is a test AI response");

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn test_ai_service_error_response() {
        let mut server = Server::new_async().await;

        // Mock error response
        let mock = server
            .mock("POST", "/api/getAnswer")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "error": "Invalid query format"
                })
                .to_string(),
            )
            .create_async()
            .await;

        let mut ai_service = AiService::new();
        ai_service.api_url = server.url() + "/api/getAnswer";
        ai_service.use_openai = false; // Force osvm.ai mode

        let result = ai_service.query("invalid question").await;

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Invalid query format"));

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn test_ai_service_plain_text_response() {
        let mut server = Server::new_async().await;

        // Mock plain text response
        let mock = server
            .mock("POST", "/api/getAnswer")
            .with_status(200)
            .with_header("content-type", "text/plain")
            .with_body("This is a plain text AI response")
            .create_async()
            .await;

        let mut ai_service = AiService::new();
        ai_service.api_url = server.url() + "/api/getAnswer";
        ai_service.use_openai = false; // Force osvm.ai mode

        let result = ai_service.query("test question").await;

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "This is a plain text AI response");

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn test_ai_service_request_format() {
        let mut server = Server::new_async().await;

        // Mock the API to capture what we're sending
        let mock = server
            .mock("POST", "/api/getAnswer")
            .match_header("content-type", "application/json")
            .match_body(r#"{"question":"test question"}"#)
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "answer": "Test response"
                })
                .to_string(),
            )
            .create_async()
            .await;

        let mut ai_service = AiService::new();
        ai_service.api_url = server.url() + "/api/getAnswer";
        ai_service.use_openai = false; // Force osvm.ai mode

        let result = ai_service.query("test question").await;

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "Test response");

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn test_ai_service_error_with_json_response() {
        let mut server = Server::new_async().await;

        // Mock error response with 500 status and JSON error
        let mock = server
            .mock("POST", "/api/getAnswer")
            .with_status(500)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "error": "Failed to process query"
                })
                .to_string(),
            )
            .create_async()
            .await;

        let mut ai_service = AiService::new();
        ai_service.api_url = server.url() + "/api/getAnswer";
        ai_service.use_openai = false; // Force osvm.ai mode

        let result = ai_service.query("invalid question").await;

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("AI API returned error: Failed to process query"));

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn test_openai_service_success() {
        let mut server = Server::new_async().await;

        // Mock successful OpenAI response
        let mock = server
            .mock("POST", "/v1/chat/completions")
            .match_header("authorization", "Bearer test_key")
            .match_header("content-type", "application/json")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "choices": [{
                        "message": {
                            "role": "assistant",
                            "content": "This is a test OpenAI response"
                        }
                    }]
                })
                .to_string(),
            )
            .create_async()
            .await;

        let mut ai_service = AiService::new();
        ai_service.api_url = server.url() + "/v1/chat/completions";
        ai_service.api_key = Some("test_key".to_string());
        ai_service.use_openai = true;

        let result = ai_service.query("test question").await;

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "This is a test OpenAI response");

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn test_openai_service_request_format() {
        let mut server = Server::new_async().await;

        // Mock the API to capture what we're sending to OpenAI
        let mock = server
            .mock("POST", "/v1/chat/completions")
            .match_header("authorization", "Bearer test_key")
            .match_header("content-type", "application/json")
            .match_body(mockito::Matcher::Json(json!({
                "model": "gpt-3.5-turbo",
                "messages": [{
                    "role": "user",
                    "content": "test question"
                }],
                "max_tokens": 1000,
                "temperature": 0.7
            })))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "choices": [{
                        "message": {
                            "role": "assistant",
                            "content": "Test OpenAI response"
                        }
                    }]
                })
                .to_string(),
            )
            .create_async()
            .await;

        let mut ai_service = AiService::new();
        ai_service.api_url = server.url() + "/v1/chat/completions";
        ai_service.api_key = Some("test_key".to_string());
        ai_service.use_openai = true;

        let result = ai_service.query("test question").await;

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "Test OpenAI response");

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn test_endpoint_detection() {
        // Test environment variable detection
        std::env::set_var("OPENAI_URL", "https://api.openai.com/v1/chat/completions");
        std::env::set_var("OPENAI_KEY", "test_key");

        let ai_service = AiService::new();
        assert!(ai_service.use_openai);
        assert_eq!(
            ai_service.api_url,
            "https://api.openai.com/v1/chat/completions"
        );
        assert_eq!(ai_service.api_key, Some("test_key".to_string()));

        // Clean up
        std::env::remove_var("OPENAI_URL");
        std::env::remove_var("OPENAI_KEY");

        // Test fallback to osvm.ai
        let ai_service_fallback = AiService::new();
        assert!(!ai_service_fallback.use_openai);
        assert_eq!(ai_service_fallback.api_url, "https://osvm.ai/api/getAnswer");
        assert_eq!(ai_service_fallback.api_key, None);
    }
}
