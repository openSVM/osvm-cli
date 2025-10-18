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

#[derive(Serialize, Debug)]
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

        debug_print!(
            VerbosityLevel::Basic,
            "Initializing AI service with debug mode: {}",
            debug_mode
        );

        let (api_url, use_openai) = match custom_api_url {
            Some(url) => {
                // Check if it's an OpenAI URL and we have an API key
                if url.contains("openai.com") || url.contains("api.openai.com") {
                    if let Some(key) = env::var("OPENAI_KEY").ok().filter(|k| !k.trim().is_empty())
                    {
                        (url, true)
                    } else {
                        eprintln!("⚠️  OpenAI URL provided but no OPENAI_KEY found, falling back to OSVM AI");
                        ("https://osvm.ai/api/getAnswer".to_string(), false)
                    }
                } else {
                    // Custom URL, treat as external API
                    (url, false)
                }
            }
            None => {
                // Default behavior: use osvm.ai unless explicitly configured for OpenAI
                if let (Some(url), Some(_)) =
                    (env::var("OPENAI_URL").ok(), env::var("OPENAI_KEY").ok())
                {
                    (url, true)
                } else {
                    ("https://osvm.ai/api/getAnswer".to_string(), false)
                }
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

        Self {
            client: reqwest::Client::new(),
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

    async fn query_osvm_ai(&self, question: &str, debug_mode: bool) -> Result<String> {
        self.query_osvm_ai_with_options(question, None, None, debug_mode)
            .await
    }

    async fn query_osvm_ai_with_options(
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
            println!("  systemPrompt: {} chars", system_prompt.as_ref().map(|s| s.len()).unwrap_or(0));
            println!("  ownPlan: {:?}", only_plan);
            println!("  Full JSON: {}", serde_json::to_string_pretty(&request_body)?);
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
            debug_print!(VerbosityLevel::Basic, "Sending OVSM plan request with custom system prompt");
            self.query_osvm_ai_with_options(
                &planning_prompt,
                Some(ovsm_system_prompt.clone()),
                Some(true), // ownPlan=true
                true, // DEBUG MODE ON
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
            raw_ovsm_plan: None,  // No OVSM plan for empty fallback
        })
    }

    /// Get the OVSM system prompt for plan generation
    ///
    /// This is the production system prompt that instructs AI models how to generate
    /// executable OVSM plans with proper control flow (WHILE loops, FOR loops,
    /// IF/THEN/ELSE branching, DECISION/BRANCH structures) and tool calls.
    ///
    /// The prompt is synced with docs/ovsm/OVSM_SYSTEM_PROMPT_COMPACT.md
    fn get_ovsm_system_prompt() -> &'static str {
        // Production OVSM system prompt - synced with docs/ovsm/OVSM_SYSTEM_PROMPT_COMPACT.md
        r#"You are an AI research agent using OVSM (Open Versatile Seeker Mind) language to plan investigations.

# Plan Structure

**Expected Plan:**
[TIME: estimate] [COST: estimate] [CONFIDENCE: %]

**Available Tools:**
[list tools you'll use]

**Main Branch:**
[execution steps with tool calls - may include WHILE loops, FOR loops, IF/THEN/ELSE]

**Decision Point:** [what you're deciding]
  BRANCH A (condition): [actions]
  BRANCH B (condition): [actions]

**Action:** [final output description]

# Syntax

Variables: $name = value
Constants: CONST NAME = value
Tools: toolName(param: $value) or toolName($value)
Loops:
  - FOR $item IN $collection: ... BREAK IF condition
  - WHILE condition: ... body ...
  - ✅ ENDWHILE, ENDFOR, ENDIF are optional (explicit block terminators)
Conditionals: IF condition THEN ... ELSE ...
  - ✅ ENDIF is optional (can use indentation or explicit terminator)
Block Styles (both supported):
  - Python-style: IF cond THEN (indented body) ELSE (indented body)
  - Explicit: IF cond THEN ... ELSE ... ENDIF
  - Same for WHILE/ENDWHILE and FOR/ENDFOR
Parallel: PARALLEL { $a = tool1(); $b = tool2() } WAIT_ALL
Errors: TRY: ... CATCH FATAL/RECOVERABLE: ...
Guards: GUARD $condition
  - ❌ NO "GUARD condition:" - just "GUARD $condition"

# Essential Built-in Tools

**Data Manipulation**: MAP, FILTER, SUM, AVG, COUNT, FLATTEN, APPEND, FIND, SORT
**Statistics**: MEAN, MEDIAN, STDDEV, T_TEST, CORRELATE, PERCENTILE
**Math**: ABS, SQRT, POW, ROUND, MAX, MIN
**Utilities**: NOW, LOG, ERROR, INPUT, derivePDA, parseU64, SLEEP

# MCP Tools Reference

## Transaction Tools
- **sendTransaction(transaction_base64: string)** - Send signed transaction to blockchain
- **sendTransactionWithOptions(transaction_base64, options)** - Send transaction with custom options
- **getTransaction(signature: string)** - Get transaction details by signature
- **getTransactions(wallet: string, limit: number)** - Get recent transactions for a wallet
- **getSignaturesForAddress(address: string, limit: number)** - Get signatures for a wallet
- **getConfirmedTransaction(signature: string)** - Get confirmed transaction details
- **getTransactionCount()** - Get total transaction count

## Account Tools
- **getAccountInfo(address: string)** - Get account details, balance, owner, data
- **getBalance(address: string)** - Get SOL balance for an address
- **getTokenAccounts(owner: string)** - Get all token accounts for wallet
- **getTokenBalance(account: string)** - Get balance of specific token account
- **getMultipleAccounts(addresses: [])** - Get info for multiple accounts in parallel
- **getProgramAccounts(program_id: string)** - Get all accounts owned by a program
- **getTokenSupply(mint: string)** - Get total supply of a token/NFT

## Block & Slot Tools
- **getSlot()** - Get current slot
- **getBlock(slot: number)** - Get block data with all transactions
- **getBlockTime(slot: number)** - Get Unix timestamp for a block
- **getFirstAvailableBlock()** - Get first available block on chain
- **getBlockHeight()** - Get current block height
- **getBlockCommitment(slot: number)** - Get commitment level for a block

## Program Tools
- **getProgramAccounts(program_id: string)** - Get all accounts for a program
- **getAccountData(address: string)** - Get raw account data
- **callProgram(program_id, data, signers)** - Call a program instruction
- **getInstructionDecoder(program_id: string)** - Get instruction format for a program

## Token & NFT Tools
- **getTokenMetadata(mint: string)** - Get token/NFT metadata
- **getTokenHolders(mint: string)** - Get accounts holding a specific token
- **getNFTsByOwner(owner: string)** - Get all NFTs owned by a wallet
- **getTokenDecimals(mint: string)** - Get token decimal places
- **getTokenLargestAccounts(mint: string)** - Get largest token holders

## Cluster & Network Tools
- **getClusterNodes()** - Get information about all cluster nodes
- **getHealth()** - Get cluster health status
- **getVersion()** - Get Solana node/cluster version
- **getNetworkInflation()** - Get current network inflation rate
- **getLargestAccounts(filter: string)** - Get largest accounts on network
- **getStakeActivation(account: string)** - Get stake account activation status

## Commitment & Finality Tools
- **getConfirmedBlocks(start_slot, end_slot)** - Get confirmed blocks in range
- **isBlockhashValid(blockhash: string)** - Check if blockhash is still valid
- **getFeeForMessage(message_base64: string)** - Get fee for a transaction message
- **getRecentBlockhash()** - Get recent blockhash for transactions

## Monitoring & Analytics Tools
- **monitorTransaction(signature: string, timeout: number)** - Monitor transaction until confirmed
- **watchAccountChanges(address: string, callback)** - Get real-time account updates
- **trackBalance(address: string, interval: number)** - Monitor balance changes over time
- **analyzeTransactionPattern(address: string, limit: number)** - Analyze transaction patterns
- **getTransactionStats(start_slot, end_slot)** - Get aggregate transaction statistics

## Validator & Stake Tools
- **getValidatorInfo()** - Get information about validators
- **getStakeActivation(account: string)** - Get stake account details
- **getEpochInfo()** - Get current epoch information
- **getLeaderSchedule()** - Get current leader schedule
- **getSlotLeader()** - Get leader for current slot

## Supply & Economics Tools
- **getSupply()** - Get total SOL supply
- **getInflationRate()** - Get inflation rate
- **getEpochInfo()** - Get epoch details including rewards
- **calculatePriorityFees(recent_transactions: [])** - Calculate recommended priority fees

## Decision Guide for Tool Selection

When planning, choose tools based on:

1. **Need recent data:** Use getBlock, getSlot, getTransaction, getClusterNodes
2. **Query specific account:** Use getAccountInfo, getBalance, getTokenAccounts
3. **Analyze transactions:** Use getTransactions, getSignaturesForAddress, analyzeTransactionPattern
4. **Monitor changes:** Use watchAccountChanges, trackBalance, monitorTransaction
5. **Get token info:** Use getTokenMetadata, getTokenSupply, getTokenHolders
6. **Check cluster health:** Use getHealth, getClusterNodes, getVersion
7. **Batch operations:** Use getMultipleAccounts, getConfirmedBlocks with PARALLEL
8. **Complex logic:** Combine tools with FOR/WHILE loops for iteration

## CRITICAL: Program Address Handling

NEVER fabricate or guess program addresses. When a user mentions a program by name:

**Use these verified addresses from ~/.osvm/addressbook.json:**
- **pumpfun / pump.fun**: 6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P
- **magic_eden**: MEisE1HzV7x8qSJDtSfGkzFmUu9cgPM5EMcarebnMMc
- **raydium**: 675kPX9MHTjS2zt1qrXjVVxt2Y8Qa2NWJJDrai4A2TS
- **jupiter**: JUP6LkbZbjS1jKKwapdHNR8UV6gWZJrMmupMHHfHvWU
- **marinade**: MarBmsSgKXdrQyA87d9KLU2VqQq6j81yh8zVskQcMAq

If a program address is NOT in the address book and NOT explicitly provided by the user:
```
LOG(message: "I need the program address to query. Could you provide the Solana program ID?")
```

**Do NOT assume or make up addresses.** This includes:
- ❌ Using placeholder patterns like "pumpfun111..." or "program111111..."
- ❌ Using all-ones addresses (111111...)
- ❌ Guessing addresses you don't have verified knowledge of

## CRITICAL: RPC API Pagination (Solana Standard)

The Solana RPC has a **HARD LIMIT of 1000 results per call** for methods like `getSignaturesForAddress`:
- ❌ **WRONG**: `limit: 10000` → RPC error "Invalid limit; max 1000"
- ✅ **CORRECT**: `limit: 1000` → Returns 1000 results

**For large data queries, ALWAYS implement pagination:**

```
CONST MAX_RESULTS_PER_CALL = 1000

$all_signatures = []
$before = null
$continue = 1

WHILE $continue == 1:
    IF $before == null THEN
        $batch = getSignaturesForAddress(address: $address, limit: MAX_RESULTS_PER_CALL)
    ELSE
        $batch = getSignaturesForAddress(address: $address, limit: MAX_RESULTS_PER_CALL, before: $before)

    $batch_size = COUNT($batch)

    IF $batch_size == 0 THEN
        $continue = 0
    ELSE
        $all_signatures = APPEND($all_signatures, $batch)

        IF $batch_size < MAX_RESULTS_PER_CALL THEN
            $continue = 0
        ELSE
            $last_idx = $batch_size - 1
            $last_item = $batch[$last_idx]
            $before = $last_item.signature

$total = COUNT($all_signatures)
```

**Why this matters:**
- Solana RPC enforces 1000-result limit to prevent server overload
- Without pagination, you only get 10 results (old default)
- With pagination, you can fetch MILLIONS of results by looping
- Each page takes ~1-2 seconds, so 20,000 results ≈ 20-40 seconds

# Rules

1. List tools in "Available Tools" section
2. Use DECISION/BRANCH for multi-way choices
3. Use WHILE loops for continuous monitoring/iteration
4. Use FOR loops for iterating over collections
5. Use IF/THEN/ELSE for conditional logic within branches
6. Handle errors with TRY/CATCH
7. Run independent ops in PARALLEL
8. Always provide confidence score
9. Use $ for variables, UPPERCASE for constants
10. NO .method() syntax - use functions only
11. For RPC methods, use solana_rpc_call(method: "method_name", params: [param_array]) when high-level tools aren't available
12. Batch related queries using PARALLEL for better performance

# Example with RPC Tool Usage

**Expected Plan:**
[TIME: ~30s] [CONFIDENCE: 90%]

**Available Tools:**
getClusterNodes, getTransaction, monitorTransaction, MEAN, COUNT

**Main Branch:**
$slot = getSlot()
$nodes = getClusterNodes()
$node_count = COUNT($nodes)

PARALLEL {
  $block_time = solana_rpc_call(method: "getBlockTime", params: [$slot])
  $health = getHealth()
}
WAIT_ALL

IF $health == "ok" THEN
  $confidence = 95
ELSE
  $confidence = 60

**Action:**
RETURN {status: $health, nodes: $node_count, block_time: $block_time, confidence: $confidence}

# Important Notes

- Use WHILE loops for continuous monitoring (e.g., "monitor for 1 hour")
- Use FOR loops with BREAK IF for conditional iteration
- Nest IF/THEN/ELSE inside loops for conditional logic
- Use DECISION/BRANCH for multi-way strategy selection
- Always include time estimates and confidence scores
- Handle edge cases with GUARD statements
- For any RPC method not listed above, use solana_rpc_call(method: "method_name", params: [array_of_params])
- RPC proxy is case-sensitive: method names must match exactly (e.g., "getBlockTime", not "getblocktime")
- Params must be passed as an array, even for single parameters

# CRITICAL SYNTAX REMINDERS

**Indentation-Based Blocks (Like Python):**
- ❌ WRONG: WHILE ... DO ... ENDWHILE
- ✅ CORRECT: WHILE condition: (colon, then indented body)
- ❌ WRONG: IF ... THEN ... ENDIF
- ✅ CORRECT: IF condition THEN (indented body)
- ❌ WRONG: GUARD condition: $x != null
- ✅ CORRECT: GUARD $x != null
- ℹ️ NOTE: OVSM is flexible - ENDIF/ENDWHILE/ENDFOR/END markers are supported but OPTIONAL

**Null Values:**
- ❌ WRONG: NULL, NIL, None
- ✅ CORRECT: null (lowercase only)

**Array Indexing:**
- Last element: $array[$last_idx] where $last_idx = COUNT($array) - 1
- No negative indexing support (no $array[-1])

**NO Lambda Functions:**
- ❌ WRONG: MAP($array, lambda $x: $x * 2)
- ✅ CORRECT: Use FOR loops instead:
  ```
  $result = []
  FOR $x IN $array:
      $doubled = $x * 2
      $result = APPEND($result, $doubled)
  ```
- ❌ WRONG: FILTER($array, lambda $x: $x > 5)
- ✅ CORRECT: Use FOR loops with IF:
  ```
  $filtered = []
  FOR $x IN $array:
      IF $x > 5 THEN
          $filtered = APPEND($filtered, $x)
  ```"#
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
            tools_context.push_str("Available MCP Tools:\n");
            for (server_id, tools) in available_tools {
                tools_context.push_str(&format!("\nServer: {}\n", server_id));
                for tool in tools {
                    let description = tool
                        .description
                        .as_deref()
                        .unwrap_or("No description available");
                    tools_context.push_str(&format!("  - {}: {}\n", tool.name, description));
                }
            }
        }

        // Create system prompt with OVSM instructions and available tools
        let system_prompt = format!(
            "{}\n\n# Your Available MCP Tools\n\n{}\n\nRemember: Use OVSM syntax and structure your plan with **Expected Plan**, **Available Tools**, **Main Branch**, **Decision Point** (if needed), and **Action** sections.",
            Self::get_ovsm_system_prompt(),
            tools_context
        );

        // Create user prompt
        let user_prompt = format!(
            "Create an OVSM execution plan for this request: {}\n\nRespond ONLY with the OVSM plan structure. No additional prose before or after.",
            user_request
        );

        Ok((user_prompt, system_prompt))
    }

    /// Parse OVSM-formatted plan
    fn parse_ovsm_plan(&self, plan_text: &str) -> Result<ToolPlan> {
        // Look for the Expected Plan marker
        if !plan_text.contains("**Expected Plan:**") && !plan_text.contains("Expected Plan:") {
            anyhow::bail!("No OVSM plan structure found");
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
            raw_ovsm_plan: Some(plan_text.to_string()),  // Store raw OVSM plan for Phase 2 executor
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
        // Extract overview
        let overview = self
            .extract_xml_value(xml_response, "overview")
            .unwrap_or_else(|_| "Analyzing request and creating execution plan".to_string());

        // Extract tools from XML
        let mut tools = Vec::new();
        if let Some(tools_section) = self.extract_xml_section(xml_response, "tools") {
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
            .extract_xml_value(xml_response, "expected_outcome")
            .or_else(|_| self.extract_xml_value(xml_response, "estimatedTime"))
            .unwrap_or_else(|_| overview.clone());

        Ok(ToolPlan {
            reasoning: overview,
            osvm_tools_to_use: tools,
            expected_outcome,
            raw_ovsm_plan: None,  // XML plans don't have full OVSM text
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
            raw_ovsm_plan: None,  // XML plans don't have full OVSM text
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
                raw_ovsm_plan: None,  // Salvaged plans don't have full OVSM text
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
                raw_ovsm_plan: None,  // Salvaged JSON doesn't have full OVSM text
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
    pub raw_ovsm_plan: Option<String>,  // Phase 2: Store raw OVSM plan for executor
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
