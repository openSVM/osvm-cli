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

#[derive(Serialize)]
struct AiRequest {
    question: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    system_prompt: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", rename = "onlyPlan")]
    only_plan: Option<bool>,
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

        // Initialize template manager
        if let Err(e) =
            template_manager.load_from_directory_with_debug("./templates/ai_prompts", debug_mode)
        {
            if debug_mode {
                debug_warn!("Failed to load AI prompt templates: {}", e);
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
        } else {
            if debug_mode {
                println!("🤖 Asking OSVM AI ({}): {}", self.api_url, question);
            }
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
                    println!(
                        "🔍 AI Response received ({} chars)",
                        result.as_ref().unwrap().len()
                    );
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
        self.query_osvm_ai_with_options(question, None, None, debug_mode).await
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
            system_prompt,
            only_plan,
        };

        if debug_mode {
            println!(
                "📤 OSVM AI Request: {}",
                serde_json::to_string_pretty(&request_body)?
            );
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
        let api_key = self.api_key.as_ref().unwrap();

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

    /// Plan which tools to use for a given user request
    pub async fn create_tool_plan(
        &self,
        user_request: &str,
        available_tools: &HashMap<String, Vec<crate::services::mcp_service::McpTool>>,
    ) -> Result<ToolPlan> {
        let (planning_prompt, system_prompt) = self.build_planning_prompt(user_request, available_tools)?;

        // Use the new method with onlyPlan:true and systemPrompt
        let ai_response = if self.use_openai {
            self.query_with_debug(&planning_prompt, false).await?
        } else {
            self.query_osvm_ai_with_options(&planning_prompt, Some(system_prompt), Some(true), false).await?
        };
        
        // Parse the AI response into a ToolPlan
        self.parse_tool_plan_response(&ai_response)
    }

    /// Build a structured prompt for tool planning
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
                    let description = tool.description.as_deref().unwrap_or("No description available");
                    tools_context.push_str(&format!("  - {}: {}\n", tool.name, description));
                    
                    // Include schema information if available
                    if let Ok(schema_str) = serde_json::to_string_pretty(&tool.input_schema) {
                        if schema_str.len() < 2000 { // Only include small schemas inline
                            tools_context.push_str(&format!("    Schema: {}\n", schema_str));
                        }
                    }
                }
            }
        }

        // Create system prompt with all tool descriptions
        let system_prompt = format!(r#"You are an OSVM Agent assistant that creates execution plans.

{}

IMPORTANT RESPONSE FORMAT:
- You MUST respond with an OSVM plan in XML format
- Your response MUST start with <osvm_plan> and end with </osvm_plan>
- Do not include any text outside the XML tags
- Only use tools from the available list above
- Match tool names and server_ids exactly as provided
- Provide realistic arguments based on the tool schemas

Required XML structure:
<osvm_plan>
  <reasoning>Explain your analysis of the request and why you chose these tools</reasoning>
  <confidence>0.0 to 1.0</confidence>
  <tools>
    <tool>
      <server_id>exact_server_id</server_id>
      <tool_name>exact_tool_name</tool_name>
      <args>{{json_object}}</args>
      <reason>Why this specific tool is needed</reason>
    </tool>
  </tools>
  <expected_outcome>What the user should expect as a result</expected_outcome>
</osvm_plan>"#, tools_context);

        // Create user prompt
        let user_prompt = format!("Create an OSVM execution plan for this request: {}", user_request);

        Ok((user_prompt, system_prompt))
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

        // Parse XML using simple string extraction (could use an XML parser for robustness)
        let reasoning = self.extract_xml_value(xml_str, "reasoning")?;
        let expected_outcome = self.extract_xml_value(xml_str, "expected_outcome")?;

        // Extract tools
        let mut tools = Vec::new();
        if let Some(tools_section) = self.extract_xml_section(xml_str, "tools") {
            // Find all tool sections
            let mut pos = 0;
            while let Some(tool_start) = tools_section[pos..].find("<tool>") {
                let start = pos + tool_start;
                if let Some(tool_end) = tools_section[start..].find("</tool>") {
                    let tool_xml = &tools_section[start..start + tool_end + 7];

                    // Extract tool fields
                    let server_id = self.extract_xml_value(tool_xml, "server_id")
                        .unwrap_or_else(|_| "default".to_string());
                    let tool_name = self.extract_xml_value(tool_xml, "tool_name")?;
                    let reason = self.extract_xml_value(tool_xml, "reason")
                        .unwrap_or_else(|_| "Tool execution".to_string());

                    // Extract args as JSON object
                    let args = if let Some(args_section) = self.extract_xml_section(tool_xml, "args") {
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

        Ok(ToolPlan {
            reasoning,
            osvm_tools_to_use: tools,
            expected_outcome,
        })
    }

    /// Extract value from XML tag
    fn extract_xml_value(&self, xml: &str, tag: &str) -> Result<String> {
        let start_tag = format!("<{}>", tag);
        let end_tag = format!("</{}>", tag);

        let start = xml.find(&start_tag)
            .ok_or_else(|| anyhow::anyhow!("Tag {} not found", tag))? + start_tag.len();
        let end = xml.find(&end_tag)
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
                            args_object.insert(tag.to_string(), serde_json::Value::String(value.to_string()));
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
        let results_summary = tool_results.iter()
            .map(|(tool, result)| {
                format!("Tool '{}' returned: {}", tool, 
                    serde_json::to_string_pretty(result)
                        .unwrap_or_else(|_| format!("{}", result))
                )
            })
            .collect::<Vec<_>>()
            .join("\n\n");

        let response_prompt = format!(r#"
You are an OSVM Agent assistant helping with blockchain operations.

Original User Request: "{}"
Expected Outcome: "{}"

Tool Execution Results:
{}

Generate a helpful response to the user based on the tool results.
Be conversational and explain what was found or accomplished.
If tools failed, explain what went wrong and suggest alternatives.
Focus on what matters to the user.
"#, original_request, expected_outcome, results_summary);

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
