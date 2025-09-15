use anyhow::{Context, Result};
use reqwest;
use serde::{Deserialize, Serialize};
use std::env;

#[derive(Serialize)]
struct AiRequest {
    question: String,
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
}

impl AiService {
    pub fn new() -> Self {
        let openai_url = env::var("OPENAI_URL").ok();
        let openai_key = env::var("OPENAI_KEY").ok();
        
        let (api_url, use_openai) = if let (Some(url), Some(_)) = (&openai_url, &openai_key) {
            (url.clone(), true)
        } else {
            ("https://osvm.ai/api/getAnswer".to_string(), false)
        };

        Self {
            client: reqwest::Client::new(),
            api_url,
            api_key: openai_key,
            use_openai,
        }
    }

    pub async fn query(&self, question: &str) -> Result<String> {
        if self.use_openai {
            println!("🤖 Asking OpenAI ({}): {}", self.api_url, question);
        } else {
            println!("🤖 Asking OSVM AI ({}): {}", self.api_url, question);
        }

        let result = if self.use_openai {
            self.query_openai(question).await
        } else {
            self.query_osvm_ai(question).await
        };

        match &result {
            Ok(response) => {
                println!("🔍 AI Response received ({} chars): {}", response.len(), 
                    if response.len() > 200 { 
                        format!("{}...", &response[..200])
                    } else { 
                        response.clone() 
                    });
            }
            Err(e) => {
                println!("❌ AI Response error: {}", e);
            }
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

    async fn query_osvm_ai(&self, question: &str) -> Result<String> {
        let request_body = AiRequest {
            question: question.to_string(),
        };

        println!("📤 OSVM AI Request: {}", serde_json::to_string_pretty(&request_body)?);

        let response = self
            .client
            .post(&self.api_url)
            .header("Content-Type", "application/json")
            .json(&request_body)
            .send()
            .await?;

        let status = response.status();
        let response_text = response.text().await?;

        println!("📥 OSVM AI Response ({}): {}", status, response_text);

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

    async fn query_openai(&self, question: &str) -> Result<String> {
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

        println!("📤 OpenAI Request: {}", serde_json::to_string_pretty(&request_body)?);

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

        println!("📥 OpenAI Response ({}): {}", status, response_text);

        if !status.is_success() {
            anyhow::bail!(
                "OpenAI API request failed with status: {} - Response: {}",
                status,
                response_text
            );
        }

        let openai_response: OpenAiResponse = serde_json::from_str(&response_text)
            .context("Failed to parse OpenAI response")?;

        if let Some(choice) = openai_response.choices.first() {
            Ok(choice.message.content.clone())
        } else {
            anyhow::bail!("No response choices returned from OpenAI API");
        }
    }
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
        assert_eq!(ai_service.api_url, "https://api.openai.com/v1/chat/completions");
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
