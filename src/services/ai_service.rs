use anyhow::Result;
use reqwest;
use serde::{Deserialize, Serialize};

#[derive(Serialize)]
struct AiRequest {
    question: String,
}

#[derive(Deserialize)]
struct AiResponse {
    #[serde(default)]
    answer: String,
    #[serde(default)]
    error: Option<String>,
}

pub struct AiService {
    client: reqwest::Client,
    api_url: String,
}

impl AiService {
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::new(),
            api_url: "https://osvm.ai/api/getAnswer".to_string(),
        }
    }

    pub async fn query(&self, question: &str) -> Result<String> {
        let request_body = AiRequest {
            question: question.to_string(),
        };

        println!("🤖 Asking AI: {}", question);

        let response = self
            .client
            .post(&self.api_url)
            .header("Content-Type", "application/json")
            .json(&request_body)
            .send()
            .await?;

        let status = response.status();
        let response_text = response.text().await?;

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

        let result = ai_service.query("invalid question").await;

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("AI API returned error: Failed to process query"));

        mock.assert_async().await;
    }
}
