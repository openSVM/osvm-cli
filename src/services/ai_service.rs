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
            anyhow::bail!("AI API request failed with status: {} - Response: {}", status, response_text);
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