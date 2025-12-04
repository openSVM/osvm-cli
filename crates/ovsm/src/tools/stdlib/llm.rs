//! LLM (Large Language Model) operations for OVSM
//!
//! Provides integration with LLM APIs (Ollama, OpenAI, Anthropic) for AI-powered agents.
//!
//! Usage in OVSM:
//! ```lisp
//! ;; Query Ollama (local)
//! (llm-query "ollama" "What is 2+2?" {:model "smollm2:latest"})
//!
//! ;; Query with system prompt
//! (llm-query "ollama" "Analyze this data" {:model "llama3.2" :system "You are a data analyst"})
//!
//! ;; Query OpenAI
//! (llm-query "openai" "Explain quantum computing" {:model "gpt-4"})
//!
//! ;; Query Anthropic
//! (llm-query "anthropic" "Write a haiku" {:model "claude-3-opus"})
//! ```

use crate::error::{Error, Result};
use crate::runtime::Value;
use serde_json::json;
use std::collections::HashMap;
use std::sync::Arc;

/// Default endpoints for LLM providers
const OLLAMA_DEFAULT_URL: &str = "http://localhost:11434/api/generate";
const OPENAI_DEFAULT_URL: &str = "https://api.openai.com/v1/chat/completions";
const ANTHROPIC_DEFAULT_URL: &str = "https://api.anthropic.com/v1/messages";
const OSVM_DEFAULT_URL: &str = "https://osvm.ai/api/getAnswer";

/// LLM query - main entry point
/// `(llm-query provider prompt [options])`
///
/// Provider: "ollama", "openai", "anthropic"
/// Options object can contain:
///   :model - model name (default varies by provider)
///   :system - system prompt
///   :temperature - sampling temperature (0.0-2.0)
///   :max-tokens - max response tokens
///   :url - custom API endpoint
///   :api-key - API key (or use env vars OPENAI_API_KEY, ANTHROPIC_API_KEY)
pub async fn llm_query(args: &[Value]) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::InvalidArguments {
            tool: "llm-query".to_string(),
            reason: "Expected at least provider and prompt".to_string(),
        });
    }

    let provider = match &args[0] {
        Value::String(s) => s.to_lowercase(),
        _ => {
            return Err(Error::InvalidArguments {
                tool: "llm-query".to_string(),
                reason: format!("Expected string provider, got {}", args[0].type_name()),
            })
        }
    };

    let prompt = match &args[1] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(Error::InvalidArguments {
                tool: "llm-query".to_string(),
                reason: format!("Expected string prompt, got {}", args[1].type_name()),
            })
        }
    };

    // Parse options
    let options = if args.len() > 2 {
        match &args[2] {
            Value::Object(obj) => obj.clone(),
            _ => Arc::new(HashMap::new()),
        }
    } else {
        Arc::new(HashMap::new())
    };

    // Extract common options (as_string returns Result, so use .ok())
    let model = options
        .get("model")
        .and_then(|v| v.as_string().ok())
        .map(|s| s.to_string());
    let system = options
        .get("system")
        .and_then(|v| v.as_string().ok())
        .map(|s| s.to_string());
    let temperature = options.get("temperature").and_then(|v| match v {
        Value::Float(f) => Some(*f),
        Value::Int(i) => Some(*i as f64),
        _ => None,
    });
    let max_tokens = options.get("max-tokens").and_then(|v| match v {
        Value::Int(i) => Some(*i as usize),
        _ => None,
    });
    let custom_url = options
        .get("url")
        .and_then(|v| v.as_string().ok())
        .map(|s| s.to_string());
    let api_key = options
        .get("api-key")
        .and_then(|v| v.as_string().ok())
        .map(|s| s.to_string());

    match provider.as_str() {
        "ollama" => query_ollama(&prompt, model, system, temperature, max_tokens, custom_url).await,
        "openai" => {
            query_openai(
                &prompt,
                model,
                system,
                temperature,
                max_tokens,
                custom_url,
                api_key,
            )
            .await
        }
        "anthropic" => {
            query_anthropic(
                &prompt,
                model,
                system,
                temperature,
                max_tokens,
                custom_url,
                api_key,
            )
            .await
        }
        "osvm" => query_osvm(&prompt, custom_url).await,
        _ => Err(Error::InvalidArguments {
            tool: "llm-query".to_string(),
            reason: format!(
                "Unknown provider '{}'. Use 'ollama', 'openai', 'anthropic', or 'osvm'",
                provider
            ),
        }),
    }
}

/// Query Ollama (local LLM)
async fn query_ollama(
    prompt: &str,
    model: Option<String>,
    system: Option<String>,
    temperature: Option<f64>,
    _max_tokens: Option<usize>,
    custom_url: Option<String>,
) -> Result<Value> {
    let url = custom_url.unwrap_or_else(|| OLLAMA_DEFAULT_URL.to_string());
    let model = model.unwrap_or_else(|| "smollm2:latest".to_string());

    let mut body = json!({
        "model": model,
        "prompt": prompt,
        "stream": false,
    });

    if let Some(sys) = system {
        body["system"] = json!(sys);
    }
    if let Some(temp) = temperature {
        body["options"] = json!({"temperature": temp});
    }

    let client = reqwest::Client::new();
    let response =
        client
            .post(&url)
            .json(&body)
            .send()
            .await
            .map_err(|e| Error::ToolExecutionError {
                tool: "llm-query".to_string(),
                reason: format!("Ollama request failed: {}", e),
            })?;

    let status = response.status();
    let text = response
        .text()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("Failed to read Ollama response: {}", e),
        })?;

    if !status.is_success() {
        return Err(Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("Ollama error ({}): {}", status, text),
        });
    }

    let json_resp: serde_json::Value =
        serde_json::from_str(&text).map_err(|e| Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("Failed to parse Ollama response: {}", e),
        })?;

    // Extract response text
    let response_text = json_resp
        .get("response")
        .and_then(|v| v.as_str())
        .unwrap_or("");

    // Return rich result with metadata
    let mut result = HashMap::new();
    result.insert(
        "response".to_string(),
        Value::String(response_text.to_string()),
    );
    result.insert("model".to_string(), Value::String(model));
    result.insert("provider".to_string(), Value::String("ollama".to_string()));

    if let Some(done_reason) = json_resp.get("done_reason").and_then(|v| v.as_str()) {
        result.insert(
            "done_reason".to_string(),
            Value::String(done_reason.to_string()),
        );
    }
    if let Some(total_duration) = json_resp.get("total_duration").and_then(|v| v.as_i64()) {
        result.insert("duration_ns".to_string(), Value::Int(total_duration));
    }

    Ok(Value::Object(Arc::new(result)))
}

/// Query OpenAI API
async fn query_openai(
    prompt: &str,
    model: Option<String>,
    system: Option<String>,
    temperature: Option<f64>,
    max_tokens: Option<usize>,
    custom_url: Option<String>,
    api_key: Option<String>,
) -> Result<Value> {
    let url = custom_url.unwrap_or_else(|| OPENAI_DEFAULT_URL.to_string());
    let model = model.unwrap_or_else(|| "gpt-4o-mini".to_string());
    let api_key = api_key
        .or_else(|| std::env::var("OPENAI_API_KEY").ok())
        .ok_or_else(|| Error::InvalidArguments {
            tool: "llm-query".to_string(),
            reason: "OpenAI API key required. Set OPENAI_API_KEY or pass :api-key option"
                .to_string(),
        })?;

    let mut messages = Vec::new();
    if let Some(sys) = system {
        messages.push(json!({"role": "system", "content": sys}));
    }
    messages.push(json!({"role": "user", "content": prompt}));

    let mut body = json!({
        "model": model,
        "messages": messages,
    });

    if let Some(temp) = temperature {
        body["temperature"] = json!(temp);
    }
    if let Some(max) = max_tokens {
        body["max_tokens"] = json!(max);
    }

    let client = reqwest::Client::new();
    let response = client
        .post(&url)
        .header("Authorization", format!("Bearer {}", api_key))
        .header("Content-Type", "application/json")
        .json(&body)
        .send()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("OpenAI request failed: {}", e),
        })?;

    let status = response.status();
    let text = response
        .text()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("Failed to read OpenAI response: {}", e),
        })?;

    if !status.is_success() {
        return Err(Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("OpenAI error ({}): {}", status, text),
        });
    }

    let json_resp: serde_json::Value =
        serde_json::from_str(&text).map_err(|e| Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("Failed to parse OpenAI response: {}", e),
        })?;

    // Extract response text
    let response_text = json_resp
        .get("choices")
        .and_then(|c| c.get(0))
        .and_then(|c| c.get("message"))
        .and_then(|m| m.get("content"))
        .and_then(|v| v.as_str())
        .unwrap_or("");

    let mut result = HashMap::new();
    result.insert(
        "response".to_string(),
        Value::String(response_text.to_string()),
    );
    result.insert("model".to_string(), Value::String(model));
    result.insert("provider".to_string(), Value::String("openai".to_string()));

    // Include usage stats
    if let Some(usage) = json_resp.get("usage") {
        if let Some(total) = usage.get("total_tokens").and_then(|v| v.as_i64()) {
            result.insert("total_tokens".to_string(), Value::Int(total));
        }
    }

    Ok(Value::Object(Arc::new(result)))
}

/// Query Anthropic API
async fn query_anthropic(
    prompt: &str,
    model: Option<String>,
    system: Option<String>,
    temperature: Option<f64>,
    max_tokens: Option<usize>,
    custom_url: Option<String>,
    api_key: Option<String>,
) -> Result<Value> {
    let url = custom_url.unwrap_or_else(|| ANTHROPIC_DEFAULT_URL.to_string());
    let model = model.unwrap_or_else(|| "claude-3-5-sonnet-20241022".to_string());
    let api_key = api_key
        .or_else(|| std::env::var("ANTHROPIC_API_KEY").ok())
        .ok_or_else(|| Error::InvalidArguments {
            tool: "llm-query".to_string(),
            reason: "Anthropic API key required. Set ANTHROPIC_API_KEY or pass :api-key option"
                .to_string(),
        })?;

    let mut body = json!({
        "model": model,
        "messages": [{"role": "user", "content": prompt}],
        "max_tokens": max_tokens.unwrap_or(1024),
    });

    if let Some(sys) = system {
        body["system"] = json!(sys);
    }
    if let Some(temp) = temperature {
        body["temperature"] = json!(temp);
    }

    let client = reqwest::Client::new();
    let response = client
        .post(&url)
        .header("x-api-key", &api_key)
        .header("anthropic-version", "2023-06-01")
        .header("Content-Type", "application/json")
        .json(&body)
        .send()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("Anthropic request failed: {}", e),
        })?;

    let status = response.status();
    let text = response
        .text()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("Failed to read Anthropic response: {}", e),
        })?;

    if !status.is_success() {
        return Err(Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("Anthropic error ({}): {}", status, text),
        });
    }

    let json_resp: serde_json::Value =
        serde_json::from_str(&text).map_err(|e| Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("Failed to parse Anthropic response: {}", e),
        })?;

    // Extract response text
    let response_text = json_resp
        .get("content")
        .and_then(|c| c.get(0))
        .and_then(|c| c.get("text"))
        .and_then(|v| v.as_str())
        .unwrap_or("");

    let mut result = HashMap::new();
    result.insert(
        "response".to_string(),
        Value::String(response_text.to_string()),
    );
    result.insert("model".to_string(), Value::String(model));
    result.insert(
        "provider".to_string(),
        Value::String("anthropic".to_string()),
    );

    // Include usage stats
    if let Some(usage) = json_resp.get("usage") {
        if let Some(input) = usage.get("input_tokens").and_then(|v| v.as_i64()) {
            result.insert("input_tokens".to_string(), Value::Int(input));
        }
        if let Some(output) = usage.get("output_tokens").and_then(|v| v.as_i64()) {
            result.insert("output_tokens".to_string(), Value::Int(output));
        }
    }

    Ok(Value::Object(Arc::new(result)))
}

/// Query OSVM.ai API (free, no API key needed)
async fn query_osvm(prompt: &str, custom_url: Option<String>) -> Result<Value> {
    let url = custom_url.unwrap_or_else(|| OSVM_DEFAULT_URL.to_string());

    let body = json!({
        "message": prompt,
    });

    let client = reqwest::Client::new();
    let response = client
        .post(&url)
        .header("Content-Type", "application/json")
        .json(&body)
        .send()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("OSVM.ai request failed: {}", e),
        })?;

    let status = response.status();
    let text = response
        .text()
        .await
        .map_err(|e| Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("Failed to read OSVM.ai response: {}", e),
        })?;

    if !status.is_success() {
        return Err(Error::ToolExecutionError {
            tool: "llm-query".to_string(),
            reason: format!("OSVM.ai error ({}): {}", status, text),
        });
    }

    // OSVM.ai returns plain text response (not JSON)
    let mut result = HashMap::new();
    result.insert("response".to_string(), Value::String(text));
    result.insert("provider".to_string(), Value::String("osvm".to_string()));

    Ok(Value::Object(Arc::new(result)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_invalid_provider() {
        let args = vec![
            Value::String("invalid".to_string()),
            Value::String("test prompt".to_string()),
        ];
        let result = llm_query(&args).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unknown provider"));
    }

    #[tokio::test]
    async fn test_missing_args() {
        let args = vec![Value::String("ollama".to_string())];
        let result = llm_query(&args).await;
        assert!(result.is_err());
    }
}
