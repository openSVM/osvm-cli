use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};

/// AI service configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AiConfig {
    /// AI provider type (openai, ollama, local, anthropic, etc.)
    #[serde(default = "default_provider")]
    pub provider: String,

    /// API endpoint URL
    #[serde(default = "default_api_url")]
    pub api_url: String,

    /// API key (optional for local/ollama)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_key: Option<String>,

    /// Model name (e.g., "gpt-4", "claude-3-opus", "qwen3-coder:30b")
    #[serde(default = "default_model")]
    pub model: String,

    /// Temperature for generation (0.0 to 1.0)
    #[serde(default = "default_temperature")]
    pub temperature: f32,

    /// Max tokens for response
    #[serde(default = "default_max_tokens")]
    pub max_tokens: u32,

    /// Request timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout_secs: u64,
}

fn default_provider() -> String {
    "local".to_string()
}

fn default_api_url() -> String {
    "https://osvm.ai/api/getAnswer".to_string()
}

fn default_model() -> String {
    "default".to_string()
}

fn default_temperature() -> f32 {
    0.7
}

fn default_max_tokens() -> u32 {
    4000
}

fn default_timeout() -> u64 {
    120
}

impl Default for AiConfig {
    fn default() -> Self {
        Self {
            provider: default_provider(),
            api_url: default_api_url(),
            api_key: None,
            model: default_model(),
            temperature: default_temperature(),
            max_tokens: default_max_tokens(),
            timeout_secs: default_timeout(),
        }
    }
}

impl AiConfig {
    /// Get the default config file path
    pub fn default_config_path() -> PathBuf {
        dirs::home_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join(".config/osvm/ai_config.yaml")
    }

    /// Load configuration from file, or create default if not exists
    /// Uses config file exclusively (no environment variable overrides)
    pub fn load() -> Result<Self> {
        let path = Self::default_config_path();
        Self::load_from_path(&path)
    }

    /// Load configuration from a specific path
    pub fn load_from_path(path: &Path) -> Result<Self> {
        if !path.exists() {
            // Create default config if it doesn't exist
            let config = Self::default();
            config.save_to_path(path)?;

            eprintln!("📝 Created default AI configuration at: {}", path.display());
            eprintln!("💡 Use 'osvm settings ai' to configure your AI provider");

            return Ok(config);
        }

        let contents = fs::read_to_string(path)
            .with_context(|| format!("Failed to read AI config from {}", path.display()))?;

        let config: AiConfig = serde_yaml::from_str(&contents)
            .with_context(|| format!("Failed to parse AI config from {}", path.display()))?;

        Ok(config)
    }

    /// Save configuration to file
    pub fn save(&self) -> Result<()> {
        let path = Self::default_config_path();
        self.save_to_path(&path)
    }

    /// Save configuration to a specific path
    pub fn save_to_path(&self, path: &Path) -> Result<()> {
        // Create parent directory if it doesn't exist
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create directory {}", parent.display()))?;
        }

        let yaml = serde_yaml::to_string(self).context("Failed to serialize AI config to YAML")?;

        fs::write(path, yaml)
            .with_context(|| format!("Failed to write AI config to {}", path.display()))?;

        Ok(())
    }

    /// Check if this is using OpenAI-compatible endpoint
    pub fn is_openai_compatible(&self) -> bool {
        self.api_url.contains("openai.com")
            || self.api_url.contains("/v1/chat/completions")
            || self.provider.to_lowercase().contains("openai")
    }

    /// Check if this is using Ollama
    pub fn is_ollama(&self) -> bool {
        self.api_url.contains("localhost:11434")
            || self.api_url.contains("ollama")
            || self.provider.to_lowercase() == "ollama"
    }

    /// Validate configuration
    pub fn validate(&self) -> Result<()> {
        // Check URL is valid
        if !self.api_url.starts_with("http://") && !self.api_url.starts_with("https://") {
            anyhow::bail!("API URL must start with http:// or https://");
        }

        // Check OpenAI endpoints have API keys
        if self.is_openai_compatible() && !self.is_ollama() && self.api_key.is_none() {
            anyhow::bail!(
                "OpenAI-compatible endpoints require an API key. \
                Use 'osvm settings ai set-key <key>' to configure."
            );
        }

        // Check temperature is in valid range
        if self.temperature < 0.0 || self.temperature > 2.0 {
            anyhow::bail!("Temperature must be between 0.0 and 2.0");
        }

        // Check max tokens is reasonable
        if self.max_tokens == 0 || self.max_tokens > 128000 {
            anyhow::bail!("Max tokens must be between 1 and 128000");
        }

        Ok(())
    }

    /// Create a preset configuration for common providers
    pub fn preset(name: &str) -> Result<Self> {
        match name.to_lowercase().as_str() {
            "openai" => Ok(Self {
                provider: "openai".to_string(),
                api_url: "https://api.openai.com/v1/chat/completions".to_string(),
                api_key: None,
                model: "gpt-4o".to_string(),
                temperature: 0.7,
                max_tokens: 4000,
                timeout_secs: 120,
            }),
            "ollama" => Ok(Self {
                provider: "ollama".to_string(),
                api_url: "http://localhost:11434/v1/chat/completions".to_string(),
                api_key: None,
                model: "qwen3-coder:30b".to_string(),
                temperature: 0.7,
                max_tokens: 4000,
                timeout_secs: 120,
            }),
            "local" => Ok(Self::default()),
            "anthropic" => Ok(Self {
                provider: "anthropic".to_string(),
                api_url: "https://api.anthropic.com/v1/messages".to_string(),
                api_key: None,
                model: "claude-3-5-sonnet-20241022".to_string(),
                temperature: 0.7,
                max_tokens: 4000,
                timeout_secs: 120,
            }),
            _ => anyhow::bail!(
                "Unknown preset: {}. Available: openai, ollama, local, anthropic",
                name
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_default_config() {
        let config = AiConfig::default();
        assert_eq!(config.provider, "local");
        assert_eq!(config.api_url, "https://osvm.ai/api/getAnswer");
        assert_eq!(config.temperature, 0.7);
    }

    #[test]
    fn test_save_and_load() {
        let dir = tempdir().unwrap();
        let config_path = dir.path().join("ai_config.yaml");

        let config = AiConfig::default();
        config.save_to_path(&config_path).unwrap();

        let loaded = AiConfig::load_from_path(&config_path).unwrap();
        assert_eq!(loaded.provider, config.provider);
        assert_eq!(loaded.api_url, config.api_url);
    }

    #[test]
    fn test_preset_openai() {
        let config = AiConfig::preset("openai").unwrap();
        assert_eq!(config.provider, "openai");
        assert!(config.api_url.contains("openai.com"));
        assert_eq!(config.model, "gpt-4o");
    }

    #[test]
    fn test_preset_ollama() {
        let config = AiConfig::preset("ollama").unwrap();
        assert_eq!(config.provider, "ollama");
        assert!(config.api_url.contains("11434"));
        assert_eq!(config.model, "qwen3-coder:30b");
    }

    #[test]
    fn test_validation() {
        let mut config = AiConfig::default();
        assert!(config.validate().is_ok());

        // Invalid URL
        config.api_url = "not-a-url".to_string();
        assert!(config.validate().is_err());

        // Invalid temperature
        config.api_url = "http://localhost:3000".to_string();
        config.temperature = 3.0;
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_is_openai_compatible() {
        let mut config = AiConfig::default();
        assert!(!config.is_openai_compatible());

        config.api_url = "https://api.openai.com/v1/chat/completions".to_string();
        assert!(config.is_openai_compatible());

        config.api_url = "http://localhost:11434/v1/chat/completions".to_string();
        assert!(config.is_openai_compatible());
    }

    #[test]
    fn test_is_ollama() {
        let mut config = AiConfig::default();
        assert!(!config.is_ollama());

        config.api_url = "http://localhost:11434/v1/chat/completions".to_string();
        assert!(config.is_ollama());

        config.provider = "ollama".to_string();
        assert!(config.is_ollama());
    }
}
