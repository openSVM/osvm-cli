use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;

const OSVM_API_BASE_URL: &str = "https://osvm.ai/api";

#[derive(Debug, Serialize, Deserialize)]
struct ApiKeyCreateRequest {
    name: String,
    #[serde(rename = "generateAuthLink")]
    generate_auth_link: bool,
}

#[derive(Debug, Deserialize)]
struct ApiKeyCreateResponse {
    success: bool,
    #[serde(rename = "rawKey")]
    raw_key: Option<String>,
    #[serde(rename = "authLink")]
    auth_link: Option<String>,
    message: Option<String>,
}

/// OSVM Authentication Service
/// Handles API key generation and configuration management
pub struct AuthService {
    config_dir: PathBuf,
}

impl AuthService {
    /// Create a new AuthService with the default config directory
    pub fn new() -> Result<Self> {
        let config_dir = dirs::home_dir()
            .context("Failed to find home directory")?
            .join(".config/osvm");

        // Ensure config directory exists
        fs::create_dir_all(&config_dir)
            .with_context(|| format!("Failed to create config directory: {:?}", config_dir))?;

        Ok(Self { config_dir })
    }

    /// Get the path to the auth config file
    fn auth_config_path(&self) -> PathBuf {
        self.config_dir.join("auth.yml")
    }

    /// Check if an OSVM API key exists in the configuration
    pub fn has_api_key(&self) -> bool {
        // Check environment variable first
        if std::env::var("OPENSVM_API_KEY").is_ok() {
            return true;
        }

        // Check config file
        if let Ok(config) = self.load_auth_config() {
            return config.opensvm_api_key.is_some();
        }

        false
    }

    /// Load the authentication configuration from file
    fn load_auth_config(&self) -> Result<AuthConfig> {
        let path = self.auth_config_path();
        if !path.exists() {
            return Ok(AuthConfig::default());
        }

        let content = fs::read_to_string(&path)
            .with_context(|| format!("Failed to read auth config: {:?}", path))?;

        serde_yaml::from_str(&content).context("Failed to parse auth config")
    }

    /// Save the authentication configuration to file
    fn save_auth_config(&self, config: &AuthConfig) -> Result<()> {
        let path = self.auth_config_path();
        let content = serde_yaml::to_string(config).context("Failed to serialize auth config")?;

        fs::write(&path, content)
            .with_context(|| format!("Failed to write auth config: {:?}", path))?;

        // Set restrictive permissions on the config file (Unix only)
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(&path)?.permissions();
            perms.set_mode(0o600); // rw-------
            fs::set_permissions(&path, perms)?;
        }

        Ok(())
    }

    /// Generate a new OSVM API key with auth link
    pub async fn generate_api_key(&self, key_name: &str) -> Result<ApiKeyInfo> {
        println!("🔑 Generating OSVM API key: {}", key_name);

        let client = reqwest::Client::new();
        let request = ApiKeyCreateRequest {
            name: key_name.to_string(),
            generate_auth_link: true,
        };

        let response = client
            .post(format!("{}/auth/api-keys/create", OSVM_API_BASE_URL))
            .json(&request)
            .send()
            .await
            .context("Failed to send API key creation request")?;

        let response_json: ApiKeyCreateResponse = response
            .json()
            .await
            .context("Failed to parse API key creation response")?;

        if !response_json.success {
            anyhow::bail!(
                "API key creation failed: {}",
                response_json
                    .message
                    .unwrap_or_else(|| "Unknown error".to_string())
            );
        }

        let raw_key = response_json
            .raw_key
            .context("API key not returned in response")?;

        let mut auth_link = response_json
            .auth_link
            .context("Auth link not returned in response")?;

        // Fix localhost:3000 bug (backend configuration issue)
        if auth_link.starts_with("http://localhost:3000") {
            auth_link = auth_link.replace("http://localhost:3000", "https://osvm.ai");
        }

        Ok(ApiKeyInfo {
            api_key: raw_key,
            auth_link,
        })
    }

    /// Save the API key to the configuration file
    pub fn save_api_key(&self, api_key: &str) -> Result<()> {
        let mut config = self.load_auth_config()?;
        config.opensvm_api_key = Some(api_key.to_string());
        self.save_auth_config(&config)?;

        println!("✅ API key saved to: {:?}", self.auth_config_path());
        Ok(())
    }

    /// Get the current API key from config or environment
    pub fn get_api_key(&self) -> Option<String> {
        // Environment variable takes precedence
        if let Ok(key) = std::env::var("OPENSVM_API_KEY") {
            return Some(key);
        }

        // Load from config file
        if let Ok(config) = self.load_auth_config() {
            return config.opensvm_api_key;
        }

        None
    }

    /// Interactive authentication flow
    /// Generates API key and prompts user to visit auth link
    pub async fn interactive_auth(&self, key_name: &str) -> Result<String> {
        println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("🔐 OSVM API Authentication Required");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        // Generate API key
        let key_info = self.generate_api_key(key_name).await?;

        // Save to config
        self.save_api_key(&key_info.api_key)?;

        println!("\n📋 API Key Generated:");
        println!("   {}\n", key_info.api_key);

        println!("🔗 Complete Authentication:");
        println!("   Visit this link to bind your API key to your wallet:\n");
        println!("   {}\n", key_info.auth_link);

        println!("⏰ This link expires in 15 minutes.\n");

        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("💡 Tip: Your API key has been saved to:");
        println!("   {:?}", self.auth_config_path());
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        Ok(key_info.api_key)
    }
}

impl Default for AuthService {
    fn default() -> Self {
        Self::new().expect("Failed to create AuthService")
    }
}

/// Information about a generated API key
pub struct ApiKeyInfo {
    pub api_key: String,
    pub auth_link: String,
}

/// Authentication configuration stored in ~/.config/osvm/auth.yml
#[derive(Debug, Serialize, Deserialize, Default)]
struct AuthConfig {
    opensvm_api_key: Option<String>,
}
