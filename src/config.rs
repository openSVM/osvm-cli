use {
    crate::utils::self_repair::read_keypair_with_repair,
    crate::utils::validators::normalize_to_url_if_moniker,
    solana_client::rpc_client::RpcClient,
    solana_commitment_config::CommitmentConfig,
    solana_sdk::{native_token::Sol, signature::Signer},
    std::env,
};

#[derive(Debug)]
pub struct Config {
    pub commitment_config: CommitmentConfig,
    pub default_signer: Box<dyn Signer>,
    pub json_rpc_url: String,
    pub verbose: u8,
    pub no_color: bool,
    pub theme_name: Option<String>,
    pub auto_theme_switching: bool,
}

impl Config {
    /// Validate configuration values to ensure they are reasonable and safe
    ///
    /// # Validation Checks (Bug #12)
    /// - RPC URL must be a valid HTTP/HTTPS URL
    /// - Verbose level must be 0-4 (reasonable debug levels)
    /// - Theme name if provided must match known themes
    ///
    /// # Returns
    /// * `Ok(())` if all validation passes
    /// * `Err()` if any validation fails
    fn validate(&self) -> Result<(), Box<dyn std::error::Error>> {
        // Validate RPC URL format
        if !self.json_rpc_url.starts_with("http://") && !self.json_rpc_url.starts_with("https://") {
            return Err(format!(
                "Invalid RPC URL: must start with http:// or https://, got: {}",
                self.json_rpc_url
            )
            .into());
        }

        // Validate verbose level (0-4 is reasonable)
        if self.verbose > 4 {
            return Err(format!(
                "Verbose level {} is too high (max 4). Using default.",
                self.verbose
            )
            .into());
        }

        // Validate theme name if provided
        if let Some(ref theme) = self.theme_name {
            let valid_themes = vec!["light", "dark", "nord", "dracula", "monokai", "solarized"];
            if !valid_themes.contains(&theme.as_str()) {
                eprintln!(
                    "Warning: Unknown theme '{}'. Valid themes: {:?}",
                    theme, valid_themes
                );
                // Don't fail, just warn - use default theme
            }
        }

        Ok(())
    }

    pub async fn load(
        app_matches: &clap::ArgMatches, // For global flags like no_color, verbose
        sub_matches: &clap::ArgMatches, // For command-specific or overridable flags
    ) -> Result<Self, Box<dyn std::error::Error>> {
        // Determine no_color from global matches
        let no_color = app_matches.contains_id("no_color") || env::var("NO_COLOR").is_ok();
        if no_color {
            colored::control::set_override(false);
        }

        // Determine verbosity. Assuming 'verbose' is a global flag, get it from app_matches.
        // If it can be per-subcommand, clparse.rs and arg definitions would need to confirm.
        // Original main.rs used sub_matches.get_count("verbose") for the Config struct.
        // For now, let's assume it could be on app_matches or sub_matches.
        // If 'verbose' is defined as global in clparse, app_matches.get_count is safer.
        // Let's take it from app_matches, as it's a common global flag.
        let verbose = app_matches.get_count("verbose");

        let cli_config_path = sub_matches
            .get_one::<String>("config_file")
            .map(|s| s.as_str())
            .unwrap_or("~/.config/osvm/config.yml");

        let cli_config = crate::utils::config_loader::Config::load(cli_config_path).unwrap_or_default();

        // OSVM uses its own keypair directory, NOT ~/.config/solana/
        // This prevents accidentally touching user's main Solana keypairs
        let default_osvm_keypair = dirs::home_dir()
            .unwrap_or_else(|| std::path::PathBuf::from("."))
            .join(".config/osvm/keypair.json")
            .to_string_lossy()
            .to_string();

        let keypair_path = sub_matches
            .get_one::<String>("keypair")
            .map(|s| s.to_string())
            .unwrap_or(default_osvm_keypair);

        let signer = match read_keypair_with_repair(&keypair_path).await {
            Ok(signer) => signer,
            Err(err) => {
                return Err(format!("Error reading keypair file {}: {}", keypair_path, err).into());
            }
        };

        // Load theme configuration
        let theme_name = sub_matches
            .get_one::<String>("theme")
            .map(|s| s.to_string());

        // get_flag() returns bool, not Option<bool>, so no need for or_else/unwrap
        let auto_theme_switching = sub_matches.get_flag("auto_theme");

        let config = Self {
            json_rpc_url: normalize_to_url_if_moniker(
                sub_matches
                    .get_one::<String>("json_rpc_url")
                    .map(|s| s.as_str())
                    .unwrap_or(&cli_config.json_rpc_url),
            ),
            default_signer: Box::new(signer), // signer is Keypair, needs to be Box<dyn Signer>
            verbose,
            no_color,
            commitment_config: CommitmentConfig::confirmed(),
            theme_name,
            auto_theme_switching,
        };

        // Validate configuration values (Bug #12)
        config.validate()?;

        Ok(config)
    }

    pub fn setup_logging_and_display_info(&self) -> Result<(), Box<dyn std::error::Error>> {
        match self.verbose {
            0 => solana_logger::setup_with_default("solana=info"),
            1 => solana_logger::setup_with_default("solana=debug"),
            2 => solana_logger::setup_with_default("solana=debug,program=trace"),
            _ => solana_logger::setup_with_default("solana=trace,program=trace"),
        }

        if self.verbose > 0 {
            println!("JSON RPC URL: {}", self.json_rpc_url);
            if self.verbose >= 2 {
                println!("Using keypair: {}", self.default_signer.pubkey());
                println!("Commitment level: {:?}", self.commitment_config.commitment);
                if self.verbose >= 3 {
                    // This RpcClient is created just for display purposes.
                    // The main RpcClient is created later in main.rs.
                    let rpc_client = RpcClient::new(self.json_rpc_url.clone());
                    let balance = rpc_client.get_balance_with_commitment(
                        &self.default_signer.pubkey(),
                        self.commitment_config,
                    )?;
                    println!("Wallet balance: {} SOL", Sol(balance.value));
                }
            }
        }
        Ok(())
    }
}
