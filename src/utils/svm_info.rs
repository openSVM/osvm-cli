//! SVM information utilities
//! Provides functionality to list and get information about SVMs

use {
    colored::Colorize,
    serde::{Deserialize, Serialize},
    solana_client::rpc_client::RpcClient,
    solana_sdk::commitment_config::CommitmentConfig,
    std::{collections::HashMap, error::Error, fmt},
};

/// SVM error types
#[derive(Debug)]
pub enum SvmError {
    /// Network error
    NetworkError(String),
    /// SVM not found
    NotFound(String),
    /// Other error
    Other(String),
}

impl fmt::Display for SvmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SvmError::NetworkError(msg) => write!(f, "Network error: {}", msg),
            SvmError::NotFound(msg) => write!(f, "SVM not found: {}", msg),
            SvmError::Other(msg) => write!(f, "Error: {}", msg),
        }
    }
}

impl Error for SvmError {}

/// RPC node information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RpcNodeInfo {
    /// URL of the RPC node
    pub url: String,
    /// Whether this is an official RPC
    pub is_official: bool,
    /// TPS (transactions per second)
    pub tps: f64,
    /// Response time (ms)
    pub response_time_ms: f64,
    /// Location
    pub location: String,
}

/// Network information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkInfo {
    /// Network name (mainnet, testnet, devnet)
    pub name: String,
    /// Available RPC nodes
    pub rpc_nodes: Vec<RpcNodeInfo>,
    /// Number of validators
    pub validator_count: u32,
    /// Current TPS
    pub current_tps: f64,
    /// Network version
    pub version: String,
    /// Block height
    pub block_height: u64,
    /// Total supply
    pub total_supply: u64,
    /// Staked tokens
    pub staked_tokens: u64,
}

/// System requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemRequirements {
    /// CPU cores
    pub cpu_cores: u8,
    /// RAM in GB
    pub ram_gb: u16,
    /// Storage in GB
    pub storage_gb: u16,
    /// Bandwidth in Mbps
    pub bandwidth_mbps: u16,
    /// Recommended cloud instance type
    pub recommended_instance: String,
    /// Estimated monthly cost (USD)
    pub estimated_monthly_cost: f64,
}

/// Detailed SVM information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SvmInfo {
    /// SVM name
    pub name: String,
    /// SVM display name
    pub display_name: String,
    /// Description
    pub description: String,
    /// Website
    pub website: String,
    /// Documentation URL
    pub documentation_url: String,
    /// Explorer URL
    pub explorer_url: String,
    /// GitHub URL
    pub github_url: String,
    /// Logo URL
    pub logo_url: String,
    /// Whether this SVM can be installed as a validator
    pub can_install_validator: bool,
    /// Whether this SVM can be installed as an RPC node
    pub can_install_rpc: bool,
    /// Token symbol
    pub token_symbol: String,
    /// Current price in USD
    pub token_price_usd: f64,
    /// Network information
    pub networks: HashMap<String, NetworkInfo>,
    /// System requirements for running a validator
    pub validator_requirements: SystemRequirements,
    /// System requirements for running an RPC node
    pub rpc_requirements: SystemRequirements,
}

/// Get information about all SVMs
///
/// # Arguments
/// * `client` - RPC client
/// * `commitment_config` - Commitment config
///
/// # Returns
/// * `Result<HashMap<String, SvmInfo>, Box<dyn Error>>` - Map of SVM names to info
pub fn list_all_svms(
    _client: &RpcClient,
    _commitment_config: CommitmentConfig,
) -> Result<HashMap<String, SvmInfo>, Box<dyn Error>> {
    // In a real implementation, this would query the blockchain for SVM information
    // For this prototype, we'll return a static list

    let mut svms = HashMap::new();

    // Add Solana
    let solana_info = SvmInfo {
        name: "solana".to_string(),
        display_name: "Solana".to_string(),
        description: "High-performance blockchain supporting builders around the world".to_string(),
        website: "https://solana.com".to_string(),
        documentation_url: "https://docs.solana.com".to_string(),
        explorer_url: "https://explorer.solana.com".to_string(),
        github_url: "https://github.com/solana-labs/solana".to_string(),
        logo_url: "https://solana.com/src/img/branding/solanaLogoMark.svg".to_string(),
        can_install_validator: true,
        can_install_rpc: true,
        token_symbol: "SOL".to_string(),
        token_price_usd: 142.78,
        networks: {
            let mut networks = HashMap::new();

            // Mainnet
            networks.insert(
                "mainnet".to_string(),
                NetworkInfo {
                    name: "mainnet".to_string(),
                    rpc_nodes: vec![
                        RpcNodeInfo {
                            url: "https://api.mainnet-beta.solana.com".to_string(),
                            is_official: true,
                            tps: 2450.0,
                            response_time_ms: 85.3,
                            location: "US West".to_string(),
                        },
                        RpcNodeInfo {
                            url: "https://solana-api.projectserum.com".to_string(),
                            is_official: false,
                            tps: 2430.0,
                            response_time_ms: 92.7,
                            location: "US East".to_string(),
                        },
                    ],
                    validator_count: 1654,
                    current_tps: 2458.7,
                    version: "1.16.0".to_string(),
                    block_height: 225_450_000,
                    total_supply: 560_000_000,
                    staked_tokens: 372_000_000,
                },
            );

            // Testnet
            networks.insert(
                "testnet".to_string(),
                NetworkInfo {
                    name: "testnet".to_string(),
                    rpc_nodes: vec![RpcNodeInfo {
                        url: "https://api.testnet.solana.com".to_string(),
                        is_official: true,
                        tps: 1850.0,
                        response_time_ms: 75.1,
                        location: "US West".to_string(),
                    }],
                    validator_count: 245,
                    current_tps: 750.5,
                    version: "1.16.0".to_string(),
                    block_height: 189_750_000,
                    total_supply: 560_000_000,
                    staked_tokens: 15_000_000,
                },
            );

            // Devnet
            networks.insert(
                "devnet".to_string(),
                NetworkInfo {
                    name: "devnet".to_string(),
                    rpc_nodes: vec![RpcNodeInfo {
                        url: "https://api.devnet.solana.com".to_string(),
                        is_official: true,
                        tps: 1500.0,
                        response_time_ms: 68.4,
                        location: "US West".to_string(),
                    }],
                    validator_count: 95,
                    current_tps: 325.2,
                    version: "1.16.0".to_string(),
                    block_height: 205_850_000,
                    total_supply: 560_000_000,
                    staked_tokens: 5_000_000,
                },
            );

            networks
        },
        validator_requirements: SystemRequirements {
            cpu_cores: 12,
            ram_gb: 128,
            storage_gb: 2048,
            bandwidth_mbps: 1000,
            recommended_instance: "AWS c5.4xlarge".to_string(),
            estimated_monthly_cost: 750.0,
        },
        rpc_requirements: SystemRequirements {
            cpu_cores: 16,
            ram_gb: 256,
            storage_gb: 4096,
            bandwidth_mbps: 2000,
            recommended_instance: "AWS c5.9xlarge".to_string(),
            estimated_monthly_cost: 1500.0,
        },
    };

    // Add Sonic
    let sonic_info = SvmInfo {
        name: "sonic".to_string(),
        display_name: "Sonic".to_string(),
        description: "Super-fast layer 1 blockchain with ZKML capabilities".to_string(),
        website: "https://sonic.xyz".to_string(),
        documentation_url: "https://docs.sonic.xyz".to_string(),
        explorer_url: "https://explorer.sonic.xyz".to_string(),
        github_url: "https://github.com/sonic-labs/sonic".to_string(),
        logo_url: "https://sonic.xyz/logo.svg".to_string(),
        can_install_validator: true,
        can_install_rpc: true,
        token_symbol: "SONIC".to_string(),
        token_price_usd: 8.45,
        networks: {
            let mut networks = HashMap::new();

            // Mainnet
            networks.insert(
                "mainnet".to_string(),
                NetworkInfo {
                    name: "mainnet".to_string(),
                    rpc_nodes: vec![
                        RpcNodeInfo {
                            url: "https://api.sonic.xyz".to_string(),
                            is_official: true,
                            tps: 4800.0,
                            response_time_ms: 45.3,
                            location: "US West".to_string(),
                        },
                        RpcNodeInfo {
                            url: "https://rpc.sonic.community".to_string(),
                            is_official: false,
                            tps: 4750.0,
                            response_time_ms: 52.4,
                            location: "EU Central".to_string(),
                        },
                    ],
                    validator_count: 120,
                    current_tps: 4820.5,
                    version: "0.9.2".to_string(),
                    block_height: 15_250_000,
                    total_supply: 100_000_000,
                    staked_tokens: 45_000_000,
                },
            );

            // Testnet
            networks.insert(
                "testnet".to_string(),
                NetworkInfo {
                    name: "testnet".to_string(),
                    rpc_nodes: vec![RpcNodeInfo {
                        url: "https://testnet-api.sonic.xyz".to_string(),
                        is_official: true,
                        tps: 4500.0,
                        response_time_ms: 48.2,
                        location: "US West".to_string(),
                    }],
                    validator_count: 35,
                    current_tps: 2250.8,
                    version: "0.9.2".to_string(),
                    block_height: 8_750_000,
                    total_supply: 100_000_000,
                    staked_tokens: 8_000_000,
                },
            );

            // Devnet
            networks.insert(
                "devnet".to_string(),
                NetworkInfo {
                    name: "devnet".to_string(),
                    rpc_nodes: vec![RpcNodeInfo {
                        url: "https://devnet-api.sonic.xyz".to_string(),
                        is_official: true,
                        tps: 4200.0,
                        response_time_ms: 51.0,
                        location: "US West".to_string(),
                    }],
                    validator_count: 18,
                    current_tps: 950.4,
                    version: "0.9.3-dev".to_string(),
                    block_height: 5_350_000,
                    total_supply: 100_000_000,
                    staked_tokens: 2_000_000,
                },
            );

            networks
        },
        validator_requirements: SystemRequirements {
            cpu_cores: 8,
            ram_gb: 32,
            storage_gb: 1024,
            bandwidth_mbps: 500,
            recommended_instance: "AWS c5.2xlarge".to_string(),
            estimated_monthly_cost: 450.0,
        },
        rpc_requirements: SystemRequirements {
            cpu_cores: 16,
            ram_gb: 64,
            storage_gb: 2048,
            bandwidth_mbps: 1000,
            recommended_instance: "AWS c5.4xlarge".to_string(),
            estimated_monthly_cost: 800.0,
        },
    };

    // Add Eclipse SVM
    let eclipse_info = SvmInfo {
        name: "eclipse".to_string(),
        display_name: "Eclipse".to_string(),
        description: "High-performance rollup platform for Solana applications".to_string(),
        website: "https://eclipse.xyz".to_string(),
        documentation_url: "https://docs.eclipse.xyz".to_string(),
        explorer_url: "https://explorer.eclipse.xyz".to_string(),
        github_url: "https://github.com/eclipse-labs/eclipse".to_string(),
        logo_url: "https://eclipse.xyz/logo.svg".to_string(),
        can_install_validator: true,
        can_install_rpc: true,
        token_symbol: "ECLP".to_string(),
        token_price_usd: 3.27,
        networks: {
            let mut networks = HashMap::new();

            // Mainnet
            networks.insert(
                "mainnet".to_string(),
                NetworkInfo {
                    name: "mainnet".to_string(),
                    rpc_nodes: vec![
                        RpcNodeInfo {
                            url: "https://api.eclipse.xyz".to_string(),
                            is_official: true,
                            tps: 5200.0,
                            response_time_ms: 38.7,
                            location: "US West".to_string(),
                        },
                        RpcNodeInfo {
                            url: "https://eclipse-api.stakingcrypto.xyz".to_string(),
                            is_official: false,
                            tps: 5150.0,
                            response_time_ms: 42.3,
                            location: "EU Central".to_string(),
                        },
                    ],
                    validator_count: 85,
                    current_tps: 5180.2,
                    version: "0.7.1".to_string(),
                    block_height: 12_350_000,
                    total_supply: 1_000_000_000,
                    staked_tokens: 280_000_000,
                },
            );

            // Testnet
            networks.insert(
                "testnet".to_string(),
                NetworkInfo {
                    name: "testnet".to_string(),
                    rpc_nodes: vec![RpcNodeInfo {
                        url: "https://testnet-api.eclipse.xyz".to_string(),
                        is_official: true,
                        tps: 5000.0,
                        response_time_ms: 35.2,
                        location: "US West".to_string(),
                    }],
                    validator_count: 28,
                    current_tps: 1850.5,
                    version: "0.7.2".to_string(),
                    block_height: 6_750_000,
                    total_supply: 1_000_000_000,
                    staked_tokens: 65_000_000,
                },
            );

            // Devnet
            networks.insert(
                "devnet".to_string(),
                NetworkInfo {
                    name: "devnet".to_string(),
                    rpc_nodes: vec![RpcNodeInfo {
                        url: "https://devnet-api.eclipse.xyz".to_string(),
                        is_official: true,
                        tps: 4800.0,
                        response_time_ms: 32.8,
                        location: "US West".to_string(),
                    }],
                    validator_count: 12,
                    current_tps: 820.3,
                    version: "0.7.3-dev".to_string(),
                    block_height: 3_250_000,
                    total_supply: 1_000_000_000,
                    staked_tokens: 25_000_000,
                },
            );

            networks
        },
        validator_requirements: SystemRequirements {
            cpu_cores: 10,
            ram_gb: 64,
            storage_gb: 2048,
            bandwidth_mbps: 750,
            recommended_instance: "AWS c5.4xlarge".to_string(),
            estimated_monthly_cost: 650.0,
        },
        rpc_requirements: SystemRequirements {
            cpu_cores: 16,
            ram_gb: 128,
            storage_gb: 4096,
            bandwidth_mbps: 1500,
            recommended_instance: "AWS c5.9xlarge".to_string(),
            estimated_monthly_cost: 1200.0,
        },
    };

    // Add Soon SVM
    let soon_info = SvmInfo {
        name: "soon".to_string(),
        display_name: "Soon".to_string(),
        description: "Next-generation blockchain for decentralized AI applications".to_string(),
        website: "https://soon.network".to_string(),
        documentation_url: "https://docs.soon.network".to_string(),
        explorer_url: "https://explorer.soon.network".to_string(),
        github_url: "https://github.com/soon-labs/soon-blockchain".to_string(),
        logo_url: "https://soon.network/logo.svg".to_string(),
        can_install_validator: true,
        can_install_rpc: true,
        token_symbol: "SOON".to_string(),
        token_price_usd: 5.12,
        networks: {
            let mut networks = HashMap::new();

            // Mainnet
            networks.insert(
                "mainnet".to_string(),
                NetworkInfo {
                    name: "mainnet".to_string(),
                    rpc_nodes: vec![RpcNodeInfo {
                        url: "https://api.soon.network".to_string(),
                        is_official: true,
                        tps: 3850.0,
                        response_time_ms: 49.2,
                        location: "US East".to_string(),
                    }],
                    validator_count: 65,
                    current_tps: 3780.5,
                    version: "0.5.2".to_string(),
                    block_height: 8_450_000,
                    total_supply: 500_000_000,
                    staked_tokens: 175_000_000,
                },
            );

            // Testnet
            networks.insert(
                "testnet".to_string(),
                NetworkInfo {
                    name: "testnet".to_string(),
                    rpc_nodes: vec![RpcNodeInfo {
                        url: "https://testnet-api.soon.network".to_string(),
                        is_official: true,
                        tps: 3700.0,
                        response_time_ms: 45.1,
                        location: "US East".to_string(),
                    }],
                    validator_count: 22,
                    current_tps: 1580.2,
                    version: "0.5.3".to_string(),
                    block_height: 4_250_000,
                    total_supply: 500_000_000,
                    staked_tokens: 45_000_000,
                },
            );

            networks
        },
        validator_requirements: SystemRequirements {
            cpu_cores: 12,
            ram_gb: 64,
            storage_gb: 1536,
            bandwidth_mbps: 700,
            recommended_instance: "AWS c5.4xlarge".to_string(),
            estimated_monthly_cost: 550.0,
        },
        rpc_requirements: SystemRequirements {
            cpu_cores: 20,
            ram_gb: 128,
            storage_gb: 3072,
            bandwidth_mbps: 1200,
            recommended_instance: "AWS c5.9xlarge".to_string(),
            estimated_monthly_cost: 1100.0,
        },
    };

    // Add OpenSVM
    let opensvm_info = SvmInfo {
        name: "opensvm".to_string(),
        display_name: "OpenSVM".to_string(),
        description: "Community-driven open Solana Virtual Machine with enhanced privacy features"
            .to_string(),
        website: "https://opensvm.org".to_string(),
        documentation_url: "https://docs.opensvm.org".to_string(),
        explorer_url: "https://explorer.opensvm.org".to_string(),
        github_url: "https://github.com/opensvm/opensvm-core".to_string(),
        logo_url: "https://opensvm.org/logo.svg".to_string(),
        can_install_validator: true,
        can_install_rpc: true,
        token_symbol: "OSVM".to_string(),
        token_price_usd: 2.34,
        networks: {
            let mut networks = HashMap::new();

            // Mainnet
            networks.insert(
                "mainnet".to_string(),
                NetworkInfo {
                    name: "mainnet".to_string(),
                    rpc_nodes: vec![
                        RpcNodeInfo {
                            url: "https://api.opensvm.org".to_string(),
                            is_official: true,
                            tps: 3200.0,
                            response_time_ms: 55.4,
                            location: "US West".to_string(),
                        },
                        RpcNodeInfo {
                            url: "https://opensvm.chainflow.io".to_string(),
                            is_official: false,
                            tps: 3150.0,
                            response_time_ms: 58.7,
                            location: "EU Central".to_string(),
                        },
                    ],
                    validator_count: 78,
                    current_tps: 3180.5,
                    version: "0.8.3".to_string(),
                    block_height: 10_250_000,
                    total_supply: 200_000_000,
                    staked_tokens: 95_000_000,
                },
            );

            // Testnet
            networks.insert(
                "testnet".to_string(),
                NetworkInfo {
                    name: "testnet".to_string(),
                    rpc_nodes: vec![RpcNodeInfo {
                        url: "https://testnet-api.opensvm.org".to_string(),
                        is_official: true,
                        tps: 3100.0,
                        response_time_ms: 52.3,
                        location: "US West".to_string(),
                    }],
                    validator_count: 24,
                    current_tps: 1350.7,
                    version: "0.8.4".to_string(),
                    block_height: 6_850_000,
                    total_supply: 200_000_000,
                    staked_tokens: 35_000_000,
                },
            );

            networks
        },
        validator_requirements: SystemRequirements {
            cpu_cores: 8,
            ram_gb: 32,
            storage_gb: 1024,
            bandwidth_mbps: 500,
            recommended_instance: "AWS c5.2xlarge".to_string(),
            estimated_monthly_cost: 400.0,
        },
        rpc_requirements: SystemRequirements {
            cpu_cores: 16,
            ram_gb: 64,
            storage_gb: 2048,
            bandwidth_mbps: 800,
            recommended_instance: "AWS c5.4xlarge".to_string(),
            estimated_monthly_cost: 750.0,
        },
    };

    // Add to map
    svms.insert("solana".to_string(), solana_info);
    svms.insert("sonic".to_string(), sonic_info);
    svms.insert("eclipse".to_string(), eclipse_info);
    svms.insert("soon".to_string(), soon_info);
    svms.insert("opensvm".to_string(), opensvm_info);

    Ok(svms)
}

/// Get information about a specific SVM
///
/// # Arguments
/// * `client` - RPC client
/// * `name` - SVM name
/// * `commitment_config` - Commitment config
///
/// # Returns
/// * `Result<SvmInfo, Box<dyn Error>>` - SVM information
pub fn get_svm_info(
    client: &RpcClient,
    name: &str,
    commitment_config: CommitmentConfig,
) -> Result<SvmInfo, Box<dyn Error>> {
    // Get all SVMs and find the requested one
    let svms = list_all_svms(client, commitment_config)?;

    svms.get(name).cloned().ok_or_else(|| {
        Box::new(SvmError::NotFound(format!("SVM '{}' not found", name))) as Box<dyn Error>
    })
}

/// Display a list of all SVMs
///
/// # Arguments
/// * `svms` - Map of SVM names to info
pub fn display_svm_list(svms: &HashMap<String, SvmInfo>) {
    println!(
        "\n{}",
        "OSVM - Solana Virtual Machine Management".cyan().bold()
    );
    println!("{}", "Available SVMs in the chain:".green().bold());
    println!("{}", "==============".green());

    if svms.is_empty() {
        println!("{}", "No SVMs are available.".yellow());
        return;
    }

    println!(
        "{:<10} {:<20} {:<10} {:<15} {:<15}",
        "NAME".blue().bold(),
        "DISPLAY NAME".blue().bold(),
        "TOKEN".blue().bold(),
        "INSTALLABLE".blue().bold(),
        "PRICE (USD)".blue().bold()
    );

    println!(
        "{:<10} {:<20} {:<10} {:<15} {:<15}",
        "----", "------------", "-----", "----------", "----------"
    );

    println!(
        "{}",
        "------------------------------------------------------------------------".bright_black()
    );

    for info in svms.values() {
        let install_status = match (info.can_install_validator, info.can_install_rpc) {
            (true, true) => "Validator, RPC".green(),
            (true, false) => "Validator".green(),
            (false, true) => "RPC".green(),
            (false, false) => "No".red(),
        };

        println!(
            "{:<10} {:<20} {:<10} {:<15} ${:<14.2}",
            info.name, info.display_name, info.token_symbol, install_status, info.token_price_usd
        );
    }

    println!(
        "\n{} Use '{}' to get detailed information about a specific SVM",
        "TIP:".yellow().bold(),
        "osvm svm get <name>".cyan()
    );
}

/// Display detailed information about a specific SVM
///
/// # Arguments
/// * `info` - SVM information
pub fn display_svm_info(info: &SvmInfo) {
    println!(
        "\n{}",
        "OSVM - Solana Virtual Machine Management".cyan().bold()
    );
    println!(
        "{} {}",
        "SVM Information:".green().bold(),
        info.display_name.cyan().bold()
    );
    println!("{}", "=====================".green());

    println!("\n{}", "General Information".blue().bold());
    println!("{}", "-------------------".blue());
    println!("  Name: {}", info.name.yellow());
    println!("  Description: {}", info.description.yellow());
    println!(
        "  Token: {} (${:.2})",
        info.token_symbol.yellow(),
        info.token_price_usd
    );
    println!(
        "  Installation: {}{}",
        if info.can_install_validator || info.can_install_rpc {
            "Available as ".green()
        } else {
            "Not available".red()
        },
        if info.can_install_validator && info.can_install_rpc {
            "Validator, RPC".green()
        } else if info.can_install_validator {
            "Validator".green()
        } else if info.can_install_rpc {
            "RPC".green()
        } else {
            "".red()
        }
    );

    println!("\n{}", "Links".blue().bold());
    println!("{}", "-----".blue());
    println!("  Website: {}", info.website.cyan().underline());
    println!(
        "  Documentation: {}",
        info.documentation_url.cyan().underline()
    );
    println!("  Explorer: {}", info.explorer_url.cyan().underline());
    println!("  GitHub: {}", info.github_url.cyan().underline());

    println!("\n{}", "Network Information".blue().bold());
    println!("{}", "------------------".blue());

    println!("\n{}", "Networks".blue().bold());
    println!("{}", "--------".blue());

    for (network_name, network) in &info.networks {
        println!(
            "  {} {}:",
            network_name.to_uppercase().yellow().bold(),
            "Network".yellow().bold()
        );
        println!("    Version: {}", network.version);
        println!("    Validators: {}", network.validator_count);
        println!("    Current TPS: {:.1} tx/sec", network.current_tps);
        println!("    Block Height: {} blocks", network.block_height);
        println!("    Total Supply: {} tokens", network.total_supply);
        println!(
            "    Staked Tokens: {} tokens ({:.1}%)",
            network.staked_tokens,
            (network.staked_tokens as f64 / network.total_supply as f64) * 100.0
        );

        println!("    {} RPC Nodes:", network.rpc_nodes.len());
        for node in &network.rpc_nodes {
            println!(
                "      - {} ({}, {}) [TPS: {:.1}, Response: {:.1} ms]",
                node.url.cyan(),
                if node.is_official {
                    "Official".green()
                } else {
                    "Community".bright_black()
                },
                node.location.bright_white(),
                node.tps,
                node.response_time_ms
            );
        }
        println!();
    }

    println!("{}", "System Requirements".blue().bold());
    println!("{}", "------------------".blue());

    println!("  {}:", "Validator Requirements".yellow().bold());
    println!("    CPU: {} cores", info.validator_requirements.cpu_cores);
    println!("    RAM: {} GB", info.validator_requirements.ram_gb);
    println!("    Storage: {} GB", info.validator_requirements.storage_gb);
    println!(
        "    Bandwidth: {} Mbps",
        info.validator_requirements.bandwidth_mbps
    );
    println!(
        "    Recommended Instance: {}",
        info.validator_requirements.recommended_instance
    );
    println!(
        "    Est. Monthly Cost: ${:.2}",
        info.validator_requirements.estimated_monthly_cost
    );

    println!("\n  {}:", "RPC Node Requirements".yellow().bold());
    println!("    CPU: {} cores", info.rpc_requirements.cpu_cores);
    println!("    RAM: {} GB", info.rpc_requirements.ram_gb);
    println!("    Storage: {} GB", info.rpc_requirements.storage_gb);
    println!(
        "    Bandwidth: {} Mbps",
        info.rpc_requirements.bandwidth_mbps
    );
    println!(
        "    Recommended Instance: {}",
        info.rpc_requirements.recommended_instance
    );
    println!(
        "    Est. Monthly Cost: ${:.2}",
        info.rpc_requirements.estimated_monthly_cost
    );

    if info.can_install_validator || info.can_install_rpc {
        println!("\n{}", "Installation".blue().bold());
        println!("{}", "------------".blue());
        println!(
            "  {}: Install this SVM on a remote host with:",
            "TIP".yellow().bold()
        );

        println!(
            "    {}",
            format!("osvm svm install {} --host user@your-server-ip", info.name).cyan()
        );
        println!(
            "    {}",
            format!(
                "osvm user@your-server-ip --svm {} --node-type validator --network mainnet",
                info.name
            )
            .cyan()
        );
    }
}
