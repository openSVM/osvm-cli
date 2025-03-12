//! CLI examples module
//!
//! This module provides practical usage examples for OSVM CLI commands,
//! organized by category to help users learn common workflows and best practices.

use crate::utils::color;
use std::collections::HashMap;

/// Category of examples
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExampleCategory {
    /// Basic commands
    Basic,
    /// SVM management examples
    SvmManagement,
    /// Node deployment examples
    NodeDeployment,
    /// Node monitoring examples
    NodeMonitoring,
    /// Workflow examples (multiple commands)
    Workflow,
}

impl ExampleCategory {
    /// Get the display name for a category
    pub fn display_name(&self) -> &'static str {
        match self {
            ExampleCategory::Basic => "Basic Commands",
            ExampleCategory::SvmManagement => "SVM Management",
            ExampleCategory::NodeDeployment => "Node Deployment",
            ExampleCategory::NodeMonitoring => "Node Monitoring and Management",
            ExampleCategory::Workflow => "Common Workflows",
        }
    }

    /// Get a description for a category
    pub fn description(&self) -> &'static str {
        match self {
            ExampleCategory::Basic => "Fundamental commands to get started with OSVM CLI",
            ExampleCategory::SvmManagement => {
                "Commands for managing and inspecting SVMs (Solana Virtual Machines)"
            }
            ExampleCategory::NodeDeployment => {
                "Commands for deploying validator and RPC nodes to servers"
            }
            ExampleCategory::NodeMonitoring => {
                "Commands for monitoring and managing deployed nodes"
            }
            ExampleCategory::Workflow => "Multi-step command sequences for common operations",
        }
    }
}

/// Example command with explanation
#[derive(Debug, Clone)]
pub struct Example {
    /// Title of the example
    pub title: &'static str,
    /// Command to run
    pub command: &'static str,
    /// Explanation of what the command does
    pub explanation: &'static str,
    /// Category this example belongs to
    pub category: ExampleCategory,
}

/// Get all examples
pub fn get_all_examples() -> Vec<Example> {
    vec![
        // Basic Commands
        Example {
            title: "Run with increased verbosity",
            command: "osvm -v svm list",
            explanation: "Run a command with increased verbosity level. Use -vv or -vvv for even more details",
            category: ExampleCategory::Basic,
        },
        Example {
            title: "Specify a different RPC URL",
            command: "osvm --url https://api.mainnet-beta.solana.com svm list",
            explanation: "Run a command using a specific Solana RPC endpoint",
            category: ExampleCategory::Basic,
        },
        Example {
            title: "Disable colored output",
            command: "osvm --no-color svm list",
            explanation: "Run a command with colored output disabled",
            category: ExampleCategory::Basic,
        },
        // SVM Management
        Example {
            title: "List all available SVMs",
            command: "osvm svm list",
            explanation: "Shows all SVMs available in the network with their basic details",
            category: ExampleCategory::SvmManagement,
        },
        Example {
            title: "Get detailed information about an SVM",
            command: "osvm svm get solana",
            explanation: "Displays comprehensive information about the specified SVM including networks, requirements, and installation options",
            category: ExampleCategory::SvmManagement,
        },
        Example {
            title: "Launch interactive SVM dashboard",
            command: "osvm svm dashboard",
            explanation: "Starts an interactive dashboard showing real-time information about all SVMs",
            category: ExampleCategory::SvmManagement,
        },
        Example {
            title: "Install an SVM on a remote host",
            command: "osvm svm install solana --host user@123.45.67.89",
            explanation: "Installs the specified SVM on the remote host as a validator node on mainnet (default settings)",
            category: ExampleCategory::SvmManagement,
        },
        // Node Deployment
        Example {
            title: "Deploy a validator node",
            command: "osvm user@host.example.com --svm solana --node-type validator --network mainnet",
            explanation: "Deploys a Solana validator node on the mainnet to the specified remote host",
            category: ExampleCategory::NodeDeployment,
        },
        Example {
            title: "Deploy an RPC node on testnet",
            command: "osvm user@host.example.com --svm solana --node-type rpc --network testnet",
            explanation: "Deploys a Solana RPC node on the testnet to the specified remote host",
            category: ExampleCategory::NodeDeployment,
        },
        Example {
            title: "Deploy a Sonic RPC node",
            command: "osvm rpc sonic root@host.example.com --network mainnet",
            explanation: "Deploys a Sonic RPC node on the mainnet to the specified remote host using the sonicfromnewyoke/solana-rpc repository",
            category: ExampleCategory::NodeDeployment,
        },
        Example {
            title: "Deploy multiple SVMs to a single host",
            command: "osvm user@host.example.com --svm solana,sonic,eclipse --node-type validator --network mainnet",
            explanation: "Deploys validator nodes for multiple SVMs to a single host, all running on mainnet",
            category: ExampleCategory::NodeDeployment,
        },
        // Enhanced Solana Validator Deployment Examples
        Example {
            title: "Deploy an enhanced Solana validator node",
            command: "osvm solana validator user@host.example.com --network mainnet",
            explanation: "Deploys a Solana validator node with enhanced features on the mainnet",
            category: ExampleCategory::NodeDeployment,
        },
        Example {
            title: "Deploy Solana validator with disk configuration",
            command: "osvm solana validator user@host.example.com --network mainnet --ledger-disk /dev/nvme0n1 --accounts-disk /dev/nvme1n1",
            explanation: "Deploys a Solana validator with optimized 3-disk configuration (OS, Ledger, Accounts/Snapshots)",
            category: ExampleCategory::NodeDeployment,
        },
        Example {
            title: "Deploy Solana validator with Jito client",
            command: "osvm solana validator user@host.example.com --network mainnet --client-type jito --version v1.18.23-jito",
            explanation: "Deploys a Solana validator using the Jito client with a specific version",
            category: ExampleCategory::NodeDeployment,
        },
        Example {
            title: "Deploy Solana validator with hot-swap capability",
            command: "osvm solana validator user@host.example.com --network mainnet --hot-swap",
            explanation: "Deploys a Solana validator with hot-swap capability for high availability",
            category: ExampleCategory::NodeDeployment,
        },
        Example {
            title: "Deploy fully-optimized Solana validator",
            command: "osvm solana validator user@host.example.com --network mainnet --ledger-disk /dev/nvme0n1 --accounts-disk /dev/nvme1n1 --client-type jito --hot-swap --metrics-config \"host=https://metrics.solana.com:8086,db=mainnet-beta\"",
            explanation: "Deploys a Solana validator with all optimizations: disk configuration, Jito client, hot-swap capability, and metrics",
            category: ExampleCategory::NodeDeployment,
        },
        Example {
            title: "Deploy enhanced Solana RPC node",
            command: "osvm solana rpc user@host.example.com --network mainnet --ledger-disk /dev/nvme0n1 --accounts-disk /dev/nvme1n1",
            explanation: "Deploys a Solana RPC node with optimized disk configuration",
            category: ExampleCategory::NodeDeployment,
        },
        Example {
            title: "Deploy Solana RPC node with transaction history",
            command: "osvm solana rpc user@host.example.com --network mainnet --enable-history",
            explanation: "Deploys a Solana RPC node with transaction history enabled (increases storage requirements)",
            category: ExampleCategory::NodeDeployment,
        },
        // Node Monitoring
        Example {
            title: "List all deployed nodes",
            command: "osvm nodes list",
            explanation: "Shows all nodes currently managed by OSVM CLI",
            category: ExampleCategory::NodeMonitoring,
        },
        Example {
            title: "Filter nodes by SVM type",
            command: "osvm nodes list --svm solana",
            explanation: "Lists only nodes running the specified SVM",
            category: ExampleCategory::NodeMonitoring,
        },
        Example {
            title: "Filter nodes by network and status",
            command: "osvm nodes list --network mainnet --status running",
            explanation: "Lists only nodes on mainnet that are currently running",
            category: ExampleCategory::NodeMonitoring,
        },
        Example {
            title: "Get node status",
            command: "osvm nodes status solana-validator-mainnet-192.168.1.1",
            explanation: "Checks and displays the current status of the specified node",
            category: ExampleCategory::NodeMonitoring,
        },
        Example {
            title: "Get detailed node information",
            command: "osvm nodes get solana-validator-mainnet-192.168.1.1",
            explanation: "Shows comprehensive information about the specified node",
            category: ExampleCategory::NodeMonitoring,
        },
        Example {
            title: "View node logs",
            command: "osvm nodes logs solana-validator-mainnet-192.168.1.1 --lines 200",
            explanation: "Shows the last 200 lines of logs from the specified node",
            category: ExampleCategory::NodeMonitoring,
        },
        Example {
            title: "Follow node logs in real-time",
            command: "osvm nodes logs solana-validator-mainnet-192.168.1.1 --follow",
            explanation: "Streams logs from the node in real-time (ctrl+c to exit)",
            category: ExampleCategory::NodeMonitoring,
        },
        Example {
            title: "Launch interactive node dashboard",
            command: "osvm nodes dashboard",
            explanation: "Starts an interactive dashboard showing real-time information about all nodes",
            category: ExampleCategory::NodeMonitoring,
        },
        Example {
            title: "Restart a node",
            command: "osvm nodes restart solana-validator-mainnet-192.168.1.1",
            explanation: "Safely restarts the specified node",
            category: ExampleCategory::NodeMonitoring,
        },
        Example {
            title: "Stop a node",
            command: "osvm nodes stop solana-validator-mainnet-192.168.1.1",
            explanation: "Safely stops the specified node",
            category: ExampleCategory::NodeMonitoring,
        },
        // Workflow Examples
        Example {
            title: "Set up a new validator node (full workflow)",
            command: "osvm svm get solana\nosvm user@host.example.com --svm solana --node-type validator --network mainnet\nosvm nodes get solana-validator-mainnet-host.example.com\nosvm nodes logs solana-validator-mainnet-host.example.com --follow",
            explanation: "Complete workflow: First check SVM info, then deploy a validator node, verify the deployment status, and monitor the logs as it starts up",
            category: ExampleCategory::Workflow,
        },
        Example {
            title: "Migrate a validator to a new host",
            command: "osvm nodes stop solana-validator-mainnet-oldhost.example.com\nosvm user@newhost.example.com --svm solana --node-type validator --network mainnet\nosvm nodes get solana-validator-mainnet-newhost.example.com",
            explanation: "Workflow for migrating a validator: Stop the existing node, deploy a new one on the new host, and verify it's running correctly",
            category: ExampleCategory::Workflow,
        },
    ]
}

/// Get examples by category
pub fn get_examples_by_category() -> HashMap<ExampleCategory, Vec<Example>> {
    let mut categories = HashMap::new();

    for example in get_all_examples() {
        categories
            .entry(example.category)
            .or_insert_with(Vec::new)
            .push(example);
    }

    categories
}

/// Display examples for all categories
pub fn display_all_examples() {
    println!("\n{}", color::heading("OSVM CLI Examples"));
    println!("{}", color::subheading("================="));

    println!("\n{}", color::important("This command provides examples of common OSVM CLI usage patterns organized by category."));
    println!(
        "Use the {} flag to see examples from a specific category.",
        color::command("--category")
    );

    let categories = get_examples_by_category();

    // Determine the correct order to display categories in
    let category_order = [
        ExampleCategory::Basic,
        ExampleCategory::SvmManagement,
        ExampleCategory::NodeDeployment,
        ExampleCategory::NodeMonitoring,
        ExampleCategory::Workflow,
    ];

    for category in category_order.iter() {
        if let Some(examples) = categories.get(category) {
            display_category(*category, examples);
        }
    }

    println!(
        "\n{}",
        color::important("TIP: Add the -v flag to any command to see more detailed output.")
    );
}

/// Display examples for a specific category
pub fn display_category(category: ExampleCategory, examples: &[Example]) {
    println!("\n{}", color::heading(category.display_name()));
    println!(
        "{}",
        color::secondary(&"-".repeat(category.display_name().len()))
    );
    println!("{}", color::important(category.description()));
    println!();

    for (i, example) in examples.iter().enumerate() {
        println!(
            "{} {}",
            color::key(&format!("{}.", i + 1)),
            color::bold(example.title)
        );

        // For workflow examples, split multi-line commands
        if category == ExampleCategory::Workflow {
            for (i, cmd) in example.command.split('\n').enumerate() {
                println!(
                    "   {}: {}",
                    color::secondary(&format!("Step {}", i + 1)),
                    color::command(cmd)
                );
            }
        } else {
            println!("   {}", color::command(example.command));
        }

        println!("   {}", color::secondary(example.explanation));
        println!();
    }
}

/// Display examples for a specific category by name
pub fn display_category_by_name(category_name: &str) {
    let category = match category_name.to_lowercase().as_str() {
        "basic" => ExampleCategory::Basic,
        "svm" | "svmmanagement" => ExampleCategory::SvmManagement,
        "node" | "nodedeployment" | "deployment" => ExampleCategory::NodeDeployment,
        "monitoring" | "nodemonitoring" => ExampleCategory::NodeMonitoring,
        "workflow" | "workflows" => ExampleCategory::Workflow,
        _ => {
            println!(
                "\n{}",
                color::error(&format!("Unknown category: {}", category_name))
            );
            println!("Available categories: basic, svm, node, monitoring, workflow");
            return;
        }
    };

    let categories = get_examples_by_category();

    if let Some(examples) = categories.get(&category) {
        println!("\n{}", color::heading("OSVM CLI Examples"));
        println!("{}", color::subheading("================="));
        display_category(category, examples);
    } else {
        println!(
            "\n{}",
            color::error(&format!(
                "No examples found for category: {}",
                category_name
            ))
        );
    }
}
