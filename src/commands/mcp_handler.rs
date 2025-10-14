use crate::services::mcp_service::{
    McpAuthConfig, McpServerConfig, McpService, McpTransportType,
};
use crate::utils::input_sanitization;

/// Handle MCP commands using the dedicated MCP service
pub async fn handle_mcp_command(
    app_matches: &clap::ArgMatches,
    matches: &clap::ArgMatches,
) -> Result<(), Box<dyn std::error::Error>> {
    let debug_mode = app_matches.get_flag("debug");
    let mut mcp_service = McpService::new_with_debug(debug_mode);

    // Load existing configurations
    if let Err(e) = mcp_service.load_config() {
        if debug_mode {
            eprintln!("⚠️  Warning: Failed to load MCP config: {}", e);
        }
    }

    let Some((mcp_sub_command, mcp_sub_matches)) = matches.subcommand() else {
        eprintln!("No MCP subcommand provided");
        std::process::exit(1);
    };

    match mcp_sub_command {
        "add" => {
            let server_id = mcp_sub_matches
                .get_one::<String>("server_id")
                .expect("server_id is required by clap");
            let url = mcp_sub_matches
                .get_one::<String>("server_url")
                .expect("server_url is required by clap");
            let name = mcp_sub_matches
                .get_one::<String>("name")
                .map(|s| s.to_string())
                .unwrap_or_else(|| server_id.clone());

            let transport_str = mcp_sub_matches
                .get_one::<String>("transport")
                .expect("transport has a default value");
            let transport_type = match transport_str.as_str() {
                "http" => McpTransportType::Http,
                "websocket" => McpTransportType::Websocket,
                "stdio" => McpTransportType::Stdio,
                _ => McpTransportType::Http,
            };

            let auth = if mcp_sub_matches
                .get_one::<String>("auth_type")
                .expect("auth_type has a default value")
                != "none"
            {
                let auth_type = mcp_sub_matches
                    .get_one::<String>("auth_type")
                    .expect("auth_type checked above")
                    .clone();
                Some(McpAuthConfig {
                    auth_type,
                    token: mcp_sub_matches.get_one::<String>("auth_token").cloned(),
                    username: mcp_sub_matches.get_one::<String>("username").cloned(),
                    password: mcp_sub_matches.get_one::<String>("password").cloned(),
                })
            } else {
                None
            };

            let enabled = mcp_sub_matches.get_flag("enabled");

            let config = McpServerConfig {
                name,
                url: url.clone(),
                transport_type,
                auth,
                enabled,
                extra_config: std::collections::HashMap::new(),
                github_url: None,
                local_path: None,
            };

            mcp_service.add_server(server_id.clone(), config);
            println!("✅ Added MCP server '{}' at {}", server_id, url);

            if enabled {
                println!("🔄 Testing server connectivity...");
                if let Err(e) = mcp_service.test_server(server_id).await {
                    eprintln!("⚠️  Warning: Server test failed: {}", e);
                    eprintln!("   The server was added but may not be accessible.");
                } else {
                    println!("✅ Server connectivity test passed!");
                }
            }
        }

        "add-github" => {
            let server_id = mcp_sub_matches
                .get_one::<String>("server_id")
                .expect("server_id is required by clap");
            let github_url = mcp_sub_matches
                .get_one::<String>("github_url")
                .expect("github_url is required by clap");
            let name = mcp_sub_matches.get_one::<String>("name").cloned();
            let enabled = mcp_sub_matches.get_flag("enabled");
            let skip_confirmation = mcp_sub_matches.get_flag("yes");

            println!("🔄 Cloning MCP server from GitHub: {}", github_url);

            match mcp_service
                .add_server_from_github(
                    server_id.clone(),
                    github_url.clone(),
                    name,
                    skip_confirmation,
                )
                .await
            {
                Ok(_) => {
                    println!(
                        "✅ Successfully cloned and configured MCP server '{}'",
                        server_id
                    );
                    println!("   Repository: {}", github_url);

                    if enabled {
                        println!("🔄 Testing server connectivity...");
                        if let Err(e) = mcp_service.test_server(server_id).await {
                            eprintln!("⚠️  Warning: Server test failed: {}", e);
                            eprintln!("   The server was configured but may not be accessible.");
                        } else {
                            println!("✅ Server connectivity test passed!");
                        }
                    } else {
                        println!("💡 Use 'osvm mcp enable {}' to activate it", server_id);
                    }
                }
                Err(e) => {
                    eprintln!("❌ Failed to clone and configure MCP server: {}", e);
                    std::process::exit(1);
                }
            }
        }

        "remove" => {
            let server_id = mcp_sub_matches.get_one::<String>("server_id").unwrap();
            if let Some(removed_config) = mcp_service.remove_server(server_id) {
                println!(
                    "✅ Removed MCP server '{}' ({})",
                    server_id, removed_config.name
                );
            } else {
                eprintln!("❌ Server '{}' not found", server_id);
                std::process::exit(1);
            }
        }

        "list" => {
            let servers = mcp_service.list_servers();
            let json_output = mcp_sub_matches.get_flag("json");
            let enabled_only = mcp_sub_matches.get_flag("enabled_only");

            if json_output {
                let filtered_servers: std::collections::HashMap<&String, &McpServerConfig> =
                    servers
                        .iter()
                        .filter(|(_, config)| !enabled_only || config.enabled)
                        .map(|(id, config)| (*id, *config))
                        .collect();
                println!("{}", serde_json::to_string_pretty(&filtered_servers)?);
            } else {
                if servers.is_empty() {
                    println!("No MCP servers configured.");
                    println!("💡 Use 'osvm mcp setup' for quick Solana MCP server setup");
                    println!("💡 Use 'osvm mcp add <server_id> --url <url>' to add custom servers");
                    return Ok(());
                }

                println!("📋 Configured MCP Servers:");
                println!("========================");

                for (server_id, config) in servers {
                    if enabled_only && !config.enabled {
                        continue;
                    }

                    let status_icon = if config.enabled { "🟢" } else { "🔴" };
                    let transport_icon = match config.transport_type {
                        McpTransportType::Http => "🌐",
                        McpTransportType::Websocket => "🔌",
                        McpTransportType::Stdio => "⚡",
                    };

                    println!();
                    println!(
                        "  {} {} {} {}",
                        status_icon, transport_icon, server_id, config.name
                    );
                    println!("     URL: {}", config.url);
                    println!("     Transport: {:?}", config.transport_type);
                    if config.auth.is_some() {
                        println!("     Auth: Configured");
                    }
                }
            }
        }

        "enable" => {
            let server_id = mcp_sub_matches.get_one::<String>("server_id").unwrap();
            mcp_service.toggle_server(server_id, true)?;
            println!("✅ Enabled MCP server '{}'", server_id);
        }

        "disable" => {
            let server_id = mcp_sub_matches.get_one::<String>("server_id").unwrap();
            mcp_service.toggle_server(server_id, false)?;
            println!("🔴 Disabled MCP server '{}'", server_id);
        }

        "test" => {
            let server_id = mcp_sub_matches.get_one::<String>("server_id").unwrap();

            println!("🔄 Testing MCP server '{}'...", server_id);

            match mcp_service.test_server(server_id).await {
                Ok(_) => {
                    println!("✅ MCP server '{}' connectivity test passed!", server_id);
                }
                Err(e) => {
                    eprintln!("❌ MCP server '{}' test failed: {}", server_id, e);
                    std::process::exit(1);
                }
            }
        }

        "init" => {
            let server_id = mcp_sub_matches.get_one::<String>("server_id").unwrap();

            println!("🔄 Initializing MCP server '{}'...", server_id);

            match mcp_service.initialize_server(server_id).await {
                Ok(_) => {
                    println!("✅ Successfully initialized MCP server '{}'", server_id);
                }
                Err(e) => {
                    eprintln!("❌ Failed to initialize MCP server '{}': {}", server_id, e);
                    std::process::exit(1);
                }
            }
        }

        "tools" => {
            let server_id = mcp_sub_matches.get_one::<String>("server_id").unwrap();
            let json_output = mcp_sub_matches.get_flag("json");

            println!("🔄 Fetching tools from MCP server '{}'...", server_id);

            match mcp_service.list_tools(server_id).await {
                Ok(tools) => {
                    if json_output {
                        println!("{}", serde_json::to_string_pretty(&tools)?);
                    } else {
                        if tools.is_empty() {
                            println!("No tools available from server '{}'", server_id);
                            return Ok(());
                        }

                        println!("🛠️  Available Tools from '{}':", server_id);
                        println!("=================================");

                        for tool in tools {
                            println!();
                            println!("  📦 {}", tool.name);
                            if let Some(description) = &tool.description {
                                println!("     Description: {}", description);
                            }

                            // Show input schema in a more readable format
                            if let Ok(schema_obj) = serde_json::from_value::<
                                serde_json::Map<String, serde_json::Value>,
                            >(
                                tool.input_schema.clone()
                            ) {
                                if let Some(properties) = schema_obj.get("properties") {
                                    if let Some(props_obj) = properties.as_object() {
                                        if !props_obj.is_empty() {
                                            println!("     Parameters:");
                                            for (param_name, _param_schema) in props_obj {
                                                println!("       - {}", param_name);
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        println!();
                        println!(
                            "💡 Use 'osvm mcp call {} <tool_name>' to execute a tool",
                            server_id
                        );
                    }
                }
                Err(e) => {
                    eprintln!(
                        "❌ Failed to fetch tools from MCP server '{}': {}",
                        server_id, e
                    );
                    std::process::exit(1);
                }
            }
        }

        "call" => {
            let server_id = mcp_sub_matches.get_one::<String>("server_id").unwrap();
            let tool_name = mcp_sub_matches.get_one::<String>("tool_name").unwrap();
            let json_output = mcp_sub_matches.get_flag("json");

            let arguments = if let Some(args_str) = mcp_sub_matches.get_one::<String>("arguments") {
                // Validate JSON with size limit (max 1MB for arguments)
                Some(
                    input_sanitization::validate_json(args_str, 1024 * 1024)
                        .map_err(|e| format!("Invalid JSON arguments: {}", e))?,
                )
            } else {
                None
            };

            if debug_mode {
                println!(
                    "🔄 Calling tool '{}' on MCP server '{}'...",
                    tool_name, server_id
                );
                if let Some(ref args) = arguments {
                    println!("   Arguments: {}", serde_json::to_string_pretty(args)?);
                }
            }

            match mcp_service.call_tool(server_id, tool_name, arguments).await {
                Ok(result) => {
                    if json_output {
                        println!("{}", serde_json::to_string_pretty(&result)?);
                    } else {
                        println!("✅ Tool '{}' executed successfully:", tool_name);
                        println!();

                        // Try to display the result in a user-friendly way
                        if let Some(content) = result.get("content") {
                            if let Some(content_array) = content.as_array() {
                                for item in content_array {
                                    if let Some(text_content) = item.get("text") {
                                        if let Some(text) = text_content.as_str() {
                                            println!("{}", text);
                                        }
                                    } else {
                                        println!("{}", serde_json::to_string_pretty(item)?);
                                    }
                                }
                            } else {
                                println!("{}", serde_json::to_string_pretty(&result)?);
                            }
                        } else {
                            println!("{}", serde_json::to_string_pretty(&result)?);
                        }
                    }
                }
                Err(e) => {
                    eprintln!(
                        "❌ Failed to call tool '{}' on MCP server '{}': {}",
                        tool_name, server_id, e
                    );
                    std::process::exit(1);
                }
            }
        }

        "setup" => {
            let url = mcp_sub_matches.get_one::<String>("mcp_url").unwrap();
            let auto_enable = mcp_sub_matches.get_flag("auto_enable");

            println!("🚀 Setting up Solana MCP Server integration...");
            println!("   URL: {}", url);

            let config = McpServerConfig {
                name: "Solana MCP Server".to_string(),
                url: url.clone(),
                transport_type: McpTransportType::Http,
                auth: None,
                enabled: auto_enable,
                extra_config: std::collections::HashMap::new(),
                github_url: None,
                local_path: None,
            };

            mcp_service.add_server("solana".to_string(), config);
            println!("✅ Added Solana MCP server configuration");

            if auto_enable {
                println!("🔄 Testing server connectivity...");
                match mcp_service.test_server("solana").await {
                    Ok(_) => {
                        println!("✅ Solana MCP server is accessible and ready!");
                        println!();
                        println!("🎉 Setup complete! You can now:");
                        println!("   • List available tools: osvm mcp tools solana");
                        println!(
                            "   • Call Solana RPC methods: osvm mcp call solana <method_name>"
                        );
                        println!("   • Query blockchain data through natural language in AI mode");
                    }
                    Err(e) => {
                        eprintln!("⚠️  Warning: Server test failed: {}", e);
                        eprintln!("   The server was configured but may not be accessible.");
                        eprintln!(
                            "   Please check that the Solana MCP server is running at: {}",
                            url
                        );
                        eprintln!();
                        eprintln!("🔧 To start the Solana MCP server:");
                        eprintln!("   git clone https://github.com/openSVM/solana-mcp-server");
                        eprintln!("   cd solana-mcp-server && cargo run web --port 3000");
                    }
                }
            } else {
                println!("✅ Solana MCP server configured (disabled)");
                println!("💡 Use 'osvm mcp enable solana' to activate it");
            }
        }

        "search" => {
            let query = mcp_sub_matches
                .get_one::<String>("query")
                .expect("query is required by clap");
            let transport = mcp_sub_matches.get_one::<String>("transport");
            let enabled_only = mcp_sub_matches.get_flag("enabled_only");
            let json_output = mcp_sub_matches.get_flag("json");

            let transport_filter =
                transport.and_then(|t| if t == "any" { None } else { Some(t.as_str()) });
            let results = mcp_service.search_servers(query, transport_filter, enabled_only);

            if results.is_empty() {
                println!("🔍 No MCP servers found matching query: '{}'", query);
                if enabled_only {
                    println!("💡 Try removing the --enabled-only filter to see all servers");
                }
                if transport_filter.is_some() {
                    println!("💡 Try changing the transport filter or use --transport=any");
                }
            } else if json_output {
                let json_results: Vec<serde_json::Value> = results
                    .iter()
                    .map(|(id, config)| {
                        serde_json::json!({
                            "id": id,
                            "name": config.name,
                            "url": config.url,
                            "transport": match config.transport_type {
                                McpTransportType::Http => "http",
                                McpTransportType::Websocket => "websocket",
                                McpTransportType::Stdio => "stdio"
                            },
                            "enabled": config.enabled,
                            "has_auth": config.auth.is_some(),
                            "github_url": config.github_url
                        })
                    })
                    .collect();
                println!("{}", serde_json::to_string_pretty(&json_results)?);
            } else {
                println!(
                    "🔍 Found {} MCP server(s) matching '{}:'",
                    results.len(),
                    query
                );
                println!();

                for (id, config) in results {
                    let status_icon = if config.enabled { "🟢" } else { "🔴" };
                    let transport_icon = match config.transport_type {
                        McpTransportType::Http => "🌐",
                        McpTransportType::Websocket => "🔗",
                        McpTransportType::Stdio => "⚡",
                    };
                    let auth_badge = if config.auth.is_some() { " 🔐" } else { "" };

                    println!(
                        "  {} {} {} {}{}",
                        status_icon, transport_icon, id, config.name, auth_badge
                    );
                    println!("     URL: {}", config.url);

                    if let Some(github_url) = &config.github_url {
                        println!("     GitHub: {}", github_url);
                    }

                    if let Some(local_path) = &config.local_path {
                        println!("     Local: {}", local_path);
                    }

                    println!("     Transport: {:?}", config.transport_type);
                    println!();
                }
            }
        }

        "mount" => {
            let tool_name = mcp_sub_matches
                .get_one::<String>("tool_name")
                .expect("tool_name is required by clap");
            let host_path = mcp_sub_matches
                .get_one::<String>("host_path")
                .expect("host_path is required by clap");
            let readonly = mcp_sub_matches.get_flag("readonly");

            match crate::commands::mount::handle_mcp_mount(tool_name, host_path, readonly) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("❌ Failed to mount folder to MCP tool: {}", e);
                    std::process::exit(1);
                }
            }
        }

        "unmount" => {
            let tool_name = mcp_sub_matches
                .get_one::<String>("tool_name")
                .expect("tool_name is required by clap");
            let host_path = mcp_sub_matches
                .get_one::<String>("host_path")
                .expect("host_path is required by clap");

            match crate::commands::mount::handle_mcp_unmount(tool_name, host_path) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("❌ Failed to unmount folder from MCP tool: {}", e);
                    std::process::exit(1);
                }
            }
        }

        "mounts" => {
            let tool_name = mcp_sub_matches.get_one::<String>("tool_name");

            match crate::commands::mount::handle_mcp_mounts(tool_name.map(|s| s.as_str())) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("❌ Failed to list MCP tool mounts: {}", e);
                    std::process::exit(1);
                }
            }
        }

        "microvm" => {
            // Handle MCP microVM subcommands
            use crate::commands::mcp_microvm;
            if let Err(e) = mcp_microvm::handle_mcp_microvm_command(mcp_sub_matches).await {
                eprintln!("❌ MCP microVM error: {}", e);
                std::process::exit(1);
            }
        }

        _ => {
            eprintln!("Unknown MCP subcommand: {}", mcp_sub_command);
            std::process::exit(1);
        }
    }

    Ok(())
}
