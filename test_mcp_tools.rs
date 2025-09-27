use osvm::services::mcp_service::McpService;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("Testing MCP tools loading...");
    
    let mut mcp_service = McpService::new();
    
    // Load config
    println!("Loading MCP config...");
    mcp_service.load_config()?;
    
    // List servers
    let servers = mcp_service.list_servers(None, None)?;
    println!("Found {} MCP servers:", servers.len());
    for (id, config) in &servers {
        println!("  - {} (enabled: {})", id, config.enabled);
    }
    
    // Try to load tools from each enabled server
    for (id, config) in servers {
        if config.enabled {
            println!("\nLoading tools from {}...", id);
            match mcp_service.list_tools(&id).await {
                Ok(tools) => {
                    println!("  Found {} tools:", tools.len());
                    for (i, tool) in tools.iter().enumerate() {
                        if i < 5 {
                            println!("    - {}: {}", tool.name, 
                                tool.description.as_ref().unwrap_or(&"No description".to_string()));
                        }
                    }
                    if tools.len() > 5 {
                        println!("    ... and {} more", tools.len() - 5);
                    }
                }
                Err(e) => {
                    println!("  Error: {}", e);
                }
            }
        }
    }
    
    Ok(())
}
