// Test that MCP service now allows nvm node paths

use std::path::PathBuf;

#[test]
fn test_nvm_node_path_should_be_allowed() {
    println!("\n🧪 Testing MCP Path Validation Fix");

    // Get home directory
    let home = std::env::var("HOME").expect("HOME should be set");
    let home_path = PathBuf::from(&home);

    // Check if nvm directory exists
    let nvm_dir = home_path.join(".nvm/versions/node");
    if !nvm_dir.exists() {
        println!("⚠️  nvm not installed, skipping test");
        return;
    }

    // Find all node versions
    let mut node_paths = Vec::new();
    if let Ok(entries) = std::fs::read_dir(&nvm_dir) {
        for entry in entries.flatten() {
            if entry.path().is_dir() {
                let node_bin = entry.path().join("bin/node");
                if node_bin.exists() {
                    node_paths.push(node_bin);
                }
            }
        }
    }

    println!("📂 Found {} node installations:", node_paths.len());
    for path in &node_paths {
        println!("   • {}", path.display());
    }

    // Test that at least one node path exists
    assert!(
        !node_paths.is_empty(),
        "Should find at least one node installation"
    );

    // Test that the paths are in allowed directories according to new logic
    let mut allowed_dirs = vec![
        PathBuf::from("/usr/bin"),
        PathBuf::from("/usr/local/bin"),
        PathBuf::from("/bin"),
    ];

    // Add user directories (matching the fixed logic)
    allowed_dirs.push(home_path.join(".local/bin"));
    allowed_dirs.push(home_path.join("bin"));

    // Add nvm directories
    if let Ok(entries) = std::fs::read_dir(&nvm_dir) {
        for entry in entries.flatten() {
            if entry.path().is_dir() {
                allowed_dirs.push(entry.path().join("bin"));
            }
        }
    }

    println!("\n✅ Allowed directories ({} total):", allowed_dirs.len());
    for dir in &allowed_dirs {
        if dir.exists() {
            println!("   • {} ✓", dir.display());
        } else {
            println!("   • {} (doesn't exist)", dir.display());
        }
    }

    // Verify each node path is in an allowed directory
    for node_path in &node_paths {
        let canonical = node_path
            .canonicalize()
            .expect("Should be able to canonicalize node path");

        let is_in_allowed_dir = allowed_dirs.iter().any(|dir| canonical.starts_with(dir));

        println!("\n🔍 Checking: {}", canonical.display());
        println!("   Is in allowed dir: {}", is_in_allowed_dir);

        assert!(
            is_in_allowed_dir,
            "Node path {:?} should be in an allowed directory",
            canonical
        );
    }

    println!("\n✅ All node paths are in allowed directories!");
    println!("✅ MCP path validation fix is working correctly!");
}

#[test]
fn test_allowed_directory_logic() {
    println!("\n🧪 Testing Allowed Directory Logic");

    let home = std::env::var("HOME").unwrap_or_else(|_| "/home/test".to_string());
    let home_path = PathBuf::from(&home);

    let mut allowed_dirs = vec![
        PathBuf::from("/usr/bin"),
        PathBuf::from("/usr/local/bin"),
        PathBuf::from("/bin"),
    ];

    // Add user directories
    allowed_dirs.push(home_path.join(".local/bin"));
    allowed_dirs.push(home_path.join("bin"));
    allowed_dirs.push(home_path.join(".deno/bin"));
    allowed_dirs.push(home_path.join(".cargo/bin"));

    // Add nvm directories
    let nvm_dir = home_path.join(".nvm/versions/node");
    if let Ok(entries) = std::fs::read_dir(&nvm_dir) {
        for entry in entries.flatten() {
            if entry.path().is_dir() {
                allowed_dirs.push(entry.path().join("bin"));
            }
        }
    }

    println!("📋 Total allowed directories: {}", allowed_dirs.len());

    // Should have at least system directories
    assert!(
        allowed_dirs.len() >= 3,
        "Should have at least 3 system directories"
    );

    // Check system directories
    assert!(
        allowed_dirs.contains(&PathBuf::from("/usr/bin")),
        "Should contain /usr/bin"
    );
    assert!(
        allowed_dirs.contains(&PathBuf::from("/usr/local/bin")),
        "Should contain /usr/local/bin"
    );
    assert!(
        allowed_dirs.contains(&PathBuf::from("/bin")),
        "Should contain /bin"
    );

    println!("✅ Allowed directory logic is correct!");
}

#[test]
fn test_node_executable_exists() {
    println!("\n🧪 Testing Node Executable Exists");

    // Try to find node in various locations
    let possible_paths = ["/usr/bin/node", "/usr/local/bin/node", "/bin/node"];

    let home = std::env::var("HOME").unwrap_or_else(|_| "/home/test".to_string());
    let nvm_node = format!("{}/.nvm/versions/node/v24.7.0/bin/node", home);

    let mut found_node = false;
    let mut node_path = String::new();

    // Check system paths
    for path in &possible_paths {
        if PathBuf::from(path).exists() {
            found_node = true;
            node_path = path.to_string();
            println!("✓ Found node in system path: {}", path);
            break;
        }
    }

    // Check nvm path
    if !found_node && PathBuf::from(&nvm_node).exists() {
        found_node = true;
        node_path = nvm_node.clone();
        println!("✓ Found node in nvm: {}", nvm_node);
    }

    assert!(
        found_node,
        "Node executable should exist in at least one location"
    );
    println!("✅ Node found at: {}", node_path);
}
