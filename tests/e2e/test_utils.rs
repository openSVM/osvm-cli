use std::{env, fs, process::Command};
use std::io::Write;

/// Checks if output contains a specific string
#[allow(dead_code)]
pub fn output_contains(output: &std::process::Output, expected: &str) -> bool {
    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout.contains(expected)
}

/// Creates a temporary Solana keypair file for testing
pub fn setup_test_keypair() -> String {
    // Create directory structure if it doesn't exist
    let home_dir = env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    let config_dir = format!("{}/.config/solana", home_dir);
    fs::create_dir_all(&config_dir).expect("Failed to create config directory");

    // Path to keypair file
    let keypair_path = format!("{}/id.json", config_dir);
    
    // Write a dummy keypair JSON
    let dummy_keypair = r#"[174,47,154,16,202,193,206,113,199,190,53,133,169,175,1,123,148,253,93,36,105,110,69,142,57,150,60,60,71,233,252,234,39,55,43,69,168,7,27,30,185,29,146,89,143,92,53,22,202,13,60,220,186,133,101,206,145,192,155,169,181,117,188,143]"#;

    let mut file = fs::File::create(&keypair_path).expect("Failed to create keypair file");
    file.write_all(dummy_keypair.as_bytes()).expect("Failed to write to keypair file");
    
    keypair_path
}

/// Sets up the test environment
pub fn setup_test_environment() {
    setup_test_keypair();
}

/// Execute command with environment setup
#[allow(dead_code)]
pub fn execute_command_with_setup(cmd: &mut Command) -> std::process::Output {
    setup_test_environment();
    cmd.output().expect("Failed to execute command")
}
