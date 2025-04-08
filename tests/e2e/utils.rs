use std::process::Output;
use std::str;

/// Check if command output contains the specified text
pub fn output_contains(output: &Output, text: &str) -> bool {
    let stdout = str::from_utf8(&output.stdout).unwrap_or("");
    let stderr = str::from_utf8(&output.stderr).unwrap_or("");
    
    stdout.contains(text) || stderr.contains(text)
}

/// Check if output contains any of the given keywords (case insensitive)
pub fn output_contains_any(output: &Output, keywords: &[&str]) -> bool {
    let stdout = str::from_utf8(&output.stdout).unwrap_or("").to_lowercase();
    let stderr = str::from_utf8(&output.stderr).unwrap_or("").to_lowercase();
    
    for keyword in keywords {
        if stdout.contains(&keyword.to_lowercase()) || stderr.contains(&keyword.to_lowercase()) {
            return true;
        }
    }
    false
}

/// Assert command succeeded and contains relevant output
pub fn assert_success_with_output(output: &Output, keywords: &[&str]) {
    assert!(output.status.success(), "Command failed with exit code: {:?}", output.status.code());
    assert!(output_contains_any(output, keywords), "Output doesn't contain any expected keywords");
}
