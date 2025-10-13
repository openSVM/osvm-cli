//! Examples command implementation
//!
//! This module handles the `examples` subcommand which displays usage examples.

use crate::utils::arg_helpers;
use crate::utils::cli_error::CliError;
use crate::utils::examples;
use clap::ArgMatches;

/// Execute the examples command
///
/// Displays CLI usage examples, either all examples or filtered by category.
///
/// # Arguments
///
/// * `matches` - The argument matches from clap for this subcommand
///
/// # Returns
///
/// Result indicating success or failure
///
/// # Example
///
/// ```ignore
/// let result = execute(matches).await?;
/// ```
pub async fn execute(matches: &ArgMatches) -> Result<(), CliError> {
    // Check if user wants to list categories
    if arg_helpers::get_flag(matches, "list_categories") {
        println!("Available example categories:");
        println!("  basic       - Basic Commands");
        println!("  svm         - SVM Management");
        println!("  node        - Node Deployment");
        println!("  monitoring  - Node Monitoring and Management");
        println!("  workflow    - Common Workflows");
        println!("\nUse 'osvm examples --category <name>' to show examples for a specific category.");
        return Ok(());
    }

    // Check if user specified a category
    if let Some(category) = arg_helpers::get_optional_str(matches, "category") {
        examples::display_category_by_name(category);
        return Ok(());
    }

    // Display all examples
    examples::display_all_examples();
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_exists() {
        // Basic test to ensure the module compiles
        assert!(true);
    }
}
