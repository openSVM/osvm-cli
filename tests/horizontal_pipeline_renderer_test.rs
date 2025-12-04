//! Horizontal Pipeline Renderer Tests

use anyhow::Result;

#[cfg(test)]
mod horizontal_pipeline_tests {
    use super::*;
    use osvm::services::research_agent::{Transfer, TransferGraph};

    #[test]
    fn test_simple_pipeline_rendering() -> Result<()> {
        let mut graph = TransferGraph::new();
        graph.token_name = Some("TestToken".to_string());
        graph.origin = Some("wallet_a".to_string());

        // Add simple flow: A → B → C
        graph.add_transfer(Transfer {
            from: "wallet_a".to_string(),
            to: "wallet_b".to_string(),
            amount: 1000000.0,
            token_symbol: "TestToken".to_string(),
            timestamp: Some("2024-01-15T10:00:00Z".to_string()),
            note: None,
        });

        graph.add_transfer(Transfer {
            from: "wallet_b".to_string(),
            to: "wallet_c".to_string(),
            amount: 500000.0,
            token_symbol: "TestToken".to_string(),
            timestamp: Some("2024-01-15T11:00:00Z".to_string()),
            note: None,
        });

        graph.set_node_label("wallet_a", "Exchange".to_string());
        graph.set_node_label("wallet_b", "Mixer".to_string());

        let output = graph.render_horizontal_pipeline();

        // Verify basic rendering (just check it produces output without crashing)
        assert!(!output.is_empty(), "Should produce non-empty output");
        assert!(output.len() > 100, "Should produce substantial output");

        println!("\n=== HORIZONTAL PIPELINE OUTPUT ===\n{}", output);
        Ok(())
    }
}
