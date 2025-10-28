//! Tests for OVSM plan generation validation
//! 
//! This test suite verifies that the AI service properly validates
//! OVSM plans and ensures they are syntactically correct before execution.

use osvm::services::ai_service::AiService;
use osvm::services::mcp_service::McpTool;
use std::collections::HashMap;

#[cfg(test)]
mod tests {
    use super::*;

    /// Test that valid OVSM code passes validation
    #[test]
    fn test_valid_ovsm_syntax() {
        let ai_service = AiService::new_with_debug(false);
        
        // Test simple valid LISP expressions
        let valid_code = r#"
            (define x 10)
            (define y 20)
            (+ x y)
        "#;
        
        let result = ai_service.validate_ovsm_syntax(valid_code);
        assert!(result.is_ok(), "Valid OVSM code should pass validation");
    }
    
    /// Test that invalid OVSM code fails validation  
    #[test]
    fn test_invalid_ovsm_syntax() {
        let ai_service = AiService::new_with_debug(false);
        
        // Test code with syntax errors
        let invalid_cases = vec![
            // Missing closing parenthesis
            "(define x 10",
            // Extra closing parenthesis
            "(define x 10))",
            // Invalid infix notation (common AI mistake) - this is actually valid syntax
            // "(define result (x + y))",  // This parses as (x + y) which is valid
            // Invalid token
            "(define x @invalid)",
            // Unclosed string
            "(define msg \"hello",
        ];
        
        for invalid_code in invalid_cases {
            let result = ai_service.validate_ovsm_syntax(invalid_code);
            assert!(result.is_err(), 
                "Invalid OVSM code '{}' should fail validation", invalid_code);
        }
    }
    
    /// Test extraction of LISP code from XML-wrapped plans
    #[test]
    fn test_extract_lisp_from_xml() {
        let ai_service = AiService::new_with_debug(false);
        
        let xml_plan = r#"
<ovsm_plan>
  <overview>Test plan</overview>
  <code>
    (define balance 100)
    (define fee 5)
    (- balance fee)
  </code>
</ovsm_plan>
        "#;
        
        let extracted = ai_service.extract_ovsm_lisp_code(xml_plan);
        assert!(extracted.contains("(define balance 100)"));
        assert!(extracted.contains("(- balance fee)"));
        assert!(!extracted.contains("<code>"), "XML tags should be removed");
    }
    
    /// Test extraction of LISP code from markdown code blocks
    #[test]
    fn test_extract_lisp_from_markdown() {
        let ai_service = AiService::new_with_debug(false);
        
        let markdown_plan = r#"
**Main Branch:**
```lisp
(define account "ABC123")
(GET_ACCOUNT_STATS account)
```
        "#;
        
        let extracted = ai_service.extract_ovsm_lisp_code(markdown_plan);
        assert!(extracted.contains("(define account"));
        assert!(extracted.contains("(GET_ACCOUNT_STATS account)"));
        assert!(!extracted.contains("```"), "Markdown markers should be removed");
    }
    
    /// Test extraction handles raw LISP code correctly
    #[test]
    fn test_extract_raw_lisp() {
        let ai_service = AiService::new_with_debug(false);
        
        let raw_lisp = "(define x 42)";
        let extracted = ai_service.extract_ovsm_lisp_code(raw_lisp);
        assert_eq!(extracted, raw_lisp, "Raw LISP should be returned as-is");
    }
    
    /// Test that error refinement prompt is generated correctly
    #[test]
    fn test_error_refinement_prompt() {
        let ai_service = AiService::new_with_debug(false);
        
        let original_query = "Get account balance";
        let broken_code = "(define balance (GET_BALANCE account))";
        let error_message = "Undefined variable: account";
        
        let prompt = ai_service.create_error_refinement_prompt(
            original_query,
            broken_code,
            error_message,
            1
        );
        
        assert!(prompt.contains(original_query));
        assert!(prompt.contains(broken_code));
        assert!(prompt.contains(error_message));
        assert!(prompt.contains("Attempt #1"));
        assert!(prompt.contains("CORRECTED OVSM plan"));
    }
    
    /// Test that retryable errors are identified correctly
    #[test]
    fn test_is_retryable_error() {
        let retryable_errors = vec![
            "Parse error: unexpected token",
            "Tokenization error at line 5",
            "Expected identifier, got number",
            "Expected RightParen at position 10",
            "Undefined variable: x",
            "Undefined tool: INVALID_TOOL",
            "Type error: expected string, got number",
        ];
        
        for error in retryable_errors {
            assert!(
                AiService::is_retryable_ovsm_error(error),
                "Error '{}' should be retryable", error
            );
        }
        
        let non_retryable_errors = vec![
            "Network timeout",
            "Connection refused",
            "API rate limit exceeded",
            "Internal server error",
        ];
        
        for error in non_retryable_errors {
            assert!(
                !AiService::is_retryable_ovsm_error(error),
                "Error '{}' should NOT be retryable", error
            );
        }
    }
    
    /// Test semantic validation of results
    #[test]
    fn test_semantic_result_validation() {
        // Test empty result detection
        let (valid, msg) = AiService::validate_ovsm_result(
            "null",
            "Get account balance",
            "What is my balance?"
        );
        assert!(!valid, "Null result should be invalid");
        assert!(msg.contains("empty"));
        
        // Test count/total validation
        let (valid, msg) = AiService::validate_ovsm_result(
            "not_a_number",
            "Count transactions",
            "How many transactions?"
        );
        assert!(!valid, "Non-numeric result for count should be invalid");
        assert!(msg.contains("numeric"));
        
        // Test list/array validation
        let (valid, msg) = AiService::validate_ovsm_result(
            "single_value",
            "List all accounts",
            "Show all accounts"
        );
        assert!(!valid, "Single value for 'all' query should be invalid");
        assert!(msg.contains("single value"));
        
        // Test valid result
        let (valid, _) = AiService::validate_ovsm_result(
            "100.5",
            "Get balance",
            "What is the balance?"
        );
        assert!(valid, "Valid numeric result should pass");
    }
    
    /// Integration test: Generate and validate a plan
    #[tokio::test]
    async fn test_create_validated_plan() {
        let ai_service = AiService::new_with_debug(false);
        let _available_tools: HashMap<String, Vec<McpTool>> = HashMap::new(); // Empty tools for testing
        
        // Test the validation flow with a syntactically valid plan
        let valid_plan = r#"
<ovsm_plan>
  <overview>Get account information</overview>
  <code>
    (define address "TestWallet123")
    (define stats (GET_ACCOUNT_STATS address))
    stats
  </code>
</ovsm_plan>
        "#;
        
        // Validate the test plan - should succeed (syntax is valid even if function doesn't exist)
        let result = ai_service.validate_ovsm_syntax(valid_plan);
        assert!(result.is_ok(), "Syntactically valid plan should pass validation");
        
        // Test with actually invalid syntax
        let invalid_plan = r#"
<ovsm_plan>
  <overview>Invalid plan</overview>
  <code>
    (define x 10
    (+ x 5)
  </code>
</ovsm_plan>
        "#;
        
        let result = ai_service.validate_ovsm_syntax(invalid_plan);
        assert!(result.is_err(), "Plan with syntax error should fail validation");
    }
    
    /// Test plan validation with complex OVSM constructs
    #[test]
    fn test_validate_complex_ovsm() {
        let ai_service = AiService::new_with_debug(false);
        
        let complex_code = r#"
            (define results [])
            (for (i (range 1 10))
              (define squared (* i i))
              (set! results (concat results [squared])))
            results
        "#;
        
        let result = ai_service.validate_ovsm_syntax(complex_code);
        assert!(result.is_ok(), "Complex valid OVSM should pass validation");
    }
    
    /// Test validation catches common AI generation mistakes
    #[test]
    fn test_catch_common_ai_mistakes() {
        let ai_service = AiService::new_with_debug(false);
        
        // Common mistake 1: Numbers as function names (invalid)
        let number_as_function = "(define result (10 20 30))";
        assert!(
            ai_service.validate_ovsm_syntax(number_as_function).is_err(),
            "Number as function name should be caught"
        );
        
        // Common mistake 2: Missing define for variables
        let _undefined_var = r#"
            (set! x 10)
            (+ x 5)
        "#;
        // This might actually parse but fail at runtime
        // The parser doesn't do semantic analysis
        
        // Common mistake 3: Unbalanced parentheses
        let unbalanced = "(if (> x 10) (print \"yes\")";
        assert!(
            ai_service.validate_ovsm_syntax(unbalanced).is_err(),
            "Unbalanced parentheses should be caught"
        );
    }
}
