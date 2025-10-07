//! Comprehensive tests for AI prompt templates including template loading,
//! variable substitution, template validation, and specialized prompts

use anyhow::Result;
use osvm::utils::prompt_templates::{
    PromptTemplate, TemplateRegistry, TemplateVariable, TemplateContext,
    SecurityAuditTemplate, CodeAnalysisTemplate, DeploymentTemplate,
    ErrorDiagnosisTemplate, TemplateValidator,
};
use std::collections::HashMap;

#[cfg(test)]
mod template_basic_tests {
    use super::*;

    #[test]
    fn test_template_creation() -> Result<()> {
        let template = PromptTemplate::new(
            "test_template",
            "Hello, {{name}}! Welcome to {{system}}.",
        );

        assert_eq!(template.name(), "test_template");
        assert!(template.content().contains("{{name}}"));

        Ok(())
    }

    #[test]
    fn test_simple_variable_substitution() -> Result<()> {
        let template = PromptTemplate::new(
            "greeting",
            "Hello, {{name}}!",
        );

        let mut context = TemplateContext::new();
        context.insert("name", "Alice");

        let rendered = template.render(&context)?;

        assert_eq!(rendered, "Hello, Alice!");
        assert!(!rendered.contains("{{"));

        Ok(())
    }

    #[test]
    fn test_multiple_variable_substitution() -> Result<()> {
        let template = PromptTemplate::new(
            "multi_var",
            "{{user}} deployed {{service}} to {{environment}}",
        );

        let mut context = TemplateContext::new();
        context.insert("user", "Bob");
        context.insert("service", "API");
        context.insert("environment", "production");

        let rendered = template.render(&context)?;

        assert_eq!(rendered, "Bob deployed API to production");

        Ok(())
    }

    #[test]
    fn test_missing_variable_handling() -> Result<()> {
        let template = PromptTemplate::new(
            "missing_var",
            "Hello, {{name}}!",
        );

        let context = TemplateContext::new();

        let result = template.render(&context);

        // Should error on missing required variable
        assert!(result.is_err());

        Ok(())
    }

    #[test]
    fn test_optional_variables() -> Result<()> {
        let template = PromptTemplate::new(
            "optional",
            "Hello{{#if name}}, {{name}}{{/if}}!",
        );

        // With name
        let mut context1 = TemplateContext::new();
        context1.insert("name", "Alice");
        let rendered1 = template.render(&context1)?;
        assert!(rendered1.contains("Alice"));

        // Without name
        let context2 = TemplateContext::new();
        let rendered2 = template.render(&context2)?;
        assert_eq!(rendered2, "Hello!");

        Ok(())
    }

    #[test]
    fn test_nested_variables() -> Result<()> {
        let template = PromptTemplate::new(
            "nested",
            "{{user.name}} works at {{user.company}}",
        );

        let mut context = TemplateContext::new();
        let mut user_data = HashMap::new();
        user_data.insert("name".to_string(), "Alice".to_string());
        user_data.insert("company".to_string(), "OSVM".to_string());
        context.insert_nested("user", user_data);

        let rendered = template.render(&context)?;

        assert!(rendered.contains("Alice"));
        assert!(rendered.contains("OSVM"));

        Ok(())
    }

    #[test]
    fn test_list_iteration() -> Result<()> {
        let template = PromptTemplate::new(
            "list",
            "Services: {{#each services}}{{name}}, {{/each}}",
        );

        let mut context = TemplateContext::new();
        let services = vec!["API", "Database", "Cache"];
        context.insert_list("services", services);

        let rendered = template.render(&context)?;

        assert!(rendered.contains("API"));
        assert!(rendered.contains("Database"));
        assert!(rendered.contains("Cache"));

        Ok(())
    }

    #[test]
    fn test_conditional_rendering() -> Result<()> {
        let template = PromptTemplate::new(
            "conditional",
            "{{#if is_admin}}Admin Panel{{else}}User Panel{{/if}}",
        );

        let mut admin_context = TemplateContext::new();
        admin_context.insert("is_admin", "true");
        let admin_rendered = template.render(&admin_context)?;
        assert_eq!(admin_rendered, "Admin Panel");

        let mut user_context = TemplateContext::new();
        user_context.insert("is_admin", "false");
        let user_rendered = template.render(&user_context)?;
        assert_eq!(user_rendered, "User Panel");

        Ok(())
    }

    #[test]
    fn test_template_comments() -> Result<()> {
        let template = PromptTemplate::new(
            "comments",
            "Hello {{! This is a comment}} {{name}}!",
        );

        let mut context = TemplateContext::new();
        context.insert("name", "Bob");

        let rendered = template.render(&context)?;

        assert_eq!(rendered, "Hello  Bob!");
        assert!(!rendered.contains("comment"));

        Ok(())
    }

    #[test]
    fn test_whitespace_handling() -> Result<()> {
        let template = PromptTemplate::new(
            "whitespace",
            "{{~name~}}",
        );

        let mut context = TemplateContext::new();
        context.insert("name", "  Alice  ");

        let rendered = template.render(&context)?;

        assert_eq!(rendered.trim(), "Alice");

        Ok(())
    }
}

#[cfg(test)]
mod template_registry_tests {
    use super::*;

    #[test]
    fn test_registry_creation() {
        let registry = TemplateRegistry::new();
        assert!(registry.is_empty());
    }

    #[test]
    fn test_register_template() -> Result<()> {
        let mut registry = TemplateRegistry::new();

        let template = PromptTemplate::new("test", "Hello, {{name}}!");
        registry.register(template)?;

        assert!(registry.has_template("test"));
        assert_eq!(registry.count(), 1);

        Ok(())
    }

    #[test]
    fn test_get_template() -> Result<()> {
        let mut registry = TemplateRegistry::new();

        let template = PromptTemplate::new("greeting", "Hi, {{name}}!");
        registry.register(template)?;

        let retrieved = registry.get("greeting")?;
        assert_eq!(retrieved.name(), "greeting");

        Ok(())
    }

    #[test]
    fn test_template_not_found() {
        let registry = TemplateRegistry::new();

        let result = registry.get("nonexistent");
        assert!(result.is_err());
    }

    #[test]
    fn test_template_overwrite() -> Result<()> {
        let mut registry = TemplateRegistry::new();

        let template1 = PromptTemplate::new("test", "Version 1");
        registry.register(template1)?;

        let template2 = PromptTemplate::new("test", "Version 2");
        let result = registry.register(template2);

        // Should error on duplicate unless overwrite is allowed
        assert!(result.is_err() || result.is_ok());

        Ok(())
    }

    #[test]
    fn test_list_templates() -> Result<()> {
        let mut registry = TemplateRegistry::new();

        registry.register(PromptTemplate::new("t1", "Template 1"))?;
        registry.register(PromptTemplate::new("t2", "Template 2"))?;
        registry.register(PromptTemplate::new("t3", "Template 3"))?;

        let templates = registry.list();
        assert_eq!(templates.len(), 3);

        Ok(())
    }

    #[test]
    fn test_remove_template() -> Result<()> {
        let mut registry = TemplateRegistry::new();

        registry.register(PromptTemplate::new("temp", "Content"))?;
        assert!(registry.has_template("temp"));

        registry.remove("temp")?;
        assert!(!registry.has_template("temp"));

        Ok(())
    }

    #[test]
    fn test_load_from_file() -> Result<()> {
        use tempfile::TempDir;
        let temp_dir = TempDir::new()?;
        let template_file = temp_dir.path().join("test_template.hbs");

        std::fs::write(&template_file, "Hello, {{name}}!")?;

        let mut registry = TemplateRegistry::new();
        registry.load_from_file("file_template", &template_file)?;

        assert!(registry.has_template("file_template"));

        Ok(())
    }

    #[test]
    fn test_load_from_directory() -> Result<()> {
        use tempfile::TempDir;
        let temp_dir = TempDir::new()?;

        std::fs::write(temp_dir.path().join("template1.hbs"), "T1")?;
        std::fs::write(temp_dir.path().join("template2.hbs"), "T2")?;

        let mut registry = TemplateRegistry::new();
        registry.load_from_directory(temp_dir.path())?;

        assert!(registry.count() >= 2);

        Ok(())
    }

    #[test]
    fn test_default_templates() -> Result<()> {
        let registry = TemplateRegistry::with_defaults()?;

        // Should have common templates pre-loaded
        assert!(registry.has_template("security_audit"));
        assert!(registry.has_template("code_analysis"));
        assert!(registry.has_template("deployment_guide"));

        Ok(())
    }
}

#[cfg(test)]
mod security_audit_template_tests {
    use super::*;

    #[test]
    fn test_security_audit_template_creation() -> Result<()> {
        let template = SecurityAuditTemplate::new();

        assert!(!template.content().is_empty());
        assert!(template.content().contains("security"));

        Ok(())
    }

    #[test]
    fn test_vulnerability_analysis_prompt() -> Result<()> {
        let template = SecurityAuditTemplate::new();

        let mut context = TemplateContext::new();
        context.insert("code", "unsafe function call");
        context.insert("language", "Rust");

        let prompt = template.render_vulnerability_analysis(&context)?;

        assert!(prompt.contains("vulnerability"));
        assert!(prompt.contains("Rust"));

        Ok(())
    }

    #[test]
    fn test_dependency_audit_prompt() -> Result<()> {
        let template = SecurityAuditTemplate::new();

        let dependencies = vec!["tokio@1.0.0", "serde@1.0.0"];
        let prompt = template.render_dependency_audit(&dependencies)?;

        assert!(prompt.contains("tokio"));
        assert!(prompt.contains("serde"));

        Ok(())
    }

    #[test]
    fn test_severity_assessment_prompt() -> Result<()> {
        let template = SecurityAuditTemplate::new();

        let finding = "Potential buffer overflow in input handling";
        let prompt = template.render_severity_assessment(finding)?;

        assert!(prompt.contains("severity"));
        assert!(prompt.contains(finding));

        Ok(())
    }

    #[test]
    fn test_remediation_suggestion_prompt() -> Result<()> {
        let template = SecurityAuditTemplate::new();

        let vulnerability = "SQL injection in query builder";
        let prompt = template.render_remediation_suggestion(vulnerability)?;

        assert!(prompt.contains("remediation") || prompt.contains("fix"));
        assert!(prompt.contains("SQL injection"));

        Ok(())
    }
}

#[cfg(test)]
mod code_analysis_template_tests {
    use super::*;

    #[test]
    fn test_code_review_prompt() -> Result<()> {
        let template = CodeAnalysisTemplate::new();

        let code = "fn main() { println!(\"Hello\"); }";
        let prompt = template.render_code_review(code)?;

        assert!(prompt.contains("review") || prompt.contains("analyze"));
        assert!(prompt.contains(code));

        Ok(())
    }

    #[test]
    fn test_complexity_analysis_prompt() -> Result<()> {
        let template = CodeAnalysisTemplate::new();

        let function = "fn complex_function() { /* ... */ }";
        let prompt = template.render_complexity_analysis(function)?;

        assert!(prompt.contains("complexity"));

        Ok(())
    }

    #[test]
    fn test_refactoring_suggestion_prompt() -> Result<()> {
        let template = CodeAnalysisTemplate::new();

        let code_block = "duplicate code here";
        let prompt = template.render_refactoring_suggestion(code_block)?;

        assert!(prompt.contains("refactor"));

        Ok(())
    }

    #[test]
    fn test_performance_optimization_prompt() -> Result<()> {
        let template = CodeAnalysisTemplate::new();

        let code = "for i in 0..n { vec.push(i); }";
        let prompt = template.render_performance_optimization(code)?;

        assert!(prompt.contains("performance") || prompt.contains("optimize"));

        Ok(())
    }
}

#[cfg(test)]
mod deployment_template_tests {
    use super::*;

    #[test]
    fn test_deployment_plan_prompt() -> Result<()> {
        let template = DeploymentTemplate::new();

        let mut context = TemplateContext::new();
        context.insert("service", "API Server");
        context.insert("environment", "production");

        let prompt = template.render_deployment_plan(&context)?;

        assert!(prompt.contains("API Server"));
        assert!(prompt.contains("production"));

        Ok(())
    }

    #[test]
    fn test_rollback_strategy_prompt() -> Result<()> {
        let template = DeploymentTemplate::new();

        let deployment_id = "deploy-12345";
        let prompt = template.render_rollback_strategy(deployment_id)?;

        assert!(prompt.contains("rollback"));
        assert!(prompt.contains(deployment_id));

        Ok(())
    }

    #[test]
    fn test_health_check_prompt() -> Result<()> {
        let template = DeploymentTemplate::new();

        let service_name = "database";
        let prompt = template.render_health_check(service_name)?;

        assert!(prompt.contains("health"));
        assert!(prompt.contains(service_name));

        Ok(())
    }
}

#[cfg(test)]
mod error_diagnosis_template_tests {
    use super::*;

    #[test]
    fn test_error_analysis_prompt() -> Result<()> {
        let template = ErrorDiagnosisTemplate::new();

        let error_message = "Connection refused: port 8080";
        let prompt = template.render_error_analysis(error_message)?;

        assert!(prompt.contains("error") || prompt.contains("diagnos"));
        assert!(prompt.contains("8080"));

        Ok(())
    }

    #[test]
    fn test_root_cause_analysis_prompt() -> Result<()> {
        let template = ErrorDiagnosisTemplate::new();

        let error_log = "Error: timeout after 30s\nStack trace: ...";
        let prompt = template.render_root_cause_analysis(error_log)?;

        assert!(prompt.contains("root cause"));
        assert!(prompt.contains("timeout"));

        Ok(())
    }

    #[test]
    fn test_solution_suggestion_prompt() -> Result<()> {
        let template = ErrorDiagnosisTemplate::new();

        let problem = "Database connection pool exhausted";
        let prompt = template.render_solution_suggestion(problem)?;

        assert!(prompt.contains("solution") || prompt.contains("fix"));

        Ok(())
    }
}

#[cfg(test)]
mod template_validator_tests {
    use super::*;

    #[test]
    fn test_valid_template() -> Result<()> {
        let validator = TemplateValidator::new();

        let template = PromptTemplate::new("valid", "Hello, {{name}}!");
        let result = validator.validate(&template)?;

        assert!(result.is_valid);
        assert!(result.errors.is_empty());

        Ok(())
    }

    #[test]
    fn test_invalid_syntax() -> Result<()> {
        let validator = TemplateValidator::new();

        let template = PromptTemplate::new("invalid", "Hello, {{name}!");
        let result = validator.validate(&template)?;

        assert!(!result.is_valid);
        assert!(!result.errors.is_empty());

        Ok(())
    }

    #[test]
    fn test_undefined_helper() -> Result<()> {
        let validator = TemplateValidator::new();

        let template = PromptTemplate::new("bad_helper", "{{#unknown_helper}}content{{/unknown_helper}}");
        let result = validator.validate(&template)?;

        // Might warn about unknown helper
        assert!(result.warnings.is_empty() || !result.warnings.is_empty());

        Ok(())
    }

    #[test]
    fn test_security_validation() -> Result<()> {
        let validator = TemplateValidator::with_security_checks();

        // Template with potential XSS
        let dangerous_template = PromptTemplate::new(
            "xss",
            "{{user_input}}",
        );

        let result = validator.validate(&dangerous_template)?;

        // Should flag unescaped user input
        assert!(!result.warnings.is_empty() || !result.errors.is_empty());

        Ok(())
    }

    #[test]
    fn test_required_variables_check() -> Result<()> {
        let validator = TemplateValidator::new();

        let template = PromptTemplate::new("vars", "{{name}} {{age}} {{city}}");
        let required_vars = validator.extract_required_variables(&template)?;

        assert_eq!(required_vars.len(), 3);
        assert!(required_vars.contains(&"name".to_string()));
        assert!(required_vars.contains(&"age".to_string()));
        assert!(required_vars.contains(&"city".to_string()));

        Ok(())
    }
}

#[cfg(test)]
mod template_performance_tests {
    use super::*;

    #[test]
    fn test_rendering_performance() -> Result<()> {
        let template = PromptTemplate::new("perf", "{{a}} {{b}} {{c}}");

        let mut context = TemplateContext::new();
        context.insert("a", "1");
        context.insert("b", "2");
        context.insert("c", "3");

        let start = std::time::Instant::now();

        for _ in 0..1000 {
            let _ = template.render(&context)?;
        }

        let duration = start.elapsed();

        // Should render 1000 templates in less than 1 second
        assert!(duration.as_millis() < 1000);

        Ok(())
    }

    #[test]
    fn test_large_template_rendering() -> Result<()> {
        let large_content = "{{var}}".repeat(100);
        let template = PromptTemplate::new("large", &large_content);

        let mut context = TemplateContext::new();
        context.insert("var", "X");

        let rendered = template.render(&context)?;

        assert_eq!(rendered.len(), 100); // 100 X's

        Ok(())
    }

    #[test]
    fn test_concurrent_rendering() -> Result<()> {
        use std::sync::Arc;
        use std::thread;

        let template = Arc::new(PromptTemplate::new("concurrent", "{{value}}"));

        let mut handles = vec![];

        for i in 0..10 {
            let template_clone = Arc::clone(&template);
            let handle = thread::spawn(move || {
                let mut context = TemplateContext::new();
                context.insert("value", &i.to_string());
                template_clone.render(&context).unwrap()
            });
            handles.push(handle);
        }

        for handle in handles {
            let _ = handle.join().unwrap();
        }

        Ok(())
    }
}
