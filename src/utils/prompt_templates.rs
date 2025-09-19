//! Configurable AI Prompt Templates System
//!
//! This module provides a flexible template system for AI prompts that can be
//! loaded from external files and customized for different analysis scenarios.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Template categories for different types of AI analysis
#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum TemplateCategory {
    #[serde(rename = "deeplogic")]
    DeepLogic,
    #[serde(rename = "vulnerability_analysis")]
    VulnerabilityAnalysis,
    #[serde(rename = "code_review")]
    CodeReview,
    #[serde(rename = "security_audit")]
    SecurityAudit,
    #[serde(rename = "fix_suggestion")]
    FixSuggestion,
}

/// Analysis vector types for specialized prompts
#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum AnalysisVector {
    #[serde(rename = "state_transition")]
    StateTransition,
    #[serde(rename = "economic_exploit")]
    EconomicExploit,
    #[serde(rename = "access_control")]
    AccessControl,
    #[serde(rename = "mathematical_integrity")]
    MathematicalIntegrity,
    #[serde(rename = "general")]
    General,
}

/// AI prompt template structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PromptTemplate {
    /// Template identifier
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Template description
    pub description: String,
    /// Template category
    pub category: TemplateCategory,
    /// Analysis vector (if applicable)
    pub analysis_vector: Option<AnalysisVector>,
    /// The actual prompt template with placeholders
    pub template: String,
    /// Variables used in the template
    pub variables: Vec<TemplateVariable>,
    /// Template version for compatibility
    pub version: String,
    /// Whether this template is enabled
    pub enabled: bool,
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

/// Template variable definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateVariable {
    /// Variable name (without braces)
    pub name: String,
    /// Variable description
    pub description: String,
    /// Whether this variable is required
    pub required: bool,
    /// Default value if not provided
    pub default_value: Option<String>,
    /// Variable type hint
    pub variable_type: VariableType,
}

/// Types of template variables
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VariableType {
    #[serde(rename = "string")]
    String,
    #[serde(rename = "code")]
    Code,
    #[serde(rename = "filename")]
    Filename,
    #[serde(rename = "number")]
    Number,
    #[serde(rename = "boolean")]
    Boolean,
    #[serde(rename = "json")]
    Json,
}

/// Template collection for organizing related templates
#[derive(Debug, Serialize, Deserialize)]
pub struct TemplateCollection {
    /// Collection name
    pub name: String,
    /// Collection description
    pub description: String,
    /// Templates in this collection
    pub templates: Vec<PromptTemplate>,
    /// Collection version
    pub version: String,
    /// Collection author/maintainer
    pub author: Option<String>,
}

/// Main template manager
pub struct PromptTemplateManager {
    /// Loaded templates by ID
    templates: HashMap<String, PromptTemplate>,
    /// Templates organized by category
    category_index: HashMap<TemplateCategory, Vec<String>>,
    /// Templates organized by analysis vector
    vector_index: HashMap<AnalysisVector, Vec<String>>,
    /// Template directories to watch for changes
    template_dirs: Vec<String>,
}

impl PromptTemplateManager {
    pub fn new() -> Self {
        Self {
            templates: HashMap::new(),
            category_index: HashMap::new(),
            vector_index: HashMap::new(),
            template_dirs: Vec::new(),
        }
    }

    /// Load templates from a directory
    pub fn load_from_directory(&mut self, dir_path: &str) -> Result<usize> {
        self.load_from_directory_with_debug(dir_path, true)
    }

    pub fn load_from_directory_with_debug(&mut self, dir_path: &str, debug_mode: bool) -> Result<usize> {
        self.template_dirs.push(dir_path.to_string());

        let dir = Path::new(dir_path);
        if !dir.exists() {
            if debug_mode {
                println!("📁 Creating template directory: {}", dir_path);
            }
            fs::create_dir_all(dir)?;
            self.create_default_templates(dir)?;
        }

        let mut loaded_count = 0;

        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path
                .extension()
                .map_or(false, |ext| ext == "yaml" || ext == "yml")
            {
                match self.load_template_file(&path) {
                    Ok(count) => {
                        loaded_count += count;
                        if debug_mode {
                            println!("✅ Loaded {} templates from {}", count, path.display());
                        }
                    }
                    Err(e) => {
                        if debug_mode {
                            println!("⚠️  Failed to load template file {}: {}", path.display(), e);
                        }
                    }
                }
            }
        }

        self.rebuild_indices();
        Ok(loaded_count)
    }

    /// Load a single template file
    fn load_template_file(&mut self, file_path: &Path) -> Result<usize> {
        let content = fs::read_to_string(file_path)
            .with_context(|| format!("Failed to read template file: {}", file_path.display()))?;

        // Try to load as a collection first
        if let Ok(collection) = serde_yaml::from_str::<TemplateCollection>(&content) {
            let count = collection.templates.len();
            for template in collection.templates {
                self.templates.insert(template.id.clone(), template);
            }
            return Ok(count);
        }

        // Try to load as a single template
        if let Ok(template) = serde_yaml::from_str::<PromptTemplate>(&content) {
            self.templates.insert(template.id.clone(), template);
            return Ok(1);
        }

        anyhow::bail!("File is neither a valid template nor template collection");
    }

    /// Rebuild category and vector indices
    fn rebuild_indices(&mut self) {
        self.category_index.clear();
        self.vector_index.clear();

        for (id, template) in &self.templates {
            // Index by category
            self.category_index
                .entry(template.category.clone())
                .or_insert_with(Vec::new)
                .push(id.clone());

            // Index by analysis vector
            if let Some(vector) = &template.analysis_vector {
                self.vector_index
                    .entry(vector.clone())
                    .or_insert_with(Vec::new)
                    .push(id.clone());
            }
        }
    }

    /// Get template by ID
    pub fn get_template(&self, id: &str) -> Option<&PromptTemplate> {
        self.templates.get(id)
    }

    /// Get templates by category
    pub fn get_templates_by_category(&self, category: &TemplateCategory) -> Vec<&PromptTemplate> {
        if let Some(template_ids) = self.category_index.get(category) {
            template_ids
                .iter()
                .filter_map(|id| self.templates.get(id))
                .filter(|t| t.enabled)
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Get templates by analysis vector
    pub fn get_templates_by_vector(&self, vector: &AnalysisVector) -> Vec<&PromptTemplate> {
        if let Some(template_ids) = self.vector_index.get(vector) {
            template_ids
                .iter()
                .filter_map(|id| self.templates.get(id))
                .filter(|t| t.enabled)
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Render a template with provided variables
    pub fn render_template(
        &self,
        template_id: &str,
        variables: &HashMap<String, String>,
    ) -> Result<String> {
        let template = self
            .get_template(template_id)
            .ok_or_else(|| anyhow::anyhow!("Template not found: {}", template_id))?;

        if !template.enabled {
            anyhow::bail!("Template is disabled: {}", template_id);
        }

        let mut rendered = template.template.clone();

        // Check required variables
        for var in &template.variables {
            if var.required && !variables.contains_key(&var.name) && var.default_value.is_none() {
                anyhow::bail!(
                    "Required variable '{}' not provided for template '{}'",
                    var.name,
                    template_id
                );
            }
        }

        // Replace variables
        for var in &template.variables {
            let value = if let Some(provided_value) = variables.get(&var.name) {
                provided_value.clone()
            } else if let Some(default_value) = &var.default_value {
                default_value.clone()
            } else {
                continue; // Skip optional variables without defaults
            };

            let placeholder = format!("{{{}}}", var.name);
            rendered = rendered.replace(&placeholder, &value);
        }

        Ok(rendered)
    }

    /// Get best template for a specific use case
    pub fn get_best_template(
        &self,
        category: &TemplateCategory,
        vector: Option<&AnalysisVector>,
    ) -> Option<&PromptTemplate> {
        // First try to find a template that matches both category and vector
        if let Some(vector) = vector {
            for template in self.get_templates_by_category(category) {
                if template.analysis_vector.as_ref() == Some(vector) {
                    return Some(template);
                }
            }
        }

        // Fallback to any template in the category
        self.get_templates_by_category(category).first().copied()
    }

    /// List all available templates
    pub fn list_templates(&self) -> Vec<&PromptTemplate> {
        self.templates.values().filter(|t| t.enabled).collect()
    }

    /// Save template to file
    pub fn save_template(&self, template: &PromptTemplate, file_path: &Path) -> Result<()> {
        let yaml = serde_yaml::to_string(template)
            .with_context(|| "Failed to serialize template to YAML")?;

        fs::write(file_path, yaml)
            .with_context(|| format!("Failed to write template to {}", file_path.display()))?;

        Ok(())
    }

    /// Create default templates if none exist
    fn create_default_templates(&self, dir: &Path) -> Result<()> {
        let default_templates = self.get_default_templates();

        for template in &default_templates {
            let filename = format!("{}.yaml", template.id);
            let file_path = dir.join(filename);
            self.save_template(template, &file_path)?;
        }

        println!("📝 Created {} default templates", default_templates.len());
        Ok(())
    }

    /// Get default template definitions
    fn get_default_templates(&self) -> Vec<PromptTemplate> {
        vec![
            PromptTemplate {
                id: "deeplogic_economic_exploit".to_string(),
                name: "DeepLogic Economic Exploit Analysis".to_string(),
                description: "Analyzes code for potential economic exploitation vulnerabilities".to_string(),
                category: TemplateCategory::DeepLogic,
                analysis_vector: Some(AnalysisVector::EconomicExploit),
                template: r#"You are an expert Solana security auditor specializing in economic vulnerability analysis.

Analyze the following code for potential economic exploitation vulnerabilities:

FILE: {filename}
CODE:
```rust
{code}
```

VULNERABILITY CONTEXT:
{vulnerability_description}

Please provide a comprehensive analysis including:

1. **Economic Risk Assessment**: How could this code be exploited for financial gain?
2. **Attack Scenarios**: Describe specific step-by-step attack vectors
3. **Impact Analysis**: Quantify potential financial losses or gains for attackers
4. **Fix Recommendations**: Provide specific code changes to prevent exploitation

Focus on:
- Flash loan attacks
- Arbitrage vulnerabilities  
- Price manipulation
- Liquidity extraction
- MEV (Maximal Extractable Value) opportunities
- Economic state inconsistencies

Provide your analysis in JSON format:
{{
  "risk_level": "Critical|High|Medium|Low",
  "confidence_score": 0.0-1.0,
  "attack_scenarios": ["scenario1", "scenario2"],
  "economic_impact": "description",
  "recommended_fixes": ["fix1", "fix2"]
}}"#.to_string(),
                variables: vec![
                    TemplateVariable {
                        name: "filename".to_string(),
                        description: "Path to the source file being analyzed".to_string(),
                        required: true,
                        default_value: None,
                        variable_type: VariableType::Filename,
                    },
                    TemplateVariable {
                        name: "code".to_string(),
                        description: "The source code to analyze".to_string(),
                        required: true,
                        default_value: None,
                        variable_type: VariableType::Code,
                    },
                    TemplateVariable {
                        name: "vulnerability_description".to_string(),
                        description: "Description of the identified vulnerability".to_string(),
                        required: true,
                        default_value: None,
                        variable_type: VariableType::String,
                    },
                ],
                version: "1.0.0".to_string(),
                enabled: true,
                metadata: HashMap::new(),
            },
            PromptTemplate {
                id: "fix_suggestion_general".to_string(),
                name: "General Fix Suggestion Generator".to_string(),
                description: "Generates specific code fixes for identified vulnerabilities".to_string(),
                category: TemplateCategory::FixSuggestion,
                analysis_vector: Some(AnalysisVector::General),
                template: r#"You are an expert Rust and Solana developer tasked with fixing security vulnerabilities.

PROBLEMATIC CODE:
```rust
{problematic_code}
```

ISSUE DESCRIPTION:
{issue_description}

VULNERABILITY TYPE: {vulnerability_type}

Please provide:

1. **Root Cause Analysis**: Why is this code vulnerable?
2. **Specific Fix**: Provide the exact corrected code
3. **Explanation**: Explain why your fix resolves the issue
4. **Additional Considerations**: Any other security improvements

Requirements:
- Maintain existing functionality
- Follow Solana/Anchor best practices
- Include proper error handling
- Add necessary imports if required

Provide your response in this format:

## Root Cause
[explanation]

## Fixed Code
```rust
[corrected code]
```

## Explanation
[why this fix works]

## Additional Considerations
[other improvements or warnings]"#.to_string(),
                variables: vec![
                    TemplateVariable {
                        name: "problematic_code".to_string(),
                        description: "The code that contains the vulnerability".to_string(),
                        required: true,
                        default_value: None,
                        variable_type: VariableType::Code,
                    },
                    TemplateVariable {
                        name: "issue_description".to_string(),
                        description: "Description of the security issue".to_string(),
                        required: true,
                        default_value: None,
                        variable_type: VariableType::String,
                    },
                    TemplateVariable {
                        name: "vulnerability_type".to_string(),
                        description: "Type/category of the vulnerability".to_string(),
                        required: false,
                        default_value: Some("General Security Issue".to_string()),
                        variable_type: VariableType::String,
                    },
                ],
                version: "1.0.0".to_string(),
                enabled: true,
                metadata: HashMap::new(),
            },
        ]
    }

    /// Reload templates from all configured directories
    pub fn reload_templates(&mut self) -> Result<usize> {
        let dirs = self.template_dirs.clone();
        self.templates.clear();
        self.category_index.clear();
        self.vector_index.clear();

        let mut total_loaded = 0;
        for dir in &dirs {
            total_loaded += self.load_from_directory(dir)?;
        }

        Ok(total_loaded)
    }

    /// Validate template syntax
    pub fn validate_template(&self, template: &PromptTemplate) -> Result<Vec<String>> {
        let mut issues = Vec::new();

        // Check for variable placeholders that don't have corresponding variable definitions
        for var in &template.variables {
            let placeholder = format!("{{{}}}", var.name);
            if !template.template.contains(&placeholder) {
                issues.push(format!(
                    "Variable '{}' is defined but not used in template",
                    var.name
                ));
            }
        }

        // Check for placeholders in template that don't have variable definitions
        let mut in_placeholder = false;
        let mut current_placeholder = String::new();

        for char in template.template.chars() {
            match char {
                '{' => {
                    in_placeholder = true;
                    current_placeholder.clear();
                }
                '}' => {
                    if in_placeholder {
                        if !template
                            .variables
                            .iter()
                            .any(|v| v.name == current_placeholder)
                        {
                            issues.push(format!(
                                "Placeholder '{{{}}}' used but not defined",
                                current_placeholder
                            ));
                        }
                        in_placeholder = false;
                    }
                }
                _ => {
                    if in_placeholder {
                        current_placeholder.push(char);
                    }
                }
            }
        }

        Ok(issues)
    }
}

impl Default for PromptTemplateManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_template_loading() {
        let temp_dir = TempDir::new().unwrap();
        let mut manager = PromptTemplateManager::new();

        let loaded = manager
            .load_from_directory(temp_dir.path().to_str().unwrap())
            .unwrap();
        assert!(loaded > 0);

        let templates = manager.list_templates();
        assert!(!templates.is_empty());
    }

    #[test]
    fn test_template_rendering() {
        let mut manager = PromptTemplateManager::new();
        let temp_dir = TempDir::new().unwrap();

        manager
            .load_from_directory(temp_dir.path().to_str().unwrap())
            .unwrap();

        let mut variables = HashMap::new();
        variables.insert("filename".to_string(), "test.rs".to_string());
        variables.insert("code".to_string(), "fn test() {}".to_string());
        variables.insert(
            "vulnerability_description".to_string(),
            "Test vuln".to_string(),
        );

        let rendered = manager.render_template("deeplogic_economic_exploit", &variables);
        assert!(rendered.is_ok());

        let content = rendered.unwrap();
        assert!(content.contains("test.rs"));
        assert!(content.contains("fn test() {}"));
    }

    #[test]
    fn test_template_validation() {
        let manager = PromptTemplateManager::new();

        let template = PromptTemplate {
            id: "test".to_string(),
            name: "Test".to_string(),
            description: "Test template".to_string(),
            category: TemplateCategory::DeepLogic,
            analysis_vector: None,
            template: "Hello {name}, welcome to {place}!".to_string(),
            variables: vec![
                TemplateVariable {
                    name: "name".to_string(),
                    description: "User name".to_string(),
                    required: true,
                    default_value: None,
                    variable_type: VariableType::String,
                },
                // Missing 'place' variable definition
            ],
            version: "1.0".to_string(),
            enabled: true,
            metadata: HashMap::new(),
        };

        let issues = manager.validate_template(&template).unwrap();
        assert!(!issues.is_empty());
        assert!(issues.iter().any(|issue| issue.contains("place")));
    }
}
