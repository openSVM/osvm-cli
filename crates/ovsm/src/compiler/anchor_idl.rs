//! Anchor IDL Generation from OVSM Source
//!
//! This module parses OVSM LISP programs and generates Anchor-compatible IDL files
//! that can be used by TypeScript/JavaScript clients.
//!
//! ## IDL Format (Legacy - pre Anchor 0.30)
//! ```json
//! {
//!   "version": "0.1.0",
//!   "name": "program_name",
//!   "instructions": [...],
//!   "accounts": [...],
//!   "types": [...],
//!   "errors": [...],
//!   "metadata": { "address": "..." }
//! }
//! ```
//!
//! ## Usage
//! ```ignore
//! use ovsm::compiler::anchor_idl::IdlGenerator;
//!
//! let source = std::fs::read_to_string("program.ovsm")?;
//! let generator = IdlGenerator::new(&source);
//! let idl_json = generator.generate()?;
//! std::fs::write("target/idl/program.json", idl_json)?;
//! ```

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Anchor IDL structure (Legacy format)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnchorIdl {
    /// IDL format version (e.g., "0.1.0")
    pub version: String,
    /// Program name
    pub name: String,
    /// List of program instructions/entry points
    pub instructions: Vec<IdlInstruction>,
    /// Account type definitions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub accounts: Vec<IdlAccountDef>,
    /// Custom type definitions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub types: Vec<IdlTypeDef>,
    /// Error code definitions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub errors: Vec<IdlError>,
    /// Program metadata (e.g., deployed address)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub metadata: Option<IdlMetadata>,
}

/// Instruction definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdlInstruction {
    /// Instruction name (camelCase)
    pub name: String,
    /// Required accounts for this instruction
    pub accounts: Vec<IdlAccountMeta>,
    /// Instruction arguments/parameters
    pub args: Vec<IdlArg>,
    /// Instruction discriminator bytes (first byte(s) of instruction data)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub discriminator: Option<Vec<u8>>,
}

/// Account metadata for instruction
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IdlAccountMeta {
    /// Account name
    pub name: String,
    /// Whether the account is mutable (writable)
    pub is_mut: bool,
    /// Whether the account must sign the transaction
    pub is_signer: bool,
    /// Optional documentation for this account
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub docs: Option<Vec<String>>,
}

/// Argument definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdlArg {
    /// Argument name
    pub name: String,
    /// Argument type
    #[serde(rename = "type")]
    pub ty: IdlType,
}

/// Account definition (data layout)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdlAccountDef {
    /// Account type name
    pub name: String,
    /// Account type structure (struct or enum)
    #[serde(rename = "type")]
    pub ty: IdlTypeDefTy,
}

/// Type definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdlTypeDef {
    /// Custom type name
    pub name: String,
    /// Type structure (struct or enum)
    #[serde(rename = "type")]
    pub ty: IdlTypeDefTy,
}

/// Type definition inner structure
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum IdlTypeDefTy {
    /// Struct type with named fields
    #[serde(rename = "struct")]
    Struct {
        /// Struct field definitions
        fields: Vec<IdlField>
    },
    /// Enum type with variants
    #[serde(rename = "enum")]
    Enum {
        /// Enum variant definitions
        variants: Vec<IdlEnumVariant>
    },
}

/// Field definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdlField {
    /// Field name
    pub name: String,
    /// Field type
    #[serde(rename = "type")]
    pub ty: IdlType,
}

/// Enum variant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdlEnumVariant {
    /// Variant name
    pub name: String,
    /// Optional fields for tuple or struct variants
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fields: Option<Vec<IdlField>>,
}

/// Supported IDL types
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum IdlType {
    /// Primitive type: "u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "bool", "string", "publicKey"
    Primitive(String),
    /// Fixed-size array: [type, length]
    Array {
        /// Array element type and size
        array: [Box<IdlType>; 2]
    },
    /// Dynamic vector type
    Vec {
        /// Vector element type
        vec: Box<IdlType>
    },
    /// Optional/nullable type
    Option {
        /// Inner type
        option: Box<IdlType>
    },
    /// Custom type reference (user-defined)
    Defined {
        /// Type name
        defined: String
    },
}

/// Error definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdlError {
    /// Error code (typically 6000+ for custom errors)
    pub code: u32,
    /// Error name (PascalCase)
    pub name: String,
    /// Optional error message
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub msg: Option<String>,
}

/// Metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdlMetadata {
    /// Deployed program address (base58 encoded public key)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub address: Option<String>,
}

/// IDL Generator - parses OVSM source and generates Anchor IDL
pub struct IdlGenerator<'a> {
    source: &'a str,
    name: String,
    version: String,
    instructions: Vec<IdlInstruction>,
    accounts: Vec<IdlAccountDef>,
    types: Vec<IdlTypeDef>,
    errors: Vec<IdlError>,
}

impl<'a> IdlGenerator<'a> {
    /// Create a new IDL generator from OVSM source
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            name: String::new(),
            version: "0.1.0".to_string(),
            instructions: Vec::new(),
            accounts: Vec::new(),
            types: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Set program name (defaults to extracted from comments).
    /// Returns self for method chaining.
    pub fn with_name(mut self, name: &str) -> Self {
        self.name = name.to_string();
        self
    }

    /// Set IDL version string (default: "0.1.0").
    /// Returns self for method chaining.
    pub fn with_version(mut self, version: &str) -> Self {
        self.version = version.to_string();
        self
    }

    /// Generate the IDL structure from parsed OVSM source.
    /// Extracts instructions, accounts, errors, and types from the source code.
    pub fn generate(&mut self) -> Result<AnchorIdl, String> {
        self.parse_source()?;

        Ok(AnchorIdl {
            version: self.version.clone(),
            name: if self.name.is_empty() {
                "ovsm_program".to_string()
            } else {
                self.name.clone()
            },
            instructions: self.instructions.clone(),
            accounts: self.accounts.clone(),
            types: self.types.clone(),
            errors: self.errors.clone(),
            metadata: None,
        })
    }

    /// Generate IDL as pretty-printed JSON string.
    /// Convenience method that calls generate() and serializes the result.
    pub fn generate_json(&mut self) -> Result<String, String> {
        let idl = self.generate()?;
        serde_json::to_string_pretty(&idl).map_err(|e| format!("JSON serialization failed: {}", e))
    }

    /// Parse the OVSM source and extract IDL information
    fn parse_source(&mut self) -> Result<(), String> {
        // Extract program name from header comments
        self.extract_program_name();

        // Extract account layouts from comments
        self.extract_account_layouts();

        // Extract instructions from discriminator patterns
        self.extract_instructions();

        // Extract errors from ERR: patterns
        self.extract_errors();

        Ok(())
    }

    /// Extract program name from header comments
    /// Looks for patterns like: ;;; AGENT MARKETPLACE - ...
    fn extract_program_name(&mut self) {
        for line in self.source.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with(";;;") {
                // Extract name from first header comment
                let content = trimmed.trim_start_matches(';').trim();
                // Skip separator lines like === or ---
                if content.starts_with('=') || content.starts_with('-') || content.is_empty() {
                    continue;
                }
                if let Some(name_part) = content.split(['-', ':']).next() {
                    let name = name_part
                        .trim()
                        .to_lowercase()
                        .replace(' ', "_")
                        .replace("=", "")
                        .replace("-", "_")
                        .trim_matches('_')
                        .to_string();
                    if !name.is_empty() && !name.contains("accounts") && name.len() > 2 {
                        self.name = name;
                        break;
                    }
                }
            }
        }
    }

    /// Extract account layouts from comments
    /// Looks for patterns like: ;;;   0: Account description
    fn extract_account_layouts(&mut self) {
        let mut in_accounts_section = false;
        let mut current_accounts: Vec<IdlAccountMeta> = Vec::new();

        for line in self.source.lines() {
            let trimmed = line.trim();

            // Check for account section headers
            if trimmed.contains("Accounts:") || trimmed.contains("Account Layout:") {
                in_accounts_section = true;
                continue;
            }

            // Check for end of accounts section
            if in_accounts_section && (trimmed.starts_with(";;;") && !trimmed.contains(":")) {
                in_accounts_section = false;
            }

            // Parse account definition: ;;;   N: Description
            if in_accounts_section && trimmed.starts_with(";;;") {
                let content = trimmed.trim_start_matches(';').trim();
                if let Some((idx_str, desc)) = content.split_once(':') {
                    let idx_str = idx_str.trim();
                    if idx_str.parse::<u32>().is_ok() {
                        let desc = desc.trim();
                        let (name, is_mut, is_signer) = self.parse_account_desc(desc, idx_str);
                        current_accounts.push(IdlAccountMeta {
                            name,
                            is_mut,
                            is_signer,
                            docs: Some(vec![desc.to_string()]),
                        });
                    }
                }
            }
        }

        // Store parsed accounts for instruction use
        // Will be associated with instructions during extraction
    }

    /// Parse account description and infer properties
    fn parse_account_desc(&self, desc: &str, idx: &str) -> (String, bool, bool) {
        let desc_lower = desc.to_lowercase();

        // Infer name from description
        let name = if desc_lower.contains("state") {
            format!("state_{}", idx)
        } else if desc_lower.contains("authority") || desc_lower.contains("signer") {
            "authority".to_string()
        } else if desc_lower.contains("pda") {
            format!("pda_{}", idx)
        } else if desc_lower.contains("token") {
            format!("token_account_{}", idx)
        } else if desc_lower.contains("system") {
            "system_program".to_string()
        } else {
            format!("account_{}", idx)
        };

        // Infer mutability
        let is_mut = desc_lower.contains("writable")
            || desc_lower.contains("state")
            || desc_lower.contains("escrow")
            || desc_lower.contains("destination")
            || !desc_lower.contains("readonly");

        // Infer signer requirement
        let is_signer = desc_lower.contains("signer")
            || desc_lower.contains("authority")
            || desc_lower.contains("payer");

        (name, is_mut, is_signer)
    }

    /// Extract instructions from discriminator patterns
    /// Looks for: (if (= discriminator N) ...)
    fn extract_instructions(&mut self) {
        let mut instruction_map: HashMap<u8, String> = HashMap::new();

        // Pattern 1: Look for instruction comments
        // ;;; INSTRUCTION N: NAME
        for line in self.source.lines() {
            let trimmed = line.trim();
            if trimmed.contains("INSTRUCTION") || trimmed.contains("Instruction") {
                // Try to extract: INSTRUCTION N: NAME or Instruction N = NAME
                let content = trimmed.trim_start_matches(';').trim();
                if let Some(instr_part) = content.strip_prefix("INSTRUCTION") {
                    let instr_part = instr_part.trim();
                    // Parse "N: NAME" or "N = NAME"
                    for sep in [':', '=', '-'] {
                        if let Some((num_str, name)) = instr_part.split_once(sep) {
                            if let Ok(num) = num_str.trim().parse::<u8>() {
                                let name = self.camel_case(name.trim());
                                instruction_map.insert(num, name);
                                break;
                            }
                        }
                    }
                }
            }
        }

        // Pattern 2: Look for discriminator checks followed by log messages
        // (if (= discriminator N) ... with nearby ">>> NAME <<<"
        let lines: Vec<&str> = self.source.lines().collect();
        let mut i = 0;
        while i < lines.len() {
            let line = lines[i];

            // Check for discriminator comparison: (= discriminator N)
            if line.contains("discriminator") {
                // Try to extract number from this or nearby lines
                let mut disc: Option<u8> = None;

                // Look for the number in current and next few lines
                for offset in 0..3 {
                    if i + offset >= lines.len() {
                        break;
                    }
                    let check_line = lines[i + offset];
                    // Look for pattern: = discriminator NUM) or (= discriminator NUM)
                    for word in
                        check_line.split(|c: char| c.is_whitespace() || c == '(' || c == ')')
                    {
                        if let Ok(num) = word.parse::<u8>() {
                            disc = Some(num);
                            break;
                        }
                    }
                    if disc.is_some() {
                        break;
                    }
                }

                // If we found a discriminator, look for the log message
                if let Some(d) = disc {
                    // Search next several lines for >>> NAME <<<
                    for offset in 0..10 {
                        if i + offset >= lines.len() {
                            break;
                        }
                        let check_line = lines[i + offset];
                        if check_line.contains(">>>") && check_line.contains("<<<") {
                            if let Some(start) = check_line.find(">>>") {
                                if let Some(end) = check_line.find("<<<") {
                                    if start < end {
                                        let name = check_line[start + 3..end].trim();
                                        if !name.is_empty() && !instruction_map.contains_key(&d) {
                                            instruction_map.insert(d, self.camel_case(name));
                                        }
                                    }
                                }
                            }
                            break;
                        }
                    }
                }
            }
            i += 1;
        }

        // Create instructions from map
        let mut instructions: Vec<(u8, IdlInstruction)> = instruction_map
            .into_iter()
            .map(|(disc, name)| {
                let args = self.infer_instruction_args(&name);
                (
                    disc,
                    IdlInstruction {
                        name,
                        accounts: self.infer_instruction_accounts(disc),
                        args,
                        discriminator: Some(vec![disc]),
                    },
                )
            })
            .collect();

        // Sort by discriminator
        instructions.sort_by_key(|(d, _)| *d);

        self.instructions = instructions.into_iter().map(|(_, i)| i).collect();
    }

    /// Infer instruction accounts based on discriminator
    fn infer_instruction_accounts(&self, _disc: u8) -> Vec<IdlAccountMeta> {
        // Default accounts - can be enhanced by parsing actual usage
        vec![
            IdlAccountMeta {
                name: "state".to_string(),
                is_mut: true,
                is_signer: false,
                docs: Some(vec!["Program state account".to_string()]),
            },
            IdlAccountMeta {
                name: "authority".to_string(),
                is_mut: false,
                is_signer: true,
                docs: Some(vec!["Transaction signer".to_string()]),
            },
        ]
    }

    /// Infer instruction arguments from name and patterns
    fn infer_instruction_args(&self, name: &str) -> Vec<IdlArg> {
        let name_lower = name.to_lowercase();
        let mut args = Vec::new();

        // Common patterns
        if name_lower.contains("transfer")
            || name_lower.contains("deposit")
            || name_lower.contains("withdraw")
        {
            args.push(IdlArg {
                name: "amount".to_string(),
                ty: IdlType::Primitive("u64".to_string()),
            });
        }

        if name_lower.contains("register") {
            args.push(IdlArg {
                name: "agent_id".to_string(),
                ty: IdlType::Primitive("u64".to_string()),
            });
        }

        if name_lower.contains("complete") || name_lower.contains("task") {
            args.push(IdlArg {
                name: "rating".to_string(),
                ty: IdlType::Primitive("u8".to_string()),
            });
        }

        if name_lower.contains("list") || name_lower.contains("price") {
            args.push(IdlArg {
                name: "price".to_string(),
                ty: IdlType::Primitive("u64".to_string()),
            });
        }

        if name_lower.contains("hire") || name_lower.contains("job") {
            args.push(IdlArg {
                name: "job_id".to_string(),
                ty: IdlType::Primitive("u64".to_string()),
            });
        }

        if name_lower.contains("dispute") {
            args.push(IdlArg {
                name: "reason".to_string(),
                ty: IdlType::Primitive("u8".to_string()),
            });
        }

        args
    }

    /// Extract error definitions from ERR: patterns
    fn extract_errors(&mut self) {
        let mut error_code = 6000u32; // Anchor custom error base

        for line in self.source.lines() {
            if line.contains("ERR:") {
                // Extract error message from: (sol_log_ "ERR: Message")
                if let Some(start) = line.find("ERR:") {
                    let rest = &line[start + 4..];
                    if let Some(end) = rest.find('"') {
                        let msg = rest[..end].trim();
                        let name = self.error_name_from_msg(msg);

                        self.errors.push(IdlError {
                            code: error_code,
                            name,
                            msg: Some(msg.to_string()),
                        });
                        error_code += 1;
                    }
                }
            }
        }
    }

    /// Convert error message to error name
    fn error_name_from_msg(&self, msg: &str) -> String {
        msg.split_whitespace()
            .map(|w| {
                let mut chars = w.chars();
                match chars.next() {
                    None => String::new(),
                    Some(c) => c.to_uppercase().chain(chars).collect(),
                }
            })
            .collect::<String>()
            .chars()
            .filter(|c| c.is_alphanumeric())
            .collect()
    }

    /// Convert to camelCase
    fn camel_case(&self, s: &str) -> String {
        let mut result = String::new();
        let mut first_word = true;

        for word in s.split([' ', '_', '-']) {
            if word.is_empty() {
                continue;
            }

            for (i, c) in word.chars().enumerate() {
                if first_word && i == 0 {
                    result.push(c.to_ascii_lowercase());
                } else if !first_word && i == 0 {
                    result.push(c.to_ascii_uppercase());
                } else {
                    result.push(c.to_ascii_lowercase());
                }
            }
            first_word = false;
        }

        result
    }
}

/// Generate Anchor IDL JSON from OVSM source string.
/// Convenience function that creates a generator, optionally sets the name, and returns JSON.
/// Returns the IDL as a pretty-printed JSON string.
pub fn generate_idl(source: &str, name: Option<&str>) -> Result<String, String> {
    let mut generator = IdlGenerator::new(source);
    if let Some(name) = name {
        generator = generator.with_name(name);
    }
    generator.generate_json()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_idl_generation() {
        // Test with simpler instruction pattern
        let source = r#"
;;; TEST PROGRAM - Basic test
;;;
;;; Accounts:
;;;   0: State account (writable)
;;;   1: Authority (signer)
;;;

(do
  (sol_log_ "Test program")
  0)
"#;

        let mut generator = IdlGenerator::new(source);
        let idl = generator.generate().unwrap();

        assert_eq!(idl.name, "test_program");
        // May not have instructions extracted from simple program
        assert_eq!(idl.version, "0.1.0");
    }

    #[test]
    fn test_accountability_demo_idl() {
        // Test with real accountability demo pattern
        let source = r#"
;;; AGENT ACCOUNTABILITY DEMO - Complete End-to-End Workflow
;;;
;;; Accounts:
;;;   0: Demo state account (writable)
;;;   1: Agent wallet
;;;   2: Client wallet
;;;   3: Authority/Signer

(do
  (define discriminator (mem-load1 instr_ptr 0))

  (if (= discriminator 0)
    (do
      (sol_log_ ">>> INIT DEMO <<<")
      0)
    0)

  (if (= discriminator 1)
    (do
      (sol_log_ ">>> REGISTER AGENT <<<")
      0)
    0)

  (if (= discriminator 2)
    (do
      (sol_log_ ">>> COMPLETE TASK <<<")
      0)
    0)

  0)
"#;

        let mut generator = IdlGenerator::new(source);
        let idl = generator.generate().unwrap();

        assert_eq!(idl.name, "agent_accountability_demo");
        assert!(
            idl.instructions.len() >= 3,
            "Expected at least 3 instructions, got {}",
            idl.instructions.len()
        );

        // Check instruction names are camelCase
        let names: Vec<&str> = idl.instructions.iter().map(|i| i.name.as_str()).collect();
        assert!(names.contains(&"initDemo"));
        assert!(names.contains(&"registerAgent"));
        assert!(names.contains(&"completeTask"));
    }

    #[test]
    fn test_camel_case() {
        let generator = IdlGenerator::new("");
        assert_eq!(generator.camel_case("INIT DEMO"), "initDemo");
        assert_eq!(generator.camel_case("REGISTER AGENT"), "registerAgent");
        assert_eq!(generator.camel_case("complete_task"), "completeTask");
        assert_eq!(generator.camel_case("hello-world"), "helloWorld");
    }

    #[test]
    fn test_error_extraction() {
        let source = r#"
(do
  (if condition
    (do (sol_log_ "ERR: Already registered") 1)
    0)
  (if condition2
    (do (sol_log_ "ERR: Not authorized") 2)
    0))
"#;

        let mut generator = IdlGenerator::new(source);
        generator.extract_errors();

        assert_eq!(generator.errors.len(), 2);
        assert_eq!(generator.errors[0].name, "AlreadyRegistered");
        assert_eq!(generator.errors[1].name, "NotAuthorized");
    }

    #[test]
    fn test_idl_json_serialization() {
        let idl = AnchorIdl {
            version: "0.1.0".to_string(),
            name: "test".to_string(),
            instructions: vec![IdlInstruction {
                name: "initialize".to_string(),
                accounts: vec![IdlAccountMeta {
                    name: "state".to_string(),
                    is_mut: true,
                    is_signer: false,
                    docs: None,
                }],
                args: vec![IdlArg {
                    name: "amount".to_string(),
                    ty: IdlType::Primitive("u64".to_string()),
                }],
                discriminator: Some(vec![0]),
            }],
            accounts: vec![],
            types: vec![],
            errors: vec![],
            metadata: None,
        };

        let json = serde_json::to_string_pretty(&idl).unwrap();
        assert!(json.contains("\"name\": \"test\""));
        assert!(json.contains("\"initialize\""));
        assert!(json.contains("\"isMut\": true"));
        assert!(json.contains("\"isSigner\": false"));
    }
}
