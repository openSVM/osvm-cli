//! # Anchor IDL Support
//!
//! Parses Anchor IDL JSON files to provide semantic naming
//! for decompiled programs.

use crate::{Error, Result};
use std::collections::HashMap;

/// Anchor IDL structure
#[derive(Debug, Clone)]
pub struct AnchorIdl {
    /// Program name
    pub name: String,
    /// Program version
    pub version: String,
    /// Instructions defined in the IDL
    pub instructions: Vec<IdlInstruction>,
    /// Account types
    pub accounts: Vec<IdlAccount>,
    /// Custom types
    pub types: Vec<IdlType>,
    /// Error codes
    pub errors: Vec<IdlError>,
}

/// IDL instruction definition
#[derive(Debug, Clone)]
pub struct IdlInstruction {
    /// Instruction name (camelCase)
    pub name: String,
    /// Discriminator (8-byte hash)
    pub discriminator: Option<[u8; 8]>,
    /// Accounts required by this instruction
    pub accounts: Vec<IdlInstructionAccount>,
    /// Arguments to the instruction
    pub args: Vec<IdlArg>,
}

/// IDL instruction account
#[derive(Debug, Clone)]
pub struct IdlInstructionAccount {
    /// Account name
    pub name: String,
    /// Is this account mutable?
    pub is_mut: bool,
    /// Is this account a signer?
    pub is_signer: bool,
    /// Optional description
    pub description: Option<String>,
}

/// IDL argument
#[derive(Debug, Clone)]
pub struct IdlArg {
    /// Argument name
    pub name: String,
    /// Type name
    pub ty: String,
}

/// IDL account type
#[derive(Debug, Clone)]
pub struct IdlAccount {
    /// Account name
    pub name: String,
    /// Discriminator
    pub discriminator: Option<[u8; 8]>,
    /// Fields in the account
    pub fields: Vec<IdlField>,
}

/// IDL field
#[derive(Debug, Clone)]
pub struct IdlField {
    /// Field name
    pub name: String,
    /// Type name
    pub ty: String,
}

/// IDL custom type
#[derive(Debug, Clone)]
pub struct IdlType {
    /// Type name
    pub name: String,
    /// Type kind (struct, enum)
    pub kind: IdlTypeKind,
}

/// Type kind
#[derive(Debug, Clone)]
pub enum IdlTypeKind {
    /// Struct with fields
    Struct(Vec<IdlField>),
    /// Enum with variants
    Enum(Vec<String>),
}

/// IDL error code
#[derive(Debug, Clone)]
pub struct IdlError {
    /// Error code number
    pub code: u32,
    /// Error name
    pub name: String,
    /// Error message
    pub msg: Option<String>,
}

impl AnchorIdl {
    /// Load IDL from JSON file
    pub fn load(path: &str) -> Result<Self> {
        let contents = std::fs::read_to_string(path)
            .map_err(|e| Error::runtime(format!("Failed to read IDL file: {}", e)))?;

        Self::parse(&contents)
    }

    /// Parse IDL from JSON string
    pub fn parse(json: &str) -> Result<Self> {
        let value: serde_json::Value = serde_json::from_str(json)
            .map_err(|e| Error::runtime(format!("Failed to parse IDL JSON: {}", e)))?;

        let name = value["name"].as_str().unwrap_or("unknown").to_string();
        let version = value["version"].as_str().unwrap_or("0.0.0").to_string();

        // Parse instructions
        let mut instructions = Vec::new();
        if let Some(instrs) = value["instructions"].as_array() {
            for instr in instrs {
                instructions.push(Self::parse_instruction(instr)?);
            }
        }

        // Parse accounts
        let mut accounts = Vec::new();
        if let Some(accts) = value["accounts"].as_array() {
            for acct in accts {
                accounts.push(Self::parse_account(acct)?);
            }
        }

        // Parse types
        let mut types = Vec::new();
        if let Some(tys) = value["types"].as_array() {
            for ty in tys {
                if let Ok(parsed) = Self::parse_type(ty) {
                    types.push(parsed);
                }
            }
        }

        // Parse errors
        let mut errors = Vec::new();
        if let Some(errs) = value["errors"].as_array() {
            for err in errs {
                errors.push(Self::parse_error(err)?);
            }
        }

        Ok(AnchorIdl {
            name,
            version,
            instructions,
            accounts,
            types,
            errors,
        })
    }

    fn parse_instruction(value: &serde_json::Value) -> Result<IdlInstruction> {
        let name = value["name"].as_str().unwrap_or("unknown").to_string();

        // Parse discriminator
        let discriminator = if let Some(disc) = value["discriminator"].as_array() {
            let bytes: Vec<u8> = disc
                .iter()
                .filter_map(|v| v.as_u64().map(|n| n as u8))
                .collect();
            if bytes.len() == 8 {
                let mut arr = [0u8; 8];
                arr.copy_from_slice(&bytes);
                Some(arr)
            } else {
                None
            }
        } else {
            None
        };

        // Parse accounts
        let mut accounts = Vec::new();
        if let Some(accts) = value["accounts"].as_array() {
            for acct in accts {
                accounts.push(IdlInstructionAccount {
                    name: acct["name"].as_str().unwrap_or("unknown").to_string(),
                    is_mut: acct["isMut"].as_bool().unwrap_or(false),
                    is_signer: acct["isSigner"].as_bool().unwrap_or(false),
                    description: acct["docs"]
                        .as_array()
                        .and_then(|d| d.first())
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string()),
                });
            }
        }

        // Parse args
        let mut args = Vec::new();
        if let Some(a) = value["args"].as_array() {
            for arg in a {
                args.push(IdlArg {
                    name: arg["name"].as_str().unwrap_or("unknown").to_string(),
                    ty: Self::type_to_string(&arg["type"]),
                });
            }
        }

        Ok(IdlInstruction {
            name,
            discriminator,
            accounts,
            args,
        })
    }

    fn parse_account(value: &serde_json::Value) -> Result<IdlAccount> {
        let name = value["name"].as_str().unwrap_or("unknown").to_string();

        let discriminator = if let Some(disc) = value["discriminator"].as_array() {
            let bytes: Vec<u8> = disc
                .iter()
                .filter_map(|v| v.as_u64().map(|n| n as u8))
                .collect();
            if bytes.len() == 8 {
                let mut arr = [0u8; 8];
                arr.copy_from_slice(&bytes);
                Some(arr)
            } else {
                None
            }
        } else {
            None
        };

        let mut fields = Vec::new();
        if let Some(ty) = value["type"].as_object() {
            if let Some(f) = ty.get("fields").and_then(|v| v.as_array()) {
                for field in f {
                    fields.push(IdlField {
                        name: field["name"].as_str().unwrap_or("unknown").to_string(),
                        ty: Self::type_to_string(&field["type"]),
                    });
                }
            }
        }

        Ok(IdlAccount {
            name,
            discriminator,
            fields,
        })
    }

    fn parse_type(value: &serde_json::Value) -> Result<IdlType> {
        let name = value["name"].as_str().unwrap_or("unknown").to_string();

        let kind = if let Some(ty) = value["type"].as_object() {
            if let Some("struct") = ty.get("kind").and_then(|v| v.as_str()) {
                let fields: Vec<IdlField> = ty
                    .get("fields")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .map(|f| IdlField {
                                name: f["name"].as_str().unwrap_or("unknown").to_string(),
                                ty: Self::type_to_string(&f["type"]),
                            })
                            .collect()
                    })
                    .unwrap_or_default();
                IdlTypeKind::Struct(fields)
            } else if let Some("enum") = ty.get("kind").and_then(|v| v.as_str()) {
                let variants: Vec<String> = ty
                    .get("variants")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| v["name"].as_str().map(|s| s.to_string()))
                            .collect()
                    })
                    .unwrap_or_default();
                IdlTypeKind::Enum(variants)
            } else {
                IdlTypeKind::Struct(Vec::new())
            }
        } else {
            IdlTypeKind::Struct(Vec::new())
        };

        Ok(IdlType { name, kind })
    }

    fn parse_error(value: &serde_json::Value) -> Result<IdlError> {
        Ok(IdlError {
            code: value["code"].as_u64().unwrap_or(0) as u32,
            name: value["name"].as_str().unwrap_or("unknown").to_string(),
            msg: value["msg"].as_str().map(|s| s.to_string()),
        })
    }

    fn type_to_string(value: &serde_json::Value) -> String {
        if let Some(s) = value.as_str() {
            s.to_string()
        } else if let Some(obj) = value.as_object() {
            if let Some(defined) = obj.get("defined") {
                if let Some(name) = defined.as_str() {
                    return name.to_string();
                }
                if let Some(name) = defined.get("name").and_then(|v| v.as_str()) {
                    return name.to_string();
                }
            }
            if let Some(vec) = obj.get("vec") {
                return format!("Vec<{}>", Self::type_to_string(vec));
            }
            if let Some(opt) = obj.get("option") {
                return format!("Option<{}>", Self::type_to_string(opt));
            }
            if let Some(arr) = obj.get("array") {
                if let Some(inner) = arr.as_array() {
                    if inner.len() == 2 {
                        return format!(
                            "[{}; {}]",
                            Self::type_to_string(&inner[0]),
                            inner[1].as_u64().unwrap_or(0)
                        );
                    }
                }
            }
            "unknown".to_string()
        } else {
            "unknown".to_string()
        }
    }

    /// Look up instruction by discriminator
    pub fn find_instruction_by_discriminator(&self, disc: &[u8; 8]) -> Option<&IdlInstruction> {
        self.instructions
            .iter()
            .find(|i| i.discriminator.as_ref() == Some(disc))
    }

    /// Look up instruction by name
    pub fn find_instruction(&self, name: &str) -> Option<&IdlInstruction> {
        self.instructions.iter().find(|i| i.name == name)
    }

    /// Look up account by name
    pub fn find_account(&self, name: &str) -> Option<&IdlAccount> {
        self.accounts.iter().find(|a| a.name == name)
    }

    /// Look up error by code
    pub fn find_error(&self, code: u32) -> Option<&IdlError> {
        self.errors.iter().find(|e| e.code == code)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_minimal_idl() {
        let json = r#"{
            "name": "test_program",
            "version": "1.0.0",
            "instructions": [],
            "accounts": [],
            "types": [],
            "errors": []
        }"#;

        let idl = AnchorIdl::parse(json).unwrap();
        assert_eq!(idl.name, "test_program");
        assert_eq!(idl.version, "1.0.0");
    }

    #[test]
    fn test_parse_instruction() {
        let json = r#"{
            "name": "test_program",
            "version": "1.0.0",
            "instructions": [
                {
                    "name": "initialize",
                    "accounts": [
                        {"name": "user", "isMut": true, "isSigner": true}
                    ],
                    "args": [
                        {"name": "amount", "type": "u64"}
                    ]
                }
            ],
            "accounts": [],
            "types": [],
            "errors": []
        }"#;

        let idl = AnchorIdl::parse(json).unwrap();
        assert_eq!(idl.instructions.len(), 1);
        assert_eq!(idl.instructions[0].name, "initialize");
        assert_eq!(idl.instructions[0].accounts.len(), 1);
        assert_eq!(idl.instructions[0].args.len(), 1);
    }
}
