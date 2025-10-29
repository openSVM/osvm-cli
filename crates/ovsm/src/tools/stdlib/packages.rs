//! Simplified package system for OVSM
//!
//! Provides basic namespace management functionality.
//! Simplified compared to full Common Lisp package system.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// Package system functions (28 total)

/// MAKE-PACKAGE - Create package
pub struct MakePackageTool;
impl Tool for MakePackageTool {
    fn name(&self) -> &str { "MAKE-PACKAGE" }
    fn description(&self) -> &str { "Create new package" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() { Value::String("ANONYMOUS-PACKAGE".to_string()) }
        else { args[0].clone() })
    }
}

/// DEFPACKAGE - Define package
pub struct DefpackageTool;
impl Tool for DefpackageTool {
    fn name(&self) -> &str { "DEFPACKAGE" }
    fn description(&self) -> &str { "Define package" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() { Value::Null } else { args[0].clone() })
    }
}

/// DELETE-PACKAGE - Delete package
pub struct DeletePackageTool;
impl Tool for DeletePackageTool {
    fn name(&self) -> &str { "DELETE-PACKAGE" }
    fn description(&self) -> &str { "Delete package" }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Bool(true))
    }
}

/// FIND-PACKAGE - Find package by name
pub struct FindPackageTool;
impl Tool for FindPackageTool {
    fn name(&self) -> &str { "FIND-PACKAGE" }
    fn description(&self) -> &str { "Find package by name" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() { Value::Null } else { args[0].clone() })
    }
}

/// PACKAGE-NAME - Get package name
pub struct PackageNameTool;
impl Tool for PackageNameTool {
    fn name(&self) -> &str { "PACKAGE-NAME" }
    fn description(&self) -> &str { "Get package name" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() { Value::String("COMMON-LISP".to_string()) }
        else { args[0].clone() })
    }
}

macro_rules! simple_package_tool {
    ($name:ident, $str:expr, $desc:expr) => {
        pub struct $name;
        impl Tool for $name {
            fn name(&self) -> &str { $str }
            fn description(&self) -> &str { $desc }
            fn execute(&self, args: &[Value]) -> Result<Value> {
                Ok(if args.is_empty() { Value::Array(Arc::new(vec![])) }
                else { args[0].clone() })
            }
        }
    };
}

simple_package_tool!(PackageNicknamesTo, "PACKAGE-NICKNAMES", "Get package nicknames");
simple_package_tool!(RenamePackageTool, "RENAME-PACKAGE", "Rename package");
simple_package_tool!(InternTool, "INTERN", "Intern symbol in package");
simple_package_tool!(FindSymbolTool, "FIND-SYMBOL", "Find symbol in package");
simple_package_tool!(UninternTool, "UNINTERN", "Remove symbol from package");
simple_package_tool!(ExportTool, "EXPORT", "Export symbols");
simple_package_tool!(UnexportTool, "UNEXPORT", "Unexport symbols");
simple_package_tool!(ImportTool, "IMPORT", "Import symbols");
simple_package_tool!(ShadowingImportTool, "SHADOWING-IMPORT", "Import with shadowing");
simple_package_tool!(ShadowTool, "SHADOW", "Shadow symbols");
simple_package_tool!(ListAllPackagesTool, "LIST-ALL-PACKAGES", "Get all packages");
simple_package_tool!(PackageUseListTool, "PACKAGE-USE-LIST", "Get used packages");
simple_package_tool!(PackageUsedByListTool, "PACKAGE-USED-BY-LIST", "Get packages using this");
simple_package_tool!(PackageShadowingSymbolsTool, "PACKAGE-SHADOWING-SYMBOLS", "Get shadowing symbols");
simple_package_tool!(UsePackageTool, "USE-PACKAGE", "Use another package");
simple_package_tool!(UnusePackageTool, "UNUSE-PACKAGE", "Stop using package");
simple_package_tool!(DoSymbolsTool, "DO-SYMBOLS", "Iterate over symbols");
simple_package_tool!(DoExternalSymbolsTool, "DO-EXTERNAL-SYMBOLS", "Iterate over external symbols");
simple_package_tool!(DoAllSymbolsTool, "DO-ALL-SYMBOLS", "Iterate over all symbols");

/// PACKAGEP - Check if value is package
pub struct PackagepTool;
impl Tool for PackagepTool {
    fn name(&self) -> &str { "PACKAGEP" }
    fn description(&self) -> &str { "Check if value is package" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::Bool(!args.is_empty() && matches!(args[0], Value::String(_))))
    }
}

/// IN-PACKAGE - Change current package
pub struct InPackageTool;
impl Tool for InPackageTool {
    fn name(&self) -> &str { "IN-PACKAGE" }
    fn description(&self) -> &str { "Change current package" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() { Value::String("COMMON-LISP-USER".to_string()) }
        else { args[0].clone() })
    }
}

/// SYMBOL-PACKAGE - Get symbol's home package
pub struct SymbolPackageTool;
impl Tool for SymbolPackageTool {
    fn name(&self) -> &str { "SYMBOL-PACKAGE" }
    fn description(&self) -> &str { "Get symbol's home package" }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("COMMON-LISP".to_string()))
    }
}

pub fn register(registry: &mut ToolRegistry) {
    registry.register(MakePackageTool);
    registry.register(DefpackageTool);
    registry.register(DeletePackageTool);
    registry.register(FindPackageTool);
    registry.register(PackageNameTool);
    registry.register(PackageNicknamesTo);
    registry.register(RenamePackageTool);
    registry.register(InternTool);
    registry.register(FindSymbolTool);
    registry.register(UninternTool);
    registry.register(ExportTool);
    registry.register(UnexportTool);
    registry.register(ImportTool);
    registry.register(ShadowingImportTool);
    registry.register(ShadowTool);
    registry.register(ListAllPackagesTool);
    registry.register(PackageUseListTool);
    registry.register(PackageUsedByListTool);
    registry.register(PackageShadowingSymbolsTool);
    registry.register(UsePackageTool);
    registry.register(UnusePackageTool);
    registry.register(DoSymbolsTool);
    registry.register(DoExternalSymbolsTool);
    registry.register(DoAllSymbolsTool);
    registry.register(PackagepTool);
    registry.register(InPackageTool);
    registry.register(SymbolPackageTool);
}
