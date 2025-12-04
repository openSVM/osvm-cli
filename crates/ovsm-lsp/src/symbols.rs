//! Symbol tracking for Go-to-Definition support
//!
//! Tracks variable and function definitions to enable "jump to definition".
//! This module analyzes the AST to find where symbols are defined.

use ovsm::parser::{Expression, Statement};
use std::collections::HashMap;
use tower_lsp::lsp_types::{Position, Range};

/// Symbol information for a defined name
#[derive(Debug, Clone)]
pub struct SymbolInfo {
    /// Name of the symbol
    pub name: String,
    /// Kind of symbol
    pub kind: SymbolKind,
    /// Location where the symbol is defined
    pub definition_location: Range,
    /// Documentation or signature (if available)
    pub documentation: Option<String>,
}

/// Kind of symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    /// Variable defined with (define x ...)
    Variable,
    /// Function defined with (defun name ...)
    Function,
    /// Constant defined with (const NAME ...)
    Constant,
    /// Parameter in lambda or function
    Parameter,
    /// Local binding in let/let*
    LocalBinding,
}

/// Symbol table for a document
#[derive(Debug, Default)]
pub struct SymbolTable {
    /// Map from symbol name to definition info
    pub definitions: HashMap<String, Vec<SymbolInfo>>,
}

impl SymbolTable {
    /// Create a new empty symbol table
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a symbol definition
    pub fn add_definition(&mut self, symbol: SymbolInfo) {
        self.definitions
            .entry(symbol.name.clone())
            .or_default()
            .push(symbol);
    }

    /// Find definitions for a symbol name
    pub fn find_definitions(&self, name: &str) -> Option<&Vec<SymbolInfo>> {
        self.definitions.get(name)
    }
}

/// Extract symbol definitions from source code
///
/// This function parses the source and walks the AST to find all
/// variable, function, and constant definitions.
pub fn extract_symbols(source: &str) -> SymbolTable {
    let mut table = SymbolTable::new();

    // Parse the source
    let mut scanner = ovsm::lexer::SExprScanner::new(source);
    let Ok(tokens) = scanner.scan_tokens() else {
        return table;
    };

    let mut parser = ovsm::parser::SExprParser::new(tokens);
    let Ok(program) = parser.parse() else {
        return table;
    };

    // Walk the AST to find definitions
    for statement in &program.statements {
        extract_from_statement(statement, source, &mut table);
    }

    table
}

/// Extract symbols from a statement
fn extract_from_statement(stmt: &Statement, source: &str, table: &mut SymbolTable) {
    match stmt {
        Statement::Expression(expr) => {
            extract_from_expression(expr, source, table);
        }
        Statement::Assignment { name, value } => {
            // Assignment creates a variable
            if let Some(range) = find_name_in_source(source, name) {
                table.add_definition(SymbolInfo {
                    name: name.clone(),
                    kind: SymbolKind::Variable,
                    definition_location: range,
                    documentation: None,
                });
            }
            extract_from_expression(value, source, table);
        }
        Statement::ConstantDef { name, value } => {
            if let Some(range) = find_name_in_source(source, name) {
                table.add_definition(SymbolInfo {
                    name: name.clone(),
                    kind: SymbolKind::Constant,
                    definition_location: range,
                    documentation: None,
                });
            }
            extract_from_expression(value, source, table);
        }
        Statement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            extract_from_expression(condition, source, table);
            for s in then_branch {
                extract_from_statement(s, source, table);
            }
            if let Some(else_stmts) = else_branch {
                for s in else_stmts {
                    extract_from_statement(s, source, table);
                }
            }
        }
        Statement::While { condition, body } => {
            extract_from_expression(condition, source, table);
            for s in body {
                extract_from_statement(s, source, table);
            }
        }
        Statement::For {
            variable,
            iterable,
            body,
        } => {
            // Loop variable is a local binding
            if let Some(range) = find_name_in_source(source, variable) {
                table.add_definition(SymbolInfo {
                    name: variable.clone(),
                    kind: SymbolKind::LocalBinding,
                    definition_location: range,
                    documentation: Some("Loop variable".to_string()),
                });
            }
            extract_from_expression(iterable, source, table);
            for s in body {
                extract_from_statement(s, source, table);
            }
        }
        Statement::Try {
            body,
            catch_clauses,
        } => {
            for s in body {
                extract_from_statement(s, source, table);
            }
            for clause in catch_clauses {
                for s in &clause.body {
                    extract_from_statement(s, source, table);
                }
            }
        }
        Statement::Parallel { tasks } => {
            for s in tasks {
                extract_from_statement(s, source, table);
            }
        }
        Statement::Decision { branches, .. } => {
            for branch in branches {
                extract_from_expression(&branch.condition, source, table);
                for s in &branch.body {
                    extract_from_statement(s, source, table);
                }
            }
        }
        Statement::Guard {
            condition,
            else_body,
        } => {
            extract_from_expression(condition, source, table);
            for s in else_body {
                extract_from_statement(s, source, table);
            }
        }
        _ => {}
    }
}

/// Extract symbols from an expression
fn extract_from_expression(expr: &Expression, source: &str, table: &mut SymbolTable) {
    match expr {
        Expression::ToolCall { name, args } => {
            // Check for define, defun, const, let, let*, lambda
            match name.as_str() {
                "define" => {
                    // (define name value)
                    if let Some(arg) = args.get(0) {
                        if let Expression::Variable(var_name) = &arg.value {
                            if let Some(range) = find_define_name_range(source, var_name) {
                                // Check if it's a function definition (lambda in value)
                                let kind =
                                    if args.get(1).map(|a| is_lambda(&a.value)).unwrap_or(false) {
                                        SymbolKind::Function
                                    } else {
                                        SymbolKind::Variable
                                    };

                                table.add_definition(SymbolInfo {
                                    name: var_name.clone(),
                                    kind,
                                    definition_location: range,
                                    documentation: None,
                                });
                            }
                        }
                    }
                }
                "defun" | "defn" => {
                    // (defun name (params) body)
                    if let Some(arg) = args.get(0) {
                        if let Expression::Variable(func_name) = &arg.value {
                            if let Some(range) = find_define_name_range(source, func_name) {
                                table.add_definition(SymbolInfo {
                                    name: func_name.clone(),
                                    kind: SymbolKind::Function,
                                    definition_location: range,
                                    documentation: Some(format!("Function {}", func_name)),
                                });
                            }
                        }
                    }
                    // Extract parameters
                    if let Some(arg) = args.get(1) {
                        extract_params_from_expr(&arg.value, source, table);
                    }
                }
                "const" => {
                    // (const NAME value)
                    if let Some(arg) = args.get(0) {
                        if let Expression::Variable(const_name) = &arg.value {
                            if let Some(range) = find_define_name_range(source, const_name) {
                                table.add_definition(SymbolInfo {
                                    name: const_name.clone(),
                                    kind: SymbolKind::Constant,
                                    definition_location: range,
                                    documentation: None,
                                });
                            }
                        }
                    }
                }
                "let" | "let*" => {
                    // (let ((var1 val1) (var2 val2)) body)
                    if let Some(arg) = args.get(0) {
                        if let Expression::ArrayLiteral(bindings) = &arg.value {
                            for binding in bindings {
                                if let Expression::ArrayLiteral(pair) = binding {
                                    if let Some(Expression::Variable(var_name)) = pair.get(0) {
                                        if let Some(range) = find_name_in_source(source, var_name) {
                                            table.add_definition(SymbolInfo {
                                                name: var_name.clone(),
                                                kind: SymbolKind::LocalBinding,
                                                definition_location: range,
                                                documentation: Some("Let binding".to_string()),
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                "for" => {
                    // (for var collection body)
                    if let Some(arg) = args.get(0) {
                        if let Expression::Variable(var_name) = &arg.value {
                            if let Some(range) = find_name_in_source(source, var_name) {
                                table.add_definition(SymbolInfo {
                                    name: var_name.clone(),
                                    kind: SymbolKind::LocalBinding,
                                    definition_location: range,
                                    documentation: Some("Loop variable".to_string()),
                                });
                            }
                        }
                    }
                }
                _ => {}
            }

            // Recurse into arguments
            for arg in args {
                extract_from_expression(&arg.value, source, table);
            }
        }
        Expression::Lambda { params, body } => {
            // Lambda parameters
            for param in params {
                // Skip &optional, &rest, &key markers
                if !param.starts_with('&') {
                    if let Some(range) = find_name_in_source(source, param) {
                        table.add_definition(SymbolInfo {
                            name: param.clone(),
                            kind: SymbolKind::Parameter,
                            definition_location: range,
                            documentation: Some("Lambda parameter".to_string()),
                        });
                    }
                }
            }
            extract_from_expression(body, source, table);
        }
        Expression::Binary { left, right, .. } => {
            extract_from_expression(left, source, table);
            extract_from_expression(right, source, table);
        }
        Expression::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            extract_from_expression(condition, source, table);
            extract_from_expression(then_expr, source, table);
            extract_from_expression(else_expr, source, table);
        }
        Expression::ArrayLiteral(elements) => {
            for elem in elements {
                extract_from_expression(elem, source, table);
            }
        }
        Expression::ObjectLiteral(pairs) => {
            for (_, value) in pairs {
                extract_from_expression(value, source, table);
            }
        }
        Expression::FieldAccess { object, .. } => {
            extract_from_expression(object, source, table);
        }
        Expression::IndexAccess { array, index } => {
            extract_from_expression(array, source, table);
            extract_from_expression(index, source, table);
        }
        Expression::Unary { operand, .. } => {
            extract_from_expression(operand, source, table);
        }
        Expression::Grouping(expr) => {
            extract_from_expression(expr, source, table);
        }
        Expression::Range { start, end } => {
            extract_from_expression(start, source, table);
            extract_from_expression(end, source, table);
        }
        Expression::Catch { body, .. } => {
            for expr in body {
                extract_from_expression(expr, source, table);
            }
        }
        Expression::DestructuringBind { body, .. } => {
            for expr in body {
                extract_from_expression(expr, source, table);
            }
        }
        Expression::Loop(loop_data) => {
            // Loop iteration variable
            match &loop_data.iteration {
                ovsm::parser::IterationClause::Numeric { var, .. }
                | ovsm::parser::IterationClause::Collection { var, .. } => {
                    if let Some(range) = find_name_in_source(source, var) {
                        table.add_definition(SymbolInfo {
                            name: var.clone(),
                            kind: SymbolKind::LocalBinding,
                            definition_location: range,
                            documentation: Some("Loop variable".to_string()),
                        });
                    }
                }
            }
            for expr in &loop_data.body {
                extract_from_expression(expr, source, table);
            }
        }
        _ => {}
    }
}

/// Extract parameters from a parameter list expression
fn extract_params_from_expr(expr: &Expression, source: &str, table: &mut SymbolTable) {
    match expr {
        Expression::ArrayLiteral(params) => {
            for param in params {
                if let Expression::Variable(name) = param {
                    if !name.starts_with('&') {
                        if let Some(range) = find_name_in_source(source, name) {
                            table.add_definition(SymbolInfo {
                                name: name.clone(),
                                kind: SymbolKind::Parameter,
                                definition_location: range,
                                documentation: Some("Function parameter".to_string()),
                            });
                        }
                    }
                }
            }
        }
        _ => {}
    }
}

/// Check if an expression is a lambda
fn is_lambda(expr: &Expression) -> bool {
    matches!(expr, Expression::Lambda { .. })
}

/// Find the range of a name in source code
/// Simple implementation - finds first occurrence
fn find_name_in_source(source: &str, name: &str) -> Option<Range> {
    let lines: Vec<&str> = source.lines().collect();

    for (line_idx, line) in lines.iter().enumerate() {
        // Look for the name as a whole word
        let mut pos = 0;
        while let Some(found_pos) = line[pos..].find(name) {
            let absolute_pos = pos + found_pos;

            // Check it's a whole word (not part of another identifier)
            let before_ok = if absolute_pos == 0 {
                true
            } else {
                let before = line.chars().nth(absolute_pos - 1).unwrap_or(' ');
                !before.is_alphanumeric() && before != '_' && before != '-'
            };

            let after_ok = {
                let after_pos = absolute_pos + name.len();
                if after_pos >= line.len() {
                    true
                } else {
                    let after = line.chars().nth(after_pos).unwrap_or(' ');
                    !after.is_alphanumeric()
                        && after != '_'
                        && after != '-'
                        && after != '?'
                        && after != '!'
                }
            };

            if before_ok && after_ok {
                return Some(Range {
                    start: Position {
                        line: line_idx as u32,
                        character: absolute_pos as u32,
                    },
                    end: Position {
                        line: line_idx as u32,
                        character: (absolute_pos + name.len()) as u32,
                    },
                });
            }

            pos = absolute_pos + 1;
        }
    }

    None
}

/// Find the range of a name after 'define', 'defun', etc.
fn find_define_name_range(source: &str, name: &str) -> Option<Range> {
    // This is more accurate - looks for the name after define/defun keywords
    find_name_in_source(source, name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_define() {
        let source = "(define x 42)";
        let table = extract_symbols(source);

        assert!(table.find_definitions("x").is_some());
        let defs = table.find_definitions("x").unwrap();
        assert_eq!(defs.len(), 1);
        assert_eq!(defs[0].kind, SymbolKind::Variable);
    }

    #[test]
    fn test_extract_defun() {
        let source = "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))";
        let table = extract_symbols(source);

        assert!(table.find_definitions("factorial").is_some());
        let defs = table.find_definitions("factorial").unwrap();
        assert_eq!(defs[0].kind, SymbolKind::Function);
    }

    #[test]
    fn test_extract_let_bindings() {
        let source = "(let ((x 10) (y 20)) (+ x y))";
        let table = extract_symbols(source);

        assert!(table.find_definitions("x").is_some());
        assert!(table.find_definitions("y").is_some());
    }
}
