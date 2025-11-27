//! REPL evaluation for inline results
//!
//! Provides code lens and execute commands for evaluating OVSM expressions
//! inline within the editor, similar to Jupyter notebooks.
//!
//! # Features
//!
//! - Code lens "▶ Run" buttons on top-level expressions
//! - "▶ Run All" to execute entire file
//! - Results displayed as inlay hints after expressions
//! - Execution time and memory tracking

use ovsm::lexer::SExprScanner;
use ovsm::parser::SExprParser;
use ovsm::runtime::LispEvaluator;
use ovsm::Value;
use std::time::Instant;
use tower_lsp::lsp_types::{
    CodeLens, Command, Position, Range,
};

/// Result of evaluating an expression
#[derive(Debug, Clone)]
pub struct EvalResult {
    /// The result value formatted as string
    pub value: String,
    /// Execution time in milliseconds
    pub duration_ms: f64,
    /// Whether execution succeeded
    pub success: bool,
    /// Error message if failed
    pub error: Option<String>,
}

/// Information about a top-level expression
#[derive(Debug, Clone)]
pub struct TopLevelExpression {
    /// Range of the expression in the document
    pub range: Range,
    /// The expression source code
    pub source: String,
    /// Index of this expression (0-based)
    pub index: usize,
}

/// Find all top-level expressions in an OVSM document
pub fn find_top_level_expressions(source: &str) -> Vec<TopLevelExpression> {
    let mut expressions = Vec::new();
    let lines: Vec<&str> = source.lines().collect();

    let mut in_expression = false;
    let mut expr_start_line = 0;
    let mut paren_depth = 0;
    let mut current_expr = String::new();

    for (line_idx, line) in lines.iter().enumerate() {
        let line_trimmed = line.trim();

        // Skip comments and empty lines when not in an expression
        if !in_expression && (line_trimmed.is_empty() || line_trimmed.starts_with(';')) {
            continue;
        }

        for ch in line.chars() {
            match ch {
                '(' | '[' | '{' => {
                    if !in_expression {
                        in_expression = true;
                        expr_start_line = line_idx;
                        current_expr.clear();
                    }
                    paren_depth += 1;
                    current_expr.push(ch);
                }
                ')' | ']' | '}' => {
                    current_expr.push(ch);
                    paren_depth -= 1;

                    // Expression complete
                    if paren_depth == 0 && in_expression {
                        expressions.push(TopLevelExpression {
                            range: Range {
                                start: Position {
                                    line: expr_start_line as u32,
                                    character: 0,
                                },
                                end: Position {
                                    line: line_idx as u32,
                                    character: line.len() as u32,
                                },
                            },
                            source: current_expr.trim().to_string(),
                            index: expressions.len(),
                        });
                        in_expression = false;
                        current_expr.clear();
                    }
                }
                ';' if !in_expression => {
                    // Comment - skip rest of line
                    break;
                }
                _ => {
                    if in_expression {
                        current_expr.push(ch);
                    }
                }
            }
        }

        if in_expression {
            current_expr.push('\n');
        }
    }

    expressions
}

/// Generate code lenses for a document
pub fn generate_code_lenses(source: &str, uri: &str) -> Vec<CodeLens> {
    let expressions = find_top_level_expressions(source);
    let mut lenses = Vec::new();

    // "Run All" lens at the top of the file
    if !expressions.is_empty() {
        lenses.push(CodeLens {
            range: Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: 0, character: 0 },
            },
            command: Some(Command {
                title: "▶ Run All".to_string(),
                command: "ovsm.runAll".to_string(),
                arguments: Some(vec![serde_json::json!(uri)]),
            }),
            data: None,
        });
    }

    // Individual "Run" lenses for each expression
    for expr in expressions {
        lenses.push(CodeLens {
            range: expr.range,
            command: Some(Command {
                title: "▶ Run".to_string(),
                command: "ovsm.runExpression".to_string(),
                arguments: Some(vec![
                    serde_json::json!(uri),
                    serde_json::json!(expr.index),
                    serde_json::json!(expr.source),
                ]),
            }),
            data: None,
        });
    }

    lenses
}

/// Evaluate a single OVSM expression
pub fn evaluate_expression(source: &str) -> EvalResult {
    let start = Instant::now();

    // Parse and evaluate
    let mut scanner = SExprScanner::new(source);
    let tokens = match scanner.scan_tokens() {
        Ok(t) => t,
        Err(e) => {
            return EvalResult {
                value: String::new(),
                duration_ms: start.elapsed().as_secs_f64() * 1000.0,
                success: false,
                error: Some(format!("Scan error: {}", e)),
            };
        }
    };

    let mut parser = SExprParser::new(tokens);
    let program = match parser.parse() {
        Ok(p) => p,
        Err(e) => {
            return EvalResult {
                value: String::new(),
                duration_ms: start.elapsed().as_secs_f64() * 1000.0,
                success: false,
                error: Some(format!("Parse error: {}", e)),
            };
        }
    };

    // Execute
    let mut evaluator = LispEvaluator::new();
    match evaluator.execute(&program) {
        Ok(value) => {
            let duration_ms = start.elapsed().as_secs_f64() * 1000.0;
            EvalResult {
                value: format_value(&value),
                duration_ms,
                success: true,
                error: None,
            }
        }
        Err(e) => EvalResult {
            value: String::new(),
            duration_ms: start.elapsed().as_secs_f64() * 1000.0,
            success: false,
            error: Some(format!("Runtime error: {}", e)),
        },
    }
}

/// Evaluate all expressions in a document
pub fn evaluate_all(source: &str) -> Vec<EvalResult> {
    let expressions = find_top_level_expressions(source);
    let mut results = Vec::new();
    let mut evaluator = LispEvaluator::new();

    for expr in expressions {
        let start = Instant::now();

        // Parse
        let mut scanner = SExprScanner::new(&expr.source);
        let tokens = match scanner.scan_tokens() {
            Ok(t) => t,
            Err(e) => {
                results.push(EvalResult {
                    value: String::new(),
                    duration_ms: start.elapsed().as_secs_f64() * 1000.0,
                    success: false,
                    error: Some(format!("Scan error: {}", e)),
                });
                continue;
            }
        };

        let mut parser = SExprParser::new(tokens);
        let program = match parser.parse() {
            Ok(p) => p,
            Err(e) => {
                results.push(EvalResult {
                    value: String::new(),
                    duration_ms: start.elapsed().as_secs_f64() * 1000.0,
                    success: false,
                    error: Some(format!("Parse error: {}", e)),
                });
                continue;
            }
        };

        // Execute (reuse evaluator to maintain state across expressions)
        match evaluator.execute(&program) {
            Ok(value) => {
                results.push(EvalResult {
                    value: format_value(&value),
                    duration_ms: start.elapsed().as_secs_f64() * 1000.0,
                    success: true,
                    error: None,
                });
            }
            Err(e) => {
                results.push(EvalResult {
                    value: String::new(),
                    duration_ms: start.elapsed().as_secs_f64() * 1000.0,
                    success: false,
                    error: Some(format!("Runtime error: {}", e)),
                });
            }
        }
    }

    results
}

/// Format a value for display
fn format_value(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => {
            if f.fract() == 0.0 {
                format!("{:.1}", f)
            } else {
                format!("{}", f)
            }
        }
        Value::String(s) => format!("\"{}\"", s),
        Value::Array(arr) => {
            if arr.len() > 5 {
                let preview: Vec<String> = arr.iter().take(5).map(format_value).collect();
                format!("[{}, ... ({} more)]", preview.join(" "), arr.len() - 5)
            } else {
                let items: Vec<String> = arr.iter().map(format_value).collect();
                format!("[{}]", items.join(" "))
            }
        }
        Value::Object(obj) => {
            if obj.len() > 3 {
                let preview: Vec<String> = obj
                    .iter()
                    .take(3)
                    .map(|(k, v)| format!(":{} {}", k, format_value(v)))
                    .collect();
                format!("{{{}... ({} more)}}", preview.join(" "), obj.len() - 3)
            } else {
                let items: Vec<String> = obj
                    .iter()
                    .map(|(k, v)| format!(":{} {}", k, format_value(v)))
                    .collect();
                format!("{{{}}}", items.join(" "))
            }
        }
        Value::Function { params, .. } => {
            format!("<fn ({})>", params.join(" "))
        }
        Value::Macro { params, .. } => {
            format!("<macro ({})>", params.join(" "))
        }
        Value::Range { start, end } => format!("[{}..{}]", start, end),
        Value::Multiple(values) => {
            let items: Vec<String> = values.iter().map(format_value).collect();
            format!("(values {})", items.join(" "))
        }
        Value::AsyncHandle { id, .. } => format!("<async {}>", id),
    }
}

/// Format result for display as inline text
pub fn format_result_inline(result: &EvalResult) -> String {
    if result.success {
        format!("=> {} ({:.2}ms)", result.value, result.duration_ms)
    } else {
        format!("❌ {}", result.error.as_deref().unwrap_or("Error"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_top_level_expressions() {
        let source = r#"
; Comment
(define x 10)

(+ x 20)

; Another comment
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
"#;

        let exprs = find_top_level_expressions(source);
        assert_eq!(exprs.len(), 3);
        assert!(exprs[0].source.contains("define x 10"));
        assert!(exprs[1].source.contains("+ x 20"));
        assert!(exprs[2].source.contains("defun factorial"));
    }

    #[test]
    fn test_evaluate_expression() {
        let result = evaluate_expression("(+ 1 2 3)");
        assert!(result.success);
        assert_eq!(result.value, "6");
    }

    #[test]
    fn test_evaluate_error() {
        let result = evaluate_expression("(undefined-function)");
        assert!(!result.success);
        assert!(result.error.is_some());
    }

    #[test]
    fn test_format_value() {
        assert_eq!(format_value(&Value::Int(42)), "42");
        assert_eq!(format_value(&Value::String("hello".to_string())), "\"hello\"");
        assert_eq!(format_value(&Value::Bool(true)), "true");
        assert_eq!(format_value(&Value::Null), "null");
    }

    #[test]
    fn test_generate_code_lenses() {
        let source = "(define x 10)\n(+ x 5)";
        let lenses = generate_code_lenses(source, "file:///test.ovsm");

        // Should have "Run All" + 2 individual "Run" lenses
        assert_eq!(lenses.len(), 3);
        assert!(lenses[0].command.as_ref().unwrap().title.contains("Run All"));
    }
}
