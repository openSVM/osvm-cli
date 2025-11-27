//! Diagnostics generation for OVSM LSP
//!
//! Converts OVSM lexer/parser errors into LSP Diagnostic objects
//! with precise source locations.

use ovsm::error::Error as OvsmError;
use ovsm::lexer::{SExprScanner, Token, TokenKind};
use ovsm::parser::SExprParser;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, Position, Range, Url,
};

/// Result of analyzing a document
pub struct AnalysisResult {
    /// Diagnostics found in the document
    pub diagnostics: Vec<Diagnostic>,
    /// Tokens from lexing (for semantic highlighting)
    pub tokens: Vec<Token>,
}

/// Analyze OVSM source code and return diagnostics
pub fn analyze_document(source: &str, uri: &Url) -> AnalysisResult {
    let mut diagnostics = Vec::new();
    let mut tokens = Vec::new();

    // Step 1: Lex the source
    let mut scanner = SExprScanner::new(source);
    match scanner.scan_tokens() {
        Ok(scanned_tokens) => {
            tokens = scanned_tokens.clone();

            // Check for parenthesis balance
            check_parenthesis_balance(&tokens, &mut diagnostics);

            // Step 2: Parse the tokens
            let mut parser = SExprParser::new(scanned_tokens);
            if let Err(e) = parser.parse() {
                diagnostics.push(ovsm_error_to_diagnostic(&e, uri));
            }
        }
        Err(e) => {
            diagnostics.push(ovsm_error_to_diagnostic(&e, uri));
        }
    }

    AnalysisResult { diagnostics, tokens }
}

/// Convert an OVSM error to an LSP Diagnostic
fn ovsm_error_to_diagnostic(error: &OvsmError, _uri: &Url) -> Diagnostic {
    match error {
        OvsmError::SyntaxError { line, col, message } => {
            let range = Range {
                start: Position {
                    line: (*line as u32).saturating_sub(1),
                    character: (*col as u32).saturating_sub(1),
                },
                end: Position {
                    line: (*line as u32).saturating_sub(1),
                    character: *col as u32, // highlight at least one character
                },
            };

            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(tower_lsp::lsp_types::NumberOrString::String(
                    "syntax-error".to_string(),
                )),
                code_description: None,
                source: Some("ovsm".to_string()),
                message: message.clone(),
                related_information: None,
                tags: None,
                data: None,
            }
        }

        OvsmError::ParseError(msg) => Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 1,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(tower_lsp::lsp_types::NumberOrString::String(
                "parse-error".to_string(),
            )),
            code_description: None,
            source: Some("ovsm".to_string()),
            message: msg.clone(),
            related_information: None,
            tags: None,
            data: None,
        },

        OvsmError::UnexpectedToken { expected, got } => Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 1,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(tower_lsp::lsp_types::NumberOrString::String(
                "unexpected-token".to_string(),
            )),
            code_description: None,
            source: Some("ovsm".to_string()),
            message: format!("Expected {}, got {}", expected, got),
            related_information: None,
            tags: None,
            data: None,
        },

        OvsmError::UnexpectedEof => Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 1,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(tower_lsp::lsp_types::NumberOrString::String(
                "unexpected-eof".to_string(),
            )),
            code_description: None,
            source: Some("ovsm".to_string()),
            message: "Unexpected end of file. Check for unclosed parentheses or strings."
                .to_string(),
            related_information: None,
            tags: None,
            data: None,
        },

        // Handle other errors generically
        other => Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 1,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(tower_lsp::lsp_types::NumberOrString::String(
                "error".to_string(),
            )),
            code_description: None,
            source: Some("ovsm".to_string()),
            message: other.to_string(),
            related_information: None,
            tags: None,
            data: None,
        },
    }
}

/// Check parenthesis balance and generate diagnostics for mismatches
fn check_parenthesis_balance(tokens: &[Token], diagnostics: &mut Vec<Diagnostic>) {
    let mut paren_stack: Vec<&Token> = Vec::new();
    let mut bracket_stack: Vec<&Token> = Vec::new();
    let mut brace_stack: Vec<&Token> = Vec::new();

    for token in tokens {
        match &token.kind {
            TokenKind::LeftParen => paren_stack.push(token),
            TokenKind::RightParen => {
                if paren_stack.pop().is_none() {
                    diagnostics.push(Diagnostic {
                        range: token_to_range(token),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(tower_lsp::lsp_types::NumberOrString::String(
                            "unmatched-paren".to_string(),
                        )),
                        code_description: None,
                        source: Some("ovsm".to_string()),
                        message: "Unmatched closing parenthesis `)`. No corresponding opening `(`.".to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
            }
            TokenKind::LeftBracket => bracket_stack.push(token),
            TokenKind::RightBracket => {
                if bracket_stack.pop().is_none() {
                    diagnostics.push(Diagnostic {
                        range: token_to_range(token),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(tower_lsp::lsp_types::NumberOrString::String(
                            "unmatched-bracket".to_string(),
                        )),
                        code_description: None,
                        source: Some("ovsm".to_string()),
                        message: "Unmatched closing bracket `]`. No corresponding opening `[`.".to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
            }
            TokenKind::LeftBrace => brace_stack.push(token),
            TokenKind::RightBrace => {
                if brace_stack.pop().is_none() {
                    diagnostics.push(Diagnostic {
                        range: token_to_range(token),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(tower_lsp::lsp_types::NumberOrString::String(
                            "unmatched-brace".to_string(),
                        )),
                        code_description: None,
                        source: Some("ovsm".to_string()),
                        message: "Unmatched closing brace `}`. No corresponding opening `{`.".to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
            }
            _ => {}
        }
    }

    // Report unclosed delimiters
    for token in paren_stack {
        diagnostics.push(Diagnostic {
            range: token_to_range(token),
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(tower_lsp::lsp_types::NumberOrString::String(
                "unclosed-paren".to_string(),
            )),
            code_description: None,
            source: Some("ovsm".to_string()),
            message: "Unclosed parenthesis `(`. Missing closing `)`.".to_string(),
            related_information: None,
            tags: None,
            data: None,
        });
    }

    for token in bracket_stack {
        diagnostics.push(Diagnostic {
            range: token_to_range(token),
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(tower_lsp::lsp_types::NumberOrString::String(
                "unclosed-bracket".to_string(),
            )),
            code_description: None,
            source: Some("ovsm".to_string()),
            message: "Unclosed bracket `[`. Missing closing `]`.".to_string(),
            related_information: None,
            tags: None,
            data: None,
        });
    }

    for token in brace_stack {
        diagnostics.push(Diagnostic {
            range: token_to_range(token),
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(tower_lsp::lsp_types::NumberOrString::String(
                "unclosed-brace".to_string(),
            )),
            code_description: None,
            source: Some("ovsm".to_string()),
            message: "Unclosed brace `{`. Missing closing `}`.".to_string(),
            related_information: None,
            tags: None,
            data: None,
        });
    }
}

/// Convert a token to an LSP Range
fn token_to_range(token: &Token) -> Range {
    let line = (token.line as u32).saturating_sub(1);
    let col_start = (token.column as u32).saturating_sub(1);
    let col_end = col_start + token.lexeme.len() as u32;

    Range {
        start: Position {
            line,
            character: col_start,
        },
        end: Position {
            line,
            character: col_end,
        },
    }
}

/// Find the token at a given position
pub fn find_token_at_position(tokens: &[Token], position: Position) -> Option<&Token> {
    let line = position.line as usize + 1; // LSP is 0-indexed, tokens are 1-indexed
    let col = position.character as usize + 1;

    for token in tokens {
        if token.line == line {
            let token_end = token.column + token.lexeme.len();
            if col >= token.column && col <= token_end {
                return Some(token);
            }
        }
    }

    None
}

/// Get the word at a position (for hover)
pub fn get_word_at_position(source: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    let line_idx = position.line as usize;
    let col = position.character as usize;

    if line_idx >= lines.len() {
        return None;
    }

    let line = lines[line_idx];
    if col >= line.len() {
        return None;
    }

    // Find word boundaries
    let chars: Vec<char> = line.chars().collect();

    // Find start of word
    let mut start = col;
    while start > 0 && is_symbol_char(chars[start - 1]) {
        start -= 1;
    }

    // Find end of word
    let mut end = col;
    while end < chars.len() && is_symbol_char(chars[end]) {
        end += 1;
    }

    if start == end {
        return None;
    }

    Some(chars[start..end].iter().collect())
}

/// Check if character is valid in a symbol name
fn is_symbol_char(c: char) -> bool {
    c.is_alphanumeric() || matches!(c, '_' | '-' | '!' | '?' | '*' | '+' | '/' | '<' | '>' | '=' | '&')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_balanced_parens() {
        let url = Url::parse("file:///test.ovsm").unwrap();
        let result = analyze_document("(+ 1 2)", &url);
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn test_unbalanced_open_paren() {
        let url = Url::parse("file:///test.ovsm").unwrap();
        let result = analyze_document("(+ 1 2", &url);
        assert!(!result.diagnostics.is_empty());
        assert!(result.diagnostics[0].message.contains("Unclosed"));
    }

    #[test]
    fn test_unbalanced_close_paren() {
        let url = Url::parse("file:///test.ovsm").unwrap();
        let result = analyze_document("(+ 1 2))", &url);
        assert!(!result.diagnostics.is_empty());
        assert!(result.diagnostics[0].message.contains("Unmatched"));
    }

    #[test]
    fn test_get_word_at_position() {
        let source = "(define foo 42)";
        let word = get_word_at_position(source, Position { line: 0, character: 1 });
        assert_eq!(word, Some("define".to_string()));

        let word = get_word_at_position(source, Position { line: 0, character: 8 });
        assert_eq!(word, Some("foo".to_string()));
    }
}
