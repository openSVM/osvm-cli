//! Semantic token support for OVSM syntax highlighting
//!
//! Provides rich syntax highlighting by classifying tokens into
//! semantic categories that editors can use for theming.

use ovsm::lexer::{Token, TokenKind};
use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend,
};

/// Token types for OVSM semantic highlighting
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,   // 0 - Special forms (if, define, let, etc.)
    SemanticTokenType::FUNCTION,  // 1 - Built-in functions
    SemanticTokenType::VARIABLE,  // 2 - Variables
    SemanticTokenType::STRING,    // 3 - Strings
    SemanticTokenType::NUMBER,    // 4 - Numbers
    SemanticTokenType::OPERATOR,  // 5 - Operators
    SemanticTokenType::COMMENT,   // 6 - Comments (handled separately)
    SemanticTokenType::PARAMETER, // 7 - Lambda parameters
    SemanticTokenType::TYPE,      // 8 - Type names
    SemanticTokenType::MACRO,     // 9 - Macros
    SemanticTokenType::PROPERTY,  // 10 - Object keys
];

/// Token modifiers for OVSM
pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DECLARATION,   // 0 - Variable declaration
    SemanticTokenModifier::DEFINITION,    // 1 - Function definition
    SemanticTokenModifier::READONLY,      // 2 - Constants
    SemanticTokenModifier::MODIFICATION,  // 3 - Mutation (set!)
    SemanticTokenModifier::DOCUMENTATION, // 4 - Doc comments
];

/// Get the semantic tokens legend for client registration
pub fn get_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: TOKEN_MODIFIERS.to_vec(),
    }
}

/// Special forms that should be highlighted as keywords
const SPECIAL_FORMS: &[&str] = &[
    "if",
    "define",
    "set!",
    "let",
    "let*",
    "lambda",
    "do",
    "progn",
    "while",
    "for",
    "when",
    "unless",
    "cond",
    "case",
    "typecase",
    "defun",
    "defn",
    "defmacro",
    "const",
    "catch",
    "throw",
    "loop",
    "flet",
    "labels",
    "destructuring-bind",
];

/// Built-in functions that should be highlighted differently
const BUILTIN_FUNCTIONS: &[&str] = &[
    // Type predicates
    "null?",
    "empty?",
    "evenp",
    "oddp",
    "zerop",
    "positivep",
    "negativep",
    "typeof",
    "type-of",
    "atom",
    "consp",
    "listp",
    // Collection operations
    "length",
    "get",
    "first",
    "rest",
    "last",
    "cons",
    "append",
    "reverse",
    "range",
    "mapcar",
    "map",
    "filter",
    "reduce",
    "member",
    "assoc",
    "elt",
    "subseq",
    "sort",
    "keys",
    "values",
    "merge",
    // String operations
    "concat",
    "split",
    "join",
    "replace",
    "trim",
    "upper",
    "lower",
    // Math functions
    "sqrt",
    "pow",
    "expt",
    "exp",
    "ln",
    "abs",
    "min",
    "max",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "floor",
    "ceiling",
    "ceil",
    "round",
    "truncate",
    "trunc",
    "gcd",
    "lcm",
    "mod",
    "rem",
    // Bitwise
    "logand",
    "logior",
    "logxor",
    "lognot",
    "ash",
    // Type conversion
    "int",
    "integer",
    "float",
    "bool",
    "string",
    "parse-int",
    "parse-float",
    // I/O
    "log",
    "print",
    "assert",
    "error",
    "now",
    // Logic
    "not",
    "and",
    "or",
    // Misc
    "incf",
    "decf",
    "gensym",
    "defvar",
];

/// Convert OVSM tokens to LSP semantic tokens
pub fn tokens_to_semantic(tokens: &[Token]) -> Vec<SemanticToken> {
    let mut result = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    // Track context for better classification
    let mut in_define = false;
    let mut in_let_bindings = false;
    let mut paren_depth = 0;

    for (i, token) in tokens.iter().enumerate() {
        let token_type = classify_token(token, i, tokens, in_define, in_let_bindings);

        // Skip unclassified tokens
        let Some(type_idx) = token_type else {
            // Update context tracking
            update_context(
                token,
                &mut in_define,
                &mut in_let_bindings,
                &mut paren_depth,
            );
            continue;
        };

        let line = (token.line as u32).saturating_sub(1);
        let start = (token.column as u32).saturating_sub(1);
        let length = token.lexeme.len() as u32;

        // LSP semantic tokens use delta encoding
        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            start - prev_start
        } else {
            start
        };

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: type_idx,
            token_modifiers_bitset: 0, // TODO: Add modifiers
        });

        prev_line = line;
        prev_start = start;

        // Update context tracking
        update_context(
            token,
            &mut in_define,
            &mut in_let_bindings,
            &mut paren_depth,
        );
    }

    result
}

/// Classify a single token into a semantic type
fn classify_token(
    token: &Token,
    _idx: usize,
    _tokens: &[Token],
    _in_define: bool,
    _in_let_bindings: bool,
) -> Option<u32> {
    match &token.kind {
        // Numbers
        TokenKind::Integer(_) | TokenKind::Float(_) => Some(4), // NUMBER

        // Strings
        TokenKind::String(_) => Some(3), // STRING

        // Booleans and null (highlight as keywords)
        TokenKind::True | TokenKind::False | TokenKind::Null => Some(0), // KEYWORD

        // Identifiers - need to classify further
        TokenKind::Identifier(name) => classify_identifier(name),

        // Operators
        TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::Star
        | TokenKind::Slash
        | TokenKind::Percent
        | TokenKind::Eq
        | TokenKind::NotEq
        | TokenKind::Lt
        | TokenKind::Gt
        | TokenKind::LtEq
        | TokenKind::GtEq
        | TokenKind::And
        | TokenKind::Or
        | TokenKind::Not => Some(5), // OPERATOR

        // Quote and other special characters
        TokenKind::Quote | TokenKind::Backtick | TokenKind::CommaAt => Some(9), // MACRO

        // Colon (keyword marker) - property
        TokenKind::Colon => Some(10), // PROPERTY

        // Skip delimiters and other tokens
        _ => None,
    }
}

/// Classify an identifier token
fn classify_identifier(name: &str) -> Option<u32> {
    // Check if it's a special form
    if SPECIAL_FORMS.contains(&name) {
        return Some(0); // KEYWORD
    }

    // Check if it's a built-in function
    if BUILTIN_FUNCTIONS.contains(&name) {
        return Some(1); // FUNCTION
    }

    // Otherwise it's a variable
    Some(2) // VARIABLE
}

/// Update context tracking based on current token
fn update_context(
    token: &Token,
    in_define: &mut bool,
    in_let_bindings: &mut bool,
    paren_depth: &mut i32,
) {
    match &token.kind {
        TokenKind::LeftParen => {
            *paren_depth += 1;
        }
        TokenKind::RightParen => {
            *paren_depth -= 1;
            if *paren_depth <= 1 {
                *in_define = false;
                *in_let_bindings = false;
            }
        }
        TokenKind::Identifier(name) if name == "define" || name == "defun" => {
            *in_define = true;
        }
        TokenKind::Identifier(name) if name == "let" || name == "let*" => {
            *in_let_bindings = true;
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ovsm::lexer::SExprScanner;

    #[test]
    fn test_special_form_classification() {
        assert_eq!(classify_identifier("if"), Some(0)); // KEYWORD
        assert_eq!(classify_identifier("define"), Some(0)); // KEYWORD
        assert_eq!(classify_identifier("lambda"), Some(0)); // KEYWORD
    }

    #[test]
    fn test_builtin_function_classification() {
        assert_eq!(classify_identifier("length"), Some(1)); // FUNCTION
        assert_eq!(classify_identifier("sqrt"), Some(1)); // FUNCTION
        assert_eq!(classify_identifier("mapcar"), Some(1)); // FUNCTION
    }

    #[test]
    fn test_variable_classification() {
        assert_eq!(classify_identifier("foo"), Some(2)); // VARIABLE
        assert_eq!(classify_identifier("my-var"), Some(2)); // VARIABLE
    }

    #[test]
    fn test_tokens_to_semantic() {
        let mut scanner = SExprScanner::new("(define x 42)");
        let tokens = scanner.scan_tokens().unwrap();
        let semantic = tokens_to_semantic(&tokens);

        // Should have tokens for: define, x, 42
        assert!(!semantic.is_empty());
    }
}
