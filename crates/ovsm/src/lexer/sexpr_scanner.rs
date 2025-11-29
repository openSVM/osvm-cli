use super::token::{Token, TokenKind};
use crate::error::{Error, Result};

/// Scanner for S-expression (LISP-style) OVSM syntax
pub struct SExprScanner {
    /// Source code as character vector
    source: Vec<char>,
    /// Accumulated tokens
    tokens: Vec<Token>,
    /// Start position of current token
    start: usize,
    /// Current position in source
    current: usize,
    /// Current line number (1-indexed)
    line: usize,
    /// Current column number (1-indexed)
    column: usize,
}

impl SExprScanner {
    /// Creates a new S-expression scanner from source code
    pub fn new(source: &str) -> Self {
        SExprScanner {
            source: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
        }
    }

    /// Scans all tokens from source code and returns them as a vector
    pub fn scan_tokens(&mut self) -> Result<Vec<Token>> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }

        self.tokens.push(Token::new(
            TokenKind::Eof,
            String::new(),
            self.line,
            self.column,
        ));

        Ok(self.tokens.clone())
    }

    fn scan_token(&mut self) -> Result<()> {
        let c = self.advance();

        match c {
            // Whitespace (ignore, no indentation tracking needed!)
            ' ' | '\r' | '\t' | '\n' => {
                if c == '\n' {
                    self.line += 1;
                    self.column = 1;
                }
            }

            // Comments - LISP uses semicolon for comments
            ';' => {
                self.skip_line_comment();
            }

            // S-expression delimiters
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            '[' => self.add_token(TokenKind::LeftBracket),
            ']' => self.add_token(TokenKind::RightBracket),

            // Special LISP tokens
            '\'' => self.add_token(TokenKind::Quote),
            '`' => self.add_token(TokenKind::Backtick),
            '@' => self.add_token(TokenKind::At),

            ',' => {
                // Check for ,@ (unquote-splice)
                if self.match_char('@') {
                    self.add_token(TokenKind::CommaAt);
                } else {
                    self.add_token(TokenKind::Comma);
                }
            }

            // Operators
            '+' => self.add_token(TokenKind::Plus),
            '-' => {
                // Check if it's a negative number or minus operator
                if self.peek().is_ascii_digit() {
                    self.scan_number(true)?;
                } else if self.match_char('>') {
                    self.add_token(TokenKind::Arrow);
                } else {
                    self.add_token(TokenKind::Minus);
                }
            }
            '*' => {
                if self.match_char('*') {
                    self.add_token(TokenKind::StarStar);
                } else {
                    self.add_token(TokenKind::Star);
                }
            }
            '/' => self.add_token(TokenKind::Slash),
            '%' => self.add_token(TokenKind::Percent),

            // Comparison operators
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::Eq);
                } else if self.match_char('>') {
                    self.add_token(TokenKind::FatArrow);
                } else {
                    self.add_token(TokenKind::Assign);
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::NotEq);
                } else {
                    self.add_token(TokenKind::Not);
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::LtEq);
                } else {
                    self.add_token(TokenKind::Lt);
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::GtEq);
                } else {
                    self.add_token(TokenKind::Gt);
                }
            }

            // Property access
            '.' => {
                if self.match_char('.') {
                    self.add_token(TokenKind::DotDot);
                } else {
                    self.add_token(TokenKind::Dot);
                }
            }

            // Colon (for keywords)
            ':' => self.add_token(TokenKind::Colon),

            // Pipe (for refinement types: {x : T | predicate})
            '|' => self.add_token(TokenKind::Pipe),

            // Strings
            '"' => self.scan_string()?,

            // Numbers
            c if c.is_ascii_digit() => self.scan_number(false)?,

            // Identifiers and keywords
            // LISP: No $variables - just regular identifiers like 'define', 'set!', 'if', etc.
            // Allow & for &rest, &optional, etc.
            c if c.is_alphabetic() || c == '_' || c == '?' || c == '&' => {
                self.scan_identifier_or_keyword()?;
            }

            _ => {
                return Err(Error::ParseError(format!(
                    "Unexpected character '{}' at line {}, column {}",
                    c, self.line, self.column
                )));
            }
        }

        Ok(())
    }

    fn skip_line_comment(&mut self) {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
    }

    fn scan_string(&mut self) -> Result<()> {
        let mut value = String::new();

        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\\' {
                self.advance();
                let escaped = self.advance();
                match escaped {
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '\\' => value.push('\\'),
                    '"' => value.push('"'),
                    _ => {
                        return Err(Error::ParseError(format!(
                            "Invalid escape sequence \\{} at line {}",
                            escaped, self.line
                        )));
                    }
                }
            } else {
                if self.peek() == '\n' {
                    self.line += 1;
                    self.column = 1;
                }
                value.push(self.advance());
            }
        }

        if self.is_at_end() {
            return Err(Error::ParseError(format!(
                "Unterminated string at line {}",
                self.line
            )));
        }

        self.advance(); // Closing "

        self.add_token(TokenKind::String(value));
        Ok(())
    }

    fn scan_number(&mut self, _negative: bool) -> Result<()> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        let mut is_float = false;
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            is_float = true;
            self.advance(); // consume .
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let text: String = self.source[self.start..self.current].iter().collect();

        if is_float {
            let value: f64 = text
                .parse()
                .map_err(|_| Error::ParseError(format!("Invalid float: {}", text)))?;
            self.add_token(TokenKind::Float(value));
        } else {
            let value: i64 = text
                .parse()
                .map_err(|_| Error::ParseError(format!("Invalid integer: {}", text)))?;
            self.add_token(TokenKind::Integer(value));
        }

        Ok(())
    }

    fn scan_identifier_or_keyword(&mut self) -> Result<()> {
        // In Common Lisp, identifiers can contain *, +, -, /, etc as suffixes
        // First, scan the base identifier
        while self.peek().is_alphanumeric()
            || self.peek() == '_'
            || self.peek() == '-'
            || self.peek() == '?'
            || self.peek() == '!'
            || self.peek() == '&'
        {
            self.advance();
        }

        // Now check for trailing *, +, / which are valid in CL identifiers like let*, 1+, etc.
        // Allow any number of these at the end
        while matches!(self.peek(), '*' | '+' | '/') {
            self.advance();
        }

        let text: String = self.source[self.start..self.current].iter().collect();

        // Check for boolean literals
        let token_kind = match text.as_str() {
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "nil" | "null" => TokenKind::Null,
            _ => {
                // Check if it's a keyword argument (starts with :)
                if text.starts_with(':') {
                    TokenKind::Identifier(text[1..].to_string()) // Remove the :
                } else {
                    // Otherwise it's an identifier or symbol
                    TokenKind::Identifier(text)
                }
            }
        };

        self.add_token(token_kind);
        Ok(())
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.current];
        self.current += 1;
        self.column += 1;
        c
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source[self.current] != expected {
            false
        } else {
            self.current += 1;
            self.column += 1;
            true
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        let lexeme: String = self.source[self.start..self.current].iter().collect();
        self.tokens
            .push(Token::new(kind, lexeme, self.line, self.column));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_sexpr() {
        let source = "(+ 1 2)";
        let mut scanner = SExprScanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        // Debug: print tokens
        for (i, token) in tokens.iter().enumerate() {
            println!("{}: {:?}", i, token.kind);
        }

        assert_eq!(tokens.len(), 6); // ( + 1 2 ) EOF
        assert_eq!(tokens[0].kind, TokenKind::LeftParen);
        assert_eq!(tokens[1].kind, TokenKind::Plus);
        assert_eq!(tokens[2].kind, TokenKind::Integer(1));
        assert_eq!(tokens[3].kind, TokenKind::Integer(2));
        assert_eq!(tokens[4].kind, TokenKind::RightParen);
        assert_eq!(tokens[5].kind, TokenKind::Eof);
    }

    #[test]
    fn test_nested_sexpr() {
        let source = "(if (== x 0) true false)";
        let mut scanner = SExprScanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        // Should parse without errors
        assert!(!tokens.is_empty());
        assert_eq!(tokens[0].kind, TokenKind::LeftParen);
    }

    #[test]
    fn test_quote() {
        let source = "'(1 2 3)";
        let mut scanner = SExprScanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Quote);
        assert_eq!(tokens[1].kind, TokenKind::LeftParen);
    }

    #[test]
    fn test_keyword_args() {
        let source = "(log :message \"hello\")";
        let mut scanner = SExprScanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        // Should contain colon and identifier
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Colon)));
    }

    #[test]
    fn test_comment() {
        let source = "; This is a comment\n(+ 1 2)";
        let mut scanner = SExprScanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        // Comment should be skipped
        assert_eq!(tokens[0].kind, TokenKind::LeftParen);
        assert_eq!(tokens[1].kind, TokenKind::Plus);
    }
}
