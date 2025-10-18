use super::token::{Token, TokenKind};
use crate::error::{Error, Result};

/// Scanner for tokenizing OVSM source code
pub struct Scanner {
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

impl Scanner {
    /// Creates a new scanner from source code
    pub fn new(source: &str) -> Self {
        Scanner {
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
            // Whitespace
            ' ' | '\r' | '\t' => {
                // Skip whitespace
            }
            '\n' => {
                self.add_token(TokenKind::Newline);
                self.line += 1;
                self.column = 1;
            }

            // Single character tokens
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            '[' => self.add_token(TokenKind::LeftBracket),
            ']' => self.add_token(TokenKind::RightBracket),
            ',' => self.add_token(TokenKind::Comma),
            ';' => self.add_token(TokenKind::Semicolon),
            '%' => self.add_token(TokenKind::Percent),

            // Operators that might be multi-character
            '+' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::PlusAssign);
                } else {
                    self.add_token(TokenKind::Plus);
                }
            }
            '-' => {
                if self.match_char('>') {
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
            '#' => {
                // Python-style comment
                while self.peek() != '\n' && !self.is_at_end() {
                    self.advance();
                }
            }
            '/' => {
                if self.match_char('/') {
                    // Line comment
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenKind::Slash);
                }
            }
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
            '?' => {
                if self.match_char('?') {
                    self.add_token(TokenKind::QuestionQuestion);
                } else if self.match_char('.') {
                    self.add_token(TokenKind::QuestionDot);
                } else {
                    self.add_token(TokenKind::Question);
                }
            }
            ':' => self.add_token(TokenKind::Colon),
            '.' => {
                if self.match_char('.') {
                    self.add_token(TokenKind::DotDot);
                } else {
                    self.add_token(TokenKind::Dot);
                }
            }

            // String literals
            '"' => self.string()?,

            // Variables (start with $)
            '$' => self.variable()?,

            // Numbers or identifiers
            _ => {
                if c.is_ascii_digit() {
                    self.number()?;
                } else if c.is_alphabetic() || c == '_' {
                    self.identifier();
                } else {
                    return Err(Error::SyntaxError {
                        line: self.line,
                        col: self.column,
                        message: format!("Unexpected character: {}", c),
                    });
                }
            }
        }

        Ok(())
    }

    fn string(&mut self) -> Result<()> {
        let mut value = String::new();

        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 0;
            }
            // Handle escape sequences
            if self.peek() == '\\' {
                self.advance();
                let escaped = self.advance();
                value.push(match escaped {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '"' => '"',
                    _ => escaped,
                });
            } else {
                value.push(self.advance());
            }
        }

        if self.is_at_end() {
            return Err(Error::SyntaxError {
                line: self.line,
                col: self.column,
                message: "Unterminated string".to_string(),
            });
        }

        // Consume closing "
        self.advance();

        self.add_token(TokenKind::String(value));
        Ok(())
    }

    fn number(&mut self) -> Result<()> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Check for decimal point
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // Consume '.'
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }

            let value: f64 = self.get_lexeme().parse().map_err(|_| {
                Error::ParseError(format!("Invalid float literal: {}", self.get_lexeme()))
            })?;
            self.add_token(TokenKind::Float(value));
        } else {
            let value: i64 = self.get_lexeme().parse().map_err(|_| {
                Error::ParseError(format!("Invalid integer literal: {}", self.get_lexeme()))
            })?;
            self.add_token(TokenKind::Integer(value));
        }

        Ok(())
    }

    fn variable(&mut self) -> Result<()> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let name = self.get_lexeme()[1..].to_string(); // Skip $
        if name.is_empty() {
            return Err(Error::SyntaxError {
                line: self.line,
                col: self.column,
                message: "Empty variable name".to_string(),
            });
        }

        self.add_token(TokenKind::Variable(name));
        Ok(())
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text = self.get_lexeme();

        // Check if it's a keyword FIRST (before checking uppercase)
        // This ensures AND, OR, etc. are recognized as keywords
        if let Some(keyword) = TokenKind::keyword(&text) {
            self.add_token(keyword);
        }
        // Check if it's true/false/null
        else if text == "true" {
            self.add_token(TokenKind::True);
        } else if text == "false" {
            self.add_token(TokenKind::False);
        } else if text == "null" {
            self.add_token(TokenKind::Null);
        }
        // Check if it's a constant (all uppercase with underscores)
        // Only if it's not a keyword
        else if text
            .chars()
            .all(|c| c.is_uppercase() || c == '_' || c.is_ascii_digit())
        {
            self.add_token(TokenKind::Constant(text));
        } else {
            self.add_token(TokenKind::Identifier(text));
        }
    }

    // Helper methods

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
        if self.is_at_end() {
            return false;
        }
        if self.source[self.current] != expected {
            return false;
        }

        self.current += 1;
        self.column += 1;
        true
    }

    fn get_lexeme(&self) -> String {
        self.source[self.start..self.current].iter().collect()
    }

    fn add_token(&mut self, kind: TokenKind) {
        let lexeme = self.get_lexeme();
        let token = Token::new(kind, lexeme, self.line, self.column);
        self.tokens.push(token);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let mut scanner = Scanner::new("+ - * / == != < > <= >=");
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Plus);
        assert_eq!(tokens[1].kind, TokenKind::Minus);
        assert_eq!(tokens[2].kind, TokenKind::Star);
        assert_eq!(tokens[3].kind, TokenKind::Slash);
        assert_eq!(tokens[4].kind, TokenKind::Eq);
        assert_eq!(tokens[5].kind, TokenKind::NotEq);
        assert_eq!(tokens[6].kind, TokenKind::Lt);
        assert_eq!(tokens[7].kind, TokenKind::Gt);
        assert_eq!(tokens[8].kind, TokenKind::LtEq);
        assert_eq!(tokens[9].kind, TokenKind::GtEq);
    }

    #[test]
    fn test_variables() {
        let mut scanner = Scanner::new("$x $variable_name $_test");
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Variable("x".to_string()));
        assert_eq!(
            tokens[1].kind,
            TokenKind::Variable("variable_name".to_string())
        );
        assert_eq!(tokens[2].kind, TokenKind::Variable("_test".to_string()));
    }

    #[test]
    fn test_numbers() {
        let mut scanner = Scanner::new("42 3.15 0 100");
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Integer(42));
        assert_eq!(tokens[1].kind, TokenKind::Float(3.15));
        assert_eq!(tokens[2].kind, TokenKind::Integer(0));
        assert_eq!(tokens[3].kind, TokenKind::Integer(100));
    }

    #[test]
    fn test_strings() {
        let mut scanner = Scanner::new(r#""hello" "world" "with\nnewline""#);
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::String("hello".to_string()));
        assert_eq!(tokens[1].kind, TokenKind::String("world".to_string()));
        assert_eq!(
            tokens[2].kind,
            TokenKind::String("with\nnewline".to_string())
        );
    }

    #[test]
    fn test_keywords() {
        let mut scanner = Scanner::new("IF ELSE WHILE FOR RETURN");
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::If);
        assert_eq!(tokens[1].kind, TokenKind::Else);
        assert_eq!(tokens[2].kind, TokenKind::While);
        assert_eq!(tokens[3].kind, TokenKind::For);
        assert_eq!(tokens[4].kind, TokenKind::Return);
    }

    #[test]
    fn test_identifiers_and_constants() {
        let mut scanner = Scanner::new("myFunc PI MAX_VALUE");
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Identifier("myFunc".to_string()));
        assert_eq!(tokens[1].kind, TokenKind::Constant("PI".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::Constant("MAX_VALUE".to_string()));
    }

    #[test]
    fn test_assignment_expression() {
        let mut scanner = Scanner::new("$x = 42");
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Variable("x".to_string()));
        assert_eq!(tokens[1].kind, TokenKind::Assign);
        assert_eq!(tokens[2].kind, TokenKind::Integer(42));
    }

    #[test]
    fn test_comments() {
        let mut scanner = Scanner::new("$x = 42 // This is a comment\n$y = 10");
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Variable("x".to_string()));
        assert_eq!(tokens[1].kind, TokenKind::Assign);
        assert_eq!(tokens[2].kind, TokenKind::Integer(42));
        assert_eq!(tokens[3].kind, TokenKind::Newline);
        assert_eq!(tokens[4].kind, TokenKind::Variable("y".to_string()));
    }
}
