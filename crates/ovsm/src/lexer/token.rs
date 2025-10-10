use serde::{Deserialize, Serialize};

/// A single token from the source code
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize, column: usize) -> Self {
        Token {
            kind,
            lexeme,
            line,
            column,
        }
    }
}

/// All possible token types in OVSM
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TokenKind {
    // Literals
    Integer(i64),
    Float(f64),
    String(String),
    True,
    False,
    Null,

    // Identifiers
    Identifier(String),
    Variable(String),  // $name
    Constant(String),  // UPPERCASE

    // Keywords
    If,
    Else,
    Then,
    While,
    For,
    In,
    Break,
    Continue,
    Return,
    Try,
    Catch,
    Parallel,
    WaitAll,
    WaitAny,
    Race,
    Decision,
    Branch,
    Guard,
    Match,
    Define,
    DefineTool,
    Const,
    Tool,
    Fail,
    Loop,
    Every,
    Timeout,
    Retry,
    CircuitBreaker,
    Fatal,
    Recoverable,
    Warning,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    StarStar,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
    Not,
    Xor,
    Assign,
    PlusAssign,
    Question,
    Colon,
    QuestionDot,
    QuestionQuestion,
    Arrow,
    FatArrow,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    DotDot,
    Semicolon,
    Newline,

    // Special
    Eof,
}

impl TokenKind {
    /// Check if token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::If
                | TokenKind::Else
                | TokenKind::Then
                | TokenKind::While
                | TokenKind::For
                | TokenKind::In
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Return
                | TokenKind::Try
                | TokenKind::Catch
                | TokenKind::Parallel
                | TokenKind::WaitAll
                | TokenKind::WaitAny
                | TokenKind::Race
                | TokenKind::Decision
                | TokenKind::Branch
                | TokenKind::Guard
                | TokenKind::Match
                | TokenKind::Define
                | TokenKind::DefineTool
                | TokenKind::Const
                | TokenKind::Tool
                | TokenKind::Fail
                | TokenKind::Loop
                | TokenKind::Every
                | TokenKind::Timeout
                | TokenKind::Retry
                | TokenKind::CircuitBreaker
                | TokenKind::Fatal
                | TokenKind::Recoverable
                | TokenKind::Warning
        )
    }

    /// Get keyword from string
    pub fn keyword(s: &str) -> Option<TokenKind> {
        match s {
            "IF" => Some(TokenKind::If),
            "ELSE" => Some(TokenKind::Else),
            "THEN" => Some(TokenKind::Then),
            "WHILE" => Some(TokenKind::While),
            "FOR" => Some(TokenKind::For),
            "IN" => Some(TokenKind::In),
            "BREAK" => Some(TokenKind::Break),
            "CONTINUE" => Some(TokenKind::Continue),
            "RETURN" => Some(TokenKind::Return),
            "TRY" => Some(TokenKind::Try),
            "CATCH" => Some(TokenKind::Catch),
            "PARALLEL" => Some(TokenKind::Parallel),
            "WAIT_ALL" => Some(TokenKind::WaitAll),
            "WAIT_ANY" => Some(TokenKind::WaitAny),
            "RACE" => Some(TokenKind::Race),
            "DECISION" => Some(TokenKind::Decision),
            "BRANCH" => Some(TokenKind::Branch),
            "GUARD" => Some(TokenKind::Guard),
            "MATCH" => Some(TokenKind::Match),
            "DEFINE" => Some(TokenKind::Define),
            "DEFINE_TOOL" => Some(TokenKind::DefineTool),
            "CONST" => Some(TokenKind::Const),
            "TOOL" => Some(TokenKind::Tool),
            "FAIL" => Some(TokenKind::Fail),
            "LOOP" => Some(TokenKind::Loop),
            "EVERY" => Some(TokenKind::Every),
            "TIMEOUT" => Some(TokenKind::Timeout),
            "RETRY" => Some(TokenKind::Retry),
            "CIRCUIT_BREAKER" => Some(TokenKind::CircuitBreaker),
            "FATAL" => Some(TokenKind::Fatal),
            "RECOVERABLE" => Some(TokenKind::Recoverable),
            "WARNING" => Some(TokenKind::Warning),
            _ => None,
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenKind::Integer(n) => write!(f, "{}", n),
            TokenKind::Float(fl) => write!(f, "{}", fl),
            TokenKind::String(s) => write!(f, "\"{}\"", s),
            TokenKind::Identifier(id) => write!(f, "{}", id),
            TokenKind::Variable(name) => write!(f, "${}", name),
            TokenKind::Constant(name) => write!(f, "{}", name),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_detection() {
        assert_eq!(TokenKind::keyword("IF"), Some(TokenKind::If));
        assert_eq!(TokenKind::keyword("WHILE"), Some(TokenKind::While));
        assert_eq!(TokenKind::keyword("RETURN"), Some(TokenKind::Return));
        assert_eq!(TokenKind::keyword("not_a_keyword"), None);
    }

    #[test]
    fn test_is_keyword() {
        assert!(TokenKind::If.is_keyword());
        assert!(TokenKind::While.is_keyword());
        assert!(!TokenKind::Integer(42).is_keyword());
        assert!(!TokenKind::Identifier("test".to_string()).is_keyword());
    }
}
