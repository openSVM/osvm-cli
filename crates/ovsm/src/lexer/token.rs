use serde::{Deserialize, Serialize};

/// A single token from the source code
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token {
    /// The type of token
    pub kind: TokenKind,
    /// Original text of the token
    pub lexeme: String,
    /// Line number where token appears (1-indexed)
    pub line: usize,
    /// Column number where token starts (1-indexed)
    pub column: usize,
}

impl Token {
    /// Creates a new token with the given properties
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
    /// Integer literal
    Integer(i64),
    /// Floating-point literal
    Float(f64),
    /// String literal
    String(String),
    /// Boolean true literal
    True,
    /// Boolean false literal
    False,
    /// Null literal
    Null,

    // Identifiers
    /// Identifier (lowercase or mixed case)
    Identifier(String),
    /// Variable name (prefixed with $)
    Variable(String),
    /// Constant name (all UPPERCASE)
    Constant(String),

    // Keywords
    /// IF keyword
    If,
    /// ELSE keyword
    Else,
    /// ELIF keyword (else-if)
    Elif,
    /// THEN keyword
    Then,
    /// WHILE keyword
    While,
    /// FOR keyword
    For,
    /// IN keyword
    In,
    /// BREAK keyword
    Break,
    /// CONTINUE keyword
    Continue,
    /// RETURN keyword
    Return,
    /// TRY keyword
    Try,
    /// CATCH keyword
    Catch,
    /// PARALLEL keyword
    Parallel,
    /// WAIT_ALL keyword
    WaitAll,
    /// WAIT_ANY keyword
    WaitAny,
    /// RACE keyword
    Race,
    /// DECISION keyword
    Decision,
    /// BRANCH keyword
    Branch,
    /// GUARD keyword
    Guard,
    /// MATCH keyword
    Match,
    /// DEFINE keyword
    Define,
    /// DEFINE_TOOL keyword
    DefineTool,
    /// CONST keyword
    Const,
    /// TOOL keyword
    Tool,
    /// FAIL keyword
    Fail,
    /// LOOP keyword
    Loop,
    /// EVERY keyword
    Every,
    /// TIMEOUT keyword
    Timeout,
    /// RETRY keyword
    Retry,
    /// CIRCUIT_BREAKER keyword
    CircuitBreaker,
    /// FATAL keyword
    Fatal,
    /// RECOVERABLE keyword
    Recoverable,
    /// WARNING keyword
    Warning,
    /// ENDIF keyword (optional block terminator)
    EndIf,
    /// ENDWHILE keyword (optional block terminator)
    EndWhile,
    /// ENDFOR keyword (optional block terminator)
    EndFor,
    /// END keyword (generic optional block terminator)
    End,
    /// LAMBDA keyword (anonymous function)
    Lambda,

    // Operators
    /// Plus operator (+)
    Plus,
    /// Minus operator (-)
    Minus,
    /// Star operator (*)
    Star,
    /// Slash operator (/)
    Slash,
    /// Percent operator (%)
    Percent,
    /// Power operator (**)
    StarStar,
    /// Equality operator (==)
    Eq,
    /// Inequality operator (!=)
    NotEq,
    /// Less than operator (<)
    Lt,
    /// Greater than operator (>)
    Gt,
    /// Less than or equal operator (<=)
    LtEq,
    /// Greater than or equal operator (>=)
    GtEq,
    /// Logical AND operator
    And,
    /// Logical OR operator
    Or,
    /// Logical NOT operator
    Not,
    /// Logical XOR operator
    Xor,
    /// Assignment operator (=)
    Assign,
    /// Plus-assign operator (+=)
    PlusAssign,
    /// Minus-assign operator (-=)
    MinusAssign,
    /// Star-assign operator (*=)
    StarAssign,
    /// Slash-assign operator (/=)
    SlashAssign,
    /// Percent-assign operator (%=)
    PercentAssign,
    /// Question mark operator (?)
    Question,
    /// Colon operator (:)
    Colon,
    /// Optional chaining operator (?.)
    QuestionDot,
    /// Null coalescing operator (??)
    QuestionQuestion,
    /// Arrow operator (->)
    Arrow,
    /// Fat arrow operator (=>)
    FatArrow,

    // Delimiters
    /// Left parenthesis (
    LeftParen,
    /// Right parenthesis )
    RightParen,
    /// Left brace {
    LeftBrace,
    /// Right brace }
    RightBrace,
    /// Left bracket [
    LeftBracket,
    /// Right bracket ]
    RightBracket,
    /// Comma delimiter
    Comma,
    /// Dot operator
    Dot,
    /// Range operator (..)
    DotDot,
    /// Semicolon delimiter
    Semicolon,
    /// Newline delimiter
    Newline,
    /// Quote for LISP-style syntax (')
    Quote,
    /// Backtick for LISP quasi-quote (`)
    Backtick,
    /// At symbol for LISP splice (@)
    At,
    /// Comma-At for LISP unquote-splice (,@)
    CommaAt,
    /// Pipe operator (|) - used for refinement types
    Pipe,
    /// Indent token (for Python-style syntax)
    Indent,
    /// Dedent token (for Python-style syntax)
    Dedent,

    // Special
    /// End of file marker
    Eof,
}

impl TokenKind {
    /// Check if token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::If
                | TokenKind::Else
                | TokenKind::Elif
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
                | TokenKind::EndIf
                | TokenKind::EndWhile
                | TokenKind::EndFor
                | TokenKind::End
                | TokenKind::Lambda
        )
    }

    /// Get keyword from string - LISP has NO keywords, everything is an identifier
    /// This function now returns None for everything - Python keywords are DEAD
    pub fn keyword(_s: &str) -> Option<TokenKind> {
        // PYTHON IS DEAD. LONG LIVE LISP.
        // In LISP, there are no reserved keywords - everything is just identifiers
        // Special forms like 'if', 'define', 'while' are handled by the parser, not the lexer
        None
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
        // In LISP, there are no lexer-level keywords - everything is identifiers
        // Keywords like 'if', 'define', 'while' are handled by the parser
        assert_eq!(TokenKind::keyword("if"), None);
        assert_eq!(TokenKind::keyword("define"), None);
        assert_eq!(TokenKind::keyword("while"), None);
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
